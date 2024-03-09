/*
    Fury, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package fury

import anticipation.*
import anthology.*
import ambience.*
import aviation.*, calendars.gregorian
import escapade.*
import eucalyptus.*
import ethereal.*
import digression.*
import fulminate.*

import galilei.*, filesystemOptions.{createNonexistent,
                                     createNonexistentParents,
                                     dereferenceSymlinks,
                                     overwritePreexisting,
                                     deleteRecursively,
                                     moveAtomically}

import gastronomy.*, alphabets.base32.zBase32Unpadded
import gossamer.*
import acyclicity.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.skip
import guillotine.*
import hellenism.*
import hypotenuse.*
import inimitable.*
import feudalism.*
import nettlesome.*
import octogenarian.*
import parasite.*
import iridescence.*
import contingency.*
import dendrology.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*
import vacuous.*
import zeppelin.*

import scala.collection.concurrent as scc

case class ConfigError(msg: Message) extends Error(msg)

case class Config(log: LogConfig = LogConfig())
case class LogConfig(path: Path = Unix / p"var" / p"log" / p"fury.log")

case class BuildError() extends Error(msg"the build could not run")

val Isolation: Semaphore = Semaphore()
val Epoch: LocalTime = 2000-Jan-1 at 0.00.am in tz"Etc/UTC"

class Builder():
  private val phases: scc.TrieMap[Hash, Phase] = scc.TrieMap()
  private val builds: scc.TrieMap[Target, Async[Hash]] = scc.TrieMap()
  private val tasks: scc.TrieMap[Hash, Async[Optional[Directory]]] = scc.TrieMap()

  given expandable: Expandable[Phase] = _.dependencies.map(phases(_))

  def dag(digest: Hash): Dag[Phase] = Dag.create(phases(digest))(_.dependencies.to(Set).map(phases(_)))
  def buildGraph(digest: Hash): DagDiagram[Phase] = DagDiagram(dag(digest))

  def build(target: Target)(using Universe)
      (using Monitor, Clock, Log[Display], WorkingDirectory, Internet, Installation, GitCommand)
          : Async[Hash] raises BuildError =

    builds.synchronized:
      builds.getOrElseUpdate
       (target,
        async:
          Log.info(msg"Starting computation of $target")
    
          given (BuildError fixes GitError)        = error => BuildError()
          given (BuildError fixes ExecError)       = error => BuildError()
          given (BuildError fixes PathError)       = error => BuildError()
          given (BuildError fixes IoError)         = error => BuildError()
          given (BuildError fixes UnknownRefError) = error => BuildError()
          given (BuildError fixes WorkspaceError)  = error => BuildError()
          given (BuildError fixes StreamError)     = error => BuildError()
          given (BuildError fixes CancelError)     = error => BuildError()
          
          val workspace = universe(target.projectId).source match
            case workspace: Workspace => workspace
 
            case vault: Vault =>
              Workspace(Cache(vault.index.releases(target.projectId).repo).await().path)
            
          workspace(target.projectId)(target.goalId) match
            case module: Module =>
              
              val sourceFiles: List[File] = module.sources.flatMap: directory =>
                workspace(directory)
                 .as[Directory]
                 .descendants
                 .filter(_.is[File])
                 .filter(_.name.ends(t".scala"))
                 .map(_.as[File])
        
              val includes = module.includes.map(build(_)).map(_.await())
              val classpath = includes.map(phases(_)).flatMap(_.runtimeClasspath).to(Set).to(List)
              
              // FIXME: Don't create directories if they don't exist
              val watchDirectories = module.sources.map(workspace(_).as[Directory]).to(Set)
              val phase = Phase(target, watchDirectories, sourceFiles, includes, classpath, Nil, Unset, Unset, Unset, Unset)
              
              phases(phase.digest) = phase
                
              Log.info(msg"Computed $target")
              phase.digest
            
            case artifact: Artifact =>
              val includes = artifact.includes.map(build(_)).map(_.await())
              val classpath = includes.map(phases(_)).flatMap(_.runtimeClasspath).to(Set).to(List)
              // FIXME: Specify watchDirectories
              val phase =
                Phase(target, Set(), Nil, includes, classpath, Nil, workspace(artifact.path), artifact, artifact.prefix.let(workspace(_)), artifact.basis)

              phases(phase.digest) = phase
              phase.digest)

  def watchDirectories(hash: Hash): Set[Directory] = dag(hash).keys.flatMap(_.watchDirectories)

  def run(hash: Hash)
      (using Log[Display], DaemonService[?], Installation, FrontEnd, Monitor, SystemProperties, Environment)
          : Async[Optional[Directory]] raises CancelError raises StreamError raises ZipError raises
             IoError raises PathError raises BuildError raises ScalacError =

    val phase = phases(hash)

    tasks.synchronized:
      tasks.getOrElseUpdate
       (hash,
        async:
          val outputName = hash.bytes.encodeAs[Base32]
          val output = installation.build / PathName(outputName.take(2)) / PathName(outputName.drop(2))
          val checkFile = output / p"checksum"
          val inputAsyncs = phase.classpath.map(run)
          
          phase.destination.let: path =>
            def checksumsDiffer(): Boolean =
              val currentHash = path.as[File].stream[Bytes].digest[Sha2[256]].bytes.encodeAs[Base32]
              checkFile.as[File].readAs[Text] != currentHash
            
            if !path.exists() || checkFile.exists() && checksumsDiffer() then
              info(t"Building file $path")
              val tmpPath = unsafely(installation.work / PathName(Uuid().show))
              val zipFile = phase.basis.lay(ZipFile.create(tmpPath)): basis =>
                basis().copyTo(tmpPath)
                ZipFile(tmpPath.as[File])
  
              (dag(hash) - phase).sorted.map(_.digest).each: hash =>
                tasks(hash).await().let: directory =>
                  val entries = directory.descendants.filter(_.is[File]).map: path =>
                    ZipEntry(ZipRef(t"/"+path.relativeTo(directory.path).show), path.as[File])
  
                  zipFile.append(entries, Epoch)
              
              tmpPath.as[File].tap: file =>
                output.as[Directory]
                phase.prefix.let: prefix =>
                  val tmpPath2 = unsafely(installation.work / PathName(Uuid().show))
                  val tmpFile = prefix.as[File].copyTo(tmpPath2).as[File]
                  file.stream[Bytes].appendTo(tmpFile)
                  tmpFile.moveTo(file.path)
                  
                file.stream[Bytes].digest[Sha2[256]].bytes.encodeAs[Base32].writeTo(checkFile.as[File])
                phase.artifact.let: artifact =>
                  if artifact.executable.or(false) then file.executable() = true
                
                file.moveTo(path)

          val inputs = inputAsyncs.map(_.await())

          if output.exists() then output.as[Directory].tap(_.touch())
          else
            if inputs.exists(_.absent) then abort(CancelError()) else
              val additions = inputs.compact.map(_.path)
              info(msg"Starting to build ${phase.target} in ${hash.bytes.encodeAs[Base32]}")
              
              val work = (installation.work / PathName(Uuid().show)).as[Directory]
              val classpath = LocalClasspath(List(ClasspathEntry.Jarfile(unsafely(Basis.Tools().path.show))))
            
              val classpath2 = additions.foldLeft(classpath)(_ + _)
              if phase.sources.isEmpty then output.as[Directory] else
                val process: ScalacProcess =
                  Log.envelop(phase.target):
                    import scalacOptions.*
                    Scalac[3.4]
                     (List(language.experimental.fewerBraces,
                           language.experimental.erasedDefinitions,
                           language.experimental.clauseInterleaving,
                           language.experimental.into,
                           language.experimental.genericNumberLiterals,
                           language.experimental.saferExceptions,
                           language.experimental.namedTypeArguments,
                           internal.requireTargetName,
                           internal.explicitNulls,
                           internal.checkPatterns,
                           internal.safeInit,
                           advanced.maxInlines(64),
                           experimental,
                           sourceFuture,
                           newSyntax,
                           warnings.deprecation,
                           warnings.feature))
                     (classpath2)
                     (phase.sources, work.path)
                
                daemon:
                  summon[FrontEnd].aborted.await()
                  process.abort()

                async:
                  process.notices.each: notice =>
                    info(e"$Bold(${phase.target})")
                    info(e"${notice.importance}: $Italic(${notice.message})")
                    info(e"${colors.Silver}(${notice.code})")
                  
                process.complete()
    
                val errorCount = process.notices.count(_.importance == Importance.Error)
                val warnCount = process.notices.count(_.importance == Importance.Warning)
    
                info(msg"Finished building ${phase.target} with $errorCount errors and $warnCount warnings")
    
                if process.cancelled then Unset
                else if errorCount == 0 then
                  work.moveTo(output)
                  output.as[Directory]
                else Unset)
  
extension (workspace: Workspace)
  def locals(ancestors: Set[Path] = Set())
      (using Monitor, Log[Display], WorkingDirectory, Internet, Installation, GitCommand)
          : Map[ProjectId, Definition] raises CancelError raises WorkspaceError =

    workspace.local.let: local =>
      local.forks.map: fork =>
        val workspace = Cache.workspace(fork.path).await()
        val projects = workspace.projects
        workspace.locals(ancestors + fork.path)
        
    .or(Nil).foldRight(workspace.projects.view.mapValues(_.definition(workspace)).to(Map))(_ ++ _)
  
  def universe()
      (using Monitor, Clock, Log[Display], WorkingDirectory, Internet, Installation, GitCommand)
          : Universe raises CancelError raises VaultError raises WorkspaceError =

    given Timezone = tz"Etc/UTC"

    val vaultProjects = Cache(workspace.ecosystem).await()
    val localProjects = locals()
    
    val projects: Map[ProjectId, Definition] =
      vaultProjects.releases.filter(_.expiry <= today()).map: release =>
        (release.id, release.definition(vaultProjects))
      .to(Map)
    
    Universe(projects -- localProjects.keySet ++ localProjects)

  def apply(projectId: ProjectId): Project = workspace.projects(projectId)

  def apply(path: WorkPath)
      (using Installation,
             Internet,
             Monitor,
             WorkingDirectory,
             Log[Display],
             Raises[CancelError],
             Raises[GitError],
             Raises[PathError],
             Raises[ExecError],
             Raises[IoError])
          : Path =

    workspace.mounts.keys.find(_.precedes(path)).optional.lay(workspace.directory.path + path.link): mount =>
      Cache(workspace.mounts(mount).repo).await().path + path.link

def universe(using universe: Universe): Universe = universe

case class Universe(projects: Map[ProjectId, Definition]):
  def apply(id: ProjectId)(using Raises[UnknownRefError]): Definition =
    projects.getOrElse(id, abort(UnknownRefError(id)))

enum Compiler:
  case Java(version: Int)
  case Scala
  case Kotlin
  
  def sources(name: Text): Boolean = this match
    case Java(_) => name.ends(t".java")
    case Scala   => name.ends(t".scala")
    case Kotlin  => name.ends(t".kt")

object Phase:
  def apply
     (target:           Target,
      watchDirectories: Set[Directory],
      sources:          List[File],
      dependencies:     List[Hash],
      classpath:        List[Hash],
      binaries:         List[Hash],
      destination:      Optional[Path],
      artifact:         Optional[Artifact],
      prefix:           Optional[Path],
      basis:            Optional[Basis])
     (using Monitor)
          : Phase raises BuildError =

    given (BuildError fixes CancelError) = error => BuildError()
    given (BuildError fixes StreamError) = error => BuildError()
    given (BuildError fixes IoError)     = error => BuildError()

    val sourceMap = sources.map { file => file.path.name -> Cache.file(file.path).text.await() }.to(Map)
    
    Phase(target, watchDirectories, sourceMap, dependencies, classpath, binaries, destination, artifact, prefix, basis)

  given show: Show[Phase] = _.target.show

case class Phase
   (target:           Target,
    watchDirectories: Set[Directory],
    sources:          Map[Text, Text],
    dependencies:     List[Hash],
    classpath:        List[Hash],
    binaries:         List[Hash],
    destination:      Optional[Path],
    artifact:         Optional[Artifact],
    prefix:           Optional[Path],
    basis:            Optional[Basis]):

  lazy val digest = (sources.values.to(List), dependencies, binaries).digest[Sha2[256]]
  def runtimeClasspath = digest :: classpath

extension (basis: Basis)
  def inclusions: Set[Text] = basis match
    case Basis.Runtime =>
      Set(t"/scala/", t"/rootdoc.txt", t"/library.properties")

    case Basis.Tools =>
      Basis.Runtime.inclusions ++
        Set(t"/xsbti/", t"/dotty/", t"/compiler.properties", t"/org/scalajs/", t"/scala-asm.properties")
  
  def exclusions: Set[Text] = basis match
    case Basis.Runtime => Set(t"/scala/tasty/", t"/scala/quoted/staging/")
    case Basis.Tools   => Set()

  def path(using Installation): Path = unsafely(installation.basis / PathName(basis.encode+t".jar"))
    
  def apply()(using FrontEnd, Environment, Installation, DaemonService[?]): File raises BuildError =
    basis.synchronized:
      given (BuildError fixes StreamError) = error => BuildError()
      given (BuildError fixes ZipError)    = error => BuildError()
      given (BuildError fixes IoError)     = error => BuildError()

      if !path.exists() then
        inclusions.pipe: inclusions =>
          exclusions.pipe: exclusions =>
            val entries =
              ZipFile(service.script.as[File])
              .entries()
              .filter: entry =>
                  val name = entry.ref.show
                  inclusions.exists(name.starts(_)) && !exclusions.exists(name.starts(_))
        
            ZipFile.create(path.as[File].path).tap(_.append(entries, Epoch))
      
      path.as[File]
