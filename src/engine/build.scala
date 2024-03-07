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
import guillotine.*
import hellenism.*
import hypotenuse.*
import inimitable.*
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

class Builder():
  private val phases: scc.TrieMap[Hash, Phase] = scc.TrieMap()
  private val builds: scc.TrieMap[Target, Async[Hash]] = scc.TrieMap()
  private val tasks: scc.TrieMap[Hash, Async[Optional[Directory]]] = scc.TrieMap()

  given expandable: Expandable[Phase] = _.dependencies.map(phases(_))

  def buildGraph(digest: Hash): DagDiagram[Phase] =
    DagDiagram(Dag.create(phases(digest))(_.dependencies.to(Set).map(phases(_))))

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
            
          val project: Project = workspace(target.projectId)
          project(target.goalId) match
            case module: Module =>
              val sourceFiles: List[File] = module.sources.flatMap: directory =>
                workspace(directory).descendants.filter(_.is[File]).filter(_.name.ends(t".scala")).map(_.as[File])
        
              val includes = module.includes.map(build(_)).map(_.await())
              val classpath = includes.map(phases(_)).flatMap(_.runtimeClasspath).to(Set).to(List)
              val phase = Phase(target, sourceFiles, includes, classpath, Nil)
              
              phases(phase.digest) = phase
                
              Log.info(msg"Computed $target")
              phase.digest
            
            case artifact: Artifact =>
              val includes = artifact.includes.map(build(_)).map(_.await())
              val classpath = includes.map(phases(_)).flatMap(_.runtimeClasspath).to(Set).to(List)
              val phase = Phase(target, Nil, includes, classpath, Nil)
              phases(phase.digest) = phase
              phase.digest)

  def run(hash: Hash)(using Log[Display], Installation, FrontEnd, Monitor, SystemProperties)
          : Async[Optional[Directory]] raises CancelError raises IoError raises PathError raises ScalacError =

    val phase = phases(hash)

    tasks.synchronized:
      tasks.getOrElseUpdate
       (hash,
        async:
          val outputName = hash.bytes.encodeAs[Base32]
          val output = installation.build / PathName(outputName.take(2)) / PathName(outputName.drop(2))
          val inputs = phase.classpath.map(run).map(_.await())

          if output.exists() then
            output.as[Directory].tap(_.touch())
          else
            if inputs.exists(_.absent) then abort(CancelError()) else
              val additions = inputs.compact.map(_.path)
              info(msg"Starting to build ${phase.target} in ${hash.bytes.encodeAs[Base32]}")
              
              val work = (installation.work / PathName(Uuid().show)).as[Directory]
     
              val rootPath = t"/home/propensive/pub/dotty/dist/target/pack/lib"
              
              val classpath =
                LocalClasspath
                 (List
                   (ClasspathEntry.Jarfile(t"$rootPath/scala-library-2.13.12.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/scala3-library_3-3.4.2-RC1-bin-SNAPSHOT.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/scala-asm-9.6.0-scala-1.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/compiler-interface-1.9.6.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/scala3-interfaces-3.4.2-RC1-bin-SNAPSHOT.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/scala3-compiler_3-3.4.2-RC1-bin-SNAPSHOT.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/tasty-core_3-3.4.2-RC1-bin-SNAPSHOT.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/scala3-staging_3-3.4.2-RC1-bin-SNAPSHOT.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/scala3-tasty-inspector_3-3.4.2-RC1-bin-SNAPSHOT.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/jline-reader-3.25.1.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/jline-terminal-3.25.1.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/jline-terminal-jna-3.25.1.jar"),
                    ClasspathEntry.Jarfile(t"$rootPath/jna-5.14.0.jar")))
            
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
          : Directory =

    workspace.mounts.keys.find(_.precedes(path)).optional.lay(workspace.directory.path + path.link): mount =>
      Cache(workspace.mounts(mount).repo).await().path + path.link
    .as[Directory]

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
  def apply(target: Target, sources: List[File], dependencies: List[Hash], classpath: List[Hash], binaries: List[Hash])(using Monitor)
          : Phase raises BuildError =

    given (BuildError fixes CancelError) = error => BuildError()
    given (BuildError fixes StreamError) = error => BuildError()
    given (BuildError fixes IoError)     = error => BuildError()

    val sourceMap = sources.map { file => file.path.name -> Cache.file(file.path).text.await() }.to(Map)
    
    Phase(target, sourceMap, dependencies, classpath, binaries)

  given show: Show[Phase] = _.target.show

case class Phase(target: Target, sources: Map[Text, Text], dependencies: List[Hash], classpath: List[Hash], binaries: List[Hash]):
  lazy val digest = (sources.values.to(List), dependencies, binaries).digest[Sha2[256]]
  def runtimeClasspath = digest :: classpath