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
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.skip, textMetrics.uniform
import guillotine.*
import hellenism.*
import telekinesis.*
import hypotenuse.*
import inimitable.*
import feudalism.*
import harlequin.*, syntaxHighlighting.numbered
import nettlesome.*
import octogenarian.*
import revolution.*
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

type PhaseResult = Attempt[Directory, AggregateError[BuildError]]

class Builder():
  private val phases: scc.TrieMap[Hash, Phase] = scc.TrieMap()
  private val builds: scc.TrieMap[Target, Async[Hash]] = scc.TrieMap()
  private val tasks: scc.TrieMap[Hash, Async[PhaseResult]] = scc.TrieMap()

  def task(hash: Hash)
      (using Monitor, Environment, FrontEnd, Log[Display], SystemProperties, Installation, DaemonService[?], Internet)
          : Async[PhaseResult] =

    tasks.synchronized:
      tasks.getOrElseUpdate(hash, async(phases(hash).run(hash)))

  extension (library: Library)
    def phase(workspace: Workspace, target: Target)
        (using Installation, Internet, Monitor, WorkingDirectory, Log[Display], Universe, GitCommand, FrontEnd)
            : LibraryPhase raises CancelError raises PathError raises GitError raises BuildError raises
               ExecError raises IoError =
      LibraryPhase(installation.build, library, target)

  extension (artifact: Artifact)
    def phase(workspace: Workspace, target: Target)
        (using Installation, Internet, Monitor, WorkingDirectory, Log[Display], Universe, GitCommand, FrontEnd)
            : ArtifactPhase raises CancelError raises PathError raises GitError raises BuildError raises
               ExecError raises IoError =

      val destination: Path = workspace(artifact.path)
      val antecedents: List[Hash] = artifact.includes.map(build(_)).map(_.await())
      val classpath: List[Hash] = antecedents.map(phases(_)).flatMap(_.runtimeClasspath).to(Set).to(List)
      val prefixPaths = artifact.prefixes.map(workspace(_))
      val suffixPaths = artifact.suffixes.map(workspace(_))
      val counterPath = artifact.counter.let(workspace(_))
      
      val resourceMap: Map[Path, SafeLink] =
        artifact.resources.map: resource =>
          val path = workspace(resource.path)
          (path, resource.jarPath.lay(? / unsafely(PathName(path.name)))(_.link))
        .to(Map)

      val watches = Set(prefixPaths, suffixPaths).compact.flatten

      ArtifactPhase
       (installation.build,
        artifact,
        target,
        destination,
        antecedents,
        classpath,
        prefixPaths,
        suffixPaths,
        counterPath,
        resourceMap,
        watches)

  extension (module: Module)
    def phase(workspace: Workspace, target: Target)
        (using Installation, Internet, Universe, Monitor, WorkingDirectory, Log[Display], FrontEnd)
            : ModulePhase raises CancelError raises GitError raises PathError raises ExecError raises
               IoError raises BuildError raises StreamError =
      
      val antecedents: List[Hash] = module.includes.map(build(_)).map(_.await())
      val classpath: List[Hash] = antecedents.map(phases(_)).flatMap(_.runtimeClasspath).to(Set).to(List)
      val sourceFiles: List[File] =
        module
         .sources
         .map(workspace(_).as[Directory])
         .flatMap(_.descendants.filter(_.is[File]))
         .filter(_.name.ends(t".scala"))
         .map(_.as[File])
      
      val sourceMap = sourceFiles.map { file => file.path.name -> Cache.file(file.path).text.await() }.to(Map)
      val watches = module.sources.map(workspace(_)).to(Set)

      ModulePhase(installation.build, module, target, watches, sourceMap, antecedents, classpath)

  object Phase:
    given show: Show[Phase] = _.target.show
  
  trait Phase:
    def build: Path
    def target: Target
    def watches: Set[Path]
    def antecedents: List[Hash]
    def classpath: List[Hash]
    def binaries: List[Hash]
    def digest: Hash
    def runtimeClasspath = digest :: classpath
    
    lazy val output: Path = digest.bytes.encodeAs[Base32].pipe: hash =>
      unsafely(build / PathName(hash.take(2)) / PathName(hash.drop(2)))

    def run(hash: Hash)
        (using FrontEnd, Log[Display], DaemonService[?], Installation, Monitor, SystemProperties, Environment, Internet)
            : PhaseResult

  case class ArtifactPhase
     (build:       Path,
      artifact:    Artifact,
      target:      Target,
      destination: Path,
      antecedents: List[Hash],
      classpath:   List[Hash],
      prefixPaths: List[Path],
      suffixPaths: List[Path],
      counterPath: Optional[Path],
      resourceMap: Map[Path, SafeLink],
      watches:     Set[Path])
  extends Phase:

    export artifact.*
    val digest = antecedents.digest[Sha2[256]]
    val binaries: List[Hash] = Nil
    
    def run(hash: Hash)
        (using FrontEnd,
               Log[Display],
               DaemonService[?],
               Installation,
               Monitor,
               SystemProperties,
               Environment,
               Internet)
            : PhaseResult =

      attempt[AggregateError[BuildError]]:
        validate[BuildError]:
          given (BuildError fixes ExecError) = error => BuildError()
          given (BuildError fixes GitError) = error => BuildError()
          given (BuildError fixes PathError) = error => BuildError()
          given (BuildError fixes ZipError) = error => BuildError()
          given (BuildError fixes IoError) = error => BuildError()
          given (BuildError fixes StreamError) = error => BuildError()
          given (BuildError fixes WorkspaceError) = error => BuildError()
          given (BuildError fixes CancelError) = error => BuildError()
 
          val inputs = antecedents.map(task(_))
          val checksumPath = output / p"checksum"

          def savedChecksum = if checksumPath.exists() then checksumPath.as[File].readAs[Text] else Unset
          
          def fileChecksum = if !destination.exists() then Unset else
            destination.as[File].stream[Bytes].digest[Sha2[256]].bytes.encodeAs[Base32]
          
          if savedChecksum.absent || fileChecksum.absent || savedChecksum != fileChecksum || counterPath.present
          then
            val tmpPath = unsafely(installation.work / PathName(Uuid().show))
            
            val zipFile = basis.lay(ZipFile.create(tmpPath)): basis =>
              basis().await().copyTo(tmpPath)
              ZipFile(tmpPath.as[File])

            val todo = (dag(hash) - this).sorted.map(_.digest)
            val total = todo.size.toDouble
            var done = 0
            
            todo.each: hash =>
              tasks(hash).await().map: directory =>
                if summon[FrontEnd].continue then
                  val entries = directory.descendants.filter(_.is[File]).map: path =>
                    ZipEntry(ZipRef(t"/"+path.relativeTo(directory.path).show), path.as[File])

                  val manifestEntry = artifact.main.lay(LazyList()): mainClass =>
                    val manifest: Manifest =
                      Manifest
                       (manifestAttributes.ManifestVersion(VersionNumber(1, 0)),
                         manifestAttributes.CreatedBy(t"Fury"),
                         manifestAttributes.MainClass(mainClass))

                    LazyList(ZipEntry(ZipRef(t"/META-INF/MANIFEST.MF"), manifest))

                  val resourceEntries = resourceMap.flatMap: (source, destination) =>
                    if source.is[Directory]
                    then source.as[Directory].descendants.filter(_.is[File]).map: descendant =>
                      ZipEntry(ZipRef(descendant.relativeTo(source).show), descendant.as[File])
                    else Iterable(ZipEntry(ZipRef(t"/$destination"), source.as[File]))

                  zipFile.append(manifestEntry ++ entries ++ resourceEntries, Epoch)
                  done += 1
                  summon[FrontEnd](target) = done/total

                else
                  tmpPath.wipe()
                  abort(BuildError())
            
            
            tmpPath.as[File].tap: file =>
              output.as[Directory]

              if prefixPaths.isEmpty && suffixPaths.isEmpty then file else
                val prefixBytes: LazyList[Bytes] = prefixPaths.to(LazyList).flatMap(_.as[File].stream[Bytes]).or(LazyList())
                val suffixBytes: LazyList[Bytes] = suffixPaths.to(LazyList).flatMap(_.as[File].stream[Bytes]).or(LazyList())

                val tmpFile = unsafely(installation.work / PathName(Uuid().show)).as[File]
                (prefixBytes ++ file.stream[Bytes] ++ suffixBytes).writeTo(tmpFile)
                file.delete()
                tmpFile.moveTo(file.path)
                
              file.stream[Bytes].digest[Sha2[256]].bytes.encodeAs[Base32].writeTo(checksumPath.as[File])

              if executable.or(false) then file.executable() = true

              counterPath.let: counter =>
                safely(counter.as[File].readAs[Text].trim.decodeAs[Int]).let: count =>
                  t"${count + 1}\n".writeTo(counter.as[File])
              
              destination.wipe()
              file.moveTo(destination)
          else summon[FrontEnd](target) = -1.0

          output.as[Directory]

  case class LibraryPhase(build: Path, library: Library, target: Target) extends Phase:
    val antecedents: List[Hash] = Nil
    val watches: Set[Path] = Set()
    val classpath: List[Hash] = Nil
    val digest: Hash = library.url.show.digest[Sha2[256]]
    val binaries: List[Hash] = List(digest)

    def run(hash: Hash)
        (using FrontEnd, Log[Display], DaemonService[?], Installation, Monitor, SystemProperties, Environment, Internet)
            : PhaseResult =

      attempt[AggregateError[BuildError]]:
        validate[BuildError]:
          given (BuildError fixes HttpError) = error => BuildError()
          given (BuildError fixes IoError) = error => BuildError()
          given (BuildError fixes StreamError) = error => BuildError()
          given (BuildError fixes OfflineError) = error => BuildError()
          given (BuildError fixes NumberError) = error => BuildError()
          
          output.as[Directory]
          val checksum: Path = output / p"checksum"
          val jarfile: Path = output / p"library.jar"
          
          def jarfileChecksum(): Text = jarfile.as[File].stream[Bytes].digest[Sha2[256]].bytes.encodeAs[Base32]
          def storedChecksum(): Text = checksum.as[File].readAs[Text].trim
          
          if !(jarfile.exists() && checksum.exists() && jarfileChecksum() == storedChecksum()) then
            summon[Internet].require: (online: Online) ?=> // FIXME: Why is this necessary?
              Log.info(t"Initiating $target")
              val response: HttpResponse = library.url.get()
              val size: Double = response(ResponseHeader.ContentLength).map(_.long.toDouble).lift(0).getOrElse(Double.MaxValue)
              var count: Int = 0
              
              response.as[LazyList[Bytes]].map: bytes =>
                count += bytes.length
                Log.info(msg"Counted ${bytes.length}")
                summon[FrontEnd](target) = count/size
                bytes
              .writeTo(jarfile.as[File])

              Log.info(t"Downloaded ${target}")
              jarfileChecksum().writeTo(checksum.as[File])
              output.as[Directory]
          else
            summon[FrontEnd](target) = -1.0
            output.as[Directory]

  case class ModulePhase
     (build:       Path,
      module:      Module,
      target:      Target,
      watches:     Set[Path],
      sourceMap:   Map[Text, Text],
      antecedents: List[Hash],
      classpath:   List[Hash])
  extends Phase:
    export module.*
    val digest = (sourceMap.values.to(List), antecedents).digest[Sha2[256]]
    val binaries: List[Hash] = Nil
    
    def run(hash: Hash)
        (using FrontEnd,
               Log[Display],
               DaemonService[?],
               Installation,
               Monitor,
               SystemProperties,
               Environment,
               Internet)
            : PhaseResult =

      attempt[AggregateError[BuildError]]:
        validate[BuildError]:
          given (BuildError fixes GitError)       = error => BuildError()
          given (BuildError fixes PathError)      = error => BuildError()
          given (BuildError fixes ZipError)       = error => BuildError()
          given (BuildError fixes IoError)        = error => BuildError()
          given (BuildError fixes StreamError)    = error => BuildError()
          given (BuildError fixes ScalacError)    = error => BuildError()
          given (BuildError fixes WorkspaceError) = error => BuildError()
          given (BuildError fixes CancelError)    = error => BuildError()

          val inputs = antecedents.map(task(_)).map(_.await())
          if !summon[FrontEnd].continue then abort(BuildError())

          if output.exists() then
            summon[FrontEnd](target) = -1.0
            output.as[Directory].tap(_.touch())
          else
            if inputs.exists(_.failure)
            then
              Log.info(t"One of the inputs did not complete")
              abort(BuildError())
            else
              val work = (installation.work / PathName(Uuid().show)).as[Directory]
              val basis = unsafely(Basis.Tools().await().path.show)
              val baseClasspath = LocalClasspath(List(ClasspathEntry.Jarfile(basis)))
              val syntax: scc.TrieMap[Text, Async[IArray[Seq[Token]]]] = scc.TrieMap()
              
              def highlight(filename: Text): Async[ScalaSource] raises CancelError raises StreamError =
                async(ScalaSource.highlight(sourceMap(filename)))

              val allBinaries = classpath.flatMap(phases(_).binaries).map(outputDirectory(_) / p"library.jar")
              
              (classpath.map(outputDirectory(_)) ++ allBinaries).foldLeft(baseClasspath)(_ + _).pipe: classpath =>
                if sourceMap.isEmpty then output.as[Directory] else
                  val process: ScalacProcess =
                    Log.envelop(target):
                      import scalacOptions.*
                      Scalac[3.4]
                       (List
                         (language.experimental.fewerBraces,
                          language.experimental.genericNumberLiterals,
                          language.experimental.clauseInterleaving,
                          language.experimental.into,
                          language.experimental.erasedDefinitions,
                          language.experimental.saferExceptions,
                          language.experimental.namedTypeArguments,
                          advanced.maxInlines(64),
                          internal.requireTargetName,
                          internal.explicitNulls,
                          internal.checkPatterns,
                          internal.safeInit,
                          internal.ccNew,
                          experimental,
                          sourceFuture,
                          newSyntax,
                          warnings.lint.privateShadow,
                          warnings.lint.typeParameterShadow,
                          warnings.deprecation,
                          warnings.feature,
                          warnings.implausiblePatterns))
                       (classpath)
                       (sourceMap, work.path)
                  
                  daemon:
                    summon[FrontEnd].attend()
                    process.abort()
      
                  daemon:
                    process.progress.each: progress =>
                      summon[FrontEnd](target) = progress.complete

                  async:
                    process.notices.each: notice =>
                      notice.importance match
                        case Importance.Error   => info(errorRibbon.fill(e"$target", notice.file.display))
                        case Importance.Warning => info(warningRibbon.fill(e"$target", notice.file.display))
                        case Importance.Info    => info(infoRibbon.fill(e"$target", notice.file.display))

                      notice.codeRange.let: range =>
                        val source: ScalaSource = highlight(notice.file).await()
                        info(range.of(source).display)
                      
                      info(e"$Italic(${notice.message})")
                      info(t"")
                  
                  process.complete()
                  
                  val errorCount = process.notices.count(_.importance == Importance.Error)
                  val warnCount = process.notices.count(_.importance == Importance.Warning)
       
                  if errorCount == 0 then work.moveTo(output).as[Directory] else abort(BuildError())

  given expandable: Expandable[Phase] = _.antecedents.map(phases(_))

  def dag(digest: Hash): Dag[Phase] = Dag.create(phases(digest))(_.antecedents.to(Set).map(phases(_)))
  def schedule(digest: Hash): Dag[Target] = dag(digest).map(_.target)

  def build(target: Target)(using Universe)
      (using Monitor, Clock, Log[Display], WorkingDirectory, Internet, Installation, GitCommand, FrontEnd)
          : Async[Hash] raises BuildError =

    builds.synchronized:
      builds.getOrElseUpdate
       (target,
        async:
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
            
          val digest = workspace(target.projectId)(target.goalId) match
            case module: Module =>
              val phase = module.phase(workspace, target)
              phases(phase.digest) = phase
              phase.digest
            
            case artifact: Artifact =>
              val phase = artifact.phase(workspace, target)
              phases(phase.digest) = phase
              phase.digest
            
            case library: Library =>
              val phase = library.phase(workspace, target)
              phases(phase.digest) = phase
              phase.digest
          
          digest)

  def watchDirectories(hash: Hash): Set[Path] = dag(hash).keys.flatMap(_.watches)

  def outputDirectory(hash: Hash)(using Installation): Path = hash.bytes.encodeAs[Base32].pipe: hash =>
    unsafely(installation.build / PathName(hash.take(2)) / PathName(hash.drop(2)))

  def run(hash: Hash, force: Boolean)
      (using Log[Display], DaemonService[?], Installation, FrontEnd, Monitor, SystemProperties, Environment, Internet)
          : PhaseResult raises CancelError raises StreamError raises
             ZipError raises IoError raises PathError raises BuildError raises ScalacError =

    if force then safely(outputDirectory(hash).as[Directory].delete())
    task(hash).await()

extension (workspace: Workspace)
  def locals()
      (using Monitor, Log[Display], WorkingDirectory, Internet, Installation, GitCommand)
          : Map[ProjectId, Definition] raises CancelError raises WorkspaceError =
    Cache.projectsMap(workspace).await()

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
             FrontEnd,
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

extension (basis: Basis)
  def classpath(using Installation): LocalClasspath =
    LocalClasspath(List(ClasspathEntry.Jarfile(unsafely(basis.path.show))))

  def inclusions: Set[Text] = basis match
    case Basis.Runtime =>
      Set(t"/scala/", t"/rootdoc.txt", t"/library.properties", t"/LICENSE", t"/NOTICE")

    case Basis.Tools =>
      Basis.Runtime.inclusions ++
        Set
         (t"/META-INF/",
          t"/com/sun/",
          t"/compiler.properties",
          t"/dotty/",
          t"/incrementalcompiler.version.properties",
          t"/module-info.class",
          t"/org/",
          t"/scala-asm.properties",
          t"/com/vladsch/",             // FIXME: Remove this when 
          t"/gesticulate/media.types",  // FIXME: Remove this
          t"/xsbti/")
  
  def exclusions: Set[Text] = basis match
    case Basis.Runtime => Set(t"/META-INF/MANIFEST.MF", t"/scala/tasty/")
    case Basis.Tools   => Set(t"/META-INF/MANIFEST.MF")

  def path(using Installation): Path = unsafely(installation.basis / PathName(basis.encode+t".jar"))
    
  def apply()(using FrontEnd, Monitor, Environment, Installation, Log[Display], DaemonService[?])
          : Async[File] raises BuildError =
    async:
      basis.synchronized:
        given (BuildError fixes StreamError) = error => BuildError()
        given (BuildError fixes ZipError)    = error => BuildError()
        given (BuildError fixes IoError)     = error => BuildError()
  
        if !path.exists() then
          val target = unsafely(Target(ProjectId(t"system"), GoalId(basis.encode)))
          summon[FrontEnd].start(target)
          inclusions.pipe: inclusions =>
            exclusions.pipe: exclusions =>
              val entries: LazyList[ZipEntry] =
                ZipFile(service.script.as[File])
                 .entries()
                 .filter: entry =>
                    val name = entry.ref.show
                    inclusions.exists(name.starts(_)) && !exclusions.exists(name.starts(_))
              
              val total: Double = entries.length/100.0
              var done: Int = 0
  
              val trackedEntries: LazyList[ZipEntry] = entries.map: entry =>
                done += 1
                if (done/total).toInt > ((done - 1)/total).toInt
                then summon[FrontEnd](target) = (done/total)/100.0
                
                entry
              
              ZipFile.create(path.as[File].path).tap: zipFile =>
                zipFile.append(trackedEntries, Epoch)
  
          .also:
            summon[FrontEnd].stop(target)
        
        path.as[File]

val errorRibbon = Ribbon(rgb"#990033", rgb"#CC0033")
val warningRibbon = Ribbon(rgb"#FFCC00", rgb"#FFCC99")
val infoRibbon = Ribbon(rgb"#006666", rgb"#6699CC")
