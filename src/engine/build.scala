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
import gastronomy.*

import galilei.*
import filesystemOptions.
   {createNonexistent,
    createNonexistentParents,
    dereferenceSymlinks,
    overwritePreexisting,
    deleteRecursively,
    moveAtomically}

import gossamer.{where as _, *}
import acyclicity.*

import hieroglyph.*,
    charEncoders.utf8,
    charDecoders.utf8,
    textSanitizers.skip,
    textMetrics.uniform

import guillotine.*
import hellenism.*
import telekinesis.*
import hypotenuse.*
import inimitable.*
import feudalism.*
import harlequin.*, syntaxHighlighting.numbered
import monotonous.*, alphabets.base32.zBase32Unpadded
import nettlesome.*
import octogenarian.*
import revolution.*
import parasite.*, orphanDisposal.cancel
import iridescence.*
import contingency.*
import cellulose.*
import dendrology.*
import rudiments.*
import serpentine.*, pathHierarchies.unixOrWindows
import spectacular.*
import turbulence.*
import vacuous.*
import zeppelin.*

import scala.collection.concurrent as scc
import scala.collection.mutable as scm

// inline given (using Log[Message]): Codicil = _.delegate: orphan =>
//   compiletime.summonFrom:
//     case given Log[Text] =>
//       Log.warn(m"Codicil cleaned up an orphan task: ${orphan.stack}")
//       orphan.cancel()
//     case _ =>
//       System.err.nn.println(t"Codicil cleaned up an orphan task: ${orphan.stack}")
//       orphan.cancel()

case class ConfigError(msg: Message) extends Error(msg)

object Config:
  given relabelling: CodlRelabelling[Config] = () => Map(t"ecosystems" -> t"ecosystem")

case class Config(log: LogConfig = LogConfig(), ecosystems: List[EcosystemFork])
case class LogConfig(path: Path = Unix / p"var" / p"log" / p"fury.log")

case class AbortError(n: Int) extends Error(m"the build was aborted by the user ($n)")

case class BuildError(error: Error)
extends Error(m"the build could not run because ${error.message}")

val Isolation: Semaphore = Semaphore()
val Epoch: LocalTime = 2000-Jan-1 at 0.00.am in tz"Etc/UTC"

class Builder():
  private val phases: scc.TrieMap[Hash, Phase] = scc.TrieMap()
  private val builds: scc.TrieMap[Target, Task[Hash]] = scc.TrieMap()
  private val tasks: Mutex[scm.HashMap[Hash, Task[Directory]]] = Mutex(scm.HashMap())

  def runTask(name: Text, hash: Hash)
      (using Monitor,
             Environment,
             FrontEnd,
             SystemProperties,
             Installation,
             DaemonService[?],
             Internet)
          : Task[Directory] logs Message =

    Log.info(m"Building task for $hash")

    tasks.isolate: tasks =>
      synchronized:
        tasks.establish(hash):
          task(name)(phases(hash).run(name, hash))

  extension (library: Library)
    def phase(workspace: Workspace, target: Target)
        (using Installation,
               Internet,
               Monitor,
               WorkingDirectory,
               Universe,
               GitCommand,
               FrontEnd)
            : LibraryPhase raises ConcurrencyError raises PathError raises GitError raises
               BuildError raises ExecError raises IoError logs Message =
      LibraryPhase(installation.build, library, target)

  extension (artifact: Artifact)
    def phase(workspace: Workspace, target: Target)
        (using Installation,
               Internet,
               Monitor,
               WorkingDirectory,
               Universe,
               GitCommand,
               FrontEnd)
            : ArtifactPhase raises ConcurrencyError raises PathError raises GitError raises
               BuildError raises ExecError raises IoError logs Message =

      val destination: Path = workspace(artifact.path)

      val antecedents: Map[Hash, Text] =
        artifact.includes.bi.map(build(_) -> _.show).map: (build, name) =>
          (build.await(), name)
        .to(Map)

      val classpath: List[Hash] =
        antecedents.map(_(0)).map(phases(_)).flatMap(_.runtimeClasspath).to(Set).to(List)

      val prefixPaths = artifact.prefixes.map(workspace(_))
      val suffixPaths = artifact.suffixes.map(workspace(_))
      val counterPath = artifact.counter.let(workspace(_))

      val resourceMap: Map[Path, SafeLink] =
        artifact.resources.map: resource =>
          val path = workspace(resource.path)
          (path, resource.jarPath.lay(? / unsafely(Name(path.name)))(_.link))
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

  extension (exec: Exec)
    def phase(workspace: Workspace, target: Target)
        (using Installation, Internet, Universe, Monitor, WorkingDirectory, FrontEnd, GitCommand)
            : ExecPhase raises ConcurrencyError raises GitError raises PathError raises ExecError raises
               IoError raises BuildError raises StreamError logs Message =

      val antecedents: Map[Hash, Text] =
        exec.includes.bi.map(build(_) -> _.show).map: (build, name) =>
          (build.await(), name)
        .to(Map)

      val classpath: List[Hash] =
        antecedents.keys.map(phases(_)).flatMap(_.runtimeClasspath).to(Set).to(List)

      ExecPhase(installation.build, target, exec, classpath, antecedents, Set())


  extension (module: Module)
    def phase(workspace: Workspace, target: Target)
        (using Installation, Internet, Universe, Monitor, WorkingDirectory, FrontEnd, GitCommand)
            : ModulePhase raises ConcurrencyError raises GitError raises PathError raises ExecError raises
               IoError raises BuildError raises StreamError logs Message =

      val antecedents: Map[Hash, Text] =
        module.includes.bi.map(build(_) -> _.show).map: (build, name) =>
          (build.await(), name)
        .to(Map)

      val classpath: List[Hash] =
        antecedents.keys.map(phases(_)).flatMap(_.runtimeClasspath).to(Set).to(List)

      val compiler = module.compiler match
        case t"java"   => Compiler.Java
        case t"kotlin" => Compiler.Kotlin
        case _         => Compiler.Scala

      val sourceFiles: List[File] =
        module
         .sources
         .map(workspace(_).as[Directory])
         .flatMap(_.descendants.filter(_.is[File]))
         .filter(compiler.compiles(_))
         .map(_.as[File])

      val sourceMap = sourceFiles.map { file => file.path.name -> Cache.file(file.path).text.await() }.to(Map)
      val watches = module.sources.map(workspace(_)).to(Set)

      ModulePhase(installation.build, compiler, module, target, watches, sourceMap, antecedents, classpath)

  object Phase:
    given Phase is Showable as show= _.target.show

  trait Phase:
    def build: Path
    def target: Target
    def watches: Set[Path]
    def antecedents: Map[Hash, Text]
    def classpath: List[Hash]
    def binaries: List[Hash]
    def digest: Hash
    def runtimeClasspath = digest :: classpath

    lazy val output: Path = digest.serialize[Base32].pipe: hash =>
      unsafely(build / Name(hash.keep(2)) / Name(hash.skip(2)))

    def run(name: Text, hash: Hash)
        (using FrontEnd, DaemonService[?], Installation, Monitor, SystemProperties, Environment, Internet)
            : Directory logs Message raises BuildError

  case class ArtifactPhase
     (build:       Path,
      artifact:    Artifact,
      target:      Target,
      destination: Path,
      antecedents: Map[Hash, Text],
      classpath:   List[Hash],
      prefixPaths: List[Path],
      suffixPaths: List[Path],
      counterPath: Optional[Path],
      resourceMap: Map[Path, SafeLink],
      watches:     Set[Path])
  extends Phase:

    export artifact.*
    summon[Hash is Digestible]
    summon[Text is Digestible]
    val digest = antecedents.digest[Sha2[256]]
    val binaries: List[Hash] = Nil

    def run(name: Text, hash: Hash)
        (using FrontEnd,
               DaemonService[?],
               Installation,
               Monitor,
               SystemProperties,
               Environment,
               Internet)
            : Directory logs Message raises BuildError =

      antecedents.each: (hash, name) =>
        runTask(name, hash)

      val checksumPath = output / p"checksum"

      def savedChecksum =
        tend:
          case error: IoError     => BuildError(error)
          case error: StreamError => BuildError(error)
        .within:
          if checksumPath.exists() then checksumPath.as[File].read[Text] else Unset

      def fileChecksum = if !destination.exists() then Unset else
        tend:
          case error: IoError     => BuildError(error)
          case error: StreamError => BuildError(error)
        .within:
          destination.as[File].checksum[Sha2[256]].serialize[Base32]

      if savedChecksum.absent || fileChecksum.absent || savedChecksum != fileChecksum || counterPath.present
      then
        val tmpPath = unsafely(installation.work / Name(Uuid().show))

        val zipFile =
          tend:
            case error: IoError          => BuildError(error)
            case error: StreamError      => BuildError(error)
            case error: ConcurrencyError => BuildError(error)
          .within:
            basis.lay(ZipFile.create(tmpPath)): basis =>
              basis().await().copyTo(tmpPath)
              ZipFile(tmpPath.as[File])

        val todo = (dag(hash) - this).sorted.map(_.digest)
        val total = todo.size.toDouble
        var done = 0

        todo.each: hash =>
          tend:
            case error: ConcurrencyError => BuildError(error)
          .within:
            val directory = tasks()(hash).await()

            if summon[FrontEnd].continue then
              val entries =
                tend:
                  case error: IoError     => BuildError(error)
                  case error: PathError   => BuildError(error)
                  case error: StreamError => BuildError(error)
                .within:
                  directory.descendants.filter(_.is[File]).map: path =>
                    ZipEntry(ZipRef(t"/"+path.relativeTo(directory.path).show), path.as[File])

              val manifestEntry = artifact.main.lay(LazyList()): mainClass =>
                val manifest: Manifest =
                  Manifest
                   (manifestAttributes.ManifestVersion(VersionNumber(1, 0)),
                     manifestAttributes.CreatedBy(t"Fury"),
                     manifestAttributes.MainClass(mainClass))

                tend:
                  case error: PathError => BuildError(error)
                .within(LazyList(ZipEntry(ZipRef(t"/META-INF/MANIFEST.MF"), manifest)))

              val resourceEntries = resourceMap.flatMap: (source, destination) =>
                tend:
                  case error: StreamError => BuildError(error)
                  case error: IoError     => BuildError(error)
                  case error: PathError   => BuildError(error)
                .within:
                  if source.is[Directory]
                  then source.as[Directory].descendants.filter(_.is[File]).map: descendant =>
                    ZipEntry(ZipRef(descendant.relativeTo(source).show), descendant.as[File])
                  else Iterable(ZipEntry(ZipRef(t"/$destination"), source.as[File]))

              tend:
                case error: StreamError => BuildError(error)
                case error: ZipError    => BuildError(error)
              .within(zipFile.append(manifestEntry ++ entries ++ resourceEntries, Epoch))
              done += 1
              summon[FrontEnd](target) = done/total

            else
              tend:
                case error: IoError => BuildError(error)
              .within(tmpPath.wipe())

              abort(BuildError(AbortError(1)))


        tend:
          case error: IoError   => BuildError(error)
        .within(tmpPath.as[File]).tap: file =>
          tend:
            case error: IoError => BuildError(error)
          .within(output.as[Directory])

          if prefixPaths.isEmpty && suffixPaths.isEmpty then file else
            val prefixBytes: LazyList[Bytes] =
              tend:
                case error: IoError     => BuildError(error)
                case error: StreamError => BuildError(error)
              .within:
                prefixPaths.to(LazyList).flatMap(_.as[File].stream[Bytes]).or(LazyList())

            val suffixBytes: LazyList[Bytes] =
              tend:
                case error: IoError     => BuildError(error)
                case error: StreamError => BuildError(error)
              .within:
                suffixPaths.to(LazyList).flatMap(_.as[File].stream[Bytes]).or(LazyList())

            val tmpFile =
              tend:
                case error: IoError => BuildError(error)
              .within:
                unsafely(installation.work / Name(Uuid().show)).as[File]

            tend:
              case error: StreamError => BuildError(error)
              case error: IoError     => BuildError(error)
            .within((prefixBytes ++ file.stream[Bytes] ++ suffixBytes).writeTo(tmpFile))

            tend:
              case error: IoError => BuildError(error)
            .within:
              file.delete()
              tmpFile.moveTo(file.path)


          tend:
            case error: IoError     => BuildError(error)
            case error: StreamError => BuildError(error)
          .within:
            file.checksum[Sha2[256]].serialize[Base32].writeTo(checksumPath.as[File])

          if executable.or(false) then file.executable() = true

          counterPath.let: counter =>
            safely(counter.as[File].read[Text].trim.decodeAs[Int]).let: count =>
              tend:
                case error: IoError     => BuildError(error)
                case error: StreamError => BuildError(error)
              .within(t"${count + 1}\n".writeTo(counter.as[File]))

          tend:
            case error: IoError     => BuildError(error)
          .within:
            destination.wipe()
            file.moveTo(destination)
      else summon[FrontEnd](target) = -1.0

      tend:
        case error: IoError => BuildError(error)
      .within(output.as[Directory])

  case class LibraryPhase(build: Path, library: Library, target: Target) extends Phase:
    val antecedents: Map[Hash, Text] = Map()
    val watches: Set[Path] = Set()
    val classpath: List[Hash] = Nil
    val digest: Hash = library.url.show.digest[Sha2[256]]
    val binaries: List[Hash] = List(digest)

    def run(name: Text, hash: Hash)
        (using FrontEnd, DaemonService[?], Installation, Monitor, SystemProperties, Environment, Internet)
            : Directory logs Message =

      attempt[AggregateError[BuildError]]:
          tend:
            case error: IoError => BuildError(error)
          .within(output.as[Directory])

          val checksum: Path = output / p"checksum"
          val jarfile: Path = output / p"library.jar"

          def jarfileChecksum(): Text =
            tend:
              case error: IoError     => BuildError(error)
              case error: StreamError => BuildError(error)
            .within(jarfile.as[File].checksum[Sha2[256]].serialize[Base32])

          def storedChecksum(): Text =
            tend:
              case error: IoError     => BuildError(error)
              case error: StreamError => BuildError(error)
            .within(checksum.as[File].read[Text].trim)

          if !(jarfile.exists() && checksum.exists() && jarfileChecksum() == storedChecksum())
          then
            tend:
              case error: OfflineError => BuildError(error)
            .within:
              summon[Internet].require: (online: Online) ?=> // FIXME: Why is this necessary?
                Log.info(m"Initiating $target")
                given Message transcribes HttpEvent = _.communicate
                val response: HttpResponse = library.url.get()

                val size: Double =
                  safely:
                    response(ResponseHeader.ContentLength).map(_.long.toDouble).lift(0).getOrElse:
                      Double.MaxValue
                  .or(Double.MaxValue)

                var count: Int = 0

                tend:
                  case error: HttpError   => BuildError(error)
                  case error: IoError     => BuildError(error)
                  case error: StreamError => BuildError(error)
                .within:
                  response.as[LazyList[Bytes]].map: bytes =>
                    count += bytes.length
                    summon[FrontEnd](target) = count/size
                    bytes
                  .writeTo(jarfile.as[File])

                Log.info(m"Downloaded ${target}")

                tend:
                  case error: IoError      => BuildError(error)
                  case error: StreamError  => BuildError(error)
                .within:
                  jarfileChecksum().writeTo(checksum.as[File])
                  output.as[Directory]
          else
            summon[FrontEnd](target) = -1.0

            tend:
              case error: IoError => BuildError(error)
            .within(output.as[Directory])

  case class ExecPhase
      (build:       Path,
       target:      Target,
       exec:        Exec,
       classpath:   List[Hash],
       antecedents: Map[Hash, Text],
       watches:     Set[Path])
  extends Phase:

    def digest = (antecedents).digest[Sha2[256]]
    def binaries: List[Hash] = Nil
    def run(name: Text, hash: Hash)
        (using FrontEnd,
               DaemonService[?],
               Installation,
               Monitor,
               SystemProperties,
               Environment,
               Internet)
            : Directory logs Message raises BuildError =

      val inputs =
        antecedents.to(List).map: (hash, name) =>
          runTask(name, hash)
        .map: task =>
          tend:
            case error: ConcurrencyError => BuildError(error)
          .within(task.await())


      summon[FrontEnd].output(t"\e[K")
      val basis = unsafely(Basis.Runtime().await().path.show)
      val baseClasspath = LocalClasspath(List(ClasspathEntry.Jar(basis)))

      val work =
        tend:
          case error: IoError        => BuildError(error)
          case error@PathError(_, _) => BuildError(error)
        .within:
           (installation.work / Name(Uuid().show)).as[Directory]

      given WorkingDirectory = WorkingDirectory(work.path)

      val allBinaries =
        classpath.flatMap(phases(_).binaries).map(outputDirectory(_) / p"library.jar")

      val javaHome: Path =
        tend:
          case error: PathError           => BuildError(error)
          case error: SystemPropertyError => BuildError(error)
        .within(Properties.java.home[Path]())

      val javaCommand: Path = javaHome / p"bin" / p"java"

      tend:
        case error: PathError => BuildError(error)
        case error: IoError   => BuildError(error)
      .within:
        (classpath.map(outputDirectory(_)) ++ allBinaries).foldLeft(baseClasspath)(_ + _)
      .pipe: classpath =>
        val command = sh"$javaCommand -classpath ${classpath()} ${exec.main}"
        val process = tend { case error: ExecError => BuildError(error) }.within:
          command.fork[ExitStatus]()

        tend { case error: ConcurrencyError => BuildError(error) }.within:
          async:
            tend { case error: StreamError => BuildError(error) }.within:
              process.stdout().stream[Text]
            .each: text =>
              summon[FrontEnd].output(text)
          .await()


        process.await() match
          case ExitStatus.Ok =>
            tend { case error: IoError => BuildError(error) }.within:
              work.moveTo(output).as[Directory]

          case _ =>
            abort(BuildError(AbortError(1)))


  case class ModulePhase
     (build:       Path,
      compiler:    Compiler,
      module:      Module,
      target:      Target,
      watches:     Set[Path],
      sourceMap:   Map[Text, Text],
      antecedents: Map[Hash, Text],
      classpath:   List[Hash])
  extends Phase:

    export module.{compiler as _, *}

    val digest = (sourceMap.values.to(List), antecedents).digest[Sha2[256]]
    val binaries: List[Hash] = Nil

    def run(name: Text, hash: Hash)
        (using FrontEnd,
               DaemonService[?],
               Installation,
               Monitor,
               SystemProperties,
               Environment,
               Internet)
            : Directory logs Message raises BuildError =

      Log.info(m"Starting to build")

      val inputs =
        tend { case error: ConcurrencyError => BuildError(error) }.within:
          antecedents.to(List).map { (hash, name) => runTask(name, hash) }.map(_.await())

      if !summon[FrontEnd].continue then abort(BuildError(AbortError(2)))

      if output.exists() then
        summon[FrontEnd](target) = -1.0
        tend { case error: IoError => BuildError(error) }.within:
          output.as[Directory].tap(_.touch())
      else
        inputs.filter(_.failure).each: input =>
          input.acknowledge:
            case AggregateError(errors) => errors.each:
              case BuildError(error) =>
                Log.warn(m"There was a build error in an input to $target: ${error.message}")
                report(error.stackTrace.teletype)

        if inputs.exists(_.failure)
        then
          Log.info(m"One of the inputs did not complete")
          abort(BuildError(AbortError(3)))
        else
          val work =
            tend:
              case error: IoError   => BuildError(error)
              case error: PathError => BuildError(error)
            .within((installation.work / Name(Uuid().show)).as[Directory])

          val basis = unsafely(Basis.Tools().await().path.show)
          val baseClasspath = LocalClasspath(List(ClasspathEntry.Jar(basis)))
          val syntax: scc.TrieMap[Text, Task[IArray[Seq[SourceToken]]]] = scc.TrieMap()

          def highlight(filename: Text): SourceCode =
            SourceCode(Scala, sourceMap(filename))

          val allBinaries =
            classpath.flatMap(phases(_).binaries).map(outputDirectory(_) / p"library.jar")

          val classpathEntries =
            tend:
              case error: PathError => BuildError(error)
              case error: IoError   => BuildError(error)
            .within:
              (classpath.map(outputDirectory(_)) ++ allBinaries).foldLeft(baseClasspath)(_ + _)

          classpathEntries.pipe: classpath =>
            given BuildError mitigates IoError = BuildError(_)

            if sourceMap.isEmpty then output.as[Directory] else
              val process: CompileProcess =
                //Log.envelop(target):
                  compiler match
                    case Compiler.Java =>
                      tend { case error: CompileError => BuildError(error) }.within:
                        Javac(Nil)(classpath)(sourceMap, work.path)

                    case Compiler.Kotlin =>
                      abort(BuildError(AbortError(4)))

                    case Compiler.Scala =>
                      import scalacOptions.*

                      tend:
                        case error: CompileError => BuildError(error)
                      .within:
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
                            warnings.unused(Unused.All),
                            warnings.implausiblePatterns))
                         (classpath)
                         (sourceMap, work.path)

              val cancellation = daemon:
                summon[FrontEnd].attend()
                process.abort()

              val progressDaemon = daemon:
                process.progress.each: progress =>
                  summon[FrontEnd](target) = progress.complete

              task(t"$target.notices"):
                process.notices.each: notice =>
                  notice.importance match
                    case Importance.Error =>
                      report(errorRibbon.fill(e"$target", notice.file.teletype))

                    case Importance.Warning =>
                      report(warningRibbon.fill(e"$target", notice.file.teletype))

                    case Importance.Info =>
                      report(infoRibbon.fill(e"$target", notice.file.teletype))

                  notice.codeRange.let: range =>
                    val source: SourceCode = SourceCode(Scala, notice.file)
                    report(range.of(source).teletype)

                  report(e"$Italic(${notice.message})")
                  report(t"")

              tend:
                case error: ConcurrencyError => BuildError(error)
              .within(process.complete())

              progressDaemon.attend()
              cancellation.cancel()

              val errorCount = process.notices.count(_.importance == Importance.Error)
              val warnCount = process.notices.count(_.importance == Importance.Warning)

              if errorCount == 0
              then
                tend { case error: IoError => BuildError(error) }.within:
                  work.moveTo(output).as[Directory]
              else abort(BuildError(AbortError(5)))

  given Phase is Expandable = _.antecedents.keys.map(phases(_)).to(List)

  def dag(digest: Hash): Dag[Phase] =
    Dag.create(phases(digest))(_.antecedents.keys.to(Set).map(phases(_)))

  def schedule(digest: Hash): Dag[Target] = dag(digest).map(_.target)

  def build(target: Target)(using Universe)
      (using Monitor,
             Clock,
             WorkingDirectory,
             Internet,
             Installation,
             GitCommand,
             FrontEnd)
          : Task[Hash] raises BuildError logs Message = synchronized:
    builds.establish(target):
      Log.info(m"Building target $target")
      task(t"$target.digest"):
        Log.info(m"Starting new async for $target")
        val workspace =
          tend:
            case error: RefError         => BuildError(error)
            case error: IoError          => BuildError(error)
            case error: ExecError        => BuildError(error)
            case error: ConcurrencyError => BuildError(error)
            case error: PathError        => BuildError(error)
            case error: WorkspaceError   => BuildError(error)
            case error: GitError         => BuildError(error)
          .within:
            universe(target.projectId).source match
              case workspace: Workspace => workspace

              case vault: Vault =>
                Workspace(Cache(vault.index.releases(target.projectId).repo).await().path)

        Log.info(m"Calculated workspace for $target")

        val goal = workspace(target.projectId)(target.goalId).or:
          abort(BuildError(RefError(target.goalId)))

        val digest: Hash =
          tend:
            case error: ConcurrencyError => BuildError(error)
            case error: IoError          => BuildError(error)
            case error: GitError         => BuildError(error)
            case error: ExecError        => BuildError(error)
            case error: PathError        => BuildError(error)
            case error: StreamError      => BuildError(error)
          .within:
            goal match
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

              case exec: Exec =>
                val phase = exec.phase(workspace, target)
                phases(phase.digest) = phase
                phase.digest


        Log.info(m"Calculated digest for $target")

        digest

  def watchDirectories(hash: Hash): Set[Path] = dag(hash).keys.flatMap(_.watches)

  def outputDirectory(hash: Hash)(using Installation): Path =
    hash.serialize[Base32].pipe: hash =>
      unsafely(installation.build / Name(hash.keep(2)) / Name(hash.skip(2)))

  def run(name: Text, hash: Hash, force: Boolean)
      (using DaemonService[?],
             Installation,
             FrontEnd,
             Monitor,
             SystemProperties,
             Environment,
             Internet)
          : Directory raises ConcurrencyError raises StreamError raises ZipError raises
             IoError raises PathError raises BuildError raises CompileError logs Message =

    if force then outputDirectory(hash).wipe()
    runTask(name, hash).await()

extension (workspace: Workspace)
  def locals()
      (using Monitor, WorkingDirectory, Internet, Installation, GitCommand)
          : Map[ProjectId, Definition] raises ConcurrencyError raises WorkspaceError logs Message =
    Cache.projectsMap(workspace).await()

  def universe()
      (using Monitor, Clock, WorkingDirectory, Internet, Installation, GitCommand)
          : Universe raises ConcurrencyError raises VaultError raises WorkspaceError logs Message =
    Log.info(m"Constructing universe")

    given Timezone = tz"Etc/UTC"
    Log.info(m"Got timezone")
    val vaultProjects = Cache(workspace.ecosystem).await()
    Log.info(m"Got vaultProjects")
    val localProjects = locals()
    Log.info(m"Got locals")

    val projects: Map[ProjectId, Definition] =
      vaultProjects.releases.filter(_.expiry <= today()).map: release =>
        (release.id, release.definition(vaultProjects))
      .to(Map)
    Log.info(m"Got projects")

    Universe(projects -- localProjects.keySet ++ localProjects)

  def apply(projectId: ProjectId): Project = workspace.projects(projectId)

  def apply(path: WorkPath)
      (using Installation,
             Internet,
             Monitor,
             WorkingDirectory,
             FrontEnd,
             GitCommand,
             Tactic[ConcurrencyError],
             Tactic[GitError],
             Tactic[PathError],
             Tactic[ExecError],
             Tactic[IoError])
          : Path logs Message =

    workspace.mounts.keys.where(_.precedes(path)).lay(workspace.directory.path + path.link): mount =>
      Cache(workspace.mounts(mount).repo).await().path + path.link

def universe(using universe: Universe): Universe = universe

case class Universe(projects: Map[ProjectId, Definition]):
  def apply(id: ProjectId): Definition raises RefError =
    projects.getOrElse(id, abort(RefError(id)))

enum Compiler:
  case Java
  case Scala
  case Kotlin

  def compiles(path: Path): Boolean = this match
    case Java    => path.name.ends(t".java")
    case Scala   => path.name.ends(t".scala") || path.name.ends(t".java")
    case Kotlin  => path.name.ends(t".kt")

extension (basis: Basis)
  def classpath(using Installation): LocalClasspath =
    LocalClasspath(List(ClasspathEntry.Jar(unsafely(basis.path.show))))

  def inclusions: Set[Text] = basis match
    case Basis.Minimum =>
      Set()

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
          t"/com/vladsch/",             // FIXME: Remove this
          t"/gesticulate/media.types",  // FIXME: Remove this
          t"/xsbti/")

  def exclusions: Set[Text] = basis match
    case Basis.Runtime => Set(t"/META-INF/MANIFEST.MF", t"/scala/tasty/")
    case Basis.Tools   => Set(t"/META-INF/MANIFEST.MF")
    case Basis.Minimum => Set()

  def path(using Installation): Path =
    unsafely(installation.basis / Name(t"${basis.encode}-${installation.buildId}.jar"))

  def apply()(using FrontEnd, Monitor, Environment, Installation, DaemonService[?])
  : Task[File] raises BuildError logs Message =
    task(t"basis"):
      basis.synchronized:

        if !path.exists() then
          val target = unsafely(Target(ProjectId(t"system"), GoalId(basis.encode)))
          summon[FrontEnd].start(target)
          inclusions.pipe: inclusions =>
            exclusions.pipe: exclusions =>
              val entries: LazyList[ZipEntry] =
                tend:
                  case error: IoError     => BuildError(error)
                  case error: StreamError => BuildError(error)
                .within:
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

              tend:
                case error: ZipError    => BuildError(error)
                case error: StreamError => BuildError(error)
                case error: IoError     => BuildError(error)
              .within:
                ZipFile.create(path.as[File].path).tap: zipFile =>
                  zipFile.append(trackedEntries, Epoch)

          .also:
            summon[FrontEnd].stop(target)

        tend:
          case error: IoError => BuildError(error)
        .within(path.as[File])

val errorRibbon = Ribbon(rgb"#990033", rgb"#CC0033")
val warningRibbon = Ribbon(rgb"#FF9900", rgb"#FFCC66")
val infoRibbon = Ribbon(rgb"#006666", rgb"#6699CC")

extension (ecosystem: Ecosystem)
  def path(using installation: Installation)(using WorkingDirectory)
      : Path raises PathError logs Message =

    val localPath: Optional[Path] =
      installation.config.ecosystems.where(_.id == ecosystem.id).let(_.path)

    localPath.or:
      installation.vault.path / Name(ecosystem.id.show) / Name(ecosystem.branch.show)
