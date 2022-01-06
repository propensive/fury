package vex

import gossamer.*
import rudiments.*
import acyclicity.*
import euphemism.*
import jovian.*
import guillotine.*
import escapade.*
import gastronomy.*
import iridescence.*, solarized.*
import eucalyptus.*
import slalom.*
import exoskeleton.*
import profanity.*
import xylophone.*
import java.nio.BufferOverflowException

import scala.collection.mutable as scm
import scala.concurrent.*

import encodings.Utf8

given Env = envs.enclosing
import rendering.ansi

val scalacliRealm = Realm(t"scalacli")
given LogFormat[Stdout.type, AnsiString] = LogFormat.timed
given Log()

erased given throwables: CanThrow[AppError | RootParentError] = compiletime.erasedValue

case class Build(pwd: Unix.Directory, index: Map[Text, Step] = Map()):
  val steps: Set[Step] = index.values.to(Set)
  
  lazy val graph: Dag[Step] throws BrokenLinkError =
    val links = index.values.map:
      step => step -> step.links.map(resolve(_)).to(Set)
    
    Dag(links.to(Seq)*)

  lazy val linearization: List[Step] throws BrokenLinkError = graph.sorted
  def sourceDirs: List[Unix.Directory] = steps.flatMap(_.sources).to(List)

  lazy val hashes: Map[Step, Text] throws BrokenLinkError | IoError | StreamCutError |
      ExcessDataError =
    def recur(todo: List[Step], hashes: Map[Step, Text])
             : Map[Step, Text] throws BrokenLinkError | StreamCutError | ExcessDataError =
      if todo.isEmpty then hashes
      else try
        val step = todo.head
        val inputsHash: List[Bytes] = (step.srcFiles.to(List) ++ step.jars.to(List)).map(Vex.hashFile)
        val linksHash: List[Bytes] = step.links.to(List).map(index(_)).map(hashes(_).bytes)
        val newHash = (inputsHash ++ linksHash).digest[Crc32].encode[Base64]
        
        recur(todo.tail, hashes.updated(step, newHash))
      catch case err: Error => throw AppError(t"An unknown error occurred", err)
    val t0 = System.currentTimeMillis
    val result = recur(linearization, Map())
    val time = System.currentTimeMillis - t0
    //Out.println(ansi"Calculated ${Yellow}(${index.size}) hashes in ${Yellow}[${time}ms]")
    result

  @targetName("addAll")
  infix def ++(build: Build): Build = Build(build.pwd, index ++ build.index)
  
  def resolve(id: Text): Step throws BrokenLinkError = index.get(id).getOrElse:
    throw BrokenLinkError(id)
  
  def transitive[T](fn: Step => Set[T])(step: Step): Set[T] throws BrokenLinkError =
    graph.reachable(step).flatMap(fn)
  
  def transitiveIn[T](fn: Step => Set[T])(step: Step): Set[T] throws BrokenLinkError =
    (graph.reachable(step) - step).flatMap(fn)

  def bases: Set[Unix.Directory] = steps.map(_.pwd).to(Set)

  def cache: Map[Step, Text] =
    try
      val caches = bases.map(_.path / t".vex").filter(_.exists()).map:
        cacheFile => Json.parse(cacheFile.file.read[Text](64.kb)).as[Cache].hashes
      
      caches.flatten.flatMap:
        case Hash(id, hash, _) => try Set(resolve(id) -> hash) catch case e: BrokenLinkError => Set()
      .to(Map)
    catch
      case err: JsonParseError  => throw AppError(t"The cache file is not in the correct format", err)
      case err: StreamCutError  => throw AppError(t"The stream was cut while reading the cache file", err)
      case err: IoError         => throw AppError(t"There was an IO error while reading the cache",err)
      case err: JsonTypeError   => throw AppError(t"The cache file did not contain the correct JSON format", err)
      case err: JsonAccessError => throw AppError(t"The cache file did not contain the correct JSON format", err)
      case err: Error           => throw AppError(t"An unexpected error occurred", err)
  
  def updateCache(step: Step, binDigest: Text): Unit =
    synchronized:
      try
        val cacheFile = (step.pwd / t".vex")
        val cache =
          if cacheFile.exists()
          then Cache:
            Json.parse(cacheFile.file.read[Text](64.kb)).as[Cache].hashes.filter:
              hash => try resolve(hash.id).pwd == step.pwd catch case err: BrokenLinkError => false
          else Cache(Set())
        
        val newHash = Hash(step.id, hashes(step), binDigest)
        val newCache = cache.copy(hashes = cache.hashes.filter(_.id != step.id) + newHash)
        newCache.json.show.bytes.writeTo(cacheFile.createFile(overwrite = true))
      catch
        case err: JsonParseError  => throw AppError(t"The cache file is not in the correct format", err)
        case err: StreamCutError  => throw AppError(t"The stream was cut while reading the cache file", err)
        case err: IoError         => throw AppError(t"There was an IO error while reading the cache", err)
        case err: JsonTypeError   => throw AppError(t"The cache file did not contain the correct JSON format", err)
        case err: JsonAccessError => throw AppError(t"The cache file did not contain the correct JSON format", err)
        case err: Error           => throw AppError(t"An unexpected error occurred", err)

case class Step(path: Unix.File, config: Config, publishing: Option[Publishing], name: Text,
                    id: Text, links: Set[Text], resources: Set[Unix.Directory],
                    sources: Set[Unix.Directory], jars: Set[Unix.File],
                    dependencies: Set[Text], version: Text, docs: List[Unix.IoPath],
                    artifact: Option[Unix.IoPath], main: Option[Text]):
  def projectId: Text = id.cut(t"/").head
  def moduleId: Text = id.cut(t"/").last
  def dashed: Text = t"$projectId-$moduleId"
  def pwd: Unix.Directory = path.parent

  def group: Text =
    publishing.getOrElse:
      throw AppError(t"Publishing details have not been specified")
    .group

  def pkg: Unix.IoPath = output(t".jar")
  def docFile: Unix.IoPath = output(t"-javadoc.jar")
  def srcsPkg: Unix.IoPath = output(t"-sources.jar")
  def pomFile: Unix.IoPath = output(t".pom")

  private def output(extension: Text): Unix.IoPath =
    pwd / t"bin" / t"$dashed-$version$extension"
  
  def compilable(f: Unix.File): Boolean = f.name.endsWith(t".scala") || f.name.endsWith(t".java")

  def srcFiles: Set[Unix.File] throws IoError =
    sources.flatMap(_.path.descendantFiles(!_.name.startsWith(t"."))).filter(compilable)

  def dependencySteps(build: Build): Set[Step] throws BrokenLinkError =
    dependencies.map(build.resolve(_))

  def classpath(build: Build): Set[Unix.IoPath] throws IoError | BrokenLinkError | AppError =
    build.graph.reachable(this).flatMap:
      step => step.jars.map(_.path) + step.cacheDir.path
  
  def compileClasspath(build: Build): Set[Unix.IoPath] throws IoError | BrokenLinkError | AppError =
    classpath(build) - cacheDir.path

  def cacheDir: Unix.Directory =
    try
      val homeDir = Unix.parse(Sys.user.home()).get.directory(Expect)
      (homeDir / t".cache" / t"vex" / projectId / moduleId).directory(Ensure)
    catch
      case err: KeyNotFoundError =>
        throw AppError(t"The user's home directory could not be determined", err)
      
      case err: IoError =>
        throw AppError(t"Could not write to the user's home directory", err)

  def runClasspath(build: Build): Set[Unix.IoPath] throws IoError | BrokenLinkError | AppError =
    build.graph.reachable(this).flatMap:
      step => step.jars.map(_.path) + step.pkg

  def pomDependency(build: Build): Dependency = Dependency(group, dashed, version)

  def pomDependencies(build: Build): List[Dependency] =
    try links.map(build.resolve(_)).map(_.pomDependency(build)).to(List)
    catch case err: BrokenLinkError => throw AppError(t"Couldn't resolve dependencies", err)

  def compile(hashes: Map[Step, Text], oldHashes: Map[Step, Text], build: Build)(using Drain)
             : Unit =
    try
      //val srcs = srcFiles.map(_.path.relativeTo(build.pwd.path).show).to(List)
      if !pkg.parent.exists() then pkg.parent.directory(Create)
      //val classpath = build.transitive(_.classpath(build))(this).to(List).map(_.relativeTo(build.pwd.path).show).flatMap(List(t"--jar", _))
      val resourceArgs = build.transitive(_.resources)(this).map(_.path.relativeTo(build.pwd.path).show).to(List).flatMap(List(t"--resource", _))
      val extraJars = build.transitive(_.jars)(this).map(_.path.relativeTo(build.pwd.path).show).to(List).flatMap(List(t"--jar", _))
      val deps = build.transitive(_.dependencies)(this).map(_.show).to(List).flatMap(List(t"-d", _))
      val relativePkg = pkg.relativeTo(build.pwd.path).show
      
      val t0 = System.currentTimeMillis
      val diagnostics = Compiler.compile(srcFiles.to(List), compileClasspath(build), cacheDir)
      diagnostics.map(_.toString.show).foreach(Out.println)

      
      //val process = cmd.fork[ExitStatus]()
      
      val time = System.currentTimeMillis - t0
      
      //process.await() match
      if diagnostics.filter(_.level == 2).isEmpty then
        //case ExitStatus.Ok =>
          Out.println(ansi"Compilation of ${Green}[${name}] succeeded in ${Yellow}[${time}ms]")
          
          // if !resources.isEmpty then
          //   val resourceArgs = resources.to(List).flatMap:
          //     dir =>
          //       Out.println(t"Adding resource dir $dir")
          //       t"-C" :: dir.show :: dir.path.descendantFiles(!_.name.startsWith(t".")).to(List).map(_.path.relativeTo(dir.path).show)
            
          //   sh"jar uf $pkg $resourceArgs".exec[ExitStatus]() match
          //     case ExitStatus.Ok      => Out.println(t"Added files to $pkg")
          //     case ExitStatus.Fail(n) => Out.println(t"Failed to add files to $pkg")
          
          // val stream = pkg.file.read[DataStream]().map:
          //   chunk => try chunk catch case err: StreamCutError => Bytes()
          // val binDigest = stream.digest[Crc32].encode[Base64]
          // build.updateCache(this, binDigest)
          
          //locally:
            //given Realm = scalacliRealm
            //val stderr: Text = process.stderr(1.mb).slurp(1.mb).uString
            //val stdout: Text = process.stdout(1.mb).slurp(1.mb).uString
            //stdout.cut(t"\n").filter(!_.isEmpty).foreach(Out.println(_))
            //stderr.cut(t"\n").filter(!_.isEmpty).foreach(Out.println(_))
          
          //main.foreach:
          //  mainClass => sh"scala-cli run -M $mainClass $classpath $resourceArgs $extraJars $deps -S ${config.scalac.version} --progress ${config.scalac.options} $srcs".exec[LazyList[Text]]().foreach(Out.println(_))

        //case ExitStatus.Fail(n) =>
      else
        Out.println(ansi"Compilation of ${Green}[${name}] failed in ${Yellow}[${time}ms]")
          //given Realm = scalacliRealm
          //val stderr: Text = process.stderr(1.mb).slurp(1.mb).uString
          //val stdout: Text = process.stdout(1.mb).slurp(1.mb).uString
          //stdout.cut(t"\n").filter(!_.isEmpty).foreach(Out.println(_))
          //stderr.cut(t"\n").filter(!_.isEmpty).foreach(Out.println(_))
    
    catch
      case err: IoError =>
        throw AppError(t"Could not read the source files", err)
      
      case err: StreamCutError =>
        throw AppError(t"The stream from scala-cli was broken before it completed", err)

      case err: ExcessDataError =>
        throw AppError(t"The scala-cli process returned too much data", err)

      case err: BrokenLinkError => err match
        case BrokenLinkError(ref) =>
          throw AppError(t"There was an unsatisfied reference to $ref", err)

      case err: Error =>
        throw AppError(t"An unexpected error occurred", err)

case class BuildConfig(imports: Option[List[Text]], config: Config, publishing: Option[Publishing],
                           modules: List[Module]):
  
  def gen(build: Build, seen: Set[Text], files: Unix.File*)(using Drain)
         : Build throws IoError | AppError | BuildfileError =
    files.to(List) match
      case Nil =>
        build
      
      case file :: tail =>
        Out.println(ansi"Reading build file $Violet(${file.path.relativeTo(build.pwd.path).show})".render)
        val steps: Map[Text, Step] = modules.map:
          module =>
            def relativize(text: Text): Unix.IoPath = file.parent.path ++ Relative.parse(text)
            val links = module.links.getOrElse(Set())
            val resources = module.resources.getOrElse(Set()).map(relativize).map(_.directory(Expect))
            val sources = module.sources.map(relativize).map(_.directory(Expect))
            val jars = module.jars.getOrElse(Set()).map(relativize).map(_.file)
            val dependencies = module.dependencies.getOrElse(Set())
            val docs = module.docs.getOrElse(Nil).map(relativize)
            val version = module.version.getOrElse(t"1.0.0")
            val artifactPath = module.artifact.map(file.parent.path ++ Relative.parse(_))
            
            Step(file, config, publishing, module.name, module.id, links, resources, sources, jars,
                dependencies, version, docs, artifactPath, module.main)
        .mtwin.map(_.id -> _).to(Map)
        
        val importFiles = imports.getOrElse(Nil).map:
          path => (file.parent.path ++ Relative.parse(path)).file
        
        Vex.readBuilds(build ++ Build(build.pwd, steps), seen, (importFiles ++ tail)*)

case class Publishing(username: Text, group: Text, url: Text, organization: Organization,
                          developers: List[Developer])

case class Config(scalac: Scalac)
case class Scalac(version: Text, options: List[Text])

case class Module(name: Text, id: Text, links: Option[Set[Text]], resources: Option[Set[Text]],
                      sources: Set[Text], jars: Option[Set[Text]], docs: Option[List[Text]],
                      dependencies: Option[Set[Text]], version: Option[Text],
                      artifact: Option[Text], main: Option[Text])

case class AppError(message: Text, cause: Maybe[Error] = Unset) extends Error(cause)

case class BuildfileError() extends Error:
  def message: Text = t"There was an error in the buildfile"

case class BrokenLinkError(link: Text) extends Error:
  def message: Text = t"The reference to $link cannot be resolved"

object Vex extends ServerApp():
  def main(using cli: CommandLine): ExitStatus = try
    //Out.println(cli.scriptName)
    //Out.println(cli.scriptDir.option.fold(t"nothing")(_.fullname))
    //val file = (cli.scriptDir.option.get / cli.scriptName).file
    //Zip.read(file, 1247).map(_.toString.show).foreach(Out.println(_))

    cli.args match
      case t"compile" :: params => Vex.build(false, params.headOption.map(_.show), false, None, cli.pwd)
      case t"publish" :: params => Vex.build(true, params.headOption.map(_.show), false, None, cli.pwd)
      case t"watch" :: params   => Vex.build(false, params.headOption.map(_.show), true, None, cli.pwd)
      case t"stop" :: params    => Vex.stop()
      case params               => Vex.build(false, params.headOption.map(_.show), false, None, cli.pwd)
  
  catch case err: AppError =>
    Out.println(err.stackTrace.ansi)


    ExitStatus.Fail(1)
  
  private lazy val fileHashes: FileCache[Bytes] = new FileCache()

  private def init(target: Option[Text], pwd: Unix.Directory)(using Drain)
                  : Build throws IoError | BuildfileError =
    val path = pwd / (target.getOrElse(t"build")+t".vex")
    
    try readBuilds(Build(pwd), Set(), path.file)
    catch case err: IoError =>
      Out.println(ansi"Configuration file $Violet(${path.show})")
      sys.exit(1)
  
  def hashFile(file: Unix.File): Bytes throws IoError | AppError =
    try fileHashes(file.path.show, file.modified):
      file.read[Bytes](1.mb).digest[Crc32].bytes
    catch
      case err: StreamCutError  => throw AppError(t"The stream was cut while hashing a file", err)
      case err: ExcessDataError => throw AppError(t"The file was too big to hash", err)
      case err: Error           => throw AppError(t"An unexpected error occurred", err)
  
  def readBuilds(build: Build, seen: Set[Text], files: Unix.File*)(using Drain)
                : Build throws BuildfileError =
    try
      files.to(List) match
        case Nil =>
          build
        
        case file :: tail =>
          val digest: Text = hashFile(file).encode[Base64]
          if !seen.contains(digest)
          then Json.parse(file.read[Text](1.mb)).as[BuildConfig].gen(build, seen + digest, files*)
          else readBuilds(build, seen, tail*)
    catch
      case err: IoError => err match
        case IoError(op, reason, path) =>
          Out.println(t"The $op operation at ${path.toString} did not complete because $reason")
          throw BuildfileError()
      
      case err: ExcessDataError =>
        Out.println(t"The configuration file was larger than 1MB")
        throw BuildfileError()
      
      case err: StreamCutError =>
        throw AppError(t"The configuration file could not be read completely", err)
      
      case err: JsonParseError =>
        Out.println(t"The configuration file was not valid JSON")
        throw BuildfileError()
      
      case err: AppError =>
        throw err

      case err: BuildfileError =>
        throw AppError(t"There was an error in the buildfile", err)

      case err: Error =>
        throw AppError(t"An unexpected error occurred", err)

  def readImports(seen: Map[Text, Unix.File], files: Unix.File*): Set[Unix.File] =
    case class Imports(imports: Option[List[Text]]):
      def gen(seen: Map[Text, Unix.File], files: Unix.File*): Set[Unix.File] =
        files.to(List) match
          case file :: tail =>
            val importFiles = imports.getOrElse(Nil).flatMap:
              path =>
                try List((file.parent.path ++ Relative.parse(path)).file)
                catch case err: IoError => Nil
            
            readImports(seen, (importFiles ++ tail)*)
          
          case Nil => seen.values.to(Set)
    
    files.to(List) match
      case Nil =>
        seen.values.to(Set)
      
      case file :: tail =>
        try
          val digest: Text = hashFile(file).encode[Base64]
          if !seen.contains(digest)
          then Json.parse(file.read[Text](1.mb)).as[Imports].gen(seen.updated(digest, file), files*)
          else readImports(seen, tail*)
        catch case err: Error => readImports(seen, tail*)

  def build(publishSonatype: Boolean, target: Option[Text], watch: Boolean = false,
                oldBuild: Option[Build], pwd: Unix.Directory)(using Drain)
           : ExitStatus =
    try
      import unsafeExceptions.canThrowAny
      val build = oldBuild.getOrElse(init(target, pwd))
      val oldHashes = build.cache
      given ExecutionContext = ExecutionContext.global
      Out.println("Starting a build")

      val futures = build.graph.traversal[Future[Unit]]:
        (set, step) => Future.sequence(set).flatMap:
          units => Future:
            try
              if oldHashes.get(step) != build.hashes.get(step) || !step.pkg.exists() ||
                  step.main.isDefined
              then
                Out.println(ansi"Compiling ${Green}[${step.name}]...")
                step.compile(build.hashes, oldHashes, build)
            catch
              case err: ExcessDataError => Out.println(t"Too much data")
              case err: StreamCutError  => Out.println(t"The stream was cut prematurely")
      .values
      
      Future.sequence(futures).await()
      Out.println(t"Compilation complete")
      val tmpPath: Unix.IoPath = pwd / t".tmp"

      build.linearization.foreach:
        step =>
          step.artifact.foreach:
            artifact =>
              val tmp = tmpPath.directory(Ensure)
              Out.println(ansi"Building $Violet(${artifact.show}) artifact")
              step.runClasspath(build).filter(_.name.endsWith(t".jar")).foreach:
                jarFile =>
                  try sh"unzip -o -d $tmp $jarFile".exec[Unit]()
                  catch case err: Error => throw AppError(t"Could not unzip files", err)
              val includes = tmp.children.map(_.name).flatMap(List(t"-C", tmp.name, _))
              try 
                val subCmd: List[Text] =
                  step.main.fold(List(t"cf", artifact.show))(List(t"cfe", artifact.show, _))
                sh"jar $subCmd $includes".exec[Unit]()
              catch case err: Error => throw AppError(t"Could not execute `jar` command", err)

      if publishSonatype then
        val password = Tty.capture:
          Thread.sleep(1000)
          Tty.print(t"Password: ")
          val pw = LineEditor.ask(t"", LineEditor.concealed)
          Tty.print(t"\n")
          pw
        
        build.linearization.groupBy(_.publishing).foreach:
          case (publishing, steps) =>
            val publish: Publishing = publishing.getOrElse:
              throw AppError(t"Publishing settings have not been specified in vex.json")

            val sonatype = Sonatype(publish.username, password, publish.group)
            Out.println(t"Using Sonatype settings ${sonatype.toString}")
            val profileId = sonatype.profile()
            val repoId = sonatype.start(profileId)
                
            steps.foreach:
              step =>
                Out.println(t"Generating POM file for ${step.id}")
                
                val pomXml = Pom(build, step, 2021,
                    t"https://propensive.com/opensource/${step.projectId}",
                    t"github.com/${step.projectId}", publish).xml
                
                pomXml.show.bytes.writeTo(step.pomFile.createFile())
                
                val srcFiles: List[Text] = step.sources.to(List).flatMap:
                  dir =>
                    Out.println(t"Adding source dir $dir")
                    dir.path.descendantFiles(!_.name.startsWith(t".")).filter:
                      file => file.name.endsWith(t".scala") || file.name.endsWith(t".java")
                    .flatMap:
                      file => List(t"-C", dir.show, dir.path.relativeTo(file.path).show)
                
                sh"jar cf ${step.srcsPkg} $srcFiles".exec[ExitStatus]()
                
                val docFiles: List[Text] = step.docs.to(List).flatMap:
                  dir =>
                    Out.println(t"Adding doc dir $dir")
                    dir.descendantFiles(!_.name.startsWith(t".")).flatMap:
                      file => List(t"-C", dir.show, dir.relativeTo(file.path).show)
                
                sh"jar cf ${step.docFile} $docFiles".exec[ExitStatus]()
    
                List(step.docFile, step.pomFile, step.pkg, step.srcsPkg).foreach:
                  file => sh"gpg -ab $file".exec[ExitStatus]()
    
                Out.println(t"Publishing ${step.id}")
                val dir = t"${step.id.sub(t"/", t"-")}/${step.version}"
    
                val uploads = List(step.pkg.file, step.pomFile.file, step.docFile.file,
                    step.srcsPkg.file)
                
                val signedUploads = uploads ++ uploads.map(_.path.rename(_+t".asc").file)
    
                val checksums =
                  signedUploads.map:
                    file =>
                      val checksum = file.path.rename(_+t".sha1")
                      sh"sha1sum $file".exec[Text]().take(40).bytes.writeTo(checksum.createFile())
                      checksum.file
    
                sonatype.deploy(repoId, dir, signedUploads ++ checksums)
            
            sonatype.finish(profileId, repoId)
            sonatype.activity(repoId)
      
      if watch then
        val dirs = build.sourceDirs.to(List) ++ build.bases.to(List)
        Vex.build(false, target, true, if waitForChange(dirs) then None else Some(build), build.pwd)
      else ExitStatus.Ok
    catch
      case err: BrokenLinkError => err match
        case BrokenLinkError(ref) =>
          Out.println(t"The reference to $ref could not be resolved")
          if watch then
            try
              val path = pwd / (target.getOrElse(t"build")+t".vex")
              val dirs = readImports(Map(), path.file).map(_.parent).to(List)
              waitForChange(dirs)
              Vex.build(false, target, true, None, pwd)
            catch case err: Error =>
              Out.println(err.toString.show)
              ExitStatus.Fail(1)
          else ExitStatus.Ok
      case err: IoError =>
        Out.println(err.toString.show)
        if watch then
          try
            val path = pwd / (target.getOrElse(t"build")+t".vex")
            val dirs = readImports(Map(), path.file).map(_.parent).to(List)
            waitForChange(dirs)
            Vex.build(false, target, true, None, pwd)
          catch case err: Error =>
            Out.println(err.toString.show)
            ExitStatus.Fail(1)
        else ExitStatus.Ok
      
      case err: TtyError =>
        Out.println(t"Could not capture TTY")
        ExitStatus.Fail(1)
      
      case err: InotifyError =>
        Out.println(t"The limit of the number if inotify instances has been exceeded")
        ExitStatus.Fail(1)
      
      case err: StreamCutError =>
        Out.println(t"The stream was cut")
        if watch then
          try
            val path = pwd / (target.getOrElse(t"build")+t".vex")
            val dirs = readImports(Map(), path.file).map(_.parent).to(List)
            waitForChange(dirs)
            Vex.build(false, target, true, None, pwd)
          catch case err: Error =>
            Out.println(err.toString.show)
            ExitStatus.Fail(1)
        else ExitStatus.Ok
      
      case err: ExcessDataError =>
        Out.println(t"Too much data")
        if watch then
          try
            val path = pwd / (target.getOrElse(t"build")+t".vex")
            val dirs = readImports(Map(), path.file).map(_.parent).to(List)
            waitForChange(dirs)
            Vex.build(false, target, true, None, pwd)
          catch case err: Error =>
            Out.println(err.toString.show)
            ExitStatus.Fail(1)
        else ExitStatus.Ok
      
      case err: BuildfileError =>
        Out.println(t"There was an error with the build file")
        if watch then
          try
            val path = pwd / (target.getOrElse(t"build")+t".vex")
            val dirs = readImports(Map(), path.file).map(_.parent).to(List)
            waitForChange(dirs)
            Vex.build(false, target, true, None, pwd)
          catch case err: Error =>
            Out.println(err.toString.show)
            ExitStatus.Fail(1)
        else ExitStatus.Ok

  def waitForChange(dirs: List[Unix.Directory])(using Drain)
                   : Boolean throws InotifyError | IoError =
    Out.println(t"Watching ${dirs.size} directories for changes")
    val watchers = dirs.map(_.watch())
    val stream = LazyList.multiplex(watchers.map(_.stream.filter(_ != Unix.FileEvent.NoChange))*)
    if stream.isEmpty then sys.exit(0)
    else
      watchers.foreach(_.stop())
      
      stream.head match
        case Unix.FileEvent.Modify(file) =>
          Out.println(ansi"The file $Violet(${file.path.show}) was modified")
          !file.path.name.startsWith(t".") && file.path.name.endsWith(t".vex")
        
        case Unix.FileEvent.Delete(path) =>
          Out.println(ansi"The file $Violet(${path.show}) was deleted")
          !path.name.startsWith(t".") && path.name.endsWith(t".vex")
        
        case Unix.FileEvent.NewFile(file) =>
          Out.println(ansi"The file $Violet(${file.path.show}) was created")
          !file.path.name.startsWith(t".") && file.path.name.endsWith(t".vex")
        
        case Unix.FileEvent.NewDirectory(dir) =>
          Out.println(ansi"The directory $Violet(${dir.path.show}) was created")
          false
        
        case _ =>
          false

case class Hash(id: Text, digest: Text, bin: Text)
case class Cache(hashes: Set[Hash])
case class Versioning(versions: List[Version])
case class Version(digest: Text, major: Int, minor: Int):
  def version: Text = t"$major.$minor"

@xmlLabel("organization")
case class Organization(name: Text, url: Text)

@xmlLabel("license")
case class License(name: Text, url: Text, distribution: Text)


@xmlLabel("scm")
case class Scm(url: Text, connection: Text)

@xmlLabel("developer")
case class Developer(id: Text, name: Text, url: Text)

@xmlLabel("dependency")
case class Dependency(groupId: Text, artifactId: Text, version: Text)

@xmlLabel("project")
case class Project(modelVersion: Text, groupId: Text, artifactId: Text, version: Text,
    licenses: List[License], name: Text, description: Text, inceptionYear: Text, url: Text,
    organization: Organization, scm: Scm, developers: List[Developer],
    dependencies: List[Dependency])

object Pom:
  def apply(build: Build, step: Step, year: Int, url: Text, git: Text, publishing: Publishing)
           : Project =
    Project(
      modelVersion = t"4.0.0",
      groupId = step.group,
      artifactId = step.dashed,
      version = step.version,
      licenses = List(License(t"Apache 2", t"http://www.apache.org/licenses/LICENSE-2.0.txt", t"repo")),
      name = step.name,
      description = step.name,
      inceptionYear = year.show,
      url = url,
      organization = publishing.organization,
      scm = Scm(t"https://$git/", t"scm:git:git@$git.git"),
      developers = publishing.developers,
      dependencies = step.pomDependencies(build)
    )

class FileCache[T]:
  private val files: scm.HashMap[Text, (Long, T)] = scm.HashMap()
  
  def apply(filename: Text, modified: Long)(calc: => T): T throws IoError =
    if !files.get(filename).fold(false)(_(0) == modified) then files(filename) = modified -> calc
    files(filename)(1)
given realm: Realm = Realm(t"vex")


object Compiler:
  import dotty.tools.dotc.*, reporting.*

  class CustomReporter() extends Reporter:
    var errors: scm.ListBuffer[Diagnostic] = scm.ListBuffer()
    def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit =
      errors += diagnostic

  val Scala3 = new dotty.tools.dotc.Compiler()

  def compile(files: List[Unix.File], inputs: Set[Unix.IoPath], out: Unix.Directory)
             (using Drain)
             : List[Diagnostic] =
    import unsafeExceptions.canThrowAny
    import dotty.tools.*, repl.*, dotc.core.*
    
    val reporter = CustomReporter()
    try
      val separator: Text = try Sys.path.separator() catch case err: KeyNotFoundError => t":"
      val classpath = Unix.parse(t"/home/propensive/niveau/scala/dist/target/pack/lib").get.directory(Expect).children.map(_.fullname)
      val classpath2: List[Text] = inputs.map(_.fullname).to(List)
      //Out.println(t"Extra classpath = ${classpath2.join(t",")}")
      val fullClasspath = (classpath2 ++ classpath).join(separator)
      
      val callbackApi = new interfaces.CompilerCallback:
        override def onClassGenerated(source: interfaces.SourceFile,
                                          generatedClass: interfaces.AbstractFile,
                                          className: String): Unit =
          ()//Out.println(t"Generated class ${className}")
  
        override def onSourceCompiled(source: interfaces.SourceFile): Unit =
          ()//Out.println(t"Compiled source ${source.toString}")
      
      object driver extends dotty.tools.dotc.Driver:
        val currentCtx =
          val ctx = initCtx.fresh
          setup(Array[String]("-d", out.fullname.s, ""), ctx) match
            case Some((_, ctx)) => ctx
            case None           => ctx
        
        def run(files: List[Unix.File], classpath: Text): List[Diagnostic] =
          val ctx = currentCtx.fresh
          val newCtx = ctx
            .setReporter(reporter)
            .setCompilerCallback(callbackApi)
            .setSetting(ctx.settings.language, List("experimental.fewerBraces", "experimental.erasedDefinitions", "experimental.saferExceptions"))
            .setSetting(ctx.settings.classpath, classpath.s)
          
          val sources = files.to(List).map:
            file => dotty.tools.io.PlainFile(dotty.tools.io.Path(file.fullname.s))
          
          val run = Scala3.newRun(using newCtx)
          run.compile(sources)
          finish(Scala3, run)(using newCtx)
          reporter.errors.to(List)

      driver.run(files, fullClasspath)

    catch case err: Throwable =>
      Out.println(err.toString.show)
      if err.getCause != null then Out.println(err.getCause.toString.show)
      reporter.errors.to(List)

object Zip:
  import java.io.*
  import java.util.zip.*
  
  def read(file: Unix.File, skipBytes: Int = 0): LazyList[java.util.zip.ZipEntry] =
    val fis = FileInputStream(file.javaFile).nn
    fis.skipNBytes(skipBytes)
    val in = ZipInputStream(BufferedInputStream(fis).nn)
    LazyList.continually(Option(in.getNextEntry)).takeWhile(_ != None).map(_.get.nn)
  
  def write(file: Unix.File, inputs: ListMap[Text, InputStream]): Unit =
    val fos = FileOutputStream(file.javaFile).nn
