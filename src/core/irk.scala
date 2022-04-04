package irk

import gossamer.*
import rudiments.*
import turbulence.*
import acyclicity.*
import euphemism.*
import jovian.*
import guillotine.*
import kaleidoscope.*
import escapade.*
import gastronomy.*
import harlequin.*
import iridescence.*, solarized.*
import eucalyptus.*
import slalom.*
import exoskeleton.*
import imperial.*
import profanity.*
import xylophone.*
import scintillate.*
import clairvoyant.*
import java.nio.BufferOverflowException

import scala.collection.mutable as scm
import scala.concurrent.*
import scala.collection.convert.ImplicitConversions.given
import scala.util.chaining.scalaUtilChainingOps

erased given CanThrow[AppError | RootParentError] = compiletime.erasedValue

import timekeeping.long
import encodings.Utf8

given Env = envs.enclosing
import rendering.ansi

given LogFormat[SystemOut.type, AnsiText] = LogFormat.timed
given Log()

case class Message(module: Text, path: Text, line: Int, from: Int, to: Int, message: Text,
                       content: IArray[Char])

case class Build(pwd: Directory, repos: Map[DiskPath, Text], publishing: Option[Publishing],
    index: Map[Text, Step] = Map()):
  val steps: Set[Step] = index.values.to(Set)
  
  lazy val graph: Dag[Step] throws BrokenLinkError =
    val links = index.values.map:
      step => step -> step.links.map(resolve(_)).to(Set)
    
    Dag(links.to(Seq)*)

  lazy val linearization: List[Step] throws BrokenLinkError = graph.sorted
  def sourceDirs: List[Directory] = steps.flatMap(_.sources).to(List)
  def resourceDirs: List[Directory] = steps.flatMap(_.resources).to(List)

  lazy val hashes: Map[Step, Text] throws BrokenLinkError | IoError | StreamCutError |
      ExcessDataError =
    def recur(todo: List[Step], hashes: Map[Step, Text])
             : Map[Step, Text] throws BrokenLinkError | StreamCutError | ExcessDataError =
      if todo.isEmpty then hashes
      else try
        val step = todo.head
        val inputsHash: List[Bytes] = step.srcFiles.to(List).map(Irk.hashFile)
        val jarsHash: List[Bytes] = step.jars.to(List).map(_.digest[Crc32].bytes)
        val linksHash: List[Bytes] = step.links.to(List).map(index(_)).map(hashes(_).bytes)
        val newHash = (inputsHash ++ linksHash).digest[Crc32].encode[Base64]
        
        recur(todo.tail, hashes.updated(step, newHash))
      catch case err: Error => throw AppError(t"An unknown error occurred", err)
    val t0 = System.currentTimeMillis
    val result = recur(linearization, Map())
    val time = System.currentTimeMillis - t0
    //Out.println(ansi"Calculated ${colors.Gold}(${index.size}) hashes in ${colors.Gold}[${time}ms]")
    result

  @targetName("addAll")
  infix def ++(build: Build): Build =
    Build(build.pwd, build.repos, publishing.orElse(build.publishing), index ++ build.index)
  
  def resolve(id: Text): Step throws BrokenLinkError = index.get(id).getOrElse:
    throw BrokenLinkError(id)
  
  def transitive[T](fn: Step => Set[T])(step: Step): Set[T] throws BrokenLinkError =
    graph.reachable(step).flatMap(fn)
  
  def transitiveIn[T](fn: Step => Set[T])(step: Step): Set[T] throws BrokenLinkError =
    (graph.reachable(step) - step).flatMap(fn)

  def bases: Set[Directory] = steps.map(_.pwd).to(Set)

  def cache: Map[Step, Text] =
    try
      val caches = bases.map(Irk.hashesDir / _.path.show.digest[Crc32].encode[Hex]).filter(_.exists()).map:
        cacheFile => Json.parse(cacheFile.file().read[Text](64.kb)).as[Cache].hashes
      
      caches.flatten.flatMap:
        case Hash(id, hash, _) =>
          try Set(resolve(id) -> hash) catch case e: BrokenLinkError => Set()
      .to(Map)
    catch
      case err: JsonParseError =>
        throw AppError(t"The cache file is not in the correct format", err)
      
      case err: StreamCutError =>
        throw AppError(t"The stream was cut while reading the cache file", err)
      
      case err: IoError =>
        throw AppError(t"There was an IO error while reading the cache",err)
      
      case err: JsonAccessError =>
        throw AppError(t"The cache file did not contain the correct JSON format", err)
      
      case err: Error =>
        throw AppError(t"An unexpected error occurred", err)
  

  def updateCache(step: Step, binDigest: Text): Unit =
    synchronized:
      try
        val cacheFile = Irk.hashesDir / step.pwd.path.show.digest[Crc32].encode[Hex]
        val cache =
          if cacheFile.exists()
          then Cache:
            Json.parse(cacheFile.file().read[Text](64.kb)).as[Cache].hashes.filter:
              hash => try resolve(hash.id).pwd == step.pwd catch case err: BrokenLinkError => false
          else Cache(Set())
        
        val newHash = Hash(step.id, hashes(step), binDigest)
        val newCache = cache.copy(hashes = cache.hashes.filter(_.id != step.id) + newHash)
        
        newCache.json.show.bytes.writeTo:
          if cacheFile.exists() then cacheFile.file(Expect).delete()
          cacheFile.file()
      catch
        case err: JsonParseError =>
          throw AppError(t"The cache file is not in the correct format", err)
        
        case err: StreamCutError =>
          throw AppError(t"The stream was cut while reading the cache file", err)
        
        case err: IoError =>
          throw AppError(t"There was an IO error while reading the cache", err)
        
        case err: JsonAccessError =>
          throw AppError(t"The cache file did not contain the correct JSON format", err)
        
        case err: Error =>
          throw AppError(t"An unexpected error occurred", err)


case class Step(path: File, publishing: Option[Publishing], name: Text,
                    id: Text, links: Set[Text], resources: Set[Directory],
                    sources: Set[Directory], jars: Set[Text], dependencies: Set[Dependency],
                    version: Text, docs: List[DiskPath], artifact: Option[DiskPath],
                    main: Option[Text]):
  def publish(build: Build): Publishing = publishing.orElse(build.publishing).getOrElse:
    throw AppError(t"There are no publishing details for $id")

  def projectId: Text = id.cut(t"/").head
  def moduleId: Text = id.cut(t"/").last
  def dashed: Text = t"$projectId-$moduleId"
  def pwd: Directory = path.parent
  def group(build: Build): Text = publish(build).group
  def docFile: DiskPath = output(t"-javadoc.jar")
  def srcsPkg: DiskPath = output(t"-sources.jar")
  def pomFile: DiskPath = output(t".pom")

  private def output(extension: Text): DiskPath =
    pwd / t"bin" / t"$dashed-$version$extension"
  
  def compilable(f: File): Boolean = f.name.endsWith(t".scala") || f.name.endsWith(t".java")

  def srcFiles: Set[File] throws IoError =
    sources.flatMap(_.path.descendantFiles(!_.name.startsWith(t"."))).filter(compilable)

  def classpath(build: Build)(using Stdout): Set[DiskPath] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap:
      step => step.jars.map(Irk.fetchFile(_).await()).map(_.path) + step.classesDir.path
  
  def allResources(build: Build)(using Stdout): Set[Directory] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap(_.resources)

  def compileClasspath(build: Build)(using Stdout): Set[DiskPath] throws IoError | BrokenLinkError =
    classpath(build) - classesDir.path

  def classesDir: Directory = synchronized:
    try (Irk.cacheDir / t"cls" / projectId / moduleId).directory(Ensure)
    catch
      case err: IoError =>
        throw AppError(t"Could not write to the user's home directory", err)

  def pomDependency(build: Build): Dependency = Dependency(group(build), dashed, version)

  def pomDependencies(build: Build): List[Dependency] =
    try links.map(build.resolve(_)).map(_.pomDependency(build)).to(List) ++
        dependencies
    catch case err: BrokenLinkError => throw AppError(t"Couldn't resolve dependencies", err)

  def compile(hashes: Map[Step, Text], oldHashes: Map[Step, Text], build: Build,
                  scriptFile: File)
             (using Stdout)
             : List[Message] =
    try
      val t0 = System.currentTimeMillis
      classesDir.children.foreach(_.delete())
      val cp = compileClasspath(build)
      val messages = Compiler.compile(id, srcFiles.to(List), cp, classesDir, scriptFile)
      val time = System.currentTimeMillis - t0
      
      if messages.isEmpty then
        Out.println(ansi"Compilation of ${Green}[${name}] succeeded in ${colors.Gold}[${time}ms]")
        val digestFiles = classesDir.path.descendantFiles().to(List).sortBy(_.name).to(LazyList)
        val digest = digestFiles.map(_.read[Bytes](1.mb).digest[Crc32]).to(List).digest[Crc32]
        build.updateCache(this, digest.encode[Base64])
      else Out.println(ansi"Compilation of ${Green}[${name}] failed in ${colors.Gold}[${time}ms]")
      
      messages
    
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

case class BuildConfig(imports: Option[List[Text]], publishing: Option[Publishing],
                           modules: List[Module], repos: Option[List[Repo]]):
  
  def gen(build: Build, seen: Set[Text], files: DiskPath*)(using Stdout)
         : Build throws IoError | AppError | BuildfileError =
    
    repos.getOrElse(Nil).foreach:
      case Repo(base, uri) =>
        val root = build.pwd.path + Relative.parse(base)
        if !root.exists() then
          Out.println(ansi"Cloning repository $uri to $base")
          Irk.cloneRepo(root, uri)

    files.to(List) match
      case Nil =>
        build
      
      case path :: tail =>
        Out.println(ansi"Reading build file ${colors.IndianRed}(${path.relativeTo(build.pwd.path).get.show})")
        val steps: Map[Text, Step] = modules.map:
          module =>
            def relativize(text: Text): DiskPath = path.file(Expect).parent.path + Relative.parse(text)
            val links = module.links.getOrElse(Set())
            val resources = module.resources.getOrElse(Set()).map(relativize).map(_.directory(Expect))
            val sources = module.sources.map(relativize).map(_.directory(Expect))
            val dependencies = module.dependencies.getOrElse(Set())
            val docs = module.docs.getOrElse(Nil).map(relativize)
            val version = module.version.getOrElse(t"1.0.0")
            val artifactPath = module.artifact.map(path.parent + Relative.parse(_))
            
            Step(path.file(), publishing, module.name, module.id, links, resources, sources,
                module.jars.getOrElse(Set()), dependencies, version, docs, artifactPath,
                module.main)
        .mtwin.map(_.id -> _).to(Map)
        
        val importPaths = imports.getOrElse(Nil).map:
          p => (path.parent + Relative.parse(p))
        
        val reposMap = repos.getOrElse(Nil).map:
          repo => (build.pwd.path + Relative.parse(repo.base)) -> repo.url
        .to(Map)

        Irk.readBuilds(build ++ Build(build.pwd, reposMap, build.publishing, steps), seen,
            (importPaths ++ tail)*)

case class Publishing(username: Text, group: Text, url: Text, organization: Organization,
                          developers: List[Developer])

case class Repo(base: Text, url: Text):
  def basePath(dir: DiskPath): DiskPath = dir + Relative.parse(base)

case class Module(name: Text, id: Text, links: Option[Set[Text]], resources: Option[Set[Text]],
                      sources: Set[Text], jars: Option[Set[Text]], docs: Option[List[Text]],
                      dependencies: Option[Set[Dependency]], version: Option[Text],
                      artifact: Option[Text], main: Option[Text])

case class AppError(message: Text, cause: Maybe[Error] = Unset) extends Error(cause)

case class BuildfileError() extends Error:
  def message: Text = t"There was an error in the buildfile"

case class BrokenLinkError(link: Text) extends Error:
  def message: Text = t"The reference to $link cannot be resolved"

object Irk extends Daemon():

  def version: Text =
    Option(getClass.nn.getPackage.nn.getImplementationVersion).fold(t"0")(_.nn.show)
  
  def javaVersion: Text = try Sys.java.version() catch case err: KeyNotFoundError => t"unknown"
  
  def scalaVersion: Text =
    val props = java.util.Properties()
    props.load(getClass.nn.getClassLoader.nn.getResourceAsStream("compiler.properties").nn)
    props.get("version.number").toString.show
 
  def homeDir: Directory = Home()
      
  def cacheDir: Directory =
    try (Home.Cache[jovian.Directory]() / t"irk").directory(Ensure)
    catch case err: IoError =>
      throw AppError(t"The user's cache directory could not be created", err)

  def libDir: Directory =
    try (Home.Cache[jovian.Directory]() / t"lib").directory(Ensure)
    catch case err: IoError =>
      throw AppError(t"The user's cache directory could not be created", err)

  def hashesDir: Directory =
    try (cacheDir / t"hashes").directory(Ensure)
    catch case err: IoError =>
      throw AppError(t"The user's cache directory could not be created", err)

  private val prefixes = Set(t"/scala", t"/dotty", t"/compiler.properties", t"/org/scalajs", t"/com",
      t"/incrementalcompiler.version.properties", t"/library.properties", t"/NOTICE")

  private lazy val scriptSize: Int =
    import unsafeExceptions.canThrowAny
    (Classpath() / t"exoskeleton" / t"invoke").resource.read[Bytes](100.kb).size

  def irkJar(scriptFile: File)(using Stdout): File throws StreamCutError = synchronized:
    import java.nio.file.*
    val jarPath = libDir.path / t"base-$version.jar"
    try if jarPath.exists() then jarPath.file(Expect) else
      val buf = Files.readAllBytes(scriptFile.javaPath).nn
      val output = java.io.BufferedOutputStream(java.io.FileOutputStream(jarPath.javaFile).nn)
      output.write(buf, scriptSize, buf.length - scriptSize)
      output.close()
      val fs = FileSystems.newFileSystem(java.net.URI(t"jar:file:${jarPath.show}".s), Map("zipinfo-time" -> "false").asJava).nn
      
      Files.walk(fs.getPath("/")).nn.iterator.nn.asScala.to(List).sortBy(-_.toString.length).foreach:
        file =>
          def keep(name: Text): Boolean =
            name == t"" || name == t"/org" || prefixes.exists(name.startsWith(_))
          
          try
            if !keep(file.nn.toString.show) then Files.delete(file.nn)
            else Files.setLastModifiedTime(file.nn, Zip.epoch)
          catch case err: Exception => Out.println(t"Got a NPE on ${file.nn.toString.show}")
      
      fs.close()
      
      jarPath.file(Expect)
    catch case err: IoError =>
      throw AppError(t"The Irk binary could not be copied to the user's cache directory")

  def fetchFile(ref: Text)(using Stdout): Future[File] =
    val libDir = cacheDir / t"lib"
    if ref.startsWith(t"https:") then
      val dest = libDir / t"${ref.digest[Crc32].encode[Hex].lower}.jar"
      if dest.exists() then Future:
        try dest.file(Expect) catch case err: IoError =>
          // FIXME: This exception is thrown inside a Future
          throw AppError(t"Could not access the dependency JAR, $ref", err)
      else Future:
        Out.println(ansi"Downloading JAR dependency ${ref}")
        try
          libDir.directory()
          val file = dest.file(Create)
          Uri(ref).writeTo(file)
          file
        catch
          case err: StreamCutError =>
            throw AppError(t"Could not download the file $ref", err)
          
          case err: IoError =>
            throw AppError(t"The downloaded file could not be written to ${dest.fullname}", err)
        
    else
      Future:
        try Unix.parse(ref).get.file(Expect) catch case err: IoError =>
          throw AppError(t"Could not access the dependency JAR, $ref", err)

  def main(using cli: CommandLine): ExitStatus = try
    cli.args match
      case t"about" :: _        => Irk.about()
      case t"help" :: _         => Irk.help()
      case t"init" :: name :: _ => Irk.init(cli.pwd, name)
      case t"version" :: _      => Irk.showVersion()
      case t"build" :: params   => Irk.build(false, params.contains(t"-w") || params.contains(t"--watch"), cli.pwd, cli.env, cli.script, cli.killStream)
      //case t"publish" :: params => Irk.build(true, false, None, cli.pwd, cli.env, cli.script)
      case t"stop" :: params    => Irk.stop(cli)
      case params               => Irk.build(false, params.contains(t"-w") || params.contains(t"--watch"), cli.pwd, cli.env, cli.script, cli.killStream)
  
  catch
    case err: Throwable =>
      Out.println(StackTrace(err).ansi)
      ExitStatus.Fail(1)
  
  private lazy val fileHashes: FileCache[Bytes] = new FileCache()

  private def initBuild(pwd: Directory)(using Stdout): Build throws IoError | BuildfileError =
    val path = pwd / t"build.irk"
    
    try readBuilds(Build(pwd, Map(), None), Set(), path) catch case err: IoError =>
      Out.println(ansi"Configuration file ${colors.IndianRed}(${path.show})")
      sys.exit(1)
  
  def hashFile(file: File): Bytes throws IoError | AppError =
    try fileHashes(file.path.show, file.modified):
      file.read[Bytes](1.mb).digest[Crc32].bytes
    catch
      case err: StreamCutError  => throw AppError(t"The stream was cut while hashing a file", err)
      case err: ExcessDataError => throw AppError(t"The file was too big to hash", err)
      case err: Error           => throw AppError(t"An unexpected error occurred", err)
  
  def cloneRepo(path: DiskPath, url: Text): Unit =
    try sh"git clone -q $url ${path.fullname}".exec[Unit]()
    catch case err: ExecError => throw AppError(t"Could not run `git clone` for repository $url")

  def readBuilds(build: Build, seen: Set[Text], files: DiskPath*)(using Stdout)
                : Build throws BuildfileError =
    try
      files.to(List) match
        case Nil =>
          build
        
        case path :: tail =>
          def digest: Text = hashFile(path.file()).encode[Base64]
          if path.exists() && seen.contains(digest) then readBuilds(build, seen, tail*)
          else if path.exists() then
            val buildConfig = Json.parse(path.file().read[Text](1.mb)).as[BuildConfig]
            buildConfig.gen(build, seen + digest, files*)
          else throw AppError(txt"""Build contains an import reference to a nonexistant build""")
            
    catch
      case err: IoError => err match
        case IoError(op, reason, path) =>
          throw AppError(t"There was an I/O error", err)
      
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

  def readImports(seen: Map[Text, File], files: File*)(using Stdout): Set[File] =
    case class Imports(repos: Option[List[Repo]], imports: Option[List[Text]]):
      def repoList: List[Repo] = repos.getOrElse(Nil)
      def gen(seen: Map[Text, File], files: File*): Set[File] =
        files.to(List) match
          case file :: tail =>
            val importFiles: List[File] = imports.getOrElse(Nil).flatMap:
              path =>
                val ref = file.parent.path + Relative.parse(path)
                try
                  if ref.exists() then
                    List((file.parent.path + Relative.parse(path)).file(Expect))
                  else
                    Out.println(t"Build file $ref does not exist; attempting to clone")
                    if ref.parent.exists()
                    then throw AppError(t"The build ${ref.name} does not exist in ${ref.parent}")
                    else
                      repoList.find(_.basePath(file.parent.path) == ref.parent) match
                        case None =>
                          throw AppError(txt"""Could not find a remote repository containing the import $path""")
                        
                        case Some(repo) =>
                          Irk.cloneRepo(ref.parent, repo.url)
                          List(ref.file(Expect))
                          
                catch case err: IoError =>
                  throw AppError(t"Imported build $ref could not be read", err)
            
            readImports(seen, (importFiles ++ tail)*)
          
          case Nil => seen.values.to(Set)
    
    files.to(List) match
      case Nil =>
        seen.values.to(Set)
      
      case file :: tail =>
        try
          def interpret(digest: Option[Text]) = Json.parse(file.read[Text](1.mb)).as[Imports].gen(digest.fold(seen)(seen.updated(_, file)), files*)
          
          if file.exists() then
            val digest: Text = hashFile(file).encode[Base64]
            if !seen.contains(digest) then interpret(Some(digest)) else readImports(seen, tail*)
          else interpret(None)

        catch case err: Error => readImports(seen, tail*)

  def stop(cli: CommandLine)(using Stdout): ExitStatus =
    Out.println(t"Shutting down Irk")
    cli.shutdown()
    ExitStatus.Ok

  def showVersion()(using Stdout): ExitStatus =
    Out.println(Irk.version)
    ExitStatus.Ok
  
  def about()(using Stdout): ExitStatus =
    Out.println(t"Irk ${Irk.version}")
    Out.println(t"Scala ${Irk.scalaVersion}")
    Out.println(t"Java ${Irk.javaVersion}")
    ExitStatus.Ok

  def help()(using Stdout): ExitStatus =
    Out.println(t"Usage: irk [subcommand] [options]")
    Out.println(t"")
    Out.println(t"Subcommands:")
    Out.println(t"  build    -- runs the build in the current directory (default)")
    Out.println(t"  about    -- show version information for Irk")
    Out.println(t"  help     -- show this message")
    Out.println(t"  stop     -- stop the Irk daemon process")
    Out.println(t"")
    Out.println(t"Options:")
    Out.println(t"  -w, --watch    Wait for changes and re-run build.")
    ExitStatus.Ok

  def init(pwd: Directory, name: Text)(using Stdout): ExitStatus =
    val buildPath = pwd / t"build.irk"
    if buildPath.exists() then
      Out.println(t"Build file build.irk already exists")
      ExitStatus.Fail(1)
    else
      import unsafeExceptions.canThrowAny
      val buildFile = buildPath.file(Create)
      val src = (pwd / t"src").directory(Ensure)
      val sourceDir = (src / t"core").directory(Ensure)
      
      val module = Module(name, t"${pwd.path.name}/core", None, None,
          Set(sourceDir.path.relativeTo(pwd.path).get.show), None, None, None, None, None, None)

      val config = BuildConfig(None, None, List(module), None)
      try
        config.json.show.writeTo(buildFile)
        ExitStatus.Ok
      catch
        case err: IoError =>
          Out.println(t"Could not write to build.irk")
          ExitStatus.Fail(1)
        case err: StreamCutError =>
          Out.println(t"Could not write to build.irk")
          ExitStatus.Fail(1)

  def reportMessages(messages: List[Message])(using Stdout): Unit =
    messages.groupBy(_.path).foreach:
      case (path, messages) =>
        val syntax = ScalaSyntax.highlight(String(messages.head.content.unsafeMutable).show)
        messages.sortBy(-_.line).groupBy(_.line).foreach:
          case (line, messages) => messages.last match
            case Message(module, path, line, from, to, message, _) =>
              val bg = Bg(Srgb(0.16, 0.06, 0.03))
              Out.println(ansi"${colors.Black}(${Bg(colors.Purple)}( $module ))${colors.Purple}(${Bg(colors.Crimson)}( ${colors.Black}($path:${line + 1}:$from) ))$bg${colors.Crimson}()$bg${escapes.EraseLine}")
              
              def format(line: Int) =
                if line >= syntax.length then ansi""
                else syntax(line).map:
                  case Token.Code(code, flair) => flair match
                    case Flair.Type              => ansi"${colors.YellowGreen}(${code})"
                    case Flair.Term              => ansi"${colors.CadetBlue}(${code})"
                    case Flair.Symbol            => ansi"${colors.Turquoise}(${code})"
                    case Flair.Keyword           => ansi"${colors.DarkOrange}(${code})"
                    case Flair.Modifier          => ansi"${colors.Chocolate}(${code})"
                    case Flair.Ident             => ansi"${colors.BurlyWood}(${code})"
                    case Flair.Error             => ansi"${colors.OrangeRed}($Underline(${code}))"
                    case Flair.Number            => ansi"${colors.Gold}(${code})"
                    case Flair.String            => ansi"${colors.Plum}(${code})"
                    case other                   => ansi"${code}"
                  case Token.Unparsed(txt)     => ansi"$txt"
                  case Token.Markup(_)         => ansi""
                  case Token.Newline           => throw Impossible("Should not have a newline")
                .join
              
              val margin = (line + 2).show.length
              
              if line > 1 then
                Out.print(ansi"${colors.Orange}(${line.show.pad(margin, Rtl)})${colors.Gray}(║)$bg ")
                Out.println(ansi"${format(line - 1)}${escapes.EraseLine}")
              
              val code = format(line)
              Out.print(ansi"${colors.Orange}($Bold(${(line + 1).show.pad(margin, Rtl)}))${colors.Gray}(║)$bg ")
              Out.print(ansi"${code.take(from)}")
              Out.print(ansi"${Underline}(${colors.OrangeRed}(${code.plain.slice(from, to)}))")
              Out.println(ansi"${code.drop(to)}${escapes.EraseLine}")
              
              if line + 1 < syntax.length
              then
                Out.print(ansi"${colors.Orange}(${(line + 2).show.pad(margin, Rtl)})${colors.Gray}(║)$bg ")
                Out.println(ansi"${format(line + 1)}${escapes.EraseLine}${escapes.Reset}")
              
              messages.reverse.foreach:
                case m@Message(module, path, line, from, to, message, _) =>
                  if m != messages.last then
                    Out.print(ansi"${colors.Orange}($Bold(${(line + 1).show.pad(margin, Rtl)}))${colors.Gray}(║)$bg ")
                    Out.print(ansi"${code.take(from)}")
                    Out.print(ansi"${Underline}(${colors.OrangeRed}(${code.plain.slice(from, to)}))")
                    Out.println(ansi"${code.drop(to)}${escapes.EraseLine}${escapes.Reset}")
                  Out.println(ansi"${colors.Gray}(${t" "*margin}╟${t"─"*from}┴${t"─"*(to - from)}┘)${escapes.EraseLine}${escapes.Reset}")
                  message.cut(t"\n").foreach:
                    line =>
                      Out.println(ansi"${t" "*margin}${colors.Gray}(║) ${Bold}($line)${escapes.EraseLine}${escapes.Reset}")
                  if m != messages.head then Out.println(ansi"${t" "*margin}${colors.Gray}(║)")
              Out.println(ansi"${t" "*margin}${colors.Gray}(╨)${escapes.EraseLine}${escapes.Reset}")
              Out.println(ansi"${escapes.Reset}")

  sealed trait Trigger
  case object Abort extends Trigger

  case class Changes(build: Boolean = false, sources: Boolean = false, resources: Boolean = false) extends Trigger:
    def changed: Boolean = build || sources || resources
    
    def apply(path: DiskPath): Changes = 
      if path.name.endsWith(t".irk") then copy(build = true) else copy(sources = true)
    
  def build(publishSonatype: Boolean, watch: Boolean = false, pwd: Directory, env: Map[Text, Text],
                scriptFile: File, killStream: LazyList[Unit])
           (using Stdout)
           : ExitStatus throws AppError =
    val rootBuild = pwd / t"build.irk"
    
    val watcher: Unix.Watcher = try  
      val watcher = Unix.watch(Nil)
      
      if watch then
        val dirs = readImports(Map(), rootBuild.file(Expect)).map(_.parent).to(Set)
        dirs.sift[Unix.Directory].foreach(watcher.add(_))
      
      watcher
    
    catch
      case err: InotifyError =>
        throw AppError(t"Could not watch directories", err)
      
      case err: IoError      =>
        throw AppError(t"Could not watch directories", err)

    val suffixes = Set(t".scala", t".java", t".irk")
    
    def interest(path: DiskPath): Boolean =
      suffixes.exists(path.name.endsWith(_)) && !path.name.startsWith(t".")

    def ephemeral(events: List[Unix.FileEvent]): Boolean = events match
      case Unix.FileEvent.NewFile(_) +: _ :+ Unix.FileEvent.Delete(_) => true
      case _                                                          => false

    def triggers(events: List[Unix.FileEvent]): Changes =
      events.groupBy(_.path).collect:
        case (path, es) if interest(path) && !ephemeral(es) => es.last
      .foldLeft(Changes()):
        (changes, event) =>
          Option(event).collect:
            case Unix.FileEvent.Delete(_)  => t"deleted"
            case Unix.FileEvent.Modify(_)  => t"modified"
            case Unix.FileEvent.NewFile(_) => t"created"
          .foreach:
            changed => Out.println(ansi"The file ${colors.IndianRed}(${event.path}) was $changed")
          
          Changes(event.path.fullname.endsWith(t".irk"), event.path.fullname.endsWith(t".scala"))


    def updateWatches(watcher: Unix.Watcher, build: Build): Build = try
      val buildDirs = readImports(Map(), rootBuild.file(Expect)).map(_.parent).to(Set)
      val dirs = (buildDirs ++ build.sourceDirs ++ build.resourceDirs).sift[Unix.Directory]
      val additions = dirs -- watcher.directories
      
      if additions.size > 5
      then Out.println(ansi"Started watching ${colors.Gold}(${additions.size}) directories")

      additions.foreach:
        dir =>
          watcher.add(dir)
          
          if additions.size <= 5
          then Out.println(ansi"Started watching ${colors.IndianRed}(${dir.path})")

      (watcher.directories -- dirs).foreach:
        dir =>
          watcher.remove(dir)
          Out.println(ansi"Stopped watching ${colors.IndianRed}(${dir.path})")
      
      build
    catch
      case err: InotifyError => throw AppError(t"Could not update watch directories", err)
      case err: IoError      => throw AppError(t"Could not update watch directories", err)

    @tailrec
    def recur(stream: LazyList[Trigger], oldBuild: Option[Build], success: Boolean): ExitStatus =
      if stream.isEmpty then
        if success then ExitStatus.Ok else ExitStatus.Fail(1)
      else stream.head match
        case Abort =>
          Out.println(t"Aborting")
          watcher.removeAll()
          recur(stream.tail, oldBuild, success)
        case changes@Changes(_, _, _) =>
          val newBuild: Option[Build] =
            if changes.build || oldBuild.isEmpty then
              try Some(if watch then updateWatches(watcher, initBuild(pwd)) else initBuild(pwd))
              catch
                case err: BuildfileError =>
                  Out.println(ansi"The build contained an error")
                  None
                case err: IoError =>
                  Out.println(ansi"The build file could not be read")
                  None
            else oldBuild
  
  
          val succeeded: Boolean = newBuild.fold(false):
            build =>
              import unsafeExceptions.canThrowAny
              val oldHashes = build.cache
              Out.println("Starting build")
  
              val futures = build.graph.traversal[Future[Set[Message]]]:
                (set, step) => Future.sequence(set).flatMap:
                  results =>
                    Future.sequence(step.jars.map(Irk.fetchFile(_))).flatMap:
                      downloads => Future:
                        blocking:
                          val messages = results.flatten
                          if messages.isEmpty then
                            try
                              if oldHashes.get(step) != build.hashes.get(step) || step.main.isDefined
                              then
                                Out.println(ansi"Compiling ${Green}[${step.name}]...")
                                messages ++ step.compile(build.hashes, oldHashes, build, scriptFile)
                              else messages
                            catch
                              case err: ExcessDataError =>
                                messages + Message(step.id, t"<unknown>", 0, 0, 0,
                                    t"too much data was received", IArray())
              
                              case err: StreamCutError =>
                                messages + Message(step.id, t"<unknown>", 0, 0, 0,
                                    t"the stream was cut prematurely", IArray())
                          else
                            Out.println(t"Skipping compilation of ${step.name}")
                            messages
              .values
  
              val messages: List[Message] = Future.sequence(futures).await().to(Set).flatten.to(List)
              val success = messages.isEmpty
              reportMessages(messages)
              if success then Out.println(t"Compilation completed successfully")
              else Out.println(t"Compilation failed\n")
              
              // FIXME: Files should be sorted by last-modified
              if success then
                build.linearization.foreach:
                  step => step.artifact.foreach:
                    artifact =>
                      val t0 = System.currentTimeMillis
                      Out.println(ansi"Building ${colors.IndianRed}(${artifact.show}) artifact...")
                      val inputJars = step.classpath(build) + irkJar(scriptFile).path
                      val zipStreams = inputJars.to(LazyList).flatMap:
                        path =>
                          if path.isFile then
                            Zip.read(path.file(Expect)).filter(_.path.parts.last != t"MANIFEST.MF")
                          else if path.isDirectory then
                            path.descendantFiles().map:
                              file =>
                                Zip.Entry(file.path.relativeTo(path).get, java.io.BufferedInputStream(java.io.FileInputStream(file.javaFile)))
                          else LazyList()
                      
                      val resourceStreams = step.allResources(build).to(List).sortBy(_.path.show).flatMap:
                        dir => dir.path.descendantFiles().map:
                          file =>
                            Zip.Entry(file.path.relativeTo(dir.path).get, java.io.BufferedInputStream(java.io.FileInputStream(file.javaFile)))
                      .to(LazyList)
                      
                      val basicMf = ListMap(
                        t"Manifest-Version"       -> t"1.0",
                        t"Created-By"             -> t"Irk ${Irk.version}",
                        t"Implementation-Title"   -> step.name,
                        t"Implementation-Version" -> step.version
                      )
                      
                      val manifest = step.main.fold(basicMf)(basicMf.updated(t"Main-Class", _)).flatMap:
                        (k, v) =>
                          val (first, rest) = t"$k: $v".snip(72)
                          first :: rest.s.grouped(71).map(t" "+_.show).to(List)
                      .join(t"", t"\n", t"\n")
                      
                      val mfEntry = Zip.Entry(Relative.parse(t"META-INF/MANIFEST.MF"), java.io.BufferedInputStream(java.io.ByteArrayInputStream(manifest.bytes.unsafeMutable)))
                      
                      val header = if artifact.name.endsWith(t".jar") then Bytes.empty else
                          (Classpath() / t"exoskeleton" / t"invoke").resource.read[Bytes](100.kb)
                      
                      Zip.write(irkJar(scriptFile), artifact, mfEntry #:: resourceStreams #::: zipStreams, header)
                      artifact.file(Expect).setPermissions(executable = true)
                      val time = System.currentTimeMillis - t0
                      Out.println(ansi"Built ${colors.IndianRed}(${artifact.show}) in ${colors.Gold}[${time}ms]")
                      
                if publishSonatype then Sonatype.publish(build, env.get(t"SONATYPE_PASSWORD"))
              success
          
          if watch then Out.println(t"Watching ${watcher.directories.size} directories for changes")
          recur(stream.tail, newBuild, succeeded)
      
    def buildStream: LazyList[Trigger] =
      if !watch then LazyList()
      else
        LazyList.multiplex(
          watcher.stream.cluster(100).map(triggers).filter(_.changed),
          killStream.map:
            unit =>
              Out.println(t"Triggered")
              Abort
        )
    recur(Changes(true, true, true) #:: buildStream, None, false)


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

object Dependency:
  given Json.Reader[Dependency] = summon[Json.Reader[Text]].map:
    case s"$group:$artifact:$version" => Dependency(group.show, artifact.show, version.show)

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
      groupId = step.group(build),
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
given realm: Realm = Realm(t"irk")


object Compiler:
  import dotty.tools.dotc.*, reporting.*

  class CustomReporter() extends Reporter:
    var errors: scm.ListBuffer[Diagnostic] = scm.ListBuffer()
    def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit =
      errors += diagnostic

  val Scala3 = new dotty.tools.dotc.Compiler()

  def compile(id: Text, files: List[File], inputs: Set[DiskPath], out: Directory, script: File)
             (using Stdout)
             : List[irk.Message] =
    import unsafeExceptions.canThrowAny
    import dotty.tools.*, io.{File as _, *}, repl.*, dotc.core.*
    
    val reporter = CustomReporter()
    try
      val separator: Text = try Sys.path.separator() catch case err: KeyNotFoundError => t":"
      //val classpath = Unix.parse(t"/home/propensive/one/scala/dist/target/pack/lib").get.directory(Expect).children.map(_.fullname)
      val classpath: List[Text] = inputs.map(_.fullname).to(List) :+ Irk.irkJar(script).fullname
      val classpathText = classpath.join(separator)
      
      val callbackApi = new interfaces.CompilerCallback:
        override def onClassGenerated(source: interfaces.SourceFile,
                                          generatedClass: interfaces.AbstractFile,
                                          className: String): Unit =
          ()//Out.println(t"Generated class ${className}")
  
        override def onSourceCompiled(source: interfaces.SourceFile): Unit =
          ()//Out.println(t"Compiled source ${source.toString}")
      
      object driver extends dotc.Driver:
        val currentCtx =
          val ctx = initCtx.fresh
          setup(Array[String]("-d", out.fullname.s, "-deprecation", "-feature", "-Wunused:all",
              "-new-syntax", "-Yrequire-targetName", "-Ysafe-init", "-Yexplicit-nulls",
              "-Ycheck-all-patmat", ""), ctx).map(_(1)).getOrElse(ctx)
        
        def run(files: List[File], classpath: Text): List[Diagnostic] =
          val ctx = currentCtx.fresh
          val newCtx = ctx
            .setReporter(reporter)
            .setCompilerCallback(callbackApi)
            .setSetting(ctx.settings.language, List("experimental.fewerBraces", "experimental.saferExceptions", "experimental.erasedDefinitions"))
            .setSetting(ctx.settings.classpath, classpath.s)
          
          val sources = files.to(List).map:
            file => PlainFile(Path(file.fullname.s))
          
          val run = Scala3.newRun(using newCtx)
          run.compile(sources)
          finish(Scala3, run)(using newCtx)
          reporter.errors.to(List)

      driver.run(files, classpathText).flatMap:
        diagnostic =>
          if diagnostic.level == 2 && diagnostic.position.isPresent then
            val pos = diagnostic.position.get.nn
            val line = pos.line
            val file = pos.source.nn.name.nn
            val content = pos.source.nn.content.nn
            List(irk.Message(id, file.show, line, pos.startColumn, pos.endColumn, diagnostic.message.show, content.unsafeImmutable))
          else Nil

    catch case err: Throwable =>
      Out.println(StackTrace(err).ansi)
      List(irk.Message(id, t"", 0, 0, 0, t"The compiler crashed", IArray()))

object Zip:
  import java.io.*
  import java.nio.*, java.nio.file.*
  import java.util.zip.*

  sealed trait ZipEntry:
    def path: Relative

  case class File2(path: Relative, diskPath: Unix.DiskPath) extends ZipEntry
  case class Entry(path: Relative, in: InputStream) extends ZipEntry

  def read(file: jovian.File): LazyList[Entry] =
    val zipFile = ZipFile(file.javaFile).nn
    zipFile.entries.nn.asScala.to(LazyList).filter(!_.getName.nn.endsWith("/")).map:
      entry => Entry(Relative.parse(entry.getName.nn.show), zipFile.getInputStream(entry).nn)

  // 00:00:00, 1 January 2000
  val epoch = attribute.FileTime.fromMillis(946684800000L)

  def write(base: jovian.File, path: jovian.DiskPath, inputs: LazyList[ZipEntry], prefix: Maybe[Bytes] = Unset)
           (using Stdout)
           : Unit throws StreamCutError | IoError =
    val tmpPath = path.rename { old => t".$old.tmp" }
    base.copyTo(tmpPath)
    val uri: java.net.URI = java.net.URI.create(t"jar:file:${tmpPath.show}".s).nn
    val fs = FileSystems.newFileSystem(uri, Map("zipinfo-time" -> "false").asJava).nn

    val dirs = inputs.map(_.path).map(_.parent).to(Set).flatMap:
      dir => (0 to dir.parts.length).map { n => Relative(0, dir.parts.take(n)) }.to(Set)
    .to(List).map(_.show+t"/").sorted

    for dir <- dirs do
      val dirPath = fs.getPath(dir.s).nn
      if Files.notExists(dirPath) then
        Files.createDirectory(dirPath)
        Files.setAttribute(dirPath, "creationTime", epoch)
        Files.setAttribute(dirPath, "lastAccessTime", epoch)
        Files.setAttribute(dirPath, "lastModifiedTime", epoch)

    inputs.foreach:
      case Entry(path, in) =>
        val entryPath = fs.getPath(path.show.s).nn
        Files.copy(in, entryPath, StandardCopyOption.REPLACE_EXISTING)
        Files.setAttribute(entryPath, "creationTime", epoch)
        Files.setAttribute(entryPath, "lastAccessTime", epoch)
        Files.setAttribute(entryPath, "lastModifiedTime", epoch)
      case File2(path, file) =>
        val filePath = fs.getPath(path.show.s).nn
        Files.copy(file.javaPath, filePath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        Files.setAttribute(filePath, "creationTime", epoch)
        Files.setAttribute(filePath, "lastAccessTime", epoch)
        Files.setAttribute(filePath, "lastModifiedTime", epoch)
    
    fs.close()


    val fileOut = BufferedOutputStream(FileOutputStream(path.javaFile).nn)
    prefix.option.foreach:
      prefix =>
        fileOut.write(prefix.unsafeMutable)
        fileOut.flush()
    
    fileOut.write(java.nio.file.Files.readAllBytes(tmpPath.javaPath))
    fileOut.close()
    java.nio.file.Files.delete(tmpPath.javaPath)

object Cache:
  import scala.collection.mutable.HashMap
  private val latest: HashMap[Text, Text] = HashMap()
  private val locks: HashMap[DiskPath, Lock] = HashMap()
  
  private class Lock()

  private def synchronously(path: DiskPath)(block: Directory => Unit): Directory throws IoError =
    Cache.synchronized:
      locks.get(path).getOrElse:
        val lock = Lock()
        locks(path) = lock
        lock
    .synchronized:
      if path.isDirectory then path.directory(Expect) else
        val dir = path.directory(Create)
        block(dir)
        dir

  def apply[T: Hashable](key: Text, input: T)(make: (T, Directory) => Unit)
           : Directory throws IoError =
    val hash = input.digest[Crc32].encode[Hex].lower
    
    synchronously(Irk.cacheDir / hash):
      workDir =>
        make(input, workDir)
        
        Cache.synchronized:
          latest.get(key).foreach:
            oldHash => if oldHash != hash then (Irk.cacheDir / hash).directory().delete()
          
          latest(key) = hash
          workDir