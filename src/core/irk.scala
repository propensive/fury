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
import iridescence.*, solarized.*
import eucalyptus.*
import slalom.*
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

object palette:
  val File = colors.Coral
  val Number = colors.SandyBrown

case class Build(pwd: Directory, repos: Map[DiskPath, Text], publishing: Option[Publishing],
    index: Map[Text, Step] = Map(), targets: Map[Text, Target] = Map()):
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
    //Out.println(ansi"Calculated ${palette.Number}(${index.size}) hashes in ${palette.Number}[${time}ms]")
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
  private def output(extension: Text): DiskPath = pwd / t"bin" / t"$dashed-$version$extension"
  
  def compilable(f: File): Boolean = f.name.endsWith(t".scala") || f.name.endsWith(t".java")

  def srcFiles: Set[File] throws IoError =
    sources.flatMap(_.path.descendantFiles(!_.name.startsWith(t"."))).filter(compilable)

  def classpath(build: Build)(using Stdout): Set[DiskPath] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap:
      step => step.jars.map(Irk.fetchFile(_, None).await()).map(_.path) + step.classesDir.path
  
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

  def compile(hashes: Map[Step, Text], oldHashes: Map[Step, Text], build: Build, scriptFile: File)
             (using Stdout)
             : List[irk.Message] =
    try
      val t0 = System.currentTimeMillis
      classesDir.children.foreach(_.delete())
      val cp = compileClasspath(build)
      val messages = Compiler.compile(id, srcFiles.to(List), cp, classesDir, scriptFile)
      val time = System.currentTimeMillis - t0
      
      if messages.isEmpty then
        //Out.println(ansi"Compilation of ${Green}[${name}] succeeded in ${palette.Number}[${time}ms]")
        val digestFiles = classesDir.path.descendantFiles().to(List).sortBy(_.name).to(LazyList)
        val digest = digestFiles.map(_.read[Bytes](1.mb).digest[Crc32]).to(List).digest[Crc32]
        build.updateCache(this, digest.encode[Base64])
      //else Out.println(ansi"Compilation of ${Green}[${name}] failed in ${palette.Number}[${time}ms]")
      
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
                           modules: List[Module], repos: Option[List[Repo]],
                           targets: Option[List[Target]]):
  
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

case class Hash(id: Text, digest: Text, bin: Text)
case class Cache(hashes: Set[Hash])
case class Versioning(versions: List[Version])

case class Version(digest: Text, major: Int, minor: Int):
  def version: Text = t"$major.$minor"

class FileCache[T]:
  private val files: scm.HashMap[Text, (Long, T)] = scm.HashMap()
  
  def apply(filename: Text, modified: Long)(calc: => T): T throws IoError =
    if !files.get(filename).fold(false)(_(0) == modified) then files(filename) = modified -> calc
    files(filename)(1)
given realm: Realm = Realm(t"irk")

object Compiler:
  import dotty.tools.dotc.*, reporting.*

  class CustomReporter() extends Reporter, UniqueMessagePositions:
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

  case class ZipPath(path: Relative, diskPath: Unix.DiskPath) extends ZipEntry
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
      case ZipPath(path, file) =>
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


object Progress:
  enum Update:
    case Add(name: AnsiText)
    case Remove(success: Boolean, name: AnsiText)
    case Print

case class Progress(active: TreeMap[AnsiText, Long], completed: List[(AnsiText, Long, Boolean)],
                        started: Boolean = false, done: Int = 0):
  private def add(name: AnsiText): Progress = copy(active = active.updated(name, System.currentTimeMillis))
  
  private def remove(success: Boolean, name: AnsiText): Progress = copy(
    active = active - name,
    completed = (name, active(name), success) :: completed
  )

  def titleText(title: Text): Text = t"\e]0;$title\u0007\r"

  def apply(update: Progress.Update)(using Stdout): Progress = update match
    case Progress.Update.Add(name) =>
      add(name)
    
    case Progress.Update.Remove(success, name) =>
      remove(success, name)
    
    case Progress.Update.Print =>
      val starting = !started && completed.nonEmpty
      if starting then Out.println(t"─"*120)
      status.foreach(Out.println(_))
      if active.size > 0 then Out.print(t"\e[${active.size + 1}A")
      
      Out.print(t"\e[?25l")
      if completed.size > 0 then Out.print(titleText(t"Irk, compiling ($done) "))
      
      copy(completed = Nil, started = started || starting, done = done + completed.size)

  val braille = IArray(t"⡀", t"⡄", t"⡆", t"⡇", t"⡏", t"⡟", t"⡿", t"⣿", t"⢿", t"⢻", t"⢹", t"⢸", t"⢰", t"⢠", t"⢀")

  private def status: List[AnsiText] =
    def line(name: AnsiText, start: Long, success: Boolean, active: Boolean): AnsiText =
      val ds = (System.currentTimeMillis - start).show.drop(2, Rtl)
      val fractional = if ds.length == 0 then t"0" else ds.take(1, Rtl)
      val time = ansi"${if ds.length < 2 then t"0" else ds.drop(1, Rtl)}.${fractional}s"
      val padding = ansi" "*(6 - time.length.min(6))
      
      if active then
        val anim = braille(((System.currentTimeMillis/100)%braille.length).toInt)
        ansi"${colors.White}([$Yellow($anim)] ${name.padTo(110, ' ')}$padding${palette.Number}($time))"
      else
        val finish = if success then ansi"[$Green(✓)]" else ansi"[$Red(✗)]"
        ansi"$Bold($finish) ${name.padTo(110, ' ')}$padding${palette.Number}($time)"
    
    completed.map(line(_, _, _, false)) ++ List(ansi"─"*120) ++
      active.to(List).map(line(_, _, true, true))
        