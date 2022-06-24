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
import anticipation.*
import java.nio.BufferOverflowException

import scala.collection.mutable as scm
import scala.concurrent.*
import scala.collection.convert.ImplicitConversions.given
import scala.util.chaining.scalaUtilChainingOps

erased given CanThrow[AppError] = compiletime.erasedValue

import timekeeping.long
import encodings.Utf8

given Env = envs.enclosing
import rendering.ansi

given LogFormat[SystemOut.type, AnsiText] = LogFormat.timed
given Log()

object palette:
  val File = colors.Coral
  val Number = colors.SandyBrown
  val ActiveNumber = colors.Gold
  val Hash = colors.MediumSeaGreen
  val Class = colors.MediumAquamarine

case class Build(pwd: Directory, repos: Map[DiskPath, Text], publishing: Option[Publishing],
    index: Map[Text, Step] = Map(), targets: Map[Text, Target] = Map()):
  val steps: Set[Step] = index.values.to(Set)
  
  lazy val graph: Dag[Step] throws BrokenLinkError =
    val links = index.values.map: step =>
      step -> step.links.map(resolve(_)).to(Set)
    
    Dag(links.to(Seq)*)

  lazy val linearization: List[Step] throws BrokenLinkError = graph.sorted
  def sourceDirs: List[Directory] = steps.flatMap(_.sources).to(List)
  def resourceDirs: List[Directory] = steps.flatMap(_.resources).to(List)

  def hashes: Map[Step, Text] throws BrokenLinkError | IoError | StreamCutError | ExcessDataError =
    def recur(todo: List[Step], hashes: Map[Step, Text])
             : Map[Step, Text] throws BrokenLinkError | StreamCutError | ExcessDataError =
      if todo.isEmpty then hashes
      else try
        val step = todo.head
        val inputsHash: List[Bytes] = step.srcFiles.to(List).map(Irk.hashFile)
        val jarsHash: List[Bytes] = step.jars.to(List).map(_.digest[Crc32].bytes)
        val linksHash: List[Bytes] = step.links.to(List).map(index(_)).map(hashes(_).bytes)
        val newHash = (inputsHash ++ linksHash).digest[Crc32].encode[Base256]
        
        recur(todo.tail, hashes.updated(step, newHash))
      catch case err: Error[?] => throw AppError(t"An unknown error occurred", err)
    val t0 = now()
    val result = recur(linearization, Map())
    val time = now() - t0
    result

  @targetName("addAll")
  infix def ++(build: Build): Build =
    Build(build.pwd, build.repos, publishing.orElse(build.publishing), index ++ build.index)
  
  def resolve(id: Text): Step throws BrokenLinkError = index.get(id).getOrElse:
    throw BrokenLinkError(id)
  
  def bases: Set[Directory] = steps.map(_.pwd).to(Set)

  def cache: Map[Step, Text] =
    try
      def encode(dir: Directory): DiskPath = Irk.hashesDir / dir.path.show.digest[Crc32].encode[Hex]
      
      val caches = bases.map(encode).filter(_.exists()).map: cacheFile =>
        Json.parse(cacheFile.file().read[Text](64.kb)).as[Cache].hashes
      
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
      
      case err: Exception =>
        throw AppError(StackTrace(err).ansi.plain)
        //throw AppError(t"An unexpected error occurred")
  
  def updateCache(step: Step, binDigest: Text): Unit = synchronized:
    try
      val cacheFile = Irk.hashesDir / step.pwd.path.show.digest[Crc32].encode[Hex]
      val cache =
        if cacheFile.exists()
        then Cache:
          Json.parse(cacheFile.file().read[Text](64.kb)).as[Cache].hashes.filter: hash =>
            try resolve(hash.id).pwd == step.pwd catch case err: BrokenLinkError => false
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
      
      case err: Exception =>
        throw AppError(StackTrace(err).ansi.plain)
        //throw AppError(t"An unexpected error occurred")

object Format:
  def unapply(value: Text): Option[Format] = value match
    case t"jar"        => Some(Format.Jar)
    case t"fat-jar"    => Some(Format.FatJar)
    case t"app"        => Some(Format.App)
    case t"daemon-app" => Some(Format.DaemonApp)
    case _             => None

enum Format:
  case Jar, FatJar, App, DaemonApp

object Artifact:
  def build(artifact: Artifact, base: File, name: Text, version: Text, classpath: List[DiskPath],
                resources: List[Directory]): Unit throws IoError =
    import stdouts.drain
    
    val zipStreams = (base.path :: classpath).to(LazyList).flatMap: path =>
      if path.isFile then Zip.read(path.file(Expect)).filter(_.path.parts.last != t"MANIFEST.MF")
      else if path.isDirectory then
        path.descendantFiles().map: file =>
          val in = java.io.BufferedInputStream(java.io.FileInputStream(file.javaFile))
          Zip.Entry(file.path.relativeTo(path).get, in)
      else LazyList()
    
    val resourceStreams = resources.sortBy(_.path.show).flatMap: dir =>
      dir.path.descendantFiles().map: file =>
        val in = java.io.BufferedInputStream(java.io.FileInputStream(file.javaFile))
        Zip.Entry(file.path.relativeTo(dir.path).get, in)
    .to(LazyList)
    
    val basicMf = ListMap(
      t"Manifest-Version"       -> t"1.0",
      t"Created-By"             -> t"Irk ${Irk.version}",
      t"Implementation-Title"   -> name,
      t"Implementation-Version" -> version
    )
    
    val manifest = artifact.main.fold(basicMf)(basicMf.updated(t"Main-Class", _)).flatMap: (k, v) =>
      val (first, rest) = t"$k: $v".snip(72)
      first :: rest.s.grouped(71).map(t" "+_.show).to(List)
    .join(t"", t"\n", t"\n")
    
    val in = java.io.BufferedInputStream(java.io.ByteArrayInputStream(manifest.bytes.mutable(using Unsafe)))
    val mfEntry = Zip.Entry(Relative.parse(t"META-INF/MANIFEST.MF"), in)
    
    val header = if artifact.path.name.endsWith(t".jar") then Bytes.empty else
        unsafely((Classpath() / t"exoskeleton" / t"invoke").resource.read[Bytes](100.kb))
    
    unsafely:
      Zip.write(base, artifact.path, mfEntry #:: resourceStreams #::: zipStreams, header)
    
    artifact.path.file(Expect).setPermissions(executable = true)

case class Step(path: File, publishing: Option[Publishing], name: Text,
                    id: Text, links: Set[Text], resources: Set[Directory],
                    sources: Set[Directory], jars: Set[Text], dependencies: Set[Dependency],
                    version: Text, docs: List[DiskPath], artifact: Option[Artifact],
                    exec: Option[Exec]):
  
  def publish(build: Build): Publishing = publishing.orElse(build.publishing).getOrElse:
    throw AppError(t"There are no publishing details for $id")
 
  def projectId: Text = id.cut(t"/").head
  def moduleId: Text = id.cut(t"/").last
  def dashed: Text = t"$projectId-$moduleId"
  def pwd: Directory = unsafely(path.parent)
  def group(build: Build): Text = publish(build).group
  def docFile: DiskPath = output(t"-javadoc.jar")
  def srcsPkg: DiskPath = output(t"-sources.jar")
  def pomFile: DiskPath = output(t".pom")
  
  private def output(extension: Text): DiskPath = unsafely(pwd / t"bin" / t"$dashed-$version$extension")
  private def compilable(f: File): Boolean = f.name.endsWith(t".scala") || f.name.endsWith(t".java")

  def srcFiles: Set[File] throws IoError =
    sources.flatMap(_.path.descendantFiles(!_.name.startsWith(t"."))).filter(compilable)

  def classpath(build: Build)(using Stdout): Set[DiskPath] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap: step =>
      step.jars.map(Irk.fetchFile(_, None).await()).map(_.path) + step.classesDir.path
  
  def allResources(build: Build)(using Stdout): Set[Directory] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap(_.resources)

  def compileClasspath(build: Build)(using Stdout): Set[DiskPath] throws IoError | BrokenLinkError =
    classpath(build) - classesDir.path

  def classesDir: Directory = synchronized:
    try unsafely(Irk.cacheDir / t"cls" / projectId / moduleId).directory(Ensure)
    catch case err: IoError => throw AppError(t"Could not write to the user's home directory", err)

  def pomDependency(build: Build): Dependency = Dependency(group(build), dashed, version)

  def pomDependencies(build: Build): List[Dependency] =
    try links.map(build.resolve(_)).map(_.pomDependency(build)).to(List) ++ dependencies
    catch case err: BrokenLinkError => throw AppError(t"Couldn't resolve dependencies", err)

  def compile(hashes: Map[Step, Text], oldHashes: Map[Step, Text], build: Build, scriptFile: File,
                  cancel: Promise[Unit])
             (using Stdout)
             : Result =
    try
      val t0 = now()
      
      classesDir.children.foreach:
        case file: Unix.File     => file.delete()
        case dir: Unix.Directory => dir.delete()
      
      val cp = compileClasspath(build)
      val result = Compiler.compile(id, pwd, srcFiles.to(List), cp, classesDir, scriptFile, cancel)
      val time = now() - t0
      
      if result.success then
        val digestFiles = classesDir.path.descendantFiles().to(List).sortBy(_.name).to(LazyList)
        val digest = digestFiles.map(_.read[Bytes](1.mb).digest[Crc32]).to(List).digest[Crc32]
        build.updateCache(this, digest.encode[Base64])
      
      if cancel.isCompleted then Result.Aborted else result
    
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

      case err: Error[?] =>
        Out.println(err.stackTrace.ansi)
        throw AppError(t"An unexpected error occurred", err)

case class BuildConfig(imports: Option[List[Text]], publishing: Option[Publishing],
                           modules: List[Module], repos: Option[List[Repo]],
                           targets: Option[List[Target]]):
  
  def gen(current: DiskPath, build: Build, seen: Set[Text], files: DiskPath*)(using Stdout)
         : Build throws IoError | AppError | BuildfileError =
    
    repos.getOrElse(Nil).foreach:
      case Repo(base, uri) =>
        val root = unsafely(current.parent + Relative.parse(base))
        if !root.exists() then
          Out.println(ansi"Cloning repository $uri to $base".render)
          Irk.cloneRepo(root, uri)

    files.to(List) match
      case Nil =>
        build
      
      case path :: tail =>
        val steps: Map[Text, Step] = modules.map: module =>
          def relativize(text: Text): DiskPath = unsafely(path.file(Expect).parent.path + Relative.parse(text))
          val links = module.links.getOrElse(Set())
          val resources = module.resources.getOrElse(Set()).map(relativize).map(_.directory(Expect))
          val sources = module.sources.map(relativize).map(_.directory(Expect))
          val dependencies = module.dependencies.getOrElse(Set())
          val docs = module.docs.getOrElse(Nil).map(relativize)
          val version = module.version.getOrElse(t"1.0.0")
          val artifact = module.artifact.map: spec =>
            val format = spec.format.flatMap(Format.unapply(_)).getOrElse(Format.FatJar)
            Artifact(unsafely(path.parent + Relative.parse(spec.path)), spec.main, format)
          
          Step(path.file(), publishing, module.name, module.id, links, resources, sources,
              module.jars.getOrElse(Set()), dependencies, version, docs, artifact,
              module.exec)
        .mtwin.map(_.id -> _).to(Map)
        
        val importPaths = imports.getOrElse(Nil).map: p =>
          unsafely(path.parent + Relative.parse(p))
        
        val reposMap = repos.getOrElse(Nil).map: repo =>
          unsafely(build.pwd.path + Relative.parse(repo.base)) -> repo.url
        .to(Map)

        Irk.readBuilds(build ++ Build(build.pwd, reposMap, build.publishing, steps), seen,
            (importPaths ++ tail)*)

case class Hash(id: Text, digest: Text, bin: Text)
case class Cache(hashes: Set[Hash])
case class Versioning(versions: List[Version])
case class Artifact(path: DiskPath, main: Option[Text], format: Format)

case class Version(digest: Text, major: Int, minor: Int):
  def version: Text = t"$major.$minor"

class FileCache[T]:
  private val files: scm.HashMap[Text, (Long, T)] = scm.HashMap()
  
  def apply(filename: Text, modified: Long)(calc: => T): T throws IoError =
    if !files.get(filename).fold(false)(_(0) == modified) then files(filename) = modified -> calc
    files(filename)(1)

given realm: Realm = Realm(t"irk")

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
    
    zipFile.entries.nn.asScala.to(LazyList).filter(!_.getName.nn.endsWith("/")).map: entry =>
      Entry(Relative.parse(entry.getName.nn.show), zipFile.getInputStream(entry).nn)

  // 00:00:00, 1 January 2000
  val epoch = attribute.FileTime.fromMillis(946684800000L)

  def write(base: jovian.File, path: jovian.DiskPath, inputs: LazyList[ZipEntry], prefix: Maybe[Bytes] = Unset)
           (using Stdout)
           : Unit throws StreamCutError | IoError =
    val tmpPath = Irk.tmpDir.tmpPath()
    base.copyTo(tmpPath)
    val uri: java.net.URI = java.net.URI.create(t"jar:file:${tmpPath.show}".s).nn
    val fs = FileSystems.newFileSystem(uri, Map("zipinfo-time" -> "false").asJava).nn

    val dirs = unsafely(inputs.map(_.path).map(_.parent)).to(Set).flatMap: dir =>
      (0 to dir.parts.length).map(dir.parts.take(_)).map(Relative(0, _)).to(Set)
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
    
    prefix.option.foreach: prefix =>
      fileOut.write(prefix.mutable(using Unsafe))
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
    
    synchronously(unsafely(Irk.cacheDir / hash)): workDir =>
      make(input, workDir)
      
      Cache.synchronized:
        latest.get(key).foreach:
          oldHash => if oldHash != hash then unsafely(Irk.cacheDir / hash).directory().delete()
        
        latest(key) = hash
        workDir

object Progress:
  enum Update:
    case Add(verb: Verb, hash: Text)
    case Remove(verb: Verb, result: Result)
    case Put(text: Text)
    case Print
    case SkipOne
    case Sigwinch
    case Stdout(verb: Verb, data: Bytes)

  def titleText(title: Text): Text = t"\e]0;$title\u0007\b \b"

case class Progress(active: TreeMap[Verb, (Text, Long)],
                        completed: List[(Verb, Text, Long, Result)],
                        started: Boolean = false, done: Int = 0, totalTasks: Int,
                        columns: Int = 120, buffers: Map[Verb, StringBuilder] = Map())
                   (using Tty):
  private def add(verb: Verb, hash: Text): Progress =
    copy(active = active.updated(verb, (hash, now())))
  
  private def remove(verb: Verb, result: Result): Progress = copy(
    active = active - verb,
    completed = (verb, active(verb)(0), active(verb)(1), result) :: completed
  )

  def apply(update: Progress.Update)(using Stdout): Progress = update match
    case Progress.Update.Sigwinch =>
      Out.println(t"resized")
      this

    case Progress.Update.Add(verb, hash) =>
      add(verb, hash)
    
    case Progress.Update.Remove(verb, result) =>
      remove(verb, result)
    
    case Progress.Update.SkipOne =>
      copy(totalTasks = totalTasks - 1)

    case Progress.Update.Put(text) =>
      Out.println(text)
      this
    
    case Progress.Update.Stdout(verb, data) =>
      if buffers.contains(verb) then
        buffers(verb).append(data.uString.s)
        this
      else copy(buffers = buffers.updated(verb, StringBuilder(data.uString.s)))

    case Progress.Update.Print =>
      val starting = if !Irk.githubActions then
        val starting = !started && completed.nonEmpty
        if done == 0 && completed.size > 0 then Out.println(t"─"*columns)
        status.map(_.render).foreach(Out.println(_))
        Out.println(t"\e[?25l\e[${active.size + 2}A")
        starting
      else
        completed.foreach:
          case (task, hash, start, success) =>
            val hashText = ansi"${palette.Hash}(${hash})"
            val time = ansi"${palette.Number}(${now() - start}ms)"
            Out.println(ansi"${task.past} $hashText ($time)".render)
            buffers
        false
        
      copy(completed = Nil, started = started || starting, done = done + completed.size)

  val braille = t"⡀⡄⠆⠇⠋⠛⠹⢹⣸⣼⣶⣷⣯⣿⣻⣽⣼⣶⣦⣇⡇⠏⠋⠙⠘⠰⠠ "

  private def status: List[AnsiText] =
    def line(verb: Verb, hash: Text, start: Long, result: Result, active: Boolean): AnsiText =
      val ds = (now() - start).show.drop(2, Rtl)
      val fractional = if ds.length == 0 then t"0" else ds.take(1, Rtl)
      val time = ansi"${if ds.length < 2 then t"0" else ds.drop(1, Rtl)}.${fractional}s"
      val padding = ansi" "*(7 - time.length.min(7))
      val hashText = ansi"${palette.Hash}($hash)"
      
      if active then
        val anim = unsafely(braille((((now() - start)/100)%braille.length).toInt))
        ansi"${colors.White}([$Yellow($anim)] $hashText ${verb.present.padTo(columns - 17, ' ')} $padding${palette.ActiveNumber}($time))"
      else
        val finish = result match
          case Result.Complete(_)   => if result.errors.isEmpty then ansi"[$Green(✓)]" else ansi"[$Red(✗)]"
          case Result.Terminal(_)   => ansi"[${colors.Crimson}(‼)]"
          case Result.Aborted       => ansi"[$Blue(⤹)]"
          case Result.Incomplete    => ansi"[${colors.Orange}(?)]"
        
        ansi"$Bold($finish) $hashText ${verb.past.padTo(columns - 17, ' ')} $padding${palette.Number}($time)"
    
    val title = Progress.titleText(t"Irk: building (${(done*100)/(totalTasks max 1)}%)")

    val content: List[AnsiText] = completed.flatMap: (task, _, start, _) =>
      val description = task match
        case Verb.Exec(cls) => t"Output from ${cls.plain}"
        case _              => t"Output"

      if buffers.contains(task) && !buffers(task).isEmpty then List(
        ansi"${Bg(colors.SaddleBrown)}(  )${Bg(colors.SandyBrown)}(${colors.SaddleBrown}()${colors.Black}( $description ))${Bg(colors.Black)}(${colors.SandyBrown}())${escapes.Reset}",
        AnsiText(buffers(task).toString.show)
      ) else Nil

    completed.map(line(_, _, _, _, false)) ++ content ++ List(ansi"${title}${t"\e[0G"}${t"─"*columns}") ++
      active.to(List).map:
        case (name, (hash, start)) => line(name, hash, start, Result.Incomplete, true)

trait Base256 extends EncodingScheme

object Base256:
  private val charset: Text = List(t"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
      t"ႠႡႢႣႤႥႦႧႨႩႪႫႬႮႯႰႱႲႴႵႶႷႸႹႻႼႾႿჀჁჂჃჄჅაბგდევზთიკლმნოპჟრსტუფქღყშჩცძწჭხჯჰჱჲჳჴჵჶჷჸჹჺჾ",
      t"БГДЖЗИЙЛПФЦЧШЩЪЫЬЭЮЯбвгджзийклмнптфцчшщъыьэюяΔΘΛΞΠΣΦΨΩαβγδεζηθιλμξπρςστυφψωϊϋ",
      t"ϐϑϔϕϖϗϘϙϚϛϜϝϞϟϠϡϢϣϤϥϦϧϨϪϫϬϭϮϯϰϱϵ϶ϷϸϻϼϽϾϿ123456789").join
 
  given ByteEncoder[Base256] = bytes =>
    bytes.map(_.toInt + 128).map[Char] { byte => unsafely(charset(byte)) }.map(_.show).join
