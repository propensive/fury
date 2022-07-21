package irk

import gossamer.*
import rudiments.*
import turbulence.*
import acyclicity.*
import euphemism.*
import joviality.*
import serpentine.*
import guillotine.*
import kaleidoscope.*
import escapade.*
import gastronomy.*
import iridescence.*, solarized.*
import eucalyptus.*
import imperial.*
import profanity.*
import xylophone.*
import telekinesis.*
import anticipation.*
import java.nio.BufferOverflowException

import scala.collection.mutable as scm
import scala.concurrent.*
import scala.util.chaining.scalaUtilChainingOps

import java.io as ji

import timekeeping.long
import encodings.Utf8
import rendering.ansi

given Env = envs.enclosing
erased given CanThrow[AppError] = compiletime.erasedValue
given LogFormat[SystemOut.type] = LogFormat.standardAnsi
given Log()

object palette:
  val File = colors.Coral
  val Number = colors.SandyBrown
  val ActiveNumber = colors.Gold
  val Hash = colors.MediumSeaGreen
  val Class = colors.MediumAquamarine

case class Build(pwd: Directory[Unix], repos: Map[DiskPath[Unix], Text], publishing: Option[Publishing],
    index: Map[Ref, Step] = Map(), targets: Map[Text, Target] = Map()):
  val steps: Set[Step] = index.values.to(Set)
  
  lazy val graph: Dag[Step] throws BrokenLinkError =
    val links = index.values.mtwin.map(_ -> _.allLinks.map(resolve(_)).to(Set))
    
    Dag(links.to(Seq)*)

  lazy val linearization: List[Step] throws BrokenLinkError = graph.sorted
  def sourceDirs: List[Directory[Unix]] = steps.flatMap(_.sources).to(List)
  def resourceDirs: List[Directory[Unix]] = steps.flatMap(_.resources).to(List)

  lazy val hashes: Map[Step, Text] throws BrokenLinkError | IoError | StreamCutError | ExcessDataError =
    def recur(todo: List[Step], hashes: Map[Step, Text])
             : Map[Step, Text] throws BrokenLinkError | StreamCutError | ExcessDataError =
      if todo.isEmpty then hashes
      else try
        val step = todo.head
        val inputsHash: List[Bytes] = step.srcFiles.to(List).sortBy(_.path.show).map(Irk.hashFile)
        val jarsHash: List[Bytes] = step.jars.to(List).map(_.digest[Crc32].bytes)
        val linksHash: List[Bytes] = step.allLinks.to(List).map(index(_)).map(hashes(_).bytes)
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
  
  def resolve(id: Ref): Step throws BrokenLinkError = index.get(id).getOrElse(throw BrokenLinkError(id))
  def bases: Set[Directory[Unix]] = steps.map(_.pwd).to(Set)

  def cache: Map[Step, Text] =
    try
      def encode(dir: Directory[Unix]): DiskPath[Unix] = Irk.hashDir / dir.path.show.digest[Crc32].encode[Hex]
      
      val caches = bases.map(encode).filter(_.exists()).map: cacheFile =>
        Json.parse(cacheFile.file().read[Text](64.kb)).as[Cache].hashes
      
      caches.flatten.flatMap:
        case Hash(id, hash, _) => try Set(resolve(id) -> hash) catch case e: BrokenLinkError => Set()
      .to(Map)
    catch
      case err: JsonParseError  => throw AppError(t"The cache file is not in the correct format", err)
      case err: StreamCutError  => throw AppError(t"The stream was cut while reading the cache file", err)
      case err: IoError         => throw AppError(t"There was an IO error while reading the cache",err)
      case err: JsonAccessError => throw AppError(t"The cache file was not in the correct JSON format", err)
      case err: Exception       => throw AppError(StackTrace(err).ansi.plain)
  
  def updateCache(step: Step, binDigest: Text): Unit = synchronized:
    try
      val cacheFile = Irk.hashDir / step.pwd.path.show.digest[Crc32].encode[Hex]
      val cache = Cache:
        if cacheFile.exists()
        then Json.parse(cacheFile.file().read[Text](64.kb)).as[Cache].hashes.filter: hash =>
          try resolve(hash.id).pwd == step.pwd catch case err: BrokenLinkError => false
        else Set()
      
      val newHash = Hash(step.id, hashes(step), binDigest)
      val newCache = cache.copy(hashes = cache.hashes.filter(_.id != step.id) + newHash)
      
      newCache.json.show.bytes.writeTo:
        if cacheFile.exists() then cacheFile.file(Expect).delete()
        cacheFile.file()
    catch
      case err: JsonParseError  => throw AppError(t"The cache file is not in the correct format", err)
      case err: StreamCutError  => throw AppError(t"The stream was cut while reading the cache file", err)
      case err: IoError         => throw AppError(t"There was an IO error while reading the cache", err)
      case err: JsonAccessError => throw AppError(t"The cache did not contain the correct JSON format", err)
      case err: Exception       => throw AppError(StackTrace(err).ansi.plain)

object Format:
  def unapply(value: Text): Option[Format] = safely(Format.valueOf(value.unkebab.pascal.s)).option

enum Format:
  case Jar, FatJar, App, DaemonApp, CompilerPlugin

case class Artifact(path: DiskPath[Unix], main: Option[Text], format: Format)

object Artifact:
  def build(artifact: Artifact, base: File[Unix], name: Text, version: Text, classpath: List[DiskPath[Unix]],
                resources: List[Directory[Unix]]): Unit throws IoError =
    import stdouts.drain
    
    val zipStreams = (base.path :: classpath.sortBy(_.show)).to(LazyList).flatMap: path =>
      if path.isFile then Zip.read(path.file(Expect)).filter(_.path.parts.last != t"MANIFEST.MF")
      else if path.isDirectory then path.descendantFiles().map: file =>
        Zip.Entry(file.path.relativeTo(path), ji.BufferedInputStream(ji.FileInputStream(file.javaFile)))
      else LazyList()
    
    val resourceStreams = resources.sortBy(_.path.show).flatMap: dir =>
      dir.path.descendantFiles().map: file =>
        Zip.Entry(file.path.relativeTo(dir.path), ji.BufferedInputStream(ji.FileInputStream(file.javaFile)))
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
    
    val in = ji.BufferedInputStream(ji.ByteArrayInputStream(manifest.bytes.mutable(using Unsafe)))
    val mfEntry = Zip.Entry(Relative.parse(t"META-INF/MANIFEST.MF"), in)
    
    val header = artifact.format match
      case Format.DaemonApp => unsafely((Classpath() / p"exoskeleton" / p"invoke").resource.read[Bytes](100.kb))
      case Format.App       => unsafely((Classpath() / p"exoskeleton" / p"invoke").resource.read[Bytes](100.kb))
      case _                => Bytes.empty
    
    val extraResources: LazyList[Zip.Entry] = artifact.format match
      case Format.CompilerPlugin =>
        val props = ji.ByteArrayInputStream(t"pluginClass=${artifact.main}\n".bytes.mutable(using Unsafe))
        LazyList(Zip.Entry(Relative.parse(t"plugin.properties"), props))
      case _ =>
        LazyList()
    
    unsafely:
      Zip.write(base, artifact.path, mfEntry #:: extraResources #::: resourceStreams #::: zipStreams, header)
    
    artifact.path.file(Expect).setPermissions(executable = true)

case class Step(path: File[Unix], publishing: Option[Publishing], name: Text,
                    id: Ref, links: Set[Ref], resources: Set[Directory[Unix]],
                    sources: Set[Directory[Unix]], jars: Set[Text], dependencies: Set[Dependency],
                    version: Text, docs: List[DiskPath[Unix]], artifact: Option[Artifact],
                    exec: Option[Exec], plugins: List[Plugin]):
  
  def publish(build: Build): Publishing = publishing.orElse(build.publishing).getOrElse:
    throw AppError(t"There are no publishing details for $id")
 
  def pwd: Directory[Unix] = unsafely(path.parent)
  def group(build: Build): Text = publish(build).group
  def docFile: DiskPath[Unix] = output(t"-javadoc.jar")
  def srcsPkg: DiskPath[Unix] = output(t"-sources.jar")
  def pomFile: DiskPath[Unix] = output(t".pom")
  def allLinks = links ++ plugins.map(_.id)
  
  private def output(extension: Text): DiskPath[Unix] = unsafely(pwd / t"bin" / t"${id.dashed}-$version$extension")
  private def compilable(f: File[Unix]): Boolean = f.name.endsWith(t".scala") || f.name.endsWith(t".java")

  def srcFiles: Set[File[Unix]] throws IoError =
    sources.flatMap(_.path.descendantFiles(!_.name.startsWith(t"."))).filter(compilable)

  def classpath(build: Build)(using Stdout, Internet): Set[DiskPath[Unix]] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap: step =>
      step.jars.map(Irk.fetchFile(_, None).await()).map(_.path) + step.classesDir.path
  
  def allResources(build: Build)(using Stdout): Set[Directory[Unix]] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap(_.resources)

  def compileClasspath(build: Build)(using Stdout, Internet): Set[DiskPath[Unix]] throws IoError | BrokenLinkError =
    classpath(build) - classesDir.path

  def classesDir: Directory[Unix] = synchronized:
    try unsafely(Irk.cacheDir / t"cls" / id.project / id.module).directory(Ensure)
    catch case err: IoError => throw AppError(t"Could not write to the user's home directory", err)

  def pomDependency(build: Build): Dependency = Dependency(group(build), id.dashed, version)

  def pomDependencies(build: Build): List[Dependency] =
    try allLinks.map(build.resolve(_)).map(_.pomDependency(build)).to(List) ++ dependencies
    catch case err: BrokenLinkError => throw AppError(t"Couldn't resolve dependencies", err)

  def compile(hashes: Map[Step, Text], oldHashes: Map[Step, Text], build: Build, scriptFile: File[Unix],
                  cancel: Promise[Unit], owners: Map[DiskPath[Unix], Step])
             (using Stdout, Internet)
             : Result =
    val t0 = now()
    
    try
      classesDir.children.foreach(_.delete())
      val cp = compileClasspath(build)
      val pluginRefs = List()
      val result = Compiler.compile(id, pwd, srcFiles.to(List), cp, classesDir, scriptFile, pluginRefs, cancel, owners)
      val time = now() - t0
      
      if result.success then
        val digestFiles = classesDir.path.descendantFiles().to(List).sortBy(_.name).to(LazyList)
        val digest = digestFiles.map(_.read[Bytes](1.mb).digest[Crc32]).to(List).digest[Crc32]
        build.updateCache(this, digest.encode[Base64])
      
      if cancel.isCompleted then Result.Aborted else result
    
    catch
      case err: IoError         => throw AppError(t"Could not read the source files", err)
      case err: StreamCutError  => throw AppError(t"Reading the source files broke before it completed", err)
      case err: ExcessDataError => throw AppError(t"Too much data was returned", err)
      case err: BrokenLinkError => throw AppError(t"There was an unsatisfied reference to ${err.link}", err)
      //case err: Error[?]        => throw AppError(t"An unexpected error occurred", err)

case class BuildConfig(imports: Option[List[Text]], publishing: Option[Publishing],
                           modules: List[Module], repos: Option[List[Repo]],
                           targets: Option[List[Target]]):
  
  def gen(current: DiskPath[Unix], build: Build, seen: Set[Text], files: DiskPath[Unix]*)(using Stdout)
         : Build throws IoError | AppError | BuildfileError =
    
    repos.presume.foreach:
      case Repo(base, uri) =>
        val root = unsafely(current.parent + Relative.parse(base))
        if !root.exists() then
          Out.println(ansi"Cloning repository $uri to $base".render)
          Irk.cloneRepo(root, uri)

    files.to(List) match
      case Nil =>
        build
      
      case path :: tail =>
        val steps: Map[Ref, Step] = modules.map: module =>
          def relativize(text: Text): DiskPath[Unix] = unsafely(path.file(Expect).parent.path + Relative.parse(text))
          val links = module.links.presume.map(Ref(_))
          val resources = module.resources.presume.map(relativize).map(_.directory(Expect))
          val sources = module.sources.map(relativize).map(_.directory(Expect))
          val dependencies = module.dependencies.presume
          val docs = module.docs.presume.map(relativize)
          val version = module.version.getOrElse(t"1.0.0")
          
          val artifact = module.artifact.map: spec =>
            val format = spec.format.flatMap(Format.unapply(_)).getOrElse(Format.FatJar)
            Artifact(unsafely(path.parent + Relative.parse(spec.path)), spec.main, format)
          
          val id = Ref(module.id)
          
          Step(path.file(), publishing, module.name, id, links, resources, sources, module.jars.presume,
              dependencies, version, docs, artifact, module.exec, module.plugins.presume.map(Plugin(_)))
        .mtwin.map(_.id -> _).to(Map)
        
        val importPaths = imports.presume.map: p =>
          unsafely(path.parent + Relative.parse(p))
        
        val reposMap: Map[DiskPath[Unix], Text] = repos.presume.map: repo =>
          unsafely(build.pwd.path + Relative.parse(repo.base)) -> repo.url
        .to(Map)

        Irk.readBuilds(build ++ Build(build.pwd, reposMap, build.publishing, steps), seen,
            (importPaths ++ tail)*)

class FileCache[T]:
  private val files: scm.HashMap[Text, (Long, T)] = scm.HashMap()
  
  def apply(filename: Text, modified: Long)(calc: => T): T throws IoError =
    if !files.get(filename).fold(false)(_(0) == modified) then files(filename) = modified -> calc
    
    files(filename)(1)

case class Cache(hashes: Set[Hash])

object Cache:
  private val latest: scm.HashMap[Text, Text] = scm.HashMap()
  private val locks: scm.HashMap[DiskPath[Unix], Lock] = scm.HashMap()
  private class Lock()

  private def sync(path: DiskPath[Unix])(block: Directory[Unix] => Unit): Directory[Unix] throws IoError =
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

  def apply[T: Hashable](key: Text, input: T)(make: (T, Directory[Unix]) => Unit)
           : Directory[Unix] throws IoError =
    val hash = input.digest[Crc32].encode[Hex].lower
    
    sync(unsafely(Irk.cacheDir / hash)): workDir =>
      make(input, workDir)
      
      Cache.synchronized:
        latest.get(key).foreach: oldHash =>
          if oldHash != hash then unsafely(Irk.cacheDir / hash).directory().delete()
        
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
                        columns: Int = 120, buffers: Map[Verb, StringBuilder] = Map()):
  private def add(verb: Verb, hash: Text): Progress =
    copy(active = active.updated(verb, (hash, now())))
  
  private def remove(verb: Verb, result: Result): Progress = copy(
    active = active - verb,
    completed = (verb, active(verb)(0), active(verb)(1), result) :: completed
  )

  def apply(update: Progress.Update)(using Stdout): Progress = update match
    case Progress.Update.Sigwinch =>
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

  private final val spinner = t"⡀⡄⠆⠇⠋⠛⠹⢹⣸⣼⣶⣷⣯⣿⣻⣽⣼⣶⣦⣇⡇⠏⠋⠙⠘⠰⠠ "
  
  def line(verb: Verb, hash: Text, start: Long, result: Result, active: Boolean): AnsiText =
    val ds = (now() - start).show.drop(2, Rtl)
    val fractional = if ds.length == 0 then t"0" else ds.take(1, Rtl)
    val time = ansi"${if ds.length < 2 then t"0" else ds.drop(1, Rtl)}.${fractional}s"
    val padding = ansi" "*(7 - time.length.min(7))
    val hashText = ansi"${palette.Hash}($hash)"
    
    if active then
      val animation = unsafely(spinner((((now() - start)/100)%spinner.length).toInt))
      val description = verb.present.padTo(columns - 17, ' ')
      ansi"${colors.White}([$Yellow($animation)] $hashText $description $padding${palette.ActiveNumber}($time))"
    else
      val finish = result match
        case Result.Complete(_)   => if result.errors.isEmpty then ansi"[$Green(✓)]" else ansi"[$Red(✗)]"
        case Result.Terminal(_)   => ansi"[${colors.Crimson}(‼)]"
        case Result.Aborted       => ansi"[$Blue(⤹)]"
        case Result.Incomplete    => ansi"[${colors.Orange}(?)]"
      
      ansi"$Bold($finish) $hashText ${verb.past.padTo(columns - 17, ' ')} $padding${palette.Number}($time)"

  private def status: List[AnsiText] =
    val title = Progress.titleText(t"Irk: building (${(done*100)/(totalTasks max 1)}%)")

    val content: List[AnsiText] = completed.flatMap: (task, _, start, _) =>
      val description = task match
        case Verb.Exec(cls) => t"Output from ${cls.plain}"
        case _              => t"Output"

      if buffers.contains(task) && !buffers(task).isEmpty then List(ansi"", ansi"",
        ansi"${Bg(colors.SaddleBrown)}(  )${Bg(colors.SandyBrown)}(${colors.SaddleBrown}()${colors.Black}( $description ))${Bg(colors.Black)}(${colors.SandyBrown}())${escapes.Reset}",
        AnsiText(buffers(task).toString.show)
      ) else Nil

    completed.map(line(_, _, _, _, false)) ++ content ++ List(ansi"${title}${t"\e[0G"}${t"─"*columns}") ++
      active.to(List).map:
        case (name, (hash, start)) => line(name, hash, start, Result.Incomplete, true)

given realm: Realm = Realm(t"irk")