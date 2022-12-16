package fury

import gossamer.*
import rudiments.*
import parasitism.*, threading.virtual
import turbulence.*
import acyclicity.*
import euphemism.*
import cellulose.*
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
import tetromino.*
import anticipation.*
import java.nio.BufferOverflowException

import scala.collection.mutable as scm
import scala.util.chaining.scalaUtilChainingOps

import java.io as ji

import timekeeping.long
import rendering.ansi

erased given CanThrow[AppError] = compiletime.erasedValue
given LogFormat[File[Unix]] = LogFormat.standardAnsi
given Encoding = encodings.Utf8

//val LogFile = unsafely(Unix.parse(t"/var/log/fury.log").file(Ensure).sink)
//given log: Log = Log(
  //{ case _ => LogFile }
//)(using monitors.global)

given log: Log = logging.silent

object palette:
  val File = colors.Coral
  val Number = colors.SandyBrown
  val ActiveNumber = colors.Gold
  val Hash = colors.MediumSeaGreen
  val Class = colors.MediumAquamarine

object Build:
  def apply(pwd: Directory[Unix], command: Maybe[Target], universe: Universe)
           (using Environment, Monitor, Threading, Stdout)
           : Build throws GitError | IoError | EnvError | RootParentError | CancelError =
    
    def steps(todo: List[Ref], done: Map[Ref, Step]): Build = todo match
      case Nil => Build(pwd, done)
      case head :: tail =>
        if done.contains(head) then steps(tail, done) else
          val project = universe.index(ProjectId(head.project))
          
          val module =
            try project.index(head.module) catch case err: Exception =>
              throw AppError(t"The module ${head.module} was not found in project ${project.id}")
          
          val ref = module.id.in(project.id)
          val includes = module.include.map(_.in(project.id))
          val uses = module.use.map(_.in(project.id))
          val resources = module.resource.map(project.resolve(_))
          val sources: Set[Directory[Unix]] = module.source.map(project.resolve(_)).sift[Directory[Unix]]

          steps((includes ++ uses).to(List) ::: tail, done.updated(ref, Step(universe, project.root, ref.show, ref, includes ++ uses, resources, sources,
                   Set(), t"1.0.0", Nil, None, None, Nil, None, module.js.or(false))))

    command.mm: cmd =>
      steps(cmd.include.map(_.ref), Map())
    .or(throw AppError(t"No valid build command was specified, and there is no default command"))
    
case class Build(pwd: Directory[Unix], index: Map[Ref, Step] = Map()):
  val steps: Set[Step] = index.values.to(Set)
  val stepsMap = steps.map { step => step.id -> step }.to(Map)
  val plugins: Set[Ref] = steps.flatMap(_.plugins.map(_.id))
  def apply(ref: Ref): Step = stepsMap(ref)
  
  lazy val graph: Dag[Step] throws BrokenLinkError =
    val links = index.values.mtwin.map(_ -> _.allLinks.map(resolve(_)).to(Set))
    
    Dag(links.to(Seq)*)

  lazy val linearization: List[Step] throws BrokenLinkError = graph.sorted
  def sourceDirs: List[Directory[Unix]] = steps.flatMap(_.sources).to(List)
  def resourceDirs: List[Directory[Unix]] = steps.flatMap(_.resources).to(List)


  private var hashesResult: Option[Map[Step, Digest[Crc32]]] = None
  def clearHashes(): Unit = synchronized { hashesResult = None }
  def hashes(using Allocator)
            : Map[Step, Digest[Crc32]] throws BrokenLinkError | IoError | StreamCutError | ExcessDataError =

    def recur(todo: List[Step], hashes: Map[Step, Digest[Crc32]])
             : Map[Step, Digest[Crc32]] throws BrokenLinkError | StreamCutError | ExcessDataError =
      if todo.isEmpty then hashes
      else try
        val step = todo.head
        val inputsHash: List[Digest[Crc32]] = step.srcFiles.to(List).sortBy(_.path.fullname).map(Fury.hashFile)
        val jarsHash: List[Digest[Crc32]] = step.jars.to(List).map(_.digest[Crc32])
        val linksHash: List[Digest[Crc32]] = step.allLinks.to(List).map(index(_)).map(hashes(_))
        val newHash = (inputsHash ++ linksHash).digest[Crc32]
        
        recur(todo.tail, hashes.updated(step, newHash))
      catch case err: Error[?] => throw AppError(t"An unknown error occurred", err)
    
    
    synchronized:
      if hashesResult.isEmpty then hashesResult = Some(recur(linearization, Map()))
      hashesResult.get

  @targetName("addAll")
  infix def ++(build: Build): Build =
    Build(build.pwd, /*build.repos, publishing.orElse(build.publishing), */index ++ build.index)
  
  def resolve(id: Ref): Step throws BrokenLinkError = index.get(id).getOrElse(throw BrokenLinkError(id))
  def bases: Set[Directory[Unix]] = steps.map(_.pwd).to(Set)

  def cache(using Allocator, Environment): Map[Step, Digest[Crc32]] =
    try
      val caches = bases.map(Fury.hashDir / _.path.fullname.digest[Crc32].encode[Hex]).filter(_.exists()).map: cacheFile =>
        Json.parse(cacheFile.file(Ensure).read[Text]()).as[Cache].hashes
      
      caches.flatten.flatMap:
        case Hash(id, hash, _) => try Set(resolve(id) -> hash) catch case e: BrokenLinkError => Set()
      .to(Map)
    catch
      case err: JsonParseError  => throw AppError(t"The cache file is not in the correct format", err)
      case err: StreamCutError  => throw AppError(t"The stream was cut while reading the cache file", err)
      case err: IoError         => throw AppError(t"There was an IO error while reading the cache",err)
      case err: JsonAccessError => throw AppError(t"The cache file was not in the correct JSON format", err)
      case err: Exception       => throw AppError(StackTrace(err).ansi.plain)
  
  def updateCache(step: Step, binDigest: Text)(using Allocator, Environment): Unit = synchronized:
    try
      val cacheFile = Fury.hashDir / step.pwd.path.fullname.digest[Crc32].encode[Hex]
      val cache = Cache:
        if cacheFile.exists()
        then Json.parse(cacheFile.file(Ensure).read[Text]()).as[Cache].hashes.filter: hash =>
          try resolve(hash.id).pwd == step.pwd catch case err: BrokenLinkError => false
        else Set()
      
      val newHash = Hash(step.id, hashes(step), binDigest)
      val newCache = cache.copy(hashes = cache.hashes.filter(_.id != step.id) + newHash)
      
      newCache.json.show.bytes.writeTo:
        if cacheFile.exists() then cacheFile.file(Expect).delete()
        cacheFile.file(Ensure)
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
                resources: List[Directory[Unix]], mainClass: Option[Text])(using Allocator, Environment)
           : Unit throws IoError =
    import stdouts.drain
    
    val zipStreams = (base.path :: classpath.sortBy(_.fullname)).to(LazyList).flatMap: path =>
      if path.isFile then Zip.read(path.file(Expect)).filter(_.path.parts.last != t"MANIFEST.MF")
      else if path.isDirectory then path.descendantFiles().map: file =>
        Zip.Entry(file.path.relativeTo(path), ji.BufferedInputStream(ji.FileInputStream(file.javaFile)))
      else LazyList()
    
    val resourceStreams = resources.sortBy(_.path.fullname).flatMap: dir =>
      dir.path.descendantFiles().map: file =>
        Zip.Entry(file.path.relativeTo(dir.path), ji.BufferedInputStream(ji.FileInputStream(file.javaFile)))
    .to(LazyList)
    
    val basicMf = ListMap(
      t"Manifest-Version"       -> t"1.0",
      t"Created-By"             -> t"Fury ${Fury.version}",
      t"Implementation-Title"   -> name,
      t"Implementation-Version" -> version
    )
    
    val manifest = mainClass.fold(basicMf)(basicMf.updated(t"Main-Class", _)).flatMap: (k, v) =>
      val (first, rest) = t"$k: $v".snip(72)
      first :: rest.s.grouped(71).map(t" "+_.show).to(List)
    .join(t"", t"\n", t"\n")
    
    val in = ji.BufferedInputStream(ji.ByteArrayInputStream(manifest.bytes.mutable(using Unsafe)))
    val mfEntry = Zip.Entry(Relative.parse(t"META-INF/MANIFEST.MF"), in)
    
    val header = artifact.format match
      case Format.DaemonApp => unsafely((Classpath() / p"exoskeleton" / p"invoke").resource.read[Bytes]())
      case Format.App       => unsafely((Classpath() / p"exoskeleton" / p"invoke").resource.read[Bytes]())
      case _                => Bytes.empty
    
    val extraResources: LazyList[Zip.Entry] = artifact.format match
      case Format.CompilerPlugin =>
        val props = ji.ByteArrayInputStream(t"pluginClass=${mainClass}\n".bytes.mutable(using Unsafe))
        LazyList(Zip.Entry(Relative.parse(t"plugin.properties"), props))
      case _ =>
        LazyList()
    
    unsafely:
      Zip.write(base, artifact.path, mfEntry #:: extraResources #::: resourceStreams #::: zipStreams, header)
    
    artifact.path.file(Expect).setPermissions(executable = true)

case class Step(uni: Universe, pwd: Directory[Unix], /*path: File[Unix], publishing: Option[Publishing],*/ name: Text,
                    id: Ref, links: Set[Ref], resources: Set[Directory[Unix]],
                    sources: Set[Directory[Unix]], jars: Set[Text],
                    version: Text, docs: List[DiskPath[Unix]], artifact: Option[Artifact],
                    exec: Option[Exec], plugins: List[Plugin], main: Option[Text], js: Boolean):
  
  def publish(build: Build): Publishing =// publishing.orElse(build.publishing).getOrElse:
    throw AppError(t"There are no publishing details for $id")
 
  def group(build: Build): Text = publish(build).group
  def docFile: DiskPath[Unix] = output(t"-javadoc.jar")
  def srcsPkg: DiskPath[Unix] = output(t"-sources.jar")
  def pomFile: DiskPath[Unix] = output(t".pom")
  def allLinks = links ++ plugins.map(_.id)
  
  private def output(extension: Text): DiskPath[Unix] = unsafely(pwd / t"bin" / t"${id.dashed}-$version$extension")
  private def compilable(f: File[Unix]): Boolean = f.name.endsWith(t".scala") || f.name.endsWith(t".java")

  def srcFiles: Set[File[Unix]] throws IoError =
    sources.flatMap(_.path.descendantFiles(!_.name.startsWith(t"."))).filter(compilable)

  def classpath(build: Build)(using Stdout, Environment): Set[DiskPath[Unix]] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap: step =>
      step.jars.map(Fury.getFile(_)).map(_.path) + step.classesDir.path
  
  def allResources(build: Build)(using Stdout): Set[Directory[Unix]] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap(_.resources)

  def compileClasspath(build: Build)(using Stdout, Environment): Set[DiskPath[Unix]] throws IoError | BrokenLinkError =
    classpath(build) - classesDir.path

  def classesDir(using Environment): Directory[Unix] = synchronized:
    try unsafely(Fury.cacheDir / t"cls" / id.project / id.module).directory(Ensure)
    catch case err: IoError => throw AppError(t"Could not write to the user's home directory", err)

  def pomDependency(build: Build): Maven.Dependency = Maven.Dependency(group(build), id.dashed, version)

  def pomDependencies(build: Build): List[Maven.Dependency] =
    try allLinks.map(build.resolve(_)).map(_.pomDependency(build)).to(List)// ++ dependencies
    catch case err: BrokenLinkError => throw AppError(t"Couldn't resolve dependencies", err)

  def compile(hashes: Map[Step, Digest[Crc32]], oldHashes: Map[Step, Digest[Crc32]], build: Build,
                  scriptFile: File[Unix], cancel: Promise[Unit], owners: Map[DiskPath[Unix], Step])
             (using Stdout, Internet, Monitor, Allocator, Environment)
             : Result =
    val t0 = now()
    
    try
      classesDir.children.foreach(_.delete())
      val cp = compileClasspath(build)
      
      val pluginRefs = plugins.map: plugin =>
        PluginRef(Fury.libJar(build.hashes(build(plugin.id))), plugin.params)
      
      val result = Compiler.compile(id, pwd, srcFiles.to(List).sortBy(_.fullname), cp.to(List).sortBy(_.fullname),
          classesDir, scriptFile, pluginRefs, cancel, owners, js)
      
      val time = now() - t0
      
      if result.success then
        val digestFiles = classesDir.path.descendantFiles().to(List).sortBy(_.name).to(LazyList)
        val digest = digestFiles.map(_.read[Bytes]().digest[Crc32]).to(List).digest[Crc32]
        build.updateCache(this, digest.encode[Base64])
      
      if cancel.ready then Result.Aborted else result
    
    catch
      case err: IoError         => throw AppError(t"Could not read the source files", err)
      case err: StreamCutError  => throw AppError(t"Reading the source files broke before it completed", err)
      case err: ExcessDataError => throw AppError(t"Too much data was returned", err)
      case err: BrokenLinkError => throw AppError(t"There was an unsatisfied reference to ${err.link}", err)
      //case err: Error[?]        => throw AppError(t"An unexpected error occurred", err)


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

  def apply[T: Hashable](key: Text, input: T)(make: (T, Directory[Unix]) => Unit)(using Environment)
           : Directory[Unix] throws IoError =
    val hash = input.digest[Crc32].encode[Hex].lower
    
    sync(unsafely(Fury.cacheDir / hash)): workDir =>
      make(input, workDir)
      
      Cache.synchronized:
        latest.get(key).foreach: oldHash =>
          if oldHash != hash then unsafely(Fury.cacheDir / hash).directory().mm(_.delete())
        
        latest(key) = hash
        workDir

object Progress:
  enum Update:
    case Add(verb: Verb, hash: Digest[Crc32])
    case Remove(verb: Verb, result: Result)
    case Resize(cols: Int)
    case Put(text: Text)
    case Print
    case SkipOne
    case Stdout(verb: Verb, data: Bytes)

  def titleText(title: Text): Text = t"\e]0;$title\u0007\b \b"

case class Progress(active: TreeMap[Verb, (Digest[Crc32], Long)],
                        completed: List[(Verb, Digest[Crc32], Long, Result)],
                        started: Boolean = false, done: Int = 0, totalTasks: Int,
                        columns: Int = 120, buffers: Map[Verb, StringBuilder] = Map()):
  private def add(verb: Verb, hash: Digest[Crc32]): Progress =
    copy(active = active.updated(verb, (hash, now())))
  
  private def remove(verb: Verb, result: Result): Progress = copy(
    active = active - verb,
    completed = (verb, active(verb)(0), active(verb)(1), result) :: completed
  )

  def apply(update: Progress.Update)(using Stdout, Environment): Progress = update match
    case Progress.Update.Resize(cols) =>
      copy(columns = cols)

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
      val starting = if !Fury.githubActions then
        val starting = !started && completed.nonEmpty
        if done == 0 && completed.size > 0 then Out.println(t"─"*columns)
        status.map(_.render).foreach(Out.println(_))
        Out.println(t"\e[?25l\e[${active.size + 2}A")
        starting
      else
        completed.foreach:
          case (task, hash, start, success) =>
            val hashText = ansi"${palette.Hash}(${hash.encode[Base256]})"
            val time = ansi"${palette.Number}(${now() - start}ms)"
            Out.println(ansi"${task.past} $hashText ($time)".render)
            buffers
        false
        
      copy(completed = Nil, started = started || starting, done = done + completed.size)

  private final val spinner = t"⡀⡄⠆⠇⠋⠛⠹⢹⣸⣼⣶⣷⣯⣿⣻⣽⣼⣶⣦⣇⡇⠏⠋⠙⠘⠰⠠ "
  
  def line(verb: Verb, hash: Digest[Crc32], start: Long, result: Result, active: Boolean): AnsiText =
    val ds = (now() - start).show.drop(2, Rtl)
    val fractional = if ds.length == 0 then t"0" else ds.take(1, Rtl)
    val time = ansi"${if ds.length < 2 then t"0" else ds.drop(1, Rtl)}.${fractional}s"
    val padding = ansi" "*(7 - time.length.min(7))
    val hashText = ansi"${palette.Hash}(${hash.encode[Base256]})"
    
    if active then
      val animation = unsafely(spinner((((now() - start)/100)%spinner.length).toInt))
      val description = verb.present.pad(columns - 17)
      ansi"${colors.White}([$Yellow($animation)] $hashText $description $padding${palette.ActiveNumber}($time))"
    else
      val finish = result match
        case Result.Complete(_)   => if result.errors.isEmpty then ansi"[$Green(✓)]" else ansi"[$Red(✗)]"
        case Result.Terminal(_)   => ansi"[${colors.Crimson}(‼)]"
        case Result.Aborted       => ansi"[$Blue(⤹)]"
        case Result.Incomplete    => ansi"[${colors.Orange}(?)]"
      
      ansi"$Bold($finish) $hashText ${verb.past.pad(columns - 17)} $padding${palette.Number}($time)"

  private def status: List[AnsiText] =
    val title = Progress.titleText(t"Fury: building (${(done*100)/(totalTasks max 1)}%)")

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

given realm: Realm = Realm(t"fury")
