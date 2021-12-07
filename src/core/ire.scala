package ire

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
import profanity.*
import xylophone.*
import java.nio.BufferOverflowException

import scala.collection.mutable as scm
import scala.concurrent.*
import scala.annotation.*

given Env = envs.enclosing

val scalacliRealm = Realm(t"scalacli")
given LogFormat[Stdout.type, AnsiString] = LogFormat.timed
given Log = Log(scalacliRealm |-> Stdout, realm |-> Stdout)

erased given CanThrow[RootParentError] = compiletime.erasedValue

@main
def run(args: String*): Unit =
  try args match
    case "compile" :: params => Ire.build(false, params.headOption.map(_.show), false, None)
    case "publish" :: params => Ire.build(true, params.headOption.map(_.show), false, None)
    case "watch" :: params   => Ire.build(false, params.headOption.map(_.show), true, None)
    case params              => Ire.build(false, params.headOption.map(_.show), false, None)
  catch case err: Exception =>
    Log.fail(err)
    sys.exit(1)

case class BrokenLinkError(link: Text)
extends Exception(s"The reference to $link cannot be resolved")

object Build:
  def empty: Build = Build(Map())
case class Build(index: Map[Text, Step]):
  val steps: Set[Step] = index.values.to(Set)
  
  lazy val graph: Dag[Step] throws BrokenLinkError =
    val links = index.values.map:
      step => step -> step.links.map(resolve(_)).to(Set)
    
    Dag(links.to(Seq)*)

  lazy val linearization: List[Step] throws BrokenLinkError = graph.sorted
  def sourceDirs: List[Unix.Directory] = steps.flatMap(_.sources).to(List)

  lazy val hashes: Map[Step, Text] throws BrokenLinkError | IoError | StreamCutError | AppError |
      ExcessDataError =
    def recur(todo: List[Step], hashes: Map[Step, Text])
             : Map[Step, Text] throws BrokenLinkError | StreamCutError | ExcessDataError | AppError =
      if todo.isEmpty then hashes
      else try
        val step = todo.head
        val inputsHash: List[Bytes] = (step.srcFiles ++ step.jars).map(Ire.hashFile)
        val linksHash: List[Bytes] = step.links.map(index(_)).map(hashes(_).bytes)
        val newHash = (inputsHash ++ linksHash).digest[Crc32].encode[Base64]
        
        recur(todo.tail, hashes.updated(step, newHash))
      catch case err: Exception => throw AppError(t"An unknown error occurred: ${err.toString.show}")
    val t0 = System.currentTimeMillis
    val result = recur(linearization, Map())
    val time = System.currentTimeMillis - t0
    Log.info(ansi"Calculated ${Yellow}(${index.size}) hashes in ${Yellow}[${time}ms]")
    result

  @targetName("addAll")
  infix def ++(build: Build): Build = Build(index ++ build.index)
  
  def resolve(id: Text): Step throws BrokenLinkError = index.get(id).getOrElse:
    throw BrokenLinkError(id)
  
  def transitive[T](fn: Step => List[T])(step: Step): List[T] throws BrokenLinkError =
    graph.reachable(step).to(List).flatMap(fn)
  
  def transitiveIn[T](fn: Step => List[T])(step: Step): List[T] throws BrokenLinkError =
    (graph.reachable(step) - step).to(List).flatMap(fn)

  def bases: Set[Unix.Directory] = steps.map(_.pwd).to(Set)

  def cache: Map[Step, Text] throws AppError =
    try
      val caches = bases.map(_.path / t".ire").filter(_.exists()).map:
        cacheFile => Json.parse(cacheFile.file.read[Text](64.kb)).as[Cache].hashes
      
      caches.flatten.flatMap:
        case Hash(id, hash, _) => try Set(resolve(id) -> hash) catch case e: BrokenLinkError => Set()
      .to(Map)
    catch
      case err: JsonParseError  => throw AppError(t"The cache file is not in the correct format")
      case err: StreamCutError  => throw AppError(t"The stream was cut while reading the cache file")
      case err: IoError         => throw AppError(t"There was an IO error while reading the cache")
      case err: JsonTypeError   => throw AppError(t"The cache file did not contain the correct JSON format")
      case err: JsonAccessError => throw AppError(t"The cache file did not contain the correct JSON format")
      case err: Exception       => throw AppError(t"The cache file could not be read: ${err.toString.show}")
  
  def updateCache(step: Step, binDigest: Text): Unit throws AppError =
    synchronized:
      try
        val cacheFile = (step.pwd / t".ire")
        val cache =
          if cacheFile.exists()
          then Cache:
            Json.parse(cacheFile.file.read[Text](64.kb)).as[Cache].hashes.filter:
              hash => try resolve(hash.id).pwd == step.pwd catch case err: BrokenLinkError => false
          else Cache(Set())
        
        val newHash = Hash(step.id, hashes(step), binDigest)
        val newCache = cache.copy(hashes = cache.hashes.filter(_.id != step.id) + newHash)
        cacheFile.createFile(overwrite = true).write(newCache.json.show.bytes)
      catch
        case err: JsonParseError  => throw AppError(t"The cache file is not in the correct format")
        case err: StreamCutError  => throw AppError(t"The stream was cut while reading the cache file")
        case err: IoError         => throw AppError(t"There was an IO error while reading the cache")
        case err: JsonTypeError   => throw AppError(t"The cache file did not contain the correct JSON format")
        case err: JsonAccessError => throw AppError(t"The cache file did not contain the correct JSON format")
        case err: Exception       => throw AppError(t"The cache file could not be read: ${err.toString.show}")

case class Step(path: Unix.File, config: Config, publishing: Option[Publishing], name: Text,
                    id: Text, links: List[Text], resources: List[Unix.Directory],
                    sources: List[Unix.Directory], jars: List[Unix.File],
                    dependencies: List[Text], version: Text, docs: List[Unix.IoPath],
                    artifact: Option[Unix.IoPath], main: Option[Text]):
  def projectId: Text = id.cut(t"/").head
  def moduleId: Text = id.cut(t"/").last
  def dashed: Text = t"$projectId-$moduleId"
  def pwd: Unix.Directory = path.parent
  
  def group: Text throws AppError =
    publishing.getOrElse:
      throw AppError(t"Publishing details have not been specified")
    .group

  def pkg: Unix.IoPath throws AppError = output(t".jar")
  def docFile: Unix.IoPath throws AppError = output(t"-javadoc.jar")
  def srcsPkg: Unix.IoPath throws AppError = output(t"-sources.jar")
  def pomFile: Unix.IoPath throws AppError = output(t".pom")

  private def output(extension: Text): Unix.IoPath =
    pwd / t"bin" / t"$dashed-$version$extension"
  
  def compilable(f: Unix.File): Boolean = f.name.endsWith(t".scala") || f.name.endsWith(t".java")

  def srcFiles: List[Unix.File] throws IoError =
    sources.flatMap(_.path.descendantFiles(!_.name.startsWith(t".")).to(List)).filter(compilable)

  def classpath(build: Build): List[Unix.IoPath] throws IoError | BrokenLinkError | AppError =
    (build.graph.reachable(this) - this).to(List).flatMap:
      step => step.pkg :: step.jars.map(_.path)

  def pomDependency(build: Build): Dependency throws AppError = Dependency(group, dashed, version)

  def pomDependencies(build: Build): List[Dependency] throws AppError =
    try links.map(build.resolve(_)).map(_.pomDependency(build)).to(List)
    catch case err: BrokenLinkError => throw AppError(t"Couldn't resolve dependencies")

  def compile(hashes: Map[Step, Text], oldHashes: Map[Step, Text], build: Build)
             : Unit throws AppError =
    try
      val srcs = srcFiles.map(_.show)
      if !pkg.parent.exists() then pkg.parent.createDirectory()
      val classpath = build.transitive(_.classpath(build))(this).map(_.show).flatMap(List(t"--jar", _))
      val resourceArgs = build.transitive(_.resources)(this).map(_.show).flatMap(List(t"--resource", _))
      val extraJars = build.transitive(_.jars)(this).map(_.show).flatMap(List(t"--jar", _))
      val deps = build.transitive(_.dependencies)(this).map(_.show).flatMap(List(t"-d", _))
      
      val cmd = sh"scala-cli package --library -o $pkg -f $classpath $resourceArgs $extraJars $deps -S ${config.scalac.version} --progress ${config.scalac.options} $srcs"

      val t0 = System.currentTimeMillis
      
      val process = cmd.fork[ExitStatus]()
      
      process.await() match
        case ExitStatus.Ok =>
          val time = System.currentTimeMillis - t0
          Log.info(ansi"Compilation of ${Green}[${name}] succeeded in ${Yellow}[${time}ms]")
          
          if !resources.isEmpty then
            val resourceArgs = resources.to(List).flatMap:
              dir =>
                Log.info(t"Adding resource dir $dir")
                t"-C" :: dir.show :: dir.path.descendantFiles(!_.name.startsWith(t".")).to(List).map(_.path.relativeTo(dir.path).show)
            
            sh"jar uf $pkg $resourceArgs".exec[ExitStatus]() match
              case ExitStatus.Ok      => Log.info(t"Added files to $pkg")
              case ExitStatus.Fail(n) => Log.warn(t"Failed to add files to $pkg")
          
          val stream = pkg.file.read[DataStream]().map:
            chunk => try chunk catch case err: StreamCutError => Bytes()
          val binDigest = stream.digest[Crc32].encode[Base64]
          build.updateCache(this, binDigest)
          
          locally:
            given Realm = scalacliRealm
            val stderr: Text = process.stderr(1.mb).slurp(1.mb).uString
            val stdout: Text = process.stdout(1.mb).slurp(1.mb).uString
            stdout.cut(t"\n").filter(!_.isEmpty).foreach(Log.info(_))
            stderr.cut(t"\n").filter(!_.isEmpty).foreach(Log.info(_))
          
          main.foreach:
            mainClass => sh"scala-cli run -M $mainClass $classpath $resourceArgs $extraJars $deps -S ${config.scalac.version} --progress ${config.scalac.options} $srcs".exec[LazyList[Text]]().foreach(Log.info(_))

        case ExitStatus.Fail(n) =>
          given Realm = scalacliRealm
          val stderr: Text = process.stderr(1.mb).slurp(1.mb).uString
          val stdout: Text = process.stdout(1.mb).slurp(1.mb).uString
          stdout.cut(t"\n").filter(!_.isEmpty).foreach(Log.info(_))
          stderr.cut(t"\n").filter(!_.isEmpty).foreach(Log.info(_))
    
    catch
      case err: IoError =>
        throw AppError(t"Could not read the source files: ${err.toString.show}")
      
      case err: StreamCutError =>
        throw AppError(t"The stream from scala-cli was broken before it completed")
      
      case err: ExcessDataError =>
        throw AppError(t"The scala-cli process returned too much data")
      
      case BrokenLinkError(ref) =>
        throw AppError(t"There was an unsatisfied reference to $ref")

case class BuildConfig(imports: Option[List[Text]], config: Config, publishing: Option[Publishing],
                           modules: List[Module]):
  
  def gen(build: Build, seen: Set[Text], files: Unix.File*)
         : Build throws IoError | AppError | BuildfileError =
    files.to(List) match
      case Nil =>
        build
      
      case file :: tail =>
        Log.info(ansi"Reading build file $Violet(${file.path.relativeTo(Ire.pwd.path).show})")
        val steps: Map[Text, Step] = modules.map:
          module =>
            def relativize(text: Text): Unix.IoPath = file.parent.path ++ Relative.parse(text)
            val links = module.links.getOrElse(Nil)
            val resources = module.resources.getOrElse(Nil).map(relativize).map(_.directory)
            val sources = module.sources.map(relativize).map(_.directory)
            val jars = module.jars.getOrElse(Nil).map(relativize).map(_.file)
            val dependencies = module.dependencies.getOrElse(Nil)
            val docs = module.docs.getOrElse(Nil).map(relativize)
            val version = module.version.getOrElse(t"1.0.0")
            val artifactPath = module.artifact.map(file.parent.path ++ Relative.parse(_))
            
            Step(file, config, publishing, module.name, module.id, links, resources, sources, jars,
                dependencies, version, docs, artifactPath, module.main)
        .mtwin.map(_.id -> _).to(Map)
        
        val importFiles = imports.getOrElse(Nil).map:
          path => (file.parent.path ++ Relative.parse(path)).file
        
        Ire.readBuilds(build ++ Build(steps), seen, (importFiles ++ tail)*)

case class Publishing(username: Text, group: Text, url: Text, organization: Organization,
                          developers: List[Developer])

case class Config(scalac: Scalac)
case class Scalac(version: Text, options: List[Text])

case class Module(name: Text, id: Text, links: Option[List[Text]], resources: Option[List[Text]],
                      sources: List[Text], jars: Option[List[Text]], docs: Option[List[Text]],
                      dependencies: Option[List[Text]], version: Option[Text],
                      artifact: Option[Text], main: Option[Text])

case class AppError(message: Text) extends Exception
case class BuildfileError() extends Exception

object Ire:
  
  private lazy val fileHashes: FileCache[Bytes] = new FileCache()

  val pwd: Unix.Directory =
    try Unix.Pwd.directory
    catch
      case err: IoError =>
        Log.warn(t"The PWD reported is not a directory")
        sys.exit(1)
      case err: PwdError =>
        Log.warn(t"Failed to get PWD")
        sys.exit(1)
  
  private def init(target: Option[Text]): Build throws AppError | IoError | BuildfileError =
    val path = Ire.pwd / (target.getOrElse(t"build")+t".ire")
    
    try readBuilds(Build.empty, Set(), path.file)
    catch case err: IoError =>
      Log.fail(ansi"Configuration file $Violet(${path.show})")
      sys.exit(1)
  
  def hashFile(file: Unix.File): Bytes throws IoError | AppError =
    try fileHashes(file.path.show, file.modified):
      file.read[Bytes](1.mb).digest[Crc32].bytes
    catch
      case err: StreamCutError  => throw AppError(t"The stream was cut while hashing a file")
      case err: ExcessDataError => throw AppError(t"The file was too big to hash")
      case err: Exception       => throw AppError(t"An unexpected error occurred")
  
  def readBuilds(build: Build, seen: Set[Text], files: Unix.File*)
                : Build throws AppError | BuildfileError =
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
          Log.fail(t"The $op operation at ${path.toString} did not complete because $reason")
          throw BuildfileError()
      
      case err: ExcessDataError =>
        Log.warn(t"The configuration file was larger than 1MB")
        throw BuildfileError()
      
      case err: StreamCutError =>
        abort(t"The configuration file could not be read completely")
      
      case err: JsonParseError =>
        Log.warn(t"The configuration file was not valid JSON")
        throw BuildfileError()
      
      case err: AppError =>
        throw err

      case err: BuildfileError =>
        throw err

      case err: Exception =>
        Log.fail(t"An unexpected error occurred: ${err.toString.show}")
        err.printStackTrace()
        abort(t"An unexpected error occurred: ${err.toString.show}")

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
        catch case err: Exception => readImports(seen, tail*)

  def build(publishSonatype: Boolean, target: Option[Text], watch: Boolean = false,
                oldBuild: Option[Build])
           : Unit throws AppError =
    try
      import unsafeExceptions.canThrowAny
      val build = oldBuild.getOrElse(init(target))
      val oldHashes = build.cache
      given ExecutionContext = ExecutionContext.global

      val futures = build.graph.traversal[Future[Unit]]:
        (set, step) => Future.sequence(set).flatMap:
          units => Future:
            try
              if oldHashes.get(step) != build.hashes.get(step) || !step.pkg.exists() ||
                  step.main.isDefined
              then
                Log.info(ansi"Compiling ${Green}[${step.name}]...")
                step.compile(build.hashes, oldHashes, build)
            catch
              case err: ExcessDataError => Log.info(t"Too much data")
              case err: StreamCutError  => Log.info(t"The stream was cut prematurely")
      .values
      
      Future.sequence(futures).await()
      Log.info(t"Compilation complete")
      // val tmpPath: Unix.IoPath = Ire.pwd / t".tmp"

      // build.linearization.foreach:
      //   step =>
      //     step.artifact.foreach:
      //       artifact =>
      //         val tmp = if !tmpPath.exists() then tmpPath.createDirectory() else tmpPath.directory
      //         Log.info(ansi"Building $Violet(${artifact.show}) artifact")
      //         step.classpath(build).filter(_.name.endsWith(t".jar")).foreach:
      //           jarFile =>
      //             try sh"unzip -o -d $tmp $jarFile".exec[Unit]()
      //             catch case err: Exception => err.printStackTrace()

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
              throw AppError(t"Publishing settings have not been specified in ire.json")

            val sonatype = Sonatype(publish.username, password, publish.group)
            Log.info(t"Using Sonatype settings ${sonatype.toString}")
            val profileId = sonatype.profile()
            val repoId = sonatype.start(profileId)
                
            steps.foreach:
              step =>
                Log.info(t"Generating POM file for ${step.id}")
                
                val pomXml = Pom(build, step, 2021,
                    t"https://propensive.com/opensource/${step.projectId}",
                    t"github.com/${step.projectId}", publish).xml
                
                pomXml.show.bytes.writeTo(step.pomFile.createFile())
                
                val srcFiles: List[Text] = step.sources.to(List).flatMap:
                  dir =>
                    Log.info(t"Adding source dir $dir")
                    dir.path.descendantFiles(!_.name.startsWith(t".")).filter:
                      file => file.name.endsWith(t".scala") || file.name.endsWith(t".java")
                    .flatMap:
                      file => List(t"-C", dir.show, dir.path.relativeTo(file.path).show)
                
                sh"jar cf ${step.srcsPkg} $srcFiles".exec[ExitStatus]()
                
                val docFiles: List[Text] = step.docs.to(List).flatMap:
                  dir =>
                    Log.info(t"Adding doc dir $dir")
                    dir.descendantFiles(!_.name.startsWith(t".")).flatMap:
                      file => List(t"-C", dir.show, dir.relativeTo(file.path).show)
                
                sh"jar cf ${step.docFile} $docFiles".exec[ExitStatus]()
    
                List(step.docFile, step.pomFile, step.pkg, step.srcsPkg).foreach:
                  file => sh"gpg -ab $file".exec[ExitStatus]()
    
                Log.info(t"Publishing ${step.id}")
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
        Ire.build(false, target, true, if waitForChange(dirs) then None else Some(build))

    catch
      case BrokenLinkError(ref) =>
        Log.fail(t"The reference to $ref could not be resolved")
        if watch then
          try
            val path = Ire.pwd / (target.getOrElse(t"build")+t".ire")
            val dirs = readImports(Map(), path.file).map(_.parent).to(List)
            waitForChange(dirs)
            Ire.build(false, target, true, None)
          catch case err: Exception => Log.fail(err.toString.show)
      case err: IoError =>
        Log.fail(err.toString.show)
        if watch then
          try
            val path = Ire.pwd / (target.getOrElse(t"build")+t".ire")
            val dirs = readImports(Map(), path.file).map(_.parent).to(List)
            waitForChange(dirs)
            Ire.build(false, target, true, None)
          catch case err: Exception => Log.fail(err.toString.show)
      case err: TtyError =>
        Log.fail(t"Could not capture TTY")
      case err: InotifyError =>
        Log.fail(t"The limit of the number if inotify instances has been exceeded")
        sys.exit(1)
      case err: StreamCutError =>
        Log.fail(t"The stream was cut")
        if watch then
          try
            val path = Ire.pwd / (target.getOrElse(t"build")+t".ire")
            val dirs = readImports(Map(), path.file).map(_.parent).to(List)
            waitForChange(dirs)
            Ire.build(false, target, true, None)
          catch case err: Exception => Log.fail(err.toString.show)
      case err: ExcessDataError =>
        Log.fail(t"Too much data")
        if watch then
          try
            val path = Ire.pwd / (target.getOrElse(t"build")+t".ire")
            val dirs = readImports(Map(), path.file).map(_.parent).to(List)
            waitForChange(dirs)
            Ire.build(false, target, true, None)
          catch case err: Exception => Log.fail(err.toString.show)
      case err: BuildfileError =>
        Log.fail(t"There was an error with the build file")
        if watch then
          try
            val path = Ire.pwd / (target.getOrElse(t"build")+t".ire")
            val dirs = readImports(Map(), path.file).map(_.parent).to(List)
            waitForChange(dirs)
            Ire.build(false, target, true, None)
          catch case err: Exception => Log.fail(err.toString.show)

  
  def waitForChange(dirs: List[Unix.Directory]): Boolean throws InotifyError | IoError =
    Log.info(t"Watching ${dirs.size} directories for changes")
    val watchers = dirs.map(_.watch())
    val stream = LazyList.multiplex(watchers.map(_.stream)*)
    if stream.isEmpty then sys.exit(0)
    else
      watchers.foreach(_.stop())
      stream.head match
        case Unix.FileEvent.Modify(file) =>
          Log.info(ansi"The file $Violet(${file.path.show}) was modified")
          file.path.name.endsWith(t".ire")
        
        case Unix.FileEvent.Delete(path) =>
          Log.info(ansi"The file $Violet(${path.show}) was deleted")
          path.name.endsWith(t".ire")
        
        case Unix.FileEvent.NewFile(file) =>
          Log.info(ansi"The file $Violet(${file.path.show}) was created")
          file.path.name.endsWith(t".ire")
        
        case Unix.FileEvent.NewDirectory(dir) =>
          Log.info(ansi"The directory $Violet(${dir.path.show}) was created")
          false

  private def abort(message: Text): Nothing throws AppError = throw AppError(message)

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
           : Project throws AppError =
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
given realm: Realm = Realm(t"ire")

