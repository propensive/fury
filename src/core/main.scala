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

import timekeeping.long
import encodings.Utf8

import rendering.ansi

object Irk extends Daemon():

  def version: Text =
    Option(getClass.nn.getPackage.nn.getImplementationVersion).fold(t"0")(_.nn.show)
  
  def javaVersion: Text = try Sys.java.version() catch case err: KeyNotFoundError => t"unknown"
  
  def githubActions: Boolean = Option(System.getenv("GITHUB_ACTIONS")).isEmpty

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

  def fetchFile(ref: Text, funnel: Option[Funnel[Progress.Update]])(using Stdout): Future[File] =
    val libDir = cacheDir / t"lib"
    if ref.startsWith(t"https:") then
      val dest = libDir / t"${ref.digest[Crc32].encode[Hex].lower}.jar"
      if dest.exists() then Future:
        try dest.file(Expect) catch case err: IoError =>
          // FIXME: This exception is thrown inside a Future
          throw AppError(t"Could not access the dependency JAR, $ref", err)
      else Future:
        val name = ansi"Downloading ${colors.CornflowerBlue}($ref)"
        funnel.foreach(_.put(Progress.Update.Add(name)))
        try
          libDir.directory()
          val file = dest.file(Create)
          Uri(ref).writeTo(file)
          funnel.foreach(_.put(Progress.Update.Remove(true, name)))
          file
        catch
          case err: StreamCutError =>
            funnel.foreach(_.put(Progress.Update.Remove(false, name)))
            throw AppError(t"Could not download the file $ref", err)
          
          case err: IoError =>
            funnel.foreach(_.put(Progress.Update.Remove(false, name)))
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
      case t"build" :: params   =>
        val target = params.headOption.filter(!_.startsWith(t"-"))
        Irk.build(target, false, params.contains(t"-w") || params.contains(t"--watch"), cli.pwd, cli.env, cli.script)
      case t"stop" :: params    => Irk.stop(cli)
      case params               =>
        val target = params.headOption.filter(!_.startsWith(t"-"))
        Irk.build(target, false, params.contains(t"-w") || params.contains(t"--watch"), cli.pwd, cli.env, cli.script)
  
  catch
    case err: Throwable =>
      Out.println(StackTrace(err).ansi)
      ExitStatus.Fail(1)
  
  private lazy val fileHashes: FileCache[Bytes] = new FileCache()

  private def initBuild(pwd: Directory)(using Stdout): Build throws IoError | BuildfileError =
    val path = pwd / t"build.irk"
    readBuilds(Build(pwd, Map(), None, Map(), Map()), Set(), path)
  
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
                : Build throws BuildfileError | IoError =
    try
      files.to(List) match
        case Nil =>
          build
        
        case path :: tail =>
          def digest: Text = hashFile(path.file()).encode[Base64]
          if path.exists() && seen.contains(digest) then readBuilds(build, seen, tail*)
          else if path.exists() then
            Out.println(ansi"Reading build file ${palette.File}(${path.relativeTo(build.pwd.path).get.show})")
            val buildConfig = Json.parse(path.file().read[Text](1.mb)).as[BuildConfig]
            buildConfig.gen(build, seen + digest, files*)
          else throw AppError(txt"""Build contains an import reference to a nonexistant build""")
            
    catch
      case err: IoError => err match
        case IoError(op, reason, path) =>
          throw AppError(t"There was an I/O error", err)
      
      case err: ExcessDataError =>
        throw BuildfileError(t"The build file was larger than 1MB")
      
      case err: StreamCutError =>
        throw AppError(t"The configuration file could not be read completely", err)
      
      case err: JsonParseError =>
        throw BuildfileError(err.message)
      
      case err: JsonAccessError =>
        throw BuildfileError(err.message)
      
      case err: AppError =>
        throw err

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

      val config = BuildConfig(None, None, List(module), None, None)
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

  private def reportMessages(messages: List[irk.Message])(using Stdout): Unit =
    messages.groupBy(_.path).foreach:
      case (path, messages) =>
        val syntax = ScalaSyntax.highlight(String(messages.head.content.unsafeMutable).show)
        messages.sortBy(-_.line).groupBy(_.line).foreach:
          case (line, messages) => messages.last match
            case irk.Message(module, path, line, from, to, message, _) =>
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
                case m@irk.Message(module, path, line, from, to, message, _) =>
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

  case class Changes(build: Boolean = false, sources: Boolean = false, resources: Boolean = false):
    def changed: Boolean = build || sources || resources
    
    def apply(path: DiskPath): Changes = 
      if path.name.endsWith(t".irk") then copy(build = true) else copy(sources = true)
    
  def build(target: Option[Text], publishSonatype: Boolean, watch: Boolean = false, pwd: Directory,
                env: Map[Text, Text], scriptFile: File)
           (using Stdout)
           : ExitStatus throws AppError =
    val rootBuild = pwd / t"build.irk"
    val tap: Tap = Tap(true)

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
            changed => Out.println(ansi"The file ${palette.File}(${event.path.relativeTo(pwd.path)}) was $changed")
          
          Changes(event.path.fullname.endsWith(t".irk"), event.path.fullname.endsWith(t".scala"))


    def updateWatches(watcher: Unix.Watcher, build: Build): Build = try
      val buildDirs = readImports(Map(), rootBuild.file(Expect)).map(_.parent).to(Set)
      val dirs = (buildDirs ++ build.sourceDirs ++ build.resourceDirs).sift[Unix.Directory]
      val additions = dirs -- watcher.directories
      
      additions.foreach:
        dir => watcher.add(dir)
          
      (watcher.directories -- dirs).foreach:
        dir => watcher.remove(dir)
      
      build
    catch
      case err: InotifyError => throw AppError(t"Could not update watch directories", err)
      case err: IoError      => throw AppError(t"Could not update watch directories", err)

    @tailrec
    def recur(stream: LazyList[Changes], oldBuild: Option[Build], success: Boolean): ExitStatus =
      if stream.isEmpty then
        if success then ExitStatus.Ok else ExitStatus.Fail(1)
      else stream.head match
        case changes@Changes(_, _, _) =>
          tap.close()
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
              Out.println(ansi"Starting build")
              val funnel = Funnel[Progress.Update]()
              //val graph = build.target.fold(build.graph)(build.graph.descendants(_))
              val futures = build.graph.traversal[Future[Set[irk.Message]]]:
                (set, step) => Future.sequence(set).flatMap:
                  results =>
                    Future.sequence:
                      step.jars.map:
                        download =>
                          Irk.fetchFile(download, Some(funnel))
                    .flatMap:
                      downloads => Future:
                        val name = ansi"Compiling ${Green}(${step.name})"
                        blocking:
                          val messages = results.flatten
                          if messages.isEmpty then
                            try
                              if oldHashes.get(step) != build.hashes.get(step) || step.main.isDefined
                              then
                                //Out.println(ansi"Compiling ${Green}[${step.name}]...")
                                funnel.put(Progress.Update.Add(name))
                                val output = step.compile(build.hashes, oldHashes, build, scriptFile)
                                funnel.put(Progress.Update.Remove(output.isEmpty, name))
                                messages ++ output
                              else messages
                            catch
                              case err: ExcessDataError =>
                                funnel.put(Progress.Update.Remove(false, name))
                                messages + irk.Message(step.id, t"<unknown>", 0, 0, 0,
                                    t"too much data was received", IArray())
              
                              case err: StreamCutError =>
                                funnel.put(Progress.Update.Remove(false, name))
                                messages + irk.Message(step.id, t"<unknown>", 0, 0, 0,
                                    t"the stream was cut prematurely", IArray())
                          else
                            //funnel.put(Progress.Update.Remove(false, name))
                            messages
              .values

              val pulsar = Pulsar(100)
              
              val task = Task:
                funnel.stream.multiplexWith:
                  pulsar.stream.map { _ => Progress.Update.Print }
                .foldLeft(Progress(TreeMap(), Nil))(_(_))
              
              val promise = task()
              val messages: List[irk.Message] = Future.sequence(futures).await().to(Set).flatten.to(List)
              val success = messages.isEmpty
              reportMessages(messages)
              
              // FIXME: Files should be sorted by last-modified
              if success then
                build.linearization.foreach:
                  step => step.artifact.foreach:
                    artifact =>
                      val t0 = System.currentTimeMillis
                      val name = ansi"Building artifact ${palette.File}(${artifact.relativeTo(pwd.path).show})"
                      funnel.put(Progress.Update.Add(name))
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
                      funnel.put(Progress.Update.Remove(true, name))
                      //val time = System.currentTimeMillis - t0
                      //Out.println(ansi"Built ${palette.File}(${artifact.show}) in ${palette.Number}[${time}ms]")
                      
                if publishSonatype then Sonatype.publish(build, env.get(t"SONATYPE_PASSWORD"))

                // if run then
                //   if parallel then
                //     val task = Task(exec())
                //     task.start()
                //   else exec()
                
                // browser.reload()
          
              pulsar.stop()
              funnel.stop()
              tap.open()
              promise.future.await()
              Out.println(t"\e[0m\e[?25h\e[A")
              success

          if watch then Out.println(t"Watching ${watcher.directories.size} directories for changes...")
          recur(stream.tail, newBuild, succeeded)
      
    def buildStream: LazyList[Changes] =
      if !watch then LazyList()
      else watcher.stream.regulate(tap).cluster(100).map(triggers).filter(_.changed)
    
    recur(Changes(true, true, true) #:: buildStream, None, false)