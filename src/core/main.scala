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
import tarantula.*
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
  
  def githubActions: Boolean = Option(System.getenv("GITHUB_ACTIONS")).isDefined

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
      val hash = ref.digest[Crc32].encode[Base256]
      val dest = libDir / t"${ref.digest[Crc32].encode[Hex].lower}.jar"
      if dest.exists() then Future:
        try dest.file(Expect) catch case err: IoError =>
          // FIXME: This exception is thrown inside a Future
          throw AppError(t"Could not access the dependency JAR, $ref", err)
      else Future:
        val verb = Verb.Download(ansi"${colors.CornflowerBlue}($ref)")
        funnel.foreach(_.put(Progress.Update.Add(verb, (hash))))
        try
          libDir.directory()
          val file = dest.file(Create)
          Uri(ref).writeTo(file)
          funnel.foreach(_.put(Progress.Update.Remove(verb, true)))
          file
        catch
          case err: StreamCutError =>
            funnel.foreach(_.put(Progress.Update.Remove(verb, false)))
            throw AppError(t"Could not download the file $ref", err)
          
          case err: IoError =>
            funnel.foreach(_.put(Progress.Update.Remove(verb, false)))
            throw AppError(t"The downloaded file could not be written to ${dest.fullname}", err)
        
    else
      Future:
        try Unix.parse(ref).get.file(Expect) catch case err: IoError =>
          throw AppError(t"Could not access the dependency JAR, $ref", err)

  def main(using cli: CommandLine): ExitStatus = try
    val buffer = StreamBuffer[Bytes throws StreamCutError]()
    given Stdout = Stdout(buffer)
    Task(cli.stdout(buffer.stream))()

    cli.args match
      case t"about" :: _        => Irk.about()
      case t"help" :: _         => Irk.help()
      case t"init" :: name :: _ => Irk.init(cli.pwd, name)
      case t"version" :: _      => Irk.showVersion()
      case t"build" :: params   =>
        val target = params.headOption.filter(!_.startsWith(t"-"))
        val watch = params.contains(t"-w") || params.contains(t"--watch")
        val webdev = params.contains(t"-b") || params.contains(t"--browser")
        Irk.build(target, false, watch, cli.pwd, cli.env, cli.script, webdev, buffer, cli.kills)
      case t"stop" :: params    => Irk.stop(cli)
      case params               =>
        val target = params.headOption.filter(!_.startsWith(t"-"))
        val watch = params.contains(t"-w") || params.contains(t"--watch")
        val webdev = params.contains(t"-b") || params.contains(t"--browser")
        Irk.build(target, false, watch, cli.pwd, cli.env, cli.script, webdev, buffer, cli.kills)
  
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
            Out.println(ansi"Reading build file ${palette.File}(${path.relativeTo(build.pwd.path).get.show})".render)
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

  private val snip = t" — "*100

  private def reportMessages(messages: List[irk.Message]): Text =
    val buf = StringBuilder("\n")
    def append(text: AnsiText): Unit = buf.append(text.render)
    
    def appendln(text: AnsiText): Unit =
      buf.append(text.render)
      buf.append('\n')
    
    messages.groupBy(_.path).foreach:
      case (path, messages) =>
        val syntax = ScalaSyntax.highlight(String(messages.head.content.unsafeMutable).show)
        messages.sortBy(-_.startLine).groupBy(_.startLine).foreach:
          case (ln, messages) => messages.last match
            case irk.Message(module, path, startLine, from, to, endLine, message, _) =>
              val bg = Bg(Srgb(0.16, 0.06, 0.03))
              val margin = (endLine + 2).show.length
              val codeWidth = 118 - margin
              
              append(ansi"${colors.Black}(${Bg(colors.Purple)}( $module ))")
              val pos = t"$path:${startLine + 1}:$from"
              append(ansi"${colors.Purple}(${Bg(colors.Crimson)}( ${colors.Black}($pos) ))")
              appendln(ansi"$bg${colors.Crimson}()$bg")
              
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
                .join.take(118 - margin)
              
              if startLine > 1 then
                append(ansi"${colors.Orange}(${startLine.show.pad(margin, Rtl)})")
                append(ansi"${colors.DarkSlateGray}(║)$bg ")
                val code = format(startLine - 1)
                appendln(ansi"$code${t" "*(codeWidth - code.length)}")
              
              (startLine to endLine).foreach:
                lineNo =>
                  if lineNo < (startLine + 2) || lineNo > (endLine - 2) then
                    val code = format(lineNo)
                    val before = if lineNo == startLine then code.take(from) else ansi""
                    val after = if lineNo == endLine then code.drop(to) else ansi""
                    
                    val highlighted =
                      if lineNo == startLine then
                        if lineNo == endLine then
                          if from == to then ansi"❰❰❰" else code.slice(from, to)
                        else code.drop(from)
                      else if lineNo == endLine then code.take(to) else code
  
                    append(ansi"${colors.Orange}($Bold(${(lineNo + 1).show.pad(margin, Rtl)}))")
                    append(ansi"${colors.DarkSlateGray}(║)$bg ")
                    append(before)
                    append(ansi"${colors.OrangeRed}(${highlighted.plain})")
                    val width = highlighted.length + before.length + after.length
                    appendln(ansi"$after${t" "*(codeWidth - width)}")
                  else if lineNo == startLine + 2
                  then
                    val code = format(startLine + 1)
                    val break = snip.take(codeWidth)
                    append(ansi"${t"".pad(margin, Rtl)}${colors.DarkSlateGray}(╫)$bg ")
                    appendln(ansi"${colors.RebeccaPurple}($break)")
              
              if endLine + 1 < syntax.length
              then
                append(ansi"${colors.Orange}(${(endLine + 2).show.pad(margin, Rtl)})")
                append(ansi"${colors.DarkSlateGray}(║)$bg ")
                val code = format(endLine + 1)
                appendln(ansi"$code${t" "*(codeWidth - code.length)}${escapes.Reset}")
              
              if startLine == endLine
              then
                append(ansi"${colors.DarkSlateGray}(${t" "*margin}╟${t"─"*from}┴${t"─"*(to - from)}┘)")
                appendln(ansi"${t"\e[K"}")
              else appendln(ansi"${t" "*margin}${colors.DarkSlateGray}(║)")
              
              message.cut(t"\n").foreach:
                line =>
                  appendln(ansi"${t" "*margin}${colors.DarkSlateGray}(║) ${Bold}($line)")
              
              appendln(ansi"${t" "*margin}${colors.DarkSlateGray}(╨)")
              appendln(ansi"${escapes.Reset}")
    
    buf.text

  case class Changes(build: Boolean = false, sources: Boolean = false, resources: Boolean = false):
    def changed: Boolean = build || sources || resources
    
    def apply(path: DiskPath): Changes = 
      if path.name.endsWith(t".irk") then copy(build = true) else copy(sources = true)
    
  def build(target: Option[Text], publishSonatype: Boolean, watch: Boolean = false, pwd: Directory,
                env: Map[Text, Text], scriptFile: File, webdev: Boolean,
                buffer: StreamBuffer[Bytes throws StreamCutError], kills: LazyList[Unit])
           (using Stdout)
           : ExitStatus throws AppError =
    val rootBuild = pwd / t"build.irk"
    val tap: Tap = Tap(true)

    lazy val watcher: Unix.Watcher = try  
      val watcher = Unix.watch(Nil)
      
      if watch then
        val dirs = readImports(Map(), rootBuild.file(Expect)).map(_.parent).to(Set)
        dirs.sift[Unix.Directory].foreach(watcher.add(_))
      
      watcher
    catch
      case err: InotifyError =>
        throw AppError(t"Could not watch directories", err)
      
      case err: IoError =>
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
            changed => Out.println(ansi"The file ${palette.File}(${event.path.relativeTo(pwd.path)}) was $changed".render)
          
          Changes(event.path.fullname.endsWith(t".irk"), event.path.fullname.endsWith(t".scala"))

    def updateWatches(watcher: => Unix.Watcher, build: Build): Build = try
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
    def recur(stream: LazyList[Changes], oldBuild: Option[Build], success: Boolean,
                  browser: List[WebDriver#Session], running: List[Subprocess], count: Int): ExitStatus =
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
                  Out.println(err.message)
                  None
                case err: IoError =>
                  Out.println(ansi"The build file could not be read".render)
                  None
            else oldBuild
          
          val (succeeded: Boolean, subprocesses: List[Subprocess]) = newBuild.fold(false):
            build =>
              import unsafeExceptions.canThrowAny
              val oldHashes = build.cache
              Out.println(t"")
              Out.print(ansi"${colors.Black}(${Bg(colors.YellowGreen)}( Build #${count + 1} ))".render)
              Out.print(ansi"${colors.YellowGreen}(${Bg(colors.Gold)}( ${colors.Black}(${build.pwd.path}) ))".render)
              Out.println(ansi"${colors.Gold}()".render)
              
              val funnel = Funnel[Progress.Update]()
              val totalTasks = build.steps.size
              
              val futures = build.graph.traversal[Future[Set[irk.Message]]]:
                (set, step) => Future.sequence(set).flatMap:
                  results =>
                    Future.sequence:
                      step.jars.map:
                        download => Irk.fetchFile(download, Some(funnel))
                    .flatMap:
                      downloads => Future:
                        val verb = Verb.Compile(ansi"${Green}(${step.name})")
                        
                        blocking:
                          val messages = results.flatten
                          
                          if messages.isEmpty then
                            val hash = build.hashes(step)
                            try
                              if oldHashes.get(step) != build.hashes.get(step)
                              then
                                funnel.put(Progress.Update.Add(verb, hash))
                                val output = step.compile(build.hashes, oldHashes, build, scriptFile)
                                funnel.put(Progress.Update.Remove(verb, output.isEmpty))
                                messages ++ output
                              else
                                funnel.put(Progress.Update.SkipOne)
                                messages
                            catch
                              case err: ExcessDataError =>
                                funnel.put(Progress.Update.Remove(verb, false))
                                messages + irk.Message(step.id, t"<unknown>", 0, 0, 0, 1,
                                    t"too much data was received", IArray())
              
                              case err: StreamCutError =>
                                funnel.put(Progress.Update.Remove(verb, false))
                                messages + irk.Message(step.id, t"<unknown>", 0, 0, 0, 1,
                                    t"the stream was cut prematurely", IArray())
                          else
                            messages
              .values

              val pulsar = Pulsar(using timekeeping.long)(100)
              
              val ui = Task:
                funnel.stream.multiplexWith:
                  pulsar.stream.map { _ => Progress.Update.Print }
                .foldLeft(Progress(TreeMap(), Nil, totalTasks = totalTasks))(_(_, buffer))
              
              val uiReady = ui()
              val messages = Future.sequence(futures).await().to(Set).flatten.to(List)
              val success = messages.isEmpty
              
              val subprocesses: List[Subprocess] =
                if success then
                  build.linearization.map:
                    step =>
                      val subprocess: Option[Subprocess] = step.webdev.fold[Option[Subprocess]](None):
                        case WebDev(browsers, url, start, stop) =>
                          val mainTask = Task:
                            val hash = t"${build.hashes.get(step)}${start}".digest[Crc32].encode[Base256]
                            val verb = Verb.Exec(ansi"main class ${palette.Class}($start)")
                            funnel.put(Progress.Update.Add(verb, hash))
                            funnel.put(Progress.Update.Switch(false))
                            running.foreach(_.stop())
                            val resources = step.allResources(build).map(_.path)
                            val result = Irk.synchronized:
                              buffer.useSecondary()
                              val oldOut = System.out.nn
                              val out = java.io.PrintStream(new java.io.OutputStream:
                                override def write(byte: Int): Unit =
                                  buffer.putSecondary(IArray(byte.toByte))
                                
                                override def write(bytes: Array[Byte], off: Int, len: Int): Unit =
                                  buffer.putSecondary(bytes.unsafeImmutable.slice(off, off + len))
                              )
                              System.setOut(out)
                              val classpath = (step.classpath(build) ++ resources).to(List)
                              val result = Run.main(classpath)(start, stop)
                              System.setOut(oldOut)
                              buffer.usePrimary()
                              result
                            funnel.put(Progress.Update.Switch(true))
                            funnel.put(Progress.Update.Remove(verb, true))
                            result
                          
                          val result = Some(mainTask().future.await())
                          browser.foreach(_.navigateTo(Url.parse(url)))
                          result
 
                      step.artifact.foreach:
                        artifact =>
                          val verb = Verb.Build(ansi"artifact ${palette.File}(${artifact.path.relativeTo(pwd.path).show})")
                          val hash = t"${build.hashes.get(step)}".digest[Crc32].encode[Base256]
                          funnel.put(Progress.Update.Add(verb, hash))
                          
                          Artifact.build(artifact, irkJar(scriptFile), step.name, step.version,
                              step.classpath(build).to(List), step.allResources(build).to(List))
                          
                          funnel.put(Progress.Update.Remove(verb, true))
                          
                      if publishSonatype then Sonatype.publish(build, env.get(t"SONATYPE_PASSWORD"))
                      
                      subprocess
                  .flatten
                    
                else Nil
              
              pulsar.stop()
              funnel.stop()
              uiReady.future.await()
              Out.print(reportMessages(messages))
              
              if messages.nonEmpty then Out.println(ansi"Build ${colors.OrangeRed}(failed) with ${colors.Gold}(${messages.size}) issue${if messages.size == 1 then t"" else t"s"}".render)
              else Out.println(ansi"Build #${count + 1} ${colors.Green}(succeeded)".render)
              
              Out.println(t"\e[0m\e[?25h\e[A")
              
              (success, subprocesses)

          buffer.usePrimary()
          if watch then
            Out.print(Progress.titleText(t"Irk: waiting for changes"))
            Out.println(ansi"${t"\n"}Watching ${colors.Gold}(${watcher.directories.size}) directories for changes...".render)
          tap.open()
          recur(stream.tail, newBuild, succeeded, browser, subprocesses, count + 1)
      
    def buildStream: LazyList[Changes] =
      if !watch then LazyList()
      else watcher.stream.regulate(tap).cluster(100).map(triggers).filter(_.changed)
    
    if webdev then Chrome.session(8869):
      recur(Changes(true, true, true) #:: buildStream, None, false, List(browser), Nil, 0)
    else recur(Changes(true, true, true) #:: buildStream, None, false, Nil, Nil, 0)

case class ExecError(err: Exception) extends Error:
  def message: Text = t"An exception was thrown while executing the task"

case class TrapExitError(status: ExitStatus) extends Error:
  def message: Text = t"A call to System.exit was trapped: ${status}"

object Run:
  import java.net.*

  def main(classpath: List[DiskPath])(start: Text, stop: Option[Text])(using Stdout): Subprocess =
    val urls = classpath.map:
      path =>
        if path.isDirectory then URL(t"file://${path.fullname}/".s)
        else URL(t"file://${path.fullname}/".s)

    val loader = URLClassLoader(urls.to(Array)).nn
    
    def invoke(className: Text): Any =
      Run.synchronized:
        if System.getSecurityManager != TrapExit then System.setSecurityManager(TrapExit)

      val cls: Class[?] = loader.loadClass(className.s).nn
      val method = cls.getMethods.nn.find(_.nn.getName == "main").get.nn
      method.invoke(null, Array[String]())
    
    val subprocess = try
      invoke(start)
      Subprocess(None)
    catch
      case err: TrapExitError => Subprocess(Some(err.status))
      case err: Exception     => Subprocess(Some(ExitStatus(1)))
    
    val term = stop.map { stop => () => try invoke(stop).unit catch case err: Exception => () }
    subprocess.copy(terminate = term)

object TrapExit extends SecurityManager:
  private var disabled: Boolean = false
  override def checkPermission(perm: java.security.Permission): Unit = ()
  
  override def checkExit(status: Int): Unit =
    erased given CanThrow[TrapExitError] = compiletime.erasedValue
    super.checkExit(status)
    if !disabled then throw TrapExitError(ExitStatus(status))
  
  def terminate(status: ExitStatus): Unit =
    disabled = true
    System.exit(status())

case class Subprocess(status: Option[ExitStatus], terminate: Option[() => Unit] = None):
  def stop(): Unit = terminate.foreach(_())

object Verb:
  given Ordering[Verb] = Ordering[AnsiText].on(_.name)

enum Verb:
  def name: AnsiText

  case Build(name: AnsiText)
  case Exec(name: AnsiText)
  case Compile(name: AnsiText)
  case Download(name: AnsiText)

  def present: AnsiText = this match
    case Build(name)    => ansi"Building $name"
    case Exec(name)     => ansi"Executing $name"
    case Compile(name)  => ansi"Compiling $name"
    case Download(name) => ansi"Downloading $name"
  
  def past: AnsiText = this match
    case Build(name)    => ansi"Built $name"
    case Exec(name)     => ansi"Finished execution of $name"
    case Compile(name)  => ansi"Compiled $name"
    case Download(name) => ansi"Downloaded $name"
