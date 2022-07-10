package irk

import gossamer.*
import rudiments.*
import turbulence.*
import acyclicity.*
import euphemism.*
import joviality.*, DiskPath.provider, Directory.provider, File.provider, filesystems.unix
import guillotine.*
import kaleidoscope.*
import escapade.*
import gastronomy.*
import harlequin.*
import iridescence.*, solarized.*
import serpentine.*
import exoskeleton.*
import imperial.*
import tarantula.*
import profanity.*
import xylophone.*
import telekinesis.*
import anticipation.*
import escritoire.*
import surveillance.*

import timekeeping.long
import encodings.Utf8
import rendering.ansi

import scala.collection.mutable as scm
import scala.concurrent.*
import scala.util.chaining.scalaUtilChainingOps

import java.nio.BufferOverflowException

given Environment = environments.system

object Irk extends Daemon():
  def version: Text =
    Option(getClass.nn.getPackage.nn.getImplementationVersion).fold(t"0")(_.nn.show)
  
  def javaVersion: Text = try Sys.java.version() catch case err: KeyNotFoundError => t"unknown"
  def githubActions: Boolean = Option(System.getenv("GITHUB_ACTIONS")).isDefined

  def scalaVersion: Text =
    val props = java.util.Properties()
    props.load(getClass.nn.getClassLoader.nn.getResourceAsStream("compiler.properties").nn)
    props.get("version.number").toString.show
 
  def homeDir: Directory[Unix] = unsafely(Home().directory(Expect))
      
  def cacheDir: Directory[Unix] =
    try (Home.Cache() / p"irk").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's cache directory could not be created", err)

  def libDir: Directory[Unix] =
    try (cacheDir / p"lib").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's cache directory could not be created", err)

  def tmpDir: Directory[Unix] =
    try (cacheDir / p"tmp").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's temporary directory could not be created", err)
  
  def hashesDir: Directory[Unix] =
    try (cacheDir / p"hashes").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's cache directory could not be created", err)

  private val prefixes = Set(t"/scala", t"/dotty", t"/compiler.properties", t"/org/scalajs", t"/com",
      t"/incrementalcompiler.version.properties", t"/library.properties", t"/NOTICE")

  private lazy val scriptSize: Int =
    import unsafeExceptions.canThrowAny
    unsafely(Classpath() / t"exoskeleton" / t"invoke").resource.read[Bytes](100.kb).size

  def irkJar(scriptFile: File[Unix])(using Stdout): File[Unix] throws StreamCutError = synchronized:
    import java.nio.file.*
    val jarPath = unsafely(libDir.path / t"base-$version.jar")

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
          catch case err: Exception => () //Out.println(t"Got a NPE on ${file.nn.toString.show}")
      
      fs.close()
      
      jarPath.file(Expect)
    catch case err: IoError =>
      throw AppError(t"The Irk binary could not be copied to the user's cache directory")

  def fetchFile(ref: Text, funnel: Option[Funnel[Progress.Update]])(using Stdout): Future[File[Unix]] =
    val libDir = unsafely(cacheDir / t"lib")
    if ref.startsWith(t"https:") then
      val hash = ref.digest[Crc32].encode[Base256]
      val dest = unsafely(libDir / t"${ref.digest[Crc32].encode[Hex].lower}.jar")
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
          funnel.foreach(_.put(Progress.Update.Remove(verb, Result.Complete(Nil))))
          file
        catch
          case err: StreamCutError =>
            funnel.foreach(_.put(Progress.Update.Remove(verb, Result.Terminal(ansi"A stream error occurred"))))
            throw AppError(t"Could not download the file $ref", err)
          
          case err: IoError =>
            funnel.foreach(_.put(Progress.Update.Remove(verb, Result.Terminal(ansi"An I/O error occurred"))))
            throw AppError(t"The downloaded file could not be written to ${dest.fullname}", err)
        
    else
      Future:
        try Unix.parse(ref).file(Expect) catch
          case err: InvalidPathError =>
            throw AppError(t"Could not access the dependency JAR, $ref", err)
          case err: IoError =>
            throw AppError(t"Could not access the dependency JAR, $ref", err)

  def main(using CommandLine): ExitStatus =
    Sys.scala.concurrent.context.maxExtraThreads() = t"800"
    Sys.scala.concurrent.context.maxThreads() = t"1000"
    Sys.scala.concurrent.context.minThreads() = t"100"
  
    try
      cli.args match
        case t"about" :: _        => Irk.about()
        case t"help" :: _         => Irk.help()
        case t"init" :: name :: _ => Irk.init(cli.pwd, name)
        case t"version" :: _      => Irk.showVersion()
        case t"build" :: params   =>
          val target = params.headOption.filter(!_.startsWith(t"-"))
          val watch = params.contains(t"-w") || params.contains(t"--watch")
          val exec = params.contains(t"-b") || params.contains(t"--browser")
          Irk.build(target, false, watch, cli.pwd, cli.env, cli.script, exec)
        case t"stop" :: params    => Irk.stop(cli)
        case params               =>
          val target = params.headOption.filter(!_.startsWith(t"-"))
          val watch = params.contains(t"-w") || params.contains(t"--watch")
          val exec = params.contains(t"-b") || params.contains(t"--browser")
          Irk.build(target, false, watch, cli.pwd, cli.env, cli.script, exec)
  
    catch
      case error: AppError => error match
        case AppError(message, _) =>
          Out.println(message)
          ExitStatus.Fail(1)
      case err: Throwable =>
        try
          Out.println(StackTrace(err).ansi)
          ExitStatus.Fail(1)
        catch case err2: Throwable =>
          System.out.nn.println(err2.toString)
          err2.printStackTrace()
          System.out.nn.println("Caused by:")
          System.out.nn.println(err.toString)
          err.printStackTrace()
          ExitStatus.Fail(2)
  
  private lazy val fileHashes: FileCache[Bytes] = new FileCache()

  private def initBuild(pwd: Directory[Unix])(using Stdout): Build throws IoError | BuildfileError =
    val path = unsafely(pwd / t"build.irk")
    readBuilds(Build(pwd, Map(), None, Map(), Map()), Set(), path)
  
  def hashFile(file: File[Unix]): Bytes throws IoError | AppError =
    try fileHashes(file.path.show, file.modified):
      file.read[Bytes](1.mb).digest[Crc32].bytes
    catch
      case err: StreamCutError  => throw AppError(t"The stream was cut while hashing a file", err)
      case err: ExcessDataError => throw AppError(t"The file was too big to hash", err)
      case err: Error[?]        => throw AppError(t"An unexpected error occurred", err)
  
  def cloneRepo(path: DiskPath[Unix], url: Text): Unit =
    try sh"git clone -q $url ${path.fullname}".exec[Unit]()
    catch case err: ExecError => throw AppError(t"Could not run `git clone` for repository $url")

  def readBuilds(build: Build, seen: Set[Text], files: DiskPath[Unix]*)(using Stdout)
                : Build throws BuildfileError | IoError =
    try
      files.to(List) match
        case Nil =>
          build
        
        case path :: tail =>
          def digest: Text = hashFile(path.file()).encode[Base64]
          if path.exists() && seen.contains(digest) then readBuilds(build, seen, tail*)
          else if path.exists() then
            Out.println(ansi"Reading build file ${palette.File}(${path.relativeTo(build.pwd.path).show})")
            val buildConfig = locally:
              import unsafeExceptions.canThrowAny
              Json.parse(path.file().read[Text](1.mb)).as[BuildConfig]
            buildConfig.gen(path, build, seen + digest, files*)
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

  def readImports(seen: Map[Text, File[Unix]], files: File[Unix]*)(using Stdout): Set[File[Unix]] =
    case class Imports(repos: Option[List[Repo]], imports: Option[List[Text]]):
      def repoList: List[Repo] = repos.getOrElse(Nil)
      def gen(seen: Map[Text, File[Unix]], files: File[Unix]*): Set[File[Unix]] = files.to(List) match
        case file :: tail =>
          val importFiles: List[File[Unix]] = imports.getOrElse(Nil).flatMap: path =>
            val ref = unsafely(file.parent.path + Relative.parse(path))
            try
              if ref.exists() then
                List(unsafely(file.parent.path + Relative.parse(path)).file(Expect))
              else
                Out.println(t"Build file $ref does not exist; attempting to clone")
                if unsafely(ref.parent).exists()
                then throw AppError(t"The build ${ref.name} does not exist in ${unsafely(ref.parent)}")
                else
                  repoList.find(_.basePath(unsafely(file.parent).path) == unsafely(ref.parent)) match
                    case None =>
                      throw AppError(txt"""Could not find a remote repository containing the import $path""")
                    
                    case Some(repo) =>
                      Irk.cloneRepo(unsafely(ref.parent), repo.url)
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
          def interpret(digest: Option[Text]) =
            Json.parse(file.read[Text](1.mb)).as[Imports].gen(digest.fold(seen)(seen.updated(_, file)), files*)
          
          if file.exists() then
            val digest: Text = hashFile(file).encode[Base64]
            if !seen.contains(digest) then interpret(Some(digest)) else readImports(seen, tail*)
          else interpret(None)

        catch case err: Exception => readImports(seen, tail*)

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

  def init(pwd: Directory[Unix], name: Text)(using Stdout): ExitStatus =
    val buildPath = unsafely(pwd / t"build.irk")
    if buildPath.exists() then
      Out.println(t"Build file build.irk already exists")
      ExitStatus.Fail(1)
    else
      import unsafeExceptions.canThrowAny
      val buildFile = buildPath.file(Create)
      val src = (pwd / t"src").directory(Ensure)
      val sourceDir = (src / t"core").directory(Ensure)
      
      val module = Module(name, t"${pwd.path.name}/core", None, None,
          Set(sourceDir.path.relativeTo(pwd.path).show), None, None, None, None, None, None)

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

  private def report(result: Result, columns: Int): Text =
    val buf = StringBuilder("\n")
    def append(text: AnsiText): Unit = buf.append(text.render)
    
    def appendln(text: AnsiText): Unit =
      buf.append(text.render)
      buf.append('\n')
  
    val sorted =
      try
        result.issues.groupBy(_.baseDir).to(List).map: (baseDir, issues) =>
          issues.groupBy(_.path).to(List).map: (path, issues) =>
            unsafely(baseDir + path).file(Ensure) -> issues
          .sortBy(_(0).modified)
        .sortBy(_.last(0).modified).flatten
        
      catch case err: IoError => throw AppError(ansi"a file containing an error was deleted: ${err.toString.show}", err)//: ${err.ansi}", err)
    
    sorted.foreach:
      case (file, issues) =>
        val syntax = ScalaSyntax.highlight(issues.head.content.text)
        issues.groupBy(_.startLine).to(List).sortBy(_(0)).foreach:
          case (ln, issues) => issues.head match
            case Issue(level, module, baseDir, path, startLine, from, to, endLine, message, _) =>
              val bg = Bg(Srgb(0.16, 0.06, 0.03))
              val margin = (endLine + 2).show.length
              val codeWidth = columns - 2 - margin
              
              append(ansi"${colors.Black}(${Bg(colors.Purple)}( $module ))")
              val pos = t"$path:${startLine + 1}:$from"
              
              val shade = level match
                case Level.Error => colors.Crimson
                case Level.Warn  => colors.Orange
                case Level.Info  => colors.SteelBlue
              
              append(ansi"${colors.Purple}(${Bg(shade)}( ${colors.Black}($pos) ))")
              appendln(ansi"$bg$shade()$bg")
              
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
                  case Token.Newline           => throw Mistake("Should not have a newline")
                .join.take(columns - 2 - margin)
              
              if startLine > 1 then
                append(ansi"${colors.Orange}(${startLine.show.pad(margin, Rtl)})")
                append(ansi"${colors.DarkSlateGray}(║)$bg ")
                val code = format(startLine - 1)
                appendln(ansi"$code${t" "*(codeWidth - code.length)}")
              
              (startLine to endLine).foreach: lineNo =>
                if lineNo < (startLine + 2) || lineNo > (endLine - 2) then
                  val code = format(lineNo)
                  val before = if lineNo == startLine then code.take(from) else ansi""
                  
                  val after =
                    if lineNo == endLine then code.drop(if from == to then to + 4 else to) else ansi""
                  
                  val highlighted =
                    if lineNo == startLine then
                      if lineNo == endLine then
                        if from == to then ansi"▎❮❮❮" else code.slice(from, to)
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

  case class Change(changeType: ChangeType, path: DiskPath[Unix])
  enum ChangeType:
    case Build, Source, Resource

    def rebuild = this == Build
    def recompile = this != Resource

    def category: Text = this match
      case Build     => t"build"
      case Source    => t"source"
      case Resource  => t"resource"

  def build(target: Option[Text], publishSonatype: Boolean, watch: Boolean = false, pwd: Directory[Unix],
                env: Map[Text, Text], scriptFile: File[Unix], exec: Boolean)
           (using Stdout, InputSource)
           : ExitStatus throws AppError = unsafely:
    Tty.capture:
      val rootBuild = unsafely(pwd / t"build.irk")
      val tap: Tap = Tap(true)
      val abortFunnel: Funnel[Event] = Funnel()
      
      //Tty.reportSize()

      def interrupts = Tty.stream[Keypress].collect:
        case Keypress.Ctrl('C')        => Event.Interrupt
        case Keypress.Resize(width, _) => Event.Resize(width)
      .filter:
        case Event.Interrupt =>
          if !tap.state() then
            abortFunnel.put(Event.Interrupt)
            false
          else true
        
        case _ =>
          true

      lazy val watcher: Watcher[Directory[Unix]] = try  
        val watcher = List[Directory[Unix]]().watch()
        
        if watch then
          val dirs = readImports(Map(), rootBuild.file(Expect)).map(_.parent).to(Set)
          dirs.sift[Directory[Unix]].foreach(watcher.add(_))
        
        watcher
      catch
        case err: InotifyError => throw AppError(t"Could not watch directories", err)
        case err: IoError      => throw AppError(t"Could not watch directories", err)

      val suffixes = Set(t".scala", t".java", t".irk")
      
      def updateWatches(watcher: => Watcher[Directory[Unix]], build: Build): Build = try
        val buildDirs = readImports(Map(), rootBuild.file(Expect)).map(_.parent).to(Set)
        val dirs = (buildDirs ++ build.sourceDirs ++ build.resourceDirs).sift[Directory[Unix]]
        
        (dirs -- watcher.directories).foreach(watcher.add(_))
        (watcher.directories -- dirs).foreach(watcher.remove(_))
        
        build
      catch
        case err: InotifyError => throw AppError(t"Could not update watch directories", err)
        case err: IoError      => throw AppError(t"Could not update watch directories", err)

      def generateBuild(): Build =
        try if watch then updateWatches(watcher, initBuild(pwd)) else initBuild(pwd)
        catch
          case err: BuildfileError => throw AppError(err.message)
          case err: IoError => throw AppError(t"The build file could not be read")

      var cancel: Promise[Unit] = Promise()
      
      def abort(): Unit = cancel.synchronized:
        if !cancel.isCompleted then cancel.complete(util.Success(()))


      Task(abortFunnel.stream.foreach { _ => abort() })()

      @tailrec
      def loop(first: Boolean, stream: LazyList[Event], oldBuild: Build, lastSuccess: Boolean,
                        browser: List[WebDriver#Session], running: List[Subprocess] = Nil,
                        count: Int = 1, columns: Int = 120)
                   : ExitStatus =
        import unsafeExceptions.canThrowAny
        tap.open()
        val cancel: Promise[Unit] = Promise()

        if stream.isEmpty then
          if lastSuccess then ExitStatus.Ok else ExitStatus.Fail(1)
        else stream.head match
          case Event.Interrupt =>
            running.foreach(_.stop())
            if lastSuccess then ExitStatus.Ok else ExitStatus.Fail(1)
          case Event.Resize(width) =>
            loop(first, stream.tail, oldBuild, lastSuccess, browser, running, count, columns)
          case Event.Changeset(fileChanges) =>
            tap.pause()
            
            def ephemeral(events: List[WatchEvent]): Boolean = events match
              case WatchEvent.NewFile(_, _) +: _ :+ WatchEvent.Delete(_, _) => true
              case _                                                        => false
            
            val changes: List[Change] =
              fileChanges.groupBy(_.path).collect:
                case (path, es) if !ephemeral(es) => es.last
              .foldLeft[List[Change]](Nil): (changes, event) =>
                Option(event).collect:
                  case WatchEvent.Delete(_, _)  => t"deleted"
                  case WatchEvent.Modify(_, _)  => t"modified"
                  case WatchEvent.NewFile(_, _) => t"created"
                .foreach: change =>
                  val file = ansi"${palette.File}(${event.path.relativeTo(pwd.path)})"
                  Out.println(ansi"The file $file was $change")
                
                if event.path.name.endsWith(t".irk")
                then Change(ChangeType.Build, event.path) :: changes
                else if event.path.name.endsWith(t".scala")
                then Change(ChangeType.Source, event.path) :: changes
                else if oldBuild.resourceDirs.exists(_.path.precedes(event.path))
                then Change(ChangeType.Resource, event.path) :: changes
                else changes
            
            if changes.isEmpty && count > 1 then
              loop(false, stream.tail, oldBuild, lastSuccess, browser, running, count, columns)
            else
              changes.foreach:
                change => Out.println(ansi"Rebuild triggered by change to a ${change.changeType.category} file")
  
              val build: Build =
                if changes.exists(_.changeType.rebuild)
                then safely(generateBuild()).otherwise(oldBuild)
                else oldBuild
  
              val oldHashes = build.cache
              Out.println(t"")
              Out.print(ansi"${colors.Black}(${Bg(colors.DarkTurquoise)}( Build #$count ))")
              Out.print(ansi"${colors.DarkTurquoise}(${Bg(colors.DodgerBlue)}( ${colors.Black}(${build.pwd.path}) ))")
              Out.println(ansi"${colors.DodgerBlue}()")
              
              val funnel: Funnel[Progress.Update] = Funnel()
              val totalTasks: Int = build.steps.size
              
              val futures = build.graph.traversal[Future[Result]]: (set, step) =>
                Future.sequence(set).flatMap: results =>
                  Future.sequence:
                    step.jars.map(Irk.fetchFile(_, Some(funnel)))
                  .flatMap: downloads =>
                    if cancel.isCompleted then Future(Result.Aborted) else Future:
                      val verb = Verb.Compile(ansi"${Green}(${step.name})")
                      
                      blocking:
                        val result: Result = results.foldLeft(Result.Complete(Nil))(_ + _)
                        
                        if result.success then
                          val hash = build.hashes(step)
                          try
                            if oldHashes.get(step) != build.hashes.get(step)
                            then
                              funnel.put(Progress.Update.Add(verb, hash))
                              val newResult: Result = step.compile(build.hashes, oldHashes, build, scriptFile, cancel)
                              funnel.put(Progress.Update.Remove(verb, if cancel.isCompleted then Result.Aborted else newResult))
                              result + newResult
                            else
                              funnel.put(Progress.Update.SkipOne)
                              result
                          catch
                            case err: ExcessDataError =>
                              val result = Result.Terminal(ansi"Too much data was received during ${step.name}")
                              funnel.put(Progress.Update.Remove(verb, result))
                              result
            
                            case err: StreamCutError =>
                              val result = Result.Terminal(ansi"An I/O stream failed during ${step.name}")
                              funnel.put(Progress.Update.Remove(verb, result))
                              result
                        else
                          result
  
              val pulsar = Pulsar(using timekeeping.long)(100)
              
              val ui = Task:
                funnel.stream.multiplexWith:
                  pulsar.stream.map { _ => Progress.Update.Print }
                .foldLeft(Progress(TreeMap(), Nil, totalTasks = totalTasks))(_(_))
              
              val uiReady = ui()
              val resultSet = Future.sequence(futures.values).await().to(Set)
              val result = resultSet.foldLeft(Result.Complete(Nil))(_ + _)
              
              val subprocesses: List[Subprocess] = if !result.success then running else
                build.linearization.map: step =>
                  val subprocess: Option[Subprocess] = step.exec.fold[Option[Subprocess]](None):
                    case Exec(browsers, url, start, stop) =>
                      val verb = Verb.Exec(ansi"main class ${palette.Class}($start)")
                      
                      val mainTask = Task:
                        val hash = t"${build.hashes.get(step)}$start".digest[Crc32].encode[Base256]
                        funnel.put(Progress.Update.Add(verb, hash))
                        running.foreach(_.stop())
                        val resources = step.allResources(build).map(_.path)
                        
                        Irk.synchronized:
                          val oldOut = System.out.nn
                          
                          val out = java.io.PrintStream(new java.io.OutputStream:
                            override def write(byte: Int): Unit = write(Array[Byte](byte.toByte))
                            
                            override def write(bytes: Array[Byte], off: Int, len: Int): Unit =
                              val content = bytes.immutable(using Unsafe).slice(off, off + len).immutable(using Unsafe)
                              funnel.put(Progress.Update.Stdout(verb, content))
                          )

                          System.setOut(out)
                          val classpath = (step.classpath(build) ++ resources).to(List)
                          val subprocess = Run.main(classpath)(start, stop)
                          System.setOut(oldOut)
                          subprocess
                      
                      val subprocess = mainTask().future.await()
                      
                      val newResult = subprocess.status match
                        case None | Some(ExitStatus.Ok) =>
                          result
                        case Some(ExitStatus.Fail(n)) =>
                          Result.Terminal(ansi"Process returned exit status $n")
                      
                      funnel.put(Progress.Update.Remove(verb, newResult))
                      for b <- browser; u <- url do
                        if running.isEmpty then b.navigateTo(Url.parse(u)) else b.refresh()
                      
                      Some(subprocess)

                  step.artifact.foreach: artifact =>
                    val verb = Verb.Build(ansi"artifact ${palette.File}(${artifact.path.relativeTo(pwd.path).show})")
                    val hash = t"${build.hashes.get(step)}".digest[Crc32].encode[Base256]
                    funnel.put(Progress.Update.Add(verb, hash))
                    
                    Artifact.build(artifact, irkJar(scriptFile), step.name, step.version,
                        step.classpath(build).to(List), step.allResources(build).to(List))
                    
                    funnel.put(Progress.Update.Remove(verb, Result.Complete(Nil)))
                      
                  if publishSonatype then Sonatype.publish(build, env.get(t"SONATYPE_PASSWORD"))
                  
                  subprocess
                .flatten
              
              pulsar.stop()
              funnel.stop()
              uiReady.future.await()
              Out.print(report(result, columns))

              val arrowColor = if result.success then colors.YellowGreen else colors.OrangeRed
              Out.print(ansi"${Bg(arrowColor)}(  )")
              Out.print(ansi"${arrowColor}() ")

              if result == Result.Aborted
              then Out.println(ansi"Build was ${colors.SteelBlue}(aborted)")
              else if !result.success
              then
                Out.print(ansi"Build ${colors.OrangeRed}(failed) with ")
                Out.println(ansi"${colors.Gold}(${result.errors.size}) ${Numerous(t"error")(result.errors)}")
              else
                Out.print(ansi"Build ${colors.Green}(succeeded)")
                
                if result.issues.length > 0
                then Out.println(ansi" with ${colors.Gold}(${result.issues.size}) ${Numerous(t"warning")(result.issues)}")
                else Out.println(ansi"")

              Out.println(t"\e[0m\e[?25h\e[A")

              if watch then
                Out.print(Progress.titleText(t"Irk: waiting for changes"))
                Out.print(ansi"${t"\n"}${Bg(colors.Orange)}(  )")
                Out.print(ansi"${colors.Orange}() ")
                Out.println(ansi"Watching ${colors.Gold}(${watcher.directories.size}) directories for changes...")
              
              tap.open()
              loop(false, stream.tail, build, result.success, browser, subprocesses, count + 1, columns)
          
      val fileChanges: LazyList[Event] =
        if !watch then LazyList()
        else interrupts.multiplexWith(watcher.stream.regulate(tap).cluster(100).map(Event.Changeset(_)))
      
      val build = generateBuild()
      
      if exec then Chrome.session(8869):
        loop(true, Event.Changeset(Nil) #:: fileChanges, build, false, List(browser))
      else loop(true, Event.Changeset(Nil) #:: fileChanges, build, false, Nil)

case class ExecError(err: Exception)
extends Error((t"an exception was thrown while executing the task: ", err.getMessage.nn.show))

case class TrapExitError(status: ExitStatus)
extends Error((t"a call to System.exit was trapped: ", status))

object Run:
  import java.net.*

  def main(classpath: List[DiskPath[Unix]])(start: Text, stop: Option[Text])(using Stdout): Subprocess =
    val urls = classpath.map { path => URL(t"file://${path.fullname}/".s) }
    val loader = URLClassLoader(urls.to(Array)).nn

    def invoke(className: Text): Any =
      Run.synchronized:
        if System.getSecurityManager != TrapExit then System.setSecurityManager(TrapExit)

      val cls: Class[?] = loader.loadClass(className.s).nn
      val method = cls.getMethods.nn.find(_.nn.getName == "main").get.nn
      method.invoke(null, Array[String]())
    
    val subprocess =
      try
        invoke(start)
        Subprocess(None)
      catch
        case err: TrapExitError => Subprocess(Some(err.status))
        case err: java.lang.reflect.InvocationTargetException => err.getCause match
          case null               => Subprocess(Some(ExitStatus(2)))
          case err: TrapExitError => Subprocess(Some(err.status))
          case err: Exception     => Subprocess(Some(ExitStatus(1)))
          case err: Throwable     => Subprocess(Some(ExitStatus(2)))
        case err: Exception     => Subprocess(Some(ExitStatus(1)))
        case err: Throwable     => Subprocess(Some(ExitStatus(2)))
    
    stop.map: stop =>
      def term = try invoke(stop).unit catch case err: Exception => ()
      val shutdownThread = Thread((() => term): Runnable)
      Runtime.getRuntime.nn.addShutdownHook(shutdownThread)
      
      subprocess.copy(terminate = Some({ () =>
        Runtime.getRuntime.nn.removeShutdownHook(shutdownThread)
        term
      }))
    .getOrElse(subprocess)

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
