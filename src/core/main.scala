package fury

import gossamer.*
import rudiments.*
import turbulence.*
import acyclicity.*
import euphemism.*
import eucalyptus.Log
import galilei.*, filesystems.unix
import anticipation.*, integration.galileiPath
import guillotine.*
import parasitism.*, threading.virtual, monitors.global
import kaleidoscope.*
import escapade.*
import gastronomy.*
import oubliette.*
import cellulose.*
import harlequin.*
import iridescence.*, solarized.*
import serpentine.*
import exoskeleton.*
import imperial.*
import tarantula.*
import profanity.*
import xylophone.*
import telekinesis.*
import escritoire.*, tableStyles.horizontalDots
import surveillance.*

import timekeeping.long
import rendering.ansi

import scala.collection.mutable as scm
import scala.util.chaining.scalaUtilChainingOps

import java.nio.BufferOverflowException

object Fury extends Daemon():

  val logo: Text = t"CiAgICAgICAgICAgICAgIBtbMzg7MjsyMTc7MTsxMDFtwrcg4oCiIOKXjyAbWzM4OzI7MjA7NjA7MTAwbeKXjyDil48g4oCiIMK3CiAgICAgICAgIBtbMzg7MjsyMTc7MTsxMDFtwrcg4pePIOKXjyAbWzM4OzI7MTEyOzQ7NDNt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDCtwogICAgICAgIBtbMzg7MjsyMjc7MTs5MW3igKIg4pePIOKXjyAbWzM4OzI7MTEyOzQ7NDNt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyAbWzM4OzI7MjEwOzExNTs3beKXjyDil48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDigKIgICAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ila3ilIDilIDilIDilIDilIDila4KICAgICAgIBtbMzg7MjsyMjc7MTs5MW3il48g4pePIOKXjyDil48gG1szODsyOzE2OzUyOzkwbeKXjyAbWzM4OzI7ODM7NDk7Mm3il48gG1szODsyOzIxMDsxMTU7N23il48g4pePIOKXjyAbWzM4OzI7MDszMzszNm3il48gG1szODsyOzE2OzUyOzkwbeKXjyDil48g4pePIOKXjyDil48gICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKUgiAg4pWt4pSA4pSA4pWvCiAgICAgIBtbMzg7MjsyMjc7MTs5MW3il48g4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsyMTA7MTE1Ozdt4pePIOKXjyDil48g4pePIBtbMzg7MjswOzMzOzM2beKXjyAbWzM4OzI7MTY7NTI7OTBt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxNjs1Mjs5MG3il48g4pePIOKXjyAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ilIIgIOKUggogICAgIBtbMzg7MjsyMzc7MTs4MW3igKIg4pePIOKXjyDil48gG1szODsyOzIyODsxMDQ7OW3il48gG1szODsyOzIxMDsxMTU7N23il48g4pePIOKXjyDil48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7MjsxNDs0ODs4NW3il48g4pePIBtbMzg7MjsxNzA7MTY4OzMxbeKXjyDil48gG1szODsyOzE0OzQ4Ozg1beKXjyDil48g4oCiICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilJTilIDilIDila7ila3ilIDilIDila4g4pWt4pSA4pSA4pWu4pWt4pSA4pSA4pWu4pSA4pSA4pSA4pWu4pWt4pSA4pSA4pWuIOKVreKUgOKUgOKVrgogICAgG1szODsyOzIzNzsxOzgxbcK3IOKXjyDil48gG1szODsyOzIxMTs0NDsxN23il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIBtbMzg7MjsyMTA7MTE1Ozdt4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsxNDs0ODs4NW3il48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxNDs0ODs4NW3il48g4pePIOKXjyDCtyAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIzilIDilIDila/ilIIgIOKUgiDilIIgIOKUguKUgiAg4pWt4pSA4pSA4pSA4pWv4pSCICDilIIg4pSCICDilIIKICAgICAbWzM4OzI7MjM3OzE7ODFt4pePIOKXjyAbWzM4OzI7MjI4OzEwNDs5beKXjyDil48g4pePIOKXjyAbWzM4OzI7MjEwOzExNTs3beKXjyDil48gG1szODsyOzIzMDsxNzU7OG3il48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyDil48gG1szODsyOzY4Ozc2OzI2beKXjyAbWzM4OzI7MTI7NDQ7ODBt4pePIOKXjyDil48g4pePICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIIgICDilIIgIOKUgiDilIIgIOKUguKUgiAg4pSCICAgIOKUgiAg4pSCIOKUgiAg4pSCCiAgICAbWzM4OzI7MjQ3OzE7NzFtwrcg4pePIBtbMzg7MjsyMjg7MTA0Ozlt4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIBtbMzg7MjsxNzA7MTY4OzMxbeKXjyDil48g4pePIBtbMzg7Mjs2ODs3NjsyNm3il48gG1szODsyOzEyOzQ0OzgwbeKXjyDil48g4pePIOKXjyDCtyAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIIgICDilIIgIOKVsOKUgOKVryAg4pSC4pSCICDilIIgICAg4pSCICDilbDilIDila8gIOKUggogICAgIBtbMzg7MjsyNDc7MTs3MW3igKIgG1szODsyOzIxMTs0NDsxN23il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIOKXjyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIOKXjyDil48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxMDs0MDs3NW3il48g4pePIOKXjyDil48g4oCiICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pWw4pSA4pSA4pWvICAg4pWw4pSA4pSA4pSA4pSA4pSA4pSA4pSA4pWv4pWw4pSA4pSA4pWvICAgIOKVsOKUgOKUgOKUgOKUgOKUkCAg4pSCCiAgICAgIBtbMzg7MjsyNDc7MTs3MW3il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIBtbMzg7MjsyMzA7MTc1Ozht4pePIBtbMzg7MjsyNDc7MTgxOzI0beKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48g4pePIBtbMzg7MjswOzMzOzM2beKXjyAbWzM4OzI7MTA7NDA7NzVt4pePIOKXjyDil48g4pePIOKXjyAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKUgiAg4pSCCiAgICAgICAbWzM4OzI7MjI4OzEwNDs5beKXjyDil48gG1szODsyOzIzMDsxNzU7OG3il48gG1szODsyOzI0NzsxODE7MjRt4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7Mjs4OzM2OzcwbeKXjyDil48g4pePIOKXjyDil48gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKVreKUgOKUgOKUgOKUgOKVryAg4pSCCiAgICAgICAgG1szODsyOzIyODsxMDQ7OW3igKIg4pePIBtbMzg7MjsyMzA7MTc1Ozht4pePIBtbMzg7MjsyNDc7MTgxOzI0beKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48gG1szODsyOzg7MzY7NzBt4pePIOKXjyDil48g4pePIOKXjyDigKIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ilbDilIDilIDilIDilIDilIDilIDilIDila8KICAgICAgICAgG1szODsyOzIyODsxMDQ7OW3CtyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7NjszMjs2NW3il48g4pePIOKXjyDil48g4pePIMK3CiAgICAgICAgICAgICAgIBtbMzg7MjsyNDc7MTgxOzI0bcK3IOKAoiDil48gG1szODsyOzE1MDsxMTc7MzRt4pePIBtbMzg7Mjs0OzI4OzYwbeKXjyDigKIgwrcKG1swbQo=".decode[Base64].uString

  def main(using CommandLine, Environment): ExitStatus =
    Sys.scala.concurrent.context.maxExtraThreads() = t"800"
    Sys.scala.concurrent.context.maxThreads() = t"1000"
    Sys.scala.concurrent.context.minThreads() = t"100"


    try supervise(t"session-${sessionCount()}"):
      cli.args match
        case t"about" :: _        => Fury.about()
        case t"help" :: _         => Fury.help()
        case t"init" :: name :: _ => Fury.init(unsafely(env.pwd.directory(Expect)), name)
        case t"version" :: _      => Fury.showVersion()
        case t"stop" :: params    => Fury.stop(cli)
        case params               =>
          val command = params.headOption.filter(!_.starts(t"-")).maybe
          val watch = params.contains(t"-w") || params.contains(t"--watch")
          val exec = params.contains(t"-b") || params.contains(t"--browser")
          Fury.build(command, false, watch, cli.script, exec)
  
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
  
  def version: Text = Option(getClass.nn.getPackage.nn.getImplementationVersion).fold(t"0")(_.nn.show)
  def javaVersion: Text = safely(Sys.java.version()).or(t"unknown")
  def githubActions(using Environment): Boolean = env(t"GITHUB_ACTIONS") != Unset

  def scalaVersion: Text =
    val props = java.util.Properties()
    props.load(getClass.nn.getClassLoader.nn.getResourceAsStream("compiler.properties").nn)
    props.get("version.number").toString.show
 
  def homeDir(using Environment): Directory[Unix] = unsafely(Home[DiskPath[Unix]]().directory(Expect))
      
  def cacheDir(using Environment): Directory[Unix] =
    try (Home.Cache[DiskPath[Unix]]() / p"fury").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's cache directory could not be created", err)

  def universeDir(using Environment): Directory[Unix] =
    try (cacheDir / p"uni").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's universe directory could not be created", err)
  
  def libDir(using Environment): Directory[Unix] =
    try (cacheDir / p"lib").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's lib directory could not be created", err)
  
  def repoDir(using Environment): Directory[Unix] =
    try (cacheDir / p"repo").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's repo cache could not be created", err)
  
  def libJar(hash: Digest[Crc32])(using Environment): DiskPath[Unix] =
    unsafely(libDir / t"${hash.encode[Hex].lower}.jar")
  
  def tmpDir(using Environment): Directory[Unix] =
    try (cacheDir / p"tmp").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's tmp directory could not be created", err)
  
  def hashDir(using Environment): Directory[Unix] =
    try (cacheDir / p"hash").directory(Ensure)
    catch case err: IoError => throw AppError(t"The user's hash directory could not be created", err)

  private val prefixes = Set(t"/scala", t"/dotty", t"/compiler.properties", t"/org/scalajs", t"/com",
      t"/incrementalcompiler.version.properties", t"/library.properties", t"/NOTICE")

  private def scriptSize: Int throws StreamCutError | ClasspathRefError =
    unsafely(Classpath() / p"exoskeleton" / p"invoke").resource.read[Bytes]().size

  def furyJar(scriptFile: File[Unix])(using Stdout, Environment): File[Unix] throws StreamCutError = synchronized:
    import java.nio.file.*
    val jarPath = unsafely(libDir.path / t"base-$version.jar")

    try if jarPath.exists() then jarPath.file(Expect) else
      val buf = Files.readAllBytes(scriptFile.javaPath).nn
      val output = java.io.BufferedOutputStream(java.io.FileOutputStream(jarPath.javaFile).nn)
      
      try output.write(buf, scriptSize, buf.length - scriptSize)
      catch case err: ClasspathRefError =>
        throw AppError(t"Could not determine the size of the bootloader script")
      
      output.close()
      val fs = FileSystems.newFileSystem(java.net.URI(t"jar:file:${jarPath.fullname}".s), Map("zipinfo-time" -> "false").asJava).nn
      
      Files.walk(fs.getPath("/")).nn.iterator.nn.asScala.to(List).sortBy(-_.toString.length).foreach:
        file =>
          def keep(name: Text): Boolean =
            name == t"" || name == t"/org" || prefixes.exists(name.starts(_))
          
          try
            if !keep(file.nn.toString.show) then Files.delete(file.nn)
            else Files.setLastModifiedTime(file.nn, Zip.epoch)
          catch case err: Exception => () //Out.println(t"Got a NPE on ${file.nn.toString.show}")
      
      fs.close()
      
      jarPath.file(Expect)
    catch case err: IoError =>
      throw AppError(t"The fury binary could not be copied to the user's cache directory")

  def getFile(ref: Text)(using Environment): File[Unix] = unsafely(libJar(ref.digest[Crc32]).file(Expect))

  def fetchFile(ref: Text, funnel: Option[Funnel[Progress.Update]])(using Stdout, Internet, Monitor, Environment)
               : Task[File[Unix]] =
    if ref.starts(t"https:") then
      val hash = ref.digest[Crc32]
      if libJar(hash).exists() then Task(t"hash"):
        try libJar(hash).file(Expect) catch case err: IoError =>
          throw AppError(t"Could not access the dependency JAR, $ref", err)
      else Task(t"download"):
        val verb = Verb.Download(ansi"${colors.CornflowerBlue}($ref)")
        funnel.foreach(_.put(Progress.Update.Add(verb, (hash))))
        try
          val file = libJar(hash).file(Create)
          Uri(ref).writeTo(file)
          funnel.foreach(_.put(Progress.Update.Remove(verb, Result.Complete())))
          file
        catch
          case err: StreamCutError =>
            funnel.foreach(_.put(Progress.Update.Remove(verb, Result.Terminal(ansi"A stream error occurred"))))
            throw AppError(t"Could not download the file $ref", err)
          
          case err: IoError =>
            funnel.foreach(_.put(Progress.Update.Remove(verb, Result.Terminal(ansi"An I/O error occurred"))))
            throw AppError(t"The downloaded file could not be written to ${libJar(hash).fullname}", err)
        
    else Task(t"parse"):
      try Unix.parse(ref).file(Expect) catch
        case err: InvalidPathError => throw AppError(t"Could not access the dependency JAR, $ref", err)
        case err: IoError          => throw AppError(t"Could not access the dependency JAR, $ref", err)


  val sessionCount: Counter = Counter(0)

  private lazy val fileHashes: FileCache[Digest[Crc32]] = new FileCache()

  def hashFile(file: File[Unix]): Digest[Crc32] throws IoError | AppError =
    try fileHashes(file.path.fullname, file.modified):
      file.read[Bytes]().digest[Crc32]
    catch
      case err: StreamCutError  => throw AppError(t"The stream was cut while hashing a file", err)
      case err: Error[?]        => throw AppError(t"An unexpected error occurred", err)
  
  def cloneRepo(path: DiskPath[Unix], url: Text)(using Environment): Unit =
    try sh"git clone -q $url ${path.fullname}".exec[Unit]()
    catch
      case err: ExecError => throw AppError(t"Could not run `git clone` for repository $url", err)
      case err: EnvError  => throw AppError(t"Could not run `git clone` for repository $url", err)
          
  def readImports(seen: Map[Digest[Crc32], File[Unix]], files: File[Unix]*)
                 (using Stdout, Environment)
                 : Set[File[Unix]] =
    case class Imports(repos: Option[List[Repo]], imports: Option[List[Text]]):
      def repoList: List[Repo] = repos.presume
      def gen(seen: Map[Digest[Crc32], File[Unix]], files: File[Unix]*): Set[File[Unix]] = files.to(List) match
        case file :: tail =>
          val importFiles: List[File[Unix]] = imports.presume.flatMap: path =>
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
                      Fury.cloneRepo(unsafely(ref.parent), repo.url)
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
          def interpret(digest: Option[Digest[Crc32]]) =
            Json.parse(file.read[Text]()).as[Imports].gen(digest.fold(seen)(seen.updated(_, file)), files*)
          
          if file.exists() then
            val digest: Digest[Crc32] = hashFile(file)
            if !seen.contains(digest) then interpret(Some(digest)) else readImports(seen, tail*)
          else interpret(None)

        catch case err: Exception => readImports(seen, tail*)

  def stop(cli: CommandLine)(using Stdout): ExitStatus =
    cli.shutdown()
    ExitStatus.Ok

  def showVersion()(using Stdout): ExitStatus =
    Out.println(Fury.version)
    ExitStatus.Ok
  
  def about()(using Stdout): ExitStatus =
    Out.println(logo)
    case class Software(name: Text, version: Text, license: Text)
    
    val software = List(
      Software(t"Fury", Fury.version, t"Apache License, Version 2.0"),
      Software(t"Scala", Fury.scalaVersion, t"Apache License, Version 2.0"),
      Software(t"Java", Fury.javaVersion, t"GNU General Public License, version 2 with Classpath Exception")
    )
    
    Table[Software](
      Column(ansi"$Bold(Software)")(_.name),
      Column(ansi"$Bold(Version)")(_.version),
      Column(ansi"$Bold(License)")(_.license)
    ).tabulate(software, 80, DelimitRows.SpaceIfMultiline).foreach(Out.println(_))

    Out.println(ansi"  ${colors.Gray}($Italic(© Copyright 2022 Propensive OÜ and Jon Pretty. All Rights Reserved.))")
    Out.println(t"")

    ExitStatus.Ok

  def help()(using Stdout): ExitStatus =
    Out.println(t"Usage: fury [subcommand] [options]")
    Out.println(t"")
    Out.println(t"Subcommands:")
    Out.println(t"  build    -- runs the build in the current directory (default)")
    Out.println(t"  about    -- show version information for Fury")
    Out.println(t"  help     -- show this message")
    Out.println(t"  stop     -- stop the Fury daemon process")
    Out.println(t"")
    Out.println(t"Options:")
    Out.println(t"  -w, --watch    Wait for changes and re-run build.")
    ExitStatus.Ok

  def init(pwd: Directory[Unix], name: Text)(using Stdout): ExitStatus =
    ExitStatus.Fail(1)
    // val buildPath = unsafely(pwd / t"build.irk")
    // if buildPath.exists() then
    //   Out.println(t"Build file build.irk already exists")
    //   ExitStatus.Fail(1)
    // else
    //   import unsafeExceptions.canThrowAny
    //   val buildFile = buildPath.file(Create)
    //   val src = (pwd / t"src").directory(Ensure)
    //   val sourceDir = (src / t"core").directory(Ensure)
      
    //   val module = Project(t"${pwd.path.name}/core", Nil, None, None,
    //       Set(sourceDir.path.relativeTo(pwd.path).show), None, None, None, None, None, None, None, Nil)

    //   val config = BuildConfig(None, None, None, List(module), List(), None, None)
    //   try
    //     config.json.show.writeTo(buildFile)
    //     ExitStatus.Ok
    //   catch
    //     case err: IoError =>
    //       Out.println(t"Could not write to build.irk")
    //       ExitStatus.Fail(1)
    //     case err: StreamCutError =>
    //       Out.println(t"Could not write to build.irk")
    //       ExitStatus.Fail(1)

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
          issues.groupBy(_.code.path).to(List).flatMap: (path, issues) =>
            path match
              case Unset =>
                Log.warn(t"Unknown source path")
                None
              case path: Relative =>
                val file = unsafely(baseDir + path)
                if !file.exists() then
                  Log.warn(t"Missing source file: ${file}")
                  None
                else Some(file.file(Ensure) -> issues)
          .sortBy(_(0).modified)
        .sortBy(_.last(0).modified).flatten
        
      catch case err: IoError =>
        throw AppError(ansi"a file containing an error was deleted: ${err.toString.show}", err)//: ${err.ansi}", err)
    
    def arrow(k1: (Srgb, Text), k2: (Srgb, Text)): AnsiText =
      def hc(c: Srgb): Srgb = if c.hsl.lightness > 0.5 then colors.Black else colors.White
      ansi"${Bg(k1(0))}( ${hc(k1(0))}(${k1(1)}) )${Bg(k2(0))}(${k1(0)}() ${hc(k2(0))}(${k2(1)}) )${k2(0)}()"

    val highlighting: scm.Map[Text, IArray[Seq[Token]]] = scm.HashMap[Text, IArray[Seq[Token]]]()
    
    def highlight(text: Text): IArray[Seq[Token]] = if highlighting.contains(text) then highlighting(text) else
      highlighting(text) = ScalaSyntax.highlight(text)
      highlighting(text)
    
    val bg = Bg(Srgb(0.1, 0.0, 0.1))
    
    def format(text: Text, line: Int, margin: Int) =
      val syntax = highlight(text)
      if line >= syntax.length then ansi""
      else syntax(line).map:
        case Token.Code(code, accent) => accent match
          case Accent.Type              => ansi"${colors.YellowGreen}(${code})"
          case Accent.Term              => ansi"${colors.CadetBlue}(${code})"
          case Accent.Symbol            => ansi"${colors.Turquoise}(${code})"
          case Accent.Keyword           => ansi"${colors.DarkOrange}(${code})"
          case Accent.Modifier          => ansi"${colors.Chocolate}(${code})"
          case Accent.Ident             => ansi"${colors.BurlyWood}(${code})"
          case Accent.Error             => ansi"${colors.OrangeRed}($Underline(${code}))"
          case Accent.Number            => ansi"${colors.Gold}(${code})"
          case Accent.String            => ansi"${colors.Plum}(${code})"
          case other                    => ansi"${code}"
        case Token.Unparsed(txt)     => ansi"$txt"
        case Token.Markup(_)         => ansi""
        case Token.Newline           => throw Mistake("Should not have a newline")
      .join.take(columns - 2 - margin)

    def codeLine(margin: Int, codeText: Text, line: Int): AnsiText =
      val lineNo = ansi"${colors.Orange}(${line.show.pad(margin, Rtl)})"
      val bar = ansi"${colors.DarkSlateGray}(║)$bg "
      val code = format(codeText, line - 1, margin)
      ansi"${escapes.Reset}$lineNo$bar$code${t" "*(columns - 2 - margin - code.length)}${escapes.Reset}"

    sorted.foreach:
      case (file, issues) =>
        val codeText = issues.head.code.content.text
        //val syntax: IArray[Seq[Token]] = highlight(codeText)
        issues.groupBy(_.code.startLine).to(List).sortBy(_(0)).foreach:
          case (ln, issues) => issues.head match
            case Issue(level, baseDir, pos, stack, message) =>
              val margin = (pos.endLine + 2).show.length
              val codeWidth = columns - 2 - margin
              val path = pos.path.fm(t"«unknown»")(_.show)
              val posText = t"${path}:${pos.startLine + 1}:${pos.from}"
              
              val shade = level match
                case Level.Error => colors.Crimson
                case Level.Warn  => colors.Orange
                case Level.Info  => colors.SteelBlue
              
              appendln(arrow(colors.Purple -> pos.module.fm(t"[external]")(_.show), shade -> posText))
              
              if pos.startLine > 1 then appendln(codeLine(margin, codeText, pos.startLine))

              (pos.startLine to pos.endLine).foreach: lineNo =>
                if lineNo < (pos.startLine + 2) || lineNo > (pos.endLine - 2) then
                  val code = format(codeText, lineNo, margin)
                  val before = if lineNo == pos.startLine then code.take(pos.from) else ansi""
                  val after = if lineNo == pos.endLine then code.drop(pos.to) else ansi""
                  
                  val highlighted =
                    if lineNo == pos.startLine then
                      if lineNo == pos.endLine then code.slice(pos.from, pos.to) else code.drop(pos.from)
                    else if lineNo == pos.endLine then code.take(pos.to) else code

                  append(ansi"${escapes.Reset}${colors.Orange}($Bold(${(lineNo + 1).show.pad(margin, Rtl)}))")
                  append(ansi"${colors.DarkSlateGray}(║)$bg ")
                  append(before)
                  append(ansi"${colors.OrangeRed}(${highlighted.plain})")
                  val width = highlighted.length + before.length + after.length
                  appendln(ansi"$after${t" "*(codeWidth - width)}${escapes.Reset}")
                else if lineNo == pos.startLine + 2 then
                  val code = format(codeText, pos.startLine + 1, margin)
                  val break = snip.take(codeWidth)
                  append(ansi"${escapes.Reset}${t"".pad(margin, Rtl)}${colors.DarkSlateGray}(╫)$bg ")
                  appendln(ansi"${colors.RebeccaPurple}($break)")

              if pos.endLine + 1 < highlight(codeText).length
              then appendln(codeLine(margin, codeText, pos.endLine + 2))

              if pos.startLine == pos.endLine then
                append(ansi"${colors.DarkSlateGray}(${t" "*margin}╟${t"─"*pos.from}┴${t"─"*(pos.to - pos.from)}┘)")
                appendln(ansi"${t"\e[K"}")
              else appendln(ansi"${escapes.Reset}${t" "*margin}${colors.DarkSlateGray}(║)")
              
              message.cut(t"\n").foreach: line =>
                appendln(ansi"${escapes.Reset}${t" "*margin}${colors.DarkSlateGray}(║) ${Bold}($line)")
              
              appendln(ansi"${escapes.Reset}${t" "*margin}${colors.DarkSlateGray}(╨)")
              
              if !stack.isEmpty then
                appendln(ansi"This includes inlined code from:")
                val pathWidth = stack.map(_.path.fm(9)(_.show.length)).max
                val refWidth = stack.map(_.module.fm(10)(_.show.length)).max
                val indent = pathWidth + refWidth + 7
                
                stack.foreach: pos =>
                  val ref = pos.module.fm(t"[external]")(_.show).pad(refWidth, Rtl)
                  val path = pos.path.mm(_.show.pad(pathWidth, Rtl))
                  val code = codeLine(margin + indent, pos.content.text, pos.startLine).drop(indent)
                  appendln(ansi"${arrow(colors.DarkCyan -> ref, colors.LightSeaGreen -> path.or(t"«unknown»"))} $code${escapes.Reset}")
                
                appendln(ansi"")
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

  def build(command: Maybe[Text], publishSonatype: Boolean, watch: Boolean = false, scriptFile: File[Unix],
                exec: Boolean)
           (using Stdout, InputSource, Monitor, Environment)
           : ExitStatus throws AppError = unsafely:
    Tty.capture:
      val rootBuild = unsafely(env.pwd / t"build.irk")
      val tap: Tap = Tap(true)

      def interrupts = Tty.stream[Keypress].map:
        case key =>
          Log.fine(t"Got input: $key")
          key
      .collect:
        case Keypress.Ctrl('C')        =>
          Log.fine(t"Received interrupt")
          Event.Interrupt
        case Keypress.Resize(width, _) =>
          Log.fine(t"Received resize message")
          Event.Resize(width)
      .filter:
        case Event.Interrupt =>
          if !tap.state() then
            summon[Monitor].cancel()
            Log.fine(t"Ignored interrupt")
            false
          else
            Log.fine(t"Propagated interrupt")
            true
        
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

      def generateBuild(command: Maybe[Text]): Build =
        try
          val pwd = env.pwd.directory(Expect)
          val (commands, universe) = Universe.resolve(pwd)
          
          val command2: Maybe[Target] = commands.find(_.id == command.or(t"default")).maybe
          val build = Build(pwd, command2, universe)
          if watch then updateWatches(watcher, build) else build
        catch
          case err: BuildfileError => throw AppError(err.message)
          case err: IoError => throw AppError(t"The build file could not be read because $err")

      @tailrec
      def loop(first: Boolean, stream: LazyList[Event], oldBuild: Build, lastSuccess: Boolean,
                        browser: List[WebDriver#Session], running: List[Jvm] = Nil,
                        count: Int = 1, columns: Int = 120)(using Internet, Monitor)
                   : ExitStatus =
        import unsafeExceptions.canThrowAny
        tap.open()
        val cancel: Promise[Unit] = Promise()

        if stream.isEmpty then if lastSuccess then ExitStatus.Ok else ExitStatus.Fail(1)
        else stream.head match
          case Event.Interrupt =>
            running.foreach(_.abort())
            if lastSuccess then ExitStatus.Ok else ExitStatus.Fail(1)
          case Event.Resize(width) =>
            loop(first, stream.tail, oldBuild, lastSuccess, browser, running, count, width)
          case Event.Changeset(fileChanges) =>
            tap.pause()
            
            def ephemeral(events: List[WatchEvent]): Boolean = events match
              case WatchEvent.NewFile(_, _) +: _ :+ WatchEvent.Delete(_, _) => true
              case _                                                        => false
            
            val changes: List[Change] =
              fileChanges.groupBy(_.path[DiskPath[Unix]]).collect:
                case (path, es) if !ephemeral(es) => es.last
              .foldLeft[List[Change]](Nil): (changes, event) =>
                Option(event).collect:
                  case WatchEvent.Delete(_, _)  => t"deleted"
                  case WatchEvent.Modify(_, _)  => t"modified"
                  case WatchEvent.NewFile(_, _) => t"created"
                .foreach: change =>
                  val file = ansi"${palette.File}(${event.path[DiskPath[Unix]].relativeTo(env.pwd)})"
                  Out.println(ansi"The file $file was $change")
                
                if event.path[DiskPath[Unix]].name.ends(t".irk")
                then Change(ChangeType.Build, event.path) :: changes
                else if event.path[DiskPath[Unix]].name.ends(t".scala")
                then Change(ChangeType.Source, event.path) :: changes
                else if oldBuild.resourceDirs.exists(_.path.precedes(event.path))
                then Change(ChangeType.Resource, event.path) :: changes
                else changes
            
            if changes.isEmpty && count > 1 then
              loop(false, stream.tail, oldBuild, lastSuccess, browser, running, count, columns)
            else
              changes.foreach: change =>
                Out.println(ansi"Rebuild triggered by change to a ${change.changeType.category} file")
  
              val build: Build =
                if changes.exists(_.changeType.rebuild)
                then safely(generateBuild(command)).or(oldBuild)
                else oldBuild

              build.clearHashes()
  
              val oldHashes = build.cache
              Out.println(t"")
              Out.print(ansi"${colors.Black}(${Bg(colors.DarkTurquoise)}( Build #$count ))")
              Out.print(ansi"${colors.DarkTurquoise}(${Bg(colors.DodgerBlue)}( ${colors.Black}(${build.pwd.path.fullname}) ))")
              Out.println(ansi"${colors.DodgerBlue}()")
              
              val funnel: Funnel[Progress.Update] = Funnel()
              funnel.put(Progress.Update.Resize(columns))
              val totalTasks: Int = build.steps.size

              val owners: Map[DiskPath[Unix], Step] = build.steps.flatMap: step =>
                step.sources.map(_.path -> step)
              .to(Map)
              
              val tasks = build.graph.traversal[Task[Result]]: (set, step) =>
                set.sequence.flatMap: results =>
                  step.jars.map(Fury.fetchFile(_, Some(funnel))).sequence.flatMap: downloads =>
                    if cancel.ready then Task(step.id.show)(Result.Aborted) else Task(step.id.show):
                      val verb = Verb.Compile(ansi"${Green}(${step.name})")
                      val result: Result = results.foldLeft(Result.Complete())(_ + _)
                      
                      if result.success then
                        val hash = build.hashes(step)
                        try
                          if oldHashes.get(step) != build.hashes.get(step)
                          then
                            funnel.put(Progress.Update.Add(verb, hash))
                            val newResult = step.compile(build.hashes, oldHashes, build, scriptFile, cancel, owners)
                            val status = if cancel.ready then Result.Aborted else newResult
                            funnel.put(Progress.Update.Remove(verb, status))
                            
                            if newResult.success && build.plugins.contains(step.id) then
                              val verb2 = Verb.Build(ansi"compiler plugin ${palette.File}(${step.id})")
                              val path = Fury.libJar(build.hashes(step))
                              if !path.exists() then
                                funnel.put(Progress.Update.Add(verb2, build.hashes(step)))
                                val artifact = Artifact(path, step.main, Format.CompilerPlugin)
                                
                                Artifact.build(artifact, furyJar(scriptFile), step.name, step.version, step.classpath(build).to(List),
                                    step.allResources(build).to(List), step.main)
                                
                                funnel.put(Progress.Update.Remove(verb2, Result.Complete()))
                            
                            if newResult.success then step.artifact.foreach: artifact =>
                              val verb = Verb.Build(ansi"artifact ${palette.File}(${artifact.path.relativeTo(env.pwd).show})")
                              funnel.put(Progress.Update.Add(verb, build.hashes(step)))
                              
                              Artifact.build(artifact, furyJar(scriptFile), step.name, step.version,
                                  step.classpath(build).to(List), step.allResources(build).to(List),
                                  artifact.main.orElse(step.main))
                              
                              funnel.put(Progress.Update.Remove(verb, Result.Complete()))
                            
                            result + newResult
                    
                          else
                            funnel.put(Progress.Update.SkipOne)
                            result
                        catch
                          case err: StreamCutError =>
                            val result = Result.Terminal(ansi"An I/O stream failed during ${step.name}")
                            funnel.put(Progress.Update.Remove(verb, result))
                            result
                      else
                        result
  
              val pulsar = Pulsar(using timekeeping.long)(100)
              
              val ui = Task(t"ui"):
                funnel.stream.multiplexWith:
                  pulsar.stream.map(Progress.Update.Print.waive)
                .foldLeft(Progress(TreeMap(), Nil, totalTasks = totalTasks))(_(_))
              
              val resultSet = tasks.values.sequence.await().to(Set)
              val result = resultSet.foldLeft(Result.Complete())(_ + _)
              
              val subprocesses: List[Jvm] = if !result.success then running else
                build.linearization.map: step =>
                  val subprocess: Option[Jvm] = step.exec.fold[Option[Jvm]](None):
                    case Exec(browsers, url, start, stop) =>
                      val verb = Verb.Exec(ansi"main class ${palette.Class}($start)")
                      
                      val mainTask = Task(t"main"):
                        val hash = t"${build.hashes.get(step)}$start".digest[Crc32]
                        funnel.put(Progress.Update.Add(verb, hash))
                        running.foreach(_.abort())
                        val resources = step.allResources(build).map(_.path)
                        val classpath = (step.classpath(build) ++ resources).to(List)
                        val jvm = Run.main(furyJar(scriptFile).path :: classpath)(start, stop)
                        jvm

                      val subprocess: Jvm = mainTask.await()
                      
                      val newResult: Result = subprocess.await() match
                        case ExitStatus.Ok =>
                          subprocess.stdout().foreach: data =>
                            funnel.put(Progress.Update.Stdout(verb, data)) 
                          result
                        case ExitStatus.Fail(n) =>
                          subprocess.stdout().foreach: data =>
                            funnel.put(Progress.Update.Stdout(verb, data)) 
                          Result.Terminal(ansi"Process returned exit status $n")
                      
                      funnel.put(Progress.Update.Remove(verb, newResult))
                      for b <- browser; u <- url do
                        if running.isEmpty then b.navigateTo(Url.parse(u)) else b.refresh()
                      
                      Some(subprocess)
                  
                  if publishSonatype then Sonatype.publish(build, env(t"SONATYPE_PASSWORD").option)
                  
                  subprocess
                .flatten
              
              pulsar.stop()
              funnel.stop()
              ui.await()
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
                
                if result.issues.size > 0
                then Out.println(ansi" with ${colors.Gold}(${result.issues.size}) ${Numerous(t"warning")(result.issues)}")
                else Out.println(ansi"")

              Out.println(t"\e[0m\e[?25h\e[A")

              if watch then
                Out.print(Progress.titleText(t"Fury: waiting for changes"))
                Out.print(ansi"${t"\n"}${Bg(colors.Orange)}(  )")
                Out.print(ansi"${colors.Orange}() ")
                Out.println(ansi"Watching ${colors.Gold}(${watcher.directories.size}) directories for changes...")
              
              tap.open()
              loop(false, stream.tail, build, result.success, browser, subprocesses, count + 1, columns)
          
      val fileChanges: LazyList[Event] =
        if !watch then LazyList()
        else interrupts.multiplexWith(watcher.stream.regulate(tap).cluster(100).map(Event.Changeset(_)))
      
      val build = generateBuild(command)
      
      internet:
        if exec then Chrome.session(8869):
          loop(true, Event.Changeset(Nil) #:: fileChanges, build, false, List(browser))
        else loop(true, Event.Changeset(Nil) #:: fileChanges, build, false, Nil)

case class ExecError(err: Exception)
extends Error(err"an exception was thrown while executing the task: ${err.getMessage.nn.show}")

object Run:
  import java.net.*

  given Classpath()
  
  lazy val adoptium: Adoptium throws ClasspathRefError | StreamCutError | IoError | NoValidJdkError =
    given Environment = environments.system
    Adoptium.install()
  
  lazy val jdk: Jdk throws ClasspathRefError | StreamCutError | IoError | EnvError | NoValidJdkError =
    given Environment = environments.system
    adoptium.get(18, jre = true, early = true, force = false)

  def main(classpath: List[DiskPath[Unix]])(start: Text, stop: Option[Text])(using Stdout, Environment)
          : Jvm throws ClasspathRefError | IoError | StreamCutError | EnvError | NoValidJdkError =
    jdk.launch(classpath, start, Nil)

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
