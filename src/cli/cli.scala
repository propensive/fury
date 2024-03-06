/*
    Fury, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package fury

import ambience.*, systemProperties.virtualMachine, environments.virtualMachine
import anticipation.*
import aviation.*
import contingency.*
import escapade.*
import escritoire.*, tableStyles.minimal, insufficientSpaceHandling.ignore
import ethereal.*, daemonConfig.supportStderr
import eucalyptus.*
import exoskeleton.*, executives.completions, unhandledErrors.stackTrace, parameterInterpretation.posix
import fulminate.*
import galilei.*, filesystemInterfaces.galileiApi, filesystemOptions.dereferenceSymlinks
import gastronomy.*
import gossamer.*
import guillotine.*
import hallucination.*
import hellenism.*, classloaders.threadContext
import hieroglyph.*, charDecoders.utf8, charEncoders.utf8, badEncodingHandlers.strict, textMetrics.uniform
import inimitable.*
import iridescence.*, colors.*
import kaleidoscope.*
import nettlesome.*
import parasite.*, threadModels.platform
import profanity.*
import rudiments.*, homeDirectories.virtualMachine
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*
import vacuous.*

enum BusMessage:
  case Ping

given Realm = realm"fury"
given Decimalizer = Decimalizer(2)

object cli:
  val Version = Switch(t"version", false, List('v'), t"Show information about the current version of Fury")
  val Interactive = Switch(t"interactive", false, List('i'), t"Run the command interactively")
  val NoTabCompletions = Switch(t"no-tab-completions", false, Nil, t"Do not install tab completions")
  val Artifact = Switch(t"artifact", false, List('a'), t"Produce an artifact")
  val Benchmarks = Switch(t"benchmark", false, List('b'), t"Run with OS settings for benchmarking")
  val Discover = Switch(t"discover", false, List('D'), t"Try to discover build details from the directory")
  val Force = Switch(t"force", false, List('F'), t"Overwrite existing files if necessary")
  def Dir(using Raises[PathError]) = Flag[Path](t"dir", false, List('d'), t"Specify the working directory")
  val Offline = Switch(t"offline", false, List('o'), t"Work offline, if possible")
  
  def Generation(using Raises[NumberError]) =
    Flag[Int](t"generation", false, List('g'), t"Use universe generation number")

  val About = Subcommand(t"about", e"About Fury")
  val Build = Subcommand(t"build", e"Start a new build (default)")
  val Cache = Subcommand(t"cache", e"Cache operations")
  val Config = Subcommand(t"config", e"View and change configuration")
  val Shutdown = Subcommand(t"shutdown", e"Shutdown the Fury daemon")
  val Init = Subcommand(t"init", e"Initialize a new project")
  val Universe = Subcommand(t"universe", e"Universe actions")
  val UniverseSearch = Subcommand(t"search", e"Search for a release")
  val UniverseShow = Subcommand(t"show", e"Show details of the current universe")
  val UniverseUpdate = Subcommand(t"update", e"Check for universe updates")
  val Graph = Subcommand(t"graph", e"Show a build graph")
  val Update = Subcommand(t"update", e"Update Fury")
  val Install = Subcommand(t"install", e"Install Fury")

  val Clean = Subcommand(t"clean", e"Clean the cache")
  val Details = Subcommand(t"info", e"Information about cache usage")

given (using Raises[UserError]): HomeDirectory =
  given (UserError fixes SystemPropertyError) =
    case SystemPropertyError(property) =>
      UserError(msg"Could not access the home directory because the $property system property was not set.")

  homeDirectories.virtualMachine

given (using Cli): WorkingDirectory = workingDirectories.daemonClient 
given installation(using Raises[UserError]): Installation =
  given (UserError fixes ConfigError) = error => UserError(msg"The configuration file could not be read.")
  Installation()

@main
def main(): Unit =
  import cli.*
  val initTime = now()

  attempt[InitError]:
    given (InitError fixes CancelError) = error => InitError(msg"A thread was cancelled")

    supervise:
      given logFormat: LogFormat[File, Display] = logFormats.standardColor[File]
      given logFormat2: LogFormat[Err.type, Display] = logFormats.standardColor[Err.type]
      import filesystemOptions.{createNonexistent, createNonexistentParents}
      
      given Log[Display] =
        given (InitError fixes IoError) =
          error => InitError(msg"An IO error occured when trying to create the log")
        
        given (InitError fixes StreamError) =
          error => InitError(msg"Stream error when logging")
        
        given (InitError fixes UserError) =
          error => InitError(error.message)
        
        Log.route: 
          case _ => installation.config.log.path.as[File]
      

      Log.info(msg"Initialized Fury in ${(now() - initTime).show}")
      
      daemon[BusMessage]:
        attempt[UserError]:
          Log.envelop(Uuid().show.take(8)):
            if Version().present then execute(frontEnd(actions.versionInfo())) else arguments match
              case Install() :: _ =>
                val interactive = Interactive().present
                val force = Force().present
                val noTabCompletions = NoTabCompletions().present
                
                execute:
                  frontEnd:
                    if interactive then actions.install.interactive(force, noTabCompletions)
                    else actions.install.batch(force, noTabCompletions)
                  
                  service.shutdown()
                  ExitStatus.Ok
                  
              case Init() :: Nil =>
                val dir: Optional[Path] = safely(Dir()).or(safely(workingDirectory))
                val discover = Discover()
                
                execute:
                  frontEnd:
                    dir.let(actions.build.initialize(_)).or:
                      abort(UserError(msg"The working directory could not be determined."))

              case Config() :: Nil =>
                execute:
                  frontEnd:
                    info(installation.config.debug)
                    ExitStatus.Ok

              case Cache() :: subcommands => subcommands match
                case Clean() :: Nil   => execute(frontEnd(actions.cache.clean()))
                case Details() :: Nil => execute(frontEnd(actions.cache.about()))
                
                case other :: _ =>
                  execute(frontEnd(other.let(actions.invalidSubcommand(_)).or(actions.missingSubcommand())))
                
                case Nil =>
                  execute:
                    Out.println(msg"Please specify a subcommand.")
                    ExitStatus.Fail(1)

              case About() :: _ => execute(about())
              case Build() :: subcommands => subcommands match
                case Nil =>
                  execute:
                    Out.println(t"Module has not been specified")
                    ExitStatus.Fail(1)

                case target :: _ =>
                  val online = Offline().absent
                  given (UserError fixes IoError)   = accede
                  given (UserError fixes PathError) = accede
                  given (UserError fixes ExecError) = accede
                  
                  safely(internet(false)(Workspace().locals())).let: map =>
                    val refs = map.values.map(_.source).flatMap:
                      case workspace: Workspace => workspace.build.projects.flatMap: project =>
                        project.modules.map: module =>
                          ModuleRef(project.id, module.id)

                    target.let(_.suggest(previous ++ refs.map(_.suggestion)))
                  
                  execute:
                    given (UserError fixes InvalidRefError) = error => UserError(error.message)
                    
                    internet(online):
                      Async:
                        frontEnd:
                          actions.build.run(target().decodeAs[ModuleRef])
                      .await()
                  
              case Graph() :: Nil =>
                val online = Offline().absent

                execute:
                  given (UserError fixes PathError)      = accede
                  given (UserError fixes CancelError)    = accede
                  given (UserError fixes IoError)        = accede
                  given (UserError fixes NumberError)    = accede
                  given (UserError fixes WorkspaceError) = accede
                  given (UserError fixes ExecError)      = accede
                  given (UserError fixes VaultError)     = accede
                  
                  internet(online):
                    val rootWorkspace = Workspace()
                    given universe: Universe = rootWorkspace.universe()
                  
                  ExitStatus.Ok
              
              case Universe() :: subcommands =>
                val online = Offline().absent
                val generation: Optional[Int] = safely(Generation())
                
                subcommands match
                  case UniverseSearch() :: _ => execute:
                    Out.println(t"TODO: Search the universe")
                    ExitStatus.Ok
                  
                  case UniverseUpdate() :: _ => execute:
                    Out.println(t"TODO: Update the universe")
                    ExitStatus.Fail(1)

                  case Nil | (UniverseShow() :: _) => execute:
                    internet(online):
                      frontEnd:
                        actions.universe.show()

                  case command :: _ => execute:
                    Out.println(e"Command $Italic(${command.vouch(using Unsafe)()}) was not recognized.")
                    ExitStatus.Fail(1)
                  
              case Shutdown() :: Nil => execute:
                service.shutdown()
                ExitStatus.Ok
              
              case Nil =>
                given (UserError fixes WorkspaceError) = error => UserError(error.message)
                execute:
                  Out.println(Workspace().build.actions.headOption.optional.debug)
                  ExitStatus.Fail(1)
                

              case subcommand :: _ =>
                val workspace = safely(Workspace())
                
                workspace.let: workspace =>
                  subcommand.let(_.suggest(previous ++ workspace.build.actions.map(_.suggestion)))
                
                execute:
                  workspace.lay(ExitStatus.Fail(2)): workspace =>
                    Out.println(workspace.debug)

                    Out.println(t"Unrecognized subcommand: ${subcommand.let(_()).or(t"")}.")
                    ExitStatus.Fail(1)

        .recover: userError =>
          execute:
            Out.println(userError.message)
            Log.fail(userError)
            ExitStatus.Fail(1)

  .recover: initError =>
    println(initError.message)
    ExitStatus.Fail(2)


def about()(using Stdio): ExitStatus =
  safely(Out.println(Image((Classpath / p"logo.png")()).render))
  
  val asciiArt =
      t"H4sIAAAAAAAA/31Ryw3AIAi9O8UbtfHcQw8wRrUzMUmTKlSx1HgA3ocXFT6FtulySUIZEIO49gllLcjIA62MmgkY"+
      t"3UOBeu+2VrdCCxfsm2RhAQQOD7aCq5KvtiTQTnDqbZ/gbf0LV8dcqUdzxN+x1CHBfa7mjPlh4HQDGOnRlikCAAA="

  unsafely(asciiArt.decode[Base64]).gunzip.utf8.cut(t"\n").each: line =>
    Out.print(t" "*19)
    Out.println(line)
  
  val buildId = safely:
    val resource = Classpath / p"build.id"
    resource().readAs[Text].trim
  
  val scalaProperties = unsafely:
    val resource = Classpath / p"compiler.properties"

    resource().readAs[Text].cut(t"\n").flatMap:
      case r"$key([^=]*)=$value(.*)" => List(key -> value)
      case _                         => Nil
    .to(Map)

  case class Software(name: Text, version: Text, copyright: Text)

  val scalaCopyright = scalaProperties(t"copyright.string").sub(t"Copyright ", t"")
  val jvmVersion = unsafely(Properties.java.vm.specification.version())
  val jvmVendor = unsafely(Properties.java.vm.specification.vendor())
  
  val software =
    List
      (Software(t"Fury", t"1.0${buildId.lay(t"") { id => t", build $id"}}", t"2017-2024, Propensive"),
       Software(t"Scala", scalaProperties(t"version.number"), scalaCopyright),
       unsafely(Software(t"Java distribution", Properties.java.version(), Properties.java.vendor())),
       Software(t"Java specification", jvmVersion, jvmVendor))

  Out.println:
    Table[Software]
      (Column(e"$Bold(Component)", textAlign = TextAlignment.Right): software =>
        e"$Bold(${software.name})",
       Column(e"$Bold(Version)")(_.version.display),
       Column(e"$Bold(Copyright)")(_.copyright.display))
    .tabulate(software).layout(72)
  
  Out.println()

  safely:
    Out.println:
      e"  ${Italic}(${Properties.os.name()} ${Properties.os.version()}, ${Properties.os.arch()})\n"
  
  ExitStatus.Ok
