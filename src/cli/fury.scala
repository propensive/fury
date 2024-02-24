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
import galilei.*, filesystemInterfaces.galileiApi, filesystemOptions.dereferenceSymlinks
import parasite.*, threadModels.platform
import gossamer.*
import escapade.*
import guillotine.*
import eucalyptus.*
import aviation.*
import iridescence.*, colors.*
import fulminate.*
import rudiments.*, homeDirectories.virtualMachine
import vacuous.*
import hieroglyph.*, charEncoders.utf8
import nettlesome.*
import serpentine.*, hierarchies.unixOrWindows
import punctuation.*
import spectacular.*
import exoskeleton.*, executives.completions, unhandledErrors.stackTrace, parameterInterpretation.posix
import contingency.*
import ethereal.*, daemonConfig.supportStderr
import profanity.*, terminalOptions.terminalSizeDetection
import turbulence.*

enum BusMessage:
  case Ping

given Realm = realm"fury"

object userInterface:
  val Version = Switch(t"version", false, List('v'), t"Show information about the current version of Fury")
  val Interactive = Switch(t"interactive", false, List('i'), t"Run the command interactively")
  val NoTabCompletions = Switch(t"no-tab-completions", false, Nil, t"Do not install tab completions")
  val Artifact = Switch(t"artifact", false, List('a'), t"Produce an artifact")
  val Benchmarks = Switch(t"benchmark", false, List('b'), t"Run with OS settings for benchmarking")
  val Discover = Switch(t"discover", false, List('D'), t"Try to discover build details from the directory")
  val Force = Switch(t"force", false, List('F'), t"Overwrite existing files if necessary")
  def Dir(using Raises[PathError]) = Flag[Path](t"dir", false, List('d'), t"Specify the working directory")
  val Offline = Switch(t"offline", false, List('o'), t"Work offline, if possible")
  def Generation(using Raises[NumberError]) = Flag[Int](t"generation", false, List('g'), t"Use universe generation number")

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
  import userInterface.*
  import unsafeExceptions.canThrowAny

  throwErrors[CancelError]:
    supervise:
      given logFormat: LogFormat[File, Display] = logFormats.standardColor[File]
      given logFormat2: LogFormat[Err.type, Display] = logFormats.standardColor[Err.type]
      import filesystemOptions.{createNonexistent, createNonexistentParents}
      
      given Log[Display] = throwErrors[UserError]:
        given (UserError fixes IoError)             = error => UserError(msg"An IO error occured when trying to create the log")
        given (UserError fixes StreamError)         = error => UserError(msg"Stream error when logging")
        
        Log.route: 
          case _ => installation.config.log.path.as[File]

      daemon[BusMessage]:
        try throwErrors[UserError]:
          if Version().present then execute(versionInfo()) else arguments match
            case Install() :: _ =>
              val interactive = Interactive().present
              val force = Force().present
              val noTabCompletions = NoTabCompletions().present
              
              execute:
                try throwErrors[UserError]:
                  if interactive then terminal(installInteractive(force, noTabCompletions))
                  else installBatch(force, noTabCompletions)
                  
                  service.shutdown()
                  ExitStatus.Ok
                
                catch
                  case userError: UserError =>
                    Out.println(userError.message)
                    ExitStatus.Fail(1)

            case Init() :: Nil =>
              val dir: Optional[Path] = safely(Dir()).or(safely(workingDirectory))
              val discover = Discover()
              execute:
                dir.let(initializeBuild(_)).or:
                  abort(UserError(msg"The working directory could not be determined."))

            case Config() :: Nil =>
              execute:
                Out.println(installation.config.debug)
                ExitStatus.Ok

            case Cache() :: subcommands => subcommands match
              case Clean() :: Nil   => execute(cleanCache())
              case Details() :: Nil => execute(cacheDetails())
              case other :: _       => execute(other.let(invalidSubcommand(_)).or(missingSubcommand()))
              case Nil              => execute:
                Out.println(msg"Please specify a subcommand.")
                ExitStatus.Fail(1)

            case About() :: _ =>
              execute:
                about()
                ExitStatus.Ok
            
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
                    runBuild(target().decodeAs[ModuleRef])
                    Out.println(t"TODO: Build ${target.let(_()).or(t"?")}")
                    ExitStatus.Fail(1)
                
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
                  terminal:
                    internet(online):
                      showUniverse()

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
        catch
          case userError: UserError =>
            execute:
              Out.println(userError.message)
              ExitStatus.Fail(1)


