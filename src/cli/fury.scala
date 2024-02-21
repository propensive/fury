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
import escritoire.*, tableStyles.horizontal, textMetrics.eastAsianScripts
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
given installation(using Raises[ConfigError], Raises[SystemPropertyError]): Installation = Installation()

@main
def main(): Unit =
  import userInterface.*
  import unsafeExceptions.canThrowAny

  throwErrors[CancelError]:
    supervise:
      given logFormat: LogFormat[File, Output] = logFormats.standardColor[File]
      given logFormat2: LogFormat[Err.type, Output] = logFormats.standardColor[Err.type]
      import filesystemOptions.{createNonexistent, createNonexistentParents}
      
      inline given Log[Output] = throwErrors[UserError]:
        given (UserError fixes IoError)             = error => UserError(msg"An IO error occured when trying to create the log")
        given (UserError fixes StreamError)         = error => UserError(msg"Stream error when logging")
        given (UserError fixes ConfigError)         = error => UserError(msg"The configuration was not valid")
        given (UserError fixes SystemPropertyError) = error => UserError(msg"The system property ${error.property} was not valid")
      
        compiletime.summonFrom:
          case given Stdio => Log.route:
            case _            => installation.config.log.path.as[File]
            case Level.Warn() => Err
          
          case _ => Log.route:
            case _ => installation.config.log.path.as[File]

      daemon[BusMessage]:
        try throwErrors[UserError]:
          if Version().present then execute(versionInfo())
          else safely(arguments.head) match
            case Install() =>
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

            case Init() =>
              val dir: Optional[Path] = safely(Dir()).or(safely(workingDirectory))
              val discover = Discover()
              execute:
                dir.let(initializeBuild(_)).or:
                  abort(UserError(msg"The working directory could not be determined."))

            case Config() =>
              execute:
                given (UserError fixes SystemPropertyError) =
                  case SystemPropertyError(property) => UserError(msg"The system property could not be read: $property")
                
                given (UserError fixes ConfigError) =
                  case ConfigError(message) => UserError(msg"The configuration could not be read: $message")
                
                Out.println(installation.config.debug)
                
                ExitStatus.Ok

            case Cache() => safely(arguments.tail.head) match
              case Clean()   => execute(cleanCache())
              case Details() => execute(cacheDetails())
              case other     => execute(other.let(invalidSubcommand(_)).or(missingSubcommand()))

            case About() =>
              execute:
                about()
                ExitStatus.Ok
            
            case Build() =>
              safely(arguments.tail.head) match
                case Unset =>
                  execute:
                    Out.println(t"Module has not been specified")
                    ExitStatus.Fail(1)

                case target =>
                  safely(internet(false)(Workspace().locals())).let: map =>
                    val refs = map.values.map(_.source).flatMap:
                      case workspace: Workspace => workspace.build.projects.flatMap: project =>
                        project.modules.map: module =>
                          ModuleRef(project.id, module.id)

                    target.let(_.suggest(previous ++ refs.map(_.suggestion)))
                  
                  execute:
                    Out.println(t"TODO: Build ${target.let(_()).or(t"?")}")
                    ExitStatus.Fail(1)
                
            case Graph() =>
              val online = Offline().absent

              execute:
                given (UserError fixes PathError)           = error => UserError(error.message)
                given (UserError fixes SystemPropertyError) = error => UserError(error.message)
                given (UserError fixes CancelError)         = error => UserError(error.message)
                given (UserError fixes IoError)             = error => UserError(error.message)
                given (UserError fixes NumberError)         = error => UserError(error.message)
                given (UserError fixes ConfigError)         = error => UserError(error.message)
                given (UserError fixes WorkspaceError)      = error => UserError(error.message)
                given (UserError fixes ExecError)           = error => UserError(error.message)
                given (UserError fixes VaultError)          = error => UserError(error.message)
                
                internet(online):
                  val rootWorkspace = Workspace()
                  given universe: Universe = rootWorkspace.universe()
                
                ExitStatus.Ok
            
            case Universe() =>
              val online = Offline().absent
              val generation: Optional[Int] = safely(Generation())
              
              safely(arguments.tail.head) match
                case UniverseSearch() =>
                  execute:
                    Out.println(t"TODO: Search the universe")
                    ExitStatus.Ok
                
                case UniverseUpdate() =>
                  execute:
                    Out.println(t"TODO: Update the universe")
                    ExitStatus.Fail(1)

                case UniverseShow() | Unset =>
                  execute:
                    given (UserError fixes PathError)           = error => UserError(error.message)
                    given (UserError fixes SystemPropertyError) = error => UserError(error.message)
                    given (UserError fixes CancelError)         = error => UserError(error.message)
                    given (UserError fixes IoError)             = error => UserError(error.message)
                    given (UserError fixes ConfigError)         = error => UserError(error.message)
                    given (UserError fixes WorkspaceError)      = error => UserError(error.message)
                    given (UserError fixes ExecError)           = error => UserError(error.message)
                    given (UserError fixes VaultError)          = error => UserError(error.message)
                      
                    internet(online):
                      val rootWorkspace = Workspace(workingDirectory)
                      given universe: Universe = rootWorkspace.universe()
                      val projects = universe.projects.to(List)
    
                      terminal:
                        Async(terminal.events.each(_ => ()))
                          
                        val table = Table[(ProjectId, Definition)](
                          Column(e"$Bold(Project)"): (project, definition) =>
                            e"${definition.name}",
                            // definition.website.lay(e"${definition.name}"): website =>
                            //   e"${escapes.link(website, definition.name)}",
                          Column(e"$Bold(ID)")(_(0)),
                          Column(e"$Bold(Description)")(_(1).description),
                          Column(e"$Bold(Source)"): (_, definition) =>
                            definition.source match
                              case workspace: Workspace => e"$Aquamarine(${rootWorkspace.directory.path.relativeTo(workspace.directory.path)})"
                              case vault: Vault         => e"$DeepSkyBlue(${vault.name})"
                        )
                        
                        table.tabulate(projects, terminal.knownColumns, DelimitRows.SpaceIfMultiline)
                          .each(Out.println(_))
    
                        ExitStatus.Ok

                case command => execute:
                  Out.println(e"Command $Italic(${command.vouch(using Unsafe)()}) was not recognized.")
                  ExitStatus.Fail(1)
                
            case Shutdown() => execute:
              service.shutdown()
              ExitStatus.Ok
            
            case subcommand =>
              safely(Workspace()).let: workspace =>
                subcommand.let(_.suggest(previous ++ workspace.build.actions.map(_.suggestion)))
              
              execute:
                safely(Workspace()).let: workspace =>
                  Out.println(workspace.debug)
  
                  Out.println(t"Unrecognized subcommand: ${subcommand.let(_()).or(t"")}.")
                  ExitStatus.Fail(1)
                .or:
                  ExitStatus.Fail(2)
        catch
          case userError: UserError =>
            execute:
              Out.println(userError.message)
              ExitStatus.Fail(1)

