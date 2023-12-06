/*
    Fury, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import ambience.*, systemProperties.jvm, environments.jvm
import anticipation.*
import cellulose.*
import galilei.*, fileApi.galileiApi, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import gastronomy.*
import parasite.*
import gossamer.*
import escapade.*
import guillotine.*
import eucalyptus.*, logFormats.standardColor
import aviation.*
import iridescence.*, colors.*
import fulminate.*
import rudiments.*, homeDirectories.jvm, workingDirectories.jvm
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8
import nonagenarian.*
import nettlesome.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import punctuation.*
import escritoire.*, tableStyles.horizontal, textWidthCalculation.eastAsianScripts
import spectacular.*
import exoskeleton.*, executives.completions, unhandledErrors.stackTrace, parameterInterpretation.posix
import perforate.*
import spectral.*, daemonConfig.supportStderr
import profanity.*, terminalOptions.terminalSizeDetection
import turbulence.*

export gitCommands.environmentDefault

//import language.experimental.captureChecking

erased given CanThrow[AppError] = ###

given (using CanThrow[AppError]): Raises[AggregateError[Error]] =
  new Raises[AggregateError[Error]]:
    def record(error: AggregateError[Error]): Unit = throw AppError(error.message, error)
    def abort(error: AggregateError[Error]): Nothing = throw AppError(error.message, error)

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

  val About = Subcommand(t"about", e"About Fury")
  val Build = Subcommand(t"build", e"Start a new build (default)")
  val Cache = Subcommand(t"cache", e"Cache operations")
  val Shutdown = Subcommand(t"shutdown", e"Shutdown the Fury daemon")
  val Init = Subcommand(t"init", e"Initialize a new project")
  val Universe = Subcommand(t"universe", e"Universe actions")
  val Update = Subcommand(t"update", e"Update Fury")
  val Install = Subcommand(t"install", e"Install Fury")

  val Clean = Subcommand(t"clean", e"Clean the cache")
  val Details = Subcommand(t"info", e"Information about cache usage")

given (using Raises[UserError]): HomeDirectory =
  mitigate:
    case SystemPropertyError(property) =>
      UserError(msg"Could not access the home directory because the $property system property was not set.")
  .within(homeDirectories.jvm)

given (using Cli): WorkingDirectory = workingDirectories.daemonClient 

@main
def main(): Unit =
  System.out.nn.println("JVM starting")
  import userInterface.*
  import unsafeExceptions.canThrowAny
  throwErrors[CancelError]:
    supervise:
      given Log[Output] = logging.silent[Output]

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
                  import logging.pinned
                  
                  if interactive then terminal(installInteractive(force, noTabCompletions))
                  else installBatch(force, noTabCompletions)
                  
                  service.shutdown()
                  ExitStatus.Ok
                
                catch
                  case userError: UserError =>
                    Out.println(userError.message)
                    ExitStatus.Fail(1)

            case Init() =>
              val dir: Maybe[Path] = safely(Dir()).or(safely(workingDirectory))
              val discover = Discover()
              execute:
                dir.let(initializeBuild(_)).or:
                  abort(UserError(msg"The working directory could not be determined."))

            case Cache() => safely(arguments.tail.head) match
              case Clean()   => execute(cleanCache())
              case Details() => execute(cacheDetails())
              case other     => execute(other.let(invalidSubcommand(_)).or(missingSubcommand()))

            case About() => //execute(about())
              execute:
                Err.println(t"This is stderr")
                ExitStatus.Fail(1)
            
            case Build() => execute(runBuild())
            
            case Shutdown() => execute:
              service.shutdown()
              ExitStatus.Ok
            
            case Universe() =>
            
              val offline = Offline().present
              execute:
                import unsafeExceptions.canThrowAny
                throwErrors[PathError | SystemPropertyError | CancelError | HostnameError | CodlReadError | UrlError |
                    GitRefError | StreamCutError | IoError | InvalidRefError | NumberError | NotFoundError | GitError |
                    UndecodableCharError | UnencodableCharError | MarkdownError | ExecError | DateError]:
                  internet(!offline):
                    frontEnd:
                      given installation: Installation = Installation((Xdg.cacheHome[Path] / p"fury").as[Directory])
                      val rootWorkspace = Workspace(Properties.user.dir())
                      given universe: Universe = rootWorkspace.universe()
                      val projects = universe.projects.to(List)

                      // val terminfo = sh"infocmp -0 -L -q -t ${Environment.term}".exec[Text]().cut(t",").
                      // Out.println(terminfo.debug)
  
                      terminal:
                        Async(tty.events.foreach(_ => ()))
                        
                        Table[(ProjectId, Definition)](
                          Column(e"$Bold(Project ID)")(_(0)),
                          Column(e"$Bold(Name)")(_(1).name),
                          Column(e"$Bold(Description)")(_(1).description),
                          Column(e"$Bold(Website)")(_(1).website.let(_.show).or(t"—")),
                          Column(e"$Bold(Source)"): (_, definition) =>
                            definition.source match
                              case workspace: Workspace => e"$Aquamarine(${rootWorkspace.directory.path.relativeTo(workspace.directory.path)})"
                              case vault: Vault         => e"$SeaGreen(${vault.name})"
                        ).tabulate(projects, tty.knownColumns, DelimitRows.SpaceIfMultiline).foreach(Out.println(_))
  
                        ExitStatus.Ok
            
            case subcommand =>
              execute:
                Out.println(t"Unrecognized subcommand: ${subcommand.let(_()).or(t"")}.")
                ExitStatus.Fail(1)
        catch
          case userError: UserError =>
            execute:
              Out.println(userError.message)
              ExitStatus.Fail(1)
