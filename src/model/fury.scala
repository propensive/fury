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
import rudiments.*, homeDirectory.jvm, workingDirectory.jvm
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
import spectral.*
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

val logo: Text = t"CiAgICAgICAgICAgICAgIBtbMzg7MjsyMTc7MTsxMDFtwrcg4oCiIOKXjyAbWzM4OzI7MjA7NjA7MTAwbeKXjyDil48g4oCiIMK3CiAgICAgICAgIBtbMzg7MjsyMTc7MTsxMDFtwrcg4pePIOKXjyAbWzM4OzI7MTEyOzQ7NDNt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDCtwogICAgICAgIBtbMzg7MjsyMjc7MTs5MW3igKIg4pePIOKXjyAbWzM4OzI7MTEyOzQ7NDNt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyAbWzM4OzI7MjEwOzExNTs3beKXjyDil48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDigKIgICAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ila3ilIDilIDilIDilIDilIDila4KICAgICAgIBtbMzg7MjsyMjc7MTs5MW3il48g4pePIOKXjyDil48gG1szODsyOzE2OzUyOzkwbeKXjyAbWzM4OzI7ODM7NDk7Mm3il48gG1szODsyOzIxMDsxMTU7N23il48g4pePIOKXjyAbWzM4OzI7MDszMzszNm3il48gG1szODsyOzE2OzUyOzkwbeKXjyDil48g4pePIOKXjyDil48gICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKUgiAg4pWt4pSA4pSA4pWvCiAgICAgIBtbMzg7MjsyMjc7MTs5MW3il48g4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsyMTA7MTE1Ozdt4pePIOKXjyDil48g4pePIBtbMzg7MjswOzMzOzM2beKXjyAbWzM4OzI7MTY7NTI7OTBt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxNjs1Mjs5MG3il48g4pePIOKXjyAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ilIIgIOKUggogICAgIBtbMzg7MjsyMzc7MTs4MW3igKIg4pePIOKXjyDil48gG1szODsyOzIyODsxMDQ7OW3il48gG1szODsyOzIxMDsxMTU7N23il48g4pePIOKXjyDil48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7MjsxNDs0ODs4NW3il48g4pePIBtbMzg7MjsxNzA7MTY4OzMxbeKXjyDil48gG1szODsyOzE0OzQ4Ozg1beKXjyDil48g4oCiICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilJTilIDilIDila7ila3ilIDilIDila4g4pWt4pSA4pSA4pWu4pWt4pSA4pSA4pWu4pSA4pSA4pSA4pWu4pWt4pSA4pSA4pWuIOKVreKUgOKUgOKVrgogICAgG1szODsyOzIzNzsxOzgxbcK3IOKXjyDil48gG1szODsyOzIxMTs0NDsxN23il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIBtbMzg7MjsyMTA7MTE1Ozdt4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsxNDs0ODs4NW3il48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxNDs0ODs4NW3il48g4pePIOKXjyDCtyAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIzilIDilIDila/ilIIgIOKUgiDilIIgIOKUguKUgiAg4pWt4pSA4pSA4pSA4pWv4pSCICDilIIg4pSCICDilIIKICAgICAbWzM4OzI7MjM3OzE7ODFt4pePIOKXjyAbWzM4OzI7MjI4OzEwNDs5beKXjyDil48g4pePIOKXjyAbWzM4OzI7MjEwOzExNTs3beKXjyDil48gG1szODsyOzIzMDsxNzU7OG3il48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyDil48gG1szODsyOzY4Ozc2OzI2beKXjyAbWzM4OzI7MTI7NDQ7ODBt4pePIOKXjyDil48g4pePICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIIgICDilIIgIOKUgiDilIIgIOKUguKUgiAg4pSCICAgIOKUgiAg4pSCIOKUgiAg4pSCCiAgICAbWzM4OzI7MjQ3OzE7NzFtwrcg4pePIBtbMzg7MjsyMjg7MTA0Ozlt4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIBtbMzg7MjsxNzA7MTY4OzMxbeKXjyDil48g4pePIBtbMzg7Mjs2ODs3NjsyNm3il48gG1szODsyOzEyOzQ0OzgwbeKXjyDil48g4pePIOKXjyDCtyAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIIgICDilIIgIOKVsOKUgOKVryAg4pSC4pSCICDilIIgICAg4pSCICDilbDilIDila8gIOKUggogICAgIBtbMzg7MjsyNDc7MTs3MW3igKIgG1szODsyOzIxMTs0NDsxN23il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIOKXjyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIOKXjyDil48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxMDs0MDs3NW3il48g4pePIOKXjyDil48g4oCiICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pWw4pSA4pSA4pWvICAg4pWw4pSA4pSA4pSA4pSA4pSA4pSA4pSA4pWv4pWw4pSA4pSA4pWvICAgIOKVsOKUgOKUgOKUgOKUgOKUkCAg4pSCCiAgICAgIBtbMzg7MjsyNDc7MTs3MW3il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIBtbMzg7MjsyMzA7MTc1Ozht4pePIBtbMzg7MjsyNDc7MTgxOzI0beKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48g4pePIBtbMzg7MjswOzMzOzM2beKXjyAbWzM4OzI7MTA7NDA7NzVt4pePIOKXjyDil48g4pePIOKXjyAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKUgiAg4pSCCiAgICAgICAbWzM4OzI7MjI4OzEwNDs5beKXjyDil48gG1szODsyOzIzMDsxNzU7OG3il48gG1szODsyOzI0NzsxODE7MjRt4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7Mjs4OzM2OzcwbeKXjyDil48g4pePIOKXjyDil48gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKVreKUgOKUgOKUgOKUgOKVryAg4pSCCiAgICAgICAgG1szODsyOzIyODsxMDQ7OW3igKIg4pePIBtbMzg7MjsyMzA7MTc1Ozht4pePIBtbMzg7MjsyNDc7MTgxOzI0beKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48gG1szODsyOzg7MzY7NzBt4pePIOKXjyDil48g4pePIOKXjyDigKIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ilbDilIDilIDilIDilIDilIDilIDilIDila8KICAgICAgICAgG1szODsyOzIyODsxMDQ7OW3CtyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7NjszMjs2NW3il48g4pePIOKXjyDil48g4pePIMK3CiAgICAgICAgICAgICAgIBtbMzg7MjsyNDc7MTgxOzI0bcK3IOKAoiDil48gG1szODsyOzE1MDsxMTc7MzRt4pePIBtbMzg7Mjs0OzI4OzYwbeKXjyDigKIgwrcKG1swbQo=".decode[Base64].uString

given Realm = realm"fury"

object userInterface:
  val Version = Switch(t"version", false, List('v'), t"Show information about the current version of Fury")
  val Install = Switch(t"install", false, List('I'), t"Install tab-completions for the fury command")
  val Interactive = Switch(t"interactive", false, List('i'), t"Run the command interactively")
  val Artifact = Switch(t"artifact", false, List('a'), t"Produce an artifact")
  val Benchmarks = Switch(t"benchmark", false, List('b'), t"Run with OS settings for benchmarking")

  val About = Subcommand(t"about", e"About Fury")
  val Build = Subcommand(t"build", e"Start a new build (default)")
  val Cache = Subcommand(t"cache", e"Cache operations")
  val Shutdown = Subcommand(t"shutdown", e"Shutdown the Fury daemon")
  val Init = Subcommand(t"init", e"Initialize a new project")
  val Universe = Subcommand(t"universe", e"Universe actions")
  val Update = Subcommand(t"update", e"Update Fury")
  val Vent = Subcommand(t"vent", e"Access the Vent Ecosystem")

  val Clean = Subcommand(t"clean", e"Clean the cache")
  val Details = Subcommand(t"info", e"Information about cache usage")

@main
def main(): Unit =
  import userInterface.*
  import unsafeExceptions.canThrowAny
  throwErrors[CancelError]:
    supervise:
      given Log[Output] = logging.silent[Output]

      daemon[BusMessage]:
        if !Version().unset then
          execute:
            Out.println(e"$Bold(Fury, version 1.0)")
            ExitStatus.Ok
        
        else safely(arguments.head) match
          case Update() =>
            Install()
            
            execute:
              if !Install().unset then
                import workingDirectories.daemonClient
                import unsafeExceptions.canThrowAny
                import logging.silent
                
                throwErrors[ExecError | PathError | IoError | StreamCutError | OverwriteError]:
                  Out.println(TabCompletions.install().communicate)
                
                ExitStatus.Ok
              
              else
                Out.println(t"This does nothing without the --install/-I flag.")
                ExitStatus.Fail(1)
          case Init() => execute:
            Out.println(t"Creating a new build")
            ExitStatus.Ok

          case Vent() => execute:
            Out.println(e"${colors.Goldenrod}(Vent your Fury!)")
            ExitStatus.Ok

          case Cache() => safely(arguments.tail.head) match
            case Clean() =>
              execute:
                Out.println(t"Clean the cache")
                ExitStatus.Ok
            
            case Details() =>
              execute:
                Out.println(t"Details of the cache")
                ExitStatus.Ok

          case About() => execute:
            Out.println(logo)
            ExitStatus.Ok
          
          case Build() => execute:
            Out.println(t"Running the build")
            ExitStatus.Ok
          
          case Shutdown() => execute:
            service.shutdown()
            ExitStatus.Ok
          
          case Universe() => execute:
            import unsafeExceptions.canThrowAny
            throwErrors[PathError | SystemPropertyError | CancelError | HostnameError | CodlReadError | UrlError |
                GitRefError | StreamCutError | IoError | InvalidRefError | NumberError | NotFoundError | GitError |
                UndecodableCharError | UnencodableCharError | MarkdownError | ExecError | DateError]:
              internet:
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
                      Column(e"$Bold(Website)")(_(1).website.mm(_.show).or(t"—")),
                      Column(e"$Bold(Source)"): (_, definition) =>
                        definition.source match
                          case workspace: Workspace => e"$Aquamarine(${rootWorkspace.directory.path.relativeTo(workspace.directory.path)})"
                          case vault: Vault         => e"$SeaGreen(${vault.name})"
                    ).tabulate(projects, tty.knownColumns, DelimitRows.SpaceIfMultiline).foreach(Out.println(_))

                    ExitStatus.Ok
          
          case subcommand =>
            execute:
              Out.println(t"Unrecognized subcommand: ${subcommand.mm(_()).or(t"")}.")
              ExitStatus.Fail(1)
