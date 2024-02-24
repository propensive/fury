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

import rudiments.*
import profanity.*
import nettlesome.*
import turbulence.*
import kaleidoscope.*
import spectacular.*
import ambience.*, systemProperties.virtualMachine
import exoskeleton.*
import fulminate.*
import hallucination.*
import iridescence.*
import galilei.*
import gastronomy.*
import gossamer.*
import anticipation.*
import parasite.*
import hellenism.*, classloaders.threadContext
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.skip, textMetrics.uniform
import vacuous.*
import eucalyptus.*
import dendrology.*, dagStyles.default
import serpentine.*
import ethereal.*
import escapade.*
import octogenarian.*
import escritoire.*, tableStyles.minimalist
import contingency.*

def accede(error: Error): UserError = UserError(error.message)
  
case class UserError(userMessage: Message) extends Error(userMessage)

def installInteractive
    (force: Boolean, noTabCompletions: Boolean)
    (using DaemonService[?], Terminal, Log[Display], SystemProperties, Environment, WorkingDirectory,
        HomeDirectory, Effectful)
    : ExitStatus raises UserError =
  given (UserError fixes InstallError) =
    case InstallError(reason) => UserError(msg"Installation was not possible because $reason.")
  
  given (UserError fixes DismissError) =
    case DismissError() => UserError(msg"Installation was aborted.")

  val directories = Installer.candidateTargets().map(_.path)
  
  if directories.length <= 1 then Installer.install(force) else
    Out.println(e"$Italic(Please choose an install location.)")
    val menu = SelectMenu(directories, directories.head)
    val (target, events2) = menu.ask(terminal.events)
    Out.println(e"Installing to $target/${service.scriptName}")
    Out.println(Installer.install(force = true, target).communicate)
  
  if !noTabCompletions then Out.println(TabCompletions.install(force = true).communicate)

  ExitStatus.Ok

def installBatch
    (force: Boolean, noTabCompletions: Boolean)
    (using DaemonService[?], Stdio, Log[Display], SystemProperties, Environment, WorkingDirectory, HomeDirectory,
        Effectful)
    : ExitStatus raises UserError =
  given (UserError fixes InstallError) =
    case InstallError(reason) => UserError(msg"Installation was not possible because $reason.")

  Out.println(Installer.install(force).communicate)
  if !noTabCompletions then Out.println(TabCompletions.install(force = true).communicate)
  ExitStatus.Ok

def initializeBuild(directory: Path)(using Stdio): ExitStatus raises UserError =
  Out.println(t"Creating a new build in $directory")
  ExitStatus.Ok

def cleanCache()(using Stdio): ExitStatus raises UserError =
  Out.println(t"Cleaning the cache")
  ExitStatus.Ok

def cacheDetails()(using Stdio): ExitStatus raises UserError =
  Out.println(t"Details of the cache")
  ExitStatus.Ok

def runBuild(ref: ModuleRef)
    (using Stdio, WorkingDirectory, Monitor, Log[Display], Internet, Installation, GitCommand)
    : ExitStatus raises UserError =
  given (UserError fixes WorkspaceError) = error => UserError(error.message)
  given (UserError fixes BuildError)     = error => UserError(error.message)
  given (UserError fixes VaultError)     = error => UserError(error.message)
  given (UserError fixes CancelError)    = error => UserError(error.message)

  val workspace = Workspace()
  given universe: Universe = workspace.universe()
  
  Engine.buildGraph(Engine.build(ref).await()).render: step =>
    e"▪ ${colors.Khaki}(${step.ref.projectId})${colors.Gray}(/)${colors.MediumAquamarine}(${step.ref.moduleId})"
  .each(Out.println(_))
  
  ExitStatus.Ok

def invalidSubcommand(command: Argument)(using Stdio): ExitStatus raises UserError =
  abort(UserError(msg"${command()} is not a valid subcommand."))

def missingSubcommand()(using Stdio): ExitStatus raises UserError =
  abort(UserError(msg"No subcommand was specified."))

def about()(using Stdio): ExitStatus =
  safely(Out.println(Image((Classpath / p"logo.png")()).render))
  val asciiArt = t"H4sIAAAAAAAA/31Ryw3AIAi9O8UbtfHcQw8wRrUzMUmTKlSx1HgA3ocXFT6FtulySUIZEIO49gllLcjIA62MmgkY3UO"+
      t"Beu+2VrdCCxfsm2RhAQQOD7aCq5KvtiTQTnDqbZ/gbf0LV8dcqUdzxN+x1CHBfa7mjPlh4HQDGOnRlikCAAA="

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

  Table[Software](
    Column(e"$Bold(Component)", align = Alignment.Right): software =>
      e"$Bold(${software.name})",
    Column(e"$Bold(Version)")(_.version.display),
    Column(e"$Bold(Copyright)")(_.copyright.display)
  ).tabulate(List(
    Software(t"Fury", t"1.0${buildId.lay(t"") { id => t", build $id"}}", t"2017-2024, Propensive"),
    Software(t"Scala", scalaProperties(t"version.number"), scalaProperties(t"copyright.string").sub(t"Copyright ", t"")),
    unsafely(Software(t"Java distribution", Properties.java.version(), Properties.java.vendor())),
    unsafely(Software(t"Java specification", Properties.java.vm.specification.version(), Properties.java.vm.specification.vendor()))
  ), 72).each(Out.println(_))

  safely(Out.println(e"  ${Italic}(${Properties.os.name()} ${Properties.os.version()}, ${Properties.os.arch()})\n"))
  
  ExitStatus.Ok

def versionInfo()(using Stdio): ExitStatus = 
  Out.println(t"Fury version 1.0")
  ExitStatus.Ok

