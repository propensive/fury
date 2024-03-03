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

import ambience.*
import anticipation.*
import contingency.*
import dendrology.*, dagStyles.default
import escapade.*
import escritoire.*, insufficientSpaceHandling.ignore, tableStyles.default
import ethereal.*
import eucalyptus.*
import exoskeleton.*
import fulminate.*
import galilei.*
import guillotine.*
import hieroglyph.*, textMetrics.eastAsianScripts
import iridescence.*, colors.*
import nettlesome.*
import octogenarian.*
import parasite.*
import profanity.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import turbulence.*
import vacuous.*

def accede(error: Error): UserError = UserError(error.message)
  
case class UserError(userMessage: Message) extends Error(userMessage)

object actions:
  object install:
    def interactive(force: Boolean, noTabCompletions: Boolean)
        (using DaemonService[?],
               Log[Display],
               SystemProperties,
               Environment,
               WorkingDirectory,
               HomeDirectory,
               Effectful,
               FrontEnd,
               Terminal,
               Stdio)
            : ExitStatus raises UserError =

      given (UserError fixes InstallError) =
        case InstallError(reason) => UserError(msg"Installation was not possible because $reason.")
      
      given (UserError fixes DismissError) = error => UserError(msg"Installation was aborted.")

      val directories = Installer.candidateTargets().map(_.path)
      
      if directories.length <= 1 then Installer.install(force) else
        inform(e"$Italic(Please choose an install location.)")
        val menu = SelectMenu(directories, directories.head)
        val (target, events2) = menu.ask(terminal.events)
        inform(e"Installing to $target/${service.scriptName}")
        inform(Installer.install(force = true, target).communicate)
      
      if !noTabCompletions then Out.println(TabCompletions.install(force = true).communicate)

      ExitStatus.Ok

    def batch(force: Boolean, noTabCompletions: Boolean)
        (using DaemonService[?],
               FrontEnd,
               Log[Display],
               SystemProperties,
               Environment,
               WorkingDirectory,
               HomeDirectory,
               Effectful)
            : ExitStatus raises UserError =

      given (UserError fixes InstallError) =
        case InstallError(reason) => UserError(msg"Installation was not possible because $reason.")

      inform(Installer.install(force).communicate)
      if !noTabCompletions then inform(TabCompletions.install(force = true).communicate)
      ExitStatus.Ok

  object cache:
    def clean()(using FrontEnd): ExitStatus raises UserError =
      inform(msg"Cleaning the cache")
      ExitStatus.Ok

    def info()(using FrontEnd): ExitStatus raises UserError =
      inform(msg"Details of the cache usage")
      ExitStatus.Ok

  object universe:
    def show()(using Internet, WorkingDirectory, Monitor, Log[Display], FrontEnd): ExitStatus raises UserError =

      given (UserError fixes PathError)      = accede
      given (UserError fixes CancelError)    = accede
      given (UserError fixes IoError)        = accede
      given (UserError fixes WorkspaceError) = accede
      given (UserError fixes ExecError)      = accede
      given (UserError fixes VaultError)     = accede
        
      internet(online):
        val rootWorkspace = Workspace()
        given universe: Universe = rootWorkspace.universe()
        val projects = universe.projects.to(List)

        //Async(terminal.events.each(_ => ()))
          
        val table =
          Table[(ProjectId, Definition)]
            (Column(e"$Bold(Project)"): (project, definition) =>
               e"${definition.name}",
               // definition.website.lay(e"${definition.name}"): website =>
               //   e"${escapes.link(website, definition.name)}",
             Column(e"$Bold(ID)")(_(0)),
             Column(e"$Bold(Description)")(_(1).description),
             Column(e"$Bold(Source)"): (_, definition) =>
               definition.source match
                 case workspace: Workspace => e"$Aquamarine(${rootWorkspace.directory.path.relativeTo(workspace.directory.path)})"
                 case vault: Vault         => e"$DeepSkyBlue(${vault.name})")
        
        inform(table.tabulate(projects))

        ExitStatus.Ok

  object build:
    def initialize(directory: Path)(using FrontEnd): ExitStatus raises UserError =
      inform(msg"Creating a new build in $directory")
      ExitStatus.Ok

    def run(ref: ModuleRef)
        (using FrontEnd, WorkingDirectory, Monitor, Log[Display], Internet, Installation, GitCommand)
            : ExitStatus raises UserError =
      given (UserError fixes WorkspaceError) = accede
      given (UserError fixes BuildError)     = accede
      given (UserError fixes VaultError)     = accede
      given (UserError fixes CancelError)    = accede

      val workspace = Workspace()
      given universe: Universe = workspace.universe()
      
      inform:
        Engine.buildGraph(Engine.build(ref).await())
      //  .render: step =>
      //    e"▪ ${Khaki}(${step.ref.projectId})${Gray}(/)${MediumAquamarine}(${step.ref.moduleId})"
      
      ExitStatus.Ok

  def invalidSubcommand(command: Argument)(using FrontEnd): ExitStatus raises UserError =
    abort(UserError(msg"${command()} is not a valid subcommand."))

  def missingSubcommand()(using FrontEnd): ExitStatus raises UserError =
    abort(UserError(msg"No subcommand was specified."))

  def versionInfo()(using FrontEnd): ExitStatus = 
    inform(msg"Fury version 1.0")
    ExitStatus.Ok
