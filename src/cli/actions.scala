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
import aviation.*
import anticipation.*, filesystemInterfaces.galileiApi
import contingency.*
import anthology.*
import escapade.*
import escritoire.*, insufficientSpaceHandling.ignore, tableStyles.default
import ethereal.*
import eucalyptus.*
import exoskeleton.*
import fulminate.*
import galilei.*
import gossamer.*
import guillotine.*
import hieroglyph.*, textMetrics.eastAsianScripts
import iridescence.*, colors.*
import nettlesome.*
import zeppelin.*
import octogenarian.*
import surveillance.*
import parasite.*
import profanity.*
import quantitative.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import turbulence.*
import vacuous.*

def accede(error: Error): UserError = UserError(error.message)
  
case class UserError(userMessage: Message) extends Error(userMessage)
case class InitError(initMessage: Message) extends Error(initMessage)

object actions:
  object install:
    def installInteractive(force: Boolean, noTabCompletions: Boolean)
        (using DaemonService[?],
               Log[Display],
               SystemProperties,
               Environment,
               WorkingDirectory,
               HomeDirectory,
               Effectful,
               CliFrontEnd,
               Terminal,
               Stdio)
            : ExitStatus raises UserError =

      given (UserError fixes InstallError) =
        case InstallError(reason) => UserError(msg"Installation was not possible because $reason.")
      
      given (UserError fixes DismissError) = error => UserError(msg"Installation was aborted.")

      val directories = Installer.candidateTargets().map(_.path)
      
      if directories.length <= 1 then Installer.install(force) else
        info(e"$Italic(Please choose an install location.)")
        
        interactive:
          SelectMenu(directories, directories.head).ask: target =>
            info(e"Installing to $target/${service.scriptName}")
            info(Installer.install(force = true, target).communicate)
      
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

      info(Installer.install(force).communicate)
      if !noTabCompletions then info(TabCompletions.install(force = true).communicate)
      ExitStatus.Ok

  def clean()(using FrontEnd, Installation): ExitStatus raises UserError =
    import filesystemOptions.doNotCreateNonexistent
    import filesystemOptions.doNotDereferenceSymlinks
    import filesystemOptions.deleteRecursively
    val size0 = safely(installation.cache.as[Directory].size()).or(0.b)
    safely(installation.cache.delete())
    val size = safely(size0 - installation.cache.as[Directory].size()).or(0.b)
    info(t"$size was cleaned up")
    ExitStatus.Ok
  
  object cache:
    def clean()(using FrontEnd): ExitStatus raises UserError =
      info(msg"Cleaning the cache")
      Cache.clear()
      ExitStatus.Ok

    def about()(using FrontEnd, Monitor): ExitStatus raises UserError = Cache.about.pipe: cache =>
      info(msg"""The cache contains ${cache.ecosystems} ecosystems, ${cache.snapshots} repository snapshots,
                 ${cache.workspaces} workspaces and ${cache.files} files, totalling ${cache.dataSize}.""")
      ExitStatus.Ok

  object universe:
    def show()(using Internet, WorkingDirectory, Monitor, Log[Display], FrontEnd, Stdio): ExitStatus raises UserError =
      given (UserError fixes PathError)      = accede
      given (UserError fixes CancelError)    = accede
      given (UserError fixes IoError)        = accede
      given (UserError fixes WorkspaceError) = accede
      given (UserError fixes ExecError)      = accede
      given (UserError fixes VaultError)     = accede
        
      val rootWorkspace = Workspace()
      given universe: Universe = rootWorkspace.universe()
      val projects = universe.projects.to(List)

      val table =
        Table[(ProjectId, Definition)]
          (Column(e"$Bold(Project)"): (project, definition) =>
             e"${definition.name}",
             // definition.website.lay(e"${definition.name}"): website =>
             //   e"${escapes.link(website, definition.name)}",
           Column(e"$Bold(ID)")(_(0)),
           Column(e"$Bold(Description)", sizing = columnar.Prose)(_(1).description),
           Column(e"$Bold(Source)"): (_, definition) =>
             definition.source match
               case workspace: Workspace =>
                 e"$Aquamarine(${workspace.directory.path.relativeTo(rootWorkspace.directory.path)})"
               
               case vault: Vault =>
                 e"$DeepSkyBlue(${vault.name})")
      info(table.tabulate(projects))
      ExitStatus.Ok

  object build:
    def initialize(directory: Path)(using CliFrontEnd): ExitStatus raises UserError =
      given (UserError fixes DismissError) = accede
      if (directory / p".fury").exists() then abort(UserError(msg"A build already exists in this directory"))
      interactive:
        Out.print(e"$Italic(Please enter the project name:) ")
        LineEditor(t"hello-world", 0).ask: choice =>
          Out.println(e"You chose $Bold($choice)")
      //val (name, events2) = LineEditor(t"hello-world").ask(events)
      //info(e"Project has the name $name")
      
      ExitStatus.Ok

    def run(target: Target, watch: Boolean, force: Boolean)
       (using CliFrontEnd,
              WorkingDirectory,
              Monitor,
              Log[Display],
              DaemonService[?],
              Internet,
              Installation,
              GitCommand,
              SystemProperties,
              Environment)
            : ExitStatus raises UserError =

      import filesystemOptions.doNotCreateNonexistent
      import filesystemOptions.dereferenceSymlinks
      given (UserError fixes WorkspaceError) = accede
      given (UserError fixes BuildError)     = accede
      given (UserError fixes VaultError)     = accede
      given (UserError fixes CancelError)    = accede
      given (UserError fixes PathError)      = accede
      given (UserError fixes ZipError)       = accede
      given (UserError fixes StreamError)    = accede
      given (UserError fixes IoError)        = accede
      given (UserError fixes WatchError)     = accede
      given (UserError fixes ScalacError)    = accede

      val workspace = Workspace()
      given universe: Universe = workspace.universe()
      
      def build(): Set[Path] =
        info(msg"Starting $target build...")
        val builder = Builder()
        val hash = builder.build(target).await()
        summon[FrontEnd].setSchedule(builder.schedule(hash))
        builder.run(hash, force)
        builder.watchDirectories(hash)
      
      if !watch then build()
      else while summon[FrontEnd].continue do
        build().watch: watches =>
          watches.stream.cluster(0.05*Second).head
          summon[FrontEnd].reset()

      ExitStatus.Ok

  def invalidSubcommand(command: Argument)(using FrontEnd): ExitStatus raises UserError =
    abort(UserError(msg"${command()} is not a valid subcommand."))

  def missingSubcommand()(using FrontEnd): ExitStatus raises UserError =
    abort(UserError(msg"No subcommand was specified."))

  def versionInfo()(using FrontEnd): ExitStatus = 
    info(msg"Fury version 1.0")
    ExitStatus.Ok
