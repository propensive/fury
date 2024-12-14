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
import anthology.*
import anticipation.*, filesystemInterfaces.galileiApi
import aviation.*, calendars.gregorian
import cellulose.*, codlPrinters.standard
import contingency.*
import escapade.*
import escritoire.*, insufficientSpaceHandling.ignore, tableStyles.default
import ethereal.*
import eucalyptus.*
import exoskeleton.*
import fulminate.*
import galilei.*, filesystemOptions.dereferenceSymlinks
import gastronomy.*, alphabets.base32.zBase32Unpadded
import gossamer.*
import guillotine.*
import hieroglyph.*, textMetrics.eastAsianScripts, charEncoders.utf8
import iridescence.*, colors.*
import nettlesome.*
import octogenarian.*
import parasite.*
import profanity.*
import quantitative.*
import rudiments.*
import serpentine.*, pathHierarchies.unixOrWindows
import spectacular.*
import surveillance.*
import turbulence.*
import vacuous.*
import zeppelin.*

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
            : Exit raises UserError =

      tend:
        val directories = Installer.candidateTargets().map(_.path)

        if directories.length <= 1 then Installer.install(force) else
          log(e"$Italic(Please choose an install location.)")

          interactive:
            SelectMenu(directories, directories.head).ask: target =>
              log(e"Installing to $target/${service.scriptName}")
              log(Installer.install(force = true, target).communicate)

        if !noTabCompletions then Out.println(TabCompletions.install(force = true).communicate)

        Exit.Ok

      . remedy:
          case InstallError(message) => abort(UserError(message.communicate))
          case DismissError()        => Exit.Fail(1)

    def batch(force: Boolean, noTabCompletions: Boolean)
       (using DaemonService[?],
              FrontEnd,
              Log[Display],
              SystemProperties,
              Environment,
              WorkingDirectory,
              HomeDirectory,
              Effectful)
            : Exit raises UserError =

      tend:
        log(Installer.install(force).communicate)
        if !noTabCompletions then info(TabCompletions.install(force = true).communicate)
        Exit.Ok

      . remedy:
          case InstallError(message) => abort(UserError(message.communicate))

  def clean(all: Boolean)(using FrontEnd, Installation): Exit raises UserError =
    import filesystemOptions.
       {doNotCreateNonexistent, doNotDereferenceSymlinks, deleteRecursively}

    val size0 = safely(installation.cache.as[Directory].size()).or(0.b)

    if all then safely(installation.cache.delete())
    else
      safely(installation.build.wipe())
      safely(installation.tmp.delete())
      safely(installation.work.wipe())

    val size = safely(size0 - installation.cache.as[Directory].size()).or(0.b)
    log(t"$size was cleaned up")
    Exit.Ok

  object cache:
    def clean()(using FrontEnd): Exit raises UserError =
      log(m"Cleaning the cache")
      Cache.clear()
      Exit.Ok

    def about()(using FrontEnd, Monitor): Exit raises UserError = Cache.about.pipe: cache =>
      info
       (m"""
        The cache contains ${cache.ecosystems} ecosystems, ${cache.snapshots} repository snapshots,
        ${cache.workspaces} workspaces and ${cache.files} files, totalling ${cache.dataSize}.
        """)
      Exit.Ok

  object universe:

    def show()(using Internet, WorkingDirectory, Monitor, Log[Display], FrontEnd, Stdio): Exit raises UserError =
      val rootWorkspace = tend(Workspace()).remedy:
        case WorkspaceError(reason) =>
          abort(UserError(m"The workspace could not be constructed because $reason"))

      given universe: Universe = tend(rootWorkspace.universe()).remedy:
        case PathError(path, _)     => abort(UserError(m"The path $path was not valid"))
        case VaultError()           => abort(UserError(m"There was a problem with the vault"))
        case WorkspaceError(reason) => abort(UserError(m"The workspace could not be constructed because $reason"))
        case IoError(_)             => abort(UserError(m"An I/O error occurred"))
        case ExecError(_, _, _)     => abort(UserError(m"An error occurred when executing"))
        case ConcurrencyError(_)    => abort(UserError(m"An asynchronous task was aborted"))

      val projects = universe.projects.to(List)

      val table = Table[(ProjectId, Definition)]
        (Column(e"$Bold(Project)"): (_, definition) =>
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

      log(table.tabulate(projects))
      Exit.Ok

  object project:
    def publish(projectId: ProjectId, streamId: Optional[StreamId])
       (using WorkingDirectory,
              Stdio,
              Environment,
              SystemProperties,
              GitCommand,
              Log[Display],
              Internet)
            : Exit raises UserError =
      import filesystemOptions.doNotCreateNonexistent

      val build = tend(Workspace().build).remedy:
        case WorkspaceError(reason) =>
          abort(UserError(m"The workspace could not be constructed because $reason"))

      build.projects.where(_.id == projectId).let: project =>
        val directory = safely(workingDirectory).or:
          abort(UserError(m"The working directory could not be determined."))

        tend:
          val repo = GitRepo(directory)
          val commit = repo.revParse(Refspec.head())
          val remote = repo.config.get[HttpUrl](t"remote.origin.url")
          val snapshot = Snapshot(remote, commit, Unset)

          if !repo.status().isEmpty
          then abort(UserError(m"The repository contains uncommitted changes. Please commit the changes and try again."))
          val stream = project.streams.where(_.id == streamId).or(project.streams.unique).or:
            abort(UserError(m"Please specify a stream to publish."))

          val release = project.release(stream.id, stream.lifetime, snapshot).codl.show
          val hash: Text = release.digest[Sha2[256]].encodeAs[Base32]

          val destination =
            unsafely(build.ecosystem.path / p"data" / PathName(hash.take(2)) / PathName(hash.drop(2)))

          release.writeTo:
            import filesystemOptions.{createNonexistent, createNonexistentParents}
            destination.as[File]

          val ecosystemRepo = GitRepo(build.ecosystem.path)
          ecosystemRepo.add(destination)
          ecosystemRepo.commit(t"Added latest ${project.name}")
          ecosystemRepo.push()
        .remedy:
          case PathError(path, _)              => abort(UserError(m"The path $path was not valid"))
          case IoError(_)                      => abort(UserError(m"An I/O error occurred"))
          case StreamError(_)                  => abort(UserError(m"A streaming error occurred"))
          case GitError(_)                     => abort(UserError(m"A Git error occurred"))
          case HostnameError(hostname, reason) => abort(UserError(m"The hostname $hostname is not valid because $reason"))
          case UrlError(url, position, reason) => abort(UserError(m"The URL $url was not valid because $reason at $position"))
          case ExecError(_, _, _)              => abort(UserError(m"An execution error occurred"))
          case ReleaseError(reason)            => abort(UserError(m"There was a problem with the release: $reason"))

        Exit.Ok
      .or:
        Out.println(t"Project $projectId is not defined in this workspace")
        Exit.Fail(1)

  object build:
    def initialize(directory: Path)(using CliFrontEnd): Exit raises UserError =
      if (directory / p".fury").exists()
      then abort(UserError(m"A build already exists in this directory"))

      tend:
        interactive:
          Out.print(e"           $Italic(Project ID:) ")
          LineEditor(directory.name).ask: id =>
            Out.print(e"         $Italic(Project name:) ")
            LineEditor(id.capitalize).ask: name =>
              Out.print(e"  $Italic(Project description:) ")
              LineEditor(t"").ask: description =>
                Out.println(e"You chose $Bold($id), $Bold($name), and $Bold($description)")

        Exit.Ok

      .remedy:
        case DismissError() => Exit.Fail(1)

    def run(target: Target, watch: Boolean, force: Boolean, concise: Boolean)
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
            : Exit raises UserError =

      import filesystemOptions.doNotCreateNonexistent
      import filesystemOptions.dereferenceSymlinks

      Log.info(m"Trying to construct workspace")
      val workspace = tend(Workspace()).remedy:
        case WorkspaceError(_) => abort(UserError(m"The workspace could not be constructed"))

      Log.info(m"Finished constructing workspace")

      given universe: Universe = tend(workspace.universe()).remedy:
        case VaultError()           => abort(UserError(m"Could not generate universe"))
        case WorkspaceError(reason) => abort(UserError(m"Could not generate universe"))
        case ConcurrencyError(_)    => abort(UserError(m"Constructing the universe was interrupted"))

      def build(): Set[Path] =
        Log.info(m"Starting $target build...")
        val builder = Builder()
        Log.info(m"Constructed the builder")

        val hash = tend(builder.build(target).await()).remedy:
          case BuildError(_)       => abort(UserError(m"Could not calculated the build hash"))
          case ConcurrencyError(_) => abort(UserError(m"Calculating the build hash was cancelled"))

        Log.info(m"Calculated hash")
        if !concise then summon[FrontEnd].setSchedule(builder.schedule(hash))
        Log.info(m"Invoking run")

        tend(builder.run(target.show, hash, force)).remedy:
          case StreamError(size)        => abort(UserError(m"A stream error occurred during the build"))
          case CompileError()           => abort(UserError(m"The compiler crashed during the build"))
          case IoError(path)            => abort(UserError(m"A disk error occurred during the build"))
          case ZipError(_)              => abort(UserError(m"There was a ZIP error during the build"))
          case PathError(path, expect)  => abort(UserError(m"An invalid path was encountered during the build"))
          case BuildError(_)            => abort(UserError(m"The build failed."))
          case ConcurrencyError(reason) => abort(UserError(m"The build aborted early"))

        Log.info(m"Returning watch directories")
        builder.watchDirectories(hash)

      if !watch then build()
      else while summon[FrontEnd].continue do
        tend:
          build().watch: watches =>
            watches.stream.cluster(0.05*Second).head
            summon[FrontEnd].reset()
        .remedy:
          case IoError(path)           => abort(UserError(m"An I/O error occurred during filewatching"))
          case WatchError()            => abort(UserError(m"There was an error filewatching"))
          case PathError(text, expect) => abort(UserError(m"An invalid path was encountered"))

      Exit.Ok

  def invalidSubcommand(command: Argument)(using FrontEnd): Exit raises UserError =
    abort(UserError(m"${command()} is not a valid subcommand."))

  def missingSubcommand()(using FrontEnd): Exit raises UserError =
    abort(UserError(m"No subcommand was specified."))

  def versionInfo()(using FrontEnd): Exit =
    log(m"Fury version 1.0")
    Exit.Ok
