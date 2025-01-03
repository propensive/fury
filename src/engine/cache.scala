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

import anticipation.*, filesystemApi.{galileiFile, galileiDirectory, galileiPath}, durationApi.aviationDuration, instantApi.aviationInstant
import aviation.*, calendars.gregorian
import cellulose.*
import eucalyptus.*

import galilei.*,
    filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents}

import hieroglyph.*, charDecoders.utf8, textSanitizers.skip
import nettlesome.*
import octogenarian.*
import parasite.*, orphanDisposal.cancel
import contingency.*
import guillotine.*
import fulminate.*
import punctuation.*
import gossamer.*
import digression.*
import gastronomy.*
import escapade.*
import rudiments.*
import vacuous.*
import serpentine.*, pathHierarchies.unixOrWindows
import spectacular.*
import turbulence.*

import fulminate.errorDiagnostics.stackTraces

import scala.collection.concurrent as scc

given Decimalizer = Decimalizer(2)
given Timezone = tz"Etc/UTC"

case class CachedFile(lastModified: Instant, text: Task[Text], hash: Task[Hash])

case class CacheInfo
   (ecosystems: Int, snapshots: Int, workspaces: Int, files: Int, dataSize: Memory)

object Cache:
  private val ecosystems: scc.TrieMap[Ecosystem, Task[Vault]] = scc.TrieMap()
  private val snapshots: scc.TrieMap[Snapshot, Task[Directory]] = scc.TrieMap()
  private val workspaces: scc.TrieMap[Path, (Instant, Task[Workspace])] = scc.TrieMap()
  private val files: scc.TrieMap[Path, CachedFile] = scc.TrieMap()
  private val locals: scc.TrieMap[Workspace, Task[Map[ProjectId, Definition]]] = scc.TrieMap()

  given supervisor: Supervisor = PlatformSupervisor

  def file(path: Path)
      : CachedFile raises IoError raises StreamError raises ConcurrencyError logs Message =

    val file = path.as[File]
    val lastModified = file.lastModified

    def cachedFile() =
      val task = async(file.read[Text])
      CachedFile(lastModified, task, task.map(_.digest[Sha2[256]]))

    val cached = files.establish(path)(cachedFile())

    if cached.lastModified == lastModified then cached else cachedFile().tap: entry =>
      files(path) = entry

  def clear(): Unit =
    ecosystems.clear()
    snapshots.clear()
    workspaces.clear()
    files.clear()

  def about: CacheInfo =
    val dataSize = Memory(files.values.map { file => safely(file.text.await().length).or(0) }.sum)
    CacheInfo(ecosystems.size, snapshots.size, workspaces.size, files.size, dataSize)

  def gitProgress(stream: LazyList[Progress]): LazyList[Double] = stream.collect:
    case Progress.Receiving(percent)         => percent/5.0
    case Progress.Unpacking(percent)         => 0.2 + percent/5.0
    case Progress.Resolving(percent)         => 0.4 + percent/5.0
    case Progress.RemoteCounting(percent)    => 0.6 + percent/5.0
    case Progress.RemoteCompressing(percent) => 0.8 + percent/5.0

  def apply(snapshot: Snapshot)
     (using Installation,
            Internet,
            WorkingDirectory,
            GitCommand,
            FrontEnd,
            Tactic[ExecError],
            Tactic[PathError],
            Tactic[IoError],
            Tactic[GitError])
          : Task[Directory] logs Message =

    snapshots.establish(snapshot):
      async:
        given Message transcribes GitEvent = _.communicate
        val destination = summon[Installation].snapshots.path / Name(snapshot.commit.show)

        if destination.exists() then destination.as[Directory] else
          Log.info(m"Cloning ${snapshot.url}")
          val process = Git.cloneCommit(snapshot.url, destination, snapshot.commit)
          val target = unsafely(Target(ProjectId(t"git"), GoalId(snapshot.commit.show)))
          summon[FrontEnd].start(target)
          gitProgress(process.progress).each: progress =>
            summon[FrontEnd](target) = progress

          summon[FrontEnd].stop(target)

          process.complete().workTree.vouch(using Unsafe).also:
            Log.info(m"Finished cloning ${snapshot.url}")

  def apply(ecosystem: Ecosystem)(using installation: Installation)
     (using Internet, WorkingDirectory, GitCommand)
          : Task[Vault] raises VaultError logs Message =

    ecosystems.establish(ecosystem):
      async:
        Log.info(m"Started async to fetch ecosystem")

        val destination =
          tend:
            case PathError(path, _) => VaultError()
          .within(ecosystem.path)

        if !destination.exists() then
          Log.info(m"Cloning ${ecosystem.url}")

          val process =
            tend:
              case GitError(detail)         => VaultError()
              case IoError(path)            => VaultError()
              case ExecError(command, _, _) => VaultError()
              case PathError(path, reason)  => VaultError()
            .within:
              given GitEvent is Loggable = Log.silent
              Git.clone(ecosystem.url, destination, branch = ecosystem.branch)

          process.complete().also:
            Log.info(m"Finished cloning ${ecosystem.url}")

        val dataDir =
          tend:
            case IoError(_) => VaultError()
          .within:
            (destination / p"data").as[Directory]

        val current =
          given projectIdDecoder: Decoder[ProjectId] =
            tend:
              case _: InvalidRefError => VaultError()
            .within:
              summon[Decoder[ProjectId]]

          given streamIdDecoder: Decoder[StreamId] =
            tend:
              case _: InvalidRefError => VaultError()
            .within:
              summon[Decoder[StreamId]]

          tend:
            case _: CodlError         => VaultError()
            case _: IoError           => VaultError()
            case _: CodlReadError     => VaultError()
            case _: GitRefError       => VaultError()
            case _: MarkdownError     => VaultError()
            case _: FqcnError         => VaultError()
            case _: DateError         => VaultError()
            case _: UrlError          => VaultError()
            case _: NumberError       => VaultError()
            case _: HostnameError     => VaultError()
            case _: InvalidRefError   => VaultError()
            case _: StreamError       => VaultError()
          .within:
            dataDir.descendants.filter(_.is[File]).to(List).map: path =>
              given licenseDecoder: CodlDecoder[LicenseId] = CodlDecoder.field[LicenseId]
              given mdDecoder: CodlDecoder[InlineMd] = CodlDecoder.field[InlineMd]
              given dateDecoder: CodlDecoder[Date] = CodlDecoder.field[Date]
              given numberDecoder: CodlDecoder[Int] = CodlDecoder.field[Int]
              val x = summon[Decoder[Fqcn]]
              given fqcnDecoder: CodlDecoder[Fqcn] = CodlDecoder.field[Fqcn]
              given keywordDecoder: CodlDecoder[Keyword] = CodlDecoder.field[Keyword]
              given urlDecoder: CodlDecoder[HttpUrl] = CodlDecoder.field[HttpUrl]
              given commitDecoder: CodlDecoder[CommitHash] = CodlDecoder.field[CommitHash]
              given branchDecoder: CodlDecoder[Branch] = CodlDecoder.field[Branch]

              val source = path.as[File].read[Text]
              Codl.read[Release](source)

            .filter: release =>
              release.date + release.lifetime.days > today()


        Vault(t"vent", 1, current)

  def workspace(path: Path)
     (using Installation, Internet, WorkingDirectory, GitCommand)
          : Task[Workspace] raises WorkspaceError logs Message =

    val lastModified =
      tend:
        case IoError(_) => WorkspaceError(WorkspaceError.Reason.Unreadable(path))
      .within(path.as[File].lastModified)

    val (cacheTime, workspace) =
      workspaces.establish(path):
        (lastModified, async(Workspace(path)))

    if cacheTime == lastModified then workspace else async(Workspace(path)).tap: async =>
      workspaces(path) = (lastModified, async)

  def projectsMap(workspace: Workspace)
     (using Installation, Internet, WorkingDirectory, GitCommand)
          : Task[Map[ProjectId, Definition]] raises WorkspaceError raises ConcurrencyError logs Message =

    locals.establish(workspace):
      async:
        val maps: List[Map[ProjectId, Definition]] = workspace.local.let(_.forks).or(Nil).map: fork =>
          Cache.workspace(fork.path).flatMap(projectsMap(_)).await()

        maps.foldLeft(workspace.projects.view.mapValues(_.definition(workspace)).toMap)(_ ++ _)

case class VaultError()(using Diagnostics) extends Error(m"the vault file is not valid")

given Realm = realm"fury"
