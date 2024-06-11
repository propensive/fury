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

import anticipation.*, filesystemApi.{galileiFile, galileiDirectory, galileiPath}, timeInterfaces.aviationApi
import aviation.*, calendars.gregorian
import cellulose.*
import eucalyptus.*

import galilei.*,
    filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents}

import hieroglyph.*, charDecoders.utf8, encodingMitigation.skip
import nettlesome.*
import octogenarian.*
import parasite.*, asyncOptions.cancelOrphans
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
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*

import scala.collection.concurrent as scc

given Timezone = tz"Etc/UTC"

case class CachedFile(lastModified: Instant, text: Task[Text], hash: Task[Hash])

case class CacheInfo
    (ecosystems: Int, snapshots: Int, workspaces: Int, files: Int, dataSize: ByteSize)

object Cache:
  private val ecosystems: scc.TrieMap[Ecosystem, Task[Vault]] = scc.TrieMap()
  private val snapshots: scc.TrieMap[Snapshot, Task[Directory]] = scc.TrieMap()
  private val workspaces: scc.TrieMap[Path, (Instant, Task[Workspace])] = scc.TrieMap()
  private val files: scc.TrieMap[Path, CachedFile] = scc.TrieMap()
  private val locals: scc.TrieMap[Workspace, Task[Map[ProjectId, Definition]]] = scc.TrieMap()

  given supervisor: Supervisor = PlatformSupervisor

  def file(path: Path)(using Log[Display])
          : CachedFile raises IoError raises StreamError raises ConcurrencyError =

    val file = path.as[File]
    val lastModified = file.lastModified
    
    def cachedFile() =
      val task = async(file.readAs[Text])
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
    val dataSize = ByteSize(files.values.map { file => safely(file.text.await().length).or(0) }.sum)
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
             Log[Display],
             WorkingDirectory,
             GitCommand,
             FrontEnd,
             Errant[ExecError],
             Errant[PathError],
             Errant[IoError],
             Errant[GitError])
          : Task[Directory] =

    snapshots.establish(snapshot):
      async:
        val destination = summon[Installation].snapshots.path / PathName(snapshot.commit.show)
        
        if destination.exists() then destination.as[Directory] else
          Log.info(msg"Cloning ${snapshot.url}")
          val process = Git.cloneCommit(snapshot.url, destination, snapshot.commit)
          val target = unsafely(Target(ProjectId(t"git"), GoalId(snapshot.commit.show)))
          summon[FrontEnd].start(target)
          gitProgress(process.progress).each: progress =>
            summon[FrontEnd](target) = progress
            
          summon[FrontEnd].stop(target)
          
          process.complete().workTree.vouch(using Unsafe).also:
            Log.info(msg"Finished cloning ${snapshot.url}")
        
  def apply(ecosystem: Ecosystem)(using installation: Installation)
      (using Internet, Log[Display], WorkingDirectory, GitCommand)
          : Task[Vault] raises VaultError =
    
    ecosystems.establish(ecosystem):
      async:
        Log.info(t"Started async to fetch ecosystem")
  
        val destination = tend(ecosystem.path).remedy:
          case PathError(path, _) => abort(VaultError())
  
        if !destination.exists() then
          Log.info(msg"Cloning ${ecosystem.url}")
          
          val process =
            tend(Git.clone(ecosystem.url, destination, branch = ecosystem.branch)).remedy:
              case GitError(_)        => abort(VaultError())
              case IoError(_)         => abort(VaultError())
              case ExecError(_, _, _) => abort(VaultError())
              case PathError(_, _)    => abort(VaultError())
            
          gitProgress(process.progress).map(_.debug).each(Log.info(_))
            
          process.complete().also:
            Log.info(msg"Finished cloning ${ecosystem.url}")            
        
        val dataDir = tend((destination / p"data").as[Directory]).remedy:
          case IoError(_) => abort(VaultError())
        
        val current =
          given projectIdDecoder: Decoder[ProjectId] = tend(summon[Decoder[ProjectId]]).remedy:
            case _: InvalidRefError => abort(VaultError())
          
          given streamIdDecoder: Decoder[StreamId] = tend(summon[Decoder[StreamId]]).remedy:
            case _: InvalidRefError => abort(VaultError())
          
          tend:
            dataDir.descendants.filter(_.is[File]).to(List).map: path =>
              given licenseDecoder: CodlDecoder[LicenseId] = CodlDecoder.field[LicenseId]
              given mdDecoder: CodlDecoder[InlineMd] = CodlDecoder.field[InlineMd]
              given dateDecoder: CodlDecoder[Date] = CodlDecoder.field[Date]
              given numberDecoder: CodlDecoder[Int] = CodlDecoder.field[Int]
              given fqcnDecoder: CodlDecoder[Fqcn] = CodlDecoder.field[Fqcn]
              given keywordDecoder: CodlDecoder[Keyword] = CodlDecoder.field[Keyword]
              given urlDecoder: CodlDecoder[HttpUrl] = CodlDecoder.field[HttpUrl]
              given commitDecoder: CodlDecoder[CommitHash] = CodlDecoder.field[CommitHash]
              given branchDecoder: CodlDecoder[Branch] = CodlDecoder.field[Branch]

              Codl.read[Release](path.as[File])

            .filter: release =>
              release.date + release.lifetime.days > today()

          .remedy:
            case _: AggregateError[?] => abort(VaultError())
            case _: IoError           => abort(VaultError())
            case _: CodlReadError     => abort(VaultError())
            case _: GitRefError       => abort(VaultError())
            case _: MarkdownError     => abort(VaultError())
            case _: FqcnError         => abort(VaultError())
            case _: DateError         => abort(VaultError())
            case _: UrlError          => abort(VaultError())
            case _: NumberError       => abort(VaultError())
            case _: HostnameError     => abort(VaultError())
            case _: InvalidRefError   => abort(VaultError())
            case _: StreamError       => abort(VaultError())

        Vault(t"vent", 1, current)

  def workspace(path: Path)
      (using Installation, Internet, Log[Display], WorkingDirectory, GitCommand)
          : Task[Workspace] raises WorkspaceError =

    val lastModified = tend(path.as[File].lastModified).remedy:
      case IoError(_) => abort(WorkspaceError(WorkspaceError.Reason.Unreadable(path)))
    
    val (cacheTime, workspace) =
      workspaces.establish(path):
        (lastModified, async(Workspace(path)))
      
    if cacheTime == lastModified then workspace else async(Workspace(path)).tap: async =>
      workspaces(path) = (lastModified, async)

  def projectsMap(workspace: Workspace)
      (using Installation, Internet, Log[Display], WorkingDirectory, GitCommand)
          : Task[Map[ProjectId, Definition]] raises WorkspaceError raises ConcurrencyError =
    
    locals.establish(workspace):
      async:
        val maps: List[Map[ProjectId, Definition]] = workspace.local.let(_.forks).or(Nil).map: fork =>
          Cache.workspace(fork.path).flatMap(projectsMap(_)).await()
        
        maps.foldLeft(workspace.projects.view.mapValues(_.definition(workspace)).toMap)(_ ++ _)

case class VaultError() extends Error(msg"the vault file is not valid")

given Realm = realm"fury"
