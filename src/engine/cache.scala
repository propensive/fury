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

import anticipation.*, filesystemInterfaces.galileiApi, timeInterfaces.aviationApi
import aviation.*
import cellulose.*
import eucalyptus.*
import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents}
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.skip
import nettlesome.*
import octogenarian.*
import parasite.*
import contingency.*
import guillotine.*
import fulminate.*
import punctuation.*
import gossamer.*
import gastronomy.*
import escapade.*
import rudiments.*
import vacuous.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*

import scala.collection.concurrent as scc

case class CachedFile(lastModified: Instant, text: Async[Text], hash: Async[Hash])
case class CacheInfo(ecosystems: Int, snapshots: Int, workspaces: Int, files: Int, dataSize: ByteSize)

object Cache:
  private val ecosystems: scc.TrieMap[Ecosystem, Async[Vault]] = scc.TrieMap()
  private val snapshots: scc.TrieMap[Snapshot, Async[Directory]] = scc.TrieMap()
  private val workspaces: scc.TrieMap[Path, (Instant, Async[Workspace])] = scc.TrieMap()
  private val files: scc.TrieMap[Path, CachedFile] = scc.TrieMap()

  def clear(): Unit =
    ecosystems.clear()
    snapshots.clear()
    workspaces.clear()
    files.clear()

  def about(using Monitor): CacheInfo =
    val dataSize = ByteSize(files.values.map { file => safely(file.text.await().length).or(0) }.sum)
    CacheInfo(ecosystems.size, snapshots.size, workspaces.size, files.size, dataSize)

  def gitProgress(stream: LazyList[Progress]): LazyList[Activity] = stream.collect:
    case Progress.Receiving(percent)         => Activity.Progress(t"receiving", percent)
    case Progress.Unpacking(percent)         => Activity.Progress(t"unpacking", percent)
    case Progress.Resolving(percent)         => Activity.Progress(t"resolving", percent)
    case Progress.RemoteCounting(percent)    => Activity.Progress(t"counting", percent)
    case Progress.RemoteCompressing(percent) => Activity.Progress(t"compressing", percent)

  def apply(snapshot: Snapshot)
      (using Installation,
             Internet,
             Log[Display],
             Monitor,
             WorkingDirectory,
             GitCommand,
             Raises[ExecError],
             Raises[PathError],
             Raises[IoError],
             Raises[GitError])
          : Async[Directory] =

    snapshots.getOrElseUpdate(snapshot, Async:
      val destination = summon[Installation].snapshots.path / PathName(snapshot.commit.show)
      
      if destination.exists() then destination.as[Directory] else
        Log.info(msg"Cloning ${snapshot.url}")
        val process = Git.cloneCommit(snapshot.url, destination, snapshot.commit)
        gitProgress(process.progress).map(_.debug).each(Log.info(_))
        
        process.complete().workTree.vouch(using Unsafe).also:
          Log.info(msg"Finished cloning ${snapshot.url}")
    )
        
  def apply(ecosystem: Ecosystem)
      (using Installation, Internet, Log[Display], Monitor, WorkingDirectory, GitCommand)
          : Async[Vault] raises VaultError =

    ecosystems.getOrElseUpdate(ecosystem, Async:

      given (VaultError fixes UrlError)             = error => VaultError()
      given (VaultError fixes InvalidRefError)      = error => VaultError()
      given (VaultError fixes ExecError)            = error => VaultError()
      given (VaultError fixes StreamError)          = error => VaultError()
      given (VaultError fixes NumberError)          = error => VaultError()
      given (VaultError fixes DateError)            = error => VaultError()
      given (VaultError fixes GitError)             = error => VaultError()
      given (VaultError fixes IoError)              = error => VaultError()
      given (VaultError fixes GitRefError)          = error => VaultError()
      given (VaultError fixes HostnameError)        = error => VaultError()
      given (VaultError fixes UndecodableCharError) = error => VaultError()
      given (VaultError fixes PathError)            = error => VaultError()
      given (VaultError fixes CodlReadError)        = error => VaultError()
      given (VaultError fixes MarkdownError)        = error => VaultError()

      val destination = installation.vault.path / PathName(ecosystem.id.show) / PathName(ecosystem.branch.show)
      if !destination.exists() then
        Log.info(msg"Cloning ${ecosystem.url}")
        val process = Git.clone(ecosystem.url, destination, branch = ecosystem.branch)
          
        gitProgress(process.progress).map(_.debug).each(Log.info(_))
          
        process.complete().also:
          Log.info(msg"Finished cloning ${ecosystem.url}")            

      Codl.read[Vault]((destination / p"vault.codl").as[File])
    )

  def workspace(path: Path)(using Installation, Internet, Log[Display], Monitor, WorkingDirectory, GitCommand)
          : Async[Workspace] raises WorkspaceError =

    given (WorkspaceError fixes IoError) =
      case IoError(path) => WorkspaceError(WorkspaceError.Reason.Unreadable(path))

    val lastModified = path.as[File].lastModified
    val (cacheTime, workspace) = workspaces.getOrElseUpdate(path, (lastModified, Async(Workspace(path))))
      
    if cacheTime == lastModified then workspace else Async(Workspace(path)).tap: async =>
      workspaces(path) = (lastModified, async)
      
  def file(path: Path)(using Monitor): CachedFile raises IoError raises StreamError raises CancelError =
    val file = path.as[File]
    val lastModified = file.lastModified
    def text() = Async(file.readAs[Text])
    
    def cachedFile() =
      val async = text()
      CachedFile(lastModified, async, async.map(_.digest[Sha2[256]]))

    val cached = files.getOrElseUpdate(path, cachedFile())
      
    if cached.lastModified == lastModified then cached else cachedFile().tap: entry =>
      files(path) = entry

case class VaultError() extends Error(msg"the vault file is not valid")

given Realm = realm"fury"
