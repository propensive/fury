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
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.collect
import nettlesome.*
import octogenarian.*
import parasite.*
import contingency.*
import guillotine.*
import fulminate.*
import punctuation.*
import gossamer.*
import escapade.*
import rudiments.*
import vacuous.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*

import scala.collection.mutable as scm

object Cache:
  private val ecosystems: scm.HashMap[Ecosystem, Async[Vault]] = scm.HashMap()
  private val snapshots: scm.HashMap[Snapshot, Async[Directory]] = scm.HashMap()
  private val workspaces: scm.HashMap[Path, (Instant, Async[Workspace])] = scm.HashMap()

  def gitProgress(stream: LazyList[Progress]): LazyList[Activity] = stream.collect:
    case Progress.Receiving(percent)         => Activity.Progress(t"receiving", percent)
    case Progress.Unpacking(percent)         => Activity.Progress(t"unpacking", percent)
    case Progress.Resolving(percent)         => Activity.Progress(t"resolving", percent)
    case Progress.RemoteCounting(percent)    => Activity.Progress(t"counting", percent)
    case Progress.RemoteCompressing(percent) => Activity.Progress(t"compressing", percent)

  def apply
      (snapshot: Snapshot)
      (using Installation, Internet, Log[Output], Monitor, WorkingDirectory, GitCommand,
          Raises[ExecError], Raises[PathError], Raises[IoError], Raises[GitError])
      : Async[Directory] =
    snapshots.synchronized:
      snapshots.getOrElseUpdate(snapshot, Async:
        val destination = summon[Installation].snapshots.path / PathName(snapshot.commit.show)
        
        if destination.exists() then destination.as[Directory] else
          Log.info(msg"Cloning ${snapshot.url}")
          val process = Git.cloneCommit(snapshot.url, destination, snapshot.commit)
          Log.info(msg"Cloning ${snapshot.url}")
          gitProgress(process.progress)
          
          process.complete().workTree.vouch(using Unsafe).also:
            Log.info(msg"Finished cloning ${snapshot.url}")
      )
        
  def apply
      (ecosystem: Ecosystem)
      (using Installation, Internet, Log[Output], Monitor, WorkingDirectory, GitCommand)
      : Async[Vault] raises VaultError =
    ecosystems.synchronized:
      ecosystems.getOrElseUpdate(ecosystem, Async:

        given (VaultError fixes UrlError) =
          case UrlError(_, _, _) => VaultError()

        given (VaultError fixes InvalidRefError) =
          case InvalidRefError(_, _) => VaultError()

        given (VaultError fixes ExecError) =
          case ExecError(_, _, _) => VaultError()

        given (VaultError fixes StreamError) =
          case StreamError(bytes) => VaultError()

        given (VaultError fixes NumberError) =
          case NumberError(_, _) => VaultError()

        given (VaultError fixes DateError) =
          case DateError(_) => VaultError()

        given (VaultError fixes GitError) =
          case GitError(_) => VaultError()

        given (VaultError fixes IoError) =
          case IoError(_) => VaultError()

        given (VaultError fixes GitRefError) =
          case GitRefError(_) => VaultError()

        given (VaultError fixes HostnameError) =
          case HostnameError(_, _) => VaultError()

        given (VaultError fixes UndecodableCharError) =
          case UndecodableCharError(_, _) => VaultError()

        given (VaultError fixes PathError) =
          case PathError(_, _) => VaultError()

        given (VaultError fixes NotFoundError) =
          case NotFoundError(_) => VaultError()

        given (VaultError fixes CodlReadError) =
          case CodlReadError(_) => VaultError()
        
        given (VaultError fixes MarkdownError) =
          case MarkdownError(_) => VaultError()

        val destination = installation.vault.path / PathName(ecosystem.id.show) / PathName(ecosystem.branch.show)
        if !destination.exists() then
          Log.info(msg"Cloning ${ecosystem.url}")
          val process = Git.clone(ecosystem.url, destination, branch = ecosystem.branch)
            
          Log.info(msg"Cloning ${ecosystem.url}")
          gitProgress(process.progress)
            
          process.complete().also:
            Log.info(msg"Finished cloning ${ecosystem.url}")            

        Codl.read[Vault]((destination / p"vault.codl").as[File])
      )

  // FIXME: No synchronization; should use a mutex instead
  def workspace
      (path: Path)
      (using Installation, Internet, Log[Output], Monitor, WorkingDirectory, GitCommand)
      : Async[Workspace] raises WorkspaceError =
    given (WorkspaceError fixes IoError) =
      case IoError(path) => WorkspaceError(WorkspaceError.Reason.Unreadable(path))

    val lastModified = path.as[File].lastModified
    val (cacheTime, workspace) = workspaces.getOrElseUpdate(path, (lastModified, Async(Workspace(path))))
      
    if cacheTime == lastModified then workspace else Async(Workspace(path)).tap: async =>
      workspaces(path) = (lastModified, async)
      
case class VaultError() extends Error(msg"the vault file is not valid")

given Realm = realm"fury"