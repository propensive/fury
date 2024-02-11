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

import anticipation.*, fileApi.galileiApi, timeApi.aviationApi
import aviation.*
import cellulose.*
import eucalyptus.*
import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents,
    overwritePreexisting}
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.collect
import nettlesome.*
import nonagenarian.*
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
          val process = Git.cloneCommit(snapshot.url.encode, destination, snapshot.commit)
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
        mitigate:
          case UrlError(_, _, _)          => VaultError()
          case InvalidRefError(_, _)      => VaultError()
          case ExecError(_, _, _)         => VaultError()
          case StreamError(bytes)         => VaultError()
          case NumberError(_, _)          => VaultError()
          case DateError(_)               => VaultError()
          case GitError(_)                => VaultError()
          case IoError(_)                 => VaultError()
          case GitRefError(_)             => VaultError()
          case HostnameError(_, _)        => VaultError()
          case UndecodableCharError(_, _) => VaultError()
          case PathError(_, _)            => VaultError()
          case NotFoundError(_)           => VaultError()
          case CodlReadError(_)           => VaultError()
          case MarkdownError(_)           => VaultError()
        .within:
          val destination = installation.vault.path / PathName(ecosystem.id.show) / PathName(ecosystem.branch.show)
          if !destination.exists() then
            Log.info(msg"Cloning ${ecosystem.url}")
            val process = Git.clone(ecosystem.url.encode, destination, branch = ecosystem.branch)
            
            Log.info(msg"Cloning ${ecosystem.url}")
            gitProgress(process.progress)
            
            process.complete().also:
              Log.info(msg"Finished cloning ${ecosystem.url}")            

          Codl.read[Vault]((destination / p"vault.codl").as[File])
      )

  // FIXME: No synchronization; should use a mutex instead
  def workspace
      (path: Path)
      (using Installation, Internet, Log[Output], Stdio, Monitor, WorkingDirectory, GitCommand)
      : Async[Workspace] raises WorkspaceError =
    mitigate:
      case IoError(path) => WorkspaceError(WorkspaceError.Reason.Unreadable(path))
    .within:
      val lastModified = path.as[File].lastModified
      val (cacheTime, workspace) = workspaces.getOrElseUpdate(path, (lastModified, Async(Workspace(path))))
      
      if cacheTime == lastModified then workspace else Async(Workspace(path)).tap: async =>
        workspaces(path) = (lastModified, async)
      
case class VaultError() extends Error(msg"the vault file is not valid")

given Realm = realm"fury"