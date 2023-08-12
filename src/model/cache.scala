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

import anticipation.*, fileApi.galileiApi
import aviation.*
import cellulose.*
import eucalyptus.*
import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents}
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.collect
import nettlesome.*
import nonagenarian.*
import parasite.*
import perforate.*
import fulminate.*
import punctuation.*
import gossamer.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*

import scala.collection.mutable as scm

object Cache:
  private val ecosystems: scm.HashMap[Ecosystem, Vault] = scm.HashMap()
  private val snapshots: scm.HashMap[Snapshot, Workspace] = scm.HashMap()

  def apply
      (snapshot: Snapshot)
      (using installation: Installation)
      (using Internet, Log, Stdio, Monitor, FrontEnd, WorkingDirectory, Raises[UndecodableCharError], Raises[UnencodableCharError], Raises[NotFoundError], Raises[GitRefError], Raises[NumberError], Raises[InvalidRefError], Raises[DateError], Raises[UrlError], Raises[CodlReadError], Raises[MarkdownError], Raises[PathError], Raises[IoError], Raises[StreamCutError], Raises[GitError], GitCommand)
      : Workspace =
    snapshots.get(snapshot).getOrElse:
      val destination = unsafely(installation.snapshots.path / PathName(snapshot.commit.show))
      
      val directory = if !destination.exists() then
        val process = Git.cloneCommit(snapshot.url.encode, destination, snapshot.commit)
        
        follow:
          process.progress.collect:
            case Progress.Receiving(percent) => TaskEvent.Progress(t"receiving", percent)
            case Progress.Unpacking(percent) => TaskEvent.Progress(t"unpacking", percent)
            case Progress.Resolving(percent) => TaskEvent.Progress(t"resolving", percent)

        process.complete().workTree.avow(using Unsafe).path
      else destination

      Workspace(destination)

  def apply
      (ecosystem: Ecosystem)
      (using installation: Installation)
      (using Internet, Log, Monitor, FrontEnd, WorkingDirectory, Raises[GitRefError], Raises[NumberError], Raises[InvalidRefError], Raises[DateError], Raises[UrlError], Raises[CodlReadError], Raises[MarkdownError], Raises[PathError], Raises[IoError], Raises[StreamCutError], Raises[GitError], GitCommand)
      : Vault =
    if ecosystems.contains(ecosystem) then ecosystems(ecosystem) else
      val destination = unsafely(installation.vault.path / PathName(ecosystem.id.show) / PathName(ecosystem.commit.show))
      
      val file = if !destination.exists() then
        val process = Git.cloneCommit(ecosystem.url.encode, destination, ecosystem.commit)
        val progress = Async:
          log(msg"Starting")
          process.progress.map(_.debug).foreach { text => log(msg"$text") }
          log(msg"Done")
        val repo = process.complete()
        safely(progress.await())
      
      val vault = Codl.read[Vault]((destination / p"vault.codl").as[File])
      ecosystems(ecosystem) = vault
      vault