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
import punctuation.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*

import scala.collection.mutable as scm

object Cache:
  def apply
      (snapshot: Snapshot)
      (using installation: Installation, internet: Internet, workDir: WorkingDirectory, log: Log)
      (using Raises[IoError], Raises[GitError], Raises[PathError])
      : Directory =
    val path = installation.cache / PathName(snapshot.commit.name)
    if path.exists() && path.is[Directory] && (path / p".git").exists() then path.as[Directory]
    else
      val process = Git.cloneCommit(snapshot.url.encode, path, CommitHash(snapshot.commit.name))
      process.complete()
      path.as[Directory]

  private val ecosystems: scm.HashMap[Ecosystem, Vault] = scm.HashMap()

  def ecosystem
      (ecosystem: Ecosystem)
      (using installation: Installation)
      (using Internet, Log, Monitor, WorkingDirectory, Raises[NumberError], Raises[InvalidRefError], Raises[DateError], Raises[UrlError], Raises[CodlReadError], Raises[MarkdownError], Raises[PathError], Raises[IoError], Raises[StreamCutError], Raises[GitError], GitCommand)
      : Vault =
    if ecosystems.contains(ecosystem) then ecosystems(ecosystem) else
      val destination = unsafely(installation.vault.path / PathName(ecosystem.id.show) / PathName(ecosystem.commit.name))
      
      val file = if !destination.exists() then
        val process = Git.cloneCommit(ecosystem.url.encode, destination, ecosystem.commit)
        val progress = Async(process.progress.map(println).length)
        val repo = process.complete()
        safely(progress.await())
      
      val vault = Codl.read[Vault]((destination / p"vault.codl").as[File])
      ecosystems(ecosystem) = vault
      vault