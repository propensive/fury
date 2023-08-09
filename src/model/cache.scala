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

import nonagenarian.*
import rudiments.*
import eucalyptus.*
import spectacular.*
import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents}
import anticipation.*, fileApi.galileiApi
import serpentine.*, hierarchies.unixOrWindows

import scala.collection.mutable as scm

object Cache:
  def apply
      (snapshot: Snapshot)
      (using installation: Installation, internet: Internet, workDir: WorkingDirectory, log: Log)
      : Directory throws IoError | PathError | GitError =
    val path = installation.cacheDir / PathName(snapshot.commit.name)
    if path.exists() && path.is[Directory] && (path / p".git").exists() then path.as[Directory]
    else
      val process = Git.cloneCommit(snapshot.url.encode, path, CommitHash(snapshot.commit.name))
      process.complete()
      path.as[Directory]
