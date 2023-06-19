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

import galilei.*
import rudiments.*
import gossamer.*
import parasite.*
import guillotine.*
import serpentine.*
import digression.*
import turbulence.*, lineSeparation.jvm
import ambience.*

object GitError:
  enum Subcommand:
    case Clone(url: Text, name: Text)
    case Checkout(ref: Text)
  
  given Show[Subcommand] =
    case Subcommand.Clone(url, name) => t"cloning $name from $url"
    case Subcommand.Checkout(ref)    => t"checking out $ref"

case class GitError(dir: DiskPath, subcommand: GitError.Subcommand)
extends Error(err"Git failed while ${subcommand.show} in $dir")

object GitCache:
  private var tasks: Map[Text, Task[Directory]] = Map()

  def apply(url: Text, commit: Text)(using env: Environment, monitor: Monitor, stdio: Stdio): Directory throws CancelError | IoError | EnvError | GitError =
    synchronized:
      if tasks.contains(commit) then tasks(commit).await() else
        val task = Task(t"Git fetch $commit")(fetch(url, commit))
        tasks = tasks.updated(commit, task)
        task.await()

  def universe(name: Text, url: Text)(using env: Environment, stdio: Stdio): Directory throws GitError | EnvError | IoError =
    val tmp = Fury.repoDir.tmpPath()
    val dest = unsafely(Fury.universeDir / name)

    if dest.exists() then dest.directory(Expect).tap(_.touch())
    else
      Io.println(t"Cloning universe $name from $url")
      sh"git clone $url ${tmp.fullname}".exec[ExitStatus]() match
        case ExitStatus.Ok => tmp.directory(Expect).moveTo(dest)
        case _ => throw AppError(t"Could not fetch the universe $name from $url")

  private def fetch(url: Text, commit: Text)(using stdio: Stdio, env: Environment)
           : Directory throws GitError | EnvError | IoError =
    val tmp = Fury.repoDir.tmpPath()
    val dest = unsafely(Fury.repoDir / commit.take(10))
    val tmpGitDir = tmp / p".git"

    if dest.exists() then dest.directory(Expect).tap(_.touch())

    else
      Io.println(t"Cloning git repository $url commit ${commit.take(7)}")
      sh"git clone $url ${tmp.fullname}".exec[ExitStatus]() match
        case ExitStatus.Ok => sh"git --git-dir ${tmpGitDir.fullname} checkout $commit".exec[ExitStatus]() match
          case ExitStatus.Ok =>
            tmpGitDir.directory(Expect).delete()
            tmp.directory(Expect).moveTo(dest)
          case _ => throw GitError(tmpGitDir, GitError.Subcommand.Checkout(commit))
        case _ => throw GitError(tmpGitDir, GitError.Subcommand.Clone(url, commit))
  
