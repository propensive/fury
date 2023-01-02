package fury

import galilei.*
import rudiments.*
import gossamer.*
import parasitism.*
import guillotine.*
import serpentine.*

object GitError:
  enum Subcommand:
    case Clone(url: Text, name: Text)
    case Checkout(ref: Text)
  
  given Show[Subcommand] =
    case Subcommand.Clone(url, name) => t"cloning $name from $url"
    case Subcommand.Checkout(ref)    => t"checking out $ref"

case class GitError(dir: DiskPath[Unix], subcommand: GitError.Subcommand)
extends Error(err"Git failed while ${subcommand.show} in $dir")

object GitCache:
  private var tasks: Map[Text, Task[Directory[Unix]]] = Map()

  def apply(url: Text, commit: Text)(using Environment, Monitor, Stdout, Threading): Directory[Unix] throws CancelError | IoError | EnvError | GitError =
    synchronized:
      if tasks.contains(commit) then tasks(commit).await() else
        val task = Task(t"Git fetch $commit")(fetch(url, commit))
        tasks = tasks.updated(commit, task)
        task.await()

  def universe(name: Text, url: Text)(using Environment, Stdout): Directory[Unix] throws GitError | EnvError | IoError =
    val tmp = Fury.repoDir.tmpPath()
    val dest = unsafely(Fury.universeDir / name)

    if dest.exists() then dest.directory(Expect).tap(_.touch())
    else
      Out.println(t"Cloning universe $name from $url")
      sh"git clone $url ${tmp.fullname}".exec[ExitStatus]() match
        case ExitStatus.Ok => tmp.directory(Expect).moveTo(dest)
        case _ => throw AppError(t"Could not fetch the universe $name from $url")

  private def fetch(url: Text, commit: Text)(using Environment, Stdout)
           : Directory[Unix] throws GitError | EnvError | IoError =
    val tmp = Fury.repoDir.tmpPath()
    val dest = unsafely(Fury.repoDir / commit.take(10))
    val tmpGitDir = tmp / p".git"

    if dest.exists() then dest.directory(Expect).tap(_.touch())

    else
      Out.println(t"Cloning git repository $url commit ${commit.take(7)}")
      sh"git clone $url ${tmp.fullname}".exec[ExitStatus]() match
        case ExitStatus.Ok => sh"git --git-dir ${tmpGitDir.fullname} checkout $commit".exec[ExitStatus]() match
          case ExitStatus.Ok =>
            tmpGitDir.directory(Expect).delete()
            tmp.directory(Expect).moveTo(dest)
          case _ => throw GitError(tmpGitDir, GitError.Subcommand.Checkout(commit))
        case _ => throw GitError(tmpGitDir, GitError.Subcommand.Clone(url, commit))
  