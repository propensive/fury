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
