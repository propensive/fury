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

import galilei.*, filesystemOptions.{createNonexistent, createNonexistentParents, dereferenceSymlinks}
import anticipation.*, fileApi.galileiApi
import rudiments.*
import hypotenuse.*
import vacuous.*
import escapade.*
import parasite.*
import aviation.*, calendars.gregorian
import guillotine.*
import fulminate.*
import ambience.*, environments.virtualMachine, systemProperties.virtualMachine
import gossamer.*
import perforate.*
import eucalyptus.*
import gastronomy.*, alphabets.base32.zBase32
import turbulence.*
import imperial.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import nettlesome.*
import nonagenarian.*

import scala.collection.mutable as scm

case class ConfigError(msg: Message) extends Error(msg)

object Installation:
  def apply(cache: Directory): Installation raises ConfigError =
    mitigate:
      case StreamError(_)                => ConfigError(msg"The stream was cut while reading a file")
      case EnvironmentError(variable)    => ConfigError(msg"The environment variable $variable could not be accessed")
      case SystemPropertyError(property) => ConfigError(msg"The JVM system property $property could not be accessed")
      case IoError(path)                 => ConfigError(msg"An I/O error occurred while trying to access $path")
      case PathError(reason)             => ConfigError(msg"The path was not valid because $reason")
    .within:
      val configPath: Path = Home.Config() / p"fury"
      val config: File = (configPath / p"config.codl").as[File]
      val vault: Directory = (cache / p"vault").as[Directory]
      val snapshots: Directory = (cache / p"repos").as[Directory]
      val lib: Directory = (cache / p"lib").as[Directory]
      val tmp: Directory = (cache / p"tmp").as[Directory]
    
      Installation(config, cache, vault, lib, tmp, snapshots)
    
    
case class Installation
    (config: File, cache: Directory, vault: Directory, lib: Directory, tmp: Directory, snapshots: Directory)
  
inline def installation(using inline installation: Installation): Installation = installation

case class BuildError() extends Error(msg"the build could not run")

object Engine:
  private val builds: scm.HashMap[ModuleRef, Async[Digest[Sha2[256]]]] = scm.HashMap()
  
  def build(moduleRef: ModuleRef)(using universe: Universe)
      (using Monitor, Clock, Log[Output], FrontEnd, Stdio, WorkingDirectory, Internet, Installation, GitCommand)
      : Async[Digest[Sha2[256]]] raises BuildError =
    builds.synchronized:
      builds.getOrElseUpdate(moduleRef, Async:

        mitigate:
          case GitError(_)        => BuildError()
          case ExecError(_, _, _) => BuildError()
          case PathError(_)       => BuildError()
          case IoError(_)         => BuildError()
          case UnknownRefError(_) => BuildError()
          case WorkspaceError()   => BuildError()
          case StreamError(_)     => BuildError()
          case CancelError()      => BuildError()
        .within:
          val workspace = universe(moduleRef.projectId).source match
            case vault: Vault         => Workspace(Cache(vault.index.releases(moduleRef.projectId).repo).await().path)
            case workspace: Workspace => workspace
          
          val project: Project = workspace(moduleRef.projectId)
          val module = project(moduleRef.moduleId)
  
          val sourceFiles: List[File] = module.sources.flatMap: directory =>
            workspace(directory).descendants.filter(_.is[File]).filter(_.name.ends(t".scala")).map(_.as[File])
  
          val includes = module.includes.map(Engine.build(_)).map(_.await())
          
          val step = Step(sourceFiles, includes, Nil)
          log(msg"Digest = ${step.digest.encodeAs[Base32]}")
          
          val part = (math.random*36).toLong
          
          val progress = LazyList.range(0, 100).map: pc =>
            Thread.sleep(part)
            TaskEvent.Progress(t"typer", pc/100.0)
          
          follow(msg"Building $moduleRef")(progress)
          progress.length
          module.digest[Sha2[256]]
      )

extension (workspace: Workspace)
  def locals
      (ancestors: Set[Path] = Set())
      (using Monitor, Log[Output], FrontEnd, Stdio, WorkingDirectory, Internet, Installation, GitCommand)
      : Map[ProjectId, Definition] raises CancelError raises WorkspaceError =
    workspace.local.let: local =>
      local.forks.map: fork =>
        val workspace = Cache.workspace(fork.path).await()
        val projects = workspace.projects
        workspace.locals(ancestors + fork.path)
        
    .or(Nil).foldRight(workspace.projects.view.mapValues(_.definition(workspace)).to(Map))(_ ++ _)
  
  def universe
      ()
      (using Monitor, Clock, Log[Output], FrontEnd, Stdio, WorkingDirectory, Internet, Installation, GitCommand)
      : Universe raises CancelError raises VaultError raises WorkspaceError =
    given Timezone = tz"Etc/UTC"
    val vaultProjects = Cache(workspace.ecosystem).await()
    val localProjects = locals()
    
    val projects: Map[ProjectId, Definition] =
      vaultProjects.releases.filter(_.expiry <= today()).map: release =>
        (release.id, release.definition(vaultProjects))
      .to(Map)
    
    Universe(projects -- localProjects.keySet ++ localProjects)

  def apply(projectId: ProjectId): Project = workspace.projects(projectId)

  def apply
      (path: WorkPath)
      (using Installation, Internet, Stdio, Monitor, FrontEnd, WorkingDirectory, Log[Output],
          Raises[CancelError], Raises[GitError], Raises[PathError], Raises[ExecError], Raises[IoError])
      : Directory =
    workspace.mounts.keys.find(_.precedes(path)).match
      case None        => workspace.directory.path + path.link
      case Some(mount) => Cache(workspace.mounts(mount).repo).await().path + path.link
    .as[Directory]

case class Universe(projects: Map[ProjectId, Definition]):
  def apply(id: ProjectId)(using Raises[UnknownRefError]): Definition =
    projects.getOrElse(id, abort(UnknownRefError(id)))

enum Compiler:
  case Java(version: Int)
  case Scala
  case Kotlin

case class Step(sources: List[File], dependencies: List[Digest[Sha2[256]]], binaries: List[Digest[Sha2[256]]]):
  def digest(using Raises[StreamError], Raises[IoError]): Digest[Sha2[256]] =
    (sources.map(_.readAs[Bytes]), dependencies, binaries).digest