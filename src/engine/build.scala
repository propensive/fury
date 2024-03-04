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

import anticipation.*
import aviation.*, calendars.gregorian
import escapade.*
import eucalyptus.*
import fulminate.*
import galilei.*, filesystemOptions.{createNonexistent, createNonexistentParents, dereferenceSymlinks}
import gastronomy.*
import gossamer.*
import acyclicity.*
import guillotine.*
import hypotenuse.*
import hieroglyph.*, charDecoders.utf8
import nettlesome.*
import octogenarian.*
import parasite.*
import contingency.*
import dendrology.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*
import vacuous.*

import scala.collection.concurrent as scc

case class ConfigError(msg: Message) extends Error(msg)

case class Config(log: LogConfig = LogConfig())
case class LogConfig(path: Path = Unix / p"var" / p"log" / p"fury.log")

case class BuildError() extends Error(msg"the build could not run")

object Engine:
  private val steps: scc.TrieMap[Hash, Step] = scc.TrieMap()
  private val builds: scc.TrieMap[ModuleRef, Async[Hash]] = scc.TrieMap()

  given expandable: Expandable[Step] = _.dependencies.map(steps(_))

  def buildGraph(digest: Hash): DagDiagram[Step] =
    DagDiagram(Dag.create(steps(digest))(_.dependencies.to(Set).map(steps(_))))

  def build(moduleRef: ModuleRef)(using universe: Universe)
      (using Monitor, Clock, Log[Display], WorkingDirectory, Internet, Installation, GitCommand)
      : Async[Hash] raises BuildError =
    builds.getOrElseUpdate(moduleRef, Async:
      Log.info(msg"Starting computation of $moduleRef")

      given (BuildError fixes GitError)        = error => BuildError()
      given (BuildError fixes ExecError)       = error => BuildError()
      given (BuildError fixes PathError)       = error => BuildError()
      given (BuildError fixes IoError)         = error => BuildError()
      given (BuildError fixes UnknownRefError) = error => BuildError()
      given (BuildError fixes WorkspaceError)  = error => BuildError()
      given (BuildError fixes StreamError)     = error => BuildError()
      given (BuildError fixes CancelError)     = error => BuildError()
      
      val workspace = universe(moduleRef.projectId).source match
        case vault: Vault =>
          Workspace(Cache(vault.index.releases(moduleRef.projectId).repo).await().path)
        
        case workspace: Workspace =>
          workspace
        
      val project: Project = workspace(moduleRef.projectId)
      val module = project(moduleRef.moduleId)

      val sourceFiles: List[File] = module.sources.flatMap: directory =>
        workspace(directory).descendants.filter(_.is[File]).filter(_.name.ends(t".scala")).map(_.as[File])

      val includes = module.includes.map(Engine.build(_)).map(_.await())
      val step = Step(moduleRef, sourceFiles, includes, Nil)
      
      steps(step.digest) = step
        
      Log.info(msg"Computed $moduleRef")
      step.digest
    )

extension (workspace: Workspace)
  def locals(ancestors: Set[Path] = Set())
      (using Monitor, Log[Display], WorkingDirectory, Internet, Installation, GitCommand)
          : Map[ProjectId, Definition] raises CancelError raises WorkspaceError =

    workspace.local.let: local =>
      local.forks.map: fork =>
        val workspace = Cache.workspace(fork.path).await()
        val projects = workspace.projects
        workspace.locals(ancestors + fork.path)
        
    .or(Nil).foldRight(workspace.projects.view.mapValues(_.definition(workspace)).to(Map))(_ ++ _)
  
  def universe()
      (using Monitor, Clock, Log[Display], WorkingDirectory, Internet, Installation, GitCommand)
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

  def apply(path: WorkPath)
      (using Installation,
             Internet,
             Monitor,
             WorkingDirectory,
             Log[Display],
             Raises[CancelError],
             Raises[GitError],
             Raises[PathError],
             Raises[ExecError],
             Raises[IoError])
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
  
  def sources(name: Text): Boolean = this match
    case Java(_) => name.ends(t".java")
    case Scala   => name.ends(t".scala")
    case Kotlin  => name.ends(t".kt")

object Step:
  def apply(ref: ModuleRef, sources: List[File], dependencies: List[Hash], binaries: List[Hash])
      (using Raises[StreamError], Raises[IoError])
          : Step =

    import badEncodingHandlers.skip
    
    val sourceMap = sources.map { file => file.path -> file.readAs[Text] }.to(Map)
    val digest = (sourceMap.values.to(List), dependencies, binaries).digest[Sha2[256]]
    
    Step(ref, sourceMap, dependencies, binaries, digest)

  given show: Show[Step] = step => step.ref.show

case class Step
    (ref:          ModuleRef,
     sources:      Map[Path, Text],
     dependencies: List[Hash],
     binaries:     List[Hash],
     digest:       Hash)