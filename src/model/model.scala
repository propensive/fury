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

import rudiments.*
import digression.*
import anticipation.*
import gossamer.*
import kaleidoscope.*
import galilei.*
import aviation.*
import spectacular.*
import cellulose.*
import punctuation.*
import nettlesome.*

import Ids.*

trait GitRepo:
  def url: Url
  def commit: Commit
  def branch: Maybe[Branch]


object Release:
  given packagesLabel: CodlLabel[Release, "packages"] = CodlLabel("provide")

case class Release
    (id: ProjectId, stream: StreamId, website: Maybe[Url], description: InlineMd, tags: List[Tag],
        license: LicenseId, date: Date, lifetime: Int, repo: Repo, packages: List[Package])


case class Repo(url: Url, commit: Commit, branch: Maybe[Branch]) extends GitRepo


object Vault:
  given releasesLabel: CodlLabel[Vault, "releases"] = CodlLabel("release")

case class Vault(releases: List[Release]):
  inline def vault: Vault = this
  
  object index:
    lazy val releases: Map[ProjectId, Release] = unsafely(vault.releases.indexBy(_.id))


object Local:
  given forksLabel: CodlLabel[Local, "forks"] = CodlLabel("fork")

case class Local(forks: List[Fork])

case class Fork(id: BuildId, path: Path)
case class Overlay(id: BuildId, url: Url, commit: Commit, branch: Maybe[Branch]) extends GitRepo
case class Mount(path: SafeLink, url: Url, commit: Commit, branch: Maybe[Branch]) extends GitRepo


object Build:
  given preludeLabel: CodlLabel[Build, "prelude"] = CodlLabel(":<<")
  given overlaysLabel: CodlLabel[Build, "overlays"] = CodlLabel("overlay")
  given commandsLabel: CodlLabel[Build, "commands"] = CodlLabel("command")
  given projectsLabel: CodlLabel[Build, "projects"] = CodlLabel("project")
  given mountsLabel: CodlLabel[Build, "mounts"] = CodlLabel("mount")

case class Build
    (prelude: Maybe[Prelude], overlays: List[Overlay], commands: List[Command],
        default: Maybe[CommandName], projects: List[Project], mounts: List[Mount])


case class Prelude(comment: List[Text])


object Project:
  given modulesLabel: CodlLabel[Project, "modules"] = CodlLabel("module")

case class Project(id: ProjectId, modules: List[Module]):
  inline def project: Project = this
  
  object index:
    lazy val modules: Map[ModuleRef, Module] =
      unsafely(project.modules.indexBy { module => ModuleRef(project.id, module.id) })


case class Assist(ref: ModuleRef, module: ModuleId)


enum Artifact:
  case Jar(path: SafeLink, main: ClassName)


object Module:
  given includesLabel: CodlLabel[Module, "includes"] = CodlLabel("include")
  given packagesLabel: CodlLabel[Module, "packages"] = CodlLabel("provide")
  given usagesLabel: CodlLabel[Module, "usages"] = CodlLabel("use")
  given omissionsLabel: CodlLabel[Module, "omissions"] = CodlLabel("omit")
  given assistsLabel: CodlLabel[Module, "assists"] = CodlLabel("assist")

case class Module
    (id: ModuleId, includes: List[ModuleRef], sources: List[SafeLink], packages: List[Package],
        usages: List[ModuleRef], omissions: List[ModuleRef], assists: List[Assist])


object ModuleRef extends RefType(t"module ref"):
  given moduleRefEncoder: Encoder[ModuleRef] = _.show
  given moduleRefDecoder(using CanThrow[InvalidRefError]): Decoder[ModuleRef] = ModuleRef(_)
  
  given Show[ModuleRef] = ref =>
    t"${ref.projectId.mm { projectId => t"$projectId/" }.or(t"")}${ref.moduleId}"
  
  def apply(value: Text): ModuleRef throws InvalidRefError = value match
    case r"${ProjectId(project)}([^/]+)\/${ModuleId(module)}([^/]+)" =>
      ModuleRef(project, module)
    
    case ModuleId(module) =>
      ModuleRef(Unset, module)
    
    case _ =>
      throw InvalidRefError(value, this)

case class ModuleRef(projectId: Maybe[ProjectId], moduleId: ModuleId)


object Command:
  given CodlLabel[Command, "actions"] = CodlLabel("action")

case class Command(name: CommandName, actions: List[ModuleRef])
