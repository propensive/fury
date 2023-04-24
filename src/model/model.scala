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
import serpentine.*
import digression.*
import gossamer.*
import kaleidoscope.*
import aviation.*
import cellulose.*
import punctuation.*
import telekinesis.*

import Ids.*

trait GitRepo:
  def url: Url
  def commit: Commit
  def branch: Maybe[Branch]

case class Release(                      id: ProjectId,
                                         stream: StreamId,
                                         website: Maybe[Url],
		                                     description: InlineMd,
                                         tags: List[Tag],
		                                     license: LicenseId,
		                                     date: Date,
		                                     lifetime: Int,
		                                     repo: Repo,
		               @codlLabel("provide") packages: List[Package])

case class Repo(url: Url, commit: Commit, branch: Maybe[Branch]) extends GitRepo

case class Vault(@codlLabel("release") releases: List[Release]):
  inline def vault: Vault = this
  
  object index:
    lazy val releases: Map[ProjectId, Release] = unsafely(vault.releases.indexBy(_.id))

case class Local(@codlLabel("fork") forks: List[Fork])
case class Fork(id: BuildId, path: Relative)
case class Overlay(id: BuildId, url: Url, commit: Commit, branch: Maybe[Branch]) extends GitRepo
case class Mount(path: Relative, url: Url, commit: Commit, branch: Maybe[Branch]) extends GitRepo

case class Build(@codlLabel(":<<")     prelude:  Maybe[Prelude],
                 @codlLabel("overlay") overlays: List[Overlay],
                 @codlLabel("command") commands: List[Command],
		                                   default:  Maybe[CommandName],
		             @codlLabel("project") projects: List[Project],
                 @codlLabel("mount")   mounts:   List[Mount])

case class Prelude(comment: List[Text])
case class Project(id: ProjectId, @codlLabel("module") modules: List[Module]):
  inline def project: Project = this
  
  object index:
    lazy val modules: Map[ModuleRef, Module] =
      unsafely(project.modules.indexBy { module => ModuleRef(project.id, module.id) })

case class Assist(ref: ModuleRef, module: ModuleId)

enum Artifact:
  case Jar(path: Relative, main: ClassName)

case class Module(                      id:        ModuleId,
                  @codlLabel("include") includes:  List[ModuleRef],
                                        sources:   List[Relative],
                  @codlLabel("provide") packages:  List[Package],
                  @codlLabel("use")     usages:    List[ModuleRef],
                  @codlLabel("omit")    omissions: List[ModuleRef],
                  @codlLabel("assist")  assists:   List[Assist])

object ModuleRef extends RefType(t"module ref"):
  given (using CanThrow[InvalidRefError]): Codec[ModuleRef] = FieldCodec(_.show, ModuleRef(_))

  given Show[ModuleRef] = ref =>
    t"${ref.projectId.mm { projectId => t"$projectId/" }.or(t"")}${ref.moduleId}"
  
  def apply(value: Text): ModuleRef throws InvalidRefError = value match
    case r"${ProjectId(project)}@([^/])+\/${ModuleId(module)}@([^/]+)" =>
      ModuleRef(project, module)
    
    case ModuleId(module) =>
      ModuleRef(Unset, module)
    
    case _ =>
      throw InvalidRefError(value, this)

case class ModuleRef(projectId: Maybe[ProjectId], moduleId: ModuleId)
case class Command(name: CommandName, @codlLabel("action") actions: List[ModuleRef])
