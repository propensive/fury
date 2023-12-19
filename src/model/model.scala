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

import anticipation.*
import aviation.*
import cellulose.*
import symbolism.*
import galilei.*
import gastronomy.*
import fulminate.*
import perforate.*
import gossamer.*
import serpentine.*
import kaleidoscope.*
import nettlesome.*
import punctuation.*
import hieroglyph.*, charEncoders.utf8
import rudiments.*
import vacuous.*
import spectacular.*
import nonagenarian.*

import calendars.gregorian

import Ids.*

object Release:
  given packagesLabel: CodlLabel[Release, "packages"] = CodlLabel("provide")

case class Release
    (id: ProjectId, name: Text, stream: StreamId, website: Optional[HttpUrl], description: InlineMd,
        license: LicenseId, date: Date, lifetime: Int, repo: Snapshot, packages: List[Package],
        keywords: List[Keyword]):
  def expiry: Date = date + lifetime.days
  
  def definition(vault: Vault): Definition =
    Definition(name, description, website, license, keywords, vault)


case class Snapshot(url: HttpUrl, commit: CommitHash, branch: Optional[Branch])

object Vault:
  given releasesLabel: CodlLabel[Vault, "releases"] = CodlLabel("release")

case class Vault(name: Text, version: Int, releases: List[Release]):
  inline def vault: Vault = this
  
  object index:
    lazy val releases: Map[ProjectId, Release] = unsafely(vault.releases.indexBy(_.id))


object Local:
  given forksLabel: CodlLabel[Local, "forks"] = CodlLabel("fork")

case class Local(forks: List[Fork])

case class Fork(id: ProjectId, path: Path)

case class Ecosystem(id: EcosystemId, version: Int, url: HttpUrl, branch: Branch)

case class Mount(path: WorkPath, repo: Snapshot)


object Build:
  given preludeLabel: CodlLabel[Build, "prelude"] = CodlLabel(":<<")
  given actionsLabel: CodlLabel[Build, "actions"] = CodlLabel("command")
  given projectsLabel: CodlLabel[Build, "projects"] = CodlLabel("project")
  given mountsLabel: CodlLabel[Build, "mounts"] = CodlLabel("mount")

case class Build
    (prelude: Optional[Prelude], ecosystem: Ecosystem, actions: List[Action], default: Optional[ActionName],
        projects: List[Project], mounts: List[Mount])


case class Prelude(terminator: Text, comment: List[Text])


object Project:
  given modulesLabel: CodlLabel[Project, "modules"] = CodlLabel("module")

case class Project
    (id: ProjectId, name: Text, description: InlineMd, modules: List[Module], website: HttpUrl, license: Optional[LicenseId],
        keywords: List[Keyword]):
  
  // FIXME: Handle not-found
  def apply(module: ModuleId): Module = modules.find(_.id == module).get
  
  def definition(workspace: Workspace): Definition =
    Definition(name, description, website, license, keywords, workspace)

case class Assist(ref: ModuleRef, module: ModuleId)


enum Artifact:
  case Jar(path: WorkPath, main: ClassName)


object Module:
  given includesLabel: CodlLabel[Module, "includes"] = CodlLabel("include")
  given packagesLabel: CodlLabel[Module, "packages"] = CodlLabel("provide")
  given usagesLabel: CodlLabel[Module, "usages"] = CodlLabel("use")
  given omissionsLabel: CodlLabel[Module, "omissions"] = CodlLabel("omit")
  given assistsLabel: CodlLabel[Module, "assists"] = CodlLabel("assist")

case class Module
    (id: ModuleId, includes: List[ModuleRef], sources: List[WorkPath], packages: List[Package],
        usages: List[ModuleRef], omissions: List[ModuleRef], assists: List[Assist],
        compiler: Optional[Text], main: Optional[ClassName], coverage: Optional[ModuleRef])

object ModuleRef extends RefType(t"module ref"):
  given moduleRefEncoder: Encoder[ModuleRef] = _.show
  given moduleRefDebug: Debug[ModuleRef] = _.show
  given moduleRefMessage: Communicable[ModuleRef] = ref => Message(ref.show)
  given moduleRefDecoder(using Raises[InvalidRefError]): Decoder[ModuleRef] = ModuleRef(_)


  given Show[ModuleRef] = ref =>
    t"${ref.projectId.let { projectId => t"$projectId/" }.or(t"")}${ref.moduleId}"
  
  def apply(value: Text)(using Raises[InvalidRefError]): ModuleRef = value match
    case r"${ProjectId(project)}([^/]+)\/${ModuleId(module)}([^/]+)" =>
      ModuleRef(project, module)
    
    // case ModuleId(module) =>
    //   ModuleRef(Unset, module)
    
    case _ =>
      raise(InvalidRefError(value, this))(ModuleRef(ProjectId(t"unknown"), ModuleId(t"unknown")))



case class ModuleRef(projectId: ProjectId, moduleId: ModuleId)

object Action:
  given CodlLabel[Action, "actions"] = CodlLabel("action")

case class Action(name: ActionName, modules: List[ModuleRef])

object WorkPath:
  given reachable: Reachable[WorkPath, GeneralForbidden, Unit] with
    def root(path: WorkPath): Unit = ()
    def prefix(root: Unit): Text = t""
    def descent(path: WorkPath): List[PathName[GeneralForbidden]] = path.descent
    def separator(path: WorkPath): Text = t"/"
  
  given rootParser: RootParser[WorkPath, Unit] = text => ((), text)
  given creator: PathCreator[WorkPath, GeneralForbidden, Unit] = (unit, descent) => WorkPath(descent)
  given show: Show[WorkPath] = _.render
  given encoder: Encoder[WorkPath] = _.render
  given debug: Debug[WorkPath] = _.render
  given digestible: Digestible[WorkPath] = (acc, path) => acc.append(path.show.bytes)
  
  given decoder(using path: Raises[PathError]): Decoder[WorkPath] = new Decoder[WorkPath]:
    def decode(text: Text): WorkPath = Reachable.decode(text)
  
  inline given add: Operator["+", Path, WorkPath] with
    type Result = Path
    def apply(left: Path, right: WorkPath): Path = right.descent.reverse.foldLeft(left)(_ / _)

case class WorkPath(descent: List[PathName[GeneralForbidden]]):
  def link: SafeLink = SafeLink(0, descent)

case class Definition
    (name: Text, description: InlineMd, website: Optional[HttpUrl], license: Optional[LicenseId],
        keywords: List[Keyword], source: Vault | Workspace)

