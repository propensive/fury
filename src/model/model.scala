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
import cellulose.*
import escapade.*
import exoskeleton.*
import fulminate.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}, fileApi.galileiApi
import gastronomy.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.strict
import iridescence.*
import kaleidoscope.*
import nettlesome.*
import nonagenarian.*
import perforate.*
import punctuation.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

export gitCommands.environmentDefault

erased given CanThrow[AppError] = ###

// FIXME: This shouldn't need to exist. AggregateError needs to be replaced.
given (using CanThrow[AppError]): Raises[AggregateError[Error]] =
  new Raises[AggregateError[Error]]:
    def record(error: AggregateError[Error]): Unit = throw AppError(error.message, error)
    def abort(error: AggregateError[Error]): Nothing = throw AppError(error.message, error)

import Ids.*

object Release:
  given relabelling: CodlRelabelling[Release] = () => Map(t"packages" -> t"provide")

case class Release
    (id: ProjectId, stream: StreamId, name: Text, website: Optional[HttpUrl], description: InlineMd,
        license: LicenseId, date: Date, lifetime: Int, repo: Snapshot, packages: List[Package],
        keywords: List[Keyword]):
  def expiry: Date = date + lifetime.days
  
  def definition(vault: Vault): Definition =
    Definition(name, description, website, license, keywords, vault)


case class Snapshot(url: HttpUrl, commit: CommitHash, branch: Optional[Branch])

object Vault:
  given relabelling: CodlRelabelling[Vault] = () => Map(t"releases" -> t"release")

case class Vault(name: Text, version: Int, releases: List[Release]):
  inline def vault: Vault = this
  
  object index:
    lazy val releases: Map[ProjectId, Release] = unsafely(vault.releases.indexBy(_.id))


object Local:
  given relabelling: CodlRelabelling[Local] = () => Map(t"forks" -> t"fork")

case class Local(forks: List[Fork])

case class Fork(id: ProjectId, path: Path)

case class Ecosystem(id: EcosystemId, version: Int, url: HttpUrl, branch: Branch)

case class Mount(path: WorkPath, repo: Snapshot)


object Build:
  given relabelling: CodlRelabelling[Build] = () => Map(
    t"prelude" -> t":<<",
    t"actions" -> t"command",
    t"projects" -> t"project",
    t"mounts" -> t"mount",
  )

case class Build
    (prelude: Optional[Prelude], ecosystem: Ecosystem, actions: List[Action], default: Optional[ActionName],
        projects: List[Project], mounts: List[Mount])


case class Prelude(terminator: Text, comment: List[Text])


object Project:
  given relabelling: CodlRelabelling[Project] = () => Map(t"modules" -> t"module")

case class Project
    (id: ProjectId, name: Text, description: InlineMd, modules: List[Module], website: HttpUrl, license: Optional[LicenseId],
        keywords: List[Keyword]):
  
  // FIXME: Handle not-found
  def apply(module: ModuleId): Module = modules.find(_.id == module).get
  
  def definition(workspace: Workspace): Definition =
    Definition(name, description, website, license, keywords, workspace)

case class Assist(ref: ModuleRef, module: ModuleId)


object Module:
  given relabelling: CodlRelabelling[Module] = () => Map(
    t"includes"     -> t"include",
    t"packages"     -> t"provide",
    t"requirements" -> t"require",
    t"usages"       -> t"use",
    t"omissions"    -> t"omit",
    t"assists"      -> t"assist"
  )
  
case class Module
    (id: ModuleId, includes: List[ModuleRef], requirements: List[ModuleRef], sources: List[WorkPath],
        packages: List[Package], usages: List[ModuleRef], omissions: List[ModuleRef], assists: List[Assist],
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
  given relabelling: CodlRelabelling[Action] = () => Map(t"actions" -> t"action")

case class Action(name: ActionName, modules: List[ModuleRef], description: Optional[Text]):
  def suggestion: Suggestion = Suggestion(name.show, description.let { text => e"${colors.Khaki}($text)"} )

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

object Workspace:
  def apply()(using WorkingDirectory): Workspace raises WorkspaceError =
    mitigate:
      case IoError(_)   => WorkspaceError()
      case PathError(_) => WorkspaceError()
    .within(apply(workingDirectory[Path]))

  def apply(path: Path): Workspace raises WorkspaceError =
    mitigate:
      case HostnameError(_)             => WorkspaceError()
      case NotFoundError(_)             => WorkspaceError()
      case CodlReadError()              => WorkspaceError()
      case GitRefError(_)               => WorkspaceError()
      case StreamError(_)               => WorkspaceError()
      case MarkdownError(_)             => WorkspaceError()
      case IoError(_)                   => WorkspaceError()
      case UrlError(_, _, _)            => WorkspaceError()
      case PathError(_)                 => WorkspaceError()
      case InvalidRefError(_, _)        => WorkspaceError()
      case NumberError(_, _)            => WorkspaceError()
      case UndecodableCharError(_, _)   => WorkspaceError()
    .within:
      val dir: Directory = path.as[Directory]
      val buildFile: File = (dir / p".fury").as[File]
      val buildDoc: CodlDoc = Codl.parse(buildFile)
      val build: Build = Codl.read[Build](buildFile)
      val localPath: Path = dir / p".local"
      val localFile: Optional[File] = if localPath.exists() then localPath.as[File] else Unset
      val local: Optional[Local] = localFile.let(Codl.read[Local](_))
  
      Workspace(dir, buildDoc, build, local)

case class Workspace(directory: Directory, buildDoc: CodlDoc, build: Build, local: Optional[Local]):
  val ecosystem = build.ecosystem
  lazy val actions: Map[ActionName, Action] = unsafely(build.actions.indexBy(_.name))
  lazy val projects: Map[ProjectId, Project] = unsafely(build.projects.indexBy(_.id))
  lazy val mounts: Map[WorkPath, Mount] = unsafely(build.mounts.indexBy(_.path))

case class WorkspaceError() extends Error(msg"there was a problem reading the build file")