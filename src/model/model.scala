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
import galilei.*
import filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import filesystemInterfaces.galileiApi
import gastronomy.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.strict
import digression.*
import kaleidoscope.*
import nettlesome.*
import octogenarian.*
import contingency.*
import punctuation.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

export gitCommands.environmentDefault

erased given CanThrow[AppError] = ###

type Hash = Digest[Sha2[256]]

// FIXME: This shouldn't need to exist. AggregateError needs to be replaced.
given (using CanThrow[AppError]): Errant[AggregateError[Error]] =
  new Errant[AggregateError[Error]]:
    def record(error: AggregateError[Error]): Unit = throw AppError(error.message, error)
    def abort(error: AggregateError[Error]): Nothing = throw AppError(error.message, error)

import Ids.*

object Release:
  given relabelling: CodlRelabelling[Release] = () => Map(t"packages" -> t"provide")

case class Release
    (id:          ProjectId,
     stream:      StreamId,
     name:        Text,
     website:     Optional[HttpUrl],
     description: InlineMd,
     license:     LicenseId,
     date:        Date,
     lifetime:    Int,
     repo:        Snapshot,
     packages:    List[Fqcn],
     keywords:    List[Keyword])
derives Debug:

  def expiry: Date = date + lifetime.days

  def definition(vault: Vault): Definition =
    Definition(name, description, website, license, keywords, vault)


case class Snapshot(url: HttpUrl, commit: CommitHash, branch: Optional[Branch]) derives Debug

object Vault:
  given relabelling: CodlRelabelling[Vault] = () => Map(t"releases" -> t"release")

case class Vault(name: Text, version: Int, releases: List[Release]) derives Debug:
  inline def vault: Vault = this

  object index:
    lazy val releases: Map[ProjectId, Release] = unsafely(vault.releases.indexBy(_.id))

object Local:
  given relabelling: CodlRelabelling[Local] = () => Map(t"forks" -> t"fork")

case class Local(forks: List[Fork]) derives Debug

case class Fork(id: ProjectId, path: Path) derives Debug

case class Ecosystem(id: EcosystemId, version: Int, url: HttpUrl, branch: Branch) derives Debug

case class EcosystemFork(id: EcosystemId, path: Path) derives Debug

case class Mount(path: WorkPath, repo: Snapshot) derives Debug


object Build:
  given relabelling: CodlRelabelling[Build] = () =>
    Map
      (t"prelude" -> t":<<",
       t"actions" -> t"command",
       t"projects" -> t"project",
       t"mounts" -> t"mount")

case class Build
    (prelude:   Optional[Prelude],
     ecosystem: Ecosystem,
     actions:   List[Action],
     default:   Optional[ActionName],
     projects:  List[Project],
     mounts:    List[Mount])
derives Debug, CodlEncoder:

  def defaultAction: Optional[Action] = actions.where(_.name == default)


case class Prelude(terminator: Text, comment: List[Text]) derives Debug, CodlEncoder


object Project:
  given relabelling: CodlRelabelling[Project] = () =>
    Map
     (t"modules"    -> t"module",
      t"artifacts"  -> t"artifact",
      t"loads"      -> t"load",
      t"libraries"  -> t"library",
      t"containers" -> t"container",
      t"variables"  -> t"set",
      t"execs"      -> t"exec")

case class Project
    (id:          ProjectId,
     name:        Text,
     description: InlineMd,
     modules:     List[Module],
     libraries:   List[Library],
     artifacts:   List[Artifact],
     containers:  List[Container],
     loads:       List[Load],
     variables:   List[Variable],
     execs:       List[Exec],
     website:     HttpUrl,
     license:     Optional[LicenseId],
     keywords:    List[Keyword])
derives Debug, CodlEncoder:

  def suggestion: Suggestion = Suggestion(id.show, t"$name: $description")
  
  def apply(goal: GoalId): Optional[Module | Artifact | Library] =
    modules.where(_.id == goal).or(artifacts.where(_.id == goal)).or(libraries.where(_.id == goal))

  def goals: List[GoalId] = modules.map(_.id) ++ artifacts.map(_.id) ++ libraries.map(_.id)
  def targets: List[Target] = goals.map(Target(id, _))

  def definition(workspace: Workspace): Definition =
    Definition(name, description, website, license, keywords, workspace)

  def release(stream: StreamId, lifetime: Int, snapshot: Snapshot): Release raises ReleaseError =
    given Timezone = tz"Etc/UTC"

    Release
     (id          = id,
      stream      = stream,
      name        = name,
      website     = website,
      description = description,
      license     = license.or:
                      raise(ReleaseError(ReleaseError.Reason.NoLicense))(License.Apache2.id),
      date        = today(),
      lifetime    = lifetime,
      repo        = snapshot,
      packages    = Nil,
      keywords    = keywords)

object ReleaseError:
  enum Reason:
    case NoLicense
  
  given Communicable[Reason] =
    case Reason.NoLicense => msg"the license has not been specified"

case class ReleaseError(reason: ReleaseError.Reason)
extends Error(msg"The project is not ready for release because $reason")

case class Assist(target: Target, module: GoalId) derives Debug, CodlEncoder

object Basis extends RefType(t"basis"):
  given encoder: Encoder[Basis] = _.toString.tt.lower
  given decoder(using Errant[InvalidRefError]): Decoder[Basis] =
    case t"runtime" => Basis.Runtime
    case t"tools"   => Basis.Tools
    case value      => raise(InvalidRefError(value, this))(Basis.Runtime)

enum Basis:
  case Runtime, Tools

object Artifact:
  given relabelling: CodlRelabelling[Artifact] = () =>
    Map
     (t"kind"      -> t"type",
      t"includes"  -> t"include",
      t"resources" -> t"resource",
      t"prefixes"  -> t"prefix",
      t"suffixes"  -> t"suffix")

case class Artifact
    (id:         GoalId,
     path:       WorkPath,
     basis:      Optional[Basis],
     includes:   List[Target],
     main:       Optional[Fqcn],
     prefixes:   List[WorkPath],
     suffixes:   List[WorkPath],
     counter:    Optional[WorkPath],
     executable: Optional[Boolean],
     manifest:   List[ManifestEntry],
     resources:  List[Resource])
derives Debug, CodlEncoder

case class Resource(path: WorkPath, jarPath: WorkPath) derives Debug, CodlEncoder

case class ManifestEntry(key: Text, value: Text) derives Debug, CodlEncoder

object Container:
  given relabelling: CodlRelabelling[Container] = () =>
    Map(t"insertions" -> t"copy")

case class Insertion(source: WorkPath, destination: WorkPath) derives Debug, CodlEncoder
case class Extraction(source: WorkPath, destination: WorkPath) derives Debug, CodlEncoder

case class Container
    (id:          GoalId,
     dockerfile:  WorkPath,
     root:        WorkPath,
     insertions:  List[Insertion],
     extractions: List[Extraction])
derives Debug, CodlEncoder

object Exec:
  given relabelling: CodlRelabelling[Exec] = () =>
    Map(t"includes" -> t"include")

case class Exec(id: GoalId, includes: List[Target]) derives Debug, CodlEncoder

object Module:
  given relabelling: CodlRelabelling[Module] = () =>
    Map
     (t"includes"     -> t"include",
      t"packages"     -> t"provide",
      t"requirements" -> t"require",
      t"usages"       -> t"use",
      t"omissions"    -> t"omit",
      t"assists"      -> t"assist")

case class Module
    (id:           GoalId,
     includes:     List[Target],
     requirements: List[Target],
     sources:      List[WorkPath],
     packages:     List[Fqcn],
     usages:       List[Target],
     omissions:    List[Target],
     assists:      List[Assist],
     compiler:     Optional[Text],
     main:         Optional[Fqcn],
     coverage:     Optional[Target])
derives Debug, CodlEncoder

case class Library(id: GoalId, url: HttpUrl) derives Debug, CodlEncoder

case class Load(id: GoalId, path: WorkPath) derives Debug, CodlEncoder
case class Variable(id: GoalId, value: Text) derives Debug, CodlEncoder

case class Replacement(pattern: Text, variable: GoalId)

object Content:
  given relabelling: CodlRelabelling[Content] = () => Map(t"replacements" -> t"replace")

case class Content(path: WorkPath, replacements: List[Replacement])

object Target extends RefType(t"target"):
  given moduleRefEncoder: Encoder[Target] = _.show
  given moduleRefDebug: Debug[Target] = _.show
  given moduleRefMessage: Communicable[Target] = target => Message(target.show)
  given moduleRefDecoder(using Errant[InvalidRefError]): Decoder[Target] = Target(_)

  given Show[Target] = target =>
    t"${target.projectId.let { projectId => t"$projectId/" }.or(t"")}${target.goalId}"

  def apply(value: Text)(using Errant[InvalidRefError]): Target = value match
    case r"${ProjectId(project)}([^/]+)\/${GoalId(module)}([^/]+)" =>
      Target(project, module)

    case _ =>
      raise(InvalidRefError(value, this))(Target(ProjectId(t"unknown"), GoalId(t"unknown")))

case class Target(projectId: ProjectId, goalId: GoalId):
  def suggestion: Suggestion = Suggestion(this.show, Unset)
  def partialSuggestion: Suggestion = Suggestion(t"${projectId}/", Unset, incomplete = true)

object Action:
  given relabelling: CodlRelabelling[Action] = () => Map(t"actions" -> t"action")

case class Action(name: ActionName, modules: List[Target], description: Optional[Text])
derives Debug:
  def suggestion: Suggestion = Suggestion(name.show, description.let { text => e"$Italic($text)"} )

object WorkPath:
  given navigable: Navigable[WorkPath, GeneralForbidden, Unit] with
    def root(path: WorkPath): Unit = ()
    def prefix(root: Unit): Text = t""
    def descent(path: WorkPath): List[PathName[GeneralForbidden]] = path.descent
    def separator(path: WorkPath): Text = t"/"

  given rootParser: RootParser[WorkPath, Unit] = text => ((), text)
  
  given creator: PathCreator[WorkPath, GeneralForbidden, Unit] =
    (unit, descent) => WorkPath(descent)
  
  given show: Show[WorkPath] = _.render
  given encoder: Encoder[WorkPath] = _.render
  //given debug: Debug[WorkPath] = _.render
  given digestible: Digestible[WorkPath] = (acc, path) => acc.append(path.show.bytes)

  given decoder(using path: Errant[PathError]): Decoder[WorkPath] = new Decoder[WorkPath]:
    def decode(text: Text): WorkPath = Navigable.decode(text)

  inline given add: AddOperator[Path, WorkPath] with
    type Result = Path
    def add(left: Path, right: WorkPath): Path = right.descent.reverse.foldLeft(left)(_ / _)

case class WorkPath(descent: List[PathName[GeneralForbidden]]) derives Debug, CodlEncoder:
  def link: SafeLink = SafeLink(0, descent)

case class Definition
    (name:        Text,
     description: InlineMd,
     website:     Optional[HttpUrl],
     license:     Optional[LicenseId],
     keywords:    List[Keyword],
     source:      Vault | Workspace)
derives Debug

object Workspace:
  def apply()(using WorkingDirectory): Workspace raises WorkspaceError =
    given (WorkspaceError fixes IoError) =
      case IoError(path)        => WorkspaceError(WorkspaceError.Reason.Unreadable(path))

    given (WorkspaceError fixes PathError) =
      case pathError: PathError =>
        WorkspaceError(WorkspaceError.Reason.Explanation(pathError.message))

    apply(workingDirectory[Path])

  def apply(path: Path): Workspace raises WorkspaceError =
    given (WorkspaceError fixes HostnameError) =
      case HostnameError(text, _) => WorkspaceError(WorkspaceError.Reason.BadData(text))

    given (WorkspaceError fixes CodlReadError) =
      case CodlReadError(_) => WorkspaceError(WorkspaceError.Reason.BadContent)

    given (WorkspaceError fixes GitRefError) =
      case GitRefError(text) => WorkspaceError(WorkspaceError.Reason.BadData(text))

    given (WorkspaceError fixes StreamError) =
      case StreamError(_) => WorkspaceError(WorkspaceError.Reason.Unreadable(path))

    given (WorkspaceError fixes MarkdownError) =
      case MarkdownError(reason) =>
        WorkspaceError(WorkspaceError.Reason.Explanation(reason.communicate))

    given (WorkspaceError fixes IoError) =
      case IoError(path) => WorkspaceError(WorkspaceError.Reason.Unreadable(path))

    given (WorkspaceError fixes UrlError) =
      case UrlError(text, _, _) => WorkspaceError(WorkspaceError.Reason.BadData(text))

    given (WorkspaceError fixes PathError) =
      case pathError: PathError =>
        WorkspaceError(WorkspaceError.Reason.Explanation(pathError.message))

    given (WorkspaceError fixes FqcnError) =
      case error: FqcnError => WorkspaceError(WorkspaceError.Reason.Explanation(error.message))

    given (WorkspaceError fixes InvalidRefError) =
      case InvalidRefError(text, _) => WorkspaceError(WorkspaceError.Reason.BadData(text))

    given (WorkspaceError fixes NumberError) =
      case NumberError(text, _) => WorkspaceError(WorkspaceError.Reason.BadData(text))

    given (WorkspaceError fixes UndecodableCharError) =
      case UndecodableCharError(_, _) => WorkspaceError(WorkspaceError.Reason.BadContent)

    val dir: Directory = path.as[Directory]
    val buildFile: File = (dir / p".fury").as[File]
    val buildDoc: CodlDoc = Codl.parse(buildFile)
    val build: Build = Codl.read[Build](buildFile)
    val localPath: Path = dir / p".local"
    val localFile: Optional[File] = if localPath.exists() then localPath.as[File] else Unset
    val local: Optional[Local] = localFile.let(Codl.read[Local](_))

    Workspace(dir, buildDoc, build, local)

case class Workspace(directory: Directory, buildDoc: CodlDoc, build: Build, local: Optional[Local])
derives Debug:
  def ecosystem = build.ecosystem
  lazy val actions: Map[ActionName, Action] = unsafely(build.actions.indexBy(_.name))
  lazy val projects: Map[ProjectId, Project] = unsafely(build.projects.indexBy(_.id))
  lazy val mounts: Map[WorkPath, Mount] = unsafely(build.mounts.indexBy(_.path))

object WorkspaceError:
  enum Reason:
    case Unreadable(filename: Path)
    case BadContent
    case Explanation(message: Message)
    case BadData(text: Text)

  given Communicable[Reason] =
    case Reason.Unreadable(path)     => msg"$path could not be read"
    case Reason.BadContent           => msg"the content was not valid CoDL"
    case Reason.Explanation(message) => message
    case Reason.BadData(text)        => msg"the value $text was not in the correct format"

case class WorkspaceError(reason: WorkspaceError.Reason)
extends Error(msg"the workspace could not be read because $reason")