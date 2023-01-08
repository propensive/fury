package fury.model

import rudiments.*
import serpentine.*
import gossamer.*
import galilei.*, filesystems.unix
import kaleidoscope.*
import temporaneous.*
import cellulose.*
import punctuation.*
import telekinesis.*

trait GitRepo:
  def url: Url
  def commit: Commit
  def branch: Maybe[Branch]

case class Release(                      id: ProjectId,
                                         stream: StreamId,
                                         website: Maybe[Url],
		                                     description: InlineMd,
                   @codlLabel("tag")     tags: List[Tag],
		                                     license: LicenseId,
		                                     date: Date,
		                                     lifetime: Int,
		                                     repo: Repo,
		               @codlLabel("provide") packages: List[Package])

case class Repo(url: Url, commit: Commit, branch: Maybe[Branch]) extends GitRepo
case class Universe(@codlLabel("release") releases: List[Release])
  //lazy val index: Map[ProjectId, Map[StreamId, Release]] =

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
case class Project(id: ProjectId, module: List[Module])
case class Assist(primary: ModuleRef, secondary: ModuleRef)

case class Module(                      id:        ModuleId,
                  @codlLabel("include") includes:  List[ModuleRef],
                  @codlLabel("source")  sources:   List[Relative],
                  @codlLabel("provide") packages:  List[Package],
                  @codlLabel("use")     usages:    List[ModuleRef],
                  @codlLabel("omit")    omissions: List[ModuleRef],
                  @codlLabel("assist")  assists:   List[Assist])


object ModuleRef extends RefType(t"module ref"):
  def apply(value: Text): ModuleRef throws InvalidRefError = value match
    case r"${ProjectId(project)}@([^/])+\/${ModuleId(module)}@([^/]+)" => ModuleRef(project, module)
    case ModuleId(module)                                              => ModuleRef(Unset, module)
    case _                                                             => throw InvalidRefError(value, this)

case class ModuleRef(projectId: Maybe[ProjectId], moduleId: ModuleId)
case class Command(name: CommandName, @codlLabel("action") actions: List[ModuleRef])
