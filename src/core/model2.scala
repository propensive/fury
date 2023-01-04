package fury.model

import rudiments.*
import serpentine.*
import gossamer.*
import kaleidoscope.*
import temporaneous.*
import cellulose.*
import telekinesis.*

case class InvalidRefError(id: Text, refType: RefType)
extends Error(err"The value $id is not a valid ${refType.name}")

trait RefType(val name: Text)

object Ids:
  opaque type ReleaseId = Text
  opaque type BuildId = Text
  opaque type StreamId = Text
  opaque type ProjectId = Text
  opaque type ModuleId = Text
  opaque type Tag = Text
  opaque type Package = Text
  opaque type Branch = Text
  opaque type Commit = Text
  opaque type CommandName = Text
  opaque type LicenseId = Text

  class Id[T]() extends RefType(t"ID"):
    def apply(value: Text): T throws InvalidRefError = value match
      case r"[a-z]([-.]?[a-z0-9])*" => value.asInstanceOf[T]
      case _                        => throw InvalidRefError(value, this)
    
    def unapply(value: Text): Option[T] = safely(Some(apply(value))).or(None)

  object ReleaseId extends Id[ReleaseId]()
  object BuildId extends Id[BuildId]()
  object StreamId extends Id[StreamId]()
  object ProjectId extends Id[ProjectId]()
  object ModuleId extends Id[ModuleId]()
  object CommandName extends Id[CommandName]()
  object Tag extends GitRefType[Tag](t"Git tag")
  object Branch extends GitRefType[Branch](t"Git branch")
  
  object Commit extends RefType(t"Git commit"):
    def apply(value: Text): Commit throws InvalidRefError = value match
      case r"[a-f0-9]{40}" => value.asInstanceOf[Commit]
      case _               => throw InvalidRefError(value, this)
  
  class GitRefType[T](name: Text) extends RefType(name):
    def apply(value: Text): T throws InvalidRefError = value match
      case r"[^. ^~\:\cA-\cZ][^ ^~\:\cA-\cZ]*(\/[^. ^~\:\cA-\cZ][^ ^~\:\cA-\cZ]*)*" =>
        if value.ends(t".lock") then throw InvalidRefError(value, this)
        if value.ends(t"/") then throw InvalidRefError(value, this)
        if value.contains(t"..") then throw InvalidRefError(value, this)
        
        value.asInstanceOf[T]
      
      case _ =>
        throw InvalidRefError(value, this)
  
  object Package extends RefType(t"package name"):
    def apply(value: Text): Package throws InvalidRefError = value match
      case r"[a-z][a-z0-9_]*(\.[a-z][a-z0-9_]*)*" => value.asInstanceOf[Package]
      case _                                      => throw InvalidRefError(value, this)

  object LicenseId extends Id[LicenseId]()

  given (using CanThrow[InvalidRefError]): Codec[ReleaseId] = FieldCodec(identity(_), ReleaseId(_))
  given (using CanThrow[InvalidRefError]): Codec[BuildId] = FieldCodec(identity(_), BuildId(_))
  given (using CanThrow[InvalidRefError]): Codec[ModuleId] = FieldCodec(identity(_), ModuleId(_))
  given (using CanThrow[InvalidRefError]): Codec[ProjectId] = FieldCodec(identity(_), ProjectId(_))
  given (using CanThrow[InvalidRefError]): Codec[StreamId] = FieldCodec(identity(_), StreamId(_))
  given (using CanThrow[InvalidRefError]): Codec[Tag] = FieldCodec(identity(_), Tag(_))
  given (using CanThrow[InvalidRefError]): Codec[LicenseId] = FieldCodec(identity(_), LicenseId(_))
  given (using CanThrow[InvalidRefError]): Codec[Package] = FieldCodec(identity(_), Package(_))
  given (using CanThrow[InvalidRefError]): Codec[Commit] = FieldCodec(identity(_), Commit(_))
  given (using CanThrow[InvalidRefError]): Codec[Branch] = FieldCodec(identity(_), Branch(_))
  given (using CanThrow[InvalidRefError]): Codec[CommandName] = FieldCodec(identity(_), CommandName(_))

export Ids.*

enum License(name: Text, id: LicenseId, category: Text):
  case Afl3 extends License(t"Academic Free License v3.0", lid"afl-3.0", t"afl")
  case Apache2 extends License(t"Apache license 2.0", lid"apache-2.0", t"apache")
  case Artistic2 extends License(t"Artistic license 2.0", lid"artistic-2.0", t"artistic")
  case Bsl1 extends License(t"Boost Software License 1.0", lid"bsl-1.0", t"bsl")
  case Bsd2Clause extends License(t"BSD 2-clause \"Simplified\" license", lid"bsd-2-clause", t"bsd")
  case Bsd3Clause extends License(t"BSD 3-clause \"New\" or \"Revised\" license", lid"bsd-3-clause", t"bsd")
  case Bsd3ClauseClear extends License(t"BSD 3-clause Clear license", lid"bsd-3-clause-clear", t"bsd")
  case Cc extends License(t"Creative Commons license family", lid"cc", t"cc")
  case Cc01 extends License(t"Creative Commons Zero v1.0 Universal", lid"cc0-1.0", t"cc")
  case CcBy4 extends License(t"Creative Commons Attribution 4.0", lid"cc-by-4.0", t"cc")
  case CcBySa4 extends License(t"Creative Commons Attribution Share Alike 4.0", lid"cc-by-sa-4.0", t"cc")
  case Wtfpl extends License(t"Do What The F*ck You Want To Public License", lid"wtfpl", t"wtfpl")
  case Ecl2 extends License(t"Educational Community License v2.0", lid"ecl-2.0", t"ecl")
  case Epl1 extends License(t"Eclipse Public License 1.0", lid"epl-1.0", t"epl")
  case Epl2 extends License(t"Eclipse Public License 2.0", lid"epl-2.0", t"epl")
  case Eupl11 extends License(t"European Union Public License 1.1", lid"eupl-1.1", t"eupl")
  case Agpl3 extends License(t"GNU Affero General Public License v3.0", lid"agpl-3.0", t"agpl")
  case Gpl extends License(t"GNU General Public License family", lid"gpl", t"gpl")
  case Gpl2 extends License(t"GNU General Public License v2.0", lid"gpl-2.0", t"gpl")
  case Gpl3 extends License(t"GNU General Public License v3.0", lid"gpl-3.0", t"gpl")
  case Lgpl extends License(t"GNU Lesser General Public License family", lid"lgpl", t"lgpl")
  case Lgpl21 extends License(t"GNU Lesser General Public License v2.1", lid"lgpl-2.1", t"lgpl")
  case Lgpl3 extends License(t"GNU Lesser General Public License v3.0", lid"lgpl-3.0", t"lgpl")
  case Isc extends License(t"ISC", lid"isc", t"isc")
  case Lppl13c extends License(t"LaTeX Project Public License v1.3c", lid"lppl-1.3c", t"lppl")
  case MsPl extends License(t"Microsoft Public License", lid"ms-pl", t"ms")
  case Mit extends License(t"MIT", lid"mit", t"mit")
  case Mpl extends License(t"Mozilla Public License 2.0", lid"mpl-2.0", t"mpl")
  case Osl3 extends License(t"Open Software License 3.0", lid"osl-3.0", t"osl")
  case PostgreSql extends License(t"PostgreSQL License", lid"postgresql", t"postgresql")
  case Ofl11 extends License(t"SIL Open Font License 1.1", lid"ofl-1.1", t"ofl")
  case Ncsa extends License(t"University of Illinois/NCSA Open Source License", lid"ncsa", t"ncsa")
  case Unlicense extends License(t"The Unlicense", lid"unlicense", t"unlicense")
  case Zlib extends License(t"zLib License", lid"zlib", t"zlib")

case class Release(                      id: ReleaseId,
                                         stream: StreamId,
		                                     description: Text,
                   @codlLabel("tag")     tags: List[Tag],
		                                     license: LicenseId,
		                                     date: Date,
		                                     lifetime: Int,
		                                     repo: Repo,
		               @codlLabel("provide") packages: List[Package])

case class Repo(url: Url, commit: Commit, branch: Maybe[Branch])
case class Universe(@codlLabel("release") releases: List[Release])
case class Local(@codlLabel("fork") forks: List[Fork])
case class Fork(id: BuildId, path: Relative)
case class Overlay(id: BuildId, url: Url, commit: Commit, branch: Maybe[Branch])
case class Mount(path: Relative)

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

extension (sc: StringContext)
  def lid(): LicenseId = unsafely(LicenseId(sc.parts(0).show))