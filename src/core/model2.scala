package fury.model

import rudiments.*
import gossamer.*
import kaleidoscope.*
import temporaneous.*

/*
case class label(name: String) extends StaticAnnotation

enum License(name: Text, id: Text):
  case Afl3 extends License(t"Academic Free License v3.0", t"afl-3.0")
  case Apache2 extends License(t"Apache license 2.0", t"apache-2.0")
  case Artistic2 extends License(t"Artistic license 2.0", t"artistic-2.0")
  case Bsl1 extends License(t"Boost Software License 1.0", t"bsl-1.0")
  case Bsd2Clause extends License(t"BSD 2-clause \"Simplified\" license", t"bsd-2-clause")
  case Bsd3Clause extends License(t"BSD 3-clause \"New\" or \"Revised\" license", t"bsd-3-clause")
  case Bsd3ClauseClear extends License(t"BSD 3-clause Clear license", t"bsd-3-clause-clear")
  case Cc extends License(t"Creative Commons license family", t"cc")
  case Cc01 extends License(t"Creative Commons Zero v1.0 Universal", t"cc0-1.0")
  case CcBy4 extends License(t"Creative Commons Attribution 4.0", t"cc-by-4.0")
  case CcBySa4 extends License(t"Creative Commons Attribution Share Alike 4.0", t"cc-by-sa-4.0")
  case Wtfpl extends License(t"Do What The F*ck You Want To Public License", t"wtfpl")
  case Ecl2 extends License(t"Educational Community License v2.0", t"ecl-2.0")
  case Epl1 extends License(t"Eclipse Public License 1.0", t"epl-1.0")
  case Epl2 extends License(t"Eclipse Public License 2.0", t"epl-2.0")
  case Eupl11 extends License(t"European Union Public License 1.1", t"eupl-1.1")
  case Agpl3 extends License(t"GNU Affero General Public License v3.0", t"agpl-3.0")
  case Gpl extends License(t"GNU General Public License family", t"gpl")
  case Gpl2 extends License(t"GNU General Public License v2.0", t"gpl-2.0")
  case Gpl3 extends License(t"GNU General Public License v3.0", t"gpl-3.0")
  case Lgpl extends License(t"GNU Lesser General Public License family", t"lgpl")
  case Lgpl21 extends License(t"GNU Lesser General Public License v2.1", t"lgpl-2.1")
  case Lgpl3 extends License(t"GNU Lesser General Public License v3.0", t"lgpl-3.0")
  case Isc extends License(t"ISC", t"isc")
  case Lppl13c extends License(t"LaTeX Project Public License v1.3c", t"lppl-1.3c")
  case MsPl extends License(t"Microsoft Public License", t"ms-pl")
  case Mit extends License(t"MIT", t"mit")
  case Mpl extends License(t"Mozilla Public License 2.0", t"mpl-2.0")
  case Osl3 extends License(t"Open Software License 3.0", t"osl-3.0")
  case PostgreSql extends License(t"PostgreSQL License", t"postgresql")
  case Ofl11 extends License(t"SIL Open Font License 1.1", t"ofl-1.1")
  case Ncsa extends License(t"University of Illinois/NCSA Open Source License", t"ncsa")
  case Unlicense extends License(t"The Unlicense", t"unlicense")
  case Zlib extends License(t"zLib License", t"zlib")

case class InvalidIdError(id: Text) extends Error(err"The value $id is not a valid identifier")

object Ids:
  opaque type ReleaseId = Text
  opaque type StreamId = Text
  opaque type ProjectId = Text
  opaque type ModuleId = Text
  opaque type Command = Text
  opaque type Tag = Text
  opaque type Package = Text
  opaque type Branch = Text
  opaque type Commit = Text
  opaque type CommandName = Text

  class Id[T]():
    def apply(value: Text): T throws InvalidIdError = value match
      case r"[a-z](-?[a-z0-9])*" => value.asInstanceOf[T]
      case _                     => throw InvalidIdError

  object ReleaseId extends Id[ReleaseId]()
  object StreamId extends Id[StreamId]()
  object ProjectId extends Id[ProjectId]()
  object ModuleId extends Id[ModuleId]()
  object Command extends Id[Command]()
  object Tag extends Id[Tag]()

export Ids.*

case class Release(id: ReleaseId,
                   stream: StreamId,
		   description: Text, 
		   @label("tag") tags: List[Tag],
		   license: License,
		   date: Date,
		   lifetime: Int,
		   repo: GitRepo,
		   @label("provide") packages: List[Package])

case class GitRepo(url: Url, commit: Commit, branch: Maybe[Branch])
case class Universe(@label("release") releases: List[Release])

case class Build(@label(":<<")     prelude:  Maybe[Prelude],
                 @label("overlay") overlays: List[Overlay],
                 @label("command") commands: List[Command],
		                               default:  Maybe[CommandName],
		             @label("project") projects: List[Project])

case class Overlay()
case class Prelude(ignore: List[Text])
case class Project(id: ProjectId, module: List[Module], )
case class Module(id: ModuleId)
case class ModuleRef(projectId: Text, moduleId: ModuleId)
*/