package fury

import rudiments.*
import parasitism.*
import gossamer.*
import kaleidoscope.*
import galilei.*
import cellulose.*
import serpentine.*
import deviation.*
import xylophone.*
import jacinta.*
import gastronomy.*
import escapade.*
import surveillance.*

import language.adhocExtensions

case class Publishing(username: Text, group: Text, url: Text, organization: Maven.Organization,
                          developers: List[Maven.Developer])

case class Issue(level: Level, baseDir: DiskPath, code: CodeRange, stack: List[CodeRange], message: Text)

case class CodeRange(module: Maybe[Ref], path: Maybe[Relative], startLine: Int, from: Int, to: Int, endLine: Int,
                         content: IArray[Char])

case class Repo(base: Text, url: Text):
  def basePath(dir: DiskPath): DiskPath = unsafely(dir + Relative.parse(base))

case class InvalidIdError(id: Text) extends Error(err"The value $id was not a valid ID")

object RepoPath:
  given Show[RepoPath] = rp => rp.repoId.fm(rp.path.show) { id => t"${id}:${rp.path.show}" }

  given (using CanThrow[InvalidIdError]): Codec[RepoPath] =
    new FieldCodec[RepoPath](_.show, v => RepoPath.unapply(v).getOrElse(throw InvalidIdError(v))):
      override val schema = Field(Arity.One, RepoPath.unapply(_).isDefined)

  def unapply(text: Text): Option[RepoPath] = text match
    case r"$repo@([a-z][a-z0-9]*):$path@(.+)" => Some(RepoPath(repo.show, Relative.parse(path.show)))
    case r"$path@(.+)"                        => Some(RepoPath(Unset, Relative.parse(path.show)))
    case _                                    => None

case class RepoPath(repoId: Maybe[Text], path: Relative)

object ModuleId:
  def unapply(text: Text): Option[ModuleId] = text match
    case r"$p@([a-z][a-z0-9]+)\/$m@([a-z][a-z0-9]+)" => Some(ModuleId(p.show, m.show))
    case r"[a-z][a-z0-9]+"                           => Some(ModuleId(Unset, text))
    case text                                        => None

  given (using CanThrow[InvalidIdError]): Codec[ModuleId] =
    FieldCodec[ModuleId](_.show, v => ModuleId.unapply(v).getOrElse(throw InvalidIdError(v)))
  
  given Show[ModuleId] = id => id.project.fm(id.module) { p => t"${p}/${id.module}" }

case class ModuleId(project: Maybe[Text], module: Text):
  def in(id: ProjectId): Ref = Ref(project.or(id.project), module)
  def ref: Ref = Ref(project.or(throw AppError(t"Project has not been specified")), module)

object RepoId:
  def unapply(text: Text): Option[RepoId] = text match
    case r"[a-z][a-z0-9]+" => Some(RepoId(text))
    case _                 => None
  
  given Show[RepoId] = _.id
  
  given (using CanThrow[InvalidIdError]): Codec[RepoId] =
    FieldCodec[RepoId](_.show, v => RepoId.unapply(v).getOrElse(throw InvalidIdError(v)))

case class RepoId(id: Text)

given Codec[Relative] =
  FieldCodec[Relative](_.show, Relative.parse(_))

object ProjectId:
  def unapply(text: Text): Option[ProjectId] = text match
    case r"[a-z][a-z0-9]+" => Some(ProjectId(text))
    case _                 => None
  
  given Show[ProjectId] = _.project
  
  given (using CanThrow[InvalidIdError]): Codec[ProjectId] =
    FieldCodec[ProjectId](_.show, v => ProjectId.unapply(v).getOrElse(throw InvalidIdError(v)))

case class ProjectId(project: Text)

case class PackageName(pkg: Text)

case class Intro(terminator: Text, header: Maybe[Text])

case class BuildConfig(@targetName("prelude") `:<<`: Maybe[Intro], universe: Maybe[Text],
                           overlay: List[Overlay], project: List[Project], command: List[Target],
                           script: Maybe[Text])

case class Project(id: ProjectId, description: Maybe[Text], module: List[Module], repo: List[GitRepo]):
  lazy val index: Map[Text, Module] = unsafely(module.indexBy(_.id.module))

case class Module(id: ModuleId, include: Set[ModuleId], use: Set[ModuleId], resource: Set[RepoPath],
                        source: Set[RepoPath], binary: Set[RepoPath], docs: Maybe[RepoPath],
                        artifact: Option[ArtifactSpec], exec: Option[Exec], plugin: List[PluginSpec],
                        main: Option[Text], publish: Option[Boolean], js: Maybe[Boolean])

case class Overlay(url: Text, commit: Text, project: ProjectId)
  //def resolve(): Directory = GitCache(url, commit)

case class Fork(id: Text, path: Relative)

case class Forks(fork: List[Fork])

case class ArtifactSpec(path: Text, main: Option[Text], format: Option[Text])

case class Exec(browsers: List[Text], url: Option[Text], start: Text, stop: Option[Text])

case class PluginSpec(id: Text, params: Option[List[Text]])

object Plugin:
  def apply(spec: PluginSpec): Plugin throws BuildfileError = spec.id.only:
    case Ref(ref) => Plugin(ref, spec.params.presume)
  .getOrElse:
    throw BuildfileError(t"The plugin name was not valid")

case class Plugin(id: Ref, params: List[Text])

object Ref:
  def apply(text: Text): Ref throws BuildfileError = unapply(text).getOrElse:
    throw BuildfileError(t"Id '$text' does not have the format <project>/<module>")

  def unapply(text: Text): Option[Ref] = text.only:
    case r"$project@([a-z](-?[a-z0-9])*)\/$module@([a-z](-?[a-z0-9])*)" => Ref(project.show, module.show)
  
  given Show[Ref] = ref => t"${ref.project}/${ref.module}"

case class Ref(project: Text, module: Text):
  def dashed: Text = t"$project-$module"

object AppError:
  def apply(msg: Text, originalCause: Maybe[Error[?]] = Unset): AppError =
    AppError(msg.ansi, originalCause)

case class AppError(appMsg: AnsiText, originalCause: Maybe[Error[?]])
extends Error(err"an application error occurred: $appMsg", originalCause)

case class BuildfileError(bfMsg: Text) extends Error(err"the build file contained an error: $bfMsg")
case class BrokenLinkError(link: Ref) extends Error(err"the reference to $link cannot be resolved")

object Maven:

  object Dependency:
    given JsonReader[Dependency] = summon[JsonReader[Text]].map:
      case s"$group:$artifact:$version" => Dependency(group.show, artifact.show, version.show)
      case value                        => throw AppError(t"Could not parse dependency $value")

  @xmlLabel("organization")
  case class Organization(name: Text, url: Text)

  @xmlLabel("license")
  case class License(name: Text, url: Text, distribution: Text)

  @xmlLabel("scm")
  case class Scm(url: Text, connection: Text)

  @xmlLabel("developer")
  case class Developer(id: Text, name: Text, url: Text)

  @xmlLabel("dependency")
  case class Dependency(groupId: Text, artifactId: Text, version: Text)

  @xmlLabel("project")
  case class Project(modelVersion: Text, groupId: Text, artifactId: Text, version: Text,
      licenses: List[License], name: Text, description: Text, inceptionYear: Text, url: Text,
      organization: Organization, scm: Scm, developers: List[Developer],
      dependencies: List[Dependency])

  object Pom:
    def apply(build: Build, step: Step, year: Int, url: Text, git: Text, publishing: Publishing)
             : Project =
      Project(
        modelVersion = t"4.0.0",
        groupId = step.group(build),
        artifactId = step.id.dashed,
        version = step.version,
        licenses = List(License(t"Apache 2", t"http://www.apache.org/licenses/LICENSE-2.0.txt", t"repo")),
        name = step.name,
        description = step.name,
        inceptionYear = year.show,
        url = url,
        organization = publishing.organization,
        scm = Scm(t"https://$git/", t"scm:git:git@$git.git"),
        developers = publishing.developers,
        dependencies = step.pomDependencies(build)
      )

enum Event:
  case Changeset(changes: List[WatchEvent])
  case Interrupt
  case Resize(width: Int)

enum Result:
  case Aborted
  case Incomplete
  case Complete(issueSet: Set[Issue] = Set())
  case Terminal(message: AnsiText)

  @targetName("plus")
  def +(result: Result): Result = this match
    case Aborted        => Aborted
    case Incomplete     => result
    case Terminal(msg)  => this
    case Complete(msgs) => result match
      case Aborted          => Aborted
      case Complete(msgs2)  => Complete(msgs ++ msgs2)
      case Incomplete       => this
      case Terminal(msg)    => result

  def success: Boolean = this match
    case Complete(_) | Incomplete => issues.count(_.level == Level.Error) == 0
    case _                        => false
  
  def issues: Set[Issue] = this match
    case Complete(issues) => issues
    case _                => Set()
  
  def errors: Set[Issue] = issues.filter(_.level == Level.Error)

given JsonWriter[Digest[Crc32]] = summon[JsonWriter[Text]].contraMap[Digest[Crc32]](_.encode[Base64])
given JsonReader[Digest[Crc32]] = summon[JsonReader[Text]].map(_.decode[Base64]).map(Digest[Crc32](_))

case class Hash(id: Ref, digest: Digest[Crc32], bin: Text)
case class Versioning(versions: List[Version])

case class Version(digest: Text, major: Int, minor: Int):
  def version: Text = t"$major.$minor"

case class PluginRef(jarFile: DiskPath, params: List[Text])

case class Target(id: Text, include: List[ModuleId], main: Maybe[Text], args: List[Text])

case class GitRepo(id: RepoId, url: Text, commit: Text)
