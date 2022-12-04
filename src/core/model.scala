package irk

import rudiments.*
import parasitism.*
import gossamer.*
import kaleidoscope.*
import joviality.*
import cellulose.*
import serpentine.*
import xylophone.*
import euphemism.*
import gastronomy.*
import escapade.*
import surveillance.*
import tetromino.*

case class Publishing(username: Text, group: Text, url: Text, organization: Maven.Organization,
                          developers: List[Maven.Developer])

case class Issue(level: Level, baseDir: DiskPath[Unix], code: CodeRange, stack: List[CodeRange], message: Text)

case class CodeRange(module: Maybe[Ref], path: Maybe[Relative], startLine: Int, from: Int, to: Int, endLine: Int,
                         content: IArray[Char])

case class Repo(base: Text, url: Text):
  def basePath(dir: DiskPath[Unix]): DiskPath[Unix] = unsafely(dir + Relative.parse(base))

object RepoPath:
  given Show[RepoPath] = rp => rp.repoId.fm(rp.path.show) { id => t"${id}:${rp.path.show}" }

  given Codec[RepoPath] with
    def schema = Field(Arity.One, RepoPath.unapply(_).isDefined)
    def serialize(value: RepoPath): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[Indexed]): RepoPath throws IncompatibleTypeError =
      RepoPath.unapply(text(value)).getOrElse(throw IncompatibleTypeError())

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

  given Codec[ModuleId] with
    def schema = Field(Arity.One, ModuleId.unapply(_).isDefined)
    def serialize(value: ModuleId): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    
    def deserialize(value: List[Indexed]): ModuleId throws IncompatibleTypeError =
      ModuleId.unapply(text(value)).getOrElse(throw IncompatibleTypeError())
  
  given Show[ModuleId] = id => id.project.fm(id.module) { p => t"${p}/${id.module}" }

case class ModuleId(project: Maybe[Text], module: Text):
  def in(id: ProjectId): Ref = Ref(project.or(id.project), module)
  def ref: Ref = Ref(project.or(throw AppError(t"Project has not been specified")), module)

object RepoId:
  def unapply(text: Text): Option[RepoId] = text match
    case r"[a-z][a-z0-9]+" => Some(RepoId(text))
    case _                 => None
  
  given Show[RepoId] = _.id
  
  given Codec[RepoId] with
    def schema = Field(Arity.One)
    def serialize(value: RepoId): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    
    def deserialize(value: List[Indexed]): RepoId throws IncompatibleTypeError =
      RepoId.unapply(text(value)).getOrElse(throw IncompatibleTypeError())


case class RepoId(id: Text)

given Codec[Relative] with
  def schema = Field(Arity.One)
  def serialize(value: Relative) = List(IArray(Node(Data(value.show))))
  def deserialize(value: List[Indexed]): Relative throws IncompatibleTypeError =
    Relative.parse(text(value))

object ProjectId:
  def unapply(text: Text): Option[ProjectId] = text match
    case r"[a-z][a-z0-9]+" => Some(ProjectId(text))
    case _                 => None
  
  given Show[ProjectId] = _.project
  
  given Codec[ProjectId] with
    def schema = Field(Arity.One)
    def serialize(value: ProjectId): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    
    def deserialize(value: List[Indexed]): ProjectId throws IncompatibleTypeError =
      ProjectId.unapply(text(value)).getOrElse(throw IncompatibleTypeError())


case class ProjectId(project: Text)

case class BuildConfig(`:<<`: Maybe[Text], universe: Maybe[Text], overlay: List[Overlay], project: List[Project],
                           command: List[Target], script: Maybe[Text])

case class Project(id: ProjectId, description: Maybe[Text], module: List[Module], repo: List[NextGen.Repo]):
  lazy val index: Map[Text, Module] = unsafely(module.indexBy(_.id.module))

case class Module(id: ModuleId, include: Set[ModuleId], use: Set[ModuleId], resource: Set[RepoPath],
                        source: Set[RepoPath], binary: Set[RepoPath], docs: Maybe[RepoPath],
                        artifact: Option[ArtifactSpec], exec: Option[Exec], plugin: List[PluginSpec],
                        main: Option[Text], publish: Option[Boolean])

case class Overlay(id: ProjectId, url: Text, commit: Text)
  //def resolve(): Directory[Unix] = GitCache(url, commit)

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
    given Json.Reader[Dependency] = summon[Json.Reader[Text]].map:
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

given Json.Writer[Digest[Crc32]] = summon[Json.Writer[Text]].contramap[Digest[Crc32]](_.encode[Base64])
given Json.Reader[Digest[Crc32]] = summon[Json.Reader[Text]].map(_.decode[Base64]).map(Digest[Crc32](_))

case class Hash(id: Ref, digest: Digest[Crc32], bin: Text)
case class Versioning(versions: List[Version])

case class Version(digest: Text, major: Int, minor: Int):
  def version: Text = t"$major.$minor"

case class PluginRef(jarFile: DiskPath[Unix], params: List[Text])

case class Target(id: Text, include: List[ModuleId], main: Maybe[Text], args: List[Text])

object NextGen:
  
  case class Project(id: Text, name: Text, description: Maybe[Text], repo: List[Repo], module: List[Module],
                         variant: List[Variant], expiry: Maybe[Int])

  case class Repo(id: RepoId, url: Text, commit: Text)
  

  case class Compiler(id: Text, version: Maybe[Text])

  case class Artifact(`type`: Text, path: Relative)
  case class Variant(id: Text)
