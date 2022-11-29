package irk

import rudiments.*
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

case class Target(name: Text, module: Text, run: Text, parallel: Boolean, trigger: Boolean)

case class Publishing(username: Text, group: Text, url: Text, organization: Organization,
                          developers: List[Developer])

case class Issue(level: Level, baseDir: DiskPath[Unix], code: CodeRange, stack: List[CodeRange], message: Text)

case class CodeRange(module: Maybe[Ref], path: Maybe[Relative], startLine: Int, from: Int, to: Int, endLine: Int,
                         content: IArray[Char])

case class Repo(base: Text, url: Text):
  def basePath(dir: DiskPath[Unix]): DiskPath[Unix] = unsafely(dir + Relative.parse(base))

case class Module(name: Text, id: Text, links: Option[Set[Text]], resources: Option[Set[Text]],
                      sources: Set[Text], jars: Option[Set[Text]], docs: Option[List[Text]],
                      dependencies: Option[Set[Dependency]], version: Option[Text],
                      artifact: Option[ArtifactSpec], exec: Option[Exec], plugins: Option[List[PluginSpec]],
                      main: Option[Text])

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

@xmlLabel("organization")
case class Organization(name: Text, url: Text)

@xmlLabel("license")
case class License(name: Text, url: Text, distribution: Text)

@xmlLabel("scm")
case class Scm(url: Text, connection: Text)

@xmlLabel("developer")
case class Developer(id: Text, name: Text, url: Text)

object Dependency:
  given Json.Reader[Dependency] = summon[Json.Reader[Text]].map:
    case s"$group:$artifact:$version" => Dependency(group.show, artifact.show, version.show)
    case value                        => throw AppError(t"Could not parse dependency $value")

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

object NextGen:
  
  val buildSchema = summon[Codec[Build]].schema

  def read(file: File[Unix])(using Allocator): (cellulose.Doc, Build) throws IoError =
    val source = unsafely(file.read[Text]())
    try
      val doc = buildSchema.parse(source)
      (doc, doc.as[Build])
    catch
      case err: CodlValidationError => throw AppError(t"Couldn't read build file: ${err.toString.show}")
      case err: CodlParseError      => throw AppError(t"Couldn't read build file: ${err.toString.show}")

  given Codec[Directory[Unix]] with
    def schema = Field(Arity.One)
    def serialize(dir: Directory[Unix]): List[IArray[Node]] = List(IArray(Node(Data(dir.fullname))))
    def deserialize(value: List[IArray[Node]]): Directory[Unix] = unsafely(Unix.parse(readField(value).option.get).directory(Expect))

  given Codec[DiskPath[Unix]] with
    def schema = Field(Arity.One)
    def serialize(dir: DiskPath[Unix]): List[IArray[Node]] = List(IArray(Node(Data(dir.fullname))))
    def deserialize(value: List[IArray[Node]]): DiskPath[Unix] = unsafely(Unix.parse(readField(value).option.get))
  
  given Codec[Relative] with
    def schema = Field(Arity.One)
    def serialize(dir: Relative): List[IArray[Node]] = List(IArray(Node(Data(dir.show))))
    def deserialize(value: List[IArray[Node]]): Relative = unsafely(Relative.parse(readField(value).option.get))

  case class Build(`:<<` : Text, project: List[Project], script: Text)
  case class Project(id: Text, name: Maybe[Text], count: Int, module: List[Module], stream: Maybe[Text])
  case class Module(id: Text)//, sources: List[Relative], include: List[Text], use: List[Text],
                        //`export`: List[Text])