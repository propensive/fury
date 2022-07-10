package irk

import rudiments.*
import gossamer.*
import joviality.*
import serpentine.*
import xylophone.*
import euphemism.*
import escapade.*
import surveillance.*

case class Target(name: Text, module: Text, run: Text, parallel: Boolean, trigger: Boolean)

case class Publishing(username: Text, group: Text, url: Text, organization: Organization,
                          developers: List[Developer])

case class Issue(level: Level, module: Text, baseDir: DiskPath[Unix], path: Relative, startLine: Int, from: Int, to: Int,
                     endLine: Int, message: Text, content: IArray[Char])

case class Repo(base: Text, url: Text):
  def basePath(dir: DiskPath[Unix]): DiskPath[Unix] = unsafely(dir + Relative.parse(base))

case class Module(name: Text, id: Text, links: Option[Set[Text]], resources: Option[Set[Text]],
                      sources: Set[Text], jars: Option[Set[Text]], docs: Option[List[Text]],
                      dependencies: Option[Set[Dependency]], version: Option[Text],
                      artifact: Option[ArtifactSpec], exec: Option[Exec])

case class ArtifactSpec(path: Text, main: Option[Text], format: Option[Text])

case class Exec(browsers: List[Text], url: Option[Text], start: Text, stop: Option[Text])

object AppError:
  def apply(msg: Text, originalCause: Maybe[Error[?]] = Unset): AppError =
    AppError(msg.ansi, originalCause)

case class AppError(msg: AnsiText, originalCause: Maybe[Error[?]])
extends Error((t"an application error occurred: ", msg), originalCause)

case class BuildfileError(msg: Text) extends Error((t"the build file contained an error: ", msg))

case class BrokenLinkError(link: Text)
extends Error((t"the reference to ", link, t" cannot be resolved"))

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
      artifactId = step.dashed,
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
  case Complete(issueList: List[Issue])
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
  
  def issues: List[Issue] = this match
    case Complete(issues) => issues
    case _                => Nil
  
  def errors: List[Issue] = issues.filter(_.level == Level.Error)

enum Level:
  case Error, Warn, Info
