package irk

import rudiments.*
import gossamer.*
import jovian.*
import slalom.*
import xylophone.*
import euphemism.*

case class Target(name: Text, module: Text, run: Text, parallel: Boolean, trigger: Boolean)

case class Publishing(username: Text, group: Text, url: Text, organization: Organization,
                          developers: List[Developer])

case class Message(module: Text, path: Text, line: Int, from: Int, to: Int, message: Text,
                       content: IArray[Char])

case class Repo(base: Text, url: Text):
  def basePath(dir: DiskPath): DiskPath = dir + Relative.parse(base)

case class Module(name: Text, id: Text, links: Option[Set[Text]], resources: Option[Set[Text]],
                      sources: Set[Text], jars: Option[Set[Text]], docs: Option[List[Text]],
                      dependencies: Option[Set[Dependency]], version: Option[Text],
                      artifact: Option[Text], main: Option[Text])

case class AppError(message: Text, cause: Maybe[Error] = Unset) extends Error(cause)

case class BuildfileError(message: Text) extends Error

case class BrokenLinkError(link: Text) extends Error:
  def message: Text = t"The reference to $link cannot be resolved"

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
