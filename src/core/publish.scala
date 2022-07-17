package irk

import rudiments.*
import turbulence.*
import gastronomy.*
import telekinesis.*
import euphemism.*
import joviality.*
import gossamer.*
import eucalyptus.*
import gesticulate.*
import xylophone.*
import guillotine.*

import scala.concurrent.*

import unsafeExceptions.canThrowAny

given ExecutionContext = ExecutionContext.global

case class UnknownProfile(name: Text)
extends Exception(t"the profile $name was not found for these credentials".s)

case class ProfileId(id: Text, repoTargetId: Text)
case class RepoId(id: Text)

object Sonatype:
  def publish(build: Build, passwordOpt: Option[Text])(using Stdout, Internet): Unit =
    val password = passwordOpt.getOrElse:
      throw AppError(t"The environment variable SONATYPE_PASSWORD is not set")
    
    build.linearization.groupBy(_.publish(build)).foreach:
      case (pub, steps) =>
        val sonatype = Sonatype(pub.username, password, pub.group)
        Out.println(t"Using Sonatype settings ${sonatype.toString}")
        val profileId = sonatype.profile()
        val repoId = sonatype.start(profileId)
            
        steps.foreach:
          step =>
            Out.println(t"Generating POM file for ${step.id}")
            
            val pomXml = Pom(build, step, 2021,
                t"https://propensive.com/opensource/${step.projectId}",
                t"github.com/${step.projectId}", pub).xml
            
            pomXml.show.bytes.writeTo(step.pomFile.file(Create))
            
            val srcFiles: List[Text] = step.sources.to(List).flatMap: dir =>
              Out.println(t"Adding source dir $dir")
              dir.path.descendantFiles(!_.name.startsWith(t".")).filter: file =>
                file.name.endsWith(t".scala") || file.name.endsWith(t".java")
              .flatMap: file =>
                List(t"-C", dir.show, file.path.relativeTo(dir.path).show)
            
            sh"jar cf ${step.srcsPkg} $srcFiles".exec[ExitStatus]()
            
            val docFiles: List[Text] = step.docs.to(List).flatMap: dir =>
              Out.println(t"Adding doc dir $dir")
              dir.descendantFiles(!_.name.startsWith(t".")).flatMap: file =>
                List(t"-C", dir.show, file.path.relativeTo(dir).show)
            
            sh"jar cf ${step.docFile} $docFiles".exec[ExitStatus]()

            List(step.docFile, step.pomFile, /*step.pkg,*/ step.srcsPkg).foreach: file =>
              sh"gpg -ab $file".exec[ExitStatus]()

            Out.println(t"Publishing ${step.id}")
            val dir = t"${step.id.sub(t"/", t"-")}/${step.version}"

            val uploads = List(/*step.pkg, */step.pomFile, step.docFile, step.srcsPkg).map(_.file(Expect))
            val signedUploads = uploads ++ uploads.map(_.path.rename(_+t".asc").file(Expect))

            val checksums = signedUploads.map: file =>
              val checksum = file.path.rename(_+t".sha1")
              sh"sha1sum $file".exec[Text]().take(40).bytes.writeTo(checksum.file(Create))
              checksum.file(Expect)

            sonatype.deploy(repoId, dir, signedUploads ++ checksums)
        
        sonatype.finish(profileId, repoId)
        sonatype.activity(repoId)
        sonatype.promote(profileId, repoId)

case class Sonatype(username: Text, password: Text, profileName: Text,
                        domain: Text = t"oss.sonatype.org"):

  val servicePath: Text = t"service/local/staging"

  val auth = RequestHeader.Authorization(t"Basic ${t"$username:$password".bytes.encode[Base64]}")
  val acceptJson = RequestHeader.Accept(media"application/json".show)
  val jsonContent = RequestHeader.ContentType(media"application/json".show)

  def profile()(using Internet): ProfileId throws AppError =
    Log.info(t"Getting Profile ID for $username")
    val json = Json.parse(uri"https://$domain/$servicePath/profiles".get(auth, acceptJson).as[Text])
    val profile = json.data.as[List[Json]].find(_.name.as[Text] == profileName).getOrElse(throw UnknownProfile(profileName))
    val profileId = ProfileId(profile.id.as[Text], profile.repositoryTargetId.as[Text])
    Log.info(t"Got profile ID ${profileId.id} and target ID ${profileId.repoTargetId}")
    profileId

  def start(profileId: ProfileId)(using Log, Internet): RepoId throws AppError =
    case class Payload(description: Text)
    case class Data(data: Payload)
    val input = Data(Payload(t"")).json
    Log.info(t"Starting a new session for ${profileId.id}")
    val json = Json.parse(uri"https://$domain/$servicePath/profiles/${profileId.id}/start".post(auth, acceptJson)(input).as[Text])
    val output = json.data.stagedRepositoryId.as[Text]
    Log.info(t"Got repository ID $output")
    RepoId(output)
  
  def deploy(repoId: RepoId, dir: Text, files: List[File[Unix]])(using Log, Internet): Unit =
    val futures = for file <- files yield Future:
      val uri = uri"https://$domain/$servicePath/deployByRepositoryId/${repoId.id}/${profileName.sub(t".", t"/").nn}/$dir/${file.name}"
      Log.info(t"Uploading file $file to $uri")
      uri.put(auth)(file.read[DataStream](5.mb))
      Log.info(t"Finished uploading $file")

    Await.result(Future.sequence(futures), duration.Duration.Inf)

  def finish(profileId: ProfileId, repoId: RepoId)(using Internet): Text =
    case class Data(data: Payload)
    case class Payload(description: Text, stagedRepositoryId: Text, targetRepositoryId: Text)
    uri"https://$domain/$servicePath/profiles/${profileId.id}/finish".post(auth, jsonContent, acceptJson)(Data(Payload(t"", repoId.id, profileId.repoTargetId)).json).as[Text]
  
  def activity(repoId: RepoId)(using Internet): Unit =
    val events = Json.parse(uri"https://$domain/$servicePath/repository/${repoId.id}".get(auth, acceptJson).as[Text])
    if !events.transitioning.as[Boolean] || events.`type`.as[Text] == t"closed"
    then Log.info("Finished")
    else
      Log.info(events.show)
      Thread.sleep(1000)
      activity(repoId)

  def promote(profileId: ProfileId, repoId: RepoId)(using Internet): Text =
    case class Data(data: Payload)
    case class Payload(description: Text, stagedRepositoryId: Text, targetRepositoryId: Text)
    uri"https://$domain/$servicePath/profiles/${profileId.id}/promote".post(auth, jsonContent, acceptJson)(Data(Payload(t"", repoId.id, profileId.repoTargetId)).json).as[Text]
  
