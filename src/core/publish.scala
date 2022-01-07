package vex

import rudiments.*
import gastronomy.*
import scintillate.*
import euphemism.*
import jovian.*
import gossamer.*
import eucalyptus.*
import gesticulate.*

import scala.concurrent.*

import unsafeExceptions.canThrowAny

given ExecutionContext = ExecutionContext.global

case class UnknownProfile(name: Text)
extends Exception(t"the profile $name was not found for these credentials".s)

case class ProfileId(id: Text, repoTargetId: Text)
case class RepoId(id: Text)

case class Sonatype(username: Text, password: Text, profileName: Text,
                        domain: Text = t"oss.sonatype.org"):

  val servicePath: Text = t"service/local/staging"

  val auth = RequestHeader.Authorization(t"Basic ${t"$username:$password".bytes.encode[Base64]}")
  val acceptJson = RequestHeader.Accept(media"application/json".show)
  val jsonContent = RequestHeader.ContentType(media"application/json".show)

  def profile(): ProfileId throws AppError =
    Log.info(t"Getting Profile ID for $username")
    val json = Json.parse(uri"https://$domain/$servicePath/profiles".get(auth, acceptJson).as[Text])
    val profile = json.data.as[List[Json]].find(_.name.as[Text] == profileName).getOrElse(throw UnknownProfile(profileName))
    val profileId = ProfileId(profile.id.as[Text], profile.repositoryTargetId.as[Text])
    Log.info(t"Got profile ID ${profileId.id} and target ID ${profileId.repoTargetId}")
    profileId

  def start(profileId: ProfileId)(using Log): RepoId throws AppError =
    case class Payload(description: Text)
    case class Data(data: Payload)
    val input = Data(Payload(t"")).json
    Log.info(t"Starting a new session for ${profileId.id}")
    val json = Json.parse(uri"https://$domain/$servicePath/profiles/${profileId.id}/start".post(auth, acceptJson)(input).as[Text])
    val output = json.data.stagedRepositoryId.as[Text]
    Log.info(t"Got repository ID $output")
    RepoId(output)
  
  def deploy(repoId: RepoId, dir: Text, files: List[Unix.File])(using Log): Unit =
    val futures = for file <- files yield Future {
      val uri = uri"https://$domain/$servicePath/deployByRepositoryId/${repoId.id}/${profileName.sub(t".", t"/").nn}/$dir/${file.name}"
      Log.info(t"Uploading file $file to $uri")
      uri.put(auth)(file.read[DataStream](5.mb))
      Log.info(t"Finished uploading $file")
    }
    Await.result(Future.sequence(futures), duration.Duration.Inf)

  def finish(profileId: ProfileId, repoId: RepoId): Text =
    case class Data(data: Payload)
    case class Payload(description: Text, stagedRepositoryId: Text, targetRepositoryId: Text)
    uri"https://$domain/$servicePath/profiles/${profileId.id}/finish".post(auth, jsonContent, acceptJson)(Data(Payload(t"", repoId.id, profileId.repoTargetId)).json).as[Text]
  
  
  def activity(repoId: RepoId): Unit =
    val events = Json.parse(uri"https://$domain/$servicePath/repository/${repoId.id}".get(auth, acceptJson).as[Text])
    if !events.transitioning.as[Boolean] || events.`type`.as[Text] == t"closed"
    then Log.info("Finished")
    else
      Log.info(events.show)
      Thread.sleep(1000)
      activity(repoId)
