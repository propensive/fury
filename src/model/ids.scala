/*
    Fury, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package fury

import rudiments.*
import vacuous.*
import fulminate.*
import gossamer.*
import anticipation.*
import gastronomy.*
import galilei.*
import contingency.*
import spectacular.*
import kaleidoscope.*
import hieroglyph.*, charEncoders.utf8

export Ids.*

case class InvalidRefError(id: Text, refType: RefType)
extends Error(msg"The value '$id' is not a valid ${refType.name}")

case class AppError(userMessage: Message, underlyingCause: Optional[Error] = Unset)
extends Error(userMessage)

case class UnknownRefError(ref: ProjectId) extends Error(msg"the reference ${ref.show} could not be resolved")

case class BuildfileError(path: Path, issues: List[Error])
extends Error(msg"There were problems with the build file $path")

trait RefType(val name: Text)

object Ids:
  opaque type GoalId      = Text
  opaque type EcosystemId = Text
  opaque type StreamId    = Text
  opaque type ProjectId   = Text
  opaque type ActionName = Text
  opaque type Keyword    = Text
  opaque type LicenseId  = Text

  class Id[IdType]() extends RefType(t"ID"):
    def apply(value: Text)(using Raises[InvalidRefError]): IdType = value match
      case r"[a-z]([-]?[a-z0-9])*" => value.asInstanceOf[IdType]
      case _                       => raise(InvalidRefError(value, this))(value.asInstanceOf[IdType])
    
    def unapply(value: Text): Option[IdType] = safely(Some(apply(value))).or(None)

  object EcosystemId extends Id[EcosystemId]()
  object StreamId extends Id[StreamId]()
  object ProjectId extends Id[ProjectId]()
  object GoalId extends Id[GoalId]()
  object Keyword extends Id[Keyword]()
  object ActionName extends Id[ActionName]()

  class GitRefType[Type](ref: Text) extends RefType(ref):
    def apply(value: Text)(using Raises[InvalidRefError]): Type =
      value.cut(t"/").each: part =>
        if part.starts(t".") || part.ends(t".")
        then raise(InvalidRefError(value, this))(GitRefType[Type](value))
        
        if part.ends(t".lock") then raise(InvalidRefError(value, this))(GitRefType[Type](value))
        if part.contains(t"@{") then raise(InvalidRefError(value, this))(GitRefType[Type](value))
        if part.contains(t"..") then raise(InvalidRefError(value, this))(GitRefType[Type](value))
        if part.length == 0 then raise(InvalidRefError(value, this))(GitRefType[Type](value))
        
        for ch <- List('*', '[', '\\', ' ', '^', '~', ':', '?')
        do if part.has(ch) then raise(InvalidRefError(value, this))(GitRefType[Type](value))
      
      value.asInstanceOf[Type]
  
  object LicenseId extends RefType(t"license ID"):
    def unsafe(value: Text) = value.asInstanceOf[LicenseId]
    def apply(value: Text)(using Raises[InvalidRefError]): LicenseId = value match
      case r"[a-z]([-.]?[a-z0-9])*" => value.asInstanceOf[LicenseId]
      
      case _ =>
        raise(InvalidRefError(value, this))(value.asInstanceOf[LicenseId])
  
  given ecosystemIdShow: Show[EcosystemId] = identity(_)
  given ecosystemIdEncoder: Encoder[EcosystemId] = identity(_)
  given ecosystemIdDecoder(using Raises[InvalidRefError]): Decoder[EcosystemId] = EcosystemId(_)
  given ecosystemIdDigestible: Digestible[EcosystemId] = (acc, ecosystemId) => acc.append(ecosystemId.bytes)
  
  given projectIdShow: Show[ProjectId] = identity(_)
  given projectIdEncoder: Encoder[ProjectId] = identity(_)
  given projectIdDecoder(using Raises[InvalidRefError]): Decoder[ProjectId] = ProjectId(_)
  given projectIdDigestible: Digestible[ProjectId] = (acc, projectId) => acc.append(projectId.bytes)
  
  given streamIdShow: Show[StreamId] = identity(_)
  given streamIdEncoder: Encoder[StreamId] = identity(_)
  given streamIdDecoder(using Raises[InvalidRefError]): Decoder[StreamId] = StreamId(_)
  given streamIdDigestible: Digestible[StreamId] = (acc, streamId) => acc.append(streamId.bytes)
  
  given licenseIdShow: Show[LicenseId] = identity(_)
  given licenseIdEncoder: Encoder[LicenseId] = identity(_)
  given licenseIdDecoder(using Raises[InvalidRefError]): Decoder[LicenseId] = LicenseId(_)
  given licenseIdDigestible: Digestible[LicenseId] = (acc, licenseId) => acc.append(licenseId.bytes)
  
  given actionNameShow: Show[ActionName] = identity(_)
  given actionNameEncoder: Encoder[ActionName] = identity(_)
  given actionNameDecoder(using Raises[InvalidRefError]): Decoder[ActionName] = ActionName(_)
  given actionNameDigestible: Digestible[ActionName] = (acc, actionName) => acc.append(actionName.bytes)

  given keywordShow: Show[Keyword] = identity(_)
  given keywordEncoder: Encoder[Keyword] = identity(_)
  given keywordDecoder(using Raises[InvalidRefError]): Decoder[Keyword] = Keyword(_)
  given keywordDigestible: Digestible[Keyword] = (acc, keyword) => acc.append(keyword.bytes)

  given goalIdShow: Show[GoalId] = identity(_)
  given goalIdEncoder: Encoder[GoalId] = identity(_)
  given goalIdDigestible: Digestible[GoalId] = (acc, goalId) => acc.append(goalId.bytes)
  given goalIdDecoder(using Raises[InvalidRefError]): Decoder[GoalId] = GoalId(_)