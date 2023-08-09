/*
    Fury, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import fulminate.*
import gossamer.*
import anticipation.*
import digression.*
import spectacular.*
import kaleidoscope.*

export Ids.*

case class InvalidRefError(id: Text, refType: RefType)
extends Error(msg"The value $id is not a valid ${refType.name}")

case class AppError(userMessage: Message, underlyingCause: Maybe[Error])
extends Error(userMessage)

trait RefType(val name: Text)

object Ids:
  opaque type BuildId = Text
  opaque type StreamId = Text
  opaque type ProjectId = Text
  opaque type ModuleId = Text
  
  @targetName("Pkg")
  opaque type Package = Text
  
  opaque type ClassName = Text
  opaque type CommandName = Text
  opaque type Keyword = Text
  opaque type LicenseId = Text

  class Id[T]() extends RefType(t"ID"):
    def apply(value: Text): T throws InvalidRefError = value match
      case r"[a-z]([-]?[a-z0-9])*" => value.asInstanceOf[T]
      case _                       => throw InvalidRefError(value, this)
    
    def unapply(value: Text): Option[T] = safely(Some(apply(value))).or(None)

  object BuildId extends Id[BuildId]()
  object StreamId extends Id[StreamId]()
  object ProjectId extends Id[ProjectId]()
  object ModuleId extends Id[ModuleId]()
  object Keyword extends Id[Keyword]()
  object CommandName extends Id[CommandName]()
  object LicenseId extends Id[LicenseId]()

  extension (projectId: ProjectId) def apply()(using universe: Universe): Maybe[Project] =
    universe.resolve(projectId)

  class GitRefType[Type](name: Text) extends RefType(name):
    def apply(value: Text): Type throws InvalidRefError =
      value.cut(t"/").foreach: part =>
        if part.starts(t".") || part.ends(t".") then throw InvalidRefError(value, this)
        if part.ends(t".lock") then throw InvalidRefError(value, this)
        if part.contains(t"@{") then throw InvalidRefError(value, this)
        if part.contains(t"..") then throw InvalidRefError(value, this)
        if part.length == 0 then throw InvalidRefError(value, this)
        
        for ch <- List('*', '[', '\\', ' ', '^', '~', ':', '?')
        do if part.contains(ch) then throw InvalidRefError(value, this)
      
      value.asInstanceOf[Type]
  
  @targetName("Pkg")
  object Package extends RefType(t"package name"):
    def apply(value: Text): Package throws InvalidRefError = value match
      case r"[a-z][a-z0-9_]*(\.[a-z][a-z0-9_]*)*" => value.asInstanceOf[Package]
      case _                                      => throw InvalidRefError(value, this)

  object ClassName extends RefType(t"class name"):
    def apply(value: Text): Package throws InvalidRefError = value match
      case r"[a-z][a-z0-9_]*(\.[A-Za-z][A-Za-z0-9_]*)*" => value.asInstanceOf[ClassName]
      case _                                            => throw InvalidRefError(value, this)


  given buildIdShow: Show[BuildId] = identity(_)
  given buildIdEncoder: Encoder[BuildId] = identity(_)
  given buildIdDecoder(using CanThrow[InvalidRefError]): Decoder[BuildId] = BuildId(_)
  
  given moduleIdShow: Show[ModuleId] = identity(_)
  given moduleIdEncoder: Encoder[ModuleId] = identity(_)
  given moduleIdDecoder(using CanThrow[InvalidRefError]): Decoder[ModuleId] = ModuleId(_)
  
  given projectIdShow: Show[ProjectId] = identity(_)
  given projectIdEncoder: Encoder[ProjectId] = identity(_)
  given projectIdDecoder(using CanThrow[InvalidRefError]): Decoder[ProjectId] = ProjectId(_)
  
  given streamIdShow: Show[StreamId] = identity(_)
  given streamIdEncoder: Encoder[StreamId] = identity(_)
  given streamIdDecoder(using CanThrow[InvalidRefError]): Decoder[StreamId] = StreamId(_)
  
  given licenseIdShow: Show[LicenseId] = identity(_)
  given licenseIdEncoder: Encoder[LicenseId] = identity(_)
  given licenseIdDecoder(using CanThrow[InvalidRefError]): Decoder[LicenseId] = LicenseId(_)
  
  given pkgShow: Show[Package] = identity(_)
  given pkgEncoder: Encoder[Package] = identity(_)
  given pkgDecoder(using CanThrow[InvalidRefError]): Decoder[Package] = Package(_)
  
  given classNameShow: Show[ClassName] = identity(_)
  given classNameEncoder: Encoder[ClassName] = identity(_)
  given classNameDecoder(using CanThrow[InvalidRefError]): Decoder[ClassName] = ClassName(_)
  
  given commandNameShow: Show[CommandName] = identity(_)
  given commandNameEncoder: Encoder[CommandName] = identity(_)
  given commandNameDecoder(using CanThrow[InvalidRefError]): Decoder[CommandName] = CommandName(_)

  given keywordShow: Show[Keyword] = identity(_)
  given keywordEncoder: Encoder[Keyword] = identity(_)
  given keywordDecoder(using CanThrow[InvalidRefError]): Decoder[Keyword] = Keyword(_)
