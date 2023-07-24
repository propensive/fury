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
import gossamer.*
import anticipation.*
import digression.*
import chiaroscuro.*
import spectacular.*
import cellulose.*
import kaleidoscope.*

export Ids.*

case class InvalidRefError(id: Text, refType: RefType)
extends Error(msg"The value $id is not a valid ${refType.name}")

case class AppError(userMsg: Text) extends Error(msg"An application error occurred: $userMsg")

trait RefType(val name: Text)

object Ids:
  opaque type BuildId = Text
  opaque type StreamId = Text
  opaque type ProjectId = Text
  opaque type ModuleId = Text
  opaque type Tag = Text
  
  @targetName("Pkg")
  opaque type Package = Text
  
  opaque type ClassName = Text
  opaque type Branch = Text
  opaque type Commit = Text
  opaque type CommandName = Text
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
  object CommandName extends Id[CommandName]()
  object Tag extends GitRefType[Tag](t"Git tag")
  object Branch extends GitRefType[Branch](t"Git branch")

  extension (projectId: ProjectId) def apply()(using universe: Universe): Maybe[Project] =
    universe.resolve(projectId)

  object Commit extends RefType(t"Git commit"):
    def apply(value: Text): Commit throws InvalidRefError = value match
      case r"[a-f0-9]{40}" => value.asInstanceOf[Commit]
      case _               => throw InvalidRefError(value, this)
  
  class GitRefType[T](name: Text) extends RefType(name):
    def apply(value: Text): T throws InvalidRefError =
      value.cut(t"/").foreach: part =>
        if part.starts(t".") || part.ends(t".") then throw InvalidRefError(value, this)
        if part.ends(t".lock") then throw InvalidRefError(value, this)
        if part.contains(t"@{") then throw InvalidRefError(value, this)
        if part.contains(t"..") then throw InvalidRefError(value, this)
        if part.length == 0 then throw InvalidRefError(value, this)
        
        for ch <- List('*', '[', '\\', ' ', '^', '~', ':', '?')
        do if part.contains(ch) then throw InvalidRefError(value, this)
      
      value.asInstanceOf[T]
  
  @targetName("Pkg")
  object Package extends RefType(t"package name"):
    def apply(value: Text): Package throws InvalidRefError = value match
      case r"[a-z][a-z0-9_]*(\.[a-z][a-z0-9_]*)*" => value.asInstanceOf[Package]
      case _                                      => throw InvalidRefError(value, this)

  object ClassName extends RefType(t"class name"):
    def apply(value: Text): Package throws InvalidRefError = value match
      case r"[a-z][a-z0-9_]*(\.[a-z][a-z0-9_]*)*" => value.asInstanceOf[ClassName]
      case _                                      => throw InvalidRefError(value, this)

  object LicenseId extends Id[LicenseId]()

  given Show[BuildId] = identity(_)
  given Show[ModuleId] = identity(_)
  given Show[ProjectId] = identity(_)
  given Show[StreamId] = identity(_)
  given Show[Tag] = identity(_)
  given Show[LicenseId] = identity(_)
  given Show[Package] = identity(_)
  given Show[ClassName] = identity(_)
  given Show[Commit] = identity(_)
  given Show[Branch] = identity(_)
  given Show[CommandName] = identity(_)

  given buildId(using CanThrow[InvalidRefError]): Codec[BuildId] =
    FieldCodec(identity(_), BuildId(_))
  
  given moduleId(using CanThrow[InvalidRefError]): Codec[ModuleId] =
    FieldCodec(identity(_), ModuleId(_))
  
  given projectId(using CanThrow[InvalidRefError]): Codec[ProjectId] =
    FieldCodec(identity(_), ProjectId(_))
  
  given streamId(using CanThrow[InvalidRefError]): Codec[StreamId] =
    FieldCodec(identity(_), StreamId(_))
  
  given tag(using CanThrow[InvalidRefError]): Codec[Tag] = FieldCodec(identity(_), Tag(_))
  
  given licenseId(using CanThrow[InvalidRefError]): Codec[LicenseId] =
    FieldCodec(identity(_), LicenseId(_))
  
  given pkg(using CanThrow[InvalidRefError]): Codec[Package] = FieldCodec(identity(_), Package(_))
  
  given className(using CanThrow[InvalidRefError]): Codec[ClassName] =
    FieldCodec(identity(_), ClassName(_))
  
  given commit(using CanThrow[InvalidRefError]): Codec[Commit] = FieldCodec(identity(_), Commit(_))
  given branch(using CanThrow[InvalidRefError]): Codec[Branch] = FieldCodec(identity(_), Branch(_))
  
  given commandName(using CanThrow[InvalidRefError]): Codec[CommandName] =
    FieldCodec(identity(_), CommandName(_))
