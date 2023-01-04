package fury.model

import rudiments.*
import gossamer.*
import cellulose.*
import kaleidoscope.*

case class InvalidRefError(id: Text, refType: RefType)
extends Error(err"The value $id is not a valid ${refType.name}")

trait RefType(val name: Text)

object Ids:
  opaque type BuildId = Text
  opaque type StreamId = Text
  opaque type ProjectId = Text
  opaque type ModuleId = Text
  opaque type Tag = Text
  opaque type Package = Text
  opaque type Branch = Text
  opaque type Commit = Text
  opaque type CommandName = Text
  opaque type LicenseId = Text

  class Id[T]() extends RefType(t"ID"):
    def apply(value: Text): T throws InvalidRefError = value match
      case r"[a-z]([-.]?[a-z0-9])*" => value.asInstanceOf[T]
      case _                        => throw InvalidRefError(value, this)
    
    def unapply(value: Text): Option[T] = safely(Some(apply(value))).or(None)

  object BuildId extends Id[BuildId]()
  object StreamId extends Id[StreamId]()
  object ProjectId extends Id[ProjectId]()
  object ModuleId extends Id[ModuleId]()
  object CommandName extends Id[CommandName]()
  object Tag extends GitRefType[Tag](t"Git tag")
  object Branch extends GitRefType[Branch](t"Git branch")
  
  object Commit extends RefType(t"Git commit"):
    def apply(value: Text): Commit throws InvalidRefError = value match
      case r"[a-f0-9]{40}" => value.asInstanceOf[Commit]
      case _               => throw InvalidRefError(value, this)
  
  class GitRefType[T](name: Text) extends RefType(name):
    def apply(value: Text): T throws InvalidRefError = value match
      case r"[^. ^~\:\cA-\cZ][^ ^~\:\cA-\cZ]*(\/[^. ^~\:\cA-\cZ][^ ^~\:\cA-\cZ]*)*" =>
        if value.ends(t".lock") then throw InvalidRefError(value, this)
        if value.ends(t"/") then throw InvalidRefError(value, this)
        if value.contains(t"..") then throw InvalidRefError(value, this)
        
        value.asInstanceOf[T]
      
      case _ =>
        throw InvalidRefError(value, this)
  
  object Package extends RefType(t"package name"):
    def apply(value: Text): Package throws InvalidRefError = value match
      case r"[a-z][a-z0-9_]*(\.[a-z][a-z0-9_]*)*" => value.asInstanceOf[Package]
      case _                                      => throw InvalidRefError(value, this)

  object LicenseId extends Id[LicenseId]()

  given (using CanThrow[InvalidRefError]): Codec[BuildId] = FieldCodec(identity(_), BuildId(_))
  given (using CanThrow[InvalidRefError]): Codec[ModuleId] = FieldCodec(identity(_), ModuleId(_))
  given (using CanThrow[InvalidRefError]): Codec[ProjectId] = FieldCodec(identity(_), ProjectId(_))
  given (using CanThrow[InvalidRefError]): Codec[StreamId] = FieldCodec(identity(_), StreamId(_))
  given (using CanThrow[InvalidRefError]): Codec[Tag] = FieldCodec(identity(_), Tag(_))
  given (using CanThrow[InvalidRefError]): Codec[LicenseId] = FieldCodec(identity(_), LicenseId(_))
  given (using CanThrow[InvalidRefError]): Codec[Package] = FieldCodec(identity(_), Package(_))
  given (using CanThrow[InvalidRefError]): Codec[Commit] = FieldCodec(identity(_), Commit(_))
  given (using CanThrow[InvalidRefError]): Codec[Branch] = FieldCodec(identity(_), Branch(_))
  given (using CanThrow[InvalidRefError]): Codec[CommandName] = FieldCodec(identity(_), CommandName(_))

export Ids.*
