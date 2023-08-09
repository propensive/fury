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

import acyclicity.*

import galilei.*, filesystemOptions.{createNonexistent, createNonexistentParents,
    dereferenceSymlinks}

import anticipation.*, fileApi.galileiApi
import rudiments.*
import fulminate.*
import ambience.*, environments.jvm, systemProperties.jvm
import digression.*
import gossamer.*
import eucalyptus.*
import gastronomy.*
import turbulence.*
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.strict
import imperial.*
import serpentine.*, hierarchies.unixOrWindows
import cellulose.*
import spectacular.*
import symbolism.*
import nettlesome.*
import nonagenarian.*

trait Compiler
trait Packager
trait Executor
trait Cloner
trait Fetcher

object Installation:
  def apply(cacheDir: Directory): Installation throws AppError =
    try
      val configDir: Path = Home.Config() / p"fury"
      val configFile: File = (configDir / p"config.codl").as[File]
      val vaultDir: Directory = (cacheDir / p"vault").as[Directory]
      val libDir: Directory = (cacheDir / p"lib").as[Directory]
      val tmpDir: Directory = (cacheDir / p"tmp").as[Directory]
    
      Installation(configFile, cacheDir, vaultDir, libDir, tmpDir)
    
    catch
      case error: StreamCutError =>
        throw AppError(msg"The stream was cut while reading a file", error)
      
      case error: EnvironmentError => error match
        case EnvironmentError(variable) =>
          throw AppError(msg"The environment variable $variable could not be accessed", error)
      
      case error: SystemPropertyError => error match
        case SystemPropertyError(property) =>
           throw AppError(msg"The JVM system property $property could not be accessed", error)
      
      case error: IoError => error match
        case IoError(path) =>
          throw AppError(msg"An I/O error occurred while trying to access $path", error)
      
      case error: PathError => error match
        case PathError(reason) =>
          throw AppError(msg"The path was not valid because $reason", error)
    
case class Installation
    (configFile: File, cacheDir: Directory, vaultDir: Directory, libDir: Directory,
        tmpDir: Directory):
  
  def libJar(hash: Digest[Crc32]): File throws IoError =
    unsafely(libDir / t"${hash.encodeAs[Hex].lower}.jar").as[File]

inline def installation(using Installation): Installation = summon[Installation]

object Workspace:
  def apply(path: Path)(using Stdio): Workspace throws IoError | InvalidRefError | NotFoundError | UrlError | PathError | StreamCutError | UnencodableCharError | UndecodableCharError | CodlReadError | NumberError | AggregateError[CodlError] =
    val dir: Directory = path.as[Directory]
    val buildFile: File = (dir / p"fury2").as[File]

    val buildDoc: CodlDoc = Codl.parse(buildFile)
    val build: Build = Codl.read[Build](buildFile)
    val localPath: Path = dir / p".local"
    val localFile: Maybe[File] = if localPath.exists() then localPath.as[File] else Unset
    val local: Maybe[Local] = localFile.mm(Codl.read[Local](_))

    Workspace(dir, buildDoc, build, local)

case class Workspace(dir: Directory, buildDoc: CodlDoc, build: Build, local: Maybe[Local]):
  lazy val commands: Map[CommandName, Command] = unsafely(build.commands.indexBy(_.name))
  lazy val projects: Map[ProjectId, Project] = unsafely(build.projects.indexBy(_.id))
  lazy val mounts: Map[WorkPath, Mount] = unsafely(build.mounts.indexBy(_.path))

  def apply
      (path: WorkPath)
      (using Installation, Internet, WorkingDirectory, Log)
      : Directory throws IoError | PathError | GitError =
    mounts.keys.find(_.precedes(path)) match
      case None        => (dir.path + path).as[Directory]
      case Some(mount) => Cache(mounts(mount).repo)
    

enum Phase:
  case Clone(repo: GitSnapshot, cloner: Cloner)
  case Compile(sources: Set[Path], compiler: Compiler)
  case Package(binary: Path)
  case Execute(classpath: Set[Path], main: ClassName)

case class Plan(phases: Dag[Phase])

case class Universe(projects: Map[Ids.ProjectId, Project]):
  def resolve(project: ProjectId): Maybe[Project] = projects.getOrElse(project, Unset)

object Universe:
  def read(vault: Vault, build: Build): Universe =
    Universe(Map())
    
