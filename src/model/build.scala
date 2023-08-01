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
import ambience.*, environments.jvm, systemProperties.jvm
import digression.*
import gossamer.*
import gastronomy.*
import turbulence.*
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.strict
import imperial.*
import serpentine.*, hierarchies.unixOrWindows
import cellulose.*
import spectacular.*
import symbolism.*
import nettlesome.*

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
        throw AppError(t"The stream was cut while reading a file")
      
      case error: EnvironmentError =>
        throw AppError(t"An expected JVM environment variable could not be accessed")
      
      case error: SystemPropertyError =>
        throw AppError(t"An expected JVM system property could not be accessed")
      
      case error: IoError =>
        throw AppError(t"An I/O error occurred")
      
      case error: PathError => error match
        case PathError(path) =>
          throw AppError(t"the path $path was invalid")
    
case class Installation
    (configFile: File, cacheDir: Directory, vaultDir: Directory, libDir: Directory,
        tmpDir: Directory):
  
  def libJar(hash: Digest[Crc32]): File throws IoError =
    unsafely(libDir / t"${hash.encodeAs[Hex].lower}.jar").as[File]

case class Workspace(id: BuildId, path: Path)

object BuildSpec:
  def apply(path: Path): BuildSpec throws IoError | InvalidRefError | NotFoundError | UrlError | PathError | StreamCutError | UnencodableCharError | UndecodableCharError | CodlReadError | NumberError | AggregateError[CodlError] =
    val dir: Directory = path.as[Directory]
    val buildFile: File = (dir / p"fury").as[File]

    val build: Build = Codl.read[Build](buildFile)
    val localPath: Path = dir / p".local"
    val localFile: Maybe[File] = if localPath.exists() then localPath.as[File] else Unset
    val local: Maybe[Local] = localFile.mm(Codl.read[Local](_))

    BuildSpec(dir, build, local)

case class BuildSpec(dir: Directory, build: Build, local: Maybe[Local]):
  lazy val commands: Map[CommandName, Command] = unsafely(build.commands.indexBy(_.name))
  lazy val projects: Map[ProjectId, Project] = unsafely(build.projects.indexBy(_.id))
  lazy val mounts: Map[Path, Mount] = unsafely(build.mounts.indexBy(dir.path + _.path))

enum Phase:
  case Clone(repo: GitRepo, cloner: Cloner)
  case Compile(sources: Set[Path], compiler: Compiler)
  case Package(binary: Path)
  case Execute(classpath: Set[Path], main: ClassName)

case class Plan(phases: Dag[Phase])

case class Universe(projects: Map[Ids.ProjectId, Project]):
  def resolve(project: ProjectId): Maybe[Project] = projects.getOrElse(project, Unset)

object Universe:
  def read(vault: Vault, build: Build): Universe =
    Universe(Map())
    
