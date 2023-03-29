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
import galilei.*, filesystems.unix
import anticipation.*, fileApi.galileiApi
import rudiments.*
import ambience.*, environments.system
import deviation.*
import gossamer.*
import gastronomy.*
import turbulence.*, characterEncodings.utf8, badEncodingHandlers.strict
import imperial.*
import serpentine.*
import cellulose.*
import telekinesis.*

trait Compiler
trait Packager
trait Executor
trait Cloner
trait Fetcher

object Installation:
  def apply(cacheDir: Directory): Installation throws AppError =
    try
      val configDir: Directory = (Home.Config() / p"fury").directory(Ensure)
      val configPath: DiskPath = configDir / p"config.codl"
      val vaultDir: Directory = (cacheDir / p"vault").directory(Ensure)
      val libDir: Directory = (cacheDir / p"lib").directory(Ensure)
      val tmpDir: Directory = (cacheDir / p"tmp").directory(Ensure)
    
      Installation(configPath, cacheDir, vaultDir, libDir, tmpDir)
    catch
      case err: StreamCutError =>
        throw AppError(t"The stream was cut wwhile reading a file")
      case err: IoError =>
        throw AppError(t"Could not initialize the application due to an I/O error: ${err.message}")
    
case class Installation
    (configPath: DiskPath, cacheDir: Directory, vaultDir: Directory, libDir: Directory, tmpDir: Directory):
  def libJar(hash: Digest[Crc32]): File throws IoError =
    unsafely(libDir / t"${hash.encode[Hex].lower}.jar").file(Expect)

case class Root(id: BuildId, path: DiskPath)

object BuildSpec:
  def apply(path: DiskPath): BuildSpec throws IoError | InvalidRefError | InvalidUrlError | StreamCutError | BadEncodingError | CodlReadError | AggregateError[CodlError] =
    val dir: Directory = path.directory(Expect)
    val buildFile: File = (dir / p"fury").file(Expect)
    val build: Build = Codl.read[Build](buildFile)
    
    val localPath: DiskPath = dir / p".local"
    val localFile: Maybe[File] = if localPath.exists() then localPath.file(Expect) else Unset
    val local: Maybe[Local] = localFile.mm(Codl.read[Local](_))

    BuildSpec(dir, build, local)

case class BuildSpec(dir: Directory, build: Build, local: Maybe[Local]):
  lazy val commands: Map[CommandName, Command] = unsafely(build.commands.indexBy(_.name))
  lazy val projects: Map[ProjectId, Project] = unsafely(build.projects.indexBy(_.id))
  lazy val mounts: Map[DiskPath, Mount] = unsafely(build.mounts.indexBy(dir.path + _.path))

enum Phase:
  case Clone(repo: GitRepo, cloner: Cloner)
  case Compile(sources: Set[DiskPath], compiler: Compiler)
  case Package(binary: DiskPath)
  case Execute(classpath: Set[DiskPath], main: ClassName)

case class Plan(phases: Dag[Phase])

case class Universe(projects: Map[Ids.ProjectId, Project]):
  def resolve(project: ProjectId): Maybe[Project] = projects.getOrElse(project, Unset)

object Universe:
  def read(vault: Vault, build: Build): Universe =
    Universe(Map())
    
