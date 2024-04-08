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

import ambience.*, environments.virtualMachine
import anticipation.*, filesystemInterfaces.galileiApi
import cellulose.*
import fulminate.*

import galilei.*, filesystemOptions.{createNonexistent, createNonexistentParents,
    dereferenceSymlinks}

import gossamer.*
import hieroglyph.*, charDecoders.utf8
import imperial.*
import hellenism.*, classloaders.threadContext
import contingency.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*
import vacuous.*

object Installation:
  def apply()(using HomeDirectory, SystemProperties): Installation raises ConfigError =
    import badEncodingHandlers.strict

    given (ConfigError fixes StreamError) = error =>
      ConfigError(msg"The stream was cut while reading a file")
    
    given (ConfigError fixes EnvironmentError) =
      case EnvironmentError(variable) =>
        ConfigError(msg"The environment variable $variable could not be accessed")
    
    given (ConfigError fixes UndecodableCharError) = error =>
      ConfigError(msg"The configuration file contained bad character data")
    
    given (ConfigError fixes SystemPropertyError) =
      case SystemPropertyError(property) =>
        ConfigError(msg"The JVM system property $property could not be read.")
    
    given (ConfigError fixes IoError) =
      case IoError(path) => ConfigError(msg"An I/O error occurred while trying to access $path")
    
    given (ConfigError fixes CodlReadError) =
      case CodlReadError(label) =>
        ConfigError(msg"The field ${label.or(t"unknown")} could not be read")
    
    given (ConfigError fixes PathError) =
      case PathError(path, reason) => ConfigError(msg"The path $path was not valid because $reason")
    
    given (ConfigError fixes InvalidRefError) =
      case InvalidRefError(ref, refType) => ConfigError(msg"$ref is not valid")
    
    val script = unsafely(Properties.ethereal.name[Text]())
    val cache: Directory = (Xdg.cacheHome[Path] / PathName(script)).as[Directory]
    val configPath: Path = Home.Config() / PathName(script)
    val config: Config = Codl.read[Config]((configPath / p"config.codl").as[File])
    val vault: Directory = (cache / p"vault").as[Directory]
    val snapshots: Directory = (cache / p"repos").as[Directory]
    val tmp: Directory = (cache / p"tmp").as[Directory]
    
    val buildId: Int =
      safely((Classpath / p"build.id")().readAs[Text].trim.decodeAs[Int]).or:
        throw Panic(msg"The build.id file was missing or corrupt")
    
    Installation(buildId, config, cache, vault, tmp, snapshots)
    
case class Installation
   (buildId:   Int,
    config:    Config,
    cache:     Directory,
    vault:     Directory,
    tmp:       Directory,
    snapshots: Directory):

  val build: Path = cache.path / p"build"
  val work: Path = cache.path / p"work"
  val basis: Path = cache.path / p"basis"
  
inline def installation(using inline installation: Installation): Installation = installation
