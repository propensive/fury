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

import ambience.*, environments.virtualMachine, systemProperties.virtualMachine
import anticipation.*, filesystemInterfaces.galileiApi
import cellulose.*
import fulminate.*
import galilei.*, filesystemOptions.{createNonexistent, createNonexistentParents, dereferenceSymlinks}
import gossamer.*
import hieroglyph.*, charDecoders.utf8
import imperial.*
import contingency.*
import rudiments.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import turbulence.*
import vacuous.*

object Installation:
  def apply()(using HomeDirectory): Installation raises ConfigError =
    import badEncodingHandlers.strict

    given (ConfigError fixes StreamError) = error => ConfigError(msg"The stream was cut while reading a file")
    
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
      case CodlReadError(label) => ConfigError(msg"The field ${label.or(t"unknown")} could not be read")
    
    given (ConfigError fixes PathError) =
      case PathError(path, reason) => ConfigError(msg"The path $path was not valid because $reason")
    
    val cache = (Xdg.cacheHome[Path] / p"fury").as[Directory]
    val configPath: Path = Home.Config() / p"fury"
    val config: Config = Codl.read[Config]((configPath / p"config.codl").as[File])
    val vault: Directory = (cache / p"vault").as[Directory]
    val snapshots: Directory = (cache / p"repos").as[Directory]
    val lib: Directory = (cache / p"lib").as[Directory]
    val tmp: Directory = (cache / p"tmp").as[Directory]
    
    Installation(config, cache, vault, lib, tmp, snapshots)
    
case class Installation
    (config: Config, cache: Directory, vault: Directory, lib: Directory, tmp: Directory,
        snapshots: Directory)
  
inline def installation(using inline installation: Installation): Installation = installation
