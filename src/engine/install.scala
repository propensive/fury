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
import anticipation.*, filesystemApi.{galileiFile, galileiPath, galileiDirectory}
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
    import encodingMitigation.strict

    tend:
      val script: Text = Properties.ethereal.name[Text]()
      val cache: Directory = (Xdg.cacheHome[Path] / PathName(script)).as[Directory]
      val configPath: Path = Home.Config() / PathName(script)

      // FIXME: This shouldn't be necessary
      given pathDecoder: CodlDecoder[Path] = CodlDecoder.field[Path]
      given ecosystemIdDecoder: CodlDecoder[EcosystemId] = CodlDecoder.field[EcosystemId]

      val configSource = (configPath / p"confix.codl").as[File].read[Text]
      val config: Config = Codl.read[Config](configSource)
      val vault: Directory = (cache / p"vault").as[Directory]
      val snapshots: Directory = (cache / p"repos").as[Directory]
      val tmp: Directory = (cache / p"tmp").as[Directory]

      val buildId: Int =
        safely((Classpath / p"build.id")().read[Text].trim.decodeAs[Int]).or:
          throw Panic(m"The build.id file was missing or corrupt")

      Installation(buildId, config, cache, vault, tmp, snapshots)

    .remedy:
      case StreamError(_)                => abort(ConfigError(m"The stream was cut while reading a file"))
      case error: AggregateError[?]      => abort(ConfigError(m"Could not read the configuration file"))
      case EnvironmentError(variable)    => abort(ConfigError(m"The environment variable $variable could not be accessed"))
      case error: CharDecodeError        => abort(ConfigError(m"The configuration file contained bad character data"))
      case error: InvalidRefError        => abort(ConfigError(m"The configuration contained a nonexistent reference"))
      case SystemPropertyError(property) => abort(ConfigError(m"The JVM system property $property could not be read."))
      case IoError(path)                 => abort(ConfigError(m"An I/O error occurred while trying to access $path"))
      case CodlReadError(label)          => abort(ConfigError(m"The field ${label.or(t"unknown")} could not be read"))
      case PathError(path, reason)       => abort(ConfigError(m"The path $path was not valid because $reason"))

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
