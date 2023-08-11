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

import ambience.*, systemProperties.jvm, environments.jvm
import anticipation.*
import cellulose.*
import galilei.*, fileApi.galileiApi, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import gastronomy.*
import parasite.*
import gossamer.*
import eucalyptus.*
import aviation.*
import fulminate.*
import rudiments.*, homeDirectory.jvm, workingDirectory.jvm
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8
import nonagenarian.*
import nettlesome.*
import serpentine.*, hierarchies.unixOrWindows
import spectacular.*
import punctuation.*
import perforate.*
import turbulence.*, basicIo.jvm

export gitCommands.environmentDefault

import language.experimental.captureChecking

erased given CanThrow[AppError] = ###

given (using CanThrow[AppError]): Raises[AggregateError[Error]] =
  new Raises[AggregateError[Error]]:
    def record(error: AggregateError[Error]): Unit = throw AppError(error.message, error)
    def abort(error: AggregateError[Error]): Nothing = throw AppError(error.message, error)

object Main:
  private val logo: Text = t"CiAgICAgICAgICAgICAgIBtbMzg7MjsyMTc7MTsxMDFtwrcg4oCiIOKXjyAbWzM4OzI7MjA7NjA7MTAwbeKXjyDil48g4oCiIMK3CiAgICAgICAgIBtbMzg7MjsyMTc7MTsxMDFtwrcg4pePIOKXjyAbWzM4OzI7MTEyOzQ7NDNt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDCtwogICAgICAgIBtbMzg7MjsyMjc7MTs5MW3igKIg4pePIOKXjyAbWzM4OzI7MTEyOzQ7NDNt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyAbWzM4OzI7MjEwOzExNTs3beKXjyDil48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDigKIgICAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ila3ilIDilIDilIDilIDilIDila4KICAgICAgIBtbMzg7MjsyMjc7MTs5MW3il48g4pePIOKXjyDil48gG1szODsyOzE2OzUyOzkwbeKXjyAbWzM4OzI7ODM7NDk7Mm3il48gG1szODsyOzIxMDsxMTU7N23il48g4pePIOKXjyAbWzM4OzI7MDszMzszNm3il48gG1szODsyOzE2OzUyOzkwbeKXjyDil48g4pePIOKXjyDil48gICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKUgiAg4pWt4pSA4pSA4pWvCiAgICAgIBtbMzg7MjsyMjc7MTs5MW3il48g4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsyMTA7MTE1Ozdt4pePIOKXjyDil48g4pePIBtbMzg7MjswOzMzOzM2beKXjyAbWzM4OzI7MTY7NTI7OTBt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxNjs1Mjs5MG3il48g4pePIOKXjyAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ilIIgIOKUggogICAgIBtbMzg7MjsyMzc7MTs4MW3igKIg4pePIOKXjyDil48gG1szODsyOzIyODsxMDQ7OW3il48gG1szODsyOzIxMDsxMTU7N23il48g4pePIOKXjyDil48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7MjsxNDs0ODs4NW3il48g4pePIBtbMzg7MjsxNzA7MTY4OzMxbeKXjyDil48gG1szODsyOzE0OzQ4Ozg1beKXjyDil48g4oCiICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilJTilIDilIDila7ila3ilIDilIDila4g4pWt4pSA4pSA4pWu4pWt4pSA4pSA4pWu4pSA4pSA4pSA4pWu4pWt4pSA4pSA4pWuIOKVreKUgOKUgOKVrgogICAgG1szODsyOzIzNzsxOzgxbcK3IOKXjyDil48gG1szODsyOzIxMTs0NDsxN23il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIBtbMzg7MjsyMTA7MTE1Ozdt4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsxNDs0ODs4NW3il48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxNDs0ODs4NW3il48g4pePIOKXjyDCtyAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIzilIDilIDila/ilIIgIOKUgiDilIIgIOKUguKUgiAg4pWt4pSA4pSA4pSA4pWv4pSCICDilIIg4pSCICDilIIKICAgICAbWzM4OzI7MjM3OzE7ODFt4pePIOKXjyAbWzM4OzI7MjI4OzEwNDs5beKXjyDil48g4pePIOKXjyAbWzM4OzI7MjEwOzExNTs3beKXjyDil48gG1szODsyOzIzMDsxNzU7OG3il48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyDil48gG1szODsyOzY4Ozc2OzI2beKXjyAbWzM4OzI7MTI7NDQ7ODBt4pePIOKXjyDil48g4pePICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIIgICDilIIgIOKUgiDilIIgIOKUguKUgiAg4pSCICAgIOKUgiAg4pSCIOKUgiAg4pSCCiAgICAbWzM4OzI7MjQ3OzE7NzFtwrcg4pePIBtbMzg7MjsyMjg7MTA0Ozlt4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIBtbMzg7MjsxNzA7MTY4OzMxbeKXjyDil48g4pePIBtbMzg7Mjs2ODs3NjsyNm3il48gG1szODsyOzEyOzQ0OzgwbeKXjyDil48g4pePIOKXjyDCtyAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIIgICDilIIgIOKVsOKUgOKVryAg4pSC4pSCICDilIIgICAg4pSCICDilbDilIDila8gIOKUggogICAgIBtbMzg7MjsyNDc7MTs3MW3igKIgG1szODsyOzIxMTs0NDsxN23il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIOKXjyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIOKXjyDil48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxMDs0MDs3NW3il48g4pePIOKXjyDil48g4oCiICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pWw4pSA4pSA4pWvICAg4pWw4pSA4pSA4pSA4pSA4pSA4pSA4pSA4pWv4pWw4pSA4pSA4pWvICAgIOKVsOKUgOKUgOKUgOKUgOKUkCAg4pSCCiAgICAgIBtbMzg7MjsyNDc7MTs3MW3il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIBtbMzg7MjsyMzA7MTc1Ozht4pePIBtbMzg7MjsyNDc7MTgxOzI0beKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48g4pePIBtbMzg7MjswOzMzOzM2beKXjyAbWzM4OzI7MTA7NDA7NzVt4pePIOKXjyDil48g4pePIOKXjyAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKUgiAg4pSCCiAgICAgICAbWzM4OzI7MjI4OzEwNDs5beKXjyDil48gG1szODsyOzIzMDsxNzU7OG3il48gG1szODsyOzI0NzsxODE7MjRt4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7Mjs4OzM2OzcwbeKXjyDil48g4pePIOKXjyDil48gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKVreKUgOKUgOKUgOKUgOKVryAg4pSCCiAgICAgICAgG1szODsyOzIyODsxMDQ7OW3igKIg4pePIBtbMzg7MjsyMzA7MTc1Ozht4pePIBtbMzg7MjsyNDc7MTgxOzI0beKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48gG1szODsyOzg7MzY7NzBt4pePIOKXjyDil48g4pePIOKXjyDigKIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ilbDilIDilIDilIDilIDilIDilIDilIDila8KICAgICAgICAgG1szODsyOzIyODsxMDQ7OW3CtyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7NjszMjs2NW3il48g4pePIOKXjyDil48g4pePIMK3CiAgICAgICAgICAgICAgIBtbMzg7MjsyNDc7MTgxOzI0bcK3IOKAoiDil48gG1szODsyOzE1MDsxMTc7MzRt4pePIBtbMzg7Mjs0OzI4OzYwbeKXjyDigKIgwrcKG1swbQo=".decode[Base64].uString
  
  def main(args: IArray[Text]): Unit throws StreamCutError | AggregateError[Error] =

    validate[InvalidRefError | StreamCutError | CodlReadError | DateError | MarkdownError | NumberError | IoError | PathError | GitError |
        NotFoundError | UrlError | SystemPropertyError | UndecodableCharError | UnencodableCharError | CancelError]:
      given installation: Installation = Installation((Xdg().cacheHome[Path] / p"fury").as[Directory])
      import workingDirectory.jvm
      
      internet:
        supervise:
          given Log = Log()
          val workspace = Workspace(Properties.user.dir())
          Io.println(installation.debug)
          
          Io.println(workspace.debug)
          Io.println(t"")
          Io.println(logo)
          Io.println(workspace.commands.debug)
          Io.println(workspace.mounts.debug)

          workspace.readEcosystems()
  