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
import profanity.*
import turbulence.*
import ambience.*
import exoskeleton.*
import fulminate.*
import galilei.*
import gastronomy.*
import gossamer.*
import anticipation.*
import hellenism.*, classloaders.threadContext
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.skip
import vacuous.*
import eucalyptus.*
import serpentine.*
import spectral.*
import escapade.*
import perforate.*

case class UserError(userMessage: Message) extends Error(userMessage)

def installInteractive
    (force: Boolean, noTabCompletions: Boolean)
    (using DaemonService[?], Terminal, Log[Output], SystemProperties, Environment, WorkingDirectory,
        HomeDirectory, Effectful)
    : ExitStatus raises UserError =
  mitigate:
    case InstallError(reason) => UserError(msg"Installation was not possible because $reason.")
    case DismissError()       => UserError(msg"Installation was aborted.")
  .within:
    val directories = Installer.candidateTargets().map(_.path)
    if directories.length <= 1 then Installer.install(force)
    else
    Out.println(e"$Italic(Please choose an install location.)")
    val menu = SelectMenu(directories, directories.head)
    val (target, events2) = menu.ask(tty.events)
    Out.println(e"Installing to $target/${service.scriptName}")
    Out.println(Installer.install(force = true, target).communicate)
    if !noTabCompletions then Out.println(TabCompletions.install(force = true).communicate)
    ExitStatus.Ok

def installBatch
    (force: Boolean, noTabCompletions: Boolean)
    (using DaemonService[?], Stdio, Log[Output], SystemProperties, Environment, WorkingDirectory, HomeDirectory,
        Effectful)
    : ExitStatus raises UserError =
  mitigate:
    case InstallError(reason) => UserError(msg"Installation was not possible because $reason.")
  .within:
    Out.println(Installer.install(force).communicate)
    if !noTabCompletions then Out.println(TabCompletions.install(force = true).communicate)
    ExitStatus.Ok

def initializeBuild(directory: Path)(using Stdio): ExitStatus raises UserError =
  Out.println(t"Creating a new build in $directory")
  ExitStatus.Ok

def cleanCache()(using Stdio): ExitStatus raises UserError =
  Out.println(t"Cleaning the cache")
  ExitStatus.Ok

def cacheDetails()(using Stdio): ExitStatus raises UserError =
  Out.println(t"Details of the cache")
  ExitStatus.Ok

def runBuild()(using Stdio): ExitStatus raises UserError =
  Out.println(t"Running the build...")
  ExitStatus.Ok

def invalidSubcommand(command: Argument)(using Stdio): ExitStatus raises UserError =
  abort(UserError(msg"${command()} is not a valid subcommand."))

def missingSubcommand()(using Stdio): ExitStatus raises UserError =
  abort(UserError(msg"No subcommand was specified."))

def about()(using Stdio): ExitStatus =
  val logo: Text = t"CiAgICAgICAgICAgICAgIBtbMzg7MjsyMTc7MTsxMDFtwrcg4oCiIOKXjyAbWzM4OzI7MjA7NjA7MTAwbeKXjyDil48g4oCiIMK3CiAgICAgICAgIBtbMzg7MjsyMTc7MTsxMDFtwrcg4pePIOKXjyAbWzM4OzI7MTEyOzQ7NDNt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDCtwogICAgICAgIBtbMzg7MjsyMjc7MTs5MW3igKIg4pePIOKXjyAbWzM4OzI7MTEyOzQ7NDNt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyAbWzM4OzI7MjEwOzExNTs3beKXjyDil48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7MjsxODs1Njs5NW3il48g4pePIOKXjyDigKIgICAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ila3ilIDilIDilIDilIDilIDila4KICAgICAgIBtbMzg7MjsyMjc7MTs5MW3il48g4pePIOKXjyDil48gG1szODsyOzE2OzUyOzkwbeKXjyAbWzM4OzI7ODM7NDk7Mm3il48gG1szODsyOzIxMDsxMTU7N23il48g4pePIOKXjyAbWzM4OzI7MDszMzszNm3il48gG1szODsyOzE2OzUyOzkwbeKXjyDil48g4pePIOKXjyDil48gICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKUgiAg4pWt4pSA4pSA4pWvCiAgICAgIBtbMzg7MjsyMjc7MTs5MW3il48g4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsyMTA7MTE1Ozdt4pePIOKXjyDil48g4pePIBtbMzg7MjswOzMzOzM2beKXjyAbWzM4OzI7MTY7NTI7OTBt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxNjs1Mjs5MG3il48g4pePIOKXjyAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ilIIgIOKUggogICAgIBtbMzg7MjsyMzc7MTs4MW3igKIg4pePIOKXjyDil48gG1szODsyOzIyODsxMDQ7OW3il48gG1szODsyOzIxMDsxMTU7N23il48g4pePIOKXjyDil48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7MjsxNDs0ODs4NW3il48g4pePIBtbMzg7MjsxNzA7MTY4OzMxbeKXjyDil48gG1szODsyOzE0OzQ4Ozg1beKXjyDil48g4oCiICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilJTilIDilIDila7ila3ilIDilIDila4g4pWt4pSA4pSA4pWu4pWt4pSA4pSA4pWu4pSA4pSA4pSA4pWu4pWt4pSA4pSA4pWuIOKVreKUgOKUgOKVrgogICAgG1szODsyOzIzNzsxOzgxbcK3IOKXjyDil48gG1szODsyOzIxMTs0NDsxN23il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIBtbMzg7MjsyMTA7MTE1Ozdt4pePIOKXjyDil48gG1szODsyOzgzOzQ5OzJt4pePIBtbMzg7MjsxNDs0ODs4NW3il48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxNDs0ODs4NW3il48g4pePIOKXjyDCtyAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIzilIDilIDila/ilIIgIOKUgiDilIIgIOKUguKUgiAg4pWt4pSA4pSA4pSA4pWv4pSCICDilIIg4pSCICDilIIKICAgICAbWzM4OzI7MjM3OzE7ODFt4pePIOKXjyAbWzM4OzI7MjI4OzEwNDs5beKXjyDil48g4pePIOKXjyAbWzM4OzI7MjEwOzExNTs3beKXjyDil48gG1szODsyOzIzMDsxNzU7OG3il48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyDil48gG1szODsyOzY4Ozc2OzI2beKXjyAbWzM4OzI7MTI7NDQ7ODBt4pePIOKXjyDil48g4pePICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIIgICDilIIgIOKUgiDilIIgIOKUguKUgiAg4pSCICAgIOKUgiAg4pSCIOKUgiAg4pSCCiAgICAbWzM4OzI7MjQ3OzE7NzFtwrcg4pePIBtbMzg7MjsyMjg7MTA0Ozlt4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIBtbMzg7MjsxNzA7MTY4OzMxbeKXjyDil48g4pePIBtbMzg7Mjs2ODs3NjsyNm3il48gG1szODsyOzEyOzQ0OzgwbeKXjyDil48g4pePIOKXjyDCtyAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pSCICDilIIgICDilIIgIOKVsOKUgOKVryAg4pSC4pSCICDilIIgICAg4pSCICDilbDilIDila8gIOKUggogICAgIBtbMzg7MjsyNDc7MTs3MW3igKIgG1szODsyOzIxMTs0NDsxN23il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIOKXjyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIOKXjyDil48gG1szODsyOzE3MDsxNjg7MzFt4pePIOKXjyAbWzM4OzI7Njg7NzY7MjZt4pePIBtbMzg7MjsxMDs0MDs3NW3il48g4pePIOKXjyDil48g4oCiICAgICAbWzM4OzI7MTUwOzE1MDsxNTBt4pWw4pSA4pSA4pWvICAg4pWw4pSA4pSA4pSA4pSA4pSA4pSA4pSA4pWv4pWw4pSA4pSA4pWvICAgIOKVsOKUgOKUgOKUgOKUgOKUkCAg4pSCCiAgICAgIBtbMzg7MjsyNDc7MTs3MW3il48gG1szODsyOzIyODsxMDQ7OW3il48g4pePIBtbMzg7MjsyMzA7MTc1Ozht4pePIBtbMzg7MjsyNDc7MTgxOzI0beKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48g4pePIBtbMzg7MjswOzMzOzM2beKXjyAbWzM4OzI7MTA7NDA7NzVt4pePIOKXjyDil48g4pePIOKXjyAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKUgiAg4pSCCiAgICAgICAbWzM4OzI7MjI4OzEwNDs5beKXjyDil48gG1szODsyOzIzMDsxNzU7OG3il48gG1szODsyOzI0NzsxODE7MjRt4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48gG1szODsyOzA7MzM7MzZt4pePIBtbMzg7Mjs4OzM2OzcwbeKXjyDil48g4pePIOKXjyDil48gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgG1szODsyOzE1MDsxNTA7MTUwbeKVreKUgOKUgOKUgOKUgOKVryAg4pSCCiAgICAgICAgG1szODsyOzIyODsxMDQ7OW3igKIg4pePIBtbMzg7MjsyMzA7MTc1Ozht4pePIBtbMzg7MjsyNDc7MTgxOzI0beKXjyDil48g4pePIOKXjyAbWzM4OzI7MTcwOzE2ODszMW3il48gG1szODsyOzg7MzY7NzBt4pePIOKXjyDil48g4pePIOKXjyDigKIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIBtbMzg7MjsxNTA7MTUwOzE1MG3ilbDilIDilIDilIDilIDilIDilIDilIDila8KICAgICAgICAgG1szODsyOzIyODsxMDQ7OW3CtyAbWzM4OzI7MjQ3OzE4MTsyNG3il48g4pePIOKXjyDil48g4pePIOKXjyAbWzM4OzI7NjszMjs2NW3il48g4pePIOKXjyDil48g4pePIMK3CiAgICAgICAgICAgICAgIBtbMzg7MjsyNDc7MTgxOzI0bcK3IOKAoiDil48gG1szODsyOzE1MDsxMTc7MzRt4pePIBtbMzg7Mjs0OzI4OzYwbeKXjyDigKIgwrcKG1swbQo=".decode[Base64].uString
  Out.println(logo)
  val buildId = safely:
    val resource = Classpath / p"spectral" / p"build.id"
    resource().readAs[Text]
  
  Out.println(e"$Bold(Build ID ${buildId.or(t"unknown")})")
  ExitStatus.Ok

def versionInfo()(using Stdio): ExitStatus = 
  Out.println(t"Fury version 1.0")
  ExitStatus.Ok

