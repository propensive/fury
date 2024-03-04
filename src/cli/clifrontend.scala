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

import profanity.*, terminalOptions.terminalSizeDetection
import vacuous.*
import acyclicity.*
import turbulence.*
import exoskeleton.*
import rudiments.*
import parasite.*
import anticipation.*

def frontEnd[ResultType](lambda: FrontEnd ?=> Terminal ?=> ResultType)(using Cli, Monitor): ResultType =
  terminal(lambda(using CliFrontEnd()))

class CliFrontEnd()(using terminal: Terminal) extends FrontEnd:
  private var graph: Optional[Dag[Task]] = Unset
  private var active: List[Task] = Nil
  given stdio: Stdio = terminal.stdio

  def schedule(task: Task): Unit = ()
  def update(task: Task): Unit = ()
  def complete(task: Task): Unit = ()
  def info[InfoType: Printable](info: InfoType) = Out.println(info)
