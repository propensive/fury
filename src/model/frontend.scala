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

import anticipation.*
import fulminate.*
import gossamer.*
import parasite.*
import perforate.*
import rudiments.*
import turbulence.*
import escapade.*

import scala.collection.mutable as scm

import language.experimental.captureChecking

enum FrontEndEvent:
  case Redraw(columns: Int, rows: Int)
  case LogMessage(message: Message)
  case TrackTask(id: Text)
  case UpdateTask(id: Text, progress: Double)
  case RemoveTask(id: Text)

export FrontEndEvent.*

def frontEnd
    [ResultType]
    (using monitor: Monitor, stdio: Stdio, cancel: Raises[CancelError])
    (block: FrontEnd ?=> ResultType)
    : ResultType^ =
  
  val funnel: Funnel[FrontEndEvent] = Funnel()
  val tasks: scm.HashMap[Text, Double] = scm.HashMap()

  val async = Async[Unit]:
    funnel.stream.foreach:
      case LogMessage(message)      => Io.println(message.out)
      case TrackTask(id)            => Io.println(msg"Started tracking $id".out)
      case UpdateTask(id, progress) => Io.println(msg"Progress for $id = ${(progress*100).toInt}".out)
      case RemoveTask(id)           => tasks -= id
      case Redraw(columns, rows)    =>
        acquiesce()
        Io.println(t"Redrawing...")

  block(using FrontEnd(funnel, async, tasks)).tap: _ =>
    funnel.stop()
    async.await()

@capability
case class FrontEnd
    (private val funnel: Funnel[FrontEndEvent], private val async: Async[Unit],
        private val tasks: scm.HashMap[Text, Double]):

  def log(message: Message): Unit = funnel.put(LogMessage(message))

def log(message: Message)(using frontEnd: FrontEnd): Unit = frontEnd.log(message)