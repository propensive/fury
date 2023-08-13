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
  case LogMessage(message: Message)
  case TaskUpdate(taskId: FrontEnd.TaskId, event: TaskEvent)

enum TaskEvent:
  case Progress(stage: Text, progress: Double)
  case Complete()

export FrontEndEvent.*, TaskEvent.*

def frontEnd[ResultType](using Monitor, Stdio)(block: FrontEnd ?=> ResultType): ResultType raises CancelError =
  
  val tasks: scm.HashMap[FrontEnd.TaskId, Double] = scm.HashMap()

  val funnel: Funnel[FrontEndEvent] = Funnel()
  
  val async = Async[Unit]:
    funnel.stream.foreach:
      case LogMessage(message)                           => Io.println(message.out)
      case TaskUpdate(taskId, Complete())                => tasks.remove(taskId)
      case TaskUpdate(taskId, Progress(stage, progress)) => tasks(taskId) = progress

  block(using FrontEnd(funnel, async, tasks)).tap: _ =>
    funnel.stop()
    async.await()

object FrontEnd:
  class TaskId(name: Message)

@capability
case class FrontEnd
    (private val funnel: Funnel[FrontEndEvent], private val async: Async[Unit],
        private val tasks: scm.HashMap[FrontEnd.TaskId, Double]):

  def log(message: Message): Unit = funnel.put(LogMessage(message))

  def follow(name: Message)(stream: LazyList[TaskEvent])(using Monitor): Unit =
    val taskId = FrontEnd.TaskId(name)
    
    Async:
      stream.map:
        case Progress(stage, progress) => TaskUpdate(taskId, Progress(stage, progress))
        case Complete()                => TaskUpdate(taskId, Complete())
      .foreach(funnel.put(_))
    
def log(message: Message)(using frontEnd: FrontEnd): Unit = frontEnd.log(message)

def follow(name: Message)(stream: LazyList[TaskEvent])(using frontEnd: FrontEnd, monitor: Monitor): Unit =
  frontEnd.follow(name)(stream)