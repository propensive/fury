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

import anticipation.*, timeApi.aviationApi
import fulminate.*
import gossamer.*
import parasite.*
import iridescence.*
import perforate.*
import quantitative.*
import rudiments.*
import turbulence.*
import hieroglyph.*, textWidthCalculation.uniform
import escapade.*
import spectacular.*

import language.experimental.captureChecking

enum FrontEndEvent:
  case LogMessage(message: Text)
  case TaskUpdate(taskId: FrontEnd.TaskId, event: TaskEvent)
  case Render

enum TaskEvent:
  case Progress(stage: Text, progress: Double)
  case Complete()

export FrontEndEvent.*, TaskEvent.*

def frontEnd[ResultType](using Monitor, Stdio)(block: FrontEnd ?=> ResultType): ResultType raises CancelError =
  val frontEnd = FrontEnd()
  block(using frontEnd).also(frontEnd.stop())
  
object FrontEnd:
  class TaskId(val name: Message)

@capability
case class FrontEnd()(using Monitor, Stdio):
  var tasks: ListMap[FrontEnd.TaskId, Double] = ListMap()
  var pending: List[Text] = Nil
  var lastTasks: Int = 0

  val funnel: Funnel[FrontEndEvent] = Funnel()
  val pulsar = Pulsar(0.1*Second)
  
  val pulse: Async[Unit] = Async[Unit]:
    pulsar.stream.each: pulse =>
      funnel.put(Render)
      acquiesce()

  val async: Async[Unit] = Async[Unit]:
    funnel.stream.each:
      case Render                                        => render()
      case LogMessage(message)                           => pending = message :: pending
      case TaskUpdate(taskId, Complete())                => tasks = tasks - taskId
      case TaskUpdate(taskId, Progress(stage, progress)) =>
        tasks = tasks.updated(taskId, progress)
  
  def stop(): Unit raises CancelError =
    Out.print(csi.dectcem(true))
    funnel.stop()
    pulsar.stop()
    async.await()

  def render(): Unit =
    Out.print(csi.dectcem(false))
    
    pending.reverse.each: line =>
      Out.print(line)
      Out.println(csi.el())
    
    pending = Nil

    tasks.each: (taskId, progress) =>
      val bar = t"━"
      val size = (progress*20).toInt
      Out.print(e"${taskId.name.text.fit(60)} [${colors.LimeGreen}(${bar.s*size}${t" ".s*(19 - size)})]")
      Out.println(csi.el())
    
    Out.print(csi.ed())
    if tasks.size > 0 then Out.print(csi.cuu(tasks.size))
    
  def log(message: Message | Text): Unit = (message: @unchecked) match
    case message: Message => funnel.put(LogMessage(message.show))
    case text: Text       => funnel.put(LogMessage(text))
  
  def follow(name: Message)(stream: LazyList[TaskEvent]): Unit =
    val taskId = FrontEnd.TaskId(name)
    
    Async:
      stream.map:
        case Progress(stage, progress) => TaskUpdate(taskId, Progress(stage, progress))
        case Complete()                => TaskUpdate(taskId, Complete())
      .each(funnel.put(_))
      funnel.put(TaskUpdate(taskId, Complete()))
    
def log(message: Text | Message)(using frontEnd: FrontEnd): Unit = frontEnd.log(message)

def follow(name: Message)(stream: LazyList[TaskEvent])(using frontEnd: FrontEnd): Unit =
  frontEnd.follow(name)(stream)

