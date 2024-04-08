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

import anticipation.*
import parasite.*
import vacuous.*
import rudiments.*
import acyclicity.*

import scala.collection.concurrent as scc
import scala.collection.mutable as scm

def info[InfoType: Printable](info: InfoType)(using frontEnd: FrontEnd): Unit = frontEnd.info(info)

object FrontEnd:
  private var frontEnds: Set[FrontEnd] = Set()
  private val termination: Optional[Promise[Unit]] = Unset
  
  def register(frontEnd: FrontEnd): Unit = synchronized:
    frontEnds += frontEnd
  
  def unregister(frontEnd: FrontEnd): Unit = synchronized:
    frontEnds -= frontEnd
    
    termination.let: promise =>
      if frontEnds.isEmpty then promise.offer(())
  
  def shutdown(): Unit = synchronized(frontEnds.each(_.abort()))

trait FrontEnd:
  protected val active: scc.TrieMap[Target, Double] = scc.TrieMap()
  protected val unscheduled: scm.LinkedHashSet[Target] = scm.LinkedHashSet()
  private val aborted: Promise[Unit] = Promise()

  def setSchedule(diagram: Dag[Target]): Unit
  def start(target: Target): Unit = unscheduled.synchronized(unscheduled.add(target))
  def stop(target: Target): Unit = unscheduled.synchronized(unscheduled.remove(target))
  def info[InfoType: Printable](info: InfoType): Unit
  def abort(): Unit = aborted.offer(())
  def attend(): Unit = aborted.attend()
  def continue: Boolean = !aborted.ready

  def update(target: Target, progress: Double) = active(target) = progress

enum Activity:
  case Progress(stage: Text, progress: Double)
  case Complete
