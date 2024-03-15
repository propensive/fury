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
import dendrology.*

import scala.collection.concurrent as scc

enum Task:
  case Download(digest: Hash)
  case Compile(digest: Hash)
  case Clone(digest: Hash)

def info[InfoType: Printable](info: InfoType)(using frontEnd: FrontEnd): Unit = frontEnd.info(info)

trait FrontEnd:

  protected val active: scc.TrieMap[Target, Double] = scc.TrieMap()
  private val aborted: Promise[Unit] = Promise()
  def continue: Boolean = !aborted.ready
  def abort(): Unit = aborted.offer(())
  def schedule(task: Task): Unit
  def update(task: Task): Unit
  def graph(diagram: DagDiagram[Target]): Unit
  def info[InfoType: Printable](info: InfoType): Unit
  def attend(): Unit = aborted.attend()

  def update(target: Target, progress: Double) = active(target) = progress

enum Activity:
  case Progress(stage: Text, progress: Double)
  case Complete
