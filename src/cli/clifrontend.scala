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
import turbulence.*
import exoskeleton.*
import acyclicity.*
import contingency.*
import parasite.*
import dendrology.*, dagStyles.default
import vacuous.*
import hieroglyph.*, textMetrics.uniform
import escapade.*
import anticipation.*
import iridescence.*
import gossamer.*
import rudiments.*
import spectacular.*
import quantitative.*
import eucalyptus.*

import scala.collection.concurrent as scc

import java.util.concurrent as juc


def frontEnd[ResultType](lambda: CliFrontEnd ?=> Terminal ?=> ResultType)(using Cli, Log[Display], Monitor)
        : ResultType =
  terminal:
    val frontEnd = CliFrontEnd()
    FrontEnd.register(frontEnd)
    var continue: Boolean = true
    val loop = async(while continue.also(frontEnd.render()) do sleep(50*Milli(Second)))
    try
      lambda(using frontEnd).also:
        continue = false
    finally
      safely(loop.await())
      safely(frontEnd.render(last = true))
      FrontEnd.unregister(frontEnd)

class CliFrontEnd()(using terminal: Terminal) extends FrontEnd:
  given stdio: Stdio = terminal.stdio
  private var dag: Optional[Dag[Target]] = Unset
  private var diagram: Optional[DagDiagram[Target]] = Unset
  private val misc: scc.TrieMap[Target, Double] = scc.TrieMap()
  private var indents: Map[Target, Text] = Map()
  private val queue: juc.ConcurrentLinkedQueue[Text] = juc.ConcurrentLinkedQueue()

  def reset(): Unit =
    dag = Unset
    indents = Map()
    active.clear()
    misc.clear()
    Out.println(t"\e[1J")

  def info[InfoType: Printable](info: InfoType) =
    queue.add(summon[Printable[InfoType]].print(info, terminal.stdio.termcap))
  
  def setSchedule(dag2: Dag[Target]): Unit =
    dag = dag2
    diagram = DagDiagram(dag2).tap: diagram =>
      indents =
        diagram.nodes.reverse.zipWithIndex.map: (target, index) =>
          val indent = terminal.knownColumns - diagram.size*2 + index*2 - target.show.length - 14
          (target, t" "*indent)
        .to(Map)
    
  val edge: Display = e"${colors.Gray}(│)"

  def showItem(target: Target, last: Boolean): Display =
    val prefix = indents.at(target).or(t" "*(terminal.knownColumns - target.show.length - 16))

    active.at(target) match
      case 1.0 =>
        e"▪ $Bold(${colors.Gray}($target))$prefix$edge${Bg(rgb"#009966")}(     ${colors.Black}($Bold(OK))     )$edge"
      
      case -1.0 =>
        e"▪ $Bold(${colors.Gray}($target))$prefix$edge${Bg(rgb"#003333")}(            )$edge"

      case Unset =>
        e"▪ ${colors.Gray}(${target.show})"
        e"▪ $Bold(${colors.Gray}(${target}))$prefix$edge            $edge"
      
      case progress: Double =>
        val highlight = if last then rgb"#990033" else colors.Gold
        e"▪ $Bold($highlight(${target}))$prefix$edge$highlight(${ProgressBar(progress)})$edge"

  def render(last: Boolean = false): Unit =
    for i <- 0 until queue.size do queue.poll().nn.cut(t"\n").each: line =>
      Out.println(line+t"\e[K")
    
    unscheduled.each { target => Out.println(showItem(target, last)) }

    diagram.let: diagram =>
      Out.print(t"\e[?25l")
      diagram.render { target => showItem(target, last) }.each(Out.println(_))
      Out.println(e"\e[K")
      Out.println(if last then t"\e[?25h" else t"\e[${diagram.size + unscheduled.size + 2}A")


object ProgressBar:
  def apply(double: Double): Text = bars((double*96).toInt.min(96).max(0))
  val partial: Text = t" ▎▍▌▋▊▉█"
  
  val bars: IArray[Text] = IArray.from:
    (0 to 96).map: progress =>
      unsafely(((t"█"*(progress/8))+partial.at(progress%8).vouch.show).fit(12))

