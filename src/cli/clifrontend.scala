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
    var continue: Boolean = true

    val loop = async(while continue do frontEnd.render().also(sleep(50*Milli(Second))))
  
    lambda(using frontEnd).also:
      continue = false
      safely(loop.await())
      frontEnd.render(last = true)


class CliFrontEnd()(using terminal: Terminal) extends FrontEnd:
  given stdio: Stdio = terminal.stdio
  private var diagram: Optional[DagDiagram[Target]] = Unset
  private val misc: scc.TrieMap[Target, Double] = scc.TrieMap()
  private var indents: Map[Target, Text] = Map()
  private val queue: juc.ConcurrentLinkedQueue[Text] = juc.ConcurrentLinkedQueue()

  def schedule(task: Task): Unit = ()
  def update(task: Task): Unit = ()
  
  def reset(): Unit =
    diagram = Unset
    indents = Map()
    active.clear()
    misc.clear()
    Out.println(t"\e[1J")

  def info[InfoType: Printable](info: InfoType) =
    queue.add(summon[Printable[InfoType]].print(info, terminal.stdio.termcap))
  
  def graph(diagram2: DagDiagram[Target]): Unit =
    diagram = diagram2
    
    indents =
      diagram2.nodes.reverse.zipWithIndex.map: (target, index) =>
        val indent = terminal.knownColumns - diagram2.size*2 + index*2 - target.show.length - 14
        (target, t" "*indent)
      .to(Map)
    
  val edge: Display = e"${colors.Gray}(│)"

  def render(last: Boolean = false): Unit = diagram.let: diagram =>
    Out.print(t"\e[?25l")

    for i <- 0 until queue.size do queue.poll().nn.cut(t"\n").each: line =>
      Out.println(line+t"\e[K")

    diagram.render: target =>
      active.get(target) match
        case Some(1.0) =>
          e"▪ $Bold(${colors.Gray}($target))${indents(target)}$edge${Bg(colors.MediumSeaGreen)}(     ${colors.Black}($Bold(OK))     )$edge"

        case None =>
          e"▪ ${colors.Gray}(${target.show})"
          e"▪ $Bold(${colors.Gray}(${target}))${indents(target)}$edge            $edge"
        
        case Some(progress) =>
          val highlight = if last then rgb"#990033" else colors.Gold
          e"▪ $Bold($highlight(${target}))${indents(target)}$edge$highlight(${ProgressBar(progress)})$edge"

    .each(Out.println(_))
    
    Out.println(e"\e[K")

    if last then
      Out.println(t"\e[?25h")
      Out.println(t"----------")
    else Out.println(t"\e[${diagram.size + 2}A")


object ProgressBar:
  def apply(double: Double): Text = bars((double*96).toInt.min(96).max(0))
  val partial: Text = t" ▎▍▌▋▊▉█"
  
  val bars: IArray[Text] = IArray.from:
    (0 to 96).map: progress =>
      unsafely(((t"█"*(progress/8))+partial.at(progress%8).vouch.show).fit(12))

