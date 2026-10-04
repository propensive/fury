                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                 ╭───╮╭───╮                                                       ┃
┃                                 │   ││   │                                                       ┃
┃                                 │   │╰───╯                                                       ┃
┃                                 │   │╭───╮╭───╮╌────╮╭─────────╮                                 ┃
┃                                 │   ││   ││   ╭──╮  ││   ╭─╮   │                                 ┃
┃                                 │   ││   ││   │  ╰──╯│   │ │   │                                 ┃
┃                                 │   ││   ││   │      │   │ │   │                                 ┃
┃                                 │   ││   ││   │      │   ╰─╯   │                                 ┃
┃                                 ╰───╯╰───╯╰───╯      ╰─────╌╰──╯                                 ┃
┃                                                                                                  ┃
┃    LIRA, version 0.1.0.                                                                          ┃
┃    © Copyright 2026 Jon Pretty, Propensive OÜ.                                                   ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://lira.nexus/                                                                       ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package fury

import soundness.*

import denominative.dysasymptotics.linearSize

// What this instance of Fury has been doing, as `fury log` shows it: every notable action the
// daemon takes is recorded here as it happens. For now those are the swarm's — a listener
// starting, a peer connecting, each message sent and received — and the events of a build join
// them when there are builds to run (fury.md §5).
//
// The journal is the daemon's, held in memory for as long as it lives, and bounded: the daemon
// is long-lived, and an unbounded history would grow without limit. It is lost when the daemon
// stops.
object Journal:
  enum Event:
    case Listening(port: Int, fingerprint: Text)
    case ListenFailed(port: Int, reason: Text)
    case Stopped(port: Int)
    case Accepted(peer: Text, version: Text)
    case Closed(peer: Text)
    case Connecting(machine: Text, host: Text, port: Int)
    case Welcomed(machine: Text, hostname: Text, version: Text)
    case Sent(message: Text, peer: Text)
    case Received(message: Text, peer: Text)
    case Failed(machine: Text, reason: Text)

  // An event, numbered in the order it was recorded and stamped with the milliseconds since the
  // Unix epoch at which it happened.
  case class Entry(sequence: Long, moment: Long, event: Event)

  // The number of entries the daemon's journal keeps; older ones fall off the end.
  val limit: Int = 1000

  // The daemon's own journal, which the commands record to and `fury log` reads.
  private val daemon: Journal = Journal(limit)

  def clock(): Long = java.lang.System.currentTimeMillis

  def record(event: Event): Unit = daemon.record(event)
  def since(sequence: Long): List[Entry] = daemon.since(sequence)
  def latest: Long = daemon.latest

  private def digits(value: Long, width: Int): Text =
    val text: Text = value.toString.tt
    t"${t"0"*(width - text.length)}$text"

  // The time of day, in UTC, to the millisecond: `09:41:07.215`.
  def time(moment: Long): Text =
    val millis: Long = moment%86400000L
    val hours: Long = millis/3600000L
    val minutes: Long = millis/60000L%60L
    val seconds: Long = millis/1000L%60L

    t"${digits(hours, 2)}:${digits(minutes, 2)}:${digits(seconds, 2)}.${digits(millis%1000L, 3)}"

  // One line per event. An arrow marks a message crossing the wire: `→` leaving this instance,
  // `←` arriving at it.
  def describe(event: Event): Text = event match
    case Event.Listening(port, fingerprint) =>
      t"opening port $port to other instances, as $fingerprint"

    case Event.ListenFailed(port, reason) =>
      t"could not listen on port $port: $reason"

    case Event.Stopped(port) =>
      t"stopped listening on port $port"

    case Event.Accepted(peer, version) =>
      t"accepted a connection from $peer (fury $version)"

    case Event.Closed(peer) =>
      t"$peer closed the connection"

    case Event.Connecting(machine, host, port) =>
      t"connecting to $machine at $host:$port"

    case Event.Welcomed(machine, hostname, version) =>
      t"$machine welcomed us as $hostname (fury $version)"

    case Event.Sent(message, peer) =>
      t"→ $message to $peer"

    case Event.Received(message, peer) =>
      t"← $message from $peer"

    case Event.Failed(machine, reason) =>
      t"$machine: $reason"

  def render(entry: Entry): Text = t"${time(entry.moment)}Z  ${describe(entry.event)}"

// A journal holding at most `limit` entries, the newest. Entries are numbered from one in the
// order they are recorded, and the numbering is never reused, so a reader that remembers the
// last number it saw can ask for what has happened since.
class Journal(limit: Int):
  import Journal.{Entry, Event}

  private val mutex: Mutex = Mutex()

  @scala.caps.unsafe.untrackedCaptures
  private var next: Long = 0L

  // Newest first.
  @scala.caps.unsafe.untrackedCaptures
  private var entries: List[Entry] = Nil

  def record(event: Event): Unit = mutex:
    next += 1L
    entries = (Entry(next, Journal.clock(), event) :: entries).keep(limit)

  // The entries recorded after the one numbered `sequence`, oldest first; `since(0L)` is
  // everything still held.
  def since(sequence: Long): List[Entry] = mutex(entries.filter(_.sequence > sequence).reverse)

  // The number of the newest entry, or zero if nothing has been recorded.
  def latest: Long = mutex(next)
