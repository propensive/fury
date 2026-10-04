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

import calendars.gregorianCalendar
import denominative.dysasymptotics.linearSize
import eucalyptus.logFormats.textLevelLogFormat
import pyrocosm.{Machine, Peer}
import timeFormats.iso8601TimeFormat

// What this instance of Fury has been doing, as `fury log` shows it. Every notable action the
// daemon takes is LOGGED — a method that does so says `logs Journal.Event` in its type, and
// emits the typed event at the level its nature deserves — and the journal keeps what a sink
// for it (`Journal.sink`) is sent. For now the events are the swarm's — a listener starting, a
// peer connecting, each message sent and received — and the events of a build join them when
// there are builds to run (fury.md §5).
object Journal:
  object Fingerprint:
    given showable: Fingerprint is Showable = fingerprint => Peer.render(fingerprint.data)

  // The SHA-256 fingerprint of the certificate a machine presents.
  case class Fingerprint(data: Data)

  object Obstacle:
    given communicable: Obstacle is Communicable =
      case Unbound    => m"the port could not be bound"
      case NoToken    => m"this machine has no token"
      case NoIdentity => m"this machine's identity could not be created"

  // Why a listener could not start.
  enum Obstacle:
    case Unbound, NoToken, NoIdentity

  object Party:
    given showable: Party is Showable =
      case Callee(machine)  => machine.name
      case Caller(hostname) => hostname.lay(t"an unnamed caller")(_.show)

  // The other end of a connection: a machine this one called, which its configuration names,
  // or a caller, known only by the hostname it gave — if that was a hostname at all.
  enum Party:
    case Callee(machine: Machine)
    case Caller(hostname: Optional[Hostname])

  object Event:
    private def release(version: Optional[Semver]): Text =
      version.lay(t"an unknown version of fury"): version => t"fury $version"

    // One line per event. An arrow marks a message crossing the wire: `→` leaving this
    // instance, `←` arriving at it.
    given communicable: Event is Communicable =
      case Listening(port, fingerprint) =>
        m"opening port ${port.number} to other instances, as ${fingerprint.show}"

      case ListenFailed(port, obstacle) =>
        m"could not listen on port ${port.number}: $obstacle"

      case Stopped(port) =>
        m"stopped listening on port ${port.number}"

      case Accepted(peer, version) =>
        m"accepted a connection from ${peer.show} (${release(version)})"

      case Closed(peer) =>
        m"${peer.show} closed the connection"

      case Connecting(machine, port) =>
        m"connecting to ${machine.name} at ${machine.host}:${port.number}"

      case Welcomed(peer, hostname, version) =>
        val name: Text = hostname.lay(t"an unnamed machine")(_.show)
        m"${peer.show} welcomed us as $name (${release(version)})"

      case Sent(message, peer) =>
        m"→ ${message.show} to ${peer.show}"

      case Received(message, peer) =>
        m"← ${message.show} from ${peer.show}"

      case Failed(machine, reason) =>
        m"${machine.name}: $reason"

  // Each event belongs to the categories of `Log` that describe its nature, by which a sink may
  // choose what to record.
  enum Event:
    case Listening(port: Tcp.Port, fingerprint: Fingerprint)   extends Event, Log.Network
    case ListenFailed(port: Tcp.Port, obstacle: Obstacle)      extends Event, Log.Network
    case Stopped(port: Tcp.Port)                               extends Event, Log.Network
    case Accepted(peer: Party, version: Optional[Semver])      extends Event, Log.Network, Log.Auth
    case Closed(peer: Party)                                   extends Event, Log.Network
    case Connecting(machine: Machine, port: Tcp.Port)          extends Event, Log.Network

    case Welcomed(peer: Party, host: Optional[Hostname], version: Optional[Semver])
    extends Event, Log.Network, Log.Auth

    case Sent(message: Wire, peer: Party)                      extends Event, Log.Protocol
    case Received(message: Wire, peer: Party)                  extends Event, Log.Protocol
    case Failed(machine: Machine, reason: Peer.Error.Reason)   extends Event, Log.Network

    // How much each event matters: a failure is a warning, or worse if it stops this instance
    // doing what its configuration asked; the routine opening and closing of a connection is
    // fine detail.
    def level: Level = this match
      case _: ListenFailed          => Level.Fail
      case _: Failed                => Level.Warn
      case _: Closed | _: Welcomed  => Level.Fine
      case _                        => Level.Info

  given wireShowable: Wire is Showable =
    case Wire.Ping(id, _, note) => if note == t"" then t"ping $id" else t"ping $id ‘$note’"
    case Wire.Pong(id, _, _)    => t"pong $id"

  // A logged event as the journal keeps it: numbered in the order it arrived, with its level,
  // the instant it was logged and the message it was transcribed to.
  case class Entry(sequence: Long, level: Level, instant: Instant over Unix, message: Message)

  // The number of entries the daemon's journal keeps; older ones fall off the end.
  val limit: Int = 1000

  // The daemon's own journal, which `fury log` reads.
  val daemon: Journal = Journal(limit)

  // A sink which keeps every event logged where it is in scope in the daemon's journal: the
  // event arrives as the `Message` it was transcribed to, with its level and the instant it was
  // logged, which `Log` gives in milliseconds since the Unix epoch. Any event whose message can
  // be told is kept, Fury's own or a library's.
  //
  // A sink is a capability, since submitting to one is an effect on its destination. This one's
  // destination is the daemon's journal, which is there for as long as the daemon is, so it is
  // vouched pure, as Eucalyptus vouches for its own loggers: it may then be a given wherever
  // something is logged, without each use of it having to be separated from the last.
  val sink: LogSink[Any, Message]^{} = scala.caps.unsafe.unsafeAssumePure:
    new LogSink[Any, Message]:
      def accepts(level: Level): Boolean = true

      def submit(level: Level, timestamp: Long, message: Message): Unit =
        daemon.record(level, Instant.of[Unix](timestamp), message)

  // Logs `event` at its own level, to whichever sinks the caller has in scope.
  def log(event: Event): Unit logs Event = event.level match
    case Level.Fine => Log.fine(event)
    case Level.Info => Log.info(event)
    case Level.Warn => Log.warn(event)
    case Level.Fail => Log.fail(event)

  // The time zone times of day are shown in: this machine's, or UTC if that cannot be had.
  lazy val timezone: Timezone =
    safely(Timezone(java.time.ZoneId.systemDefault.nn.getId.nn.tt)).or(tz"UTC")

  // The time of day, to the millisecond: `09:41:07.215`. The clock face is the instant's, in
  // this machine's time zone; a clock face counts whole seconds, so the thousandths are read
  // from the instant directly.
  def time(instant: Instant over Unix, timezone: Timezone = timezone): Text =
    val thousandths: Text = (instant.long%1000L).show
    t"${(instant in timezone).time.show}.${t"0"*(3 - thousandths.length)}$thousandths"

  def render(entry: Entry): Text =
    t"${time(entry.instant)}  ${entry.level.show}  ${entry.message.text}"

// A journal holding at most `limit` entries, the newest. Entries are numbered from one in the
// order they are recorded, and the numbering is never reused, so a reader that remembers the
// last number it saw can ask for what has happened since.
class Journal(limit: Int):
  import Journal.Entry

  private val mutex: Mutex = Mutex()

  @scala.caps.unsafe.untrackedCaptures
  private var next: Long = 0L

  // Newest first.
  @scala.caps.unsafe.untrackedCaptures
  private var entries: List[Entry] = Nil

  def record(level: Level, instant: Instant over Unix, message: Message): Unit = mutex:
    next += 1L
    entries = (Entry(next, level, instant, message) :: entries).keep(limit)

  // The entries recorded after the one numbered `sequence`, at `level` or above, oldest first;
  // `since(0L)` is everything still held.
  def since(sequence: Long, level: Level = Level.Fine): List[Entry] = mutex:
    def wanted(entry: Entry): Boolean =
      entry.sequence > sequence && entry.level.ordinal >= level.ordinal

    entries.filter(wanted(_)).reverse

  // The number of the newest entry, or zero if nothing has been recorded.
  def latest: Long = mutex(next)
