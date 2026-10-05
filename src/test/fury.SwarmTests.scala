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

import Journal.{Event, Obstacle, Party}
import alphabets.hexLowerCase
import environments.javaBaseEnvironment
import errorDiagnostics.emptyDiagnostics
import probates.cancelProbate
import proscenium.List
import pyrocosm.{Machine, Peer}
import strategies.throwUnsafely
import threading.platformThreading

// Tests for one Fury talking to another: that the wire messages survive their codec and are
// still, byte for byte, what they were before their fields were typed; that the journal keeps
// what it is sent and no more than its bound; that each event is logged at its own level and
// tells its own message; and — over a real TLS connection to a listener in this JVM — that a
// `ping` is answered with a `pong`, that both ends log it in order, and that a caller with the
// wrong token or the wrong fingerprint is turned away.
object SwarmTests extends Suite(m"Fury swarm tests"):
  private val secret: Text = t"a-token-for-the-swarm-tests"

  // A port nothing is listening on.
  private val port: Tcp.Port = Port[Tcp]()

  // A ping and a pong as they were encoded while an instant was a `Long` and a hostname a
  // `Text`, in hexadecimal.
  private val pingBytes: Text =
    t"01000300083366326139316330010d31373931313038303030303030020b68656c6c6f207468657265"

  private val pongBytes: Text =
    List
      ( t"01010300083366326139316330010d3137393131303830303030343202156c696e75782d626f782e",
        t"6578616d706c652e6f7267" )

    . join

  // The fingerprint of the messages' schema.
  private val protocol: Text =
    t"7e5e105996c7999c65a754aa57896f43d21b802c09239eed92f91198e3b24fff"

  private val moment: Instant over Unix = Instant.of[Unix](1791108000000L)
  private val laptop: Party = Party.Caller(host"laptop")

  private def machine(name: Text, fingerprint: Data, token: Text): Machine =
    Machine(name, t"127.0.0.1", port.number, fingerprint, token, Nil)

  // What the one conversation with the listener showed.
  private case class Observed
    ( answer:         Optional[Hostname],
      exchange:       scala.List[Text],
      badToken:       Optional[Swarm.Error.Reason],
      badFingerprint: Optional[Swarm.Error.Reason],
      warnings:       Int,
      listening:      Boolean,
      last:           Text )

  // What the lasting connections showed.
  private case class Lasting
    ( connected:      Boolean,
      beats:          (Int, Int),
      reply:          Optional[Hostname],
      reconnections:  Int,
      lostByListener: Int,
      still:          Boolean,
      lostByCaller:   Int,
      retried:        Int,
      attempts:       Int,
      released:       Boolean )

  private def settings(keyword: Text): Optional[Text] =
    if keyword == t"listenToken" then secret else Unset

  // The listener binds a moment after `serve` is called, so the first pings may be refused.
  private def patiently(attempts: Int, machine: Machine, note: Text)(using Monitor)
  :   Optional[Swarm.Reply] =

    safely(Swarm.ping(machine, note)).or:
      if attempts <= 1 then Unset else
        snooze(0.2*Second)
        patiently(attempts - 1, machine, note)

  // Why a ping failed, if it did.
  private def failure(machine: Machine)(using Monitor): Optional[Swarm.Error.Reason] =
    attempt[Swarm.Error](Swarm.ping(machine, t"")) match
      case Attempt.Failure(failed) => failed.reason
      case Attempt.Success(_)      => Unset

  // Waits, for a few seconds at most, for `condition` to hold.
  private def await(attempts: Int)(condition: => Boolean)(using Monitor): Unit =
    if attempts > 0 && !condition then
      snooze(0.2*Second)
      await(attempts - 1)(condition)

  // The message an event tells.
  private def told(event: Event): Text = event.communicate.text

  // What kind of event each entry's message tells of.
  private def kinds(entries: List[Journal.Entry]): scala.List[Text] =
    entries.stdlib.map(_.message.text).map: text =>
      if text.starts(t"opening port") then t"listening"
      else if text.starts(t"could not listen") then t"listen-failed"
      else if text.starts(t"stopped listening") then t"stopped"
      else if text.starts(t"accepted a connection") then t"accepted"
      else if text.ends(t"closed the connection") then t"closed"
      else if text.starts(t"connecting to") then t"connecting"
      else if text.contains(t"welcomed us") then t"welcomed"
      else if text.starts(t"→ ping") then t"sent-ping"
      else if text.starts(t"← ping") then t"received-ping"
      else if text.starts(t"→ pong") then t"sent-pong"
      else if text.starts(t"← pong") then t"received-pong"
      else if text.starts(t"→ beat") then t"sent-beat"
      else if text.starts(t"← beat") then t"received-beat"
      else if text.starts(t"keeping the connection") then t"linked"
      else if text.starts(t"lost the connection") then t"lost"
      else if text.starts(t"no longer keeping") then t"unlinked"
      else if text.starts(t"trying") then t"retrying"
      else t"failed"

  def run(): Unit =
    suite(m"The wire"):
      test(m"a ping survives its codec"):
        val ping: Wire = Wire.Ping(t"3f2a91c0", moment, t"hello there")
        Wire.codec.decode(Wire.codec.encode(ping))

      . assert(_ == Wire.Ping(t"3f2a91c0", moment, t"hello there"))

      test(m"a pong survives its codec"):
        val pong: Wire = Wire.Pong(t"3f2a91c0", moment, host"linux-box.example.org")
        Wire.codec.decode(Wire.codec.encode(pong))

      . assert(_ == Wire.Pong(t"3f2a91c0", moment, host"linux-box.example.org"))

      test(m"a beat survives its codec"):
        Wire.codec.decode(Wire.codec.encode(Wire.Beat(moment)))

      . assert(_ == Wire.Beat(moment))

      // The protocol is named by a fingerprint of the messages' schema, so that two builds which
      // disagree about them refuse each other. Adding `beat` changed it, as any change to the
      // messages must; it is pinned here so that no change to them goes unnoticed.
      test(m"the protocol is the one this build was written for"):
        Wire.codec.protocol

      . assert(_ == protocol)

      test(m"a ping is the bytes it was before its fields were typed"):
        Wire.codec.encode(Wire.Ping(t"3f2a91c0", moment, t"hello there")).serialize[Hex]

      . assert(_ == pingBytes)

      test(m"a pong is the bytes it was before its fields were typed"):
        val received: Instant over Unix = Instant.of[Unix](1791108000042L)
        val pong: Wire = Wire.Pong(t"3f2a91c0", received, host"linux-box.example.org")
        Wire.codec.encode(pong).serialize[Hex]

      . assert(_ == pongBytes)

    suite(m"The journal"):
      test(m"entries are returned oldest first, after the one asked for"):
        val journal: Journal = Journal(10)
        journal.record(Level.Info, moment, m"one")
        journal.record(Level.Info, moment, m"two")
        journal.record(Level.Info, moment, m"three")
        (journal.latest, journal.since(1L).stdlib.map(_.message.text))

      . assert(_ == (3L, scala.List(t"two", t"three")))

      test(m"only the newest entries are kept, and their numbers are not reused"):
        val journal: Journal = Journal(5)
        (1 to 12).foreach: index => journal.record(Level.Info, moment, m"entry $index")
        val held = journal.since(0L).stdlib
        (held.map(_.sequence), held.head.message.text)

      . assert(_ == (scala.List(8L, 9L, 10L, 11L, 12L), t"entry 8"))

      test(m"entries below a level are left out"):
        val journal: Journal = Journal(10)
        journal.record(Level.Fine, moment, m"detail")
        journal.record(Level.Warn, moment, m"trouble")
        journal.record(Level.Info, moment, m"news")
        journal.since(0L, Level.Info).stdlib.map(_.message.text)

      . assert(_ == scala.List(t"trouble", t"news"))

      test(m"an instant is shown as the time of day, to the millisecond"):
        Journal.time(Instant.of[Unix](1791108067215L), tz"UTC")

      . assert(_ == t"10:01:07.215")

    suite(m"Events"):
      test(m"a message is told with its direction"):
        told(Event.Received(Wire.Ping(t"3f2a", moment, t"hello"), laptop))

      . assert(_ == t"← ping 3f2a ‘hello’ from laptop")

      test(m"a caller which gave no hostname is still told of"):
        told(Event.Accepted(Party.Caller(Unset), Unset))

      . assert(_ == t"accepted a connection from an unnamed caller (an unknown version of fury)")

      test(m"a version is told as it was declared"):
        val version: Semver = t"0.1.0-9f3f552b898c".as[Semver]
        told(Event.Accepted(laptop, version))

      . assert(_ == t"accepted a connection from laptop (fury 0.1.0-9f3f552b898c)")

      test(m"a listener which cannot start is a failure, and a refused call a warning"):
        val unbound: Event = Event.ListenFailed(port, Obstacle.Unbound)
        val self: Machine = machine(t"self", Data(), secret)
        val reason = Swarm.Error.Reason.Connection(Peer.Error.Reason.Refused(t"bad-token"))
        val refused: Event = Event.Failed(self, reason)
        (unbound.level, refused.level, Event.Stopped(port).level)

      . assert(_ == (Level.Fail, Level.Warn, Level.Info))

      test(m"a beat is fine detail, and a lost connection a warning"):
        val beat: Event = Event.Sent(Wire.Beat(moment), laptop)
        val ping: Event = Event.Sent(Wire.Ping(t"3f2a", moment, t""), laptop)
        (beat.level, ping.level, Event.Lost(laptop, 3.0*Second).level)

      . assert(_ == (Level.Fine, Level.Info, Level.Warn))

      test(m"a lost connection is told with how long it was silent"):
        told(Event.Lost(laptop, 3.25*Second))

      . assert(_ == t"lost the connection with laptop: nothing heard for 3250ms")

      test(m"the pause before a connection is made again doubles, to half a minute at most"):
        scala.List(0, 1, 2, 3, 4, 5, 9).map(Swarm.delay(_).value.toInt)

      . assert(_ == scala.List(1, 2, 4, 8, 16, 30, 30))

      test(m"events belong to the categories that describe them"):
        val sent: Event = Event.Sent(Wire.Ping(t"3f2a", moment, t""), laptop)
        val stopped: Event = Event.Stopped(port)

        ( Log.Protocol.reference.isInstance(sent), Log.Network.reference.isInstance(sent),
          Log.Network.reference.isInstance(stopped) )

      . assert(_ == (true, false, true))

      test(m"a logged event reaches the daemon's journal at its own level"):
        given (LogSink[Any, Message]^{}) = Journal.sink
        val start: Long = Journal.daemon.latest
        Journal.log(Event.ListenFailed(Port.unsafe[Tcp](1), Obstacle.NoToken))

        val failures = Journal.daemon.since(start, Level.Fail).stdlib
        failures.map(_.message.text).filter(_.contains(t"port 1:"))

      . check(_ == scala.List(t"could not listen on port 1: this machine has no token"))

    suite(m"A listener and a caller"):
      // Everything that touches the network happens here, once and in order, before any test is
      // defined: a test's body may be run later, on another thread, so the tests below only
      // judge what was observed.
      val observed: Observed =
        supervise:
          val fingerprint: Data = Peer.identity.fingerprint
          val start: Long = Journal.daemon.latest
          def logged: scala.List[Text] = kinds(Journal.daemon.since(start))
          async(Swarm.service.serve(port.number, settings))

          val self: Machine = machine(t"self", fingerprint, secret)
          val answer = patiently(25, self, t"hello")

          // The listener's `closed` is logged once the caller has hung up, a moment later.
          await(25)(logged.contains(t"closed"))
          val exchange = logged.drop(logged.lastIndexOf(t"connecting"))
          val mark: Long = Journal.daemon.latest

          val badToken = failure(machine(t"self", fingerprint, t"not-the-token"))
          val wrong: Data = Peer.parseFingerprint(t"00"*32).or(fingerprint)
          val badFingerprint = failure(machine(t"self", wrong, secret))
          val warnings: Int = Journal.daemon.since(mark, Level.Warn).stdlib.length

          Swarm.service.stop()
          await(25)(Swarm.listening.absent)

          Observed
            ( answer.let(_.hostname), exchange, badToken, badFingerprint, warnings,
              Swarm.listening.present, logged.last )

      test(m"a ping is answered with a pong"):
        observed.answer

      . assert(_ == Swarm.local)

      // The two ends run on different threads, so what each logs interleaves freely with the
      // other's, except where a message crossing the wire orders them.
      test(m"the caller logs connecting, the welcome, its ping and the pong, in order"):
        val caller = scala.List(t"connecting", t"welcomed", t"sent-ping", t"received-pong")
        observed.exchange.filter(caller.contains(_))

      . assert(_ == scala.List(t"connecting", t"welcomed", t"sent-ping", t"received-pong"))

      test(m"the listener logs the connection, the ping and its pong, in order"):
        val listener = scala.List(t"accepted", t"received-ping", t"sent-pong", t"closed")
        observed.exchange.filter(listener.contains(_))

      . assert(_ == scala.List(t"accepted", t"received-ping", t"sent-pong", t"closed"))

      test(m"the ping arrives after it leaves, and the pong leaves before it arrives"):
        scala.List(t"sent-ping", t"received-ping", t"sent-pong", t"received-pong")
          .map(observed.exchange.indexOf(_))

      . assert: indices => indices == indices.sorted && indices.head >= 0

      test(m"a caller with the wrong token is refused, and told so"):
        observed.badToken

      . assert(_ == Swarm.Error.Reason.Connection(Peer.Error.Reason.Refused(t"bad-token")))

      test(m"a caller pinning the wrong fingerprint does not connect"):
        observed.badFingerprint

      . assert(_ == Swarm.Error.Reason.Connection(Peer.Error.Reason.Unreachable(t"self")))

      test(m"each failure is logged as a warning"):
        observed.warnings

      . assert(_ == 2)

      test(m"stopping the listener is logged"):
        (observed.listening, observed.last)

      . assert(_ == (false, t"stopped"))

    suite(m"A lasting connection"):
      // As above: everything is done here, once and in order, and judged afterwards. Three
      // connections are made. The first is a real one, from this daemon to its own listener,
      // kept for a few seconds. The second is from a caller which sends one beat and then says
      // nothing, which the listener must give up for lost. The third is to a listener which
      // never sends a beat, which this daemon must give up for lost and then make again.
      val lasting: Lasting =
        supervise:
          val identity: Peer.Identity = Peer.identity
          val fingerprint: Data = identity.fingerprint
          val start: Long = Journal.daemon.latest
          def all: List[Journal.Entry] = Journal.daemon.since(start)
          def logged: scala.List[Text] = kinds(all)
          def count(kind: Text): Int = logged.count(_ == kind)

          def about(name: Text, kind: Text): Int =
            all.stdlib.count: entry =>
              entry.message.text.contains(name) && kinds(List(entry)).head == kind

          val here: Tcp.Port = Port[Tcp]()
          val there: Tcp.Port = Port[Tcp]()

          val steady: Machine =
            Machine(t"steady", t"127.0.0.1", here.number, fingerprint, secret, Nil)

          val silent: Machine =
            Machine(t"silent", t"127.0.0.1", there.number, fingerprint, secret, Nil)

          // A real connection, kept for long enough to see beats pass each way.
          async(Swarm.service.serve(here.number, settings))
          await(25)(Swarm.listening.present)
          Swarm.connect(steady)
          await(50)(Swarm.connected(t"steady"))
          val connected: Boolean = Swarm.connected(t"steady")
          snooze(2.5*Second)
          val beats: (Int, Int) = (count(t"sent-beat"), count(t"received-beat"))
          val before: Int = about(t"steady", t"connecting")
          val answer: Optional[Swarm.Reply] = safely(Swarm.ping(steady, t"over the link"))
          val reply: Optional[Hostname] = answer.let(_.hostname)
          val after: Int = about(t"steady", t"connecting")

          // A caller which sends one beat and then says nothing, holding the connection open.
          async:
            safely:
              Peer.connect[Wire, Unit](steady, t"fury", t"0.0.0", Wire.codec, here.number):
                session =>
                  session.send(Wire.Beat(now()))
                  snooze(5.0*Second)

          await(30)(count(t"lost") >= 1)
          val lostByListener: Int = count(t"lost")
          val still: Boolean = Swarm.connected(t"steady")

          // A listener which welcomes a caller and then never sends a beat.
          val gate: () => Optional[Text] = () => Unset

          val deaf: Peer.Listener[Wire] =
            Peer.Listener[Wire](t"fury", t"0.0.0", Wire.codec, secret, identity, Nil, gate):
              session => snooze(8.0*Second)

          async(deaf.serve(there.number))
          snooze(0.5*Second)
          Swarm.connect(silent)
          await(40)(about(t"silent", t"lost") >= 1)
          await(25)(about(t"silent", t"connecting") >= 2)
          val lostByCaller: Int = about(t"silent", t"lost")
          val retried: Int = about(t"silent", t"retrying")
          val attempts: Int = about(t"silent", t"connecting")

          // Tidying up: both connections are let go of, and both listeners stopped.
          Swarm.disconnect(t"silent")
          deaf.stop()
          Swarm.disconnect(t"steady")
          await(25)(!Swarm.connected(t"steady"))
          val released: Boolean = !Swarm.connected(t"steady") && Swarm.connections.nil
          Swarm.service.stop()
          await(25)(Swarm.listening.absent)

          Lasting
            ( connected, beats, reply, after - before, lostByListener, still, lostByCaller,
              retried, attempts, released )

      test(m"a connection asked for is made and kept"):
        lasting.connected

      . assert(_ == true)

      test(m"beats pass each way, about one a second from each end"):
        lasting.beats

      . assert: (sent, received) => sent >= 4 && received >= 4

      test(m"a ping goes over the connection that is already open"):
        (lasting.reply, lasting.reconnections)

      . assert(_ == (Swarm.local, 0))

      test(m"a listener gives up a caller which goes silent"):
        lasting.lostByListener

      . assert(_ >= 1)

      test(m"another caller's silence does not disturb a healthy connection"):
        lasting.still

      . assert(_ == true)

      test(m"a caller gives up a listener which goes silent, and connects again"):
        (lasting.lostByCaller >= 1, lasting.retried >= 1, lasting.attempts >= 2)

      . assert(_ == (true, true, true))

      test(m"a connection let go of is closed, and no longer kept"):
        lasting.released

      . assert(_ == true)
