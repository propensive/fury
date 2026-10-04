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

  private def settings(keyword: Text): Optional[Text] =
    if keyword == t"listenToken" then secret else Unset

  // The listener binds a moment after `serve` is called, so the first attempts may be refused.
  private def patiently(attempts: Int)(action: => Optional[Swarm.Reply])(using Monitor)
  :   Optional[Swarm.Reply] =

    action.or:
      if attempts <= 1 then Unset else
        snooze(0.2*Second)
        patiently(attempts - 1)(action)

  // Why a ping failed, if it did.
  private def failure(machine: Machine): Optional[Swarm.Error.Reason] logs Event =
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

      // What the protocol was while an instant was a `Long` and a hostname a `Text`: typing
      // those fields must change neither the schema's fingerprint nor a byte of a message.
      test(m"the protocol is the one it was before its fields were typed"):
        Wire.codec.protocol

      . assert(_ == t"9f6e4ca0393d67bc4fd949cd15273f2de738e6b21a985fea638b9c045bcba647")

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
        (unbound.level, refused.level, Event.Stopped(port).level, Event.Closed(laptop).level)

      . assert(_ == (Level.Fail, Level.Warn, Level.Info, Level.Fine))

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
          given (LogSink[Any, Message]^{}) = Journal.sink
          val fingerprint: Data = Peer.identity.fingerprint
          val start: Long = Journal.daemon.latest
          def logged: scala.List[Text] = kinds(Journal.daemon.since(start))
          async(Swarm.service.serve(port.number, settings))

          val self: Machine = machine(t"self", fingerprint, secret)
          val answer = patiently(25)(safely(Swarm.ping(self, t"hello")))

          // The listener's `closed` is logged once the caller has hung up, a moment later.
          await(25)(logged.contains(t"closed"))
          val exchange = logged.drop(logged.lastIndexOf(t"connecting"))
          val mark: Long = Journal.daemon.latest

          val badToken = failure(machine(t"self", fingerprint, t"not-the-token"))
          val wrong: Data = Peer.parseFingerprint(t"00"*32).or(fingerprint)
          val badFingerprint = failure(machine(t"self", wrong, secret))
          val warnings: Int = Journal.daemon.since(mark, Level.Warn).stdlib.length

          Swarm.service.stop()
          await(25)(!Swarm.listening)

          Observed
            ( answer.let(_.hostname), exchange, badToken, badFingerprint, warnings,
              Swarm.listening, logged.last )

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
