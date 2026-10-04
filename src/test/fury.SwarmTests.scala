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

import environments.javaBaseEnvironment
import probates.cancelProbate
import proscenium.List
import pyrocosm.{Machine, Peer}
import strategies.throwUnsafely
import threading.platformThreading

// Tests for one Fury talking to another: that the wire messages survive their codec, that the
// journal keeps what it is told and no more than its bound, and — over a real TLS connection to
// a listener in this JVM — that a `ping` is answered with a `pong`, that both ends record it in
// order, and that a caller with the wrong token or the wrong fingerprint is turned away.
object SwarmTests extends Suite(m"Fury swarm tests"):
  private val secret: Text = t"a-token-for-the-swarm-tests"

  // A port unlikely to be in use, different on each run.
  private val port: Int = 20000 + (Journal.clock()%20000L).toInt

  private def machine(name: Text, fingerprint: Data, token: Text): Machine =
    Machine(name, t"127.0.0.1", port, fingerprint, token, Nil)

  // What the one conversation with the listener showed.
  private case class Observed
    ( answer:         scala.Either[Text, Text],
      exchange:       scala.List[Text],
      badToken:       scala.Option[Text],
      badFingerprint: scala.Option[Text],
      failures:       scala.List[Text],
      listening:      Boolean,
      last:           Text )

  private def settings(keyword: Text): Optional[Text] =
    if keyword == t"listenToken" then secret else Unset

  // The listener binds a moment after `serve` is called, so the first attempts may be refused.
  private def patiently(attempts: Int)(action: => scala.Either[Text, Swarm.Reply])(using Monitor)
  :   scala.Either[Text, Swarm.Reply] =

    action match
      case scala.Left(_) if attempts > 1 =>
        snooze(0.2*Second)
        patiently(attempts - 1)(action)

      case result =>
        result

  // Waits, for a few seconds at most, for `condition` to hold.
  private def await(attempts: Int)(condition: => Boolean)(using Monitor): Unit =
    if attempts > 0 && !condition then
      snooze(0.2*Second)
      await(attempts - 1)(condition)

  private def kinds(entries: List[Journal.Entry]): scala.List[Text] =
    entries.stdlib.map(_.event).map:
      case _: Journal.Event.Listening         => t"listening"
      case _: Journal.Event.ListenFailed      => t"listen-failed"
      case _: Journal.Event.Stopped           => t"stopped"
      case _: Journal.Event.Accepted          => t"accepted"
      case _: Journal.Event.Closed            => t"closed"
      case _: Journal.Event.Connecting        => t"connecting"
      case _: Journal.Event.Welcomed          => t"welcomed"
      case Journal.Event.Sent(message, _)     => t"sent-${message.cut(t" ").stdlib.head}"
      case Journal.Event.Received(message, _) => t"received-${message.cut(t" ").stdlib.head}"
      case _: Journal.Event.Failed            => t"failed"

  def run(): Unit =
    suite(m"The wire"):
      test(m"a ping survives its codec"):
        val ping: Wire = Wire.Ping(t"3f2a91c0", 1791108000000L, t"hello there")
        Wire.codec.decode(Wire.codec.encode(ping))

      . assert(_ == Wire.Ping(t"3f2a91c0", 1791108000000L, t"hello there"))

      test(m"a pong survives its codec"):
        val pong: Wire = Wire.Pong(t"3f2a91c0", 1791108000042L, t"linux-box")
        Wire.codec.decode(Wire.codec.encode(pong))

      . assert(_ == Wire.Pong(t"3f2a91c0", 1791108000042L, t"linux-box"))

      test(m"the protocol is named by a 32-byte fingerprint of the schema"):
        Wire.codec.fingerprint.length

      . assert(_ == 32)

    suite(m"The journal"):
      test(m"events are returned oldest first, after the one asked for"):
        val journal: Journal = Journal(10)
        journal.record(Journal.Event.Stopped(1))
        journal.record(Journal.Event.Stopped(2))
        journal.record(Journal.Event.Stopped(3))
        (journal.latest, journal.since(1L).stdlib.map(_.event))

      . assert(_ == (3L, scala.List(Journal.Event.Stopped(2), Journal.Event.Stopped(3))))

      test(m"only the newest events are kept, and their numbers are not reused"):
        val journal: Journal = Journal(5)
        (1 to 12).foreach: port => journal.record(Journal.Event.Stopped(port))
        val held = journal.since(0L).stdlib
        (held.map(_.sequence), held.head.event)

      . assert(_ == (scala.List(8L, 9L, 10L, 11L, 12L), Journal.Event.Stopped(8)))

      test(m"a moment is rendered as the time of day in UTC"):
        Journal.time(1791108067215L)

      . assert(_ == t"10:01:07.215")

      test(m"a message is described with its direction"):
        val ping: Text = Swarm.describe(Wire.Ping(t"3f2a", 0L, t"hello"))
        Journal.describe(Journal.Event.Received(ping, t"laptop"))

      . assert(_ == t"← ping 3f2a ‘hello’ from laptop")

    suite(m"A listener and a caller"):
      // Everything that touches the network happens here, once and in order, before any test is
      // defined: a test's body may be run later, on another thread, so the tests below only
      // judge what was observed.
      val observed: Observed =
        supervise:
          val fingerprint: Data = Peer.identity.fingerprint
          val start: Long = Journal.latest
          async(Swarm.service.serve(port, settings))

          val self: Machine = machine(t"self", fingerprint, secret)
          val answer = patiently(25)(Swarm.ping(self, t"hello"))

          // The listener's `closed` is recorded once the caller has hung up, a moment later.
          await(25)(kinds(Journal.since(start)).contains(t"closed"))
          val all = kinds(Journal.since(start))
          val exchange = all.drop(all.lastIndexOf(t"connecting"))
          val mark: Long = Journal.latest

          val badToken = Swarm.ping(machine(t"self", fingerprint, t"not-the-token"), t"")
          val wrong: Data = Peer.parseFingerprint(t"00"*32).or(fingerprint)
          val badFingerprint = Swarm.ping(machine(t"self", wrong, secret), t"")

          val failures =
            Journal.since(mark).stdlib.map(_.event).collect:
              case Journal.Event.Failed(name, _) => name

          Swarm.service.stop()
          await(25)(!Swarm.listening)

          Observed
            ( answer.map(_.hostname), exchange, badToken.swap.toOption,
              badFingerprint.swap.toOption, failures, Swarm.listening,
              kinds(Journal.since(start)).last )

      test(m"a ping is answered with a pong"):
        observed.answer

      . assert(_ == scala.Right(Machine.Identity.local.hostname))

      // The two ends run on different threads, so what each records interleaves freely with the
      // other's, except where a message crossing the wire orders them.
      test(m"the caller records connecting, the welcome, its ping and the pong, in order"):
        val caller = scala.List(t"connecting", t"welcomed", t"sent-ping", t"received-pong")
        observed.exchange.filter(caller.contains(_))

      . assert(_ == scala.List(t"connecting", t"welcomed", t"sent-ping", t"received-pong"))

      test(m"the listener records the connection, the ping and its pong, in order"):
        val listener = scala.List(t"accepted", t"received-ping", t"sent-pong", t"closed")
        observed.exchange.filter(listener.contains(_))

      . assert(_ == scala.List(t"accepted", t"received-ping", t"sent-pong", t"closed"))

      test(m"the ping arrives before the pong leaves, and the pong leaves before it arrives"):
        scala.List(t"sent-ping", t"received-ping", t"sent-pong", t"received-pong")
          .map(observed.exchange.indexOf(_))

      . assert: indices => indices == indices.sorted && indices.head >= 0

      test(m"a caller with the wrong token is refused, and told so"):
        observed.badToken

      . assert(_ == scala.Some(t"the peer refused the connection: bad-token"))

      test(m"a caller pinning the wrong fingerprint does not connect"):
        observed.badFingerprint

      . assert(_ == scala.Some(t"machine self could not be connected to"))

      test(m"each failure is recorded against the machine"):
        observed.failures

      . assert(_ == scala.List(t"self", t"self"))

      test(m"stopping the listener is recorded"):
        (observed.listening, observed.last)

      . assert(_ == (false, t"stopped"))
