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
import pyrocosm.{Invitation, Machine, Peer}
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

  // The signature of the protocol's schema, in hexadecimal: the hash by which its base is
  // known. A change to the messages changes it, as it must; it is pinned here so that no
  // change to them goes unnoticed.
  private val signature: Text =
    t"ddfa9d041cc2a5ffebe0df6bad8bb882b234987af5efb7289cac8cbd5ef44dd1"

  private val id: Uuid = Uuid(0x3f2a91c000000000L, 0L)

  private val unservable: Text =
    List
      ( t"could not agree a protocol with laptop: it accepts no form of the protocol this",
        t"instance can write" )

    . join(t" ")

  private val moment: Instant over Unix = Instant.of[Unix](1791108000000L)
  private val laptop: Party = Party.Caller(host"laptop")

  private def machine(name: Text, fingerprint: Data, token: Text): Machine =
    Machine(name, List(t"127.0.0.1"), port.number, fingerprint, token, Nil)

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
      released:       Boolean,
      negotiated:     Int,
      strangers:      Int,
      unannounced:    Int,
      mismatched:     Optional[Swarm.Error.Reason],
      calleeAdvert:   Optional[Wire.Advert],
      callerAdvert:   Optional[Wire.Advert],
      calleeLoad:     Optional[Double] )

  // What the invitation showed.
  private case class Invited
    ( words:      Int,
      read:       Boolean,
      joined:     Boolean,
      again:      Boolean,
      declared:   Boolean,
      connected:  Boolean,
      admitted:   Boolean,
      revoked:    Int,
      afterwards: Optional[Text] )

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
      else if text.starts(t"→ advert") then t"sent-advert"
      else if text.starts(t"← advert") then t"received-advert"
      else if text.starts(t"→ beat") then t"sent-beat"
      else if text.starts(t"← beat") then t"received-beat"
      else if text.starts(t"keeping the connection") then t"linked"
      else if text.starts(t"lost the connection") then t"lost"
      else if text.starts(t"no longer keeping") then t"unlinked"
      else if text.starts(t"trying") then t"retrying"
      else if text.starts(t"exchanged acceptances") then t"negotiated"
      else if text.starts(t"could not agree") then t"unnegotiated"
      else if text.contains(t"does not accept") then t"unaccepted"
      else if text.contains(t"of no form") then t"unread"
      else t"failed"

  def run(): Unit =
    suite(m"The wire"):
      // A message is written to an acceptance — here this build's own, as it is when two of the
      // same build talk — and read back from the document that makes.
      test(m"a ping survives being written and read"):
        val ping: Wire = Wire.Ping(id, moment, t"hello there")
        Wire.write(ping, Wire.acceptance).let(Wire.read(_))

      . assert(_ == Wire.Ping(id, moment, t"hello there"))

      test(m"a pong survives being written and read"):
        val pong: Wire = Wire.Pong(id, moment, host"linux-box.example.org")
        Wire.write(pong, Wire.acceptance).let(Wire.read(_))

      . assert(_ == Wire.Pong(id, moment, host"linux-box.example.org"))

      test(m"a beat survives being written and read"):
        Wire.write(Wire.Beat(moment, Unset), Wire.acceptance).let(Wire.read(_))

      . assert(_ == Wire.Beat(moment, Unset))

      test(m"a beat carries the sender's load"):
        Wire.write(Wire.Beat(moment, 1.25), Wire.acceptance).let(Wire.read(_))

      . assert(_ == Wire.Beat(moment, 1.25))

      test(m"an advert survives being written and read"):
        val advert: Wire = Wire.Advert(host"linux-box", t"Linux", t"amd64", 16)
        Wire.write(advert, Wire.acceptance).let(Wire.read(_))

      . assert(_ == Wire.Advert(host"linux-box", t"Linux", t"amd64", 16))

      test(m"the schema is the one this build was written for"):
        Wire.signature.serialize[Hex]

      . assert(_ == signature)

      test(m"the schema as it is written is the schema as it is derived"):
        Schemas.source(t"wire.schema.tel").let(Wire.signature(_)).let(_.serialize[Hex])

      . assert(_ == signature)

      test(m"the written schema passes the validity battery"):
        Schemas.load(t"wire.schema.tel").name

      . assert(_ == t"wire")

      test(m"an acceptance names the protocol's one form"):
        Wire.acceptance.alternatives.stdlib.length

      . assert(_ == 1)

      test(m"an acceptance survives being sent"):
        Wire.offered(Wire.offer).let(_ == Wire.acceptance)

      . assert(_ == true)

      test(m"a message is not an acceptance, and is not taken for one"):
        Wire.write(Wire.Beat(moment, Unset), Wire.acceptance).let(Wire.offered(_)).absent

      . assert(_ == true)

      test(m"a message is not written to an acceptance of another protocol"):
        Wire.offered(Stranger.offer).let(Wire.write(Wire.Beat(moment, Unset), _)).absent

      . assert(_ == true)

      test(m"an acceptance of another protocol is still an acceptance"):
        Wire.offered(Stranger.offer).present

      . assert(_ == true)

      test(m"a document of another protocol is not read"):
        Wire.read(Stranger.document).absent

      . assert(_ == true)

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
        told(Event.Received(Wire.Ping(id, moment, t"hello"), laptop))

      . assert(_ == t"← ping 3f2a91c0 ‘hello’ from laptop")

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
        val beat: Event = Event.Sent(Wire.Beat(moment, Unset), laptop)
        val ping: Event = Event.Sent(Wire.Ping(id, moment, t""), laptop)
        (beat.level, ping.level, Event.Lost(laptop, 3.0*Second).level)

      . assert(_ == (Level.Fine, Level.Info, Level.Warn))

      test(m"a message the other end does not accept is told, as a warning"):
        val event: Event = Event.Unaccepted(Wire.Beat(moment, Unset), laptop)
        (event.level, told(event))

      . assert(_ == (Level.Warn, t"laptop does not accept beat, which was not sent"))

      test(m"a failure to agree a protocol is told with why"):
        told(Event.Unnegotiated(laptop, Journal.Mismatch.Unservable))

      . assert(_ == unservable)

      test(m"an advert is told with what it says"):
        told(Event.Received(Wire.Advert(host"linux-box", t"Linux", t"amd64", 16), laptop))

      . assert(_ == t"← advert (Linux amd64, 16 cores) from laptop")

      test(m"this machine advertises what it is"):
        val advert: Wire.Advert = Swarm.advert
        (advert.hostname, advert.cores >= 1, advert.os != t"", advert.arch != t"")

      . assert(_ == (Swarm.local, true, true, true))

      test(m"a lost connection is told with how long it was silent"):
        told(Event.Lost(laptop, 3.25*Second))

      . assert(_ == t"lost the connection with laptop: nothing heard for 3250ms")

      test(m"the pause before a connection is made again doubles, to half a minute at most"):
        scala.List(0, 1, 2, 3, 4, 5, 9).map(Swarm.delay(_).value.toInt)

      . assert(_ == scala.List(1, 2, 4, 8, 16, 30, 30))

      test(m"events belong to the categories that describe them"):
        val sent: Event = Event.Sent(Wire.Ping(id, moment, t""), laptop)
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
            Machine(t"steady", List(t"127.0.0.1"), here.number, fingerprint, secret, Nil)

          val silent: Machine =
            Machine(t"silent", List(t"127.0.0.1"), there.number, fingerprint, secret, Nil)

          // A real connection, kept for long enough to see beats pass each way.
          async(Swarm.service.serve(here.number, settings))
          await(25)(Swarm.listening.present)
          Swarm.connect(steady)
          await(50)(Swarm.connected(t"steady"))
          val connected: Boolean = Swarm.connected(t"steady")
          snooze(2.5*Second)
          val beats: (Int, Int) = (count(t"sent-beat"), count(t"received-beat"))

          // What each end has learnt of the other: the callee from the caller's side, and the
          // caller from the listener's.
          val kept: scala.List[Swarm.Connection] = Swarm.connections.stdlib

          val callee: Optional[Swarm.Standing] =
            kept.find(_.machine.name == t"steady").map(_.standing).getOrElse(Unset)

          val caller: Optional[Swarm.Standing] =
            Swarm.visitors.stdlib.headOption.map(_.standing).getOrElse(Unset)

          val before: Int = about(t"steady", t"connecting")
          val answer: Optional[Swarm.Reply] = safely(Swarm.ping(steady, t"over the link"))
          val reply: Optional[Hostname] = answer.let(_.hostname)
          val after: Int = about(t"steady", t"connecting")

          // A caller which sends one beat and then says nothing, holding the connection open.
          async:
            safely:
              Peer.connect[Data, Unit](steady, t"fury", t"0.0.0", Wire.codec, here.number):
                session =>
                  session.send(Wire.offer)
                  session.receive()
                  Wire.write(Wire.Beat(now(), Unset), Wire.acceptance).let(session.send(_))
                  snooze(5.0*Second)

          await(30)(count(t"lost") >= 1)
          val lostByListener: Int = count(t"lost")
          val still: Boolean = Swarm.connected(t"steady")

          // A listener which welcomes a caller and then never sends a beat.
          val gate: () => Optional[Text] = () => Unset

          val deaf: Peer.Listener[Data] =
            Peer.Listener[Data](t"fury", t"0.0.0", Wire.codec, secret, identity, Nil, gate):
              session =>
                session.send(Wire.offer)
                session.receive()
                snooze(8.0*Second)

          async(deaf.serve(there.number))
          snooze(0.5*Second)
          Swarm.connect(silent)
          await(40)(about(t"silent", t"lost") >= 1)
          await(25)(about(t"silent", t"connecting") >= 2)
          val lostByCaller: Int = about(t"silent", t"lost")
          val retried: Int = about(t"silent", t"retrying")
          val attempts: Int = about(t"silent", t"connecting")

          // A caller which speaks another protocol: it says what it accepts, which is nothing
          // this instance can write.
          val before2: Int = count(t"unnegotiated")

          async:
            safely:
              Peer.connect[Data, Unit](steady, t"fury", t"0.0.0", Wire.codec, here.number):
                session =>
                  session.send(Stranger.offer)
                  session.receive()
                  snooze(1.0*Second)

          await(25)(count(t"unnegotiated") > before2)
          val strangers: Int = count(t"unnegotiated") - before2

          // A caller which begins with something other than an acceptance.
          async:
            safely:
              Peer.connect[Data, Unit](steady, t"fury", t"0.0.0", Wire.codec, here.number):
                session =>
                  session.send(Stranger.document)
                  session.receive()
                  snooze(1.0*Second)

          await(25)(count(t"unnegotiated") > before2 + strangers)
          val unannounced: Int = count(t"unnegotiated") - before2 - strangers

          // A listener which speaks another protocol, pinged by this instance.
          val elsewhere: Tcp.Port = Port[Tcp]()

          val foreign: Peer.Listener[Data] =
            Peer.Listener[Data](t"fury", t"0.0.0", Wire.codec, secret, identity, Nil, gate):
              session =>
                session.send(Stranger.offer)
                session.receive()
                snooze(1.0*Second)

          async(foreign.serve(elsewhere.number))
          snooze(0.5*Second)

          val stranger: Machine =
            Machine(t"stranger", List(t"127.0.0.1"), elsewhere.number, fingerprint, secret, Nil)

          val mismatched: Optional[Swarm.Error.Reason] = failure(stranger)
          foreign.stop()

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
              retried, attempts, released, count(t"negotiated"), strangers, unannounced,
              mismatched, callee.let(_.advert), caller.let(_.advert), callee.let(_.load) )

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

      test(m"each end says what it accepts before anything else is said"):
        lasting.negotiated >= 2

      . assert(_ == true)

      test(m"a caller which accepts another protocol is told apart, and not spoken to"):
        lasting.strangers

      . assert(_ == 1)

      test(m"a caller which does not begin with an acceptance is not spoken to"):
        lasting.unannounced

      . assert(_ == 1)

      test(m"a ping to an instance of another protocol fails for that reason"):
        lasting.mismatched

      . assert(_ == Swarm.Error.Reason.Mismatched)

      // Both ends of the one connection are this machine, so each learns of the other what this
      // machine advertises.
      test(m"the end which connected learns what the other is"):
        lasting.calleeAdvert

      . assert(_ == Swarm.advert)

      test(m"the end which was connected to learns what its caller is"):
        lasting.callerAdvert

      . assert(_ == Swarm.advert)

      test(m"each beat brings the sender's load, where its platform keeps one"):
        lasting.calleeLoad.present == Swarm.load.present

      . assert(_ == true)

    suite(m"Invitations"):
      // A listener in this JVM invites, and this JVM joins: the invitation is accepted once,
      // the machine is declared, a connection is kept to it, and once revoked it is refused.
      // The declaration and the granted token are written to a configuration directory of the
      // test's own, so that the user's `machines.tel` is untouched; the listener's records of
      // invitations and admissions are the machine's, as they must be for it to see them.
      val invited: Invited =
        supervise:
          val config: Text = java.nio.file.Files.createTempDirectory("fury-config").nn.toString.tt

          given Environment = name =>
            if name == t"XDG_CONFIG_HOME" then config
            else Optional(java.lang.System.getenv(name.s)).let(_.tt)

          val port: Tcp.Port = Port[Tcp]()
          async(Swarm.service.serve(port.number, settings))
          await(25)(Swarm.listening.present)

          val made: Invitation = unsafely(Swarm.invite(port, 60.0*Second))
          val word: Text = Invitation.encode(made)
          val read: Optional[Invitation] = safely(Invitation.parse(word))

          // The invitation lists every address of this machine; the test joins over loopback.
          val local: Optional[Invitation] = read.let(_.copy(hosts = List(t"127.0.0.1")))

          def join(name: Text): Optional[Machine] =
            local.let: invitation => safely(Swarm.join(invitation, name))

          val joined: Optional[Machine] = join(t"inviter")
          val again: Optional[Machine] = join(t"again")

          val declared: Boolean =
            Machine.shared.let(Machine.parse(_).stdlib.exists(_.name == t"inviter")).or(false)

          joined.let(Swarm.connect(_))
          await(50)(Swarm.connected(t"inviter"))
          val connected: Boolean = Swarm.connected(t"inviter")
          val admitted: Boolean = Swarm.peers.stdlib.contains(Machine.Identity.local.hostname)

          Swarm.disconnect(t"inviter")
          await(25)(!Swarm.connected(t"inviter"))
          val revoked: Int = Swarm.revoke(Machine.Identity.local.hostname)

          val afterwards: Optional[Text] = joined.let: machine =>
            attempt[Swarm.Error](Swarm.ping(machine, t"")) match
              case Attempt.Failure(failed) => failed.reason match
                case Swarm.Error.Reason.Connection(Peer.Error.Reason.Refused(reason)) => reason
                case other                                                            => t"$other"

              case Attempt.Success(_) =>
                t"answered"

          Swarm.service.stop()
          await(25)(Swarm.listening.absent)

          Invited
            ( word.cut(t" ").stdlib.length, read.present, joined.present, again.absent, declared,
              connected, admitted, revoked, afterwards )

      test(m"an invitation is one word, and reads back"):
        (invited.words, invited.read)

      . assert(_ == (1, true))

      test(m"an invitation is accepted once"):
        (invited.joined, invited.again)

      . assert(_ == (true, true))

      test(m"the machine joined is declared in the shared machines.tel"):
        invited.declared

      . assert(_ == true)

      test(m"a connection is kept to the machine joined"):
        invited.connected

      . assert(_ == true)

      test(m"the inviting machine lists the joiner among those it admitted"):
        invited.admitted

      . assert(_ == true)

      test(m"a machine revoked is refused"):
        (invited.revoked >= 1, invited.afterwards)

      . assert(_ == (true, Peer.Refusal.token))
