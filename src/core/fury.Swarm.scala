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

import Journal.{Event, Fingerprint, Obstacle, Party}
import environments.javaBaseEnvironment
import errorDiagnostics.emptyDiagnostics
import pyrocosm.{Channel, Machine, Peer, Tool}

// One Fury talking to another (fury.md §8), at its first rung: this daemon LISTENS for other
// instances when its configuration says `listen` (or for as long as `fury listen` runs), and
// `fury ping` connects to a configured machine, says `ping`, and waits for its `pong`. Both ends
// log what they do, so `fury log` on each machine shows the message arrive and its answer
// return. A message is logged as sent BEFORE it is written, and as received after it is read,
// so that no log can show a message arriving before it left.
//
// The transport is Pyrocosm's, as fume's is: TLS to the machine's self-signed certificate, which
// the caller pins by fingerprint, with a shared token proving the caller, and BinTEL messages in
// a length-prefixed framing. Each exchange is one short connection.
object Swarm:
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Connection(reason) => m"$reason"
        case Unanswered         => m"the connection closed before a pong arrived"

    // Why a machine could not be pinged: the connection to it could not be made, for the
    // reason Pyrocosm gives, or it was made and then closed without the `pong`.
    enum Reason:
      case Connection(reason: Peer.Error.Reason)
      case Unanswered

  case class Error(machine: Machine, reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(m"the ping to ${machine.name} failed because $reason")

  // What a `pong` told the caller: who answered, with which Fury, and how long the round trip
  // took.
  case class Reply(hostname: Hostname, version: Optional[Semver], elapsed: Duration)

  private def identity: Optional[Peer.Identity] = safely(Peer.identity)

  // What the other end says of itself arrives as text, which need not be what it claims to be.
  private def hostname(text: Text): Optional[Hostname] = safely(text.as[Hostname])
  private def version(text: Text): Optional[Semver] = safely(text.as[Semver])

  // This machine's name, as it tells it to a caller.
  def local: Hostname = hostname(Machine.Identity.local.hostname).or(host"localhost")

  private val active: Atomic[Boolean] = Atomic(false)

  // Whether this daemon is listening now, by configuration or by `fury listen`.
  def listening: Boolean = active()

  val service: Tool.Service = new Tool.Service:
    def keyword: Text = t"listen"
    def portKeyword: Text = t"listenPort"
    def port: Int = Wire.port.number

    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var listener: Optional[Peer.Listener[Wire]] = Unset

    private val stopping: Atomic[Boolean] = Atomic(false)

    // `listen-token` names the shared secret (a file, or the secret itself); without it the
    // machine's own token file is used, generated on first use.
    def serve(number: Int, settings: Text => Optional[Text])
      ( using monitor: Monitor, probate: Probate )
    :   Unit =

      // A service runs for the daemon, outside any invocation, so its sink is its own.
      given (LogSink[Any, Message]^{}) = Journal.sink
      val port: Tcp.Port = Port.unsafe[Tcp](number)

      identity match
        case identity: Peer.Identity =>
          settings(t"listenToken").let(Machine.secret(_)).or(Peer.token) match
            case token: Text =>
              val gate: () => Optional[Text] = () => Unset

              val made: Peer.Listener[Wire] =
                Peer.Listener[Wire](t"fury", Fury.version, Wire.codec, token, identity, Nil, gate):
                  session => Swarm.answer(session)

              listener = made
              stopping() = false
              active() = true
              Journal.log(Event.Listening(port, Fingerprint(identity.fingerprint)))

              // `serve` blocks until `stop`, and returns at once, saying nothing, if the port
              // could not be bound; which of the two happened is what `stopping` tells apart.
              try made.serve(number) finally
                if stopping() then Journal.log(Event.Stopped(port))
                else Journal.log(Event.ListenFailed(port, Obstacle.Unbound))

                active() = false

            case _ =>
              Journal.log(Event.ListenFailed(port, Obstacle.NoToken))

        case _ =>
          Journal.log(Event.ListenFailed(port, Obstacle.NoIdentity))

    def stop(): Unit =
      stopping() = true
      listener.let(_.stop())

  // One caller's connection: every `ping` is answered with a `pong` until the caller hangs up.
  private def answer(session: Peer.Session[Wire]): Unit logs Event =
    val peer: Party = Party.Caller(hostname(session.peer.identity.hostname))
    Journal.log(Event.Accepted(peer, version(session.peer.version)))

    def recur(): Unit = session.receive() match
      case Channel.Frame.Message(ping: Wire.Ping) =>
        Journal.log(Event.Received(ping, peer))
        val pong: Wire = Wire.Pong(ping.id, now(), local)
        Journal.log(Event.Sent(pong, peer))
        session.send(pong)
        recur()

      case Channel.Frame.Closed =>
        Journal.log(Event.Closed(peer))

      case _ =>
        recur()

    recur()

  private def identifier(): Text = Uuid().show.keep(8)

  // Says `ping` to `machine` and waits for its `pong`. A failure is logged as it is raised.
  def ping(machine: Machine, note: Text): Reply raises Error logs Event =
    val peer: Party = Party.Callee(machine)
    Journal.log(Event.Connecting(machine, Port.unsafe[Tcp](machine.portOr(Wire.port.number))))

    def logged(reason: Error.Reason): Error.Reason =
      Journal.log(Event.Failed(machine, reason))
      reason

    mitigate:
      case Peer.Error(reason) => Error(machine, logged(Error.Reason.Connection(reason)))

    . protect:
        Peer.connect[Wire, Reply](machine, t"fury", Fury.version, Wire.codec, Wire.port.number):
          session =>
            val theirs: Optional[Semver] = version(session.peer.version)
            Journal.log(Event.Welcomed(peer, hostname(session.peer.identity.hostname), theirs))

            val sent: Instant over Unix = now()
            val ping: Wire = Wire.Ping(identifier(), sent, note)
            Journal.log(Event.Sent(ping, peer))
            session.send(ping)

            session.receive() match
              case Channel.Frame.Message(pong: Wire.Pong) =>
                Journal.log(Event.Received(pong, peer))
                Reply(pong.hostname, theirs, now() - sent)

              case _ =>
                abort(Error(machine, logged(Error.Reason.Unanswered)))
