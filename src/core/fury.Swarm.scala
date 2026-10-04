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
import pyrocosm.{Channel, Machine, Peer, Tool}

// One Fury talking to another (fury.md §8), at its first rung: this daemon LISTENS for other
// instances when its configuration says `listen` (or for as long as `fury listen` runs), and
// `fury ping` connects to a configured machine, says `ping`, and waits for its `pong`. Both ends
// record what they do in the `Journal`, so `fury log` on each machine shows the message arrive
// and its answer return.
//
// The transport is Pyrocosm's, as fume's is: TLS to the machine's self-signed certificate, which
// the caller pins by fingerprint, with a shared token proving the caller, and BinTEL messages in
// a length-prefixed framing. Each exchange is one short connection.
object Swarm:
  // What a `pong` told the caller: who answered, and how long the round trip took.
  case class Reply(hostname: Text, version: Text, milliseconds: Long)

  private def identity: Optional[Peer.Identity] = safely(Peer.identity)

  def describe(message: Wire): Text = message match
    case Wire.Ping(id, _, note) => if note == t"" then t"ping $id" else t"ping $id ‘$note’"
    case Wire.Pong(id, _, _)    => t"pong $id"

  private val active: Atomic[Boolean] = Atomic(false)

  // Whether this daemon is listening now, by configuration or by `fury listen`.
  def listening: Boolean = active()

  val service: Tool.Service = new Tool.Service:
    def keyword: Text = t"listen"
    def portKeyword: Text = t"listenPort"
    def port: Int = Wire.port

    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var listener: Optional[Peer.Listener[Wire]] = Unset

    private val stopping: Atomic[Boolean] = Atomic(false)

    // `listen-token` names the shared secret (a file, or the secret itself); without it the
    // machine's own token file is used, generated on first use.
    def serve(port: Int, settings: Text => Optional[Text])(using monitor: Monitor, probate: Probate)
    :   Unit =

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
              Journal.record(Journal.Event.Listening(port, Peer.render(identity.fingerprint)))

              // `serve` blocks until `stop`, and returns at once, saying nothing, if the port
              // could not be bound; which of the two happened is what `stopping` tells apart.
              try made.serve(port) finally
                if stopping() then Journal.record(Journal.Event.Stopped(port)) else
                  Journal.record(Journal.Event.ListenFailed(port, t"the port could not be bound"))

                active() = false

            case _ =>
              Journal.record(Journal.Event.ListenFailed(port, t"this machine has no token"))

        case _ =>
          val reason: Text = t"this machine's identity could not be created"
          Journal.record(Journal.Event.ListenFailed(port, reason))

    def stop(): Unit =
      stopping() = true
      listener.let(_.stop())

  // One caller's connection: every `ping` is answered with a `pong` until the caller hangs up.
  private def answer(session: Peer.Session[Wire]): Unit =
    val peer: Text = session.peer.identity.hostname
    Journal.record(Journal.Event.Accepted(peer, session.peer.version))

    def recur(): Unit = session.receive() match
      case Channel.Frame.Message(ping: Wire.Ping) =>
        Journal.record(Journal.Event.Received(describe(ping), peer))
        val pong: Wire = Wire.Pong(ping.id, Journal.clock(), Machine.Identity.local.hostname)
        session.send(pong)
        Journal.record(Journal.Event.Sent(describe(pong), peer))
        recur()

      case Channel.Frame.Closed =>
        Journal.record(Journal.Event.Closed(peer))

      case _ =>
        recur()

    recur()

  private def identifier(): Text = java.util.UUID.randomUUID().toString.take(8).tt

  // Says `ping` to `machine` and waits for its `pong`.
  def ping(machine: Machine, note: Text): scala.Either[Text, Reply] =
    val port: Int = machine.portOr(Wire.port)
    Journal.record(Journal.Event.Connecting(machine.name, machine.host, port))

    val outcome: scala.Either[Peer.Error.Reason, Optional[Reply]] =
      Peer.exchange[Wire, Optional[Reply]](machine, t"fury", Fury.version, Wire.codec, Wire.port):
        session =>
          val hostname: Text = session.peer.identity.hostname
          Journal.record(Journal.Event.Welcomed(machine.name, hostname, session.peer.version))

          val sent: Long = Journal.clock()
          val ping: Wire = Wire.Ping(identifier(), sent, note)
          session.send(ping)
          Journal.record(Journal.Event.Sent(describe(ping), machine.name))

          session.receive() match
            case Channel.Frame.Message(pong: Wire.Pong) =>
              Journal.record(Journal.Event.Received(describe(pong), machine.name))
              Reply(pong.hostname, session.peer.version, Journal.clock() - sent)

            case _ =>
              Unset

    val result: scala.Either[Text, Reply] = outcome match
      case scala.Right(reply: Reply)  => scala.Right(reply)
      case scala.Right(_)             => scala.Left(t"the connection closed before a pong arrived")
      case scala.Left(reason)         => scala.Left(Peer.explain(reason))

    result match
      case scala.Left(reason) => Journal.record(Journal.Event.Failed(machine.name, reason))
      case _                  => ()

    result
