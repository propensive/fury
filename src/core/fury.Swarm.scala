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

// One Fury talking to another (fury.md §8), at its first rungs. This daemon LISTENS for other
// instances when its configuration says `listen`, or from `fury listen` until `fury listen
// stop`; it keeps a lasting connection to each machine it is asked to CONNECT to, by
// `fury connect` or a `connect` line of its configuration, trying again whenever the connection
// cannot be made or is lost; and `fury ping` says `ping` to a machine and waits for its `pong`,
// over the lasting connection if there is one, or one made for the purpose.
//
// A lasting connection is kept honest by a heartbeat: each end sends a `beat` every second, and
// an end which hears nothing for three seconds takes the connection for lost, says so, and
// closes it. The end which made the connection then tries to make it again.
//
// Both ends log what they do, so `fury log` on each machine shows a message arrive and its
// answer return. A message is logged as sent BEFORE it is written, and as received after it is
// read, so that no log can show a message arriving before it left.
//
// The transport is Pyrocosm's, as fume's is: TLS to the machine's self-signed certificate, which
// the caller pins by fingerprint, with a shared token proving the caller, and BinTEL messages in
// a length-prefixed framing.
object Swarm:
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Connection(reason) => m"$reason"
        case Unanswered         => m"the connection closed before a pong arrived"
        case Silent             => m"no pong arrived in time"

    // Why a machine could not be pinged: the connection to it could not be made, for the
    // reason Pyrocosm gives; it was made and then closed without the `pong`; or it stayed open
    // and the `pong` did not come.
    enum Reason:
      case Connection(reason: Peer.Error.Reason)
      case Unanswered
      case Silent

  case class Error(machine: Machine, reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(m"the ping to ${machine.name} failed because $reason")

  // What a `pong` told the caller: who answered, with which Fury, and how long the round trip
  // took.
  case class Reply(hostname: Hostname, version: Optional[Semver], elapsed: Duration)

  // A lasting connection this daemon is asked to keep, and when it last heard from the other
  // end, if it is connected now.
  case class Connection(machine: Machine, heard: Optional[Instant over Unix])

  // How often each end of a lasting connection says it is still there, and how long an end
  // waits, hearing nothing, before it takes the connection for lost.
  val interval: Duration = 1.0*Second
  val patience: Duration = 3.0*Second

  // How long the end which made a connection waits before making it again: a second at first,
  // twice as long after each failure, and half a minute at most.
  def delay(failures: Int): Duration = (1 << failures.min(5)).min(30).toDouble*Second

  // What this object does of its own accord — for the daemon, outside any invocation — it logs
  // to the daemon's journal.
  private given sink: (LogSink[Any, Message]^{}) = Journal.sink

  private def identity: Optional[Peer.Identity] = safely(Peer.identity)

  // What the other end says of itself arrives as text, which need not be what it claims to be.
  private def hostname(text: Text): Optional[Hostname] = safely(text.as[Hostname])
  private def version(text: Text): Optional[Semver] = safely(text.as[Semver])

  // This machine's name, as it tells it to a caller.
  def local: Hostname = hostname(Machine.Identity.local.hostname).or(host"localhost")

  private def identifier(): Text = Uuid().show.keep(8)

  // One connection, at either end of it. Both ends do the same things with it: answer a `ping`,
  // deliver a `pong` to whoever asked for it, and — once the connection is a lasting one — send
  // a `beat` each second and listen for the other end's.
  private class Tether(val peer: Party, val release: Optional[Semver], session: Peer.Session[Wire]):
    private val mutex: Mutex = Mutex()
    private val open: Atomic[Boolean] = Atomic(true)
    private val beating: Atomic[Boolean] = Atomic(false)
    private val silent: Atomic[Boolean] = Atomic(false)

    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var last: Instant over Unix = now()

    @scala.caps.unsafe.untrackedCaptures
    private var asked: List[(Text, Promise[Wire.Pong])] = Nil

    // When the other end was last heard from.
    def heard: Instant over Unix = last

    // Whether the connection was given up for lost, having gone silent.
    def lost: Boolean = silent()

    def close(): Unit = if open.swap(false) then safely(session.close())

    // Writes a message, having logged it; a connection which cannot be written to is closed.
    private def send(message: Wire): Unit =
      Journal.log(Event.Sent(message, peer))
      try session.send(message) catch case _: Exception => close()

    // Sends a beat each second for as long as the connection is open, and gives it up for lost
    // if the other end has not been heard from for too long. Run on a task of its own.
    def beat()(using Monitor): Unit =
      if open() then
        val silence: Duration = now() - last

        if silence > patience then
          Journal.log(Event.Lost(peer, silence))
          silent() = true
          close()
        else
          send(Wire.Beat(now()))
          snooze(interval)
          beat()

    // Reads what the other end says until the connection closes. `lasting` is run when the
    // first beat arrives, which is how a listener learns that the caller means to stay.
    def read(lasting: => Unit): Unit =
      val frame: Channel.Frame[Wire] =
        try session.receive() catch case _: Exception => Channel.Frame.Closed

      frame match
        case Channel.Frame.Message(beat: Wire.Beat) =>
          last = now()
          Journal.log(Event.Received(beat, peer))
          if !beating.swap(true) then lasting
          read(lasting)

        case Channel.Frame.Message(ping: Wire.Ping) =>
          last = now()
          Journal.log(Event.Received(ping, peer))
          send(Wire.Pong(ping.id, now(), local))
          read(lasting)

        case Channel.Frame.Message(pong: Wire.Pong) =>
          last = now()
          Journal.log(Event.Received(pong, peer))
          mutex(asked.filter(_(0) == pong.id)).each: (_, promise) => promise.offer(pong)
          read(lasting)

        case Channel.Frame.Closed =>
          close()

        case _ =>
          read(lasting)

    // Says `ping` and waits for the `pong` which answers it, which `read` delivers.
    // The result is spelt out, where `raises` would do, to say that it holds the monitor.
    def ask(machine: Machine, note: Text)(using monitor: Monitor)
    :   (Tactic[Error]^) ?->{monitor} Reply =

      val id: Text = identifier()
      val promise: Promise[Wire.Pong] = Promise()
      mutex { asked = (id, promise) :: asked }
      val sent: Instant over Unix = now()
      send(Wire.Ping(id, sent, note))
      val pong: Optional[Wire.Pong] = safely(promise.await(patience))
      mutex { asked = asked.filter(_(0) != id) }

      pong.lay(abort(Error(machine, Error.Reason.Silent))): pong =>
        Reply(pong.hostname, release, now() - sent)

  // ── listening ─────────────────────────────────────────────────────────────────────────────

  // The port this daemon is listening on, while it is; zero while it is not.
  private val bound: Atomic[Int] = Atomic(0)

  // The port this daemon is listening on now, by configuration or by `fury listen`, if it is.
  def listening: Optional[Tcp.Port] = bound() match
    case 0      => Unset
    case number => Port.unsafe[Tcp](number)

  // Starts listening on `port` in the background, under the daemon's monitor, so that the
  // listener outlives the invocation which asked for it; `fury listen stop`, or the daemon's
  // end, stops it. Nothing is started if the daemon is listening already.
  def start(port: Tcp.Port, settings: Text -> Optional[Text])(using Monitor, Probate): Unit =
    if listening.absent then async(service.serve(port.number, settings))

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
              bound() = number
              Journal.log(Event.Listening(port, Fingerprint(identity.fingerprint)))

              // `serve` blocks until `stop`, and returns at once, saying nothing, if the port
              // could not be bound; which of the two happened is what `stopping` tells apart.
              try made.serve(number) finally
                if stopping() then Journal.log(Event.Stopped(port))
                else Journal.log(Event.ListenFailed(port, Obstacle.Unbound))

                bound() = 0

            case _ =>
              Journal.log(Event.ListenFailed(port, Obstacle.NoToken))

        case _ =>
          Journal.log(Event.ListenFailed(port, Obstacle.NoIdentity))

    // Stops accepting callers, and hangs up on those there are.
    def stop(): Unit =
      stopping() = true
      listener.let(_.stop())
      mutex(callers).each(_.close())

  private val mutex: Mutex = Mutex()

  // The callers connected to this daemon now.
  @scala.caps.unsafe.untrackedCaptures
  private var callers: List[Tether] = Nil

  // One caller's connection, for as long as the caller keeps it. A caller which sends a beat
  // means to stay, and is sent beats in return, and watched for silence.
  private def answer(session: Peer.Session[Wire])(using Monitor, Probate): Unit =
    val peer: Party = Party.Caller(hostname(session.peer.identity.hostname))
    val link: Tether = Tether(peer, version(session.peer.version), session)
    Journal.log(Event.Accepted(peer, link.release))
    mutex { callers = link :: callers }

    def lasting(): Unit =
      Journal.log(Event.Linked(peer))
      async(link.beat())

    link.read(lasting())
    link.close()
    mutex { callers = callers.filter(_ != link) }
    if !link.lost then Journal.log(Event.Closed(peer))

  // ── connecting ────────────────────────────────────────────────────────────────────────────

  // The machines this daemon is asked to stay connected to, and the connections it has now.
  @scala.caps.unsafe.untrackedCaptures
  private var wanted: List[Machine] = Nil

  @scala.caps.unsafe.untrackedCaptures
  private var links: List[(Text, Tether)] = Nil

  private def link(name: Text): Optional[Tether] = mutex(links.filter(_(0) == name)).prim.let(_(1))
  private def wants(name: Text): Boolean = mutex(wanted.filter(_.name == name)).prim.present

  // What this daemon is asked to stay connected to, and how each connection stands.
  def connections: List[Connection] =
    mutex(wanted).reverse.map: machine => Connection(machine, link(machine.name).let(_.heard))

  // Whether a lasting connection to the machine of this name is open now.
  def connected(name: Text): Boolean = link(name).present

  // Keeps a lasting connection to `machine` from now on, in the background, under the daemon's
  // monitor: until `disconnect`, or the daemon's end. Nothing more is done for a machine this
  // daemon is keeping a connection to already.
  def connect(machine: Machine)(using Monitor, Probate): Unit =
    val added: Boolean = mutex:
      val absent: Boolean = wanted.filter(_.name == machine.name).prim.absent
      if absent then wanted = machine :: wanted
      absent

    if added then async(keep(machine, 0))

  // Stops keeping a connection to the machine of this name, closing it if it is open.
  def disconnect(name: Text): Boolean =
    val found: Boolean = mutex:
      val present: Boolean = wanted.filter(_.name == name).prim.present
      wanted = wanted.filter(_.name != name)
      present

    link(name).let(_.close())
    found

  def disconnect(): Unit = mutex(wanted).each: machine => disconnect(machine.name)

  // Makes the connection, keeps it for as long as it lasts, and makes it again — after a pause
  // which grows with each failure in a row — until this daemon no longer wants it.
  private def keep(machine: Machine, failures: Int)(using Monitor, Probate): Unit =
    val peer: Party = Party.Callee(machine)

    if wants(machine.name) then
      Journal.log(Event.Connecting(machine, Port.unsafe[Tcp](machine.portOr(Wire.port.number))))

      val made: Attempt[Unit, Peer.Error] = attempt[Peer.Error]:
        Peer.connect[Wire, Unit](machine, t"fury", Fury.version, Wire.codec, Wire.port.number):
          session =>
            val theirs: Optional[Semver] = version(session.peer.version)
            val link: Tether = Tether(peer, theirs, session)
            Journal.log(Event.Welcomed(peer, hostname(session.peer.identity.hostname), theirs))
            mutex { links = (machine.name, link) :: links }
            Journal.log(Event.Linked(peer))
            async(link.beat())
            link.read(())
            link.close()
            mutex { links = links.filter(_(0) != machine.name) }
            if !link.lost && wants(machine.name) then Journal.log(Event.Closed(peer))

      val next: Int = made match
        case Attempt.Failure(error) =>
          Journal.log(Event.Failed(machine, Error.Reason.Connection(error.reason)))
          failures + 1

        case Attempt.Success(_) =>
          0

      if wants(machine.name) then
        Journal.log(Event.Retrying(machine, delay(next)))
        snooze(delay(next))
        keep(machine, next)
      else
        Journal.log(Event.Unlinked(peer))

    else
      Journal.log(Event.Unlinked(peer))

  // The machines a `connect` line of the configuration names are connected to when the daemon
  // starts, as `listen` makes it listen. A service has no invocation, and so no project: the
  // machines are those the user's own configuration and the shared file declare.
  val connector: Tool.Service = new Tool.Service:
    def keyword: Text = t"connect"
    def portKeyword: Text = t"connectPort"
    def port: Int = 0

    def serve(number: Int, settings: Text => Optional[Text])
      ( using monitor: Monitor, probate: Probate )
    :   Unit =

      val known: List[Machine] = Machine.resolve(List(Fury.userConfig, Machine.shared))
      val names: List[Text] = settings(t"connect").lay(Nil: List[Text])(_.cut(t":"))
      val waiting: Promise[Unit] = Promise()
      stopped = waiting
      known.filter { machine => names.has(machine.name) }.each: machine => connect(machine)

      // The connections are kept by tasks of this one, which end when it does, so it waits
      // here for as long as the service runs.
      safely(waiting.attend())

    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var stopped: Optional[Promise[Unit]] = Unset

    def stop(): Unit =
      disconnect()
      stopped.let(_.offer(()))

  // ── pinging ───────────────────────────────────────────────────────────────────────────────

  private def failed(machine: Machine, reason: Error.Reason): Error.Reason =
    Journal.log(Event.Failed(machine, reason))
    reason

  // A `ping` over a connection made for the purpose, and closed once it is answered.
  private def once(machine: Machine, note: Text): Reply raises Error =
    val peer: Party = Party.Callee(machine)
    Journal.log(Event.Connecting(machine, Port.unsafe[Tcp](machine.portOr(Wire.port.number))))

    mitigate:
      case Peer.Error(reason) => Error(machine, failed(machine, Error.Reason.Connection(reason)))

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
                abort(Error(machine, failed(machine, Error.Reason.Unanswered)))

  // A `ping` over the lasting connection to `machine`.
  private def over(link: Tether, machine: Machine, note: Text)(using monitor: Monitor)
  :   (Tactic[Error]^) ?->{monitor} Reply =

    mitigate:
      case Error(_, reason) => Error(machine, failed(machine, reason))

    . protect(link.ask(machine, note))

  // Says `ping` to `machine` and waits for its `pong`: over the lasting connection to it, if
  // there is one, and otherwise over a connection made for the purpose. A failure is logged, to
  // the daemon's journal, as it is raised. The result is spelt out, where `raises` would do, to
  // say that it holds the monitor.
  def ping(machine: Machine, note: Text)(using monitor: Monitor)
  :   (Tactic[Error]^) ?->{monitor} Reply =

    link(machine.name).lay(once(machine, note))(over(_, machine, note))
