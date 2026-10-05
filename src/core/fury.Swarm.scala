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

import Journal.{Event, Fingerprint, Mismatch, Obstacle, Party}
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
// the caller pins by fingerprint, with a shared token proving the caller, and a length-prefixed
// framing. What is framed is Fury's own: BinTEL documents under the protocol's schema
// (`wire.schema.tel`), each written to the acceptance the other end sent when the connection
// was made.
object Swarm:
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Connection(reason) => m"$reason"
        case Unanswered         => m"the connection closed before a pong arrived"
        case Silent             => m"no pong arrived in time"
        case Mismatched         => m"the two instances could not agree a protocol"

    // Why a machine could not be pinged: the connection to it could not be made, for the
    // reason Pyrocosm gives; it was made and then closed without the `pong`; it stayed open
    // and the `pong` did not come; or the two ends, having exchanged acceptances, found that
    // one could not read what the other writes.
    enum Reason:
      case Connection(reason: Peer.Error.Reason)
      case Unanswered
      case Silent
      case Mismatched

  case class Error(machine: Machine, reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(m"the ping to ${machine.name} failed because $reason")

  // What a `pong` told the caller: who answered, with which Fury, and how long the round trip
  // took.
  case class Reply(hostname: Hostname, version: Optional[Semver], elapsed: Duration)

  // What is known of the other end of a connection: when it was last heard from, what it
  // advertised itself to be, and how loaded it last said it was. All are absent of a machine
  // this daemon is asked to stay connected to but is not connected to now.
  case class Standing
    ( heard:  Optional[Instant over Unix] = Unset,
      advert: Optional[Wire.Advert]       = Unset,
      load:   Optional[Double]            = Unset )

  // A lasting connection this daemon is asked to keep, and how it stands.
  case class Connection(machine: Machine, standing: Standing)

  // A caller which has made a lasting connection to this daemon, and how it stands.
  case class Caller(peer: Party, standing: Standing)

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

  // What this machine is, as it advertises itself over a lasting connection (fury.md §8): what a
  // build would be placed by. It says nothing yet of the universes and tools it can serve, its
  // store or the work it would take on, there being none of those to speak of.
  def advert: Wire.Advert =
    val machine: Machine.Identity = Machine.Identity.local
    Wire.Advert(local, machine.os, machine.arch, machine.cores)

  // This machine's one-minute load average, where its platform keeps one: the JVM answers a
  // negative number where it does not, as it always does on Windows.
  def load: Optional[Double] =
    val average: Double =
      java.lang.management.ManagementFactory.getOperatingSystemMXBean.nn.getSystemLoadAverage

    if average < 0.0 then Unset else average

  // One connection, at either end of it. Both ends do the same things with it: answer a `ping`,
  // deliver a `pong` to whoever asked for it, and — once the connection is a lasting one — send
  // a `beat` each second and listen for the other end's.
  private class Tether
    ( val peer:    Party,
      val release: Optional[Semver],
      session:     Peer.Session[Data],
      theirs:      Tel.Acceptance ):

    private val mutex: Mutex = Mutex()
    private val open: Atomic[Boolean] = Atomic(true)
    private val beating: Atomic[Boolean] = Atomic(false)
    private val silent: Atomic[Boolean] = Atomic(false)

    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var last: Instant over Unix = now()

    @scala.caps.unsafe.untrackedCaptures
    private var asked: List[(Uuid, Promise[Wire.Pong])] = Nil

    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var told: Optional[Wire.Advert] = Unset

    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var burden: Optional[Double] = Unset

    // When the other end was last heard from, what it advertised, and its load at its last beat.
    def standing: Standing = Standing(last, told, burden)

    // Whether this is a lasting connection, on which beats have begun.
    def lasting: Boolean = beating()

    // Says what this machine is.
    def advertise(): Unit = send(advert)

    // Whether the connection was given up for lost, having gone silent.
    def lost: Boolean = silent()

    def close(): Unit = if open.swap(false) then safely(session.close())

    // Writes a message as the other end said it can read it, having logged it; a connection
    // which cannot be written to is closed. A message the other end does not accept is not
    // sent, and the log says so.
    def send(message: Wire): Unit =
      Wire.write(message, theirs).lay(Journal.log(Event.Unaccepted(message, peer))): document =>
        Journal.log(Event.Sent(message, peer))
        try session.send(document) catch case _: Exception => close()

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
          send(Wire.Beat(now(), load))
          snooze(interval)
          beat()

    // Reads what the other end says until the connection closes. `lasting` is run when the
    // first beat arrives, which is how a listener learns that the caller means to stay.
    def read(lasting: => Unit): Unit =
      val frame: Channel.Frame[Data] =
        try session.receive() catch case _: Exception => Channel.Frame.Closed

      frame match
        case Channel.Frame.Message(document) =>
          Wire.read(document) match
            case beat: Wire.Beat =>
              last = now()
              burden = beat.load
              Journal.log(Event.Received(beat, peer))
              if !beating.swap(true) then lasting

            case advert: Wire.Advert =>
              last = now()
              told = advert
              Journal.log(Event.Received(advert, peer))

            case ping: Wire.Ping =>
              last = now()
              Journal.log(Event.Received(ping, peer))
              send(Wire.Pong(ping.id, now(), local))

            case pong: Wire.Pong =>
              last = now()
              Journal.log(Event.Received(pong, peer))
              mutex(asked.filter(_(0) == pong.id)).each: (_, promise) => promise.offer(pong)

            case _ =>
              Journal.log(Event.Unread(peer))

          read(lasting)

        case Channel.Frame.Closed =>
          close()

        case _ =>
          read(lasting)

    // Says `ping` and waits for the `pong` which answers it, which `read` delivers.
    // The result is spelt out, where `raises` would do, to say that it holds the monitor.
    def ask(machine: Machine, note: Text)(using monitor: Monitor)
    :   (Tactic[Error]^) ?->{monitor} Reply =

      val id: Uuid = Uuid()
      val promise: Promise[Wire.Pong] = Promise()
      mutex { asked = (id, promise) :: asked }
      val sent: Instant over Unix = now()
      send(Wire.Ping(id, sent, note))
      val pong: Optional[Wire.Pong] = safely(promise.await(patience))
      mutex { asked = asked.filter(_(0) != id) }

      pong.lay(abort(Error(machine, Error.Reason.Silent))): pong =>
        Reply(pong.hostname, release, now() - sent)

  // What each end does first, once Pyrocosm has welcomed the connection: sends its acceptance
  // (BinTEL §8.4), which says what forms of the protocol it can read, and reads the other
  // end's. Both send before either reads, so neither waits on the other. What comes back is
  // the other end's acceptance, if it sent one and this instance can write to it.
  private def negotiate(peer: Party, session: Peer.Session[Data]): Optional[Tel.Acceptance] =
    val frame: Channel.Frame[Data] =
      try
        session.send(Wire.offer)
        session.receive()
      catch case _: Exception => Channel.Frame.Closed

    val theirs: Optional[Tel.Acceptance] = frame match
      case Channel.Frame.Message(document) => Wire.offered(document)
      case _                               => Unset

    theirs.lay(mismatch(peer, Mismatch.NoAcceptance)): theirs =>
      val probe: Wire = Wire.Beat(now(), Unset)

      if Wire.write(probe, theirs).absent then mismatch(peer, Mismatch.Unservable) else
        Journal.log(Event.Negotiated(peer))
        theirs

  private def mismatch(peer: Party, mismatch: Mismatch): Optional[Tel.Acceptance] =
    Journal.log(Event.Unnegotiated(peer, mismatch))
    Unset

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
    private var listener: Optional[Peer.Listener[Data]] = Unset

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

              val made: Peer.Listener[Data] =
                Peer.Listener[Data](t"fury", Fury.version, Wire.codec, token, identity, Nil, gate):
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
  private def answer(session: Peer.Session[Data])(using Monitor, Probate): Unit =
    val peer: Party = Party.Caller(hostname(session.peer.identity.hostname))
    val release: Optional[Semver] = version(session.peer.version)
    Journal.log(Event.Accepted(peer, release))

    negotiate(peer, session).let: theirs =>
      val link: Tether = Tether(peer, release, session, theirs)
      mutex { callers = link :: callers }

      def lasting(): Unit =
        Journal.log(Event.Linked(peer))
        link.advertise()
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
    def standing(machine: Machine): Standing = link(machine.name) match
      case link: Tether => link.standing
      case _            => Standing()

    mutex(wanted).reverse.map: machine => Connection(machine, standing(machine))

  // The callers which have made a lasting connection to this daemon, and how each stands.
  def visitors: List[Caller] =
    mutex(callers).reverse.filter(_.lasting).map: link => Caller(link.peer, link.standing)

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
        Peer.connect[Data, Unit](machine, t"fury", Fury.version, Wire.codec, Wire.port.number):
          session =>
            val release: Optional[Semver] = version(session.peer.version)
            Journal.log(Event.Welcomed(peer, hostname(session.peer.identity.hostname), release))

            negotiate(peer, session).let: theirs =>
              val link: Tether = Tether(peer, release, session, theirs)
              mutex { links = (machine.name, link) :: links }
              Journal.log(Event.Linked(peer))
              link.advertise()
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
        Peer.connect[Data, Reply](machine, t"fury", Fury.version, Wire.codec, Wire.port.number):
          session =>
            val release: Optional[Semver] = version(session.peer.version)
            Journal.log(Event.Welcomed(peer, hostname(session.peer.identity.hostname), release))

            val theirs: Tel.Acceptance = negotiate(peer, session).or:
              abort(Error(machine, failed(machine, Error.Reason.Mismatched)))

            val link: Tether = Tether(peer, release, session, theirs)
            val sent: Instant over Unix = now()
            link.send(Wire.Ping(Uuid(), sent, note))

            // The answer is the next document that is a `pong`; nothing else is expected here.
            val answer: Optional[Wire] = session.receive() match
              case Channel.Frame.Message(document) => Wire.read(document)
              case _                               => Unset

            answer match
              case pong: Wire.Pong =>
                Journal.log(Event.Received(pong, peer))
                Reply(pong.hostname, release, now() - sent)

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
