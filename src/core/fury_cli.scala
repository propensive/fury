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

// `pyrocosm.Tool` is imported explicitly, so that it outranks the `Tool` the `soundness.*`
// wildcard exports (anthology's); `standard` is a package-level extension on it.
import backstops.silentBackstop
import executives.completionsExecutive
import interpreters.posixInterpreter
import pyrocosm.{Machine, Peer, Tool}
import systems.javaBaseSystem
import threading.platformThreading

// Fury as a Pyrocosm tool (fury.md): `about`, `install`, `quit` and `--version` come from `Tool`,
// as does its configuration — the flag, then a `fury.`-prefixed system property, a `FURY_`-
// prefixed environment variable, the project's `.pyrocosm/fury/config.tel` and the user's
// `~/.config/fury/config.tel`. That file has one role — how the Fury program runs on this
// machine (daemon, web front-end, swarm membership) — and never affects what a build produces
// (fury.md §11). The daemon also listens for other instances of Fury when that configuration
// says `listen` (`Swarm.service`).
val Fury: Tool =
  Tool
    ( t"fury",
      prose = t"Fury is the LIRA build tool: it reads a build file, plans a static DAG of steps " +
        t"and runs them through tools, memoized in the content-addressed store, on this " +
        t"machine or across a swarm of them.",
      services = List(Swarm.service, Swarm.connector) )

// Exit statuses are declared as objects (soundness#1811), so that an `execute` block's result
// type documents the precise union in the manpage's EXIT STATUS section.
object UsageError extends Status(2, t"the command line was not understood")
object NoMachine extends Status(11, t"no machine of that name is configured")
object RemoteFailed extends Status(12, t"the connection to another machine could not be made")

object ui:
  val Check = Subcommand("check", "parse and validate the build file")
  val Identity = Subcommand("identity", "show this machine's identity, for another to declare")
  val Listen = Subcommand("listen", "accept connections from other instances, in the background")
  val Stop = Subcommand("stop", "stop accepting connections from other instances")
  val Ping = Subcommand("ping", "send a message to a configured machine and await its answer")
  val Connect = Subcommand("connect", "keep a connection to a configured machine open")
  val Disconnect = Subcommand("disconnect", "stop keeping a connection to a machine open")
  val Log = Subcommand("log", "show what this instance of Fury has been doing")

  val Follow =
    Flag[Unit]("follow", false, proscenium.List('f'), "keep showing events until Ctrl+C")

  // The least a logged event must matter for `fury log` to show it: `--log-level`, `log-level`
  // in either config file, and so on. `info` by default, which leaves out the beats.
  val Threshold =
    Setting[Text](t"logLevel", t"show only events at this level or above: fine, info, warn or fail")

  // The port `fury listen` accepts other instances on, and the port the daemon listens on when
  // a config says `listen`. On the command line it is `--port`, or `-p`; everywhere else it
  // is the listener's own — the `fury.listenPort` property, `FURY_LISTEN_PORT`, and
  // `listen-port` in either config file — since plain `port` there is the web front-end's.
  val ListenPort: Setting of Text =
    val description: Text = t"the port on which to accept connections from other instances"

    new Setting(t"listenPort", Flag[Text](t"port", false, List('p'), description), Unset):
      type Topic = Text

// The levels an event may be logged at, by the names `--log-level` takes.
private val levels: Map[Text, Level] =
  Map(t"fine" -> Level.Fine, t"info" -> Level.Info, t"warn" -> Level.Warn, t"fail" -> Level.Fail)

// The machines declared to this invocation: the repository's configuration, then the user's,
// then the shared `~/.config/pyrocosm/machines.tel`, a name's first declaration winning. Read
// eagerly, in the pure section, so that `ping` can complete to the names.
private def machines()(using cli: Cli, environment: Environment): List[Machine] =
  val directory: Text = cli.workingDirectory.directory()
  Machine.resolve(List(Fury.repoConfig(directory), Fury.userConfig, Machine.shared))

// How `fury listen` can end.
private type Listened = Exit | RemoteFailed.type

// `fury listen`: starts the listener in the background, waits long enough to see whether it
// could start, and says which. A listener which cannot bind its port gives up at once, saying
// why in the log, so one still listening after a moment has started.
private def listen(port: Tcp.Port, token: Optional[Text])
  ( using Stdio, Monitor, Probate, Environment )
:   Listened =

  val settings: Text -> Optional[Text] =
    keyword => if keyword == t"listenToken" then token else Unset

  val mark: Long = Journal.daemon.latest

  def already(port: Tcp.Port): Listened =
    Out.println(t"this daemon is already accepting other instances on port ${port.number}")
    Exit.Ok

  def accepting(port: Tcp.Port, identity: Peer.Identity): Listened =
    Out.println(t"accepting other instances of Fury on port ${port.number}, in the background")
    Out.println(t"this machine's identity is ${Peer.render(identity.fingerprint)}")
    Out.println(t"`fury listen stop` stops it; `fury log` shows what it does")
    Exit.Ok

  def failed(): Listened =
    Journal.daemon.since(mark, Level.Fail).each: entry => Out.println(entry.message.text)
    RemoteFailed

  def start(): Listened = safely(Peer.identity) match
    case identity: Peer.Identity =>
      Swarm.start(port, settings)
      snooze(0.5*Second)
      Swarm.listening.lay(failed())(accepting(_, identity))

    case _ =>
      Out.println(t"this machine's identity could not be created; is `keytool` there?")
      RemoteFailed

  Swarm.listening.lay(start())(already(_))

// How a command which names a machine can end.
private type Connected = Exit | NoMachine.type | UsageError.type

private def unknown(name: Text)(using Stdio): Connected =
  Out.println(t"no machine named $name is configured")
  NoMachine

private def usage(form: Text)(using Stdio): Connected =
  Out.println(t"Usage: $form")
  UsageError

// `fury disconnect <machine>`.
private def disconnect(name: Text)(using Stdio): Connected =
  if Swarm.disconnect(name)
  then Out.println(t"no longer keeping a connection to $name")
  else Out.println(t"this daemon is not keeping a connection to $name")

  Exit.Ok

// What is known of the other end of a connection, in a few words: what it advertised itself to
// be, how loaded it last said it was, and when it was last heard from.
private def described(standing: Swarm.Standing): Text =
  def machine(advert: Wire.Advert): Text = t"${advert.os} ${advert.arch}, ${advert.cores} cores; "

  def burden(load: Double): Text =
    val hundredths: Long = (load*100.0).toLong
    t"load ${hundredths/100}.${hundredths%100/10}${hundredths%10}; "

  def heard(instant: Instant over Unix): Text = t"last heard from at ${Journal.time(instant)}"

  val advert: Text = standing.advert.let(machine(_)).or(t"")
  val load: Text = standing.load.let(burden(_)).or(t"")
  val last: Text = standing.heard.let(heard(_)).or(t"")
  t"$advert$load$last"

// `fury connect`, with no machine: the connections this daemon is keeping, the callers which are
// keeping one to it, and how each stands.
private def connections()(using Stdio): Connected =
  val kept: List[Swarm.Connection] = Swarm.connections
  val visitors: List[Swarm.Caller] = Swarm.visitors

  def kept0(connection: Swarm.Connection): Text =
    val name: Text = connection.machine.name

    if connection.standing.heard.absent then t"$name  not connected; trying again"
    else t"$name  connected; ${described(connection.standing)}"

  def visitor(caller: Swarm.Caller): Text =
    t"  ${caller.peer.show}  ${described(caller.standing)}"

  if kept.nil then Out.println(t"this daemon is keeping no connections; see `fury connect`")
  kept.each: connection => Out.println(kept0(connection))

  if !visitors.nil then
    Out.println(t"")
    Out.println(t"connected to this daemon:")
    visitors.each: caller => Out.println(visitor(caller))

  Exit.Ok

// `fury connect <machine>`: asks the daemon to keep the connection, waits a moment to see whether
// it could be made, and says which. Either way the daemon goes on trying.
private def connect(machine: Machine)(using Stdio, Monitor, Probate): Connected =
  val name: Text = machine.name
  val mark: Long = Journal.daemon.latest

  def wait(attempts: Int): Unit =
    if attempts > 0 && !Swarm.connected(name) then
      snooze(0.1*Second)
      wait(attempts - 1)

  if Swarm.connected(name) then Out.println(t"this daemon is already connected to $name") else
    Swarm.connect(machine)
    wait(20)

    if Swarm.connected(name)
    then Out.println(t"connected to $name; `fury disconnect $name` closes the connection")
    else
      Journal.daemon.since(mark, Level.Warn).each: entry => Out.println(entry.message.text)
      Out.println(t"not connected to $name yet; this daemon will keep trying")

  Exit.Ok

// How `fury ping` can end, once the machine is known.
private type Pinged = Exit | RemoteFailed.type

// `fury ping`, once the machine is known: says what answered, or why nothing did. Both the
// handler and the block are given the one type, since a recovery's result is typed by its
// block's alone.
private def ping(machine: Machine, note: Text)(using Stdio, Monitor): Pinged =
  def failed(error: Swarm.Error): Pinged =
    Out.println(error.message.text)
    RemoteFailed

  def answered(reply: Swarm.Reply): Pinged =
    val release: Text = reply.version.lay(t"an unknown version"): version => t"fury $version"
    val elapsed: Int = (reply.elapsed.value*1000.0).toInt
    Out.println(t"pong from ${reply.hostname} ($release) in ${elapsed}ms")
    Exit.Ok

  recover:
    case error: Swarm.Error => failed(error)

  . protect:
      val reply: Swarm.Reply = Swarm.ping(machine, note)
      answered(reply)

// Set when `fury quit` is invoked. A daemon asked to stop serves no new invocation and waits for
// those in flight, so one that runs until interrupted — `log --follow`, `listen` — must end of
// its own accord, or it would hold the daemon in that state until its drain limit.
private val retiring: Atomic[Boolean] = Atomic(false)

// Runs `action` every quarter of a second until Ctrl+C — the signal, or, on a terminal, the
// byte the launcher forwards for it (or Ctrl+D) — or until `finished` says so, or the daemon is
// asked to stop.
private def until(finished: => Boolean)(action: => Unit)
  ( using cli: Cli, service: DaemonService[?], stdio: Stdio, monitor: Monitor )
:   Unit =

  val tty: Boolean = service.cliInput == ethereal.Terminus.Terminal
  val aborted: Atomic[Boolean] = Atomic(false)

  trap:
    case profanity.Signal(Interrupt.Int, _, _, _) =>
      aborted() = true
      SignalResponse.Accept

  def recur(): Unit =
    if tty then while stdio.in.available() > 0 do
      val byte: Int = stdio.in.read()
      if byte == 3 || byte == 4 then aborted() = true

    action

    if !aborted() && !finished && !retiring() then
      snooze(0.25*Second)
      recur()

  recur()

def run(): Unit =
  cli:
    // `quit` itself is `Tool.standard`'s to handle; this only notes that it was asked for, and
    // only for a real invocation, never a tab-completion of the word.
    summon[Cli] match
      case _: Invocation =>
        arguments match
          case Argument(head) :: _ if head == t"quit" =>
            retiring() = true
            Swarm.service.stop()
            Swarm.disconnect()

          case _                                      => ()

      case _ =>
        ()

    Fury.standard:
      arguments match
        case ui.Check() :: _ =>
          execute:
            given Stdio = summon[Invocation].stdio
            Check.run(summon[Invocation].workingDirectory.directory())

        // `fury identity` — this machine's certificate fingerprint, for the `machine` block
        // another machine declares it with, and where its token lives.
        case ui.Identity() :: _ =>
          execute:
            given Stdio = summon[Invocation].stdio

            safely(Peer.identity) match
              case identity: Peer.Identity =>
                val hostname: Text = Machine.Identity.local.hostname
                Peer.token
                Out.println(t"identity  ${Peer.render(identity.fingerprint)}")
                Peer.tokenFile.let: file => Out.println(t"token     ${file.encode}")
                Out.println(t"")
                Out.println(t"Declare this machine in another's ~/.config/fury/config.tel as:")
                Out.println(t"")
                Out.println(t"  machine $hostname")
                Out.println(t"    host      $hostname")
                Out.println(t"    port      ${Wire.port.number}")
                Out.println(t"    identity  ${Peer.render(identity.fingerprint)}")
                Out.println(t"    token     <a file there, holding the token file's contents>")
                Exit.Ok

              case _ =>
                Out.println(t"this machine's identity could not be created; is `keytool` there?")
                RemoteFailed

        // `fury listen stop` — stop accepting other instances, however the listener was started.
        // A listener the configuration asked for stays stopped for the rest of this daemon's
        // life, or until `fury listen`.
        case ui.Listen() :: ui.Stop() :: _ =>
          execute:
            given Stdio = summon[Invocation].stdio

            val listening: Optional[Tcp.Port] = Swarm.listening
            Swarm.service.stop()

            Out.println:
              listening.lay(t"this daemon is not listening"): port =>
                t"no longer accepting other instances of Fury on port ${port.number}"

            Exit.Ok

        // `fury listen [--port]` — start accepting other instances, in the background: the
        // daemon listens until `fury listen stop`, or for as long as it lives, as it does of its
        // own accord when a config says `listen`.
        case ui.Listen() :: _ =>
          val number: Optional[Int] = ui.ListenPort().let: text => safely(text.as[Int])
          val port: Tcp.Port = Port.unsafe[Tcp](number.or(Wire.port.number))

          // Read now, since the listener outlives this invocation and its configuration.
          val token: Optional[Text] = summon[Configurator].read(t"listenToken")

          execute:
            given Stdio = summon[Invocation].stdio
            import probates.cancelProbate

            listen(port, token)

        // `fury connect [machine]` — keep a lasting connection to a configured machine, in the
        // background: the daemon makes it, sends a beat over it each second, and makes it again
        // whenever it cannot be made or is lost, until `fury disconnect`. Without a machine, the
        // connections the daemon is keeping are listed.
        case ui.Connect() :: rest =>
          val known: List[Machine] = machines()

          rest.prim.let: argument =>
            val names: List[Suggestion] = known.map: machine => Suggestion(machine.name)
            summon[Cli].suggest(argument, names, t"", t"")

          val words: List[Text] = rest.map: (argument: Argument) => argument()

          execute:
            given Stdio = summon[Invocation].stdio
            import probates.cancelProbate

            words match
              case name :: _ =>
                known.seek(_.name == name) match
                  case machine: Machine => connect(machine)
                  case _                => unknown(name)

              case _ =>
                connections()

        // `fury disconnect <machine>` — stop keeping the connection to a machine.
        case ui.Disconnect() :: rest =>
          val known: List[Machine] = machines()

          rest.prim.let: argument =>
            val names: List[Suggestion] = known.map: machine => Suggestion(machine.name)
            summon[Cli].suggest(argument, names, t"", t"")

          val words: List[Text] = rest.map: (argument: Argument) => argument()

          execute:
            given Stdio = summon[Invocation].stdio

            words match
              case name :: _ => disconnect(name)
              case _         => usage(t"fury disconnect <machine>")

        // `fury ping <machine> [note …]` — say `ping` to a configured machine and wait for its
        // `pong`; the note is shown in that machine's log.
        case ui.Ping() :: rest =>
          val known: List[Machine] = machines()

          rest.prim.let: argument =>
            val names: List[Suggestion] = known.map: machine => Suggestion(machine.name)
            summon[Cli].suggest(argument, names, t"", t"")

          val words: List[Text] = rest.map: (argument: Argument) => argument()

          execute:
            given Stdio = summon[Invocation].stdio

            words match
              case name :: note =>
                known.seek(_.name == name) match
                  case machine: Machine =>
                    ping(machine, note.join(t" "))

                  case _ =>
                    Out.println(t"no machine named $name is configured")
                    NoMachine

              case _ =>
                Out.println(t"Usage: fury ping <machine> [note]")
                UsageError

        // `fury log [--follow] [--log-level]` — what this instance has been doing, oldest first:
        // everything logged at `info` or above, or at the given level or above. The beats of a
        // lasting connection are `fine`.
        case ui.Log() :: _ =>
          val follow: Boolean = ui.Follow().present
          val level: Level = ui.Threshold().let(levels.at(_)).or(Level.Info)

          execute:
            given Stdio = summon[Invocation].stdio
            val seen: Atomic[Long] = Atomic(0L)

            def show(): Unit =
              Journal.daemon.since(seen(), level).each: entry =>
                Out.println(Journal.render(entry))
                seen() = entry.sequence

            if follow then until(false)(show()) else show()
            Exit.Ok

        case _ =>
          execute:
            given Stdio = summon[Invocation].stdio
            Out.println(t"Usage: fury <subcommand>")
            Out.println(t"")
            Out.println(t"  check      parse and validate the build file")
            Out.println(t"  ping       send a message to a configured machine and await its answer")
            Out.println(t"  log        show what this instance of Fury has been doing")
            Out.println(t"  connect    keep a connection to a configured machine open")
            Out.println(t"  disconnect stop keeping a connection to a machine open")
            Out.println(t"  listen     accept connections from other instances, in the background")
            Out.println(t"  identity   show this machine's identity, for another to declare")
            Out.println(t"  about      show this tool's name, version and daemon")
            Out.println(t"  install    install shell tab-completions and the manpage")
            Out.println(t"  quit       stop the background daemon")
            UsageError
