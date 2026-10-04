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
      services = List(Swarm.service) )

// Exit statuses are declared as objects (soundness#1811), so that an `execute` block's result
// type documents the precise union in the manpage's EXIT STATUS section.
object UsageError extends Status(2, t"the command line was not understood")
object NoMachine extends Status(11, t"no machine of that name is configured")
object RemoteFailed extends Status(12, t"the connection to another machine could not be made")

object ui:
  val Check = Subcommand("check", "parse and validate the build file")
  val Identity = Subcommand("identity", "show this machine's identity, for another to declare")
  val Listen = Subcommand("listen", "accept connections from other instances until Ctrl+C")
  val Ping = Subcommand("ping", "send a message to a configured machine and await its answer")
  val Log = Subcommand("log", "show what this instance of Fury has been doing")

  val Follow =
    Flag[Unit]("follow", false, proscenium.List('f'), "keep showing events until Ctrl+C")

  // The least a logged event must matter for `fury log` to show it: `--level`, `log-level` in
  // either config file, and so on. Everything is shown by default.
  val Threshold =
    Setting[Text](t"logLevel", t"show only events at this level or above: fine, info, warn or fail")

  // The port `fury listen` accepts other instances on, and the port the daemon listens on when
  // a config says `listen`: `--listen-port`, the `fury.listenPort` property, `FURY_LISTEN_PORT`
  // and `listen-port` in either config file all reach it.
  val ListenPort =
    Setting[Text](t"listenPort", t"the port on which Fury accepts connections from other instances")

// The levels an event may be logged at, by the names `--log-level` takes.
private val levels: Map[Text, Level] =
  Map(t"fine" -> Level.Fine, t"info" -> Level.Info, t"warn" -> Level.Warn, t"fail" -> Level.Fail)

// The machines declared to this invocation: the repository's configuration, then the user's,
// then the shared `~/.config/pyrocosm/machines.tel`, a name's first declaration winning. Read
// eagerly, in the pure section, so that `ping` can complete to the names.
private def machines()(using cli: Cli, environment: Environment): List[Machine] =
  val directory: Text = cli.workingDirectory.directory()
  Machine.resolve(List(Fury.repoConfig(directory), Fury.userConfig, Machine.shared))

// How `fury ping` can end, once the machine is known.
private type Pinged = Exit | RemoteFailed.type

// `fury ping`, once the machine is known: says what answered, or why nothing did. Both the
// handler and the block are given the one type, since a recovery's result is typed by its
// block's alone.
private def ping(machine: Machine, note: Text)(using Stdio): Pinged logs Journal.Event =
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
          case Argument(head) :: _ if head == t"quit" => retiring() = true
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

        // `fury listen [--listen-port]` — accept other instances until Ctrl+C, as the daemon does
        // for as long as it lives when a config says `listen`.
        case ui.Listen() :: _ =>
          val port: Int = ui.ListenPort() match
            case text: Text => safely(text.as[Int]).or(Wire.port.number)
            case _          => Wire.port.number

          execute:
            given Stdio = summon[Invocation].stdio
            import probates.cancelProbate

            // The daemon's own listener is the configuration's to stop, not this command's.
            safely(Peer.identity) match
              case _ if Swarm.listening =>
                Out.println(t"this daemon is already listening, as its configuration says")
                Exit.Ok

              case identity: Peer.Identity =>
                Out.println(t"this machine's identity is ${Peer.render(identity.fingerprint)}")
                Out.println(t"accepting other instances of Fury on port $port until Ctrl+C")

                val stopped: Atomic[Boolean] = Atomic(false)

                async:
                  try Fury.run(Swarm.service, port) finally stopped() = true

                val seen: Atomic[Long] = Atomic(Journal.daemon.latest)
                val failed: Atomic[Boolean] = Atomic(false)

                // The one failure a listener logs is that it could not start.
                def show(): Unit =
                  Journal.daemon.since(seen()).each: entry =>
                    Out.println(Journal.render(entry))
                    seen() = entry.sequence
                    if entry.level == Level.Fail then failed() = true

                until(stopped())(show())
                Swarm.service.stop()

                // The listener records that it stopped, or why it never started, as it ends.
                snooze(0.25*Second)
                show()

                if failed() then RemoteFailed else
                  Out.println(t"the listener has stopped")
                  Exit.Ok

              case _ =>
                Out.println(t"this machine's identity could not be created; is `keytool` there?")
                RemoteFailed

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
            given (LogSink[Any, Message]^{}) = Journal.sink

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

        // `fury log [--follow] [--level]` — what this instance has been doing, oldest first:
        // everything, or only what was logged at the given level or above.
        case ui.Log() :: _ =>
          val follow: Boolean = ui.Follow().present
          val level: Level = ui.Threshold().let(levels.at(_)).or(Level.Fine)

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
            Out.println(t"  listen     accept connections from other instances until Ctrl+C")
            Out.println(t"  identity   show this machine's identity, for another to declare")
            Out.println(t"  about      show this tool's name, version and daemon")
            Out.println(t"  install    install shell tab-completions and the manpage")
            Out.println(t"  quit       stop the background daemon")
            UsageError
