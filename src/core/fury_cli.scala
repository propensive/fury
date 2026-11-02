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
import interpreters.posixInterpreter
import pyrocosm.{ambient, Tool}
import systems.javaBaseSystem
import threads.platformThreads

// Fury as a Pyrocosm tool (fury.md): `about`, `install`, `quit` and `--version` come from `Tool`,
// as does its configuration — the flag, then a `fury.`-prefixed system property, a `FURY_`-
// prefixed environment variable, the project's `.pyrocosm/fury/config.tel` and the user's
// `~/.config/fury/config.tel`. That file has one role — how the Fury program runs on this
// machine (daemon, web front-end, swarm membership) — and never affects what a build produces
// (fury.md §11). The daemon is a node of the Pyrocosm swarm, as every tool's is: `fury swarm`
// and its subcommands come from `Tool`.
val Fury: Tool =
  Tool
    ( t"fury",
      prose = t"Fury is the LIRA build tool: it reads a build file, plans a static DAG of steps " +
        t"and runs them through tools, memoized in the content-addressed store, on this " +
        t"machine or across a swarm of them." )

object ui:
  val Check = Subcommand("check", "parse and validate the build file")

object Client uses parasite.threads, surveillance.Watcher:
  def run(): Unit =
    // The completions executive supervises under the global `threads`, a capability it declares it
    // uses; `cli` takes a pure executive, so it is vouched pure here.
    given executive: exoskeleton.executives.CompletionsExecutive =
      scala.caps.unsafe.unsafeAssumePure(executives.completionsExecutive)

    cli:
      Fury.standard:
        dispatch()

// The dispatch is a method rather than the body of the `cli` block: that block runs on a
// connection's task and may retain only what is durable, a rule the inline body's closures fail
// under, where a method's are typed on their own.
private def dispatch()(using Cli, Resident, Monitor, Environment, Interpreter, Configurator)
:   Execution =

  val width: Int = Fury.columns

  arguments match
    case ui.Check() :: _ =>
      ambient:
        Check.run(summon[WorkingDirectory].directory(), width)

    case _ =>
      ambient(Fury.usage(List(ui.Check)))
