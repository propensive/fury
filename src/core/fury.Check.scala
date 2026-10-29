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

import scala.collection.immutable as sci

import soundness.*

import charsets.utf8Charset
import errorDiagnostics.emptyDiagnostics
import filesystemBackends.javaBaseFilesystem
import palettes.solarizedDarkGaugePalette
import parsing.trackPositions
import pyrocosm.{Block, TerminalRenderer}
import systems.javaBaseSystem
import tableStyles.thickTableStyle
import textMetrics.uniformMetric
import textSanitizers.skipSanitizer

object CheckFailed extends Status(1, t"the build file has errors")
object NoBuildFile extends Status(3, t"no build.tel was found at or above the working directory")

// `fury check` (fury.md §15, step 1): find the build file, parse it, validate it against
// build.schema.tel — accruing every violation rather than stopping at the first — decode it to
// the typed model, print what it declares, and apply the milestone's refuse/ignore lint. Nothing
// is resolved and nothing is built.
object Check:
  // The accrual accumulator: each violation with its focus, newest first. An `Error`, because a
  // contingency accrual must be a `Hazard`; the position-tracked parse (`trackPositions`) is
  // what lets `Tel.Type.assign` give each focus a span.
  private case class Accrued(items: sci.List[(Optional[Tel.Focus], Tel.Error)] = sci.Nil)
    ( using Diagnostics )
  extends Error(m"${items.length} TEL errors"):
    def add(focus: Optional[Tel.Focus], error: Tel.Error): Accrued =
      Accrued((focus, error) :: items)

  // The build file at or above `directory`, found by walking up as `.git` is found, so that the
  // command works from anywhere inside a project.
  def locate(directory: Text): Optional[Path on Local] =
    def recur(dir: Text): Optional[Path on Local] =
      val candidate: Optional[Path on Local] =
        safely(t"$dir/build.tel".as[Path on Local]).let: path =>
          if path.existent() then path else Unset

      candidate.or:
        val cut = dir.s.lastIndexOf('/')
        if cut > 0 then recur(dir.s.substring(0, cut).nn.tt) else Unset

    recur(directory)

  type Outcome = Exit | NoBuildFile.type | CheckFailed.type

  def run(directory: Text)(using Stdio, Environment): Outcome =
    locate(directory).lay[Outcome]:
      Out.println(t"fury: no build.tel at or above $directory")
      NoBuildFile

    . apply(check(_))

  private def check(file: Path on Local)(using Stdio, Environment): Outcome =
    recover:
      case error: Io.Error         => report(error.message)
      case error: Truncation.Error => report(error.message)
      case error: SchemaError      => report(error.message)
      case error: BuildError       => report(error.message)

    . protect:
        val data: Data = file.read[Data]
        val source: Text = file.read[Text]
        val schema: Tels = Schemas.build

        // A parse failure is one error at one position; validation failures accrue below.
        val parsed: Optional[Tel] =
          recover:
            case error: Tel.Error =>
              located(file, source, error.span, error, Unset)
              Unset

          . protect(data.read[Tel])

        parsed.lay[Outcome](CheckFailed): document =>
          violations(document, schema) match
            case sci.Nil =>
              val build = Model.decode(document)
              summarize(file, build)

              val findings = Lint(build).stdlib
              findings.foreach(display)

              if findings.exists(_.error) then CheckFailed else Exit.Ok

            case errors =>
              // A focus arrives with a keyword path and, once located against the position-
              // tracked document, the span of the compound it names; the path is printed too,
              // since a validator's rejection says nothing else about which field it was.
              errors.foreach: (focus, error) =>
                val located0: Optional[Tel.Focus] = focus.let(_.withSpan(document))
                val span: Span = located0.lay(error.span)(_.span)

                val path: Optional[Text] =
                  located0.let: (focus: Tel.Focus) =>
                    focus.pointer.components.stdlib.map(_.s).mkString("/").tt

                located(file, source, span, error, path)

              CheckFailed

  // Every violation of `schema` by `document`, oldest first, under Fury's validators: the
  // accrual boundary collects them where fail-fast would stop at the first.
  def violations(document: Tel, schema: Tels): sci.List[(Optional[Tel.Focus], Tel.Error)] =
    val accrued: Accrued =
      validate[Tel.Focus](Accrued()):
        case error: Tel.Error => accrual.add(prior, error)

      . protect:
          venture(Tel.Type.assign(document, schema, Validators.registry))
          ()

    accrued.items.reverse

  private def display(finding: Lint.Finding)(using Stdio): Unit =
    Out.println(t"${finding.severity}: ${finding.message}")

  // A TEL error as Pyrocosm's diagnostic: the file and the position, the message, the lines of
  // the build file around the span (a parse error's own, or that of the focus accrued with a
  // validation error) highlighted as TEL with the span marked, and, after, the error's number
  // and the keyword path it was found at.
  private def located
    ( file: Path on Local, source: Text, span: Span, error: Tel.Error, path: Optional[Text] )
    ( using Stdio, Environment )
  :   Unit =

    val diagnostic: Block.Diagnostic = Block.Diagnostic.of(error, file.encode, source, span)
    val number: Text = t"E${error.reason.number}"
    val paragraph: Text = path.lay(number): (path: Text) => t"$number at $path"
    val detail: List[Block] = List(Block.paragraph(paragraph))
    val blocks: List[Block] = List(diagnostic.copy(detail = detail))
    TerminalRenderer().blocks(blocks, columns).each: (line: Teletype) => Out.println(line)

  // The width a report is laid out at: the terminal's columns, within reason, or a page's.
  private def columns(using Environment): Int =
    safely(Environment.columns[Text].s.toInt).lay(100)(_.min(160))

  private def report(message: Message)(using Stdio): Exit =
    Out.println(t"fury: $message")
    Exit.Fail(2)

  private def summarize(file: Path on Local, build: Model.Build)(using Stdio): Unit =
    Out.println(t"${file.encode}: valid")

    build.commands.stdlib.foreach: (command: Model.Command) =>
      Out.println(t"  command ${command.name}")

    build.toolchains.stdlib.foreach: (toolchain: Model.Toolchain) =>
      val tools: Text = toolchain.tools.stdlib.map(_.name.s).mkString(", ").tt
      Out.println(t"  toolchain ${toolchain.name}: $tools")

    build.projects.stdlib.foreach: (project: Model.Project) =>
      Out.println(t"  project ${project.id}")

      project.modules.stdlib.foreach: (module: Model.Module) =>
        val sources: Int = module.sources.stdlib.length
        val includes: Text = module.includes.stdlib.map(_.module.s).mkString(", ").tt
        Out.println(t"    module ${module.name}: $sources source globs; includes $includes")
