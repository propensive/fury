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

import codepages.utf8Codepage
import errorDiagnostics.emptyDiagnostics
import pyrocosm.Resources

// A schema Fury ships could not be read: a defect of the build, never of a user's file. It is
// ↯/f.33.12: one which is absent is its first variant, and one which is malformed carries the
// code of the TEL error it failed with, in place of a variant: `↯/f.33.12/s.118.1/t.1.7`.
object SchemaError:
  object Reason:
    given communicable: Reason is Communicable =
      case Absent(name)           => m"$name is not among the shipped schemas"
      case Malformed(name, error) => m"$name is malformed: ${error.message}"

  enum Reason(val number: Int):
    case Absent(name: Text)                      extends Reason(1)
    case Malformed(name: Text, error: Tel.Error) extends Reason(2)

case class SchemaError(reason: SchemaError.Reason)(using Diagnostics)
extends Error(m"schema: $reason"):
  override def bolt: fulminate.Bolt = reason match
    case SchemaError.Reason.Malformed(_, error) =>
      FuryBolts(FuryBolts.core, 12).carrying(error.bolt)

    case reason =>
      FuryBolts(FuryBolts.core, 12, reason.number)

// The TEL schemas Fury validates against (fury.md §14): copied by the build from the repository
// root into the `schemas/` resource directory, and reconstructed into stratiform's `Tels` — the
// document's shape by `Reconstructor`, then the schema-validity battery by `Validation` — on
// demand. Read through the thread-context classloader, never the system one, which under Burdock
// sees only the slim pre-repackage jar (fume's `Suites` carries the same note).
object Schemas:
  private def resource(name: Text): Optional[Text] = Resources.text(name)

  // A shipped schema as it was written.
  def source(name: Text): Optional[Text] = resource(t"schemas/$name")

  def load(name: Text): Tels raises SchemaError =
    val text: Text =
      resource(t"schemas/$name").or(abort(SchemaError(SchemaError.Reason.Absent(name))))

    mitigate:
      case error: Tel.Error => SchemaError(SchemaError.Reason.Malformed(name, error))

    . protect:
        Tels.Validation.validate(text.read[Tel].as[Tels])

  def build: Tels raises SchemaError = load(t"build.schema.tel")
