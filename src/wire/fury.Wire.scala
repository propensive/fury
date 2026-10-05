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
import pyrocosm.Channel
import stratiform.TelSchematic

object Wire:
  // An instant, a hostname and an identifier are each written as a scalar — the milliseconds
  // since the Unix epoch as a whole number, the name and the identifier as text — which is what
  // the schema (`wire.schema.tel`) says of them.
  given instantSchematic: (Instant over Unix) is TelSchematic over Tels.Type =
    () => Tels.Scalar(Array.empty)

  given hostnameSchematic: Hostname is TelSchematic over Tels.Type =
    () => Tels.Scalar(Array.empty)

  given uuidSchematic: Uuid is TelSchematic over Tels.Type =
    () => Tels.Scalar(Array.empty)

  given instantEncodable: (Instant over Unix) is Tel.Encodable =
    Tel.Encodable(() => Morphology.Whole, Tel.Nature.Scalar): instant =>
      Tel.scalar(instant.long.show)

  given instantDecodable: Tactic[Tel.Error] => (Instant over Unix) is Tel.Decodable =
    Tel.Decodable(() => Morphology.Whole, Tel.Nature.Scalar): tel =>
      Instant.of[Unix](summon[Long is Tel.Decodable].decoded(tel))

  // Everything below is derived from the `Wire` enum, which is why it is ALONE in a module
  // compiled without capture checking, as fume keeps its `Relay`: stratiform's derivations
  // expand to instances the capture checker sees as fresh where a pure instance is required.
  // A failure inside any of them is an exception here, and an absent result to a caller.
  import Channel.derivation.throwing

  // The protocol's schema, derived from the enum; `wire.schema.tel` is the same schema, written
  // out, and `signature` is what the two must agree on.
  lazy val schema: Tels = Tels.tels[Wire](t"wire")

  // The hash of a schema's base, by which a schema is known: this protocol's, and that of a
  // schema read from its TEL source.
  lazy val signature: Data = SchemaSignature.componentHashes(schema, Tels.Axiom.tels)(0)

  def signature(source: Text): Optional[Data] =
    try SchemaSignature.componentHashes(source.read[Tel], Tels.Axiom.tels)(0)
    catch case _: Exception => Unset

  // What this build can read: an acceptance (BinTEL §8.4) of the protocol's one form.
  private lazy val accepting: Tel.Acceptance.Typed[Tuple1[Wire]] = Tel.Acceptance[Tuple1[Wire]]()

  def acceptance: Tel.Acceptance = accepting.acceptance

  // The acceptance as it is sent, a framed BinTEL document, at the start of every connection.
  lazy val offer: Data = acceptance.framed

  // The acceptance the other end sent, if that is what `data` is.
  def offered(data: Data): Optional[Tel.Acceptance] =
    try Tel.Acceptance(data) catch case _: Exception => Unset

  // A message as the other end has said it can read it: a framed BinTEL document under the
  // first form of the protocol its acceptance names that this build can serve. Absent if it
  // names none.
  def write(message: Wire, theirs: Tel.Acceptance): Optional[Data] =
    try message.fulfil(theirs).let(_.document) catch case _: Exception => Unset

  // A message read from a document written to this build's acceptance. Absent if the document
  // is of no form this build accepts.
  def read(data: Data): Optional[Wire] =
    try accepting.read(data) catch case _: Exception => Unset

  // What Pyrocosm's channel carries for Fury: documents, opaquely. Pyrocosm's handshake compares
  // the fingerprint of a channel's schema and refuses any difference; Fury's channel is given
  // the acceptance schema, which never differs, so that it is the acceptances exchanged over
  // the channel, and not that comparison, which decide what two Furies can say to each other.
  lazy val codec: Channel.Codec[Data] =
    Channel.Codec(t"fury", Tels.Axiom.acceptance, data => data, data => data)

  // The port a Fury listens on by default.
  val port: Tcp.Port = Port.unsafe[Tcp](8092)

// What one Fury says to another: the Fury protocol (fury.md §8), of which these are the first
// messages. They are a proof of the link and little more — a `ping` carrying a note, the `pong`
// that answers it, and the `beat` by which each end of a lasting connection tells the other, once
// a second, that it is still there.
//
//   either → other   ping   an id, when it was sent, and a note to show at the other end
//   other → either   pong   the same id, when it arrived, and who answered
//   each → other     beat   when it was sent; silence in its place is how a loss is noticed
//
// The messages are a coproduct — one `select` in the schema, `wire.schema.tel` — and each is
// sent as a BinTEL document. Neither end assumes what the other can read: each begins by
// sending its acceptance, and writes every message to the acceptance it was sent.
enum Wire:
  case Ping(id: Uuid, sent: Instant over Unix, note: Text)
  case Pong(id: Uuid, received: Instant over Unix, hostname: Hostname)
  case Beat(sent: Instant over Unix)
