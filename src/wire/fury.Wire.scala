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

import pyrocosm.Channel
import stratiform.TelSchematic

object Wire:
  // An instant and a hostname are written as the scalars they always were — the milliseconds
  // since the Unix epoch as a whole number, the name as text — so that typing these fields
  // properly changes neither the schema, nor its fingerprint, nor a byte on the wire.
  given instantSchematic: (Instant over Unix) is TelSchematic over Tels.Type =
    () => Tels.Scalar(Array.empty)

  given hostnameSchematic: Hostname is TelSchematic over Tels.Type =
    () => Tels.Scalar(Array.empty)

  given instantEncodable: (Instant over Unix) is Tel.Encodable =
    Tel.Encodable(() => Morphology.Whole, Tel.Nature.Scalar): instant =>
      Tel.scalar(instant.long.show)

  given instantDecodable: Tactic[Tel.Error] => (Instant over Unix) is Tel.Decodable =
    Tel.Decodable(() => Morphology.Whole, Tel.Nature.Scalar): tel =>
      Instant.of[Unix](summon[Long is Tel.Decodable].decoded(tel))

  // Derived once: the schema, its fingerprint (the protocol the handshake names) and the codec.
  // The derivation is why this enum is ALONE in a module compiled without capture checking, as
  // fume keeps its `Relay`: stratiform's derived codecs expand to instances the capture checker
  // sees as fresh where a pure instance is required.
  lazy val codec: Channel.Codec[Wire] =
    import Channel.derivation.throwing
    val schema: Tels = Tels.tels[Wire](t"fury-wire")

    Channel.Codec
      ( t"fury-wire", schema, message => Channel.encode(message, schema),
        data => Channel.decode[Wire](data) )

  // The port a Fury listens on by default.
  val port: Tcp.Port = Port.unsafe[Tcp](8092)

// What one Fury says to another over a Pyrocosm `Channel`, once Pyrocosm's handshake has
// welcomed the connection: the Fury protocol (fury.md §8), of which this is the first rung. The
// messages are a proof of the link and little more — a `ping` carrying a note, the `pong` that
// answers it, and the `beat` by which each end of a lasting connection tells the other, once a
// second, that it is still there.
//
//   either → other   ping   an id, when it was sent, and a note to show at the other end
//   other → either   pong   the same id, when it arrived, and who answered
//   each → other     beat   when it was sent; silence in its place is how a loss is noticed
//
// Only this enum's layout must agree between two Furies: its schema's fingerprint is the
// protocol the handshake names, and a peer with a different one is refused before any message is
// decoded.
enum Wire:
  case Ping(id: Text, sent: Instant over Unix, note: Text)
  case Pong(id: Text, received: Instant over Unix, hostname: Hostname)
  case Beat(sent: Instant over Unix)
