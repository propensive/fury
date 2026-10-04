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

// What one Fury says to another over a Pyrocosm `Channel`, once Pyrocosm's handshake has
// welcomed the connection: the Fury protocol (fury.md §8), of which this is the first rung. The
// messages are a proof of the link and nothing more — a `ping` carrying a note, and the `pong`
// that answers it — so that both ends can be seen to have spoken.
//
//   caller → listener   ping   an id, when it was sent, and a note to show at the other end
//   listener → caller   pong   the same id, when it arrived, and who answered
//
// Only this enum's layout must agree between two Furies: its schema's fingerprint is the
// protocol the handshake names, and a peer with a different one is refused before any message is
// decoded.
object Wire:
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
  val port: Int = 8092

enum Wire:
  case Ping(id: Text, sent: Long, note: Text)
  case Pong(id: Text, received: Long, hostname: Text)
