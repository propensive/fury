# Fury

__Fury__ is the [LIRA](https://github.com/propensive/lira) build tool: it reads a build file,
plans a static DAG of steps and runs them through tools, memoized in the content-addressed
store, on this machine or across a swarm of them. It is deliberately language-agnostic:
everything that knows what a `.scala` file is lives in
[Fever](https://github.com/propensive/fever), which Fury reaches only through the `lira.tool`
contract. Its design and roadmap are
[`design/fury.md`](https://github.com/propensive/lira/blob/main/design/fury.md) in the lira
repository.

Fury runs as an [Ethereal](https://soundness.dev/ethereal/) daemon and is a
[Pyrocosm](https://github.com/propensive/pyrocosm) tool, so the subcommands and configuration
files every Pyrocosm tool shares are fury's too.

Fury is being built one step at a time. So far it can check a build file, and one instance can
reach another on a different machine; it cannot yet build.

## Usage

```sh
fury check                    # parse and validate build.tel, and say what this milestone cannot build
fury log                      # what this instance has been doing; --follow keeps showing it until
                              # Ctrl+C, and --log-level fine or warn shows more or less
fury swarm                    # the connections this daemon keeps, and the callers keeping one to it
fury swarm invite             # invite another machine to connect to this one, in one word
fury swarm join <invitation>  # accept an invitation, and stay connected to the machine it is from
fury swarm ping linux-box hi  # say `ping` to a machine and wait for its `pong`
fury swarm connect linux-box  # keep a connection to a declared machine open
fury swarm disconnect linux-box   # stop keeping it
fury swarm listen             # accept other instances, in the background; --port, or -p, says where
fury swarm disconnect         # stop accepting them, and hang up on those connected
fury swarm peers              # the machines this one has admitted by invitation
fury swarm revoke linux-box   # refuse one of them from now on
fury swarm identity           # this machine's fingerprint and addresses, to declare it by hand
fury install                  # install tab-completions and the manpage
fury about                    # fury's version, and the daemon serving it
fury --version                # the version alone
fury quit                     # stop the daemon
```

## Installing

```sh
curl -fsSL https://propensive.dev/fury | sh
```

## Connecting two machines

One instance of Fury can talk to another. For now all they say is `ping` and `pong`, which is
enough to show the link working: `fury log` on each machine shows the message arrive and its
answer return.

On the machine that is to accept connections, make an invitation:

```
linux-box$ fury swarm invite
ЊḀȕlinux-boxḁḍ192Į168Į1Į20…
```

It is one word, to be copied to the other machine, and it holds everything that machine needs:
the addresses this one may be reached at, the port, the fingerprint of the certificate it will
present, and a token which admits one machine, once, within the hour (`--expires 30m`, `2h`,
`1d` to choose). `invite` starts the daemon listening, if it was not.

On the other machine, accept it:

```
laptop$ fury swarm join ЊḀȕlinux-boxḁḍ192Į168Į1Į20…
found linux-box nearby, at linux-box.local, 192.168.1.20
joined linux-box, at linux-box.local, 192.168.1.20
connected to linux-box; `fury swarm disconnect linux-box` closes the connection
```

`join` connects, exchanges the invitation's token for one of the laptop's own, declares
`linux-box` in `~/.config/pyrocosm/machines.tel` (so fume knows it too), and keeps a connection
to it. A second `join` with the same invitation is refused. `fury swarm peers` on `linux-box`
lists the machines it has admitted, and `fury swarm revoke` refuses one from then on.

A machine may have several addresses — on a local network and beyond it — and a connection is
made to the nearest which answers: private addresses and `.local` names first, then other
names, then public addresses, each tried a quarter of a second after the one before, so that
one which does not answer costs no more than that. A laptop which comes home moves back to the
local network the next time it connects.

While an invitation is open, the inviting machine also advertises itself on the local network
(DNS-SD over mDNS, as `_fury._tcp`), and `join` looks for it there for a few seconds before
connecting: a machine found nearby is tried first, by its `.local` name and the addresses it
answers from, ahead of those the invitation lists. Nothing changes if it is not found. `fury
log` on the inviter shows `advertising this machine on the local network`, and on the joiner
`found linux-box nearby`.

Machines can still be declared by hand. `fury swarm listen` makes the daemon listen, and a
`listen` line in `~/.config/fury/config.tel` has it listen whenever it runs:

```
tel 1.0

listen
listen-port 8092        # optional; 8092 is the default
```

`fury swarm identity` there prints the machine's fingerprint, its addresses and where its token
is kept. On the machine that is to connect, copy the token into a file and declare the other
machine, with a `host` line for each address:

```
machine linux-box
  host      192.168.1.20
  host      build.example.org
  identity  sha256:3f9a…
  token     ~/.config/pyrocosm/tokens/linux-box
```

Then, with `fury log --follow` running on both:

```
$ fury swarm ping linux-box hello
pong from linux-box (fury 0.3.0) in 8ms
```

The caller's log shows the connection, `→ ping … to linux-box` and `← pong … from linux-box`;
the listener's shows `← ping … from <caller>` and `→ pong … to <caller>`:

```
19:22:04.046  INFO  → ping 49ddb6ca ‘hello’ to linux-box
19:22:04.056  INFO  ← pong 49ddb6ca from linux-box
```

Each line has the time of day on that machine, the level the event was logged at (`FINE`,
`INFO`, `WARN` or `FAIL`) and what happened. `fury log` shows `INFO` and above; `--log-level`
(or `log-level` in `config.tel`) chooses another level, and `fine` shows everything. The log is
the daemon's, kept in memory (its newest thousand events), and is lost when it stops.

### Staying connected

`fury swarm ping` on its own makes a connection, uses it once and closes it. `fury swarm
connect linux-box` asks the daemon to keep one open instead, in the background, as `join`
does; a `connect linux-box` line in `~/.config/fury/config.tel` has it do so whenever it runs.
Pings to that machine then go over the open connection.

A connection that is kept is checked: each end sends the other a `beat` every second, and an
end which hears nothing for three seconds takes the connection for lost, says so in its log as
a warning, and closes it. The end which made the connection then makes it again, after a pause
of a second which doubles with each failure in a row, to half a minute at most.

```
07:22:01.671  INFO  keeping the connection with linux-box open, with a beat each second
07:22:19.402  WARN  lost the connection with linux-box: nothing heard for 3004ms
07:22:19.403  INFO  trying linux-box again in 1s
```

When a connection becomes a lasting one, each end tells the other what it is — its name,
operating system, architecture and cores — and each beat carries the sender's load. `fury
swarm` lists the connections the daemon is keeping, the callers keeping one to it, and what is
known of each:

```
linux-box  connected; Linux amd64, 16 cores; load 0.42; last heard from at 15:31:10.545

connected to this daemon:
  laptop  Mac OS X aarch64, 12 cores; load 2.95; last heard from at 15:31:10.545
```

`fury swarm disconnect linux-box` lets a connection go. The beats themselves are logged at
`FINE`. A machine named by a `connect` line must be declared in the user's own configuration
(or the shared `machines.tel`), since the daemon has no project of its own.

### The protocol

What two instances say to each other is the Fury protocol, and its schema is
[`wire.schema.tel`](../wire.schema.tel): one TEL schema, in which the messages — `ping`, `pong`,
`beat` and `advert`, so far — are the cases of a single coproduct. Each message is sent as a BinTEL
document under that schema.

Neither end assumes what the other can read. When a connection is made, each end first sends
its *acceptance* (BinTEL §8.4): the forms of the protocol it can read. From then on each message
is written to the acceptance the other end sent, and a message the other end does not accept is
not sent, with a warning in the log. Two instances which can agree nothing say so, and hang up:

```
15:24:40.085  INFO  exchanged acceptances with linux-box: each can read what the other sends
```

Today an acceptance names one form, the protocol as that build knows it, so two instances agree
when their messages are the same, and a build which changes the messages must also go on
accepting the form it had before, if it is to talk to builds which have not changed.

### The transport

The connection is TLS to the listener's self-signed certificate, which the caller pins by the
fingerprint it declares (the SSH known-hosts model), and the caller proves itself with a token:
the one it was granted when it joined, or the listener's own; documents travel in a
length-prefixed framing. This is
[Pyrocosm](https://github.com/propensive/pyrocosm)'s transport, which fume uses too. Things to
know:

- The listener accepts connections on every network interface.
- A `token` that does not name an existing file is taken to be the token itself, so a mistyped
  path fails as a wrong token.
- A machine has one daemon per user, so a machine pinging itself shows both ends in one log.

## The schemas

The TEL schemas a build is written against are at the root of this repository, and ship inside
fury: `build.schema.tel`, `tool.schema.tel`, `guarantees.schema.tel`, `local.schema.tel`,
`lock.schema.tel` and `registry.schema.tel`, with `scalac.tool.tel`, the descriptor of the
built-in Scala tool. `wire.schema.tel` is the schema of the protocol two instances speak. `build.tel` and `local.tel` are specimens: the tests validate and decode
them, and `build.tel` deliberately exercises more than fury can yet build.

## Building

The libraries fury builds against (Soundness, Pyrocosm and lira) are pinned in
[`etc/refs`](../etc/refs), and the tools it runs in [`etc/tools`](../etc/tools).

```sh
make sync-deps   # install the pinned releases into ~/.ivy2/local
make tools       # install fume and flair
make test        # run the suite with fume  (make test-plain uses plain java)
make fury        # build the native executable for this machine
make install     # copy it to ~/.local/bin
make check       # check the sources with flair
```

A release is cut by a signed tag on a commit CI has passed; the tag is the only place the
version is declared:

```sh
git tag -s X.Y.Z && git push --tags
```
