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
fury check         # parse and validate build.tel, and report what this milestone cannot build
fury ping linux-box hello   # say `ping` to a configured machine and wait for its `pong`
fury connect linux-box      # keep a connection to a machine open, with a beat each second
fury connect                # the connections this daemon is keeping, and how each stands
fury disconnect linux-box   # stop keeping it
fury log           # what this instance has been doing; --follow keeps showing it until Ctrl+C,
                   # --log-level warn shows only warnings and failures, and --log-level fine
                   # shows the beats as well
fury listen        # start accepting other instances, in the background; --port, or -p, says where
fury listen stop   # stop accepting them
fury identity      # this machine's certificate fingerprint, for another machine's configuration
fury install       # install tab-completions and the manpage
fury about         # fury's version, and the daemon serving it
fury --version     # the version alone
fury quit          # stop the daemon
```

## Installing

```sh
curl -fsSL https://propensive.dev/fury | sh
```

## Connecting two machines

One instance of Fury can talk to another. For now all they say is `ping` and `pong`, which is
enough to show the link working: `fury log` on each machine shows the message arrive and its
answer return.

On the machine that is to accept connections, run `fury listen`: its daemon then listens in
the background, on port 8092 unless `--port` (or `-p`) says otherwise, until `fury listen stop`
or the daemon's end. To have the daemon listen whenever it runs, say so in
`~/.config/fury/config.tel` instead:

```
tel 1.0

listen
listen-port 8092        # optional; 8092 is the default
```

`fury identity` there prints the machine's certificate fingerprint and where its token is
kept. On the machine that is to connect, copy the token into a file and declare the other
machine in `~/.config/fury/config.tel` (or, for every Pyrocosm tool at once, in
`~/.config/pyrocosm/machines.tel`):

```
machine linux-box
  host      build.example.org
  identity  sha256:3f9a…
  token     ~/.config/pyrocosm/tokens/linux-box
```

Then, with `fury log --follow` running on both:

```
$ fury ping linux-box hello
pong from linux-box (fury 0.1.0) in 8ms
```

The caller's log shows the connection, `→ ping … to linux-box` and `← pong … from linux-box`;
the listener's shows `← ping … from <caller>` and `→ pong … to <caller>`:

```
19:22:04.046  INFO  → ping 49ddb6ca ‘hello’ to linux-box
19:22:04.056  INFO  ← pong 49ddb6ca from linux-box
```

Each line has the time of day on that machine, the level the event was logged at (`FINE`,
`INFO`, `WARN` or `FAIL`) and what happened. `fury log` shows `INFO` and above; `--log-level`
(or `log-level` in `config.tel`) chooses another level, and `fine` shows everything. The log is the daemon's, kept in memory (its newest
thousand events), and is lost when it stops.

### Staying connected

`fury ping` on its own makes a connection, uses it once and closes it. `fury connect linux-box`
asks the daemon to keep one open instead, in the background, and a `connect linux-box` line in
`~/.config/fury/config.tel` has it do so whenever it runs. Pings to that machine then go over
the open connection.

A connection that is kept is checked: each end sends the other a `beat` every second, and an
end which hears nothing for three seconds takes the connection for lost, says so in its log as
a warning, and closes it. The end which made the connection then makes it again, after a pause
of a second which doubles with each failure in a row, to half a minute at most.

```
07:22:01.671  INFO  keeping the connection with linux-box open, with a beat each second
07:22:19.402  WARN  lost the connection with linux-box: nothing heard for 3004ms
07:22:19.403  INFO  trying linux-box again in 1s
```

`fury connect` with no machine lists the connections the daemon is keeping and when each was
last heard from; `fury disconnect linux-box` lets one go. The beats themselves are logged at
`FINE`. A machine named by a `connect` line must be declared in the user's own configuration
(or the shared `machines.tel`), since the daemon has no project of its own.

The connection is TLS to the listener's self-signed certificate, which the caller pins by the
fingerprint it declares (the SSH known-hosts model), and the caller proves itself with the
shared token; messages are BinTEL in a length-prefixed framing. This is
[Pyrocosm](https://github.com/propensive/pyrocosm)'s transport, which fume uses too. Things to
know:

- The listener accepts connections on every network interface.
- Two instances must be the same build of the protocol: a difference in the messages is refused
  in the handshake.
- A `token` that does not name an existing file is taken to be the token itself, so a mistyped
  path fails as a wrong token.
- A machine has one daemon per user, so a machine pinging itself shows both ends in one log.

## The schemas

The TEL schemas a build is written against are at the root of this repository, and ship inside
fury: `build.schema.tel`, `tool.schema.tel`, `guarantees.schema.tel`, `local.schema.tel`,
`lock.schema.tel` and `registry.schema.tel`, with `scalac.tool.tel`, the descriptor of the
built-in Scala tool. `build.tel` and `local.tel` are specimens: the tests validate and decode
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

A release is cut by a signed tag, after bumping `furyVersion` in `build.mill` and merging it:

```sh
git tag -s X.Y.Z && git push --tags
```
