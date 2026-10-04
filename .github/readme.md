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

Fury is being built one step at a time. So far it can check a build file; it cannot yet build.

## Usage

```sh
fury check         # parse and validate build.tel, and report what this milestone cannot build
fury install       # install tab-completions and the manpage
fury about         # fury's version, and the daemon serving it
fury --version     # the version alone
fury quit          # stop the daemon
```

## Installing

```sh
curl -fsSL https://propensive.dev/fury | sh
```

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
