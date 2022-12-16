[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/fury/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/fury/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)

![Fury logo](doc/logo.svg)

# Fury

__Fury__ is a simplistic and minimalistic build tool, intended primarily for bootstrapping other projects. It is written in Scala
3 using [Scala One](https://github.com/propensive/one/) libraries. Its features are deliberately limited, but it can nevertheless
build a variety of Scala projects.

Fury is available as a single 30MB download including everything necessary to compile and run Scala code.

## Building Fury

Fury may either be built from scratch with just the Scala 3 compiler, or may be built using a previous release build of Fury.
Continuous integration builds Fury using just the Scala compiler, then checks that Fury can build itself.

### Building Fury with Fury

To build Fury with an earlier build of Fury, run the launcher script, `fury`. The first time this is run, it will download a release
version of Fury and replace itself. It will also run as a daemon so that subsequent runs of `fury` are instantaneous:
```sh
./fury
```

### Building with Scala

Fury currently requires an unreleased version of the Scala 3 compiler. The Dockerfile in the root directory will build both Scala 3
and Fury. This can be invoked with `make`.

To build Fury, run,
```sh
make distribution
```
which will generate the files `dist/fury-0.2.0` and `dist/launcher-0.2.0`.

