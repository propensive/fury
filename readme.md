[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/irk/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/irk/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)

![Irk logo](doc/logo.svg)

# Irk

__Irk__ is a simplistic and minimalistic build tool, intended primarily for bootstrapping other projects. It is written in Scala
3 using [Scala One](https://github.com/propensive/one/) libraries. Its features are deliberately limited, but it can nevertheless
build a variety of Scala projects.

Irk is available as a single 30MB download including everything necessary to compile and run Scala code.

## Building Irk

Irk may either be built from scratch with just the Scala 3 compiler, or may be built using a previous release build of Irk.
Continuous integration builds Irk using just the Scala compiler, then checks that Irk can build itself.

### Building Irk with Irk

To build Irk with an earlier build of Irk, run the launcher script, `irk`. The first time this is run, it will download a release
version of Irk and replace itself. It will also run as a daemon so that subsequent runs of `irk` are instantaneous:
```sh
./irk
```

### Building with Scala

Irk currently requires an unreleased version of the Scala 3 compiler. The Dockerfile in the root directory will build both Scala 3
and Irk. This can be invoked with `make`.

To build Irk, run,
```sh
make distribution
```
which will generate the files `dist/irk-0.2.0` and `dist/launcher-0.2.0`.

