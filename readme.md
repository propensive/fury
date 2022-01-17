![Vex logo](doc/logo.svg)

# Vex

__Vex__ is a simplistic and minimalistic build tool, intended primarily for bootstrapping other projects. It is written in Scala
3 using [Niveau](https://github.com/propensive/niveau/). Its features are deliberately limited, but it can nevertheless build a
variety of Scala projects.

Vex is available as a single 30MB download including everything necessary to compile and run Scala code.

## Building Vex

Vex may either be built from scratch with just the Scala 3 compiler, or may be built using a previous release build of Vex.
Continuous integration builds Vex using just the Scala compiler, then checks that Vex can build itself.

### Building Vex with Vex

To build Vex with an earlier build of Vex, run the launcher script, `vex`. The first time this is run, it will download a release
version of `Vex` and replace itself. It will also run as a daemon so that subsequent runs of `vex` are instantaneous:
```sh
./vex
```

### Building with Scala

Vex currently requires an unreleased version of the Scala 3 compiler. The Dockerfile in the root directory will build both Scala 3
and Vex. This can be invoked with `make`.

To build Vex, run,
```sh
make distribution
```
which will generate the files `dist/vex-0.2.0` and `dist/launcher-0.2.0`.

