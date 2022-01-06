![Vex logo](doc/logo.svg)

# Vex

__Vex__ is a simplistic and minimalistic build tool, intended primarily for bootstrapping other projects. It is written in Scala 3 using
[Niveau](https://github.com/propensive/niveau/). Its features are deliberately limited.

## Building Vex

To build Vex from source, simply run `make`. This will create the `vex` binary in the root directory which can then be added to the `PATH`. Currently, Vex expects `scala-cli` to be on the path.

The `make` process will carry out several steps:
1. downloading a local copy of `sbt`
2. downloading a development branch of Scala 3
3. building and locally publishing a snapshot of Scala 3 with `sbt`
4. downloading `scala-cli`
5. building `vex` using `scala-cli` and the snapshot of Scala 3

It is hoped that future releases will be able to omit steps 1-3, once a suitable release version of Scala is available.
