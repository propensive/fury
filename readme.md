[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/fury/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/fury/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Fury

__A build tool for Scala__

TBC

## Features

TBC


## Availability

Fury has not yet been published as a binary.

## Getting Started


A Fury build is a specification of the components involved in building a
software product and the relationships between them. _Building_ may describe
the construction of a binary artifact from source code, or may describe other
related tasks, like testing, debugging and deployment, that rely on much of the
same information.

Builds are data, and do not contain code. When we ask Fury to perform a task,
it uses the information in the build to dedece what steps should be run, and in
what order, to complete it. Usually, this will involve running compilers and
maybe running code which has been compiled in earlier steps; but the build
itself is static and immutable.

Builds are living entities which evolve with a project as it develops and as
the entire ecosystem of software, notably dependencies and compilers, change.
The build tool, Fury, will evolve too. To stay sane when _everything_ is
changing around us, Fury builds are designed to produce the same output for
each task, whether running it today or in ten years' time on a newer release of
Fury. This is true, so long as the code that runs in each step is
deterministic.

But to accommodate an ever-evolving ecosystem, Fury makes it easy to update a build to use newer dependencies

Builds are composable, and 


## Status

Fury is classified as __embryonic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Fury is designed to be _small_. Its entire source code currently consists
of 2686 lines of code.

## Building

Fury can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Fury are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/fury/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Fury easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Fury was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

TBC

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo



## License

Fury is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
