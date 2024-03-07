[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/fury/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/fury/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Fury

__Develop software in anger__

TBC

## Features

TBC


## Availability Plan

Fury has not yet been published. The medium-term plan is to build Fury
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Fury.

Subsequently, Fury will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started


A Fury build is a specification of the components involved in building a
software product, and the relationships between them. _Building_ can describe
the construction of a binary artifact from source code, or may describe other
related tasks, like testing, debugging and deployment, that rely on much of the
same information.

Builds are data, and do not contain code. When we ask Fury to perform a task,
it uses the information in the build to deduce what steps should be run, and in
what order, to complete it. Each step typically involves running a compiler or
running code which has been compiled in earlier steps. The build defines what
code will be executed, but the build itself is static and immutable data, and
its structure is not dynamic.

Builds are living entities which evolve with a project as it develops and as
the entire ecosystem of software, notably dependencies and compilers, change.
The build tool, Fury, will evolve too. To stay sane when _everything_ is
changing around us, Fury builds are designed to produce the same output for
each step, whether running it today or in ten years' time on a newer release of
Fury. This is true, so long as the code that runs in each step is
deterministic.

## Build file format

However, to accommodate an ever-evolving ecosystem

Builds are composable, and 

## Escaping Dependency Hell

Fury takes on the valiant endeavour of defeating Dependency Hell, the situation
that many projects fall into when their transitive dependencies cannot coexist
coherently in the same project.

This occurs when one project has at least two dependencies which depend on
different versions of the same project. A coherent set of dependencies can only
contain one version of the same project, so one of the two primary dependencies
must be changed. If those two versions are compatible, resolving the conflict
may be trivial, or may be problematic, leading to new conflicts or requiring
code changes to accommodate a new version.

The colloquialism of "Dependency Hell" accurately reflects the user's
experience of dealing with a series of dependency conflicts, though it also
manifests itself passively as Dependency Stagnation, where a working set of
dependencies never changes for fear that it would lead to Dependency Hell.

These scenarios are never desirable, and developers on projects with many
dependencies can waste hours, days, or longer on build maintenance just to keep
up to date with an evolving ecosystem, while the project is making no material
progress.

Dependency Hell arises for several complex reasons, but it is fundamentally a
problem of scale, exacerbated by the network effect and a long feedback loop.
Of these reasons, it is the network effect which transforms it into a
worse-than-exponential problem.

Fury's approach to overcoming Dependency Hell is to introduce new ideas which
attack these problems in several different ways:
 - to reduce the number of concurrent candidate versions for each dependency
 - to make it possible for certain conflicting versions of some dependencies to
   coexist as transitive dependencies
 - to obviate dependencies which do not need to be transitive
 - to make it easier to resolve conflicts when they occur
 - to clearly communicate expectations about dependencies

Alone, any one of these ideas would probably be insufficient to overcome
Dependency Hell, but together they can make an impact on the likelihood of a
conflict occurring at any node in a dependency tree, and together they can
overcome the exponential growth which can cause a dependency conflict to spread
throughout the tree.

The new ideas which form this concerted attack on Dependency Hell are,
 - [scope-qualified dependencies](qualified-dependencies.md)
 - [Vent](vent.md), a resource-scarce publishing ecosystem
 - [source dependencies and composable builds](sources.md)
each of which is described on its own page.

## Extensibility

Fury is designed primarily as a build tool for JVM languages, and has special
support for Scala. Build tools need to coordinate a variety of different, and
complex third-party tools, and developers are often eager to have a build tool
support a feature or capability they have written.

This is usually achieved through a plugin architecture, and many build tools
have vibrant ecosystems of plugins which enhance those builds with new
capabilities.

But a plugin-based architecture imposes two closely-related compromises on a
build tool.

The design of the plugin interface becomes extremely critical to the user
experience: An overly simplistic design limits the ability for the build tool
to provide tight, coherent and safe integration between different features. But
a complex design will surely be vulnerable to frequent change, but the codebase
which depends on that interface is distributed across multiple repositories
maintained by many different developers, and the architecture becomes
impossible to change without breaking existing plugins.

The choice is therefore between badly-integrated functionality and a brittle
build tool. Fury chooses neither.

Firstly, many build tasks that would need to be designed as plugins in other
build tools, could be implemented as "ordinary" steps in the build.

Instead, Fury strives to be a "batteries-included" build tool, which
accommodates additional features and capabilities through integration into the
Fury codebase. Most importantly, this makes it possible to extend Fury without
any API constraints, and subjects all enhancements to the same long-term
maintenance that Fury receives.

This is the same decision that was made by the Linux Kernel, which includes
thousands of third-party contributions, and has been largely successful.

But it introduces a new question of which features should be included and which
should not. For now, that decision is a judgement call on how useful a proposed
enhancement would be to the majority of Fury users for its inclusion to be
justified.

## Compilers

A compiler is an external tool which reads input files and produces consistent
output files, or reports errors with the inputs. Usually these inputs are
sourcecode files and the outputs are binary files which are executable.

Although each compiler follows this paradigm, each has a unique set of options
to control it. Fury currently provides five compilers, and will provide more in
the future.

- Scala
- Kotlin
- Java
- Amok
- Docker

Scala, Kotlin and Java are all JVM languages whose compilers take sources as
`.scala`, `.java` and `.kt` files, and produce `.class` files. Scala can also
produce JavaScript files by means of ScalaJS.

Amok is a documentation tool which can generate documentation for Java, Kotlin
and Scala, reading markdown files and producing HTML files.

Docker is also considered a "compiler" in a Fury build, since a dockerfile and
corresponding directory are considered its inputs, and one or more files from
the container (after its execution) can be selected as its outputs.

Docker is a particularly interesting compiler since it enables almost any
program to be included in a build. This is especially useful to mitigate any
gaps in Fury's native capabilities: if Fury does not (yet) have native support
for a particular build action, then it should always be possible to perform
that step in a Docker container as a step in the build.

## Dependencies






## Status

Fury is classified as __embryonic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Fury is designed to be _small_. Its entire source code currently consists
of 1289 lines of code.

## Building

Fury will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Fury?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Fury's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Fury and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `fury`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Fury's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Fury are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/fury/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Fury
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Fury was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

TBC

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo



## License

Fury is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

