
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

While the builds 

But to accommodate an ever-evolving ecosystem, Fury makes it easy to update a build to use newer dependencies

Builds are composable, and 

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


