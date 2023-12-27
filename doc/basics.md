
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
