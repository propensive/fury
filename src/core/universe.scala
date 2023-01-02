package fury

import rudiments.*
import galilei.*
import gossamer.*
import serpentine.*
import parasitism.*, threading.virtual
import turbulence.*
import cellulose.*

object Universe:
  def apply(name: Text)(using Environment, Stdout): Universe throws IoError | EnvError | GitError | AggregateError | CodlReadError | IncompatibleTypeError | InvalidPathError =
    val url = name match
      case t"hell" => t"https://github.com/propensive/hell"
      case _ => throw AppError(t"Unknown universe: $name")
    
    val dir = GitCache.universe(name, url)
    
    Universe:
      dir.files.filter(_.name.ends(t".fury")).collect: file =>
        file.name.drop(5, Rtl).as[Int]
      .sift[Int].sorted.foldLeft(Set[ProjectRoot]()): (acc, next) =>
        val content = unsafely((dir / t"$next.fury").file(Expect).read[Text]())
        acc ++ Codl.read[BuildConfig](content).project.map(ProjectRoot(_, dir))
  
  def resolve(pwd: Directory[Unix])(using Environment, Stdout, Monitor)
             : (List[Target], Universe) throws InvalidPathError | IncompatibleTypeError | CodlReadError |
                 AggregateError | GitError | EnvError | IoError | RootParentError | DuplicateIndexError |
                 CancelError =

    def dirs(pwd: Directory[Unix], overlay: List[Overlay], forks: List[Fork]): List[Directory[Unix]] =
      val forkSet = forks.indexBy(_.id)

      overlay.map: ov =>
        if !forkSet.contains(ov.project.project) then GitCache(ov.url, ov.commit)
        else (pwd.path + forkSet(ov.project.project).path).directory(Expect)

    def recur(universe: Maybe[(List[Target], Universe)], todo: List[Directory[Unix]], seen: Set[Directory[Unix]]): (List[Target], Universe) =
      todo match
        case Nil =>
          universe.or(throw AppError(t"Could not resolve the build"))
          
        case pwd :: tail =>
          if seen.contains(pwd) then recur(universe, tail, seen) else
            val buildFile = (pwd / p"fury").file()
            val forksFile = (pwd / p".forks").file()

            safely(buildFile.mm(_.read[Text]())).mm(Codl.read[BuildConfig](_)).mm: config =>
              val universe2 = universe.or(config.command -> Universe(config.universe.or(throw AppError(t"The universe has not been specified"))))
              val forks: List[Fork] =
                try safely(forksFile.mm(_.read[Text]())).mm(Codl.read[Forks](_).fork).or(Nil)
                catch case err: Exception => unsafely(throw new Exception(s"${summon[Codec[Forks]].schema}"))
              val projects2 = config.project.map(ProjectRoot(_, pwd)).to(Set)
              recur(universe2(0) -> (universe2(1) ++ Universe(projects2)), dirs(pwd, config.overlay, forks) ::: todo, seen + pwd)
            .or(recur(universe, tail, seen))
          
    recur(Unset, List(pwd), Set())

case class Universe(projects: Set[ProjectRoot]):
  lazy val index: Map[ProjectId, ProjectRoot] =
    try projects.indexBy(_.id) catch case err: DuplicateIndexError => throw AppError(t"", err)
  
  //lazy val packageIndex: Map[Text, ProjectRoot] = projects.flatMap { p => p.provide.map(_ -> p) }.to(Map)

  @targetName("addAll")
  def ++(next: Universe): Universe = Universe((index -- next.projects.map(_.id)).values.to(Set) ++ next.projects)

  @targetName("add")
  def +(project: ProjectRoot): Universe = this ++ Universe(Set(project))

case class Release(id: ProjectId, stream: Text, description: Text, tag: List[Text], license: Text, date: Text,
                       life: Int, repo: GitRepo, provide: List[Text])

case class ProjectRoot(project: Project, root: Directory[Unix]):
  export project.*

  def resolve(repoPath: RepoPath)(using Environment, Monitor, Threading, Stdout) 
             : Directory[Unix] throws GitError | EnvError | IoError | CancelError | RootParentError =
  repoPath.repoId.fm(root.path + repoPath.path): repoId =>
    val repo = project.repo.find(_.id.id == repoId).getOrElse(throw AppError(t"The repo ${repoId} was not defined in ${project.toString}"))
    GitCache(repo.url, repo.commit).path + repoPath.path
  .directory(Expect)
