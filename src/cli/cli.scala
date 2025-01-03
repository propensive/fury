/*
    Fury, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package fury

import ambience.*, systemProperties.virtualMachine, environments.virtualMachine
import anticipation.*
import aviation.*
import contingency.*
import digression.*
import escapade.*
import escritoire.*, tableStyles.minimal, insufficientSpaceHandling.ignore
import ethereal.*, daemonConfig.supportStderr
import eucalyptus.*
import exoskeleton.*, executives.completions, unhandledErrors.stackTrace, parameterInterpretation.posix
import fulminate.*
import galilei.*, filesystemInterfaces.galileiApi, filesystemOptions.dereferenceSymlinks
import gastronomy.*
import gossamer.*
import guillotine.*
import hallucination.*
import hellenism.*, classloaders.threadContext
import hieroglyph.*, charDecoders.utf8, charEncoders.utf8, badEncodingHandlers.strict, textMetrics.uniform
import inimitable.*
import iridescence.*, colors.*
import kaleidoscope.*
import nettlesome.*
import parasite.*, threadModels.platform, orphanDisposal.cancel
import profanity.*
import quantitative.*
import rudiments.*, homeDirectories.virtualMachine
import serpentine.*, pathHierarchies.unixOrWindows
import spectacular.*
import turbulence.*
import vacuous.*

given Realm = realm"fury"
given Decimalizer = Decimalizer(2)

object cli:
  val Version =
    Switch(t"version", false, List('v'), t"Show information about the current version of Fury")

  val Interactive = Switch(t"interactive", false, List('i'), t"Run the command interactively")

  val NoTabCompletions =
    Switch(t"no-tab-completions", false, Nil, t"Do not install tab completions")

  val Artifact = Switch(t"artifact", false, List('a'), t"Produce an artifact")
  val Benchmarks = Switch(t"benchmark", false, List('b'), t"Run with OS settings for benchmarking")

  val Discover =
    Switch(t"discover", false, List('D'), t"Try to discover build details from the directory")

  val Force = Switch(t"force", false, List('F'), t"Overwrite existing files if necessary")
  val All = Switch(t"all", false, List('a'), t"Clean system artifacts as well")
  val ForceRebuild = Switch(t"force", false, List('f'), t"Force the module to be rebuilt")

  def Dir(using Tactic[PathError]) =
    Flag[Path](t"dir", false, List('d'), t"Specify the working directory")

  val Offline = Switch(t"offline", false, List('o'), t"Work offline, if possible")
  val Watch = Switch(t"watch", false, List('w'), t"Watch source directories for changes")
  val Concise = Switch(t"concise", false, List(), t"Produce less output")

  def Generation(using Tactic[NumberError]) =
    Flag[Int](t"generation", false, List('g'), t"Use universe generation number")

  def Stream(using Tactic[InvalidRefError]) =
    Flag[StreamId](t"stream", false, List('s'), t"Which stream to publish to")

  val About          = Subcommand(t"about",     e"About Fury")
  val Build          = Subcommand(t"build",     e"Start a new build (default)")
  val Cache          = Subcommand(t"cache",     e"Cache operations")
  val Config         = Subcommand(t"config",    e"View and change configuration")
  val Shutdown       = Subcommand(t"shutdown",  e"Shutdown the Fury daemon")
  val Init           = Subcommand(t"init",      e"Initialize a new project")
  val Ecosystem      = Subcommand(t"ecosystem", e"Ecosystem operations")
  val Universe       = Subcommand(t"universe",  e"Universe actions")
  val UniverseSearch = Subcommand(t"search",    e"Search for a release")
  val UniverseShow   = Subcommand(t"show",      e"Show details of the current universe")
  val UniverseUpdate = Subcommand(t"update",    e"Check for universe updates")
  val Graph          = Subcommand(t"graph",     e"Show a build graph")
  val Update         = Subcommand(t"update",    e"Update Fury")
  val Publish        = Subcommand(t"publish",   e"Publish a project")
  val Install        = Subcommand(t"install",   e"Install Fury")
  val Clean          = Subcommand(t"clean",     e"Clean the cache")
  val Details        = Subcommand(t"info",      e"Information about cache usage")

given (using Tactic[UserError]): HomeDirectory =
  tend:
    case SystemPropertyError(property) =>
      UserError(m"""
        Could not access the home directory because the $property system property was not set.
      """)
  .within(homeDirectories.virtualMachine)

given (using Cli): WorkingDirectory = workingDirectories.daemonClient

given (using Tactic[UserError]): Installation =
  tend:
    case ConfigError(message) =>
      UserError(m"The configuration file could not be read because $message")
  .within(Installation())

@main
def main(): Unit =
  import cli.*
  val initTime: Optional[Instant] = safely(Instant(Properties.ethereal.startTime[Long]()))

  tend:
    supervise:
      given logFormat: LogFormat[File, Display] = logFormats.standardColor[File]
      import filesystemOptions.{createNonexistent, createNonexistentParents}

      given Log[Display] = logging.silent[Display]
        /*tend:
          Log.route[Display]:
            case _ => installation.config.log.path.as[File]
        .remedy:
          case error: IoError     => abort(InitError(m"An I/O error occurred when trying to create the log"))
          case error: StreamError => abort(InitError(m"A stream error occurred while logging"))
          case error: UserError   => abort(InitError(error.message))*/

      intercept:
        case error: Throwable =>
          Log.fail(m"Detected an async failure in ${trace.inspect}")
          Log.fail(error.inspect)
          Transgression.Absorb

      initTime.let: initTime =>
        Log.info(m"Initialized Fury in ${(now() - initTime).show}")

      cliService:
        attempt[UserError]:
          Log.envelop(Uuid().show.take(8)):
            arguments match
              case Install() :: _ =>
                val interactive = Interactive().present
                val force = Force().present
                val noTabCompletions = NoTabCompletions().present

                execute:
                  frontEnd:
                    if interactive then actions.install.installInteractive(force, noTabCompletions)
                    else actions.install.batch(force, noTabCompletions)

                  Exit.Ok

              case Init() :: _ =>
                execute:
                  frontEnd:
                    val directory = safely(workingDirectory).or:
                      abort(UserError(m"The working directory could not be determined."))
                    actions.build.initialize(directory)

              case Config() :: _ =>
                execute:
                  frontEnd:
                    log(installation.config.inspect)
                    Exit.Ok

              case Clean() :: _ =>
                val all: Boolean = All().present
                execute(frontEnd(actions.clean(all)))

              case Cache() :: subcommands => subcommands match
                case Nil => execute(frontEnd(actions.cache.about()))

                case other :: _ => execute:
                  frontEnd(other.let(actions.invalidSubcommand(_)).or(actions.missingSubcommand()))

              case About() :: _ => execute(about())

              case Build() :: subcommands => subcommands match
                case Nil =>
                  execute:
                    Out.println(t"Module has not been specified")
                    Exit.Fail(1)

                case target :: _ =>
                  val online = Offline().absent
                  val watch = Watch().present
                  val concise = Concise().present
                  val force = ForceRebuild().present

                  safely(internet(false)(Workspace().locals())).let: map =>
                    val targets = map.values.map(_.source).flatMap:
                      case workspace: Workspace => workspace.build.projects.flatMap(_.targets)

                    target.let: target =>
                      if target().contains(t"/")
                      then target.suggest(previous ++ targets.map(_.suggestion))
                      else target.suggest(previous ++ targets.map(_.partialSuggestion))

                  execute:
                    internet(online):
                      frontEnd:
                        val buildTask = task(t"build"):
                          tend:
                            case InvalidRefError(ref, refType) =>
                              UserError(m"The target $ref could not be decoded")

                            case IoError(_) =>
                              UserError(m"An I/O error occurred")

                            case PathError(_, _) =>
                              UserError(m"A path error occurred")

                            case ExecError(_, _, _) =>
                              UserError(m"An execution error occurred")

                          .within:
                            actions.build.run(target().decode[Target], watch, force, concise)

                        daemon:
                          terminal.events.stream.each:
                            case Keypress.Escape | Keypress.Ctrl('C') => summon[FrontEnd].abort()
                            case other => ()

                        tend:
                          case ConcurrencyError(reason) =>
                            UserError(m"The build was aborted")
                        .within(buildTask.await()).also(Out.print(t"\e[?25h"))

              case Universe() :: subcommands =>
                val online = Offline().absent

                subcommands match
                  case Nil | (UniverseShow() :: _) =>
                    execute:
                      internet(online)(frontEnd(actions.universe.show()))

                  case command :: _ => execute:
                    Out.println:
                      e"Command $Italic(${command.vouch(using Unsafe)()}) was not recognized."

                    Exit.Fail(1)

              case Ecosystem() :: subcommands => subcommands match
                case Publish() :: target => target match
                  case Nil =>
                    execute:
                      Out.println(t"Project has not been specified")
                      Exit.Fail(1)

                  case projectId :: _ =>
                    val workspace = safely(Workspace())
                    val online = Offline().absent

                    workspace.let: workspace =>
                      projectId.suggest(workspace.build.projects.map(_.suggestion))

                      workspace.build.projects.where(_.id.show == projectId()).let: project =>
                        safely(Stream.suggest(() => project.streams.map(_.suggestion)))

                    execute:
                      internet(online):
                        tend:
                          case InvalidRefError(ref, refType) =>
                            UserError(m"The target $ref could not be decoded")

                          case IoError(_) =>
                            UserError(m"An I/O error occurred")

                          case PathError(path, reason) =>
                            UserError(m"The path $path is not valid because $reason")

                          case ExecError(_, _, _) =>
                            UserError(m"An execution error occurred")

                        .within(actions.project.publish(projectId().decode[ProjectId], Stream()))


                case _ =>
                  execute:
                    Out.println(t"Unknown command")
                    Exit.Fail(1)

              case Shutdown() :: Nil => execute:
                FrontEnd.shutdown()
                service.shutdown()
                Exit.Ok

              case Nil =>
                execute:
                  tend:
                    case WorkspaceError(_) =>
                      Exit.Fail(1)
                  .within:
                    val workspace = Workspace()
                    Out.println(Workspace().build.actions.prim.inspect)
                    Exit.Ok

              case subcommands =>
                if Version().present then execute(frontEnd(actions.versionInfo())) else
                  val subcommand = subcommands.filter(!_().starts(t"-")).prim
                  val workspace = safely(Workspace())

                  if subcommand.present && subcommand.lay(false)(_().contains(t"/")) then execute:
                    unsafely:
                      Out.println(e"Executing script $Bold(${subcommand.vouch()})...")
                    Exit.Fail(1)
                  else
                    val online = Offline().absent
                    val watch = Watch().present
                    val concise = Concise().present
                    val force = ForceRebuild().present

                    workspace.let: workspace =>
                      subcommand.let(_.suggest(previous ++ workspace.build.actions.map(_.suggestion)))

                    execute:
                      workspace.lay(Exit.Fail(2)): workspace =>
                        subcommand.let: subcommand =>
                          tend:
                            case InvalidRefError(ref, _) =>
                              abort(UserError(m"The reference $ref is not valid"))
                          .within(subcommand().populated.let(ActionName(_)))
                          .or(workspace.build.default).let: action =>
                            workspace.build.actions.where(_.name == action).let: action =>
                              internet(online):
                                val main = async:
                                  frontEnd:
                                    val buildTask = task(t"build"):
                                      tend:
                                        action.modules.each(actions.build.run(_, watch, force, concise))
                                      .remedy:
                                        case PathError(path, reason) =>
                                          abort(UserError(m"The path $path is not valid"))

                                        case ExecError(_, _, _) =>
                                          abort(UserError(m"An execution error occurred"))

                                        case IoError(_) =>
                                          abort(UserError(m"An I/O error occurred"))

                                    daemon:
                                      terminal.events.stream.each:
                                        case Keypress.Escape | Keypress.Ctrl('C') =>
                                          log(m"Aborting the build.")
                                          summon[FrontEnd].abort()

                                        case TerminalInfo.WindowSize(rows, cols) =>
                                          summon[FrontEnd].resize(rows, cols)

                                        case other =>
                                          ()

                                    tend:
                                      buildTask.await().also(Out.print(t"\e[?25h"))
                                      Exit.Ok
                                    .remedy:
                                      case ConcurrencyError(_) =>
                                        Exit.Fail(1)

                                tend(main.await()).remedy:
                                  case ConcurrencyError(_) =>
                                    abort(UserError(m"The task was aborted"))

                        .or:
                          subcommand.let(frontEnd(actions.invalidSubcommand(_))).or:
                            Out.println(t"No subcommand was specified.")
                            Exit.Fail(1)

        .recover: userError =>
          execute:
            Out.println(userError.message)
            Log.fail(userError)
            Exit.Fail(4)

  .remedy:
    // case InitError(message) =>
    //   println(message)
    //   Exit.Fail(2)

    // case error: IoError =>
    //   println(error.message)
    //   Exit.Fail(2)

    case ConcurrencyError(reason) =>
      println(m"There was a concurrency error")
      Exit.Fail(3)

def about()(using stdio: Stdio): Exit =
  safely:
    Out.println:
      stdio.termcap.color match
        case ColorDepth.TrueColor => Image((Classpath / p"logo.png")()).render
        case _                    => Image((Classpath / p"logo2.png")()).render

  val asciiArt =
      t"H4sIAAAAAAAA/31Ryw3AIAi9O8UbtfHcQw8wRrUzMUmTKlSx1HgA3ocXFT6FtulySUIZEIO49gllLcjIA62MmgkY"+
      t"3UOBeu+2VrdCCxfsm2RhAQQOD7aCq5KvtiTQTnDqbZ/gbf0LV8dcqUdzxN+x1CHBfa7mjPlh4HQDGOnRlikCAAA="

  unsafely(asciiArt.decode[Base64]).gunzip.utf8.cut(t"\n").each: line =>
    Out.print(t" "*19)
    Out.println(line)

  val buildId = safely((Classpath / p"build.id")().read[Text].trim)

  val scalaProperties = unsafely:
    val resource = Classpath / p"compiler.properties"

    resource().read[Text].cut(t"\n").flatMap:
      case r"$key([^=]*)=$value(.*)" => List(key -> value)
      case _                         => Nil
    .to(Map)

  case class Software(name: Text, version: Text, copyright: Text)

  val scalaCopyright = scalaProperties(t"copyright.string").sub(t"Copyright ", t"")
  val jvmVersion = unsafely(Properties.java.vm.specification.version())
  val jvmVendor = unsafely(Properties.java.vm.specification.vendor())

  val software =
    List
     (Software(t"Fury", t"1.0${buildId.lay(t"") { id => t" build $id"}}", t"2017-2024, Propensive"),
      Software(t"Scala", scalaProperties(t"version.number"), scalaCopyright),
      unsafely(Software(t"Java distribution", Properties.java.version(), Properties.java.vendor())),
      Software(t"Java specification", jvmVersion, jvmVendor))

  Out.println:
    Table[Software]
     (Column(e"$Bold(Component)", textAlign = TextAlignment.Right): software =>
       e"$Bold(${software.name})",
      Column(e"$Bold(Version)")(_.version.display),
      Column(e"$Bold(Copyright)")(_.copyright.display))
    .tabulate(software).layout(72)

  Out.println()

  safely:
    Out.println:
      e"  ${Italic}(${Properties.os.name()} ${Properties.os.version()}, ${Properties.os.arch()})\n"

  Exit.Ok
