package fury

import rudiments.*
import parasitism.*
import gossamer.*
import galilei.*
import serpentine.*
import escapade.*
import eucalyptus.*
import ambience.*
import turbulence.*, lineSeparation.jvm
import deviation.*

import rendering.ansi

import dotty.tools.dotc.*, reporting.*, interfaces as dtdi, util as dtdu

import scala.collection.mutable as scm

import language.adhocExtensions

object Compiler:
  private var Scala3 = new dotty.tools.dotc.Compiler()
  
  class CustomReporter() extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
    val errors: scm.ListBuffer[Diagnostic] = scm.ListBuffer()
    def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit = errors += diagnostic

  def compile
      (id: Ref, pwd: Directory, files: List[File], inputs: List[DiskPath], out: Directory, script: File,
          plugins: List[PluginRef], cancel: Promise[Unit], owners: Map[DiskPath, Step], js: Boolean)
      (using Stdio, Monitor, Environment, Log)
      : Result =
    import unsafeExceptions.canThrowAny
    import dotty.tools.*, io.{File as _, *}, repl.*, dotc.core.*

    val reporter = CustomReporter()
    
    try
      val separator = env.pathSeparator.show
      
      val classpath: List[Text] = inputs.map: path =>
        if path.isDirectory then t"${path.fullname}/" else path.fullname
      .to(List) :+ Fury.furyJar(script).fullname
      
      val classpathText = classpath.reverse.join(separator)
      
      val callbackApi = new interfaces.CompilerCallback:
        override def onClassGenerated
            (source: interfaces.SourceFile, generatedClass: interfaces.AbstractFile, className: String): Unit =
          ()
  
        override def onSourceCompiled(source: interfaces.SourceFile): Unit = ()

      object driver extends dotc.Driver:
        val currentCtx =
          val ctx = initCtx.fresh
          val pluginParams = plugins.map(_.jarFile.fullname).map(t"-Xplugin:"+_).ss.to(Array)
          val jsParams = if js then Array[String]("-scalajs") else Array[String]()

          setup(pluginParams ++ jsParams ++ Array[String]("-d", out.fullname.s, "-deprecation", "-feature", "-Wunused:all",
              "-new-syntax", "-Yrequire-targetName", "-Ysafe-init", "-Yexplicit-nulls", "-Xmax-inlines", "64",
              "-Ycheck-all-patmat", "-classpath", classpathText.s, ""), ctx).map(_(1)).get
        
        def run(files: List[File], classpath: Text): List[Diagnostic] =
          val ctx = currentCtx.fresh
          val featureList = List(t"fewerBraces", t"saferExceptions", t"erasedDefinitions", t"namedTypeArguments")
          val features = featureList.map(t"experimental."+_)
          
          val ctx2 = ctx
            .setReporter(reporter)
            .setCompilerCallback(callbackApi)
            .setSetting(ctx.settings.language, features.ss)
            .setSetting(ctx.settings.classpath, classpath.s)
          
          val sources = files.to(List).map: file =>
            PlainFile(Path(file.fullname.s))
          
          val run: Run = Scala3.newRun(using ctx2)

          val cancelator: Task[Unit] = Task(t"cancelator"):
            cancel.await()
            run.isCancelled = true
          
          run.compile(sources)
          finish(Scala3, run)(using ctx2)
          cancelator.cancel()
          reporter.errors.to(List)

      def codeRange(pos: dtdi.SourcePosition): Maybe[CodeRange] =
        val content = pos.source.nn.content.nn.immutable(using Unsafe)
        val file = pos.source.nn.path.nn.show
        
        safely(Unix.parse(file)).mm: p =>
          val roots = owners.filter(_(0).precedes(p))
          val (step: Maybe[Step], path: Maybe[Relative]) =
            if roots.isEmpty then Unset -> Unset else
              val (root: DiskPath, step: Step) = roots.maxBy(_(0).parts.size)
              step -> p.relativeTo(step.pwd.path)
          
          CodeRange(step.mm(_.id), path, pos.line, pos.startColumn, pos.endColumn, pos.endLine, content)

      def getRanges(pos: dtdu.SourcePosition | Null, acc: List[CodeRange] = Nil): List[CodeRange] =
        if pos == dtdu.NoSourcePosition || pos == null then acc else
          val cr = codeRange(pos)
          // FIXME
          if cr.unset then Io.println(t"Could not get code range for ${pos.toString}")
          getRanges(pos.outer, cr.fm(acc)(_ :: acc))

      driver.run(files, classpathText).foldLeft(Result.Complete(Set())): (prev, diagnostic) =>
        if diagnostic.position.isPresent then
          val level = Level.fromOrdinal(diagnostic.level)
          
          val stack = diagnostic.position.get.nn match
            case pos: dtdu.SourcePosition => getRanges(pos)
            case pos: dtdi.SourcePosition => codeRange(pos).option.to(List)
          
          stack.headOption.fold(prev): head =>
            prev + Result.Complete(Set(Issue(level, pwd.path, head, stack.tail, diagnostic.message.show)))
        
        else prev + Result.Complete()

    catch case err: Throwable =>
      if !cancel.ready then
        Io.println(StackTrace(err).ansi)
        
        Compiler.synchronized:
          Scala3 = new dotty.tools.dotc.Compiler()
        
        Result.Terminal(ansi"The compiler crashed")
      else Result.Aborted

enum Level:
  case Info, Warn, Error
