package irk

import rudiments.*
import gossamer.*
import joviality.*
import escapade.*
import serpentine.*

import rendering.ansi

import dotty.tools.dotc.*, reporting.*, interfaces as dtdi, util as dtdu

import scala.collection.mutable as scm
import scala.concurrent.*

object Compiler:
  class CustomReporter() extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
    var errors: scm.ListBuffer[Diagnostic] = scm.ListBuffer()
    def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit =
      errors += diagnostic

  private var Scala3 = new dotty.tools.dotc.Compiler()

  def compile(id: Ref, pwd: Directory[Unix], files: List[File[Unix]], inputs: Set[DiskPath[Unix]],
                  out: Directory[Unix], script: File[Unix], plugins: List[PluginRef], cancel: Promise[Unit],
                  owners: Map[DiskPath[Unix], Step])
             (using Stdout)
             : Result =
    import unsafeExceptions.canThrowAny
    import dotty.tools.*, io.{File as _, *}, repl.*, dotc.core.*

    val reporter = CustomReporter()
    
    try
      val separator: Text = try Sys.path.separator() catch case err: KeyNotFoundError => t":"
      
      val classpath: List[Text] = inputs.map: path =>
        if path.isDirectory then t"${path.fullname}/" else path.fullname
      .to(List) :+ Irk.irkJar(script).fullname
      
      val classpathText = classpath.reverse.join(separator)
      
      val callbackApi = new interfaces.CompilerCallback:
        override def onClassGenerated(source: interfaces.SourceFile,
                                          generatedClass: interfaces.AbstractFile,
                                          className: String): Unit = ()
  
        override def onSourceCompiled(source: interfaces.SourceFile): Unit = ()

      object driver extends dotc.Driver:
        val currentCtx =
          val ctx = initCtx.fresh
          val pluginParams = plugins.map(_.jarFile.show).map(t"-Xplugin:"+_).ss.to(Array)

          setup(pluginParams ++ Array[String]("-d", out.fullname.s, "-deprecation", "-feature", "-Wunused:all",
              "-new-syntax", "-Yrequire-targetName", "-Ysafe-init", "-Yexplicit-nulls", "-Xmax-inlines", "64",
              "-Ycheck-all-patmat", "-classpath", classpathText.s, ""), ctx).map(_(1)).get
        
        def run(files: List[File[Unix]], classpath: Text): List[Diagnostic] =
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
          
          cancel.future.andThen: _ =>
            run.isCancelled = true
          
          run.compile(sources)
          finish(Scala3, run)(using ctx2)
          reporter.errors.to(List)

      def codeRange(pos: dtdi.SourcePosition): CodeRange =
        val file = pos.source.nn.path.nn.show
        val absPath = Unix.parse(file)
        val (root: DiskPath[Unix], step: Step) = owners.filter(_(0).precedes(absPath)).maxBy(_(0).parts.length)
        val path = safely(absPath.relativeTo(root)).otherwise(Relative.parse(file))
        val content = pos.source.nn.content.nn.immutable(using Unsafe)
        CodeRange(step.id, path, pos.line, pos.startColumn, pos.endColumn, pos.endLine, content)

      def getRanges(pos: dtdu.SourcePosition | Null, acc: List[CodeRange] = Nil): List[CodeRange] =
        if pos == dtdu.NoSourcePosition || pos == null then acc else getRanges(pos.outer, codeRange(pos) :: acc)

      driver.run(files, classpathText).foldLeft(Result.Complete(Nil)): (prev, diagnostic) =>
        if diagnostic.position.isPresent then
          val level = Level.fromOrdinal(diagnostic.level)
          
          val stack = diagnostic.position.get.nn match
            case pos: dtdu.SourcePosition => getRanges(pos)
            case pos: dtdi.SourcePosition => List(codeRange(pos))
          
          prev + Result.Complete(List(Issue(level, pwd.path, stack.head, stack.tail, diagnostic.message.show)))
        
        else prev + Result.Complete(Nil)

    catch case err: Throwable =>
      if !cancel.isCompleted then
        Out.println(StackTrace(err).ansi)
        
        Compiler.synchronized:
          Scala3 = new dotty.tools.dotc.Compiler()
        
        Result.Terminal(ansi"The compiler crashed")
      else Result.Aborted

enum Level:
  case Info, Warn, Error