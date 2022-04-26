package irk

import rudiments.*
import gossamer.*
import jovian.*
import escapade.*

import rendering.ansi

import dotty.tools.dotc.*, reporting.*

import scala.collection.mutable as scm
import scala.concurrent.*

object Compiler:
  class CustomReporter() extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
    var errors: scm.ListBuffer[Diagnostic] = scm.ListBuffer()
    def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit =
      errors += diagnostic

  private var Scala3 = new dotty.tools.dotc.Compiler()

  def compile(id: Text, files: List[File], inputs: Set[DiskPath], out: Directory, script: File,
                  cancel: Promise[Unit])
             (using Stdout)
             : Result =
    import unsafeExceptions.canThrowAny
    import dotty.tools.*, io.{File as _, *}, repl.*, dotc.core.*

    val reporter = CustomReporter()
    
    try
      val separator: Text = try Sys.path.separator() catch case err: KeyNotFoundError => t":"
      
      val classpath: List[Text] = inputs.map:
        path => if path.isDirectory then t"${path.fullname}/" else path.fullname
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
          setup(Array[String]("-d", out.fullname.s, "-deprecation", "-feature", "-Wunused:all",
              "-new-syntax", "-Yrequire-targetName", "-Ysafe-init", "-Yexplicit-nulls",
              "-Ycheck-all-patmat", ""), ctx).map(_(1)).get
        
        def run(files: List[File], classpath: Text): List[Diagnostic] =
          val ctx = currentCtx.fresh
          val ctx2 = ctx
            .setReporter(reporter)
            .setCompilerCallback(callbackApi)
            .setSetting(ctx.settings.language, List("experimental.fewerBraces", "experimental.saferExceptions", "experimental.erasedDefinitions"))
            .setSetting(ctx.settings.classpath, classpath.s)
          
          val sources = files.to(List).map:
            file => PlainFile(Path(file.fullname.s))
          

          val run: Run = Scala3.newRun(using ctx2)
          cancel.future.andThen:
            _ => run.isCancelled = true
          run.compile(sources)
          finish(Scala3, run)(using ctx2)
          reporter.errors.to(List)

      driver.run(files, classpathText).foldLeft(Result.Complete(Nil)):
        (prev, diagnostic) =>
          if diagnostic.position.isPresent then
            val pos = diagnostic.position.get.nn
            val line = pos.line
            val file = pos.source.nn.name.nn
            val content = pos.source.nn.content.nn
            val level = diagnostic.level match
              case 0 => Level.Info
              case 1 => Level.Warn
              case 2 => Level.Error
              case _ => Level.Info
            
            prev + Result.Complete(List(Issue(level, id, file.show, line, pos.startColumn, pos.endColumn,
                pos.endLine, diagnostic.message.show, content.unsafeImmutable)))
          else prev + Result.Complete(Nil)

    catch case err: Throwable =>
      if !cancel.isCompleted then
        Out.println(StackTrace(err).ansi)
        Compiler.synchronized:
          Scala3 = new dotty.tools.dotc.Compiler()
        Result.Terminal(ansi"The compiler crashed")
      else Result.Aborted