package irk

import rudiments.*
import gossamer.*
import jovian.*
import escapade.*

import rendering.ansi

import dotty.tools.dotc.*, reporting.*

import scala.collection.mutable as scm


object Compiler:
  class CustomReporter() extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
    var errors: scm.ListBuffer[Diagnostic] = scm.ListBuffer()
    def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit =
      errors += diagnostic

  private var Scala3 = new dotty.tools.dotc.Compiler()

  def compile(id: Text, files: List[File], inputs: Set[DiskPath], out: Directory, script: File)
             (using Stdout)
             : List[irk.Message] =
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
          
          val run = Scala3.newRun(using ctx2)
          run.compile(sources)
          finish(Scala3, run)(using ctx2)
          reporter.errors.to(List)

      driver.run(files, classpathText).flatMap:
        diagnostic =>
          if diagnostic.level == 2 && diagnostic.position.isPresent then
            val pos = diagnostic.position.get.nn
            val line = pos.line
            val file = pos.source.nn.name.nn
            val content = pos.source.nn.content.nn
            List(irk.Message(id, file.show, line, pos.startColumn, pos.endColumn, pos.endLine, diagnostic.message.show, content.unsafeImmutable))
          else Nil

    catch case err: Throwable =>
      Out.println(StackTrace(err).ansi)
      Compiler.synchronized:
        Scala3 = new dotty.tools.dotc.Compiler()
      List(irk.Message(id, t"", 0, 0, 0, 1, t"The compiler crashed", IArray()))