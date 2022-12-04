package irk

import cellulose.*
import gossamer.*
import escapade.*, rendering.ansi
import iridescence.*
import rudiments.*

object Render:
  def showErrors(err: AggregateError, lines: IArray[Text], filename: Text)(using Stdout): Unit =
    err.errors.sortBy(_.line).foreach:
      case CodlError(line, col, length, issue) =>
        val content = lines(line)
  
        val margin = (line + 2).show.length
  
        Out.println(ansi"${Bg(colors.SeaGreen)}( $Bold(${colors.Black}(Build error)) )${Bg(colors.SpringGreen)}(${colors.SeaGreen}()${colors.Black}( ${filename}:${line + 1}:${col + 1} ))${Bg(colors.Black)}(${colors.SpringGreen}())")
        
        lines.lift(line - 1).foreach: content =>
          Out.println(ansi"${colors.Gray}(${line.show.pad(margin, Rtl)} ║)${Bg(Srgb(0.0, 0.01, 0.01))}(${content}${t" "*(80 - content.length)})")
        
        Out.println(ansi"${colors.Gray}(${(line + 1).show.pad(margin, Rtl)} ║)${Bg(Srgb(0.0, 0.01, 0.01))}(${content.take(col)}${colors.OrangeRed}(${Underline}(${content.drop(col).take(length)}))${content.drop(col + length)}${t" "*(80 - content.length)})")
        
        lines.lift(line + 1).foreach: content =>
          Out.println(ansi"${colors.Gray}(${line + 2} ║)${Bg(Srgb(0.0, 0.01, 0.01))}(${content}${t" "*(80 - content.length)})")
        
        Out.println(ansi"${t" "*((line + 2).show.length + 2)}${issue}")
        
        Out.println(t"")