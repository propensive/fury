package fury

import anticipation.*
import perforate.*

import scala.quoted.*

extension (inline context: StringContext)
  inline def lid(): LicenseId = ${Macros.parse('context)}

object Macros:
  def parse(context: Expr[StringContext])(using Quotes): Expr[LicenseId] =
    val string = context.valueOrAbort.parts(0)
    
    failCompilation:
      LicenseId(string.tt)
    
    '{${Expr(string)}.asInstanceOf[LicenseId]}
    