package grapher

import scala.language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._
    c.Expr[Unit](q"""println("Hello World")""")
  }

  def printff: Unit = macro impl
}