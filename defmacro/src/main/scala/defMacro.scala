package grapher

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object Macros {
  
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*) = {
    import c.universe._
    c.Expr[Unit](q"""println("def macro has been expanded")""")
  }

  def defMacro(annottees: Any*): Any = macro impl
}