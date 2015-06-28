/*
 * this macro definition takes care of 
 * outputing all method calls made by every method of a class.
 * it gets embedded by the static annotation. 
 */

package grapher

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object Macros {
  
   def methodDo(c: whitebox.Context)(expr: c.universe.Tree) = {
      import c.universe._
      import Console._
      // for each expression of a found method, examines all of the method applications employed by it,
      // to extract its callees.
      expr.foreach { 
        // TODO: need to further hone the quasiquote for capturing only and all cases of method application, *along* the object they are applied to.
        case x@q"$obj $f" => println(s"  which calls " + BLUE + BOLD + f + RESET + 
                                    " on object " + obj + 
                                    " of type " + CYAN_B + obj.tpe.typeSymbol + RESET)
        case x => 
      }
    }
  
  def impl(c: whitebox.Context)(annottees: c.universe.Tree*): c.universe.Tree = {
    import c.universe._
    methodDo(c)(annottees.head) // TODO: this transition from varargs to a single tree still baffles me
    annottees.head
  }

  def defMacro(annottees: Any*): Any = macro impl
}