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
                                    // if (obj.tpe != null) println(CYAN_B + obj.tpe.typeSymbol + RESET)
        case x => 
                  // println(YELLOW + s"in unmatched AST part: $x" + RESET); 
                  // case q"$obj $f($args)" => println(s"  which calls $f with args $args")
      }
    }
  
  def impl(c: whitebox.Context)(annottees: c.universe.Tree*): c.universe.Tree = {
    import c.universe._
    methodDo(c)(annottees.head) // TODO: this transition from varargs to single tree is probably not a good idea
    //println(annottees.head)
    annottees.head
    //c.Expr[Unit](q"""println("def macro has been expanded")""")
  }

  def defMacro(annottees: Any*): Any = macro impl
}