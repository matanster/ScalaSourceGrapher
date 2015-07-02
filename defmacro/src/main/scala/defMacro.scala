/*
 * this macro definition takes care of 
 * outputing all method calls made by every method of a class.
 * it gets embedded by the static annotation. 
 */

package grapher

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import play.api.libs.json._  
import play.api.libs.json.Json._
//import fileUtil.util._

object Macros {
  
  def recordAndExecuteMethod(c: whitebox.Context)(expr: c.universe.Tree, typeName: c.Expr[Any]) = {
    // for each expression of a found method, examines all of the method applications employed by it,
    // to extract its callees.
    import c.universe._
    import Console._
    
    val typeNameAsString = typeName.tree.toString.drop(1).dropRight(1)
    
    expr.foreach { 
      // TODO: need to further hone the quasiquote for capturing only and all cases of method application, *along* the object they are applied to.
      case x@q"$obj $f" => 
        
        // val ownedMethod = new model.OwnedMethod(obj.tpe.typeSymbol, f)
        // modelType.applies = modelType.applies :+ method
        
      
        //writeJsonFile(modelTypeJson, modelType.name)
        println(typeNameAsString + " calls " + BLUE + BOLD + f + RESET + " on object " + obj + " of type " + CYAN_B + obj.tpe.typeSymbol + RESET)
                                  
                           //val method = new model.Method(tname.toString)       
      case x => 
    }
  }
  
  def impl(c: whitebox.Context)(typeName: c.Expr[Any], annottees: c.universe.Tree*): c.universe.Tree = {
    import c.universe._
    recordAndExecuteMethod(c)(annottees.head, typeName)
    annottees.head // return the AST unchanged. TODO: this transition from varargs to a single tree still baffles me  
  }

  def macroWrapper(typeName: Any, annottees: Any*): Any = macro impl
}