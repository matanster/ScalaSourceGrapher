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
import fileUtil.util._

object Macros {
  
  def recordAndExecuteMethod(c: whitebox.Context)(expr: c.universe.Tree, modelTypeName: c.Expr[Any], modelTypeSingleton: c.Expr[Any], methodName: c.Expr[Any]) = {
    // for each expression of a found method, examines all of the method applications employed by it,
    // to extract its callees.
    import c.universe._
    import Console._
    
    def ExprToString(expr: c.Expr[Any]) = expr.tree.toString.drop(1).dropRight(1) 
    
    val method = new model.Method(ExprToString(methodName))
    val typeNameAsString = ExprToString(modelTypeName)
    val typeSingletonAsString = ExprToString(modelTypeSingleton)

    // type of owner of this method
    val selfOwnerType = new model.ModelType(name = typeNameAsString.toString(), typeType = typeSingletonAsString)
    
    expr.foreach { 
      // TODO: need to further hone the quasiquote for capturing only and all cases of method application, *along* the object they are applied to.
      case x@q"$obj $f" => 

        val calleeOwner  = new model.ModelType(name = obj.tpe.typeSymbol.name.toString, 
                                               typeType = obj.tpe.typeSymbol.toString().split(" ").head) 

        val calleeMethod = new model.Method(f.toString)
        
        val callee = new model.OwnedMethod(calleeOwner, calleeMethod)
        
        selfOwnerType.applies = selfOwnerType.applies :+ callee
      
        //println(typeNameAsString + " calls " + BLUE + BOLD + f + RESET + " on object " + obj + " of type " + CYAN_B + obj.tpe.typeSymbol + RESET)
       
      case x => 
    }
    
    val selfOwnerTypeJson = Json.toJson(Map("name"   -> toJson(selfOwnerType.name),
                                            "type"   -> toJson(selfOwnerType.typeType),
                                            "method" -> toJson(ExprToString(methodName)),
                                            "applies" -> toJson(selfOwnerType.applies map { 
                                               ownedMethod => Json.obj("method"    -> toJson(ownedMethod.method.name), 
                                                                       "ownerName" -> toJson(ownedMethod.methodTypeOwner.name),
                                                                       "ownerType" -> toJson(ownedMethod.methodTypeOwner.typeType)
                                                                      )})))
    
    println("Type applies: " + Json.prettyPrint(selfOwnerTypeJson))
    writeJsonFile(selfOwnerTypeJson, selfOwnerType.name + "-applies")
    
  }
  
  def impl(c: whitebox.Context)(modelTypeName: c.Expr[Any], modelTypeSingleton: c.Expr[Any], methodName: c.Expr[Any], annottees: c.universe.Tree*): c.universe.Tree = {
    import c.universe._
    recordAndExecuteMethod(c)(annottees.head, modelTypeName, modelTypeSingleton, methodName)
    annottees.head // return the AST unchanged. TODO: this transition from varargs to a single tree still baffles me  
  }

  def macroWrapper(modelTypeName: Any, modelTypeSingleton: Any, methodName: Any, annottees: Any*): Any = macro impl
}