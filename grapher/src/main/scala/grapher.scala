/*
 * This code compiles first (courtesy of the sbt definitions...)
 * 
 * In this phase, macro annotations embed a regular macro 
 * in place of every annotated method call of the project being built.
 * 
 * This phase also outputs all detected class and object definitions. 
 */

package grapher

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import play.api.libs.json._  
import play.api.libs.json.Json._
import fileUtil.util._

object init {} // object that may host stuff that needs to run once, if referenced in the annotation macro object

// bind the annotation macro
class AN extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any = macro analyze.impl
}

// the annotation macro definition
object analyze {
  
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Console._
    
    if (annottees.length != 1) { 
      // I have yet to have seen this happen or know what kind of code-under-analysis
      // would provide multiple annottees here.
      print(YELLOW)
      println("annottees length: " + annottees.length)
      println("annottes: " + annottees)
      print(RESET)
    }
    
    val returnIdentity = c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant(())))) // return expression that duplicates the original annottees (from https://github.com/scalamacros/paradise/blob/5e5f0c129dd1861f86250d7ce94635b89996938c/tests/src/main/scala/identity.scala#L8)

    // augment input AST with method wrappers
    def wrapMethods(body: List[c.Tree], modelType: model.ModelType): List[c.Tree] = {
       
      val wrapped: List[c.Tree] = body map {
        case x@q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" => {
          //println(CYAN_B + s"$typ $name" + RESET + " has method " + BLUE + BOLD + tname + RESET)
          
          val method = new model.Method(tname.toString)
          modelType.methods = modelType.methods :+ method
         
          //
          // replace the method with a macro that gets the method's AST as its argument.
          // why? this macro will analyze its code during its expansion, having type information automatically available to it.
          // after the macro analyzes the code, it will re-expand the method's AST so that it runs as intended.
          //

          import grapher.Macros._
          q"$mods def $tname[..$tparams](...$paramss): $tpt = macroWrapper(${modelType.name}, ${modelType.typeType}, ${tname.toString}, $expr)"
        }
        case x => x
      }
      
      val modelTypeJson = Json.toJson(Map("name" -> toJson(modelType.name),
                                          "type" -> toJson(modelType.typeType),
                                          "methods" -> toJson(modelType.methods map { method => toJson(method.name) })
          )          
      )
      
      println("Type: " + Json.prettyPrint(modelTypeJson))
      writeJsonFile(modelTypeJson, modelType.name + "-methods")
     
      wrapped
    }
    
    // iterate the annottees to find and handle all methods    
    annottees.map(_.tree).toList match {
      
      case x@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil =>
        val modelType = new model.ModelType(name = name.toString(), typeType = "object")
        //println(s"found object ${modelType.name}")
        val wrappedMethods = wrapMethods(body, modelType)
        c.Expr[Any](q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$wrappedMethods }")
        
      case x@q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil =>
        val modelType = new model.ModelType(name = name.toString(), typeType = "class")
        //println(s"found object ${modelType.name}")
        //println(s"found class $name")
        val wrappedMethods = wrapMethods(body, modelType)
        c.Expr[Any](q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$wrappedMethods }")
          
      case x@q"import $ref.{..$sels}" => 
        println(s"found import $ref $sels")
        returnIdentity
        
      case x => 
        println(YELLOW + "in unmatched AST part" + RESET); q"..$x"
        returnIdentity
    }
  }
}
