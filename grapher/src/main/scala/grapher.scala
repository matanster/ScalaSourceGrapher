package grapher

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

// bind the annotation macro
class NN extends StaticAnnotation {
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

    def methodDo(expr: c.universe.Tree) = {
      // for each expression of a found method, examines all of the method applications employed by it,
      // to extract its callees.
      expr.foreach { 
        // TODO: need to further hone the quasiquote for capturing only and all cases of method application, *along* the object they are applied to.
        case q"$obj $f" => println(s"  which calls " + BLUE + BOLD + f + RESET + 
                                    " on object " + obj + 
                                    " of type " + CYAN_B + obj.tpe.typeSymbol + RESET)
                                    // if (obj.tpe != null) println(CYAN_B + obj.tpe.typeSymbol + RESET)
        case x => // println(YELLOW + s"in unmatched AST part: $x" + RESET); 
                  // case q"$obj $f($args)" => println(s"  which calls $f with args $args")
      }
    }
    
    // iterate all methods of an object
    def findMethods(typ: String, name: Any, body: List[c.Tree]) = {
      body map {
        case x@q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" => {
          println(CYAN_B + s"$typ $name" + RESET + 
                  " has method " + BLUE + BOLD + 
                  tname + RESET)

          Macros.printff
          //val defMacroWrapped = q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr"
          //println("wrapped: \n" + defMacroWrapped)
          //methodDo(expr)
        }
        case x => // println(YELLOW + s"in unmatched AST part: $x" + RESET);
      }
      
    }
    
    def typeCheck(annottees: c.Expr[Any]*) = {
      // add type information to the annottees
      println(GREEN + "about to typecheck" + RESET)
      val typeCheckedAnnottees = annottees.map(annottee => c.typecheck(annottee.tree, silent = false)).toList 
      println(GREEN_B + "typechecking didn't crash this time!" + RESET)
      typeCheckedAnnottees
    }
    
    //val typeCheckedAnnottees = typeCheck(annottees.head)
   
    //annottees.map(_.tree).toList match {
    
    // iterate the annottees to find and handle all methods    
    annottees.map(_.tree).toList match {
      
      case x@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil =>
        println(s"found object $name")
        findMethods("object", name, body)
        returnIdentity
        
      //case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)        
        
      case x@q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil => 
        println(s"found class $name")
        findMethods("class", name, body)
        returnIdentity
          
      case x@q"import $ref.{..$sels}" => 
        println(s"found import $ref $sels")
        returnIdentity
        
      case x => 
        println(YELLOW + "in unmatched AST part" + RESET); q"..$x"
        returnIdentity
    }
    
    //annottees.head
    //c.Expr[Any](result)
  }
}
