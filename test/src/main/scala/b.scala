/*
 * This is a source file that is annotated
 * to extract call graph information from it
 * 
 * The following two imports are required for the annotations to ultimately work  
 */

import grapher.AN
import grapher.Macros._

@AN case class TestCaseClass(foo: String)

@AN class B {
  def doB = { 
    println("method doB of class B has runnnn")
    3 
  }
  println("class B has runnnn")
  doB
  
  @AN class C {}
}

@AN 
class B1 {
  def doB = { 3 }
}
