import grapher.NN

@NN case class TestCaseClass(foo: String)

@NN class B {
  def doB = { 
    println("method doB of class B has runnnn")
    3 
  }
  println("class B has runnnn")
  doB
}

class B1 {
  def doB = { 3 }
}
