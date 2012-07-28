package interpreter

import org.scalatest.FlatSpec

class LispInterpreterTest extends FlatSpec {

  "The Lisp 1.5 interpreter" should "handle simple definitions" in {
    val lispInterpreter = new LispInterpreter()
    lispInterpreter.interpret("DEFINE ((FOO (LAMBDA (X) (CAR X))))").interpret("FOO (X Y Z)")
  }
}
