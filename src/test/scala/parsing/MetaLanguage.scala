package parsing
import parsing.Tokens._

class MetaLanguage extends FlatSpecForParsers with MetaLanguageParsers {

  //
  // Section 1.4 : The LISP Meta-language
  //
  
  "The meta language parsers" should "handle function definitions" in {
    implicit val parserToTest = functionDefinition
    
    parsing("third[x]=car[cdr[cdr[x]]]") should equal(Fun("third", List("x"),
        CarOf(CdrOf(CdrOf(Var("x"))))))
  }
  
  they should "handle function evaluation" in {
    implicit val parserToTest = functionDefinition
    
    val fun = parsing("third[x]=car[cdr[cdr[x]]]")
    fun.eval(Map("x" -> parsing("(A B C D)")(sexp))) should equal(Atom("C"))
  }
  
  // "A conditional expression has the form..."
  they should "parse conditional expressions" in {
    implicit val parserToTest = conditionalExpression
    
    parsing("[eq[car[x];A]→cons[B;cdr[X]]; T→x]") should equal(
        Cond(List(
            CondElem(
                EqP(CarOf(Var("x")),Atom("A")), 
                Cons(Atom("B"),CdrOf(Atom("X")))), 
            CondElem(
                Atom("T"), 
                Var("x")))))
  }

  // "Using the lambda notation, we can write..."
  
  // a problem with the parser of function calls in the lambda below, so first test that. 
  they should "parse function calls" in {
    parsing("ff[car[x]]")(functionCall) should equal(FunCall("ff", List(CarOf(Var("x")))))
    parsing("ff[car[x]]")(funOrSexp) should equal(FunCall("ff", List(CarOf(Var("x")))))
    parsing("[atom[x]→x; T→ff[car[x]]]")(conditionalExpression) should equal(
    	Cond(List(
                CondElem(
                    AtomP(Var("x")),
                    Var("x")),
                CondElem(
                    T,
                    FunCall("ff", List(CarOf(Var("x"))))))))
  }

  val lambdaExpression = Lambda(
    List(Var("x")),
    Cond(List(
      CondElem(
        AtomP(Var("x")),
        Var("x")),
      CondElem(
        T,
        FunCall("ff", List(CarOf(Var("x"))))))))

  they should "parse lambda notation" in {
    implicit val parserToTest = lambdaNotation
    val expr = "λ[[x];[atom[x]→x; T→ff[car[x]]]]"

    (parsing(expr)) should equal(lambdaExpression)
  }

  they should "parse label expressions" in {
    implicit val parserToTest = labelNotation
    val expr = "label[ff;λ[[x];[atom[x]→x; T→ff[car[x]]]]]"
    (parsing(expr)) should equal(Label("ff", lambdaExpression))

  }
}