package parsing
import parsing.Tokens._

class MetaLanguage extends FlatSpecForParsers with MetaLanguageParsers {

  //
  // Section 1.4 : The LISP Meta-language
  //
  
  "The meta language parser" should "handle function definitions" in {
    implicit val parserToTest = functionDefinition
    
    parsing("third[x]=car[cdr[cdr[x]]]") should equal(Fun(Var("third"), List(Var("x")),
        CarOf(CdrOf(CdrOf(Var("x"))))))
  }
  
  they should "handle function evaluation" in {
    implicit val parserToTest = functionDefinition
    
    val fun = parsing("third[x]=car[cdr[cdr[x]]]")
    
    fun.callWith(List(Binding("x", parsing("(A B C D)")(sexp)))) should equal(Atom("C"))
  }
}