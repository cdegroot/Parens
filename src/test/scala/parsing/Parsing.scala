package parsing
import parsing.SymbolicExpressionAst._

class ParsingSymbolicExpressions extends FlatSpecForParsers with SymbolicExpressionParsers {

  //
  // Section 1.1 : Symbolic Expressions
  //
  
  // "The most elimentary definition of S-expression is the atomic symbol"
  "The Sexp parsers" should "parse an atom" in {
    implicit val parserToTest = atom

    parsing("A") should equal(Atom("A"))
    parsing("APPLE") should equal(Atom("APPLE"))
    parsing("EXTRALONGSTRINGOFLETTERS") should equal(Atom("EXTRALONGSTRINGOFLETTERS"))
    parsing("A4B66XYZ2") should equal(Atom("A4B66XYZ2"))
  }
  
  // "All S-expressions are built out of atomic symbols and the punctuation marks "(", ")", and ".".
  they should "parse an sexp" in {
    implicit val parserToTest = sexp
    
    parsing("ATOM") should equal(Atom("ATOM"))
    parsing("(A . B)") should equal(Sexp(Atom("A"), Atom("B")))
    parsing("(A . (B . C))") should equal(Sexp(Atom("A"), Sexp(Atom("B"), Atom("C"))))
    parsing("((A1 . A2) . B)") should equal(Sexp(Sexp(Atom("A1"), Atom("A2")), Atom("B")))
    parsing("((U . V) . (X . Y))") should equal(Sexp(Sexp(Atom("U"), Atom("V")), Sexp(Atom("X"), Atom("Y"))))
    parsing("((U . V) . (X . (Y . Z)))") should 
    	equal(Sexp(
    			Sexp(Atom("U"), Atom("V")), 
    			Sexp(Atom("X"), Sexp(Atom("Y"), Atom("Z"))))) 
  }
  
}

class ElementaryFunctions extends FlatSpecForParsers with ElementaryFunctionParsers {
  
  // 
  // Section 1.2 : Elementary Functions
  //
  
  // "The first function that we shall introduce is the function cons..."
  "The elementary function parsers" should "parse cons" in {
    implicit val parserToTest = cons
    
    parsing("cons[A;B]") should equal(Sexp(Atom("A"), Atom("B")))
    parsing("cons[(A . B);C]") should equal(Sexp(Sexp(Atom("A"),Atom("B")),Atom("C")))
    parsing("cons[cons[A;B];C]") should equal(Sexp(Sexp(Atom("A"),Atom("B")),Atom("C")))
  }
  
  // "The function car has one argument..."
  they should "parse car" in {
    implicit val parserToTest = car
    
    parsing("car[(A . B)]") should equal(Atom("A"))
    parsing("car[(A . (B1 . B2))]") should equal(Atom("A"))
    parsing("car[((A1 . A2) . B)]") should equal(Sexp(Atom("A1"), Atom("A2")))
    evaluating { parsing("car[A]") } should produce [Exception]
  }
  
  // "The function cdr has one argument..."
  they should "parse cdr" in {
    implicit val parserToTest = funOrSexp // should be cdr but examples in book use other funs as well
    
    parsing("cdr[(A . B)]") should equal(Atom("B"))
    parsing("cdr[(A . (B1 . B2))]") should equal(Sexp(Atom("B1"), Atom("B2")))
    parsing("cdr[((A1 . A2) . B)]") should equal(Atom("B"))
    evaluating { parsing("cdr[A]") } should produce [Exception]
    parsing("car[cdr[(A . (B1 . B2))]]") should equal(Atom("B1"))
    evaluating { parsing("car[cdr[(A . B)]]") } should produce [Exception]
    parsing("car[cons[A;B]]") should equal(Atom("A"))
  }
}