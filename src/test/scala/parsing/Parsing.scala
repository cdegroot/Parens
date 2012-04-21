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
  "The elementary function parsers" should "parse cons" in {
    implicit val parserToTest = cons
    
    parsing("cons[A;B]") should equal(Sexp(Atom("A"), Atom("B")))
    parsing("cons[(A . B);C]") should equal(Sexp(Sexp(Atom("A"),Atom("B")),Atom("C")))
    parsing("cons[cons[A;B];C]") should equal(Sexp(Sexp(Atom("A"),Atom("B")),Atom("C")))
  }
}