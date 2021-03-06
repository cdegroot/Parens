package parsing
import parsing.Tokens._

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
  def none = EmptyContext
  
  // 
  // Section 1.2 : Elementary Functions
  //
  
  // "The first function that we shall introduce is the function cons..."
  "The elementary function parsers" should "parse cons" in {
    implicit val parserToTest = cons
    
    parsing("cons[A;B]").eval(none) should equal(Sexp(Atom("A"), Atom("B")))
    parsing("cons[(A . B);C]").eval(none) should equal(Sexp(Sexp(Atom("A"),Atom("B")),Atom("C")))
    parsing("cons[cons[A;B];C]").eval(none) should equal(Sexp(Sexp(Atom("A"),Atom("B")),Atom("C")))
  }
  
  // "The function car has one argument..."
  they should "parse car" in {
    implicit val parserToTest = car
    
    parsing("car[(A . B)]").eval(none) should equal(Atom("A"))
    parsing("car[(A . (B1 . B2))]").eval(none) should equal(Atom("A"))
    parsing("car[((A1 . A2) . B)]").eval(none) should equal(Sexp(Atom("A1"), Atom("A2")))
    evaluating { parsing("car[A]").eval(none) } should produce [Exception]
  }
  
  // "The function cdr has one argument..."
  they should "parse cdr" in {
    implicit val parserToTest = funOrSexp // should be cdr but examples in book use other funs as well
    
    parsing("cdr[(A . B)]").eval(none) should equal(Atom("B"))
    parsing("cdr[(A . (B1 . B2))]").eval(none) should equal(Sexp(Atom("B1"), Atom("B2")))
    parsing("cdr[((A1 . A2) . B)]").eval(none) should equal(Atom("B"))
    evaluating { parsing("cdr[A]").eval(none) } should produce [Exception]
    parsing("car[cdr[(A . (B1 . B2))]]").eval(none) should equal(Atom("B1"))
    evaluating { parsing("car[cdr[(A . B)]]").eval(none) } should produce [Exception]
    parsing("car[cons[A;B]]").eval(none) should equal(Atom("A"))
  }
  
  // "If x and y represent any two S-expressions...."
  they should "handle variables" in {
    implicit val parserToTest = funOrSexp
    
    parsing("car[cons[x;y]]").eval(none) should equal(Var("x"))
    parsing("cdr[cons[x;y]]").eval(none) should equal(Var("y"))
  }
  
  // "The predicate eq is a test for equality on atomic symbols..."
  they should "parse eq" in {
    implicit val parserToTest = eqPredicate
    
    parsing("eq[A;A]").eval(none) should equal(T)
    parsing("eq[A;B]").eval(none) should equal(F)
    evaluating { parsing("eq[A;(A . B)]").eval(none) } should produce [Exception]
    evaluating { parsing("eq[(A . B);(A . B)").eval(none) } should produce [Exception]
  }
  
  // "The predicate atom is true if its argument is an atomic symbol..."
  they should "parse atom" in {
    implicit val parserToTest = atomPredicate
    
    parsing("atom[EXTRALONGSTRINGOFLETTERS]").eval(none) should equal(T)
    parsing("atom[(U . V)]").eval(none) should equal(F)
    parsing("atom[car[(U . V)]]").eval(none) should equal(T)
  }  
}

class ParsingListNotation extends FlatSpecForParsers with ElementaryFunctionParsers {
  def none = EmptyContext

  //
  // Section 1.3 : List notation
  //
  
  "The Sexp parsers" should "parse list notation" in {
    implicit val parserToTest = sexp
    
    parsing("(A B C)") should equal(parsing("(A . (B . (C . NIL)))"))
    parsing("((A B) C)") should equal(parsing("((A . (B . NIL)) . (C . NIL))"))
    parsing("(A B (C D))") should equal(parsing("(A . (B . ((C . (D . NIL)) . NIL)))"))
    parsing("(A)") should equal(parsing("(A . NIL)"))
    parsing("((A))") should equal(parsing("((A . NIL) . NIL)"))
    parsing("(A (B . C))") should equal(parsing("(A . ((B . C) . NIL))"))
  }
  
  "The elementary function parsers" should "handle list notation" in {
    implicit val parserToTest = funOrSexp
    
    parsing("car[(A B C)]").eval(none) should equal(Atom("A"))
    parsing("cdr[(A B C)]").eval(none) should equal(parsing("(B C)"))
    parsing("cons[A;(B C)]").eval(none) should equal(parsing("(A B C)"))
    parsing("car[((A B) C)]").eval(none) should equal(parsing("(A B)"))
    parsing("cdr[(A)]").eval(none) should equal(NIL)
    parsing("car[cdr[(A B C)]]").eval(none) should equal(Atom("B"))
  }

  they should "handle compound head/tail functions" in {
    implicit val parserToTest = funOrSexp
    
    parsing("cadr[(A B C)]").eval(none) should equal(Atom("B"))
    parsing("caddr[(A B C)]").eval(none) should equal(Atom("C"))
    parsing("cadadr[(A (B C) D)]").eval(none) should equal(Atom("C"))
  }
  
}
