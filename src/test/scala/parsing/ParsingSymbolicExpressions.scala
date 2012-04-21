package parsing
import parsing.SymbolicExpressionAst._

class ParsingSymbolicExpressions extends FlatSpecForParsers with SymbolicExpressionParsers {

  "The Sexp parsers" should "parse an atom" in {
    implicit val parserToTest = atom

    parsing("ABCDE") should equal(Atom("ABCDE"))
  }

}