package parsing

import parsing.Tokens.{Atom, Context}

class UniversalLispFunction extends FlatSpecForParsers with MetaLanguageParsers {
  // First, we test whether we can define and execute a recursive function

  "The meta language parsers" should "define and execute recursive functions" in {
    implicit val parserToTest = functionDefinition

    val expr = "equal[x;y]=[atom[x]→[atom[y]→eq[x;y]; T→F]; equal[car[x];car[y]]→equal[cdr[x];cdr[y]];T→F]"
    val fun = parsing(expr)
    fun.eval(Map(
      "x" -> parsing("(A B)")(sexp),
      "y" -> parsing("(A B)")(sexp),
      "equal" -> fun)) should equal(Tokens.T)
  }
}
