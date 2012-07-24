package parsing

import parsing.Tokens.Binding
import interpreter.LispMachine

class UniversalLispFunction extends FlatSpecForParsers with MetaLanguageParsers {
  // First, we test whether we can define and execute a recursive function

  "The meta language parsers" should "define and execute recursive functions" in {
    implicit val parserToTest = functionDefinition

    val expr = "equal[x;y]=[atom[x]→[atom[y]→eq[x;y]; T→F]; equal[car[x];car[y]]→equal[cdr[x];cdr[y]];T→F]"
    val fun = parsing(expr)
    fun.callWith(List(Binding("x", parsing("(A B)")(sexp)), Binding("y", parsing("(A B)")(sexp)))) should equal(Tokens.T)
  }
}
