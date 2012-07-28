package parsing

import parsing.Tokens.{Context, EmptyContext}

class UniversalLispFunction extends FlatSpecForParsers with MetaLanguageParsers {
  implicit val parserToTest = funOrSexp

  // Note that we skip the M->S rewriting on page 10. We keep our meta language
  // parser around to directly parse M-exps

  def buildContextFrom(exps: String*): Context = {
    exps.foldLeft(EmptyContext)((ctx, exp) => {
      val fun = parsing(exp)(functionDefinition)
      ctx + (fun.name -> fun)
    })
  }

  val equal = "equal[x;y]=[atom[x]→[atom[y]→eq[x;y]; T→F]; equal[car[x];car[y]]→equal[cdr[x];cdr[y]];T→F]"

  "The meta language parsers" should "parse the definition of equal" in {
    val ctx = buildContextFrom(equal)
    val code = parsing("equal[x;y]")
    code.eval(ctx ++ List(
      "x" -> parsing("(A B)")(sexp),
      "y" -> parsing("(A B)")(sexp))) should equal(Tokens.T)
  }

  val subst = "subst[x;y;z]=[equal[y;z]→x;atom[z]→z;T→cons[subst[x;y;car[z]];subst[x;y;cdr[z]]]]"

  they should "parse the definition of subst" in {
    val ctx = buildContextFrom(equal, subst)
    val code = parsing("subst[(X . A);B;((A . B) . C)]")
    code.eval(ctx) should equal(parsing("((A . (X . A)) . C)")(sexp))
  }

  // null is not given, but this should work...
  val nul = "null[x]=[equal[x;NIL]→T;T→F]"

  they should "parse the definition of null" in {
    val ctx = buildContextFrom(equal, nul)
    parsing("null[(A . B)]").eval(ctx) should equal(Tokens.F)
    parsing("null[NIL]").eval(ctx) should equal(Tokens.T)
  }

}
