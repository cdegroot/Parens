package parsing

import parsing.Tokens.{Token, Atom, Context, EmptyContext}
import interpreter.{LispInterpreterParser, LispMachine}
import interpreter.LispInterpreterParser.buildContextFrom

class UniversalLispFunction extends FlatSpecForParsers with MetaLanguageParsers {
  implicit val parserToTest = funOrSexp

  // Note that we skip the M->S rewriting on page 10. We keep our meta language
  // parser around to directly parse M-exps (probably "for now")

  def evaluating(expr: String)(implicit context: Context): Token = parsing(expr)(parserToTest).eval(context)

  "The meta language parsers" should "parse the definition of equal" in {
    val ctx = buildContextFrom(Seq(LispMachine.equal))
    val code = parsing("equal[x;y]")
    code.eval(ctx ++ List(
      "x" -> parsing("(A B)")(sexp),
      "y" -> parsing("(A B)")(sexp))) should equal(Tokens.T)
  }

  val subst = "subst[x;y;z]=[equal[y;z]→x;atom[z]→z;T→cons[subst[x;y;car[z]];subst[x;y;cdr[z]]]]"

  they should "parse the definition of subst" in {
    implicit val ctx = buildContextFrom(Seq(LispMachine.equal, subst))
    evaluating("subst[(X . A);B;((A . B) . C)]") should equal(parsing("((A . (X . A)) . C)")(sexp))
  }

  they should "parse the definition of null" in {
    implicit val ctx = buildContextFrom(Seq(LispMachine.equal, subst, LispMachine.nul))
    evaluating("null[(A . B)]") should equal(Tokens.F)
    evaluating("null[NIL]") should equal(Tokens.T)
  }

  they should "parse the definition of append" in {
    implicit val ctx = buildContextFrom(LispMachine.exprsForAppend)
    evaluating("append[(A B);(C D E)]") should equal(parsing("(A B C D E)")(sexp))
  }

  they should "parse the definition of member" in {
    implicit val ctx = buildContextFrom(LispMachine.exprsForMember)
    evaluating("member[(A B);(C D (A B) E)]") should equal(Tokens.T)
  }

  they should "parse the definition of pairlis" in {
    implicit val ctx = buildContextFrom(LispMachine.exprsForPairlis)
    evaluating("pairlis[(A B C);(U V W);((D . X) (E . Y))]") should equal(parsing("((A . U) (B . V) (C . W) (D . X) (E . Y))")(sexp))
  }

  they should "parse the definition of assoc" in {
    implicit val ctx = buildContextFrom(LispMachine.exprsForAssoc)
    evaluating("assoc[B;((A.(M N)), (B .(CAR X)), (C .(QUOTE M)), (C .(CDR X)))]") should equal(parsing("(B . (CAR X))")(sexp))
  }

  they should "parse the definition of sublis" in {
    implicit val ctx = buildContextFrom(LispMachine.exprsForSublis)
    evaluating("sublis[((X . SHAKESPEARE) (Y . (THE TEMPEST)));(X WROTE Y)]") should  equal(parsing("(SHAKESPEARE WROTE (THE TEMPEST))"))
  }


  they should "parse evalquote" in {
    implicit val ctx = buildContextFrom(LispMachine.exprsForEvalquote)

    evaluating("evalquote[(LAMBDA (X Y) (CONS (CAR X) Y)); ((A B) (C D))]") should equal(parsing("(A C D)")(sexp))
  }

}
