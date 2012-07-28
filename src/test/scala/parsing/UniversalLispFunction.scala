package parsing

import parsing.Tokens.{Atom, Context, EmptyContext}

class UniversalLispFunction extends FlatSpecForParsers with MetaLanguageParsers {
  implicit val parserToTest = funOrSexp

  // Note that we skip the M->S rewriting on page 10. We keep our meta language
  // parser around to directly parse M-exps

  def buildContextFrom(exps: Seq[String]): Context = {
    exps.foldLeft(EmptyContext)((ctx, exp) => {
      val fun = parsing(exp)(functionDefinition)
      ctx + (fun.name -> fun)
    })
  }

  val equal = "equal[x;y]=[" +
    "atom[x]→[atom[y]→eq[x;y]; T→F];" +
    "atom[y]→F;" + // BUG in the original paper.
    "equal[car[x];car[y]]→equal[cdr[x];cdr[y]];" +
    "T→F]"

  "The meta language parsers" should "parse the definition of equal" in {
    val ctx = buildContextFrom(Seq(equal))
    val code = parsing("equal[x;y]")
    code.eval(ctx ++ List(
      "x" -> parsing("(A B)")(sexp),
      "y" -> parsing("(A B)")(sexp))) should equal(Tokens.T)
  }

  val subst = "subst[x;y;z]=[equal[y;z]→x;atom[z]→z;T→cons[subst[x;y;car[z]];subst[x;y;cdr[z]]]]"

  they should "parse the definition of subst" in {
    val ctx = buildContextFrom(Seq(equal, subst))
    val code = parsing("subst[(X . A);B;((A . B) . C)]")
    code.eval(ctx) should equal(parsing("((A . (X . A)) . C)")(sexp))
  }

  // null is not given, but this should work...
  val nul = "null[x]=[equal[x;NIL]→T;T→F]"

  they should "parse the definition of null" in {
    val ctx = buildContextFrom(Seq(equal, subst, nul))
    parsing("null[(A . B)]").eval(ctx) should equal(Tokens.F)
    parsing("null[NIL]").eval(ctx) should equal(Tokens.T)
  }

  val append = "append[x;y]=[null[x]→y;T→cons[car[x];append[cdr[x];y]]]"
  val exprsForAppend = Seq(equal, subst, nul, append)

  they should "parse the definition of append" in {
    val ctx = buildContextFrom(exprsForAppend)
    parsing("append[(A B);(C D E)]").eval(ctx) should equal(parsing("(A B C D E)")(sexp))
  }

  val member = "member[x;y]=[" +
    "null[y] → F;" +
    "equal[x;car[y]] → T;" +
    "T → member[x;cdr[y]]]"
  val exprsForMember = exprsForAppend ++ Seq(member)

  they should "parse the definition of member" in {
    val ctx = buildContextFrom(exprsForMember)
    parsing("member[(A B);(C D (A B) E)]").eval(ctx) should equal(Tokens.T)
  }

  val pairlis = "pairlis[x;y;a] = [" +
    "null[x]→a; " +
    "T→cons[cons[car[x]; car[y]]; " +
    "pairlis[cdr[x]; cdr[y]; a]]]"
  val exprsForPairlis = exprsForMember ++ Seq(pairlis)

  they should "parse the definition of pairlis" in {
    val ctx = buildContextFrom(exprsForPairlis)
    parsing("pairlis[(A B C);(U V W);((D . X) (E . Y))]").eval(ctx) should equal(parsing("((A . U) (B . V) (C . W) (D . X) (E . Y))")(sexp))
  }

  val assoc = "assoc[x;a]=[" +
    "equal[caar[a];x]→car[a];" +
    "T→assoc[x;cdr[a]]]"
  val exprsForAssoc = exprsForPairlis ++ Seq(assoc)

  they should "parse the definition of assoc" in {
    val ctx = buildContextFrom(exprsForAssoc)
    parsing("assoc[B;((A.(M N)), (B .(CAR X)), (C .(QUOTE M)), (C .(CDR X)))]").eval(ctx) should equal(parsing("(B . (CAR X))")(sexp))
  }

  val sub2 = "sub2[a;z] = [" +
    "null[a]→z;" +
    "eq[caar[a];z]→cdar[a];" +
    "T→sub2[cdr[a];z]]"
  val sublis =  "sublis[a;y] = " +
    "[atom[y]→sub2[a;y];" +
    "T→cons[sublis[a;car[y]];sublis[a;cdr[y]]]]"
  val exprsForSublis = exprsForAssoc ++ Seq(sub2, sublis)

  they should "parse the definition of sublis" in {
    val ctx = buildContextFrom(exprsForSublis)
    parsing("sublis[((X . SHAKESPEARE) (Y . (THE TEMPEST)));(X WROTE Y)]").eval(ctx) should  equal(parsing("(SHAKESPEARE WROTE (THE TEMPEST))"))
  }

  // Finishing off with the main juice
  val evalquote = "evalquote[fn;x] = apply[fn;x;NIL]"
  val apply = "apply[fn;x;a]=[" +
    "atom[fn] → [eq[fn;CAR] → caar[x];" +
    "            eq[fn;CDR] → cdr[x];" +
    "            eq[fn;CONS]→ cons[car[x];cadr[x]];" +
    "            eq[fn;ATOM]→ atom[car[x]];" +
    "            eq[fn;EQ]  → eq[car[x];cadr[x]];" +
    "            T          → apply[eval[fn;a];x;a]];" +
    "eq[car[fn];LAMBDA] → eval[caddr[fn];pairlis[cadr[fn];x;a]];" +
    "eq[car[fn];LABEL]  → aply[caddr[fn];x;cons[cons[cadr[fn];caddr[fn]];a]]]"
  val eval = "eval[e;a] = [" +
    "atom[e]      → cdr[assoc[e;a]];" +
    "atom[car[e]] → [" +
    "       eq[car[e];QUOTE] → cadr[e];" +
    "       eq[car[e];COND]  → evcon[cdr[e];a];" +
    "       T                → apply[car[e];evlis[cdr[e];a];a]];" +
    "T            → apply[car[e];evlis[cdr[e];a];a]]"
  val evcon = "evcon[c;a] = [" +
    "eval[caar[c];a] → eval[cadar[c];a];" +
    "T               → evcon[cdr[c];a]]"
  val evlis = "evlis[m;a] = [" +
    "null[m] → NIL;" +
    "T       → cons[eval[car[m];a];evlis[cdr[m];a]]]"

  val exprsForEvalquote = exprsForSublis ++ Seq(evalquote, apply, eval, evcon, evlis)

  they should "parse evalquote" in {
    val ctx = buildContextFrom(exprsForEvalquote)
    parsing("evalquote[CAR;((A B))]").eval(ctx) should equal(Atom("A")) // Trivial test case to check for typos :-)
    parsing("evalquote[(LAMBDA (X Y) (CONS (CAR X ) Y)); ((A B) (C D))]").eval(ctx) should equal(parsing("(A C D)")(sexp))
  }

}
