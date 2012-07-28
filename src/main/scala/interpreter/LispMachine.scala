package interpreter

import javax.naming.Context

object LispMachine {

  // The expressions building up the universal Lisp Function, and some easy
  // to handle chunks for unit testing :)

  val equal = "equal[x;y]=[" +
    "atom[x]→[atom[y]→eq[x;y]; T→F];" +
    "atom[y]→F;" + // BUG in the original paper.
    "equal[car[x];car[y]]→equal[cdr[x];cdr[y]];" +
    "T→F]"

  val subst = "subst[x;y;z]=[equal[y;z]→x;atom[z]→z;T→cons[subst[x;y;car[z]];subst[x;y;cdr[z]]]]"

  // null is not given, but this should work...
  val nul = "null[x]=[equal[x;NIL]→T;T→F]"

  val append = "append[x;y]=[null[x]→y;T→cons[car[x];append[cdr[x];y]]]"
  val exprsForAppend = Seq(equal, subst, nul, append)

  val member = "member[x;y]=[" +
    "null[y] → F;" +
    "equal[x;car[y]] → T;" +
    "T → member[x;cdr[y]]]"
  val exprsForMember = exprsForAppend ++ Seq(member)

  val pairlis = "pairlis[x;y;a] = [" +
    "null[x]→a; " +
    "T→cons[cons[car[x]; car[y]]; " +
    "pairlis[cdr[x]; cdr[y]; a]]]"
  val exprsForPairlis = exprsForMember ++ Seq(pairlis)

  val assoc = "assoc[x;a]=[" +
    "equal[caar[a];x]→car[a];" +
    "T→assoc[x;cdr[a]]]"
  val exprsForAssoc = exprsForPairlis ++ Seq(assoc)

  val sub2 = "sub2[a;z] = [" +
    "null[a]→z;" +
    "eq[caar[a];z]→cdar[a];" +
    "T→sub2[cdr[a];z]]"
  val sublis = "sublis[a;y] = " +
    "[atom[y]→sub2[a;y];" +
    "T→cons[sublis[a;car[y]];sublis[a;cdr[y]]]]"
  val exprsForSublis = exprsForAssoc ++ Seq(sub2, sublis)

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


}
