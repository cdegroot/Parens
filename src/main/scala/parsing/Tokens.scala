package parsing

object Tokens {
  
	sealed abstract class Token { def eval: Token } 
	
	abstract class BaseSexp extends Token
	case class Atom(name: String) extends BaseSexp { def eval = this }
	case class Sexp(car: Token, cdr: Token) extends BaseSexp {def eval = Sexp(car.eval, cdr.eval)}
	
	val T = Atom("T")
	val F = Atom("F")
	val NIL = Atom("NIL")
	
	case class Var(name: String) extends Token { 
	  def eval = this // Later on, we want variable binding here, I think :)
	}
	
	case class Fun(name: Var, args: List[Var], body: Token) extends Token {
	  def eval = throw new Exception("eval(Fun) undefined")
	}
	
	// elementary functions
	abstract class ElementaryFunction extends Token
	case class Cons(left: Token, right: Token) extends ElementaryFunction {
	  def eval = Sexp(left.eval, right.eval)
	}
	case class CdrOf(exp: Token) extends ElementaryFunction {
	  def eval = exp.eval match {
	    case sexp:Sexp => sexp.cdr
	    case _ => throw new Exception("cdr only defined on sexps")
	  }
	}
	case class CarOf(exp: Token) extends ElementaryFunction {
	  def eval = exp.eval match {
	    case sexp:Sexp => sexp.car
	    case _ => throw new Exception("cdr only defined on sexps")
	  }
	}

	// elementary predicates
	case class EqP(left: Atom, right: Atom) extends ElementaryFunction {
	  def eval = if (left.eval.equals(right.eval)) T else F
	}
	
	case class AtomP(value: Token) extends ElementaryFunction {
	  def eval = if (value.eval.isInstanceOf[Atom]) T else F
	}

}

