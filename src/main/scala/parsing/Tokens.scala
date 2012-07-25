package parsing

object Tokens {
  
	sealed abstract class Token {
	  def eval(bindings: Context) : Token
	} 
	
	abstract class BaseSexp extends Token
	case class Atom(name: String) extends BaseSexp {
	  def eval(bindings: Context) = trace (this, bindings) { this }
	}
	case class Sexp(car: Token, cdr: Token) extends BaseSexp {
	  def eval(bindings: Context) = trace (this, bindings) { Sexp(car.eval(bindings), cdr.eval(bindings)) }
	}
	
	val T = Atom("T")
	val F = Atom("F")
	val NIL = Atom("NIL")
	
	case class Var(name: String) extends Token { 
	  def eval(bindings: Context) = trace (this, bindings) { bindings.get(name) match {
        case Some(expr) => expr.eval(bindings)
        case None => Var(name)
      }
    }
	}

  type Context = Map[String, Token]

	// elementary functions
	abstract class ElementaryFunction extends Token
	case class Cons(left: Token, right: Token) extends ElementaryFunction {
	  def eval(bindings: Context) = trace (this, bindings) { Sexp(left.eval(bindings), right.eval(bindings)) }
	}
	case class CdrOf(exp: Token) extends ElementaryFunction {
	  def eval(bindings: Context) = trace (this, bindings) { exp.eval(bindings) match {
	    case sexp:Sexp => sexp.cdr.eval(bindings)
	    case _ => throw new Exception("cdr only defined on sexps")
	  }
	}}
	case class CarOf(exp: Token) extends ElementaryFunction {
	  def eval(bindings: Context) = trace (this, bindings) { exp.eval(bindings) match {
	    case sexp:Sexp => sexp.car.eval(bindings)
	    case _ => throw new Exception("cdr only defined on sexps")
	  }
	}}

	// elementary predicates
	case class EqP(left: Token, right: Token) extends ElementaryFunction {
	  def eval(bindings: Context) = trace (this, bindings) {
      val leftEvaled = left.eval(bindings)
      val rightEvaled = right.eval(bindings)
	    if (!(leftEvaled.isInstanceOf[Atom] && rightEvaled.isInstanceOf[Atom])) {
	      throw new Exception("eq is only defined for atoms")
	    }
	    if (leftEvaled == rightEvaled) T else F
	  }
	}
	
	case class AtomP(value: Token) extends ElementaryFunction {
	  def eval(bindings: Context) = trace (this, bindings) { if (value.eval(bindings).isInstanceOf[Atom]) T else F }
	}

	// conditional expression
	case class Cond(clauses: List[CondElem]) extends Token {
	  def eval(bindings: Context) = trace(this, bindings) {
      clauses.find(clause => clause.eval(bindings) == T).getOrElse(NIL_COND).action.eval(bindings)
    }

	}
	case class CondElem(condition: Token, action: Token) extends Token {
	  def eval(bindings: Context) = trace(this, bindings) {
      condition.eval(bindings)
    }
	}
  object NIL_COND extends CondElem(T, NIL)
	
	// lambda notation
	case class Lambda(args: List[Var], body: Token) extends Token {
	  def eval(bindings: Context) = trace(this, bindings) { NIL }
	}

  // label notation
  case class Label(name: String, expression: Lambda) extends Token {
    def eval(bindings: Context) = trace(this, bindings) { NIL }
  }

  case class Fun(name: String, arguments: List[String], body: Token) extends Token {
    def eval(bindings: Context): Token = trace (this, bindings) { body.eval(bindings) }
  }

  // a function call.
	case class FunCall(name: String, args: List[Token]) extends Token {
	  def eval(bindings: Context) = trace (this, bindings) {
      val fun = bindings(name).asInstanceOf[Fun]
      val boundArgs = fun.arguments.zip(args.map(arg => arg.eval(bindings)))
      fun.body.eval(bindings ++ boundArgs)

    }
	}
  var indent = 0
  val SOFT_STACK_LIMIT = 40
  val VERBOSE_TRACING = false
  def trace(thisToken: Token, bindings: Context)(f : => Token) = {
    if (indent > SOFT_STACK_LIMIT) throw new RuntimeException("Soft stack overflow")
    val space = "% 2d".format(indent) + ":" + (" " * indent)
    if (VERBOSE_TRACING) System.err.println(space + "trace(" + thisToken + ")\n" + space + "    (" + bindings + ")")
    indent += 1
    val ret = f
    indent -= 1
    if (VERBOSE_TRACING) System.err.println(space + "-> " + ret)
    ret
  }
}

