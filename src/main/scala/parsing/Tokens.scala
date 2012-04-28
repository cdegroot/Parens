package parsing

object Tokens {
  
	sealed abstract class Token {
	  def eval : Token
	  def bind(binding: Binding) : Token
	} 
	
	abstract class BaseSexp extends Token
	case class Atom(name: String) extends BaseSexp {
	  def eval = this; 
	  def bind(binding: Binding) = this 
	}
	case class Sexp(car: Token, cdr: Token) extends BaseSexp {
	  def eval = Sexp(car.eval, cdr.eval)
	  def bind(binding: Binding) = Sexp(car.bind(binding), cdr.bind(binding)) 
	}
	
	val T = Atom("T")
	val F = Atom("F")
	val NIL = Atom("NIL")
	
	case class Var(name: String) extends Token { 
	  def eval = this
      def bind(binding: Binding) = if (binding.name.equals(name)) binding.value else this
	}
	
	case class Binding(name: String, value: Token)
	case class Fun(name: Var, arguments: List[Var], body: Token) extends Token {
	  def eval = throw new Exception("eval(Fun) undefined")
	  
	  def callWith(bindings: List[Binding]) = {
	    // check for valid arguments
	    bindings.map(binding => 
	    	    arguments.find(arg => arg.name.equals(binding.name)).getOrElse(
	    	    		throw new Exception("Unknown argument name " + binding.name)
	    	    ));

	    // a very functional binding - optimization comes later :P
	    bindings.foldLeft(this: Token)((fun, arg) => fun.bind(arg)).eval
	  }
	  def bind(binding: Binding) = {
	    body.bind(binding)
	  }
	}
	
	// elementary functions
	abstract class ElementaryFunction extends Token
	case class Cons(left: Token, right: Token) extends ElementaryFunction {
	  def eval = Sexp(left.eval, right.eval)
	  def bind(binding: Binding) = Cons(left.bind(binding), right.bind(binding))
	}
	case class CdrOf(exp: Token) extends ElementaryFunction {
	  def eval = exp.eval match {
	    case sexp:Sexp => sexp.cdr
	    case _ => throw new Exception("cdr only defined on sexps")
	  }
	  def bind(binding: Binding) = CdrOf(exp.bind(binding))
	}
	case class CarOf(exp: Token) extends ElementaryFunction {
	  def eval = exp.eval match {
	    case sexp:Sexp => sexp.car
	    case _ => throw new Exception("cdr only defined on sexps")
	  }
	  def bind(binding: Binding) = CarOf(exp.bind(binding))
	}

	// elementary predicates
	case class EqP(left: Token, right: Token) extends ElementaryFunction {
	  def eval = {
	    if (!(left.isInstanceOf[Atom] && right.isInstanceOf[Atom])) {
	      throw new Exception("eq is only defined for atoms")
	    }
	    if (left.eval.equals(right.eval)) T else F
	  }
	  def bind(binding: Binding) = EqP(left.bind(binding), right.bind(binding))
	}
	
	case class AtomP(value: Token) extends ElementaryFunction {
	  def eval = if (value.eval.isInstanceOf[Atom]) T else F
	  def bind(binding: Binding) = AtomP(value.bind(binding))
	}

	// conditional expression
	case class Cond(clauses: List[CondElem]) extends Token {
	  def eval =  NIL
	  def bind(binding: Binding) = NIL
	}
	
	case class CondElem(condition: Token, action: Token) extends Token {
	  def eval =  NIL
	  def bind(binding: Binding) = NIL
	}
	
	// lambda notation
	case class Lambda(args: List[Var], body: Token) extends Token {
	  def eval = NIL
	  def bind(binding: Binding) = NIL
	}
	
	// a function call. I expect this to disappear, it's ugly ;-)
	case class FunCall(name: String, args: List[Token]) extends Token {
	  def eval = NIL
	  def bind(binding: Binding) = NIL
	}
}

