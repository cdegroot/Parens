package parsing

object SymbolicExpressionAst {
  
	sealed abstract class BaseSexp
	case class Atom(name: String) extends BaseSexp
	case class Sexp(car: BaseSexp, cdr: BaseSexp) extends BaseSexp
	
}