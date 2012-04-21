package parsing

object SymbolicExpressionAst {
  
	sealed abstract class Token
	
	case class BaseSexp extends Token
	case class Atom(name: String) extends BaseSexp
	case class Sexp(car: Token, cdr: Token) extends BaseSexp
	
	val T = Atom("T")
	val F = Atom("F")
	
	case class Var(name: String) extends Token	
}