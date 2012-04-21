package parsing

object SymbolicExpressionAst {
	sealed abstract class Token
	case class Atom(name: String) extends Token
}