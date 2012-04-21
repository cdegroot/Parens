package parsing
import scala.util.parsing.combinator.RegexParsers
import parsing.SymbolicExpressionAst._

trait SymbolicExpressionParsers extends RegexParsers {

  def atom = regex("[A-Z][A-Z0-9]*"r) ^^ { new Atom(_) }
}