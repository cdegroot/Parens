package parsing
import scala.util.parsing.combinator.RegexParsers
import parsing.SymbolicExpressionAst._

trait SymbolicExpressionParsers extends RegexParsers {

  def sexp: Parser[Token] = "(" ~> sexp ~ "." ~ sexp <~ ")" ^^ { case x~dot~y => Sexp(x, y) } | atom  
  
  def atom: Parser[Atom]  = regex("[A-Z][A-Z0-9]*"r) ^^ { new Atom(_) }

}