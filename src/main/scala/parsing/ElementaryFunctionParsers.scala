package parsing
import scala.util.parsing.combinator.RegexParsers
import parsing.SymbolicExpressionAst._

trait ElementaryFunctionParsers extends SymbolicExpressionParsers {

  def cons: Parser[Sexp] = "cons" ~ "[" ~> funOrSexp ~ ";" ~ funOrSexp <~ "]" ^^ { case x~semicolon~y => Sexp(x,y)}
  
  def funOrSexp = sexp | cons
}