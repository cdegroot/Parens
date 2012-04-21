package parsing
import scala.util.parsing.combinator.RegexParsers
import parsing.SymbolicExpressionAst._

trait ElementaryFunctionParsers extends SymbolicExpressionParsers {

  def cons: Parser[Sexp] = "cons" ~ "[" ~> funOrSexp ~ ";" ~ funOrSexp <~ "]" ^^ { case x~semicolon~y => Sexp(x,y)}
  
  def car: Parser[BaseSexp] = "car" ~ "[" ~> funOrSexp <~ "]" ^^ { 
    							case Sexp(a,b) => a 
    							case Atom(_) => throw new Exception("car[] over atom is undefined") }
  
  def cdr: Parser[BaseSexp] = "cdr" ~ "[" ~> funOrSexp <~ "]" ^^ { 
    							case Sexp(a,b) => b 
    							case Atom(_) => throw new Exception("car[] over atom is undefined") }
 
  
  def funOrSexp = sexp | cons | car | cdr
}