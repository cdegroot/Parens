package parsing
import scala.util.parsing.combinator.RegexParsers
import parsing.SymbolicExpressionAst._

trait ElementaryFunctionParsers extends SymbolicExpressionParsers {

  def cons: Parser[BaseSexp] = "cons" ~ "[" ~> funOrSexp ~ ";" ~ funOrSexp <~ "]" ^^ { 
    							case x~semicolon~y => Sexp(x,y)}
  
  def car: Parser[Token] = "car" ~ "[" ~> funOrSexp <~ "]" ^^ { 
    							case Sexp(a,b) => a 
    							case Atom(_) => throw new Exception("car[] over atom is undefined") }
  
  def cdr: Parser[Token] = "cdr" ~ "[" ~> funOrSexp <~ "]" ^^ { 
    							case Sexp(a,b) => b 
    							case Atom(_) => throw new Exception("car[] over atom is undefined") }
 
  def eqPredicate: Parser[Token] = "eq" ~ "[" ~> funOrSexp ~ ";" ~ funOrSexp <~ "]" ^^ {
    							case Atom(x)~semicolon~Atom(y) => if (x.equals(y)) T else F 
    							case _ => throw new Exception("eq[] only defined for atoms") }
  
  def atomPredicate: Parser[Token] = "atom" ~ "[" ~> funOrSexp <~ "]" ^^ {
                                case Atom(x) => T
                                case _ => F }
  
  def variable: Parser[Var] = regex("[a-z][a-z0-9]*"r) ^^ { Var(_) }
  
  def funOrSexp: Parser[Token] = sexp | cons | car | cdr | eqPredicate | atomPredicate | variable
}