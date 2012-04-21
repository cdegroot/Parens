package parsing
import scala.util.parsing.combinator.RegexParsers
import parsing.SymbolicExpressionAst._

trait ElementaryFunctionParsers extends SymbolicExpressionParsers {

  def cons: Parser[BaseSexp] = "cons" ~ "[" ~> funOrSexp ~ ";" ~ funOrSexp <~ "]" ^^ { 
    							case x~semicolon~y => Sexp(x,y)}
  
  def car: Parser[Token] = "car" ~ "[" ~> funOrSexp <~ "]" ^^ carOf
  
  private def carOf(exp: Token): Token = exp match { 
    case Sexp(a,b) => a 
    case Atom(_) => throw new Exception("car[] over atom is undefined")
  }
  
  def cdr: Parser[Token] = "cdr" ~ "[" ~> funOrSexp <~ "]" ^^ cdrOf 
  
  private def cdrOf(exp: Token): Token = exp match { 
  	case Sexp(a,b) => b 
    case Atom(_) => throw new Exception("car[] over atom is undefined") 
  }
 
  def carOrCdrs: Parser[Token] = "c" ~> rep1(regex("[ad]".r)) ~ "r[" ~ funOrSexp <~ "]" ^^ {
    case ops~sep~data => 
      val operations = ops.reverse.map(op => op match { case "a" => carOf _; case "d" => cdrOf _})
      operations.foldLeft(data)((list,op) => op(list))
  } 
  
  def eqPredicate: Parser[Token] = "eq" ~ "[" ~> funOrSexp ~ ";" ~ funOrSexp <~ "]" ^^ {
    							case Atom(x)~semicolon~Atom(y) => if (x.equals(y)) T else F 
    							case _ => throw new Exception("eq[] only defined for atoms") }
  
  def atomPredicate: Parser[Token] = "atom" ~ "[" ~> funOrSexp <~ "]" ^^ {
                                case Atom(x) => T
                                case _ => F }
  
  def variable: Parser[Var] = regex("[a-z][a-z0-9]*"r) ^^ { Var(_) }
  
  def funOrSexp: Parser[Token] = sexp | cons | carOrCdrs | eqPredicate | atomPredicate | variable
  

}