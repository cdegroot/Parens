package parsing
import scala.util.parsing.combinator.RegexParsers
import parsing.Tokens._

trait SymbolicExpressionParsers extends RegexParsers {

  def sexp: Parser[Token] = "(" ~> sexp ~ "." ~ sexp <~ ")" ^^ { case x~dot~y => Sexp(x, y) } | atom | list
  
  def atom: Parser[Atom]  = regex("[A-Z][A-Z0-9]*"r) ^^ { new Atom(_) }

  // List notation
  
  def list: Parser[Token] = "(" ~> rep1sep(sexp, sexpSep) <~ ")" ^^ {
    case sexps => listToDots(sexps)
  }

  def sexpSep = elem(' ') | elem(',') ~ elem(' ')
    
  def listToDots(sexps: List[Token]) : Sexp = sexps match {
    case x :: Nil => Sexp(x, NIL)
      case x :: xs  => Sexp(x, listToDots(xs))      
  }  
}
