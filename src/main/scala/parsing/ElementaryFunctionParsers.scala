package parsing
import scala.util.parsing.combinator.RegexParsers
import parsing.Tokens._

trait ElementaryFunctionParsers extends SymbolicExpressionParsers {

  def cons: Parser[ElementaryFunction] = "cons" ~ "[" ~> funOrSexp ~ ";" ~ funOrSexp <~ "]" ^^ { 
    							case x~semicolon~y => Cons(x,y) }
  
  def car: Parser[ElementaryFunction] = "car" ~ "[" ~> funOrSexp <~ "]" ^^ { case x => CarOf(x) }

  def cdr: Parser[ElementaryFunction] = "cdr" ~ "[" ~> funOrSexp <~ "]" ^^ { case x => CdrOf(x) }
 
  def carOrCdrs: Parser[Token] = "c" ~> rep1(regex("[ad]".r)) ~ "r[" ~ funOrSexp <~ "]" ^^ {
    case ops~sep~data =>
      val operations = ops.reverse.map(op => op match { 
        case "a" => { x => CarOf(x) } 
        case "d" => { x => CdrOf(x) }
      })
      operations.foldLeft(data)((list,operation) => operation(list))
  } 
  
  def eqPredicate: Parser[Token] = "eq" ~ "[" ~> funOrSexp ~ ";" ~ funOrSexp <~ "]" ^^ {
    							case Atom(x)~semicolon~Atom(y) => if (x.equals(y)) T else F 
    							case _ => throw new Exception("eq[] only defined for atoms") }
  
  def atomPredicate: Parser[Token] = "atom" ~ "[" ~> funOrSexp <~ "]" ^^ { case x => AtomP(x) }
  
  def variable: Parser[Var] = regex("[a-z][a-z0-9]*"r) ^^ { Var(_) }
  
  def funOrSexp: Parser[Token] = sexp | cons | carOrCdrs | eqPredicate | atomPredicate | variable
  

}