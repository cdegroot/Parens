package parsing
import parsing.Tokens._

trait MetaLanguageParsers extends ElementaryFunctionParsers {

  def functionDefinition : Parser[Fun] = variable ~ "[" ~ rep1sep(variable, elem(';')) ~ "]" ~ "=" ~ funOrSexp ^^ {
    case funName~leftBlock~args~rightBlock~equalsSign~body => Fun(funName, args, body)      
  }
  
  def conditionalExpression: Parser[Cond] = "[" ~> rep1sep(singleConditional, elem(';')) <~ "]" ^^ {
    case conditionals => Cond(conditionals)
  }
  
  def singleConditional: Parser[CondElem] = funOrSexp ~ "→" ~ funOrSexp ^^ {
    case cond~sep~action => CondElem(cond, action)
  }
  
  def functionCall: Parser[FunCall] = variable ~ "[" ~ rep1sep(funOrSexp, elem(';')) <~ "]" ^^ {
    case name~sep~args => FunCall(name.name, args)
  }
  
  override def funOrSexp: Parser[Token] = sexp | cons | carOrCdrs | eqPredicate | atomPredicate | conditionalExpression | functionCall | variable
  
  def lambdaNotation: Parser[Lambda] = "λ[[" ~> repsep(variable, elem(';')) ~ "];" ~ funOrSexp <~ "]" ^^ {
    case args~sep~body => Lambda(args, body)
  }
  		
}