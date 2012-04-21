package parsing
import parsing.Tokens._

trait MetaLanguageParsers extends ElementaryFunctionParsers {

  def functionDefinition : Parser[Fun] = variable ~ "[" ~ rep1sep(variable, elem(';')) ~ "]" ~ "=" ~ funOrSexp ^^ {
    case funName~leftBlock~args~rightBlock~equalsSign~body => Fun(funName, args, body)      
  }
}