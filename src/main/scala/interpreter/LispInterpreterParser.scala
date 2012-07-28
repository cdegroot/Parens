package interpreter

import util.parsing.input.CharSequenceReader
import parsing.Tokens._
import util.parsing.input.CharSequenceReader
import parsing.{Tokens, MetaLanguageParsers}

/**
 * Lisp interpreter, as defined in chapter II.
 */
class LispInterpreter(context: Context = LispInterpreterParser.buildContextFrom(LispMachine.exprsForEvalquote)) {

  def interpret(input: String): LispInterpreter = {
    val doublet = LispInterpreterParser.parseCommand(input)
    doublet.name match {
      case Atom("DEFINE") => new LispInterpreter(handleDefine(doublet.value))
      case _ => { executeDoublet(doublet); this }
    }
  }

  // DEFINE (((fundef....) (fundef...) (fundef...)))
  def handleDefine(definitions: Token): Context = {
    val sexp = definitions.asInstanceOf[Sexp]
    System.err.println("Handle define of " + sexp)
    context
  }

  def executeDoublet(doublet: Doublet) {

  }


}

case class Doublet(name: Token, value: Token)

object LispInterpreterParser extends MetaLanguageParsers {
  def command = sexp ~ elem(' ') ~ sexp ^^ { case atom~sep~sexp => Doublet(sexp, sexp)}

  def parseCommand(string: String) = {
    val phraseParser = phrase(command)
    parseWithPhraseParser(string, phraseParser)
  }


  def buildContextFrom(exps: Seq[String]): Context = {
    exps.foldLeft(EmptyContext)((ctx, exp) => {
      val fun = parse(exp, functionDefinition)
      ctx + (fun.name -> fun)
    })
  }

  def parse[T](string: String, parser: Parser[T]) = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(parser)
    parseWithPhraseParser(string, phraseParser)
  }

  def parseWithPhraseParser[T](string: String, parser: Parser[T]) = {

    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(string)
    parser(input) match {
      case Success(t,_)     => t
      case NoSuccess(msg,next) => throw new IllegalArgumentException(
        "[" + next.pos + "] Could not parse: " + msg + "\n\n" + next.pos.longString)
    }

  }
}
