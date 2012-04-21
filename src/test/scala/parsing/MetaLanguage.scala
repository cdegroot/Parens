package parsing

class MetaLanguage extends FlatSpecForParsers with MetaLanguageParsers {

  "The meta language parser" should "handle function definitions" in {
    implicit val parserToTest = functionDefinition
    
    parsing("third[x]=car[cdr[cdr[x]]]") 
  }
}