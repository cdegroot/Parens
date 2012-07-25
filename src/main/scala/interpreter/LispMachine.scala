package interpreter

import parsing.Tokens.{Label, Lambda}
import collection.mutable

class LispMachine {
  val functionBindings = new mutable.HashMap[String, Lambda]

  def addBinding(labelNotation: Label): Unit = addBinding(labelNotation.name, labelNotation.expression)

  def addBinding(name: String, expression: Lambda): Unit = functionBindings.put(name, expression)


}
