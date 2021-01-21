package es.tmoor.compiler.parsers

import util.parsing.combinator.RegexParsers

trait Utils extends RegexParsers {
  def comment: Parser[String] = """//[^\n]*""".r 
  def string: Parser[String] = """"([^\n"]|\\")*"""".r
  def ident: Parser[String] = """[_a-zA-Z][_a-zA-Z0-9]*""".r
}