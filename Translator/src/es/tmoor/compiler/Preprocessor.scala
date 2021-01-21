package es.tmoor.compiler

import util.parsing.combinator._
import collection.mutable.HashSet

trait Preprocessor extends RegexParsers {
  override def skipWhitespace: Boolean = true
  private class Define(name: String, definition: String) {
    def parser = name.r ^^ { _ => definition }
  }

  private class ArgDefine(name: String, args: List[String], definition: String) extends Define(name, definition) {
    private def fill(l: List[String]) = l
    override def parser = (name.r ~ "(" ~ (args.indices.foldLeft(ident)((acc, v) => (("""("([^\n"]|\\")"|[^\n,])""".r <~ ",") ~ acc)^^{s => s._1++s._2})) ~ ")") ^^ { _.toString }
  }
  def defArgs: Parser[List[String]] = "(" ~> (((ident <~ ",").* ~ ident).?) ~ ")" ^^ {
    case Some(a) ~ _ => a._1 :+ a._2
    case None ~ _ => List()
  }
  def comment: Parser[String] = """//[^\n]*""".r 
  def string: Parser[String] = """"([^\n"]|\\")*"""".r
  def ident: Parser[String] = """[_a-zA-Z][_a-zA-Z0-9]*""".r
  def definition: Parser[String] = """[^\n]*""".r
  private def define: Parser[Define] = """#define""".r ~> ident ~ (defArgs.?) ~ definition ^^ {
    case i ~ Some(a) ~ d => defines += new ArgDefine(i,a,d); defines.last
    case i ~ None ~ d => defines += new Define(i, d); defines.last
  }
  def defineP: Parser[String] = defines.map(_.parser).reduce(_|_)
//  def program: Parser[String] = 

  private val defines = HashSet[Define]()

  def test = {
    val t = new Define("hello", "printf(\"hello world!\")")
    defines.add(t)
  }
}
