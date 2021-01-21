package es.tmoor.compiler

import util.parsing.combinator._
import collection.mutable.HashSet

trait Preprocessor extends RegexParsers {
  val defines = HashSet[Define]()

  override def skipWhitespace: Boolean = true

  class Define(name: String, definition: String) {
    def parser = name.r ^^ { _ => definition }
    override def toString = name
  }

  class ArgDefine(name: String, args: List[String], definition: String) extends Define(name, definition) {
    private def fill(l: List[String]) = l
    def replace(l: List[String]) = args.mkString("|").r.replaceAllIn(definition, _.matched match {
      case m: String if m != "" => l(args.indexOf(m))
      case _ => ""
    })
    override def parser = (name.r ~ defApplyArgs) ^^ { 
      case n ~ a => replace(a)
     }
  }
  def defArgs: Parser[List[String]] = "(" ~> (((ident <~ ",").* ~ ident).?) ~ ")" ^^ {
    case Some(a) ~ _ => a._1 :+ a._2
    case None ~ _ => List()
  }
  def defApplyArgs: Parser[List[String]] = "(" ~> (((defineP match {
    case None => """[^\n,]+""".r | string
    case Some(d) => d | """[^\n,]+""".r | string
  }).* <~ ",").* ~ (defineP match {
    case None => """[^\n)]+""".r | string
    case Some(d) => d | """[^\n)]+""".r | string
  }).*).? <~ ")" ^^ {
    case Some(l ~ a) => l.map(_.mkString) :+ a.mkString
    case None => List()
  }
  def comment: Parser[String] = """//[^\n]*""".r 
  def string: Parser[String] = """"([^\n"]|\\")*"""".r
  def ident: Parser[String] = """[_a-zA-Z][_a-zA-Z0-9]*""".r
  def definition: Parser[String] = """[^\n]*""".r
  def defineP: Option[Parser[String]] = defines.map(_.parser).reduceOption(_|_)
  private def define: Parser[String] = """#define""".r ~> ident ~ (defArgs.?) ~ definition ^^ {
    case i ~ Some(a) ~ d => defines += new ArgDefine(i,a,d); s"// #define ${i}(${a.mkString(",")}) ${d}"
    case i ~ None ~ d => defines += new Define(i, d); s"// #define ${i} ${d}"
  }
  def program: Parser[String] = (string | comment | define | (defineP match {
    case None => "[^\\n]".r
    case Some(d) => d | "[^\\n]".r
  })).* ^^ { _.mkString }
}
