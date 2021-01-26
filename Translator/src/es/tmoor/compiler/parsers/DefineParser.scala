package es.tmoor.compiler.parsers

import scala.util.parsing.combinator.RegexParsers
import collection.mutable.HashSet

trait DefineParser extends RegexParsers with Utils {
  var ws = true
  override def skipWhitespace: Boolean = ws
  class Define(name: String, definition: String) {
    def parser = ((name ^^ { _ => definition }) | """[^\n]""".r).+ ^^ { _.mkString }
    override def toString = name
  }

  class ArgDefine(name: String, args: List[String], definition: String) extends Define(name, definition) {
    private def replace(l: List[String]) = args.mkString("|").r.replaceAllIn(definition, _.matched match {
      case m: String if m != "" => l(args.indexOf(m))
      case _ => ""
    })

    private def list = {
      ws = true
      val l = "(" ~> ((("""[^\n,)]+""".r | string) <~ ",").* ~ ("""[^\n)]+""".r | string)).? <~ ")" ^^ {
        case Some(x ~ y) => x :+ y
        case None => List()
      }
      ws = false
      l
    }

    override def parser = ((name ~> list ^^ { replace(_)}) | """[^\n]""".r).+ ^^ { _.mkString}
  }

  val defines = HashSet[Define]()
  
  def defArgs: Parser[List[String]] = "(" ~> (((ident <~ ",").* ~ ident).?) ~ ")" ^^ {
    case Some(a) ~ _ => a._1 :+ a._2
    case None ~ _ => List()
  }

  def define: Parser[String] = "#" ~ "define" ~> ident ~ (defArgs.?) ~ definition ^^ {
    case i ~ Some(a) ~ d => defines += new ArgDefine(i,a,d); s"#define ${i}(${a.mkString(",")}) ${d}"
    case i ~ None ~ d => defines += new Define(i, d); s"#define ${i} ${d}"
  }

  def defineP: Parser[String] = (defines.map(_.parser).reduceOption((x,y) => (x|||y)) match {
    case Some(p) => {
      ("""[^\n]""".r ~ (whiteSpace.+ ~> p).?).* ^^ { _.toList.map(x => x._1 ++ x._2.getOrElse("")).mkString }
    }
    case None => """[^\n]*""".r
  })

  def definition: Parser[String] = """[^\n]*""".r

}
