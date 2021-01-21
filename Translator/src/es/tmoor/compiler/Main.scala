package es.tmoor.compiler

object Main extends App with Preprocessor {
  val input = io.Source.fromFile("test/test.c").getLines
//  parse(program, input.next)
//  println(defines.mkString(","))
//  println(parse(defineP.get, "console(\"hello\")"))
  
  println(input.map(ln => parse(program, ln).getOrElse(sys.error("Oh noes!"))).mkString("\n"))
}