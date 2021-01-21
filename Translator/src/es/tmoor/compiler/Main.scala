package es.tmoor.compiler

object Main extends App with Preprocessor {
  val input = io.Source.fromFile("test/test.c")
  test
  println(parse(defineP, "hello;"))
}