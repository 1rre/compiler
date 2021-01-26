package es.tmoor.compiler

import parsers.DefineParser

object Main extends App with DefineParser {
  val input = io.Source.fromFile(args(0)).getLines.toArray

  input.mapInPlace(line => {
    ws = true
    val r0 = parse(define, line)
    ws = false
    if (r0.isEmpty) defines.foldLeft(line)((acc, v) => parse(v.parser, acc).getOrElse(acc))
    else line
  })

  //println(defines.head.parser)

  input.foreach(println(_))

}