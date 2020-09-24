defmodule C_Py do
  import NimbleParsec

  defparsecp(:whitespace, ignore(repeat(choice([ascii_char(' '), ascii_char('\t'), ascii_char('\n')]))))

  defparsecp(:int_literal, choice([optional(ascii_char('-')) |> repeat(ascii_char('0')) |> choice([
      optional(ascii_char([?0..?1])) |> integer(max: 9),
      ascii_char('2') |> choice([
        ascii_char('0') |> integer(8),
        ascii_char('1') |> choice([
          ascii_char([?0..?3]) |> integer(7),
          ascii_char('4') |> choice([
            ascii_char([?0..?6]) |> integer(6),
            ascii_char('7') |> choice([
              ascii_char([?0..?3]) |> integer(5),
              ascii_char('4') |> choice([
                ascii_char([?0..?7]) |> integer(4),
                ascii_char('8') |> choice([
                  ascii_char([?0..?2]) |> integer(3),
                  ascii_char('3') |> choice([
                    ascii_char([?0..?5]) |> integer(2),
                    ascii_char('6') |> choice([
                      ascii_char([?0..?3]) |> integer(1),
                      ascii_char('4') |> ascii_char([?0..?7])
                    ])
                  ])
                ])
              ])
            ])
          ])
        ])
      ])
    ]), ascii_char('-') |> repeat(ascii_char('0')) |> string("2147483648")])
  )

  defparsecp(:variable_name,
    choice([ascii_char('_'), ascii_char([?a..?z] ++ [?A..?Z])])
    |> repeat(choice([ascii_char('_'), ascii_char([?a..?z] ++ [?A..?Z] ++ [?0..?9])]))
  )

  defparsecp(:int,
    parsec(:whitespace)
    |> string("int")
    |> parsec(:whitespace)
    |> parsec(:variable_name)
    |> parsec(:whitespace)
  )

  defparsecp(:int_decl,
    parsec(:whitespace)
    |> parsec(:variable_name)
    |> parsec(:whitespace)
    |> ascii_char('=')
    |> parsec(:whitespace)
    |> parsec(:int_literal)
    |> parsec(:whitespace)
    |> ascii_char(';')
    |> parsec(:whitespace)
  )

  defparsecp(:operation,
    parsec(:variable_name)
  )

  defparsecp(:declaration,
    ignore(string("int"))
    |> parsec(:whitespace)
    |> parsec(:variable_name)
    |> ignore(ascii_char(';'))
  )

  defparsecp(:assignment,
    parsec(:declaration)
    |> parsec(:whitespace)
    |> ascii_char('=')
    |> parsec(:whitespace)
    |> choice([parsec(:int_literal), parsec(:operation)])
    |> parsec(:whitespace)
    |> ascii_char(';')
    |> parsec(:whitespace)
  )

  defparsecp(:instruction,
    parsec(:whitespace)
    |> ascii_char(';')
    |> parsec(:whitespace)
  )

  defparsecp(:code_block,
    parsec(:whitespace)
    |> ignore(ascii_char('{'))
    |> repeat(parsec(:instruction))
    |> ignore(ascii_char('}'))
    |> parsec(:whitespace)
  )

  defparsecp(:int_func,
    parsec(:whitespace)
    |> string("int")
    |> parsec(:whitespace)
    |> ignore(ascii_char('('))
    |> optional(repeat(parsec(:whitespace) |> parsec(:int) |> parsec(:whitespace) |> ignore(ascii_char(',')) |> parsec(:whitespace)) |> parsec(:int))
    |> parsec(:whitespace)
    |> ignore(ascii_char(')'))
    |> parsec(:code_block)
  )

  defparsecp(:comparison,
    choice([parsec(:variable_name), parsec(:int_literal)])
    |> parsec(:whitespace)
    |> choice([string("=="), string(">"), string("<"), string(">="), string("<=")])
    |> parsec(:whitespace)
    |> choice([parsec(:variable_name), parsec(:int_literal)])
  )

  defparsecp(:boolean_expression,
    ignore(repeat(ascii_char('') |> eventually(ascii_char(''))))
    |> parsec(:comparison)
    |> repeat(choice([string("&&"), string("||")]) |> parsec(:boolean_expression))
    |> ignore(optional(ascii_char('')))
  )

  defparsecp(:if_statement,
    ignore(string("if") |> parsec(:whitespace) |> ascii_char('(') |> parsec(:whitespace))
    |> choice([parsec(:boolean_expression), string("true"), string("false")])
    |> ignore(parsec(:whitespace) |> ascii_char(')'))
    |> parsec(:code_block)
  )

  defparsec(:if_else_statement,
    parsec(:if_statement)
    |> repeat(string("else") |> parsec(:if_statement))
    |> optional(string("else") |> parsec(:code_block))
  )

  defparsec(:for_loop,
    ignore(string("for") |> parsec(:whitespace) |> ascii_char('('))
    |> parsec(:instruction)
    |> parsec(:boolean_expression)
    |> ignore(ascii_char(';'))
    |> parsec(:operation)
  )



  defparsec(:program,
    parsec(:whitespace)
    |> repeat(choice([parsec(:int_func), parsec(:instruction)]) |> parsec(:whitespace))
  )

end
