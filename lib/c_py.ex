defmodule C_Py.Helpers do
  import NimbleParsec

  def whitespace(combinator \\ empty()) do
    combinator |> ignore(repeat(choice([ascii_char(' '), ascii_char('\t'), ascii_char('\n')])))
  end
  def int_literal(combinator \\ empty()) do
    combinator |> choice([optional(ascii_char('-')) |> repeat(ascii_char('0')) |> choice([
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
  end

  def int_param(combinator \\ empty()) do
    combinator
    |> whitespace()
    |> string("int")
    |> whitespace()
    |> ascii_char([?a..?z] ++ [?A..?Z])
    |> repeat(ascii_char([?a..?z] ++ [?A..?Z] ++ [?0..?9]))
    |> whitespace()
  end

  def int_decl(combinator \\ empty()) do
    combinator
    |> int_param()
    |> whitespace()
    |> ascii_char('=')
    |> whitespace()
    |> int_literal()
    |> whitespace()
    |> ascii_char(';')
    |> whitespace()
  end

  def line(combinator \\ empty()) do
    combinator
    |> whitespace()
    |> ascii_char(';')
    |> whitespace()
  end

  def int_func(combinator \\ empty()) do
    combinator
    |> whitespace()
    |> string("int")
    |> whitespace()
    |> ignore(ascii_char('('))
    |> optional(repeat(whitespace() |> int_param() |> whitespace() |> ignore(ascii_char(',')) |> whitespace()) |> int_param())
    |> whitespace()
    |> ignore(ascii_char(')'))
    |> whitespace()
    |> ascii_char('{')
    |> ignore(repeat(ascii_char('\n')))
    |> repeat(line())
    |> ascii_char('}')
  end

  def program(combinator \\ empty()) do
    combinator
    |> whitespace()
    |> repeat(choice([int_func(), line()]) |> whitespace())
  end

  def boolean_expression(combinator \\ empty()) do
    combinator
  end

  def if_statement(combinator \\ empty()) do
    combinator
    |> ignore(string("if") |> whitespace() |> ascii_char('(') |> whitespace())
    |> boolean_expression()
    |> ignore(whitespace() |> ascii_char('{'))
    |> repeat(line())
    |> ignore(ascii_char('}'))
    |> whitespace()
  end

end
defmodule C_Py do
  import NimbleParsec
  import C_Py.Helpers
  defparsec(:int_literal, int_literal())
  defparsec(:int_decl, int_decl())
end
