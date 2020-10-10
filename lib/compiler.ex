defmodule Compiler do
  import NimbleParsec

  defparsec(:ws, ignore(repeat(string(" "))))

  defparsec(:charKw, string("char"))

  defparsec(:uCharKw,
    string("unsigned") |>
    parsec(:ws) |>
    string("char")
  )

  defparsec(:sCharKw,
    string("signed") |>
    parsec(:ws) |>
    string("char")
  )

  defparsec(:shortKw,
    optional(
      string("signed") |>
      parsec(:ws)
    ) |>
    string("short") |>
    optional(
      parsec(:ws)
      |> string("int")
    )
  )

  defparsec(:uShortKw,
    string("unsigned") |>
    parsec(:ws) |>
    string("short")
    |> optional(
      parsec(:ws) |>
      string("int")
    )
  )

  defparsec(:intKw,
    choice([
      string("int"),
      string("signed") |>
        parsec(:ws) |>
        string("int"),
      string("signed")
    ])
  )

  #TODO: unsigned int, long, uLong, long long, uLong long, floats

  defparsec(:keyword,
    choice([
      parsec(:charKw),
      parsec(:uCharKw),
      parsec(:sCharKw),
      parsec(:shortKw),
      parsec(:uShortKw),
      parsec(:intKw)
    ])
  )

  defparsec(:decl,
    string("int ") |>
    parsec(:ws) |>
    ascii_string([?a..?z, ?A..?Z], min: 1) |>
    optional(
      parsec(:ws) |>
      string("=") |>
      parsec(:ws) |>
      integer(min: 1, max: 8)
    ) |>
    parsec(:ws) |>
    ignore(string(";"))
  )

  defparsec(:intFunc,
    ignore(string("int "))
  )

end
