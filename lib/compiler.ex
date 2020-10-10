defmodule Compiler do
  import NimbleParsec

  defparsecp(:ws, ignore(repeat(string(" "))))

  defparsecp(:charKw, string("char"))

  defparsecp(:uCharKw,
    string("unsigned") |>
    parsec(:ws) |>
    string("char") |>
    replace("uChar")
  )

  defparsecp(:sCharKw,
    string("signed") |>
    parsec(:ws) |>
    string("char") |>
    replace("sChar")
  )

  defparsecp(:shortKw,
    optional(
      string("signed") |>
      parsec(:ws)
    ) |>
    string("short") |>
    optional(
      parsec(:ws)
      |> string("int")
    ) |>
    replace("short")
  )

  defparsecp(:uShortKw,
    string("unsigned") |>
    parsec(:ws) |>
    string("short")
    |> optional(
      parsec(:ws) |>
      string("int")
    ) |>
    replace("uShort")
  )

  defparsecp(:intKw,
    choice([
      string("int"),
      string("signed") |>
        parsec(:ws) |>
        string("int"),
      string("signed")
    ]) |>
    replace("int")
  )

  defparsecp(:uIntKw,
    string("unsigned") |>
    optional(
      parsec(:ws) |>
      string("int")
    ) |>
    replace("uInt")
  )

  defparsecp(:longKw,
    optional(
      string("signed") |>
      parsec(:ws)
    ) |>
    string("long") |>
    optional(
      parsec(:ws) |>
      string("int")
    ) |>
    replace("long")
  )
  defparsecp(:uLongKw,
    string("unsigned") |>
    parsec(:ws) |>
    string("long") |>
    optional(string("int")) |>
    replace("uLong")
  )

  #TODO: unsigned int, long, uLong, long long, uLong long, floats

  defparsec(:keyword,
    choice([
      parsec(:uCharKw),
      parsec(:sCharKw),
      parsec(:charKw),
      parsec(:shortKw),
      parsec(:uShortKw),
      parsec(:uLongKw),
      parsec(:longKw),
      parsec(:uIntKw),
      parsec(:intKw)
    ]) |>
    parsec(:ws)
  )

  defparsec(:identifier,
    ascii_string([?a..?z, ?A..?Z], min: 1) |> ascii_string([?a..?z, ?A..?Z, ?0..?9], min: 0)
  )

  defparsec(:decl,
    parsec(:keyword) |>
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
