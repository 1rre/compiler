defmodule Compiler do
  import NimbleParsec
  defparsec(:ws, ignore(repeat(string(" "))))
  defparsec(:intDecl,
    ignore(string("int ")) |>
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
end
