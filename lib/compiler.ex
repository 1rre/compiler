defmodule Compiler do
  import NimbleParsec

  #Function to tell if a letter is either a..z or A..Z
  defp is_letter(x), do: x > 64 && x < 91 || x > 96 && x < 123 || x > 47 && x < 58 || x == 95

  #Replace each non-space newline character in the input file with a space then replace multiple spaces with a single space.
  def format(s) do
    String.replace(s, ~r/(\n | \t | \v | \f | \r)+/, " ") |>
    String.replace(~r/(\ )+/, " ") |>
    String.replace(~r/(\ )./, fn <<_, a>> ->
      if is_letter(a) do
        List.to_string([' ', a])
      else
        List.to_string([a])
      end
    end) |>
    String.replace(~r/.(\ )/, fn <<c, _>> ->
      if is_letter(c) do
        List.to_string([c, ' '])
      else
        List.to_string([c])
      end
    end)
  end

  defparsecp(:ws, ignore(string(" ")))

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
    ])
  )

  defparsec(:identifier,
    ascii_string([?_, ?a..?z, ?A..?Z], min: 1) |>
    ascii_string([?_, ?a..?z, ?A..?Z, ?0..?9], min: 0)
  )

  defparsec(:param,
    parsec(:keyword) |>
    parsec(:ws) |>
    parsec(:identifier)
  )

  defparsec(:decl,
    parsec(:keyword) |>
    parsec(:ws) |>
    ascii_string([?a..?z, ?A..?Z], min: 1) |>
    optional(
      string("=") |>
      integer(min: 1, max: 8)
    ) |>
    ignore(string(";"))
  )

  defparsec(:line,
    parsec(:decl)
  )

  defparsec(:intFunc,
    parsec(:param) |>
    string("(") |>
    optional(
      repeat(
        parsec(:param) |>
        string(",")
      ) |>
      parsec(:param)
    ) |>
    string(")") |>
    string("{") |>
    repeat_while(parsec(:line), {:closing_bracket, []}) |>
    string("}"),
    debug: true)
  defp closing_bracket(<<?}, _::binary>>, context, _, _), do: {:halt, context}
  defp closing_bracket(_, context, _, _), do: {:cont, context}

end
