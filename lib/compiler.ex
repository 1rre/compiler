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

  defparsecp(:keyword,
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

  defp escape_char(args) do
    case args do
      ?a -> 0x07
      ?b -> 0x08
      ?e -> 0x1B
      ?f -> 0x0C
      ?n -> 0x0A
      ?r -> 0x0D
      ?t -> 0x09
      ?v -> 0x0B
      ?\\-> 0x5C
      ?' -> 0x27
      ?" -> 0x22
      ?? -> 0x3F
    end
  end
  defparsecp(:charLiteral,
    ignore(string("'")) |>
    choice([
      ascii_char([?\0..?&, ?(..?[, ?]..127]),
      ignore(string("\\")) |> choice([
        ascii_char([?a, ?b, ?e, ?f, ?n, ?r, ?t, ?v, ?\\, ?', ?", ??]) |> map({:escape_char, []}), #TODO: Implement escape chars \a..\?
        ascii_string([?0..?7], min: 1, max: 3) |> map({String, :to_integer, [8]}),
        ignore(string("x")) |> ascii_string([?0..?9, ?a..?f, ?A..?F], 2) |> map({String, :to_integer, [16]}),
        ignore(string("u")) |> ascii_string([?0..?9, ?a..?f, ?A..?F], 4) |> map({String, :to_integer, [16]})
      ])
    ]) |>
    ignore(string("'"))
  )

  defparsecp(:intLiteral,
    choice([
      integer(min: 1),
      string("0") |> choice([
        ascii_string([?0..?7], min: 0) |> map({String, :to_integer, [8]}),
        ignore(string("x")) |> ascii_string([?0..?9, ?a..?f, ?A..?F], min: 1) |> map({String, :to_integer, [16]})
      ])
    ])

  )

  defp process_float(args) do
    case Enum.at(args, 1) do
      69 when length(args) == 3 -> Enum.at(args, 0) * :math.pow(10, Enum.at(args, 2))
      101 when length(args) == 3 -> Enum.at(args, 0) * :math.pow(10, Enum.at(args, 2))
      69 -> Enum.at(args, 0) * :math.pow(10, -Enum.at(args, 3))
      101 -> Enum.at(args, 0) * :math.pow(10, -Enum.at(args, 3))
      46 -> [to_string(Enum.at(args, 0)), ".", to_string(Enum.at(args, 2))] |> Enum.join() |> String.to_float()
    end
  end
  defparsecp(:floatLiteral,
    integer(min: 1) |>
    choice([
      ascii_char([69, 101]) |>
        optional(ascii_char([45])),
      ascii_char([46])
    ]) |>
    integer(min: 1) |>
    reduce({:process_float, []})
  )

  defp fix_minus(args) do
    if Enum.at(args, 0) == 45, do: -(Enum.at(args, 1)), else: Enum.at(args, 0)
  end
  defparsec(:literal,
    optional(ascii_char([45])) |>
    choice([
      parsec(:charLiteral),
      parsec(:floatLiteral),
      parsec(:intLiteral)
    ]) |>
    reduce({:fix_minus, []})
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
    parsec(:param) |>
    optional(
      string("=") |>
      parsec(:literal)
    ) |>
    ignore(string(";"))
  )

  defparsec(:assignment,
    parsec(:keyword) |>
    ignore(string("=")) |>
    parsec(:literal)
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
    string("}")
  )
  defp closing_bracket(<<?}, _::binary>>, context, _, _), do: {:halt, context}
  defp closing_bracket(_, context, _, _), do: {:cont, context}

end
