defmodule CompilerTest do
  use ExUnit.Case
  import Compiler

  def is_ok(x) do
    assert(elem(x, 0) == :ok)
  end

  test "int function" do
    intFunc("int x (int j, int k) {
      int x = 3;
    }" |> format()) |> is_ok()

    # "params:\n#{
    #   Enum.drop_while(x, fn y ->
    #     y != "("
    #   end) |>
    #   tl() |>
    #   Enum.take_while(fn y ->
    #     y != ")"
    #   end) |>
    #   Enum.map(fn y ->
    #     if y == ",", do: "\n", else: y
    #   end) |> Enum.join()
    # }\n\ncode lines:\n#{
    #     Enum.drop_while(x, fn y ->
    #     y != "{"
    #   end) |>
    #   tl() |>
    #   Enum.take_while(fn y ->
    #     y != "}"
    #   end) |>
    #   Enum.map(fn y ->
    #     if y == ";", do: ";\n", else: y
    #   end) |> Enum.join()
    # }"
  end

  test "a" do
    literal("'\\n'" |> format) |> is_ok()
  end
end
