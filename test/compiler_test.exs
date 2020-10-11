defmodule CompilerTest do
  use ExUnit.Case
  import Compiler

  def is_ok(x) do
    assert(elem(x, 0) == :ok)
  end

  test "identifier" do
    identifier("xcx21") |> is_ok()
  end

  test "ulong" do
    keyword("unsigned long int") |> is_ok()
  end

  test "int function" do
    IO.puts("")
    x = intFunc("int x (int j, int k) {
      int x = 3;
    }" |> format()) |> elem(1)
    "params:\n#{
      Enum.drop_while(x, fn y ->
        y != "("
      end) |>
      tl() |>
      Enum.take_while(fn y ->
        y != ")"
      end) |>
      Enum.map(fn y ->
        if y == ",", do: "\n", else: y
      end) |> Enum.join()
    }\n\ncode lines:\n#{
        Enum.drop_while(x, fn y ->
        y != "{"
      end) |>
      tl() |>
      Enum.take_while(fn y ->
        y != "}"
      end) |>
      Enum.map(fn y ->
        if y == ";", do: ";\n", else: y
      end) |> Enum.join()
    }" |> IO.puts
  end

  test "format" do
    format("int x (int j, int k) {
      int x = 3;
      x += 5;
      cout<<x<<endl;
      return x;
    }")
  end
end
