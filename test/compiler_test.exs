defmodule CompilerTest do
  use ExUnit.Case

  test "test 1" do
    import Compiler
    keyword("signed int") |> IO.puts()
  end
end
