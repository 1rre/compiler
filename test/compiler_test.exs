defmodule CompilerTest do
  use ExUnit.Case

  test "greets the world" do
    import Compiler
    intDecl("int x = 1424;") |> IO.puts()
  end
end
