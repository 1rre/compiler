defmodule CompilerTest do
  use ExUnit.Case

  test "test 1" do
    import Compiler
    identifier("xcx21") |> IO.puts()
  end

  test "test 2" do
    import Compiler
    keyword("unsigned long int") |> IO.puts()
  end
end
