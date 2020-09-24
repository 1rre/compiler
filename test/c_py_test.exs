defmodule C_PyTest do
  use ExUnit.Case
  doctest C_Py

  test "greets the world" do
    assert C_Py.hello() == :world
  end
end
