defmodule Day18 do
  @moduledoc """
  Solution for day 18 of AoC 2020 in Elixir.
  """

  def main do
    input = File.read!("resources/input.txt") |> String.split("\n")
    input |> Enum.join(" ")
  end
end
