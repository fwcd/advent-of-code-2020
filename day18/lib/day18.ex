defmodule Day18 do
  @moduledoc """
  Solution for day 18 of AoC 2020 in Elixir.
  """

  def interpret_expr(toks) do
    IO.puts "#{toks}"
    case toks do
      [x] ->
        {n, _} = Integer.parse(x)
        n
      [x, op | ts] ->
        {n, _} = Integer.parse(x)
        case op do
          "+" -> n + interpret_expr(ts)
          "-" -> n - interpret_expr(ts)
          "*" -> n * interpret_expr(ts)
          "/" -> n / interpret_expr(ts)
          _   -> exit "Invalid operator: #{op}"
        end
        _ -> exit "Invalid tokens: #{toks}"
    end
  end

  def main do
    input = File.read!("resources/input.txt") |> String.split("\n")
    input |> Enum.map(fn(l) -> l |> String.split(" ") |> interpret_expr end)
  end
end
