defmodule Day18 do
@moduledoc """
Solution for day 18 of AoC 2020 in Elixir.
"""

def tokenize(line) do
  Regex.split(~r/\b|\s+|(?=[\(\)])/, line) |> Enum.filter(fn l -> String.trim(l) != "" end)
end

def parse_params(toks, acc \\ [], i \\ 1) do
  if i == 0 do
    [_ | inner] = acc
    {Enum.reverse(inner), toks}
  else
      case toks do
        [] -> exit "Unmatched parentheses!"
        ["(" | ts] -> parse_params(ts, ["(" | acc], i + 1)
        [")" | ts] -> parse_params(ts, [")" | acc], i - 1)
        [t   | ts] -> parse_params(ts, [t | acc], i)
      end
    end
  end

  def parse_primary(toks) do
    case toks do
      ["(" | rest1] ->
        {ts, rest2} = parse_params(rest1)
        n = interpret_expr(ts)
        {n, rest2}
      [x | ts] ->
        {n, _} = Integer.parse(x)
        {n, ts}
      [] -> :error
    end
  end

  def interpret_expr(toks) do
    case parse_primary toks do
      {n, []} -> n
      {n, [op | ts]} ->
        case parse_primary ts do
          {m, ts2} ->
            res = case op do
              "+" -> n + m
              "*" -> n * m
              _   -> exit "Invalid operator: #{op}"
            end
            interpret_expr(["#{res}" | ts2])
        end
        
    end
  end

  def part2_parenthesize(raw) do
    # Use a clever trick to apply the correct precedences
    inner = raw
          |> String.replace("*", " ) ) * ( ( ")
          |> String.replace("+", " ) + ( ")
    "( ( #{inner} ) )"
  end

  def main do
    input = File.read!("resources/input.txt")
          |> String.split("\n")
          |> Enum.filter(fn l -> String.trim(l) != "" end)

    part1 = input
          |> Enum.map(&tokenize/1)
          |> Enum.map(&interpret_expr/1)
          |> Enum.sum
    IO.puts "Part 1: #{part1}"

    part2 = input
          |> Enum.map(&part2_parenthesize/1)
          |> Enum.map(&tokenize/1)
          |> Enum.map(&interpret_expr/1)
          |> Enum.sum
    IO.puts "Part 2: #{part2}"
  end
end
