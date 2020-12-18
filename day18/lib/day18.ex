defmodule Day18 do
  @moduledoc """
  Solution for day 18 of AoC 2020 in Elixir.
  """

  require IEx

  def span(list, p) do
    case list do
      [] -> {[], []}
      [x | xs] ->
        if p.(x) do
          {as, bs} = span(xs, p)
          {[x | as], bs}
        else
          {[], list}
        end
    end
  end

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

  def interpret_expr(toks) do
    IO.puts "#{toks |> Enum.join(", ")}"
    case toks do
      ["(" | rest1] ->
        IO.puts "Spannin'"
        {ts, rest2} = parse_params(rest1)
        n = interpret_expr(ts)
        interpret_expr([n | rest2])
      [x] ->
        {n, _} = Integer.parse(x)
        n
      [x, op | ts] ->
        IEx.pry
        {n, _} = Integer.parse(x)
        IO.puts "Pars'd"
        case op do
          "+" -> n + interpret_expr(ts)
          "-" -> n - interpret_expr(ts)
          "*" -> n * interpret_expr(ts)
          "/" -> n / interpret_expr(ts)
          _   -> exit "Invalid operator: #{op}"
        end
    end
  end

  def main do
    input = File.read!("resources/input.txt") |> String.split("\n")
    input |> Enum.map(fn l -> l |> tokenize |> interpret_expr end)
  end
end
