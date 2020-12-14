-module(day14).
-export([main/1]).
-mode(compile).

-record(machine, {mask=error, tape=dict:new()}).

parse_instruction(Raw) ->
    Split = lists:map(fun string:trim/1, string:split(Raw, "=")),
    case Split of
        ["mask", Rhs]          -> {mask, Rhs};
        ["mem" ++ Access, Rhs] -> 
            {N, _} = string:to_integer(string:slice(Access, 1, string:length(Access) - 2)),
            {X, _} = string:to_integer(Rhs),
            {mem, N, X}
    end.

parse_program(Raw) ->
    lists:map(fun(RawInst) -> parse_instruction(RawInst) end, string:tokens(Raw, "\n")).

apply_mask(Mask, X) ->
    {OrMask, _} = string:to_integer(string:replace(Mask, "X", "0")),
    {AndMask, _} = string:to_integer(string:replace(Mask, "X", "1")),
    (X band AndMask) bor OrMask.

run_instruction(Inst, Machine) ->
    case Inst of
        {mask, Rhs}   -> Machine#machine{mask=Rhs};
        {mem, N, X} ->
            #machine{mask=Mask, tape=Tape} = Machine,
            Y = apply_mask(Mask, X),
            io:fwrite("At ~B writing ~B~n", [N, X]),
            Tape2 = dict:store(N, Y, Tape),
            Machine#machine{tape=Tape2}
    end.

run_program([], Machine) -> Machine;
run_program([Inst|Insts], Machine) ->
    Machine2 = run_instruction(Inst, Machine),
    run_program(Insts, Machine2).

main(_) ->
    {ok, BinInput} = file:read_file("resources/input.txt"),
    Input = unicode:characters_to_list(BinInput),
    Prog = parse_program(Input),
    Machine = #machine{},
    Machine2 = run_program(Prog, Machine),
    Part1 = lists:sum(lists:map(fun ({_, X}) -> X end, dict:to_list(Machine2#machine.tape))),
    io:fwrite("Part 1: ~B~n", [Part1]).
