-module(day14).
-export([main/1]).

-record(machine, {mask=error, tape=#{}}).

parse_instruction(Raw) ->
    case lists:map(fun string:trim/1, string:split(Raw, "=")) of
        ["mask", Rhs]                  -> {mask, Rhs};
        ["mem" ++ [$[, RawN, $]], Rhs] -> {mem, string:to_integer(RawN), Rhs}
    end.

parse_program(Raw) ->
    string:split(Raw, "\n").

apply_mask(Mask, X) ->
    OrMask = string:to_integer(string:replace(Mask, "X", "0")),
    AndMask = string:to_integer(string:replace(Mask, "X", "1")),
    (X band AndMask) bor OrMask.

run_instruction(Inst, Machine) ->
    case parse_instruction(inst) of
        {mask, Rhs}   -> Machine#machine{mask=Rhs};
        {mem, N, Rhs} ->
            Tape = Machine#machine.tape,
            Machine#machine{tape=Tape}
    end.

run_program([], Machine) -> Machine;
run_program([RawInst|RawInsts], Machine) ->
    Inst = parse_instruction(RawInst),
    Machine2 = run_instruction(Inst, Machine),
    run_program(RawInsts, Machine2).

main(_) ->
    {ok, Input} = file:read_file("resources/input.txt"),
    Prog = parse_program(Input),
    Machine = #machine{},
    Machine2 = run_program(Prog, Machine),
    Part1 = lists:sum(lists:map(fun ({_, X}) -> X end, Machine2#machine.tape)),
    io:fwrite("Part 1: ~B", [Part1]).
