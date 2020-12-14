-module(day14).
-export([main/1]).

-record(machine, {mask=error, tape=dict:new()}).

parse_instruction(Raw) ->
    Split = lists:map(fun string:trim/1, string:split(Raw, "=")),
    case Split of
        ["mask", Rhs]                     -> {mask, Rhs};
        [[$m, $e, $m, $[, RawN, $]], Rhs] -> {mem, string:to_integer(RawN), string:to_integer(Rhs)}
    end.

parse_program(Raw) ->
    lists:map(fun(RawInst) -> parse_instruction(RawInst) end, string:tokens(Raw, "\n")).

apply_mask(Mask, X) ->
    OrMask = string:to_integer(string:replace(Mask, "X", "0")),
    AndMask = string:to_integer(string:replace(Mask, "X", "1")),
    (X band AndMask) bor OrMask.

run_instruction(Inst, Machine) ->
    case Inst of
        {mask, Rhs}   -> Machine#machine{mask=Rhs};
        {mem, N, Rhs} ->
            #machine{mask=Mask, tape=Tape} = Machine,
            Tape2 = dict:update(N, fun(_) -> apply_mask(Mask, Rhs) end, Tape),
            Machine#machine{tape=Tape2}
    end.

run_program([], Machine) -> Machine;
run_program([RawInst|RawInsts], Machine) ->
    Inst = parse_instruction(RawInst),
    Machine2 = run_instruction(Inst, Machine),
    run_program(RawInsts, Machine2).

main(_) ->
    {ok, BinInput} = file:read_file("resources/input.txt"),
    Input = unicode:characters_to_list(BinInput),
    Prog = parse_program(Input),
    Machine = #machine{},
    Machine2 = run_program(Prog, Machine),
    Part1 = lists:sum(lists:map(fun ({_, X}) -> X end, dict:to_list(Machine2#machine.tape))),
    io:fwrite("Part 1: ~B", [Part1]).
