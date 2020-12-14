-module(day14).
-export([main/1, floating_addresses/1]).
-mode(compile).

-record(machine, {mask=error, tape=dict:new()}).

replace([], _, _) -> [];
replace([E|Es], E, G) -> [G|replace(Es, E, G)];
replace([E|Es], F, G) -> [E|replace(Es, F, G)].

enumerate(Es) -> lists:zip(lists:seq(0, length(Es) - 1), Es).

parse_instruction(Raw) ->
    Split = lists:map(fun string:trim/1, string:split(Raw, "=")),
    case Split of
        ["mask", Rhs]          -> {mask, Rhs};
        ["mem" ++ Access, Rhs] -> 
            N = list_to_integer(string:slice(Access, 1, string:length(Access) - 2)),
            X = list_to_integer(Rhs),
            {mem, N, X}
    end.

parse_program(Raw) ->
    lists:map(fun(RawInst) -> parse_instruction(RawInst) end, string:tokens(Raw, "\n")).

set_memory(N, X, Machine) ->
    io:fwrite("memory[~B] <- ~B~n", [N, X]),
    Tape = Machine#machine.tape,
    Tape2 = dict:store(N, X, Tape),
    Machine#machine{tape=Tape2}.

run_instruction(Inst, Machine, AccessHandler) ->
    case Inst of
        {mask, Rhs} -> Machine#machine{mask=Rhs};
        {mem, N, X} ->
            Mask = Machine#machine.mask,
            Accesses = AccessHandler(Mask, N, X),
            lists:foldr(fun({M, Y}, Machine2) -> set_memory(M, Y, Machine2) end, Machine, Accesses)
    end.

run_program([], Machine, _) -> Machine;
run_program([Inst|Insts], Machine, AccessHandler) ->
    Machine2 = run_instruction(Inst, Machine, AccessHandler),
    run_program(Insts, Machine2, AccessHandler).

sum_memory(Machine) ->
    lists:sum(lists:map(fun({_, X}) -> X end, dict:to_list(Machine#machine.tape))).

apply_part1_mask(Mask, X) ->
    OrMask = list_to_integer(replace(Mask, $X, $0), 2),
    AndMask = list_to_integer(replace(Mask, $X, $1), 2),
    (X band AndMask) bor OrMask.

floating_raw_addresses(Acc, []) -> [lists:reverse(Acc)];
floating_raw_addresses(Acc, [B|Bs]) ->
    case B of
        $X -> floating_raw_addresses(Acc, [$0|Bs]) ++ floating_raw_addresses(Acc, [$1|Bs]);
        _ -> floating_raw_addresses([B|Acc], Bs)
    end.

floating_addresses(Mask) ->
    RawAddrs = floating_raw_addresses("", Mask),
    % io:fwrite("Addrs: [~s]~n", [lists:join(", ", RawAddrs)]),
    lists:map(fun(R) -> list_to_integer(R, 2) end, RawAddrs).

decode_part2_mask(Mask, N) ->
    BitCount = length(Mask),
    lists:map(fun({I, B}) ->
        case B of
            $0 ->
                Bit = (N bsr (BitCount - 1 - I)) band 1,
                [B2|_] = integer_to_list(Bit),
                B2;
            _ -> B
        end
    end, enumerate(Mask)).

part1_handler(Mask, N, X) ->
    [{N, apply_part1_mask(Mask, X)}].

part2_handler(Mask, N, X) ->
    Mask2 = decode_part2_mask(Mask, N),
    lists:map(fun(M) -> {M, X} end, floating_addresses(Mask2)).

main(_) ->
    {ok, BinInput} = file:read_file("resources/example2.txt"),
    Input = unicode:characters_to_list(BinInput),
    Prog = parse_program(Input),
    Machine = #machine{},

    Part1 = sum_memory(run_program(Prog, Machine, fun part1_handler/3)),
    io:fwrite("Part 1: ~B~n", [Part1]),

    Part2 = sum_memory(run_program(Prog, Machine, fun part2_handler/3)),
    io:fwrite("Part 2: ~B~n", [Part2]).
