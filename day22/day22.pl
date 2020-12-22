:- use_module(library(pio)).

take(N, _, Xs) :-
    N =< 0, !, N =:= 0,
    Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :-
    M is N - 1,
    take(M, Xs, Ys).

sort_desc(Xs, Zs) :-
    sort(Xs, Ys),
    reverse(Ys, Zs).

% The Game of Combat (pt. 1)

combat_round([C|Cs], [D|Ds], Es, Ds) :-
    C > D, !,
    append(Cs, [C, D], Es).
combat_round([C|Cs], [D|Ds], Cs, Es) :-
    C =< D, !,
    append(Ds, [D, C], Es).

combat_game([], Ds, Ds) :- !.
combat_game(Cs, [], Cs) :- !.
combat_game([C|Cs], [D|Ds], Ws) :-
    combat_round([C|Cs], [D|Ds], Es, Fs),
    combat_game(Es, Fs, Ws).

% The Game of Recursive Combat (pt. 2)

update_decks([C|Cs], [D|Ds], 0, Es, Ds) :-
    sort_desc([C, D], Zs),
    append(Cs, Zs, Es).
update_decks([C|Cs], [D|Ds], 1, Cs, Es) :-
    sort_desc([C, D], Zs),
    append(Ds, Zs, Es).

recursive_combat_round(Cs, Ds, Ps1, Ps2, Cs, []) :-
    (member(Cs, Ps1); member(Ds, Ps2)), !.
recursive_combat_round([C|Cs], [D|Ds], _, _, Es, Fs) :-
    length(Cs, CL),
    length(Ds, DL),
    CL >= C, DL >= D, !,
    take(C, Cs, Es),
    take(D, Ds, Fs),
    recursive_combat_game(Es, Fs, [], _, _, W),
    update_decks([C|Cs], [D|Ds], W, Es, Fs).
recursive_combat_round([C|Cs], [D|Ds], _, _, Es, Ds) :-
    C >= D, !,
    append(Cs, [C, D], Es).
recursive_combat_round([C|Cs], [D|Ds], _, _, Cs, Es) :-
    C >= D, !,
    append(Ds, [D, C], Es).

recursive_combat_game([], Ds, _, _, Ds, 1) :- !.
recursive_combat_game(Cs, [], _, _, Cs, 0) :- !.
recursive_combat_game([C|Cs], [D|Ds], Ps1, Ps2, Ws, W) :-
    recursive_combat_round([C|Cs], [D|Ds], Ps1, Ps2, Es, Fs),
    recursive_combat_game(Es, Fs, [Es|Ps1], [Fs|Ps2], Ws, W).

% Game scores

score_impl([X], 1, X) :- !.
score_impl([X|Xs], M, Z) :-
    score_impl(Xs, N, Y),
    M is N + 1,
    Z is Y + (X * M).

score(Xs, Y) :- score_impl(Xs, _, Y).

% DCG for parsing the input

decks([])     --> call(eos), !.
decks([D|Ds]) --> deck(D), decks(Ds).

deck(Vs) --> line(_), call(num_lines(Vs)).

num_lines([])     --> line([]), !.
num_lines([N|Ns]) --> call(num_line(N)), num_lines(Ns). 

num_line(N, I, J) :-
    line(L, I, J),
    number_codes(N, L).

line([])     --> ("\n"; call(eos)), !.
line([C|Cs]) --> [C], line(Cs).

eos([], []).

% Main program

parse_input(Ds) :-
    phrase_from_file(decks(Ds), 'resources/input.txt').

main :-
    parse_input([Cs, Ds]),

    combat_game(Cs, Ds, Ws1),
    score(Ws1, Score1),
    print(['Part 1', Score1]), nl,

    recursive_combat_game(Cs, Ds, [], [], Ws2, _),
    score(Ws2, Score2),
    print(['Part 2', Score2]), nl.
