:- use_module(library(pio)).

append([], Xs, Xs).
append([E|Es], Xs, [E|Fs]) :- append(Es, Xs, Fs).

% Simulates a round of the game with Cs being player 1's deck
% and Ds being player 2's deck
round([C|Cs], [D|Ds], Es, Ds) :-
    C > D, !,
    append(Cs, [C, D], Es).
round([C|Cs], [D|Ds], Cs, Es) :-
    C =< D, !,
    append(Ds, [D, C], Es).

% Simulates the game until one of the decks is empty.
game([], Ds, Ds) :- !.
game(Cs, [], Cs) :- !.
game([C|Cs], [D|Ds], Ws) :-
    round([C|Cs], [D|Ds], Es, Fs),
    % print([Es, Fs]), nl,
    game(Es, Fs, Ws).

score_impl([X], 1, X) :- !.
score_impl([X|Xs], M, Z) :-
    score_impl(Xs, N, Y),
    M is N + 1,
    Z is Y + (X * M).

score(Xs, Y) :- score_impl(Xs, _, Y).

game_score(Cs, Ds, Y) :-
    game(Cs, Ds, Ws),
    score(Ws, Y).

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

