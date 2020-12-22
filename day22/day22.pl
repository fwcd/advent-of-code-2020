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

scoreImpl([X], 1, X) :- !.
scoreImpl([X|Xs], M, Z) :-
    scoreImpl(Xs, N, Y),
    M is N + 1,
    Z is Y + (X * M).

score(Xs, Y) :- scoreImpl(Xs, _, Y).

part1(Cs, Ds, Y) :-
    game(Cs, Ds, Ws),
    score(Ws, Y).
