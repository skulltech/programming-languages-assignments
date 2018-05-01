% Helper predicates.
prefix(A, B) :- append(A, W, B).
suffix(A, B) :- append(W, A, B).
substring(A, B) :- append(W, A, Z), append(Z, Y, B).

member(X, []) :- fail.
member(X, [X|Ls]).
member(X, [Y|Ls]) :- member(X, Ls).

append([], L2, L2).
append([X|Xs], L2, [X|L3]) :- append(Xs, L2, L3).
