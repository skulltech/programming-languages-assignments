% Helper predicates.
append([], L2, L2).
append([X|Xs], L2, [X|L3]) :- append(Xs, L2, L3).

member(X, []) :- fail.
member(X, [X|Ls]).
member(X, [Y|Ls]) :- member(X, Ls).

% Numerical and boolean constants.
hasType(G, E, intT) :- integer(E).
hasType(G, true, boolT).
hasType(G, false, boolT).

% Boolean operations over boolean expressions.
hasType(G, and(E1, E2), boolT) :- hasType(G, E1, boolT), hasType(G, E2, boolT).
hasType(G, or(E1, E2), boolT) :- hasType(G, E1, boolT), hasType(G, E2, boolT).
hasType(G, not(E), boolT) :- hasType(G, E, boolT).
hasType(G, implies(E1, E2), boolT) :- hasType(G, E1, boolT), hasType(G, E2, boolT).

% Arithmetic operations over numerical expressions.
hasType(G, add(E1, E2), intT) :- hasType(G, E1, intT), hasType(G, E2, intT).
hasType(G, sub(E1, E2), intT) :- hasType(G, E1, intT), hasType(G, E2, intT).
hasType(G, mul(E1, E2), intT) :- hasType(G, E1, intT), hasType(G, E2, intT).
hasType(G, div(E1, E2), intT) :- hasType(G, E1, intT), hasType(G, E2, intT).
hasType(G, mod(E1, E2), intT) :- hasType(G, E1, intT), hasType(G, E2, intT).
hasType(G, abs(E), intT) :- hasType(G, E, intT).

% Comparison operations over numerical expressions.
hasType(G, grt(E1, E2), boolT) :- hasType(G, E1, intT), hasType(G, E2, intT).
hasType(G, les(E1, E2), boolT) :- hasType(G, E1, intT), hasType(G, E2, intT).

% Equality over arbitrary expressions.
hasType(G, eql(E1, E2), boolT) :- hasType(G, E1, T), hasType(G, E2, T).

% Conditional expressions. If E1 then E2 else E3.
hasType(G, conditional(E1, E2, E3), T) :- 
							hasType(G, E1, boolT), 
							hasType(G, E2, T), 
							hasType(G, E3, T).

% Variables.
hasType(G, varT(X), T) :- member({X, T}, G).

% Abstraction.
hasType(G, function(X, E), arrowT(T1, T2)) :- append([{X, T1}], G, Gi), hasType(Gi, E, T2).
hasType(G, apply(A, Arg), T) :- hasType(G, A, arrowT(Ti, T)), hasType(G, Arg, Ti).

% Qualification
hasType(G, qualif(D, E), T) :- 
							typeElaborates(G, D, TE), 
							append(TE, G, Gi),
							hasType(Gi, E, T).

% Tuples and projection.
hasType(G, [], []) :- true.
hasType(G, [E|Es], [T|Ts]) :- hasType(G, E, T), hasType(G, Es, Ts).
hasType(G, proj(E, N), T) :- nth0(N, E, X), hasType(G, X, T).


% Definitions and type elaborations.
typeElaboratesOverlap(TE1, []) :- false.
typeElaboratesOverlap(TE1, [{X, Y}|Ls]) :- member({X, _}, TE1).
typeElaboratesOverlap(TE1, [X|Ls]) :- typeElaboratesOverlap(TE1, Ls).

typeElaborates(G, def(X, E), [{X, T}]) :- hasType(G, E, T).
typeElaborates(G, defSeql(D1, D2), TE) :- 
							typeElaborates(G, D1, TE1),
							append(TE1, G, Gi), 
							typeElaborates(Gi, D2, TE2), 
							append(TE1, TE2, TE).
typeElaborates(G, defParl(D1, D2), TE) :- 
							typeElaborates(G, D1, TE1), 
							typeElaborates(G, D2, TE2), 
							not(typeElaboratesOverlap(TE1, TE2)),
							append(TE1, TE2, TE).
typeElaborates(G, defLocl(D1, D2), TE) :- 
							typeElaborates(G, D1, TEi), 
							append(TEi, G, Gi),
							typeElaborates(Gi, D2, TE).


/* TESTCASES */
hasType([{"X", intT}], add(5, mul(varT("X"), 5)), T).
hasType([], and(true, or(true, false)), T).
hasType([], grt(6, 2), T).

hasType([{"X", intT}, {"Y", boolT}], conditional(varT("Y"), 2, varT("X")), T).

typeElaborates([{"X", boolT}], def("Y", varT("X")), TE).
typeElaborates([{"X", boolT}], defParl(def("Y", 5), def("Z", varT("X"))), TE).
typeElaborates([{"X", boolT}], defSeql(def("X", 5), def("Y", varT("X"))), TE).
typeElaborates([{"X", boolT}], defLocl(def("X", 5), def("Y", varT("X"))), TE).

hasType([{"Y", intT}], function("X", add(varT("X"), varT("Y"))), T).
hasType([{"Y", intT}], apply(function("X", add(varT("X"), varT("Y"))), 2), T).
hasType([], qualif(def("X", 3), add(varT("X"), 2)), T).

hasType([{"X", intT}], [true, varT("X")], T).
hasType([], proj([true, 5], 0), T).
