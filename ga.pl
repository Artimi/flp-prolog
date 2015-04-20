elliptic(X, Y):- length(X, D), elliptic(X, 1, D, 0, Y).
elliptic([X|XS], I, D, Acc, Y):-
    I1 is I + 1,
    Acc1 is Acc + 1000000**((I - 1)/(D - 1)) * X * X,
    elliptic(XS, I1, D, Acc1, Y).
elliptic([], _, _,Acc, Y):- Y is Acc, !.

rastrigin(X, Y):- rastrigin(X, 0, Y).
rastrigin([X|XS],  Acc, Y) :-
    Acc1 is Acc + X * X - 10 * cos(2 * pi * X) + 10,
    rastrigin(XS, Acc1, Y).
rastrigin([], Acc, Y):- Y is Acc, !.

ackley(X, Y):- length(X, D), ackley(X, D, 0, 0, Y).
ackley([X|T], D, Xacc, Cacc, Y):-
    Xacc1 is Xacc + X * X,
    Cacc1 is Cacc + cos(2 * pi * X),
    ackley(T, D, Xacc1, Cacc1, Y).
ackley([], D, Xacc, Cacc, Y) :-
    Y is -20 * exp(-0.2 * sqrt((1/D) * Xacc)) - exp((1/D) * Cacc) + 20 + e, !.

schwefel(X, Y) :- schwefel(X, 0, 0, Y).
schwefel([X|XS], Xacc, Xpowacc, Y):-
    Xacc1 is Xacc + X,
    Xpowacc1 is Xpowacc + Xacc1 * Xacc1,
    schwefel(XS, Xacc1, Xpowacc1, Y).
schwefel([], _, Xpowacc, Y):- Y is Xpowacc, !.

rosenbrock([X|XS], Y) :- rosenbrock(XS, X, 0, Y).
rosenbrock([X|XS], X1, Acc, Y):-
    Acc1 is Acc + 100 * (X**2  - X1)**2 + (X - 1)**2,
    rosenbrock(XS, X, Acc1, Y).
rosenbrock([], _, Acc1, Y):- Y is Acc1, !.

dim(2).

generate_random_float(X):- random(0, 10000, Y), X is Y / 10000.

generate_individual(X) :- dim(Dim), generate_individual(Dim, [], X).
generate_individual(0, Y, Y):- !.
generate_individual(D, Y, X):-
    generate_random_float(RF),
    R is (RF - 0.5) * 2 * 10,
    D1 is D - 1,
    generate_individual(D1, [R| Y], X).

generate_population(X) :- generate_population(50, [], X).
generate_population(0, X, X):- !.
generate_population(N, Y, X) :-
    generate_individual(I),
    N1 is N -1,
    generate_population(N1, [I| Y], X).

evaluate_population(Pop, Func, Val):- evaluate_population(Pop, Func, Val, []).
evaluate_population([], _, Val, Val):- !.
evaluate_population([X|XS], Func, Val, A):- call(Func, X, Y), evaluate_population(XS, Func, Val, [Y|A]).

sort_population(Pop, Func, S) :-
    evaluate_population(Pop, Func, Eval),
    pairs_keys_values(SS, Eval, Pop),
    keysort(SS, SSS),
    pairs_values(SSS, S).

select_parent(Pop, P):-
    length(Pop, Size),
    random(0, Size, P1),
    random(0, Size, P2),
    random(0, Size, P3),
    P4 is min(P1, P2),
    Pindex is min(P3, P4),
    nth0(Pindex, Pop, P).

crossover([], [], []).
crossover([M|MM], [_|FF], [M|CH]):- maybe, crossover(MM, FF, CH), !.
crossover([_|MM], [F|FF], [F|CH]):- crossover(MM, FF, CH).

crossover_population(Pop, Cross, N):- crossover_population(Pop, Cross, N, []).
crossover_population(_, Cross, 0, Cross):- !.
crossover_population(Pop, Cross, N, Y):-
    select_parent(Pop, M),
    select_parent(Pop, F),
    crossover(M, F, C),
    N1 is N - 1,
    crossover_population(Pop, Cross, N1, [C|Y]).

mutate([], []).
mutate([_|II], [R|RR]) :-
    dim(Dim),
    generate_random_float(RFMUT),
    MutP is 1/Dim,
    RFMUT < MutP,
    generate_random_float(RF),
    R is (RF - 0.5) * 2 * 10, mutate(II, RR), !.
mutate([I|II], [I|RR]) :- mutate(II, RR).

mutate_population(Pop, Mut, N) :- mutate_population(Pop, Mut, N, []).
mutate_population(_, Mut, 0, Mut):- !.
mutate_population(Pop, Mut, N, Y):-
    random_member(M, Pop),
    mutate(M, R),
    N1 is N -1,
    mutate_population(Pop, Mut, N1, [R|Y]).

taken(X, N, Y):- length(Y, N), append(Y, _, X).

% assert Pop is sorted
step(Pop, Func, NewPop):-
    taken(Pop, 10, Best),
    crossover_population(Pop, Cross, 30),
    mutate_population(Pop, Mut, 10),
    append(Cross, Mut, G),
    append(Best, G, N),
    sort_population(N, Func, NewPop).

run(Func, Result) :-
    generate_population(Pop),
    sort_population(Pop, Func, Sorted),
    run(Func, Result, Sorted, 0).
run(Func, Best, [Best|_], _) :-
    good_enough(Best, Func), !.
run(_, Best, [Best|_], 10000):- !.
run(Func, Result, Pop, Iteration) :-
    Iteration1 is Iteration + 1,
    step(Pop, Func, NewPop),
    run(Func, Result, NewPop, Iteration1).

good_enough(Individual, Func) :-
    call(Func, Individual, Val),
    Val < 1e-4.
