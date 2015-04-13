elliptic(X, Y):- length(X, D), elliptic(X, 1, D, 0, Y).
elliptic([X|XS], I, D, Acc, Y):-
    I1 is I + 1,
    Acc1 is Acc + 1000000**((I - 1)/(D - 1)) * X * X,
    elliptic(XS, I1, D, Acc1, Y).
elliptic([], _, _,Acc, Y):- Y is Acc.

rastrigin(X, Y):- rastrigin(X, 0, Y).
rastrigin([X|XS],  Acc, Y) :-
    Acc1 is Acc + X * X - 10 * cos(2 * pi * X) + 10,
    rastrigin(XS, Acc1, Y).
rastrigin([], Acc, Y):- Y is Acc.

ackley(X, Y):- length(X, D), ackley(X, D, 0, 0, Y).
ackley([X|T], D, Xacc, Cacc, Y):-
    Xacc1 is Xacc + X * X,
    Cacc1 is Cacc + cos(2 * pi * X),
    ackley(T, D, Xacc1, Cacc1, Y).
ackley([], D, Xacc, Cacc, Y) :-
    Y is -20 * exp(-0.2 * sqrt((1/D) * Xacc)) - exp((1/D) * Cacc) + 20 + e.

schwefel(X, Y) :- schwefel(X, 0, 0, Y).
schwefel([X|XS], Xacc, Xpowacc, Y):-
    Xacc1 is Xacc + X,
    Xpowacc1 is Xpowacc + Xacc1 * Xacc1,
    schwefel(XS, Xacc1, Xpowacc1, Y).
schwefel([], _, Xpowacc, Y):- Y is Xpowacc.

rosenbrock([X|XS], Y) :- rosenbrock(XS, X, 0, Y).
rosenbrock([X|XS], X1, Acc, Y):-
    Acc1 is Acc + 100 * (X**2  - X1)**2 + (X - 1)**2,
    rosenbrock(XS, X, Acc1, Y).
rosenbrock([], _, Acc1, Y):- Y is Acc1.
