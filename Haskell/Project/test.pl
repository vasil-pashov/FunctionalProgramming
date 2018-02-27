p(x, y).
p(X, Y) :- q(X), z(Y).

q(x).
q(y).

z(g).

c(x).
c(f(X)) :- c(X).