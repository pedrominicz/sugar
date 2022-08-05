:- ensure_loaded(library(aggregate)).
:- ensure_loaded(library(lists)).

% Generate over one million closed simply-typed lambda calculus expressions
% together with their types.
%
% Based on Paul Tarau's paper A Hiking Trip Through the Orders of Magnitude:
% Deriving Efficient Generators for Closed Simply-Typed Lambda Terms and Normal
% Forms.
%
% https://arxiv.org/pdf/1608.03912.pdf

% Note that there are multiple definitions for the size of a lambda expression.
% In particular, this definition is different from the one in Jue Wang's paper
% Generating Random Lambda Calculus Terms.
size(v(_), 0).
size(l(_, X), S) :- size(X, S0), S is S0 + 1.
size(a(X, Y), S) :- size(X, S0), size(Y, S1), S is S0 + S1 + 1.

n2s(0, z) :- !.
n2s(N, s(X)) :- N > 0, N0 is N - 1, n2s(N0, X).

down(s(X), X).

% Generate simply-typed lambda expressions of a given size and their types.
lambda(N, X, T) :- n2s(N, S), lambda(_, X, T, [], S, z).

lambda(v(X : A), v(X), A, Ctx) -->
  { member(X : A0, Ctx), unify_with_occurs_check(A0, A) }.
lambda(l(X : A, Y), l(X, NewY), A -> B, Ctx) -->
  down,
  lambda(Y, NewY, B, [X : A | Ctx]).
lambda(a(X, Y), a(NewX, NewY), B, Ctx) -->
  down,
  lambda(X, NewX, A -> B, Ctx),
  lambda(Y, NewY, A, Ctx).

% Count the number of simply-typed lambda expressions of a given size.
count(N, Count) :-
  aggregate_all(count, lambda(N, _, _), Count).

% Print the counts of simply-typed lambda expressions of at most a given size.
%
%   ?- counts(10).
%   1: 1
%   2: 2
%   3: 9
%   4: 40
%   5: 238
%   6: 1564
%   7: 11807
%   8: 98529
%   9: 904318
%   10: 9006364
%   true.
%
counts(Max) :-
  between(1, Max, N),
  count(N, Count),
  format('~d: ~d~n', [N, Count]),
  fail.
counts(_).

% Pretty print an expression.
pretty(X) :-
  numbervars(X, 0, _),
  pretty(X, Xs, []),
  maplist(write, Xs),
  nl.

pretty(v('$VAR'(I))) --> [x, I].
pretty(l('$VAR'(I), X)) --> ['(\\', x, I, ' -> '], pretty(X), [')'].
pretty(a(X, Y)) --> ['('], pretty(X), [' '], pretty(Y), [')'].

% Pretty print a type.
pretty_type(A) :-
  numbervars(A, 0, _),
  pretty_type(A, As, []),
  maplist(write, As),
  nl.

pretty_type(A -> B) -->
  ['('], pretty_type(A), !, [' -> '], pretty_type(B), [')'].
pretty_type(A) --> [A].

% Print all simply-typed lambda expressions of a given size together with their
% types.
show(N) :-
  lambda(N, X, T),
  pretty(X),
  pretty_type(T),
  fail.
show(_).

main :-
  between(1, 9, N),
  show(N),
  fail.
main.
