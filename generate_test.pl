:- ensure_loaded(library(lists)).

:- set_prolog_flag(optimise, true).
:- set_prolog_flag(optimise_unify, true).

% Generate one million closed simply-typed lambda calculus expressions and one
% million typable SK-combinator calculus expressions together with their types.
%
% Based on Paul Tarau's paper A Hiking Trip Through the Orders of Magnitude:
% Deriving Efficient Generators for Closed Simply-Typed Lambda Terms and Normal
% Forms.
%
% https://arxiv.org/pdf/1608.03912.pdf

n2s(0, z) :- !.
n2s(N, s(X)) :- N > 0, N0 is N - 1, n2s(N0, X).

down(s(X), X).

% Generate simply-typed lambda calculus expressions of a given size.
lambda(N, X, A) :- n2s(N, S), lambda(_, X, A, [], S, z).

lambda(v(X : A), v(X), A, Ctx) -->
  { member(X : A0, Ctx), unify_with_occurs_check(A0, A) }.
lambda(l(X : A, Y), l(X, NewY), A -> B, Ctx) -->
  down,
  lambda(Y, NewY, B, [X : A | Ctx]).
lambda(a(X, Y), a(NewX, NewY), B, Ctx) -->
  down,
  lambda(X, NewX, A -> B, Ctx),
  lambda(Y, NewY, A, Ctx).

% Generate typable SK-combinator calculus expression of a given size.
sk(N, X, A) :- n2s(N, S), sk(X, _, A, S, z).

sk(s, s, (A -> B -> C) -> (A -> B) -> A -> C) --> [].
sk(k, k, A -> _B -> A) --> [].
sk(a(X, Y), a(NewX : (A -> B), NewY : A), B) -->
  down,
  sk(X, NewX, A -> B),
  sk(Y, NewY, A0),
  { unify_with_occurs_check(A0, A) }.

% Pretty print an expression.
pretty(X) :-
  numbervars(X, 0, _),
  pretty(X, Xs, []),
  maplist(write, Xs),
  nl.

pretty(s) --> [s].
pretty(k) --> [k].
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

% Print all simply-typed lambda calculus expressions of a given size together
% with their types.
show_lambda(N) :-
  lambda(N, X, A),
  pretty(X),
  pretty_type(A),
  fail.
show_lambda(_).

% Print all typable SK-combinator calculus expressions of a given size together
% with their types.
show_sk(N) :-
  sk(N, X, A),
  pretty(X),
  pretty_type(A),
  fail.
show_sk(_).

main :-
  between(1, 9, N),
  show_lambda(N),
  fail.
main :-
  between(0, 8, N),
  show_sk(N),
  fail.
main.
