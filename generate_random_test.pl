:- ensure_loaded(library(apply)).
:- ensure_loaded(library(random)).

:- set_prolog_flag(optimise, true).
:- set_prolog_flag(optimise_unify, true).

% Randomly generate 1,000 simply-typed lambda calculus terms using Boltzmann
% samplers.
%
% Based on the paper Random generation of closed simply-typed λ-terms: a
% synergy between logic programming and Boltzmann samplers
%
% https://arxiv.org/pdf/1612.07682.pdf

% Note that there are multiple definitions for the size of a lambda term. In
% particular, the definition implicitly used here is different from the one
% used in `generate_test.pl`.
min_size(120).
boltzmann_variable(R) :- R < 0.3569605561766718.
boltzmann_variable_zero(R) :- R < 0.7044187641738427.
boltzmann_lambda(R) :- R < 0.6525417920028290.

next(R, Size1, Size2) :-
  random(R),
  Size2 is Size1 + 1.

random_lambda(X, A) :-
  random(R),
  random_lambda(X, A, [], R, 0, Size),
  min_size(MinSize),
  Size >= MinSize,
  !.
random_lambda(X, A) :- random_lambda(X, A).

random_lambda(X, A, Ctx, R) -->
  { boltzmann_variable(R), !, random(NewR) },
  random_variable(X, A, Ctx, NewR).
random_lambda(l(X), A -> B, Ctx, R) -->
  { boltzmann_lambda(R), ! },
  next(NewR),
  random_lambda(X, B, [A|Ctx], NewR).
random_lambda(a(X, Y), B, Ctx, _) -->
  next(R1),
  random_lambda(X, A -> B, Ctx, R1),
  next(R2),
  random_lambda(Y, A, Ctx, R2).

random_variable(z, A0, [A|_], R) -->
  { boltzmann_variable_zero(R), !, unify_with_occurs_check(A0, A) }.
random_variable(s(X), A, [_|Ctx], _) -->
  next(R),
  random_variable(X, A, Ctx, R).

multithreaded_random_lambda(X, A) :-
  prolog_flag(cpu_count, MaxThreads0),
  MaxThreads is MaxThreads0 - 1,
  Goal = random_lambda(X, A),
  length(Goals, MaxThreads),
  maplist(=(Goal), Goals),
  first_solution([X, A], Goals, []).

convert(X, NewX) :- convert(X, NewX, []).

convert(z, v(X), [X|_]).
convert(s(X), NewX, [_|Ctx]) :- convert(X, NewX, Ctx).
convert(l(Y), l(X, NewY), Ctx) :- convert(Y, NewY, [X|Ctx]).
convert(a(X, Y), a(NewX, NewY), Ctx) :-
  convert(X, NewX, Ctx),
  convert(Y, NewY, Ctx).

% Pretty print a term.
pretty(X0) :-
  convert(X0, X),
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

main :-
  between(1, 1000, _),
  multithreaded_random_lambda(X, A),
  pretty(X),
  pretty_type(A),
  fail.
main.
