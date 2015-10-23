:- module(facts,
   [val/3,
    set/4,
    add/4,
    fsr/1,
    del/1,
    jel/1,
    add/1,
    jdd/1,
    out/1,
    err/1,
    jog/1,
    now/1,
    set/2,
    rll/2,
    jll/2]).

/*----------------------------------------------------------------------------*
 * Define A Feature Structure Record
 *----------------------------------------------------------------------------*/
:- dynamic fsr/1.

/*----------------------------------------------------------------------------*
 * Current System Time
 *----------------------------------------------------------------------------*/
:- dynamic now/1.

now(0).

/*----------------------------------------------------------------------------*
 * Check if a feature structure has a certain value on the specified path of the
 * feature structure. The feature structure must not be instantiated before the
 * predicate is called. The feature term as well as the value term can be free
 * variables. In this case the variables are bound to all the possible solutions
 * for the feature or the value, respectively.
 *----------------------------------------------------------------------------*/
val(F:P, V, [F:M|_]) :-
  val(P, V, M).
val(F, V, [F:V|_]).
val(F, V, [_|T]) :-
  val(F, V, T).

/*----------------------------------------------------------------------------*
 * Set The Values Of All Paths That Match The Given Path In The Feature Record
 *----------------------------------------------------------------------------*/
set(F:P, V, [F:[E|M]|T], [F:R|L]) :-
  !, set(P, V, [E|M], R),
     set(F:P, V, T, L).
set(F, V, [F:_|T], [F:V|R]) :-
  !, set(F, V, T, R).
set(_, _, [], []) :-
  !.
set(F, V, [H|T], [H|R]) :-
  !, set(F, V, T, R).

/*----------------------------------------------------------------------------*
 * Add The Given Value At A Certain Feature Path To The Given Feature Structure
 *----------------------------------------------------------------------------*/
add(F:P, V, [], [F:M]) :-
  !, add(P, V, [], M).
add(F, V, [], [F:V]) :-
  !.
add(F, V, [H|T], [H|R]) :-
  !, add(F, V, T, R).

/*----------------------------------------------------------------------------*
 * Retract A Feature Structure Record
 *----------------------------------------------------------------------------*/
del(R) :-
  retract(fsr(R)),
  out('Retracting:\n'),
  out(R). % Retract The Fact

/*----------------------------------------------------------------------------*
 * Retract A Feature Structure Record
 *----------------------------------------------------------------------------*/
jel(R) :-
  retract(fsr(R)),
  jog('Retracting:\n'),
  jog(R). % Retract The Fact

/*----------------------------------------------------------------------------*
 * Assert A Feature Structure Record
 *----------------------------------------------------------------------------*/
add(R) :-
  assertz(fsr(R)),
  out('Asserting:\n'),
  out(R), !. % Assertz The Fact

add(R) :-
  out('Cannot Assert:\n'),
  out(R). % Print Information

/*----------------------------------------------------------------------------*
 * Assert A Feature Structure Record
 *----------------------------------------------------------------------------*/
jdd(R) :-
  assertz(fsr(R)),
  jog('Asserting:\n'),
  jog(R), !. % Assertz The Fact

jdd(R) :-
  jog('Cannot Assert:\n'),
  jog(R). % Print Information

/*----------------------------------------------------------------------------*
 * Variable Assignment
 *----------------------------------------------------------------------------*/
set(V, V).

/*----------------------------------------------------------------------------*
 * Retract All With Features
 *----------------------------------------------------------------------------*/
rll(P, V) :-
  forall((fsr(R), val(P, V, R)), del(R)).

/*----------------------------------------------------------------------------*
 * Retract With Features
 *----------------------------------------------------------------------------*/
jll(P, V) :-
  forall((fsr(R), val(P, V, R)), jel(R)).

/*----------------------------------------------------------------------------*
 * Print Feature Structure To Prolog Console
 *----------------------------------------------------------------------------*/
out(R) :- % Print To Stdout
  vwrite(R, '', user_output).

err(R) :- % Print To Stderr
  vwrite(R, '', user_error).

/*----------------------------------------------------------------------------*
 * Print Feature Structure To Java Logger
 *----------------------------------------------------------------------------*/

vwrite(V, _, S) :- % Print A Variable Value
  var(V), !, write(S, V).

vwrite([H|T], I, S) :- % Print A Matrix Value
  !,
  write(S, '['), nl(S),
  string_concat(I, '    ', J),
  lwrite([H|T], J, S),
  nl(S), write(S, I), write(S, ']').

vwrite(V, _, S) :- % Print A Simple Value
  !, write(S, V).

lwrite([H], I, S) :- % Print A Whole List
  !, ewrite(H, I, S).

lwrite([H|T], I, S) :- % Print A List Member
  !,
  ewrite(H, I, S),
  write(S, ','), nl(S),
  lwrite(T, I, S).

ewrite(F:V, I, S) :- % Print A Pair Member
  !,
  write(S, I),
  write(S, F),
  write(S, ':'),
  vwrite(V, I, S).

ewrite(E, I, S) :- % Print Simple Member
  !,
  write(S, I),
  vwrite(E, I, S).

/*----------------------------------------------------------------------------*
 * Print Feature Structure To Java Logger
 *----------------------------------------------------------------------------*/
jog(R) :-
   jvwrite(R, '', '', M),
   jpl_call('de.dfki.vsm.util.log.LOGDefaultLogger', getInstance, [], L),
   jpl_call(L, message, [M], _).

/*----------------------------------------------------------------------------*
 * Print Feature Structure To Java Logger
 *----------------------------------------------------------------------------*/

jvwrite(V, _, O, N) :- % Print A Variable Value
  var(V), !, concat(O, V, N).

jvwrite([H|T], I, O, N) :- % Print A Matrix Value
  !,
  concat(I, '    ', J),
  concat(O, '[\n', Z1),
  %concat(Z1, 'X', Z2),
  jlwrite([H|T], J, Z1, Z2),
  concat(Z2, '\n', Z3),
  concat(Z3, I, Z4),
  concat(Z4, ']', N).

jvwrite(V, _, O, N) :- % Print A Simple Value
  !, concat(O, V, N).

jlwrite([H], I, O, N) :- % Print A Whole List
  !, jewrite(H, I, O, N).

jlwrite([H|T], I, O, N) :- % Print A List Member
  !,
  jewrite(H, I, O, Z1),
  concat(Z1, ',', Z2),
  concat(Z2, '\n', Z3),
  jlwrite(T, I, Z3, N).

jewrite(F:V, I, O, N) :- % Print A Pair Member
  !,
  concat(O, I, Z1),
  concat(Z1, F, Z2),
  concat(Z2, ':', Z3),
  jvwrite(V, I, Z3, N).

jewrite(E, I, O, N) :- % Print Simple Member
  !,
  concat(O, I, Z1),
  jvwrite(E, I, Z1, N).
  
/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/