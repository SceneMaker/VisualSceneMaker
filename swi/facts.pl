/*  This code is part of the Behavior Flow Query Language based on SWI-Prolog.

    Author:        Gregor Mehlmann
    E-mail:        mehlmann@hcm-lab.de
    WWW:           http://www.hcm-lab.de

    Copyright (C): 2008-2018,
                   University of Augsburg
                   Applied Computer Sciences
                   Human Centered Multimedia

    This program is free software; you can redistribute it and/or modify it
    under the terms of the 'GNU General Public License' as published by the
    Free Software Foundation, version 2 or any later version of the License.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY and without even the implied warranty
        of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    You should have received a copy of the GNU General Public License along
    with this library; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

    As an exception, if you link this library with other files, compiled with
    a Free Software compiler, to produce an executable, this library does not
    by itself cause the resulting executable to be covered by the GNU General
    Public License. This exception does not invalidate any other reasons why
    the executable file might be covered by the 'GNU General Public License'.
*/
:- module(facts,
  [
    fsr/1,
    now/1,
    val/3,
    add/5,
    del/1,
    jel/1,
    add/1,
    jdd/1,
    rll/2,
    jll/2
  ]).
  
:- reexport('terms').
:- reexport('print').

/*----------------------------------------------------------------------------*
 * The current system time
 *----------------------------------------------------------------------------*/
:- dynamic now/1.
:- assertz(now(0)).

/*----------------------------------------------------------------------------*
 * A feature structure record
 *----------------------------------------------------------------------------*/
:- dynamic fsr/1.

/*----------------------------------------------------------------------------*

  val(?F:path, ?V:value, ?R:record) is nondet

  This predicate checks if a feature structure has a certain value on the
  specified path of the feature structure. The feature structure may be a
  partially instantiated term containing free variables when the predicate
  is called. The predicate takes care not to produce an infinite number of
  solutions for the feature structure record and the interpreter cannot run
  into any stack errors. The feature term as well as the value term can be
  unbound variables. In this case the variables are bound to the possible
  solutions for the feature and/or the value, respectively, that means the
  predicate call unifies the value and/or the path with substitutions so
  that the given feature structure has the value at the given feature path.

 *----------------------------------------------------------------------------*/
val(F, V, [F:V|_]) :-
    fkeyterm(F).
val(F:P, V, [F:M|_]) :-
    \+allvarls([F, P, M]),
    val(P, V, M).
val(F, V, [_|T]) :-
    nonvar(T),
    val(F, V, T).

/*----------------------------------------------------------------------------*

  add(?P:path, ?V:value, ?I:record, ?O:record, ?C:integer) is nondet

  This predicate adds a certain feature value =|V|= to a specific feature
  path =|P|= in the specified feature structure =|I|= and returns the new
  feature structure =|O|= as result. The feature value, path and also the
  structures may be partially instantiated terms containing free variables
  when the predicate is called. The predicate takes care not to produce an
  infinite number of solutions for the value, path or the feature structure
  record and the interpreter cannot run into any stack errors. The feature
  value term as well as the feature path term can be unbound variables. In
  this case the variables are bound to the possible solutions for the value
  and/or the path, respectively, that means the predicate call unifies the
  value and/or the path with substitutions so that the new feature structure
  has the value at the given feature path. This predicate only works if the
  feature value pair is added to the end of the enclosing feature record in
  the input record.

 *----------------------------------------------------------------------------*/
add(P, V, I, O) :- add_(P, V, I, O, _, '').

add(P, V, I, O, C) :- add_(P, V, I, O, C).

addl(P, I, O) :-
    O = [P|I], !.
addl(P, [H|I], [H|O]) :-
    addl(P, I, O), !.
addl(P, [], [P]).

add_(F, V, I, O, C) :-
    fkeyterm(F), !,
    fvalterm(V),
    fvlsterm(I),
    addl(F:V, I, O), C is 1.
add_(P, V, I, O, C) :-
    fklsterm(P), P = F:Q,
    I = [F:R|M], O = [F:S|N], !,
    add_(Q, V, R, S, K),
    (\+allvarls([M, N])
     -> add_(P, V, M, N, L)
      ; M = [], N = [], L is 0
    ), C is K + L.
add_(P, V, I, O, C) :-
    fklsterm(P),
    I = [H|M], O = [H|N], !,
    (\+allvarls([M, N])
     -> add_(P, V, M, N, K)
      ; M = [], N = [], K is 0
    ), C is K.
add_(P, _, I, O, C) :-
    fklsterm(P),
    I = [], O = [], C is 0.

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
  jvw(R, M),
  concat('Retracting:\n', M, O),
  %jog('Retracting:\n'),
  jog(O). % Retract The Fact

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
  jvw(R, M),
  concat('Asserting:\n', M, O),
  %jog('Asserting:\n'),
  jog(O), !. % Assertz The Fact

jdd(R) :-
  jog('Cannot Assert:\n'),
  jog(R). % Print Information

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
 *
 *----------------------------------------------------------------------------*/