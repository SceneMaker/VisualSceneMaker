/** <module> Feature Structure Records

This library provides the manipulation operator val/3 on feature structure
records, also known as typed feature structures or attribute value matrices.

This library has been developed as part of the _|Behavior Flow Query Language|_
at the lab for _|Human Centered Multimedia|_ of the Faculty of Applied Computer
Sciences at the University of Augburg. Please see _| http://www.hcm-lab.de /|_
for more information. The library has been developed by Gregor Mehlmann and is
published under the GNU General Public License.

@author Gregor Mehlmann
@copyright 2008-2018,
  University of Augsburg,
  Applied Computer Sciences,
  Human Centered Multimedia.
@license GNU General Public License.
*/
:- module(featuresval,
  [
    val/3
  ]).

:- reexport('thedebugger').
:- reexport('inspections').

/**

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
  
*/

val(F, V, R) :-
    is_verbose(val), !,
    val_(F, V, R, '').
val(F, V, R) :-
    val_(F, V, R).


val_(F, V, [F:V|_]) :-
    fkeyterm(F).
val_(F:P, V, [F:M|_]) :-
    \+allvarls([F, P, M]),
    val_(P, V, M).
val_(F, V, [_|T]) :-
    nonvar(T),
    val_(F, V, T).
  

/* TODO:
  Make a type save / instantiation save version
*/

/*----------------------------------------------------------------------------*
 *                           VERBOSE VERSION
 *----------------------------------------------------------------------------*/
val_(F, V, R, S) :-
write(S),
write('val('),
write(F),
write(', '),
write(V),
write(', '),
write(R),
write('):'),
nl, %
    validkey(F), R = [F:V|_],
write(S),
write('  -> matching '),
nl.

val_(Q, V, R, S) :-
write(S),
write('val('),
write(Q),
write(', '),
write(V),
write(', '),
write(R),
write('):'),
nl, %
    Q = F:P, R = [F:M|_], \+allvarls([F, P, M]),
write(S),
write('  -> descending '),
nl,
atom_concat(S, '  ', SS),
    %
    val_(P, V, M, SS).

val_(F, V, [H|T], S) :- % look at the rest of the list
write(S),
write('val('),
write(F),
write(', '),
write(V),
write(', '),
write([H|T]),
write('):'),
nl, %
    nonvar(T),
write('  -> proceeding '),
nl,
atom_concat(S, '  ', SS),
    %
    val_(F, V, T, SS).

/*----------------------------------------------------------------------------*
 *                            TEST SUITE
 *----------------------------------------------------------------------------*/
/*
:- begin_tests(featuresval).

test(val) :-
  findall((X, Y, Z), val(X, Y, Z), L), write(L).
test(val) :-
  findall((X, Y, Z), val(X, Y, [Z]), L), write(L).
test(val) :-
  findall((X, Y, A, B), val(X, Y, [A,B]), L), write(L).
test(val) :-
  findall((X, Y, A, B), val(X, Y, [A|B]), L), write(L).
test(val) :-
  findall((X, Y), val(X, Y, [f1:v1, F2:v2, f3:V3, F4:V4]), L), write(L).
test(val) :-
  findall((X, Y), val(X, Y, [F5:[f6:v6, F7:v7, f8:V8, F9:V9, M10]]), L), write(L).
test(val) :-
  findall((X, Y), val(X:Y, v, [f1:v1, F2:v, f3:V3, F4:V4, f5:[ f6:v], F7:[ f8:v] ]), L), write(L).

:- end_tests(featuresval).
*/
/*----------------------------------------------------------------------------*
 *                       DEPRECATED VERSIONS
 *----------------------------------------------------------------------------*
This is the former version of val/3 which is deprecated since 01.04.2016.
It has been replaced to handle infinite searches when using unbound variables.

val(F:P, V, [F:M|_]) :-
  val(P, V, M).
val(F, V, [F:V|_]).
val(F, V, [_|T]) :-
  val(F, V, T).

val(F, V, [F:V|_]) :-
    validkey(F).
val(Q, V, [F:M|_]) :-
    \+allvarls([Q, M]), Q = F:P, val(P, V, M).
val(F, V, [_|T]) :-
    nonvar(T), val(F, V, T).
*-----------------------------------------------------------------------------*/