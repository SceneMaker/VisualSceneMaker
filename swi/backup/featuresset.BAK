/** <module> Feature Structure Records

This library provides the manipulation operator set/4 on feature structure
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
:- module(featuresset,
  [
    set/4
  ]).

:- reexport('thedebugger').
:- reexport('inspections').

/**

  set(?F:path, ?V:value, ?I:record, ?O:record) is nondet
  
*/

set(F, V, I, O) :-
    is_verbose(set), !,
    set_(F, V, I, O, '').
set(F, V, I, O) :- set_(F, V, I, O).


% If the feature is a key, then try to find the feature in current level
% and if we find it, then we replace the value and return with success.
%If the

set_(F, V, I, O) :-
    validkey(F), !, write('set0'),
    set0(F, V, I, O).
set_(Q, V, I, O) :-  write('set1'),
    set1(Q, V, I, O).


set0(F, V, [F:M|T], [F:V|R]) :- !, set0(F, V, T, R).
set0(F, V, [H|T], [H|R]) :- nonvar(T), set0(F, V, T, R).
set0(_, _, [], []).

% This is set but we want set all on a list
% (which possible is always true or returns the number of sets)

set1(Q, V, I, O) :-
    Q = F:P, I = [F:M|T], O = [F:R|S], \+allvarls([F, P, M]),
    set1(P, V, M, R),
    set1(Q, V, T, S).


/*
set_(F, V, [F:_|T], [F:V|T]) :-
    validkey(F).
set_(Q, V, [F:M|T], [F:R|T]) :-
    Q = F:P, \+allvarls([F, P, M]),
    set_(P, V, M, R).
set_(F, V, [H|T], [H|R]) :-
    nonvar(T),
    set_(F, V, T, R).
*/

/*----------------------------------------------------------------------------*
 *                            TEST SUITE
 *----------------------------------------------------------------------------*/
/*
:- begin_tests(featuresset).

:- end_tests(featuresset).
*/
/*----------------------------------------------------------------------------*
 *                       DEPRECATED VERSIONS
 *----------------------------------------------------------------------------*
set(F:P, V, [F:[E|M]|T], [F:R|L]) :-
  !, set(P, V, [E|M], R),
     set(F:P, V, T, L).
set(F, V, [F:_|T], [F:V|R]) :-
  !, set(F, V, T, R).
set(_, _, [], []) :-
  !.
set(F, V, [H|T], [H|R]) :-
  !, set(F, V, T, R).
*-----------------------------------------------------------------------------*/