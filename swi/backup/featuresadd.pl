/** <module> Feature Structure Records

This library provides the manipulation operator add/4 on feature structure
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
:- module(featuresadd,
  [
    %add/4,
    add/5,
    addl/3
  ]).

:- reexport('thedebugger').
:- reexport('inspections').

/**

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

*/

/*
add(P, V, I, O) :-
    is_verbose(add), !,
    add_(P, V, I, O, _, '').
add(P, V, I, O) :- add_(P, V, I, O, _, '').
*/

add(P, V, I, O, C) :-
    is_verbose(add), !,
    add_(P, V, I, O, C, '').
add(P, V, I, O, C) :- add_(P, V, I, O, C).

/*----------------------------------------------------------------------------*
 *                           DEFAULT VERSION
 *----------------------------------------------------------------------------*/
% Todo:
% Splitte auf in ein add_value und add_path prädikat
% entscheide in add welche der beiden zu nehmen
% add_value produziert keine neuen records sondern kann nur ein feature
% value pair innerhalt eines records ablegen - vorzugsweise am ende oder anfang
% und gibt immer true zurück. Paar wird auch hineingelegt, wenn bereits das
% feature vorhanden ist, bzw. in einer type-saven version kommt eine warnung.
% add_path läuft über den record und steigt hinab wo immer das feature bereits
% vorhanden ist um dort wieder add aufzurüfen. Es gibt keine warnung wenn feature
% bereits vorhanden. Außerdem läuft es über die komplette liste und versucht das
% bis zum ende der liste. somit müssen add und add_path ebenfalls immer mit
% true zurückkehren.
% Eine extra add struct methode kann dann records erzeugen.
% Bei Variablen finden wir die 'Minimale Lösung' und bieten keinerlei choice
% Punkte bzw. es kommt zu keiner endlos - instantiierung von Lösungen!!!

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
 *                           VERBOSE VERSION
 *----------------------------------------------------------------------------*/
/*
add_(F, V, I, O, C, Z) :-
atom_concat(Z, '  ', Y), write(Z), write('add[0]('), write(F), write(', '), write(V), write(', '), write(I), write(', '), write(O), write(', '), write(C), write('):'), nl,

    fkeyterm(F), write(Z), write('  -> valid feature key '), write(F), nl,
    !,
    

    fvalterm(V), write(Z), write('  -> valid feature value '), write(V), nl,
    fvlsterm(I), write(Z), write('  -> valid feature list '), write(I), nl,
    
    %O = [F:V|I],
    addl(F:V, I, O),
    write(Z), write('  -> valid output record '), write(O), nl,

    C is 1.

%------------------------------------------------------------------------------%

add_(P, V, I, O, C, Z) :-
atom_concat(Z, '  ', Y), write(Z), write('add[1]('), write(P), write(', '), write(V), write(', '), write(I), write(', '), write(O), write(', '), write(C), write('):'), nl,

    %
    fklsterm(P), P = F:Q, write(Z), write('  -> valid feature path '), write(P), nl,
    
    I = [F:R|M], write(Z), write('  -> valid input record '), write([F:R|M]), nl,
    O = [F:S|N], write(Z), write('  -> valid output record '), write([F:S|N]), nl,
    !,

    write(Z), write('-> descending with add('), write(Q), write(', '), write(V), write(', '), write(R), write(', '), write(S), write(', '), write(K), write(')'), nl,
    add_(Q, V, R, S, K, Y),

    (
        \+allvarls([M, N])
    ->

        write(Z), write('-> continuing with add('), write(P), write(', '), write(V), write(', '), write(M), write(', '), write(N), write(', '), write(L), write(')'), nl,
         add_(P, V, M, N, L, Y)
    ;
        write(Z), write('we have only variables in tail'), nl,
        M = [], N = [], L is 0
    ),
    C is K + L.

%------------------------------------------------------------------------------%

add_(P, V, I, O, C, Z) :-
atom_concat(Z, '  ', Y), write(Z), write('add[2]('), write(P), write(', '), write(V), write(', '), write(I), write(', '), write(O), write(', '), write(C), write('):'), nl,

    %
    fklsterm(P), P = F:Q, write(Z), write('  -> valid feature path '), write(P), nl,
    
    I = [H|M], write(Z), write('  -> valid input record '), write([H|M]), nl,
    O = [H|N], write(Z), write('  -> valid output record '), write([H|N]), nl,

    !,

    (
        \+allvarls([M, N])
        
    ->
        write(Z), write('-> continuing with add('), write(P), write(', '), write(V), write(', '), write(M), write(', '), write(N), write(', '), write(K), write(')'), nl,
        add_(P, V, M, N, K, Y)
    ;
        write(Z), write('we have only variables in tail'), nl,
        M = [], N = [], K is 0
    ),
    C is K.

%------------------------------------------------------------------------------%

add_(P, V, I, O, C, Z) :-
atom_concat(Z, '  ', Y), write(Z), write('add[3]('), write(P), write(', '), write(V), write(', '), write(I), write(', '), write(O), write(', '), write(C), write('):'), nl,

    %
    fklsterm(P),
    %P = F:Q,
    write(Z), write('  -> valid feature path '), write(P), nl,
    
    I = [],  write(Z), write('  -> valid input record '), write(I), nl,
    O = [],  write(Z), write('  -> valid output record '), write(O), nl,
    C is 0.   %old
    %O = [F:R],   write(Z), write('  -> valid output record '), write(O), nl, %new
    %write(Z), write('-> inserting with add('), write(Q), write(', '), write(V), write(', '), write(I), write(', '), write(R), write(', '), write(C), write(')'), nl,  %new
    %add_(Q, V, I, R, C, Y),  write(Z), write('-> terminating '), nl.    %new


 */

/*----------------------------------------------------------------------------*
 *                            TEST SUITE
 *----------------------------------------------------------------------------*/
/*
:- begin_tests(featuresadd).
:- end_tests(featuresadd).
*/
/*----------------------------------------------------------------------------*
 * Add The Given Value At A Certain Feature Path To The Given Feature Structure
 *----------------------------------------------------------------------------*/
%add(F:P, V, [], [F:M]) :-
%  !, add(P, V, [], M).
%add(F, V, [], [F:V]) :-
%  !.
%add(F, V, [H|T], [H|R]) :-
%  !, add(F, V, T, R).
