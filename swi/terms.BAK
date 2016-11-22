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
:- module(inspections,
  [
    fkeyterm/1,
    fklsterm/1,
    fvalterm/1,
    fvlsterm/1,
    allvarls/1,
    nonvarls/1,
    termvars/2,
    extgvars/2,
    extlvars/2,
    extqvars/2,
    uniqvars/2
  ]).

/**
  fkeyterm(?T:term) is semidet

  This predicate returns with success if the argument term is a variable,
  an atom or a rational number, which includes integer and floating point
  values. It returns with failure if the term is some kind of compound term.
  The predicate is used to examine if the given argument term is a valid
  feature key for a feature value pair within a feature structure record.
*/

fkeyterm(T) :- var(T), !.
fkeyterm(T) :- atom(T), !.
fkeyterm(T) :- number(T).

/**
  fklsterm(?T:term) is semidet

  This predicate returns with success if the argument term is a valid
  feature path but not a single feature key for a feature value pair
  within a feature structure record.
*/

fklsterm(T) :- var(T), !, fail.
fklsterm(F:P) :-
    fkeyterm(F),
    fkeyterm(P).
fklsterm(F:P) :-
    fkeyterm(F),
    fklsterm(P).

/**
  fvalterm(?T:term) is semidet

  This predicate returns with success if the argument term is a valid value
  of a feature structure record, which includes atoms, rational numbers such
  as integer and floating point values, variables, lists of terms or functor
  terms. It returns with failure if the term is some kind of compound term of
  the form =|_:_|= because these terms are reserved for the construction of
  feature paths in a feature structure record. The predicate is used to check
  if the given argument term is a valid feature value for a feature value pair
  within a feature structure record.
*/

fvalterm(T) :- var(T), !.
fvalterm(_:_) :- !, fail.
fvalterm(_).

/**
  fvlsterm(?T:term) is semidet

  This predicate returns with success if the argument term is a list term and
  it returns with failure if the term is some other kind of term. The predicate
  is used to check if the given argument term is a valid feature record matrix
  value for a feature value pair within a feature structure record.
*/

fvlsterm(T) :- var(T), !.
fvlsterm([]) :- !.
fvlsterm([_|_]).

/**
  allvarls(+L:list) is semidet

  This predicate returns with success if the argument term =|L|= is a list
  that exclusively contains variables and will return with failure if the
  list contains any term that is not a variable. The predicate will also
  return with failure if the argument is a variable which is not bound.
*/

allvarls(L) :-
    var(L), !, fail.
allvarls([T]) :-
    var(T).
allvarls([H|T]) :-
    var(H), allvarls(T).

/**
  nonvarls(+L:list) is semidet

  This predicate returns with success if the argument term =|L|= is a list
  that does not contain any variables and will return with failure if the
  list contains one or more terms that are variables. The predicate will
  also return with failure if the argument is a variable that is not bound.
*/
nonvarls(L) :-
    var(L), !, fail.
nonvarls([T]) :-
    nonvar(T).
nonvarls([H|T]) :-
    nonvar(H), nonvarls(T).

/**
  termvars(+T:term, -L:list) is det

  This predicate always returns with success. It inspects the input term
  =|T|= for variables and unifies the list =|L|= with a list of variables,
  each sharing with a unique variable of term =|T|=. The variables in the
  list =|L|= are ordered in order of appearance traversing =|T|= using a
  depth-first and left-to-right strategy by falling back on the built-in
  predicate term_variables/3. The following example illustrates termvars/2:
  ==
  ?- termvars(X^(a(X, Y), b(Y, [Y:V|T])), L).
  L = [X, Y, V, T].
  ==
*/
termvars(T, L) :-
    term_variables(T, L).

/**
  extgvars(+G:goal, -L:list) is det

  This predicate always returns with success. It inspects the input goal
  =|G|= for variables that are existentially quantified using the built-in
  existential operator ^/2. Then it unifies the list =|L|= with the list of
  variables in =|G|= that are existentially quantified. Thereby, as proposed
  by the ISO standard, only outermost existential quantification is accepted,
  that means, that the term =|G|= must have the form =|G=X^Y^...^Goal|=.
  
  The existential qualifier ^/2 is usually used inside the goal of setof/3
  and bagof/3 in order to avoid binding the corresponding variables and thus
  avoiding the backtracking for the alternaitive bindings of these variables.
  The use of this existential qualifier is superfluous outside of bagof/3 and
  setof/3, in SWI-Prolog, ^/2 can even only appear as the second argument of
  setof/3 and bagof/3, otherwise an error is produced. The following example
  illustrates the use of extgvars/2 in goals with or without the ^/2 operator:
*/
extgvars(G, []) :- var(G), !.
extgvars(H^G, [H|T]) :-
    var(H), !, extgvars(G, T).
extgvars(_, []).

%
/**
  extlvars(?T:list, ?I:list, ?O:list) is det
  
  Find all existantially quantified variables in the list of terms =|T|=. Add
  all these existantially quantified variables to the input variable list =|I|=
  and return the resulting concatenated output variable list =|O|= as result.
*/
extlvars([], []) :- !.
extlvars([H|T], O) :-
    extqvars(H, V), write('term '), write(H), write(' has variables '), write(V), nl,
    extlvars(T, L),
    append(V, L, O).

/**
  extqvars(?T:term, ?L:list) is det

  Find all existantially quantified variables in the term =|T|= and return them
  in the resulting output variable list =|L|=.
*/
extqvars(V, []) :- var(V), !.
extqvars([], []) :- !.
extqvars([H|T], L) :-
    !, extlvars([H|T], L).
extqvars(G, [H|R]) :-
    G =.. [^|[H|T]], var(H), !,
    extlvars(T, R).
extqvars(G, Q) :-
    G =.. [^|[H|T]], !,
    extqvars(H, V),
    extlvars(T, R),
    append(V, R, Q).
extqvars(G, V) :-
    G =.. [F|L], write('functor '), write(F), write(' arguments '), write(L), nl,
    extlvars(L, V).

/**
  uniqvars(?T:term, ?L:list) is det

  Find all existentially quantified variables in the term =|T|= and return them
  in the resulting output variable list =|L|=.
*/
uniqvars(Term, List) :-
  termvars(Term, All0), sort(All0, AllVars), write('all variables found in the term: '), write(AllVars), nl,
  extqvars(Term, Ext0), sort(Ext0, ExtVars), write('existentially quantified variables: '), write(ExtVars), nl,
  ord_subtract(AllVars, ExtVars, UniVars), write('universally quantified variables: '), write(UniVars), nl,
  List = UniVars.

quantify([] , Generator, Generator).
quantify([Head|Tail], Generator, Head^Quantified) :-
  write(Head), write('^'), quantify(Tail, Generator, Quantified).
%quantify([M, D], (fsr(E), val(type, event, E), val(mode, M, E), val(data, D, E)), Goal), bagof(E, Goal, List).

/*----------------------------------------------------------------------------*
 *     Get template, existential quantified, range and scope variables in term
 *----------------------------------------------------------------------------*/
inspect(Quanfifier, TemplVars, QuantVars, RangeVars, ScopeVars) :-
  Quanfifier =.. [Functor|[TemplTerm, QuantVars, RangeGoal, ScopeGoal]],
  write('Functor :'), write(Functor), nl,
  %write('Template :'), write(Template), nl,
  %write('Variables :'), write(Variables), nl,
  %write('RangeGoal :'), write(RangeGoal), nl,
  %write('ScopeGoal :'), write(ScopeGoal), nl,
  termvars(TemplTerm, TemplVars),
  termvars(RangeGoal, RangeVars),
  termvars(ScopeGoal, ScopeVars),
  write('RangeVars :'), write(RangeVars), nl,
  write('ScopeVars :'), write(ScopeVars), nl.


