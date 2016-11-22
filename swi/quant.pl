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
:- module(quant,
  [
    %Collections
    collect/3,
    arrange/3,
    %Templates
    gettemplfun/4,
    gettemplist/3,
    %Quantifiers
    forevery/4,
    forsome/4,
    fornone/4,
    formost/4,
    forfraction/3,
    forfraction/4,
    forfraction/5,
    forlessthan/4,
    formorethan/4,
    forlargest/4,
    forlargest/3,
    forlongest/3
  ]).

:- reexport('terms').

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
 
collect(Template, Generator, Collection) :-
    bagof(Template, Generator, List)
    *->
        Collection = List
    ;
        Collection = [].

arrange(Template, Generator, Collection) :-
    setof(Template, Generator, List)
    *->
        Collection = List
    ;
        Collection = [].
        
/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
        
% TODO: Check for and exclude existentially quantified variables
gettemplfun(Generator, Condition, Functor, Template) :-
    termvars(Generator, GenVars0), sort(GenVars0, GenVars), write('Generator Variables: '), write(GenVars), nl,
    termvars(Condition, ConVars0), sort(ConVars0, ConVars), write('Condition Variables: '), write(ConVars), nl,
    ord_intersection(GenVars, ConVars, SharedVars), write('Shared Variables: '), write(SharedVars), nl,
    Template =.. [Functor|SharedVars], write(Template), nl.

gettemplist(Generator, Condition, Template) :-
    termvars(Generator, GenVars0), sort(GenVars0, GenVars), write('Generator Variables: '), write(GenVars), nl,
    termvars(Condition, ConVars0), sort(ConVars0, ConVars), write('Condition Variables: '), write(ConVars), nl,
    ord_intersection(GenVars, ConVars, SharedVars), write('Shared Variables: '), write(SharedVars), nl,
    Template = SharedVars, write(Template), nl.

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/

forevery(Variables, Template, Generator, Condition) :-
    collect(Template, Variables^Generator, Range),
    collect(Template, Variables^(Generator, Condition), Scope),
    length(Range, R), length(Scope, S), R is S.

forsome(Variables, Template, Generator, Condition) :-
    \+ forevery(Variables, Template, Generator, \+(Condition)).
    
fornone(Variables, Template, Generator, Condition) :-
    \+ forsome(Variables, Template, Generator, Condition).

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/

formost(Template, Generator, Condition) :-
    collect(Template, (Generator, Condition), Range),  write('Range: '), nl, out(Range), nl,
    collect(Template, (Generator, \+(Condition)), Scope), write('Scope: '), nl, out(Scope), nl,
    length(Range, R),  write('Range Size: '), out(R), nl,
    length(Scope, S),  write('Scope Size: '), out(S), nl,
    R > S.
    
formost(Variables, Template, Generator, Condition) :-
    collect(Template, Variables^(Generator, Condition), Range), write('Range: '), nl, out(Range), nl,
    collect(Template, Variables^(Generator, \+(Condition)), Scope), write('Scope: '), nl, out(Scope), nl,
    length(Range, R), write('Range Size: '), out(R), nl,
    length(Scope, S), write('Scope Size: '), out(S), nl,
    R > S.

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
 
% Reduce signature to without template
forfraction(Fraction, Variables, Template, Generator, Condition) :-
    collect(Template, Variables^Generator, Range), write('Range: '), out(Range), nl,
    collect(Template, Variables^(Generator, Condition), Scope), write('Scope: '), out(Scope), nl,
    length(Range, R), write('Range Size: '), out(R), nl,
    length(Scope, S), write('Scope Size: '), out(S), nl,
    R \== 0, Fraction is S/R,  write('Fraction: '), out(Fraction), nl.

% Here Fraction can be a free variable
forfraction(Fraction, Template, Generator, Condition) :-
    collect(Template, Generator, Range), write('Range: '), out(Range), nl,
    collect(Template,(Generator, Condition), Scope), write('Scope: '), out(Scope), nl,
    length(Range, R), write('Range Size: '), out(R), nl,
    length(Scope, S), write('Scope Size: '), out(S), nl,
    R \== 0, Fraction is S/R, write('Fraction: '), out(Fraction), nl.

% Reduce signature to without template
forfraction(Fraction, Generator, Condition) :-
    gettemplfun(Generator, Condition, template, Template),
    forfraction(Fraction, Template, Generator, Condition).

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
 
% Here Fraction has to be instatiated
formorethan(Fraction, Template, Generator, Condition) :-
    forfraction(Portion, Template, Generator, Condition), write('Portion: '), write(Portion), nl,
    number(Fraction), Portion > Fraction.

% Here Fraction has to be instatiated
forlessthan(Fraction, Template, Generator, Condition) :-
   forfraction(Portion, Template, Generator, Condition), write('Portion: '), write(Portion), nl,
   number(Fraction), Portion < Fraction.

%formorethan(Template, Generator, Condition, Percentage) :-
%    bagof(Template, Generator, Range),
%    bagof(Template,(Generator, Condition), Scope),
%    length(Range, R), length(Scope, S), S/R > Percentage.


%fornumber???

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*
 * Find the largest subsets
 *----------------------------------------------------------------------------*/
 
% For the majority of individuals
forlargest(Template, Generator, Condition, Scope) :-
    collect(Template, Generator, Range), %write('Range: '), out(Range), nl, %For all possible range alternatives
    findall(X, bagof(Template, (Generator, Condition), X), Scopes), %write('Scopes: '), nl, out(Scopes), nl, length(Range, R), write('Range Size: '), out(R), nl, forall(member(Set, Scopes), (length(Set, S), write('Scope Size: '), out(S), nl)),
    max_size_list(Scopes, MaxList), %write('MaxSizeList: '), out(MaxList), nl, length(MaxList, M), write('MaxSizeList Size: '), out(M), nl,
    %merging(X, max_size_list(Scopes, X), Candidates), write('Candidates: '), out(Candidates), nl,   %setof?  findall?
    collect(Template, (Generator, Condition), Scope), %write('Scope '), out(Scope), nl,     %For all possible scope alternatives   length(Scope, Q), write('Scope Size: '), out(Q), nl ,
    %member(Scope, Candidates)
    Scope = MaxList .

forlargest(Template, Generator, Condition) :-
    forlargest(Template, Generator, Condition, _).

% also implement max_duration_list      !!!!
max_size_list([List], List).
max_size_list([Head|Tail], List) :-
      max_size_list(Tail, Some),
      length(Some, SomeL),
      length(Head, HeadL),
      (
        (SomeL  >  HeadL, List = Some);
        (SomeL  <  HeadL, List = Head);
        (SomeL == HeadL, Some \== Head,
          (List = Head ; List = Some)
        )
      ).

% for majority of duration
forlongest(Template, Generator, Scopes) :-
          findall(X, bagof(Template, Generator, X), Scopes).


% Largest and longest brauchen keine zwei mengen, denn man kann einfach die größte menge
% nehmen (eine der größten) , aus denjenigen alternativen mengen, die durch die freien
% variablen im generator erzeugt werden.( semantik anders ????) onelargestalternative()
% dieses prädikat wäre dann quasi der zweite teil (nach dem ersten bagof) im obigen prädikat
% forlargest???  D.h. die mengen für die einzenlen alternativen finde ich mi bagof/collect.
% diese kann ich in die menge der alternativmengen zusammenfassen durch den findall aufruf.
% dann muss ich diejenige menge nehmen, die maximal ist, bzw die teilmenge davonn mit den maximalen mengen.
% als letztes kann ich eine member abfrage machen, wenn ich will, oder einfach die menge zurückgeben oder beides.
%largest(Template, Generator, Splitter, Condition) :-

%  bagof(Color, S^formajority(X, (fsr(X), val(mode, gaze, X)), (val(data:color, Color, X)), S),L), out(L).
%quantify([M, D], (fsr(E), val(type, event, E), val(mode, M, E), val(data, D, E)), Goal), bagof(E, Goal, List).

/*
 findall((MajorTouchedArea, AreaTouchingEventList),
          formajority(Event,
                     (fsr(Event), val(type, event, Event), val(mode, hand, Event)),
                     (val(data:area, MajorTouchedArea, Event)),
                      AreaTouchingEventList),
          MajorTouchedAreaAndEventListList).

  formajority(E, (fsr(E), val(mode, hand, E), bigger(data:conf, 0.9, E)), (val(data:area, A, E)), M).
 */

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
%formany()  % Für viele
%forafew()   % Für wenige
%forleast() % Für die wenigsten

%foralmost()   % für die ungefähr anzahl von
%foralmostof() % für den ungefähr anteil von

%forminority()
%forlessthan()
%formorethan()

%foratleast() = formorethan
%foratmost()
%forlatest
%forhighestnum (can fail?)
%forlowestnum  (can fail?)

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/

  /**
  freevars(+T:term, -L:list) is det

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

freevars(Template, Generator, Variables) :-
  free_variables(Generator, Template, [], Variables).

template(RangeGenerator, ScopeGenerator, Functor, Template) :-
  term_variables(RangeGenerator, RangeVariables0), sort(RangeVariables0, RangeVariables), write('Range Generator Variables: '), write(RangeVariables), nl,
  term_variables(ScopeGenerator, ScopeVariables0), sort(ScopeVariables0, ScopeVariables), write('Scope Generator Variables: '), write(ScopeVariables), nl,
  ord_intersection(RangeVariables, ScopeVariables, SharedVariables), write('Shared Generator Variables: '), write(SharedVariables), nl,
  Template =.. [Functor|SharedVariables], write(Template), nl.


myforeach(Generator, Goal) :-
        term_variables(Generator, GenVars0), sort(GenVars0, GenVars),       write('Gen Vars '), write(SharedGoalVars), nl,
        term_variables(Goal, GoalVars0), sort(GoalVars0, GoalVars),     write('Goal Vars '), write(SharedGoalVars), nl,
        ord_subtract(GoalVars, GenVars, SharedGoalVars),   write('Shared Goal Vars '), write(SharedGoalVars), nl,
        (   SharedGoalVars == []
        ->  \+ (Generator, \+Goal)      % = forall(Generator, Goal)
        ;   ord_intersection(GenVars, GoalVars, SharedVars), write('Shared Vars '), write(SharedVars), nl,
            Templ =.. [v|SharedVars],        write(Templ), nl,
            SharedTempl =.. [v|SharedGoalVars],    write(SharedTempl), nl,
           % findall(Templ, Generator, List),   out(List),nl,      %write(List), nl,
            bagof(Templ, Generator, List),   out(List),nl,      %write(List), nl,
            prove_list(List, Templ, SharedTempl, Goal)
    ).

prove_list([], _, _, _).
prove_list([H|T], Templ, SharedTempl, Goal) :-
        copy_term(Templ+SharedTempl+Goal,
                  H+SharedTempl+Copy),   write('Copy '), write(Copy), nl,
        Copy,
        prove_list(T, Templ, SharedTempl, Goal).
*/

