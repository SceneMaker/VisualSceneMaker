/** <module> Generalized Quantifier Predicates

This library provides generalized quantifier predicates for quantifications.

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
:- module(quantifiers,
  [
    gettemplfun/4,
    gettemplist/3,
    forfraction/3,
    forfraction/4,
    forfraction/5,
    forlessthan/4,
    formorethan/4,
    forlargest/4,
    forlargest/3,
    forlongest/3
  ]).

:- reexport('thefactbase').
:- reexport('assignments').
:- reexport('collections').
:- reexport('featstructs').
:- reexport('inspections').
:- reexport('domspecific').
:- reexport('prettyprint').
:- reexport('systemtimes').
:- reexport('thedebugger').

% TODO: Check and exclude for existentially quantified variables
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

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
/**
 * foreach(:Generator, :Condition)
 *
 * This predicate tests if for all alternative bindings of =|Generator|=
 * condition =|Condition|= becomes true as well. If there does not exist
 * any binding for =|Generator|= then the predicate succeeds. The predicate
 * does not tell which binding is wrong if one binding proves wrong.
 */
%_forall(Generator, Condition) :-
%    \+ (Generator, \+ Condition).

/**
 * fornone(:Generator, :Condition)
 *
 * This predicate tests if for all alternative bindings of =|Generator|=
 * condition =|Condition|= becomes false as well. If there does not exist
 * any binding for =|Generator|= then the predicate fails. The predicate
 * does not tell which binding is wrong if one binding proves wrong.
 */
fornone(Generator, Condition) :-
    \+ forsome(Generator, Condition).
    
/**
 * forsome(:Generator, :Condition)
 *
 * This predicate tests whether there exists some binding of =|Generator|= for
 * which the condition =|Condition|= becomes true. If there does not exist any
 * binding for =|Generator|= then the predicate fails. The predicate does not
 * tell which binding yields success if one binding proves true.
 */
forsome(Generator, Condition) :-
    \+ foreach(Generator, \+ Condition).
    
/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
/**
 * formost(:Template, :Generator, :Condition)
 *
 * This predicate tests whether there exists some binding of =|Generator|= for
 * which the condition =|Condition|= becomes true. If there does not exist any
 * binding for =|Generator|= then the predicate fails. The predicate does not
 * tell which binding yields success if one binding proves true.
 */
formost(Template, Generator, Condition) :-
    collect(Template, (Generator, Condition), Range), write('Range: '), nl, out(Range), nl,
    collect(Template,(Generator, \+Condition), Scope), write('Scope: '), nl, out(Scope), nl,
    length(Range, R), length(Scope, S), R > S.

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
% Reduce signature to without template
forfraction(Fraction, Variables, Template, Generator, Condition) :-
    collect(Template, Variables^Generator, Range), write('Range: '), out(Range), nl,
    collect(Template, Variables^(Generator, Condition), Scope), write('Scope: '), out(Scope), nl,
    length(Range, R), write('Range Size: '), out(R), nl,
    length(Scope, S), write('Scope Size: '), out(S), nl, % TODO: Check for division by zero and use assignment
    Fraction is S/R,  write('Fraction: '), out(Fraction), nl.

% Here Fraction can be a free variable
forfraction(Fraction, Template, Generator, Condition) :-
    collect(Template, Generator, Range), write('Range: '), out(Range), nl,
    collect(Template,(Generator, Condition), Scope), write('Scope: '), out(Scope), nl,
    length(Range, R), write('Range Size: '), out(R), nl,
    length(Scope, S), write('Scope Size: '), out(S), nl,
    Fraction is S/R, write('Fraction: '), out(Fraction), nl.  % TODO: Check for division by zero and use assignment

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

        
%formorethan(Template, Generator, Condition, Percentage) :-
%    bagof(Template, Generator, Range),
%    bagof(Template,(Generator, Condition), Scope),
%    length(Range, R), length(Scope, S), S/R > Percentage.
        
/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
% Here Fraction has to be instatiated
forlessthan(Fraction, Template, Generator, Condition) :-
   forfraction(Portion, Template, Generator, Condition), write('Portion: '), write(Portion), nl,
   number(Fraction), Portion < Fraction.

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
%fornumber???

/*----------------------------------------------------------------------------*
 * Find the largest subsets
 *----------------------------------------------------------------------------*/
% Eigentlich sollte das hier die absolute Minderheit/Mehrheit sein
% For majority of individuals
forlargest(Template, Generator, Condition, Scope) :-
    collect(Template, Generator, Range), write('Range: '), out(Range), nl,   %collect?
    findall(X, bagof(Template, (Generator, Condition), X), Scopes), write('Scopes: '), nl, out(Scopes), nl,
    merging(X, max_size_list(Scopes, X), Candidates), write('Candidates: '), out(Candidates), nl,   %setof?  findall?
    collect(Template, (Generator, Condition), Scope), write('Scope '), out(Scope), nl,     %collect
    member(Scope, Candidates) .

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

bigger(P, N, E) :-
    val(P, V, E), V > N.


%  bagof(Color, S^formajority(X, (fsr(X), val(mode, gaze, X)), (val(data:color, Color, X)), S),L), out(L).



/*----------------------------------------------------------------------------*
 * Record Updating Predicates
 *----------------------------------------------------------------------------*/
test_1:-
add([type:event, mode:hand, data:[area:chest, name:c1, conf:0.95]]),
add([type:event, mode:hand, data:[area:chest, name:c2, conf:0.95]]),
add([type:event, mode:hand, data:[area:chest, name:c3, conf:0.95]]),
add([type:event, mode:hand, data:[area:shoulder, name:s1, conf:0.85]]),
add([type:event, mode:hand, data:[area:shoulder, name:s2, conf:0.85]]),
add([type:event, mode:hand, data:[area:jaw, name:j1, conf:0.75]]),
add([type:event, mode:hand, data:[area:jaw, name:j2, conf:0.75]]),
add([type:event, mode:hand, data:[area:jaw, name:j3, conf:0.75]]),
add([type:event, mode:hand, data:[area:jaw, name:j4, conf:0.75]]),
add([type:event, mode:hand, data:[area:forehead, name:f1, conf:0.85]]),
add([type:event, mode:hand, data:[area:forehead, name:f2, conf:0.85]]),
add([type:event, mode:hand, data:[area:forehead, name:f3, conf:0.85]]),
add([type:event, mode:hand, data:[area:forehead, name:f4, conf:0.85]]),
add([type:event, mode:hand, data:[area:forehead, name:f5, conf:0.85]]),
add([type:event, mode:hand, data:[area:forehead, name:f6, conf:0.85]]),
add([type:event, mode:hand, data:[area:forehead, name:f7, conf:0.85]]),
add([type:event, mode:hand, data:[area:forehead, name:f8, conf:0.85]]),
add([type:event, mode:hand, data:[area:forehead, name:f9, conf:0.85]]),
add([type:event, mode:hand, data:[area:forehead, name:f10, conf:0.85]]).

test_2:-
add([type:event, mode:face, data:[name:neutral, conf:0.98]]),
add([type:event, mode:face, data:[name:frown, conf:0.87]]),
add([type:event, mode:face, data:[name:smile, conf:0.78]]),
add([type:event, mode:face, data:[name:frown, conf:0.81]]).

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
%foreach() % Für alle
%formost()  % Für die meisten
%forsome()  % Für einige
%formany()  % Für viele
%forafew()   % Für wenige
%forleast() % Für die wenigsten
%fornone()  % Für keine
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
*/

 