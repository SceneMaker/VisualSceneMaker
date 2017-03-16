:- module('quant',
   [ collect/3, arrange/3, template/4, forevery/4, forsome/4, fornone/4,
     formost/3, formost/4, forfraction/3, forfraction/4, forfraction/5,
     forlessthan/4, formorethan/4, forlargest/2, forlargest/3, forlargest/4 ]).
:- reexport('terms').
:- reexport('print').

/* Collect All Solutions */
collect(Template, Generator, Collection) :-
    bagof(Template, Generator, List)
    *-> Collection = List ; Collection = [].
        
/* Arrange All Solutions */
arrange(Template, Generator, Collection) :-
    setof(Template, Generator, List)
    *-> Collection = List ; Collection = [].
        
/* Construct Template List */
template(Generator, Condition, Functor, Template) :-
    termvars(Generator, G), sort(G, GenVars),
    termvars(Condition, C), sort(C, ConVars),
    ord_intersection(GenVars, ConVars, SharedVars),
    Template =.. [Functor|SharedVars].

/* For Every Quantifier */
forevery(Variables, Template, Generator, Condition) :-
    collect(Template, Variables^Generator, Range),
    collect(Template, Variables^(Generator, Condition), Scope),
    length(Range, R), length(Scope, S), R is S.

/* For Some Quantifier */
forsome(Variables, Template, Generator, Condition) :-
    \+ forevery(Variables, Template, Generator, \+(Condition)).

/* For None Quantifier */
fornone(Variables, Template, Generator, Condition) :-
    \+ forsome(Variables, Template, Generator, Condition).
    
/* For Most Quantifier */
formost(Template, Generator, Condition) :-
    collect(Template, (Generator, Condition), Range),
    collect(Template, (Generator, \+(Condition)), Scope),
    length(Range, R), length(Scope, S), R > S.

formost(Variables, Template, Generator, Condition) :-
    collect(Template, Variables^(Generator, Condition), Range),
    collect(Template, Variables^(Generator, \+(Condition)), Scope),
    length(Range, R), length(Scope, S), R > S.

/* For Fraction Quantifier */
forfraction(Fraction, Generator, Condition) :-
    template(Generator, Condition, 'template', Template),
    forfraction(Fraction, Template, Generator, Condition).
    
forfraction(Fraction, Template, Generator, Condition) :-
    collect(Template, Generator, Range),
    collect(Template,(Generator, Condition), Scope),
    length(Range, R), length(Scope, S),
    R \== 0, Fraction is S/R.
    
forfraction(Fraction, Variables, Template, Generator, Condition) :-
    collect(Template, Variables^Generator, Range),
    collect(Template, Variables^(Generator, Condition), Scope),
    length(Range, R), length(Scope, S),
    R \== 0, Fraction is S/R.

/* For More Than Quantifier */
formorethan(Fraction, Template, Generator, Condition) :-
    forfraction(Portion, Template, Generator, Condition),
    number(Fraction), Portion > Fraction.

/* For Less Than Quantifier */
forlessthan(Fraction, Template, Generator, Condition) :-
   forfraction(Portion, Template, Generator, Condition),
   number(Fraction), Portion < Fraction.

/* For Largest Part Quantifier */
forlargest(Generator, Condition) :-
    template(Generator, Condition, 'template', Template),
    forlargest(Template, Generator, Condition, _).

forlargest(Template, Generator, Condition) :-
    forlargest(Template, Generator, Condition, _).

forlargest(Template, Generator, Condition, Scope) :-
    collect(Template, Generator, Range), % Get the quantifier range
    findall(X, bagof(Template, (Generator, Condition), X), Scopes),
    findall(X, max_size_list(Scopes, X), Candidates), out(Range), nl,
    sort(Candidates, Sorted), % Get a sorted list of largest scopes
    collect(Template, (Generator, Condition), Scope), out(Scope), nl,
    member(Scope, Sorted). % And check for each scope if it is maximal

% Get list of lists with maximal length
max_size_list([List], List).
max_size_list([Head|Tail], List) :-
      max_size_list(Tail, Some),
      length(Some, SomeL),
      length(Head, HeadL),
      ((SomeL  >  HeadL, List = Some);
       (SomeL  <  HeadL, List = Head);
       (SomeL == HeadL, Some \== Head,
          (List = Head ; List = Some))).