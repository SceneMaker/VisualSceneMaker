:- module('terms',
   [ fkeyterm/1, fklsterm/1, fvalterm/1, fvlsterm/1,
     allvarls/1, nonvarls/1, termvars/2, extgvars/2,
     extlvars/2, extqvars/2, nonqvars/2 ]).

/* Keys Of A Feature Pair */
fkeyterm(Term) :- var(Term), !.
fkeyterm(Term) :- atom(Term), !.
fkeyterm(Term) :- number(Term).

/* Matrix Of A Feature Pair */
fklsterm(Term) :- var(Term), !, fail.
fklsterm(Feature:Path) :-
    fkeyterm(Feature),
    fkeyterm(Path).
fklsterm(Feature:Path) :-
    fkeyterm(Feature),
    fklsterm(Path).

/* Values Of A Feature Pair */
fvalterm(Term) :- var(Term), !.
fvalterm(_:_) :- !, fail.
fvalterm(_).

/* List Of A Feature Pair */
fvlsterm(Term) :- var(Term), !.
fvlsterm([]) :- !.
fvlsterm([_|_]).

/* All Members Are Variables */
allvarls(List) :-
    var(List), !, fail.
allvarls([Term]) :-
    var(Term).
allvarls([Head|Tail]) :-
    var(Head), allvarls(Tail).

/* No Members Are Variables */
nonvarls(List) :-
    var(List), !, fail.
nonvarls([Term]) :-
    nonvar(Term).
nonvarls([Head|Tail]) :-
    nonvar(Head), nonvarls(Tail).

/* Get All Term Variables */
termvars(Term, List) :-
    term_variables(Term, List).
    
/* Extential Qualified Variables */
extgvars(Goal, []) :- var(Goal), !.
extgvars(Head^Goal, [Head|Tail]) :-
    var(Head), !, extgvars(Goal, Tail).
extgvars(_, []).

extlvars([], []) :- !.
extlvars([Head|Tail], List) :-
    extqvars(Head, HeadVars),
    extlvars(Tail, TailVars),
    append(HeadVars, TailVars, List).

extqvars(Var, []) :- var(Var), !.
extqvars([], []) :- !.
extqvars([Head|Tail], ExtVars) :-
    !, extlvars([Head|Tail], ExtVars).
extqvars(Goal, [Head|Rest]) :-
    Goal =.. [^|[Head|Tail]], var(Head), !,
    extlvars(Tail, Rest).
extqvars(Goal, ExtVars) :-
    Goal =.. [^|[Head|Tail]], !,
    extqvars(Head, HeadVars),
    extlvars(Tail, TailVars),
    append(HeadVars, TailVars, ExtVars).
extqvars(Goal, ExtVars) :-
    Goal =.. [_|List],
    extlvars(List, ExtVars).

/* Non-Qualified Variables*/
nonqvars(Term, List) :-
  termvars(Term, All), sort(All, AllVars),
  extqvars(Term, Ext), sort(Ext, ExtVars),
  ord_subtract(AllVars, ExtVars, UniVars),
  List = UniVars.