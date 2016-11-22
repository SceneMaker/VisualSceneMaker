% Destructive Unification With Open-Ended Lists

unify0(Dag,Dag) :- !.  % Prolog Unification Cases
unify0([Feature:Value|Rest],Dag) :-
    val(Feature,Value,Dag,StripDag),
    unify0(Rest,StripDag).
    
val(Feature,Value1,[Feature:Value2|Rest],Rest) :-
    !,
    unify0(Value1,Value2).
val(Feature,Value,[Dag|Rest],[Dag|NewRest]) :-
    !,
    val(Feature,Value,Rest,NewRest).

unify(Dag1,Dag2) :-
  unify0(Dag1,Dag2),
  write('Result of unification is: '),nl,write(Dag1),
  nl.
  
  
getValue(Path, Matrix, Value) :-
  unify0([Path:Value], Matrix).