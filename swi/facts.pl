:- module('facts',
  [ fsr/1, val/3, add/4, del/3, set/4,
    del/1, jel/1, add/1, jdd/1, rll/2, jll/2 ]).
:- reexport('terms').
:- reexport('print').

:- dynamic fsr/1.

/* Infer A Feature Value Pair */
val(Feature, Value, [Feature:Value|_]) :-
    fkeyterm(Feature).
val(Feature:Path, Value, [Feature:Record|_]) :-
    \+allvarls([Feature, Path, Record]),
    val(Path, Value, Record).
val(Feature, Value, [_|Record]) :-
    nonvar(Record),
    val(Feature, Value, Record).

/* Insert A Feature Value Pair */
add(Path, Value, Input, Output) :-
    add_(Path, Value, Input, Output, _).

add_(Feature, Value, Input, Output, Counter) :-
    fkeyterm(Feature), !,
    fvalterm(Value),
    fvlsterm(Input),
    % Add the pair to the list
    addl(Feature:Value,
         Input, Output),
    Counter is 1.
add_(Path, Value, Input, Output, Counter) :-
    fklsterm(Path),
    Path = Feature:Q,
    Input = [Feature:R|M],
    Output = [Feature:S|N], !,
    % Recursively add the value
    add_(Q, Value, R, S, K),
    % Proceed with the remainder
    (\+allvarls([M, N])
     -> add_(Path, Value, M, N, L)
      ; M = [], N = [], L is 0
    ), Counter is K + L.
add_(Path, Value, Input, Output, Counter) :-
    fklsterm(Path),
    Input = [H|M],
    Output = [H|N], !,
    % Proceed with the remainder
    (\+allvarls([M, N])
     -> add_(Path, Value, M, N, K)
      ; M = [], N = [], K is 0
    ), Counter is K.
add_(Path, _, Input, Output, Counter) :-
    fklsterm(Path),
    Input = [],
    Output = [],
    Counter is 0.%, !.

% Add object to simple list
addl(Object, Input, Output) :-
    Output = [Object|Input], !.
addl(Object, [H|Input], [H|Output]) :-
    addl(Object, Input, Output), !.
addl(Object, [], [Object]).

/* Delete A Feature Value  Pair */
del(Feature, Input, Output) :-
  fkeyterm(Feature), !,
  % Delete feature from list
  dell(Feature, Input, Output).
del(Path, Input, Output) :-
  fklsterm(Path),
  Path = Feature:Q,
  Input = [Feature:R|M],
  Output = [Feature:S|N], !,
  % Recursively delete feature
  del(Q, R, S),
  % Procees with the remainder
  (\+allvarls([M, N])
     -> del(Path, M, N)
      ; M = [], N = []).
del(Path, Input, Output) :-
  fklsterm(Path),
  Input = [H|M],
  Output = [H|N], !,
  % Procees with the remainder
  (\+allvarls([M, N])
     -> del(Path, M, N)
      ; M = [], N = []).
del(Path, Input, Output) :-
  fklsterm(Path),
  Input = [],
  Output = [].%, !.

% Delete object from the list
dell(Feature, [Feature:_|Input], Output) :-
    dell(Feature, Input, Output), !.
dell(Feature, [H|Input], [H|Output]) :-
    dell(Feature, Input, Output), !.
dell(_, Value, Value) :- fvalterm(Value).

/* Change A Feature Value */
set(Path, Value, Input, Output) :-
  del(Path, Input, Temp),
  add(Path, Value, Temp, Output).

/* Retract A Feature Record */
del(Record) :-
  retract(fsr(Record)),
  out('Retracting:\n'), out(Record).
  
jel(Record) :-
  retract(fsr(Record))%, jvw(Record, String),
  %concat('Retracting:\n', String, Output), jog(Output)
  .
  
/* Assert A Feature Record */
add(Record) :-
  assertz(fsr(Record)),
  out('Asserting:\n'), out(Record), !.
add(Record) :-
  out('Cannot Assert:\n'), out(Record).

jdd(Record) :-
  assertz(fsr(Record)), %jvw(Record, String),
  %concat('Asserting:\n', String, Output), jog(Output),
  !.
jdd(Record) :-
  jog('Cannot Assert:\n'), jog(Record).

/* Retract Feature Records */
rll(Path, Value) :-
  forall((fsr(Record),
    val(Path, Value, Record)),
  del(Record)).

jll(Path, Value) :-
  forall((fsr(Record),
    val(Path, Value, Record)),
  jel(Record)).