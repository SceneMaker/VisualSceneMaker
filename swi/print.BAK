:- module('print', [ out/1, err/1, jog/1, jvw/2 ]).

/* Console Print Out */
out(Record) :- % Print to stdout
  vwrite(Record, '', 'user_output').

err(Record) :- % Print to stderr
  vwrite(Record, '', 'user_error').

/* Pretty Print Record */
vwrite(V, _, S) :- % Print a variable value
  var(V), !, write(S, V).
vwrite([H|T], I, S) :- !, % Print a matrix value
  write(S, '['), nl(S),
  string_concat(I, '    ', J),
  lwrite([H|T], J, S),
  nl(S), write(S, I), write(S, ']').
vwrite(V, _, S) :- % Print a simple value
  !, write(S, V).

lwrite([H], I, S) :- % Print a whole list
  !, ewrite(H, I, S).
lwrite([H|T], I, S) :- !,% Print a list member
  ewrite(H, I, S),
  write(S, ','), nl(S),
  lwrite(T, I, S).

ewrite(F:V, I, S) :- !, % Print a pair member
  write(S, I),
  write(S, F),
  write(S, ':'),
  vwrite(V, I, S).
ewrite(E, I, S) :- !, % Print a simple member
  write(S, I),
  vwrite(E, I, S).
  
/* Java Logger Print */
jog(Record) :-
   jvwrite(Record, '', '', String),
   jpl_call('de.dfki.vsm.util.log.LOGDefaultLogger',
            'getInstance', [], Logger),
   jpl_call(Logger, 'message', [String], _).

/* Java Pretty Print */
jvw(Record, String) :-
  jvwrite(Record, '', '', String).
  
/* Java Format Print */
jvwrite(V, _, O, N) :- % Print a variable value
  var(V), !, concat(O, V, N).
jvwrite([H|T], I, O, N) :- !, % Print a matrix value
  concat(I, '    ', J),
  concat(O, '[\n', Z1),
  jlwrite([H|T], J, Z1, Z2),
  concat(Z2, '\n', Z3),
  concat(Z3, I, Z4),
  concat(Z4, ']', N).
jvwrite([], _, O, N) :- % Print an empty list
  !, concat(O, '[]', N).
jvwrite(V, _, O, N) :- % Print a simple value
  !, concat(O, V, N).

jlwrite([H], I, O, N) :- % Print a whole list
  !, jewrite(H, I, O, N).
jlwrite([H|T], I, O, N) :- !, % Print a list member
  jewrite(H, I, O, Z1),
  concat(Z1, ',', Z2),
  concat(Z2, '\n', Z3),
  jlwrite(T, I, Z3, N).

jewrite(F:V, I, O, N) :- !, % Print a pair member
  concat(O, I, Z1),
  concat(Z1, F, Z2),
  concat(Z2, ':', Z3),
  jvwrite(V, I, Z3, N).
jewrite(E, I, O, N) :- !, % Print simple member
  concat(O, I, Z1),
  jvwrite(E, I, Z1, N).