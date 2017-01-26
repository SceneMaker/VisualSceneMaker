:- module('clean',
   [ clean/0, clean/2 ]).
:- reexport('facts').
:- reexport('timer').
:- reexport('print').

/* Fact Base Cleanup */
clean :-
  out('Cleaning Fact Base'), nl,
  retractall(start(_)),
  forall(timer(Name, Delay),
   (out('Retracting Timer '), out(Name), nl, retract(timer(Name, Delay)))),
  
  forall((fsr(Record),
    val('type', 'event', Record)),
  retract(fsr(Record))),
  forall((fsr(Record),
    val('type', 'signal', Record)),
  retract(fsr(Record))),
  forall((fsr(Record),
    val('type', 'entity', Record)),
  retract(fsr(Record))),
  forall((fsr(Record),
    val('type', 'instruct', Record)),
  retract(fsr(Record))).

/* Garbage Collection */
clean(Mode, Age) :-
  now(Now), Lim is Now - Age,
  forall((fsr(Record),
    val('mode', Mode, Record),
    val('time', Time, Record),
    val('from', Dist, Record),
    val('life', Life, Record),
    End is Time - Dist + Life, Lim > End),
  retract(fsr(Record))).