:- module('timer',
   [ now/1, stop/1, init/2, expired/1, timeout/2 ]).
:- reexport('facts').

/* Current System Time */
:- dynamic start/1.

now(Now) :-
  start(Start), !, % System running
  statistics('walltime', [Time, _]),
  Now is Time - Start.
now(Now) :- % Register startup time
  statistics('walltime', [Time, _]),
  assertz(start(Time)), Now is 0.

/* Timeout Handling */
:- dynamic timer/2.

stop(Name) :-
  retractall(timer(Name, _)).

init(Name, Delay) :-
   stop(Name), now(Now), Time is Now + Delay,
   assertz(timer(Name, Time)).

expired(Name) :-
  timer(Name, Time), now(Now), Now > Time.
  
timeout(Name, _) :-
  timer(Name, Time), !, now(Now), Now > Time,
  retractall(timer(Name, Time)).
timeout(Name, Delay) :-
  now(Now), Time is Now + Delay,
  assertz(timer(Name, Time)), fail.