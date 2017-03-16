:- module('timer',
   [ now/1, stop/1, init/2, expired/1, timeout/2, timer/2, start/1 ]).
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
  out('Stopping Timer '), out(Name), nl,
  retractall(timer(Name, _)).

init(Name, Delay) :-
  out('Restarting Timer '), out(Name), nl,
  stop(Name), now(Now), Time is Now + Delay,
  assertz(timer(Name, Time)).

expired(Name) :-
  timer(Name, Time), now(Now), Now > Time.
  
timeout(Name, _) :-
  timer(Name, Time), !,
  out('Evaluating Timer '), out(Name), nl,
  now(Now), Now > Time,
  out('Retracting Timer '), out(Name), nl,
  retractall(timer(Name, Time)).
timeout(Name, Delay) :-
  out('Constructing Timer '), out(Name), nl,
  now(Now), Time is Now + Delay,
  assertz(timer(Name, Time)), fail.