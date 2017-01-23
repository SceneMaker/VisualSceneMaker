:- module('timer', [ start/1, now/1, stop/1, init/2,
                     timer/2, timeout/2, timeout/3, expired/1 ]).
:- reexport('facts').

/* Current System Time */
:- dynamic start/1.

now(Now) :-
  start(Start), !,
  statistics('walltime', [Time, _]),
  Now is Time - Start.
  
now(Now) :-
  statistics('walltime', [Time, _]),
  assertz(start(Time)), Now is 0.

/* Timeout Predicates */
:- dynamic timer/2.

stop(Name) :-
  retractall(timer(Name, _)).

init(Name, Delay) :-
   stop(Name), now(Now),
   Time is Now + Delay,
   assertz(timer(Name, Time)).

expired(Name) :-
  timer(Name, Time),
  now(Now), Now > Time.
  
timeout(Name, _) :-
  timer(Name, Time), !,
  now(Now), Now > Time,
  retractall(timer(Name, Time)).
timeout(Name, Delay) :-
  now(Now), Time is Now + Delay,
  assertz(timer(Name, Time)), fail.

timeout(Name, _, _) :-
  timer(Name, Time),
  now(Now), Now > Time,
  retractall(timer(Name, Time)).
timeout(Name, Delay, Event) :-
  val('time', Time, Event),
  val('dist', Dist, Event),
  val('life', Life, Event),
  End is Time - Dist + Life + Delay,
  assertz(timer(Name, End)), fail.