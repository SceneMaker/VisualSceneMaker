:- module('tempr',
   [ concurrent/2, during/2, contains/2, before/2, after/2, overlaps/2,
     overlapped/2, meets/2, met/2, starts/2, started/2, finishes/2, finished/2,
     distexactly/3, distlessthan/3, distmorethan/3, follows/2, followed/2,
     between/3, followers/2, ancestors/2, interims/3, nextto/2, prevto/2,
     framed/3, newest_of_list/2, oldest_of_list/2, newest/3, oldest/3 ]).
:- reexport('facts').
:- reexport('print').

t(A, B, SA, SB, EA, EB) :-
  val('time', TA, A), val('time', TB, B), % Get the timestamp
  val('dist', DA, A), val('dist', DB, B), % Get the distances
  val('life', LA, A), val('life', LB, B), % Get the durations
  SA = TA - DA, SB = TB - DB, % Compute the event start times
  EA = SA + LA, EB = SB + LB. % Compute the event end times

/* Temporal Interval Relations */
concurrent(A, B) :-
  t(A, B, SA, SB, EA, EB), SA == SB, EA == EB.

during(A, B) :-
  t(A, B, SA, SB, EA, EB), SA > SB, EA < EB.

contains(A, B) :-
  t(A, B, SA, SB, EA, EB), SA < SB, EB < EA.

before(A, B) :-
  t(A, B, _, SB, EA, _), EA < SB.

after(A, B) :-
  t(A, B, SA, _, _, EB), EB < SA.

overlaps(A, B) :-
  t(A, B, SA, SB, EA, EB), SA < SB, SB < EA, EA < EB.

overlapped(A, B) :-
  t(A, B, SA, SB, EA, EB), SB < SA, SA < EB, EB < EA.

meets(A, B) :-
  t(A, B, _, SB, EA, _), EA == SB.

met(A, B) :-
  t(A, B, SA, _, _, EB), EB == SA.

starts(A, B) :-
 t(A, B, SA, SB, EA, EB), SA == SB, EA < EB.

started(A, B) :-
 t(A, B, SA, SB, EA, EB), SB == SA, EB < EA.

finishes(A, B) :-
  t(A, B, SA, SB, EA, EB), EA == EB, SB < SA.

finished(A, B) :-
 t(A, B, SA, SB, EA, EB), EA == EB, SB < SA.

/* Temporal Distance Relations */
distexactly(A, B, D) :-
  t(A, B, _, SB, EA, _), EA < SB, D is SB - EA.

distlessthan(A, B, D) :-
  t(A, B, _, SB, EA, _), EA < SB, D > SB - EA.

distmorethan(A, B, D) :-
  t(A, B, _, SB, EA, _), EA < SB, D < SB - EA.

/* Temporal Ordering Relations */
follows(A, B) :-
    val('mode', MA, A), val('mode', MB, B),
    MA == MB, after(A, B).

followed(A, B) :-
    val('mode', MA, A), val('mode', MB, B),
    MA == MB, before(A, B).

between(E, A, B) :-
    follows(E, A), followed(E, B).

followers(E, L) :-
    bagof(T, (fsr(T), follows(T, E)), L).

ancestors(E, L) :-
    bagof(T, (fsr(T), followed(T, E)), L).

interims(A, B, L) :-
    bagof(T, (fsr(T), between(T, A, B)), L).

nextto(A, B) :-
    followers(B, L), oldest_of_list(A, L).

prevto(A, B) :-
    ancestors(B, L), newest_of_list(A, L).

framed(E, A, B) :-
    nextto(E, A), prevto(E, B).
    
/* List Ordering Relations */
newest_of_list(R, [R]):- !.
newest_of_list(R, [H|T]) :-
  newest_of_list(L, T),
  ( after(L, H), !, R = L ; after(H, L), !, R = H ).

oldest_of_list(R, [R]):- !.
oldest_of_list(R, [H|T]) :-
  oldest_of_list(L, T),
  ( before(L, H), !, R = L ; before(H, L), !, R = H ).

oldest(T, C, N) :-
  bagof(T, C, L), oldest_of_list(N, L).

newest(T, C, N) :-
  bagof(T, C, L), newest_of_list(N, L).