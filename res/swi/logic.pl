/*  This code is part of the Behavior Flow Query Language based on SWI-Prolog.

    Author:        Gregor Mehlmann
    E-mail:        mehlmann@hcm-lab.de
    WWW:           http://www.hcm-lab.de

    Copyright (C): 2008-2018,
                   University of Augsburg
                   Applied Computer Sciences
                   Human Centered Multimedia

    This program is free software; you can redistribute it and/or modify it
    under the terms of the 'GNU General Public License' as published by the
    Free Software Foundation, version 2 or any later version of the License.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY and without even the implied warranty
        of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    You should have received a copy of the GNU General Public License along
    with this library; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

    As an exception, if you link this library with other files, compiled with
    a Free Software compiler, to produce an executable, this library does not
    by itself cause the resulting executable to be covered by the GNU General
    Public License. This exception does not invalidate any other reasons why
    the executable file might be covered by the 'GNU General Public License'.
*/
:- module(logic,
  [
    % Facts
    fsr/1,
    now/1,
    set/2,
    val/3,
    add/5,
    del/1,
    jel/1,
    add/1,
    jdd/1,
    rll/2,
    jll/2,
    % Print
    out/1,
    err/1,
    jog/1,
    % Timeout
    timeout/2,
    % Signals
    signal/3,
    consume/3,

   attr/3,
   % Temporal Interval Predicates
   after/2,
   % Event Detection Predicates
   gaze/3,
   move/3,
   voice/3,
   speech/1,
   function/2,
   content/2,
   % Event Counting Predicates
   count/2,
   % Garbage Collection
   clear/2
   
   
  ]).

:- reexport('facts').
:- reexport('print').

/*----------------------------------------------------------------------------*
 * Timeout Predicates
 *----------------------------------------------------------------------------*/
timeout(S, T) :- now(N), N > S + T.

/*----------------------------------------------------------------------------*
 * Signalling Predicates
 *----------------------------------------------------------------------------*/
signal(T, N, D) :-
  forall(
    (fsr(R),
     val(type, event, R),
     val(mode, signal, R),
     val(recv, T, R),
     val(name, N, R)),
  del(R)), now(Q),
  add(
  [type:event,
   mode:signal,
   recv:T,
   name:N,
   time:Q,
   data:D]).

consume(T, N, D) :-
  fsr(R),
  val(type, event, R),
  val(mode, signal, R),
  val(recv, T, R),
  val(name, N, R),
  val(data, D, R),
  del(R).

  
/*----------------------------------------------------------------------------*
 * Record Attribute Predicates
 *----------------------------------------------------------------------------*/
%agent(V, R) :-
%  fsr(R), type(R, agent), name(R, V).
  
%event(V, R) :-
%  fsr(R), type(R, event), name(R, V).

%set(P, R, V) :-
%  fsr(R), set(P, V, R, S), add(S), del(R).

/*----------------------------------------------------------------------------*
 * Attribute List Predicates
 *----------------------------------------------------------------------------*/
attr(N, D, [H|_]) :-
  H = [type:attribute, name:N, data:D].
attr(N, D, [_|T]) :-
  attr(N, D, T).


  
/*----------------------------------------------------------------------------*
 * Voice Event Extraction
 *----------------------------------------------------------------------------*/
voice(N, D, E) :-
  findall(R,
    (fsr(R),
     val(type, event, R),
     val(mode, voice, R),
     val(name, N, R)), L),
  latest(E, L),
  val(data, D, E),
  forall(
    (fsr(R),
     member(R, L)),
     jel(R)).
/*----------------------------------------------------------------------------*
 * Move Event Extraction
 *----------------------------------------------------------------------------*/
move(N, D, E) :-
  findall(R,
    (fsr(R),
     val(type, event, R),
     val(mode, move, R),
     val(name, N, R)), L),
  latest(E, L),
  val(data, D, E),
  forall(
    (fsr(R),
     member(R, L)),
     jel(R)).
/*----------------------------------------------------------------------------*
 * Gaze Event Extraction
 *----------------------------------------------------------------------------*/
gaze(N, D, E) :-
  findall(R,
    (fsr(R),
     val(type, event, R),
     val(mode, gaze, R),
     val(name, N, R)),
    L),
  latest(E, L),
  val(data, D, E),
  forall(
    (fsr(R),
     member(R, L)),
     jel(R)).
     
/*----------------------------------------------------------------------------*
 * Speech Event Extraction
 *----------------------------------------------------------------------------*/
speech(Event) :-
  findall(R,
    (fsr(R),
     type(R, event),
     mode(R, speech)),
  List),
  latest(Event, List).

function(Function, Event) :-
  fsr(Event),
  val(data:data:data:function, Function, Event).
  
content(Content, Event) :-
  fsr(Event),
  val(data:data:data:content, Content, Event).

/*----------------------------------------------------------------------------*
 * Event Counting Predicates
 *----------------------------------------------------------------------------*/
count(Mode, Count) :-
  findall(Event, (fsr(Event),
     mode(Event, Mode)), List),
  length(List, Count).
  
/*----------------------------------------------------------------------------*
 * Garbage Collection
 *----------------------------------------------------------------------------*/
clear(M, P) :-
  forall((fsr(R),
     val(mode, M, R),
     val(time, T, R),
     val(dist, D, R),
     val(life, L, R),
     E is T - D + L, now(Q),
     B is Q - P, B > E),
  jel(R)).

/*----------------------------------------------------------------------------*
 * Qualitative Time Predicates
 *----------------------------------------------------------------------------*/
after(A, B) :-
  val(time, TA, A),
  val(time, TB, B),
  val(dist, DA, A),
  val(dist, DB, B),
  val(life, LB, B),
  SA = TA - DA,
  SB = TB - DB,
  EB = SB + LB,
  SA > EB.

/*----------------------------------------------------------------------------*
 * Event Ordering Predicates
 *----------------------------------------------------------------------------*/
latest(R, [R]) :- !.
latest(R, [H|T]) :-
   latest(L, T),
    (
      after(L, H), !, R = L
    ;
      after(H, L), !, R = H
    ).

