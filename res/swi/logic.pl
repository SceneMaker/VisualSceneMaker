﻿/*  This code is part of the Behavior Flow Query Language based on SWI-Prolog.

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
  [ % Term Manipulation
   val/3,
   fsr/1,
   del/1,
   jel/1,
   add/1,
   jdd/1,
   out/1,
   err/1,
   jog/1,
   % Helper Predicates
   now/1,
   set/2,
   rll/2,
   jll/2,
   % General Predicates
   type/2,
   name/2,
   sent/2,
   mode/2,
   dist/2,
   life/2,
   time/2,
   conf/2,
   data/2,
   % Attribute Lists
   attr/3,
   % Temporal Interval Predicates
   after/2,
   % Event Detection Predicates
   speech/1,
   function/2,
   content/2,
   % Event Counting Predicates
   count/2,
   %
   uaid/2,
   lang/2,
   prio/2,
   talk/2,
   gaze/2,
   role/2,
   part/2,
   agent/2
  ]).

:- reexport('facts').
:- reexport('print').

/*----------------------------------------------------------------------------*
 * General Helper Predicates
 *----------------------------------------------------------------------------*/
type(R, V) :-
  val(type, V, R).
   
name(R, V) :-
  val(name, V, R).

sent(R, V) :-
  val(sent, V, R).
  
recv(R, V) :-
  val(recv, V, R).

mode(R, V) :-
  val(mode, V, R).

dist(R, V) :-
  val(dist, V, R).

life(R, V) :-
  val(life, V, R).

time(R, V) :-
  val(time, V, R).

conf(R, V) :-
  val(conf, V, R).
   
data(R, V) :-
  val(data, V, R).

uttr(R, V) :-
  val(uttr, V, R).

corr(R, V) :-
  val(corr, V, R).

uaid(R, V) :-
  val(uaid, V, R).
  
lang(R, V) :-
  val(lang, V, R).

prio(R, V) :-
  val(prio, V, R).
  
role(R, V) :-
  val(role, V, R).
  
part(R, V) :-
  val(part, V, R).
  
gaze(R, V) :-
  val(gaze, V, R).
  
talk(R, V) :-
  val(talk, V, R).

/*----------------------------------------------------------------------------*
 * Record Attribute Predicates
 *----------------------------------------------------------------------------*/
agent(V, R) :-
  fsr(R), type(R, agent), name(R, V).
  
event(V, R) :-
  fsr(R), type(R, event), name(R, V).

set(P, R, V) :-
  fsr(R), set(P, V, R, S), add(S), del(R).

/*----------------------------------------------------------------------------*
 * Attribute List Predicates
 *----------------------------------------------------------------------------*/
attr(N, D, [H|_]) :-
  H = [type:attribute, name:N, data:D].
attr(N, D, [_|T]) :-
  attr(N, D, T).

/*----------------------------------------------------------------------------*
 * Speech Event Predicates
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
 * Garbage Collecting Predicates
 *----------------------------------------------------------------------------*/
clear(Mode, Period) :-
  jog('CLEARING'),  
  forall((fsr(Event),
     mode(Event, Mode), time(Event, Time),
     dist(Event, Dist), life(Event, Life),
     End is Time - Dist + Life, now(Now),
     Bound is Now - Period, Bound > End),
  jel(Event)).

/*----------------------------------------------------------------------------*
 * Qualitative Time Predicates
 *----------------------------------------------------------------------------*/
after(A, B) :-
  time(A, TA),
  time(B, TB),
  dist(A, DA),
  dist(B, DB),
  life(B, LB),
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

