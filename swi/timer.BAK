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
:- module(timer,
  [
    now/1,
    init/2,
    stop/1,
    start/1,
    timer/2,
    timeout/2,
    timeout/3,
    expired/1
    ]).
    
:- reexport('facts').

/*----------------------------------------------------------------------------*
 * The Current System Time
 *----------------------------------------------------------------------------*/
:- dynamic start/1.
:- dynamic timer/2.

now(Now) :-
  start(Start), !,
  statistics(walltime, [Time, _]),
  Now is Time - Start.
  
now(Now) :-
  statistics(walltime, [Time, _]),
  assertz(start(Time)), Now is 0.

/*----------------------------------------------------------------------------*
 * The Timeout Predicates
 *----------------------------------------------------------------------------*/
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
  val(time, Time, Event),
  val(dist, Dist, Event),
  val(life, Life, Event),
  End is Time - Dist + Life + Delay,
  assertz(timer(Name, End)), fail.
  
