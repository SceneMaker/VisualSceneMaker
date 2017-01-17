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
:- module(clean,
  [
    clean/0,
    clean/2
    ]).
    
:- reexport('facts').
:- reexport('timer').

/*----------------------------------------------------------------------------*
 * Garbage Predicates
 *----------------------------------------------------------------------------*/
clean(Mode, Age) :-
  now(Now), Lim is Now - Age,
  forall((fsr(Record),
     val(mode, Mode, Record),
     val(time, Time, Record),
     val(from, Dist, Record),
     val(life, Life, Record),
     End is Time - Dist
          + Life, Lim > End),
  retract(fsr(Record))).

/*----------------------------------------------------------------------------*
 * Cleanup Predicates
 *----------------------------------------------------------------------------*/
clean :-
  write('Clean Fact Base'), nl,
  retractall(timer(_,_)),
  retractall(start(_)),
  forall((fsr(R),
    val(type, entity, R)),
    retract(fsr(R))),
  forall((fsr(R),
    val(type, event, R)),
    retract(fsr(R))),
  forall((fsr(R),
    val(type, signal, R)),
    retract(fsr(R))).