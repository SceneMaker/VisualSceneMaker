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
:- module(print,
  [
    out/1,
    err/1,
    jog/1,
    jvw/2
  ]).

/*----------------------------------------------------------------------------*
 * Print a feature structure to the Prolog console
 *----------------------------------------------------------------------------*/
out(R) :- % Print To Stdout
  vwrite(R, '', user_output).

err(R) :- % Print To Stderr
  vwrite(R, '', user_error).

/*----------------------------------------------------------------------------*/

vwrite(V, _, S) :- % Print A Variable Value
  var(V), !, write(S, V).

vwrite([H|T], I, S) :- % Print A Matrix Value
  !,
  write(S, '['), nl(S),
  string_concat(I, '    ', J),
  lwrite([H|T], J, S),
  nl(S), write(S, I), write(S, ']').

vwrite(V, _, S) :- % Print A Simple Value
  !, write(S, V).

lwrite([H], I, S) :- % Print A Whole List
  !, ewrite(H, I, S).

lwrite([H|T], I, S) :- % Print A List Member
  !,
  ewrite(H, I, S),
  write(S, ','), nl(S),
  lwrite(T, I, S).

ewrite(F:V, I, S) :- % Print A Pair Member
  !,
  write(S, I),
  write(S, F),
  write(S, ':'),
  vwrite(V, I, S).

ewrite(E, I, S) :- % Print Simple Member
  !,
  write(S, I),
  vwrite(E, I, S).

/*----------------------------------------------------------------------------*
 * Print a feature structure to the Java logger
 *----------------------------------------------------------------------------*/
jvw(R, M) :-
  jvwrite(R, '', '', M).
  
jog(R) :-
   jvwrite(R, '', '', M),
   jpl_call('de.dfki.vsm.util.log.LOGDefaultLogger', getInstance, [], L),
   jpl_call(L, message, [M], _).

/*----------------------------------------------------------------------------*/

jvwrite(V, _, O, N) :- % Print A Variable Value
  var(V), !, concat(O, V, N).

jvwrite([H|T], I, O, N) :- % Print A Matrix Value
  !,
  concat(I, '    ', J),
  concat(O, '[\n', Z1),
  %concat(Z1, 'X', Z2),
  jlwrite([H|T], J, Z1, Z2),
  concat(Z2, '\n', Z3),
  concat(Z3, I, Z4),
  concat(Z4, ']', N).
  
jvwrite([], _, O, N) :- % Print An Empty List
  !, concat(O, '[]', N).

jvwrite(V, _, O, N) :- % Print A Simple Value
  !, concat(O, V, N).

jlwrite([H], I, O, N) :- % Print A Whole List
  !, jewrite(H, I, O, N).

jlwrite([H|T], I, O, N) :- % Print A List Member
  !,
  jewrite(H, I, O, Z1),
  concat(Z1, ',', Z2),
  concat(Z2, '\n', Z3),
  jlwrite(T, I, Z3, N).

jewrite(F:V, I, O, N) :- % Print A Pair Member
  !,
  concat(O, I, Z1),
  concat(Z1, F, Z2),
  concat(Z2, ':', Z3),
  jvwrite(V, I, Z3, N).

jewrite(E, I, O, N) :- % Print Simple Member
  !,
  concat(O, I, Z1),
  jvwrite(E, I, Z1, N).

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/