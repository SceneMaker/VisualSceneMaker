/** <module> User Defined operators

This library provides user-defined operators on feature structure records.

This library has been developed as part of the _|Behavior Flow Query Language|_
at the lab for _|Human Centered Multimedia|_ of the Faculty of Applied Computer
Sciences at the University of Augburg. Please see _| http://www.hcm-lab.de /|_
for more information. The library has been developed by Gregor Mehlmann and is
published under the GNU General Public License.

@author Gregor Mehlmann
@copyright 2008-2018,
  University of Augsburg,
  Applied Computer Sciences,
  Human Centered Multimedia.
@license GNU General Public License.
*/
:- module(domspecific,
  [ at/3 ]).

:- reexport('featstructs').

at([], _, _) :- write('empty'),!, fail.
at([H|_], 0, H) :- write('end'), !.
at([_|T], I, H) :-  write('head'),
  I > 0, J is I - 1, write(J), write(T), !, at(T, J, H).

data(I, E, V) :- val(data, D, E), !, at(D, I, V).

/*
%:- op(700, xfx, user:(at)).
%:- op(700, xfx, user:(in)).
%:- op(800, xfy, user:(has)).
%:- op(800, xfy, user:(iss)).
%M has (V at F) :- val(F, V, M).
%F iss (V in M) :- val(F, V, M).
*/