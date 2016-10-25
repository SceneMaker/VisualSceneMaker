/** <module> Solution Collection Predicates

This library provides certain solution collection predicates such as collect/3
and arrange/3 that are based on the built-in prediates bagof/3 and setof/3 or
als findall/3 and can be used to collect all solutions to a goal with unbound
variable. Like the bagof/3 and setof/3 predicates most of them are designed
to deal with existential qualified variables (Var^Goal) and prove multiple
solutions for remaining free variables in the goal.

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
:- module(collections,
  [
    collect/3,
    arrange/3
  ]).
  
:- reexport('thefactbase').
:- reexport('assignments').
:- reexport('featstructs').
:- reexport('inspections').
:- reexport('domspecific').
:- reexport('prettyprint').
:- reexport('systemtimes').
:- reexport('thedebugger').
:- reexport('quantifiers').

collect(T, G, C) :-
    bagof(T, G, L)
    *->
        C = L
    ;
        C = [].

arrange(T, G, C) :-
    setof(T, G, L)
    *->
        C = L
    ;
        C = [].

/* TODO:
 Make versions of collect/arrange/... without the template parameter
 by finding the template inspecting the variables in scope and range
*/