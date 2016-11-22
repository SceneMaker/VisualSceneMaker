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
:- module(tests,
    [
      fact_base/0
    ]).

:- reexport('facts').
:- reexport('logic').
:- reexport('print').

% SWI documentation server
%:- doc_server(19239).

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
fact_base :-
%Start Triangle (obj1)
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:5000,from:20,life:100,conf:0.75]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:5100,from:20,life:100,conf:0.85]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:5200,from:20,life:100,conf:0.95]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:5300,from:20,life:100,conf:0.95]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:5400,from:20,life:100,conf:0.75]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:5500,from:20,life:100,conf:0.75]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj3, data:[color:red,shape:square]],time:5600,from:20,life:100,conf:0.85]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj3, data:[color:red,shape:square]],time:5700,from:20,life:100,conf:0.95]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:5800,from:20,life:100,conf:0.95]),
% End Triangle

add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:5900,from:20,life:100,conf:0.75]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:6000,from:20,life:100,conf:0.75]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:6100,from:20,life:100,conf:0.75]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:6200,from:20,life:100,conf:0.95]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:6300,from:20,life:100,conf:0.95]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:6400,from:20,life:100,conf:0.75]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:6500,from:20,life:100,conf:0.75]),

% Start Star  (obj2)
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:6600,from:20,life:100,conf:0.85]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:6700,from:20,life:100,conf:0.95]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:6800,from:20,life:100,conf:0.95]),
% End Star

add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj3, data:[color:red,shape:square]],time:6900,from:20,life:100,conf:0.85]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj3, data:[color:red,shape:square]],time:7000,from:20,life:100,conf:0.75]),

% Start Square  (obj1)
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj3, data:[color:red,shape:square]],time:7100,from:20,life:100,conf:0.75]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:7200,from:20,life:100,conf:0.95]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:7300,from:20,life:100,conf:0.95]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:7400,from:20,life:100,conf:0.75]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:7500,from:20,life:100,conf:0.85]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:7600,from:20,life:100,conf:0.95]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:7700,from:20,life:100,conf:0.85]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:7800,from:20,life:100,conf:0.75]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:7900,from:20,life:100,conf:0.95]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj3, data:[color:red,shape:square]],time:8000,from:20,life:100,conf:0.75]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj3, data:[color:red,shape:square]],time:8100,from:20,life:100,conf:0.95]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj3, data:[color:red,shape:square]],time:8200,from:20,life:100,conf:0.75]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj3, data:[color:red,shape:square]],time:8300,from:20,life:100,conf:0.85]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:8400,from:20,life:100,conf:0.95]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:8500,from:20,life:100,conf:0.75]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:8600,from:20,life:100,conf:0.95]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:8700,from:20,life:100,conf:0.75]),
add([type:event,name:user,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:8800,from:20,life:100,conf:0.85]),
%End Square

add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj2, data:[color:yellow,shape:star]],time:8900,from:20,life:100,conf:0.95]),
add([type:event,name:agent,mode:gaze,data:[type:puzzle_piece, name:obj1, data:[color:green,shape:triangle]],time:9000,from:20,life:100,conf:0.85]),

add([type:event,name:agent,mode:speech,data:[type:dialog_act,cat:info_seeking,fun:propositional,data:[color:green,shape:triangle,location:[]]],time:6000,from:2020,life:1980,conf:1.0]),
add([type:event,name:agent,mode:speech,data:[type:dialog_act,cat:info_seeking,fun:propositional,data:[color:yellow,shape:star,location:[]]],time:7000,from:520,life:480,conf:1.0]),
add([type:event,name:agent,mode:speech,data:[type:dialog_act,cat:info_seeking,fun:propositional,data:[color:red,shape:square,location:[]]],time:9000,from:2000,life:1980,conf:1.0]).

