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
      fact_base/0,
      reset/0
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
add([type:entity,sort:piece,name:p1,data:[type:marker, size:small, color:yellow, shape:triangle, pos:[x:0, y:0], state:absent],desc:'the large yellow square']),
add([type:entity,sort:piece,name:p2,data:[type:marker, size:small, color:green, shape:triangle, pos:[x:0, y:0], state:absent],desc:'the small green triangle']),
add([type:entity,sort:piece,name:p3,data:[type:marker, size:small, color:yellow, shape:square, pos:[x:0, y:0], state:absent],desc:'the large red star']),
add([type:entity,sort:piece,name:p4,data:[type:marker, size:small, color:green, shape:triangle, pos:[x:0, y:0], state:absent],desc:'the large red star']),
   
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:5000,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:5100,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:5200,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:5300,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:5400,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:5500,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p3, conf:1.0],time:5600,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p3, conf:1.0],time:5700,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:5800,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:5900,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:6000,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:6100,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:6200,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:6300,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:6400,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:6500,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:6600,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:6700,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:6800,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p3, conf:1.0],time:6900,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p3, conf:1.0],time:7000,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p3, conf:1.0],time:7100,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:7200,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:7300,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:7400,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:7500,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:7600,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:7700,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:7800,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:7900,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p3, conf:1.0],time:8000,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p3, conf:1.0],time:8100,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p3, conf:1.0],time:8200,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p3, conf:1.0],time:8300,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:8400,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:8500,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:8600,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:8700,dist:100,life:100,conf:1.0]),
add([type:event,name:user,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:8800,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p2, conf:1.0],time:8900,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:gaze,data:[type:entity, sort:piece, name:p1, conf:1.0],time:9000,dist:100,life:100,conf:1.0]),
add([type:event,name:agent,mode:speech, id:1,
     data:[type:dialog_act,fun:info_seeking,cat:check_question,
         data:[color:yellow,shape:square,locref:here]],
     time:6000,dist:2000,life:2000,conf:1.0]),
add([type:event,name:agent,mode:speech, id:2,
     data:[type:dialog_act,fun:info_seeking,cat:check_question,
         data:[size:small,color:green,shape:triangle]],
     time:7000,dist:500,life:500,conf:1.0]),
add([type:event,name:agent,mode:speech, id:3,
     data:[type:dialog_act,fun:info_seeking,cat:check_question,
         data:[size:small,shape:triangle,locref:'over there']],
     time:9000,dist:9000,life:9000,conf:1.0]).
     







test :-
add([mode:1, time:1000,dist:100,life:100]),
add([mode:2, time:2000,dist:100,life:100]),
add([mode:1, time:3000,dist:100,life:100]),
add([mode:1, time:4000,dist:100,life:100]),
add([mode:2, time:5000,dist:100,life:100]),
add([mode:3, time:6000,dist:100,life:100]),
add([mode:1, time:7000,dist:100,life:100]),
add([mode:2, time:8000,dist:100,life:100]),
add([mode:1, time:9000,dist:100,life:100]),
followers([mode:1, time:3000,dist:100,life:100], L1), out(L1),
ancestors([mode:2, time:8000,dist:100,life:100], L2), out(L2),
interims([mode:2, time:8000,dist:100,life:100], [mode:2, time:2000,dist:100,life:100], L3), out(L3).


