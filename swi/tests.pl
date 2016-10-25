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
      construct_fact_base/0
    ]).

:- reexport('facts').
:- reexport('logic').
:- reexport('print').

% SWI documentation server
%:- doc_server(19239).

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
construct_fact_base :-
%add([type:event,name:user,mode:voice,data:start,time:7315,from:129,life:0,conf:1.0]),  %end 7186
%add([type:event,name:user,mode:voice,data:stop,time:8591,from:1429,life:720,conf:1.0]),%end 7882
%add([type:event,name:user,mode:voice,data:start,time:12196,from:168,life:0,conf:1.0]), %end 12028
%add([type:event,name:user,mode:voice,data:stop,time:13092,from:1070,life:390,conf:1.0]),%end 12412
add([type:event,name:agent,mode:voice,data:start,time:8000,from:0,life:0,conf:1.0]), %end 8000
add([type:event,name:agent,mode:voice,data:stop,time:9000,from:0,life:0,conf:1.0]).   %end 9000




% add([type:event, mode:gaze, data:[type:object, name:chest, conf:0.95]]),
% add([type:event, mode:gaze, data:[area:chest, name:c2, conf:0.95]]),
% add([type:event, mode:gaze, data:[area:chest, name:c3, conf:0.95]]),
% add([type:event, mode:gaze, data:[area:shoulder, name:s1, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:shoulder, name:s2, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:jaw, name:j1, conf:0.75]]),
% add([type:event, mode:gaze, data:[area:jaw, name:j2, conf:0.75]]),
% add([type:event, mode:gaze, data:[area:jaw, name:j3, conf:0.75]]),
% add([type:event, mode:gaze, data:[area:jaw, name:j4, conf:0.75]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f1, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f2, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f3, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f4, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f5, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f6, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f7, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f8, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f9, conf:0.85]]),
% add([type:event, mode:gaze, data:[area:forehead, name:f10, conf:0.85]]).