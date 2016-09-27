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
:- module(tests, []).

:- reexport('facts').
:- reexport('logic').
:- reexport('print').

% SWI documentation server
%:- doc_server(19239).

/*----------------------------------------------------------------------------*
 *
 *----------------------------------------------------------------------------*/
construct_fact_base :-
add([type:event, mode:gaze, data:[type:object, name:chest, conf:0.95]]),
add([type:event, mode:gaze, data:[area:chest, name:c2, conf:0.95]]),
add([type:event, mode:gaze, data:[area:chest, name:c3, conf:0.95]]),
add([type:event, mode:gaze, data:[area:shoulder, name:s1, conf:0.85]]),
add([type:event, mode:gaze, data:[area:shoulder, name:s2, conf:0.85]]),
add([type:event, mode:gaze, data:[area:jaw, name:j1, conf:0.75]]),
add([type:event, mode:gaze, data:[area:jaw, name:j2, conf:0.75]]),
add([type:event, mode:gaze, data:[area:jaw, name:j3, conf:0.75]]),
add([type:event, mode:gaze, data:[area:jaw, name:j4, conf:0.75]]),
add([type:event, mode:gaze, data:[area:forehead, name:f1, conf:0.85]]),
add([type:event, mode:gaze, data:[area:forehead, name:f2, conf:0.85]]),
add([type:event, mode:gaze, data:[area:forehead, name:f3, conf:0.85]]),
add([type:event, mode:gaze, data:[area:forehead, name:f4, conf:0.85]]),
add([type:event, mode:gaze, data:[area:forehead, name:f5, conf:0.85]]),
add([type:event, mode:gaze, data:[area:forehead, name:f6, conf:0.85]]),
add([type:event, mode:gaze, data:[area:forehead, name:f7, conf:0.85]]),
add([type:event, mode:gaze, data:[area:forehead, name:f8, conf:0.85]]),
add([type:event, mode:gaze, data:[area:forehead, name:f9, conf:0.85]]),
add([type:event, mode:gaze, data:[area:forehead, name:f10, conf:0.85]]).