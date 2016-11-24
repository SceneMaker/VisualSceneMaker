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
:- module(logic,
  [
    % Facts
    fsr/1,
    val/3,
    add/4,
    set/4,
    del/1,
    jel/1,
    add/1,
    jdd/1,
    rll/2,
    jll/2,
   % Print
    out/1,
    err/1,
    jog/1,
    % Timer
    now/1,
    init/2,
    stop/1,
    start/1,
    timer/2,
    timeout/2,
    expired/1,
    %Clean
    clean/0,
    clean/2,
    % Signals
    signal/1,
    signal/2,
    signal/3,
    remove/1,
    remove/2,
    remove/3,
    % Events
    scene/1,
    gaze/1,
    move/3,
    voice/3,
    state/3,
    speech/1,
    % Evaluation
    eq/2,
    ev/2,
    %Quantifier
    collect/3,
    arrange/3,
    %Disambiguate
    disambiguate/2,
    matches/2,
    %Temporal
    iduring/2,
    iafter/2,
    %
    max_size_list/2
  ]).

:- reexport('facts').
:- reexport('print').
:- reexport('timer').
:- reexport('clean').
:- reexport('quant').
 
/*----------------------------------------------------------------------------*
 * Comparison Predicates
 *----------------------------------------------------------------------------*/
eq(X, Y) :-
   nonvar(X),
   nonvar(Y),
   X == Y.
   
/*----------------------------------------------------------------------------*
 * Assignment Predicates
 *----------------------------------------------------------------------------*/
%set(V, N) :-
%  var(V), nonvar(N), V = N.

ev(X, Y) :-
   var(X),
   X is Y.

/*----------------------------------------------------------------------------*
 * Signalling Predicates
 *----------------------------------------------------------------------------*/
signal(Name) :-
  forall(
    (fsr(Record),
     val(type, signal, Record),
     val(name, Name, Record)),
  del(Record)), now(Time),
  add(
  [type:signal,
   name:Name,
   time:Time]).

signal(Sent, Name) :-
  forall(
    (fsr(Record),
     val(type, signal, Record),
     val(sent, Sent, Record),
     val(name, Name, Record)),
  del(Record)), now(Time),
  add(
  [type:signal,
   sent:Sent,
   name:Name,
   time:Time]).
   
signal(Sent, Name, Data) :-
  forall(
    (fsr(Record),
     val(type, signal, Record),
     val(sent, Sent, Record),
     val(name, Name, Record)),
  del(Record)), now(Time),
  add(
  [type:signal,
   sent:Sent,
   name:Name,
   data:Data,
   time:Time]).

remove(Name) :-
  fsr(Record),
  val(type, signal, Record),
  val(name, Name, Record),
  del(Record).
  
remove(Sent, Name) :-
  fsr(Record),
  val(type, signal, Record),
  val(sent, Sent, Record),
  val(name, Name, Record),
  del(Record).

remove(Sent, Name, Data) :-
  fsr(Record),
  val(type, signal, Record),
  val(sent, Sent, Record),
  val(name, Name, Record),
  val(data, Data, Record),
  del(Record).
  
/*----------------------------------------------------------------------------*
 * Voice Event Extraction
 *----------------------------------------------------------------------------*/
% This predicate finds the oldest voice event based on the end times of the
% events and sets the name, the data and the event itself before deleting it.
voice(Name, Data, Event) :-
  findall(Record,
    (fsr(Record),
     val(type, event, Record),
     val(mode, voice, Record)),
    List),
  % Get oldest based on end times
  eoldest(Event, List),
  val(name, Name, Event),
  val(data, Data, Event),
  jel(Event).
  
/*----------------------------------------------------------------------------*
 * State Event Extraction
 *----------------------------------------------------------------------------*/
state(Name, Data, Event) :-
  findall(Record,
    (fsr(Record),
     val(type, event, Record),
     val(mode, state, Record)),
    List),
  eoldest(Event, List),
  val(name, Name, Event),
  val(data, Data, Event),
  jel(Event).
  
 /*----------------------------------------------------------------------------*
 * Gaze Event Extraction
 *----------------------------------------------------------------------------*/
gaze(Event) :-
  findall(Record,
    (fsr(Record),
     val(type, event, Record),
     val(mode, gaze, Record)),
    List),
   eoldest(Event, List),
   del(Event).

/*----------------------------------------------------------------------------*
 * Move Event Extraction
 *----------------------------------------------------------------------------*/
move(N, D, E) :-
  findall(R,
    (fsr(R),
     val(type, event, R),
     val(mode, move, R),
     val(name, N, R)), L),
  eoldest(E, L),
  val(data, D, E),
  forall(
    (fsr(R),
     member(R, L)),
     jel(R)).

/*----------------------------------------------------------------------------*
 * Speech Event Extraction
 *----------------------------------------------------------------------------*/
speech(Event) :-
  findall(R,
    (fsr(R),
     type(R, event),
     mode(R, speech)),
  List),
  eoldest(Event, List).

function(Function, Event) :-
  fsr(Event),
  val(data:data:data:function, Function, Event).
  
content(Content, Event) :-
  fsr(Event),
  val(data:data:data:content, Content, Event).

/*----------------------------------------------------------------------------*
 * Scene Event Extraction
 *----------------------------------------------------------------------------*/
scene(Data) :-
  findall(Record,
    (fsr(Record),
     val(type, event, Record),
     val(name, agent, Record),
     val(mode, scene, Record)),
    List),
  eoldest(Event, List),
  val(data, Data, Event),
  jel(Event).
  
/*----------------------------------------------------------------------------*
 * Event Counting Predicates
 *----------------------------------------------------------------------------*/
count(Mode, Count) :-
  findall(Event, (fsr(Event),
     mode(Event, Mode)), List),
  length(List, Count).


/*----------------------------------------------------------------------------*
 * Time/Ordering Predicates
 *----------------------------------------------------------------------------*/
iafter(A, B) :-
  val(time, TA, A),
  val(time, TB, B),
  val(dist, DA, A),
  val(dist, DB, B),
  val(life, LB, B),
  SA = TA - DA,
  SB = TB - DB,
  EB = SB + LB,
  SA > EB.
  
ibefore(A, B) :-
  val(time, TA, A),
  val(time, TB, B),
  val(dist, DA, A),
  val(dist, DB, B),
  val(life, LA, A),
  SA = TA - DA,
  SB = TB - DB,
  EA = SA + LA,
  EA < SB.

iduring(A, B) :-
  val(time, TA, A),
  val(time, TB, B),
  val(dist, DA, A),
  val(dist, DB, B),
  val(life, LA, A),
  val(life, LB, B),
  SA = TA - DA,
  EA = SA + LA,
  SB = TB - DB,
  EB = SB + LB,
  SA > SB,
  EA < EB.

inewest(R, [R]) :- !.
inewest(R, [H|T]) :-
   inewest(L, T),
    (
      iafter(L, H), !, R = L
    ;
      iafter(H, L), !, R = H
    ).



eoldest(R, [R]) :- !.
eoldest(R, [H|T]) :-
   eoldest(L, T),
    (
      ebefore(L, H), !, R = L
    ;
      ebefore(H, L), !, R = H
    ).
    
ebefore(A, B) :-
  val(time, TA, A),
  val(time, TB, B),
  val(dist, DA, A),
  val(dist, DB, B),
  val(life, LA, A),
  val(life, LB, B),
  EA = TA - DA + LA,
  EB = TB - DB + LB,
  EA =< EB.
  

%set(Path, Value, Input) :-
%  fsr(Input), set(Path, Value, Input, Output), add(Output), del(Input).
  
%Works only for propositional questions
disambiguate(Speech, Fused) :-
  % Check if the question is a set or check type
  val(data:cat, Cat, Speech), out(Cat), nl,
  % Check if the question has a location reference
  val(data:data:locref, Ref, Speech), out(Ref), nl, !,
  % Get all other features from the speech event
  %(val(data:data:size, Size, Speech)  -> out(Size); out('no size')), nl,
  %(val(data:data:shape, Shape, Speech)-> out(Shape); out('no shape')), nl,
  %(val(data:data:color, Color, Speech)-> out(Color); out('no color')), nl,
  %TODO: filter such that the other attributes match
  
  forlargest(Gaze, (fsr(Gaze), % For the majority of events
    val(mode, gaze, Gaze),     % from the gaze modality
    iduring(Gaze, Speech),     % during the speech event
    matches(Gaze, Speech)),    % whose features match

    % holds that they have the name
    val(data:name, Name, Gaze)),
    
  write('Gaze Target Name:'), out(Name), nl,
  add(data:data:name, Name, Speech, Fused).

disambiguate(Event, Event).

% Check if the features of a speech event match with those of the piece with the name
matches(Gaze, Speech):-
  val(data:name, Name, Gaze), out(Name), nl,
  fsr(Piece), val(name, Name, Piece), out(Piece), nl,
  (val(data:data:size, Size, Speech)   -> out(Size), nl, val(data:size, Size, Piece); true),
  (val(data:data:color, Color, Speech) -> out(Color), nl,val(data:color, Color, Piece); true),
  (val(data:data:shape, Shape, Speech) -> out(Shape), nl,val(data:shape, Shape, Piece); true).


%fsr(SpeechEvent), val(mode, speech, SpeechEvent), val(data:data:shape, square, SpeechEvent), disambiguate(SpeechEvent, FusedEvent), out(FusedEvent).
/*
fsr(SpeechEvent), val(mode, speech, SpeechEvent), val(data:data, [color:red,shape:square,location:[]], SpeechEvent), disambiguate(SpeechEvent, FusedEvent), out(FusedEvent).
Speech Event:[
    type:event,
    name:agent,
    mode:speech,
    data:[
        type:dialog_act,
        cat:info_seeking,
        fun:propositional,
        data:[
            color:red,
            shape:square,
            location:[]
        ]
    ],
    time:9000,
    from:2000,
    life:1980,
    conf:1.0
]
square
red
Gaze Target:[
    type:puzzle_piece,
    name:obj3,
    data:[
        color:red,
        shape:square
    ]
]
[
    type:event,
    name:agent,
    mode:speech,
    data:[
        type:dialog_act,
        cat:info_seeking,
        fun:propositional,
        data:[
            fixation:[
                type:puzzle_piece,
                name:obj3,
                data:[
                    color:red,
                    shape:square
                ]
            ],
            color:red,
            shape:square,
            location:[]
        ]
    ],
    time:9000,
    from:2000,
    life:1980,
    conf:1.0
]
SpeechEvent = [type:event, name:agent, mode:speech, data:[type:dialog_act, cat:info_seeking, fun:propositional, ... : ...], time:9000, from:2000, life:1980, conf:1.0],
FusedEvent = [type:event, name:agent, mode:speech, data:[type:dialog_act, cat:info_seeking, fun:propositional, ... : ...], time:9000, from:2000, life:1980, conf:1.0] ;
false.
*/