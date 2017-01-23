:- module('logic', [
  % Clean and Garbage Collection
  reset/0, clean/0, clean/2,
  % Time and Timeout Handling
  now/1, timeout/2,
  % Signal Production and Detection
  signal/2, signal/3, detect/2, detect/3,
  % Modality-Specific Data Extraction
  state/2, voice/2, gaze/2, touch/4, speech/3, action/1, face/1, head/1,
  % Object Update And Disambiguation
  update/3, disambiguate/2 ]).
:- reexport('facts').
:- reexport('print').
:- reexport('quant').
:- reexport('timer').
:- reexport('clean').

/* Signal Production and Detection */
signal(Mode, Name) :-
  forall((fsr(Record),
    val('type', 'signal', Record),
    val('mode', Mode, Record),
    val('name', Name, Record)),
  del(Record)), now(Time),
  add(['type':'signal',
       'mode':Mode,
       'name':Name,
       'time':Time]).
       
signal(Mode, Name, Data) :-
  forall((fsr(Record),
    val('type', 'signal', Record),
    val('mode', Mode, Record),
    val('name', Name, Record)),
  del(Record)), now(Time),
  add(['type':'signal',
       'mode':Mode, 'name':Name,
       'data':Data, 'time':Time]).

detect(Mode, Name) :-
  fsr(Record),
  val('type', 'signal', Record),
  val('mode', Mode, Record),
  val('name', Name, Record),
  del(Record).
  
detect(Mode, Name, Data) :-
  fsr(Record),
  val('type', 'signal', Record),
  val('mode', Mode, Record),
  val('name', Name, Record),
  val('data', Data, Record),
  del(Record).

/* Modality-Specific Data Extraction */
state(Name, Data) :-
  oldest('state', Event),
  val('name', Name, Event),
  val('data', Data, Event).
  
voice(Name, Data) :-
  oldest('voice', Event),
  val('name', Name, Event),
  val('data', Data, Event).
  
gaze(Name, Data) :-
  oldest('gaze', Event),
  val('name', Name, Event),
  val('data':'name', Data, Event).
  
touch(Type, Name, Xpos, Ypos) :-
  oldest('touch', Event),
  val('data':'type', Type, Event),
  val('data':'name', Name, Event),
  val('data':'pos':'x', Xpos, Event),
  val('data':'pos':'y', Ypos, Event).
  
speech(Event, Fun, Cat) :-
  oldest('speech', Event),
  val('data':'fun', Fun, Event),
  val('data':'cat', Cat, Event).
  
action(Data) :-
  oldest('action', Event),
  val('data', Data, Event).
  
face(Data) :-
  oldest('face', Event),
  val('data', Data, Event).
  
head(Data) :-
  oldest('head', Event),
  val('data', Data, Event).

/* Event Ordering Predicates */
oldest(Mode, Event) :-
  findall(Record, (fsr(Record),
    val('type', 'event', Record),
    val('mode', Mode, Record)), List),
  eoldest(Event, List), jel(Event).

eoldest(R, [R]) :- !.
eoldest(R, [H|T]) :-
  eoldest(L, T),
  ( ebefore(L, H), !, R = L
  ; ebefore(H, L), !, R = H ).

ebefore(A, B) :-
  val('time', TA, A),
  val('time', TB, B),
  val('dist', DA, A),
  val('dist', DB, B),
  val('life', LA, A),
  val('life', LB, B),
  EA = TA - DA + LA,
  EB = TB - DB + LB,
  EA =< EB.

/* Speech Disambiguation */
disambiguate(Speech, Fused) :-
  % Check if the speech event's dialog act
  % is a question and has the check category
  val('data':'fun', 'info_seeking', Speech),
  val('data':'cat', 'check_question', Speech),
  % Check if the question has a location reference
  val('data':'data':'locref', _, Speech),
  % Get name of the majority of gaze events
  forlargest(Gaze, (fsr(Gaze), % For the majority of events
    val('mode', 'gaze', Gaze), % from the gaze modality
    iduring(Gaze, Speech),     % during the speech event
    matches(Gaze, Speech)),    % whose features match
    % holds that they have the name
    val('data':'name', Name, Gaze)), !,
  set('data':'data':'name', Name, Speech, Fused).

disambiguate(Speech, Fused) :-
  findall(Name, (fsr(Piece),
    val('sort', 'piece', Piece),
    matches(Piece, Speech),
    val('name', Name, Piece)),
  List),
  set('data':'data':'name', List, Speech, Fused).

matches(Gaze, Speech):-
  val('mode', 'gaze', Gaze), !,   % Check if we have a gaze event
  val('data':'name', Name, Gaze), % Get the name of the gaze target
  fsr(Piece), val('name', Name, Piece), % Get the respective piece
  % Check if the values of those attributes that are provided with
  % the speech event match with the attributes of that puzzle piece
  (val('data':'data':'size', Size, Speech)
    -> val('data':'size', Size, Piece); true), % Check the size
  (val('data':'data':'color', Color, Speech)
    -> val('data':'color', Color, Piece); true), % Check the color
  (val('data':'data':'shape', Shape, Speech)
    -> val('data':'shape', Shape, Piece); true). % Check the shape

matches(Piece, Speech):-
  val('sort', 'piece', Piece), !, % Check if we have a piece here
  % Check if the values of those attributes that are provided with
  % the speech event match with the attributes of that puzzle piece
  (val('data':'data':'size', Size, Speech)
    -> val('data':'size', Size, Piece); true), % Check the size
  (val('data':'data':'color', Color, Speech)
    -> val('data':'color', Color, Piece); true), % Check the color
  (val('data':'data':'shape', Shape, Speech)
    -> val('data':'shape', Shape, Piece); true). % Check the shape

/* Puzzle Piece Update */
update(Name, Xpos, Ypos) :-
  fsr(Old),
  val('type', 'entity', Old),
  val('sort', 'piece', Old),
  val('name', Name, Old),
  set('data':'state', 'present', Old, Temp),
  set('data':'pos', ['x':Xpos, 'y':Ypos], Temp, New),
  del(Old), add(New).

