:- module(logic,
  [ % Term Manipulation
   val/3,
   fsr/1,
   del/1,
   jel/1,
   add/1,
   jdd/1,
   out/1,
   err/1,
   jog/1,
   % Helper Predicates
   now/1,
   set/2,
   rll/2,
   jll/2,
   % General Predicates
   type/2,
   name/2,
   sent/2,
   mode/2,
   dist/2,
   life/2,
   time/2,
   conf/2,
   data/2,
   % Touch Predicates
   touch/7,
   % Gaze Predicates
   gaze/1,
   gazes/1,
   find_all_gaze/1,
   latest_of_gaze/1,
   latest_of_agaze/1,
   % Entity Predicates
   entityfs/2,
   entityactfs/3,
   entityattr/3,
   findbyattr/3,
   matchfs/2,
   matchid/2,
   sharedattr/3,
   nonsharedattr/4,
   removeattr/4,
   replaceattr/5,
   updateattr/4,
   updatepos/5,
   updatemarker/7,
   entitypos/3,
   fieldcenter/3,
   insidefield/3,
   % Speech Predicates
   dialogueact/2,
   actfromscore/2,
   category/3,
   function/3,
   command/3,
   contents/2,
   descript/3,
   numdescript/2,
   keyphrase/3,
   speech/6,
   % Attribute Lists
   data/3,
   % Instruction Predicates
   instructfs/2,
   instruct/6,
   % matchinstruct_attr/3,
   % matchinstruct_marker/2,
   % matchinstruct_pos/4,
   % Control Message Predicates
   cntrl/2,
   fieldnum/1,
   markernum/1,
   instructnum/1,
   rllfields/0,
   rllmarkers/0,
   rllinstruct/0]).

:- use_module(facts).

/*----------------------------------------------------------------------------*
 * Current System Time
 *----------------------------------------------------------------------------*/
:- dynamic now/1.

/*----------------------------------------------------------------------------*
 * Variable Assignment
 *----------------------------------------------------------------------------*/
set(V, V).

/*----------------------------------------------------------------------------*
 * Retract All With Features
 *----------------------------------------------------------------------------*/
rll(P, V) :-
  forall((fsr(R), val(P, V, R)), del(R)).
    
/*----------------------------------------------------------------------------*
 * Retract With Features
 *----------------------------------------------------------------------------*/
jll(P, V) :-
  forall((fsr(R), val(P, V, R)), jel(R)).
  
/*----------------------------------------------------------------------------*
 * General Helper Predicates
 *----------------------------------------------------------------------------*/
type(R, V) :-
  val(type, V, R).
   
name(R, V) :-
  val(name, V, R).

sent(R, V) :-
  val(sent, V, R).

mode(R, V) :-
  val(mode, V, R).

dist(R, V) :-
  val(dist, V, R).

life(R, V) :-
  val(life, V, R).

time(R, V) :-
  val(time, V, R).

conf(R, V) :-
  val(conf, V, R).
   
data(R, V) :-
  val(data, V, R).

uttr(R, V) :-
  val(uttr, V, R).

corr(R, V) :-
  val(corr, V, R).
  
/*----------------------------------------------------------------------------*
 * Attribute List Predicates
 *----------------------------------------------------------------------------*/
data(N, D, [H|_]) :-
  H = [type:attribute, name:N, data:D].
data(N, D, [_|T]) :-
  data(N, D, T).
  
/*----------------------------------------------------------------------------*
 * Touch Event Predicates
 *----------------------------------------------------------------------------*/
touch(Event, Name, EntityType, Id, Time, XPos, YPos) :-
  fsr(Event),
  type(Event, event),
  mode(Event, touch),
  time(Event, Time),
  data(Event, EventData),
  val(name, Name, EventData),
  data(EventData, DataList),
  data(id, Id, DataList),
  data(xpos, XPos, DataList),
  data(ypos, YPos, DataList),
  data(type, EntityType, DataList).


/*----------------------------------------------------------------------------*
 * Speech Event Predicates
 *----------------------------------------------------------------------------*/

% get the dialogue act
 dialogueact(Event, Act) :-
  data(Event, EventData),
  data(EventData, DataList),
  nth1(1, DataList, Score),
  actfromscore(Score, Act).
  
% get the dialogue act (included among keyphrases and/or orphaned words) 
actfromscore(Score, Act) :-
  data(Score, ScoreList),
  member(Act, ScoreList),
  is_list(Act),
  val(type, dialogueact, Act).
  
% get the dialogue act (the one and only member of the score data) 
actfromscore(Score, Act) :-
  data(Score, Act),
  is_list(Act),
  val(type, dialogueact, Act).
   
% get the nth category
category(N, Act, CategoryName) :-
  val(category:name, CategoryList, Act),
  is_list(CategoryList),
  nth1(N, CategoryList, CategoryName).

% get the one and only category (N=1)
category(1, Act, CategoryName) :-
  val(category:name, CategoryName, Act),
  \+is_list(CategoryName).
  
% get the nth function / the index of a specific function
function(N, Act, FunctionName) :-
  val(function:name, FunctionList, Act),
  is_list(FunctionList),
  nth1(N, FunctionList, FunctionName).
  
% get the one and only function (N=1)
function(1, Act, FunctionName) :-
  val(function:name, FunctionName, Act),
  \+is_list(FunctionName).

% get the nth command / the index of a specific command
command(N, Act, CommandName) :-
  val(commands:name, CommandList, Act),
  is_list(CommandList),
  nth1(N, CommandList, CommandName).

% get the one and only command (N=1)
command(1, Act, CommandName) :-
  val(commands:name, CommandName, Act),
  \+is_list(CommandName).


contents(Act, Contents) :- % Get The Contents
  val(contents:data, Contents, Act).
  
% get n-th description
descript(N, Contents, Description) :-
  nth1(N, Contents, Description),
  val(type, description, Description).
  
% get the one and only description
descript(1, Contents, Description) :-
  val(type, description, Contents),
  set(Description, Contents).

% get the number of descriptions in the dialogue act's content
% (>1, assuming it contains only descriptions)
numdescript(Act, NumDesc) :-
   contents(Act, ContentData),
   \+val(type, description, ContentData),
   length(ContentData, NumDesc).

% get the number of descriptions in the dialogue act's content
% (=1, assuming it contains only descriptions)
numdescript(Act, 1) :-
   contents(Act, ContentData),
   val(type, description, ContentData).

% get the number of descriptions in the dialogue act's content
% (empty except for "type: contents")
numdescript(Act, 0) :-
   val(contents, Contents, Act),
   length(Contents, 1).

% get a keyphrase contained in an event
keyphrase(Event, KeyphraseName, Keyphrase) :-
  data(Event, EventData),
  data(EventData, DataList),
  nth1(1, DataList, Score),
  data(Score, ScoreList),
  member(Keyphrase, ScoreList),
  val(type, keyphrase, Keyphrase),
  val(name, KeyphraseName, Keyphrase).

% predicate for retrieving a SPIN-parsed speech event
speech(Record, Start, End, Utterance, Confidence, Act) :-
  fsr(Record),
  type(Record, event),
  sent(Record, audio),
  mode(Record, speech),
  dist(Record, Dist),
  life(Record, Life),
  time(Record, Time),
  conf(Record, Prob),
  Start is Time - Dist,
  End is Time - Dist + Life,
  data(Record, EventData),
  corr(EventData, Utterance),
  data(EventData, DataList),
  length(DataList, 1),
  nth1(1, DataList, Score),
  conf(Score, ScoreConf),
  Confidence is ScoreConf * Prob ,
  actfromscore(Score, Act).
  %% TODO avoid redundancy, but keep the flexibility with regards to the number of content entries
  %& (the usual "one and only vs. list" problem) -> maybe split dialogueact/2 into two helper predicates?

  
% predicate for retrieving a Microsoft Speech Platform event
% (taking the first steps towards "streamlining" the TFS...)
speech(Record, Start, End, Utterance, Confidence, Act) :-
  fsr(Record),
  type(Record, event),
  sent(Record, msspeech),
  mode(Record, speech),
  dist(Record, Dist),
  life(Record, Life),
  time(Record, Time),
  conf(Record, Prob),
  Start is Time - Dist,
  End is Time - Dist + Life,
  data(Record, EventData),
  uttr(EventData, Utterance),
  data(EventData, InterprData),
  conf(InterprData, InterprConf),
  Confidence is InterprConf * Prob ,
  actfromscore(InterprData, Act).


/*----------------------------------------------------------------------------*
 * Entity Helper Predicates
 *----------------------------------------------------------------------------*/

%% Get the Attribute List
entityfs(Entity, AttrList) :-
   data(Entity, Data),
   data(Data, AttrList).

%% get the nth entity's attribute list from the dialogue act
%% (more than one attribute)
entityactfs(N, DialogueAct, AttrList) :-
   contents(DialogueAct, Contents),
   descript(N, Contents, Desc),
   data(Desc, AttrList),
   \+type(AttrList, attribute).

%% get the nth entity's attribute list from the dialogue act
%% (only one attribute contained)
entityactfs(N, DialogueAct, AttrList) :-
   contents(DialogueAct, Contents),
   descript(N, Contents, Desc),
   data(Desc, OnlyAttr),
   type(OnlyAttr, attribute),
   set(AttrList, [OnlyAttr]).

%% query/enforce an entity's attribute
entityattr(AttrName, AttrValue, Entity) :-
   data(Entity, AttrList),
   member(Attribute, AttrList),
   val(name, AttrName, Attribute),
   val(data, AttrValue, Attribute).

%% retrieve an entity by a particular attribute, e.g. its ID
findbyattr(AttrName, AttrValue, Entity) :-
   fsr(Entity),
   val(type, entity, Entity),
   entityattr(AttrName, AttrValue, Entity).

   
%% compare an incomplete attribute list to that of an entity record
matchfs(PartialList, FullEntity) :-
   data(FullEntity, FullList),
   intersection(PartialList, FullList, PartialList).
   
%% compare an incomplete attribute list to that of the entity with the given id 
matchid(PartialList, EntityId) :-
   findbyattr(id, EntityId, EntityRecord),
   matchfs(PartialList, EntityRecord).

%% get the attributes found in both the incomplete list and the stored record
sharedattr(PartialList, EntityId, SharedList) :-
   findbyattr(id, EntityId, FullEntity),
   data(FullEntity, FullList),
   intersection(PartialList, FullList, SharedList).

subtractHelper(FullList, SubList, Rest) :-
   length(FullList, LF),
   length(SubList, LS),
   compare(>, LF, LS),
   subtract(FullList, SubList, Rest).

subtractHelper(FullList, SubList, Rest) :-
   length(FullList, LF),
   length(SubList, LS),
   (compare(<, LF, LS); compare(=, LF, LS)),
   set(Rest, []).


%% get the attributes which are not shared between the incomplete list and the stored record
nonsharedattr(PartialList, EntityId, DiffList_P, DiffList_R) :-
   findbyattr(id, EntityId, FullEntity),
   data(FullEntity, FullList),
   intersection(PartialList, FullList, SharedList),
   subtractHelper(PartialList, SharedList, DiffList_P),
   subtractHelper(FullList, SharedList, DiffList_R).

%%remove the given attribute from the list
removeattr(AttrName, AttrValue, OldList, NewList) :-
    select([type: attribute, name: AttrName, data: AttrValue], OldList, NewList).
    
%% replaces an attribute within a list  
replaceattr(AttrName, OldValue, NewValue, OldList, NewList) :-
        select([type: attribute, name: AttrName, data: OldValue],
           OldList, TmpList),
        append([[type: attribute, name: AttrName, data: NewValue]],
                   TmpList, NewList).

%% updates one attribute of an entity record, defined by its id
updateattr(Id, AttrName, OldValue, NewValue) :-
        findbyattr(id, Id, Entity),
        data(Entity, OldData),
        replaceattr(AttrName, OldValue, NewValue, OldData, NewData),
        val(name, EntityName, Entity),
        forall((findbyattr(id, Id, ER)), del(ER)),
        add([type: entity, name: EntityName, data: NewData]).

%% updates both coordinates of an entity record, defined by its id
updatepos(Id, OldX, OldY, NewX, NewY) :-
        findbyattr(id, Id, Entity),
        data(Entity, OldData),
        replaceattr(xpos, OldX, NewX, OldData, TmpData),
        replaceattr(ypos, OldY, NewY, TmpData, NewData),
        val(name, EntityName, Entity),
        forall((findbyattr(id, Id, ER)), del(ER)),
        add([type: entity, name: EntityName, data: NewData]).

%% updates the presence and both coordinates of an entity record, defined by its id
updatemarker(Id, OldPresence, OldX, OldY, NewPresence, NewX, NewY) :-
        findbyattr(id, Id, Entity),
        data(Entity, OldData),
        replaceattr(present, OldPresence, NewPresence, OldData, TmpData1),
        replaceattr(xpos, OldX, NewX, TmpData1, TmpData2),
        replaceattr(ypos, OldY, NewY, TmpData2, NewData),
        val(name, EntityName, Entity),
        forall((findbyattr(id, Id, ER)), del(ER)),
        add([type: entity, name: EntityName, data: NewData]).

%% shortcut for retrieving both coordinates
entitypos(Id, XPos, YPos) :-
   findbyattr(id, Id, Entity),
   entityattr(xpos, XPos, Entity),
   entityattr(ypos, YPos, Entity).
                
%% compares a pair of coordinates to the position of a field entity
insidefield(Xpos, Ypos, FieldId) :-
   findbyattr(id, FieldId, Field),
   entityattr(xpos_start, MinX, Field),
   compare(<, MinX, Xpos),
   entityattr(xpos_end, MaxX, Field),
   compare(<, Xpos, MaxX),
   entityattr(ypos_start, MinY, Field),
   compare(<, MinY, Ypos),
   entityattr(ypos_end, MaxY, Field),
   compare(<, Ypos, MaxY).
   
 
%% shortcut for retrieving both coordinates
fieldcenter(Id, XPosCenter, YPosCenter) :-
   findbyattr(id, Id, Entity),
   entityattr(xpos_start, XPosStart, Entity),
   entityattr(xpos_end, XPosEnd, Entity),
   entityattr(ypos_start, YPosStart, Entity),
   entityattr(ypos_end, YPosEnd, Entity),
   XPosCenter is round(XPosStart + (XPosEnd - XPosStart)/2),
   YPosCenter is round(YPosStart + (YPosEnd - YPosStart)/2).
   
/*----------------------------------------------------------------------------*
 * Instruction Helper Predicates
 *----------------------------------------------------------------------------*/

%% get an instruction by its index
instructfs(N, Instruction) :-
   fsr(InstRecord),
   val(type, instruction, InstRecord),
   val(data, Instruction, InstRecord),
   val(index, N, Instruction).

%% get the instruction information by its index
instruct(N, MarkerId, FieldId, MarkerAmbDesc, MarkerUnambDesc, FieldDesc) :-
   instructfs(N, Instruction),
   val(ambMarker, MarkerAmbDesc, Instruction),
   val(markerId, MarkerId, Instruction),
   findbyattr(id, MarkerId, Marker),
   entityattr(unambiguous, MarkerUnambDesc, Marker),
   val(fieldId, FieldId, Instruction),
   findbyattr(id, FieldId, Field),
   entityattr(unambiguous, FieldDesc, Field).

/*----------------------------------------------------------------------------*
 * Extract A Control Message
 *----------------------------------------------------------------------------*/
cntrl(Sender, Signal) :-
  fsr(Record), 
  type(Record, event),
  mode(Record, control),
  sent(Record, Sender),
  data(Record, Signal).

/*----------------------------------------------------------------------------*
 * Count All Field Objects
 *----------------------------------------------------------------------------*/
fieldnum(N) :-
 findall(R,
 ( fsr(R),
   type(R, entity),
   data(R, D),
   member(V, D),
   type(V, attribute),
   name(V, type),
   data(V, field)),
  L),
  length(L, N).

/*----------------------------------------------------------------------------*
 * Count All Marker Objects
 *----------------------------------------------------------------------------*/
markernum(N) :-
  findall(R,
  ( fsr(R),
    type(R, entity),
    data(R, D),
    member(V, D),
    type(V, attribute),
    name(V, type),
    data(V, marker)),
  L),
  length(L, N).

/*----------------------------------------------------------------------------*
 * Count All Instructions
 *----------------------------------------------------------------------------*/
instructnum(N) :-
  findall(R,
  (  fsr(R),
     type(R, instruction)),
  L),
  length(L, N).

/*----------------------------------------------------------------------------*
 * Retract All Instructions
 *----------------------------------------------------------------------------*/
rllinstruct :-
  forall(
  (  fsr(R),
     type(R, instruction)),
  del(R)).

/*----------------------------------------------------------------------------*
 * Retract All Marker Objects
 *----------------------------------------------------------------------------*/
rllmarkers :-
  forall(
  ( fsr(R),
    type(R, entity),
    data(R, D),
    member(V, D),
    type(V, attribute),
    name(V, type),
    data(V, marker)),
  del(R)).
  
/*----------------------------------------------------------------------------*
 * Retract All Field Objects
 *----------------------------------------------------------------------------*/
rllfields :-
  forall(
  ( fsr(R),
    type(R, entity),
    data(R, D),
    member(V, D),
    type(V, attribute),
    name(V, type),
    data(V, field)),
  del(R)).
  
/*----------------------------------------------------------------------------*
 * Check if an event has occurred after another
 * event by comparing the time features as well
 * as the distance and lifetime features of the
 * corresponding feature records for the events
 *----------------------------------------------------------------------------*/
after(A, B) :-
  time(A, TA),
  time(B, TB),
  dist(A, DA),
  dist(B, DB),
  life(B, LB),
  SA = TA - DA,
  SB = TB - DB,
  EB = SB + LB,
  SA > EB.

/*----------------------------------------------------------------------------*
 * Get the latest event feature record in a list
 *----------------------------------------------------------------------------*/
latest_of_list(R, [R]) :- write('End Case '), write(R), nl, !.
latest_of_list(R, [H|T]) :-
   latest_of_list(L, T),
    (
      after(L, H), !, R = L, write(R), nl
    ;
      after(H, L), !, R = H, write(R), nl
    ).
    
latest_of_alist(R, [R]) :- write('End Case '), write(R), nl, !.
%latest_of_alist(H, [H|T]) :-
%     latest_of_alist(L, T),
%     after(H, L), write(H), nl, !.
latest_of_alist(R, [H|T]) :-
     latest_of_alist(R, T),
     after(R, H), write(R), write(' after '), write(H),nl, !.
latest_of_alist(H, [H|_]) :-
    % latest_of_alist(R, T),
    % after(R, H),
    write(H), write(' is newer '), nl, !.

/*----------------------------------------------------------------------------*
 * Find the latest gaze event feature record
 * of all gaze event feature records that are
 * currently in the knowledge base.
 *----------------------------------------------------------------------------*/
find_all_gaze(L) :-
  findall(R,
    (fsr(R),
     type(R, event),
     name(R, ssiv2),
     sent(R, artkp),
     mode(R, mfixp)), L).

latest_of_gaze(R) :-
  find_all_gaze(L), out(L),
  latest_of_list(R, L).

latest_of_agaze(R) :-
  find_all_gaze(L), out(L),
  latest_of_alist(R, L).
/*----------------------------------------------------------------------------*
 * Gaze Reasoning Predicates
 *----------------------------------------------------------------------------*/
gazes(N)  :-
  find_all_gaze(L),
  length(L, N).
  
/*----------------------------------------------------------------------------*
 * Gaze Reasoning Predicates
 *----------------------------------------------------------------------------*/
gaze(N)  :-
  latest_of_gaze(R),
  data(R, D), name(D, N).
