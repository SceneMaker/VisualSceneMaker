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
   % Updating Feature Records
   set/3,
   % General Predicates
   type/2,
   mode/2,
   name/2,
   data/2,
   color/2,
   %
   exp/1,
   turn/2,
   state/2,
   voice/2,
   facs/2,
   gaze/2,
   mood/2,
   scene/2,
   shift/2,
   speech/2,
   speech/1,
   gui/2,
   gui/1,
   elicit/0,
   mutual/0,
   directed/1,
   contrib/2,
   % extracting data from input acts
   inputfunction/2,
   inputcontent/2,
   actiontype/2,
   actionparam/2,
   infotype/2,
   info/2
   ]).

:- use_module(facts).


/*----------------------------------------------------------------------------*
 * Record Updating Predicates
 *----------------------------------------------------------------------------*/
set(P, R, V) :-
  fsr(R), set(P, V, R, S), add(S), del(R).
  
/*----------------------------------------------------------------------------*
 * General Helper Predicates
 *----------------------------------------------------------------------------*/
type(R, V) :-
  val(type, V, R).

mode(R, V) :-
  val(mode, V, R).

name(R, V) :-
  val(name, V, R).
  
data(R, V) :-
  val(data, V, R).
  
color(R, V) :-
  val(color, V, R).

/*----------------------------------------------------------------------------*
 * User Event Predicates
 *----------------------------------------------------------------------------*/
turn(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, turn),
  name(R, N),
  data(R, D),
  jel(R), % Delete the consumed event and all other turn events of this participant
  forall((fsr(S), mode(S, turn), name(S, N)), jel(S)).
  
state(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, state),
  name(R, N),
  data(R, D),
  jel(R).

voice(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, voice),
  name(R, N),
  data(R, D),
  jel(R).
  
facs(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, facs),
  name(R, N),
  data(R, D),
  jel(R).

mood(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, mood),
  name(R, N),
  data(R, D),
  jel(R).

gaze(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, gaze),
  name(R, N),
  data(R, D),
  jel(R).

speech(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, speech),
  name(R, N),
  data(R, D),
  jel(R).

speech(D) :-
  fsr(R),
  type(R, event),
  mode(R, speech),
  data(R, D),
  jel(R).
  
gui(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, gui),
  name(R, N),
  data(R, D),
  jel(R).  

gui(D) :-
  fsr(R),
  type(R, event),
  mode(R, gui),
  data(R, D),
  jel(R).  
  
scene(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, scene),
  name(R, N),
  data(R, D),
  jel(R).
  
contrib(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, contrib),
  name(R, N),
  data(R, D),
  jel(R).
  
shift(N, D) :-
  fsr(R),
  type(R, event),
  mode(R, shift),
  name(R, N),
  data(R, D),
  jel(R).

directed(N) :-
  fsr(R),
  type(R, event),
  mode(R, shift),
  name(R, user),
  (
    data(R, robot)
  *->
    fail
  ;
    data(R, N),
    jel(R), true
  ).

mutual :-
  fsr(R),
  type(R, event),
  mode(R, shift),
  name(R, user),
  data(R, robot),
  jel(R).
  
elicit :-
  fsr(R),
  type(R, event),
  mode(R, feedback),
  name(R, user),
  data(R, elicit),
  jel(R).

exp(D) :-
  fsr(R),
  type(R, event),
  mode(R, exp),
  name(R, user),
  data(R, D),
  jel(R).
  

formorethan(Template, Generator, Condition, Percentage) :-
    bagof(Template, Generator, Range),
    bagof(Template,(Generator, Condition), Scope),
    length(Range, R), length(Scope, S), S/R > Percentage.
    
/*----------------------------------------------------------------------------*
 * Record Updating Predicates
 *----------------------------------------------------------------------------*/
test:-
add([type:event, name:e1, mode:gaze, data:[color:red, name:a, conf:0.7]]),
add([type:event, name:e2, mode:gaze, data:[color:red, name:a, conf:0.8]]),
add([type:event, name:e3, mode:gaze, data:[color:green, name:b, conf:0.4]]),
add([type:event, name:e4, mode:gaze, data:[color:green, name:b, conf:0.3]]),
add([type:event, name:e5, mode:gaze, data:[color:red, name:c, conf:0.9]]).


%bagof(E, D^(fsr(E),type(E,event),mode(E,gaze),data(E,D),color(D,red)), Range),
%bagof(E, D^(fsr(E),type(E,event),mode(E,gaze),data(E,D),color(D,red),name(D,N)), Scope),
%length(Range,RL), length(Scope,SL), SL/RL>0.5.


/*----------------------------------------------------------------------------*
 * extracting info from input acts
 *----------------------------------------------------------------------------*/
inputfunction(Function, InputAct):-
    val(function, Function, InputAct).

inputcontent(Content, InputAct):-
    val(content, Content, InputAct).

actiontype(Type, InputAct):-
    inputcontent(Content, InputAct),
    val(actiontype, Type, Content).

actionparam(Param, InputAct):-
    inputcontent(Content, InputAct),
    val(actionparam, Param, Content).
        
infotype(Type, InputAct):-
    inputcontent(Content, InputAct),
    val(infotype, Type, Content).

info(Info, InputAct):-
    inputcontent(Content, InputAct),
    val(info, Info, Content).
