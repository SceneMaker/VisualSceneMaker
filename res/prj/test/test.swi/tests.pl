:- module(tests,
  [% Fact Manipulation
   fsr/1,
   del/1,
   add/1,
   % Term Manipulation
   db_loadall/0,
   % Term Manipulation
   val/3,
   out/1,
   err/1,
   jog/1,
   % Helper Predicates
   set/2,
   now/1,
   rll/2,
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
   % Entity Predicates
   entityfs/2,
   % Speech Predicates
   dialogueact/2,
   category/3,
   function/3,
   command/3,
   contents/2,
   descript/3,
   speech/6,
   % Attribute Lists
   data/3,
   % Tests Predicates
   touch_testsuite/0,
   touch_test_one/1,
   speech_testsuite/0,
   speech_testsuite_proposition/0,
   speech_test_one/1,
   speech_test_two/1,
   entity_testsuite/0,
   entity_test_one/1,
   instruction_testsuite/0,
   control_testsuite/0,
   db_testsuite/0,
   gazetestsuite/0,
   gazetestcase1/0,
   gazetestcase2/0

]).

:- use_module(logic).



% the physical markers
%(TODO: include "present" attribute in updatepos predicate)
db_markers :-
  add(
  [
    type:entity,
    name:real,
    data:
    [
      [ type:attribute, name:type,  data:marker ],
      [ type:attribute, name:id,    data:13 ],
      [ type:attribute, name:present,  data:false ],
      [ type:attribute, name:xpos,  data:0 ],
      [ type:attribute, name:ypos,  data:0 ],
      [ type:attribute, name:size,  data:large ],
      [ type:attribute, name:color, data:green ],
      [ type:attribute, name:shape, data:square ],
      [ type:attribute, name:unambiguous,  data:'das grosse gruene Quadrat' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:real,
    data:
    [
      [ type:attribute, name:type,  data:marker ],
      [ type:attribute, name:id,    data:1 ],
      [ type:attribute, name:present,  data:false ],
      [ type:attribute, name:xpos,  data:0 ],
      [ type:attribute, name:ypos,  data:0 ],
      [ type:attribute, name:size,  data:small ],
      [ type:attribute, name:color, data:blue ],
      [ type:attribute, name:shape, data:square ],
      [ type:attribute, name:unambiguous,  data:'das kleine blaue Quadrat' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:real,
    data:
    [
      [ type:attribute, name:type,  data:marker ],
      [ type:attribute, name:id,    data:15 ],
      [ type:attribute, name:present,  data:false ],
      [ type:attribute, name:xpos,  data:0 ],
      [ type:attribute, name:ypos,  data:0 ],
      [ type:attribute, name:size,  data:large ],
      [ type:attribute, name:color, data:green ],
      [ type:attribute, name:shape, data:triangle ],
      [ type:attribute, name:unambiguous,  data:'das grosse gruene Dreieck' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:real,
    data:
    [
      [ type:attribute, name:type,  data:marker ],
      [ type:attribute, name:id,    data:9 ],
      [ type:attribute, name:present,  data:false ],
      [ type:attribute, name:xpos,  data:0 ],
      [ type:attribute, name:ypos,  data:0 ],
      [ type:attribute, name:size,  data:small ],
      [ type:attribute, name:color, data:red ],
      [ type:attribute, name:shape, data:triangle ],
      [ type:attribute, name:unambiguous,  data:'das kleine rote Dreieck' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:real,
    data:
    [
      [ type:attribute, name:type,  data:marker ],
      [ type:attribute, name:id,    data:11 ],
      [ type:attribute, name:present,  data:false ],
      [ type:attribute, name:xpos,  data:0 ],
      [ type:attribute, name:ypos,  data:0 ],
      [ type:attribute, name:size,  data:big ],
      [ type:attribute, name:color, data:blue ],
      [ type:attribute, name:shape, data:star ],
      [ type:attribute, name:unambiguous,  data:'den großen blauen Stern' ]
    ]
  ]).

% the fields
db_fields :-
  add(
  [
    type:entity,
    name:virtual,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:f1 ],
      [ type:attribute, name:number,  data:1 ],
      [ type:attribute, name:xpos,  data:311 ],
      [ type:attribute, name:ypos,  data:252 ],
      [ type:attribute, name:xpos_start,  data:267 ],
      [ type:attribute, name:ypos_start,  data:208 ],
      [ type:attribute, name:xpos_end,  data:354 ],
      [ type:attribute, name:ypos_end,  data:295 ],
      [ type:attribute, name:unambiguous,  data:'Feld eins' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:virtual,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:f2 ],
      [ type:attribute, name:number,  data:2 ],
      [ type:attribute, name:xpos,  data:484 ],
      [ type:attribute, name:ypos,  data:252 ],
      [ type:attribute, name:xpos_start,  data:440 ],
      [ type:attribute, name:ypos_start,  data:208 ],
      [ type:attribute, name:xpos_end,  data:527 ],
      [ type:attribute, name:ypos_end,  data:295 ],
      [ type:attribute, name:unambiguous,  data:'Feld zwei' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:virtual,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:f3 ],
      [ type:attribute, name:number,  data:3 ],
      [ type:attribute, name:xpos,  data:657 ],
      [ type:attribute, name:ypos,  data:252 ],
      [ type:attribute, name:xpos_start,  data:613 ],
      [ type:attribute, name:ypos_start,  data:208 ],
      [ type:attribute, name:xpos_end,  data:700 ],
      [ type:attribute, name:ypos_end,  data:295 ],
      [ type:attribute, name:unambiguous,  data:'Feld drei' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:virtual,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:f4 ],
      [ type:attribute, name:number,  data:4 ],
      [ type:attribute, name:xpos,  data:311 ],
      [ type:attribute, name:ypos,  data:425 ],
      [ type:attribute, name:xpos_start,  data:267 ],
      [ type:attribute, name:ypos_start,  data:381 ],
      [ type:attribute, name:xpos_end,  data:354 ],
      [ type:attribute, name:ypos_end,  data:468 ],
      [ type:attribute, name:unambiguous,  data:'Feld vier' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:virtual,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:f5 ],
      [ type:attribute, name:number,  data:5 ],
      [ type:attribute, name:xpos,  data:484 ],
      [ type:attribute, name:ypos,  data:425 ],
      [ type:attribute, name:xpos_start,  data:440 ],
      [ type:attribute, name:ypos_start,  data:381 ],
      [ type:attribute, name:xpos_end,  data:527 ],
      [ type:attribute, name:ypos_end,  data:468 ],
      [ type:attribute, name:unambiguous,  data:'Feld fuenf' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:virtual,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:f6 ],
      [ type:attribute, name:number,  data:6 ],
      [ type:attribute, name:xpos,  data:657 ],
      [ type:attribute, name:ypos,  data:425 ],
      [ type:attribute, name:xpos_start,  data:613 ],
      [ type:attribute, name:ypos_start,  data:381 ],
      [ type:attribute, name:xpos_end,  data:700 ],
      [ type:attribute, name:ypos_end,  data:468 ],
      [ type:attribute, name:unambiguous,  data:'Feld sechs' ]
    ]
  ]),add(
  [
    type:entity,
    name:virtual,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:f7 ],
      [ type:attribute, name:number,  data:7 ],
      [ type:attribute, name:xpos,  data:311 ],
      [ type:attribute, name:ypos,  data:598 ],
      [ type:attribute, name:xpos_start,  data:267 ],
      [ type:attribute, name:ypos_start,  data:554 ],
      [ type:attribute, name:xpos_end,  data:354 ],
      [ type:attribute, name:ypos_end,  data:641 ],
      [ type:attribute, name:unambiguous,  data:'Feld sieben' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:virtual,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:f8 ],
      [ type:attribute, name:number,  data:8 ],
      [ type:attribute, name:xpos,  data:484 ],
      [ type:attribute, name:ypos,  data:598 ],
      [ type:attribute, name:xpos_start,  data:440 ],
      [ type:attribute, name:ypos_start,  data:554 ],
      [ type:attribute, name:xpos_end,  data:527 ],
      [ type:attribute, name:ypos_end,  data:641 ],
      [ type:attribute, name:unambiguous,  data:'Feld acht' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:virtual,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:f9 ],
      [ type:attribute, name:number,  data:9 ],
      [ type:attribute, name:xpos,  data:657 ],
      [ type:attribute, name:ypos,  data:598 ],
      [ type:attribute, name:xpos_start,  data:613 ],
      [ type:attribute, name:ypos_start,  data:554 ],
      [ type:attribute, name:xpos_end,  data:700 ],
      [ type:attribute, name:ypos_end,  data:641 ],
      [ type:attribute, name:unambiguous,  data:'Feld neun' ]
    ]
  ]).


/*----------------------------------------------------------------------------*
 * Static Database Entries
 *----------------------------------------------------------------------------*/
% the instructions (referencing the entities)
db_instructions :-
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 1,
      markerId: 13,
      fieldId: f1,
      ambMarker: 'das grosse gruene Teil'
    ]
  ]),
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 2,
      markerId: 01,
      fieldId: f2,
      ambMarker: 'das Dreieck'
    ]
  ]),
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 3,
      markerId: 15,
      fieldId: f3,
      ambMarker: 'das kleine Teil'
    ]
  ]),
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 4,
      markerId: 09,
      fieldId: f4,
      ambMarker: 'das Dreieck'
    ]
  ]),
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 5,
      markerId: 11,
      fieldId: f5,
      ambMarker: 'das blaue Objekt'
    ]
  ]).

% resetting and entering all the data
db_loadall :-
  rll(type, entity),
  rll(type, events),
  rll(type, instruction),
  db_fields,
  db_markers,
  db_instructions.



/*----------------------------------------------------------------------------*
 * Touch Test Suite
 *----------------------------------------------------------------------------*/
touch_testsuite :-
  rll(mode, touch),
  add(
  [
    type:event,
    name:undef,
    sent:table,
    mode:touch,
    dist:0,
    life:0,
    time:15912,
    conf:1.0,
    data:
    [
      type:touch,
      name:release, % Can be 'pressed' or 'release'
      data:         % A list with various attributes
      [
        [
          type:attribute,
          name:id,
          data:3
        ],
        [
          type:attribute,
          name:xpos,
          data:320
        ],
        [
          type:attribute,
          name:ypos,
          data:430
        ],
        [
          type:attribute,
          name:type,
          data:marker
        ]
      ]
    ]
  ]).
  % add(
  % [
    % type:event,
    % name:undef,
    % sent:table,
    % mode:touch,
    % dist:0,
    % life:0,
    % time:35912,
    % conf:1.0,
    % data:
    % [
      % type:touch,
      % name:release, % Can Be 'pressed' or 'release'
      % data:         % A list with various attributes
      % [
        % [
          % type:attribute,
          % name:id,
          % data:1
        % ],
        % [
          % type:attribute,
          % name:xpos,
          % data:780
        % ],
        % [
          % type:attribute,
          % name:ypos,
          % data:650
        % ],
        % [
          % type:attribute,
          % name:type,
          % data:marker
        % ]
      % ]
    % ]
  % ]),
  % add(
  % [
    % type:event,
    % name:undef,
    % sent:table,
    % mode:touch,
    % dist:0,
    % life:0,
    % time:35912,
    % conf:1.0,
    % data:
    % [
      % type:touch,
      % name:release, % Can Be 'pressed' or 'release'
      % data:         % A list with various attributes
      % [
        % [
          % type:attribute,
          % name:id,
          % data:1
        % ],
        % [
          % type:attribute,
          % name:xpos,
          % data:580
        % ],
        % [
          % type:attribute,
          % name:ypos,
          % data:450
        % ],
        % [
          % type:attribute,
          % name:type,
          % data:marker
        % ]
      % ]
    % ]
  % ]),
  % add(
  % [
    % type:event,
    % name:undef,
    % sent:table,
    % mode:touch,
    % dist:0,
    % life:0,
    % time:35912,
    % conf:1.0,
    % data:
    % [
      % type:touch,
      % name:pressed, % Can Be 'pressed' or 'release'
      % data:         % A list with various attributes
      % [
        % [
          % type:attribute,
          % name:id,
          % data:14
        % ],
        % [
          % type:attribute,
          % name:xpos,
          % data:550
        % ],
        % [
          % type:attribute,
          % name:ypos,
          % data:650
        % ],
        % [
          % type:attribute,
          % name:type,
          % data:field
        % ]
      % ]
    % ]
  % ]).

touch_test_one(Event) :-
  % Test Some Predicates
  fsr(Event), type(Event, event), mode(Event, touch),
  name(Event, Name), write('name:'), out(Name), nl,
  sent(Event, Sent), write('sent:'), out(Sent), nl,
  dist(Event, Dist), write('dist:'), out(Dist), nl,
  life(Event, Life), write('life:'), out(Life), nl,
  time(Event, Time), write('time:'), out(Time), nl,
  conf(Event, Conf), write('conf:'), out(Conf), nl,
  data(Event, Data), write('data:'), out(Data), nl.
  
  
touch_test_two(Event, Name, Type, Mark, Time, XPos, YPos) :-
  touch(Event, Name, Type, Mark, Time, XPos, YPos).
  
  
/*----------------------------------------------------------------------------*
 * Speech Test Suite
 *----------------------------------------------------------------------------*/
speech_testsuite :-
  rll(mode, speech),
  add(
  [
    type:event,
    name:undef,
    sent:audio,
    mode:speech,
    dist:6449,
    life:3200,
    time:10058,
    conf:0.72,
    data:
    [
      type:spin,
      uttr:'meinst du das kleine rote oder das große gelbe feld',
      corr:'meinst du das kleine rote oder das grosse gelbe feld',
      data:
      [
        [
          type:data,
          conf:0.53,
          time:9.63,
          data:
          [
            [
              type:dialogueact,
              category:
              [
                type:category,
                name:seeking
              ],
              function:
              [
                type:function,
                name:question
              ],
              commands:
              [
                type:commands,
                name:selection
              ],
              contents:
              [
                type:contents,
                data:
                [
                  [
                    type:description,
                    data:
                    [
                      [
                        type:attribute,
                        name:size,
                        data:small
                      ],
                      [
                        type:attribute,
                        name:color,
                        data:red
                      ]
                    ]
                  ],
                  [
                    type:description,
                    data:
                    [
                      type:attribute,
                      name:size,
                      data:large
                    ]
                  ]
                ]
              ]
            ],
            [
              type:keyphrase,
              name:perspronoun,
              data:none
            ],
            [
              type:keyphrase,
              name:determiner,
              data:none
            ],
            [
              type:keyphrase,
              name:disjunction,
              data:none
            ],
            [
              type:keyphrase,
              name:determiner,
              data:none
            ],
            gelbe,
            [
              type:attribute,
              name:object,
              data:field
            ]
          ]
        ]
      ]
    ]
  ]).
  
speech_testsuite_proposition :-
  add(
  [
    type:event,
    name:undef,
    sent:audio,
    mode:speech,
    dist:5512,
    life:2350,
    time:13188,
    conf:0.72,
    data:
    [
      type:spin,
      uttr:'das rote dreieck',
      corr:'das rote dreieck',
      data:
      [
        [
          type:data,
          conf:0.67,
          time:13.58,
          data:
          [
            [
              type:keyphrase,
              name:determiner,
              data:none
            ],
            [
              type:dialogueact,
              category:
              [
                type:category,
                name: [ providing, seeking ]
              ],
              function:
              [
                type:function,
                name: [ answer, proposition ]
              ],
              contents:
              [
                type:contents,
                data:
                [
                  type:description,
                  data:
                  [
                    [
                      type:attribute,
                      name:color,
                      data:red
                    ],
                    [
                      type:attribute,
                      name:shape,
                      data:triangle
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]).

speech_testsuite_setquestion :-
  add([
    type:event,
    name:undef,
    sent:audio,
    mode:speech,
    dist:4082,
    life:900,
    time:599701,
    conf:0.64,
    data:
    [
      type:spin,
      uttr:'welches',
      corr:'welches',
      data:
      [
        [
          type:data,
          conf:0.67,
          time:1.02,
          data:
          [
            type:dialogueact,
            category:
            [
              type:category,
              name:seeking
            ],
            function:
            [
              type:function,
              name:question
            ],
            commands:
            [
              type:commands,
              name:refinement
            ],
            contents:
            [
              type:contents
            ]
          ]
        ]
      ]
    ]
  ]).

speech_testsuite_choice :-
  add([
      type:event,
      name:undef,
      sent:audio,
      mode:speech,
      dist:6670,
      life:3150,
      time:201710,
      conf:0.67,
      data:[
          type:spin,
          uttr:'meinst du große grüne oder das kleine blaue',
          corr:'meinst du grosse gruene oder das kleine blaue',
          data:[
              [
                  type:data,
                  conf:0.63,
                  time:5.93,
                  data:[
                      [
                          type:dialogueact,
                          category:[
                              type:category,
                              name:seeking
                          ],
                          function:[
                              type:function,
                              name:question
                          ],
                          commands:[
                              type:commands,
                              name:selection
                          ],
                          contents:[
                              type:contents,
                              data:[
                                  [
                                      type:description,
                                      data:[
                                          [
                                              type:attribute,
                                              name:size,
                                              data:large
                                          ],
                                          [
                                              type:attribute,
                                              name:color,
                                              data:green
                                          ]
                                      ]
                                  ],
                                  [
                                      type:description,
                                      data:[
                                          [
                                              type:attribute,
                                              name:size,
                                              data:small
                                          ],
                                          [
                                              type:attribute,
                                              name:color,
                                              data:blue
                                          ]
                                      ]
                                  ]
                              ]
                          ]
                      ],
                      [
                          type:keyphrase,
                          name:perspronoun,
                          data:none
                      ],
                      [
                          type:keyphrase,
                          name:disjunction,
                          data:none
                      ],
                      [
                          type:keyphrase,
                          name:determiner,
                          data:none
                      ]
                  ]
              ]
          ]
      ]
  ]).
  
speech_testsuite_ref :-
  add([
    type:event,
    name:undef,
    sent:audio,
    mode:speech,
    dist:6042,
    life:2250,
    time:526961,
    conf:0.65,
    data:
    [
      type:spin,
      uttr:'meinst du das quadrat dort dr³ben',
      corr:'meinst du das quadrat dort drueben',
      data:
      [
        [
          type:data,
          conf:0.56,
          time:5.49,
          data:
          [
            [
              type:dialogueact,
              category:
              [
                type:category,
                name:seeking
              ],
              function:
              [
                type:function,
                name:question
              ],
              commands:
              [
                type:commands,
                name:selection
              ],
              contents:
              [
                type:contents,
                data:
                [
                  type:description,
                  data:
                  [
                    [
                      type:attribute,
                      name:shape,
                      data:square
                    ],
                    [
                      type:attribute,
                      name:reference,
                      data:location
                    ]
                  ]
                ]
              ]
            ],
            [
              type:keyphrase,
              name:perspronoun,
              data:none
            ],
            [
              type:keyphrase,
              name:determiner,
              data:none
            ],
            drueben
          ]
        ]
      ]
    ]
  ]),
  add([
      type:event,
      name:undef,
      sent:audio,
      mode:speech,
      dist:2893,
      life:750,
      time:386455,
      conf:0.41,
      data:[
          type:spin,
          uttr:'das da',
          corr:'das da',
          data:[
              [
                  type:data,
                  conf:0.67,
                  time:2.66,
                  data:[
                      [
                          type:keyphrase,
                          name:determiner,
                          data:none
                      ],
                      [
                          type:dialogueact,
                          category:[
                              type:category,
                              name:[
                                  providing,
                                  seeking
                              ]
                          ],
                          function:[
                              type:function,
                              name:[
                                  answer,
                                  proposition
                              ]
                          ],
                          contents:[
                              type:contents,
                              data:[
                                  type:description,
                                  data:[
                                      type:attribute,
                                      name:reference,
                                      data:location
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      ]
  ]).

speech_test_synonym :-
add(
  [
    type:event,
    name:undef,
    sent:audio,
    mode:speech,
    dist:5512,
    life:2350,
    time:13188,
    conf:0.72,
    data:
    [
      type:spin,
      uttr:'das dunkle dreieck',
      corr:'das dunkle dreieck',
      data:
      [
        [
          type:data,
          conf:0.67,
          time:13.58,
          data:
          [
            [
              type:keyphrase,
              name:determiner,
              data:none
            ],
            [
              type:dialogueact,
              category:
              [
                type:category,
                name: [ providing, seeking ]
              ],
              function:
              [
                type:function,
                name: [ answer, proposition ]
              ],
              contents:
              [
                type:contents,
                data:
                [
                  type:description,
                  data:
                  [
                    [
                      type:attribute,
                      name:color,
                      data:dark
                    ],
                    [
                      type:attribute,
                      name:shape,
                      data:triangle
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]).

speech_test_one(Event) :-
  % Test Some Predicates
  fsr(Event), type(Event, event), mode(Event, speech),
  name(Event, Name), write('name:'), out(Name), nl,
  sent(Event, Sent), write('sent:'), out(Sent), nl,
  dist(Event, Dist), write('dist:'), out(Dist), nl,
  life(Event, Life), write('life:'), out(Life), nl,
  time(Event, Time), write('time:'), out(Time), nl,
  conf(Event, Conf), write('conf:'), out(Conf), nl,
  data(Event, Data), write('data:'), out(Data), nl.
  
speech_test_two(Event) :-
  % Test Some Predicates
  fsr(Event), type(Event, event), mode(Event, speech),
  dialogueact(Event, DialogueAct), out(DialogueAct), nl,
  category(1, DialogueAct, Category), out(Category), nl,
  function(1, DialogueAct, Function), out(Function), nl,
  command(1, DialogueAct, Commands), out(Commands), nl,
  contents(DialogueAct, Contents), out(Contents), nl,
  descript(1, Contents, Descript), out(Descript), nl.

speech_test_three(R, S, E, U, C, A) :-
  speech(R, S, E, U, C, A).
/*----------------------------------------------------------------------------*
 * Entity Test Suite
 *----------------------------------------------------------------------------*/
entity_testsuite :-
  rll(type, entity),
  add(
  [
    type:entity,
    name:real,
    data:
    [
      [ type:attribute, name:type,  data:marker ],
      [ type:attribute, name:id,    data:170 ],
      [ type:attribute, name:xpos,  data:10 ],
      [ type:attribute, name:ypos,  data:40 ],
      [ type:attribute, name:size,  data:large ],
      [ type:attribute, name:color, data:green ],
      [ type:attribute, name:shape, data:square ],
      [ type:attribute, name:unambiguous,  data:'das grosse gruene Quadrat' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:undef,
    data:
    [
      [ type:attribute, name:type,  data:marker ],
      [ type:attribute, name:id,    data:171 ],
      [ type:attribute, name:xpos,  data:10 ],
      [ type:attribute, name:ypos,  data:20 ],
      [ type:attribute, name:size,  data:small ],
      [ type:attribute, name:color, data:blue ],
      [ type:attribute, name:shape, data:square ],
      [ type:attribute, name:unambiguous,  data:'das kleine blaue Quadrat' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:undef,
    data:
    [
      [ type:attribute, name:type,  data:marker ],
      [ type:attribute, name:id,    data:172 ],
      [ type:attribute, name:xpos,  data:20 ],
      [ type:attribute, name:ypos,  data:30 ],
      [ type:attribute, name:size,  data:large ],
      [ type:attribute, name:color, data:green ],
      [ type:attribute, name:shape, data:triangle ],
      [ type:attribute, name:unambiguous,  data:'das grosse gruene Dreieck' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:undef,
    data:
    [
      [ type:attribute, name:type,  data:marker ],
      [ type:attribute, name:id,    data:173 ],
      [ type:attribute, name:xpos,  data:20 ],
      [ type:attribute, name:ypos,  data:10 ],
      [ type:attribute, name:size,  data:small ],
      [ type:attribute, name:color, data:red ],
      [ type:attribute, name:shape, data:triangle ],
      [ type:attribute, name:unambiguous,  data:'das kleine rote Dreieck' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:undef,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:1 ],
      [ type:attribute, name:number,  data:1 ],
      [ type:attribute, name:xpos_start,  data:193 ], 
      [ type:attribute, name:ypos_start,  data:43 ],
      [ type:attribute, name:xpos_end,  data:280 ],
      [ type:attribute, name:ypos_end,  data:140 ],
      [ type:attribute, name:unambiguous,  data:'Feld 1' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:undef,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:2 ],
      [ type:attribute, name:number,  data:2 ],
      [ type:attribute, name:xpos_start,  data:366 ],
      [ type:attribute, name:ypos_start,  data:43 ],
      [ type:attribute, name:xpos_end,  data:463 ],
      [ type:attribute, name:ypos_end,  data:140 ],
      [ type:attribute, name:unambiguous,  data:'Feld 2' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:undef,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:3 ],
      [ type:attribute, name:number,  data:3 ],
      [ type:attribute, name:xpos_start,  data:539 ],
      [ type:attribute, name:ypos_start,  data:43 ],
      [ type:attribute, name:xpos_end,  data:636 ],
      [ type:attribute, name:ypos_end,  data:140 ],
      [ type:attribute, name:unambiguous,  data:'Feld 3' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:undef,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:4 ],
      [ type:attribute, name:number,  data:4 ],
      [ type:attribute, name:xpos_start,  data:193 ],
      [ type:attribute, name:ypos_start,  data:216 ],
      [ type:attribute, name:xpos_end,  data:280 ],
      [ type:attribute, name:ypos_end,  data:313 ],
      [ type:attribute, name:unambiguous,  data:'Feld 4' ]
    ]
  ]),
  add(
  [
    type:entity,
    name:undef,
    data:
    [
      [ type:attribute, name:type,  data:field ],
      [ type:attribute, name:id,    data:5 ],
      [ type:attribute, name:number,  data:5 ],
      [ type:attribute, name:xpos_start,  data:366 ],
      [ type:attribute, name:ypos_start,  data:216 ],
      [ type:attribute, name:xpos_end,  data:463 ],
      [ type:attribute, name:ypos_end,  data:313 ],
      [ type:attribute, name:unambiguous,  data:'Feld 5' ]
    ]
  ]).
%   add(
%   [
%     type:entity,
%     name:undef,
%     data:       % A list with various attributes
%     [
%       [ type:attribute, name:id, data:15 ],
%       [ type:attribute, name:xpos, data:356 ],
%       [ type:attribute, name:ypos, data:455 ],
%       [ type:attribute, name:type, data:field ] % Can be 'field' or 'marker'
%     ]
%   ]).
  
entity_test_one(Entity) :-
  fsr(Entity), type(Entity, entity),
  name(Entity, Name), write('name:'), out(Name), nl,
  data(Entity, Data), write('data:'), out(Data), nl.

/*----------------------------------------------------------------------------*
 * Instruction Test Suite
 *----------------------------------------------------------------------------*/
%Feld 1: großes grünes Quadrat (id 170)
%Feld 2: großes grünes Dreieck (id 172)
%Feld 3: kleines blaues Quadrat (id 171)
%Feld 4: kleines rotes Dreieck (id 173)
%Feld 5: großer blauer Stern (id 174)
instruction_testsuite :-
  rll(type, instruction),
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 1,
      markerId: 170,
      fieldId: 1,
      ambMarker: 'das große grüne Teil'
    ]
  ]),
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 2,
      markerId: 172,
      fieldId: 2,
      ambMarker: 'das Dreieck'
    ]
  ]),
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 3,
      markerId: 171,
      fieldId: 3,
      ambMarker: 'das kleine Teil'
    ]
  ]),
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 4,
      markerId: 173,
      fieldId: 4,
      ambMarker: 'das Dreieck'
    ]
  ]),
  add(
  [
    type: instruction,
    name: undef,
    data:
    [
      index: 5,
      markerId: 175,
      fieldId: 5,
      ambMarker: 'das blaue Objekt'
    ]
  ]).

/*----------------------------------------------------------------------------*
 * Control Event Test Suite
 *----------------------------------------------------------------------------*/
control_testsuite :-
  add([
      type: event,
      name: undef,
      sent: table,
      mode: control,
      data: 'table ready'
    ]).
    
/*----------------------------------------------------------------------------*
 * Database Test Suite
 *----------------------------------------------------------------------------*/

db_testsuite :-
  db_loadall,
  updatemarker(1, _,  _,  _, true, 42, 42),
  updatemarker(9,  _,  _,  _, true, 42, 42),
  updatemarker(11,  _,  _,  _, true, 42, 42),
  updatemarker(13,  _,  _,  _, true, 42, 42),
  updatemarker(15,  _,  _,  _, true, 42, 42).


debug_msspeech :-
 add(
[
    type:event,
    name:undef,
    sent:audio,
    mode:speech,
    dist:2.178,
    life:1.11,
    time:47341,
    conf:1,
    data:[
        type:msspeech,
        uttr:'meinst du das Dreieck',
        corr:'meinst du das Dreieck',
        data:[
            [
                type:data,
                conf:0.6447226,
                data:[
                    [
                        type:dialogueAct,
                        category:[
                            type:category,
                            name:seeking
                        ],
                        function:[
                            type:function,
                            name:proposition
                        ],
                        contents:[
                            type:contents,
                            data:[
                                [
                                    type:description,
                                    data:[
                                        [
                                            type:attribute,
                                            name:shape,
                                            data:triangle
                                        ]
                                    ]
                                ]
                            ]
                        ],
                        commands:[
                            type:commands,
                            data:[
                                state_intention
                            ]
                        ]
                    ],
                    [
                        type:keyphrase,
                        name:perspronoun,
                        data:you
                    ]
                ]
            ]
        ]
    ]
]).



gazetestsuite :-
rll(sent, artkp),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:12000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:13000,
    conf:1.0,
    data:[
        type:mfixd,
        name:2
    ]
]),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:14000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:15000,
    conf:1.0,
    data:[
        type:mfixd,
        name:2
    ]
]),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:16000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:17000,
    conf:1.0,
    data:[
        type:mfixd,
        name:2
    ]
]),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:18000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:19000,
    conf:1.0,
    data:[
        type:mfixd,
        name:2
    ]
]),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:20000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:21000,
    conf:1.0,
    data:[
        type:mfixd,
        name:2
    ]
]),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:22000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:43000,
    conf:1.0,
    data:[
        type:mfixd,
        name:2
    ]
]),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:24000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:25000,
    conf:1.0,
    data:[
        type:mfixd,
        name:2
    ]
]),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:26000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:27000,
    conf:1.0,
    data:[
        type:mfixd,
        name:2
    ]
]),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:28000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:29000,
    conf:1.0,
    data:[
        type:mfixd,
        name:2
    ]
]),
add(
[
  type:event,
  name:ssiv2,
  sent:artkp,
  mode:mfixp,
  dist:133,
  life:0,
  time:30000,
  conf:1.00,
  data:
  [
    type:mfixd,
    name:1
  ]
]),
add(
[
    type:event,
    name:ssiv2,
    sent:artkp,
    mode:mfixp,
    dist:117,
    life:0,
    time:31000,
    conf:1.0,
    data:[
        type:mfixd,
        name:6
    ]
]).

gazetestcase1 :-
  find_all_gaze(R), length(R, N), out(N).

gazetestcase2 :-
  latest_of_agaze(R), out(R).

