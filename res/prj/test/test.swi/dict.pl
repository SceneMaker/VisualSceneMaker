:- module(dict,
  [
   % general
   add_word/6,
   match_word/4,
   match_word_full/6,
   % string generation
   assemble_string/2,
   % entity-specific
   generate_ref_entity/3,
   generate_ref_desc/3,
   % add dictionary entries
   load_nouns/0,
   load_attributes/0,
   load_numbers/0,
   load_keywords/0,
   load_dictionary/0
  ]
  ).

:- use_module(logic).


%-------------------------------------------------------------------------------
% general predicates
%-------------------------------------------------------------------------------

add_word(Word, Category, WordType, Genus, Case, OutputText):-
   add(
     [
        type: word,
        name: Word,   %the semantic identifier of this word
        data:
        [
            category: Category,
            type:     WordType,
            genus:    Genus,
            case:     Case,
            output:   OutputText
        ]
     ]
   ).


match_word(Word, Genus, Case, OutputText):-
   % find the word
   fsr(W),
   val(type, word, W),
   val(name, Word, W),
   val(data, Data, W),
   % look at the entry
   val(genus, Genus, Data),
   val(case, Case, Data),
   val(output, OutputText, Data),
   !.

% no matching word found -> return an empty string
match_word(_, _, _, OutputText):-
   set(OutputText, ' ').


match_word_full(Word, Category, WordType, Genus, Case, OutputText):-
   % find the word
   fsr(W),
   val(type, word, W),
   val(name, Word, W),
   val(data, Data, W),
   % look at the entry
   val(category, Category, Data),
   val(type, WordType, Data),
   val(genus, Genus, Data),
   val(case, Case, Data),
   val(output, OutputText, Data),
   !.

% no matching word found -> return an empty string
match_word_full(_, _, _, _, _, OutputText):-
   set(OutputText, ' ').

/*----------------------------------------------------------------------------
 * complex translation predicates
 * (takes care of the proper word order)
 *----------------------------------------------------------------------------*/

% place the number at the end --------------------------------------------------
translate_desc(Genus, Case, AttrList, OutString):-
   select(NumberAttr, AttrList, RestList),
   val(name, number, NumberAttr),
   val(data, NumberVal, NumberAttr),
   match_word(NumberVal, _, Case, NumberStr),
   translate_desc(Genus, Case, RestList, RestStr),
   assemble_string([RestStr, NumberStr], OutString),
   !.


% is there a noun which determines the genus? ----------------------------------
% yes -> use that genus, add the rest
translate_desc(Genus, Case, AttrList, OutString):-
   select(Attr, AttrList, RestList),
   val(data, AttrVal, Attr),
   match_word_full(AttrVal, _, noun, Genus, Case, AttrStr),
   translate_desc_helper(Genus, Case, RestList, RestStr),
   assemble_string([RestStr, AttrStr], OutString),
   !.

% no -> use neutral genus
% TODO: proper negation? (ie. none of the attributes is a noun)
translate_desc(neutral, Case, AttrList, OutString):-
   translate_desc_helper(neutral, Case, AttrList, OutString).


% place the size at the beginning ----------------------------------------------
translate_desc_helper(Genus, Case, AttrList, OutString):-
   select(SizeAttr, AttrList, RestList),
   val(name, size, SizeAttr),
   val(data, SizeVal, SizeAttr),
   match_word(SizeVal, Genus, Case, SizeStr),
   translate_desc_helper(Genus, Case, RestList, RestStr),
   assemble_string([SizeStr, RestStr], OutString),
   !.


% translate the rest of the list recursively -----------------------------------

translate_desc_helper(Genus, Case, [Attr|Tail], OutString):-
   val(data, AttrVal, Attr),
   match_word(AttrVal, Genus, Case, AttrStr),
   translate_desc_helper(Genus, Case, Tail, TailStr),
   assemble_string([AttrStr, TailStr], OutString).

translate_desc_helper(Genus, Case, [Attr], OutString):-
   val(data, AttrVal, Attr),
   match_word(AttrVal, Genus, Case, AttrStr),
   OutString = AttrStr.

/*----------------------------------------------------------------------------
 * string generation predicates
 *----------------------------------------------------------------------------*/

assemble_string([Word], OutStr) :-
   set(OutStr, Word).

assemble_string([Head|Tail], OutStr) :-
   string_concat(Head, ' ', TmpStr1),
   assemble_string(Tail, TmpStr2),
   string_concat(TmpStr1, TmpStr2, OutStr).


/*----------------------------------------------------------------------------
 * entity-related predicates
 *----------------------------------------------------------------------------*/

% translates a marker Entity to a complete reference
generate_ref_entity(Case, Entity, OutString):-
   entityattr(type, marker, Entity),
   % filter the visible attributes and translate them
   data(Entity, AllAttr),
   logic:filter(name, [size, color, shape], AllAttr, VisibleAttr),
   translate_desc(Genus, Case, VisibleAttr, DescStr),
   % get the article
   match_word(the, Genus, Case, ArticleStr),
   assemble_string([ArticleStr, DescStr], OutString).

% translates a field Entity to a complete reference
generate_ref_entity(Case, Entity, OutString):-
   entityattr(type, field, Entity),
   % filter the visible attributes and translate them
   data(Entity, AllAttr),
   logic:filter(name, [type, number], AllAttr, VisibleAttr),
   translate_desc(_, Case, VisibleAttr, OutString).


% translates an attribute list to a reference
generate_ref_desc(Case, AttrList, OutString):-
   match_word(the, Genus, Case, ArticleStr),
   translate_desc(Genus, Case, AttrList, DescString),
   assemble_string([ArticleStr, DescString], OutString).

   
/*----------------------------------------------------------------------------
 * add dictionary entries
 *----------------------------------------------------------------------------*/

load_attributes:-
  % color
  add_word(red,      'attribute.color',  adjective, neutral, accusative, 'rote'),
  add_word(red,      'attribute.color',  adjective, male,    accusative, 'roten'),
  add_word(blue,     'attribute.color',  adjective, neutral, accusative, 'blaue'),
  add_word(blue,     'attribute.color',  adjective, male,    accusative, 'blauen'),
  add_word(green,    'attribute.color',  adjective, neutral, accusative, 'gruene'),
  add_word(green,    'attribute.color',  adjective, male,    accusative, 'gruenen'),
  add_word(yellow,   'attribute.color',  adjective, neutral, accusative, 'gelbe'),
  add_word(yellow,   'attribute.color',  adjective, male,    accusative, 'gelben'),
  % size
  add_word(large,    'attribute.size',   adjective, neutral,  accusative, 'grosse'),
  add_word(large,    'attribute.size',   adjective, male,     accusative, 'grossen'),
  add_word(small,    'attribute.size',   adjective, neutral,  accusative, 'kleine'),
  add_word(small,    'attribute.size',   adjective, male,     accusative, 'kleinen'),
  % shape
  add_word(triangle, 'attribute.shape',  noun,      neutral,  accusative, 'Dreieck'),
  add_word(pentagon, 'attribute.shape',  noun,      neutral,  accusative, 'Fuenfeck'),
  add_word(hexagon,  'attribute.shape',  noun,      neutral,  accusative, 'Sechseck'),
  add_word(square,   'attribute.shape',  noun,      neutral,  accusative, 'Quadrat'),
  add_word(star,     'attribute.shape',  noun,      male,     accusative, 'Stern').

load_nouns:-
  add_word(object,   'object',           noun,      neutral,  accusative, 'Teil'),
  add_word(field,    'object',           noun,      neutral,  accusative, 'Feld').

load_numbers:-
  % color
  add_word(1,      'number',           cardinal,  _,        accusative, 'eins'),
  add_word(2,      'number',           cardinal,  _,        accusative, 'zwei'),
  add_word(3,      'number',           cardinal,  _,        accusative, 'drei'),
  add_word(4,      'number',           cardinal,  _,        accusative, 'vier'),
  add_word(5,      'number',           cardinal,  _,        accusative, 'fuenf'),
  add_word(6,      'number',           cardinal,  _,        accusative, 'sechs'),
  add_word(7,      'number',           cardinal,  _,        accusative, 'sieben'),
  add_word(8,      'number',           cardinal,  _,        accusative, 'acht'),
  add_word(9,      'number',           cardinal,  _,        accusative, 'neun').

load_keywords:-
  add_word(the,      'article.def',      article,   neutral,  accusative, 'das'),
  add_word(the,      'article.def',      article,   male,     accusative, 'den'),
  add_word(yes,      'answer.pos',       phrase,    unknown,  unknown,    'ja'),
  add_word(no,       'answer.neg',       phrase,    unknown,  unknown,    'nein'),
  add_word(neither,  'answer.neg',       phrase,    unknown,  unknown,    'weder noch').

% add all the words
load_dictionary:-
  rll(type, word),
  load_nouns,
  load_attributes,
  load_numbers,
  load_keywords.

