////////////////////////////////////////////////////////////////////////////////
// Start User Code /////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Local Package Definition
package de.dfki.vsm.model.sceneflow;
// Import Java Cup Runtime
import java_cup.runtime.Symbol;
////////////////////////////////////////////////////////////////////////////////
// End User Code ///////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
%%
////////////////////////////////////////////////////////////////////////////////
// Start Directives ////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
%char                                       // Enable Token Symbol Char Counting
%line                                       // Enable Token Symbol Line Counting
%column                                     // Enable Token Symbol Column Counting
%unicode                                    // 16-Bit Unicode Character Encoding
%final                                      // Make Generated Scanner Class Final
%public                                     // Make Generated Scanner Class Public
// Set The Scanner Interface    
%implements java_cup.runtime.Scanner
// Set The Scanner Token Class    
%type java_cup.runtime.Symbol
// Set The Scanner Class Name   
%class ChartLexxer
// Set Scanner Token Function
%function next_token
// The Standard Construtor Stuff                  
%init{  
    // Do Nothing Here
%init}
%eofval{
    // Return NULL At End Of File
    return null;
    // Return End Of File Token EOF At End                                    
    //return create(ScriptFields.EOF);
%eofval}
%eof{
    // Do Nothing
%eof}
%eofclose
////////////////////////////////////////////////////////////////////////////////
// End Directives //////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
////////////////////////////////////////////////////////////////////////////////
// Start Macro Definitions /////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
alpha   = [a-zA-Z\u00e4\u00c4\u00f6\u00d6\u00fc\u00dc\u00df\u0040\u00b5\u00C0\u00C1\u00C2\u00C6\u00C7\u00C8\u00C9\u00CA\u00CB\u00CE\u00CF\u00D4\u00D6\u00E0\u00E2\u00E6\u00E7\u00E8\u00E9\u00EA\u00EB\u00EE\u00EF\u00F4\u00F6\u00FB\u00FC\u00FF]
digit   = [0-9]
special = [<>'!?$%&#/=~_:;,]|\\|\.|\+|\*|\-|\||\[|\]|\(|\)
win_new = (\n)
mac_new = (\r)
uni_new = (\r\n)
newline = ({win_new}|{uni_new}|{mac_new})
white   = ({newline}|([\ \t\f\b]))
escape  = (\\)
//char    = (\'({alpha}|{digit}|{special})\')
//ident   = (({alpha}|_)({alpha}|{digit}|_)*)
string  = (\"({alpha}|{digit}|{special}|{white})*\")
////////////////////////////////////////////////////////////////////////////////
// End Macro Definitions ///////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
%%
////////////////////////////////////////////////////////////////////////////////
// Start Token Definitions /////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
"Play" { return new Symbol(ChartFields.PLAY); }
"PlaySceneGroup" { return new Symbol(ChartFields.PSG); }
"PlayDialogueAct" { return new Symbol(ChartFields.PDA); }
"UnblockSceneGroup" { return new Symbol(ChartFields.USG); }
"UnblockAllSceneGroups" { return new Symbol(ChartFields.UASG); }
"Get" { return new Symbol(ChartFields.GET); }
"Remove" { return new Symbol(ChartFields.REMOVE); }
"AddFirst" { return new Symbol(ChartFields.ADDFIRST); }
"AddLast" { return new Symbol(ChartFields.ADDLAST); }
"Random" { return new Symbol(ChartFields.RANDOM); }
"RemoveFirst" { return new Symbol(ChartFields.REMOVEFIRST); }
"RemoveLast" { return new Symbol(ChartFields.REMOVELAST); }
"First" { return new Symbol(ChartFields.FIRST); }
"Last" { return new Symbol(ChartFields.LAST); }
"Clear" { return new Symbol(ChartFields.CLEAR); }
"Size" { return new Symbol(ChartFields.SIZE); }
"Contains" { return new Symbol(ChartFields.CONTAINS); }
"Default" { return new Symbol(ChartFields.DEFAULT); }
"In" { return new Symbol(ChartFields.IN); }
"in" { return new Symbol(ChartFields.IN); }
"IN" { return new Symbol(ChartFields.IN); }
"iN" { return new Symbol(ChartFields.IN); }
"Empty" { return new Symbol(ChartFields.EMPTY); }
"Timeout" { return new Symbol(ChartFields.TIMEOUT); }
"HistoryClear" { return new Symbol(ChartFields.HISTORYCLEAR); }
"HistoryDeepClear" { return new Symbol(ChartFields.HISTORYDEEPCLEAR); }
"HistoryContainsState" { return new Symbol(ChartFields.HISTORYCONTAINSSTATE); }
"HistoryValueOf" { return new Symbol(ChartFields.HISTORYVALUEOF); }
"HistorySetDepth" { return new Symbol(ChartFields.HISTORYSETDEPTH); }
"HistoryRunTimeOf" { return new Symbol(ChartFields.HISTORYRUNTIMEOF); }
"ValueOf" { return new Symbol(ChartFields.VALUEOF); }
"true" { return new Symbol(ChartFields.BOOLEAN, new java.lang.Boolean(yytext())); }
"false" { return new Symbol(ChartFields.BOOLEAN, new java.lang.Boolean(yytext())); }
"null"  { return new Symbol(ChartFields.NULL); }
"new"   { return new Symbol(ChartFields.NEW); }
"query" { return new Symbol(ChartFields.QUERY); }
"Query" { return new Symbol(ChartFields.QUERY); }
"?-" { return new Symbol(ChartFields.QUERY); }
"!-" { return new Symbol(ChartFields.PLAY); }

[0-9]+\.[0-9]+ { return new Symbol(ChartFields.FLOAT, new java.lang.Float(yytext())); }
[0-9]+ { return new Symbol(ChartFields.INTEGER, new java.lang.Integer(yytext())); }
//\"([a-zA-Z]|[_-])([a-zA-Z0-9]|[ _-])*([a-zA-Z0-9])*\" { return new Symbol(ChartFields.STRING, new java.lang.String(yytext())); }
//\"([a-zA-Z0-9]|[ _-=.!>():'])*\" { return new Symbol(ChartFields.STRING, new java.lang.String(yytext())); }
{string}        { return new Symbol(ChartFields.STRING, new java.lang.String(yytext())); }

[a-zA-Z_][a-zA-Z0-9_]* { return new Symbol(ChartFields.VARIABLE, new java.lang.String(yytext())); }

"@" { return new Symbol(ChartFields.AT); }
"+" { return new Symbol(ChartFields.PLUS); }
"-" { return new Symbol(ChartFields.MINUS); }
"*" { return new Symbol(ChartFields.TIMES); }
"/" { return new Symbol(ChartFields.DIV); }
"&&" { return new Symbol(ChartFields.AND); }
"||" { return new Symbol(ChartFields.OR); }
"==" { return new Symbol(ChartFields.EQUALEQUAL); }
"!=" { return new Symbol(ChartFields.NOTEQUAL); }
"<" { return new Symbol(ChartFields.LESS); }
">" { return new Symbol(ChartFields.GREATER); }
"<=" { return new Symbol(ChartFields.LESSEQUAL); }
">=" { return new Symbol(ChartFields.GREATEREQUAL); }
"!" { return new Symbol(ChartFields.NOT); }
"(" { return new Symbol(ChartFields.LPAREN); }
")" { return new Symbol(ChartFields.RPAREN); }
"[" { return new Symbol(ChartFields.LBRACK); }
"]" { return new Symbol(ChartFields.RBRACK); }
"{" { return new Symbol(ChartFields.LBRACE); }
"}" { return new Symbol(ChartFields.RBRACE); }
"," { return new Symbol(ChartFields.COMMA); }
"." { return new Symbol(ChartFields.DOT); }
"?" { return new Symbol(ChartFields.QUESTION); }
":" { return new Symbol(ChartFields.COLON); }
"=" { return new Symbol(ChartFields.EQUAL); }

[ \t\r\n\f] { /* ignore white space. */ }
. { System.err.println("Illegal character: "+yytext()); }
