////////////////////////////////////////////////////////////////////////////////
// Start User Code /////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Local Package Definition
package de.dfki.vsm.model.sceneflow.glue;
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
// End Directives 
////////////////////////////////////////////////////////////////////////////////
//
////////////////////////////////////////////////////////////////////////////////
// Macro Definitions 
////////////////////////////////////////////////////////////////////////////////
digit       = [0-9]
special     = [<>'!?$%&#/=~_:;,]|\\|\.|\+|\*|\-|\||\[|\]|\(|\)
alpha       = [a-zA-Z\u00e4\u00c4\u00f6\u00d6\u00fc\u00dc\u00df\u0040\u00b5\u00C0\u00C1\u00C2\u00C6\u00C7\u00C8\u00C9\u00CA\u00CB\u00CE\u00CF\u00D4\u00D6\u00E0\u00E2\u00E6\u00E7\u00E8\u00E9\u00EA\u00EB\u00EE\u00EF\u00F4\u00F6\u00FB\u00FC\u00FF]
win_new     = (\n)
mac_new     = (\r)
uni_new     = (\r\n)
newline     = ({win_new}|{uni_new}|{mac_new})
white       = ({newline}|([\ \t\f\b]))
escape      = (\\)
integer     = (({digit})+) //[0-9]+ 
floating    = (({digit})+\.({digit})+) //[0-9]+\.[0-9]+ 
identifier  = (({alpha}|_)({alpha}|{digit}|_)*) //[a-zA-Z_][a-zA-Z0-9_]* 
character   = (\'({alpha}|{digit}|{special})\')
dqstring    = (\"({alpha}|{digit}|{special}|{white})*\")
%%
////////////////////////////////////////////////////////////////////////////////
// Token Definitions 
////////////////////////////////////////////////////////////////////////////////
"PlayDialogAct"         { return new Symbol(ChartFields.PlayDialogAct); }
"PlaySceneGroup"        { return new Symbol(ChartFields.PlaySceneGroup); }
"PlayActionCommand"     { return new Symbol(ChartFields.PlayActionCommand); }
"PlayActionSequential"  { return new Symbol(ChartFields.PlayActionSequential); }
"!-"                    { return new Symbol(ChartFields.PlayActionSequential); }
"PlayActionConcurrent"  { return new Symbol(ChartFields.PlayActionConcurrent); }
//"!="                  { return new Symbol(ChartFields.PlayActionConcurrent); }
"UnblockSceneGroup"     { return new Symbol(ChartFields.UnblockSceneGroup); }
"UnblockSceneScript"    { return new Symbol(ChartFields.UnblockSceneScript); }

"HistoryClearFlat"      { return new Symbol(ChartFields.HistoryClearFlat); }
"HistoryClearDeep"      { return new Symbol(ChartFields.HistoryClearDeep); }
"HistorySetDepth"       { return new Symbol(ChartFields.HistorySetDepth); }

"HistoryContains"       { return new Symbol(ChartFields.HistoryContains); }
"HistoryRunTimeOf"      { return new Symbol(ChartFields.HistoryRunTimeOf); }
"HistoryValueOf"        { return new Symbol(ChartFields.HistoryValueOf); }

"query"                 { return new Symbol(ChartFields.Query); }
"Query"                 { return new Symbol(ChartFields.Query); }
"?-"                    { return new Symbol(ChartFields.Query); }

"Timeout"               { return new Symbol(ChartFields.Timeout); }

"true"                  { return new Symbol(ChartFields.Boolean, new java.lang.Boolean(yytext())); }
"false"                 { return new Symbol(ChartFields.Boolean, new java.lang.Boolean(yytext())); }
"null"                  { return new Symbol(ChartFields.JavaNull); }
"new"                   { return new Symbol(ChartFields.JavaNew); }

"int"                   { return new Symbol(ChartFields.TINT, new java.lang.String(yytext())); }
"short"                 { return new Symbol(ChartFields.TSHORT, new java.lang.String(yytext())); }
"long"                  { return new Symbol(ChartFields.TLONG, new java.lang.String(yytext())); }
"float"                 { return new Symbol(ChartFields.TFLOAT, new java.lang.String(yytext())); }
"double"                { return new Symbol(ChartFields.TDOUBLE, new java.lang.String(yytext())); }
"bool"                  { return new Symbol(ChartFields.TBOOL, new java.lang.String(yytext())); }
"char"                  { return new Symbol(ChartFields.TCHAR, new java.lang.String(yytext())); }
"string"                { return new Symbol(ChartFields.TSTRING, new java.lang.String(yytext())); }

//"Get" { return new Symbol(ChartFields.GET); }
//"Remove" { return new Symbol(ChartFields.REMOVE); }
//"AddFirst" { return new Symbol(ChartFields.ADDFIRST); }
//"AddLast" { return new Symbol(ChartFields.ADDLAST); }
//"Random" { return new Symbol(ChartFields.RANDOM); }
//"RemoveFirst" { return new Symbol(ChartFields.REMOVEFIRST); }
//"RemoveLast" { return new Symbol(ChartFields.REMOVELAST); }
//"First" { return new Symbol(ChartFields.FIRST); }
//"Last" { return new Symbol(ChartFields.LAST); }
//"Clear" { return new Symbol(ChartFields.CLEAR); }
//"Size" { return new Symbol(ChartFields.SIZE); }
//"Contains" { return new Symbol(ChartFields.CONTAINS); }
//"Default" { return new Symbol(ChartFields.DEFAULT); }
//"In" { return new Symbol(ChartFields.IN); }
//"Empty" { return new Symbol(ChartFields.EMPTY); }

{floating}              { return new Symbol(ChartFields.Floating, new java.lang.Float(yytext())); }
{integer}               { return new Symbol(ChartFields.Integer, new java.lang.Integer(yytext())); }
{dqstring}              { return new Symbol(ChartFields.DQString, new java.lang.String(yytext())); }
{identifier}            { return new Symbol(ChartFields.Identifier, new java.lang.String(yytext())); }

"++" { return new Symbol(ChartFields.INC); }
"--" { return new Symbol(ChartFields.DEC); }
"&&" { return new Symbol(ChartFields.ANDAND); }
"||" { return new Symbol(ChartFields.OROR); }
"==" { return new Symbol(ChartFields.EQUALEQUAL); }
"!=" { return new Symbol(ChartFields.NOTEQUAL); }
"<" { return new Symbol(ChartFields.LESS); }
">" { return new Symbol(ChartFields.GREATER); }
"<=" { return new Symbol(ChartFields.LESSEQUAL); }
">=" { return new Symbol(ChartFields.GREATEREQUAL); }
//"->" { return new Symbol(ChartFields.ARROW); }
"+" { return new Symbol(ChartFields.PLUS); }
"-" { return new Symbol(ChartFields.MINUS); }
"*" { return new Symbol(ChartFields.TIMES); }
"/" { return new Symbol(ChartFields.DIV); }
"%" { return new Symbol(ChartFields.MOD); }
"~" { return new Symbol(ChartFields.LNOT); }
"!" { return new Symbol(ChartFields.NOT); }
"&" { return new Symbol(ChartFields.AND); }
"^" { return new Symbol(ChartFields.XOR); }
"|" { return new Symbol(ChartFields.OR); }
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

[ \t\r\n\f]             { /* Ignore white space characters with glue language lexer */ }
.                       { System.err.println("Illegal character: '" + yytext() + "'"); }