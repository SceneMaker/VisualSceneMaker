package de.dfki.vsm.model.sceneflow.glue;

// Import Directives
import java_cup.runtime.Symbol;
%%

// Lexer Configuration /////////////////////////////////////////////////////////
// Enable Token Symbol Char Counting
%char 
// Enable Token Symbol Line Counting                                      
%line 
// Enable Token Symbol Column Counting                                     
%column   
// 16-Bit Unicode Character Encoding                                  
%unicode 
// Make Generated Scanner Class Final                                   
%final    
// Make Generated Scanner Class Public                                 
%public                                    
// Set The Scanner Interface 
%implements java_cup.runtime.Scanner  
// Set The Scanner Token Class         
%type java_cup.runtime.Symbol   
// Set The Scanner Class Name             
%class GlueLexxer      
// Set Scanner Token Function                       
%function next_token     
// Construtor Initialization                     
%init{  
    // Do nothing here
%init}
%eofval{
    // Return NULL At End Of File
    return null;
    // Return EOF Token At File End                                    
    //return create(GlueFields.EOF);
%eofval}
%eof{
    // Do Nothing
%eof}
%eofclose

// Macro Definitions ///////////////////////////////////////////////////////////
digit       = [0-9]
special     = [@<>'!?$&#/=~_:;,]|\\|\.|\+|\-|\*|\%|\||\[|\]|\(|\)|\{|\}\^
alpha       = [a-zA-Z]|[\u00A0-\u00FF]
win_new     = (\n)
mac_new     = (\r)
uni_new     = (\r\n)
newline     = ({win_new}|{uni_new}|{mac_new})
white       = ({newline}|([\ \t\f\b]))
//escape    = (\\)
integer     = (({digit})+) //[0-9]+ 
float       = (({digit})+\.({digit})+) //[0-9]+\.[0-9]+ 
identifier  = (({alpha}|_)({alpha}|{digit}|_)*) //[a-zA-Z_][a-zA-Z0-9_]* 
//character = (\'({alpha}|{digit}|{special})\')
string      = (\"({alpha}|{digit}|{special}|{white}|(\\\"))*\")
boolean     = (true|false)
object      = (null)
%%

// Token Definitions ///////////////////////////////////////////////////////////
"PlayScene"             { return new Symbol(GlueFields.PlayScene); }
"StopAction"            { return new Symbol(GlueFields.StopAction); }
"PlayAction"            { return new Symbol(GlueFields.PlayAction); }
"PlayDialogAct"         { return new Symbol(GlueFields.PlayDialogAct); }
"UnblockSceneGroup"     { return new Symbol(GlueFields.UnblockSceneGroup); }
"UnblockSceneScript"    { return new Symbol(GlueFields.UnblockSceneScript); }
"HistoryClearFlat"      { return new Symbol(GlueFields.HistoryClearFlat); }
"HistoryClearDeep"      { return new Symbol(GlueFields.HistoryClearDeep); }
"HistorySetDepth"       { return new Symbol(GlueFields.HistorySetDepth); }
"HistoryContains"       { return new Symbol(GlueFields.HistoryContains); }
"HistoryRunTimeOf"      { return new Symbol(GlueFields.HistoryRunTimeOf); }
"HistoryValueOf"        { return new Symbol(GlueFields.HistoryValueOf); }
"Timeout"               { return new Symbol(GlueFields.Timeout); }
"Random"                { return new Symbol(GlueFields.Random); }
"Contains"              { return new Symbol(GlueFields.Contains); }
"In"                    { return new Symbol(GlueFields.InState); }

"int"                   { return new Symbol(GlueFields.INT); }
"short"                 { return new Symbol(GlueFields.SHORT); }
"long"                  { return new Symbol(GlueFields.LONG); }
"float"                 { return new Symbol(GlueFields.FLOAT); }
"double"                { return new Symbol(GlueFields.DOUBLE); }
"bool"                  { return new Symbol(GlueFields.BOOL); }
"char"                  { return new Symbol(GlueFields.CHAR); }
"string"                { return new Symbol(GlueFields.STRING); }
"var"                   { return new Symbol(GlueFields.VAR); }
"object"                { return new Symbol(GlueFields.OBJECT); }
"new"                   { return new Symbol(GlueFields.NEW); }

"list"                  { return new Symbol(GlueFields.List); }
"struct"                { return new Symbol(GlueFields.Struct); }
"fun"                   { return new Symbol(GlueFields.Fun); }
"class"                 { return new Symbol(GlueFields.Class); }

{object}                { return new Symbol(GlueFields.OBJECT_LITERAL, null); }
{boolean}               { return new Symbol(GlueFields.BOOLEAN_LITERAL, Boolean.valueOf(yytext())); }
{integer}               { return new Symbol(GlueFields.INTEGER_LITERAL, Integer.valueOf(yytext())); }
{float}                 { return new Symbol(GlueFields.FLOAT_LITERAL, Float.valueOf(yytext())); }
{string}                { return new Symbol(GlueFields.STRING_LITERAL, yytext()); }
{identifier}            { return new Symbol(GlueFields.IDENTIFIER, yytext()); }

"=="                    { return new Symbol(GlueFields.EQUALEQUAL); }
"!="                    { return new Symbol(GlueFields.NOTEQUAL); }
"!-"                    { return new Symbol(GlueFields.NOTMINUS); }
"!~"                    { return new Symbol(GlueFields.NOTTILDE); }
"?-"                    { return new Symbol(GlueFields.QSTMINUS); }
"++"                    { return new Symbol(GlueFields.INC); }
"--"                    { return new Symbol(GlueFields.DEC); }
"&&"                    { return new Symbol(GlueFields.ANDAND); }
"||"                    { return new Symbol(GlueFields.OROR); }
"<"                     { return new Symbol(GlueFields.LESS); }
">"                     { return new Symbol(GlueFields.GREATER); }
"<="                    { return new Symbol(GlueFields.LESSEQUAL); }
">="                    { return new Symbol(GlueFields.GREATEREQUAL); }
"->"                    { return new Symbol(GlueFields.ARROW); }
"+"                     { return new Symbol(GlueFields.PLUS); }
"-"                     { return new Symbol(GlueFields.MINUS); }
"*"                     { return new Symbol(GlueFields.TIMES); }
"/"                     { return new Symbol(GlueFields.DIV); }
"%"                     { return new Symbol(GlueFields.MOD); }
"~"                     { return new Symbol(GlueFields.LNOT); }
"!"                     { return new Symbol(GlueFields.NOT); }
"&"                     { return new Symbol(GlueFields.AND); }
"^"                     { return new Symbol(GlueFields.XOR); }
"|"                     { return new Symbol(GlueFields.OR); }
"("                     { return new Symbol(GlueFields.LPAREN); }
")"                     { return new Symbol(GlueFields.RPAREN); }
"["                     { return new Symbol(GlueFields.LBRACK); }
"]"                     { return new Symbol(GlueFields.RBRACK); }
"{"                     { return new Symbol(GlueFields.LBRACE); }
"}"                     { return new Symbol(GlueFields.RBRACE); }
","                     { return new Symbol(GlueFields.COMMA); }
"."                     { return new Symbol(GlueFields.DOT); }
"?"                     { return new Symbol(GlueFields.QUESTION); }
":"                     { return new Symbol(GlueFields.COLON); }
"="                     { return new Symbol(GlueFields.EQUAL); }

[ \t\r\n\f]             { /* Ignore white space characters with glue language lexer */ }
.                       { System.err.println("Illegal character: '" + yytext() + "'"); }