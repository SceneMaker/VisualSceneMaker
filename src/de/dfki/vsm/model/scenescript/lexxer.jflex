////////////////////////////////////////////////////////////////////////////////
// Start User Code /////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Package Definition
package de.dfki.vsm.model.scenescript;
// All Import Directives
import de.dfki.vsm.util.syn.*;
// Java Cup Runtime
import java.io.*;

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
// Set The Scanner Class Name     
%class ScriptLexxer     
// Set The Scanner Interface                            
%extends SyntaxDocLexxer
// Set The Scanner Token Function
%function next_token                        
// Set The Scanner Class Token            
%type ScriptSymbol   
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
// Define Lexical States
%state YY_SCENE_HEAD
%state YY_SCENE_UNDL
%state YY_SCENE_LANG
%state YY_SCENE_NAME
%state YY_SCENE_BODY

%state YY_TURN_HEAD
%state YY_TURN_NAME
%state YY_TURN_INIT
%state YY_TURN_BODY
%state YY_TURN_FOOT

%state YY_VARIABLE
%state YY_ACTION_BODY
%state YY_ERROR_STATE
%state YYCOMMENT
////////////////////////////////////////////////////////////////////////////////
// End Directives //////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
%{
    //////////////////////////////////////////////////////////////////////////////
    // Start User Code ///////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////

    // Reset The Token Scanner 
    @Override
    public final void init(
            final Reader reader,
            final boolean comment,
            final boolean newline,
            final boolean whitespace) {
        // Reset The Internal Stuff
        yyreset(reader);
        // Initialize The Comment Flag
        mComment = comment;
        // Initialize The Comment Flag
        mNewline = newline; //comment
        // Initialize The Whitespace Flag
        mWhiteSpace = whitespace;
        // Reset  Token Object Index
        mTokenIndex = -1;
        // Reset  Last Type Of Token
        mLastToken = -1;
        // Reset  Last Lexxer State
        mLastState = YYINITIAL;
    }
    // The Token Scanner Construction
    public ScriptLexxer(
        Reader reader,
        boolean comment, 
        boolean newline,
        boolean whitespace) {
        // Initialize The Reader
        init(
            reader,
            comment,
            newline,
            whitespace);
    }
    // The Token Scanner Construction
    public ScriptLexxer(
        boolean comment,
        boolean newline,
        boolean whitespace) {
        // Initialize The Reader
        init(
            null,
            comment,
            newline,
            whitespace);
    }
    // Creating A New Token
    public final ScriptSymbol create(final int type) {
        // Set The Last Token Type
        mLastToken = type;
        // Increment The Token Index
        ++mTokenIndex;
        // Compute The Field Value
        final String field = ScriptSymbol.getField(type);
        // Compute The State Value
        final String lexic = ScriptSymbol.getState(yystate());
        // Create The New Token
        final SyntaxDocToken token = new SyntaxDocToken(
            mTokenIndex,    // Set The Index
            yystate(),      // Set The State
            type,           // Set The Token
            yychar,         // Set Left Bound
            yychar + yytext().length(), 
            yyline,         // Set Line
            yycolumn,       // Set Column
            yytext(),       // Set Content
            field,          // Set Field
            lexic);         // Set State
        // Create The New Symbol
        final ScriptSymbol symbol = 
            new ScriptSymbol(type, yychar, yychar + yytext().length(), token);
        // Print Some Information
        //mLogger.message("Scanning Symbol " + symbol + "");    
        // Return The New Symbol
        return symbol;
    }
////////////////////////////////////////////////////////////////////////////////
// End User Code ///////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
%}
////////////////////////////////////////////////////////////////////////////////
//////////////////// Macro Definitions /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Underline
UNDERLINE      = "_"
// Colon Marks
COLONMARK      = ":"
// Single Quote
SINGLEQUOTE    = "'"
// Action Head
ACTIONHEAD     = "["
// Action Foot
ACTIONFOOT     = "]"
// Assignment
ASSIGNMENT     = "="
// Placeholder 
PLACEHOLDER     = [$]
// Punctuations
PUNCTUATION     = [\.\?!,;]
// Characters
CHARACTER       = [a-zA-Z\u00e4\u00c4\u00f6\u00d6\u00fc\u00dc\u00df\u0040\u00b5\u00C0\u00C1\u00C2\u00C6\u00C7\u00C8\u00C9\u00CA\u00CB\u00CE\u00CF\u00D4\u00D6\u00E0\u00E2\u00E6\u00E7\u00E8\u00E9\u00EA\u00EB\u00EE\u00EF\u00F4\u00F6\u00FB\u00FC\u00FF]
// Specials
SPECIAL = [!?$%&#/=~_:;,]|\\|\.|\+|\*|\-|\||\[|\]|\(|\)//'
// Whitespaces
BLANK           = ([ ])
TAB             = ([\t])
FF              = ([\f])
// Whitespaces
WHITESPACE      = ({BLANK}|{TAB}|{FF})
// Newlines
CR              = ([\r])
LF              = ([\n])
NEWLINE         = ({CR}|{LF}|{CR}{LF})
// Datatypes
BOOLEAN         = (true|false)
DIGITAL         = [0-9]
SIGN            = ([-]?)
INTEGER         = {SIGN}(0|[1-9][0-9]*)
FLOATING        = {SIGN}((0|([1-9][0-9]*))\.[0-9]+)
//STRING          = (\"({CHARACTER}|{DIGITAL}|{SPECIAL}|{WHITESPACE})*\")
STRING          = ('({CHARACTER}|{DIGITAL}|{SPECIAL}|{WHITESPACE})*')
link            = (<a {WHITESPACE}+ "href="\"(.*?)\" {WHITESPACE}+ "target="\"(.*?)\">(.*?)<\/a>)
// Abbreviation
ABBREVIATION    = {SINGLEQUOTE}
// Language
LANGUAGE        = ({CHARACTER}{CHARACTER})
// Simple Words
SIMPLEWORD     = ({CHARACTER}+|{INTEGER}|{link})
// Identifiers
IDENTIFIER      = ({CHARACTER}({CHARACTER}|{DIGITAL}|_)*)
// Comments
COMMENTHEAD    = (\/\*)
COMMENTFOOT    = (\*\/)
// Scenes & Turns
SCENEHEADER     = (scene|Scene)
%%
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// All Comments 
<YYINITIAL>
{COMMENTHEAD}(.*){COMMENTFOOT}
{   
    if(mComment) {
        // Return The Comment Token
        return create(ScriptFields.COMMENT);
    }
}
// Scene Header 
<YYINITIAL>
{SCENEHEADER}
{   
    // Enter The Scene Head Scope
    yybegin(YY_SCENE_HEAD);
    // Return The Scene Head Token
    return create(ScriptFields.SCENEHEADER);
}
// Whitespace
<YYINITIAL>
{WHITESPACE}
{   
    if(mWhiteSpace) {
        // Return The Whitespace Token
        return create(ScriptFields.WHITESPACE);
    } 
}
// Newline
<YYINITIAL>
{NEWLINE}
{   
    if(mWhiteSpace) {
        // Return The Newline Token
        return create(ScriptFields.NEWLINE);
    } 
}
// Error State
<YYINITIAL>
.
{   
    // Enter The Error State Scope
    yybegin(YY_ERROR_STATE);
    // Return The Scene Head Token
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Underline
<YY_SCENE_HEAD>
{UNDERLINE}
{   
    // Enter The Scene Undl Scope
    yybegin(YY_SCENE_UNDL);
    // Return The Scene Name Token
    return create(ScriptFields.UNDERLINE);
}
// Error State
<YY_SCENE_HEAD>
.|{NEWLINE}
{   
    // Enter The Error State Scope
    yybegin(YY_ERROR_STATE);
    // Return The Scene Head Token
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Underline
<YY_SCENE_UNDL>
{LANGUAGE}
{   
    // Enter The Scene Lang Scope
    yybegin(YY_SCENE_LANG);
    // Return The Scene Lang Token
    return create(ScriptFields.LANGUAGE);
}
// Error State
<YY_SCENE_UNDL>
.|{NEWLINE}
{   
    // Enter The Error State Scope
    yybegin(YY_ERROR_STATE);
    // Return The Scene Head Token
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Whitespace
<YY_SCENE_LANG>
{WHITESPACE}
{   
    if(mWhiteSpace) {
        // Return The Whitespace Token
        return create(ScriptFields.WHITESPACE);
    } 
}
<YY_SCENE_LANG>
{IDENTIFIER}
{   
    // Enter The Scene Lang Scope
    yybegin(YY_SCENE_NAME);
    // Return The Scene Name Token
    return create(ScriptFields.IDENTIFIER);
}
// Error State
<YY_SCENE_LANG>
.|{NEWLINE}
{   
    // Enter The Error State Scope
    yybegin(YY_ERROR_STATE);
    // Return The Scene Head Token
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Whitespace
<YY_SCENE_NAME>
{WHITESPACE}
{   
    if(mWhiteSpace) {
        // Return The Scene Lang Token
        return create(ScriptFields.WHITESPACE);
    } 
}
<YY_SCENE_NAME>
{COLONMARK}
{   
    // Enter The Scene Body Scope
    yybegin(YY_SCENE_BODY);
    // Return The Scene Foot Token
    return create(ScriptFields.COLONMARK);
}
// Error State
<YY_SCENE_NAME>
.|{NEWLINE}
{   
    // Enter The Error State Scope
    yybegin(YY_ERROR_STATE);
    // Return The Scene Head Token
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Turn Header
<YY_SCENE_BODY>
{NEWLINE}
{
    // Enter The Turn Head Scope
    yybegin(YY_TURN_HEAD);
    // Return The Turn Head Token
    if(mWhiteSpace) {
        // Return The Scene Lang Token
        return create(ScriptFields.NEWLINE);
    }
}
// Error State
<YY_SCENE_BODY>
.
{   
    // Enter The Error State Scope
    yybegin(YY_ERROR_STATE);
    // Return The Scene Head Token
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Turn Name
<YY_TURN_HEAD>
{IDENTIFIER}
{
    // Enter The Error State Scope
    yybegin(YY_TURN_NAME);
    // Return The Turn Head Token
    return create(ScriptFields.IDENTIFIER);
}
<YY_TURN_HEAD>
{WHITESPACE}
{   
    if(mWhiteSpace) {
        // Return The Scene Lang Token
        return create(ScriptFields.WHITESPACE);
    } 
}
// Turn Header
<YY_TURN_HEAD>
{NEWLINE}
{
    // Enter The Turn Head Scope
    yybegin(YYINITIAL);
    // Return The Turn Head Token
    if(mWhiteSpace) {
        // Return The Scene Lang Token
        return create(ScriptFields.NEWLINE);
    }
}
// Error State
<YY_TURN_HEAD>
.
{   
    // Enter The Error State Scope
    yybegin(YY_ERROR_STATE);
    // Return The Scene Head Token
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Colon Mark
<YY_TURN_NAME>
{COLONMARK}
{
    // Enter The Turn Head Scope
    yybegin(YY_TURN_INIT);
    // Return The Turn Head Token
    return create(ScriptFields.COLONMARK);
}
<YY_TURN_NAME>
{WHITESPACE}
{   
    if(mWhiteSpace) {
        // Return The Scene Lang Token
        return create(ScriptFields.WHITESPACE);
    } 
}
// Error State
<YY_TURN_NAME>
.|{WHITESPACE}
{   
    // Enter The Error State Scope
    yybegin(YY_ERROR_STATE);
    // Return The Scene Head Token
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
// Placeholder /////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
<YY_VARIABLE>
{IDENTIFIER}
{
    // Leave The Placeholder Scope 
    yybegin(mLastState);
    // Return The Parameter Token 
    return create(ScriptFields.VARIABLE);
}
<YY_VARIABLE>
.
{
    // Leave The Placeholder Scope 
    yybegin(YY_ERROR_STATE);
    // Create An Error Here
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
// Turn Init ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Simple Words
<YY_TURN_INIT>
{SIMPLEWORD}
{
    //
    yybegin(YY_TURN_BODY);
    // Return The Simple Word Token
    return create(ScriptFields.SIMPLEWORD);
}
// Abbreviations
<YY_TURN_INIT>
{ABBREVIATION}
{
    //
    yybegin(YY_TURN_BODY);
    // Return The Abbreviation Token
    return create(ScriptFields.ABBREVIATION);
}
// Placeholders
<YY_TURN_INIT>
{PLACEHOLDER}
{
    // Enter The Placeholder Scope 
    yybegin(YY_VARIABLE);
    // Remember The Last Lexxer State
    mLastState = YY_TURN_BODY;
    // Return The Parameter Token 
    return create(ScriptFields.PLACEHOLDER);
}
// Action Head
<YY_TURN_INIT>
{ACTIONHEAD}
{
    // Enter The Action Tag Scope
    yybegin(YY_ACTION_BODY);
    // Return The New Token Object
    return create(ScriptFields.ACTIONHEAD);
}
// Whitespace
<YY_TURN_INIT>
{WHITESPACE}
{   
    //
    yybegin(YY_TURN_BODY);
    if(mWhiteSpace) {
        // Return The Scene Lang Token
        return create(ScriptFields.WHITESPACE);
    } 
}
// Error State
<YY_TURN_INIT>
.|{NEWLINE}
{
    // Leave The Placeholder Scope 
    yybegin(YY_ERROR_STATE);
    // Create An Error Here
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
// Turn Body ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Simple Words
<YY_TURN_BODY>
{SIMPLEWORD}
{
    // Return The Simple Word Token
    return create(ScriptFields.SIMPLEWORD);
}
// Abbreviations
<YY_TURN_BODY>
{ABBREVIATION}
{
    // Return The Abbreviation Token
    return create(ScriptFields.ABBREVIATION);
}
// Placeholders
<YY_TURN_BODY>
{PLACEHOLDER}
{
    // Enter The Placeholder Scope 
    yybegin(YY_VARIABLE);
    // Remember The Last Lexxer State
    mLastState = YY_TURN_BODY;
    // Return The Parameter Token 
    return create(ScriptFields.PLACEHOLDER);
}
// Action Head
<YY_TURN_BODY>
{ACTIONHEAD}
{
    // Enter The Action Tag Scope
    yybegin(YY_ACTION_BODY);
    // Return The New Token Object
    return create(ScriptFields.ACTIONHEAD);
}
// Whitespace
<YY_TURN_BODY>
{WHITESPACE}
{   
    if(mWhiteSpace) {
        // Return The Scene Lang Token
        return create(ScriptFields.WHITESPACE);
    } 
}
// Punctuations
<YY_TURN_BODY>
{PUNCTUATION}
{
    // Enter The Action Tag Scope
    yybegin(YY_TURN_FOOT);
    // Return The Punctuation Token
    return create(ScriptFields.PUNCTUATION);
}
// Error State
<YY_TURN_BODY>
.|{NEWLINE}
{
    // Leave The Placeholder Scope 
    yybegin(YY_ERROR_STATE);
    // Create An Error Here
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
// Turn Foot ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Simple Words
<YY_TURN_FOOT>
{SIMPLEWORD}
{
    //
    yybegin(YY_TURN_BODY);
    // Return The Simple Word Token
    return create(ScriptFields.SIMPLEWORD);
}
// Abbreviations
<YY_TURN_FOOT>
{ABBREVIATION}
{
    //
    yybegin(YY_TURN_BODY);
    // Return The Abbreviation Token
    return create(ScriptFields.ABBREVIATION);
}
// Placeholders
<YY_TURN_FOOT>
{PLACEHOLDER}
{
    // Enter The Placeholder Scope 
    yybegin(YY_VARIABLE);
    // Remember The Last Lexxer State
    mLastState = YY_TURN_BODY;
    // Return The Parameter Token 
    return create(ScriptFields.PLACEHOLDER);
}
// Action Head
<YY_TURN_FOOT>
{ACTIONHEAD}
{
    // Enter The Action Tag Scope
    yybegin(YY_ACTION_BODY);
    // Return The New Token Object
    return create(ScriptFields.ACTIONHEAD);
}
// Whitespace
<YY_TURN_FOOT>
{WHITESPACE}
{   
    //
    yybegin(YY_TURN_BODY);
    if(mWhiteSpace) {
        // Return The Scene Lang Token
        return create(ScriptFields.WHITESPACE);
    } 
}
// Turn Header
<YY_TURN_FOOT>
{NEWLINE}
{
    // Enter The Turn Head Scope
    yybegin(YY_TURN_HEAD);
    // Return The Turn Head Token
    if(mWhiteSpace) {
        // Return The Scene Lang Token
        return create(ScriptFields.NEWLINE);
    }
}
// Error State
<YY_TURN_FOOT>
.
{
    // Leave The Placeholder Scope 
    yybegin(YY_ERROR_STATE);
    // Create An Error Here
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
// Action Body /////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
<YY_ACTION_BODY>
{ASSIGNMENT}
{
    return create(ScriptFields.ASSIGNMENT);
}
<YY_ACTION_BODY>
{PLACEHOLDER}
{
    // Enter The Placeholder Scope 
    yybegin(YY_VARIABLE);
    // Remember The Last Lexxer State
    mLastState = YY_ACTION_BODY;
    // Return The Parameter Token 
    return create(ScriptFields.PLACEHOLDER);
}
<YY_ACTION_BODY>
{BOOLEAN}
{
    return create(ScriptFields.BOOLEAN);
}
<YY_ACTION_BODY>
{INTEGER}
{
    return create(ScriptFields.INTEGER);
}
<YY_ACTION_BODY>
{FLOATING}
{
    return create(ScriptFields.FLOATING);
}
<YY_ACTION_BODY>
{STRING}
{
    return create(ScriptFields.STRING);
}
<YY_ACTION_BODY>
{IDENTIFIER}
{
    return create(ScriptFields.IDENTIFIER);
}
<YY_ACTION_BODY>
{ACTIONFOOT}
{
    // Leave The Action Tag Scope 
    yybegin(YY_TURN_BODY);
    // Return The New Token Object
    return create(ScriptFields.ACTIONFOOT);
}
////////////////////////////////////////////////////////////////////////////////
// Error State /////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
<YY_ERROR_STATE>
(.|{NEWLINE})
{
    // Create An Error Here
    return create(ScriptFields.ERRORSTATE);
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
{WHITESPACE}
{ 
    if(mWhiteSpace) {
        return create(ScriptFields.WHITESPACE);
    }
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
{NEWLINE}
{ 
    if(mWhiteSpace) {
        return create(ScriptFields.NEWLINE);
    } 
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
.
{
    // Create An Error Here
    return create(ScriptFields.ERRORSTATE);
}