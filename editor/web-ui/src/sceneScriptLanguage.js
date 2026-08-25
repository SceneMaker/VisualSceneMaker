import { LanguageSupport, StreamLanguage } from "@codemirror/language";
import { Tag } from "@lezer/highlight";

export const noteTag = Tag.define();

const IDENT = /^[\p{L}@_][\p{L}\p{N}@_]*\b/u;
const WORD = /^[\p{L}\p{N}@]+/u;
const LANGUAGE = /^[\p{L}]{2}\b/u;
const BOOL = /^(true|false)\b/i;
const NUMBER = /^-?(?:\d+\.\d+|\d+)\b/;

const MODE = {
  INITIAL: "initial",
  SCENE_UNDL: "sceneUndl",
  SCENE_LANG: "sceneLang",
  SCENE_BODY: "sceneBody",
  TURN_HEAD: "turnHead",
  TURN_NAME: "turnName",
  TURN_INIT: "turnInit",
  TURN_BODY: "turnBody",
  TURN_FOOT: "turnFoot",
  ACTION_BODY: "actionBody",
  VARIABLE: "variable"
};

const sceneScriptLanguage = StreamLanguage.define({
  name: "scenescript",
  tokenTable: { note: noteTag },
  startState() {
    return {
      mode: MODE.INITIAL,
      returnMode: MODE.TURN_BODY,
      inComment: false,
      actionStage: "start"
    };
  },
  blankLine(state) {
    if (!state.inComment) {
      state.mode = MODE.INITIAL;
    }
    return null;
  },
  token(stream, state) {
    if (stream.sol() && !state.inComment) {
      if (
        state.mode === MODE.SCENE_BODY ||
        state.mode === MODE.TURN_BODY ||
        state.mode === MODE.TURN_FOOT ||
        state.mode === MODE.TURN_INIT
      ) {
        state.mode = MODE.TURN_HEAD;
      }
    }

    if (state.inComment) {
      if (stream.match("*/")) {
        state.inComment = false;
      } else {
        stream.skipToEnd();
      }
      return "comment";
    }

    if (stream.match("/*")) {
      state.inComment = true;
      return "comment";
    }

    if (stream.eatWhile(/[ \t]/)) {
      return null;
    }

    switch (state.mode) {
      case MODE.INITIAL:
        if (stream.match(/^(scene)\b/i)) {
          state.mode = MODE.SCENE_UNDL;
          return "keyword";
        }
        if (stream.match(/^#{1,3}(?=[ \t])/)) {
          stream.skipToEnd();
          return "header";
        }
        if (stream.match(/^(Note:|NOTE:)/)) {
          stream.skipToEnd();
          return "note";
        }
        stream.next();
        return "invalid";
      case MODE.SCENE_UNDL:
        if (stream.match(LANGUAGE)) {
          state.mode = MODE.SCENE_LANG;
          return "atom";
        }
        stream.next();
        return "invalid";
      case MODE.SCENE_LANG:
        if (stream.match(IDENT)) {
          state.mode = MODE.SCENE_BODY;
          return "definition";
        }
        stream.next();
        return "invalid";
      case MODE.SCENE_BODY:
        if (stream.match(/^(scene)\b/i)) {
          state.mode = MODE.SCENE_UNDL;
          return "keyword";
        }
        stream.next();
        return "invalid";
      case MODE.TURN_HEAD:
        if (stream.match(/^(scene)\b/i)) {
          state.mode = MODE.SCENE_UNDL;
          return "keyword";
        }
        if (stream.match(IDENT)) {
          state.mode = MODE.TURN_NAME;
          return "variableName";
        }
        stream.next();
        return "invalid";
      case MODE.TURN_NAME:
        if (stream.match(/^:/)) {
          state.mode = MODE.TURN_INIT;
          return "operator";
        }
        stream.next();
        return "invalid";
      case MODE.TURN_INIT:
      case MODE.TURN_BODY:
      case MODE.TURN_FOOT:
        return tokenTurn(stream, state);
      case MODE.ACTION_BODY:
        return tokenAction(stream, state);
      case MODE.VARIABLE:
        if (stream.match(IDENT)) {
          state.mode = state.returnMode || MODE.TURN_BODY;
          return "variableName";
        }
        stream.next();
        state.mode = state.returnMode || MODE.TURN_BODY;
        return "invalid";
      default:
        stream.next();
        return "invalid";
    }
  }
});

function tokenTurn(stream, state) {
  if (stream.match(/^\[/)) {
    state.mode = MODE.ACTION_BODY;
    state.returnMode = MODE.TURN_BODY;
    state.actionStage = "start";
    return "bracket";
  }
  if (stream.match(/^\]/)) {
    return "bracket";
  }
  if (stream.match(/^\$/)) {
    state.mode = MODE.VARIABLE;
    state.returnMode = MODE.TURN_BODY;
    return "variableName";
  }
  if (stream.match(/^'/)) {
    return null;
  }
  if (stream.match(/^[\.\?!,;]/)) {
    state.mode = MODE.TURN_FOOT;
    return "punctuation";
  }
  if (stream.match(WORD)) {
    state.mode = MODE.TURN_BODY;
    return null;
  }
  stream.next();
  return "invalid";
}

function tokenAction(stream, state) {
  if (stream.match(/^\]/)) {
    state.mode = state.returnMode || MODE.TURN_BODY;
    return "bracket";
  }
  if (stream.match(/^\$/)) {
    state.mode = MODE.VARIABLE;
    state.returnMode = MODE.ACTION_BODY;
    return "variableName";
  }
  if (stream.match(/^[:=]/)) {
    return "operator";
  }
  if (stream.match(BOOL)) {
    return "bool";
  }
  if (stream.match(NUMBER)) {
    return "number";
  }
  if (stream.match(/^'(?:[^'\\]|\\.)*'/)) {
    return "string";
  }
  if (stream.match(IDENT)) {
    if (state.actionStage === "start") {
      const rest = stream.string.slice(stream.pos);
      if (/^\s*:/.test(rest)) {
        state.actionStage = "expectAction";
        return "variableName";
      }
      state.actionStage = "features";
      return "function";
    }
    if (state.actionStage === "expectAction") {
      state.actionStage = "features";
      return "function";
    }
    return "propertyName";
  }
  stream.next();
  return "invalid";
}

export function sceneScript() {
  return new LanguageSupport(sceneScriptLanguage);
}
