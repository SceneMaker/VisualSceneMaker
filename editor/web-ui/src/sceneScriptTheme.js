import { EditorView } from "@codemirror/view";
import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import { tags } from "@lezer/highlight";

export const sceneScriptTheme = EditorView.theme(
  {
    "&": {
      color: "#1c1b15",
      backgroundColor: "#fbfaf7"
    },
    ".cm-content": {
      caretColor: "#1c1b15"
    },
    "&.cm-focused .cm-cursor": {
      borderLeftColor: "#1c1b15"
    },
    "&.cm-focused .cm-selectionBackground, .cm-selectionBackground": {
      backgroundColor: "rgba(46, 93, 58, 0.18)"
    },
    ".cm-activeLine": {
      backgroundColor: "rgba(46, 93, 58, 0.08)"
    },
    ".cm-activeLineGutter": {
      backgroundColor: "rgba(46, 93, 58, 0.14)",
      color: "#1c1b15"
    },
    ".cm-matchingBracket": {
      backgroundColor: "rgba(46, 93, 58, 0.2)",
      color: "#1c1b15"
    },
    ".cm-line.cm-scene-line": {
      position: "relative",
      paddingLeft: "0.95rem"
    },
    ".cm-line.cm-scene-line::before": {
      content: "\"\"",
      position: "absolute",
      left: "0.2rem",
      top: "0.14em",
      bottom: "0.14em",
      width: "3px",
      borderRadius: "999px",
      backgroundColor: "rgba(46, 93, 58, 0.28)"
    },
    ".cm-line.cm-scene-line-played::before": {
      backgroundColor: "rgba(46, 93, 58, 0.28)"
    },
    ".cm-line.cm-scene-line-active::before": {
      backgroundColor: "rgba(46, 93, 58, 0.62)"
    },
    ".cm-line.cm-scene-line-active-turn::before": {
      backgroundColor: "rgba(46, 93, 58, 0.95)"
    }
  },
  { dark: false }
);

const sceneScriptHighlightStyle = HighlightStyle.define([
  { tag: tags.keyword, color: "#1e14c8", fontWeight: "600" },
  { tag: tags.atom, color: "#1e14c8", fontWeight: "600" },
  { tag: tags.definition, color: "#645064", fontStyle: "italic" },
  { tag: tags.variableName, color: "#1e14c8", fontStyle: "italic" },
  { tag: tags.function, color: "#1e14c8", fontWeight: "600" },
  { tag: tags.propertyName, color: "#645064", fontStyle: "italic" },
  { tag: tags.comment, color: "#5e912f", fontStyle: "italic" },
  { tag: tags.bool, color: "#22cc33" },
  { tag: tags.number, color: "#22cc33" },
  { tag: tags.string, color: "#22cc33" },
  { tag: tags.operator, color: "#1c1b15" },
  { tag: tags.punctuation, color: "#1c1b15" },
  { tag: tags.bracket, color: "#1c1b15" },
  { tag: tags.invalid, color: "#ff0000" }
]);

export const sceneScriptHighlighting = syntaxHighlighting(sceneScriptHighlightStyle);
