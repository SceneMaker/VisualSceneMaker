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
    ".cm-scene-played": {
      backgroundColor: "rgba(46, 93, 58, 0.08)",
      borderLeft: "3px solid rgba(46, 93, 58, 0.25)"
    },
    ".cm-scene-active": {
      backgroundColor: "rgba(46, 93, 58, 0.18)",
      borderLeft: "3px solid rgba(46, 93, 58, 0.6)"
    },
    ".cm-scene-activeTurn": {
      backgroundColor: "rgba(46, 93, 58, 0.28)",
      borderLeft: "3px solid rgba(46, 93, 58, 0.9)"
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
