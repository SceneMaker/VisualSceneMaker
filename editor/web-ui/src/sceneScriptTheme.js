import { EditorView } from "@codemirror/view";
import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import { tags } from "@lezer/highlight";
import { noteTag } from "./sceneScriptLanguage";

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
      paddingLeft: "0.95rem",
      boxShadow: "inset 3px 0 0 rgba(46, 93, 58, 0.28)"
    },
    ".cm-line.cm-scene-line-played": {
      boxShadow: "inset 3px 0 0 rgba(46, 93, 58, 0.28)"
    },
    ".cm-line.cm-scene-line-active": {
      boxShadow: "inset 3px 0 0 rgba(46, 93, 58, 0.62)"
    },
    ".cm-line.cm-scene-line-active-turn": {
      boxShadow: "inset 3px 0 0 rgba(46, 93, 58, 0.95)"
    },
    ".cm-turn-play-btn": {
      display: "inline-flex",
      alignItems: "center",
      justifyContent: "center",
      width: "16px",
      height: "16px",
      marginLeft: "0.35rem",
      padding: "0",
      verticalAlign: "baseline",
      position: "relative",
      top: "-1px",
      border: "1px solid #5b8edc",
      borderRadius: "4px",
      background: "rgba(91, 142, 220, 0.15)",
      color: "#3d5f96",
      cursor: "pointer"
    },
    ".cm-turn-play-btn:hover:not(:disabled)": {
      background: "rgba(91, 142, 220, 0.32)"
    },
    ".cm-turn-play-btn-disabled": {
      border: "1px solid #c9c4b8",
      background: "rgb(243, 240, 236)",
      color: "#b8b2a3",
      cursor: "default"
    },
    // Badge background keeps the teal used for "action" type command badges in the SceneFlow
    // graph view (SceneFlowView.svelte's CMD_TYPE_COLORS.action = "#007B76") — kept as a literal
    // here since that file's colors aren't exported as a shared module; update both if it ever
    // changes. Text color matches plain turn text (the theme's base "&" color) so the command
    // reads as part of the turn rather than a differently-colored foreign element.
    ".cm-action-compact": {
      display: "inline-flex",
      alignItems: "center",
      gap: "0.2em",
      padding: "0 0.1rem",
      borderRadius: "3px",
      background: "rgba(0, 123, 118, 0.15)",
      color: "#1c1b15",
      fontFamily: "inherit",
      fontSize: "1em",
      cursor: "grab"
    },
    ".cm-action-compact-dragging": {
      opacity: "0.4"
    },
    ".cm-action-compact-swatch": {
      display: "inline-block",
      width: "0.8em",
      height: "0.8em",
      margin: "0 0.2em",
      borderRadius: "2px",
      border: "1px solid rgba(0, 0, 0, 0.35)",
      verticalAlign: "baseline",
      position: "relative",
      top: "1px"
    },
    // Same accent used for selected command badges in the SceneFlow graph view (SceneFlowView.svelte).
    ".cm-action-compact-selected": {
      outline: "2px solid #5b8fdc",
      outlineOffset: "1px"
    },
    ".cm-action-compact-icon": {
      width: "0.85em",
      height: "0.85em",
      flex: "none",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "2",
      strokeLinecap: "round",
      strokeLinejoin: "round"
    },
    ".cm-heading-compact": {
      fontWeight: "700",
      color: "#1c1b15"
    },
    ".cm-heading-compact-1": { fontSize: "1.3em" },
    ".cm-heading-compact-2": { fontSize: "1.15em" },
    ".cm-heading-compact-3": { fontSize: "1.05em" },
    ".cm-note-compact": {
      fontStyle: "italic",
      color: "#1c1b15",
      background: "rgba(122, 106, 63, 0.1)",
      borderLeft: "3px solid rgba(122, 106, 63, 0.5)",
      padding: "0 0.4em"
    },
    ".cm-md-bold": {
      fontWeight: "700"
    },
    ".cm-md-italic": {
      fontStyle: "italic"
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
  { tag: tags.invalid, color: "#ff0000" },
  { tag: tags.heading, color: "#1c1b15", fontWeight: "700" },
  { tag: noteTag, color: "#1c1b15", fontStyle: "italic" }
]);

export const sceneScriptHighlighting = syntaxHighlighting(sceneScriptHighlightStyle);
