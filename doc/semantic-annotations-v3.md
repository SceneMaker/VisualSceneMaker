# Semantic Annotations Schema v3

The JSON stored in `semantic-annotations.json` in a project root.

**v3 (2026-08-05) adds `clauses`, `anchors` and `commands` to each annotation.** The flat `basic`
block is unchanged and still emitted, so a v2 reader keeps working against a v3 document. Nothing
validates the version number strictly — it is provenance telling a consumer whether the fine-grained
layers are present.

## Goals

- Keep annotations in a separate project file.
- Preserve backward compatibility with v1 and v2 payloads.
- Explicit provenance per layer for hybrid processing:
  - `basic`, `clauses`, `anchors` via the UD parser
  - `dialogueAct`, `themeRheme` via LLM
- Give behavior-command placement a structural label space (`anchors`).

## Top-level document

```json
{
  "version": 3,
  "schema": { "id": "vsm.semantic.annotations", "version": 3 },
  "scriptHash": "sha256:...",
  "generatedAt": "2026-08-05T10:27:16.987Z",
  "updatedAt": "2026-08-05T10:27:16.987Z",
  "provenance": {
    "source": "server-analyze-script|editor-web-ui|semantic-ud",
    "service": "semantic-ud|llm|hybrid",
    "model": "stanza-de|gpt-5.2|...",
    "analyzedAt": "2026-08-05T10:27:16.987Z",
    "layers": {
      "basic": "ud|llm|heuristic|unknown",
      "dialogueAct": "llm|heuristic|unknown",
      "themeRheme": "llm|heuristic|unknown"
    }
  },
  "stats": { "sentences": 11, "annotations": 11, "commands": 10 },
  "warnings": ["line 4: UD analysis unavailable"],
  "annotations": []
}
```

`stats` and `warnings` are written by `POST /api/v1/projects/{pid}/semantic/analyze-script`; both are
optional. `stats.commands` is a useful invariant — it must match the number of `ActionObject`s in the
stored script.

## Annotation object

One per sentence (see *Sentence units* below).

```json
{
  "id": "s1",
  "sentence": 1,
  "line": 4,
  "speaker": "Xenia",
  "text": "Hallo ich bin Xenia.",
  "scriptFrom": 43,
  "scriptTo": 168,

  "basic":       { "…": "v2-compatible flat role set — unchanged" },
  "clauses":     [ "…" ],
  "anchors":     [ "…" ],
  "commands":    [ "…" ],
  "dialogueAct": { "label": "greeting", "scheme": "dailydialog-v1", "confidence": 0.93 },
  "themeRheme":  { "theme": "Hallo", "rheme": "", "confidence": 0.62 },

  "provenance": {
    "analyzedAt": "2026-08-05T10:27:16.987Z",
    "layers": { "basic": "ud", "clauses": "ud", "anchors": "ud" }
  }
}
```

`text` is the **clean text**: the utterance with all inline behavior commands removed. That is what
the parser saw, and every offset below is a script offset remapped back from it.

### `basic` — v2 shape, two additive changes

Flat, one role set per sentence: `subject`, `verb`, `object`, `predicate`, `address`,
`addressPhrase`, and the `*Modifiers` arrays. Head-token spans only, one object only. Retained
verbatim so existing consumers and renderers keep working; **prefer `clauses` for new work**, since
`basic` mixes roles across clauses and reports only the first object.

Two additions, both backward compatible (new key, new field — nothing renamed or removed):

**`verbModifiers`.** Modifiers used to be collected for `subject`, `object`, `address` and
`predicate` only, so an adverbial of the verb was dropped entirely. That silently erased the entire
content of the shortest evaluative utterances — `Super gemacht!`, `Toll gemacht!` — where UD puts the
adjective on the participle and the verb is the only other word. Those are precisely the turns that
carry the most behavior commands, so the clause read as structureless exactly where it is densest.

**`usage` on every modifier span**, `"adverbial"` or `"attributive"`, beside the existing `pos`. The
two are deliberately separate: `pos` is the word class, `usage` is the slot it fills. `Super` in
`Super gemacht` is `pos: "adjective"`, `usage: "adverbial"` — German ADJD. Labelling such a word
`adverb`, as the code previously would have, hides the adjective the author actually wrote.

Three mis-attributions in `basic` were fixed at the same time:

| Input | Was | Now |
|---|---|---|
| `Ja, das ist gut!` | `gut` reported as both `predicate` **and** `subjectModifiers` | `predicate` only |
| `Ja, das ist gut!` | `address: das`, which is the subject | no address |
| `Hallo Bob, schön dass Du da bist.` | `object: bist` — the verb itself, so every verb modifier was re-emitted as an object modifier | no object |

The first came from a predicative special case that attached the copular root adjective to the
subject; `select_predicate()` already returns that same word, so it was always a duplicate, drawn
twice in two different role colours. The second came from a comma fallback that accepted any
post-comma `nsubj` pronoun; it now requires a second-person pronoun, since an addressee cannot be
`das`. Names and nouns still pass.

Also, `predicate` is now reported for **verbless predicative fragments** (`Sehr gut!`, `Klasse!`).
There is no copula in these, so the copula-driven lookup found nothing and `basic` came back empty
for the most common shape of positive feedback — while `clauses` reported a predicate, leaving the
two layers disagreeing.

Known upstream limitation, not a defect here: in `Das hast Du sehr gut gemacht.` Stanza assigns
`Das`=`nsubj` and `Du`=`obj`, which is semantically reversed for this fronted-object order. The
extraction faithfully mirrors the parse. Tracked as an expected-fail eval case (`de-modverb-03`),
which asserts the correct reading and is expected to fail until the parser improves.

### `clauses` — new in v3

```json
"clauses": [
  {
    "id": "c0",
    "type": "main",
    "from": 354, "to": 386,
    "text": "Lass mich einen Vorschlag machen",
    "roles": {
      "verb":      { "head":   { "text": "machen", "from": 380, "to": 386 },
                     "phrase": { "text": "Lass mich einen Vorschlag machen", "from": 354, "to": 386 },
                     "modifiers": [ { "…": "same shape as the basic *Modifiers spans" } ],
                     "confidence": 0.96 },
      "subject":   { "head": { "…": "" }, "phrase": { "…": "" }, "confidence": 0.96 },
      "predicate": { "…": "" },
      "address":   { "…": "" }
    },
    "objects": [
      { "kind": "direct", "deprel": "obj", "case": "Acc",
        "head":   { "text": "Ball", "from": 28, "to": 32 },
        "phrase": { "text": "den roten Ball", "from": 18, "to": 32 },
        "confidence": 0.95 },
      { "kind": "indirect", "deprel": "obl:arg", "case": "Dat",
        "head": { "…": "" }, "phrase": { "…": "" } },
      { "kind": "prepositional", "deprel": "obl", "preposition": "nach",
        "head": { "…": "" }, "phrase": { "…": "" } }
    ]
  }
]
```

- `type` — `main` | `subordinate` | `relative` | `coordinate` | `parataxis`
- `roles.*.head` is the head token span; `roles.*.phrase` is the full subtree span, clipped to the
  clause, edge punctuation trimmed — `den roten Ball`, not `Ball`.
- The verb **does** carry a `phrase`, and it is always the clause span itself: the verb heads its
  clause, so its subtree is the whole clause. `Ich sehe dich.` gives the verb `sehe` the phrase
  `Ich sehe dich`. It is emitted for uniformity across roles and carries no information beyond the
  clause's own `from`/`to` — read those instead. Two consumers already act on this: the editor's
  phrase wash skips the verb, since shading it would tint the whole clause and hide every mark inside
  it, and `build_anchors` falls back to the verb's **head** token, because a phrase whose boundaries
  are the clause boundaries offers no interior position to anchor to. The same fallback applies to a
  verbless predicative root, which heads its clause for the same reason.
- `objects[].kind` — `direct` | `indirect` | `prepositional` | `clausal` | `oblique`. *All*
  object-like dependents of the clause verb are listed, not just the first. `deprel` and `case` are
  kept so a consumer can second-guess the mapping; note German UD encodes the indirect object as
  `obl:arg` with `Case=Dat`, **not** `iobj`.
- A clause's `from`/`to` **encloses any clause embedded in it** — that is the structure, not an
  error. Derive positions from `anchors`, never from clause spans.
- Clause roles are resolved by assigning each word to its nearest clause-root ancestor, which is what
  stops a subordinate clause's subject being reported against the main clause's verb.

#### Per-role `modifiers`, and the clause `linker`

Each role entry carries its own `modifiers` array, in the same shape as the `basic` ones (`pos`,
`usage`, span). Restricted to words inside the clause, so a role never collects a neighbour's
modifier.

Each clause may also carry a `linker`: the conjunction or interrogative adverb that joins it to its
parent — `wie`, `dass`, `weil`. It is deliberately **not** a modifier. Stanza tags the `wie` of
`wie wir zusammen weitermachen` as `SCONJ`/`KOUS` carrying an `advmod` relation, but it modifies
nothing; it marks the seam between two clauses, which is itself a position where a behavior command
can sit. Forcing it into a role would misdescribe it, and dropping it lost the boundary.

#### Why the editor now draws roles per clause, not from `basic`

`basic` has one slot per role for the whole sentence and fills each from whichever clause matched
first. On

    Lass mich einen Vorschlag machen wie wir zusammen weitermachen.

it returns `subject: wir` and `verb: machen` — **from different clauses**. `wir` is the subject of
`weitermachen`; `machen` is the main clause's verb. Drawn together they read as a subject-verb pair
that does not exist. And with only one verb slot, `weitermachen` could never be marked at all, nor
its adverbial `zusammen`, which modifies it rather than `machen`.

The editor therefore takes role heads and modifiers from `clauses` whenever any clause has roles, and
falls back to `basic` only when none does. `basic` is still emitted unchanged for other consumers.
The one mark still taken from `basic` is `addressHead`, which has no clause-level equivalent.

Consequence worth knowing: a multi-clause sentence now shows several subject/verb marks rather than
one. That is denser, and correct — the previous single set was not merely incomplete but wrong.

### `anchors` — new in v3

The candidate positions for placing a behavior command: the label space a placement model predicts
over. Named structurally, not numerically, because a structural name survives re-wording where a
character offset does not.

```json
"anchors": [
  { "slot": "utterance-initial", "clauseId": null, "tokenIndex": 0, "from": 71, "to": 71 },
  { "slot": "before-object", "clauseId": "c0", "role": "object", "kind": "direct",
    "tokenIndex": 3, "from": 18, "to": 18 },
  { "slot": "before-final-punct", "clauseId": null, "tokenIndex": 4, "from": 164, "to": 164 },
  { "slot": "utterance-final", "clauseId": null, "tokenIndex": 4, "from": 165, "to": 165 }
]
```

Slots: `utterance-initial`, `clause-initial`, `before-`/`after-subject`, `before-`/`after-verb`,
`before-`/`after-predicate`, `after-address`, `before-`/`after-object`, `before-final-punct`,
`utterance-final`.

- `from` == `to`: an anchor is a **position**, not a span. Encoding it as a degenerate span lets the
  server's generic span remapper rewrite it to script coordinates with no special-casing.
- `tokenIndex` — the number of spoken tokens preceding the anchor, added server-side. **This is what
  makes an anchor comparable with `commands[].tokenIndex`**: the parser and the script model tokenise
  differently, so the character offset is the only shared coordinate.
- Several slots can share a position (a clause-initial boundary is also its subject's start). Both
  labels are offered; they carry different structural meaning, and the model predicts a label.
- Only the *trailing* boundary of an address is offered: a command conventionally follows a vocative
  (`Hallo $user, [emotion] …`), it does not precede it.
- The verb anchors on its head token rather than its phrase, since the phrase would be the clause.

Measured coverage: on `plugins/charamel-embed/ExampleProject`, **10 of 10 authored commands sit at a
position the inventory offers**.

### `commands` — new in v3

The inline behavior commands removed from `text`, attached to the first annotation of the sentence
(`sentence` groups them).

```json
"commands": [
  { "name": "emotion", "actor": "Bob", "tokenIndex": 4,
    "cleanOffset": 15, "scriptFrom": 515, "scriptTo": 527 }
]
```

- `tokenIndex` — spoken tokens preceding the command; `0` is utterance-initial. **Adjacent commands
  share one index**, which is correct: the position is a boundary, not a character offset.
- `scriptFrom`/`scriptTo` span the command's **actor-qualified name** (`time: init`), not the whole
  bracket — that is all `ActionObject` records in `lower`/`upper`.

## Sentence units

One annotation per **sentence**, not per `SceneUttr`. The script grammar ends an utterance at *any*
punctuation, commas included, so `Hallo $user, wie geht's Dir?` parses as two `SceneUttr`s;
`UtteranceProjection.sentencesOf(turn)` merges consecutive utterances until one ends in a
sentence-final mark. Handing a parser the fragment `Hallo $user,` would also defeat the UD service's
own greeting guardrail.

## Compatibility and migration

- v1 (no `schema`/`provenance`) and v2 (no `clauses`/`anchors`) documents are valid input.
- On load, missing metadata is normalized with defaults; unknown keys are preserved, so a v3
  document survives a save/load round trip unchanged.
- `basic` renderer logic needs no change for v3.
- **Known caveat, pre-existing:** on load, `scriptHash` is overwritten with the *current* script's
  hash rather than preserved, so it cannot currently be used to detect a stale annotation file.
