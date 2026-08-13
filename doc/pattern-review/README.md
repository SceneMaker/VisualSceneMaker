# Interaction Pattern Review: Understandability Study

Expert review instrument for the pattern catalogue in
[`../sceneflow-modelling-support-concept.md`](../sceneflow-modelling-support-concept.md) §3.

**Purpose:** decide the final level cut and the pattern naming (concept doc §8, Q1) on evidence
rather than intuition. Raters are psychologists and other non-technical domain experts, which is the
audience the patterns are meant to serve.

## What is measured

Per pattern, a single 0 to 5 item: **how understandable is this description?**
(`0` = not understandable at all, `5` = completely understandable), plus an optional free-text
comment. Usefulness, need and preference are deliberately not measured, because the instrument
tests communication rather than desirability. Raters are told this explicitly.

23 patterns in 3 levels: 8 sequences, 6 parallel-activity, 9 reactive/coordination. Each is
described in one abstract sentence plus one concrete example in which an agent acts, or an agent
and a person interact. Implementation status is deliberately hidden, since several patterns are not
built yet and that is irrelevant to whether the description communicates.

Reviewers are assigned a review code automatically (`R-` plus six unambiguous characters, no `O`,
`0`, `I` or `1`), so nothing has to be typed before rating starts. The code persists in
`localStorage` and survives **Clear**, so a reviewer who restarts keeps one identity.

## Running it

`sceneflow-pattern-review.html` is a complete standalone document: no server, no build, no network
calls, and the SIA portrait is inlined as a data URI. Double-clicking the file works, and so does
emailing it. Answers are held in `localStorage` and saved as the rater types, so a session survives
a reload. The rater presses **Export CSV** at the end and returns the file.

Two hard rules for editing it, both learned from real breakage:

1. **Keep the source pure ASCII.** Write typographic punctuation as HTML entities
   (`&ldquo;`, `&middot;`) in markup and as `\u` escapes in JavaScript. A reviewer opening the file
   from disk gives the browser no HTTP `Content-Type`, and Safari then decoded UTF-8 bytes as
   Latin-1 and rendered `â€"` throughout. The `<meta charset="utf-8">` in the head fixes that on
   its own, but an ASCII-only file cannot be mis-decoded by anything, including mail clients.
   Check with:
   `python3 -c "print(any(b>127 for b in open('sceneflow-pattern-review.html','rb').read()))"` → `False`
2. **No dashes as sentence punctuation** anywhere a reviewer can read. Use a relative clause or a
   second sentence instead. Hyphens inside compound words (`follow-up`, `sub-dialogue`) are fine.
3. **Introduce no vocabulary the reviewer has to absorb.** See Terminology below. In an instrument
   that measures comprehension, an unfamiliar word does not merely read badly, it contaminates the
   measurement: the rating then partly records puzzlement over the word rather than the clarity of
   the pattern.

## Terminology

**"Flow" is explained before it is named.** The introduction first states the choice an author
faces, which is *what* the agent says and shows versus *how the session proceeds*, then compares the
second to an interview schedule, then names the two properties a paper schedule lacks (it runs
itself, and it reacts while the session is under way). Only then does it say that Visual SceneMaker
calls this the flow. Two reasons this order matters:

- For psychologists, **"flow" is a false friend**, not merely unfamiliar. Flow is an established
  construct in their own field, the state of absorbed engagement. A reader has to suppress the
  reading they know best, which produces quiet misunderstanding rather than a question.
- The what-versus-when separation is load-bearing for several items, and the repo states it exactly
  once, in half a sentence of `editor/web-ui/public/sceneflow-help.html`: "SceneScript (the dialogue
  text) supplies *what* gets said; SceneFlow decides *when*." The introduction here is the fuller
  version of that sentence and is worth reusing in the help files, the tutorial tour, and the
  README, which currently carry four differing one-line descriptions of VSM and none respectively.

**"Narration" was considered and rejected** as the framing. It names the content rather than the
conduct, so it collides with the one separation the system is built on; `SceneFlowNarrativeExplainer`
already uses "narrative" for prose generated *from* a flow. It also implies a single forward line,
which is exactly what Levels 2 and 3 are not, and its register suits fiction rather than clinical
intake.

**No coined noun for a parallel line of activity.** An earlier draft called these "strands". That
failed twice over: German `Strand` means *beach*, and Merriam-Webster's first sense of the English
noun is likewise shore or beach, with the intended sense ("one of the elements interwoven in a
complex whole") appearing last in its fourth entry and illustrated literarily. The sentences carry
the meaning without a noun, so they now say "other things run alongside it" and "the session
continues only once all of them have finished". If a specific term is ever unavoidable, use
**track**: it is unclaimed in VSM, carries simultaneity from multitrack audio, and translates as
`Spur`. Avoid **activity**, which collides with the runtime's `Activity` (a single executed action,
not a continuing line); avoid **thread** as visible jargon duplicating the interpreter's own term;
avoid **branch**, which already means the Level 1 conditional split.

Also published as a link, for reviewers who would rather not handle a file:
<https://claude.ai/code/artifact/e5c891f2-5c2c-495f-9cff-957e1050f76f>
(private until shared from the page's share menu). The published copy is generated from this file
with its document wrapper stripped, because the publishing pipeline supplies its own.

## CSV output

One row per pattern, always 23 rows, so unrated items are visible as gaps rather than missing
records. UTF-8 with BOM, CRLF, RFC 4180 quoting (verified against comments containing commas,
quotes and newlines). Session-level fields repeat on every row, which is the tidy long format
R and pandas expect.

| Column | Notes |
|---|---|
| `participant_id` | Auto-assigned review code, e.g. `R-8M3LK2` |
| `background` | Free text, optional |
| `level` | `1`, `2` or `3` |
| `level_label` | e.g. `Reacting and coordinating` |
| `item_id` | e.g. `3.8`, a stable key matching concept doc §3 |
| `pattern_label` | e.g. `Taking turns, with listening behaviour` |
| `rating` | `0` to `5`; **empty string** if unrated (note `0` is a valid rating, not a gap) |
| `comment` | Per-item free text |
| `general_comment` | Session-level remarks, repeated per row |
| `exported_at` | ISO 8601 export timestamp |

Filename: `sceneflow-pattern-review_<participant>_<YYYY-MM-DD>.csv`.

## Analysis notes

- Low mean with high variance points at *ambiguous wording*; uniformly low points at a
  *wrong abstraction*. The second is the one that should change the level cut.
- Compare within levels: if Level 2 items rate consistently below Levels 1 and 3, the
  parallelism framing is the problem, not the individual patterns.
- The open question behind the study is whether 3 levels is the right cut, or whether Level 3
  should split into "reactive" and "multi-party coordination" (concept doc §8). Items 3.6 to 3.9 are
  the ones to watch for that.
- Language: currently English. If raters are German-speaking, a translated variant should be a
  separate file so ratings stay comparable within a language version.
