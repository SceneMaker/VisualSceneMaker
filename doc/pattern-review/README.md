# Interaction Pattern Review: Understandability Study

Expert review instrument for the pattern catalogue in
[`../vsm-modelling-support.md`](../vsm-modelling-support.md) §3.

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

## The two language variants

| File | Language | Reviewer sees | localStorage key | CSV `language` |
|---|---|---|---|---|
| `sceneflow-pattern-review.html` | English | "flow", explained first | `vsm.pattern-review.v1` | `en` |
| `sceneflow-pattern-review-de.html` | German | `Ablauf`, then named "flow" | `vsm.pattern-review.de.v1` | `de` |

The storage keys differ deliberately: a reviewer who opens both files gets two independent
sessions instead of one overwriting the other.

German-specific decisions:

- **The agent is `die Agentin`** throughout, matching the portrait, and the reviewer is addressed
  as `Sie`. The person interacting is always `die Person`, whose gender is left open.
- **`Ablauf` carries the explanation, "flow" stays the name.** German has the everyday word English
  lacks, so the concept is built with `Ablauf` and the paragraph closes with "In Visual SceneMaker
  wird dieser Ablauf *flow* genannt". The English label is what the reviewer will meet in the
  software, whose interface is English, so translating it away would help nobody.
- **The three-paragraph structure is kept even where German could be terser**, so the two files
  stay comparable as instruments rather than one explaining more than the other.
- **Umlauts and `ß` are entities in markup and `\u` escapes in JavaScript.** German text is far more
  exposed to the mis-decoding described below than English is, so the ASCII rule matters more here,
  not less. German quotation marks are `&bdquo;` and `&ldquo;` (`„` and `“`), not the English pair.

Ratings should be pooled **within** a language, not across, since a translation is never a perfectly
equivalent stimulus. The `language` column makes that filterable rather than dependent on which file
a CSV came from. Join across languages on `item_id`, never on `pattern_label`, because the labels
are translated.

## Running it

Both files are complete standalone documents: no server, no build, no network calls, and the SIA
portrait is inlined as a data URI. Double-clicking a file works, and so does emailing it. Answers
are held in `localStorage` and saved as the rater types, so a session survives a reload. The rater
presses **Export CSV** / **CSV exportieren** at the end and returns the file.

Both are also published as links, for reviewers who would rather not handle a file (each is private
until shared from its own share menu):

- English: <https://claude.ai/code/artifact/e5c891f2-5c2c-495f-9cff-957e1050f76f>
- German: <https://claude.ai/code/artifact/4bacc4dd-95b2-43b2-8ff4-ceb3a70c0890>

Each published copy is generated from its repo file with the document wrapper stripped, because the
publishing pipeline supplies its own. Regenerate with:

```bash
python3 - <<'EOF'
import pathlib, re
for name in ('sceneflow-pattern-review.html', 'sceneflow-pattern-review-de.html'):
    src = pathlib.Path(name).read_text(encoding='utf-8')
    head = re.search(r'<head>(.*?)</head>', src, re.S).group(1)
    body = re.search(r'<body>(.*?)</body>', src, re.S).group(1)
    frag = (re.search(r'<title>.*?</title>', head, re.S).group(0) + '\n\n'
            + re.search(r'<style>.*?</style>', head, re.S).group(0) + '\n' + body)
    pathlib.Path('/tmp/' + name).write_text(frag, encoding='utf-8')
EOF
```

Three hard rules for editing either file, the first two learned from real breakage:

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

## CSV output

One row per pattern, always 23 rows, so unrated items are visible as gaps rather than missing
records. UTF-8 with BOM, CRLF, RFC 4180 quoting (verified against comments containing commas,
quotes and newlines). Session-level fields repeat on every row, which is the tidy long format
R and pandas expect.

| Column | Notes |
|---|---|
| `participant_id` | Auto-assigned review code, e.g. `R-8M3LK2` |
| `background` | Free text, optional |
| `language` | `en` or `de`; pool within a language, not across |
| `level` | `1`, `2` or `3` |
| `level_label` | Translated, e.g. `Reacting and coordinating` / `Reagieren und Koordinieren` |
| `item_id` | e.g. `3.8`, a stable key matching concept doc §3. **Join on this**, not on the label |
| `pattern_label` | Translated, e.g. `Taking turns, with listening behaviour` |
| `rating` | `0` to `5`; **empty string** if unrated (note `0` is a valid rating, not a gap) |
| `comment` | Per-item free text |
| `general_comment` | Session-level remarks, repeated per row |
| `exported_at` | ISO 8601 export timestamp |

Filename: `sceneflow-pattern-review-<en|de>_<participant>_<YYYY-MM-DD>.csv`.

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
