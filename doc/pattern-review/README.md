# Interaction Pattern Review — Understandability Study

Expert review instrument for the pattern catalogue in
[`../sceneflow-modelling-support-concept.md`](../sceneflow-modelling-support-concept.md) §3.

**Purpose:** decide the final level cut and the pattern naming (concept doc §8, Q1) on evidence
rather than intuition. Raters are psychologists and other non-technical domain experts — the same
audience the patterns are meant to serve.

## What is measured

Per pattern, a single 0–5 item: **how understandable is this description?**
(`0` = not understandable at all, `5` = completely understandable), plus an optional free-text
comment. Deliberately *not* measured: usefulness, need, or preference — the instrument tests
communication, not desirability. Raters are told this explicitly.

23 patterns in 3 levels: 8 sequences, 6 parallel-activity, 9 reactive/coordination. Each is
described in one abstract sentence plus one concrete example involving an agent and a person.
Implementation status is deliberately hidden — several patterns are not built yet, and that is
irrelevant to whether the description communicates.

## Running it

`sceneflow-pattern-review.html` is self-contained: no server, no build, no network calls. Answers
are held in `localStorage` and saved as the rater types, so a session survives a reload. The rater
presses **Export CSV** at the end and returns the file.

Published for distribution at:
<https://claude.ai/code/artifact/e5c891f2-5c2c-495f-9cff-957e1050f76f>
(private until shared from the page's share menu). Redeploy from this file path to update that URL.

Note: the file is an Artifact-style fragment (no `<!doctype>`/`<head>`/`<body>` wrapper — those are
added at publish time). Use the published URL for distribution; opening the raw file from disk
renders in quirks mode.

## CSV output

One row per pattern — always 23 rows, so unrated items are visible as gaps rather than missing
records. UTF-8 with BOM, CRLF, RFC 4180 quoting (verified against comments containing commas,
quotes and newlines). Session-level fields repeat on every row, which is the tidy long format
R and pandas expect.

| Column | Notes |
|---|---|
| `participant_id` | Rater-chosen code; `anonymous` if left blank |
| `background` | Free text, optional |
| `level` | `1`, `2` or `3` |
| `level_label` | e.g. `Reacting and coordinating` |
| `item_id` | e.g. `3.8` — stable key, matches concept doc §3 |
| `pattern_label` | e.g. `Taking turns, with listening behaviour` |
| `rating` | `0`–`5`; **empty string** if unrated (note `0` is a valid rating, not a gap) |
| `comment` | Per-item free text |
| `general_comment` | Session-level remarks, repeated per row |
| `exported_at` | ISO 8601 export timestamp |

Filename: `sceneflow-pattern-review_<participant>_<YYYY-MM-DD>.csv`.

## Analysis notes

- Low mean with high variance points at *ambiguous wording*; uniformly low points at a
  *wrong abstraction* — the second is the one that should change the level cut.
- Compare within levels: if Level 2 items rate consistently below Levels 1 and 3, the
  parallelism framing is the problem, not the individual patterns.
- The open question behind the study is whether 3 levels is the right cut, or whether Level 3
  should split into "reactive" and "multi-party coordination" (concept doc §8). Items 3.6–3.9 are
  the ones to watch for that.
- Language: currently English. If raters are German-speaking, a translated variant should be a
  separate file so ratings stay comparable within a language version.
