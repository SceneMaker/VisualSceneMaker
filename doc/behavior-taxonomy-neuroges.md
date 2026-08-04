# Behavior Taxonomy for VSM Commands: NEUROGES® as Starting Point

Investigation, 2026-08-04. Feeds Phase 2.2 of
`~/.claude/plans/behavior-command-placement-learning.md`.

Sources: Müller, Lausberg & Gebhard, *AI-based Analysis and Generation of Nonverbal Behavior*
(ICMI Companion '26); Lausberg & Sloetjes, *Coding gestural behavior with the NEUROGES–ELAN
system* (Behav Res Methods 2009); Lausberg & Kryger, *Gestisches Verhalten als Indikator
therapeutischer Prozesse* (Psychotherapie-Wissenschaft 2011); project notes `NEUROGES.txt`.

## 1. Why NEUROGES fits this problem

The placement service needs a label space over ~60 opaque command names, learned from very few
examples. NEUROGES supplies three properties that no ad-hoc category list would:

1. **Function, not form.** Categories name what a movement *does* (deixis, emphasis,
   self-regulation) rather than how it looks. Placement is a function-level regularity — an author
   puts a deictic gesture on the referent, not "pointdownright" on the referent.
2. **Ordered value axes.** Each category's values sit on a polar axis (Focus: within body → on
   body → on attached object → on separate object → on person → in space; Function:
   non-representational → egocentric representational → allocentric representational). This is the
   answer to the sparse-data problem: back-off can move to a *neighbouring value on the axis*
   instead of collapsing to a generic prior. Principled smoothing, not a hand-tuned hierarchy.
3. **Analysis/generation symmetry.** The same tags annotate human video and specify SIA behavior,
   which is the ICMI paper's core argument. For VSM concretely: annotated therapy/interaction
   corpora become reference distributions for what the agent should do, and constraints
   ("suppress dominance cues", "no excess self-touch in anxiety") become expressible.

## 2. What NEUROGES actually contains — and the version question

The sources describe three successive versions, and they do not match:

| Source | Structure |
|---|---|
| Lausberg & Sloetjes 2009 | 3 modules: I kinetic (segmentation, structure, location), II bimanual relation, III function + type |
| Lausberg & Kryger 2011 | 4 modules: I segmentation/structure/location, II bimanual coordination, III function/semantics, IV rest positions |
| **ICMI '26 (current ®) — PINNED** | 7 categories: Activation, Structure, Focus, Contact, Formal Relation, Function, Type — vertical (build on each other) + horizontal (polar axes), 3–11 values each |

**Decision (2026-08-04, confirmed by PG): pinned to the current 7-category system.** The ICMI
paper commits to it, and it is the version with the documented polar structure we exploit for
back-off. The version string is recorded in the taxonomy file (`taxonomy.systemVersion`), same
discipline as `specVersion` for plugin specs; any later NEUROGES revision is a version bump with
a migration, never a silent re-tag.

Consequences of pinning: the 2009 three-module vocabulary is a *historical reference* only —
useful because its Function/Type tree is published in full (below), but its module numbering must
not leak into the taxonomy file. Where this document cites Module I/II/III, that is provenance for
where a value came from, not the schema.

### 2.1 Value inventories — RESOLVED from the coding manual

The full manual is available at
`~/NextCloudDFKI/PG Research/Projects/Proposals/NEUROGES/Resources/Hedda Lausberg The NEUROGES Analysis System for Nonverbal Behavior.md`.
Every gap previously flagged here is now closed, and the authoritative values are recorded in
`core/src/main/resources/behavior-taxonomy.json`. Summary, with the corrections that mattered:

| Category | Module / step | Values (in horizontal order) | Axis semantics |
|---|---|---|---|
| Activation | I / 1 | movement, rest/pose | segmentation, not polar |
| Structure | I / 2 | irregular, repetitive, phasic, shift, aborted (+ r/p rest, r/p pose) | irregular→phasic = increasing complexity, motor arousal → formative motor processes; `shift` = transition between still positions; `aborted` at the axis end |
| Focus | I / 3 | within body, on body, on attached object, on separate object, on person, in space | body-internal → body-external (loci of sensory stimulation) |
| Contact | II / 4 | act on each other, act as a unit, act apart (+ r/p crossed, closed, open; special `prep-retract`) | bihemispheric sensorimotor activation decreases / expressive freedom increases; for rest/pose, psychological openness and rapport increase crossed → closed → open |
| Formal Relation | II / 5 | symmetrical, right hand dominance, left hand dominance, asymmetrical | increasing complexity of neural control (dominance order holds for right-handers) |
| Function | III / 6 | emotion/attitude, emphasis, egocentric deictic, egocentric direction, pantomime, form presentation, spatial relation presentation, motion quality presentation, object-oriented action, subject-oriented action, emblem/social convention (+ special `different functions`) | emotional motions → gestures → actions → conventionalized; within the seven gesture values, more emotional → more cognitive with increasing creative complexity and abstraction |
| Type | III / 7 | 24 values, dependent on Function (labels carry the parent, e.g. `emphasis-baton`) | not polar |

Corrections to what this document previously assumed:

- **Structure order was wrong.** I had listed phasic first; the manual's axis begins at
  *irregular*. The 2009 values *continuous* and *stopped/holding* are **not** current — the
  current set is irregular / repetitive / phasic / shift / aborted, with rest/pose split off into
  `r/p rest` and `r/p pose`.
- **Contact order was reversed.** The ICMI paper's prose reads "act apart, act as a unit, act on
  each other"; the manual's axis runs *act on each other → act as a unit → act apart*.
- **Formal Relation is not the 2009 set.** *complementary* and *independent* are gone; the current
  values are symmetrical / right hand dominance / left hand dominance / asymmetrical.
- **Function has 11 values, not the 12 of the 2009 tree**, and the ICMI paper's
  "non-representational → egocentric representational → allocentric representational" maps onto
  three named main groups: **egocentric gestures** {egocentric deictic, egocentric direction,
  pantomime}, **presentation gestures** {form, spatial relation, motion quality}, and **actions**
  {object-oriented, subject-oriented}. `emphasis` sits before the egocentric group as the
  non-representational gesture value.
- **The self-regulation branch is now precisely addressable.** What 2009 called *autostimulation*
  is `subject-oriented action` plus the supplementary category **Trigger/Motive**
  (physical regulation | visual appearance | mental regulation | unknown), which the manual applies
  *only* to subject-oriented actions. This is a better grounding for §4.2 than the invented
  `cospeech: false` flag — the flag is now derived from it.

The manual also defines **seven supplementary categories** the earlier draft did not know about,
two of which are directly useful here: **Referent** (material | non-material, applied to the six
representational Function values) and **Target Location** (right side | left side | body-midline |
both sides) — the latter is what Lausberg & Kryger 2011 used to show that *where* in gesture space
a referent is placed is itself clinically meaningful. Also available: Technique of Presentation,
Efforts (Laban), Temporal Structure, Execution Hemi-Space. Referent and Trigger/Motive are marked
work-in-progress in the manual, with hypothetical values — worth honouring that status in anything
we publish.

Two questions remain, and they are judgement calls for the NEUROGES side rather than lookups:
(i) which categories are *required* vs *optional* when tagging a generation asset rather than an
observed movement, given §3's asymmetry; (ii) whether a reduced author-facing vocabulary is
acceptable, and which collapses preserve validity (see §8.4).

The 2009 function/type tree (Pointing, Space, Objects, Motion, Emphasis, Convention, Emotion,
Autostimulation, Objective purpose, Position shift, Incomplete gestures, Several gestures in a
unit) is retained here only as provenance for older annotations. It must not be used for new
tagging: several of its values have no counterpart in the pinned version — notably *Several
gestures in a unit*, which is why VSM's `sequence` command has no NEUROGES value and must be
decomposed into its member gestures before it can be tagged.

## 3. The asymmetry that shapes the design

NEUROGES is an **analysis** system for observed movement. VSM commands are **generation**
directives that trigger a pre-authored animation on a character engine. The consequence:

- **Module I (Structure) and Module II (Contact, Formal Relation) are not properties of the
  command.** VSM does not control trajectory, dynamics, or hand laterality — the animation clip
  does. These are *declared attributes of the repertoire asset*, determinable only by coding the
  animation on video.
- **Module III (Function, Type) plus Focus is the layer authors actually reason about** and the
  layer placement depends on.

This splits the annotation work into two targets with very different costs:

| | **(a) Repertoire annotation** | **(b) Instance annotation** |
|---|---|---|
| Unit | one animation / command | one authored placement in a script |
| Tags | Structure, Contact, Formal Relation, Function, Type, Focus | intended Function/Type + anchor slot |
| Method | video coding by trained NEUROGES raters | scene author picks from (a)'s vocabulary |
| Frequency | once per character engine | continuous, as authoring happens |

Placement learning consumes **(b)**. But (b) is only cheap if (a) exists — otherwise every author
is re-deciding what `pointovershoulder` even is. **(a) is the prerequisite, and it is a natural
collaboration item with Hedda's group:** code the VuppetMaster/Charamel animation repertoire once,
and the tags serve both selection and placement thereafter.

## 4. Mapping the existing repertoire

All 65 behavior commands (charamel-ws 61, charamel-embed 4) are tagged in
`core/src/main/resources/behavior-taxonomy.json`, from names, summaries and vendor descriptions
only — **no video, so every gesture tag is `evidence: inferred-from-name`**. Coverage:

| | n |
|---|---|
| Function value assigned | 24 |
| rest/pose or shift — no Function applies by definition | 12 |
| In NEUROGES scope but unresolved without video | 14 |
| No NEUROGES unit at all (gaze, face-only, stage, control) | 15 |
| **Total** | **65** |

By Function value:

| Function | Commands | n |
|---|---|---|
| emotion/attitude | angry, bored, crazy, demanding, disappointed, disgust, happy, pensively, sad, smile, surprised, `emotion` (charamel-embed) | 12 |
| egocentric deictic | pointopenpalm, pointovershoulder, pointdownleft, pointdownright | 4 |
| emblem/social convention | nod, headshake, shakehead, wave, countleft | 5 |
| emphasis | emphasis (→ `emphasis-baton`), showpalm (→ `emphasis-palm-out`) | 2 |
| subject-oriented action | hairback (Trigger/Motive: visual appearance) | 1 |
| **egocentric direction** | **— none —** | **0** |
| **pantomime** | **— none —** | **0** |
| **form presentation** | **— none —** | **0** |
| **spatial relation presentation** | **— none —** | **0** |
| **motion quality presentation** | **— none —** | **0** |
| object-oriented action | — none confirmed (handontable is ambiguous) | 0 |

Four findings, in order of consequence:

**4.1 The entire representational branch is empty.** Five consecutive Function values —
egocentric direction, pantomime, form presentation, spatial relation presentation, motion quality
presentation — have zero commands. That is the whole *egocentric gestures* main group except
deixis, plus the whole *presentation gestures* main group. These are exactly the gestures whose
placement is most tightly determined by utterance semantics, because they attach to a referent or
a spatial relation: NEUROGES applies the supplementary **Referent** category only to these six
Function values. **The placement learner's most linguistically-determined branch has nothing to
place.** The single plausible candidate in the repertoire is `handscircle` (could be `form-shape`,
`spatial relation-route` or `motion quality-manner`); `explain` is the next most likely. This is
the strongest result the taxonomy work produces: a justified animation request list for the
character-engine side, with the coding-system rationale attached.

A sixth Function value is also empty: **`object-oriented action`**. It is excluded from the
animation request rather than counted as a gap, because it means changing the external physical
world and a seated upper-body character has no props to act on. That exclusion is
character-specific, so it belongs in the animation request; `uncoveredFunctionValues()` reports the
gap as it is, all six. (`handontable` is the only near-candidate and is ambiguous between an
object-oriented action and a rest/pose on a separate object.)

**4.2 Twelve commands are rest/pose, not movement — so no Function applies at all.** This is
sharper than the earlier "autostimulation" reading. `armscrossed`, `foldhands`, `handstogether`,
`legscrossed`, `luemmeln`, `protectdefensive`, `protectassertive`, `sit`, `sitbrave`, `sittalk`,
`sitnodd` and `headtilt` are postures and transitions, which NEUROGES codes on the *Activation*
(rest/pose) and *r/p Contact* axes — Function and Type are assessed only for phasic and repetitive
units. The r/p Contact axis is directly meaningful here: **psychological openness and rapport
increase along crossed → closed → open**, so `armscrossed` (crossed) and `openarm` (open) sit at
opposite ends of a validated clinical axis rather than being two arbitrary poses.

Only one command is a genuine self-regulatory action: `hairback`, which is `subject-oriented
action` with Trigger/Motive `visual appearance`. `think` is a likely second (`mental regulation`)
pending video. Either way the earlier conclusion holds and is now properly grounded: these are not
co-speech, should be driven by affect state (the `alma` plugin) rather than authored inline, and
`vsm.cospeech: false` is derived from the Function/Trigger-Motive tags rather than asserted.
`protectassertive` is the clearest candidate dominance cue — precisely the class the ICMI paper
argues should be constrainable in anxiety-provoking contexts — and is flagged for review.

**4.3 Fourteen commands cannot be resolved without video**, and 19 of 65 carry confidence ≤ 0.4.
Not a defect of the analysis — it is what makes video-based repertoire coding necessary rather
than optional. Two structural causes worth noting: some commands are **composites** (`sittalk`,
`sitnodd` bundle a posture with a movement) and must be decomposed before they can be tagged at
all; and `sequence` is a VSM composition construct with no counterpart in the pinned version,
since *several gestures in a unit* was a 2009 value that no longer exists.

**4.4 Fifteen commands have no NEUROGES unit at all, and eleven more have an unknown channel.**
NEUROGES codes four body parts — upper limbs, lower limbs, head, trunk — so `nod`/`headshake`/
`headtilt` are *in* scope as head movements, while gaze (4 commands) and facial expression are
not: gaze and face appear inside NEUROGES value definitions as movement *criteria*, never as coded
units. Stage effects (3) and plugin control (5) are not behavior in any sense.

Separately, the eleven emotion presets sit on channel `unknown`. Their communicative function is
clear (`emotion/attitude`), but NEUROGES's emotion/attitude is a limb movement obligatorily
accompanied by a postural-facial expression — so whether a given avatar preset contains a codable
movement unit, or only changes the face, is undecidable from the name. `unknown` is deliberately
distinct from an out-of-scope channel: these may well turn out to be codable.

## 5. The artifact

`core/src/main/resources/behavior-taxonomy.json` — on the core classpath (Java 17, Android-safe) so
the placement service, the corpus tooling and the Web UI can all read one copy. Structure: the
seven pinned categories with their authoritative values, orders, axis semantics and per-value
interrater reliabilities; the seven supplementary categories; the channel list; and one entry per
command. A worked entry:

```json
{ "plugin": "charamel-ws", "command": "pointdownleft", "channel": "hand-gesture",
  "neuroges": { "structure": "phasic", "focus": "in space",
                "function": "egocentric deictic", "type": "deictic-external target" },
  "supplementary": { "targetLocation": "left side" },
  "vsm": { "cospeech": true, "affiliate": "referent" },
  "evidence": "inferred-from-name", "confidence": 0.7,
  "note": "Target Location matters here: Lausberg & Kryger (2011) showed that where a referent is placed in gesture space is itself meaningful." }
```

Design decisions worth stating:

- **`neuroges` vs `vsm` namespacing.** Everything under `neuroges` and `supplementary` uses
  authoritative value names and must not be edited without a `systemVersion` bump. Everything under
  `vsm` (`cospeech`, `affiliate`) is our own derived data and carries no NEUROGES authority. Value
  names are kept verbatim from the manual — spaces and slashes included (`"act on each other"`,
  `"emotion/attitude"`) — rather than kebab-cased, so string comparison against
  NEUROGES-annotated data needs no mapping table.
- **Absent keys mean "not applicable", not "unknown".** A rest/pose entry has no `function` key at
  all, because NEUROGES assesses Function only for phasic and repetitive units. An explicit
  `"function": null` means undetermined and pending video. Tooling must distinguish these.

- **The taxonomy file is the authority, not `plugin-properties.json`.** Tags are evidence-based
  research data with coder provenance and confidence; plugin specs are hashed and version-bumped
  on structural change (`CLAUDE.md`), and taxonomy revisions would churn `specVersion` across
  every plugin for no functional reason. `uiCategory` stays in the plugin spec for UI grouping and
  should be *derived* from the taxonomy.
- **`evidence` is mandatory** — `video-coded | declared-by-vendor | inferred-from-name` — so
  §4's guesses are never mistaken for NEUROGES-grade annotation. The corpus and any published
  numbers must be able to filter on it.
- **`cospeech`** gates the placement service, per §4.2.
- **`affiliate`** records what the behavior attaches to semantically (`referent`, `rheme`,
  `accented-word`, `clause`, `whole-utterance`, `none`) — the bridge from taxonomy to the anchor
  slots of Phase 1.4.
- **The vocabulary is now authoritative; the tags are not.** With §2.1 resolved, every value name
  in the file comes from the manual. What remains provisional is which value each command *gets*:
  56 of 65 entries are `evidence: inferred-from-name`, and 19 carry confidence ≤ 0.4. So the file
  is usable today for VSM-internal grouping, `cospeech` gating and placement back-off, but nothing
  in it may be reported as NEUROGES-grade until the corresponding entries are `video-coded`.
- **Reliability figures travel with the taxonomy, not with our annotations.** The per-value
  EasyDIAg figures in the file are NEUROGES's own (Lausberg & Slöetjes 2016) and describe the
  ceiling a trained rater achieves on that value — see §7.
- **The author-facing reduction lives in the file too**, as `displayGroups`: 11 Function values and
  24 Type values collapsed to 11 display columns (Background, Emotion, Emphasis, Pointing,
  Depiction, Convention, Action, Posture, Gaze, Face, Unclassified). Array order is display order.
  Resolution is Function-first, then channel — so an emotion preset on the `unknown` channel still
  lands in Emotion, while `background` (no NEUROGES unit) resolves via its `stage` channel. These
  groups are VSM-derived and carry no NEUROGES authority; they live here rather than in the Web UI
  because collapsing a validated 11-value category is a reviewable design decision, not a UI detail
  (§8.4). Two deliberate choices: `Unclassified` is **visible**, so a command whose Function is
  undetermined shows up as classification debt instead of silently disappearing; and `Action` is
  visible in the preview panel even though self-regulatory actions are never co-speech, because
  previewing the animation is still useful.

### 5.1 Consumers

- **`core/src/main/java/de/dfki/vsm/model/behavior/`** — `BehaviorTaxonomy` (classpath-loaded,
  cached; `isCoSpeech()`, `neighboursOnAxis()`, `displayGroupOf()`, `uncoveredFunctionValues()`),
  `TaxonomyCategory` (`neighboursOf()`, `mainGroupOf()`, `axisIndexOf()`), `BehaviorChannel`
  (three-valued scope), `BehaviorTag` (the three kinds of "no value"), `BehaviorDisplayGroup`.
  Java 17 and Android-safe: `org.json` plus the JDK.
- **`WebUiServer.enrichCommandsWithTaxonomy`** annotates each command served in the plugin
  interface JSON with `neurogesFunction`, `neurogesType`, `cospeech`, and a derived `uiCategory` /
  `uiCategoryLabel` / `uiCategoryOrder` / `uiCategorySiaVisible`. Untagged commands pass through
  untouched, so an authored `uiCategory` in a non-behavior plugin is never destroyed.
- **`SiaPanel.svelte`** groups its columns by those derived fields. Its former hardcoded
  `SIA_VISIBLE_CATEGORIES` / `CATEGORY_LABELS` are gone: labels and column order now change by
  editing the taxonomy.

`BehaviorTaxonomyTest` (19 tests, runs under `./gradlew check`) is the guard. It pins the manual's
value sets and axis orders — including explicit assertions that the superseded 2009 values
(`continuous`, `stopped/holding`, `complementary`, `independent`) do not reappear and that Contact
runs `act on each other → act as a unit → act apart` rather than the reversed order published
prose gives. It cross-checks coverage against every declared plugin command in **both** directions,
validates Type-under-Function, asserts every Function value is reachable from some display group,
and asserts that no tag yet claims `video-coded` evidence — that last one is designed to fail when
the first Xenia clips land, so the docs get updated deliberately rather than drifting.

## 6. Where the phase structure already helps

`GesticonGesture` (`core/src/main/java/de/dfki/vsm/model/gesticon/GesticonGesture.java`) already
carries `preparation-end`, `stroke-phase-start`, `stroke-phase-end`, `retraction-start`,
`category`, `blendable`, `duration` — the same phase decomposition NEUROGES Module I uses
(Vorbereitungs-/Haupt-/Rückzugsphase, and the Module III *preparation / stroke / retraction*
types). This matters for placement: **the semantic affiliate aligns with the stroke, not with the
command's insertion point.** A gesture command placed at word *w* must start
`preparation-end` milliseconds *before* w for its stroke to land on w. The placement service
should therefore predict a *stroke target* and let the runtime offset the trigger.

Both example gesticons in the repo (`plugins/charamel-embed/ExampleProject/gesticon.xml`,
`doc/DesignPatterns/gesticon.xml`) are empty, so this machinery is currently unused. Incidental:
`GesticonGesture`'s constructor swaps `animName`/`animPath` (lines 41–42) — harmless today because
`parseXML` assigns them correctly, but it will bite anyone constructing gestures programmatically.

## 7. Calibration: what agreement to expect

The manual publishes per-value interrater reliability (EasyDIAg, Lausberg & Slöetjes 2016) for
trained raters. These are recorded in the taxonomy file; the ranges matter for how we set targets:

| Category | Range across values | Weakest / strongest values |
|---|---|---|
| Structure | 0.67 – 0.84 | aborted 0.67 ± 0.27 / irregular 0.84 ± 0.09 |
| Focus | 0.57 – 0.89 | on separate object 0.57 ± 0.23 / in space 0.89 ± 0.08 |
| Formal Relation | 0.68 – 0.79 | asymmetrical 0.68 ± 0.15 / symmetrical 0.79 ± 0.07 |
| **Function** | **0.43 – 0.79** | spatial relation presentation 0.43 ± 0.29, egocentric deictic 0.56 ± 0.41 / emblem 0.79 ± 0.06 |

(These supersede the 2011 modified-κ figures cited in the previous draft, which measured temporal
*and* categorical agreement and are therefore not comparable to a text-position task.)

Three implications for the multi-annotator agreement report (plan Phase 2.3):

- **Function is the weakest category, and it is the only one scene authors annotate.** Trained
  NEUROGES raters reach 0.43–0.79 on it. Our task is easier in one respect — no temporal
  segmentation, since placements are discrete positions in text — but there is no reason to expect
  untrained scene authors to beat trained raters on the category itself.
- **Agreement targets should be per-value, not global.** `emblem/social convention` (0.79) and
  `emphasis` (0.75) are reliable; `spatial relation presentation` (0.43) and `egocentric deictic`
  (0.56, SD 0.41 — the largest spread in the system) are not. A single κ threshold would either
  wave through the unreliable values or reject the reliable ones. Where our numbers land relative
  to *these* per-value figures is the meaningful comparison.
- **A reduced author vocabulary can be justified on reliability grounds**, not just usability. If
  the values authors would confuse are also the ones trained raters confuse, collapsing them costs
  little and is defensible to the NEUROGES side (§8.4).

## 8. Next steps

Done: §2.1 value inventories resolved from the manual; `behavior-taxonomy.json` authored with all
65 commands tagged and validated.

1. **Scope the repertoire coding effort.** 14 commands are unresolvable from names, 11 emotion
   presets have an undetermined channel, and 19 of 65 carry confidence ≤ 0.4 — so roughly 40 assets
   need video coding before any tag is NEUROGES-grade. Questions: what video of the
   VuppetMaster/Charamel repertoire exists, who codes it, and is a NEUROGES certificate required
   (the manual describes a certification path, ch. 1.10). Longest lead time of anything here.
2. **Decompose the composite commands** (`sittalk`, `sitnodd`, `sequence`) — they bundle a posture
   with a movement, or several gestures, and cannot be tagged as single units.
3. ~~**Feed the representational gap back to the character-engine side**~~ ✅ **done 2026-08-04** —
   20-clip request to Charamel for the new SIA Xenia, covering all five empty Function values.
   Figure-specific animation specs live in a separate repo and in German (Charamel is the audience):
   `~/Code/Repo/xenia-animation/animationsanfrage-xenia-20-clips.md`. General, figure-independent
   taxonomy documents — this file and `behavior-taxonomy.json` — stay here and in English.
4. **Decide the authoring vocabulary** (§3's target (b)): which Function/Type values scene authors
   see. A first reduction now exists as `displayGroups` (§5) and drives the SIA preview panel, but it
   was chosen for column legibility, not validity. §7 gives the reliability-based rationale for which
   collapses are defensible — worth a pass with Hedda before the same reduction is used for
   *authoring* rather than just display.
5. **Review the clinical-constraint candidates** flagged in the file: `protectassertive` and
   `demanding` as dominance cues, the crossed↔open rapport axis across the posture commands. This
   is where the ICMI paper's "specify behavioral constraints at the level of communicative
   function" becomes a concrete VSM feature.
6. **Confirm the two open judgement calls with Hedda** (§2.1): required vs optional categories when
   tagging a generation asset, and acceptability of a reduced author vocabulary. Neither blocks
   the work in progress.
