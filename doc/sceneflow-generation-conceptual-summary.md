# SceneFlow Generation: Conceptual Summary

## 1. Motivation

We moved from direct prompt-to-XML generation to a constrained and explainable generation pipeline.  
Goal: generate valid Visual SceneMaker SceneFlow models from natural-language situations while preserving runtime semantics and model validity.

Core driver:

- A generated flow must be structurally valid, semantically valid, and executable.
- Generation must be inspectable (why a pattern was selected, which assumptions were used, and what scientific rationale supports it).

## 2. Concept Shift

Initial approach was intent-specific (for example, "reminder intent").  
We generalized it to **constrained activity modeling**:

- **Constraint**: what must become true (for example: `event == "OkayButtonPressed"`).
- **Constrained activity**: what happens while constraint is false (wait, remind, multimodal behavior, social behavior).
- **Policy**: cadence and control of the constrained activity.
- **Completion**: transition behavior when constraint becomes true.

This is treated as an **interactive design pattern** abstraction.

## 3. Two-Layer Pattern Architecture

We established two explicit layers:

1. **Meta layer (abstract)**
- Describes interaction in terms of constraint/activity/policy/completion.

2. **Pattern layer (executable)**
- Realizes meta elements into SceneFlow operations (`create_supernode`, `create_node`, `create_edge`, `TEDGE`, `IEDGE`, `CEDGE`, etc.).

This separation enables deterministic compilation and easier validation.

## 4. Scientific Grounding

We introduced a scientific pattern catalog to justify modeling choices and guide future extensions:

- Statecharts and workflow patterns for hierarchical reactive flow composition.
- Interruption and mixed-initiative literature for policy design.
- Reminder/prospective memory and socially assistive robotics literature for constrained activities.

Scientific references are stored per pattern entry.

## 5. Implemented Artifacts

### 5.1 Main design documentation
- `/Users/gebhard/Code/Repo/VisualSceneMaker/doc/llm-supported-flow-generation.md`

### 5.2 Two-layer catalog with scientific sources
- `/Users/gebhard/Code/Repo/VisualSceneMaker/doc/interactive-design-pattern-catalog.json`
- Contains:
  - `metaModel`
  - `patternLibrary`
  - `humanDescription` per pattern
  - `scientificSources`

### 5.3 Meta-to-SceneFlow realization matrix
- `/Users/gebhard/Code/Repo/VisualSceneMaker/doc/meta-to-sceneflow-mapping.json`
- Machine-readable mapping from meta elements to SceneFlow concepts and invariants.
- Also contains semantic rule governance:
  - `ruleDefinitions` (id, scope, activation, severity)
  - `disabledRules`

## 6. Implemented Generation Pipeline Improvements

### 6.1 Meta-level representation in code
- `ConstrainedActivitySpec` added to template generation path:
  - `/Users/gebhard/Code/Repo/VisualSceneMaker/src/main/java/de/dfki/vsm/sceneflow/ir/SceneFlowIrTemplateLibrary.java`

### 6.2 Prompt resolution (deterministic)
- Prompt -> resolved meta fields (`activity kind`, `interruptibility`, interval, constraint label).

### 6.3 Data-driven pattern selection
- Selector now reads:
  - `/Users/gebhard/Code/Repo/VisualSceneMaker/doc/interactive-design-pattern-catalog.json`
- Selects implemented pattern by `supportsMeta`.
- Falls back to base implemented pattern when no implemented match exists.

### 6.4 Candidate metadata traceability
- Generated candidates now include:
  - selected pattern id
  - selection reason
  - resolved meta object

### 6.5 Pipeline report traceability
- `/Users/gebhard/Code/Repo/VisualSceneMaker/src/main/java/de/dfki/vsm/sceneflow/ir/SceneFlowSituationPipeline.java`
- Each attempt now includes `interactiveDesignPattern` with:
  - selected id
  - resolved meta
  - matched catalog entry
  - scientific sources

### 6.6 Output modes and generation target
- `patch` mode: apply IR operations onto an existing reference SceneFlow.
- `standalone` mode: generate a self-contained SceneFlow and a generated VSM project artifact.
- Report includes output mode, generated project path, and generation warnings.

### 6.7 Semantic rule engine architecture
- `/Users/gebhard/Code/Repo/VisualSceneMaker/src/main/java/de/dfki/vsm/sceneflow/ir/SceneFlowIrSemanticValidator.java`
- Validator uses:
  - operation-handler registry (instead of monolithic switch)
  - invariant-rule handler registry
  - mapping-driven rule activation (general/context/pattern scope)
- Rule execution metadata is exposed per attempt:
  - `activeSemanticRules` (`id`, `scope`, `severity`, `enabled`, `active`, `activationReason`)
  - `semanticRuleExecution` (`executed`, `violatedCount`, `firstViolationPath`, plus rule metadata)

### 6.8 Severity model and gating
- Semantic findings use explicit severity:
  - `error`: blocks acceptance
  - `warning`: reported but non-blocking
- Attempt report includes:
  - `semanticIssues[]` (`code`, `path`, `message`, `severity`)
  - `semanticErrorCount`
  - `semanticWarningCount`
- Current default policy in mapping:
  - `VAR_REF_UNKNOWN` is configured as `warning`.

## 7. SceneFlow Runtime-Semantic Realization

Key realization rules now made explicit:

- Waiting behavior needs an active internal flow (liveness).
- Reactive wait exit should use a valid edge target node id.
- For supernode semantics, avoid invalid cross-scope node-edge targets.
- Reminder-like behavior is modeled as an internal timed cycle, not as a one-shot condition.

## 8. Validation and Testing

Extended tests cover:

- constrained-activity template selection
- reminder mapping and interval parsing
- fallback behavior for planned-only kinds
- report-level pattern traceability and scientific source inclusion
- mapping/validator alignment for known rule ids and valid severities
- warning-vs-error gating behavior in semantic validation
- report presence of rule metadata fields (`severity`, `enabled`)

Relevant tests:

- `/Users/gebhard/Code/Repo/VisualSceneMaker/src/test/java/de/dfki/vsm/sceneflow/ir/SceneFlowIrTemplateLibraryTest.java`
- `/Users/gebhard/Code/Repo/VisualSceneMaker/src/test/java/de/dfki/vsm/sceneflow/ir/SceneFlowSituationPipelineTest.java`

## 9. Current Status

Implemented and working:

- Meta/pattern two-layer architecture
- Catalog-driven selector
- Scientific-source traceability in reports
- Human-readable + machine-readable conceptual artifacts
- Mapping-driven semantic rule configuration (`ruleDefinitions`, `disabledRules`, `severity`)
- Rule-level observability in generation reports (active/executed/violated + severity)
- Warning-capable semantic gating (non-blocking warnings, blocking errors)
- Prompt-resolution traceability with confidence + ambiguity reporting in candidate metadata and pipeline reports
- Reverse-explanation prototype (`sceneflow.xml` -> deterministic report with pattern evidence IDs)

Partially implemented / planned:

- Attention-aware interruptibility
- Mixed-initiative prompting policies
- Social/multimodal constrained activities beyond base fallback
- Broaden reverse explanation coverage beyond constrained-activity wait pattern (branching, retries, policy variants)

## 10. Recommended Next Steps

1. Make validator rules data-driven from `meta-to-sceneflow-mapping.json` to avoid rule drift.
2. Add executable realizations for currently planned pattern entries.
3. Add evaluation metrics per pattern (compile success, semantic pass rate, runtime behavior quality).
4. Build reverse explanation pipeline: parse -> pattern detection -> deterministic explanation templates -> optional LLM paraphrase.
5. Add optional policy profiles for severity (strict vs permissive) by context/pattern.
