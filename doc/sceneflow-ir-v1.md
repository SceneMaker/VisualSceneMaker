# SceneFlow IR v1

This document defines the first constrained intermediate representation for LLM-assisted flow generation.

- Schema file: `doc/sceneflow-ir.schema.json`
- Version: `irVersion = "1.0"`
- Primary mode: `patch` (mutate existing flow)

## Why This Exists

The IR is intentionally shorter and stricter than raw `sceneflow.xml`:

- easier for LLMs to produce reliably
- easier to validate and repair
- easier to map into existing model APIs

`XSD` remains the final structural guardrail after IR compilation.

## v1 Operation Set

### Graph structure

- `create_supernode`
- `create_node`
- `update_node`
- `delete_node`
- `create_edge`
- `update_edge`
- `delete_edge`

### Node content

- `add_node_command`
- `update_node_command`
- `delete_node_command`
- `add_variable_definition`
- `update_variable_definition`
- `delete_variable_definition`

## Edge Type Canonical Names

The IR uses short edge symbols used in the current editor/runtime bridge:

- `EEDGE` -> epsilon edge (`EEdge` / `EpsilonEdge`)
- `CEDGE` -> guarded/conditional edge (`CEdge` / `GuargedEdge`)
- `PEDGE` -> probabilistic edge (`PEdge` / `RandomEdge`)
- `TEDGE` -> timeout edge (`TEdge` / `TimeoutEdge`)
- `FEDGE` -> forking edge (`FEdge` / `ForkingEdge`)
- `IEDGE` -> interrupt edge (`IEdge` / `InterruptEdge`)

## Mapping to Current Core Model (Setter/Getter Anchors)

The compiler layer should map IR fields to existing model APIs.

### Supernode / node

- `create_supernode.parentSuperNodeId` -> parent `SuperNode` resolution via `getChildNodeById(...)`
- `create_supernode.superNodeId` -> `BasicNode.setId(...)`
- `create_supernode.name` -> `BasicNode.setName(...)`
- `create_supernode.comment` -> `BasicNode.setComment(...)`
- insertion under parent -> `SuperNode.addSuperNode(...)`

- `create_node.parentSuperNodeId` -> parent `SuperNode` resolution
- `create_node.nodeId` -> `BasicNode.setId(...)`
- `create_node.name` -> `BasicNode.setName(...)`
- `create_node.comment` -> `BasicNode.setComment(...)`
- insertion under parent -> `SuperNode.addNode(...)`

- `update_node` -> `BasicNode.setName(...)`, `BasicNode.setComment(...)`, `BasicNode.setHistoryNodeFlag(...)`
- `delete_node` -> `SuperNode.removeNode(...)` or `SuperNode.removeSuperNode(...)` depending on subtype

### Commands

- `add_node_command.commandText` -> parse to `Command`, append via `BasicNode.addCmd(...)`
- `update_node_command` -> `BasicNode.setCmdAt(...)`
- `delete_node_command` -> `BasicNode.removeCmdAt(...)`

### Variable definitions

- `add_variable_definition.varDef` -> create `VariableDefinition`, append via `BasicNode.addVarDef(...)`
- `update_variable_definition` -> `BasicNode.setVarDefAt(...)`
- `delete_variable_definition` -> `BasicNode.removeVarDefAt(...)`

### Edges (base)

- source/target IDs -> `AbstractEdge.setSourceUnid(...)`, `AbstractEdge.setTargetUnid(...)`
- resolved references -> `AbstractEdge.setSourceNode(...)`, `AbstractEdge.setTargetNode(...)`

### Edges (specialized payload)

- `TEDGE.payload.timeoutMs` -> `TimeoutEdge.setTimeout(...)`
- `TEDGE.payload.conditionText` (optional) -> parse to `Expression`, `TimeoutEdge.setExpression(...)`
- `CEDGE.payload.conditionText` -> parse to `Expression`, `GuargedEdge.setCondition(...)`
- `IEDGE.payload.conditionText` -> parse to `Expression`, `InterruptEdge.setCondition(...)`
- `PEDGE.payload.probability` -> `RandomEdge.setProbability(...)`

## Compatibility and Migration Policy

### Contract

- `irVersion` is mandatory.
- Unknown `irVersion` values must be rejected.
- Unknown operation names must be rejected.

### Evolution strategy

- Backward-compatible additions:
  - new optional fields in existing ops
  - new ops only when orchestrator supports capability negotiation
- Backward-incompatible changes:
  - bump major (`2.0`, etc.)
  - provide explicit migration transform (`v1 -> v2`) in compiler/orchestrator

### Recommended runtime behavior

1. Validate against `sceneflow-ir.schema.json`.
2. Validate semantic constraints against capability snapshot.
3. Compile to model.
4. Validate XML/model invariants.
5. Reject on first hard error with structured diagnostics.

## v1 Implementation Notes

- Keep compiler deterministic:
  - stable operation ordering
  - stable ID policy
  - stable XML output format
- Prefer `patch` mode first.
- Add `replace` mode only when full-flow generation is stable.

## Example

For a concrete v1 example, see:

- `doc/sceneflow-ir.wait-for-ok-button.example.json`

## Snapshot Generation Task (Phase 2 Bootstrap)

Use the repository Gradle task to generate capability snapshots from any directory containing `project.xml` and `sceneflow.xml`.

Command:

```bash
./gradlew generateCapabilitySnapshot \
  -PsnapshotProjectDir=/absolute/path/to/DesignPatterns \
  -PsnapshotOut=/absolute/path/to/output/capability-snapshot.json
```

Examples:

```bash
./gradlew generateCapabilitySnapshot \
  -PsnapshotProjectDir=/Users/gebhard/Code/Repo/VisualSceneMaker/doc/DesignPatterns \
  -PsnapshotOut=/Users/gebhard/Code/Repo/VisualSceneMaker/build/reports/capability-snapshot.designpatterns.generated.json
```

```bash
./gradlew generateCapabilitySnapshot \
  -PsnapshotProjectDir=/Users/gebhard/Code/Repo/VisualSceneMaker/android-stub/app/src/main/assets/SimpleProject \
  -PsnapshotOut=/Users/gebhard/Code/Repo/VisualSceneMaker/build/reports/capability-snapshot.androidstub.generated.json
```

You can also target larger projects outside the repository (for example under `/Users/gebhard/Code/Temp`) by providing an absolute `snapshotProjectDir`.

If no `snapshotProjectDir` is provided, the task defaults to:

- `/Users/gebhard/Code/Repo/VisualSceneMaker/doc/DesignPatterns`
