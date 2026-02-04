# Implementation Plan: Typed Bounded Event Variables

## Overview

Enhance Event variables from simple untyped FIFO queues to typed, bounded queues with value-matching semantics on edge conditions.

**New declaration syntax**: `Event event(10, Int)` — 10-slot queue of Int values.
**Default**: `Event event` resolves to `Event event(10, String)`.

**Edge condition semantics**:

| Expression      | Queue empty | Head matches | Head doesn't match |
|-----------------|-------------|--------------|---------------------|
| `event`         | `false`     | `true`, consume | `true`, consume  |
| `event == X`    | `false`     | `true`, consume | `false`           |
| `event != X`    | `false`     | `false`        | `true`, consume   |

**Assignment semantics**:
- `event = "first"` — enqueue value (type-checked against element type)
- `event = []` — clear the entire queue
- At capacity: drop oldest, log warning

---

## Step 1: Extend `VariableDefinition` model

**File**: `core/src/main/java/de/dfki/vsm/model/sceneflow/glue/command/definition/VariableDefinition.java`

Add two new optional fields:

```java
private int mCapacity = -1;        // -1 means "not an event variable"
private String mElementType = null; // e.g. "Int", "String", "Bool", "Float"
```

Add getters/setters: `getCapacity()`, `setCapacity()`, `getElementType()`, `setElementType()`.

Update `writeXML()` — only emit attributes when type is "Event":

```java
if ("Event".equalsIgnoreCase(mType)) {
    out.println("<VariableDefinition type=\"" + mType + "\" name=\"" + mName
        + "\" capacity=\"" + mCapacity + "\" elementType=\"" + mElementType + "\"/>");
} else {
    // existing writeXML logic
}
```

Update `parseXML()` — read optional attributes:

```java
String capStr = element.getAttribute("capacity");
if (capStr != null && !capStr.isEmpty()) {
    mCapacity = Integer.parseInt(capStr);
}
String elemType = element.getAttribute("elementType");
if (elemType != null && !elemType.isEmpty()) {
    mElementType = elemType;
}
```

Update `getCopy()` to copy the new fields.

Update syntax methods (`getConcreteSyntax`, `getFormattedSyntax`, `getAbstractSyntax`) — for Event type, show `Event name(capacity, elementType)` instead of `Event name = <exp>`.

---

## Step 2: Enhance `EventValue`

**File**: `core/src/main/java/de/dfki/vsm/runtime/interpreter/value/EventValue.java`

Add fields:

```java
private final int mCapacity;
private final AbstractValue.Type mElementType;
```

Add constructors:

```java
public EventValue() { this(10, Type.STRING); }  // default
public EventValue(int capacity, Type elementType) { ... }
```

Modify `enqueue(AbstractValue value)`:
- Type-check: `if (value.getType() != mElementType)` throw or reject with warning
- Capacity check: `if (mQueue.size() >= mCapacity)` → dequeue oldest, log warning via `LOGDefaultLogger`
- Then add to queue

Add `clear()` method for the `event = []` case.

Add getters: `getCapacity()`, `getElementType()`.

Update `getCopy()` to use the parameterized constructor.

Update `getFormattedSyntax()` / `getConcreteSyntax()` to show element type, e.g. `Event<Int>[3/10]`.

---

## Step 3: Update `SymbolEntry.write()` for queue clearing

**File**: `core/src/main/java/de/dfki/vsm/runtime/interpreter/symbol/SymbolEntry.java`

In the existing EVENT branch of `write(AbstractValue value)`, add handling for empty list:

```java
if (mValue.getType() == AbstractValue.Type.EVENT) {
    EventValue ev = (EventValue) mValue;
    // event = [] clears the queue
    if (value.getType() == AbstractValue.Type.LIST && ((ListValue) value).getValueList().isEmpty()) {
        ev.clear();
    } else {
        ev.enqueue(value);  // type checking and capacity enforcement inside EventValue
    }
    EventDispatcher.getInstance().convey(new VariableChangedEvent(this, ...));
    return mValue;
}
```

Import `ListValue` (already imported).

---

## Step 4: Update `Evaluator.define()`

**File**: `core/src/main/java/de/dfki/vsm/runtime/interpreter/Evaluator.java`

Read capacity and elementType from the VariableDefinition:

```java
if ("Event".equalsIgnoreCase(def.getType())) {
    int capacity = def.getCapacity() > 0 ? def.getCapacity() : 10;
    AbstractValue.Type elemType = mapElementType(def.getElementType());
    env.create(def.getName(), new EventValue(capacity, elemType));
}
```

Add helper `mapElementType(String)`:

```java
private AbstractValue.Type mapElementType(String typeStr) {
    if (typeStr == null || typeStr.isBlank()) return AbstractValue.Type.STRING;
    switch (typeStr.trim().toLowerCase()) {
        case "int":    return AbstractValue.Type.INT;
        case "float":  return AbstractValue.Type.FLOAT;
        case "bool":   return AbstractValue.Type.BOOLEAN;
        case "string": return AbstractValue.Type.STRING;
        default:       return AbstractValue.Type.STRING;
    }
}
```

---

## Step 5: Modify `Evaluator.evaluate()` — BinaryExpression for `==` / `!=`

**File**: `core/src/main/java/de/dfki/vsm/runtime/interpreter/Evaluator.java`

In the `BinaryExpression` branch, BEFORE the normal `left`/`right` evaluation (currently at line ~168), insert an early check for event variable comparisons:

```java
} else if (exp instanceof BinaryExpression) {
    final BinaryExpression bin = (BinaryExpression) exp;
    final BinaryExpression.BinaryOp operator = bin.getOperator();

    // Event variable comparison: event == X or event != X
    if ((operator == BinaryExpression.BinaryOp.Eq || operator == BinaryExpression.BinaryOp.Neq)
            && bin.getLeftExp() instanceof SimpleVariable) {
        AbstractValue leftRaw = env.read(((SimpleVariable) bin.getLeftExp()).getName());
        if (leftRaw instanceof EventValue) {
            EventValue ev = (EventValue) leftRaw;
            if (ev.isEmpty()) {
                return new BooleanValue(false);
            }
            AbstractValue right = evaluate(bin.getRightExp(), env);
            AbstractValue head = ev.peek();
            boolean matches = head.equalsValue(right);
            if (operator == BinaryExpression.BinaryOp.Eq) {
                if (matches) { ev.dequeue(); return new BooleanValue(true); }
                return new BooleanValue(false);
            } else { // Neq
                if (!matches) { ev.dequeue(); return new BooleanValue(true); }
                return new BooleanValue(false);
            }
        }
    }

    // Normal binary expression evaluation (existing code follows)
    final AbstractValue left = evaluate(bin.getLeftExp(), env);
    final AbstractValue right = evaluate(bin.getRightExp(), env);
    ...
```

The bare `event` case in the `SimpleVariable` branch (lines 399-411) stays unchanged — it continues to consume-any and return BooleanValue.

**Why this works**: The early check reads the raw EventValue from the environment directly (`env.read()`), bypassing the SimpleVariable evaluate branch which would convert it to BooleanValue. The right-hand side is evaluated normally. Only the Eq/Neq operators get this treatment.

---

## Step 6: Update `WebUiServer.parseVarDef()`

**File**: `core/src/main/java/de/dfki/vsm/web/WebUiServer.java`

In the Event early-return block, read capacity and elementType from JSON:

```java
if ("Event".equalsIgnoreCase(type)) {
    int capacity = source.optInt("capacity", 10);
    String elementType = source.optString("elementType", "String");
    VariableDefinition def = new VariableDefinition(name, type, null);
    def.setCapacity(capacity);
    def.setElementType(elementType);
    return def;
}
```

---

## Step 7: Update `WebUiServer` variable display

**File**: `core/src/main/java/de/dfki/vsm/web/WebUiServer.java`

In `resolveVariableValue()`, the existing EVENT handling already shows `Event[N]`. Update to show element type:

```java
if (value.getType() == AbstractValue.Type.EVENT) {
    EventValue ev = (EventValue) value;
    return "Event<" + ev.getElementTypeName() + ">[" + ev.size() + "/" + ev.getCapacity() + "]";
}
```

In `resolveTypeFlavor()`, "Event" is already handled.

---

## Step 8: Update `SceneFlowSnapshotBuilder.varDefsToJson()`

**File**: `core/src/main/java/de/dfki/vsm/web/SceneFlowSnapshotBuilder.java`

For Event type definitions, include capacity and elementType in the JSON:

```java
if ("Event".equalsIgnoreCase(def.getType())) {
    json.put("capacity", def.getCapacity() > 0 ? def.getCapacity() : 10);
    json.put("elementType", def.getElementType() != null ? def.getElementType() : "String");
}
```

This lets the Web UI read back the parameters when editing an existing Event variable.

---

## Step 9: Update Web UI dialog

**File**: `editor/web-ui/src/App.svelte`

### 9a. Extend `varDefDraft` state

Add fields to the draft object:

```javascript
function defaultVarDefDraft() {
    const preferred = nodeEditorTypeOptions.includes("Bool") ? "Bool" : nodeEditorTypeOptions[0] || "Bool";
    return { name: "", type: preferred, expression: "", capacity: 10, elementType: "String" };
}
```

In `startVarDefEdit()`, populate capacity/elementType from the existing definition.

### 9b. Update dialog template

Replace the current Event hint paragraph with capacity and element type controls:

```svelte
{#if varDefDraft.type === "Event"}
    <label for="var-def-elem-type">Element type</label>
    <select id="var-def-elem-type" bind:value={varDefDraft.elementType}>
        <option value="String">String</option>
        <option value="Int">Int</option>
        <option value="Float">Float</option>
        <option value="Bool">Bool</option>
    </select>
    <label for="var-def-capacity">Queue capacity</label>
    <input id="var-def-capacity" type="number" min="1" max="1000"
           bind:value={varDefDraft.capacity} />
{:else}
    <label for="var-def-exp">Expression</label>
    <input id="var-def-exp" ... />
{/if}
```

### 9c. Include in WebSocket payload

In `applyVarDefEdit()`, add capacity/elementType to the payload when type is Event:

```javascript
const payload = { name, type, expression: varDefDraft.expression ?? "" };
if (type === "Event") {
    payload.capacity = varDefDraft.capacity || 10;
    payload.elementType = varDefDraft.elementType || "String";
}
```

---

## Step 10: Build and test

1. `./gradlew :core:build` — core compiles
2. `cd editor/web-ui && npm run build` — Web UI builds
3. `./gradlew build` — full project compiles
4. `./gradlew test` — existing tests pass

---

## Files Summary

| File | Change |
|---|---|
| `core/.../definition/VariableDefinition.java` | Add `mCapacity`, `mElementType` fields; update XML, copy, syntax |
| `core/.../value/EventValue.java` | Add capacity, element type; bounded enqueue; type checking; `clear()` |
| `core/.../symbol/SymbolEntry.java` | Handle `event = []` clearing in EVENT write branch |
| `core/.../interpreter/Evaluator.java` | `define()`: pass capacity/elementType; `evaluate()`: BinaryExpr event check |
| `core/.../web/WebUiServer.java` | `parseVarDef()`: read capacity/elementType; display: show params |
| `core/.../web/SceneFlowSnapshotBuilder.java` | Include capacity/elementType in varDef JSON |
| `editor/web-ui/src/App.svelte` | Dialog: capacity spinner, element type dropdown; draft/payload updates |
