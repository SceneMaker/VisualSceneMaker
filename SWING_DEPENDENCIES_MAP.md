# Swing Dependencies Map - WebUiServer.java Refactoring Plan

**Date:** 2026-01-13
**File:** `editor/src/main/java/de/dfki/vsm/web/WebUiServer.java`
**Total Lines:** 8048
**Goal:** Replace all Swing dependencies with headless services

---

## Dependency Overview

| Category | Component | Occurrences | Priority | Estimated Time |
|----------|-----------|-------------|----------|----------------|
| **1. Core** | EditorInstance | 72 | HIGH | 2-3 hours |
| **2. Core** | ProjectEditor | 93 | HIGH | 1-2 hours |
| **3. Core** | WorkSpacePanel | 36 | MEDIUM | 1-2 hours |
| **4. Actions** | Create/Remove/Undo Actions | 12 | MEDIUM | 2-3 hours |
| **5. Swing UI** | JTabbedPane | 7 | LOW | 1 hour |
| **6. Swing UI** | SwingUtilities | 0 (import only) | LOW | 5 min |
| **7. Swing UI** | UndoManager | 0 (import only) | LOW | 5 min |
| **8. Config** | PreferencesDesktop | 8 | LOW | 30 min |
| **Total** | | **228** | | **8-12 hours** |

---

## 1. EditorInstance Dependencies (72 occurrences) - PRIORITY 1

### Current Usage Pattern

```java
EditorInstance instance = EditorInstance.getInstance();
ProjectEditor editor = instance.getSelectedProjectEditor();
EditorProject project = editor.getProject();
```

### Key Methods Using EditorInstance

**Line ranges with EditorInstance.getInstance():**
- Lines 474, 496, 516, 540, 578 (REST endpoints)
- Lines 736, 821, 852, 889, 937, 975, 996, 1016, 1045, 1089, 1117, 1140, 1158, 1175, 1192 (WebSocket handlers)

### Common Patterns

1. **Get Selected Project:**
   ```java
   // Current (Swing)
   EditorInstance instance = EditorInstance.getInstance();
   ProjectEditor editor = instance.getSelectedProjectEditor();
   EditorProject project = editor.getProject();

   // Replace with (Headless)
   private Map<String, EditorProject> mOpenProjects = new HashMap<>();
   private String mCurrentProjectId;
   EditorProject project = mOpenProjects.get(mCurrentProjectId);
   ```

2. **Get Project Tabs (for iteration):**
   ```java
   // Current (Swing)
   JTabbedPane tabs = instance.getProjectEditors();
   for (int i = 0; i < tabs.getTabCount(); i++) {
       ProjectEditor editor = (ProjectEditor) tabs.getComponentAt(i);
   }

   // Replace with (Headless)
   for (EditorProject project : mOpenProjects.values()) {
       // Process project
   }
   ```

3. **Open/Close Project:**
   ```java
   // Current (Swing)
   EditorInstance.getInstance().newProject(path);

   // Replace with (Headless)
   private EditorProjectService mEditorService;
   EditorProject project = mEditorService.createProject(name);
   String projectId = UUID.randomUUID().toString();
   mOpenProjects.put(projectId, project);
   ```

### Replacement Strategy

**Phase 1a: Add Service Fields**
```java
public final class WebUiServer implements UiEventListener {
    // Add these fields
    private EditorProjectService mEditorProjectService;
    private Map<String, EditorProject> mOpenProjects;
    private String mCurrentProjectId;

    public void start() {
        mEditorProjectService = new EditorProjectService();
        mOpenProjects = new ConcurrentHashMap<>();
        // ... existing code
    }
}
```

**Phase 1b: Replace Usage (one method at a time)**

Replace in this order (least risky first):
1. REST endpoints (lines 474-578)
2. Read-only WebSocket handlers (lines 736-1192)
3. Write WebSocket handlers (later)

---

## 2. ProjectEditor Dependencies (93 occurrences) - PRIORITY 2

### Current Usage Pattern

```java
ProjectEditor editor = instance.getSelectedProjectEditor();
EditorProject project = editor.getProject();
WorkSpacePanel workSpace = editor.getSceneFlowEditor().getWorkSpace();
editor.getSceneFlowEditor().refresh();
```

### Key Usage Locations

**Lines with getSelectedProjectEditor():**
- 826, 857, 1570, 1596 (WebSocket handlers)

### Common Patterns

1. **Get Project from Editor:**
   ```java
   // Current (Swing)
   ProjectEditor editor = instance.getSelectedProjectEditor();
   EditorProject project = editor.getProject();

   // Replace with (Headless)
   EditorProject project = mOpenProjects.get(mCurrentProjectId);
   ```

2. **Refresh UI after changes:**
   ```java
   // Current (Swing)
   editor.getSceneFlowEditor().refresh();

   // Replace with (Headless)
   broadcastSceneflowSnapshot(projectId);
   ```

3. **Get WorkSpace for actions:**
   ```java
   // Current (Swing)
   WorkSpacePanel workSpace = editor.getSceneFlowEditor().getWorkSpace();

   // Replace with (Headless)
   // Pass project directly to service methods
   ```

### Replacement Strategy

**Replace after EditorInstance removal** (dependency: Phase 1 complete)

1. Replace `editor.getProject()` with direct project access
2. Replace `editor.getSceneFlowEditor().refresh()` with WebSocket broadcasts
3. Remove intermediate `editor` variable entirely

---

## 3. WorkSpacePanel Dependencies (36 occurrences) - PRIORITY 3

### Current Usage Pattern

```java
WorkSpacePanel workSpace = editor.getSceneFlowEditor().getWorkSpace();
CreateNodeAction action = new CreateNodeAction(workSpace, node);
action.run();
```

### Key Usage Locations

**Lines with getWorkSpace():**
- 1061, 1570, 1596, 1781, 1820 (action invocations)

### Common Pattern

WorkSpacePanel is ONLY used as a parameter to Swing Action constructors.

### Replacement Strategy

**Replace with SceneFlowService** (dependency: Phase 2 complete)

```java
// Current (Swing)
WorkSpacePanel workSpace = editor.getSceneFlowEditor().getWorkSpace();
CreateNodeAction action = new CreateNodeAction(workSpace, dataNode);
action.run();

// Replace with (Headless)
private SceneFlowService mSceneFlowService;
BasicNode createdNode = mSceneFlowService.createNode(
    project,
    parentNode,
    nodeType,
    position,
    nodeName
);
```

---

## 4. Swing Action Dependencies (12 occurrences) - PRIORITY 4

### Actions to Replace

| Action Class | Occurrences | Lines | Replacement Service Method |
|--------------|-------------|-------|----------------------------|
| CreateNodeAction | 2 | 1844, 2405 | SceneFlowService.createNode() |
| RemoveNodeAction | 1 | 2159 | SceneFlowService.deleteNode() |
| CreateEdgeAction | 7 | 2463, 2472, 2481, 2490, 2500, 3783, 4353 | SceneFlowService.createEdge() |
| RemoveEdgeAction | 1 | 4353 | SceneFlowService.deleteEdge() |
| CreateCommentAction | 1 | 3509 | SceneFlowService.createComment() |
| RemoveCommentAction | 1 | 3686 | SceneFlowService.deleteComment() |
| UndoAction | 0 | (import only) | Implement undo stack in service |
| RedoAction | 0 | (import only) | Implement undo stack in service |

### Example Replacements

**CreateNodeAction (Line 1844):**
```java
// Current
CreateNodeAction action = new CreateNodeAction(workSpace, dataNode);
action.run();

// Replace with
BasicNode createdNode = mSceneFlowService.createNode(
    project,
    parentNode,
    nodeType,
    new Point(params.getInt("x"), params.getInt("y")),
    params.optString("name", "NewNode")
);
```

**CreateEdgeAction (Line 2463):**
```java
// Current
new CreateEdgeAction(workSpace, source, target, edge, Edge.TYPE.CEDGE).run();

// Replace with
AbstractEdge createdEdge = mSceneFlowService.createEdge(
    project,
    Edge.TYPE.CEDGE,
    sourceId,
    targetId,
    edgeData
);
```

**RemoveNodeAction (Line 2159):**
```java
// Current
new RemoveNodeAction(workSpace, node).run();

// Replace with
boolean deleted = mSceneFlowService.deleteNode(project, nodeId);
```

### Replacement Strategy

**Replace AFTER WorkSpacePanel removal** (dependency: Phase 3 complete)

For each action:
1. Create corresponding SceneFlowService method
2. Extract business logic from action class
3. Replace action instantiation with service call
4. Broadcast WebSocket update
5. Test thoroughly

---

## 5. JTabbedPane Dependencies (7 occurrences) - PRIORITY 5

### Usage Locations

**Lines with JTabbedPane:**
- 110 (import)
- 737, 827, 871, 898, 943, 1001, 1571 (usage)

### Current Usage Pattern

```java
JTabbedPane tabs = instance.getProjectEditors();
for (int i = 0; i < tabs.getTabCount(); i++) {
    ProjectEditor editor = (ProjectEditor) tabs.getComponentAt(i);
    EditorProject project = editor.getProject();
    // Process project
}
```

### Replacement Strategy

```java
// Replace with
for (EditorProject project : mOpenProjects.values()) {
    // Process project
}
```

**Replace AFTER EditorInstance removal** (dependency: Phase 1 complete)

---

## 6. SwingUtilities Dependencies (0 occurrences)

**Line 111:** `import javax.swing.SwingUtilities;`

**Status:** Import only, not used in code.

**Action:** Delete import after all other Swing dependencies removed.

---

## 7. UndoManager Dependencies (0 occurrences)

**Line 115:** `import javax.swing.undo.UndoManager;`

**Status:** Import only, not used in code.

**Action:** Delete import after all other Swing dependencies removed.

---

## 8. PreferencesDesktop Dependencies (8 occurrences) - PRIORITY 6

### Usage Locations

**Lines with PreferencesDesktop:**
- 3 (import)
- 605, 606 (tutorials/samples paths)
- 753-760 (recent projects)

### Current Usage Pattern

```java
String samplesPath = PreferencesDesktop.sSAMPLE_PROJECTS;
String tutorialsPath = PreferencesDesktop.sTUTORIALS_PROJECTS;
int maxRecent = PreferencesDesktop.sMAX_RECENT_PROJECTS;
String prop = PreferencesDesktop.getProperty("key");
```

### Replacement Strategy

**Option 1: Keep PreferencesDesktop (Low Risk)**
- PreferencesDesktop has minimal AWT dependencies (just java.awt.Dimension)
- Works fine in headless mode
- No immediate need to replace

**Option 2: Replace with Preferences (Clean Architecture)**
```java
import de.dfki.vsm.Preferences;

String samplesPath = Preferences.sSAMPLE_PROJECTS;
String tutorialsPath = Preferences.sTUTORIALS_PROJECTS;
```

**Recommendation:** Replace in final cleanup phase (Priority 6).

---

## Refactoring Phases Summary

### Phase 1: Replace EditorInstance (2-3 hours)
**Files to modify:**
- `WebUiServer.java` (72 replacements)

**Files to create:**
- `EditorProjectService.java`

**Key changes:**
- Add `mEditorProjectService`, `mOpenProjects`, `mCurrentProjectId` fields
- Replace `EditorInstance.getInstance()` with project map access
- Update project open/close/save logic

**Test after:** All REST endpoints work, WebSocket project access works

---

### Phase 2: Replace ProjectEditor (1-2 hours)
**Files to modify:**
- `WebUiServer.java` (93 replacements)

**Key changes:**
- Replace `editor.getProject()` with `mOpenProjects.get(projectId)`
- Replace `editor.getSceneFlowEditor().refresh()` with WebSocket broadcasts
- Remove all `ProjectEditor` variables

**Test after:** Project loading works, UI refreshes via WebSocket

---

### Phase 3: Replace WorkSpacePanel (1-2 hours)
**Files to modify:**
- `WebUiServer.java` (36 replacements)

**Key changes:**
- Replace all `getWorkSpace()` calls with direct project access
- Pass projects to service methods instead of workspace panels

**Test after:** SceneFlow rendering still works

---

### Phase 4: Replace Swing Actions (2-3 hours)
**Files to modify:**
- `WebUiServer.java` (12 replacements)

**Files to create:**
- `SceneFlowService.java` (with methods: createNode, deleteNode, createEdge, deleteEdge, createComment, deleteComment)

**Key changes:**
- Extract business logic from action classes into services
- Replace action instantiations with service calls
- Add WebSocket broadcasts after mutations

**Test after:** Node/edge/comment creation/deletion works, undo/redo works (if implemented)

---

### Phase 5: Clean Up Swing UI Components (1 hour)
**Files to modify:**
- `WebUiServer.java` (7 JTabbedPane replacements)

**Key changes:**
- Replace JTabbedPane iteration with map iteration
- Remove SwingUtilities import
- Remove UndoManager import

**Test after:** Full Web UI functionality works

---

### Phase 6: Replace PreferencesDesktop (30 min)
**Files to modify:**
- `WebUiServer.java` (8 replacements)

**Key changes:**
- Replace PreferencesDesktop with Preferences
- Or keep if acceptable (minimal dependencies)

**Test after:** Tutorials, samples, recent projects work

---

## Import Changes

### Remove These Imports (17 total)

```java
import de.dfki.vsm.editor.CmdBadge;              // Line 4
import de.dfki.vsm.editor.Comment;               // Line 5
import de.dfki.vsm.editor.Edge;                  // Line 6
import de.dfki.vsm.editor.EditorInstance;        // Line 7
import de.dfki.vsm.editor.Node;                  // Line 8
import de.dfki.vsm.editor.action.CreateCommentAction;    // Line 9
import de.dfki.vsm.editor.action.CreateEdgeAction;       // Line 10
import de.dfki.vsm.editor.action.CreateNodeAction;       // Line 11
import de.dfki.vsm.editor.action.RedoAction;             // Line 12
import de.dfki.vsm.editor.action.RemoveCommentAction;    // Line 13
import de.dfki.vsm.editor.action.RemoveEdgeAction;       // Line 14
import de.dfki.vsm.editor.action.RemoveNodeAction;       // Line 15
import de.dfki.vsm.editor.action.UndoAction;             // Line 16
import de.dfki.vsm.editor.project.ProjectEditor;         // Line 18
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;  // Line 19
import javax.swing.JTabbedPane;                  // Line 110
import javax.swing.SwingUtilities;               // Line 111
import javax.swing.undo.UndoManager;             // Line 115
```

### Keep These Imports (Important!)

```java
import de.dfki.vsm.editor.project.EditorProject;        // Line 17 - KEEP
import de.dfki.vsm.editor.util.SceneFlowManager;        // Line 20 - KEEP
```

### Add These Imports

```java
import de.dfki.vsm.editor.service.EditorProjectService;
import de.dfki.vsm.editor.service.SceneFlowService;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
```

---

## Testing Strategy

### After Each Phase

1. **Compile:**
   ```bash
   ./gradlew :editor:compileJava -PskipWebUi=true
   ```

2. **Run:**
   ```bash
   ./gradlew :editor:run
   ```

3. **Test Web UI:**
   - Landing page loads
   - Connection works
   - Tutorials/samples visible
   - Recent projects visible
   - Can open project
   - SceneFlow renders correctly
   - AGENTS panel populated
   - SCENES list populated
   - Can create/edit/delete nodes/edges
   - Inspector panel works
   - Can save project

4. **Commit if successful:**
   ```bash
   git add editor/src/main/java/de/dfki/vsm/web/WebUiServer.java
   git commit -m "Phase 6.X: [Description]

   - [What changed]
   - [Test results]

   All tests passed - Web UI still works."
   ```

---

## Risk Assessment

| Phase | Risk | Mitigation |
|-------|------|------------|
| Phase 1 | HIGH - Core functionality | Test after EACH method, commit frequently |
| Phase 2 | MEDIUM - Depends on Phase 1 | Ensure Phase 1 100% working first |
| Phase 3 | MEDIUM - Affects rendering | Visual testing critical |
| Phase 4 | HIGH - Editing functionality | Test all CRUD operations |
| Phase 5 | LOW - Mostly cleanup | Minimal functional impact |
| Phase 6 | LOW - Config only | Easy rollback |

---

## Success Criteria

**Phase Complete When:**
- ✅ Zero imports from `de.dfki.vsm.editor.*` (except EditorProject, util, services)
- ✅ Zero imports from `javax.swing.*`
- ✅ All 228 Swing dependency usages replaced
- ✅ Web UI 100% functional
- ✅ All features work (open, save, edit, runtime control)
- ✅ Comprehensive test pass
- ✅ Ready to rename to UnifiedWebUiServer

---

## Next Immediate Action

**Start Phase 1a:**
1. Create `EditorProjectService.java`
2. Add service fields to WebUiServer.java
3. Test compilation
4. Commit

**Expected time:** 30 minutes
**Expected result:** Clean compilation, ready for Phase 1b

---

**Status:** 🟢 READY TO START
**Total Estimated Time:** 8-12 hours over 2-3 sessions
**Current Session Remaining:** ~22% tokens (~45000 tokens)
**Can accomplish today:** Phase 1a + partial Phase 1b
