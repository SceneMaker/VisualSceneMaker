# Quick Reference - Refactoring WebUiServer.java

**Date:** 2026-01-13
**Goal:** Remove Swing dependencies from working WebUiServer.java incrementally

---

## Swing Dependencies to Replace

### 1. EditorInstance → EditorProjectService
**Pattern:**
```java
// OLD (Swing-coupled)
EditorProject project = EditorInstance.getInstance().getSelectedProjectEditor().getProject();

// NEW (Headless)
private EditorProjectService mEditorProjectService = new EditorProjectService();
private Map<String, EditorProject> mOpenProjects = new HashMap<>();
EditorProject project = mOpenProjects.get(projectId);
```

**Occurrences:** 72 usages to replace

---

### 2. ProjectEditor → Direct Project Access
**Pattern:**
```java
// OLD (Swing-coupled)
ProjectEditor editor = EditorInstance.getInstance().getSelectedProjectEditor();
editor.getSceneFlowEditor().refresh();

// NEW (Headless)
EditorProject project = mOpenProjects.get(projectId);
// Broadcast WebSocket update instead of Swing refresh
broadcastSceneflowUpdate(projectId);
```

**Occurrences:** 93 usages to replace

---

### 3. WorkSpacePanel → SceneFlowService
**Pattern:**
```java
// OLD (Swing-coupled)
WorkSpacePanel workspace = editor.getSceneFlowEditor().getSceneFlowManager().getWorkSpace();
workspace.doSomething();

// NEW (Headless)
private SceneFlowService mSceneFlowService = new SceneFlowService();
// Use service methods
```

**Occurrences:** 36 usages to replace

---

### 4. Swing Actions → Service Methods
**Pattern:**
```java
// OLD (Swing-coupled)
import de.dfki.vsm.editor.action.CreateNodeAction;
CreateNodeAction action = new CreateNodeAction(...);
action.run();

// NEW (Headless)
private SceneFlowService mSceneFlowService = new SceneFlowService();
BasicNode node = mSceneFlowService.createNode(project, parentId, nodeType, position, name);
```

**Actions to replace:**
- CreateNodeAction
- CreateEdgeAction
- CreateCommentAction
- RemoveNodeAction
- RemoveEdgeAction
- RemoveCommentAction
- UndoAction
- RedoAction

**Occurrences:** 9 action instantiations to replace

---

## Files Overview

### Primary File to Refactor
- `editor/src/main/java/de/dfki/vsm/web/WebUiServer.java` (8048 lines)

### Service Classes to Create/Use
- `editor/src/main/java/de/dfki/vsm/editor/service/EditorProjectService.java`
- `editor/src/main/java/de/dfki/vsm/editor/service/SceneFlowService.java`

### Utilities to Preserve
- `editor/src/main/java/de/dfki/vsm/editor/util/SceneFlowManager.java`

---

## Workflow

### 1. Make ONE Change
Pick a single dependency type (e.g., all EditorInstance calls in one WebSocket method)

### 2. Test
```bash
./gradlew :editor:compileJava -PskipWebUi=true
./gradlew :editor:run
# Open Web UI and test functionality
```

### 3. Commit if Works
```bash
git add editor/src/main/java/de/dfki/vsm/web/WebUiServer.java
git commit -m "Phase 6: Replace EditorInstance in [method name]

- Removed EditorInstance.getInstance() calls
- Added project tracking map
- All tests passed - Web UI still works"
```

### 4. Repeat
Move to next dependency or next method

---

## Test Checklist

After each change, verify:

**Compilation:**
- ✅ `./gradlew :editor:compileJava -PskipWebUi=true` succeeds

**Server Start:**
- ✅ Server starts without errors
- ✅ Token displayed
- ✅ No ClassNotFoundException

**Web UI Landing Page:**
- ✅ Logo visible
- ✅ Connection badge shows "connected"
- ✅ Tutorials list populated
- ✅ Recent projects visible

**Project Loading:**
- ✅ Click project, editor view appears
- ✅ SceneFlow canvas visible
- ✅ AGENTS panel populated
- ✅ SCENES list populated

**Visual Rendering:**
- ✅ Nodes visible at correct sizes
- ✅ Edges visible with labels
- ✅ Variables displayed on supernodes
- ✅ Inspector panel functional

---

## Rollback Strategy

If anything breaks:

```bash
# Undo last commit (keep changes for editing)
git reset --soft HEAD~1

# Or discard changes entirely
git reset --hard HEAD~1
```

---

## Import Changes Summary

**Remove these imports:**
```java
import de.dfki.vsm.editor.CmdBadge;
import de.dfki.vsm.editor.Comment;
import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.action.CreateCommentAction;
import de.dfki.vsm.editor.action.CreateEdgeAction;
import de.dfki.vsm.editor.action.CreateNodeAction;
import de.dfki.vsm.editor.action.RedoAction;
import de.dfki.vsm.editor.action.RemoveCommentAction;
import de.dfki.vsm.editor.action.RemoveEdgeAction;
import de.dfki.vsm.editor.action.RemoveNodeAction;
import de.dfki.vsm.editor.action.UndoAction;
import de.dfki.vsm.editor.project.ProjectEditor;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
```

**Add these imports:**
```java
import de.dfki.vsm.editor.service.EditorProjectService;
import de.dfki.vsm.editor.service.SceneFlowService;
import java.util.HashMap;
import java.util.Map;
```

---

## Estimated Timeline

| Phase | Dependency | Occurrences | Time |
|-------|-----------|-------------|------|
| 1 | EditorInstance | 72 | 2-3 hours |
| 2 | ProjectEditor | 93 | 1-2 hours |
| 3 | WorkSpacePanel | 36 | 1-2 hours |
| 4 | Swing Actions | 9 | 2-3 hours |
| 5 | Cleanup | - | 1 hour |
| **Total** | | **210** | **7-11 hours** |

---

## Success Criteria

**When complete:**
- ✅ Zero imports from `de.dfki.vsm.editor.*` (except EditorProject, util classes, services)
- ✅ Web UI fully functional
- ✅ All features work (load, save, edit, runtime)
- ✅ Ready to rename to UnifiedWebUiServer

---

**Current Status:** Ready to start Phase 1 (Replace EditorInstance)

**Next Action:** Create EditorProjectService, add project tracking, replace first EditorInstance usage
