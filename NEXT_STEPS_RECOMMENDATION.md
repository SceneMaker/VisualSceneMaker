# Next Steps Recommendation - Phase 6 Completion

**Date:** 2026-01-13
**Status:** 🟢 CLEAR PATH FORWARD

---

## Summary of Investigation

**Root Cause Identified:** At commit `3fe491fb` (working state), the system uses:
- **Active Server:** `editor/src/main/java/de/dfki/vsm/web/WebUiServer.java` (374KB, Swing-coupled) ✅ WORKING
- **Inactive Server:** `editor/src/main/java/de/dfki/vsm/web/UnifiedWebUiServer.java` (1227 lines, incomplete) ❌ INCOMPLETE

**Proof:** `SceneMaker4.java` line 4 imports `de.dfki.vsm.web.WebUiServer` (the Swing one).

**Why UnifiedWebUiServer Cannot Work:**
- Missing 11/17 WebSocket methods (most critically: `SceneFlow.Get`)
- Missing 10+ REST endpoints (`/api/v1/token`, `/api/v1/projects/tutorials`, etc.)
- Minimal serialization (no position, size, variables, commands)

See `CRITICAL_ANALYSIS.md` for full details.

---

## Recommended Approach

### REFACTOR SWING WebUiServer (Not Replace It)

**Don't delete `WebUiServer.java` - REFACTOR it incrementally.**

---

## Step-by-Step Plan

### Phase 1: Analyze Swing Dependencies (30 minutes)

**Task:** Map all Swing dependencies in WebUiServer.java

```bash
cd editor/src/main/java/de/dfki/vsm/web

# Find Swing imports
grep "^import.*editor\." WebUiServer.java | sort > swing_imports.txt

# Count dependencies
grep -c "EditorInstance.getInstance()" WebUiServer.java
grep -c "ProjectEditor" WebUiServer.java
grep -c "WorkSpacePanel" WebUiServer.java
```

**Expected Output:**
- List of Swing classes used (EditorInstance, ProjectEditor, WorkSpacePanel, etc.)
- Count of each dependency
- Priority order for replacement

**Deliverable:** `SWING_DEPENDENCIES_MAP.md`

---

### Phase 2: Replace EditorInstance Calls (2-3 hours)

**Task:** Replace all `EditorInstance.getInstance()` calls with `EditorProjectService`

**Current pattern:**
```java
// In WebUiServer.java
EditorProject project = EditorInstance.getInstance().getSelectedProjectEditor().getProject();
```

**Replace with:**
```java
// Add field to WebUiServer.java
private EditorProjectService mEditorProjectService = new EditorProjectService();
private Map<String, EditorProject> mOpenProjects = new HashMap<>();

// Replace usage
EditorProject project = mOpenProjects.get(projectId);
```

**Test after EACH replacement:**
```bash
./gradlew :editor:compileJava -PskipWebUi=true
./gradlew :editor:run
# Test Web UI
```

**Commit when working:**
```bash
git add editor/src/main/java/de/dfki/vsm/web/WebUiServer.java
git commit -m "Phase 6: Replace EditorInstance with EditorProjectService

- Removed EditorInstance.getInstance() calls
- Added mEditorProjectService field
- Added mOpenProjects map for project tracking

All tests passed - Web UI still works."
```

---

### Phase 3: Replace ProjectEditor Calls (1-2 hours)

**Task:** Replace `ProjectEditor` dependencies

**Current pattern:**
```java
ProjectEditor editor = EditorInstance.getInstance().getSelectedProjectEditor();
editor.getSceneFlowEditor().refresh();
```

**Replace with:**
```java
// Direct project manipulation
EditorProject project = mOpenProjects.get(projectId);
// Broadcast WebSocket update instead of Swing refresh
broadcastSceneflowUpdate(projectId);
```

**Test and commit as before.**

---

### Phase 4: Replace WorkSpacePanel/Swing Actions (2-3 hours)

**Task:** Replace Swing editing actions with SceneFlowService

**Current pattern:**
```java
import de.dfki.vsm.editor.action.CreateNodeAction;
import de.dfki.vsm.editor.action.CreateEdgeAction;

// WebSocket handler uses Swing action
CreateNodeAction action = new CreateNodeAction(...);
action.run();
```

**Replace with:**
```java
// Add field
private SceneFlowService mSceneFlowService = new SceneFlowService();

// WebSocket handler uses service
BasicNode node = mSceneFlowService.createNode(project, parentId, nodeType, position, name);
```

**Test and commit as before.**

---

### Phase 5: Remove Remaining Swing Dependencies (1-2 hours)

**Task:** Replace any remaining Swing references

- `PreferencesDesktop` → `Preferences`
- Swing dialogs → Return errors in JSON
- Event dispatching → WebSocket broadcasts

**Test and commit as before.**

---

### Phase 6: Rename and Clean Up (30 minutes)

**Task:** Once Swing-free, rename to UnifiedWebUiServer

```bash
# Rename file
mv editor/src/main/java/de/dfki/vsm/web/WebUiServer.java \
   editor/src/main/java/de/dfki/vsm/web/RefactoredWebUiServer.java

# Update imports
sed -i 's/import de.dfki.vsm.web.WebUiServer;/import de.dfki.vsm.web.RefactoredWebUiServer;/g' \
    src/main/java/de/dfki/vsm/SceneMaker4.java

# Test
./gradlew :editor:run
```

**Then delete old UnifiedWebUiServer:**
```bash
rm editor/src/main/java/de/dfki/vsm/web/UnifiedWebUiServer.java
```

**Final commit:**
```bash
git add .
git commit -m "Phase 6 Complete: Swing-free WebUiServer

- Renamed WebUiServer.java → RefactoredWebUiServer.java
- Removed old UnifiedWebUiServer.java (was incomplete)
- All Swing dependencies removed
- Web UI fully functional

Ready for Phase 7 (Swing UI removal)."
```

---

## Timeline Estimate

| Phase | Time | Can Do Today? |
|-------|------|---------------|
| 1. Analyze dependencies | 30 min | ✅ Yes (within 22% tokens) |
| 2. Replace EditorInstance | 2-3 hours | ⚠️ Partial (may run out of tokens) |
| 3. Replace ProjectEditor | 1-2 hours | ❌ No (need new session) |
| 4. Replace actions | 2-3 hours | ❌ No |
| 5. Remove remaining Swing | 1-2 hours | ❌ No |
| 6. Rename and cleanup | 30 min | ❌ No |
| **Total** | **7-11 hours** | **Need 2-3 sessions** |

---

## What We Can Do in This Session (~22% tokens left)

### Option A: Start Phase 1 (Recommended)

**Analyze Swing dependencies in WebUiServer.java:**

```bash
# Count imports
grep "^import.*editor\." editor/src/main/java/de/dfki/vsm/web/WebUiServer.java | wc -l

# Find EditorInstance usage
grep -n "EditorInstance" editor/src/main/java/de/dfki/vsm/web/WebUiServer.java

# Analyze WebSocket protocol
grep -A 5 "dispatchWs" editor/src/main/java/de/dfki/vsm/web/WebUiServer.java
```

**Create:** `SWING_DEPENDENCIES_MAP.md` - Complete roadmap for refactoring

**Deliverable:** Clear plan you can follow independently

**Time:** 15-20 minutes, ~4000 tokens

---

### Option B: Document WebSocket Protocol

**Extract complete WebSocket method list from working WebUiServer.java:**

Read lines 1000-2000 of WebUiServer.java to document:
- All WebSocket methods it handles
- What each method does
- Parameters expected
- Response format

**Deliverable:** `WEBSOCKET_PROTOCOL_COMPLETE.md` - Reference for implementing

**Time:** 15-20 minutes, ~5000 tokens

---

### Option C: Create Quick-Reference Guide

**Minimal documentation for immediate use:**

```markdown
# Quick Reference - Refactoring WebUiServer.java

## Swing Dependencies to Replace

1. EditorInstance → EditorProjectService
2. ProjectEditor → Direct project access
3. WorkSpacePanel → SceneFlowService
4. Swing actions → Service methods

## Files to Modify

- editor/src/main/java/de/dfki/vsm/web/WebUiServer.java

## Files to Add

- Add mEditorProjectService field
- Add mSceneFlowService field
- Add mOpenProjects map

## Test Command

./gradlew :editor:run

## Git Workflow

1. Make ONE change
2. Test
3. Commit if works
4. Repeat
```

**Time:** 5 minutes, ~500 tokens

---

## Recommendation for This Session

**Do BOTH Option A and Option C:**

1. **Option C first** (5 min) - Quick reference for immediate use
2. **Option A next** (15 min) - Detailed dependency analysis

**Total time:** ~20 minutes
**Total tokens:** ~4500 (well within remaining budget)

**Then you have:**
- ✅ Clear understanding of what needs to be done
- ✅ Roadmap for refactoring
- ✅ Can continue independently in next session

---

## Alternative: If You Want to Start Coding

**I can start Phase 2** (Replace EditorInstance) but will likely run out of tokens mid-way.

**Pros:**
- Make actual progress on refactoring
- See concrete example of replacement pattern

**Cons:**
- May not finish
- You'll need to continue from partial state

**Your choice!**

---

## Long-Term Benefits of This Approach

### Compared to UnifiedWebUiServer Approach

| Aspect | Refactor Swing WebUiServer | Complete UnifiedWebUiServer |
|--------|--------------------------|---------------------------|
| **Start** | Complete working code | Incomplete stub |
| **Risk** | Low (test after each change) | High (big rewrite) |
| **Time** | 7-11 hours | 1-2 weeks |
| **Testing** | Continuous (works throughout) | All at end |
| **Rollback** | Easy (git revert one change) | Hard (big changeset) |
| **Learning** | See what each dependency does | Guess what's needed |

---

## Summary

**Current State:**
- ✅ Reverted to commit 3fe491fb (working)
- ✅ Experimental work saved in `phase6-experimental-work` branch
- ✅ Root cause identified (UnifiedWebUiServer incomplete)

**Recommended Path:**
- ✅ Refactor `WebUiServer.java` (Swing-coupled, complete)
- ❌ Don't complete `UnifiedWebUiServer.java` (would rebuild from scratch)

**This Session:**
- ✅ Create dependency analysis (Option A)
- ✅ Create quick reference (Option C)
- ✅ Provide roadmap for next session

**Next Session:**
- Start Phase 2 (Replace EditorInstance)
- Continue incremental refactoring

---

**What would you like me to do with remaining tokens?**

A) Analyze dependencies + Create quick reference (Recommended)
B) Document WebSocket protocol
C) Start coding Phase 2 (Replace EditorInstance)
D) Something else
