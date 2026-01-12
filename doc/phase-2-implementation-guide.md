# Phase 2 Implementation Guide: Extract Business Logic from Swing UI

**Status**: Ready to implement
**Prerequisites**: Phase 1 completed ✅
**Estimated time**: 3-4 hours
**Token estimate**: 50,000-75,000 tokens

---

## Context: What Was Done in Phase 1

✅ **Completed:**
- Moved `EditorConfig.java` from core to editor module
- Removed all editor dependencies from core module
- Verified core module compiles independently
- Core module is now 100% decoupled (Java 17, Android-ready)

**Files changed in Phase 1:**
- **Moved**: `core/src/.../EditorConfig.java` → `editor/src/.../EditorConfig.java`
- **Verified**: `VariableDefinition.java` validate() method already commented out
- **Build test**: `./gradlew :core:build` ✅ succeeds

---

## Phase 2 Overview

**Goal**: Extract business logic from Swing UI components into headless services that can be used by the Web UI.

**Why needed**: The current Web UI (in `editor/src/.../web/WebUiServer.java`) is tightly coupled to Swing components:
- Calls `EditorInstance.getInstance()` for project management
- Uses Swing action classes (`CreateNodeAction`, `RemoveNodeAction`, etc.) for sceneflow editing
- Cannot run without Swing dependencies

**Solution**: Create service layer classes that implement business logic without any Swing/UI dependencies.

---

## Phase 2 Tasks

### Task 2.1: Create EditorProjectService

**Purpose**: Handle project lifecycle without Swing UI dependencies.

**New file**: `editor/src/main/java/de/dfki/vsm/editor/service/EditorProjectService.java`

**What to extract from**: `editor/src/main/java/de/dfki/vsm/editor/EditorInstance.java`

#### Step-by-step Implementation

1. **Read EditorInstance.java to understand current implementation:**
   ```bash
   # File to read
   editor/src/main/java/de/dfki/vsm/editor/EditorInstance.java
   ```

2. **Identify methods to extract:**
   - `newProject()` - Creates new EditorProject
   - `openProject(String path)` - Opens existing project
   - `save()` - Saves current project
   - `saveAs(File file)` - Saves project to new location
   - `close()` - Closes project
   - Recent projects management (check for recent projects list)

3. **Create EditorProjectService.java with this structure:**

```java
package de.dfki.vsm.editor.service;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Headless service for managing EditorProject lifecycle.
 * Extracted from EditorInstance to remove Swing dependencies.
 */
public class EditorProjectService {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // Store for open projects (project ID -> EditorProject)
    private final Map<String, EditorProject> projects = new HashMap<>();

    // Recent projects list
    private final List<RecentProject> recentProjects = new ArrayList<>();

    /**
     * Creates a new empty project.
     * @param name Project name
     * @return New EditorProject instance
     */
    public EditorProject createProject(String name) {
        // TODO: Extract logic from EditorInstance.newProject()
        // 1. Create new EditorProject(true) // isNewProject = true
        // 2. Set project name
        // 3. Initialize default sceneflow structure
        // 4. Return project
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Opens an existing project from filesystem.
     * @param path Path to project directory
     * @return Loaded EditorProject instance, or null if failed
     */
    public EditorProject openProject(String path) {
        // TODO: Extract logic from EditorInstance.openProject()
        // 1. Create new EditorProject()
        // 2. Call project.parse(path)
        // 3. If successful, add to recent projects
        // 4. Store in projects map
        // 5. Return project
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Saves project to its current location.
     * @param project Project to save
     * @return true if successful
     */
    public boolean saveProject(EditorProject project) {
        // TODO: Extract logic from EditorInstance.save()
        // 1. Get project file from project.getProjectFile()
        // 2. Call project.write(file)
        // 3. If successful, mark project clean
        // 4. Return success status
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Saves project to a new location (Save As).
     * @param project Project to save
     * @param newPath New path for project
     * @return true if successful
     */
    public boolean saveProjectAs(EditorProject project, String newPath) {
        // TODO: Extract logic from EditorInstance.saveAs()
        // 1. Create File from newPath
        // 2. Call project.write(file)
        // 3. Update project path
        // 4. Mark project clean
        // 5. Add to recent projects
        // 6. Return success status
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Closes a project.
     * @param projectId Project ID to close
     * @return true if successful
     */
    public boolean closeProject(String projectId) {
        // TODO: Extract logic from EditorInstance.close()
        // 1. Get project from map
        // 2. If project.hasChanged(), warn (caller should handle save prompt)
        // 3. Call project.unload() if runtime is loaded
        // 4. Remove from projects map
        // 5. Return success status
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Checks if project has unsaved changes.
     * @param project Project to check
     * @return true if project has unsaved changes
     */
    public boolean isProjectDirty(EditorProject project) {
        return project.hasChanged();
    }

    /**
     * Marks project as clean (no unsaved changes).
     * Used after successful save.
     * @param project Project to mark clean
     */
    public void markProjectClean(EditorProject project) {
        // Update initial hash to current hash
        project.setInitialHash(project.getHashCode());
    }

    /**
     * Gets list of recent projects.
     * @return List of recent projects
     */
    public List<RecentProject> getRecentProjects() {
        return new ArrayList<>(recentProjects);
    }

    /**
     * Adds project to recent projects list.
     * @param path Project path
     * @param name Project name
     */
    public void addRecentProject(String path, String name) {
        // TODO: Extract from EditorInstance
        // 1. Create RecentProject object
        // 2. Remove if already exists (avoid duplicates)
        // 3. Add to beginning of list
        // 4. Limit to max 10 recent projects
        // 5. Save to preferences
    }

    /**
     * Gets all currently open projects.
     * @return Map of project ID -> EditorProject
     */
    public Map<String, EditorProject> getOpenProjects() {
        return new HashMap<>(projects);
    }

    /**
     * Gets a specific open project.
     * @param projectId Project ID
     * @return EditorProject or null if not found
     */
    public EditorProject getProject(String projectId) {
        return projects.get(projectId);
    }

    // Inner class for recent projects
    public static class RecentProject {
        public final String path;
        public final String name;
        public final long lastOpened;

        public RecentProject(String path, String name, long lastOpened) {
            this.path = path;
            this.name = name;
            this.lastOpened = lastOpened;
        }
    }
}
```

4. **Extract actual implementation:**
   - Open `EditorInstance.java`
   - Find each method mentioned above
   - Copy business logic (skip Swing UI code like `JOptionPane`, `setVisible()`, etc.)
   - Adapt to service methods
   - Test each method

5. **Testing EditorProjectService:**
   ```java
   // Create test in editor/src/test/java/de/dfki/vsm/editor/service/EditorProjectServiceTest.java
   EditorProjectService service = new EditorProjectService();

   // Test create
   EditorProject project = service.createProject("TestProject");
   assertNotNull(project);

   // Test save
   boolean saved = service.saveProjectAs(project, "/tmp/test-project");
   assertTrue(saved);

   // Test open
   EditorProject opened = service.openProject("/tmp/test-project");
   assertNotNull(opened);
   assertEquals("TestProject", opened.getProjectName());
   ```

---

### Task 2.2: Create SceneFlowService

**Purpose**: Handle sceneflow graph editing (nodes, edges, comments) without Swing dependencies.

**New file**: `editor/src/main/java/de/dfki/vsm/editor/service/SceneFlowService.java`

**What to extract from**:
- `editor/src/main/java/de/dfki/vsm/editor/action/CreateNodeAction.java`
- `editor/src/main/java/de/dfki/vsm/editor/action/CreateEdgeAction.java`
- `editor/src/main/java/de/dfki/vsm/editor/action/CreateCommentAction.java`
- `editor/src/main/java/de/dfki/vsm/editor/action/RemoveNodeAction.java`
- `editor/src/main/java/de/dfki/vsm/editor/action/RemoveEdgeAction.java`
- `editor/src/main/java/de/dfki/vsm/editor/action/ModifyNodeAction.java`
- And other action classes in `editor/src/main/java/de/dfki/vsm/editor/action/`

#### Step-by-step Implementation

1. **Read action classes to understand patterns:**
   ```bash
   # Files to read
   editor/src/main/java/de/dfki/vsm/editor/action/CreateNodeAction.java
   editor/src/main/java/de/dfki/vsm/editor/action/CreateEdgeAction.java
   editor/src/main/java/de/dfki/vsm/editor/action/RemoveNodeAction.java
   ```

2. **Identify common pattern in action classes:**
   - Most extend `AbstractAction` (Swing-specific)
   - `actionPerformed(ActionEvent e)` method contains business logic
   - They manipulate `EditorProject.getSceneFlow()` directly
   - Often get current node/edge from UI selection

3. **Create SceneFlowService.java structure:**

```java
package de.dfki.vsm.editor.service;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.model.sceneflow.chart.*;
import de.dfki.vsm.model.sceneflow.chart.edge.*;
import de.dfki.vsm.model.sceneflow.chart.graphics.*;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/**
 * Headless service for sceneflow graph manipulation.
 * Extracted from Swing action classes.
 */
public class SceneFlowService {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    /**
     * Creates a new node in the sceneflow.
     *
     * @param project EditorProject containing the sceneflow
     * @param type Node type ("BasicNode" or "SuperNode")
     * @param position Position for the new node (x, y)
     * @return Created BasicNode or null if failed
     */
    public BasicNode createNode(EditorProject project, String type, Point position) {
        // TODO: Extract from CreateNodeAction.actionPerformed()
        // 1. Get sceneflow from project
        // 2. Generate unique node ID
        // 3. Create node based on type
        // 4. Set node graphics (position, size)
        // 5. Add node to sceneflow
        // 6. Mark project as dirty
        // 7. Return created node
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Updates node properties.
     *
     * @param project EditorProject
     * @param nodeId Node ID to update
     * @param updates Map of property name -> new value
     * @return true if successful
     */
    public boolean updateNode(EditorProject project, String nodeId, Map<String, Object> updates) {
        // TODO: Extract from ModifyNodeAction
        // 1. Find node by ID in sceneflow
        // 2. Update properties (name, position, etc.)
        // 3. If position changed, update graphics
        // 4. Mark project as dirty
        // 5. Return success
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Deletes a node from the sceneflow.
     *
     * @param project EditorProject
     * @param nodeId Node ID to delete
     * @return true if successful
     */
    public boolean deleteNode(EditorProject project, String nodeId) {
        // TODO: Extract from RemoveNodeAction
        // 1. Find node in sceneflow
        // 2. Remove all connected edges
        // 3. Remove node from sceneflow
        // 4. Mark project as dirty
        // 5. Return success
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Creates a new edge between two nodes.
     *
     * @param project EditorProject
     * @param type Edge type (EEDGE, CEDGE, TEDGE, IEDGE, PEDGE, FEDGE)
     * @param sourceId Source node ID
     * @param targetId Target node ID
     * @return Created AbstractEdge or null if failed
     */
    public AbstractEdge createEdge(EditorProject project, String type,
                                   String sourceId, String targetId) {
        // TODO: Extract from CreateEdgeAction
        // 1. Find source and target nodes
        // 2. Validate edge can be created (no duplicates, valid connection)
        // 3. Create edge based on type
        // 4. Set edge graphics (control points)
        // 5. Add edge to sceneflow
        // 6. Mark project as dirty
        // 7. Return created edge
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Updates edge properties.
     *
     * @param project EditorProject
     * @param edgeId Edge ID to update
     * @param updates Map of property name -> new value
     * @return true if successful
     */
    public boolean updateEdge(EditorProject project, String edgeId,
                             Map<String, Object> updates) {
        // TODO: Extract from ModifyEdgeAction classes
        // Supported properties: condition, probability, timeout, etc.
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Deletes an edge from the sceneflow.
     *
     * @param project EditorProject
     * @param edgeId Edge ID to delete
     * @return true if successful
     */
    public boolean deleteEdge(EditorProject project, String edgeId) {
        // TODO: Extract from RemoveEdgeAction
        // 1. Find edge in sceneflow
        // 2. Remove edge
        // 3. Mark project as dirty
        // 4. Return success
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Creates a comment in the sceneflow.
     *
     * @param project EditorProject
     * @param text Comment text
     * @param bounds Comment bounds (x, y, width, height)
     * @return Created CommentBadge or null if failed
     */
    public CommentBadge createComment(EditorProject project, String text,
                                     Rectangle bounds) {
        // TODO: Extract from CreateCommentAction
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Updates comment text.
     *
     * @param project EditorProject
     * @param commentId Comment ID
     * @param text New comment text
     * @return true if successful
     */
    public boolean updateComment(EditorProject project, String commentId, String text) {
        // TODO: Implement
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Deletes a comment.
     *
     * @param project EditorProject
     * @param commentId Comment ID to delete
     * @return true if successful
     */
    public boolean deleteComment(EditorProject project, String commentId) {
        // TODO: Extract from RemoveCommentAction
        throw new UnsupportedOperationException("Not implemented yet");
    }

    // Helper methods

    private BasicNode findNodeById(SceneFlow sceneflow, String nodeId) {
        // Search recursively through sceneflow hierarchy
        // TODO: Implement node search
        return null;
    }

    private AbstractEdge findEdgeById(SceneFlow sceneflow, String edgeId) {
        // Search through all edges
        // TODO: Implement edge search
        return null;
    }

    private String generateNodeId(SceneFlow sceneflow) {
        // Generate unique node ID
        // Check existing IDs to avoid conflicts
        int counter = 1;
        String id;
        do {
            id = "node" + counter;
            counter++;
        } while (findNodeById(sceneflow, id) != null);
        return id;
    }
}
```

4. **Key extraction points per action class:**

| Action Class | Business Logic to Extract | Skip (UI Code) |
|--------------|---------------------------|----------------|
| `CreateNodeAction` | Node creation, ID generation, add to sceneflow | Dialog boxes, selection handling |
| `CreateEdgeAction` | Edge creation, validation, add to sceneflow | Mouse handlers, visual feedback |
| `RemoveNodeAction` | Node deletion, cascade edge removal | Confirmation dialogs |
| `ModifyNodeAction` | Property updates | Property dialog UI |
| `CopyNodesAction` | Clipboard logic | Visual selection |

5. **Testing SceneFlowService:**
   ```java
   SceneFlowService service = new SceneFlowService();
   EditorProject project = /* create test project */;

   // Test node creation
   Point pos = new Point(100, 100);
   BasicNode node = service.createNode(project, "BasicNode", pos);
   assertNotNull(node);
   assertTrue(project.hasChanged()); // dirty flag

   // Test edge creation
   BasicNode node2 = service.createNode(project, "BasicNode", new Point(200, 100));
   AbstractEdge edge = service.createEdge(project, "EEDGE", node.getId(), node2.getId());
   assertNotNull(edge);
   ```

---

### Task 2.3: Create SceneScriptService

**Purpose**: Handle scenescript editing without Swing dependencies.

**New file**: `editor/src/main/java/de/dfki/vsm/editor/service/SceneScriptService.java`

**What to extract from**:
- `editor/src/main/java/de/dfki/vsm/editor/project/ProjectEditor.java` (script editing logic)
- SceneScript model classes are already in core (no Swing dependencies)

#### Step-by-step Implementation

```java
package de.dfki.vsm.editor.service;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.util.List;

/**
 * Headless service for scenescript management.
 */
public class SceneScriptService {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    /**
     * Loads the scenescript from project.
     * @param project EditorProject
     * @return SceneScript instance
     */
    public SceneScript loadScript(EditorProject project) {
        return project.getSceneScript();
    }

    /**
     * Saves scenescript content.
     * @param project EditorProject
     * @param content Script content as text
     * @return true if successful
     */
    public boolean saveScript(EditorProject project, String content) {
        // TODO: Parse content and update SceneScript model
        // This is simpler than sceneflow since SceneScript parsing already exists
        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Validates scenescript syntax.
     * @param project EditorProject
     * @return List of syntax errors/warnings
     */
    public List<ScriptDiagnostic> validateScript(EditorProject project) {
        // TODO: Implement validation
        throw new UnsupportedOperationException("Not implemented yet");
    }

    public static class ScriptDiagnostic {
        public final int line;
        public final String message;
        public final String severity; // "error", "warning", "info"

        public ScriptDiagnostic(int line, String message, String severity) {
            this.line = line;
            this.message = message;
            this.severity = severity;
        }
    }
}
```

---

## Files to Read During Implementation

### Priority 1 (Essential):
1. `editor/src/main/java/de/dfki/vsm/editor/EditorInstance.java` - Project lifecycle
2. `editor/src/main/java/de/dfki/vsm/editor/action/CreateNodeAction.java` - Node creation
3. `editor/src/main/java/de/dfki/vsm/editor/action/CreateEdgeAction.java` - Edge creation
4. `editor/src/main/java/de/dfki/vsm/editor/action/RemoveNodeAction.java` - Node deletion

### Priority 2 (Important):
5. `editor/src/main/java/de/dfki/vsm/editor/action/ModifyNodeAction.java` - Node updates
6. `editor/src/main/java/de/dfki/vsm/editor/action/RemoveEdgeAction.java` - Edge deletion
7. `editor/src/main/java/de/dfki/vsm/editor/action/CreateCommentAction.java` - Comment creation

### Priority 3 (Nice to have):
8. Other action classes for complete coverage
9. `editor/src/main/java/de/dfki/vsm/editor/util/SceneFlowManager.java` - Utility methods

---

## Implementation Strategy

### Recommended Approach: Incremental

1. **Day 1**: EditorProjectService
   - Read EditorInstance.java
   - Extract basic project operations (create, open, save)
   - Test with simple project

2. **Day 2**: SceneFlowService - Part 1
   - Read CreateNodeAction and RemoveNodeAction
   - Implement node CRUD operations
   - Test node creation/deletion

3. **Day 3**: SceneFlowService - Part 2
   - Read CreateEdgeAction and RemoveEdgeAction
   - Implement edge CRUD operations
   - Test edge creation/deletion

4. **Day 4**: SceneScriptService + Integration
   - Implement basic script operations
   - Integration testing with all services
   - Verify no Swing dependencies

---

## Verification Checklist

After completing Phase 2, verify:

- [ ] All three service classes compile
- [ ] No Swing imports in service classes (`import javax.swing.*` or `import java.awt.*` except Point/Rectangle for coordinates)
- [ ] Services can be instantiated without UI
- [ ] Basic operations work (create project, add node, add edge)
- [ ] Projects can be saved/loaded using services
- [ ] Unit tests pass
- [ ] Services are ready to be called from WebUiServer

---

## Known Challenges & Solutions

### Challenge 1: Node ID Generation
**Problem**: Current implementation may use UI-based counters
**Solution**: Use UUID or counter from sceneflow model itself

### Challenge 2: Undo/Redo Support
**Problem**: Current Swing UI has undo/redo via UndoManager
**Solution**: Phase 2 focuses on basic operations. Undo/redo can be added in Phase 4

### Challenge 3: Validation Logic
**Problem**: Some validation happens in UI dialogs
**Solution**: Move validation to service layer, return error messages

### Challenge 4: Grid Snapping
**Problem**: Node positioning uses EditorConfig grid settings
**Solution**: Pass EditorConfig to service methods or make it optional

---

## Integration with Phase 3

Once Phase 2 is complete, Phase 3 will use these services:

```java
// In RuntimeServerImpl (Phase 3)
// For editor mode, inject services:
private EditorProjectService projectService = new EditorProjectService();
private SceneFlowService sceneflowService = new SceneFlowService();

// Then in WebSocket handlers:
private void handleNodeAdd(JSONObject params) {
    String projectId = params.getString("projectId");
    EditorProject project = projectService.getProject(projectId);

    Point position = new Point(
        params.getInt("x"),
        params.getInt("y")
    );

    BasicNode node = sceneflowService.createNode(
        project,
        params.getString("type"),
        position
    );

    // Return node data as JSON
    return createNodeResponse(node);
}
```

---

## Success Criteria

Phase 2 is complete when:

1. ✅ `EditorProjectService.java` exists and handles project lifecycle
2. ✅ `SceneFlowService.java` exists and handles sceneflow editing
3. ✅ `SceneScriptService.java` exists and handles script operations
4. ✅ All services compile without Swing dependencies
5. ✅ Basic operations tested and working
6. ✅ Code is documented with JavaDoc
7. ✅ Ready for integration in Phase 3/4

---

## Next Steps After Phase 2

Once Phase 2 is complete:
- **Phase 3**: Create RuntimeServer API (uses core only)
- **Phase 4**: Merge WebUiServer implementations (uses services from Phase 2)
- **Phase 5**: Editor-to-Runtime connections
- **Phase 6**: Remove Swing UI code
- **Phase 7**: Module restructuring

---

## Command Reference

```bash
# Compile services
./gradlew :editor:compileJava

# Run tests
./gradlew :editor:test

# Check for Swing dependencies
grep -r "import javax.swing" editor/src/main/java/de/dfki/vsm/editor/service/
grep -r "import java.awt" editor/src/main/java/de/dfki/vsm/editor/service/ | grep -v Point | grep -v Rectangle

# Build entire project
./gradlew build -x test
```

---

**Document Version**: 1.0
**Created**: 2026-01-11
**Phase 1 Status**: ✅ Complete
**Phase 2 Status**: 📋 Ready to implement
