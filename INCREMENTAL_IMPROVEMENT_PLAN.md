# Phase 6 - Incremental Improvement Plan

**Date:** 2026-01-13
**Status:** 🟢 REVERTED TO WORKING STATE - Ready for incremental improvements

---

## Current State

**Branch:** `web2026`
**Commit:** `3fe491fb` - "Refactoring code 5/7 phases done"
**Status:** ✅ WORKING - Web UI confirmed functional by user

### What Exists Now

**Swing UI (Still Present):**
- ✅ `EditorInstance.java` - Main Swing editor
- ✅ All dialogs, actions, workspace panels (120+ files)
- ✅ Full Swing functionality intact

**Web UI:**
- ✅ `UnifiedWebUiServer.java` (1227 lines - original implementation)
- ✅ `WebUiServer.java` in editor module (374KB - duplicate from core)
- ✅ Web UI works at this commit (confirmed by user)

**Services:**
- ✅ `EditorProjectService.java` - Headless project operations
- ✅ `SceneFlowService.java` - Headless sceneflow operations
- ✅ `RuntimeConnection.java`, `RuntimeConnectionManager.java` - Remote runtime support

### What Was Saved in Experimental Branch

**Branch:** `phase6-experimental-work`
**Commit:** `2871c53c`

Contains all our work from today:
- Editor.java (web-only entry point)
- LegacyWebUiServer.java (copy of core WebUiServer)
- Updated UnifiedWebUiServer.java (with WebSocket fixes)
- build.gradle improvements (plugin dependencies)
- 15+ documentation files

**Available for cherry-picking successful parts**

---

## Incremental Improvement Strategy

Apply improvements ONE AT A TIME, testing after each change.

---

## Improvement 1: Fix Token Generation Format ✅

**Issue:** UnifiedWebUiServer generates Base64 tokens that don't match Web UI's pattern validation.

**Fix:** Change to lowercase hex (32 characters).

### Files to Modify

1. **`editor/src/main/java/de/dfki/vsm/web/UnifiedWebUiServer.java`**

**Add method (around line 200):**
```java
private static String generateSecureToken() {
    SecureRandom random = new SecureRandom();
    byte[] bytes = new byte[16]; // 16 bytes = 128 bits = 32 hex chars
    random.nextBytes(bytes);
    StringBuilder sb = new StringBuilder();
    for (byte b : bytes) {
        sb.append(String.format("%02x", b));
    }
    return sb.toString();
}
```

**Update token generation in start() method:**
```java
// Find line with Base64 encoding:
// mAuthToken = Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);

// Replace with:
if (token == null || token.isEmpty()) {
    token = generateSecureToken();
}
mAuthToken = token;
```

### Test

```bash
./gradlew :editor:compileJava -PskipWebUi=true
./gradlew :editor:run
```

**Expected:**
- Token displayed shows 32 lowercase hex characters (e.g., `a1b2c3d4e5f6...`)
- Web UI auto-fetches token successfully
- No "pattern mismatch" error

**If SUCCESS:** Commit
**If FAILURE:** Revert, analyze

---

## Improvement 2: Add Plugin Dependencies to build.gradle ✅

**Issue:** Missing runtime plugin classes cause ClassNotFoundException.

**Fix:** Add runtimeOnly dependencies for all 28 plugins.

### Files to Modify

1. **`editor/build.gradle`**

**Add after existing dependencies (around line 20):**
```gradle
dependencies {
    // ... existing dependencies ...

    // Runtime plugin dependencies (for headless execution)
    runtimeOnly project(':plugins:console')
    runtimeOnly project(':plugins:timer')
    runtimeOnly project(':plugins:email')
    runtimeOnly project(':plugins:alma')
    runtimeOnly project(':plugins:AndroidGui')
    runtimeOnly project(':plugins:ssi')
    runtimeOnly project(':plugins:tricatworld')
    runtimeOnly project(':plugins:charamel')
    runtimeOnly project(':plugins:charamel-ws')
    runtimeOnly project(':plugins:decad')
    runtimeOnly project(':plugins:sockets')
    runtimeOnly project(':plugins:DriveSimulator')
    runtimeOnly project(':plugins:user-cue-service')
    runtimeOnly project(':plugins:fortunecookie')
    runtimeOnly project(':plugins:htmlgui-ws')
    runtimeOnly project(':plugins:odp')
    runtimeOnly project(':plugins:qrwebcam')
    runtimeOnly project(':plugins:reeti')
    runtimeOnly project(':plugins:ssj')
    runtimeOnly project(':plugins:studymaster-web')
    runtimeOnly project(':plugins:unity')
    runtimeOnly project(':plugins:wizard')
    runtimeOnly project(':plugins:yallah')
}
```

### Test

```bash
./gradlew :editor:compileJava -PskipWebUi=true
./gradlew :editor:run
# Load a project that uses plugins
```

**Expected:**
- No ClassNotFoundException errors
- Plugins load successfully

**If SUCCESS:** Commit
**If FAILURE:** Revert, analyze

---

## Improvement 3: Set Working Directory for Tutorials ✅

**Issue:** Tutorials not found because working directory is wrong.

**Fix:** Set workingDir in build.gradle.

### Files to Modify

1. **`editor/build.gradle`**

**Add after application block:**
```gradle
run {
    workingDir = rootProject.projectDir
}
```

### Test

```bash
./gradlew :editor:run
```

**Expected:**
- Tutorials panel shows 3 tutorials (not empty)
- Can load tutorial projects

**If SUCCESS:** Commit
**If FAILURE:** Revert, analyze

---

## Improvement 4: Add Authentication Middleware ⚠️

**Issue:** All endpoints require token, but /token and /info should be public.

**Fix:** Add authenticate() middleware with exemptions.

### Files to Modify

1. **`editor/src/main/java/de/dfki/vsm/web/UnifiedWebUiServer.java`**

**Add method (around line 250):**
```java
private void authenticate(Context ctx) {
    // Exempt public endpoints
    String path = ctx.path();
    if (path.equals(API_PREFIX + "/info") ||
        path.equals(API_PREFIX + "/token") ||
        path.startsWith("/images/") ||
        path.startsWith("/web-ui/")) {
        return;
    }

    // Check token
    String token = ctx.header("Authorization");
    if (token != null && token.startsWith("Bearer ")) {
        token = token.substring(7);
    }
    if (token == null) {
        token = ctx.queryParam("token");
    }

    if (!mAuthToken.equals(token)) {
        ctx.status(401).result("Unauthorized");
    }
}
```

**In start() method, after Javalin.create():**
```java
mApp.before(this::authenticate);
```

### Test

```bash
./gradlew :editor:compileJava -PskipWebUi=true
./gradlew :editor:run
```

**Expected:**
- /api/v1/token returns token without auth (200 OK)
- /api/v1/info returns info without auth (200 OK)
- Other endpoints require token (401 without token)

**If SUCCESS:** Commit
**If FAILURE:** Revert, analyze - This might break Web UI if token flow isn't right

---

## Improvement 5: Analyze Original UnifiedWebUiServer ⚠️ CRITICAL

**Before making any more WebSocket changes, we need to understand WHY the original version works.**

### Investigation Steps

1. **Read original UnifiedWebUiServer.java carefully**
2. **Compare with core WebUiServer.java**
3. **Identify what's different**
4. **Understand the working protocol**

### Key Questions

1. What WebSocket methods does UnifiedWebUiServer handle?
2. How does it serialize nodes/edges?
3. Does it have SceneFlow.Get handler?
4. How does mutation work?

**DO NOT modify anything yet** - just understand what's there.

---

## Improvement 6: Add Missing WebSocket Handlers (IF NEEDED) ⚠️ HIGH RISK

**Only proceed if investigation in Improvement 5 shows handlers are missing.**

### Potential Additions

From experimental branch (cherry-pick carefully):

1. **SceneFlow.Get handler** - If missing
2. **Comprehensive node serialization** - If minimal
3. **Comprehensive edge serialization** - If minimal
4. **mutateAndSnapshot pattern** - If missing

**CRITICAL:** Test THOROUGHLY after EACH addition.

---

## Testing Checklist (After Each Improvement)

### Compilation
```bash
./gradlew :editor:compileJava -PskipWebUi=true
```
✅ Must pass

### Server Start
```bash
./gradlew :editor:run
```
✅ Server starts without errors

### Web UI Landing Page
- ✅ Logo visible
- ✅ Connection badge shows "connected"
- ✅ Tutorials list populated
- ✅ Recent projects list populated
- ✅ Token auto-fetched (no manual entry)

### Project Loading
- ✅ Click recent project
- ✅ Editor view appears (not stuck on landing)
- ✅ SceneFlow canvas visible

### Visual Rendering
- ✅ AGENTS panel populated (Emma, Joal, etc.)
- ✅ SCENES list populated (welcome, name, etc.)
- ✅ SceneFlow nodes visible at correct sizes
- ✅ Edges visible with labels
- ✅ Variables displayed on supernodes
- ✅ Inspector panel shows data when node selected

### Browser Console
- ❌ No JavaScript errors
- ❌ No JSON parse errors
- ❌ No 401 errors (except expected ones)

---

## Commit Strategy

After each successful improvement:

```bash
git add <modified-files>
git commit -m "Phase 6 Improvement N: <Brief description>

<What was changed>
<Why it was needed>
<Test results>

All tests passed - Web UI still works."
```

Push regularly:
```bash
git push origin web2026
```

---

## Rollback Strategy

If any improvement breaks something:

```bash
# Undo last commit (keep changes)
git reset --soft HEAD~1

# Or discard changes entirely
git reset --hard HEAD~1
```

---

## Cherry-Picking from Experimental Branch

If you need specific changes from experimental work:

```bash
# Show what's in experimental branch
git show phase6-experimental-work:<file>

# Cherry-pick specific file changes
git checkout phase6-experimental-work -- <file>
# Review changes
# Test
# Commit if works
```

---

## Success Criteria for Phase 6

**Minimum (Must Have):**
1. ✅ Web UI works (projects load, SceneFlow renders)
2. ✅ AGENTS panel populated
3. ✅ SCENES list populated
4. ✅ Node/edge rendering correct
5. ✅ Inspector panel functional
6. ✅ No Swing UI dependencies (final step - do last)

**Nice to Have:**
1. Project save functionality
2. Project creation
3. Plugin management
4. Full editing capabilities

**Final Step (ONLY AFTER ALL ABOVE WORKS):**
- Remove Swing UI files
- Update build.gradle to remove JavaFX
- Create new Editor.java entry point
- Test EXTENSIVELY

---

## Current Next Steps

1. ✅ **DONE:** Revert to working state (commit 3fe491fb)
2. ✅ **DONE:** Save experimental work in branch
3. **TODO:** Apply Improvement 1 (Token format)
4. **TODO:** Test Improvement 1
5. **TODO:** Commit if successful
6. **TODO:** Continue with remaining improvements

---

## Notes

- **Web UI confirmed working at 3fe491fb** ✅
- **All experimental work preserved** in `phase6-experimental-work` branch
- **Can cherry-pick successful parts** from experimental branch
- **Low risk approach** - always have working state to fall back on
- **Test after EVERY change** - catch issues early

---

**Status:** 🟢 READY TO START INCREMENTAL IMPROVEMENTS

**Next Action:** Apply Improvement 1 (Token Format Fix)
