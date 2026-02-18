# VSM Android Integration Guide

This document explains how to use `android-stub` as a template to embed VSM runtime into your own Android app.

## 1. What `android-stub` contains

`android-stub` is a minimal Android app that:

- Loads a VSM project from app assets into internal app storage.
- Starts `CoreRuntime` on Android.
- Hosts Android-compatible HTTP/WebSocket runtime endpoints on port `8091` using `core-http-android`.
- Optionally lets a desktop Web UI connect through `adb forward`.
- Demonstrates plugin usage with the `timer` plugin jar.

Key files:

- `android-stub/app/src/main/java/de/dfki/vsm/androidstub/MainActivity.java`
- `android-stub/app/build.gradle`
- `core-http-android/src/main/java/de/dfki/vsm/runtime/api/android/*`

## 2. Architecture overview

At runtime, the flow is:

1. Android app starts.
2. `PlatformBootstrap.configureForAndroid()` configures Android-safe runtime defaults.
3. Project XML files are copied from `assets/SimpleProject` to internal storage.
4. `CoreRuntime` is created with that project directory.
5. `AndroidRuntimeEndpoint` wraps runtime command handling.
6. `AndroidRuntimeServer` exposes HTTP + WS (`/api/v1/...`, `/ws`).
7. UI (local app or remote Web UI) controls runtime via runtime commands.

## 3. Minimum integration steps

## 3.1 Add modules/jars

Your Android app needs access to:

- `core` jar
- `core-http-android` jar
- Your plugin jars (for plugins used by the project)

In `android-stub`, this is automated by Gradle tasks in `android-stub/app/build.gradle`:

- `buildVsmJars`
- `syncVsmJars`

These tasks build jars from the main repo and copy them into `android-stub/app/libs`.

## 3.2 Add Android dependencies

`android-stub/app/build.gradle` includes:

- `org.nanohttpd:nanohttpd:2.3.1`
- `org.nanohttpd:nanohttpd-websocket:2.3.1`
- `implementation fileTree(dir: 'libs', include: ['*.jar'])`

## 3.3 Provide project assets

Copy project files (`project.xml`, `sceneflow.xml`, `scenescript.xml`, etc.) into:

- `android-stub/app/src/main/assets/SimpleProject`

Then copy them at runtime to internal storage before creating `CoreRuntime`.

## 3.4 Start runtime + server

Follow the pattern in `MainActivity.ensureBackendReady()`:

1. `PlatformBootstrap.configureForAndroid()`
2. `CoreRuntime runtime = new CoreRuntime(projectDir)`
3. `AndroidRuntimeEndpoint endpoint = new AndroidRuntimeEndpoint(runtime, projectDir)`
4. `AndroidRuntimeServer server = new AndroidRuntimeServer(8091, endpoint, "")`
5. `server.startServer()`

## 4. Plugin integration

## 4.1 How plugins are brought into the Android app

VSM plugins are regular jars. To make them available on Android:

1. Build the plugin jar (for example `:plugins:timer:jar`).
2. Copy it into Android app `libs/`.
3. Include `fileTree(dir: 'libs', include: ['*.jar'])` dependency.
4. Ensure `project.xml` references the plugin class and marks it to load.

In `android-stub`, this is done in `syncVsmJars` and currently copies:

- `timer-plugin.jar`

## 4.2 Add another plugin (example)

Update `buildVsmJars`:

- Add plugin Gradle task, e.g. `:plugins:myplugin:jar`

Update `syncVsmJars`:

- Copy built plugin jar from `plugins/myplugin/build/libs` into `app/libs`, optionally renaming.

Ensure the project config (`project.xml`) has a plugin entry that matches:

- `className` = plugin implementation class
- load flag enabled

## 4.3 Android plugin caveats

A plugin that works on desktop may fail on Android if it depends on:

- `java.awt` / `javax.swing`
- desktop file paths assumptions
- unsupported native libraries
- desktop-only network/server APIs

For Android portability, plugin code should depend only on Android-safe/core Java APIs available on Android.

## 5. Remote Web UI connection

When app is running and server started:

1. Connect device/emulator via ADB.
2. Forward port:

```bash
adb forward tcp:8091 tcp:8091
```

3. Open desktop Web UI against:

- `http://127.0.0.1:8091`

## 6. Runtime command control

The Android runtime endpoint supports key runtime commands such as:

- `Runtime.Start`
- `Runtime.Pause`
- `Runtime.Resume`
- `Runtime.Stop`
- `Runtime.Variable.Set`

These can be issued via WS `cmd` messages or mapped HTTP routes under `/api/v1/runtime/*`.

## 7. Recommended production hardening

For real app integration:

1. Add authentication token to `AndroidRuntimeServer` (currently empty token in stub).
2. Restrict bind/network exposure if remote control is not needed.
3. Replace demo logging with structured app logging.
4. Run runtime/server in lifecycle-aware components (Service or foreground Service if needed).
5. Add error reporting for failed plugin loads and project parse/runtime startup errors.

## 8. Troubleshooting checklist

If runtime does not start:

- Verify project files were copied to internal storage.
- Verify plugin jars are present in `app/libs`.
- Verify plugin class names in `project.xml`.

If Web UI cannot connect:

- Ensure app started server (`Android runtime server listening on port 8091`).
- Re-run `adb forward tcp:8091 tcp:8091`.
- Check CORS/preflight responses from `/api/v1/...`.

If a plugin seems ignored:

- Confirm plugin jar is actually packaged in APK (`app/libs` + dependency).
- Confirm plugin is marked to load in project config.
- Confirm plugin has no desktop-only dependencies.

## 9. Suggested adaptation strategy for your app

1. Keep `android-stub` as a reference module.
2. Move `ensureBackendReady()` logic into your app-specific runtime manager.
3. Keep project asset copy + runtime/server startup unchanged initially.
4. Add your own UI layer on top of `AndroidRuntimeEndpoint` commands.
5. Add plugins one by one and validate compatibility early.
