# Visual Scene Maker (Master)

(c) 2003-2026. All rights reserved.

Download releases: https://github.com/SceneMaker/VisualSceneMaker/releases

![SceneMaker](http://scenemaker.dfki.de/images/scenemaker/workspace.png)

This branch tracks current development for the Web-v1.0 architecture and Android-ready runtime split.

Website: [scenemaker.dfki.de](http://scenemaker.dfki.de)

## What Is New

- Web-first launcher: `SceneMaker4` starts the browser-based editor/runtime UI.
- Runtime split into dedicated modules:
  - `core-webserver` (desktop/web server integration)
  - `runtime-server` (headless runtime API server)
  - `core-http-android` (Android-ready HTTP/WS runtime transport)
- Android stub app (`android-stub/`) for end-to-end runtime integration on device/emulator.
- Portability guard tasks for core + Android-compatible modules.
- Semantic editor support via optional local embeddings service.

## Requirements

- JDK 21 for the main build.
- Some modules are compiled with Java 17 compatibility for portability.

## Build

- Full build: `./gradlew build`
- Tests only: `./gradlew test`
- Fat jar: `./gradlew shadowJar`
- Android thin jars (core + `androidCompatible` plugins): `./gradlew assembleAndroidThinJars`
- Clean: `./gradlew clean`
- Build with JavaFX runtime bundled in jar (legacy packaging): `./gradlew build -PincludeJavaFx=true`

## Run (Web UI)

Build and run the main app:

```bash
./gradlew shadowJar
java -jar build/libs/*-shadow.jar
```

Useful startup flags:

- `--allow-lan` (or `--allow-external`) to bind for external/LAN access
- `--no-browser` to skip auto-opening the browser

## Runtime Server (Headless)

The standalone runtime server runs projects without the full editor and serves runtime APIs/UI:

```bash
./gradlew :runtime-server:jar
java -jar runtime-server/build/libs/runtime-server-*.jar --port=8091
```

Common options:

- `--allow-lan`
- `--project=/path/to/project`
- `--autostart`
- `--token=...`

See `doc/runtime-server.md` for full API and deployment details.

## Android Stub Integration

`android-stub/` contains a minimal Android app that bundles runtime jars, runs a sample project, and exposes the Android runtime server.

Quick entry points:

- Overview: `android-stub/README.md`
- Full integration guide: `android-stub/ANDROID_INTEGRATION.md`

## Verification Tasks

- Core portability checks: `./gradlew verifyPortableCore`
- Android portability + compile gates: `./gradlew verifyAndroidPortable`
- Compile all services: `./gradlew compileServices`

## Embeddings Service (Optional)

The editor can use a local embeddings service for semantic features (for example scene title suggestions and dangling PlayScene replacement hints). The embedding model is not committed to the repository.

Download the model:

- `./gradlew :services:embeddings:downloadModel`

Build and run manually (auto-start is also supported by the editor):

- `./gradlew :services:embeddings:shadowJar`
- `java -jar services/embeddings/build/libs/embeddings-all.jar`

Optional environment overrides:

- `EMBEDDINGS_MODEL_DIR` - path to downloaded model directory
- `EMBEDDINGS_PORT` - service port (default `4050`)
- `EMBEDDINGS_JAR` - path to embeddings service jar
