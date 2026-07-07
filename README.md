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
- Runtime benchmark module (`benchmark/`) for measuring interpreter timing and memory.

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

Or run directly from Gradle:

```bash
./gradlew run             # localhost only
./gradlew run-lan         # bind to 0.0.0.0 for LAN access / remote co-editing
./gradlew run-lan-secure  # LAN access + HTTPS via a locally-trusted mkcert cert
```

> Note: `./gradlew run --allow-lan` does **not** work — `--allow-lan` would be
> parsed as a Gradle option. Use the `run-lan` task, or pass app args explicitly
> with `./gradlew run --args="--allow-lan"`.

Useful startup flags:

- `--allow-lan` (or `--allow-external`) to bind for external/LAN access
- `--secure` serve HTTPS/WSS via mkcert (see below)
- `--no-browser` to skip auto-opening the browser

### Secure mode (`--secure`) — HTTPS for remote collaboration

Remote collaborators over a LAN IP get a browser **insecure context**, which breaks
features that require Web Crypto (e.g. the charamel character's model loading). Serving
over HTTPS fixes this. `--secure` provisions a locally-trusted certificate with
[mkcert](https://github.com/FiloSottile/mkcert) and serves the **htmlgui GUI** and the
**charamel-embed character** over HTTPS/WSS.

The **editor stays on plain HTTP** on purpose: a remote user who hasn't installed the CA
yet can always load the share link and download the certificate without hitting a TLS
warning. The server also opens an HTTPS trust-probe port (`8443`) that the web UI silently
checks; if the client doesn't yet trust the CA, the editor shows a one-time
**"install the certificate"** banner (with a download button, per-OS steps, and a
*Re-check* button) and hides it automatically once trust is detected.

Install mkcert once on the **host** machine:

```bash
brew install mkcert nss     # macOS (nss = Firefox support)
# choco install mkcert       # Windows
# see the mkcert README for Linux
```

Then run:

```bash
./gradlew run-lan-secure
```

Each **collaborator** installs the host's CA once. The banner walks them through it (or grab
it directly from `GET /api/v1/ca`, served over HTTP as `vsm-ca.crt`); import into the OS trust
store (and Firefox's own store, if used). After that the character loads with no warnings.

If mkcert is not installed, `--secure` logs an install hint and falls back to plain HTTP.

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

## Benchmark

The `benchmark/` module measures pure-interpreter runtime performance (no plugins):

```bash
# Memory + latency, default tiers: 1 / 10 / 50 / 100 concurrent projects
./gradlew :benchmark:runBenchmark

# Memory only, extended range
./gradlew :benchmark:runBenchmark -Pmode=memory -PprojectCounts=1,10,50,100,200

# Latency with ZGC for GC-impact comparison
./gradlew :benchmark:runBenchmark -Pmode=latency -PjvmArgs="-XX:+UseZGC -Xmx8g"
```

Options: `-PprojectCounts`, `-Piterations` (default 100), `-Ptimeout` (default 200 ms), `-Pmode` (all/memory/latency), `-PjvmArgs`.
GC log is written to `benchmark/gc.log` on each run.

First measured results (Apple Silicon, macOS 15, G1GC, 4 GB heap):

| Concurrency | p50 deviation | p99 deviation | Live heap overhead |
|---|---|---|---|
| 1 project | 3 ms | 5 ms | ~1 MB |
| 10 projects | 3 ms | 5 ms | ~1 MB |
| 50 projects | 3 ms | 6 ms | ~2 MB |
| 100 projects | 3 ms | 6 ms | ~2 MB |

Latency is flat from 1 to 100 concurrent projects; natural GC pauses stayed below 2.1 ms.
See `doc/vsm-realtime-capabilities.md` for full analysis and methodology.

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
