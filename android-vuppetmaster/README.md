# VSM VuppetMaster (Android)

Runs a VisualSceneMaker project on-device and renders a VuppetMaster character natively in a
WebView. The mobile counterpart to the desktop charamel-embed plugin — it drives the *same*
character page (`character.html` + `vm-adapter.js`) but over a WebView JS bridge instead of a
WebSocket, via the `AndroidBridgeTransport` in the `charamel-embed` plugin.

**Milestone 1 (this app):** launch → the character loads over a secure origin → tap to start →
Xenia speaks the SceneFlow's welcome line.

This folder is **self-contained**: the VSM runtime jars (`app/libs/*.jar`) and the character page
(`assets/character.html`, `assets/vm-adapter.js`) are committed, so you can build it in Android
Studio **without** the parent VisualSceneMaker repo. You only supply your SDK path and a VuppetMaster
license key.

## Prerequisites

- Android Studio (bundles a JDK) + Android SDK; a device or emulator with **API 26+**.
- A VuppetMaster **license key** and **app name** (the character to load) — from Charamel.
- Internet on the device (the character engine downloads from `engine.vuppetmaster.com`).

## One-time setup: local.properties

Copy the template and fill it in (`local.properties` is git-ignored — machine-specific SDK path and
secret key):

```bash
cp local.properties.template local.properties
```

```properties
sdk.dir=/absolute/path/to/Android/sdk        # Android Studio usually sets this on first open
vuppetmaster.licenseKey=YOUR_KEY_HERE
vuppetmaster.appName=Xenia
# optional: vuppetmaster.engineUrl=https://engine.vuppetmaster.com/api/engine/vuppetmaster.iife.js
```

The build injects these into `BuildConfig`, and the app synthesizes `window.VSM_CONFIG` for the
character page at runtime. The committed `assets/XeniaDemo/project.xml` keeps an empty `licenseKey`.

## Build & run

Open this folder in Android Studio and press **Run**, or from the command line:

```bash
./gradlew installDebug   # build + install on the connected device
```

No parent repo or extra build step is needed — it compiles against the committed jars.

## Updating the vendored VSM artifacts (maintainers only)

The committed jars + character page are a **snapshot**. If you change `core` / `charamel-embed` in
the parent monorepo, refresh them (only works when this folder sits inside the VSM repo):

```bash
./gradlew refreshVsmArtifacts    # rebuilds app/libs/*.jar + assets/*.js from live source
```

Then commit the updated `app/libs/*.jar` and `assets/character.html|vm-adapter.js`.

## How it works

- **Secure context:** the character page is served to the WebView over
  `https://appassets.androidplatform.net` (via `WebViewAssetLoader`) — a secure context, which the
  VuppetMaster engine requires (`crypto.subtle`) to load the model. No certificate needed.
- **Transport:** `CharamelEmbedExecutor` detects Android (`PlatformBootstrap.isAndroid()`) and uses
  `AndroidBridgeTransport`. VSM → page envelopes are pushed via
  `webView.evaluateJavascript("window.vsmDispatch(…)")`; page → VSM feedback arrives through the
  `AndroidVSM.send` JS bridge.
- **Timing:** the SceneFlow waits for `avatar_ready` (set when the engine reports `vm.ready`) before
  speaking, so the welcome utterance never races the model load.

## Layout

```
app/src/main/
  assets/XeniaDemo/        bundled VSM project (empty licenseKey; key injected via BuildConfig)
  assets/character.html    synced from the charamel-embed plugin (git-ignored)
  assets/vm-adapter.js     synced from the charamel-embed plugin (git-ignored)
  java/.../MainActivity.java
```
