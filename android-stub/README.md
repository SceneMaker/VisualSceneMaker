# Android Stub

This is a minimal Android app that runs VSM core with `doc/SimpleProject`.

For a full integration guide (including plugin packaging/loading), see:

- `android-stub/ANDROID_INTEGRATION.md`

## What it does

- Copies `doc/SimpleProject/*.xml` into app internal storage.
- Builds and bundles runtime jars from the main repo:
  - `core` jar (renamed to `core-android.jar`)
  - `core-http-android` jar (renamed to `core-http-android.jar`)
  - `plugins/timer` jar (renamed to `timer-plugin.jar`)
  - `plugins/AndroidGui` jar (renamed to `androidgui-plugin.jar`)
- Starts the runtime with `CoreRuntime`.
- Starts an embedded Android runtime HTTP/WS server from `core-http-android` on port `8091`.
- Executes the project and shows live `cnt` and `time` variable values.
- Demonstrates Android GUI roundtrip:
  - SceneFlow updates `counterLabel` / `messageLabel`
  - Android `inputField` and `submitButton` write back to SceneFlow variables

## Run in emulator

1. Open `/Users/gebhard/Code/Repo/VisualSceneMaker/android-stub` in Android Studio.
2. Let Gradle sync download Android dependencies.
3. Run the `app` target on an emulator (API 26+).
4. Tap `Start`.

The app build runs these tasks automatically before `preBuild`:

- `buildVsmJars` (`../gradlew :core:jar :core-http-android:jar :plugins:timer:jar :plugins:AndroidGui:jar`)
- `syncVsmJars` (copies jars to `app/libs`)
- `syncSimpleProjectAssets` (copies project XML files)

For external Android apps, prefer building Android thin jars from the repo root:

- `./gradlew assembleAndroidThinJars`

This writes jars to `android-thin-jars/` (core + plugins where `plugin-properties.json` has `plugin.androidCompatible: true`).

## Connect from desktop Web UI

1. Run the app in emulator/device and tap `Start`.
2. In desktop terminal run:
   - `adb forward tcp:8091 tcp:8091`
3. Point your web client to:
   - `http://127.0.0.1:8091`
