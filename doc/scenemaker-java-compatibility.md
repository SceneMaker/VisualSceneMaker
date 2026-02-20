# Java Language Compatibility (Current State)

This document describes the Java compatibility policy as currently enforced by Gradle in the `web2026` architecture.

## Policy Summary

- Portable/runtime-critical modules compile with Java 17 API/bytecode compatibility.
- Desktop/editor/services modules use Java 21.
- Root packaging uses Java 21 and assembles both sets.

## Compatibility Matrix

| Module / Group | Java Level | Primary Enforcement | Notes |
|---|---|---|---|
| `:core` | 17 (`--release 17`) | `:core:verifyPortableCoreApis` | Portable runtime/model baseline |
| `:core-webserver` | 17 (`--release 17`) | Root Java 17 module set | Desktop/server adapter (`WebUiServer`) |
| `:core-http-jdk` | 17 (`--release 17`) | Root Java 17 module set | JDK HTTP transport adapter |
| `:core-http-android` | 17 (`--release 17`) | `:core-http-android:verifyAndroidPortableApis` | Android HTTP/WS transport |
| `:core-logic-jpl` | 17 (`--release 17`) | Root Java 17 module set | JPL logic adapter module |
| `:runtime-server` | 17 (`--release 17`) | Root Java 17 module set | `RuntimeMain` runtime-only launcher |
| `:plugins:*` | 17 (`--release 17`) | Root Java 17 module set + plugin-specific checks (for example `:plugins:AndroidGui:verifyAndroidPortableApis`) | Plugin ecosystem remains Java 17-compatible |
| `:editor` | 21 (toolchain) | Root Java 21 toolchain rule | Web UI build + editor-side service layer |
| `:services`, `:services:*` | 21 (toolchain) | Root Java 21 toolchain rule | Includes embeddings/semantic services |
| root app project | 21 (toolchain) | Root Java toolchain + packaging tasks | `SceneMaker4` packaging and desktop run tasks |

## Java 17-Compatible Modules

The root `build.gradle` marks these modules as Java 17-compatible by applying:

- `tasks.withType(JavaCompile).configureEach { options.release = 17 }`

Modules:

- `:core`
- `:core-webserver`
- `:core-http-jdk`
- `:core-http-android`
- `:core-logic-jpl`
- `:runtime-server`
- `:plugins:*` (all plugin subprojects)

## Java 21 Modules

For Java subprojects that are not in the Java 17 set, the root build applies:

- `java.toolchain.languageVersion = 21`

This includes:

- `:editor`
- `:services`
- `:services:embeddings`
- `:services:semantic-analysis`
- `:services:semantic-ud`
- root application project

## Enforcement Tasks

Portability checks are implemented and wired in:

- `:core:verifyPortableCoreApis`
- `:core-http-android:verifyAndroidPortableApis`
- `:plugins:AndroidGui:verifyAndroidPortableApis`
- Root convenience tasks:
  - `./gradlew verifyPortableCore`
  - `./gradlew verifyAndroidPortable`

These checks fail the build on imports/usages that violate portability boundaries (for example Swing/JavaFX/Javalin/JPL in Android-portable modules).

## Practical Rules

- In Java 17-compatible modules, do not use Java 18+ language features or APIs.
- Keep desktop/server-only APIs out of portable modules:
  - UI toolkits (`java.awt`, `javax.swing`, `javafx`)
  - Desktop HTTP/server libraries where not allowed by module policy
  - JPL usage outside `:core-logic-jpl` and explicitly desktop-targeted code
- Put Android transport code in `:core-http-android`.
- Put desktop web server code in `:core-webserver`.

## Architecture Context

The old monolithic desktop path has been split:

- `SceneMaker4` (root app) runs full editor mode via `:core-webserver`.
- `RuntimeMain` (`:runtime-server`) runs runtime-only mode.
- Android integration uses `:core-http-android` (plus Android stub/app flows).

This split is why Java 17 compatibility is enforced for runtime-critical modules while higher-level editor/services remain on Java 21.
