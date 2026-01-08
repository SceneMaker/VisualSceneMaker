# Java Language Compatibility (Core + Plugins)

This document defines the language level policy needed for Android compatibility.
The guiding rule is: **core and plugins compile as Java 17**.

## Targets
- Core: Java 17 (Android-compatible bytecode).
- Plugins: Java 17 (same guarantee as core).
- Editor (web UI + Swing reference): Java 21 is acceptable.
- Root application: Java 21 is acceptable for desktop packaging.

## Rationale
- Android runtime currently targets Java 17 language level.
- Keeping core/plugins at 17 ensures portability to Android.
- Editor and desktop-only tooling can use newer Java features.

## Build enforcement (planned)
- `:core` Gradle:
  - Toolchain `languageVersion = 17`.
  - `JavaCompile.options.release = 17` to lock APIs.
- `:plugins:*` Gradle:
  - Toolchain `languageVersion = 17`.
  - `JavaCompile.options.release = 17`.
- `:editor` and root can remain on 21.

## Compatibility rules
- Do not use Java 18+ language features in core/plugins.
- Avoid JDK 18+ APIs in core/plugins (enforced by `--release 17`).
- UI toolkits (Swing/JavaFX) must stay in editor only.

## Dependency constraints
- Core/plugins dependencies must support Java 17.
- If a plugin requires a higher JDK or desktop-only APIs, mark it as
  desktop-only and exclude it from Android builds.

## Android build inputs
- Android build uses `:core` + compatible plugins only.
- Editor module is excluded from Android packaging.

## Verification (planned)
- Add a CI task to compile `:core` and `:plugins:*` with `--release 17`.
- Add a compatibility note in plugin READMEs if they are desktop-only.
