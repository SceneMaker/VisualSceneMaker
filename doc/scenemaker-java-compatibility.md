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

## Next Step After Core Android Sanitization
Once `:core` is sanitized for Android (no desktop-only APIs on portable paths), the next architectural phase is enabling a desktop web server/UI to connect to a remote core running on Android.

Concrete implementation steps:
1. Extract web server code from `:core` into a desktop adapter module (for example `:core-webserver`) that depends on `:core`.
2. Define a stable runtime API contract (`core-api` DTOs) for commands, snapshots, and runtime events.
3. Add a `RuntimeGateway` abstraction in `:core` for command execution and event streaming.
4. Implement an Android-side gateway host (HTTP + WebSocket) in an Android `Service`.
5. Implement a desktop remote gateway client and switch web server logic from direct in-memory runtime access to gateway calls.
6. Keep dual-mode operation:
   - Local mode (desktop): in-process gateway.
   - Remote mode (desktop to Android): network gateway.
7. Add reconnect/synchronization behavior:
   - Initial full snapshot fetch.
   - Ordered delta/event stream with sequence numbers.
   - Snapshot re-fetch on sequence gaps.
8. Add capability negotiation (for example logic engine disabled on Android) so desktop UI hides unsupported actions.
9. Secure the connection (token auth, CORS policy, optional TLS).
10. Add CI/integration tests for both local and remote modes.
