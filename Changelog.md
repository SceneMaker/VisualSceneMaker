# Changelog

Visual Scene Maker (VSM) public code repository.

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Android stub integration app (`android-stub`) with end-to-end runtime flow and sample project asset sync.
- Android runtime HTTP/WebSocket transport module (`core-http-android`) and Android runtime bridge/server components.
- Standalone runtime launcher module (`runtime-server`) with `RuntimeMain` for `RUNTIME_ONLY` server mode.
- New architecture overview diagram for February 2026 (`doc/architecture-overview-Feb-26.svg`).
- iOS core planning document (`doc/ios-core-implementation-plan.md`).
- Optional semantic UD service wiring and editor-facing semantic syntax endpoint support.
- The Changelog!
- Plugin `studymaster-web`: new interface to control the flow from web page (#185, #209)
- Plugin `yallah`: to control YALLAH avatars via websocket protocol (#184)
- Plugin `VLCRemoteController`: to control VLC from VSM and play any kind of media (#249).

### Changed
- Architecture moved to web-first `SceneMaker4` launcher and dual `WebUiServer` mode operation (`FULL_EDITOR` / `RUNTIME_ONLY`).
- Runtime/server stack split into dedicated modules (`core-webserver`, `core-http-jdk`, `core-http-android`, `core-logic-jpl`, `runtime-server`).
- Web UI and Android runtime flow cleanup for finalized Android-ready project execution and device UX.
- Runtime performance optimizations finalized (including SymbolTable copy-on-write/caching improvements and guard dependency tracking refinements).
- Project documentation refreshed for current architecture and workflows:
  - `README.md`
  - `doc/architecture-details.md`
  - `doc/architecture-overview-Jan-26.svg` (historical snapshot)
- study-master web has a much longer timeout for websockets

### Removed
- Swing-first launcher path was retired in favor of the web-first launcher/runtime architecture.

### Deprecated
- Plugin `decad` is deprecated as it is replaced by `yallah`

### Fixed
- Android WebSocket broadcasting moved off the main thread; session shutdown handling hardened.
- Supernode outgoing edge connection behavior improved in the web UI/editor flow.
- Solved bug freezing the editor while navigating sub-nodes (#214)
- Key press management library was flooding the console with messages (#227)
- PlayAction command can now parse floats in scientific format (#230)

### Security

## [4.0.1] - 2020-01-22
