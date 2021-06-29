# Changelog

Visual Scene Maker (VSM) public code repository.

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- The Changelog!
- Plugin `studymaster-web`: new interface to control the flow from web page (#185, #209)
- Plugin `yallah`: to control YALLAH avatars via websocket protocol (#184)
### Changed
### Removed
### Deprecated
- Plugin `decad` is deprecated as it is replaced by `yallah`
### Fixed
- Solved bug freezing the editor while navigating sub-nodes (#214)
- Key press management library was flooding the console with messages (#227)
- PlayAction command can now parse floats in scientific format (#230)
### Security

## [4.0.1] - 2020-01-22