# Repository Guidelines

## Project Structure & Module Organization
The main application lives under `src/main/java` with resources in `src/main/resources`. Unit tests are in `src/test/java` (for example, `de/dfki/vsm/xtension/decad/*Test.java`). The Gradle multi-project layout includes the core engine in `core/` and optional extensions under `plugins/` (for example, `plugins/DriveSimulator`). Shared assets and bundled jars live in `res/` and `lib/`. `swi/` contains auxiliary components, and `doc/` holds documentation. Build outputs go to `build/`.

## Build, Test, and Development Commands
- `./gradlew build` compiles all modules, runs tests, and produces jars.
- `./gradlew test` runs the JUnit test suite under `src/test/java`.
- `./gradlew shadowJar` builds a fat jar with dependencies; the main entry point is `de.dfki.vsm.SceneMaker3`.
- `./gradlew clean` removes build outputs before a fresh build.

## Coding Style & Naming Conventions
Use Java 21 syntax (`sourceCompatibility = JavaVersion.VERSION_21`). Follow the existing code style: 4-space indentation, braces on the same line, and standard Java naming (classes in `PascalCase`, methods/fields in `camelCase`, constants in `UPPER_SNAKE_CASE`). Keep package names under `de.dfki.vsm.*`. There is no enforced formatter; match the surrounding file.

## Testing Guidelines
Tests use JUnit (both 4 and 5 dependencies are present) and live in `src/test/java`. Name tests with the `*Test.java` suffix and keep helper fakes in `src/test/java/fakes`. Some plugins include their own test assets (for example, `plugins/studymaster-web/...`); follow their local README files if you touch them.

## Commit & Pull Request Guidelines
Recent commits use short, descriptive messages (often lowercase) without ticket prefixes or conventional-commit tags. Keep subjects concise and focused on the change. For pull requests, include a clear summary, testing notes, and links to any relevant issues. Provide screenshots or recordings for editor/UI changes.

## Configuration Notes
JavaFX is required (configured via the Gradle JavaFX plugin). External jars may be referenced from `lib/`, so keep that folder updated if you add dependencies outside Maven.
