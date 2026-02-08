# Visual Scene Maker Master Version

(c) 2003-26. All rights reserved

### Download the current version at: https://github.com/SceneMaker/VisualSceneMaker/releases


Branch of Visual SceneMaker reserved for bleeding edge bug fixes and enhancements.
![SceneMaker](http://scenemaker.dfki.de/images/scenemaker/workspace.png)

This version is the master version. It includes the latest changes. Always handle with care.

For more information, visit the website 
[scenemaker.dfki.de](http://scenemaker.dfki.de)

## Build

- Standard build (no JavaFX runtime bundled): `./gradlew build`
- Legacy build with JavaFX jars included in the fat jar: `./gradlew build -PincludeJavaFx=true`

## Embeddings Service (Optional)

The editor uses a local embeddings service to power semantic features such as
scene title suggestions and dangling PlayScene replacement hints. The model is
**not** committed to the repo.

Download the local embedding model:

- `./gradlew :services:embeddings:downloadModel`

The editor will auto-start the embeddings service when needed. You can also
start it manually:

- `./gradlew :services:embeddings:shadowJar`
- `java -jar services/embeddings/build/libs/embeddings-all.jar`

Environment overrides (optional):

- `EMBEDDINGS_MODEL_DIR` – path to the downloaded model directory
- `EMBEDDINGS_PORT` – service port (default `4050`)
- `EMBEDDINGS_JAR` – path to the embeddings service jar
