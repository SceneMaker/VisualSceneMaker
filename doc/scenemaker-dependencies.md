# SceneMaker Dependency Map

This document describes the Gradle module dependencies after introducing the `editor` subproject.

```mermaid
graph TD
    Root["root (SceneMaker3)"] --> Core[:core]
    Root --> Editor[:editor]
    Root --> PlugAlma[:plugins:alma]
    Root --> PlugAndroid[:plugins:AndroidGui]
    Root --> PlugConsole[:plugins:console]
    Root --> PlugEmail[:plugins:email]
    Root --> PlugStudy[:plugins:studymaster-web]
    Root --> PlugFortune[:plugins:fortunecookie]
    Root --> PlugEmma[:plugins:emma-user-model]
    Root --> PlugDecad[:plugins:decad]
    Root --> PlugHtmlGui[:plugins:htmlgui-ws]
    Root --> PlugOdp[:plugins:odp]
    Root --> PlugQr[:plugins:qrwebcam]
    Root --> PlugUnity[:plugins:unity]
    Root --> PlugWizard[:plugins:wizard]
    Root --> PlugYallah[:plugins:yallah]
    Root --> PlugSsi[:plugins:ssi]
    Root --> PlugSsj[:plugins:ssj]
    Root --> PlugReeti[:plugins:reeti]
    Root --> PlugTimer[:plugins:timer]
    Root --> PlugTriCat[:plugins:tricatworld]
    Root --> PlugCharamel[:plugins:charamel]
    Root --> PlugCharamelWs[:plugins:charamel-ws]
    Root --> PlugSockets[:plugins:sockets]
    Root --> PlugDrive[:plugins:DriveSimulator]
    Root --> PlugVlc[:plugins:VLCRemoteController]

    Editor --> Core
    Editor --> ExtApi[:plugins:extensionAPI]

    PlugAlma --> Core
    PlugAlma --> ExtApi
    PlugAndroid --> Core
    PlugAndroid --> ExtApi
    PlugConsole --> Core
    PlugConsole --> ExtApi
    PlugEmail --> Core
    PlugEmail --> ExtApi
    PlugStudy --> Core
    PlugStudy --> ExtApi
    PlugFortune --> Core
    PlugFortune --> ExtApi
    PlugEmma --> Core
    PlugEmma --> ExtApi
    PlugDecad --> Core
    PlugDecad --> ExtApi
    PlugHtmlGui --> Core
    PlugHtmlGui --> ExtApi
    PlugOdp --> Core
    PlugQr --> Core
    PlugQr --> ExtApi
    PlugUnity --> Core
    PlugUnity --> ExtApi
    PlugWizard --> Core
    PlugWizard --> ExtApi
    PlugYallah --> Core
    PlugYallah --> ExtApi
    PlugSsi --> Core
    PlugSsi --> ExtApi
    PlugSsj --> Core
    PlugReeti --> Core
    PlugReeti --> ExtApi
    PlugTimer --> Core
    PlugTimer --> ExtApi
    PlugTriCat --> Core
    PlugTriCat --> ExtApi
    PlugCharamel --> Core
    PlugCharamel --> ExtApi
    PlugCharamelWs --> Core
    PlugCharamelWs --> ExtApi
    PlugSockets --> Core
    PlugSockets --> ExtApi
    PlugDrive --> Core
    PlugDrive --> ExtApi
    PlugDrive --> PlugSockets
    PlugVlc --> Core
    PlugVlc --> ExtApi

    Core --> Cup["java-cup-runtime (compileOnly/runtimeOnly)"]
    Core --> Jpl["jpl"]
    Core --> Jaxb["jaxb-api"]
    Core --> Anno["annotations"]
```

Notes:
- `root` assembles the runnable jar and depends on `:editor`, `:core`, and all plugins.
- `:editor` contains the Swing UI, web server, and web UI build (`editor/web-ui`).
- `:core` remains independent of editor code and provides runtime + model.
- Most plugins depend on `:core` and `:plugins:extensionAPI`.
