# SceneMaker Web UI (Svelte)

This folder contains the Svelte frontend source. The production build outputs
to `src/main/resources/web-ui` so it can be served by the Javalin server.

## Development
```
npm install
npm run dev
```
The dev server proxies `/api` and `/ws` to `http://localhost:8090`. Set
`VSM_WEB_TARGET=http://host:port` to override.

## Production build
```
npm run build
```

The build writes directly into `src/main/resources/web-ui`. That will overwrite
the placeholder HTML in that folder.
