import { defineConfig } from "vite";
import { svelte } from "@sveltejs/vite-plugin-svelte";
import { resolve } from "path";

const apiTarget = process.env.VSM_WEB_TARGET || "http://localhost:8090";
const wsTarget = apiTarget.replace(/^http/, "ws");

export default defineConfig({
  plugins: [svelte()],
  base: "/",
  build: {
    outDir: resolve(__dirname, "../src/main/resources/web-ui"),
    emptyOutDir: true
  },
  server: {
    proxy: {
      "/api": apiTarget,
      "/ws": {
        target: wsTarget,
        ws: true
      }
    }
  }
});
