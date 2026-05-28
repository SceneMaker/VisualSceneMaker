import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  plugins: [
    react({
      include: /\.(j|t)sx?$/
    })
  ],
  build: {
    outDir: "build",
    emptyOutDir: true
  },
  server: {
    proxy: {
      "/ws": {
        target: "ws://localhost:8080",
        ws: true
      }
    }
  }
});
