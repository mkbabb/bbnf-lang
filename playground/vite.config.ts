import { defineConfig, type Plugin } from "vite";
import vue from "@vitejs/plugin-vue";
import tailwindcss from "@tailwindcss/vite";
import { fileURLToPath } from "url";
import { copyFileSync } from "fs";
import { resolve } from "path";

/** Copy index.html → 404.html after build for SPA fallback routing. */
function spaFallback(): Plugin {
    return {
        name: "spa-fallback",
        closeBundle() {
            const dist = resolve(__dirname, "dist");
            copyFileSync(resolve(dist, "index.html"), resolve(dist, "404.html"));
        },
    };
}

export default defineConfig({
    plugins: [vue(), tailwindcss(), spaFallback()],
    resolve: {
        alias: {
            "@": fileURLToPath(new URL("./src", import.meta.url)),
            "@mkbabb/keyframes.js": fileURLToPath(
                new URL("../node_modules/@mkbabb/keyframes.js/dist/keyframes.js", import.meta.url),
            ),
            "monaco-themes/themes": fileURLToPath(
                new URL("../node_modules/monaco-themes/themes", import.meta.url),
            ),
            "@docs": fileURLToPath(new URL("../docs", import.meta.url)),
            "@grammars": fileURLToPath(new URL("../grammar", import.meta.url)),
        },
    },
    server: {
        port: 5600,
    },
    // Ensure WASM files are included in the build output
    assetsInclude: ["**/*.wasm"],
});
