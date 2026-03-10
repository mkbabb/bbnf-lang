import { defineConfig } from "vite";
import vue from "@vitejs/plugin-vue";
import tailwindcss from "@tailwindcss/vite";
import { fileURLToPath } from "url";

export default defineConfig({
    plugins: [vue(), tailwindcss()],
    resolve: {
        alias: {
            "@": fileURLToPath(new URL("./src", import.meta.url)),
            "@mkbabb/keyframes.js": fileURLToPath(
                new URL("../../keyframes.js/dist/keyframes.js", import.meta.url),
            ),
            "monaco-themes/themes": fileURLToPath(
                new URL("../node_modules/monaco-themes/themes", import.meta.url),
            ),
        },
    },
    server: {
        port: 5600,
    },
    // Ensure WASM files are included in the build output
    assetsInclude: ["**/*.wasm"],
});
