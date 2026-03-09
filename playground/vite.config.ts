import { defineConfig } from "vite";
import vue from "@vitejs/plugin-vue";
import tailwindcss from "@tailwindcss/vite";
import { fileURLToPath } from "url";
import path from "path";

export default defineConfig({
    plugins: [vue(), tailwindcss()],
    resolve: {
        alias: {
            "@": fileURLToPath(new URL("./src", import.meta.url)),
            "monaco-themes/themes": fileURLToPath(
                new URL("../node_modules/monaco-themes/themes", import.meta.url),
            ),
        },
    },
    // Ensure WASM files are included in the build output
    assetsInclude: ["**/*.wasm"],
});
