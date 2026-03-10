import { defineConfig } from "vite";
import dts from "vite-plugin-dts";

export default defineConfig({
    base: "./",

    build: {
        minify: false,
        sourcemap: true,
        lib: {
            entry: {
                bbnf: "./src/index.ts",
            },
            formats: ["es", "cjs"],
        },
        rollupOptions: {
            external: ["@mkbabb/parse-that", "@mkbabb/pprint"],
        },
    },

    plugins: [dts()],
});
