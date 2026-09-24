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
                // node only (build time): the generator and the `bbnf` CLI
                gen: "./src/gen.ts",
                cli: "./src/cli.ts",
            },
            formats: ["es", "cjs"],
        },
        rollupOptions: {
            external: ["@mkbabb/parse-that", "@mkbabb/pprint", /^node:/],
        },
    },

    plugins: [dts()],
});
