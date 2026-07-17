import { defineConfig } from "tsup";
import vuePlugin from "esbuild-plugin-vue3";

export default defineConfig([
    // Non-Vue entries: standard tsup with DTS
    {
        entry: {
            index: "src/index.ts",
            transform: "src/transform.ts",
            vite: "src/vite.ts",
        },
        format: ["esm"],
        dts: true,
        sourcemap: true,
        clean: true,
        external: ["katex", "vite"],
        loader: {
            ".bbnf": "text",
        },
    },
    // Vue entry: esbuild compiles SFCs, DTS generated separately via vue-tsc
    {
        entry: {
            vue: "src/vue/index.ts",
        },
        format: ["esm"],
        dts: false,
        sourcemap: true,
        clean: false,
        external: ["katex", "vue"],
        esbuildPlugins: [vuePlugin()],
    },
]);
