/// <reference types="vite/client" />

declare module "*.vue" {
    import type { DefineComponent } from "vue";
    const component: DefineComponent<{}, {}, any>;
    export default component;
}

declare module "monaco-themes/themes/*.json" {
    const theme: any;
    export default theme;
}
