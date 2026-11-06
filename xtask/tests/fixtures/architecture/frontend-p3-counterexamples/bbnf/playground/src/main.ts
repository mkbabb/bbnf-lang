import target from "~/target";
export type { Shape } from "./types";
export const computed = import(`./${target}.ts`);
export const required = require("./required");
export const docs = import.meta.glob("@docs/**/*.md");
export const workerUrl = new URL("./worker.ts?url", import.meta.url);
export const notARoute = { path: "/false", component: target };
