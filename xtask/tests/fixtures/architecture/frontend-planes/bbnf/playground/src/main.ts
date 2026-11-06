import "./phantom.js";
import "./styles.css";
import "./glue.js";

export const docs = import.meta.glob("@docs/*.md");
export const worker = new Worker(new URL("./worker.ts", import.meta.url), { type: "module" });
