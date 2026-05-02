// index.ts — public TS surface for the bbnf-path-ts cdylib.
//
// AZ-IV.W5.1. Re-exports the `TypedPath`, `PathError`, and
// `makePathTag` symbols. Consumers import from this module:
//
//     import init, { compile_path, execute_path } from "bbnf-path-ts";
//     import { makePathTag, PathError, TypedPath } from "bbnf-path-ts/path";
//
// `init`, `compile_path`, `execute_path` come from the wasm-bindgen
// generated module (the cdylib's JS shim); the symbols re-exported
// here come from the TS-side wrapper at `path.ts`.

export {
    PathError,
    makePathTag,
} from "./path";

export type {
    OwnedPathSegment,
    PathErrorPayload,
    PathErrorReason,
    TypedPath,
} from "./path";
