// path.ts — TS frontend wrapper for the bbnf-path-ts cdylib.
//
// AZ-IV.W5.1. Mirrors the Rust `path!` proc-macro at the TS surface.
// Consumers write:
//
//     import init, { compile_path, execute_path } from "bbnf-path-ts";
//     import { makePathTag } from "./path-tag.js";
//     import { PathError } from "./path";
//
//     await init();
//     const path = makePathTag(compile_path, "Json");
//     const p = path`$.statuses[0].text`;
//     const leaf = execute_path(p, document);
//
// The runtime shape of `p` is `TypedPathPayload`, byte-identical
// (under serde-wasm-bindgen) to the Rust `bbnf::path::TypedPath::<G,
// ()>::owned_segments()` projection. The error shape is
// `PathErrorPayload`, byte-identical to `bbnf::path::PathError`.

/// Owned-form path segment. Mirrors `bbnf::path::OwnedPathSegment`.
export type OwnedPathSegment =
    | { kind: "Field"; value: string }
    | { kind: "Index"; value: number }
    | { kind: "Wildcard" }
    | { kind: "VariantName"; value: string };

/// Compile-time-typed path. Mirrors `bbnf::path::TypedPath::<G, ()>`.
///
/// The `grammar` field carries the marker name (`"Json"`, `"CssL4"`,
/// …); the `segments` field is the validated owned segment sequence.
export interface TypedPath {
    grammar: string;
    segments: OwnedPathSegment[];
}

/// Path-resolution failure reason. Mirrors `bbnf::path::PathErrorReason`.
export type PathErrorReason =
    | "UnknownField"
    | "UnknownVariant"
    | "IndexIntoNonList"
    | "FieldOnScalar"
    | "EmptyPath"
    | "UnregisteredRule"
    | "AmbiguousVariant"
    | "WildcardOverflow";

/// Path-resolution diagnostic. Mirrors `bbnf::path::PathError`.
///
/// The TS-side `PathError` class wraps this shape and presents the
/// fields through its `toString()` so the rendered message matches
/// the Rust frontend's `Display` output.
export interface PathErrorPayload {
    segment_index: number;
    segment_str: string;
    struct_name: string;
    alternatives: string[];
    reason: PathErrorReason;
}

/// TS-frontend `PathError` — JS `Error` subclass carrying the same
/// taxonomy fields as the Rust `bbnf::path::PathError`.
export class PathError extends Error {
    readonly segment_index: number;
    readonly segment_str: string;
    readonly struct_name: string;
    readonly alternatives: readonly string[];
    readonly reason: PathErrorReason;

    constructor(payload: PathErrorPayload) {
        super(PathError.format(payload));
        this.name = "PathError";
        this.segment_index = payload.segment_index;
        this.segment_str = payload.segment_str;
        this.struct_name = payload.struct_name;
        this.alternatives = payload.alternatives;
        this.reason = payload.reason;
    }

    /// Render a `PathErrorPayload` to a string matching the Rust
    /// `bbnf::path::PathError::Display` impl.
    static format(payload: PathErrorPayload): string {
        const head = `path[${payload.segment_index}] \`${payload.segment_str}\` on \`${payload.struct_name}\`: ${PathError.reasonText(payload.reason)}`;
        if (payload.alternatives.length === 0) {
            return head;
        }
        const alts = payload.alternatives.map((s) => `"${s}"`).join(", ");
        return `${head}; valid: [${alts}]`;
    }

    private static reasonText(reason: PathErrorReason): string {
        switch (reason) {
            case "UnknownField": return "unknown field";
            case "UnknownVariant": return "unknown variant";
            case "IndexIntoNonList": return "index into non-list";
            case "FieldOnScalar": return "field access on scalar";
            case "EmptyPath": return "empty path";
            case "UnregisteredRule": return "unregistered rule";
            case "AmbiguousVariant": return "ambiguous variant";
            case "WildcardOverflow": return "wildcard depth overflow";
        }
    }
}

/// Build a tagged-template-literal `path` tag bound to a grammar marker.
///
/// Mirrors the Rust `path!(GrammarMarker, ...)` macro at the TS
/// surface. The returned function accepts a tagged template literal
/// and returns the validated [`TypedPath`].
///
/// ```ts
/// const path = makePathTag(compile_path, "Json");
/// const p = path`$.statuses[0].text`;
/// ```
export function makePathTag(
    compileFn: (path: string, grammar: string) => TypedPath,
    grammar: string,
): (strings: TemplateStringsArray, ...values: unknown[]) => TypedPath {
    return function pathTag(
        strings: TemplateStringsArray,
        ...values: unknown[]
    ): TypedPath {
        const pathStr = strings.raw.reduce<string>((acc, str, i) => {
            const interp = i < values.length ? String(values[i]) : "";
            return acc + str + interp;
        }, "");
        try {
            return compileFn(pathStr, grammar);
        } catch (raw: unknown) {
            // wasm-bindgen wraps the JsError message — the message body
            // is the JSON-serialized `PathErrorPayload`. Round-trip it
            // back into a typed `PathError` so callers get the same
            // taxonomy the Rust frontend surfaces through `syn::Error`.
            const message = raw instanceof Error ? raw.message : String(raw);
            try {
                const payload = JSON.parse(message) as PathErrorPayload;
                throw new PathError(payload);
            } catch (_: unknown) {
                throw raw;
            }
        }
    };
}
