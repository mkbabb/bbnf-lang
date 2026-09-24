// SERVED MODEL: claude-opus-5-5
//
// compile.ts — runtime `compile()`: a grammar known only at runtime goes through THE emitter
// (`emit.ts`) and its text is evaluated once. The answer is the object `createParser` returns from
// the build-time module `bbnf gen` writes for the same grammar: one semantics, no interpreter.

import type { AST, RecoverDirective } from "./types.js";
import { emitGrammar, FAIL } from "./emit.js";
import type { ActionKind, Emission } from "./emit.js";

/** An action: its kind decides what it receives (VALUE-SEMANTICS.md; README "Actions"). */
export type Action =
    | Readonly<{ kind: "map"; fn: (value: any) => unknown }>
    | Readonly<{ kind: "span"; fn: (value: any, start: number, end: number) => unknown }>
    | Readonly<{ kind: "text"; fn: (text: string) => unknown }>;

/** A rule the host supplies: matches at `offset`, answers the end (or -1), writes its value to `box.v`. */
export type HostRule = (source: string, offset: number, box: { v: unknown }) => number;

export type CompileOptions = Readonly<{
    actions?: Readonly<Record<string, Action>>;
    /** Rules the grammar leaves to the host (they must not be defined by the grammar). */
    host?: Readonly<Record<string, HostRule>>;
    /** Entry rules (default: every rule). */
    entries?: readonly string[];
    recovers?: readonly RecoverDirective[];
    /** Back-edge nesting limit (`0`: no depth fault). */
    maxDepth?: number;
    /** Evidence builds only: the routing/guard audit (see `EmitOptions.audit`). */
    audit?: Audit;
}>;

export type Compiled = Readonly<{
    /** Entry rule over a whole input: its value, or `FAIL`. */
    entries: Readonly<Record<string, (source: string) => unknown>>;
    /** Entry rule at an offset: the end of its match or -1; the value is `value()`. */
    rules: Readonly<Record<string, (source: string, offset: number) => number>>;
    value(): unknown;
    /** With `@recover` directives: the spans the last call recovered, flat (rule index, start, end). */
    recovered?: () => readonly number[];
    /** Audit builds only: every rule in recognize mode. */
    recognize?: Readonly<Record<string, (source: string, offset: number) => number>>;
    emission: Emission;
}>;

/** The routing audit's tally (evidence builds only). */
export type Audit = {
    checks: number;
    violations: number;
    samples: string[];
    check(fast: number, slow: number, label: string, source: string, offset: number): void;
};

export function createAudit(): Audit {
    return {
        checks: 0, violations: 0, samples: [],
        check(fast, slow, label, source, offset) {
            this.checks++;
            if (fast === slow) return;
            this.violations++;
            if (this.samples.length < 20) this.samples.push(`${label}: fast ${fast} vs plain ${slow} @${offset} in ${JSON.stringify(source.slice(0, 80))}`);
        },
    };
}

/** Compiles `ast` through the emitter and evaluates it once. */
export function compile(ast: AST, options: CompileOptions = {}): Compiled {
    const actions = options.actions ?? {};
    const actionKinds: Record<string, ActionKind> = {};
    for (const [name, a] of Object.entries(actions)) actionKinds[name] = a.kind;
    const emission = emitGrammar(ast, {
        actionKinds,
        entries: options.entries,
        host: Object.keys(options.host ?? {}),
        recovers: options.recovers,
        maxDepth: options.maxDepth,
        audit: options.audit !== undefined,
    });
    const make = new Function("A", "H", "ACTION_KINDS", "FAIL", "AUDIT", emission.body) as (
        a: unknown, h: unknown, kinds: unknown, fail: symbol, audit: Audit | undefined) => Omit<Compiled, "emission">;
    return { ...make(actions, options.host ?? {}, emission.actionKinds, FAIL, options.audit), emission };
}
