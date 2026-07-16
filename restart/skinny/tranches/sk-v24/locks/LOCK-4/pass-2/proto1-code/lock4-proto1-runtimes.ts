// LOCK-4 / pass-2 / PROTO-1 — three TS runtimes of ONE projection-dense
// value.js-shaped region: the rgb( n , n , n ) color triple (arity-7 all(),
// PAST the arity-3 unrolled fuseAll arms → the general `.parser`-IC loop is the
// honest baseline). All three expose the SAME shape: a raw ParserFunction
// (state)->state, driven over a fresh ParserState. This isolates the terminal
// execution face (no packrat / error-display wrapper pollutes any lane).
//
//   C  baseline   = today's all(...).map(project)   → fuseAll general arm,
//                    `parsers[i].parser(state)` property-IC per element.
//   B  emitted     = build-time-specialized closure via `new Function`; leaf
//                    RAW functions bound as lexical consts f0..f6, called BY
//                    LEXICAL NAME. Emitted source has ZERO `.parser(`.
//   A  vm          = switch-loop decoder over a msgpack-serialized bytecode
//                    program; regexes re-derived as sticky RegExp; runtime
//                    `switch(op)` dispatch — the SpanParser-refutation shape.

import { all, string, regex, Parser } from "../src/parse";
import { ParserState } from "../src/parse/state.js";

// ─────────────────────────────────────────────────────────────────────────────
// The region: leaves for  rgb( <num> , <num> , <num> )
// ─────────────────────────────────────────────────────────────────────────────
const NUM = /[0-9]+(?:\.[0-9]+)?/;
const COMMA = /\s*,\s*/;

const lp = string("rgb(");
const num = regex(NUM);
const comma = regex(COMMA);
const rp = string(")");

// The 7-arm ordered sequence (the general fuseAll arm — arity > 3).
const LEAVES: Parser<string>[] = [lp, num, comma, num, comma, num, rp];

// Projection: the map callback (identical for all three lanes). Values array is
// the drop-undefined result of the sequence; punctuation arms return strings, so
// out = ["rgb(", r, ",", g, ",", b, ")"] → pick numeric channels.
export function project(vals: unknown[]): { r: number; g: number; b: number } {
    return { r: +(vals[1] as string), g: +(vals[3] as string), b: +(vals[5] as string) };
}

export type PFn = (s: ParserState<unknown>) => ParserState<unknown>;

// ─────────────────────────────────────────────────────────────────────────────
// C — baseline: the REAL parse-that path (fuseAll general arm + map wrapper).
// ─────────────────────────────────────────────────────────────────────────────
const baselineParser = all(lp, num, comma, num, comma, num, rp).map(project as any);
export const C_fn: PFn = baselineParser.parser as unknown as PFn;

// ─────────────────────────────────────────────────────────────────────────────
// B — build-time-emitted specialized closure. We EMIT source text that unrolls
// the 7 arms calling f0..f6 BY LEXICAL NAME (never `.parser`), threading the
// exact drop-undefined + offset-restore semantics of fuseAll, then applies the
// bound map fn. The emitted source string is exported for the `.parser(`-grep.
// ─────────────────────────────────────────────────────────────────────────────
function emitSpecialized(leaves: Parser<unknown>[]): { fn: PFn; src: string } {
    const n = leaves.length;
    const lines: string[] = [];
    lines.push("return function fused(state){");
    lines.push("  var savedOffset = state.offset;");
    lines.push("  var w = 0;");
    lines.push("  var out = new Array(" + n + ");");
    for (let i = 0; i < n; i++) {
        lines.push("  f" + i + "(state);");
        lines.push("  if (state.isError) { state.offset = savedOffset; state.isError = true; return state; }");
        lines.push("  if (state.value !== undefined) out[w++] = state.value;");
    }
    lines.push("  if (w !== " + n + ") out.length = w;");
    lines.push("  return state.ok(mapfn(out));");
    lines.push("}");
    const src = lines.join("\n");

    // Bind the RAW leaf functions as lexical consts f0..f6 + the map fn.
    const argNames: string[] = [];
    const argVals: unknown[] = [];
    for (let i = 0; i < n; i++) {
        argNames.push("f" + i);
        argVals.push(leaves[i].parser); // captured ONCE at emit time — no per-call .parser load
    }
    argNames.push("mapfn");
    argVals.push(project);

    // eslint-disable-next-line @typescript-eslint/no-implied-eval
    const factory = new Function(...argNames, src);
    const fn = factory(...argVals) as PFn;
    return { fn, src };
}

const emitted = emitSpecialized(LEAVES);
export const B_fn: PFn = emitted.fn;
export const B_emittedSrc: string = emitted.src;

// ─────────────────────────────────────────────────────────────────────────────
// A — switch-loop VM over a msgpack-serialized bbnf_ir-shaped bytecode program.
// Op encoding (bbnf_ir leaf shape, to_vec_named-isomorphic): each op is a
// 2-tuple [tag, payload]. tag 0 = STRING (payload = literal), tag 1 = REGEX
// (payload = source). The program is a fixarray of these tuples. We serialize to
// msgpack bytes, then DECODE at load: STRING payloads kept as literals, REGEX
// payloads re-derived as sticky RegExp. VM runs a `for` + `switch(tag)`.
// ─────────────────────────────────────────────────────────────────────────────
const OP_STRING = 0;
const OP_REGEX = 1;

// --- minimal msgpack (subset: nil, fixint 0..127, fixstr, str8, fixarray) ----
function mpEncode(value: unknown, out: number[]): void {
    if (value === null || value === undefined) { out.push(0xc0); return; }
    if (typeof value === "number" && Number.isInteger(value) && value >= 0 && value <= 127) {
        out.push(value); return;
    }
    if (typeof value === "string") {
        const bytes = Array.from(new TextEncoder().encode(value));
        if (bytes.length < 32) out.push(0xa0 | bytes.length);
        else { out.push(0xd9); out.push(bytes.length & 0xff); }
        for (const b of bytes) out.push(b);
        return;
    }
    if (Array.isArray(value)) {
        if (value.length < 16) out.push(0x90 | value.length);
        else throw new Error("array too long for probe encoder");
        for (const el of value) mpEncode(el, out);
        return;
    }
    throw new Error("unsupported msgpack value in probe: " + String(value));
}

function mpDecode(buf: Uint8Array, posRef: { p: number }): unknown {
    const b = buf[posRef.p++];
    if (b === 0xc0) return null;
    if (b < 0x80) return b;                                   // fixint
    if (b >= 0xa0 && b <= 0xbf) {                             // fixstr
        const len = b & 0x1f; return readStr(buf, posRef, len);
    }
    if (b === 0xd9) { const len = buf[posRef.p++]; return readStr(buf, posRef, len); }
    if (b >= 0x90 && b <= 0x9f) {                             // fixarray
        const len = b & 0x0f; const arr: unknown[] = [];
        for (let i = 0; i < len; i++) arr.push(mpDecode(buf, posRef));
        return arr;
    }
    throw new Error("probe msgpack: unhandled byte 0x" + b.toString(16));
}
function readStr(buf: Uint8Array, posRef: { p: number }, len: number): string {
    const slice = buf.subarray(posRef.p, posRef.p + len); posRef.p += len;
    return new TextDecoder().decode(slice);
}

// Build the bytecode program from the SAME region (Rust-serialized bbnf_ir shape).
function buildProgramBytes(): Uint8Array {
    const program: unknown[] = [
        [OP_STRING, "rgb("],
        [OP_REGEX, NUM.source],
        [OP_REGEX, COMMA.source],
        [OP_REGEX, NUM.source],
        [OP_REGEX, COMMA.source],
        [OP_REGEX, NUM.source],
        [OP_STRING, ")"],
    ];
    const out: number[] = [];
    mpEncode(program, out);
    return Uint8Array.from(out);
}

interface DecodedOp {
    tag: number;
    lit: string;         // STRING literal
    litLen: number;
    re: RegExp | null;   // sticky RegExp for REGEX
}

function decodeProgram(bytes: Uint8Array): DecodedOp[] {
    const posRef = { p: 0 };
    const raw = mpDecode(bytes, posRef) as unknown[];
    return raw.map((opAny) => {
        const op = opAny as [number, string];
        const tag = op[0];
        if (tag === OP_STRING) {
            return { tag, lit: op[1], litLen: op[1].length, re: null };
        }
        // REGEX — re-derive as sticky
        return { tag, lit: "", litLen: 0, re: new RegExp(op[1], "y") };
    });
}

const PROGRAM_BYTES: Uint8Array = buildProgramBytes();
export const A_programByteLen = PROGRAM_BYTES.length;
const PROGRAM: DecodedOp[] = decodeProgram(PROGRAM_BYTES);

// The VM: `for` + `switch(tag)` runtime dispatch (the SpanParser-refutation shape).
function makeVm(program: DecodedOp[], mapfn: (v: unknown[]) => unknown): PFn {
    const n = program.length;
    return function vm(state: ParserState<unknown>): ParserState<unknown> {
        const savedOffset = state.offset;
        let w = 0;
        const out: unknown[] = new Array(n);
        const src = state.src;
        for (let i = 0; i < n; i++) {
            const op = program[i];
            switch (op.tag) {
                case OP_STRING: {
                    if (src.startsWith(op.lit, state.offset)) {
                        state.offset += op.litLen;
                        state.value = op.lit;
                    } else {
                        state.offset = savedOffset; state.isError = true; return state;
                    }
                    break;
                }
                case OP_REGEX: {
                    const re = op.re!;
                    re.lastIndex = state.offset;
                    const m = re.exec(src);
                    if (m !== null && m.index === state.offset) {
                        state.offset += m[0].length;
                        state.value = m[0];
                    } else {
                        state.offset = savedOffset; state.isError = true; return state;
                    }
                    break;
                }
                default:
                    state.isError = true; return state;
            }
            if (state.value !== undefined) out[w++] = state.value;
        }
        if (w !== n) out.length = w;
        return state.ok(mapfn(out));
    };
}

export const A_fn: PFn = makeVm(PROGRAM, project);

// ─────────────────────────────────────────────────────────────────────────────
// Common driver + corpus.
// ─────────────────────────────────────────────────────────────────────────────
export function drive(fn: PFn, src: string): unknown {
    const s = new ParserState(src);
    fn(s);
    return s.isError ? undefined : s.value;
}

export const CORPUS = "rgb(255, 128, 0)";
