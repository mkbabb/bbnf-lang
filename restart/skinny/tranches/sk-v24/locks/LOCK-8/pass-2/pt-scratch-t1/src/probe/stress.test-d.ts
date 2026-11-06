/**
 * LOCK-8 pass-2 probe t1 — 8B typed-algebra instantiation-budget stress corpus.
 *
 * Reproduces latex-paper's DOMINANT real type shapes (R4 §7 / FAMILY-SPECS 8B):
 *   (a) deep `.then()` chains — commands.ts reaches 12 `.then(`;
 *   (b) mutually-recursive grammar via `lazy(() => …): Parser<T>` seams;
 *   (c) wide `all(...)` sites, several with `opt()` members — the DropUndefined crux;
 *   (d) `.or()`/`.chain()`/`.skip()`/`.sepBy()` at the same nominal `Parser<Node>` seam.
 *
 * Every rule is annotated `const x: Parser<Node>` (the structural recursion cap the
 * spec relies on). The `Expect`/`Equal` assertions are the precision-both-ways gate.
 */
import { Parser, string, regex, all, any } from "../parse/index.js";

// ── type-level equality (invariant Equal — the standard trick) ───────────────
type Equal<A, B> =
    (<T>() => T extends A ? 1 : 2) extends (<T>() => T extends B ? 1 : 2) ? true : false;
type Expect<T extends true> = T;

// ── leaves ───────────────────────────────────────────────────────────────────
const ident = regex(/[A-Za-z_][A-Za-z0-9_]*/);
const num = regex(/-?\d+(\.\d+)?/).map(Number);
const ws = regex(/\s*/);
const lbrace = string("{");
const rbrace = string("}");
const lbrack = string("[");
const rbrack = string("]");

// ── (a) DEEP .then() chain — 12 deep (latex commands.ts dominant) ────────────
const deep12 = ident
    .then(ws)
    .then(lbrace)
    .then(ident)
    .then(ws)
    .then(rbrace)
    .then(lbrack)
    .then(num)
    .then(rbrack)
    .then(ident)
    .then(ws)
    .then(num);
// 12-deep .then builds 12 nested 2-tuples; assert the fully-nested shape resolves.
type Deep12 = typeof deep12 extends Parser<infer V> ? V : never;
type _t_deep = Expect<Equal<Deep12,
    [[[[[[[[[[[string, string], string], string], string], string], string], number], string], string], string], number]>>;

// ── (c) wide all() sites, WITH opt() members (DropUndefined crux) ────────────
// all(a, opt(b), c): runtime tuple length varies with b's presence.
const wideOpt = all(ident, num.opt(), ident, num.opt(), ident);
type WideOpt = typeof wideOpt extends Parser<infer V> ? V : never;
// SPEC target (tightened `all → Parser<DropUndefined<Values<Ps>>>`):
//   drop the `undefined` arms → [string, number, string, number, string].
// BASELINE (current `ExtractValue`, no DropUndefined):
//   [string, number | undefined, string, number | undefined, string].
type _t_wideopt_TIGHTENED = Expect<Equal<WideOpt, [string, number, string, number, string]>>;

const wide8 = all(ident, num, ident, num, ident, num, ident, num);
type Wide8 = typeof wide8 extends Parser<infer V> ? V : never;
type _t_wide8 = Expect<Equal<Wide8, [string, number, string, number, string, number, string, number]>>;

// ── (b) mutually-recursive grammar via lazy() at the Parser<Node> seam ───────
type Node =
    | { tag: "num"; v: number }
    | { tag: "list"; items: Node[] };

const numNode: Parser<Node> = num.map((v) => ({ tag: "num" as const, v }));
const listNode: Parser<Node> = Parser.lazy(() =>
    expr.sepBy(ws).trim().wrap(lbrack, rbrack).map((items) => ({ tag: "list" as const, items }))
);
const expr: Parser<Node> = Parser.lazy(() => numNode.or(listNode));
type ExprV = typeof expr extends Parser<infer V> ? V : never;
type _t_expr = Expect<Equal<ExprV, Node>>;

// ── (d) or / chain / skip precision (the tightened input-leak removal) ────────
// or<S>: result should be T | S.  ident.or(num) → string | number.
const orMix = ident.or(num);
type OrMix = typeof orMix extends Parser<infer V> ? V : never;
type _t_or = Expect<Equal<OrMix, string | number>>;

// chain<S>: result should be S (the continuation's value), NOT S | T.
const chained = ident.chain((name) => (name.length > 2 ? num : num));
type Chained = typeof chained extends Parser<infer V> ? V : never;
type _t_chain = Expect<Equal<Chained, number>>;

// opt<>: T | undefined.
const optNum = num.opt();
type OptNum = typeof optNum extends Parser<infer V> ? V : never;
type _t_opt = Expect<Equal<OptNum, number | undefined>>;

// then<S>: [T, S], NOT [T, S|T].
const pair = ident.then(num);
type Pair = typeof pair extends Parser<infer V> ? V : never;
type _t_then = Expect<Equal<Pair, [string, number]>>;

// ── precision BOTH ways: a genuinely WRONG shape MUST still error (gate 8B-#3) ─
// @ts-expect-error — orMix is `string | number`, NOT `string`; the checker must reject.
type _neg_or = Expect<Equal<OrMix, string>>;
// @ts-expect-error — pair is `[string, number]`, NOT `[number, string]`; must reject.
type _neg_then = Expect<Equal<Pair, [number, string]>>;

// keep the values referenced so the file is not dead-code-eliminated pre-check
export const _corpus = { deep12, wideOpt, wide8, expr, orMix, chained, optNum, pair, any };
export type { _neg_or, _neg_then };
