// SERVED MODEL: claude-opus-5-5
//
// emit.ts — THE emitter: a BBNF grammar becomes JavaScript source, one function per rule and mode.
// Every face compiles through it: `bbnf gen` writes its output as an ES module (+ `.d.ts`) at build
// time; runtime `compile()` evaluates the same text; the parse-that façade wraps its rules. One
// semantics (VALUE-SEMANTICS.md), no second interpreter.
//
// Staging (value.js X.P.W7 research, route-ts-compiler `src/emit.ts`, the ratified route):
//   · two modes per rule: VALUE builds the rule's value and runs its action; RECOGNIZE answers only
//     where the match ends — used wherever the grammar discards a value (`a >> b`'s `a`, `a << b`'s
//     `b`, `a - b`'s `b`, a `text` action's body). Actions are pure and total, so a discarded subtree
//     never runs them;
//   · an ordered choice routed by the one analysis (`analysis/first.ts`) is a `switch` on the first
//     code unit (ASCII table, one non-ASCII route, the end-of-input route); an alternative that is
//     not a plain leaf or reference is hoisted into its own function, so routes share its code;
//   · a single-class run (`[^()"']+`, `\s*`) is a code-unit loop; a non-nullable regex leaf, `p?`
//     and `p*` test the first unit before entering the regex engine or the call;
//   · a reference is a DIRECT call to the referenced rule's function (no trampoline, no table);
//   · the nesting-depth fault: a counter on the recursive back-edges alone (a DFS over the rule
//     graph marks one edge on every cycle). Beyond `maxDepth` the call answers failure, the parse
//     trips, and the entry answers FAIL: a deep input is refused, never a `RangeError`.
// The value register `V` is shared by the functions of one parser: the parser is not re-entrant,
// and an action never calls a parse entry (see README "Actions").

import type { AST, Expression, RecoverDirective } from "./types.js";
import type { Info } from "./analysis/first.js";
import { analyzeFirst, routes } from "./analysis/first.js";
import { singleClassRun } from "./analysis/regex.js";
import { collectDependencies } from "./analysis/deps.js";

/**
 * How a rule's action receives its match: the value; the value and its span; the text alone; or, for a
 * rule whose body is one regex leaf with capturing groups, the groups the match captured (`fn(...groups)`,
 * an unmatched group `undefined`), so the action never re-splits the text with a second regex.
 */
export type ActionKind = "map" | "span" | "text" | "groups";

/** The number of capturing groups in `re`. */
const groupCount = (re: RegExp): number => new RegExp(`${re.source}|`, re.flags.replace(/[gy]/g, "")).exec("")!.length - 1;

export type EmitOptions = Readonly<{
    /** The rules that carry an action, and each action's kind (read from the consumer's table). */
    actionKinds?: Readonly<Record<string, ActionKind>>;
    /** The entry rules: each gets `entries[name](source)` → value | FAIL. Default: every rule. */
    entries?: readonly string[];
    /** Rules the host supplies (`H[name](s, i, box)` → end, value in `box.v`); undefined in `ast`. */
    host?: readonly string[];
    /** `@recover` directives: a failing rule answers `null` where its sync expression matches. */
    recovers?: readonly RecoverDirective[];
    /** Back-edge nesting limit; `0` emits no depth fault. Default `DEFAULT_MAX_DEPTH`. */
    maxDepth?: number;
    /** Lines prepended to the module and the `.d.ts` (the generator's DO-NOT-EDIT header). */
    header?: string;
    /**
     * Evidence builds only: every routed choice is re-run as the plain ordered choice and every
     * first-unit guard that rejects is re-run unguarded, counting disagreements in `AUDIT`; every
     * rule is also emitted in recognize mode (`recognize[name]`) for the mode differential.
     */
    audit?: boolean;
    /**
     * Evidence builds only: the alternatives a non-ASCII first unit is routed to, per ordered choice
     * (`null` keeps the grammar's). `test/stock-ascii.test.ts` passes bbnf-lang 0.1.4's ASCII-only
     * dispatch here to prove the one analysis differs from 0.1.4 only in the F-b-4 rows; a shipping
     * build never sets it (COHESION §0ck 1: stock-ASCII routing is never reproduced).
     */
    nonAsciiRoute?: (alternatives: readonly Expression[]) => readonly number[] | null;
}>;

export type Emission = Readonly<{
    /** The body of `createParser(A, H)`; free names `ACTION_KINDS`, `FAIL`. */
    body: string;
    /** The ES module: `RULE_NAMES`, `ENTRY_NAMES`, `ACTION_KINDS`, `FAIL`, `createParser`. */
    module: string;
    /** Its declarations: the typed `Actions` table and the parser's shape. */
    dts: string;
    ruleNames: readonly string[];
    entryNames: readonly string[];
    actionKinds: Readonly<Record<string, ActionKind>>;
    hostNames: readonly string[];
}>;

/**
 * The default back-edge nesting limit, set below the smallest engine's measured throw point with
 * margin (README "Nesting depth"). Measured 2026-09-23/24 on value.js's grammar, 13 nesting shapes,
 * a fresh main thread: V8 throws at 881–934 back-edges at the least (node 26, Chromium 148),
 * JavaScriptCore at 4,970–5,047 (WebKit 26.4). 256 is under 0.3 of V8's.
 */
export const DEFAULT_MAX_DEPTH = 256;

/** The key of the FAIL sentinel every face shares (`Symbol.for`, so modules agree without imports). */
export const FAIL_KEY = "@mkbabb/bbnf-lang/FAIL";
/** What an entry answers when the input is refused. */
export const FAIL: unique symbol = Symbol.for(FAIL_KEY) as never;

type Mode = "v" | "r";
const unwrap = (e: Expression): Expression => (e.type === "group" ? unwrap(e.value as Expression) : e);

/** Each rule's references to rules of the grammar (its body and its `@recover` sync expressions). */
function ruleDeps(ast: AST, recovers: readonly RecoverDirective[]): Map<string, string[]> {
    const deps = new Map<string, string[]>();
    for (const [name, rule] of ast) {
        const d = new Set<string>();
        collectDependencies(rule.expression, d);
        for (const r of recovers) if (r.ruleName === name) collectDependencies(r.syncExpr, d);
        deps.set(name, [...d].filter((n) => ast.has(n)));
    }
    return deps;
}

/** One edge `from→to` on every cycle of the rule graph: the back-edges of a DFS in rule order. */
export function backEdges(ast: AST, recovers: readonly RecoverDirective[] = []): Set<string> {
    const deps = ruleDeps(ast, recovers);
    const state = new Map<string, 1 | 2>(); // 1 on the stack, 2 done
    const out = new Set<string>();
    for (const root of ast.keys()) {
        if (state.has(root)) continue;
        const stack: [string, number][] = [[root, 0]];
        state.set(root, 1);
        while (stack.length > 0) {
            const top = stack[stack.length - 1];
            const next = deps.get(top[0])![top[1]++];
            if (next === undefined) { state.set(top[0], 2); stack.pop(); continue; }
            const st = state.get(next);
            if (st === 1) out.add(`${top[0]}\u0000${next}`);
            else if (st === undefined) { state.set(next, 1); stack.push([next, 0]); }
        }
    }
    return out;
}

/** A constant as JavaScript source (typed tables, sticky regexes). */
function literalOf(v: unknown): string {
    if (v instanceof Uint8Array) return `new Uint8Array([${Array.from(v).join(",")}])`;
    if (v instanceof Int16Array) return `new Int16Array([${Array.from(v).join(",")}])`;
    if (v instanceof RegExp) return `new RegExp(${JSON.stringify(v.source)}, ${JSON.stringify(v.flags)})`;
    throw new Error(`emit: unserializable constant ${String(v)}`);
}

/** Emits `ast` as the body of `createParser(A, H)` plus its module and declarations. */
export function emitGrammar(ast: AST, opts: EmitOptions = {}): Emission {
    const actionKinds: Record<string, ActionKind> = {};
    for (const [name, kind] of Object.entries(opts.actionKinds ?? {})) {
        if (!ast.has(name)) throw new Error(`emit: action for unknown rule \`${name}\``);
        if (kind !== "map" && kind !== "span" && kind !== "text" && kind !== "groups") throw new Error(`emit: \`${name}\` has action kind \`${String(kind)}\``);
        if (kind === "groups") {
            const leaf = unwrap(ast.get(name)!.expression);
            if (leaf.type !== "regex" || groupCount(leaf.value as RegExp) === 0) {
                throw new Error(`emit: \`${name}\` has a groups action, so its body must be one regex leaf with a capturing group`);
            }
        }
    }
    for (const name of ast.keys()) { const k = opts.actionKinds?.[name]; if (k !== undefined) actionKinds[name] = k; }
    const hostNames = [...(opts.host ?? [])];
    const host = new Set(hostNames);
    for (const h of hostNames) if (ast.has(h)) throw new Error(`emit: host rule \`${h}\` is also defined by the grammar`);
    const recovers = new Map<string, Expression>();
    for (const r of opts.recovers ?? []) if (ast.has(r.ruleName)) recovers.set(r.ruleName, r.syncExpr);
    const ruleNames = [...ast.keys()];
    const entryNames = [...(opts.entries ?? ruleNames)];
    for (const e of entryNames) if (!ast.has(e)) throw new Error(`emit: entry \`${e}\` is not a rule`);
    const maxDepth = opts.maxDepth ?? DEFAULT_MAX_DEPTH;
    if (!Number.isInteger(maxDepth) || maxDepth < 0) throw new Error(`emit: maxDepth ${maxDepth}`);
    const depth = maxDepth > 0;
    const back = depth ? backEdges(ast, opts.recovers) : new Set<string>();
    // An entry that reaches no back-edge cannot nest: it carries no depth reset or check at all.
    const deps = ruleDeps(ast, opts.recovers ?? []);
    const backFrom = new Set([...back].map((edge) => edge.slice(0, edge.indexOf("\u0000"))));
    const nests = (entry: string): boolean => {
        const seen = new Set([entry]), stack = [entry];
        while (stack.length > 0) {
            const r = stack.pop()!;
            if (backFrom.has(r)) return true;
            for (const d of deps.get(r)!) if (!seen.has(d)) { seen.add(d); stack.push(d); }
        }
        return false;
    };
    const { info } = analyzeFirst(ast);
    // With `@recover` directives, recovered spans (`RC`, flat triples rule·start·end) are rolled back
    // wherever the parse backtracks, exactly as parse-that rolls its diagnostics back.
    const hasRec = recovers.size > 0;
    const rcSave = (v: string) => (hasRec ? `const ${v} = RC.length; ` : "");
    const rcBack = (v: string) => (hasRec ? `RC.length = ${v}; ` : "");

    // Constants, interned: equal tables and equal regexes share one binding.
    const consts: string[] = [];
    const constKey = new Map<string, string>();
    const constName = (v: unknown): string => {
        const src = literalOf(v);
        let n = constKey.get(src);
        if (n === undefined) { n = `K${consts.length}`; consts.push(src); constKey.set(src, n); }
        return n;
    };
    const hostName = (name: string) => `H${hostNames.indexOf(name)}`;
    let tmpN = 0;
    const tmp = (p: string) => `${p}${tmpN++}`;
    const fnName = new Map<string, string>(); // `${rule}/${mode}` → function name
    const queue: [string, Mode][] = [];
    const ruleFn = (name: string, mode: Mode): string => {
        const key = `${name}/${mode}`;
        let f = fnName.get(key);
        if (f === undefined) {
            f = `r${ruleNames.indexOf(name)}_${name.replace(/\W/g, "_")}_${mode}`;
            fnName.set(key, f);
            queue.push([name, mode]);
        }
        return f;
    };
    const helpers: string[] = [];
    const actNames: string[] = [];

    /** A first-unit test of `c` against `inf` (true = may start). */
    const mayStart = (c: string, inf: Info): string =>
        `(${c} < 128 ? ${constName(inf.first.ascii)}[${c}] === 1 : ${inf.first.nonAscii ? `${c} === ${c}` : "false"})`;

    /** Skips ASCII whitespace (9–13, 32) from `from` into the fresh binding `to`. */
    const skipWs = (from: string, to: string): string => {
        const c = tmp("c");
        return `let ${to} = ${from}; for (; ${to} < s.length; ${to}++) { const ${c} = s.charCodeAt(${to}); if (${c} !== 32 && (${c} < 9 || ${c} > 13)) break; }\n`;
    };

    /**
     * The regex leaf of the `groups` rule being emitted in value mode: that one leaf runs `exec` and
     * leaves its match array in `V`, which the rule's action line spreads into the action's groups.
     */
    let groupsLeaf: Expression | null = null;

    /** Emits code that sets `out` to the end offset (or -1) of `e` matched at `pos`, in `mode`. */
    function gen(e: Expression, mode: Mode, pos: string, out: string, rn: string): string {
        const keep = mode === "v";
        switch (e.type) {
            case "literal": {
                const str = e.value as string;
                if (str.length === 0) return `${keep ? `V = ""; ` : ""}${out} = ${pos};\n`;
                const c0 = str.charCodeAt(0);
                const test = str.length === 1 ? `s.charCodeAt(${pos}) === ${c0}` : `s.charCodeAt(${pos}) === ${c0} && s.startsWith(${JSON.stringify(str)}, ${pos})`;
                return `if (${test}) { ${keep ? `V = ${JSON.stringify(str)}; ` : ""}${out} = ${pos} + ${str.length}; } else ${out} = -1;\n`;
            }
            case "regex": {
                // parse-that 2.x (F-p-EOF): end of input is an ordinary position; an empty match is `undefined`.
                const re = e.value as RegExp;
                if (keep && e === groupsLeaf) {
                    const R = constName(new RegExp(re.source, re.flags.replace(/[gy]/g, "") + "y"));
                    const inf = info(e), m = tmp("m");
                    const match = `${R}.lastIndex = ${pos}; const ${m} = ${R}.exec(s); if (${m} !== null) { ${out} = ${R}.lastIndex; V = ${m}; } else ${out} = -1;`;
                    if (inf.nullable) return `{ ${match} }\n`;
                    const c = tmp("c");
                    const audit = opts.audit ? `if (!${mayStart(c, inf)}) { ${R}.lastIndex = ${pos}; AUDIT.check(${out}, ${R}.test(s) ? ${R}.lastIndex : -1, ${JSON.stringify(`guard /${re.source}/${re.flags} in ${rn}`)}, s, ${pos}); }\n` : "";
                    return `{ const ${c} = s.charCodeAt(${pos}); if (!${mayStart(c, inf)}) ${out} = -1; else { ${match} }\n  ${audit}}\n`;
                }
                const run = singleClassRun(re);
                if (run !== null && run.all) {
                    const miss = run.min === 1 ? `if (${pos} >= s.length) ${out} = -1; else ` : "";
                    return `${miss}{ ${keep ? `V = ${pos} < s.length ? s.substring(${pos}) : undefined; ` : ""}${out} = s.length; }\n`;
                }
                if (run !== null) {
                    const T = constName(run.tbl), C = constName(new RegExp(run.cls.source, run.cls.flags + "y"));
                    const q = tmp("q"), c = tmp("c");
                    return `{ let ${q} = ${pos};
  for (; ${q} < s.length; ${q}++) { const ${c} = s.charCodeAt(${q}); if (${c} < 128) { if (${T}[${c}] === 0) break; } else { ${C}.lastIndex = ${q}; if (!${C}.test(s)) break; } }
  if (${q} - ${pos} < ${run.min}) ${out} = -1; else { ${keep ? `V = ${q} > ${pos} ? s.substring(${pos}, ${q}) : undefined; ` : ""}${out} = ${q}; } }\n`;
                }
                const R = constName(new RegExp(re.source, re.flags.replace(/[gy]/g, "") + "y"));
                const inf = info(e);
                if (!inf.nullable) {
                    const c = tmp("c");
                    const audit = opts.audit ? `if (!${mayStart(c, inf)}) { ${R}.lastIndex = ${pos}; AUDIT.check(${out}, ${R}.test(s) ? ${R}.lastIndex : -1, ${JSON.stringify(`guard /${re.source}/${re.flags} in ${rn}`)}, s, ${pos}); }\n` : "";
                    return `{ const ${c} = s.charCodeAt(${pos});
  if (!${mayStart(c, inf)}) ${out} = -1;
  else { ${R}.lastIndex = ${pos}; if (${R}.test(s)) { ${out} = ${R}.lastIndex; ${keep ? `V = s.substring(${pos}, ${out}); ` : ""}} else ${out} = -1; }
  ${audit}}\n`;
                }
                return `{ ${R}.lastIndex = ${pos}; if (${R}.test(s)) { ${out} = ${R}.lastIndex; ${keep ? `V = ${out} > ${pos} ? s.substring(${pos}, ${out}) : undefined; ` : ""}} else ${out} = -1; }\n`;
            }
            case "nonterminal": {
                const target = e.value as string;
                if (!ast.has(target)) {
                    if (!host.has(target)) throw new Error(`emit: undefined rule \`${target}\` (in \`${rn}\`)`);
                    return `${out} = ${hostName(target)}(s, ${pos}, HB);${keep ? ` if (${out} >= 0) V = HB.v;` : ""}\n`;
                }
                const call = `${ruleFn(target, mode)}(s, ${pos})`;
                if (!back.has(`${rn}\u0000${target}`)) return `${out} = ${call};\n`;
                return `if (D >= ${maxDepth}) { D = TRIP; ${out} = -1; } else { D++; ${out} = ${call}; D--; }\n`;
            }
            case "group": return gen(e.value as Expression, mode, pos, out, rn);
            case "epsilon": return `${keep ? "V = undefined; " : ""}${out} = ${pos};\n`;
            case "optionalWhitespace": {
                const a = tmp("w"), t = tmp("t"), b = tmp("w");
                return `{ ${skipWs(pos, a)}let ${t}; ${gen(e.value as unknown as Expression, mode, a, t, rn)}if (${t} < 0) ${out} = -1; else { ${skipWs(t, b)}${out} = ${b}; } }\n`;
            }
            case "optional": {
                const inner = e.value as Expression, inf = info(inner), t = tmp("t");
                const miss = `${keep ? "V = undefined; " : ""}${out} = ${pos};`;
                const rl = tmp("rl");
                const body = `{ ${rcSave(rl)}let ${t}; ${gen(inner, mode, pos, t, rn)}if (${t} < 0) { ${rcBack(rl)}${miss} } else ${out} = ${t}; }`;
                if (inf.nullable) return body + "\n";
                const c = tmp("c");
                let audit = "";
                if (opts.audit) {
                    const u = tmp("t"), v = tmp("v");
                    audit = `if (!${mayStart(c, inf)}) { const ${v} = V; let ${u}; ${gen(inner, mode, pos, u, rn)}AUDIT.check(${out}, ${u} < 0 ? ${pos} : ${u}, ${JSON.stringify(`guard ? in ${rn}`)}, s, ${pos}); V = ${v}; }\n`;
                }
                return `{ const ${c} = s.charCodeAt(${pos}); if (!${mayStart(c, inf)}) { ${miss} } else ${body}\n${audit}}\n`;
            }
            case "many": case "many1": {
                const inner = e.value as Expression, inf = info(inner), min = e.type === "many1" ? 1 : 0;
                const q = tmp("q"), t = tmp("t"), n = tmp("n"), arr = tmp("a"), c = tmp("c"), rl = tmp("rl");
                let guard = inf.nullable ? "" : `const ${c} = s.charCodeAt(${q}); if (!${mayStart(c, inf)}) break; `;
                if (opts.audit && !inf.nullable) {
                    const u = tmp("t"), v = tmp("v");
                    guard = `const ${c} = s.charCodeAt(${q}); if (!${mayStart(c, inf)}) { const ${v} = V; let ${u}; ${gen(inner, mode, q, u, rn)}AUDIT.check(-1, ${u} < 0 || ${u} === ${q} ? -1 : ${u}, ${JSON.stringify(`guard * in ${rn}`)}, s, ${q}); V = ${v}; break; } `;
                }
                return `{ let ${q} = ${pos}, ${n} = 0; ${keep ? `const ${arr} = []; ` : ""}
  for (;;) { ${guard}let ${t}; ${rcSave(rl)}${gen(inner, mode, q, t, rn)}if (${t} < 0 || ${t} === ${q}) { ${rcBack(rl)}break; } ${keep ? `${arr}.push(V); ` : ""}${n}++; ${q} = ${t}; }
  if (${n} < ${min}) ${out} = -1; else { ${keep ? `V = ${arr}; ` : ""}${out} = ${q}; } }\n`;
            }
            case "next": {
                const [a, b] = e.value as Expression[];
                const t = tmp("t");
                return `{ let ${t}; ${gen(a, "r", pos, t, rn)}if (${t} < 0) ${out} = -1; else { ${gen(b, mode, t, out, rn)}} }\n`;
            }
            case "skip": {
                const [a, b] = e.value as Expression[];
                const t = tmp("t"), u = tmp("u"), v = tmp("v");
                return `{ let ${t}; ${gen(a, mode, pos, t, rn)}if (${t} < 0) ${out} = -1; else { ${keep ? `const ${v} = V; ` : ""}let ${u}; ${gen(b, "r", t, u, rn)}if (${u} < 0) ${out} = -1; else { ${keep ? `V = ${v}; ` : ""}${out} = ${u}; } } }\n`;
            }
            case "minus": {
                const [a, b] = e.value as Expression[];
                const t = tmp("t"), rl = tmp("rl");
                return `{ ${rcSave(rl)}let ${t}; ${gen(b, "r", pos, t, rn)}${rcBack(rl)}if (${t} >= 0) ${out} = -1; else { ${gen(a, mode, pos, out, rn)}} }\n`;
            }
            case "concatenation": {
                const parts = e.value as Expression[];
                const L = tmp("L");
                const ts = parts.map(() => tmp("t")), vs = parts.map(() => tmp("v"));
                let code = `${L}: { ${out} = -1;\n`;
                let at = pos;
                parts.forEach((p, idx) => {
                    code += `let ${ts[idx]}; ${gen(p, mode, at, ts[idx], rn)}if (${ts[idx]} < 0) break ${L}; ${keep ? `const ${vs[idx]} = V;` : ""}\n`;
                    at = ts[idx];
                });
                if (keep) code += `V = [${vs.join(", ")}];\n`; // positional (VALUE-SEMANTICS.md)
                return code + `${out} = ${at}; }\n`;
            }
            case "alternation": {
                const alts = e.value as Expression[];
                const r = routes(alts.map(info), opts.nonAsciiRoute?.(alts) ?? null);
                const L = tmp("L"), rl = tmp("rl");
                const next = (o: string, Lx: string, last: boolean) =>
                    hasRec ? `if (${o} >= 0) break ${Lx}; ${rcBack(rl)}\n` : last ? "" : `if (${o} >= 0) break ${Lx};\n`;
                const tryAll = (o: string) => rcSave(rl) + alts.map((a, j) => `${gen(a, mode, pos, o, rn)}${next(o, L, j === alts.length - 1)}`).join("");
                if (r === null) return `${L}: { ${tryAll(out)}}\n`;
                // A routed choice: each alternative that is not a plain leaf or reference is hoisted
                // into its own function, so the routes (which share alternatives) never duplicate it.
                const genAlt = alts.map((a) => {
                    const u = unwrap(a);
                    if (u.type === "nonterminal" || u.type === "literal" || u.type === "regex") return (o: string) => gen(a, mode, pos, o, rn);
                    const h = tmp("h"), o2 = tmp("o");
                    helpers.push(`/** @type {Rule} */ function ${h}(s, i) { let ${o2}; ${gen(a, mode, "i", o2, rn)}return ${o2}; }`);
                    return (o: string) => `${o} = ${h}(s, ${pos});\n`;
                });
                const T = constName(r.tbl), c = tmp("c"), g = tmp("g");
                const routed = (o: string, Lr: string) => {
                    let code = `${Lr}: { ${rcSave(rl)}const ${c} = s.charCodeAt(${pos}); const ${g} = ${c} < 128 ? ${T}[${c}] : ${c} === ${c} ? ${r.na} : ${r.eof};\n switch (${g}) {\n`;
                    r.groups.forEach((members, gi) => {
                        code += `case ${gi}: { ${members.map((m, j) => `${genAlt[m](o)}${next(o, Lr, j === members.length - 1)}`).join("")}break ${Lr}; }\n`;
                    });
                    return code + `default: ${o} = -1; } }\n`;
                };
                if (!opts.audit) return routed(out, L);
                const La = tmp("L"), b = tmp("b"), v = tmp("v");
                return `${routed(out, La)}{ const ${v} = V; let ${b}; ${L}: { ${tryAll(b)}} AUDIT.check(${out}, ${b}, ${JSON.stringify(`choice in ${rn}`)}, s, ${pos}); V = ${v}; }\n`;
            }
            default: throw new Error(`emit: unsupported BBNF node \`${(e as Expression).type}\``);
        }
    }

    // Every rule in value mode (and, for an audit build, in recognize mode); references pull in the
    // recognize-mode functions they need.
    for (const name of ruleNames) ruleFn(name, "v");
    if (opts.audit) for (const name of ruleNames) ruleFn(name, "r");
    const fns: string[] = [];
    while (queue.length > 0) {
        const [name, mode] = queue.shift()!;
        const kind = actionKinds[name];
        const bodyMode: Mode = kind === "text" ? "r" : mode;
        groupsLeaf = kind === "groups" && mode === "v" ? unwrap(ast.get(name)!.expression) : null;
        let body = `let o; ${rcSave("rl")}${gen(ast.get(name)!.expression, bodyMode, "i", "o", name)}${hasRec ? "if (o < 0) RC.length = rl;\n" : ""}`;
        const sync = recovers.get(name);
        if (sync !== undefined) {
            // `@recover`: where the rule fails at a unit it can start with (its FIRST set; anywhere when
            // it is nullable) and its sync expression then matches with progress, the rule answers
            // `null` there and the span is recorded (its action does not run on it). Guarding by
            // FIRST keeps the rule's facts those of its body, so routing never changes an answer.
            const u = tmp("u"), c = tmp("c"), inf = info(ast.get(name)!.expression);
            const starts = inf.nullable ? "" : `const ${c} = s.charCodeAt(i); if (${mayStart(c, inf)}) `;
            body += `if (o < 0) { ${starts}{ let ${u}; ${gen(sync, "r", "i", u, name)}if (${u} > i) { RC.push(${ruleNames.indexOf(name)}, i, ${u}); ${mode === "v" ? "V = null; " : ""}return ${u}; } } }\n`;
        }
        if (kind !== undefined && mode === "v") {
            const A = `A${actNames.length}`;
            actNames.push(name);
            const groups = kind === "groups" ? Array.from({ length: groupCount(groupsLeaf!.value as RegExp) }, (_, g) => `V[${g + 1}]`).join(", ") : "";
            body += kind === "map" ? `if (o >= 0) V = ${A}(V);\n`
                : kind === "span" ? `if (o >= 0) V = ${A}(V, i, o);\n`
                    : kind === "groups" ? `if (o >= 0) V = ${A}(${groups});\n`
                        : `if (o >= 0) V = ${A}(s.substring(i, o));\n`;
        }
        fns.push(`/** @type {Rule} */ function ${fnName.get(`${name}/${mode}`)}(s, i) {\n${body}return o;\n}`);
    }

    const q = JSON.stringify;
    const fault = entryNames.map((n) => depth && nests(n));
    const reset = (j: number) => `${fault[j] ? "D = 0; " : ""}${hasRec ? "RC.length = 0; " : ""}`;
    const entryFns = entryNames.map((n, j) =>
        `/** @param {string} s */ function e${j}(s) { ${reset(j)}const o = ${fnName.get(`${n}/v`)}(s, 0); return o === s.length${fault[j] ? ` && D <= ${maxDepth}` : ""} ? V : FAIL; }`);
    const wrap = entryNames.map((_, j) => fault[j] || hasRec);
    const ruleFns = entryNames.flatMap((n, j) => (wrap[j]
        ? [`/** @type {Rule} */ function x${j}(s, i) { ${reset(j)}const o = ${fnName.get(`${n}/v`)}(s, i); return ${fault[j] ? `D > ${maxDepth} ? -1 : ` : ""}o; }`]
        : []));
    const table = (names: readonly string[], f: (n: string, j: number) => string) => `{ ${names.map((n, j) => `${q(n)}: ${f(n, j)}`).join(", ")} }`;
    const body = [
        `"use strict";`,
        `/** @typedef {(s: string, i: number) => number} Rule */`,
        `for (const [n, k] of Object.entries(ACTION_KINDS)) { const a = A[n]; if (a == null || a.kind !== k || typeof a.fn !== "function") throw new TypeError("bbnf: rule \`" + n + "\` needs a " + k + " action"); }`,
        `for (const n of Object.keys(A)) if (!Object.prototype.hasOwnProperty.call(ACTION_KINDS, n)) throw new TypeError("bbnf: no rule \`" + n + "\` takes an action (a stale parser module?)");`,
        ...actNames.map((n, j) => `const A${j} = A[${q(n)}].fn;`),
        ...(hostNames.length > 0
            ? [...hostNames.map((n, j) => `const H${j} = H[${q(n)}]; if (typeof H${j} !== "function") throw new TypeError("bbnf: host rule \`" + ${q(n)} + "\` is not supplied");`), `const HB = { v: undefined };`]
            : []),
        ...consts.map((src, j) => `const K${j} = ${src};`),
        `/** @type {any} */ let V;`,
        ...(depth ? [`let D = 0;`, `const TRIP = 1073741824;`] : []),
        ...(hasRec ? [`const RC = [];`] : []),
        ...fns, ...helpers, ...entryFns, ...ruleFns,
        `return { rules: ${table(entryNames, (n, j) => (wrap[j] ? `x${j}` : fnName.get(`${n}/v`)!))}, entries: ${table(entryNames, (_, j) => `e${j}`)}, value: () => V${
            hasRec ? `, recovered: () => RC` : ""}${
            opts.audit ? `, recognize: ${table(ruleNames, (n) => `(s, i) => { ${depth ? "D = 0; " : ""}return ${fnName.get(`${n}/r`)}(s, i); }`)}` : ""} };`,
    ].join("\n");

    const header = opts.header ?? "";
    const module = `${header}export const RULE_NAMES = Object.freeze(${q(ruleNames)});
export const ENTRY_NAMES = Object.freeze(${q(entryNames)});
export const ACTION_KINDS = Object.freeze(${q(actionKinds)});
${hostNames.length > 0 ? `export const HOST_NAMES = Object.freeze(${q(hostNames)});\n` : ""}export const FAIL = Symbol.for(${q(FAIL_KEY)});
/**
 * @param {Readonly<Record<string, { readonly kind: string; readonly fn: (...args: any[]) => unknown }>>} A${hostNames.length > 0 ? "\n * @param {Readonly<Record<string, (s: string, i: number, box: { v: unknown }) => number>>} H" : ""}
 */
export function createParser(A${hostNames.length > 0 ? ", H" : ""}) {
${body}
}
`;
    return { body, module, dts: declarations(header, ruleNames, entryNames, actionKinds, hostNames), ruleNames, entryNames, actionKinds, hostNames };
}

/** The module's declarations: the typed action table (kinds map/span/text/groups) and the parser's shape. */
function declarations(header: string, ruleNames: readonly string[], entryNames: readonly string[],
    actionKinds: Readonly<Record<string, ActionKind>>, hostNames: readonly string[]): string {
    const q = (v: string) => JSON.stringify(v);
    const kindType: Record<ActionKind, string> = { map: "MapAction", span: "SpanAction", text: "TextAction", groups: "GroupsAction" };
    const acts = Object.entries(actionKinds);
    return `${header}export declare const RULE_NAMES: readonly [${ruleNames.map(q).join(", ")}];
export type RuleName = (typeof RULE_NAMES)[number];
export declare const ENTRY_NAMES: readonly [${entryNames.map(q).join(", ")}];
export type EntryName = (typeof ENTRY_NAMES)[number];
export declare const ACTION_KINDS: { ${acts.map(([n, k]) => `readonly ${q(n)}: ${q(k)};`).join(" ")} };
${hostNames.length > 0 ? `export declare const HOST_NAMES: readonly [${hostNames.map(q).join(", ")}];\n` : ""}/** What an entry answers when the input is refused (\`Symbol.for(${q(FAIL_KEY)})\`). */
export declare const FAIL: unique symbol;
export type Fail = typeof FAIL;
/** Receives the rule's value (a positional tuple for a sequence). Pure and total. */
export interface MapAction<R = unknown> { readonly kind: "map"; readonly fn: (value: any) => R }
/** Receives the rule's value and the span \`[start, end)\` it matched. Pure and total. */
export interface SpanAction<R = unknown> { readonly kind: "span"; readonly fn: (value: any, start: number, end: number) => R }
/** Receives only the text the rule matched (the rule is recognized, never built). Pure and total. */
export interface TextAction<R = unknown> { readonly kind: "text"; readonly fn: (text: string) => R }
/** Receives the capturing groups its regex leaf matched, in order (an unmatched group \`undefined\`). Pure and total. */
export interface GroupsAction<R = unknown> { readonly kind: "groups"; readonly fn: (...groups: any[]) => R }
/** One action per rule that takes one, of exactly the kind the grammar was generated with. */
export interface Actions {
${acts.map(([n, k]) => `    readonly ${q(n)}: ${kindType[k]};`).join("\n")}
}
${hostNames.length > 0 ? `/** A host rule: matches at \`offset\`, answers the end (or -1) and writes its value to \`box.v\`. */
export type HostRule = (source: string, offset: number, box: { v: unknown }) => number;
export type Host = { readonly [K in (typeof HOST_NAMES)[number]]: HostRule };
` : ""}type ValueOf<A, N> = N extends keyof A ? (A[N] extends { readonly fn: (...args: any[]) => infer R } ? R : unknown) : unknown;
export interface Parser<A extends Actions = Actions> {
    /** Each entry rule over a WHOLE input: its value, or FAIL. Not re-entrant. */
    readonly entries: { readonly [N in EntryName]: (source: string) => ValueOf<A, N> | Fail };
    /** Each entry rule at an offset: the end of its match, or -1; the value is \`value()\`. */
    readonly rules: { readonly [N in EntryName]: (source: string, offset: number) => number };
    /** The value of the last successful \`rules\` call. */
    value(): unknown;
}
export declare function createParser<A extends Actions>(actions: A${hostNames.length > 0 ? ", host: Host" : ""}): Parser<A>;
`;
}
