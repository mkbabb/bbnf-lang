// SERVED MODEL: claude-opus-5-5
//
// analysis/regex.ts — a SOUND first-character analysis of a JavaScript RegExp, reading
// its `source` AND its `flags` (the root cure of F-b-3: bbnf-lang 0.1.4's `regexFirstChars` read the
// source only, so `/none/i` dispatched on `n` alone). The analysis answers, for a sticky match
// attempted at offset i:
//   first    — the code units the match can START with (128 ASCII bits + one `nonAscii` bit);
//   nullable — whether the match can be EMPTY (then it can succeed before any character).
// Soundness is the contract: `first` is a SUPERSET of the truth, `nullable` is true whenever an empty
// match is possible. Lookarounds, anchors and word boundaries are zero-width and are read as
// "matches empty" (a superset); a back-reference is read as "anything, or empty".
// Case-insensitive (`i`, no `u`): ES Canonicalize never maps a non-ASCII unit onto ASCII, so an ASCII
// letter's other case is the only extra first unit. With `u`/`v`, K (U+212A) and ſ (U+017F) fold
// onto k/s: those set `nonAscii`.
// Provenance: value.js X.P.W7 research, route-ts-compiler `src/regex.ts` (the ratified analysis).

/** 128 ASCII bits (four 32-bit words) + one bit for every non-ASCII code unit. */
export class CharSet {
    w0 = 0; w1 = 0; w2 = 0; w3 = 0;
    nonAscii = false;
    add(c: number): void {
        const b = 1 << (c & 31);
        switch (c >> 5) { case 0: this.w0 |= b; break; case 1: this.w1 |= b; break; case 2: this.w2 |= b; break; default: this.w3 |= b; }
    }
    has(c: number): boolean {
        if (c >= 128) return this.nonAscii;
        const w = c < 32 ? this.w0 : c < 64 ? this.w1 : c < 96 ? this.w2 : this.w3;
        return ((w >>> (c & 31)) & 1) === 1;
    }
    addRange(from: number, to: number): void {
        for (let c = from; c <= to && c < 128; c++) this.add(c);
        if (to >= 128) this.nonAscii = true;
    }
    union(other: CharSet): void { unionInto(this, other); }
    intersection(other: CharSet): CharSet {
        const s = new CharSet();
        s.w0 = this.w0 & other.w0; s.w1 = this.w1 & other.w1; s.w2 = this.w2 & other.w2; s.w3 = this.w3 & other.w3;
        s.nonAscii = this.nonAscii && other.nonAscii;
        return s;
    }
    isDisjoint(other: CharSet): boolean { return this.intersection(other).isEmpty(); }
    isEmpty(): boolean { return (this.w0 | this.w1 | this.w2 | this.w3) === 0 && !this.nonAscii; }
    /** The number of ASCII members (the `nonAscii` bit is not counted). */
    len(): number {
        let n = 0;
        for (const w of [this.w0, this.w1, this.w2, this.w3]) { let x = w >>> 0; while (x) { x &= x - 1; n++; } }
        return n;
    }
    /** The ASCII members, ascending (the `nonAscii` bit is read from `nonAscii`). */
    *[Symbol.iterator](): IterableIterator<number> { for (let c = 0; c < 128; c++) if (this.has(c)) yield c; }
    clone(): CharSet { const s = new CharSet(); unionInto(s, this); return s; }
    /** The byte-per-unit table the parsers index (built once, after the analysis settles). */
    #table: Uint8Array | undefined;
    get ascii(): Uint8Array {
        if (this.#table === undefined) { const t = new Uint8Array(128); for (let c = 0; c < 128; c++) t[c] = this.has(c) ? 1 : 0; this.#table = t; }
        return this.#table;
    }
}
export const emptySet = (): CharSet => new CharSet();
export const fullSet = (): CharSet => { const s = new CharSet(); s.w0 = s.w1 = s.w2 = s.w3 = -1; s.nonAscii = true; return s; };
export function unionInto(a: CharSet, b: CharSet): boolean {
    const w0 = a.w0 | b.w0, w1 = a.w1 | b.w1, w2 = a.w2 | b.w2, w3 = a.w3 | b.w3, na = a.nonAscii || b.nonAscii;
    const changed = w0 !== a.w0 || w1 !== a.w1 || w2 !== a.w2 || w3 !== a.w3 || na !== a.nonAscii;
    a.w0 = w0; a.w1 = w1; a.w2 = w2; a.w3 = w3; a.nonAscii = na;
    return changed;
}
export const setHas = (s: CharSet, code: number): boolean => s.has(code);

type Node =
    | { t: "set"; set: CharSet }
    | { t: "seq"; items: Node[] }
    | { t: "alt"; alts: Node[] }
    | { t: "rep"; node: Node; min: number }
    | { t: "zero" }                 // lookaround, anchor, \b
    | { t: "backref" };

const isWs = (c: number) => c === 32 || (c >= 9 && c <= 13);
const isDigit = (c: number) => c >= 48 && c <= 57;
const isWord = (c: number) => isDigit(c) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || c === 95;

function classSet(pred: (c: number) => boolean, nonAscii: boolean): CharSet {
    const s = emptySet();
    for (let c = 0; c < 128; c++) if (pred(c)) s.add(c);
    s.nonAscii = nonAscii;
    return s;
}
const ESCAPE_CLASS: Record<string, () => CharSet> = {
    d: () => classSet(isDigit, false),
    D: () => classSet((c) => !isDigit(c), true),
    w: () => classSet(isWord, false),            // (with `iu`, K/ſ are word characters: handled by fold)
    W: () => classSet((c) => !isWord(c), true),
    s: () => classSet(isWs, true),               // \s includes U+00A0, U+FEFF, the Zs block, U+2028/9
    S: () => classSet((c) => !isWs(c), true),
};
const SIMPLE_ESCAPE: Record<string, number> = { n: 10, r: 13, t: 9, v: 11, f: 12, "0": 0 };

class RegexReader {
    i = 0;
    constructor(readonly src: string, readonly unicode: boolean, readonly dotAll: boolean) {}
    peek() { return this.src[this.i]; }
    eat(ch: string) { if (this.src[this.i] === ch) { this.i++; return true; } return false; }

    disjunction(): Node {
        const alts: Node[] = [this.alternative()];
        while (this.eat("|")) alts.push(this.alternative());
        return alts.length === 1 ? alts[0] : { t: "alt", alts };
    }
    alternative(): Node {
        const items: Node[] = [];
        while (this.i < this.src.length && this.peek() !== "|" && this.peek() !== ")") items.push(this.quantified());
        return { t: "seq", items };
    }
    quantified(): Node {
        const atom = this.atom();
        const ch = this.peek();
        let min = -1;
        if (ch === "*") { this.i++; min = 0; }
        else if (ch === "+") { this.i++; min = 1; }
        else if (ch === "?") { this.i++; min = 0; }
        else if (ch === "{") {
            const m = /^\{(\d+)(,(\d*))?\}/.exec(this.src.slice(this.i));
            if (m) { this.i += m[0].length; min = Number(m[1]); }
        }
        if (min < 0) return atom;
        this.eat("?"); // lazy suffix: the same language
        return { t: "rep", node: atom, min };
    }
    atom(): Node {
        const ch = this.src[this.i++];
        switch (ch) {
            case "^": case "$": return { t: "zero" };
            case ".": return { t: "set", set: this.dotAll ? fullSet() : classSet((c) => c !== 10 && c !== 13, true) };
            case "[": return { t: "set", set: this.charClass() };
            case "(": {
                let zero = false;
                if (this.src.startsWith("?:", this.i)) this.i += 2;
                else if (this.src.startsWith("?=", this.i) || this.src.startsWith("?!", this.i)) { this.i += 2; zero = true; }
                else if (this.src.startsWith("?<=", this.i) || this.src.startsWith("?<!", this.i)) { this.i += 3; zero = true; }
                else if (this.src.startsWith("?<", this.i)) { this.i = this.src.indexOf(">", this.i) + 1; }
                else if (this.src[this.i] === "?") throw new Error(`regex: unsupported group at ${this.i} in /${this.src}/`);
                const inner = this.disjunction();
                if (!this.eat(")")) throw new Error(`regex: unclosed group in /${this.src}/`);
                return zero ? { t: "zero" } : inner;
            }
            case "\\": return this.escape(false) as Node;
            default: {
                if (ch === undefined) throw new Error("regex: unexpected end");
                return { t: "set", set: this.unitSet(ch.charCodeAt(0)) };
            }
        }
    }
    unitSet(code: number): CharSet {
        const s = emptySet();
        if (code < 128) s.add(code); else s.nonAscii = true;
        return s;
    }
    /** An escape outside (inClass=false) or inside a class; answers a Node, or a code/CharSet in a class. */
    escape(inClass: boolean): Node | CharSet | number {
        const ch = this.src[this.i++];
        if (ch in ESCAPE_CLASS) return inClass ? ESCAPE_CLASS[ch]() : { t: "set", set: ESCAPE_CLASS[ch]() };
        if (!inClass && (ch === "b" || ch === "B")) return { t: "zero" };
        if (!inClass && /[1-9]/.test(ch)) { while (/\d/.test(this.src[this.i] ?? "")) this.i++; return { t: "backref" }; }
        if (!inClass && ch === "k") { this.i = this.src.indexOf(">", this.i) + 1; return { t: "backref" }; }
        let code: number;
        if (ch === "u") {
            if (this.src[this.i] === "{") { const end = this.src.indexOf("}", this.i); code = parseInt(this.src.slice(this.i + 1, end), 16); this.i = end + 1; }
            else { code = parseInt(this.src.slice(this.i, this.i + 4), 16); this.i += 4; }
        } else if (ch === "x") { code = parseInt(this.src.slice(this.i, this.i + 2), 16); this.i += 2; }
        else if (ch === "c") { code = this.src.charCodeAt(this.i++) % 32; }
        else if (ch === "p" || ch === "P") { this.i = this.src.indexOf("}", this.i) + 1; return inClass ? fullSet() : { t: "set", set: fullSet() }; }
        else if (inClass && ch === "b") code = 8;
        else code = SIMPLE_ESCAPE[ch] ?? ch.charCodeAt(0);
        return inClass ? code : { t: "set", set: this.unitSet(code) };
    }
    charClass(): CharSet {
        const negate = this.eat("^");
        const s = emptySet();
        const addRange = (lo: number, hi: number) => {
            for (let c = lo; c <= Math.min(hi, 127); c++) s.add(c);
            if (hi >= 128) s.nonAscii = true;
        };
        while (this.i < this.src.length && this.peek() !== "]") {
            let lo: number | CharSet;
            if (this.eat("\\")) lo = this.escape(true) as number | CharSet; else lo = this.src.charCodeAt(this.i++);
            if (typeof lo !== "number") { unionInto(s, lo); continue; }
            if (this.peek() === "-" && this.src[this.i + 1] !== "]" && this.i + 1 < this.src.length) {
                this.i++;
                let hi: number | CharSet;
                if (this.eat("\\")) hi = this.escape(true) as number | CharSet; else hi = this.src.charCodeAt(this.i++);
                if (typeof hi !== "number") { addRange(lo, lo); addRange(45, 45); unionInto(s, hi); continue; }
                addRange(lo, hi);
            } else addRange(lo, lo);
        }
        if (!this.eat("]")) throw new Error(`regex: unclosed class in /${this.src}/`);
        if (!negate) return s;
        const n = emptySet();
        for (let c = 0; c < 128; c++) if (!s.has(c)) n.add(c);
        n.nonAscii = true;
        return n;
    }
}

function nullable(n: Node): boolean {
    switch (n.t) {
        case "set": return false;
        case "seq": return n.items.every(nullable);
        case "alt": return n.alts.some(nullable);
        case "rep": return n.min === 0 || nullable(n.node);
        default: return true;
    }
}
function first(n: Node, out: CharSet): void {
    switch (n.t) {
        case "set": unionInto(out, n.set); return;
        case "seq": for (const it of n.items) { first(it, out); if (!nullable(it)) return; } return;
        case "alt": for (const a of n.alts) first(a, out); return;
        case "rep": first(n.node, out); return;
        case "backref": unionInto(out, fullSet()); return;
        default: return;
    }
}

function foldCase(s: CharSet, unicode: boolean): void {
    for (let c = 65; c <= 90; c++) {
        if (s.has(c) || s.has(c + 32)) { s.add(c); s.add(c + 32); }
    }
    if (unicode && (s.has(107) || s.has(115))) s.nonAscii = true; // K (U+212A) · ſ (U+017F)
}

export type RegexFirst = Readonly<{ first: CharSet; nullable: boolean }>;
const CACHE = new Map<string, RegexFirst>();

/** The sound FIRST set and nullability of `re` (source AND flags). */
export function regexFirst(re: RegExp): RegexFirst {
    const key = `/${re.source}/${re.flags}`;
    const hit = CACHE.get(key);
    if (hit) return hit;
    const unicode = re.flags.includes("u") || re.flags.includes("v");
    const r = new RegexReader(re.source, unicode, re.flags.includes("s"));
    const ast = r.disjunction();
    if (r.i !== re.source.length) throw new Error(`regex: trailing input in ${key}`);
    const set = emptySet();
    first(ast, set);
    if (re.flags.includes("i")) foldCase(set, unicode);
    const out = { first: set, nullable: nullable(ast) };
    CACHE.set(key, out);
    return out;
}

/**
 * `[…]*` · `[…]+` · `\s*` · `\s+` (a single class, repeated): the class as its own regex and the
 * EXACT membership of every ASCII unit (decided by the class regex itself, so exact by construction).
 */
export function singleClassRun(re: RegExp): { cls: RegExp; tbl: Uint8Array; min: 0 | 1; all: boolean } | null {
    const m = /^(\[(?:\\[\s\S]|[^\]\\])*\]|\\[sSdDwW])([*+])$/.exec(re.source);
    if (m === null) return null;
    const flags = re.flags.replace(/[gy]/g, "");
    const cls = new RegExp(m[1], flags);
    const whole = new RegExp(`^${m[1]}$`, flags);
    const tbl = new Uint8Array(128);
    for (let c = 0; c < 128; c++) tbl[c] = whole.test(String.fromCharCode(c)) ? 1 : 0;
    // A class that admits every code unit, by its spelling: [\s\S] · [\S\s] · [\d\D] · [\w\W] · [^].
    const all = /^\[(?:\\s\\S|\\S\\s|\\d\\D|\\D\\d|\\w\\W|\\W\\w|\^)\]$/.test(m[1]);
    return { cls, tbl, min: m[2] === "+" ? 1 : 0, all };
}

/**
 * The escape-run loop, unrolled (value.js X.P.W7 `.l`): `(?:\\[\s\S]|[^X\\])*` — a star over an
 * alternation of an escape pair and a class that excludes the backslash, as a string literal's body is
 * spelt — becomes `[^X\\]*(?:\\[\s\S][^X\\]*)*` (either order of the two arms; greedy `*` only).
 * Same language and same preferred match in every context: the arms are disjoint (one begins with `\`,
 * the other excludes it) and each is one iteration, so both spellings stop at the same boundaries and
 * give them back in the same order. What changes is the engine's work: no per-iteration alternation to
 * record for backtracking. JavaScriptCore paid ~12 ms to refuse one unclosed quote 178 KB from the end
 * of a real sheet in the first spelling, ~1 ms in the second (value.js `bench/records/2026-09-24-x-p-w7-yarr.json`).
 */
export function unrollEscapeRuns(source: string): string {
    return source.replace(/(?<!(?:^|[^\\])(?:\\\\)*\\)\(\?:(?:\\\\\[\\s\\S\]\|\[\^([^\]\\]*)\\\\\]|\[\^([^\]\\]*)\\\\\]\|\\\\\[\\s\\S\])\)\*(?![?*+{])/g,
        (_, a: string | undefined, b: string | undefined) => {
            const cls = `[^${a ?? b}\\\\]`;
            return `${cls}*(?:\\\\[\\s\\S]${cls}*)*`;
        });
}
