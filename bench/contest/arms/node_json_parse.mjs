// JSON P1/P4 arm: the runtime's native `JSON.parse` (sk-v27 LW2.CON-1, row R-JS-P4-json-parse).
//
// Witness mode prints, per input file, `ACCEPT <sha256 of the canonical form>` or
// `REJECT <offset|->`, in the format of bench/contest/oracle_json.py.
//
// Work this adapter adds so that the arm meets the P4 plane (bench/planes/P4.md),
// each item charged to the opponent in ROWS.tsv's work checklist:
//   - fatal UTF-8 decoding, keeping a byte-order mark so that JSON.parse rejects it;
//   - a well-formedness check on every string and key (JSON.parse accepts unpaired
//     surrogate escapes);
//   - number fidelity from the source text (JSON.parse rounds every number to binary64):
//     an integer lexeme in [-2^63, 2^64 - 1] is kept exact, and a number that rounds to
//     infinity is rejected.
// Error offsets are not read from V8's message, so this arm prints `REJECT -` and is
// admissible for the VAL class only, not for CHK (offset) cells.
import { readFileSync } from "node:fs";
import { createHash } from "node:crypto";

const I64_MIN = -(2n ** 63n);
const U64_MAX = 2n ** 64n - 1n;
const utf8 = new TextEncoder();

class Reject extends Error {}

function number(source) {
    if (!/[.eE]/.test(source)) {
        const n = BigInt(source);
        if (n >= I64_MIN && n <= U64_MAX) return { int: n };
    }
    const x = Number(source);
    if (!Number.isFinite(x)) throw new Reject();
    return { f64: x };
}

function parse(bytes) {
    let text;
    try {
        text = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true }).decode(bytes);
    } catch {
        throw new Reject();
    }
    try {
        return JSON.parse(text, (key, value, context) => {
            if (!key.isWellFormed()) throw new Reject();
            if (typeof value === "string" && !value.isWellFormed()) throw new Reject();
            if (typeof value === "number") return number(context.source);
            return value;
        });
    } catch (e) {
        if (e instanceof SyntaxError || e instanceof Reject) throw new Reject();
        throw e;
    }
}

function canonical(v) {
    const parts = [];
    const str = (s) => {
        const b = utf8.encode(s);
        parts.push(Buffer.from(`s${b.length}:`), Buffer.from(b));
    };
    const cmp = (a, b) => Buffer.compare(Buffer.from(utf8.encode(a)), Buffer.from(utf8.encode(b)));
    const enc = (x) => {
        if (x === null) parts.push(Buffer.from("n"));
        else if (x === true) parts.push(Buffer.from("t"));
        else if (x === false) parts.push(Buffer.from("f"));
        else if (typeof x === "string") str(x);
        else if (Array.isArray(x)) {
            parts.push(Buffer.from("["));
            x.forEach(enc);
            parts.push(Buffer.from("]"));
        } else if (typeof x.int === "bigint") parts.push(Buffer.from(`i${x.int};`));
        else if (typeof x.f64 === "number") {
            const b = Buffer.alloc(8);
            b.writeDoubleBE(x.f64);
            parts.push(Buffer.from(`d${b.toString("hex")};`));
        } else {
            parts.push(Buffer.from("{"));
            for (const k of Object.keys(x).sort(cmp)) {
                str(k);
                enc(x[k]);
            }
            parts.push(Buffer.from("}"));
        }
    };
    enc(v);
    return Buffer.concat(parts);
}

// Timed mode (LW2.CON-2, S11): `--time K FILE` parses FILE K times in this one process and folds
// every node of every result into a checksum inside the timed loop (PREREG §1: the walk is timed),
// then prints `checksum=<fold>` once. The input bytes are read before the loop.
function fold(h, x) {
    if (x === null) return (h * 31 + 1) >>> 0;
    if (x === true) return (h * 31 + 2) >>> 0;
    if (x === false) return (h * 31 + 3) >>> 0;
    if (typeof x === "string") return (h * 31 + x.length + 4) >>> 0;
    if (Array.isArray(x)) {
        for (const y of x) h = fold(h, y);
        return (h * 31 + x.length + 5) >>> 0;
    }
    if (typeof x.int === "bigint") return (h * 31 + Number(x.int & 0xffffn) + 6) >>> 0;
    if (typeof x.f64 === "number") return (h * 31 + (Math.trunc(x.f64) & 0xffff) + 7) >>> 0;
    let n = 0;
    for (const k in x) {
        h = fold((h * 31 + k.length) >>> 0, x[k]);
        n++;
    }
    return (h * 31 + n + 8) >>> 0;
}

if (process.argv[2] === "--time") {
    const k = Number(process.argv[3]);
    const bytes = readFileSync(process.argv[4]);
    let h = 0;
    for (let i = 0; i < k; i++) h = fold(h, parse(bytes));
    console.log(`checksum=${h.toString(16)}`);
    process.exit(0);
}

for (const path of process.argv.slice(2)) {
    let line;
    try {
        const v = parse(readFileSync(path));
        line = "ACCEPT " + createHash("sha256").update(canonical(v)).digest("hex");
    } catch (e) {
        if (!(e instanceof Reject)) throw e;
        line = "REJECT -";
    }
    console.log(line);
}
