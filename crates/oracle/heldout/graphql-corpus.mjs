// sk-v27 LW3.COMP-5b: the GraphQL held-out corpus, from graphql-js's own tests
// (ruling H-1: "the graphql-js corpus").
//
//   node graphql-corpus.mjs <graphql-js source tree at tag v16.11.0> <out dir>
//
// Collects every GraphQL document text graphql-js's test suite writes as a
// string literal: the kitchen sinks, and each literal passed to `parse`,
// `parseValue`'s document siblings, `expectSyntaxError`, `buildSchema`,
// `cycleSDL`, `expectPrintedSchema` and every `dedent`-tagged template. A
// template literal with an interpolation is skipped (its text is not fixed).
// Each distinct text is written once as cases/doc-NNNN.graphql, ordered by its
// sha256. Two deterministic single-character mutations of each (seed
// 20261111: one deletion, one insertion from GraphQL's punctuation) are written
// as cases/mut-NNNN.graphql. The corpus holds inputs only; the verdict of
// each is graphql-js's, read at G1 time from the pinned package.
import { createHash } from "node:crypto";
import { mkdirSync, readdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import path from "node:path";

const [src, out] = process.argv.slice(2);
if (!src || !out) throw new Error("usage: graphql-corpus.mjs <graphql-js tree> <out dir>");

function walk(d, acc) {
    for (const e of readdirSync(d)) {
        const p = path.join(d, e);
        if (statSync(p).isDirectory()) walk(p, acc);
        else if (/\.ts$/.test(e) && (p.includes("__tests__") || p.includes("__testUtils__"))) acc.push(p);
    }
    return acc;
}

// A JS string literal's value, without running any code: the literal is
// re-read as JSON after its quote and escape forms are normalised.
function cook(lit) {
    const q = lit[0];
    let body = lit.slice(1, -1);
    let s = "";
    for (let i = 0; i < body.length; i++) {
        const c = body[i];
        if (c !== "\\") {
            s += c === '"' ? '\\"' : c === "\n" ? "\\n" : c === "\r" ? "\\r" : c === "\t" ? "\\t" : c;
            continue;
        }
        const n = body[++i];
        if (n === undefined) return null;
        if (n === q || n === "'" || n === "`") s += n === '"' ? '\\"' : n;
        else if (n === "$") s += "$";
        else if (n === "\n") s += "";
        else if ("\\/bfnrt\"".includes(n)) s += "\\" + n;
        else if (n === "u") {
            if (body[i + 1] === "{") {
                const end = body.indexOf("}", i);
                const cp = parseInt(body.slice(i + 2, end), 16);
                s += JSON.stringify(String.fromCodePoint(cp)).slice(1, -1);
                i = end;
            } else {
                s += "\\u" + body.slice(i + 1, i + 5);
                i += 4;
            }
        } else if (n === "x") {
            s += "\\u00" + body.slice(i + 1, i + 3);
            i += 2;
        } else if (n === "0") s += "\\u0000";
        else s += n;
    }
    try {
        return JSON.parse('"' + s.replace(/[\u0000-\u001f]/g, (c) => "\\u" + c.charCodeAt(0).toString(16).padStart(4, "0")) + '"');
    } catch {
        return null;
    }
}

// String.raw`…` keeps escapes as written.
const raw = (lit) => lit.slice(1, -1).replace(/\\`/g, "`").replace(/\\\$/g, "$");

const CALLS = /(?:\bparse|expectSyntaxError|buildSchema|cycleSDL|expectPrintedSchema|parseSDL)\(\s*$/;
const docs = new Set();
for (const file of walk(path.join(src, "src"), [])) {
    const text = readFileSync(file, "utf8");
    // Every template literal (no interpolation) and quoted string literal.
    const re = /`(?:[^`\\$]|\\[\s\S]|\$(?!\{))*`|'(?:[^'\\\n]|\\[\s\S])*'|"(?:[^"\\\n]|\\[\s\S])*"/g;
    for (let m; (m = re.exec(text)); ) {
        const before = text.slice(Math.max(0, m.index - 40), m.index);
        const tagged = /\b(?:dedent|String\.raw)\s*$/.test(before);
        const called = CALLS.test(before);
        const kitchen = /kitchenSink(?:Query|SDL)/.test(file) && m[0][0] === "`";
        if (!(tagged || called || kitchen)) continue;
        const value = /String\.raw\s*$/.test(before) ? raw(m[0]) : cook(m[0]);
        if (value === null || value.trim() === "") continue;
        docs.add(value);
    }
}
const sorted = [...docs].sort((a, b) => {
    const ha = createHash("sha256").update(a).digest("hex");
    const hb = createHash("sha256").update(b).digest("hex");
    return ha < hb ? -1 : 1;
});
mkdirSync(path.join(out, "graphql/cases"), { recursive: true });
let seed = 20261111n;
const rnd = (n) => {
    seed = (seed * 6364136223846793005n + 1442695040888963407n) & 0xffffffffffffffffn;
    return Number((seed >> 33n) % BigInt(n));
};
const PUNCT = "!$&()[]{}:=@|.\"#,\n";
let k = 0;
sorted.forEach((d, i) => {
    const id = String(i).padStart(4, "0");
    writeFileSync(path.join(out, `graphql/cases/doc-${id}.graphql`), d);
    const chars = [...d];
    if (chars.length > 1) {
        const del = chars.slice();
        del.splice(rnd(chars.length), 1);
        writeFileSync(path.join(out, `graphql/cases/mut-${String(k++).padStart(4, "0")}.graphql`), del.join(""));
        const ins = chars.slice();
        ins.splice(rnd(chars.length + 1), 0, PUNCT[rnd(PUNCT.length)]);
        writeFileSync(path.join(out, `graphql/cases/mut-${String(k++).padStart(4, "0")}.graphql`), ins.join(""));
    }
});
console.log(`docs=${sorted.length} mutations=${k}`);
