// sk-v27 consumers lane (USE): shared readers. Every reader works at a pinned
// commit through `git`, never on a working tree, so a record regenerates
// byte-for-byte from HEADS.tsv alone.
import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

export const HERE = dirname(fileURLToPath(import.meta.url));

export function git(repo, args, opts = {}) {
    return execFileSync("git", ["-C", repo, ...args], {
        encoding: opts.encoding ?? "utf8",
        maxBuffer: 1 << 30,
    });
}

/** HEADS.tsv: repo \t path (~-relative) \t ref \t sha. */
export function readHeads() {
    const rows = readFileSync(join(HERE, "HEADS.tsv"), "utf8")
        .split("\n")
        .filter((l) => l && !l.startsWith("#") && !l.startsWith("repo\t"));
    return rows.map((l) => {
        const [repo, path, ref, sha] = l.split("\t");
        return { repo, path: path.replace(/^~/, homedir()), ref, sha };
    });
}

export function lsTree(repo, sha) {
    return git(repo, ["ls-tree", "-r", "--name-only", "-z", sha])
        .split("\0")
        .filter(Boolean);
}

// Only what the lane reads: source, grammars and manifests outside docs and records.
const READABLE = /\.(ts|tsx|mts|cts|js|mjs|cjs|vue|bbnf|json|ya?ml)$/;
const UNREAD = /(^|\/)(node_modules|docs|evidence|records|\.lighthouseci|\.worktrees)\//;
const BLOBS = new Map(); // `${repo}@${sha}` -> Map(file -> text)

/** Every blob of a tree, read in one `git cat-file --batch` pass. */
function treeBlobs(repo, sha) {
    const key = `${repo}@${sha}`;
    if (BLOBS.has(key)) return BLOBS.get(key);
    const entries = git(repo, ["ls-tree", "-r", "-z", sha])
        .split("\0")
        .filter(Boolean)
        .map((l) => {
            const [meta, file] = l.split("\t");
            const [, type, oid] = meta.split(" ");
            return { type, oid, file };
        })
        .filter((e) => e.type === "blob" && READABLE.test(e.file) && !UNREAD.test(e.file));
    const buf = execFileSync("git", ["-C", repo, "cat-file", "--batch"], {
        input: entries.map((e) => e.oid).join("\n") + "\n",
        maxBuffer: 2 ** 32,
    });
    const map = new Map();
    let off = 0;
    for (const e of entries) {
        const nl = buf.indexOf(10, off);
        const size = Number(buf.subarray(off, nl).toString("latin1").split(" ")[2]);
        map.set(e.file, buf.subarray(nl + 1, nl + 1 + size));
        off = nl + 1 + size + 1;
    }
    BLOBS.set(key, map);
    return map;
}

export function show(repo, sha, file) {
    const b = treeBlobs(repo, sha).get(file);
    if (b === undefined) throw new Error(`${repo}@${sha}: no blob ${file}`);
    return b.toString("utf8");
}

export function tsv(header, rows) {
    const clean = (v) => String(v ?? "").replace(/[\t\n\r]/g, " ");
    return [header.join("\t"), ...rows.map((r) => header.map((h) => clean(r[h])).join("\t"))].join("\n") + "\n";
}

export function parseTsv(text) {
    const [head, ...lines] = text.split("\n").filter(Boolean);
    const cols = head.split("\t");
    return lines.map((l) => Object.fromEntries(l.split("\t").map((v, i) => [cols[i], v])));
}

/**
 * Lexical masks for JS/TS source. `code` blanks comments only (import
 * specifiers survive); `bare` also blanks string, template and regex bodies, so
 * a call or a regex is found in code, never in prose. Newlines are kept, so
 * line numbers survive both masks. `regexes` lists each regex literal's offset.
 */
const REGEX_PRECEDERS = new Set([..."(,=:[!&|?{};+-*%<>~^"]);
const REGEX_KEYWORDS = new Set(["return", "typeof", "case", "do", "else", "in", "of", "new", "delete", "void", "throw", "yield", "await", "instanceof"]);

export function mask(src) {
    const n = src.length;
    const code = src.split("");
    const bare = src.split("");
    const regexes = [];
    const blank = (arr, a, b) => {
        for (let k = a; k < b; k++) if (arr[k] !== "\n") arr[k] = " ";
    };
    let i = 0;
    let prev = ""; // last significant token, for the regex/division decision
    const tplStack = []; // brace depth at each `${` entry
    let braceDepth = 0;
    const scanString = (q) => {
        const s = i;
        i++;
        while (i < n && src[i] !== q) {
            if (src[i] === "\\") i++;
            else if (src[i] === "\n") break;
            i++;
        }
        i++;
        blank(bare, s + 1, Math.min(i - 1, n));
    };
    const scanTemplate = () => {
        // from just after a backtick or a closing `}` of `${…}`
        const s = i;
        while (i < n) {
            if (src[i] === "\\") {
                i += 2;
                continue;
            }
            if (src[i] === "`") {
                blank(bare, s, i);
                i++;
                return;
            }
            if (src[i] === "$" && src[i + 1] === "{") {
                blank(bare, s, i);
                i += 2;
                tplStack.push(braceDepth);
                braceDepth++;
                return;
            }
            i++;
        }
        blank(bare, s, n);
    };
    while (i < n) {
        const c = src[i];
        if (c === "/" && src[i + 1] === "/") {
            const s = i;
            while (i < n && src[i] !== "\n") i++;
            blank(code, s, i);
            blank(bare, s, i);
            continue;
        }
        if (c === "/" && src[i + 1] === "*") {
            const s = i;
            const e = src.indexOf("*/", i + 2);
            i = e < 0 ? n : e + 2;
            blank(code, s, i);
            blank(bare, s, i);
            continue;
        }
        if (c === "'" || c === '"') {
            scanString(c);
            prev = "str";
            continue;
        }
        if (c === "`") {
            i++;
            scanTemplate();
            prev = "str";
            continue;
        }
        if (c === "{") {
            braceDepth++;
            prev = "{";
            i++;
            continue;
        }
        if (c === "}") {
            braceDepth--;
            if (tplStack.length && tplStack[tplStack.length - 1] === braceDepth) {
                tplStack.pop();
                i++;
                scanTemplate();
                prev = "str";
                continue;
            }
            prev = "}";
            i++;
            continue;
        }
        if (c === "/") {
            const isRegex = prev === "" || REGEX_PRECEDERS.has(prev) || REGEX_KEYWORDS.has(prev);
            if (isRegex) {
                const s = i;
                i++;
                let cls = false;
                while (i < n && src[i] !== "\n") {
                    if (src[i] === "\\") {
                        i += 2;
                        continue;
                    }
                    if (src[i] === "[") cls = true;
                    else if (src[i] === "]") cls = false;
                    else if (src[i] === "/" && !cls) break;
                    i++;
                }
                if (src[i] !== "/") {
                    // not a regex after all (an unterminated slash); treat as division
                    i = s + 1;
                    prev = "/";
                    continue;
                }
                i++;
                while (i < n && /[a-z]/.test(src[i])) i++;
                regexes.push({ start: s, end: i });
                blank(bare, s + 1, i);
                prev = "re";
                continue;
            }
            prev = "/";
            i++;
            continue;
        }
        if (/[A-Za-z_$]/.test(c)) {
            const s = i;
            while (i < n && /[\w$]/.test(src[i])) i++;
            prev = src.slice(s, i);
            if (!REGEX_KEYWORDS.has(prev)) prev = "id";
            continue;
        }
        if (/[0-9]/.test(c)) {
            while (i < n && /[\w.]/.test(src[i])) i++;
            prev = "num";
            continue;
        }
        if (/\s/.test(c)) {
            i++;
            continue;
        }
        prev = c === ")" || c === "]" ? "id" : c;
        i++;
    }
    return { code: code.join(""), bare: bare.join(""), regexes };
}

/** Vue SFC: keep only <script> bodies (other text blanked, newlines kept). */
export function scriptOnly(file, text) {
    if (!file.endsWith(".vue")) return text;
    const out = text.replace(/[^\n]/g, " ").split("");
    const re = /<script\b[^>]*>([\s\S]*?)<\/script>/g;
    let m;
    while ((m = re.exec(text))) {
        const s = m.index + m[0].indexOf(">") + 1;
        for (let k = 0; k < m[1].length; k++) out[s + k] = text[s + k];
    }
    return out.join("");
}

export function lineOf(text, offset) {
    let line = 1;
    for (let k = 0; k < offset; k++) if (text.charCodeAt(k) === 10) line++;
    return line;
}
