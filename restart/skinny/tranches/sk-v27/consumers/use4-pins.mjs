// LW1.USE-4: the pin census. For every package.json and package-lock.json in
// the six consumers (at HEADS.tsv), the declared range and every resolved copy
// of the lockstep packages. `--check` regenerates byte-for-byte and holds the
// oracle pins; the one-copy gate is REPORTED per lockfile (a consumer's own
// gate, read here, never cured here).
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { join, dirname } from "node:path";
import { HERE, readHeads, lsTree, show, tsv, parseTsv } from "./lib.mjs";

const PKGS = ["@mkbabb/value.js", "@mkbabb/parse-that", "@mkbabb/bbnf-lang", "@mkbabb/keyframes.js", "@mkbabb/glass-ui"];
const SKIP = /(^|\/)(node_modules|docs|evidence|records|\.worktrees|fixtures)\//;
const DEP_FIELDS = ["dependencies", "devDependencies", "peerDependencies", "optionalDependencies"];

const rows = [];
for (const h of readHeads()) {
    const files = lsTree(h.path, h.sha).filter((f) => !SKIP.test(f));
    const manifests = files.filter((f) => /(^|\/)package\.json$/.test(f));
    const locks = new Set(files.filter((f) => /(^|\/)package-lock\.json$/.test(f)));
    for (const m of manifests) {
        const pj = JSON.parse(show(h.path, h.sha, m));
        const lockPath = (dirname(m) === "." ? "" : dirname(m) + "/") + "package-lock.json";
        const lock = locks.has(lockPath) ? JSON.parse(show(h.path, h.sha, lockPath)) : null;
        for (const pkg of PKGS) {
            const declared = DEP_FIELDS.map((f) => (pj[f]?.[pkg] ? `${f.replace("Dependencies", "")}:${pj[f][pkg]}` : null)).filter(Boolean);
            const copies = lock
                ? Object.entries(lock.packages ?? {})
                      .filter(([k]) => k === `node_modules/${pkg}` || k.endsWith(`/node_modules/${pkg}`))
                      .map(([k, v]) => ({ path: k, version: v.version ?? (v.link ? `link:${v.resolved}` : "?"), integrity: v.integrity ?? "" }))
                : [];
            if (!declared.length && !copies.length) continue;
            const versions = [...new Set(copies.map((c) => c.version))];
            rows.push({
                repo: h.repo,
                sha: h.sha.slice(0, 12),
                manifest: m,
                package: pkg,
                declared: declared.join(" ") || "(transitive)",
                lockfile: lock ? lockPath : "(none)",
                copies: copies.length,
                versions: versions.join(" "),
                one_copy: !lock ? "n/a" : copies.length === 1 ? "GREEN" : copies.length === 0 ? "n/a" : versions.length === 1 ? `RED ${copies.length} paths, 1 version` : `RED ${versions.length} versions`,
                paths: copies.map((c) => `${c.path}@${c.version}`).join(" "),
                integrity: copies.map((c) => c.integrity.slice(0, 19)).join(" "),
            });
        }
    }
}
const HEADER = ["repo", "sha", "manifest", "package", "declared", "lockfile", "copies", "versions", "one_copy", "paths", "integrity"];
const text = tsv(HEADER, rows);

// Oracle pins, from the consumers' own records (not this reader):
//   fourier F-D11 (value.js COHESION §0hy): web pins value.js 4.2.0 exact and keyframes.js 6.3.0.
//   sk-v27 PLAN §1 / LEDGER LW1.USE-4: keyframes.js and daraksha resolve value.js 4.2.0.
const ORACLE = [
    ["fourier-analysis", "web/package.json", "@mkbabb/value.js", "4.2.0"],
    ["fourier-analysis", "web/package.json", "@mkbabb/keyframes.js", "6.3.0"],
    ["keyframes.js", "package.json", "@mkbabb/value.js", "4.2.0"],
    ["daraksha", "package.json", "@mkbabb/value.js", "4.2.0"],
];
const P = join(HERE, "PIN-CENSUS.tsv");
if (process.argv.includes("--check")) {
    let bad = 0;
    const onDisk = existsSync(P) ? parseTsv(readFileSync(P, "utf8")) : [];
    for (const [repo, manifest, pkg, v] of ORACLE) {
        const r = onDisk.find((x) => x.repo === repo && x.manifest === manifest && x.package === pkg);
        if (!r || !r.versions.split(" ").includes(v)) {
            console.log(`RED oracle ${repo} ${manifest} ${pkg}@${v}: ${r ? `recorded ${r.versions}` : "no row"}`);
            bad++;
        }
    }
    if (!existsSync(P)) (console.log("RED PIN-CENSUS.tsv: missing"), bad++);
    else if (readFileSync(P, "utf8") !== text) (console.log("RED PIN-CENSUS.tsv: differs from the regeneration"), bad++);
    if (!bad) {
        const red = rows.filter((r) => r.one_copy.startsWith("RED"));
        console.log(`GREEN PIN-CENSUS.tsv: ${rows.length} rows regenerated identically; oracle ${ORACLE.length}/${ORACLE.length}; one-copy gate RED on ${red.length} row(s)${red.length ? ": " + red.map((r) => `${r.repo}/${r.lockfile} ${r.package} [${r.versions}]`).join("; ") : ""}`);
    }
    process.exit(bad ? 1 : 0);
}
writeFileSync(P, text);
console.log(`wrote ${rows.length} pin rows`);
