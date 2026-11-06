// LW1.USE-5: the value.js `files` gap, born-RED. X-8 makes value.js's six
// WPT-gated grammars canonical and pins bbnf-lang's `grammar/corpus/value-js/`
// to a RELEASED value.js tarball with checked hashes (LW3.USE-1). That needs the
// `.bbnf` sources in the tarball; today `files` is `dist` only.
//
// This check stays RED until value.js publishes a tarball carrying every
// `src/css/grammar/*.bbnf` of its tagged commit, byte-identical. The cure is a
// value.js `package.json` edit, a held path (PLAN §1: wf_d837f96c's hold, then
// value.js's own app-work hold). It is filed, not made, here.
//
//   node use5-files-gap.mjs [version]   (default: the newest tarball under ~/.dev-logs/bbnf-lockstep/consumers/tarballs)
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { homedir } from "node:os";

const T = join(homedir(), ".dev-logs/bbnf-lockstep/consumers/tarballs");
const VJ = join(homedir(), "Programming/value.js");
const versions = readdirSync(T)
    .map((f) => f.match(/^mkbabb-value\.js-(\d+\.\d+\.\d+)\.tgz$/)?.[1])
    .filter(Boolean)
    .sort((a, b) => a.localeCompare(b, undefined, { numeric: true }));
const version = process.argv[2] ?? versions.at(-1);
const tgz = join(T, `mkbabb-value.js-${version}.tgz`);
const tarSha = createHash("sha256").update(readFileSync(tgz)).digest("hex");
const listed = execFileSync("tar", ["tzf", tgz], { encoding: "utf8" }).split("\n").filter(Boolean);
const tag = `v${version}`;
const want = execFileSync("git", ["-C", VJ, "ls-tree", "--name-only", `${tag}`, "src/css/grammar/"], { encoding: "utf8" })
    .split("\n")
    .filter((f) => f.endsWith(".bbnf"));
let bad = 0;
const missing = [];
for (const f of want) {
    const inTar = `package/${f}`;
    if (!listed.includes(inTar)) {
        missing.push(f);
        continue;
    }
    const a = execFileSync("tar", ["xzOf", tgz, inTar]);
    const b = execFileSync("git", ["-C", VJ, "show", `${tag}:${f}`]);
    if (!a.equals(b)) (console.log(`RED ${f}: tarball bytes differ from ${tag}`), bad++);
}
const pkgFiles = JSON.parse(execFileSync("git", ["-C", VJ, "show", `${tag}:package.json`], { encoding: "utf8" })).files;
if (missing.length) {
    console.log(`RED value.js ${version} tarball (sha256 ${tarSha.slice(0, 16)}…): ${missing.length}/${want.length} grammar sources absent (${missing.map((f) => f.split("/").pop()).join(" ")}); ${tag} package.json files = ${JSON.stringify(pkgFiles)}`);
    bad++;
}
if (!bad) console.log(`GREEN value.js ${version}: all ${want.length} grammar sources ship byte-identical to ${tag}`);
process.exit(bad ? 1 : 0);
