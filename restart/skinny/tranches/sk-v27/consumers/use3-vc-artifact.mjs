// LW2.USE-3: LG-d row 0's same-artifact conformance. value.js's own V-C judge
// (bench/wpt-conformance/conformance.ts at the release tag) is bundled twice:
//   tarball  its `src/css` and `src/color` imports resolved to the PUBLISHED tarball's dist/subpaths/{css,color}.js
//   source   the same judge over the tag's own src (the control: the tarball must judge exactly as its source)
// The tarball is checked against its sha256 before anything runs; the tag's tree is read through git.
//
//   node use3-vc-artifact.mjs judge [version]   write records/use3-vc-<version>.txt (the tarball's V-C table)
//   node use3-vc-artifact.mjs --check           RED unless BENEFIT-ROW0's value row cites ONE tarball sha256 for
//                                               its bytes and its conformance, and that conformance is this judge's
import { execFileSync, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import { HERE, parseTsv } from "./lib.mjs";

const LOG = join(homedir(), ".dev-logs/bbnf-lockstep/consumers");
const VJ = join(homedir(), "Programming/value.js");
const ESBUILD = resolve(HERE, "../../../../../typescript/node_modules/esbuild/lib/main.js");
export const TARBALLS = {
    "4.3.0": { sha256: "7b3146927731a26ae2a7c1d53c87b302b2d72f76f8b5ccfed1cbb469e0a40908", tag: "v4.3.0" },
};
const sha256 = (b) => createHash("sha256").update(b).digest("hex");
export const recordPath = (v) => join(HERE, `records/use3-vc-${v}.txt`);

function table(out) {
    // the judge's table and verdict lines, without the per-miss listing
    return out.split("\n").filter((l) => /^(V-C|module|TOTAL|[a-z0-9-]+\s+\d)/.test(l)).join("\n");
}

async function judge(version) {
    const t = TARBALLS[version];
    if (!t) throw new Error(`no tarball row for ${version}`);
    const tgz = join(LOG, `tarballs/mkbabb-value.js-${version}.tgz`);
    const got = sha256(readFileSync(tgz));
    if (got !== t.sha256) throw new Error(`tarball ${version} sha256 ${got} != ${t.sha256}`);
    const work = join(LOG, `vc-${version}`);
    rmSync(work, { recursive: true, force: true });
    mkdirSync(join(work, "tar"), { recursive: true });
    execFileSync("tar", ["-xzf", tgz, "-C", join(work, "tar")]);
    execFileSync("sh", ["-c", `git -C "${VJ}" archive ${t.tag} bench/wpt-conformance test/css/wpt-values src package.json | tar -x -C "${work}"`]);
    const npmVersion = JSON.parse(readFileSync(join(work, "tar/package/package.json"), "utf8")).version;
    if (npmVersion !== version) throw new Error(`tarball says ${npmVersion}`);
    const { build } = await import(ESBUILD);
    const judgeTs = join(work, "bench/wpt-conformance/conformance.ts");
    const dist = join(work, "tar/package/dist/subpaths");
    const toTarball = { name: "to-tarball", setup(b) {
        b.onResolve({ filter: /^\.\.\/\.\.\/src\/(css|color)\/index$/ }, (a) => (a.importer === judgeTs ? { path: join(dist, `${a.path.split("/")[3]}.js`) } : undefined));
    } };
    const res = {};
    for (const arm of ["tarball", "source"]) {
        const out = join(work, `judge-${arm}.mjs`);
        await build({ entryPoints: [judgeTs], outfile: out, bundle: true, platform: "node", format: "esm", target: "node22", logLevel: "warning",
            absWorkingDir: work, plugins: arm === "tarball" ? [toTarball] : [], define: { "import.meta.url": "__judgeUrl" },
            banner: { js: `const __judgeUrl = ${JSON.stringify("file://" + judgeTs)};` } });
        const bundle = readFileSync(out, "utf8");
        if (arm === "tarball" && bundle.includes("src/css/index.ts")) throw new Error("the tarball judge still bundles the tag's src/css");
        const r = spawnSync(process.execPath, [out], { cwd: work, encoding: "utf8", maxBuffer: 1 << 26 });
        res[arm] = { status: r.status, table: table(r.stdout), stderr: r.stderr.slice(0, 400) };
    }
    if (res.tarball.table !== res.source.table) throw new Error(`the tarball judges differently from ${t.tag}'s source:\n${res.tarball.table}\n---\n${res.source.table}`);
    const total = res.tarball.table.split("\n").find((l) => l.startsWith("TOTAL"));
    const text = `# LW2.USE-3: value.js V-C judged on the published ${version} tarball (sha256 ${t.sha256}); judge ${t.tag}:bench/wpt-conformance/conformance.ts with src/{css,color} resolved to the tarball's dist/subpaths; control: the same judge over ${t.tag}'s source gives the identical table; judge exit ${res.tarball.status}\n${res.tarball.table}\n`;
    mkdirSync(join(HERE, "records"), { recursive: true });
    writeFileSync(recordPath(version), text);
    console.log(text.trimEnd());
    console.log(`TOTAL row: ${total}`);
}

function check() {
    const rows = parseTsv(readFileSync(join(HERE, "BENEFIT-ROW0.tsv"), "utf8"));
    const v = rows.find((r) => r.consumer === "value");
    let bad = 0;
    const say = (s) => (console.log(s), bad++);
    if (!v) say("RED BENEFIT-ROW0 has no value row");
    else if (!("artifact_sha256" in v)) say(`RED value row 0 names no artifact sha256: bytes are of ${v.pinned_value}, conformance cites "${v.conformance.slice(0, 80)}…"`);
    else {
        const t = TARBALLS[v.pinned_value];
        if (!t || t.sha256 !== v.artifact_sha256) say(`RED value row 0 artifact ${v.artifact_sha256} is not the published ${v.pinned_value} tarball`);
        if (!v.conformance.includes(v.artifact_sha256)) say("RED value row 0's conformance cites a different artifact than its bytes");
        if (!existsSync(recordPath(v.pinned_value))) say(`RED no V-C record judged on the ${v.pinned_value} tarball`);
        else if (!readFileSync(recordPath(v.pinned_value), "utf8").includes(v.artifact_sha256)) say("RED the V-C record judged another artifact");
    }
    if (!bad) console.log(`GREEN value row 0: bytes and V-C both cite value.js ${v.pinned_value} tarball sha256 ${v.artifact_sha256.slice(0, 16)}…`);
    process.exit(bad ? 1 : 0);
}

const [, , cmd, arg] = process.argv;
if (cmd === "judge") await judge(arg ?? "4.3.0");
else if (cmd === "--check") check();
else console.log("usage: judge [version] | --check");
