#!/usr/bin/env node
// SERVED MODEL: claude-opus-5-5
//
// cli.ts — `bbnf gen <grammar.bbnf> --actions <module> --out <file.js> [--export <name>]
//            [--entries a,b,…] [--max-depth n] [--check] [--instrument]`
// Writes `<file.js>` and `<file.d.ts>` (the emitted parser; see gen.ts). The action kinds are read
// from the consumer's action-table module: its `--export` (default `actions`, else the default
// export), each entry `{ kind, fn }`. `--check` writes nothing and exits 1 when either file is not
// what the grammar and this emitter generate now. `--instrument` emits an evidence build with the
// per-rule profile (`profile()`, `resetProfile()`); a shipped module is never generated with it.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";
import type { ActionKind } from "./emit.js";
import { check, generate } from "./gen.js";

const USAGE = "usage: bbnf gen <grammar.bbnf> --actions <module> --out <file.js> [--export <name>] [--entries a,b] [--max-depth n] [--check] [--instrument]";

function fail(message: string): never {
    process.stderr.write(`bbnf: ${message}\n`);
    process.exit(2);
}

async function main(argv: string[]): Promise<number> {
    const [command, ...rest] = argv;
    if (command !== "gen") fail(USAGE);
    const flags = new Map<string, string>();
    const positional: string[] = [];
    let checkOnly = false, instrument = false;
    for (let i = 0; i < rest.length; i++) {
        const a = rest[i];
        if (a === "--check") checkOnly = true;
        else if (a === "--instrument") instrument = true;
        else if (a.startsWith("--")) { const v = rest[++i]; if (v === undefined) fail(`${a} needs a value\n${USAGE}`); flags.set(a.slice(2), v); }
        else positional.push(a);
    }
    const grammar = positional[0], actionsPath = flags.get("actions"), out = flags.get("out");
    if (positional.length !== 1 || actionsPath === undefined || out === undefined) fail(USAGE);
    const mod = (await import(pathToFileURL(resolve(actionsPath)).href)) as Record<string, unknown>;
    const name = flags.get("export");
    const table = (name !== undefined ? mod[name] : mod.actions ?? mod.default) as Record<string, { kind: ActionKind }> | undefined;
    if (table === undefined || table === null || typeof table !== "object") fail(`${actionsPath} exports no action table${name ? ` \`${name}\`` : ""}`);
    const maxDepth = flags.has("max-depth") ? Number(flags.get("max-depth")) : undefined;
    const generated = generate({
        grammar: resolve(grammar),
        read: (id) => readFileSync(id, "utf8"),
        actions: table,
        entries: flags.get("entries")?.split(",").filter((e) => e !== ""),
        maxDepth,
        instrument,
    });
    const js = resolve(out), dts = js.replace(/\.[cm]?js$/, "") + ".d.ts";
    if (checkOnly) {
        const drift = check(generated, {
            js: existsSync(js) ? readFileSync(js, "utf8") : undefined,
            dts: existsSync(dts) ? readFileSync(dts, "utf8") : undefined,
        });
        if (drift.length > 0) {
            process.stderr.write(`bbnf gen --check: ${out} is stale (${drift.join(", ")} differ); regenerate with \`bbnf gen\` (sha256 now ${generated.sha256})\n`);
            return 1;
        }
        process.stdout.write(`bbnf gen --check: ${out} is current (sha256 ${generated.sha256})\n`);
        return 0;
    }
    writeFileSync(js, generated.js);
    writeFileSync(dts, generated.dts);
    process.stdout.write(`bbnf gen: wrote ${out} (+ .d.ts), sha256 ${generated.sha256}\n`);
    return 0;
}

main(process.argv.slice(2)).then((code) => process.exit(code), (e: unknown) => fail(e instanceof Error ? e.message : String(e)));
