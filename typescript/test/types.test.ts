// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import ts from "typescript";

import { generate } from "../src/gen.js";

/**
 * E-5: the generated module and its `.d.ts` pass `tsc --strict` (the `.js` checked as JavaScript,
 * `checkJs`), and the typed `Actions` table rejects a missing action and an action of the wrong kind.
 */
const strict: ts.CompilerOptions = {
    strict: true, noEmit: true, allowJs: true, checkJs: true, skipLibCheck: false, noUnusedLocals: false,
    target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ESNext, moduleResolution: ts.ModuleResolutionKind.Bundler,
    lib: ["lib.es2022.d.ts"], types: [],
};
/** Every diagnostic tsc reports over `files`, as `file:line: message`. */
function tsc(files: string[]): string[] {
    const program = ts.createProgram(files, strict);
    return ts.getPreEmitDiagnostics(program).map((d) => {
        const at = d.file && d.start !== undefined ? d.file.getLineAndCharacterOfPosition(d.start).line + 1 : 0;
        return `${d.file ? d.file.fileName.split("/").pop() : "?"}:${at}: ${ts.flattenDiagnosticMessageText(d.messageText, "\n")}`;
    });
}

describe("the generated module's types", () => {
    const dir = mkdtempSync(join(tmpdir(), "bbnf-types-"));
    const read = (id: string) => readFileSync(id, "utf8");
    const write = (name: string, g: { js: string; dts: string }) => {
        writeFileSync(join(dir, `${name}.js`), g.js);
        writeFileSync(join(dir, `${name}.d.ts`), g.dts);
        return join(dir, `${name}.js`);
    };

    it("value.js's grammar (160 rules; map, span and text actions): module + .d.ts pass tsc --strict", () => {
        const file = write("css", generate({
            grammar: new URL("./fixtures/value-js/css.bbnf", import.meta.url).pathname, read,
            actions: { colorTop: { kind: "map" }, valueTop: { kind: "span" }, ident: { kind: "text" } },
            entries: ["colorTop", "valueTop", "ruleList"],
        }));
        expect(tsc([file, file.replace(/\.js$/, ".d.ts")])).toEqual([]);
    });

    it("Actions accepts the right table and rejects a missing action or a wrong kind", () => {
        writeFileSync(join(dir, "sum.bbnf"), `num = /[0-9]+/ ;\nsum = num , ( "+" >> num ) * ;\n`);
        write("sum", generate({ grammar: join(dir, "sum.bbnf"), read, actions: { num: { kind: "text" } }, entries: ["sum", "num"] }));
        const consumer = join(dir, "consumer.ts");
        writeFileSync(consumer, [
            `import { createParser, FAIL } from "./sum.js";`,
            `const p = createParser({ num: { kind: "text", fn: (t: string) => Number(t) } });`,
            `const n: number | typeof FAIL = p.entries.num("1");`,
            `const end: number = p.rules.sum("1+2", 0);`,
            `void n; void end;`,
            `createParser({});`, // line 6: missing action
            `createParser({ num: { kind: "map", fn: (v: unknown) => v } });`, // line 7: wrong kind
            `createParser({ num: { kind: "text", fn: (t: number) => t } });`, // line 8: wrong signature
            `p.entries.nope("1");`, // line 9: not an entry
        ].join("\n") + "\n");
        const lines = tsc([consumer]).map((d) => d.split(":").slice(0, 2).join(":"));
        expect([...new Set(lines)].sort()).toEqual(["consumer.ts:6", "consumer.ts:7", "consumer.ts:8", "consumer.ts:9"]);
    });
});
