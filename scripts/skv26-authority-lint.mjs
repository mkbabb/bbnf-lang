#!/usr/bin/env node

import { createHash } from "node:crypto";
import {
  readdirSync,
  readFileSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { dirname, isAbsolute, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptPath = fileURLToPath(import.meta.url);
const root = resolve(dirname(scriptPath), "..");
const authorityArgument = process.argv
  .slice(2)
  .find((value) => value !== "--write-scan");
const authorityPath = resolve(
  authorityArgument ??
    `${root}/restart/skinny/tranches/sk-v26/artifacts/W0-AUTHORITY.tsv`,
);
const trancheRoot = resolve(root, "restart/skinny/tranches/sk-v26");
const dispatchScanPath = resolve(
  trancheRoot,
  "artifacts/W0-DISPATCH-SCAN.tsv",
);
const writeScan = process.argv.includes("--write-scan");
const lines = readFileSync(authorityPath, "utf8").trimEnd().split("\n");
const columns = lines[0].split("\t");
const rows = lines.slice(1).map((line, index) => {
  const fields = line.split("\t");
  if (fields.length !== columns.length) {
    throw new Error(
      `${authorityPath}:${index + 2}: expected ${columns.length} columns, found ${fields.length}`,
    );
  }
  return Object.fromEntries(columns.map((column, offset) => [column, fields[offset]]));
});

const seenIds = new Set();
const seenPaths = new Set();
for (const row of rows) {
  if (seenIds.has(row.id)) throw new Error(`duplicate authority id ${row.id}`);
  if (seenPaths.has(row.path)) throw new Error(`duplicate authority path ${row.path}`);
  seenIds.add(row.id);
  seenPaths.add(row.path);
  if (!/^[0-9a-f]{64}$/.test(row.sha256)) {
    throw new Error(`${row.id}: sha256 is not a full lowercase digest`);
  }
  if (row.authority_class === "ACTIVE_CONTRACT" && !row.path.includes("sk-v26/")) {
    throw new Error(`${row.id}: ACTIVE_CONTRACT lies outside sk-v26`);
  }
  if (row.id.startsWith("H") && row.dispatchable !== "NO") {
    throw new Error(`${row.id}: historical evidence is dispatchable`);
  }
}

const dispatchable = rows.filter((row) => row.dispatchable === "YES");
if (dispatchable.length !== 1) {
  throw new Error(`expected one dispatchable row, found ${dispatchable.length}`);
}
if (
  dispatchable[0].authority_class !== "STATE" ||
  dispatchable[0].path !== "restart/skinny/tranches/sk-v26/STATE.md"
) {
  throw new Error("the sole dispatchable row is not STATE authority");
}
const currentWave = dispatchable[0].current_wave;
const activeContracts = rows.filter(
  (row) =>
    row.authority_class === "ACTIVE_CONTRACT" &&
    row.current_wave === currentWave,
);
if (activeContracts.length !== 1) {
  throw new Error(
    `expected one ${currentWave} ACTIVE_CONTRACT, found ${activeContracts.length}`,
  );
}

for (const row of rows.filter((candidate) => candidate.id.startsWith("A"))) {
  const path = isAbsolute(row.path) ? row.path : resolve(root, row.path);
  if (!statSync(path).isFile()) {
    throw new Error(`${row.id}: indexed authority is not a file: ${path}`);
  }
  const actual = createHash("sha256").update(readFileSync(path)).digest("hex");
  if (actual !== row.sha256) {
    throw new Error(`${row.id}: hash mismatch: expected ${row.sha256}, found ${actual}`);
  }
}

const scanExtensions = new Set([".md", ".tsv"]);
const keywordPattern =
  /\b(active|next|ready|closed|converged|dispatch|execute|begin|start|implement)\b/gi;
const strongPattern =
  /^(?:#{1,6}\s*)?(?:exact\s+)?next\s+(?:action|command)\b|^(?:[-*]\s*)?(?:dispatch|execute|begin|start|implement)\s*[:：]|\b(?:state|status)\s*[:=]\s*(?:active|ready|in_progress)\b|\|\s*(?:active|ready|in_progress)\s*\|/i;
const scanRows = [];

function scanClass(path) {
  if (path === "STATE.md") return "STATE_AUTHORITY";
  if (path.startsWith("epochs/")) return "HISTORICAL_EPOCH";
  if (path.startsWith("waves/")) return "NONDISPATCH_ACCEPTANCE_CONTRACT";
  if (path === "artifacts/W0-REPLAY.md") return "NONDISPATCH_REPLAY_PROCEDURE";
  return "NONDISPATCH_POLICY_OR_EVIDENCE";
}

function walk(directory) {
  for (const entry of readdirSync(directory, { withFileTypes: true }).sort(
    (left, right) => left.name.localeCompare(right.name),
  )) {
    const absolute = join(directory, entry.name);
    if (entry.isDirectory()) {
      if (entry.name === "corpora") continue;
      walk(absolute);
      continue;
    }
    const path = relative(trancheRoot, absolute);
    if (
      path === "artifacts/W0-DISPATCH-SCAN.tsv" ||
      ![...scanExtensions].some((extension) => path.endsWith(extension))
    ) {
      continue;
    }
    for (const [offset, line] of readFileSync(absolute, "utf8")
      .split("\n")
      .entries()) {
      const keywords = [...line.matchAll(keywordPattern)].map((match) =>
        match[1].toLowerCase(),
      );
      if (keywords.length === 0) continue;
      keywordPattern.lastIndex = 0;
      const classification = scanClass(path);
      const strong = strongPattern.test(line);
      if (
        strong &&
        ![
          "STATE_AUTHORITY",
          "HISTORICAL_EPOCH",
          "NONDISPATCH_REPLAY_PROCEDURE",
        ].includes(classification)
      ) {
        throw new Error(
          `${path}:${offset + 1}: unallowlisted dispatch-shaped language: ${line}`,
        );
      }
      scanRows.push({
        path,
        line: offset + 1,
        keywords: [...new Set(keywords)].sort().join(","),
        classification,
        strong: strong ? "YES" : "NO",
        lineSha256: createHash("sha256").update(line).digest("hex"),
      });
    }
  }
}

walk(trancheRoot);
const scanBody = [
  "path\tline\tkeywords\tclassification\tdispatch_shaped\tline_sha256",
  ...scanRows.map((row) =>
    [
      row.path,
      row.line,
      row.keywords,
      row.classification,
      row.strong,
      row.lineSha256,
    ].join("\t"),
  ),
  "",
].join("\n");
if (writeScan) {
  writeFileSync(dispatchScanPath, scanBody);
} else if (readFileSync(dispatchScanPath, "utf8") !== scanBody) {
  throw new Error(
    "dispatch scan drift; inspect and regenerate with --write-scan",
  );
}

process.stdout.write(
  `authority rows=${rows.length} dispatchable=${dispatchable[0].id} current_wave=${currentWave} active_contract=${activeContracts[0].id} verified_files=${rows.filter((row) => row.id.startsWith("A")).length} dispatch_occurrences=${scanRows.length}\n`,
);
