#!/usr/bin/env node

import { createHash } from "node:crypto";
import {
  lstatSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  readlinkSync,
  renameSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { basename, dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptPath = fileURLToPath(import.meta.url);
const bbnfWorktree = resolve(dirname(scriptPath), "..");
if (process.argv.includes("--verify")) {
  throw new Error(
    "live audit inputs are mutable; use scripts/skv26-w0-audit-verify.mjs after capture",
  );
}
const outputArgument = process.argv[2];
const programmingRoot =
  process.env.SKV26_PROGRAMMING_ROOT ?? "/Users/mkbabb/Programming";
const home = process.env.HOME;
const bbnfMain =
  process.env.SKV26_BBNF_MAIN ?? join(programmingRoot, "bbnf-lang");
const parseThatMain =
  process.env.SKV26_PARSE_THAT_MAIN ?? join(programmingRoot, "parse-that");
const valueMain =
  process.env.SKV26_VALUE_MAIN ?? join(programmingRoot, "value.js");
const atlasRoot =
  process.env.SKV26_ATLAS_ROOT ??
  join(
    programmingRoot,
    ".p-totality/sci/atlas/docs/tranches/Q/audit/2026-07-24-perfection",
  );
const output = resolve(
  outputArgument ??
    join(
      bbnfWorktree,
      "restart/skinny/tranches/sk-v26/artifacts/W0-AUDIT-MANIFEST.tsv",
    ),
);
const epochOutput = resolve(
  bbnfWorktree,
  "restart/skinny/tranches/sk-v26/artifacts/W0-AUDIT-SOURCE-EPOCH.json",
);

if (!home) {
  throw new Error("HOME is required to locate retained session logs");
}

const skipDirectories = new Set([
  ".git",
  ".test-dist",
  "dist",
  "node_modules",
  "target",
]);
const rows = [];

function sha256Bytes(bytes) {
  return createHash("sha256").update(bytes).digest("hex");
}

function sha256File(file) {
  return sha256Bytes(readFileSync(file));
}

function isoTime(value) {
  return new Date(value).toISOString();
}

function sessionTimes(file) {
  const text = readFileSync(file, "utf8");
  const times = [];
  for (const line of text.split("\n")) {
    if (!line) continue;
    const match = line.match(
      /"timestamp"\s*:\s*"([^"]+)"|"created_at"\s*:\s*"([^"]+)"/,
    );
    const value = match?.[1] ?? match?.[2];
    if (value && !Number.isNaN(Date.parse(value))) {
      times.push(Date.parse(value));
    }
  }
  if (times.length === 0) {
    const stat = statSync(file);
    return [isoTime(stat.mtimeMs), isoTime(stat.mtimeMs)];
  }
  return [isoTime(Math.min(...times)), isoTime(Math.max(...times))];
}

function directoryLedger(root) {
  const entries = [];
  let first = Number.POSITIVE_INFINITY;
  let last = 0;

  function walk(directory) {
    for (const entry of readdirSync(directory, { withFileTypes: true }).sort(
      (a, b) => a.name.localeCompare(b.name),
    )) {
      if (entry.isDirectory() && skipDirectories.has(entry.name)) continue;
      const absolute = join(directory, entry.name);
      if (entry.isDirectory()) {
        walk(absolute);
        continue;
      }
      const stat = lstatSync(absolute);
      first = Math.min(first, stat.mtimeMs);
      last = Math.max(last, stat.mtimeMs);
      const rel = relative(root, absolute);
      const digest = entry.isSymbolicLink()
        ? sha256Bytes(`symlink:${readlinkSync(absolute)}`)
        : sha256File(absolute);
      entries.push(`${rel}\t${digest}`);
    }
  }

  walk(root);
  const ledger = `${entries.join("\n")}\n`;
  return {
    digest: sha256Bytes(ledger),
    files: entries.length,
    first:
      first === Number.POSITIVE_INFINITY ? "UNKNOWN" : isoTime(first),
    last: last === 0 ? "UNKNOWN" : isoTime(last),
  };
}

function addRow(row) {
  rows.push({
    category: row.category,
    identity: row.identity,
    project: row.project,
    path: row.path,
    date_start: row.date_start,
    date_end: row.date_end,
    inclusion_reason: row.inclusion_reason,
    inclusion_decision:
      row.inclusion_decision ??
      (row.category.endsWith("_ABSENCE")
        ? "EXCLUDED_ABSENCE_PROOF"
        : "INCLUDED_IDENTITY_OR_ROOT_CENSUS"),
    semantic_state: row.semantic_state,
    content_sha256: row.content_sha256,
    file_count: String(row.file_count ?? 1),
    extractor_sha256: row.extractor_sha256 ?? "N_A",
    replay: row.replay,
  });
}

const replay =
  "node scripts/skv26-w0-audit-verify.mjs";
const claudeExtractor = join(
  atlasRoot,
  "instruments/anamnesis-extract.py",
);
const claudeExtractorSha = sha256File(claudeExtractor);

for (const [project, directory] of [
  [
    "bbnf-lang",
    join(
      home,
      ".claude/projects/-Users-mkbabb-Programming-bbnf-lang",
    ),
  ],
  [
    "value.js",
    join(home, ".claude/projects/-Users-mkbabb-Programming-value-js"),
  ],
]) {
  const files = readdirSync(directory)
    .filter((name) => name.endsWith(".jsonl"))
    .sort();
  for (const name of files) {
    const file = join(directory, name);
    const [dateStart, dateEnd] = sessionTimes(file);
    addRow({
      category: "CLAUDE_OWNER_ROOT",
      identity: name.slice(0, -".jsonl".length),
      project,
      path: file,
      date_start: dateStart,
      date_end: dateEnd,
      inclusion_reason: "RELEVANT_PROJECT_ROOT",
      semantic_state:
        project === "bbnf-lang"
          ? "IDENTITY_HASHED_REVIEW_UNRECEIPTED"
          : "LOSSLESS_OWNER_EXTRACTION_RECEIPTED_WITH_PREFIX_CAVEAT",
      content_sha256: sha256File(file),
      extractor_sha256: claudeExtractorSha,
      replay,
    });
  }
}

addRow({
  category: "CLAUDE_ABSENCE",
  identity: "parse-that-root-jsonl",
  project: "parse-that",
  path: join(
    home,
    ".claude/projects/-Users-mkbabb-Programming-parse-that",
  ),
  date_start: "UNKNOWN",
  date_end: "UNKNOWN",
  inclusion_reason: "PROJECT_DIRECTORY_CONTAINS_ZERO_ROOT_JSONL",
  semantic_state: "ABSENT",
  content_sha256: "ABSENT",
  file_count: 0,
  extractor_sha256: claudeExtractorSha,
  replay,
});

const codexCorpus = join(atlasRoot, "anamnesis/codex/CORPUS-103.json");
const codexScript = join(atlasRoot, "instruments/codex-anamnesis.mjs");
const codexScriptSha = sha256File(codexScript);
const codex = JSON.parse(readFileSync(codexCorpus, "utf8"));
for (const record of [...codex.records].sort((a, b) =>
  a.id.localeCompare(b.id),
)) {
  const exists = (() => {
    try {
      return statSync(record.file).isFile();
    } catch {
      return false;
    }
  })();
  addRow({
    category: "CODEX_OWNER_ROOT",
    identity: record.id,
    project: record.project ?? record.cwd ?? "UNKNOWN",
    path: record.file,
    date_start: record.timestamp ?? "UNKNOWN",
    date_end: record.timestamp ?? "UNKNOWN",
    inclusion_reason: "CURRENT_USER_OWNED_ROOT_CENSUS",
    semantic_state: "MECHANICAL_CENSUS_NOT_SEMANTIC_ADJUDICATION",
    content_sha256: exists ? sha256File(record.file) : "MISSING",
    extractor_sha256: codexScriptSha,
    replay,
  });
}

addRow({
  category: "CODEX_ABSENCE",
  identity: "formation-era-47-root-cohort",
  project: "bbnf-lang+value.js",
  path: codexCorpus,
  date_start: "2026-03-04",
  date_end: "2026-05-24",
  inclusion_reason: "FORMATION_CLAIM_HAS_NO_DURABLE_ROW_MANIFEST",
  semantic_state: "ABSENT_UNRECONSTRUCTIBLE",
  content_sha256: sha256File(codexCorpus),
  extractor_sha256: codexScriptSha,
  replay,
});

function immediateDirectories(parent, predicate = () => true) {
  return readdirSync(parent, { withFileTypes: true })
    .filter((entry) => entry.isDirectory() && predicate(entry.name))
    .map((entry) => join(parent, entry.name))
    .sort();
}

const trancheRoots = new Set();
for (const root of immediateDirectories(
  join(bbnfMain, "docs/tranches"),
  (name) => !["archive", "meta-audit"].includes(name),
)) {
  trancheRoots.add(root);
}
for (const root of immediateDirectories(
  join(bbnfMain, "restart/skinny/tranches"),
  (name) => /^sk-v\d+$/.test(name),
)) {
  trancheRoots.add(root);
}
for (const root of immediateDirectories(join(parseThatMain, "docs/tranches"))) {
  trancheRoots.add(root);
}
for (const root of immediateDirectories(join(valueMain, "docs/tranches"))) {
  trancheRoots.add(root);
}
for (const root of immediateDirectories(
  join(bbnfMain, "docs/tranches/archive"),
  (name) => name.startsWith("pre-restart-"),
)) {
  trancheRoots.add(root);
}
for (const root of immediateDirectories(
  join(bbnfMain, "restart-archive-2026-05-04/tranches"),
  (name) => /^[A-J]$/.test(name),
)) {
  trancheRoots.add(root);
}

for (const root of [...trancheRoots].sort()) {
  const ledger = directoryLedger(root);
  addRow({
    category: "TRANCHE_ROOT",
    identity: basename(root),
    project: root.startsWith(parseThatMain)
      ? "parse-that"
      : root.startsWith(valueMain)
        ? "value.js"
        : "bbnf-lang",
    path: root,
    date_start: ledger.first,
    date_end: ledger.last,
    inclusion_reason: "DISTINCT_CHRONOLOGICAL_ROOT_CONVENTION",
    semantic_state: "IDENTIFIED_NOT_ROW_ADJUDICATED",
    content_sha256: ledger.digest,
    file_count: ledger.files,
    replay,
  });
}

const columns = [
  "category",
  "identity",
  "project",
  "path",
  "date_start",
  "date_end",
  "inclusion_reason",
  "inclusion_decision",
  "semantic_state",
  "content_sha256",
  "file_count",
  "extractor_sha256",
  "replay",
];
function tsv(value) {
  return String(value).replaceAll("\t", " ").replaceAll("\n", "\\n");
}
const body = [
  columns.join("\t"),
  ...rows
    .sort(
      (a, b) =>
        a.category.localeCompare(b.category) ||
        a.project.localeCompare(b.project) ||
        a.identity.localeCompare(b.identity),
    )
    .map((row) => columns.map((column) => tsv(row[column])).join("\t")),
  "",
].join("\n");

mkdirSync(dirname(output), { recursive: true });
const temporary = `${output}.tmp`;
writeFileSync(temporary, body);
renameSync(temporary, output);
const epochTemporary = `${epochOutput}.tmp`;
writeFileSync(
  epochTemporary,
  `${JSON.stringify(
    {
      schema: "skv26-w0-audit-source-epoch-v1",
      columns,
      rows: rows.sort(
        (a, b) =>
          a.category.localeCompare(b.category) ||
          a.project.localeCompare(b.project) ||
          a.identity.localeCompare(b.identity),
      ),
    },
    null,
    2,
  )}\n`,
);
renameSync(epochTemporary, epochOutput);

const counts = Object.groupBy(rows, (row) => row.category);
for (const [category, categoryRows] of Object.entries(counts).sort()) {
  process.stdout.write(`${category}\t${categoryRows.length}\n`);
}
process.stdout.write(`TOTAL\t${rows.length}\n`);
process.stdout.write(`WRITTEN\t${sha256Bytes(body)}\n`);
