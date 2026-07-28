#!/usr/bin/env node

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  mkdirSync,
  readFileSync,
  writeFileSync,
} from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const tranche = join(root, "restart/skinny/tranches/sk-v26");
const epochPath = join(tranche, "artifacts/W0-EPOCH.tsv");

function argument(name) {
  const offset = process.argv.indexOf(name);
  if (offset === -1) return undefined;
  const value = process.argv[offset + 1];
  if (!value || value.startsWith("--")) {
    throw new Error(`${name} requires a value`);
  }
  return value;
}

function sha256(value) {
  return createHash("sha256").update(value).digest("hex");
}

function git(...args) {
  return execFileSync("git", ["-C", root, ...args], {
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
}

function sealedBbnfCommit() {
  const rows = readFileSync(epochPath, "utf8")
    .trimEnd()
    .split("\n")
    .slice(1)
    .map((line) => line.split("\t"));
  const row = rows.find(
    ([repository, role]) => repository === "bbnf-lang" && role === "w0_payload",
  );
  if (!row) {
    throw new Error("W0-EPOCH.tsv has no bbnf-lang w0_payload row");
  }
  return row[4];
}

function locationAt(source, offset) {
  let line = 1;
  let column = 1;
  for (let index = 0; index < offset; index += 1) {
    if (source.charCodeAt(index) === 10) {
      line += 1;
      column = 1;
    } else {
      column += 1;
    }
  }
  return { line, column };
}

/**
 * Extract slash-delimited regex terminals from a BBNF grammar.
 *
 * The scanner deliberately recognizes only the grammar envelope: quoted
 * strings and line/block comments are skipped, while regex payloads retain
 * their authored escapes byte-for-byte. A slash closes a regex only when it
 * is neither escaped nor inside a character class.
 */
function regexOccurrences(source, path) {
  const occurrences = [];
  let index = 0;

  while (index < source.length) {
    const byte = source[index];
    const next = source[index + 1];

    if (byte === '"' || byte === "'") {
      const quote = byte;
      index += 1;
      while (index < source.length) {
        if (source[index] === "\\") {
          index += 2;
        } else if (source[index] === quote) {
          index += 1;
          break;
        } else {
          index += 1;
        }
      }
      continue;
    }

    if (byte === "/" && next === "/") {
      const newline = source.indexOf("\n", index + 2);
      index = newline === -1 ? source.length : newline + 1;
      continue;
    }

    if (byte === "/" && next === "*") {
      const close = source.indexOf("*/", index + 2);
      if (close === -1) {
        throw new Error(`${path}: unterminated block comment`);
      }
      index = close + 2;
      continue;
    }

    if (byte !== "/") {
      index += 1;
      continue;
    }

    const start = index;
    index += 1;
    let escaped = false;
    let inClass = false;
    let closed = false;

    while (index < source.length) {
      const current = source[index];
      if (escaped) {
        escaped = false;
        index += 1;
        continue;
      }
      if (current === "\\") {
        escaped = true;
        index += 1;
        continue;
      }
      if (current === "[" && !inClass) {
        inClass = true;
        index += 1;
        continue;
      }
      if (current === "]" && inClass) {
        inClass = false;
        index += 1;
        continue;
      }
      if (current === "/" && !inClass) {
        const pattern = source.slice(start + 1, index);
        const location = locationAt(source, start);
        occurrences.push({ path, ...location, pattern });
        index += 1;
        closed = true;
        break;
      }
      index += 1;
    }

    if (!closed) {
      const location = locationAt(source, start);
      throw new Error(
        `${path}:${location.line}:${location.column}: unterminated regex terminal`,
      );
    }
  }

  return occurrences;
}

const sealedCommit = argument("--ref") ?? sealedBbnfCommit();
const sealedTree = git("rev-parse", `${sealedCommit}^{tree}`).trim();
const outputDirectory = resolve(
  argument("--out-dir") ?? join(tranche, "artifacts"),
);
const verifyOnly = process.argv.includes("--verify");
const trackedGrammarPaths = git("ls-tree", "-r", "--name-only", sealedCommit)
  .trimEnd()
  .split("\n")
  .filter((path) => path.endsWith(".bbnf"))
  .sort((left, right) => left.localeCompare(right));

const occurrences = [];
for (const path of trackedGrammarPaths) {
  const source = git("show", `${sealedCommit}:${path}`);
  occurrences.push(...regexOccurrences(source, path));
}

occurrences.sort(
  (left, right) =>
    left.path.localeCompare(right.path) ||
    left.line - right.line ||
    left.column - right.column ||
    left.pattern.localeCompare(right.pattern),
);

const caseByPattern = new Map();
for (const [offset, occurrence] of occurrences.entries()) {
  occurrence.occurrenceId = `R${String(offset + 1).padStart(4, "0")}`;
  let entry = caseByPattern.get(occurrence.pattern);
  if (!entry) {
    entry = {
      pattern: occurrence.pattern,
      patternSha256: sha256(occurrence.pattern),
      occurrences: [],
    };
    caseByPattern.set(occurrence.pattern, entry);
  }
  entry.occurrences.push(occurrence.occurrenceId);
}

const cases = [...caseByPattern.values()].sort(
  (left, right) =>
    left.patternSha256.localeCompare(right.patternSha256) ||
    left.pattern.localeCompare(right.pattern),
);
for (const [offset, entry] of cases.entries()) {
  entry.caseId = `C${String(offset + 1).padStart(4, "0")}`;
}
const caseIdByPattern = new Map(cases.map((entry) => [entry.pattern, entry.caseId]));

const metadata = [
  `# schema=skv26-regex-corpus-v1`,
  `# source_repository=bbnf-lang`,
  `# source_commit=${sealedCommit}`,
  `# source_tree=${sealedTree}`,
  `# selector=git-tracked:**/*.bbnf`,
  `# grammar_files=${trackedGrammarPaths.length}`,
];
const occurrenceBody = [
  ...metadata,
  `# occurrence_count=${occurrences.length}`,
  "occurrence_id\tcase_id\tpattern_sha256\tsource_path\tline\tcolumn\tpattern_utf8_hex",
  ...occurrences.map((occurrence) =>
    [
      occurrence.occurrenceId,
      caseIdByPattern.get(occurrence.pattern),
      sha256(occurrence.pattern),
      occurrence.path,
      occurrence.line,
      occurrence.column,
      Buffer.from(occurrence.pattern, "utf8").toString("hex"),
    ].join("\t"),
  ),
  "",
].join("\n");
const caseBody = [
  ...metadata,
  `# case_count=${cases.length}`,
  "case_id\tpattern_sha256\toccurrence_count\toccurrence_ids\tpattern_utf8_hex",
  ...cases.map((entry) =>
    [
      entry.caseId,
      entry.patternSha256,
      entry.occurrences.length,
      entry.occurrences.join(","),
      Buffer.from(entry.pattern, "utf8").toString("hex"),
    ].join("\t"),
  ),
  "",
].join("\n");

mkdirSync(outputDirectory, { recursive: true });
const outputs = [
  [join(outputDirectory, "W1-REGEX-OCCURRENCES.tsv"), occurrenceBody],
  [join(outputDirectory, "W1-REGEX-CASES.tsv"), caseBody],
];

for (const [path, body] of outputs) {
  if (verifyOnly) {
    const actual = readFileSync(path, "utf8");
    if (actual !== body) {
      throw new Error(
        `${path} drift: expected ${sha256(body)}, found ${sha256(actual)}`,
      );
    }
  } else {
    writeFileSync(path, body);
  }
}

process.stdout.write(
  [
    `regex grammar_files=${trackedGrammarPaths.length}`,
    `occurrences=${occurrences.length}`,
    `cases=${cases.length}`,
    `source=${sealedCommit}`,
    `occurrences_sha256=${sha256(occurrenceBody)}`,
    `cases_sha256=${sha256(caseBody)}`,
    verifyOnly ? "VERIFIED" : "WRITTEN",
    "",
  ].join(" "),
);
