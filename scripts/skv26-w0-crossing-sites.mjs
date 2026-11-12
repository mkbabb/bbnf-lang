#!/usr/bin/env node

import { createHash } from "node:crypto";
import { readdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const output = resolve(
  root,
  "restart/skinny/tranches/sk-v26/artifacts/W0-CROSSING-SITES.tsv",
);
const verifyOnly = process.argv.includes("--verify");
const rows = [];

function sha256(value) {
  return createHash("sha256").update(value).digest("hex");
}

function classify(path, line) {
  if (path.endsWith("Cargo.toml")) {
    return {
      facility: line.includes("bbnf-regex")
        ? "BBNF_REGEX_CRATE_EDGE"
        : "PARSE_THAT_CRATE_EDGE",
      disposition: line.includes("bbnf-regex") ? "REPLACEABLE" : "REQUIRED",
      replacement:
        "minimal independently packaged runtime plus compile-time regex authority",
    };
  }
  if (path.includes("/generated/") || path.includes("grammar/generated/")) {
    return {
      facility: "LEGACY_GENERATED_PARSER_ABI",
      disposition: "RETIRE",
      replacement: "explicit Run and checked terminal-plan imports",
    };
  }
  if (
    /\b(ParserState|ParserFn|Parser)\b|Span::new|PaddedView/.test(line)
  ) {
    return {
      facility: "LEGACY_MUTABLE_PARSE_ABI",
      disposition: "RETIRE",
      replacement: "invocation-owned Run with checked Input Cursor and Span",
    };
  }
  if (/bbnf_regex|parse_that::regex|RegexInfo|RegexClass|EngineSet|Dfa|Nfa/.test(line)) {
    return {
      facility: "REGEX_COMPILER_OR_ENGINE_SURFACE",
      disposition: "REPLACEABLE",
      replacement: "compile-time authority emitting opaque checked terminal plan",
    };
  }
  if (/parse_that::\*|use\s+parse_that\s*::\s*\*/.test(line)) {
    return {
      facility: "WILDCARD_RUNTIME_IMPORT",
      disposition: "RETIRE",
      replacement: "explicit selected runtime imports",
    };
  }
  return {
    facility: "SCANNER_OR_RUNTIME_HELPER",
    disposition: "REPLACEABLE",
    replacement: "selected terminal scalar codec or standard-library primitive",
  };
}

function walk(directory) {
  for (const entry of readdirSync(directory, { withFileTypes: true }).sort(
    (left, right) => left.name.localeCompare(right.name),
  )) {
    if (
      entry.isDirectory() &&
      [".git", "target", "node_modules", "restart"].includes(entry.name)
    ) {
      continue;
    }
    const absolute = join(directory, entry.name);
    if (entry.isDirectory()) {
      walk(absolute);
      continue;
    }
    if (!(entry.name.endsWith(".rs") || entry.name === "Cargo.toml")) continue;
    const path = relative(root, absolute);
    for (const [offset, line] of readFileSync(absolute, "utf8")
      .split("\n")
      .entries()) {
      if (
        !/\bparse_that\b|\bparse_that::|\bbbnf[_-]regex\b|\bbbnf_regex::|\bParserState\b|\bParserFn\b|\bParser\s*<|Span::new|PaddedView/.test(
          line,
        )
      ) {
        continue;
      }
      const classification = classify(path, line);
      rows.push({
        path,
        line: offset + 1,
        lineSha256: sha256(line),
        ...classification,
      });
    }
  }
}

walk(root);
rows.sort(
  (left, right) =>
    left.path.localeCompare(right.path) || left.line - right.line,
);
const body = [
  "path\tline\tline_sha256\tfacility\tdisposition\treplacement",
  ...rows.map((row) =>
    [
      row.path,
      row.line,
      row.lineSha256,
      row.facility,
      row.disposition,
      row.replacement,
    ].join("\t"),
  ),
  "",
].join("\n");

if (verifyOnly) {
  const actual = readFileSync(output, "utf8");
  if (actual !== body) {
    throw new Error(
      `crossing census drift: expected ${sha256(body)}, found ${sha256(actual)}`,
    );
  }
} else {
  writeFileSync(output, body);
}

const counts = Object.groupBy(rows, (row) => row.disposition);
process.stdout.write(
  `crossing sites=${rows.length} files=${new Set(rows.map((row) => row.path)).size} required=${counts.REQUIRED?.length ?? 0} replaceable=${counts.REPLACEABLE?.length ?? 0} retired=${counts.RETIRE?.length ?? 0} sha256=${sha256(body)} ${verifyOnly ? "VERIFIED" : "WRITTEN"}\n`,
);
