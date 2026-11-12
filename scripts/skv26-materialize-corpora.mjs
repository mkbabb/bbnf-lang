#!/usr/bin/env node

import { createHash } from "node:crypto";
import {
  lstatSync,
  mkdirSync,
  readFileSync,
  readlinkSync,
  symlinkSync,
} from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const bbnfRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const parseThatRoot =
  process.env.SKV26_PARSE_THAT_WORKTREE ??
  resolve(bbnfRoot, "../parse-that-skv26");
const corpusRoot = join(
  bbnfRoot,
  "restart/skinny/tranches/sk-v26/corpora",
);
const verifyOnly = process.argv.includes("--verify");

const mappings = [
  ["bbnf-lang-skv26", bbnfRoot, "data/json/canada.json", "json/canada.json"],
  ["bbnf-lang-skv26", bbnfRoot, "data/json/citm_catalog.json", "json/citm_catalog.json"],
  ["bbnf-lang-skv26", bbnfRoot, "data/json/data.json", "json/data.json"],
  ["bbnf-lang-skv26", bbnfRoot, "data/json/twitter.json", "json/twitter.json"],
  ["bbnf-lang-skv26", bbnfRoot, "data/css/bootstrap.css", "css/bootstrap.css"],
  ["bbnf-lang-skv26", bbnfRoot, "data/css/normalize.css", "css/normalize.css"],
  ["bbnf-lang-skv26", bbnfRoot, "data/css/tailwind.css", "css/tailwind.css"],
  [
    "parse-that-skv26",
    parseThatRoot,
    "data/json/data-l.json",
    "parse-that/json/data-l.json",
  ],
  [
    "parse-that-skv26",
    parseThatRoot,
    "data/json/data.json",
    "parse-that/json/data.json",
  ],
  [
    "parse-that-skv26",
    parseThatRoot,
    "data/csv/data.csv",
    "parse-that/csv/data.csv",
  ],
  [
    "parse-that-skv26",
    parseThatRoot,
    "data/css/normalize.css",
    "parse-that/css/normalize.css",
  ],
  [
    "parse-that-skv26",
    parseThatRoot,
    "data/css/bootstrap.css",
    "parse-that/css/bootstrap.css",
  ],
  [
    "parse-that-skv26",
    parseThatRoot,
    "data/css/tailwind-output.css",
    "parse-that/css/tailwind-output.css",
  ],
];

function sha256(path) {
  return createHash("sha256").update(readFileSync(path)).digest("hex");
}

for (const [repository, root, targetRelative, sourceRelative] of mappings) {
  const source = join(corpusRoot, sourceRelative);
  const target = join(root, targetRelative);
  const link = relative(dirname(target), source);
  if (!verifyOnly) {
    mkdirSync(dirname(target), { recursive: true });
    try {
      const stat = lstatSync(target);
      if (!stat.isSymbolicLink() || readlinkSync(target) !== link) {
        throw new Error(
          `${target}: existing path is not the expected materialized symlink`,
        );
      }
    } catch (error) {
      if (error?.code !== "ENOENT") throw error;
      symlinkSync(link, target);
    }
  }
  const stat = lstatSync(target);
  if (!stat.isSymbolicLink() || readlinkSync(target) !== link) {
    throw new Error(`${target}: materialization drift`);
  }
  const sourceHash = sha256(source);
  const targetHash = sha256(target);
  if (targetHash !== sourceHash) {
    throw new Error(
      `${repository}:${targetRelative}: expected ${sourceHash}, found ${targetHash}`,
    );
  }
}

process.stdout.write(
  `corpus materialization ${verifyOnly ? "verified" : "written"} mappings=${mappings.length}\n`,
);
