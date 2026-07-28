#!/usr/bin/env node

import { createHash } from "node:crypto";
import {
  copyFileSync,
  mkdirSync,
  readFileSync,
  renameSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptPath = fileURLToPath(import.meta.url);
const worktree = resolve(dirname(scriptPath), "..");
const sourceRoot =
  process.env.SKV26_CORPUS_SOURCE ??
  "/Users/mkbabb/Programming/bbnf-lang/data";
const parseThatSourceRoot =
  process.env.SKV26_PARSE_THAT_CORPUS_SOURCE ??
  "/Users/mkbabb/Programming/parse-that/data";
const targetRoot = join(
  worktree,
  "restart/skinny/tranches/sk-v26/corpora",
);

const corpus = [
  [sourceRoot, "json/canada.json", "json/canada.json", 2251051, "f83b3b354030d5dd58740c68ac4fecef64cb730a0d12a90362a7f23077f50d78"],
  [sourceRoot, "json/citm_catalog.json", "json/citm_catalog.json", 1727204, "a73e7a883f6ea8de113dff59702975e60119b4b58d451d518a929f31c92e2059"],
  [sourceRoot, "json/data.json", "json/data.json", 35491, "aaf8a8b2a42fbe7c95c1960b7736e5ea16276b4331b017712f724ebd8628f4ec"],
  [sourceRoot, "json/twitter.json", "json/twitter.json", 631514, "a08b769f32b95f426cbc3abafcec65c1a19d3eb544d4ddf320eae142c99efc5d"],
  [sourceRoot, "css/bootstrap.css", "css/bootstrap.css", 280311, "4a50207b956a4ab943640ee993118b554a34e96a23261cfe58b9aa1807a7849b"],
  [sourceRoot, "css/normalize.css", "css/normalize.css", 6138, "580818700724d42d7fcc4979b0197971fca1c6d2e0286769237a0ac897df5512"],
  [sourceRoot, "css/tailwind.css", "css/tailwind.css", 3642321, "de305d923dde1f8559e7734bb1f01970af3a76efc844b94ca3c8488fea8da3f7"],
  [parseThatSourceRoot, "json/data-l.json", "parse-that/json/data-l.json", 3798443, "02be45299748ea80cca9aa10f17f6856d64799af5f01a396808d28f628ad7229"],
  [parseThatSourceRoot, "json/data.json", "parse-that/json/data.json", 35482, "122f5b62815ee01fa4e3cc61a82af39d623f12119f028f42fbe879d0ab1e8f4c"],
  [parseThatSourceRoot, "csv/data.csv", "parse-that/csv/data.csv", 1409, "cd602842ee6ddf451fd3c0c1d3047b3ee7ec1325f17ae0ef7529eafe724391ca"],
  [parseThatSourceRoot, "css/normalize.css", "parse-that/css/normalize.css", 6138, "580818700724d42d7fcc4979b0197971fca1c6d2e0286769237a0ac897df5512"],
  [parseThatSourceRoot, "css/bootstrap.css", "parse-that/css/bootstrap.css", 281046, "18a105d7cb38e01e5ed0ca255c092992a2e211b39594a7fa57262bfc6fc4ea9c"],
  [parseThatSourceRoot, "css/tailwind-output.css", "parse-that/css/tailwind-output.css", 3791278, "cd6e8eacbfd6f2ddcc6d8e6e1306c387d1ed695455d95431a41d1a425de7740e"],
];

function sha256(path) {
  return createHash("sha256").update(readFileSync(path)).digest("hex");
}

const rows = [];
for (const [
  sourceDirectory,
  sourceRelativePath,
  targetRelativePath,
  expectedBytes,
  expectedHash,
] of corpus) {
  const source = join(sourceDirectory, sourceRelativePath);
  const sourceBytes = statSync(source).size;
  const sourceHash = sha256(source);
  if (sourceBytes !== expectedBytes || sourceHash !== expectedHash) {
    throw new Error(
      `${targetRelativePath}: source receipt mismatch: bytes=${sourceBytes} sha256=${sourceHash}`,
    );
  }
  const target = join(targetRoot, targetRelativePath);
  mkdirSync(dirname(target), { recursive: true });
  const temporary = `${target}.tmp`;
  copyFileSync(source, temporary);
  renameSync(temporary, target);
  const targetHash = sha256(target);
  if (targetHash !== expectedHash) {
    throw new Error(`${targetRelativePath}: copied hash mismatch: ${targetHash}`);
  }
  rows.push([
    targetRelativePath,
    String(expectedBytes),
    expectedHash,
    "LOCAL_IGNORED_SOURCE_CONTENT_ADDRESSED",
  ]);
}

const manifest = join(targetRoot, "MANIFEST.tsv");
const body = [
  "path\tbytes\tsha256\tprovenance",
  ...rows.map((row) => row.join("\t")),
  "",
].join("\n");
const temporaryManifest = `${manifest}.tmp`;
writeFileSync(temporaryManifest, body);
renameSync(temporaryManifest, manifest);
process.stdout.write(
  `imported=${rows.length} bytes=${rows.reduce((sum, row) => sum + Number(row[1]), 0)} manifest=${manifest}\n`,
);
