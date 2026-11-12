#!/usr/bin/env node

import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const artifactRoot = resolve(
  root,
  "restart/skinny/tranches/sk-v26/artifacts",
);
const epochPath = resolve(artifactRoot, "W0-AUDIT-SOURCE-EPOCH.json");
const manifestPath = resolve(artifactRoot, "W0-AUDIT-MANIFEST.tsv");

function sha256(value) {
  return createHash("sha256").update(value).digest("hex");
}

function tsv(value) {
  return String(value).replaceAll("\t", " ").replaceAll("\n", "\\n");
}

const epoch = JSON.parse(readFileSync(epochPath, "utf8"));
if (epoch.schema !== "skv26-w0-audit-source-epoch-v1") {
  throw new Error(`unsupported audit source epoch ${epoch.schema}`);
}
const body = [
  epoch.columns.join("\t"),
  ...epoch.rows.map((row) =>
    epoch.columns.map((column) => tsv(row[column])).join("\t"),
  ),
  "",
].join("\n");
const actual = readFileSync(manifestPath, "utf8");
if (body !== actual) {
  throw new Error(
    `audit manifest drift: expected ${sha256(body)}, found ${sha256(actual)}`,
  );
}
const counts = Object.groupBy(epoch.rows, (row) => row.category);
for (const [category, rows] of Object.entries(counts).sort()) {
  process.stdout.write(`${category}\t${rows.length}\n`);
}
process.stdout.write(
  `TOTAL\t${epoch.rows.length}\nVERIFIED\t${sha256(actual)}\n`,
);
