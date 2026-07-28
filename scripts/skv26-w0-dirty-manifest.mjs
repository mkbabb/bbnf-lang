#!/usr/bin/env node

import { createHash } from "node:crypto";
import {
  lstatSync,
  mkdirSync,
  readFileSync,
  readlinkSync,
  renameSync,
  writeFileSync,
} from "node:fs";
import { dirname, join, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const scriptPath = fileURLToPath(import.meta.url);
const worktree = resolve(dirname(scriptPath), "..");
const verifyOnly = process.argv.includes("--verify");
const programmingRoot =
  process.env.SKV26_PROGRAMMING_ROOT ?? "/Users/mkbabb/Programming";
const artifactRoot = join(
  worktree,
  "restart/skinny/tranches/sk-v26/artifacts",
);

const repositories = [
  {
    name: "bbnf-main",
    path:
      process.env.SKV26_BBNF_MAIN ??
      join(programmingRoot, "bbnf-lang"),
    output: "W0-DIRTY-BBNF-MAIN.tsv",
    owner: "BBNF_PROTECTED_MAIN_OWNER_UNATTRIBUTED_PER_PATH",
    provenance: "BBNF_PROTECTED_FILESYSTEM_EPOCH_2026-07-28",
  },
  {
    name: "parse-that-main",
    path:
      process.env.SKV26_PARSE_THAT_MAIN ??
      join(programmingRoot, "parse-that"),
    output: "W0-DIRTY-PARSE-THAT-MAIN.tsv",
    owner: "PARSE_THAT_PROTECTED_MAIN_OWNER_UNATTRIBUTED_PER_PATH",
    provenance: "PARSE_THAT_PROTECTED_FILESYSTEM_EPOCH_2026-07-28",
  },
];

function sha256Bytes(bytes) {
  return createHash("sha256").update(bytes).digest("hex");
}

function fileReceipt(path) {
  try {
    const stat = lstatSync(path);
    if (stat.isSymbolicLink()) {
      return ["SYMLINK", sha256Bytes(readlinkSync(path))];
    }
    if (stat.isFile()) {
      return ["FILE", sha256Bytes(readFileSync(path))];
    }
    if (stat.isDirectory()) {
      return ["DIRECTORY", "EXPANDED_BY_GIT_UALL"];
    }
    return ["OTHER", "UNHASHED"];
  } catch (error) {
    if (error?.code === "ENOENT") return ["MISSING", "MISSING"];
    throw error;
  }
}

function parsePorcelain(buffer) {
  const records = buffer.toString("utf8").split("\0");
  const rows = [];
  for (let index = 0; index < records.length; index += 1) {
    const record = records[index];
    if (!record) continue;
    const status = record.slice(0, 2);
    const path = record.slice(3);
    let original = "";
    if (status.includes("R") || status.includes("C")) {
      original = records[index + 1] ?? "";
      index += 1;
    }
    rows.push({ status, path, original });
  }
  return rows;
}

for (const repository of repositories) {
  const result = spawnSync(
    "git",
    [
      "-C",
      repository.path,
      "status",
      "--porcelain=v1",
      "-z",
      "--untracked-files=all",
    ],
    { encoding: null },
  );
  if (result.status !== 0) {
    throw new Error(
      `git status failed for ${repository.path}: ${result.stderr}`,
    );
  }

  const rows = parsePorcelain(result.stdout).map((row) => {
    const [kind, digest] = fileReceipt(join(repository.path, row.path));
    return { ...row, kind, digest };
  });
  const header =
    "repository\tstatus\tpath\toriginal_path\tkind\tcontent_sha256\towner\tprovenance\n";
  const body = rows
    .map((row) =>
      [
        repository.name,
        row.status,
        row.path,
        row.original,
        row.kind,
        row.digest,
        repository.owner,
        repository.provenance,
      ]
        .map((value) =>
          String(value).replaceAll("\t", " ").replaceAll("\n", "\\n"),
        )
        .join("\t"),
    )
    .join("\n");
  const output = join(artifactRoot, repository.output);
  const expected = `${header}${body}\n`;
  if (verifyOnly) {
    const actual = readFileSync(output, "utf8");
    if (actual !== expected) {
      throw new Error(
        `${repository.name} manifest drift: expected ${sha256Bytes(expected)}, found ${sha256Bytes(actual)}`,
      );
    }
  } else {
    mkdirSync(artifactRoot, { recursive: true });
    const temporary = `${output}.tmp`;
    writeFileSync(temporary, expected);
    renameSync(temporary, output);
  }
  process.stdout.write(
    `${repository.name}\t${rows.length}\t${sha256Bytes(result.stdout)}\t${verifyOnly ? "VERIFIED" : "WRITTEN"}\n`,
  );
}
