#!/usr/bin/env node

import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const bbnfRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const parseThatRoot =
  process.env.SKV26_PARSE_THAT_WORKTREE ??
  resolve(bbnfRoot, "../parse-that-skv26");
const pprintRoot =
  process.env.SKV26_PPRINT_WORKTREE ??
  resolve(bbnfRoot, "../pprint-skv26");
const artifactRoot = join(
  bbnfRoot,
  "restart/skinny/tranches/sk-v26/artifacts",
);
const overlayPath = join(artifactRoot, "W0-OVERLAYS.tsv");
const receiptPath = join(artifactRoot, "W0-RECEIPT.tsv");
const verifyOnly = process.argv.includes("--verify");

const repositories = [
  {
    name: "bbnf-lang-skv26",
    root: bbnfRoot,
    base: "af15f63e0d2d3d719938c13b906a50acbb92ea3b",
    commit: "47d51a190fda7f51686d5bad1be149191bbfeba6",
  },
  {
    name: "parse-that-skv26",
    root: parseThatRoot,
    base: "ef10d5b78236c4a30a7bb22a6113b60bdc4bdf42",
    commit: "627c9b46a773a278ed8c60728c9d335be6747905",
  },
];
const dependency = {
  name: "pprint-skv26",
  root: pprintRoot,
  commit: "2b4d2d48657ebc4eb4c23297929ad3532e3eb6b4",
};

function sha256(value) {
  return createHash("sha256").update(value).digest("hex");
}

function git(root, args, encoding = null) {
  const result = spawnSync("git", ["-C", root, ...args], {
    encoding,
    maxBuffer: 64 * 1024 * 1024,
  });
  if (result.status !== 0) {
    throw new Error(
      `git ${args.join(" ")} failed in ${root}: ${result.stderr.toString("utf8")}`,
    );
  }
  return result.stdout;
}

function parseNameStatus(buffer) {
  const records = buffer.toString("utf8").split("\0");
  const rows = [];
  for (let index = 0; index < records.length; ) {
    const status = records[index++];
    if (!status) continue;
    if (status.startsWith("R") || status.startsWith("C")) {
      rows.push({
        status,
        original: records[index++] ?? "",
        path: records[index++] ?? "",
      });
    } else {
      rows.push({ status, original: "", path: records[index++] ?? "" });
    }
  }
  return rows;
}

function commitTree(repository) {
  return git(repository.root, ["rev-parse", `${repository.commit}^{tree}`], "utf8").trim();
}

function overlayBytes() {
  const rows = [];
  for (const repository of repositories) {
    const changes = parseNameStatus(
      git(repository.root, [
        "diff",
        "--name-status",
        "-z",
        repository.base,
        repository.commit,
      ]),
    );
    for (const change of changes) {
      if (change.status.startsWith("D")) {
        rows.push({
          repository: repository.name,
          base: repository.base,
          commit: repository.commit,
          ...change,
          mode: "DELETED",
          blob: "DELETED",
          bytes: 0,
          digest: "DELETED",
        });
        continue;
      }
      const object = `${repository.commit}:${change.path}`;
      const bytes = git(repository.root, ["show", object]);
      const lsTree = git(
        repository.root,
        ["ls-tree", repository.commit, "--", change.path],
        "utf8",
      )
        .trim()
        .split(/\s+/);
      rows.push({
        repository: repository.name,
        base: repository.base,
        commit: repository.commit,
        ...change,
        mode: lsTree[0],
        blob: lsTree[2],
        bytes: bytes.length,
        digest: sha256(bytes),
      });
    }
  }
  rows.sort((left, right) =>
    `${left.repository}\0${left.path}`.localeCompare(
      `${right.repository}\0${right.path}`,
    ),
  );
  return Buffer.from(
    [
      "repository\tbase_commit\tw0_commit\tstatus\tpath\toriginal_path\tmode\tgit_blob\tbytes\tcontent_sha256",
      ...rows.map((row) =>
        [
          row.repository,
          row.base,
          row.commit,
          row.status,
          row.path,
          row.original,
          row.mode,
          row.blob,
          row.bytes,
          row.digest,
        ].join("\t"),
      ),
      "",
    ].join("\n"),
  );
}

function artifactAtCommit(repository, path) {
  const bytes = git(repository.root, ["show", `${repository.commit}:${path}`]);
  return {
    path,
    bytes: bytes.length,
    digest: sha256(bytes),
  };
}

function receiptBytes(overlay) {
  const rows = [];
  for (const repository of repositories) {
    rows.push([
      "REPOSITORY",
      repository.name,
      "base_commit",
      repository.base,
    ]);
    rows.push([
      "REPOSITORY",
      repository.name,
      "w0_commit",
      repository.commit,
    ]);
    rows.push([
      "REPOSITORY",
      repository.name,
      "w0_tree",
      commitTree(repository),
    ]);
  }
  rows.push([
    "DEPENDENCY",
    dependency.name,
    "commit",
    dependency.commit,
  ]);
  rows.push([
    "DEPENDENCY",
    dependency.name,
    "tree",
    git(dependency.root, ["rev-parse", `${dependency.commit}^{tree}`], "utf8").trim(),
  ]);
  rows.push([
    "SEAL",
    "W0-OVERLAYS.tsv",
    "sha256",
    sha256(overlay),
  ]);
  rows.push([
    "SEAL",
    "W0-OVERLAYS.tsv",
    "rows",
    String(overlay.toString("utf8").trimEnd().split("\n").length - 1),
  ]);

  const bbnf = repositories[0];
  for (const path of [
    "restart/skinny/tranches/sk-v26/FORMATION-RECEIPT.sha256",
    "restart/skinny/tranches/sk-v26/artifacts/W0-AUDIT-SOURCE-EPOCH.json",
    "restart/skinny/tranches/sk-v26/artifacts/W0-AUDIT-MANIFEST.tsv",
    "restart/skinny/tranches/sk-v26/artifacts/W0-AUTHORITY.tsv",
    "restart/skinny/tranches/sk-v26/artifacts/W0-CROSSING-SITES.tsv",
    "restart/skinny/tranches/sk-v26/artifacts/W0-HISTORY-CLASSIFICATION.tsv",
    "restart/skinny/tranches/sk-v26/artifacts/W0-SEMANTIC-PLANES.tsv",
    "restart/skinny/tranches/sk-v26/corpora/MANIFEST.tsv",
  ]) {
    const artifact = artifactAtCommit(bbnf, path);
    rows.push([
      "ARTIFACT",
      artifact.path,
      `${artifact.bytes}_bytes_sha256`,
      artifact.digest,
    ]);
  }
  rows.sort((left, right) => left.join("\0").localeCompare(right.join("\0")));
  return Buffer.from(
    [
      "kind\tidentity\tfield\tvalue",
      ...rows.map((row) => row.join("\t")),
      "",
    ].join("\n"),
  );
}

const expectedOverlay = overlayBytes();
const expectedReceipt = receiptBytes(expectedOverlay);

if (verifyOnly) {
  const actualOverlay = readFileSync(overlayPath);
  const actualReceipt = readFileSync(receiptPath);
  if (!actualOverlay.equals(expectedOverlay)) {
    throw new Error(
      `overlay mismatch: expected ${sha256(expectedOverlay)}, found ${sha256(actualOverlay)}`,
    );
  }
  if (!actualReceipt.equals(expectedReceipt)) {
    throw new Error(
      `receipt mismatch: expected ${sha256(expectedReceipt)}, found ${sha256(actualReceipt)}`,
    );
  }
} else {
  writeFileSync(overlayPath, expectedOverlay);
  writeFileSync(receiptPath, expectedReceipt);
}

process.stdout.write(
  `W0 git-object seal ${verifyOnly ? "verified" : "written"} overlay=${sha256(expectedOverlay)} receipt=${sha256(expectedReceipt)} rows=${expectedOverlay.toString("utf8").trimEnd().split("\n").length - 1}\n`,
);
