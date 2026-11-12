#!/usr/bin/env node

import { createHash } from "node:crypto";
import {
  existsSync,
  readFileSync,
  writeFileSync,
} from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync, spawnSync } from "node:child_process";

const W0_PAYLOAD = "47d51a190fda7f51686d5bad1be149191bbfeba6";
const W0_ADMISSION = "f819a9657966e66493fc2be26bf80d5fe876259a";
const W0_CROSSING_PATH =
  "restart/skinny/tranches/sk-v26/artifacts/W0-CROSSING-SITES.tsv";
const W0_CONTRACT_PATH =
  "restart/skinny/tranches/sk-v26/artifacts/W0-CROSSING.tsv";
const ARTIFACT_DIR = "restart/skinny/tranches/sk-v26/artifacts";
const SCRIPT_DIR = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(SCRIPT_DIR, "..");
const VERIFY = process.argv.slice(2).includes("--verify");
const RECORD_COMPILE = process.argv.slice(2).includes("--record-compile");

const OUTPUTS = {
  paths: join(ROOT, ARTIFACT_DIR, "W1-CRATER.tsv"),
  sites: join(ROOT, ARTIFACT_DIR, "W1-CRATER-SITES.tsv"),
  summary: join(ROOT, ARTIFACT_DIR, "W1-CRATER-CENSUS.json"),
  compile: join(ROOT, ARTIFACT_DIR, "W1-CRATER-COMPILE.tsv"),
};

function git(args, encoding = "utf8") {
  return execFileSync("git", ["-C", ROOT, ...args], {
    encoding,
    maxBuffer: 64 * 1024 * 1024,
  });
}

function sha256(bytes) {
  return createHash("sha256").update(bytes).digest("hex");
}

function sortedObject(entries) {
  return Object.fromEntries(
    [...entries].sort(([left], [right]) => left.localeCompare(right)),
  );
}

function increment(map, key, amount = 1) {
  map.set(key, (map.get(key) ?? 0) + amount);
}

function cleanField(value) {
  return String(value).replaceAll("\t", " ").replaceAll("\n", " ");
}

function facilityContract(facility) {
  const contracts = {
    BBNF_REGEX_CRATE_EDGE: "DEP-02/DEP-06/DEP-07/DEP-08",
    LEGACY_GENERATED_PARSER_ABI: "API-01/API-02/API-03",
    LEGACY_MUTABLE_PARSE_ABI: "API-02/API-03",
    PARSE_THAT_CRATE_EDGE: "DEP-01/DEP-03/DEP-04/DEP-05/DEP-09/DEP-10/DEP-11",
    REGEX_COMPILER_OR_ENGINE_SURFACE: "REGEX-01/REGEX-02/REGEX-03/REGEX-04/REGEX-06",
    SCANNER_OR_RUNTIME_HELPER: "API-04/API-05",
    WILDCARD_RUNTIME_IMPORT: "API-01/DEP-11",
  };
  const contract = contracts[facility];
  if (contract === undefined) {
    throw new Error(`unclassified W0 facility: ${facility}`);
  }
  return contract;
}

function isUnreachable(path) {
  return (
    path.startsWith("docs/benchmarks/archive/") ||
    path.startsWith("docs/tranches/AZ-II/archive/") ||
    path.startsWith("docs/tranches/B2/audit/")
  );
}

function isBanked(path) {
  const exact = new Set([
    "crates/core/tests/backend_ts.rs",
    "crates/core/tests/gen_ts_parser.rs",
    "crates/core/tests/ts_node_execute.rs",
  ]);
  return (
    path.startsWith("skinny/") ||
    path.startsWith("crates/core/benches/") ||
    path.startsWith("crates/core/src/backend/ts/") ||
    exact.has(path)
  );
}

function explicitDisposition(path) {
  if (path === ".cargo/config.toml") {
    return {
      disposition: "REQUIRED",
      contracts: "DEP-01/DEP-02",
      rationale:
        "retain the isolated runtime and compile-time regex package bindings; no engine surface crosses the runtime package",
    };
  }

  if (path === "Cargo.toml" || path === "crates/core/Cargo.toml") {
    return {
      disposition: "REQUIRED",
      contracts: "DEP-03/DEP-04",
      rationale:
        "retain one parse_that runtime edge for the selected Rust vertical; retire the duplicate core dev edge during W2",
    };
  }

  if (
    path.startsWith("crates/bbnf-path/") ||
    path.startsWith("crates/bbnf-path-ts/")
  ) {
    return {
      disposition: "RETIRED",
      contracts: "DEP-07/DEP-08/REGEX-04",
      rationale:
        "remove the regex/path crossing; the path consumer itself remains a separately triggered banked lane",
    };
  }

  if (path === "crates/gorgeous/Cargo.toml") {
    return {
      disposition: "RETIRED",
      contracts: "DEP-09",
      rationale:
        "remove the unused diagnostics dependency; gorgeous is outside the scalar crossing",
    };
  }

  if (path === "wasm/Cargo.toml") {
    return {
      disposition: "RETIRED",
      contracts: "DEP-10",
      rationale:
        "remove the unused parse_that dependency; Wasm is outside the selected vertical",
    };
  }

  if (
    path === "crates/analysis/src/state/parsing.rs" ||
    path === "crates/egraph/src/cost_weights.rs" ||
    path === "crates/ir/src/cost_config.rs" ||
    path.startsWith("crates/ir/src/egraph/") ||
    path.startsWith("crates/ir/src/passes/csp_strategy/")
  ) {
    return {
      disposition: "RETIRED",
      contracts: "API-02/REGEX-03",
      rationale:
        "delete the mutable-parser or regex egraph/CSP crossing; direct sealed algorithms are the W2 authority",
    };
  }

  if (path === "crates/ir/Cargo.toml") {
    return {
      disposition: "REPLACED",
      contracts: "DEP-05/DEP-06",
      rationale:
        "replace IR imports of the legacy parser and regex engine with sealed terminal facts and plans",
    };
  }

  if (path === "xtask/Cargo.toml") {
    return {
      disposition: "REPLACED",
      contracts: "DEP-11",
      rationale:
        "retain regeneration ownership but replace its legacy runtime dependency with the explicit W1 contract",
    };
  }

  if (
    path === "crates/core/src/generate/regex/emit/dfa/accel.rs" ||
    path === "crates/core/src/generate/regex/emit/simd.rs"
  ) {
    return {
      disposition: "RETIRED",
      contracts: "API-06/REGEX-06",
      rationale:
        "remove public acceleration coupling; acceleration is internal and profile-gated after the scalar vertical",
    };
  }

  return undefined;
}

function classifyPath(path, sites) {
  if (isUnreachable(path)) {
    return {
      disposition: "UNREACHABLE",
      contracts: "W1-EXCLUDED/ARCHIVE",
      rationale:
        "tracked historical snapshot outside the Cargo workspace and every selected production package",
    };
  }

  if (isBanked(path)) {
    return {
      disposition: "BANKED",
      contracts: "W1-EXCLUDED/R2/R5",
      rationale:
        "prototype, benchmark, or TypeScript/backend lane excluded from the selected Rust scalar vertical",
    };
  }

  const explicit = explicitDisposition(path);
  if (explicit !== undefined) {
    return explicit;
  }

  const dispositions = new Set(sites.map((site) => site.w0Disposition));
  const facilities = [...new Set(sites.map((site) => site.facility))].sort();
  const contracts = facilities.map(facilityContract).join("/");

  if (dispositions.has("REQUIRED")) {
    return {
      disposition: "REQUIRED",
      contracts,
      rationale:
        "the selected Rust vertical retains this package/runtime crossing, narrowed to the W1 kernel",
    };
  }
  if (dispositions.has("REPLACEABLE")) {
    return {
      disposition: "REPLACED",
      contracts,
      rationale:
        "W2 must replace this legacy crossing with checked Run, sealed terminal plans, or explicit scalar operations",
    };
  }
  if (dispositions.size === 1 && dispositions.has("RETIRE")) {
    return {
      disposition: "RETIRED",
      contracts,
      rationale:
        "the W0 crossing contract retires this legacy generated, mutable, or wildcard runtime surface",
    };
  }
  throw new Error(
    `unclassified W0 disposition mixture for ${path}: ${[...dispositions].join(",")}`,
  );
}

function readPayload(path) {
  return git(["show", `${W0_PAYLOAD}:${path}`], null);
}

function currentState(path, w0Bytes, trackedPaths) {
  if (!trackedPaths.has(path) || !existsSync(join(ROOT, path))) {
    return {
      state: "MISSING_FROM_CURRENT",
      sha256: "-",
    };
  }
  const currentBytes = readFileSync(join(ROOT, path));
  return {
    state:
      Buffer.compare(w0Bytes, currentBytes) === 0
        ? "SAME_AS_W0"
        : "MODIFIED_AFTER_W0",
    sha256: sha256(currentBytes),
  };
}

function render() {
  git(["merge-base", "--is-ancestor", W0_ADMISSION, "HEAD"]);
  const payloadTree = git(["rev-parse", `${W0_PAYLOAD}^{tree}`]).trim();
  const crossingBytes = readPayload(W0_CROSSING_PATH);
  const contractBytes = readPayload(W0_CONTRACT_PATH);
  const crossingText = crossingBytes.toString("utf8");
  const lines = crossingText.trimEnd().split("\n");
  const header = lines.shift();
  if (
    header !==
    "path\tline\tline_sha256\tfacility\tdisposition\treplacement"
  ) {
    throw new Error(`unexpected W0 crossing header: ${header}`);
  }

  const trackedPaths = new Set(
    git(["ls-files", "-z"])
      .split("\0")
      .filter(Boolean),
  );
  const paths = new Map();
  const sites = lines.map((line) => {
    const [path, lineNumber, lineSha256, facility, w0Disposition, replacement] =
      line.split("\t");
    if (
      [
        path,
        lineNumber,
        lineSha256,
        facility,
        w0Disposition,
        replacement,
      ].some((field) => field === undefined)
    ) {
      throw new Error(`malformed W0 crossing row: ${line}`);
    }
    const site = {
      path,
      line: Number(lineNumber),
      lineSha256,
      facility,
      w0Disposition,
      replacement,
      authority: "W0_CROSSING_SITES",
    };
    if (!Number.isSafeInteger(site.line) || site.line < 1) {
      throw new Error(`invalid line number in W0 crossing row: ${line}`);
    }
    const pathSites = paths.get(path) ?? [];
    pathSites.push(site);
    paths.set(path, pathSites);
    return site;
  });
  const canonicalPathCount = paths.size;
  const canonicalSiteCount = sites.length;
  const supplements = [
    {
      path: ".cargo/config.toml",
      line: 51,
      contains: "parse_that",
      facility: "PARSE_THAT_CRATE_EDGE",
      w0Disposition: "REQUIRED",
      replacement:
        "bind only ../parse-that-skv26/rust/parse_that in the isolated tree",
    },
    {
      path: ".cargo/config.toml",
      line: 52,
      contains: "bbnf-regex",
      facility: "BBNF_REGEX_CRATE_EDGE",
      w0Disposition: "REPLACEABLE",
      replacement:
        "retain bbnf-regex only as the compile-time authority emitting checked table plans",
    },
  ];
  for (const supplement of supplements) {
    const sourceLine = readPayload(supplement.path)
      .toString("utf8")
      .split("\n")[supplement.line - 1];
    if (!sourceLine.includes(supplement.contains)) {
      throw new Error(
        `W0 contract supplement no longer matches ${supplement.path}:${supplement.line}`,
      );
    }
    const site = {
      ...supplement,
      lineSha256: sha256(sourceLine),
      authority: "W0_CROSSING_CONTRACT",
    };
    const pathSites = paths.get(site.path) ?? [];
    pathSites.push(site);
    paths.set(site.path, pathSites);
    sites.push(site);
  }

  const pathRows = [];
  const pathClassifications = new Map();
  const pathDispositionCounts = new Map();
  const siteDispositionCounts = new Map();
  const facilityCounts = new Map();
  const currentStateCounts = new Map();

  for (const path of [...paths.keys()].sort()) {
    const pathSites = paths
      .get(path)
      .sort(
        (left, right) =>
          left.line - right.line ||
          left.authority.localeCompare(right.authority),
      );
    const classification = classifyPath(path, pathSites);
    pathClassifications.set(path, classification);
    increment(pathDispositionCounts, classification.disposition);
    for (const site of pathSites) {
      increment(siteDispositionCounts, classification.disposition);
      increment(facilityCounts, site.facility);
    }

    const w0Bytes = readPayload(path);
    const w0Lines = w0Bytes.toString("utf8").split("\n");
    for (const site of pathSites) {
      const sourceLine = w0Lines[site.line - 1];
      if (sourceLine === undefined || sha256(sourceLine) !== site.lineSha256) {
        throw new Error(
          `W0 line digest mismatch at ${site.path}:${site.line} (${site.authority})`,
        );
      }
    }
    const current = currentState(path, w0Bytes, trackedPaths);
    increment(currentStateCounts, current.state);
    const facilities = new Map();
    const w0Dispositions = new Map();
    const authorities = new Map();
    for (const site of pathSites) {
      increment(facilities, site.facility);
      increment(w0Dispositions, site.w0Disposition);
      increment(authorities, site.authority);
    }
    const renderCounts = (map) =>
      [...map]
        .sort(([left], [right]) => left.localeCompare(right))
        .map(([name, count]) => `${name}:${count}`)
        .join(",");
    pathRows.push(
      [
        path,
        pathSites.length,
        renderCounts(authorities),
        pathSites[0].line,
        pathSites.at(-1).line,
        renderCounts(facilities),
        renderCounts(w0Dispositions),
        classification.disposition,
        classification.contracts,
        classification.rationale,
        sha256(w0Bytes),
        current.state,
        current.sha256,
      ]
        .map(cleanField)
        .join("\t"),
    );
  }

  const pathTsv = [
    [
      "path",
      "site_count",
      "site_authorities",
      "first_line",
      "last_line",
      "facilities",
      "w0_site_dispositions",
      "w1_disposition",
      "contract_refs",
      "rationale",
      "w0_source_sha256",
      "current_state",
      "current_source_sha256",
    ].join("\t"),
    ...pathRows,
    "",
  ].join("\n");

  const siteRows = [...sites]
    .sort(
      (left, right) =>
        left.path.localeCompare(right.path) ||
        left.line - right.line ||
        left.authority.localeCompare(right.authority),
    )
    .map((site) => {
    const classification = pathClassifications.get(site.path);
    return [
      site.path,
      site.line,
      site.lineSha256,
      site.authority,
      site.facility,
      site.w0Disposition,
      classification.disposition,
      classification.contracts,
      site.replacement,
    ]
      .map(cleanField)
      .join("\t");
    });
  const siteTsv = [
    [
      "path",
      "line",
      "line_sha256",
      "site_authority",
      "facility",
      "w0_disposition",
      "w1_path_disposition",
      "contract_refs",
      "replacement",
    ].join("\t"),
    ...siteRows,
    "",
  ].join("\n");

  const summary = `${JSON.stringify(
    {
      schema: "sk-v26-w1-crater-census-v1",
      source: {
        w0_payload_commit: W0_PAYLOAD,
        w0_payload_tree: payloadTree,
        w0_admission_commit: W0_ADMISSION,
        w0_crossing_path: W0_CROSSING_PATH,
        w0_crossing_sha256: sha256(crossingBytes),
        w0_contract_path: W0_CONTRACT_PATH,
        w0_contract_sha256: sha256(contractBytes),
      },
      totals: {
        canonical_w0_paths: canonicalPathCount,
        canonical_w0_sites: canonicalSiteCount,
        coarse_contract_supplement_paths: paths.size - canonicalPathCount,
        coarse_contract_supplement_sites: sites.length - canonicalSiteCount,
        paths: paths.size,
        sites: sites.length,
      },
      path_dispositions: sortedObject(pathDispositionCounts),
      site_dispositions: sortedObject(siteDispositionCounts),
      facilities: sortedObject(facilityCounts),
      current_states: sortedObject(currentStateCounts),
      artifacts: {
        "W1-CRATER.tsv": sha256(pathTsv),
        "W1-CRATER-SITES.tsv": sha256(siteTsv),
      },
      interpretation: {
        required:
          "retained minimal package/runtime crossing for the selected Rust vertical",
        replaced:
          "active legacy crossing that W2 must substitute with the admitted W1 contract",
        retired:
          "legacy crossing deleted rather than preserved or adapted",
        banked:
          "owned non-W1 lane with a later trigger; not a hidden W1 deliverable",
        unreachable:
          "tracked evidence/source snapshot outside the selected production build graph",
      },
    },
    null,
    2,
  )}\n`;

  return new Map([
    [OUTPUTS.paths, pathTsv],
    [OUTPUTS.sites, siteTsv],
    [OUTPUTS.summary, summary],
  ]);
}

const rendered = render();
if (RECORD_COMPILE) {
  const targetDir = join(ROOT, "target/skv26-w1-crater");
  const args = ["check", "-p", "bbnf", "--lib", "--locked"];
  const result = spawnSync("cargo", args, {
    cwd: ROOT,
    encoding: "utf8",
    env: {
      ...process.env,
      CARGO_TARGET_DIR: targetDir,
      CARGO_TERM_COLOR: "never",
    },
    maxBuffer: 64 * 1024 * 1024,
  });
  if (result.error !== undefined) {
    throw result.error;
  }
  const combined = `${result.stdout ?? ""}${result.stderr ?? ""}`;
  const normalized = combined
    .replaceAll(ROOT, "$SKV26_BBNF")
    .replaceAll("/Users/mkbabb/Programming/parse-that-skv26", "$SKV26_PT")
    .replaceAll(targetDir, "$SKV26_BBNF_TARGET");
  const expected =
    "package `gorgeous` depends on `parse_that` with feature `diagnostics` but `parse_that` does not have that feature.";
  if (result.status !== 101 || !normalized.includes(expected)) {
    throw new Error(
      `compile observation changed: exit=${result.status}\n${normalized}`,
    );
  }
  const compileTsv = [
    [
      "scope",
      "command",
      "exit_code",
      "status",
      "first_blocker",
      "source",
      "remainder",
      "normalized_output_sha256",
      "normalized_excerpt",
    ].join("\t"),
    [
      "bbnf_lib",
      "cargo check -p bbnf --lib --locked",
      result.status,
      "BLOCKED_AT_DEPENDENCY_RESOLUTION",
      "removed parse_that diagnostics feature requested by gorgeous",
      "crates/gorgeous/Cargo.toml:78",
      "UNREACHABLE_AFTER_FIRST_BLOCKER",
      sha256(normalized),
      expected,
    ]
      .map(cleanField)
      .join("\t"),
    "",
  ].join("\n");
  rendered.set(OUTPUTS.compile, compileTsv);
}
let failed = false;
for (const [path, content] of rendered) {
  if (VERIFY) {
    const actual = existsSync(path) ? readFileSync(path, "utf8") : undefined;
    if (actual !== content) {
      console.error(`DRIFT ${relative(ROOT, path)}`);
      failed = true;
    } else {
      console.log(`OK ${relative(ROOT, path)}`);
    }
  } else {
    writeFileSync(path, content);
    console.log(`WROTE ${relative(ROOT, path)}`);
  }
}
if (failed) {
  process.exitCode = 1;
}
