#!/usr/bin/env node

import { createHash } from "node:crypto";
import {
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const bbnfRoot = resolve(scriptDirectory, "..");
const parseThatRoot = resolve(bbnfRoot, "../parse-that-skv26");
const rustRoot = join(parseThatRoot, "rust");
const profileRoot = join(
  bbnfRoot,
  "restart/skinny/tranches/sk-v26/prototypes/w1-profile",
);
const artifactPath = join(
  bbnfRoot,
  "restart/skinny/tranches/sk-v26/artifacts/W1-PROFILE.json",
);
const decisionPlanPath = join(
  bbnfRoot,
  "restart/skinny/tranches/sk-v26/artifacts/W1-PROFILE-PLAN.md",
);
const profileSourcePath = join(profileRoot, "src/main.rs");

function run(command, args, options = {}) {
  const result = spawnSync(command, args, {
    cwd: options.cwd ?? bbnfRoot,
    encoding: "utf8",
    env: { ...process.env, ...options.env },
    maxBuffer: 64 * 1024 * 1024,
  });
  if (result.status !== 0) {
    throw new Error(
      `${command} ${args.join(" ")} failed (${result.status})\n${result.stdout}\n${result.stderr}`,
    );
  }
  return result.stdout.trim();
}

function optional(command, args) {
  const result = spawnSync(command, args, {
    encoding: "utf8",
    maxBuffer: 4 * 1024 * 1024,
  });
  return result.status === 0 ? result.stdout.trim() : "unavailable";
}

function sha256(value) {
  return createHash("sha256").update(value).digest("hex");
}

const porcelain = run("git", ["status", "--porcelain=v1", "-z"], {
  cwd: parseThatRoot,
});
if (porcelain.length !== 0) {
  throw new Error(
    "W1 acceptance requires a clean committed parse-that candidate; commit and re-run",
  );
}

const compileElapsedNs = [];
for (let sample = 0; sample < 20; sample += 1) {
  const temporaryRoot = mkdtempSync(join(tmpdir(), "skv26-w1-compile-"));
  try {
    const started = process.hrtime.bigint();
    run(
      "cargo",
      [
        "check",
        "--locked",
        "--release",
        "--manifest-path",
        join(rustRoot, "Cargo.toml"),
        "-p",
        "parse_that",
        "--no-default-features",
      ],
      {
        cwd: rustRoot,
        env: { CARGO_TARGET_DIR: join(temporaryRoot, "target") },
      },
    );
    compileElapsedNs.push(Number(process.hrtime.bigint() - started));
  } finally {
    rmSync(temporaryRoot, { recursive: true, force: true });
  }
}

const profileTarget = join(bbnfRoot, "target/skv26-w1-profile-acceptance");
run(
  "cargo",
  ["build", "--locked", "--release", "--manifest-path", join(profileRoot, "Cargo.toml")],
  {
    cwd: profileRoot,
    env: { CARGO_TARGET_DIR: profileTarget },
  },
);
const rawProfile = JSON.parse(
  run(join(profileTarget, "release/skv26-w1-profile"), [], {
    cwd: profileRoot,
  }),
);

const inputHashes = Object.fromEntries(
  [64, 1024, 65_536].map((size) => [
    String(size),
    sha256(Buffer.alloc(size, 0x61)),
  ]),
);

const artifact = {
  schema: "sk-v26.w1.acceptance-profile.v1",
  disposition: "NON_PROMOTING_CORRECTNESS_TRANSPOSITION",
  subject: {
    repository: parseThatRoot,
    branch: run("git", ["branch", "--show-current"], { cwd: parseThatRoot }),
    commit: run("git", ["rev-parse", "HEAD"], { cwd: parseThatRoot }),
    tree: run("git", ["rev-parse", "HEAD^{tree}"], { cwd: parseThatRoot }),
    dirty_porcelain_sha256: sha256(porcelain),
  },
  environment: {
    observed_at: new Date().toISOString(),
    host_model: optional("sysctl", ["-n", "hw.model"]),
    cpu_brand: optional("sysctl", ["-n", "machdep.cpu.brand_string"]),
    logical_cpu_count: optional("sysctl", ["-n", "hw.ncpu"]),
    cpu_features: optional("sysctl", ["-n", "hw.optional.arm.FEAT_AES"]),
    os_version: optional("sw_vers", ["-productVersion"]),
    os_build: optional("sw_vers", ["-buildVersion"]),
    power_mode: optional("pmset", ["-g", "batt"]),
    load: optional("uptime", []),
    rustc: run("rustc", ["--version", "--verbose"], { cwd: rustRoot }),
    cargo: run("cargo", ["--version", "--verbose"], { cwd: rustRoot }),
    hardware_counters: "unavailable in this unprivileged observational run",
  },
  semantic_plane: {
    class: "W1 direct recognition and one retained event",
    validation: "full input through Run::parse_full",
    utf8_policy: "opaque bytes; all profile inputs are ASCII 0x61",
    string_policy: "no escape or decoding work",
    number_policy: "not applicable",
    error_policy: "not timed; adversarial errors are correctness laws",
    retained_output:
      "NoSink for literal/regex rows; one usize event in a fresh Vec for retained rows",
    input_mutation_padding_copying: "immutable borrowed slice; no padding or copying",
    reuse:
      "input bytes reused; each measured invocation creates a fresh Input/Run; retained row creates a fresh sink",
  },
  inputs: {
    byte_value: "0x61",
    sha256_by_size: inputHashes,
  },
  measurement_sources: {
    decision_plan: {
      path: "restart/skinny/tranches/sk-v26/artifacts/W1-PROFILE-PLAN.md",
      sha256: sha256(readFileSync(decisionPlanPath)),
    },
    acceptance_script: {
      path: "scripts/skv26-w1-acceptance.mjs",
      sha256: sha256(readFileSync(fileURLToPath(import.meta.url))),
    },
    profile_harness: {
      path: "restart/skinny/tranches/sk-v26/prototypes/w1-profile/src/main.rs",
      sha256: sha256(readFileSync(profileSourcePath)),
    },
  },
  build: {
    command:
      "cargo check --locked --release --manifest-path rust/Cargo.toml -p parse_that --no-default-features",
    flags: "release profile from parse-that rust/Cargo.toml; fresh CARGO_TARGET_DIR per sample",
    clean_compile_elapsed_ns: compileElapsedNs,
  },
  recognition: rawProfile,
  decision: {
    performance_promotion: false,
    reason:
      "production v1 and TypeScript invocation-v4 do not perform an equivalent W1 Rust semantic plane",
    allowed_use:
      "gross-regression observation only; W2/W4 must seal paired end-to-end comparisons",
  },
};

writeFileSync(artifactPath, `${JSON.stringify(artifact, null, 2)}\n`);
process.stdout.write(
  `${artifactPath}\t${sha256(readFileSync(artifactPath))}\n`,
);
