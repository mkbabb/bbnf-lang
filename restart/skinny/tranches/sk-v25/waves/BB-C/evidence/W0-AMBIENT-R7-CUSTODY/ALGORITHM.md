# R7 custody census algorithm

Schema: `bb-c.w0.r7-custody.v1`.

Canonical invocation sets `PYTHONDONTWRITEBYTECODE=1` so custody never gains
interpreter cache bytes:

```text
PYTHONDONTWRITEBYTECODE=1 python3 W0-AMBIENT-R7-CUSTODY/census.py \
  --workspace /Users/mkbabb/Programming/bbnf-lang \
  --sealed-fleet /tmp/bbcw0-r6-output-fleet.TwgDag \
  --candidate /tmp/bbcw0-r7-atomic-candidate.CrYkNb \
  --out /Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/waves/BB-C/evidence/W0-AMBIENT-R7-CUSTODY
```

`__pycache__`, `.pyc`, and other interpreter-generated files are never
evidence members. The incidental pre-seal `census.cpython-314.pyc` was removed
before final hashing.

All paths are resolved first, rejected if symlinked or non-regular, normalized
to POSIX form relative to `/Users/mkbabb/Programming`, and sorted by raw UTF-8
path bytes. Workspace-local paths receive the explicit `bbnf-lang/` prefix.
Each TSV row is `path`, four-digit permission mode, byte count, and lowercase
SHA-256, separated by one tab and terminated by LF. The path-set digest is
SHA-256 of the sorted `path + LF` stream. The ledger digest is SHA-256 of the
complete TSV bytes, including its header.

## Build-input inclusion

1. Root `Cargo.toml`, `Cargo.lock`, `rust-toolchain.toml`, and
   `.cargo/config.toml`. Root `rust-toolchain` and `.cargo/config` are absent;
   Cargo has no second root toolchain/config file in this workspace.
2. Every direct workspace member declared in root `Cargo.toml`: its
   `Cargo.toml`, root `build.rs` when present, and every regular file under its
   direct `src/` tree.
3. Every `.bbnf` and `.ron` regular file under `grammar/`.
4. Every `.toml` regular file under `xtask/runtime-projections/`, because R7
   must reconcile generator output and the implicated runtime projection as
   one candidate.
5. The parse-that Rust workspace manifest and the four external path-patched
   packages selected by `.cargo/config.toml`: `parse_that`, `bbnf-regex`,
   `pprint`, and `pprint_derive`. For each package, include its manifest,
   direct `src/` tree, root `build.rs`, and an explicit `[lib].path` outside
   `src/` (notably `pprint/rust/derive/lib.rs`).

Excluded: `.git`, `target`, `restart`, docs, tests, benches, examples, frontend
trees, package caches, external lockfiles, hidden metadata, and arbitrary
files merely sharing the name `src` inside test fixtures. These do not enter
the isolated xtask build or the declared generator/runtime-projection input
cone. Generated parsers and sidecars remain included because they live under
the core member's direct `src/` tree and compile into `bbnf`.

## Runtime-projection inclusion

The implicated families are JSON, CSS L4, CSS Pretty, and Google Sheets. The
separate projection ledger contains:

- `xtask/src/lib.rs`, `main.rs`, `regen_simple_runtime.rs`, and
  `regen_css.rs`;
- the four family projection TOMLs;
- the four checked registry sidecars used as rule/layout preimages; and
- every regular file in the four checked runtime output directories.

These bytes are also copied with relative paths and modes into
`runtime-projection-snapshot/`. Shared runtime dependencies are already bound
by `BUILD-INPUTS.tsv`; they are not duplicated into the focused projection
snapshot.

## Fleet inclusion

The exact fleet set is the Cartesian product of the nine manifest identifiers
and `{.rs, .registry.json}`. Missing, extra, symlinked, or non-regular members
are RED. The source R6 fleet, durable custody copy, and fresh atomic candidate
must have identical normalized path/mode/byte/SHA-256 rows.

## R6 ambiguity

R6 retained only aggregate values for 867 members. It did not retain those
paths, per-file identities, normalization, exclusions, or digest procedure.
Consequently its aggregate cannot be reproduced or treated as the R7
pre-candidate ledger. This v1 algorithm is the first reproducible successor;
any count or digest difference from the opaque R6 tuple is expected and does
not imply source motion by itself.
