# Profiling

## Shared target

For bench, profiling, and `cargo expand` analysis:

- profiling artifacts live under the main repo `.profiles/`
- all agents in a profiling wave share one absolute `CARGO_TARGET_DIR`
- prepare once, then profile many
- worktrees are optional for profiling; they are for git isolation, not
  build isolation

Set the shared target first:

```bash
export CARGO_TARGET_DIR=/absolute/path/to/shared/profile-target
```

## Prepare

```bash
scripts/prepare-profile-wave.sh \
  > .profiles/samply/prebuild/prepare.stdout \
  2> .profiles/samply/prebuild/prepare.stderr
```

## Profile

```bash
scripts/profile-bench-headless.sh \
  --bench json_monolithic \
  --entry canada \
  --record-port 3130 \
  --load-port 3131 \
  --artifact-dir .profiles/samply/json_monolithic/canada \
  --bench-cwd "$(pwd)/crates/core" \
  --bin /absolute/path/to/binary
```

Required profiling artifacts:

- `bench.txt`
- `build.txt`
- `record.txt`
- `load.txt`
- `profile.json.gz`
- `profile.json.syms.json`
- `syms-proof.txt`

Rules:

- preflight ports before profiling
- never write retained profiling artifacts to `/tmp`
- use `--unstable-presymbolicate`
- do not use `--save-only`
- bench binaries are cwd-sensitive; run from `crates/core`

`cargo expand` must also be file-first:

```bash
cargo expand -p bbnf --bench json_monolithic > /tmp/expand-json.txt
```

Then inspect with targeted search, not full rereads.

## Bench execution

Benchmarks run sequentially to avoid interference.

Common bench commands:

```bash
cargo bench -p bbnf --bench compile_pipeline > /tmp/bench-compile.txt 2>&1
cargo bench -p bbnf --bench json_monolithic > /tmp/bench-json.txt 2>&1
cargo bench -p bbnf --bench css_l4 > /tmp/bench-css.txt 2>&1
cargo bench -p bbnf --bench google_sheets_monolithic > /tmp/bench-sheets.txt 2>&1
cargo bench -p bbnf --bench bbnf_monolithic > /tmp/bench-bbnf.txt 2>&1
```

Primary datasets:

- `data/json/`
- `data/css/`
- `data/sheets/`
- `grammar/`

## Performance claims

- Every claimed perf win needs profiler evidence.
- Every codegen activation claim needs `cargo expand` evidence.
- Separate emitted-code facts from runtime hotspot facts.
- Tie optimization claims to saved artifacts, not memory.
