# AZ-II O3a-A1 Plan - Analysis, LSP, Prototype, JIT, and Bootstrap Disposition

Status: plan lane output. This document makes no source edits and does
not amend shared O5/O6/O7 files directly. The orchestrator should apply
the exact amendment text below in the shared wave-spec pass after P1/A1
integration.

## Evidence Read

- `docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt` records the
  O3a baseline: 1645 tests run, 1561 passed, 84 failed, 25 skipped.
- A1 owns two live product tests:
  `bbnf-analysis::directives import_directive_has_semantic_tokens` and
  `bbnf-lsp::integration test_hover_recover_keyword`.
- A1 owns seven historical prototype tests:
  `json-prototype::corpus parses_citm`, `parses_data_s`,
  `parses_data_xl`, `parses_canada`, `parses_twitter`,
  `tape_visitor_twitter`, and `tape_visitor_data_s`.
- `crates/core/Cargo.toml` still depends on
  `json-prototype = { path = "benches/json-prototype" }`; the prototype
  manifest depends on `tape = { path = "../../../tape" }`.
- `crates/core/benches/json-prototype/tests/corpus.rs` still imports
  `TapeVisitor`, proving the failed prototype lane is tape-era
  substrate, not a StructDirect parity oracle.
- `crates/gorgeous/src/jit.rs` generates a temporary Cargo project that
  depends on published `bbnf_derive` and published `bbnf` versions. It
  is a retired derive-shaped JIT path, not a `cargo xtask regen`
  consumer.
- `crates/core/src/grammar/mod.rs` and
  `crates/core/src/pipeline/directives.rs` still route BBNF parsing
  through `bootstrap_parser::parse`; generated
  `BbnfBootstrap::parse` is compile-visible but not canonical.

## Disposition

| Surface | Decision | Owner | Rationale |
|---|---|---|---|
| Live analysis failure | Repair | A1-LIVE source redress before O5 close | Semantic tokens for import directives are user-facing analysis behavior. Do not delete or mark historical. |
| Live LSP failure | Repair | A1-LIVE source redress before O5 close | Hover on `@recover` is live editor behavior. Do not route to O7 as residual risk. |
| `json-prototype` crate | Archive, then delete from Cargo/source surfaces | O5.A1-PROTOTYPE | It is a historical speed-ceiling prototype with direct `tape` and `TapeVisitor` dependencies. The generated emitter now owns the reusable shape lessons. Repairing it would preserve a tape-era product surface. |
| `crates/gorgeous/src/jit.rs` | Delete, not rewrite in AZ-II | O5.A1-JIT | A rewrite onto `cargo xtask regen` would be a new dynamic grammar compilation product, not tape deletion. The existing file is derive-era substrate and may not survive as a shim. |
| `bootstrap_parser.rs` | Bounded bridge with close-blocking proof | O6.A1-BP proof, O7.A1-CLOSE status | The bridge may remain only until generated `BbnfBootstrap::parse` self-host proof is green and the canonical entry points are rewired. O7 cannot declare terminal close while the bridge is the public parser. |

No compatibility shim is permitted. In particular, do not preserve
`json-prototype` behind a feature flag, do not leave a Gorgeous JIT mode
that shells out to retired derive crates, and do not add an adapter that
makes generated BBNF parse failures fall back to `bootstrap_parser`.

## Source and Archive Owners

### A1-LIVE - Analysis and LSP Repair

Primary source owners:

- `crates/analysis/src/directives/import.rs`
- `crates/analysis/src/directives/recover.rs`
- `crates/analysis/src/state/parsing.rs`
- `crates/analysis/src/features/hover/directive.rs`
- `crates/analysis/src/features/hover/import.rs`
- `crates/lsp/tests/integration.rs`
- `crates/analysis/tests/directives.rs`

Patch intent: repair directive extraction and hover/token behavior on
the current BBNF parse surface. If the failure is caused by missing
directive spans from the bridge parser, fix the bridge or generated
BBNF document projection; do not relax test assertions and do not
special-case raw source text in LSP as a second parser.

### O5.A1-PROTOTYPE - Prototype Archive and Deletion

Archive owner:

- `docs/tranches/AZ-II/archive/json-prototype/`

Source deletion owners:

- `crates/core/benches/json-prototype/**`
- `crates/core/Cargo.toml`
- `crates/core/benches/json/value.rs` only for dependency/import
  cleanup if it still imports `json_prototype`
- workspace lockfile/metadata as produced by normal cargo commands

Patch intent: move enough provenance into the archive to preserve why
the prototype existed, then remove the crate from build, test, and bench
surfaces. Generated emitter comments may continue to cite the historical
shape only as archaeology; no compiled crate or dependency may remain.

### O5.A1-JIT - Gorgeous JIT Deletion

Source owners:

- `crates/gorgeous/src/jit.rs`
- `crates/gorgeous/src/main.rs`
- `crates/gorgeous/Cargo.toml` if CLI feature/help text exposes the JIT
  mode
- Gorgeous tests or docs that mention dynamic derive-backed JIT

Patch intent: delete the retired dynamic JIT path and recode CLI
dispatch/help so no option promises runtime grammar compilation. Do not
rewrite this path inside AZ-II; a first-class dynamic formatter can be a
future product tranche only if it is designed around the current regen
pipeline from the start.

### O6.A1-BP - Bootstrap Parser Proof

Proof owners:

- `crates/core/src/grammar/generated/bbnf.rs`
- `crates/core/src/grammar/mod.rs`
- `crates/core/src/pipeline/directives.rs`
- `crates/core/src/grammar/bootstrap_parser.rs`
- `crates/core/tests/bbnf_bootstrap_reproducibility.rs`
- `docs/benchmarks/AZ-II/cutover/O6-bootstrap-parser-proof.txt`

Patch intent: prove generated `BbnfBootstrap::parse` can parse the BBNF
fixture corpus and drive the pipeline without the hand-written bridge.
If the proof fails, O6 blocks O7 and names the exact source owner. If it
passes, the source owner rewires `grammar::parse` and
`parse_to_pipeline_inputs` to the generated parser and deletes
`bootstrap_parser.rs` before terminal close.

## Verification Commands

A1 live analysis/LSP repair:

```bash
cargo nextest run -p bbnf-analysis \
  --test directives \
  --cargo-profile ax-iter \
  import_directive_has_semantic_tokens \
  --no-fail-fast -- --nocapture \
  > /tmp/az-ii-o3a-a1-analysis.txt 2>&1

cargo nextest run -p bbnf-lsp \
  --test integration \
  --cargo-profile ax-iter \
  test_hover_recover_keyword \
  --no-fail-fast -- --nocapture \
  > /tmp/az-ii-o3a-a1-lsp.txt 2>&1
```

Prototype archive/delete verification:

```bash
test ! -e crates/core/benches/json-prototype
rg -n 'json-prototype|json_prototype|TapeVisitor|tape = \{ path = "../../../tape" \}' \
  crates/core/Cargo.toml crates/core/benches Cargo.toml
cargo metadata --no-deps --format-version 1 \
  > /tmp/az-ii-o5-a1-metadata.txt 2>&1
! rg -n '"name":"json-prototype"|crates/core/benches/json-prototype' \
  /tmp/az-ii-o5-a1-metadata.txt
```

Gorgeous JIT deletion verification:

```bash
test ! -e crates/gorgeous/src/jit.rs
! rg -n 'mod jit|format_grammar|bbnf_derive|gorgeous-jit|cache_dir' \
  crates/gorgeous/src crates/gorgeous/Cargo.toml
cargo check -p gorgeous --profile ax-iter \
  > /tmp/az-ii-o5-a1-gorgeous-check.txt 2>&1
```

Bootstrap parser proof:

```bash
cargo nextest run -p bbnf \
  --test bbnf_bootstrap_reproducibility \
  --cargo-profile ax-iter \
  --no-fail-fast -- --nocapture \
  > /tmp/az-ii-o6-a1-bbnf-bootstrap-repro.txt 2>&1

cargo xtask regen --grammar bbnf --check \
  > /tmp/az-ii-o6-a1-bbnf-regen-check.txt 2>&1

rg -n 'bootstrap_parser::parse|pub mod bootstrap_parser|bootstrap_parser;' \
  crates/core/src/grammar crates/core/src/pipeline \
  > /tmp/az-ii-o6-a1-bootstrap-parser-scan.txt
```

Terminal A1 close scan:

```bash
! rg -n 'json-prototype|json_prototype|TapeVisitor|bbnf_derive|gorgeous-jit|bootstrap_parser::parse' \
  Cargo.toml crates docs/tranches/AZ-II/FINAL.md docs/tranches/AZ-II/PROGRESS.md
```

## Exact Wave-Amendment Text

### Amendment for `docs/tranches/AZ-II/waves/cutover/O5.md`

Append after O5.11:

```markdown
### AZ-II.cutover.O5.12 O3a A1 Prototype Archive and Gorgeous JIT Deletion

Mechanism: consume `docs/tranches/AZ-II/audit/O3a-A1-plan.md`.
Archive then delete `crates/core/benches/json-prototype/**` because it
is a historical speed-ceiling prototype with direct `tape` and
`TapeVisitor` dependencies. Delete `crates/gorgeous/src/jit.rs` and the
CLI/help dispatch that exposes the retired derive-backed dynamic JIT
surface. Do not repair either surface and do not retain feature-gated or
fallback compatibility shims.

Files touched: `crates/core/benches/json-prototype/**`,
`crates/core/Cargo.toml`, `crates/core/benches/json/value.rs` if it
imports `json_prototype`, `docs/tranches/AZ-II/archive/json-prototype/**`,
`crates/gorgeous/src/jit.rs`, `crates/gorgeous/src/main.rs`,
`crates/gorgeous/Cargo.toml` if JIT mode is feature/help-visible, and
O5 scan/progress artifacts.

Sub-gate: `test ! -e crates/core/benches/json-prototype` and
`test ! -e crates/gorgeous/src/jit.rs` pass; cargo metadata contains no
`json-prototype` package; `rg 'json-prototype|json_prototype|TapeVisitor|bbnf_derive|gorgeous-jit|mod jit|format_grammar' crates/core crates/gorgeous Cargo.toml`
returns no live source hits outside archived provenance; `cargo check -p
gorgeous --profile ax-iter` and `cargo check -p bbnf --benches
--profile ax-iter` pass.
```

Add this hard-gate bullet:

```markdown
7. O3a A1 prototype/JIT disposition is closed with no compatibility
   shim: `json-prototype` is archived then deleted from Cargo/source
   surfaces, and the retired Gorgeous derive-backed JIT surface is
   deleted rather than rewritten inside AZ-II.
```

Add this verification artifact:

```markdown
- `docs/tranches/AZ-II/archive/json-prototype/**`
- `/tmp/az-ii-o5-a1-metadata.txt`
- `/tmp/az-ii-o5-a1-gorgeous-check.txt`
```

### Amendment for `docs/tranches/AZ-II/waves/cutover/O6.md`

Append after O6.15:

```markdown
### AZ-II.cutover.O6.16 O3a A1 Live Product and Bootstrap Bridge Proof

Mechanism: consume `docs/tranches/AZ-II/audit/O3a-A1-plan.md`. Verify
the live analysis and LSP failures are green after A1-LIVE redress.
Then prove whether generated `BbnfBootstrap::parse` can be the
canonical BBNF self-host parser without `bootstrap_parser::parse`.
O6 does not add a fallback parser and does not reopen source substrate;
if generated self-host is not ready, O6 records the exact failing proof
and blocks O7 until a source owner retires the bridge.

Files touched: `docs/benchmarks/AZ-II/cutover/O6-bootstrap-parser-proof.txt`,
`docs/benchmarks/AZ-II/cutover/O6-workspace-nextest.txt`, and progress
docs. Source files are read for proof only unless the orchestrator has
already integrated an A1 source-retirement commit before O6 starts.

Sub-gate: `/tmp/az-ii-o3a-a1-analysis.txt` and
`/tmp/az-ii-o3a-a1-lsp.txt` pass; `cargo xtask regen --grammar bbnf
--check` and `bbnf_bootstrap_reproducibility` pass; the bootstrap proof
artifact states either `bootstrap_parser.rs deleted and generated parser
canonical` or `O7 BLOCKED` with the named source owner. O6 may not claim
terminal BBNF self-host while `grammar::parse` or
`parse_to_pipeline_inputs` falls back to `bootstrap_parser::parse`.
```

Add these hard-gate bullets:

```markdown
12. O3a A1 live-product repairs are green:
    `bbnf-analysis::directives import_directive_has_semantic_tokens`
    and `bbnf-lsp::integration test_hover_recover_keyword` pass on the
    post-O5 tree.
13. `bootstrap_parser.rs` has a close disposition backed by
    `docs/benchmarks/AZ-II/cutover/O6-bootstrap-parser-proof.txt`.
    Terminal close is blocked unless generated `BbnfBootstrap::parse`
    is canonical and no fallback/adapter to the hand-written bridge
    remains.
```

Add this verification artifact:

```markdown
- `docs/benchmarks/AZ-II/cutover/O6-bootstrap-parser-proof.txt`
- `/tmp/az-ii-o3a-a1-analysis.txt`
- `/tmp/az-ii-o3a-a1-lsp.txt`
- `/tmp/az-ii-o6-a1-bbnf-bootstrap-repro.txt`
- `/tmp/az-ii-o6-a1-bbnf-regen-check.txt`
- `/tmp/az-ii-o6-a1-bootstrap-parser-scan.txt`
```

### Amendment for `docs/tranches/AZ-II/waves/cutover/O7.md`

Append after O7.11:

```markdown
### AZ-II.cutover.O7.12 O3a A1 Terminal Disposition Conversion

Mechanism: convert A1 from active routing into terminal close evidence.
FINAL must cite the A1 plan, the live analysis/LSP verification
artifacts, the `json-prototype` archive/delete commit, the Gorgeous JIT
delete commit, and the O6 bootstrap-parser proof. Terminal close is
forbidden if any A1 surface remains active: `json-prototype` compiled
as a package, `crates/gorgeous/src/jit.rs` exists, live analysis/LSP
tests are red, or `bootstrap_parser::parse` is still the public BBNF
parser.

Files touched: `docs/tranches/AZ-II/FINAL.md`,
`docs/tranches/AZ-II/PROGRESS.md`,
`docs/tranches/AZ-II/waves/cutover/O3a-A1.md`,
`docs/tranches/AZ-II/audit/O3a-A1-plan.md`, and
`docs/benchmarks/AZ-II/cutover/O7-close-doc-scan.txt`.

Sub-gate: O7 close scan records zero live hits for
`json-prototype`, `json_prototype`, `TapeVisitor`, `bbnf_derive`,
`gorgeous-jit`, `mod jit`, `format_grammar`, and
`bootstrap_parser::parse` outside explicitly historical archive text.
If any hit remains in live source or close docs, AZ-II remains open.
```

Add this hard-gate bullet:

```markdown
7. O3a A1 is terminally closed: live analysis/LSP tests are green,
   `json-prototype` is archived/deleted, the retired Gorgeous JIT is
   deleted, and the BBNF bootstrap bridge is either deleted after
   generated self-host replacement or recorded as an O7 blocker.
```

Add this verification artifact:

```markdown
- `docs/tranches/AZ-II/audit/O3a-A1-plan.md`
- `docs/benchmarks/AZ-II/cutover/O6-bootstrap-parser-proof.txt`
- `/tmp/az-ii-o7-a1-close-scan.txt`
```

## Close Criteria

O3a-A1 closes only when the orchestrator has a committed plan artifact
with these decisions, plus amended O5/O6/O7 specs that consume them.
The source redress may land later in the owning waves, but those waves
must not proceed with an implicit compatibility path. O7 cannot convert
AZ-II to terminal close unless every A1 live surface is green or deleted
according to this plan.
