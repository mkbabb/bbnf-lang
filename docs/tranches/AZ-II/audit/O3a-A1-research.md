# AZ-II.cutover.O3a-A1 Research

Date: 2026-04-29
Agent: AZ-II O3a-A1 research
Worktree: `/Users/mkbabb/Programming/bbnf-wt-azii-o3a-a1-research`

## Scope readout

This research separates the two live product failures from the
historical `json-prototype` failures, records delete/archive/repair
options for `json-prototype`, records rewrite/delete options for the
Gorgeous JIT, and names the close disposition required for
`bootstrap_parser.rs` before O5/O7 can close.

Primary baseline evidence:

- `docs/benchmarks/archive/AZ-II/cutover/O3a-test-failures.txt:160-168`
  assigns the A1 failures: one `bbnf-analysis` test, one `bbnf-lsp`
  test, and seven `json-prototype::corpus` tests.
- `docs/tranches/AZ-II/waves/cutover/O3a-A1.md:54-63` requires this
  research to distinguish live product failures from historical
  prototype failures, name `json-prototype` and JIT dispositions, and
  give `bootstrap_parser.rs` an explicit O6/O7 close disposition.
- Targeted reruns confirmed the current symptoms:
  - `cargo nextest run -p bbnf-analysis import_directive_has_semantic_tokens --profile ax-iter --no-fail-fast`
    failed at `crates/analysis/tests/directives.rs:232` with
    `should have semantic token for @import keyword`.
  - `cargo nextest run -p bbnf-lsp test_hover_recover_keyword --profile ax-iter --no-fail-fast`
    failed with `{"jsonrpc":"2.0","result":null,"id":10}` for hover.
  - `cargo nextest run -p json-prototype parses_data_s tape_visitor_data_s --profile ax-iter --no-fail-fast`
    failed because `../../data/json/data.json` did not exist from the
    test process working directory.

## Live versus historical surfaces

### Live: analysis and LSP

`bbnf-analysis::directives import_directive_has_semantic_tokens` and
`bbnf-lsp::integration test_hover_recover_keyword` are live product
failures.

Evidence:

- `crates/analysis/tests/directives.rs:219-244` asserts that
  `@import { foo, bar } from "other.bbnf" ;` emits a KEYWORD token for
  `@import` and RULE_REFERENCE tokens for imported names.
- `crates/lsp/tests/integration.rs:1441-1464` drives the LSP server and
  expects hover over the `@recover` keyword to return directive
  documentation.
- Analysis semantic token generation assumes directive spans begin at
  the keyword: `import_semantic_tokens` uses `(imp.span.0,
  imp.span.0 + 7)` for `@import`
  (`crates/analysis/src/directives/import.rs:26-33`), and recover
  token generation uses `(rec.span.0, rec.span.0 + 8)`
  (`crates/analysis/src/directives/recover.rs:47-54`).
- LSP hover is also span-driven: `hover_recover` checks whether the
  cursor offset is inside `rec.span` before returning hover content
  (`crates/analysis/src/features/hover/directive.rs:6-16`).

Likely root cause:

- `crates/core/src/grammar/bootstrap_parser.rs:744-767` consumes
  `@import` but does not push a Span leaf for the `@import` keyword or
  other literal keyword bytes before ending the `import_directive`
  compound.
- `crates/core/src/grammar/bootstrap_parser.rs:770-784` has the same
  pattern for `@recover`.
- `BbnfView::byte_span` derives a compound span only from descendant
  Span leaves (`crates/core/src/runtime/bbnf/view.rs:88-97` and
  `crates/core/src/runtime/bbnf/view.rs:204-237`). Therefore a
  directive compound whose first source-backed child is `{`, an import
  path, or the target identifier cannot recover a byte span that starts
  at the directive keyword.
- `decode_recover` and `decode_import` trust `item.byte_span()` when
  creating directive spans (`crates/core/src/grammar/host.rs:331-341`
  and `crates/core/src/grammar/host.rs:478-504`). The downstream
  analysis/LSP code then computes keyword ranges from the wrong start
  offset.

Disposition:

- Repair is live source redress, not archive/delete. The fix should
  make the BBNF struct document preserve directive keyword source spans
  for `@import` and `@recover`, preferably in the bootstrap bridge or
  its generated replacement, not by adding analysis/LSP compensating
  offsets. A compensating analysis shim would keep the bridge defect
  hidden.
- Ownership: A1 redress can repair this directly in
  `crates/core/src/grammar/bootstrap_parser.rs` plus focused
  analysis/LSP tests, or O6/O7 must carry an explicit source owner. It
  is not a tape-crate blocker by itself, but it is an O7 workspace
  health blocker.

### Historical: json-prototype

The seven `json-prototype::corpus` failures are historical prototype
surface, not live product regressions.

Evidence:

- `crates/core/benches/json-prototype/Cargo.toml:1-5` describes the
  crate as a JSON-only hand-tuned prototype and AW-V.W2 speed-ceiling
  validator.
- The crate is not a root workspace member in `Cargo.toml`; it is
  pulled into the test graph through the core crate's dev-dependency
  `json-prototype = { path = "benches/json-prototype" }` in
  `crates/core/Cargo.toml:85`.
- `crates/core/benches/json-prototype/tests/corpus.rs:1-15` documents
  and implements fixture loading via `../../data/json/{name}`. In this
  worktree, `data/json` exists at the repository root, while
  `../../data/json` from the process working directory resolved outside
  the worktree during the targeted rerun.
- Two failed tests are explicitly tape-era: `tape_visitor_data_s` and
  `tape_visitor_twitter` (`crates/core/benches/json-prototype/tests/corpus.rs:60-78`).
- The implementation imports and materializes `tape::Tape` directly:
  `crates/core/benches/json-prototype/src/visitor.rs:16-18` imports
  `Tape`, `PayloadData`, and `TapeKind`; `visitor.rs:326-405` defines
  `TapeVisitor` over `tape::Columns`.
- The prototype crate depends on `tape` directly at
  `crates/core/benches/json-prototype/Cargo.toml:10-16`.

Delete/archive/repair options:

1. Delete from live test/bench graph.
   - Remove the core dev-dependency, remove or rewrite
     `crates/core/benches/json/value.rs` prototype comparison lanes,
     and delete `crates/core/benches/json-prototype/**` with the O5
     tape severance.
   - Evidence supports this as the cleanest O5 path because the crate
     is explicitly prototype provenance and directly depends on tape.
   - The generated parser and emitter may keep comments that say
     "mirrors json-prototype" as historical algorithm provenance only
     if O7 marks them non-live or rewrites wording.

2. Archive as provenance outside the live Cargo graph.
   - Remove all manifest/dev-dependency edges and move the code under a
     docs or archive location that Cargo does not discover.
   - This preserves AW-V speed-ceiling archaeology without retaining
     tests, benches, or a tape dependency.
   - This is compatible with O5 if `cargo metadata` no longer sees the
     package and tape scans ignore explicitly historical docs.

3. Repair as a live crate.
   - Minimum repair: fix fixture paths to use `CARGO_MANIFEST_DIR` or
     repo-root resolution.
   - Required O5 repair beyond that: delete `TapeVisitor`, remove the
     `tape` dependency, and decide whether `ValueVisitor` still supplies
     a close-matrix value not already covered by current JSON benches.
   - This is the highest-cost option and risks preserving a shadow JSON
     parser beside generated StructDirect, contrary to the no-hybrid
     codegen policy. It should be chosen only if O6 needs a current,
     non-tape speed oracle and the plan explicitly wires it into the
     canonical bench surface.

Recommended disposition for the plan agent: delete from the live Cargo
graph in O5, with optional archival outside Cargo if provenance matters.
Do not repair only the fixture path; that would make a tape-era
prototype green while O5 is trying to delete tape.

## Gorgeous JIT disposition

`crates/gorgeous/src/jit.rs` is live CLI-reachable but implemented on a
retired substrate.

Evidence:

- `crates/gorgeous/src/main.rs:15-28` advertises `gorg --grammar
  my.bbnf input.txt` as a user-facing grammar-file mode.
- `crates/gorgeous/src/main.rs:107-128` routes `--grammar` through
  `jit::format_grammar`.
- `crates/gorgeous/src/jit.rs:35-39` hard-codes old published crate
  versions: `parse_that = 0.3.0`, `bbnf_derive = 0.2.3`, `bbnf =
  0.2.4`, `pprint = 0.3.4`.
- `crates/gorgeous/src/jit.rs:41-81` generates a temporary Cargo
  project using the retired derive path and crates.io dependencies
  rather than the workspace `cargo xtask regen` manifest and generated
  parser path.
- `crates/gorgeous/src/jit.rs:183-202` shells out to `cargo build
  --release` in the generated project; `jit.rs:256-290` hashes the
  grammar, generates the project, compiles it, caches the binary, and
  runs it.

Options:

1. Delete `--grammar` / JIT mode.
   - Appropriate if arbitrary runtime grammar formatting is not a
     product gate for AZ-II close.
   - O7 must update CLI help and docs so no live surface points at the
     deleted mode.
   - This avoids keeping a second parser-generation path beside
     `cargo xtask regen`.

2. Rewrite onto the current regen/codegen surface.
   - Appropriate if `gorg --grammar` remains product.
   - The rewrite must not call retired `bbnf_derive`; it needs an
     explicit current-codegen path. If using `cargo xtask regen`, the
     plan must account for arbitrary grammar input versus the current
     workspace metadata grammar list.
   - This is larger than a version-pin bump. Merely updating crates.io
     versions would preserve the retired derive architecture and should
     be rejected as a compatibility shim.

Recommended disposition for the plan agent: either delete JIT grammar
mode as non-product before O7, or create a dedicated rewrite wave. Do
not leave `jit.rs` as ambient legacy at terminal close.

## Bootstrap parser close disposition

`crates/core/src/grammar/bootstrap_parser.rs` is an intentional
hand-written bridge, currently load-bearing for BBNF parsing and for
analysis/LSP/Gorgeous observational callers.

Evidence:

- `crates/core/src/grammar/mod.rs:55-64` says the public grammar parse
  entry point routes through the hand-written bootstrap parser because
  the regen-derived `BbnfBootstrap::parse` does not yet self-parse.
- `crates/core/src/grammar/mod.rs:64-67` calls
  `bootstrap_parser::parse(input)` and then
  `host::extract_observational(document)`.
- The same comment names the generated parser self-host gap as a
  deferred follow-up (`crates/core/src/grammar/mod.rs:56-63`).
- The live analysis/LSP failures above show the bridge is already
  observable outside bootstrap: directive keyword spans are incomplete
  for product editor features.

Close options:

1. Generated self-host replacement gate.
   - Before O7, route BBNF grammar parse through the generated
     `BbnfBootstrap::parse` path, prove parity against the bridge, and
     delete `bootstrap_parser.rs`.
   - This best matches AZ-II's self-host thesis.

2. Bounded bridge with explicit proof.
   - Keep `bootstrap_parser.rs` only if O6/O7 record it as a bounded,
     intentional bootstrap component with focused invariants: byte-equal
     regen, BBNF self-parity, directive span coverage, and no tape
     dependency.
   - This requires plan-level approval because the tranche text
     currently describes it as a bridge and deferred follow-up, not a
     terminal architecture.

3. Close-blocking repair.
   - At minimum, repair directive span preservation for live
     analysis/LSP and add tests that cover `@import`, `@recover`, and
     any other directive whose keyword is consumed but not represented
     by a Span leaf.
   - This can make O3a A1 green but does not by itself close the
     self-host replacement question.

Recommended disposition for the plan agent: repair directive spans now
as live product failures, then make O6/O7 require either generated
self-host replacement and bridge deletion, or an explicit approved
bounded-bridge proof. O7 should not leave the current comment's
"deferred follow-up" language as terminal state.

## O5 and O7 blockers

O5 is blocked by A1 until:

- `json-prototype` has a no-shim delete/archive/repair disposition.
  O5's own spec says it must consume A1 before deleting tape
  (`docs/tranches/AZ-II/waves/cutover/O5.md:126-149`).
- If deletion/archive is chosen, all Cargo and bench references to
  `json-prototype` are removed before `crates/tape` is deleted.
- If repair is chosen, the prototype must have no `tape` dependency and
  no `TapeVisitor` surface before the O5 zero-tape scans.
- Analysis/LSP are recorded as live product redress, not tape-only
  archive surfaces.

O7 is blocked by A1 until:

- The live analysis/LSP failures are fixed or closed under an explicit
  source owner and are not hidden as historical failures.
- `json-prototype` is absent from the live test graph or repaired as a
  non-tape live surface.
- Gorgeous JIT is deleted or rewritten; the retired derive-based
  temp-project implementation is not terminal architecture.
- `bootstrap_parser.rs` has an explicit close disposition and O7 cites
  A1. O7's spec requires all O3a outputs to be cited and no cohort to
  remain active (`docs/tranches/AZ-II/waves/cutover/O7.md:132-145`,
  `docs/tranches/AZ-II/waves/cutover/O7.md:166-175`).

## Evidence command log

- `cargo nextest run -p bbnf-analysis import_directive_has_semantic_tokens --profile ax-iter --no-fail-fast`
  - Result: failed, 0 passed / 1 failed.
  - Key stderr: `should have semantic token for @import keyword`.
- `cargo nextest run -p bbnf-lsp test_hover_recover_keyword --profile ax-iter --no-fail-fast`
  - Result: failed, 0 passed / 1 failed.
  - Key stderr: `@recover keyword hover: {"jsonrpc":"2.0","result":null,"id":10}`.
- `cargo nextest run -p json-prototype parses_data_s tape_visitor_data_s --profile ax-iter --no-fail-fast`
  - Result: failed, 0 passed / 2 failed.
  - Key stderr: `../../data/json/data.json: No such file or directory (os error 2)`.
