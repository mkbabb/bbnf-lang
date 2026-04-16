# Tranche AX — Replay, Recovery, and the Subsystem Ledger

AX is the replay-and-recovery tranche. AW lands the substrate
hooks (decision log + `DtaSnapshot`) but commits no consumer.
AX builds the consumers — incremental re-parse, generalised
grammar-structural error recovery, parse-step debugger, test-
case minimisation — on top of AW's `dta-replay` feature gate.
AX also closes the four pre-existing subsystem failures AW
explicitly defers as Category A: closure tests, analysis
structural-mode gates, gorgeous-fixture tests, pprint-vm hint
tests. The `test_selective_transitive_unfurling` imports-bug
either folds in here or gets its own slice depending on
AW.5.5's disposition.

The framing: AW's substrate makes the long-deferred parser
properties (incremental, recoverable, replayable) architectur-
ally cheap to land. AX is where they ship.

## Architectural thesis

A DTA parse is a deterministic walk of a counter-DFA over a
padded byte slice. That fact has three consequences AX
exploits:

1. **Replay is a re-walk.** Given the AW.1.7 decision-log
   stream, re-driving the DTA against the log produces a
   bit-identical tape without re-reading the input. The log
   is small (~1 byte per ~6–8 input bytes); replay tooling
   becomes a debugger, a test-case minimiser, and a parse-
   trace inspector at substrate cost.

2. **Snapshot-and-resume is mechanical.** The AW.1.7
   `DtaSnapshot` captures the DTA's frame stack + counter
   registers + byte offset in a serialisable struct. Resume
   = pass the snapshot back into the driver. Stack depth ≤
   12 on every grammar in the corpus; snapshot cost is O(1)
   in input size.

3. **Incremental re-parse is two snapshots, a localised
   re-walk, and a column splice.** When the source buffer is
   edited from `[old_lo, old_hi]` to new bytes:
   - Locate the highest tape record whose span fully
     contains `[old_lo, old_hi]` via binary search on
     `span_lo`.
   - Snapshot the DTA at that record's saved state (kept on
     the tape's per-record metadata column, populated during
     the original parse).
   - Re-drive the DTA over `[span_lo, edit_end]` from the
     snapshot.
   - Splice the resulting Columns slice into the master
     Columns at the affected record range; bump downstream
     `span_lo`/`span_hi` by the byte delta in one linear
     pass.

   tree-sitter ships this as its headline feature. AX ships
   it because the substrate makes it cheap, not because it
   reframes anything load-bearing in the architecture.

The recovery story is parallel: when a DTA transition fails,
walk the frame stack upward looking for a state whose grammar
declares a structural-sync byte (or the AW-derived default —
the next byte in the structural-alphabet that closes or
advances that level). Skip ahead, pop the frame, resume.
`@recover` becomes sugar over the structural default rather
than the only mechanism.

## Invariants (inherited from AU/AV/AW, refined)

1. **No new grammar directives.** AX consumes the AW.1.7
   `dta-replay` feature gate; grammar-author surface is
   unchanged. `@recover` already exists; its semantics
   refine to "override the structural default for this
   rule's level" rather than "the only recovery mechanism."
2. **Incremental and full-parse share one driver.** The DTA
   driver carries one entry point; incremental is a snapshot
   + re-walk, not a separate code path. No fallback parser,
   no dual build.
3. **Workspace green at every wave boundary.** AW
   re-established this discipline; AX preserves it.
4. **Bench checkpoints per wave.** AW's bench-checkpoint
   contract carries forward. AX's wave gates target
   incremental-edit latency and recovery correctness, not
   raw cold-parse throughput.
5. **The pre-existing subsystem failures close in this
   tranche.** Closure language-feature gap, analysis
   structural-mode gates, gorgeous fixture tests, pprint-vm
   hint tests — each gets a dedicated phase. The Category A
   ledger zeroes at AX close.

## Cross-tranche debt — ledger reconciled

Items routed forward from AW Category A and any orchestrator-
declared deferrals:

| Item | Origin | Carried through | AX phase |
|------|--------|-----------------|----------|
| Incremental re-parse | tree-sitter parity gap noted in `competitors.rs` | Never planned in any prior tranche | **Phase 2** |
| Generalised grammar-structural recovery | `@recover` was per-rule hand-author | AU diagnostic-replay sketch only; AV.3.4 sketch | **Phase 3** |
| Decision-log replay tooling (debugger / minimiser) | AW.1.7 substrate ships hooks; consumers deferred | AW Phase 1 | **Phase 1** |
| 5 closure tests (`closure_*_param`, `lower::expression` gap) | Pre-AV | AW W5 Category A | **Phase 4** |
| 4 analysis structural-mode gates (cycle/alias detection, diagnostics) | Pre-AV | AW W5 Category A | **Phase 5** |
| 3 gorgeous dump tests (non-checked-in fixtures) | Pre-AV | AW W5 Category A | **Phase 6** |
| 2 pprint-vm hint tests (softbreak/indent_group drift) | Pre-AV | AW W5 Category A | **Phase 6** |
| `test_selective_transitive_unfurling` (imports subsystem) | Pre-AV | AV.0.12 → AW.5.5 (disposition decides) | **Phase 7** (if AW.5.5 routes here) |

## Wave schedule

Seven waves; bench-checkpoint contract preserved from AW. The
target metrics shift from cold-parse throughput to
incremental-edit latency on the parser-side waves and to
test-suite-clean on the subsystem-closure waves.

| Wave | Parallel sub-agents | Workspace state | Wave gate |
|------|---------------------|-----------------|-----------|
| **X0 — Replay tooling** (3 parallel) | (a) Decision-log inspector CLI (AX.1.1). (b) Test-case minimiser over the log (AX.1.2). (c) Parse-step debugger / log-replay test harness (AX.1.3). | Green. | **post-AX-X0.json** — substrate cost of `dta-replay` feature on; cold-parse regression < 2% vs AW close. |
| **X1 — Snapshot persistence + driver entrypoints** (serial) | Single agent: extend `DtaSnapshot` with stable serde + bincode round-trip; expose `parse_resume(snapshot, input)` driver entrypoint; per-record snapshot metadata column lands on the tape (AX.2.1, AX.2.2). | Green. | Snapshot round-trip parity tests pass. |
| **X2 — Incremental re-parse** (serial) | Single agent: edit-localisation + subtree re-walk + Columns splice + downstream span-shift (AX.2.3, AX.2.4, AX.2.5). | Green. | Incremental-edit median latency ≤ 200 µs on a 100 KB CSS edit; cold-parse unchanged from X1. |
| **X3 — Grammar-structural recovery** (2 parallel) | (a) Structural-default recovery in the DTA driver (AX.3.1). (b) `@recover` semantics refinement: override the default for a rule's level (AX.3.2). | Green. | Recovery test suite (new) passes on canonical malformed inputs across all four grammars. |
| **X4 — Closure language-feature closure** (serial) | Single agent: address `closure_*_param` and `lower::expression` gaps. Closure-binding semantics in BBNF were partial pre-AV; this wave finishes the language feature. | Green. | 5 closure tests un-ignore. |
| **X5 — Analysis structural-mode** (2 parallel) | (a) Cycle/alias detection under structural mode (AX.5.1). (b) Diagnostics surface for analysis gates (AX.5.2). | Green. | 4 analysis structural-mode tests un-ignore. |
| **X6 — Gorgeous fixtures + pprint-vm hints** (2 parallel) | (a) Gorgeous fixtures: commit non-checked-in snapshot files OR delete the tests (AX.6.1). (b) pprint-vm `softbreak`/`indent_group` drift fix (AX.6.2). | Green. | 5 fixture/hint tests un-ignore or delete. |
| **X7 — Tranche completion** (serial) | Single agent: `FINAL.md`, `post-AX.json`, ignored count = 0 (or documented residual with explicit ticket). | Green. | — |

## Phases

### Phase 1 — Replay tooling on AW.1.7 substrate (X0)

The decision log + snapshot the AW DTA driver exposes are
substrate hooks; X0 builds the three obvious consumers.

#### AX.1.1 Decision-log inspector

A CLI binary `cargo run -p bbnf-cli -- inspect-log
<grammar> <input>` parses with `dta-replay` enabled, dumps
the decision log alongside per-transition annotation:
`(byte_offset, dfa_state_id, transition_taken, frame_depth)`.
Reads as a parse trace at the structural-transition level.
The grammar author's debugger when a parse goes wrong.

#### AX.1.2 Test-case minimiser

Given a malformed input that triggers a parse error, produce
the shortest substring that triggers the same error. Drives
the DTA against incrementally-shorter prefixes/suffixes of
the input, using the decision log to detect when the failing
transition first appears. O(log n × parse cost) shrink loop.

#### AX.1.3 Log-replay test harness

A `#[test]` macro that captures a parse's decision log,
re-runs the DTA against the log, and asserts the resulting
tape is bit-identical. Catches regressions in the DTA driver
that would change the parse's decision sequence even when
the final tape happens to match.

### Phase 2 — Incremental re-parse (X1, X2)

The substrate's payoff. tree-sitter-grade incremental edits
without their bytecode-interpreter overhead.

#### AX.2.1 Per-record snapshot metadata

Tapes gain a sparse `snapshot_at: Vec<(TapeOffset,
DtaSnapshot)>` overlay populated during the parse at every
list-rule boundary (the same boundaries Phase 7 fork points
identified). Cost: one snapshot per ruleset / array-element /
rule / formula-line. For tailwind.css (~12k rulesets) this is
~12k × ~120 B ≈ 1.4 MB — sized comparably to the tape itself.
Cheaper grammars (BBNF self-host, JSON object trees) carry
proportionally fewer snapshots.

#### AX.2.2 Driver `parse_resume` entrypoint

`pub fn parse_resume(snapshot: &DtaSnapshot, input:
PaddedView<'_>) -> Result<(Columns, PayloadStream, Vec<u8>),
ParseErr>` re-enters the DTA at the snapshot's state and
parses forward. Output is the local Columns / PayloadStream /
frame_depth slice; the caller splices into a master Columns.

#### AX.2.3 Edit-localisation pass

Given an `Edit { old_lo, old_hi, new_bytes }`, binary-search
the master tape's `span_lo` column for the highest record
fully containing `[old_lo, old_hi]`. That record's snapshot
metadata gives the resume state.

#### AX.2.4 Subtree re-walk

Call `parse_resume(snapshot, padded_view(span_lo, ...))`.
Driver re-parses up to the natural close point of the
covering record's rule. Output: new Columns slice for the
affected range.

#### AX.2.5 Columns splice + span shift

Replace the master Columns' record range with the new slice.
For records after the splice point, shift `span_lo`/`span_hi`
by `(new_bytes.len() - (old_hi - old_lo))`. Single linear
pass per affected column. ShapeRef and bloom-GADT dedup
re-fire on the spliced slice — structurally-identical
re-parses hit the prior tape verbatim.

Hard gate: edit a 100 KB CSS file with a 16-byte property-
value edit; incremental re-parse median latency ≤ 200 µs;
cold-parse comparison shows the splice produces a
bit-identical tape to the cold parse of the post-edit
buffer.

### Phase 3 — Generalised grammar-structural recovery (X3)

#### AX.3.1 Structural-default recovery

When a DTA transition fails, the driver walks the frame
stack upward. For each frame, the rule's structural alphabet
identifies the rule-level "sync byte" — the next byte in the
structural-alphabet that closes or advances that level (CSS
`;` and `}`, JSON `,` and `]`/`}`, BBNF `;` and `.`, Sheets
newline). Skip ahead to the sync byte, pop the frame, resume.

The DTA records the failure in a `RecoveryEvent` stream
alongside the decision log, so callers see "what was tried,
what synced, where it resumed."

#### AX.3.2 `@recover` semantics refinement + deprecation audit

`@recover ruleName syncExpr ;` retains its grammar-author
surface but the semantics shift: `syncExpr` overrides the
structural default for `ruleName`'s level. Existing BBNF
recovery annotations continue to work; rules without
explicit `@recover` get the structural default.

Recovery test suite lands: per-grammar canonical malformed
inputs (truncated CSS rules, missing JSON braces, BBNF
syntax errors mid-grammar, Sheets formula errors mid-row),
each asserting a specific recovery point and downstream
parse continuation.

**Deprecation audit.** `@recover` predates the AV invariant
("grammar-author surface remains the BBNF syntax existing
grammars already use") so the directive is grandfathered.
But if the structural default is sufficient for every
production usage, `@recover` becomes dead surface — kept
only because it once was. The audit:

1. Enumerate every `@recover` site in the BBNF self-host
   grammar (`grammar/bbnf/*.bbnf`). The four production
   grammars (JSON, CSS, Sheets) declare none.
2. For each site, derive the structural-default sync byte
   the AX.3.1 driver would pick at that frame.
3. Compare: does the explicit `syncExpr` produce different
   recovery behaviour than the default? Record per-site
   verdict.
4. If every BBNF site reduces to the structural default,
   route `@recover` for **removal in AY**: drop the
   directive grammar, drop the bootstrap annotations, drop
   the codegen path. The deprecation removes one historical
   directive without changing any observable parse
   behaviour.
5. If any site genuinely needs the override, `@recover`
   stays as the grammar-author override knob for non-default
   sync semantics — the audit produces a documented
   rationale for its retention.

The audit ships in `docs/tranches/AX/recover-audit.md` as a
standalone artefact (one section per site, concrete
verdict). The removal-or-retention decision does not block
AX completion; AY inherits whichever direction the audit
recommends.

### Phase 4 — Closure language-feature closure (X4)

The 5 ignored closure tests (`closure_lambda_param`,
`closure_let_binding`, `closure_grammar_call`,
`closure_recursive_param`, `closure_lower_expression`) cover
gaps in the closure-binding semantics that AT introduced via
first-class closures + `GrammarCall` + `@host` directive.
The `lower::expression` gap is in the IR-lowering of closure-
captured `value_expr` nodes.

The wave's deliverable is feature-completion, not feature-
introduction: the AT design is sound; the lowering paths
weren't completed under the original tranche's time budget
and got carried forward.

### Phase 5 — Analysis structural-mode (X5)

The 4 ignored analysis tests cover cycle detection, alias
detection, and the diagnostics surface under structural mode
(post-AE). The legacy nominal-mode paths still work; the
structural-mode equivalents weren't ported when the AE
restructuring landed.

### Phase 6 — Gorgeous fixtures + pprint-vm hints (X6)

#### AX.6.1 Gorgeous fixture commit-or-delete

Three gorgeous tests reference snapshot files that are not
checked into the repo. Decide per test: either commit the
snapshot (proves output stability under future changes) or
delete the test (the property it captures isn't sufficiently
load-bearing to maintain). The W6.1 audit produces the
disposition.

#### AX.6.2 pprint-vm hint drift

Two pprint-vm tests (`softbreak_after_indent`,
`indent_group_no_break`) drifted post-AT pprint refactor.
Either the tests' expected output is stale (re-snapshot) or
the pprint-vm behaviour regressed (fix). Audit per test.

### Phase 7 — Imports subsystem (conditional, X7-or-route)

`test_selective_transitive_unfurling` exposes a bug in the
`imports.rs` module's selective-import resolution. AW.5.5
either fixes it in W5 scope (closing the deferral) or routes
to AX as a standalone phase. If routed:

- **AX.7.1 Imports-resolution audit**. Read the failing
  test's expected behaviour, trace the resolver's decision
  path, identify the divergence.
- **AX.7.2 Fix or document**. Either the resolver path lands
  the missing case, or the test's expectation is wrong (in
  which case update the test with rationale).

If AW.5.5 fixes it, this phase deletes from AX scope before
X7 dispatches.

## Critical files

| File | Phase |
|------|-------|
| `crates/bbnf-cli/src/inspect_log.rs` (**new** — decision-log inspector) | 1 |
| `crates/bbnf-cli/src/minimise.rs` (**new** — test-case minimiser) | 1 |
| `crates/bbnf-tape/src/replay.rs` (**new** — log-replay harness) | 1 |
| `crates/bbnf-tape/src/snapshot.rs` (**new** — `DtaSnapshot` serde + per-record overlay) | 2 |
| `crates/bbnf-tape/src/incremental.rs` (**new** — edit-localisation + splice) | 2 |
| `crates/bbnf-tape/src/driver.rs` (`parse_resume` entrypoint, recovery walk) | 2, 3 |
| `crates/bbnf-tape/src/recovery.rs` (**new** — structural-default + `RecoveryEvent`) | 3 |
| `crates/ir/src/lower/expression.rs` (closure-captured value_expr lowering) | 4 |
| `crates/analysis/src/state/structural/` (cycle/alias detection ports) | 5 |
| `crates/gorgeous/tests/fixtures/` (commit or delete) | 6 |
| `crates/pprint/src/vm/` (softbreak/indent_group drift) | 6 |
| `crates/core/src/imports.rs` (selective-transitive resolver) | 7 |
| `crates/core/tests/incremental_parity.rs` (**new** — incremental-edit fixture suite) | 2 |
| `crates/core/tests/recovery_parity.rs` (**new** — recovery-event fixture suite) | 3 |
| `docs/tranches/AX/{PROGRESS,FINAL}.md` + `docs/benchmarks/{post-AX,post-AX-X{0,1,2,3,4,5,6}}.json` | 0–7 |

## Hard gates summary

### X0 — Replay tooling

1. `cargo run -p bbnf-cli -- inspect-log <grammar> <input>` produces a per-transition trace dump.
2. The minimiser shrinks a 1 KB malformed input to ≤ 32 bytes in O(log n) parse calls.
3. `bbnf_tape::replay::log_round_trip` test passes for every grammar.
4. `dta-replay` feature on shows < 2% cold-parse regression vs AW close (`post-AX-X0.json` confirms).

### X1 — Snapshot persistence

5. `DtaSnapshot` serdes round-trip via bincode; `snapshot_round_trip` tests pass for every grammar.
6. Per-record snapshot metadata column lands; tape size grows ≤ 30% on the four parse-bench matrix; documented in `post-AX-X1.json`.

### X2 — Incremental re-parse

7. `crates/core/tests/incremental_parity.rs` lands; per-grammar canonical edits (insert, delete, replace at varying scopes) produce bit-identical tapes to the cold re-parse.
8. Median incremental-edit latency ≤ 200 µs on a 100 KB CSS edit; ≤ 50 µs on a 10 KB JSON edit; ≤ 10 µs on a 1 KB Sheets edit.
9. ShapeDict + bloom-GADT dedup re-fire on the spliced slice; structurally-identical re-parses hit the prior tape verbatim (verified via tape-record equality on a fixture).

### X3 — Recovery

10. `crates/core/tests/recovery_parity.rs` lands; per-grammar canonical malformed inputs each assert a specific recovery point + downstream parse continuation.
11. `@recover` semantics refinement preserves AT/AU bootstrap-recovery behaviour (BBNF self-hosting recovery test suite passes unchanged).
12. `RecoveryEvent` stream produces audit-friendly output; CLI inspector reads it.
13. `docs/tranches/AX/recover-audit.md` lands with a per-site verdict for every `@recover` annotation in the BBNF self-host grammar; AY inherits the removal-or-retention decision the audit recommends.

### X4–X6 — Subsystem closures

13. 5 closure tests un-ignore.
14. 4 analysis structural-mode tests un-ignore.
15. 3 gorgeous fixture tests un-ignore or delete with documented rationale.
16. 2 pprint-vm hint tests un-ignore.

### X7 — Imports + completion

17. `test_selective_transitive_unfurling` un-ignores OR carries a documented forward-ticket to a dedicated imports tranche.
18. `cargo test --workspace --no-fail-fast` 0 failures, ignored count = 0 (or documented residual with explicit ticket per AW Category A discipline).
19. `docs/tranches/AX/FINAL.md` exists per `docs/instructions/README.md` requirements.
20. `docs/benchmarks/post-AX.json` exists covering the four parse-bench matrix; cold-parse regression < 5% vs AW close (incremental + recovery substrate is paid for by feature value, not throughput).

## Indefatigability

AX is smaller in architectural scope than AV/AW but wider in
subsystem reach. The bench-checkpoint contract continues; the
cold-parse trajectory is not the AX target, so each wave's
gate is feature-correctness or test-suite-clean rather than
MB/s. The ledger zeroes here: AW closes the substrate debt;
AX closes the tooling and subsystem debt. Subsequent tranches
inherit a workspace where every test passes, every typed AST
round-trips against its peer parser, every parse is replayable
and editable, and every grammar's recovery surface is
well-defined.
