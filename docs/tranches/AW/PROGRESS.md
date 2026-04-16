# Tranche AW — PROGRESS log

Indefatigable orchestration record. Dated entries; what landed,
what committed, what blocked, what shifted. The diff between
`AW.md` and this file names every contact-adapted shift.

## 2026-04-16 — AW kickoff

### Orchestrator opening

Plan committed (`docs/tranches/AW/AW.md`, commits `d174af3…4177a18`,
"The Activation" — eight waves W0–W7). AV closed at V5 with the
substrate intact and the hot path unwired: every bench entry
regressed 2.5–4.5× versus post-AU because the `fn __<rule>`
recursion carries every V0–V5 correctness write *on top of* the
legacy emission. AW deletes the legacy path, activates the DTA
driver + PSI + ShapeRef + PHF/SIMD dispatch + bloom/GADT dedup,
and recovers the regression with bench checkpoints between every
wave.

The orchestrator's operational posture is inherited from AV and
strengthened by the bench-checkpoint contract: master stays
workspace-green at every wave boundary, sub-agents commit at
every milestone (not at end of work), bench artefacts land per
wave to `docs/benchmarks/post-AW-W{N}.json`, and no wave closes
until its bench trajectory matches the wave gate or carries a
written rationale.

Per user directive: the `post-AV-substrate-only.json` open-the-
tranche reference bench is skipped. The post-AV bench matrix in
`docs/benchmarks/post-AV.json` supplies the regression baseline
directly; the W0 recovery measurement reads against that file.

### Pre-flight audit confirmations

Grep audit against master (commit `4177a18`) confirms the
friction points AW.md names:

- `crates/bbnf-tape/src/builder.rs:631–636` — `finish()` calls
  `derive_frame_depth` + `finalise` unconditionally (AW.0.1
  target).
- `crates/bbnf-tape/src/columns.rs:283` —
  `compute_sibling_skip` intact with `dead_code` warning (AW.0.2
  deletion target).
- `crates/ir/src/passes/transform/inline.rs:42` and
  `crates/ir/src/passes/transform/fuse.rs:55` — the
  `r.meta.scc_id.is_none()` always-true guards surface at the
  actual line numbers (plan cited `:23`/`:31`; drift since plan
  authorship noted for agent dispatch).
- `crates/gorgeous/src/{bbnf,bnf,css,ebnf,google_sheets,json}.rs`
  — **six** source files carry inline `#[cfg(test)] mod tests`
  blocks, not just `google_sheets.rs`. AW.0.6's "project-wide
  audit + migration" lands all six in this tranche.
- `crates/core/src/grammar/generated.rs` — 28326 lines at AW
  open; W1 deletion target is ≤ 12000.
- `.github/workflows/{ci,release}.yml` — CI substrate exists;
  AW.0.7's `check-bootstrap-clean.sh` wires here.

### Wave 0α — Research wave (dispatched, read-only)

Five parallel sub-agents in sibling worktrees producing the
design documents AW.md §Research artefacts prescribes:

- `01-dta-driver-design.md` — W1 walker contract, frame-stack
  overflow handling, `frame_depth` emission.
- `02-shaperef-runtime-dispatch.md` — W2.3 dict consultation
  cost model, `shape_hash` collision strategy, view-layer
  expansion correctness sketch.
- `03-pratt-lowering-generality.md` — W4.6 Pratt loop
  generalisation from Sheets to CSS / BBNF.
- `04-named-struct-abi-finalisation.md` — W0.5 layout
  contract, `LargeAggregate` arena encoding, view-layer Color
  accessor codegen, lightningcss equivalence.
- `05-bench-checkpoint-protocol.md` — per-wave bench agent
  contract, input matrix, output JSON shape, attribution
  requirements, regression rationale format.

Docs 04 and 05 gate W0 dispatch (W0.5 depends on 04, the W0
close bench checkpoint depends on 05). Docs 01–03 gate their
respective waves.

## GrammarProfile population matrix (AW.0.9 ledger)

Each AW wave that consumes a profile slot is responsible for
populating it. Stub slots at AW open (all `&[]` per V1 closure):
`active_columns`, `list_rules`, `keyword_tables`, `shape_dict`,
`dedup_eligible_rules`. Matrix updated at each W2/W3/W4 close.

| Slot | Populated by | Status at AW open |
|------|--------------|-------------------|
| `active_columns` | W2.3 (ShapeRef view-layer wiring) | `&[]` |
| `shape_dict` | W2.3 (ShapeRef dispatch) | `&[]` |
| `keyword_tables` | W3.1 (PHF) + W3.2 (SIMD compare) | `&[]` |
| `list_rules` | W4.1 (list-rule recogniser) | `&[]` |
| `dedup_eligible_rules` | W4.5 (eligibility IR pass) | `&[]` |

A wave that closes without populating its slot violates AW.0.9.
JSON has no keyword Alts — its populated-by-design `&[]` for
`keyword_tables` records here post-W3, not stub-carried.
