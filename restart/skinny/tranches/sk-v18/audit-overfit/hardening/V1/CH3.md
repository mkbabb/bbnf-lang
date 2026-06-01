# S-P0 audit-overfit hardening V1 — CH3 Regression

Lens: CH3 REGRESSION. The audit must not (1) re-open a pre-blocked REDRESS family; (2) prune
>SOTA-bearing code; (3) contradict a carried-forward VALIDATED fact; (4) ship a sequencing
graph whose revert chain can let a downstream wave march over a REDRESSed predecessor. Subject:
`a0..a3` + `SYNTHESIS-AUDIT-OVERFIT.md` at live HEAD `83b66db42`. Every disk claim CH3 leans on
was re-grepped this pass (not inherited from the audit prose).

## Disk re-verification (CH3-critical claims, re-grepped at `83b66db42`)

- **P1 prunes DORMANT x86 — zero >SOTA reachable.** `src/x86_64/` = 24 files, `ext/x86/` = 4
  files (confirmed). `build.rs:38-40` early-returns on `target_arch != "x86_64"`; the 24-file
  tree carries 14 `unimplemented!` stubs and ZERO reachable x86 intrinsics (the 2 files matching
  `_mm*`/`core::arch::x86` carry the tokens only in doc/`unimplemented` lines — the non-comment
  filter returns empty). `pub mod x86_64;` is declared unconditionally (`lib.rs:5`) but bodies are
  inert. The build-soundness coupling is REAL: `checkasm_parity.rs` has 9 active
  `bbnf_simd::x86_64::…` call sites — a2 §3 correctly makes P1's exit gate `cargo test --no-run`
  clean and sequences the decouple SAME-wave. **P1 deletes no aarch64-reachable code → no >SOTA
  regression.**
- **P2 prunes the warm path, KEEPS `css_canon_bench`.** `css_canon_bench.rs` present (the harness
  that produced the headline numbers); `measure_mbps`/`lightningcss_facts` = 48 hits in
  `nonjson_css_l4.rs` (the warm micro-fixture path P2 deletes). V3 C3 confirms the warm path "did
  NOT produce the headline numbers." **No >SOTA-bearing measurement deleted.**
- **P3 collapses byte-IDENTICAL replicas.** All 7 `css_l4_*/generated.rs` share md5
  `b654562ccff46ed62dd48e9ace325830` — no unique code is lost in the collapse.
- **L6 deadness real.** `find_css_significant` reaches only `lib.rs:574`, inside `mod tests`
  (`#[cfg(test)]` at `:51`, `:574 ≫ :51`). G6 wires-or-retires; nothing >SOTA is removed.

## Dispositions

- **No pre-block re-opened (ACCEPT).** Cross-checked every prune/generalize against the full §0.4
  pre-block list (`SYNTHESIS.md:386-454`): AZ-IV eager tree (G4 trait is LAZY over the tape,
  `:394-395`); StructRegistry per-leaf (no registry, `:396-399`); fact-stream-String as admission
  plane (G2 retires the courier TOWARD lowering, NOT a fact-stream String, `:400-403`);
  `W5C_REQUEST_FACT_PROFILES` relocation seam (`:404-416` — policed structurally, the audit
  inherits this, not re-opens it); 24-broadcast (`:417-418`); FNV/fixture (`:419-422`, stays
  bench-only); x86/AVX/SVE (P1 ENFORCES by deletion, `:423-426` — enforcement, not re-open);
  verbatim-blob / phantom-generic / distinct-output re-entries (`:427-435` — the audit's L1/L4/L2
  lenses POLICE these, they are the pre-block's own machine-checks); no second substrate
  (`:454-455` — G3/G4 emit over the EXISTING `Tape`/`ValueRef`). The audit re-opens NONE.
- **Prune list deletes no >SOTA-bearing code (ACCEPT).** Disk-verified above: P1 = DORMANT x86
  (0 reachable intrinsics, build.rs early-returns); P2 = the warm path that did NOT produce the
  headline (KEEPS `css_canon_bench`); P3 = byte-identical replicas (no unique code); P4/P5 =
  gate/symbol fixes. The audit's CLEAN-KEEP inventory (`SYNTHESIS-AUDIT-OVERFIT.md:106-110`)
  explicitly preserves `css_canon_bench`, the 14-file checkasm discipline, the neutral kernel, and
  the substrate — the prune does not throw the aarch64 hardening out with the x86 bathwater.
- **VALIDATED facts preserved (ACCEPT).** αD (`alphaD-validated-invalidated.md`) closed V4 ACCEPT
  on every section, ZERO REVISE/REJECT, the lone V3 REVISE (stale checkasm "18" → disk-true **14**)
  folded. The audit carries αD's V1–V8 forward as CLEAN/KEEP — Lock 1 substrate-union, JSON >sonic
  cold strict, CSS canonical cold, checkasm 14, neutral kernel, honest harness, regen plumbing,
  FNV quarantine. No audit finding contradicts these; the audit's checkasm count is **14** (a2 §3,
  matching the folded αD), not the stale 18.
- **R16 does not regress the gate (ACCEPT).** The NEW a3 finding (F-A3.5: `fact_schema`/`row_id`/
  `output_plane` are NESTED in `output_labels`, not top-level `RuntimeTarget` fields) SHARPENS the
  `runtime_target_rows_collapsed` IMPLEMENTATION RECIPE; it does NOT change the close-gate
  DEFINITION (already sound by-exclusion — it excludes only `output_dir`/`expected_files`, so the
  nest IS in the comparison set). The gate stays correctly RED pre-P3 (7 distinct
  `profile`+`output_labels`). a3 §3 reconciles with a2 §4a / a0 §L2 WITHOUT contradiction ("not
  TOP-LEVEL fields" stands; the sharpening is recipe-recurse-into-nest). No regression of the gate.
- **REDRESS-W2-1 single-emitter (ACCEPT).** Carried as the G3 SUBJECT admitted to be discharged
  (`SYNTHESIS.md:379,445`: "REDRESS-W2-1 single-emitter is the G3 SUBJECT, not a re-open — it is
  admitted to be discharged here"), NOT a re-opened REDRESS. The audit's L3 lens is the discharge
  machine-check. Correct.

## REVISE (1)

- **R1-CH3 (foldable, single-edit, non-architectural).** The revert dependency graph is correct
  and the SYNTHESIS declares it authoritative (`SYNTHESIS-AUDIT-OVERFIT.md` §5 fact 3:
  "the dependency-graph diagram above is authoritative"). But two prose surfaces carry a residual
  ambiguity the REGRESSION lens must not let drift, because the revert chain is what blocks a wave
  from marching over a REDRESSed predecessor:
  (i) `SYNTHESIS-AUDIT-OVERFIT.md` §5 fact 3 states "G1 failure BLOCKS G2/G3/G4/PROVE" while G2's
  graph entry-gate is "G1 + P3 closed" — the bare prose could read as G2-blocks-on-G1-only when a
  **P3** failure ALSO blocks G2 independent of G1.
  (ii) a2 §4 titles its constraint "**G3 un-fork gating G1/G2**" while the binding graph orders
  G3 AFTER G1/G2 (PRUNE → G1 → G2 → G3); the directional revert claim that IS binding is
  "G3-failure blocks PROVE" (correct) — the title's "gates G1/G2" inverts the arrow and could
  confuse a reader reconstructing the revert chain.
  **Fold:** annotate fact 3 with the explicit dual entry-gate "(G2 also entry-gates on P3 — a P3
  failure blocks G2 independent of G1)"; and reword the a2 §4 constraint title to its accurate
  directional sense ("G1/G3 co-derive; G3-failure blocks PROVE"). Both are prose-disambiguation of
  an already-correct, already-authoritative graph — single-edit, zero architectural change, no
  gate touched. The revert chain ITSELF never inverts; only its prose restatements are loose.

## Tally
ACCEPT 5 · REVISE 1 · REJECT 0 — **83.3%**. Zero REJECT. The single REVISE is a foldable
prose-disambiguation of an already-correct, SYNTHESIS-declared-authoritative dependency graph;
it touches no gate, no architecture, no >SOTA-bearing surface. Disk re-verified: the prune deletes
only DORMANT/IDENTICAL/warm-non-headline code, re-opens no §0.4 pre-block, and contradicts no
folded αD VALIDATED fact.

TALLY accept=5 revise=1 reject=0
