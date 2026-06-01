---
agent: 1E
pass: T-P1-totality-excavation
cycle: V5-SKV18-totality
cycle_self_label: V5
generated_at: 2026-06-01T00:00:00Z
spec_surfaces_audited:
  - restart/locks/LOCKS.md
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/skinny/tranches/sk-v18/SPEC.md
  - restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md
  - restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
  - skinny/REDRESS.md
  - skinny/RESULTS.md
  - skinny/Cargo.toml
  - Cargo.toml
  - skinny/crates/codegen/src/grammar_provider.rs
  - skinny/crates/codegen/src/runtime_generator.rs
  - skinny/crates/codegen/src/lower/mod.rs
  - skinny/crates/codegen/src/json_sink_direct.rs
  - skinny/crates/runtime/src/tape/mod.rs
  - skinny/crates/runtime/src/grammars/json/generated.rs
  - skinny/crates/bbnf-bench/src/lock14_baseline.rs
  - skinny/crates/bbnf-simd/src/lib.rs
  - crates/core/src/grammar/generated/
  - crates/core/src/runtime/css_l4/builder.rs
files_audited_count: 23
live_truth_method: "Read all three V1 greater-spec surfaces (LOCKS, ARCHITECTURE, MASTER-PLAN line spans) and the three SK-V18 spec surfaces end-to-end, then re-grounded every cited residual on disk with rg, find, wc -l, and direct file reads in BOTH trees (skinny/crates = SK-V18 benched witness; crates/core = totality adoption target). No cargo. No source edits, builds, tests, staging, or commits. Prior cycle V4 of this file folded forward; new rows carry V5/SKV18 ids. HEAD verification point this pass = 4e4aa0648 (dirty tree); the `LOCKS.md:408` Pattern-H 67 baseline this file cites carries an INHERITED `e12c5323d` stamp from the SK-V14 lock-clause cycle — every claim spot-checked still verifies at 4e4aa0648 (CH1-V1-F16)."
prior_cycle_dispositions_folded:
  accepted:
    - V4-LAC-1E-V1-01..14 (16-lock evidence inventory; carried forward verbatim where unchanged)
    - V4-verified-invariant 16-lock-count + 5-shape-BackendShape canon
  revised:
    - V4-LAC-1E-V1-12 Pattern-H 67 baseline -> totality-core now 71 (drift; re-keyed D-1E-V5-06)
    - V4-Lock-15 root-thin-LTO confirmed still live at Cargo.toml:81 (re-keyed D-1E-V5-13)
  superseded_context:
    - SK-V17 T-P3 addendum (LOCKS.md:610-622) already folds SK-V18 TAPE + CLASSIFIER discipline; this cycle adds the SK-V18 GENERALIZATION discipline (named-primitive gate, relocated-seam firewall, neutrality-proof, x86-deletion, verbatim-blob prohibition) NOT yet bound
divergence_count:
  spec_claims_implemented: 2
  spec_claims_unimplemented: 11
  impl_exceeds_spec: 2
  unknown: 3
locks_amendment_candidates: 7
---

# Totality T-P1 1E Locks Evidence (V5 — SK-V18 Generalization Lens)

## Executive Summary

All 16 locks were re-audited against current code in BOTH trees plus the three
SK-V18 spec surfaces. Invariants hold: lock count is 16; `BackendShape` is the
exact 5-shape canon. The SK-V17 T-P3 addendum (`LOCKS.md:610`-`622`) already
folds SK-V18's TAPE-substrate and CLASSIFIER discipline; but the SK-V18
GENERALIZATION discipline — the named-primitive (a)-(d) gate, the
`emit_shape_source==lowered_program` relocated-seam firewall, the
`css_balanced_component_scan` neutrality-proof obligation, the aarch64-only /
x86-deleted standing, and the verbatim-blob-courier prohibition — is ABSENT from
the lock surface (grep returns zero for `emit_shape_source`,
`css_balanced_component_scan`, `named-primitive`, `verbatim-blob`). Every SK-V18
S-P0 residual (R1/R3/R5/R8/R9/R15) is verified STILL LIVE in `skinny/crates/`:
the CSS const courier, the emitter fork, the phantom `<G>`, the 28-file x86
surface, the green-by-exclusion gate, and the metalang leak. Lock 1, 5, 6, 14,
and 16 carry the most generalization pressure: the parser is hand-written/forked,
not generator-derived. This artifact catalogues evidence and surfaces 7
amendment CANDIDATES only; it does not amend LOCKS.

## Verified Invariants

| invariant | status | evidence |
|---|---|---|
| 16-lock count | verified | 16 numbered headings: `restart/locks/LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`. |
| 5-shape `BackendShape` canon | verified | live `select_lowering` matches exactly five at `skinny/crates/codegen/src/lower/mod.rs:18`-`24` (`EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage`); SK-V18 SPEC re-affirms the five-shape domain at `restart/skinny/tranches/sk-v18/SPEC.md:75`-`78`. |
| SK-V17 addendum present | verified | LOCKS already folds SK-V18 tape/classifier at `restart/locks/LOCKS.md:610`-`622`. |
| SK-V18 GENERALIZATION discipline absent from LOCKS | verified | `rg 'emit_shape_source|css_balanced_component_scan|named-primitive|verbatim-blob' restart/locks/LOCKS.md` returns ZERO. |

## Spec-Claim to Implementation Table

| lock | claim path:line | impl path:line | verdict | note |
|---|---|---|---|---|
| L01 Tape substrate / no parallel substrate | `restart/locks/LOCKS.md:75`, `:614` | `skinny/crates/runtime/src/tape/mod.rs:94`, `:175` | implemented (partial / JSON-tape) | One `Tape`/`ValueRef` substrate is CLEAN per S-P0 §2 (`SYNTHESIS-AUDIT-OVERFIT.md:109`). The phantom `<G>` rider (`tape/mod.rs:175`) is an R5 residual G4 deletes; not a parallel substrate. |
| L02 Layout lowering canonical name | `restart/locks/LOCKS.md:160`, `:616` | `skinny/crates/passes/src/lib.rs` `LayoutFacts.backend_shape` | drifted | Public `passes::layout`/`Layout`/`LayoutSink` absent; `LayoutFacts` side-table only. `StructLayout` lives 960× in `crates/` per the SK-V17 reconcile clause `LOCKS.md:616`. |
| L03 Cursor-parse + byte-skip empty path | `restart/locks/LOCKS.md:170`-`176` | none found | silent-must-add | `__EAGER_EMPTY_PATH` grep is zero; closure needs a generated-code golden. Carried from V4 D-1E-V1-12. |
| L04 Per-domain orthogonal optimization | `restart/locks/LOCKS.md:179`, `:183` | `crates/egraph/Cargo.toml:11` | drifted | Root `egraph` has a direct `csp-solver` dependency against the import-separation wording. Carried from V4. |
| L05 IR + per-backend lower | `restart/locks/LOCKS.md:181` | `skinny/crates/codegen/src/lower/mod.rs:18` | implemented (partial / Rust-only) | IR boundary present; but the generator DOES NOT EXIST per S-P0 §3 A4 (`SYNTHESIS-AUDIT-OVERFIT.md:141`) — two forked hand-written parsers + 7 replicas. The per-backend lower contract is honoured in shape, not in grammar-derivation. |
| L06 xtask committed generated source | `restart/locks/LOCKS.md:183`, `:185`-`196` | `skinny/crates/codegen/src/runtime_generator.rs:701` | drifted (over-stated) | `// @generated` CSS file is a verbatim `const CSS_GENERATED_RS: &str = r#"` literal — a hand-written courier, NOT round-trip-derived. Violates Lock-6 v+1 (LAC-1E-13) byte-equivalent-regen rule. R1 HIGH. |
| L07 `crates/path/` consolidated | `restart/locks/LOCKS.md:200` | `Cargo.toml:2` | drifted | Root lists `crates/bbnf-path`, `crates/bbnf-path-ts`; no `crates/path`, `crates/path-core`, `crates/parse-that`. Carried from V4. |
| L08 SOTA gates | `restart/locks/LOCKS.md:202`-`258` | `skinny/RESULTS.md:5-25` (JSON bench rows); CSS ratio `SYNTHESIS-AUDIT-OVERFIT.md:36`-`37` (synthesis-doc assertion) | implemented (JSON bench-backed; CSS DIRECTIONAL not re-locked) | SK-V18 headline is MEASUREMENT-VALID (JSON cold strict +1.4–164.7%, backed by `RESULTS.md:5-25` measured rows; CSS cold N=200 1.9–3.3×) per S-P0 §0 — unlike SK-V13's fake admit. INLINE CAVEAT (CH6-F3, per 1D U-4): the CSS 1.9–3.3× ratio is cited to the synthesis doc ASSERTING it (`SYNTHESIS-AUDIT-OVERFIT.md:36`), not a bench-row table; the S-P1 absolute ratios ran under loadavg 4.35 and are DIRECTIONAL, NOT re-locked (`SPEC.md:113-118`) — the H1 quiet re-capture on `css_canon_bench` is the re-lock gate. The warm micro-fixture path (`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3091`, `fn measure_mbps`; bare name disambiguated per CH1-V2-F5 — the `benches/` sibling has no `:3091`) is a P2 prune target, did NOT produce the numbers. R13/R14 are framing residuals, not fake admits. |
| L09 Slice-borrow + bump/owned hatches | `restart/locks/LOCKS.md:260`-`267` | JSON borrowed `parse` + `Cow` view | drifted | `parse_in(input,&bump)`/`parse_owned` absent. Carried from V4 D-1E-V1-11. |
| L10 Auto-detected Pratt/SIMD/materialization | `restart/locks/LOCKS.md:269`-`274` | `skinny/crates/codegen/src/lower/mod.rs:18` | over-stated | 5-shape canon exists and `select_lowering` dispatches on `BackendShape`; SK-V15 W8/W9 landed lowerers (`skinny/REDRESS.md:6356`,`:6382`). But the decision-engine load-bearing depth remains the open L10 question; SK-V18 R-E precedence-tower is the un-tested generality stressor (`SYNTHESIS-RESEARCH.md:249`-`255`). |
| L11 Path-deps / archive legacy crates | `restart/locks/LOCKS.md:319`-`326` | `Cargo.toml:2` | drifted | Root still lists `ser`, `gorgeous`, `simd-scan`, `bbnf-path`, `bbnf-path-ts`; `archive/` absent. Carried from V4. |
| L12 Archive before A.W0 | `restart/locks/LOCKS.md:328`-`334` | `Cargo.toml:2` | drifted | No archive ceremony; root retains `ser`/`gorgeous`. Carried from V4. |
| L13 No god directories / LOC cap | `restart/locks/LOCKS.md:336`-`347` | `crates/core/src/runtime/css_l4/builder.rs` | drifted | CSS L4 `OpenFrame` builder is 817 LOC at `crates/core/src/runtime/css_l4/builder.rs:16`-`89` (>500 non-generated cap). Pattern H in core = 71 files. The SK-V18 fold (`LOCKS.md:614`) retires this builder. |
| L14 Full grammar generalisation / zero overfit | `restart/locks/LOCKS.md:349`-`419` | `grammar_provider.rs:40`-`42`; `runtime_generator.rs:701`; `crates/ir/src/registry/strategy.rs:137`-`185`; `crates/core/src/css_types.rs:1` | drifted (HIGH) — LOCKS:349 gate RED | `RuntimeEmitterKind{CompiledLowering,RequestFacts}` is a grammar-family fork (R3); CSS courier is a hand-written const (R1); 7 byte-identical css_l4 replicas (R4); Lock-14 gate green-by-exclusion (R9, `lock14_baseline.rs:2442/2463`). TOTALITY-TREE LEAKS (CH2-V4-005): the 9-grammar idents table `crates/ir/src/registry/strategy.rs:137-185` and the lock-NAMED `crates/core/src/css_types.rs:1` host-shim BOTH leak grammar names into generic crates. The LOCKS:349 verification command ASSERTS "returns ZERO" but the live command `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` returns **13 sites** (11 `crates/ir/src/` + 2 `crates/analysis/src/`) — Lock 14's OWN gate is FALSIFIED / RED, not merely "catching" a leak. The generator is the un-built SK-V18 target. See D-1E-V5-14. |
| L15 Build-profile discipline | `restart/locks/LOCKS.md:436`-`451` | `Cargo.toml:81`; `skinny/Cargo.toml:80` | drifted | Root release is `lto = "thin"` at `Cargo.toml:81`; skinny release is `lto = "fat"` at `skinny/Cargo.toml:80`. Root drift persists. |
| L16 SIMD/ASM admissibility allowlist | `restart/locks/LOCKS.md:453`-`491`, `:622` | `skinny/crates/bbnf-simd/src/lib.rs:5` | drifted (over-stated vs aarch64-only) | x86 still live: `pub mod x86_64;` (`lib.rs:5`), 28 files under `src/x86_64`+`ext/x86`. SK-V18 P1 deletes the whole x86 surface (aarch64-only standing). `find_css_significant` NEON dead at admission (R7, test-only caller). DM3/DM4 `_neon` mislabels (R10/R11). |

## Divergences Catalogued

loc_delta cells REFERENCE the cross-inventory sibling estimate at path:line where a
delta is reused (no new measurement); risk is the lock-pressure class (CH4-V1-006).

| id | locks | divergence | loc_delta | risk | evidence |
|---|---|---|---:|---|---|
| D-1E-V5-01 | L06, L14 | CSS `generated.rs` is a verbatim `&str` courier in codegen, not generator-derived — the generator does not exist. | ≈ −910 courier (`1C` D1 911-LOC `CSS_GENERATED_RS` span; `1F-coherence` COH18-003) | HIGH | `skinny/crates/codegen/src/runtime_generator.rs:91` (`normalize(CSS_GENERATED_RS)`), `:701` (`const CSS_GENERATED_RS: &str = r#"`); `SYNTHESIS-AUDIT-OVERFIT.md:85` R1. |
| D-1E-V5-02 | L05, L14 | `RuntimeEmitterKind{CompiledLowering,RequestFacts}` is a grammar-family emitter fork; the un-fork is unbuilt. | ≈ −fork-arms (`1B` D1; path-change not new primitive) | HIGH | `skinny/crates/codegen/src/grammar_provider.rs:40`-`42`,`:110`; `SYNTHESIS-AUDIT-OVERFIT.md:87` R3. |
| D-1E-V5-03 | L01, L14 | Phantom `G: EventGrammar = AnyGrammar` type parameter has zero non-test production animator. | ≈ −10..−40 DELETE `<G>` axis (`1C` D5; preserve `K` axis) | HIGH | `skinny/crates/runtime/src/tape/mod.rs:175`,`:179`; `SYNTHESIS-RESEARCH.md:28`-`30`; `SYNTHESIS-AUDIT-OVERFIT.md:89` R5. |
| D-1E-V5-04 | L16 | x86 surface STILL LIVE (28 files) while SK-V18 P1 declares aarch64-only / x86-deleted standing; `diagnostic-x86` gate exclusion present. | ≈ −4500 (`1F-anti` R8 = canonical −4500 source; disk 28 files / 4401 LOC — dangling `1A 1A-DIV reuse` cross-ref struck per CH4-V3-006: `1A-substrate-evidence.md` has ZERO x86 content) | HIGH | `skinny/crates/bbnf-simd/src/lib.rs:5` `pub mod x86_64`; `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f = 28`; `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2463`; SK-V18 SPEC `:130`. |
| D-1E-V5-05 | L14 | Lock-14 gate is green-by-exclusion: leak surfaces parked in the weak `SKV15_W2_EXTRA_COVERAGE_ROOTS`, not strict `GENERIC_SCAN_ROOTS`. | gate-only (root promotion + drop `diagnostic-x86`, no body LOC) | HIGH | `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2409`,`:2442`,`:2463`; `SYNTHESIS-AUDIT-OVERFIT.md:93` R9. |
| D-1E-V5-06 | L13, L14 | Pattern H drift: core totality runtime now 71 files (LAC-1E-15 census baseline was 67). | +4 census (`tape/{mod,cursor,arena,record}.rs`; `1F-coherence` COH18-007) — trace, not delete | MEDIUM | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l = 71`; baseline `restart/locks/LOCKS.md:408`-`409` asserts 67. |
| D-1E-V5-07 | L15 | Root release profile is thin LTO; skinny is fat. Root migration gap. | ≈0 — 1-line profile flip (`Cargo.toml:81`) | LOW | `Cargo.toml:81` `lto = "thin"`; `skinny/Cargo.toml:80` `lto = "fat"`; lock requires fat per `LOCKS.md:436`. |
| D-1E-V5-08 | L06, L14 | Metalang bench-wave-id leak in shipped JSON runtime: `parse_w11_1_number` ×7. | ≈0 — rename-only (`1C` D7 / `1D` D-8 P5) | MEDIUM | `rg -c parse_w11_1_number skinny/crates/runtime/src/grammars/json/generated.rs = 7`; `SYNTHESIS-AUDIT-OVERFIT.md:99` R15. |
| D-1E-V5-09 | L08, L14 | The §6 named-primitive escape — the single largest paper-close surface — is prose-reviewed, not machine-checked, in the LOCK surface. The (a)-(d) gate lives only in the SK-V18 SPEC. | +20..+80 — bind the (a)-(d) gate as lock clause (LAC-1E-V5-01) | HIGH | `SYNTHESIS-RESEARCH.md:257`-`266`; `SYNTHESIS-AUDIT-OVERFIT.md:103` R-A0-3; LOCKS grep for `(a)-(d)`/`PROFILE-PROVEN-NARROW-LEAF` = 0. |
| D-1E-V5-10 | L01, L05, L14 | The relocated-seam firewall (`emit_shape_source==lowered_program`; `render(program)` reads NO `target.*` field) is an SK-V18 SPEC gate, not a lock. | +1 co-gate derive (`1B` D5 R16 `PartialEq`) + lock-clause text | HIGH | SK-V18 SPEC `:179`-`185`,`:1115`; `SYNTHESIS-RESEARCH.md:272`-`279` (risk-1); LOCKS grep for `emit_shape_source` = 0. |
| D-1E-V5-11 | L14, L16 | `css_balanced_component_scan` neutrality-proof obligation (CH6) is an SK-V18 SPEC discipline; the primitive does not yet exist in code, and the obligation is not lock-bound. | new-primitive ≤450 LOC / G2 build (`SPEC.md:439` G2 band imported per CH4-V4-009 — "≤450 hand source/test/gate LOC; new `lower/css_scan.rs` + `css_scan_direct.rs` + primitive shell + arg-derivation"); lock-clause +text | MEDIUM | `SYNTHESIS-RESEARCH.md:231`-`237`; SK-V18 SPEC `:439` (G2 ≤450 band), `:985`-`996`,`:1034`; `rg css_balanced_component_scan skinny/crates = 0`. |
| D-1E-V5-12 | L02 | `StructLayout` lives 960× in `crates/`; canonical `Layout`/`LayoutFacts` are grep-zero in `crates/` (skinny-only). | large rename surface (960 sites; SK-V19 totality fold) | MEDIUM | `restart/locks/LOCKS.md:616` SK-V17 reconcile clause; `crates/core/src/grammar/generated/` 9 grammars regenerate under the rename surface. |
| D-1E-V5-13 | L13, L14 | CSS L4 `OpenFrame` eager builder is 817 LOC (>500 non-generated cap) and is the SK-V18 retirement target. | ≈ −817 builder retire (live `wc -l = 817`; CH4 EXACT) | MEDIUM | `crates/core/src/runtime/css_l4/builder.rs:16`,`:68`,`:89`; `wc -l = 817`; `restart/locks/LOCKS.md:614`. |
| D-1E-V5-14 | L14 | Totality-tree Lock-14 leaks falsify the LOCKS:349 self-gate: the 9-grammar idents table in the generic `ir` crate + the lock-NAMED `css_types.rs` host shim. The LOCKS:349 command ASSERTS ZERO but returns 13 live (CH2-V4-005). | ≈ −60..−200 R16 row-collapse (idents table; `1F-coherence` COH18-005) + ≈ −66 relocate/delete (`css_types.rs`; `1C` D9) | HIGH | `crates/ir/src/registry/strategy.rs:137`-`185` (9 idents rows); `crates/core/src/css_types.rs:1` (66 LOC, lock-NAMED at `LOCKS.md:349`); live `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` = 13 (11 ir + 2 analysis) vs asserted ZERO. |

## Gaps / Missing Primitives

- `css_balanced_component_scan` named primitive does NOT exist in code yet (planned G2 §6 finding); its neutrality-proof obligation is unbound in LOCKS.
- The (a)-(d) machine-checked named-primitive gate is SK-V18-SPEC-only; no lock binds it.
- `emit_shape_source==lowered_program` relocated-seam firewall is SK-V18-SPEC-only.
- The one grammar-driven generator does NOT exist; CSS is a const courier, JSON is fixed-literal `render()`.
- `Layout`/`LayoutSink` public canonical names still absent (carried).
- `parse_in`/`parse_owned` runtime surfaces absent (carried).
- `crates/path`, `crates/path-core`, `crates/parse-that`, `archive/` absent from root (carried).
- aarch64-only standing not yet realized: 28 x86 files live; `diagnostic-x86` gate exclusion live.

## Open Questions (UNKNOWN -> verify_action)

| UNKNOWN | why unknown | verify_action |
|---|---|---|
| 1E-V5-U1 named-primitive (a)-(d) gate as lock discipline | The SK-V18 (a)-(d) gate is the single largest paper-close surface; whether it should be a LOCK-bound discipline (vs SK-V18-SPEC-scoped) is a T-P3 disposition. | T-P3 decides whether LAC-1E-V5-01 binds the (a)-(d) gate into a Lock 14/16 addendum or carries it as SK-V18-SPEC-scoped governance. Pass Omega ratifies. |
| 1E-V5-U2 css_balanced_component_scan neutrality demotion | The s6/C4 GROUND finding FORCES the `css_balanced_component_scan` rename (the offered non-CSS dischargers are parse-with-emit descents structurally incompatible with the byte-SKIP shell). Whether a future non-CSS invocation can re-promote to neutral is open. | T-P3 binds whether the neutrality-proof obligation requires re-promotion on a future non-CSS caller or is a permanent CSS-scoped name. Re-grep `rg balanced_component_scan skinny/crates/codegen` after G2 lands. |
| 1E-V5-U3 totality-core Pattern H 71 vs 67 baseline | The LAC-1E-15 census asserts 67; live `find` returns 71 in `crates/core/src/runtime`. The +4 delta is unattributed at this read. | T-P3 traces the +4 to (a) a grammar-roster change or (b) a sub-wave count update per LAC-1E-15; if unattributable, an O(N) generator regression scan opens. Re-run `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`. |

## LOCKS-AMENDMENTS-CANDIDATE (1E scope — CANDIDATES ONLY; never amend, never re-number)

The SK-V17 T-P3 addendum (`LOCKS.md:610`-`622`) already folds SK-V18's TAPE-substrate
and CLASSIFIER discipline. The candidates below cover ONLY the SK-V18 GENERALIZATION
discipline NOT yet bound. Each carries a wave-alignment hint + path:line evidence.

| candidate | type | target locks | proposed candidate text | wave hint | supporting path:line evidence |
|---|---|---|---|---|---|
| LAC-1E-V5-01 | addition | L14, L16, L08 | Bind the §6 named-primitive (a)-(d) gate as a lock discipline: every emitted hand-shaped kernel admitted under the honest-finding escape MUST be (a) grammar-INVOKED-by-name, (b) emitted-output-VARIES-under-invoking-rule-mutation, (c) `verbatim_blob_present==false`, (d) PROFILE-PROVEN-NARROW-LEAF (single profiled hot leaf; surrounding skeleton walk-derived). A primitive failing any of the four is a relabeled hand-written blob = REJECT. | G2 (CSS scan) ∧ G1 (JSON leaf kernels) | `SYNTHESIS-RESEARCH.md:257`-`266`; SK-V18 SPEC `:358`-`390`; `SYNTHESIS-AUDIT-OVERFIT.md:103` R-A0-3. |
| LAC-1E-V5-02 | addition | L05, L14, L01 | Bind the relocated-seam firewall: the un-forked emitter's `render(program)` reads its output-shape ONLY from `program.policy_summary.backend_shape`, NEVER from any `RuntimeTarget`/`profile`/`emitter`/`output_labels` field (`emit_shape_source==lowered_program`). md5-distinctness is NECESSARY-NOT-SUFFICIENT; the structural `runtime_target_rows_collapsed` (full-row `PartialEq`) co-gate is mandatory — the arm-census grep is syntactically incapable of seeing a per-grammar branch relocated into a neutral data table. (`runtime_target_rows_collapsed` is a PLANNED SK-V18 gate at `restart/skinny/tranches/sk-v18/SPEC.md:247`, NOT a live symbol — `rg runtime_target_rows_collapsed skinny/crates skinny/xtask` = 0; per CH1-V3-F12.) | G3 (un-fork) ∧ P3 (row-collapse) | SK-V18 SPEC `:179`-`185`,`:247`,`:1115`; `SYNTHESIS-RESEARCH.md:272`-`279`; `SYNTHESIS-AUDIT-OVERFIT.md:59`-`63` addendum 2. |
| LAC-1E-V5-03 | addition | L14, L16 | Bind the neutrality-proof obligation: a primitive named neutrally but exercised by ONE grammar in a campaign MUST be PROVEN neutral by ≥1 non-that-grammar invocation OR demoted to an honestly grammar-scoped name (the `balanced_component_scan` → `css_balanced_component_scan` FORCED demotion). A neutrally-named single-grammar primitive is an overfit-in-waiting. | G2 ∧ G6 (one seam) | `SYNTHESIS-RESEARCH.md:231`-`237`; SK-V18 SPEC `:973`,`:985`-`996`,`:1034`; s6/C4 GROUND finding SK-V18 SPEC `:7`-`12`. |
| LAC-1E-V5-04 | refinement | L16, L08 | Sharpen the x86 standing: the host is aarch64 / Apple M5 Max ONLY; the whole x86 surface (`src/x86_64/` + `ext/x86/` + nasm driver + `diagnostic-x86` gate exclusion) is a DELETION target, not a measured plane. x86/AVX-512 literature is architecture pressure that cannot close any row. (Strengthens the SK-V17 `LOCKS.md:622` aarch64-primary clause to aarch64-ONLY.) | P1 (x86 delete) | SK-V18 SPEC `:51`-`52`,`:130`; `skinny/crates/bbnf-simd/src/lib.rs:5`; `lock14_baseline.rs:2463`; `SYNTHESIS-AUDIT-OVERFIT.md:92` R8. |
| LAC-1E-V5-05 | refinement | L06, L14 | Strengthen the verbatim-blob-courier prohibition: a `@generated` file that is a verbatim `&str` literal in codegen is hand-written, NOT derived — REJECT as "grammar-driven". Round-trip byte-equivalence against the deletable oracle (not a ±5% line-count delta) is the binding proof of a real projection. (Co-binds Lock-6 v+1 LAC-1E-13 to the SK-V18 verbatim-blob addendum.) | G1 (JSON oracle) ∧ G2 (CSS courier delete) | `runtime_generator.rs:701`; `SYNTHESIS-AUDIT-OVERFIT.md:50` addendum 1, `:122`-`124` §2.1; SK-V18 SPEC `:329`-`339`. |
| LAC-1E-V5-06 | refinement | L14 | Bind the green-by-exclusion fix as a precondition: any wave authoring a new emitter MUST first move the codegen surfaces (`runtime_generator.rs`, JSON sink/typed/template) from the weak extra-coverage roots into strict `GENERIC_SCAN_ROOTS`, extend `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}`, and drop `diagnostic-x86` — proven by a re-inject-then-revert RED falsifier (`lock14_gate_scans_codegen==true`). | P4 (MUST land before G2/G3) | `lock14_baseline.rs:2420`,`:2442`,`:2463`; `SYNTHESIS-AUDIT-OVERFIT.md:170`-`175` P4; SK-V18 SPEC `:690`. |
| LAC-1E-V5-07 | refinement | L13, L14 | Re-key the Pattern H census invariant: the LAC-1E-15 67-file baseline has drifted to 71 in `crates/core/src/runtime`; the +N delta MUST trace to a grammar-roster or sub-wave change, else an O(N) generator-regression scan opens (per `[generated-size-budget]`). | totality-core census / SK-V19 adoption | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l = 71`; baseline `restart/locks/LOCKS.md:408`-`409`. |

### No-candidates axes scanned (explicit)

- 6th `BackendShape` variant: NONE — the five-shape canon holds verbatim (`lower/mod.rs:18`-`24`; SK-V18 SPEC `:75`-`78`); SK-V18 adds no shape.
- New directive / BIR variant: NONE — SK-V18 is generalization (un-fork + delete), adds no surface.
- New substrate / public substrate API / retained sidecar: NONE in the SKINNY benched tree — one `Tape`/`ValueRef` holds (S-P0 CLEAN, `SYNTHESIS-AUDIT-OVERFIT.md:109`); the phantom `<G>` is a DELETE, not an addition. TOTALITY-TREE CARRY (CH5-V1-03): the `crates/core` tree carries a LIVE per-parse `OnceCell<::simd_scan::StructuralIndex>` probe substrate emitted into **8 of 9** generated grammars (all but `math`, whose structural alphabet falls outside the `ctns_probe_admits` 12–24-byte window — `math.rs` carries only a documented-but-inert `ScanState` shell with 0 `ensure_structural_index`; breadth corrected 9→8 per CH5-V3-003) (`crates/core/src/grammar/generated/json.rs:701`, `ensure_structural_index` `:719`, `scan_structural` `:732`; gate `ctns_probe_admits` at `support.rs:74-95`; the emitter names it "The probe substrate (OnceCell + helper)" at `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67`). Its lifetime is the admissible `generated_function` class (`&mut ScanState` per parse, NOT cross-call), so it is a SK-V19-adoption Lock-1 classification carry, NOT a proven violation — but the "NONE" universality is scoped to skinny; the totality OnceCell + the `crates/simd-scan` `next_structural_at_or_after` probe API (absent from skinny `bbnf-simd`, which nonetheless carries its OWN `StructuralIndex`/`scan_dispatch`/`scan_scalar` + a skinny-only `parity_hash` per CH5-V4-011) must be classified at SK-V19 adoption under ONE shared priced disposition: the ≈ +20..+217 LOC SK-V19 scanner-unification reconcile (`wc -l crates/simd-scan/src/{index.rs,lib.rs} = 217` probe-API + the 8/9 generated-grammar `OnceCell<StructuralIndex>` emission-site re-route), cross-linked to `1F-coherence` COH18-015 and `1F-anti-pattern` OnceCell row per CH4-V4-008.
- Lock addition / retirement / re-number: NONE — all 7 candidates are additions/refinements to existing locks' clause text; the 16-lock count is preserved.
- REDRESS rejected-route pre-block (identified by 1D AND 1E per `restart/prompts/totality/PASS-1-EXCAVATION.md:117-118`; second witness added per CH3-V4-005): the four-item pre-block cluster is items 246 (W11T parse-only structural stream, `skinny/REDRESS.md:6184-6219`, REJECT), 247 (W11V parse-only string64 mask, `:6230-6260`, REJECT), 51 (SK-V5 `JsonEventCursor`, `:742-768`, REJECT), 53 (SK-V5 `JsonStructuralCursor`, `:784-813`, REJECT) — enumerated in full at `1D-skinny-lessons.md:166-171`. The SK-V18 G2 (`css_balanced_component_scan`), G4 (lazy `Cursor`/`CssNode`), and G6 (NEON retarget) moves each ADJOIN one of these rejects; the FALSIFYING distinction is admissible = retarget/decorate the EXISTING in-loop single-substrate leaf (G4's `Cursor` is a VIEW over the existing `Tape`/`ValueRef`/`PayloadArena`; G6 retargets NEON onto the existing `find_component_delim` shell) vs rejected = a second scanner / structural-stream driver / bespoke per-grammar mask / parser-local cursor. No reject is re-opened by any SK-V18 wave; this 1E witness binds the pre-block to the admissible single-substrate distinction so the list is no longer 1D-only.
