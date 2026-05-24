---
agent: 3C
pass: T-P3-synthesis
cycle: V4
generated_at: 2026-05-23T00:00:00Z
v1_surface_targeted: restart/locks/LOCKS.md
diff_status: proposed-only
---

# 3C Proposed LOCKS v+1 Diff (V4 — incremental atop V3-merged baseline)

This document is a line-level proposed diff only. T-P3 must not edit `restart/locks/LOCKS.md`; Pass Omega CRUD applies ratified edits after G3 and G-Omega. Evidence: `restart/prompts/totality/PASS-3-SYNTHESIS.md:21`-`24`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:189`-`198`, `restart/prompts/ORCHESTRATOR.md:165`-`170`, `restart/HANDOFF.md:44`-`47`.

V4 baseline: the 12 V3 hunks were merged into `restart/locks/LOCKS.md` post-V3 §3Z LOCK via Pass Omega CRUD (HEAD `34a28f5c1`); V4 layers 9 incremental hunks atop the V3-merged text without touching it. The disposition matrix in `3C-locks-crystallisation.md` enumerates V3 hunks as `already merged at HEAD; no v+1 delta` and V4 hunks as `V4-NEW`. ACCEPT/MODIFY in this diff remains a lock-text disposition only, never an implementation admission.

No lock is renumbered. The 16-lock count stays fixed. LAC-1E-12 lands as an in-preface CH7 binding clause (NOT Lock 17) per T-P1 V5 §6.1 disposition carrier. Any new lock, lock retirement, new directive, new BIR variant, public substrate API, retained sidecar, or `BackendShape` expansion remains user/G-Omega gated. Evidence: `restart/prompts/totality/PASS-3-SYNTHESIS.md:210`-`215`, `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:616`-`617`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:182`-`192`.

## V4 hunk index

| # | Hunk | Target line(s) at HEAD `34a28f5c1` | Source LAC(s) |
|---|---|---|---|
| V4-1 | Preface CH7 Overfit-Prune binding clause | before `restart/locks/LOCKS.md:44` `## Gestalt — sixteen locks` | LAC-1E-12 (promoted) |
| V4-2 | Lock 1 substrate-union v+1 elevation (no cross-call retention) | append after `restart/locks/LOCKS.md:90` | LAC-2F-V5-02 ELEVATED |
| V4-3 | Lock 1 FactStream 5th substrate category | append after `restart/locks/LOCKS.md:71` | LAC-1E-14 |
| V4-4 | Lock 6 + Lock 14 regen-roundtrip clean check | append to Lock 6 at `restart/locks/LOCKS.md:115` and Lock 14 v+1 generated-output allowance at `restart/locks/LOCKS.md:222`-`238` | LAC-1E-13 |
| V4-5 | Lock 8 audit-overlay 4-column + numeric abrogate gates | append after `restart/locks/LOCKS.md:128` | LAC-1E-16 + T2A-LAC-V1-05 |
| V4-6 | Lock 10 cohort-wide BBNF_SIMD_STRICT + regex/HIR mandate | append after `restart/locks/LOCKS.md:188` | LAC-2F-V5-04 + V3 F-V3-CH4-A |
| V4-7 | Lock 14 Pattern H census + byte_class_from_range_64 sibling | append after `restart/locks/LOCKS.md:253` | LAC-1E-15 + LAC-2F-V5-03 |
| V4-8 | Lock 16 CollapsedStage x86-only co-requirement | replace `restart/locks/LOCKS.md:344`-`349` | LAC-2D-06 |
| V4-9 | Lock 16 bbnf-regex::Dfa admissibility + CH3 pre-flight reflex | append after `restart/locks/LOCKS.md:364` | LAC-2F-V5-01 |

V3 hunks (already merged at HEAD, preserved verbatim from V3 cycle for traceability):

| V3 # | V3 hunk title | V3 target | V3 status |
|---|---|---|---|
| V3-1 | Supersede Scoped SK-V9 Allowance History | `restart/locks/LOCKS.md:1`-`13` (HEAD) | Merged |
| V3-2 | Lock 1 Substrate-Ceiling, Fact-Stream, And Union History | `restart/locks/LOCKS.md:50`-`90` | Merged |
| V3-3 | Lock 2 Live-First Layout Wording | `restart/locks/LOCKS.md:94`-`100` | Merged |
| V3-4 | Lock 3 Empty-Path Verification | `restart/locks/LOCKS.md:104`-`109` | Merged |
| V3-5 | Lock 8 Row-Plane And BENCH Section 8 Non-JSON Feed | `restart/locks/LOCKS.md:121`-`153` | Merged |
| V3-6 | Lock 9 Runtime API Obligations | `restart/locks/LOCKS.md:157`-`162` | Merged |
| V3-7 | Lock 10 Decision-Engine + Cost + Five-Shape Fence | `restart/locks/LOCKS.md:166`-`188` | Merged |
| V3-8 | Locks 11 + 12 Workspace Drift | `restart/locks/LOCKS.md:192`-`205` | Merged |
| V3-9 | Lock 13 Generated And Gate/Report Exceptions | `restart/locks/LOCKS.md:209`-`218` | Merged |
| V3-10 | Lock 14 Generated Output + Per-Wave Gate + Grammar-Policy Transfer | `restart/locks/LOCKS.md:222`-`263` | Merged |
| V3-11 | Lock 15 Skinny Versus Root Profile Scope | `restart/locks/LOCKS.md:272`-`280` | Merged |
| V3-12 | Lock 16 Manifest + Strict Checkasm + Escape Mask + Orphans + Hardware Gates | `restart/locks/LOCKS.md:309`-`364` | Merged |
| V3-13 | G-Omega Boundary Footer | `restart/locks/LOCKS.md:366`-`375` (`## v+1 Governance Boundary`) | Merged |

## Hunk V4-1 — Preface CH7 Overfit-Prune Binding Clause

Target: insert before `restart/locks/LOCKS.md:44` `## Gestalt — sixteen locks` heading, after the `# Hardening pass — plan set` preamble closing at `restart/locks/LOCKS.md:42`.

```diff
+## CH7 Overfit-Prune lens binding
+
+Per `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62`-`87`, every plan + every
+REDRESS entry + every audit + every hardening cycle at every CHALLENGE phase runs
+CH1-CH7, not CH1-CH6. CH7 (Overfit-Prune) is a first-class lens with the same
+blocking authority as CH1 (Correctness). A CH7 REJECT triggers (a) immediate plan
+revise for plan artefacts, OR (b) immediate redress revert with a new REDRESS
+entry for implementation artefacts. CH7 cannot be carried as
+"acknowledged but not blocking".
+
+The CH7 lens scans for: fabricated baselines; cited-but-absent surface text;
+counter-surface fabrication (asserting prose into a document that does not
+contain it; meta-CH7 collision pattern per
+`restart/audit/totality/p1/1F-coherence-scan.md:64,83,100,109,117` COH-012);
+SK-V14 cohort 31:69 = 31.7% refutation density preservation; anti-paper-close
+anchor enumeration. Authority: T-P1 V5 §6.1 disposition (carrier: in-preface
+clause, NOT Lock 17 — preserves the 16-lock count per
+`restart/prompts/totality/PASS-3-SYNTHESIS.md:210`).
+
+Evidence: `restart/audit/totality/p1/hardening/V1/CH7.md:64`,
+`restart/audit/totality/p1/hardening/V1/CH7.md:180`-`181`,
+`restart/audit/totality/p1/hardening/V1/CH7.md:208`,
+`restart/audit/totality/p1/hardening/V1/CH7.md:218`,
+`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:604`-`619`,
+`restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:48` (CH7
+3-cycle LOCK in T-P2 cohort).
+
```

Candidate coverage: LAC-1E-12 (promoted to T-P3 §3C-priority per V1 CH7 recommendation).

## Hunk V4-2 — Lock 1 Substrate-Union v+1 Elevation (No Cross-Call Retained Classifier State)

Target: append after Lock 1 substrate-union manifest paragraph at `restart/locks/LOCKS.md:90`, before the next lock's blank line.

```diff
+
+    **2026-05-23 v+1 substrate-union ELEVATION (LAC-2F-V5-02; T-P3 §3C
+    amendment surface)**: no cross-call retained classifier state. Period.
+    Quote-mask, escape-mask, structural-mask, class-stream, prev-state byte,
+    prefix-XOR carry word, or any prefix carry of any kind — none is admissible
+    under Lock 1 substrate-union. Carry MUST stay within a single chunk-call
+    boundary. The closure of REDRESS 96 / 97 / 98 (retained class-column /
+    streaming structural cursor / class-lane-only on M5 Max) generalises to ALL
+    transient classifier-state primitives, not just the three falsified shapes.
+    Every Layer 1 primitive declares
+    `retention_lifetime ∈ {transient-single-call, retained-within-chunk,
+    retained-across-call-boundary}`; the third value is the REJECT class under
+    Lock 1 v+1. Any future SIMD primitive that proposes cross-call
+    classifier-state retention is REJECT under Lock 1 v+1 without further
+    measurement. The per-call composed form (e.g. Gap PTG-PREV-IN-STRING-LOCK1,
+    `scan_string_with_carry_64`) is the admissible substrate-union-compatible
+    primitive; the per-call SIMD ceiling sits structurally below simdjson's
+    published 1 GB/s by construction. Evidence:
+    `restart/audit/totality/p2/2F-parse-that-gaps.md:519`,
+    `restart/audit/totality/p2/2F-parse-that-gaps.md:490`,
+    `restart/audit/totality/p2/2B-primitive-vocabulary.md:233`-`306`,
+    `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:182`-`192`
+    (cohort §4 row 4 STRONGEST AMENDMENT SURFACE).
```

Candidate coverage: LAC-2F-V5-02 ELEVATED (T-P2 V3 §3Z LOCK strongest amendment surface).

## Hunk V4-3 — Lock 1 FactStream 5th Substrate Category

Target: append after the fact-stream paragraph closing at `restart/locks/LOCKS.md:71`, before the substrate-target manifest paragraph at `restart/locks/LOCKS.md:73`.

```diff
+
+    **v+1 FactStream 5th substrate category (LAC-1E-14)**: `FactStream` is the
+    5th admitted-product category at the Lock 1 substrate manifest, alongside
+    `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage`. A fact-stream
+    row carries `substrate_target = admitted_fact_output` per the manifest
+    vocabulary below; comparator/oracle provenance and gate-consumed telemetry
+    remain mandatory per the fact-stream paragraph above. The 5th category is a
+    substrate-manifest classification only; it is NOT a 6th `BackendShape`
+    variant. The 5-shape `BackendShape` search domain at Lock 10 holds:
+    `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`. Adding a
+    6th `BackendShape` variant remains G-Omega gated per Lock 10 v+1 and PASS-3
+    §8.1. This amendment resolves the CSS L4 declaration-values
+    substrate-classification gap surfaced at
+    `restart/audit/totality/p1/1C-runtime-evidence.md:102` (1C-D5) and
+    `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH2.md:87` (F2
+    zero-profile-evidence carry). Evidence:
+    `restart/audit/totality/p1/1E-locks-evidence.md:124`,
+    `skinny/RESULTS.md:94`.
```

Candidate coverage: LAC-1E-14.

## Hunk V4-4 — Lock 6 + Lock 14 `cargo xtask regen-{grammar}` Round-Trip Clean Check

Target A: append to current Lock 6 paragraph after `restart/locks/LOCKS.md:115`.

```diff
+
+    **v+1 regen round-trip discipline (LAC-1E-13)**: every file carrying
+    `// @generated by skinny bbnf-codegen` (or equivalent rostered header) MUST
+    (a) trace to a rostered xtask emission (`cargo xtask regen-{grammar}`); (b)
+    emit byte-equivalent output when regenerated from grammar source + workspace
+    metadata; (c) reject hand-patching per memory `[clean-regen-discipline]`.
+    The round-trip clean check distinguishes real codegen from fake `@generated`
+    `include_str!` templates (the SK-V13 W1b CSS L4 pathology). R4 CSS L4 is
+    the first instance; the family extends to JSON / Sheets / BBNF / EBNF / BNF
+    / CSV / Math. Evidence:
+    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:96` R4,
+    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:110`-`120` P-1 fake @generated
+    recurrence vector,
+    `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:153,184`
+    (8 fake-codegen providers in `skinny/crates/codegen/src/`).
```

Target B: append to current Lock 14 v+1 generated-output allowance paragraph after `restart/locks/LOCKS.md:229`, before the generic-crates consumption paragraph at `restart/locks/LOCKS.md:231`.

```diff
+
+    The generated-output allowance is bound to the Lock 6 v+1 regen round-trip
+    clean check (LAC-1E-13). A file under `runtime/src/grammars/<name>/`
+    carrying `// @generated` survives Lock 14 only when `cargo xtask
+    regen-{grammar}` produces byte-equivalent output from grammar source +
+    workspace metadata; hand-patched generated files are Lock 14 violations.
+    Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:123`,
+    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:96`.
```

Candidate coverage: LAC-1E-13.

## Hunk V4-5 — Lock 8 Audit-Overlay 4-Column + Numeric Abrogate Gates

Target: append after Lock 8 row-plane accounting paragraph at `restart/locks/LOCKS.md:128`, before the comparator-plane provenance paragraph at `restart/locks/LOCKS.md:130`.

```diff
+
+    **v+1 audit-overlay column binding (LAC-1E-16)**: every gate-consumed
+    `skinny/RESULTS.md` row carries four required schema columns:
+    `track2_entry_point`, `comparator_plane`, `per_iter_equality`,
+    `audit_overlay_verdict`. `xtask gate-json` REJECTS any row missing any of
+    the four — an admitted row missing a required column is no admit at all.
+    Falsifiability gate companion to the row-plane accounting above. Evidence:
+    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:240`-`255`,
+    `restart/skinny/tranches/sk-v14/SYNTHESIS.md:230`,
+    `restart/audit/totality/p1/1E-locks-evidence.md:126`.
+
+    **v+1 numeric abrogate-gate binding (T2A-LAC-V1-05; V3 F-V3-CH4-B
+    numeric-bound at `restart/audit/totality/p2/2D-cost-model.md:151`-`162`)**:
+    every gate-consumed comparator + bench run carries the 6 abrogate-gate
+    numerics from T2A-LAC-V1-05: e-graph saturation ≤50000 nodes / ≤10000
+    classes / ≤30 iter; CSP timeout ≤1 s/grammar; stale-cost ≤30%;
+    generated-LOC growth bound to `loc_budget`; row regression admit;
+    parity/checkasm failure. Any abrogate-gate trip rejects the wave; numbers
+    are uniform across cohort dossiers 2A:192 + 2C:303-305 + 2D:142-149.
+    Evidence:
+    `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:141`-`145`
+    (F-V3-CH4-B numeric-bind).
```

Candidate coverage: LAC-1E-16 + T2A-LAC-V1-05 (numeric-bound).

## Hunk V4-6 — Lock 10 Cohort-Wide `BBNF_SIMD_STRICT=1` + Regex/HIR Fact Mandate

Target: append after Lock 10 fail-closed paragraph at `restart/locks/LOCKS.md:188`.

```diff
+
+    **v+1 cohort-wide `BBNF_SIMD_STRICT=1` precondition (V3 F-V3-CH4-A)**:
+    SIMD admissibility under Lock 10 requires `BBNF_SIMD_STRICT=1` cohort-wide,
+    not merely per-primitive. Institutionalized at three load-bearing cohort
+    sites with mutual cross-references:
+    `restart/audit/totality/p2/2A-sota-landscape.md:192`,
+    `restart/audit/totality/p2/2C-grammar-neutrality.md:303`-`305`,
+    `restart/audit/totality/p2/2D-cost-model.md:142`-`149`. Non-strict parity is
+    exploratory only and cannot admit a primitive, route, or row at the
+    decision-engine layer (per Lock 16 v+1 admission checkasm rule). Evidence:
+    `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:74`
+    (F-V3-CH4-A discharge).
+
+    **v+1 regex/HIR fact mandate (LAC-2F-V5-04; strengthens V4 LAC-2F-03)**:
+    regex/HIR facts are MANDATORY inputs to CSP/egraph/cost selection. Opaque
+    pattern strings of the form `SinkOnlyExpr::RegexProgram { pattern: String }`
+    (`crates/codegen/src/lower/sink_only.rs:19`-`93`) are insufficient for
+    backend-shape or scanner selection. The decision-engine consumes
+    regex-engine HIR facts (state count, lazy-DFA viability, NFA branching,
+    Aho-Corasick eligibility) from the absorption-wave-resolved `bbnf-regex`
+    crate (LAC-2F-V5-01 Q1 + SK-V14 W11 absorption). Opaque-pattern-only
+    selection is non-admitting at the cost-model layer. Evidence:
+    `restart/audit/totality/p2/2F-parse-that-gaps.md:521`,
+    `restart/audit/totality/p2/2D-cost-model.md:120` (T2D-REGEX-NFA-DFA-PLAN).
```

Candidate coverage: LAC-2F-V5-04 + V3 F-V3-CH4-A.

## Hunk V4-7 — Lock 14 Pattern H Census + `byte_class_from_range_64` Sibling Primitive

Target: append after Lock 14 grammar-neutral primitives paragraph closing at `restart/locks/LOCKS.md:263`, before the next lock.

```diff
+
+    **v+1 Pattern H per-tranche census (LAC-1E-15)**: every tranche commits a
+    Pattern H file-count transcript via `find crates/core/src/runtime
+    -mindepth 2 -maxdepth 2 -type f -name '*.rs' \| wc -l` (and the skinny
+    mirror equivalent). Tranche +N over prior tranche MUST trace to (a) a
+    grammar-roster change (e.g. css_pretty +7 from SK-V13 to SK-V14) OR (b) a
+    sub-wave count update (e.g. PRUNE-4 9 sub-waves). Substrate templates at
+    `crates/core/src/runtime/builder_template.rs:13`-`31` and
+    `crates/core/src/runtime/arena_template.rs:1`-`31` MUST NOT enshrine
+    hot-grammar opt-outs in doc-comments — the opt-out doc-comment passages
+    are themselves Lock 14 violations under "any plan that introduces
+    grammar-specific code in a generic crate is a fault" per the lock body
+    above. Pattern H 67-file recurrence is the category-scale failure Lock 14
+    was authored to prevent. Evidence:
+    `restart/audit/totality/p1/1E-locks-evidence.md:125`,
+    `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:10`-`12`,
+    `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:41`-`56`,
+    `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:153`-`157`.
+
+    **v+1 abstract-primitive sibling (LAC-2F-V5-03)**: `byte_class_from_range_64`
+    (PTG-RANGE-CLASS-PRIMITIVE) is pinned as a sibling of
+    `byte_class_from_eq_set_64` in the abstract-primitive declaration list.
+    The two-primitive split (set ≤8 vs inclusive range) is the load-bearing
+    grammar-neutral generalization vehicle for digit-run / UTF-8-continuation
+    / CSS hex / BBNF identifier classification. Per memory feedback
+    `[regex-generalized]`, the range primitive lives in `bbnf-simd`, not
+    `bbnf-lang`. Evidence:
+    `restart/audit/totality/p2/2F-parse-that-gaps.md:520`.
```

Candidate coverage: LAC-1E-15 + LAC-2F-V5-03.

## Hunk V4-8 — Lock 16 `CollapsedStage` x86-Only Co-Requirement + `BackendExpr.substrate_target`

Target: replace current `CollapsedStage` paragraph at `restart/locks/LOCKS.md:344`-`349`.

```diff
-    `CollapsedStage` is admissible only as a concrete emitted transient
-    strategy with scalar reference, strict parity/checkasm, feature gate,
-    local temporary lifetime, and same-wave measured consumer. AVX-512
-    literature is x86 architecture pressure and cannot close M5/aarch64 rows.
-    Evidence: `restart/audit/totality/p2/2D-cost-model.md:191`,
-    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:223`-`230`.
+    `CollapsedStage` is admissible only as a concrete emitted transient
+    strategy with scalar reference, strict parity/checkasm, feature gate,
+    local temporary lifetime, and same-wave measured consumer. AVX-512
+    literature is x86 architecture pressure and cannot close M5/aarch64 rows.
+
+    **v+1 predicate hardening (LAC-2D-06; CH5 F-CH5-V1-03)**: the live
+    `admits_collapsed_stage` predicate at `skinny/crates/passes/src/lib.rs:874`-`876`
+    MUST co-require `target.arch == x86` alongside `target.avx512bw` and
+    `Entry(_)`, refusing aarch64 admission via cross-build
+    `target.avx512bw` inheritance. Marker-string lowerers at
+    `skinny/crates/codegen/src/lower/collapsed_stage.rs:15`-`17` are not
+    admissible (P1-1B-D6). Every `BackendExpr` node / rewrite guard /
+    extraction result MUST declare
+    `substrate_target ∈ {local_temp_only, existing_tape, direct_sink,
+    admitted_fact_output}` per Lock 1 v+1 manifest. E-graph extraction MUST
+    reject plans whose `substrate_target` is not one of the four admitted
+    values. Until a generated aarch64 strategy lands (UNKNOWN-2D-05 +
+    2E source-backed aarch64 candidate), `CollapsedStage` admission is
+    mechanically refused on aarch64.
+    Evidence: `restart/audit/totality/p2/2D-cost-model.md:265`,
+    `restart/audit/totality/p2/2D-cost-model.md:123`,
+    `restart/audit/totality/p2/2D-cost-model.md:191`,
+    `restart/skinny/tranches/sk-v13/SYNTHESIS.md:223`-`230`,
+    `skinny/crates/passes/src/lib.rs:874`-`876`.
```

Candidate coverage: LAC-2D-06 (V2-NEW; CH5 F-CH5-V1-03).

## Hunk V4-9 — Lock 16 `bbnf-regex::Dfa` Admissibility + CH3 Pre-Flight Reflex

Target: append after Lock 16 PMULL/CSSC/parse-that hardware-facade paragraph at `restart/locks/LOCKS.md:364`, before `## v+1 Governance Boundary` heading at `restart/locks/LOCKS.md:366`.

```diff
+
+    **v+1 `bbnf-regex::Dfa` admissibility (LAC-2F-V5-01)**: contingent on
+    absorption-wave Q1 resolution (SK-V14 W11), the manifest gains a
+    `bbnf-regex::Dfa` admissibility row. Admissibility requires (a) a scalar
+    reference (Hoehrmann/Thompson straightforward construction at
+    `regex-engine.md:28`-`44`); (b) checkasm-parity equivalence to
+    `regex_automata::meta::Regex::find` over the byte stream; (c) a same-wave
+    consumer (host-fn or leaf-parser dispatch site).
+
+    **CH3 pre-flight reflex (V6 F-CH3-2F-08, LOW prophylactic)**: before any
+    `bbnf-regex::Dfa` admissibility row dispatches, the absorption wave MUST
+    (i) execute a CH3-class REDRESS regression scan over `skinny/REDRESS.md`
+    and `restart/skinny/tranches/sk-v{1..14}/` for any prior DFA / NFA /
+    Aho-Corasick / regex-engine admission attempt, recording the result inline
+    as a precondition; (ii) the amendment MUST carry an explicit REDRESS
+    pre-block citation listing — at minimum the routes the forward absorption
+    MUST NOT re-open: REDRESS 96 (retained class-column substrate, falsified
+    per `skinny/REDRESS.md:2797`-`2848`), REDRESS 97 (streaming structural
+    cursor, falsified per `:2852`-`2906`), REDRESS 98
+    (`G-W3-UNION-SUBSTRATE` retired per `:2910`-`2950`), plus any prior
+    regex-shaped admission attempt surfaced by clause (i). This restores
+    parity with LAC-2A-V1-01 / LAC-2D-05 / LAC-2E-04 / LAC-2F-V5-02 /
+    LAC-2F-V5-04, which all carry the REDRESS pre-block citation inside
+    amendment text. Evidence:
+    `restart/audit/totality/p2/2F-parse-that-gaps.md:518`,
+    `restart/audit/totality/p2/2F-parse-that-gaps.md:23` (v6_fold F-CH3-2F-08).
```

Candidate coverage: LAC-2F-V5-01 (V5-NEW; CH3 F-CH3-2F-08 pre-flight reflex).

## V4 Diff Application Notes

- All V4 hunks are append-only or single-paragraph replacements; no V3-merged text is touched. The diff applies cleanly against `restart/locks/LOCKS.md` at HEAD `34a28f5c1`.
- Total V4 incremental diff line count: ~250 lines (approx ~60 lines for hunk V4-1; ~25 each for V4-2/V4-3/V4-6/V4-7-second-block/V4-8/V4-9; ~20 each for V4-4-A/V4-4-B/V4-5/V4-7-first-block).
- 16-lock count preserved: LAC-1E-12 lands as preface clause; LAC-1E-14 lands as Lock 1 substrate-category sub-paragraph (NOT a 6th `BackendShape`).
- All diffs proposed-only per `restart/prompts/totality/PASS-3-SYNTHESIS.md:21`-`24`; Pass Omega CRUD applies post-G-Omega per `restart/prompts/totality/PASS-3-SYNTHESIS.md:189`-`198`.

## v+1 Governance Boundary (V4 reaffirmation)

The V4 text above is proposed by T-P3 only. It is not active LOCKS text until G3 accepts the T-P3 V4 packet, Pass Omega CHALLENGE converges, and G-Omega authorizes CRUD operations on governance surfaces. No implementation wave may use proposed v+1 wording as permission to edit source, write RESULTS/REDRESS, add a directive, add a BIR variant, add or retire a lock, expand `BackendShape` (the 5-shape canon at Lock 10 holds even with LAC-1E-14 5th substrate category folded), add a public substrate API, retain a sidecar, or dispatch SK-V14 W0 / S-P3 / SK-V13 wave admission before G-Omega closes. Evidence: `restart/prompts/totality/PASS-3-SYNTHESIS.md:179`-`198`, `restart/prompts/ORCHESTRATOR.md:165`-`170`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:317`-`343`.
