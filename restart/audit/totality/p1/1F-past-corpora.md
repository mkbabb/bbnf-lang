---
agent: 1F
pass: T-P1-excavation
cycle: V5-SKV18-totality
generated_at: 2026-06-01T00:00:00Z
scope: past-corpora scan (sk-v{1..18} research + prior audit reports) — findings THIS cycle must NOT re-derive
companion: restart/audit/totality/p1/1F-coherence-scan.md (authoritative live coherence packet)
sources_scanned:
  - restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
  - restart/skinny/tranches/sk-v18/research/p1/SYNTHESIS-PROFILE.md
  - restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md
  - restart/skinny/tranches/sk-v18/SPEC.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/p1/1F-coherence-scan.md (prior V4 SK-V15 packet)
---

# 1F Past-Corpora Do-Not-Re-Derive Ledger — SK-V18 Totality Cycle (V5)

Findings already established in the sk-v1..sk-v18 corpus that THIS totality cycle
must NOT re-derive from scratch. Each row binds the prior finding to its
already-named receiver so the totality fold cites it, never rediscovers it.

## A — Already-Validated Facts (do NOT re-litigate as open)

| Finding | Prior-corpus evidence | Binding implication |
|---|---|---|
| **SK-V18 headline >SOTA — JSON SETTLED, CSS DIRECTIONAL** | JSON half: `sk-v18/SYNTHESIS-AUDIT-OVERFIT.md:36` "JSON cold strict +1.4%–164.7%" backed by `RESULTS.md:5-25` measured rows (twitter 8349>4913, citm 9079>8335, canada 16709>12970, per-iter PASS) + `SPEC.md:184` 51/51 guard. CSS half: `SYNTHESIS-AUDIT-OVERFIT.md:36` "CSS canonical cold N=200 1.9–3.3×" is a synthesis-doc ASSERTION, not a bench-row table. | SPLIT (CH6-F4). JSON half is SETTLED — bench-row-backed, do NOT re-litigate. CSS half is DIRECTIONAL, NOT re-locked: the 1.9–3.3× ratio (bootstrap 2.190, tailwind 3.375, material 1.658, animate 2.101, `SPEC.md:113-118`) ran under loadavg 4.35; the H1 `css_canon_bench` re-lock gate (1D U-4 `1D:239-243`) is the falsifier. Do NOT re-derive CSS as "audit-demoted/contrived" (SK-V15 posture, prior V4 COH-002/009) AND do NOT freeze the CSS ratio as a validated bench fact — it is newly measurement-valid (NOT a fake admit) but NOT yet a re-locked bench row. The residual is hand-written/forked/replicated IMPLEMENTATION. |
| **JSON 51/51 strict guard holds, same-plane** | `sk-v18/SPEC.md:184` "JSON 51/51 strict-vs-sonic-rs cold rows remain admitted, same-plane"; `SYNTHESIS-AUDIT-OVERFIT.md:139` | Do NOT re-derive the JSON guard as if CSS findings invalidated it (prior V4 COH-011). JSON is the W0-locked guard baseline carried forward. |
| **The unified `Tape`/`ValueRef`/`PayloadArena` substrate is CLEAN (Lock 1 holds)** | `SYNTHESIS-AUDIT-OVERFIT.md:109` "(KEEP) … the genuine foundation"; `SYNTHESIS-AUDIT-OVERFIT.md:142` A5 "substrate REAL" | Do NOT re-audit Lock 1 substrate as open. The value-API trait (G4) and the un-fork (G3) are the open work, NOT a second substrate. |
| **9-grammar census stable** | totality `crates/core/src/grammar/generated/{bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json,math}.rs` (9); skinny benches 3 (json+css+sheets-witness) | The 9-grammar roster is re-verified, NOT a new discovery. SK-V18 proves on 3; SK-V19 adopts to 9. |
| **The neutral alphabet NEON kernel is already generalized (caller-data)** | `SYNTHESIS-AUDIT-OVERFIT.md:110` "the neutral alphabet NEON kernel (caller-data, already generalized)" | Do NOT re-derive the kernel as overfit. G6 RETARGETS it onto the CSS hot leaf; the kernel itself is clean. |
| **CollapsedStage is x86-pinned; aarch64 candidate is UNKNOWN-2D-05** | carried `restart/audit/totality/sk-v17/p1/1f-coherence-scan.md:81` (SKV17-COH17-004); `restart/ARCHITECTURE.md:1206` | Do NOT re-flag the aarch64 CollapsedStage gap as undiscovered — it is the SPEC-NAMED UNKNOWN-2D-05. SK-V18 deletes x86 (P1); the canon reconcile is SK-V19/Omega. |

## B — Already-Bound Residuals (cite the wave, do NOT re-find)

The SK-V18 S-P0 audit consolidated 16 implementation residuals (R1-R16) + 3
goalset framing residuals (R-A0-1..3), each bound to a named wave
(`SYNTHESIS-AUDIT-OVERFIT.md:83-103`). The S-P0 residual roster was first verified
LIVE at the SK-V18-audit cycle HEAD `83b66db42`; every R1-R16 witness re-grounded
this pass still resolves at the current verification HEAD `4e4aa0648` (the
`83b66db42` stamp is the inherited S-P0-cycle anchor, CH1-V1-F16). This cycle MUST
cite these, not re-derive them:

| Residual | Witness | Bound wave | Do-not-re-derive note |
|---|---|---|---|
| R1 CSS const-`&str` courier | `runtime_generator.rs:701` | G2 | Re-grounded live this pass (COH18-003). |
| R2 JSON fixed-literal render | `json_sink_direct.rs` 7× push_str | G1 | The JSON `_RS` literals (`runtime_generator.rs:195`…`:665`). |
| R3 `RuntimeEmitterKind` fork | `grammar_provider.rs:40-42` | G3 | Re-grounded live (COH18-003). |
| R4 7 byte-identical css_l4 replicas | md5 `b654562c…` | P3 | Re-confirmed md5-identical live (7× `b654562ccff46ed62dd48e9ace325830`). |
| R5 phantom `<G>` | `tape/mod.rs:175` | G4 | Re-grounded live (COH18-008). |
| R8 x86 two surfaces | `src/x86_64/`+`ext/x86/`+nasm | P1 | Re-confirmed live in skinny; x86-free in totality (COH18-009). |
| R9 Lock-14 green-by-exclusion | `lock14_baseline.rs:2442/2463` | P4 | Carried; the totality analog is the `codegen/`-scoped leak scan (COH18-012). |
| R13 warm micro-fixture CSS bench | `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3091` (`fn measure_mbps`) | P2 | god-file census this pass (3737 LOC). The bare `nonjson_css_l4.rs` name is AMBIGUOUS — two files carry it: `src/nonjson_css_l4.rs` (3737 LOC; `:3091` in range) and `benches/nonjson_css_l4.rs` (318 LOC; `:3091` out-of-range). The 3737-LOC/`:3091` witness resolves ONLY against `src/` (CH1-V2-F5 disambiguated). |
| R15 metalang leak `parse_w11_1_number ×7` | `json/generated.rs` | P5 | rename-only. |
| R16 nested-struct gate-recipe hazard | `skinny/xtask/src/regen.rs:5` (`#[derive(Clone, Copy, Debug)]` over `pub(crate) struct RuntimeTarget` `:6`) | S-P3 recipe pin | Skinny `RuntimeTarget` derives only `Clone,Copy,Debug` at `skinny/xtask/src/regen.rs:5` — R16 adds the `+1-line PartialEq`. (Bare `regen.rs:17-18` re-anchored: `:17-18` are the `frontend_requirements`/`output_labels` fields, NOT the recipe; `entry_rule`/`source_roots` are at `skinny/xtask/src/regen.rs:9-10`. The load-bearing pin is the `:5` derive. Disk-verified live this pass — CH1-V2-F1 reject corrected; the prior V1-inherited `:17-18`=`entry_rule`/`source_roots` claim was FALSE.) Confirmed live this pass. |

## C — Prior-Cycle Pre-Blocks (route patterns this cycle must NOT reopen)

| Pre-block | Prior-corpus source | Binding |
|---|---|---|
| No verbatim-blob `@generated` courier | `SYNTHESIS-AUDIT-OVERFIT.md:50` addendum 1 | A courier-swap or string relocation under `@generated` is REJECT; `verbatim_blob_present==false` is the gate. |
| No md5-identity-as-proof | addendum 2 (`:51`,`:59`) | md5-distinctness is NECESSARY-NOT-SUFFICIENT; also need branch-count==0 + type-count==0 + row-collapse. The relocated seam (grammar branch in a neutral table) is caught ONLY structurally. |
| No phantom-generic ≥2-count gaming | addendum 4 (`:53`,`:64`) | The ≥2 impl count is necessary-not-sufficient; `json_rich_navigation_preserved==true` is the companion (preserve-rich-ast); the trait may NOT LCD-flatten JSON. |
| No warm/micro-fixture comparator | addendum 5 (`:54`); `feedback_no_warm_benches` | corpus-in-timer, cold, real-corpus only; `css_canon_bench` is the honest path. |
| No `#[cfg(test)]`-only acceleration claim | addendum 6 (`:55`) | NEON must reach the hot path AT ADMISSION; a `generated.rs` call site in dead code the profile does NOT sample == `dead`, NOT `admission`. |
| No "named primitive" paper-close | R-A0-3 (`:103`); `SYNTHESIS-RESEARCH.md:257-266` | The §6 escape is admissible ONLY under (a) grammar-INVOKED + (b) emitted-output-VARIES-under-rule-mutation + (c) `verbatim_blob_present==false` + (d) PROFILE-PROVEN-NARROW-LEAF. |
| No fabricated distinctness | R-A0-2 (`:102`) | Manufacturing 7 fake `.bbnf` roots to satisfy a distinctness gate is the EXACT overfit forbidden; `generator_grammar_count==3` (json+css+sheets, NOT json+7css+sheets). |
| No re-derivation of the SK-V15 PRUNE/REBUILD coherence packet | prior V4 `1F-coherence-scan.md` COH-001..016 | That packet was the SK-V15 CSS-demoted posture; SK-V18 supersedes it with the measurement-valid generalization framing. Cite as history only. |
| No re-opening the x86-CollapsedStage canon as a NEW finding | SKV17-COH17-004 | It is the spec-named UNKNOWN-2D-05; aarch64-only is the SK-V18 plane. |

## D — Cross-Tranche Lessons (the empirical floor SK-V18 stands on)

| Lesson | Source | Binding for the totality fold |
|---|---|---|
| SK-V13: S-P0's progenitor audit found the headline ADMITTED numbers were FAKE | `SYNTHESIS-AUDIT-OVERFIT.md:34` | SK-V18 is the OPPOSITE case — measurement-valid headline, implementation overfit. Do not conflate the two failure modes. |
| SK-V15: CSS audit-demoted (broadcast admits, comparator mismatch, CSS_GENERATED_RS contrivance) | prior V4 COH-002/009 (the specific superseded-packet IDs, not a bare "prior V4 packet") | SK-V15 ADDRESSED the broadcast/comparator contrivances (prior V4 COH-002/009); the CSS measurement is now directional-not-fake but NOT yet re-locked (per 1D U-4 / the H1 `css_canon_bench` gate) — "RESOLVED" downgraded per CH6-V3-F2, the closure word is not carried into the SK-V18 inheritance predicate. SK-V18 targets only the IMPLEMENTATION fork. |
| SK-V16/V17: shared flat-tape substrate landed + lazy-`ValueRef` + shared-NEON contract | `restart/HANDOFF.md:6-10` | These are the FOUNDATION SK-V18 generalizes ATOP — not re-built. The substrate is Lock-1-clean (row A above). |
| SK-V17→SK-V18 boundary | `restart/HANDOFF.md:17` (stale: says SK-V18 = totality-adopt) | SK-V18 was RE-SCOPED to skinny-generalization (COH18-001); the totality `crates/core/` adoption moved to SK-V19. HANDOFF must be re-authored — this is the do-not-re-derive note for the boundary itself. |

## What This Cycle CAN Add (not pre-blocked)

The totality-tree relocated-seam analog (`crates/ir/src/registry/strategy.rs:137-155`
grammar-named ident table) and `css_types.rs` persistence are NEW this-cycle
groundings (COH18-005/006) — the SK-V19 totality fold receivers. They are the
totality mirror of the skinny `RuntimeTarget` R16 collapse, surfaced here so
SK-V19 inherits the structural-collapse obligation rather than re-discovering it.
