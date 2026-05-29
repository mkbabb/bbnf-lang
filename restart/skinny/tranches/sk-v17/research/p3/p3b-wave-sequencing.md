# SK-V17 P3-B: Wave Sequencing

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-29.
Scope: Order the P3-A ≤8 active shortlist (S-P2 survivors L1-L8 active + L9 conditional) into waves W0…W{n}. W0 = baseline/telemetry lock; behaviour waves follow. Topological — substrate before consumer, guard rows before risk rows. Per-wave: entry gate, owner-path family, conditional-dispatch status, hard cap. THE binding wave manifest is `restart/skinny/tranches/sk-v17/SPEC.md` Section 2 (W0–W5, six waves); this artefact sequences to that manifest exactly.
Output: this file.
Pass Alpha goalset: SYNTHESIS.md §0.1 close-condition (tape activation + layout-driven projection + CSS typed equality + CSS >SOTA on ≥1 regular corpus animate/bootstrap at N≥50 median vs lightningcss full-CSSOM + NEON hot-leaf union + JSON 51/51 guard + clean regen), §0.5 per-corpus close, §0.6 strict comparator gate, Section 2 telemetry.
Candidate pool: research/p2/ post-CHALLENGE survivors L1-L9 (HARDENING-S-P2-V3-CONSOLIDATED §3, commit f87ee713a).

## §0 — Cycle V3 fold note (SPEC-citation re-key; content ACCEPTed)

The V2 CHALLENGE ACCEPTed P3-B: REVISE-3 verified (`p3b:10-31` re-authored to the SPEC
six-wave manifest verbatim — W0/W1-tape/W2-projection/W3-NEON/W4-L9/W5-close, L7→W1,
L4→W2, L9 post-W1); R-CH7-1 verified (`p3b:132` wave count = 6, >SOTA gate uniformly
W3-attributed). The residual V3 REVISE items touching wave placement (CH4-6 single-value
L4→W2; CH5-2 L8→W2 with its branch-tag guard) route to P3-A + P3-C to **reconcile to the
binding P3-B/SPEC placement** — P3-B already lands L4 in W2 (`§1` Note-on-L4, `§2` W2
manifest) and L8 in W2 (`§2` W2), so no P3-B placement edit is required. This V3 re-keys
ONLY the stale `SPEC.md:` line citations: the SPEC grew when the W1/W2 consumer
enumeration + R-CH2-1 byte-equal check folded in (W1 PRUNE `:382→:390`, W2 projection
`:466→:494`, W3 NEON `:534→:583`, W4 L9 `:614→:663`, W5 close `:669→:725`, §9 ledger
`:721→:777`, regen 9/9 `:687,696→:743,752`, post-W1 L9 gate `:616,637,786→:666-672,690,842`,
W1 NO-speed-admission `:447→:475`, W2 -2.0%/no-worse `:564`). Content is correct, the
six-wave topology is unchanged, no route is re-opened.

## §0.1 — Cycle V2 fold note (re-sequence to the SPEC six-wave manifest)

The V1 CHALLENGE returned a load-bearing REVISE (D1 / R-CH2-2 / CH4-2 / CH4-3 / CH5-1
/ R-CH7-1 / REVISE-3): V1 P3-B sequenced a **five-wave** map (W0…W4, W2=NEON,
W3=L9, W4=close) while the binding `SPEC.md` Section 2, P3-C, and P3-F all carry a
**six-wave** map (W0…W5, W1=tape PRUNE, W2=layout projection, W3=NEON, W4=L9,
W5=close). The same candidates were bound to different wave ordinals across the
cohort — an orphan REVISE the orchestrator could not dispatch (the >SOTA close gate,
the index==offsets coupling, the L9 re-profile, all attributed to the wrong wave
number).

This V2 re-authors P3-B to the **SPEC six-wave manifest verbatim** (`SPEC.md:260-271`):
the V1 W1 (tape+projection merged) is split into SPEC **W1 (PRUNE/tape)** + SPEC
**W2 (layout-driven projection)**; the V1 W2 NEON wave shifts to SPEC **W3**; the
V1 W3 L9 wave shifts to SPEC **W4**; the V1 W4 close shifts to SPEC **W5**. The
candidate-to-wave placement is reconciled to `SPEC.md` Sections 4-8 and P3-C §1.3:
L7 lands in **W1** (not the NEON wave — CH4-3 fix), L4 lands in **W2** (the
projection wave that consumes the index once), L1/L5/L6 land in **W3**. The L9
re-profile is keyed to **post-W1** (not post-W1/W3 — the alloc floor falls at W1,
unmasking the recognition-control antecedent; `SPEC.md:666-672,690,842`). The
`regen --check` 9/9 generated-cleanliness gate is keyed to the **W5 close**
(`SPEC.md:743,752`), not W0. Phase caps are dispatch-hard-cap 20/15/30 per wave.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

The wave order is forced by three hard topological constraints, each cited to a
locked source. It is not free.

**Constraint A — substrate before NEON (the structural-index dependency).** The
NEON gate in SYNTHESIS.md §0.1 states verbatim: *"NEON is gated behind tape
activation — there is no structural index to pre-scan into until the tape decodes
CSS."* The L1 classifier (`select_classifier`, `bbnf-simd/src/dispatch.rs:42`)
produces a `Vec<u32>` structural index whose binding carry-forward condition
(HARDENING-S-P2-V3 §6.1 / `SPEC.md:837`) is *index == tape-offsets identity* — the
produced `Vec<u32>` IS the tape's `offsets`. There are no tape offsets to BE until
L2's `push_plain_offset` (`runtime/src/tape/assembler.rs:71`) is the live CSS sink.
Therefore L1/L4/L5/L6/L7 (the NEON family + the tokenize-once / one-shot-reserve
consumers) cannot land until the tape substrate is live. The substrate waves (W1
PRUNE/tape, W2 projection) precede the NEON wave (W3). A NEON-first ordering would
force the classifier to produce a parallel index retained alongside a fact-stream
parse — exactly the REDRESS-53 collapse the §6.1 identity condition forbids.

**Constraint B — re-profile before the conditional lever (L9 admission gate).**
L9 (commit-by-construction Alt-mode, HARDENING-S-P2-V3 §3 L9 / §6 L9-gate /
`SPEC.md:842`) carries a HARD blocking S-P1-re-confirm obligation: it admits as
active ONLY if a **post-W1** (post-tape) typed-`Tape`/`ValueRef` re-profile at N≥50
surfaces the recognition-control loop (P1-E §3.3 28.87% `emit_full_parse`/
`parse_stylesheet`, un-masked by the retired alloc floor) or a speculative-rollback
leaf as top-N self-time. P1-E measured ZERO speculative checkpoint/rollback
self-time on either benched plane; the antecedent the L9 gate keys on is unmasked by
the **retired alloc floor (which falls at W1)**, NOT by the W3 scan collapse —
therefore the re-profile is keyed to post-W1, not post-W3 (`SPEC.md:666-672,690`; the
W1 substrate close, not the W3 NEON close, exposes the recognition-control plane).
That plane does not exist until the W1 substrate wave closes. Therefore L9 cannot be
sequenced into a behaviour wave ahead of the W1-close re-profile; its wave (W4) is
CONDITIONAL and its entry gate IS that re-profile. SYNTHESIS.md §0.4 also pre-blocks
any speculative-checkpoint re-entry without the re-profile evidence.

**Constraint C — telemetry lock before any speed claim (build-infra-first).** Per
the `build-infra-first` discipline and PASS-3 §8.3 (*"W0 is always baseline +
telemetry"*), the N≥50 cold harness + same-run full-CSSOM lightningcss comparator
(SYNTHESIS.md §0.1 Telemetry-honesty gate; the W6 `W6_SAMPLE_COUNT=1` single-sample
+ fact-stream `assert_lightningcss_strict_equality` comparator is retired) must
exist and `gate-json` must reject sub-threshold rows BEFORE any behaviour wave can
make a >SOTA claim. The §0.5 per-corpus endpoints are all UNMEASURED-PENDING — no
behaviour-wave exit gate may key on a per-corpus lightningcss endpoint until W0's
harness emits the per-corpus split. W0 produces the `SK-V17-open` baseline (the
strict anchor every later exit gate deltas against, per CH1).

These three constraints, applied over the four-lever stack (SYNTHESIS.md Section 3:
lever 1 kill fact-stream + lever 2 alloc-removal, lever 3 NEON index, lever 4
commit-by-construction) and the PRUNE-before-rebuild discipline (the fact-stream +
W5C array are DELETED before the tape/projection is rebuilt), collapse the L1-L9
pool into the SPEC **six-wave** topological order W0…W5 (well under the §3Z ≤12
skinny-bracket ceiling and the P3-A ≤8 active-shortlist ceiling):

- **W0** = infra (baseline + telemetry lock).
- **W1** = PRUNE + levers 1+2 substrate: DELETE fact-stream + W5C array, activate
  the tape (L2 `push_plain_offset`, L7 one-shot reserve gated behind the tape,
  L3-minimal cursor read sufficient to re-prove equality).
- **W2** = rebuild lever 2's projection generality: layout-driven lazy projection
  generator (L3-full rich rider, L8 sparse-flag kind-disambiguation, L4
  tokenize-once consuming the index once).
- **W3** = lever 3: NEON structural index (L1 eq-set classifier, L5
  `comment_body_mask_64`, L6 `bracket_depth_mask_64`), re-profiled on the benched
  tape path first.
- **W4** = lever 4: commit-by-construction spine (L9, CONDITIONAL on the post-W1
  re-profile).
- **W5** = close (clean regen 9/9, Lock-14 audit, corpus close, Alpha feedback).

Note on L7 placement (CH4-3 fix). L7 (one-shot SIMD reserve, `CapacityPlan::OneShotSimd`)
sizes the `offsets` vector from the structural scan count
(`scan_structurals(src).positions().len()+8`, `runtime/src/grammars/json/scan.rs:51`
`CapacityPlan::OneShotSimd`). Its same-wave
consumer is L2's tape (HARDENING-S-P2-V3 §3 L7), and L2 is the W1 substrate.
Therefore L7 lands in **W1** with L2 (`SPEC.md:396,446`): in W1 it sizes `offsets`
from a conservative byte-proportional cold bound, never a per-corpus literal,
because the W3 NEON scan count does not exist yet; when W3 lands the L1 NEON count,
W3 supplies that count as L7's sizing input (`SPEC.md:446-448` task: *"L7 sizes
`offsets` from the W3 scan count in one cold reserve … if W3 has not landed the
index, L7 sizes from a conservative byte-proportional bound — never a per-corpus
literal"*). L7's kernel (the
one-shot reserve) and its consumer (the W1 tape `offsets`) both land in W1 — "same-wave
consumer per kernel" honest. The V1 placement of L7 in the NEON wave is RETIRED.

Note on L4 placement. L4 (tokenize-once shared-scan reuse) consumes the structural
index ONCE on the tape (the index IS the tape, no second cursor). Its wave is **W2**
(`SPEC.md:498-499,544` — the projection wave consumes the structural index once; if W3
has not landed the NEON index, L4 reuses the W1 single-walk). L4 is the projection
generator's index consumer, not a NEON primitive; it lands in W2 with the rich rider.

## §2 — Deliverable: the wave sequence W0…W5

### Wave manifest

| Wave | Name | Levers | Candidates landed | Initial dispatch status | Conditional-dispatch status |
|---|---|---|---|---|---|
| **W0** | Baseline Profile + N≥50 Telemetry Lock + lightningcss CSSOM re-baseline | infra | none (telemetry/harness only) | Dispatchable after S-P3 convergence + G-Alpha (auto-pass per SYNTHESIS authority) | unconditional first wave |
| **W1** | PRUNE: retire fact-stream + W5C array → tape activation (substrate) | 1 + 2 | L2, L7, L3-minimal | Conditional on W0 close + first-of-class CHALLENGE | PRUNE-before-rebuild; retires fact-stream + W5C as live planes |
| **W2** | Layout-driven lazy projection generator | 2 (generality) | L3-full, L8, L4 | Conditional on W0/W1 close + first-of-class CHALLENGE | substrate-union rebuild; the generator walks `BackendRule`, JSON+CSS riders |
| **W3** | NEON structural index (re-profiled) | 3 | L1, L5, L6 | Conditional on W0/W1/W2 close + first-of-class CHALLENGE | NEON gated behind W1 tape activation (§0.1 NEON gate); re-profile the benched tape path first |
| **W4** | Commit-by-Construction Alt-mode (CONDITIONAL) | 4 | L9 | **CONDITIONAL on the post-W1 re-profile firing the L9 gate** | dispatches ONLY if the post-W1 typed-tape N≥50 re-profile surfaces the recognition-control loop or a rollback leaf top-N (§6 L9-gate); else NOT dispatched, recorded not-needed |
| **W5** | Close, clean regen, Lock-14 audit, Alpha feedback | — | none (reconciliation) | Conditional on W0-W4 dispositions | close-honesty + clean regen 9/9 + REDRESS reconciliation |

Wave count = 6 (W0-W5) ≤ 12 (§3Z). Active shortlist = 8 (L1,L2,L3,L4,L5,L6,L7,L8);
L9 conditional (not active until its re-profile gate fires). No candidate is
orphaned across waves (every primitive's same-wave consumer is named, §2.1 per-wave
below). Wave-to-candidate placement is identical to `SPEC.md` Sections 4-8 and
P3-C §1.3.

### W0 — Baseline Profile + N≥50 Telemetry Lock (infra)

- **Levers:** none (build-infra-first; no production behaviour LOC).
- **Owner-path family:** `skinny/crates/bbnf-bench/` (the `css_canon_bench.rs` N≥50
  cold harness + the full-CSSOM lightningcss comparator wiring, retiring
  `assert_lightningcss_strict_equality`-against-fact-stream, `nonjson_css_l4.rs:776`);
  `skinny/xtask/src/` (the `gate-json --skv17-css-sota-report` consumer);
  `skinny/RESULTS.md`; `restart/skinny/tranches/sk-v17/research/wave-0-*.md`;
  `skinny/REDRESS.md` only on reject.
- **Candidates landed:** none. W0 lands NO L-candidate and NO generated change — it
  is the measurement substrate. (Per PASS-3 §8.3, W0 is always baseline + telemetry,
  behaviour waves conditional on close; `SPEC.md:264,375` — W0 lands no parser/scanner/
  SIMD/codegen behaviour or generated parser output change.)
- **Entry gate:** S-P3 converged; G-Alpha auto-passes (SYNTHESIS authority line);
  `skinny/RESULTS.md` is the SK-V16 close baseline at HEAD; the W0 plan names the
  `SK-V17-open` capture method, the N≥50 cold sampling method, the same-run
  full-CSSOM lightningcss build, and a no-behaviour-change proof (W0 changes no
  parser/generated LOC).
- **Conditional-dispatch status:** unconditional first wave.
- **Hard cap:** research 20 min / plan 15 min / redress 30 min (per dispatch-hard-cap
  default; W0 is infra, not first-of-class behaviour). ≤300 harness/gate/report/test
  LOC, 0 behaviour LOC (`SPEC.md:264`).
- **Same-wave consumer:** `gate-json` consumes every emitted telemetry field in the
  same wave (Section 2 schema: `css_sample_count≥50`, `css_sample_statistic==median`,
  `css_sample_mode==cold`, `css_comparator_plane==full-cssom`); an
  emitted-but-unconsumed field fails the wave (PASS-3 §8.2 producer-only artefact).
- **First-touch fold:** apply the HARDENING-S-P2-V3 §5 R1 cosmetic fix on first
  touch of `p2a`/`p2c` if those artefacts are touched in W0 research (they are not
  expected to be; the fold is carried forward to whichever wave first touches them —
  W0 does not touch p2a/p2c).

### W1 — PRUNE: retire fact-stream + W5C array → tape activation (levers 1 + 2; substrate)

- **Levers:** 1 (kill fact-stream String serialization) + 2 (alloc-removal via O(1)
  tape checkpoint). PRUNE-before-rebuild: the fact-stream String plane and the
  hand-coded W5C routing array are DELETED before CSS routes into the existing tape;
  no parallel tape path beside the String.
- **Owner-path family (reconciled to `SPEC.md:403-422`):**
  `skinny/crates/codegen/src/lib.rs` (DELETE `W5C_REQUEST_FACT_PROFILES` `:336`,
  consumed `:567,:611`, selected `:299`; the generator-output-string assertions that
  `.contains("emit_fact_stream")` at `:581,1001,1035` migrated to assert the
  tape-emitting generator output or deleted);
  `skinny/crates/codegen/src/lower/{tape_plan,offset_tape,event_tape,eager_tape}.rs`
  (the routing derived from `BackendRule` shape, preserved as DATA);
  `skinny/crates/codegen/src/runtime_generator.rs:621,666,694` (route CSS off
  `RuntimeEmitterKind::RequestFacts`);
  `skinny/crates/runtime/src/grammars/css_l4_*/parser.rs:6` (the SEVEN live
  `generated::emit_fact_stream(input)` consumers re-pointed to the tape plane);
  `skinny/crates/runtime/src/lib.rs:76,91,108,126,143,162,434` (the SEVEN
  `css_l4_*_emit_fact_stream` round-trip test consumers migrated/deleted — no dangling
  round-trip assertion survives); `skinny/xtask/src/regen_css.rs:45,63,81,99,117,135,153`
  (the seven `RequestFactsProfile` literals flipped off the fact stream, regen fn
  `:164`); `skinny/crates/runtime/src/tape/assembler.rs:42,71,89` (`TapeBuilder`,
  `push_plain_offset`, `reserve_offsets_cold` — USED, not modified to add a second
  tape); `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` (fresh regen,
  named + diff-audited); `skinny/crates/bbnf-bench/`, `skinny/RESULTS.md`;
  `skinny/REDRESS.md` if rejected.
- **Candidates landed:** L2 (`push_plain_offset` tape append, retires
  `emit_fact_stream` as the live plane), L7 (one-shot SIMD reserve, gated behind L2's
  tape — sizes `offsets` from a conservative byte-proportional cold bound at W1, from
  the W3 NEON count once W3 lands; never a per-corpus literal), L3-minimal (the
  minimal `ValueRef` cursor read sufficient to re-prove the 8-field equality — L2's
  same-wave consumer; the full rich rider generalizes in W2). L2's same-wave consumer
  is L3-minimal (P2 §L3); they land together or neither (Lock 1).
- **Entry gate:** W0 closed (`SK-V17-open` baseline + N≥50 harness + full-CSSOM
  comparator gate live, `gate-json` rejects sub-threshold rows); JSON 51/51 baseline
  captured as the tape-activation tripwire (§0.1 JSON-guard); W1 plan names the
  seam-flip site (`regen_css.rs:45-153`), the W5C deletion site, the
  `BackendRule`-derived routing DATA, the `offsets.len()` checkpoint / `truncate`
  rollback (no `split_off`, no `Vec<Vec>` arena, no per-leaf eager payload), and the
  revert slice; first-of-class CHALLENGE accepts that W1 is not a renamed REDRESS
  50-55/60-72 route and introduces no second substrate (Lock 1).
- **Conditional-dispatch status:** conditional on W0 close + first-of-class CHALLENGE.
  Substrate-union wave — Lock 1: L2/L3 are the SAME `Tape`/`ValueRef` substrate JSON
  rides; no second tape, no `StructLayout`/`TapeStructBuilder`/`TapeCursor` (§0.4
  No-second-substrate).
- **Hard cap:** research 20 min / plan 15 min / redress 30 min, PLUS the first-of-class
  CHALLENGE 90 min wall (substrate-touching). ≤450 source/test LOC; generated output
  named separately + diff-audited (`SPEC.md:265`).
- **Same-wave consumer:** L3-minimal IS L2's consumer (the cursor read consumes the
  tape L2 builds); the generated CSS retained parser is the production consumer of the
  tape; L7's consumer is L2's `offsets` vector it sizes one-shot. No orphan kernel; no
  dangling `emit_fact_stream` round-trip assertion survives (every consumer at
  `runtime/src/lib.rs:76,91,108,126,143,162,434` + the `lib.rs:581,1001,1035`
  generator-output assertions is migrated/deleted in this commit, `SPEC.md:414-421`).
- **Binding shortlist conditions enforced (HARDENING-S-P2-V3 §6 / `SPEC.md:834-848`):**
  §6.3 L2/L3 routing derived-from-grammar (`W5C_REQUEST_FACT_PROFILES` RETIRED, every
  residual CSS routing entry names its `.bbnf` rule; relocating per-rule branching
  into projection DATA is the Lock-14-phrase-#1 re-entry seam, FORBIDDEN).
- **Post-W1 obligation:** a typed-tape re-profile (N≥50) on the new path is taken
  after W1 close; it determines whether the W4 L9 gate fires (the alloc floor falls
  here, unmasking the recognition-control antecedent; `SPEC.md:616-618,666-672`).
- **First-touch fold:** if W1 research first touches p2a/p2c, apply the
  HARDENING-S-P2-V3 §5 R1 cosmetic fix on that touch; else carried forward.

### W2 — Layout-Driven Lazy Projection Generator (lever 2 generality)

- **Levers:** 2 (the rebuild: the full rich rider generalizes the W1-minimal cursor
  into a layout-driven lazy projection generator, SYNTHESIS.md §0.1 row 3). PRUNE-then-
  rebuild: W1 deleted the fact-stream / W5C; W2 derives projection from the grammar.
- **Owner-path family:** `skinny/crates/codegen/src/grammar_provider.rs` +
  `lower/{tape_plan,offset_tape,event_tape}.rs` (the accessor generator walking the
  `BackendRule` shape, emitting `document/value/view/visitor`); the generated CSS
  `document/value/view/visitor` (named, diff-audited);
  `skinny/crates/runtime/src/tape/{mod.rs,assembler.rs}` (the EXISTING sparse
  `flag_cursors`/`flag_values` pair `:93-113`, `flags_at` `:144-150` — USED for L8,
  NOT widened, NOT a new vector); `skinny/crates/bbnf-bench/`, `skinny/RESULTS.md`;
  `skinny/REDRESS.md` if rejected.
- **Candidates landed:** L3-full (the codegen lazy-view accessor generator emitting
  `document/value/view/visitor` for CSS from ONE `BackendRule`-walking generator,
  isomorphic to JSON's `value_from_ref` `json/value.rs:143`), L8 (sparse-flag
  side-table — L3's kind-disambiguation mechanism, the EXISTING
  `flag_cursors`/`flag_values` pair, NOT a new vector; each bit a `BackendRule`
  branch-tag projection), L4 (tokenize-once shared-scan reuse — consume the structural
  index ONCE; if W3 has not landed the NEON index, L4 reuses the W1 single-walk).
- **Entry gate:** W0/W1 closed (tape activated; the EXACT 8-field equality holds on
  the W1-minimal path); W2 plan names the `BackendRule`-walk recipe (child-position →
  `ValueRef` child, branch tag → meta dispatch, typed leaf → decode by type, rule
  reference → child + recurse), the four generated artefacts, and the eager-tree
  population-parity baseline; first-of-class CHALLENGE accepts that L8 flag bits are
  `BackendRule` branch-tag projections (NOT a hand-curated per-rule catalogue, the
  relocated-`W5C_REQUEST_FACT_PROFILES` overfit → CH2 REJECT-at-wave) and the L1/L4
  index, when consumed, IS the tape's `offsets` (never a parallel retained vector →
  REDRESS-53 → CH5 REJECT-at-wave).
- **Conditional-dispatch status:** conditional on W0/W1 close + first-of-class
  CHALLENGE. Substrate-union rebuild — the generator emits over the EXISTING
  `Tape`/`ValueRef`, no new cursor/builder type (Lock 1, §0.4 No-second-substrate).
  The projection-generality riders exercised are JSON + CSS only
  (`projection_generality_exercise ∈ {json, css_l4}`); `sheets_witness` has no
  `.bbnf`/`BackendRule` shape and CANNOT serve as a projection exercise (Lock 14
  deferred to SK-V18; `SPEC.md:327-330`).
- **Hard cap:** research 20 min / plan 15 min / redress 30 min, PLUS the first-of-class
  CHALLENGE 90 min wall (projection generator is first-of-class). ≤450 source/test LOC
  default; ≤650 only with an accepted pre-redress fit proof (the plan presents a
  per-artefact LOC accounting showing the four generated artefacts + the generator
  walk cannot land under 450 without splitting a grammar; CHALLENGE accepts or the
  450 ceiling binds; `SPEC.md:266`).
- **Same-wave consumer:** the generated CSS projection (`value_from_ref`-isomorphic)
  is the production reader of the W1 tape; L8 flags are read by L3-full in the same
  wave; L4's tokenize-once reuse is consumed by the same projection walk. No orphan
  kernel.
- **Binding shortlist conditions enforced (HARDENING-S-P2-V3 §6 / `SPEC.md:834-848`):**
  §6.2 L8 flag bit = `BackendRule` branch-tag projection (NOT a hand-curated per-rule
  catalogue, else relocated-`W5C` overfit → CH2 REJECT-at-wave); §6.3 the
  `W5C_REQUEST_FACT_PROFILES` RETIREMENT is a derivation from grammar, not a relocation
  into projection DATA (Lock-14-phrase-#1, FORBIDDEN); the JSON `value_from_ref` rider
  re-emits BYTE-EQUAL through the new generator (if the JSON rider's behaviour changes,
  W2 FAILS — the generic generator must not be a CSS-only generator leaving JSON's
  hand-written path untouched; this is the R-CH2-1 / CH2 load-bearing check, the
  generic-named CSS generator failure mode, `SPEC.md` W2 task 1 / exit gate).
- **First-touch fold:** if W2 research first touches p2a/p2c, apply the R1 cosmetic
  fix; else carried forward.

### W3 — NEON Structural Index (lever 3; hot-leaf union; re-profiled)

- **Levers:** 3 (NEON structural pre-scan). NEON-gated behind tape activation
  (§0.1 NEON gate): the wave RE-PROFILES the benched tape path first; it does NOT
  inherit the core-tree `find_component_delim ~56%` figure (`SPEC.md:592-593,616`).
- **Owner-path family:** `skinny/crates/bbnf-simd/src/dispatch.rs:42,101`
  (`select_classifier(alphabet:&'static [u8;64])` the single neutrality entry,
  `lo6_table_admissible`); `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33`
  (the eq-set fan — NOT the lo6 `classify_tbl4` table, which collides `;{`→slot-59
  under `& 0x3f` and runs scalar today); net-new `aarch64/comment_body_mask_64.rs`
  (L5), net-new `aarch64/bracket_depth_mask_64.rs` (L6); the scalar twins under
  `skinny/crates/bbnf-simd/src/scalar/` (`byte_class_from_eq_set_64.rs` PRESENT;
  net-new `scalar/comment_body_mask_64.rs`, `scalar/bracket_depth_mask_64.rs` with the
  p2e §2 verbatim sketch); `skinny/crates/bbnf-simd/tests/`
  (`checkasm_byte_class_from_eq_set_64.rs` PRESENT; net-new
  `checkasm_comment_body_mask_64.rs`, `checkasm_bracket_depth_mask_64.rs`);
  `skinny/crates/runtime/src/grammars/css_l4_*/` (the tape consumer of the `Vec<u32>`
  index; L7 sizes the W1 `offsets` from the W3 NEON count now that the count exists,
  `runtime/src/grammars/json/scan.rs:51` `CapacityPlan::OneShotSimd`);
  `skinny/crates/bbnf-bench/`, `skinny/RESULTS.md`;
  `skinny/REDRESS.md` if rejected.
- **Candidates landed:** L1 (block-wide byte-class eq-set classifier, the ~69%
  scan-leaf consumer), L5 (`comment_body_mask_64` net-new suppressor, AND-NOTed into
  the L1 index), L6 (`bracket_depth_mask_64` net-new depth-balance mask, scalar-balance
  default body). (L4 tokenize-once and L7 one-shot reserve are NOT new W3 candidates —
  L4 landed in W2, L7 in W1; W3 supplies the L1 NEON count that L7 now sizes from and
  that L4 now reads.)
- **Entry gate:** W0/W1/W2 closed (the typed plane exists; there is a structural index
  to scan INTO, §0.1 NEON gate); **the post-W1 N≥50 re-profile on the benched tape
  path is banked** (so the ~69% scan-leaf antecedent — `find_component_delim`
  56.52-59.24% + `consume_balanced_at` 10.31-11.05%, HARDENING-S-P1-V4 §3.3 / §3 L1 —
  is RE-CONFIRMED on the benched typed-tape path, not inherited from the fact-stream
  plane, per actual-profiling + the §0.1 NEON-gate "RE-PROFILE on the benched tape path
  first" clause; if no scan leaf survives as top-N, W3 does NOT land a NEON kernel and
  the >SOTA gate is evaluated on the W2 plane, `SPEC.md:616-618`); per primitive,
  scalar reference + checkasm differential present BEFORE wiring; first-of-class
  CHALLENGE accepted (W3 is the first NEON-on-CSS wave, primitive + substrate-adjacent).
- **Conditional-dispatch status:** conditional on W0/W1/W2 close + first-of-class
  CHALLENGE accept. NEON-gated behind tape activation. The non-JSON SIMD exercise is
  `css_l4` sharing the `select_classifier(alphabet)` kernel (§0.4 / Section 2
  `simd_non_json_exercise=css_l4`) — Lock 14 dischargeable.
- **Hard cap:** research 20 min / plan 15 min / redress 30 min, PLUS the first-of-class
  CHALLENGE 90 min wall (NEON primitive, scalar-ref + checkasm + same-wave per
  primitive). ≤450 source/test LOC; generated SIMD named separately (`SPEC.md:267`).
- **Same-wave consumer:** the tape's structural decode consumes the `Vec<u32>` index
  in the same commit (scan + tape land together or neither); L5's mask is AND-NOTed
  into L1's index in-wave; L6's balance threads within one `scan_components_to_index`
  call in-wave. Every primitive's consumer is in-wave — NO orphan kernel ships
  (PASS-3 §8.5, the SK-V5 orphan-kernel lesson).
- **Binding shortlist conditions enforced (HARDENING-S-P2-V3 §6 / `SPEC.md:834-848`):**
  §6.1 L1/L4 (G3) index == tape-offsets identity, verbatim — the produced `Vec<u32>`
  IS the tape's `offsets`, carry/depth threads WITHIN a single
  `scan_components_to_index` call, reset per parse (a retained parallel index collapses
  into REDRESS-53 → CH5 REJECT-at-wave); §6.4 L6 (G2) scalar-balance default — the
  SHIPPED body is the scalar running balance with an i32 `depth_carry` threaded within
  one `scan_components_to_index` call, the CTZ-ranges refinement is consumer-only +
  parity-gated + REVISE-back-conditioned, NOT the default (promotion re-opens
  REDRESS-89 → CH3 REVISE); L5 uses the `escape_mask_64` `overflowing_add` carry idiom
  (`lib.rs:188`), NOT PMULL (REDRESS-88).

### W4 — Commit-by-Construction Alt-mode (lever 4; CONDITIONAL)

- **Levers:** 4 (commit-by-construction, remove speculative rollback on the
  structural backbone).
- **Owner-path family:** `skinny/crates/codegen/src/lower/tape_plan.rs` (the generic
  codegen Alt-mode property; rides the SK-V16-banked O(1) `offsets.len()` checkpoint /
  `truncate` rollback on the one offset vector — no `split_off`, no `Vec<Vec>` arena);
  `skinny/crates/runtime/src/grammars/css_l4_*/` (the recognizer spine — the consumer);
  `skinny/crates/bbnf-bench/`, `skinny/RESULTS.md`; `skinny/REDRESS.md` if rejected.
- **Candidates landed:** L9 (commit-by-construction Alt-mode codegen property).
- **Entry gate — THE conditional gate:** W1 closed AND **the post-W1** typed-`Tape`/
  `ValueRef` re-profile at N≥50 has surfaced the recognition-control loop (P1-E §3.3
  28.87% `emit_full_parse`/`parse_stylesheet`/`parse_block_item`, un-masked by the
  retired alloc floor that fell at W1) OR a speculative-rollback leaf as top-N
  self-time. The LOCKED 28.87%+2.45% recognition-control figures are NOT a measured
  rollback antecedent (HARDENING-S-P2-V3 §6 L9-gate / `SPEC.md:842`); the gate is the
  post-W1 re-profile, which does not exist until W1 closes. The re-profile is keyed to
  **post-W1** (the alloc floor unmasks the antecedent at the W1 substrate close, NOT
  at the W3 scan collapse; `SPEC.md:666-672,690`). The W4 plan names the Alt shape
  (pure-lexical keyword-dispatch Alts that deposit nothing structural), the O(1)
  checkpoint/truncate mechanism, and the byte-identical-tape parity proof.
- **Conditional-dispatch status:** **CONDITIONAL — may be NOT-DISPATCHED.** If the
  post-W1 re-profile does NOT surface the recognition-control loop or a rollback leaf
  top-N, W4 does NOT dispatch; L9 is recorded as not-needed in RESULTS/REDRESS (not a
  paper-close, an honest non-dispatch). This is the only conditional behaviour wave
  (`SPEC.md:690-712`).
- **Hard cap:** research 20 min / plan 15 min / redress 30 min (codegen control-flow,
  recognizer-output equality with/without the Alt-mode pass — byte-identical tape).
  ≤300 source/test LOC (`SPEC.md:268`).
- **Same-wave consumer:** the post-W1 CSS recognizer spine (the corpus the re-profile
  identified the recognition-control loop as hot on) — the live consumer on the
  post-W1 profile, not a promised future consumer. If admitted, the spine IS the
  same-wave consumer.

### W5 — Close, Clean Regen, Lock-14 Audit, Alpha Feedback

- **Levers:** none (reconciliation).
- **Owner-path family:** `restart/skinny/tranches/sk-v17/HANDOFF.md`; a W5 close
  artefact under `restart/skinny/tranches/sk-v17/research/`; `skinny/REDRESS.md`
  (close reconciliation); `skinny/RESULTS.md` only for documented-mismatch
  reconciliation without behaviour change; source only if a named Lock-14 cleanup
  (≤150 LOC) is in scope; the 8 dirty generated files (clean regen).
- **Candidates landed:** none.
- **Entry gate:** W0-W4 each have admitted/rejected/routed status; their
  REDRESS/RESULTS/HANDOFF updates are present (W4 admitted or recorded not-dispatched).
- **Conditional-dispatch status:** conditional on W0-W4 dispositions.
- **Hard cap:** research 20 min / plan 15 min / redress 30 min (docs/reconciliation +
  clean regen; no source revert by default). 0 source LOC default; ≤150 named Lock-14
  cleanup LOC (`SPEC.md:269`).
- **Same-wave consumer:** the close checklist + the `cargo xtask regen --check` 9/9
  gate + document reconciliation (RESULTS ≡ REDRESS ≡ HANDOFF ≡ SPEC agree at close).
- **Close gate (the W5-keyed clean-regen, moved here from the V1 W4):**
  `cargo xtask regen --check` 9/9 exit 0 — the 8 git-dirty generated files cleanly
  regenerated as fresh generator output, never hand-patched (Lock 6/14; §0.1
  Generated-state gate; `SPEC.md:108,743,752`). W0 lands no generated change, so this gate
  is keyed to the W5 close, not W0.

### Topological invariant summary

```
W0 (telemetry lock) ─► W1 (PRUNE/tape: L2 L7 L3-min) ─► W2 (projection: L3-full L8 L4) ─► W3 (NEON: L1 L5 L6)
                                  │                                                              │
                          re-profile @ W1 close ──────────────────────────────────────────────┴─► W4? (L9, conditional) ─► W5 (close, regen 9/9)
```

- Substrate (W1 PRUNE/tape, W2 projection) strictly precedes NEON (W3): there is no
  structural index to scan into until the tape decodes CSS (§0.1 NEON gate). [Constraint A]
- The conditional lever (W4/L9) is gated on the **post-W1** re-profile (the alloc floor
  unmasks the antecedent at W1 close), not sequenced ahead of it. [Constraint B]
- Telemetry (W0) strictly precedes every speed claim. [Constraint C]
- L7 lands in W1 (its kernel + consumer = the W1 tape `offsets`); L4 lands in W2 (the
  projection index consumer); L1/L5/L6 land in W3 (the NEON family). [CH4-3]
- Guard rows before risk rows: JSON 51/51 captured in W0 and re-asserted as the
  tape-activation tripwire at every wave close (§0.1 JSON-guard); the regular-corpus
  >SOTA risk row (animate/bootstrap, §0.5) is gated only after the substrate +
  projection + NEON levers land (W3 close, `SPEC.md:637-642`).

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

Each wave's measurable exit gate is the responsibility of P3-C; this section binds
the corpus-row anchors the sequence requires so P3-C's gates are well-typed. All
per-corpus lightningcss endpoints are UNMEASURED-PENDING until W0 emits the
per-corpus split (SYNTHESIS.md §0.5) — therefore no exit gate names a hard Mbps
number ahead of W0; gates are expressed as `> same-run lightningcss full-CSSOM
median at N≥50` (a ratio gate, not a fixed number), per §0.5. The single >SOTA
close gate is keyed to **W3** across all four artefacts (this artefact, SPEC §6,
P3-C §2.3, P3-F).

- **W0 exit (measurable):** `skinny/RESULTS.md` carries the `SK-V17-open` baseline
  with, per benched corpus `{bootstrap, tailwindcss, material-components-web, animate}`
  (`css_l4_corpus.rs:22-54`): `css_sample_count≥50`, `css_sample_statistic==median`,
  `css_sample_mode==cold`, `css_lightningcss_full_cssom_median_mbps` (same-run, plane
  `full-cssom`), `css_track1_typed_median_mbps` (current fact-stream/tape-pending
  plane). `gate-json` rejects any CSS row missing those or carrying a single-tuple
  broadcast (`sample_count==1` or one tuple across rows — the W8R tripwire). NO Mbps
  lift claimed — W0 is the measurement floor.
- **W1 exit (measurable, equality-before-speed):** `css_typed_summary_equal==true`
  EXACT 8-field (`rules=10136, style=9561, sel=9561, decls=20043`, `track1_errors=0`,
  `cssparser_errors=0`, 4/4 corpora) re-proven on the NEW typed-tape path (§0.1
  CSS-typed-equality gate); `tape_activated==true` (PROOF: `TapeBuilder|ValueRef|
  PayloadArena|crate::tape` grep over `grammars/css_l4_*/` returns NON-ZERO +
  `PayloadArena` write/alloc counters confirm tape emission, NOT a `crates/core/`
  grep — wrong-tree dishonesty REJECTed); `w5c_profile_array_retired==true` (grep
  `W5C_REQUEST_FACT_PROFILES` over `skinny/crates/` returns ZERO; no dangling
  `emit_fact_stream` round-trip assertion); `css_rich_ast_preserved==true`. JSON 51/51
  maintain. **NO >SOTA claim and NO speed-admission threshold at W1** — equality +
  activation only (`SPEC.md:475` "NO speed admission this wave"; a fact-stream-improvement
  threshold at W1 is a DIAGNOSTIC sizing signal only, never a W1 admission gate, per
  P3-C §3 W1 closes on substrate truth; a >SOTA gate at W1 would be a paper-close, CH6,
  since the NEON lever has not landed).
- **W2 exit (measurable, generality-before-speed):** `lazy_view_generated==true` (the
  generator emits `document/value/view/visitor` over `BackendRule`);
  `css_rich_ast_preserved==true` (value-plane population parity vs the eager-tree
  baseline, NOT flattened, NOT eager — no per-leaf `Box::new`, `PayloadArena` the
  bounded escape hatch); `projection_generality_exercise∈{json,css_l4}` with **the
  JSON `value_from_ref` rider re-emitting BYTE-EQUAL through the new generator** (if
  the JSON rider changes, W2 FAILS — the R-CH2-1 generic-generator check);
  `W5C_REQUEST_FACT_PROFILES` grep empty; `css_typed_summary_equal==true` re-proven;
  per-corpus typed-median Mbps no worse than -2.0% vs the W1 typed-tape baseline. JSON
  51/51 maintain. NO >SOTA claim at W2 (refactor wave, not a speedup wave).
- **W3 exit (THE >SOTA gate, measurable):** on ≥1 regular corpus (animate OR bootstrap,
  §0.5 tranche-level criterion), `css_track1_typed_median_mbps >
  css_lightningcss_full_cssom_median_mbps` at N≥50 median (`delta_vs_lightningcss >
  1.0x`), with `css_typed_summary_equal==true` re-proven, `css_rich_ast_preserved
  ==true`, JSON 51/51 maintain, `native_simd_status∈{parity-pass,checkasm-pass}`,
  `simd_non_json_exercise==css_l4`. tailwindcss benched cold N≥50 — admit if
  `> lightningcss`, else REPORT honest residual gap + hot leaf in REDRESS (§0.1
  Honest-tailwind gate; NOT tranche-blocking provided ≥1 regular corpus crosses).
  material-components-web reports per-corpus median delta (integration check).
- **W4 exit (conditional, measurable):** IF dispatched, byte-identical tape with/
  without the Alt-mode pass (recognizer-output equality) AND a measured lift (≥ +5%
  N≥50 vs the W3 plane on the gated corpus the re-profile identified; below +5%
  disposes L9 as not-warranted, per P3-C §2.4 / SPEC §7 exit gate). IF not dispatched,
  the not-dispatched-on-evidence record IS the close (honest, not a paper-close).
- **W5 exit (measurable):** `cargo xtask regen --check` 9/9 exit 0
  (`dirty_generated_state==clean`); ≥1 regular corpus crosses re-confirmed at close;
  tailwind admit-or-honest-REDRESS, NO corpus-average substitution; Lock-14 audit
  clean; RESULTS ≡ REDRESS ≡ HANDOFF ≡ SPEC agree.

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

P3-E owns the full ledger; the sequence-relevant pre-blocks per wave (so no wave's
ordering silently re-opens a route), reconciled to the SPEC §9 per-wave table:

- **W0 (telemetry) must NOT re-open:** the 24-row broadcast (one timing tuple across
  N rows, RESULTS 112-135; §0.4 / the W8R tripwire — W0 RETIRES it, never re-emits);
  `W6_SAMPLE_COUNT=1` single-sample harness (retired); fact-stream comparator
  (`assert_lightningcss_strict_equality` against a fact stream → full-CSSOM CSSOM
  build); fixture/FNV capacity constants; phantom `normalize`. (The dirty-generated
  --check 9/9 gate is NOT a W0 pre-block — W0 lands no generated change; that gate is
  keyed to the W5 close, `SPEC.md:375,743`.)
- **W1 (PRUNE/substrate) must NOT re-open:** AZ-IV eager-value-tree materialization
  (the 118x regression — materialization stays lazy-by-default, no per-leaf `Box::new`,
  no eager value tree; §0.4); StructRegistry / `Arena<G>` / `Builder<G>` hot-path
  indirection (28-65x / 983x / 10583x WATCHDOG; the `TapeBuilder` is a single
  non-generic sink; §0.4); CSS fact-stream String as a live admission plane
  (`emit_fact_stream`, retired not kept; §0.4) — and every `emit_fact_stream`
  round-trip ASSERTION (`lib.rs:581,597,1001,1035,1109,1113`) migrated/deleted in the
  W1 commit (no dangling consumer survives the retirement, per same-wave-consumer
  non-negotiable); `W5C_REQUEST_FACT_PROFILES` extended rather than retired, or its
  per-rule branching relocated into projection DATA (the Lock-14-phrase-#1 re-entry
  seam; §0.4 / §6.3); No-second-substrate (skinny
  `StructLayout`/`TapeStructBuilder`/`TapeCursor` → Lock 1 REJECT; §0.4);
  `split_off`/`Vec<Vec>` arena; REDRESS 50-55 (offset-tape family), 60-72.
- **W2 (projection) must NOT re-open:** eager materialization (no per-leaf `Box::new`,
  the eager tree stays the parity baseline, never the live plane); L8 flag as a
  hand-curated per-rule catalogue (the relocated-`W5C` overfit; §6.2); the L1/L4 index
  retained as a parallel vector (REDRESS-53); retained cursor / aux density / sidecar
  event vector / a second substrate; relocating per-rule branching into projection
  DATA; the L8 sparse-flag → sidecar / hand-curated-catalogue route (the existing
  `flag_cursors`/`flag_values` pair is USED, never widened into a sidecar — re-keyed
  to W2 per the SPEC L8-in-W2 placement, `SPEC.md:498,515-516,542-543,571`; this is the V1
  mis-filing under W1 corrected); a CSS-only generator leaving JSON's hand-written
  path untouched (the R-CH2-1 generic-named-CSS-generator failure mode — the JSON
  rider MUST re-emit byte-equal).
- **W3 (NEON) must NOT re-open:** the 24-row broadcast measurement; lo6 `classify_tbl4`
  reuse on the CSS alphabet (the `;{`→slot-59 `& 0x3f` collision + table-NEON scalar
  passthrough — the CSS answer is L1's eq-set fan, §4 route-eliminated); PMULL for the
  comment mask (REDRESS-88 — L5 uses the `escape_mask_64` `overflowing_add` carry
  idiom, `lib.rs:188`); CTZ-ranges as the L6 default body (REDRESS-89 — scalar balance
  is the default, §6.4); x86/AVX/SVE (§0.4 aarch64-only); the orphan udot 4-digit
  decode (CF-4a/C5/C-B3/G4) + net-new i8mm digit kernel (CF-4b/C6) — barred from the
  active shortlist, no benched CSS antecedent, P1-E §4.4(a) categorical
  (HARDENING-S-P2-V3 §4); FNV/hex kernel (retires wholesale with the String, never a
  primitive; §4); a retained index vector parallel to the tape (REDRESS-53);
  cross-call classifier-state retention.
- **W4 (conditional spine) must NOT re-open:** a speculative checkpoint re-entry
  without the post-W1 re-profile evidence (§6 L9-gate; the LOCKED recognition-control
  figures are NOT a rollback antecedent); `split_off` / `Vec<Vec>` arena rollback
  (rides O(1) `offsets.len()` marker + `truncate` on the one offset vector); a
  non-byte-identical tape (a behaviour change masquerading as a control-flow
  optimization).
- **W5 (close) must NOT re-open:** full-codegen close claims while dirty generated CSS
  files remain (§0.4); deleting legacy CSS generated/runtime shims before replacement
  proof lands (§0.4); brace-counter proof as CSS admission; lightningcss comparison
  before Track 1 emits comparable CSSOM/value output (§0.4 — already satisfied by W1
  equality gate); paper close ("wired"/"integrated" without a bench-row threshold);
  corpus-average claim substituting for per-corpus medians (honest-tailwind, §0.1);
  the full-codegen close while regen is dirty (the `regen --check` 9/9 gate, §0.1
  Generated-state gate, keyed HERE not W0).
- **Every wave:** D6 second substrate (REJECT-on-sight, §4); retained sidecars /
  sidecar event vectors / retained cursor / aux density tables / parallel source
  passes / public `UnionTape` / sixth `BackendShape` / cross-call classifier-state
  retention (§0.4 hidden-coupling pre-blocks; a SIMD mask stream is a TRANSIENT
  producer, not a retained sidecar — if structural offsets are retained, the
  projection IS the tape, Lock 1 `LOCKS.md:75`).

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (§2 P3-B scope, §3 CH1-CH6,
  §8.3 W0-always-baseline, §8.5 same-wave-consumer, §3Z ≤12 wave / ≤8 shortlist).
- `restart/skinny/tranches/sk-v17/SPEC.md` (THE binding wave manifest: Section 2
  W0-W5 manifest + caps `:260-271`, Section 2.1 generality/Lock-14 `:327-330`,
  Sections 3-8 the per-wave owner-path/entry-gate/exit-gate/revert structure I
  sequence to — W0 `:337`, W1 PRUNE `:390`, W2 projection `:494`, W3 NEON `:583`,
  W4 L9 conditional `:663`, W5 close `:725`; Section 9 the route ledger `:777` +
  binding conditions `:834-848`; Section 10 dispatch scope `:851`; the post-W1 L9
  re-profile `:666-672,690,842`; the W5 regen 9/9 gate `:108,743,752`).
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` (§0.1 close-condition gates, §0.4
  pre-blocks + generality clause, §0.5 per-corpus close / UNMEASURED-PENDING,
  §0.6 strict comparator, Section 2 telemetry, Section 3 four-lever stack).
- `restart/skinny/tranches/sk-v17/research/p3/p3a-candidate-shortlist.md` (the
  ≤8 active shortlist L1-L8 + L9 conditional this sequence orders).
- `restart/skinny/tranches/sk-v17/research/p3/p3c-falsifiability-gates.md` (§1.3
  wave→candidate map, §2.0-2.5 per-wave gates this sequence's exit-gate anchors feed;
  reconciled six-wave numbering, >SOTA gate at W3).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  (§3 LOCKED pool L1-L9, §4 REJECTed set, §5 R1 first-touch fold, §6 binding
  shortlist conditions 1-4 + L9 post-W1 re-profile gate, commit f87ee713a).
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  (§3.3 hot-leaf table: `find_component_delim` 56.52-59.24%, `consume_balanced_at`
  10.31-11.05%, `emit_fact_stream` 24.59-25.01%, recognition-control 28.87%,
  block dispatch 2.45%; P1-E §4.4(a) zero digit leaf; commit 0ae1caa52).
- `restart/skinny/tranches/sk-v8/SPEC.md` (the SPEC wave-manifest shape S-P3 mirrors:
  §2 manifest + caps + reruns, §2.1 generality gate, per-wave owner-path / entry-gate
  / exit-gate / same-wave-consumer / revert structure).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (the per-wave research→plan→
  redress contract each wave conforms to; first-of-class CHALLENGE interposition).
- Owner-path verification (master HEAD f87ee713a):
  `skinny/crates/bbnf-simd/src/dispatch.rs:42,101` (`select_classifier`/
  `lo6_table_admissible`); `skinny/crates/runtime/src/tape/assembler.rs:42,71,89`
  (`TapeBuilder`/`push_plain_offset`/`reserve_offsets_cold`);
  `skinny/crates/codegen/src/lib.rs:299,336,567,581,597,611,1001,1035,1109,1113`
  (`W5C_REQUEST_FACT_PROFILES` + `emit_fact_stream` round-trip consumers);
  `skinny/xtask/src/regen_css.rs:45,63,81,99,117,135,153,164` (the seven
  `RequestFacts` literals + regen fn).
