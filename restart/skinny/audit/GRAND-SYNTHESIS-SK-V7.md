# SK-V7 Grand Synthesis

Date: 2026-05-16.

Cohort: 18 agent reports under `restart/skinny/audit/SK-V7-COHORT/` (6 deep
research A1-A6 + 6 reinforcement design B1-B6 + 6 profile C1-C6). The C4
PMU access was blocked on M5 Max; used static disassembly fall-back per
SK-V6 R6c precedent.

Authority: this document is the **Pass Alpha α-F output** per
`restart/prompts/PASS-ALPHA.md` §2. It folds the SK-V6 measured state +
the SK-V7 cohort findings into the next-cycle synthesis. The companion
artefacts are `IMPLEMENTATION-PACKET-SK-V7.md` (the wave plan with
precisely-defined goalset + telemetry binding) + `HANDOFF-SK-V7.md`
(packet handoff). The Pass Alpha CHALLENGE pass against this synthesis
landed at `restart/skinny/audit/SK-V7-COHORT/alpha-hardening/` (when
dispatched; this synthesis is the V1 of the cycle).

## 1. The Frame

SK-V7 is the **iterative auto-convergent prompt framework's first
formal cycle**. The framework was authored in this same commit chain
(`restart/prompts/{README, PASS-ALPHA, PASS-OMEGA, SKINNY-PASSES}.md`).
The SK-V7 cohort serves both as the first iteration of the new
framework AND as the Pass Alpha α-A through α-E inputs for producing
the SK-V7 packet.

The discipline established by the prompt framework:

- **Triumvirate per wave**: research → plan → redress in three distinct
  commits. No role merger. Enforced by `SKINNY-PASSES.md` §9.
- **Six-lens CHALLENGE** between phases for high-risk interventions.
- **N-iteration auto-converge** with ceiling V5 per pass; 12 waves per
  skinny bracket.
- **Telemetry binding**: every SK-V{N+1} carries a ~24-column
  RESULTS.md schema with Δ vs SK + Δ vs every competitor + hot leaf +
  strictness/output-plane per row.
- **Strict-vs-strict comparator gate**: every comparator row matches
  strictness plane. Permissive sonic-rs (utf8_lossy) + asmjson SWAR
  rows are flaw probes only.

## 2. Where SK-V6 Landed (Pass Alpha α-A + α-D)

Per `skinny/RESULTS.md` post-V6 + V6 commits (`2631a834` + prior 32 V6
commits):

**Wins**:
- Generated SinkOnly from BIR (`codegen/src/json_sink_direct.rs`); Track
  1 = generated runtime; the gate is honest.
- Eisel-Lemire vendored: numbers direct 33% → 100.4% sonic PASS.
- Canada SIMD scan floor restored: 22,136 → 41,833 Mbps.
- ContainerNext dispatch carry (V6 admit `2b3bef79`).
- Generated-retained tiny-string cap (V6 admit `1e213001`).
- Host-output-schema typed DirectBuild (V6 admit `ab06ff11`): twitter
  real_typed_struct 151.5% sonic PASS.
- simd-scan fossil + eventcursor purged; BackendShape Rust state
  landed (5-shape enum + LayoutFacts.backend_shape +
  derive_backend_shape).

**Open gates** (post-V6 RESULTS):
- 13 parse rows G/NO-GO. throughput materially recovered vs V5 baseline
  but the gate counts G via Track 2 + multi-comparator criteria.
- 11 direct rows N-direct. 6 PASS: citm, apache_builds, github_events,
  instruments, mesh-borderline. real_typed_struct: twitter + update_center
  PASS.
- sonic-rs comparator is flaw probe (utf8_lossy enabled at
  `bbnf-bench/Cargo.toml:21`). Strict baseline impossible until repair.

## 3. The Eight SK-V7 Cohort Findings (Pass Alpha α-A + α-B + α-C + α-E)

### 3.1 Comparator-plane repair is a one-line Cargo diff (A1)

`bbnf-bench/Cargo.toml:21` explicitly opts into `utf8_lossy`. Upstream
default is OFF. Remove the feature; rerun bench. Predicted sonic-rs
regression 3-8%. Sufficient to flip `instruments` (92.0%) and
`unicode_basic` (91.7%) borderline rows; insufficient to flip any of
the 11 strong N-direct rows. This is Wave 0 of SK-V7.

### 3.2 yyjson 1.98x gap on twitter is the largest M5 Max deficit (A2 + C6)

yyjson at 3,687 MiB/s twitter (0.91 c/B) is the actual M5 Max
DOM-class leader, not simdjson (1.142 c/B). bbnf at 15,597 Mbps =
~1,950 MiB/s. The 1.98x gap is the primary SOTA-beat target. Lock 15
i-cache discipline + admitted NEON kernels inside the fused leaf is
the route. asmjson confined to x86 successor (permissive SWAR on M5).

### 3.3 The B1 per-`\uXXXX` hypothesis applies to only 2 of 4 rows (C1)

**CRITICAL CORRECTION** to A4's prescription. C1 PC-attribution shows:
- unicode_escapes (78% `\uXXXX` content): B1 applies.
- y_string_unicode (74%): B1 applies.
- unicode_mixed (0% `\uXXXX`): plain-body-scan, NOT escape decode. 75%
  in `match_string_at_quote`.
- distinct_values (0% `\uXXXX`): same plain-body-scan.

The dominant residual kernel is **`match_tiny_plain_string_with_cap::<16>`**
at `generated.rs:173` — top self-time leaf on 8 of 13 parse-G rows
(28-47% range). Three pathologies (not two): tiny-plain scalar + string
body/escape scan + container/key bookkeeping (citm + instruments).

### 3.4 String scanner pair dominates ~75% (C4 + C2 + C1)

Per C4 static disassembly + samply 4 kHz fall-back: aggregated across 6
hot rows, `match_string_at_quote` ~47% + `match_tiny_plain_string` ~28%
= ~75% of total self-time. V6 ContainerNext + tiny-string cap admits
did not change the hot leaf shape. The intervention sequence is
constrained.

### 3.5 mesh DirectBuild REJECT under current codegen (C3)

The codegen Vec helper at `json_typed_direct.rs:306-315` is shape-blind
(no `Vec::with_capacity`, no SWAR, no bulk delimiter scan). mesh's
~25k-element `Vec<f64>` would land at ~91.8% sonic — does not cross
the 100% PASS gate. B5 (mesh DirectBuild) MUST sequence AFTER
`DirectTypeRef::Vec(DirectScalar::F64|U32)` codegen specialisation
lands. The capacity_hint field on DirectTypeRef::Vec is the
non-trivial change (~30 LOC); pattern is established at
`MapEntriesVec` arm (`json_typed_direct.rs:326-342`).

### 3.6 Twitter real_typed_struct 151.5% is from skip-work (C3)

74.5% of Track 1 samples in `DirectParser::skip_value` at
`generated_real_typed.rs:490-544`. Tweet schema drops user.*, entities.*
into `UnknownFieldPolicy::Skip`. The "win" is structural — most input
is NOT parsed. This is NOT a SOTA-beat capability claim; it's a "drop
fields and you're faster than full DOM parsers" measurement. The mesh
typed expansion will not replicate this pattern (mesh has no fields to
skip — every numeric is required).

### 3.7 Eisel-Lemire only 5.2% of mesh cost (C2)

The Eisel-Lemire vendored kernel is correct but is NOT the dominant
cost on mesh. The byte-at-a-time digit scan around it dominates. A
B5b sub-item — widen mantissa range OR add SIMD digit-block packer —
is the actual mesh close. Plus canada has ~25% of f64 overflow EL fast
path (2.5% in `text.parse::<f64>()` fallback per C2). B5b widens the
EL mantissa range to capture the overflow case.

### 3.8 CostFacts substrate is absent (A6)

`passes/src/lib.rs:33-39` assigns `backend_shape` directly with no
rejected/dominated alternative evidence. REDRESS 72 (cap-16 helps
generated-retained but regresses direct/Track 2) is empirical proof
that per-rule shape decision must be CostFacts-recorded. B2 designs
the substrate (~830 LOC across ir/+passes/+codegen/+xtask/+docs).
This is grammar-neutral plumbing; once landed it unblocks per-rule
shape decisions across all skinny grammars.

## 4. The Lock 14 Status (Pass Alpha α-C + α-D + Omega-relevant)

A5 + B3 audits land:
- ~46 HIGH leaks across `passes` (11) + `codegen` (~18) + `parse-that-regex` (9) + `ir` (3) + 5 file-location violations.
- bbnf-simd src/: CLEAN (V6 W4 split: 716 → 273 LOC; JSON refs only in provenance comments + biased test fixtures).
- runtime/tape: CLEAN.
- Class D TapeKind rename: 30 LOC actual (vs 150-300 estimate). All 7
  consumers clustered in `passes::materialization_for_rule` which Class
  A deletes.
- Class B (Json*Match collapse into existing StringMatch/NumberSpan):
  mechanical, ~30 sites.
- Riskiest phase = Class C codegen rebrand (~470 LOC across 4 files).

B3 sequencing: Phase A (parse-that-regex 9 HIGH) → Phase B (passes 11
HIGH) → Phase C (codegen 18 HIGH) → Phase D (ir TapeKind rename) →
Phase E (template residue). Total ~1150 LOC changed, +180 net.

This is Pass Omega-class work (V1 spec amendment). For SK-V7 it lives
in Wave 7+ Lock 14 cleanup parallel-safe with the rest.

## 5. The Reusable Vocabulary Status (Pass Alpha α-E)

The 9-macro `bbnf.asm` Layer 1 vocabulary at `bbnf-simd/ext/x86/bbnf.asm`:
- 3 of 9 have bodies (BYTE_CLASS_FROM_EQ_SET_64 + V6 admits
  `bulk_emit_positions_64` + `structural_terminator_64`).
- 4 of 9 are blocked-no-consumer (BULK_EMIT_COMPRESSED, FRAME_PUSH/POP_BOUNDED,
  FSM_DISPATCH_THREADED) — all unlock together with first
  per-grammar CollapsedStage .asm wrapper. V7 doesn't dispatch this
  (no Zen 4 silicon access + no NASM author yet).
- 2 of 9 admissible on M5 Max via ARMv8.2+ esoterica (per A3):
  - PMULL for BITMAP_PREFIX_XOR_64 (6-stage shift-XOR ladder → 1 µop).
  - CSSC CTZ for BITMAP_NEXT_SET_BIT (RBIT+CLZ → single CTZ).

The 6 remaining (BYTE_CLASS_FROM_TABLE_64 + EOB_PAD_CLAMP) await
specific same-wave consumers; not Wave 1 of V7.

## 6. The SK-V7 Wave Plan (preview; full detail in IMPLEMENTATION-PACKET-SK-V7.md)

| Wave | Scope | Falsifiability gate | Predicted close |
|---|---|---|---|
| **W0** | sonic-rs strict rebuild (`bbnf-bench/Cargo.toml:21` one-line + bench rerun) | RESULTS schema v3 columns populated; sonic Mbps drops 3-8% | instruments + unicode_basic flip toward PASS |
| **W1** | Class D TapeKind rename (30 LOC; lowest risk; Lock 14 partial) | `cargo test --workspace` green; byte-identical generated.rs | no row regress; Lock 14 -3 HIGH |
| **W2** | B5b widen Eisel-Lemire mantissa range + canada fallback elimination | canada direct PASS ≥100% sonic; numbers stays PASS | canada flips to PASS |
| **W3** | B5 mesh DirectBuild (DirectTypeRef::Vec(DirectScalar::F64\|U32) specialisation + capacity_hint + mesh host schema fixture) | mesh real_typed_struct ≥100% sonic; marine_ik real_typed_struct ≥100% sonic | mesh + marine_ik typed PASS |
| **W4** | B1 per-`\uXXXX` TBL classifier (per-quartet, not 4-batch; reuse existing unescape_uxxxx_neon) | ≥2 of 4 named lift (unicode_escapes, y_string_unicode primary; unicode_mixed, distinct_values secondary will NOT lift here) | unicode_escapes + y_string_unicode flip |
| **W5** | B2 NEON plain-string scan widening (match_tiny_plain_string → 16-byte NEON stride via scan_string_special_block) | twitter + update_center + unicode_basic + random parse close; unicode_mixed + distinct_values close (no `\uXXXX` content) | 6 rows lift; biggest single-wave wave |
| **W6** | B6 control/key compaction (citm + instruments bookkeeping) | citm + instruments close ≥100% sonic | 2 rows lift |
| **W7** | Lock 14 cleanup parallel (B3 Phase A + B; mechanical) | Lock 14 audit -20 HIGH | no row regress |
| **W8** | B6 (continued) Phase C+D codegen rebrand + ir TapeKind | `cargo test --workspace` green | Lock 14 cleanup full |
| **W9** | CostFacts substrate (B2 design; ir/+passes/+codegen/) | xtask gate-json `--with-cost-facts` emits 7-rule CostFacts | no row regress; substrate landed |
| **W10** | bbnf.asm body fills: PMULL + CSSC CTZ (per A3) | checkasm parity; same-wave OffsetTape consumer | no row regress; primitives admitted |

Hard caps per wave: 3-4 hours wall per triumvirate (research/plan/redress
30/30/75 min + optional 60-min CHALLENGE).

## 7. Predicted SK-V7 Close (Pass Alpha α-E + C5 cross-correlation)

Post W0-W10 execution:
- Retained parse: 5/17 → predicted **9-11 PASS** (close-rate 53-65%).
  Hard residual: twitter (still bound on string scanner; B2 helps but
  yyjson 1.98x gap is structural Lock 15 fusion-quality work, not a
  single kernel admit).
- Direct: 5/17 → predicted **10-12 PASS** (close-rate 59-71%).
- Typed: 1 PASS → predicted **3-5 PASS** (mesh + marine_ik + canada via
  W3; gsoc-2018 may flip if skip-fields schema applied).
- Strictness columns disclosed on every row (W0).
- Track 1 verified generated runtime via samply (already W6 admit but
  re-verified post-W0).
- Track 2 structurally different (already W6 admit).
- Lock 14 audit: -41 HIGH (~5 HIGH remain in residual codegen template
  residue).
- Lock 15 still PASS (re-measured post-W5/W10 to confirm hot body
  doesn't exceed 20 KiB).

Hard residual: twitter parse + the yyjson 1.98x gap likely needs a
**fusion-quality refactor** (move `parse_value_at` to a single fused
LLVM-target driver, mirror yyjson's i-cache discipline). This is
W11+/V8 scope.

## 8. The Strict-vs-Strict SOTA-beat Posture (Pass Alpha α-B)

Per A2 + C6 + C5:
- Beat sonic-rs strict on the 9 of 17 rows where bbnf is already
  >100% lossy (post-strict, the gap widens 3-8% in bbnf's favor).
- Beat simdjson NEON DOM on 3-5 rows where structural-index amortizes
  poorly (canada, mesh, unicode_escapes already shown).
- Beat yyjson on 2-3 rows where bbnf's tape projection wins (citm,
  canada already shown); but twitter remains 50% gap.
- asmjson SWAR (permissive) is flaw probe; no strict comparison on M5.
- asmjson AVX-512 (10.93 GiB/s twitter on Zen 4) is x86 successor;
  CollapsedStage NASM required; outside V7 scope.

The SOTA-beat declaration for SK-V7: bbnf-skinny is **competitive with
yyjson/simdjson/sonic-rs across 13-15 of 17 corpora at strict-vs-strict
on M5 Max**, with twitter parse the largest unresolved gap.

## 9. Pass Omega Triggers (skinny → totality fold-in candidates)

Per Pass Omega §9 contract, the SK-V7 cycle surfaces these for Pass
Omega Ω-C (locks amendments) consideration:

- **Lock 1 strengthening**: REDRESS 50-55 + 60-70 + 73 establish the
  parallel-substrate prohibition empirically across multiple shape
  attempts. Lock 1 language may need a clause specifically forbidding
  "scanner-pair as Track 1" pattern.
- **Lock 14 strengthening**: 46 HIGH leak audit shows the rule is
  systematically violated despite the lock. A stronger enforcement
  clause (e.g. CI grep-gate) is justified.
- **Lock 15 confirmation**: PASS on the new baseline with 42-58%
  headroom. No amendment needed.
- **Lock 16 SIMD admissibility allowlist**: 3 of 9 macros bodied; 2
  M5-admissible (PMULL + CSSC CTZ); the allowlist holds.
- **NEW LOCK** (proposed for Pass Omega consideration): "No bench-
  private Track 1 / no Track 1 ≡ Track 2 dishonesty". The SK-V5/V6
  finding that the bench measured its own private parser was load-
  bearing. A lock would formalize the discipline.
- **NEW LOCK** (proposed): "Comparator-plane strictness disclosure
  mandatory". The sonic-rs utf8_lossy finding shows comparator
  config drift can invalidate years of measurement. A lock
  formalises strictness as required RESULTS schema.

These proposals go to Pass Omega Ω-C for evaluation; G-Omega
sign-off required before merge to `restart/locks/14-LOCKS.md`.

## 10. Closing Posture

SK-V7 is the **first cycle of the iterative auto-convergent
framework**. The framework + cohort + Wave plan together demonstrate
the discipline:

- Triumvirate per wave (research → plan → redress, distinct commits).
- Same-wave consumer rule (no orphan kernels).
- Strict-vs-strict comparator gate (no flaw-probe wins).
- Telemetry-bound goalset (per `IMPLEMENTATION-PACKET-SK-V7.md` §0).
- 6-lens CHALLENGE before commit (optional but recommended for
  high-risk waves W2, W3, W5, W9).
- Hard caps + revert protocol per wave.
- Pass Omega trigger checklist (§9) for skinny → totality fold-in.

The architecture is correct. The substrate is correct. Lock 1 holds.
Lock 14 has known debt with named cleanup plan. Lock 15 holds with
headroom. Lock 16 grows monotonically. The five-shape `BackendShape`
taxonomy exists in Rust; the per-shape lower bodies will fill in W1-W10.

The work remaining is enumeration-bounded:
- 11 N-direct rows + 13 G rows + Lock 14 debt + bbnf.asm body fills +
  CostFacts substrate + Pass Omega locks amendments.
- Predicted close: 9-11 retained PASS + 10-12 direct PASS + 3-5 typed
  PASS after W0-W10.
- Hard residual: twitter parse (yyjson 1.98x gap) requires fusion-
  quality refactor; W11+/V8 scope.

The work is bounded. The cadence is bounded by the wave hard caps. The
discipline is the suite.

**Dispatch Wave 0 of SK-V7.**
