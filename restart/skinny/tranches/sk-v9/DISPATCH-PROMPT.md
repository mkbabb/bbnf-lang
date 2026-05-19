# SK-V9 P3-F: DISPATCH-PROMPT Draft

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-18.
Scope: the next SK-V9 DISPATCH-PROMPT — the implementation-agent
per-wave dispatch contract that binds to the SK-V9 SPEC wave plan.
Output: this file (a DRAFT; the orchestrator promotes it to
`sk-v9/DISPATCH-PROMPT.md` after S-P3 CHALLENGE converges).

This prompt is composed alongside `skv9-p3-F-spec-draft.md`. Where it
references a falsifiability gate or a pre-blocked route, it references
the SPEC section by number and does not restate the gate — the SPEC
draft is the single source.

§0 footer — V2 fold: integrated P3-A..E; all [INTEGRATE] markers
resolved; unified W1-W5 manifest with W4 sub-waved; 10-outcome enum;
36-field schema; live RESULTS floors.

§0 V3 fold footer — V3 comprehensive integration. The wave manifest,
required-reading source map, cascade-lock prose, CHALLENGE/redress
phase rules, falsifiability-gate facts, pre-blocked-route list, and the
convergence section all rebind to the W4b three-way sub-division
(W4b-1 scalar reference + checkasm harness, W4b-2 fixed-width bodies +
JSON consumer — the row-moving sub-wave PAIRED with W4a, W4b-3
variable-width bindings + codegen). The W3 row carries the HIGH-risk
escalation and the CHALLENGE-gated ≤110-min redress extension.
Arithmetic: `update_center` W3 floor `14369 → 14370`; `gsoc-2018` W4b
no-regression floor `21430 → 21963` (live base 22184); the W10b block
is floored uniformly — `citm_catalog` `28631 → 28630`, `numbers`
`17597 → 17596`. The G-Gate enumerates `G-W4b-1-CODEC-HARNESS` /
`G-W4b-2-CODEC` / `G-W4b-3-CODEC-BINDINGS` in place of `G-W4b-CODEC`.

§0 V4 fold footer — REDRESS 98 supersession. W3 is retired as a
falsified hypothesis, not blocked pending another implementation. REDRESS 96
and REDRESS 97 measured faithful correctness-green union-substrate
implementations and both missed every W3 must-improve row plus every W10b
maintain floor; CHALLENGE V4 rejected class-lane-only as a paper-close. The
old cascade-lock is abrogated for SK-V9. W4 no longer dispatches as a W3 union
consumer; any surviving W4 work must be re-planned against existing
offset-tape/string/unescape call sites with its own scalar reference, checkasm,
same-wave consumer, W10b maintain gate, and REDRESS 98 pre-block. W5 is
superseded by Pass Alpha dispatch for SK-V9 -> SK-V10.

---

# SK-V9 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration
SK-V9. It binds to the SK-V9 packet at
`restart/skinny/tranches/sk-v9/`. Each wave of the SK-V9 SPEC is
executed by one triumvirate per `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.

G-Alpha is closed. W0 telemetry-lock is closed under
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`. The S-P1 rerun converged
six-of-six lenses against the fresh PMU table at
`/tmp/skv9-xctrace-v3/pmu_rows.tsv`; `G-S-P1-RERUN-CONVERGED` is
recorded PASS. S-P2 Research converged; S-P3 distilled the six
interventions into the C1..C8 shortlist and sequenced them W1-W5. W1 and W2
closed; W3 is now retired by REDRESS 98. The only current dispatch authority is
the REDRESS 98 resequence: typed-plane work and existing-substrate W4 work may
advance only after fresh SPEC/Alpha gates are written. A historical W3 or old
cascade-lock entry gate is not dispatch authority.

## Required Reading

Read in order:

1. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
2. `restart/prompts/skinny/PASS-1-PROFILE.md`.
3. `restart/prompts/skinny/PASS-2-RESEARCH.md`.
4. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
5. `restart/skinny/tranches/sk-v9/SYNTHESIS.md`.
6. `restart/skinny/tranches/sk-v9/SPEC.md`.
7. `restart/skinny/tranches/sk-v9/HANDOFF.md`.
8. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.
9. `restart/skinny/tranches/sk-v9/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.
10. The S-P3 synthesis cohort:
    - `research/p3/skv9-p3-A-candidate-shortlist.md`.
    - `research/p3/skv9-p3-B-wave-sequencing.md`.
    - `research/p3/skv9-p3-C-falsifiability-gates.md`.
    - `research/p3/skv9-p3-D-telemetry-schema.md`.
    - `research/p3/skv9-p3-E-preblocked-ledger.md`.
11. The S-P2 report owning the dispatched wave:
    - W1 → `research/p2/skv9-p2-C-apache-citm-admission.md`.
    - W2 → `research/p2/skv9-p2-B-retained-grammar-proof.md`.
    - W3 → retired; read `skinny/REDRESS.md` entries 96-98 and
      `research/skv9-W3-challenge-v4.md` as pre-block evidence.
    - W4a → `research/p2/skv9-p2-D-aarch64-asm-opportunities.md` §4,
      re-scoped to the existing string-scanner call site only.
    - W4b-1 / W4b-2 / W4b-3 → `research/p2/skv9-p2-E-unicode-escape-codec.md`
      (§7.1 eleven-slice table — W4b-1 = S1/S6, W4b-2 = S2/S3/S5/S7/S8/S11,
      W4b-3 = S4/S9/S10) + `skv9-p2-D-aarch64-asm-opportunities.md` §3.
    - W4c → retired with W3 unless a future Alpha/S-P3 contract names a
      non-union consumer.
    - W4d → `research/p2/skv9-p2-D-aarch64-asm-opportunities.md` §4.4,
      re-scoped to any admitted existing-substrate W4a consumer.
12. `restart/skinny/tranches/sk-v9/research/skv9-W0-close.md`.
13. `skinny/RESULTS.md`.
14. `skinny/REDRESS.md`.

## Wave Manifest

| Wave | SPEC section | Title | Shortlist | Dispatch status | Hard cap |
|---|---|---|---|---|---:|
| W0 | Section 3 | SK-V9-open Telemetry-Lock Recovery | — | Closed | — |
| W1 | Section 4 | Apache/CITM Measured Typed-Row Admission | C1 | Dispatchable — independent | ≤90 min |
| W2 | Section 5 | Retained Class/Event Grammar + `ValueRef` Proof | C2 | Conditional on W1 close + proof CHALLENGE | ≤90 min |
| W3 | Section 6 | Union Event-Model — Class-Column Substrate | C3 (+ §5 chain) | **Retired by REDRESS 98 — no further SK-V9 dispatch** | — |
| W4a | Section 7.1 | 32-byte String-Block Widening | C5 | Replan required: existing string-scanner call site; no W3 dependency | ≤90 min wall / 75-min redress |
| W4b-1 | Section 7.2.1 | `escape_codec` Scalar Reference + Checkasm Harness | C4 (S1/S6) | Replan required: existing unescape/codec substrate — lands FIRST if retained | ≤90 min wall / 75-min redress |
| W4b-2 | Section 7.2.2 | Fixed-Width Codec Bodies + JSON Consumer | C4 (S2/S3/S5/S7/S8/S11) | PAIRED with existing-substrate W4a if retained; conditional on W4b-1 close | ≤90 min wall / 75-min redress |
| W4b-3 | Section 7.2.3 | Variable-Width Const-Generic Bindings + Codegen | C4 (S4/S9/S10) | Conditional on W4b-2 close | ≤90 min wall / 75-min redress |
| W4c | Section 7.3 | SHA3 EOR3 Prefix-XOR Ladder | C6 | Retired with W3 unless a future non-union consumer is specified | ≤90 min wall / 75-min redress |
| W4d | Section 7.4 | CSSC CTZ String-Mask Consumer | C7 | Replan required after W4a; no union-substrate consumer | ≤90 min wall / 75-min redress |
| W5 | Section 8 | Close And Alpha Feedback | — | Superseded by Pass Alpha dispatch | ≤90 min |

REDRESS 98 supersedes the original dependency order. W1 is closed. W2 is
closed. W3 is retired and must not be forced a fourth time under a renamed
gate. The old cascade-lock is void because its substrate was the falsified
candidate. W4a/W4b/W4d may be reintroduced only by a fresh plan against
existing call sites; W4c is retired with W3 unless a new non-union consumer is
specified. If a requested wave cites the old W3 cascade-lock, refuse dispatch
and record REDRESS 98 as the reason.

## Per-Wave Triumvirate Protocol

Every wave — and every W4 sub-wave — is one triumvirate per
`SKINNY-TRIUMVIRATE.md` §1: three phases, each its own commit.

### Phase 1 — Research (read-only)

- Up to six parallel research agents on disjoint scope rows; 30 min
  cap each. For admitted W1/W2 and any re-scoped W4 work, the S-P2 report
  owning the wave plus the P3-A..E cohort is archived research; new research
  lands only if a wave blocker is discovered.
- Research writes one artefact per agent under
  `restart/skinny/tranches/sk-v9/research/` and edits no source.
- Commit: `docs(sk-v9-wave{W}-research): archive {scope} cohort`.

### Phase 2 — Plan (synthesis)

- One or two plan agents; 30 min cap. The plan selects the single SPEC
  intervention and names: owner paths (exactly the SPEC Section's owner
  table), the falsifiability gate (the SPEC Section's exit gate), the
  LOC budget + risk class + hard cap, the revert protocol, the
  same-wave consumer, the per-primitive scalar-reference + checkasm
  requirement, and the pre-blocked routes (the SPEC Section's
  pre-blocked-routes list, sourced from P3-E).
- Plan agents edit no source.
- Commit: `docs(sk-v9-wave{W}-plan): select {intervention}`.

### Phase 2.5 — CHALLENGE (mandatory for W2 and any re-scoped W4 source wave)

- Six-lens adversarial review of the plan per `SKINNY-TRIUMVIRATE.md`
  §4 (CH1 correctness, CH2 generality/Lock 14, CH3 regression/REDRESS,
  CH4 cost, CH5 hidden coupling, CH6 anti-paper-close). 90 min wall.
- CHALLENGE is mandatory for any re-scoped W4 source wave because each lands a
  SIMD/ASM kernel, parity harness, or production caller. CHALLENGE is optional
  for W1 and skipped for pure docs reconciliation.
- W3 CHALLENGE authority is closed by CHALLENGE V4 and REDRESS 98. No new W3
  CHALLENGE may grant a redress extension in SK-V9.
- A REJECT disposition routes the wave back to plan; the wave does not
  reach redress until CHALLENGE accepts.

### Phase 3 — Redress (implementation + measurement)

- One redress agent — single implementation thread per wave; no
  shared-file races. 75-min redress cap (60 impl + 15 measure). Each W4
  sub-wave — including each of W4b-1/W4b-2/W4b-3 — gets its own 75-min
  redress; that is the point of the sub-wave structure. The old W3 extension
  authority is closed by REDRESS 98.
- The redress agent implements only the SPEC Section's owner paths;
  any other source path returns REVISE before editing. For a re-scoped W4
  sub-wave it wires the kernel into an existing hot-path caller in the same
  commit; the W3 union substrate is not a valid caller. It produces a fresh
  Criterion capture
  with `RUSTFLAGS="-C target-cpu=native"`, measures against the SPEC
  Section's exit gate, and per `SKINNY-TRIUMVIRATE.md` §8 confirms the
  same-wave consumer call shows in the `samply` symbol path on the
  affected rows.
- Commit on PASS: `feat(sk-v9-wave{W}): admit {intervention}` — source
  edits + bench rerun output + REDRESS entry numbering the admit.
- Commit on FAIL: `docs(sk-v9-wave{W}-redress): reject {intervention}`
  — REDRESS entry with per-row Mbps measurement evidence + the
  reverted patch saved at `/tmp/skv9-wave{W}-rejected.patch`.

A failed wave still produces three commits. The next wave starts fresh. The
old W4b-2 NEAR-FAIL / FAIL rule is historical until a fresh existing-substrate
gate re-admits it.

## Falsifiability Gates

Live or retired gates are recorded in the SPEC — `G-W1-TYPED-ADMISSION` (§4),
`G-W2-RETAINED-PROOF` (§5), `G-W3-UNION-SUBSTRATE` (§6, retired by REDRESS 98),
`G-W4a-STRING-BLOCK` (§7.1), `G-W4b-1-CODEC-HARNESS` (§7.2.1),
`G-W4b-2-CODEC` (§7.2.2), `G-W4b-3-CODEC-BINDINGS` (§7.2.3),
`G-W4c-EOR3` (§7.3), `G-W4d-CTZ` (§7.4), and `G-W5-CLOSE` (§8, superseded by
Pass Alpha). The SPEC is the single source. Any future re-scoped wave must name
corpus rows and Mbps thresholds from the live `skinny/RESULTS.md` baseline; an
unmeasurable gate is a REVISE.

Load-bearing gate facts the redress agent must carry into measurement:

- **The W10b six-row block** (`canada` ≥ 15866, `citm_catalog` ≥
  28630, `instruments` ≥ 15865, `marine_ik` ≥ 11831, `mesh` ≥ 12186,
  `numbers` ≥ 17596) remains a binding maintain gate on every re-scoped W4
  sub-wave that touches the parse loop or an aarch64 SIMD kernel. Each
  floor is `floor(today × 0.98)` or `ceil(sonic_strict / 1.10)`,
  whichever higher; the `today × 0.98` leg is floored uniformly across
  all six rows (the single rounding convention for the block). For
  `canada` the live sonic-strict parse_only is 12723 (`RESULTS.md:10`),
  so `floor(today × 0.98)` = 15866 binds — there is no 15871 sonic
  floor; that figure was a stale SK-V8-era carryover and is corrected
  here. Any one row below its floor falsifies the wave.
- **The codec admits zero rows alone.** P2-E §6.4 remains evidence: the codec
  alone closes none of `unicode_escapes`, `y_string_unicode`, `unicode_mixed`,
  or `gsoc-2018`. Any future W4b-2 row-moving rule must be re-gated against an
  existing unicode-unescape/sink caller and must keep the W10b maintain block.
- **W3 is retired.** The prior W3 must-improve rows and self-time clauses are
  retained as REDRESS 96/97 evidence, not as a live gate. Any attempt to reopen
  the same union-substrate thesis is pre-blocked by REDRESS 98.

## Pre-Blocked Routes

The pre-blocked-route ledger is `research/p3/skv9-p3-E-preblocked-ledger.md`
— the binding S-P3 ledger, carried per-wave in each SPEC Section's
"Pre-blocked routes" list. A wave that touches a rejected ownership
boundary must cite the REDRESS entry, state the material differential,
and pass CHALLENGE before redress.

The five material differentials each REDRESS-adjacent wave must clear
(P3-E §3):

- **W3 (union) vs REDRESS 92 + 50-72** — retired by REDRESS 98. The prior
  material differential (co-indexed class column, no `UnionTape`, SIMD index
  consumed by move) was implemented and measured below every W3/W10b floor;
  it is now pre-block evidence, not a route to redress.
- **W4b (codec, W4b-1/W4b-2/W4b-3) vs REDRESS 82** — the 4-quartet
  batched path may no longer cite the retired union substrate; any retained codec
  plan must name an existing unicode-unescape/sink caller, keep
  `escape_codec_hex_unit` as a const-generic primitive with five bindings, and
  pass CHALLENGE before source redress. The old pre-block is not cleared until
  the row-moving sub-wave W4b-2 is re-gated without W3.
- **W4c (EOR3) vs REDRESS 88** — EOR3 is a 3-input bitwise XOR
  (1-cycle), not PMULL (4-cycle carryless multiply); it accelerates
  the scalar ladder REDRESS 88 *kept*, gated by `FEAT_SHA3`, scalar
  fallback unconditional. PMULL stays rejected as the default.
- **W4a (string-block) vs REDRESS 83** — different call site (the full
  `match_string_at_quote_trusted_utf8` path, not the tiny 16-byte-cap
  probe); a 32-byte successor primitive, not a JSON wrapper; the gate
  measures the combined producer + consumer path.
- **W4d (CTZ) vs REDRESS 89** — different call site (the string-mask
  first-set extract, not the `bulk_emit_positions_64` bulk consumer);
  LOSS rows under guard, not the WIN-block numeric rows; the W10b
  six-row maintain gate is the hard blocking precondition.

The 13 hard pre-blocks no SK-V9 wave reopens under any framing
(P3-E §4):

1. Apache/CITM measured-row overclaim (REDRESS 91) — W1's fresh-run-id
   differential is the only admissible path, Apache + CITM only.
2. `canada/real_typed_struct` without full-fixture DirectBuild-vs-serde
   checksum proof (REDRESS 91, 80).
3. W3 structural implementation under any union-substrate framing (REDRESS 98).
4. W4 scalar-parent / parent-digest fold (REDRESS 93).
5. REDRESS 73 helper-shape transfer across the generated/hand boundary.
6. Sidecar / parallel-substrate class (REDRESS 50, 51, 53, 60-72, 92):
   new `BackendShape`/BIR/directive, `UnionTape`, public substrate API,
   parser-owned cursor/fact slots, `tape_vs_tape` production consumer.
7. PMULL prefix-XOR and CTZ/bulk production rewires as default hot
   paths (REDRESS 88, 89).
8. Generic JSON policy leaks / Lock 14 weakening (REDRESS 85, 86, 87).
9. String-scanner widening / boundary-collapse class without a
   pre-registered same-row falsification gate (REDRESS 60-65, 82-84) —
   only W4a and the codec sub-waves W4b-1/W4b-2/W4b-3 carry
   pre-registered admissions.
10. Direct receiver / scratch / semantic-fact class (REDRESS 66-69) —
    no SK-V9 wave enters the direct plane.
11. Bench-private hand Track 1 / hand typed sink class (REDRESS 34, 70).
12. PMU / cycles / Criterion-slope / masking / structural-scan as a
    producer (SPEC §1).
13. NEON `match_tiny_plain_string` as a retained parse-G fix (REDRESS
    28 + 33).

## Non-Negotiables

The SPEC Section 1 non-negotiables bind every wave. Load-bearing for
the implementation agent:

- No primitive ships without a scalar reference, a checkasm
  differential test, and a same-wave hot-path consumer landed in the
  same commit. An orphan kernel is a REJECT — the SK-V5 failure shape.
  For a re-scoped W4 sub-wave the consumer is an existing hot-path caller,
  wired same-commit.
- No new outcome variant (the enum is the 10-identifier W0-admissible
  set `A C G I J K L M N-direct S`) and no new telemetry column (the
  schema is the 36-identifier P3-D set). A wave that adds either fails
  closed.
- Every field a wave emits into `RESULTS.md` is consumed by `gate-json`
  in the same wave — no emit-now-consume-later; a producer-only field
  fails the wave.
- No behavior source change without a same-wave consumer and a measured
  row gate.
- No wave closes on a future-phase promise. "Wired" or "integrated"
  without a bench-row threshold is a paper-close (CH6).
- Every generic-crate edit carries a CSS L4 / Sheets / BBNF-self
  non-JSON proof (SPEC §2.1).
- Substrate cardinality stays at one across every wave; REDRESS 98 blocks a new
  union/event substrate in SK-V9.
- Research, plan, CHALLENGE, redress, and close are distinct commits;
  same-commit role merger was the SK-V5 failure pattern.

## Status Discipline

Before any status reply, reconcile agent status, running cargo/rustc
processes, artefact mtimes, and dirty worktree state. Keep research,
plan, CHALLENGE, redress, and close artefacts in distinct commits per
`SKINNY-TRIUMVIRATE.md` §9. Stage the intended slice separately from
any pre-existing dirty worktree state.

Every dispatch carries an explicit minute cap. At 0.9× the cap the
agent commits; at the cap it halts and surfaces an extension decision
to the user.

## Convergence And Escalation

The SK-V9 bracket now converges by REDRESS 98 supersession: W0, W1, and W2 are
closed; REDRESS 96 and REDRESS 97 reject the two faithful W3 implementations;
CHALLENGE V4 rejects class-lane-only; REDRESS 98 retires W3 and supersedes the
old W4/W5 cascade. The five close documents must agree that no further SK-V9
W3 source redress is authorized and that Pass Alpha is the next live action.

If a future re-scoped wave's redress fails, the wave records a measured REDRESS
reject and the next wave starts fresh. A W4b-2 per-row NEAR-FAIL / FAIL
recorded honestly remains a measured admit of the codec primitive only if the
fresh existing-substrate gate says so.

A wave whose falsifiability gate cannot be made measurable, or a
goalset row no shortlist candidate can meet, escalates to the user as a
`BLOCKED` verdict naming the unresolved gate.
