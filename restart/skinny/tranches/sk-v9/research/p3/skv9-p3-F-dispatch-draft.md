# SK-V9 P3-F: DISPATCH-PROMPT Draft

Pass: S-P3 Synthesis-Plan. Cycle: V2.
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
interventions into the C1..C8 shortlist and sequenced them W1-W5. The
behaviour waves are dispatchable in the SPEC Section 2 dependency
order. A wave is dispatch authority only after its own SPEC entry gate
passes.

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
    - W3 → `research/p2/skv9-p2-A-union-event-model.md` +
      `skv9-p2-D-aarch64-asm-opportunities.md` §5.
    - W4a → `research/p2/skv9-p2-D-aarch64-asm-opportunities.md` §4.
    - W4b → `research/p2/skv9-p2-E-unicode-escape-codec.md` +
      `skv9-p2-D-aarch64-asm-opportunities.md` §3.
    - W4c → `research/p2/skv9-p2-D-aarch64-asm-opportunities.md` §5.3.1.
    - W4d → `research/p2/skv9-p2-D-aarch64-asm-opportunities.md` §4.4.
12. `restart/skinny/tranches/sk-v9/research/skv9-W0-close.md`.
13. `skinny/RESULTS.md`.
14. `skinny/REDRESS.md`.

## Wave Manifest

| Wave | SPEC section | Title | Shortlist | Dispatch status | Hard cap |
|---|---|---|---|---|---:|
| W0 | Section 3 | SK-V9-open Telemetry-Lock Recovery | — | Closed | — |
| W1 | Section 4 | Apache/CITM Measured Typed-Row Admission | C1 | Dispatchable — independent | ≤90 min |
| W2 | Section 5 | Retained Class/Event Grammar + `ValueRef` Proof | C2 | Conditional on W1 close + proof CHALLENGE | ≤90 min |
| W3 | Section 6 | Union Event-Model — Class-Column Substrate | C3 (+ §5 chain) | Conditional on W2 proof acceptance | ≤90 min |
| W4a | Section 7.1 | 32-byte String-Block Widening | C5 | Conditional on W3 close (cascade-lock) | ≤90 min |
| W4b | Section 7.2 | `escape_codec_hex_unit` Codec — Conditional Admission | C4 | PAIRED with W4a — strictly adjacent | ≤90 min |
| W4c | Section 7.3 | SHA3 EOR3 Prefix-XOR Ladder | C6 | Conditional on W3 close | ≤90 min |
| W4d | Section 7.4 | CSSC CTZ String-Mask Consumer | C7 | Conditional on W3 close + W4a close | ≤90 min |
| W5 | Section 8 | Close And Alpha Feedback | — | Conditional on W1-W4 dispositions | ≤90 min |

The dependency order is firm (`HARDENING-S-P2-CONVERGED.md`,
P3-B §2-§3): W1 is fully independent and lands first among behaviour
waves; the W2 proof unblocks W3; the W3 union substrate is the
cascade-locked consumer base for W4a-d. W4 is **sub-waved** — W4a, W4b,
W4c, W4d are four separate triumvirates. The sub-wave structure exists
because a monolithic codec+string-block+ASM wave is ~1,595-1,860 LOC
and cannot complete in a 75-min redress (SPEC §2.2). If a requested
wave's SPEC entry gate is not PASS, refuse dispatch and record why. A
conditional section is not dispatch authority until its entry gate
passes.

**The cascade-lock (SPEC §2.2).** P2-D §0 forbids a P2-D kernel
landing *without the union substrate existing*. This is satisfied by
W3 preceding W4a-d — by the time any W4 sub-wave dispatches the W3
union class column is landed and live. It does NOT mean one monolithic
wave. Each W4 sub-wave's redress commit wires its kernel into the
already-landed W3 union **in the same commit**: the consumer exists,
the caller is wired same-commit, no orphan ships. W4b is additionally
**strictly paired with W4a** — it dispatches only with W4a landed,
because neither the codec nor the string-block widening closes the four
uncloseable rows alone (P2-E §6.4).

## Per-Wave Triumvirate Protocol

Every wave — and every W4 sub-wave — is one triumvirate per
`SKINNY-TRIUMVIRATE.md` §1: three phases, each its own commit.

### Phase 1 — Research (read-only)

- Up to six parallel research agents on disjoint scope rows; 30 min
  cap each. For W1-W4 the S-P2 report owning the wave plus the P3-A..E
  cohort is the archived research; new research lands only if a wave
  blocker is discovered.
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

### Phase 2.5 — CHALLENGE (mandatory for W2, W3, W4a, W4b, W4c, W4d)

- Six-lens adversarial review of the plan per `SKINNY-TRIUMVIRATE.md`
  §4 (CH1 correctness, CH2 generality/Lock 14, CH3 regression/REDRESS,
  CH4 cost, CH5 hidden coupling, CH6 anti-paper-close). 90 min wall.
- CHALLENGE is mandatory for W2 (first-of-class proof surface), W3
  (substrate-touching), and each of W4a/W4b/W4c/W4d (each lands a
  SIMD/ASM kernel). CHALLENGE is optional for W1 (mechanical
  baseline-whitelist expansion) and skipped for W5 (docs
  reconciliation).
- A REJECT disposition routes the wave back to plan; the wave does not
  reach redress until CHALLENGE accepts.

### Phase 3 — Redress (implementation + measurement)

- One redress agent — single implementation thread per wave; no
  shared-file races. 75 min cap (60 impl + 15 measure). Each W4
  sub-wave gets its own 75-min redress — that is the point of the
  sub-wave structure.
- The redress agent implements only the SPEC Section's owner paths;
  any other source path returns REVISE before editing. For a W4
  sub-wave it wires the kernel into the already-landed W3 union
  substrate in the same commit. It produces a fresh Criterion capture
  with `RUSTFLAGS="-C target-cpu=native"`, measures against the SPEC
  Section's exit gate, and per `SKINNY-TRIUMVIRATE.md` §8 confirms the
  same-wave consumer call shows in the `samply` symbol path on the
  affected rows.
- Commit on PASS: `feat(sk-v9-wave{W}): admit {intervention}` — source
  edits + bench rerun output + REDRESS entry numbering the admit.
- Commit on FAIL: `docs(sk-v9-wave{W}-redress): reject {intervention}`
  — REDRESS entry with per-row Mbps measurement evidence + the
  reverted patch saved at `/tmp/skv9-wave{W}-rejected.patch`.

A failed wave still produces three commits. The next wave starts fresh.
For W4b a per-row NEAR-FAIL / FAIL recorded honestly is not a wave
reject — it is a measured admit of the codec primitive with the row's
GO status withheld; see Falsifiability Gates.

## Falsifiability Gates

Every wave's falsifiability gate is the exit gate stated in its SPEC
section — `G-W1-TYPED-ADMISSION` (§4), `G-W2-RETAINED-PROOF` (§5),
`G-W3-UNION-SUBSTRATE` (§6), `G-W4a-STRING-BLOCK` (§7.1), `G-W4b-CODEC`
(§7.2), `G-W4c-EOR3` (§7.3), `G-W4d-CTZ` (§7.4), `G-W5-CLOSE` (§8).
The gates are not restated here; the SPEC is the single source. Each
gate names corpus rows and Mbps thresholds; every threshold derives
from the live `skinny/RESULTS.md` SK-V9-open baseline. An unmeasurable
gate is a REVISE.

Load-bearing gate facts the redress agent must carry into measurement:

- **The W10b six-row block** (`canada` ≥ 15866, `citm_catalog` ≥
  28631, `instruments` ≥ 15865, `marine_ik` ≥ 11831, `mesh` ≥ 12186,
  `numbers` ≥ 17597) is a binding maintain gate on W3 and on every W4
  sub-wave that touches the parse loop or an aarch64 SIMD kernel. Each
  floor is `today × 0.98` or `ceil(sonic_strict / 1.10)`, whichever
  higher. For `canada` the live sonic-strict parse_only is 12723
  (`RESULTS.md:10`), so `today × 0.98` = 15866 binds — there is no
  15871 sonic floor; that figure was a stale SK-V8-era carryover and
  is corrected here. Any one row below its floor falsifies the wave.
- **W4b admits zero rows on the codec alone.** P2-E §6.4 is binding:
  the codec alone closes none of `unicode_escapes`, `y_string_unicode`,
  `unicode_mixed`, `gsoc-2018`. W4b admits per-row, on measurement:
  `unicode_escapes` ≥ 16319 (NEAR-FAIL 94.5% projected),
  `y_string_unicode` ≥ 8270 (NEAR-FAIL 94.8%), `unicode_mixed` ≥ 12338
  (FAIL 63.7% on the codec alone — admits only on the *combined* W4a +
  W4b measured Mbps), `gsoc-2018` ≥ 21430 (no-regression basis). A row
  that NEAR-MISSES stays `S / NO-GO`, its measured codec contribution
  is recorded in REDRESS, and the wave still admits the codec as a
  checkasm-verified primitive. **W4 may close with zero strict
  unicode-row admissions — that is an honest measured outcome, not a
  paper-close.** W4b is reverted wholesale only on a checkasm parity
  failure or a W10b WIN-block regression.
- **`gsoc-2018` does not bind W3.** W3 falsifies only if the
  structural-rediscovery hot leaf does not drop to ≤ 5% self-time.
- **The W3 must-improve rows** are the seven GO-target structural-dense
  rows: `twitter` ≥ 17685, `apache_builds` ≥ 14124, `update_center` ≥
  14369, `distinct_values` ≥ 15731 (each `ceil(sonic_strict / 1.10)`),
  with `consume_structural` ≤ 5% and `at_cursor` ≤ 1% self-time.

## Pre-Blocked Routes

The pre-blocked-route ledger is `research/p3/skv9-p3-E-preblocked-ledger.md`
— the binding S-P3 ledger, carried per-wave in each SPEC Section's
"Pre-blocked routes" list. A wave that touches a rejected ownership
boundary must cite the REDRESS entry, state the material differential,
and pass CHALLENGE before redress.

The five material differentials each REDRESS-adjacent wave must clear
(P3-E §3):

- **W3 (union) vs REDRESS 92 + 50-72** — the union splits cursor and
  class; the class column is the tape's own column at the existing
  `emit_plain_offset` site, not a sidecar; the SIMD index is consumed
  by move (Lock 1 cardinality stays at one). W3 implements the routed
  precursor, gated behind the W2 proof.
- **W4b (codec) vs REDRESS 82** — the 4-quartet batched path is the
  union-substrate path, not a parser-owned per-quartet classifier;
  `escape_codec_hex_unit` is a const-generic primitive with five
  bindings; the gate is `parse_only` only.
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
3. W3 structural implementation without the W2 retained-grammar +
   `ValueRef` proof accepted (REDRESS 92).
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
   only W4a and W4b carry pre-registered admissions.
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
  For a W4 sub-wave the consumer is the already-landed W3 union
  substrate, wired same-commit.
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
- Substrate cardinality stays at one across every wave; the W3 class
  column is a co-indexed column on the existing offset tape, not a new
  tape.
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

The SK-V9 bracket converges when every SPEC wave — W1, W2, W3, W4a,
W4b, W4c, W4d, W5 — has admitted or rejected with measurement, the
§0.1 close condition holds (including clause 6: W4 may close with zero
strict unicode-row admissions if every uncloseable row records
NEAR-FAIL / FAIL honestly), and the five close documents agree
(`SKINNY-TRIUMVIRATE.md` §3). Convergence triggers `G-ALPHA-SK-V9` and
the Pass Alpha dispatch for the SK-V9 → SK-V10 synthesis.

If a wave's redress fails, the wave records a measured REDRESS reject
and the next wave starts fresh — a rejected wave is not a bracket
stall. A W4b per-row NEAR-FAIL / FAIL recorded honestly is a measured
admit of the codec primitive, not a wave reject. If the SK-V9 bracket
exceeds 12 waves without convergence, the orchestrator escalates to the
user with `BLOCKED: skinny bracket V9 exceeded 12 waves; user adjudicate
scope or abandon`.

A wave whose falsifiability gate cannot be made measurable, or a
goalset row no shortlist candidate can meet, escalates to the user as a
`BLOCKED` verdict naming the unresolved gate.
