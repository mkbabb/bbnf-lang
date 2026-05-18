# SK-V9 P3-B: Wave Sequencing

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-18.
Scope: Order the S-P2 / P3-A candidate interventions into a SK-V9
post-W0 wave manifest W1..W{n}, with per-wave entry gate, wave class,
dependency justification, and triumvirate shape.
Output: this file.
Pass Alpha goalset: SK-V9 §0 close — coherent telemetry manifest (W0,
closed); no behavior surface moved without same-wave consumer + measured
gate; strict admission strict-vs-strict on matching planes only; the
four named blocked routes (Apache/CITM measured rows, retained
class/event grammar, direct contract, comparator sidecar) each admit by
their named gate or remain explicitly blocked.
Candidate pool: research/p2/ post-CHALLENGE survivors (S-P2 converged
per `HARDENING-S-P2-CONVERGED.md`); P3-A shortlist cross-referenced.

## §1 — Method

S-P3 W0 (SK-V9-open telemetry-lock) is **already CLOSED** — commit
`90609aee`, run id `sk-v9-open:criterion-fnv64-cd1673844eeea12f`. This
artefact sequences the post-W0 behavior waves only.

The candidate pool is the six converged S-P2 artefacts. Five carry
intervention shapes; P2-F is the SOTA-teardown reference (a dependency
graph and an anchoring-signal supplier, not itself a candidate). The
post-W0 behavior candidates are therefore four:

| Candidate | Source | Class | Independence |
|---|---|---|---|
| Apache/CITM measured-row admission | P2-C | row-moving (GO-count) | fully independent |
| Retained class/event grammar + `ValueRef` proof | P2-B | proof-only | independent; gates P2-A |
| Union event-model | P2-A | row-moving (structural-dense) | gated by P2-B |
| aarch64 ASM consumers + unicode codec + string-block widening | P2-D + P2-E | row-moving (string/unicode-dense) | gated by P2-A |

Three sequencing constraints are firm, taken verbatim from
`HARDENING-S-P2-CONVERGED.md` §"convergent picture":

1. **P2-B proof precedes P2-A union.** P2-B removes the HANDOFF §5
   pre-block "W3 structural implementation without retained class/event
   grammar plus retained `ValueRef` cursor proof" — the REDRESS 92
   gating clause. P2-B §5: the proof is the *necessary* (not
   sufficient) condition for the SC-3 union to become eligible to
   dispatch. P2-A may not dispatch as a measured-row wave until that
   pre-block is removed.
2. **P2-A union precedes the P2-D consumers that bind to it.** P2-F §7.4
   records `III ← P2-D ← P2-A` and `II ← P2-E (← P2-A secondary)`. The
   P2-D string-mask CTZ consumer, the codec offset-tape sink, and the
   stage-1-index consume-by-move all bind to the P2-A union substrate
   (the class column + co-indexed cursor stream). P2-D §3.5 / §4.4 state
   the codec broadening and the CTZ consumer "block on P2-A landing in
   the same wave OR fail CH5".
3. **P2-C is fully independent.** P2-C §"Lock surface" — no Lock 1
   substrate touch, no Lock 14 weakening, no SIMD primitive. It is the
   cheapest GO-count lift (300 LOC, ≤90 min) and may sequence early.

The sequencing principle is **topological + risk-graded**: proof before
consumer (P2-B → P2-A → P2-D/E); guard rows before risk rows (P2-C, the
substrate-independent low-risk GO lift, lands before the substrate
churn of P2-A); and the conditional same-wave pairing of P2-E codec with
the P2-D string-block widening (P2-E §6.4: "neither closes the four
uncloseable rows alone") is honored by landing them in **one wave**, not
two.

The wave count is **four** post-W0 (W1–W4) plus a W5 close
reconciliation — five behavior-bracket waves total, six counting the
closed W0. This is inside the ≤12 skinny-bracket ceiling (`ORCHESTRATOR`
§3Z) and aligns with the SPEC §2 placeholder slot count.

## §2 — The wave manifest

W0 SK-V9-open telemetry-lock is CLOSED (`90609aee`). The Interlock
(fresh S-P1 rerun, SPEC §4) is a pass-internal convergence gate, not a
behavior wave; `G-S-P1-RERUN-CONVERGED` and `G-BEHAVIOR-RELEASE` are the
entry conditions for W1 and are assumed satisfied by the time this
manifest dispatches (S-P3 itself is the revised S-P2/S-P3 release that
SPEC §5 names — convergence of this pass *is* `G-BEHAVIOR-RELEASE`).

| Wave | Name | Candidate | Entry gate | Class | Triumvirate shape |
|---|---|---|---|---|---|
| **W1** | Apache/CITM measured-row admission | P2-C | W0 closed; `G-S-P1-RERUN-CONVERGED` + `G-BEHAVIOR-RELEASE` (S-P3 convergence) | row-moving (typed GO-count) | research 6 / plan 1 / redress 1; CHALLENGE optional (mechanical, well-understood — skippable per SKINNY-TRIUMVIRATE §4) |
| **W2** | Retained class/event grammar + `ValueRef` proof | P2-B | W1 closed; SK-V9-open baseline maintained by W1 | proof-only (no `RESULTS.md` row movement) | research 6 / plan 1 / **CHALLENGE mandatory** / redress 1 — first-of-class (new trait + substrate-contract artefact) |
| **W3** | Union event-model (class-column substrate) | P2-A | W2 closed; **P2-B proof admitted** (the HANDOFF §5 W3 pre-block removed); REDRESS 92 gating clause discharged | row-moving (structural-dense losses) | research 6 / plan 1–2 / **CHALLENGE mandatory** / redress 1 — first-of-class (substrate touch, Lock 1 audit) |
| **W4** | aarch64 ASM consumers — unicode codec + string-block widening | P2-D + P2-E (paired) | W3 closed; **P2-A union substrate landed** (class column + cursor stream live); five checkasm differential test files authored as same-wave preconditions | row-moving (string-dense + unicode-dense) | research 6 / plan 2 / **CHALLENGE mandatory** / redress 1 — first-of-class (NEON kernels, checkasm parity, Lock 16 host-cap gate) |
| **W5** | Close + Alpha feedback | none (reconciliation) | W1–W4 each admitted or rejected with measurement | cleanup / telemetry reconciliation | research 1–2 / plan 1 / redress 1; no CHALLENGE (no source-behavior edit) |

Notes on the manifest:

- **No W0 row** is restated here — it is closed. Numbering continues from
  W1 to keep the post-W0 sequence contiguous and to mirror the SPEC §2
  placeholder slots (SPEC W1=release, W2=typed, W3=tape, W4=direct,
  W5=close). P3-F resolves the SPEC-slot rename: the SPEC's W1 "release"
  slot is consumed by S-P3 convergence itself, so the behavior waves
  shift to occupy SPEC §6/§7/§8/§9. P3-B's W1–W5 names are the *behavior*
  sequence; P3-F binds them to SPEC section numbers.
- **The SPEC §8 direct-contract placeholder is NOT scheduled.** No S-P2
  candidate furnishes a direct output / control-path contract; REDRESS
  93 (scalar-parent fold) remains a pre-block with no fresh candidate.
  The direct route stays an explicitly-blocked placeholder, satisfying
  SK-V9 §0 close-condition 6 ("either admit by named gates or remain
  explicitly blocked"). This is recorded, not silently dropped.
- **P2-E is not a standalone wave.** Its admission rule (P2-E §6.4) is
  the "same-wave conditional": the codec admits *paired with* a
  per-string-span scanner intervention. W4 lands P2-E and the P2-D
  32-byte string-block widening in one redress commit; neither closes
  `unicode_escapes` / `y_string_unicode` / `unicode_mixed` / `gsoc-2018`
  alone, so a single-candidate wave would paper-close.

## §3 — Dependency justification per wave

### W1 — Apache/CITM (P2-C) — first because cheapest + independent

P2-C is substrate-independent: it touches no Lock 1 surface, adds no
SIMD primitive, and does not depend on P2-A's union or P2-B's proof. It
is the cheapest GO-count lift (300 LOC, ≤90 min) and moves two rows from
`citm_catalog` / `apache_builds` source-parity (REDRESS 91) to measured
`real_typed_struct A / GO`. Placing it first banks a guaranteed
row-movement before the substrate churn of W3, and gives W3/W4 a fresh,
larger maintain-row block to gate against. It is the "guard rows before
risk rows" instance — a low-risk admit before the high-risk substrate
work. Its falsifiability gate (P2-C §4.3) is fully measurable from the
bench today, with no upstream proof obligation. The SPEC §2.1 generality
gate is trivially satisfied: P2-C touches no generic crate.

### W2 — P2-B proof — second because it gates W3

P2-B is proof-only: an `EventGrammar` trait + `ValueRef<G>` + JSON &
Sheets witnesses behind `cfg(feature = "proof")`, ~395 LOC, zero
generated-output LOC, **no production consumer and no `RESULTS.md` row
movement** (P2-B §1.2, §5). It cannot move a row, so it could in
principle sit anywhere — but it is the *necessary* gate for W3: P2-B §5
states the proof "removes exactly one pre-block from the SK-V9 HANDOFF
§5 ledger" — the W3 structural-implementation pre-block — and "makes the
SC-3 Tier A migration *eligible to dispatch*". Without W2 closed, W3
re-opens REDRESS 92 directly and returns REVISE without a source edit
(SPEC §2: "any W1+ behavior attempt before release returns REVISE").
W2 therefore sits immediately before W3. It carries a mandatory
CHALLENGE: it is a first-of-class artefact (a new trait + substrate
contract), and CH5 must confirm the witnesses do not smuggle a
production consumer (the proof's admissibility *rests* on having none —
P2-B §5.1).

### W3 — Union event-model (P2-A) — third, gated on W2

P2-A is the structural fix for bbnf's substrate-bound parse-plane losses
(`HARDENING-S-P2-CONVERGED.md` §"convergent picture": "losses are
substrate-bound, not kernel-bound"). It keeps the parser-event cursor
stream, adds a co-indexed class column at emit time, and consumes the
SIMD index as a transient producer by move (~265 hand + ~120 regen LOC).
Its entry gate is **W2 closed with the P2-B proof admitted** — the
REDRESS 92 gating clause is discharged only by the proof's admission.
P2-A also deletes `consume_structural` (P2-A §2.5), so it must precede
W4: the W4 P2-D consumers (string-mask CTZ, codec offset-tape sink,
stage-1-index consume-by-move) all bind to the class column + cursor
stream the W3 substrate establishes. Mandatory CHALLENGE — it is a
substrate touch; CH5 runs the Lock 1 cardinality audit (P2-A §2.1: the
SIMD index stays a transient single producer, cardinality one), and the
W10b six-row maintain block (`canada`, `citm_catalog`, `instruments`,
`marine_ik`, `mesh`, `numbers`) is the binding non-advisory regression
gate (P2-A §4.2 / §4.4).

### W4 — P2-D + P2-E paired — fourth, gated on W3

W4 lands the string/unicode-dense interventions: the P2-E
`escape_codec_hex_unit` primitive and the P2-D 32-byte string-block
widening, paired in one redress commit. The pairing is mandatory: P2-E
§6.4's honest verdict is that zero of the four uncloseable rows admit on
the codec alone (`unicode_escapes` NEAR-FAIL 94.5%, `y_string_unicode`
94.8%, `unicode_mixed` FAIL 63.7%, `gsoc-2018` no-regression-basis); the
admission rule is the "same-wave conditional" — the codec admits only
*paired with* the per-string-span scanner widening. Its entry gate is
**W3 closed with the P2-A union substrate landed**: P2-D §3.5 / §4.4
state the codec broadening and the CSSC CTZ consumer block on P2-A
landing in the same wave or fail CH5, because their same-wave production
consumer *is* the P2-A union-substrate tape/event consumer. The five
checkasm differential test files (P2-D §"five checkasm differential
test files to author as same-wave preconditions") are authored inside
W4 as preconditions to the kernel admits — scalar reference + checkasm
parity before any kernel wires, per SKINNY-TRIUMVIRATE §8 and §10. W4
carries the largest CHALLENGE surface: NEON kernels, checkasm parity,
the Lock 16 host-capability gate on SHA3 `veor3q_u8` (see §4), and CH5
hidden-coupling on the codec offset-tape sink (P2-E §4.3: no retained
sidecar over `\u` positions).

### W5 — Close + Alpha feedback — last, reconciliation only

W5 reconciles `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SPEC.md`,
`DISPATCH-PROMPT.md`, `HANDOFF.md`, and V10 Alpha inputs (SK-V9 §0
close-condition 7). It edits no behavior source, so it carries no
CHALLENGE and a thin triumvirate. Entry gate: W1–W4 each admitted or
rejected with measurement evidence. It is the SPEC §9 close wave.

## §4 — Pass Omega gating

Pass Omega owns SC-6-L1-R1 — the Lock 1 refinement amendment — plus
broad lock amendments and canonical-path cleanup (`HANDOFF` §3 closing
paragraph; `ORCHESTRATOR` §6 G-Omega gate). The question P3-B must
settle: **does the P2-A union need the SC-6-L1-R1 refinement, and which
waves dispatch under SK-V9's existing authority?**

**The P2-A union does NOT need Pass Omega's SC-6-L1-R1.** P2-A §2.1
designs the union so the Lock 1 cardinality invariant is *preserved as
written*: the SIMD structural index is a transient producer consumed by
move, leaving exactly one retained substrate identity — "the SIMD index
is a transient producer consumed by move (Lock 1 cardinality stays at
one)" (`HARDENING-S-P2-CONVERGED.md` §"What S-P2 hands to S-P3", item
1). P2-A §2.2 cites Lock 1 verbatim ("A SIMD mask stream is a transient
producer, never a retained second copy") and the class column is an
additive column on the *existing* retained tape, not a second substrate.
P2-A §4.4 falsifying observation 1 names "Lock 1 R1 in SC-6" only as a
*failure detector* — if `consume_structural` self-time exceeds 5%
post-implementation, that signals a sidecar drift and the wave halts;
it is a falsification trigger, not a dispatch precondition. P2-B §5
confirms the asymmetry explicitly: the proof "does NOT bind: Pass
Omega's SC-6-L1-R1 refinement … it can be ratified before, after, or
independently of the proof's admission."

Consequently:

| Wave | Pass Omega dependency | Authority to dispatch |
|---|---|---|
| W1 (P2-C) | None | SK-V9 existing authority. P2-C touches Lock 14 only as a *scoped parent-diff allowance entry* (`sk-v9-real-typed-w{n}`, seven owner paths) — P2-C §4.1 frames it as an allowance, not an amendment; gated by `cargo test -p bbnf-bench lock14_baseline`. No Omega. |
| W2 (P2-B) | None | SK-V9 existing authority. Proof-only, `cfg(feature = "proof")`, no substrate change, no lock amendment. P2-B §5: SC-6-L1-R1 is not pre-bound. |
| W3 (P2-A) | **None** | SK-V9 existing authority. The union is designed *within* Lock 1 as written — cardinality stays at one. No new `UnionTape`, no new `BackendShape`, no new substrate surface (SPEC §1 non-negotiables hold). If a future SC-3 Tier A *production migration* wants the SC-6-L1-R1 refinement it is a separate SK-V10+ wave with its own S-P3 plan (P2-B §5) — but the SK-V9 W3 union event-model does not. |
| W4 (P2-D + P2-E) | **Conditional, on the SHA3 slice only** | The unicode codec, the 32-byte string-block widening, and the per-quartet NEON fallback dispatch under SK-V9 existing authority + Lock 16's existing host-capability-gate predicate. The P2-D SHA3 `veor3q_u8` EOR3 prefix-XOR collapse is host-capability-gated by `FEAT_SHA3` (P2-D §"Lock 16 admissibility caveat"); it is admissible under Lock 16's *existing* allowlist predicate as a per-host capability gate — **no Omega amendment is required** so long as the SHA3 path is a `FEAT_SHA3`-conditional branch with a scalar/EOR-chain fallback. If W4 instead wanted SHA3 as an unconditional default hot path, that would be a pre-blocked route (HANDOFF §5: "PMULL prefix-XOR … as default hot paths") and would require Omega; P3-B sequences the SHA3 slice as capability-gated, so W4 stays under SK-V9 authority. |
| W5 (close) | None | SK-V9 existing authority. Reconciliation only. |

**Verdict: every SK-V9 post-W0 behavior wave (W1–W5) dispatches under
SK-V9's existing authority.** No wave is gated by Pass Omega. The
SC-6-L1-R1 refinement is a parallel, independently-ratifiable Omega
concern; it is the precondition for a *future* SK-V10+ SC-3 Tier A
production migration, not for any SK-V9 wave in this manifest. The
single Omega-adjacent surface — the SHA3 EOR3 slice in W4 — is kept
within Lock 16's existing predicate by sequencing it as a host-capability
conditional, not a default rewire.

## §5 — Sources

- `restart/skinny/tranches/sk-v9/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
  — the converged dependency order; the convergent-picture statement.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`
  — §2.1 cursor/class split + Lock 1 cardinality; §2.5 `consume_structural`
  deletion; §4.2/§4.4 W10b maintain block + falsifying observations; §5
  per-slice cost.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`
  — §1.2 owner files + LOC; §5 unlock geometry (W3 pre-block removal,
  SC-6-L1-R1 not bound); §5.1 same-wave-consumer disposition.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`
  — §2.0 per-slice budget; §4.1 owner files + Lock 14 allowance; §4.2
  dispatch sequence; §4.3 falsifiability gates.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
  — §3.5 codec broadening blocks on P2-A; §4.2/§4.4 string-block widening +
  CTZ consumer; §"Lock 16 admissibility caveat" SHA3 host-cap gate; the
  five checkasm differential test files.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`
  — §4 same-wave consumer plan; §6.3/§6.4 honest verdict + same-wave
  conditional admission rule.
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md`
  — §7.4 inter-report dependency graph (`I ← P2-A ← P2-B`; `II ← P2-E`;
  `III ← P2-D ← P2-A`).
- `restart/skinny/tranches/sk-v9/SPEC.md` — §0 close-condition; §1
  non-negotiables; §2 wave manifest placeholders; §4 Interlock; §5–§9
  wave placeholders.
- `restart/skinny/tranches/sk-v9/HANDOFF.md` — §3 candidate boundaries +
  cost binding; §5 pre-blocked routes; W0 close + run id.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` — §2 P3-B scope; §8
  bbnf-specific axes (W0 always baseline; same-wave consumer per kernel).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` — §1 phase
  structure; §4 CHALLENGE interposition; §8 same-wave-consumer rule; §9
  role separation.
- `restart/prompts/ORCHESTRATOR.md` — §3Z convergence + ≤12-wave ceiling;
  §6 G-Omega / G-Alpha gates; §8 non-negotiables.
