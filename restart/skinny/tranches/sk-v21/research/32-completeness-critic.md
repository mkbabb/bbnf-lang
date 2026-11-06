# 32 — Completeness Critic: charter-axis coverage, contradictions, unverified acts, modalities not run

Date: 2026-07-05. Posture: walk every numbered axis and standing constraint of `PASS-ALPHA.md`,
name the report(s) that own each or declare the gap, then surface (a) uncovered charter items,
(b) inter-report contradictions, (c) claims still unverified that the pass-1 plan would act on,
and (d) modalities that were not actually run to the charter's discipline. Verified claims from
the two adversarial verifiers (reports 30/31) are taken as settled and are not re-litigated.

All 32 pass-0 agents are accounted for: dig shards 01–15, now-state auditors 16–21, SOTA
frontier 22–29, adversarial verifiers 30–31, this critic 32. The corpus is unusually
well-sourced — 30 confirms 13/14 load-bearing historical claims, 31 confirms every HEAD file:line
the plan would touch. The gaps below are therefore not about trustworthiness of what was found;
they are about **what was never assigned an owner**, and about **two opening-state artifacts the
charter names as binding that no report re-verified**.

---

## Part I — Axis-by-axis ownership walk

### The standing aim (charter §Charter, lines 8–12)
"A fully generalized parser generator that bests every SOTA … by a perfected union of
backend-agnostic code with an emitter specializing in a hybrid of Rust and hand-rolled ASM, in the
dav1d discipline — parse speed … first-order."

OWNED: the SOTA frontier is thoroughly covered — 22 (simdjson On-Demand / materialization lever),
23 (sonic-rs/yyjson/serde_json Eisel-Lemire), 24 (lightningcss eager-typed CSSOM), 25
(dav1d/checkasm/NEON-on-Apple-M), 27 (KernelCostTable via checkasm), 26 (the four-part-conjunction
novelty seam, still empty). The dav1d *discipline* (function-pointer kernel table + liveness
harness) is well-owned by 25/27.

PARTIAL GAP — "hand-rolled ASM": every report that touches the emitter (29, 25, 22) establishes
that bbnf today emits `core::arch::aarch64` **NEON intrinsics**, and 25 gives the honest bound
that hand-asm buys only ~0–5% over well-shaped intrinsics except at register-pressure failures. No
report audits whether the generator has, or can be made to have, an **ASM emission path at all**,
nor what it would take to co-emit the dav1d checkasm harness the charter's discipline implies. The
"hybrid of Rust and hand-rolled ASM" half of the aim is researched as *external aspiration*, never
as *current-emitter feasibility*. This is a design-question the synthesis must open explicitly, or
the ASM half of the aim silently degrades to "intrinsics only."

### NEW Axis 1 — "the restart itself is in question" (topology, MASTER-PLAN, locks; find the lacuna)
OWNED, well: 20 (crate topology / 247K-LOC god-crate / Lock 12–14 as aspirational prose / TW
manifest silently abandoning the 24-crate greenfield), 16 (MASTER-PLAN MP.SK19.W6 stale row;
Lock-14 live two-crate blast radius), 21 (the profiling lacuna at HEAD). The lacuna hunt landed.
Note the charter's own qualifier "Prototype, profile, debug … to find the lacuna" — prototype and
debug are pass-3 modalities and are correctly absent from pass-0; only *profile* was a pass-0
expectation, and it was run sub-discipline (see Part IV).

### NEW Axis 2 — "the Rust stack questioned wholesale" (edition, toolchain pin, SOTA deps, cutting-edge concepts)
OWNED: 19 (edition 2021→2024, Lock-15 profile violation, stale-dep census, serde_json smell), 29
(skinny sole 2021/1.78 holdout, 12-week-stale nightly, cranelift/parallel-front-end dormant,
divan half-migration). "All concepts cutting-edge": grammars/parsers → 26; type solvers → 28; CSP
→ 18/27; parsers/NEON → 22–25. Fully covered.

### NEW Axis 3 — "the archaeological dig" (all 49 tranche dirs)
OWNED, complete: dig shards 01–15 partition the entire `docs/tranches/` corpus. Verified on-disk:
the letters run AA, AB, AC, **(no AD — the corpus jumps AC→AE)**, AE…AH, AI…AL, AM…AO, AP…AR,
AS…AV, AW, AX, AY-{I,II-I,III}, AZ-{I,II,III,IV}, B0…B7, BA…BD, W, X, Y, Z, `archive`,
`meta-audit`. There is **no AD gap** — AD never existed. `archive` and `meta-audit` are covered by
15. No dig gap.

### NEW Axis 4 — "csp-solver has an active tranche set — coordinate, do not clobber"
OWNED: 18. Modality **satisfied** — the canonical tree
`~/Programming/csc411/CSC411_HW2_ProgrammingQuestion/` is present on-host (confirmed this pass),
report 18 diffed the vendored `crates/csp-solver/src` against it (byte-identical) and read the
csc411 tranche docs (`docs/tranches/2026-07-grand-uplift/README.md`), and verifier 31 independently
re-derived "from the canonical csc411 csp-solver tree." The coordination read really happened.
See Part III for the one unverified severity the plan would act on.

### NEW Axis 5 — "parse-that has moved" (refresh folds in/out; the coordination letter)
OWNED: 17. Modality **satisfied** — both `rust/` and `typescript/` trees and the VALUEJS-PT-E
letter were read. One live cross-repo obligation surfaced (bbnf owes parse-that a GO/NO-GO on the
LR/packrat tier) — see Part II gap.

### Standing constraints (charter §Standing constraints)
- "NOT an implementation phase; prototypes inform" — process constraint governing pass-3; no report
  owns it and none needs to.
- "NO quick solutions / workarounds / legacy" — the dig (01–15) is the negative-example corpus that
  arms this; adequately served.
- "No overfitting JSON/CSS; full grammar generalization" — PARTIAL. Enforcement is audited (16:
  Lock-14 live gate scans only 2 crates; 20: the LOCKS.md Lock-14 falsifier greps 13 paths, 8
  phantom, near-vacuous) and the *history* of overfitting is richly flagged (04, 06, 08, 14, 15,
  24). But no report **affirmatively certifies the current generalization state of the live
  emitter** — whether today's codegen still carries named JSON/CSS arms, given both Lock-14
  artifacts are shown toothless. The constraint's *violation surface* is owned; its *current
  status* is not.
- "Chronically deferred items fold into THIS tranche; the SK-V20 DEFERRED-LEDGER is the opening
  state" — **GAP** (see Part II).
- "All prior precepts recapped and verified; SK-V20 PRECEPTS-RECAP is the opening scorecard;
  re-verify" — **GAP** (see Part II).
- "Benchmarks: cold-only, loadavg-quiet, single-invocation; aarch64/Apple-M" — OWNED by 21, but 21
  itself ran **outside** this discipline (see Part IV).
- "Orchestration on core, fanout Opus/Sonnet" — process; not a research axis.
- "One cargo per target dir; profiling via samply (debug=true, interactive)" — the samply-interactive
  modality was **not** delivered for bbnf (see Part IV).

---

## Part II — Charter items no report covers (uncovered items, gap (a))

**G1 — The SK-V20 PRECEPTS-RECAP is never re-verified.** The charter binds pass-0 to re-verify the
SK-V20 PRECEPTS-RECAP scorecard ("re-verify, do not re-derive"), and the file exists at
`restart/skinny/tranches/sk-v20/PRECEPTS-RECAP.md`. Report 16 is titled "HEAD delta re-verify" but
it re-verifies the **nine SK-V20 head-delta adjudications** (7 of 9 hold) — a *different artifact*.
No pass-0 report walks the PRECEPTS-RECAP scorecard precept-by-precept against HEAD. The one
standing constraint that most directly protects against re-litigating settled precepts has no
owner.

**G2 — The SK-V20 DEFERRED-LEDGER is never walked as the opening census.** The charter declares the
DEFERRED-LEDGER (`restart/skinny/tranches/sk-v20/DEFERRED-LEDGER.md`, present on disk) to be "the
opening state," and requires chronically-deferred items to fold into THIS tranche. Individual items
are touched in isolation — 20 re-schedules C1 (AoS→SoA) and C3 (BackendShape) without diagnosing
the five-cycle stall, 18 confirms Lock 4 (egraph→csp) is live per DEFERRED-LEDGER:47, 11 recounts
AZ-IV's own 13/15 chronic figure — but **no report reconciles the SK-V20 DEFERRED-LEDGER line by
line** against pass-0 findings. Synthesis (pass-1 step 2) will need this reconciliation as its
opening ledger and it does not yet exist.

**G3 — The parse-that LR/packrat GO/NO-GO obligation is unassigned.** Report 17 surfaces that the
only live cross-repo *ask* lands on bbnf: parse-that holds its packrat/WDM LR tier open expressly
for the bbnf-lang LR-consumer question, and "SK-V21 owes a GO/NO-GO or the tier may be retired."
This is an outbound obligation with a deadline pressure, not merely an inbound fold. No report
proposes an owner or a disposition; it must be an explicit line item in SPEC.md.

**G4 — No frontier research on any target language beyond JSON and CSS.** The aim names "structured-
data languages (CSS, JSON, …)". Every SOTA researcher (22–25) targets only JSON and CSS; the dig
notes non-JSON/CSS grammars exist and are fragile (09: CSS-L4 and Sheets PANIC at columns.rs:409;
13: nine hardcoded grammars). No pass-0 report tests the *generalization aspiration* against a
third structured-data target. Given JSON/CSS are the committed corpora this is a minor gap, but the
"…" in the aim is currently unbacked by evidence.

---

## Part III — Contradictions between reports (gap (b))

**C1 — serde_json number decode: 19 vs 21 (ADJUDICATED, 21 wins).** Report 19's stack-abrogation
lever "the skinny runtime delegates JSON number parsing to serde_json (latent perf ceiling), route
through fast-float2" is **misleading**: verifier 31 and report 21 establish the primary citation
(lib.rs:265) is a `#[test]` fixture, and the live parse hot path already decodes numbers hand-rolled
(`emit_number_direct`/`materialize_i64/u64/f64`, generated.rs:768–838); serde_json appears only
off-path (lazy `as_f64`, Display). The plan **must consume the corrected version**: the real lever
is 21's SIMD/SWAR number-parsing target (+45% parse_only→track1), not a phantom serde_json
dependency to excise from the hot path. If synthesis lifts 19's framing verbatim it will chase a
dependency that is not on the path it thinks it is.

**C2 — Two distinct artifacts both named "Lock 14."** Report 16's Lock-14 is a **live compiled test
gate** (`crates/ir/tests/lock14_grammar_generalization.rs`, two-crate ROSTER). Report 20's Lock-14
is a **LOCKS.md prose falsifier** (a grep of 13 crate paths, 8 phantom). Both are real, both are
near-toothless, and they are **different mechanisms**. Verifier 31 already flagged 20's location
drift (:349 not :279; "near-vacuous" not "vacuous"). Synthesis must keep the two Lock-14 artifacts
distinct or it will double-count or mis-fix the generalization gate.

**C3 — "tree absent" (11/14 verdict) vs "tree present, diff run" (18 verdict / 31).** The adversarial
note on reports 11/14 says the live diff vs csc411 was "not runnable (tree absent)," while report
18's verdict states the diff *was* run against `csc411/.../csp-solver/src` with no differences, and
31 re-derived from the canonical tree. Reconciled this pass: the tree **is** present on-host; 11/14
simply did not run the live diff (their claim was byte-count identity of `restart/corpora`), while
18 did. Not a substantive contradiction, but the "tree absent" phrasing should not be carried
forward as fact — csc411 is available and canonical HEAD has not moved past the vendor point
(b70098676), so there is no drift to reconcile, only a staged API-breaking future re-vendor.

**C4 — Report 14 "ZERO production code" (CORRECTED) and Report 22 simdjson subversion/date
(CORRECTED).** Both already adjudicated by 30/31; carried here only so synthesis uses the corrected
forms: BA–BD shipped **one** `feat(xtask)` scaffold commit (substance — no FINAL.md, no green
close — intact); simdjson's current line is 4.6.x with latest 4.6.1 (2026-04-03), not 4.6.4/2025
(transferable-lever conclusion intact).

---

## Part IV — Claims still unverified that the plan would act on, and modalities not run (gaps (c)/(d))

**U1 / MODALITY — Profiling did not run to the charter's benchmark discipline.** The standing
constraint is "cold-only, loadavg-quiet, single-invocation," and "profiling via samply (debug=true,
interactive)." Report 21 — the pass-0 profiling owner — states plainly that (i) it ran at **1-min
loadavg 10.66**, far above the 2.0 quiet threshold; (ii) **JSON was measured WARM** (profile_direct
tight-loop, output dropped), only CSS cold-per-parse, with a ~3× warm/cold gap on the same binary
(25448 vs 8349 Mbps twitter); (iii) **no cold-per-parse JSON harness exists at all**; and (iv) **no
samply flame attribution was captured** ("headless symbolization unreliable"), so every cycles/byte
split is inferred from PMU deltas + static reads, "DIRECTIONAL." Consequently *any pass-1 lever
whose priority rests on 21's numbers* — chiefly "number materialization is +45%, the SIMD/SWAR
target" and the realloc-growth argument for a SIMD one-shot presize — is standing on non-cold,
non-quiet, unsampled deltas. This is the sharpest modality gap: the charter's own benchmark
discipline was not met by the only report that produced live bbnf numbers. A re-profile under
loadavg-quiet, cold-per-parse (including a net-new cold JSON harness), with an interactive samply
flame, is a prerequisite for the plan to *rank* levers rather than merely *name* them.

**U2 — The csp-solver P0 severity is carried from csc411 docs, not re-derived.** Report 18 stages
the `ac3_from_variable` Unsatisfiable-arm "silent false-UNSAT" as the defining P0 the next re-vendor
must consume. Verifier 31/the verdict confirm **code presence + byte-identity** of the arm in both
trees, but explicitly *not* the **severity** ("silent false-UNSAT under Ac3 enumeration") — that is
carried from csc411's own tranche docs, not re-derived from bbnf's caller semantics. Since bbnf's
own CSP is proven tautological (18: solve_rule recovers its input), whether this P0 can ever fire on
a bbnf call path is unestablished. The plan should not import the P0's urgency at csc411's stated
weight without a bbnf-side derivation, or it risks prioritizing a soundness fix for a code path
bbnf never exercises.

**U3 — The contamination magnitude behind "every BEAT-sonic-rs claim is broken" is half-verified.**
Report 05's thesis (post-AP/AQ throughput came from a modifier-skipping parser) is directionally
CONFIRMED on the **post-fix** side (canada 1097 MB/s), but the **pre-fix 1810 figure was not
independently re-derived**. The direction is safe to act on (do not trust pre-AR "BEAT" numbers);
the specific 1810→1097 magnitude is not a verified quantity and should not anchor any regression
budget.

**Modalities that WERE run (for the record, so synthesis does not re-order them):** the csc411
canonical read (18 + 31), the parse-that both-trees + letter read (17), the simdjson from-source
C++ profiling on Apple silicon (22 — but that is the *competitor's* numbers, not bbnf's), and the
full 49-directory dig (01–15). These are complete; the only unrun-to-discipline modality is
bbnf-side cold+quiet+samply profiling (U1).

---

## Part V — What the synthesis must do before claiming pass-0 complete

1. Assign and execute the PRECEPTS-RECAP re-verification (G1) and the DEFERRED-LEDGER opening-census
   reconciliation (G2) — both are charter-binding opening artifacts with no current owner.
2. Put the parse-that LR/packrat GO/NO-GO (G3) on the SPEC as a dated outbound decision.
3. Consume the *corrected* serde_json framing (C1) and keep the two Lock-14 artifacts distinct (C2).
4. Re-profile bbnf cold + loadavg-quiet + interactive-samply, with a net-new cold JSON harness (U1),
   before ranking any perf lever; treat 21's current numbers as directional-only.
5. Derive the csp-solver P0 severity against bbnf caller semantics before importing its weight (U2).
6. Open the "hand-rolled ASM emitter" feasibility question explicitly (standing-aim partial gap).
</content>
</invoke>
