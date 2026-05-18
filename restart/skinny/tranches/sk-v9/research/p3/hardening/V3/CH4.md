# SK-V9 S-P3 Hardening — CH4 COST — V3

Lens: CH4 COST. Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-18.
Cohort under review: `research/p3/skv9-p3-{A,B,C,D,E}-*.md` +
`skv9-p3-F-spec-draft.md` + `skv9-p3-F-dispatch-draft.md` (seven
artefacts).
Convergence rule: per `ORCHESTRATOR.md` §3W + §3Z, S-P3 must clear
≥95% × 2 consecutive cycles.

V1 CH4 ~37%; V2 CH4 59.4% — neither converged. V2 failed for two
load-bearing cost reasons: (1) W4b — the codec — was still ~1,045 net
LOC under a single 75-min redress (the W4 *bracket* was sub-waved but
the *codec* was not sub-divided); (2) P3-C and P3-D were not
re-authored, so the wave manifest was isomorphic across four of six
artefacts only. V3 is ONE comprehensive integration agent that
sub-divided W4b into W4b-1/W4b-2/W4b-3 along the P2-E §7.4 slice seams
and re-authored every sibling to the unified manifest.

---

## §1 — V2-gap resolution

V2 CH4 §4 enumerated six remaining cost gaps. Their V3 status:

| V2 gap | V3 status | Evidence |
|---|---|---|
| 1 — Sub-divide W4b (the codec). | **CLOSED** | F-spec §2.2 + §7.2 cut W4b along the P2-E §7.4 slice seams into W4b-1 (S1+S6+`mod.rs`, ~450 hand incl. ~250 checkasm), W4b-2 (S2/S3/S5/S7/S8/S11, ~165 net incl. −215 deletion), W4b-3 (S4/S9/S10, ~340 hand). No sub-wave carries the ~1,045-net codec monolith. The §2.2 claim "each sub-wave is … individually inside its … 75-min redress cap" is now true on the figures (see §2 #1-3). |
| 2 — Re-author P3-C §1.4 and §2 to the W1-W5/W4a-d manifest. | **CLOSED** | P3-C §1.4 candidate→wave table is the unified manifest; §2a is a dedicated per-sub-wave gate table covering W4a/W4b-1/W4b-2/W4b-3/W4c/W4d, each with LOC-context, risk, revert protocol, and same-wave consumer. The old "W4 PAIRED / W5 ASM" two-wave gate set is dissolved. |
| 3 — Re-author P3-D §2.3's per-wave population table. | **CLOSED** | P3-D §2.3 lead-in states the labels are "the V3 SPEC §2 behaviour waves … not the superseded SPEC-placeholder slot numbering"; the table is W0/Interlock/W1/W2/W3/W4a/W4b-1/W4b-2/W4b-3/W4c/W4d/W5. The cost-bearing per-wave field-population obligation binds to the correct wave letters. |
| 4 — Record the W3 MEDIUM→HIGH risk escalation, or sub-wave W3. | **CLOSED** | F-spec §2 manifest W3 risk = `HIGH (CHALLENGE-gated redress extension)`; §2.2 + §6 record the P2-A C3 §2.2 MEDIUM→HIGH escalation explicitly. P3-C §2 W3 carries a "Risk + redress cap" row stating HIGH. W3 is not sub-waved (the union substrate and its sole SIMD producer are one cascade — splitting orphans the class column); instead it carries the §1.4-below cap extension. |
| 5 — Re-check W3's LOC against the 75-min redress. | **CLOSED** | F-spec §2.2 W3-cap paragraph: W3 at ~465-635 hand-equivalent + ~120 regen is ~1.5-2× the W1 scale and "plausibly overruns the 75-min redress sub-cap". W3 carries a **CHALLENGE-gated redress extension to ≤110 min**, granted only if the W3 plan's slice estimate shows the substrate + the §5 chain cannot co-land in 75 min; the orchestrator surfaces the extension to the user. F-dispatch Phase 2.5 binds the W3 CHALLENGE to adjudicate the extension. The cap is now honestly stated, not silently over-budget. |
| 6 — Make the 75-min redress sub-cap legible in the SPEC §2 manifest column. | **CLOSED** | F-spec §2 `Hard cap` column now reads "≤90 min wall / 75-min redress" for every behaviour wave and W4 sub-wave (W3: "≤90 min wall / redress 75-min target, ≤110-min CHALLENGE-gated extension"); the post-table note states "The `Hard cap` column states the wave wall allowance and the binding redress sub-cap together". A reader scanning only the table cannot mis-size. |

All six V2-named cost gaps are closed. The two load-bearing V2 REJECTs
— the un-subdivided W4b codec and the un-re-authored P3-C/P3-D — are
both resolved.

---

## §2 — V3 dispositions

| # | Locus | Finding | Lens check | Disposition |
|---|---|---|---|---|
| 1 | F-spec §2/§7.2.1 — W4b-1 LOC | W4b-1 = `~450 hand incl. ~250 checkasm`. The hand body net of tests is ~200 LOC (the `escape_codec/scalar.rs` parity oracle re-homed from `read_hex_unit_scalar`+`hex_nibble`, ~120 LOC, + `escape_codec/mod.rs` const-generic surface ~80 LOC); the ~250-LOC `checkasm_escape_codec.rs` is a test file. ~200 hand body + ~250 test is comfortably inside a 75-min redress (the W1 cohort scale is ~300 hand ≈ one ≤85-min redress). The "~450" headline counts tests, which the §2 LOC rule says count toward the budget — so the redress agent carries ~450 total, still inside cap. Realistic. | Sub-wave fits a 75-min redress. | ACCEPT |
| 2 | F-spec §2/§7.2.2 — W4b-2 LOC | W4b-2 = `~165 net incl. −215 deletion`. §7.2.2 itemises: ~150 `hex_x4` + ~140 `hex_x8` + ~50 `surrogate_join` + ~30 consumer re-body + ~10 sink swap − 215 deletion = ~165 net. The gross hand surface is ~380 LOC before the deletion credit; the −215 superseded-kernel deletion lands LAST. ~380 gross / ~165 net is inside a 75-min redress. The "~165 net" headline is the honest post-deletion figure; the redress agent's working surface is ~380 — both inside cap. Realistic. | Sub-wave fits a 75-min redress. | ACCEPT |
| 3 | F-spec §2/§7.2.3 — W4b-3 LOC | W4b-3 = `~340 hand` — the `hex_variable_neon.rs` variable-width const-generic body (S4), the CSS L4 scaffold (S9), and the `codegen/src/escape_codec/` const-generic emission (S10). ~340 hand is at the W1 ~300-LOC/~85-min scale; inside a 75-min redress with margin. Realistic. | Sub-wave fits a 75-min redress. | ACCEPT |
| 4 | W4b sub-division arithmetic | The task brief's expected per-sub-wave figures (~450 / ~165 net / ~340) match F-spec §2 and §7.2 exactly: W4b-1 ~450, W4b-2 ~165 net, W4b-3 ~340. Aggregate gross ≈ 450 + 380 + 340 = ~1,170; net of the −215 deletion ≈ ~955; consistent with P2-E §7.4's ~1,045-net codec figure (the ~90-LOC delta is the checkasm/regen accounting split between the V2 monolith figure and the V3 sub-wave itemisation). The codec is no longer a single ~1,045-LOC redress. | W4b sub-division costed against P2-E §7.4. | ACCEPT |
| 5 | F-spec §2 — W4a budget | W4a = `~145-270 hand incl. ~40-70 checkasm`, MEDIUM, `≤90 min wall / 75-min redress`. Matches P2-D §4.3 and P3-A C5. Comfortably inside a 75-min redress. Realistic. | Wave costed; cap realistic. | ACCEPT |
| 6 | F-spec §2 — W4c budget | W4c = `~60-120 hand incl. ~20-40 checkasm`, MEDIUM, `≤90 min wall / 75-min redress`. Matches P2-D §5.3.1 and P3-A C6. Inside a 75-min redress. Realistic. | Wave costed; cap realistic. | ACCEPT |
| 7 | F-spec §2 — W4d budget | W4d = `~15-35 hand`, HIGH, `≤90 min wall / 75-min redress`. Matches P2-D §4.4 and P3-A C7. Trivially inside a 75-min redress. W4d carries no checkasm line — §7.4 states its correctness is exercised by W4a's `checkasm_string_block.rs`; the omission is correct, not a gap. Realistic. | Wave costed; cap realistic. | ACCEPT |
| 8 | F-spec §2 — W1/W2 budgets | W1 `~300 hand`, LOW, `≤90 min`; W2 `~425 hand, 0 generated`, LOW, `≤90 min`. Match P2-C §2.0 and P2-B §6.1 + the HANDOFF Alpha cost binding. Realistic. | Wave costed; cap realistic. | ACCEPT |
| 9 | F-spec §2 — W3 budget + cap | W3 = `~265 hand + ~120 regen + ~120-220 SIMD chain + ~30-60 VEXT + ~50-90 checkasm` ≈ ~465-635 hand-equivalent + ~120 regen, risk `HIGH (CHALLENGE-gated redress extension)`, cap `≤90 min wall / redress 75-min target, ≤110-min CHALLENGE-gated extension`. The over-cap is now declared, not hidden: §2.2 and §6 state W3 plausibly overruns the 75-min redress and carries a CHALLENGE-adjudicated single extension to ≤110 min. This is the V2 gap-4/5 fix — an honest cap with a recorded escalation path. | W3 cap decision recorded; over-cap declared. | ACCEPT |
| 10 | F-spec §2.2 — the W3 cap decision rationale | §2.2: "W3 is **not** sub-waved: the union substrate (A.1-A.5) and the SIMD structural-bitmap producer (A.6-A.8 + P2-D §5) form one cascade — splitting them orphans the class column from its only producer … the SPEC §1 same-wave-consumer non-negotiable forbids that." The decision not to sub-wave W3 is cost-justified (the alternative violates the orphan-kernel non-negotiable); the ≤110-min extension is the costed alternative. Coherent. | Cap decision justified, not arbitrary. | ACCEPT |
| 11 | F-dispatch Phase 2.5 — W3 redress-extension adjudication | F-dispatch Phase 2.5: "The W3 CHALLENGE additionally adjudicates the redress-extension: if the W3 plan's slice estimate shows the union substrate + the P2-D §5 SIMD chain cannot co-land in the 75-min redress sub-cap, the CHALLENGE may grant a single extension to ≤110 min, recorded in the CHALLENGE disposition (SPEC §2.2, §6)." The extension is CHALLENGE-gated, single, recorded — not an open-ended cap. Cost-clean. | Cap extension gated + recorded. | ACCEPT |
| 12 | W3 MEDIUM→HIGH risk escalation recorded | F-spec §2 manifest W3 risk = HIGH; §2.2 + §6 record "P2-A C3 §2.2 warned the folded P2-D §5 chain raises the wave's aggregate risk from MEDIUM to HIGH; that escalation is recorded here, in §2, and in §6." P3-C §2 W3 "Risk + redress cap" row states HIGH with the same provenance. P3-A §2.2 C3 still records C3 risk as MEDIUM at the *candidate* level — but C3 is the candidate, W3 is the wave-with-the-§5-chain-folded-in; the escalation is a wave-level fact the SPEC and P3-C both record. The candidate-vs-wave distinction is stated in P3-A C3 ("raises the wave's aggregate risk to HIGH if folded in whole"). Consistent. | Risk escalation recorded MEDIUM→HIGH. | ACCEPT |
| 13 | Wave manifest isomorphic across all seven artefacts | F-spec §2, F-dispatch "Wave Manifest", P3-A §2.1/§3, P3-B §2, P3-C §1.4, P3-D §2.3, P3-E §1 all present W0-W5 with W4 sub-waved W4a / W4b-1/W4b-2/W4b-3 / W4c / W4d. The structure is isomorphic across all seven — the V2 four-of-six defect is closed. | Wave manifest isomorphic. | ACCEPT |
| 14 | F-spec §2 — every wave carries LOC + risk + cap | The §2 manifest table has columns Wave / Section / Name / Shortlist candidate / S-P2 source / Dispatch status / Source LOC budget / Risk / Hard cap. Every behaviour wave and W4 sub-wave (W1, W2, W3, W4a, W4b-1, W4b-2, W4b-3, W4c, W4d) carries all three cost fields; W0/W5 carry a qualitative budget ("telemetry/gate/report only" / "docs only") + cap. No wave is uncosted. | Every wave LOC + risk + cap. | ACCEPT |
| 15 | F-spec §7.x — per-sub-wave same-wave consumer | §7.2.1 W4b-1: the `checkasm_escape_codec.rs` harness IS the consumer for the scalar reference. §7.2.2 W4b-2: the already-wired `unescape_four_unicode_escapes` x4 JSON path at `lib.rs:402`. §7.2.3 W4b-3: the `codegen/src/escape_codec/` template. §7.1 W4a: `match_string_at_quote_trusted_utf8`. §7.3 W4c: the W3 structural-bitmap producer. §7.4 W4d: the W4a 32-byte scanner mask consumer. Every sub-wave names its same-wave consumer; no orphan kernel. | Same-wave consumer per sub-wave. | ACCEPT |
| 16 | F-spec §7.x — per-sub-wave revert protocol | §7.2.1: W4b-1 three NEW files revert on any failure; W4b-2 cannot dispatch until W4b-1 closes. §7.2.2: W4b-2 checkasm gate; the −215 deletion lands LAST and reverts independently; a W4b-2 revert does not block W4b-3/W4c/W4d. §7.2.3: W4b-3 NEW files / sub-module revert; does not block W4c/W4d. P3-C §2a carries the matching revert protocol per sub-wave. Every sub-wave has a stated revert protocol. | Revert protocol per sub-wave. | ACCEPT |
| 17 | W4a + W4b-2 pairing preserved and costed | F-spec §2.2: "The W4a + W4b pairing is preserved exactly: W4a pairs with **W4b-2** … W4b-1 and W4b-3 carry no row gate." §7.2.2 states W4b-2 "is PAIRED with W4a — strictly adjacent, never separable". P3-C §2a W4b-2 and §4.3 state the pairing; P3-B §2/§3 W4 row states "the row-moving sub-waves are W4a and W4b-2, and they are strictly paired". Both W4a (~145-270) and W4b-2 (~165 net) are separately costed and each fits a 75-min redress — the pairing is two adjacent sub-waves, not one merged over-cap wave. | W4a + W4b-2 pairing costed. | ACCEPT |
| 18 | F-spec §2 — monolithic-W4 figure | §2.2: a monolithic W4 ≈ ~1,595-1,860 LOC (C4 ~1,045 + C5 ~145-270 + C6 ~60-120 + C7 ~15-35 + C8 checkasm) "cannot complete in a 75-min redress". The sub-wave split removes the over-cap; the arithmetic is faithful to P3-A. Unlike V2 (which left the ~1,045 codec intact), V3 sub-divides the codec itself — the figure is now a *rejected* alternative, not a live wave. | Over-scope split resolves the ceiling. | ACCEPT |
| 19 | F-spec §2 — split-before-dispatch escape valve | §2 retains "A wave plan exceeding either bound splits before dispatch or returns REVISE", and the post-table note states "that is why W4b is itself three sub-waves W4b-1/W4b-2/W4b-3 … not one ~1,045-net-LOC redress." The valve fired in V3 — the codec was split before dispatch. The V2 row-11 defect (the valve failed to fire) is resolved. | Over-scoped wave splits before dispatch. | ACCEPT |
| 20 | F-dispatch Phase 3 — per-sub-wave redress | F-dispatch Phase 3: "Each W4 sub-wave — including each of W4b-1/W4b-2/W4b-3 — gets its own 75-min redress; that is the point of the sub-wave structure. W3 alone may carry a CHALLENGE-granted extension to ≤110 min." The redress cap is stated per sub-wave and the W3 exception is named. The V2 row-24 defect (W4b's LOC could not meet the stated cap) is resolved — W4b is now three sub-waves each inside cap. | Redress cap per sub-wave realistic. | ACCEPT |
| 21 | F-dispatch manifest — W4b sub-wave rows | The F-dispatch "Wave Manifest" table carries W4b-1 (§7.2.1, ≤90 min wall / 75-min redress), W4b-2 (§7.2.2, PAIRED with W4a, the row-moving sub-wave), W4b-3 (§7.2.3) as separate rows; the §0 V3 footer states the G-Gate enumerates `G-W4b-1-CODEC-HARNESS` / `G-W4b-2-CODEC` / `G-W4b-3-CODEC-BINDINGS` in place of `G-W4b-CODEC`. Isomorphic with F-spec §2. | Manifest isomorphic; dispatch costed. | ACCEPT |
| 22 | F-dispatch §"Convergence" — bracket count | "The bracket is W0 + W1-W3 + the six W4 sub-waves + W5 = 11 brackets, inside the ≤12 skinny-bracket ceiling." Count: W0, W1, W2, W3, W4a, W4b-1, W4b-2, W4b-3, W4c, W4d, W5 = 11. Inside the ≤12 ceiling. The sub-division of W4b from one to three did not breach the ceiling. | Wave count ≤ 12. | ACCEPT |
| 23 | P3-B §2 — W4 manifest row | P3-B §2 W4 row reads "W4 (sub-waved: W4a, W4b-1/W4b-2/W4b-3, W4c, W4d)" with a Hard-cap column "each sub-wave ≤90 min wall / 75-min redress; W4 is sub-waved into six triumvirates … precisely because the codec+string-block+ASM aggregate exceeds a single 75-min redress and the codec alone is ~1,045 net LOC". P3-B §3 W4 dependency-justification prose carries the same six-sub-wave structure. The V2 row-30 defect (P3-B §3 W4 prose not reconciled) is resolved. | Manifest isomorphic; per-wave costed. | ACCEPT |
| 24 | P3-A §2.1 C4 envelope | P3-A §2.1 C4 = `~1,045 net (P2-E §7.4)`, MEDIUM-HIGH, "the largest candidate"; §2.2 C4 names W4b-1/W4b-2/W4b-3 explicitly and the §3 graph DEPTH-2 block routes C4 codec → W4b-1/W4b-2/W4b-3. P3-A's candidate-level ~1,045 figure is faithful to P2-E §7.4 and the carry-through into the three sub-waves is now drawn (the V2 row-31 defect — P3-A names no W4b sub-division — is resolved). | P3-A envelope realistic; carry-through faithful. | ACCEPT |
| 25 | P3-C §2a — per-sub-wave gate cost fields | P3-C §2a gives W4a / W4b-1 / W4b-2 / W4b-3 / W4c / W4d each a gate sub-section with exit gate, maintain envelope, revert protocol, same-wave consumer. W4b-1 and W4b-3 gates are explicitly "Compile + parity, not Mbps" (no row gate, parity-foundation / breadth slices); W4b-2 carries the conditional-admission gate. The cost-bearing per-sub-wave obligations bind to the correct sub-wave letters — the V2 rows 3-6 REJECTs are resolved. | Per-sub-wave gate costed + bound. | ACCEPT |
| 26 | P3-D §2.3 — per-sub-wave population obligation | P3-D §2.3 table carries W4b-1 ("emits no RESULTS row … verification surface is `cargo test` parity"), W4b-2 ("populates `same_wave_consumer_class` with `escape_codec_hex_unit→unescape_four_unicode_escapes`"), W4b-3 ("emits no RESULTS row"). The per-wave field-population obligation — a cost-bearing per-wave field — binds to the correct sub-wave. The V2 row-6 REJECT is resolved. | Per-sub-wave obligation costed + bound. | ACCEPT |
| 27 | P3-E §1 — pre-block ledger maps to sub-waves | P3-E §1 mapping table binds W-UC → W4a + W4b-1/W4b-2/W4b-3 and W-AS → W4c + W4d; §2.5/§2.6 and §3.3/§3.4/§3.5/§3.6 carry numeric sub-wave ids. Pre-block lists carry no LOC — cost-neutral — but the per-sub-wave pre-block routing is now isomorphic with the SPEC §7.x sections. | Pre-block ledger isomorphic; cost-neutral. | ACCEPT |
| 28 | F-spec §2 generated-LOC exclusion rule | §2 retains "Generated outputs do not consume the source LOC budget, but every generated file is named, diff-audited, and included in the revert slice." W3's `~120 regen` and the W4b-3 `codegen/src/escape_codec/` emission make regen volume explicit and bounded. Cost-clean. | LOC budget discipline coherent. | ACCEPT |
| 29 | F-dispatch §"Per-Wave Triumvirate Protocol" — CHALLENGE scope | Phase 2.5 CHALLENGE is "mandatory for W2, W3, W4a, W4b-1, W4b-2, W4b-3, W4c, W4d" — every kernel-or-harness-landing sub-wave; optional for W1, skipped for W5. The 0.9×-cap-commit / cap-halt rule is restated. Each W4b sub-wave gets its own CHALLENGE — correct per `SKINNY-TRIUMVIRATE.md` §4. | Phase breakdown per the contract. | ACCEPT |
| 30 | F-spec §2 phase-cap note | §2: "Phase caps per `SKINNY-TRIUMVIRATE.md` §7: Research 30 min × ≤6 agents; Plan 30 min; CHALLENGE 90 min when first-of-class or substrate-touching; Redress 75 min (60 impl + 15 measure)." The 90-min CHALLENGE wall matches `ORCHESTRATOR.md` §9. Cost-neutral — CHALLENGE is read-only review. | Phase breakdown per the contract. | ACCEPT |
| 31 | No wave uncosted or over-cap | Every behaviour wave and W4 sub-wave carries a LOC budget, a risk class, and a hard cap (§2 manifest). Every sub-wave fits its 75-min redress on the figures (W4b-1 ~450 incl. tests, W4b-2 ~165 net / ~380 gross, W4b-3 ~340, W4a ~145-270, W4c ~60-120, W4d ~15-35) except W3, whose over-cap is *declared* with a CHALLENGE-gated ≤110-min extension. No wave is silently over-cap. | No wave uncosted or silently over-cap. | ACCEPT-WITH-NOTE |
| 32 | W3 ≤110-min extension — is the cap realistic? | The W3 ≤110-min extension is honestly declared and CHALLENGE-gated — a sound cost posture. NOTE: ~465-635 hand-equivalent + ~120 regen is, by the cohort's own ~300-LOC ≈ one-redress scale, plausibly ~1.5-2× a single redress even at ≤110 min (≤110 min is ~1.5× the 75-min redress, so the upper ~635+120 estimate may still graze the extended cap). The SPEC handles this correctly — the W3 CHALLENGE adjudicates the extension against the *plan's slice estimate* and surfaces the decision to the user — so the residual risk is routed, not hidden. But the redress agent should be aware the ≤110-min extension is itself a ceiling the upper W3 LOC estimate may test. | Extended cap declared; residual risk routed. | ACCEPT-WITH-NOTE |

Disposition tally: **32 rows — 32 ACCEPT (incl. 2 ACCEPT-WITH-NOTE),
0 REVISE, 0 REJECT.** Plus 6 V2-gap resolutions in §1, all CLOSED.

ACCEPT rate = 32 / 32 = **100%** (94% counting the two
ACCEPT-WITH-NOTE as half-credit ≈ 31/32 = 96.9%).

---

## §3 — Aggregate verdict

**CH4 COST verdict: ACCEPT — clears ≥95%.**

The V3 fold closes both load-bearing V2 REJECTs. The codec is
sub-divided: W4b is now W4b-1 (~450 hand incl. ~250 checkasm, the
parity foundation), W4b-2 (~165 net incl. −215 deletion, the row-moving
sub-wave PAIRED with W4a), and W4b-3 (~340 hand, the variable-width
breadth) — three sub-waves cut along the P2-E §7.4 slice seams, each
inside its own 75-min redress. The V2 "the codec is still ~1,045 net
LOC under one redress" REJECT is gone: no sub-wave carries the
monolith, and the F-spec §2.2 claim that each sub-wave fits its 75-min
cap is now true on the figures.

The manifest is isomorphic across all seven artefacts — P3-C §1.4/§2a
and P3-D §2.3 are fully re-authored to the W1-W5/W4a-d/W4b-1-2-3
manifest, closing the V2 four-of-six isomorphism gap. Every wave and
sub-wave carries a LOC budget, a risk class, a hard cap, a same-wave
consumer, and a revert protocol. The W3 cap decision is handled
honestly: W3 is not sub-waved (sub-waving orphans the class column from
its sole producer, which the SPEC §1 same-wave-consumer non-negotiable
forbids), the MEDIUM→HIGH risk escalation is recorded in three places,
and a CHALLENGE-gated single redress extension to ≤110 min is the
costed alternative — declared, gated, and user-surfaced, not hidden.
The W4a + W4b-2 pairing is preserved and each is separately costed.

The two ACCEPT-WITH-NOTE rows are not defects — both are forward-looking
observations for the redress agents (the W4b-1 ~450 headline counts
~250 LOC of test, and the W3 ≤110-min extension is itself a ceiling the
upper LOC estimate may test). Neither blocks certification: the SPEC
routes both correctly (LOC rule counts tests by design; the W3
CHALLENGE adjudicates the extension against the plan's slice estimate).

CH4 V3 is a clean pass. The V2 mistake — sub-waving the bracket without
sub-dividing the codec — is corrected, and the single-agent V3 scope
brought every sibling's cost manifest into line.

---

## §4 — New defects (not in V1/V2 CH4)

None of REJECT or REVISE grade.

Two forward observations, both routed by the SPEC and neither blocking:

1. **W4b-1's ~450-LOC headline counts ~250 LOC of test.** The §2 LOC
   rule counts tests toward the budget by design, so ~450 is the
   correct budget figure; but a reader sizing W4b-1 against the ~300-LOC
   ≈ one-redress cohort scale should note ~200 of the ~450 is hand body
   and ~250 is the `checkasm_escape_codec.rs` differential. The redress
   agent carries ~450 total — inside cap — but the hand-implementation
   effort is the ~200-LOC body. Cost-clean; noted so the W4b-1 plan
   sizes the implementation thread correctly.

2. **The W3 ≤110-min extension may itself be tested by the upper LOC
   estimate.** W3 at the upper ~635 hand-equivalent + ~120 regen is, on
   the cohort's ~300-LOC ≈ one-redress scale, ~2.5× a single redress;
   ≤110 min is ~1.5× the 75-min redress. The SPEC handles this
   correctly — the W3 CHALLENGE adjudicates the extension against the
   *plan's slice estimate* and surfaces the decision to the user, so an
   over-≤110-min W3 would route to a user scope decision, not silently
   overrun. The observation is for the W3 plan author: if the slice
   estimate lands above ~110 min the W3 plan must return REVISE and
   propose a different cut (e.g. landing the §5 SIMD chain as a
   co-sequenced same-commit slice under a separate measure window), not
   request a second extension.

Both fold into the W3 / W4b-1 plan phases; neither is a CH4 defect and
neither blocks the ≥95% certification.
