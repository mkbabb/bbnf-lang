# SK-V9 S-P3 CHALLENGE — CH4 COST (V2 verify)

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V2. Lens: CH4 COST.
Date: 2026-05-18.
Scope: Adversarial cost-discipline verify of the SK-V9 S-P3 V2 fold —
the seven P3 artefacts at `restart/skinny/tranches/sk-v9/research/p3/`
after commit `ef40c0fc docs(sk-v9-p3-v2): integrate P3-A..E into the
SPEC + DISPATCH drafts`. V1 CH4 returned **REJECT (~37%)**: a
non-isomorphic wave manifest (three disagreeing artefacts), a
~1,600-1,860-LOC monolithic W4 over the 75-min redress ceiling, and a
codec under-budgeted ~4×. The V2 fold re-authored the P3-F SPEC +
DISPATCH drafts with W4 sub-waved (W4a-d) and per-wave LOC/risk/cap
reconciled. CH4 V2 verifies the eight V1 cost gaps closed.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

---

## §1 — V1-gap resolution (the 8 gaps)

V1 CH4 §4 enumerated eight gaps that had to close before ACCEPT. Their
V2 status:

| # | V1 gap | V2 status | Evidence |
|---|---|---|---|
| 1 | Unify the wave manifest across P3-A/B/C/F (three disagreeing manifests). | **PARTIAL** | P3-A §2.2 / P3-B §2 / P3-F SPEC §2 / P3-F DISPATCH are now isomorphic on a single W1-W5 sequence with W4 sub-waved W4a-d. **But P3-C §1.4/§2 and P3-D §2.3 were NOT re-authored** — P3-C still gates a W4=paired / W5=kernels two-wave manifest; P3-D §2.3's per-wave field-population table still uses the old SPEC-placeholder numbering (W1=release, W2=typed, W3=tape, W4=direct). The cohort is isomorphic across four of six artefacts, not six. |
| 2 | Re-budget or split W5 (the codec). | **NOT CLOSED** | The codec is now W4b. P3-F SPEC §7.2 budgets W4b at **`~1,045 net incl. ~250 checkasm`** with a **`≤90 min` hard cap**. This is the V1 figure verbatim — the codec was sub-*waved* out of the monolith but was **not itself sub-divided**. An ~1,045-net-LOC, eleven-slice, ~6.0 h-per-slice-aggregate intervention (P2-E §7.4) still cannot complete in a 75-min redress. The wave bracket changed; the wave's own over-cap did not. |
| 3 | Reconcile the P2-D §5 structural-bitmap chain into a budgeted wave. | **CLOSED** | P3-F SPEC §2 W3 budget is now `~265 hand + ~120 regen + ~120-220 SIMD chain + ~50-90 checkasm`; §6 prose and the A.6/A.8 owner rows fold the chain in explicitly. P3-A §2.2's reconciliation question is answered: the chain lands in W3. |
| 4 | Add a risk-class column to the P3-F SPEC §2 manifest. | **CLOSED** | P3-F SPEC §2 manifest now carries a `Risk` column: W1 LOW, W2 LOW, W3 MEDIUM, W4a MEDIUM, W4b MEDIUM-HIGH, W4c MEDIUM, W4d HIGH. Carried from P3-A §2.1. |
| 5 | Add a hard-cap column to the P3-B §2 wave-manifest table. | **CLOSED** | P3-B §2's table already carries (and V1 row 20 missed, or V2 added) a `Hard cap (per-wave triumvirate)` column with the full phase breakdown per wave. |
| 6 | Budget the C8 checkasm LOC explicitly. | **CLOSED** | Every P3-F SPEC wave landing a primitive now names its checkasm allowance inline: W3 `~50-90 checkasm`, W4a `incl. ~40-70 checkasm`, W4b `incl. ~250 checkasm`, W4c `incl. ~20-40 checkasm`. C8's ~330-390 LOC is distributed and visible. |
| 7 | Resolve the 75-min vs 90-min redress-cap contradiction. | **PARTIALLY CLOSED** | P3-F SPEC §2's manifest note now reads "CHALLENGE 90 min …; Redress 75 min (60 impl + 15 measure)" and §2.2 / DISPATCH Phase 3 state "75 min cap" explicitly. The `≤90 min` manifest column is now legible as the wave-redress *wall allowance*, not the redress phase. The 90/75 split is no longer self-contradictory — but the manifest column header still reads `Hard cap` `≤90 min` with no inline note that the binding redress sub-cap is 75, so a reader scanning only the table can still mis-size. |
| 8 | Reconcile the cascade-lock against the redress ceiling. | **CLOSED (mechanism) — but see gap 2** | P3-F SPEC §2.2 disambiguates P2-D §0: "may not be split" means a P2-D kernel must not land *without the W3 union substrate existing*, satisfied by W3 preceding W4a-d — NOT one monolithic redress. The three "same-wave" relations are named distinctly. The cascade-lock no longer mandates an over-cap wave **for W4a/W4c/W4d**. W4b remains over-cap for the independent reason in gap 2 (its own LOC, not the cascade). |

**Six of eight closed or substantially closed. Two remain open: gap 1
(P3-C + P3-D not re-authored — partial isomorphism) and gap 2 (W4b
codec still ~1,045 LOC under a single 75-min redress).** Gap 2 is the
load-bearing V1 REJECT and it is not resolved.

---

## §2 — V2 dispositions

| # | Locus | Finding | Lens check | Disposition |
|---|---|---|---|---|
| 1 | P3-F SPEC §2 manifest | The manifest now carries Wave / Section / Name / Candidate / S-P2 source / Dispatch status / **Source LOC budget** / **Risk** / **Hard cap** — nine columns. Every behaviour wave and sub-wave (W1, W2, W3, W4a-d, W5) carries all five mandatory cost fields. The V1 missing risk column is restored. | Every wave carries LOC budget + risk + hard cap. | ACCEPT |
| 2 | P3-F SPEC §2 / DISPATCH §"Wave Manifest" / P3-A §2.2 / P3-B §2 — manifest agreement | All four artefacts now manifest one W1-W5 sequence with W4 sub-waved W4a-d. P3-F SPEC §2 ≡ P3-F DISPATCH ≡ P3-B §2 ≡ P3-A's wave dispositions (C6→W4c, C7→W4d named in P3-A §2.2). The three-disagreeing-manifest V1 defect is resolved **across these four**. | Wave manifest isomorphic across the cohort. | REVISE |
| 3 | P3-C §1.4 candidate→wave map | P3-C §1.4's table still reads "W4 — P2-E codec + P2-D §4 string-block widening (paired)" and "W5 — P2-D §5 aarch64 ASM kernels (EOR3 ladder, CSSC CTZ, structural-bitmap)". This is the **V1 two-wave manifest verbatim** — P3-C was not re-authored to the W4a-d sub-wave structure. The V1 CH4 row 25 REJECT ("the P3-C W5 gate gates a different wave than the SPEC manifests") is **NOT closed**. | Wave manifest isomorphic across the cohort. | REJECT |
| 4 | P3-C §2 per-wave gate tables — wave letters | P3-C §2 still carries gate tables headed "W4 — Unicode codec + string-block widening, PAIRED" and "W5 — aarch64 ASM substrate kernels". The P3-F SPEC has no W4 (only W4a-d) and its W5 is the close wave. P3-C's W4 gate has no SPEC wave; the SPEC's W4a/W4b/W4c/W4d sub-waves have **no dedicated P3-C gate table** (their gates live only in the SPEC §7.x sections). The falsifiability-gate artefact and the SPEC manifest are non-isomorphic. | Every wave carries a matching gate. | REJECT |
| 5 | P3-C §2 W5 gate | P3-C §2 "W5" gates the ASM kernels with exit rows `gsoc-2018 ≥ 41198`, `github_events ≥ 19418`, `random ≥ 13788`. The SPEC W5 is docs-only close — it has no exit rows. The P3-C W5 ASM-kernel gate now corresponds to SPEC §7.3 (W4c) + §7.4 (W4d), but P3-C never re-binds. The gate content survives; the wave letter is stale. | Every wave carries a matching gate. | REJECT |
| 6 | P3-D §2.3 per-wave population table | P3-D §2.3 labels the waves "W1 (revised S-P2/S-P3 release)", "W2 (Apache/CITM typed admission)", "W3 (tape + structural projection)", "W4 (direct contract)", "W5 (close)" — the old SPEC-placeholder slot numbering. The P3-F SPEC has W1=Apache/CITM, W2=proof, W3=union, W4a-d=consumers. The per-wave field-population obligation — a cost-bearing per-wave field — binds to the wrong wave letters. P3-D was not re-authored. | Wave manifest isomorphic; per-wave obligation costed. | REJECT |
| 7 | P3-E §2.x wave labels | P3-E uses abstract labels W-AC / W-RG / W-UE / W-UC / W-AS — letter-agnostic, so they carry no W1-W5 conflict. But §2.5 W-UC bundles "unicode codec + string-block widening" and §2.6 W-AS bundles "SHA3 EOR3, CSSC CTZ" — i.e. P3-E still treats them as two waves, not four sub-waves. The per-wave pre-blocked-route lists are correctly carried into the SPEC §7.x sub-wave sections (verified — P3-F §7.1-§7.4 cite P3-E §2.5/§3.5, §2.5/§3.4, §2.6/§3.3, §2.6/§3.6), so the *content* is isomorphic even though P3-E's own sectioning is two-wave. Cost-neutral — pre-block lists carry no LOC. | Pre-block ledger carried into wave sections. | ACCEPT |
| 8 | P3-F SPEC §7.2 W4b LOC budget | W4b budgeted `~1,045 net incl. ~250 checkasm` — the unmodified P2-E §7.4 figure (~890 hand + ~120 regen + ~250 tests − 215 deletion, eleven slices, ~6.0 h aggregate per-slice cap). | P3-A LOC envelope carried faithfully. | ACCEPT |
| 9 | P3-F SPEC §7.2 W4b hard cap | W4b carries a `≤90 min` hard cap. The binding redress sub-cap is 75 min (`SKINNY-TRIUMVIRATE.md` §7, restated in SPEC §2.2 + DISPATCH Phase 3). An ~1,045-net-LOC, eleven-slice intervention with a ~6.0 h aggregate per-slice cap cannot land in a 75-min redress — the over-cap is ~4.8×. The V1 REJECT rows 6/7 are **not resolved**: W4 was sub-waved, but W4b *is* the whole codec; sub-waving the bracket did not sub-divide the codec. | Wave hard cap realistic for the 75-min redress phase. | REJECT |
| 10 | P3-F SPEC §2.2 — the sub-wave rationale | §2.2 states "The W4 sub-wave structure exists precisely so that no redress overruns the 75-min ceiling" and "each W4x is … individually inside its LOC budget and its 75-min redress cap." For W4a (~145-270), W4c (~60-120), W4d (~15-35) this is true. For **W4b (~1,045 net)** the claim is false on its own figures — §2.2's stated guarantee is contradicted by §7.2's own budget. The SPEC asserts a property the W4b row violates. | Sub-wave structure resolves the 75-min ceiling. | REJECT |
| 11 | P3-F SPEC §2 "split-before-dispatch" escape valve | §2 retains "A wave plan exceeding either bound splits before dispatch or returns REVISE." W4b's `~1,045 net` exceeds any realistic 75-min-redress LOC envelope (W1 ~300/≤85 min redress sets the cohort's own scale: ~300 LOC ≈ one redress). By the SPEC's own conjunctive rule W4b should have split into W4b-1..W4b-n before dispatch — the valve did not fire in P3-F V2, exactly as V1 row 16 flagged it failed to fire in V1. | Over-scoped wave splits before dispatch. | REVISE |
| 12 | P3-F SPEC §2 W3 budget | W3 = `~265 hand + ~120 regen + ~120-220 SIMD chain + ~30-60 VEXT + ~50-90 checkasm` ≈ ~465-635 hand-equivalent + ~120 regen. The P2-D §5 dead-scanner structural-bitmap chain (V1 gap 5 / rows 11-12) is now folded in and costed explicitly. P3-A §2.2's reconciliation request is answered. | P2-D §5 dead-scanner chain folded into W3 budget. | ACCEPT |
| 13 | P3-F SPEC §6 W3 — risk grade vs folded chain | P3-A C3 §2.2 warned the §5 chain "raises the wave's aggregate risk to HIGH if folded in whole." P3-F SPEC §2 records W3 risk as **MEDIUM**, and §6 prose attributes MEDIUM to "the contracting mechanism … the codegen-template structural-walk lowering is the novel surface" — it does not record the P2-A-warned MEDIUM→HIGH escalation now that the chain is folded. The risk grade may understate the wave. | Risk class realistic for the as-budgeted wave. | REVISE |
| 14 | P3-F SPEC §6 W3 LOC vs 75-min redress | W3 at ~465-635 hand-equivalent + ~120 regen, eight P2-A slices + the P2-D §5 chain, under a `≤90 min` cap / 75-min redress. This is ~1.5-2× the W1 ~300-LOC/~85-min-redress scale. W3 is plausibly over the 75-min redress ceiling once the §5 SIMD chain is folded — the SPEC neither flags this nor sub-waves W3. Less egregious than W4b but the same class of defect. | Wave hard cap realistic for the redress phase. | REVISE |
| 15 | P3-F SPEC §2 W1 budget | `~300 hand`, LOW, `≤90 min`, `≤85-min redress estimate` (§4). Matches P2-C §2.0 and the HANDOFF Alpha cost binding. Mechanical, no kernel; CHALLENGE optional. Realistic. | P3-A envelope carried; cap realistic. | ACCEPT |
| 16 | P3-F SPEC §2 W2 budget | `~425 hand, 0 generated`, LOW, `≤90 min`. Matches P2-B §6.1 (~425 aggregate per-slice cap) and the HANDOFF 450-LOC envelope; five small NEW-file slices, proof-only. Realistic. | P3-A envelope carried; cap realistic. | ACCEPT |
| 17 | P3-F SPEC §2 W4a budget | `~145-270 hand incl. ~40-70 checkasm`, MEDIUM, `≤90 min`. Matches P2-D §4.3 and P3-A C5. Comfortably inside a 75-min redress. Realistic. | P3-A envelope carried; cap realistic. | ACCEPT |
| 18 | P3-F SPEC §2 W4c budget | `~60-120 hand incl. ~20-40 checkasm`, MEDIUM, `≤90 min`. Matches P2-D §5.3.1 and P3-A C6. Inside a 75-min redress. Realistic; the EOR3 ladder is a small capability-gated specialisation. | P3-A envelope carried; cap realistic. | ACCEPT |
| 19 | P3-F SPEC §2 W4d budget | `~15-35 hand`, HIGH, `≤90 min`. Matches P2-D §4.4 and P3-A C7 (the smallest candidate). Trivially inside a 75-min redress. Realistic. | P3-A envelope carried; cap realistic. | ACCEPT |
| 20 | P3-F SPEC §2 W4d — no checkasm line | W4d carries no checkasm allowance because §7.4 states "W4d's correctness is exercised by W4a's `checkasm_string_block.rs` … no separate checkasm file." Cost-clean — the CTZ extract is a sub-step of the 32-byte block scanner; its differential is W4a's. The omission is correct, not a gap. | Checkasm budgeted (or correctly waived). | ACCEPT |
| 21 | P3-F SPEC §2 generated-LOC exclusion rule | §2 retains "Generated outputs do not consume the source LOC budget, but every generated file is named, diff-audited, and included in the revert slice." The W3 `~120 regen` and W4b `~120 regen` columns make regen volume explicit and bounded. Cost-clean. | LOC budget discipline coherent. | ACCEPT |
| 22 | P3-F SPEC §2.2 — monolithic-W4 figure | §2.2 states a monolithic W4 would be ~1,595-1,860 LOC (C4 ~1,045 + C5 ~145-270 + C6 ~60-120 + C7 ~15-35 + C8 checkasm) and "cannot complete in a 75-min redress." The arithmetic is faithful to P3-A. The sub-wave split correctly removes ~550-815 LOC (W4a+W4c+W4d) of cascade-locked surface from the over-cap monolith — but leaves the ~1,045-LOC W4b core intact and still over-cap. | Over-scope split resolves the ceiling. | REVISE |
| 23 | P3-F SPEC §2 phase-cap note | §2 cites "Research 30 min × ≤6 agents; Plan 30 min; CHALLENGE 90 min …; Redress 75 min (60 impl + 15 measure)" — `SKINNY-TRIUMVIRATE.md` §7 verbatim except the CHALLENGE wall (§7 says 60 min wall; SPEC says 90 min, matching `ORCHESTRATOR.md` §9's "~90 min wall" for the CHALLENGE wave). The 90 vs 60 is a contract-internal discrepancy upstream of S-P3; the SPEC picks the ORCHESTRATOR figure. Cost-neutral — CHALLENGE is read-only review, not LOC-bearing. | Phase breakdown per the contract. | ACCEPT |
| 24 | P3-F DISPATCH Phase 3 | DISPATCH "Phase 3 — Redress" states "75 min cap (60 impl + 15 measure). Each W4 sub-wave gets its own 75-min redress — that is the point of the sub-wave structure." The redress cap is stated correctly and per-sub-wave. The defect is not the stated cap — it is that W4b's LOC cannot meet it. | Phase breakdown realistic. | REVISE |
| 25 | P3-F DISPATCH §"Per-Wave Triumvirate Protocol" | The 0.9×-cap-commit / cap-halt rule is restated from `ORCHESTRATOR.md` §9; CHALLENGE mandatory for W2/W3/W4a-d, optional for W1, skipped for W5 — correct per `SKINNY-TRIUMVIRATE.md` §4. Phase breakdown contract-faithful. | Phase breakdown per the contract. | ACCEPT |
| 26 | P3-F SPEC §2 W5 / W0 | W0 `telemetry/gate/report only`, W5 `docs only`, both `≤90 min`, both risk `—`. Non-source waves correctly carry a qualitative budget + cap. Realistic for a five-document reconcile. | Every wave carries a budget + cap. | ACCEPT |
| 27 | P3-A §2.1 shortlist count | Eight candidates C1-C8, exactly at the `PASS-3-SYNTHESIS-PLAN.md` §2 ≤8 ceiling; C6/C7/C8 correctly non-standalone. P3-A §2.2 now adds explicit W4c/W4d wave dispositions for C6/C7 (the V1 silent-drop the consolidation F-AUX item flagged). | Shortlist ≤ 8; no candidate orphaned. | ACCEPT |
| 28 | Wave count | W1-W5 with W4 sub-waved W4a-d = 8 behaviour brackets + W0 = 9 total; well inside the ≤12 skinny-bracket ceiling (`ORCHESTRATOR.md` §3 / `SKINNY-TRIUMVIRATE.md`). The sub-wave split does not breach the ceiling. | Wave count ≤ 12. | ACCEPT |
| 29 | P3-F SPEC §1 / §7.2 — same-wave consumer per W4 sub-wave | Every W4 sub-wave names its same-wave consumer wired same-commit: W4a → `match_string_at_quote_trusted_utf8`; W4b → the x4 JSON path at `lib.rs:402`; W4c → the W3 structural-bitmap producer; W4d → the W4a 32-byte scanner mask consumer. No orphan kernel. The §2.2 three-relations disambiguation keeps "same-wave consumer" distinct from "cascade-lock". | Same-wave consumer per primitive. | ACCEPT |
| 30 | P3-B §2 / §0 — F-AUX touch-up | P3-B §2 carries the hard-cap column (V1 gap 5 closed) and §0 records "V2 fold: F-AUX surgical touch-up." But P3-B §2/§3 still manifest "W4 … paired in one wave" / "W4 sub-waves (W4a/W4b/…)" — §2's W4 row prose half-mentions sub-waves while §3's W4 dependency-justification text still reads as one paired wave. P3-B is mostly isomorphic but its §3 W4 prose was not fully reconciled to the four-sub-wave structure. | Wave manifest isomorphic; per-wave costed. | REVISE |
| 31 | P3-A §2.1 C4 envelope | C4 still sized `~1,045 net (P2-E §7.4)`, risk MEDIUM-HIGH, "~6.0 h aggregate per-slice cap — the largest candidate." P3-A's estimate is faithful and well-sourced; the defect is downstream — P3-F W4b carries this ~1,045 figure into a single 75-min-redress sub-wave without sub-dividing it. P3-A's own §2.2 names no W4b sub-division. | P3-A envelope realistic; carry-through faithful. | REVISE |
| 32 | P3-F SPEC §7.2 W4b — eleven slices vs one redress | P2-E §7.4 names eleven slices (S1 scalar re-home, S2-S5 four NEON bodies, S6 checkasm, regen, CSS scaffold, sink swap, deletion, …). Each NEON body (`hex_x4`, `hex_x8`, `hex_variable`, `surrogate_join`) is itself a kernel with its own checkasm surface. The natural sub-division — W4b-1 scalar+checkasm harness, W4b-2 the x4/x8 fixed-width bodies + JSON consumer, W4b-3 the variable-width CSS/JS/TOML bindings + codegen — is never drawn. W4b is the one wave the V2 fold left structurally unaddressed. | Over-scoped wave splits before dispatch. | REJECT |

Disposition tally: 32 rows — **19 ACCEPT, 7 REVISE, 6 REJECT.**

ACCEPT-rate: 19/32 ≈ **59.4%** (counting ACCEPT only).
ACCEPT+REVISE: 26/32 ≈ 81.3%.

---

## §3 — Aggregate verdict

**CH4 COST verdict: REJECT.**

The V2 fold made real, substantial progress. Five of the eight V1 gaps
are cleanly closed: the §5 structural-bitmap chain is folded into a
costed W3 (gap 3); the SPEC manifest carries a risk column (gap 4);
P3-B carries its hard-cap column (gap 5); the C8 checkasm LOC is
distributed and visible per primitive-landing wave (gap 6); the
cascade-lock is disambiguated so it no longer *mandates* an over-cap
wave (gap 8). The W4 sub-wave structure (W4a-d) is the right shape and
correctly removes ~550-815 LOC of cascade-locked surface from the
monolith. Nineteen of thirty-two rows ACCEPT — the formal cost
discipline is largely sound, and the cohort is far healthier than the
V1 ~37%.

But CH4 cannot clear, for two load-bearing reasons:

1. **W4b — the codec — is still ~1,045 net LOC under a single 75-min
   redress (gaps 2, 9, 10, 22, 32).** The V1 REJECT was *not* "the
   monolith is too big"; it was "the codec is ~4× over a redress cap."
   V2 sub-waved the *bracket* (W4a/W4b/W4c/W4d) but never sub-divided
   the *codec*. W4b carries P2-E §7.4's ~1,045-net, eleven-slice,
   ~6.0 h-aggregate figure verbatim into one `≤90 min` / 75-min-redress
   sub-wave. The SPEC §2.2 even asserts "each W4x is … individually
   inside its … 75-min redress cap" — a claim §7.2's own W4b budget
   directly contradicts. The codec must be sub-divided (W4b-1..W4b-n,
   the natural cut being scalar+checkasm harness / fixed-width bodies +
   JSON consumer / variable-width bindings + codegen), or W4b is a paper
   budget exactly as the V1 W5 codec was.

2. **P3-C and P3-D were not re-authored — the cohort is isomorphic
   across four of six artefacts, not six (gaps 1, rows 3-6, 30).** The
   V1 root defect was a non-isomorphic manifest. P3-F SPEC, P3-F
   DISPATCH, P3-A, and P3-B now agree on W1-W5/W4a-d. But P3-C §1.4 and
   §2 still gate the old two-wave manifest (W4=paired codec+string-block,
   W5=ASM kernels) — the V1 CH4 row 25 REJECT is reproduced unchanged.
   P3-D §2.3's per-wave field-population table — a cost-bearing per-wave
   obligation — still uses the old SPEC-placeholder numbering (W1=release,
   W4=direct contract). The F-AUX touch-up extended P3-C's W4 maintain
   envelope to the three direct-GO rows but never rebound P3-C's wave
   letters; P3-D was not touched at all. The W4a-d sub-waves have no
   dedicated P3-C falsifiability-gate table — their gates exist only
   inside the SPEC §7.x prose. A wave whose budget lives under one
   letter in the SPEC and whose gate lives under a different letter in
   P3-C is not coherently costed.

The V2 fold's own consolidation forecast ("all six clear ≥95% — V2 is
an integration pass") under-scoped the integration: it re-authored the
two P3-F drafts and surgically touched P3-A/P3-B, but the F-AUX scope
did not include rebinding P3-C's and P3-D's wave manifests, and no
agent sub-divided the W4b codec. Both omissions are squarely in the
CH4 lens.

This must fold to V3. The V3 fold is narrow and well-understood:
(a) sub-divide W4b into LOC-bounded sub-waves each inside a 75-min
redress; (b) re-author P3-C §1.4/§2 and P3-D §2.3 to the W1-W5/W4a-d
manifest so all six artefacts are isomorphic. Neither requires fresh
research — the W4b slice structure is already in P2-E §7.4, and the
target manifest already exists in P3-F.

---

## §4 — Remaining cost gaps requiring V3 fold

1. **Sub-divide W4b (the codec).** ~1,045 net LOC / eleven slices /
   ~6.0 h aggregate cannot land in one 75-min redress. Cut it along
   P2-E §7.4's slice seams — e.g. W4b-1 (scalar re-home S1 + checkasm
   harness S6, the parity foundation that lands FIRST), W4b-2 (the
   fixed-width `hex_x4`/`hex_x8` NEON bodies + the JSON x4 consumer
   re-body + the −215 deletion), W4b-3 (the variable-width
   `hex_variable`/`surrogate_join` bodies + the codegen const-generic
   emission + CSS/JS/TOML scaffolds). Each sub-wave gets its own LOC
   budget ≤ ~350-450 hand and its own 75-min redress. Update SPEC §2.2's
   "each W4x is inside its 75-min cap" claim to be true. (Rows 8, 9, 10,
   22, 32; V1 gap 2.)

2. **Re-author P3-C §1.4 and §2 to the W1-W5/W4a-d manifest.** P3-C must
   gate W4a, W4b (or W4b-1..n), W4c, W4d as separate falsifiability-gate
   tables, not a W4-paired / W5-kernels two-wave set. The gate *content*
   is sound and already cited by the SPEC §7.x sections — only the wave
   letters and table headers are stale. (Rows 3, 4, 5; V1 gap 1 residue.)

3. **Re-author P3-D §2.3's per-wave population table.** Replace the old
   SPEC-placeholder labels (W1 release / W2 typed / W3 tape / W4 direct /
   W5 close) with the actual W1-W5/W4a-d behaviour waves, so each wave's
   field-population obligation binds to the correct wave. (Row 6; V1
   gap 1 residue.)

4. **Record the W3 MEDIUM→HIGH risk escalation, or sub-wave W3.** P2-A
   C3 §2.2 warned the folded P2-D §5 chain pushes W3 to HIGH. P3-F SPEC
   §2 records W3 as MEDIUM. Either record HIGH (and confirm a HIGH-risk
   ~465-635-hand-equivalent + ~120-regen wave fits one 75-min redress —
   it plausibly does not, see point 5), or sub-wave W3 into the union
   substrate and the §5 SIMD chain. (Rows 13, 14.)

5. **Re-check W3's LOC against the 75-min redress.** W3 at ~465-635
   hand-equivalent + ~120 regen, eight P2-A slices + the §5 chain, is
   ~1.5-2× the W1 ~300-LOC/~85-min-redress scale and plausibly overruns
   the 75-min redress ceiling. The SPEC neither flags nor sub-waves it.
   Either demonstrate W3 fits one redress or sub-wave it as point 4
   suggests. (Row 14.)

6. **Make the 75-min redress sub-cap legible in the SPEC §2 manifest
   column.** The `Hard cap` column reads `≤90 min` for every wave; the
   binding redress sub-cap is 75 min. Add an inline note or a second
   column so a reader scanning only the manifest table cannot mis-size a
   wave. (Row 9 residue; V1 gap 7 residue.)

CH4 V2 disposition: **REJECT** — clear-rate 59.4% ACCEPT (81.3%
ACCEPT+REVISE), below the ≥95% bar. Fold the six gaps above into S-P3
V3. The two REJECT-grade defects — the un-subdivided W4b codec and the
un-re-authored P3-C/P3-D manifests — are the load-bearing failures;
the V3 fold is a narrow, no-fresh-research integration pass.
