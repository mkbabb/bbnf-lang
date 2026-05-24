# SK-V14 S-P3 V1 CHALLENGE — CH6 ANTI-PAPER-CLOSE

Pass: S-P3 Synthesis-Plan. Cycle: V1. Lens: CH6 ANTI-PAPER-CLOSE.
Date: 2026-05-23.
Scope: every wave closes on **measurement** (named bench-row threshold), not future-phase promise; revert protocol per wave; same-wave consumer NAMED per candidate (no orphan kernel ships); SPEC forbids deferral; F-V2-P1ABC-RERECORD Stage-0 binding verified at SPEC §1 + §11/§12/§13.
Output: this file.
HARD CAP: 30 min. WRITE-ONLY (no git add/commit). Aggregator commits 8 hardening files atomically.

Authority:
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md` (HEAD 8f4756113)
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` (CH6 lens definition lines 140-145)
- `restart/skinny/tranches/sk-v14/research/p3/{p3a..p3f}` + `sk-v14/SPEC.md` + `sk-v14/DISPATCH-PROMPT.md`
- S-P2 V3 §6.3 binding (F-V2-P1ABC-RERECORD Stage-0 wave commitment)

## §1 — Synthesis (the CH6 lens applied)

### §1.1 — CH6 binding (verbatim from PASS-3-SYNTHESIS-PLAN.md §3 lines 140-145)

> **CH6 ANTI-PAPER-CLOSE** — does every wave close on **measurement**, not
> a future-phase promise? A wave whose exit gate is "wired" or
> "integrated" without a bench-row threshold is a paper-close. Does every
> wave carry a revert protocol? Does the SPEC forbid deferral — "no wave
> closes on a future-phase promise"? Is each candidate's same-wave
> consumer named, so no orphan kernel ships?

Five concrete CH6 sub-tests fall out:
1. **Measurement closure** — every wave's exit gate names a measurable threshold (bench Mbps, samply attribution, cargo asm output, file-existence grep, ROLLING-SOTA-DELTA cell state, du-sh corpora floor, gate-json column rule). No "wired"/"integrated"/"prepared" closures.
2. **Revert protocol per wave** — every wave carries an explicit revert protocol with a named REDRESS slot.
3. **No-deferrals language in SPEC** — SPEC explicitly forbids closing on a future-phase promise.
4. **Same-wave consumer per candidate** — every shortlist candidate names its consumer; no kernel ships without its hot-path consumer landing in the same commit.
5. **F-V2-P1ABC-RERECORD Stage-0 binding** — P3-A C5 IS the packet; SPEC §1 + §11/§12/§13 must bind it as Stage-0 of admit-waves with no orphan-kernel risk.

### §1.2 — Sub-test 1: Measurement closure (per wave)

Walk each wave's exit gate language at SPEC §3..§14 and P3-C §2.0..§2.11:

| Wave | SPEC exit-gate measurement | Bench-row anchor | Disposition |
|---|---|---|---|
| W0 (§3) | `xtask gate-json` rejects rows missing required columns; throughput cells stay within ±1.0% of `SK-V14-open`; all 75 rows carry 4 new SK-V14 columns | 51 JSON + 24 CSS L4 baseline rows | ACCEPT — telemetry-substrate closure on column-population + ±1.0% floor; bench-anchored. |
| W1 (§4) | Single-lane `sonic_rs_anchor` deleted (grep returns 0); 3 plane-correct anchors wired; `per_iter_equality` PASS per iter; 22 admit-row reverts in RESULTS + ROLLING-SOTA-DELTA; ROLLING-SOTA-DELTA shows 0/17 across 3 planes; ±1.0% on non-target | ROLLING-SOTA-DELTA cell-state + 22-row revert | ACCEPT — measurable file-grep + cell-state + per-iter gate-rule. |
| W2 (§5) | `cargo xtask regen-css` round-trip clean (rm + regen + diff empty on both runtime trees); `check-css-l4-<provider>` companion exists; bypass-header detector empty; ±1.0% on JSON rows | round-trip exit code + git-diff empty + file-existence grep | ACCEPT — bash-executable round-trip evidence. |
| W3 (§6) | `du -sh ≥ 800 KB`; manifest.md cites 4 sources; checksum + size telemetry captured; loader resolves; ±1.0% JSON rows | du-sh corpora floor (working set ≥ 800 KB) | ACCEPT — measurable bytes-on-disk floor + integration test. |
| W4 (§7) | 7-template-dir delete grep (`wc -l == 0`); 7-provider-module delete grep (`wc -l == 0`); regen-css produces byte-deterministic output (git diff empty); ROLLING-SOTA-DELTA shows CSS L4 0/24; 24 REDRESS entries land; ±1.0% on JSON rows | file-existence grep + cell-state + REDRESS census | ACCEPT — file-grep + cell-state + REDRESS evidence. |
| W5 (§8) | `RuntimeProvider::*\|JsonGrammar\|parse_json_grammar` grep returns 0; 8 per-grammar providers collapsed to 1 generic template (`*_provider.rs ≠ grammar_provider.rs == 0`); Lock-14 baseline gate passes; non-JSON proof; regen-css continues clean; ±1.0% on all rows | grep returns 0 + Lock-14 baseline + ±1.0% maintain | ACCEPT — exact grep-counts + baseline-gate enforcement + maintain budget. |
| W6 (§9) | Per-sub-wave (W6.1..W6.9): `find crates/core/src/runtime/<G>` returns 0 dirs; generated-output regen empty diff; per-grammar parser tests pass; Lock-14 baseline passes; ±1.0% on all rows. W6 aggregate: 9 per-grammar dirs collapsed; 67 hand-written → 0 + 67 generated; Pattern-H opt-out rewritten; LegacyPath shim removed; forward invariant active | per-sub-wave grep + diff-empty + maintain budget | ACCEPT — measurable per-sub-wave + aggregate gates. |
| W7 (§10) | Named pre-wave row `json/numbers/direct_to_struct/main` hot-leaf attribution shifts in **samply trace** (W7 names exact symbol path of pre vs post; samply diff = gate evidence); Lock-1 triad declared per shape in REDRESS; shape consumer exercised on ≥2 grammar families; hardcoded P1-P8 cascade fails closed; BackendShape-only dispatch (grep returns 0 grammar-name match arms); ±1.0% non-target | samply trace + grep + ±1.0% maintain | ACCEPT — samply attribution change is the measurement (per SYNTHESIS §3 C-4 verbatim binding). |
| W8 (§11) | At least one CSS L4 feature ADMITs > strict-vs-strict against lightningcss full-parse on production corpora (≥800 KB) on same plane/equality; per-iter equality PASS; audit_overlay_verdict shifts to AUDIT-SUSTAINED for ADMIT rows; Lock-14 + non-JSON proof; ±1.0% non-target; if admitting any 12-consumer-dependency primitive, F-V2-P1ABC-RERECORD Stage-0 shipped | per-feature Mbps threshold vs lightningcss + per-iter equality | **REVISE-1** — Stage-0 binding is CONDITIONAL on primitive-admission census, which creates an orphan-kernel risk (see §1.6 below). |
| W9 (§12) | Every selected JSON direct/typed row meets Track 1 + Track 2 floors; correctness parity; plane-correct strict comparator; per-iter equality; Track 2 entry-point divergence; ±2.0% on non-target; if any 12-dep primitive admits, F-V2-P1ABC-RERECORD Stage-0 shipped | per-row Mbps + comparator-strict + per-iter equality | **REVISE-1** — same Stage-0 conditional concern. |
| W10 (§13) | Distinct parse_only path exists; ≥1 row ADMITs > sonic_rs::Skipper on same plane/corpus/equality; per-iter equality PASS; AUDIT-SUSTAINED transition; Track 1/2 structural independence; ±1.0% non-target; if any 12-dep primitive admits, F-V2-P1ABC-RERECORD Stage-0 shipped | Skipper Mbps + per-iter + structural-independence | **REVISE-1** — same Stage-0 conditional concern. |
| W11 (§14) | Every wave has admitted/rejected/routed status; ADMIT counts per family; per-row architectural-block proof for non-admits; SK-V15 brackets if any goal unmet | reconciliation + per-family ADMIT count | ACCEPT — close ceremony measures by wave-disposition census. |

**Result:** 9/12 ACCEPT measurement-closure. 3/12 carry REVISE-1 on the F-V2-P1ABC-RERECORD Stage-0 conditional language (not a measurement-closure failure per se, but a CH6 paper-close vulnerability — see §1.6).

### §1.3 — Sub-test 2: Revert protocol per wave

Walk each wave's SPEC revert-protocol clause:

| Wave | Revert protocol present? | Names REDRESS slot? | Disposition |
|---|---|---|---|
| W0 (§3:373-375) | YES — "revert the W0 implementation commits together, restore the opening RESULTS schema, and record a W0 REDRESS rejection naming the missing profiler, gate, or row" | YES | ACCEPT |
| W1 (§4:445-447) | YES — "revert comparator/oracle/PRUNE-1 changes together; preserve audit-trail in research artefact; add REDRESS naming the missing strict-comparator binding or oracle path" | YES | ACCEPT |
| W2 (§5:503-505) | YES — "revert xtask/regen_css.rs changes + delete emitted output trees; add REDRESS naming the failing round-trip case or missing grammar-derived emission path" | YES | ACCEPT |
| W3 (§6:554-555) | YES — "delete `skinny/corpora/css-l4-sk-v14/` + revert loader changes; add REDRESS naming the unattainable corpus target" | YES | ACCEPT |
| W4 (§7:613-615) | YES — "revert the template + provider + runtime-twin deletions + RESULTS / DELTA reverts as one slice; add REDRESS naming the failing emission path" | YES | ACCEPT |
| W5 (§8:672-674) | YES — "revert trait-dispatch + provider-collapse changes as one slice; restore the 8 per-grammar provider modules; add REDRESS naming the failing dispatch path or missing input-contract field" | YES | ACCEPT |
| W6 (§9:760-763) | YES — per-sub-wave + W6 aggregate revert; "revert the collapse + restore the hand-written `<G>/` files; record per-sub-wave REDRESS naming the failing emission contract or test gap. A W6 aggregate revert undoes all 9 sub-waves; sub-wave-granular revert is the default" | YES — per-sub-wave + aggregate | ACCEPT |
| W7 (§10:824-826) | YES — "revert PRUNE-5 wire-up changes + restore the SCAFFOLD-only state of W8 + W9; add REDRESS naming the failing CSP-shape consumer or missing Lock-1 triad slot" | YES | ACCEPT |
| W8 (§11:888-890) | YES — "revert row admits + REDRESS / RESULTS changes + bench wiring as one slice; add REDRESS naming the failed comparator parity or missing production-corpus path" | YES | ACCEPT |
| W9 (§12:948-950) | YES — "revert row admits + REDRESS / RESULTS changes + bench wiring as one slice; add REDRESS naming the failed comparator parity or missing per-corpus binding" | YES | ACCEPT |
| W10 (§13:1006-1008) | YES — "revert distinct path + bench wiring + RESULTS / DELTA changes as one slice; add REDRESS naming the failed Skipper parity or missing distinct emission path" | YES | ACCEPT |
| W11 (§14:1057-1059) | YES (alternate form for close-ceremony) — "no source revert by default. Reopen the producing wave or mark close blocked with a mismatch list naming file paths, rows, and missing evidence" | YES (close-ceremony form) | ACCEPT |

**Result:** 12/12 ACCEPT revert-protocol presence. Every wave names a REDRESS slot. Per-sub-wave revert at W6 is properly granular (per `[clean-regen-discipline]` + S-P0 §3.3 sub-wave count).

### §1.4 — Sub-test 3: No-deferrals language in SPEC

SPEC §1 line 220 (verbatim):
> No deferrals: a wave cannot close on "wired", "advisory", "future consumer", "integrated", or "paper close" language without measured evidence (per `[no-deferrals]`).

SPEC §14 W11 line 1050 (verbatim):
> Pre-blocked routes: paper close (W11 must close on measurement, not promise);…

SPEC §1 line 227 (verbatim — CH7-V2 procedural addendum carried into SK-V14):
> any past-perfect verb-tense claim ("landed", "delivered", "shipped") on a function body whose path:line returns NOT-PRESENT at the cycle HEAD is paper-close even if the cite chain is otherwise complete;…

P3-D §1.144 lists "P-7 paper-close on future-phase promise" as a binding pre-block.

**Result:** ACCEPT — SPEC §1 forbids deferral verbatim with five proscribed terms ("wired", "advisory", "future consumer", "integrated", "paper close"); SPEC §14 reinforces at the close-ceremony wave; CH7-V2 verb-tense discipline is embedded.

### §1.5 — Sub-test 4: Same-wave consumer per candidate (orphan-kernel prevention)

Walk P3-A §2.1 shortlist table + SPEC same-wave-consumer clauses:

| # | Candidate | P3-A consumer NAMED? | SPEC wave consumer NAMED? | Orphan-kernel risk? |
|---|---|---|---|---|
| C1 | long_string_body_simd_scan | YES — `parse_that_regex::skip_string_plain_trusted` at `lib.rs:547`; rows: unicode_escapes/twitter/gsoc-2018/mesh/github_events direct | Folded into W9 admit per SPEC §12 (R7 JSON direct+typed) | NO |
| C2 | structural_index_singular_substrate_consumer | YES — direct + typed envelopes at `generated.rs:466,506,2949`; rows: twitter/citm_catalog/marine_ik/github_events/mesh typed | Folded into W9 admit per SPEC §12 | NO |
| C3 | digit_block_simd_accumulate | YES — direct-plane number kernel; rows: canada/mesh/numbers/marine_ik direct | Folded into W9 per SPEC §12 | NO |
| C4 | unicode_escape_neon_nibble_decode | YES — `unicode_escapes` direct + `y_string_unicode` parse_only; CSS L4 escaped-ident row | Folded into W9 + W10 + W8 per SPEC §11/§12/§13 | NO |
| C5 | parse_attribution_envelope_cracker | YES — IS F-V2-P1ABC-RERECORD; "consumer = 12 dep primitives NAMED" | **see §1.6 — conditional binding creates orphan-kernel risk** | **REVISE-1** |
| C6 | force_inline_lto_envelope_discipline | YES — codegen template + cargo asm + samply NAMED; paired with C5 | Paired with C5 — same risk inherited | **REVISE-1 (inherits C5 risk)** |
| C7 | ascii_whitespace_skip_64 | YES — every JSON value-position prelude; CSS L4 declaration-value whitespace | Folded into W10 + W8 per SPEC §13/§11 | NO |
| C8 | BackendShape::SinkOnly activation | YES — 8 P1-B direct-plane rows where envelope is 70%+ top-1 | Folded into W9 per SPEC §12 | NO |

**Result:** 8/8 candidates carry CF-3 3-gate cell with consumer NAMED. Six candidates (C1, C2, C3, C4, C7, C8) carry no orphan-kernel risk — consumer wired in admit-wave SPEC §11..§13. Two candidates (C5, C6) carry orphan-kernel risk per §1.6.

### §1.6 — Sub-test 5: F-V2-P1ABC-RERECORD Stage-0 binding (THE LOAD-BEARING CH6 CHECK)

**The dispatch context binding (CHALLENGE-CONTEXT §1 line 25):**
> F-V2-P1ABC-RERECORD Stage 0 binding: P3-A C5 = the packet; P3-B W9 schedules it; P3-F SPEC §1 + §11/§12/§13 bind. CH6 must verify no orphan-kernel risk.

**S-P2 V3 §6.3 verbatim binding (per P3-F draft §1.3.3 lines 96-117):**
> Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive.

**Three artefacts state the binding three different ways:**

1. **P3-A §2.1 C5 (`p3a-candidate-shortlist.md:124`):**
   > "Per `[no-deferrals]`, C5 ships in any wave admitting C1/C3/C7 (the three F-V2-P1ABC-RERECORD-dependent shortlist entries)."
   → CONDITIONAL on C1/C3/C7 admission in the wave.

2. **P3-B §2.12 W9 (`p3b-wave-sequencing.md:256, 354-355`):**
   > "F-V2-P1ABC-RERECORD is **Stage 0 of W9** per S-P2 §6.3 binding ('Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive'); W9 is the first such wave because R6 admits CSS L4 candidates including P2-F C1 structural classify + P2-C C-P2C-1 ascii_set_member64_css_delimiter + P2-F C5 string-block 64-byte oracle (envelope-internal primitives per CH2 V3 dual-gate inheritance)."
   AND
   > "Stage 0 lands in W9; if W9's primitive consumers do NOT include the 12-list (i.e., W9 only admits CSS L4 primitives not in the 12-list), Stage 0 STILL ships in W9 because W9 is the first implementation wave per S-P2 §6.3 binding."
   → UNCONDITIONAL on W9 (which is R6 CSS L4 re-admit per P3-B numbering).

3. **SPEC §11 W8 (line 856 + 873; W8 in SPEC numbering = R6 CSS L4 per SPEC §11):**
   > "W8 plan does NOT include Stage-0 F-V2-P1ABC-RERECORD UNLESS it admits one of the 12 consumer-dependency primitives (per S-P2 V3 §6.3)."
   AND
   > "If wave admits any consumer-dependency primitive, F-V2-P1ABC-RERECORD Stage-0 shipped per S-P2 V3 §6.3."
   → CONDITIONAL on 12-consumer-dependency primitive admission.

   Same conditional language at SPEC §12 W9 (line 916, 933) and SPEC §13 W10 (line 975, 993).

4. **P3-F draft §1.3.3 (`p3f-spec-draft.md:117-122`):**
   > "The Stage-0 F-V2-P1ABC-RERECORD therefore lands as Stage 0 of W8 OR W9 OR W10 — whichever first admits any of the 12. 'Any W8..W10 wave admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives ships the rerun in Stage 0 of the same…'"
   → CONDITIONAL on 12-primitive admission across W8/W9/W10.

**The three-way divergence:**

- P3-A C5 binding: ships when wave admits C1/C3/C7 (P3-A shortlist names — long-string SIMD, digit-block, whitespace).
- P3-B W9 binding: ships UNCONDITIONALLY in W9 because W9 is the first implementation wave per S-P2 §6.3 verbatim.
- SPEC + P3-F binding: ships CONDITIONALLY in W8/W9/W10 only if 12-consumer-dependency primitive admits.

**The CH6 paper-close vulnerability:**

The SPEC's conditional binding ("UNLESS it admits one of the 12 consumer-dependency primitives") opens an orphan-kernel hole:

- C5 IS itself a load-bearing shortlist candidate (P3-A §2 §2.1 row 5 — one of 8 SPEC interventions).
- If W8 (R6 CSS L4 re-admit) happens to admit CSS L4 primitives that are NOT in the 12-list (e.g., P2-F C1 structural classify or other non-envelope-internal primitives), then per SPEC §11:856 W8 does NOT ship F-V2-P1ABC-RERECORD.
- Similarly W9 (R7 JSON direct+typed): if the actual W9 plan selects rows whose consumers don't hit the 12-list, Stage-0 doesn't ship.
- Similarly W10 (R8 parse_only): same conditional gap.
- The cascade can complete W8 → W9 → W10 WITHOUT EVER SHIPPING C5, and C5 is itself a load-bearing intervention in the P3-A ≤8 slate.
- Per S-P2 V3 §6.3 verbatim ("Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive"), the binding is on "implementation wave", not on "wave that happens to select a 12-list primitive". P3-B reads this verbatim and pins Stage-0 to W9 unconditionally.

This is exactly the CH6 failure mode the lens is designed to catch: a load-bearing kernel (C5 / F-V2-P1ABC-RERECORD) shipping behind a CONDITIONAL gate whose precondition (12-primitive admission) is itself fluid across waves. The SPEC's wave-W8/W9/W10 conditional language does NOT bind C5 to a specific wave with a measurable closure threshold; instead, it predicates Stage-0 shipping on a primitive-admission census that the per-wave plan controls. The wave can paper-close on "no 12-list primitive admitted this wave, so no Stage-0 required" — and C5 (the kernel) ships in no wave.

Additionally: **wave-numbering divergence** between P3-B (W9 = R6 CSS L4) and SPEC (W8 = R6 CSS L4). The dispatch context §1 line 21 already flags this for CH1; CH6 inherits it because the Stage-0 binding clause names a different wave in P3-B vs SPEC.

**Disposition: REVISE-1** — three artefacts (P3-A, P3-B, SPEC + P3-F) hold three different bindings on F-V2-P1ABC-RERECORD Stage-0 wave commitment. The SPEC's conditional binding creates an orphan-kernel risk for C5 (load-bearing shortlist candidate). V2 must:
(a) Resolve the wave-numbering convergence (with CH1) — either W8 or W9 is R6 CSS L4, pick one.
(b) Pin the F-V2-P1ABC-RERECORD Stage-0 commitment to ONE named wave UNCONDITIONALLY (per P3-B's reading of S-P2 V3 §6.3) — that wave's exit gate carries a measurable threshold on samply-attribution surfacing of the 12 dep primitives (the P3-A C5 falsifiability sketch at §2 C5 lines 121).
(c) Insert SPEC §1 binding language verbatim: "F-V2-P1ABC-RERECORD ships as Stage-0 of W<N> (the first re-admit wave) regardless of which primitives admit in that wave; exit gate = post-rerecord samply trace shows inner-primitive attribution > 0% on at least one of {match_tiny_plain_string, match_number_at_digit, parse_number_direct, skip_ascii_whitespace}."

## §2 — Deliverable (CH6 disposition summary)

### §2.1 — Per-sub-test scorecard

| CH6 sub-test | Pass count | Fail count | Disposition |
|---|---|---|---|
| 1 — Measurement closure | 9/12 | 0/12 (3/12 carry REVISE-1 on Stage-0 conditional) | 9 ACCEPT + 3 REVISE-1 |
| 2 — Revert protocol per wave | 12/12 | 0/12 | 12 ACCEPT |
| 3 — No-deferrals SPEC language | 1/1 | 0/1 | ACCEPT |
| 4 — Same-wave consumer per candidate | 6/8 ACCEPT + 2/8 REVISE-1 (C5/C6) | 0/8 | 6 ACCEPT + 2 REVISE-1 |
| 5 — F-V2-P1ABC-RERECORD Stage-0 binding | 0/1 (three-way divergence) | 1/1 (conditional opens orphan-kernel hole) | REVISE-1 |

### §2.2 — Aggregate disposition

Per CH6 lens:
- 9/12 wave exit-gates close on measurement with no orphan-kernel risk (W0, W1, W2, W3, W4, W5, W6, W7, W11).
- 3/12 wave exit-gates (W8, W9, W10) close on measurement BUT their F-V2-P1ABC-RERECORD Stage-0 binding language is conditional in a way that allows the kernel (C5) to ship in no wave.
- 12/12 revert protocols present.
- SPEC §1 no-deferral language verbatim present.
- Same-wave consumer NAMED on 8/8 shortlist candidates (CF-3 3-gate cell complete).
- F-V2-P1ABC-RERECORD Stage-0 binding diverges three ways across P3-A/P3-B/SPEC+P3-F.

**Disposition: 14 ACCEPT + 5 REVISE-1.**

ACCEPT-rate (per CH6 sub-tests, weighted by line-item): 14/(14+5) = **73.7%**.

If we collapse the 3 REVISE-1 on Stage-0 (since they are ONE root issue surfacing in three waves) to a single line-item: 14/(14+1+2) = **17/19 = 89.5%** still below the §3Z ≥95% ACCEPT bar — V2 must fold the Stage-0 binding fix.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds — CH6 verification)

CH6 disposition is rooted in **measurable** evidence:

1. **The wave-numbering divergence is grep-verifiable:**
   - `grep -n "W8\|W9\|W10" restart/skinny/tranches/sk-v14/SPEC.md` (SPEC numbering: W8 = R6 CSS L4 per §11; W9 = R7 JSON direct+typed per §12; W10 = R8 parse_only per §13).
   - `grep -n "W8\|W9\|W10\|W11" restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (P3-B numbering: W9 = R6 CSS L4 per §2.12; W10 = R7 per §2.13; W11 = R8 per §2.14).
   - Two different wave-numbers attached to the same R-target. CH1 owns the convergence; CH6 inherits the Stage-0 binding because the binding names a wave-id.

2. **The Stage-0 binding inconsistency is grep-verifiable:**
   - `grep -n "F-V2-P1ABC-RERECORD\|Stage 0\|Stage-0" restart/skinny/tranches/sk-v14/SPEC.md` returns 10 hits; line 856 + 873 + 916 + 933 + 975 + 993 carry the "UNLESS"/"if … shipped" conditional language.
   - `grep -n "F-V2-P1ABC-RERECORD\|Stage 0\|Stage-0" restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` shows P3-B §2.12 W9:256 + §4 cross-wave pre-block:354-355 carry UNCONDITIONAL Stage-0-in-W9.
   - The three-way divergence is observable by reading lines 256 (P3-B), 856 (SPEC), 124 (P3-A C5).

3. **The orphan-kernel risk is logically verifiable:**
   - C5 is row 5 of P3-A §2.1 shortlist (one of 8 SPEC interventions).
   - C5 IS F-V2-P1ABC-RERECORD per P3-A §2 line 124 verbatim.
   - SPEC §11 W8:856 conditional ⇒ wave can admit CSS L4 features whose primitives are not in the 12-list ⇒ no Stage-0 ships in W8.
   - SPEC §12 W9:916 same conditional ⇒ wave can re-admit JSON direct+typed with primitives not in the 12-list ⇒ no Stage-0 ships in W9.
   - SPEC §13 W10:975 same conditional ⇒ wave can stand up parse_only distinct path with primitives not in the 12-list ⇒ no Stage-0 ships in W10.
   - Result: C5 ships in zero waves. Orphan-kernel.

4. **The measurement-closure pass count is bench-verifiable** at each named wave's exit-gate threshold (the cells in §1.2 above are direct quotations from SPEC §3..§14 exit-gate language; each names either a Mbps floor, a grep-count, a file-existence check, a samply-attribution change, a du-sh corpora floor, or a cell-state in ROLLING-SOTA-DELTA).

## §4 — Pre-blocked routes (REDRESS entries this lens must NOT re-open)

CH6 is a closure lens, not an admission lens. It does not re-open any REDRESS route by construction. The pre-blocks relevant to CH6's findings:

- **REDRESS no-deferrals binding** (per `[no-deferrals]` memory): every primitive lands its hot-path consumer in the same commit. The C5 orphan-kernel risk in §1.6 is a direct paper-close of `[no-deferrals]` — V2 must close this.
- **CH7-V2 verb-tense discipline** (SPEC §1:227): C5's wave-commitment cannot ship past-perfect ("F-V2-P1ABC-RERECORD shipped") unless its samply-trace evidence is captured at HEAD; SPEC's conditional language doesn't violate this directly but creates a vehicle for wave-slot truth loss.
- **`[no-orphan-redress]` discipline:** every wave's exit gate names the corpus rows it must lift and the rows it must maintain; the C5 paper-close hole creates a vehicle where no REDRESS entry surfaces because the wave honestly closes on "no 12-list primitive admitted this wave" without ever shipping the kernel.
- **P-5 pattern pre-block (SPEC §15:1073):** "Scaffold-research counted as load-bearing. SK-V14 PRUNE-5 (W7) wires W8 + W9 end-to-end; no row admit may cite W8 / W9 as evidence until the runtime consumer is measured." The C5 case is the inverse: a primitive (the parse-attribution build-discipline) ships only if its consumers (other primitives) admit — a paper-close vehicle.

CH6 V1 does not re-open any REDRESS route; it surfaces a binding inconsistency for V2 to fold.

## §5 — Sources (every upstream artefact cited)

### §5.1 — Authority chain
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md` (HEAD 8f4756113; §1 cross-axis convergence rules; §2 CH6 verbatim disposition focus)
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` lines 140-145 (CH6 verbatim definition)
- `restart/prompts/ORCHESTRATOR.md §3W + §3Z` (cohort LOCK ≥95% × 2 cycles; V≤5 ceiling)

### §5.2 — Six P3 axis artefacts + SPEC + DISPATCH-PROMPT
- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (317 lines)
  - §2 C5 (line 113-124) — parse_attribution_envelope_cracker is F-V2-P1ABC-RERECORD itself
  - §2.1 shortlist table (line 167-179) — 8/8 CF-3 3-gate cell completeness
  - §3 falsifiability binding (line 201-218) — per-candidate revert protocol verbatim
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (406 lines)
  - §1.1 binding inputs (line 30-33) — F-V2-P1ABC-RERECORD Stage-0 binding verbatim
  - §2.1 wave manifest (line 70-86) — 12-wave manifest with same-wave consumer NAMED per wave
  - §2.12 W9 details (line 250-264) — UNCONDITIONAL Stage-0-in-W9 binding
  - §3 per-wave falsifiability + revert (line 300-319) — 12 revert protocols enumerated
  - §4 cross-wave pre-blocks (line 354-355) — UNCONDITIONAL Stage-0-in-W9 reiterated
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (527 lines)
  - §1.2 wave manifest (line 22-37) — 12-wave manifest
  - §2.0..§2.11 per-wave gates (line 78-449) — every gate carries measurable exit + revert protocol
  - §3 summary (line 450-473) — ZERO unmeasurable gates rejected at V1
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md` (168 lines)
  - §1.144 P-7 paper-close on future-phase promise binding
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md` (903 lines)
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (245 lines)
  - §1.3.3 F-V2-P1ABC-RERECORD Stage-0 wave commitment (line 96-122) — CONDITIONAL W8/W9/W10 binding
- `restart/skinny/tranches/sk-v14/SPEC.md` (1138 lines)
  - §1:220 no-deferrals language verbatim
  - §1:227 CH7-V2 procedural addendum
  - §3..§14 per-wave sections (line 315-1059) — 12 same-wave consumers + 12 revert protocols + 12 exit gates
  - §11:856 + 873 — W8 CONDITIONAL Stage-0 binding
  - §12:916 + 933 — W9 CONDITIONAL Stage-0 binding
  - §13:975 + 993 — W10 CONDITIONAL Stage-0 binding
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (344 lines; per-wave dispatch contract)

### §5.3 — S-P2 V3 carry-forward binding authority
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.3` (F-V2-P1ABC-RERECORD Stage-0 wave commitment binding — "Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive")

### §5.4 — Memory binding
- `[no-deferrals]` — every primitive lands its hot-path consumer in the same commit; never defer to future tranches
- `[no-orphan-redress]` — every wave's exit gate names corpus rows it must lift + maintain
- `[execute-planned-architecture]` — don't retreat from planned architectural changes; never ship stub/shim
- `[execute-planned-architecture]` reinforces: support-only landings (= scaffold-only kernel C5 shipping in no wave) are invalid

## §6 — Disposition

**ACCEPT-rate: 73.7% (line-item) / 89.5% (root-issue collapsed).**

Both rates below the §3Z ≥95% ACCEPT bar. V2 cycle required.

**REVISE-1 dispositions (load-bearing for V2):**

1. **REVISE-1 / Stage-0 binding wave-pin** — Reconcile the three-way divergence (P3-A: ships when wave admits C1/C3/C7; P3-B: UNCONDITIONAL Stage-0-in-W9; SPEC + P3-F: CONDITIONAL W8/W9/W10 on 12-primitive admission). The S-P2 V3 §6.3 verbatim binding ("Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive") supports P3-B's unconditional reading. SPEC §1 must add: "F-V2-P1ABC-RERECORD ships as Stage-0 of W<N> (the first re-admit wave) unconditionally; exit gate = post-rerecord samply trace shows inner-primitive attribution > 0% on at least one of {match_tiny_plain_string, match_number_at_digit, parse_number_direct, skip_ascii_whitespace}." Apply same fix to SPEC §11/§12/§13 — remove "UNLESS" / "if … shipped" conditional language.

2. **REVISE-1 / Wave-numbering convergence** — Resolve P3-B (W9 = R6) vs SPEC (W8 = R6) numbering with CH1. The Stage-0 binding language must name one wave-id consistently.

3. **REVISE-1 / Orphan-kernel closure for C5** — Per `[no-deferrals]` + `[execute-planned-architecture]`: C5 is a load-bearing shortlist candidate (1 of 8 SPEC interventions). It MUST ship in a named wave with a measurable closure threshold, not conditionally behind a 12-primitive-admission census the per-wave plan controls. SPEC §1 must bind C5 to a specific wave-id with a samply-attribution exit-gate threshold (the P3-A §2 C5:121 falsifiability sketch is the threshold authority).

**ACCEPT dispositions (no action needed in V2):**

- 12/12 revert protocols present + REDRESS-slot-naming.
- 9/12 wave exit-gates measurement-closed with no orphan-kernel risk.
- SPEC §1 no-deferrals language verbatim with five proscribed terms.
- 6/8 shortlist candidates carry orphan-kernel-free consumer wiring.
- CH7-V2 verb-tense discipline embedded.
- W6 per-sub-wave revert granularity properly enforced.
- W7 samply-attribution exit-gate is the gold-standard CH6 closure (measurable bench-side evidence).
- W11 close-ceremony forbids paper-close verbatim (line 1050).

**CH6 ACCEPT-rate breakdown for V2 fold:**

- V2 fix the Stage-0 binding (1 root-issue surfacing across 3 wave-sections): expected V2 ACCEPT-rate ≥95% (collapses REVISE-1 to 0).
- V2 wave-numbering reconciliation: depends on CH1's V2 fold; CH6 inherits the resolution.

CH6 V1 disposition: **REVISE-1 (89.5% root-issue / 73.7% line-item); fold Stage-0 wave-pin into V2.**

## §7 — Output for aggregator

Path: `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH6.md` (this file).

ACCEPT-rate: **73.7% line-item / 89.5% root-issue-collapsed** (below §3Z ≥95% bar — V2 fold required).

Cycle disposition: **REVISE-1** (3 line-items / 1 root-issue: F-V2-P1ABC-RERECORD Stage-0 binding three-way divergence creates C5 orphan-kernel risk).

Findings: (a) every wave's exit gate carries measurable closure (samply trace, bench Mbps, grep counts, du-sh floors, cell-state); (b) every wave carries an explicit revert protocol naming a REDRESS slot; (c) SPEC §1:220 forbids deferral verbatim; (d) 8/8 shortlist candidates carry CF-3 3-gate cell with same-wave consumer NAMED; (e) the F-V2-P1ABC-RERECORD Stage-0 binding diverges three ways across P3-A (conditional on C1/C3/C7 admission), P3-B (UNCONDITIONAL Stage-0-in-W9), and SPEC + P3-F (CONDITIONAL on 12-primitive admission in W8/W9/W10), creating a paper-close vehicle whereby C5 (a load-bearing shortlist intervention) can ship in zero waves; (f) wave-numbering divergence between P3-B (W9 = R6 CSS L4) and SPEC (W8 = R6 CSS L4) compounds the Stage-0 binding ambiguity. V2 fold: pin Stage-0 to ONE wave UNCONDITIONALLY with measurable samply-attribution exit-gate threshold; remove "UNLESS" / "if … shipped" conditional language from SPEC §11/§12/§13.
