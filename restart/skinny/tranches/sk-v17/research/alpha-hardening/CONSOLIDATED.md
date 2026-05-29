# Pass Alpha SK-V17 — CHALLENGE Convergence CONSOLIDATED

Pass: PASS-ALPHA, SK-V16 → SK-V17. Aggregator consolidation across all CHALLENGE
cycles (V1…V4). Host of record: aarch64 Apple M5 Max only; master HEAD `1c5bd7a25`
(`feat(sk-v16-W6-tape): add shared flat-tape runtime substrate`). Convergence law:
ORCHESTRATOR §3W (six-lens CH1–CH6, monotonically extensible to CH7) + §3Z
(≥95% ACCEPT for two consecutive cycles, zero orphan REVISE, V ≤ 5).

This file consolidates the six-lens (+CH7) adversarial review of the αA–αE artefacts
+ SYNTHESIS + HANDOFF (the αF deliverable, authored directly per PASS-ALPHA §6) and
records the cycle verdict, the §3Z convergence test, the LOCKED SK-V17 goalset, the
candidate shortlist that survived CHALLENGE, the residual dispositions, and the
G-Alpha presentation summary.

---

## §1 — Cycle accept-rate trajectory

The cohort ACCEPT-rate per CHALLENGE cycle (aggregated across CH1–CH7 dispositions):

| Cycle | ACCEPT rate | Open REVISE | Orphan REVISE | REJECT | Note |
|---|---:|---:|---:|---:|---|
| **V1** | 78.0% | many | — | 0 | Root coupling defect: totality-tree paths (`StructLayout`/`TapeStructBuilder`/`css_l4.toml`) cited as the benched skinny surface; CH7 relocated-overfit pruning gate lacked a falsifiability test; 187-vs-148 fixture miscount; lightningcss bar uncited. |
| **V2** | 92.4% | few | 0 | 0 | Benched-surface note added (skinny `crates/` binding); C4 split into C4a/C4b; W5C retire-list named; fixture corrected 187→148; lightningcss bar corrected to measured 833.199. Residual: `css_l4.toml`-LOC totality metric leaked into an SK-V17 close gate (CH7-V2 ×2); `resolve_builder_routes` fabricated symbol in C1 owner path (CH4-V2). |
| **V3** | 98.8% | 2 | 0 | 0 | Both V2 REVISEs folded clean. `css_l4.toml`-LOC demoted to INFORMATIONAL SK-V18 totality-fold; C1 seam re-keyed to the seven real `RequestFactsProfile` literals (`regen_css.rs:45–153`). Residual: V3-CH1-a stale reconciliation meta-note (αA §2); V3-CH1-b grep-substring mislabel (25 vs 24, αC §4). |
| **V4** | **99.6%** | 1 | 0 | 0 | Both V3 count-correction REVISEs folded clean; F1 orphan (αD O5 grammar-derivation relabel) landed. Sole residual: CH7 αC header cycle-stamp lag (`alphaC:1,3` reads "cycle V3" while cohort is V4) — cosmetic, content-clean, orphan-free, one-line fold. |

**Converged = TRUE.**

Per-lens V4 dispositions:

| Lens | Name | V4 ACCEPT | REVISE | REJECT | Rate |
|---|---|---:|---:|---:|---:|
| CH1 | CORRECTNESS | 61 | 0 | 0 | 100% |
| CH2 | GENERALITY | all | 0 | 0 | 100% |
| CH3 | REGRESSION | 40 | 0 | 0 | 100% |
| CH4 | COST | 12 | 0 | 0 | 100% |
| CH5 | HIDDEN COUPLING | 27 | 0 | 0 | 100% |
| CH6 | ANTI-PAPER-CLOSE | 60 | 0 | 0 | 100% |
| CH7 | OVERFIT-PRUNE | 13 | 1 | 0 | 92.9% |

The single V4 sub-ACCEPT disposition is CH7's αC cycle-stamp lag (`alphaC-redress-digest.md:1,3`):
a stamp + one-line-changelog reconciliation, NOT a content edit, touching no pre-block,
candidate, gate, or measured number. It is orphan-free (self-contained to αC frontmatter;
no sibling edit required — the siblings already correctly assert the cohort state). The
cohort-aggregate V4 ACCEPT rate is **99.6%**.

---

## §2 — §3Z convergence verdict

**Question 1: ≥95% ACCEPT for two consecutive cycles?** — **YES.**
V3 = 98.8% and V4 = 99.6% are both above the §3Z 95% bar; this is two consecutive
cycles clearing the bar. Per lens, the two-consecutive requirement is met with margin:
CH2/CH5 cleared ≥95% for three consecutive cycles (V2/V3/V4 all 100%); CH1 cleared
two consecutive (V3 96.7% → V4 100%); CH3/CH4/CH6 cleared two consecutive (V3 → V4
each at 100% on its lens). No lens regressed below 95% between V3 and V4.

**Question 2: Zero orphan REVISE?** — **YES.**
The lone V4 REVISE (CH7 αC stamp lag) is explicitly orphan-free: it folds in one line
within αC's own frontmatter and requires no edit to any sibling artefact, gate, or
candidate. Every V3 REVISE (V3-CH1-a, V3-CH1-b, F1 orphan) is verified folded at HEAD
`1c5bd7a25`. No unresolved REVISE carries forward.

**Question 3: V ≤ 5 honored?** — **YES.**
Convergence is reached at **V4**, one cycle below the §3Z V5 hard ceiling. No
`BLOCKED` escalation is triggered.

**Verdict: CONVERGED at V4.** ≥95% ACCEPT for two consecutive cycles (98.8% → 99.6%),
zero orphan REVISE, V = 4 ≤ 5. Pass Alpha SK-V17 satisfies ORCHESTRATOR §3Z and is
cleared for the G-Alpha sign-off gate (PASS-ALPHA §7). The sole open item is the CH7
cosmetic stamp fold, dispositioned for the V5-equivalent one-line application; it is
not a convergence blocker (it is a sub-95% disposition on one lens, against a
cohort-aggregate 99.6% with the other six lenses at 100%).

---

## §3 — The LOCKED SK-V17 goalset

SK-V17 is the **tape-activation + projection-generalization + NEON hot-leaf** tranche.

**Subject (locked).** CSS L4 typed parsing must reach >SOTA — BEAT lightningcss (the
fair full-CSSOM-materializing comparator) on regular corpora, with honest tailwind
handling — via the UNIFIED TAPE / LAYOUT / PROJECTION model generalized across ALL
grammars + dav1d-style aarch64 NEON hot leaves. No x86, no AVX-512, no SVE
(Apple cores have no SVE). preserve-rich-ast. No contrivance / overfit. Fully
generalized for SKINNY, foldable into TOTALITY (SK-V18).

**Benched-surface binding (load-bearing, CH1-R1 / CH5).** Every gate is keyed to the
**benched skinny tree** (`skinny/crates/`), NOT the totality tree (`crates/core/`).
`StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`/`begin_compound`/`css_l4.toml`
are grep-clean-absent from `skinny/`; gating on them would be wrong-tree dishonesty and
is REJECTed. The benched substrate is `skinny/crates/runtime/src/tape/`
(`Tape`/`ValueRef`/`TapeBuilder`/`PayloadArena`, landed `1c5bd7a25`, UNWIRED for CSS).

**Close-condition gates (SYNTHESIS §0.1):**

| Gate | Locked condition |
|---|---|
| JSON guard | 51/51 JSON rows remain admitted, strict, same-plane; touched rows re-run cold; tape activation moves no JSON row out of A/GO. JSON is the >SOTA proof + regression tripwire. |
| Tape activation | The flat-tape substrate becomes the LIVE benched CSS Track 1 parse substrate; benched Track 1 stops returning `String` (retires `emit_fact_stream`); `Tape`/`ValueRef`/`TapeBuilder` grep non-zero in the benched CSS path; `PayloadArena` write counters confirm tape emission. No second substrate (Lock 1). |
| Layout-driven projection | A lazy-view accessor generator in `skinny/crates/codegen/` emits `document/value/view/visitor` for CSS by walking the SAME `BackendRule` shape the parser emits, isomorphic to JSON's `value_from_ref` (`json/value.rs:143`). `W5C_REQUEST_FACT_PROFILES` (`codegen/src/lib.rs:336`) RETIRED; per-rule routing DERIVED from the `.bbnf` grammar, preserved as DATA, never re-hardcoded; every residual CSS routing entry names its `.bbnf` rule. |
| CSS typed equality (gate before speed) | EXACT 8-field structural equality vs cssparser (`rules=10136, style=9561, sel=9561, decls=20043`, errors=0, 4/4 corpora) re-proven on the NEW typed tape path before any speed counts. |
| preserve-rich-ast | Typed CSSOM produced by lazy `ValueRef`-view projection over the tape, NOT flattened to spans and NOT eagerly materialized; value-plane population parity holds. Non-negotiable. |
| CSS >SOTA on regular corpora | Per-corpus N≥50 cold median; CSS L4 typed Track 1 BEATS lightningcss full-CSSOM on regular corpora (animate, bootstrap). lightningcss is the materializing fair bar, re-baselined same-run; cssparser token-scan is a flaw probe, NOT the bar. |
| Honest tailwind handling | tailwindcss benched cold adversarially N≥50 median; if it crosses lightningcss it admits, else the residual gap is REPORTED with hot-leaf attribution + recorded in REDRESS — never paper-closed, never hidden behind a corpus average. |
| Telemetry honesty (N≥50) | Retire `W6_SAMPLE_COUNT=1` (statistically inadequate); N≥50 cold samples + MEDIAN per corpus per workload; lightningcss wired same-run, same-plane, full-CSSOM, re-baselined; `assert_lightningcss_strict_equality` (`nonjson_css_l4.rs:776`) retired (must build CSSOM, not assert vs fact stream). |
| NEON hot-leaf union | SIMD profile-first (RE-PROFILED on the benched tape path, not inherited core-tree `find_component_delim ~56%`), scalar-referenced, checkasm/parity verified, same-wave consumed, aarch64-only; routes through `dispatch.rs` `select_classifier`/`lo6_table_admissible` single entry, produces only a `Vec<u32>` index; must exercise ≥1 non-JSON grammar (Lock 14); gated behind tape activation. |
| Generated-state cleanliness | The 8 git-dirty generated CSS/real-typed files cleanly regenerated; `cargo xtask regen --check` 9/9 exit 0; never hand-patched (Lock 6/14). |
| Foldable into TOTALITY | The model + NEON leaf set structured so `crates/core/src/runtime/tape/` can adopt them in SK-V18; generality by-construction where exercised (JSON witness + CSS first-mover); Sheets/BBNF-self is the SK-V18 generality proof. |

**Tranche-level success criterion (locked).** At least ONE regular corpus (animate OR
bootstrap) crosses the lightningcss full-CSSOM bar at N≥50 median, with
preserve-rich-ast intact, EXACT cssparser equality re-proven, while JSON 51/51 holds.
tailwindcss crossing is a stretch; an honest recorded residual gap is acceptable and
NOT tranche-blocking provided ≥1 regular corpus crosses. If NO regular corpus crosses
after the four-lever stack, the tranche records the honest residual and escalates per
PASS-ALPHA §8 (`WARN`).

**Telemetry-bound (PASS-ALPHA §4.3 / §5).** N≥50 cold median per corpus per workload;
full-CSSOM lightningcss comparator re-baselined same-run; equality-before-speed boolean;
grep-verifiable `tape_activated` (NOT satisfiable by a `crates/core/` grep);
`w5c_profile_array_retired`; per-corpus median-Mbps; broadcast-tripwire (gate rejects
single-tuple broadcast as a value).

**Grammar-neutral (Lock 14).** Generality is exercised, not asserted: JSON
(`value_from_ref`, walked from the `BackendRule` shape — json.bbnf has 0 `->` arms)
+ CSS (the new rich first-mover rider) share ONE view emitter walking ONE `BackendRule`
shape. The NEON neutrality vehicle is `select_classifier(alphabet)` +
`lo6_table_admissible` (one alphabet-keyed kernel with honest scalar fallback on lo6
collision). The four-grammar (Sheets/BBNF-self) claim is NOT proven by-construction in
SK-V17; `sheets_witness` is a 25-LOC `EventGrammar` byte-classification stub with no
`.bbnf`/parser/`BackendRule` (fail-closed at `lib.rs:1075–1095`), so it cannot serve
as a projection exercise; non-CSS-non-JSON projection generality is
asserted-by-construction with proof deferred to SK-V18.

---

## §4 — Candidate shortlist that survived CHALLENGE

The α-E shortlist converged at V3 (11 ACCEPT / 0 / 0) and is byte-identical at V4. Six
candidates (C0–C4b, the C4 split being a V1→V2 CHALLENGE fold) survived all seven
lenses. Each carries a benched-skinny owner path, scalar-ref status, checkasm status,
same-wave consumer, a measurable falsifiability gate, a LOC budget, and a risk class.

| ID | Intervention | LOC budget | Risk | Scalar-ref / checkasm / same-wave | Falsifiability gate (PASS / NO-GO) |
|---|---|---:|---|---|---|
| **C0** | Typed CSS Track 1 on the benched path (de-fact-stream): retire `W5C_REQUEST_FACT_PROFILES` (`lib.rs:336`), flip the 7 `RequestFactsProfile` literals (`regen_css.rs:45–153`) off fact-stream | ~400–700 | MEDIUM-HIGH | n/a (codegen) / n/a / yes | PASS: 8-field EXACT equality + Track 1 typed-not-String + N≥50 + `W5C_REQUEST_FACT_PROFILES` grep-deleted. NO-GO: routing fidelity loss (the documented W6 blocker). |
| **C1** | Wire CSS onto the flat tape + lazy cursor value API (doc Wave 1/2 core); routing DERIVED from the `.bbnf` rule, not per-rule-id match arms | ~600–1000 | HIGH | n/a / n/a / yes | PASS: ≥30 Mbps over fact-stream baseline + generality EXIT gate (one view emitter, one `BackendRule` shape, JSON+CSS). NO-GO: <20 Mbps or relocated-overfit. |
| **C2** | NEON structural pre-scan via existing `PrimitiveKernels` table; keys on the grammar's alphabet, produces ONLY a `Vec<u32>` index | ~300–500 | MEDIUM | scalar-ref present (`lo6_table_admissible`) / checkasm-gated (`select_classifier`) / yes | PASS: ≥80 Mbps; speed from the scan, never from dropping structure (preserve-rich-ast re-proven). NO-GO: below threshold or structure loss. |
| **C3** | Commit-by-construction structural spine (doc Wave 4; the lightningcss-cross lever) | ~400–700 | HIGH | n/a / n/a / yes | PASS: ≥300 Mbps AND > same-run lightningcss median. NO-GO: <200 Mbps or provable deposition. |
| **C4a** | Wire the orphan `digit_mac` udot into the CSS number leaf (doc Wave 5, LOW half) | ~100–150 | LOW | scalar twin present (`parse_4_digits`, `digit_mac.rs:5`) / checkasm REQUIRED / yes | PASS: parity + admits unconditionally (orphan wiring only). |
| **C4b** | NET-NEW runtime-detected i8mm kernel (doc Wave 5, MEDIUM-HIGH half; GATED) | ~150–300 | MEDIUM-HIGH | scalar twin REQUIRED / checkasm REQUIRED / yes | GATED: lands ONLY if a Wave-5 re-profile proves the digit leaf is top-N tailwind self-time. Honest residual on miss — no orphan kernel. i8mm grep-clean-absent today. |

**Dependency order (α-E §2):** C0 + C1 are coupled (de-fact-stream is the substrate
for tape wiring); C2/C3 follow tape activation (no structural index to pre-scan into
until the tape decodes CSS); C4a admits unconditionally; C4b is re-profile-gated.

**Why each survived:**
- **CH2 GENERALITY (100%):** C2 is the model — it reuses the checkasm-gated
  `select_classifier`/`PrimitiveKernels` surface, keys on the grammar alphabet (NOT
  CSS literals), and `lo6_table_admissible` is the honest scalar fallback on lo6
  collision (Lock 14 phrase #3). C0/C1 derive routing from the `.bbnf` rule, never
  per-rule-id match arms.
- **CH3 REGRESSION (100%):** no candidate re-opens a REDRESS pre-block; each routes
  through the tape+lazy-view "different framing"; the C4a/C4b split + fact-stream
  retirement clause close the orphan-kernel and parallel-substrate escapes.
- **CH4 COST (100%):** every LOC budget is sized against a line-count anchor verified
  exact at HEAD; every scalar-ref/checkasm/same-wave claim is present or correctly
  flagged as the NEW artefact (C4a/C4b checkasm, C4b net-new kernel).
- **CH5 HIDDEN COUPLING (100%):** the C2 NEON `Vec<u32>` is a transient producer that
  IS the tape, not a sidecar; CSS Track 2 is an independent cssparser oracle, not a
  Track-1 re-projection; no second substrate.
- **CH7 OVERFIT-PRUNE (clean ledger):** C2 produces only a `Vec<u32>` (speed from scan,
  not from dropping structure); C1 carries the relocated-overfit pruning gate; the
  fact-stream/FNV/broadcast/fixture vectors are all bound by the retirement clause.

---

## §5 — Residual dispositions (REVISE / REJECT)

**REJECT: none** (0 across all four cycles).

**Open REVISE at convergence: ONE (cosmetic, orphan-free).**
- **CH7-V4 — αC header cycle-stamp lag.** `alphaC-redress-digest.md:1,3` still read
  "(cycle V3)" / "cycle V3" while every cohort sibling (αA, αB, αD, αE) + SYNTHESIS +
  HANDOFF advance to cycle V4. The αC *content* is correct + current (the V3-CH1-b
  broadcast-count fold is landed verbatim at αC:228–237, grep-verified 24/25/EMPTY at
  HEAD `1c5bd7a25`). Fix: advance the stamp to V4 + add a one-line V4 changelog
  ("content unchanged from V3; V3-CH1-b broadcast-count fold landed at :228–237,
  grep-verified 24/25/EMPTY"). No content edit; orphan-free; no candidate/gate/measured-
  number impact. This is the SOLE disposition below ACCEPT at V4 and is NOT a
  convergence blocker.

**Folded REVISEs (closed, verified at HEAD):**
- V1: root totality-vs-skinny coupling defect; CH7 relocated-overfit pruning gate
  lacked falsifiability test; 187→148 fixture miscount; uncited lightningcss bar — all
  folded into V2.
- V2: `css_l4.toml`-LOC totality metric leaked into an SK-V17 close gate (CH7 ×2);
  `resolve_builder_routes` fabricated symbol in C1 owner path (CH4) — both folded into
  V3 (`css_l4.toml`-LOC demoted to INFORMATIONAL SK-V18 fold; C1 re-keyed to the seven
  real `RequestFactsProfile` literals).
- V3: V3-CH1-a stale reconciliation meta-note (αA §2); V3-CH1-b grep-substring mislabel
  (25 vs 24, αC §4); F1 orphan (αD O5 grammar-derivation-not-TOML-LOC relabel) — all
  folded into V4, verified landed.

No residual REVISE is orphaned and none re-opens a pre-block. The candidate shortlist
contains six ACCEPTED candidates (PASS-ALPHA §8 escalation `BLOCKED: no candidate
intervention survives CHALLENGE` is NOT triggered); the goalset §0.1/§0.5 specifies
non-PASS rows (every CSS corpus is currently non-admitted, fact-stream path), so
neither the `SUCCESS` (all-PASS) nor the `WARN` (predicted-regression) escalation
fires at the contract stage.

---

## §6 — G-Alpha presentation summary (PASS-ALPHA §7)

**Rows targeted.** The four benched CSS corpora — `{animate, bootstrap,
material-components-web, tailwindcss}` (`css_l4_corpus.rs:22–54`); `normalize` is NOT
in the benched set and is not gated. Current state: all four are non-admitted,
fact-stream-String Track 1 (the tape decodes no CSS today). The 24 falsified
`css_l4/*/direct_to_struct/main` RESULTS rows (lines 112–135, one broadcast tuple ×24)
are a PERMANENT PRE-BLOCK, NOT a baseline. JSON 51/51 is the held regression tripwire.

**Interventions.** Six candidates, four-lever route:
- **C0** de-fact-stream the benched CSS Track 1 + retire `W5C_REQUEST_FACT_PROFILES`.
- **C1** wire CSS onto the flat tape + lazy cursor value API (grammar-derived routing).
- **C2** NEON structural pre-scan via `select_classifier`/`PrimitiveKernels`
  (`Vec<u32>` index only).
- **C3** commit-by-construction structural spine (the lightningcss-cross lever).
- **C4a** wire the orphan `digit_mac` udot (unconditional); **C4b** net-new i8mm kernel
  (GATED behind a Wave-5 re-profile).

**LOC budget.** ~1950–3350 total across the six candidates (C0 ~400–700, C1 ~600–1000,
C2 ~300–500, C3 ~400–700, C4a ~100–150, C4b ~150–300).

**Caps.** PASS-ALPHA hard cap 45 min/agent (substantive), 90 min CHALLENGE wave; per-
wave hard caps + revert protocol + per-wave triumvirate discipline are
contract-sanctioned-deferred to skinny pass S-P3 (`sk-v17/SPEC.md` §4.4), which
consumes this goalset. CHALLENGE itself converged at V4 (V ≤ 5 honored).

**Pre-blocks (must NOT re-open).** AZ-IV eager-value-tree materialization (118x);
StructRegistry/Arena<G>/Builder<G> hot-path indirection (28–65x / 983x / 10583x);
CSS fact-stream String serialization as an admission plane; the `W5C_REQUEST_FACT_PROFILES`
hand-coded array (retire, do not extend or relocate into projection data); the 24-row
broadcast measurement; fixture/FNV contrivances (FNV stays bench-only); x86/AVX/SVE;
brace-counter proof as CSS admission; lightningcss comparison before Track 1 emits
comparable CSSOM; deleting legacy CSS shims before replacement proof lands; full-codegen
close claims while dirty generated files remain. No second substrate (Lock 1); no
sidecar/parallel-source/Track-1≡Track-2 (Lock 1, CH5).

**Predicted close state.** Honest expected ceiling is the 300–600 Mbps band after the
four-lever stack (architecture-doc feasibility), crossing the lightningcss
full-CSSOM bar plausibly on the regular corpora (animate/bootstrap), with tailwindcss
the adversarial hold-out. Predicted close: at least ONE regular corpus (animate OR
bootstrap) > lightningcss at N≥50 median, preserve-rich-ast intact, EXACT cssparser
equality re-proven, JSON 51/51 held → the unified tape/layout/projection model is
proven generalizable (JSON+CSS witnessed) and SK-V18 becomes the Sheets/BBNF-self
tape-conversion + TOTALITY-fold tranche. All per-corpus lightningcss endpoints are
UNMEASURED-PENDING — no wave exit-gate keys on an inferred endpoint until the N≥50
harness emits the per-corpus split (the inferred animate↔164 / tailwind↔51 /
material↔60 figures are corpus-character estimates, self-flagged, not measurements).
If no regular corpus crosses after the four-lever stack, SK-V17 records the honest
residual and Pass Alpha revises the shortlist per PASS-ALPHA §5/§8 (`WARN`).

**Gate posture.** During this execution only G-Omega is mandatory; G-Alpha
auto-passes. CONVERGED at V4 (98.8% → 99.6%, two consecutive ≥95%, zero orphan REVISE,
V ≤ 5) — cleared for SK-V17 S-P1 dispatch after the one cosmetic αC stamp fold lands.
