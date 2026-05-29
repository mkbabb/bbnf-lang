# CH7 — OVERFIT-PRUNE (Pass Alpha SK-V17, cycle V4)

Lens: CH7 OVERFIT-PRUNE. Focus: **no contrivance** — no fixture/FNV/broadcast/fact-stream
re-entry; CSS variants derived from grammar projections, not hand-curated; the path is
**genuinely generalized, not CSS-special-cased**. Adversarial review of
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA..alphaE}.md` + `SYNTHESIS.md` +
`HANDOFF.md` per PASS-ALPHA §3 + ORCHESTRATOR §3W/§3Z.

Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25` (verified
`git rev-parse HEAD` = `1c5bd7a25250640f3a6fcfc00abed11f556f674f`). Every disposition
carries a path:line + the measured/grepped fact it rests on, re-greped at HEAD this cycle.

**Cycle context.** V1 CH7 produced ONE substantive REVISE (the relocated-CSS-overfit
pruning gate missing a falsifiability test) + four citation/number/scope REVISEs; all five
folded clean into V2. V2 CH7 produced exactly TWO REVISEs — a single residual seam: the
`css_l4.toml`-LOC-convergence metric (a TOTALITY artefact) leaked into an SK-V17 close/scan
gate at SYNTHESIS:101 + HANDOFF:122 without the totality-fold caveat the source artefacts
already carried. Both folded clean into V3 (verified V3 CH7 §2: SYNTHESIS:105 + HANDOFF:140
demote the metric to an INFORMATIONAL SK-V18 totality-fold). V3 CH7 closed at **13 ACCEPT /
0 REVISE / 0 REJECT** — 100%. The cohort-wide V3 REVISEs (V3-CH1-a, V3-CH1-b, F1 orphan) all
targeted **count-correction** sibling text (the 24-vs-6 broadcast-row reconciliation and the
αD O5 grammar-derivation-not-TOML-LOC relabel) — NONE was a CH7/contrivance disposition.

This V4 review (a) verifies the V4 cohort folds landed at HEAD, (b) re-scans the full
αA-E + SYNTHESIS + HANDOFF surface for any new or surviving contrivance vector, and (c)
records the one cohort-coherence residue this lens found.

---

## §1 — Verification battery (re-greped at HEAD `1c5bd7a25`, this lens, this cycle)

| Claim under test | Artefact cite | Ground truth (verified this cycle) | Verdict |
|---|---|---|---|
| `StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`/`begin_compound` ABSENT from skinny | SYNTH:36-37; HANDOFF:16-18; αD §0:24-26; αE 0:133-134 | `grep -rln` across `skinny/crates/` = **EMPTY** (✓) | CONFIRMED |
| fixture parse fns = **148** (not stale 187) | αC §5:278; αD O5:154; αE 0:109 | `grep -c "fn parse_" generated_real_typed.rs` = **148** (✓) | CONFIRMED |
| `W5C_REQUEST_FACT_PROFILES` decl :336, iterated :567/:611, selected :299 | αC §3:176; αD O5:154; αE C0:169; SYNTH:57,179; HANDOFF:139,160 | `grep -n` lib.rs: decl :336, iterated :567,:611, selected :299 (✓ exact) | CONFIRMED |
| 7 per-grammar `RequestFactsProfile` literals carry `emitter: RuntimeEmitterKind::RequestFacts` in regen_css.rs | αE C1:233; SYNTH:179; αC §3:174 | `grep -n "emitter:" regen_css.rs` = :45,:63,:81,:99,:117,:135,:153 = **7** (✓ exact) | CONFIRMED |
| broadcast rows = **24** (not 6); the 25th css_l4/ match is prose, not a row | αA §2:141-146; αB §V4-fold:33-42; αC §4:228-237; αD §0/§5; SYNTH:155-164; HANDOFF:52-57 | `grep -c '^\| css_l4/.*/direct_to_struct/main '` = **24**; `grep -c 'css_l4/'` = **25**; `grep -nE 'W6.*css\|tape.*direct_to_struct'` = **EMPTY** (✓ all three) | CONFIRMED |
| benched CSS Track 1 = `Result<String,String>` fact-stream at :596 | αA §0:53; αB §0.1:84-86; αC §3:168; αD §0:38; αE C0:169; SYNTH:48; HANDOFF:21 | `nonjson_css_l4.rs:596` `track1_facts -> Result<String,String>` (cited consistently) | CONFIRMED |
| `sheets_witness` = 25 LOC stub (24 witness + 1 mod), no `.bbnf`/parser/`BackendRule`; BBNF-self absent | αD §0:56-60; αE 0:115; SYNTH §0.4:248; HANDOFF:93 | `wc -l sheets_witness/*.rs` = 24 + 1 = **25** (✓) | CONFIRMED |
| i8mm grep-clean-absent from skinny | αD O3:152; αE 0:114; SYNTH:63; HANDOFF:167 | `grep -rn i8mm skinny/crates/` = **EMPTY** (✓) | CONFIRMED |
| `parse_4_digits`/`parse_4_digits_dotprod` udot orphan never called in skinny runtime | αD O3:152; αE C4a:417; SYNTH:61-62 | `grep -rn parse_4_digits skinny/crates/runtime/` = **EMPTY** (orphan ✓); decl at digit_mac.rs:27, dispatch wrapper :12 | CONFIRMED |
| `select_classifier`/`PrimitiveKernels`/`OnceLock`/`lo6_table_admissible` grammar-general SIMD entry | αE C2:306; SYNTH §0.1:117; αD O3:152 | dispatch.rs:42 `select_classifier(alphabet: &'static [u8;64])`, :50 `PrimitiveKernels`, :59 `OnceLock`, :101 `lo6_table_admissible` (✓ exact) | CONFIRMED |
| `css_l4.toml` / `runtime-projections/` grep-absent from skinny (TOTALITY-only) | αC §0:34; αD §0:39 + O5:154; SYNTH:111; HANDOFF:144 | `find skinny -name css_l4.toml -o -path '*runtime-projections*'` = **EMPTY** (✓) | CONFIRMED |

Every load-bearing fact this lens depends on is grep-verified true at HEAD. No uncited
number survives into V4. The 24/25/EMPTY broadcast triple — the seam V3-CH1-b corrected
cohort-wide — is exactly as every V4 artefact states it.

---

## §2 — The V3 → V4 fold ledger (did the cohort folds land?)

CHALLENGE V3 returned the cohort at 59/61 ACCEPT (96.7%), with the residual REVISEs all
being **count-correction** dispositions on sibling artefacts (none a CH7 disposition). Each
is verified folded at HEAD:

| V3 disposition | Target | V4 status | Evidence |
|---|---|---|---|
| **V3-CH1-a** — stale/self-contradictory cross-artefact reconciliation meta-note | alphaA-results-extraction.md:139-148 | **FOLDED** | αA:141-147 now reads "All cohort artefacts state 24 / lines 112-135 as of V3 … the V2 '6' undercount is resolved across the cohort"; the ground-truth grep (24, lines 112-135) is retained. αA frontmatter v4_fold_dispositions:18-19 records the fold. |
| **V3-CH1-b** — grep-substring mislabel (25 vs 24) | alphaC-redress-digest.md:227-231 | **FOLDED (content)** | αC:228-237 now reads "25 substring matches, of which 24 are `^\| css_l4/` table rows (112-135) and the 25th (:154) is a prose REDRESS-127 companion reference, not a row; there is NO admitted/distinct W6 typed CSS row (`grep 'W6.*css\|tape.*direct_to_struct'` = EMPTY)". Grep-verified true this cycle (24/25/EMPTY). |
| **F1 orphan** — css_l4.toml LOC label → grammar-derivation | alphaD-validated-invalidated.md:154 (O5 row) | **FOLDED** | αD O5:154 now reads "The skinny-greppable exit gate is grammar-derivation, NOT TOML-LOC count … the 594-line css_l4.toml LOC convergence is a TOTALITY metric (SK-V18 fold), INFORMATIONAL only, NOT an SK-V17 close/exit gate (SYNTHESIS §0.1)" — verbatim agreement with SYNTHESIS §0.1:111 / HANDOFF:144 / αC §0. |

All three folds are landed with verified evidence. The αE candidate content is UNCHANGED
from V3 (it converged at 11 ACCEPT / 0 REVISE in V3; αE V4 changelog:12-21 records this and
the cycle-stamp-only advance). The SYNTHESIS:10-15 + HANDOFF:8-11 V4 changelogs both record
the same three folds plus the carried V3 substantive folds (the sheets_witness (b′) repair,
the 6→24 reconciliation, the css_l4.toml-is-totality demotion).

---

## §3 — Per-section dispositions (V4)

### alphaA (results extraction) — ACCEPT

The 24-row broadcast is correctly framed as a **PERMANENT-PRE-BLOCK, not a baseline**
(§2:135-138, §7:361: `not_admitted:SK-V15-W0-broadcast-diagnostic`/`AUDIT-FALSIFIED`,
grep-verified 24, "must NOT be lifted as a baseline"). The "zero ADMITTED typed CSS rows"
framing is the honest anti-contrivance position — there is no SK-V16 per-corpus typed-CSS
row to delta against, so no fabricated endpoint is admitted (§2:137-138, §7:319-322). The
"no CSS special-case" O(1)-checkpoint claim (§4:219) is cited to
`sk-v16-w6p2-o1-checkpoint-report.md:25-37` — the grammar-neutral banked win, generic across
all 9 grammars, not a CSS-specific patch. The benched CSS Track 1 is correctly disclosed as
the `RuntimeEmitterKind::RequestFacts` fact-stream String (§0:51-53; nonjson_css_l4.rs:596),
NOT a typed product, with the typed retime (3.093) explicitly a *separate* core-tree path
(§5:262). The V4 reconciliation note (:141-147) is the folded V3-CH1-a. No contrivance.

### alphaB (competitor deltas) — ACCEPT

Every per-corpus endpoint cell is marked `[INF]` **inline** (§2:165-168: animate↔164,
bootstrap↔70, material↔60, tailwind↔51 all carry `[INF]`/`[RNG][INF assign]`); the
endpoint-to-corpus assignment is UNMEASURED-PENDING (§2:182-185) and §6.2:287-296 forbids any
SK-V17 wave exit-gate keying on an inferred endpoint until the N≥50 harness emits the split.
Only the corpus-aggregate ~14×/~36× rows are cited as measured. The cssparser-is-a-flaw-probe
plane discipline (§0:48-64, §3:189-217) is the CSS analogue of the SK-V6 `utf8_lossy`
finding and prevents a token-scan win from masking as a >SOTA claim — this is anti-contrivance
at the comparator plane. The V4 broadcast-count reconciliation (§:28-42, §verification:346-357)
is the folded cohort residue; the deltas are computed over the benched corpus set, not the
RESULTS broadcast rows, so no number moves. No fabricated per-corpus measurement is admitted.
No contrivance.

### alphaC (REDRESS digest) — ACCEPT (content), with ONE coherence residue (§4 below; not a contrivance vector)

This remains the strongest overfit-prevention artefact. The skinny overfit fingerprint is
correctly localized: `W5C_REQUEST_FACT_PROFILES` + the 7 `RequestFacts` registrations + the
148 fixture parse fns (§0:34, §5:278), all skinny-greppable. The **retirement clause**
(§3:195-199, table 2b/3 §:350-351: CH3/CH5 fail if the 7 `RequestFacts` registrations or
`W5C_REQUEST_FACT_PROFILES` still drive an admitted row) is the load-bearing anti-relocation
gate. §2b:147-148 forbids any new hand-coded per-grammar profile/route table parallel to
`W5C_REQUEST_FACT_PROFILES` (relocated overfit, Lock 14, LOCKS.md:380-387). The `css_l4.toml`
is correctly classified a core-tree artefact absent from skinny (§0:34). The Lock 2
name-retirement (`StructLayout`→`Layout`/`LayoutFacts`, LOCKS.md:160) is cited correctly
(§2b:126-129). The V3-CH1-b broadcast-count correction is landed in the content (§:228-237,
grep-verified 24/25/EMPTY). No fact-stream/FNV/broadcast/fixture re-entry survives. Airtight
on substance. (The one residue — a stale "cycle V3" header stamp while the cohort is V4 — is
documented in §4; it is cosmetic, not a contrivance, and does not alter any pre-block.)

### alphaD (validated/invalidated ledger) — ACCEPT

The Anti-relabel pruning gate is present and falsifiable (O1:150: "wave FAILS if CSS needs
match arms / hand-curated packing constants JSON does not, OR if the CSS regen profile array
does not trend toward the JSON shape"). The folded F1 orphan is landed: O5:154 now keys the
skinny exit gate to **grammar-derivation, NOT TOML-LOC count**, and tags the `css_l4.toml`
LOC convergence a TOTALITY SK-V18-fold metric, INFORMATIONAL only — verbatim agreement with
SYNTHESIS §0.1. The JSON-witnessed-only generality downgrade is honest (§0:53-65: "tape-
generality demonstrated today is JSON-witnessed only"; sheets_witness a 25-LOC stub;
bbnf-self absent). O2:151 correctly strikes the sheets_witness projection target as
structurally non-dischargeable (no `.bbnf`/parser/`BackendRule`). The 148→fixture-overfit
retire-list is in O5. No contrivance.

### alphaE (candidate shortlist) — ACCEPT

C2 remains the model anti-contrivance candidate: it REUSES the checkasm-gated
`select_classifier`/`PrimitiveKernels` surface (dispatch.rs:42,50 — grep-verified), keys on
the grammar's alphabet (NOT CSS literals, §C2:322-323), produces ONLY a `Vec<u32>` index
("speed comes from the scan, never from dropping structure", :336-338), and
`lo6_table_admissible` (dispatch.rs:101 verified) is the honest scalar-fallback when the CSS
alphabet collides mod 0x3f — genuine generalization, not a CSS special-case. C0/C1 carry the
relocated-overfit pruning gate (C1:289-296: routing must derive from the `.bbnf` rule, NOT
per-rule-id match arms; "every residual CSS entry must name the `.bbnf` rule it derives
from"). C4a admits unconditionally (orphan udot wiring, scalar + checkasm present); C4b is
GATED behind a Wave-5 re-profile proving the digit leaf is top-N tailwind self-time (no
orphan kernel, :450-453). The sheets_witness "emit a ValueRef view" clause is struck
(option-b′, :55-63) and the exercised projection riders are JSON + CSS only. The V4 changelog
(:12-43) documents the cohort reconciliation with αE content UNCHANGED. No contrivance.

### SYNTHESIS.md (αF contract draft) — ACCEPT

The four V3 substantive folds are carried and the three V4 count-correction folds are landed
(changelog :5-16). The benched-surface note (:31-68) localizes every citation to
`skinny/crates/`. The Layout-driven-projection close gate (:111) enumerates the CSS routing
("declaration/selector/aggregate/numeric/function/color rule sets, 0/1/N value-list collapse,
selector span-vs-single, dir-pseudo synthesis, hex packing, color-component order") **only as
the surface that must be RETIRED as hand-coded branching and DERIVED from the `.bbnf` grammar
/ `BackendRule` shape, NOT lost and NOT re-hardcoded** — this is the correct anti-overfit
framing (the enumeration names what must be derived, not a curated list to preserve), with
the skinny-greppable pruning gate ("no per-rule-id match arms in the skinny generic crates
that JSON does not need; every residual CSS routing entry names the `.bbnf` rule"). The
`css_l4.toml`-LOC clause is demoted to an INFORMATIONAL SK-V18 totality-fold with the
wrong-tree-dishonesty rationale stated inline (:111). The `tape_activated` telemetry column
is honestly defined as "NOT satisfiable by a grep in `crates/core/`" (Section 2:370). The
generality clause (§0.4:241-264) is witness-honest: JSON+CSS only, sheets_witness
non-dischargeable, four-grammar claim NOT proven in SK-V17. The gate rejects single-tuple
broadcast (Section 2:393-394) and rejects `sheets_witness` as a
`projection_generality_exercise` value (:372). Strong contract; no residual seam.

### HANDOFF.md — ACCEPT

The V4 changelog (:5-11) records the three count-correction folds + zero orphan REVISE. The
CH7 scan scope (:138-146) is exactly the four skinny-greppable tests (a)
`W5C_REQUEST_FACT_PROFILES` retired/not-relocated, (b) no per-rule-id match arms / hand-curated
packing-color constants JSON does not need, (c) every residual CSS routing entry names its
`.bbnf` rule, (d) the CSS regen profile array trends toward the JSON emitter shape — then
explicitly: "The `css_l4.toml` LOC convergence is NOT a CH7 scan gate … an SK-V18 totality-fold
metric only, noted not gated" (:143-146). The benched-substrate disclosure (:13-24) cites the
skinny tape correctly; the `W5C_REQUEST_FACT_PROFILES` retirement is a pre-block ("RETIRE, do
not extend or relocate into projection data — the overfit re-entry seam", :160-161); the
no-second-substrate Lock-1 clause (:171-174) and the `tape_activated`-not-by-`crates/core/`-grep
gate (:217-220) are present. The generality scope (:90-100) is JSON+CSS-only, sheets_witness
non-dischargeable. No residual seam.

---

## §4 — The one cohort-coherence residue (NOT a contrivance vector; REVISE)

**αC header stamp lag.** Every cohort artefact and both contract files advance to **cycle
V4** (αA frontmatter `pass: PASS-ALPHA cycle V4`; αB:1 "cycle V4"; αD:1 "cycle V4"; αE:1
"cycle V4"; SYNTHESIS:5 "cycle V4"; HANDOFF:5 "cycle V4") — but **alphaC-redress-digest.md:1
and :3 still read "(cycle V3)" / "cycle V3."** The αC *content* is correct and current: the
V3-CH1-b broadcast-count fold is landed verbatim at αC:228-237 (grep-verified 24/25/EMPTY
this cycle), and the αE V4 changelog (:30-34) explicitly names this fold as "VERIFIED landed"
at "alphaC-redress-digest.md:227-231." So this is a **stamp-only** lag, not a content defect,
and it touches NO pre-block, candidate, gate, or measured number.

Why CH7 raises it: the cohort's load-bearing reconciliation sentence — repeated in αA, αB,
αD, SYNTHESIS, HANDOFF — is "**All cohort artefacts state 24 / lines 112-135 as of V3**."
αC is the *source* of that count discipline (it owns the broadcast pre-block, §4). An αC
that still self-labels "cycle V3" while every sibling claims cohort-wide V4 agreement is a
minor reconciliation residue that a CH1/coherence re-scan would flag; CH7 surfaces it because
the broadcast-count seam is the exact vector this lens guards (the 24-row broadcast is a
PERMANENT pre-block, and its citation chain must be internally coherent). This is the SOLE
disposition below ACCEPT.

- **path:line:** `restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md:1,3`
- **concrete fix:** advance the αC header stamp "(cycle V3)" → "(cycle V4)" on line 1 and
  "cycle V3" → "cycle V4" on line 3, and add a one-line V4 changelog note mirroring the
  siblings ("V4: cohort cycle-stamp advance; content unchanged from V3 — the V3-CH1-b
  broadcast-count correction at :228-237 is the landed fold, grep-verified 24/25/EMPTY at
  HEAD `1c5bd7a25`"). No content edit; this is a stamp + changelog-line reconciliation only.

This is an **orphan-free** REVISE: it is self-contained to αC's frontmatter, requires no
edit to any sibling (the siblings already correctly assert the cohort state), and folds in
one line.

---

## §5 — The contrivance ledger (my lens's bottom line, V4)

| Contrivance vector | Re-entry blocked? | By which artefact clause | CH7 V4 verdict |
|---|---|---|---|
| Fact-stream String as admitted product | YES (permanent pre-block + retirement clause) | αC §3:195-199; αE C0:207-214; SYNTH §0.4:199-208; HANDOFF:157-161 | clean |
| 24-row broadcast (one tuple ×24 rows) | YES (permanent, no re-frame; gate rejects single-tuple) | αA §2:135-138; αC §4:239-260; SYNTH Section 2:393-394; HANDOFF:162-164 | clean (count 24/25/EMPTY verified) |
| FNV closed-enum arbiter | YES (bench-quarantine only) | αC §5a:271-274; SYNTH §0.4:212-215; HANDOFF:165 | clean |
| Fixture-named parse fns / per-corpus capacity consts (148-fn surface) | YES (named for retirement; tuning grammar-derived, not corpus literal) | αC §5:278-280; αD O5:154; αE C0/C4a/C4b; SYNTH §0.4:212-215 | clean (number 148, verified) |
| x86 / AVX / SVE | YES (out-of-scope, diagnostic-only) | αC §6; αE pre-blocks; SYNTH §0.4:216-217; HANDOFF:166-167 | clean (i8mm grep-EMPTY verified) |
| **CSS-special-casing relocated into projection DATA** (hex packing / color order / rule-id sets as TOML or `match rule_id`) | YES — falsifiable skinny-greppable pruning gate (every residual CSS entry names its `.bbnf` rule; wave FAILS if CSS needs match arms JSON does not) | αE C1:289-296; αD O1:150; SYNTH §0.1:111; HANDOFF:138-142 | clean |
| Inferred per-corpus numbers baked into goalset | YES (UNMEASURED-PENDING; no exit-gate may key on them) | αB §2/§6.2; SYNTH §0.5:284-287 | clean |
| Citing core-tree (`StructLayout`/`TapeStructBuilder`/`css_l4.toml`) as the benched surface | YES (the V2 residual seam stayed closed through V3+V4) | SYNTH §0.1:111; HANDOFF:144 (informational SK-V18 fold, NOT a gate) | clean |
| NEON win from dropping structure (not from scan) | YES — C2 produces ONLY a `Vec<u32>` index; preserve-rich-ast + 8-field equality re-proven before any admit | αE C2:336-338; αA §3:192-195; SYNTH preserve-rich-ast gate :113 | clean |
| Cohort count-citation incoherence (αC stamp lag) | residue surfaced (§4 REVISE) | αC:1,3 | **REVISE (stamp-only; content clean)** |

The headline: **every contrivance vector this lens owns is bound.** No fixture/FNV/broadcast/
fact-stream re-entry; no x86/SVE admission (i8mm grep-EMPTY); no flattened-AST contrivance
(preserve-rich-ast + 8-field equality re-proven before admit); no relocated-projection-data
overfit (the every-residual-CSS-entry-names-its-`.bbnf`-rule pruning gate is present and
falsifiable); no wrong-tree close gate (the `css_l4.toml`-LOC metric is demoted to an
informational SK-V18 fold in both SYNTHESIS and HANDOFF and stays demoted). The path is
genuinely generalizing, not CSS-special-cased — proven by the C2 NEON reuse
(`select_classifier`/alphabet, not CSS literals — grep-verified dispatch.rs:42,101), the
Vec<u32>-index-only discipline (speed from scan, not from dropping structure), the
W5C-retirement-and-derive-from-grammar pruning gate, and the JSON-witnessed-only generality
honesty (no fabricated four-grammar claim; sheets_witness non-dischargeable, grep-verified
25-LOC stub). The SOLE sub-ACCEPT disposition is a cosmetic cohort-coherence residue (αC's
"cycle V3" header stamp lagging the cohort's V4 advance) — content-clean, orphan-free,
one-line fold.

---

## §6 — Required revisions (V4)

**ONE (cohort-coherence, not contrivance):**

1. **αC header cycle-stamp lag** — `alphaC-redress-digest.md:1,3` still read "(cycle V3)" /
   "cycle V3" while every cohort sibling (αA, αB, αD, αE) and both SYNTHESIS/HANDOFF advance
   to cycle V4 and assert "All cohort artefacts state 24 … as of V3" cohort-wide. αC is the
   source of that broadcast-count discipline; its self-label must match the cohort it
   anchors. Fix: advance the stamp to V4 + add a one-line V4 changelog ("content unchanged
   from V3; V3-CH1-b broadcast-count fold landed at :228-237, grep-verified 24/25/EMPTY at
   HEAD `1c5bd7a25`"). No content edit. Orphan-free.

No contrivance vector surfaces on the αA-E + SYNTHESIS + HANDOFF surface. Every load-bearing
grep fact is true at HEAD `1c5bd7a25`. The αE candidate content converged at V3 and is
unchanged at V4.

---

## §7 — Disposition summary

**14 reviewable sections** across alphaA-E + SYNTHESIS + HANDOFF (αF is realised as the
SYNTHESIS + HANDOFF pair at the tranche root per PASS-ALPHA §6; there is no separate
`alphaF-contract-draft.md`, which this lens accepts as the αF deliverable). The 14: αA, αB,
αC, αD, αE, SYNTHESIS, HANDOFF as primary sections, with αE counted across its candidate
surface (C0/C1/C2/C3/C4a/C4b/§2-4) and the contract files across their gate surfaces — for a
clean disposition the lens scores at the artefact level (7 artefacts) plus the αE
candidate-bank as a distinguished section and the SYNTHESIS §0.1/§0.4/§0.5/Section-2 and
HANDOFF CH7-scope/pre-block as distinguished gate surfaces, totalling 14 reviewable units.

- **ACCEPT (13):** αA (all §); αB (all §); αC (all content §0-§8 — the broadcast pre-block,
  retirement clause, tree-disambiguation all airtight); αD (all §); αE (§0, C0, C1, C2, C3,
  C4a, C4b, §2-§4); SYNTHESIS (all sections — §0.1/§0.4/§0.5/Section-2 gates clean, the
  css_l4.toml seam stays demoted); HANDOFF (all sections — CH7-scope + pre-blocks clean).
- **REVISE (1):** αC header cycle-stamp lag (αC:1,3) — cosmetic cohort-coherence residue,
  content-clean, orphan-free, one-line fold (§4, §6).
- **REJECT (0):** No section proposes a fixture/FNV/broadcast/fact-stream re-entry, an
  x86/SVE admission, a flattened-AST contrivance, or a relocated-projection-data overfit.

Counts: **ACCEPT 13 / REVISE 1 / REJECT 0** → 13/14 = **92.9% ACCEPT** (this lens).

The single REVISE is a stamp-only reconciliation, not a contrivance vector; the contrivance
ledger (§5) is wholly clean. The path is genuinely generalized, not CSS-special-cased; no
fixture/FNV/broadcast/fact-stream re-entry; no x86/SVE; no flattened-AST contrivance; no
relocated-projection-data overfit; no wrong-tree close gate. The §3Z convergence is met from
CH7's standpoint on substance (the prior two cycles V2→V3 closed the only contrivance seam
this lens owned, and V3 was 100% on this lens); the lone V4 disposition is a single
orphan-free cosmetic stamp fold that folds clean into V5 with no candidate/gate impact.
