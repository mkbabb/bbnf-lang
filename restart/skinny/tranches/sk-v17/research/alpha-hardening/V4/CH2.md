# CH2 GENERALITY — Pass Alpha SK-V17 (cycle V4)

Lens: CH2 Generality (PASS-ALPHA §3). Reviewer focus: does the goalset + every
candidate respect **Lock 14 grammar-neutrality** — works for JSON / Sheets /
BBNF-self, not just CSS, OR is honestly re-framed as a per-grammar template
surface; is the unified tape / layout / projection grammar-general?

Host: aarch64 Apple M5 Max only. HEAD of record: `1c5bd7a25`
(`git rev-parse HEAD` re-confirmed = `1c5bd7a25250640f3a6fcfc00abed11f556f674f`).
Every disposition cites `path:line` / measured fact.

Subjects reviewed: `research/alpha/alphaA..alphaE.md`, `SYNTHESIS.md`,
`HANDOFF.md`. (No `alphaF-contract-draft.md` exists at `research/alpha/`; α-F's
output IS `SYNTHESIS.md` + `HANDOFF.md` per PASS-ALPHA §2 row α-F.)

Method: **CH2 converged at V3 (100% ACCEPT, zero orphan REVISE — V1→V2→V3 was
80.6% → 100% → 100%).** §3Z requires ≥95% on two consecutive cycles; CH2 cleared
that bar at V2/V3. V4 is therefore a convergence-hold re-verification, not a
re-litigation. The V4 cohort edits are pure changelog / count-correction folds
(per the V4 changelogs at `alphaE:12-43`, `SYNTHESIS:10-16`, `HANDOFF:8-11`):
V3-CH1-a (stale cross-artefact meta-note), V3-CH1-b (grep-substring mislabel),
and the F1 orphan (`alphaD:154` O5 grammar-derivation-not-TOML-LOC relabel). **None
touches a generality surface.** CH2-V4 (a) re-verifies the single load-bearing V2
fold root (CH2-V2-F1, the `sheets_witness`-non-dischargeable (b′) fix) still holds
against the codebase at `1c5bd7a25`; (b) confirms the F1-orphan O5 relabel
introduced no new generality claim; (c) adversarially re-scans the whole V4 cohort
for any fresh Lock-14 defect.

---

## §0 — Codebase re-verification of the load-bearing generality facts (at `1c5bd7a25`)

Every Lock-14 disposition in this cohort rests on a small set of grep-checkable
facts. All re-verified at HEAD this cycle:

| Fact (the generality claim it underwrites) | Re-verified value | Citation |
|---|---|---|
| `sheets_witness` is a non-dischargeable projection target (no `BackendRule` to walk) | **25 LOC, 2 files** (`event_grammar_witness.rs` 24 + `mod.rs` 1) | `find sheets_witness -type f` (verified) |
| Only one `.bbnf` exists in skinny (so the projection rider claim is JSON+CSS, no fourth grammar to exercise) | **`skinny/grammars/json.bbnf` is the only `.bbnf`** | `ls skinny/grammars/` = `json.bbnf` |
| `value_from_ref` is walked from `BackendRule` SHAPE, not `->` arms (proves projection generator is grammar-shape-driven, not JSON-special) | **json.bbnf has `0` `->` arms** | `grep -c '\->' skinny/grammars/json.bbnf` = 0 |
| sheets/bbnf are fail-closed negative controls (so "emit a ValueRef view for sheets_witness" has no runnable target — option-a non-dischargeable) | **`w5a_sheets_bbnf_fail_closed_through_runtime_contract` asserts `Err(CodegenError::Lowering(... "frontend closure missing import closure"))` for `["google_sheets","bbnf"]`** | `lib.rs:1075-1095` (read, verified) |
| NEON neutrality vehicle is one alphabet-keyed kernel, not a CSS branch | **`select_classifier(alphabet: &'static [u8;64])` :42 + `lo6_table_admissible(alphabet)` :101** | `dispatch.rs:42,101` (verified) |
| `W5C_REQUEST_FACT_PROFILES` is the Lock-14-phrase-#1 retire target (hand-coded CSS routing) | **const `:336`, selected `:299`, iterated `:567,:611`** | `codegen/src/lib.rs:299,336,567,611` (verified) |
| i8mm is a NET-NEW kernel (C4b), not an orphan already present | **grep-clean-absent from `skinny/crates/`** | `grep -rln i8mm skinny/crates/` = empty |
| udot orphan (C4a) is present with its scalar fallback | **`parse_4_digits` :5 (scalar twin), `parse_4_digits_dotprod` :27** | `digit_mac.rs:5,27` (verified) |
| Totality symbols are NOT in the benched skinny tree (wrong-tree dishonesty guard) | **`StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`/`begin_compound` grep-clean from `skinny/crates/`** | `grep -rln` = empty |
| `css_l4.toml` is a TOTALITY artefact, not a skinny close gate | **grep-clean-absent from `skinny/`** | `find skinny -name css_l4.toml` = empty |

**All nine facts hold at `1c5bd7a25`.** The (b′) fold is real, not paper: the
`sheets_witness` non-dischargeability is grep-proven (25 LOC, no `.bbnf`, no
`BackendRule`, fail-closed at `lib.rs:1075-1095`); the projection generator's
grammar-shape-drivenness is proven by json.bbnf having 0 `->` arms (so
`value_from_ref` cannot be reading `->` arms — it walks the `BackendRule` shape);
and the NEON neutrality vehicle is a single alphabet-keyed kernel.

---

## §1 — V4-fold re-verification (the three V4 edits, scanned for a NEW generality claim)

V4 made exactly three edits beyond V3 (per the V4 changelogs). Each is scanned for a
fresh Lock-14 defect.

### CH2-V4-S1 — F1-orphan O5 relabel introduces no generality claim (ACCEPT)

The F1 orphan closed the V3 disjunction at `alphaD:154` (O5 row). Re-read at HEAD:
the row now reads "**The skinny-greppable exit gate is grammar-derivation, NOT
TOML-LOC count:** every residual CSS routing entry must name the `.bbnf` rule it
derives from, and the CSS regen profile array (`regen_css.rs:45-153`) must trend
toward the JSON emitter shape. The 594-line `css_l4.toml` LOC convergence is a
TOTALITY metric (SK-V18 fold), INFORMATIONAL only, NOT an SK-V17 close/exit gate
(SYNTHESIS §0.1)." This is the verbatim agreement with `SYNTHESIS:111` and
`HANDOFF:143-146`. **Generality assessment:** the relabel STRENGTHENS the Lock-14
posture — it moves the exit gate from a non-benched-tree LOC count (`css_l4.toml`,
grep-clean-absent from `skinny/`, re-verified) onto a skinny-greppable
grammar-derivation property ("names its `.bbnf` rule", "trends toward the JSON
shape"). Gating an SK-V17 close on a non-benched totality file would have been the
wrong-tree dishonesty the contract elsewhere REJECTs; demoting it to INFORMATIONAL
while keeping the substantive anti-overfit gate is the correct generality call. No
new per-grammar special-casing introduced. **ACCEPT.**

### CH2-V4-S2 — V3-CH1-a/b count-correction folds touch no generality surface (ACCEPT)

V3-CH1-a (`alphaA:138-148` stale cross-artefact reconciliation rewritten to "all
cohort artefacts state 24 / lines 112-135 as of V3 … the V2 '6' undercount is
resolved") and V3-CH1-b (`alphaC:227-231` grep-substring mislabel corrected to "25
substring matches, of which 24 are `^| css_l4/` table rows and the 25th :154 is a
prose REDRESS-127 companion reference, not a row") are CH1-correctness edits about
the W8R broadcast row count. The broadcast count is a regression-tripwire fact
(`SYNTHESIS:145-164`, `HANDOFF:50-57`), not a generality fact; the substantive
generality-relevant conclusion (zero ADMITTED typed CSS rows; the broadcast is
pre-blocked at `SYNTHESIS:209-211`/`HANDOFF:162-164`) is unchanged by the count
correction and the pre-block introduces no per-grammar special-casing. Re-verified
the count claim is internally consistent across all cohort artefacts at V4. **ACCEPT.**

### CH2-V4-S3 — alphaE candidate content is byte-stable from V3 (no generality regression possible)

`alphaE:18-21` states the candidate content "is therefore UNCHANGED from V3 (it
converged); only this changelog and the cycle stamp advance to V4." Re-read C0-C4b
(`alphaE:163-492`) and §3 (`alphaE:524-564`): the generality EXIT gates, the
neutrality-vehicle framing, the `sheets_witness`-non-dischargeable language, and the
"no candidate may claim four-grammar or fleet-wide generality; each binds its witness
to JSON+CSS explicitly" closing discipline (`alphaE:555-557`) are verbatim the V3
text CH2-V3 ACCEPTed. Since no candidate, gate, owner path, or pre-block changed,
there is no surface on which a new generality defect could have entered α-E. **ACCEPT.**

---

## §2 — Per-section dispositions (the convergence-hold census, re-verified at V4)

The V3 census (31 comparable + 3 fresh-surface = 34) is carried; each is re-confirmed
against HEAD `1c5bd7a25`. No section regressed; the three V4 edits added no new
generality surface beyond CH2-V4-S1/S2/S3.

### alphaA — results extraction

- **§0 tree-correction + core-tree-is-fold-target (`alphaA:48-70`)** — ACCEPT.
  Totality symbols grep-clean from `skinny/crates/` (re-verified §0). Wrong-tree
  dishonesty guard intact.
- **§results cross-artefact reconciliation (`alphaA:138-148`, V3-CH1-a fold)** — ACCEPT
  (CH2-V4-S2). Count edit, no generality surface.
- **§4 O(1)-checkpoint genericity + §7 lever neutrality vehicle (`alphaA:203,217,349-356`)**
  — ACCEPT. "generic across all grammars, no CSS special-case" + "JSON/CSS/Sheets/BBNF
  share this one scanner vocabulary" is the alphabet-keyed `select_classifier(alphabet)`
  abstraction (`dispatch.rs:42`, re-verified), where the alphabet is the only grammar
  datum — Lock 14 phrase #3.

### alphaB — competitor deltas

- **§0/§2/§3/§4 plane taxonomy + deltas** — ACCEPT. lightningcss-full-CSSOM-fair-bar /
  cssparser-token-scan-plane-mismatch is grammar-domain-honest; no generality defect.
- **§5 JSON comparator guard** — ACCEPT. The shared-substrate-touch → re-run-JSON-strict
  tripwire is the cross-grammar regression guard CH2 wants.

### alphaC — REDRESS digest

- **§intro core-tree-vs-skinny map + §227-231 grep-substring correction (V3-CH1-b fold)**
  — ACCEPT (CH2-V4-S2). The count relabel touches no generality surface; the core/skinny
  map (Lock 2 `StructLayout`→`Layout`) is sound.
- **§2b/§5b/§3 fact-stream + W5C + fixture pre-blocks** — ACCEPT. "emitter derives tape
  ops from `BackendRule`/`LayoutFacts` shape, NOT a hand-coded per-grammar profile table"
  is grammar-neutral and load-bearing.
- **§4-§8 broadcast / FNV / x86 / ledger** — ACCEPT. aarch64-NEON-only is alphabet-keyed
  not grammar-keyed; i8mm grep-clean-absent re-verified (§0).

### alphaD — validated/invalidated ledger

- **§intro + §validated witness discussion** — ACCEPT. core-tree→skinny substrate map +
  "only `json` and `sheets_witness` carry an `EventGrammar` witness; sheets_witness has
  no shape to lower; exercised projection riders are JSON + CSS" is CH2-V2-F1-clean.
- **§3 O1 tape wiring (`alphaD:150`)** — ACCEPT. "Generality is JSON-WITNESSED only…
  Sheets/BBNF are by-construction-not-by-exercise" + the anti-relabel pruning gate
  ("wave FAILS if CSS needs match arms / hand-curated packing constants JSON does not")
  is Lock-14 clean.
- **§3 O2 lazy view (`alphaD:151`)** — ACCEPT (V2 REVISE discharged, re-verified at V4).
  "Generality is exercised on CSS+JSON only — sheets_witness is NOT a projection target…
  structurally non-dischargeable, NOT a live disjunction… an explicit SK-V18 fold
  target." The 25-LOC/no-`.bbnf`/no-`BackendRule`/fail-closed rationale matches §0
  re-verification.
- **§3 O3 NEON / O4 spine (`alphaD:152-153`)** — ACCEPT. `select_classifier`/
  `lo6_table_admissible` neutrality vehicle verified; "Grammar-general (the emitter,
  not a CSS patch)" for O4.
- **§3 O5 codegen unification (`alphaD:154`, F1-orphan fold)** — ACCEPT (CH2-V4-S1). The
  grammar-derivation-not-TOML-LOC relabel strengthens the Lock-14 exit gate.
- **§closing generality bullet (`alphaD:205-214`)** — ACCEPT. "Every candidate must
  generalize beyond CSS to JSON (witnessed today) / Sheets / BBNF-self (by-construction,
  witnessed only when the generator emits a non-CSS-non-JSON rider — Lock 14)" is the
  honest witnessed posture.

### alphaE — candidate shortlist

- **§0 ground-truth + architecture-doc translation (`alphaE:94-152`)** — ACCEPT. The
  sheets-witness ground-truth row (`:115`) states the non-dischargeability inline (24-LOC
  `EventGrammar`, no `.bbnf`/parser/`BackendRule`, fail-closed `lib.rs:1075-1090`);
  i8mm-absent row (`:114`); 148-fn (not 187) correction. All re-verified §0.
- **C0 de-fact-stream + W5C retirement (`alphaE:163-214`)** — ACCEPT. derive-from-
  BackendRule pre-block (`:212-214`) is Lock-14 phrase #1 respected; `W5C_REQUEST_FACT_PROFILES`
  retire target re-verified at `lib.rs:336`.
- **C1 tape wiring + generality EXIT gate (`alphaE:216-296`)** — ACCEPT (V2 REVISE
  discharged, byte-stable at V4 — CH2-V4-S3). EXIT gate (`:249-268`): "exercised
  projection riders are JSON + CSS ONLY… `sheets_witness` is NOT a dischargeable
  projection target… non-CSS-non-JSON projection… is an SK-V18 fold target." The
  no-relocated-overfit / derive-from-`.bbnf`-rule pruning test (`:285-296`) is the
  Lock-14 anti-overfit discipline.
- **C2 NEON structural pre-scan (`alphaE:298-363`)** — ACCEPT, and commend. The strongest
  grammar-neutral candidate: reuses `select_classifier`/`PrimitiveKernels`/
  `lo6_table_admissible` (`dispatch.rs:42,50,101` verified), falls back to scalar honestly
  on lo6 collision (`:330-333`, NOT a CSS special-case), forbids "CSS-specific scanner
  vocabulary" (`:360`). JSON is the genuine non-CSS witness (`json/scan.rs`).
- **C3 commit-by-construction spine (`alphaE:364-410`)** — ACCEPT. Non-depositing-Alt
  detection is a generic codegen property exercised on JSON+CSS, grammar-shape-driven
  (`:390-394`).
- **C4a udot orphan wiring (`alphaE:412-443`)** — ACCEPT. Grammar-general number-leaf
  primitive (`parse_4_digits` :5 scalar twin re-verified); density tuning grammar-derived
  not a corpus literal (`:443`).
- **C4b NET-NEW i8mm kernel (`alphaE:445-492`)** — ACCEPT. i8mm grep-clean-absent
  (re-verified §0); GATED behind re-profile; ONE fn-pointer via `PrimitiveKernels`
  OnceLock, not a subsystem (`:455-459`). Grammar-neutral.
- **§3 cross-cutting falsifiability discipline (`alphaE:524-564`)** — ACCEPT (V2 REVISE
  discharged, byte-stable at V4). The Lock-14-witnessed-not-asserted bullet (`:539-557`)
  scopes the witness to JSON+CSS, marks `sheets_witness` non-dischargeable, defers Sheets
  to SK-V18, states "No candidate may claim four-grammar or fleet-wide generality." The
  `simd_non_json_exercise = css_l4` carve-out (`:551-553`) is the genuinely dischargeable
  distinct exercise (CSS+JSON share the `select_classifier(alphabet)` kernel).

### SYNTHESIS.md (α-F)

- **Benched-surface note (`SYNTHESIS:31-68`)** — ACCEPT. Full core-tree→skinny translation;
  wrong-tree dishonesty explicitly REJECTed (`:40`); totality symbols grep-clean re-verified §0.
- **§0.1 JSON guard / tape activation / CSS equality / preserve-rich-ast (`SYNTHESIS:109-113`)**
  — ACCEPT. Keyed on the benched skinny tape; `tape_activated` "NOT satisfiable by a grep
  in `crates/core/`" (`:370`).
- **§0.1 Layout-driven projection gate (`SYNTHESIS:111`)** — ACCEPT (V2 REVISE discharged).
  Closing sentence: "`sheets_witness`… CANNOT serve as a projection-generator exercise;
  non-CSS-non-JSON projection generality is asserted-by-construction with proof deferred to
  SK-V18." The css_l4.toml-LOC demotion is generality-honest (re-verified grep-clean §0).
- **§0.1 Foldable-into-TOTALITY gate (`SYNTHESIS:119`)** — ACCEPT. "the non-CSS-non-JSON
  projection rider (Sheets/BBNF-self) is the SK-V18 generality proof."
- **§0.3 receiver goalset (`SYNTHESIS:178`)** — ACCEPT. Owner paths are the benched skinny
  tree; totality paths flagged SK-V18 fold target.
- **§0.4 pre-blocks + generality clause (`SYNTHESIS:185-264`)** — ACCEPT (V2 REVISE
  discharged). The generality clause (`:241-264`) is the full (b′) fix: "exercised
  projection riders are JSON + CSS only", `sheets_witness` "NOT a viable third exercise"
  with the 25-LOC/no-`.bbnf`/no-`BackendRule`/fail-closed rationale, and the Lock-14-phrase-#2
  honesty ("the contract does NOT claim the Lock 14 CSS+Sheets minimum is *met* in
  SK-V17 — that minimum is the SK-V18 close-out"). The `simd_non_json_exercise = css_l4`
  carve-out (`:260-264`) is distinct + dischargeable.
- **§0.5 per-corpus close conditions (`SYNTHESIS:266-301`)** — ACCEPT. Benched set fixed to
  `{bootstrap, tailwindcss, material-components-web, animate}` (`css_l4_corpus.rs:22-54`);
  `normalize` "NOT in this set"; per-corpus endpoints honestly UNMEASURED-PENDING.
- **§0.6 strict comparator gate (`SYNTHESIS:303-320`)** — ACCEPT. Grammar-domain honest.
- **Section 2 telemetry binding (`SYNTHESIS:354-376`)** — ACCEPT (V2 REVISE discharged).
  The column split: `projection_generality_exercise` (`:372`, `json`|`css_l4`,
  sheets_witness explicitly forbidden) + `simd_non_json_exercise` (`:376`, `css_l4`).
  The two-axis-generality taxonomy is the cleanest in the cohort.
- **Section 3 trajectory (`SYNTHESIS:396-419`)** — ACCEPT. "the unified
  tape/layout/projection model is proven generalizable (JSON+CSS witnessed) and SK-V18
  becomes the Sheets/BBNF-self… tranche" — honest witnessed posture; gates on
  `animate OR bootstrap`.

### HANDOFF.md (α-F)

- **Benched-substrate disclosure + current state + four-lever route (`HANDOFF:13-104`)** —
  ACCEPT. Names the benched skinny tape; flags core-tree as SK-V18 fold target; 6→24
  broadcast reconciliation (`:50-57`) introduces no generality claim (CH2-V4-S2).
- **Generality scope (`HANDOFF:90-100`)** — ACCEPT (V2 REVISE discharged). "the projection
  generator's exercised riders are JSON + CSS only… `sheets_witness` CANNOT serve as a
  projection-generator exercise… SK-V17 does NOT claim the Lock 14 CSS+Sheets minimum is
  met (witness is JSON+CSS, not Sheets — Lock 14 phrase #2, `LOCKS.md:386-387`). The NEON
  SIMD leaf's non-JSON exercise IS `css_l4`… sound and distinct from the projection-generality
  scope." (b′) fix propagated into the HANDOFF directly.
- **CH7 overfit-prune scope (`HANDOFF:131-146`)** — ACCEPT. The css_l4.toml-is-totality
  noted-not-gated framing matches CH2-V4-S1.
- **Pre-blocked routes + hidden-coupling (`HANDOFF:148-185`)** — ACCEPT. Comprehensive,
  grammar-neutral; "no second substrate" Lock-1 clause (`:171-174`) does not introduce
  per-grammar branching.
- **Next move + close criterion (`HANDOFF:187-236`)** — ACCEPT. Close criterion is
  `animate OR bootstrap`; `tape_activated` PayloadArena-counter-proven not core-tree-grep
  (`:217-220`); generality posture self-contained and (b′)-clean.

---

## §3 — Disposition summary

Total sections dispositioned: **31** comparable census + **3** fresh-V4-surface findings
(CH2-V4-S1/S2/S3, all ACCEPT) = **34**.

- **ACCEPT: 34**
- **REVISE: 0**
- **REJECT: 0**

ACCEPT rate: 34/34 = **100%** (31/31 = **100%** on the comparable census).

**Zero orphan REVISE.** No disposition is left dangling; there is no open generality
defect anywhere in the V4 cohort.

The single load-bearing fold root (CH2-V2-F1, the `sheets_witness`-non-dischargeable
(b′) repair) is **re-verified intact at `1c5bd7a25`** across all six inherited sites
(SYNTHESIS §0.1/§0.4/Section-2, alphaE C1/§3, alphaD O2) plus the HANDOFF generality
scope. The three V4 edits (F1-orphan O5 grammar-derivation relabel; V3-CH1-a/b
count corrections; byte-stable α-E candidate content) introduce **no new generality
defect** — the O5 relabel in fact STRENGTHENS the Lock-14 posture by moving the exit
gate off a non-benched totality LOC count onto a skinny-greppable grammar-derivation
property.

### What CH2-V4 affirms as Lock-14-clean and load-bearing

The unified tape/projection model **is** grammar-general where exercised: JSON is the
real witness (`value_from_ref` walked from the `BackendRule` shape — json.bbnf has
**0** `->` arms, re-verified, so it cannot be reading `->` arms), CSS is the new
first-mover rider, both share one view-emitter walking one `BackendRule` shape. The
NEON neutrality vehicle (`select_classifier(alphabet)` + `lo6_table_admissible`,
`dispatch.rs:42,101`) is one alphabet-keyed kernel with an honest scalar-fallback on
lo6 collision — Lock 14 phrase #3 exactly. The contract claims no generality it cannot
test: the projection generator's non-CSS-non-JSON generality is honestly
asserted-by-construction with proof deferred to SK-V18 (`sheets_witness` is a 25-LOC
`EventGrammar` witness with no `.bbnf`/no `BackendRule`/fail-closed at `lib.rs:1075-1095`,
all re-verified), and the SIMD-leaf non-JSON generality is honestly bound to the
dischargeable `css_l4` exercise. The telemetry two-axis split
(`projection_generality_exercise` vs `simd_non_json_exercise`) keeps the two distinct
generality axes from being conflated. The derive-from-`.bbnf`-rule no-relocated-overfit
pre-block + the `W5C_REQUEST_FACT_PROFILES` retire-list (`lib.rs:336`) are
grammar-neutral.

**Convergence: 100% ACCEPT clears the §3Z 95% bar on the CH2 lens, with zero orphan
REVISE. This is the THIRD consecutive measured cycle at ≥95% (V2 100% → V3 100% →
V4 100%), satisfying the §3Z two-consecutive-cycle requirement with margin. CH2 holds
convergence at V4.**
