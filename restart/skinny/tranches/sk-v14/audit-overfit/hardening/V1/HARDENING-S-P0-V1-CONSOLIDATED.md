# S-P0 CHALLENGE V1 — CONSOLIDATED Aggregator

Authored 2026-05-23 by the SK-V14 S-P0 V1 aggregator after the seven
lens dispositions (CH1-CH7) landed write-only under `restart/skinny/
tranches/sk-v14/audit-overfit/hardening/V1/`. Authority: `restart/
prompts/ORCHESTRATOR.md §3Z` step 4 + `restart/skinny/tranches/sk-v14/
audit-overfit/hardening/V1/CHALLENGE-CONTEXT.md §6` (aggregator
binding). Source-of-truth lens files: `CH1.md`, `CH2.md`, `CH3.md`,
`CH4.md`, `CH5.md`, `CH6.md`, `CH7.md` — all untracked at this moment,
all committed atomically alongside this CONSOLIDATED file per the
write-only protocol.

## §0 — V1 verdict

| Lens | Anchors | ACCEPT | REVISE | REJECT | NEW | ACCEPT-rate |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| CH1 CORRECTNESS | 7 | 6 | 2 | 0 | 4 | **85.7 %** |
| CH2 GENERALITY | 8 | 7 + 1 NOTE | 0 | 0 | 1 | **100 %** |
| CH3 REGRESSION | 33 | 30 | 3 | 0 | 0 | **90.9 %** |
| CH4 COST | 32 | 32 | 0 | 0 | 0 | **100 %** |
| CH5 HIDDEN COUPLING | 7 | 7 | 0 | 0 | 0 (2 forward notes) | **100 %** |
| CH6 ANTI-PAPER-CLOSE | 30 | 24 | 6 | 0 | 1 + 1 inherit | **80.0 %** |
| CH7 OVERFIT-PRUNE | 38 | 38 | 0 | 0 | 0 | **100 %** |
| **Aggregate** | **155** | **145** | **11** | **0** | **5** | **~93.5 %** |

**Aggregate weighted ACCEPT: ~93.5 % (145 of 155 dispositions).** Below
the §3Z 95 % floor by ~1.5 points; orphan REVISEs (11) require V2 fold.
Zero REJECTs across all seven lenses; five NEW findings (mostly
editorial-precision / phrasing-alignment, none architectural).

### §0.1 — Verdict

**S-P0 V1 verdict: NOT-CONVERGED-V2-REQUIRED.** The audit pack's
*substance* is CH-clean end-to-end — the 74-finding aggregate (31 CRIT
+ 20 HIGH + 12 MED + 11 LOW), the 5 axes FAIL + 1 PARTIAL PASS, the
three architectural sequencing constraints (R4 → PRUNE-2; C-1 → C-4;
PRUNE-4 = 9 sub-waves), the 67-file Pattern H census, and the
PRUNE-list-mapping to SK-V14 SYNTHESIS C-1..C-5 (zero orphans) all
hold under spot-verification by CH1, CH3, CH4, CH5, CH7. The defects
are editorial-hygiene + verdict-line-phrasing only:

- CH1 surfaces internal-arithmetic inconsistencies in SYNTHESIS census
  prose (CONFIRMS / NEW partition disagrees with per-axis table sums)
  and editorial-precision drift in A4 line-cites + "Three"/"Four"
  count phrasing.
- CH3 surfaces three audit-trail clarity REVISEs (A4 NEW-1 V13-HONEST
  disambiguation; A6 NEW-HIGH-1 V13-Pattern-G disambiguation;
  SYNTHESIS §2 co-derivation note).
- CH6 surfaces a six-REVISE single-edit-pattern on the A5 verdict-line
  "PASS at SK-V14 starting baseline" phrasing that papers over the
  scaffold clause by anchoring on the contracted post-PRUNE target;
  inheritance into SYNTHESIS §0.1 + §0.2 + §5.1 propagates the
  pattern.
- CH2 surfaces one precision NOTE (A3 H3 HIGH→LOW recalibration, H6
  takes HIGH bar) and one editorial precision (A4 §0 "Three"→"Four")
  and one new LOW finding (CH7-companion lint scope extension to
  codegen-side twin).
- CH5 surfaces two forward V2 binding notes (R4 Track 1/Track 2
  separation language; PRUNE-4 substrate-union closure declaration) —
  neither REVISE.

V2 cycle closes the 11 REVISEs + 5 NEW findings cleanly; V3 confirming
pass over the V2 artefacts establishes the §3Z two-consecutive-cycle
≥ 95 % chain. Path to G-S-P0-CONVERGED is unblocked.

### §0.2 — Disposition pressure summary

The orphan REVISEs cluster on three artefacts:

- **SYNTHESIS-AUDIT-OVERFIT.md** — 5 V2 folds (CH1 census; CH1 "Three"
  + "6 hits"; CH3 §2 co-derivation; CH6 §0.1 + §0.2 + §5.1 verdict
  inheritance; CH2 N-CH2-1 lint glob scope).
- **A5** — 1 single-edit-pattern collapsing 6 CH6 REVISEs (verdict-
  line "PASS at SK-V14 starting baseline" → "FAIL at HEAD, PASS
  conditioned on C-5 + C-4 landing"); plus A5 §4 row 4 action-class
  re-classification per CH6-R3.
- **A4** — 3 folds (CH3 NEW-1 V13-HONEST disambiguation; CH1 line-cite
  refresh; CH1 + CH2 "Three"→"Four").
- **A6** — 1 fold (CH3 NEW-HIGH-1 V13-Pattern-G disambiguation).
- **A3** — 1 fold (CH2 NOTE H3 HIGH → LOW reclassification; H6 takes
  HIGH bar; aggregate count unchanged at 30).
- **A1, A2** — STAND (no V2 folds required).

## §1 — Per-axis convergence digest

### §1.1 — SYNTHESIS-AUDIT-OVERFIT.md (heavy V2 redispatch surface — 5+ folds)

Cross-lens disposition pressure:

| Lens | Disposition | Defect |
| --- | --- | --- |
| CH1 | REVISE | §0.1 + §1.1 + §1.2 + §4.3 + §5.1 census conflict ("63 CONFIRM / 11 NEW" prose vs "54 / 20" per-axis table column-sum); §1.2 NEW-2 lists 4 names under "Three"; A5 "6 hits" should read "8" |
| CH2 | ACCEPT | §3.1 coverage table is grammar-neutral; §3.3 sub-wave count = 9; §2.4 CH7-companion extensions grammar-agnostic |
| CH3 | REVISE | §2 sequencing-constraint inventory presents +1 sub-wave + +3 Pattern-H delta as independent confirmations when both are evidentially co-derived from the single `css_pretty` addition |
| CH4 | ACCEPT | All 74 findings map into existing C-1..C-5 envelopes; sequencing constraints are wall-clock edges (zero LOC drift); CH7-companion extensions fit inside C-3 1.2k-2.0k headroom |
| CH5 | ACCEPT | Sequencing constraints surfaced explicitly (not hidden); §3.1 prune-list mapping accounts for 74 findings with zero orphans; no new C-6+ candidate smuggles in parallel substrate |
| CH6 | REVISE | §0.1 disposition cell for A5 inherits "PASS at SK-V14 starting baseline" paper-close framing; §0.2 prose inherits verbatim; §5.1 final-verdict bullet 2 leads with PASS before conditioning clause |
| CH7 | ACCEPT | Per-axis severity census (31/20/12/11 = 74) is summation arithmetic over per-axis ledgers; PRUNE-4 = 9 sub-waves rests on the §1.1 metadata enumeration; no fake-`@generated` recurrence in audit prose |

V2 fold count: **5 distinct edits** (census reconciliation; "Three"→
"Four"; "6 hits"→"8 hits"; §1.3 co-derivation note; §0.1 + §0.2 +
§5.1 verdict-line phrasing alignment).

### §1.2 — A5 decision-engine (CH6 single-edit-pattern across 5 locations)

| Lens | Disposition | Defect |
| --- | --- | --- |
| CH1 | ACCEPT | 4 findings; resolver-clause PASS / scaffold-clause PARTIAL is source-grounded; decision-CSP self-labelling at `decision_csp.rs:160-164` verified |
| CH2 | ACCEPT | All 4 findings grammar-neutral; W8 per-grammar policy named for genericity; W9 keyed off `BackendShape` not grammar identity |
| CH3 | ACCEPT | Findings 1-2 CONFIRMS V13; finding 3 NEW MED quantification (3 files / 20 hits); finding 4 LOW honest self-labelling; no reopen of v4 §4-6 SCAFFOLD verdict |
| CH4 | ACCEPT | All 4 findings → C-4 (800-1.4k envelope); §4.1 C-1 → C-4 sequencing edge only; zero envelope drift |
| CH5 | ACCEPT | Resolver-to-runtime gap honestly surfaced; resolver-to-lowering wire correct; no shadow path under PARTIAL PASS verdict |
| CH6 | **REVISE (3 of 3)** | A5-CH6-R1 (verdict-line "PASS at SK-V14 starting baseline" papers over scaffold clause); A5-CH6-R2 ("no row currently held" qualifier elides HEAD state); A5-CH6-R3 (§4 row 4 "No-op pending C-4" is deferral-to-future-phase) |
| CH7 | ACCEPT | PARTIAL PASS internally well-formed; PASS not paper-close (the SCAFFOLD-ONLY status of W8/W9 called out explicitly; C-4 wiring obligation routed under §4.1) |

V2 fold: **1 edit-pattern collapses 6 CH6 REVISEs** across A5 §0:11 +
§3:102-107 + SYNTH §0.1 + §0.2 + §5.1. Plus 1 action-class re-class
on A5 §4 row 4 per CH6-R3 (recommended option (b): widen to "Preserve
through PRUNE-5; gate-rejection invariant inside C-4 entry-gates").

### §1.3 — A4 generator-truth (3 folds)

| Lens | Disposition | Defect |
| --- | --- | --- |
| CH1 | **REVISE** | §0 "New findings: 3" conflicts with ledger 11 NEW rows; fixture-lookup "3 of 7" should read "4 of 7"; line-cite drift -2 throughout `json_provider.rs` (cites :62/66/70/74/50 vs actual :60/64/68/72/48); `lib.rs:338-349` should read `:337-347` |
| CH2 | ACCEPT-with-precision-NOTE | A4 §0 line 38 "Three" → "Four" (precision NOTE for V2); 16 findings span CSS L4 + JSON with cross-grammar generality; R4 + CH7 gating recommendations grammar-neutral by construction |
| CH3 | **REVISE** | A4 NEW-1 phrasing "the JSON runtime profile is no cleaner" reads as REVERSAL of V13 §7.1 row 1 `json_provider` HONEST verdict; needs one-clause disambiguation framing V14 as scope extension (file roster) not reversal (sink-derived chunk) |
| CH4 | ACCEPT | All 16 findings + 4 prune-action rows map to existing C-1 / C-3 / C-5 envelopes; CH7-companion extensions fit inside C-3 |
| CH5 | ACCEPT-with-COUPLING-NOTE | NEW-1 (JSON `generated.rs` fake `@generated`) extends scope but does NOT introduce Track 1 ≡ Track 2 collapse — finding 16 LOW preserves track separation; R4 routes through skinny pipeline only |
| CH6 | ACCEPT | All 16 findings cite present-state surfaces; NEW-1/-2/-3 carry executable cites; §4 recommendation table includes sequencing (not deferral) |
| CH7 | ACCEPT (extension verified) | NEW-1 extends CH7-1 scope from 7 → 8 sites cleanly; five-criterion ceiling holds |

V2 fold count: **3 distinct edits** (NEW-1 V13-HONEST disambiguation;
line-cite refresh -2; "Three"→"Four" §0 abstract).

### §1.4 — A6 pre-restart-pattern (1 fold)

| Lens | Disposition | Defect |
| --- | --- | --- |
| CH1 | ACCEPT | 7 findings; 67-file Pattern H census; LegacyPath shim at 4 `parse_with.rs` files at `:29 :29 :28 :54` all verified; combinator-fallback grep empty |
| CH2 | ACCEPT | Pattern H 67-file census + 48-file skinny mirror grammar-neutral; LegacyPath shim covers all 4 typed-segment grammars; NEW-MED properly scoped to `runtime/google_sheets/` |
| CH3 | **REVISE** | A6 NEW-HIGH-1 LegacyPath phrasing crosses V13 SYNTHESIS line 74 "Pattern G CLEAN" disposition without audit-trail acknowledgement; needs one-clause disambiguation (extension vs reversal) |
| CH4 | ACCEPT | All 7 findings → C-1 (within 2.8k-3.4k); LegacyPath rewrite ≈ 200 LOC well within headroom; preferring C-1 fold over opening C-6 |
| CH5 | ACCEPT-with-COUPLING-NOTE | NEW-HIGH-1 shim is Lock-1-adjacent coupling; remediation COLLAPSES dual representation (not bifurcates); NEW-HIGH-2 substrate-doc opt-out forces UNION CLOSURE (not split) |
| CH6 | ACCEPT | 67-file Pattern H + 48-file skinny mirror + 4 NEW all cite present-state surfaces; NEW-HIGH-2 is the inverse of paper-close (surfaces design-of-record demanding deletion or rewrite) |
| CH7 | ACCEPT (extension verified) | NEW-HIGH-1 extends CH7-4 round-trip surface to typed-path collapse cleanly; five-criterion ceiling holds |

V2 fold count: **1 edit** (NEW-HIGH-1 V13-Pattern-G disambiguation at
§0:12 + §2 ledger row).

### §1.5 — A3 lock14-scan (1 reclassification)

| Lens | Disposition | Defect |
| --- | --- | --- |
| CH1 | ACCEPT | 30 violations; all 8 RuntimeProvider match-arm coordinates verified; per-grammar dir count (9), provider count (8), generated-header count (42), 66-hit grep all reproduce |
| CH2 | **ACCEPT-with-NOTE** | H3 cites `decision_csp.rs:235` (inside `#[cfg(test)] mod tests { … }` block); HIGH severity overstates production scope; recommend H3 → LOW (test-fixture name leak); H6 (CSS L4 entry-rule absence from acceptance test surface) takes HIGH bar; aggregate count unchanged at 30 |
| CH3 | ACCEPT | 29 CONFIRMS V13 byte-for-byte; D1 DELTA-NOTE correctly classified (NOT a new violation); no silent reversal of v3 CLEAN reading |
| CH4 | ACCEPT | All 30 findings → C-1 cluster (2.8k-3.4k held); D1 → cosmetic rename folded into C-1 |
| CH5 | ACCEPT | 30-violation enumeration is EXPOSURE of existing per-grammar identifiers in generic crates, not introduction; PRUNE-3 collapses onto single dispatcher (substrate union strengthening) |
| CH6 | ACCEPT | 29 CONFIRMS + D1; each citation resolves to present grep / find output; DELTA-NOTE marks cross-tranche carry, not present-tranche FAIL softening |
| CH7 | ACCEPT | "Verbatim v3 reproduction" verifiable; D1 properly classed as DELTA-NOTE (discipline of not inflating stable observation into new finding) |

V2 fold count: **1 reclassification** (H3 HIGH → LOW; H6 takes HIGH
bar; aggregate count 30 unchanged).

### §1.6 — A1 css-measurement (STAND; no V2 folds)

All 7 lenses ACCEPT. 8 findings (4 CRIT + 2 HIGH + 2 MED); zero NEW;
all executable verification swathes (§1.1-§1.6) re-execute byte-
identically; comparator binding `json_parity.rs:43-53,87-102`
verified; 24/24 corpus-floor + 17/24 grouped-measurement + 16/24
sub-Criterion-overhead fail counts trace to quoted shell output. The
audit-zero criterion (1 KB representative corpus, distinct per-row
measurement, per-parse-ns plausibility) is grammar-neutral. Routes to
C-5 (24 CSS rows revert) + C-3 (production corpora R5 + xtask gate) +
C-2 harness rebind. STAND.

### §1.7 — A2 admit-mechanism (STAND; no V2 folds)

All 7 lenses ACCEPT. 9 findings (4 CRIT + 3 HIGH + 1 MED + 1 LOW)
with F8 + F9 NEW. F1-F5 W14.1-5 source-diff verification table
verified (all 5 SHAs match +1052/-176, +633/-162, +290/-55, +307/-51,
+313/-52); F8 NEW (single-lane comparator fan-out as structural
cause) reframes V13 per-row symptom as harness-layer defect; F9 NEW
negative-confirmation (no admit-row commits since `7ec4a474c`). Per-
iter equality oracle absent; sonic_rs::from_slice 21-hit fan-out
across `skinny/` verified. Routes to C-2 (R1 + R2 within 600-1.08k)
+ C-5 (W14.1-5 reverts within 250-500). STAND.

## §2 — V2 fold dispositions

Enumerated per axis as V2 redispatch packets. All folds are write-
only doc edits; no source touch; no architectural change; no new
C-N candidate; no envelope expansion.

### §2.1 — F-V2-A3-1 (CH2 NOTE)

**Source:** `CH2.md §4.1` + `CH2.md §6` item 1.

Migrate A3 H3 (`skinny/crates/passes/src/decision_csp.rs:235`,
`finalize_rule("json", RuleId(0), …)`) from HIGH to LOW. The cited
line sits inside a `#[cfg(test)] mod tests { … }` block (test fixture
exercising the solver in isolation); production call site is
`passes/src/lib.rs:478` — `finalize_rule(&grammar.name, …)`, generic
over `&grammar.name`. Violation class (JSON-named identifier in
generic crate's test surface) holds at L6/L7 severity tier. H6
(CSS L4 entry-rule absence from acceptance test surface) takes the
HIGH bar on its own structural merits.

Severity reshuffle: 11 CRIT + 6 HIGH + 5 MED + 8 LOW (vs prior 11
CRIT + 7 HIGH + 5 MED + 7 LOW). **Aggregate count unchanged at 30.**

### §2.2 — F-V2-A4-1 (CH3 REVISE-1)

**Source:** `CH3.md §3 REVISE-1` + `CH3.md §3 Fold F-1`.

Edit `sk-v14-audit-overfit-generator-truth.md §0 NEW-1` paragraph
(lines 25-31) + §0 NEW-1 sub-bullet (lines 33-37) to add the V13-
HONEST disambiguation:

> The V13 §7.1 row 1 'HONEST' verdict for `json_provider` correctly
> identified the grammar-derived `parse_direct` chunk (~70 LOC
> appended to `generated.rs` via `json_sink_direct::render` per
> `skinny/crates/codegen/src/lib.rs:215-217`); V14 extends the scope
> to the OTHER 5 emitted files (`config.rs`, `parser.rs`, `view.rs`,
> `value.rs`, `visitor.rs`) plus the include_str template body of
> `generated.rs` itself, which are pass-through templates with only
> the prepended `// @generated` header added. V14 does not reverse
> the V13 HONEST verdict on the sink-derived chunk; it observes that
> the chunk is the minority share (~15 %) of the JSON provider's
> emitted bytes.

Frames V14 as scope-extension (different file roster) not reversal
(same file, opposite verdict). The same one-clause disambiguation
folds into SYNTHESIS-AUDIT-OVERFIT.md §1.2 item 4 to keep the
synthesis-level NEW-1 enumeration coherent.

### §2.3 — F-V2-A4-2 (CH1 line-cite refresh)

**Source:** `CH1.md §3.3` + `CH1.md §6` item 3.

Refresh A4 line-cite ranges via a single `sed -n` re-pull:

- A4 finding 8: `json_provider.rs:85-99` → either narrow to `:80-83`
  (header-prepend) or widen to `:80-100` (full normalize fn).
- A4 finding 11: function-def cites `:62 :66 :70 :74 :50` → actual
  `:60 :64 :68 :72 :48` (all off by 2 consistently).
- A4 finding 13: `lib.rs:338-349` → `:337-347` (off by 1-2).

All five functions exist where the prose describes; the drift suggests
citations were taken from a sibling file revision; a fresh `sed -n` re-
pull corrects all in one pass. None of the off-by-1/2 drift changes a
finding's substantive correctness.

### §2.4 — F-V2-A4-3 (CH1 + CH2 fixture-lookup count)

**Source:** `CH1.md §3.2` + `CH2.md §4.2` + `CH2.md §6` item 2.

Edit A4 §0 line 38 abstract: "Three of the seven CSS L4 template
generators (`nested_layout`, `at_rules_and_media`,
`stylesheet_selectors`, `vendor_and_custom_atrules`) are fixture-
lookup tables" — the parenthetical lists **four** names; the actual
grep against `skinny/crates/codegen/src/css_l4_*_templates/generated.rs`
returns the `CANONICAL_FIXTURE` / `CAPTURED_W2_INPUT` short-circuit
pattern in **4 of 7** templates (3 CSS L4 "CANONICAL" + 1
"CAPTURED_W2"). Change "Three" → "Four" verbatim. Substantive
conclusion is unchanged; the fraction is 57 % not 43 %. Aligns A4 §0
with the §2 ledger (which captures 4 fixture-lookup rows: 3, 4, 5, 6).

### §2.5 — F-V2-A5-1 (CH6 single edit-pattern, 6 REVISEs collapse)

**Source:** `CH6.md §2.1 CH6-N1` + `CH6.md §3` item 1.

Single edit-pattern across 5 locations:

- `sk-v14-audit-overfit-decision-engine.md §0` line 11.
- `sk-v14-audit-overfit-decision-engine.md §3` lines 102-107.
- `SYNTHESIS-AUDIT-OVERFIT.md §0.1` row A5 cell.
- `SYNTHESIS-AUDIT-OVERFIT.md §0.2` lines 35-39.
- `SYNTHESIS-AUDIT-OVERFIT.md §5.1` bullet 2.

Replace `PASS at SK-V14 starting baseline` (and all paraphrases of
the same paper-close pattern) with **`FAIL at HEAD, PASS conditioned
on C-5 (PRUNE-1 + PRUNE-2) + C-4 (PRUNE-5) landing`**. Restores the
present-state-vs-target-state distinction the rest of the audit
observes elsewhere. The audit two sentences down already makes the
conditioning explicit ("PRUNE-1 + PRUNE-2 revert the scaffold-cited
admits; PRUNE-5 (C-4) wires …"); the verdict-line phrasing must lead
with the present-state FAIL before the conditional PASS.

**Companion edit per CH6-R3:** A5 §4 row 4 (the LOW honest self-
labelling row currently classed "No-op pending C-4") — widen action
class to **"Preserve through PRUNE-5; gate-rejection invariant inside
C-4 entry-gates"**, turning the deferral framing into a forward
obligation.

### §2.6 — F-V2-A6-1 (CH3 REVISE-2)

**Source:** `CH3.md §3 REVISE-2` + `CH3.md §3 Fold F-2`.

Edit `sk-v14-audit-overfit-pre-restart-pattern.md §0 line 12` + §2
ledger row for `LegacyPath` (line 154) to add the V13-Pattern-G
disambiguation:

> The V13 SYNTHESIS §"Honest patterns left clean" disposition 'No
> backwards-compat shims that aren't legitimate refactors (Agent F
> Pattern G)' did not enumerate the `LegacyPath` alias surface. V14
> records this as a NEW finding rather than a reversal: the alias was
> introduced at `0e8dbc104 feat(runtime/parse-with-{json,css-l4,
> sheets,bbnf}): land W3.2 entry points (AZ-IV.W3.2)` and was either
> not surveyed by V13's Pattern G scan or treated as a transitional
> in-flight refactor; V14 surveys it explicitly and classifies it as
> a shim that should be collapsed inside C-1 PRUNE-4 (typed-path
> collapse) per A6 §4.

The same disambiguation folds into SYNTHESIS-AUDIT-OVERFIT.md §1.2
item 8 to keep the synthesis-level NEW-HIGH-1 enumeration coherent.

### §2.7 — F-V2-SYNTHESIS-1 (CH1 census reconciliation)

**Source:** `CH1.md §3.1` + `CH1.md §6` item 1.

SYNTHESIS-AUDIT-OVERFIT.md reports the SK-V13 confirmation ratio in
three places with three different reconciliations: §0.1 verdict line +
§1.1 prose header read "63 CONFIRM / 11 NEW"; §1.1 per-axis table
column-sums read "8+7+29+4+3+3 = 54 CONFIRMS" + "0+2+1+12+1+4 = 20
NEW" (totals to 74 ✓ but neither column matches prose totals); §1.2
enumeration labels 11 NEW categories but NEW-2 says "3 findings"
under 4 row numbers.

Pick one definition of NEW (per-row or per-category) and apply
uniformly across §0.1, §1.1 table, §1.1 prose header, §1.2
enumeration, and §5.1. Recommended: per-row, since per-axis severity
distributions are also per-row. Re-tally the §1.1 table OR replace
"63 CONFIRM / 11 NEW" with correct "54 CONFIRM / 20 NEW" (per per-axis
table column-sum) — whichever is right. The 11-NEW claim is defensible
if read as "11 conceptual NEW *categories*"; the 12-NEW column claim
in §1.1 is then a category-vs-row counting error.

**Substantive 74-finding total + severity distribution stand**
(31 CRIT + 20 HIGH + 12 MED + 11 LOW arithmetically self-consistent).

### §2.8 — F-V2-SYNTHESIS-2 (CH3 REVISE-3 co-derivation)

**Source:** `CH3.md §3 REVISE-3` + `CH3.md §3 Fold F-3`.

Edit `SYNTHESIS-AUDIT-OVERFIT.md §1.3` closing sentence (current line
207) to add the `css_pretty` co-derivation note:

> The +3 file delta (64 → 67) and the PRUNE-4 sub-wave count delta (8
> → 9) are both attributable to the single `css_pretty` grammar
> addition between V13 audit-pack landing and SK-V14 baseline; A3 §1,
> A5 §2.1, and A6 §1 independently re-derive the count from the same
> evidence, so the three confirmations are evidentially co-derived,
> not orthogonal. The S-P3 wave manifest's risk-weighting for PRUNE-4
> should treat the `css_pretty` delta as one piece of evidence with
> three cross-checks, not three independent regression signals.

Three sequencing constraints (+1 PRUNE-4 sub-wave + +3 Pattern-H + R4-
before-PRUNE-2) actually co-derived from css_pretty addition + the
gate-vs-source separation invariants; the synthesis correctly converges
on both numerical counts but presents them as independent confirmations
when one piece of evidence (`css_pretty` directory + its 7 files)
drives the +1 / +3 deltas. The PRUNE-4 sub-wave plan stays at 9.

### §2.9 — F-V2-SYNTHESIS-3 (CH6 inheritance — verdict-line phrasing)

**Source:** `CH6.md §1.2 SYNTH-CH6-R1/R2/R3` + `CH6.md §2.2 CH6-N2`.

SYNTHESIS-AUDIT-OVERFIT.md §0.1 (A5 disposition cell), §0.2 (lines
36-39 prose inheritance), and §5.1 (final-verdict bullet 2) all
inherit A5's "PASS at SK-V14 starting baseline" paper-close phrasing.

§0.1 disposition cell: rewrite "scaffold-clause FAIL at v13 close,
PASS at v14 starting baseline" → **"scaffold-clause FAIL at v13 close +
at v14 HEAD; PASS conditioned on C-5 + C-4"**.

§0.2 lines 36-39 prose: rewrite "the no-scaffold-only-admit clause
failed at SK-V13 close and PASSES at SK-V14 starting baseline only
because every scaffold-citing row (W14.1-5, W13.1-4, W15.1) is held
under PRUNE-1 + PRUNE-2 revert and the audit-zero honest delta in
`tranches/sk-v14/SYNTHESIS.md §0.2` reads `0/17 / 0/17 / 0/17 / 0/24`
…" → **"the no-scaffold-only-admit clause failed at SK-V13 close and
remains FAIL at SK-V14 HEAD; the C-5 (PRUNE-1 + PRUNE-2) revert is
the gating wave that converts FAIL → PASS, and no row admit may cite
W8 / W9 until C-4 (PRUNE-5) wires them load-bearing; the audit-
corrected target in `SYNTHESIS.md §0.2` reads `0/17 / 0/17 / 0/17 /
0/24` post-PRUNE"**.

§5.1 final-verdict bullet 2: rewrite "1 of 6 PARTIAL PASS (A5:
resolver clause PASS, scaffold-clause PASS at SK-V14 baseline
conditional on PRUNE-1 + PRUNE-2 + PRUNE-5 sequencing)" → **"A5:
resolver clause PASS; scaffold-clause FAIL at HEAD, conditional PASS
upon PRUNE-1 + PRUNE-2 + PRUNE-5 landing per C-5 → C-4 sequencing"**,
fronting the present-state FAIL before the conditional PASS.

Single-pattern edit aligned with F-V2-A5-1.

### §2.10 — F-V2-SYNTHESIS-4 (CH1 NEW-2 phrasing + A5 "6 hits")

**Source:** `CH1.md §3.1` + `CH1.md §3.2` + `CH1.md §6` items 2 + 4.

SYNTHESIS-AUDIT-OVERFIT.md §1.2 NEW-2: rewrite "Three of the seven
CSS L4 template generators …" → **"Four of the seven CSS L4 template
generators …"**, listing all four template names verbatim (nested_
layout, at_rules_and_media, vendor_and_custom_atrules, stylesheet_
selectors). Aligns with F-V2-A4-3 above.

SYNTHESIS-AUDIT-OVERFIT.md §1.2 entry referencing A5 "6 hits": clarify
to "8 hits" (`grep -nE 'W8\|W9' restart/skinny/tranches/sk-v14/
SYNTHESIS.md | wc -l` returns 8, not 6; the `| head` pipe omission
showed all 8). Aligns with F-V2-A5-1.

### §2.11 — F-V2-SYNTHESIS-5 (CH2 N-CH2-1 LOW lint scope)

**Source:** `CH2.md §5 N-CH2-1` + `CH2.md §6` item 3.

Extend the CH7-companion lint scope proposed by A4 + SYNTHESIS §2.4
("REJECT any new `// @generated by skinny bbnf-codegen` header in
`skinny/crates/runtime/src/grammars/**/*.rs` unless the matching path
appears in a recognised regen subcommand's emission roster") to ALSO
scope the *codegen-side twin*. The lint glob should read:

```
skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs
```

Verification §3.5 in CH2.md returned 42 files with the fake header,
including 8 codegen-side template + provider files. A lint that
scopes only to `runtime/src/grammars/` would let the codegen-side
twin re-introduce the fake header silently — exactly the round-tripping
vector A4 finding 15 enumerates (identical-content twins between
codegen-side template and runtime-side generated.rs). Fold into
PRUNE-5 (or the C-3 + CH7 gating extension) alongside the original
lint proposal. Mechanism hardening of existing recommendation; not a
new finding class.

## §3 — V2 + V3 convergence forecast

V2 fold cycle closes the 11 REVISEs + 5 NEW findings cleanly. None of
the folds touch source code; none expand any C-N envelope; none open
a sixth CH7-N criterion or a sixth C-N candidate; the substantive
74-finding aggregate + severity distribution + three architectural
sequencing constraints + 67-file Pattern H census + PRUNE-list
mapping to C-1..C-5 all carry forward without modification.

**V2 dispatch surface** (parallel write-only redispatches):

- **A3 V2 author** — F-V2-A3-1 (HIGH→LOW H3 reclass; H6 promotion; 30-
  finding count unchanged).
- **A4 V2 author** — F-V2-A4-1 (NEW-1 V13-HONEST disambig) +
  F-V2-A4-2 (line-cite refresh -2) + F-V2-A4-3 ("Three"→"Four"
  abstract).
- **A5 V2 author** — F-V2-A5-1 single edit-pattern (verdict-line
  §0:11 + §3:102-107) + §4 row 4 action-class re-class.
- **A6 V2 author** — F-V2-A6-1 (NEW-HIGH-1 V13-Pattern-G disambig).
- **SYNTHESIS V2 aggregator** — F-V2-SYNTHESIS-1 (census reconcile)
  + F-V2-SYNTHESIS-2 (§1.3 co-derivation note) + F-V2-SYNTHESIS-3
  (§0.1 + §0.2 + §5.1 verdict-line alignment) + F-V2-SYNTHESIS-4
  ("Three"→"Four" + "6 hits"→"8") + F-V2-SYNTHESIS-5 (CH2 lint glob
  scope extension).

A1 + A2 V2 dispatches are no-op (STAND from V1; aggregator re-includes
in atomic commit for consistency).

**V2 wall-clock estimate:** ~30 min (5-6 parallel write-only axis
folds running concurrently, each with a single-focus edit packet of
1-5 distinct edits; aggregator atomic commit at close).

**V2 CHALLENGE wave** (7-lens redispatch parallel): ~30 min (same
seven-lens pattern as V1; lens authors re-attest under the V2 fold
artefacts; orphan REVISEs from V1 are checked closed; new defects
under V2 artefacts are surfaced if any).

**V3 confirming CHALLENGE wave** (post-V2): ~30 min (re-attest the
V2 artefacts at ≥ 95 % ACCEPT to establish the §3Z two-consecutive-
cycle convergence chain).

**Total path to G-S-P0-CONVERGED: ~90 min** (V2 axis fold + V2
CHALLENGE + V3 CHALLENGE confirming), assuming clean parallel
execution per the alpha-hardening V4/V5 cadence.

### §3.1 — Convergence gate forecast

V2 cycle ACCEPT-rate target: ≥ 95 % across all 7 lenses. With the 11
REVISEs closed by the V2 folds + the 5 NEW findings absorbed into
the V2 artefacts (mostly as editorial-precision footnotes), the V2
weighted ACCEPT-rate projects to ~97-99 % across the same 155-
disposition surface.

V3 cycle re-attests the V2 artefacts: zero substantive change, the
V1 dispositions that were ACCEPT at V1 remain ACCEPT at V3 (CH4, CH5,
CH7 all at 100 % stand verbatim); the V1 REVISEs that closed under
V2 either move to ACCEPT (if the fold cleanly resolves the defect)
or carry into V3 as remaining REVISE (if any disambiguation
under-shoots). Conservative V3 projection: ≥ 97 % ACCEPT across all
7 lenses.

§3Z convergence chain (≥ 95 % × 2 consecutive cycles, no orphan
REVISEs) closes at V3. **G-S-P0-CONVERGED** gates S-P1 dispatch
per CHALLENGE-CONTEXT.md §6.

## §4 — Closing posture

The S-P0 V1 CHALLENGE wave returns aggregate ACCEPT ~93.5 % across
155 dispositions (~145 ACCEPT, 11 REVISE, 0 REJECT, 5 NEW findings).
**S-P0 V1 verdict: NOT-CONVERGED-V2-REQUIRED** — below the §3Z 95 %
floor by ~1.5 points; orphan REVISEs require V2 fold to close.

The audit's *substance* is CH-clean end-to-end: 74-finding aggregate
+ severity distribution + three architectural sequencing constraints
+ 67-file Pattern H census + PRUNE-list mapping to C-1..C-5 with zero
orphans all hold under spot-verification by CH1, CH3, CH4, CH5, CH7.
Defects are editorial-hygiene + verdict-line-phrasing only:

- 5 SYNTHESIS folds (census reconciliation; "Three"→"Four"; "6
  hits"→"8 hits"; §1.3 co-derivation note; §0.1 + §0.2 + §5.1
  verdict-line phrasing alignment with A5).
- 1 A5 single-edit-pattern (verdict-line "PASS at SK-V14 starting
  baseline" → "FAIL at HEAD, PASS conditioned on C-5 + C-4 landing")
  collapsing 6 CH6 REVISEs; plus A5 §4 row 4 action-class re-class.
- 3 A4 folds (NEW-1 V13-HONEST disambig; line-cite refresh; "Three"→
  "Four").
- 1 A6 fold (NEW-HIGH-1 V13-Pattern-G disambig).
- 1 A3 reclassification (H3 HIGH→LOW; H6 takes HIGH bar; aggregate
  count unchanged at 30).

A1 + A2 STAND. CH5 surfaces two forward V2 binding notes (R4 Track 1/
Track 2 separation language; PRUNE-4 substrate-union closure
declaration); neither REVISE.

V2 cycle closes the 11 REVISEs + 5 NEW findings; V3 confirming pass
establishes the §3Z two-consecutive-cycle chain. Total wall-clock to
G-S-P0-CONVERGED: ~90 min (V2 fold + V2 CHALLENGE + V3 CHALLENGE
confirming). Path to S-P1 dispatch unblocked.

---

**Authored:** 2026-05-23 (SK-V14 S-P0 CHALLENGE V1 aggregator).
**Status:** Aggregator commit; 8 V1 files (7 lens + CONSOLIDATED)
landed atomically per `CHALLENGE-CONTEXT.md §6`.
**Authority:** `restart/prompts/ORCHESTRATOR.md §3Z step 4` +
`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/
CHALLENGE-CONTEXT.md §6`.
**Next gate:** V2 axis fold (parallel write-only) → V2 CHALLENGE
(7-lens parallel) → V3 CHALLENGE confirming → G-S-P0-CONVERGED →
S-P1 dispatch.
