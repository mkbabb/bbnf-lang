# 29 — Adversarial Verification of Six Load-Bearing Claims

Read-only re-derivation at HEAD `36ca10646`. Each claim was attacked with
independently-gathered `file:line` evidence; the cited source report was read
only for context, never trusted. Verdicts: 1 CONFIRMED, 1 CONFIRMED-with-nit,
2 CONFIRMED-clean, 2 QUALIFIED (one materially mis-attributed number, one
mis-attributed tranche).

---

## Claim 01 — AW-III / AW-IV "both 19-entry gates, both 0/19" — QUALIFIED

The thesis (substrate-first at scale; both tranches missed their bench gates)
holds. The specific numbers do not survive contact with the primary cited
source.

- **AW-III gate** was *strict-better than post-AU on ≥ 15/19* — NOT a
  "0/19 exceeding" gate. Recorded result: "**0 of 17 parse entries**; 2 format
  within noise — **miss**" (`docs/tranches/AW/FINAL-III.md:57`). The "19" is
  17 parse + 2 format (`FINAL-III.md:536`).
- **AW-IV gate** was "every parse entry exceeds post-AU." Recorded result is
  "**0/17 parse entries**" with a 17-row matrix
  (`docs/tranches/AW/FINAL-IV.md:76,80-100`). The cited span `:80-103` says
  **0 of 17**, not 0/19. The claim's "0/19 ... recorded in FINAL-IV.md:80-103"
  is therefore false to its own citation.
- The "0/19" figure exists ONLY in the secondary archaeology summary table
  (`docs/tranches/meta-audit/06-commit-archaeology.md:121-122`), which
  **conflicts** with both FINAL docs (0/17). The archaeology table is itself
  internally inconsistent with the primary close records.

**Verdict QUALIFIED.** Both tranches did declare hard bench gates and both
missed (CONFIRMED). But (a) AW-III's gate was 15-of-19 strict-better, not
"0/19 exceeding"; (b) the actual recorded miss is 0/**17** parse entries in
both FINAL docs; (c) the "0/19" appears only in a secondary table that
contradicts the primary evidence the claim cites. Citing FINAL-IV.md:80-103
for a "0/19" figure is a citation error — that span says 0/17.

---

## Claim 02 — MASTER-PLAN wave-tracking is structurally stale — CONFIRMED

- SK-V15 receiver rows `MASTER-PLAN.md:960-973` (MP.SK15.W0 … W11) **all read
  `active pending`**. SK-V15 closed: `sk-v15/SYNTHESIS.md:5,7,143`
  ("PASS-IMPL V2 returns `ACCEPT-SK-V15-CLOSE-WITH-ROUTED-BLOCKS`") and the
  successor SK-V18 is closed at HEAD (`405786b1a`, git log). The status column
  was never advanced.
- SK-V18 receiver block `MASTER-PLAN.md:1100-1114` carries exactly **12**
  `MP.SK18.*` rows (P1-P5, G1-G4, G5/G6, PROVE, H1 — verified count = 12) and
  **none carry a CLOSED/ADMITTED marker** — the table has no status column at
  all. Yet git log shows `405786b1a honesty(sk-v18-h1): … SK-V18 closes`.

Wave-tracking cannot be used as a status oracle: a closed tranche's rows read
`active pending`, and a second closed tranche's rows have no closure state.

**Verdict CONFIRMED.** Both halves verified independently; line refs exact.

---

## Claim 03 — "the sk-v13 CSS admissions were entirely fake" — QUALIFIED

The fake-template *mechanism* is real and confirmed; the *tranche attribution*
is wrong, and the source report contradicts itself on SK-V13.

- The fabrication mechanism is confirmed: `include_str!()` of hand-written
  templates carrying a fabricated `@generated` marker
  (`sk-v14/SYNTHESIS.md:109-114`); 85-357-byte Criterion-overhead fixtures
  (`sk-v14/SYNTHESIS.md:125-128`;
  `sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:131-132`); no genuine
  `cargo xtask regen-css` pipeline (introduced only as R4 in SK-V14,
  `sk-v14/SYNTHESIS.md:96`); no `.bbnf`-derived codegen (14 of 15
  `grammar/css/l4/*.bbnf` orphaned, `SYNTHESIS-AUDIT-OVERFIT.md:81`).
- But the **first** dishonest CSS admission was **SK-V12** W1b
  (`css_l4/declaration_values/direct_to_struct/main`, 429.3 vs 168.9 Mbps),
  demolished by the SK-V14 audit (`03-sk-arc-ledger.md:156-168`, the source
  report this claim rides on). The source report explicitly grades **SK-V13**
  as "entirely a contract / process document; **zero code shipped**"
  (`03-sk-arc-ledger.md:178,181,304`). The 85-357 B fixtures are attributed to
  "the **SK-V13** W10.*/W2 ADMITTED rows" at `SYNTHESIS-AUDIT-OVERFIT.md:132`,
  so SK-V13 *did* carry inherited fake admit-rows — but it shipped no new fake
  infrastructure; SK-V12 originated it.
- The "full prune-then-rebuild tranche" was **SK-V14** (PRUNE→REBUILD) plus
  **SK-V15** (CSS prune-then-rebuild) — two tranches, not "a tranche."
- The arc conclusion holds: CSS was genuinely >SOTA only at **SK-V17**
  ("first cycle to actually beat lightningcss," `03-sk-arc-ledger.md:252`),
  after SK-V16 measured CSS ~14× slower (`:228`).

**Verdict QUALIFIED.** Mechanism CONFIRMED; the entirely-fake admissions
originate in **SK-V12**, not SK-V13 (which the source report itself calls
zero-code). The load-bearing arc conclusion (real CSS SOTA only at SK-V17)
is CONFIRMED.

---

## Claim 04 — SK-V17/SK-V18 doc/code ratios use exact date ranges — CONFIRMED (with caveat)

- The claimed ranges match the source verbatim: SK-V17 `2026-05-28..30`,
  SK-V18 `2026-05-31..06-03` (`04-meta-process.md:19-20`). The claim that the
  ratios are `git log --numstat` over date windows is faithful
  (`04-meta-process.md:13-14`).
- Independent commit-date census: SK-V17-tagged commits actually span
  **2026-05-29** (T-P1/2/3) → **2026-05-31** (the SK-V17 close audit
  `7dbe44c22`); close commit `f6a38445b` is **05-30**. SK-V18 spans
  **2026-05-31** (S-P0/S-P1, `0fbee121f`/`9b52e162d`) → **2026-06-03** (H1
  close `405786b1a`).
- **Caveat:** the SK-V17 window `05-28..30` starts a day before any SK-V17
  commit and ends a day before SK-V17's `05-31` close audit — that audit's
  lines fall into SK-V18's `05-31..` window instead. SK-V18's window is clean.
  The numstat *figures* cannot be re-derived under the read-only constraint
  (no git numstat run).

**Verdict CONFIRMED.** The claim is a faithful, self-aware methodological
disclaimer ("boundaries are approximate"); the date ranges are reproducible as
stated. The SK-V17 window is loosely fitted (off by ~1 day each end) but the
claim pre-discloses this approximation.

---

## Claim 05 — W6c shape_dict_bbnf structurally re-keyed, DONE at f4633d468 — CONFIRMED

`crates/ir/src/passes/recognizers/shape_dict_bbnf.rs`:

- Dispatch (lines **82-98**) calls `is_big_comment_shape` and
  `is_mapped_factor_empty_shape` per rule body.
- `is_big_comment_shape` (116-132) keys on the literals `"/*"` (`:126`) and
  `"*/"` (`:128`) plus an interior regex — never the rule name.
- `is_mapped_factor_empty_shape` (146-166) keys the optional branch on the
  `"->"` arrow via `repeat_body_leads_with_arrow` (`:176`) — never the rule
  name.
- Commit `f4633d468` ("fold(sk-v19-w6c): re-key the BBNF shape recognizer on
  structure, not rule name") modified this file (+113 lines, git --stat); the
  file is byte-identical between `f4633d468` and HEAD `36ca10646`.

**Nit:** the claim cites `:79-81` for the dispatch; `:78-81` is the explanatory
comment, and the executable dispatch is `:82-98`. Residual (out of scope of the
claim): the module is named `shape_dict_bbnf`, the enum `BbnfShapeKind`, and
`rule_name: String` is still stored (`:43,85,92`) — recorded as data only
(`:81`), not used for dispatch.

**Verdict CONFIRMED.** Structural re-key is real and live at HEAD; only the
line citation (79-81 vs 82-98) is off by the comment band.

---

## Claim 06 — Lock-14 self-gate GREEN for ir/analysis but xtask outside boundary — CONFIRMED

`crates/ir/tests/lock14_grammar_generalization.rs`:

- The gate scans only `crates/ir/src` and `crates/analysis/src`
  (`generic_crate_src_roots`, `:83-87`); roster is `<Name>Parser`/
  `<Name>Grammar` idents (`:26-44`); doc comments excluded (`:49-52`).
- **GREEN re-derived:** scanning both `src/` roots for all 17 roster tokens on
  non-doc lines returns **zero hits**. The gate passes at HEAD.
- **xtask outside boundary, with live grammar-specific guards:**
  `xtask/src/regen_css.rs:278` `if projection.grammar != "css_l4"`, `:287`
  `if projection.parser_type != "CssL4Parser"`, `:281`/`:293` hardcode
  `crate::runtime::css_l4` / `crates/core/src/runtime/css_l4`. The gate test
  contains **no** reference to `xtask` (re-grep: none). `regen_simple_runtime.rs`
  similarly carries `if projection.grammar != grammar` (`:177`) plus a
  `RuntimeStyle{Simple,TypedFormula,TypedBbnf,TypedJson}` family fork (per
  `06-precepts-recap.md:15`).

The gate is green-by-exclusion (the D-7 anti-pattern from
`01-pre-restart-divergence.md:128-142`): it provably scans the two generic
crates and provably excludes the xtask emitter that still hardcodes
`"css_l4"`/`"CssL4Parser"`. It is NOT load-bearing for the xtask grammar guards.

**Verdict CONFIRMED.** Both halves verified independently; line refs exact.

---

## Cross-cutting note

Two of six claims (01, 03) carry materially wrong attributions — a benchmark
*count* (0/19 vs the recorded 0/17) and a *tranche* (SK-V13 vs SK-V12). In both
cases the underlying thesis survives, but a downstream planner citing "0/19" or
"SK-V13 fake codegen" would be repeating a number/owner that the primary close
records and the source report's own narrative refute. The secondary
archaeology table (`06-commit-archaeology.md:121-122`) is the propagation
vector for the 0/19 error and should be reconciled against the FINAL docs.
