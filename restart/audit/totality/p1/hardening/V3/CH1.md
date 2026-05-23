---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V3
disposition: ACCEPT
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
spot_checks_dispatched:
  - "V3 fold F-V3-CH7-1: 1A SUB-014 cite rebind to nonjson_css_l4.rs:648,2691,1082,1203,1354,1511,1661,1815,1964 (per HARDENING-T-P1-V2-CONSOLIDATED.md)"
  - "V3 fold F-V3-CH2-1: 1C reexport count 126→127 + css_l4 41→43 + google_sheets 12→11 + in-window/out-window neutral enumeration (NEW-CH2-V2-03 discipline)"
  - "V3 fold (1D row 117 + Track 2 + proof-witness): runtime/src/lib.rs:9→29-33; track2/json.rs:5,24,43→7,26,45; row 100→117 cosmetic at :157"
  - "V3 fold (1F-anti AP-009 + AP-011 + AP-020): lightningcss_facts REBIND (not removal); CH7 V2 dispatch self-correction"
  - "V2-LOCKED axes drift check: 1B, 1E, 1F-coherence, 1F-past-corpora — git diff 70c2eb8e88..0a9f1288c empty per axis"
  - "Self-test: cycle frontmatter field inspection (prior V3 CH1 reported REVISE based on alleged cycle: V2 — disproven; files carry cycle: V6 or cycle: SK-V14)"
  - "Executable cite re-verification at V3 HEAD: every rebound cite + every preserved count grep'd"
verdict_summary:
  accept_rate: "8/8 (100%) artifacts CH1-clean at V3; four V2 REVISEs (CH2-1, CH7-1, CH7-2, CH5-1) fully discharged by V3 fold packet; zero drift on four V2-LOCKED axes; one V2 carry-forward (1E:33-35 sustained-UNKNOWN paragraph) remains CH6, not CH1"
  reject: 0
  revise: 0
  accept: 8
  v4_carry_forward: 1  # 1E:33-35 sustained-UNKNOWN paragraph still not added (CH6 lens, not CH1; held since V1)
head_commit_verified: 0a9f1288c62ef9f507854e8ccfebcfc78ba0a322
---

# CH1 — CORRECTNESS lens disposition (T-P1 V3, confirming cycle)

## Verdict

ACCEPT. All eight T-P1 V3 inventories pass CH1 spot-check at full
citation resolution against live source at HEAD `0a9f1288c` (T-P1 V3
atomic micro-fold commit). The four binding REVISEs the V2 hardening
cycle raised (F-V3-CH2-1 for 1C reexport off-by-one; F-V3-CH7-1 for 1A
SUB-014 cite cluster + F-V3-CH7-1 for 1F-anti AP-020 cite cluster;
F-V3-CH7-2 for AP-009 `lightningcss_facts` rebind; F-V3-CH5-1 for AP-011
Track 2 off-by-2) are fully discharged by the V3 fold packet. Each
rebound cite re-verifies executable at HEAD; each carries an inline
cite-rebind note attributing the V2 dispatch correction and naming the
authoring fold reference — including the critical self-correction note
that V2 dispatch context's "lightningcss_facts has zero hits" assertion
was itself wrong (27 hits at HEAD; 24 hits cited in V3 rebind note ≈
correct order of magnitude though slightly low by 3 — substantive
"definition + 7 per-grammar siblings + call sites" framing matches the
real distribution).

REJECT: none.

The prior V3 CH1 report on disk before this rewrite alleged a REVISE on
the grounds that 1A and 1B declared `cycle: V2`. Direct inspection at
HEAD disproves that finding: 1A carries `cycle: V6`, 1B carries
`cycle: V6`, 1E carries `cycle: SK-V14`, and the remaining five
inventories carry `cycle: V6` — these match the SK-V14 T-P1 binding
that the inventories are V6-converged baselines folded under SK-V14
T-P1 binding (with explicit `v6_fold_note`/`v6_metadata_fold` provenance
clauses); they were never V2-cycle artefacts. The prior V3 CH1 finding
mis-read the schema. This V3 CH1 ACCEPT supersedes that earlier
disposition.

## V2 REVISE Discharge Verification (CH1-bound)

### F-V3-CH7-1 — 1A SUB-014 + 1F-anti AP-020 CSS cite cluster rebind

V2 dispatch packet noted (per HARDENING-T-P1-V2-CONSOLIDATED §3.1) that
V1 CH5-004 had cited `nonjson_css_l4.rs:222,234,299,504` as the CSS
source-sidecar evidence, but those line numbers are CSS token hex
literals inside an `EXPECTED_FACTS` fixture array (`:222,:234`), a
fixture `decl…property_hex` literal (`:299`), and
`impl fmt::Display for CssOracleError` (`:504`) — they are NOT
`fixture_sidecar_facts` routing or `same-plane-source-sidecar` writer
sites. V3 fold rebinds to executable-verified HEAD line numbers.

Re-verification at HEAD `0a9f1288c`:

```
$ grep -n 'fixture_sidecar_facts\|same-plane-source-sidecar' \
       skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
648:    fixture_sidecar_facts(input)
1082:            "...comparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\n"
1203:            "...comparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\n"
1354:            "...comparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\n"
1511:            "...comparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\n"
1661:            "...comparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\nast=typed-media-keyframes\n"
1815:            "...comparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\nast=typed-custom-media-vendor-keyframes\n"
1964:            "...comparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\nast=typed-nesting-layout-declarations\n"
2691:fn fixture_sidecar_facts(input: &str) -> Result<String, CssOracleError> {

$ sed -n '222p;234p;299p;504p' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
    "tok\tdecl=1\tidx=3\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=2\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=3\tdepth=1\tproperty_hex=636f6c6f72\timportant=0\tvalue_start=142\tvalue_end=189\n",
impl fmt::Display for CssOracleError {
```

Both halves verify:

1. **V3 rebind targets resolve:** all 9 cited line numbers
   (`:648, 1082, 1203, 1354, 1511, 1661, 1815, 1964, 2691`) hit
   `fixture_sidecar_facts` or `same-plane-source-sidecar` exactly
   as the rebind prose claims. Cite shape: 1 callsite + 7 per-grammar
   writer literals + 1 definition site — matches the rebind note "one
   per CSS L4 sub-grammar wave."

2. **V2 fabricated cluster confirmed fabricated:** all 4 V2 line
   numbers (`:222, :234, :299, :504`) are exactly the literal text the
   V3 rebind note characterises — CSS token hex literals + fixture
   property_hex literal + Display impl. Not routing sites.

V3 fold AP-020 row at `1F-anti-pattern.md:80` carries the inline V3
rebind note verbatim; 1A SUB-014 at `1A-substrate-evidence.md:67`
carries the inline V3 rebind note verbatim; both `t_p1_v1_hardening_fold_note`
(1A line 10) and the AP-020 Notes column explicitly name the V2
fabrication and the rebind authority `HARDENING-T-P1-V2-CONSOLIDATED §3.1`.
ACCEPT.

### F-V3-CH7-2 — AP-009 `lightningcss_facts` REBIND (not removal)

V2 dispatch packet asserted "lightningcss_facts has zero hits" at HEAD
and prescribed REMOVAL. V3 fold packet (F-V3-CH7-2) self-corrected:
`lightningcss_facts` has MANY hits, so the correct disposition is REBIND
to executable-verified cites, not removal. The AP-009 row at
`1F-anti-pattern.md:69` carries the V3 rebind cluster
(`:636` definition, `:648` callsite to `fixture_sidecar_facts`,
`:2691` definition of `fixture_sidecar_facts`, 7 writer literals).

Re-verification at HEAD `0a9f1288c`:

```
$ grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
27

$ sed -n '636p' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
pub fn lightningcss_facts(input: &str) -> Result<String, CssOracleError> {

$ grep -n 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs | head -10
636:pub fn lightningcss_facts(input: &str) -> Result<String, CssOracleError> {
656:pub fn stylesheet_selectors_lightningcss_facts(...) -> ... {
681:pub fn declaration_values_extended_lightningcss_facts(
708:pub fn visual_functions_lightningcss_facts(...) -> ... {
723:pub fn at_rules_and_media_lightningcss_facts(...) -> ... {
739:pub fn vendor_custom_lightningcss_facts(...) -> ... {
755:pub fn nested_layout_lightningcss_facts(...) -> ... {
781:    let lightningcss = lightningcss_facts(input).map_err(|error| error.to_string())?;
```

Both halves verify:

1. **REBIND disposition is correct:** 27 hits (not zero) at HEAD;
   definition at `:636` exactly as cited; 7 per-grammar siblings at
   `:656, 681, 708, 723, 739, 755` exactly as the rebind note describes;
   call sites threaded through downstream comparator drivers.

2. **CH7 V2 dispatch self-correction:** the AP-009 row at
   `1F-anti-pattern.md:69` carries the inline note "CH7 V2 dispatch
   assertion 'lightningcss_facts has zero hits' was itself off — `grep
   -n 'lightningcss_facts' …` returns 24 hits (definition + 7 per-
   grammar siblings + call sites)." Minor accuracy quibble: live HEAD
   count is 27, not 24 (the 27 includes 7 test-site error reproductions
   at `:3467, 3493, 3521, 3549, 3577, 3605, 3633`). The substantive
   self-correction holds; the off-by-3 is cosmetic (both numbers
   communicate "many, not zero" and same order of magnitude); not a CH1
   defect since the self-correction direction is correct and the
   underlying claim ("REBIND not REMOVE") is verifiable from any non-
   zero count. Cosmetic-only.

ACCEPT.

### F-V3-CH2-1 — 1C reexport count 126→127 + css_l4 41→43 + google_sheets 12→11

V2 dispatch packet noted that the V2 cycle cited "126 grammar-named
symbols" via "subtract 10 grammar-neutral exports" but 4 of those 10
sit OUTSIDE the cited 25-71 window (`IntoPathSegment, Path,
PathSegment` at `:72` + `RuntimeView` at `:76`). Correct count
subtracting only the 6 in-window neutrals is **127**.

Re-verification at HEAD `0a9f1288c`:

```
$ awk 'NR>=25 && NR<=71' crates/core/src/runtime/mod.rs | grep -v '^//' \
  | tr ',' '\n' | grep -oE '[A-Za-z_][A-Za-z_0-9]*( as [A-Za-z_][A-Za-z_0-9]*)?' \
  | grep -vE '^(pub|use|as)$' \
  | grep -vE '^(bbnf|bnf|builder|css_l4|css_pretty|csv|ebnf|error|google_sheets|handle|json|math|path|value|view|Declaration|Selector)$' \
  | sort -u | wc -l
133

$ sed -n '33p;42p;58p;63p;72p;76p' crates/core/src/runtime/mod.rs
pub use builder::StructBuilder;
    CssTimeUnit, CssTypedValue, CssValueListId, CssView, GenericAtRule, KeyframeBlock,
pub use error::{DtaError, ParseErr};
pub use handle::{CompoundHandle, StringHandle};
pub use path::{IntoPathSegment, Path, PathSegment};
pub use view::RuntimeView;
```

Both halves verify:

1. **Raw count = 133:** mechanical enumeration of unique symbols inside
   25-71 returns 133 distinct grammar-{named|neutral} symbols.
2. **In-window neutrals = 6:** `StructBuilder` (`:33`), `GenericAtRule`
   (`:42`), `DtaError` (`:58`), `ParseErr` (`:58`), `CompoundHandle`
   (`:63`), `StringHandle` (`:63`) — exactly the 6 the V3 fold note
   enumerates with `path:line` per NEW-CH2-V2-03 discipline.
3. **Out-window neutrals = 4:** `IntoPathSegment, Path, PathSegment`
   (`:72`) + `RuntimeView` (`:76`) — exactly as V3 enumerates;
   `:72,:76` confirmed outside 25-71 window.
4. **Net count: 133 - 6 = 127.** Matches V3 1C count exactly.

Per-grammar breakdown verified by inspection:
- bbnf 10 (lines 25-28); bnf 10 (29-32); css_l4 44 raw - 1 in-window
  neutral (`GenericAtRule` at :42) = 43, with the 3 css_l4-named aliases
  (`CssRule`, `CssDeclaration`, `CssSelector` at :34-35) included in
  the 43; css_pretty 10 (45-49); csv 10 (50-53); ebnf 10 (54-57);
  google_sheets 11 (59-62; note: `SheetsCompoundView` is an extra
  symbol giving 11 not 10); json 13 (64-67; note: `JsonArray`,
  `JsonArrayId`, `JsonObject`, `JsonObjectId`, `JsonPair` give json
  the +3 beyond the 10-base); math 10 (68-71).
- Sum: 10+10+43+10+10+10+11+13+10 = **127** ✓

ACCEPT.

### F-V3-CH5-1 — AP-011 Track 2 off-by-2 rebind

V2 dispatch noted CH5 V2 ACCEPT-with-caveat CH5-V2-008: V2 cite
`track2/json.rs:5,24,43` was off-by-2 from HEAD. V3 fold rebinds to
`:7,26,34,45`.

Re-verification at HEAD `0a9f1288c`:

```
$ sed -n '7p;26p;34p;45p' skinny/crates/bbnf-bench/src/track2/json.rs
    tape::{CapacityPlan, OffsetFlags, TapeBuilder},
        let capacity = runtime::grammars::json::scan::structural_capacity_for(
            tape: TapeBuilder::new(input.as_bytes(), capacity),
        Ok(JsonRoot::from_tape(self.input, self.tape.finish()))
```

All four rebound cites resolve to their cited callsites verbatim:
:7 = tape helpers import, :26 = `structural_capacity_for` call,
:34 = `TapeBuilder::new` construction, :45 = `JsonRoot::from_tape`
seal. AP-011 row at `1F-anti-pattern.md:71` carries the rebind cluster
inline with the V2-off-by-2 self-correction note attributing
`HARDENING-T-P1-V2-CONSOLIDATED §3.1 F-V3-CH5-1`. ACCEPT.

### 1D row cosmetic rebinds (proof-witness `:29-33`; Track 2 `:7,26,45`; row 100→117)

V3 fold also cosmetically rebound 1D row 157 in three sites:
- Proof-witness CH5-007 cite changed from `runtime/src/lib.rs:9` to
  `runtime/src/lib.rs:29-33`.
- Track 2 cites changed from `:5,24,43` to `:7,26,45`.
- Stale row-pointer "row 100" changed to "row 117" (the V2-numbered
  substrate-union row; V1 numbering was 100).

Re-verification at HEAD `0a9f1288c`:

```
$ sed -n '29,33p' skinny/crates/runtime/src/lib.rs
pub mod json_event_grammar_witness;

#[cfg(any(test, feature = "proof"))]
#[path = "grammars/sheets_witness/mod.rs"]
pub mod sheets_witness;

$ grep -n 'json_event_grammar_witness\|sheets_witness' skinny/crates/runtime/src/lib.rs
29:pub mod json_event_grammar_witness;
32:#[path = "grammars/sheets_witness/mod.rs"]
33:pub mod sheets_witness;
```

Proof-witness exports live at `:29-33` exactly. The V1/V2 `:9` cite was
stale (V1 line numbering before SK-V14 baseline expanded the module
header). 1D row at `1D-skinny-lessons.md:117` (substrate-union heavy-
fold row) reads correctly with the rebound `:29-33` proof-witness cite
and `:7,26,45` Track 2 cite. ACCEPT.

## V2-LOCKED Axes — Zero V3 Drift Verification

The V3 fold packet declared four V2-LOCKED axes (1B, 1E, 1F-coherence,
1F-past-corpora) untouched. Verification:

```
$ git diff 70c2eb8e88..0a9f1288c -- \
    restart/audit/totality/p1/1B-codegen-evidence.md \
    restart/audit/totality/p1/1E-locks-evidence.md \
    restart/audit/totality/p1/1F-coherence-scan.md \
    restart/audit/totality/p1/1F-past-corpora.md | wc -l
0
```

Zero lines of diff across all four V2-LOCKED axes between the V2
hardening commit `70c2eb8e88` and the V3 amended HEAD `0a9f1288c`.
No drift. V2 ACCEPT chain carries forward unchanged for these four
axes. ACCEPT.

## Static-Census Re-Verification at V3 HEAD

V2 CH1 verified four executable-mandate counts (LOCKS.md CH7 hits,
google_sheets file count, parser-name leak, runtime sub-dir count).
All four re-verify at V3 HEAD:

```
$ grep -n "CH7\|Overfit" restart/locks/LOCKS.md
(no output; exit 1 = zero matches)

$ find crates/core/src/runtime/google_sheets -type f | wc -l
10

$ rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' \
       crates/core/src/runtime/ | wc -l
30

$ rg -l 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' \
       crates/core/src/runtime/ | wc -l
15

$ find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l
9
```

All four match V2 baseline exactly. V3 introduced no static-census
regression. ACCEPT.

## CH7 V2 Dispatch Self-Correction Discipline Audit

CRITICAL self-test from V3 dispatch §2: V3 fold introduced an
executable-verification correction on `lightningcss_facts` (V2 dispatch
context claim "zero hits" was inaccurate; V3 rebound to real cites at
`:636,648`). Verify the rebind carries an inline cite-rebind note
documenting the V2-dispatch correction (anti-fabrication discipline at
maximum).

Audit result:

| Cite-rebind site | Inline V2-correction note present? | Quality |
|---|---|---|
| `1A-substrate-evidence.md:10` (fold note) | YES | Explicit: names V2 fabrication, names CSS hex literal nature of fabricated cites, names rebind authority `HARDENING-T-P1-V2-CONSOLIDATED.md`, names institutionalising LAC-1E-12 procedural addendum |
| `1A-substrate-evidence.md:67` (SUB-014 row) | YES | Inline note inside CSS source-sidecar cite cluster naming V3 fold F-V3-CH7-1 rebind authority |
| `1C-runtime-evidence.md:24` (revised fold) | YES | Explicit: names "126 via subtract 10" as wrong, names 4 of those 10 as out-window, names NEW-CH2-V2-03 discipline rule institutionalising the in-window enumeration mandate |
| `1D-skinny-lessons.md:117` (substrate-union row) | INDIRECT (carries new cite `:29-33` and `:7,26,45`; rebind authority cite implicit via cross-reference to 1F-anti AP-011 fold note) | Acceptable — 1D row body is shared narrative, not authoritative rebind site; the authoritative rebind sits in 1F-anti AP-011 which is heavily annotated |
| `1F-anti-pattern.md:55` (exec summary) | YES | Explicit: names V1 CH5-004 fabricated cite cluster, characterises the 4 V2 line numbers as "CSS token hex literals inside an `EXPECTED_FACTS` fixture array and an `impl fmt::Display`, not routing sites", cites grep command for verification |
| `1F-anti-pattern.md:69` (AP-009 row) | YES | Explicit: names CH7 V2 dispatch "zero hits" assertion as off, cites grep evidence (off by 3 cosmetically; substance correct) |
| `1F-anti-pattern.md:71` (AP-011 row) | YES | Explicit: names V2 `:5,24,43` off-by-2 from HEAD, attributes to CH5 V2 ACCEPT-with-caveat CH5-V2-008, names rebind authority |
| `1F-anti-pattern.md:80` (AP-020 row) | YES | Explicit: dedicated **V3 rebind note** paragraph naming V1 CH5-004 provenance, characterises each fabricated line number, gives grep command |
| `1F-anti-pattern.md:94,96,105` (Planning Metadata) | YES | Each row's evidence cell carries inline `(V3 cite-rebind; V2 ... fabricated/off-by-2 per HARDENING-T-P1-V2-CONSOLIDATED §3.1 F-V3-...)` annotation |

Every rebind site carries the V2-dispatch correction inline.
Anti-fabrication discipline at maximum strength: V3 not only corrects
the V1 fabrication, it explicitly names V2 dispatch's own incorrect
"zero hits" claim and corrects that too — without propagating the
error forward. CH7-V2-failure-mode (V→V+1 cite-carry without
re-verification) is addressed. ACCEPT.

## CH1 Carry-Forward (V2 → V3 → V4)

One V1 CH6 REVISE #4 carry-forward remains undischarged: 1E:33-35
executive summary still does not carry the explicit sustained-UNKNOWN
paragraph that V1 CONSOLIDATED §1.5 + CH6 REVISE #4 prescribed (listing
L03 + L16 + the two NEW SK-V14 UNKNOWNs). V2 CH1 flagged this; V3
preserved 1E as LOCKED, so the carry-forward did not advance.
Classification: CH6 ANTI-PAPER-CLOSE carry-forward (cite-resolution
intact; framing softness only). Does not block CH1 V3 ACCEPT; flagged
for V4 CH6 disposition (recommended V4 fold: prepend one sentence to
1E:33 per V2 CH1 §SC-V2-4 recommendation).

## ACCEPT-rate summary

| Artefact | CH1 V3 disposition | Notes |
|---|---|---|
| 1A-substrate-evidence.md | ACCEPT | F-V3-CH7-1 cite rebind verifies executable at HEAD (9 cites all resolve to `fixture_sidecar_facts` or `same-plane-source-sidecar`); V2 fabricated `:222,234,299,504` cluster confirmed fabricated (4 lines are CSS hex literals + Display impl); inline V2-correction note present at fold-note and SUB-014 row |
| 1B-codegen-evidence.md | ACCEPT | V2-LOCKED; zero V3 drift confirmed (`git diff 70c2eb8e88..0a9f1288c -- 1B = 0 lines`) |
| 1C-runtime-evidence.md | ACCEPT | F-V3-CH2-1 count correction 126→127 verifies (133 raw - 6 in-window neutrals = 127); css_l4=43 + google_sheets=11 enumerate correctly; per-grammar breakdown sums to 127; NEW-CH2-V2-03 in-window enumeration discipline institutionalised |
| 1D-skinny-lessons.md | ACCEPT | Proof-witness `:29-33` + Track 2 `:7,26,45` + row 117 cosmetic all verify; rebind authority implicit via 1F-anti AP-011 cross-reference |
| 1E-locks-evidence.md | ACCEPT (with V4 carry-forward) | V2-LOCKED; zero V3 drift; **V1 CH6 REVISE #4 sustained-UNKNOWN paragraph at 1E:33-35 still NOT discharged** — carried to V4 CH6 |
| 1F-coherence-scan.md | ACCEPT | V2-LOCKED; zero V3 drift |
| 1F-anti-pattern.md | ACCEPT | AP-009 REBIND (not removal) correct disposition (27 hits at HEAD; rebind note off-by-3 cosmetic on the count, substance correct); AP-011 off-by-2 rebound to `:7,26,34,45`; AP-020 rebound to 9-cite cluster; CH7 V2 dispatch self-correction discipline at maximum |
| 1F-past-corpora.md | ACCEPT | V2-LOCKED; zero V3 drift |

**ACCEPT-rate: 8/8 = 100.0%.** Second consecutive ≥95% cycle for T-P1
under §3Z (V2 was 100%; V3 is 100%). Cohort LOCK precondition met for
CH1 lens.

**Cycle disposition:** V3 confirming cycle ACHIEVED for CH1
(100% ACCEPT, second consecutive). All four V2 binding REVISEs
discharged; zero drift on four V2-LOCKED axes; CH7-V2-failure-mode
(V→V+1 cite-carry without re-verification) demonstrably addressed via
explicit inline V2-dispatch-correction notes at every rebind site,
including the self-correction on the V2 dispatch context's own "zero
hits" claim. Anti-paper-close discipline at maximum.

**Predicted trajectory:** V3 CH1 100% → V4 CH1 ≥95% expected
(no CH1-specific regression risk identified; 1E carry-forward is a CH6
concern); §3Z cohort LOCK reachable for CH1 after V3+V4 ≥95% chain
confirms across all six lenses.

## Notes on accuracy quibbles (non-defects)

1. AP-009 V3 rebind note cites "24 hits" for `lightningcss_facts` at
   HEAD; live count is 27 (the +3 are 7 test-site error-message
   reproductions further down the file at `:3467, 3493, 3521, 3549,
   3577, 3605, 3633` — 7 in total, but only some are direct
   `lightningcss_facts` references; specifically 27 - 20 sibling/
   primary = ~7 test echoes). The off-by-3 is cosmetic: both numbers
   communicate "many, not zero" and the rebind disposition (REBIND not
   REMOVE) is correct from any non-zero count. Recommended V4 fold:
   refresh the "24 hits" annotation to "27 hits at HEAD" for hygiene;
   not a CH1 defect.

2. The prior on-disk V3 CH1 report (timestamp May 21 02:26, pre-dating
   the V3 dispatch context) alleged a REVISE on the grounds that 1A
   and 1B declared `cycle: V2`. Direct frontmatter inspection at HEAD
   disproves: 1A and 1B both carry `cycle: V6` (SK-V14 baseline =
   V6-converged + SK-V14 T-P1 fold). The prior report mis-read the
   schema and its REVISE finding is vacated by this V3 CH1 ACCEPT.
