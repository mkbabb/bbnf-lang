# CH7 Overfit-Prune — S-P0 V3 Disposition (SK-V14 Overfit Audit)

Lens binding unchanged: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`.
Five CH7 criteria remain the disposition spine — (CH7-1) every
"generated" artefact is grammar-derived (template + grammar metadata +
emission command), never hand-written under a `// @generated` header;
(CH7-2) Lock 14 generic-crate compliance preserved (no JSON/CSS/Sheets
literals, function names, enum variants, match arms in nominally-
generic code); (CH7-3) every admit lands via a real
parser/codegen/SIMD source change measured against a strict-vs-strict
comparator on the same plane with a per-iteration equality oracle;
(CH7-4) every "generated" output passes a round-trip test (delete +
regen ⇒ byte-equivalent); (CH7-5) no SCAFFOLD-ONLY landing counts as
an admit. The V3 confirming pass re-attests the V2 100 % verdict
across the three V3-touched artefacts (commit `007624849`,
3 files +10 / -6 prose-only), re-executes the C-3 round-trip
metadata-derived loop at V3 HEAD, and verifies the F-V3-SYNTHESIS-2
prose-count refresh ("8 codegen-side template+provider files" → "14
codegen-side files (8 providers+templates + 6 ancillary)") is
mechanically consistent with the V2-folded F-V2-SYNTHESIS-5 lint glob
scope (`skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs`)
that CH7-1 and CH7-4 already bind.

## §0 — Disposition summary

- Artefacts reviewed: **3** (SYNTHESIS V3 — three single-phrase prose
  edits at §1.1:81 + §2.4:~311 + §3.1:343; `decision-engine.md` V3 —
  §5:133 action-class propagation; `generator-truth.md` V3 — §1:153
  line-count refresh). A1, A2, A3, A6 STAND verbatim (no V3 folds) per
  the V2 CONSOLIDATED §2 micro-fold scope.
- Per-section dispositions issued: **18** (15 V2 sections re-attested +
  3 new V3-fold-specific sections: F-V3-A5-1 action-class symmetry +
  F-V3-SYNTHESIS-1 C-4 row mirror + F-V3-SYNTHESIS-2 prose-count
  refresh + F-V3-SYNTHESIS-3 in-table NEW-2 cell fold + F-V3-A4-1
  line-count refresh).
- ACCEPT: **18**.
- REVISE: **0**.
- REJECT: **0**.
- ACCEPT-rate: 18 / 18 = **100.0 %**.
- Critical findings against the V3 audit's own write-up: **0**.
- New finding (informational, cross-flag to CH1): **1** — SYNTHESIS
  §3.1 C-3 row at line 342 retains the stale parenthetical "3 scanners
  are fixture lookups" inside the A4-findings cluster while every
  other site has folded to "4". Not a CH7-N criterion violation; the
  finding lives at CH1 (internal cite consistency); flagged here as a
  V4 micro-fold candidate so the F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3
  arithmetic-correction sweep reaches its final residual cell.
- Escalation flag: **NO.** The three V3-touched files preserve the V2
  CH7-clean posture: zero fake-`@generated` header recurrence in audit
  prose, zero scaffold-as-load-bearing claim, zero gate-relabel
  adoption, zero orphan-grammar dressed as load-bearing. The V3 folds
  tighten prose-count precision (F-V3-A4-1, F-V3-SYNTHESIS-2,
  F-V3-SYNTHESIS-3), reinforce CH6 paper-close defence
  (F-V3-A5-1, F-V3-SYNTHESIS-1), and add zero new CH7-N criteria.

## §1 — Executable verification (per §3 mandate)

### §1.1 — C-3 round-trip gate metadata-derived loop re-executed (V3 HEAD)

Per the dispatch §1 mandate ("re-execute `cargo metadata --format-
version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'` (expect
9 grammars unchanged at V3)"), the canonical C-3 round-trip metadata
command was re-executed against the live workspace at `HEAD =
007624849`.

**Command (verbatim from `alpha-E-candidate-shortlist.md:366-367` /
V1 CH7 §1.1 / V2 CH7 §1.1):**

```
cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
```

**Output (captured in `/Users/mkbabb/Programming/bbnf-lang` at V3
HEAD `007624849`):**

```
bbnf
json
css_l4
css_pretty
google_sheets
ebnf
bnf
csv
math
```

The command enumerates **9 grammars** verbatim: `bbnf`, `json`,
`css_l4`, `css_pretty`, `google_sheets`, `ebnf`, `bnf`, `csv`, `math`.
This is byte-identical to the V1 CH7 §1.1 attestation
(HEAD `7d0fbe071`), the V2 CH7 §1.1 attestation
(HEAD `42e3edb9a`), and to the V5 Pass-Alpha attestation
(`restart/skinny/tranches/sk-v14/research/alpha-hardening/V5/CH7.md
§1.1`). The ninth grammar `css_pretty` continues to enumerate at the
gate's runtime under S-P0 V3; the SYNTHESIS-AUDIT-OVERFIT V3 §2.3
"PRUNE-4 = 9 sub-waves" binding (and its V2-folded §1.3 co-derivation
note crediting the +1 to the single `css_pretty` directory addition)
reconciles to this enumeration verbatim across three consecutive cycle
boundaries (V1 → V2 → V3).

### §1.2 — F-V3-SYNTHESIS-2 prose-count refresh mechanically consistent with CH7-1 + CH7-4

Per the dispatch §1 mandate ("verify F-V3-SYNTHESIS-2 (lint scope '14
codegen-side') is consistent with CH7-1 + CH7-4 mechanically"), the V3
SYNTHESIS-AUDIT-OVERFIT.md §2.4 second-item parenthetical was re-read
at HEAD `007624849` and the underlying twin-site recurrence vector
re-validated against both the V2-folded lint glob and the two CH7-N
criteria the glob enforces.

**Pre-V3 prose (V2 baseline, V2 CH7.md §1.2:122):**

> 42 files carry the fake header including 8 codegen-side template+
> provider files

**Post-V3 prose (V3 fold landed at SYNTHESIS-AUDIT-OVERFIT.md:308-319):**

```
   42 files carry the fake header including 14 codegen-side files
   (8 providers+templates + 6 ancillary; `git grep -l '@generated by
   skinny bbnf-codegen' skinny/crates/codegen/src/ | wc -l = 14`).
   The lint glob `codegen/src/**/*.rs` catches all 14 regardless;
   only the prose count needs the 8-vs-14 distinction.
```

**Executable re-verification of the count claim:**

```
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/ | wc -l
14
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/
skinny/crates/codegen/src/css_l4_at_rules_and_media_provider.rs
skinny/crates/codegen/src/css_l4_declaration_values_extended_provider.rs
skinny/crates/codegen/src/css_l4_declaration_values_provider.rs
skinny/crates/codegen/src/css_l4_nested_layout_provider.rs
skinny/crates/codegen/src/css_l4_stylesheet_selectors_provider.rs
skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_provider.rs
skinny/crates/codegen/src/css_l4_visual_functions_provider.rs
skinny/crates/codegen/src/json_provider.rs
skinny/crates/codegen/src/json_templates/generated.rs
skinny/crates/codegen/src/json_templates/parser.rs
skinny/crates/codegen/src/json_templates/value.rs
skinny/crates/codegen/src/json_templates/view.rs
skinny/crates/codegen/src/json_typed_direct.rs
skinny/crates/codegen/src/lib.rs
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ | wc -l
42
```

The 14-file codegen-side count splits cleanly into the V3-prose
"8 providers+templates + 6 ancillary" partition:

- **8 providers+templates (V2-cited subset):** 7 CSS L4 `*_provider.rs`
  + `json_provider.rs` (the eight render-driver files CH2 V2 §3.5
  enumerated as the "template+provider" cohort).
- **6 ancillary:** `json_templates/generated.rs` (the codegen-side
  twin of the runtime `json/generated.rs`), `json_templates/parser.rs`
  + `value.rs` + `view.rs` (three JSON sub-template body files),
  `json_typed_direct.rs` (the direct-AST JSON emitter), and `lib.rs`
  (the codegen crate root binding the per-grammar provider exports).

The 8 + 6 = 14 partition arithmetic matches the V3 prose; the V3 fold
also adds the clarifying sentence "The lint glob `codegen/src/**/*.rs`
catches all 14 regardless; only the prose count needs the 8-vs-14
distinction" which correctly separates the **prose-count refresh**
(census tightening from 8 to 14) from the **lint-glob scope**
(unchanged at `{runtime/src/grammars,codegen/src}/**/*.rs` per
F-V2-SYNTHESIS-5).

**Mechanical consistency with CH7-1 (no fake `@generated` outside
emission roster):** The V2-folded lint glob
`skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs` already
scoped all `*.rs` under the codegen crate root — the brace-expansion
form catches all 14 files including the 6 ancillary ones the V2 prose
under-cited at 8. The V3 prose refresh aligns the **narrative count**
with the **glob's actual reach** (56 = 42 + 14 files), eliminating the
V2 prose-vs-glob asymmetry where the glob caught 14 but the prose
mentioned 8. CH7-1 enforcement substrate is **unchanged**; the V3 fold
is a **CH1-evidence-precision** tightening of the **CH7-1 narrative
description**. No new CH7-N criterion required; no recurrence vector
introduced; the lint glob's mechanical reach was already complete.

**Mechanical consistency with CH7-4 (round-trip test for every
generated output):** The V2 CH7 §1.2 round-trip vector identification
named "the identical-content round-tripping vector A4 finding 15
enumerates between codegen-side template and runtime-side
`generated.rs`" — i.e. the round-trip surface is the **identity map
from codegen-side template to runtime-side mirror**. The 6 ancillary
codegen-side files surfaced by F-V3-SYNTHESIS-2 are precisely the
files that **populate the codegen side of that identity map**:
`json_templates/{generated,parser,value,view}.rs` are the 4 files
whose contents identity-twin onto `runtime/src/grammars/json/*.rs`;
`json_typed_direct.rs` is the JSON typed-AST emitter that A4 finding
14 (the `*_templates/` Pattern-H projection) categorises;
`lib.rs` is the crate root binding all per-grammar provider exports
that A4 finding 14 collectively diagnoses. The V3 prose refresh
**makes visible** the full codegen-side round-trip surface that the
V2-folded lint glob **already protected mechanically**; the round-trip
vector itself is unchanged in scope or shape. CH7-4 enforcement
substrate is **unchanged**; the V3 fold is **descriptive completeness**
of the round-trip-surface census, not a surface expansion.

F-V3-SYNTHESIS-2 lands cleanly; CH7-1 + CH7-4 are **mechanically
unchanged** at the lint-glob substrate; the V3 prose now correctly
narrates the substrate's full reach.

### §1.3 — Cross-axis sub-wave count consistency check (V3 confirming)

Per the V1 CHALLENGE-CONTEXT §2 bound-fact "PRUNE-4 sub-wave count is
9 not 8 (css_pretty added between V13 and SK-V14)", the V3-touched
files preserve the four-corner consistency from V2:

- A3 V2 §1 (unchanged from V1, STAND at V3): `find crates/core/src/
  runtime -mindepth 1 -maxdepth 1 -type d` returns 9 directories.
- A5 V3 §2.1 (verdict-line FAIL-at-HEAD aligned at V2; STAND
  structurally at V3 — only §5:133 action-class prose tightened):
  "9 dirs — one over the dispatch-cited 8 because of `css_pretty`".
- A6 V2 §1 (unchanged from V1, STAND at V3): per-grammar census table
  sums to 9 directories / 67 files.
- SYNTHESIS V3 §1.3 (V2-folded with co-derivation note; STAND at V3):
  "Pattern H file count: 64 → 67"; the +3 file delta and +1 sub-wave
  delta attributed to the single `css_pretty` addition; the three
  A3/A5/A6 cross-checks remain **co-derived, not orthogonal**.

**Re-executed verification (V3 HEAD `007624849`):**

```
$ find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l
9
$ find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d
.../runtime/google_sheets
.../runtime/bnf
.../runtime/css_l4
.../runtime/ebnf
.../runtime/math
.../runtime/bbnf
.../runtime/json
.../runtime/csv
.../runtime/css_pretty
```

§1.1 metadata enumeration above confirms the ninth grammar at the
manifest layer; this filesystem enumeration confirms it at the
runtime-crate layer. The cross-axis coherence holds without exception
across the V1 → V2 → V3 cycle boundary.

**Cross-version stability of the 56-file lint-glob coverage
(F-V2-SYNTHESIS-5 substrate; F-V3-SYNTHESIS-2 prose):**

```
$ git grep -l '@generated by skinny bbnf-codegen' \
    skinny/crates/runtime/src/grammars/ \
    skinny/crates/codegen/src/ | wc -l
56
```

56 = 42 + 14 holds verbatim at V3 HEAD; the V2 confirmation
(HEAD `42e3edb9a`) returned 56 likewise. Three-cycle stability is
established.

### §1.4 — Fresh-finding scan across the 5 CH7-N criteria (V3 confirming)

Per the dispatch §1 mandate ("verify no fold introduces new CH7-N
pattern"), each criterion was independently re-scanned against the V3
artefact set + the underlying source repository at V3 HEAD:

- **CH7-1 (no fake `@generated` outside emission roster):** V1 + V2
  scope-extension stands at 8 sites of authored render drivers
  (7 CSS providers + JSON provider); the V3 prose refresh now
  enumerates the 14-file codegen-side reach the V2-folded lint glob
  already protected mechanically. The substrate is unchanged; only
  the narrative description tightened. No new fake-`@generated`
  recurrence site emerged between V2 and V3 (the 17-commit doc-only
  delta — V2 axis-redispatch `1735882a5` + V2 hardening `0ac009a84`
  + V3 micro-redispatch `007624849` — touches zero source bytes).
- **CH7-2 (Lock 14 generic-crate compliance):** A3 V2 H3 HIGH→LOW
  reclassification + H6 freestanding HIGH preservation carry forward
  at V3 STAND; the 11 CRIT + 6 HIGH + 5 MED + 8 LOW = 30 distribution
  holds unchanged. No new Lock-14 surface emerged.
- **CH7-3 (strict-vs-strict comparator + per-iter oracle):** A2
  STANDs from V1 + V2 (no V3 fold); F8 + F9 NEW findings preserve
  their V2 disposition. No new comparator-binding pathology emerged.
- **CH7-4 (round-trip test for every generated output):** A6 V2
  LegacyPath shim disambiguation stands at V3; F-V3-SYNTHESIS-2
  prose refresh makes the round-trip surface census descriptively
  complete (14 codegen-side files identity-twin onto the runtime
  side) without expanding the surface shape. The V2-folded lint
  glob substrate (`{runtime/src/grammars,codegen/src}/**/*.rs`)
  remains the round-trip-runtime-AND-lint-CI defence on both sides
  of the twin.
- **CH7-5 (no SCAFFOLD-ONLY admit):** A5 V3 §5:133 action-class
  refresh ("preserved through PRUNE-5 as a gate-rejection invariant
  inside C-4 entry-gates so any admit attempting to cite W8/W9 pre-
  runtime-consumer is denied at admit time") **strengthens** the
  CH7-5 posture by giving the LOW resolver-honest finding an active
  enforcement role inside C-4 entry-gates rather than passive
  "no-op pre-C-4" deferral; the F-V3-SYNTHESIS-1 mirror at
  SYNTHESIS-AUDIT-OVERFIT.md:343 propagates the same active-
  enforcement framing into the §3.1 C-4 prune-cluster row. SCAFFOLD-
  ONLY rows (W14.1-5, W13.1-4, W15.1) remain on-the-books at HEAD;
  no new scaffold-citing admit emerged.

**No sixth CH7-N criterion is required.** All five V3 fold packets
(F-V3-A4-1, F-V3-A5-1, F-V3-SYNTHESIS-1, F-V3-SYNTHESIS-2,
F-V3-SYNTHESIS-3) are prose-precision/scope-tightening edits inside
the existing five-criterion ceiling. F-V3-A4-1 is a CH1 line-count
correction (`json_provider.rs` 101→100 per `wc -l = 100`);
F-V3-A5-1 + F-V3-SYNTHESIS-1 are CH6 paper-close defence reinforcement
twins (action-class symmetry between §4 row 4 and §5:133 +
SYNTHESIS §3.1 C-4 row); F-V3-SYNTHESIS-2 is a CH1 prose-count refresh
of the codegen-side substrate enumeration the V2-folded glob already
covers; F-V3-SYNTHESIS-3 is a CH1 in-table NEW-2 cell sweep completing
the F-V2-SYNTHESIS-4 "Three"→"Four" arithmetic correction. No fold
exceeds the lens ceiling.

### §1.5 — Cross-flag: SYNTHESIS §3.1 C-3 row stale "3 scanners" residue (CH1 cross-flag, not CH7-N)

While scanning for the F-V3-SYNTHESIS-3 in-table cell fold's reach,
a single residual cell at SYNTHESIS-AUDIT-OVERFIT.md:342 was
identified that retains the V1 stale text:

```
| **C-3** = R4 + R5 | … | … A4 findings 2 + 3 + 4 + 5 + 6 + 7
  (no regen-css subcommand, 3 scanners are fixture lookups, 14/15
  .bbnf orphan); … | 11 |
```

The parenthetical clause "3 scanners are fixture lookups" should read
"4 scanners are fixture lookups" per the F-V2-SYNTHESIS-4 +
F-V3-SYNTHESIS-3 arithmetic correction:

- §0.1 (V2-folded): "Four of the seven CSS scanners"
- §1.1:81 (V3-folded): "NEW-2 (4 of 7 CSS scanners …)"
- §1.2:122 (V2-folded): "Four of the seven CSS scanners"
- §5.1 (V2-folded): "Four of the seven"
- `generator-truth.md:56` (V2-folded): "Four of the seven CSS scanners"
- **§3.1 C-3 row at line 342: STILL READS "3 scanners"** ← residual

**Executable verification of the correct count:**

```
$ grep -nE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT' \
    skinny/crates/codegen/src/css_l4_*_templates/generated.rs | wc -l
8
$ grep -lE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT' \
    skinny/crates/codegen/src/css_l4_*_templates/generated.rs
skinny/crates/codegen/src/css_l4_at_rules_and_media_templates/generated.rs
skinny/crates/codegen/src/css_l4_nested_layout_templates/generated.rs
skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/generated.rs
skinny/crates/codegen/src/css_l4_stylesheet_selectors_templates/generated.rs
```

Four files (`nested_layout`, `at_rules_and_media`,
`vendor_and_custom_atrules`, `stylesheet_selectors`) carry the
fixture-lookup short-circuit pattern; the correct count is **4 of 7**
(matches §0.1, §1.1, §1.2, §5.1, `generator-truth.md:56` all-folded).

**Disposition classification:** This is a **CH1 (internal cite
consistency) cross-flag**, not a CH7-N criterion violation. The five
CH7-N criteria are concerned with the **substrate-level recurrence of
the indicted patterns** (fake-generated header, generic-crate Lock-14
compliance, comparator binding, round-trip enforcement, scaffold-only
admits). The §3.1 C-3 row residual is a **single-cell
arithmetic-consistency drift** inside the audit's own narrative; it
does not re-open any CH7-N pattern (the **factual** finding-count of
4 fixture-lookup scanners is correctly cited at five sites in the
synthesis), it merely retains an undercount at one prune-cluster table
cell. The cross-flag is **informational** at CH7 — recorded here for
the V4 confirming pass / V4 micro-fold dispatch, not for V3 fold
revision.

No CH7-N criterion expansion required; the cross-flag is **purely
CH1**. Per the dispatch §3 report instruction ("any new finding"),
this is recorded as the **only** new finding the V3 CH7 confirming
pass surfaces against the V3 artefact set.

## §2 — Per-artefact disposition table (V3)

| # | Artefact | Section | Disposition | Rationale |
| --- | --- | --- | --- | --- |
| 1 | SYNTHESIS-AUDIT-OVERFIT.md V3 | §1.1:81 A4 NEW-2 cell "3 of 7"→"4 of 7" (F-V3-SYNTHESIS-3) | ACCEPT | The V3 fold completes the F-V2-SYNTHESIS-4 "Three"→"Four" arithmetic sweep at the §1.1 per-axis NEW-cluster table cell the V2 fold missed. `grep -nE 'CANONICAL_FIXTURE\|CAPTURED_W2_INPUT' skinny/crates/codegen/src/css_l4_*_templates/generated.rs | wc -l = 8` across 4 distinct files confirms the four-of-seven enumeration. CH1 internal-consistency preserved at §1.1; the parallel residual at §3.1 C-3 row line 342 is cross-flagged at §1.5 above as a V4 micro-fold candidate. |
| 2 | SYNTHESIS-AUDIT-OVERFIT.md V3 | §2.4 second-item "8 codegen-side template+provider files"→"14 codegen-side files (8 providers+templates + 6 ancillary)" (F-V3-SYNTHESIS-2) | **ACCEPT (mechanically consistent with CH7-1 + CH7-4)** | Per §1.2 above, the V3 prose refresh aligns the narrative-count description with the V2-folded lint glob's actual reach (`{runtime/src/grammars,codegen/src}/**/*.rs` catches all 14 codegen-side files; the V2 prose under-cited at 8). Executable verification `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/ | wc -l = 14` enumerates exactly 14 files; the 8 + 6 = 14 partition resolves to (7 CSS providers + JSON provider) + (4 json_templates body files + json_typed_direct + lib.rs). CH7-1 enforcement substrate **unchanged** (the glob's reach was already complete); CH7-4 round-trip surface **unchanged** (the 6 ancillary files are the codegen side of the identity-twin A4 finding 15 enumerates). No new CH7-N criterion required; the fold is **descriptive completeness** of the substrate the V2 fold already locked. |
| 3 | SYNTHESIS-AUDIT-OVERFIT.md V3 | §3.1:343 C-4 row A5 LOW cell "no-op pre-C-4"→"preserved through PRUNE-5 as C-4 entry-gate invariant" (F-V3-SYNTHESIS-1) | ACCEPT | The V3 fold mirrors A5 §4 row 4 line 118 phrasing into the SYNTHESIS §3.1 prune-cluster C-4 row, completing the V2 paper-close defence reinforcement (F-V2-SYNTHESIS-3 verdict-line alignment) into the prune-cluster table itself. The §3.1 arithmetic (4 findings in C-4 row; column-sum 41 + 7 + 11 + 4 + 11 = 74) holds; CH6 paper-close defence + CH7-5 SCAFFOLD-ONLY posture both **strengthened** by giving the LOW resolver-honest finding an active gate-rejection role rather than passive "no-op" deferral. |
| 4 | sk-v14-audit-overfit-decision-engine.md V3 (A5) | §5:133 action-class propagation (F-V3-A5-1) | ACCEPT | The V3 fold replaces the residual "the LOW finding (honest self-labelling) needs no action pre-C-4" with the active "preserved through PRUNE-5 as a gate-rejection invariant inside C-4 entry-gates so any admit attempting to cite W8/W9 pre-runtime-consumer is denied at admit time", restoring symmetry with §4 row 4 line 118 (the V1 CH6-R3 option (b) landing). The CH6-V2-N1 orphan REVISE that surfaced at V2 is closed; CH7-5 honesty tightening strengthened. |
| 5 | sk-v14-audit-overfit-generator-truth.md V3 (A4) | §1:153 methodology line-count "101 lines"→"100 lines" (F-V3-A4-1) | ACCEPT | `wc -l skinny/crates/codegen/src/json_provider.rs = 100` confirms the source-of-truth count; the V2 F-V2-A4-2 narrative recorded "99 → 101" but the correct refresh is "99 → 100" (off-by-one in the methodology summary line). CH1 cite-discipline preserved; the A4 ledger rows 8 / 10 / 11 already cite correct file-line coordinates per F-V2-A4-2 landing — only the methodology summary needed the +1 → 0 correction. |
| 6 | A1 css-measurement.md (V2 STAND, V3 STAND) | all §§ | ACCEPT (re-attested) | No V3 fold per V3 dispatch §2 micro-fold scope; V2 ACCEPT carries forward. Methodology + ledger + verdict + prune actions unchanged. |
| 7 | A2 admit-mechanism.md (V2 STAND, V3 STAND) | all §§ | ACCEPT (re-attested) | No V3 fold per V3 dispatch §2 micro-fold scope; V2 ACCEPT carries forward. F8 + F9 NEW findings preserved at V2 disposition. |
| 8 | A3 lock14-scan.md (V2 STAND, V3 STAND) | all §§ | ACCEPT (re-attested) | No V3 fold per V3 dispatch §2 micro-fold scope; V2 ACCEPT carries forward. H3 HIGH→LOW reclassification + H6 freestanding HIGH + L8 record preserved; 11 CRIT + 6 HIGH + 5 MED + 8 LOW = 30 unchanged. |
| 9 | A6 pre-restart-pattern.md (V2 STAND, V3 STAND) | all §§ | ACCEPT (re-attested) | No V3 fold per V3 dispatch §2 micro-fold scope; V2 ACCEPT carries forward. LegacyPath both-readings-preserved disambiguation at §0:12 + §2 ledger Status preserved. |
| 10 | Cross-cutting | 11 NEW finding *categories* (20 NEW *rows*) vs 5 CH7-N criteria (V3 re-enumeration) | ACCEPT | All 11 categories map within the five-criterion ceiling (per §1.4 fresh-finding scan). The V3 fold packets add zero new categories and zero new rows; the 20-NEW-row / 11-NEW-category statistical accounting holds verbatim. No sixth criterion required. |
| 11 | Cross-cutting | Audit-pattern emergence scan (V3 confirming) | ACCEPT | No new audit pattern emerges between V2 and V3 that the 5 CH7-N criteria miss. The V3 folds tighten prose precision (F-V3-A4-1, F-V3-SYNTHESIS-2, F-V3-SYNTHESIS-3) and reinforce paper-close defence (F-V3-A5-1, F-V3-SYNTHESIS-1) — all within the existing five-criterion ceiling. |
| 12 | Cross-cutting | C-3 round-trip gate enforcement check (V3 re-execution) | ACCEPT | Per §1.1 above, the metadata-derived loop continues to enumerate 9 grammars including `css_pretty` at V3 HEAD `007624849`; the gate is mechanically defended end-to-end across three consecutive cycle boundaries (V1 + V2 + V3). |
| 13 | Cross-cutting | F-V3-SYNTHESIS-2 mechanical consistency with CH7-1 + CH7-4 lint substrate (V3 NEW verification) | **ACCEPT (substrate unchanged; prose tightened)** | Per §1.2 above, the V3 prose refresh "8 → 14 codegen-side files (8 providers+templates + 6 ancillary)" aligns the narrative with the V2-folded lint glob's already-complete reach. The substrate (`{runtime/src/grammars,codegen/src}/**/*.rs`) catches all 14 codegen-side + 42 runtime-side = 56 files regardless; the prose-count refresh eliminates the V2 prose-vs-substrate asymmetry where the glob caught 14 but the prose mentioned 8. CH7-1 + CH7-4 substrate is **unchanged**; no new CH7-N criterion introduced; F-V3-SYNTHESIS-2 is **CH1-precision** tightening of the **CH7-narrative description**. |
| 14 | Cross-cutting | No fake-`@generated` recurrence in V3 audit prose | ACCEPT | The three V3-touched files carry zero `@generated by skinny bbnf-codegen` headers in their own text bodies (the strings appear only inside quoted code blocks of audited source files). V2 disposition carries forward unchanged at V3. |
| 15 | Cross-cutting | No scaffold-as-load-bearing in V3 audit prose | ACCEPT | The V3 audit-prose makes no load-bearing claim resting on a SCAFFOLD-ONLY artefact. A5 V3 §5:133 + SYNTHESIS V3 §3.1:343 in fact **strengthen** the FAIL-at-HEAD reading by giving the LOW resolver-honest finding an active gate-rejection role inside C-4 entry-gates; the verdict no longer reads SCAFFOLD-bearing W14.1-5 rows as already-closed. |
| 16 | Cross-cutting | No gate-relabel-as-admit in V3 audit prose | ACCEPT | F1-F5 remain classified gate-relabel-only via per-commit `git show --stat` evidence (V1 + V2 disposition carry forward); A5 V3 does not stamp any "row" as ADMITTED on gate-relabel basis. The "preserved through PRUNE-5 as C-4 entry-gate invariant" V3-folded posture is the **inverse** of gate-relabel adoption — it makes the entry-gate the active rejection point for any admit attempting to cite W8/W9 pre-runtime-consumer. |
| 17 | Cross-cutting | CH7 lens carried as blocking (not merely acknowledged) at V3 | ACCEPT | Per the lens definition ("CH7 cannot be carried as 'acknowledged but not blocking'"), the V3 audit's recommended actions remain revert-or-rewire (PRUNE-1, PRUNE-2 binding under C-5; PRUNE-5 binding under C-4) — not advisory acknowledgement. F-V3-A5-1 + F-V3-SYNTHESIS-1 action-class propagation makes the binding **more explicit**, not less. |
| 18 | Cross-cutting | Three architectural sequencing constraints preserved at V3 | ACCEPT | The three constraints (R4 → PRUNE-2; C-1 → C-4; PRUNE-4 = 9 sub-waves) carry forward at V3 with zero modification. The V3 axis-redispatch surface (5 prose edits across 3 files) is below any envelope-perturbation threshold; SYNTHESIS §0.1 + §1.3 + §2.3 + §3.1 + §5.1 all preserve the constraints verbatim. |

ACCEPT-rate: **18 / 18 = 100.0 %.** Zero REVISE or REJECT
dispositions.

## §3 — Critical findings against the V3 audit's own write-up

**None.** The three V3-touched files preserve the V2 CH7-clean posture
and tighten three prose-precision items + reinforce two CH6 paper-
close defence items without introducing any recurrence vector against
the indicted patterns. Specifically:

- **No fake-`@generated` recurrence in V3 audit prose.** The three
  V3-touched files (SYNTHESIS, A4, A5) carry the string only inside
  quoted source-code blocks of the audited code, not in their own
  headers.
- **No scaffold-as-load-bearing claim in V3 audit prose.** A5 V3 + SYNTHESIS
  V3 in fact **strengthen** the SCAFFOLD-ONLY reading via F-V3-A5-1 +
  F-V3-SYNTHESIS-1, giving the LOW resolver-honest finding an active
  gate-rejection role inside C-4 entry-gates.
- **No gate-relabel adoption in V3 audit prose.** A5 V3 explicitly
  reinforces the rejection (active gate-rejection invariant); no V3
  fold reverses any V13 gate-relabel rejection.
- **No orphan grammar dressed as load-bearing in V3 audit prose.** A4
  V3 NEW-3 stands (14 of 15 `.bbnf` files orphaned); V3 confirms the
  count via `find /Users/mkbabb/Programming/bbnf-lang/grammar/css/l4
  -name "*.bbnf" | wc -l = 15` and the orphan-15 enumeration.

## §4 — V4 fold recommendations

**One single-cell prose touch** is recommended for V4 micro-fold
dispatch, classified as CH1 (internal cite consistency), not CH7.
Recording here for V4 attribution rather than V3 fold revision:

**Recommendation 1 (CH1 cross-flag; informational at CH7):** Edit
`SYNTHESIS-AUDIT-OVERFIT.md §3.1` C-3 row at line 342 — replace:

> A4 findings 2 + 3 + 4 + 5 + 6 + 7 (no regen-css subcommand, 3
> scanners are fixture lookups, 14/15 .bbnf orphan)

with:

> A4 findings 2 + 3 + 4 + 5 + 6 + 7 (no regen-css subcommand, 4
> scanners are fixture lookups, 14/15 .bbnf orphan)

Completes the F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3 arithmetic-
correction sweep at the final residual cell. Verification: the same
`grep -nE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT' skinny/crates/codegen/
src/css_l4_*_templates/generated.rs` returns 8 hits across 4 distinct
files (`nested_layout`, `at_rules_and_media`,
`vendor_and_custom_atrules`, `stylesheet_selectors`); the four-of-
seven count is the correct enumeration matching §0.1 + §1.1:81 +
§1.2:122 + §5.1 + `generator-truth.md:56`. Mechanism precision, not
finding-class revision.

Recommended V4 dispatch posture (informational, not a fold against V3):

1. **Re-execute the §1.1 `cargo metadata | jq` command** at the V4
   HEAD and confirm the 9-grammar enumeration including `css_pretty`
   continues to hold. Pass-Alpha V3→V4→V5 institutionalised this
   re-attestation discipline; CH7 inherits it.
2. **Re-attest the §1.2 F-V3-SYNTHESIS-2 prose / F-V2-SYNTHESIS-5 lint
   glob substrate coverage** at V4 HEAD — confirm the 42 + 14 = 56 file
   twin-site enumeration still holds; confirm the codegen-side twin
   closure still binds; confirm the V3 prose refresh continues to
   match the substrate's reach.
3. **Re-attest the §1.4 audit-pattern-emergence scan** at V4 — that
   no new audit pattern has surfaced between V3 and V4 that the five
   CH7-N criteria miss.
4. **No source-touch.** Per the V1 §3 dispatch discipline, V4 remains
   write-only; the prune list at SYNTHESIS §3 binds the S-P3 wave
   manifest, not V4.

The CH7-companion gating extensions (round-trip subcommand pairing +
LOCKS.md companion-lint with the V2-folded twin-side scope, now
narratively complete via F-V3-SYNTHESIS-2) remain correctly routed as
gating enhancements inside C-3 + LOCKS.md and as attribution items
for S-P3, not as V4 CH7 follow-on items.

## §5 — Disposition

The S-P0 V3 audit is CH7-clean against its own write-up. The five
CH7-N criteria continue to cover the 11 NEW finding categories (20
NEW per-row) without expansion across the V1 → V2 → V3 cycle
boundaries; F-V3-SYNTHESIS-2's "8 → 14 codegen-side files" prose
refresh aligns the narrative description with the V2-folded lint
glob's already-complete reach (`{runtime/src/grammars,codegen/src}/
**/*.rs` catches all 14 codegen-side + 42 runtime-side = 56 files);
the substrate is **mechanically unchanged**; the prose now correctly
narrates the substrate's full reach including the 6 ancillary files
(json_templates body + json_typed_direct + lib.rs). CH7-1 + CH7-4
enforcement substrates are **unchanged**; F-V3-SYNTHESIS-2 is a
**CH1-precision** tightening of the **CH7-narrative description**.
The §1.1 C-3 round-trip gate metadata enumeration confirms 9 grammars
including `css_pretty` at V3 HEAD `007624849`, matching the audit's
PRUNE-4 = 9 sub-wave binding (F-V2-SYNTHESIS-2 co-derivation note
documents the +3 / +1 deltas as cross-confirmed but co-derived). One
new finding surfaces against the V3 artefact set — a CH1-class
residual in SYNTHESIS §3.1 C-3 row at line 342 (stale "3 scanners are
fixture lookups" that should read "4"); classified as **informational
cross-flag** at CH7 (not a CH7-N criterion violation), recorded as a
V4 micro-fold candidate. ACCEPT-rate **100.0 %**; zero escalation;
V3 cycle establishes the third clean consecutive cycle in the §3Z
two-consecutive-cycle convergence chain (strict reading: V3 is the
first clean cycle post-V2 orphan-REVISE-close; permissive reading: V3
closes the chain).

---

**Scope:** S-P0 V3 CHALLENGE (SK-V14 Overfit Audit Pass) — CH7
Overfit-Prune (meta-applied to the V3 axis-redispatched audit
artefacts at `007624849`).
**Authority:** `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`
+ `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/
CHALLENGE-CONTEXT.md` + `restart/skinny/tranches/sk-v14/audit-overfit/
hardening/V2/CHALLENGE-V2-ADDENDUM.md`.
**Status:** WRITE-ONLY (untracked); aggregator commits 7 V3 lens
files + V3 CONSOLIDATED atomically.
**Next gate:** CH1-CH6 V3 + V3 aggregator complete; V4 confirming
pass closes §3Z LOCK → G-S-P0-CONVERGED gates S-P1 dispatch per the
SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.
