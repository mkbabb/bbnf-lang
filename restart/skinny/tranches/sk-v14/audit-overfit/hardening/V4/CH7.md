# CH7 Overfit-Prune — S-P0 V4 Disposition (SK-V14 Overfit Audit)

Lens binding unchanged: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`.
Five CH7 criteria remain the disposition spine — (CH7-1) every
"generated" artefact is grammar-derived (template + grammar metadata
+ emission command), never hand-written under a `// @generated`
header; (CH7-2) Lock 14 generic-crate compliance preserved (no
JSON/CSS/Sheets literals, function names, enum variants, match arms
in nominally-generic code); (CH7-3) every admit lands via a real
parser/codegen/SIMD source change measured against a strict-vs-strict
comparator on the same plane with a per-iteration equality oracle;
(CH7-4) every "generated" output passes a round-trip test (delete +
regen ⇒ byte-equivalent); (CH7-5) no SCAFFOLD-ONLY landing counts as
an admit. The V4 confirming pass re-attests the V3 100 % verdict
across the single V4-touched artefact (commit `f8e279877`, 1 file
+9 / -4 prose-only on SYNTHESIS-AUDIT-OVERFIT.md), re-executes the
C-3 round-trip metadata-derived loop at V4 HEAD, verifies the
F-V4-SYNTHESIS-1 disjoint-sets arithmetic ("56 files (42 runtime +
14 codegen)") through two independent `git grep` invocations, and
confirms F-V4-SYNTHESIS-2 closes the V3 §1.5 inheritance-miss flag
that cross-flagged the §3.1 C-3 row "3 scanners" residual at line
342.

## §0 — Disposition summary

- Artefacts reviewed: **1** (SYNTHESIS-AUDIT-OVERFIT.md V4 — two
  single-phrase prose edits at §2.4:308-318 + §3.1:347). A1, A2, A3,
  A4, A5, A6 STAND verbatim (no V4 folds) per the V3 CONSOLIDATED §2
  micro-fold scope (single SYNTHESIS V4 agent dispatch closing both
  V3 orphan REVISEs).
- Per-section dispositions issued: **18** (15 V3 sections re-attested
  + 3 new V4-fold-specific sections: F-V4-SYNTHESIS-1 disjoint-sets
  inclusion-relation correction + F-V4-SYNTHESIS-2 §3.1 C-3 row
  arithmetic-correction sweep closure + cross-cycle stability of the
  56-file lint-glob substrate now narratively coherent).
- ACCEPT: **18**.
- REVISE: **0**.
- REJECT: **0**.
- ACCEPT-rate: 18 / 18 = **100.0 %**.
- Critical findings against the V4 audit's own write-up: **0**.
- New finding: **0**. The V4 fold packets close both V3 orphan
  REVISEs (the CH2 N-V3-CH2-1 inclusion-relation defect and the
  CH7 §1.5 inheritance-miss flag at SYNTHESIS §3.1 C-3 row line 342);
  no fresh CH7-N pattern emerges from the V4 prose-only delta. The
  V3 §1.5 cross-flag is **CLOSED** by F-V4-SYNTHESIS-2 — the §3.1
  C-3 row now reads "4 scanners are fixture lookups" matching the
  five other folded sites (§0.1, §1.1:81, §1.2:122, §5.1,
  `generator-truth.md:56`).
- Escalation flag: **NO.** The single V4-touched file preserves the
  V3 CH7-clean posture: zero fake-`@generated` header recurrence in
  audit prose, zero scaffold-as-load-bearing claim, zero gate-relabel
  adoption, zero orphan-grammar dressed as load-bearing. The V4 folds
  tighten arithmetic precision (F-V4-SYNTHESIS-1 disjoint-sets phrasing
  for the codegen+runtime twin enumeration; F-V4-SYNTHESIS-2 final
  residual cell of the F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3 sweep) and
  add zero new CH7-N criteria. The §3Z LOCK convergence chain is
  now in its **fourth consecutive clean cycle** (V1 → V2 → V3 → V4).

## §1 — Executable verification (per §3 mandate)

### §1.1 — C-3 round-trip gate metadata-derived loop re-executed (V4 HEAD)

Per the dispatch §1 mandate ("re-execute `cargo metadata --format-
version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'` (expect
9 unchanged at V4)"), the canonical C-3 round-trip metadata command
was re-executed against the live workspace at `HEAD = f8e279877`.

**Command (verbatim from `alpha-E-candidate-shortlist.md:366-367` /
V1 CH7 §1.1 / V2 CH7 §1.1 / V3 CH7 §1.1):**

```
cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
```

**Output (captured in `/Users/mkbabb/Programming/bbnf-lang` at V4
HEAD `f8e279877`):**

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
This is byte-identical to the V1 CH7 §1.1 attestation (HEAD
`7d0fbe071`), the V2 CH7 §1.1 attestation (HEAD `42e3edb9a`), the V3
CH7 §1.1 attestation (HEAD `007624849`), and to the V5 Pass-Alpha
attestation (`restart/skinny/tranches/sk-v14/research/alpha-hardening/
V5/CH7.md §1.1`). The ninth grammar `css_pretty` continues to enumerate
at the gate's runtime under S-P0 V4; the SYNTHESIS-AUDIT-OVERFIT V4
§2.3 "PRUNE-4 = 9 sub-waves" binding (and its V2-folded §1.3
co-derivation note crediting the +1 to the single `css_pretty`
directory addition) reconciles to this enumeration verbatim across
four consecutive cycle boundaries (V1 → V2 → V3 → V4).

### §1.2 — F-V4-SYNTHESIS-1 disjoint-sets arithmetic verified via two independent `git grep` invocations

Per the dispatch §1 mandate ("verify F-V4-SYNTHESIS-1 disjoint
arithmetic correct (42+14=56 via two git greps)"), the V4
SYNTHESIS-AUDIT-OVERFIT.md §2.4 second-item parenthetical was re-read
at HEAD `f8e279877` and the underlying disjoint-set claim re-validated
through two independent grep invocations against the two sibling path
roots.

**Pre-V4 prose (V3 baseline, V3 SYNTHESIS-AUDIT-OVERFIT.md:308-315):**

```
   42 files carry the fake header including 14 codegen-side files
   (8 providers+templates + 6 ancillary; `git grep -l '@generated by
   skinny bbnf-codegen' skinny/crates/codegen/src/ | wc -l = 14`).
   The lint glob `codegen/src/**/*.rs` catches all 14 regardless;
   only the prose count needs the 8-vs-14 distinction.
```

The V3 prose's "42 ... including 14" wording is an
**inclusion-relation defect** — it implies the 14 codegen-side files
are a subset of the 42 runtime-side files, but the two counts are
**disjoint sets** under the lint glob brace-expansion
`{runtime/src/grammars,codegen/src}/**/*.rs`. CH2 V3 §4.1 surfaced
this as N-V3-CH2-1 LOW; CH7 V3 §1.5 cross-flagged the parallel §3.1
residual. The V4 fold corrects the arithmetic to disjoint-sets
phrasing.

**Post-V4 prose (V4 fold landed at SYNTHESIS-AUDIT-OVERFIT.md:308-318):**

```
   56 files carry the fake header (42 runtime-side mirror + 14
   codegen-side (8 providers+templates + 6 ancillary)) — disjoint
   sets per the lint glob brace-expansion
   `{runtime/src/grammars,codegen/src}/**/*.rs`; `git grep -l
   '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/
   | wc -l = 42` and `git grep -l '@generated by skinny bbnf-codegen'
   skinny/crates/codegen/src/ | wc -l = 14`, summing to the 56 total
   with zero overlap (the two path roots are siblings).
```

**Executable re-verification of the disjoint-arithmetic claim via
two independent invocations:**

```
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ | wc -l
42
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/ | wc -l
14
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ skinny/crates/codegen/src/ | wc -l
56
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ skinny/crates/codegen/src/ | sort -u | wc -l
56
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ skinny/crates/codegen/src/ | awk -F/ '{print $3}' | sort -u
codegen
runtime
```

42 + 14 = 56 holds verbatim, and the `sort -u` invariance confirms
zero path-overlap between the two sibling roots (every file lives
under exactly one of `runtime/src/grammars/` or `codegen/src/`, never
both). The disjoint-sets claim is mechanically true — the brace-
expansion glob unions two non-overlapping path-prefix sets.

**Mechanical consistency with CH7-1 (no fake `@generated` outside
emission roster):** The V4 prose refresh aligns the **narrative
description** with the **substrate's actual reach**. The V2-folded
lint glob `{runtime/src/grammars,codegen/src}/**/*.rs` already
protected all 56 files mechanically; the V3 prose under-described the
disjoint-set relation by phrasing it as inclusion ("42 including 14"),
and the V4 fold restores the precise disjoint-sets arithmetic. CH7-1
enforcement substrate is **unchanged**; the V4 fold is a
**CH2-arithmetic-precision** tightening of the **CH7-1 narrative
description**. No new CH7-N criterion required; no recurrence vector
introduced.

**Mechanical consistency with CH7-4 (round-trip test for every
generated output):** The 56-file census is the union of the codegen-
side template/provider files (14, the round-trip source) and the
runtime-side mirror files (42, the round-trip target). The V4 fold
makes the **disjoint-set structure of the round-trip surface**
narratively explicit — the codegen side is the source-of-truth that
emits onto the runtime side; they are not the same files counted
twice but two halves of an identity-twin pair. CH7-4 round-trip
surface census is **unchanged in shape and reach**; the V4 prose
refresh is **descriptive precision** of the twin-substrate the
V2-folded lint glob already protected.

F-V4-SYNTHESIS-1 lands cleanly; CH7-1 + CH7-4 are **mechanically
unchanged** at the lint-glob substrate; the V4 prose now correctly
narrates the disjoint-sets arithmetic relation across the substrate's
twin path roots.

### §1.3 — F-V4-SYNTHESIS-2 closes V3 CH7 §1.5 inheritance-miss flag

Per the dispatch §1 mandate ("verify F-V4-SYNTHESIS-2 closes your
V3 §1.5 inheritance-miss flag"), the V3 CH7 §1.5 cross-flag is
re-read here and the V4 fold's closure verified at HEAD `f8e279877`.

**V3 CH7 §1.5 cross-flag (verbatim from V3/CH7.md:315-369):**

> While scanning for the F-V3-SYNTHESIS-3 in-table cell fold's reach,
> a single residual cell at SYNTHESIS-AUDIT-OVERFIT.md:342 was
> identified that retains the V1 stale text "3 scanners are fixture
> lookups" inside the §3.1 C-3 row A4 findings cluster while every
> other site has folded to "4". Classified as **informational
> cross-flag** at CH7 (not a CH7-N criterion violation); recorded as
> a V4 micro-fold candidate.

**V4 fold (landed at SYNTHESIS-AUDIT-OVERFIT.md:347):**

```diff
-| **C-3** = R4 + R5 | … | A4 findings 2 + 3 + 4 + 5 + 6 + 7
-  (no regen-css subcommand, 3 scanners are fixture lookups, 14/15
-  .bbnf orphan); A4 finding 1 … | 11 |
+| **C-3** = R4 + R5 | … | A4 findings 2 + 3 + 4 + 5 + 6 + 7
+  (no regen-css subcommand, 4 scanners are fixture lookups, 14/15
+  .bbnf orphan); A4 finding 1 … | 11 |
```

**Executable re-verification of the four-of-seven count:**

```
$ grep -lE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT' \
    skinny/crates/codegen/src/css_l4_*_templates/generated.rs
skinny/crates/codegen/src/css_l4_at_rules_and_media_templates/generated.rs
skinny/crates/codegen/src/css_l4_nested_layout_templates/generated.rs
skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/generated.rs
skinny/crates/codegen/src/css_l4_stylesheet_selectors_templates/generated.rs
$ grep -lE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT' \
    skinny/crates/codegen/src/css_l4_*_templates/generated.rs | wc -l
4
```

Four files (`at_rules_and_media`, `nested_layout`,
`vendor_and_custom_atrules`, `stylesheet_selectors`) carry the
fixture-lookup short-circuit pattern; the correct count is **4 of 7**
matching every other folded site (§0.1, §1.1:81, §1.2:122, §5.1,
`generator-truth.md:56`).

**Closure verification of the V3 §1.5 inheritance-miss flag:** Every
site enumerated in V3 CH7 §1.5 (§0.1 V2-folded, §1.1:81 V3-folded,
§1.2:122 V2-folded, §5.1 V2-folded, `generator-truth.md:56` V2-folded,
+ §3.1 C-3 row at line 342 — V4-folded) now reads "4". The
F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3 + F-V4-SYNTHESIS-2 arithmetic-
correction sweep is **complete**; no further "3 scanners" residual
remains in SYNTHESIS-AUDIT-OVERFIT.md or in `generator-truth.md`.
The V3 §1.5 inheritance-miss flag is **CLOSED** at V4 HEAD.

**Mechanical consistency with CH7-3 (strict-vs-strict comparator +
per-iter oracle):** The "4 scanners are fixture lookups" finding is
the A4 evidence cluster that establishes — at the C-3 prune-cluster
table row — the **mechanism by which the 24 CSS L4 rows were admitted
without a real strict-vs-strict comparator** (the fixture-lookup
short-circuit returned the canonical bytes without re-parsing the
input, making the strict-vs-strict pair degenerate). The V4 fold
restores the correct count (4 of 7) so the C-3 prune-cluster row
matches the actual scope of the comparator-binding pathology the A4
evidence reaches. CH7-3 substrate is **unchanged**; the V4 fold is a
**CH1-cite-precision** + **CH2-arithmetic-precision** tightening of
the **CH7-3 narrative description**. No new CH7-N criterion required.

### §1.4 — Cross-axis sub-wave count consistency check (V4 confirming)

Per the V1 CHALLENGE-CONTEXT §2 bound-fact "PRUNE-4 sub-wave count is
9 not 8 (css_pretty added between V13 and SK-V14)", the V4-touched
file preserves the four-corner consistency from V1 / V2 / V3:

- A3 V2 §1 (unchanged through V3 / V4 STAND): `find crates/core/src/
  runtime -mindepth 1 -maxdepth 1 -type d` returns 9 directories.
- A5 V3 §2.1 (verdict-line FAIL-at-HEAD aligned at V2; STAND
  structurally at V3 / V4): "9 dirs — one over the dispatch-cited 8
  because of `css_pretty`".
- A6 V2 §1 (unchanged through V3 / V4 STAND): per-grammar census
  table sums to 9 directories / 67 files.
- SYNTHESIS V4 §1.3 + §2.3 (STAND from V3): "Pattern H file count:
  64 → 67"; the +3 file delta and +1 sub-wave delta attributed to
  the single `css_pretty` addition; the three A3/A5/A6 cross-checks
  remain **co-derived, not orthogonal**.

**Re-executed verification (V4 HEAD `f8e279877`):**

```
$ find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l
9
$ find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d
/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/google_sheets
/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/bnf
/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/css_l4
/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/ebnf
/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/math
/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/bbnf
/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/json
/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/csv
/Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/css_pretty
```

§1.1 metadata enumeration above confirms the ninth grammar at the
manifest layer; this filesystem enumeration confirms it at the
runtime-crate layer. The cross-axis coherence holds without exception
across the V1 → V2 → V3 → V4 cycle boundary — **four consecutive
clean cycles**.

**Cross-version stability of the 56-file lint-glob coverage
(F-V2-SYNTHESIS-5 substrate; F-V3-SYNTHESIS-2 prose; F-V4-SYNTHESIS-1
disjoint-sets refresh):**

```
$ git grep -l '@generated by skinny bbnf-codegen' \
    skinny/crates/runtime/src/grammars/ \
    skinny/crates/codegen/src/ | wc -l
56
```

56 = 42 + 14 holds verbatim at V4 HEAD; the V2 confirmation (HEAD
`42e3edb9a`) returned 56; the V3 confirmation (HEAD `007624849`)
returned 56. **Four-cycle stability is established** — the lint glob
substrate is unchanged across the entire convergence chain, and the
V4 prose now narrates the substrate's reach with disjoint-sets
arithmetic precision.

### §1.5 — Fresh-finding scan across the 5 CH7-N criteria (V4 confirming)

Per the dispatch §1 mandate ("fresh-finding scan across all 5 CH7-N
criteria"), each criterion was independently re-scanned against the
V4 artefact set + the underlying source repository at V4 HEAD:

- **CH7-1 (no fake `@generated` outside emission roster):** V1 + V2
  scope-extension stands at 8 sites of authored render drivers
  (7 CSS providers + JSON provider); the V3 prose enumerated the
  14-file codegen-side reach; the V4 prose now correctly narrates
  the 42-runtime + 14-codegen = 56 disjoint-sets total the V2-folded
  lint glob already protected mechanically. The substrate is
  unchanged across V1 → V2 → V3 → V4; only the narrative description
  tightened arithmetically. No new fake-`@generated` recurrence site
  emerged between V3 and V4 (the single-file V4 commit `f8e279877`
  touches zero source bytes — 9 prose insertions + 4 prose deletions
  on `SYNTHESIS-AUDIT-OVERFIT.md` only). The 3 occurrences of the
  string `@generated by skinny bbnf-codegen` in the V4-touched file
  appear only inside backticks/inline-code (quoting the pattern the
  lint should reject), never as a real `// @generated` header.
- **CH7-2 (Lock 14 generic-crate compliance):** A3 V2 H3 HIGH→LOW
  reclassification + H6 freestanding HIGH preservation carry forward
  at V3 / V4 STAND; the 11 CRIT + 6 HIGH + 5 MED + 8 LOW = 30
  distribution holds unchanged. The V4 disjoint-sets refresh in
  F-V4-SYNTHESIS-1 is a CH2-class arithmetic precision tightening
  (N-V3-CH2-1 closure) — it strengthens, rather than weakens, the
  Lock-14 narrative coherence by aligning the prose-described
  substrate reach with the lint glob's actual reach. No new Lock-14
  surface emerged.
- **CH7-3 (strict-vs-strict comparator + per-iter oracle):** A2
  STANDs from V1 / V2 / V3 (no V4 fold); F8 + F9 NEW findings
  preserve their V2 disposition. The F-V4-SYNTHESIS-2 §3.1 C-3 row
  refresh ("3 scanners" → "4 scanners") aligns the prune-cluster
  table's count with the A4 evidence the comparator-binding
  pathology rests on; no new comparator-binding pathology emerged.
- **CH7-4 (round-trip test for every generated output):** A6 V2
  LegacyPath shim disambiguation stands at V3 / V4; F-V4-SYNTHESIS-1
  prose refresh makes the round-trip surface census **descriptively
  complete with disjoint-sets arithmetic** (14 codegen-side files +
  42 runtime-side files = 56 union, no overlap) without expanding
  the surface shape. The V2-folded lint glob substrate
  (`{runtime/src/grammars,codegen/src}/**/*.rs`) remains the
  round-trip-runtime-AND-lint-CI defence on both sides of the twin.
- **CH7-5 (no SCAFFOLD-ONLY admit):** A5 V3 §5:133 action-class
  refresh ("preserved through PRUNE-5 as a gate-rejection invariant
  inside C-4 entry-gates so any admit attempting to cite W8/W9 pre-
  runtime-consumer is denied at admit time") **strengthens** the
  CH7-5 posture by giving the LOW resolver-honest finding an active
  enforcement role inside C-4 entry-gates rather than passive
  "no-op pre-C-4" deferral; the F-V3-SYNTHESIS-1 mirror at
  SYNTHESIS-AUDIT-OVERFIT.md:343 propagated the same active-
  enforcement framing into the §3.1 C-4 prune-cluster row; the V4
  fold does not touch §3.1 C-4 row content (only the §3.1 C-3 row
  count cell). SCAFFOLD-ONLY rows (W14.1-5, W13.1-4, W15.1) remain
  on-the-books at HEAD; no new scaffold-citing admit emerged.

**No sixth CH7-N criterion is required.** Both V4 fold packets
(F-V4-SYNTHESIS-1, F-V4-SYNTHESIS-2) are arithmetic-precision /
inclusion-relation-correction edits inside the existing
five-criterion ceiling. F-V4-SYNTHESIS-1 is a CH2 disjoint-sets
correction (N-V3-CH2-1 closure); F-V4-SYNTHESIS-2 is a CH1 cite-
arithmetic correction (V3 §1.5 cross-flag closure, final residual of
the F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3 sweep). Neither fold exceeds
the lens ceiling.

**No fresh CH7-N pattern emerges from the V4 prose-only delta.**
The single-file commit `f8e279877` (+9 / -4 prose lines on
SYNTHESIS-AUDIT-OVERFIT.md, zero source bytes) is the smallest
hardening-cycle delta in the chain; both edits close pre-identified
V3 orphan REVISEs; no new audit pattern surfaces.

## §2 — Per-artefact disposition table (V4)

| # | Artefact | Section | Disposition | Rationale |
| --- | --- | --- | --- | --- |
| 1 | SYNTHESIS-AUDIT-OVERFIT.md V4 | §2.4:308-318 inclusion-relation defect closure ("42 including 14" → "56 (42 runtime + 14 codegen) disjoint sets") (F-V4-SYNTHESIS-1) | **ACCEPT (closes V3 CH2 N-V3-CH2-1)** | Per §1.2 above, two independent `git grep` invocations confirm 42 runtime-side + 14 codegen-side = 56 disjoint-sets total; `sort -u` invariance confirms zero path overlap between sibling roots `skinny/crates/runtime/src/grammars/` and `skinny/crates/codegen/src/`. The V4 prose now correctly narrates disjoint-sets arithmetic, eliminating the V3 inclusion-relation defect. CH7-1 + CH7-4 enforcement substrate **unchanged** (the lint glob's reach was already complete); the fold is **CH2-arithmetic-precision** tightening of the **CH7-narrative description**. |
| 2 | SYNTHESIS-AUDIT-OVERFIT.md V4 | §3.1:347 C-3 row "3 scanners are fixture lookups" → "4 scanners are fixture lookups" (F-V4-SYNTHESIS-2) | **ACCEPT (closes V3 CH7 §1.5 inheritance-miss flag)** | Per §1.3 above, `grep -lE 'CANONICAL_FIXTURE\|CAPTURED_W2_INPUT' skinny/crates/codegen/src/css_l4_*_templates/generated.rs | wc -l = 4` confirms the four-of-seven count; the four files (`at_rules_and_media`, `nested_layout`, `vendor_and_custom_atrules`, `stylesheet_selectors`) carry the fixture-lookup short-circuit pattern. The V4 fold completes the F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3 arithmetic-correction sweep at the final residual cell; every site (§0.1, §1.1:81, §1.2:122, §3.1:347, §5.1, `generator-truth.md:56`) now reads "4". CH1 internal-consistency restored across the full SYNTHESIS narrative; V3 CH7 §1.5 cross-flag **CLOSED**. |
| 3 | A1 css-measurement.md (V2 STAND, V3 STAND, V4 STAND) | all §§ | ACCEPT (re-attested) | No V4 fold per V3 CONSOLIDATED §2 micro-fold scope (single SYNTHESIS V4 agent only); V3 ACCEPT carries forward. Methodology + ledger + verdict + prune actions unchanged. |
| 4 | A2 admit-mechanism.md (V2 STAND, V3 STAND, V4 STAND) | all §§ | ACCEPT (re-attested) | No V4 fold per V3 CONSOLIDATED §2 micro-fold scope; V3 ACCEPT carries forward. F8 + F9 NEW findings preserved at V2 disposition. |
| 5 | A3 lock14-scan.md (V2 STAND, V3 STAND, V4 STAND) | all §§ | ACCEPT (re-attested) | No V4 fold per V3 CONSOLIDATED §2 micro-fold scope; V3 ACCEPT carries forward. H3 HIGH→LOW reclassification + H6 freestanding HIGH + L8 record preserved; 11 CRIT + 6 HIGH + 5 MED + 8 LOW = 30 unchanged. |
| 6 | A4 generator-truth.md (V3 STAND, V4 STAND) | all §§ | ACCEPT (re-attested) | No V4 fold per V3 CONSOLIDATED §2 micro-fold scope; V3 ACCEPT carries forward. F-V3-A4-1 methodology line-count refresh (101 → 100) preserved; the A4 ledger rows 8 / 10 / 11 file-line coordinates remain accurate per F-V2-A4-2 landing. |
| 7 | A5 decision-engine.md (V3 STAND, V4 STAND) | all §§ | ACCEPT (re-attested) | No V4 fold per V3 CONSOLIDATED §2 micro-fold scope; V3 ACCEPT carries forward. F-V3-A5-1 §5:133 action-class propagation ("preserved through PRUNE-5 as a gate-rejection invariant inside C-4 entry-gates") preserved; CH6-V2-N1 orphan-REVISE closure carries forward. |
| 8 | A6 pre-restart-pattern.md (V2 STAND, V3 STAND, V4 STAND) | all §§ | ACCEPT (re-attested) | No V4 fold per V3 CONSOLIDATED §2 micro-fold scope; V3 ACCEPT carries forward. LegacyPath both-readings-preserved disambiguation at §0:12 + §2 ledger Status preserved. |
| 9 | Cross-cutting | 11 NEW finding *categories* (20 NEW *rows*) vs 5 CH7-N criteria (V4 re-enumeration) | ACCEPT | All 11 categories map within the five-criterion ceiling (per §1.5 fresh-finding scan). The V4 fold packets add zero new categories and zero new rows; the 20-NEW-row / 11-NEW-category statistical accounting holds verbatim. No sixth criterion required. |
| 10 | Cross-cutting | Audit-pattern emergence scan (V4 confirming) | ACCEPT | No new audit pattern emerges between V3 and V4 that the 5 CH7-N criteria miss. The V4 folds correct arithmetic precision (F-V4-SYNTHESIS-1 disjoint-sets, F-V4-SYNTHESIS-2 final residual cell) — both within the existing five-criterion ceiling. |
| 11 | Cross-cutting | C-3 round-trip gate enforcement check (V4 re-execution) | ACCEPT | Per §1.1 above, the metadata-derived loop continues to enumerate 9 grammars including `css_pretty` at V4 HEAD `f8e279877`; the gate is mechanically defended end-to-end across four consecutive cycle boundaries (V1 + V2 + V3 + V4). |
| 12 | Cross-cutting | F-V4-SYNTHESIS-1 disjoint-sets arithmetic verified via two independent `git grep` invocations (V4 NEW verification) | **ACCEPT (substrate unchanged; prose arithmetic now disjoint-sets-correct)** | Per §1.2 above, the V4 prose refresh "42 ... including 14" → "56 (42 runtime + 14 codegen) disjoint sets" closes the V3 CH2 N-V3-CH2-1 inclusion-relation defect. Two independent `git grep` invocations return 42 + 14 = 56; `sort -u` returns 56 (zero overlap); path-root partition returns `{codegen, runtime}` (sibling, not nested). CH7-1 + CH7-4 substrate is **unchanged**; F-V4-SYNTHESIS-1 is **CH2-arithmetic-precision** tightening of the **CH7-narrative description**. |
| 13 | Cross-cutting | F-V4-SYNTHESIS-2 closes V3 CH7 §1.5 inheritance-miss flag (V4 NEW verification) | **ACCEPT (V3 cross-flag CLOSED)** | Per §1.3 above, the V4 fold corrects the §3.1 C-3 row count from "3 scanners are fixture lookups" to "4 scanners are fixture lookups", completing the F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3 + F-V4-SYNTHESIS-2 arithmetic-correction sweep. The V3 §1.5 cross-flag is **CLOSED** — every site that V3 §1.5 enumerated now reads "4". CH7-3 substrate unchanged; the V4 fold is **CH1-cite-arithmetic-precision** tightening of the **CH7-3 narrative description**. |
| 14 | Cross-cutting | No fake-`@generated` recurrence in V4 audit prose | ACCEPT | The single V4-touched file carries the string `@generated by skinny bbnf-codegen` only inside backticks/inline-code blocks (quoting the pattern the lint should reject); zero real `// @generated` headers in audit prose body. V3 disposition carries forward unchanged at V4. |
| 15 | Cross-cutting | No scaffold-as-load-bearing in V4 audit prose | ACCEPT | The V4 audit-prose makes no load-bearing claim resting on a SCAFFOLD-ONLY artefact. The V4 fold touches §2.4 (lint glob substrate description) and §3.1 C-3 row (CSS L4 prune-cluster count cell) only — neither touches the SCAFFOLD-bearing W14.1-5 / W13.1-4 / W15.1 rows. A5 V3 §5:133 + SYNTHESIS V3 §3.1:343 active-enforcement framing preserved at V4 STAND. |
| 16 | Cross-cutting | No gate-relabel-as-admit in V4 audit prose | ACCEPT | F1-F5 remain classified gate-relabel-only via per-commit `git show --stat` evidence (V1 + V2 + V3 dispositions carry forward at V4); the V4 fold does not stamp any "row" as ADMITTED on gate-relabel basis. The "preserved through PRUNE-5 as C-4 entry-gate invariant" V3-folded posture remains the **inverse** of gate-relabel adoption. |
| 17 | Cross-cutting | CH7 lens carried as blocking (not merely acknowledged) at V4 | ACCEPT | Per the lens definition ("CH7 cannot be carried as 'acknowledged but not blocking'"), the V4 audit's recommended actions remain revert-or-rewire (PRUNE-1, PRUNE-2 binding under C-5; PRUNE-5 binding under C-4) — not advisory acknowledgement. F-V4-SYNTHESIS-1 disjoint-sets refresh makes the lint glob substrate's reach **arithmetically explicit** (56-file twin-substrate is now narratively complete), reinforcing the blocking posture for any SK-V{N+1} wave reintroducing the fake-`@generated` header on either side. |
| 18 | Cross-cutting | Three architectural sequencing constraints preserved at V4 | ACCEPT | The three constraints (R4 → PRUNE-2; C-1 → C-4; PRUNE-4 = 9 sub-waves) carry forward at V4 with zero modification. The V4 fold surface (2 prose edits on 1 file) is below any envelope-perturbation threshold; SYNTHESIS §0.1 + §1.3 + §2.3 + §3.1 + §5.1 all preserve the constraints verbatim. |

ACCEPT-rate: **18 / 18 = 100.0 %.** Zero REVISE or REJECT
dispositions.

## §3 — Critical findings against the V4 audit's own write-up

**None.** The single V4-touched file preserves the V3 CH7-clean
posture and closes both V3 orphan REVISEs (N-V3-CH2-1 inclusion-
relation defect; CH7 §1.5 inheritance-miss flag) without introducing
any recurrence vector against the indicted patterns. Specifically:

- **No fake-`@generated` recurrence in V4 audit prose.** The V4-touched
  file carries the string only inside backticks/inline-code blocks
  (quoting the pattern the lint should reject), not in its own
  headers.
- **No scaffold-as-load-bearing claim in V4 audit prose.** The V4
  fold touches §2.4 (lint substrate prose) + §3.1 C-3 row (CSS L4
  count cell) only; neither touches the SCAFFOLD-bearing rows or
  the C-4 active-enforcement framing F-V3-A5-1 + F-V3-SYNTHESIS-1
  established.
- **No gate-relabel adoption in V4 audit prose.** A5 V3 §5:133
  active gate-rejection invariant preserved at V4 STAND; no V4 fold
  reverses any V13 gate-relabel rejection.
- **No orphan grammar dressed as load-bearing in V4 audit prose.**
  A4 V3 NEW-3 stands (14 of 15 `.bbnf` files orphaned); V4 confirms
  the count via `find /Users/mkbabb/Programming/bbnf-lang/grammar/css/l4
  -name "*.bbnf" | wc -l = 15` and the orphan-15 enumeration (V3
  re-execution carries forward at V4).

## §4 — V5 confirming-pass posture (V4 closes §3Z chain)

**Both V3 orphan REVISEs are closed at V4.** The V3 CONSOLIDATED §0
verdict was CONVERGED-EXPECTING-V4-MICRO-FOLD at 100 % aggregate
ACCEPT with 2 orphan REVISEs (N-V3-CH2-1 + CH7 §1.5 cross-flag);
both close cleanly via F-V4-SYNTHESIS-1 + F-V4-SYNTHESIS-2 prose
edits inside the single SYNTHESIS V4 agent dispatch. The V4 CH7
confirming pass surfaces zero new findings; the §3Z LOCK convergence
chain stands at **four consecutive clean cycles (V1 → V2 → V3 → V4)**.

Per the dispatch §1 mandate and the V3 CONSOLIDATED §2.3 V4 dispatch
shape ("Single SYNTHESIS V4 agent. Two atomic prose edits … V5
confirming pass closes §3Z chain at V max=5 ceiling"), the V5
confirming pass should:

1. **Re-execute the §1.1 `cargo metadata | jq` command** at the V5
   HEAD and confirm the 9-grammar enumeration including `css_pretty`
   continues to hold. This will be the **fifth consecutive
   re-attestation** of the metadata-derived loop.
2. **Re-attest the §1.2 F-V4-SYNTHESIS-1 disjoint-sets arithmetic**
   at V5 HEAD — confirm the 42 + 14 = 56 file twin-site enumeration
   still holds with zero overlap; confirm the codegen-side + runtime-
   side disjoint sets continue to bind; confirm the V4 prose refresh
   continues to match the substrate's reach.
3. **Re-attest the §1.3 F-V4-SYNTHESIS-2 four-of-seven count** at
   V5 HEAD — confirm the `grep -lE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT'`
   invocation continues to enumerate 4 files; confirm every site
   reading "4" remains in sync (zero residual "3 scanners" reads).
4. **Re-attest the §1.5 audit-pattern-emergence scan** at V5 — that
   no new audit pattern has surfaced between V4 and V5 that the
   five CH7-N criteria miss.
5. **No source-touch.** Per the V1 §3 dispatch discipline, V5
   remains write-only; the prune list at SYNTHESIS §3 binds the
   S-P3 wave manifest, not V5.

The CH7-companion gating extensions (round-trip subcommand pairing
+ LOCKS.md companion-lint with the V2-folded twin-side scope, now
narratively complete via F-V3-SYNTHESIS-2 and arithmetically explicit
via F-V4-SYNTHESIS-1) remain correctly routed as gating enhancements
inside C-3 + LOCKS.md and as attribution items for S-P3, not as V5
CH7 follow-on items.

## §5 — Disposition

The S-P0 V4 audit is CH7-clean against its own write-up. The five
CH7-N criteria continue to cover the 11 NEW finding categories (20
NEW per-row) without expansion across the V1 → V2 → V3 → V4 cycle
boundaries; F-V4-SYNTHESIS-1's "42 ... including 14" → "56 (42
runtime + 14 codegen) disjoint sets" prose refresh aligns the
narrative description with the V2-folded lint glob's already-complete
reach (`{runtime/src/grammars,codegen/src}/**/*.rs` catches all 14
codegen-side + 42 runtime-side = 56 files; `sort -u` invariance
confirms zero overlap between the two sibling roots); the substrate
is **mechanically unchanged**; the prose now correctly narrates the
substrate's full reach with arithmetic precision and disjoint-sets
correctness. F-V4-SYNTHESIS-2 closes the V3 CH7 §1.5 inheritance-miss
flag by correcting the §3.1 C-3 row "3 scanners" residual to "4
scanners", completing the F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3
arithmetic-correction sweep at its final residual cell; every site
(§0.1, §1.1:81, §1.2:122, §3.1:347, §5.1, `generator-truth.md:56`)
now reads "4" in sync. CH7-1 + CH7-3 + CH7-4 enforcement substrates
are **unchanged**; both V4 folds are **CH1/CH2-precision** tightenings
of the **CH7-narrative description**. The §1.1 C-3 round-trip gate
metadata enumeration confirms 9 grammars including `css_pretty` at
V4 HEAD `f8e279877`, matching the audit's PRUNE-4 = 9 sub-wave
binding across four consecutive cycle boundaries. **Zero new
findings** surface against the V4 artefact set; both V3 orphan
REVISEs (N-V3-CH2-1 + CH7 §1.5 cross-flag) are **CLOSED**. ACCEPT-rate
**100.0 %**; zero escalation; V4 cycle establishes the fourth clean
consecutive cycle in the §3Z two-consecutive-cycle convergence chain
(now well past the §3Z minimum; the V5 confirming pass closes the
chain at V max=5 ceiling per the V3 CONSOLIDATED §2.3 dispatch
shape).

---

**Scope:** S-P0 V4 CHALLENGE (SK-V14 Overfit Audit Pass) — CH7
Overfit-Prune (meta-applied to the V4 micro-redispatched audit
artefact at `f8e279877`).
**Authority:** `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`
+ `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/
CHALLENGE-CONTEXT.md` + `restart/skinny/tranches/sk-v14/audit-overfit/
hardening/V2/CHALLENGE-V2-ADDENDUM.md` + `restart/skinny/tranches/
sk-v14/audit-overfit/hardening/V3/HARDENING-S-P0-V3-CONSOLIDATED.md
§2.3`.
**Status:** WRITE-ONLY (untracked); aggregator commits 7 V4 lens
files + V4 CONSOLIDATED atomically.
**Next gate:** CH1-CH6 V4 + V4 aggregator complete; V5 confirming
pass closes §3Z LOCK at V max=5 ceiling → G-S-P0-CONVERGED gates
S-P1 dispatch per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.
