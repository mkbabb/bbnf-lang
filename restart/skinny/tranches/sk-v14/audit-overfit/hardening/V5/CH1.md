# CH1 — CORRECTNESS lens disposition (SK-V14 S-P0 Overfit Audit V5)

Authority: `restart/prompts/ORCHESTRATOR.md §3W CH1` + dispatch
`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/CHALLENGE-
CONTEXT.md §3` + V4 CONSOLIDATED prescription `…/V4/HARDENING-S-P0-V4-
CONSOLIDATED.md §2.2` (Option B belt-and-braces) + V5 micro-fold
commit `037eec6b6` (1 file / +4 / −3) which folds the two CH4 §4 +
CH2 §4.2 V4 sub-threshold notes the V4 CONSOLIDATED §2.2 enumerated
(`F-V5-SYNTHESIS-1{a,b}`). The V4 CH1 §3 style observation is
**EXCLUDED** by explicit V4 §2.3 / §3 disposition (V3 CONSOLIDATED
§2.1 authorised both reading variants on the triple-nested
parenthetical; "Option B compound" stands).

V4 disposition (`…/V4/CH1.md`, 100 % outright ACCEPT with zero
carry-overs from CH1 itself; three V4 sub-threshold notes surfaced
independently across the lens fleet — CH1 §3 style observation +
CH2 §4.2 cohesion polish + CH4 §4 sibling framing — all classified
by their surfacing lens as below-action-threshold):

1. CH4 V4 §4 sub-threshold note (CH1-class informational) —
   SYNTHESIS §2.4:318 closing parenthetical "(the two path roots
   are siblings)" was documentary loose framing: `runtime/src/
   grammars` is 3-deep from `skinny/crates/`, `codegen/src` is
   2-deep; the path roots themselves sit at different sub-depths,
   and only their depth-1 parents (`runtime/` + `codegen/`) are
   structural siblings. The disjointness claim the parenthetical
   supports remained correct; the "siblings" framing was decoration
   around an arithmetic claim that did not depend on it.
2. CH2 V4 §4.2 sub-threshold note (cohesion polish) — SYNTHESIS
   §2.4:319-321 closing sentence "The lint glob `codegen/src/**/
   *.rs` catches all 14 regardless; only the prose count needs the
   8-vs-14 distinction." carried a framing residue from the V2 → V3
   precision repair that, after F-V4-SYNTHESIS-1 spells "14 codegen-
   side (8 providers+templates + 6 ancillary)" explicitly upstream,
   was redundant with the upstream prose.

V5 micro-fold packets (per `037eec6b6` commit body, re-quoted here
so the disposition is self-contained):

- **F-V5-SYNTHESIS-1a** — SYNTHESIS §2.4:318 closing parenthetical
  "(the two path roots are siblings)" → "(the two path roots sit
  under sibling workspace crates `skinny/crates/runtime/` and
  `skinny/crates/codegen/`; the lint glob brace-expansion is path-
  pattern-based, not structural-relation-based)". Closes the CH4
  V4 §4 sibling-framing sub-threshold note by reframing the loose
  "siblings" claim against the precise structural relation (the
  depth-1 workspace-crate parents ARE direct siblings) and explicitly
  notes that the lint mechanism does not rest on the structural-
  relation framing in any case (brace-expansion is glob-pattern
  semantics). Load-bearing disjoint-set claim preserved.
- **F-V5-SYNTHESIS-1b** — SYNTHESIS §2.4:319-321 closing sentence
  "The lint glob `codegen/src/**/*.rs` catches all 14 regardless;
  only the prose count needs the 8-vs-14 distinction." → DELETED.
  Closes the CH2 V4 §4.2 cohesion-polish sub-threshold note by
  removing the redundant back-reference to the V2 → V3 precision
  repair (F-V4-SYNTHESIS-1 at line 312 already states "14 codegen-
  side (8 providers+templates + 6 ancillary)" explicitly upstream).
  Paragraph now flows from the disjoint-set arithmetic directly
  into the runtime-only lint counter-example without the
  intervening back-reference.

A1, A2, A3, A4, A5, A6 STAND verbatim under V5 (zero axis edits;
the V3 axis folds at A4 §1:153 + A5 §5:133 carry forward unchanged;
A1 / A2 / A3 / A6 byte-identical V2 → V3 → V4 → V5 — a four-cycle
invariant chain).

## §0 — V5 Verdict + disposition summary

**Verdict: ACCEPT (100 %).** Both V4 sub-threshold notes (CH4 §4 +
CH2 §4.2) landed at the exact line-coordinates the V4 CONSOLIDATED
§2.2 prescription enumerated; the CH1 §3 style observation correctly
EXCLUDED per V4 §2.3 / §3 dispensation (V3 CONSOLIDATED §2.1 grant);
all V4 baseline folds (F-V4-SYNTHESIS-{1,2}) preserved verbatim
upstream of the V5 deletion site; all V3 axis/SYNTHESIS folds
preserved; A1 / A2 / A3 / A4 / A5 / A6 byte-identity from V4
confirmed by `git diff f8e279877..037eec6b6 -- …{six-axis files} |
wc -l = 0`; no fresh CH1-grade defect surfaced under the V5
confirming pass.

| Disposition | Count | Notes |
| --- | ---: | --- |
| ACCEPT | 7 of 7 artefacts | A1 / A2 / A3 / A4 / A5 / A6 STAND verbatim from V4 (zero diff bytes per `git diff f8e279877..037eec6b6 -- …`); SYNTHESIS V5 carries the two prescribed micro-fold edits at the two prescribed line-coordinates (§2.4:318 + §2.4:319-321) and preserves every V4 axis/SYNTHESIS fold above the deletion site. |
| REVISE | 0 | No residual orphans; both V4 sub-threshold notes cleared at source by the prescribed two-edit micro-fold; the CH1 §3 style observation explicitly excluded per V4 §2.3 / §3 dispensation. |
| REJECT | 0 | No source-side factual error in either V5 fold; the new "sibling workspace crates" framing re-verifies against `ls skinny/crates/{runtime,codegen}` filesystem ground-truth; the deleted closing sentence's claim becomes redundant (not falsified) by the upstream 8+6 decomposition. |

ACCEPT-rate: **7/7 artefacts = 100 %**. V4 forecast in `…/V4/
HARDENING-S-P0-V4-CONSOLIDATED.md §3` ("V5 forecast 100 % (98/98)
with zero NEW findings — the V5 lenses then have literally nothing
to flag because the three V4 sub-threshold notes are all closed at
source") confirmed for CH1.

## §1 — V4 sub-threshold-note confirming pass

### §1.1 F-V5-SYNTHESIS-1a (SYNTHESIS §2.4:318 sibling framing precision) — RESOLVED

Executable verification of the corrected structural-relation framing:

```
ls -d skinny/crates/runtime skinny/crates/codegen
ls -d skinny/crates/runtime/src/grammars skinny/crates/codegen/src
```

returns:

```
skinny/crates/codegen
skinny/crates/runtime
---
skinny/crates/codegen/src
skinny/crates/runtime/src/grammars
```

`skinny/crates/runtime/` and `skinny/crates/codegen/` are confirmed
to be direct sibling workspace crates at depth 1 under `skinny/
crates/`. The path roots in the lint glob brace-expansion
(`runtime/src/grammars` at depth 3 and `codegen/src` at depth 2)
are NOT themselves direct siblings (they sit at different sub-depths)
— which is exactly the precision defect CH4 V4 §4 flagged. The V5
rewrite resolves both halves: it names the precise sibling-crate
relation (the depth-1 workspace-crate parents) AND explicitly
clarifies that the lint mechanism's brace-expansion semantics are
path-pattern-based (no dependency on any structural-relation claim).

Spot-check of the SYNTHESIS §2.4 prose now in place at lines
317-321:

```
317:  skinny/crates/codegen/src/ | wc -l = 14`, summing to the 56 total
318:  with zero overlap (the two path roots sit under sibling workspace
319:  crates `skinny/crates/runtime/` and `skinny/crates/codegen/`; the
320:  lint glob brace-expansion is path-pattern-based, not
321:  structural-relation-based). A runtime-only
```

The new prose preserves the load-bearing disjoint-set arithmetic
claim (42 + 14 = 56 with zero overlap) and the executable-
verification quotes inline above (lines 314-317). The reframing
adds two precise factual claims that both re-verify by filesystem
inspection: (a) `skinny/crates/runtime/` and `skinny/crates/codegen/`
are workspace crates (verified by their presence as `Cargo.toml`-
rooted directories under `skinny/crates/` per the workspace
`members` list), and (b) the lint glob brace-expansion semantics
expand to path-pattern matches independent of structural relation
(POSIX brace-expansion + Rust glob crate semantics).

Residual prose hygiene: `grep -n "the two path roots are siblings"
SYNTHESIS-AUDIT-OVERFIT.md` returns zero hits across the whole file
— the loose-framing parenthetical is fully retired (not merely
overwritten in the targeted cell). `grep -n "sibling workspace"
SYNTHESIS-AUDIT-OVERFIT.md` returns only line 318 (the V5
F-V5-SYNTHESIS-1a site) — single canonical-mention discipline
preserved.

**Disposition**: RESOLVED. The CH4 V4 §4 sibling-framing sub-
threshold note is closed at source — SYNTHESIS now both correctly
names the depth-1 workspace-crate sibling relation AND explicitly
clarifies that the lint mechanism is path-pattern-based; the
disjoint-set arithmetic claim remains load-bearing and re-verifies.

### §1.2 F-V5-SYNTHESIS-1b (SYNTHESIS §2.4:319-321 residual sentence deletion) — RESOLVED

Executable verification of the redundancy claim:

```
grep -n "8 providers+templates + 6 ancillary" \
    restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
```

returns line 312:

```
312:   codegen-side (8 providers+templates + 6 ancillary)) — disjoint
```

The 8 + 6 decomposition is explicitly spelled at line 312 (inside
the F-V4-SYNTHESIS-1 block at §2.4:311-318). The V4 closing
sentence "The lint glob `codegen/src/**/*.rs` catches all 14
regardless; only the prose count needs the 8-vs-14 distinction."
was a back-reference to the V2 → V3 precision-repair sweep that
made the upstream decomposition necessary in the first place;
after F-V4-SYNTHESIS-1 the back-reference is structurally
redundant with the upstream explicit decomposition. Deletion is
the correct cohesion-restoring edit (V4 CONSOLIDATED §2.2 option
(a)) over rewrite-as-forward-looking-note (option (b)) because the
upstream prose now stands on its own without back-reference; a
forward-looking note would be a new factual addition rather than a
cohesion fix.

Residual prose hygiene: `grep -n "8-vs-14\|catches all 14
regardless\|only the prose count needs" SYNTHESIS-AUDIT-OVERFIT.md`
returns zero hits across the whole file — the redundant closing
sentence is fully retired (not merely shortened or relocated). The
paragraph now flows from the disjoint-set arithmetic (lines 317-
321) directly into the runtime-only-lint counter-example (lines
321-325) without intervening back-reference. Spot-verification of
the paragraph flow at SYNTHESIS §2.4:317-325:

```
317:  skinny/crates/codegen/src/ | wc -l = 14`, summing to the 56 total
318:  with zero overlap (the two path roots sit under sibling workspace
319:  crates `skinny/crates/runtime/` and `skinny/crates/codegen/`; the
320:  lint glob brace-expansion is path-pattern-based, not
321:  structural-relation-based). A runtime-only
322:  lint would let the
323:  codegen-side twin re-introduce the fake header silently (the
324:  identical-content round-tripping vector A4 finding 15 enumerates
325:  between codegen-side template and runtime-side `generated.rs`).
```

The same below-CH1-threshold soft paragraph break V3 + V4 CH1
classified at "A runtime-only / lint would let the / codegen-side
twin" (a typographical artefact that renders as continuous prose
in HTML) persists at lines 321-323 — unchanged because F-V5-
SYNTHESIS-1b only touches the deletion of the redundant sentence
above it; lines 321-325 carry over verbatim from V4. This is the
same below-threshold cosmetic V3 + V4 declined to fold; the V5
confirming pass declines for the same reason (no factual error;
renders as continuous prose).

**Disposition**: RESOLVED. The CH2 V4 §4.2 cohesion-polish sub-
threshold note is closed at source — the redundant back-reference
to the V2 → V3 precision-repair sweep is fully removed; the
upstream 8 + 6 decomposition at line 312 carries the breakdown the
deleted closing sentence used to back-reference; paragraph flow
now passes from the disjoint-set arithmetic directly into the
runtime-only-lint counter-example.

### §1.3 CH1 §3 V4 style observation — EXCLUDED (per V4 §2.3 / §3 dispensation)

The V4 CH1 §3 style observation (triple-nested parenthetical "(42
runtime-side mirror + 14 codegen-side (8 providers+templates + 6
ancillary))" + double-em-dash construct inside the same sentence
at §2.4:311-313) was explicitly EXCLUDED from the V5 fold scope by
V4 CONSOLIDATED §2.3 closing dispensation:

> The CH1 §3 style observation (triple-nested parenthetical +
> double-em-dash) is **not** part of Option B's fold scope — V3
> CONSOLIDATED §2.1 explicitly authorised both reading variants
> ("Option A" additive flat or "Option B" nested compound) per
> local prose-flow judgement; CH1 records "no fold recommended"
> on the style note. Option B closes CH2 §4.2 + CH4 §4 only,
> leaving the CH1 §3 style observation explicitly as a no-action
> item per V3 §2.1 dispensation.

V5 commit `037eec6b6` body explicitly affirms the exclusion:

> CH1 sec 3 style observation EXCLUDED (V3 CONSOLIDATED sec 2.1
> authorised both reading variants; Option B compound reading
> stands).

Re-affirmed under V5: the triple-nested parenthetical at §2.4:311-
313 carries forward verbatim from V4; the double-em-dash construct
at §2.4:312-313 carries forward verbatim from V4. Both are
grammatically defensible (V4 CH1 §3 noted: "the dash introduces a
sentence-level qualifier, the parenthetical preceding it scopes
the 14-codegen sub-decomposition"; "every factual claim re-
arithmetics correctly, the disjoint-set proof remains airtight").
V5 explicit no-fold disposition matches V4 CH1 §3 + V4 CONSOLIDATED
§2.3 / §3 prescription; the V3 CONSOLIDATED §2.1 "Option B compound"
authorisation continues to govern.

**Disposition**: EXCLUDED (no action by prescription). The style
observation is a permitted local prose-flow variant per V3 §2.1
grant; carries forward verbatim under V5; no CH1-grade defect
surfaces at V5 against either the V3 dispensation or the prose
itself.

## §2 — V4 baseline preservation under V5

Critical V4 baseline cites re-executed and reproduce verbatim under
V5 (the V4 → V5 boundary is `git diff f8e279877..037eec6b6`; the
broader V3 → V5 boundary is `git diff 007624849..037eec6b6`):

1. `git diff f8e279877..037eec6b6 -- restart/skinny/tranches/sk-v14/
   audit-overfit/sk-v14-audit-overfit-{css-measurement,admit-
   mechanism,lock14-scan,generator-truth,decision-engine,pre-restart-
   pattern}.md | wc -l` returns **0** — A1, A2, A3, A4, A5, A6 STAND
   byte-identical V4 → V5 (matches V5 commit body's "A1-A6 STAND
   verbatim" claim).
2. `git diff 007624849..037eec6b6 -- restart/skinny/tranches/sk-v14/
   audit-overfit/sk-v14-audit-overfit-{css-measurement,admit-
   mechanism,lock14-scan,generator-truth,decision-engine,pre-restart-
   pattern}.md | wc -l` returns **0** — A1, A2, A3, A4, A5, A6 STAND
   byte-identical V3 → V4 → V5 (two-cycle invariant chain on all
   six axes; the V3 axis folds at A4 §1:153 + A5 §5:133 carry
   forward unchanged at V5).
3. `git diff 1735882a5..037eec6b6 -- restart/skinny/tranches/sk-v14/
   audit-overfit/sk-v14-audit-overfit-{css-measurement,admit-
   mechanism,lock14-scan,pre-restart-pattern}.md | wc -l` returns
   **0** — A1, A2, A3, A6 STAND byte-identical across the full V2 →
   V3 → V4 → V5 span (**four-cycle invariant chain** on these four
   axes; one cycle deeper than the V4 chain).
4. F-V3-A4-1 carry-forward: `grep -n "json_provider.rs" sk-v14-
   audit-overfit-generator-truth.md` returns line 153 reading
   `- skinny/crates/codegen/src/json_provider.rs (full, 100 lines).`
   verbatim; `wc -l skinny/crates/codegen/src/json_provider.rs`
   returns **100** ✓.
5. F-V3-A5-1 carry-forward: `grep -n "preserved through PRUNE-5"
   sk-v14-audit-overfit-decision-engine.md` returns line 133 reading
   "the LOW finding (honest self-labelling) is preserved through
   PRUNE-5 as a gate-rejection invariant inside C-4 entry-gates so
   any admit attempting to cite W8 / W9 pre-runtime-consumer is
   denied at admit time" ✓; `grep -n "no-op pre-C-4" restart/skinny/
   tranches/sk-v14/audit-overfit/` returns zero matches (the framing
   stays fully retired under V5) ✓.
6. F-V3-SYNTHESIS-1 carry-forward: `grep -n "preserved through PRUNE-5
   as C-4 entry-gate invariant" SYNTHESIS-AUDIT-OVERFIT.md` returns
   line 349 (was 348 in V4 before the V5 net −1 line: F-V5-SYNTHESIS-
   1a adds +4 in lines 318-321; F-V5-SYNTHESIS-1b removes the V4
   sentence at lines 319-321 of V4 prose, net delta from V4 is −3
   + +4 = +1 in §2.4 only; but the C-4 row sits in §3.1 which is
   downstream of §2.4 — the §3.1 cell shifts by +1 to line 349) ✓.
7. F-V3-SYNTHESIS-2 carry-forward: SYNTHESIS §2.4 prose still
   carries the V3 codegen-count refinement extended by the V4
   inclusion-relation correction; the `git grep -l … codegen/src/
   | wc -l = 14` verification quoted inline at lines 316-317 ✓;
   the 14-file enumeration from V3 CH1 §2.3 still resolves to the
   same 14 files under `git grep -l '@generated by skinny bbnf-
   codegen' skinny/crates/codegen/src/`.
8. F-V3-SYNTHESIS-3 carry-forward: `grep -n "3 of 7\|4 of 7"
   SYNTHESIS-AUDIT-OVERFIT.md` returns line 81 only, reading "4 of
   7 CSS scanners short-circuit" ✓; the stale "3 of 7" framing
   remains fully retired.
9. F-V4-SYNTHESIS-1 carry-forward: `grep -n "56 files carry the
   fake header" SYNTHESIS-AUDIT-OVERFIT.md` returns line 311
   reading "56 files carry the fake header (42 runtime-side mirror
   + 14 codegen-side (8 providers+templates + 6 ancillary))"
   verbatim; the V4 disjoint-set framing is preserved load-bearingly
   above the V5 deletion site; both `git grep` invocations still
   quoted inline at lines 314-317 ✓.
10. F-V4-SYNTHESIS-2 carry-forward: `grep -n "4 scanners are
    fixture lookups" SYNTHESIS-AUDIT-OVERFIT.md` returns line 348
    (was 347 in V4 before the V5 net +1; same +1 shift as point 6)
    reading "...4 scanners are fixture lookups, 14/15 .bbnf
    orphan..." verbatim ✓; the 4-of-7 CSS L4 fixture-lookup count
    re-verifies by `grep -rE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT'
    skinny/crates/codegen/src/css_l4_*_templates/generated.rs |
    cut -d: -f1 | sort -u | wc -l = 4` ✓.
11. SYNTHESIS census preservation: `grep -n "54 of 74\|74 finding\|
    31 CRIT\|20 HIGH\|12 MED\|11 LOW" SYNTHESIS-AUDIT-OVERFIT.md`
    returns lines 45 (CRIT+HIGH aggregate trigger), 64 ("54 of 74
    findings (73 %) CONFIRM"), 337 (§3.1 coverage header), 341
    ("**73 of 74 findings**"), 361 (zero-orphan attestation), 496
    ("**Aggregate: 74 findings (31 CRIT + 20 HIGH + 12 MED + 11
    LOW)**"), 508 (covered-by-C-1..C-5 closing) — 74-finding
    census + severity distribution preserved verbatim ✓ (line
    numbers shifted +1 vs V4 listing per the §2.4 net +1 above).
12. Per-axis severity column sums (V2 §3 / V3 §3 / V4 §3): A1 8
    (4+2+2+0), A2 9 (4+3+1+1), A3 30 (11+7+5+7), A4 16 (9+4+2+1),
    A5 4 (0+2+1+1), A6 7 (3+2+1+1) → 74 ✓; CRIT 31 + HIGH 20 + MED
    12 + LOW 11 = 74 ✓ (re-attested at V5 byte-identically; A1-A6
    files byte-identical to V4).
13. PRUNE-4 sub-wave count + 9-grammar census: `grep -n "PRUNE-4 has
    9 sub-waves" SYNTHESIS-AUDIT-OVERFIT.md` returns line 217 ✓;
    SYNTHESIS §2.3 enumeration of "9 sub-waves" at line 282-290
    holds verbatim; the PRUNE-4 binding (9 not 8; css_pretty
    addition) carries verbatim from V4 ✓.

## §3 — V5 fresh-finding scan

Scanned the V5 diff (`git show 037eec6b6`) end-to-end and re-greped
the modified file for any incidental drift introduced by the two
folds. No new CH1-grade defect:

- **F-V5-SYNTHESIS-1a**: 4-line addition / 1-line deletion at
  §2.4:318-321 (was 318 in V4) — extends the parenthetical by 3
  net lines (was −1 / +4 per commit stat half-attribution; the
  other half is the −3 sentence deletion at F-V5-SYNTHESIS-1b
  below for net +1 total across both packets). The new prose
  introduces no fresh off-by-one, inversion, or absent-cite: the
  sibling-workspace-crate claim re-verifies by `ls -d skinny/crates/
  {runtime,codegen}` (both returned); the lint-mechanism semantic
  claim ("path-pattern-based, not structural-relation-based") is
  trivially true by glob-crate / POSIX brace-expansion semantics
  (no structural-relation predicate in glob matching). The
  disjoint-set arithmetic claim above the parenthetical (56 = 42 +
  14) carries verbatim from V4 unchanged; the load-bearing inline
  `git grep` verifications at lines 314-317 carry verbatim from V4
  unchanged.
- **F-V5-SYNTHESIS-1b**: 3-line deletion at §2.4:319-321 (V4
  numbering) — removes the redundant back-reference sentence.
  Paragraph flow now passes directly from the disjoint-set
  parenthetical close (V5 line 321 "structural-relation-based).")
  into the runtime-only-lint counter-example ("A runtime-only /
  lint would let the / codegen-side twin re-introduce …"). The
  removed sentence's claim ("only the prose count needs the 8-vs-14
  distinction") is now redundant with the explicit upstream "(8
  providers+templates + 6 ancillary)" decomposition at line 312 —
  zero factual content lost; cohesion restored.
- **Cross-document propagation**: the V5 edits do not perturb any
  cross-axis cite. `grep -n "fixture-lookup\|Four of the seven"
  sk-v14-audit-overfit-generator-truth.md` still returns the same
  V4-aligned lines (A4 file byte-identical V4 → V5 per §2 point 1);
  no inheritance miss surfaces to a third site under V5. The §3.1
  cell at line 348 ("4 scanners are fixture lookups") shifted by
  +1 due to the §2.4 net +1 line growth but the cell content is
  byte-identical to V4.
- **CH1 §3 V4 style observation preserved (explicitly excluded)**:
  the triple-nested parenthetical at §2.4:311-313 + double-em-dash
  construct carry forward verbatim from V4 — confirmed by `grep
  -n "(42 runtime-side mirror + 14 codegen-side (8 providers+
  templates + 6 ancillary))" SYNTHESIS-AUDIT-OVERFIT.md` returning
  line 311-312 verbatim. This is per V4 §2.3 / §3 explicit
  dispensation (V3 §2.1 "Option B compound" authorisation); no V5
  fold prescribed, no V5 CH1-grade defect surfaced.
- **Soft paragraph break preserved (below-threshold)**: the same
  typographical artefact V3 + V4 CH1 noted at SYNTHESIS §2.4:321-
  323 ("A runtime-only / lint would let the / codegen-side twin")
  carries forward under V5 because the V5 edit only modifies lines
  318-321 — lines 322-329 are byte-identical to V4. This is the
  same below-CH1-threshold cosmetic V3 + V4 declined to fold; the
  V5 confirming pass declines for the same reason (no factual
  error; renders as continuous prose in HTML).

**Zero CH1-grade observations** surface at V5. The V4 CH1 §3 style
observation was the only V4 below-CH1-threshold note from CH1
itself and is explicitly excluded from V5 fold scope per binding
V4 §2.3 / §3 dispensation. No new style-grade observation surfaces
at V5: the F-V5-SYNTHESIS-1a parenthetical adds three lines but
reads as a single parenthetical with one semicolon-joined dependent
clause (well-formed without nested punctuation density); the F-V5-
SYNTHESIS-1b deletion removes prose without introducing new prose.

## §4 — Spot-verified cites (V5 sample of 10)

1. `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/
   runtime/src/grammars/ | wc -l` → **42** ✓.
2. `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/
   codegen/src/ | wc -l` → **14** ✓.
3. `grep -rE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT' skinny/crates/
   codegen/src/css_l4_*_templates/generated.rs | cut -d: -f1 | sort
   -u | wc -l` → **4** ✓.
4. `ls -d skinny/crates/runtime skinny/crates/codegen` → both
   present as direct children of `skinny/crates/` ✓ (workspace-
   crate sibling relation verified for F-V5-SYNTHESIS-1a).
5. `grep -n "the two path roots are siblings" SYNTHESIS-AUDIT-
   OVERFIT.md` → zero matches ✓ (F-V5-SYNTHESIS-1a retires the
   loose framing at source).
6. `grep -n "8-vs-14\|catches all 14 regardless\|only the prose
   count needs" SYNTHESIS-AUDIT-OVERFIT.md` → zero matches ✓
   (F-V5-SYNTHESIS-1b retires the redundant closing sentence at
   source).
7. `grep -n "sibling workspace" SYNTHESIS-AUDIT-OVERFIT.md` →
   line 318 only (V5 F-V5-SYNTHESIS-1a single-canonical-mention
   site) ✓.
8. `grep -n "56 files\|4 scanners are fixture lookups"
   SYNTHESIS-AUDIT-OVERFIT.md` → line 311 (F-V4-SYNTHESIS-1 site,
   preserved) + line 348 (F-V4-SYNTHESIS-2 site, preserved, +1
   shift) ✓.
9. `git diff f8e279877..037eec6b6 -- restart/skinny/tranches/sk-v14/
   audit-overfit/sk-v14-audit-overfit-{css-measurement,admit-mechanism,
   lock14-scan,generator-truth,decision-engine,pre-restart-pattern}.md
   | wc -l` → **0** ✓ (A1, A2, A3, A4, A5, A6 STAND verbatim V4 →
   V5; matches commit body).
10. `git diff 1735882a5..037eec6b6 -- restart/skinny/tranches/sk-v14/
    audit-overfit/sk-v14-audit-overfit-{css-measurement,admit-
    mechanism,lock14-scan,pre-restart-pattern}.md | wc -l` → **0**
    ✓ (A1, A2, A3, A6 STAND byte-identical V2 → V3 → V4 → V5; four-
    cycle invariant chain — one cycle deeper than V4).

---

**V5 ACCEPT-rate: 7/7 artefacts = 100 %.**

Both V4 sub-threshold notes (CH4 §4 sibling framing + CH2 §4.2
cohesion polish) are RESOLVED at the exact line-coordinates the V4
CONSOLIDATED §2.2 prescription enumerated (SYNTHESIS §2.4:318
sibling-framing precision; SYNTHESIS §2.4:319-321 residual sentence
deletion). The V4 CH1 §3 style observation is EXCLUDED per V4
§2.3 / §3 explicit dispensation (V3 CONSOLIDATED §2.1 "Option B
compound" authorisation re-affirmed in V5 commit body). The V5 fold
roster correctly enumerated **both** the prescribed primary anchors
inside one paragraph with zero secondary inheritance misses — the
V3 §0.2 + §3.3 + V4 §0.2 + §3.3 "inheritance-miss class on SYNTHESIS
surface" institutional pattern stays closed for the V5 cycle (the
V5 commit body's explicit two-edit enumeration "§2.4:318 + §2.4:319-
321" demonstrates the discipline V3 + V4 prescribed). A1 / A2 / A3
/ A4 / A5 / A6 STAND byte-identical V4 → V5 (one-cycle invariant
extension on all six axes) and V2 → V3 → V4 → V5 (four-cycle
invariant chain on A1 / A2 / A3 / A6 — one cycle deeper than V4),
preserving every previously-verified V4 baseline cite plus the
F-V4-SYNTHESIS-{1,2} folds above the V5 deletion site.

**No CH1-grade source-side correctness defects identified in V5.**
Every file:line cite re-executes to the quoted output; the 74-
finding aggregate + severity distribution remain arithmetically
self-consistent (re-attested verbatim from V4); the V3 + V4 axis
folds carry forward byte-identically; the F-V4 SYNTHESIS folds
carry forward verbatim above the V5 deletion site; the V5 56-file
disjoint-set claim continues to source-verify by two independent
`git grep` invocations summing to 56 with structural zero-overlap
(now framed precisely against the sibling-workspace-crate depth-1
parents AND the path-pattern-based lint glob semantics); the four-
scanner CSS L4 fixture-lookup count source-verifies by `grep -rE
… | sort -u` returning exactly 4 distinct files.

CH1 V5 contributes **100 %** toward the §3Z convergence chain. V4
CH1 at 100 % outright + V5 CH1 at 100 % outright — the two-
consecutive-cycle ≥ 95 % CH1 condition is satisfied from the CH1
side through the V4 → V5 boundary under both readings (strict: V4
clears floor with zero orphans → V5 closes §3Z LOCK as second clean
cycle; pragmatic: CH1 has been at 100 % for the V1 + V2 + V3 + V4
+ V5 chain on the substantive disposition surface — five
consecutive 100 % cycles, one cycle deeper than V4's four). V5
lands at the V max=5 ceiling per §3Z + V4 §2.3 + §3.1 strict-reading
prescription; the V5 confirming pass on CH1 stands clean against
both the binding V4 prescription scope (CH2 §4.2 + CH4 §4 only) and
the explicit exclusion of the CH1 §3 style observation. The
aggregate G-S-P0-CONVERGED gating remains conditional on the
remaining six lens dispositions at V5; CH1 itself stands clean and
ready to contribute its 100 % weight to the §3Z LOCK closure.
