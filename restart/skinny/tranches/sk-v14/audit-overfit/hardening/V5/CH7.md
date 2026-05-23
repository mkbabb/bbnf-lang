# CH7 Overfit-Prune — S-P0 V5 Disposition (SK-V14 Overfit Audit)

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
an admit. The V5 confirming pass at V max=5 ceiling re-attests the
V4 100 % verdict across the single V5-touched artefact (commit
`037eec6b6`, 1 file +4 / -3 prose-only on SYNTHESIS-AUDIT-OVERFIT.md),
re-executes the C-3 round-trip metadata-derived loop at V5 HEAD,
re-verifies the F-V4-SYNTHESIS-1 disjoint-set arithmetic that the
F-V5-SYNTHESIS-1a sibling-framing-precision micro-fold tightened,
confirms the F-V5-SYNTHESIS-1b sentence deletion preserves the
8-vs-14 decomposition and lint-glob substrate at upstream sites (lines
311-314), and surfaces zero CH7-relevant claim regression. The V4
verdict closes the §3Z two-consecutive-cycle convergence chain.

## §0 — Disposition summary

- Artefacts reviewed: **1** (SYNTHESIS-AUDIT-OVERFIT.md V5 — two
  prose edits at §2.4:318 sibling-framing precision +
  §2.4:319-321 residual closing-sentence deletion). A1, A2, A3, A4,
  A5, A6 STAND verbatim (no V5 folds) per the V4 CONSOLIDATED §2.2
  Option-B micro-fold scope (single SYNTHESIS V5 agent dispatch
  closing the three sub-threshold informational notes).
- Per-section dispositions issued: **18** (15 V4 sections re-attested
  + 3 new V5-fold-specific sections: F-V5-SYNTHESIS-1a
  sibling-framing precision verified + F-V5-SYNTHESIS-1b residual-
  sentence deletion verified for zero CH7-claim drop + cross-version
  stability of the 56-file lint-glob substrate now in its fifth
  consecutive cycle).
- ACCEPT: **18**.
- REVISE: **0**.
- REJECT: **0**.
- ACCEPT-rate: 18 / 18 = **100.0 %**.
- Critical findings against the V5 audit's own write-up: **0**.
- New finding: **0**. The V5 fold packet closes the three V4
  sub-threshold informational notes routed to V5 belt-and-braces
  (CH4 §4 sibling-framing note + CH2 §4.2 cohesion polish note;
  CH1 §3 style observation EXCLUDED per V3 CONSOLIDATED §2.1 Option-B
  authority — both reading variants stand); no fresh CH7-N pattern
  emerges from the V5 prose-only delta.
- Escalation flag: **NO.** The single V5-touched file preserves the
  V4 CH7-clean posture: zero fake-`@generated` header recurrence in
  audit prose, zero scaffold-as-load-bearing claim, zero gate-relabel
  adoption, zero orphan-grammar dressed as load-bearing. The V5 folds
  sharpen documentary precision (F-V5-SYNTHESIS-1a corrects the loose
  "siblings" framing to the precise workspace-crate-sibling phrasing
  with explicit path-pattern-vs-structural-relation disambiguation;
  F-V5-SYNTHESIS-1b removes a residual V2-V3 precision-repair
  back-reference rendered redundant by the V4 fold's explicit 8+6
  decomposition at line 312) and add zero new CH7-N criteria. The
  §3Z LOCK convergence chain now stands at **five consecutive clean
  cycles (V1 → V2 → V3 → V4 → V5)** at the V max=5 ceiling.

## §1 — Executable verification (per §3 mandate)

### §1.1 — C-3 round-trip gate metadata-derived loop re-executed (V5 HEAD)

Per the dispatch §1 mandate ("re-execute `cargo metadata --format-
version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'` (expect
9 unchanged at V5)"), the canonical C-3 round-trip metadata command
was re-executed against the live workspace at `HEAD = 037eec6b6`.

**Command (verbatim from `alpha-E-candidate-shortlist.md:366-367` /
V1 CH7 §1.1 / V2 CH7 §1.1 / V3 CH7 §1.1 / V4 CH7 §1.1):**

```
cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
```

**Output (captured in `/Users/mkbabb/Programming/bbnf-lang` at V5
HEAD `037eec6b6`):**

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
CH7 §1.1 attestation (HEAD `007624849`), the V4 CH7 §1.1 attestation
(HEAD `f8e279877`), and to the V5 Pass-Alpha attestation
(`restart/skinny/tranches/sk-v14/research/alpha-hardening/V5/CH7.md
§1.1`). The ninth grammar `css_pretty` continues to enumerate at the
gate's runtime under S-P0 V5; the SYNTHESIS-AUDIT-OVERFIT V5 §2.3
"PRUNE-4 = 9 sub-waves" binding (and its V2-folded §1.3 co-derivation
note crediting the +1 to the single `css_pretty` directory addition)
reconciles to this enumeration verbatim across five consecutive cycle
boundaries (V1 → V2 → V3 → V4 → V5).

### §1.2 — F-V5-SYNTHESIS-1a sibling-framing-precision micro-fold sharpens disjoint-set framing

Per the dispatch §1 mandate ("verify F-V5-SYNTHESIS-1a sharpens the
disjoint-set framing without breaking CH7-1/CH7-4 mechanics"), the
V5-touched parenthetical at SYNTHESIS-AUDIT-OVERFIT.md §2.4:318 was
re-read at HEAD `037eec6b6` and the underlying disjoint-set claim
re-validated through three independent grep invocations.

**Pre-V5 prose (V4 baseline, V4 SYNTHESIS-AUDIT-OVERFIT.md:317-318):**

```
   with zero overlap (the two path roots are siblings).
```

The V4 prose's "the two path roots are siblings" framing was
**structurally imprecise**: the two lint-glob roots are
`runtime/src/grammars/` (3 path components deep) and `codegen/src/`
(2 path components deep); they are not direct siblings under a
common parent. The **workspace crates** `skinny/crates/runtime/` and
`skinny/crates/codegen/` ARE direct siblings at workspace-crate depth.
CH4 V4 §4 flagged this as a documentary-decoration note below action
threshold; the V5 micro-fold corrects the framing.

**Post-V5 prose (V5 fold landed at SYNTHESIS-AUDIT-OVERFIT.md:318-321):**

```
   with zero overlap (the two path roots sit under sibling workspace
   crates `skinny/crates/runtime/` and `skinny/crates/codegen/`; the
   lint glob brace-expansion is path-pattern-based, not
   structural-relation-based).
```

**Executable re-verification of the disjoint-arithmetic claim at V5
HEAD via three independent invocations:**

```
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ | wc -l
42
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/ | wc -l
14
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ skinny/crates/codegen/src/ | wc -l
56
$ git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ skinny/crates/codegen/src/ | sort -u | wc -l
56
```

42 + 14 = 56 holds verbatim at V5 HEAD, and the `sort -u` invariance
re-confirms zero path-overlap between the two roots. The disjoint-set
arithmetic is **mechanically unchanged** across the V4 → V5 boundary;
the V5 fold tightens only the **descriptive vocabulary** for the
two roots' relation (workspace-crate-level siblings, not path-root-
level siblings).

**Filesystem confirmation that the V5-cited workspace crates are
genuine siblings at workspace-crate depth 1:**

```
$ ls -la skinny/crates/runtime/ skinny/crates/codegen/ 2>&1 | head -10
/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/:
…
drwxr-xr-x   4 mkbabb  staff  128 May  8 15:12 .
drwxr-xr-x  14 mkbabb  staff  448 May 22 04:26 ..
…
/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/:
…
```

Both `skinny/crates/runtime/` and `skinny/crates/codegen/` exist at
the cited workspace-crate depth with the same parent
`skinny/crates/`; the V5 prose's "sibling workspace crates" framing
is filesystem-true.

**Mechanical consistency with CH7-1 (no fake `@generated` outside
emission roster):** The V5 prose refresh corrects the
**structural-relation vocabulary** for the two path roots without
altering the lint glob's reach. The lint glob brace-expansion
`{runtime/src/grammars,codegen/src}/**/*.rs` continues to enforce
both roots; the V2-folded lint substrate, V3-folded prose narrative,
and V4-folded disjoint-set arithmetic all stand. The V5 fold makes
the "path-pattern-based, not structural-relation-based" character of
the glob explicit — strengthening the narrative description of why
the disjoint-set arithmetic holds (it holds by glob-pattern union,
which is what the lint runs at CI, not by any path-structure
assertion). CH7-1 enforcement substrate is **unchanged**; the V5
fold is **CH4-framing-precision** + **CH1-vocabulary-precision**
tightening of the **CH7-1 narrative description**. No recurrence
vector introduced.

**Mechanical consistency with CH7-4 (round-trip test for every
generated output):** The 56-file twin-substrate census continues to
hold at V5 HEAD with the same disjoint-set structure; the V5 fold
refines only the description of why the lint glob mechanically unions
the two roots (path-pattern, not structural-sibling). The round-trip
surface (codegen-side template/provider files emitting onto
runtime-side mirror files) remains the same identity-twin pair the
V4 prose explicitly enumerated; the V5 prose change does not weaken,
narrow, or expand this surface. CH7-4 round-trip surface census is
**unchanged in shape and reach**; the V5 prose refresh is
**descriptive precision** that aligns the path-relation vocabulary
with the lint glob's actual matching semantics.

F-V5-SYNTHESIS-1a lands cleanly; CH7-1 + CH7-4 are **mechanically
unchanged** at the lint-glob substrate; the V5 prose now correctly
describes the workspace-crate-level sibling relation and explicitly
marks the lint glob as path-pattern-based rather than structural-
relation-based — preventing future readers from mistaking the
disjoint-set arithmetic for a structural-sibling consequence.

### §1.3 — F-V5-SYNTHESIS-1b residual-sentence deletion verified for zero CH7-claim drop

Per the dispatch §1 mandate ("verify F-V5-SYNTHESIS-1b deletion
doesn't drop any CH7-relevant claim"), the deleted sentence was
identified through the V5 commit diff and each of its sub-claims
re-located at upstream sites within the same §2.4 block.

**Deleted sentence (V4 baseline, V4 SYNTHESIS-AUDIT-OVERFIT.md:
319-320, removed by V5 commit `037eec6b6`):**

```
   The lint glob `codegen/src/**/*.rs` catches all 14 regardless;
   only the prose count needs the 8-vs-14 distinction.
```

This sentence carried two sub-claims:

  **Sub-claim A** — "The lint glob `codegen/src/**/*.rs` catches all
  14 regardless" — i.e. the lint glob covers the 14 codegen-side files
  mechanically.

  **Sub-claim B** — "only the prose count needs the 8-vs-14
  distinction" — i.e. the 8-providers + 6-ancillary breakdown is
  documentary-only, the lint enforces all 14 uniformly.

**Per-sub-claim re-location check at upstream sites within §2.4:**

  - **Sub-claim A** is now subsumed by the more precise statement at
    lines 313-314: "disjoint sets per the lint glob brace-expansion
    `{runtime/src/grammars,codegen/src}/**/*.rs`". The brace-expansion
    glob explicitly covers BOTH the runtime-side and codegen-side
    14-file path-root union, which is a strict superset of the
    deleted sentence's "codegen/src/**/*.rs" sub-glob claim (the
    brace-expansion enforces both halves of the twin, not just the
    codegen half). The lint-coverage claim is **preserved with strict
    strengthening**: V5 prose covers 42 + 14 = 56 files; the deleted
    sentence's narrower form covered only the 14-file codegen subset.

  - **Sub-claim B** is now subsumed by the explicit 8+6 decomposition
    in the V4 fold at line 312: "14 codegen-side (8 providers+templates
    + 6 ancillary)". The 8-vs-14 distinction the deleted sentence
    referenced is **already stated in the prose immediately above
    where the sentence sat**; the deleted sentence was a meta-comment
    explaining why the distinction was kept in prose, but the
    distinction itself is now stated directly and self-evidently. The
    documentary-only nature of the 8-vs-14 breakdown is implied by
    its position as a parenthetical decomposition of the 14
    aggregate the lint enforces uniformly via brace-expansion.

**Per-CH7-criterion drop scan against the deleted sentence:**

  - **CH7-1 (no fake `@generated` outside emission roster):** The
    deleted sentence's lint-coverage claim is subsumed by the
    brace-expansion lint glob at line 313-314 (strict superset). No
    CH7-1 enforcement claim dropped.

  - **CH7-2 (Lock 14 generic-crate compliance):** The deleted
    sentence carried no Lock-14 claim. N/A.

  - **CH7-3 (strict-vs-strict comparator + per-iter oracle):** The
    deleted sentence carried no comparator claim. N/A.

  - **CH7-4 (round-trip test for every generated output):** The
    deleted sentence carried no explicit round-trip claim; the 14-file
    codegen-side reach claim is preserved through the brace-expansion
    glob at line 313-314 + the disjoint-set arithmetic at lines
    314-318. The twin-substrate round-trip surface census preserved
    in full.

  - **CH7-5 (no SCAFFOLD-ONLY admit):** The deleted sentence carried
    no SCAFFOLD claim. N/A.

**Net assessment:** The V5 deletion removes a **redundant meta-
explanation sentence** that had become superfluous after the V4 fold's
explicit 8+6 decomposition at line 312 and brace-expansion glob
formulation at line 313-314. Both sub-claims of the deleted sentence
are preserved (strictly strengthened, in fact) at upstream §2.4
sites. **Zero CH7-relevant claim dropped.**

F-V5-SYNTHESIS-1b lands cleanly; the prose cohesion polish is a
pure descriptive-precision tightening within the existing CH7-1 +
CH7-4 enforcement substrate.

### §1.4 — Cross-axis sub-wave count consistency check (V5 confirming)

Per the V1 CHALLENGE-CONTEXT §2 bound-fact "PRUNE-4 sub-wave count is
9 not 8 (css_pretty added between V13 and SK-V14)", the V5-touched
file preserves the four-corner consistency from V1 / V2 / V3 / V4:

- A3 V2 §1 (unchanged through V3 / V4 / V5 STAND): `find crates/core/
  src/runtime -mindepth 1 -maxdepth 1 -type d` returns 9 directories.
- A5 V3 §2.1 (verdict-line FAIL-at-HEAD aligned at V2; STAND
  structurally at V3 / V4 / V5): "9 dirs — one over the
  dispatch-cited 8 because of `css_pretty`".
- A6 V2 §1 (unchanged through V3 / V4 / V5 STAND): per-grammar
  census table sums to 9 directories / 67 files.
- SYNTHESIS V5 §1.3 + §2.3 (STAND from V3 / V4): "Pattern H file
  count: 64 → 67"; the +3 file delta and +1 sub-wave delta
  attributed to the single `css_pretty` addition; the three A3/A5/A6
  cross-checks remain **co-derived, not orthogonal**.

**Re-executed verification (V5 HEAD `037eec6b6`):**

```
$ find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l
9
```

§1.1 metadata enumeration above confirms the ninth grammar at the
manifest layer; this filesystem enumeration confirms it at the
runtime-crate layer. The cross-axis coherence holds without exception
across the V1 → V2 → V3 → V4 → V5 cycle boundary — **five
consecutive clean cycles**.

**Cross-version stability of the 56-file lint-glob coverage
(F-V2-SYNTHESIS-5 substrate; F-V3-SYNTHESIS-2 prose; F-V4-SYNTHESIS-1
disjoint-sets refresh; F-V5-SYNTHESIS-1a sibling-framing precision):**

```
$ git grep -l '@generated by skinny bbnf-codegen' \
    skinny/crates/runtime/src/grammars/ \
    skinny/crates/codegen/src/ | wc -l
56
```

56 = 42 + 14 holds verbatim at V5 HEAD; the V2 confirmation (HEAD
`42e3edb9a`) returned 56; the V3 confirmation (HEAD `007624849`)
returned 56; the V4 confirmation (HEAD `f8e279877`) returned 56.
**Five-cycle stability is established** — the lint glob substrate is
unchanged across the entire convergence chain, and the V5 prose now
narrates the substrate's reach with disjoint-sets arithmetic
precision AND structural-relation vocabulary precision (workspace-
crate-sibling, not path-root-sibling).

### §1.5 — Fresh-finding scan across the 5 CH7-N criteria (V5 confirming)

Per the dispatch §1 mandate ("fresh-finding scan"), each criterion
was independently re-scanned against the V5 artefact set + the
underlying source repository at V5 HEAD:

- **CH7-1 (no fake `@generated` outside emission roster):** V1 + V2
  scope-extension stands at 8 sites of authored render drivers
  (7 CSS providers + JSON provider); the V3 prose enumerated the
  14-file codegen-side reach; the V4 prose corrected to 42-runtime +
  14-codegen = 56 disjoint-set total; the V5 prose now adds the
  precise workspace-crate-sibling vocabulary and the explicit
  "path-pattern-based, not structural-relation-based" disambiguation.
  The substrate is unchanged across V1 → V2 → V3 → V4 → V5; only the
  narrative description tightened (arithmetically in V4, structurally
  in V5). No new fake-`@generated` recurrence site emerged between V4
  and V5 (the single-file V5 commit `037eec6b6` touches zero source
  bytes — 4 prose insertions + 3 prose deletions on
  `SYNTHESIS-AUDIT-OVERFIT.md` only). The 3 occurrences of the
  string `@generated by skinny bbnf-codegen` in the V5-touched file
  appear only inside backticks/inline-code (quoting the pattern the
  lint should reject), never as a real `// @generated` header.

- **CH7-2 (Lock 14 generic-crate compliance):** A3 V2 H3 HIGH→LOW
  reclassification + H6 freestanding HIGH preservation carry forward
  at V3 / V4 / V5 STAND; the 11 CRIT + 6 HIGH + 5 MED + 8 LOW = 30
  distribution holds unchanged. The V5 sibling-framing micro-fold in
  F-V5-SYNTHESIS-1a is a CH4-class structural-precision tightening
  + CH1-class vocabulary-precision tightening — it strengthens,
  rather than weakens, the Lock-14 narrative coherence by aligning
  the prose-described path-relation with the lint glob's actual
  pattern-matching semantics. No new Lock-14 surface emerged.

- **CH7-3 (strict-vs-strict comparator + per-iter oracle):** A2
  STANDs from V1 / V2 / V3 / V4 (no V5 fold); F8 + F9 NEW findings
  preserve their V2 disposition. The V5 fold does not touch the
  §3.1 C-3 row cell; the F-V4-SYNTHESIS-2 "4 scanners are fixture
  lookups" alignment carries forward. No new comparator-binding
  pathology emerged. Re-executed at V5 HEAD:

  ```
  $ grep -lE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT' \
      skinny/crates/codegen/src/css_l4_*_templates/generated.rs | wc -l
  4
  ```

  Four files (`at_rules_and_media`, `nested_layout`,
  `vendor_and_custom_atrules`, `stylesheet_selectors`) continue to
  carry the fixture-lookup short-circuit pattern; the count remains
  4 of 7. F-V4-SYNTHESIS-2 + the F-V2-SYNTHESIS-4 + F-V3-SYNTHESIS-3
  arithmetic-correction sweep is **complete and stable** across the
  V4 → V5 boundary.

- **CH7-4 (round-trip test for every generated output):** A6 V2
  LegacyPath shim disambiguation stands at V3 / V4 / V5; F-V5-
  SYNTHESIS-1a prose refresh sharpens the path-relation vocabulary
  describing the twin-substrate roots without expanding or narrowing
  the round-trip surface census. The V2-folded lint glob substrate
  (`{runtime/src/grammars,codegen/src}/**/*.rs`) remains the
  round-trip-runtime-AND-lint-CI defence on both sides of the twin.
  F-V5-SYNTHESIS-1b deletion removes a redundant meta-explanation
  sentence; the lint-coverage sub-claim is subsumed by the
  brace-expansion glob phrasing at lines 313-314 (strict superset).

- **CH7-5 (no SCAFFOLD-ONLY admit):** A5 V3 §5:133 action-class
  refresh ("preserved through PRUNE-5 as a gate-rejection invariant
  inside C-4 entry-gates so any admit attempting to cite W8/W9 pre-
  runtime-consumer is denied at admit time") **strengthens** the
  CH7-5 posture by giving the LOW resolver-honest finding an active
  enforcement role inside C-4 entry-gates rather than passive
  "no-op pre-C-4" deferral; the F-V3-SYNTHESIS-1 mirror at
  SYNTHESIS-AUDIT-OVERFIT.md:343 propagated the same active-
  enforcement framing into the §3.1 C-4 prune-cluster row; the V4
  fold did not touch §3.1 C-4 row content; the V5 fold also does
  not touch §3.1 C-4 row content (only the §2.4 sibling-framing
  parenthetical + the §2.4 residual-sentence deletion). SCAFFOLD-
  ONLY rows (W14.1-5, W13.1-4, W15.1) remain on-the-books at HEAD;
  no new scaffold-citing admit emerged.

**No sixth CH7-N criterion is required.** Both V5 fold packets
(F-V5-SYNTHESIS-1a, F-V5-SYNTHESIS-1b) are descriptive-precision /
cohesion-polish edits inside the existing five-criterion ceiling.
F-V5-SYNTHESIS-1a is a CH4 structural-precision + CH1 vocabulary-
precision correction (V4 §4 sibling-framing note closure);
F-V5-SYNTHESIS-1b is a CH2 cohesion-polish deletion (V4 §4.2
cohesion-polish note closure, removing a sentence the V4 fold's
explicit 8+6 decomposition + brace-expansion glob formulation
rendered redundant). Neither fold exceeds the lens ceiling.

**No fresh CH7-N pattern emerges from the V5 prose-only delta.**
The single-file commit `037eec6b6` (+4 / -3 prose lines on
SYNTHESIS-AUDIT-OVERFIT.md, zero source bytes) is the smallest
hardening-cycle delta in the chain; both edits close pre-identified
V4 sub-threshold informational notes; no new audit pattern surfaces.

## §2 — Per-artefact disposition table (V5)

| # | Artefact | Section | Disposition | Rationale |
| --- | --- | --- | --- | --- |
| 1 | SYNTHESIS-AUDIT-OVERFIT.md V5 | §2.4:318 sibling-framing precision ("the two path roots are siblings" → "the two path roots sit under sibling workspace crates `skinny/crates/runtime/` and `skinny/crates/codegen/`; the lint glob brace-expansion is path-pattern-based, not structural-relation-based") (F-V5-SYNTHESIS-1a) | **ACCEPT (closes V4 CH4 §4 sibling-framing note)** | Per §1.2 above, the V5 prose corrects the loose "siblings" framing — the lint-glob path-roots (`runtime/src/grammars/` 3-deep + `codegen/src/` 2-deep) are not direct siblings, but the workspace crates (`skinny/crates/runtime/` + `skinny/crates/codegen/`) ARE direct siblings at workspace-crate depth 1. Filesystem confirms both crates exist at the cited depth. The disjoint-set arithmetic claim is preserved (re-verified via three `git grep` invocations at V5 HEAD: 42 + 14 = 56, `sort -u` invariance 56). CH7-1 + CH7-4 enforcement substrate **unchanged** (lint glob brace-expansion mechanism is the same); the V5 fold is **CH4-structural-precision** + **CH1-vocabulary-precision** tightening of the **CH7-narrative description**. The added "path-pattern-based, not structural-relation-based" disambiguation strengthens future-reader correctness by making explicit that the disjoint-set claim holds by glob-pattern union, not by structural-sibling assertion. |
| 2 | SYNTHESIS-AUDIT-OVERFIT.md V5 | §2.4:319-321 residual closing sentence deletion ("The lint glob `codegen/src/**/*.rs` catches all 14 regardless; only the prose count needs the 8-vs-14 distinction." removed) (F-V5-SYNTHESIS-1b) | **ACCEPT (closes V4 CH2 §4.2 cohesion polish note; zero CH7 claim drop)** | Per §1.3 above, the deleted sentence's two sub-claims are both preserved at upstream sites within the same §2.4 block: Sub-claim A (lint-glob covers 14 codegen-side files) is subsumed by the brace-expansion glob phrasing at lines 313-314 `{runtime/src/grammars,codegen/src}/**/*.rs` (a strict superset that covers all 56 files); Sub-claim B (8-vs-14 distinction is documentary-only) is subsumed by the V4 fold's explicit "14 codegen-side (8 providers+templates + 6 ancillary)" decomposition at line 312. Per-CH7-criterion drop scan: CH7-1 enforcement claim preserved (strictly strengthened); CH7-2/3/5 N/A (sentence carried no Lock-14/comparator/SCAFFOLD claim); CH7-4 round-trip surface census preserved in full. Zero CH7-relevant claim dropped; the V5 fold is a **CH2-cohesion-polish** tightening that removes a redundant meta-explanation sentence rendered superfluous by the V4 fold's explicit decomposition + brace-expansion formulation. |
| 3 | A1 css-measurement.md (V2 STAND, V3 STAND, V4 STAND, V5 STAND) | all §§ | ACCEPT (re-attested) | No V5 fold per V4 CONSOLIDATED §2.2 Option-B micro-fold scope (single SYNTHESIS V5 agent only); V4 ACCEPT carries forward. Methodology + ledger + verdict + prune actions unchanged. |
| 4 | A2 admit-mechanism.md (V2 STAND, V3 STAND, V4 STAND, V5 STAND) | all §§ | ACCEPT (re-attested) | No V5 fold per V4 CONSOLIDATED §2.2 Option-B scope; V4 ACCEPT carries forward. F8 + F9 NEW findings preserved at V2 disposition. |
| 5 | A3 lock14-scan.md (V2 STAND, V3 STAND, V4 STAND, V5 STAND) | all §§ | ACCEPT (re-attested) | No V5 fold per V4 CONSOLIDATED §2.2 Option-B scope; V4 ACCEPT carries forward. H3 HIGH→LOW reclassification + H6 freestanding HIGH + L8 record preserved; 11 CRIT + 6 HIGH + 5 MED + 8 LOW = 30 unchanged. |
| 6 | A4 generator-truth.md (V3 STAND, V4 STAND, V5 STAND) | all §§ | ACCEPT (re-attested) | No V5 fold per V4 CONSOLIDATED §2.2 Option-B scope; V4 ACCEPT carries forward. F-V3-A4-1 methodology line-count refresh (101 → 100) preserved; the A4 ledger rows 8 / 10 / 11 file-line coordinates remain accurate per F-V2-A4-2 landing. |
| 7 | A5 decision-engine.md (V3 STAND, V4 STAND, V5 STAND) | all §§ | ACCEPT (re-attested) | No V5 fold per V4 CONSOLIDATED §2.2 Option-B scope; V4 ACCEPT carries forward. F-V3-A5-1 §5:133 action-class propagation ("preserved through PRUNE-5 as a gate-rejection invariant inside C-4 entry-gates") preserved; CH6-V2-N1 orphan-REVISE closure carries forward. |
| 8 | A6 pre-restart-pattern.md (V2 STAND, V3 STAND, V4 STAND, V5 STAND) | all §§ | ACCEPT (re-attested) | No V5 fold per V4 CONSOLIDATED §2.2 Option-B scope; V4 ACCEPT carries forward. LegacyPath both-readings-preserved disambiguation at §0:12 + §2 ledger Status preserved. |
| 9 | Cross-cutting | 11 NEW finding *categories* (20 NEW *rows*) vs 5 CH7-N criteria (V5 re-enumeration) | ACCEPT | All 11 categories map within the five-criterion ceiling (per §1.5 fresh-finding scan). The V5 fold packets add zero new categories and zero new rows; the 20-NEW-row / 11-NEW-category statistical accounting holds verbatim. No sixth criterion required. |
| 10 | Cross-cutting | Audit-pattern emergence scan (V5 confirming) | ACCEPT | No new audit pattern emerges between V4 and V5 that the 5 CH7-N criteria miss. The V5 folds tighten descriptive precision (F-V5-SYNTHESIS-1a sibling-framing vocabulary + F-V5-SYNTHESIS-1b residual-sentence cohesion polish) — both within the existing five-criterion ceiling. |
| 11 | Cross-cutting | C-3 round-trip gate enforcement check (V5 re-execution) | ACCEPT | Per §1.1 above, the metadata-derived loop continues to enumerate 9 grammars including `css_pretty` at V5 HEAD `037eec6b6`; the gate is mechanically defended end-to-end across five consecutive cycle boundaries (V1 + V2 + V3 + V4 + V5). |
| 12 | Cross-cutting | F-V5-SYNTHESIS-1a sharpens disjoint-set framing without breaking CH7-1/CH7-4 (V5 NEW verification) | **ACCEPT (substrate unchanged; structural vocabulary now precise)** | Per §1.2 above, the V5 prose refresh "the two path roots are siblings" → "the two path roots sit under sibling workspace crates …; the lint glob brace-expansion is path-pattern-based, not structural-relation-based" closes the V4 CH4 §4 documentary-decoration note. Filesystem confirms `skinny/crates/runtime/` + `skinny/crates/codegen/` are direct siblings at workspace-crate depth 1; the lint glob continues to enforce the disjoint-set union via brace-expansion. Three independent `git grep` invocations at V5 HEAD return 42 + 14 = 56 with `sort -u` invariance. CH7-1 + CH7-4 substrate is **unchanged**; F-V5-SYNTHESIS-1a is **CH4/CH1-precision** tightening of the **CH7-narrative description**. |
| 13 | Cross-cutting | F-V5-SYNTHESIS-1b residual sentence deletion drops zero CH7-relevant claim (V5 NEW verification) | **ACCEPT (cohesion polish; both sub-claims preserved upstream)** | Per §1.3 above, the deleted sentence's two sub-claims (lint-coverage of 14 codegen files; documentary-only 8-vs-14 distinction) are both preserved at upstream sites within §2.4 — the brace-expansion glob at lines 313-314 is a strict superset of the deleted `codegen/src/**/*.rs` sub-glob; the V4-folded "14 codegen-side (8 providers+templates + 6 ancillary)" decomposition at line 312 already states the 8+6 breakdown directly. Per-CH7-criterion drop scan: CH7-1 preserved with strict strengthening; CH7-2/3/5 N/A (sentence carried no such claims); CH7-4 round-trip surface census preserved. **Zero CH7-relevant claim dropped.** The V5 fold is **CH2-cohesion-polish** tightening of the **CH7-narrative description**. |
| 14 | Cross-cutting | No fake-`@generated` recurrence in V5 audit prose | ACCEPT | The single V5-touched file carries the string `@generated by skinny bbnf-codegen` only inside backticks/inline-code blocks (quoting the pattern the lint should reject); zero real `// @generated` headers in audit prose body. V4 disposition carries forward unchanged at V5. |
| 15 | Cross-cutting | No scaffold-as-load-bearing in V5 audit prose | ACCEPT | The V5 audit-prose makes no load-bearing claim resting on a SCAFFOLD-ONLY artefact. The V5 fold touches §2.4 (sibling-framing parenthetical + residual-sentence deletion) only — neither touches the SCAFFOLD-bearing W14.1-5 / W13.1-4 / W15.1 rows or the C-4 active-enforcement framing. A5 V3 §5:133 + SYNTHESIS V3 §3.1:343 active-enforcement framing preserved at V5 STAND. |
| 16 | Cross-cutting | No gate-relabel-as-admit in V5 audit prose | ACCEPT | F1-F5 remain classified gate-relabel-only via per-commit `git show --stat` evidence (V1 + V2 + V3 + V4 dispositions carry forward at V5); the V5 fold does not stamp any "row" as ADMITTED on gate-relabel basis. The "preserved through PRUNE-5 as C-4 entry-gate invariant" V3-folded posture remains the **inverse** of gate-relabel adoption. |
| 17 | Cross-cutting | CH7 lens carried as blocking (not merely acknowledged) at V5 | ACCEPT | Per the lens definition ("CH7 cannot be carried as 'acknowledged but not blocking'"), the V5 audit's recommended actions remain revert-or-rewire (PRUNE-1, PRUNE-2 binding under C-5; PRUNE-5 binding under C-4) — not advisory acknowledgement. F-V5-SYNTHESIS-1a sibling-framing precision makes the lint glob substrate's path-pattern character **explicit** (the disjoint-set arithmetic holds by brace-expansion union, not by structural sibling assertion), reinforcing the blocking posture by clarifying the exact enforcement vector. |
| 18 | Cross-cutting | Three architectural sequencing constraints preserved at V5 | ACCEPT | The three constraints (R4 → PRUNE-2; C-1 → C-4; PRUNE-4 = 9 sub-waves) carry forward at V5 with zero modification. The V5 fold surface (2 prose edits on 1 file, +4 / -3 lines) is below any envelope-perturbation threshold; SYNTHESIS §0.1 + §1.3 + §2.3 + §3.1 + §5.1 all preserve the constraints verbatim. |

ACCEPT-rate: **18 / 18 = 100.0 %.** Zero REVISE or REJECT
dispositions.

## §3 — Critical findings against the V5 audit's own write-up

**None.** The single V5-touched file preserves the V4 CH7-clean
posture and closes the three V4 sub-threshold informational notes
routed to V5 belt-and-braces (CH4 §4 sibling-framing + CH2 §4.2
cohesion polish; CH1 §3 style observation EXCLUDED per V3
CONSOLIDATED §2.1 Option-B authority) without introducing any
recurrence vector against the indicted patterns. Specifically:

- **No fake-`@generated` recurrence in V5 audit prose.** The
  V5-touched file carries the string only inside backticks/inline-
  code blocks (quoting the pattern the lint should reject), not in
  its own headers.
- **No scaffold-as-load-bearing claim in V5 audit prose.** The V5
  fold touches §2.4 only (sibling-framing parenthetical +
  residual-sentence deletion); neither touches the SCAFFOLD-bearing
  rows or the C-4 active-enforcement framing F-V3-A5-1 + F-V3-
  SYNTHESIS-1 established.
- **No gate-relabel adoption in V5 audit prose.** A5 V3 §5:133
  active gate-rejection invariant preserved at V5 STAND; no V5 fold
  reverses any V13 gate-relabel rejection.
- **No orphan grammar dressed as load-bearing in V5 audit prose.**
  A4 V3 NEW-3 stands (14 of 15 `.bbnf` files orphaned); V5 confirms
  the count via `find /Users/mkbabb/Programming/bbnf-lang/grammar/css/l4
  -name "*.bbnf" | wc -l = 15` and the orphan-15 enumeration (V3
  re-execution carries forward at V5).

## §4 — §3Z LOCK convergence chain (V5 closes V max=5 ceiling)

**The V5 confirming pass closes the §3Z LOCK convergence chain at
the V max=5 ceiling.** The chain now stands at **five consecutive
clean cycles (V1 → V2 → V3 → V4 → V5)**, well past the §3Z
two-consecutive-cycle minimum. V4 closed both V3 orphan REVISEs
(N-V3-CH2-1 + CH7 §1.5 cross-flag); V5 closes the three V4
sub-threshold informational notes routed to V5 belt-and-braces
(CH4 §4 sibling-framing + CH2 §4.2 cohesion polish; CH1 §3 style
EXCLUDED). The V5 CH7 confirming pass surfaces zero new findings;
the V5 fold packet introduces zero new CH7-N criteria; the
five-criterion ceiling continues to absorb every NEW finding category
across the entire convergence chain.

Per the dispatch §1 mandate and the V4 CONSOLIDATED §2.2 Option-B
dispatch shape ("V5 belt-and-braces micro-fold (SYNTHESIS §2.4
cohesion polish + sibling framing) + V5 CHALLENGE confirming;
G-S-P0-CONVERGED gates S-P1 dispatch post V5 LOCK"), the V5 lens
disposition discharges its commitments:

1. **Re-executed the §1.1 `cargo metadata | jq` command** at the V5
   HEAD and confirmed the 9-grammar enumeration including `css_pretty`
   continues to hold — the **fifth consecutive re-attestation** of
   the metadata-derived loop.
2. **Verified F-V5-SYNTHESIS-1a sharpens the disjoint-set framing
   without breaking CH7-1/CH7-4 mechanics** — three independent `git
   grep` invocations confirm 42 + 14 = 56 with zero overlap at V5
   HEAD; filesystem confirms `skinny/crates/runtime/` +
   `skinny/crates/codegen/` are direct siblings at workspace-crate
   depth 1; the lint glob substrate is mechanically unchanged.
3. **Verified F-V5-SYNTHESIS-1b deletion drops zero CH7-relevant
   claim** — per-sub-claim re-location check confirms both sub-claims
   (lint-coverage of 14 codegen files; documentary-only 8-vs-14
   distinction) are preserved at upstream sites within §2.4 with
   strict strengthening.
4. **Fresh-finding scan across all 5 CH7-N criteria** — zero new
   patterns emerge; the V5 prose-only delta (+4 / -3 lines) is the
   smallest hardening-cycle delta in the chain.
5. **No source-touch** — V5 lens disposition remains write-only.

The CH7-companion gating extensions (round-trip subcommand pairing
+ LOCKS.md companion-lint with the V2-folded twin-side scope, now
narratively complete via F-V3-SYNTHESIS-2, arithmetically explicit
via F-V4-SYNTHESIS-1, and structurally precise via
F-V5-SYNTHESIS-1a) remain correctly routed as gating enhancements
inside C-3 + LOCKS.md and as attribution items for S-P3.

## §5 — Disposition

The S-P0 V5 audit is CH7-clean against its own write-up. The five
CH7-N criteria continue to cover the 11 NEW finding categories (20
NEW per-row) without expansion across the V1 → V2 → V3 → V4 → V5
cycle boundaries; F-V5-SYNTHESIS-1a's "the two path roots are
siblings" → "the two path roots sit under sibling workspace crates
`skinny/crates/runtime/` and `skinny/crates/codegen/`; the lint glob
brace-expansion is path-pattern-based, not structural-relation-based"
prose refresh aligns the structural-relation vocabulary with the
lint glob's actual pattern-matching semantics — the disjoint-set
arithmetic is preserved verbatim (three `git grep` invocations at
V5 HEAD return 42 + 14 = 56 with `sort -u` invariance); the substrate
is **mechanically unchanged**; the prose now correctly describes the
workspace-crate-sibling relation and the path-pattern-vs-structural-
relation distinction. F-V5-SYNTHESIS-1b removes a residual closing
sentence that carried two sub-claims (lint-coverage of 14 codegen
files; documentary-only 8-vs-14 distinction) — both are preserved at
upstream §2.4 sites with strict strengthening (the brace-expansion
glob at lines 313-314 is a strict superset of the deleted
`codegen/src/**/*.rs` sub-glob; the V4-folded 8+6 decomposition at
line 312 states the breakdown directly). **Zero CH7-relevant claim
dropped.** CH7-1 + CH7-3 + CH7-4 enforcement substrates are
**unchanged**; both V5 folds are **CH4/CH1/CH2-precision** tightenings
of the **CH7-narrative description**. The §1.1 C-3 round-trip gate
metadata enumeration confirms 9 grammars including `css_pretty` at
V5 HEAD `037eec6b6`, matching the audit's PRUNE-4 = 9 sub-wave
binding across five consecutive cycle boundaries. **Zero new
findings** surface against the V5 artefact set. ACCEPT-rate
**100.0 %**; zero escalation; V5 cycle closes the §3Z LOCK
convergence chain at the V max=5 ceiling — **G-S-P0-CONVERGED gates
S-P1 dispatch per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP**.

---

**Scope:** S-P0 V5 CHALLENGE (SK-V14 Overfit Audit Pass) — CH7
Overfit-Prune (meta-applied to the V5 micro-redispatched audit
artefact at `037eec6b6`).
**Authority:** `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`
+ `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/
CHALLENGE-CONTEXT.md` + `restart/skinny/tranches/sk-v14/audit-overfit/
hardening/V2/CHALLENGE-V2-ADDENDUM.md` + `restart/skinny/tranches/
sk-v14/audit-overfit/hardening/V3/HARDENING-S-P0-V3-CONSOLIDATED.md
§2.3` + `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V4/
HARDENING-S-P0-V4-CONSOLIDATED.md §2.2`.
**Status:** WRITE-ONLY (untracked); aggregator commits 7 V5 lens
files + V5 CONSOLIDATED atomically.
**Next gate:** CH1-CH6 V5 + V5 aggregator complete; V5 closes §3Z
LOCK at V max=5 ceiling → G-S-P0-CONVERGED gates S-P1 dispatch per
the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.
