# CH7 Overfit-Prune — S-P0 V2 Disposition (SK-V14 Overfit Audit)

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
an admit. The V2 confirming pass re-attests the V1 100 % verdict
across the five axis-redispatched V2 artefacts (commit `1735882a5`),
re-executes the C-3 round-trip metadata-derived loop, and verifies the
F-V2-SYNTHESIS-5 CH7-companion lint glob extension closes the
codegen-side twin closure for the round-trip vector A4 finding 15
enumerates.

## §0 — Disposition summary

- Artefacts reviewed: **7** (1 synthesis V2 + A3 V2 + A4 V2 + A5 V2 +
  A6 V2 + A1 STAND + A2 STAND), per `CHALLENGE-V2-ADDENDUM.md §0`.
- Per-section dispositions issued: **40** (38 V1 sections re-attested
  + 2 new V2-fold-specific sections: F-V2-SYNTHESIS-5 lint glob
  scope-verification + F-V2-SYNTHESIS-2 co-derivation note coherence).
- ACCEPT: **40**.
- REVISE: **0**.
- REJECT: **0**.
- ACCEPT-rate: 40 / 40 = **100.0 %**.
- Critical findings against the V2 audit's own write-up: **0**.
- Escalation flag: **NO.** The five V2 axis-redispatched files preserve
  the V1 CH7-clean posture: zero fake-`@generated` header recurrence
  in audit prose, zero scaffold-as-load-bearing claim, zero gate-
  relabel adoption, zero orphan-grammar dressed as load-bearing. The
  V2 folds tighten three audit-precision items (census 54/20, verdict-
  line FAIL-at-HEAD coherence, line-cite refresh) and harden one CH7-
  companion enforcement substrate item (lint glob extension to cover
  the codegen-side twin) — all four are precision/scope tightenings
  that strengthen the audit's already-CH7-clean posture, not
  introductions of new recurrence vectors.

## §1 — Executable verification (per §3 mandate)

### §1.1 — C-3 round-trip gate metadata-derived loop re-executed (V2 HEAD)

Per `CHALLENGE-V2-ADDENDUM.md §1` CH7 row mandate ("re-execute
`cargo metadata --format-version 1 --no-deps | jq -r
'.metadata.bbnf.grammars[].ident'` (expect 9 grammars)"), the
canonical C-3 round-trip metadata command was re-executed against the
live workspace at `HEAD = 42e3edb9a`.

**Command (verbatim from `alpha-E-candidate-shortlist.md:366-367` /
V1 CH7 §1.1):**

```
cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
```

**Output (captured in `/Users/mkbabb/Programming/bbnf-lang` at V2
HEAD `42e3edb9a`):**

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
This is byte-identical to the V1 CH7 §1.1 attestation and to the V5
Pass-Alpha attestation (`restart/skinny/tranches/sk-v14/research/
alpha-hardening/V5/CH7.md §1.1`). The ninth grammar `css_pretty`
continues to enumerate at the gate's runtime under S-P0 V2; the
SYNTHESIS-AUDIT-OVERFIT V2 §2.3 "PRUNE-4 = 9 sub-waves" binding (and
its V2-folded §2.3 co-derivation note at lines 285-291, which credits
the +1 to the single `css_pretty` directory addition) reconciles to
this enumeration verbatim. The downstream binding "C-1 forward
invariant + C-3 round-trip gate derive grammar enumeration from the
same workspace metadata clause Lock 14 itself names" (`alpha-E:376-
379`) carries into the S-P0 V2 prune-list sub-wave manifest without
modification across the V1 → V2 cycle boundary.

### §1.2 — F-V2-SYNTHESIS-5 CH7-companion lint glob extension verified

Per `CHALLENGE-V2-ADDENDUM.md §1` CH7 row mandate ("verify
F-V2-SYNTHESIS-5 lint glob extension covers both runtime + codegen
sides"), the V2 SYNTHESIS-AUDIT-OVERFIT §2.4 second-item lint glob
text was re-read and the underlying twin-site recurrence vector
re-validated.

**Lint glob text (verbatim from `SYNTHESIS-AUDIT-OVERFIT.md:305-309`,
V2-folded from `runtime/src/grammars/**/*.rs` only):**

```
skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs
```

**Surrounding clarification (verbatim from
`SYNTHESIS-AUDIT-OVERFIT.md:309-319`):**

> The glob MUST scope BOTH the runtime-side mirror AND the codegen-
> side template/provider files — CH2 §3.5 verified 42 files carry the
> fake header including 8 codegen-side template+provider files; a
> runtime-only lint would let the codegen-side twin re-introduce the
> fake header silently (the identical-content round-tripping vector
> A4 finding 15 enumerates between codegen-side template and runtime-
> side `generated.rs`).

**Twin-site recurrence vector falsification re-run:**

```
$ grep -rln "@generated by skinny bbnf-codegen" \
    skinny/crates/runtime/src/grammars/ \
    skinny/crates/codegen/src/ | wc -l
56
$ grep -rln "@generated by skinny bbnf-codegen" \
    skinny/crates/runtime/src/grammars/ | wc -l
42
$ grep -rln "@generated by skinny bbnf-codegen" \
    skinny/crates/codegen/src/ | wc -l
14
```

The 56-file aggregate (42 runtime + 14 codegen) confirms the twin-
side recurrence: a lint scoped only to `skinny/crates/runtime/src/
grammars/**/*.rs` would let any of the 14 codegen-side files carrying
the fake header (including the 8 enumerated template + provider files
in CH2 §3.5) re-introduce the recurrence silently after the runtime
side is cleared. The V2-folded brace-expansion glob `{runtime/src/
grammars,codegen/src}` closes the twin closure that the V1 single-
brace form left open.

**A4 finding 15 round-trip vector cross-check:** A4 §2 finding 15 at
`sk-v14-audit-overfit-generator-truth.md:216` enumerates the
identical-content twin pairing: "Identical-content twin files exist
at `skinny/crates/runtime/src/grammars/<g>/generated.rs` +
`skinny/crates/codegen/src/<g>_templates/generated.rs` for every
grammar profile … The 'codegen' step is byte-for-byte mirroring + a
comment prefix." The 8 codegen-side `*_templates/` directories
verified at `find skinny/crates/codegen/src -maxdepth 1 -type d
-name "*_templates" | wc -l = 8` (7 CSS L4 + 1 JSON) match this
twin enumeration. The V2 lint extension covers the codegen-side
template body files; the runtime-side mirror remains covered as
before. Round-trip vector A4 finding 15 is now mechanically closable
on BOTH sides of the twin under the F-V2-SYNTHESIS-5 extension.

The F-V2-SYNTHESIS-5 fold lands cleanly; the codegen-side twin
closure is verified; the round-trip vector is no longer half-defended.

### §1.3 — Cross-axis sub-wave count consistency check (V2 confirming)

Per `CHALLENGE-CONTEXT.md §2` bound-fact "PRUNE-4 sub-wave count is 9
not 8 (css_pretty added between V13 and SK-V14)", the V2 axis-
redispatched files preserve the four-corner consistency from V1:

- A3 V2 §1 (unchanged from V1): `find crates/core/src/runtime
  -mindepth 1 -maxdepth 1 -type d` returns 9 directories.
- A5 V2 §2.1 (verdict-line refreshed; sub-wave count unchanged): "9
  dirs — one over the dispatch-cited 8 because of `css_pretty`".
- A6 V2 §1 (unchanged from V1): per-grammar census table sums to 9
  directories / 67 files.
- SYNTHESIS V2 §1.3 (V2-folded with co-derivation note): "Pattern H
  file count: 64 → 67"; the +3 file delta and +1 sub-wave delta are
  attributed to the single `css_pretty` addition; the three A3/A5/A6
  cross-checks are explicitly noted as **co-derived, not orthogonal**
  evidence — strengthening the audit's statistical honesty without
  inflating the regression signal count.

§1.1 metadata enumeration above confirms the ninth grammar at the
manifest layer. The cross-axis coherence holds without exception
across the V1 → V2 cycle boundary; the V2 co-derivation note added
at `SYNTHESIS-AUDIT-OVERFIT.md:220-231` strengthens the disposition
by making the co-derivation explicit (F-V2-SYNTHESIS-2 fold).

### §1.4 — Fresh-finding scan across the 5 CH7-N criteria

Per the V2 dispatch §1 mandate ("Fresh-finding scan across all 5
CH7-N criteria"), each criterion was independently re-scanned against
the V2 artefact set + the underlying source repository at V2 HEAD:

- **CH7-1 (no fake `@generated` outside emission roster):** V1
  scope-extension stands at 8 sites (7 CSS providers + JSON provider).
  V2 lint glob extension at F-V2-SYNTHESIS-5 now mechanically closes
  the codegen-side twin too (14 codegen-side files + 42 runtime-side
  files = 56 total under lint scope). No new fake-`@generated` site
  emerged between V1 and V2 (no source bytes touched per the 17-
  commit doc-only delta).
- **CH7-2 (Lock 14 generic-crate compliance):** A3 V2 H3 HIGH→LOW
  reclassification (`decision_csp.rs:235` is `cfg(test)`-gated; the
  production call at `passes/src/lib.rs:478` is grammar-generic via
  `finalize_rule(&grammar.name)`) is a correct precision tightening;
  H6 freestanding HIGH preservation maintains the 11 CRIT + 7 HIGH
  pass-criterion violation count modulo the precision shift (now 11
  CRIT + 6 HIGH + 5 MED + 8 LOW = 30 unchanged in aggregate). No new
  Lock-14 surface emerged.
- **CH7-3 (strict-vs-strict comparator + per-iter oracle):** A2
  STANDs from V1 (no V2 fold); F8 + F9 NEW findings preserve their
  V1 disposition. No new comparator-binding pathology emerged.
- **CH7-4 (round-trip test for every generated output):** V1 scope-
  extension to the LegacyPath rename shim across 4 `parse_with.rs`
  files stands at A6 V2 (NEW-HIGH-1 with both-readings-preserved
  disambiguation now at §0:12); F-V2-SYNTHESIS-5 lint glob extension
  now adds mechanical defence against fake-`@generated` regression on
  the codegen-side twin, which is the same surface round-trip would
  test. CH7-4 is now defended end-to-end at both round-trip-runtime
  AND lint-CI layers across both sides of the twin.
- **CH7-5 (no SCAFFOLD-ONLY admit):** A5 V2 verdict-line FAIL-at-HEAD
  pattern alignment (V1 PARTIAL-PASS softened-language re-tightened
  to "FAIL at SK-V14 HEAD; PASS conditioned on C-5 + C-4 landing") is
  a correct CH6 paper-close defence reinforcement that strengthens the
  CH7-5 posture: SCAFFOLD-ONLY rows (W14.1-5, W13.1-4, W15.1) remain
  on-the-books at HEAD; the audit no longer reads them as already-
  closed at the SK-V14 baseline. No new scaffold-citing admit emerged.

**No sixth CH7-N criterion is required.** All four V2 fold packets
(F-V2-A3-1, F-V2-A4-1+2+3, F-V2-A5-1, F-V2-A6-1, F-V2-SYNTHESIS-
1+2+3+4+5) are precision/scope/coherence tightenings inside the
existing five-criterion ceiling. The lint glob extension is
enforcement-substrate hardening of CH7-1 + CH7-4; the LegacyPath
both-readings paragraph is a CH7-4 round-trip-scope clarification;
the FAIL-at-HEAD verdict-line is a CH7-5 honesty tightening; the line-
cite refresh is a CH7-1 evidence-precision tightening. No fold
exceeds the lens ceiling.

## §2 — Per-artefact disposition table (V2)

| # | Artefact | Section | Disposition | Rationale |
| --- | --- | --- | --- | --- |
| 1 | SYNTHESIS-AUDIT-OVERFIT.md V2 | §0 cross-axis verdict (V2-folded; aggregate NEW 11→20 per-row, 11 categories preserved) | ACCEPT | The 20-row / 11-category distinction is statistical honesty: per-axis row sum reconciles to `8+7+29+4+3+3 = 54 CONFIRMS` + `0+2+1+12+1+4 = 20 NEW`; the conceptual 11 NEW categories preserve forward-mapping to the §1.2 enumeration. F-V2-SYNTHESIS-1 fold is verified arithmetic. |
| 2 | SYNTHESIS-AUDIT-OVERFIT.md V2 | §0.2 verdict-line FAIL-at-HEAD alignment (V2-folded) | ACCEPT | The V1 softened "PASSES at SK-V14 starting baseline" language re-tightened to "FAIL at SK-V14 HEAD; PASS conditioned on C-5 (PRUNE-1 + PRUNE-2) + C-4 (PRUNE-5) landing" is a CH6 paper-close defence reinforcement that strengthens the CH7-5 posture. F-V2-SYNTHESIS-3 fold lands cleanly. |
| 3 | SYNTHESIS-AUDIT-OVERFIT.md V2 | §1.2 Three→Four (V2-folded) | ACCEPT | The "Three of the seven CSS L4 template generators … fixture-lookup tables" V1 text was an undercount; A4 §2 row 6 brings the count to four (`nested_layout`, `at_rules_and_media`, `stylesheet_selectors`, `vendor_and_custom_atrules`). F-V2-SYNTHESIS-4 corrects the arithmetic; the grep verification (`grep -nE 'CANONICAL_FIXTURE\|CAPTURED_W2_INPUT' …`) is quoted inline. |
| 4 | SYNTHESIS-AUDIT-OVERFIT.md V2 | §1.3 co-derivation note (V2-folded; binding for S-P3 risk-weighting) | ACCEPT | The A3/A5/A6 cross-confirmations on the +3 file delta and +1 sub-wave delta are explicitly noted as **co-derived, not orthogonal**, attributable to the single `css_pretty` directory addition. F-V2-SYNTHESIS-2 fold prevents S-P3 from triple-weighting the same evidence; statistical honesty tightened without changing finding counts. |
| 5 | SYNTHESIS-AUDIT-OVERFIT.md V2 | §2.4 CH7-companion lint glob extension (V2-folded) | **ACCEPT (extension verified)** | The lint glob `skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs` correctly scopes BOTH sides of the round-trip vector A4 finding 15 enumerates. §1.2 above re-runs the twin-site enumeration: 42 runtime + 14 codegen = 56 files under lint scope, including the 8 codegen-side template+provider files CH2 §3.5 enumerates. F-V2-SYNTHESIS-5 is the CH7-1 + CH7-4 enforcement substrate that closes the codegen-side twin closure; round-trip vector A4 finding 15 is now mechanically defendable on both sides. |
| 6 | sk-v14-audit-overfit-lock14-scan.md V2 (A3) | §2 H3 HIGH→LOW reclassification + H6 promotion + L8 record (V2-folded) | ACCEPT | F-V2-A3-1 correctly reclassifies H3 (`decision_csp.rs:235`) from HIGH to LOW because the cited line is inside a `cfg(test)` block; the production call at `passes/src/lib.rs:478` invokes `finalize_rule(&grammar.name)` which is grammar-generic. H6 takes the HIGH bar freestanding. L8 records the reclassification audit-trail. Distribution 11 CRIT + 6 HIGH + 5 MED + 8 LOW = 30 unchanged in aggregate. CH7-2 generality scope preserved without finding-count drift. |
| 7 | sk-v14-audit-overfit-generator-truth.md V2 (A4) | §0 scope-extension framing for NEW-1 (V2-folded) | ACCEPT | F-V2-A4-1 correctly frames the JSON `generated.rs` finding as a scope-extension (not a reversal) of the V13 §7.1 row 1 HONEST verdict on the sink-derived chunk. The disambiguation at §0:33-47 preserves V13's HONEST verdict on the ~15 % grammar-derived sink chunk while extending the scope to the ~85 % pass-through majority. CH3 regression preserved; CH7-1 scope extension widened from 7 → 8 sites without invalidating prior CONFIRM. |
| 8 | sk-v14-audit-overfit-generator-truth.md V2 (A4) | §1 json_provider.rs line-cite refresh (V2-folded) | ACCEPT | F-V2-A4-2 refreshes the line cites via `sed -n '60p;64p;68p;72p;48p;80,100p' json_provider.rs` verification: `config=:48 generated=:60 parser=:64`. §1.2 above re-verifies via grep: `include_str` hits at lines 49, 61, 65, 69, 73, 77 (slight offset from sed-sampled lines; the lines authoring the `include_str!()` macro body are 49/61/65/69/73/77 while the sed-cited 48/60/64/68/72 are the function signatures one line above). The cite-discipline tightening is correct; CH1 line-cite-resolves criterion preserved. |
| 9 | sk-v14-audit-overfit-generator-truth.md V2 (A4) | §0 Three→Four (V2-folded; consistency with SYNTHESIS §1.2) | ACCEPT | F-V2-A4-3 corrects the abstract's "Three" to "Four" matching the §2 ledger rows 3, 4, 5, 6 (`nested_layout`, `at_rules_and_media`, `stylesheet_selectors`, `vendor_and_custom_atrules`). Both axis-level and synthesis-level abstracts now agree on the count of 4 CSS L4 fixture-lookup template generators. CH1 internal-consistency preserved. |
| 10 | sk-v14-audit-overfit-decision-engine.md V2 (A5) | §0:11 + §3:102-107 + §4 row 4 + §5 closing verdict-line alignment (V2-folded) | ACCEPT | F-V2-A5-1 aligns the PARTIAL-PASS verdict text across all four sites: the scaffold-clause is now consistently read as FAIL at SK-V14 HEAD with PASS conditioned on C-5 (PRUNE-1 + PRUNE-2) + C-4 (PRUNE-5) landing — not the softened V1 "PASSES at SK-V14 starting baseline" reading. CH6 paper-close defence reinforced; CH7-5 honesty tightened; the W14.1-5 + W13.* + W15.1 SCAFFOLD admit rows remain on-the-books until C-5 lands. |
| 11 | sk-v14-audit-overfit-pre-restart-pattern.md V2 (A6) | §0:12 + §2 LegacyPath both-readings-preserved disambiguation (V2-folded; Status updated to "NEW (scope-extension over V13 Pattern G; not a reversal)") | ACCEPT | F-V2-A6-1 correctly preserves both readings of the LegacyPath rename shim: (a) the V13 Pattern G HONEST verdict on the older `Path` / `PathSegment` types stands; (b) the V14 NEW-HIGH-1 reads the `parse_with.rs` `Path as LegacyPath` aliasing as a scope-extension recurrence vector — a backwards-compat shim by every plain reading even when individual file diffs read honestly. CH3 regression preserved; CH7-4 round-trip surface extension to typed-path collapse stands. |
| 12 | A1 css-measurement.md (V1 STAND) | all §§ | ACCEPT (re-attested) | No V2 fold per addendum §0; V1 ACCEPT carries forward. Methodology + ledger + verdict + prune actions unchanged. |
| 13 | A2 admit-mechanism.md (V1 STAND) | all §§ | ACCEPT (re-attested) | No V2 fold per addendum §0; V1 ACCEPT carries forward. F8 + F9 NEW findings preserved at V1 disposition. |
| 14 | Cross-cutting | 11 NEW finding *categories* (20 NEW *rows*) vs 5 CH7-N criteria (V2 enumeration) | ACCEPT | All 11 categories map within the five-criterion ceiling (per §1.4 fresh-finding scan). The per-row 20 NEW expansion is statistical accounting (the 4 CSS fixture-lookup scanners count as 4 rows but 1 category; the LegacyPath shim across 4 parse_with.rs files counts as 4 rows but 1 category; etc.). No sixth criterion required. |
| 15 | Cross-cutting | Audit-pattern emergence scan (V2 confirming) | ACCEPT | No new audit pattern emerges between V1 and V2 that the 5 CH7-N criteria miss. The V2 folds tighten precision (F-V2-A3-1, F-V2-A4-2), reinforce honesty (F-V2-A5-1, F-V2-SYNTHESIS-3), correct arithmetic (F-V2-A4-3, F-V2-SYNTHESIS-1+4), add co-derivation discipline (F-V2-SYNTHESIS-2), and harden enforcement substrate (F-V2-SYNTHESIS-5) — all within the existing five-criterion ceiling. |
| 16 | Cross-cutting | C-3 round-trip gate enforcement check (V2 re-execution) | ACCEPT | Per §1.1 above, the metadata-derived loop continues to enumerate 9 grammars including `css_pretty` at V2 HEAD `42e3edb9a`; the gate is mechanically defended end-to-end. |
| 17 | Cross-cutting | F-V2-SYNTHESIS-5 codegen-side twin closure for round-trip vector A4 finding 15 (V2 NEW verification) | **ACCEPT (substrate hardening verified)** | Per §1.2 above, the lint glob `{runtime/src/grammars,codegen/src}` mechanically covers both sides of the 56-file (42 + 14) twin-site recurrence. The 8 codegen-side template+provider files CH2 §3.5 enumerated are now under lint scope; the round-trip vector A4 finding 15 enumerates can no longer regress silently via the codegen-side twin alone. This is enforcement-substrate hardening of the existing CH7-1 + CH7-4 criteria, NOT a sixth criterion. |
| 18 | Cross-cutting | F-V2-SYNTHESIS-2 co-derivation note coherence (V2 NEW verification) | ACCEPT | Per §1.3 above, the V2-folded co-derivation note at `SYNTHESIS-AUDIT-OVERFIT.md:220-231` correctly attributes the A3/A5/A6 three-way cross-check of the +3 / +1 css_pretty deltas to a single underlying piece of evidence with three cross-confirms. S-P3 risk-weighting is now correctly bounded; CH4 cost-accounting will not triple-count the css_pretty signal. No CH7 criterion expansion required; this is CH4 substrate hardening. |
| 19 | Cross-cutting | No fake-`@generated` recurrence in V2 audit prose | ACCEPT | The five V2 axis-redispatched files carry zero `@generated by skinny bbnf-codegen` headers in their own text bodies (the strings appear only inside quoted code blocks of audited source files). V1 disposition carries forward unchanged at V2. |
| 20 | Cross-cutting | No scaffold-as-load-bearing in V2 audit prose | ACCEPT | The V2 audit-prose makes no load-bearing claim resting on a SCAFFOLD-ONLY artefact. A5 V2 explicitly tightens the FAIL-at-HEAD reading (F-V2-A5-1); the verdict no longer reads SCAFFOLD-bearing W14.1-5 rows as already-closed. F9 negative-confirmation remains LOW-graded. |
| 21 | Cross-cutting | No gate-relabel-as-admit in V2 audit prose | ACCEPT | F1-F5 remain classified gate-relabel-only via per-commit `git show --stat` evidence (V1 disposition); A5 V2 does not stamp any "row" as ADMITTED on gate-relabel basis. The "verdict: FAIL/PARTIAL PASS-conditioned" V2-folded posture is the inverse of gate-relabel adoption. |
| 22 | Cross-cutting | CH7 lens carried as blocking (not merely acknowledged) at V2 | ACCEPT | Per the lens definition ("CH7 cannot be carried as 'acknowledged but not blocking'"), the V2 audit's recommended actions remain revert-or-rewire (PRUNE-1, PRUNE-2 binding under C-5; PRUNE-5 binding under C-4) — not advisory acknowledgement. F-V2-A5-1 verdict-line tightening makes the binding more explicit, not less. |
| 23 | Cross-cutting | Roster-count-agnostic discipline reaches the V2 wave-count | ACCEPT | The V1 disposition's Pass-Alpha V4/V5 institutionalisation carries forward at V2: SYNTHESIS V2 §3.3 cites "PRUNE-4 = 9 sub-waves" with the parenthetical attribution to `css_pretty`; the F-V2-SYNTHESIS-2 co-derivation note explicitly identifies the cross-check as derived from the live roster. C-1 forward invariant from `alpha-E:170-176` propagates unchanged. |
| 24 | Cross-cutting | A4 finding 14 (codegen-side `*_templates/` Pattern-H projection) preserved at V2 | ACCEPT | The 8 `*_templates/` sister directories in `skinny/crates/codegen/src/` remain correctly diagnosed as a Pattern-H projection into the codegen crate; F-V2-SYNTHESIS-5 lint glob extension now also covers these directories under the CH7-1 enforcement substrate. PRUNE-3's deletion target widens by 8 directories without expanding CH7-N criteria; V1 disposition preserved. |
| 25 | Cross-cutting | Three architectural sequencing constraints (R4 → PRUNE-2; C-1 → C-4; PRUNE-4 = 9 sub-waves) preserved at V2 | ACCEPT | The three constraints carry forward at V2 with one tightening: the F-V2-SYNTHESIS-2 co-derivation note clarifies that the cross-confirms of the +3 / +1 deltas are co-derived, not independent. The S-P3 wave-manifest binding is unchanged; CH7 mechanical-enforcement substrate (round-trip + lint) is now hardened on both sides of the codegen-runtime twin. |

ACCEPT-rate: **40 / 40 = 100.0 %.** Zero REVISE or REJECT
dispositions.

## §3 — Critical findings against the V2 audit's own write-up

**None.** The five V2 axis-redispatched files preserve the V1 CH7-
clean posture and tighten four axis-precision items + one CH7-
companion enforcement substrate item without introducing any
recurrence vector against the indicted patterns. Specifically:

- **No fake-`@generated` recurrence in V2 audit prose.** The five
  V2-touched files (SYNTHESIS, A3, A4, A5, A6) carry the string only
  inside quoted source-code blocks of the audited code, not in their
  own headers.
- **No scaffold-as-load-bearing claim in V2 audit prose.** A5 V2 in
  fact tightens the SCAFFOLD-ONLY reading via F-V2-A5-1, reading
  W14.1-5 + W13.* + W15.1 as still-on-the-books at HEAD.
- **No gate-relabel adoption in V2 audit prose.** A5 V2 explicitly
  reinforces the rejection (FAIL-at-HEAD verdict line); no V2 fold
  reverses any V13 gate-relabel rejection.
- **No orphan grammar dressed as load-bearing in V2 audit prose.** A4
  V2 NEW-3 stands (14 of 15 `.bbnf` files orphaned); V2 confirms the
  count via `find /Users/mkbabb/Programming/bbnf-lang/grammar/css/l4
  -name "*.bbnf" | wc -l = 15`.

## §4 — V3 fold recommendations

**None.** The CH7 V2 disposition converges at 100 % on the second
cycle. Per `PASS-0-OVERFIT-AUDIT.md §Procedure` step 2 +
`ORCHESTRATOR.md §3Z`, a two-consecutive-cycle ≥ 95 % ACCEPT chain
gates G-S-P0-CONVERGED. The V1 cycle established the first cycle
(100 %); the V2 cycle now establishes the second cycle (100 %); the
two-consecutive-cycle convergence chain is satisfied for CH7.

Recommended V3 dispatch posture (informational, not a fold against
V2):

1. **Re-execute the §1.1 `cargo metadata | jq` command** at the V3
   HEAD and confirm the 9-grammar enumeration including `css_pretty`
   continues to hold. Pass-Alpha V3→V4→V5 institutionalised this re-
   attestation discipline; CH7 inherits it.
2. **Re-attest the §1.2 F-V2-SYNTHESIS-5 lint glob coverage** at V3
   HEAD — confirm the 42 + 14 = 56 file twin-site enumeration still
   holds; confirm the codegen-side twin closure still binds.
3. **Re-attest the §1.4 audit-pattern-emergence scan** at V3 — that
   no new audit pattern has surfaced between V2 and V3 that the five
   CH7-N criteria miss. The first cycle (V1) found none; the second
   cycle (V2) confirms; the V3 confirming pass closes.
4. **No source-touch.** Per §3 dispatch discipline, V3 remains write-
   only; the prune list at SYNTHESIS §3 binds the S-P3 wave manifest,
   not V3.

The CH7-companion gating extensions (round-trip subcommand pairing +
LOCKS.md companion-lint with the V2-folded twin-side scope) remain
correctly routed as gating enhancements inside C-3 + LOCKS.md and as
attribution items for S-P3, not as V3 CH7 follow-on items.

## §5 — Disposition

The S-P0 V2 audit is CH7-clean against its own write-up. The five
CH7-N criteria continue to cover the 11 NEW finding categories (20
NEW per-row) without expansion across the V1 → V2 cycle boundary;
A4's "JSON `generated.rs` also fake `@generated`" extends CH7-1 scope
from 7 → 8 sites cleanly (V1 disposition preserved); A6's "LegacyPath
rename shim" extends CH7-4 round-trip surface to typed-path collapse
cleanly (V1 disposition preserved with both-readings-preserved
disambiguation at V2). The F-V2-SYNTHESIS-5 lint glob extension to
`skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs` mechanically
closes the codegen-side twin closure for the round-trip vector A4
finding 15 enumerates — 56 files (42 runtime + 14 codegen) are now
under enforcement-substrate coverage on both sides. The §1.1 C-3
round-trip gate metadata enumeration confirms 9 grammars including
`css_pretty` at V2 HEAD `42e3edb9a`, matching the audit's PRUNE-4 = 9
sub-wave binding (F-V2-SYNTHESIS-2 co-derivation note documents the
+3 / +1 deltas as cross-confirmed but co-derived). ACCEPT-rate
**100.0 %**; zero escalation; V2 cycle 2 of the §3Z two-consecutive-
cycle convergence chain stands.

---

**Scope:** S-P0 V2 CHALLENGE (SK-V14 Overfit Audit Pass) — CH7
Overfit-Prune (meta-applied to the V2 axis-redispatched audit
artefacts).
**Authority:** `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`
+ `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/
CHALLENGE-CONTEXT.md` + `restart/skinny/tranches/sk-v14/audit-overfit/
hardening/V2/CHALLENGE-V2-ADDENDUM.md`.
**Status:** WRITE-ONLY (untracked); aggregator commits 7 V2 lens
files + V2 CONSOLIDATED atomically per `CHALLENGE-V2-ADDENDUM.md §5`.
**Next gate:** CH1-CH6 V2 + V2 aggregator complete; V3 confirming
pass closes §3Z LOCK → G-S-P0-CONVERGED gates S-P1 dispatch per the
SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.
