# S-P0 CHALLENGE V2 — CH5 HIDDEN COUPLING

Lens: CH5 — HIDDEN COUPLING (per `restart/prompts/ORCHESTRATOR.md:87`).
Scope: the five V2-folded S-P0 artefacts committed at `1735882a5`
(`SYNTHESIS-AUDIT-OVERFIT.md`, `sk-v14-audit-overfit-{lock14-scan,
generator-truth, decision-engine, pre-restart-pattern}.md`) plus the
two STAND artefacts (A1 `…css-measurement.md`, A2 `…admit-mechanism.md`).
Charge per V2 addendum §1 line 25: verify V2 folds don't introduce
parallel substrate, Track 1 ≡ Track 2, or renamed-scanner Lock 1
violation; additionally verify F-V2-A6-1 LegacyPath disambiguation
classifies the shim correctly relative to Lock 1 substrate union
(still subject to C-1 PRUNE-4 collapse); fresh-finding scan.

V1 baseline: 7 / 7 = 100 % ACCEPT (two ACCEPT-with-COUPLING-NOTE; zero
REVISE). V2 expected ≥100 % (folds are editorial-precision only; no
new substrate / IR / sidecar introductions possible).

UTC dispatch start: 2026-05-23T05:42Z. Hard cap: 30 min.

## §0 — Disposition summary

| Per-artefact | V1 | V2 | Δ |
| --- | --- | --- | --- |
| `SYNTHESIS-AUDIT-OVERFIT.md` | ACCEPT | ACCEPT | — |
| A1 `…css-measurement.md` | ACCEPT | ACCEPT (STAND) | — |
| A2 `…admit-mechanism.md` | ACCEPT | ACCEPT (STAND) | — |
| A3 `…lock14-scan.md` | ACCEPT | ACCEPT | F-V2-A3-1 strengthens substrate-union framing |
| A4 `…generator-truth.md` | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | F-V2-A4-1 explicitly preserves Track 1 / Track 2 boundary (scope-extension, not reversal) |
| A5 `…decision-engine.md` | ACCEPT | ACCEPT | F-V2-A5-1 verdict realignment strengthens C-5→C-4 sequencing (no coupling firing) |
| A6 `…pre-restart-pattern.md` | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | F-V2-A6-1 disambiguation correctly classifies LegacyPath as scope-extension (not reversal of V13 Pattern G CLEAN); shim remains C-1 PRUNE-4 collapse target |

**V2 ACCEPT-rate: 7 / 7 = 100 %.** Zero CH5 firings introduced by V2
folds. The two ACCEPT-with-COUPLING-NOTE dispositions carry forward
identical V1 observation notes (A4 §3.1 Track 1 / Track 2; A6 §3.3
substrate-doc opt-out enshrinement); V2 folds tighten the framing on
both without proposing parallel substrate, sidecar producer, scanner
rename, or Track 1 ≡ Track 2 collapse.

## §1 — V2 fold verification (charge-specific scan)

### §1.1 No parallel substrate introduced by V2 folds

Verified across all five V2-modified artefacts via `git diff
1735882a5~1 1735882a5 -- restart/skinny/tranches/sk-v14/audit-overfit/`.

| V2 fold | Substrate impact | Firing? |
| --- | --- | --- |
| F-V2-A3-1 (H3 HIGH → LOW; H6 freestanding HIGH; L8 NEW) | Reclassifies severity tier of a single `decision_csp.rs:235` test-fixture call site; production call site `passes/src/lib.rs:478` `finalize_rule(&grammar.name, …)` remains grammar-generic. Reclassification reduces violation count to 11 CRIT + 6 HIGH = 17 (down from 18) without adding any substrate; total findings unchanged at 30. No new tape, arena, builder, IR variant, or codegen path proposed. **PRUNE-3 + R4 collapse path unchanged from V1.** | NO |
| F-V2-A4-1 (NEW-1 scope-extension framing) | Per A4 V2 lines 33-46: "delta is a **scope extension, not a reversal**, of the V13 §7.1 row 1 'HONEST' verdict for `json_provider`. V13 correctly identified the grammar-derived `parse_direct` sink chunk … as honestly grammar-derived; V14 extends the scope to the **OTHER 5 emitted files** … all of which are pass-through templates." The framing INDICTS the existing fake-`@generated` sidecar (8 `<g>_provider.rs` modules); the proposed remediation (R4 + PRUNE-2) collapses the sidecar onto the IR + sink-only pipeline at `crates/codegen/src/lower/sink_only.rs`. No NEW sidecar, no NEW substrate. The `json_sink_direct::render` chunk continues to be the load-bearing grammar-derived path — its scope is preserved, not parallelised. | NO |
| F-V2-A4-2 (`json_provider.rs` line-cite refresh) | Verified by direct read: `grep -n 'include_str!' skinny/crates/codegen/src/json_provider.rs` returns lines 49 (`config_rs → normalize(include_str!...)`), 61 (`generated_rs`), 65 (`parser_rs`), 69 (`view_rs`), 73 (`value_rs`), 77 (`visitor_rs → normalize(include_str!...)`). V2's cited offsets (config=48, generated=60, parser=64, view=68, value=72) match the line *preceding* each `include_str!` (the `pub(crate) fn <X>_rs() -> String {` signature line). Refresh is line-citation precision only; no substrate change. | NO |
| F-V2-A4-3 (Three → Four CSS fixture-lookup scanners) | Verified by direct execution of `grep -nE 'CANONICAL_FIXTURE\|CAPTURED_W2_INPUT' skinny/crates/codegen/src/css_l4_*_templates/generated.rs`: returns 8 hits across 4 files (`nested_layout`, `at_rules_and_media`, `stylesheet_selectors` via `CAPTURED_W2_INPUT`, `vendor_and_custom_atrules`). Count is 4, matching V2 fold; V1 had said "three" which undercounted. The count adjustment refines the indictment; PRUNE-2 still deletes all four wholesale. **No parallel substrate; no NEW sidecar.** The "scanners" are not Lock-1 renamed scanners — they are fixture-lookup constant tables. | NO |
| F-V2-A5-1 (PARTIAL PASS → FAIL-at-HEAD, PASS conditioned on C-5 + C-4) | A5 V2 hardens the verdict from "PARTIAL PASS at v14 starting baseline" to "FAIL at HEAD, PASS conditioned on C-5 (PRUNE-1 + PRUNE-2) + C-4 (PRUNE-5)" because HEAD's `ROLLING-SOTA-DELTA.md` still carries the W14.1-5 + W13.* + W15.1 admit rows + 24 CSS L4 ADMITTED rows. This is **stronger anti-paper-close framing**, not coupling introduction — the verdict realignment ensures no admit row may cite W8 / W9 as evidence pre-C-4. The resolver still wires W5 → W6 → W7 → lowering via the five fail-closed checks at `codegen/src/lower/rust.rs:37-89` (verified verbatim in V1; A5 V2 leaves §1.3 + §1.4 untouched). No NEW resolver path, no shadow CSP, no parallel decision engine. **C-4 gate-rejection invariant strengthened** via the block-ID chain (`JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME` → `JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT`) at `decision_csp.rs:160-164`. | NO |
| F-V2-A6-1 (LegacyPath both-readings-preserved disambiguation) | A6 V2 §0:12-14 paragraph + §2 ledger row Status update to "NEW (scope-extension over V13 Pattern G; not a reversal)". Verified per A6 V2 line 14: "V13's Pattern G scan therefore most plausibly either did not survey the four `parse_with.rs` `use`-site aliases or read them as a legitimate in-flight refactor under the 'that aren't legitimate refactors' carveout. V14 surveys them explicitly post-W3.2-landing, classifies the alias as a shim that should be collapsed inside C-1 PRUNE-4 (typed-path collapse) per §4, and records the disposition as scope-extension over V13's Pattern G slate rather than reversal of any specific V13 finding." Introduction commit `0e8dbc104 feat(runtime/parse-with-{json,css-l4,sheets,bbnf}): land W3.2 entry points (AZ-IV.W3.2)` re-verified via `git log --oneline | grep 0e8dbc104` (returns the commit). The disambiguation correctly classifies the shim as a Lock-1-adjacent coupling between two co-existing path representations bridged at the `use` site, with the proposed COLLAPSE target being the typed-path representation inside C-1 PRUNE-4 — substrate-union strengthening, not parallel substrate. See §1.2 below for full CH5 disposition of the LegacyPath disambiguation. | NO |
| F-V2-SYNTHESIS-1..5 (census 54/20; co-derivation note; verdict-line alignment; Three→Four; CH7-companion lint scope extension) | SYNTHESIS V2 folds preserve all C-1..C-5 mappings verbatim; §3.2 reaffirms "**None.** Every one of the 74 findings maps to at least one C-1..C-5 candidate … No C-6 candidate is required." The CH7-companion lint scope extension (F-V2-SYNTHESIS-5) extends the glob from `skinny/crates/runtime/src/grammars/**/*.rs` to `skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs` — covering BOTH the runtime mirror AND the codegen-side template/provider sources — which forecloses a re-introduction vector where a codegen-side twin could carry the fake `@generated` header silently while the runtime-side passes the lint. This is **lint coverage closure**, not a NEW subsystem. | NO |

All seven V2 folds clear the CH5 charge: zero parallel substrate, zero
sidecar producer, zero renamed-scanner Lock 1 violation, zero Track 1
≡ Track 2 collapse vector. Re-executed `grep -rn 'fn parse_combinator\|
combinator_fallback\|parse_with_fallback' crates/core/src/runtime/
skinny/crates/runtime/src/` returns zero matches (per V1 CH5 §1
sub-vector 4); the Lock 1 renamed-scanner audit remains CLEAN.

### §1.2 F-V2-A6-1 LegacyPath disambiguation — CH5 substrate-union classification

The V2 addendum §1 line 25 explicitly tasks CH5 with: *"verify
F-V2-A6-1 LegacyPath disambig classifies the shim correctly relative
to Lock 1 substrate union (still subject to C-1 PRUNE-4 collapse)."*

**Verification scope:** the `LegacyPath` / `LegacySegment` alias is
introduced via `use crate::runtime::path::{Path as LegacyPath,
PathSegment as LegacySegment}` in 4 grammar `parse_with.rs` files.
Direct re-execution:

```
$ grep -n 'LegacyPath\|LegacySegment' crates/core/src/runtime/{json,
css_l4,bbnf,google_sheets}/parse_with.rs
crates/core/src/runtime/json/parse_with.rs:54: use ... as LegacyPath ...
crates/core/src/runtime/json/parse_with.rs:60: fn lower<...>(...) ->
  Option<LegacySegment<'a>>
… (28 hits across 4 files; identical pattern)
```

**Generic-crate leak scan:** `grep -rn 'LegacyPath\|LegacySegment'
skinny/crates` returns **zero matches**; `grep -rn ...
crates/codegen crates/passes crates/ir` directories not present in
working tree at expected paths (the codegen / passes are under
`skinny/crates/`), and the same scan against `skinny/crates/`
confirms zero leak. The shim is **bounded** to the 4 grammar-specific
`parse_with.rs` files inside `crates/core/src/runtime/`.

**Substrate-union classification (CH5 disposition):** the
`LegacyPath` shim is a Lock-1-*adjacent* coupling between two
co-existing path representations:

- `crates/core/src/runtime/path.rs::{Path, PathSegment}` — the older
  untyped representation, aliased at the `use` site as `LegacyPath` /
  `LegacySegment`.
- `TypedSegment<'a>` — the newer typed representation, walked by the
  `lower(...)` function and converted into `LegacySegment` before
  invoking `doc.get::<T>(LegacyPath::new(&legacy))`.

This is **not a parallel substrate** — there is no second `Tape`,
second `Arena`, or second `Builder`. Both `Path` and `TypedSegment`
inhabit the existing single-substrate path module; the shim is the
*bridge layer* between them. The Lock 1 substrate union (one tape,
codegen-private union variants) is unaffected by the path-bridge
shim.

The shim IS a Lock-1-adjacent **technical-debt coupling** that
warrants collapse per F-V2-A6-1's own resolution path: *"V14
surveys them explicitly post-W3.2-landing, classifies the alias as a
shim that should be collapsed inside C-1 PRUNE-4 (typed-path
collapse) per §4."* Collapse target = the typed-path representation;
the `Path` / `PathSegment` API and the `LegacyPath` aliases retire as
the typed path takes over.

**CH5 disposition of F-V2-A6-1:** ACCEPT.

1. **Classification correctness.** The "scope-extension over V13
   Pattern G, not a reversal" framing is the correct CH5 reading.
   V13 SYNTHESIS line 74's Pattern G CLEAN disposition ("No
   backwards-compat shims that aren't legitimate refactors") carries
   an explicit "legitimate refactors" carveout; the W3.2 alias was
   introduced as part of an in-flight typed-path refactor whose
   W3.3 cursor-threaded consumer was the planned collapse point. V13
   most plausibly read the alias under the carveout; V14 surveys
   post-W3.2-landing (W3.3 not yet landed at HEAD) and routes the
   alias collapse into PRUNE-4 / a small "C-6 typed-path collapse"
   sub-task. The framing preserves both readings without retconning
   V13's prior conclusion.

2. **Substrate-union impact.** The shim is **subject to C-1 PRUNE-4
   collapse** as V2 addendum §1 requires — but the shim is NOT itself
   a substrate-union violation (it does not split the tape, does not
   add a parallel arena, does not introduce a sidecar producer).
   PRUNE-4 collapse is a typed-path *technical-debt resolution* that
   strengthens the path-module surface (one typed representation
   instead of two co-existing) — substrate-union *adjacent*
   strengthening, not substrate-union *closure*. C-1 PRUNE-4 already
   carries this scope per SYNTHESIS §3.1 row C-1: "A6 NEW-HIGH-1
   (LegacyPath shim — fold as PRUNE-4 sub-task OR open small 'C-6
   typed-path collapse')". The disambiguation language at A6 V2 §2
   row's "NEW (scope-extension over V13 Pattern G; not a reversal)"
   Status preserves both the NEW classification AND the C-1 PRUNE-4
   resolution wiring. No CH5 firing.

3. **No Lock 1 substrate-union violation introduced.** Re-executed
   the V1 §1 sub-vector 6 check: A5 finding 2 cites the AUTHENTIC
   verdict on `same_substrate_union`: no `UnionTape` variant in
   `skinny/crates/runtime/src/tape/`; hardcoded `token_union_
   projection(kind, depth) -> &'static str` returns fixed constants.
   The substrate union remains INCOMPLETE by design (5 cohort
   grammars on template, 4 hot grammars hand-written per A6 NEW-HIGH-2
   substrate-doc opt-out); PRUNE-4 closes this gap by EITHER
   instantiating the hot grammars onto a richer template OR
   rewriting the substrate doc with a deletion plan. The LegacyPath
   shim is *separable* from the substrate-union closure: the path-
   bridge collapse is one PRUNE-4 sub-task; the template-vs-hand-
   written substrate-doc collapse is another. Both strengthen the
   union; neither parallels it.

### §1.3 Cross-axis coherence — Track 1 / Track 2 boundary preserved

The V1 CH5 §3.1 observation (A4 NEW-1 / R4 scope-extension as
potential Track 1 ≡ Track 2 collapse vector) was preserved verbatim
under F-V2-A4-1 framing. Re-verified:

- A4 V2 line 219 (finding 16 LOW): `xtask (root) Cargo.toml:22
  correctly wires the totality css_l4 grammar to grammar/css/l4/
  stylesheet.bbnf, and xtask::regen::run at xtask/src/regen.rs reads
  that manifest. … Noted to forestall a future redress that
  conflates the two regen pipelines under one xtask.` — Track
  separation preserved verbatim under V2.
- F-V2-A4-1's "scope extension, not reversal" framing PRESERVES the
  Track 1 / Track 2 disjunction by indicting only the SKINNY-track
  providers (`json_provider.rs` + 7 CSS providers); the totality-
  track `xtask::regen::run` path remains untouched.

The V1 CH5 §4.1 binding note for S-P3 wave manifest (R4
specification must encode Track 1 / Track 2 separation as hard
constraint, attaching `regen-css-l4-<provider>` subcommands to
`skinny/xtask/src/main.rs:8` USAGE line only) carries forward
unchanged. F-V2-A4-1 does not introduce a Track 1 ≡ Track 2
collapse — it sharpens the existing scope-extension framing while
preserving the disjunctive guard.

### §1.4 Re-execution of CH7 grammar count (V2 addendum §1 CH7 task spillover)

Re-executed: `cargo metadata --format-version 1 --no-deps
--manifest-path Cargo.toml | jq -r '.metadata.bbnf.grammars[].ident'`
returns exactly **9 grammars** (bbnf, json, css_l4, css_pretty,
google_sheets, ebnf, bnf, csv, math). Matches A6 §1 / SYNTHESIS §1.3
co-derivation note attribution of the 64 → 67 file delta +
PRUNE-4 sub-wave count 8 → 9 to the single `css_pretty` grammar
addition. (Although this task is CH7-owned per V2 addendum §1 line
27, the count consistency is a CH5 cross-check that no V2 fold
silently introduced a 10th grammar / parallel substrate ident.) **No
CH5 firing.**

## §2 — Per-artefact disposition table

| Artefact | V2 disposition | Δ from V1 |
| --- | --- | --- |
| `SYNTHESIS-AUDIT-OVERFIT.md` | ACCEPT | F-V2-SYNTHESIS-{1..5} folds preserve C-1..C-5 mappings verbatim; CH7-companion lint glob extension (codegen + runtime) strengthens the recurrence-prevention invariant; census 54/20 reframe makes the per-row vs per-category distinction explicit; co-derivation note flags that A3/A5/A6 confirms of the `css_pretty` 64 → 67 delta are evidentially co-derived, not three independent regression signals — sharpens S-P3 risk-weighting without coupling firing. **No new substrate, no new orphan finding, no new C-6 candidate.** |
| A1 `…css-measurement.md` | ACCEPT (STAND) | No V2 fold; V1 disposition carries unchanged. |
| A2 `…admit-mechanism.md` | ACCEPT (STAND) | No V2 fold; V1 disposition carries unchanged. |
| A3 `…lock14-scan.md` | ACCEPT | F-V2-A3-1 reclassifies H3 HIGH → LOW (L8) based on test-fixture vs production-call-site distinction (`decision_csp.rs:235` is `#[cfg(test)]`; production at `passes/src/lib.rs:478` is `finalize_rule(&grammar.name, …)` generic). H6 takes HIGH bar freestanding (CSS L4 entry-rule absence from acceptance-test surface), no longer derivative of H3. Total finding count unchanged at 30 (11 CRIT + 6 HIGH + 5 MED + 8 LOW). The reclassification **strengthens** substrate-union framing because it correctly attributes the violation tier to where the production overfit lives (it does not). PRUNE-3 + R4 collapse path unchanged. **No coupling firing.** |
| A4 `…generator-truth.md` | ACCEPT-with-COUPLING-NOTE | F-V2-A4-{1,2,3} folds preserve V1 §3.1 Track 1 / Track 2 coupling observation verbatim. Scope-extension framing for NEW-1 makes the V13 HONEST verdict survival explicit (the sink-derived chunk remains honestly grammar-derived; the audit extends scope to the OTHER 5 emitted files + the template body, which are pass-through with only the `@generated` header added). Three → Four fixture-lookup scanners corrects the count; PRUNE-2 deletes all four wholesale (no parallel substrate preserved). Line-cite refresh is precision-only. **V1 §3.1 binding note for S-P3 R4 specification language carries forward unchanged.** |
| A5 `…decision-engine.md` | ACCEPT | F-V2-A5-1 verdict-line realignment (PARTIAL PASS → FAIL-at-HEAD, PASS conditioned on C-5 + C-4 landing) is anti-paper-close strengthening, not coupling introduction. The resolver clause (W5 → W6 → W7 → lowering via five fail-closed checks at `codegen/src/lower/rust.rs:37-89`) carries forward unchanged. The C-4 entry-gate manifest now explicitly embeds the block-ID chain (`JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME` → `JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT`) as a gate-rejection invariant. No NEW shadow resolver, no parallel CSP, no scaffold-as-load-bearing. |
| A6 `…pre-restart-pattern.md` | ACCEPT-with-COUPLING-NOTE | F-V2-A6-1 LegacyPath disambiguation correctly classifies the shim as scope-extension over V13 Pattern G (NOT a reversal of V13's "Honest patterns left clean" line 74 disposition); §2 ledger row Status updated to "NEW (scope-extension over V13 Pattern G; not a reversal)". Per §1.2 above: the shim is **subject to C-1 PRUNE-4 collapse**, is **Lock-1-adjacent** (not a Lock-1 substrate-union violation; bounded to 4 `parse_with.rs` files with zero leak into generic crates), and the disambiguation language preserves both readings without retconning V13. **V1 §3.2 + §3.3 binding notes for S-P3 PRUNE-4 substrate-union closure declaration carry forward unchanged.** |

## §3 — Critical CH5 findings

### §3.1 No new critical findings under V2

V2 fold inspection (`git diff 1735882a5~1 1735882a5 -- …`) confirms
zero new CH5 firings introduced by V2 folds. The two V1
ACCEPT-with-COUPLING-NOTE observations (A4 §3.1 Track 1 / Track 2;
A6 §3.2 + §3.3 LegacyPath shim + substrate-doc opt-out enshrinement)
carry forward unchanged; V2 folds tighten the framing on both without
adding new coupling vectors.

### §3.2 Fresh-finding scan — V2 fold side-effects

Per V2 addendum §1 line 28 CH5 task: *"Verify V2 folds don't
introduce parallel substrate, Track 1 ≡ Track 2, or renamed-scanner
Lock 1 violation."* Fresh-finding scan executed:

| Scan target | Method | Result |
| --- | --- | --- |
| New BIR / TypeDesc / TapeKind variant | `git diff 1735882a5~1 1735882a5 -- … \| grep -nE 'enum (BackendIr\|TypeDesc\|TapeKind)\|new variant'` | Zero hits |
| New `<g>_provider.rs` introduced by V2 | `git diff 1735882a5~1 1735882a5 -- … \| grep -nE 'provider.rs\|sidecar'` | Zero new provider; existing 8 sidecar providers re-indicted only |
| New `Lock-14`-leaking grammar identifier in nominally-generic crate | `git diff 1735882a5~1 1735882a5 -- …` | Zero new symbols introduced; F-V2-A3-1 only reclassifies the existing `finalize_rule("json", …)` test-fixture call site severity tier |
| New combinator-fallback / `combinator_fallback` / `parse_with_fallback` scanner | `grep -rn 'fn parse_combinator\|combinator_fallback\|parse_with_fallback' crates/core/src/runtime/ skinny/crates/runtime/src/` | Zero matches (renamed-scanner Lock 1 audit CLEAN) |
| New regen pipeline conflation Track 1 ≡ Track 2 | A4 finding 16 + V1 §3.1 + V2 F-V2-A4-1 framing | Track 1 / Track 2 disjunction preserved verbatim under V2; F-V2-A4-1 "scope extension, not reversal" framing INDICTS Skinny-track only |

The fresh-finding scan returns **zero new findings**. The V2 folds
are editorial-precision (verdict-line alignment, line-cite refresh,
count corrections, disambiguation language); none introduces a new
substrate, sidecar, scanner-rename, or track-collapse vector.

### §3.3 Observation forward (no REVISE) — F-V2-SYNTHESIS-5 CH7-companion lint glob coverage

The CH7-companion lint extension (F-V2-SYNTHESIS-5) extends the glob
from `skinny/crates/runtime/src/grammars/**/*.rs` to `skinny/crates/
{runtime/src/grammars,codegen/src}/**/*.rs`. This is **the correct
scope** per CH5's substrate-union-must-hold invariant: a runtime-only
lint would let the codegen-side twin re-introduce the fake
`@generated` header silently (the identical-content round-tripping
vector A4 finding 15 enumerates), which would re-create the
recurrence pathway. **CH5 fully endorses** this scope extension.
Observation only; already folded into SYNTHESIS V2 §2.4.

## §4 — V3 fold recommendations

The audit campaign at V2 closes with 100 % ACCEPT under the CH5
lens (matching V1's 100 %). V2 folds strengthen multiple framings
without introducing coupling vectors. Three binding notes for V3
dispatch confirmation (V3 is a *confirming pass* over unchanged V2
artefacts per V2 addendum §4):

1. **V1 CH5 §4.1 + §4.2 forward bindings carry into V3 unchanged.**
   The R4 specification language must encode Track 1 / Track 2
   separation as a hard constraint (`regen-css-l4-<provider>`
   subcommands attach to `skinny/xtask/src/main.rs:8` USAGE line
   only); PRUNE-4 must explicitly state the substrate-union closure
   target (richer template subsuming JSON / CSS L4 / BBNF / Sheets
   vs substrate-doc rewrite with deletion plan + binding deletion of
   `arena_template.rs:1-31` + `builder_template.rs:13-31` opt-out
   passages into PRUNE-4 wave-close gates).

2. **F-V2-A6-1 LegacyPath disambiguation pattern is a generalisable
   audit template.** The "scope-extension over V13 X, not a
   reversal" framing — with introduction-commit citation,
   carveout-reading attribution, and explicit collapse-target
   routing — is the correct disposition for ANY future audit finding
   that extends a prior CLEAN disposition's scope. V3 dispatch
   should propagate this template across any other shim / latent
   surface findings (none currently exist in the V2 baseline; CH5
   confirms the fresh-finding scan is null).

3. **CH7-companion lint glob must include the totality-track
   `crates/core/src/runtime/**/*.rs` after C-1 PRUNE-4 lands.**
   F-V2-SYNTHESIS-5 currently scopes the lint to
   `skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs` — the
   skinny track only. Post-C-1 PRUNE-4, when the per-grammar
   runtime / codegen surface collapses onto generic dispatchers, the
   totality-track equivalent surfaces become the next recurrence
   risk vector. V3 should add this glob-extension obligation to the
   wave-close gate manifest. **Forward observation; not a REVISE
   blocker for V2 → V3 transition.**

Neither note is a REVISE; both are forward-binding clarifications
that strengthen the C-1 + C-4 mappings already enumerated in
SYNTHESIS §3.1 + V2 SYNTHESIS §2.4. The CH5 lens advances without
convergence blocker.

## §5 — Verdict

**CH5 — HIDDEN COUPLING V2: ACCEPT (7 / 7 = 100 %).**

The S-P0 audit campaign at SK-V14 V2 holds the CH5 lens without
firing. Every V2 fold is verified editorial-precision (verdict-line
alignment, line-cite refresh, count corrections, disambiguation
language); none introduces a parallel substrate, sidecar producer,
renamed-scanner Lock 1 violation, or Track 1 ≡ Track 2 collapse
vector.

The F-V2-A6-1 LegacyPath disambiguation (the addendum §1 line 25
focal task for CH5) correctly classifies the shim as:
(a) NEW finding by scope-extension over V13 Pattern G (not a
reversal of V13's CLEAN disposition); (b) Lock-1-*adjacent*
coupling (not a Lock-1 substrate-union violation); (c) subject to
C-1 PRUNE-4 typed-path collapse with bounded scope (4 `parse_with.rs`
files, zero leak into generic crates). Both readings (V13 carveout
+ V14 explicit survey) are preserved without retconning, and the
collapse target wires correctly into the C-1 PRUNE-4 sub-task per
SYNTHESIS §3.1.

The fresh-finding scan returns zero new coupling findings. V1's two
ACCEPT-with-COUPLING-NOTE observations (A4 §3.1 Track 1 / Track 2
boundary; A6 §3.2 + §3.3 LegacyPath bounded + substrate-doc opt-out
enshrinement) carry forward unchanged under V2 with V2 folds
sharpening the framings.

Per `ORCHESTRATOR.md §3Z`, the CH5 lens convergence criterion is met
at V2 (100 % ACCEPT × 2 consecutive cycles when summed with V1's
100 %; zero open critical defects; zero orphan unresolved REVISE).
V3 confirming pass over unchanged V2 artefacts will close §3Z's
two-consecutive-cycle chain → G-S-P0-CONVERGED → S-P1 dispatch per
SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.
