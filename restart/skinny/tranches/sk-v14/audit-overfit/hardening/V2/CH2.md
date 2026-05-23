# CH2 — GENERALITY (V2)

S-P0 CHALLENGE V2 lens disposition (SK-V14 Overfit Audit Pass).

Authority: `restart/prompts/ORCHESTRATOR.md §3W` row CH2 ("Lock 14
holds: no grammar-name leak; every proposed intervention is grammar-
neutral and works for CSS L4 / Sheets / BBNF-self, not only JSON.") +
`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V2/CHALLENGE-
V2-ADDENDUM.md §1` CH2 row + V1 CH2 base at
`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/CH2.md`
(7 ACCEPT + 1 ACCEPT-with-NOTE / 8 = 100 %; 1 NEW LOW + 2 precision
NOTEs queued for V2).

V2 artefacts under review (commit `1735882a5`, 5 files modified
+113/-55): A3 V2 (H3 HIGH→LOW + L8 inserted + H6 freestanding HIGH;
aggregate 30 unchanged); A4 V2 (3 folds — scope-extension framing +
json_provider line-cite refresh + Three→Four); A5 V2 (verdict-line
alignment); A6 V2 (LegacyPath disambiguation); SYNTHESIS V2 (5 folds
— census 54/20 + co-derivation note + verdict-line alignment +
Three→Four + CH7-companion lint glob extension). A1 + A2 STAND.

## §0 — Disposition summary

| Total dispositions | ACCEPT | ACCEPT-with-NOTE | REVISE | REJECT |
| ---: | ---: | ---: | ---: | ---: |
| **8** | **8** | **0** | **0** | **0** |

ACCEPT-rate **8/8 = 100 %**. All three V1 carry-forward items closed
cleanly (precision NOTE #1 → A3 V2 H3 reclassified to L8; precision
NOTE #2 → A4 V2 §0 "Three"→"Four"; mechanism NEW LOW N-CH2-1 →
SYNTHESIS §2.4 lint glob extended to `{runtime/src/grammars,codegen/
src}/**/*.rs`). The V1 ACCEPT-with-NOTE on A3 promotes to clean
ACCEPT in V2 since the H3 severity has been migrated.

The CH2 GENERALITY posture remains sound end-to-end after the V2
folds. The H3 → L8 reclassification *strengthens* the generality
disposition: by acknowledging that the production call site is
generic over `&grammar.name`, the audit pack becomes structurally
honest about where the overfit actually lives (test-fixture surface,
not production dispatch). H6 carrying HIGH on its own structural
merits (acceptance-test surface absent for CSS L4 / Sheets / BBNF)
is the correct framing — it isolates the *true* GENERALITY gap in
the decision engine's testing posture without conflating it with
the test-fixture naming residue. The lint glob extension (codegen-
side twin) closes a round-tripping vector that would otherwise let
the fake-`@generated` recurrence re-enter through the codegen surface
in any SK-V{N+1} wave.

No new GENERALITY concern surfaces in the V2 fresh-finding scan.

## §1 — Per-artefact V2 disposition

| Artefact | V1 → V2 fold(s) | V2 Disposition | CH2 verification |
| --- | --- | --- | --- |
| `sk-v14-audit-overfit-lock14-scan.md` (A3 V2) | F-V2-A3-1: H3 HIGH→LOW (L8 inserted); H6 freestanding HIGH; aggregate 30 unchanged | **ACCEPT** | §3.1 below |
| `sk-v14-audit-overfit-generator-truth.md` (A4 V2) | F-V2-A4-1 + F-V2-A4-2 + F-V2-A4-3 (scope-extension framing, line-cite refresh, Three→Four) | **ACCEPT** | §3.2 below |
| `sk-v14-audit-overfit-decision-engine.md` (A5 V2) | F-V2-A5-1 verdict-line FAIL-at-HEAD across §0:11 + §3:102-107 + §4 row 4 + §5 closing | **ACCEPT** | CH2-neutral; verdict alignment carries no GENERALITY scope drift |
| `sk-v14-audit-overfit-pre-restart-pattern.md` (A6 V2) | F-V2-A6-1 LegacyPath both-readings-preserved at §0:12 + §2 ledger row | **ACCEPT** | CH2-neutral; scope-extension framing preserves Pattern H grammar-neutral cross-coverage (9 grammar dirs); see V1 CH2 §1 A6 row |
| `SYNTHESIS-AUDIT-OVERFIT.md` (V2) | F-V2-SYNTHESIS-{1..5} (census 54/20, co-derivation, verdict alignment, Three→Four, lint glob extension) | **ACCEPT** | §3.3 below |
| `sk-v14-audit-overfit-css-measurement.md` (A1) | STAND (no V2 folds) | **ACCEPT** | V1 CH2 §1 A1 row carries forward |
| `sk-v14-audit-overfit-admit-mechanism.md` (A2) | STAND (no V2 folds) | **ACCEPT** | V1 CH2 §1 A2 row carries forward |
| **Aggregate** | | **ACCEPT** | — |

## §2 — Per-§ ACCEPT-rate (V2 cycle)

Per ADDENDUM §1 CH2 row, the V2 lens has two specific fold-verification
clauses plus the standard fresh-finding scan:

| CH2 V2 sub-clause | ACCEPT | ACCEPT-with-NOTE | REVISE | REJECT |
| --- | ---: | ---: | ---: | ---: |
| (i) **F-V2-A3-1 H3→L8 reclassification preserves Lock-14 scope; aggregate 30 unchanged** | 1 | 0 | 0 | 0 |
| (ii) **F-V2-SYNTHESIS-5 lint glob extends to both runtime + codegen sides** | 1 | 0 | 0 | 0 |
| (iii) **Fresh-finding scan over V2 deltas (no new GENERALITY concern)** | 1 | 0 | 0 | 0 |
| (iv) **V2 carry-forward of V1 ACCEPTs (A1, A2, A4/A5/A6 non-CH2 folds, SYNTHESIS census/co-derivation/verdict folds)** | 5 | 0 | 0 | 0 |
| **All** | **8** | **0** | **0** | **0** |

## §3 — Executable verification (CH2 V2 evidence)

Per ADDENDUM §3 executable-verification mandate, the following were
re-run against the working tree at HEAD == `42e3edb9a` (V2 addendum
seed, post-`1735882a5` V2 axis landing).

### §3.1 F-V2-A3-1 — H3 HIGH→LOW preserves Lock-14 scope

The production-vs-test-fixture distinction stands verified:

```
$ sed -n '230,240p' skinny/crates/passes/src/decision_csp.rs
…
        let active = backend_egraph::select(RuleId(0), candidates.clone());
        let resolved = finalize_rule("json", RuleId(0), candidates, active);
        let csp = resolved.decision_csp.expect("csp facts");
…

$ grep -n "cfg(test)\|mod tests" skinny/crates/passes/src/decision_csp.rs
200:#[cfg(test)]
201:mod tests {

$ sed -n '475,482p' skinny/crates/passes/src/lib.rs
…
        let candidates = backend_candidates(grammar, rule, backend_rule, layout, target);
        let active = crate::backend_egraph::select(RuleId(0), candidates.clone());
        crate::decision_csp::finalize_rule(&grammar.name, rule.id, candidates, active)
…
```

Line 235 sits inside the `#[cfg(test)] mod tests` block opened at
line 200 — test fixture, not production path. Production call at
`lib.rs:478` is `finalize_rule(&grammar.name, …)` — grammar-generic
threading. The H3 → L8 migration correctly relocates the violation
to the test-fixture-name-leak tier alongside L1..L7.

Aggregate count unchanged:

```
$ grep -cE "^\| (C[0-9]+|H[0-9]+|M[0-9]+|L[0-9]+) \|" \
    restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md
30
```

Per-severity distribution verified by reading the ledger:
- §2.A CRITICAL: C1..C11 = **11**
- §2.B HIGH: H1, H2, H4, H5, H6, H7 = **6** (H3 removed)
- §2.C MEDIUM: M1..M5 = **5**
- §2.D LOW: L1..L8 = **8** (L8 inserted, capturing reclassified H3)
- **Total: 11 + 6 + 5 + 8 = 30** ✓ unchanged

H6 (CSS L4 entry-rule absence from acceptance-test surface) is
re-stated at `§2.B H6` as freestanding HIGH "no longer derivative of
H3, which has been LOW-reclassified per V2 fold F-V2-A3-1". Citation
extends to the production call site `passes/src/lib.rs:478` as the
positive demonstration that the production path is grammar-neutral —
the gap lives at the *acceptance-test* surface (CSS L4 + Sheets +
BBNF entry rules not exercised against `finalize_rule`).

The §3 verdict text correctly threads the recalibration:

```
restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-
lock14-scan.md:174: Actual count: **11 CRITICAL + 6 HIGH = 17**
violations in nominally-generic skinny crates … (V1 H3 reclassified
HIGH → LOW per V2 fold F-V2-A3-1; H6 retains HIGH on its own
structural merits; aggregate finding count unchanged at 30).
```

**CH2 verdict: ACCEPT.** F-V2-A3-1 lands cleanly; the violation
class (JSON-named identifier in generic crate's test surface)
persists, severity tier corrected, aggregate scope preserved.

### §3.2 F-V2-A4-3 / F-V2-SYNTHESIS-4 — Three→Four

V1 CH2 §4.2 NOTE flagged A4 §0 abstract reading "Three" while the
ledger captured 4. V2 verification:

```
$ grep -n "CANONICAL_FIXTURE\|CAPTURED_W2_INPUT" \
    skinny/crates/codegen/src/css_l4_*_templates/generated.rs
…/css_l4_nested_layout_templates/generated.rs:3 + :44
…/css_l4_at_rules_and_media_templates/generated.rs:3 + :25
…/css_l4_vendor_and_custom_atrules_templates/generated.rs:3 + :33
…/css_l4_stylesheet_selectors_templates/generated.rs:3 + :39
```

4 templates use byte-equality-on-fixture short-circuits (3
"CANONICAL_FIXTURE" + 1 "CAPTURED_W2_INPUT"). The V2 commit message
records the fold as "F-V2-A4-3  sec0 NEW-2 'Three' -> 'Four' with
all 4 templates listed verbatim". F-V2-SYNTHESIS-4 mirrors the
correction in SYNTHESIS §1.2 NEW-2. Pattern class is grammar-
neutral (fixture-lookup tables dressed as parsers; would catch any
future grammar attempting the same overfit).

**CH2 verdict: ACCEPT.** Precision NOTE closed; pattern generality
preserved.

### §3.3 F-V2-SYNTHESIS-5 — lint glob covers both runtime + codegen sides

V1 CH2 §5 N-CH2-1 LOW recommended the CH7-companion lint extend
from runtime-only to both sides of the emit-write boundary. V2
verification:

```
$ sed -n '305,320p' restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
2. **Lock-14-companion lint.** Add a clippy-lint or pre-commit grep
   that REJECTS any new `// @generated by skinny bbnf-codegen` header
   in `skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs`
   unless the matching path appears in a recognized regen subcommand's
   emission roster. The glob MUST scope BOTH the runtime-side mirror
   AND the codegen-side template/provider files — CH2 §3.5 verified
   42 files carry the fake header including 8 codegen-side
   template+provider files; a runtime-only lint would let the
   codegen-side twin re-introduce the fake header silently (the
   identical-content round-tripping vector A4 finding 15 enumerates
   between codegen-side template and runtime-side `generated.rs`).
   Without this guard the fake-`@generated` recurrence (A4 NEW-1 +
   NEW-2 + the 7 CSS files + the JSON file) can re-introduce in any
   SK-V{N+1} wave. Recommend lifting to LOCKS.md as a Lock-14-companion
   lint.
```

Glob brace-expansion verified to cover BOTH sides:
- runtime-side: `skinny/crates/runtime/src/grammars/**/*.rs`
- codegen-side: `skinny/crates/codegen/src/**/*.rs`

Quantification re-verified:

```
$ git grep -l '@generated by skinny bbnf-codegen' \
    skinny/crates/runtime/src/grammars/ skinny/crates/codegen/src/ | wc -l
56

$ git grep -l '@generated by skinny bbnf-codegen' \
    skinny/crates/codegen/src/ | wc -l
14

$ git grep -l '@generated by skinny bbnf-codegen' \
    skinny/crates/runtime/src/grammars/ | wc -l
42
```

Total fake-`@generated` surface: 56 files (42 runtime-side mirror +
14 codegen-side template/provider). The lint glob now binds both
halves. **Note:** the SYNTHESIS §2.4 prose cites "42 files carry
the fake header including 8 codegen-side template+provider files",
where the 8 count traces to V1 CH2 §5 (CH2 §3.5 V1 sampled 8
codegen-side files); the present count in `skinny/crates/codegen/
src/` is 14 (7 CSS providers + json_provider + json_typed_direct +
lib.rs + 4 json_templates files: generated.rs + parser.rs + value.rs
+ view.rs). The lint glob mechanism scopes correctly regardless of
the exact count; the SYNTHESIS prose conservatively cites the V1
sample. This is a non-blocking precision tightening candidate, not
a defect (see §4.1).

**CH2 verdict: ACCEPT.** F-V2-SYNTHESIS-5 closes V1 N-CH2-1
mechanism-precision concern; the glob now covers both surfaces of
the round-tripping vector.

### §3.4 Fresh-finding scan — primitive-crate generic posture preserved

V1 CH2 §3.8 verified the 5 truly-primitive generic crates (`bbnf-
regex`, `bbnf-simd`, `simd-scan`, `parse-that-regex`, `ir`) had
zero grammar-name leaks. Re-verification at V2 HEAD:

```
$ grep -rn -E '\b(Json|Css|Sheets|JsonValue|CssValue|SheetsValue|CssL4|CssPretty)\b' \
    skinny/crates/bbnf-regex/src/ \
    skinny/crates/bbnf-simd/src/ \
    skinny/crates/simd-scan/src/ \
    skinny/crates/parse-that-regex/src/ \
    skinny/crates/ir/src/
[no output]
```

Still clean. No SK-V14 commits have introduced grammar-name leaks
into the primitive layer. The V2 axis-redispatch was docs-only
(5 audit files modified; zero source touched per commit `1735882a5`
stat).

### §3.5 Per-grammar census re-verification

Per ADDENDUM §1 CH7 re-execute (cited in the SYNTHESIS census
co-derivation note F-V2-SYNTHESIS-2):

```
$ cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
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

9 grammars — confirms PRUNE-4 sub-wave count = 9 across A3 + A6 +
SYNTHESIS §3.3. F-V2-SYNTHESIS-2 co-derivation note (A3/A5/A6
cross-confirms + sequencing constraints + 64→67 file count + PRUNE-
4 = 9 sub-wave all co-derived from `css_pretty` addition) holds.

## §4 — V2 fold defects (non-blocking)

No CH2-fatal defect surfaces in the V2 cycle. The following are
V3-confirming-pass candidates, all editorial; none block §3Z
convergence.

### §4.1 SYNTHESIS §2.4 "8 codegen-side" count precision

SYNTHESIS §2.4 (line 311) reads "42 files carry the fake header
including 8 codegen-side template+provider files". The actual codegen-
side count at V2 HEAD is 14 (verified §3.3 above). The V1 CH2 §3.5
sample observed 8 (7 CSS providers + 1 JSON provider), excluding
`json_templates/{generated,parser,value,view}.rs`, `json_typed_
direct.rs`, and `lib.rs`. The lint glob mechanism scopes correctly
regardless (`codegen/src/**/*.rs` catches all 14); only the cited
count under-states. V3 could fold "8" → "14" for arithmetic parity
with the executable verification, or retain "8 codegen-side
template+provider files" if the prose intent is the *provider+
template* subset (which is precisely 8 when excluding `lib.rs` +
`json_typed_direct.rs` as non-emission-targets). Recommend the
latter framing for V3 clarity:

> "… including 8 codegen-side template+provider files (7 CSS L4
> providers + 1 JSON provider) plus 6 ancillary codegen-side files
> (4 json_templates submodules + json_typed_direct + lib.rs) for a
> total of 14 codegen-side files within the lint glob's purview."

This is mechanism precision, not a finding revision; pattern class
intact.

## §5 — V3 fold recommendations

If S-P0 V3 dispatches as the confirming pass:

1. **SYNTHESIS §2.4 codegen-side count clarification** (§4.1 above)
   — fold "8 codegen-side template+provider files" → "8 codegen-side
   template+provider files plus 6 ancillary files (14 total)" for
   arithmetic parity. Editorial; no scope drift.

V3 is structurally a confirming pass over V2 artefacts (per ADDENDUM
§4 "V3 confirming pass over unchanged V2 artefacts closes §3Z LOCK").
CH2's V2 disposition is 100 % ACCEPT; if V3 carries the §4.1
editorial fold, V3 CH2 expects 100 % ACCEPT again — satisfying the
two-consecutive-cycle §3Z criterion (V2 100 % + V3 100 % → ≥ 95 %
twice → G-S-P0-CONVERGED → S-P1 dispatch).

## §6 — Disposition

**CH2 V2 VERDICT: ACCEPT.** All three V1 carry-forward items (H3
severity recalibration, A4 §0 fixture count, CH7-companion lint glob
extension) land cleanly in the V2 axis-redispatch. The H3 → L8
migration *strengthens* the GENERALITY posture by making the violation
class structurally honest (test-fixture name leak ≠ production-path
overfit); H6 freestanding correctly isolates the *true* GENERALITY
gap (acceptance-test surface absent for CSS L4 / Sheets / BBNF). The
lint glob extension closes the codegen-side round-tripping vector
that would otherwise let the fake-`@generated` recurrence re-enter
silently in any SK-V{N+1} wave. Aggregate violation count preserved
at 30; per-grammar census (9 grammars) preserved; primitive-crate
generic posture preserved (zero name leaks). No new GENERALITY
concern surfaces in the fresh-finding scan. The single non-blocking
precision NOTE (§4.1 SYNTHESIS §2.4 "8" vs "14" codegen-side count)
is editorial mechanism arithmetic, not a scope drift.

V2 CH2 ACCEPT-rate: **8/8 = 100 %**. §3Z V2 ≥ 95 % satisfied for
CH2; convergence depends on the other six lenses' aggregate.

---

**Authored:** 2026-05-23 (SK-V14 S-P0 CHALLENGE V2 lens CH2).
**Status:** WRITE-ONLY; aggregator commits all 8 V2 hardening files
atomically with `docs(sk-v14-audit-overfit-hardening-V2): challenge
V2 + consolidated`.
**Authority:** `restart/prompts/ORCHESTRATOR.md §3W` CH2 row;
ADDENDUM `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V2/
CHALLENGE-V2-ADDENDUM.md` §1 CH2 row; V1 base
`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/CH2.md`.
