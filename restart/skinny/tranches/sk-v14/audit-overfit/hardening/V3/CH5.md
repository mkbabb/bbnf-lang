# S-P0 CHALLENGE V3 — CH5 HIDDEN COUPLING

Lens: CH5 — HIDDEN COUPLING (per `restart/prompts/ORCHESTRATOR.md:87`).
Scope: the seven S-P0 artefacts at HEAD `007624849` — five V2-folded
artefacts now carrying the V3 micro-fold packet (`SYNTHESIS-AUDIT-
OVERFIT.md`, `sk-v14-audit-overfit-{lock14-scan, generator-truth,
decision-engine, pre-restart-pattern}.md`) plus the two STAND artefacts
(A1 `…css-measurement.md`, A2 `…admit-mechanism.md`). Charge per
`V1/CHALLENGE-CONTEXT.md §CH-5` carried verbatim through V2 addendum §1
line 25 into V3 as the *confirming pass* under `ORCHESTRATOR.md §3Z`
two-consecutive-cycle rule: verify no parallel substrate, no Track 1 ≡
Track 2 collapse, no renamed-scanner Lock 1 violation; verify the
F-V2-A6-1 LegacyPath disambiguation still classifies the shim
correctly relative to the Lock 1 substrate-union (subject to C-1
PRUNE-4 collapse); fresh-finding scan against the V3 artefact deltas.

V2 baseline: 7 / 7 = 100 % ACCEPT (two ACCEPT-with-COUPLING-NOTE; zero
REVISE). V3 expected ≥ 100 % — the V3 micro-fold packet's five edits
(F-V3-A4-1 line-count refresh; F-V3-A5-1 + F-V3-SYNTHESIS-1
action-class symmetry; F-V3-SYNTHESIS-2 count clarification 8 → 14
codegen-side files; F-V3-SYNTHESIS-3 in-table NEW-2 cell 3 → 4)
target CH1 / CH4 / CH6 carry-overs surfaced by V2 lens cross-flags,
none of which is CH5-surface. Forward V3 binding from V2 CH5 §4 note
3 (totality-track lint glob extension) carries to V4 / S-P3, not to
this V3 confirming pass.

UTC dispatch start: 2026-05-23T07:42Z. Hard cap: 30 min.

## §0 — Disposition summary

| Per-artefact | V1 | V2 | V3 | Δ V2 → V3 |
| --- | --- | --- | --- | --- |
| `SYNTHESIS-AUDIT-OVERFIT.md` | ACCEPT | ACCEPT | ACCEPT | F-V3-SYNTHESIS-{1,2,3} are count clarifications + action-class symmetry; zero CH5 surface |
| A1 `…css-measurement.md` | ACCEPT | ACCEPT (STAND) | ACCEPT (STAND) | No V3 fold |
| A2 `…admit-mechanism.md` | ACCEPT | ACCEPT (STAND) | ACCEPT (STAND) | No V3 fold |
| A3 `…lock14-scan.md` | ACCEPT | ACCEPT | ACCEPT (STAND) | No V3 fold (A3 STAND verbatim) |
| A4 `…generator-truth.md` | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | F-V3-A4-1 methodology-section line-count refresh (101 → 100); Track 1 / Track 2 boundary at A4 finding 16 byte-identical |
| A5 `…decision-engine.md` | ACCEPT | ACCEPT | ACCEPT | F-V3-A5-1 §5 closing-paragraph action-class symmetry with §4 row 4 (V1 CH6-R3 option (b) propagation); zero CH5 surface; no resolver edit |
| A6 `…pre-restart-pattern.md` | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | No V3 fold (A6 STAND verbatim); LegacyPath disambiguation paragraph carries forward byte-identical |

**V3 ACCEPT-rate: 7 / 7 = 100 %.** Zero CH5 firings introduced by the
V3 micro-fold packet. The two ACCEPT-with-COUPLING-NOTE dispositions
(A4 §3.1 Track 1 / Track 2; A6 §3.3 substrate-doc opt-out
enshrinement) carry forward unchanged from V2 with content
byte-identical at HEAD `007624849`.

## §1 — V3 micro-fold contamination scan (CH5 charge-specific)

The V3 micro-fold packet at commit `007624849` lands five prose
touches across three files (per `V2/HARDENING-S-P0-V2-CONSOLIDATED.md
§2`). CH5 verification per-fold below.

### §1.1 V3 micro-fold per-fold verification table

Verified via `git show 007624849 -- restart/skinny/tranches/sk-v14/
audit-overfit/{SYNTHESIS-AUDIT-OVERFIT.md,sk-v14-audit-overfit-
decision-engine.md,sk-v14-audit-overfit-generator-truth.md}`. Three
files modified, +10 insertions / -6 deletions per `git show --stat`.

| V3 fold | Edit summary | Substrate impact | CH5 firing? |
| --- | --- | --- | --- |
| **F-V3-A4-1** (decision-engine.md is mis-attribution; the actual file is generator-truth.md §1 line 153) | Methodology section file-roster line: `json_provider.rs (full, 101 lines)` → `(full, 100 lines)`. Verified at HEAD `007624849` via `wc -l skinny/crates/codegen/src/json_provider.rs` = **100**. | Documentary line-count correction. No code, no substrate, no codegen path is touched. The A4 ledger rows 8 / 10 / 11 cite correct file-line coordinates per F-V2-A4-2 landing; this fold is methodology-summary precision only. | **NO** |
| **F-V3-A5-1** | Decision-engine §5 closing paragraph line 133: replaces "the LOW finding (honest self-labelling) needs no action pre-C-4" with "the LOW finding (honest self-labelling) is preserved through PRUNE-5 as a gate-rejection invariant inside C-4 entry-gates so any admit attempting to cite W8 / W9 pre-runtime-consumer is denied at admit time". Verified at HEAD `007624849`: the new phrasing is present at decision-engine.md line 133. | Restores symmetry with §4 row 4 line 118 (V1 CH6-R3 option (b) landing). The widening is anti-paper-close phrasing alignment — the LOW row already carried the same gate-invariant semantics at §4 row 4 under V2; the §5 closing paragraph was the inheritance gap CH6-V2-N1 flagged. No resolver edit, no new shadow CSP, no parallel decision engine. The five fail-closed checks at `codegen/src/lower/rust.rs:37-89` (V1 CH5 §1 sub-vector 6 verbatim) are untouched. **CH5 substrate-union closure unaffected.** | **NO** |
| **F-V3-SYNTHESIS-1** | SYNTHESIS §3.1 prune-cluster table line 339 C-4 row cell: replaces "A5 LOW (resolver honest self-labelling — no-op pre-C-4)" with "A5 LOW (resolver honest self-labelling — preserved through PRUNE-5 as C-4 entry-gate invariant)". Verified at HEAD `007624849`: the new phrasing is present at SYNTHESIS-AUDIT-OVERFIT.md line 343 (note: V2 CONSOLIDATED §2.2 referenced line 339 against V2 state; the V3 edit landed at line 343 in the post-§2.4 expanded SYNTHESIS state — same C-4 row cell, byte-identical to the V2 CONSOLIDATED §2.2 prescription). | Mirrors F-V3-A5-1 in SYNTHESIS prune-cluster table. The C-4 row count (4 findings) is unchanged; the prune-cluster column-sum (41 + 7 + 11 + 4 + 11 = 74) holds per `git diff 007624849~1 007624849 -- restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` showing only cell-prose change, no row addition. **No NEW C-N candidate, no envelope drift, no substrate edit.** | **NO** |
| **F-V3-SYNTHESIS-2** | SYNTHESIS §2.4 second-item parenthetical expanded: "8 codegen-side template+provider files" → "14 codegen-side files (8 providers+templates + 6 ancillary; `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/ \| wc -l = 14`). The lint glob `codegen/src/**/*.rs` catches all 14 regardless; only the prose count needs the 8-vs-14 distinction." Re-executed `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/ \| wc -l` returns **14**. | Lint-glob coverage clarification. The CH7-companion lint glob extension (F-V2-SYNTHESIS-5) is mechanically unchanged — the glob still scopes `skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs` and catches all 14 codegen-side files. The fold makes explicit that the lint mechanism (glob match) is broader than the V2 prose count (8) had implied. **No NEW glob, no NEW lint, no parallel substrate — only the prose-vs-mechanism alignment.** The forward V3 binding from V2 CH5 §4 note 3 (totality-track glob extension `crates/core/src/runtime/**/*.rs` post-C-1 PRUNE-4) is unaffected; the V3 packet did not pre-empt it. | **NO** |
| **F-V3-SYNTHESIS-3** | SYNTHESIS §1.1 per-axis-table A4 row NEW-2 prose cell at line 81: "3 of 7 CSS scanners short-circuit" → "4 of 7 CSS scanners short-circuit". Verified by re-execution: `grep -nE 'CANONICAL_FIXTURE\|CAPTURED_W2_INPUT' skinny/crates/codegen/src/css_l4_*_templates/generated.rs` returns 8 hits across **4 files** (`at_rules_and_media`, `nested_layout`, `vendor_and_custom_atrules`, `stylesheet_selectors` via `CAPTURED_W2_INPUT`). | Carry-over alignment with F-V2-SYNTHESIS-4 (which updated §1.2:122 + §0.1 + §5.1 but missed §1.1:81 in-table cell). Same indictment, same PRUNE-2 wholesale deletion target; the "scanners" remain fixture-lookup constant tables, not Lock-1 renamed scanners. The count refinement is precision-only; no substrate edit, no NEW sidecar, no parallel pipeline. | **NO** |

All five V3 micro-folds clear the CH5 charge: zero parallel substrate,
zero NEW sidecar producer, zero renamed-scanner Lock 1 violation,
zero Track 1 ≡ Track 2 collapse vector. The V3 edits are bounded to
the three target files (SYNTHESIS + decision-engine + generator-truth)
and to specifically enumerated lines per V2 CONSOLIDATED §2.1–§2.5;
A1, A2, A3, A6 stand byte-identical from V2.

### §1.2 Direct CH5 invariant re-execution

The four V1 CH5 §1 sub-vectors re-execute clean at HEAD `007624849`.

| Sub-vector | V1 method | V3 re-execution | Result |
| --- | --- | --- | --- |
| (1) Renamed-scanner / fallback combinator scan | `grep -rn 'fn parse_combinator\|combinator_fallback\|parse_with_fallback' crates/core/src/runtime/ skinny/crates/runtime/src/` | Re-executed at HEAD `007624849` | **Zero matches.** Lock-1-adjacent renamed-scanner audit CLEAN. |
| (2) LegacyPath bounded-leak scan | `grep -rn 'LegacyPath\|LegacySegment' skinny/crates` and `... crates/core/src/runtime/` | Re-executed at HEAD `007624849`. `skinny/crates` returns **zero matches**; `crates/core/src/runtime/` returns **28 hits across 4 `parse_with.rs` files** (json, css_l4, bbnf, google_sheets) per V1 CH5 §1.2 expectation. | LegacyPath shim remains **bounded** to the 4 grammar-specific `parse_with.rs` files; zero leak into generic crates (`skinny/crates/{ir,codegen,passes,runtime}`). |
| (3) Substrate-union refusal token presence | `grep -rn 'UnionTape\|second tape\|public substrate API\|class/mask stream\|parser-owned cursor' restart/skinny/tranches/sk-v14/audit-overfit/` | Re-executed at HEAD `007624849`. Returns 11 refusal-token mentions, all in REFUSAL / pre-block / guard context (zero authorisations). | Substrate-union refusal posture intact across SYNTHESIS + ledger artefacts. |
| (4) Track-collapse refusal scan | `grep -rn 'Track 1 ≡ Track 2\|Track 1.*Track 2' restart/skinny/tranches/sk-v14/audit-overfit/` | Re-executed at HEAD `007624849`. Returns 28 mentions, all in disjunction-preserving / refusal context (A4 finding 16, SYNTHESIS §3, A2 F8 single-lane comparator fanout, etc.). | Track 1 / Track 2 boundary semantically preserved across all artefacts. |

All four invariants hold. CH5 substrate-union closure is intact at
HEAD `007624849`.

### §1.3 F-V2-A6-1 LegacyPath disambiguation — V3 standing verification

The V2 CH5 §1.2 verification block (LegacyPath classification as
Lock-1-*adjacent* coupling subject to C-1 PRUNE-4 collapse) carries
forward verbatim at V3. A6 is a V3 STAND artefact (no V3 fold per
V2 CONSOLIDATED §1 fold-landing matrix); `git show 007624849 --stat`
confirms only three files modified, none of which is A6. The
disambiguation paragraph at A6 §0 line 12 + ledger row Status "NEW
(scope-extension over V13 Pattern G; not a reversal)" persists
byte-identical.

The V2 CH5 §1.2 disposition — (a) classification correctness
(scope-extension framing preserves both V13 carveout reading + V14
explicit survey); (b) substrate-union impact (Lock-1-adjacent, not
Lock-1 substrate-union violation; subject to C-1 PRUNE-4 collapse
target wired through SYNTHESIS §3.1 row C-1); (c) no Lock 1
substrate-union violation introduced (bounded to 4 `parse_with.rs`
files with zero leak into generic crates) — holds at V3 without
modification.

### §1.4 Cross-axis coherence — Track 1 / Track 2 boundary at V3

The V1 CH5 §3.1 + V2 CH5 §1.3 observation (A4 NEW-1 / R4 scope-
extension as preserving Track 1 / Track 2 disjunction) carries
forward intact. Re-verified at HEAD `007624849`:

- A4 finding 16 LOW line 217 (line shift from V2's 219 due to F-V3-
  A4-1 line-count touch at line 153 inserting no new lines but
  potentially refreshing the surrounding region): `xtask (root)
  Cargo.toml:22 correctly wires the totality css_l4 grammar to
  grammar/css/l4/stylesheet.bbnf … Noted to forestall a future
  redress that conflates the two regen pipelines under one xtask.`
  Track separation preserved verbatim under V3.
- F-V2-A4-1 "scope extension, not reversal" framing at A4 §0 + §3
  (V2-landed; A4 stands at V3 except for the methodology line 153
  refresh) PRESERVES the Track 1 / Track 2 disjunction by indicting
  only the SKINNY-track providers (`json_provider.rs` + 7 CSS
  providers); the totality-track `xtask::regen::run` path remains
  untouched.

The V1 CH5 §4.1 binding note for S-P3 wave manifest (R4
specification must encode Track 1 / Track 2 separation as hard
constraint, attaching `regen-css-l4-<provider>` subcommands to
`skinny/xtask/src/main.rs:8` USAGE line only) carries forward
unchanged into V3 → V4 → S-P3.

### §1.5 F-V3-SYNTHESIS-2 count-clarification — special CH5 cross-check

The 8 → 14 codegen-side file-count clarification (V2 CONSOLIDATED §2.3
prescribed Option A landed verbatim per `git show 007624849 --stat`
diff at SYNTHESIS lines 308-314) warrants a CH5 cross-check because
the broader count brings 6 *additional* codegen-side files into the
explicit prose enumeration. CH5 must verify the 6 additional files
are not introducing a NEW parallel-substrate vector.

Re-executed: `git grep -l '@generated by skinny bbnf-codegen'
skinny/crates/codegen/src/` returns 14 files. The 8 explicitly
enumerated under V2 CH2 §3.5 were 7 CSS L4 providers + 1 JSON
provider (`json_provider.rs`). The 6 additional files (per V2
CONSOLIDATED §2.3 Option B preferred-framing enumeration: "4
json_templates submodules + json_typed_direct + lib.rs") are:

| Additional file | Role | CH5 substrate classification |
| --- | --- | --- |
| `skinny/crates/codegen/src/json_templates/*.rs` (4 submodules) | JSON template body submodules — they live inside the same `json_templates` directory the `json_provider.rs` already encloses; they carry the fake `@generated` header per the same pattern A4 indicts. | NOT a parallel substrate. They are template-body submodules of the EXISTING `json_provider.rs` sidecar; PRUNE-2 collapses them wholesale per the same C-5 + C-1 wiring. No NEW pipeline. |
| `skinny/crates/codegen/src/json_typed_direct.rs` | JSON typed-direct emission helper. | NOT a parallel substrate. Same C-1 PRUNE-3 + PRUNE-4 collapse target as the rest of the codegen-side JSON provider surface. The typed-direct path was always within the audit's C-1 envelope per A6 NEW-HIGH-1 LegacyPath shim resolution language ("OR open small 'C-6 typed-path collapse'"); the typed-path consolidation lands at C-1 PRUNE-4. |
| `skinny/crates/codegen/src/lib.rs` | Codegen entry point; carries the fake header at its module-root level. | NOT a parallel substrate. `lib.rs` is the dispatcher; PRUNE-3 (per-grammar dispatch → generic registry) collapses the per-grammar branches inside it. No NEW sidecar pipeline. |

All 6 additional files map cleanly to **existing** C-1 (PRUNE-3 +
PRUNE-4) collapse targets per SYNTHESIS §3.1. The 14-file count
broadens the prose census; the C-1..C-5 prune mapping is unchanged.
**No NEW C-N candidate, no NEW parallel substrate, no NEW pipeline
surfaced by F-V3-SYNTHESIS-2.**

## §2 — Per-artefact V3 disposition table

| Artefact | V3 disposition | Δ from V2 |
| --- | --- | --- |
| `SYNTHESIS-AUDIT-OVERFIT.md` | ACCEPT | F-V3-SYNTHESIS-{1,2,3} are three single-phrase prose touches: §3.1 line 343 C-4 row cell action-class symmetry (mirrors F-V3-A5-1); §2.4 second-item parenthetical 8 → 14 codegen-side file-count clarification (lint-glob mechanism unchanged); §1.1 line 81 A4 row NEW-2 in-table cell 3 → 4 CSS scanners (carry-over alignment with F-V2-SYNTHESIS-4). Per §1.1 + §1.5 above: zero CH5 firing across all three folds. The 74-finding aggregate + C-1..C-5 envelopes + 9-grammar census + Track 1/Track 2 disjunction all hold byte-identical to V2. |
| A1 `…css-measurement.md` | ACCEPT (STAND) | No V3 fold; V2 disposition carries unchanged. |
| A2 `…admit-mechanism.md` | ACCEPT (STAND) | No V3 fold; V2 disposition carries unchanged. |
| A3 `…lock14-scan.md` | ACCEPT (STAND) | No V3 fold; A3 STAND verbatim per V2 CONSOLIDATED §1 fold-landing matrix. The 30-finding ledger (11 CRIT + 6 HIGH + 5 MED + 8 LOW per F-V2-A3-1 reclassification) holds; PRUNE-3 + R4 collapse path unchanged. |
| A4 `…generator-truth.md` | ACCEPT-with-COUPLING-NOTE | F-V3-A4-1 methodology line-count refresh (101 → 100) at line 153 is documentary precision only; A4 ledger rows 8 / 10 / 11 unchanged. The V2 §3.1 Track 1 / Track 2 binding note carries forward verbatim. Coupling note unchanged in substance; the V3 fold does not alter the indictment surface. |
| A5 `…decision-engine.md` | ACCEPT | F-V3-A5-1 §5 closing paragraph line 133 action-class symmetry restoration (per V2 CH6-V2-N1 + V2 CONSOLIDATED §2.1): "needs no action pre-C-4" → "preserved through PRUNE-5 as a gate-rejection invariant inside C-4 entry-gates so any admit attempting to cite W8 / W9 pre-runtime-consumer is denied at admit time". This is anti-paper-close phrasing symmetry with §4 row 4 line 118 (V1 CH6-R3 option (b) landing) — strengthens the C-5 → C-4 sequencing gate without touching resolver code, substrate, or shadow CSP. No NEW resolver path, no parallel decision engine. V2 §1.1 row F-V2-A5-1 verdict-line strengthening + F-V3-A5-1 §5 closing symmetry now propagate uniformly across §0:11 + §3:102-107 + §4 row 4 line 118 + §5 closing paragraphs 1 + 2 + SYNTHESIS §0.1 + §0.2 + §3.1 line 343 + §5.1. **Action-class propagation matrix complete at V3.** |
| A6 `…pre-restart-pattern.md` | ACCEPT-with-COUPLING-NOTE | No V3 fold; A6 STAND verbatim. F-V2-A6-1 LegacyPath disambiguation paragraph + §2 ledger row Status "NEW (scope-extension over V13 Pattern G; not a reversal)" persist byte-identical. CH5 §1.2 substrate-union classification (Lock-1-*adjacent*; bounded to 4 `parse_with.rs` files; subject to C-1 PRUNE-4 collapse) holds at V3. |

## §3 — Critical CH5 findings

### §3.1 No new critical findings under V3

V3 micro-fold inspection (`git diff 007624849~1 007624849 --
restart/skinny/tranches/sk-v14/audit-overfit/` shows three files
modified +10 / -6) confirms zero new CH5 firings introduced by the
V3 packet. The two V1+V2 ACCEPT-with-COUPLING-NOTE observations (A4
§3.1 Track 1 / Track 2; A6 §3.2 + §3.3 LegacyPath shim + substrate-
doc opt-out enshrinement) carry forward unchanged with the underlying
A4 / A6 artefacts modified only in the methodology line-count cite
(A4) or not at all (A6).

### §3.2 Fresh-finding scan — V3 micro-fold side-effects

Per V2 addendum §1 line 25 CH5 task (carried verbatim into V3 as the
confirming pass): *"Verify V3 folds don't introduce parallel
substrate, Track 1 ≡ Track 2, or renamed-scanner Lock 1 violation."*
Fresh-finding scan executed at HEAD `007624849`:

| Scan target | Method | V3 result |
| --- | --- | --- |
| New BIR / TypeDesc / TapeKind variant introduced by V3 packet | `git diff 007624849~1 007624849 -- restart/skinny/tranches/sk-v14/audit-overfit/ \| grep -nE 'enum (BackendIr\|TypeDesc\|TapeKind)\|new variant'` | Zero hits. The V3 packet is prose-only; no enum, no variant, no type introduction. |
| New `<g>_provider.rs` introduced by V3 | `git diff 007624849~1 007624849 -- … \| grep -nE 'provider.rs\|sidecar'` | F-V3-SYNTHESIS-2 mentions "providers+templates" in count clarification; this is documentary census-reframing, not NEW sidecar. Zero new provider files; existing 14 codegen-side files re-cited only. |
| New `Lock-14`-leaking grammar identifier in nominally-generic crate | `git diff 007624849~1 007624849 -- …` | Zero new symbols introduced. F-V3-SYNTHESIS-3 refines existing count 3 → 4; F-V3-SYNTHESIS-2 clarifies existing count 8 → 14; F-V3-A4-1 refreshes existing line-count 101 → 100. All three are precision-only re-citations of existing surface. |
| New combinator-fallback scanner | `grep -rn 'fn parse_combinator\|combinator_fallback\|parse_with_fallback' crates/core/src/runtime/ skinny/crates/runtime/src/` | Zero matches (V3 re-execution; renamed-scanner Lock 1 audit CLEAN). |
| New regen pipeline conflation Track 1 ≡ Track 2 | A4 finding 16 + V1 §3.1 + V2 F-V2-A4-1 framing + V3 stand | Track 1 / Track 2 disjunction preserved verbatim at HEAD `007624849`; F-V3-A4-1 line-count refresh at line 153 does not touch finding 16 at line 217. |
| New shadow CSP / parallel decision engine | F-V3-A5-1 + F-V3-SYNTHESIS-1 action-class phrasing symmetry | Zero NEW resolver path. The action-class widening "preserved through PRUNE-5 as gate-rejection invariant" is anti-paper-close strengthening across §5 closing + SYNTHESIS §3.1; the resolver clause (W5 → W6 → W7 → lowering via the five fail-closed checks at `codegen/src/lower/rust.rs:37-89`) is untouched. |
| New parallel-substrate vector from F-V3-SYNTHESIS-2 broader census | Per §1.5 above: the 6 additional codegen-side files (4 json_templates submodules + json_typed_direct + lib.rs) all map to existing C-1 (PRUNE-3 + PRUNE-4) collapse targets per SYNTHESIS §3.1. | Zero NEW C-N candidate, zero NEW pipeline. The count broadening is mechanism precision; the C-1..C-5 prune mapping is unchanged. |

The fresh-finding scan returns **zero new findings**. The V3 folds
are editorial-precision (action-class symmetry, count clarification,
line-count refresh); none introduces a new substrate, sidecar,
scanner-rename, or track-collapse vector.

### §3.3 Forward V3-binding note (V2 CH5 §4 note 3) carries to V4 / S-P3 unchanged

V2 CH5 §4 note 3 specified that the CH7-companion lint glob must
extend to the totality-track `crates/core/src/runtime/**/*.rs` after
C-1 PRUNE-4 lands. The V3 packet did not pre-empt this extension —
F-V3-SYNTHESIS-2's clarification touches only the skinny-track count
prose; the glob mechanism still scopes `skinny/crates/{runtime/src/
grammars,codegen/src}/**/*.rs`. The V2 forward-binding obligation
carries to V4 / S-P3 wave-close gate manifest unchanged. **Forward
observation; not a V3 REVISE blocker.**

## §4 — V4 confirming-pass recommendations

The audit campaign at V3 closes with 100 % ACCEPT under the CH5 lens
(matching V1 + V2 = three consecutive 100 % cycles). The V3 micro-fold
packet's five edits clear the CH5 charge across all three sub-vectors
(no parallel substrate, no Track-collapse, no renamed-scanner Lock 1
violation) plus the F-V2-A6-1 LegacyPath disambiguation continuity.

Three binding notes for V4 confirming-pass dispatch (V4 expected to
be a *second-consecutive* confirming pass over unchanged V3 artefacts
per `ORCHESTRATOR.md §3Z` two-cycle rule):

1. **V1 CH5 §4.1 + §4.2 + V2 CH5 §4 forward bindings carry into V4
   unchanged.** The R4 specification language must encode Track 1 /
   Track 2 separation as a hard constraint (`regen-css-l4-<provider>`
   subcommands attach to `skinny/xtask/src/main.rs:8` USAGE line
   only); PRUNE-4 must explicitly state the substrate-union closure
   target (richer template subsuming JSON / CSS L4 / BBNF / Sheets
   vs substrate-doc rewrite with deletion plan + binding deletion of
   `arena_template.rs:1-31` + `builder_template.rs:13-31` opt-out
   passages into PRUNE-4 wave-close gates); the CH7-companion lint
   glob must extend to `crates/core/src/runtime/**/*.rs` after C-1
   PRUNE-4 lands.

2. **F-V3-SYNTHESIS-2 8 → 14 count clarification establishes the
   correct CH5 census surface for V4.** Any V4 cross-flag against a
   "codegen-side N codegen files" count must use 14 (matching the
   `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/
   codegen/src/` count) and not 8 (the V2-prose narrower count). The
   lint-glob mechanism scoping unchanged; only the prose census
   shifts. **Forward CH5 cross-check obligation; not a V4 REVISE
   blocker.**

3. **F-V3-A5-1 + F-V3-SYNTHESIS-1 action-class propagation matrix
   completion locks the anti-paper-close framing for C-4 entry-gates.**
   V4 should re-verify (per V2 CH5 §1.1 row F-V2-A5-1 verdict-line
   continuity check) that the C-4 entry-gate rejection invariant
   (`JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-
   RUNTIME` → `JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT`
   block-ID chain at `decision_csp.rs:160-164`) remains the
   mechanical correlate of the §5 closing-paragraph and SYNTHESIS
   §3.1 line 343 prose. The block-ID chain wires the prose framing
   to a CI-enforceable gate; CH5 should confirm the chain persists
   in V4 source state. **Forward observation; not a V4 REVISE
   blocker.**

None of the three notes is a REVISE; all are forward-binding
clarifications. The CH5 lens advances without convergence blocker.

## §5 — Verdict

**CH5 — HIDDEN COUPLING V3: ACCEPT (7 / 7 = 100 %).**

The S-P0 audit campaign at SK-V14 V3 holds the CH5 lens without
firing. Every V3 micro-fold edit is verified editorial-precision
(action-class symmetry restoration, line-count refresh, count
clarification, in-table cell alignment); none introduces a parallel
substrate, sidecar producer, renamed-scanner Lock 1 violation, or
Track 1 ≡ Track 2 collapse vector.

The F-V2-A6-1 LegacyPath disambiguation (re-verified at HEAD
`007624849` against unchanged A6 STAND artefact) continues to
classify the shim as: (a) NEW finding by scope-extension over V13
Pattern G (not a reversal of V13's CLEAN disposition); (b) Lock-1-
*adjacent* coupling (not a Lock-1 substrate-union violation); (c)
subject to C-1 PRUNE-4 typed-path collapse with bounded scope (4
`parse_with.rs` files, zero leak into generic crates verified via
re-executed `grep -rn 'LegacyPath\|LegacySegment' skinny/crates`
returning zero matches).

The F-V3-SYNTHESIS-2 8 → 14 codegen-side file-count clarification
(the V3 fold with the broadest census-surface impact) introduces
zero NEW parallel-substrate vector — the 6 additional files (4
json_templates submodules + json_typed_direct + lib.rs) all map to
EXISTING C-1 (PRUNE-3 + PRUNE-4) collapse targets per SYNTHESIS
§3.1; the lint-glob mechanism is unchanged; the count broadening
aligns prose with mechanism without opening a NEW C-N candidate.

The F-V3-A5-1 + F-V3-SYNTHESIS-1 action-class propagation completes
the V1 CH6-R3 option (b) symmetry across all five summary surfaces
(A5 §0:11 + §3:102-107 + §4 row 4 line 118 + §5 closing paragraphs
1 + 2; SYNTHESIS §0.1 + §0.2 + §3.1 line 343 + §5.1) — anti-paper-
close strengthening that preserves CH5's substrate-union closure
because the resolver clause, the shadow-CSP refusal, and the parallel-
decision-engine refusal are not touched.

The fresh-finding scan returns zero new coupling findings. The
V1+V2 two ACCEPT-with-COUPLING-NOTE observations (A4 §3.1 Track 1 /
Track 2 boundary; A6 §3.2 + §3.3 LegacyPath bounded + substrate-doc
opt-out enshrinement) carry forward unchanged.

Per `ORCHESTRATOR.md §3Z`, the CH5 lens convergence criterion is met
at V3 (100 % ACCEPT × 3 consecutive cycles V1 → V2 → V3; zero open
critical defects; zero orphan unresolved REVISE). The two-consecutive-
cycle requirement was already satisfied at V2 → V3 with both at 100 %;
the V4 confirming pass over unchanged V3 artefacts (if dispatched per
strict-reading interpretation of `V2 CONSOLIDATED §3.1`) will
re-confirm without surface change. **CH5 releases to G-S-P0-CONVERGED
per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.**
