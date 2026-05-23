# S-P0 CHALLENGE V5 — CH5 HIDDEN COUPLING

Lens: CH5 — HIDDEN COUPLING (per `restart/prompts/ORCHESTRATOR.md:87`).
Scope: the seven S-P0 artefacts at HEAD `037eec6b6` — six STAND
artefacts unchanged from V4 (A1 `…css-measurement.md`, A2 `…admit-
mechanism.md`, A3 `…lock14-scan.md`, A4 `…generator-truth.md`, A5
`…decision-engine.md`, A6 `…pre-restart-pattern.md`) plus
`SYNTHESIS-AUDIT-OVERFIT.md` carrying the V5 micro-fold packet
(F-V5-SYNTHESIS-1{a,b}). Charge per V1 `CHALLENGE-CONTEXT.md §CH-5`,
carried verbatim through V2 addendum §1 line 25, V3 confirming-pass,
V4 confirming-pass into V5 as the *fifth consecutive* confirming pass
under `ORCHESTRATOR.md §3Z` two-consecutive-cycle rule at V max=5
ceiling: verify no parallel substrate, no Track 1 ≡ Track 2 collapse,
no renamed-scanner Lock 1 violation; verify the F-V2-A6-1 LegacyPath
disambiguation still classifies the shim correctly; fresh-finding
scan against the V5 artefact deltas.

V4 baseline: 7 / 7 = 100 % ACCEPT (two ACCEPT-with-COUPLING-NOTE;
zero REVISE), matching V1 + V2 + V3 for four consecutive 100 % cycles.
V5 expected ≥ 100 % — the V5 micro-fold packet (single-file commit
`037eec6b6` per `git show 037eec6b6 --stat`: SYNTHESIS-AUDIT-
OVERFIT.md, +4 / -3) lands two prose touches at §2.4 lines 318-321,
both belt-and-braces sub-threshold closures authorised by V4
CONSOLIDATED §2.3 Option B recommendation. F-V5-SYNTHESIS-1a is a
framing-precision edit ("the two path roots are siblings" → "the two
path roots sit under sibling workspace crates …; the lint glob brace-
expansion is path-pattern-based, not structural-relation-based"),
neither of which is CH5-surface. F-V5-SYNTHESIS-1b is a cohesion
deletion of the residual closing sentence rendered redundant by the
V4 fold's explicit 8+6 decomposition at line 312, again non-CH5.
Forward V3 binding from V2 CH5 §4 note 3 (totality-track lint glob
extension post-C-1 PRUNE-4) carries through V5 to S-P3 unchanged.

UTC dispatch start: 2026-05-23T14:32Z. Hard cap: 30 min.

## §0 — Disposition summary

| Per-artefact | V1 | V2 | V3 | V4 | V5 | Δ V4 → V5 |
| --- | --- | --- | --- | --- | --- | --- |
| `SYNTHESIS-AUDIT-OVERFIT.md` | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | F-V5-SYNTHESIS-1{a,b} are prose precision + cohesion deletion at §2.4 lines 318-321. Zero CH5 surface. |
| A1 `…css-measurement.md` | ACCEPT | ACCEPT (STAND) | ACCEPT (STAND) | ACCEPT (STAND) | ACCEPT (STAND) | No V5 fold |
| A2 `…admit-mechanism.md` | ACCEPT | ACCEPT (STAND) | ACCEPT (STAND) | ACCEPT (STAND) | ACCEPT (STAND) | No V5 fold |
| A3 `…lock14-scan.md` | ACCEPT | ACCEPT | ACCEPT (STAND) | ACCEPT (STAND) | ACCEPT (STAND) | No V5 fold |
| A4 `…generator-truth.md` | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | No V5 fold; A4 STAND verbatim from V4 |
| A5 `…decision-engine.md` | ACCEPT | ACCEPT | ACCEPT | ACCEPT (STAND) | ACCEPT (STAND) | No V5 fold |
| A6 `…pre-restart-pattern.md` | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | ACCEPT-with-COUPLING-NOTE | No V5 fold; A6 STAND verbatim |

**V5 ACCEPT-rate: 7 / 7 = 100 %.** Zero CH5 firings introduced by the
V5 micro-fold packet. The two ACCEPT-with-COUPLING-NOTE dispositions
(A4 §3.1 Track 1 / Track 2; A6 §3.3 substrate-doc opt-out
enshrinement) carry forward unchanged from V4 with the underlying
A4 / A6 artefacts byte-identical at HEAD `037eec6b6` (V5 commit
modifies only SYNTHESIS-AUDIT-OVERFIT.md per `git show 037eec6b6
--stat` returning a single-file delta of +4/-3).

## §1 — V5 micro-fold contamination scan (CH5 charge-specific)

The V5 micro-fold packet at commit `037eec6b6` lands two prose touches
in one file (per `git show 037eec6b6 --stat`: SYNTHESIS-AUDIT-
OVERFIT.md, +4 insertions / -3 deletions). CH5 verification per-fold
below.

### §1.1 V5 micro-fold per-fold verification table

Verified via `git diff 80ee76607..037eec6b6 -- restart/skinny/tranches/
sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`. One file modified,
+4 / -3 hunk-pair spanning §2.4 lines 318-321.

| V5 fold | Edit summary | Substrate impact | CH5 firing? |
| --- | --- | --- | --- |
| **F-V5-SYNTHESIS-1a** | SYNTHESIS §2.4:318 "(the two path roots are siblings)" → "(the two path roots sit under sibling workspace crates `skinny/crates/runtime/` and `skinny/crates/codegen/`; the lint glob brace-expansion is path-pattern-based, not structural-relation-based)". Re-read at HEAD `037eec6b6` lines 318-321 confirms exact placement. The corrected framing replaces a loose structural-sibling claim (which was false: `runtime/src/grammars` is 3-deep, `codegen/src` is 2-deep; their PARENTS `runtime/` + `codegen/` are direct siblings at depth 1 inside `skinny/crates/`) with two precise observations: (i) the path roots live under sibling workspace crates `skinny/crates/{runtime,codegen}/`; (ii) the lint glob brace-expansion is path-pattern-based, not structural-relation-based — i.e. the disjointness derives from path-string disambiguation under glob match semantics, not from any directory-tree sibling-relation between `runtime/src/grammars` and `codegen/src`. | Framing-precision correction (CH4 §4 note from V4 CONSOLIDATED §2.2 Option B). The load-bearing CH5 claim — that the 42 runtime-side mirror files and 14 codegen-side files are two **disjoint populations** under the lint glob — is preserved verbatim. The substrate-union refusal posture under CH5 is reinforced, not diluted: the path-pattern-based disambiguation framing makes the disjointness derivation mechanically explicit (glob-match on first path component after `skinny/crates/` partitions cleanly into `runtime/` vs `codegen/`). No NEW lint glob, no NEW path root, no NEW substrate, no NEW pipeline. Workspace-crate sibling framing identifies the two crates as **sibling cargo workspace members** under `skinny/crates/`, which is structurally accurate per `skinny/Cargo.toml` workspace member enumeration; this strictly tightens the V4 framing without opening new surface. | **NO** |
| **F-V5-SYNTHESIS-1b** | SYNTHESIS §2.4:319-321 (V4 pre-image): "The lint glob `codegen/src/**/*.rs` catches all 14 regardless; only the prose count needs the 8-vs-14 distinction." — DELETED. Re-verified at HEAD `037eec6b6`: the closing sentence "A runtime-only lint would let the codegen-side twin re-introduce the fake header silently …" now follows directly after the parenthetical close, with no intervening "catches all 14 regardless; only the prose count needs the 8-vs-14 distinction" residual. | Cohesion-polish deletion (CH2 §4.2 note from V4 CONSOLIDATED §2.2 Option B). The V4 fold's explicit 8+6 decomposition at line 312 ("14 codegen-side (8 providers+templates + 6 ancillary)") already enumerates the breakdown, rendering the V2-V3 precision-repair back-reference redundant. The deletion removes documentary fossil; no semantic content is lost; the 56-file census + disjoint-set claim + lint-glob brace-expansion mechanism all carry forward intact in the surviving prose at lines 311-318. No NEW substrate, no NEW pipeline, no NEW finding, no removed indictment. The two-path-root substrate-union refusal token-set (42 / 14 / 56 / disjoint / brace-expansion) persists at HEAD `037eec6b6`. | **NO** |

Both V5 micro-folds clear the CH5 charge: zero parallel substrate,
zero NEW sidecar producer, zero renamed-scanner Lock 1 violation,
zero Track 1 ≡ Track 2 collapse vector. The V5 edits are bounded to
the single file SYNTHESIS-AUDIT-OVERFIT.md and to the single region
§2.4 lines 318-321; A1-A6 stand byte-identical from V4.

### §1.2 Direct CH5 invariant re-execution

The four V1 CH5 §1 sub-vectors re-execute clean at HEAD `037eec6b6`.

| Sub-vector | V1 method | V5 re-execution | Result |
| --- | --- | --- | --- |
| (1) Renamed-scanner / fallback combinator scan | `grep -rn 'fn parse_combinator\|combinator_fallback\|parse_with_fallback' crates/core/src/runtime/ skinny/crates/runtime/src/` | Re-executed at HEAD `037eec6b6` | **Zero matches.** Lock-1-adjacent renamed-scanner audit CLEAN. |
| (2) LegacyPath bounded-leak scan | `grep -rln 'LegacyPath\|LegacySegment' skinny/crates` and `… crates/core/src/runtime/` | Re-executed at HEAD `037eec6b6`. `skinny/crates` returns **zero matches**; `crates/core/src/runtime/` returns **4 files** (`json/parse_with.rs`, `css_l4/parse_with.rs`, `bbnf/parse_with.rs`, `google_sheets/parse_with.rs`) per V1 CH5 §1.2 expectation. | LegacyPath shim remains **bounded** to the 4 grammar-specific `parse_with.rs` files; zero leak into generic crates (`skinny/crates/{ir,codegen,passes,runtime}`). |
| (3) Substrate-union refusal token presence | `grep -rn 'UnionTape\|second tape\|public substrate API\|class/mask stream\|parser-owned cursor' restart/skinny/tranches/sk-v14/audit-overfit/` | Re-executed at HEAD `037eec6b6`. Returns 16 refusal-token mentions across V1-V4 CH5 disposition files + V1 CHALLENGE-CONTEXT + V2 ADDENDUM + V1 CONSOLIDATED, all in REFUSAL / pre-block / guard context (zero authorisations). | Substrate-union refusal posture intact across SYNTHESIS + ledger artefacts. |
| (4) Track-collapse refusal scan | `grep -rcE 'Track 1 ≡ Track 2\|Track 1.*Track 2' restart/skinny/tranches/sk-v14/audit-overfit/` | Re-executed at HEAD `037eec6b6`. Returns mentions in V1 CHALLENGE-CONTEXT (1), V2 ADDENDUM (1), V1+V2+V3+V4 CH5 disposition files (8+16+14+13), V1 CONSOLIDATED (2) — all in disjunction-preserving / refusal context. | Track 1 / Track 2 boundary semantically preserved across all artefacts. |

All four invariants hold. CH5 substrate-union closure is intact at
HEAD `037eec6b6`. The V5 micro-fold packet (SYNTHESIS-only, +4/-3)
does not touch any source file under `crates/core/src/runtime/`,
`skinny/crates/`, or `crates/core/src/`; the four CH5 invariants
re-execute identically to V4 because the underlying source tree is
unchanged across the V4→V5 commit pair (`git diff f8e279877..
037eec6b6 -- skinny/ crates/ | wc -l` returns 0).

### §1.3 F-V2-A6-1 LegacyPath disambiguation — V5 standing verification

The V2 CH5 §1.2 verification block (LegacyPath classification as
Lock-1-*adjacent* coupling subject to C-1 PRUNE-4 collapse) carries
forward verbatim at V5. A6 is a V5 STAND artefact (per the V5 commit
message stating "A1-A6 STAND verbatim (V5 commit touches SYNTHESIS
only)"); `git show 037eec6b6 --stat` confirms only one file modified
(SYNTHESIS-AUDIT-OVERFIT.md), so A6's disambiguation paragraph at §0
line 12 + ledger row Status "NEW (scope-extension over V13 Pattern G;
not a reversal)" persists byte-identical from V4 / V3 / V2.

The V4 CH5 §1.3 disposition — (a) classification correctness
(scope-extension framing preserves both V13 carveout reading + V14
explicit survey); (b) substrate-union impact (Lock-1-adjacent, not
Lock-1 substrate-union violation; subject to C-1 PRUNE-4 collapse
target wired through SYNTHESIS §3.1 row C-1); (c) no Lock 1
substrate-union violation introduced (bounded to 4 `parse_with.rs`
files with zero leak into generic crates) — holds at V5 without
modification.

### §1.4 Cross-axis coherence — Track 1 / Track 2 boundary at V5

The V1 CH5 §3.1 + V2 CH5 §1.3 + V3 CH5 §1.4 + V4 CH5 §1.4 observation
(A4 NEW-1 / R4 scope-extension as preserving Track 1 / Track 2
disjunction) carries forward intact. Re-verified at HEAD `037eec6b6`:

- A4 finding 16 LOW persists byte-identical from V4 (A4 is a V5
  STAND artefact; `git diff f8e279877..037eec6b6 -- restart/skinny/
  tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-generator-
  truth.md` returns empty). Track separation preserved verbatim
  under V5.
- F-V2-A4-1 "scope extension, not reversal" framing at A4 §0 + §3
  PRESERVES the Track 1 / Track 2 disjunction by indicting only the
  SKINNY-track providers (`json_provider.rs` + 7 CSS providers); the
  totality-track `xtask::regen::run` path remains untouched.

The V1 CH5 §4.1 binding note for S-P3 wave manifest (R4 specification
must encode Track 1 / Track 2 separation as a hard constraint,
attaching `regen-css-l4-<provider>` subcommands to `skinny/xtask/src/
main.rs:8` USAGE line only) carries forward unchanged into V5 → S-P3.

### §1.5 F-V5-SYNTHESIS-1a sibling-workspace-crate framing — special CH5 cross-check

The framing-precision correction "the two path roots are siblings" →
"the two path roots sit under sibling workspace crates `skinny/
crates/runtime/` and `skinny/crates/codegen/`; the lint glob brace-
expansion is path-pattern-based, not structural-relation-based"
warrants a CH5 cross-check because the new prose now explicitly names
two cargo workspace crates as the locus of the disjoint-set census.
CH5 must verify the **workspace-crate** framing does not introduce a
NEW substrate-union vector through the sibling-crate naming itself.

Re-verified at HEAD `037eec6b6`: `skinny/Cargo.toml` workspace member
enumeration confirms `skinny/crates/runtime` and `skinny/crates/
codegen` are both workspace members of the `skinny/` workspace. The
two crates' relationship is **publish-distinct** (separate Cargo
packages with separate `Cargo.toml`s + separate dependency graphs);
the codegen-side is build-time-only (xtask invoker), the runtime-
side is the production runtime crate. The "sibling workspace crates"
framing makes this **distinct-package** relationship explicit, which
is a CH5 *strengthening*: it precludes the misreading that the
codegen-side template and runtime-side mirror could share substrate
via in-crate module-system coupling (they cannot, because they are
separate cargo packages with no upstream-downstream import path
between codegen and runtime — the runtime-side mirror is emitted
by xtask invocations of codegen during a regen, not imported from
codegen as a library dependency). The path-pattern-based versus
structural-relation-based clarification reinforces the same point at
the glob-match level: the disjointness derives from path-string
disambiguation, not from any module-system or import-graph relation.

| Census band | Files | CH5 substrate classification at V5 |
| --- | --- | --- |
| 42 runtime-side mirror files (`skinny/crates/runtime/src/grammars/**/*.rs` carrying the fake `@generated` header) | Re-verified at HEAD `037eec6b6`: `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ \| wc -l` returns **42**. Identical-content twin of codegen-side template per A4 finding 15. Distinct cargo package from codegen-side. | NOT a parallel substrate. Cross-crate emission (codegen → runtime via xtask invocation), not in-crate substrate sharing. PRUNE-4 collapses wholesale per SYNTHESIS §3.1 row C-1; the V5 sibling-workspace-crate framing makes the cross-package boundary explicit, reinforcing CH5's substrate-union refusal. |
| 14 codegen-side files (`skinny/crates/codegen/src/**/*.rs` carrying the fake `@generated` header) | Re-verified at HEAD `037eec6b6`: `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/ \| wc -l` returns **14**. Per V3 F-V3-SYNTHESIS-2: 7 CSS L4 providers + 1 JSON provider + 4 json_templates submodules + json_typed_direct + lib.rs. | NOT a parallel substrate (V4 CH5 §1.5 disposition verbatim). PRUNE-2 + PRUNE-3 + PRUNE-4 collapse targets per SYNTHESIS §3.1 C-1 + C-5. |
| Disjointness verification | The brace expansion `{runtime/src/grammars,codegen/src}` produces path roots whose first segment after `skinny/crates/` differs (`runtime/` vs `codegen/`); glob match is path-string-based, so the intersection is empty by construction. The V5 prose now explicitly notes "path-pattern-based, not structural-relation-based" framing. | Verified by construction: empty intersection. The V5 prose tightens the V4 framing from a loose structural-sibling claim to a precise glob-match disambiguation claim, preserving the load-bearing disjoint-set property. |

The 42 runtime-side mirror files + 14 codegen-side files map cleanly
to the **existing** C-1 (PRUNE-4) and C-1+C-5 (PRUNE-2 + PRUNE-3 +
PRUNE-4) collapse targets per SYNTHESIS §3.1. The 56-file census +
sibling-workspace-crate framing + path-pattern-based disambiguation
all reinforce the V4 disjoint-set establishment without opening any
NEW C-N candidate, NEW parallel substrate, NEW pipeline, or Lock 1
substrate-union violation. **F-V5-SYNTHESIS-1a is a positive CH5
strengthening, not a regression.**

## §2 — Per-artefact V5 disposition table

| Artefact | V5 disposition | Δ from V4 |
| --- | --- | --- |
| `SYNTHESIS-AUDIT-OVERFIT.md` | ACCEPT | F-V5-SYNTHESIS-1{a,b} are two prose touches at §2.4 lines 318-321: framing-precision correction (CH4 §4 note from V4 CONSOLIDATED §2.2 Option B) replacing loose structural-sibling claim with sibling-workspace-crate framing + path-pattern-based disambiguation; cohesion deletion (CH2 §4.2 note) removing the V2-V3 precision-repair back-reference rendered redundant by the V4 fold's explicit 8+6 decomposition at line 312. Per §1.1 + §1.5 above: zero CH5 firing across both folds. The 74-finding aggregate + C-1..C-5 envelopes + 9-grammar census + Track 1/Track 2 disjunction + lint-glob brace-expansion mechanism + 56-file disjoint-set census all hold byte-identical to V4. |
| A1 `…css-measurement.md` | ACCEPT (STAND) | No V5 fold; V4 disposition carries unchanged. |
| A2 `…admit-mechanism.md` | ACCEPT (STAND) | No V5 fold; V4 disposition carries unchanged. |
| A3 `…lock14-scan.md` | ACCEPT (STAND) | No V5 fold; A3 STAND verbatim from V4. The 30-finding ledger (11 CRIT + 6 HIGH + 5 MED + 8 LOW per F-V2-A3-1 reclassification) holds; PRUNE-3 + R4 collapse path unchanged. |
| A4 `…generator-truth.md` | ACCEPT-with-COUPLING-NOTE | No V5 fold; A4 STAND verbatim from V4. V2 §3.1 Track 1 / Track 2 binding note carries forward verbatim. Coupling note unchanged in substance. |
| A5 `…decision-engine.md` | ACCEPT (STAND) | No V5 fold; A5 STAND verbatim from V4. F-V3-A5-1 + F-V3-SYNTHESIS-1 action-class propagation matrix completion preserved without modification. |
| A6 `…pre-restart-pattern.md` | ACCEPT-with-COUPLING-NOTE | No V5 fold; A6 STAND verbatim. F-V2-A6-1 LegacyPath disambiguation paragraph + §2 ledger row Status "NEW (scope-extension over V13 Pattern G; not a reversal)" persist byte-identical. CH5 §1.2 substrate-union classification (Lock-1-*adjacent*; bounded to 4 `parse_with.rs` files; subject to C-1 PRUNE-4 collapse) holds at V5. |

## §3 — Critical CH5 findings

### §3.1 No new critical findings under V5

V5 micro-fold inspection (`git diff 80ee76607..037eec6b6 -- restart/
skinny/tranches/sk-v14/audit-overfit/` shows one file modified +4/-3;
`git diff f8e279877..037eec6b6 -- skinny/ crates/ | wc -l` returns 0
confirming zero source-tree change across the V4→V5 commit pair)
confirms zero new CH5 firings introduced by the V5 packet. The two
V1+V2+V3+V4 ACCEPT-with-COUPLING-NOTE observations (A4 §3.1 Track 1 /
Track 2; A6 §3.2 + §3.3 LegacyPath shim + substrate-doc opt-out
enshrinement) carry forward unchanged with the underlying A4 / A6
artefacts byte-identical at V5 (both STAND artefacts).

### §3.2 Fresh-finding scan — V5 micro-fold side-effects

Per the V5 CH5 dispatch task ("Verify no parallel substrate / Track 1
≡ Track 2 / renamed-scanner Lock 1. Fresh-finding scan."):
fresh-finding scan executed at HEAD `037eec6b6`.

| Scan target | Method | V5 result |
| --- | --- | --- |
| New BIR / TypeDesc / TapeKind variant introduced by V5 packet | `git diff 80ee76607..037eec6b6 -- restart/skinny/tranches/sk-v14/audit-overfit/ \| grep -nE 'enum (BackendIr\|TypeDesc\|TapeKind)\|new variant'` | Zero hits. The V5 packet is prose-only; no enum, no variant, no type introduction. |
| New `<g>_provider.rs` introduced by V5 | `git diff 80ee76607..037eec6b6 -- … \| grep -nE 'provider.rs\|sidecar'` | Zero hits. F-V5-SYNTHESIS-1{a,b} are framing + deletion edits at §2.4:318-321; no provider citation introduced. |
| New runtime-side mirror file enumerated by V5 | `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime/src/grammars/ \| wc -l` | Returns 42 — unchanged from V4. The V5 packet preserves the 42 / 14 / 56 census verbatim; only the surrounding framing prose is refined. |
| New `Lock-14`-leaking grammar identifier in nominally-generic crate | `git diff 80ee76607..037eec6b6 -- …` | Zero new symbols introduced. F-V5-SYNTHESIS-1a refines framing (structural-sibling → sibling-workspace-crate + path-pattern-based); F-V5-SYNTHESIS-1b deletes the V2-V3 fossil back-reference. Both are precision/cohesion polish over existing surface. |
| New combinator-fallback scanner | `grep -rn 'fn parse_combinator\|combinator_fallback\|parse_with_fallback' crates/core/src/runtime/ skinny/crates/runtime/src/` | Zero matches (V5 re-execution; renamed-scanner Lock 1 audit CLEAN). |
| New regen pipeline conflation Track 1 ≡ Track 2 | A4 finding 16 + V1 §3.1 + V2 F-V2-A4-1 framing + V3 stand + V4 STAND + V5 STAND | Track 1 / Track 2 disjunction preserved verbatim at HEAD `037eec6b6`; the V5 SYNTHESIS-only edit does not touch A4 finding 16 (A4 is a V5 STAND artefact, byte-identical from V4). |
| New shadow CSP / parallel decision engine | No A5 V5 fold (A5 STAND); F-V3-A5-1 + F-V3-SYNTHESIS-1 action-class propagation matrix completion preserved | Zero NEW resolver path. The action-class widening "preserved through PRUNE-5 as gate-rejection invariant" carries forward verbatim across §5 closing + SYNTHESIS §3.1; the resolver clause (W5 → W6 → W7 → lowering via the five fail-closed checks at `codegen/src/lower/rust.rs:37-89`) is untouched. |
| New parallel-substrate vector from F-V5-SYNTHESIS-1a sibling-workspace-crate framing | Per §1.5 above: the sibling-workspace-crate framing names cargo workspace siblings whose codegen → runtime relationship is **build-time xtask emission**, not in-crate substrate sharing; the path-pattern-based disambiguation makes the disjointness derivation glob-match explicit. | Zero NEW C-N candidate, zero NEW pipeline. The framing-precision tightens V4 from a loose structural-sibling claim to a precise distinct-package + glob-match claim; both formulations preserve the load-bearing 42 / 14 / 56 disjoint-set census. |
| New lint-glob mechanism | `git diff 80ee76607..037eec6b6 -- restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md \| grep -E 'glob\|brace\|expansion'` | The V5 prose explicitly cites the brace-expansion mechanism `{runtime/src/grammars,codegen/src}/**/*.rs` (already present in V4 §2.4 lines 313-314; V5 edit reinforces the path-pattern-based glob semantics at line 320-321). No NEW glob, no NEW path root, no NEW lint mechanism. |
| F-V5-SYNTHESIS-1b cohesion deletion content loss | Diff inspection: the deleted sentence "The lint glob `codegen/src/**/*.rs` catches all 14 regardless; only the prose count needs the 8-vs-14 distinction." was a V2-V3 precision-repair back-reference whose content is now subsumed by the V4 explicit 8+6 decomposition at SYNTHESIS §2.4:312 ("14 codegen-side (8 providers+templates + 6 ancillary)"). | Zero indictment lost. The deletion is fossil-removal; the surviving prose at lines 311-318 carries the full 56 / 42 / 14 / 8+6 / disjoint / brace-expansion / glob-actual-union token-set. |

The fresh-finding scan returns **zero new findings**. The V5 folds
are editorial-precision (framing-precision correction over a loose
structural claim; cohesion deletion of a V2-V3 fossil rendered
redundant by the V4 fold); neither introduces a new substrate,
sidecar, scanner-rename, or track-collapse vector.

### §3.3 Forward V4 binding (V2 CH5 §4 note 3 + V3 CH5 §4 + V4 CH5 §4) carries to S-P3 unchanged

V2 CH5 §4 note 3 specified that the CH7-companion lint glob must
extend to the totality-track `crates/core/src/runtime/**/*.rs` after
C-1 PRUNE-4 lands. The V5 packet did not pre-empt this extension —
F-V5-SYNTHESIS-1a's framing-precision correction touches only the
skinny-track brace expansion `{runtime/src/grammars,codegen/src}/
**/*.rs` (sibling workspace crates inside `skinny/crates/`); the
totality-track glob mechanism is untouched. The V2 / V3 / V4 forward-
binding obligation carries to S-P3 wave-close gate manifest unchanged.
**Forward observation; not a V5 REVISE blocker.**

### §3.4 V5-specific observation — F-V5-SYNTHESIS-1a strengthens CH5 substrate-union closure (second-order)

The framing-precision correction "the two path roots are siblings" →
"the two path roots sit under sibling workspace crates …; the lint
glob brace-expansion is path-pattern-based, not structural-relation-
based" is, beyond CH4's precision mandate, a **second-order
substrate-union refusal strengthening** under CH5. The V4 prose's
loose structural-sibling claim could be misread as an assertion that
the two path roots are direct directory-tree siblings — which would
have implied a within-crate / within-module-tree relation between
codegen-side template and runtime-side mirror. The V5 correction
makes explicit two distinct facts:

1. The path roots' parents are **sibling cargo workspace crates**
   (separate cargo packages with separate `Cargo.toml`s, separate
   dependency graphs; codegen is build-time-only, runtime is the
   production runtime crate). Cross-crate emission via xtask, not
   in-crate substrate coupling.
2. The lint-glob brace-expansion disjointness derives from **path-
   pattern matching**, not from any directory-tree sibling-relation.
   The glob's empty-intersection property is a string-disambiguation
   theorem, not a structural one.

Together these clarifications preclude two distinct misreadings the
V4 prose tacitly admitted: (a) the within-module substrate-sharing
misreading (CH5 substrate-union direct); (b) the structural-relation-
to-glob-semantics conflation misreading (CH5 substrate-union by
inferential leakage). **F-V5-SYNTHESIS-1a is a positive CH5
strengthening that follows F-V4-SYNTHESIS-1's first-order disjoint-
set strengthening with a second-order framing-mechanism
strengthening.** The combined V4 + V5 effect on CH5 is to lock the
substrate-union refusal posture across both census arithmetic
(disjoint set, 42 + 14 = 56) and census mechanism (path-pattern
glob, cross-package emission).

## §4 — V5 confirming-pass closure

The audit campaign at V5 closes with 100 % ACCEPT under the CH5 lens
(matching V1 + V2 + V3 + V4 = **five consecutive 100 % cycles**,
exceeding the `ORCHESTRATOR.md §3Z` two-consecutive-cycle convergence
floor by three full cycles, hitting the V max=5 ceiling). The V5
micro-fold packet's two edits clear the CH5 charge across all three
sub-vectors (no parallel substrate, no Track-collapse, no renamed-
scanner Lock 1 violation) plus the F-V2-A6-1 LegacyPath disambiguation
continuity. Four binding observations to S-P3 / wave manifest:

1. **V1 CH5 §4.1 + §4.2 + V2 CH5 §4 + V3 CH5 §4 + V4 CH5 §4 forward
   bindings carry into S-P3 unchanged.** The R4 specification language
   must encode Track 1 / Track 2 separation as a hard constraint
   (`regen-css-l4-<provider>` subcommands attach to `skinny/xtask/
   src/main.rs:8` USAGE line only); PRUNE-4 must explicitly state the
   substrate-union closure target; the CH7-companion lint glob must
   extend to `crates/core/src/runtime/**/*.rs` after C-1 PRUNE-4
   lands.

2. **F-V4-SYNTHESIS-1 56-file disjoint-set census + F-V5-SYNTHESIS-1a
   sibling-workspace-crate framing establishes the canonical CH5
   census surface for S-P3.** Any S-P3 cross-flag against a "codegen-
   side N codegen files" count must use the disjoint-union semantics
   (42 runtime-side mirror + 14 codegen-side = 56 total, zero overlap)
   AND the path-pattern-based glob-match disambiguation framing (the
   disjointness derives from path-string disambiguation, not
   directory-tree sibling-relation). The lint-glob mechanism scoping
   unchanged; only the prose census + framing semantics align with
   the glob's set-union behaviour and the cross-package emission
   architecture. **Forward CH5 cross-check obligation; not a
   blocker.**

3. **F-V5-SYNTHESIS-1b cohesion-deletion sweep closes the V2-V3
   precision-repair back-reference fossil.** The deletion is
   content-neutral (V4 fold's explicit 8+6 decomposition at line 312
   subsumes the deleted sentence). S-P3 should not re-introduce the
   "catches all 14 regardless" framing; the brace-expansion
   `{runtime/src/grammars,codegen/src}/**/*.rs` is the load-bearing
   lint scope (not the V2-era `codegen/src/**/*.rs` narrower glob).
   **Forward observation; not a blocker.**

4. **V5-introduced positive strengthening (per §3.4 above) locks the
   second-order substrate-union refusal posture.** The sibling-
   workspace-crate + path-pattern-based framing precludes (a) within-
   module substrate-sharing misreadings and (b) structural-relation-
   to-glob-semantics conflation misreadings. S-P3 should preserve
   this framing in any post-PRUNE-4 lint-glob extension prose.
   **Forward observation; not a blocker.**

None of the four observations is a REVISE; all are forward-binding
clarifications. The CH5 lens converges without blocker.

## §5 — Verdict

**CH5 — HIDDEN COUPLING V5: ACCEPT (7 / 7 = 100 %).**

The S-P0 audit campaign at SK-V14 V5 holds the CH5 lens without
firing. Both V5 micro-fold edits are verified editorial-precision
(framing-precision correction for F-V5-SYNTHESIS-1a; cohesion
deletion for F-V5-SYNTHESIS-1b); neither introduces a parallel
substrate, sidecar producer, renamed-scanner Lock 1 violation, or
Track 1 ≡ Track 2 collapse vector.

The F-V2-A6-1 LegacyPath disambiguation (re-verified at HEAD
`037eec6b6` against unchanged A6 STAND artefact) continues to
classify the shim as: (a) NEW finding by scope-extension over V13
Pattern G (not a reversal of V13's CLEAN disposition); (b) Lock-1-
*adjacent* coupling (not a Lock-1 substrate-union violation); (c)
subject to C-1 PRUNE-4 typed-path collapse with bounded scope (4
`parse_with.rs` files in `crates/core/src/runtime/`; zero leak into
`skinny/crates` verified via re-executed `grep -rln 'LegacyPath\|
LegacySegment' skinny/crates` returning zero matches).

The F-V5-SYNTHESIS-1a sibling-workspace-crate + path-pattern-based
framing (the V5 fold with the broadest framing-mechanism impact)
introduces zero NEW parallel-substrate vector — it strictly tightens
the V4 disjoint-set framing by replacing a loose structural-sibling
claim with two precise observations (sibling cargo workspace crates;
glob-match path-pattern disambiguation). The 42 runtime-side mirror
files all map to the EXISTING C-1 (PRUNE-4) collapse target per
SYNTHESIS §3.1; the 14 codegen-side files were already enumerated
under V3 F-V3-SYNTHESIS-2 and V4 F-V4-SYNTHESIS-1; the disjointness
remains verified by construction. Beyond mere CH4 framing precision,
the correction *reinforces* CH5's substrate-union refusal posture
(per §3.4) by precluding within-module substrate-sharing misreadings
and structural-relation-to-glob-semantics conflation.

The F-V5-SYNTHESIS-1b cohesion deletion removes a V2-V3 precision-
repair fossil rendered redundant by the V4 fold's explicit 8+6
decomposition; the deletion is content-neutral, lossless of
indictment, and aligns the §2.4 prose with the V4 surviving
enumeration. The substrate-union token-set (56 / 42 / 14 / disjoint /
brace-expansion / sibling workspace crates / path-pattern-based) is
intact at HEAD `037eec6b6`.

The fresh-finding scan returns zero new coupling findings. The
V1+V2+V3+V4 two ACCEPT-with-COUPLING-NOTE observations (A4 §3.1
Track 1 / Track 2 boundary; A6 §3.2 + §3.3 LegacyPath bounded +
substrate-doc opt-out enshrinement) carry forward unchanged at V5
(A4 and A6 are STAND artefacts byte-identical from V4).

Per `ORCHESTRATOR.md §3Z`, the CH5 lens convergence criterion is met
at V5 (100 % ACCEPT × **5 consecutive cycles** V1 → V2 → V3 → V4 →
V5; zero open critical defects; zero orphan unresolved REVISE). The
two-consecutive-cycle requirement was already satisfied at V2 → V3
with both at 100 %; V3 → V4 reinforced; V4 → V5 closes the §3Z
chain at the V max=5 ceiling. **CH5 releases to G-S-P0-CONVERGED
per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP. S-P1 dispatch
unblocked.**
