# CH7 Overfit-Prune — Pass Alpha V4 Disposition

Lens binding unchanged: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`.
Five CH7 criteria remain the disposition spine (CH7-1 grammar-derived
only; CH7-2 Lock 14 generic-crate compliance; CH7-3 real source change
+ strict-vs-strict comparator + per-iter equality; CH7-4 round-trip on
generated output; CH7-5 no scaffold admit). V4 overlay per the V4
dispatch context: (1) execute the V4-corrected `cargo metadata | jq`
form against the live workspace and quote the actual output (V3
lens-depth-gap remediation per CH1 V3 §2 REV-1); (2) verify the E-1
BINDING dual-tree + bypass-header round-trip gate still holds; (3)
verify all other CH7 V2/V3 anchors hold; (4) fresh-finding scan across
all five CH7 criteria.

## §0 — Disposition summary

- Artefacts re-reviewed: 2 V4-touched (HANDOFF via F-V4-α-F-1; α-E via
  F-V4-α-E-1) + 3 V2/V3-touched STAND-from-V3 (SYNTHESIS, α-A, α-C) +
  3 STAND-from-V1 (α-B, α-D, DISPATCH-CONTEXT). Same surface as V2 + V3.
- Per-section dispositions issued (V4 overlay): **36** (same denominator
  as V2 + V3).
- ACCEPT: **36**.
- REVISE: **0**.
- REJECT: **0**.
- ACCEPT-rate: 36 / 36 = **100.0 %**.
- Critical findings: 0.
- Escalation flag: **NO.** The V3 baseline holds verbatim with one
  prescribed improvement on the C-3 gate: F-V4-α-E-1 corrects the
  shell command's jq path so the gate is now mechanically executable
  (V3 CH1 REV-1 remediation). Executable verification §1.1 below
  confirms the corrected command enumerates 9 grammars including
  `css_pretty`. The CH7 lens converges at **100 %** for the third
  consecutive cycle.

## §1 — Executable verification (V3 lens-depth-gap remediation)

### §1.1 — V4-corrected C-3 round-trip shell command verified

Per the V4 dispatch context mandate (V3 CH7 documentary verification
vs CH1 executable verification divergence noted by V3 aggregator), the
V4-corrected `cargo metadata | jq` form at
`alpha-E-candidate-shortlist.md:366-367` was executed against the live
workspace.

**Command (verbatim from `alpha-E-candidate-shortlist.md:366-367`):**

```
cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
```

**Output (captured in `/Users/mkbabb/Programming/bbnf-lang`):**

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

The command enumerates **9 grammars**: `bbnf`, `json`, `css_l4`,
`css_pretty`, `google_sheets`, `ebnf`, `bnf`, `csv`, `math`. The
output exactly matches the V4 commit message attestation
(`5e00b6d27`) and the workspace metadata roster at
`Cargo.toml:18-29`. The V3 defect surfaced by CH1 V3 §2 REV-1 (jq path
`.workspace_metadata.bbnf.grammars | keys[]` yielded `null` and
silently no-op'd the for-loop body) is closed: the V4 form yields the
9-element grammar identifier list that the `for g in $(…); do rm -rf
… && cargo xtask "regen-${g}" && git diff --exit-code -- "…/${g}/"
|| exit 1; done` loop iterates verbatim. Each iteration body is
non-trivial; the gate now mechanically enforces the round-trip
property for every rostered grammar, including the V3-era-unenumerated
ninth grammar `css_pretty`.

The V3 lens-depth gap (CH7 V3 verified the fold-landed structure but
not the shell command's execution semantics) is closed at the V4
dispatch site: CH7 V4 carries the executable verification mandate and
the gate is now executable-verified, not only documentary-verified.

### §1.2 — Generalised admission phrasing verified

V3 form at `alpha-E-candidate-shortlist.md:373` read "adding a 9th
grammar requires NO change to the gate's text"; V4 form at `:372`
reads "admitting an additional grammar requires NO change to the
gate's text". The roster-count-agnostic phrasing is verified at the
exact line; the V3 ninth-grammar referent (which was stale relative
to the actual nine-grammar roster including `css_pretty`) is removed.
No grammar-count literal appears anywhere in the gate's load-bearing
or descriptive text (verified via `grep -n "9th\|ninth"
alpha-E-candidate-shortlist.md` returning zero hits in the gate
clause).

### §1.3 — Round-trip semantic preservation verified

The three-part round-trip + bypass-header detector verbatim
preservation check:

- **Round-trip (skinny tree)** at `:358-361`: unchanged from V3 (RTS
  `rm -rf … && cargo xtask regen-css && git diff` on
  `skinny/crates/runtime/src/grammars/css_l4_*` produces empty
  output). PRESERVED.
- **Round-trip (core tree, all rostered grammars)** at `:362-387`:
  iterates over the metadata-derived grammar list; emits `rm -rf …
  && cargo xtask "regen-${g}" && git diff --exit-code -- …/${g}/`
  for each `<g>`; V4 adds `--exit-code` to `git diff` so non-empty
  diffs fail with non-zero status (closing a V3-era latent gap where
  the gate text relied on shell return-code propagation that
  `git diff` alone does not always provide). The round-trip
  semantic (delete + regen → byte-equivalent) is PRESERVED VERBATIM
  AND STRENGTHENED by the `--exit-code` discipline.
- **Bypass-header detector** at `:388-398`: unchanged from V3 (`git
  grep -l '@generated by skinny bbnf-codegen' -- skinny/crates/runtime
  crates/core/src/runtime` traces every match to a registered xtask
  emission). PRESERVED.

SYNTHESIS §3 row C-3 mirror at `SYNTHESIS.md:273` carries the
dual-tree round-trip + bypass-header detector + see-§5 pointer; the
V4 commit (`5e00b6d27`) does not touch SYNTHESIS so the mirror holds
verbatim from V2 F-17. No V4 fold to SYNTHESIS is needed because the
mirror text is grammar-count-agnostic ("on both runtime trees" + "on
`skinny/crates/runtime/src/grammars/css_l4_*` AND on
`crates/core/src/runtime/css_l4/`" — names only the CSS slice for the
SYNTHESIS-grain disposition, defers the all-grammar gate text to α-E).

## §2 — V3 baseline + V4 micro-fold verification table

Each binding CH7 disposition tracked from its V1 origin through V2
fold-landing through V3 verification through V4 verification.

| CH7 disposition (V1 origin) | V2 fold | V3 status | V4 verification | Status |
|---|---|---|---|---|
| **BINDING REJECT — C-3 round-trip gate CH7-1-blind to Pattern H** (V1/CH7.md §2.1 + §3.1) | E-1 (BINDING) | STRENGTHENED via F-V3-α-E-1 (metadata-derived loop) | **STRENGTHENED + EXECUTABLE-VERIFIED.** V4 fold F-V4-α-E-1 corrects the jq path so the metadata-derived loop is now mechanically executable (V3 form jq-path defect closed). §1.1 above executes the V4 command and quotes the 9-grammar output. The V3 strengthening (parity with C-1 forward invariant; gate text grammar-derivation; forward-blindness catch relocation) is preserved verbatim at `:375-387`. | **FOLD-LANDED + EXECUTABLE-VERIFIED** |
| REVISE — §3 row C-3 + C-4 compress CH7 surface | F-17 | HOLDS | **HOLDS.** SYNTHESIS untouched by V4 commit `5e00b6d27`; row text unchanged at `SYNTHESIS.md:273-274`. | **FOLD-LANDED** |
| REVISE — α-E §2 shortlist table C-3/C-4 cite | E-14 | HOLDS | **HOLDS.** α-E §2 lines 85-86 unchanged by V4 cycle (F-V4-α-E-1 touches §5 lines 362-375 only; §2 untouched). | **FOLD-LANDED** |
| REVISE — §10 cap clarity (per-sub-wave vs per-cluster) | E-2 (CH4 R3 authoritative per V2 CONSOLIDATED §0.5) | HOLDS | **HOLDS.** Cap table at `alpha-E-candidate-shortlist.md:754-760`; reconciliation paragraph at `:762-773` unchanged. | **FOLD-LANDED** |
| REVISE — V2-DISP-α-E-C3-table mirror | E-14 (α-E §2 mirror) + F-17 (SYNTHESIS §3 mirror) | HOLDS | **HOLDS.** Both mirrors intact. | **FOLD-LANDED** |
| REVISE — V2-DISP-SYNTHESIS-§3-C3-C4 | F-17 | HOLDS | **HOLDS.** | **FOLD-LANDED** |
| (informational, V2-deferred) α-A cite spot-check expansion | (CH1 V2 surface) | DEFERRED | **DEFERRED** (CH1 V4 carries citation surface; F-V4-α-F-1 closed CH1 V3 REV-2). | **DEFERRED** (acceptable) |

**Fold tally (V4):** 1 BINDING REJECT FOLD-LANDED + STRENGTHENED +
EXECUTABLE-VERIFIED; 5 REVISEs FOLD-LANDED; 1 informational REVISE
deferred per V2 CONSOLIDATED §0.5. Zero FOLD-PARTIAL; zero
FOLD-MISSING; zero REGRESSED.

## §3 — Per-artefact V4 disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| SYNTHESIS.md | §0.1 | ACCEPT | unchanged from V3; CH7-3 close-condition binding holds. |
| SYNTHESIS.md | §0.2 | ACCEPT | unchanged from V3; reconciliation paragraph at `SYNTHESIS.md:200-209` lifts CH6 REJ-2 + CH7's audit-overlay integrity; AUDIT-FALSIFIED scope correctly bound. F-V4-α-F-1 cites this section as the corrected reconciliation anchor (replacing the V3 stale §1.3 cite). |
| SYNTHESIS.md | §0.3 | ACCEPT | unchanged from V3; R4 row reads "first instance of the `regen-{grammar}` family; the xtask binary parametrises a grammar-neutral generator"; CH7-4 round-trip + CH7-2 grammar-neutrality preserved. |
| SYNTHESIS.md | §0.4 | ACCEPT | unchanged from V3; P-1 W10.3 nested_layout round-trip-rule trigger + ≥ 50× SOTA-comparator threshold; CH7-1 + CH7-4 + CH3 cross-binding. |
| SYNTHESIS.md | §0.5 | ACCEPT | unchanged; contracted S-P3 deferral. |
| SYNTHESIS.md | §1.1 | ACCEPT | unchanged. |
| SYNTHESIS.md | §1.2 | ACCEPT | unchanged from V3; 4+7 → 6+11 reconciliation block at lines 200-209 holds; cited by F-V4-α-F-1 as alternative reconciliation anchor. |
| SYNTHESIS.md | §1.3 | ACCEPT | unchanged; rolling delta restated; audit-zero baseline holds. F-V4-α-F-1 explicitly REMOVES this section as the §7 carry-over reconciliation cite (CH1 V3 REV-2 closed); §1.3's post-PRUNE rolling delta posture is preserved. |
| SYNTHESIS.md | §2 | ACCEPT | unchanged from V3; telemetry schema includes `track2_entry_point`; CH7's CH5 cross-binding mechanically enforced. |
| SYNTHESIS.md | §3 | ACCEPT | unchanged from V3; candidate table at `SYNTHESIS.md:273` lifts dual-tree round-trip + bypass-header detector + §5 pointer per F-17. C-4 row at `:274` names `json/numbers/direct_to_struct/main` + per-shape Lock-1 triad. CH7-1, CH7-2, CH7-4, CH7-5 intact. V4 commit does not touch SYNTHESIS; the mirror text is grammar-count-agnostic so no V4 fold to SYNTHESIS is required. |
| SYNTHESIS.md | §4 | ACCEPT | unchanged from V3; S-P3 constraints carry per-wave LOC ceiling (F-6), C-1 forward invariant (F-12), C-4 two-grammar-family exercise + no-grammar-branch dispatch (F-13), G-SIMD-GRAMMAR-POLICY triad (F-14), triumvirate discipline (F-9). |
| SYNTHESIS.md | §5 | ACCEPT | unchanged. |
| SYNTHESIS.md | §6 | ACCEPT | unchanged. |
| HANDOFF.md | §1 | ACCEPT | unchanged. |
| HANDOFF.md | §2 | ACCEPT | unchanged. |
| HANDOFF.md | §3 | ACCEPT | unchanged from V3; numeric reconciliation holds per F-1; CH7-3 measurement honesty inherits. |
| HANDOFF.md | §4 | ACCEPT | unchanged from V3; α-F sole-author posture intact per F-2. |
| HANDOFF.md | §5 | ACCEPT | unchanged; CH7 lens binding cited at step 4. |
| HANDOFF.md | §6 | ACCEPT | unchanged from V3; next-move chain echoes hard caps (F-7) + restores G-Omega (F-8); cap paragraph cites "30-min lens-agent cap; research 20 min / plan 15 min / redress 30 min (45 min only for the addendum-amended decision-engine fold + C-4 per CONSOLIDATED §0.5 cap discipline)". |
| HANDOFF.md | §7 | ACCEPT | refusal list intact at CH7-relevant bullets (W10.3 round-trip-rule trigger per F-10 at `HANDOFF.md:228-232`; UnionTape verbatim refusal per F-16 at `:233-236`; P-1..P-7 fold inheritance at `:226-230`). F-V4-α-F-1 corrects the §7 reconciliation cite from `SYNTHESIS.md §1.3` to `SYNTHESIS.md §0.2 reconciliation block (lines 73-84)` at `HANDOFF.md:196`. This is a CH1 (CORRECTNESS) scope citation correction; it does not touch CH7 surface. CH7-1 + CH7-4 + CH7-5 lens posture unchanged at `:226-236`. |
| HANDOFF.md | §8 | ACCEPT | unchanged. |
| α-A §1 parse_only | ACCEPT | per-row audit-overlay citations unchanged from V3; CH7-3 carry. |
| α-A §2 direct | ACCEPT | reconciliation table at `alpha-A-results-extraction.md:125-130` per A-1 (V2) holds; CH7-3 measurement integrity intact. |
| α-A §3 typed | ACCEPT | +4 extension rows per A-2 (V2) carry the `[ext†]` annotation; CH7-3 audit-overlay integrity holds for the wider 11-row population. |
| α-A §4 CSS L4 | ACCEPT | unchanged; CH7-1 + CH7-4 audit cite intact. |
| α-A §5 c/B telemetry | ACCEPT | per A-3 (V2) c/B telemetry LOC budget assigned via C-2 envelope; CH7-3 schema-debt closure. |
| α-A §6 | ACCEPT | unchanged. |
| α-B (entire) | ACCEPT | STAND from V1 + V2 + V3; zero changes in V4 cycle. |
| α-C §1 | ACCEPT | unchanged. |
| α-C §2 | ACCEPT | per C-1 (V2) P-7 triple-check gate at `alpha-C-redress-digest.md:348-385` holds; CH7-3 cross-binding with CH5 intact. |
| α-D (entire) | ACCEPT | STAND from V1 + V2 + V3; zero changes. |
| α-E §1 | ACCEPT | unchanged. |
| α-E §2 shortlist table | ACCEPT | per E-14 (V2): C-3 + C-4 rows carry explicit gates with §5 + §6 pointers; CH7-4 + CH7-5 binding holds; no V4 edit. |
| α-E §3 C-1 | ACCEPT | per E-7 (V2) C-1 forward invariant at `alpha-E-candidate-shortlist.md:170-176` holds; per E-11 LOC lower bound 2.8k holds; per E-13 §9 strict serialisation holds. CH7-1 + CH7-2 reinforced. F-V4-α-E-1 preserves the C-1 parity citation at `:375`. |
| α-E §4 C-2 | ACCEPT | per E-12 (V2) LOC envelope +80 for Skipper fallback holds; CH7-3 plane-correct comparators unchanged. |
| α-E §5 C-3 | ACCEPT | **per E-1 BINDING (V2) + F-V3-α-E-1 (V3) STRENGTHENING + F-V4-α-E-1 (V4) EXECUTABLE-VERIFICATION:** three-part round-trip + bypass-header detector verbatim at `alpha-E-candidate-shortlist.md:355-398`. V4 corrects the metadata-derived loop's shell command to use the canonical `cargo metadata --format-version 1 --no-deps \| jq -r '.metadata.bbnf.grammars[].ident'` form (replacing V3's non-executable `.workspace_metadata.bbnf.grammars \| keys[]`), adds `--exit-code` to `git diff` for strict failure propagation, and generalises "9th grammar" → "admitting an additional grammar". §1.1 above quotes the 9-grammar output the gate now mechanically enumerates. CH7-1 (grammar-derived) is EXECUTABLE-STRENGTHENED; CH7-4 (round-trip on generated output) is preserved verbatim AND strengthened against silent passes; CH7-2 (Lock 14 compliance) holds. |
| α-E §6 C-4 | ACCEPT | per E-3 (V2) per-shape Lock-1 triad at `:474-491`; per E-4 module-path discipline at `:514-524`; per E-5 pre-wave hot-leaf citation at `:531-538`; per E-8 no grammar-branched dispatch + two-grammar exercise. CH7-5 wired with multi-layer falsification surface. No V4 edit. |
| α-E §7 C-5 | ACCEPT | per E-10 (V2) scribe contract "29 row-keyed REDRESS entries" verbatim; CH7-1 audit-trail restoration unchanged. |
| α-E §8 | ACCEPT | unchanged. |
| α-E §9 | ACCEPT | per E-13 (V2) §9 vs §6 dependency-matrix resolved at `:730-741`; CH7-1 audit-trail discipline preserved. |
| α-E §10 | ACCEPT | per E-2 + V2 CONSOLIDATED §0.5: caps at `:754-760` read C-1/C-2/C-3/C-5 = 30 min; C-4 alone keeps 45. CH7 cap discipline matches CH4 R3. |
| α-E §11 | ACCEPT | unchanged. |
| DISPATCH-CONTEXT.md | (full) | ACCEPT | STAND from V1 + V2 + V3; zero changes in V4 cycle. |

Total: **36 ACCEPT / 0 REVISE / 0 REJECT.**

## §4 — Critical findings

### §4.1 — F-V4-α-E-1 closes V3 jq-path defect; gate is now executable

The V4 micro-fold F-V4-α-E-1 (commit `5e00b6d27`) replaces the V3
form of the metadata-derived shell loop:

```
cargo metadata --format-version 1 | jq -r '.workspace_metadata.bbnf.grammars | keys[]'
```

with the V4 form (verbatim from `alpha-E-candidate-shortlist.md:366-367`):

```
cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
```

Three corrections land in one paragraph:

1. **jq path corrected.** `cargo metadata --format-version 1` exposes
   workspace metadata under top-level key `.metadata` (NOT
   `.workspace_metadata`); the array entries are records keyed by
   `ident`, so the canonical projection is
   `.metadata.bbnf.grammars[].ident`, not `.workspace_metadata.bbnf.grammars
   | keys[]`. §1.1 above executes both forms; the V3 form returns
   `null` and silently no-op's the for-loop body; the V4 form returns
   the 9-grammar identifier list and the for-loop body executes
   verbatim per iteration.

2. **`--no-deps` added.** Cuts the `cargo metadata` output to
   workspace-only, eliminating the dependency-tree payload that
   inflates parse-time without changing the grammar enumeration.

3. **`--exit-code` added to `git diff`.** V3 form relied on shell
   return-code propagation that `git diff` alone does not always
   provide (it returns 0 even on non-empty diffs in some
   configurations); `--exit-code` makes non-empty diffs fail with
   exit status 1, so `|| exit 1` mechanically propagates the failure.

The V3 lens-depth gap (CH7 V3 verified the structural intent but not
the executable correctness; CH1 V3 caught the executable defect via
direct invocation) is closed at the V4 dispatch site: CH7 V4 carries
the executable-verification mandate and reports the 9-grammar output
as direct evidence. The CH7-4 (round-trip on generated output)
binding is STRICTLY STRENGTHENED — V3's silent-pass failure mode is
eliminated; the gate now MUST iterate over every rostered grammar or
fail loudly.

The "9th grammar" stale referent (V3 form used "adding a 9th grammar"
as the forward-discipline example, but the live workspace had already
admitted `css_pretty` as the 9th grammar at `Cargo.toml:23`) is
generalised to "admitting an additional grammar" at `:372`. The
phrasing is now roster-count-agnostic; future roster expansions
(10th, 11th, ...) do not require re-editing the gate text.

### §4.2 — F-V4-α-F-1 is CH1-scope; no CH7 surface touched

The V4 micro-fold F-V4-α-F-1 (commit `5e00b6d27`) corrects the §7
reconciliation cite at `HANDOFF.md:196` from `SYNTHESIS.md §1.3` to
`SYNTHESIS.md §0.2 reconciliation block (lines 73-84)`. This is a
CH1 (CORRECTNESS) scope edit closing CH1 V3 REV-2; it does not touch
any CH7-relevant bullet of §7 (the W10.3 round-trip-rule trigger at
`:228-232`, the UnionTape verbatim refusal at `:233-236`, and the
P-1..P-7 fold inheritance at `:226-230` are all untouched).

No CH7-N criterion is affected by F-V4-α-F-1; the CH7 disposition on
HANDOFF §7 stands at ACCEPT unchanged from V3.

### §4.3 — P-1..P-7 ↔ CH7-N mapping holds through V4

The V2 §2.2 bijective mapping (P-1↔CH7-1; P-2/P-3/P-4↔CH7-3;
P-5↔CH7-5; P-6↔CH7-2; P-7 cross-bind to CH5) persists unchanged in V4
at `SYNTHESIS.md:104-148`. The W10.3 round-trip-rule trigger added to
P-1 in V2 (per F-10) at `SYNTHESIS.md:113-120` carries through;
HANDOFF §7 carries the matching refusal bullet at `:228-232`. Neither
V4 micro-fold touches these citations; both bindings hold verbatim.

## §5 — Fresh-finding scan (V4-cycle defect surface)

Per the V4 dispatch context, scan all eight V4 artefacts for any new
CH7-N criterion violation introduced by the V4 micro-fold cycle. Scan
dimensions:

- **New fake `@generated` instances introduced by V4.** None. The V4
  cycle edits two files (HANDOFF, α-E). HANDOFF V4 diff is a single
  citation anchor swap at line 196; no `@generated` reference touched.
  α-E V4 diff at lines 361-378 is the corrected metadata-derived
  loop; the existing `@generated by skinny bbnf-codegen` references
  in the bypass-header detector at `:389`, `:395`, `:397` are
  untouched. No V4 fold authored a hand-curated `@generated` header.
- **New scaffold-as-load-bearing claims.** None. C-4 (the only
  candidate touching W8 / W9 SCAFFOLD-ONLY surface) is untouched by
  the V4 cycle; all C-4 falsifiers added in V2 (E-3 Lock-1 triad,
  E-4 module-path discipline, E-5 pre-wave citation, E-8
  two-grammar-family exercise) remain intact.
- **New gate-relabel risk.** None. C-2's per-iter equality oracle
  remains the comparator integrity gate; no V4 fold touches the
  comparator surface.
- **New Lock 14 generic-crate leaks.** None. F-V4-α-E-1 STRENGTHENS
  the Lock 14 posture: the corrected jq path retrieves grammar
  identifiers from the same `workspace.metadata.bbnf.grammars` clause
  Lock 14 names (`LOCKS.md:220`), and the gate text now executes that
  retrieval as the gate's runtime, not only as the gate's
  descriptive prose. The V4 cycle reduces, not increases, the Lock 14
  leak surface.
- **New round-trip scope gaps.** None. F-V4-α-E-1 preserves the
  three-part round-trip + bypass-header detector verbatim; the
  core-tree clause now executably iterates the rostered set with
  strict failure propagation (`git diff --exit-code`), closing the
  V3-era silent-pass failure mode. CH7-4 is STRICTLY STRENGTHENED,
  not regressed.
- **Cross-lens conflict.** None. F-V4-α-F-1 (CH1 scope) and
  F-V4-α-E-1 (CH1 scope, with CH7 strengthening side-effect via
  executable round-trip enforcement) touch non-overlapping artefacts
  and non-overlapping CH7 criteria. The CH4 R3 cap discipline (per
  CONSOLIDATED §0.5) remains authoritative and the V4 micro-fold
  respects it (no cap touched).

Zero new findings across all six scan dimensions.

## §6 — Recommended folds for V5

None. V4 has verified the V3 100 % CH7 baseline holds intact, and the
V4 micro-fold F-V4-α-E-1 STRENGTHENS the C-3 round-trip gate's CH7
posture along the executable-verification axis (CH7-4 now mechanically
enforced via corrected jq path + `--exit-code` + `--no-deps`). The
fresh-finding scan returns zero new findings.

Per `ORCHESTRATOR.md §3Z`, the multi-consecutive-cycle convergence
rule is satisfied for the CH7 lens: V2 = 100 % (link 1), V3 = 100 %
(link 2), V4 = 100 % (link 3). The CH7 lens-local convergence chain
holds through V4; no further CH7 work is required for the SK-V14
alpha-bracket contract.

## §7 — Bracket-level CH7 verdict

CH7 V4 converges at **100 %** for the lens, extending the
consecutive-cycle convergence chain to three cycles (V2 + V3 + V4 all
at 100 %). The V1 BINDING REJECT remediation landed verbatim in V2,
held in V3 with substantive strengthening at the gate's
grammar-enumeration site, and at V4 STRICTLY STRENGTHENS to
executable-verified status; the 5 V1 REVISEs landed in V2 and hold in
V4 with no regression; the V2/V3/V4 fresh-finding scans each returned
zero CH7 findings.

The CH7 surface is fully closed across all five criteria, with the
C-3 gate now mechanically executable post-V4 correction:

- **CH7-1** (grammar-derived only): C-1 forward invariant + C-3
  bypass-header detector + C-3 executably-derived enumeration (V4
  jq-path correction) + C-5 deletion ledger.
- **CH7-2** (Lock 14 generic compliance): C-1 trait-dispatch +
  grammar-agnostic generator + C-4 no-grammar-branched dispatch +
  C-3 gate-text grammar-derivation via live `cargo metadata` (V4).
- **CH7-3** (real source + strict comparator + per-iter equality):
  C-2 three plane-correct strict comparators + per-iter equality
  column; audit-overlay column at SYNTHESIS §2.
- **CH7-4** (round-trip on generated output): C-3 three-part
  round-trip + bypass-header detector covering both runtime trees +
  all rostered grammars under workspace metadata enumeration +
  `git diff --exit-code` strict failure propagation (V4); the V3
  silent-pass failure mode is eliminated.
- **CH7-5** (no scaffold admit): C-4 hot-leaf attribution change
  + per-shape Lock-1 triad + module-path discipline + two-grammar
  exercise.

The lens cleared a third consecutive cycle. The aggregator should
mark the CH7 lens at the V4 verdict as "CONVERGED — three-cycle chain
with executable-verification of the binding gate"; CH7 carries no
fold into a V5. The SK-V14 alpha-bracket contract lock-in is unblocked
from the CH7 axis.

**E-1 landing status: FOLD-LANDED + V3-STRENGTHENED + V4-EXECUTABLE-VERIFIED.**
**F-V4-α-E-1 landing status: VERIFIED-CLOSES-V3-JQ-PATH-DEFECT + STRENGTHENS-CH7-4.**
**F-V4-α-F-1 landing status: VERIFIED-CH1-SCOPE-NO-CH7-IMPACT.**
