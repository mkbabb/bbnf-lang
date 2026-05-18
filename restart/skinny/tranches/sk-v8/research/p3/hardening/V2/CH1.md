# SK-V8 S-P3 Hardening V2 CH1 - Correctness

Date: 2026-05-18.
Lens: CH1 correctness.
Scope: S-P3 V2 hardening fold and live SK-V8 packet: `SPEC.md`,
`DISPATCH-PROMPT.md`, `HANDOFF.md`, `p3-v2-hardening-fold.md`, P3-A through
P3-F, and `HARDENING-S-P3-V1-CONSOLIDATED.md`.

## Verdict

REVISE.

Confidence: 90%.

The V2 packet resolves the V1 CH1 numeric-gate blocker and preserves the
G-Alpha implementation lock. It does not fully resolve the V1 CH1 citation
hygiene fold: many P3 claims now cite only bare local paths instead of
file:line references or stable section references. That removes stale line
numbers, but it is not line-resolved traceability under ORCHESTRATOR CH1.

## Blockers

### B1 - Citation hygiene was weakened, not resolved

ORCHESTRATOR CH1 requires correctness claims to cite file:line, commit SHA,
RESULTS row, or REDRESS entry that resolves
(`restart/prompts/ORCHESTRATOR.md:81-88`). V1 consolidation required the V2 fold
to refresh volatile P3 path:line citations against the folded SPEC/DISPATCH or
replace them with stable section references; the V2 packet must not cite a line
range that points to the wrong section
(`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:48-50`).

The V2 fold says local P3 citations were "normalized away from stale live-packet
line numbers"
(`restart/skinny/tranches/sk-v8/research/p3/p3-v2-hardening-fold.md:20`). In
practice, P3-A through P3-E contain large numbers of bare-path citations such as:

- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:7`
  cites the Pass Alpha goalset to `SPEC.md` twice without lines or section
  references.
- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:14`
  cites SC-1 and SPEC Section 6 claims with bare file paths only.
- `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:12-16`
  cites V7 governance, PASS-2 convergence, and current measured state with bare
  file paths only.
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:12-18`
  cites PASS-3 scope, the W0-W6 manifest, V7 governance, SYNTHESIS, and RESULTS
  with bare file paths only.
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:14-18`
  cites schema-v3, Alpha extraction, SPEC telemetry, and W3 blocking conditions
  with bare file paths only.
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:12-18`
  cites V7 governance, S-P3 wave requirements, pre-block coverage, and hard-cap
  authority with bare file paths only.

These references resolve as local paths, but they do not resolve to the claimed
line or stable section. This is still a CH1 correctness defect because the
reader cannot verify the exact claim without re-auditing whole documents.

Required fold: restore line-resolved citations for material claims in P3-A
through P3-F, or replace each volatile line reference with a stable section
reference that names the exact section in the target file. Bare repeated
citations such as `SPEC.md`, `SPEC.md` are not sufficient for CH1.

## V1 CH1 Fold Disposition

Resolved:

- W2 candidate typed seed table is folded into SPEC Section 0.5:
  `canada/real_typed_struct`, `numbers/real_typed_struct`,
  `unicode_basic/real_typed_struct`, `citm_catalog/real_typed_struct`, and
  `apache_builds/real_typed_struct` with strict Mbps floors
  (`restart/skinny/tranches/sk-v8/SPEC.md:177-189`).
- W2 now states `Track 1 Mbps >= ceil(sonic-rs strict Mbps / 1.10)` and requires
  post-W0 recomputation from `SK-V8-open` when the strict anchor changes
  (`restart/skinny/tranches/sk-v8/SPEC.md:177-181`).
- W2 existing real-typed GO floors and existing direct GO guard floors are
  present (`restart/skinny/tranches/sk-v8/SPEC.md:157-175`).
- W2 dispatch is bounded to the Section 0.5 seed table unless a later accepted
  S-P3 revision expands it (`restart/skinny/tranches/sk-v8/SPEC.md:460-468`;
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:117-125`).
- The unresolved wildcard path was fixed: W0 artifacts now point to the concrete
  research directory plus the `wave-0-<topic>.md` naming pattern, not a missing
  glob (`restart/skinny/tranches/sk-v8/SPEC.md:331-335`;
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:62-64`).

Not resolved:

- V1 citation hygiene required refreshed path:line references or stable section
  references. V2 mostly removed line numbers and left bare paths. See B1.

## Correctness Evidence

The live SPEC is falsifiable in the core CH1 sense:

- W0 names all 38 current rows, required telemetry, +/-1.0% movement against
  `SK-V8-open`, malformed sidecar rejection, and no behavior change
  (`restart/skinny/tranches/sk-v8/SPEC.md:343-383`).
- W1 binds CostFacts and strict-admission refusal gates, with full-table maintain
  against `SK-V8-open` (`restart/skinny/tranches/sk-v8/SPEC.md:402-440`).
- W2 has named typed candidate rows and thresholds after V2
  (`restart/skinny/tranches/sk-v8/SPEC.md:177-189`;
  `restart/skinny/tranches/sk-v8/SPEC.md:460-504`).
- W3 keeps Tier A as one retained `Tape`, requires W0/W1, challenge acceptance,
  exact owners, selected rows, scalar/checkasm when relevant, measured-path
  proof, and generated retained parser production consumer
  (`restart/skinny/tranches/sk-v8/SPEC.md:536-586`).
- W4 names direct guard selection constraints and strict direct-row validation
  against `SK-V8-open` (`restart/skinny/tranches/sk-v8/SPEC.md:623-658`).
- W5/W6 remain audit and close-reconciliation gates, not performance
  shortcuts (`restart/skinny/tranches/sk-v8/SPEC.md:663-765`).

Strict-vs-strict comparator discipline is preserved. The SPEC admits same-run
strict anchors only when output plane matches and validation happens inside the
measured row; lossy, permissive, stale, historical, view-boundary, and
sidecar-only evidence remain guard telemetry
(`restart/skinny/tranches/sk-v8/SPEC.md:61-77`;
`restart/skinny/tranches/sk-v8/SPEC.md:230-248`).

Local link existence is not the blocker. The checked local paths in SPEC,
DISPATCH, HANDOFF, the V2 fold, V1 consolidated, and P3-A through P3-F resolve.
The remaining issue is line/section precision.

## Dispatch Lock

Implementation remains blocked before G-Alpha:

- SPEC Section 11 says `G-Alpha closed` authorizes W0 only, W1-W6 remain
  conditional, and no SK-V8 implementation wave dispatches before G-Alpha
  (`restart/skinny/tranches/sk-v8/SPEC.md:814-825`).
- DISPATCH says S-P3 alone dispatches no implementation wave, G-Alpha signoff is
  still required, and W1-W6 require W0 close plus a fresh wave plan and
  orchestrator/user dispatch (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6-9`).
- HANDOFF preserves the same lock: G-Alpha is required, only W0 is dispatchable
  after G-Alpha, and W1-W6 require W0 closure plus plan augmentation
  (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-7`).

## Required Fold If REVISE

1. Replace bare-path citations in P3-A through P3-F with either current
   file:line citations or stable section references that identify the exact
   target section.
2. Keep the V2 W2 candidate table, recomputation rule, W2 dispatch bound, W0
   naming-pattern fix, dispatch lock, and LOC/time gates unchanged while folding
   the citation repair.
3. Re-run local path validation after the citation repair to ensure no new
   broken links, stale path:line citations, or future-artifact globs are
   introduced.
