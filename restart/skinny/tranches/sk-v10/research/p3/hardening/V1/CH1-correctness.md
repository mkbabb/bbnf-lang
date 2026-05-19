# SK-V10 S-P3 V1 CH1 Correctness

Verdict: REVISE

Acceptance percentage: 82%

## Scope

Audited numeric floors, gate/source mappings, dependency order, outcome/gate
names, run-id claims, and contradictions between P3 artifacts and the top-level
SK-V10 SPEC/DISPATCH contract.

Primary sources reviewed:

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` Section 3, especially CH1
  correctness criteria at lines 102-117.
- `restart/skinny/tranches/sk-v10/SPEC.md`.
- `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`.
- `restart/skinny/tranches/sk-v10/research/p3/p3a-candidate-shortlist.md`.
- `restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md`.
- `restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md`.
- `restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md`.
- `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md`.
- `restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md`.
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`.
- `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`.
- `skinny/RESULTS.md` and `skinny/REDRESS.md` as needed.

## Findings

### F1 - P3-C gate wave numbers contradict the integrated SPEC manifest

Severity: blocking REVISE.

`SPEC.md` defines the binding W0-W10 topology: W2 is Direct Row-Table
Reclamation, W3 is the W3/parse-only firewall, W4 is `instruments` typed
admission, W5 is root-type proof, W6 is root typed row admission, W7/W8 are
micro-proof waves, W9 is kernel production, and W10 is direct residual behavior
(`SPEC.md:160-175`, `DISPATCH-PROMPT.md:42-57`).

`p3c-falsifiability-gates.md` uses an incompatible older compressed sequence:
W2 is `instruments` typed admission, W3 is root-type typed generalization, W4
is primitive micro-proof, W5 is production kernel wiring, and W6 is rejected and
inventory routes (`p3c-falsifiability-gates.md:198-263`,
`p3c-falsifiability-gates.md:265-368`). This makes gate names and dependency
references ambiguous for the implementation agents because `G-W2-*` and
`G-W3-*` mean different things depending on which artifact is read.

Required fix: update P3-C to use the final SPEC wave numbering and gate names,
or add an explicit non-authoritative alias table that maps every P3-C compressed
gate to the final SPEC wave. The safer correction is to rewrite P3-C Sections
3.2-3.6 to match `G-W2-DIRECT-RECLAMATION`, `G-W3-PARSE-FIREWALL`,
`G-W4-INSTRUMENTS-TYPED`, `G-W5-ROOT-TYPED-PROOF`,
`G-W6-ROOT-TYPED-ROW`, `G-W7-STRING-MICROPROOF`,
`G-W8-ESCAPE-SEGMENT-MICROPROOF`, `G-W9-KERNEL-PRODUCTION`, and
`G-W10-DIRECT-RESIDUAL`.

### F2 - P3-E per-wave ledger also uses the wrong wave map

Severity: blocking REVISE.

The top-level SPEC binds W0 as telemetry, W1 as direct contract, W2 as direct
row reclamation, W3 as firewall, W4 as instruments typed admission, W5-W6 as
root typed proof/admission, W7-W9 as primitive proof/production, and W10 as
direct residual (`SPEC.md:160-175`). P3-E instead labels W2 as `instruments`,
W3 as root-type typed generalization, W4 as existing-substrate unicode/string
kernel pair, and W5 as comparator/telemetry refresh (`p3e-preblocked-ledger.md:119-227`).

This is more than naming drift: P3-E is the binding negative authority named by
the SPEC (`SPEC.md:672-691`) and by the dispatch prompt
(`DISPATCH-PROMPT.md:156-176`). If an implementation agent follows P3-E
literally, the wrong pre-blocks can be attached to the wrong SPEC wave.

Required fix: realign P3-E per-wave sections to the SPEC W0-W10 names, or
replace the per-wave headings with candidate-scoped headings that cannot be
mistaken for dispatch wave numbers. The global pre-block table is usable; the
per-wave labels need correction.

### F3 - C8/C9 are listed as W9-production candidates but have no final SPEC proof wave

Severity: blocking REVISE.

P2-G marks `C8-digit-number-proof` as proof-first and potentially row-gated, and
marks `C9-whitespace-class-skip` as maintain-only unless paired with an exact
caller and floors (`p2g-candidate-ledger.md:44-45`). P3-A shortlists C8 with a
full proof gate and leaves C9 out of the shortlist as maintain-only
(`p3a-candidate-shortlist.md:386-425`, `p3a-candidate-shortlist.md:474-479`).
P3-C also includes C8 and C9 in primitive proof/production gates
(`p3c-falsifiability-gates.md:267-294`, `p3c-falsifiability-gates.md:307-327`).

The integrated SPEC, however, gives W7 only `C4` or `C5`, W8 only `C6` or `C7`,
then allows W9 to consume a proven `C4`-`C9` primitive (`SPEC.md:171-173`,
`SPEC.md:479-558`, `SPEC.md:560-578`). `DISPATCH-PROMPT.md` repeats the same
shape (`DISPATCH-PROMPT.md:53-61`). There is no final SPEC wave that can prove
C8 digit/number or C9 whitespace before W9 consumes them.

Required fix: either narrow W9 to proven `C4`-`C7`, or add explicit proof-wave
coverage for C8/C9 before W9, with scalar oracle, checkasm or differential
parity, caller microbench, exact caller, W10b maintain floors, and row-floor
rules. If C9 remains unshortlisted maintain-only, remove it from W9 candidate
range unless a future SPEC wave promotes it through CHALLENGE.

### F4 - W9 dependency wording over-constrains and under-specifies the relevant proof

Severity: REVISE.

P3-B says W9 depends on `W7 + W8` and cannot dispatch if either proof is missing
(`p3b-wave-sequencing.md:61-63`, `p3b-wave-sequencing.md:75-77`). The SPEC and
DISPATCH prompt say W9 is conditional on `W7/W8 proof` and may consume only a
W7/W8-proven primitive (`SPEC.md:173`, `SPEC.md:576-578`,
`DISPATCH-PROMPT.md:55`, `DISPATCH-PROMPT.md:59-63`). That slash wording is
ambiguous: a C4/C5 string primitive should not require an unrelated C6/C7 escape
proof, while a C6/C7 escape/segment primitive should require its own relevant
proof and any stated dependency on W7.

Required fix: replace `W7 + W8` / `W7/W8 proof` with "the relevant accepted
W7 or W8 proof for the exact primitive and caller; W8 additionally requires W7
only when its selected primitive depends on the string proof." Apply this in
P3-B, SPEC, and DISPATCH.

## Verified Correct

- Numeric floors are consistent with the current `skinny/RESULTS.md` values and
  the `ceil(sonic / 1.10)` rule. Examples: direct floors in `SPEC.md:73-89`
  match direct rows in `RESULTS.md:6`, `RESULTS.md:12`, `RESULTS.md:14`,
  `RESULTS.md:17`, `RESULTS.md:19`, `RESULTS.md:22`, `RESULTS.md:25`,
  `RESULTS.md:27`, `RESULTS.md:32`, `RESULTS.md:34`, `RESULTS.md:36`,
  `RESULTS.md:38`, `RESULTS.md:42`, and `RESULTS.md:44`; direct guard floors in
  `SPEC.md:93-97` match `RESULTS.md:9`, `RESULTS.md:29`, and `RESULTS.md:40`;
  typed maintain floors in `SPEC.md:99-108` match `RESULTS.md:7`,
  `RESULTS.md:10`, `RESULTS.md:15`, `RESULTS.md:20`, `RESULTS.md:23`, and
  `RESULTS.md:30`.
- The W10b maintain block in `SPEC.md:110-122` matches REDRESS W10b floor
  evidence in `REDRESS.md:2824-2837` and `REDRESS.md:2882-2895`.
- Run-id claims are honest: the opening authority is explicitly the inherited
  W1-rendered SK-V9 snapshot with run id
  `sk-v9-open:criterion-fnv64-a1e8a51ae806d386` (`SPEC.md:55-60`,
  `p3d-telemetry-schema.md:43-47`, `p3d-telemetry-schema.md:146-152`).
- Outcome enum and telemetry binding are consistent with SK-V9's 36-identifier
  schema and 10-outcome set (`SPEC.md:47-48`, `SPEC.md:693-717`,
  `DISPATCH-PROMPT.md:178-188`, `p3d-telemetry-schema.md:32-64`,
  `p3d-telemetry-schema.md:85-112`, `sk-v9/SPEC.md:202-230`).
- W3 retirement is carried through the top-level SPEC and DISPATCH prompt
  (`SPEC.md:10-13`, `SPEC.md:123-158`, `DISPATCH-PROMPT.md:12-16`,
  `REDRESS.md:2910-2922`).

## Required Fixes

1. Normalize all P3-C and P3-E wave headings, dependencies, and gate references
   to the final SPEC W0-W10 manifest.
2. Decide whether C8/C9 are in SK-V10's final executable plan. If yes, add proof
   coverage before W9. If no, narrow W9 from `C4`-`C9` to the actually proved
   candidate set.
3. Clarify W9 dependency text as "relevant W7 or W8 proof" rather than
   requiring both W7 and W8 or using ambiguous slash wording.
4. Re-run CH1 after the textual fold; arithmetic and telemetry do not require
   recalculation unless the SPEC changes floor tables.

## Acceptance Rationale

The packet is mostly correct on quantitative thresholds, telemetry binding,
run-id handling, and high-level pre-blocks. The remaining defects are
dispatch-critical because they can send implementation agents to the wrong wave
gate or authorize W9 candidates that lack a final proof wave. That warrants
REVISE rather than ACCEPT, but not REJECT because the corrections are textual
alignment and candidate-range decisions, not a failed numeric basis.
