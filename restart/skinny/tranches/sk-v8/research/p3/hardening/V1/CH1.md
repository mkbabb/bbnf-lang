# SK-V8 S-P3 Hardening V1 CH1 - Correctness

Date: 2026-05-18.
Lens: CH1 correctness.
Scope: current S-P3 V1 packet after P3-F fold: P3-A through P3-F,
`SPEC.md`, `DISPATCH-PROMPT.md`, `HANDOFF.md`, S-P2 SC-1..SC-6, V7
consolidation, `skinny/RESULTS.md`, and `skinny/REDRESS.md`.

Review questions: every shortlist candidate traces to S-P2 or Alpha evidence;
every falsifiability gate is measurable with named rows and thresholds; gates
compare against `SK-V8-open`; comparator deltas stay on the strict plane; local
document links and path:line citations are not broken.

## Verdict

REVISE.

Confidence: 93%.

Most of the S-P3 structure is correct: W0 is the only initially dispatchable
wave after G-Alpha, W1-W6 remain conditional, strict-vs-strict admission is
preserved, the W3 lead is bounded to Tier A tape plus structural-projection
union, and the packet keeps the 90-minute implementation/redress cap
(`restart/skinny/tranches/sk-v8/SPEC.md:228-247`;
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6-9`). The packet still
needs revision because one folded gate is not fully measurable and multiple
local path/path:line references are broken or stale.

## Blockers

### B1 - W2 new typed gate lost its named candidate rows and thresholds

PASS-3 requires candidate waves and falsifiability gates to carry named corpus
rows plus Mbps thresholds, and rejects unmeasurable gates
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:56-63`). P3-C supplies the
missing W2 candidate typed seed table:

| Candidate row | sonic strict | Minimum Track 1 Mbps |
|---|---:|---:|
| `canada/real_typed_struct` | 12421 | 11292 |
| `numbers/real_typed_struct` | 12838 | 11671 |
| `unicode_basic/real_typed_struct` | 8502 | 7730 |
| `citm_catalog/real_typed_struct` | 19966 | 18151 |
| `apache_builds/real_typed_struct` | 11122 | 10111 |

Source: `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:120-128`.

The folded SPEC includes only the four existing typed GO maintain floors
(`restart/skinny/tranches/sk-v8/SPEC.md:157-164`). Its W2 entry then defers
exact new rows and thresholds to a later plan
(`restart/skinny/tranches/sk-v8/SPEC.md:426-431`), and the W2 exit gate says
"at least two new generated typed rows" must pass their declared gate
(`restart/skinny/tranches/sk-v8/SPEC.md:442-449`). That is not sufficient for
the S-P3 packet: the live packet must itself bind the allowed seed rows and
numeric floors or explicitly require a later accepted S-P3 revision before W2
redress. Otherwise W2 can become a post-hoc row-picking gate.

Required fold:

1. Add the P3-C W2 candidate typed table to `SPEC.md` Section 0.5 or Section 5.
2. State that a selected new typed row uses
   `Track 1 Mbps >= ceil(sonic-rs strict Mbps / 1.10)`, recomputed from
   `SK-V8-open` if W0 refreshes the same-run strict anchor.
3. Preserve existing typed GO rows with both sonic GO floors and no-regression
   floors; preserve existing direct GO rows as guards.
4. Update `DISPATCH-PROMPT.md` W2 notes so W2 cannot dispatch from a plan that
   names rows outside the folded table without a new accepted S-P3 revision.

### B2 - Stale local path:line citations remain after the P3-F SPEC rewrite

The local paths exist, but several path:line citations no longer point to the
claimed content. This breaks the packet's traceability check.

Examples:

- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:14`
  cites `restart/skinny/tranches/sk-v8/SPEC.md:431` and
  `restart/skinny/tranches/sk-v8/SPEC.md:452` for the W3/Tier A and
  `tape_vs_tape` boundary, but the current `SPEC.md:428-452` is W2 typed-plane
  entry/Track 2 language.
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:14`
  cites `restart/skinny/tranches/sk-v8/SPEC.md:194-204` as the W0-W6 wave
  manifest, but the current `SPEC.md:191-204` is the W4 direct planning-floor
  table. The current wave manifest is `restart/skinny/tranches/sk-v8/SPEC.md:228-238`.

Required fold: refresh P3-A through P3-F path:line citations against the folded
SPEC/DISPATCH, or replace volatile live-SPEC line references with stable section
names plus current section links in the V2 artifacts. The live packet should
not contain citations that point to the wrong section.

### B3 - One unresolved wildcard path is presented as a local markdown path

Path validation found the literal future-output pattern
`restart/skinny/tranches/sk-v8/research/wave-0-*.md` in:

- `restart/skinny/tranches/sk-v8/SPEC.md:301`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:59`

No matching file exists now. As an owner-path pattern this is understandable,
but CH1 was asked to confirm that no broken local document links remain.

Required fold: present the existing directory
`restart/skinny/tranches/sk-v8/research/` as the link/path and describe
`wave-0-<agent>.md` as a naming pattern in prose, or otherwise make the packet's
link checker distinguish non-link glob patterns from document links.

## Candidate Trace Disposition

| Wave | CH1 disposition | Trace and gate notes |
|---|---|---|
| W0 | ACCEPT, conditional on B2/B3 citation cleanup | Traces to Alpha telemetry and P3-A/P3-C/P3-D. The folded gate names all 38 current rows, `SK-V8-open`, required telemetry, malformed sidecar rejection, +/-1.0% movement, and no behavior change (`restart/skinny/tranches/sk-v8/SPEC.md:294-349`; `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:34-62`; `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:55-99`). |
| W1 | ACCEPT, conditional on citation cleanup | CostFacts is gate-bound before behavior, with all 38 current rows maintained and producer-only evidence rejected (`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:64-90`; `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:23`). |
| W2 | REVISE | The candidate traces to Alpha/REDRESS 81 and generated typed product-plane work (`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:24`), but the folded SPEC omits P3-C's named new typed rows and floors. See B1. |
| W3 | ACCEPT, conditional on W0/W1/challenge and B2 cleanup | The lead hypothesis traces to S-P2 Tier A: structural-class cursor migration inside one retained `Tape`, no Tier B string-boundary/parity claim, no new directive/BIR/substrate/API, and same-wave generated retained parser consumer (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md:272-320`; `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:414-480`; `restart/skinny/tranches/sk-v8/SPEC.md:500-559`). |
| W4 | ACCEPT, conditional on row selection by plan | Direct rows remain digest guard rows, not typed product proof. The folded packet provides strict direct floors and requires selected rows to pass Track 1 and Track 2 gates against post-W0 evidence (`restart/skinny/tranches/sk-v8/SPEC.md:188-203`; `restart/skinny/tranches/sk-v8/SPEC.md:581-619`). |
| W5 | ACCEPT, conditional on prior dispositions | Lock 14 grammar-neutral audit is explicit: no generic JSON policy, renamed-residue audit, non-JSON proof, and zero behavior drift unless fixing a recorded drift (`restart/skinny/tranches/sk-v8/SPEC.md:623-673`; `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:58-60`). |
| W6 | ACCEPT, conditional on prior dispositions | Close is reconciliation, not performance work: wave statuses, RESULTS/REDRESS/HANDOFF agreement, no accepted source change without evidence, and Lock 1/Omega routing (`restart/skinny/tranches/sk-v8/SPEC.md:675-725`; `restart/skinny/tranches/sk-v8/HANDOFF.md:135-143`). |

## Comparator And SK-V8-Open Discipline

The strict-plane discipline is correctly preserved in the folded packet. Same-run
strict anchors may support admission only when output plane and measured-row
validation match; lossy/permissive rows and sidecars are planning/flaw-probe
signals only (`restart/skinny/tranches/sk-v8/SPEC.md:61-77`). The current
RESULTS table still reports deferred/view-boundary rows and sidecar provenance
that cannot be strict admission by itself (`skinny/RESULTS.md:5-42`;
`skinny/RESULTS.md:216-219`). SPEC non-negotiables also reject stale sidecar,
permissive, lossy, historical, or view-boundary evidence as strict admission
(`restart/skinny/tranches/sk-v8/SPEC.md:207-225`).

The remaining issue is not comparator laxity; it is the W2 fold gap. W3 and W4
already state post-W0 or `SK-V8-open` comparison duties
(`restart/skinny/tranches/sk-v8/SPEC.md:166-203`;
`restart/skinny/tranches/sk-v8/SPEC.md:527-530`;
`restart/skinny/tranches/sk-v8/SPEC.md:599-605`). W2 must receive the same
explicit folded treatment for new typed rows.

## V6/V7 Governance

V6/V7 governance is correctly carried forward. The V7 consolidation records
6/6 ACCEPT, states that V6 plus V7 form the two consecutive qualifying ACCEPT
cycles, and authorizes S-P3 only - not implementation, W3 redress, or G-Alpha
close (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:7-20`).

The preserved boundaries are also correct for CH1: strict-vs-strict only, Tier A
separate from Tier B, Lock 14 grammar-neutrality, no new directive/BIR/
`BackendShape`/`UnionTape`/public substrate API/parser-owned cursor or facts/
parallel substrate, and `tape_vs_tape` as telemetry rather than W3 production
consumer (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:44-64`).

The live SPEC and dispatch prompt preserve that no implementation wave follows
from S-P3 alone and that G-Alpha closed dispatches W0 only
(`restart/skinny/tranches/sk-v8/SPEC.md:774-785`;
`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6-9`).

## Residual Non-Blocking Risks

- W3 Tier A is still broad for one 90-minute implementation/redress slice once
  scalar oracle, checkasm, generated-output audit, retained view parity, Track 2
  proof, gate refresh, RESULTS, and REDRESS are counted. SPEC correctly requires
  split or REVISE before redress if it cannot fit (`restart/skinny/tranches/sk-v8/SPEC.md:232-247`;
  `restart/skinny/tranches/sk-v8/SPEC.md:556-559`).
- `parse_only` rows remain substrate-guard non-admission unless a separate
  schema/gate amendment proves plane-matched strict eligibility
  (`restart/skinny/tranches/sk-v8/SPEC.md:541-542`).
- P3-F currently records "Blockers: None" and self-verdict ACCEPT
  (`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:78-81`;
  `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:116-118`).
  That is acceptable as an input artifact, but the next fold must reconcile this
  CH1 REVISE so the V2 packet does not claim a blocker-free CH1 surface.

## Required Folds If REVISE

1. Fold P3-C's W2 candidate typed seed table and strict threshold rule into the
   live SPEC, with post-W0 recomputation from `SK-V8-open`.
2. Update W2 dispatch language so row selection is bounded by the folded table
   or by a later accepted S-P3 revision.
3. Refresh stale P3-A through P3-F path:line citations against the folded
   SPEC/DISPATCH, especially references to SPEC W3 and the wave manifest.
4. Replace or classify the unresolved `wave-0-*.md` wildcard path so local link
   validation no longer reports it as a missing document.
5. Carry this CH1 REVISE into the V2 P3-F disposition; do not advance S-P3 on
   the current V1 packet as ACCEPT.
