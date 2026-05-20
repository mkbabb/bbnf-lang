# SK-V12 Pass Alpha CHALLENGE V1 - CH4 Cost / Wave Alignment

Date: 2026-05-20.
Lens: CH4 cost / wave alignment.
Scope: Pass Alpha SK-V11 -> SK-V12 re-bracket under
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
Output: `restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CH4.md`.

## Authority Read

- `restart/prompts/ORCHESTRATOR.md` Section 3W.
- `restart/prompts/pass-contracts/PASS-ALPHA.md` Section 3.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/skinny/tranches/sk-v12/SPEC.md`.

## Overall Disposition

REVISE.

The pin-aware Alpha packet is directionally correct: CSS L4 is first, the
close bar is `lightningcss_mbps + 1`, W0 is revalidate-not-redo in
Alpha-F/SYNTHESIS/HANDOFF, same-wave consumers are named for every Alpha-E
candidate, and union plus ASM-gen are no longer treated as category-level
pre-blocks.

The packet is not CH4-clean because cost and wave alignment do not yet survive
the user-pin hard caps. The most serious defect is W1 feasibility: Alpha-E
couples E1 CSS baseline (`<=620` hand LOC) with E2 `GrammarConfig` legality
(`<=360` hand LOC), which creates an implied `<=980` hand-LOC first-of-class
generic-codegen/runtime/comparator wave before generated output. That is not
credible under the campaign hard caps (20 min research, 15 min plan, 30 min
redress) and is also misaligned with the pre-pin SPEC's stale 75-minute
redress model. S-P3 can repair this by splitting the legality, CSS baseline,
SIMD correctness, union attempt, and ASM-gen attempt into explicit bounded
waves while staying under the 12-wave ceiling.

G-Alpha should not present the Alpha V1 packet as CH4-accepted until the folds
below are made.

## Disposition Matrix

| Area | Disposition | Finding |
|---|---|---|
| Candidate metadata | ACCEPT | Alpha-E has five candidates and each names owner paths, LOC, risk, gate, scalar/parity status, same-wave consumer, and revert posture. |
| W0 cost posture | ACCEPT with SPEC fold | Alpha-F/SYNTHESIS/HANDOFF correctly say revalidate `f788eb97`, not redo W0; SPEC still carries pre-pin W0 dispatch text and must be updated downstream. |
| Candidate LOC realism | REVISE | E1+E2 imply up to 980 hand LOC before generated output; no 30-minute redress slice can honestly absorb that. |
| Hard caps | REVISE | Alpha-E/F do not map candidates to the user-pin 20/15/30 caps; SPEC still lists 30/30/90/75 style caps from the pre-pin plan. |
| W1 CSS redress feasibility | REVISE | CSS baseline plus `GrammarConfig` plus lightningcss comparator needs a split or a concrete preflight proving the single-wave path is smaller than the budget. |
| Union/ASM-gen attempts | REVISE | E4/E5 are costed, but no wave slots or cap envelopes guarantee at least one measured union and one measured ASM-gen attempt on the FIXPOINT path. |
| Micro-prove-first | REVISE | E4/E5 require microbenching, but their LOC/time budgets do not reserve separate scalar, checkasm/parity, microbench, consumer, and report/gate slices. |
| Same-wave consumers | ACCEPT | Every candidate names a row, gate/report, or checkasm/corpus consumer; producer-only telemetry fails closed. |
| <=12 wave posture | REVISE | The packet asserts a campaign path but does not show an explicit fast-path and fixpoint-path manifest under 12 waves. |
| Generated-size budget | REVISE | Candidate LOC budgets exclude generated output but do not state a generated LOC ceiling or O(N) regression check. |

## Findings

### CH4-1 - REVISE - W1 is over-budget if E1 and E2 remain one redress

Alpha-E says E1 and E2 are coupled for W1 because E2 is the generic-crate
prerequisite without which E1 is not legal. The stated hand budgets are:

- E1 CSS L4 baseline plus lightningcss comparator: `<=620` hand LOC.
- E2 `GrammarConfig` / Lock 14 leak extraction: `<=360` hand LOC.

That is an implied `<=980` hand-LOC W1 before generated CSS output, fixture
material, regenerated files, and measurement artifacts. It crosses generic
codegen, runtime tape/config, generated runtime modules, bench/report/gate, an
independent oracle, and a lightningcss comparator. The user-pin campaign hard
caps are 20 min research, 15 min plan, and 30 min redress. A single W1 redress
with this footprint is not realistic.

Required fold: S-P3 must either prove a materially smaller W1 by naming a
specific existing generator seam, existing oracle path, compile/equality smoke,
and exact hand-LOC slice, or split W1 into bounded waves. A CH4-credible split
is:

| Slot | Purpose | Cost posture |
|---|---|---|
| W0 | Revalidate `f788eb97` gate/report lock only | docs/gate only, no behavior LOC |
| W1a | `GrammarConfig` legality and JSON parity | `<=360` hand LOC, generic-crate CHALLENGE |
| W1b | CSS L4 generated baseline plus lightningcss comparator | `<=620` hand LOC, generated output budgeted separately |
| W2 | `escape_mask_64` correctness closure | `<=180` hand LOC, blocks SIMD admission |
| W3 | CSS-local union-substrate attempt | `<=420` hand LOC, mandatory microbench first |
| W4 | ARMv9.2 TBL/TBX or selected ASM-gen consumer | `<=430` hand LOC, mandatory checkasm and same-wave consumer |
| W5 | Close/G-Alpha reconciliation | docs/report only |

This is 6 behavior/close slots plus W0 and remains within the 12-wave ceiling.
If the pin requires the CSS row to remain named "W1", S-P3 can preserve that
label by sub-waving W1a/W1b, but the cost split must be explicit before
behavior dispatch.

### CH4-2 - REVISE - Hard-cap sources conflict and the Alpha packet does not resolve them

ORCHESTRATOR Section 3W requires realistic hard caps. PASS-ALPHA Section 3
asks CH4 to assess LOC, risk, wave alignment, and same-wave consumers. The
current user campaign instruction binds 20/15/30 caps, while pre-pin SPEC
Section 2 still carries 30-minute research/plan, 90-minute challenge, and
75-minute redress assumptions.

Alpha-F correctly says SPEC remains downstream/stale where it conflicts with
the pin, but Alpha-F and SYNTHESIS do not replace the stale cap model with a
pin-aware cost matrix. That leaves S-P3 without a cost authority for the next
plan.

Required fold: add a pin-aware Alpha cost matrix to Alpha-F or SYNTHESIS and
mirror it in HANDOFF. The matrix must list each candidate, wave slot,
hand-LOC budget, generated-LOC treatment, risk, phase caps, mandatory
CHALLENGE, same-wave consumer, microbench/checkasm budget, and split trigger.

### CH4-3 - ACCEPT WITH REQUIRED SPEC FOLD - W0 revalidate-not-redo is correct

Alpha-F, SYNTHESIS, and HANDOFF correctly preserve W0 as a revalidation of
commit `f788eb97`, not a redo. That is the right cost decision: W0 should not
spend a behavior wave reimplementing the telemetry/gate lock.

The stale SPEC still presents W0 as the first dispatchable implementation wave
with W0 tasks and pre-pin caps. Because Alpha-F says SPEC is pre-pin context,
this does not reject Alpha-F. It does require S-P3 to rewrite SPEC before any
wave dispatch so W0 is a bounded revalidation command set with zero behavior
LOC and a no-redo default.

### CH4-4 - REVISE - The CSS redress path needs a preflight or split trigger

The user pin forbids skipping CSS L4 on preflight alone; Sheets and BBNF-self
are fallback-only after a CSS L4 redress attempt. That makes the CSS attempt
mandatory, but it does not make an oversized single-wave W1 acceptable.

Alpha-E names the right CSS evidence: generated Track 1, lightningcss
comparator, strict equality, sample count, output plane `direct_sink`, gate
consumption, Lock 14, and JSON guards. It does not show that these can fit in
one redress after the `GrammarConfig` prerequisite.

Required fold: before W1 behavior dispatch, S-P3 must provide a redress
feasibility table with:

- exact CSS grammar subset and fixture corpus;
- generated source/runtime path;
- lightningcss comparator command and adapter scope;
- independent equality/oracle path;
- compile/equality smoke command;
- report/gate fields consumed;
- hand LOC per owner path;
- generated LOC estimate and ceiling;
- rollback slice.

If the table exceeds cap, the plan returns REVISE and splits. It must not skip
to Sheets without a measured CSS redress attempt.

### CH4-5 - REVISE - Union and ASM-gen are unblocked but not scheduled

The user pin requires any FIXPOINT close to include a new measured
union-substrate implementation attempt and a new measured ASM-gen attempt.
Alpha-E supplies E4 and E5 with plausible material differentials:

- E4 is CSS-local and same-tape, not the JSON class-column/streaming-cursor
  shapes rejected by REDRESS 96/97/98.
- E5 is CSS byte-class/TBL/TBX oriented, not the PMULL default body, CTZ bulk
  consumer, or canary row-movement shapes rejected by REDRESS 88/89/90.

The cost defect is scheduling: Alpha-F/SYNTHESIS/HANDOFF state the FIXPOINT
requirements, but no wave posture guarantees these attempts happen within the
closing tranche if CSS ADMIT is uncloseable. They also do not reserve the
microbench/checkasm/report budget inside E4/E5.

Required fold: add a fixpoint-path manifest seed. It must show W3/E4 and W4/E5
or equivalent wave slots, each with CHALLENGE, scalar/reference, microbench,
parity/checkasm where applicable, same-wave CSS or JSON-guard consumer, and
REDRESS material-differential citation.

### CH4-6 - REVISE - Micro-prove-first lacks budget decomposition

The packet declares micro-prove-first, and Alpha-E says E4/E5 require positive
same-host microbenches before source redress continues. That is necessary but
not enough for CH4.

Each primitive/substrate wave needs a five-part budget before dispatch:

1. scalar/reference LOC;
2. checkasm/parity LOC;
3. isolated microbench LOC and command;
4. generated same-wave consumer LOC;
5. report/gate/provenance LOC.

E4 and E5 currently have aggregate budgets (`<=420` and `<=430`) without this
breakdown. Under a 30-minute redress cap, aggregate budgets hide the exact
failure mode that caused prior orphan and microbench-only admits.

### CH4-7 - ACCEPT - Same-wave consumers are present

Alpha-E's same-wave consumer posture is CH4-acceptable:

- E1 is consumed by the non-JSON Criterion row, equality artifact, and gate.
- E2 is consumed by the E1 CSS generated runtime and non-JSON gate.
- E3 is consumed by Lock 16/checkasm and corpus parity harnesses.
- E4 is consumed by the CSS L4 generated direct parser.
- E5 is consumed by the generated CSS declaration-value parser in a selected
  layout/delimiter/string-interesting or digit-run caller.

SYNTHESIS and HANDOFF make producer-only fields, orphan SIMD, and missing gate
consumption reject the wave. No additional same-wave-consumer fold is required,
except for the cost decomposition in CH4-6.

### CH4-8 - REVISE - <=12 wave posture is plausible but not proven

The packet can satisfy the <=12 wave ceiling, but it does not prove it. A
credible posture must show both:

- ADMIT path: W0 revalidation, legality/CSS baseline waves, optional close.
- FIXPOINT path: CSS measured miss, union attempt, ASM-gen attempt, zero-orphan
  disposition, close.

The split in CH4-1 fits inside 12 waves, even with one CSS reroute or one
candidate rejection. The Alpha packet should include this arithmetic so S-P3
does not rediscover the split under pressure.

### CH4-9 - REVISE - Generated LOC and O(N) regression budget are missing

Alpha-E excludes generated CSS output from hand LOC, which is reasonable, but
the campaign discipline also requires a generated-size budget and an O(N)
regression check. The current packet does not state:

- expected generated CSS runtime LOC;
- generated module ceiling;
- regen/check command;
- O(N) guard for grammar-size growth;
- response if generated output exceeds the ceiling.

Required fold: add generated LOC accounting to the Alpha cost matrix and make
overflow a plan-time REVISE before redress.

## Required Folds Before CH4 Accepts

1. Add a pin-aware cost/cap matrix for E1-E5 with wave slots, 20/15/30 caps,
   hand LOC, generated LOC, risk, same-wave consumer, microbench/checkasm
   budget, and split trigger.
2. Split or preflight the W1 CSS path. The current E1+E2 aggregate cost is not
   feasible as one redress.
3. Update downstream SPEC in S-P3 so W0 is revalidate-not-redo, the CSS floor
   is `lightningcss_mbps + 1`, Sheets/BBNF-self are post-CSS-redress fallback
   only, and union/ASM-gen are category-unblocked.
4. Add a fixpoint-path wave seed that schedules at least one measured union
   attempt and one measured ASM-gen attempt under the <=12 ceiling.
5. Add generated-size accounting and an O(N) regression check for CSS output.
6. Add five-part primitive/substrate budgets for E4/E5 before either reaches
   redress.

## Blockers To G-Alpha

CH4 blocks G-Alpha presentation until CH4-1, CH4-2, and CH4-5 are folded. The
remaining defects are also required before implementation dispatch, but those
three determine whether the Alpha packet is cost-aligned at all.

If the folds above land, CH4 can move to ACCEPT: the candidate set is bounded,
W0 is cheap, W1 is split or proven feasible, union/ASM-gen fixpoint attempts
are scheduled, same-wave consumers are present, and the <=12 wave posture is
credible.
