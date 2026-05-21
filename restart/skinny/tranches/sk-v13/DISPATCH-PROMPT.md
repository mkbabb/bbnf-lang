# SK-V13 DISPATCH-PROMPT - Per-Wave Implementation Contract V1 Draft

Date: 2026-05-21.

Status: S-P3 V1 planning draft. This prompt is not live implementation
authority. Implementation waves remain blocked until G-Omega closes and S-P3
converges or the user explicitly pins S-P3 final.

## Authority

Read in this order before dispatching any SK-V13 implementation wave:

1. `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
2. `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
3. `restart/skinny/tranches/sk-v13/HANDOFF.md`
4. `restart/skinny/tranches/sk-v13/SPEC.md`
5. `restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md`
6. `restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md`
7. `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md`
8. `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md`
9. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
10. `restart/prompts/ORCHESTRATOR.md` Section 3, Section 3W, and Section 3Z
11. `skinny/RESULTS.md`
12. `skinny/REDRESS.md`

P3-B, P3-C, P3-D, and P3-E are absent at V1 draft time. If they exist by the
time of dispatch, the orchestrator must read and fold them before issuing a
wave packet.

## Global Block

Do not dispatch Wave 0 or any later implementation wave until both are true:

- G-Omega has closed by user sign-off and totality V1.1 CRUD is landed.
- S-P3 has converged by two consecutive accepted CHALLENGE cycles, or the user
  has explicitly pinned S-P3 final.

Before both gates close, allowed work is planning/research only under
`restart/skinny/tranches/sk-v13/` plus read-only inspection of `skinny/RESULTS.md`
and `skinny/REDRESS.md`. No source edit, generated runtime edit, gate/report
edit, RESULTS edit, or REDRESS append is authorized before the block lifts.

## Wave Triumvirate Contract

Each wave runs the skinny triumvirate:

| Phase | Agent shape | Default cap | Output |
|---|---|---:|---|
| Research | up to 6 read-only agents | 20 min each unless plan says 30 | `restart/skinny/tranches/sk-v13/research/wave-{W}-*.md` |
| Plan | 1-2 synthesis agents | 15 min unless plan says 30 | `restart/skinny/tranches/sk-v13/research/wave-{W}-plan.md` |
| CHALLENGE | CH1-CH6 when required | 90 min | `restart/skinny/tranches/sk-v13/research/wave-{W}-hardening/` |
| Redress | 1 implementation agent | 30 min implementation + 15 min measurement; W5-W9/W12 use 45 + 15 | source/RESULTS/REDRESS only inside owner paths |

Research and plan are read-only. Redress may edit only the owner paths named by
the accepted wave plan. A wave may not merge research, plan, and redress into
one commit or one role. If the user instructs "no stage/no commit", obey that
instruction and leave files unstaged.

## Required Wave Packet

Every dispatch packet must include:

- Track/pass/wave id: `SK-V13 Wave {W}`.
- Exact SPEC section and candidate id.
- Owner paths and forbidden paths.
- Entry gate and blocking prerequisites.
- Falsifiability rows and thresholds.
- Same-wave consumer path.
- Scalar reference and parity/checkasm path if SIMD/ASM is involved.
- Generality proof target: CSS L4 plus Sheets or BBNF-self when generic crates
  are edited.
- Pre-blocked REDRESS entries.
- LOC cap, phase cap, rerun ceiling, and revert slice.
- Required RESULTS/REDRESS/rolling-delta updates on admit or reject.

If any item is missing, dispatch returns REVISE before source redress.

## Universal Falsifiability Gates

JSON:

- Scope is all 51 rows: 17 corpora x `parse_only`, `direct_to_struct`, and
  `real_typed_struct`.
- Admission threshold is `Track 1 Mbps > sonic-rs strict Mbps + 1` on the same
  corpus, output plane, strictness, and equality path.
- `parse_only` is admission-eligible. It is not diagnostic-only.
- Missing typed rows must generate the product surface and same-run sonic
  strict anchor before admission.

CSS:

- Scope is 24 non-OUT_OF_SCOPE CSS parity features.
- Admission threshold is `Track 1 Mbps > lightningcss Mbps + 1` on the same
  output plane with strict equality, full feature coverage, and cssparser or
  hand-checked golden oracle.
- No `PARTIAL` feature closes SK-V13.

Decision engine:

- No resolver wave closes on scaffold. The resolver output must be consumed by
  lowering/codegen in the same wave family, and the old P1-P8 cascade must be
  deleted or fail-closed.

Union:

- A union wave must cite REDRESS 96/97/98 and name a material differential.
  Retained class columns, parser-owned cursors, sidecar vectors, aux tables,
  public `UnionTape`, and second source scanners reject.

SIMD/ASM:

- Scalar reference, strict checkasm/differential coverage, corpus parity,
  feature mask, and same-wave production consumer are mandatory.
- Checkasm-only, microbench-only, and future-consumer landings reject.
- Close requires zero aarch64 orphans.

Close:

- Implementation-limited misses do not close. Fixpoint does not close. Only
  full ADMIT or architectural-level intrinsic-block proof closes a target.
- If any target remains open, bracket SK-V14 immediately through Pass Alpha.

## Generality And Lock Policy

Lock 14:

- Generic crates may not branch on grammar name, corpus name, CSS feature, JSON
  structural role, object/array/key role, string role, layout role, or field
  name.
- Grammar-specific behavior enters through generated per-grammar modules,
  tables, templates, or opaque facts.
- Generic edits require a non-JSON proof: CSS L4 plus Sheets or BBNF-self
  compile/lower/cost/unchanged-output audit.

Lock 16:

- SIMD/ASM primitives require executable scalar reference and strict
  checkasm/corpus parity before wiring.
- The selected classifier must use the consuming grammar's quote, escape,
  control, delimiter, string, and number policy, or an explicit no-policy
  statement when the row has no string/number domain.
- No public substrate API and no retained sidecar classifier state.

Lock 1:

- One substrate. SinkOnly is legal only as an output projection with no
  queryable retained document identity.
- If structural projection is retained, it is the tape/fact stream itself.

## Wave Map

| Wave | Dispatch purpose | Required close signal |
|---|---|---|
| W0 | Capture `SK-V13-open`, bind telemetry, create rolling delta | Gate consumes all required fields; no behavior diff |
| W1 | Expand CSS comparator/oracle harness | Same-plane lightningcss/cssparser/golden gate rejects stale/report-only data |
| W2 | CSS stylesheet root + selectors | `css_l4/stylesheet_and_selectors` admits or rejects with evidence |
| W3 | CSS declaration-value expansion | selected value feature row admits or rejects with evidence |
| W4 | CSS visual/at-rule/nesting pack | selected CSS pack admits or rejects with evidence |
| W5 | Regex extraction + decision feature gate | regex facts consumed by IR/passes; no support-only crate extraction |
| W6 | E-graph + active cost | bounded e-graph and cost extraction consumed by backend selection |
| W7 | CSP + cascade fail-closed | CSP or measured abrogation; old cascade cannot silently serve rows |
| W8 | Per-grammar policy/sink/view/flags | generated policy consumed by named JSON/CSS rows; Lock 14 proof passes |
| W9 | Union material differential | one fresh union variant admits or architectural-blocks |
| W10.N | CSS feature parity expansion | every remaining non-OUT_OF_SCOPE CSS feature admits or blocks |
| W11.N | JSON direct residual reopens | selected direct rows beat sonic strict + 1 or block |
| W12 | SIMD/ASM production + zero orphans | production consumer measured; orphan count zero |
| W13 | Typed product completion | missing typed rows generated and admitted/blocked |
| W14.N | `parse_only` admissions | selected parse rows beat sonic strict + 1 or block |
| W15 | Close or bracket | full G1-G7 admit/block or immediate SK-V14 bracket |

Subwaves W10.N, W11.N, and W14.N may be dispatched concurrently only after
their plans prove non-overlapping file domains. RESULTS and REDRESS writes
serialize.

## Pre-Blocked Routes

Every wave inherits these blocks unless the wave plan cites the REDRESS entry,
names a fresh material differential, and passes challenge where required:

- REDRESS 28/33: tiny-string NEON replay or broad string-kernel close.
- REDRESS 50-55: aux side tables, event cursors, structural-mask cursors,
  decoded-string stats sinks, quote-source hash streams, visitor sidecars.
- REDRESS 60-72: direct-materialization replay, source-hook receiver shortcuts,
  semantic fact hashing, retained string-boundary collapse, direct cap replay.
- REDRESS 80: one-row `canada` mantissa widening.
- REDRESS 82-84: single-quartet unicode classifier, StringBlock16 tiny probe,
  object-pair value-byte control compaction.
- REDRESS 88-90: PMULL/CSSC/body-fill performance admission or canary-only
  route as row movement.
- REDRESS 92 and 96-98: scanner/tape isomorphism, class column, streaming
  cursor, class lane, sidecar event vector.
- REDRESS 107/108 and 113-120: proof-only escape routes, non-JSON axis blocks,
  and direct fixpoint history as close authority.
- REDRESS 121-127: preserve GrammarConfig, escape-mask, CSS comparator,
  zero-orphan, and SK-V12 close evidence.

## Redress Outcome Rules

On admit:

- Update the relevant RESULTS/report/rolling-delta surfaces.
- Append REDRESS with row table, thresholds, commands, run id, host, and
  same-wave consumer evidence.
- Confirm no silent demotion across already-admitted JSON/CSS rows.

On reject:

- Revert the source/gate/RESULTS slice.
- Save rejected patch path if source was attempted.
- Append REDRESS with measured failure, threshold missed, pre-block status,
  and routed next material differential or architectural-block evidence.

On architectural block:

- The proof must show intrinsic architectural impossibility for bbnf-lang under
  the current locks, not an implementation-limited miss.
- Surface to the user for re-pin if the block would mark a feature/row
  OUT_OF_SCOPE.

## Final Close Rule

W15 may close SK-V13 only if G1-G7 are fully admitted or architecturally
blocked. Otherwise W15 returns close REJECT and Pass Alpha brackets SK-V14
immediately. No implementation agent may soften this rule in a wave prompt.
