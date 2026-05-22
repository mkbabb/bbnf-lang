# SK-V13 DISPATCH-PROMPT - Per-Wave Implementation Contract V2 Draft

Date: 2026-05-21.

Status: live implementation authority. S-P3 converged at
`G-S-P3-SPEC-DISPATCH-CONVERGED` in
`research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md`; G-Omega closed by
explicit user sign-off at `2026-05-22T03:52:18Z`; Pass Omega CRUD completed in
`restart/audit/totality/astral/V1/CRUD-LOG.md`. Implementation waves may
dispatch under this contract after their per-wave packet is accepted.

## Authority

Read in this order before dispatching any SK-V13 implementation wave:

1. `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
2. `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
3. `restart/skinny/tranches/sk-v13/HANDOFF.md`
4. `restart/skinny/tranches/sk-v13/SPEC.md`
5. `restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md`
6. `restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md`
7. `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md`
8. `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md`
9. `restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md`
10. `restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md`
11. `restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`
12. `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md`
13. `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md`
14. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
15. `restart/prompts/ORCHESTRATOR.md` Section 3, Section 3W, and Section 3Z
16. `skinny/RESULTS.md`
17. `skinny/REDRESS.md`

P3-A through P3-E are current required inputs. P3-A owns the candidate
shortlist; P3-B owns cost/dependency/bracket accounting; P3-C owns formulas;
P3-D owns telemetry and gate-json rejection; P3-E owns the REDRESS route-state
ledger. No implementation wave packet may treat those artifacts as absent.

## Global Block Status

The pre-W0 block is lifted for SK-V13:

- G-Omega closed by user sign-off and totality V1.1 CRUD is landed.
- S-P3 converged by two consecutive accepted CHALLENGE cycles.

The lift is not blanket source authority. Wave 0 and later redress may edit only
the owner paths named by the accepted wave packet, and each wave must preserve
the research -> plan -> CHALLENGE when required -> redress commit discipline.

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
- Generality proof target: CSS L4 plus both Sheets and BBNF-self for
  fleet-wide grammar-neutral claims; CSS L4 plus only one of Sheets or
  BBNF-self is scoped evidence only.
- Pre-blocked REDRESS entries.
- LOC cap, phase cap, rerun ceiling, and revert slice.
- Required RESULTS/REDRESS/rolling-delta updates on admit or reject.
- Required telemetry fields: `row_state`, `source_commit`, `consumer_gate`,
  `g_omega_status`, CSS feature id/status, domain extension blocks, generated
  LOC budget, and all SPEC Section 0.4 gate-json rejection fields.

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
- Generic edits require a non-JSON proof. Fleet-wide grammar-neutral claims
  require CSS L4 plus both Sheets and BBNF-self fail-closed, compile/lower/cost,
  unchanged-output, or generated-role fact-row witnesses. CSS L4 plus only one
  of Sheets or BBNF-self is scoped to the witnessed grammars and cannot close a
  fleet-wide Lock 14 claim.

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

Canonical wave-accounting rule: this W0-W15 table is authoritative for V2.
P3-B's W0-W11 labels are packing aliases only. A W10.N, W11.N, or W14.N
subwave is a planning lane until the accepted wave plan declares it a real
triumvirate; each real subwave counts against the active skinny-bracket
accounting. If bracket accounting overflows, W15 closes the tranche as
`REJECT-BRACKET` and Pass Alpha immediately opens SK-V14 without dropping any
pinned row or feature.

Decision/policy anti-paper-close rule: W5, W6, W7, and W8 must be consumed by a
named generated selection, backend, compile, or generated row path and must move
at least one JSON or CSS row by P3-C `row_move_toward_sota`, admit a row, or
record a measured architectural block. API extraction, e-graph/cost telemetry,
CSP plumbing, cascade retirement, or generated policy wiring without row
movement/block is a measured reject.

Same-wave consumer minimums:

| Wave family | Required consumer |
|---|---|
| W10.N | Generated CSS feature row and production fact-stream caller in `skinny/crates/runtime/src/grammars/css_l4_*`. |
| W11.N | Generated JSON direct sink/digest production path exercised by `skinny/crates/bbnf-bench/src/direct_struct.rs`. |
| W13 | Generated real-typed product parser and independent Track 2/oracle harness for the selected corpus batch. |
| W14.N | Generated JSON parse path or selected parse runtime caller exercised by `bbnf-bench` `parse_only`. |

SIMD same-wave zero-orphan rule: any wave that touches `skinny/crates/bbnf-simd/`
or selects a SIMD-generated consumer, including W9/C3 union, must exit with
`orphan_count_after = 0`, strict checkasm status, scalar-reference status,
delete/demote/revert protocol, and production consumer row evidence in that
same wave. Later W12 cleanup is not an admissible dependency for a wave that
creates or reclassifies a SIMD primitive.

## Pre-Blocked Routes

Every wave inherits these blocks unless the wave plan cites the REDRESS entry,
names a fresh material differential, and passes challenge where required:

- Pre-W0/W0: REDRESS 75, 77, 78, 99-102, 111, 119-127 are gate feed; 119/120
  cannot close; no source, RESULTS, or REDRESS work before G-Omega.
- W1-W4 and W10.N CSS: REDRESS 112, 113, 123-127 are gate feed; 28/33,
  50-55, 60-72, 82-84, 88/89, and 126 block string, escape, or SIMD replays;
  123-125/127 are not full CSS close.
- W5-W7 decision engine: REDRESS 84, 87, 114, 115 plus 85-87/121 Lock 14 and
  CostFacts families block JSON-specific generic branches, support-only
  resolver extraction, and old cascade fallback admission.
- W8 policy/sink/view: REDRESS 121, 54/55/66-69, and 80/82/84 block generic
  JSON policy, public `GrammarConfig`, generic `JsonSink` acceleration, and
  direct source-hook/string/control replays.
- W9 union: REDRESS 50, 51, 53, 88, 89, 92, 96, 97, 98, and 126 block class
  column, streaming cursor, parser-local cursor, sidecar, scalar-delegate body,
  and `UnionTape` replays.
- W11.N direct: REDRESS 54, 55, 66-69, 73, 80, 82, 84, 106-108, 114-119
  block direct source-hook/digest/hash/string/number/control replays; 119/120
  are history only.
- W12 SIMD/ASM: REDRESS 88, 89, 90, 122, 126 and relevant 121-127 gate feed
  block microbench-only/checkasm-only admission and require zero orphans.
- W13 typed product: REDRESS 70-72 and 103-110 are mixed precedent; typed
  product precedent is allowed but hidden typed sinks, proof-only escape routes,
  and no-op production rows do not admit.
- W14.N parse-only: REDRESS 28, 33, 50, 51, 53, 60-65, 72 overgeneralization,
  82-84, 88, 89, 92, 96-98, and 102 block docs-only parse movement and stale
  retained string/control/union replays.
- W15 close: REDRESS 119, 120, 123-127 plus the full-SOTA addendum block
  ordinary fixpoint, implementation-limited miss, one-CSS-row close, or
  REDRESS-history close.

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
