# SK-V8 W3 Hardening V1 - CH4

Date: 2026-05-18.
Reviewer: CH4.
Target reviewed: `fc91c217`
(`docs(sk-v8-wave3-plan): reject Tier A implementation on fit gate`).

## Verdict

ACCEPT.

Confidence: 96%.

## Findings

1. No CH4 blocker. HEAD adds only
   `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md`; no runtime,
   SIMD, codegen, generated JSON, bench gate, `RESULTS.md`, or `REDRESS.md`
   implementation change is present in `fc91c217`. That matters for Lock 14:
   the reviewed plan names generic/runtime/codegen owner paths only as evidence
   that Tier A is too large for W3, not as authority to edit them.

2. The plan avoids generic JSON policy by rejecting implementation before any
   generic crate moves. It records that scanner positions are structural
   punctuation plus real quotes while retained tape offsets are generated parser
   events, including container events, opening quotes, number starts, and literal
   starts. That is the correct non-JSON implication: the issue is an event-model
   mismatch, not a license to encode JSON structural roles in generic SIMD,
   runtime tape, or codegen under neutral names.

3. The event-model work is routed to the right place. The plan requires a
   split precursor for SK-V9/Pass Omega: define the retained class/event grammar
   including numbers/literals and string quote ownership, prove the `ValueRef`
   cursor contract over that grammar, and only then measure structural-heavy
   parse rows in a later wave. This satisfies the SPEC requirement that W3 split
   or return REVISE when the exact plan exceeds the LOC/time cap.

4. Same-wave consumer discipline is preserved. The plan explicitly refuses
   `tape_vs_tape`, `simd_structural_scan`, Track 2, comparator rows, and
   retained-view-only checks as W3 production consumers. It keeps the only valid
   future consumer as generated JSON retained Track 1 parsing plus retained
   view/`ValueRef` parity in the same slice.

5. Lock 14 and non-JSON proof are not weakened. Because no W3 source
   implementation is authorized, CSS L4/Sheets/BBNF-self proof is not needed for
   this docs-only rejection. Any future attempt to touch `bbnf-simd`,
   `runtime/src/tape`, generic runtime, or codegen templates must still provide
   the public API, grammar-branch, primitive/table, template/provider-boundary,
   and non-JSON proof required by SPEC Section 2.1.

## Verification

- `git rev-parse HEAD`: `fc91c2173e8451dd06733381346bd800b0711f6e`.
- `git status --short --untracked-files=all`: clean before this owned file was
  created.
- `git diff --name-status fc91c217^ fc91c217`: only
  `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md` was added.
- `git diff --exit-code fc91c217^ fc91c217 -- skinny/crates/bbnf-simd skinny/crates/runtime skinny/crates/codegen skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/materialization.rs skinny/crates/bbnf-bench/src/bin/gate.rs skinny/RESULTS.md skinny/REDRESS.md`:
  PASS, no generic/runtime/codegen/gate/results/redress implementation diff.
- `cargo test -p bbnf-bench offset_stream_tracks_verified_source_events -- --nocapture`
  from `skinny/`: PASS.
- `cargo test -p bbnf-bench counts_json_lazy_tape_materialization_shape -- --nocapture`
  from `skinny/`: PASS.
- `git diff --exit-code HEAD -- skinny/RESULTS.md`: PASS.
- `git diff --check`: PASS before this owned file was added.
- Reviewed `skv8-W3-plan.md`, `skv8-W3-tape-structural-research.md`,
  `SPEC.md` Section 2.1/6/10, and `HANDOFF.md` W3/Pass Omega language for Lock
  14, same-wave consumer, split gate, and pre-block alignment.

## Required Folds

1. Preserve the W3 disposition as reject/routed, not implementation-authorized.
2. Add the planned REDRESS entry for the scanner/tape event-model mismatch and
   failed W3 fit gate.
3. Update HANDOFF to mark W3 rejected/routed and unblock W4 only under that
   disposition.
4. Feed SK-V9/Pass Omega with the split event-model precursor before any generic
   SIMD/runtime/tape/codegen work is reconsidered.
5. Do not change generic runtime, SIMD, tape, or codegen for W3 without a fresh
   challenged plan and full Lock 14/non-JSON proof.
