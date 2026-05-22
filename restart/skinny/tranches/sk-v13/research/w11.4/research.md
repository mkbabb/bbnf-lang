# SK-V13 W11.4 Research - Direct Cursor Byte Fetch

Date: 2026-05-22.
Scope: JSON direct residual reopen after W14.5 exhausted report-only parse
admissions.

## Authority

- `restart/skinny/tranches/sk-v13/SPEC.md` Section 15 authorizes JSON direct
  residual reopen subwaves and rejects closes by REDRESS 119/120 history.
- The 2026-05-21 full-SOTA addendum lifts the old direct fixpoint: every JSON
  direct row remains wave-eligible until it admits over same-plane sonic strict
  + 1 Mbps or records architectural-block evidence.
- W14.5 REDRESS-158 routes the next work away from status/report plumbing and
  names `json/instruments/direct_to_struct/main` as the closest pinned-margin
  implementation target.

## Current Row

Current `ROLLING-SOTA-DELTA.md` records:

| row | Track 1 | sonic+1 | margin | status |
|---|---:|---:|---:|---|
| `json/instruments/direct_to_struct/main` | 12307 | 12785 | -478 | OPEN |

`RESULTS.md` still shows the row as top-table `A / GO` under the older
measured-row contract, but the addendum's full-SOTA bar keeps it OPEN because
Track 1 remains below sonic strict + 1.

## Prior Attempt

W11.3 landed direct sink stack specialization and admitted
`json/mesh/direct_to_struct/main`. Its same Criterion run measured
`json/instruments/direct_to_struct/main` at Track 1 `12179.139` Mbps versus
sonic strict `12787.011` Mbps, so instruments remained routed.

That route changed the sink parent access layer only. It did not change the
generated direct parser's value dispatch byte fetch.

## Hot-Leaf Evidence

S-P1 hot-leaf attribution identifies instruments direct Track 1 as:

`58.3% Option<&u8>::copied (core/src/option.rs:2141)`.

The current generated direct parser still uses:

- `parse_value_direct`: `bytes.get(*cursor).copied()`
- `parse_object_value_at_direct`: `bytes.get(*cursor).copied()`
- `parse_array_element_at_direct`: `bytes.get(*cursor).copied()`
- `parse_array_direct`: `bytes.get(*cursor).copied()` inside the element loop

These checks are correctness-preserving candidates for explicit
`*cursor >= bytes.len()` guards followed by `unsafe { *bytes.get_unchecked(*cursor) }`.
The error path and cursor semantics stay identical.

## Candidate

Replace the direct parser's hot byte fetch shape in the JSON generated template
and the checked-in generated JSON file. This is a direct-parser dispatch
intervention, not a sink-stack, SIMD, union, digest-shortcut, comparator, or
report-only change.

Expected affected rows:

- primary: `json/instruments/direct_to_struct/main`
- guard probes: `json/random/direct_to_struct/main`,
  `json/canada/direct_to_struct/main`, `json/github_events/direct_to_struct/main`

## Owner Paths

- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w11.4/`

## Falsifiability

Admit only if at least one direct row, preferably
`json/instruments/direct_to_struct/main`, exceeds same-run sonic strict + 1
Mbps with strict equality, Track 2 independence, no admitted-row demotion, and
gate-consumed provenance.

If no row admits, revert the source patch, save
`/tmp/skv13-waveW11.4-rejected.patch`, and record measured movement in
REDRESS.

## Pre-Blocked Routes

- No digest shortcut, fixture branch, comparator weakening, new directive, BIR
  variant, BackendShape variant, public substrate API, or x86 path.
- No union-substrate replay and no status-only admission.
- No generated/runtime divergence: template and checked-in generated JSON must
  move together or the wave returns REVISE.
