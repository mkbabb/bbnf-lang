# SK-V10 S-P3 V1 CH5 - Hidden Coupling / Lock 1

Verdict: REVISE

Acceptance percentage: 82%

Scope audited: Track1/Track2 independence, generated/hand/codegen boundaries,
schema-root coupling, W0/W1 gate dependencies, sidecar freshness, W3 aliases,
and producer-only telemetry across `SPEC.md`, `DISPATCH-PROMPT.md`, P3
research artifacts, and relevant owner files.

## Findings

### 1. Generated-file owner paths create a hand/generated drift route

`SPEC.md` lists generated artifacts as direct owner paths for proof-only W7:
`skinny/crates/runtime/src/grammars/json/generated.rs`,
`skinny/crates/codegen/src/typed_direct.rs`, and
`skinny/crates/bbnf-bench/src/generated_real_typed.rs`
(`restart/skinny/tranches/sk-v10/SPEC.md:486`,
`restart/skinny/tranches/sk-v10/SPEC.md:488`,
`restart/skinny/tranches/sk-v10/SPEC.md:490`). W7 then says to add/refine a
scalar oracle, add differential parity, add a caller microbench, and not wire
production behavior (`restart/skinny/tranches/sk-v10/SPEC.md:499`-`502`).
The same pattern appears for W8, where proof-only work includes JSON generated
runtime, direct output, and generated typed owner paths
(`restart/skinny/tranches/sk-v10/SPEC.md:526`-`530`) while also forbidding
production wiring (`restart/skinny/tranches/sk-v10/SPEC.md:539`-`543`).

The hidden coupling is that both generated files are explicitly marked
generated in source (`skinny/crates/runtime/src/grammars/json/generated.rs:1`,
`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1`), while the dispatch
contract says redress implements only SPEC owner paths and returns REVISE for
other paths (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:111`-`113`).
That lets an implementation agent hand-edit generated artifacts because the
generated artifact itself is in the owner list, bypassing the generator/schema
boundary.

Required fix: mark generated artifacts in W7/W8 as read-only call-site evidence
unless the same wave also owns the generator input and regeneration command.
For any production change in W9 that changes generated JSON or generated typed
code, the SPEC must name the generator/codegen owner path and require generated
output to be regenerated, not hand-patched.

### 2. Track 2 independence is specified in P3-C but diluted in SPEC/gate text

P3-C has the correct hard rule: Track 2 may not call generated Track 1,
generated SinkOnly helpers, generated typed helpers, or benchmark-private
shared parser code (`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:38`-`41`).
The final SPEC W1/W2 language requires Track 2 independence but only says
`gate-json` rejects absent or mixed Track 2 independence
(`restart/skinny/tranches/sk-v10/SPEC.md:262`-`264`,
`restart/skinny/tranches/sk-v10/SPEC.md:303`-`307`). P3-D also has the right
rejection concept (`track2_coupling`) but frames it as "cannot call or depend
on generated Track 1 unless a SPEC explicitly owns that proof"
(`restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md:126`-`129`).

The current report validator only checks the telemetry status string
`independent_verified` (`skinny/crates/bbnf-bench/src/report.rs:1049`-`1056`).
That is useful but not enough to prevent a future wave from coupling Track 2 to
generated helpers while still emitting the accepted status.

Required fix: copy P3-C's explicit forbidden dependency list into SPEC W1/W2
and the dispatch load-bearing facts. W1's contract gate should require the
dependency proof, not just a status field. If `gate-json` cannot mechanically
prove it, the wave plan must require a concrete audit artifact naming the
Track 2 source path and forbidden generated dependencies.

### 3. Root-model proof has a Lock 14 escape hatch around generic-code edits

The global close condition is correct: generic crate, codegen, or
runtime-outside-JSON edits require named CSS L4, Sheets, or BBNF-self proof
(`restart/skinny/tranches/sk-v10/SPEC.md:49`-`51`). P3-B says the same for
generic crate, codegen, or runtime-outside-JSON edits
(`restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md:31`-`33`).

W5, however, explicitly owns generic/codegen root-model paths
(`restart/skinny/tranches/sk-v10/SPEC.md:402`-`403`) and its task is to extend
`DirectRootSchema` or a successor
(`restart/skinny/tranches/sk-v10/SPEC.md:415`-`416`). The exit gate then allows
"Lock 14 proof names CSS L4, Sheets, or BBNF-self impact, or proves no generic
behavior changed" (`restart/skinny/tranches/sk-v10/SPEC.md:426`-`427`). Because
the wave's named work is a root-model/codegen extension, "proves no generic
behavior changed" can become a loophole unless it is tied to no generic/codegen
edit actually occurring.

Required fix: narrow W5 so any edit to `skinny/crates/codegen/src/direct_schema.rs`
or `skinny/crates/codegen/src/typed_direct.rs` must carry the non-JSON proof
named by Lock 14. Allow "proves no generic behavior changed" only when the diff
shows no generic/codegen/runtime-outside-JSON behavior edit, for example a
fixture-only proof.

### 4. P3-E keeps stale per-wave W-number aliases that conflict with the final SPEC

The final SPEC makes W3 a governance firewall only
(`restart/skinny/tranches/sk-v10/SPEC.md:167`,
`restart/skinny/tranches/sk-v10/SPEC.md:183`,
`restart/skinny/tranches/sk-v10/SPEC.md:316`-`350`). Dispatch repeats that W3
is firewall-only and never a W3 substrate
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:59`-`63`).

P3-E still has pre-SPEC per-wave headings where "W3" means root-type typed
generalization (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:141`-`150`),
"W4" means kernel pair (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:164`-`170`),
and "W5" means comparator/telemetry refresh
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:207`-`210`).
The global ledger rule says later SPEC/DISPATCH may tighten but not loosen the
blocks (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:21`-`23`),
so the global pre-blocks remain usable; the stale wave-number aliases are the
problem.

Required fix: revise P3-E headings or add a prominent note that its per-wave
numbering is superseded by final SPEC W0-W10 numbering. Otherwise CH5 should
continue to treat the final SPEC as authoritative but require agents not to use
P3-E W3/W4/W5 headings as dispatch identifiers.

## Accepted Coupling Controls

- W0/W1 sequencing is strong. W1 must exist before W2 or W10 can move direct
  rows (`restart/skinny/tranches/sk-v10/SPEC.md:181`-`182`), and W2 entry is
  gated on a live W1 contract (`restart/skinny/tranches/sk-v10/SPEC.md:291`).
- Sidecar freshness is correctly non-producer. SPEC blocks sidecar freshness
  as parser producer or strict admission shortcut
  (`restart/skinny/tranches/sk-v10/SPEC.md:231`-`233`), P3-E does the same for
  PASS-3 sidecars (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:77`-`79`),
  and source validation rejects claimed same-run sidecars without a structured
  manifest (`skinny/crates/bbnf-bench/src/report.rs:1329`-`1333`).
- Producer-only telemetry is well locked. P3-D forbids any field not consumed
  by `gate-json` in the same wave
  (`restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md:171`-`190`),
  and dispatch repeats that producer-only telemetry rejects
  (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:180`-`188`).
- Output-plane separation is consistently stated: direct digest cannot admit
  typed rows (`restart/skinny/tranches/sk-v10/SPEC.md:40`-`42`,
  `restart/skinny/tranches/sk-v10/SPEC.md:129`), and direct/typed/retained/hand
  Track 2 evidence are not interchangeable
  (`restart/skinny/tranches/sk-v10/SPEC.md:146`-`147`).

## Required Fix Set For ACCEPT

1. Tighten W7/W8/W9 generated-file ownership so generated artifacts are either
   read-only evidence or regenerated from named generator/schema inputs.
2. Promote P3-C's full Track 2 forbidden-dependency list into SPEC W1/W2 and
   dispatch, with an audit requirement if `gate-json` cannot prove it.
3. Narrow the W5 Lock 14 escape hatch so generic/codegen edits always require
   non-JSON proof; "no generic behavior changed" applies only to no generic
   behavior diff.
4. Supersede or rename P3-E's stale W3/W4/W5 per-wave aliases to match the
   final SPEC wave numbers.

After those fixes, CH5 should move to ACCEPT.
