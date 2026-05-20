# SK-V12 S-P3 CH4 Cost/Scope Challenge - Cycle PIN-V1

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V1.
Lens: CH4 cost, wave sizing, cap feasibility, generated LOC/O(N) budget, owner
path scope, and hidden multi-wave work.
Date: 2026-05-20.
Verdict: REVISE.
Confidence: 84%.

## Summary

The PIN-V1 packet is directionally aligned with the user pin, but it is not yet
cost-safe for dispatch. The tight campaign caps are correctly stated, and W0,
W1a, W2, and W5 are mostly bounded. Three scope issues still need folding before
S-P3 can converge:

1. W1b combines CSS L4 grammar emission, generated runtime, independent oracle,
   lightningcss same-plane comparator, strict equality, throughput benchmark,
   gate/report consumption, generated-size accounting, and JSON guard treatment
   inside one 30-minute redress wave. That is too broad unless the packet names a
   pre-existing comparator/oracle scaffold or splits the baseline row from
   comparator/gate integration.
2. W4 combines one ASM-gen production attempt with disposition of all five
   production aarch64 orphans. Under the 30-minute cap this is hidden multi-wave
   work unless the wave is narrowed to one selected primitive plus an
   inventory-only orphan disposition, or sub-waved.
3. The packet is internally inconsistent about whether W3 is required for ADMIT
   close. P3-B permits the minimum admission path without W3, while SPEC W5
   requires W3 disposition before close. This changes campaign cost and must be
   made explicit.

## Findings

### CH4-1 - W1b is over-scoped for a single 30-minute redress

Severity: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v12/SPEC.md:241-242` gives W1b `<=620 hand;
  generated output named separately` and a `<=30 min` redress cap.
- `restart/skinny/tranches/sk-v12/SPEC.md:363-368` requires the plan to name the
  CSS corpus, canonical fact stream, output plane, independent oracle/Track 2,
  lightningcss comparator command, equality command, benchmark command, gate
  command, generated paths, and rollback slice.
- `restart/skinny/tranches/sk-v12/SPEC.md:372-379` then asks the same wave to
  generate CSS L4 Track 1, build the oracle/Track 2 and lightningcss comparator,
  emit canonical CSS facts, run equality and throughput, and record generated
  LOC/O(N) and JSON guards.
- `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:97-141`
  defines a large required CSS evidence record, including source/runtime
  checksums, oracle independence, lightningcss output checksums, benchmark
  artifacts, PMU/Time Profiler/samply artifacts, gate status, and REDRESS id.

Cost read:

This is a first generated CSS row and first same-plane lightningcss comparator.
The packet does not show that comparator harness, independent oracle/fact stream,
CSS fixture normalization, and gate consumer already exist. Without that
pre-existing scaffold, W1b is at least two redress-sized slices:

- W1b-1: generated CSS L4 baseline + canonical fact stream + strict equality
  against independent oracle.
- W1b-2: same-plane lightningcss throughput comparator + gate/report/RESULTS
  consumption + generated-size/O(N) accounting.

Required fold:

Either prove in the SPEC that the oracle, fact-stream, lightningcss comparator,
and gate adapter already exist and name their exact paths/commands, or split W1b
into two sub-waves with separate redress caps and exit gates. If kept single-wave,
the owner table must list concrete existing bench/gate paths instead of broad
families.

### CH4-2 - W4 hides all-orphan cleanup inside the ASM-gen attempt

Severity: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v12/SPEC.md:245` budgets W4 as `<=430
  hand/test/gate` under `<=30 min`.
- `restart/skinny/tranches/sk-v12/SPEC.md:486-489` limits W4 to at most one
  primary ASM-gen candidate.
- `restart/skinny/tranches/sk-v12/SPEC.md:491-499` also requires scalar
  reference/checkasm refresh, production consumer wiring, CSS/JSON measurement,
  and disposition of all five carried orphans:
  `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
  `byte_context`, and `cache_hints`.
- `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:239-255`
  requires per-orphan status, consumer or demotion/removal artifact, Lock 16
  status, and REDRESS entry.

Cost read:

One primary ASM-gen candidate with scalar reference, checkasm, microbench,
production routing, and CSS/JSON guard measurement is already a full 30-minute
redress. Disposing five unrelated production orphans in the same wave is a
second scope. The packet says removal, consumption, or inventory demotion is
allowed, but it does not bound the work per orphan or state that demotion-only is
acceptable for non-selected primitives.

Required fold:

Narrow W4 to one selected ASM-gen primitive plus a zero-orphan accounting table,
where non-selected orphans may be inventory-demoted only if the plan names the
exact proof and no source behavior changes are needed. If any orphan needs
consumption or removal, split W4 into sub-waves or route that orphan to W5/SK-V13
with close explicitly blocked until disposition is complete.

### CH4-3 - W3 close cost is contradictory

Severity: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:84-89`
  gives a minimum admission path `W0 -> W1a -> W1b -> W2 -> W4 -> W5` when W1b or
  W4 beats lightningcss, with W3 inserted only when needed for ADMIT or FIXPOINT.
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:79-82` similarly says W3
  and W4 are required for FIXPOINT, while ADMIT before W3/W4 still requires zero
  orphans and close-doc agreement.
- `restart/skinny/tranches/sk-v12/SPEC.md:524-527` requires W0, W1a, W1b, W2,
  W3, and W4 to have admitted, rejected, routed, or blocked before W5 entry.

Cost read:

These are not equivalent. If W3 is required before all closes, then the minimum
ADMIT path is longer and includes a high-risk union wave even when the CSS row
already clears the user pin. If W3 is only required for FIXPOINT, then SPEC W5's
entry gate overstates required work and can force a hidden union attempt after an
already measured ADMIT.

Required fold:

Choose one rule and bind it everywhere:

- ADMIT path: W3 is optional when W1b or W4 satisfies
  `track1_mbps > lightningcss_mbps + 1`; W5 may enter without W3 if zero
  orphans, Lock 14/16, JSON guards, strict equality, and comparator evidence
  hold.
- FIXPOINT path: W3 is mandatory and must produce measured REDRESS evidence.

Update SPEC Section 9 and the dispatch manifest to match the chosen rule.

### CH4-4 - Owner path scope is too broad in generated/comparator waves

Severity: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v12/SPEC.md:354-361` lists W1b owner paths as
  `grammar/css/l4/`, all `skinny/crates/codegen/src/`,
  `skinny/crates/runtime/src/grammars/css/`, generated JSON guard output,
  `skinny/benches/`, `skinny/xtask/`, report/gate paths, `RESULTS`, and
  `REDRESS`.
- `restart/skinny/tranches/sk-v12/SPEC.md:431-437` lists W3 owner paths as all
  `skinny/crates/runtime/src/`, all `skinny/crates/codegen/src/`, generated CSS
  runtime paths, benchmark/gate paths, `RESULTS`, and `REDRESS`.
- `restart/skinny/tranches/sk-v12/SPEC.md:469-477` lists W4 owner paths across
  bbnf-simd, runtime, codegen, generated CSS runtime, bench/gate/report, and
  results ledgers.

Cost read:

The wave protocol says a redress agent returns REVISE before editing outside
owner paths, so owner paths are the cost guard. These broad families make it too
easy for a 30-minute redress to wander across codegen, runtime, generated output,
bench harnesses, gates, and reports without a concrete slice boundary.

Required fold:

For W1b/W3/W4, replace broad owner families with owner tables containing exact
candidate files or file globs, plus a "plan may add paths only by CH4/CH5
REVISE" rule. W1b especially must name whether the real bench root is
`skinny/crates/bbnf-bench/...`, `skinny/benches/...`, or both.

## Accepted Items

- The hard caps themselves are correctly stated in the dispatch prompt:
  `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:91-107` and
  `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:120-133` bind 20-minute
  research, 15-minute plan, and 30-minute redress, with distinct commits.
- W0 is cost-safe: `restart/skinny/tranches/sk-v12/SPEC.md:293-301` forbids
  behavior/generator/parser edits.
- W1a is high-risk but plausibly bounded if it is restricted to the seven
  enumerated Lock-14 leaks and the plan names the exact JSON guard rerun.
- W2 is cost-safe as a correctness prerequisite: `restart/skinny/tranches/sk-v12/SPEC.md:410-421`
  is a narrow SIMD parity/checkasm scope and explicitly gives no throughput
  credit.
- Fallback waves are not hidden in the opening CSS path:
  `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:98-104`
  correctly requires a subsequent folded cycle or tranche before Sheets/BBNF-self
  can enter.
- W5 is docs/gate-only in stated intent: `restart/skinny/tranches/sk-v12/SPEC.md:517-535`
  lists close, reconciliation, disposition recording, and SK-V13 routing rather
  than behavior implementation.

## Required Folds

1. Split W1b or prove the CSS oracle/comparator/gate scaffold exists with exact
   paths and commands.
2. Narrow or sub-wave W4 so "one primary ASM-gen attempt" does not also hide five
   independent orphan source changes.
3. Resolve W3's ADMIT-vs-FIXPOINT requirement across SPEC, P3-B, and
   DISPATCH-PROMPT.
4. Tighten W1b/W3/W4 owner paths to exact files/globs and fix the bench-path
   naming mismatch.

After these folds, CH4 should re-run on the revised packet.
