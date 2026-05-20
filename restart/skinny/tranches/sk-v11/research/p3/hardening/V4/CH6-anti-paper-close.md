# SK-V11 S-P3 V4 CH6: Anti-Paper-Close

Pass: S-P3 Synthesis-Plan.
Cycle: V4 CHALLENGE.
Lens: CH6 anti-paper-close / next-tranche impact.
Date: 2026-05-20.
Scope: challenge whether the V4 S-P3 packet preserved the V3 measured-close
discipline for same-wave consumers, row floors, strict comparator/oracle
binding, kernel micro-proofs, and measured fixpoint/uncloseable proofs.

## Verdict

ACCEPT.

V4 is a stability-preserving packet under CH6. It keeps the V3 anti-paper-close
requirements intact: no row can admit by prose, W0 clamp, producer-only
telemetry, proof-only primitive evidence, orphan kernel, or future-wave promise;
row movement remains bound to same-wave consumers, named floors, strict
same-plane comparator/oracle evidence, micro-prove-first, and measured REDRESS
proofs for fixpoint rows (`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:19-23`,
`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:45-47`,
`restart/skinny/tranches/sk-v11/SPEC.md:16-59`).

## Evidence

### Governance And Stability

ACCEPT. The governing CH6 lens rejects self-reported "complete", "wired", or
"verified" status without live evidence and forbids future-phase deferral
(`restart/prompts/ORCHESTRATOR.md:81-88`). The S-P3 prompt specializes that into
bench-row thresholds, same-wave consumers, revert protocols, and no future-phase
close (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:140-145`). V3 already
accepted this CH6 shape, and V3 consolidation required a V4 stability cycle that
preserves V3 semantics to satisfy the two-cycle convergence rule
(`restart/skinny/tranches/sk-v11/research/p3/hardening/V3/CH6-anti-paper-close.md:11-19`,
`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:19-23`,
`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:45-47`).

### Accepted Inputs

ACCEPT. S-P1 convergence fixes the SK-V11 profile authority: direct residuals
are the primary close surface, W0-clamped rows remain non-admissions until
behavior measurement, diagnostic/parse-only/PMU/sidecar facts do not admit rows,
and SK-V11 still requires a measured non-JSON generated direct or typed parser
intervention (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:19-20`,
`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:34-55`).
S-P2 convergence leaves only C1-C7 as parser primitives, C8 as oracle/host sink,
C9 as accounting, support/proof surfaces as non-row-movers, W3 substrate repair
closed, and non-JSON generality measured by generated direct/typed parser
benchmarking (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:7-12`,
`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:23-32`).

### Same-Wave Consumers

ACCEPT. P3-A keeps every candidate as an intervention packet with owner paths,
scalar/checkasm state, micro-prove-first state, same-wave consumer, output
planes, and concrete row floors, and forbids row movement by analogy
(`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:69-74`).
The candidate table names product consumers for direct dispatch, string spans,
escaped segments, numeric spans, byte-set layout, non-JSON generated dispatch,
and typed guard work (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:120-124`,
`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:164-166`,
`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:210-215`,
`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:256-257`,
`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:299-303`,
`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:342-357`,
`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:383-386`).
P3-B also makes missing consumer a REJECT rather than deferral, and the dispatch
prompt requires plans to name same-wave consumer and redress to stay within the
selected owner paths (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:115-119`,
`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:88-96`,
`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:117-128`).

### Row Floors

ACCEPT. V4 preserves the direct floor rule: every residual direct row uses
`ceil(sonic-rs direct Mbps / 1.10)`, and both generated Track 1 and independent
Track 2/oracle must meet that floor (`restart/skinny/tranches/sk-v11/SPEC.md:116-134`,
`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:106-127`).
Direct and typed guard floors are explicit and remain maintain gates for waves
that touch direct, typed, generated, SIMD, parser, report, or gate surfaces
(`restart/skinny/tranches/sk-v11/SPEC.md:136-161`,
`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:128-159`).
P3-A repeats the concrete floor table across candidates, including the W1b/W2
non-JSON baseline/improvement split and the corrected typed guard floors
(`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:402-418`).

### Strict Comparator And Oracle Binding

ACCEPT. SPEC §0 requires each residual direct row to become strict same-run
`A / GO` on generated Track 1 plus independent Track 2/oracle or receive a
measured REDRESS proof, and it restricts direct admission to the strict direct
digest plane and typed admission to the strict typed plane
(`restart/skinny/tranches/sk-v11/SPEC.md:26-29`,
`restart/skinny/tranches/sk-v11/SPEC.md:51-53`). SPEC comparator classes make
same-run flaw probes and historical/sidecar signals planning-only, while
non-JSON oracles close generality only when the same wave benchmarks generated
Track 1 and consumes the oracle proof (`restart/skinny/tranches/sk-v11/SPEC.md:61-69`).
P3-C binds direct, typed, non-JSON, and SIMD/ASM admissions to same-output
comparator/oracle evidence and same-wave gate consumption, and rejects gates
missing generated Track 1, independent Track 2/oracle, strict same-plane
comparator/oracle, or gate consumer (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:161-185`,
`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:187-205`).
P3-D adds the telemetry-side fail-closed rules for stale anchors, strict-plane
mismatch, wrong strict comparator, unconsumed non-JSON oracle, direct-digest-as-
typed proof, W3 reopen claims, and producer-only fields
(`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:195-222`).

### Micro-Proof Requirements

ACCEPT. SPEC §2.1 requires W2-W7 plans to record scalar reference or exact
product oracle, strict checkasm when SIMD/ASM is used, same-host microbench,
observed value, threshold, run id, host, flags, sample count, feature gate,
same-wave consumer path, row gate, fallback, and REDRESS-tied reject boundary
before redress (`restart/skinny/tranches/sk-v11/SPEC.md:213-227`). P3-A makes
the same micro-prove-first checklist binding for all candidates, including
same-wave product path and gate consumption for every emitted field
(`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:421-430`).
P3-C sets the SIMD/ASM caller microbench rule and states the production row gate
still decides admission (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:176-185`).
P3-E keeps proof-only and inventory-only surfaces from becoming row movers until
a wave supplies scalar reference, strict parity/checkasm, feature/fallback,
caller microbench, same-wave consumer, and row gate
(`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:92-97`,
`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:125-130`).

### Measured Fixpoint And Close Discipline

ACCEPT. W8 remains a measured direct residual fixpoint, not a narrative close:
it enters after W3-W7 dispositions and W2 non-JSON admission or BLOCKED route,
admits only rows that meet §0.4 on both generated Track 1 and independent
Track 2/oracle, and records misses in REDRESS with attempted candidate,
measured tracks, comparator, floor, and guard status
(`restart/skinny/tranches/sk-v11/SPEC.md:692-732`). W9 may start only after W8
closes or escalates and every W1a-W8 wave has admitted, proof-closed, or
rejected with measurement, and its exit gate requires each residual direct row
to be `A / GO` or backed by an uncloseable proof naming attempted intervention,
Track 1, Track 2/oracle, comparator, floor, guard result, and routed remainder
(`restart/skinny/tranches/sk-v11/SPEC.md:740-770`). P3-B and P3-E independently
pre-block paper close by routed residual, W0-clamped admission, future-phase
promises, close drift, and G-Alpha presentation while any W1a-W8 wave lacks a
measured disposition (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:101-119`,
`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:153-154`,
`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:202-212`).
The dispatch prompt repeats that SK-V11 convergence requires W1a-W8 and W9 to
admit, proof-close, or reject with measurement, and that close cannot waive the
non-JSON benchmarked-intervention axis without a `BLOCKED` verdict
(`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:214-222`).

## Residual Notes

- W1a and W1b remain non-admitting prerequisites; they are acceptable under CH6
  because W1a is gate/report-only and W1b is baseline/oracle-only, and both fail
  closed before W2 may claim an intervention (`restart/skinny/tranches/sk-v11/SPEC.md:283-320`,
  `restart/skinny/tranches/sk-v11/SPEC.md:326-377`).
- A `BLOCKED` grammar-generalization fixpoint is not an ACCEPT close; it is the
  fail-closed state required when the non-JSON benchmarked intervention axis
  cannot be honestly admitted (`restart/skinny/tranches/sk-v11/SPEC.md:755-763`,
  `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:216-222`).

## File Changed

- `restart/skinny/tranches/sk-v11/research/p3/hardening/V4/CH6-anti-paper-close.md`
