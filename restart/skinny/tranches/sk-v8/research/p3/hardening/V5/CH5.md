# SK-V8 S-P3 Hardening V5 CH5: Hidden Coupling

## Scope

V5 CH5 reviewed the unchanged V4-folded S-P3 packet under the hidden-coupling,
same-wave consumer, and cross-wave dependency lenses. This review is planning
only and does not authorize or implement any SK-V8 wave.

Read set:

- `restart/prompts/ORCHESTRATOR.md` sections 3W and 3Z.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
- `restart/skinny/tranches/sk-v8/SPEC.md`.
- `restart/skinny/tranches/sk-v8/HANDOFF.md`.
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`.
- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`
  through `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md`.
- `restart/skinny/tranches/sk-v8/research/p3/p3-v4-exact-traceability-fold.md`.
- `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md`.

## Verdict

ACCEPT.

Confidence: 97%.

Blockers: none.

Required fold if any: none. No new critical defect was found, so V5 CH5 does not
propose a fold.

## Evidence

The V4 fold is explicitly traceability-only and preserves the operative
constraints relevant to CH5: no new directive, BIR, substrate, `BackendShape`,
`UnionTape`, public substrate API, parser-owned cursor or facts, sidecar
substrate, or consumer-later primitive, and it preserves W3's one-Tape
representation-replacement model (`p3-v4-exact-traceability-fold.md:1`,
`:28`). V4 consolidation records CH5 as ACCEPT at 96% and records the V4 cycle
as a qualifying ACCEPT cycle with no open critical defect
(`HARDENING-S-P3-V4-CONSOLIDATED.md:7`, `:17`, `:20`, `:31`).

The governing challenge rules still bind this V5 review. ORCHESTRATOR CH5
requires rejection of parallel substrate, sidecar producer, renamed-scanner Lock
1 violations, Track 1 equals Track 2 dishonesty, and broken substrate union
(`restart/prompts/ORCHESTRATOR.md:74`). ORCHESTRATOR convergence requires two
consecutive qualifying ACCEPT cycles at confidence at least 95%, with no
critical defects or orphaned unresolved REVISE findings
(`restart/prompts/ORCHESTRATOR.md:104`). PASS-3 repeats the same CH5 lens and
requires S-P3 to remain a synthesis plan, not an implementation pass
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:25`, `:102`, `:151`).

No implementation route can close from S-P3 alone. The live SPEC dispatch lock
says S-P3 emits no SK-V8 implementation wave, G-Alpha may dispatch W0 only, and
W1-W6 remain blocked until their exact entry gates, evidence, owner paths,
challenge acceptance, and user or orchestrator dispatch are satisfied
(`restart/skinny/tranches/sk-v8/SPEC.md:29`). DISPATCH-PROMPT carries the same
lock, names W0 as telemetry-only, and blocks W1-W6 unless W0 is admitted and a
fresh per-wave packet names exact paths, gates, same-wave consumers, reversions,
pre-block references, Lock 14 proof, and challenge acceptance
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:1`, `:56`, `:90`).
HANDOFF likewise states that S-P3/G-Alpha do not authorize W3 and that W1-W6
need the later gated packet and challenge path
(`restart/skinny/tranches/sk-v8/HANDOFF.md:98`, `:119`, `:191`).

Same-wave consumer discipline is explicit and not papered over by telemetry or
future follow-up. SPEC forbids any primitive, kernel, generated path, or
substrate representation change from entering without a same-wave hot-path
production consumer and rejects deferrals, future consumers, and paper closes
(`restart/skinny/tranches/sk-v8/SPEC.md:230`). The pre-block ledger applies the
same rule globally and treats no-deferral as a hard gate, not an explanatory
note (`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:103`,
`:174`). P3-C's falsifiability gates require failure to be observable in the
same wave, so a producer-only artifact or telemetry-only consumer cannot close a
route (`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:14`,
`:41`, `:370`).

W1 does not create a future-only behavior proof. SPEC confines W1 to
`CostFacts` and strict-admission gate consumption, with parser behavior and
generated output unchanged unless a separate challenged behavior consumer is
accepted; W1 rejection blocks W2-W6 behavior
(`restart/skinny/tranches/sk-v8/SPEC.md:385`, `:423`, `:437`). DISPATCH-PROMPT
names `gate-json --with-cost-facts` and strict refusal as W1's consumer and
blocks behavior changes, generic JSON policy, generated drift, and producer-only
`CostFacts` (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:108`). The P3
artifacts repeat that W1 cannot be used as a performance claim or route reopen
without later same-wave behavior proof
(`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:24`,
`restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:28`,
`:29`).

W3 remains one Tape by representation replacement, not a side substrate or
hidden W4/W5 dependency. SPEC limits W3 Tier A to a retained stage-1 structural
index inside the existing Tape, with scan-written structural classes or ordinals
and generated JSON retained parser consumption in the same wave; `tape_vs_tape`,
direct/SinkOnly rows, path rows, Track 2 audit, and residual telemetry are
explicitly not W3 consumers (`restart/skinny/tranches/sk-v8/SPEC.md:506`,
`:544`, `:562`). SPEC also pre-blocks `BackendShape`, `UnionTape`, public API,
sidecar event vector, retained parser cursor, aux density table, parser-owned
slot, second scan, old append path, Tier B, and `tape_vs_tape` as a consumer
(`restart/skinny/tranches/sk-v8/SPEC.md:580`). DISPATCH-PROMPT and HANDOFF carry
the same W3 requirements and blocked surfaces
(`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:127`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:56`). P3-F preserves the W3 same-wave
consumer as the generated JSON retained parse path and keeps Tier B and
`tape_vs_tape` outside W3 closure
(`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:13`, `:40`,
`:83`).

W4 and W5 do not act as hidden W3 follow-up. SPEC requires W4 to enter only
after W0/W1 plus W2/W3 disposition or route, and its same-wave consumer is the
selected direct rows consuming generated Track 1 direct/SinkOnly code with an
independent Track 2 proof; Track 2 may not call generated SinkOnly, typed
helpers, Track 1, or a shared benchmark-private parser
(`restart/skinny/tranches/sk-v8/SPEC.md:605`, `:631`, `:646`). W5 enters only
after W1-W4 dispositions and is audit-only; it cannot supply a deferred W3 or W4
production consumer (`restart/skinny/tranches/sk-v8/SPEC.md:663`, `:681`,
`:696`). P3-B and P3-D preserve that sequencing and consumer split
(`restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md:52`,
`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:133`).

Lock 14 and strict-vs-strict discipline remain grammar-neutral and do not hide
a JSON-specific substrate. SPEC requires generated byte-set or opaque ordinal
classes, scalar mirrors, same-wave consumers, and non-JSON counter-evidence
before any structural classification can generalize beyond JSON
(`restart/skinny/tranches/sk-v8/SPEC.md:300`). The pre-block list rejects
generic JSON APIs, public structural APIs, hidden grammar directives, or
grammar-specific structural policy (`restart/skinny/tranches/sk-v8/SPEC.md:767`,
`:814`). P3-D's telemetry additions remain gate-consumed and explicitly do not
add a directive, BIR field, public substrate field, or sixth `BackendShape`
(`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:96`).

## Residual Non-Blocking Risks

- The retained Tape plan remains a high-risk implementation wave because it must
  replace representation rather than append a second structural channel. The
  current packet blocks that risk through exact owner-path, scalar/checkasm,
  same-wave generated parser consumption, and challenge gates; it is not an open
  CH5 blocker.
- W1 `CostFacts` could be overread as behavioral proof by a future implementer.
  The live SPEC, DISPATCH, HANDOFF, and P3 ledger all reject that interpretation
  by tying W1 to strict gate consumption only.
- V5 CH5 is only one lens. This file can support the second consecutive
  qualifying challenge cycle for CH5, but overall S-P3 convergence still depends
  on the full V5 consolidation, not this single review.
