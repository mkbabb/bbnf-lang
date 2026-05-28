# T-P2 V1 Challenge Context — SK-V15

Challenge target commit: `0fb621116`.

Target packet:

- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`

Authority:

1. `restart/prompts/totality/PASS-2-RESEARCH.md` §3.
2. `restart/prompts/ORCHESTRATOR.md` §3W and §3Z.
3. `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md`.
4. `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`.
5. `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`.
6. `restart/skinny/tranches/sk-v15/SPEC.md`.

Entry facts:

- T-P1 entry state is `CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z`; do not
  rewrite it into a normal §3Z lock.
- Existing SK-V14 T-P2 hardening files were removed from the live tree at
  `31d301e97`; V1 hardening starts fresh here.
- CH1-CH6 follow `PASS-2-RESEARCH.md` §3.
- CH7 is the SK-V15 overfit-prune guard from the dispatch context.

SK-V15 CH7 scope:

1. Broadcast admission detection: repeated throughput tuples across
   distinct row IDs are evidence of broadcast unless each row has independent
   command, input, equality, and timing.
2. Gate-exclusion detection: a grep/check gate that excludes files introduced
   by the same change is a contrivance.
3. Wave-graph cycle detection: deletion or retirement routes must prove the
   replacement provider precedes the deletion consumer.
4. CSS overfit detection: CSS claims must not treat brace counters,
   fact-stream strings, or byte-identical generated modules as CSSOM/value
   API parity.
5. aarch64 close-route detection: x86/AVX-512 evidence cannot close SK-V15
   M5 Max work.

Disposition vocabulary: `ACCEPT`, `REVISE`, or `REJECT`.

Every CH file must include:

- lens name;
- disposition;
- critical findings table;
- evidence inspected;
- fold requirements if disposition is REVISE or REJECT;
- explicit note on whether the issue blocks T-P2 V1 convergence.
