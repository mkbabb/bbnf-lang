# SK-V12 S-P2 CHALLENGE V1: CH4 COST

Pass: S-P2 Research CHALLENGE. Cycle: V1.
Date: 2026-05-20.
Lens: CH4 COST.
Disposition: REVISE.

## §1 — Verdict

REVISE. The S-P2 V1 corpus has enough scalar-oracle/checkasm discipline to
keep the candidate pool alive, but it is not CH4-complete. PASS-2 requires
every candidate to carry scalar-reference status, checkasm-parity expectation,
and a same-wave-consumer note (`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`:124`);
the orchestrator makes scalar/checkasm parity and same-wave consumer separate
non-negotiables (`restart/prompts/ORCHESTRATOR.md:205`-`:206`). P2-B and P2-E
mostly satisfy that triad. P2-A, P2-C, P2-D, and P2-F need a fold before S-P2
can converge.

## §2 — Findings

| Finding | Evidence | CH4 effect |
| --- | --- | --- |
| P2-B is the strongest CH4-complete artifact. | Its candidate table has explicit `Scalar-ref status`, `Strict parity/checkasm`, and `Same-wave consumer rule` columns for all 12 admission gates (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:36`-`:49`). The cited local harnesses actually compare candidates against scalar references, for example eq-set parity calls scalar and candidate side by side (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:156`-`:179`) and the table classifier compares dispatch against `byte_class_from_table_64_scalar` (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs:13`-`:49`). | ACCEPT for P2-B under CH4. Use P2-B's table shape as the fold template. |
| P2-E is CH4-complete for its five parse-that gaps. | Each candidate carries a scalar sketch, arch/scalar status, checkasm expectation, and same-wave consumer note. Examples: `pt_byte_set_run_skip` carries the scalar loop, strict checkasm expectation, and generated-consumer note (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:60`-`:89`); `pt_digit_run_span_accumulate` carries scalar sketch, AArch64 parity condition, checkasm expectation, and generated-number consumer (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:175`-`:216`); `pt_escaped_string_segments` carries scalar segment sketch, strict subprimitive checkasm, and same-wave generated output consumer (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:307`-`:354`). | ACCEPT for P2-E under CH4. |
| P2-A's candidate implications are not CH4-complete. | C1 says scalar reference first and optional SIMD checkasm parity, but the consumer is only implicit in the shape (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:31`-`:39`). C2 only states strict control-byte behavior and gives no scalar-reference status, checkasm expectation, or same-wave consumer note (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:41`-`:51`). C4 and C5 require fresh non-JSON consumers or avoid object-carry routes, but do not state scalar-reference or checkasm status (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:65`-`:87`). C6 and C7 likewise name shapes and boundaries without the triad (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:89`-`:111`). | REVISE P2-A. Every C1-C7 candidate needs an explicit CH4 row or the artifact must mark it support-only/non-candidate. |
| P2-C has scalar and checkasm detail, but same-wave-consumer notes are inconsistent. | C1-C8 each list scalar-ref status and checkasm expectation (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:40`-`:102`). Only some rows explicitly mention a consumer: C2 requires proving the consumer reads one canonical stream (`:52`), C5 requires same-wave source delta (`:76`), and C8 limits corpus parity through a same-wave consumer (`:100`). C1, C3, C4, C6, and C7 leave consumer admission implicit through "caller-local" or support wording (`:42`-`:46`, `:58`-`:62`, `:66`-`:70`, `:82`-`:86`, `:90`-`:94`). | REVISE P2-C. Add a same-wave-consumer bullet to each candidate, including support-only candidates. |
| P2-D's table is useful substrate research, but it lacks CH4 accounting columns. | The table carries `Scalar-reference status` and `Arch surface`, but no checkasm status and no same-wave-consumer note (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:73`-`:78`). For non-SIMD capacity/sparse-flag work, checkasm can be `N/A`, but the artifact still needs a proof/consumer note. The rejected `structural_class_lane_union` row says no admissible scalar reference exists under Lock 1 (`:78`), which is correct only if it remains rejected and is not counted as selectable. | REVISE P2-D. Add `Checkasm/parity status` and `Same-wave consumer/proof note`; move or label the rejected structural lane as non-candidate. |
| P2-F is grammar-neutral abstraction, not yet CH4-complete candidate accounting. | The F1-F8 family table includes scalar-ref status and arch notes, but no per-family checkasm expectation column and no same-wave-consumer note column (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:28`-`:36`). §3 supplies a same-wave generated-consumer condition for F1 only (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:42`); F2-F8 remain implicit or are described as oracle/accounting surfaces (`:43`-`:49`). | REVISE P2-F. Add triad status per family or split F7/F8 out as oracle/accounting guardrails rather than candidate primitives. |
| Existing checkasm surface supports the fold, but does not waive same-wave consumer duties. | The local harness already has parity shapes for scalar-vs-candidate calls, alignment sweeps, stack canaries, and signal guards (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:41`-`:51`). It also keeps the no-orphan rule binding (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:251`-`:254`). REDRESS 120 keeps generated non-JSON baseline work first and direct residual rows exhausted (`skinny/REDRESS.md:3531`-`:3552`), while RESULTS remains `N-direct / NoGo` (`skinny/RESULTS.md:143`). | The pass should fold missing consumer notes now; S-P3 must not infer row movement from primitive parity alone. |

## §3 — Revise List

1. Revise `p2a-sota-teardown.md` §2 so every C1-C7 candidate has explicit
   columns or bullets for scalar-reference status, checkasm-parity expectation
   or `N/A`, and same-wave-consumer note. C2, C4, C5, C6, and C7 are the
   immediate blockers.
2. Revise `p2c-arch-esoterica.md` §2 so C1-C8 each has an explicit
   same-wave-consumer note. Support primitives must say "not selectable until
   paired with {named generated/runtime consumer}" rather than relying on
   caller-local wording.
3. Revise `p2d-substrate-tape.md` §2 with CH4 columns: `Checkasm/parity
   status` and `Same-wave consumer/proof note`. Non-SIMD rows may state
   `checkasm N/A`; optional SIMD/table packing must state the parity gate before
   use. Keep `structural_class_lane_union` outside the selectable candidate pool
   or mark it `REJECTED / CH4-ineligible`.
4. Revise `p2f-grammar-neutral.md` §2 so F1-F8 carry the triad directly, or
   separate oracle-only/accounting-only families from candidate primitives. F7
   and F8 should not be counted as parser candidates unless a legal consumer and
   evidence gate are named.
5. Preserve P2-B and P2-E as accepted CH4 models. Their table/paragraph shapes
   are sufficient for the fold and should be reused rather than reinterpreted.

## §4 — Boundaries

No source, RESULTS, REDRESS, lock, or sibling CH file change is requested by
this lens. The required fold is documentary and belongs in the next S-P2 cycle.
