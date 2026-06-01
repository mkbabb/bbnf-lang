# S-P0 audit-overfit hardening V3 — CH1 Correctness (2nd confirm, independent re-grep)

Independent re-grep at HEAD `83b66db42` (`git rev-parse HEAD` matches). V3 confirms the 8 V1
folds held through V2 with no orphan REVISE and no new defect introduced.

## Independent witness re-grep
- `CSS_GENERATED_RS` const `&str` `runtime_generator.rs:701`, body closes `:1611` = **910 lines**
  (re-measured — corroborates the R1-CH1 fold's disk-measured span superseding the seed 646–910).
- `RuntimeEmitterKind{CompiledLowering,RequestFacts}` `grammar_provider.rs:40-42` + `:110` dispatch.
- 7× css_l4 `generated.rs` md5 `b654562c…` (1 distinct).
- phantom `G: EventGrammar = AnyGrammar` `tape/mod.rs:175`.
- metalang `parse_w11_1_number` ×7 in shipped `json/generated.rs`.
- `RuntimeTarget` 12 fields `regen.rs:6-19`; derives only `Clone, Copy, Debug` (`regen.rs:5`) —
  NOT `PartialEq` (corroborates the R1-CH5 fold's one-line-derive cost).
- BOTH nested structs: `frontend_requirements` `regen.rs:17` (struct `grammar_provider.rs:46`,
  `PartialEq, Eq`) AND `output_labels` `regen.rs:18` (struct `grammar_provider.rs:92`,
  `PartialEq, Eq`) — R16 is now stated at the full-expanded-row altitude (both, not one).

## Fold-hold confirm
- R1-CH1 (a1 LOC annotation): `grep "SUPERSEDED"` in a1 §L1 → present; the 910 disk-measure stands.
- R1-CH2 (a1 (b) mutate-falsifier): "EMITTED OUTPUT VARIES … per-primitive mutate-falsifier"
  present `a1:121-123`.
- R1-CH5 (both nested structs): `frontend_requirements` present in a3, SYNTHESIS, a2.
- R1-CH6/R2-CH6 (SYNTHESIS rows): R-A0-1 "beats CSSOM"/"equal-work" REJECT clause + R-A0-2
  `generator_grammar_count == 3` collapse-to-one both present `SYNTHESIS:98-99`.

All V1+V2 folds held; every dispositive path:line / SHA / count / md5 resolves. No orphan, no new
correctness defect.

## Tally
ACCEPT 10 · REVISE 0 · REJECT 0 — **100%**.

TALLY accept=10 revise=0 reject=0
