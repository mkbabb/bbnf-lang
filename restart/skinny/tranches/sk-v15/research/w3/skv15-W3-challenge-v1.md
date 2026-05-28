# SK-V15 W3 CHALLENGE V1

Input plan: `restart/skinny/tranches/sk-v15/research/w3/skv15-W3-plan.md`
at commit `d3160ec9e`.

## Verdict

ACCEPT 7/7. W3 may enter redress only under the receiver guardrails below.

| Lens | Verdict | Acceptance reason |
|---|---|---|
| CH1 correctness | ACCEPT | The plan removes one actual leak family: static profile/mode/config selection in generic codegen. It does not claim CSS typed output or old-proof retirement. |
| CH2 generality | ACCEPT | Request-carried contracts make expected files, emitter kind, frontend requirements, and output labels data rather than JSON/CSS profile branches. |
| CH3 regression | ACCEPT | JSON compiled output remains on the source-lowered path, CSS output labels remain byte-equivalent when possible, and generated CSS dirty state is not repaired by W3. |
| CH4 cost | ACCEPT | Owner paths fit the 150-320 manual LOC envelope if the redress stays on contract plumbing and tests. CSS typed provider, W6 retime, and Decision work remain out of scope. |
| CH5 hidden coupling | ACCEPT | The plan forbids second tape, parallel source pass, Track 1 == Track 2 sidecar, and CSS old-proof deletion. Generic passes are excluded rather than widened. |
| CH6 anti-paper-close | ACCEPT | Redress must prove the contract path with executable codegen tests and `gate-json --check-results`, not detached documentation. |
| CH7 overfit-prune / gate-exclusion | ACCEPT | The non-CSS receiver proof cannot be a toy fixture or row-count proxy. It must use a real named non-CSS grammar source path, or W3 records intrinsic block instead of closing that receiver. |

## Receiver Guardrails

- A synthetic one-line non-CSS frontend source is REJECT. The non-CSS proof must
  use an actual repository grammar from the allowed receiver set, preferably
  `grammar/bbnf/*.bbnf` for BBNF-self.
- The proof may be a generated-output fixture inside `codegen` tests, but it
  must exercise `RuntimeGenerationRequest`, frontend closure parsing, contract
  validation, and runtime generation. A contract struct compiled but unused is
  paper close.
- If actual BBNF-self / Sheets / CSV / math sources cannot satisfy the current
  frontend materiality requirements without inventing fake grammar directives,
  redress must report the receiver as intrinsically blocked and not mark the
  receiver obligation closed.
- Do not alter `CSS_GENERATED_RS`, `CssFullParseSummary`, generated CSS runtime
  files, root Pattern H runtime files, or benchmark ledgers.

## Redress Guardrails

- Keep W3 source edits inside the owner paths named in the plan.
- `rg` must show the old static family tokens are gone from live generic
  codegen: `RuntimeGenerationMode`, `PassCompiled`, `FrontendFacts`,
  `css_profile_config`, `CssProfileConfig`, `runtime_profiles`, `CSS_L4_`, and
  `JSON_PROFILE`.
- CSS profile names may remain in `regen_css.rs` as target data, but not in
  generic codegen branch logic.
- `check-css-l4-*` failures caused by pre-existing dirty generated files are
  recorded as blockers, not repaired in W3.
