# SK-V11 W6 CH2 - Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality, Lock 14, and non-JSON close-axis honesty.
Scope: adversarial review of
`restart/skinny/tranches/sk-v11/research/w6/w6-plan-escaped-segment-digest-fold.md`.
Source edits: none.

## Disposition

ACCEPT.

CH2 accepts the W6 plan as a JSON direct-plane intervention only. It does not
accept W6 as a non-JSON grammar proof, a Lock 14 close-axis proof, or a generic
escaped-string policy intervention.

No plan edit is required before redress if the implementation keeps the plan's
default owner shape: `skinny/crates/bbnf-bench/src/direct_struct.rs`, optional
JSON parity tests, gate/report consumption, `RESULTS.md` on measured pass only,
and `REDRESS.md` for disposition. Any redress that edits codegen, generated
runtime, `runtime/src/grammars/json/sink.rs`, runtime outside generated JSON, or
generic crates beyond the narrow bounds below re-enters CHALLENGE as REVISE.

## Findings

1. The selected behavior is JSON-specific by design, and that is admissible for
   W6. The plan selects a JSON direct-plane escaped-segment digest fold for
   `G-W6-ESCAPE-SEGMENT-DIRECT` and names `unicode_mixed/direct_to_struct` as
   the primary row (`w6-plan-escaped-segment-digest-fold.md:9`,
   `w6-plan-escaped-segment-digest-fold.md:35`). SPEC Section 10 permits
   selected JSON direct rows from `unicode_escapes`, `unicode_mixed`, and
   `y_string_unicode` (`SPEC.md:625`-`628`).

2. The default owner set avoids the Lock 14 generic-policy trap. The plan keeps
   the production delta in `bbnf-bench/src/direct_struct.rs`, explicitly rejects
   `runtime/src/grammars/json/sink.rs`, codegen, generated JSON, and generated
   typed edits for this plan, and states that `parse-that-regex` is only
   optional if CHALLENGE accepts a small scalar oracle/test extraction
   (`w6-plan-escaped-segment-digest-fold.md:53`-`75`). That shape respects the
   SPEC non-negotiable barring JSON policy in generic crates or runtime outside
   generated per-grammar modules (`SPEC.md:170`-`178`).

3. REDRESS 113 is carried honestly. W6 research says the current tranche cannot
   satisfy the non-JSON close axis without creating, benchmarking, and
   gate-consuming a generated non-JSON direct/typed parser plus independent
   same-plane oracle (`w6-R5-grammar-neutral-escape-policy.md:11`-`20`). The
   plan repeats that W6 does not close the SK-V11 non-JSON grammar axis and
   carries REDRESS 113 forward (`w6-plan-escaped-segment-digest-fold.md:116`).

4. The plan does not improperly claim SK-V11 close-condition 8. SPEC requires at
   least one non-JSON grammar to carry an admitted, benchmarked SK-V11
   intervention through a generated direct or typed parser (`SPEC.md:43`-`45`).
   The W6 plan has no such parser, no non-JSON Track 1, and no non-JSON oracle.
   Because it says so explicitly, CH2 does not treat the absence of
   close-condition-8 evidence as a W6 plan defect.

5. The remaining generality risk is conditional, not current. SPEC Section 10
   requires JSON surrogate policy to stay in the generated JSON caller, with CSS
   variable-width escapes and BBNF literal policy kept per grammar
   (`SPEC.md:619`-`623`). R5 sharpens that C3 is grammar-neutral only at the
   hex-run / escaped-segment layer, and that JSON simple escapes and surrogate
   handling must not be advertised as grammar-neutral (`w6-R5-grammar-neutral-escape-policy.md:55`-`68`).
   The plan is acceptable only while it keeps that policy boundary intact.

## Binding CH2 Conditions

1. W6 redress must not claim Lock 14 closure, close-condition-8 progress,
   grammar-generalization evidence, or non-JSON admission. A measured JSON pass
   may move only the selected JSON direct row(s) and their guards.

2. If no JSON direct row closes, W6 may not pivot during redress to a CSS,
   Sheets, or BBNF-self escaped-string/hex-color claim under this plan. That
   would require a revised plan with generated non-JSON Track 1, independent
   same-plane oracle, baseline Mbps, gate/report consumption, and explicit
   owner authority, as R5 lists (`w6-R5-grammar-neutral-escape-policy.md:100`-`129`).

3. `parse-that-regex` may not receive a JSON-semantic helper under this ACCEPT.
   Acceptable generic-crate edits are limited to tests or strictly neutral
   hex/segment primitives that encode no JSON slash-escape table, object/key
   role, surrogate joining, corpus special case, or output digest fact. If W6
   needs JSON simple-escape mapping or surrogate policy outside
   `direct_struct.rs`, CH2 disposition becomes REVISE.

4. Gate/report output for W6 must keep `grammar_id=json`, `output_plane=direct`,
   and W6 JSON-row provenance. It must not introduce a telemetry field or status
   implying a non-JSON proof, and it must consume REDRESS 113 as carried
   unresolved if the row admits.

5. Any codegen, runtime-outside-JSON, generated non-JSON, or generic parser edit
   must run and consume a same-wave CSS L4, Sheets, or BBNF-self proof before a
   generality claim can pass (`SPEC.md:230`-`245`). The current W6 plan has no
   such proof, so those edits are outside this ACCEPT.

## Required Changes

None to the W6 plan before redress.

The redress agent must carry the five binding CH2 conditions above into the
implementation and REDRESS entry. Violating any one of them converts this lens
to REVISE for grammar generality.
