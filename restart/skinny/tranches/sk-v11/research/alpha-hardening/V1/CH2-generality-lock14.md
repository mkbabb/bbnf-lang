# SK-V11 Pass Alpha CHALLENGE V1 - CH2 Generality / Lock 14

Date: 2026-05-19.
Lens: CH2 generality / Lock 14.
Scope: review `restart/skinny/tranches/sk-v11/SYNTHESIS.md`,
`HANDOFF.md`, and Alpha A-F for grammar neutrality, non-JSON execution
requirements, generic-crate JSON-policy leakage, and prose-proof avoidance.

## Disposition

ACCEPT.

The Alpha V1 packet makes non-JSON execution a close condition, blocks generic
JSON policy leakage, and keeps Lock 14 prose out of the admission path. No CH2
blocking defect is open.

## Findings

| Check | Result | Evidence |
|---|---|---|
| Non-JSON grammar generalization is exercised, not asserted | PASS | `SYNTHESIS.md` requires at least one non-JSON grammar to carry an admitted, benchmarked SK-V11 intervention through a generated direct or typed parser, and states that a Lock 14 prose proof alone does not close the axis (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:55-59`). The dedicated grammar goal repeats that the proof must be generated-parser work with a benchmark row, comparator or oracle, and same-wave gate consumer (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:148-163`). `HANDOFF.md` carries the same bound axis and says the non-JSON wave should exercise the same primitive family that S-P1/S-P2 identify on the JSON residual surface (`restart/skinny/tranches/sk-v11/HANDOFF.md:69-80`). |
| Generic-crate JSON-policy leakage is blocked | PASS | `SYNTHESIS.md` forbids generic-crate JSON policy and requires generic, codegen, runtime-outside-JSON, `bbnf-simd`, or `parse-that-regex` edits to carry named non-JSON proof in the same SK-V11 bracket (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:70-76`). Alpha-C states JSON quote, slash, `\\u`, surrogate, number, whitespace, output, and row policy belong in generated per-grammar templates, not generic crates (`restart/skinny/tranches/sk-v11/research/alpha/alpha-C-redress-digest.md:89-94`). Alpha-E repeats that generic JSON policy in `parse-that-regex`, `bbnf-simd`, `passes`, `ir`, or `codegen` cannot pass CH2 (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:485-487`). |
| CSS/Sheets/BBNF-self path is concrete enough for S-P1/S-P2 | PASS | The preferred order is explicit: CSS L4 declaration values, then Sheets, then BBNF-self (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:150-159`). S-P1 must inventory the non-JSON grammar harness and first runnable profile path (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:255-268`). S-P2 must ground a grammar-neutral abstraction for CSS L4 / Sheets / BBNF-self before S-P3 writes the packet (`restart/skinny/tranches/sk-v11/HANDOFF.md:100-113`). The named repo paths resolve for the first two choices: `grammar/css/l4/`, `crates/core/benches/css/`, `crates/core/tests/parse_with_css_l4.rs`, `grammar/google-sheets/google-sheets.bbnf`, `crates/core/benches/google_sheets/`, and `crates/core/tests/parse_with_google_sheets.rs`. BBNF-self also has `grammar/bbnf/bbnf.bbnf` and `crates/core/tests/bbnf_self_parity.rs`. |
| Candidate shortlist is grammar-neutral rather than JSON-only | PASS | Alpha-E global eligibility requires every SIMD/ASM candidate to micro-prove on representative JSON and non-JSON slices, and every generic/codegen/runtime-outside-JSON edit to carry a named non-JSON benchmark (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:57-74`). C1 creates a non-JSON benchmark gate and requires a rendered CSS L4 or Sheets row (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:110-159`). C2 requires a CSS L4 declaration-value scanner or Sheets tokenizer (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:179-233`). C3 ties numeric work to CSS numeric units or Sheets literals (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:255-308`). C4 separates grammar-owned escape policy from byte-copy/fold and requires CSS or Sheets escape parity (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:330-381`). C5 requires CSS or Sheets string/identifier proof (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:405-450`). |
| Lock 14 prose proof is not counted as close | PASS | Alpha-F says grammar generalization must be execution and that a Lock 14 prose statement is not enough (`restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md:96-99`). It also couples direct, non-JSON, and SIMD axes so a grammar-generalization wave cannot count unless it benchmarks a real generated direct or typed parser (`restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md:106-112`). The challenge notes explicitly invite rejection if the non-JSON axis is reduced to a prose Lock 14 proof (`restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md:156-164`). |

## Non-Blocking Notes For Fold

1. S-P3 should not weaken C2-C5's "CSS or Sheets owner selected by S-P3"
   phrasing into an unnamed future proof. The acceptable fold is to bind one
   concrete non-JSON owner path, benchmark command, oracle/comparator, and gate
   consumer before any source wave dispatches.
2. If S-P1 finds CSS L4 is not the best runnable path, the packet already
   allows Sheets or BBNF-self, but the replacement still needs generated direct
   or typed parser evidence, not grammar fixture parsing alone.
3. If non-JSON rows enter `skinny/RESULTS.md`, the report and gate consumer
   must update in the same wave; `SYNTHESIS.md` already requires this and
   rejects producer-only fields (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:225-236`).

## CH2 Verdict

ACCEPT. The Alpha packet satisfies Lock 14 for Pass Alpha V1. It advances
generalization as an executable gate: one admitted non-JSON generated parser
intervention is required for close, generic JSON policy is pre-blocked, and
S-P1/S-P2 have concrete CSS L4 / Sheets / BBNF-self surfaces to profile and
research before S-P3 writes `SPEC.md`.
