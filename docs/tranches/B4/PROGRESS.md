# B4 — Progress Log

Dated execution log for tranche B4, the codegen-emission correctness +
W0' re-land hardening tranche.

- `Status`: in_progress
- `Current wave`: W0 closed; W1 planned
- `Next wave`: W1 (consumer-fixture polish for `FusedOutput<R>`
  migration; planned, implementation in a later dispatch)

---

## 2026-04-25 — W0 closed (codegen-emission `syn::parse2` fix landed)

W0 dispatched and closed in a single execution round. The defect
traced to `crates/core/src/generate/regex/emit/simd.rs` lines 179
and 200 — the `emit_structural_bitmap_kernel` AVX2 nibble-LUT scan
emitter produced two `break 'avx2_scan ::core::option::Option::Some(...)`
forms whose bare-path break-values syn's expression parser rejects
with "expected loop or block expression". Syn admits only a loop, a
brace block, or a labelled block as the break-value, not a path
expression. The bbnf grammar exercises the kernel through prettify
codegen (negated character-class regexes inside `@pretty`-marked
rules: `big_comment`, `comment`, `pretty_hint`, `grammar_item`); the
json grammar does not.

Fix wraps each break-value in `{ ... }` so the emitted form becomes
`break 'avx2_scan { ::core::option::Option::Some(...) }`. The block
satisfies syn's expectation while preserving identical runtime
semantics. Single-source change at the offending emitter; no shadow
surface, no `try`-fallback at the consumer, no compositional change
elsewhere in the codegen pipeline.

Hard-gate evidence (all closed):

- `cargo xtask regen --grammar bbnf` exit 0; `crates/core/src/grammar/generated/bbnf.rs`
  written populated at 34,048 lines (~3% above the existing
  `crates/core/src/grammar/generated.rs` reference of 33,279 lines).
  Per-phase walls: compile_paths_request 3.15 ms; generate_all
  9.22 ms; prettyplease 59.23 ms.
- `cargo xtask regen --grammar json` exit 0 (regression sanity).
  Per-phase walls: compile_paths_request 1.46 ms; generate_all
  3.00 ms; prettyplease 10.27 ms.
- `cargo check -p bbnf -p xtask -p bbnf_derive --profile ax-iter`
  exit 0 in 5.89 s.
- `cargo nextest run -p tape --profile ax-iter` 100/100 passed in
  0.186 s.

Worktree HEAD: `8a1c1c94`. Fix commit ready for cherry-pick to master.

## 2026-04-25 — Plan authored + W0 opened

B4 opens as a focused two-wave tranche. The predecessor tranche closed
on parser-baseline restoration; its FINAL noted that the bbnf self-host
xtask regen reaches the codegen TokenStream emission stage cleanly and
then fails at `syn::parse2` with "expected loop or block expression".
The TokenStream produced by `generate_all` for the bbnf grammar
contains a position-context defect: an expression slot receives a
syntactic form that Rust's grammar admits only as a statement or
block. The defect surfaces only on the bbnf grammar (which exercises
shapes the smaller json grammar does not — Pratt operator chains, deep
alternation, recursion-elimination dispatchers, visitor codegen).

W0's scope: reproduce the panic, capture the offending TokenStream
excerpt, attribute the defect to a single emitter at file:line, fix at
source, verify end-to-end (bbnf regen exit 0, json regen sanity,
workspace check, tape tests), cherry-pick to master.

W1's scope: forward-pointer to the consumer-fixture polish that
AY-II.W0' migration deferred (per `AY-II/PATH-FORWARD.md` §"Immediate
cleanup targets"). Implementation lands in a later dispatch.

Authored in this initial state:

- `B4.md` — 6 invariants, two-wave schedule, cross-tranche debt
  ledger pointing at AY-II.W0' close ceremony at B4 close, escape
  clause covering scope expansion if multiple emitter shapes
  contribute to the defect.
- `waves/W0.md` — phase decomposition: reproduce + capture, root-
  cause analysis, fix at source, verify, cherry-pick + close.
- `waves/W1.md` — forward-pointer for consumer-fixture polish.
- `AGENT_DISPATCH.md` — W0 sub-agent dispatch surface with explicit
  anti-patterns (no `try-or-fallback`, no callsite work-around, no
  `git reset --hard`, no γ-η revert).
- `PROGRESS.md` — this file.

W0 dispatched in this state. Master HEAD: `573cc672` (predecessor
tranche close).
