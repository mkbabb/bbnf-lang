# B4 — Progress Log

Dated execution log for tranche B4, the codegen-emission correctness +
W0' re-land hardening tranche.

- `Status`: in_progress
- `Current wave`: W0 (codegen-emission `syn::parse2` fix)
- `Next wave`: W1 (consumer-fixture polish for `FusedOutput<R>`
  migration; planned, implementation in a later dispatch)

---

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
