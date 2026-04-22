# AY-II — Path Forward (2026-04-22)

AY-II is not on a parallel-infra path any more. The immediate execution
order is:

1. **B1 closes first** as the bounded prelude annex over the dev-loop,
   bootstrap, expand, bench, and profiling surfaces.
2. **AY-II.W0' closes next** on the refreshed surface: regen,
   alias-shim retirement, fresh expand, fat-LTO bench matrix, samply,
   and nm.
3. **AY-II W1-W5 execute sequentially** on that post-B1, post-W0'
   truth.

Anything else reintroduces the same ambiguity B1 exists to delete.

## Current truth

- Master HEAD is `e777a68d`.
- W0'.a / W0'.b / W0'.c / W0'.d1 / W0'.d3 / W0'.d4-d7 are landed in
  source.
- `generated.rs` is still pre-regen with the bridge-era parse entry, so
  AY-II.W0' is **not** formally closed yet.
- The W0'.a compose-boundary aliases and shim surfaces are still present
  by design until the post-B1 regen replaces them.
- Routine warm iteration is usable today; B1 owns the renewed proof of
  the full command/bootstrap/expand/profile/bench surface.

## Ordered work

### 1. B1 prelude annex (blocks every further AY-II runtime step)

Execute B1 per:

- [B1.md](/Users/mkbabb/Programming/bbnf-lang/docs/tranches/B1/B1.md)
- [AGENT_DISPATCH.md](/Users/mkbabb/Programming/bbnf-lang/docs/tranches/B1/AGENT_DISPATCH.md)
- [W0.md](/Users/mkbabb/Programming/bbnf-lang/docs/tranches/B1/waves/W0.md)
- [W1.md](/Users/mkbabb/Programming/bbnf-lang/docs/tranches/B1/waves/W1.md)

Close conditions B1 must establish before AY-II resumes:

1. The routine command surface is truthful and documented as-built.
2. `bootstrap-bbnf`, `ay-prepare-profile-wave`, `ay-samply-*`, and
   `ay-bench-close` are all proven on current repo state.
3. The stale workflow comments/docs inherited from B0 + AY-II infra
   churn are deleted.
4. AY-II docs are normalized so B1 is predecessor, not sidecar.

### 2. AY-II.W0' close ceremony

After B1 closes, finish W0' in one uninterrupted sequence:

1. Run bootstrap regen.
2. Run double-regen idempotency.
3. Retire the W0'.a compose-boundary aliases and shim surfaces.
4. Capture fresh expands for JSON, CSS, Sheets, and BBNF.
5. Run the fat-LTO 5-bench matrix.
6. Capture samply on the four primary grammars.
7. Run `nm` on the bench binaries.
8. Update `PROGRESS.md`, `AY-II.md`, and `waves/W0p.md` to mark W0'
   closed.

The W0' close gate remains the one declared in
[W0p.md](/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AY-II/waves/W0p.md).

### 3. AY-II W1-W5

Dispatch only after W0' closes:

1. W1 — JSON semantic parity + peer-referenced performance.
2. W2 — CSS L4 typed-semantic parity.
3. W3 — Sheets typed semantics + performance.
4. W4 — BBNF self-hosting identity + grammar-meta typed semantics.
5. W5 — close matrix + FINAL + successor handoff.

No annex, no sidecar wave, and no infra detour runs in parallel with
these waves.

## Immediate cleanup targets already identified

These are not optional polish items; they are W0' close work:

- `crates/tape/src/builder/mod.rs` — retire `pub type TapeBuilder = FusedBuilder;`
- `crates/core/src/runtime/mod.rs` — retire `ValueBuilderOutput` alias
  and the `value_builder` shim module
- `crates/core/src/runtime/parsed.rs` — retire the 4-arg `new_fused`
  bridge once regen no longer emits it
- `crates/core/tests/value_api_apples_to_apples.rs` — move the builder
  counter imports onto the fused builder surface

## Discipline

- One codepath.
- No parallel B1/AY-II execution.
- No pre-regen assumptions treated as runtime truth.
- No stale docs left active once superseded.
- No quick solutions, no workarounds.
