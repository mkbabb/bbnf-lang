# SK-V14 W5B-GEN Redress: Provider-Free Generator Body Rejection

Date: 2026-05-26.
Wave: W5B-GEN PRUNE-3B.
Disposition: REDRESS-211 closes as REJECTED.

## Decision

W5B-GEN cannot admit under the current SPEC shape. W5A admitted the
source-consuming request boundary, but the production runtime generator body
still depends on the provider/template mesh, and the current generic parser does
not compile the CSS L4 source constructs that W5B-GEN would need to consume.

No source redress was attempted or retained for W5B-GEN. The only edited files
for this closure are `skinny/REDRESS.md` and this evidence note.

## Challenge Convergence

The rejection route is §3Z locked:

- V2: CH1 through CH7 ACCEPT, zero orphan REVISE.
- V3: CH1 through CH7 ACCEPT, zero orphan REVISE.
- V<=5 ceiling preserved.

The locked V3 archive is
`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-GEN-challenge/V3/HARDENING-SKV14-W5B-GEN-V3-CONSOLIDATED.md`.

## Proof Bundle

Commands run from repository root unless noted.

```sh
git diff --exit-code HEAD -- \
  skinny/crates/codegen/src/lib.rs \
  skinny/crates/codegen/src/grammar_provider.rs \
  skinny/crates/grammar/src/lib.rs \
  skinny/xtask/src/regen.rs \
  skinny/xtask/src/regen_css.rs \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs
```

Result: clean. No W5B-GEN source owner path changed.

```sh
git diff --exit-code HEAD -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md
```

Result: clean. No admit ledger or rolling-delta state was altered by the
rejection close.

```sh
cd skinny && rg -n '\b(render_runtime_profile|RuntimeProvider|GrammarProfile|json_provider|css_l4_.*provider)\b' \
  crates/codegen/src/lib.rs crates/codegen/src/grammar_provider.rs
```

Result: expected failure evidence remains present. `grammar_provider.rs` still
imports and calls `render_runtime_profile`, `lib.rs` still declares the CSS
provider modules, dispatches on `RuntimeProvider`, and emits JSON through
`json_provider`.

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l | tr -d ' '
```

Result: `16` locks and `67` Pattern H files.

## Corrective Route

Pass Omega V7 is required because REDRESS-211 materially changes the SK-V14 wave
graph. The proposed amendment is:

| Wave | Scope |
|---|---|
| W5B-FRONTEND | Generic BBNF grammar-source frontend/import/IR closure; CSS L4 is the strict positive witness. |
| W5C-GEN | Provider-free runtime generator body consuming the generic request/frontend IR for every admitted grammar. |
| W5D-DELETE | Provider/template deletion and Lock 14 baseline close after W5C-GEN admits. |

W6 moves behind W5D-DELETE. W7 and W8/W9/W10 remain blocked by the PRUNE chain.
