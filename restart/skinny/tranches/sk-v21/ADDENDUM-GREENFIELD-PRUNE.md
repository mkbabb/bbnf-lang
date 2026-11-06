# SK-V21 ADDENDUM — the greenfield prune manifest and the shopwindow

Date: 2026-07-06, post-freeze. Source: research/39 (playground census), 40 (prune census),
41 (feature-surface census). This addendum dispositions every surface the frozen SPEC left
unowned; it adds NO wave reordering and reopens NO frozen decision. Items marked **TW0+** are
additions to TW0's workstream list for the implementation campaign to sequence; **TW-P** is one
net-new late wave proposed below.

## §1 — The prune manifest (consolidated)

Already SPEC-owned (coordinates of record):
| Surface | Act | Owner |
|---|---|---|
| `crates/bootstrap` husk; x86/wasm bodies in `simd-scan` (952 LOC + 92 stamped AVX2); `ser` Lock-12 archive | DELETE / ARCHIVE | TW0 W0.6 |
| `Sampled`/`Exact`/`OneShotSimd` presize family | DELETE (`GrowOnly` only) | TW1c |
| Eager-builder families (~1,080 hits, 95% regen-owned) + `TapeStructBuilder` | DELETE | TW1b |
| skinny `bbnf-regex` stub (322 LOC) + `parse-that-regex` (3,022 LOC Eisel-Lemire/scanner fork) | DELETE — one regex home, the canonical parse-that engine | TW2 |
| `decision_csp` (tautological stage) | DELETE with the CSP retirement | TW4.2 |
| Legacy `parse_that 0.4.0 → smallbox[nightly]` dep edge in core/ir/gorgeous | MOOT — dies as deps re-point at cutover | TW-S scope decision |
| playground junk (`test-results/`, `.playwright-mcp`) | gitignore | TW0 W0.10 |

Newly dispositioned (was unowned — **TW0+** unless noted):
| Surface | Disposition |
|---|---|
| `docs/tranches` (49 dirs, 743 files, 14 MB) + `docs/benchmarks` (220 files) | **ARCHIVE**: tag `pre-restart-corpus-final`, then remove from the working tree. The dig (research/01-15) extracted the lessons; git history retains the corpus. `docs/precepts` submodule stays live. |
| `skinny/REDRESS.md` (413 KB) + `skinny/RESULTS.md` (262 KB) | **PRUNE post-tag**: stale SK-V12→V20 campaign logs; `sk-v19-frozen` preserves them; the frozen oracle tree carries code + harnesses only. |
| `restart/README.md` + `HANDOFF.md` (~102 KB, superseded-adjacent) | Fold into the W0.1 supersession commit (marked SUPERSEDED alongside ARCHITECTURE.md/MIGRATION.md §3-14). |
| `crates/analysis` (5,241 LOC) | Fold into the TW1a split: analysis passes → `ir`; the diagnostics/hover/completion surface → the LSP constellation (§2). Closes the standing analysis-consolidation item. |
| `crates/egraph-derive` (343 LOC) | KEEP — rides `egraph`, TW4.2's engine. |
| `crates/lsp` (4,123 LOC) + the VM/debug constellation (`crates/ir/src/vm/` 1,772 LOC, `@debug`, `SourceMapEntry`, `DebugBreak`, `lsp/dap/adapter.rs`) | **KEEP-DORMANT, quarantined from the hot path** (it already is — a separate engine). Re-binds at TW-P as the debugger showcase. NOT pruned: live grammar debugging is novel surface no competing generator ships. |
| `gorgeous` (604 LOC, VM-interpreted mainline) | **Lock-12 revision: archive `ser`, KEEP `gorgeous` dormant.** Re-binds at TW-P; the fused parse+format pipeline is a TW3-adjacent consumer. The 87× fused claim stays unclaimed until re-measured on the fused sink. |
| `backend/ts` (1,719 LOC, live `CompileTarget::Ts` arm) | KEEP-DORMANT. Reconciliation recorded: Lock 5 defers new TS *work*, not the arm's existence — it is the Emitter-seam proof. No new TS emission work before V1. |
| `bbnf-path-ts` | DEFERRED with the WASM authority (unchanged), noting TW-P is its likely consumer. |

## §2 — TW-P, the shopwindow (net-new late wave; proposed, not frozen)

The playground (~10.2K-LOC Vue3/Vite SPA: Monaco grammar editor → live parse → AST + gorgeous +
telemetry) is bound to a workspace-excluded `wasm/` crate whose deps are ENTIRELY the legacy
tree, with a prebuilt binary from 2026-03-30 — it is disconnected from everything the campaign
ships, and no wave owns it (39). Skinny has zero wasm bindings. Disposition:

- **TW0**: junk hygiene only (W0.10, already owned) + adjudicate the uncommitted vite.config.ts
  alias cleanup (commit or revert; it is a safe redundancy removal, unverified).
- **TW-P opens after TW2** (one generator) and closes after TW3/TW6: rebuild `bbnf-wasm` against
  the surviving tree (wasm-pack, O3+simd — the old crate dies with the legacy tree otherwise),
  wire the loader, and make the playground the generator's FIRST external consumer — the
  shopwindow is itself a consumer-invariant gate.
- **The showcase set** (from 41's KEEP-AND-SHOWCASE column): live grammar → emitted-parser parse
  with per-stage timing (the SIMD classification made visible); a typed value-plane explorer
  (the actual typed tree — widths, rows, sums — replacing `JSON.stringify(result.value)`); the
  ten-grammar fleet including TOML/CSV; gorgeous formatting on the fused pipeline; and the
  debugger (DAP + VM: breakpoints in a grammar, stepping a parse). Playwright suite folds into CI.
- **bbnf-buddy does not exist in the tree** (memory-only aspiration; the live brand surface is a
  static wordmark). Commission it as TW-P brand surface or strike the memory — a TW-P decision.

## §3 — Tensions recorded for TW0 resolution

1. **The freeze/worksite tension (40)**: W0.0 makes skinny architecture-read-only, yet §5.1/§9
   cite executable TW4.1/TW6 work at skinny paths. Resolution of record: skinny coordinates in
   the SPEC are *reference*, not worksite — TW2+ work happens in `crates/` after the TW1a/TW2
   port; skinny remains differential oracle + checkasm/microbench only, per the tag.
2. **`CompileTarget::Ts` wired vs "deferred" (41)**: resolved in §1 — the arm stays, new TS work
   is deferred; the ledger label means the latter.
