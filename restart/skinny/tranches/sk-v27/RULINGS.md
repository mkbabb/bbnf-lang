# sk-v27 — owner rulings (2026-11-05, by the owner's delegate, value-js-e7)

The plan's §2 defaults are confirmed as written for X-1 to X-9. X-10 is set below. These rulings are binding from now.

| # | Ruling |
|---|---|
| X-1 | Confirmed. The sk-v26 spine carries the compiler; skinny is a lab whose rows are admitted by census; the root pipeline is a quarry that can only delete. |
| X-2 | Confirmed. There is one engine, `parse-that-regex`, inside parse-that, and bbnf-lang holds a thin `bbnf-regex-facts` adapter plus the emitters. Falsifier: one regex engine package in the graph, and 0 `RegexInfo` definitions outside it. |
| X-3 | Confirmed. `parse-that-simd` lives in `parse-that/rust/simd` and holds the only `core::arch` code; `parse_that::scan` is its public face. |
| X-4 | Confirmed. There is one `OnceLock` dispatch over one scalar oracle. An x86 tier merges only if CI executes it on hardware that has the ISA; x86 never certifies; O033 is retired by addendum. |
| X-5 | Confirmed (R-8). egraph, egraph-derive and csp-solver move into the parse-that workspace as standalone general crates, and the DAG runs pprint → parse-that → bbnf-lang. The `mkbabb/egraph-csp` leaf repo is not taken. |
| X-6 | Confirmed (a). All 71 commits are merged ordinarily. The dirty tree is preserved by an alternate-index commit on `sk-lineage/dirty-preserve`, nothing is discarded in this programme, and per-entry fates are ruled at LW7. |
| X-7 | Confirmed (i). The orchestrator schedules the bench window; a record waits at most 60 min for a 1-minute load below 8 and then runs labelled NOISY; each certification unit has a 72 h budget and is re-queued once. |
| X-8 | Confirmed. value.js's six WPT-gated grammars are canonical, and bbnf-lang `grammar/css/l4` is retired in favour of `grammar/corpus/value-js/`, pinned to a released tarball with checked hashes. |
| X-9 | Confirmed. The programme holds at most 1 workflow slot while megatranche-realization, x-0hr-residuals-2, daraksha-v3-repair and owner-1105-eve are open, and 2 once any of them closes. Waves run in series and lanes run as seats. |
| X-10 | **Set as follows.** Over the formatter-parity corpus (frozen at LW4.PP-4 entry), output must be AST-equivalent to Prettier/Biome on 100 % of files, with 0 semantic diffs. It must be byte-identical on ≥ 98 % of files, and every residual byte diff must be catalogued with a cited style rule and ruled KEEP or FIX. No throughput claim is made until this budget holds ×2. |

Also ruled:
- Wave LW1 runs to its LG-1 gate.
- The three UNREAD objections (regex C1-9 and C2-6, consumers critic 2 #7) are re-asked and ruled inside LW1 (LW1.RX-4 and LW1.USE).
- The leftover sk-v26 pre-commit hook (an unguarded `cargo test` of a missing test, plus a `../parse-that` path) is a defect. LW1.COMP-4 removes it, and `--no-verify` is not used after that.

## Addendum, 2026-11-06: the LG-1 rulings (by the delegate)

| # | Ruling |
|---|---|
| R-a | The private `docs/precepts` submodule is **removed** from bbnf-lang. A public library cannot carry a private submodule that every downstream cargo git edge must clone. The precepts stay in their own private repo, and bbnf-lang references them only by a plain link in its docs. X-5's crate move (LW2.TOPO-1) proceeds regardless. Falsifier: parse-that #3 and pprint #2 resolve their bbnf-lang edges on a credential-free public runner (RED today → GREEN ×2). |
| R-b | TS↔Rust parity: each of the 20 divergences is ruled row by row in LW2. Authority runs in this order: the BBNF language spec text first; where the spec is silent, the compiler of record (the sk-v26 spine, X-1) decides, and TS follows. Every ruling adds a spec sentence, so the spec stops being silent there. No divergence is closed by deleting its test. |
| R-c | The corpus artifact is a **versioned GitHub release asset with a sha256 manifest**, not LFS. LFS is ruled out because it means bandwidth quotas, lfs in every CI job, and a second custody path. It is fetched and verified by `xtask corpus fetch/verify`, locally and in CI; X-8 pins the CSS part to a released value.js tarball. |
| Z-32 | The 32 zero-caller substrates are given their fates by LW2 S2 CENSUS under X-1/X-5, each as delete, route (with a cited unit) or move with its crate. "Sanction" is not available without a named caller in LW2–LW4. |
