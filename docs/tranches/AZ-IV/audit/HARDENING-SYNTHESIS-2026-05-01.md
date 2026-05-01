# AZ-IV Hardening Synthesis - 2026-05-01

Six read-only agents hardened the AZ-IV planning claims before doc creation. No files were edited by the agents.

## Agents

| Agent | Angle | Disposition |
|---|---|---|
| Pauli | precepts, orchestration, committing | accepted with plan-language corrections |
| Meitner | source archaeology | accepted with DTA/color/PatternAnnotations narrowed |
| Wegener | grammar generality and parity | accepted; live checks made parity/regen status stricter |
| Mencius | optimization substrate wiring | accepted with CSP/SIMD/Pratt caveats |
| Locke | sibling/workspace topology | accepted |
| Socrates | wave-order challenge | accepted; AZ-IV collapsed to W0-W3 |

## Accepted Process Claims

1. Current canon is six agents max per wave. Older ten-agent language is historical.
2. Empty/no-evidence return handling is ambiguous in precepts. AZ-IV resolves it locally: one verbatim redispatch, then mandatory triumvirate on the second empty/no-evidence return.
3. Read-only agents do not commit at hard cap. Write-authorized agents commit only with a clean owned slice.
4. Parallel writers use sibling worktrees and unique `CARGO_TARGET_DIR`; no parallel writer uses main.
5. Commit plans must include the local `commit-discipline` staging protocol, not only subject/body guidance.
6. Profiling waves may share a prepared target only after preparation; no concurrent cargo against one target dir.

## Accepted Source Claims

1. `emit_dfa_inline_body` is internally orphaned. The live Rust path calls `emit_regex_scan_adapter`.
2. Rust backend still pays generic per-rule compile work that Rust discards.
3. `substrate_path` can fall back to `::bbnf::runtime::JsonStructBuilder` on malformed backend paths.
4. DTA is not dead. It remains a regex/operator/precedence fact source, but walker/tape/runtime-table wording is stale.
5. `backend/rust/view/color` is shim-like and under-consumed, not cleanly dead. Current CSS uses `runtime::css_l4::CssColor`.
6. `runtime/view.rs` is live; only the color compatibility re-export is suspect.
7. `PatternAnnotations` is legacy and under-consumed but still read by Pratt detection.
8. `crates/bootstrap` is still a workspace member with dev bins; its metadata must be updated if its role is now compatibility/diagnostic.

## Accepted Grammar And Parity Claims

1. No AZ-IV doc tree existed before this tranche doc creation.
2. Live strict regen is red on 7/9 grammars: `bbnf`, `json`, `css_l4`, `css_pretty`, `google_sheets`, `ebnf`, `bnf`.
3. Parser strategy binding is still a literal parser-name allowlist and unknown grammars panic.
4. JSON and BBNF are the strongest parity surfaces.
5. CSS remains partial because named-color payloads and Tailwind behavior are not closed.
6. Sheets evidence is stale: prior docs say 122/133, but a hardening live rerun saw 115/133.
7. TS is red and under-gated: structural string tests do not prove executable parser parity.
8. Shape detector claims generality, but emitter tests and runtime proof are still JSON-biased.

## Accepted Optimization Claims

1. CSP strategy is real and cost-aware.
2. Shape/layout/dispatch have consumers, but CSP is not yet the sole authority; sidecars still mediate alt/wrap paths.
3. Regex engine decisions are consulted, but not always emitted as the exact chosen engine path.
4. Egraph can represent/write back `Map`, but extraction can choose the cheaper inner node if semantics are not pinned.
5. Ruler and RuleSet are live substrate, but production egraph/codegen still does not consume loaded rules end to end.
6. Generic shape dictionary runtime consumption is not substantiated.
7. SIMD/structural scan is wired but gated and narrower than older comments imply.
8. Pratt/view consumers are real, but some formatter/projection paths remain grammar-specific.
9. Benchmark wiring exists, but `compile_css_l4` is not currently measured by the active `compile_pipeline` bench.

## Accepted Sibling Claims

1. WASM still carries `bbnf_derive`/deleted derive residue in lock/config surfaces.
2. parse-that bootstrap still depends on deprecated `bbnf_derive`.
3. `csp-solver` source of truth is split between bbnf-lang and a csc411 path.
4. NPM locks are stale in bbnf-lang and parse-that TypeScript.
5. Docs sync is not a gate and external docs have drift.
6. Manifest metadata validation warns where AZ-IV should fail.

## Narrowed Claims

| Original claim | Hardened wording |
|---|---|
| DTA is dead | DTA runtime/walker claims are stale; current fact extraction must be renamed, split, or consumed accurately |
| color view is dead | old `Color` is compatibility/test-used; runtime `CssColor` is current |
| CSP is unconsumed | CSP is consumed, but not authoritative everywhere |
| SIMD is dead | SIMD is wired, but narrow/gated and not the broad DTA-wide mechanism docs imply |
| PatternAnnotations can delete immediately | migrate Pratt and any remaining consumers first |

## Plan Consequence

Socrates' challenge is accepted: AZ-IV is four waves, not BA then BB and not six broad waves. The plan starts with truth and regen because semantic parity, rewrite activation, and performance evidence cannot be trusted on a drifting generated tree.
