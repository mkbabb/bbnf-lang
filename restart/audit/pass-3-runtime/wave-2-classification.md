# Wave 2 PASS-3 amendment classification

Pre-fill verifications run against `restart/audit/pass-3-runtime/PASS-3.md` post-baseline (commit `0cd8ef98`). Each item is classified DONE / PARTIAL / ABSENT.

| Item | Source | rg verification | State | Surgery scope |
|---|---|---|---|---|
| 5 | HARDENING-CONSOLIDATED §4.5 | `rg -nC2 'consumer acceptance\|API wrappers\|materialisation cost' restart/audit/pass-3-runtime/PASS-3.md` returns zero | ABSENT | Add explicit consumer acceptance gates binding PASS-3 surfaces to PASS-2 emission contract. |
| 9 | HARDENING-CONSOLIDATED §4.9 | `@error(recover = ...)` and the legacy-alias note appear at lines 35 and 139 | DONE | Acceptance gate met by Wave-2 baseline; no further amendment. |
| 12 | HARDENING-CONSOLIDATED §4.12 | `rg -nC2 'fixtures/yaml\|four-fixture-dir' restart/audit/pass-3-runtime/PASS-3.md` returns zero | ABSENT | Reframe the §6 `test-fixtures` block: declare onboarding requires zero entries here; fixtures are post-onboarding parity surface only. |
| 13 | HARDENING-CONSOLIDATED §4.13 | No per-grammar table exists; line 300 mentions BBNF corpus only as a benchmark dataset | ABSENT | Add a 10-row feeder table with typed-root / `ValueRef` / runtime files / visitor / path schema / fixture manifest / host route columns for the SYNTHESIS 10x9 table. |
| 17 | HARDENING-CONSOLIDATED §4.17 | All proposed crate names use `path-core`, `path`, `path-ts`, `test-fixtures`; remaining `bbnf-path*` text is explicit deletion archaeology at line 84 | DONE | Acceptance gate met. |
| 18 | HARDENING-CONSOLIDATED §4.18 | `path!` text absent; `pointer!` is the only authored macro name | DONE | Acceptance gate met. |
| 19 | HARDENING-CONSOLIDATED §4.19 | Current tree shows `grammar/`, `parse/`, `document/`, `value/`, `tape/`, `visitor/`, `diagnostics/`, `host/` (8 children) | PARTIAL | Reconcile to the hardening canonical 8-children list: `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/` plus `value/` and `tape/`. Move `grammar/` into `metadata/` and `host/` into `metadata/` to keep query/metadata cohesion. |
| 26 | HARDENING-CONSOLIDATED §4.26 | Generated-surface budget table at lines 318-325 covers visitor / path / tape / diagnostics; tape-identity delta, bench-report budget, and regen wall budget absent | PARTIAL | Extend the budget table with rows for tape identity field/method delta, bench-report generation, and `cargo xtask regen --check` wall budget. |
| 33 | HARDENING-CONSOLIDATED §4.33 | `bbnf/self_host/internal` row at line 314 reads "progress-only until final close" without the < 100 ms target or no-peer-claim framing | PARTIAL | Replace target column with "< 100 ms self-parse + format roundtrip; explicit non-Lock-8 internal gate; no SOTA peer claim attaches." |
| 34 | HARDENING-CONSOLIDATED §4.34 | `BBNF-LIFE`, `BBNF-LAYOUT`, `BBNF-OPT`, `BBNF-GRAMMAR`, `BBNF-POINTER`, `HostSignature` codes all absent | ABSENT | Add a §6.5 (or §11) compiler diagnostic ledger with verbatim strings for lookbehind width, host signature mismatch, layout conflict, chain step, Pratt/SIMD non-application, pointer segments, lifetime escape, arena mismatch, yaml metadata, host-chain WASM, and lowerer Grammar IR import. |
| 36 | HARDENING-CONSOLIDATED §4.36 | Fallback-rate text appears in §5 prose and bench row 315 but no dataset-level threshold, snapshot-reuse gate, or LSP user-facing output policy | PARTIAL | Extend §5 with dataset thresholds (json bench, css bench, large-edit corpus), a snapshot-reuse percentage gate, and explicit LSP user-facing silence policy. |
| 37 | HARDENING-CONSOLIDATED §4.37 | §8 hand-off table at line 329 carries `Contract | Receiver | Blocker | Gate`; §10 unresolved punch-list at lines 372-378 is a flat numbered list with no triple | PARTIAL | Rename §8 column "Gate" to "Receiving gate" for explicit conformance; restructure §10 into a Receiver / Blocker / Receiving-gate table. |
| 47 | HARDENING-CONSOLIDATED §4.47 | "Replace hardcoded fixtures with metadata" at line 33 and line 91; no explicit deletion close gate; no `rg` command bound | ABSENT | Add §3 close gate: `rg -n "GRAMMAR_PATH_REGISTRY\|grammar.*marker.*registry" returns zero outside generated data; metadata route is the only validation surface.` |

## Surgery routing

Wave-2 amendment commit will land:

1. §3 — registry deletion gate (item 47).
2. §3 — consumer acceptance gates (item 5; cross-reference via §8 hand-off).
3. §6 — bbnf tree reconciliation to canonical 8-children (item 19).
4. §6 — fixture separation language (item 12).
5. §6.5 (new) — per-grammar feeder table (item 13).
6. §6.5 (new) — diagnostic ledger (item 34).
7. §7 — generated-surface budget extensions (item 26).
8. §7 — self-host gate target + framing (item 33).
9. §5 — incremental fallback dataset thresholds + LSP policy (item 36).
10. §8 — column rename to "Receiving gate" (item 37).
11. §10 — restructure unresolved punch-list to triple-column table (item 37).

No structural reordering of section numbering. New subsections inserted in-place to preserve cross-references.
