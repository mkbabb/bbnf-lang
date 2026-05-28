# SK-V15 T-P1 V3 Hardening Consolidated

Cycle: T-P1 Excavation V3.
Date: 2026-05-28.
Input inventories: `restart/audit/totality/p1/1A-substrate-evidence.md`
through `1F-coherence-scan.md`, plus superseded 1F auxiliary files.
Inventory fold commit: `7e32eddaa`.
Hardening root: `restart/audit/totality/p1/hardening/V3/`.

## Verdict

ACCEPT-RATE: 3 / 7 = 42.9%.

Cycle verdict: REVISE. V3 correctly folds the V2 blockers for grammar-neutral
Lock 14 proof routing, delete/rebuild regression guards, 1E cost keying, 1E
LAC alignment, Lock 1 wording, generated-provenance routing, and CH7
gate-exclusion posture. Four bounded defects remain across citation
resolvability, cost/primitive receiver granularity, FNV runtime-hash coupling,
and two paper-close wordings. No lens returned REJECT.

## Lens Dispositions

| Lens | Disposition | Output | Fold surface |
|---|---|---|---|
| CH1 CORRECTNESS | REVISE | `V3/CH1.md` | Resolve six directory-local CSS `config.rs` citations and two brace-path lowerer citations into full repo-root path:line cites. |
| CH2 GENERALITY | ACCEPT | `V3/CH2.md` | `P1-1B-D9` / `P1-1B-D10` are grammar-neutral Lock 14 failures with non-JSON proof receivers; 1F carries the owner/receiver map. |
| CH3 REGRESSION | ACCEPT | `V3/CH3.md` | `NEW-CH3-V5-01` delete/rebuild dependency rule remains load-bearing; REDRESS-183/184/209..213 remain pre-blocked. |
| CH4 COST | REVISE | `V3/CH4.md` | Split remaining 1D class-level cost buckets and enumerate primitive/kernel receivers with LOC/risk/wave/hard-cap/proof/disposition. |
| CH5 HIDDEN COUPLING | REVISE | `V3/CH5.md` | Add current-source FNV production-coupling census for CSS generated runtimes and template sites; extend sidecar/hash grep guard. |
| CH6 ANTI-PAPER-CLOSE | REVISE | `V3/CH6.md` | Downgrade 1E L05 and 1C CSS fact-stream row from broad implemented/honoured wording to partial scoped wording. |
| CH7 OVERFIT-PRUNE / GATE-EXCLUSION | ACCEPT | `V3/CH7.md` | No stale W8R, x86, PMULL/CSSC, sidecar, FNV, or header-only close route remains hidden as clean evidence. |

## Deduplicated V4 Fold Roster

| id | required fold | target files |
|---|---|---|
| T-P1-V4-F01 | Expand all remaining non-root-resolving citations. Replace six CSS runtime `config.rs` short paths with `skinny/crates/runtime/src/grammars/<profile>/config.rs:1-9`; replace brace lowerer paths with four explicit lowerer file cites. Re-run CH1's two citation greps and require zero output. | `1C-runtime-evidence.md`, `1B-codegen-evidence.md` |
| T-P1-V4-F02 | Split 1D class-level cost rows into bounded receiver rows or an adjacent carrier table keyed to CSS broadcast/value, Lock 14/16 gates, Pattern H, Decision Engine, codegen leaks, FNV quarantine, JSON c/B research, CSS typed API/re-timing, parse-that vocabulary, and primitive same-wave consumers. Each receiver needs owner path/row, LOC range, risk, wave, hard cap, same-wave consumer/proof, and route/revert disposition. | `1D-skinny-lessons.md` |
| T-P1-V4-F03 | Enumerate primitive/kernel rows from P2-B and P2-E instead of class-only rows. Include `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT` / `BULK_EMIT_POSITIONS_64`, `EOB_PAD_CLAMP`, `escape_mask_64`, UTF-8 validation, unicode escape SIMD, long-string scanner, direct cursor/whitespace, tape/allocation pressure, product-builder/hash rows, and parse-that gaps `skip_byte_set_run`, `classify_local_block_64`, `bounded_plain_literal_span`, `validate_utf8_run`, `digit_run_span_accumulate`, and `escaped_literal_segments`. For each row state LOC, risk, wave, hard cap, consumer/proof, and absent-consumer disposition. | `1D-skinny-lessons.md` |
| T-P1-V4-F04 | Add an explicit current-source FNV production-coupling row. Cite generated CSS runtime `input_fnv64` / `fnv64` surfaces and template sites, classify them as telemetry-only / UNKNOWN / real coupling, and state they are not CSS Value API proof, retained document identity, same-substrate evidence, or production equality arbiters unless a gate proves otherwise. | `1F-coherence-scan.md` or `1D-skinny-lessons.md` |
| T-P1-V4-F05 | Extend the live sidecar/hash grep guard to include `input_fnv64`, `stream_fnv64`, `fn fnv64`, and `fnv64(` before any substrate or CH5 close claim. | `1F-coherence-scan.md` |
| T-P1-V4-F06 | Downgrade 1E Lock 5 from `honoured` to scoped partial wording, e.g. `partial / Rust-only IR boundary present`; state that formal `Backend` trait and concrete per-shape lowerer depth remain open under L10, `P1-1B-D1`, and `P1-1B-D7`. | `1E-locks-evidence.md` |
| T-P1-V4-F07 | Downgrade the 1C CSS fact-stream row from `Implemented` to partial wording that separates the no-sixth-`BackendShape` enum fact from admitted-output-plane file state and unresolved `W7_POLICY_BACKEND_SHAPE` / schema evidence. | `1C-runtime-evidence.md` |

## Accepted V3 Folds To Preserve

- `T-P1-V3-F02` and `T-P1-V3-F03`: 1A and 1F frontmatter counts are mechanically auditable.
- `T-P1-V3-F04`: stale V1 self-description targeted by V2 CH1 is removed.
- `T-P1-V3-F05`: `P1-1B-D9` / `P1-1B-D10` are grammar-neutral, non-JSON Lock 14 proof receivers.
- `T-P1-V3-F06`: Lock 14 owner/receiver map exists and covers runtime roots, codegen profile roster, pass recognizer/materialization leaks, Pattern H, CSS generated-output contrivance, and gate exclusions.
- `T-P1-V3-F07`: 1E divergence cost/wave rows are keyed to their own divergence IDs.
- `T-P1-V3-F09` for 1E LACs: every 1E LAC has wave/cost/risk/hard-cap alignment.
- `T-P1-V3-F10`: Lock 1 wording is downgraded to partial JSON-tape-only.
- `T-P1-V3-F11`: `1A-SUB-001` is scoped to JSON/example implementation rather than broad runtime closure.
- CH3 and CH7 guard posture: delete/rebuild pre-blocks, CSS broadcast fences, x86/aarch64 discipline, PMULL/CSSC blocks, EventTape sidecar fences, and generated-header-only rejection remain load-bearing.

## Non-Findings

- No lens returned REJECT.
- CH2, CH3, and CH7 have no V4 fold requirements.
- V3 does not reopen JSON guard validation, CSS audit demotion, Pattern H count, five-shape `BackendShape` canon, or the G-Omega-only mandatory relinquish discipline.

## Next Dispatch

Fold the V4 roster into the live T-P1 inventories, then run a fresh CH1-CH7
V4 hardening cycle. T-P1 can lock only after two consecutive >=95% hardening
cycles with zero orphan REVISEs; because V3 is REVISE, V4 can at most become
the first clean cycle.
