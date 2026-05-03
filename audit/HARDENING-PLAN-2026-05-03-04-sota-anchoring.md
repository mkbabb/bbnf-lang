# Hardening Plan Audit 04 — SOTA Anchoring

Date: 2026-05-03
Standard: every parsing performance gate names a competitor number, dataset, and platform. Source anchors are `audit/SOTA-2026-05-03.md`.

## Ratified Performance Gates

| Gate | Site | SOTA anchor | Verdict |
|---|---|---|---|
| BA-G1 twitter | `docs/tranches/BA/BA.md:15`, `docs/tranches/BA/waves/W5.md:85-90` | simd-json twitter 424 us and sonic-rs twitter 436 us on M1 Pro at `audit/SOTA-2026-05-03.md:50-58`. | honored |
| BB-G1 bootstrap | `docs/tranches/BB/BB.md:15` | lightningcss bootstrap-4 4.16 ms at `audit/SOTA-2026-05-03.md:130-136`, but that local SOTA row does not state platform or prove parse-only surface. | surface-mismatch |
| BB-G2 tailwind | `docs/tranches/BB/BB.md:16` | lightningcss tailwind 43.37 ms at `audit/SOTA-2026-05-03.md:130-136`, but that local SOTA row does not state platform or prove parse-only surface. | surface-mismatch |
| BB-G3 citm | `docs/tranches/BB/BB.md:17` | simd-json citm_catalog 831 us and sonic-rs citm_catalog 854 us at `audit/SOTA-2026-05-03.md:50-58`. | honored |
| BB-G4 canada | `docs/tranches/BB/BB.md:18` | sonic-rs canada 3.144 ms at `audit/SOTA-2026-05-03.md:50-58`. | honored |
| BC-G1 twitter | `docs/tranches/BC/BC.md:15` | simd-json twitter 424 us and sonic-rs twitter 436 us at `audit/SOTA-2026-05-03.md:50-58`. | honored |
| BC-G2 bootstrap | `docs/tranches/BC/BC.md:16` | lightningcss bootstrap-4 4.16 ms at `audit/SOTA-2026-05-03.md:130-136`, but that local SOTA row does not state platform or prove parse-only surface. | surface-mismatch |
| BC-G3 canada | `docs/tranches/BC/BC.md:17` | sonic-rs canada 3.144 ms at `audit/SOTA-2026-05-03.md:50-58`. | honored |

## Faults

| ID | Site | Fault | Substitute |
|---|---|---|---|
| S04-1 | `docs/tranches/BA/BA.md:11`, `docs/tranches/BA/BA.md:16-24` | BA says every hard gate cites a competitor, but BA-G2/G3/G5/G6/G7/G8/G10 are allocation, toolchain, LOC, or structural gates without competitor numbers. This is false text. | Replace line 11 with: "Every parse-throughput gate cites a specific competitor + dataset + platform. Non-throughput engineering gates are separately labelled." |
| S04-2 | `docs/tranches/BA/BA.md:17`, `docs/tranches/BA/BA.md:106` | BA-G3 is an internal halving gate (59.98 s / 52.53 s), not SOTA. | Move BA-G3 to a "Toolchain gates" table. Do not claim it satisfies Lock 8. |
| S04-3 | `docs/tranches/BA/BA.md:23`, `docs/tranches/BA/waves/W4.md:41-46` | BA-G9 names the 4196x internal gap and a ratio to eager parse; it does not name a competitor number on twitter. | Add a pre-BA SOTA measurement row to `audit/SOTA-2026-05-03.md`: sonic-rs `get_unchecked(twitter, pointer![...])` on M1 Pro. Until then, gate only as `<= 5 x BA-G1 eager parse` and mark it non-SOTA. |
| S04-4 | `docs/tranches/BB/BB.md:141-142` | The per-grammar trajectory lists BBNF and Sheets without concrete external SOTA gates; Sheets cites `cssparser ~600 MB/s` against `audit/SOTA-2026-05-03.md:122`, but that line is lightningcss memory-model text, not a cssparser throughput number. | Remove these rows from the perf trajectory or amend SOTA with a concrete cssparser Sheets-like dataset on M1 Pro. |
| S04-5 | `docs/tranches/BC/BC.md:11` | BC repeats "Every gate cites..." while BC-G4..G10 are contract, crate split, API, and LOC gates at `docs/tranches/BC/BC.md:18-24`. | Replace with parse-throughput-only wording as in S04-1. |
| S04-6 | `docs/tranches/BA/waves/W4.md:60`, `docs/tranches/BA/waves/W4.md:145-146` | W4 uses "matching or improving on the pre-W4 baseline" and ratio gates. That is an internal regression gate, not SOTA. | Keep as regression evidence, but add no Lock 8 credit. If a SOTA number is required, use sonic-rs twitter 436 us from `audit/SOTA-2026-05-03.md:50-58` only for eager parse, not `get<T>`. |
| S04-7 | `docs/tranches/BB/BB.md:15-16`, `docs/tranches/BC/BC.md:16`, `audit/SOTA-2026-05-03.md:130-136` | The CSS gates say `parse(bootstrap.css)` / `parse(tailwind.css)`, but the local SOTA row only says "Throughput" and gives README numbers without platform or parse-only proof. A surface-mismatched benchmark is not a SOTA anchor. | Either change the plan gates to the exact lightningcss operation measured by the README row, or add a local M1 Pro lightningcss parse-only benchmark row to SOTA before execution. |
| S04-8 | `audit/SOTA-2026-05-03.md:214`, `audit/SOTA-2026-05-03.md:279`, `docs/tranches/BA/BA.md:16`, `docs/tranches/BA/waves/W5.md:67` | The SOTA synthesis overstates bumpalo-backed records as the default, while Lock 9 requires slice-borrow default and bumpalo only through `parse_in`. The plan copies the eager arena language into BA-G2 and W5. | Amend SOTA closing posture and BA.W5 signatures: default parse borrows slices; bumpalo appears only in `parse_in(input, &bump)`. |

## Substitute Anchor Table

| Workload | Competitor number | Source |
|---|---:|---|
| JSON twitter parse | sonic-rs 436 us, simd-json 424 us, serde_json 831 us on Apple M1 Pro | `audit/SOTA-2026-05-03.md:50-58` |
| JSON citm_catalog parse | sonic-rs 854 us, simd-json 831 us, serde_json 1.376 ms on Apple M1 Pro | `audit/SOTA-2026-05-03.md:50-58` |
| JSON canada parse | sonic-rs 3.144 ms, simd-json 3.226 ms, serde_json 4.988 ms on Apple M1 Pro | `audit/SOTA-2026-05-03.md:50-58` |
| simdjson On-Demand parse | 7 GB/s on Intel Skylake | `audit/SOTA-2026-05-03.md:83-89` |
| CSS bootstrap-4 parse | no ratified parse-only anchor yet; lightningcss README row is 4.16 ms but lacks platform/surface proof | `audit/SOTA-2026-05-03.md:130-136` |
| CSS animate parse | lightningcss 1.97 ms | `audit/SOTA-2026-05-03.md:130-136` |
| CSS tailwind parse | no ratified parse-only anchor yet; lightningcss README row is 43.37 ms but lacks platform/surface proof | `audit/SOTA-2026-05-03.md:130-136` |

## Lane Verdict

| Status | Count |
|---|---:|
| honored | 5 |
| surface-mismatch | 3 |
| violated | 8 |
| silent-must-add | 1 |

The JSON parse-throughput gates are sound after naming simd-json where it is faster. The CSS and `get<T>` gates are not ratified until their benchmark surface and platform are made explicit.
