# Alpha-B — Competitor Deltas — SK-V15 V1

Pass: Pass Alpha. Cycle: SK-V14 -> SK-V15.
Date: 2026-05-27.
Scope: strict comparator extraction and demotion rules.
Output: this file.

## JSON Strict State

JSON carries forward as strict SOTA on the M5 Max native bracket. The
rolling ledger uses `sonic-rs strict + 1 Mbps` as the JSON threshold
(`restart/skinny/ROLLING-SOTA-DELTA.md:97`). All 51 JSON rows are
positive in the SK-V14 close ledger; the tightest margins are:

| Row | Margin |
|---|---:|
| `random/parse_only` | +155.460 Mbps |
| `unicode_escapes/real_typed_struct` | +206.770 Mbps |
| `unicode_escapes/direct_to_struct` | +504.006 Mbps |

The broadest JSON margin is `citm_catalog/direct_to_struct`
+12115.480 Mbps (`restart/skinny/ROLLING-SOTA-DELTA.md:18`).

SK-V15 keeps these as guard rows, not new work. A wave that changes JSON
must re-run the strict row families it touches and preserve strict product
planes (`skinny/RESULTS.md:147`).

## CSS Comparator Demotion

CSS L4 rows are demoted from admission evidence. PASS-IMPL V1 makes two
findings that bind this bracket:

| Finding | Consequence |
|---|---|
| one CSS aggregate timing tuple is broadcast across 24 row ids | no 24-row admit without 24 independent measurements or one explicitly aggregate row |
| Track 1 `full_parse` is a four-counter summary while lightningcss builds CSSOM | lightningcss is diagnostic until Track 1 emits comparable CSSOM/value output |
| cssparser is faster than Track 1 in the same aggregate row | cssparser is the near-term same-workload comparator |

Evidence: `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`
and `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:29`.

## SK-V15 Comparator Binding

| Domain | Admission comparator | Flaw-probe only |
|---|---|---|
| JSON parse_only | `sonic-rs::Skipper` strict + serde_json strict baseline | sonic lossy, RapidJSON permissive, absent C++ sidecars |
| JSON direct_to_struct | strict product plane vs sonic/serde strict struct products | digest-plane comparison |
| JSON real_typed_struct | typed direct plane vs sonic/serde typed strict products | closed-enum sidecar-only equality |
| CSS L4 | cssparser same-workload typed value/CSSOM-equivalent plane | lightningcss until CSSOM parity; brace-counter summary |

Per the Apple M5 Max / aarch64 pin, x86-only AVX-512 comparator rows are
not admission anchors in SK-V15 and must not consume implementation effort.
Deep SIMD work targets the native aarch64 SIMD surface and is admitted only
with cold per-parse evidence from that host.
