# SK-V12 Pass Alpha - alpha-B Competitor Deltas

Pass: Alpha re-bracket SK-V11 -> SK-V12.
Agent: alpha-B.
Date: 2026-05-20.
Scope: competitor-delta extraction under
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.

## Contract And Source Boundary

PASS-ALPHA assigns alpha-B to compute Track 1 deltas against every runnable
comparator, with comparator strictness and output plane disclosed. This
re-bracket is under the USER PIN, so the earlier SK-V12 Sheets-first alpha
framing is obsolete.

Sources read for this lane:

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120
- CSS/lightningcss local references:
  `crates/core/benches/css/competitors.rs`,
  `crates/core/benches/css/l4.rs`,
  `crates/core/tests/lightningcss_parity.rs`,
  `audit/SOTA-2026-05-03.md`,
  `docs/benchmarks/post-H.json`,
  `docs/benchmarks/post-AW-IV-W5.json`,
  `Cargo.lock`, and `package-lock.json`.

## User-Pin Rebinding

The SK-V12 close comparator is now CSS L4 versus lightningcss, not JSON versus
sonic-rs. The binding admission floor is:

```text
css_l4_track1_mbps > lightningcss_mbps + 1
```

The rescinded formula is:

```text
ceil(css_l4_baseline_mbps * 1.01)
```

That older baseline-plus-one-percent expression may appear in pre-pin
SYNTHESIS/SPEC/alpha text, but it is no longer a close bar. It can be used only
as historical context.

JSON comparator rows remain guard and freshness facts. They do not define the
campaign close bar unless a JSON guard demotion must be measured and routed.
`parse_only` remains diagnostic-only.

Rounding rule: `lightningcss_mbps` and `css_l4_track1_mbps` are the integer
Mbps values emitted by the same-run gate/report row. The comparator passes only
when the generated integer Track 1 Mbps is strictly greater than
`lightningcss_mbps + 1`; equality at `+1` is a miss, not slack.

## CSS L4 Comparator Authority

Current skinny close authority has no admitted CSS L4 row:

- REDRESS 111 admits a non-JSON report/gate lane only, not generated Track 1
  CSS runtime evidence (`skinny/REDRESS.md:3282`).
- REDRESS 112 rejects the generated CSS L4 baseline because the skinny codegen
  and runtime path remained JSON-profiled and no generated CSS L4 runtime
  existed (`skinny/REDRESS.md:3311`).
- REDRESS 113 blocks the CSS L4 intervention because no baseline row existed
  (`skinny/REDRESS.md:3340`).
- REDRESS 120 closes SK-V11 as a measured fixpoint with no non-JSON generated
  admission (`skinny/REDRESS.md:3529`).

The local repository does contain the comparator machinery SK-V12 must bind:

| Comparator source | Plane/status | alpha-B treatment |
|---|---|---|
| `crates/core/benches/css/competitors.rs:153` / `:166` / `:179` | lightningcss full semantic CSS parse over `normalize.css`, `bootstrap.css`, and `tailwind.css` | Runnable comparator template. W1 must either move this into the skinny gate/report lane or cite a same-host capture derived from it. |
| `crates/core/benches/css/l4.rs:167`-`:169` | bbnf CSS L4 typed-slab parse over the same three CSS fixtures | Historical/root-workspace bbnf comparator template, not current skinny admission evidence. |
| `crates/core/tests/lightningcss_parity.rs:96`, `:111`, `:127` | EOF/admission parity for normalize/bootstrap/tailwind, plus color parity in the same file | Strict-equality reference shape. W1 must strengthen this into gate-consumed equality for the selected CSS L4 row. |
| `Cargo.lock:1908`-`:1909` | Rust `lightningcss` crate version `1.0.0-alpha.71` | The Rust comparator version for same-host benchmarking unless S-P3 pins a newer crate in the skinny workspace. |
| `package-lock.json:868`, `:2153` | Node `lightningcss` package `1.31.1` present in the repo tree | Reference only for non-Rust tooling; not the default SK-V12 strict comparator unless the wave explicitly selects the Node plane. |

The selected SK-V12 CSS row must record these fields in the companion report or
`RESULTS.md` before alpha/G-Alpha can claim a delta:

| Field | Required value |
|---|---|
| `row_id` | `css_l4/declaration_values/direct_to_struct/main` or `css_l4/declaration_values/real_typed_struct/main`, as selected by W1 |
| `comparator` | `lightningcss` |
| `comparator_version` | exact Rust crate/package version and lockfile source |
| `output_plane` | same as selected bbnf row; do not compare direct digest against a different AST/DOM plane |
| `strictness` | strict semantic equality, fail-closed on parser or oracle mismatch |
| `css_l4_track1_mbps` | generated Track 1 Mbps from same-host run |
| `lightningcss_mbps` | same-host lightningcss Mbps on same corpus and output plane |
| `admission_floor_mbps` | `lightningcss_mbps + 1` |
| `delta_vs_lightningcss_mbps` | `css_l4_track1_mbps - lightningcss_mbps` |
| `delta_vs_lightningcss_pct` | `(css_l4_track1_mbps / lightningcss_mbps - 1) * 100` |
| `fixture_provenance` | path plus content hash for the CSS corpus |
| `equality_artifact` | strict oracle/equality proof consumed by the gate |

Until those fields exist, the CSS L4 competitor delta is `UNMEASURED`, not
zero and not a pass. The W1 gate may use the W0 companion schema seed
`sk-v12-nonjson-generated-v1`, but the W0 fixture is only a schema smoke row;
it is not measured CSS L4 performance authority.

The output plane is one canonical CSS L4 declaration-value fact stream for all
three parties: generated Track 1, independent Track 2/oracle, and lightningcss.
The symmetric equality adapter must derive the same declaration-value facts
from both generated bbnf and lightningcss parses; it may not be a bbnf-only
bridge or a lightningcss-only normalization path. If S-P3 selects a full
stylesheet row instead, the row id, fixture, oracle, and lightningcss
comparator must all move to the full-stylesheet plane together.

## CSS Reference Deltas: Historical Only

The repo's older CSS measurements are useful for route selection but cannot
close the pin because they are not skinny SK-V12 same-host gate rows:

| Reference | bbnf CSS L4 surface | lightningcss surface | alpha-B disposition |
|---|---|---|---|
| `audit/SOTA-2026-05-03.md:130`-`:136` | none; external SOTA survey | lightningcss README timings for Bootstrap / animate.css / tailwind.css | Historical competitor profile only. Not same-host, not same output-plane proof. |
| `docs/benchmarks/post-H.json:19`-`:22` | root-workspace bbnf `css_l4` Mbps for bootstrap/normalize/tailwind | no same-row lightningcss Mbps | Historical bbnf trend only. |
| `docs/benchmarks/post-AW-IV-W5.json:31`-`:34` | root-workspace bbnf `css_l4` Mbps for normalize/bootstrap/tailwind | parity harness recorded, but no same-row lightningcss Mbps in the JSON | Historical regression/freshness signal only. |

These references justify targeting CSS first, but the pin's close bar is
created only by a fresh same-host `lightningcss_mbps + 1` row.

## JSON Comparator Availability By Plane

The current JSON rows remain important guards. They are not the CSS close
target.

| Plane | Same-run native strict comparators | Historical/absent sidecars | SK-V12 role under pin |
|---|---|---|---|
| `parse_only` / borrowed view over offset tape vs DOM | sonic-rs strict DOM and serde_json DOM | historical simdjson/yyjson/RapidJSON on some rows; simdjson On Demand and asmjson absent | Diagnostic only. No SOTA admission. |
| `direct_to_struct` / digest | sonic-rs strict direct and serde_json direct | simdjson DOM/On Demand, yyjson, asmjson, RapidJSON absent for direct | Guard/freshness facts; residual rows exhausted by REDRESS 119 unless a future wave names fresh material evidence. |
| `real_typed_struct` / typed direct | sonic-rs strict typed and serde_json typed on the seven typed rows | C++ sidecars absent for typed rows | Product-plane guard/freshness facts. |

## JSON Direct Residual Guard Table

Formula: direct residual floor is `ceil(sonic-rs strict direct Mbps / 1.10)`,
matching REDRESS 119. Delta columns are `(Track 1 / comparator - 1) * 100`.

| Row | Track 1 | Track 2 | sonic strict direct | serde_json direct | Delta vs sonic | Delta vs serde | Floor | Guard disposition |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 10286 | -23.2% | +12.9% | 13740 | NO-GO, exhausted by REDRESS 119 |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 6967 | -11.8% | +48.1% | 10637 | NO-GO, numeric sibling route rejected |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 12505 | -19.2% | -4.7% | 13403 | NO-GO, exhausted by REDRESS 119 |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 8056 | -26.0% | +1.6% | 10059 | NO-GO, exhausted by REDRESS 119 |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 7037 | -10.3% | +21.7% | 8675 | NO-GO, W3 measured-rejected |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 6280 | -11.2% | +22.5% | 7878 | NO-GO, W4 probe rejected |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3364 | -35.2% | -20.8% | 3737 | NO-GO, no W8 source candidate |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 9218 | +17.3% | +25.5% | 8969 | Numerically above floor but W0-clamped; no docs-only admission |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 1782 | +67.9% | +151.3% | 2425 | Track 2 floor miss; W0-clamped |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 1392 | +31.9% | +169.6% | 2588 | Track 2 floor miss; W0-clamped |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 2362 | -64.5% | -43.1% | 3441 | NO-GO, large unicode escape miss |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 3355 | -40.1% | -47.8% | 2658 | NO-GO, string/digest miss |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3527 | -54.4% | -43.8% | 3950 | NO-GO, unicode/string miss |

## JSON Direct A/GO Guards To Hold

These are digest-plane guards only. They do not define CSS close.

| Corpus | Track 1 | Track 2 | sonic strict direct | serde_json direct | Delta vs sonic | Delta vs serde | Guard note |
|---|---:|---:|---:|---:|---:|---:|---|
| `citm_catalog` | 18563 | 17787 | 15530 | 9540 | +19.5% | +94.6% | inherited direct guard row |
| `apache_builds` | 11254 | 10189 | 10995 | 9723 | +2.4% | +15.7% | strict measured-row direct guard |
| `marine_ik` | 8938 | 9437 | 8473 | 6896 | +5.5% | +29.6% | inherited direct guard row |
| `unicode_basic` | 2299 | 2227 | 2353 | 1592 | -2.3% | +44.4% | GO by 1.10x time slack, not a throughput win |

## JSON Typed Product Guards To Hold

Typed rows compare against sonic-rs strict typed and serde_json typed, not
direct or parse-only comparators.

| Corpus | Track 1 typed | Track 2/oracle | sonic strict typed | serde_json typed | Delta vs sonic | Delta vs serde | Guard note |
|---|---:|---:|---:|---:|---:|---:|---|
| `twitter` | 17740 | 15912 | 15010 | 15664 | +18.2% | +13.3% | typed product win |
| `citm_catalog` | 30539 | 17675 | 20726 | 18295 | +47.3% | +66.9% | largest current typed delta |
| `apache_builds` | 8478 | 6892 | 8106 | 6807 | +4.6% | +24.5% | typed product win |
| `github_events` | 11871 | 12275 | 12224 | 12249 | -2.9% | -3.1% | GO by 1.10x time slack |
| `update_center` | 11851 | 10358 | 12467 | 10143 | -4.9% | +16.8% | GO by 1.10x time slack |
| `mesh` | 9403 | 7897 | 8923 | 7562 | +5.4% | +24.3% | typed product win |
| `marine_ik` | 11788 | 10096 | 9010 | 10036 | +30.8% | +17.5% | typed product win |

## alpha-B Carry-Forward

1. SK-V12 must create a fresh CSS L4 comparator row before any close claim:
   `css_l4_track1_mbps`, `lightningcss_mbps`, strict equality, same output
   plane, and provenance must be gate-consumed.
2. The only valid CSS admission floor is strict
   `css_l4_track1_mbps > lightningcss_mbps + 1`.
3. JSON sonic/serde deltas remain guard facts. A JSON guard regression must be
   measured and routed, but JSON wins cannot substitute for the CSS L4 pin.
4. Historical CSS docs and root-workspace benches guide candidate selection but
   are not same-host SK-V12 close evidence.
5. Absent comparator rows are not wins. Missing lightningcss, simdjson On
   Demand, yyjson, asmjson, or RapidJSON data must be represented as absent or
   unmeasured, never as a positive delta.
