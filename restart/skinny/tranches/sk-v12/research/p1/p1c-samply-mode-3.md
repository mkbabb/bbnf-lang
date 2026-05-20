# SK-V12 P1-C: Mode III / CSS Target Boundary

Pass: S-P1 Profile. Cycle: V12 pin-aware rerun.
Date: 2026-05-20.
Scope: Mode III masking-probe and structural-scan boundary under the CSS L4
user pin; no implementation or profile capture ownership.
Output: this file.
Baseline: pin-aware S-P1 at documentation head `cf7848b2`; W0 JSON behavior
lock remains the SK-V12-open surface (`f788eb97` revalidation path,
`50bd1648` profile seed) unless W0 revalidation proves drift.
Host triple: `aarch64-apple-darwin`; prior captures identify Apple M5 Max.
Build flags: parent capture root uses native release binaries under
`/tmp/skv12-pin-profile-target-cf7848b2`.
Profile tool: artifact inspection only. I did not run cargo, xctrace, samply,
Criterion, or any bench binary.
Corpus coverage: fresh Mode III under `/tmp/skv12-pin-p1` is 0/17 JSON and
0/1 CSS target; W0 Criterion-only Mode III evidence remains 17/17 JSON probes
and 17/17 JSON structural-scan diagnostics.

## Section 1 - Method

P1-C is a boundary artifact in this pin rerun. The parent orchestrator owns the
fresh capture process under `/tmp/skv12-pin-p1`; this file only inspects what
is already on disk and states what is absent.

Commands used for inspection:

```sh
find /tmp/skv12-pin-p1 -maxdepth 3 -type f | sort
find /tmp/skv12-pin-p1 -maxdepth 2 -type d | sort
awk '{ if ($0 ~ /profile_direct/ && $0 ~ /real_typed/) typed++; else if ($0 ~ /profile_direct/) direct++; else if ($0 ~ /xctrace_probe/) parse++; else other++ } END { printf "direct=%d\nparse=%d\ntyped=%d\nother=%d\n", direct, parse, typed, other }' /tmp/skv12-pin-p1/pmu/pmu-commands.sh
rg -n "json_probes|simd_structural|host_call_eager|alternate_scalar|cold_first|css|CSS|nonjson|sheets" /tmp/skv12-pin-p1/pmu/pmu-commands.sh
find /tmp/skv11-open-criterion-3ce75df -path '*/json_probes_*/*/new/estimates.json' | wc -l
find /tmp/skv11-open-criterion-3ce75df -path '*/simd_structural_scan/*/new/estimates.json' | wc -l
find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort
rg -n "ensure_runtime_profile|grammar_name|emit_from_source|emit_typed_from_source" skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs
```

The parent PMU/samply/xctrace capture has completed. P1-C still records Mode
III as absent because the final pin root contains parse, direct, and typed JSON
lanes only; it does not contain fresh `host_call_eager_decode`,
`alternate_scalar_plan`, `cold_first_parse`, structural-scan-only, or CSS L4
Mode III call stacks.

## Section 2 - Fresh `/tmp/skv12-pin-p1` Evidence

There is no fresh Mode III evidence under `/tmp/skv12-pin-p1`.

| Path or lane | Observed state | P1-C disposition |
|---|---|---|
| `/tmp/skv12-pin-p1/pmu/pmu-commands.sh` | 82 PMU commands: 34 direct, 34 parse, 14 typed, 0 other | Fresh PMU only; not Mode III. |
| `/tmp/skv12-pin-p1/pmu/capture_status.tsv` | 82 PMU data rows, all PASS | PMU authority for P1-D; not a probe profile. |
| `/tmp/skv12-pin-p1/logs/pmu-*` | stdout/stderr from JSON PMU binaries | Useful for P1-D/P1-E only. |
| `/tmp/skv12-pin-p1/samply` | parse/direct/typed JSON captures complete | No fresh samply Mode III call stacks. |
| `/tmp/skv12-pin-p1/probes` or `json_probes_*` | absent | No fresh `host_call_eager_decode`, `alternate_scalar_plan`, or `cold_first_parse` capture. |
| `/tmp/skv12-pin-p1/simd_structural_scan` | absent | No fresh structural-scan Criterion capture. |
| `/tmp/skv12-pin-p1/*css*` | absent | No CSS L4 generated parser profile or comparator capture. |

The command manifest contains no `json_probes`, `simd_structural`,
`host_call_eager`, `alternate_scalar`, `cold_first`, `css`, `nonjson`, or
`sheets` command. Therefore no S-P2/S-P3 plan may cite `/tmp/skv12-pin-p1` as
fresh Mode III or CSS hot-leaf authority.

## Section 3 - W0 Criterion-Only Evidence

The only Mode III data available to P1-C remains the W0 Criterion diagnostic
surface under `/tmp/skv11-open-criterion-3ce75df`:

| Diagnostic family | Artifact count | Coverage | Status |
|---|---:|---|---|
| `json_probes_*/*/new/estimates.json` | 68 | 17 JSON corpora times four probe directories | W0 Criterion-only. |
| `simd_structural_scan/*/new/estimates.json` | 34 | 17 JSON corpora times SIMD/scalar scanner variants | W0 Criterion-only. |

Those W0 diagnostics retain their prior meaning:

- `host_call_eager_decode` is a masking probe, not an eager-decode
  implementation route.
- `alternate_scalar_plan` is diagnostic only; it does not admit a scalar plan
  and does not define a product-plane comparator.
- `cold_first_parse` is a cold-start signal only.
- `simd_structural_scan` is a raw scanner diagnostic and a report nonproducer.

Under the user pin, union-substrate and ASM-gen categories are unblocked at
the category level. That does not convert W0 diagnostics into wave authority.
A new union or ASM-gen candidate still needs a material differential, fresh
profile antecedent, micro-prove-first evidence, scalar/parity or checkasm
coverage, same-wave consumer, and CHALLENGE acceptance.

## Section 4 - CSS L4 Target Treatment

CSS L4 is authoritative for SK-V12 after the user pin. Sheets and BBNF-self are
fallbacks only after a CSS L4 redress attempt fails. The Sheets execution scout
is therefore useful only as a fallback pattern; it is not equivalent to CSS and
does not satisfy the pinned S-P1 CSS target.

No skinny generated CSS L4 profile exists. The source blockers are concrete:

| Blocker | Evidence | Consequence |
|---|---|---|
| No generated CSS runtime module | `skinny/crates/runtime/src/grammars/` contains generated `json/` plus `sheets_witness/`; no `css_l4/` or `css_l4_declaration_values/` exists | There is no CSS Track 1 binary to profile. |
| Codegen rejects non-JSON runtime emission | `skinny/crates/codegen/src/json_provider.rs:4-12` accepts only `backend.grammar_name == "json"`; `skinny/crates/codegen/src/lib.rs:108` and `:146` call that guard | `emit_from_source("css_l4", ...)` cannot legally produce runtime files today. |
| JSON template policy is embedded in generated runtime | `skinny/crates/codegen/src/json_templates/generated.rs:10`, `:47-57`, `:90-116`, `:142-216` hardcode JSON structural alphabet, dispatch, key/colon, string escape, and number policy | Lock 14 blocks CSS emission until per-grammar config/extraction lands. |
| `GrammarConfig` surface is not landed | `skv12-value-api-audit.md` requires the config trait and per-grammar metadata before non-JSON emission | CSS profile before this fix would be a JSON-policy clone. |
| No CSS comparator row exists under the pin root | `/tmp/skv12-pin-p1` has no CSS or non-JSON artifacts | No lightningcss-vs-generated same-plane number exists. |

P1-C therefore records the CSS L4 target as `unprofiled: no generated Track 1`.
That is not a preflight substitution with Sheets. It is the boundary condition
S-P2/S-P3 must consume: W1 must first create a generated CSS L4 parser plus
strict lightningcss comparator/equality path before any CSS hot-leaf,
Mode III, or SOTA claim becomes measurable.

## Section 5 - Delta And Anomalies

This P1-C rerun moves no result rows and changes no gate status.

| Surface | Disposition |
|---|---|
| JSON `parse_only` | Diagnostic-only under the pin; never a SOTA admission target. |
| JSON direct/typed guards | Parent PMU capture may refresh guard evidence, but P1-C has no Mode III delta to apply. |
| CSS L4 generated parser | Absent; no Track 1, Track 2, lightningcss comparator, or equality artifact. |
| Sheets | Fallback-only after CSS redress failure; not an equivalent profile target. |
| REDRESS 112/113 | The user pin supersedes the old target choice/block at the mandate level, but the factual codegen/runtime blockers remain until fixed. |
| REDRESS 96/97/98 and 88/89/90 | Historical implementations remain evidence; categories are unblocked, but P1-C has no new implementation or microbench. |

The only actionable P1-C finding is negative and load-bearing: as of this
pin-aware S-P1 cycle, fresh Mode III and CSS L4 profiles do not exist. Any
downstream intervention that cites them must first produce them.

## Section 6 - Sources

- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md` prior
  pre-pin artifact
- `restart/skinny/tranches/sk-v12/research/skv12-profile-truth-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A7-sheets-execution-scout.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `/tmp/skv12-pin-p1/pmu/pmu-commands.sh`
- `/tmp/skv12-pin-p1/pmu/capture_status.tsv`
- `/tmp/skv11-open-criterion-3ce75df`
