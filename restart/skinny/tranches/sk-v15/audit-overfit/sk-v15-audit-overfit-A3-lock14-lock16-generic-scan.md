# SK-V15 S-P0 A3 - Lock 14 / Lock 16 Generic-Crate Scan

Date: 2026-05-27.
Scope: SK-V15 S-P0 Axis A3, Lock 14 / Lock 16 generic-crate scan.

## Commands Run

- `git status --short`
- `nl -ba restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`
- `nl -ba restart/skinny/tranches/sk-v15/SYNTHESIS.md`
- `nl -ba restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`
- `nl -ba restart/locks/LOCKS.md`
- `nl -ba skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `nl -ba skinny/xtask/src/main.rs`
- `nl -ba skinny/crates/bbnf-bench/src/report.rs`
- `nl -ba skinny/crates/bbnf-bench/src/bin/gate.rs`
- `rg -n` token scans over declared Lock 14 generic roots and omitted codegen roots.
- `rg -n` Lock 16 scan over `bbnf-simd`, parse-that, generated scanners, lowerers, and gate/report code.

No cargo/build/check command was run, to preserve the "one artifact and no other files" constraint.

## Verdict Table

| id | severity | verdict | evidence | prune receiver |
|---|---|---|---|---|
| A3-L14-ROOT-01 | CRITICAL | Lock 14 generic scan still silently omits leak-bearing codegen roots. | `restart/skinny/tranches/sk-v15/SYNTHESIS.md:44`, `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45`, `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:47`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370` | PRUNE-WAVE-B |
| A3-L14-TOKEN-02 | CRITICAL | The scan token universe is JSON-only, so in-scope CSS / Sheets / decision leaks pass. | `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2381`, `skinny/crates/codegen/src/grammar_profile.rs:89`, `skinny/crates/runtime/src/lib.rs:6`, `skinny/crates/passes/src/decision_csp.rs:162` | PRUNE-WAVE-B + PRUNE-WAVE-C + REBUILD-WAVE-F |
| A3-L14-CODEGEN-03 | HIGH | Omitted codegen roots contain explicit grammar-profile matches, JSON templates, and a hand-written CSS runtime string. | `skinny/crates/codegen/src/runtime_generator.rs:81`, `skinny/crates/codegen/src/runtime_generator.rs:114`, `skinny/crates/codegen/src/runtime_generator.rs:713`, `skinny/crates/codegen/src/json_sink_direct.rs:82`, `skinny/crates/codegen/src/json_typed_direct.rs:26`, `skinny/crates/codegen/src/json_templates/generated.rs:4` | PRUNE-WAVE-C |
| A3-GATE-SELF-04 | HIGH | Companion report gate path remains self-exempting for legacy non-JSON/CSS reports when `--check-results` is omitted. | `skinny/xtask/src/main.rs:285`, `skinny/crates/bbnf-bench/src/bin/gate.rs:55`, `skinny/crates/bbnf-bench/src/bin/gate.rs:63`, `skinny/crates/bbnf-bench/src/bin/gate.rs:74`, `skinny/crates/bbnf-bench/src/bin/gate.rs:91` | PRUNE-WAVE-B |
| A3-L16-REPORT-05 | HIGH | Lock 16 admission/report coverage is not a source-present primitive manifest and does not enforce strict command text. | `restart/locks/LOCKS.md:480`, `restart/locks/LOCKS.md:491`, `restart/locks/LOCKS.md:506`, `skinny/crates/bbnf-bench/src/report.rs:1034`, `skinny/crates/bbnf-bench/src/report.rs:2410`, `skinny/crates/bbnf-bench/src/bin/gate.rs:2536` | PRUNE-WAVE-B |
| A3-L16-CHECKASM-06 | MEDIUM | `xtask primitive-checkasm` is strict when used, but standalone checkasm still defaults to non-strict logging. | `skinny/xtask/src/main.rs:1861`, `skinny/xtask/src/main.rs:1876`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:16`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:112`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:280` | PRUNE-WAVE-B |
| A3-CLEAN-07 | CLEAN | The bench gate does call `lock14_baseline::validate` before companion reports; newer SK-V13+ companion reports require `--check-results`. | `skinny/crates/bbnf-bench/src/bin/gate.rs:51`, `skinny/crates/bbnf-bench/src/bin/gate.rs:95`, `skinny/crates/bbnf-bench/src/bin/gate.rs:388` | none |

## Findings

### A3-L14-ROOT-01 - CRITICAL

The binding SK-V15 close condition says Lock 14 / Lock 16 gates must include previously excluded leak files, report every exclusion, and reject self-exempting gates (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:44`). The prior audit already named the excluded files: `runtime_generator.rs`, `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, and `json_templates/` (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45`, `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:47`).

The live generic scan roots still name only `bbnf-regex`, `codegen/src/lib.rs`, `codegen/src/lower`, `grammar_profile.rs`, `passes`, `runtime/src/lib.rs`, `runtime/src/tape`, and `ir/src` (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370`). They do not include the previously excluded leak-bearing roots. The tests only assert W7 roots are present (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:4661`) and explicitly bless JSON-owned leak roots (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:4711`).

Surgery: PRUNE-WAVE-B must replace the handpicked root list with a full generic crate walk plus explicit, reported allowlist classes. Excluded roots must be emitted as gate findings, not hidden pass conditions.

### A3-L14-TOKEN-02 - CRITICAL

`FORBIDDEN_GENERIC_TOKENS` only covers JSON-shaped tokens (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2381`). That misses non-JSON grammar names inside roots the gate already claims to scan:

- `grammar_profile.rs` hardcodes eight runtime profiles, including seven CSS L4 profile statics (`skinny/crates/codegen/src/grammar_profile.rs:89`, `skinny/crates/codegen/src/grammar_profile.rs:117`).
- `runtime/src/lib.rs` exposes grammar-named modules and aliases for JSON, seven CSS L4 runtimes, and sheets witness (`skinny/crates/runtime/src/lib.rs:3`, `skinny/crates/runtime/src/lib.rs:6`, `skinny/crates/runtime/src/lib.rs:31`, `skinny/crates/runtime/src/lib.rs:35`).
- `passes/src/decision_csp.rs` carries `static_css_provider_status`, `json_sink_only_status`, and a `JSON-CSS` block id (`skinny/crates/passes/src/decision_csp.rs:162`, `skinny/crates/passes/src/decision_csp.rs:166`). The IR facts carry the same grammar-named fields (`skinny/crates/ir/src/cost.rs:242`).

Surgery: PRUNE-WAVE-B expands the token classes beyond JSON; PRUNE-WAVE-C removes grammar-name dispatch from codegen/runtime facades; REBUILD-WAVE-F replaces grammar-named decision facts with grammar-id indexed neutral facts.

### A3-L14-CODEGEN-03 - HIGH

The omitted codegen files contain the exact leak shapes Axis A3 is meant to catch:

- `runtime_generator.rs` branches on `RuntimeGenerationMode` and routes CSS frontend facts separately (`skinny/crates/codegen/src/runtime_generator.rs:19`, `skinny/crates/codegen/src/runtime_generator.rs:81`).
- `css_profile_config` is a seven-arm CSS profile match over grammar strings (`skinny/crates/codegen/src/runtime_generator.rs:114`).
- CSS runtime files are emitted from embedded string constants, including `CSS_GENERATED_RS` (`skinny/crates/codegen/src/runtime_generator.rs:97`, `skinny/crates/codegen/src/runtime_generator.rs:713`).
- The full-parse surface is a `CssFullParseSummary` counter plane (`skinny/crates/codegen/src/runtime_generator.rs:762`, `skinny/crates/codegen/src/runtime_generator.rs:766`).
- JSON direct/template providers expose `JsonSink`, JSON regex helpers, `JsonNodeKind`, and JSON structural bytes (`skinny/crates/codegen/src/json_sink_direct.rs:82`, `skinny/crates/codegen/src/json_typed_direct.rs:26`, `skinny/crates/codegen/src/json_templates/generated.rs:4`, `skinny/crates/codegen/src/json_templates/generated.rs:13`).

Surgery: PRUNE-WAVE-C collapses `RuntimeGenerationMode`, removes the CSS profile match table, and reclassifies JSON templates/providers as generated or grammar-owned with reported provenance. They cannot remain silent generic-crate exclusions.

### A3-GATE-SELF-04 - HIGH

`xtask gate-json` validates the results snapshot only when `--check-results` is present (`skinny/xtask/src/main.rs:285`, `skinny/xtask/src/main.rs:289`). The bench gate computes whether a companion run includes that flag (`skinny/crates/bbnf-bench/src/bin/gate.rs:55`), but three legacy companion paths return success without it:

- W1a non-JSON returns `Ok(())` when no explicit JSON check is present (`skinny/crates/bbnf-bench/src/bin/gate.rs:63`).
- SK-V12 non-JSON returns `Ok(())` under the same condition (`skinny/crates/bbnf-bench/src/bin/gate.rs:74`).
- SK-V12 CSS L4 SOTA returns `Ok(())` under the same condition (`skinny/crates/bbnf-bench/src/bin/gate.rs:91`).

Newer SK-V13 companion reports reject this path (`skinny/crates/bbnf-bench/src/bin/gate.rs:95`, `skinny/crates/bbnf-bench/src/bin/gate.rs:118`), but the legacy self-exemption still violates the SK-V15 close condition.

Surgery: PRUNE-WAVE-B makes every companion report require `--check-results` or emits an explicit non-close diagnostic. No report path may pass close by being the only companion flag.

### A3-L16-REPORT-05 - HIGH

Lock 16 requires every `core::arch::*`, `target_feature`, and `asm!` source-present primitive to map to a manifest row with scalar reference, strict checkasm/parity command, consumer, rollback, and disposition (`restart/locks/LOCKS.md:480`). Admission checkasm must run with `BBNF_SIMD_STRICT=1` (`restart/locks/LOCKS.md:491`), and source-present primitives must close as exactly one of `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS` (`restart/locks/LOCKS.md:506`).

The live report type is still `SkV13SimdAsmProductionReport`, hardcoded to one W12 CSS delimiter route (`skinny/crates/bbnf-bench/src/report.rs:1034`, `skinny/crates/bbnf-bench/src/report.rs:2368`). It has no `lock16_status` field, only Lock 14 fields (`skinny/crates/bbnf-bench/src/report.rs:1073`). Its `checkasm_command` validation rejects empty/status-only strings but does not require `BBNF_SIMD_STRICT=1` (`skinny/crates/bbnf-bench/src/report.rs:2410`). The binary gate verifies only the measurement, checkasm artifact, and orphan inventory hashes (`skinny/crates/bbnf-bench/src/bin/gate.rs:2536`).

Surgery: PRUNE-WAVE-B replaces the W12-only report with a Lock 16 primitive manifest consumed by gate close. The validator must assert strict command text, source-present primitive coverage, and one final disposition per primitive.

### A3-L16-CHECKASM-06 - MEDIUM

The strict runner exists: `primitive-checkasm` runs ten checkasm tests and sets `BBNF_SIMD_STRICT=1` (`skinny/xtask/src/main.rs:1861`, `skinny/xtask/src/main.rs:1876`). This is good coverage for the named harnesses.

The raw checkasm harness is still self-softening when invoked directly: it documents that strict is off by default (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:16`), reads strictness from the environment (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:112`), and logs instead of panicking when strict is absent (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:280`). This is acceptable for exploration, not for close evidence. The report validator gap above is what makes this a close risk.

Surgery: PRUNE-WAVE-B keeps exploratory direct tests, but close gates must accept only the strict xtask path or a command string proven equivalent.

### A3-CLEAN-07 - CLEAN

The bench gate does call `lock14_baseline::validate` before any companion reports (`skinny/crates/bbnf-bench/src/bin/gate.rs:51`). Newer SK-V13 companion reports, including the SIMD/ASM production flag, require `--check-results` before validating (`skinny/crates/bbnf-bench/src/bin/gate.rs:95`, `skinny/crates/bbnf-bench/src/bin/gate.rs:388`). `xtask primitive-checkasm` also forces strict mode when used (`skinny/xtask/src/main.rs:1876`).

This clean point does not clear A3, because the validator's root/token/report coverage is incomplete.

## Prune Receiver Routing

| receiver | routed findings | required receiving action |
|---|---|---|
| PRUNE-WAVE-B | A3-L14-ROOT-01, A3-L14-TOKEN-02, A3-GATE-SELF-04, A3-L16-REPORT-05, A3-L16-CHECKASM-06 | Restore Lock 14 / Lock 16 close coverage. Scan all generic roots, report all exclusions, require `--check-results`, require strict checkasm evidence, and consume a source-present primitive manifest. |
| PRUNE-WAVE-C | A3-L14-TOKEN-02, A3-L14-CODEGEN-03 | Remove grammar-family codegen/runtime leaks: CSS profile match table, `RuntimeGenerationMode` family split, embedded CSS runtime string, JSON template/provider carveouts. Any touch to JSON template/provider/direct/typed paths must rerun and preserve 51/51 JSON guard rows. |
| REBUILD-WAVE-F | A3-L14-TOKEN-02 | Replace `static_css_provider_status`, `json_sink_only_status`, and `JSON-CSS` block identifiers with grammar-neutral decision facts indexed by grammar id. |

## Close Verdict

Axis A3 is NOT clean. SK-V15 cannot claim Lock 14 / Lock 16 generic-crate cleanliness until the CRITICAL and HIGH rows above are pruned and re-audited.
