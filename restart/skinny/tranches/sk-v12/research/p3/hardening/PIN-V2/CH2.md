# CH2 - Generality / Lock 14 Review

Review base: commit `7316d87b`.
Lens: Lock 14 genericity for SK-V12 S-P3 PIN-V2.
Disposition: PASS.
Confidence: High.

## Findings

No blocking CH2 findings.

1. Lock 14 legality is sequenced before CSS emission. The user pin requires the seven value/API leaks to be resolved before CSS L4 emission is legal (`USER-PIN-W1-CSS-L4-SOTA.md:101-103`), and the SPEC makes W1a the `GrammarConfig + Lock 14 Legality Gate` before W1b CSS generation (`SPEC.md:314-347`, `SPEC.md:387-424`). P3-B and P3-C preserve the same dependency: W1a removes the named leaks and W1b-1 cannot emit CSS until W1a passes (`p3b-wave-sequencing.md:135-143`, `p3c-falsifiability-gates.md:163-190`).

2. Generic-crate policy leakage is fail-closed. The packet forbids `JsonParser`, `CssL4Parser`, `GoogleSheetsParser`, `BbnfBootstrap`, grammar-name branches, JSON structural alphabet, JSON escape/number/key/sink policy, and grammar-named handwritten runtime shortcuts in generic crates (`SPEC.md:261-275`, `p3c-falsifiability-gates.md:177-184`, `p2f-grammar-neutral.md:91-99`). The audit identifies the exact seven leaks that W1a must resolve (`skv12-value-api-audit.md:63-108`).

3. Sheets and BBNF-self fallback ordering is preserved. CSS L4 is authoritative, and Sheets/BBNF-self are fallback-only after a measured CSS L4 redress attempt (`USER-PIN-W1-CSS-L4-SOTA.md:18-24`, `HANDOFF.md:64-66`). W1b-1 and W1b-2 explicitly block Sheets/BBNF fallback inside CSS redress, and fallback requires a later folded plan after measured CSS evidence (`SPEC.md:421-440`, `SPEC.md:486-489`, `p3b-wave-sequencing.md:104-110`).

4. No new directive, BIR, `BackendShape`, or public substrate API is authorized. The SPEC non-negotiables and W1a exit gate forbid those expansions (`SPEC.md:220-224`, `SPEC.md:341-347`), and P3-E keeps the same route hard-blocked while allowing only generated metadata/config as the Lock 14 remedy (`p3e-preblocked-ledger.md:60-62`, `p3e-preblocked-ledger.md:129-134`). Watchpoint: `GrammarConfig` must remain the generated metadata/config surface described by the pin, not a broad new public substrate API.

5. Grammar neutrality is benchmark- and equality-proved, not prose-proved. SPEC Section 2.1 requires CSS L4 benchmark/equality proof (`SPEC.md:271-275`), W1b-1 requires generated Track 1 plus independent oracle equality and finite Mbps (`SPEC.md:433-440`), and W1b-2 requires same-plane lightningcss, strict equality, sample count, and `track1_mbps > lightningcss_mbps + 1` (`SPEC.md:470-489`). P3-D makes those fields gate-consumed and rejects producer-only telemetry (`p3d-telemetry-schema.md:97-142`, `p3d-telemetry-schema.md:257-295`).

## Required Fixes

None for CH2.

## CH2 Verdict

PASS.
