# CH2 - Generality / Lock 14 Review

Review base: commit `471bf53e`.
Lens: CH2 generality / Lock 14 for SK-V12 S-P3 PIN-V4 confirmation.
Disposition: PASS.
Confidence: High.

## Findings

No blocking CH2 findings.

1. PIN-V4 does not regress the PIN-V3 CH2 surface. The accepted PIN-V3
   consolidation records that fallback remains blocked until W1b-2 records
   measured CSS lightningcss comparator/admission redress, that the CSS row and
   output plane are exact, that W2 remains the SIMD prerequisite, and that CSS
   ADMIT requires strict `> lightningcss_mbps + 1` with Lock 14/16 and zero
   production orphans (`restart/skinny/tranches/sk-v12/research/p3/hardening/PIN-V3/CONSOLIDATED.md:29`-`:47`).
   The PIN-V4 packet preserves those same commitments: SPEC status is the only
   SPEC-level cycle marker changed in this commit, while the current SPEC keeps
   Lock 14, fallback, CSS, and close clauses intact (`restart/skinny/tranches/sk-v12/SPEC.md:3`-`:5`,
   `:51`-`:64`, `:213`-`:235`).

2. `GrammarConfig` or equivalent generated metadata still precedes CSS emission.
   The user pin requires the seven value/API leaks to be resolved before CSS L4
   emission is legal (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:97`-`:103`,
   `:148`-`:150`). SPEC W1a exists for that legality step and exits only on a
   generic-crate scan, JSON parity/guard evidence, no CSS row claim, and no new
   directive/BIR/BackendShape/public substrate API (`restart/skinny/tranches/sk-v12/SPEC.md:314`-`:347`).
   W1b-1/W1b-2 remain downstream of W1a in the manifest
   (`restart/skinny/tranches/sk-v12/SPEC.md:241`-`:248`).

3. Generic crates remain barred from grammar-name branches and generic JSON
   policy. SPEC Section 2.1 forbids `JsonParser`, `CssL4Parser`,
   `GoogleSheetsParser`, `BbnfBootstrap`, grammar-name branches, JSON structural
   alphabets, JSON escape/number/key policy, JSON `OffsetFlags` meaning, and
   `JsonSink` shape in generic code (`restart/skinny/tranches/sk-v12/SPEC.md:259`-`:275`).
   P3-C makes the W1a proof executable by requiring a negative scan for
   `match grammar`, grammar-named public APIs, handwritten non-generated
   CSS/Sheets/BBNF runtime modules, and generic JSON structural alphabets
   (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:171`-`:190`).

4. The packet targets the exact leaks identified by the value/API audit rather
   than renaming JSON templates as generic. The audit names structural alphabet,
   value dispatch, string escape/quote policy, number policy, quoted key/object
   pair policy, `OffsetFlags` semantics, and `JsonSink` methods as the leak set
   (`restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63`-`:108`).
   P2-F makes `GrammarConfig` the per-grammar template surface and defines the
   negative Lock 14 proof as no grammar-named generic APIs, no `match grammar`
   behavior selection, no grammar-named generic modules except generated output,
   and no handwritten CSS/Sheets/BBNF runtime modules
   (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:89`-`:99`).

5. Fallback order is still pin-compliant. CSS L4 is the authoritative first
   target; Sheets and BBNF-self are fallback-only after a measured CSS redress
   attempt (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18`-`:24`).
   P3-B states that Sheets/BBNF-self are not W1b-1 or W1b-2 alternatives and may
   enter only after W1b-2 records measured CSS L4 redress as BLOCKED or REJECTED
   (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:104`-`:110`).
   P3-C repeats that no hidden fallback may occur inside W1b-2 redress
   (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:258`-`:264`).

6. No new directive, BIR, BackendShape, or public substrate API is authorized.
   SPEC non-negotiables block those surfaces (`restart/skinny/tranches/sk-v12/SPEC.md:220`-`:224`),
   SPEC Section 11 keeps them in the still-blocked route list
   (`restart/skinny/tranches/sk-v12/SPEC.md:651`-`:655`), and P3-E states that
   D3/D4 category unblocks do not permit new public substrate/API or enum surface
   (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:58`-`:62`).
   P3-D's union/ASM telemetry likewise requires `public_api_delta` to remain
   blocked (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:220`-`:237`).

7. Grammar neutrality remains benchmark/equality proved, not asserted. SPEC
   requires CSS L4 benchmark/equality exercise and generated-size/O(N) evidence
   (`restart/skinny/tranches/sk-v12/SPEC.md:271`-`:275`). W1b-2 requires same-plane
   lightningcss, strict three-way equality, sample count, gate consumption, and
   strict `track1_mbps > lightningcss_mbps + 1`
   (`restart/skinny/tranches/sk-v12/SPEC.md:447`-`:491`). P3-D makes the CSS
   admission fields gate-consumed and rejects unresolved GrammarConfig legality,
   generic JSON policy leakage, stale run ids, oracle coupling, producer-only
   telemetry, and non-lightningcss CSS admission
   (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:97`-`:142`,
   `:257`-`:295`).

## Required Fixes

None for CH2.

## CH2 Verdict

PASS.
