# SK-V23 — PASS-ALPHA: the dual-repo tranche codex (x86 abrogation, SIMD perfection, the greenfield delineation)

Date opened: 2026-07-09. Prior plans of record: SK-V22 SPEC (FROZEN — graduation & archival,
GW0-GW6 + PT-R/PT-T) and SK-V21 SPEC (performance/architecture adjudications). SK-V23 develops
those into the executable dual-repo tranche codex, folding three new binding directives.

## The three new axes (binding)

1. **Full x86 abrogation — BOTH repos.** aarch64/Apple-M only, everywhere: bbnf-lang (simd-scan
   avx2.rs/avx512.rs/compaction/parity, core generate/regex/emit/simd.rs, generated-file stamps,
   skinny bench/codegen surfaces) AND parse-that (rust/parse_that/src/parsers/scan/
   quote_parity.rs, number_simd.rs, decode.rs). Deletion plus an enforcement design (cfg-deny /
   CI guard / workspace lint) so x86 cannot regrow.
2. **Deep, idiomatic, fully modern SIMD within the ASM-hybrid union — BOTH repos.** The dav1d
   discipline stands (intrinsics default, per-kernel asm gate, checkasm-liveness); the mandate is
   SOTA NEON leverage: the vshrn idiom family, multi-block pipelining, table-lookup
   classification, Hyperscan-lineage acceleration (shufti/truffle/Teddy-class prefilters,
   vectorized DFA stages) applied to bbnf's kernel plane AND parse-that's regex engine + scanners.
3. **Greenfield-from-skinny, delineated honestly.** Skinny seeds the graduation but is NOT
   ratified as-is: extant problems — hardcoding residue (WART-C two-site name-gate and kin), NO
   type system (monotone fold only), no proper SIMD support (dead/partial kernels), poor Value
   API (prototype-grade sink, no unified path/lane surface) — get GREENFIELD implementation.
   The delineation (greenfield vs carry, per subsystem) is a first-class deliverable.

## The deliverable

**At least 5 tranches for EACH repo** (more or less as befits), pursuant to the most modern
precept set (the precepts submodule + the SK-V20/21/22 process surgery: Y.13 consumer-invariant
gates, profiling standing law — PMU + paired interleaving + multi-window + per-block loadavg —,
no-legacy, one challenge wave, batches, cold-only). Includes: the cleanup/archival execution
(SK-V22 §7 sequence), skinny ratification-and-greenfield, the x86 abrogation, the SIMD
modernization, the parse-that SOTA uplift (PT-R/PT-T matured into full tranches), and the novel
/ QoL / performance feature folds surfaced by this pass's brainstorm fleet.

## The non-clobber law (binding)

value.js and keyframes.js tranche sessions actively develop against parse-that. Observed at
opening: parse-that HEAD quiet since VALUEJS-PT-E (2026-07-05) but carrying UNTRACKED
work-in-progress (docs/instructions/, docs/precepts/, rust/.cargo/, .gitmodules). Therefore:
NOTHING in ~/Programming/parse-that is modified, staged, or committed by this campaign EXCEPT
one additive coordination letter (a NEW file, our own letter ID) delivered at close to keep
those sessions informed. Prototyping against parse-that code happens on scratch COPIES, never
in its working tree. Tranche lettering for parse-that avoids collision with its native series
(A/B/Q/S...) — ours propose as the PT-R*/PT-T* track family.

## The pass protocol (the convergence loop, verbatim)

1. Up to 8 agents in parallel research the web, the extant codebases, the tranches hitherto.
2. One synthesis agent folds the results into a cogent specification and plan.
3. A fleet of prototyping agents — concrete implementation or prototype-augmented
   specification — greenfield-brainstorms and test-implements against that specification
   (analysis instruments: isolated, never landed).
4. A fleet of critique agents hardens, challenges, refines; each returns a percentage of
   convergence with critical analysis.
5. A final synthesizing agent agglomerates; the loop re-enters with newfound context.
Whereupon 100% convergence: stop, and develop out the exact tranche plan/wave sets.

NOT an implementation phase. Tranche development only.
