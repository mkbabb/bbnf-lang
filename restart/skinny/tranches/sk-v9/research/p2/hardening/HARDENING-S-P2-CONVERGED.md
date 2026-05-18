# SK-V9 S-P2 CONVERGED

Date: 2026-05-18.
Verdict: S-P2 Research fully converges per `restart/prompts/ORCHESTRATOR.md` §3Z.

## §3Z convergence audit

| Lens | V1 | V2 | V3 | V4 | Two consecutive ≥95% |
|---|---:|---:|---:|---:|---|
| CH1 CORRECTNESS | 96.7% | 98.2% | 97.6% | 100% | V1+V2 ✓ |
| CH2 GENERALITY | 80.6% | 100% | 100% | — | V2+V3 ✓ |
| CH3 REGRESSION | 67.4% | 93.0% | 100% | 100% | V3+V4 ✓ |
| CH4 COST | 22.7% | 100% | 100% | — | V2+V3 ✓ |
| CH5 HIDDEN COUPLING | ACCEPT | ACCEPT | ACCEPT | — | V1+V2 ✓ |
| CH6 ANTI-PAPER-CLOSE | 68% | 90.6% | 100% | 100% | V3+V4 ✓ |

All six lenses have two consecutive qualifying cycles. S-P2 advances to
S-P3 Synthesis-Plan per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.

## What S-P2 hands to S-P3

The six converged S-P2 research artefacts:

1. **P2-A union event-model** — the alternate W3 design after the
   SK-V8 W3 fit-gate rejection (REDRESS 92). The fit-gate failed
   because SC-3's alphabet handled 3 of 7 JSON event classes and the
   4 scalar anchors had no derivation, plus the hidden per-cursor
   source-byte rediscovery in `JsonNodeKind::at_cursor`. Alternate:
   keep the parser-event cursor stream, add a co-indexed class column
   at emit time, use the SIMD index as a transient producer consumed
   by move (Lock 1 cardinality stays at one). ~265 hand + 120 regen
   LOC, medium-low risk. CSS L4 / Sheets / BBNF-self instances given.

2. **P2-B retained class/event grammar + `ValueRef` proof** — an
   `EventGrammar` trait + `ValueRef<G>` + JSON & Sheets witnesses
   behind `cfg(feature = "proof")`. Proof-only depth (no production
   consumer; no row movement). ~395 LOC. Unlocks the union reopen.

3. **P2-C Apache/CITM measured-row admission** — REDRESS 91's gap is
   the `SK_V8_OPEN_BASELINE` whitelist not expanded with W2's
   admission. Mechanical fix; 300 LOC; ≤90 min. Lifts the typed-GO
   count; generalises to github_events / gsoc-2018 / instruments.

4. **P2-D aarch64 ASM opportunities** — `unescape_uxxxx_x4_neon`
   already wired at `parse-that-regex/src/lib.rs:402`; the differential
   is broadening the x4-only batcher to all-quartet + rebinding the
   consumer to the P2-A union substrate. SHA3 `veor3q_u8` collapses
   the 6-stage prefix-XOR to 3 EOR3 ops (the PMULL alternative;
   Lock-16-gated by FEAT_SHA3). 32-byte string-block widening. Five
   checkasm differential test files to author as same-wave
   preconditions.

5. **P2-E unicode-escape codec** — `escape_codec_hex_unit` primitive
   with 5 const-generic bindings (JSON-4, CSS L4 variable, JS variable,
   TOML-4, TOML-8). Honest PMU verdict: zero of the four uncloseable
   rows admit on the codec alone — unicode_escapes NEAR-FAIL 94.5%,
   y_string_unicode 94.8%, unicode_mixed FAIL 63.7%, gsoc-2018
   no-regression-basis. Admission is the §6.4 same-wave conditional
   rule (codec paired with the string-scanner widening).

6. **P2-F SOTA teardown M5 Max** — yyjson > simdjson > sonic on
   string-heavy DOM on this host; asmjson has no aarch64 backend
   (non-anchored sidecar planning signal). The >SOTA path is a
   dependency graph: I (consume the stage-1 index) ← P2-A ← P2-B;
   II (fused `\uXXXX` codec) ← P2-E; III (cost-fact-gated NEON
   tiny-string + ASM next-bit) ← P2-D ← P2-A.

## The convergent picture S-P3 must wave-sequence

bbnf's parse-plane losses are substrate-bound, not kernel-bound. The
union event-model (P2-A) is the structural fix; it is gated behind the
P2-B proof. The four uncloseable rows need the unicode codec (P2-E)
paired with the string-scanner widening (P2-D) — neither closes them
alone. Apache/CITM (P2-C) is the cheapest GO-count lift and is
substrate-independent. The dependency order is firm: P2-B proof → P2-A
union → P2-D consumers; P2-E codec is independent but conditional;
P2-C is fully independent.

S-P3 owns: the wave manifest, per-wave falsifiability gates, the
telemetry schema, the pre-blocked-route ledger, and the SK-V9 SPEC +
DISPATCH-PROMPT. The S-P2 cohort supplies intervention shapes +
preliminary cost envelopes; S-P3 authors the binding plan.

## Next move

`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` S-P3 dispatches six
sub-agents: P3-A candidate shortlist, P3-B wave sequencing, P3-C
per-wave falsifiability gates, P3-D telemetry-schema binding, P3-E
pre-blocked-route ledger, P3-F SPEC.md + DISPATCH-PROMPT.md drafting.
