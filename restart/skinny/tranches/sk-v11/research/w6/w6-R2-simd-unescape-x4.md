# SK-V11 W6 R2 - SIMD Unescape x4 Diagnosis

Scope: bbnf-simd AArch64 `unescape_uxxxx` x4 / hex-decode parity,
checkasm-style tests, feature gates, and whether any SIMD body can be a
same-wave production candidate under SPEC Section 10.

## Disposition

`unescape_uxxxx_x4_neon` is valid proof inventory, not an immediate W6
production admit. Under SPEC Section 10, no existing SIMD body can admit a row
by being wrapped, re-gated, or re-claimed through the already-consuming
`unescape_string` path. A production plan is admissible only if it lands a new
escaped-segment source delta, a scalar segment/x4 oracle, strict x4 parity, and
a same-wave direct/typed/non-JSON product consumer.

The best W6 candidate shape is therefore:

1. Add a scalar escaped-segment visitor or hex-run oracle in
   `parse-that-regex`.
2. Add a scalar x4 oracle that calls x1 semantics four times and preserves
   valid, invalid, mixed-lane, surrogate, boundary, and error-offset behavior.
3. Expand strict x4 checkasm before any row measurement counts.
4. Wire one new generated direct/typed escaped-segment consumer in the same
   wave. The existing `unescape_string` caller may remain a fallback/reference,
   but it cannot be the production integration claim.

If the W6 plan selects only the current `unescape_string ->
unescape_four_unicode_escapes -> unescape_uxxxx_x4_neon` path, reject before
redress as REDRESS 107/108 paper-close.

## Binding Owner Surface

SPEC Section 10 names the W6 owner paths: `parse-that-regex/src/lib.rs`,
`bbnf-simd/src/aarch64/unescape_uxxxx.rs`, `bbnf-simd/tests/`, `codegen/src/`,
`runtime/src/grammars/json/generated.rs`, `bbnf-bench/src/direct_struct.rs`,
`bbnf-bench/src/generated_real_typed.rs`, `bbnf-bench/benches/json_parity.rs`,
`skinny/RESULTS.md`, and `skinny/REDRESS.md`
(`restart/skinny/tranches/sk-v11/SPEC.md:600`). The W6 entry gate requires W5
disposition plus a plan naming a new source delta beyond the already-consuming
`unescape_string` path (`restart/skinny/tranches/sk-v11/SPEC.md:613`). Its
tasks explicitly require a scalar escaped-segment visitor or hex-run oracle,
strict x4 checkasm if x4 is routed, and a new direct/typed/non-JSON consumer
(`restart/skinny/tranches/sk-v11/SPEC.md:617`). The exit gate says x4 proof
cannot admit production without that new source delta and same-wave consumer
(`restart/skinny/tranches/sk-v11/SPEC.md:625`), and the pre-blocked routes
include REDRESS 107/108 plus reuse of existing `unescape_string`
(`restart/skinny/tranches/sk-v11/SPEC.md:645`).

Targetable W6 direct rows are `unicode_escapes`, `unicode_mixed`, and
`y_string_unicode` (`restart/skinny/tranches/sk-v11/SPEC.md:627`). Their SK-V11
floors are 3441, 2588, and 3950 Mbps respectively
(`restart/skinny/tranches/sk-v11/SPEC.md:132`,
`restart/skinny/tranches/sk-v11/SPEC.md:133`,
`restart/skinny/tranches/sk-v11/SPEC.md:135`). Direct guard floors and typed
guard floors still bind if reports are refreshed
(`restart/skinny/tranches/sk-v11/SPEC.md:137`,
`restart/skinny/tranches/sk-v11/SPEC.md:149`).

## Implementation Facts

The scalar x1 reference exists in `bbnf-simd`: `unescape_uxxxx_scalar` decodes
one four-byte quartet using `hex_nibble` and returns `None` for any non-hex
nibble (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40`). The module
also exposes `join_surrogates` as an algebraic UTF-16 pair helper
(`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:54`).

The x1 NEON body is present and uses a low-nibble LUT plus ASCII digit/letter
range masks (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:74`,
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:81`,
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:85`,
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:116`). The x4 body is
also present: it loads 16 packed hex bytes, applies the same LUT/range mask
logic, rejects if any lane is non-hex, stores nibble lanes, and packs four
u32 units (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:125`,
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:126`,
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:151`,
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:157`,
`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:160`).

Feature gating is compile-time AArch64 only. The module is exported behind
`#[cfg(target_arch = "aarch64")]`
(`skinny/crates/bbnf-simd/src/aarch64/mod.rs:29`), and the current x4 path does
not use an additional `target_feature` guard. That is acceptable for NEON on
AArch64, but it means non-AArch64 fallback is achieved by not compiling this
caller branch, not by runtime dispatch.

The current production caller is already wired. `parse-that-regex`
`unescape_four_unicode_escapes` packs four consecutive `\uXXXX` quartets
(`skinny/crates/parse-that-regex/src/lib.rs:386`,
`skinny/crates/parse-that-regex/src/lib.rs:391`,
`skinny/crates/parse-that-regex/src/lib.rs:397`), calls
`unescape_uxxxx_x4_neon` (`skinny/crates/parse-that-regex/src/lib.rs:401`),
handles surrogate pairs in the caller (`skinny/crates/parse-that-regex/src/lib.rs:415`,
`skinny/crates/parse-that-regex/src/lib.rs:419`,
`skinny/crates/parse-that-regex/src/lib.rs:437`), and returns `slash + 24` on a
four-escape batch (`skinny/crates/parse-that-regex/src/lib.rs:458`). That caller
is invoked directly from `unescape_string` on AArch64 before falling back to
scalar `decode_unicode_escape` (`skinny/crates/parse-that-regex/src/lib.rs:775`,
`skinny/crates/parse-that-regex/src/lib.rs:778`,
`skinny/crates/parse-that-regex/src/lib.rs:783`).

Existing direct and typed consumers already reach x4 only through
`unescape_string`: Track 2 direct string parsing calls `unescape_string` after
`match_string_at_quote_trusted_utf8` when `span.needs_decode()`
(`skinny/crates/bbnf-bench/src/direct_struct.rs:549`,
`skinny/crates/bbnf-bench/src/direct_struct.rs:557`); generated typed code does
the same (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1660`,
`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1666`); and the codegen
template for typed direct parsers emits the same call shape
(`skinny/crates/codegen/src/typed_direct.rs:491`,
`skinny/crates/codegen/src/typed_direct.rs:497`). Reusing any of those call
sites is exactly the REDRESS 108 failure shape unless W6 changes the product
contract in a real, measured way.

## Parity And Checkasm Status

Current x1 parity is materially stronger than current x4 parity. The
`sk_v3_intrinsic_parity_aarch64` test builds valid and invalid x1 hex cases
(`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:550`,
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:557`), sweeps alignments 0..63
(`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:617`,
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:646`), and compares
`unescape_uxxxx_neon` against `unescape_uxxxx_scalar`
(`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:650`,
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:651`,
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:656`).

Current x4 test coverage is only a fixed valid smoke case:
`unescape_uxxxx_x4_matches_scalar` decodes `0041d83dde0000e9`, checks the four
units, and checks one surrogate join
(`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:58`,
`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:60`,
`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:63`,
`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:65`). P2 already records
this as smoke-only: x4 production needs invalid-case, alignment,
surrogate-policy, caller differential, and strict-mode coverage
(`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:129`,
`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:139`).

The missing x4 checkasm cell must cover:

- valid and invalid quartets in each lane;
- mixed-validity packs where earlier lanes are valid and a later lane fails;
- alignment 0..63 for the 16-byte packed load;
- dense surrogate pairs, lone high surrogates, lone low surrogates, and
  surrogate split across lane/batch boundaries;
- short/tail cases where fewer than four consecutive `\uXXXX` escapes exist;
- error offset parity against scalar caller semantics.

The last point is not cosmetic. The x4 body returns `None` when any packed nibble
is invalid (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:151`), and the
current caller maps that to a single error at the starting slash
(`skinny/crates/parse-that-regex/src/lib.rs:401`,
`skinny/crates/parse-that-regex/src/lib.rs:404`). Existing `unescape_string`
tests check scalar invalid offsets for several single and surrogate cases
(`skinny/crates/parse-that-regex/src/lib.rs:1174`,
`skinny/crates/parse-that-regex/src/lib.rs:1181`,
`skinny/crates/parse-that-regex/src/lib.rs:1183`), but they do not constitute
strict x4 mixed-lane offset parity. W6 must either define x4 fallback so exact
scalar offsets are preserved or prove that the product contract only requires
coarser batch failure, which would need explicit CHALLENGE acceptance.

## Micro-Proof Status

REDRESS 107 admits the SK-V10 W8 micro-proof only as proof. It proved the
existing `unescape_string -> unescape_four_unicode_escapes ->
unescape_uxxxx_x4_neon` path on AArch64 with `-C target-cpu=native`, scalar
oracle `unescape_uxxxx_scalar + scalar JSON surrogate policy`, and threshold
`>=1.08x` (`skinny/REDRESS.md:3174`, `skinny/REDRESS.md:3178`,
`skinny/REDRESS.md:3180`, `skinny/REDRESS.md:3182`). It cleared aggregate
speedup at `1.268x`, with `unicode_escapes` at `2.636x`, `y_string_unicode` at
`0.943x`, and `unicode_mixed` zero eligible because its `\u` text is
escaped-backslash data (`skinny/REDRESS.md:3185`, `skinny/REDRESS.md:3186`,
`skinny/REDRESS.md:3187`). The W8 artifact records the same binding:
`target_arch=aarch64`, `aarch64-apple-darwin`, `-C target-cpu=native`, 25
samples, and aggregate threshold `>=1.08x`
(`restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/W8-ESCAPE-MICROPROOF.md:15`,
`restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/W8-ESCAPE-MICROPROOF.md:16`,
`restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/W8-ESCAPE-MICROPROOF.md:17`,
`restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/W8-ESCAPE-MICROPROOF.md:19`,
`restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/W8-ESCAPE-MICROPROOF.md:31`).

That proof does not admit production. REDRESS 107 explicitly says W8 moves no
`RESULTS.md` row and wires no new production behavior (`skinny/REDRESS.md:3194`).
REDRESS 108 rejects W9 because the exact caller already consumed x4 before W9;
no cosmetic wrapper, constant, or feature re-gate was attempted
(`skinny/REDRESS.md:3200`, `skinny/REDRESS.md:3201`,
`skinny/REDRESS.md:3202`). W9 moved no source, gate, report, or `RESULTS.md`
row (`skinny/REDRESS.md:3204`), and future production reuse requires a new
SPEC/CHALLENGE route naming a real source delta (`skinny/REDRESS.md:3220`).

W6 may reuse W8 as micro-proof background only. It cannot count W8 as the W6
same-wave consumer proof because the consumer already exists.

## Consumer Constraints Under SPEC Section 10

An admissible W6 production route needs a new product-facing consumer. Plausible
shapes:

- A generated JSON direct consumer that uses a new escaped-segment visitor to
  produce the existing direct digest output, mirrored by independent Track 2 and
  strict parity against serde/sonic comparators.
- A typed decoded-string field path where the generated typed parser consumes
  escaped segments directly and returns the same typed value without parser-owned
  scratch or semantic facts.
- A non-JSON escaped-string or hex-color consumer only if it is already inside
  the accepted W1b/W2 baseline authority or CHALLENGE explicitly accepts it
  under SPEC Section 10's fallback clause.

Non-admissible shapes:

- Reusing `unescape_string` as the same-wave production consumer.
- Adding a wrapper around `unescape_four_unicode_escapes`.
- Re-gating the current x4 path behind a new feature flag.
- Treating x4 primitive parity or W8 caller microbench as row admission.
- Moving JSON surrogate policy into `bbnf-simd` or a grammar-neutral
  `parse-that-regex` API.
- Adding decoded scratch, output hash side channels, or semantic string facts.

The P3 shortlist says the same thing: C3 requires a segment visitor over raw
spans, simple escapes, and decoded scalar values, with JSON surrogate policy,
CSS variable-width escapes, and BBNF literal policy in generated or host caller
code (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:197`).
It lists existing scalar references but requires a new scalar segment-stream
oracle and scalar x4 oracle for production (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:201`).
It also states current x4 evidence is proof/smoke only and that strict x4
checkasm must cover valid, invalid, mixed-validity, alignment, surrogate,
unpaired-surrogate, and boundary/tail cases
(`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:206`).

## Risks

1. **REDRESS 107/108 paper-close.** The strongest risk is re-claiming the
   already-wired `unescape_string` caller. SPEC Section 10 pre-blocks it, and
   REDRESS 108 is explicit that a future route needs a real source delta.
2. **x4 error semantics.** Current x4 failure is all-or-nothing. Without a
   scalar x4 oracle and mixed-lane offset parity, a production path can silently
   weaken malformed-string diagnostics or fallback behavior.
3. **Sparse-row performance.** W8 showed `unicode_escapes` strong and
   `y_string_unicode` below threshold at `0.943x`; `unicode_mixed` had zero
   eligible fixed-width Unicode escape payload. W6 should not select all three
   rows by analogy.
4. **Policy leakage.** JSON surrogate joining belongs in generated/host caller
   policy. `bbnf-simd` should decode hex units, not own JSON, CSS, Sheets, or
   BBNF escape policy.
5. **Consumer coupling.** Track 1 and Track 2 both currently call
   `unescape_string`; if W6 edits only the shared generic function, it risks
   coupling the independent oracle and producing a false parity pass.
6. **Close-condition pressure.** W5 is REDRESS 116 blocked before source
   redress, and HANDOFF says W6 may dispatch only through an independent segment
   plan with a new source delta beyond `unescape_string`
   (`restart/skinny/tranches/sk-v11/HANDOFF.md:113`,
   `restart/skinny/tranches/sk-v11/HANDOFF.md:117`).

## Sources

- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `skinny/REDRESS.md`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/bbnf-simd/src/aarch64/mod.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/W8-ESCAPE-MICROPROOF.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md`
