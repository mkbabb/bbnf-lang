# SK-V12 W4 A3 - CSS Generated Consumer Feasibility

Date: 2026-05-20
Scope: W4 ASM-gen CSS consumer feasibility for
`css_l4/declaration_values/direct_to_struct/main`.
Ownership: this research writes only this file.

## Sources Read

- `restart/skinny/tranches/sk-v12/SPEC.md` Section 9.
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/{generated,parser,sink,config,mod}.rs`.
- `skinny/crates/codegen/src/css_l4_declaration_values_templates/{generated,parser,config,mod}.rs`.
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`.
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`.
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`.
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`.
- Relevant SIMD code under `skinny/crates/bbnf-simd/src/{lib.rs,dispatch.rs,aarch64,scalar}` and
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`.

## Finding

Yes, one selectable ASM-gen route can be wired same-wave into the CSS L4
declaration-values Track 1 consumer without generic policy leaks:

`a64_ascii_set_run_skip`, implemented as a CSS-generated delimiter run-skip over
the existing NEON `bbnf_simd::prim::byte_class_from_eq_set_64` primitive.

The consumer is the generated CSS scanner, not a generic substrate. It replaces
byte-at-a-time scans in the CSS-local parser:

- top-level `{` discovery in
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:25-34`;
- block delimiter scan for `{`, `;`, `}` in
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:38-58`;
- optionally, colon/depth scan for `:`, `(`, `)`, `[`, `]` in
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:180-190`.

The minimal admissible consumer is the block delimiter scan only. It is enough
to execute the selected primitive in the Track 1 path because the scanner may
load a 64-byte block starting at the current cursor and return the first mask
bit even when the delimiter appears early inside that block. It does not require
pre-indexing, a public substrate, a new directive, a BIR variant, or JSON
policy.

This should be treated as a measured candidate, not a guaranteed speedup. The
fixture is only 187 bytes, Track 1 is already far above lightningcss in the
W1b-2b report (`429.34420791225705 Mbps` vs `168.92962215656692 Mbps`), and a
64-byte classifier can lose if call overhead or block setup dominates. W4 can
still close as `BEHAVIOR-PASS-NONCLOSE` or `MEASURED-REJECT` if parity and
consumer evidence are complete but the row does not improve.

## Why Not TBL/TBX As The Primary CSS Block-Delimiter Route

The existing TBL classifier in `aarch64/classify_tbl4.rs` is a low-6-bit table.
It rejects or misrepresents byte sets with low-6 collisions. CSS block
delimiters collide:

- `b'{' & 0x3f == 59`;
- `b';' & 0x3f == 59`.

Therefore `a64_tbl_tbx_byte_class_mask64` is not the minimal legal primitive for
the CSS block delimiter set unless W4 first implements a new full TBX/fallback
table shape. That would be a larger primitive than the available redress budget
needs. The existing eq-set NEON body does not have this collision problem and
already has a scalar reference plus checkasm harness.

## Policy / Lock-14 Boundary

The legal shape keeps CSS policy inside the CSS generated module:

- The byte sets are CSS-local constants generated from the CSS declaration-value
  consumer (`b"{};"`, and optionally `b"{"` or `b":()[]"`).
- `bbnf-simd` sees only an ASCII byte-set membership primitive. It does not
  learn CSS grammar rules.
- `parse-that-regex` is not needed for the minimal route.
- `codegen/src/json_templates/` is not needed and should not be touched.
- `runtime/src/grammars/json/` is not needed and should not be touched.

One SPEC owner-table issue should be corrected by W4 plan/CHALLENGE before
redress: SPEC Section 9 lists `skinny/crates/codegen/src/json_templates/`, but
the regen source for this consumer is
`skinny/crates/codegen/src/css_l4_declaration_values_templates/generated.rs`.
Editing JSON templates would be a generic policy leak for this CSS-only
consumer.

## Exact Source Owner Paths For The Feasible Route

Required behavior/codegen owner paths:

- `skinny/crates/codegen/src/css_l4_declaration_values_templates/generated.rs`
  - Add the generated source of the CSS-local run-skip helper.
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
  - Regenerated output containing the same helper and the `scan_block` consumer.

Required verification/gate owner paths:

- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
  - Add CSS delimiter, colon/depth, tail, duplicate, high-bit, and all-miss
    cases for `b"{};"` and optional `b":()[]"`.
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
  - Add an isolated same-host microbench helper for the CSS block-delimiter
    run-skip, plus strict equality hooks if the generated code changes.
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
  - Add/consume the microbench lane and keep the existing Track 1/cssparser/
    lightningcss lanes.
- `skinny/crates/bbnf-bench/src/report.rs`
  - Record W4 Lock 16 status, selected primitive, microbench evidence, and
    unchanged row identity/provenance.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
  - Consume the W4 microbench/report evidence and reject report-only or
    checkasm-only primitive admissions.
- `skinny/REDRESS.md`
  - Record the W4 admit/nonclose/reject measurement and material differential.
- `skinny/RESULTS.md`
  - Move only if W4 changes admitted CSS row status or JSON guard disposition.
- `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`
  - Account for all five W4 orphans per SPEC Section 9.

Not required for the minimal route:

- `skinny/crates/parse-that-regex/src/`.
- `skinny/crates/codegen/src/json_templates/`.
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/{parser,sink,config,mod}.rs`.
- `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs` and
  `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs`, unless
  CHALLENGE requires additional documentation or a safe wrapper. The existing
  primitive body and scalar reference are sufficient.

## Minimal Consumer Patch Sketch

The sketch below shows the intended generated-code shape. It is deliberately
CSS-local and does not create a public substrate.

```rust
use bbnf_simd::prim::byte_class_from_eq_set_64;

const CSS_BLOCK_DELIMS: &[u8] = b"{};";

#[inline]
fn next_css_byte_set(bytes: &[u8], mut pos: usize, end: usize, set: &[u8]) -> usize {
    debug_assert!(set.len() <= 8);
    while pos + 64 <= end {
        let block: &[u8; 64] = bytes[pos..pos + 64]
            .try_into()
            .expect("64-byte CSS scan block");
        let mask = byte_class_from_eq_set_64(block, set);
        if mask != 0 {
            return pos + mask.trailing_zeros() as usize;
        }
        pos += 64;
    }
    while pos < end && !set.contains(&bytes[pos]) {
        pos += 1;
    }
    pos
}

fn scan_block(&mut self, depth: u32, sink: &mut FactSink) -> Result<(), CssFactError> {
    let mut segment_start = self.pos;
    while self.pos < self.bytes.len() {
        self.pos = next_css_byte_set(self.bytes, self.pos, self.bytes.len(), CSS_BLOCK_DELIMS);
        if self.pos >= self.bytes.len() {
            break;
        }
        match self.bytes[self.pos] {
            b'{' => {
                self.pos += 1;
                self.scan_block(depth + 1, sink)?;
                segment_start = self.pos;
            }
            b';' => {
                self.emit_declaration(segment_start, self.pos, depth, sink)?;
                self.pos += 1;
                segment_start = self.pos;
            }
            b'}' => {
                self.emit_declaration(segment_start, self.pos, depth, sink)?;
                self.pos += 1;
                return Ok(());
            }
            _ => unreachable!("CSS delimiter set only yields block delimiters"),
        }
    }
    Err(CssFactError {
        offset: self.pos,
        message: "unterminated CSS block",
    })
}
```

If CHALLENGE wants a smaller blast radius, apply the helper only to
`scan_block` first. If microbenching shows a positive isolated result and no
equality drift, the same helper can later be applied to top-level `{` discovery
and `find_colon`.

## Required Redress Checks

- `cargo test -p bbnf-simd --release --test checkasm_byte_class_from_eq_set_64 -- --nocapture`
  with added CSS sets.
- `cargo test -p runtime css_l4_declaration_values_emit_fact_stream -- --nocapture`.
- `cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture`.
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30`.
- W4 gate command consuming the W4 report, W1b-2b SOTA report, and JSON guard
  state.
- Strict byte equality of Track 1, cssparser, and lightningcss fact streams.

## Reject Paths

- `a64_tbl_tbx_byte_class_mask64` on the CSS block delimiter set as currently
  implemented: rejected for `{`/`;` low-6 collision. A new full TBX fallback
  table would be a separate, larger primitive and needs its own micro-proof.
- `byte_class_from_table_64` as an aarch64 admission: rejected because the
  aarch64 body delegates to the scalar table implementation today.
- `a64_udot_digit_run_span`: rejected for this CSS fixture unless a later plan
  proves a real CSS numeric-token consumer. Current CSS tokens are sign,
  decimal, percent, and dimension spans; the in-tree UDOT helper is a four-digit
  decode proof, not a CSS number-span consumer.
- `a64_hex_quartet_decode_x4`: rejected for this output plane. The fixture has
  CSS hash colors, but Track 1 emits raw hash lexemes and does not decode
  unicode or hex quartets.
- `a64_wide_string_special_scan64`: rejected for this fixture/consumer because
  the W1b CSS row contains no quoted CSS string token that the generated Track 1
  scanner consumes.
- PMULL prefix-XOR, CSSC CTZ/bulk emit, `byte_context`, and `cache_hints`:
  rejected as W4 CSS row movers unless a separate plan supplies fresh
  microbench and a CSS or JSON-guard hot-leaf consumer. They are currently
  orphan/support inventory under SPEC Section 9 and the SIMD coverage audit.
- Any edit under `codegen/src/json_templates/` or a JSON runtime consumer for
  this W4 CSS route: rejected as generic policy leakage and not a same-wave CSS
  consumer.

## Disposition

Primary feasible route: `a64_ascii_set_run_skip` over
`byte_class_from_eq_set_64`, consumed in generated CSS `scan_block`.

Dispatch condition: W4 plan must explicitly fix or CHALLENGE-clear the SPEC
Section 9 owner-path mismatch for CSS templates, require the CSS delimiter
microbench before redress, and bind the W4 gate to strict equality plus the
W1b-2b lightningcss report. If the isolated microbench is not positive, this
route should be recorded as a measured reject rather than patched into Track 1.
