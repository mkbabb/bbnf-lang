# SK-V9 Wave W3 CHALLENGE V3: Streaming Union Cursor

Disposition: ACCEPT with binding redress constraints.

Inputs: `restart/skinny/tranches/sk-v9/research/skv9-W3-plan-v2.md`;
`restart/skinny/tranches/sk-v9/research/skv9-W3-research-v2.md`;
`skinny/REDRESS.md` Item 96; `restart/skinny/tranches/sk-v9/SPEC.md`
Section 6.

The revised plan is authorized for W3 source redress. The accepted material
differential from REDRESS 96 is narrow: W3 may consume a parse-frame-local
streaming structural cursor, but may not allocate or move a full structural
position vector into `ParserState`, and may not discover delimiters with the
old scalar path before asking the cursor to confirm them.

## CH1 Correctness

Accepted. A streaming cursor can be the W3 structural producer only if it is
monotonic, consumes the same JSON structural positions as `scan_structurals`,
and handles string open/close ownership with the same quote/escape semantics.
The parser still owns scalar anchors because numbers and literals are not JSON
structural alphabet members.

Binding tests:

- The cursor parity harness must compare streaming cursor positions against
  `scan_structurals` and the scalar cursor on generated strings and at least
  one corpus fixture.
- The generated parser must consume string opening and closing quotes through
  the cursor; `match_string_at_quote_trusted_utf8` remains the W3 string
  validator.
- Invalid-input errors may still inspect source bytes for diagnostics, but
  retained event-class recovery may not.

## CH2 Generality And Lock 14

Accepted. The streaming cursor is not a retained parser-owned structural
cursor if all of these hold:

- It is private to JSON parse execution, retained nowhere after `parse`
  returns, and exposed through no public substrate API.
- It stores only transient scan state: input reference, cursor/base, masks, and
  quote/escape carry.
- Generic tape code stores opaque packed class nibbles and never interprets
  JSON event semantics.

Extending `JsonEventGrammar::STRUCTURAL_CLASS_COUNT` is accepted as an event
grammar-domain correction for JSON retained events. It does not enlarge the
SIMD structural alphabet.

## CH3 Regression And REDRESS

Accepted with REDRESS 96 guards. The failed shape is not reopened if these
grep gates pass:

```text
rg -n 'into_positions\(|structural_positions' skinny/crates/runtime/src/grammars/json
rg -n 'consume_structural' skinny/crates/runtime/src skinny/crates/codegen/src
```

Both must return zero after redress. A helper that first calls
`skip_ascii_whitespace` to discover a delimiter and then validates the cursor
is also a REDRESS 96 reopen even if those grep gates pass.

The W10b six-row maintain block remains binding. Any one W10b miss rejects the
wave, regardless of cursor parity or local correctness.

## CH4 Cost

Accepted. The CHALLENGE grants the SPEC Section 6 redress extension to
≤110 minutes. The plan touches tape representation, JSON scanner cursor,
generated parser lowering, templates, Track 2/parity, proof witness, and
measurement; this is the HIGH-risk W3 case.

## CH5 Hidden Coupling

Accepted with owner enforcement:

- `bbnf-bench/src/track2/json.rs` and `bbnf-bench/src/parity.rs` must be
  updated in the same redress because Track 2 constructs `JsonRoot` values with
  `TapeBuilder`.
- `ValueRef` layout and lifetime semantics must remain unchanged.
- Codegen templates and checked-in generated JSON output must match.
- No `bbnf-simd::StructuralIndex` public layout change is authorized by W3 V2.

## CH6 Anti-Paper-Close

Accepted with no partial admission. A class-consumer-only W3 is rejected for
this wave because it cannot move `parse_only` rows. The redress must wire the
streaming cursor into the generated parse path and then measure the SPEC W3
must-improve rows plus W10b floors. If correctness is green but Mbps floors
miss, the wave records a REDRESS reject and restores source, as REDRESS 96 did.

Required evidence on PASS or FAIL:

- Cursor parity test output.
- Runtime, bbnf-bench parity/materialization/track2, codegen, and proof checks.
- `rg` deletion/integration greps named above.
- Native Criterion capture with `RUSTFLAGS="-C target-cpu=native"`.
- Diagnostic split for scan-only, grow-only parse, capacity-plan-C parse, and
  streaming-cursor parse.

## Redress Authorization

Proceed to W3 source redress under `skv9-W3-plan-v2.md`. Any need to edit
outside the accepted owner paths returns REVISE before source edits.
