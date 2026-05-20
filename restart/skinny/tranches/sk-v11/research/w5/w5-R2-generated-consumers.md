# SK-V11 W5 R2 - Generated String/Key Consumers

Date: 2026-05-20.
Lane: W5 Phase 1 research R2.
Scope: generated JSON direct and typed string/key consumers; no source edits.
Output: this file.

## Question

Identify admissible generated string/key caller candidates for W5, plus the
regeneration requirements, Track 1/Track 2 parity hooks, and owner-path risks.
SPEC Section 9 selects one scalar span shape, one string/key caller, a cap, and
at most two target rows; it requires a scalar span oracle with offsets and
decode-needed status, one generated direct/typed string/key consumer, strict
parity if SIMD is used, Unicode residual monitoring, and no decoded scratch,
retained string side table, retained `StringBlock16` wrapper, primitive-only
production, or 64-byte retained scan (`SPEC.md:540`, `SPEC.md:559`,
`SPEC.md:565`, `SPEC.md:573`, `SPEC.md:580`).

## Candidate 1 - Generated Direct Key/String Source

Primary caller: generated JSON direct `parse_string_direct`, consumed through
`parse_object_direct` object keys and through root/object/array string values.
It is emitted from `skinny/crates/codegen/src/sink_direct.rs` into
`skinny/crates/runtime/src/grammars/json/generated.rs`.

- Generated root, object, and array string values call `parse_string_direct`
  and then `sink.string_source`, `sink.object_string_source`, or
  `sink.array_string_source` with a raw slice plus `needs_unescape`
  (`generated.rs:440`, `generated.rs:480`, `generated.rs:520`).
- Object keys call the same `parse_string_direct` and then `sink.key_source`
  (`generated.rs:562`).
- The current direct fast path is `match_tiny_plain_string_direct` with cap 8,
  falling back to `match_string_at_quote_trusted_utf8`; it returns content
  offsets via `content_start`/`content_end` and carries `span.needs_decode()`
  (`generated.rs:166`, `generated.rs:610`, `generated.rs:624`).
- Codegen owns the same call sites in `sink_direct.rs`, so a generated-source
  intervention must update the renderer, not hand-patch `generated.rs`
  (`sink_direct.rs:138`, `sink_direct.rs:178`, `sink_direct.rs:218`,
  `sink_direct.rs:265`, `sink_direct.rs:319`).

Admissible row pairing: select at most two W5 direct rows from the named
string/key-heavy set. The best first pair is `twitter/direct_to_struct` and
`github_events/direct_to_struct`: both are W5-listed rows, both exercise object
keys and string fields, and both have strict direct floors already declared
(`SPEC.md:123`, `SPEC.md:125`, `SPEC.md:573`). `update_center/direct_to_struct`
is a strong alternate if CHALLENGE prefers larger map/key pressure
(`SPEC.md:126`).

## Candidate 2 - Generated Typed Map/String Parser

Secondary caller: generated typed `DirectParser::parse_string`, consumed through
struct field-key dispatch, scalar string fields, and map-entry keys. The best
W5-local hook is `parse_w5_map_entry_root_probe`, with
`parse_w5_array_root_probe` as the simpler array-root comparison.

- `parse_w5_array_root_probe` parses a vector of `W5ArrayEvent`; each object
  key uses `parser.parse_string()`, and the `actor` field uses
  `parse_option_scalar_string` (`generated_real_typed.rs:1039`,
  `generated_real_typed.rs:1054`, `generated_real_typed.rs:1063`).
- `parse_w5_map_entry_root_probe` parses map entries; each map key is retained
  as `Cow<'i, str>`, and each value's `label` field also uses
  `parse_option_scalar_string` (`generated_real_typed.rs:1153`,
  `generated_real_typed.rs:1160`, `generated_real_typed.rs:1165`,
  `generated_real_typed.rs:1084`, `generated_real_typed.rs:1106`).
- Typed `DirectParser::parse_string` has a cap-32 tiny plain-string path,
  falls back to `match_string_at_quote_trusted_utf8`, and decodes only when
  `span.needs_decode()` is true (`generated_real_typed.rs:1649`,
  `generated_real_typed.rs:1653`, `generated_real_typed.rs:1660`,
  `generated_real_typed.rs:1666`).
- Typed skip paths are separate and larger-cap: `skip_string_raw` uses
  `skip_plain_string_end` with cap 96. Treat that as a guard or follow-up, not
  the selected W5 caller unless CHALLENGE explicitly chooses skip-heavy ignored
  fields (`generated_real_typed.rs:1796`, `generated_real_typed.rs:1825`).

Admissible use: this is a typed parity and micro-proof candidate rather than a
direct row admission by itself. If selected, pair it with direct row measurement
above and use the W5 probe tests to prove typed key/value string behavior did
not diverge.

## Regeneration Requirements

- Direct runtime output must be regenerated from `sink_direct.rs` and the JSON
  template path with `cargo xtask regen-json`; staleness is checked by
  `cargo xtask check-json` (`xtask/src/main.rs:121`, `xtask/src/main.rs:128`).
- Typed output must be regenerated from `typed_direct.rs` and
  `xtask/src/real_typed_schema.rs` with `cargo xtask regen-real-typed`;
  staleness is checked by `cargo xtask check-real-typed`
  (`xtask/src/main.rs:136`, `xtask/src/main.rs:144`).
- The W5 typed roots are schema-owned in `xtask/src/real_typed_schema.rs`, not
  handwritten in `generated_real_typed.rs` (`real_typed_schema.rs:43`,
  `real_typed_schema.rs:48`, `real_typed_schema.rs:298`,
  `real_typed_schema.rs:307`).
- Current codegen emission is still JSON-profile gated by
  `json_provider::ensure_runtime_profile`, so a W5 generic/codegen change must
  either stay JSON generated-output-only or carry the SPEC-required non-JSON
  string/literal proof (`codegen/src/lib.rs:108`, `codegen/src/lib.rs:146`,
  `json_provider.rs:4`, `SPEC.md:582`).

## Parity Hooks

- Direct Track 1 is `direct_struct::track1_digest`, which calls
  `runtime::generated_json::parse_direct` and `JsonDigestSink`
  (`direct_struct.rs:401`).
- Direct Track 2 is `direct_struct::track2_digest`, a local hand parser
  (`direct_struct.rs:408`). Its string path independently consumes
  `match_string_at_quote_trusted_utf8`, decodes on `needs_decode`, and has its
  own tiny plain-string cap (`direct_struct.rs:541`, `direct_struct.rs:549`,
  `direct_struct.rs:557`).
- Direct parity is `assert_direct_struct_parity`: exact Track 1/Track 2 digest
  equality plus serde/sonic shape equality (`direct_struct.rs:420`).
- Typed parity is `assert_real_typed_parity`: generated typed Track 1 versus
  serde-backed Track 2, serde, and sonic checksums (`real_typed_struct.rs:348`,
  `real_typed_struct.rs:390`, `real_typed_struct.rs:450`).
- W5-local typed probes already compare generated output to serde and sonic for
  both array-root string fields and map-entry keys (`real_typed_struct.rs:866`,
  `real_typed_struct.rs:883`).
- The Criterion harness runs parse parity, direct digest parity, and real typed
  parity before row measurement, then benchmarks `track1_direct_to_struct`,
  `track2_direct_to_struct`, sonic/serde direct, and real typed guard rows
  (`json_parity.rs:15`, `json_parity.rs:181`, `json_parity.rs:251`).

## Owner-Path Risks

- Hand-patching `generated.rs` or `generated_real_typed.rs` is inadmissible;
  generated output may be committed only as regenerated output from named inputs.
- `parse-that-regex` changes are generic. They need the Section 9 non-JSON
  string/literal proof and must avoid JSON-only quote, slash, `\u`, surrogate,
  or retained semantic string policy in generic code.
- A SIMD body under `bbnf-simd/src/aarch64/string_block.rs` needs strict parity,
  caller microbench coverage, REDRESS 106 material differential, and no retained
  `StringBlock16` wrapper or 64-byte retained scan in production.
- The direct Track 2 hand parser shares low-level regex helpers with Track 1.
  That is acceptable for current parity, but a scanner-policy change should keep
  serde/sonic comparators and exact direct Track 1/Track 2 digest equality in
  the admission evidence.
- Unicode rows remain residual unless selected. A plain-string W5 intervention
  must not claim `unicode_escapes`, `unicode_mixed`, or `y_string_unicode` as
  admitted guards without the SPEC-required Unicode treatment.
