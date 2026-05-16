# SK-V6 A1: asmjson Architecture and Generalization

Date: 2026-05-15.
Workspace read-only: `/Users/mkbabb/Programming/bbnf-lang`.
Output artifact: `/tmp/skv6-A1-asmjson-generalization.md`.

## Summary

asmjson is best read as two separate things:

1. A JSON-specific high-throughput parser architecture: 64-byte byte-class
   masks, direct-threaded state dispatch, explicit bounded object/array stack,
   and either a SAX-style event sink or a flat DOM/tape.
2. A non-strict comparator for BBNF's current skinny gates: its own docs say it
   accepts all C0 controls as whitespace and does not scan string bodies for
   unescaped controls. Local SK-V5/SK-V6 notes already classify current asmjson
   evidence as permissive/flaw-probe, not a strict SOTA target.

The transferable BBNF lesson is not "add an asmjson mode" and not "add a
directive." The transferable lesson is to lower a grammar plus output-schema
facts into a tight event-consumer automaton over the accepted event stream.
BBNF already has the right surface: `TapeEmit`, `DirectBuild { shape, fields }`,
`SinkOnlyProgram`, and host/API direct schema facts. The remaining work is to
generalize byte-class arithmetic and field/source facts without JSON rule-name
switches.

## 1. What asmjson Actually Does

Upstream crate/repo facts:

- The upstream crate is `asmjson` 0.2.6, repository
  `https://github.com/atomicincrement/asmjson`, local checkout
  `/tmp/asmjson-research` at commit `3d6965d`.
- The README describes it as experimental/research, not production-ready, with
  incomplete test coverage and unaudited hand assembly
  (`/tmp/asmjson-research/README.md:14-18`; docs.rs lines 92-94).
- Public safe entry points choose AVX-512BW or SWAR once:
  `dom_parser()` returns a DOM parser and `sax_parser()` returns a SAX parser;
  both use `is_x86_feature_detected!("avx512bw")` for the zmm path
  (`src/lib.rs:530-550`, `:575-610`).
- Portable `parse_to_dom` and `parse_with` use the Rust SWAR classifier and
  parser (`src/lib.rs:395-414`, `:613-623`, `:657-1080`).
- Unsafe x86-64 entry points `parse_to_dom_zmm` and `parse_with_zmm` call
  hand-written AVX-512BW assembly and must only be used after CPU feature
  verification (`README.md:42-44`, `:199-207`; `src/lib.rs:416-516`,
  `:626-655`).

Architecture:

- The core parser is a nine-state JSON DPDA: `ValueWhitespace`, `StringChars`,
  `KeyChars`, `KeyEnd`, `AfterColon`, `AtomChars`, `ObjectStart`,
  `ArrayStart`, and `AfterValue` (`src/lib.rs:225-254`; `doc/dev.md:14-23`).
- It classifies up to 64 bytes into four masks: whitespace, quotes,
  backslashes, and atom delimiters. The SWAR classifier defines whitespace as
  byte `<= 0x20`, quote equality, backslash equality, and delimiter as
  whitespace/comma/close-brace/close-bracket (`src/lib.rs:1182-1278`).
- The assembly path computes the same masks with AVX-512BW byte compares,
  `kmovq`, and `korq`, then uses `tzcnt` to jump to the next relevant byte
  (`asm/x86_64/parse_json_zmm_sax.S:244-289`, `:301-305`,
  `:391-405`; same shape in `parse_json_zmm_dom.S:272-292`).
- State dispatch is direct-threaded: state labels jump directly, with `r10` as
  the target state and `r11` as the EOF handler when a new chunk is fetched
  (`asm/x86_64/parse_json_zmm_sax.S:73-77`, `:216-224`, `:288-295`).
- The nesting stack is explicit and bounded at 64 frames
  (`src/lib.rs:262-270`; assembly `MAX_JSON_DEPTH` at
  `parse_json_zmm_sax.S:146-151` and stack frame state at `:94-108`).
- Strings and keys are bulk-scanned by `quote | backslash` masks. Backslash
  parity decides whether a quote closes the string (`src/lib.rs:724-763`,
  `:766-795`; assembly `parse_json_zmm_sax.S:391-475`, `:487-520`).
- Atoms are consumed until delimiter and then validated/mapped as
  `true`/`false`/`null` or a JSON number (`src/lib.rs:279-334`, `:366-389`,
  `:859-912`).

Output planes:

- `parse_with` / `parse_with_zmm` drive a SAX-style `Sax` trait. The sink
  receives null, bool, number slice, string slice, raw escaped string/key
  slices, and container begin/end events (`src/sax.rs:5-46`). The zmm SAX path
  calls a stable C-layout vtable (`src/lib.rs:16-42`, `:56-188`).
- `parse_to_dom` / `parse_to_dom_zmm` build a flat `Dom` of 16-byte `DomEntry`
  values. Entries encode kind in the high four bits plus string length or
  container end-index; strings borrow source bytes unless escaped, where the
  decoded string is boxed (`src/dom/mod.rs:9-60`, `:394-410`, `:449-514`).
- The DOM supports O(1) subtree skip by storing the matching close index in
  `StartObject` / `StartArray` entries (`src/dom/mod.rs:394-403`, `:550-559`).
- Upstream benchmarks are synthetic 10 MiB workloads and measure parse plus
  full traversal of string values/object keys, not BBNF's 17-corpus strict
  retained parse gate and not the `real_typed_struct` owned-output plane
  (`README.md:48-60`, `:115-123`; `benches/parse.rs:9-70`,
  `:128-180`).

Strictness caveats:

- RFC 8259 allows only four whitespace bytes: space, horizontal tab, line feed,
  and carriage return; strings must escape quotation mark, reverse solidus, and
  control characters U+0000 through U+001F
  (`https://www.rfc-editor.org/rfc/rfc8259`, lines 211-218 and 335-382).
- asmjson deliberately treats every byte `< 0x20` as whitespace and does not
  scan string contents for unescaped controls (`README.md:209-222`; docs.rs
  lines 212-215).
- The Rust API accepts `&str`, so callers normally arrive with UTF-8 already
  admitted by Rust. The parser itself is not a byte-level UTF-8 validator, and
  the zmm trampolines use `from_utf8_unchecked` when passing raw string slices
  to sinks (`src/lib.rs:79-109`). For BBNF byte-input gates, that is not the
  same strictness plane as scan-boundary UTF-8 validation.
- Local SK docs agree: asmjson SWAR is permissive, current refreshed asmjson
  rows are synthetic flaw probes, and asmjson/RapidJSON rows must be rendered
  with permissive/no strictness metadata when included
  (`GRAND-SYNTHESIS-SK-V5.md:135-143`; `SK-V6-COHORT/skv6-R5-sidecar-refresh.md:44-51`,
  `:116-144`; `skinny/RESULTS.md:224`).

Target ISA assumptions:

- The fast assembly path is x86-64 AVX-512BW, described by upstream as Ice
  Lake+ on Intel and Zen 4+ on AMD, not available on other architectures
  (`README.md:100-103`).
- The assembly also uses mask registers and BMI-style `tzcnt` heavily
  (`parse_json_zmm_sax.S:264-285`, `:305`, `:397`, and many later state
  seeks). The safe dispatch checks `avx512bw`; the effective fast-path silicon
  assumption is therefore AVX-512BW-class x86-64 with the usual accompanying
  bit-manipulation support.
- On Apple M-series, asmjson's assembly path is unavailable. The only runnable
  path is SWAR/u64, and local SK-V5 explicitly demotes asmjson as an ARM close
  condition while keeping it as a possible x86 successor architecture
  (`GRAND-SYNTHESIS-SK-V5.md:281-304`).

## 2. Ideas That Transfer to BBNF Without New Directives/BIR Variants

1. Event-sink direct materialization.
   asmjson's `Sax` path is an event stream into a caller-supplied sink. BBNF's
   equivalent is existing `SinkOnlyProgram` lowered from BIR `DirectBuild`.
   The receiver is already present: `real_typed_struct` now admits a generated
   typed DirectBuild path from host/API output-schema facts, with no BBNF
   directive and no new BIR variant (`skinny/REDRESS.md:1944-1952`).

2. Flat accepted-event substrate with multiple materializations.
   asmjson DOM and SAX are two outputs of the same parser. BBNF's controlling
   line is the same: retained tape and direct `SinkOnly` are materializations
   of the same accepted typed event stream, not second parsers or second trees
   (`skinny/REDRESS.md:126-132`, `:1233-1239`; `R3d:22-27`).

3. Chunk classification plus `ctz/tzcnt` seek.
   The arithmetic pattern transfers as a primitive family: classify a chunk
   into masks, then state code asks for "next byte in this grammar-defined
   interesting set." This can be lowered by existing SIMD/primitive admission
   machinery and same-wave consumers. It does not require a grammar directive.

4. Direct-threaded state dispatch as a backend lowering.
   A PC-as-state assembly or generated-code lowering can be a backend choice
   for a hot grammar/state cluster. It belongs under the existing lowerer/cost
   model and the guarded x86 `.asm` authoring route, not under a user-facing
   directive (`GRAND-SYNTHESIS-SK-V5.md:300-304`; `skinny/REDRESS.md:1233-1239`).

5. Explicit bounded stacks.
   asmjson's `frames_buf[64]`/`open_buf[64]` show that a tight parser can keep
   nesting state explicit and bounded. BBNF can express this as generated
   runtime resource bounds over grammar node/frame kinds. The JSON-specific
   fixed `64` should not become a BIR concept.

6. Output-plane honesty.
   asmjson's own benchmark note says parse-only would undercount work relative
   to lazy competitors (`README.md:115-119`). BBNF should keep the current
   split between retained parse, semantic digest stressor, and real typed
   output. The current `RESULTS.md` already reports strictness and output
   planes; `real_typed_struct` is not a rename of the old digest miss
   (`skinny/RESULTS.md:3-28`; `skinny/REDRESS.md:1985-1992`).

## 3. JSON-Specific Ideas and Grammar-Neutral Abstractions

JSON-specific in asmjson:

- State names and transitions: object start, object key, colon, array start,
  atom, after value.
- Whitespace shortcut: byte `<= 0x20`, which is not strict JSON and not
  grammar-neutral.
- String scanner: double quote as terminator, backslash as escape introducer,
  odd/even backslash parity, unescaped C0 controls not checked.
- Atom delimiter: whitespace/comma/`]`/`}`.
- Scalar atoms: `true`, `false`, `null`, and JSON number grammar.
- Output types: object, array, key, string, escaped string, number, bool, null.
- DOM shape: JSON start/end container entries with end-index skip and JSON key
  entries.
- Host output shape: JSON object-field lookup and dynamic object maps.

Grammar-neutral abstraction:

- Replace JSON states with generated automaton states derived from grammar
  positions: `ExpectLayoutOrToken`, `InDelimitedScalar`, `AfterScalar`,
  `AfterFieldLabel`, `AfterSeparator`, `BeforeClose`, etc. Names do not matter;
  the state machine is compiled from grammar/FIRST/FOLLOW and shape facts.
- Replace byte `<= 0x20` with a grammar-defined layout mask. For JSON strict
  mode this mask is exactly `{0x20, 0x09, 0x0A, 0x0D}` and C0 controls remain
  a separate invalid/control mask.
- Replace `quote | backslash` with per-scalar `terminator_mask |
  escape_intro_mask | invalid_body_mask`. JSON's parity rule is one instance
  of a general escaped-delimiter recognizer.
- Replace atom delimiter with the compiled FOLLOW set for the current scalar
  rule, encoded as byte masks or small table lookups.
- Replace `true`/`false`/`null` with `LiteralChoiceFact { expr_id, bytes,
  const_value }` and replace number parsing with an opaque
  `ScalarPrimitive { primitive, input_shape }` reference.
- Replace JSON object fields with `DirectFieldFact` over binding ids, field
  ids, cardinality, source refs, and materializer refs. JSON key names are
  schema/metadata, not generic crate logic.
- Replace fixed depth with a grammar/runtime resource bound selected by
  lowering and exposed in bench metadata if it affects correctness or security.

This is exactly the R3d finding: keep the existing `DirectBuild` node, make its
field/source facts specific enough for a generic lowerer, and use ids rather
than JSON rule-name strings (`SK-V6-COHORT/skv6-R3d-direct-generality.md:7-16`,
`:41-53`, `:126-135`, `:204-238`).

## 4. Exact Implications for Skinny and Global Specs

Skinny spec/reporting implications:

1. Keep asmjson out of strict SOTA target selection unless a strict row exists.
   If asmjson appears in `RESULTS.md`, render it as a permissive flaw probe
   with API/output plane named, matching current note requirements
   (`skinny/RESULTS.md:224`; `R5:44-51`, `:126-129`).

2. Keep strictness columns mandatory. The SK-V5 fix
   `Strictness | parse_utf8 | escape_complete | flaw_probe` is load-bearing:
   bbnf's retained parse is still `deferred / view-boundary / yes` in current
   rows, so ratios against strict sidecars are not strict-vs-strict wins
   (`GRAND-SYNTHESIS-SK-V5.md:128-150`; `skinny/RESULTS.md:3-21`).

3. Do not resurrect rejected side substrates. asmjson's mask seek does not
   justify structural-index parser prepasses, byte-class EventCursor wrappers,
   parser-local structural-mask cursors, generic SWAR whitespace, or sink-local
   decoded hash helpers. The skinny ledger keeps these non-canonical unless a
   future bench overturns them (`skinny/REDRESS.md:1212-1219`).

4. Preserve the current typed-output split. `direct_to_struct` /
   `semantic_full_digest_stressor` remains a stressor; `real_typed_struct`
   is the representative host/API typed-output premise. asmjson's SAX sink
   supports this split: event sinks should be judged on the output contract
   they actually build, not on a different maximal traversal
   (`skinny/RESULTS.md:25-45`; `skinny/REDRESS.md:1881-1886`,
   `:1985-1992`).

5. Generalize JSON direct facts through data, not directives. The smallest
   skinny cleanup remains: move JSON shape/direct facts into a data-only
   grammar-side fixture or generated sidecar, have extraction inject
   `DirectBuild` from resolved facts, and stop putting JSON rule-name switches
   in `passes` (`R3d:7-16`, `:29-35`, `:240-260`).

6. SIMD/scan specs should name two products: strict structural-only scan and
   parser-grade parse index. asmjson's classifier combines layout, quote,
   escape, and delimiter facts for parser control; BBNF should keep the
   structural floor product cheaper and separate unless a measured row pays for
   parser facts (`skinny/REDRESS.md:984-999`, `:1081-1089`).

7. The JSON whitespace contract in `restart/skinny/COMPILER.md` should remain
   caller-owned trailing layout. asmjson's "state eats whitespace and next
   dispatch byte" is a lowering optimization, not a grammar semantics change
   (`skinny/REDRESS.md:1001-1013`).

Global spec implications:

1. Add/clarify a grammar-neutral "chunk-classified automaton lowering" concept:
   classify bytes into grammar-defined layout/terminator/escape/delimiter/
   invalid masks, use `ctz` to seek, and compile state transitions from
   grammar facts. This is a backend lowering option, not a BIR variant.

2. Preserve `DirectBuild` as the sole direct materialization hook. If more
   expressiveness is needed, enrich `DirectBuildField`/source/materializer
   payloads and `ShapeFacts.direct_builds`; do not add `DirectBuildObject`,
   `DirectBuildArray`, `@direct`, `@shape`, or equivalent user directives
   (`R3d:22-27`, `:49-53`, `:126-135`).

3. Treat host/API schemas as external output contracts consumed by
   `DirectBuild` facts. Candidate 12's admitted schema-source path is the
   global receiver: schema from host/xtask/API code, not BBNF grammar syntax
   and not a benchmark-private parser (`skinny/REDRESS.md:1931-1940`,
   `:1944-1952`, `:1959-1965`).

4. ISA-specific code belongs behind primitive admission and backend selection.
   The x86 route may eventually use per-grammar `.asm` with AVX-512BW/BMI-like
   features and checkasm coverage. ARM/M-series requires NEON/SVE-native
   kernels and cannot cite asmjson's zmm path as evidence (`GRAND-SYNTHESIS-SK-V5.md:292-304`).

5. Global reporting must keep output plane and strictness attached to every
   comparator row. At minimum: structural scan, retained typed root over offset
   tape, SAX/SinkOnly event sink, generated typed DirectBuild owned output, DOM
   sidecar, and typed serde sidecar are different planes.

## 5. Sources and Paths Cited

Local BBNF/SK docs:

- `/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v5/SYNTHESIS.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v6/research/skv6-R3d-direct-generality.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v6/research/skv6-R5-sidecar-refresh.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v6/research/skv6-schema-C-redress-gates.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v6/research/skv6-R1g-real-typed-struct-workload.md`

asmjson primary sources:

- `https://github.com/atomicincrement/asmjson`
- `https://docs.rs/asmjson/latest/asmjson/`
- `/tmp/asmjson-research` local checkout, commit `3d6965d`
- `/tmp/asmjson-research/README.md`
- `/tmp/asmjson-research/doc/dev.md`
- `/tmp/asmjson-research/src/lib.rs`
- `/tmp/asmjson-research/src/sax.rs`
- `/tmp/asmjson-research/src/dom/mod.rs`
- `/tmp/asmjson-research/asm/x86_64/parse_json_zmm_sax.S`
- `/tmp/asmjson-research/asm/x86_64/parse_json_zmm_dom.S`
- `/tmp/asmjson-research/benches/parse.rs`

JSON strictness reference:

- RFC 8259, `https://www.rfc-editor.org/rfc/rfc8259`

End of report.
