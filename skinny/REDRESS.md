# Skinny Redress: Mbps, Tape Materialization, and Spec Amendments

Date: 2026-05-12.

This note records the implemented redress after the skinny prototype was brought
closer to the restart skinny/full contracts. The measured findings are now also
recorded in the runnable prototype surfaces under `skinny/`; the guarded
`restart/` authority surfaces remain outside this implementation pass.

## Current Bench Fact

The gate report is canonicalized to Mbps. The current measured authority is
`skinny/RESULTS.md`, regenerated after the SK-V5 Wave 2 generated SinkOnly
implementation and full per-wave Criterion run. It records **overall outcome
N-direct / NoGo**.

Two blockers must stay separate:

1. The parse/tape plane still has hard G rows across the expanded corpus.
   Track 1 and Track 2 move together on those rows, so the miss is
   substrate/runtime shape rather than generator overhead alone. The passing
   parse rows are currently `canada`, `mesh`, `marine_ik`, and `numbers`.
2. The direct-to-struct workload is correctness-green
   (exact generated Track 1 / hand Track 2 digest equality; sonic-rs and
   serde_json shape parity) and now uses generated SinkOnly for Track 1. The
   prior bench-private SinkParser table is superseded. Zero of 17 direct rows
   currently pass the `1.10x` sonic-rs time slack.

The earlier original-triad pass remains useful historical evidence, but it is
not the current close condition. The full 17-fixture gate plus
`direct_to_struct` is now the binding authority.

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | Track 1 / sonic | Track 2 / sonic |
|---|---:|---:|---:|---:|---:|
| twitter | 6048 | 6065 | 21028 | 28.8% | 28.8% |
| citm_catalog | 22323 | 22450 | 25354 | 88.0% | 88.5% |
| canada | 17829 | 17355 | 13981 | 127.5% | 124.1% |
| mesh | 13155 | 12989 | 11832 | 111.2% | 109.8% |
| random | 3253 | 3258 | 15521 | 21.0% | 21.0% |
| marine_ik | 12983 | 12856 | 10023 | 129.5% | 128.3% |
| numbers | 18811 | 18984 | 13728 | 137.0% | 138.3% |
| unicode_mixed | 2377 | 2407 | 18281 | 13.0% | 13.2% |
| unicode_basic | 2559 | 2559 | 15946 | 16.0% | 16.0% |

Structural scan is not the current blocker: the `canada` structural-only scan
reports 69976 Mbps against a 40000 Mbps floor.

Direct-to-struct is now the binding workload blocker:

| Corpus | Track 1 direct Mbps | Track 2 direct Mbps | sonic-rs direct Mbps | Track 1 / sonic direct |
|---|---:|---:|---:|---:|
| twitter | 4765 | 4758 | 11630 | 41.0% |
| citm_catalog | 14889 | 15236 | 21475 | 69.3% |
| canada | 7249 | 7656 | 12182 | 59.5% |
| mesh | 6308 | 6501 | 9568 | 65.9% |
| marine_ik | 7276 | 7621 | 8853 | 82.2% |
| numbers | 8073 | 8601 | 12918 | 62.5% |
| unicode_mixed | 1648 | 1654 | 6421 | 25.7% |
| unicode_basic | 1991 | 1980 | 7184 | 27.7% |

Lazy tape materialization is now reported per corpus:

| Corpus | Offsets | Logical offset bytes | Flag bytes | Allocated tape bytes | Object opens | Array opens | Closes | String quotes | Numbers | Literals |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| twitter | 29573 | 118292 | 1560 | 133632 | 1264 | 1050 | 2314 | 18099 | 2109 | 4737 |
| citm_catalog | 85035 | 340140 | 5 | 524312 | 10937 | 10451 | 21388 | 26604 | 14392 | 1263 |
| canada | 223236 | 892944 | 0 | 1048576 | 4 | 56045 | 56049 | 12 | 111126 | 0 |

## Implemented Redress

1. Report units are Mbps.

   `crates/bbnf-bench/src/report.rs` converts Criterion nanosecond estimates
   with `bytes * 8000 / ns` and renders Track 1, Track 2, sonic-rs, and both
   Track-to-sonic ratios. `crates/bbnf-bench/src/bin/gate.rs` renders the
   structural floor as Mbps as well. `RESULTS.md` is regenerated from the gate.

2. Parse-index and structural-scan products are split.

   `bbnf-simd` now exposes `scan_json_structurals` for the structural-only
   bench and `scan_json_parse_index` for parser-grade prefilter/probe work. The
   parse index carries structural offsets and string escape/control candidates;
   the structural-only scanner does not pay that extra parser cost. A duplicate
   structural-byte column was measured and removed after it improved all six
   Track 1/Track 2 parse rows by roughly 3-6%.

3. Track 1 and Track 2 consume the same one-buffer tape builder.

   Generated Track 1 and hand-coded Track 2 now write source-verified events
   directly through `runtime::tape::TapeBuilder`. `ParserState.structural_offsets`
   and the string escape/control side vectors are gone from the parser state;
   the sealed tape is one `Box<[u32]>` offset stream plus one packed `Box<[u8]>`
   flag stream.

4. Parser whitespace materialization was corrected.

   `parse_value` no longer performs a trailing whitespace skip. Whitespace is
   consumed by the caller boundary: root, object separators, array separators,
   and closing-token checks. `consume_structural` also avoids a second input
   byte-vector load; structural byte checks read `input[offset]` from the
   original source.

5. Tape/direct-to-struct remains one substrate.

   The direct view layer is a typed projection over sealed tape offsets and
   `ValueRef`, not a parallel struct tree. Object/array/pair/string/number/
   bool/null wrappers point back into the tape; strings and numbers remain
   borrowed spans with lazy materialization. The former eager `TapeToken` carrier
   has been removed from the skinny runtime.

6. Payload arena remains cold on JSON.

   The runtime test path asserts zero payload bytes, zero writes, and zero
   allocations for JSON parse/projection. Strings with escapes carry the packed
   `OffsetFlags::HAS_ESC` bit and allocate only when `JsonString::as_str()` is
   called.

7. BIR now carries the materialization events it claims to test.

   `passes::extract` wraps JSON materialized rules with `SpanMark`,
   `TapeEmit`, `DirectBuild`, and `Return` nodes, and the pass tests assert
   those events are present. Codegen still lowers through the skinny fixed
   template, but the Backend IR is no longer missing the tape/direct-build
   contract entirely.

8. Bench metadata no longer hardcodes payload counters.

   The criterion harness parses each fixture through Track 1 and Track 2 before
   writing row metadata and records the observed payload arena write/allocation
   counters. The gate fails schema validation for bbnf rows whose counters are
   missing or non-zero, and `RESULTS.md` publishes the per-corpus 0/0 counters.

9. Tape materialization is now a report artifact.

   `bbnf-bench::materialization` derives offset economy from the sealed
   `JsonRoot` tape after parsing. The gate publishes offset count, logical
   offset bytes, allocated offset bytes, both offset/input ratios, payload
   bytes, and node-kind counts. This does not perturb the hot path.

10. Masking probes are now a report artifact.

   The gate reads Criterion estimates for host-call dispatch, eager string
   decode, alternate scalar plan, optional PEXT plan, and cold first parse.
   `RESULTS.md` renders Mbps, ns/iter, Track 1 ratio, and a signal column so
   Lens L does not depend on hidden Criterion directories. The original
   `alternate_dispatch_table_plan` row duplicated Track 1 and is now marked
   invalid until a distinct implementation exists.

11. Generated runtime files now own the JSON API definitions.

   `runtime::grammars::json` aliases the generated module. `view.rs`,
   `value.rs`, and `visitor.rs` contain the concrete root, document,
   value/projection, token, error, and visitor definitions instead of
   re-exporting an inlined runtime copy. The old inlined JSON module was removed
   from `runtime/src/lib.rs`.

12. JSON number and whitespace scanners were tightened.

   `parse-that-regex` now uses direct slice indexing with explicit length
   guards for `skip_json_whitespace` and `match_json_number`. This is shared by
   Track 1 and Track 2. Targeted Canada parse benches improved by roughly 9.7%
   for Track 1 and 7.6% for Track 2 before the later parse-index and sealing
   changes. The current full regenerated gate reports Canada Track 1 at 16264
   Mbps and Track 2 at 16217 Mbps.

13. Close-token elision is now canonical for JSON.

   This was the last accepted eager-token perturbation before lazy mode. JSON
   now stores close offsets in the lazy tape because direct views need container
   boundaries, but it still emits no `TapeToken` close stream on the JSON path.

14. The parser-grade structural byte vector was removed.

   The earlier parse index carried offsets plus string escape/control
   candidates. Wave 1 then removed `ParserState.structural_offsets` entirely:
   `consume_structural` validates from source and writes directly into
   `TapeBuilder`. Targeted eager-track benches improved materially; the final
   lazy full bench is recorded in `RESULTS.md`.

15. Tape sealing is private-Vec semantic sealing.

   This remains the eager-mode sealing record. JSON lazy mode now seals offsets
   into `Box<[u32]>` plus packed `Box<[u8]>` flags; allocated tape bytes are
   logical offset bytes plus the flag stream in the current report.

16. Pair-token fusion was measured and rejected.

   A pair-token-free object projection reduced token count but regressed Track
   1 on twitter and canada and did not deliver a clean substrate win. The
   canonical JSON tape keeps explicit key/value cursor pairing in the view layer
   until a different representation beats the current Mbps gate.

17. Dispatch-table alternate was audited and rejected as a signal.

   The reported `alternate_dispatch_table_plan` rows were not a valid alternate:
   the Criterion body called the canonical generated parser. A real 256-entry
   function-pointer dispatch table was implemented in both Track 1 generation
   and Track 2, measured, and reverted because it regressed key corpora instead
   of producing a stable win. The canonical lowering remains Rust `match`
   dispatch and the gate now reports this probe as invalid rather than reading
   stale Criterion rows.

18. Skipless 12-byte tape tokens were measured and rejected as canonical.

   A narrow token shape (`kind + flags + start + end`) removed the stored
   `payload_or_skip` column and derived subtree skips from spans at view
   traversal time. It kept correctness and reduced logical tape bytes, but
   targeted track benches were mixed: twitter regressed, citm improved, and
   canada remained within noise. The canonical substrate stays with the
   16-byte aligned token and stored sibling skip until a lazy-offset tape
   replacement is implemented and beats the gate.

19. Host-call evidence split dispatch from eager decode.

   The isolated dispatch microbench passes comfortably, so `CallHost`
   indirection is not the problem. The gross eager-decode rows are MASKING on
   the current full report, which means parse-time decoding every string is not
   SOTA-faithful for JSON. The host-fn-free skinny remains faithful only for a
   V1 JSON path that keeps string decode lazy in the substrate/view layer; a
   parse-time `decode_json_string_to_arena` grammar needs an explicit SOTA
   concession or a lazy lowering amendment.

20. Lazy-offset tape-union migration was implemented and measured.

   JSON Track 1 and Track 2 now seal a lazy offset tape through
   `TapeBuilder`: no `TapeToken` stream is emitted on the JSON path, the public
   tape stores u32 offsets plus packed per-offset flags, and direct views compute
   node kind from `source[offsets[cursor]]`. Separators are now stored as part
   of the structural projection, eliminating the parser sidecar while preserving
   view traversal. The immediate post-migration gate was still outcome G
   against sonic-rs, with twitter Track 1 at 14810 Mbps; later sparse-flag and
   parser hot-path wins moved the historical triad to pass; the later expanded
   corpus has current parse G rows, and the full gate is `N-direct / NoGo`.

21. Lock 15 release-profile discipline is enforced in the skinny workspace.

   `[profile.release]` now uses `lto = "fat"`, `codegen-units = 1`, and
   `panic = "abort"`. The verbose release build shows rustc invocations carrying
   `-C lto=fat`. Hot generated JSON parser functions are emitted with
   `#[inline(always)]`, and `passes::recognizers::hot_path` records the
   cost-model-derived hot-rule fact for the later real lowerer.

22. `bbnf-simd` replaced the runtime scanner dependency surface.

   Runtime and bench crates now depend on `bbnf-simd`, with external parity
   tests covering all one-byte inputs and the available JSON corpora. The old
   old scanner crate is no longer a skinny workspace member. Wave 2 still
   reported outcome G, but the structural-only `canada` scanner remains well
   above floor. The scanner floor is not the expanded-gate blocker.

23. Sparse flags and direct spare-capacity offset writes landed.

   The lazy tape now stores flag bytes only for offsets that need non-default
   flags and writes offsets directly into spare capacity before sealing. This
   removes the former byte-per-offset flag stream on low-escape corpora:
   twitter now reports 1560 sparse flag bytes, citm_catalog 5, and canada 0.

24. Parser hot-path wins landed without changing the substrate contract.

   The accepted wins are cold error paths, SWAR digit runs, SWAR plain-string
   scanning, fused comma/close delimiter consumption, newline-indent
   space-run skipping, parser split via `parse_value_at`, a short
   plain-string fast path, and Track 2 inline parity. These changes preserve
   the same lazy offset tape and view projection contract while moving the gate
   from prior outcome G to a passing historical triad. The expanded gate then
   exposed remaining SOTA-BEAT blockers.

25. Measured alternates remain rejected.

   Structural-index typed parser prepass, NEON no-escape string matcher,
   separator elision, generic SWAR whitespace skipper, 12-byte/width churn,
   and dispatch-table/function-pointer alternates were measured or audited and
   not retained. They either duplicated an existing signal, regressed key
   corpora, or failed to beat the direct hot-path changes above.

26. Bench auditability gates landed after the triad pass and before expanded
   parse-G and `N-direct / NoGo` classification.

   The compact report now renders all three competitor anchors, names the
   fastest `S` row used by the classifier, and reports Track 1 / S plus Track 2
   / S. SIMD scan benches persist parity-hash metadata, the gate rechecks the
   persisted hash against the scalar hash, peak RSS is sampled in one-shot
   subprocess probes for bbnf and the fastest `S` anchor, and `xtask
   check-conformance` exercises UTF-8 rejection, surrogate rejection,
   non-character acceptance, and float-bit parity over the expanded corpus
   manifest. This moved `bbnf-bench` and `xtask` above their old micro-budgets,
   so WORKSPACE.md redresses those local caps to 3,300 and 650 LOC after the
   direct-to-struct proof and `primitive-checkasm` became mandatory gate
   surfaces. The total skinny handwritten envelope is redressed to 32,000 LOC.

27. SK-V3 reprofile split the expanded blockers by mechanism.

   `random` and `unicode_escapes` are dominated by
   `runtime::generated_json::generated::parse_value_at`, so the next parser
   work is typed event cursor consumption over the tape projection, not string
   decode or another tape-width perturbation. `update-center` spreads across
   parse entry, sparse-flag capacity, and allocation growth, so builder
   capacity policy is a measured SOTA item. Profiles live under
   `skinny/profile/reprofile-2026-05-12/`.

28. SK-V3 Wave 0/1 closed SIMD parity and admitted the host aarch64 primitive
    kernels, but rejected active 16-byte tiny-string dispatch.

   Plan D (`GrowOnly`) is now the production capacity default. Strict
   `bbnf-simd` checkasm parity passes, including the adversarial
   `escape_mask_64` handoff sweep and the new aarch64 Class A/Class B
   primitive admission tests. The generic TBL tiny-string kernel and the
   TBL-driven `\uXXXX` decoder are implemented and parity-checked. When the
   16-byte tiny-string helper was routed into Track 1/Track 2, Criterion showed
   a real `twitter` regression of roughly 25% on both tracks, so the active
   parser remains on the 8-byte scalar tiny recognizer. Fresh profiles under
   `skinny/profile/reprofile-sk-v3-wave1/` show `parse_value_at` still
   dominating `random`, `unicode_escapes`, and `update-center`.

29. HEAD vocabulary state after `74406332` and `9eef728c`.

   The restart docs now canonize the two-layer primitive vocabulary: Layer 0 is
   the vendored macro substrate and Layer 1 is grammar-neutral `bbnf.asm`.
   HEAD contains the `bbnf.asm` skeleton plus the first end-to-end
   `BYTE_CLASS_FROM_EQ_SET_64` scalar/aarch64/x86/checkasm path. That is an
   admission and vocabulary milestone, not a SOTA-BEAT verdict change: the
   parse/tape plane still has G rows and the next measured target is
   `parse_value_at` / event-cursor consumption.

30. Direct-to-struct is now a throughput gate, not just a correctness proof.

   `bbnf-bench` records Track 1 sink-only direct, Track 2 sink-only direct,
   sonic-rs direct serde, and serde_json direct serde rows for every fixture.
   The old retained-tape view walk remains in `assert_direct_struct_parity` as
   an untimed parity oracle. Correctness passes by exact Track 1 / Track 2
   digest equality and sonic-rs / serde_json shape parity. This entry recorded
   the pre-Wave-2 bench-private SinkOnly table; entries 34 and 40 supersede
   its throughput interpretation. After generated Track 1 SinkOnly landed,
   zero of 17 rows pass the current `1.10` time slack, so the gate still
   appends outcome `N-direct / NoGo` when either bbnf direct track is slower
   than `sonic-rs * 1.10` in time.

31. Direct sink profiling moved the next blocker from view traversal to
    materialization leaves.

   `crates/bbnf-bench/src/bin/profile_direct.rs` records focused direct-sink
   profiles for Track 1, Track 2, sonic-rs, and serde baselines. The first
   samply pass on `twitter`, `numbers`, and `unicode_mixed` is summarized in
   `skinny/profile/direct-sink-2026-05-12/PROFILE-REPORT.md`; it showed
   duplicate UTF-8 validation, string/unescape loops, and exact number
   materialization as the hot leaves. The implemented redress removes the
   duplicate UTF-8 validation after `match_json_string_at_quote` has already
   validated the span, and moves integer/non-integer classification into
   `parse_that_regex::JsonNumberMatch` so integral spans do not pay a second
   scan. A direct `raw.parse::<f64>()` fast path was tested and rejected on
   parity: `canada` exposed float-shape mismatch against the serde oracle. The
   remaining direct work is therefore exact float materialization and
   Unicode/string decode quality inside `SinkOnly`, not another retained-view
   rewrite.

32. Gate status and budget-cliff handling are executable, not prose-only.

   `crates/bbnf-bench/src/bin/gate.rs` now exits from the worst measured
   verdict after writing `skinny/RESULTS.md`: local `xtask gate-json` hard-fails
   on `N-direct / NoGo`, while the explicit `--advisory` mode keeps CI
   throughput rows report-only and still hard-fails schema, parity, and SIMD
   parity correctness blockers. `xtask lint-loc` now emits
   `BBNF-BUDGET-CLIFF` when `bbnf-bench` enters the documented 3250-3300 LOC
   warning band; the current crate is 3278/3300 LOC, so the warning is live.

33. SK-V5 Wave 3: Class A `match_tiny_plain_string` NEON wiring is INVALIDATED
    as the parse-G fix; the corrected parse-G target is the NEON UTF-8
    codepoint pipeline.

   The aarch64 kernel at `crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs`
   is parity-green and grammar-generic, but cohort attribution shows it targets
   the wrong boundary. The active call site is the 8-byte scalar early-out layer
   at `crates/bbnf-simd/src/lib.rs:195`, not the actual hot kernel surfaced by
   B1 attribution. The earlier wiring of the 16-byte tiny-string helper into
   Track 1/Track 2 already regressed `twitter` ~25% (entry 28 above), and the
   refined cohort reading explains why: the kernel-versus-call-site mismatch
   means the Class A primitive cannot land the parse-G fix even when its
   checkasm parity is clean. The kernel stays in tree as a parity-green,
   grammar-generic primitive available to future grammars that genuinely
   address the 8-byte early-out layer (CSV-shape or other narrow-scan
   grammars); it is not the SK-V5 parse-G fix and Track 1/Track 2 remain on
   the 8-byte scalar tiny recognizer. The corrected parse-G fix is the NEON
   UTF-8 codepoint pipeline at `crates/parse-that-regex/src/lib.rs:331-339`,
   scheduled as Wave 3 of `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md`.
   Cohort cites: `restart/skinny/audit/SK-V5-COHORT/skv5-D6-class-ab-novelty.md`
   and `restart/skinny/audit/SK-V5-COHORT/skv5-B1-parse-attribution.md`. This
   refines the diagnosis recorded in entry 28; the kernel admission stands, the
   active-dispatch verdict stands, and the parse-G attribution moves to the
   UTF-8 codepoint pipeline.

34. SK-V5 Wave 2: bench-private `SinkParser` dishonesty IDENTIFIED in
    direct-to-struct attribution. Status: CLOSED by Wave 2.

   `crates/bbnf-bench/src/direct_struct.rs:150-156` shows `track1_digest` and
   `track2_digest` both call the same `sink_only_digest`. That digest is a
   hand-rolled recursive-descent SinkParser over `&[u8]` plus a cursor; it
   never touches `runtime::tape::Tape` and never enters generated codegen
   output. The current direct-to-struct gate therefore measures the same
   private parser twice and reports the result as Track 1 versus Track 2.
   The 6-of-17 sink-only passing rows recorded in entry 30 and the
   33-124% sonic-rs ratios summarized under Sonic Closeness were throughput
   for that bench-private parser, not for generated Track 1 over the canonical
   substrate. Wave 2 generated `SinkOnly` from BIR `DirectBuild`, rewired
   `bbnf-bench` Track 1 to call `runtime::generated_json::parse_direct`, and
   reshaped Track 2 into a structurally different hand-coded SinkOnly parser.
   The old shared `sink_only_digest` path is gone; the residual `mod hand`
   path is Track 2's required independent comparison surface. Cohort cites:
   `restart/skinny/audit/SK-V5-COHORT/skv5-B2-direct-attribution.md` and
   `restart/skinny/audit/SK-V5-COHORT/skv5-D5-sinkonly-novelty.md`.

35. SK-V5 Wave 1: codegen `lib.rs:111-117` decorative pass-through IDENTIFIED.
    Status: PENDING.

   `crates/codegen/src/lib.rs:111-117` literally writes `let _ = backend;` and
   then `include_str!`s `templates/json/parser.rs` and
   `templates/json/generated.rs` verbatim into the output. The BIR → Rust text
   step is a no-op pass-through that discards the `backend` argument and
   ignores every BIR fact the upstream extract pass attached to the plan. The
   BIR build itself in `passes::extract::single_plan` is honest and continues
   to attach `SpanMark`, `TapeEmit`, `DirectBuild`, and `Return` events to
   JSON materialized rules; the dishonesty is strictly the last text-emission
   step. Status is PENDING Wave 1 of
   `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md`: introduce a
   `BackendShape` enum, attach it to `LayoutFacts.backend_shape` via a
   `derive_backend_shape` pass, stand up a real `codegen/src/lower/` hierarchy
   that walks the BIR, and replace the `lib.rs:111-117` pass-through with the
   shape-driven emitter. Cohort cites:
   `restart/skinny/audit/SK-V5-COHORT/skv5-D3-derive-shape-novelty.md` and
   `restart/skinny/audit/SK-V5-COHORT/skv5-D5-sinkonly-novelty.md`.

36. SK-V5 Wave 4: JSON-hardcoded scalar references in `bbnf-simd` IDENTIFIED
    as Lock 14 violation. Status: PENDING.

   Four separate `classify_block_scalar` functions hardcode the seven JSON
   structural characters in their bodies:
   `crates/bbnf-simd/src/x86_64/avx2/classify.rs:31`,
   `crates/bbnf-simd/src/x86_64/avx512_vbmi2/classify.rs:28`,
   `crates/bbnf-simd/src/x86_64/avx512_gfni/classify_affine.rs:31`, and
   `crates/bbnf-simd/src/x86_64/avx512_bitalg/multiclass.rs:30`. The aarch64
   side bakes the same set into a TBL4 LUT at
   `crates/bbnf-simd/src/aarch64/classify_tbl4.rs:65-71`. This is a Lock 14
   violation: a primitive library that the spec requires to be grammar-neutral
   instead carries the JSON alphabet directly in its `.text` and `.rodata`.
   Status is PENDING Wave 4 of
   `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md`: parameterise each
   classifier on the alphabet, move JSON-specific data tables out of
   `bbnf-simd` into codegen-emitted `.data`, and admit the result through a
   Lock 14 remediation gate. Cohort cite:
   `restart/skinny/audit/SK-V5-COHORT/skv5-D4-simd-split-novelty.md`.

37. SK-V5 Wave 4: `bbnf-simd/src/lib.rs` JSON god-module status IDENTIFIED.
    Status: PENDING.

   `crates/bbnf-simd/src/lib.rs` is 716 LOC and concentrates JSON-specific
   surfaces that the two-layer vocabulary plan requires to be lifted into
   grammar-neutral primitives: a `JSON_STRUCTURAL` constant, an
   `is_json_punctuation` helper, `scan_json_tail`, a `JsonParseIndex` alias,
   and `resolve_json_string_masks_64`. Inside the same file a 230-LOC
   `mod neon` block at lines 463-693 contains 6× `vceqq_u8` JSON-punctuation
   fan-in at lines 642-647 and again at lines 665-670.
   `restart/skinny/MIGRATION.md:259-269` declares the split-by-primitive
   intent, and commit `9eef728c` lifted only 1 of the 9 declared primitives
   (`BYTE_CLASS_FROM_EQ_SET_64`); the other 8 remain inside the god module.
   Status is PENDING Wave 4 of
   `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md`: complete the
   primitive-by-primitive split into Layer 1 `bbnf.asm` modules and reduce
   `lib.rs` to a vocabulary index. Cohort cites:
   `restart/skinny/audit/SK-V5-COHORT/skv5-D4-simd-split-novelty.md` and
   `restart/skinny/audit/SK-V5-COHORT/skv5-A5-grammar-generalization.md`.

38. SK-V5 Wave 4: `crates/simd-scan/` fossil status IDENTIFIED. Status:
    PENDING deletion.

   `crates/simd-scan/` is a 584-LOC near-verbatim duplicate of
   `crates/bbnf-simd/src/lib.rs:13-100`. It is not in
   `skinny/Cargo.toml` `workspace.members`, it is not in
   `skinny/Cargo.toml` `workspace.dependencies`, and zero crates depend on
   it; the `simd_scan` token in `crates/bbnf-bench/Cargo.toml:34` is a
   criterion `[[bench]]` target name, not a crate dependency. Carrying a
   detached duplicate of the structural scanner alongside the canonical
   `bbnf-simd` surface violates the single-crate substrate boundary recorded
   in entry 22 and adds drift risk for every future SIMD change. Status is
   PENDING Wave 4 of
   `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md`: delete
   `crates/simd-scan/` outright. Cohort cite:
   `restart/skinny/audit/SK-V5-COHORT/skv5-D4-simd-split-novelty.md`.

39. SK-V5 Wave 2: Eisel-Lemire number materialization was vendored and
    wired into generated and hand SinkOnly. Status: CLOSED with residual.

   `crates/parse-that-regex/src/number/` now carries the upstream
   parse-that Eisel-Lemire implementation plus integer width materializers.
   Generated Track 1 and hand Track 2 both classify spans once and materialize
   `i64`, `u64`, or `f64` through the shared primitive. This fixes the
   prior exact-number planning gap without taking the rejected
   `raw.parse::<f64>()` shortcut from entry 31. The measured Wave 2 gate still
   reports direct NO-GO on number-heavy rows: `numbers` direct is Track 1
   8073 Mbps and Track 2 8601 Mbps against sonic-rs 12918 Mbps; `canada`
   direct is Track 1 7249 Mbps and Track 2 7656 Mbps against sonic-rs
   12182 Mbps. The residual is therefore not "missing exact float code"; it is
   typed sink emission overhead plus the still-deferred UTF-8/string pipeline.

40. SK-V5 Wave 2: generated `SinkOnly` is now the Track 1 direct-to-struct
    workload. Status: CLOSED, but the workload gate remains `N-direct / NoGo`.

   `runtime::grammars::json::JsonSink` is the grammar-local sink trait and
   `runtime::generated_json::parse_direct` is emitted through the
   `codegen/src/lower/sink_only.rs` lowerer. `bbnf-bench` Track 1 calls the
   generated runtime entry; Track 2 calls the independent hand-coded parser.
   Samply/nm attribution distinguishes the symbol paths:
   `runtime::generated_json::generated::parse_value_direct::<...JsonDigestSink>`
   for Track 1 and `<bbnf_bench::direct_struct::hand::HandParser>::value` for
   Track 2. The full Wave 2 gate shows correctness PASS on all 17 direct
   rows, but zero direct rows satisfy the `1.10x` sonic-rs time slack. The
   representative residuals are `mesh` 6308/6501/9568 Mbps,
   `marine_ik` 7276/7621/8853 Mbps, `unicode_mixed` 1648/1654/6421 Mbps, and
   `unicode_basic` 1991/1980/7184 Mbps for Track 1 / Track 2 / sonic-rs.
   Wave 3 owns the Unicode/string close; this wave proves that the prior
   bench-private attribution is gone.

41. SK-V5 Wave 2: `CARGO_TARGET_DIR` gate and metadata routing were corrected.
    Status: CLOSED.

   The gate previously read Criterion estimates from `skinny/target/criterion`
   even when the wave used `/tmp/skv5-wave2-target`, so a filtered run could
   silently mix stale default-target data with fresh per-wave benches. The
   gate now resolves the Criterion root from `CARGO_TARGET_DIR`, and both
   `json_parity` and `simd_scan` bench metadata writers honor the same target
   directory. The completed Wave 2 full bench initially wrote metadata to the
   old default path; those metadata files were copied into the wave target for
   this run only after the code fix, avoiding a second full benchmark cycle
   while preserving same-run provenance. Future full benches write estimates
   and metadata into the same target tree directly.

## Sonic Closeness

The parser works as the tape/direct hybrid the spec requires, but the current
full gate is not SOTA-close enough to dispatch. The expanded corpus is now the
authority for SOTA-BEAT: `twitter`, `random`, `unicode_mixed`, and
`unicode_basic` classify as G / NoGo; `unicode_escapes`,
`y_string_unicode`, `update_center`, `instruments`, and `distinct_values`
classify as C / GO; the remaining rows classify as A / GO. The common parse
blocker is still `parse_value_at`-heavy descent and string/Unicode projection,
not tape payload writes.

Direct-to-struct remains explicitly classified after the generated SinkOnly
rewrite. The workload now proves generated typed sink correctness, not merely
view projection or a bench-private parser. It moved the attribution to the
right symbol paths but did not close the SOTA gap: Wave 2 reports zero direct
rows within the 1.10 sonic-rs time slack. The residual is concentrated in
Unicode/string-heavy rows and dense typed-sink emission overhead, with number
rows improved by shared Eisel-Lemire materialization but still below the direct
gate.

The largest code win already landed was removing redundant whitespace scans:
large-corpus Track 1 improved by roughly 26-34% when that change first landed.
Adding an eager whitespace-bearing parse index was also tested and rejected:
twitter Track 1 doubled to roughly 783 us. A duplicate structural-byte column
was then removed and improved every targeted track row. Pair-token fusion was
also tested and rejected because it reduced token count without improving the
canonical Track 1 Mbps. The dispatch-table alternate was then corrected:
the old probe duplicated canonical Track 1, while a real function-pointer table
regressed the important rows, so there is no current cost-model masking
evidence from dispatch shape.
Dropping the skip column to make a 12-byte token was also measured and rejected
as canonical: it saved memory but did not produce a clean parse-throughput win.
The host-call probe now gives a separate warning: dispatch overhead is fine,
but eager parse-time string decode is too expensive to hide behind the
host-fn-free cut. The final accepted wins were local hot-path changes on top of
lazy-offset tape, not a new structural-index prepass or another tape-width
perturbation.

## Skinny Spec Amendments Folded

1. `restart/skinny/BENCH.md` makes Mbps the report unit.

   The classifier can continue to compute from nanoseconds internally, but the
   published gate reports Mbps for parse rows and scan rows.

2. `restart/skinny/BENCH.md` requires both Track 1 / sonic and Track 2 /
   sonic ratios in the table.

   The dual-track contract distinguishes codegen overhead from substrate
   ceiling (`BENCH.md:112-119`). Reporting only one ratio hides whether the
   failure is generator overhead or substrate materialization.

3. `restart/skinny/SUBSTRATE.md` splits structural-only scan from parser
   parse-index scan.

   The current text treats the structural stream as the main SIMD product
   (`SUBSTRATE.md:223-279`). The implementation found a real distinction:
   structural-only scan is the bench/floor product; parse-index scan is the
   parser product and may export string escape/control classification facts. It
   no longer exports a duplicate structural-byte vector because that measured
   as throughput-negative.

4. `restart/skinny/SUBSTRATE.md` documents the exact no-quotes fast path.

   For stripes outside strings with no quotes, the structural scanner can skip
   escape/parity classification and still be exact. This is necessary for
   numeric-heavy corpora such as `canada` and does not weaken the exactness
   guarantee at `SUBSTRATE.md:273-277`.

5. `restart/skinny/COMPILER.md` clarifies the whitespace boundary contract.

   The grammar sketch currently spells `value = ws (...) ws`
   (`COMPILER.md:56`) while the efficient parser shape is caller-owned trailing
   whitespace. The semantic contract is unchanged, but the codegen contract
   should say emitted `parse_value` skips leading layout only; callers consume
   trailing layout before separators, closers, or EOF.

   A stronger variant, "include all whitespace in the parse-index structural
   stream," was measured and rejected in this prototype because it doubled
   twitter parse time. If the spec wants indexed layout, it needs a separate
   layout-index design that does not force every parser scan to classify and
   emit whitespace bytes.

6. `restart/skinny/COMPILER.md` clarifies DirectBuild as typed projection.

   `COMPILER.md:200-202` says `DirectBuild` builds typed views, and
   `ARCHITECTURE.md:1403-1409` says direct builders do not bypass tape. The
   skinny spec should make the lazy projection shape explicit so "direct to
   struct" is not misread as an eager parallel owned struct tree.

7. `restart/skinny/SUBSTRATE.md` adopts close-token elision and keeps pair
   tokens as a measured load-bearing choice.

   Open container tokens carry end spans and subtree skips; JSON close-token
   count is now zero. Pair tokens are retained because a pair-token-free
   projection measured as token-count-positive but throughput-negative.
   Removing or fusing pair tokens still requires an explicit skinny substrate
   amendment and before/after bench row, not an implementation-only tweak.

8. `restart/skinny/BENCH.md` includes the masking-probe report contract, and
   the prototype gate renders compact probe rows in `RESULTS.md`.

9. `restart/skinny/*` updates local RESULTS path references.

   The implemented prototype writes the canonical run output to
   `skinny/RESULTS.md` next to the prototype workspace, and the authority specs
   now name that path while keeping `restart/skinny/` as the spec home.

10. `restart/skinny/SUBSTRATE.md` and `BENCH.md` fix parse/tape
    ownership wording.

    The implementation owns a sealed `Tape` inside `JsonRoot` / `JsonDocument`;
    `ValueRef` borrows that tape. It does not return a root borrowing a parser
    state's temporary tape.

11. `restart/skinny/SUBSTRATE.md` changes the committed tape storage from
    boxed-slice sealing to private-Vec semantic sealing.

    The read API stays `&[TapeToken]`, but the parse hot path no longer pays a
    shrink/copy to drop over-reserved capacity. `BENCH.md` now requires
    allocated tape bytes in addition to logical tape bytes so this is not a
    hidden memory tradeoff.

12. `restart/skinny/BENCH.md` and `COMPILER.md` reject the fake dispatch-table
    masking signal.

    The alternate dispatch-table probe must be a distinct implementation before
    it can classify Lens L. The prototype's old row duplicated canonical Track
    1, and a real function-pointer table measured worse than the canonical
    Rust `match`; the spec now keeps `match` as the canonical lowering and
    treats the current dispatch-table probe as invalid.

13. `restart/skinny/SUBSTRATE.md` and `ARCHITECTURE.md` record the rejected
    skipless-token perturbation.

    The 12-byte token experiment removed stored subtree skips and derived them
    from spans. Because the throughput result was mixed rather than a clean
    win, both specs keep the 16-byte aligned token canonical and route the
    remaining structural lever to lazy-offset tape rather than a hidden
    side substrate.

14. `restart/skinny/COMPILER.md` and `BENCH.md` record the host-call split.

    Dispatch overhead is measured separately from eager string decode. The
    current eager-decode rows are MASKING for parse-time decode, so the skinny
    can only claim JSON host-fn-free faithfulness if V1 keeps string decode
    lazy rather than emitting parse-time `decode_json_string_to_arena` for
    every string token.

## Full V1 Amendments Folded

1. `restart/ARCHITECTURE.md` names the two SIMD outputs.

   `ARCHITECTURE.md:951` has exact vs prefilter semantics for `SimdScan`, but
   the runtime architecture does not distinguish a bench-grade structural index
   from a parser-grade JSON parse index. V1 now names both products so future
   grammars do not accidentally pay parser-prefilter costs in structural-only
   gates, and it records that duplicate structural-byte columns are not free.

2. `restart/ARCHITECTURE.md` preserves DirectBuild-as-projection.

   `ARCHITECTURE.md:1403-1409` already says direct builders are typed
   projections over the same parse event stream. That line should be the
   controlling rule for V1 graduation. If V1 wants eager generated structs for
   selected grammars, that should be a cache over declared payload slots, not a
   second authoritative tree.

3. `restart/ARCHITECTURE.md` clarifies parse/tape ownership.

   The full spec keeps parse latency gates for JSON/CSS and reports structural
   scan floors in Mbps. It also removes the "JsonRoot over `&Tape`" shorthand:
   the root owns or is paired with a sealed document/tape snapshot, and
   `ValueRef` borrows the tape inside that document.

4. `restart/ARCHITECTURE.md` defines the token
   economy perturbation gate.

   If close tokens, pair tokens, allocation capacity, or skip patching remain
   the sonic gap, the full spec now has a sanctioned perturbation path:
   before/after bench rows for close-token elision with open-token end spans,
   pair-token fusion into key/value metadata, private-Vec semantic sealing, or
   chunked tape sealing. It records that JSON skinny adopted close-token
   elision and private-Vec sealing, while pair-token fusion failed the Mbps
   test. It forbids solving the gap by creating a side substrate.

5. `restart/MASTER-PLAN.md` converts the structural-scan SOTA row to Mbps.

## Hitherto Documented Changes

- `RESULTS.md` is now Mbps-first and reports both codegen and substrate closeness
  to sonic.
- `report.rs` owns the Mbps conversion and ratio formatting.
- `gate.rs` still classifies by the skinny threshold matrix but renders scan
  floors in Mbps and fails bbnf rows with non-zero/missing arena counters.
- `bbnf-simd` owns the structural scanner surface used by runtime and bench.
- Generated Track 1 and hand-coded Track 2 both write through `TapeBuilder` and
  emit the same tape.
- The parser state no longer carries structural offsets, whitespace bytes, or
  duplicate structural bytes; each was measured as throughput-negative or
  unrecovered parser-index cost.
- `passes::extract` now emits BIR materialization markers for tape/direct build
  on JSON materialized rules.
- Track 1 regenerated source matches the codegen template via `xtask check-json`.
- Runtime tests assert 16-byte aligned tape tokens and zero JSON payload-arena
  writes/allocations.
- The gate report publishes tape materialization stats and masking-probe rows
  per corpus.
- Generated `view.rs`, `value.rs`, and `visitor.rs` now own their definitions;
  `runtime::grammars::json` is the generated module alias.
- `match_json_number` / `skip_json_whitespace` use the tightened shared scanner
  path; Canada improved materially and now classifies outcome A in the final
  gate.
- JSON close tokens are elided; close kinds remain reserved, and open container
  tokens carry end spans plus subtree skips.
- The finished tape uses private-Vec semantic sealing and reports both logical
  and allocated tape bytes.
- Pair-token fusion was benchmarked and rejected as a canonical change.
- The alternate dispatch-table probe was invalidated because it duplicated
  Track 1; a real function-pointer table was tested and rejected, so the gate
  now marks that probe invalid instead of reporting stale Mbps.
- A skipless 12-byte token was tested and reverted; it reduced logical tape
  bytes but did not cleanly improve parse Mbps.
- Host-call dispatch overhead passes, but eager parse-time string decode is now
  documented as a MASKING signal for V1 JSON unless decode stays lazy.
- Lazy-offset JSON tape plus tape-union migration was implemented and measured;
  subsequent sparse-flag, spare-capacity write, SWAR, delimiter-fusion, and
  parser-split wins move the historical triad to pass. The current expanded
  parse gate still has G rows, and the full gate is `N-direct / NoGo`.
- The report now renders the actual fastest-anchor `S` comparator rather than
  only sonic-rs; conformance and SIMD parity metadata gates are executable.
- `bbnf-simd` is now the scanner crate used by runtime and bench, with
  byte-level and corpus parity tests under `crates/bbnf-simd/tests/`.
- Skinny and full specs now use the prototype workspace result path
  `skinny/RESULTS.md` for the runnable prototype, with `restart/skinny/` kept
  as spec authority.
- Current expanded-corpus parse gate: `skinny/RESULTS.md` records G / NoGo
  rows for `twitter`, `random`, `unicode_mixed`, and `unicode_basic`.
- Current direct-to-struct workload gate: correctness passes, Track 1 now
  calls generated `parse_direct`, Track 2 is an independent hand-coded
  SinkOnly parser, and zero of 17 rows pass the sonic-rs `1.10x` time slack.
  The overall gate reports `N-direct / NoGo`.

## Closed Reporting Gates

- The compact report now includes the signed Track 2 checklist required by the
  long-form `BENCH.md` result template. The current report states that Track 2
  uses `runtime::tape::TapeBuilder`, shares the Track 1 parity oracle, and
  never calls `runtime::generated_json::parse`.
- Peak RSS is now measured through row metadata and rendered by the compact
  gate. The current report shows bbnf peak RSS below the fastest competitor on
  the historical triad: twitter 3,424,256 vs 4,898,816 bytes, citm_catalog
  4,718,592 vs 7,733,248 bytes, and canada 5,750,784 vs 11,337,728 bytes.
- SK-V5 Wave 0 adds strictness disclosure to the compact result schema:
  `Strictness`, `parse_utf8`, `escape_complete`, `Output plane`, and
  `flaw_probe`. The current bbnf rows are marked `deferred /
  view-boundary / yes` rather than scan-boundary strict; this is the B3
  honesty correction and prevents permissive/deferred parse rows from being
  read as strict-vs-strict wins.
- SK-V5 Wave 0 adds the `runtime/parse-attribution` feature. Default builds
  keep Lock 15 force-inline behavior; attribution builds no-inline the seven
  profile boundaries documented in `restart/skinny/COMPILER.md` §6.5:
  `dispatch_value`, whitespace boundary, container open/close, string
  primitive entry, number primitive entry, literal verification, and tape
  emit/advance. This closes the cohort B1 complaint that `parse_value_at`
  collapsed all hot leaves into one symbol.
- SK-V5 Wave 0 records nuke decisions only. Deletions remain routed to their
  owner waves: generated EventCursor, `eventcursor`, `simd-scan`, and
  ParseIndexCursor die in Wave 4; the bench-private `SinkParser` and misplaced
  integer materializer die in Wave 2 after generated `SinkOnly` exists; the
  JSON hardcoded SIMD scalar references are split in Wave 4.

## Next No-Workaround Work

1. Keep the rejected-route ledger intact: structural-index typed parser prepass,
   NEON no-escape string matcher, separator elision, generic SWAR whitespace,
   12-byte/width churn, and dispatch-table/function-pointer alternates remain
   non-canonical unless a future bench row overturns them.
2. Carry the current G rows into V1 planning as the parse/tape SOTA-BEAT block:
   `parse_value_at` descent, random/key-dispatch overhead, and string/Unicode
   projection are now implementation requirements, not optional tuning.
3. Carry `N-direct / NoGo` into V1 planning as a separate typed-emission block:
   sink-only direct parsing closed much of the view-walk gap, but the remaining
   11 failing rows require exact float, string, and Unicode materialization
   work inside generated `SinkOnly`.
4. Carry the SK-V4 asmjson/dav1d reassay into V1 planning as an architecture
   correction, not a new directive: the substrate boundary is now the typed
   event stream, retained tape and direct `SinkOnly` are two materializations
   of that stream, and `CollapsedStage` is a conditional x86 per-grammar NASM
   authoring route guarded by `BBNF-COLLAPSEDSTAGE-NOT-VIABLE`. The current
   receiver packet is
   `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md`.
