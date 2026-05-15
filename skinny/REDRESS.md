# Skinny Redress: Mbps, Tape Materialization, and Spec Amendments

Date: 2026-05-12 through 2026-05-14.

This note records the implemented redress after the skinny prototype was brought
closer to the restart skinny/full contracts. The measured findings are now also
recorded in the runnable prototype surfaces under `skinny/`; the guarded
`restart/` authority surfaces remain outside this implementation pass.

## Current Bench Fact

The gate report is canonicalized to Mbps. The current measured authority is
`skinny/RESULTS.md`, regenerated after the SK-V5 direct receiver/source-shape
redress and full Criterion run. It records **overall outcome N-direct /
NoGo**.

Three blockers must stay separate:

1. The retained parse/tape plane has 13 hard G rows across the expanded corpus.
   Track 1 and Track 2 move together on those rows, so the miss is
   substrate/runtime shape rather than generator overhead alone.
2. `canada` parses faster than sonic-rs and no longer reports the stale
   **L / NO-GO** structural-floor failure. Item 56 is folded into the current
   full matrix: Canada structural-only scan reports 41495 Mbps against the
   40000 Mbps NEON floor. The retained GO rows in the full report are
   `canada`, `mesh`, `marine_ik`, and `numbers`.
3. The direct-to-struct workload is correctness-green
   (exact generated Track 1 / hand Track 2 digest equality; sonic-rs and
   serde_json shape parity) and now uses generated SinkOnly for Track 1. The
   prior bench-private SinkParser table is superseded. `citm_catalog`, `mesh`,
   `marine_ik`, and `numbers` currently pass the `1.10x` sonic-rs time slack;
   the other 13 direct rows remain N-direct blockers.

The earlier original-triad pass remains useful historical evidence, but it is
not the current close condition. The full 17-fixture gate plus
`direct_to_struct` is now the binding authority, and the report now renders an
explicit output-plane column beside strictness.

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | Track 1 / sonic | Track 2 / sonic |
|---|---:|---:|---:|---:|---:|
| twitter | 12303 | 12308 | 21176 | 58.1% | 58.1% |
| citm_catalog | 20775 | 20856 | 25413 | 81.8% | 82.1% |
| canada | 17738 | 17277 | 13719 | 129.3% | 125.9% |
| mesh | 13411 | 13278 | 11871 | 113.0% | 111.8% |
| random | 7794 | 7763 | 15451 | 50.4% | 50.2% |
| marine_ik | 12818 | 12803 | 9977 | 128.5% | 128.3% |
| numbers | 18740 | 18777 | 13523 | 138.6% | 138.9% |
| unicode_mixed | 8720 | 8623 | 15681 | 55.6% | 55.0% |
| unicode_escapes | 12848 | 13039 | 19090 | 67.3% | 68.3% |
| unicode_basic | 10898 | 10581 | 15753 | 69.2% | 67.2% |
| y_string_unicode | 6084 | 6051 | 13633 | 44.6% | 44.4% |

Structural scan is no longer the current Canada gate blocker: the full
report's `canada` structural-only scan reports 41495 Mbps against a
40000 Mbps floor after item 56. The remaining retained parse misses are
runtime/materialization and event/tape-consumption gaps, not a scanner-floor
failure.

Direct-to-struct is now the binding workload blocker:

| Corpus | Track 1 direct Mbps | Track 2 direct Mbps | sonic-rs direct Mbps | Track 1 / sonic direct |
|---|---:|---:|---:|---:|
| twitter | 11932 | 10986 | 15614 | 76.4% |
| citm_catalog | 21546 | 20204 | 21874 | 98.5% |
| canada | 10529 | 10455 | 12606 | 83.5% |
| mesh | 8942 | 9142 | 9691 | 92.3% |
| marine_ik | 9500 | 9337 | 8809 | 107.8% |
| numbers | 12633 | 12153 | 12583 | 100.4% |
| unicode_mixed | 4633 | 4593 | 11117 | 41.7% |
| unicode_escapes | 5262 | 5129 | 14427 | 36.5% |
| unicode_basic | 5520 | 5163 | 9653 | 57.2% |
| y_string_unicode | 4518 | 4323 | 8691 | 52.0% |

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
   parser remains on the 8-byte scalar tiny recognizer. The now-purged
   SK-V3 reprofile directory showed `parse_value_at` still dominating
   `random`, `unicode_escapes`, and `update-center`; SK-V6 requires new
   profiles on the generated Track 1 baseline before using that finding.

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
   the pre-Wave-2 bench-private SinkOnly table; entries 34, 40, and 46
   supersede its throughput interpretation. After generated Track 1 SinkOnly
   landed, the direct-number/context-sink redress ran, and item 57 refreshed
   the full matrix, `citm_catalog`, `mesh`, `marine_ik`, and `numbers` pass
   the current `1.10` time slack. The gate still appends outcome
   `N-direct / NoGo` when either bbnf direct track is slower than
   `sonic-rs * 1.10` in time.

31. Direct sink profiling moved the next blocker from view traversal to
    materialization leaves.

   `crates/bbnf-bench/src/bin/profile_direct.rs` records focused direct-sink
   profiles for Track 1, Track 2, sonic-rs, and serde baselines. The first
   samply pass on `twitter`, `numbers`, and `unicode_mixed` showed duplicate
   UTF-8 validation, string/unescape loops, and exact number materialization
   as the hot leaves; its stale profile directory is purged by SK-V6 Wave 0
   and its surviving findings are folded into SK-V5/SK-V6 authority. The
   implemented redress removes the
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
   the 8-byte scalar tiny recognizer. The later SK-V5 candidate was the NEON
   UTF-8 codepoint pipeline at `crates/parse-that-regex/src/lib.rs:331-339`,
   but SK-V6 records that route as refuted by entries 50-55 on the generated
   Track 1 baseline.
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

35. SK-V5 Wave 1/Wave 2 follow-up: codegen lowerer scaffolding exists and
    SinkOnly direct source is no longer template-authoritative. Status:
    CLOSED, with performance residual.

   `BackendShape`, `LayoutFacts.backend_shape`, `derive_backend_shape`, and
   `codegen/src/lower/` now exist. The follow-up lowerer pass extends the
   existing `DirectBuild` payload with a field/source roster, makes
   `lower::sink_only` walk BIR into a grammar-neutral `SinkOnlyProgram`, and
   makes `emit_json_with_layout` append direct source rendered from that
   program. The old handwritten `codegen/src/json_templates/sink_direct.rs`
   file is deleted, and codegen refuses emission when `DirectBuild` nodes are
   stripped from the backend. Track 1 direct now calls generated runtime code
   whose direct entry is lowerer-authored from BIR, not a bench-private parser
   and not a static direct template. This closes the codegen honesty blocker;
   the remaining NoGo belongs to measured parser/runtime work.
   Cohort cites:
   `restart/skinny/audit/SK-V5-COHORT/skv5-D3-derive-shape-novelty.md`,
   `restart/skinny/audit/SK-V5-COHORT/skv5-D5-sinkonly-novelty.md`, and the
   SK-V5 post-assay codegen read-only pass.

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
   `raw.parse::<f64>()` shortcut from entry 31. The later
   direct-number/context-sink redress lifts the number workload to a PASS:
   `numbers` direct now reports Track 1 12182 Mbps and Track 2 12069 Mbps
   against sonic-rs 12748 Mbps. The number-heavy geometry corpora are close
   but still red: `canada` direct is 10316/10480/12536 Mbps, `mesh` is
   8777/8841/9768 Mbps, and `marine_ik` is 7805/8004/8846 Mbps for Track 1 /
   Track 2 / sonic-rs. The residual is typed sink emission overhead, float
   materialization quality, and structural/event consumption, not a missing
   integer materializer.

40. SK-V5 Wave 2: generated `SinkOnly` is now the Track 1 direct-to-struct
    workload. Status: CLOSED, but the workload gate remains `N-direct / NoGo`.

   `runtime::grammars::json::JsonSink` is the grammar-local sink trait and
   `runtime::generated_json::parse_direct` is the generated runtime entry. The
   direct source is now rendered from the `codegen/src/lower/sink_only.rs`
   `SinkOnlyProgram` over BIR `DirectBuild` nodes (entry 35), so it is no
   longer a bench-private parser or a static JSON direct template.
   `bbnf-bench` Track 1 calls the generated runtime entry; Track 2 calls the
   independent hand-coded parser.
   Samply/nm attribution distinguishes the symbol paths:
   `runtime::generated_json::generated::parse_value_direct::<...JsonDigestSink>`
   for Track 1 and `<bbnf_bench::direct_struct::hand::HandParser>::value` for
   Track 2. The current full gate shows correctness PASS on all 17 direct
   rows. Item 57 refreshes this to four direct pass rows: `citm_catalog`,
   `mesh`, `marine_ik`, and `numbers`. Representative current residuals are
   `canada` 10529/10455/12606 Mbps, `unicode_mixed` 4633/4593/11117 Mbps,
   `unicode_escapes` 5262/5129/14427 Mbps, and `distinct_values`
   6212/5609/13214 Mbps for Track 1 / Track 2 / sonic-rs. The
   prior bench-private attribution and template-authority gaps are gone; the
   next close belongs to decoded-string delivery, float/materialization
   quality, event-stream consumption, and full-report incorporation of the
   later scan-floor redress.

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

42. SK-V5 Wave 3: trusted-UTF-8 JSON string boundary matching is VALIDATED as
    necessary, but insufficient.

   `parse-that-regex` now exposes a mode-aware string matcher with a trusted
   `&str` JSON path. Generated Track 1 retained parsing, generated SinkOnly
   direct parsing, and the hand Track 2 parsers use the trusted path when the
   caller already holds Rust `&str`; byte-string and grammar-string modes keep
   explicit UTF-8 validation. The full Wave 3 gate shows the duplicate
   validation penalty was real: `twitter` retained parse is now 12398 / 12441
   / 20727 Mbps for Track 1 / Track 2 / sonic-rs, `random` is 7965 / 8028 /
   15716 Mbps, `unicode_mixed` is 8578 / 8606 / 17751 Mbps, and
   `unicode_basic` is 10800 / 10890 / 15873 Mbps. Those rows improve the
   diagnosed string boundary, but they remain G / NoGo; the close now belongs
   to structural parser/materialization and event-stream consumption, not
   another UTF-8 boundary check.

43. SK-V5 Wave 3: the active post-escape skip and validation-batch route was
    INVALIDATED and redressed.

   The first Wave 3 wiring paired the trusted matcher with a post-escape cursor
   skip inside string validation and regressed `unicode_escapes` retained parse
   to roughly 4.4K Mbps. The redress removed the post-escape skip, restored
   scalar validation for `\uXXXX` syntax checking, and moved the NEON
   four-escape consumer to string materialization where batching is actually
   amortized. The latest full run reports `unicode_escapes` retained parse at
   12801 / 12855 / 18946 Mbps and direct at 5018 / 4986 / 14746 Mbps for
   Track 1 / Track 2 / sonic-rs. The fix closes the transient
   regression, but does not close the SOTA gate.

44. SK-V5 Wave 3: direct Track 2's false strict-string penalty is REMOVED.

   The independent hand SinkOnly parser now routes through the same trusted
   JSON string matcher used by generated Track 1 when parsing a Rust `&str`.
   This preserves the strictness disclosure honestly: the scan remains
   `view-boundary`, while the benchmark no longer makes Track 2 pay a duplicate
   UTF-8 validation tax that Track 1 had already avoided. The direct workload
   improves materially versus the Wave 2 direct attribution on several rows:
   `twitter` Track 2 moves from 4758 to 9815 Mbps, `random` from 2632 to 4829
   Mbps, `unicode_mixed` from 1654 to 4022 Mbps, and `unicode_basic` from 1980
   to 5163 Mbps. The workload remains correctness-green and reports only one
   direct PASS row (`numbers`) within the sonic-rs 1.10x time slack.

45. SK-V5 Wave 3 close decision: correctness and primitive gates are green,
    but the exit gate does not fire.

   `cargo test --workspace --profile ax-iter`, `xtask check-conformance`,
   `xtask check-json`, and `xtask primitive-checkasm` pass after the Wave 3
   changes; `primitive-checkasm` now includes the UTF-8 block parity test. The
   full `bench-json` run updates `RESULTS.md` and returns the expected NoGo
   gate status: retained parse has 13 G rows plus one Canada L row, and the
   direct workload has 16 NO-GO rows plus the `numbers` PASS row, with overall
   **N-direct / NoGo**. Items 56 and 57 later supersede this row: the current
   full matrix clears Canada L and records four direct pass rows.
   This is a measured handoff to true codegen lowering, event-stream
   consumption, and decoded-string delivery, not a permission to reopen
   rejected Class A tiny-string wiring or eager sidecar prepasses.

46. SK-V5 direct-number/context-sink redress: numeric direct rows improved,
    but the direct gate remains `N-direct / NoGo`.

   Generated and hand direct parsers now dispatch scalar values through
   context-specific `JsonSink` hooks, and `JsonDigestSink` folds scalar
   array/object values into the active frame without allocating a temporary
   value node. The shared number scanner gained 8/4/2 digit-run paths and
   span-native integer materialization for common integral spans; a 16-byte
   digit-prefix probe was tested and rejected because it regressed
   `canada`, `numbers`, and `mesh`. The accepted redress lifted `numbers`
   direct to PASS at 12182 / 12069 / 12748 Mbps for Track 1 / Track 2 /
   sonic-rs. The broader direct matrix remains red: `canada` is
   10316 / 10480 / 12536 Mbps, `mesh` is 8777 / 8841 / 9768 Mbps,
   `marine_ik` is 7805 / 8004 / 8846 Mbps, and Unicode/string-heavy rows
   remain decisive NO-GO. This validates shared number materialization and
   context hooks as necessary work, while rejecting another local digit-scan
   round as the SOTA close.

47. SK-V5 reporting redress: advisory bench mode and output-plane disclosure
    were corrected.

   `xtask bench-json --advisory` previously forwarded `--advisory` into the
   Criterion binaries, which reject it, and would have skipped the gate writer
   because the passthrough vector was non-empty. The harness now strips
   `--advisory` before Criterion and passes it only to `gate-json` after a full
   bench run. `RESULTS.md` also renders an explicit `Output plane` column for
   retained parse rows and direct workload rows, so strictness disclosure is
   paired with the materialization surface being compared.

48. SK-V5 SinkOnly lowerer redress: direct parser emission now consumes BIR.
    Status: CLOSED, with no throughput claim.

   `DirectBuild` now carries a field/source roster instead of only a shape
   string. `codegen/src/lower/sink_only.rs` lowers `BackendIr` into a
   grammar-neutral `SinkOnlyProgram` containing entry rule, rules, direct
   shapes, span kinds, literals, dispatch-alt count, and direct field rosters.
   `codegen/src/json_sink_direct.rs` validates the JSON grammar-local sink
   renderer against that program before emitting `parse_direct`; it rejects
   backends missing required rules, literals, span programs, shapes, or
   `DirectBuild` fields. The old `json_templates/sink_direct.rs` splice is
   removed, `cargo xtask regen-json` marks generated direct source with
   `sink-only lowered from BackendIr`, and codegen tests prove emission fails
   if `DirectBuild` nodes are stripped. Verification: `cargo fmt --all`,
   `xtask check-json`, `xtask check-conformance`, and targeted `runtime`,
   `bbnf-bench`, `parse-that-regex`, and `codegen` tests pass under
   `CARGO_TARGET_DIR=/tmp/skv5-lowerer-target`. `xtask lint-loc` remains red
   on the pre-existing `crates/bbnf-bench` budget (3393/3300 LOC); codegen is
   2027/4500 and generated runtime JSON is 1683/4000. A direct `gate-json
   --advisory` run without a fresh Criterion data set correctly produced
   invalid `n/a` rows and was discarded; no new performance measurement is
   claimed by this codegen-authority redress.

49. SK-V5 direct string-source redress: generated source hooks are ADMITTED,
    but the no-allocation decoded visitor route is REJECTED.

   Generated `parse_direct` now carries `ParsedString { raw, needs_unescape }`
   to `JsonSink::{key_source,string_source,array_string_source,
   object_string_source}` instead of unescaping inside the parser before the
   sink call. The default source hooks preserve the old behavior by allocating
   only when `needs_unescape`, so the architecture now has a grammar-neutral
   receiver for future decoded-at-sink materialization without adding a BIR
   variant or directive. `xtask check-json`, `xtask check-conformance`, and the
   targeted `runtime`, `bbnf-bench`, `parse-that-regex`, and `codegen` tests
   pass under `CARGO_TARGET_DIR=/tmp/skv5-string-target`.

   The attempted no-allocation decoded-string visitor was measured and
   rejected before commit. It added a chunk/char visitor over
   `parse-that-regex::unescape_json_string`, batched four `\uXXXX` escapes
   into one decoded byte chunk, and wired `JsonDigestSink` plus hand Track 2 to
   hash decoded source without building a `String`. Correctness was green, but
   focused Criterion rows regressed versus the prior direct baseline:
   `unicode_escapes` landed around 4105 / 4085 / 14352 Mbps for Track 1 /
   Track 2 / sonic-rs and `unicode_mixed` around 3578 / 3553 / 11004 Mbps.
   After reverting that active no-allocation consumer and keeping only the
   generated source-hook seam, the filtered rows returned to baseline:
   `unicode_escapes` 5031 / 5016 / 14525 Mbps, `unicode_mixed` 4165 / 4077 /
   11021 Mbps, `unicode_basic` 5523 / 5181 / 9688 Mbps, and
   `y_string_unicode` 4483 / 4473 / 9027 Mbps. The redress conclusion is
   narrow: source hooks are the right substrate seam, but decoded direct
   delivery must be a fused parse-that sink primitive rather than a generic
   visitor layered on top of the existing unescape path.

50. SK-V5 retained projection redress: parse-time aux side tables are
    REJECTED.

   A dense per-cursor aux column was implemented experimentally on the retained
   offset tape. The parser patched string body ends, scalar ends, and container
   next-sibling cursors at parse time; views consumed those fields instead of
   re-matching strings or depth-scanning containers. Correctness and
   conformance were green, and the eager retained traversal probe improved
   materially: focused Criterion rows showed `host_call_eager_decode` at
   twitter 6586 Mbps (+45.8% versus the prior target), canada 7881 Mbps
   (+96.5%), and unicode_basic 4570 Mbps (+74.7%). The governing parse plane
   regressed, however: focused `track1_generated` landed at twitter 12143 Mbps
   (-25.4%), citm_catalog 20625 Mbps (-29.2%), and canada 16614 Mbps (-2.2%).

   A sparse aux side table with O(1) parser-owned slot patching was then tested
   to avoid zero-writing an aux value for every cursor. It reduced neither the
   governing parse cost nor the tradeoff: focused rows landed at twitter 11676
   Mbps (-28.3%), citm_catalog 19874 Mbps (-31.8%), canada 11438 Mbps
   (-32.7%), and unicode_basic 9496 Mbps (+44.2%) on `track1_generated`; the
   retained traversal probe improved only twitter 5102 Mbps (+13.0%), canada
   5517 Mbps (+37.5%), and unicode_basic 2998 Mbps (+14.6%). Both side-table
   variants were reverted before commit. The conclusion is narrow and binding:
   retained view projection facts are real, but they cannot be written as a
   parse-time side table in the SOTA parse path. The admissible route remains
   typed event consumption over the existing tape projection, with string and
   number rescans confined to grammar-neutral primitives.

51. SK-V5 event-cursor redress: byte-class whitespace cursor is REJECTED.

   A transient `JsonEventCursor` wrapper was implemented experimentally in the
   retained generated parser. It did not retain a side table and it kept
   string/number work inside the existing parse-that primitives, but the
   cursor only centralized "next non-whitespace byte" using
   `BYTE_CLASS_FROM_EQ_SET_64`; it did not consume the JSON structural emit
   mask or carry quote/escape state. The first focused triad run was
   correctness-green after a start-offset repair, but regressed the governing
   retained parse plane: `track1_generated` measured approximately twitter
   7130 Mbps, citm_catalog 10291 Mbps, and canada 14110 Mbps. That is far
   below the current RESULTS baseline of 12398 / 21110 / 17321 Mbps and below
   the rejected-side-table experiments on twitter/citm.

   The slice was reverted before commit. The rejected route is narrow: moving
   whitespace skipping behind an `EventCursor` name is not the H.W1 close. The
   then-admissible cursor had to consume the scanner's live per-64-byte JSON
   emit mask (`punctuation & !string_body | real_quotes`) with O(1) pending
   state, yield structural punctuation and quote events, and cross-check any
   skipped source bytes with the grammar-neutral string/number/literal
   primitive boundary so invalid bytes such as `[1x,2]` could not disappear
   between scalar and delimiter. Item 53 subsequently measured and rejected
   that shape when implemented as a parser-local second scanner; the remaining
   admissible route is single-substrate event/tape consumption, not a retained
   parser cursor. No precomputed `StructuralIndex`, no `Vec<JsonEvent>`, no
   whitespace bitmap sidecar, and no aux projection column are admissible.

52. SK-V5 baseline reassay after the event-cursor rejection.

   Three fresh `samply` captures were recorded under
   `skinny/profile/skv5-event-redress/`: retained `twitter`, retained
   `random`, and generated direct `unicode_mixed` Track 1. The retained
   profiles preserve the prior diagnosis: symbol-level self time still
   collapses into `runtime::generated_json::generated::parse_value_at`, with
   PC-level leaves spread across the same inlined parse hub. Profile-loop
   throughput was twitter 11396 Mbps and random 7339 Mbps. The direct
   `unicode_mixed` Track 1 profile measured 3885 Mbps and attributes the
   dominant leaves to
   `runtime::generated_json::generated::parse_object_value_at_direct` plus
   `parse_that_regex::unescape_json_string`; decoded string delivery remains a
   direct gate blocker independent of retained parse cursor work.

53. SK-V5 structural-mask parser-local cursor is REJECTED.

   A second retained-parser cursor attempt implemented the stricter route that
   item 51 left open: `JsonStructuralCursor` consumed the JSON scanner's
   per-64-byte emit mask (`punctuation & !string_body | real_quotes`), carried
   quote / backslash state plus only O(1) pending mask state, yielded
   structural punctuation and quote events, and cross-checked every parser gap
   with `skip_json_whitespace` so invalid bytes such as `[1x,2]` could not be
   skipped. It introduced no retained `StructuralIndex`, no `Vec<JsonEvent>`,
   no whitespace bitmap sidecar, and no aux projection column. Runtime and
   codegen tests, `xtask check-json`, and `xtask check-conformance` were green.

   The focused retained triad rejected the shape decisively. `track1_generated`
   measured twitter 6156 Mbps, citm_catalog 8344 Mbps, and canada 7139 Mbps,
   versus the current RESULTS baseline of 12398 / 21110 / 17321 Mbps. Track 2
   stayed healthy at twitter 12171 Mbps, citm_catalog 20818 Mbps, and canada
   17184 Mbps, so the regression is not a substrate-wide correctness or bench
   problem; it is the parser-local cursor cost. The cursor still performs a
   second structural scan while the recursive-descent parser continues to read
   the same source for strings, numbers, literals, and whitespace validation.
   That turns the "event cursor" into an additional parse-time scanner rather
   than the parse substrate.

   The slice was reverted before commit. The admissible H.W1 route is now
   narrower: structural projection must be the parser's single substrate, not a
   second scanner bolted onto source-byte recursive descent. Either the scanner
   writes the tape/event stream and generated lowering consumes that stream
   directly, or a `CollapsedStage` / `SinkOnly` lowering consumes live masks in
   the same loop. A `ParserState`-owned structural cursor over source bytes is
   non-canonical unless a future before/after row overturns this measurement.

54. SK-V5 exact decoded-string stats sink is REJECTED.

   A narrower direct-string attempt kept the admitted `JsonSink::*_source`
   seam but replaced escaped-string allocation in `JsonDigestSink` with an
   exact decoded-length plus exact `hash_bytes` computation. The helper reused
   parse-that Unicode escape validation, computed decoded length in one pass,
   streamed decoded bytes into a chunked hasher in a second pass, and preserved
   byte-for-byte digest parity with the allocating baseline. `cargo fmt`,
   targeted `parse-that-regex`, `bbnf-bench`, `runtime`, and `codegen` tests,
   `xtask check-json`, and `xtask check-conformance` were green under
   `CARGO_TARGET_DIR=/tmp/skv5-string-stats-target`.

   The focused direct workload rejected the shape. Versus the current RESULTS
   baseline, ordinary and numeric rows stayed within noise or slightly
   improved (`numbers` Track 1/Track 2/sonic-rs: about 12446 / 12267 / 12886
   Mbps; `unicode_basic`: about 5539 / 5167 / 9682 Mbps), but escaped-string
   rows regressed precisely where the route needed to close the gap:
   `unicode_mixed` Track 1/Track 2/sonic-rs landed around 3428 / 4187 / 11268
   Mbps, `unicode_escapes` around 2385 / 5117 / 14442 Mbps, and
   `y_string_unicode` around 3301 / 4327 / 9029 Mbps. Track 2 stayed near its
   prior allocation baseline on the same rows, so the regression is not a
   corpus or comparator problem; the generated Track 1 sink paid the exact
   two-pass stats cost on escape-heavy strings.

   The slice was reverted before commit. The source-hook seam remains
   canonical, but sink-local exact stats are not the direct-string close. At
   this point the admissible route narrowed to a one-pass parse-that/SinkOnly
   materializer; item 55 then measured and rejected the sink-local streaming
   hash version of that route. Allocation removal alone does not count as
   progress unless the affected direct rows cross the sonic-rs 1.10x slack.

55. SK-V5 quote-source fused string materializer is REJECTED.

   A stricter one-pass direct-string attempt moved the generated parser from
   pre-scanning strings into `ParsedString { raw, needs_unescape }` to
   quote-source sink hooks. The specialized digest sink then called a new
   parse-that fused materializer that scanned, validated escapes, decoded, and
   streamed decoded bytes into a hash accumulator in the same loop. AArch64
   four-`\uXXXX` batching was preserved and then tightened to push the decoded
   UTF-8 stack buffer in one callback. This route avoided the item 49 generic
   visitor and the item 54 two-pass decoded length/hash helper. Correctness was
   green: `cargo fmt`, targeted `parse-that-regex`, `bbnf-bench`, `runtime`,
   and `codegen` tests, `xtask check-json`, and `xtask check-conformance`
   passed under `CARGO_TARGET_DIR=/tmp/skv5-fused-string-target`.

   The focused direct rows still rejected the shape. Initial rows measured
   Track 1 / Track 2 / sonic-rs at roughly: `unicode_mixed` 3744 / 3746 /
   10279 Mbps, `unicode_escapes` 2716 / 2761 / 14312 Mbps,
   `unicode_basic` 5153 / 5002 / 8029 Mbps, and `y_string_unicode` 2834 /
   2855 / 7942 Mbps. The follow-up after batching decoded Unicode callbacks
   did not recover the miss: `unicode_mixed` stayed around 3741 / 3757 /
   10273 Mbps, `unicode_escapes` around 2681 / 2683 / 14335 Mbps, and
   `y_string_unicode` around 2800 / 2841 / 7743 Mbps; Criterion marked the
   `unicode_escapes` Track 2 change as a regression. All of these remain below
   the checked-in direct baseline where the default source hooks allocate then
   hash contiguous decoded strings (`unicode_mixed` 4178 / 4022 / 11143 Mbps,
   `unicode_escapes` 5018 / 4986 / 14746 Mbps, `y_string_unicode` 4518 /
   4323 / 8691 Mbps).

   The slice was reverted before commit. The conclusion is narrower than item
   54: even a true one-pass quote-source streaming hasher is not the
   string/Unicode direct close for the current digest workload. The source-hook
   seam remains canonical, but the next admissible direct-string plan must beat
   the default allocate-then-contiguous-hash baseline. That points to a
   field-layout materializer or same-loop `SinkOnly` / `CollapsedStage` plan
   that produces the required typed field representation directly; another
   sink-local decoded hash path is non-canonical unless a before/after row
   overturns items 49, 54, and 55.

56. SK-V5 structural scan floor redress is ADMITTED.

   The stale full `RESULTS.md` gate recorded Canada as L / NO-GO because the
   structural-only scan measured 22136 Mbps against the 40000 Mbps NEON floor.
   A fresh pre-redress explorer measurement reproduced the floor miss more
   sharply: Canada SIMD structural scan was about 20482 Mbps, while scalar was
   about 7341 Mbps. The admitted redress keeps the scanner grammar-neutral but
   removes two hot fixed costs on quote-free stripes: a structural+terminator
   classifier returns punctuation and quote masks in one NEON table pass, and
   `compact_mask` now emits all set-bit positions through the admitted
   `bulk_emit_positions_64` primitive into reserved spare capacity.

   Both primitives have scalar references, dispatch-table entries, and
   checkasm coverage. They are also consumed in the same change:
   `bulk_emit_positions_64` is used by `bbnf_simd::compact_mask`, and the
   structural+terminator classifier is used by JSON scan's no-quote fast path.
   The candidate is not a JSON leak into `bbnf-simd`: the runtime supplies the
   class table and terminator byte; the primitive remains table-driven.

   Verification under `CARGO_TARGET_DIR=/tmp/skv5-scan-target`:
   `primitive-checkasm` passed for the admitted primitive set including
   `checkasm_bulk_emit_positions_64` and
   `checkasm_structural_terminator_64`; targeted `bbnf-simd`, `runtime`, and
   `bbnf-bench` tests passed under `--profile ax-iter`. The focused
   `simd_scan` bench measured twitter SIMD at about 24040 Mbps, citm_catalog
   SIMD at about 25672 Mbps, and Canada SIMD at about 41833 Mbps. Canada now
   clears the 40000 Mbps structural floor in the focused row.

   Item 57's full `bench-json --advisory` refresh incorporates this slice:
   Canada structural scan reports 41495 Mbps against the 40000 Mbps NEON
   floor. The expanded retained parse rows still carry 13 G / NoGo failures,
   and direct-to-struct remains `N-direct / NoGo`. The admitted conclusion is
   exactly bounded: the Canada structural-scan floor is no longer the active
   blocker after this slice; the remaining close belongs to single-substrate
   event/tape consumption, string/Unicode projection, and direct field-layout
   materialization.

## Sonic Closeness

The parser works as the tape/direct hybrid the spec requires, but the current
full gate is not SOTA-close enough to dispatch. The expanded corpus is now the
authority for SOTA-BEAT: `canada`, `mesh`, `marine_ik`, and `numbers`
classify as A / GO in the checked-in full report, and Canada structural scan
is green at 41495 Mbps. The remaining 13 retained parse rows classify as
G / NoGo. The common parse blocker is source/tape event consumption and
string/Unicode projection, not codegen overhead, tape payload writes, or the
Canada structural floor.

Direct-to-struct remains explicitly classified after the generated SinkOnly
rewrite. The workload now proves generated typed sink correctness, not merely
view projection or a bench-private parser. It moved the attribution to the
right symbol paths but did not close the SOTA gap: the latest full run reports
four direct rows (`citm_catalog`, `mesh`, `marine_ik`, `numbers`) within the
1.10 sonic-rs time slack. Generated source hooks now preserve raw string spans
to the sink boundary, but the no-allocation decoded-string, exact stats, and
quote-source streaming routes regressed and were rejected. The residual
therefore remains dense typed-sink emission, field-layout decoded-string
delivery, exact float/string/Unicode materialization, and event-stream
consumption. A later exact decoded-stats sink also regressed escape-heavy
direct rows and is rejected in item 54, and a true quote-source one-pass
streaming hasher is rejected in item 55. Another sink-local decoded hash path
does not count as the close.
Parse-time retained projection side tables were also measured and rejected in
item 50, the byte-class whitespace cursor was rejected in item 51, and the
parser-local structural-mask cursor was rejected in item 53. View facts must be
consumed through the single tape/event substrate rather than written as another
retained column, hidden behind a renamed whitespace skipper, or scanned again
through a second parser-local cursor.

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
  path; Canada parse throughput improved materially. Item 56's structural
  classifier/bulk-emit redress is now folded into the full matrix: Canada
  structural-only scan reports 41495 Mbps against the 40000 Mbps NEON floor.
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
  parser-split wins remain real, but the checked-in expanded parse gate has
  13 G rows and four A / GO retained rows (`canada`, `mesh`, `marine_ik`,
  `numbers`). The full gate remains `N-direct / NoGo`.
- The report now renders the actual fastest-anchor `S` comparator rather than
  only sonic-rs; conformance and SIMD parity metadata gates are executable.
- `bbnf-simd` is now the scanner crate used by runtime and bench, with
  byte-level and corpus parity tests under `crates/bbnf-simd/tests/`.
- Skinny and full specs now use the prototype workspace result path
  `skinny/RESULTS.md` for the runnable prototype, with `restart/skinny/` kept
  as spec authority.
- Current expanded-corpus parse gate: `skinny/RESULTS.md` records A / GO rows
  for `canada`, `mesh`, `marine_ik`, and `numbers`; every other retained
  parse row remains G / NoGo.
- Current direct-to-struct workload gate: correctness passes, Track 1 now
  calls generated `parse_direct`, Track 2 is an independent hand-coded
  SinkOnly parser, and `citm_catalog`, `mesh`, `marine_ik`, and `numbers`
  pass the sonic-rs `1.10x` time slack. The overall gate reports
  `N-direct / NoGo` because 13 direct rows still miss.
- SK-V5 Wave 3 adds mode-aware string matching, a trusted UTF-8 JSON string
  path for Rust `&str` callers, a grammar-neutral UTF-8 block parity gate, and
  batched `\uXXXX` materialization. It removes duplicate UTF-8 validation from
  both generated and hand direct parsers without changing the strictness
  disclosure: parse rows are still `deferred / view-boundary / yes`.

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
   12-byte/width churn, dispatch-table/function-pointer alternates, parse-time
   projection side tables, byte-class whitespace EventCursor wrappers,
   parser-local structural-mask cursors, and sink-local decoded hash helpers
   remain non-canonical unless a future bench row overturns them.
2. Carry the current G rows into V1 planning as the parse/tape SOTA-BEAT
   block: event-stream consumption, random/key-dispatch overhead, and
   string/Unicode projection are now implementation requirements, not optional
   tuning. Canada structural-scan floor restoration is admitted in item 56
   and incorporated in the full matrix; the remaining work is not scanner
   floor refresh.
3. Carry `N-direct / NoGo` into V1 planning as a separate typed-emission block:
   sink-only direct parsing closed much of the view-walk gap and the BIR
   lowerer now owns the generated direct source, but the remaining 13 failing
   rows require exact float, decoded string, Unicode field materialization, and
   event-stream consumption inside generated `SinkOnly`; decoded string
   materialization must beat the current allocate-then-contiguous-hash baseline
   rather than merely remove allocation.
4. Carry the SK-V4 asmjson/dav1d reassay into V1 planning as an architecture
   correction, not a new directive: the substrate boundary is now the typed
   event stream, retained tape and direct `SinkOnly` are two materializations
   of that stream, and `CollapsedStage` is a conditional x86 per-grammar NASM
   authoring route guarded by `BBNF-COLLAPSEDSTAGE-NOT-VIABLE`. The current
   receiver is SK-V6's profile-first dispatch over the SK-V5 landed substrate,
   not the purged SK-V4 packet.

## SK-V5 Wave 5 Primitive Admission Redress

- Wave 5 admitted only primitives with same-wave runtime consumers.
  `BYTE_CLASS_FROM_TABLE_64` is consumed by generic `scan_dispatch`;
  `BITMAP_PREFIX_XOR_64` is consumed by JSON string-region scan;
  `BITMAP_NEXT_SET_BIT` is consumed by `compact_mask`; `EOB_PAD_CLAMP` is
  consumed by JSON tail scan. Dedicated checkasm gates cover all four plus
  the pre-existing `BYTE_CLASS_FROM_EQ_SET_64` primitive.
- Post-Wave 5 scan-floor redress admits two more consumed scan primitives:
  `BULK_EMIT_POSITIONS_64`, consumed by `compact_mask`, and the table-driven
  structural+terminator classifier consumed by JSON scan's no-quote fast path.
  The full matrix now clears the 40000 Mbps NEON floor with Canada structural
  scan at 41495 Mbps. This is scan-floor credit only; it does not close
  `N-direct` or the expanded retained parse G rows.
- The packet-level phrase "all remaining bbnf.asm primitive bodies" conflicts
  with the same-wave-consumer non-negotiable for `BULK_EMIT_COMPRESSED`,
  `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, and
  `FSM_DISPATCH_THREADED`: no current generated/runtime hot path consumes
  those bodies. Per the A2 dav1d-process report, admitting them now would
  create orphan kernels and violate Lock 16. They remain blocked until the
  structural-tape compressed sink, bracket-stack CollapsedStage, and
  per-grammar `.asm` CollapsedStage consumers exist.
- This is not a performance deferral claim. It is an admission correction:
  primitives without consumers cannot close Wave 5 honestly, cannot lift a
  named row, and cannot be credited toward SOTA. The next implementation
  packet must either land the missing consumers in the same wave or remove
  those primitive bodies from the Wave 5 close condition.
- The first register-clobber harness attempt wrapped arbitrary Rust closures
  with AArch64 callee-saved GPR sentinels. That is not a sound checkasm shape:
  the Rust compiler may legitimately allocate callee-saved registers inside
  the closure frame, so the sentinel can report false positives before any ASM
  candidate is involved. Wave 5 keeps verified stack canaries for Rust
  candidate calls and reserves raw register sentinels for future FFI/ASM
  `call_new` shims, where the callee boundary is explicit.
- `gate-json` remains `N-direct / NoGo` after the admitted primitive gates.
  This is expected: the admitted primitives harden scanner vocabulary and
  checkasm coverage; they do not claim to close the direct-to-struct rows.

## SK-V5 Direct Receiver + Full Matrix Redress

- Item 57 admits direct receiver inlining plus the generated direct
  tiny-plain-string fast path as a bounded direct-to-struct improvement, not
  as a retained parse-G fix. The implementation adds force-inline annotations
  to the `JsonDigestSink` / `JsonDirectDigest` receiver hot leaves and routes
  generated `SinkOnly` direct string parsing through the existing
  `match_tiny_plain_string` raw-span fast path before the trusted full JSON
  string matcher. Track 2 receives the same bounded scalar early-out so the
  dual-track diagnosis remains fair.
- The full advisory matrix was refreshed after item 57 with
  `CARGO_TARGET_DIR=/tmp/skv5-tiny-direct-target RUSTFLAGS="-C target-cpu=native"
  cargo xtask bench-json --advisory`. The current `skinny/RESULTS.md` records
  retained parse A / GO rows for `canada`, `mesh`, `marine_ik`, and `numbers`;
  the stale Canada L row is gone and the structural scan reports 41495 Mbps
  against the 40000 Mbps NEON floor.
- Direct-to-struct improves from the prior single passing row to four passing
  rows: `citm_catalog`, `mesh`, `marine_ik`, and `numbers`. The overall gate
  remains `N-direct / NoGo`: 13 direct rows still miss sonic-rs direct by the
  1.10x time-slack rule, with the hardest misses concentrated in
  `unicode_mixed`, `unicode_escapes`, `distinct_values`, and
  `y_string_unicode`.
- This redress does not reopen the rejected Class A route. Active
  `match_tiny_plain_string` wiring as a retained parse-G fix remains rejected
  by REDRESS entries 28 and 33. Item 57 is direct-only receiver/source-shape
  work: it reduces generated `SinkOnly` overhead and raw-span routing cost but
  does not make the scalar tiny-string kernel a SOTA-BEAT primitive.
- A transient 16-byte AArch64 escape/control scanner for the direct unescape
  path was tested and reverted before landing. Focused Unicode rows regressed
  rather than closing the gap, so the admissible close remains a fused
  field-layout decoded string materializer that beats the current
  allocate-then-contiguous-hash baseline. Reintroducing an escape/control scan
  inside `unescape_json_string` without a same-loop consumer does not count as
  a new route.

## SK-V6 Wave 0 Regression-Recovery Redress

- Item 58 records the SK-V6 dispatch framing. The current measured authority is
  the post-SK-V5 `skinny/RESULTS.md` baseline: full gate `N-direct / NoGo`, 13
  retained parse G rows, four retained A rows (`canada`, `mesh`, `marine_ik`,
  `numbers`), four direct pass rows (`citm_catalog`, `mesh`, `marine_ik`,
  `numbers`), and 13 direct red rows. Canada structural scan is green at 41495
  Mbps against the 40000 Mbps NEON floor. The SK-V6 prompt originally requested
  entries 57/58, but item 57 is already committed to direct receiver/source
  redress; the ledger stays monotonic and records the SK-V6 additions as 58/59.
- Item 58 closes no performance row. Its purpose is dispatch hygiene: purge
  superseded SK-V3/SK-V4 implementation packets, SK-V1/SK-V2 hardening drafts,
  and pre-SK-V5 profile directories; preserve SK-V5 as substrate-history
  authority; and require fresh PC-level profiles of the generated Track 1
  baseline before any kernel prescription. This prevents another hypothesis
  transfer from the bench-private SK-V4/SK-V5 audit baseline.
- Item 59 refutes the SK-V5 Wave 3 UTF-8 fusion class as a close route. The
  binding sub-routes are already measured: retained projection side tables
  (50), byte-class whitespace cursor (51), parser-local structural-mask cursor
  (53), exact decoded-string stats sink (54), and quote-source fused streaming
  materializer (55). Together they invalidate the claim that "fold UTF-8
  validation into the NEON 16-byte body scan" is enough to close parse-G or
  direct string rows on the current generated runtime baseline.
- Item 59 does not ban `parse-that` string/Unicode work. It bans prescribing
  that class without same-row falsification gates. Future work must name the
  corpus rows, profile path, c/B or Mbps delta, and hot symbol boundary before
  implementation; failure to lift the named row reverts and records another
  rejected route here.

## SK-V6 Wave 2 Candidate-1 Redress

- Item 60 rejects the retained trusted-string boundary collapse. The tested
  shape removed retained parse's scalar `match_tiny_plain_string` probe before
  `match_string_at_quote` in both the checked-in generated runtime and the
  codegen template, while preserving the helper for direct `SinkOnly` because
  `parse_string_direct` still consumes it. The intent was to eliminate the
  double string-boundary scan identified by the SK-V6 R1/R2 profiles.
- The route failed the same-row falsification gate on the new generated Track 1
  baseline. Focused retained `profile-lazy` rows were built twice from the same
  tree with only the two-file patch toggled:

  | row | baseline Mbps | candidate Mbps | delta |
  |---|---:|---:|---:|
  | twitter | 12009 | 9546 | -20.5% |
  | random | 7773 | 4675 | -39.9% |
  | unicode_basic | 10753 | 5707 | -46.9% |
  | apache_builds | 12106 | 6796 | -43.9% |
  | distinct_values | 6100 | 5382 | -11.8% |
  | gsoc-2018 | 21282 | 15894 | -25.3% |
  | y_string_unicode | 5882 | 4899 | -16.7% |

- The measurements came from `/tmp/skv6-wave2-candidate1-mbps.txt` using
  `/tmp/skv6-wave2-baseline-release/release/profile-lazy` and
  `/tmp/skv6-wave2-candidate1-release/release/profile-lazy`. The gate required
  at least 5% improvement on `twitter`, `random`, `unicode_basic`, and
  `distinct_values` with no retained row regressing more than 2%; the candidate
  regressed every measured row and was reverted before commit.
- The correction is architectural, not cosmetic: the tiny-string probe is not
  redundant front matter. On dense short-string rows it prevents the trusted
  full-string matcher from paying a larger loop and error-construction boundary
  for short plain strings. Future retained string work must specialize the
  second boundary (`match_string_at_quote`) for long/Unicode rows or split
  short-string and long-string profiles explicitly; simply deleting the scalar
  early-out is now a blocked route.

## SK-V6 Wave 2 Candidate-2 Redress

- Item 61 rejects the retained long-string trusted scan specialization as
  tested. The route added a grammar-neutral AArch64 64-byte quote/backslash/
  control scanner in `bbnf-simd::aarch64::string_block`, a scalar executable
  reference plus `checkasm_string_block_64`, and a same-wave consumer inside
  `parse-that-regex::skip_json_string_plain_trusted`. It did not add retained
  sidecar state and did not reopen the SK-V5 UTF-8-fusion class; the primitive
  targeted only trusted string-special-byte discovery after the JSON source had
  already entered the generated runtime path.
- Focused retained `profile-lazy` measurements showed the route was plausible
  but incomplete:

  | row | baseline Mbps | candidate Mbps | delta |
  |---|---:|---:|---:|
  | unicode_mixed | 7899 | 9235 | +16.9% |
  | gsoc-2018 | 21651 | 25082 | +15.8% |
  | y_string_unicode | 5955 | 6313 | +6.0% |
  | twitter | 12191 | 12370 | +1.5% |

- The full advisory gate failed the Candidate 2 falsifiability contract. The
  committed authority before the route was compared against the candidate
  `bench-json --advisory` output:

  | row | baseline Track 1 Mbps | candidate Track 1 Mbps | delta |
  |---|---:|---:|---:|
  | twitter | 12303 | 12141 | -1.3% |
  | citm_catalog | 20775 | 20364 | -2.0% |
  | canada | 17738 | 15998 | -9.8% |
  | apache_builds | 12341 | 14142 | +14.6% |
  | github_events | 13161 | 14087 | +7.0% |
  | update_center | 9430 | 9884 | +4.8% |
  | mesh | 13411 | 13223 | -1.4% |
  | random | 7794 | 8407 | +7.9% |
  | gsoc-2018 | 21907 | 25285 | +15.4% |
  | marine_ik | 12818 | 12448 | -2.9% |
  | instruments | 11887 | 10991 | -7.5% |
  | numbers | 18740 | 18774 | +0.2% |
  | unicode_mixed | 8720 | 9495 | +8.9% |
  | unicode_escapes | 12848 | 13350 | +3.9% |
  | unicode_basic | 10898 | 11730 | +7.6% |
  | distinct_values | 6097 | 6369 | +4.5% |
  | y_string_unicode | 6084 | 6516 | +7.1% |

- The gate required Track 1 to improve by at least 10% on at least two of
  `unicode_mixed`, `gsoc-2018`, and `y_string_unicode`, with no retained row
  regression above the written budget. Only `gsoc-2018` cleared the 10% bar in
  the full matrix, while `canada` and `instruments` regressed by more than 5%.
  The route was reverted; `skinny/RESULTS.md` remains the pre-candidate
  authority.
- The parse-attribution criterion also proved too blunt for this shape. The
  scanner is consumed under the same `match_string_at_quote` wrapper, so the
  symbol share did not fall below 45% even when row Mbps improved. Captured
  profiles at `/tmp/skv6-wave2-candidate2-profiles/` showed wrapper shares of
  72.26% (`unicode_mixed`), 66.58% (`gsoc-2018`), and 71.93%
  (`y_string_unicode`). Future string-scan candidates must either expose a
  separate noinline symbol boundary for the new primitive or use c/B and row
  Mbps deltas as the falsification signal.

## SK-V6 Wave 2 Candidate-3 Redress

- Item 62 rejects the delayed-wide retained trusted string scan. The route kept
  `match_tiny_plain_string`, preserved the first 16-byte AArch64 trusted string
  probe, and entered a 64-byte quote/backslash/control scanner only after that
  first local block reported no special byte. It included a scalar executable
  reference, AArch64 checkasm parity, and a same-wave consumer inside
  `parse-that-regex::skip_json_string_plain_trusted`.
- Correctness and primitive gates were green before measurement:
  `cargo test -p bbnf-simd --profile ax-iter --test checkasm_string_block_64`,
  `cargo test -p parse-that-regex --profile ax-iter`,
  `cargo build --workspace --profile ax-iter`, and
  `cargo test --workspace --profile ax-iter`.
- The R6b production `profile-lazy` smoke failed before Criterion. Measurements
  used baseline and candidate release binaries built from the same tree under
  `/tmp/skv6-wave2-candidate3-smoke`; each row reports the median of three
  repetitions:

  | row | base c/B | candidate c/B | c/B delta | base Mbps | candidate Mbps | Mbps delta |
  |---|---:|---:|---:|---:|---:|---:|
  | apache_builds | 2.292639 | 2.352151 | +2.60% | 12213 | 11904 | -2.53% |
  | canada | 1.630846 | 1.662807 | +1.96% | 17169 | 16839 | -1.92% |
  | citm_catalog | 1.349398 | 1.366187 | +1.24% | 20750 | 20495 | -1.23% |
  | distinct_values | 4.544717 | 4.966300 | +9.28% | 6161 | 5638 | -8.49% |
  | github_events | 2.150372 | 2.246830 | +4.49% | 13021 | 12462 | -4.29% |
  | gsoc-2018 | 1.278539 | 1.324503 | +3.60% | 21900 | 21140 | -3.47% |
  | instruments | 2.375297 | 2.486899 | +4.70% | 11788 | 11259 | -4.49% |
  | marine_ik | 2.208028 | 2.268492 | +2.74% | 12681 | 12343 | -2.67% |
  | mesh | 2.075612 | 2.114484 | +1.87% | 13490 | 13242 | -1.84% |
  | numbers | 1.528635 | 1.525719 | -0.19% | 18317 | 18352 | +0.19% |
  | random | 3.567333 | 3.719447 | +4.26% | 7849 | 7528 | -4.09% |
  | twitter | 2.289078 | 2.473498 | +8.06% | 12232 | 11320 | -7.46% |
  | unicode_basic | 2.565747 | 2.595476 | +1.16% | 10913 | 10788 | -1.15% |
  | unicode_escapes | 2.290201 | 2.300929 | +0.47% | 12226 | 12169 | -0.47% |
  | unicode_mixed | 3.473084 | 3.306956 | -4.78% | 8062 | 8467 | +5.02% |
  | update_center | 2.992732 | 3.159558 | +5.57% | 9356 | 8862 | -5.28% |
  | y_string_unicode | 4.712218 | 4.757859 | +0.97% | 5942 | 5885 | -0.96% |

- The gate required `gsoc-2018` to improve by at least 10% c/B, at least one
  of `unicode_mixed` or `apache_builds` to improve by at least 6%, and no
  retained row to regress by more than 1.5% c/B before running Criterion. The
  candidate improved only `unicode_mixed` materially and regressed multiple
  sentinel rows; it was reverted before `bench-json --advisory`.
- The blocked class is now broader than Candidate 2's always-wide first block:
  even a delayed 64-byte trusted scanner is not an admissible retained parse
  close on this baseline. The next Wave 2 retained intervention should leave
  string scanning alone unless a fresh profile names a non-wide, non-sidecar
  string boundary. The current admissible fallback is the parser-control
  `ContainerNext` / next-byte carry candidate from
  `GRAND-SYNTHESIS-SK-V6.md` §8.

## SK-V6 Wave 2 Candidate-4 Redress

- Item 63 admits the `ContainerNext` / next-byte carry intervention. The route
  changes generated retained array parsing so the first element is parsed once,
  then array separators are consumed by `consume_array_next`; after a comma the
  next value's first byte is carried directly into `dispatch_value` instead of
  re-entering `parse_value_at`. This keeps Lock 1 intact: no retained sidecar,
  no second source pass, no new BIR variant, and no grammar directive.
- The same change landed in both the checked-in generated runtime and the
  `codegen` JSON template so regeneration preserves the hot path. The helper is
  grammar-shaped in generated JSON code only; it does not place JSON logic in
  `bbnf-simd`, `parse-that-regex`, or generic lowering crates.
- Production `profile-lazy` smoke used baseline and candidate release binaries
  built from the same tree under `/tmp/skv6-wave2-candidate4-smoke`. Each row
  reports the median of three repetitions:

  | row | base c/B | candidate c/B | c/B delta | base Mbps | candidate Mbps | Mbps delta |
  |---|---:|---:|---:|---:|---:|---:|
  | apache_builds | 2.272912 | 2.236779 | -1.59% | 12319 | 12518 | +1.62% |
  | canada | 1.613182 | 1.457802 | -9.63% | 17357 | 19207 | +10.66% |
  | citm_catalog | 1.331241 | 1.255155 | -5.72% | 21033 | 22308 | +6.06% |
  | distinct_values | 4.486461 | 4.429679 | -1.27% | 6241 | 6321 | +1.28% |
  | github_events | 2.133333 | 2.101313 | -1.50% | 13125 | 13325 | +1.52% |
  | gsoc-2018 | 1.268748 | 1.262399 | -0.50% | 22069 | 22180 | +0.50% |
  | instruments | 2.338205 | 2.222222 | -4.96% | 11975 | 12600 | +5.22% |
  | marine_ik | 2.189039 | 1.977680 | -9.66% | 12791 | 14158 | +10.69% |
  | mesh | 2.066268 | 1.921625 | -7.00% | 13551 | 14571 | +7.53% |
  | numbers | 1.507159 | 1.365987 | -9.37% | 18578 | 20498 | +10.33% |
  | random | 3.521127 | 3.317929 | -5.77% | 7952 | 8439 | +6.12% |
  | twitter | 2.249719 | 2.232677 | -0.76% | 12446 | 12541 | +0.76% |
  | unicode_basic | 2.528445 | 2.459808 | -2.71% | 11074 | 11383 | +2.79% |
  | unicode_escapes | 2.292076 | 2.114484 | -7.75% | 12216 | 13242 | +8.40% |
  | unicode_mixed | 3.375934 | 3.049112 | -9.68% | 8294 | 9183 | +10.72% |
  | update_center | 2.949853 | 2.917882 | -1.08% | 9492 | 9596 | +1.10% |
  | y_string_unicode | 4.660453 | 4.516858 | -3.08% | 6008 | 6199 | +3.18% |

- A full `bench-json --advisory` pass then refreshed `skinny/RESULTS.md`. The
  retained Track 1 rows most directly targeted by the candidate moved as
  follows against the prior committed authority:

  | row | prior Track 1 Mbps | advisory Track 1 Mbps | delta |
  |---|---:|---:|---:|
  | citm_catalog | 20775 | 21811 | +5.0% |
  | canada | 17738 | 18036 | +1.7% |
  | apache_builds | 12341 | 12511 | +1.4% |
  | github_events | 13161 | 13184 | +0.2% |
  | marine_ik | 12818 | 13265 | +3.5% |
  | instruments | 11887 | 12532 | +5.4% |
  | numbers | 18740 | 19853 | +5.9% |
  | distinct_values | 6097 | 6144 | +0.8% |
  | y_string_unicode | 6084 | 6272 | +3.1% |

- The long advisory run showed run-order noise on `mesh`, `random`,
  `update_center`, `unicode_mixed`, and `unicode_escapes`, so the decision used
  a focused five-run side-by-side rerun at
  `/tmp/skv6-wave2-candidate4-focused.csv` and
  `/tmp/skv6-wave2-candidate4-update-center-focused.csv` for those rows:

  | row | median baseline Mbps | median candidate Mbps | median delta |
  |---|---:|---:|---:|
  | update_center | 9237 | 9356 | +1.29% |
  | mesh | 13332 | 14311 | +7.34% |
  | random | 7768 | 8270 | +6.46% |
  | unicode_mixed | 7765 | 8348 | +7.51% |
  | unicode_escapes | 11272 | 11930 | +5.84% |
  | unicode_basic | 11001 | 11263 | +2.38% |
  | distinct_values | 6046 | 6082 | +0.60% |
  | y_string_unicode | 5843 | 6041 | +3.39% |

- The PC-level attribution gate was checked with `runtime/parse-attribution`
  builds and `samply` profiles under `/tmp/skv6-wave2-candidate4-profiles/`.
  The old redundant boundary set (`consume_container_next + parse_value_at +
  dispatch_value`) dropped from 24.97% to 14.51% self samples on
  `citm_catalog` and from 27.37% to 6.48% self samples on `canada`. Normalized
  by the profiled Mbps, that is approximately 0.121 -> 0.070 attributed ns/B
  on `citm_catalog` (-42%) and 0.172 -> 0.041 attributed ns/B on `canada`
  (-76%). The new `consume_array_next` helper is visible separately, as
  intended; it is the replacement boundary, not a residual re-entry cost.
- Correctness and build gates were green before admission: `cargo fmt --all`,
  `cargo test -p runtime --profile ax-iter`, `cargo build --workspace
  --profile ax-iter`, `cargo test --workspace --profile ax-iter`,
  `cargo run -p xtask --release -- check-json`, and `cargo run -p xtask
  --release -- check-conformance`. The stale prompt command
  `cargo run -p xtask --release -- gen --check` is not a live `xtask` command
  in this workspace; `check-json` is the on-disk regeneration/parity gate.
- Item 63 does not close SK-V6. It is an admissible throughput recovery on the
  honest generated runtime baseline: parse-G remains dominated by string /
  Unicode and competitor-anchor gaps after the results refresh. The next Wave 2
  candidate must be selected from a fresh profile angle for the remaining
  string/Unicode cluster or from a direct-to-struct Wave 3 bridge if parse-G is
  deliberately left with falsifiability-tested residuals.

## SK-V6 Wave 2 Candidate-5 Redress

- Item 64 rejects the retained Unicode-escape run validator as shipped. The
  route added an AArch64 four-unit `\uXXXX` validation fast path inside
  `validate_json_unicode_escape_run`, using the existing
  `bbnf-simd::aarch64::unescape_uxxxx_x4_neon` primitive and falling back to
  scalar validation for invalid hex, non-contiguous runs, or a high surrogate
  at the fourth unit boundary. It also added a forwarded `parse-attribution`
  feature so the helper could be profiled as its own boundary. This stayed
  within the canonical substrate: no sidecar, no second source pass, no new BIR
  variant, and no grammar directive.
- Correctness was green before measurement:
  `CARGO_TARGET_DIR=/tmp/skv6-wave2-candidate5-target cargo test -p
  parse-that-regex --profile ax-iter` passed 24 unit tests, including new
  four-unit validation-shape coverage for BMP units, surrogate pairs, a
  boundary-crossing surrogate pair, lone low surrogates, bad high/low pairs,
  and invalid hex offsets.
- Production `profile-lazy` smoke used baseline and candidate release binaries
  built from the same tree under `/tmp/skv6-wave2-candidate5-base` and
  `/tmp/skv6-wave2-candidate5-cand`. Raw measurements are archived at
  `/tmp/skv6-wave2-candidate5-smoke.csv`; medians are archived at
  `/tmp/skv6-wave2-candidate5-smoke-summary.csv`.

  | row | median baseline Mbps | median candidate Mbps | median delta |
  |---|---:|---:|---:|
  | apache_builds | 12377 | 12510 | +1.07% |
  | canada | 19115 | 19165 | +0.26% |
  | citm_catalog | 21903 | 21798 | -0.48% |
  | distinct_values | 6070 | 6090 | +0.33% |
  | github_events | 13142 | 13149 | +0.05% |
  | gsoc-2018 | 21885 | 21922 | +0.17% |
  | instruments | 12271 | 12202 | -0.56% |
  | unicode_basic | 11180 | 11069 | -0.99% |
  | unicode_escapes | 12758 | 16818 | +31.82% |
  | unicode_mixed | 8786 | 8945 | +1.81% |
  | update_center | 9252 | 9330 | +0.84% |
  | y_string_unicode | 6133 | 5905 | -3.72% |

- The route failed its falsifiability gate. It cleared the
  `unicode_escapes >= +12%` focus threshold by a wide margin, but it regressed
  `y_string_unicode` instead of improving it by at least 8%, and neither
  `unicode_mixed` nor `gsoc-2018` reached the required +5% companion lift. The
  direct interpretation is that dense contiguous Unicode-escape runs are only
  the `unicode_escapes` row. `y_string_unicode` has short runs and
  boundary-crossing surrogate shapes that do not amortize the four-unit path;
  `unicode_mixed` and `gsoc-2018` are not primarily fixed-width Unicode-escape
  validation rows.
- The candidate was reverted before commit. The rejected patch is saved at
  `/tmp/skv6-wave2-candidate5-rejected.patch`. Do not reopen the same
  four-unit retained validator unless a new profile names a broader local fact
  than contiguous `\uXXXX` runs. The remaining retained parser-control
  candidate is object next-key carry; the higher-impact next direct route is
  the field-layout string materializer described in
  `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` §9.

## SK-V6 Wave 2 Candidate-6 Redress

- Item 65 rejects object next-key carry. The route mirrored the admitted array
  `ContainerNext` shape for retained object loops: after a comma it skipped
  whitespace, consumed and emitted the next key quote offset, and carried that
  quote state into `parse_pair_after_open_quote` instead of re-entering the
  generic pair/key prologue. The checked-in generated runtime and the codegen
  JSON template were both updated during the attempt; no generic crate or
  grammar directive changed.
- Correctness caught one loop-shape bug before measurement. The initial patch
  parsed the first pair at the top of every loop iteration and therefore
  attempted to parse a new key before checking `}`. After fixing the loop to
  parse the first pair once and drive subsequent pairs from
  `consume_object_next`, `CARGO_TARGET_DIR=/tmp/skv6-wave2-candidate6-target
  cargo test -p runtime --profile ax-iter` passed all six runtime tests. The
  unused old `consume_container_next` helper was also removed during the
  attempt.
- Production `profile-lazy` smoke used baseline and candidate release binaries
  built from the same tree under `/tmp/skv6-wave2-candidate6-base` and
  `/tmp/skv6-wave2-candidate6-cand`. Raw measurements are archived at
  `/tmp/skv6-wave2-candidate6-smoke.csv`; medians are archived at
  `/tmp/skv6-wave2-candidate6-smoke-summary.csv`.

  | row | median baseline Mbps | median candidate Mbps | median delta |
  |---|---:|---:|---:|
  | apache_builds | 12260 | 12334 | +0.60% |
  | citm_catalog | 20229 | 20301 | +0.36% |
  | distinct_values | 6093 | 6066 | -0.44% |
  | github_events | 13050 | 13033 | -0.13% |
  | instruments | 12367 | 12236 | -1.06% |
  | random | 8292 | 8192 | -1.21% |
  | unicode_basic | 11204 | 11221 | +0.15% |
  | update_center | 9346 | 9391 | +0.48% |

- The route failed the written gate. It required `citm_catalog >= +3%`,
  `random >= +2%`, `instruments >= +2%`, and `update_center >= +1.5%`, with
  `distinct_values` regressing no more than 1%. Only the guard bound held.
  Object key carry therefore does not buy enough production throughput to
  justify another retained parser-control admission.
- The candidate was reverted before commit. The rejected patch is saved at
  `/tmp/skv6-wave2-candidate6-rejected.patch`. With REDRESS 60-65, the SK-V6
  retained parse Wave 2 shortlist is exhausted under the current single-
  substrate rules: tiny-probe deletion, always-wide scanning, delayed-wide
  scanning, Unicode-escape run validation, and object next-key carry all have
  same-row falsification, while array `ContainerNext` was admitted but did not
  close parse-G. The next admissible implementation route is the generated
  direct field-layout string materializer from
  `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` §9.

## SK-V6 Wave 3 Candidate-7 Redress

- Item 66 rejects the direct source-hook field-layout materializer. The route
  added direct-only `JsonSink` source hooks (`key_direct_source`,
  `string_direct_source`, `array_string_direct_source`,
  `object_string_direct_source`) and changed the generated SinkOnly direct
  parser to call those hooks. `JsonDigestSink` then overrode the hooks to fold
  keys and string scalar values directly into the current object/array/root
  digest frame, avoiding the existing closure receiver path while preserving
  the canonical `unescape_json_string` allocation path for escaped strings.
  This stayed within the direct generated runtime surface: no new directive, no
  BIR variant, no generic-crate JSON leakage, and no parallel source pass.
- Correctness was green before measurement:
  `CARGO_TARGET_DIR=/tmp/skv6-wave3-candidate7-target cargo test -p runtime
  --profile ax-iter` passed six runtime tests, and
  `CARGO_TARGET_DIR=/tmp/skv6-wave3-candidate7-target cargo test -p bbnf-bench
  --profile ax-iter` passed 23 bench tests.
- Production `profile_direct` smoke used baseline and candidate release
  binaries built from the same tree under
  `/Users/mkbabb/Programming/bbnf-lang-skv6-candidate7-base` and the main
  candidate workspace. Raw measurements are archived at
  `/tmp/skv6-wave3-candidate7-direct-smoke.csv`; medians are archived at
  `/tmp/skv6-wave3-candidate7-direct-smoke-summary.csv`.

  | row | median baseline Mbps | median candidate Mbps | median delta |
  |---|---:|---:|---:|
  | unicode_escapes | 4447 | 4491 | +0.99% |
  | unicode_mixed | 3726 | 3730 | +0.11% |
  | y_string_unicode | 3364 | 3423 | +1.75% |
  | distinct_values | 6152 | 6247 | +1.54% |
  | gsoc-2018 | 14680 | 14678 | -0.01% |

- The route failed the written Wave 3 gate from
  `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` §9. It required
  `unicode_escapes >= +20%`, `unicode_mixed >= +15%`, at least two of
  `y_string_unicode`, `distinct_values`, and `gsoc-2018 >= +8%`, and no direct
  row regressing by more than 5%. None of the required lift thresholds fired.
  The direct interpretation is that receiver/closure removal is too small to
  close direct string/Unicode rows; the dominant cost remains escaped-string
  decode/materialization and generated direct parser control, not the trait
  hook call shape itself.
- The candidate was reverted before commit. The rejected patch is saved at
  `/tmp/skv6-wave3-candidate7-rejected.patch`. Do not reopen direct source-hook
  folding under another name. The next direct route must materially change the
  escaped-string materialization shape without repeating REDRESS 54
  sink-local decoded stats, REDRESS 55 quote-source streaming hash, or this
  direct source-hook receiver shortcut.

## SK-V6 Wave 3 Candidate-8 Redress

- Item 67 rejects parser-owned decoded scratch for generated direct escaped
  strings. The route added
  `materialize_json_string_at_quote_trusted_utf8_into` to
  `parse-that-regex`, forwarded `runtime/parse-attribution` into
  `parse-that-regex`, threaded one reusable `String` scratch through generated
  `parse_direct`, and changed escaped strings to call the normal semantic sink
  methods (`key`, `string`, `array_string`, `object_string`) with
  `scratch.as_str()`. Plain strings stayed on the borrowed path. The generated
  renderer template was updated in lockstep with
  `runtime/src/grammars/json/generated.rs`.
- Correctness was green before measurement:
  `CARGO_TARGET_DIR=/tmp/skv6-candidate8-correctness cargo test -p
  parse-that-regex --profile ax-iter` passed 24 tests,
  `cargo test -p runtime --profile ax-iter` passed six tests,
  `cargo test -p bbnf-bench --profile ax-iter` passed 23 tests,
  `cargo run -p xtask --release -- check-json` passed, and
  `cargo run -p xtask --release -- check-conformance` accepted 21 valid
  fixtures and rejected seven invalid fixtures.
- Production `profile_direct` smoke used baseline and candidate release
  binaries built from the same HEAD under
  `/Users/mkbabb/Programming/bbnf-lang-skv6-candidate8-base` and the main
  candidate workspace. Because the primary escaped row immediately regressed by
  a decisive margin, the longer guard run was stopped after the escaped target
  rows plus a partial `y_string_unicode` sample. Raw logs and partial CSV are
  archived under `/tmp/skv6-candidate8-direct-smoke/`.

  | row | median baseline Mbps | median candidate Mbps | median delta | samples |
  |---|---:|---:|---:|---:|
  | unicode_escapes | 4999 | 2798 | -44.03% | 5/5 |
  | unicode_mixed | 4541 | 4318 | -4.91% | 5/5 |
  | y_string_unicode | 3592 | 2990 | -16.76% | 2/1 |

- The route failed the written Wave 3 gate from
  `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` §10. It required
  `unicode_escapes >= +20%`, `unicode_mixed >= +15%`, and one companion
  signal. Instead, the parser-owned scratch path was 44% slower on the primary
  escaped row. The direct interpretation is that keeping `unescape_json_string`
  as a second pass is faster than folding semantic materialization into the
  generated parser's direct control path on this host. The baseline allocator
  cost was not the limiting factor; the direct parser now pays a heavier
  branch/control mix while still writing the same decoded bytes.
- The candidate was reverted before commit. The rejected patch is saved at
  `/tmp/skv6-wave3-candidate8-rejected.patch`. Do not reopen parser-owned
  decoded scratch under another name. The remaining admissible direct close
  needs a new local fact beyond allocation reuse or parser-owned decode:
  either a grammar-neutral decoded-string primitive that beats
  `unescape_json_string` as a standalone materializer with checkasm/scalar
  parity and same-wave generated consumer, or a different DirectBuild
  field-fact plan that changes what the direct workload must materialize while
  preserving strict semantic equality.

## SK-V6 Wave 3 Candidate-9 Redress

- Item 68 rejects byte-output `unescape_json_string` materialization. The route
  kept the public `unescape_json_string(raw_content) -> Cow<str>` API and the
  existing generated direct consumer, but changed the escaped-string body from
  incremental `String::push_str` / `String::push(char)` writes to a `Vec<u8>`
  writer. Plain segments used `extend_from_slice`, simple escapes pushed raw
  bytes, Unicode scalars encoded into a small stack buffer, and the finished
  buffer became an owned `String` after a debug UTF-8 assertion. No generated
  parser, direct sink, BIR surface, directive, or retained substrate changed.
- Correctness was green before measurement:
  `CARGO_TARGET_DIR=/tmp/skv6-candidate9-correctness cargo test -p
  parse-that-regex --profile ax-iter` passed 22 tests,
  `cargo test -p runtime --profile ax-iter` passed six tests,
  `cargo test -p bbnf-bench --profile ax-iter` passed 23 tests,
  `cargo run -p xtask --release -- check-json` passed, and
  `cargo run -p xtask --release -- check-conformance` accepted 21 valid
  fixtures and rejected seven invalid fixtures.
- Production `profile_direct` smoke used baseline and candidate release
  binaries built from the same HEAD under
  `/Users/mkbabb/Programming/bbnf-lang-skv6-candidate9-base` and the main
  candidate workspace. The primary escaped row falsified the route after five
  paired samples, so the longer guard run was stopped. Raw CSV is archived at
  `/tmp/skv6-candidate9-direct-smoke.csv`; full raw process output is archived
  at `/tmp/skv6-candidate9-direct-smoke.raw`; the hand summary is archived at
  `/tmp/skv6-candidate9-direct-smoke-summary.csv`.

  | row | median baseline Mbps | median candidate Mbps | median delta | samples |
  |---|---:|---:|---:|---:|
  | unicode_escapes | 4970 | 4771 | -4.00% | 5/5 |
  | unicode_mixed | 4513 | 4491 | -0.49% | 1/1 |

- The route failed the written Wave 3 gate from
  `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` §11. It required
  `unicode_escapes >= +8%`, `unicode_mixed >= +5%`, and
  `y_string_unicode >= +3%` or a direct attribution proof that
  `unescape_json_string` self-time fell by at least 20%. Instead, the primary
  escaped row regressed by 4.00% with a same-HEAD baseline. The direct
  interpretation is that `String`'s current UTF-8 appends are not the direct
  row bottleneck; replacing them with manual byte writes adds enough control
  and finalization overhead to lose even before guard rows are exhausted.
- The candidate was reverted before commit. The rejected patch is saved at
  `/tmp/skv6-wave3-candidate9-rejected.patch`. Do not reopen byte-output
  escaped-string materialization inside the current `Cow<str>` API. With
  REDRESS 66-68, the direct-string allocation / receiver / byte-writing family
  is exhausted under the current direct digest workload. The next admissible
  Wave 3 route must change DirectBuild field facts or the direct workload's
  representation contract while preserving strict semantic equality, not merely
  the local escaped-string writer.

## SK-V6 Wave 3 Candidate-10 Redress

- Item 69 rejects DirectBuild semantic string field facts as implemented. The
  route extended the existing `DirectBuildField` payload with a generic
  materializer enum, populated JSON string fields with `SemanticStringFact`,
  preserved that fact through `SinkOnlyProgram` validation, and changed
  generated direct string/key calls to fact-aware sink methods. The
  `JsonDigestSink` override then computed semantic length/fingerprint facts
  without materializing a decoded `String` for Track 1. No directive, top-level
  BIR variant, retained side table, or parallel source scan was added.
- Correctness was green before measurement:
  `CARGO_TARGET_DIR=/tmp/skv6-candidate10-correctness cargo test -p runtime
  --profile ax-iter` passed six tests,
  `cargo test -p bbnf-bench --profile ax-iter` passed 23 tests,
  `cargo run -p xtask --release -- check-json` passed, and
  `cargo run -p xtask --release -- check-conformance` accepted 21 valid
  fixtures and rejected seven invalid fixtures.
- Production `profile_direct` smoke used baseline and candidate release
  binaries built from the same HEAD under
  `/Users/mkbabb/Programming/bbnf-lang-skv6-candidate10-base` and the main
  candidate workspace. The primary escaped row failed decisively after two
  paired samples, so the guard run was stopped. Raw CSV is archived at
  `/tmp/skv6-candidate10-direct-smoke.csv`; raw process output is archived at
  `/tmp/skv6-candidate10-direct-smoke.raw`; the hand summary is archived at
  `/tmp/skv6-candidate10-direct-smoke-summary.csv`.

  | row | baseline samples Mbps | candidate samples Mbps | average delta | samples |
  |---|---:|---:|---:|---:|
  | unicode_escapes | 4751; 4988 | 4143; 4115 | -15.22% | 2/2 |

- The route failed the written Wave 3 gate from
  `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` §12. It required
  `unicode_escapes >= +20%`, `unicode_mixed >= +15%`, and two secondary rows
  at `>= +8%`. Instead, the primary row regressed by roughly 15% immediately.
  The direct interpretation is narrow but important: carrying semantic string
  facts through `DirectBuild` is architecturally cleaner, but the attempted
  streaming semantic fact consumer repeats the cost class of REDRESS 54/55 in
  production. The remaining direct gap is not closed by replacing contiguous
  decoded-string hashing with one-pass semantic fact hashing under the current
  digest representation.
- The candidate was reverted before commit. The rejected patch is saved at
  `/tmp/skv6-wave3-candidate10-rejected.patch`. Do not reopen semantic string
  fact hashing for the current direct digest workload unless a new profile
  names a different consumer representation than decoded byte length plus
  fingerprint. With REDRESS 66-69, the generated direct string/Unicode close is
  exhausted under the current strict digest workload. The next admissible move
  is a new research tranche on the direct output contract itself: either a real
  typed-struct workload with field-specific access patterns, or an explicit
  decision that the synthetic digest workload is a SOTA stressor rather than a
  representative DirectBuild closure gate.

## SK-V6 Wave 3 Candidate-11 Redress

- Item 70 rejects the first `real_typed_struct` implementation as a SOTA close.
  The route added a separate typed-output profile mode for `twitter` and
  `update_center`, strict owned Rust structs shared by generated Track 1,
  independent Track 2, sonic-rs, and serde_json, and post-parse checksums over
  the owned output. Track 1 called `runtime::generated_json::parse_direct`;
  Track 2 used a structurally independent recursive parser. No directive, BIR
  variant, retained side table, or parallel source scan was added.
- Correctness was green: `CARGO_TARGET_DIR=/tmp/skv6-c11-target cargo test -p
  bbnf-bench --profile ax-iter real_typed_struct -- --nocapture` passed the
  `twitter` and `update_center` parity tests across generated Track 1,
  independent Track 2, serde_json, and sonic-rs.
- The initial `serde_json::Value`-then-typed materializer was not close:

  | row | Track 1 Mbps | sonic-rs Mbps | ratio |
  |---|---:|---:|---:|
  | twitter | 3309 | 6286 | 0.53x |
  | update_center | 2018 | 3812 | 0.53x |

- A same-loop `UpdateCenterSink` typed builder removed the Value materializer
  for generated Track 1. It lifted `update_center` to roughly 2.74 Gbps with a
  serialization checksum, then to a 4.84 Gbps median after replacing checksum
  serialization with a zero-allocation post-parse structural checksum. The
  same measurement gave sonic-rs a 7.12 Gbps median and serde_json a 5.33 Gbps
  median. The candidate still failed the written scout gate: generated Track 1
  did not reach `sonic-rs * 1.10` on any fixture, and the independent Track 2
  path remained materially slower.

  | row | mode | median Mbps | notes |
  |---|---|---:|---|
  | update_center | generated Track 1 typed builder | 4845 | five 500-iteration samples: 4836 / 4880 / 4761 / 4816 / 4845 |
  | update_center | independent Track 2 typed path | 2661 | five samples: 2677 / 2661 / 2630 / 2687 / 2573 |
  | update_center | sonic-rs typed struct | 7117 | five samples: 7088 / 7212 / 7117 / 7165 / 7087 |
  | update_center | serde_json typed struct | 5327 | five samples: 5287 / 5351 / 5166 / 5327 / 5332 |

- PC attribution for the generated Track 1 typed builder is archived at
  `/tmp/skv6-c11-profiles/update_center_typed_track1.profile.json.gz`.
  The symbol table names `runtime::generated_json::parse_direct` /
  `parse_object_direct::<UpdateCenterSink>`, `UpdateCenterSink` event methods,
  `BTreeMap::insert`, typed-output drop, and, before the checksum rewrite,
  `serde_json::to_vec` / BLAKE3. Removing checksum serialization improved the
  absolute row but also revealed sonic-rs' typed parse headroom more clearly.
- The candidate was reverted before commit. The rejected patch is saved at
  `/tmp/skv6-wave3-candidate11-rejected.patch`. Do not treat a hand-authored
  typed sink over JSON as proof that grammar-only `DirectBuild` can emit an
  arbitrary user struct. The new finding is architectural: for JSON-class
  "direct to struct" comparisons, the output schema is not present in the JSON
  grammar. A conforming V1/SK receiver must name the schema source explicitly
  as a host/API type contract consumed by `DirectBuild` field facts, not as a
  hidden BBNF directive and not as a benchmark-private parser. Until that
  schema-source contract exists, `real_typed_struct` remains a measurement
  surface rather than a SOTA-close route.

## SK-V6 Wave 3 Candidate-12 Redress

- Item 71 accepts generated typed `DirectBuild` from a host/API output schema
  as the representative typed-output close for the two Wave 3 proof rows. The
  route added a grammar-neutral schema-source API in
  `codegen::direct_schema`, a `SinkOnlyProgram + DirectSchemaSet` lowerer, and
  a generated JSON typed parser renderer. The schema enters from xtask/host
  code, not from a BBNF directive; no new BIR variant, retained side table, or
  benchmark-private Track 1 parser was added. `DirectBuildField` gained an
  optional target payload, preserving the existing `DirectBuild { shape,
  fields }` variant.
- Correctness was green. `cargo test -p codegen --profile ax-iter`,
  `cargo test -p bbnf-bench --profile ax-iter real_typed -- --nocapture`,
  and `cargo run -p xtask --profile ax-iter -- check-real-typed` passed. The
  generated Track 1 typed output, structurally different Track 2 oracle,
  sonic-rs typed serde, and serde_json typed serde all produced equal
  post-parse checksums for `twitter` and `update_center`.
- The admitted implementation uses three schema-general payload facts:
  `MapEntriesVec { capacity_hint }` for object-as-entry-vector output,
  generated skip-only plain-string scanning for ignored fields, and explicit
  `ignored_fields` with skip kinds (`String`, `Array`, etc.) so known
  non-output fields do not fall through the generic `skip_value` dispatcher.
  These are host/API output facts and apply equally to CSS AST fields, CSV row
  materializers, or Sheets formula nodes; JSON key names remain schema data.
- Candidate sub-routes measured and rejected before the accepted shape:

  | sub-route | result |
  |---|---|
  | Generated full-ish schema without capacity hints | Correct but slow: `update_center` Track 1 about 4.5 Gbps vs sonic about 5.2 Gbps; `twitter` about 9.0 Gbps vs sonic about 13.6 Gbps. |
  | `MapEntriesVec` full-ish schema only | Worsened the first generated shape: `update_center` about 3.85 Gbps vs sonic about 4.56 Gbps; `twitter` about 7.41 Gbps vs sonic about 10.88 Gbps. |
  | Raw key byte dispatch for struct fields | Rejected: `update_center` profile scout dropped from 11,537 Mbps to 11,273 Mbps. LLVM's string `match` lowering was better than the hand-emitted byte if-chain. |
  | Narrow selected-output `Plugin { name, version }` plane | Rejected as a close: the profile scout crossed slack, but Criterion made sonic-rs much faster and widened the gap (`update_center` Track 1 382.45 us vs sonic 278.63 us). |
  | Global 40-byte tiny string cap | Rejected: profile scout dropped `update_center` Track 1 to 10,918 Mbps. |
  | Skip-only 64-byte plain-string cap | Rejected: profile scout dropped `update_center` Track 1 to 10,919 Mbps. |

- The accepted shape is measured by Criterion on the same strict selected
  output plane:

  | row | Track 1 median | sonic-rs median | result |
  |---|---:|---:|---|
  | twitter `real_typed_struct` | 278.67 us | 422.12 us | PASS; generated Track 1 is faster than sonic-rs typed serde. |
  | update_center `real_typed_struct` | 354.15 us | 351.23 us | PASS; generated Track 1 is within the `sonic-rs * 1.10` time slack. |

- Gate accounting was corrected so `real_typed_struct` is not judged by the
  old maximal digest-stressor rule. The representative typed row gates
  generated Track 1 against sonic-rs/serde sidecars; Track 2 remains a
  structurally different oracle and is reported, but it is not the SOTA
  comparator. The old `direct_to_struct` row remains visible as
  `semantic_full_digest_stressor` and still reports N-direct failures. This is
  not a rename of that miss; it is an explicit split between a maximal semantic
  stressor and a real host/API typed-output premise.
