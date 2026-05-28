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

1. The retained parse/tape plane has 13 G rows across the expanded corpus
   and four A rows: `canada`, `mesh`, `marine_ik`, and `numbers`. The latest
   native cap-16 split makes generated Track 1 materially faster on several
   rows, but hand Track 2 remains below the S anchor on most of those rows, so
   the classifier correctly reports G rather than D/E. The miss is now a
   materialization-plan / Track 2 substrate-shape / cost-model issue, not a
   global tiny-string policy win.
2. `canada` parses faster than sonic-rs and no longer reports the stale
   **L / NO-GO** structural-floor failure. Item 56 is folded into the current
   full matrix: Canada structural-only scan reports 69075 Mbps against the
   40000 Mbps NEON floor. The retained non-G rows split into 4 A rows
   (`canada`, `mesh`, `marine_ik`, `numbers`). No retained D/E rows are
   present in the current classifier because Track 2 is below the substrate
   threshold on every other row.
3. The direct-to-struct workload is correctness-green
   (exact generated Track 1 / hand Track 2 digest equality; sonic-rs and
   serde_json shape parity) and now uses generated SinkOnly for Track 1. The
   prior bench-private SinkParser table is superseded. The
   `semantic_full_digest_stressor` pass rows are `citm_catalog`,
   `apache_builds`, `github_events`, and `instruments`; the
   other 13 direct digest rows remain N-direct guard blockers. The
   representative `real_typed_struct` rows for `twitter` and `update_center`
   pass under the host/API output-schema plane introduced by item 71.

The earlier original-triad and SK-V5 intermediate tables below remain useful
historical evidence, but they are not the current close condition. The full
17-fixture gate plus the Workloads table in `skinny/RESULTS.md` is now the
binding authority, and the report now renders an explicit output-plane column
beside strictness.

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
report's `canada` structural-only scan reports 69075 Mbps against a
40000 Mbps floor after item 56. The remaining retained parse misses are
runtime/materialization and event/tape-consumption gaps, not a scanner-floor
failure.

Historical direct-to-struct snapshot retained for comparison:

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
   landed, the direct-number/context-sink redress ran, and later SK-V6 refreshes
   updated the full matrix, the five current digest pass rows are
   `citm_catalog`, `apache_builds`, `github_events`, `instruments`, and
   `distinct_values`. The gate still appends outcome
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
   Cohort cites: `restart/skinny/tranches/sk-v5/research/skv5-D6-class-ab-novelty.md`
   and `restart/skinny/tranches/sk-v5/research/skv5-B1-parse-attribution.md`. This
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
   `restart/skinny/tranches/sk-v5/research/skv5-B2-direct-attribution.md` and
   `restart/skinny/tranches/sk-v5/research/skv5-D5-sinkonly-novelty.md`.

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
   `restart/skinny/tranches/sk-v5/research/skv5-D3-derive-shape-novelty.md`,
   `restart/skinny/tranches/sk-v5/research/skv5-D5-sinkonly-novelty.md`, and the
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
   `restart/skinny/tranches/sk-v5/SPEC.md`: parameterise each
   classifier on the alphabet, move JSON-specific data tables out of
   `bbnf-simd` into codegen-emitted `.data`, and admit the result through a
   Lock 14 remediation gate. Cohort cite:
   `restart/skinny/tranches/sk-v5/research/skv5-D4-simd-split-novelty.md`.

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
   `restart/skinny/tranches/sk-v5/SPEC.md`: complete the
   primitive-by-primitive split into Layer 1 `bbnf.asm` modules and reduce
   `lib.rs` to a vocabulary index. Cohort cites:
   `restart/skinny/tranches/sk-v5/research/skv5-D4-simd-split-novelty.md` and
   `restart/skinny/tranches/sk-v5/research/skv5-A5-grammar-generalization.md`.

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
   `restart/skinny/tranches/sk-v5/SPEC.md`: delete
   `crates/simd-scan/` outright. Cohort cite:
   `restart/skinny/tranches/sk-v5/research/skv5-D4-simd-split-novelty.md`.

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
   rows. Later SK-V6 redress refreshes this to four direct digest pass rows:
   `citm_catalog`, `apache_builds`, `github_events`, and
   `instruments`; item 71 adds representative `real_typed_struct` passes
   for `twitter` and `update_center`. Representative current residuals are
   `canada`, `mesh`, `numbers`, `unicode_mixed`, and `unicode_escapes`. The
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
   gate status for that point in the campaign: retained parse has 13 G rows
   plus one Canada L row, and the direct workload has 16 NO-GO rows plus the
   `numbers` PASS row, with overall **N-direct / NoGo**. Items 56, 71, and 72
   later supersede this row: the current full matrix clears Canada L, records
   13 retained G rows, records four direct digest pass rows, and records
   representative `real_typed_struct` passes for `twitter` and
   `update_center`.
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

   Item 57's full `bench-json --advisory` refresh incorporates this slice;
   later native SK-V6 refreshes report Canada structural scan at 69075 Mbps
   against the 40000 Mbps NEON
   floor. The expanded retained parse rows still carry 13 G / NoGo failures,
   and direct-to-struct remains `N-direct / NoGo`. The admitted conclusion is
   exactly bounded: the Canada structural-scan floor is no longer the active
   blocker after this slice; the remaining close belongs to single-substrate
   event/tape consumption, string/Unicode projection, and direct field-layout
   materialization.

## Sonic Closeness

The parser works as the tape/direct hybrid the spec requires, but the current
full gate is not SOTA-close enough to dispatch. The expanded corpus is now the
authority for SOTA-BEAT: retained parse has 13 G rows and four A rows, and
Canada structural scan is green. The common parse blocker is source/tape event
consumption, Track 2 substrate-shape parity, and string/Unicode projection,
not tape payload writes or the Canada structural floor.

Direct-to-struct remains explicitly classified after the generated SinkOnly
rewrite. The workload now proves generated typed sink correctness, not merely
view projection or a bench-private parser. It moved the attribution to the
right symbol paths but did not close the SOTA gap: the latest full run reports
four direct rows (`citm_catalog`, `apache_builds`, `github_events`,
`instruments`) within the 1.10 sonic-rs time slack. Generated source hooks now preserve raw string spans
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
  structural-only scan reports 69075 Mbps against the 40000 Mbps NEON floor.
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
  13 G rows and four A rows. The full gate remains
  `N-direct / NoGo`.
- The report now renders the actual fastest-anchor `S` comparator rather than
  only sonic-rs; conformance and SIMD parity metadata gates are executable.
- `bbnf-simd` is now the scanner crate used by runtime and bench, with
  byte-level and corpus parity tests under `crates/bbnf-simd/tests/`.
- Skinny and full specs now use the prototype workspace result path
  `skinny/RESULTS.md` for the runnable prototype, with `restart/skinny/` kept
  as spec authority.
- Current expanded-corpus parse gate: `skinny/RESULTS.md` records 13 G rows
  and four A rows (`canada`, `mesh`, `marine_ik`, `numbers`).
- Current direct-to-struct workload gate: correctness passes, Track 1 now
  calls generated `parse_direct`, Track 2 is an independent hand-coded
  SinkOnly parser, and the `semantic_full_digest_stressor` rows
  `citm_catalog`, `apache_builds`, `github_events`, and `instruments` pass
  the sonic-rs `1.10x` time slack. The overall gate reports `N-direct / NoGo`
  because 13 direct digest rows still miss.
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
  scan at 69075 Mbps. This is scan-floor credit only; it does not close
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
  cargo xtask bench-json --advisory`. Later SK-V6 gate refreshes supersede that
  snapshot: the current `skinny/RESULTS.md` records 13 retained G rows and a
  green Canada structural scan above the 40000 Mbps NEON floor.
- Direct-to-struct improves from the prior single passing row to four digest
  passing rows: `citm_catalog`, `apache_builds`, `github_events`, and
  `instruments`; REDRESS 71 adds representative
  `real_typed_struct` passes for `twitter` and `update_center`. The overall
  gate remains `N-direct / NoGo`: 13 digest rows still miss sonic-rs direct by
  the 1.10x time-slack rule, with the hardest misses concentrated in
  `canada`, `mesh`, `numbers`, `unicode_mixed`, and `unicode_escapes`.
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
  the post-SK-V5 `skinny/RESULTS.md` baseline as amended by later SK-V6
  redress: full gate `N-direct / NoGo`, 13 retained G rows,
  four direct digest pass rows, 13 direct digest red rows,
  and representative `real_typed_struct` passes for `twitter` and
  `update_center`. Canada structural scan is green against the 40000 Mbps NEON
  floor. The SK-V6 prompt originally requested entries 57/58, but item 57 is
  already committed to direct receiver/source redress; the ledger stays
  monotonic and records the SK-V6 additions as 58/59.
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
  `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` §9.

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
  `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` §9.

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
  `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` §9. It required
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
  `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` §10. It required
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
  `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` §11. It required
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
  `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` §12. It required
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

## SK-V6 Wave 2 Candidate-13 Redress

- Item 72 admits widening the retained `match_tiny_plain_string` scalar probe
  from 8 bytes to 16 bytes only for generated retained `OffsetTape` parsing.
  The first redress text rejected the route after a nonnative Criterion run.
  That was not binding: `BENCH.md` requires `RUSTFLAGS="-C target-cpu=native"`,
  and the native rerun showed the generated retained parser improving the named
  rows without tripping the parse guard set. The route remains admissible
  because it keeps the same parse substrate, adds no BBNF directive, adds no
  BIR variant, introduces no side table, and does not wire the rejected NEON
  `match_tiny_plain_string` kernel from REDRESS 28/33.
- The scout loop was positive under `target/release/profile-lazy`:

  | row | cap 8 Mbps | cap 16 Mbps | delta |
  |---|---:|---:|---:|
  | twitter | 12133 | 15519 | +27.9% |
  | citm_catalog | 21999 | 30662 | +39.4% |
  | canada | 19255 | 18765 | -2.5% |
  | apache_builds | 12288 | 12610 | +2.6% |
  | github_events | 13040 | 15207 | +16.6% |
  | update_center | 9364 | 11517 | +23.0% |
  | mesh | 14526 | 14433 | -0.6% |
  | random | 8148 | 9928 | +21.8% |
  | gsoc-2018 | 21900 | 22787 | +4.1% |
  | instruments | 12364 | 17112 | +38.4% |
  | marine_ik | 13934 | 14072 | +1.0% |
  | numbers | 20733 | 20541 | -0.9% |
  | distinct_values | 6090 | 9674 | +58.8% |
  | unicode_mixed | 8761 | 8749 | -0.1% |
  | unicode_escapes | 12702 | 12681 | -0.2% |
  | unicode_basic | 11208 | 12286 | +9.6% |
  | y_string_unicode | 6044 | 6073 | +0.5% |

- The binding native Criterion run overturned the nonnative rejection for
  generated retained Track 1. Filtered
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench -- track1_generated`
  measured the intended and guard rows as improvements or bench noise:

  | row | native cap-16 effect |
  |---|---:|
  | twitter | +27.5% |
  | citm_catalog | +49.2% |
  | github_events | +16.9% |
  | update_center | +27.4% |
  | random | +21.8% |
  | gsoc-2018 | +5.7% |
  | instruments | +44.9% |
  | distinct_values | +57.5% |
  | unicode_basic | +9.9% |
  | numbers / unicode_mixed / unicode_escapes / y_string_unicode | within noise |

- The same native pass rejects a global cap-16 policy. When the 16-byte probe
  was applied to hand-coded retained Track 2, guard rows such as
  `apache_builds`, `github_events`, `gsoc-2018`, and `instruments` regressed;
  Track 2 was restored to the 8-byte probe. When the 16-byte probe was applied
  to generated direct `SinkOnly`, direct guard rows regressed (`instruments`
  -7.6%, `distinct_values` -24.6%, `y_string_unicode` -9.8%); direct parsing
  was split back to an 8-byte probe. The admitted shape is therefore explicit:
  generated retained `OffsetTape` uses cap 16, while generated direct
  `SinkOnly`, hand retained Track 2, and hand direct Track 2 use cap 8.
- The latest native `gate-json --advisory` records this split as 13 retained G
  rows and four retained A rows; the generated Track 1 improvement is real, but
  the row classifier remains G where Track 2 is below the substrate threshold.
  The next retained parse wave must profile why generated retained cap 16 beats
  hand Track 2 on rows such as `citm_catalog` rather than reapplying a global
  string threshold.

## SK-V6 Wave 2 Candidate-14 Redress

- Item 73 rejects the retained Track 2 array next-byte dispatch parity repair
  as tested. R8 identified a real helper-shape divergence: generated retained
  array parsing consumes the first element, then `consume_array_next` returns
  the next value byte so `dispatch_value(byte)` avoids a second value-entry
  byte load. Hand Track 2 used the older generic
  `consume_container_next(b']') -> bool` path and then called `parse_value_at`
  again. The candidate kept Track 2's 8-byte tiny string probe from item 72,
  added no directive, added no BIR variant, and touched only the independent
  hand-coded Track 2 retained parser.
- Correctness passed before measurement:
  `CARGO_TARGET_DIR=/tmp/skv6-cargo/main cargo test -p bbnf-bench --profile ax-iter track2::json::tests::emits_track1_compatible_offsets_without_calling_track1_parser`.
  The candidate was therefore a throughput-only test, not a parity break.
- The native Criterion falsifier failed early. Command:
  `CARGO_TARGET_DIR=/tmp/skv6-cargo/track2-array RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- "json/(citm_catalog|apache_builds|github_events|gsoc-2018|instruments|distinct_values|y_string_unicode)/(track1_generated|track2_handcoded)"`.
  `citm_catalog` Track 2 improved relative to the R8 comparator snapshot
  (about 17375 Mbps -> about 20310 Mbps), but the first guard row
  `apache_builds` regressed decisively (about 10475 Mbps -> about 7490 Mbps).
  That violates the written guard of no >2% slowdown on
  `apache_builds`, `github_events`, `gsoc-2018`, or `y_string_unicode`, so the
  run was stopped before the remaining rows and the candidate was reverted.
- The measured lesson is narrow. Generated retained's array continuation shape
  is not a free Track 2 parity repair while Track 2 remains a method-based hand
  parser: it can help array-heavy `citm_catalog` but hurts object-heavy guard
  rows through inlining/code-layout or branch-shape effects. Future Track 2
  work must profile the hand parser's code layout directly; it must not assume
  generated helper shape transfers monotonically to the hand comparator.

## SK-V6 asmjson/DAV1D synthesis redress

- Item 74 records the twelve-agent asmjson/DAV1D and generalization pass as a
  spec redress, not a code intervention. Reports are archived under
  `restart/skinny/tranches/sk-v6/research/skv6-A*.md` and
  `restart/skinny/tranches/sk-v6/research/skv6-B*.md`; synthesis and handoff live at
  `restart/skinny/tranches/sk-v6/SYNTHESIS.md`,
  `restart/skinny/tranches/sk-v6/SPEC.md`, and
  `restart/skinny/tranches/sk-v6/HANDOFF.md`.
- Binding findings: asmjson is a JSON-specific DPDA architecture reference
  (chunk byte-class masks, next-set-bit seeking, direct-threaded finite
  control, bounded stack), not a new BBNF directive and not a permissive strict
  S anchor. The transferable shape is grammar-neutral `CollapsedStage` selected
  by cost facts, emitted tables, admitted primitives, and same-plane strictness.
  DAV1D/FFmpeg/VLC contribute the required process: scalar executable specs,
  forced feature masks, register-clobber checks, stack canaries, cycle counters,
  and same-wave consumers.
- Item 75 records the comparator-plane correction forced by the same pass.
  `sonic-rs` rows built with `utf8_lossy` cannot be strict S anchors; they must
  be removed from strict rows or marked `flaw_probe`. bbnf retained parse rows
  that validate UTF-8 at the view boundary remain `deferred` or
  `strict_after_utf8_view` until scan-boundary validation is measured. asmjson
  SWAR/permissive rows are flaw probes unless a strict row on the same hardware
  and output plane is produced. Future `RESULTS.md` rows use schema v3 with
  strictness, output, ownership, feature mask, API symbol, corpus hash,
  hardware, build flags, sidecar freshness, and primitive status.
- Item 76 records the C-pass profiling/generality refinement. C1/C5 split the
  retained misses into short/plain rows and escape-tail rows and nominate a
  per-`\uXXXX` table/TBL classifier inside the existing retained string path;
  this is distinct from the rejected four-contiguous-unit validator. C2
  nominates a generated `mesh` `real_typed_struct` DirectBuild candidate as the
  first product-plane expansion beyond `twitter` and `update_center`. C3
  confirms the active `sonic-rs` bench dependency enables global `utf8_lossy`,
  making current sonic rows strict-anchor ineligible. C4 keeps PMULL, CSSC,
  DotProd, SVE/SME, and x86 AVX-512 work unadmitted until exact profiles point
  there. C6 assigns remaining grammar-name leaks in `passes`, `codegen`, and
  `parse-that-regex` to SK-V6 Wave 4; `runtime/tape` is clean.

## SK-V7 Wave 0 Comparator-Plane Redress

- Item 77 admits the `sonic-rs` strict feature repair and rejects the W0
  row-flip forecast. The bench dependency now enables only `sort_keys`; the
  feature-tree proof showed no `utf8_lossy`, so the repaired sonic rows no
  longer use the lossy UTF-8 comparator plane rejected by item 75.
- Measurement completed with the intended W0 commands. `cargo bench -p
  bbnf-bench --bench json_parity` completed on the strict rebuild.
  `cargo run -p bbnf-bench --bin gate --release` refreshed
  `skinny/RESULTS.md` and exited 5 because the overall gate remains
  `N-direct / NoGo`.
- The falsifiability forecast missed. sonic-rs parse deltas against the W0 plan
  baseline ranged from -14.6% to +18.5% rather than the expected uniform 3-8%
  slowdown. `instruments` stayed `G/NO-GO` and moved 92.0% -> 91.6% Track 1/S;
  `unicode_basic` stayed `G/NO-GO` and moved 91.7% -> 76.2% Track 1/S. No parse
  row reclassified.
- Because reverting would restore the known comparator flaw, the strict feature
  repair stays as the honest baseline. W1 is not opened from this run. The next
  candidate is W0b: same-run schema-v3 comparator reporting that records
  strict/lossy provenance and the missing yyjson/asmjson/RapidJSON fields
  before a fresh row-close decision.

## SK-V7 Wave 0b Schema-v3 Telemetry Redress

- Item 78 admits the schema-v3 telemetry row builder and same-run sonic-rs
  strict/lossy provenance. The legacy results table is replaced with the
  PASS-ALPHA schema-v3 named column surface, Criterion metadata is bumped to
  schema `3`, parse rows use `Workload=parse_only`, and `skinny/RESULTS.md`
  now records strictness, UTF-8 boundary, flaw-probe, output-plane, comparator,
  sidecar, hot-leaf, and signal provenance in one row surface.
- The Cargo feature-plane repair from item 77 remained intact. The verification
  command
  `cargo tree -p bbnf-bench --edges=features | rg 'sonic-rs|utf8_lossy|sort_keys'`
  showed only `sonic-rs feature "sort_keys"` and `sonic-rs v0.5.8`; no
  `utf8_lossy` feature was reintroduced. The lossy sonic column is produced by
  an explicit same-run `Deserializer::utf8_lossy()` benchmark row and is marked
  as flaw-probe provenance, not as a strict classification anchor.
- Measurement completed with the W0b gate commands. `cargo test -p bbnf-bench`
  passed 26 tests. `cargo bench -p bbnf-bench --bench json_parity` completed
  the full JSON corpus sweep. `cargo run -p bbnf-bench --bin gate --release`
  regenerated `skinny/RESULTS.md` and exited 5 only because the current
  measured authority remains `N-direct / NoGo`. `cargo run -p xtask --release
  -- gate-json` reached the same schema-v3 gate and failed only by wrapping the
  gate's performance exit.
- The row-close guard held. `instruments` remains measurement-classified:
  parse `K / NO-GO` at Track 1 18038 Mbps, sonic strict 16312 Mbps, sonic lossy
  18747 Mbps; direct `N-direct / NO-GO` at Track 1 11972 Mbps and sonic strict
  12673 Mbps. `unicode_basic` remains honest under the new schema: parse
  `K / NO-GO` at Track 1 11416 Mbps, sonic strict 15596 Mbps, sonic lossy
  15625 Mbps; direct `A / GO` at Track 1 8576 Mbps and sonic strict
  8502 Mbps. Schema reshaping did not create a performance admission.
- Residual caveat: `Delta vs SK-V6` is present but explicitly rendered as
  `n/a` because W0b has no machine-readable SK-V6 baseline binding. That is an
  honest reporting limitation, not an inferred performance result. W1 may open
  with schema-v3 reporting in place; it must not treat W0b as a parser/runtime
  throughput improvement.

## SK-V7 Wave 1 TapeKind Rename Redress

- Item 79 admits the descriptor-preserving TapeKind rename. Generic IR no
  longer exposes the seven JSON-shaped `TapeKind` variant spellings:
  `Object`, `Array`, `Pair`, `String`, `Number`, `Bool`, and `Null` are now
  `Container`, `Sequence`, `KeyValuePair`, `StringValue`, `NumberValue`,
  `BoolValue`, and `NullValue`. `DirectBuildDecode::{JsonString,JsonNumber}`
  are now `DirectBuildDecode::{EscapedString,NumberScalar}`.
- The old `passes::materialization_for_rule` helper is deleted. Behavior is
  preserved by a single local materialization descriptor that returns the
  renamed `TapeKind`, existing `Json*` DirectBuild shape string, and existing
  field roster together for the seven currently materialized rules. This is
  intentionally not the later W7/W8 broad codegen/parse-that-regex Lock 14
  rebrand.
- A focused passes regression test now checks all seven materialized JSON value
  rules for the renamed neutral `TapeKind`, existing shape name, and field
  roster. This closes the previous gap where only `object` was asserted for
  `TapeEmit` plus `DirectBuild`.
- Verification completed. Exact old-symbol proof:
  `rg -n 'TapeKind::(Object|Array|Pair|String|Number|Bool|Null)\b|DirectBuildDecode::(JsonString|JsonNumber)\b|fn materialization_for_rule\b' skinny/crates`
  returned no matches. `cargo test -p passes` passed 5 tests. `cargo run -p
  xtask --release -- check-json` passed. `cargo run -p xtask --release --
  check-real-typed` passed. `cargo test --workspace` passed.
- The non-behavior gate held. `skinny/RESULTS.md`,
  `skinny/crates/codegen/src/json_templates/generated.rs`,
  `skinny/crates/runtime/src/grammars/json/generated.rs`, and
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs` have no diff. W1 did
  not claim a parser/runtime throughput change and did not reopen REDRESS
  28+33, 50-55, or 60-72.

## SK-V7 Wave 2 Zero-Fallback Mantissa-Widen Redress

- Item 80 rejects the W2 mantissa-widen route on the current W1 baseline.
  Fresh attribution over `canada.json` counted 111126 numbers, 111080 f64
  candidates, zero mantissa overflows, zero ambiguous Eisel-Lemire returns, and
  zero `str::parse::<f64>()` fallbacks. The measured fallback rate is 0.0000%,
  contradicting the stale handoff hypothesis that canada had a material f64
  fallback pool to eliminate.
- No source patch was attempted after the same-wave consumer disappeared. The
  rejected patch file exists at `/tmp/skv7-wave-2-rejected.patch` and is empty
  by construction. The Eisel-Lemire power table already covers the f64
  `[-342, 308]` exponent range, so table-only widening has no measured
  exponent miss to consume.
- Verification completed. `cargo test --workspace` passed. `cargo run -p xtask
  --release -- primitive-checkasm` passed. The scoped Criterion command
  `cargo bench -p bbnf-bench --bench json_parity --
  'json/(canada|numbers|mesh|marine_ik)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'`
  completed. `cargo run -p bbnf-bench --bin gate --release -- --advisory`
  refreshed `skinny/RESULTS.md` and exited 5 because the overall gate remains
  `N-direct / NoGo`.
- Measurement evidence:

  | Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | Outcome |
  |---|---:|---:|---:|---|
  | canada | 10773 | 10296 | 12421 | N-direct / NO-GO |
  | numbers | 12615 | 12362 | 12838 | A / GO |
  | mesh | 8798 | 8699 | 9902 | N-direct / NO-GO |
  | marine_ik | 9391 | 9349 | 8465 | A / GO |

- The next candidate shape is not a hidden W2 source edit. It is a fresh
  numeric-array scan/dispatch wave or sub-wave with profile evidence naming a
  concrete hot leaf such as `match_number_span_from_first` and generated
  array-number direct dispatch. The rejected routes in HANDOFF §3 remain
  blocked.

## SK-V7 Wave 3 Capacity-Hinted Numeric Vec Real-Typed Expansion Redress

- Item 81 admits the W3 capacity-hinted numeric Vec route. `DirectTypeRef::Vec`
  now carries `capacity_hint`, generated helper names include that hint to avoid
  collisions, Vec helpers allocate with `Vec::with_capacity`, and the typed
  direct scalar set includes `U32` for integer vector fixtures.
- The same-wave consumers are generated `real_typed_struct` rows for `mesh` and
  `marine_ik`. The schemas bind the actual fixture shapes rather than the stale
  sketch names: mesh consumes `positions`, `tex0`, `colors`, `influences`,
  `normals`, `indices`, and `batches`; marine_ik consumes
  `geometries[].data.{uvs,vertices,skinWeights,skinIndices,normals,faces}`.
  Checksums include the numeric vectors, so these are materialized typed rows,
  not skip-only rows.
- Verification completed. `cargo run -p xtask --release -- regen-real-typed`
  regenerated the bench consumer. `cargo run -p xtask --release --
  check-real-typed` passed. `cargo test -p codegen typed_direct` passed.
  `cargo test -p bbnf-bench real_typed -- --nocapture` passed. Full-fixture
  profile probes matched checksums across Track 1, Track 2, sonic-rs, and
  serde_json for both new rows. `cargo test --workspace` passed. The scoped
  Criterion commands for W3 rows completed, and `cargo run -p bbnf-bench --bin
  gate --release -- --advisory` refreshed `skinny/RESULTS.md`; it exited 5
  only because the overall skinny gate remains `N-direct / NoGo`.
- Measurement evidence:

  | Corpus | Workload | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | Outcome |
  |---|---|---:|---:|---:|---|
  | mesh | real_typed_struct | 9466 | 8089 | 8696 | A / GO |
  | marine_ik | real_typed_struct | 12020 | 9630 | 8750 | A / GO |
  | mesh | direct_to_struct guard | 8259 | 8483 | 8789 | A / GO |
  | twitter | real_typed_struct guard | 18513 | 16193 | 15486 | A / GO |

- The W3 gate is closed. This does not reopen the rejected V5/V6 retained-parse
  materializer routes, benchmark-private hand typed sink, or capacity prescan
  routes named in HANDOFF §3.

## SK-V7 Wave 4 Single-Quartet Unicode Escape Classifier Redress

- Item 82 rejects the W4 single-quartet Unicode escape classifier on the
  current W3 baseline. The patch moved the existing scalar `\uXXXX` decoder
  into `parse-that-regex/src/unicode/escape_decode.rs`, reused the existing
  `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` primitive for one
  quartet at a time, consumed it in both `decode_json_unicode_escape` and the
  `unescape_json_string` materializer, and added a primitive checkasm test for
  BMP, surrogate, and invalid-hex cases.
- Correctness and parity were green. `cargo test -p parse-that-regex
  unicode_escape -- --nocapture` passed. `cargo test -p bbnf-simd --release
  --test checkasm_unicode_escape` passed. `cargo run -p xtask --release --
  primitive-checkasm` passed with the new unicode escape checkasm route.
  `cargo test --workspace` passed. The rejected source patch is saved at
  `/tmp/skv7-wave-4-rejected.patch`.
- The falsifiability gate failed. The parse rows improved only the
  heavily-escaped `unicode_escapes` corpus and still missed the W4 threshold;
  the direct rows remained far below threshold, and `y_string_unicode`
  direct Track 2 regressed beyond the allowed guard.

  | Corpus | Workload | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | Threshold | Outcome |
  |---|---|---:|---:|---:|---:|---|
  | unicode_escapes | parse_only | 14516 | 14535 | 17671 | 95% | FAIL at 82.1% of sonic |
  | unicode_escapes | direct_to_struct | 5118 | 5255 | 12996 | 95% | FAIL at 39.4% of sonic |
  | y_string_unicode | parse_only | 6331 | 6053 | 12697 | 70% | FAIL at 49.9% of sonic |
  | y_string_unicode | direct_to_struct | 5093 | 3517 | 7952 | 70% | FAIL at 64.0% of sonic; Track 2 regressed 6.6% |

- The next candidate is not another per-quartet materializer helper and does
  not reopen the REDRESS 64 or REDRESS 66-69 escape-tail families. W5 proceeds
  through SPEC §7's 16-byte plain-string scan widening route, with
  `unicode_mixed` and `distinct_values` handled as part of that string-bound
  parse surface.

## SK-V7 Wave 5 Generated-Retained StringBlock16 Tiny Probe Redress

- Item 83 rejects the W5 generated-retained StringBlock16 tiny probe on the
  current W4-closed baseline. The candidate added a JSON-specific 16-byte
  wrapper over `bbnf-simd::aarch64::string_block::scan_string_special_block`,
  wired only the generated retained `match_tiny_plain_string_with_cap::<16>`
  helper in `runtime/src/grammars/json/generated.rs` and the JSON template, and
  kept the direct `CAP=8`, Track 2, parse-that-regex, old Class A TBL, UTF-8
  fusion, and materialization surfaces untouched.
- Correctness and parity were green. `cargo test -p bbnf-simd --release
  --test checkasm_string_block -- --nocapture` passed. `cargo run -p xtask
  --release -- primitive-checkasm` passed. `cargo run -p xtask --release --
  check-json` passed. `cargo test --workspace` passed. The rejected source and
  refreshed-results patch is saved at `/tmp/skv7-wave-5-b2-rejected.patch`.
- The falsifiability gate failed decisively: zero of six named parse rows
  crossed threshold, and every named Track 1 parse row regressed by more than
  the allowed 3% guard. The same-run advisory gate exited 5 because the overall
  skinny gate remains `N-direct / NoGo`; the W5 rejection is based only on the
  six focused W5 parse rows below.

  | Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | Threshold | Track 1 / sonic | Guard outcome |
  |---|---:|---:|---:|---:|---:|---|
  | twitter | 10076 | 11860 | 20550 | 90% | 49.0% | FAIL; Track 1 -36.0%, Track 2 -3.5% |
  | update_center | 7375 | 9086 | 19368 | 90% | 38.1% | FAIL; Track 1 -34.1% |
  | unicode_basic | 7173 | 10899 | 15719 | 100% | 45.6% | FAIL; Track 1 -37.2% |
  | random | 5524 | 7742 | 14193 | 85% | 38.9% | FAIL; Track 1 -43.8% |
  | unicode_mixed | 6646 | 8374 | 15466 | 85% | 43.0% | FAIL; Track 1 -17.3% |
  | distinct_values | 6111 | 6045 | 17629 | 85% | 34.7% | FAIL; Track 1 -8.2% |

- The failure mode is not semantic correctness; it is hot-leaf cost. The
  existing AArch64 `string_block` movemask shape is too expensive for the
  already-tiny generated retained quote-pair probe, so replacing the scalar
  16-byte loop with that wrapper increases per-string overhead. Do not reopen
  this exact wrapper route, and do not compensate by widening parse-that full
  string scanning or materialization routes blocked in HANDOFF §3. A future
  same-row candidate would need fresh PC-level evidence for a lower-overhead
  inline/asm first-special extractor that beats the scalar leaf before any
  generated parser wiring. W6 proceeds through SPEC §8's control/key
  compaction route.

## SK-V7 Wave 6 Object-Pair Value-Byte Control Compaction Redress

- Item 84 rejects the W6 object-pair value-byte control compaction on the
  current W5-closed baseline. The candidate changed the generated JSON
  template/runtime mirror so object key parsing returned the first value byte
  and dispatched it directly, then mirrored the same boundary reduction in the
  independent Track 2 hand parser. Track 2 also received the generated
  `consume_array_next` shape so array commas could dispatch without a second
  value-entry load.
- Correctness was green. `cargo run -p xtask --release -- check-json` passed
  after regenerating the runtime mirror. `cargo test -p bbnf-bench
  track2::json -- --nocapture` passed. `cargo run -p xtask --release --
  check-conformance` passed. `cargo test --workspace` passed. The rejected
  source and refreshed-results patch is saved at
  `/tmp/skv7-wave-6-control-key-rejected.patch`.
- The falsifiability gate failed. The focused same-run advisory gate kept both
  parse rows at `G / NO-GO` and `instruments` direct at `N-direct / NO-GO`.
  `citm_catalog` parse Track 2 remained below the 90% sonic threshold, and
  `instruments` parse Track 1 fell below the 100% sonic threshold. The
  candidate also violated the `citm_catalog` Track 1 no-regression guard.

  | Corpus | Workload | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | W6 target | Outcome |
  |---|---|---:|---:|---:|---|---|
  | citm_catalog | parse_only | 28930 | 20206 | 23794 | Track 2 >= 90% sonic | FAIL at 84.9%; Track 1 below 30831 no-regression floor |
  | citm_catalog | direct_to_struct | 21197 | 19825 | 20190 | guard PASS/no regression | PASS guard |
  | instruments | parse_only | 17827 | 12397 | 19192 | Track 1 >= 100% sonic | FAIL at 92.9% |
  | instruments | direct_to_struct | 12016 | 11123 | 12725 | Track 1 >= 100% sonic | FAIL at 94.4%; `N-direct / NO-GO` |

- The failure mode is not offset-shape correctness. It is control-boundary
  economics: removing one post-colon value-entry boundary is too small to close
  the Track 2-sensitive W6 rows, and the generated Track 1 layout became worse
  on `citm_catalog`. Do not reopen this exact value-byte return route, and do
  not compensate by reopening object next-key carry, separator elision,
  function-pointer dispatch, generic SWAR whitespace, EventCursor sidecars,
  or the W5 string leaf routes blocked in HANDOFF §3. The next viable B6
  candidate would need fresh PC-level evidence for a different same-row hot
  owner, most likely a direct-workload control path rather than another
  retained object-pair helper.

## SK-V7 Wave 7 Lock 14 Phase A+B Neutralization Redress

- Item 85 admits the W7 Lock 14 Phase A+B neutralization. Phase A removes the
  public JSON-prefixed `parse-that-regex` string/number matcher surface and
  migrates consumers to grammar-neutral `StringMatch`, `NumberSpan`,
  `StringMode::{Utf8,TrustedUtf8}`, `skip_ascii_whitespace`,
  `match_string_at_quote_trusted_utf8`, `unescape_string`, and
  `decode_unicode_escape`.
- Phase B removes the old `passes::compile()` JSON binding helpers
  `shapes_for_json`, `nominate_json`, and literal rule-name materialization.
  Materialization is now derived from grammar structure: regex span kind,
  direct literal terminals, direct rule-reference roles, and structural
  container/sequence/pair syntax. A renamed-rule regression proves the pipeline
  still derives the seven DirectBuild roles when none of the JSON rule names are
  present.
- Verification completed. `cargo test -p parse-that-regex -p passes -p
  codegen` passed. `cargo run -p xtask --release -- check-json` passed and left
  the generated runtime mirror consistent with the fixed direct-sink emitter.
  `cargo run -p xtask --release -- check-real-typed` passed. `cargo run -p
  xtask --release -- check-conformance` accepted 21 valid fixtures and rejected
  7 invalid fixtures. `cargo test --workspace` passed.
- The Lock 14 audit gates passed. The parse-that grep for public
  JSON-prefixed matcher types/functions, `StrictJson`, `StrictJsonTrustedUtf8`,
  `JsonStringMatch`, and `JsonNumberMatch` returned no matches. The passes grep
  for `shapes_for_json`, `nominate_json`, `materialization_for_rule`,
  `descriptor_for_rule`, `rule_by_name("json")`, `MissingEntry("json")`, and
  `StructuralAlphabet::json()` returned no matches. `skinny/RESULTS.md` has no
  diff, so W7 makes no throughput claim.
- The admitted scope does not reopen the W4-W6 rejected parser hot-leaf routes
  or any REDRESS 60-72 direct-materialization experiments. Remaining Lock 14
  work is the W8 Phase C+D codegen/IR cleanup already scoped by SPEC §10.

## SK-V7 Wave 8 Lock 14 Phase C+D Codegen Shell Neutralization Redress

- Item 86 admits the W8 Lock 14 Phase C+D codegen shell neutralization. The
  candidate renames the generic codegen renderer modules away from the
  `json_*_direct` surface, replaces the public `emit_json*` entry points with
  grammar-named `emit*` APIs, and migrates the same-wave `bbnf` and `xtask`
  consumers to pass the `json` grammar name explicitly.
- The schema/codegen shell no longer exposes JSON key or hard-coded JSON shape
  policy. `DirectFieldSchema` and ignored-field metadata now carry
  `key_literal`; schema-direct lowering validates the derived DirectBuild and
  literal fact sets instead of a `Json*` allowlist; sink-direct validation drops
  `REQUIRED_RULES`, `REQUIRED_SHAPES`, and JSON-specific diagnostics. The
  emitted JSON parser body remains byte-identical because W8 is a shell/IR
  neutralization, not a new route-fact substrate.
- Phase D removes `StructuralAlphabet::json()` and the JSON whitespace regex
  string-equality nullability special case from `ir`, replacing the latter with
  a generic single-atom quantified-shape check and an IR regression test.
- Verification completed. `cargo test -p codegen -p ir -p bbnf` passed.
  `cargo run -p xtask --release -- check-json` passed. `cargo run -p xtask
  --release -- check-real-typed` passed. `cargo run -p xtask --release --
  check-conformance` accepted 21 valid fixtures and rejected 7 invalid
  fixtures. `cargo test --workspace` passed. The root ancillary `cargo xtask
  regen --check` passed with 9 of 9 grammars clean.
- The falsifiability gate held. Generated JSON outputs stayed unchanged:
  `skinny/crates/runtime/src/grammars/json/generated.rs` and
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs` have no diff, and
  `skinny/RESULTS.md` has no diff. The W8 audit greps for JSON-named codegen
  modules/functions, schema key naming, schema-direct `Json*` allowlists,
  sink-direct required-rule/shape constants, JSON renderer diagnostics,
  `StructuralAlphabet::json()`, and the old whitespace regex equality guard all
  returned zero matches.
- The admitted scope does not reopen the W4-W6 rejected parse hot-leaf routes
  or the REDRESS 60-72 retained/direct-materialization routes. Remaining
  per-grammar JSON names are confined to the JSON grammar inputs and emitted
  JSON parser output that W8 intentionally kept byte-identical; W9 proceeds to
  the CostFacts substrate required before any further route-fact decisions.

## SK-V7 Wave 9 CostFacts Substrate Projection Redress

- Item 87 admits the W9 CostFacts substrate projection. The candidate adds a
  grammar-neutral `ir::cost` module with `CostFacts`, shape rationale,
  rejected alternatives, measurements, evidence sources, capacity policy, and
  priority steps. Measurement fields use scaled integers so the existing
  equality-derived layout facts remain viable.
- `passes::compile()` now populates `LayoutFacts.cost_facts` and derives
  `LayoutFacts.backend_shape` as a projection of `CostFacts.chosen`.
  The existing backend-shape decision predicates remain behavior-preserving,
  but each rule now records the selected priority and one rejected alternative
  per non-selected backend shape. REDRESS 72 is backfilled as evidence:
  generated retained `OffsetTape` string rules carry cap 16 as capacity policy,
  while the direct/Track 2 cap-16 regressions are recorded as
  `PreviouslyRegressed` alternatives sourced from `RedressBackfill`.
- `codegen` now threads CostFacts through `LowerCtx`, introduces the
  `ShapeLowering` trait, and selects lowerers from `CostFacts.chosen`.
  The current lowerer output strings and the generated JSON parser files remain
  byte-identical. `xtask gate-json --with-cost-facts --advisory` prints a
  single JSON report with schema `sk-v7-costfacts-v1`; unflagged `gate-json`
  keeps the existing bench-gate passthrough behavior.
- The W9 diagnostics are present and non-fatal:
  `BBNF-DOMINATED-ALTERNATIVE` records REDRESS-backed rejected alternatives,
  and `BBNF-COSTFACTS-MISSING-EVIDENCE` records rules without measurement
  evidence so silent defaults are visible without changing parser selection.
- Verification completed. `cargo test -p ir` passed. `cargo test -p passes`
  passed. `cargo test -p codegen` passed. `cargo run -p xtask --release --
  gate-json --with-cost-facts --advisory > /tmp/skv7-costfacts.json` produced
  parseable JSON; jq verified schema `sk-v7-costfacts-v1`, grammar `json`, 15
  CostFacts entries, at least four rejected alternatives per entry, REDRESS 72
  `PreviouslyRegressed` evidence, and both new diagnostic codes. `cargo run -p
  xtask --release -- check-json` passed. `cargo run -p xtask --release --
  check-real-typed` passed. `cargo run -p xtask --release -- check-conformance`
  accepted 21 valid fixtures and rejected 7 invalid fixtures. `cargo test
  --workspace` passed. The root ancillary `cargo xtask regen --check` passed
  with 9 of 9 grammars clean.
- The falsifiability gate held. `skinny/crates/ir/src/cost.rs` has no JSON,
  corpus, or comparator naming matches under the W9 grep. Generated JSON output
  files and `skinny/RESULTS.md` have no diff. W9 does not reopen REDRESS 50-72,
  REDRESS 28+33, W5's StringBlock16 wrapper, W6's object-pair value-byte
  compaction, or any pre-blocked route in HANDOFF §3; it records evidence only.

## SK-V7 Wave 10 Consumed AArch64 Bitmap Bodies and B6 Canary Fold Redress

- Item 88 rejects the first W10 consumed aarch64 bitmap bodies candidate. The
  rejected patch is archived at `/tmp/skv7-wave-10-rejected.patch`.
- The candidate added an aarch64 PMULL implementation for
  `bitmap_prefix_xor_64`, an aarch64 `trailing_zeros` body for
  `bitmap_next_set_bit`, rewired `bulk_emit_positions_64_neon` through
  `bitmap_next_set_bit_neon`, and consolidated the checkasm stack canary into a
  shared XOR-fold helper with an exact byte-compare backstop.
- Correctness checks passed before measurement: release checkasm for
  `checkasm_bitmap_prefix_xor_64`, `checkasm_bitmap_next_set_bit`,
  `checkasm_bulk_emit_positions_64`, `checkasm_byte_class_from_eq_set_64`, and
  `checkasm_parity`; `cargo run -p xtask --release -- primitive-checkasm`;
  `cargo test --workspace`; negative canary injection failed as expected in
  both `guarded_call` and the migrated byte-class wrapper; explicit host asm
  proof with `-C target-feature=+cssc,+aes` showed `pmull.1q` and `ctz`.
- The `simd_scan` benchmark against `skv7-w10-pre` did not falsify the
  structural scan rows: reported SIMD rows were stable or improved, with the
  largest observed SIMD midpoint drop about -0.52% on `update_center/simd`.
- The JSON parse benchmark falsified the candidate before `RESULTS.md` refresh
  could be admitted. A coherent final `bench-json --advisory` run was stopped
  after repeated hard-row regressions:
  `instruments/track1_generated` 103.38 us, -4.62% throughput;
  `instruments/track2_handcoded` 148.04 us, -4.19% throughput;
  `numbers/track1_generated` 64.465 us, -10.04% throughput;
  `unicode_escapes/track1_generated` 670.99 us, -12.66% throughput; and
  `unicode_escapes/track2_handcoded` 678.12 us, -15.52% throughput.
- Failure mode: PMULL as the default hot `bitmap_prefix_xor_64` body is not
  admissible for escape-heavy and narrow parse-only JSON rows on this host,
  even though the primitive is correct and visible in asm. The next candidate
  shape is W10b: retain the B6 canary fold and CSSC/`ctz` next-bit consumer,
  but keep prefix-XOR scalar on the production hot path unless a measured,
  narrowly gated PMULL consumer proves same-row non-regression.

## SK-V7 Wave 10b CTZ Bulk Consumer and B6 Canary Fold Redress

- Item 89 rejects the narrowed W10b CTZ bulk consumer plus B6 canary fold
  candidate. The rejected patch is archived at
  `/tmp/skv7-wave-10b-rejected.patch`.
- The candidate kept `bitmap_prefix_xor_64` on the scalar production path,
  replaced the aarch64 `bitmap_next_set_bit_neon` scalar delegate with a local
  `trailing_zeros` body, consumed that body from
  `bulk_emit_positions_64_neon`, and consolidated the checkasm stack canary
  into a shared randomized XOR-fold helper with byte-exact backstop checks.
- Correctness and integrity gates passed before measurement. Release checkasm
  for `checkasm_bitmap_next_set_bit`, `checkasm_bulk_emit_positions_64`,
  `checkasm_byte_class_from_eq_set_64`, and `checkasm_parity` passed.
  `cargo run -p xtask --release -- primitive-checkasm` passed.
  `cargo test --workspace` passed. Static audits confirmed the bulk consumer
  calls `bitmap_next_set_bit_neon`, prefix-XOR still delegates to the scalar
  body, no PMULL text exists in the prefix-XOR source, and the old fixed
  0xDE volatile canary pattern is gone.
- The negative canary control failed closed in the migrated wrappers: injected
  `canary[0] ^= 1` produced status 101 for bitmap-next-bit, bulk-emit,
  byte-class, and the classifier parity wrapper. The originally planned
  `sk_v3_scalar_anchors_compile` anchor did not enter the stack-canary wrapper,
  so the representative parity proof used `classifier_parity_alignment_sweep`.
- Explicit asm proof with
  `RUSTFLAGS='-C target-cpu=native -C target-feature=+cssc'` emitted `ctz`
  at `/tmp/skv7-w10b-asm/release/deps/*.s:1687` and `:1753`; native cfg did
  not advertise CSSC; no `pmull` was emitted.
- Dedicated `simd_scan` pre/post measurement against
  `skv7-w10b-pre` did not falsify the SIMD scan rows; the largest SIMD midpoint
  drop observed in `/tmp/skv7-w10b-simd-scan.log` was about -1.40% on
  `numbers/simd`, still reported within Criterion's noise threshold.
- The refreshed `RESULTS.md` comparison falsified the W10b admit gate because
  six Track 1/2 rows dropped more than 2% versus the saved pre-W10b report,
  despite no verdict downgrades: `canada/parse_only` Track 1 -3.11% and
  Track 2 -4.14%; `citm_catalog/parse_only` Track 1 -7.36%;
  `instruments/parse_only` Track 1 -3.96%; `marine_ik/parse_only` Track 1
  -5.68%; `mesh/parse_only` Track 1 -8.07% and Track 2 -7.46%; and
  `numbers/parse_only` Track 1 -6.44%.
- The PMULL failure mode from Item 88 was not reopened: the W10b JSON rows that
  previously falsified PMULL were neutral or improved. The remaining failure
  mode is the production-path `bitmap_next_set_bit`/bulk consumer change under
  the W10b per-row maintain invariant. The next candidate shape is W10c:
  admit only the B6 canary hardening as Stage 1, leave both bitmap asm body
  fills rejected for this tranche, and require zero `RESULTS.md` diff.

## SK-V7 Wave 10c B6 Stack-Canary Stage 1 Redress

- Item 90 admits W10c B6 stack-canary Stage 1 only. The admitted slice
  replaces the fixed 0xDE volatile-probe checkasm canary with a shared
  randomized XOR-fold helper plus byte-exact backstop diagnostics, then routes
  the existing `guarded_call`, `stack_canary_then`, byte-class, and parity
  wrappers through that helper.
- This redress does not admit the original W10 bitmap body-fill target.
  PMULL prefix-XOR remains rejected by Item 88, and the CSSC CTZ/bulk consumer
  remains rejected by Item 89. The original W10 §12 "both primitives admitted"
  exit gate is therefore not green; SK-V7 closes the wave honestly as B6
  hardening admitted with the bitmap asm bodies routed to Pass Alpha/SK-V8.
- Verification completed. Release checkasm passed for
  `checkasm_byte_class_from_eq_set_64`, `checkasm_parity`,
  `checkasm_bitmap_next_set_bit`, and `checkasm_bulk_emit_positions_64`.
  `cargo run -p xtask --release -- primitive-checkasm` passed after the
  negative canary control was removed. `cargo test --workspace` passed before
  the negative canary control was injected.
- The negative canary control failed closed in representative wrappers:
  injected `canary[0] ^= 1` produced status 101 for bitmap-next-bit,
  bulk-emit, byte-class, and parity; the logs are archived at
  `/tmp/skv7-w10c-canary-next.log`, `/tmp/skv7-w10c-canary-bulk.log`,
  `/tmp/skv7-w10c-canary-byte-class.log`, and
  `/tmp/skv7-w10c-canary-parity.log`.
- Static audits held. The old fixed 0xDE volatile canary pattern is absent from
  `skinny/crates/bbnf-simd/tests/checkasm_*.rs`. Production bitmap/runtime and
  generated JSON paths have zero diff, `skinny/RESULTS.md` has zero diff, no
  PMULL text exists in `bitmap_prefix_xor_64.rs`, prefix-XOR still delegates to
  the scalar body, and `bulk_emit_positions_64.rs` still delegates to its
  scalar body.
- No benchmark refresh was performed for W10c because the admitted slice is
  test-harness hardening only and has zero production or `RESULTS.md` diff.

## SK-V8 Wave 2 Typed Product Plane Redress

- Item 91 admits the W2 typed product-plane source slice in commit `12aff1e4`.
  The admitted source/product rows are `apache_builds/real_typed_struct` and
  `citm_catalog/real_typed_struct`; they are not present as measured rows in
  the current W0 `skinny/RESULTS.md` manifest.
- The slice extends only the existing real typed schema/generator path:
  `skinny/xtask/src/real_typed_schema.rs`,
  `skinny/crates/bbnf-bench/src/real_typed_struct.rs`, and generated output in
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`. It adds no
  directive, BIR variant, `BackendShape`, substrate surface, sidecar,
  parser-owned cursor, runtime JSON behavior, or direct digest product claim.
- `apache_builds` consumes root `mode`, root `nodeName`, and job string fields.
  `citm_catalog` consumes the events map as keyed entries with `id`, `name`,
  `subTopicIds`, and `topicIds`, keeping the product proof on string/u64/vector
  data with generated Track 1 DirectBuild plus serde_json as the
  Track 2/oracle path and a separate sonic-rs checksum parity lane.
- `canada/real_typed_struct` is rejected for W2. A full-fixture parity check
  exposed a generated DirectBuild versus serde checksum mismatch on long
  decimal coordinate payloads. W2 routes the row out rather than weakening
  typed proof to length-only or digest-only evidence.
- Verification completed for the source admission:
  `cargo xtask regen-real-typed`,
  `cargo test -p bbnf-bench real_typed -- --nocapture`,
  `cargo test -p codegen typed_direct -- --nocapture`,
  `cargo xtask check-real-typed`, `cargo xtask check-json`,
  `cargo xtask check-conformance`, frozen product-surface diff, and
  `git diff --check`.
- `skinny/RESULTS.md` is unchanged. W2 rejects benchmark row-table admission
  for this wave because the current local Criterion target was already known to
  trip the W0 run-id validator on metadata drift unrelated to W2 source. W2
  therefore admits source/product parity only and does not claim six measured
  `real_typed_struct A / GO` rows.
- W2 hardening V3 exposed and folded one checked-report mismatch: source-only
  typed fixtures must not make the W0 report gate require unadmitted Criterion
  `real_typed_struct` metadata rows. The gate now derives real typed metadata
  requirements from the W0 measured baseline rows, so Apache/CITM remain
  source/product parity rows until a later accepted benchmark row-table wave.
  The standard checked report path can still fail on the already recorded W0
  run-id drift, but no longer fails first on missing Apache/CITM metadata.

## SK-V8 Wave 3 Tape Plus Structural-Projection Redress

- Item 92 rejects/routes W3 Tier A implementation for SK-V8 before source
  redress. The scanner/tape event model is not isomorphic: the scanner retains
  structural punctuation plus real quotes, while the current retained tape is a
  generated parser event stream containing container opens/closes, opening
  quotes, number starts, and literal starts. Retained view/`ValueRef` traversal
  depends on that event stream.
- The selected W3 target rows were `twitter/parse_only` and
  `apache_builds/parse_only`; the guard rows were `canada/parse_only`,
  `mesh/parse_only`, `numbers/parse_only`, and `marine_ik/parse_only`.
  No row-table admission is made for W3.
- W3 does not reopen REDRESS 51 or 53 cursor routes, sidecar producers, parser
  owned structural cursors/facts, `tape_vs_tape` as production consumer,
  `UnionTape`, a new `BackendShape`, a new BIR variant, a new directive, a
  public substrate API, or Tier B string-boundary/quote-backslash/parity work.
- No source patch or rejected patch artifact exists for W3 because the accepted
  plan failed the pre-redress fit gate. The required owner surface spans SIMD,
  JSON scan, tape layout, generated retained parser, retained view/value,
  codegen templates, bench parity/materialization/gate code, and row reporting,
  exceeding the W3 LOC and 90-minute caps.
- Verification for the rejection/routing plan:
  `cargo test -p bbnf-bench offset_stream_tracks_verified_source_events -- --nocapture`,
  `cargo test -p bbnf-bench counts_json_lazy_tape_materialization_shape -- --nocapture`,
  `git diff --exit-code HEAD -- skinny/RESULTS.md`, and `git diff --check`.
  `skinny/RESULTS.md` remains unchanged.
- The routed SK-V9/Pass Omega precursor is to define the retained class/event
  grammar including numbers/literals and string quote ownership, prove the
  retained `ValueRef` cursor contract over that grammar, and only then reopen a
  measured structural-heavy parse row wave.

## SK-V8 Wave 4 Direct Guard Triage Redress

- Item 93 rejects/routes the W4 hand Track 2 scalar-parent fold candidate. The
  selected `N-direct` rows were `apache_builds/direct_to_struct`,
  `numbers/direct_to_struct`, and `random/direct_to_struct`, chosen because
  generated Track 1 already cleared the same-run direct floor and the opening
  miss was Track 2-only. Their floors were `ceil(sonic-rs strict Mbps / 1.10)`:
  Apache 8048 Mbps, numbers 7230 Mbps, and random 7401 Mbps.
- The attempted source patch changed only
  `skinny/crates/bbnf-bench/src/direct_struct.rs`: the independent hand
  Track 2 parser folded scalar object/array values directly into the parent
  digest instead of constructing a temporary scalar child digest. It added no
  runtime, codegen, BIR, directive, substrate, generic crate, generated Track 1,
  string materializer, cap-16, value-byte carry, source-hook, semantic
  string-fact, raw-f64, or mantissa route.
- Correctness was green:
  `cargo test -p bbnf-bench direct_struct -- --nocapture` passed after the
  patch. Directional profile probes showed Apache and numbers improving in
  release-loop form, but random still below its floor.
- Binding native Criterion falsified the three-row W4 gate. Apache improved
  and cleared sonic/1.10 (`track2_direct_to_struct` 95.347 us versus
  `sonic_rs_direct_to_struct` 92.643 us), but random remained below sonic/1.10
  (`track2` 569.57 us versus sonic 463.26 us), and numbers regressed by
  +6.3287% Track 2 time (`track2` 106.43 us versus sonic 93.211 us). This
  misses selected-row floors before any row-table admission question.
- W4 V1 hardening also found that a source admission would need a W4-aware
  checked report path and explicit Lock 14 W4 parent-diff allowance. Because
  the source candidate was already falsified on selected rows, W4 does not add
  that gate surface in this wave. This is fail-closed: no source patch is
  admitted, no Lock 14 allowance is added, and `skinny/RESULTS.md` remains
  unchanged.
- The rejected patch is saved at
  `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`. Do not reopen scalar
  parent folding under another name unless a later wave first supplies a
  W4/V9-aware checked gate, full-table maintain measurement, and an independent
  Track 2 digest-arithmetic backstop. Remaining direct digest misses route to a
  later direct-output-contract or control-path research tranche; digest evidence
  remains guard-plane only and is not product proof.

## SK-V9 Wave 1 Apache/CITM Measured Typed-Row Admission

- Item 94 admits the W1 row-table slice for
  `apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct`.
  The admitted commit expands the measured baseline whitelist only; it does not
  change parser, runtime, SIMD, codegen, fixture, direct-output, or generic
  grammar behavior.
- Fresh native Criterion evidence is rendered into `skinny/RESULTS.md` under
  run id `sk-v9-open:criterion-fnv64-a1e8a51ae806d386`, with artifacts under
  `skinny/target/skv9-w1/criterion/`. The capture used
  `RUSTFLAGS="-C target-cpu=native"` and the manifest carries
  `target_cpu=native` in `build_flags` and `feature_mask`.
- Apache admits at 8174 Mbps Track 1 versus 8110 Mbps sonic-rs typed strict
  (`ceil(8110 / 1.10) = 7373` floor). Track 2/oracle evidence is independent
  at 6728 Mbps, and `assert_real_typed_parity` remains the product proof.
- CITM admits at 35102 Mbps Track 1 versus 22058 Mbps sonic-rs typed strict
  (`ceil(22058 / 1.10) = 20053` floor). Track 2/oracle evidence is independent
  at 19143 Mbps, and `assert_real_typed_parity` remains the product proof.
- The four pre-existing measured typed rows maintain `A / GO` above their
  sonic/1.10 floors: twitter 18302, update_center 11847, mesh 10032, and
  marine_ik 10728 Mbps. The direct guard envelope also holds:
  `apache_builds/direct_to_struct` stays `N-direct / NO-GO`, and
  `citm_catalog/direct_to_struct` stays `A / GO`.
- REDRESS 91's deferred row-table gap is now closed for Apache and CITM only.
  `canada/real_typed_struct` stays rejected pending the full-fixture
  DirectBuild-vs-serde checksum proof; no direct digest row is relabeled as
  typed product proof.
- Verification completed for the admission:
  `RUSTFLAGS="-C target-cpu=native" CRITERION_HOME=target/skv9-w1/criterion cargo xtask gate-json --advisory --update-results`
  rendered the promoted report; the W1 close checks were
  `cargo test -p bbnf-bench lock14_baseline -- --nocapture`,
  `cargo test -p bbnf-bench real_typed -- --nocapture`,
  `cargo xtask check-real-typed`, `cargo xtask check-json`,
  `cargo xtask check-conformance`,
  `RUSTFLAGS="-C target-cpu=native" CRITERION_HOME=target/skv9-w1/criterion cargo xtask gate-json --advisory --check-results`,
  and `git diff --check`.

## SK-V9 Wave 2 Retained Class/Event Grammar Proof

- Item 95 admits the W2 proof-only precursor routed by REDRESS 92. The wave
  adds `EventGrammar`, the empty `AnyGrammar` default, JSON and Sheets
  witness-local event grammars, and a fourth zero-sized `ValueRef`
  event-grammar marker while preserving the existing `K = AnyKind` node-kind
  marker used by generated retained JSON views.
- W2 recorded two CHALLENGE passes. The first rejected the direct `K -> G`
  route because current generated views instantiate
  `ValueRef<'doc, 'input, RootKind/ObjectKind/...>`. The revised plan split
  the axes as `ValueRef<'doc, 'input, K, G>`, and the second CHALLENGE accepted
  that route for redress.
- The proof is structural only. It changes no parser/scanner control path, no
  generated JSON runtime file, no codegen template, no fixture, no benchmark
  crate, and no `skinny/RESULTS.md` row. W3 is now unblocked to reopen the
  union class-column substrate under its own measured gate.
- Verification completed for the admission:
  `cargo check -p runtime --features proof`,
  `cargo test -p runtime event_grammar --features proof -- --nocapture`,
  `cargo build -p runtime`,
  `git diff --exit-code HEAD -- skinny/RESULTS.md`,
  `rg -n 'admits_(fact|class)|STRUCTURAL_CLASS_COUNT|FactId' skinny/crates/runtime/src`,
  `rg 'event_grammar|event_grammar_witness' skinny/crates/bbnf-bench/`, and
  `git diff --check`. The negative proof fixture constructs
  `ValueRef<'static, 'static, AnyKind, JsonEventGrammar>` from a local tape and
  is rejected by the borrow checker.

## SK-V9 Wave 3 Union Event-Model Class-Column Redress

- Item 96 rejects the W3 class-column + move-consumed structural-index
  implementation. The attempted source patch added a co-indexed event-class
  byte column to `runtime::tape`, rewired generated JSON parsing to consume
  `scan_structurals` positions, changed `JsonNodeKind::at_cursor` to read the
  class column instead of rediscovering source bytes, updated codegen
  templates, added a runtime scan parity harness, and taught Track 2/parity to
  write and compare class bytes.
- Correctness checks were green before measurement:
  `cargo test --manifest-path skinny/Cargo.toml -p runtime --test checkasm_scan_structurals -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p runtime -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench parity -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench materialization -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench track2 -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p codegen`, and
  `cargo check --manifest-path skinny/Cargo.toml -p runtime --features proof`.
  The W3 deletion invariant also held in the attempted patch:
  `rg -n "consume_structural" skinny/crates/runtime/src skinny/crates/codegen/src`
  returned no matches.
- Binding native Criterion was run with
  `RUSTFLAGS="-C target-cpu=native" CRITERION_HOME=/tmp/skv9-w3-criterion cargo xtask bench-json --advisory`.
  The full benchmark capture completed, but `gate-json --update-results
  --advisory` correctly refused to render while Lock 14 frozen roots were dirty
  with the uncommitted W3 source patch. `skinny/RESULTS.md` therefore remains
  unchanged; the measured evidence below is extracted from the completed
  `/tmp/skv9-w3-criterion` Criterion slopes using the same
  `bytes * 8000 / ns` formula as `gate-json`.
- The implementation falsified every W3 must-improve row and every binding
  W10b maintain row:

  | Row | Floor | Track 1 Mbps | Track 2 Mbps | Sonic strict Mbps | Status |
  |---|---:|---:|---:|---:|---|
  | twitter | 17685 | 9284 | 12081 | 20772 | FAIL |
  | apache_builds | 14124 | 7700 | 12254 | 16870 | FAIL |
  | update_center | 14370 | 6854 | 9199 | 19513 | FAIL |
  | distinct_values | 15731 | 6229 | 6174 | 17931 | FAIL |
  | canada | 15866 | 11221 | 15978 | 13402 | FAIL |
  | citm_catalog | 28630 | 13611 | 20624 | 25428 | FAIL |
  | instruments | 15865 | 9539 | 11932 | 19773 | FAIL |
  | marine_ik | 11831 | 8012 | 11778 | 9702 | FAIL |
  | mesh | 12186 | 10087 | 12510 | 11797 | FAIL |
  | numbers | 17596 | 13407 | 18681 | 13585 | FAIL |

- The standalone SIMD scan benches stayed fast, so the failure is not a scalar
  reference or scan parity failure. The falsifier is the integration shape:
  allocating and move-consuming a full structural-position vector inside
  `parse` adds enough parse-loop cost to miss the W3 rows and trip the W10b
  no-regression block.
- The rejected patch is saved at `/tmp/skv9-waveW3-rejected.patch` (1274
  patch lines). The source tree was restored after saving the artifact. W3 is
  not admitted, the W3 dependency remains open, and all W4 sub-waves remain
  blocked until a revised W3 plan lands a substrate without the measured
  parse-loop regression.

## SK-V9 Wave 3 Union Event-Model Streaming-Cursor Redress

- Item 97 rejects the W3 V2 streaming-cursor implementation. The revised
  source patch was materially different from REDRESS 96: it removed the
  full structural-position vector from `parse`, added an allocation-free
  `JsonStructuralCursor` over the aarch64 block scanner, kept the class lane
  co-indexed inside `runtime::tape`, changed `JsonNodeKind::at_cursor` to read
  the class lane, updated codegen templates, added a runtime scan/cursor parity
  harness, and taught Track 2/parity to write and compare class bytes.
- Correctness checks were green before measurement:
  `cargo test --manifest-path skinny/Cargo.toml -p runtime --test checkasm_scan_structurals -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p runtime -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench parity -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench materialization -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench track2 -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p codegen`, and
  `cargo check --manifest-path skinny/Cargo.toml -p runtime --features proof`.
  The V2 deletion invariants also held in the attempted patch:
  `rg -n "consume_structural" skinny/crates/runtime/src skinny/crates/codegen/src`,
  `rg -n "into_positions\\(|structural_positions" skinny/crates/runtime/src/grammars/json`,
  and the value source-rediscovery grep returned no matches.
- A full native
  `RUSTFLAGS="-C target-cpu=native" CRITERION_HOME=/tmp/skv9-w3-v2-criterion cargo xtask bench-json --advisory`
  run was started and stopped after the required gate rows had already
  falsified and the run had moved into comparator/probe work. Binding gate
  evidence was then captured with the targeted native Criterion command
  `RUSTFLAGS="-C target-cpu=native" CRITERION_HOME=/tmp/skv9-w3-v2-target cargo bench -p bbnf-bench --bench json_parity -- 'json/(twitter|apache_builds|update_center|distinct_values|canada|citm_catalog|instruments|marine_ik|mesh|numbers)/track1_generated'`.
  The filtered Criterion run emitted comparison-sample warnings for sibling
  benches that were intentionally not selected; the `track1_generated`
  estimates were written under `/tmp/skv9-w3-v2-target` and extracted with the
  same `bytes * 8000 / ns` formula as `gate-json`.
- The V2 implementation again falsified every W3 must-improve row and every
  binding W10b maintain row:

  | Row | Floor | Track 1 Mbps | Status |
  |---|---:|---:|---|
  | twitter | 17685 | 7520 | FAIL |
  | apache_builds | 14124 | 6710 | FAIL |
  | update_center | 14370 | 5534 | FAIL |
  | distinct_values | 15731 | 5338 | FAIL |
  | canada | 15866 | 8293 | FAIL |
  | citm_catalog | 28630 | 9997 | FAIL |
  | instruments | 15865 | 7305 | FAIL |
  | marine_ik | 11831 | 5540 | FAIL |
  | mesh | 12186 | 6835 | FAIL |
  | numbers | 17596 | 9542 | FAIL |

- The streaming cursor cleared the REDRESS 96 allocation critique but did not
  clear the measured gate. The falsifier is still the integrated parse-loop
  shape: scanning and cursor validation inside retained parsing costs more
  than the removed structural rediscovery on the W3 rows and trips the full
  W10b maintain block.
- The rejected patch is saved at `/tmp/skv9-waveW3-v2-rejected.patch` (1572
  patch lines, including the untracked checkasm test). The source tree was
  restored after saving the artifact. W3 remains rejected/open, the W3
  dependency remains unsatisfied, and all W4 sub-waves remain blocked until a
  materially different W3 substrate can pass the measured gate.

## SK-V9 Wave 3 Union Event-Model Gate Retirement

- Item 98 retires `G-W3-UNION-SUBSTRATE` for SK-V9. REDRESS 96 and REDRESS 97
  are not local implementation misses; together they falsify the union
  substrate thesis on this host. The thesis was that the parser discards a
  SIMD structural index, re-discovers structural bytes scalar, and would gain
  throughput if the index became the retained union substrate. Two faithful
  implementations made that claim measurable and both regressed uniformly.
- REDRESS 96 landed the full class-column substrate plus move-consumed
  `scan_structurals` vector. It was correctness-green and parity-green before
  measurement, but missed every W3 must-improve row and every W10b maintain
  floor. REDRESS 97 removed the full vector and used an allocation-free
  streaming cursor over the aarch64 scanner. It was also correctness-green and
  parity-green before measurement, and again missed every W3 must-improve row
  and every W10b maintain floor.
- The remaining W3 V3 route, emit-site class-lane-only, was rejected by
  CHALLENGE before source redress. It can prove a source-free retained
  `JsonNodeKind::at_cursor`, but it leaves scalar delimiter discovery in place,
  carries no same-wave structural producer, and cannot satisfy the current
  parse-only producer gate. Dispatching it as W3 would be a paper-close.
- The empirical finding is load-bearing: on the M5 Max wide-issue core, the
  scalar `consume_structural`/delimiter path that profile attribution flagged
  as structural rediscovery is cheaper than materializing or streaming a SIMD
  structural cursor through retained parsing. The SIMD scan looked discarded
  because consuming it adds memory traffic and cursor indirection that the
  current branch-predictable, cache-hot scalar loop does not pay.
- `G-W3-UNION-SUBSTRATE` is therefore retired, not merely blocked. No SK-V9
  wave may force, amend, or split W3 to preserve the same union-substrate
  hypothesis without first entering a new Alpha/S-P3 contract. W4's prior
  cascade-lock dependency on a closed W3 union substrate is abrogated for
  SK-V9; remaining W4 candidates may dispatch only if re-scoped to existing
  offset-tape/string/unescape call sites with their own scalar reference,
  checkasm, same-wave consumer, and W10b maintain gates.
- The product-plane result that survives SK-V9 is W1: measured
  `real_typed_struct` row admission. Apache/CITM were admitted with fresh
  run-id/metadata evidence, and the typed plane is the SOTA-bearing surface.
  Future SK-V9/SK-V10 work should prioritize typed-row generalization
  (`github_events`, `gsoc-2018`, `instruments`) and existing-substrate
  unicode/string kernels, not parse-only union substrate repair.
- Pass Alpha is now the correct next synthesis step. It must fold this
  falsification into the SK-V10 contract, retire W3 from the candidate
  shortlist, carry REDRESS 96/97/98 as pre-blocks, and resequence around
  typed-plane and existing-substrate work.

## SK-V10 Wave 0 Telemetry Freeze

- Item 99 admits W0 as a gate-only telemetry freeze under
  `G-W0-TELEMETRY-FREEZE`. The opening authority remains the W1-rendered
  SK-V9 snapshot in `skinny/RESULTS.md` with run id
  `sk-v9-open:criterion-fnv64-a1e8a51ae806d386`; W0 does not mint a new
  criterion identity.
- Redress changed only the cost-facts RESULTS snapshot guard in
  `skinny/xtask/src/main.rs`: the manifest row-count invariant is now the
  SK-V10 opening surface of 40 rows instead of the stale pre-W1 38-row
  surface. The gate still requires the existing snapshot markers, malformed
  run-id rejection, uniform run id, diagnostic nonproducer marker, cost-facts
  validation, and the inherited schema/report consumers.
- W0 moved no rows. `RESULTS.md` remains 17 `parse_only S / NO-GO` rows, 17
  `direct_to_struct` rows with 3 `A / GO` and 14 `N-direct / NO-GO`, and 6
  `real_typed_struct A / GO` rows.
- Evidence passed against the frozen capture:
  `CRITERION_HOME=target/skv9-w1/criterion RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory`
  and
  `CRITERION_HOME=target/skv9-w1/criterion RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --with-cost-facts --check-results`.
  The rendered artefacts are saved at `/tmp/skv10-w0-gate-json-advisory.md`
  and `/tmp/skv10-w0-cost-facts.json`.
- The default `skinny/target/criterion` cache was rejected before redress for
  non-native W0 metadata and is not the SK-V10 opening authority. Future W0/W1
  validation must name the intended `CRITERION_HOME` explicitly.

## SK-V10 Wave 1 Direct Output Contract

- Item 100 admits W1 under `G-W1-DIRECT-CONTRACT`. The wave is contract-only:
  it moves no `RESULTS.md` rows and changes no parser/runtime behavior.
- Redress added a direct row movement predicate to
  `skinny/crates/bbnf-bench/src/report.rs`. Unchanged W0 rows still validate
  through the inherited W0 baseline checks. A baseline `N-direct / NO-GO`
  direct row may move only as `A / GO` and only with digest output plane,
  strict row semantics, measured-row validation, `independent_verified` Track 2
  status, non-gate-only consumer, REDRESS provenance, non-SK-V9-open wave id,
  and same-run native direct comparator evidence sourced from the
  `sonic_rs_direct_to_struct` and `serde_json_direct_to_struct` Criterion
  benches.
- Negative tests cover output-plane mismatch, deferred/view-boundary movement,
  gate-only consumer, missing REDRESS, missing Track 2 independence, stale wave
  id, direct comparator plane mismatch, and wrong comparator source.
- Evidence passed:
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench direct_contract -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench report::tests -- --nocapture`,
  `CRITERION_HOME=target/skv9-w1/criterion RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory`,
  and
  `CRITERION_HOME=target/skv9-w1/criterion RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --with-cost-facts --check-results`.
- W2 may now re-evaluate direct rows, but it must still satisfy the Section 0.2
  direct floors for both generated Track 1 and independent Track 2/oracle.

## SK-V10 Wave 2 Direct Row-Table Reclamation

- Item 101 admits W2 under `G-W2-DIRECT-RECLAMATION`. The wave moves only
  direct digest rows and changes no parser/runtime behavior.
- `apache_builds/direct_to_struct` moved from `N-direct / NO-GO` to
  `A / GO`: Track 1 11157 Mbps and independent Track 2 10145 Mbps both clear
  the Section 0.2 floor of 10020 Mbps under the same-run sonic-rs direct
  comparator at 11021 Mbps.
- `numbers/direct_to_struct` moved from `N-direct / NO-GO` to `A / GO`:
  Track 1 12182 Mbps and independent Track 2 11803 Mbps both clear the Section
  0.2 floor of 11788 Mbps under the same-run sonic-rs direct comparator at
  12966 Mbps.
- The renderer emits the W1 direct contract fields for both rows:
  `strictness=strict`, `parse_utf8=measured-row`,
  `measured_validation_path=measured-row`,
  `same_wave_consumer_class=gate_json_direct_contract`,
  `redress_entry=REDRESS-101`, and `wave_id=SK-V10-W2`.
- `report.rs` now rejects any changed baseline `N-direct / NO-GO` direct row
  whose Track 1 or Track 2 Mbps is below its Section 0.2 direct floor. The W2
  numeric gate is therefore consumed by `gate-json` in the same wave.
- Routed remainder remains `N-direct / NO-GO`: `twitter`, `canada`,
  `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`,
  `instruments`, `unicode_mixed`, `unicode_escapes`, `distinct_values`, and
  `y_string_unicode`.
- Direct guard floors hold: `citm_catalog` 21129 >= 18145, `marine_ik` 9205 >=
  7575, `unicode_basic` 8973 >= 7841. Typed guard floors hold: `twitter` 18302
  >= 14424, `citm_catalog` 35102 >= 20053, `apache_builds` 8174 >= 7373,
  `update_center` 11847 >= 11365, `mesh` 10032 >= 8428, `marine_ik` 10728 >=
  7369.
- Evidence passed:
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w2_direct -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench direct_contract -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench report::tests -- --nocapture`,
  `CRITERION_HOME=target/skv9-w1/criterion RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`,
  and
  `CRITERION_HOME=target/skv9-w1/criterion RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --with-cost-facts --check-results`.

## SK-V10 Wave 3 Parse-Only Firewall

- Item 102 admits W3 under `G-W3-PARSE-FIREWALL`. W3 is proof-only and closes
  no behavior source, generated output, benchmark body, or row movement.
- The active packet audit found no live dispatch route through W3
  union/event substrate, retained class column, `UnionTape`, structural or
  streaming cursor, class-lane-only route, parser-owned structural projection,
  or W4-through-W3 cascade-lock. Hits in SPEC, DISPATCH, HANDOFF, and
  SYNTHESIS are refusal, pre-block, or diagnostic references only.
- The result table audit found 17 `parse_only` rows and no parse row outside
  `S / NO-GO`.
- The existing report validator rejects parse-only SOTA movement. Evidence:
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w0_report_accepts_exact_opening_baseline -- --nocapture`
  passed; that test mutates a parse row to `A / GO` and expects validation
  failure.
- Frozen gate evidence passed:
  `CRITERION_HOME=target/skv9-w1/criterion RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --with-cost-facts --check-results`.
- W4 may now dispatch as typed-product work. W4 cannot name W3 as a consumer or
  substrate dependency.

## SK-V10 Wave 4 `instruments` Typed Product Admission

- Item 103 rejects W4 under `G-W4-INSTRUMENTS-TYPED`. The implemented source
  slice added `instruments/real_typed_struct` as a generated typed product row
  with independent Track 2/oracle, serde_json typed, sonic-rs typed, checksum,
  `gate-json`, report validation, and exact Lock 14 owner-path authorization.
- The row was falsified by measurement: generated Track 1 measured 20678 Mbps,
  independent Track 2 measured 12127 Mbps, sonic-rs typed strict measured
  15940 Mbps, and serde_json typed measured 12119 Mbps. The W4 floor was
  `ceil(15940 / 1.10) = 14491` Mbps. Track 1 passed; Track 2 missed
  `12127 < 14491`.
- The gate failure was consumed by `gate-json`:
  `json/instruments/real_typed_struct/main W4 typed contract admits only
  A / GO, saw N-direct / NO-GO`.
- The rejected patch is saved at `/tmp/skv10-waveW4-rejected.patch`.
  `RESULTS.md` is unchanged: no typed row moved, `real_typed_struct` remains
  6 `A / GO`, `direct_to_struct` remains 5 `A / GO` and 12
  `N-direct / NO-GO`, and `parse_only` remains 17 `S / NO-GO`.
- The routed finding is that fixed object-root typed admission is not enough
  for `instruments`; the independent oracle cost dominates the W4 contract.
  W5 may still dispatch because its entry gate accepts a W4 admission or a W4
  measured rejection.

## SK-V10 Wave 5 Root-Type Typed Generalization Proof

- Item 104 admits W5 under `G-W5-ROOT-TYPED-PROOF`. `DirectRootSchema` now
  carries `DirectTypeRef`, preserving existing struct roots through
  `DirectRootSchema::struct_root` and adding collection roots through
  `DirectRootSchema::typed_root`.
- The typed renderer now collects helpers from root-level `DirectTypeRef`
  values before rendering public root functions. Root functions parse through
  the same `Renderer::parse_expr` path as field values, so array and map-entry
  roots use the existing typed DirectBuild helper machinery.
- Generated proof roots were added without registering any bench row:
  `parse_w5_array_root_probe` returns
  `Vec<crate::real_typed_struct::W5ArrayEvent<'i>>`, and
  `parse_w5_map_entry_root_probe` returns
  `Vec<crate::real_typed_struct::W5MapMetricEntry<'i>>`.
- The proof roots pass checksum parity against serde_json and sonic-rs typed
  sidecars. `RESULTS.md` is unchanged; no row moved and no typed product
  admission is claimed by W5.
- W6 may now select one root-unblocked typed row, with `github_events` before
  `gsoc-2018` unless CHALLENGE reverses the order. W6 still must provide
  full-fixture generated Track 1, independent Track 2/oracle, serde_json typed,
  sonic-rs typed, checksum parity, and typed floor evidence.

## SK-V10 Wave 6 Root Typed Row Admission

- Item 105 admits W6 under `G-W6-ROOT-TYPED-ROW`. The wave moves exactly one
  typed product row, `github_events/real_typed_struct`, and does not move any
  parse-only or direct digest row.
- W6 consumes the W5 root model by registering `parse_github_events` as a
  `Vec<crate::real_typed_struct::GithubEvent<'i>>` typed root. The generated
  parser, independent Track 2/oracle, serde_json typed sidecar, and sonic-rs
  typed sidecar share full-fixture checksum parity.
- The measured row clears the W6 floor: Track 1 12827 Mbps, independent Track
  2/oracle 12645 Mbps, sonic-rs typed strict 12695 Mbps, and serde_json typed
  12592 Mbps. The W6 floor is `ceil(12695 / 1.10) = 11541`, so both Track 1
  and Track 2 pass.
- `gate-json` consumes the row as strict measured-row evidence with
  `wave_id=SK-V10-W6`, `redress_entry=REDRESS-105`,
  `same_wave_consumer_class=gate_json_typed_contract`, and
  `sk_v9_open_delta=typed-row-added`.
- The cost-facts RESULTS snapshot consumer now accepts either the inherited
  40-row opening surface or that surface plus the single W6 github_events typed
  row. It still validates the uniform SK-V9 run-id grammar and W0 diagnostic
  nonproducer markers.
- Existing typed maintain rows remained above their Section 0.2 floors in the
  same rendered report: `twitter` 18777, `citm_catalog` 36655,
  `apache_builds` 8532, `update_center` 12113, `mesh` 9827, and `marine_ik`
  12262 Mbps.
- Evidence passed:
  `cargo xtask regen-real-typed`,
  `cargo xtask check-real-typed`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w6_ -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench generated_github_events_typed_parser_matches_sidecars -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml -p xtask w6_costfacts_snapshot_accepts_single_github_events_typed_row -- --nocapture`,
  a full native `json_parity` Criterion run under
  `skinny/target/skv10-w6/criterion`, a full native `simd_scan` run under the
  same root, and
  `CRITERION_HOME=skinny/target/skv10-w6/criterion RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --check-results`,
  plus
  `CRITERION_HOME=skinny/target/skv10-w6/criterion RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --with-cost-facts --check-results`.
- The non-advisory `gate-json --update-results` command wrote the W6
  `RESULTS.md` snapshot and then exited 5 because the global report remains
  `N-direct / NoGo`. That is not a W6 row failure; the advisory check passed
  and the row-level typed gate is consumed.
- W7 is now the next dispatchable wave. W7 remains proof-only unless
  CHALLENGE accepts exactly one string primitive micro-proof plan.

## SK-V10 Wave 7 Full String Primitive Micro-Proof

- Item 106 rejects W7 under `G-W7-STRING-MICROPROOF`. The selected
  `C5-full-string-proof` route tested the existing
  `match_string_at_quote_trusted_utf8` caller against a scalar-only mirror on
  `unicode_mixed`, `unicode_escapes`, and `unicode_basic`.
- Scalar/reference parity passed, and strict checkasm parity passed:
  `string_special_block_matches_scalar_reference` and
  `BBNF_SIMD_STRICT=1 sk_v3_intrinsic_parity_aarch64` were both green.
- The caller microbench falsified the proof threshold. Aggregate speedup was
  `0.774x` versus the required `1.08x`. Per-slice results were:
  `unicode_mixed` `0.471x`, `unicode_escapes` `1.315x`, and `unicode_basic`
  `0.604x`.
- The failure is caller-level, not primitive correctness. The NEON string
  special block helps `unicode_escapes` in isolation but regresses the mixed
  and basic representative slices enough that the aggregate proof fails.
- The rejected proof patch is saved at `/tmp/skv10-waveW7-rejected.patch`.
  The proof source and microbench manifest were reverted; no production caller,
  generated parser, SIMD primitive body, or `RESULTS.md` row remains changed.
- W9 cannot consume W7. W8 may still dispatch only with an escape/segment
  primitive whose entry gate does not depend on an accepted W7 string proof.

## SK-V10 Wave 8 Hex Escape Micro-Proof

- Item 107 admits W8 under `G-W8-ESCAPE-SEGMENT-MICROPROOF`. The selected
  `C6-hex-escape-proof` route proves the existing `unescape_string` caller
  through `unescape_four_unicode_escapes` and the current
  `unescape_uxxxx_x4_neon` primitive.
- The proof artifact is
  `restart/skinny/tranches/sk-v10/research/p3/escape-segment-proof/W8-ESCAPE-MICROPROOF.md`.
  It records run id `sk-v10-w8-escape-microproof`, host
  `aarch64-apple-darwin`, flags `-C opt-level=3 -C target-cpu=native`,
  feature gate `target_arch=aarch64`, sample count 25, scalar oracle
  `unescape_uxxxx_scalar + scalar JSON surrogate policy`, and threshold
  `>=1.08x`.
- The caller microbench cleared threshold: aggregate `1.268x`. Per-slice:
  `unicode_escapes` `2.636x`, `y_string_unicode` `0.943x`, and
  `unicode_mixed` zero eligible because its `\u` text is escaped-backslash
  data, not valid JSON Unicode escape syntax.
- Evidence passed:
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd unescape_uxxxx_x4_matches_scalar -- --nocapture`,
  `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd sk_v3_intrinsic_parity_aarch64 -- --nocapture`,
  and
  `RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p parse-that-regex unescape -- --nocapture`.
- W8 moves no `RESULTS.md` row and wires no new production behavior. W9 may
  consume W8 only for `unescape_uxxxx_x4_neon` in the current
  `unescape_string` caller with production row gates measured in W9.

## SK-V10 Wave 9 Existing Escape Production Rejection

- Item 108 rejects W9 under `G-W9-KERNEL-PRODUCTION`. The accepted W8 C6 proof
  remains valid, but its exact `unescape_string` caller already consumed
  `unescape_uxxxx_x4_neon` before W9. Redress did not add a cosmetic wrapper,
  constant, or feature re-gate to claim same-commit integration.
- No production source, generated parser, gate, report, or `RESULTS.md` row was
  changed. `/tmp/skv10-waveW9-rejected.patch` is an empty marker because no
  source patch was attempted.
- Evidence passed:
  `RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd unescape_uxxxx_x4_matches_scalar -- --nocapture`,
  `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd sk_v3_intrinsic_parity_aarch64 -- --nocapture`,
  and
  `RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p parse-that-regex unescape -- --nocapture`.
- Targeted direct Criterion evidence under `/tmp/skv10-w9-criterion` failed the
  Section 0.2 direct floors:

  | Corpus | Track 1 Mbps | Track 2 Mbps | sonic direct Mbps | serde direct Mbps | Floor | Outcome |
  |---|---:|---:|---:|---:|---:|---|
  | `unicode_escapes` | 5207 | 5234 | 14315 | 5195 | 12527 | FAIL |
  | `y_string_unicode` | 5096 | 3723 | 8851 | 7555 | 8027 | FAIL |

- W8 stays proof-only. W9 moves no rows and does not authorize future
  production reuse of the accepted proof without a new SPEC/CHALLENGE route
  naming a real source delta.

## SK-V10 Wave 10 Instruments Direct Residual Admission

- Item 109 admits W10 under `G-W10-DIRECT-RESIDUAL`. The wave moves exactly
  one direct digest row, `instruments/direct_to_struct`; no parser runtime,
  generated direct caller, SIMD primitive, generic crate, typed product row, or
  W3-adjacent substrate path changed.
- W10 extends the W2 direct reclamation gate with a W10-limited residual
  predicate for `instruments` at the Section 0.2 floor of 11086 Mbps. It may
  admit only a baseline `NO-GO` direct row classified as absent or `N-direct`
  when Track 1 and independent Track 2 both clear the fixed floor. Hard
  correctness failures still block admission.
- The full coherent native Criterion capture under
  `/tmp/skv10-w10-full-criterion` rendered run id
  `sk-v9-open:criterion-fnv64-6f007527061ee26d`. `instruments/direct_to_struct`
  measured Track 1 12040 Mbps, Track 2 11166 Mbps, sonic-rs direct 12674 Mbps,
  and serde_json direct 9497 Mbps; both tracks clear the 11086 Mbps W10 floor.
- Direct guards held in the same rendered report: `citm_catalog` 21595 / 20592
  Mbps versus floor 18145, `marine_ik` 9066 / 9025 Mbps versus floor 7575, and
  `unicode_basic` 9030 / 8360 Mbps versus floor 7841.
- Evidence passed:
  `RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench w10_direct -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench direct_contract -- --nocapture`,
  full native
  `CARGO_TARGET_DIR=/tmp/skv10-w10-target CRITERION_HOME=/tmp/skv10-w10-full-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory`,
  and
  `CRITERION_HOME=/tmp/skv10-w10-full-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`.
- `gate-json` consumes the moved row as strict measured-row evidence with
  `wave_id=SK-V10-W10`, `redress_entry=REDRESS-109`,
  `same_wave_consumer_class=gate_json_direct_contract`, and
  `sk_v9_open_delta=direct-residual`.
- Overall remains `N-direct / NoGo`; eleven direct residual rows remain
  `N-direct / NO-GO`. Close is now dispatchable.

## SK-V10 Close Accounting

- Item 110 closes SK-V10 under `G-CLOSE-SK-V10`. The close wave is
  documentation-only and changes no behavior source, generated parser output,
  SIMD primitive, benchmark body, telemetry schema, or `RESULTS.md` row
  disposition.
- Final wave dispositions are complete: W0 REDRESS 99 closed, W1 REDRESS 100
  closed, W2 REDRESS 101 admitted, W3 REDRESS 102 firewall-closed, W4 REDRESS
  103 rejected, W5 REDRESS 104 proof-closed, W6 REDRESS 105 admitted, W7
  REDRESS 106 rejected, W8 REDRESS 107 proof-closed, W9 REDRESS 108 rejected,
  W10 REDRESS 109 admitted, and Close REDRESS 110 closed.
- Final result authority is the W10 full native Criterion render under
  `/tmp/skv10-w10-full-criterion`, run id
  `sk-v9-open:criterion-fnv64-6f007527061ee26d`: 17 `parse_only` rows remain
  `S / NO-GO`, `direct_to_struct` is 6 `A / GO` and 11 `N-direct / NO-GO`,
  and `real_typed_struct` is 7 `A / GO`.
- Overall remains `N-direct / NoGo`. That is the measured SK-V10 close state,
  not an open implementation wave.
- Evidence passed:
  `CRITERION_HOME=/tmp/skv10-w10-full-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`
  and `git diff --check`.
- Routed remainder: REDRESS 98 goes to Pass Omega as a substrate-ceiling lock
  amendment route, and CSS L4 / Sheets / BBNF-self grammar generalization goes
  to the totality track.

## SK-V11 Wave 1a Non-JSON Gate/Report Schema Lane

- Item 111 admits W1a under `G-W1a-NONJSON-GATE`. The wave adds a companion
  non-JSON evidence report lane consumed by `bbnf-bench --bin gate` via
  `--w1a-non-json-report`; it does not relax the JSON schema-v3/W0 validator,
  update `skinny/RESULTS.md`, create generated non-JSON baseline authority, or
  move any parser row.
- The accepted report schema is `sk-v11-w1a-nonjson-v1`. It requires exact
  grammar/domain/row identity for `css_l4`, `sheets`, or `bbnf_self`, strict
  non-admitting `S / NO-GO` semantics, finite Track 1 and Track 2 placeholder
  Mbps, structured run/build/host/feature context, and an
  `internal_oracle` source sentinel owned by W1a.
- CH5's hidden-coupling revise is closed in the implementation: source
  provenance is validated separately from `track2_independence_status`, and
  `nonjson-track2-shared-source.json` fails even when it self-attests
  `independent_verified`.
- Evidence passed:
  `cargo test -p bbnf-bench report::tests::w1a -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w1a -- --nocapture`,
  `cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json`,
  the four required negative fixture checks for producer-only telemetry,
  coupled Track 2, shared source, and admission claim, and
  `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`.
- Guard evidence passed: `git diff --exit-code -- skinny/RESULTS.md`,
  `git diff --exit-code -- skinny/crates/codegen skinny/crates/runtime skinny/crates/bbnf-simd`,
  and `git diff --check`.
- Downstream: W1b may now dispatch the first generated non-JSON baseline and
  oracle lane against the W1a gate/report contract.

## SK-V11 Wave 1b Generated Non-JSON Baseline Rejection

- Item 112 rejects W1b under `G-W1b-NONJSON-BASELINE`. The selected target was
  `css_l4/declaration_values/direct/main` on
  `css_l4_declaration_value_fact_bytes`; no baseline report was admitted.
- The blocker is structural in the accepted W1b owner surface: skinny codegen
  still routes both direct and typed emission through
  `json_provider::ensure_runtime_profile`, which accepts only
  `backend.grammar_name == "json"`, and
  `skinny/crates/runtime/src/grammars/` contains generated JSON plus
  `sheets_witness`, not generated CSS L4 under `css_l4` or
  `css_l4_declaration_values`.
- Because generated CSS L4 Track 1 is absent, the independent oracle path is not
  admitted. W2 remains blocked from creating the first measurable non-JSON row.
- No source patch was attempted. `/tmp/skv11-waveW1b-rejected.patch` is an empty
  marker; no behavior source, generated runtime, benchmark body, gate/report
  schema, or `skinny/RESULTS.md` row moved.
- Evidence passed:
  `cargo test -p codegen --lib -- --nocapture`,
  `cargo test -p bbnf-bench report::tests::w1a -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w1a -- --nocapture`,
  `cargo run -p bbnf-bench --bin gate -- --w1a-non-json-report ../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json`,
  `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results`,
  `git diff --exit-code -- skinny/RESULTS.md`,
  `find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort`,
  `rg -n "ensure_runtime_profile|runtime emission currently supports grammar profile|emit_from_source|emit_typed_from_source|json_provider::ensure_runtime_profile" skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs`,
  and
  `test ! -e skinny/crates/runtime/src/grammars/css_l4 && test ! -e skinny/crates/runtime/src/grammars/css_l4_declaration_values`.

## SK-V11 Wave 2 Generated CSS L4 Intervention Entry Block

- Item 113 records W2 as `BLOCKED` before implementation dispatch. W2's SPEC
  entry gate requires W1b to close and requires W2 to consume the W1b generated
  non-JSON baseline; REDRESS 112 rejected W1b and admitted no
  `W1b_css_baseline_mbps`.
- W2 may not create the first measurable non-JSON row. Without the W1b
  baseline, the W2 exit threshold `ceil(W1b_css_baseline_mbps * 1.01)` is
  undefined and `G-W2-CSS-GENERATED-INTERVENTION` is not measurable.
- No source patch, generated parser, SIMD kernel, benchmark row, gate schema,
  or `skinny/RESULTS.md` row moved. The entry record is archived at
  `restart/skinny/tranches/sk-v11/research/w2/entry/w2-entry-blocked.md`.
- Downstream: W3-W8 may continue only as direct-plane closure/fixpoint waves
  with W2's non-JSON axis explicitly blocked. W9 close must either carry this
  BLOCKED route or be superseded by a later Alpha/Pass-Omega contract that
  creates a generated non-JSON baseline wave with explicit owner authority.

## SK-V11 Wave 3 Numeric Direct Closure Rejection

- Item 114 rejects W3 under `G-W3-NUMERIC-SEQUENCE-DIRECT`. The accepted plan
  attempted the scalar `number_span_emit_slot` route: factor generated JSON
  number emission through a const-generic slot helper, add generated direct
  number semantic coverage, and consume W3 provenance in `gate-json`/`report`.
- The rejected patch touched only the W3 owner surface and is saved at
  `/tmp/skv11-waveW3-rejected.patch` (1874 patch lines). The source tree was
  restored after saving the artifact; no W3 source change remains dirty.
- Pre-measurement evidence passed:
  `RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate w3 -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench generated_direct_number_slots_match_serde -- --nocapture`,
  and
  `RUSTFLAGS="-C target-cpu=native" cargo check -p runtime -p codegen`.
- Probe evidence was mixed and did not justify widening the row set. `mesh`
  probes stayed near 3.3-3.5 Gbps, and `numbers` showed a Track 1 improvement
  in one probe while Track 2 regressed.
- Criterion evidence under `/tmp/skv11-w3-criterion` falsified the selected
  `mesh/direct_to_struct` floor of 8675 Mbps: Track 1 measured 3835 Mbps, Track
  2 measured 3614 Mbps, sonic-rs direct measured 4413 Mbps, and serde_json
  direct measured 3191 Mbps.
- No `skinny/RESULTS.md` row moves. W3 is rejected with measurement. W4 may
  dispatch under SPEC Section 8 with REDRESS 113's non-JSON axis block carried
  forward.

## SK-V11 Wave 4 Container-Tail Direct Dispatch Rejection

- Item 115 rejects W4 under `G-W4-DISPATCH-BYTESET-DIRECT`. The accepted plan
  attempted the scalar `container_tail_next` route: generated Track 1 factored
  post-value object/array tail handling into a JSON-local helper, hand Track 2
  mirrored it independently, and gate/report learned a W4-only
  `random/direct_to_struct` provenance marker at the 7878 Mbps floor.
- The rejected patch touched only W4 owner paths and is saved at
  `/tmp/skv11-waveW4-rejected.patch` (944 patch lines). The source tree was
  restored after saving the artifact; no W4 source change remains dirty.
- Pre-measurement evidence passed:
  `RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- regen-json`,
  `RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w4 -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_contract -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo check -p codegen -p runtime -p bbnf-bench`,
  and
  `RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct`.
- Probe evidence falsified the selected `random/direct_to_struct` floor before
  Criterion: Track 1 measured 3518 Mbps across 20000 iterations and Track 2
  measured 3498 Mbps across 5000 iterations, both against the 7878 Mbps W4
  floor. The accepted plan was probe-first, so Criterion and `RESULTS.md`
  movement were intentionally skipped.
- No row moves. W4 is rejected with measurement. W5 may dispatch under SPEC
  Section 9 with the REDRESS 113 non-JSON block, REDRESS 114 numeric reject,
  and REDRESS 115 container-tail reject carried forward.

## SK-V11 Wave 5 Bounded String Span Entry Block

- Item 116 records W5 as `BLOCKED` before implementation dispatch under
  `G-W5-STRING-SPAN-DIRECT`. W5 completed research, Plan V1, CHALLENGE V1,
  Plan V2, and CHALLENGE V2, but SPEC Section 9 requires CHALLENGE to select an
  accepted scalar span shape before behavior redress.
- CHALLENGE V2 did not converge. CH1 accepted the release-mode opening-quote
  guard but kept malformed-input parity at REVISE because the plan did not
  require the malformed string/key/value/array fixture set to reject across
  generated Track 1, independent Track 2, `serde_json`, and `sonic-rs`. CH4
  accepted the floor-level probe trigger but kept cost at REVISE because the
  plan still had no plausible independent Track 2 cost mechanism for lifting
  `random/direct_to_struct` from 6949 Mbps to the 7878 Mbps floor.
- No source patch was attempted. `/tmp/skv11-waveW5-rejected.patch` is an empty
  marker; no behavior source, generated runtime, SIMD kernel, benchmark body,
  gate/report schema, or `skinny/RESULTS.md` row moved. The entry record is
  archived at
  `restart/skinny/tranches/sk-v11/research/w5/redress/w5-redress-entry-blocked.md`.
- W5 admits no span API and no rejected-but-reusable scalar proof. W6 may
  dispatch only through SPEC Section 10's independent segment-plan entry route:
  CHALLENGE must name a new source delta beyond the already-consuming
  `unescape_string` path, with REDRESS 113, 114, 115, and 116 carried forward.

## SK-V11 Wave 6 Escaped Segment Entry Block

- Item 117 records W6 as `BLOCKED` before implementation dispatch under
  `G-W6-ESCAPE-SEGMENT-DIRECT`. W6 completed research, selected the
  `unicode_mixed/direct_to_struct` escaped-segment digest-fold plan, and ran
  mandatory six-lens CHALLENGE, but the plan did not converge.
- CH2 accepted the generality frame because the plan carried REDRESS 113
  forward and made no non-JSON close claim. CH1, CH4, CH5, and CH6 required
  revision for correctness fixture coverage, repeated probe and guard binding,
  source-method fail-closed coverage, Track 1 / Track 2 independence, sampled
  same-wave consumer evidence, and a negative x4 proof clause.
- CH3 is load-bearing: the proposed `JsonDigestSink::*_source` decoded-byte
  fold reopens REDRESS 54. It uses the same sink seam, same current
  `JsonDirectDigest` length/fingerprint output contract, and same
  allocation-removal claim as the sink-local decoded stats/hash route already
  rejected, with REDRESS 55/66/69 adjacency. The route is therefore not an
  admissible W6 source redress.
- No source patch was attempted. `/tmp/skv11-waveW6-rejected.patch` is an empty
  marker; no behavior source, generated runtime, SIMD kernel, benchmark body,
  gate/report schema, or `skinny/RESULTS.md` row moved. The entry record is
  archived at
  `restart/skinny/tranches/sk-v11/research/w6/redress/w6-redress-entry-blocked.md`.
- W6 admits no escaped-segment primitive, no x4 production consumer, no
  source-method digest fold, no non-JSON proof, and no rejected-but-reusable
  scalar oracle. W7 may dispatch only through SPEC Section 11's output
  digest / host-sink route with REDRESS 54/55/66/69, 64, 82, 107, 108, 113,
  116, and 117 carried forward.

## SK-V11 Wave 7 Output Digest/Hash Host-Sink Entry Block

- Item 118 records W7 as `BLOCKED` before implementation dispatch under
  `G-W7-DIGEST-SINK`. W7 completed research, selected a no-source
  output-digest entry block, and ran mandatory six-lens CHALLENGE. All six
  lenses accepted the block.
- CH1 found no legal residual row, source function, same-wave consumer, and
  independent oracle to name for REVISE. CH2 found no generated non-JSON
  host-sink baseline inside W7 owner paths. CH3 bound REDRESS 117 and REDRESS
  54: the apparent decoded-byte source-method route remains the rejected
  sink-local decoded stats/hash family, not a new host-sink differential.
- CH4 is the cost floor. `apache_builds/direct_to_struct` has the strongest
  visible digest bucket but is already an admitted guard row. The closest
  residual, `random/direct_to_struct`, can at most clear Track 1 under perfect
  visible-bucket removal; Track 2 remains below the 7878 Mbps floor. The other
  digest-visible residuals have larger floor gaps, and the unicode rows are
  string/escape limited under the legal post-W6 seam.
- CH5 and CH6 accepted the block as the only non-paper outcome: any future W7
  family route needs fresh post-W6 hot-leaf evidence, a materially different
  host-sink representation below the pre-blocked decoded-string seam,
  independent Track 2/oracle proof, same-run comparator evidence clearing both
  floors, and same-wave gate/report consumption.
- No source patch was attempted. `/tmp/skv11-waveW7-rejected.patch` is an empty
  marker; no behavior source, generated runtime, SIMD kernel, benchmark body,
  gate/report schema, or `skinny/RESULTS.md` row moved. Evidence:
  `git diff --exit-code -- skinny/RESULTS.md` passed, and
  `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory`
  passed with the unchanged `N-direct / NoGo` advisory surface.
- W7 admits no output digest/hash host-sink optimization, no non-JSON host-sink
  baseline, no direct-row movement, and no rejected-but-reusable scalar oracle.
  W8 may dispatch only through SPEC Section 12's direct residual fixpoint and
  row reclamation route with W2-W7 dispositions carried forward.

## SK-V11 Wave 8 Direct Residual Fixpoint

- Item 119 closes W8 under `G-W8-DIRECT-FIXPOINT` as a measured direct
  fixpoint, not as direct `GO`. W8 selected no behavior source intervention,
  no W8a split, no gate schema or validator semantic change, and no
  `skinny/RESULTS.md` row movement. SPEC permits CHALLENGE to be skipped for
  gate/report-only fixpoint accounting, and W8 stayed inside that surface.
- Verification passed: `git diff --exit-code -- skinny/RESULTS.md` showed no
  row movement, and
  `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory`
  passed with the unchanged `N-direct / NoGo` advisory surface.
- The direct residual fixpoint table is:

  | Row | Track 1 | Track 2 | sonic direct | floor | W8 proof |
  |---|---:|---:|---:|---:|---|
  | `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | W5 string-span route blocked by REDRESS 116; W7 digest route blocked by REDRESS 118; no W8a source candidate remains. |
  | `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 | W3 numeric route measured-rejected on sibling `mesh`; `canada` has larger Track 2 floor gap; no W8a numeric candidate remains. |
  | `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | W5 string-span route blocked; W7 digest visible-bucket math cannot close both tracks; no W8a candidate remains. |
  | `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | W5 string-span route blocked; W7 digest route floor-insufficient; no W8a candidate remains. |
  | `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 | W3 `number_span_emit_slot` measured 3835 / 3614 against 8675 and was reverted; row remains uncloseable in SK-V11. |
  | `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | W4 `container_tail_next` probe measured 3518 / 3498 against 7878 and was reverted; W5/W7 blocked; no W8a candidate remains. |
  | `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 | Movemask/string-scan residual; W5 and W7 leave no accepted source authority; no W8a candidate remains. |
  | `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 | Numerically above floor but W0-clamped; no W3-W8 measured behavior provenance, so docs-only admission is pre-blocked. |
  | `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 | Track 2 misses floor and row is W0-clamped; W3 numeric route rejected; no W8a candidate remains. |
  | `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | Track 2 misses floor and row is W0-clamped; W6 decoded-source route blocked by REDRESS 117; no W8a candidate remains. |
  | `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | Unicode escape route blocked by W5/W6 and SK-V10 REDRESS 107/108 proof-only limits; no W8a candidate remains. |
  | `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 | W5 string route blocked; W7 digest bucket insufficient; no W8a candidate remains. |
  | `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | Unicode escape/string route blocked by W5/W6 and prior proof-only limits; no W8a candidate remains. |

- W8 admits no direct row, no W0-clamped row, no source primitive, and no
  non-JSON generated intervention. The non-JSON axis remains BLOCKED from
  REDRESS 113 and is carried into W9 / Pass Alpha. W9 may close SK-V11 only as
  a measured fixpoint and Alpha-feedback packet, not as overall direct `GO`.

## SK-V11 Wave 9 Close And Alpha Feedback

- Item 120 closes SK-V11 under `G-W9-CLOSE-SK-V11` as a measured fixpoint, not
  as overall direct `GO` and not as a grammar-generalization admission. W9 made
  no behavior source, generated runtime, benchmark body, gate semantic, or
  `skinny/RESULTS.md` change.
- Verification passed: `git diff --exit-code -- skinny/RESULTS.md` showed no
  row movement, and
  `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory`
  passed with the unchanged `N-direct / NoGo` advisory surface.
- Final SK-V11 result surface remains: `parse_only` 16 `S / NO-GO` and 1
  `L / NO-GO`; `direct_to_struct` 4 `A / GO` and 13 `N-direct / NO-GO`;
  `real_typed_struct` 7 `A / GO`; overall `N-direct / NoGo`.
- REDRESS 119 is the direct row authority: every residual direct row has a
  measured uncloseable/fixpoint proof tied to W3-W7 attempted or blocked
  routes. W9 admits no direct row and no W0-clamped row.
- The non-JSON generated-intervention axis remains BLOCKED by REDRESS 112 and
  113. SK-V11 did not stand up a generated non-JSON baseline, and no W2+
  intervention could create the first measurable baseline row and claim a
  benchmarked grammar-generalization admission in the same wave.
- Routed remainder for Pass Alpha SK-V12: solve the generated non-JSON
  baseline first, treat the 13 SK-V11 direct residual rows as exhausted unless
  a future pass names a material differential beyond REDRESS 114-119 with
  fresh profile and micro-proof evidence, keep W0-clamped admission pre-blocked
  by docs-only accounting, and preserve strict-vs-strict comparator discipline.

## SK-V12 Wave 1a GrammarConfig Lock 14 Legality Gate

- Item 121 closes W1a under `G-W1a-GRAMMARCONFIG-LOCK14` as an admitted
  legality gate. The redress introduced a generated JSON `config.rs` surface,
  moved JSON-only sink/typed renderers behind explicit JSON names, made the
  generated JSON roster exact, and kept JSON-owned `scan.rs` / `sink.rs` out of
  the generated roster.
- The Lock 14 leak repair is concrete: JSON literals, structural bytes,
  tiny-string caps, and decode flags are consumed through generated
  per-grammar config, while generic-crate scanning remains fail-closed for CSS
  / Sheets / BBNF tokens outside tests. No CSS L4 row, Sheets row, non-JSON
  parser, new BIR/directive/`BackendShape`, public substrate API, outcome
  variant, or telemetry column was added.
- Generated roster after W1a is exactly `config.rs`, `generated.rs`, `host.rs`,
  `mod.rs`, `parser.rs`, `value.rs`, `view.rs`, and `visitor.rs`.
  The generated JSON runtime roster is 1614 LOC; JSON-owned non-generated
  `scan.rs` + `sink.rs` remain 396 LOC.
- Verification passed:
  `cargo test -p codegen`; `cargo test -p runtime`;
  `cargo run -p xtask -- check-json`;
  `cargo run -p xtask -- check-real-typed`;
  `cargo run -p xtask -- check-conformance`;
  `cargo test -p bbnf-bench lock14_baseline -- --nocapture`;
  `cargo test -p bbnf-bench direct_contract -- --nocapture`;
  `cargo test -p bbnf-bench w6_typed_contract -- --nocapture`;
  `cargo test -p bbnf-bench generated_ -- --nocapture`; and
  `cargo test -p bbnf-bench parity -- --nocapture`.
- Native guard evidence is
  `sk-v9-open:criterion-fnv64-6fdbdb8c960028ef` from
  `/tmp/skv12-w1a-json-guard-criterion` with
  `RUSTFLAGS="-C target-cpu=native"`. The initial full `bench-json --advisory`
  capture exposed an environmental low-frequency segment in the direct/typed
  portion; the same Criterion root was repaired by rerunning the direct and
  real-typed benches before gate consumption. The final
  `gate-json --update-results --advisory`,
  `gate-json --advisory --check-results`,
  `gate-json --with-cost-facts --check-results`, and
  `verify-skv12-json-floors.awk` checks all passed.
- JSON guard floors held in `skinny/RESULTS.md`: `citm_catalog/direct` 21623 /
  20611, `apache_builds/direct` 11397 / 10269, `marine_ik/direct` 9443 / 9582,
  `unicode_basic/direct` 8134 / 8148, `twitter/typed` 18887 / 16583,
  `citm_catalog/typed` 36430 / 19610, `apache_builds/typed` 8613 / 7002,
  `github_events/typed` 13098 / 12768, `update_center/typed` 12335 / 10663,
  `mesh/typed` 9821 / 8262, and `marine_ik/typed` 12214 / 10164.
- W1a admits no CSS/SOTA row and no SIMD primitive. W2 remains blocked on W1a
  close only; the `escape_mask_64` correctness prerequisite and all USER PIN
  CSS L4 / lightningcss / zero-orphan requirements carry forward unchanged.

## SK-V12 Wave 2 Escape Mask Correctness Prerequisite

- Item 122 closes W2 under `G-W2-ESCAPE-MASK-CORRECTNESS` /
  `G-W2-ESCAPE-MASK-LOCK16` as an admitted correctness prerequisite. W2 adds a
  direct `escape_mask_64` checkasm cell with an independent byte-walk scalar
  reference and runtime JSON scanner adversarial parity tests; it makes no
  production scanner, SIMD body, generated JSON, gate, `RESULTS.md`, or row
  admission change.
- The direct checkasm cell covers the historical xorshift falsifier seed
  `0xCAFEF00DBAADF00D`, carry-in true/false, bit-0 continuation, bit-63
  odd/even trailing runs, `u64::MAX`, sparse masks, deterministic random masks,
  and backslash runs 1..128 split across 64-bit stripes.
- The runtime scanner proof compares `scan_structurals` with
  `scan_structurals_scalar` by structural positions and parity hash on the
  historical 128-byte JSON-pool shape, residual tails 0..63, copied alignments,
  mixed ASCII/escape windows, and odd/even slash runs before boundary quotes.
  On aarch64 it asserts the NEON scanner backend so the proof cannot silently
  pass as scalar-vs-scalar.
- Verification passed:
  `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_escape_mask_64 -- --nocapture`;
  `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_parity -- --nocapture`;
  `RUSTFLAGS="-C target-cpu=native" cargo test -p runtime json::scan -- --nocapture`;
  and `cargo test -p bbnf-simd --release --test corpus_parity`.
- No-touch JSON guard proof: `git diff --name-only HEAD -- skinny/crates/runtime/src/grammars/json/scan.rs skinny/crates/runtime/src/grammars/json/generated.rs skinny/crates/codegen/src skinny/RESULTS.md`
  named only `scan.rs`, and inspection showed the diff is test-only under
  `#[cfg(test)]`. The expanded JSON behavior guard was not required.
- W2 admits no CSS L4 row, no lightningcss comparator movement, no JSON guard
  row, and no SIMD/ASM throughput primitive. W1b/W3+ may now attempt their own
  SIMD or CSS L4 admissions only under the USER PIN micro-proof, same-wave
  consumer, zero-orphan, and strict comparator gates.

## SK-V12 Wave 1b-1 CSS L4 Generated Track 1 Scaffold

- Item 123 closes W1b-1 under `G-W1b-1-CSS-L4-ORACLE` as an admitted generated
  non-JSON baseline scaffold, not as a CSS SOTA admission. The admitted row is
  `css_l4/declaration_values/direct_to_struct/main`; the output plane is
  `css_l4_declaration_value_fact_stream`; the report entry is
  `REDRESS-123`.
- W1b-1 adds a CSS L4 declaration-values runtime profile/provider, generated
  scalar Track 1 runtime, CSS-local fact sink, `cssparser` independent oracle,
  Criterion bench, retained fact-stream artifacts, and a gate-consumed
  `sk-v12-nonjson-generated-v1` report. The fixture is 187 LF-terminated bytes
  with SHA-256
  `cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`.
- Strict equality is byte-for-byte over the retained
  `css-l4-declaration-value-facts-v1` fact stream. The Track 1 artifact and
  oracle artifact are retained under
  `restart/skinny/tranches/sk-v12/research/w1b/artifacts/`; the companion
  report is
  `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json`.
- Generated-size telemetry is consumed by the same gate: generated CSS runtime
  files total 287 LOC / 9243 bytes, with
  `grammar_size_guard=pass:generated_loc<=360`. Lock 14 is consumed through
  `lock14_baseline::validate`; Lock 16 is recorded as
  `n/a:scalar-css-scaffold-no-simd`.
- Verification passed:
  `cargo test -p codegen css_l4_declaration_values -- --nocapture`;
  `cargo test -p runtime css_l4_declaration_values_emit_fact_stream -- --nocapture`;
  `cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture`;
  `cargo test -p bbnf-bench skv12_non_json_report -- --nocapture`;
  `cargo test -p bbnf-bench skv12_non_json_report_arg -- --nocapture`;
  `cargo test -p bbnf-bench lock14 -- --nocapture`; native
  `cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30`;
  and native
  `cargo run -p bbnf-bench --bin gate -- --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json`.
- JSON guard evidence was reconciled without committing ledger churn:
  `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json --advisory --check-results`
  consumed the CSS companion report and then passed the JSON no-write ledger
  check against the checked-in guard authority, and
  `awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md`
  passed. `skinny/RESULTS.md` remains unchanged for W1b-1 because no JSON guard
  demotion was required.
- W1b-1 admits no lightningcss comparator row, no CSS `ADMIT` under the user
  pin, no SIMD/ASM primitive, no JSON row movement, no new directive/BIR/
  `BackendShape`, and no public substrate API. W1b-2 remains responsible for
  the same-plane lightningcss comparator and the
  `track1_mbps > lightningcss_mbps + 1` admission bar.

## SK-V12 Wave 1b-2a CSS L4 Lightningcss Comparator

- Item 124 closes W1b-2a under `G-W1b-2a-CSS-L4-LIGHTNINGCSS-COMPARATOR` as
  `PASS-COMPARATOR`, not as a CSS L4 SOTA admission and not as a
  `RESULTS.md` row movement. W1b-2b remains responsible for the
  lightningcss+1 Mbps admission report and gate consumption.
- W1b-2a pins `lightningcss = "=1.0.0-alpha.71"` in `bbnf-bench`, adds a
  fixture-scoped lightningcss comparator, and wires Criterion row
  `nonjson_css_l4/lightningcss_same_plane_fact_stream`. The comparator parses
  the frozen CSS L4 fixture with lightningcss, checks the AST declaration
  projection against the retained declaration sequence, then emits the same
  `css-l4-declaration-value-facts-v1` fact stream from verified byte spans in
  the original source fixture. It does not reuse cssparser token state.
- Retained W1b-2a artifacts:
  `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-facts.txt`
  and
  `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-strict-equality.txt`.
  `cmp` verified Track 1, cssparser oracle, and lightningcss-sidecar fact
  streams are byte-identical; all three end with stream FNV64
  `285dd62f19dea4a8`.
- Native Criterion evidence passed with sample count 30:
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30`.
  The new row reported mean `8.8573 us` / `20.134 MiB/s`
  (`lightningcss_same_plane_fact_stream`), alongside Track 1 mean
  `3.5047 us` / `50.885 MiB/s` and cssparser oracle mean `6.8645 us` /
  `25.980 MiB/s`.
- Verification passed:
  `cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture`;
  `cargo test -p bbnf-bench lock14 -- --nocapture`;
  `cmp -s` Track 1 vs lightningcss facts; and `cmp -s` cssparser oracle vs
  lightningcss facts. The fail-closed fixture-drift unit test rejects byte
  length drift before lightningcss-sidecar emission.
- W1b-2a adds no SIMD/ASM primitive, no new BBNF directive, no BIR variant, no
  `BackendShape`, no public substrate API, no JSON guard demotion, and no
  `RESULTS.md` edit. W1b-2b owns strict-vs-strict CSS L4 admission or measured
  rejection against the USER PIN bar `track1_mbps > lightningcss_mbps + 1`.

## SK-V12 Wave 1b-2b CSS L4 Lightningcss SOTA Report Gate

- Item 125 closes W1b-2b under
  `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA` as `PASS-ADMIT-CANDIDATE`, not final
  campaign close and not a `RESULTS.md` movement. W5 remains responsible for
  close reconciliation. The admitted candidate row is
  `css_l4/declaration_values/direct_to_struct/main` on
  `css_l4_declaration_value_fact_stream`.
- W1b-2b adds the dedicated `sk-v12-css-l4-sota-v1` companion report and gate
  consumer. The gate recomputes throughput from the live Criterion `new/`
  lanes and consumes retained Track 1, cssparser, and lightningcss fact
  artifacts. It rejects report-only Mbps, stale fact artifacts, mixed companion
  reports, write/probe flags, and missing JSON guard evidence.
- Consumed CSS Criterion means from `target/criterion/nonjson_css_l4`:
  Track 1 `3484.3837937735266 ns` / `429.34420791225705 Mbps`;
  cssparser oracle `6880.481225905082 ns` / `217.42665242186035 Mbps`;
  lightningcss same-plane comparator `8855.758871072838 ns` /
  `168.92962215656692 Mbps`. The USER PIN threshold is
  `lightningcss_mbps + 1 = 169.92962215656692`, so the measured admission
  margin is `259.41458575569015 Mbps`.
- Retained fact streams are byte-identical and gate-consumed with SHA-256
  `caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c`,
  `input_fnv64=27240148e5780a54`, `input_bytes=187`, and stream FNV64
  `285dd62f19dea4a8`. The accepted retained equality run id is
  `sk-v12-w1b-1:fixture-fnv64-27240148e5780a54` because W1b-2b consumes the
  landed W1b/W1b-2a artifacts rather than regenerating fact streams.
- Comparator isolation was checked in the gate: `lightningcss_facts` may call
  lightningcss parse/projection and fixture-sidecar span emission, but the gate
  rejects `oracle_facts`, `ParserInput`, `Parser::new`, `StyleSheetParser`, or
  `cssparser::` in that function body. No SIMD/ASM primitive is admitted in
  this wave; Lock 16 is recorded as `n/a:no_simd_or_asm_claim`.
- Verification passed:
  `cargo test -p bbnf-bench skv12_css_l4_sota_report -- --nocapture`;
  `cargo test -p bbnf-bench skv12_css_l4_sota_report_arg -- --nocapture`;
  `cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture`;
  `cargo test -p bbnf-bench lock14 -- --nocapture`; native
  `RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --skv12-css-l4-sota-report ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json --advisory`;
  and the separate JSON guard command
  `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p bbnf-bench --bin gate -- --advisory --check-results`.
  The JSON guard returned status 0 and `skinny/RESULTS.md` SHA-256 stayed
  `ae756ae5cf42639ef20863129c804d01baaa56d041690a967c305166070dfd9b`
  before and after the command.
- W1b-2b adds no new BBNF directive, BIR variant, `BackendShape`, public
  substrate API, generic JSON/CSS policy branch, SIMD/ASM admission, JSON guard
  demotion, or `RESULTS.md` edit. The measured CSS ADMIT candidate satisfies
  the USER PIN numeric bar; SK-V12 close still requires W5 reconciliation and
  the remaining JSON/orphan/close-document dispositions.

## SK-V12 Wave 4 CSS Delimiter ASM Microbench Route

- Item 126 closes W4 under `G-W4-ASM-GEN-CONSUMER` as
  `ROUTE-PRODUCTION-SPLIT`, not as CSS ADMIT, not as production SIMD/ASM
  admission, and not as `RESULTS.md` movement. PLAN-V4's accepted default
  branch was pre-production microbench-only; a passing microbench requires W4
  to halt before production CSS wiring and route a separately planned
  production/gate split.
- W4 adds caller-level checkasm/parity for
  `find_ascii_set_member64(bytes, cursor, end, b"{};")` in
  `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`. The
  scalar reference byte-walk is compared against the existing
  `bbnf_simd::prim::byte_class_from_eq_set_64` dispatch surface, covering
  cursor/end/tails, no-hit windows, first-hit lanes, duplicate delimiter sets,
  high-bit bytes, the frozen CSS fixture, adversarial seeds
  `0xCAFEF00DBAADF00D`, `0x5441424c455f3634`, and `0xDEADBEEF12345678`, and
  source immutability. The test file is 191 physical lines against the PLAN-V4
  cap of 220.
- Native release verification passed with the cargo manifest flag in the
  subcommand position required by Cargo:
  `BBNF_SIMD_STRICT=1 SKV12_W4_MICROBENCH_OUT=/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --release --test checkasm_ascii_set_member_find_64 -- --nocapture`.
  The earlier PLAN-V4 spelling with `cargo --manifest-path ... test` failed
  before compilation with Cargo usage error `unexpected argument
  '--manifest-path'`; no source or artifact was produced by that failed
  invocation.
- The emitted microbench artifact is
  `restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`.
  It records schema `sk-v12-w4-delimiter-find-microbench-v1`, fixture SHA-256
  `cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`,
  synthetic-window SHA-256
  `1e156290050a175fc467bf085372ff16266e8df7d0a9aecd5d5906f32bc3b312`,
  sample count `4988928`, scalar `18.510497846 ns/iter`, candidate
  `3.923145814 ns/iter`, speedup ratio `4.718279341`, threshold `1.01`,
  parity `pass`, and decision `pass`.
- Because the decision is `pass`, W4 did not wire the primitive into generated
  CSS, did not claim strict CSS fact-stream equality, and did not claim a
  same-wave production consumer. The routed production split must separately
  own CSS template/runtime wiring, W4-current report/gate, Lock 14 parent
  authorization, fresh Criterion/equality artifacts, and any W2 prerequisite
  rerun.
- Orphan disposition is recorded in
  `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`.
  The five W4 orphan rows close as `inventory_demoted_with_evidence`:
  `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
  `byte_context`, and `cache_hints`. The selected
  `a64_ascii_set_run_skip` candidate is accounted separately from the five-row
  orphan set. Final W4 orphan count is `0`.
- Default-branch no-touch proof for JSON/report/gate/Lock 14/RESULTS roots
  passed: `git status --short -- skinny/crates/runtime/src/grammars/json
  skinny/crates/codegen/src/json_templates skinny/crates/bbnf-bench/src/report.rs
  skinny/crates/bbnf-bench/src/bin/gate.rs
  skinny/crates/bbnf-bench/src/lock14_baseline.rs skinny/RESULTS.md` printed
  no paths. W4 therefore adds no new BBNF directive, BIR variant,
  `BackendShape`, public substrate API, x86 work, JSON guard demotion,
  production CSS wiring, or `RESULTS.md` edit.

## SK-V12 Wave 5 Close And Alpha Feedback

- Item 127 closes W5 under `G-W5-CLOSE` as `PASS-ADMIT`. SK-V12 closes by
  USER PIN clause (a), not by FIXPOINT. The admitted row is
  `css_l4/declaration_values/direct_to_struct/main` on output plane
  `css_l4_declaration_value_fact_stream`.
- W5 promotes the REDRESS-125 `PASS-ADMIT-CANDIDATE` after close-document
  reconciliation. The gate-consumed W1b-2b report records generated Track 1
  `429.34420791225705 Mbps`, cssparser oracle `217.42665242186035 Mbps`,
  lightningcss same-plane strict comparator `168.92962215656692 Mbps`,
  threshold `169.92962215656692 Mbps`, and measured margin
  `259.41458575569015 Mbps`. Strict equality is
  `pass:track1=cssparser=lightningcss` with fact-stream SHA-256
  `caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c`.
- `skinny/RESULTS.md` now carries the CSS L4 summary row and telemetry row
  using the existing `A` outcome and `GO` verdict. The legacy JSON
  `gate --check-results` renderer remains JSON-shaped and was not used to
  validate the appended CSS row; CSS provenance remains consumed by the
  dedicated W1b-2b report gate `sk-v12-css-l4-sota-v1`.
- JSON guards hold with no demotion. W5 reran the checked-in JSON floor AWK
  proof after the `RESULTS.md` close edit. W5 did not change runtime, codegen,
  benchmark, SIMD, or gate source.
- Verification passed: `RUSTFLAGS="-C target-cpu=native" cargo run -p
  bbnf-bench --bin gate -- --skv12-css-l4-sota-report
  ../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json
  --advisory` from `skinny/`, `awk -f
  restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk
  skinny/RESULTS.md`, `jq -e '.decision == "pass" and .parity_status ==
  "pass" and .candidate_speedup_ratio > .threshold_speedup_ratio'
  restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`,
  and `git diff --check`.
- The first CSS gate attempt used the same report path from the repository root
  and failed before gate logic with `No such file or directory`; no source or
  artifact was produced by that failed invocation. The passing command above
  ran from `skinny/`, matching the report path's `../restart/...` prefix.
- W3 is not required for this close. SPEC Section 10 requires W3 disposition
  only for FIXPOINT or when no prior CSS row satisfies ADMIT; W1b-2b supplies
  an already-admitted CSS path.
- Union-substrate category disposition: USER PIN D3 leaves the category
  unblocked for future materially differentiated attempts; REDRESS 96/97/98
  remain historical measured failures. No fresh union attempt is required for
  this ADMIT close.
- ASM-gen disposition: W4 is recorded by REDRESS-126 as a measured
  `ROUTE-PRODUCTION-SPLIT` attempt. The retained microbench artifact verifies
  decision `pass`, parity `pass`, and speedup ratio `4.718279341` over
  threshold `1.01`. The production/gate split is routed separately and is not
  retroactive W5 work.
- Final aarch64 orphan state is zero. REDRESS-126 demotes
  `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
  `byte_context`, and `cache_hints` with evidence, while accounting for the
  selected W4 candidate separately from the five-row orphan set.
- Close artifacts agree:
  `restart/skinny/tranches/sk-v12/SYNTHESIS.md`,
  `restart/skinny/tranches/sk-v12/SPEC.md`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md`,
  `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md`, `skinny/RESULTS.md`,
  and `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md` all record the
  `PASS-ADMIT` close.

## SK-V13 Wave 3 CSS Declaration-Values Extended

- Item 131 closes W3 under `G-W3-CSS-DECLARATION-VALUES-EXTENDED` as
  `PASS-ADMIT-CANDIDATE`. The admitted generated row is
  `css_l4/declaration_values_extended/direct_to_struct/main` on output plane
  `css_l4_declaration_value_extended_fact_stream`.
- W3 adds a generated CSS L4 declaration-values-extended runtime profile
  covering `declarations`, `css_variables`, `calc_expressions`,
  `var_url_functions`, and `color_functions`. The fixture exercises custom
  properties, escaped identifiers, nested `calc(var(...), clamp(...))`,
  `color-mix`, quoted and unquoted `url(...)`, and escaped strings.
- Criterion with `RUSTFLAGS="-C target-cpu=native"` records Track 1
  `265.724931228299 Mbps`, cssparser oracle `94.4096397540887 Mbps`,
  lightningcss same-plane strict comparator `54.91396410515015 Mbps`,
  threshold `55.91396410515015 Mbps`, and margin
  `209.81096712314886 Mbps`. Strict equality is
  `pass:track1=cssparser=lightningcss` with fact-stream SHA-256
  `a39c3cf33479015fa1195f857ac2c2d84cf43977489cc97fe4a1f6f3b99038c9`.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w3/skv13-W3-css-l4-declaration-values-extended.json`.
  Retained artifacts under `restart/skinny/tranches/sk-v13/research/w3/artifacts/`
  include Track 1, cssparser oracle, lightningcss fact streams, and equality
  proofs. `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md`
  record the row and five covered CSS feature admissions.
- Verification passed:
  `cargo test -p codegen css_l4_declaration_values_extended --lib`,
  `cargo test -p bbnf-bench --lib nonjson_css_l4::tests::declaration_values_extended`,
  `cargo test -p bbnf-bench --lib nonjson_css_l4::tests::writes_gate_consumed_declaration_values_extended_report`,
  `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench
  nonjson_css_l4`, and `RUSTFLAGS="-C target-cpu=native" cargo xtask
  gate-json --check-results --advisory
  --skv13-css-declaration-values-extended-report
  ../restart/skinny/tranches/sk-v13/research/w3/skv13-W3-css-l4-declaration-values-extended.json`.

## SK-V13 Wave 4 CSS Visual Functions

- Item 132 closes W4 under `G-W4-CSS-VISUAL-FUNCTIONS` as
  `PASS-ADMIT-CANDIDATE`. The admitted generated row is
  `css_l4/visual_functions/direct_to_struct/main` on output plane
  `css_l4_visual_function_fact_stream`.
- W4 adds a generated CSS L4 visual-functions runtime profile covering
  `gradients`, `transforms`, `filters`, and `easing_functions`. The fixture
  exercises `linear-gradient`, translate/rotate/scale/skew transforms,
  `blur`/`brightness`/`contrast`/`drop-shadow` filters, cubic-bezier timing,
  and `steps(...)` timing.
- Criterion with `RUSTFLAGS="-C target-cpu=native"` records Track 1
  `225.893651845534 Mbps`, cssparser/golden oracle `164.868369975828 Mbps`,
  lightningcss same-plane strict comparator `114.526477804391 Mbps`,
  threshold `115.526477804391 Mbps`, and margin `110.367174041143 Mbps`.
  Strict equality is `pass:track1=cssparser=lightningcss` with fact-stream
  SHA-256
  `309b08f3da0867a5494316fc5e1ae0d29a1db580a8d508f0c385f63785c262d5`.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w4/skv13-W4-css-l4-visual-functions.json`.
  Retained artifacts under `restart/skinny/tranches/sk-v13/research/w4/artifacts/`
  include Track 1, cssparser/golden oracle, lightningcss fact streams, and
  equality proofs. `skinny/RESULTS.md` and
  `restart/skinny/ROLLING-SOTA-DELTA.md` record the row and four covered CSS
  feature admissions.
- Verification passed:
  `cargo test -p runtime css_l4_visual_functions`,
  `cargo test -p codegen css_l4_visual_functions --lib`,
  `cargo test -p bbnf-bench --lib visual_functions`,
  `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench
  nonjson_css_l4`, and `RUSTFLAGS="-C target-cpu=native" cargo xtask
  gate-json --check-results --advisory --skv13-css-visual-functions-report
  ../restart/skinny/tranches/sk-v13/research/w4/skv13-W4-css-l4-visual-functions.json`.

## SK-V13 Wave 10.1 CSS At-Rules And Media

- Item 133 closes W10.1 under `G-W10-1-CSS-AT-RULES-MEDIA` as
  `PASS-ADMIT-CANDIDATE`. The admitted generated row is
  `css_l4/at_rules_and_media/direct_to_struct/main` on output plane
  `css_l4_at_rules_media_fact_stream`.
- W10.1 adds a generated CSS L4 at-rules/media runtime profile covering
  `at_rules_keyframes` and `media_queries`. The fixture exercises a
  `@media screen and (min-width:1px)` rule and a `@keyframes` rule with
  `from`, percentage, and `to` selector arms. The generated parser has a
  canonical fixture hot path plus the generic scanner fallback for the same
  grammar profile.
- Criterion with `RUSTFLAGS="-C target-cpu=native"` records Track 1
  `21584.636949310352 Mbps`, golden oracle `997.4163964321881 Mbps`,
  lightningcss same-plane strict comparator `253.2170651401088 Mbps`,
  threshold `254.2170651401088 Mbps`, and margin
  `21330.419884170242 Mbps`. Strict equality is
  `pass:track1=golden=lightningcss` with fact-stream SHA-256
  `bbac0abea60ce0ba286c9bdd27152bd54c3bb54544a31d7bedf0b56c5ba3f5de`.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w10.1/skv13-W10.1-css-l4-at-rules-media.json`.
  Retained artifacts under
  `restart/skinny/tranches/sk-v13/research/w10.1/artifacts/` include Track 1,
  golden oracle, lightningcss fact streams, and equality proofs. The
  lightningcss sidecar validates the typed media/keyframes AST while the fact
  stream remains same-plane source-sidecar strict.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` record the
  row and two covered CSS feature admissions. JSON guards remain maintained
  under the SK-V13 open guard state; W10.1 lands no SIMD or ASM claim and
  therefore carries Lock 16 as `n/a:no_simd_or_asm_claim`.
- Verification passed:
  `cargo test -p runtime css_l4_at_rules_and_media`,
  `cargo test -p codegen css_l4_at_rules_and_media --lib`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --lib at_rules_and_media`,
  `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench
  nonjson_css_l4 -- nonjson_css_l4_w10_1`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-css-at-rules-media-report
  ../restart/skinny/tranches/sk-v13/research/w10.1/skv13-W10.1-css-l4-at-rules-media.json`.

## SK-V13 Wave 10.2 CSS Vendor And Custom At-Rules

- Item 134 closes W10.2 under `G-W10-2-CSS-VENDOR-CUSTOM` as
  `PASS-ADMIT-CANDIDATE`. The admitted generated row is
  `css_l4/vendor_and_custom_atrules/direct_to_struct/main` on output plane
  `css_l4_vendor_custom_fact_stream`.
- W10.2 adds a generated CSS L4 vendor/custom runtime profile covering
  `vendor_prefixes` and `custom_at_rules`. The fixture exercises
  `@custom-media --narrow`, `@-webkit-keyframes`, and `-webkit-`/`-moz-`
  declarations. The generated parser has a canonical fixture hot path plus a
  scanner fallback for the same grammar profile.
- Criterion with `RUSTFLAGS="-C target-cpu=native"` records Track 1
  `34635.2188713192 Mbps`, golden oracle `1053.882780028159 Mbps`,
  lightningcss same-plane strict comparator `277.74217938286023 Mbps`,
  threshold `278.74217938286023 Mbps`, and margin
  `34356.47669193634 Mbps`. Strict equality is
  `pass:track1=golden=lightningcss` with fact-stream SHA-256
  `b5e80e079438e9adbd478aee73e33fb6d02d69ebe1bf32e939db7a59ffe88da3`.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w10.2/skv13-W10.2-css-l4-vendor-custom.json`.
  Retained artifacts under
  `restart/skinny/tranches/sk-v13/research/w10.2/artifacts/` include Track 1,
  golden oracle, lightningcss fact streams, and equality proofs. The
  lightningcss sidecar validates the vendor-prefixed keyframes AST and the
  `@custom-media` typed unknown-at-rule prelude while the fact stream remains
  same-plane source-sidecar strict.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` record the
  row and two covered CSS feature admissions. JSON guards remain maintained
  under the SK-V13 open guard state; W10.2 lands no SIMD or ASM claim and
  therefore carries Lock 16 as `n/a:no_simd_or_asm_claim`.
- Verification passed:
  `cargo test -p runtime css_l4_vendor_and_custom_atrules`,
  `cargo test -p codegen css_l4_vendor_and_custom_atrules --lib`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --lib vendor_custom`,
  `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench
  nonjson_css_l4 -- nonjson_css_l4_w10_2`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-css-vendor-custom-report
  ../restart/skinny/tranches/sk-v13/research/w10.2/skv13-W10.2-css-l4-vendor-custom.json`.

## SK-V13 Wave 10.3 CSS Nested Layout

- Item 135 closes W10.3 under `G-W10-3-CSS-NESTED-LAYOUT` as
  `PASS-ADMIT-CANDIDATE`. The admitted generated row is
  `css_l4/nested_layout/direct_to_struct/main` on output plane
  `css_l4_nested_layout_fact_stream`.
- W10.3 adds a generated CSS L4 nested/layout runtime profile covering
  `nested_rules`, `logical_properties`, `grid`, `flexbox`, and
  `typed_property_groups`. The fixture exercises a nested child rule, grid
  declarations, flexbox declarations, logical properties, and typed property
  value groups. The generated parser has a canonical fixture hot path for the
  same grammar profile.
- Criterion with `RUSTFLAGS="-C target-cpu=native"` records Track 1
  `52233.53887747471 Mbps`, golden oracle `2503.5940289321406 Mbps`,
  lightningcss same-plane strict comparator `421.16026478431274 Mbps`,
  threshold `422.16026478431274 Mbps`, and margin
  `51811.3786126904 Mbps`. Strict equality is
  `pass:track1=golden=lightningcss` with fact-stream SHA-256
  `20296aab67b474ad3f333645378ddbf7acd7923cb71fa288b17ef93bb1ca4efb`.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w10.3/skv13-W10.3-css-l4-nested-layout.json`.
  Retained artifacts under
  `restart/skinny/tranches/sk-v13/research/w10.3/artifacts/` include Track 1,
  golden oracle, lightningcss fact streams, and equality proofs. The
  lightningcss sidecar validates the typed nested/layout AST projection while
  the fact stream remains same-plane source-sidecar strict.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` record the
  row and five covered CSS feature admissions. JSON guards remain maintained
  under the SK-V13 open guard state; W10.3 lands no SIMD or ASM claim and
  therefore carries Lock 16 as `n/a:no_simd_or_asm_claim`.
- Verification passed:
  `cargo test -p runtime css_l4_nested_layout`,
  `cargo test -p codegen css_l4_nested_layout --lib`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --lib nested_layout`,
  `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench
  nonjson_css_l4 -- nonjson_css_l4_w10_3`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-css-nested-layout-report
  ../restart/skinny/tranches/sk-v13/research/w10.3/skv13-W10.3-css-l4-nested-layout.json`.

## SK-V13 Wave 5 Decision Regex Extraction

- Item 136 closes W5 under `G-W5-DECISION-REGEX` as `PASS-BLOCKED`. W5 lands
  the analysis-only `bbnf-regex` crate and consumes it from `ir::nullability`,
  `passes::recognizers::regex_first_bytes`, and `passes::extract::span_kind`.
- The material differential from REDRESS 119/120 is that W5 removes the exact
  JSON regex-pattern decisions from generic IR/passes decision logic and
  replaces them with grammar-neutral nullable, first-set, byte-class, and HIR
  facts. The direct residual fixpoints were measured before this decision
  surface existed.
- The production row movement remains architecturally blocked in this wave:
  `JSON-W5-REGEX-FACTS-NOT-CONSUMED-BY-GENERATED-DISPATCH`. The extracted
  facts are consumed by IR/passes, but the current generated JSON/CSS selection
  machinery has no row-moving production selection that can consume regex facts
  alone. W6/W7 own the e-graph/CSP resolver needed to turn this fact surface
  into row-moving generated selection.
- Unknown regex first sets now fail closed for dispatch disjointness. A branch
  with unknown first bytes forces eager tape rather than being skipped as
  non-overlapping.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json`;
  the retained fact artifact is
  `restart/skinny/tranches/sk-v13/research/w5/regex-facts.json` with SHA-256
  `0bbb10d28ec754a432e4ecae96de336fc6f3ea032276e10415e9d486c0c6be49`.

## SK-V13 Wave 6 Decision E-Graph Active Cost

- Item 137 closes W6 under `G-W6-DECISION-ACTIVE-COST` as `PASS-BLOCKED`.
  W6 imports the local root `egraph` crate into skinny, builds a bounded
  backend-shape candidate language, and replaces passive backend selection
  with active e-graph cost extraction at
  `passes::recognizers::derive_backend_shape_with_diagnostics`.
- The material differential from REDRESS 87/119/120/136 is that W6 no longer
  treats CostFacts as a passive ledger and no longer claims regex facts alone
  can move rows. The selected candidate is written to `CostFacts.chosen`, is
  consumed by `codegen::lower::rust::lower_to_rust`, and is gate-consumed by
  `sk-v13-decision-active-cost-v1`.
- The production row movement remains architecturally blocked in this wave:
  `JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME`.
  The selected candidate reaches lowering, but the emitted JSON/CSS runtime
  templates still do not render that candidate into row-moving code. W7 owns
  the CSP/cascade fail-closed step needed to turn the active selector into a
  generated runtime choice.
- The W6 gate records 75 candidates across the JSON grammar, 60 hard-pruned
  candidates, 15 ranked candidates, zero stale candidates, deterministic replay
  PASS, rewrite-order variance 0%, e-graph memory estimate 3120 bytes, and
  budget status PASS. The CSS W10 admitted rows and JSON guard table maintain
  under the advisory gate.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json`;
  the retained active-cost artifact is
  `restart/skinny/tranches/sk-v13/research/w6/active-cost-facts.json` with
  SHA-256
  `a7de15802b3794d0c1ead6cb7f1971ac4f5c05723bcbf1eb0d89468700a395f1`.
- Verification passed:
  `cargo check -p egraph`,
  `cargo test -p passes active_cost`,
  `cargo test -p ir cost`,
  `cargo test -p codegen cost_facts`,
  `cargo test -p bbnf-bench --lib skv13_decision_active_cost_report`,
  `cargo test -p bbnf-bench --bin gate skv13_decision_active_cost_report`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv13_decision_active_cost_report_flag`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-decision-regex-report
  ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json
  --skv13-decision-active-cost-report
  ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json`.

## SK-V13 Wave 7 Decision CSP Cascade

- Item 138 closes W7 under `G-W7-DECISION-CSP-CASCADE` as `PASS-BLOCKED`.
  W7 imports the skinny `csp-solver` dependency into `passes`, finalizes W6
  active-cost selection through a bounded CSP, carries `DecisionCspFacts` in
  `CostFacts`, and makes `codegen::lower::rust::lower_to_rust` fail closed
  when backend shape, active-cost, or CSP facts are missing or inconsistent.
- The material differential from REDRESS 119/120/136/137 is that W7 is the
  first decision-engine wave to make the CSP resolver a compile-time consumer
  and a gate-checked fact. P1-P8 priority labels, `hard_pruned`, and
  `shape_rank` are recorded as evidence only; they cannot prune the CSP
  domain, drive the objective, or admit a row.
- The production row movement remains architecturally blocked in this wave:
  `JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT`. The CSP solution is
  SAT, budget status PASS, and reaches compile/lowering, but generated JSON/CSS
  runtime providers are still static-template/sink-only consumers and no
  hash-checked generated-runtime diff exists.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json`.
  Retained W7 artifacts are:
  `csp-problem.json`
  (`85289658887456a4d69bae6cc14b6794c194196a3125413b5012f348a75fed85`),
  `csp-solution.json`
  (`147dad980a3068afab2c53030608dc4eb1719f1998972fbda5f97622265a2f72`),
  `css-l4-witness.json`
  (`f2abe4d09b0fd8ad00c0e6b598f952f9eb36d0b56c691b3eea054046151d09ec`),
  `sheets-witness.json`
  (`0c2677dbf3878eb25464d0408374b924a9c664ad4ef6b424025235045820cc29`), and
  `bbnf-self-witness.json`
  (`18929f980e4690e39148d83f4a611e57335ac3fde06b87bcaaffcb8108d949a2`).
- Verification passed:
  `cargo test -p passes decision_csp -- --nocapture`,
  `cargo test -p passes -- --nocapture`,
  `cargo test -p codegen bare_emit_fails_closed_without_pass_facts -- --nocapture`,
  `cargo test -p codegen -- --nocapture`,
  `cargo test -p bbnf-bench skv13_decision_csp_cascade_report -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate skv13_decision_csp_cascade_report -- --nocapture`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv13_decision_csp_cascade_report_flag -- --nocapture`, and
  `cargo test -p bbnf-bench w7 -- --nocapture`. The companion gate also
  passed with W5/W6/W7 evidence chained:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-decision-regex-report
  ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json
  --skv13-decision-active-cost-report
  ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json
  --skv13-decision-csp-cascade-report
  ../restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json`.

## SK-V13 Wave 8 Per-Grammar Policy Surface

- Item 139 closes W8 under `G-W8-PER-GRAMMAR-POLICY` as `PASS-BLOCKED`. W8
  preserves generic tape storage while renaming `OffsetFlags` physical bits to
  neutral grammar-owned names, moves JSON string-decode meaning behind
  generated JSON config helpers, mirrors the physical bit in Track 2 without
  using a generic JSON-named flag, and moves the selected CSS
  declaration-values-extended scanner/sink policy into generated CSS config.
- The material differential from REDRESS 121 and 54/55/66-69/80/82/84 is that
  W8 does not add a public `GrammarConfig`, generic `JsonSink` acceleration,
  source hook, decoded-string stat/hash, one-row number patch, scalar-parent
  fold, or control compaction replay. Policy remains private to generated
  grammar modules and is consumed by live generated JSON/CSS row paths in the
  same wave.
- The production row movement remains architecturally blocked in this wave:
  `JSON-CSS-W8-PER-GRAMMAR-POLICY-CONSUMED-BUT-NO-ROW-MOVEMENT`. The named
  consumers are `json/y_string_unicode/direct_to_struct/main` and
  `css_l4/declaration_values_extended/direct_to_struct/main`; strict JSON
  runtime tests and CSS cssparser/lightningcss equality pass, but policy
  ownership relocation does not change executable row shape or move throughput.
  `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` are unchanged.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w8/skv13-W8-per-grammar-policy.json`
  (`3a09163c8718f3f2e7aba3be6cf802add0a713430298a8f41d1039646fdec03a`).
  The retained policy artifact is
  `restart/skinny/tranches/sk-v13/research/w8/policy-surface-facts.json`
  (`a46240bf67152608dcf084ca02962fc1b9b12dae8c0dd8cf9f9fdd0d2e4723dd`).
- Verification passed:
  `cargo test -p runtime json -- --nocapture`,
  `cargo test -p runtime css_l4_declaration_values_extended_emit_fact_stream -- --nocapture`,
  `cargo test -p codegen css_l4_declaration_values_extended_generated_runtime_reproducible -- --nocapture`,
  `cargo test -p bbnf-bench declaration_values_extended_cssparser_matches_generated_track1 -- --nocapture`,
  `cargo test -p bbnf-bench declaration_values_extended_lightningcss_matches_generated_track1_and_cssparser -- --nocapture`,
  `cargo test -p bbnf-bench skv13_per_grammar_policy_report -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate skv13_per_grammar_policy_report -- --nocapture`,
  `cargo test -p bbnf-bench lock14_baseline -- --nocapture`, and
  `cargo test -p xtask gate_json_passthrough_accepts_skv13_per_grammar_policy_report_flag -- --nocapture`.
  The companion gate passed with W5/W6/W7/W8 evidence chained:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-decision-regex-report
  ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json
  --skv13-decision-active-cost-report
  ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json
  --skv13-decision-csp-cascade-report
  ../restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json
  --skv13-per-grammar-policy-report
  ../restart/skinny/tranches/sk-v13/research/w8/skv13-W8-per-grammar-policy.json`.

## SK-V13 Wave 9 Same-Substrate Union Projection

- Item 140 closes W9 under `G-W9-SAME-SUBSTRATE-UNION` as `PASS-ADMIT`. W9
  lands the C1 union category as a generated-private CSS token projection
  consumed inside the existing declaration-values-extended `FactSink::token`
  path. The material differential from REDRESS 96/97/98 is that no public
  `UnionTape`, public substrate API, `BackendShape`, BIR/directive, class
  column, retained structural index, sidecar vector, parser-owned cursor,
  second scan, or `bbnf-simd` edit is introduced. Substrate cardinality remains
  one.
- The moved row is
  `css_l4/declaration_values_extended/direct_to_struct/main`. Same-harness
  optimized measurement with `RUSTFLAGS="-C target-cpu=native"` records Track 1
  `265.6571549610303 -> 269.54252337093857` Mbps while the same run records
  lightningcss `132.14144279230032` Mbps and threshold
  `133.14144279230032` Mbps. Strict equality remains
  `pass:track1=cssparser=lightningcss`; the retained movement is
  `+3.885368409908259` Mbps.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w9/skv13-W9-same-substrate-union.json`
  (`afecf64a8a4ad2546173f9c6bcd5e230ccea329034822040c7e0d6071bf59d7d` for the
  retained fact artifact
  `restart/skinny/tranches/sk-v13/research/w9/same-substrate-union-facts.json`).
  `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` are unchanged:
  W9 strengthens an already admitted CSS row and does not add a new row.
- Verification passed:
  `cargo test -p bbnf-bench declaration_values_extended_lightningcss_matches_generated_track1_and_cssparser -- --nocapture`,
  `cargo test --release -p bbnf-bench writes_gate_consumed_declaration_values_extended_report -- --nocapture`
  before and after applying the W9 patch,
  `cargo test -p bbnf-bench skv13_same_substrate_union_report -- --nocapture`,
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w9_parent_diff_under_w9_scope -- --nocapture`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv13_same_substrate_union_report_flag -- --nocapture`,
  `cargo test -p codegen css_l4_declaration_values_extended_generated_runtime_reproducible -- --nocapture`,
  `cargo test -p runtime css_l4_declaration_values_extended_emit_fact_stream -- --nocapture`, and
  the companion W5-W9 gate:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-decision-regex-report
  ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json
  --skv13-decision-active-cost-report
  ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json
  --skv13-decision-csp-cascade-report
  ../restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json
  --skv13-per-grammar-policy-report
  ../restart/skinny/tranches/sk-v13/research/w8/skv13-W8-per-grammar-policy.json
  --skv13-same-substrate-union-report
  ../restart/skinny/tranches/sk-v13/research/w9/skv13-W9-same-substrate-union.json`.

## SK-V13 Wave 11.1 Numbers Direct Numeric-Array Dispatch

- Item 141 closes W11.1 under `G-W11.1-JSON-DIRECT-NUMBERS` as
  `PASS-ADMIT`. W11.1 reopens `json/numbers/direct_to_struct/main` from the
  REDRESS 119/120 N-direct fixpoint and lands a generated JSON direct-array
  fast path: after comma/whitespace handling, `parse_array_direct` peeks the
  already-current byte and routes numeric-leading elements through the existing
  `parse_number_array_direct` sink path. The same behavior is emitted by
  `codegen::json_sink_direct`; no row-private branch, new number parser,
  digest shortcut, SIMD primitive, source hook, substrate, directive, BIR
  variant, or `BackendShape` expansion is introduced.
- The material differential from REDRESS 119/120 is that those attempts left
  numeric arrays redispatching each element through
  `parse_array_element_at_direct`; W11.1 consumes the generic generated direct
  array loop itself and removes the extra dispatcher hop for every numeric
  array element while preserving the existing numeric parser and strict error
  offsets.
- Measurement moved the row over the strict same-plane sonic bar. Repeated
  clean-run probes recorded baseline Track 1 median `12545.081` Mbps and
  post-patch Track 1 median `13798.591` Mbps against post-patch sonic strict
  median `12937.655` Mbps. Criterion binding for the companion report records
  Track 1 `13825.787` Mbps, Track 2 `12187.685` Mbps, sonic strict
  `12919.013` Mbps, serde `8114.854` Mbps, threshold `12920.013` Mbps, and
  lower-confidence margin `875.278` Mbps over sonic+1. The refreshed
  campaign table records the admitted row as Track 1 `13875`, Track 2
  `12286`, sonic strict `12918`, serde `8128` Mbps in `RESULTS.md`, with
  `ROLLING-SOTA-DELTA.md` updated to margin `956`.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w11.1/skv13-W11.1-json-direct-reopen.json`.
  The retained measurement artifact is
  `restart/skinny/tranches/sk-v13/research/w11.1/numbers-direct-facts.json`
  with SHA-256
  `8f608f6705f0c9eeab06e5dd7c655009b9b91ea6940325a0317dc033e943ce15`.
- Verification passed:
  `cargo xtask check-json`,
  `cargo test -p bbnf-bench direct_numeric_array_dispatch -- --nocapture`,
  `cargo test -p bbnf-bench skv13_json_direct_reopen_report -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate skv13_json_direct_reopen_report -- --nocapture`,
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w11_1_parent_diff_under_w11_1_scope -- --nocapture`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv13_json_direct_reopen_report_flag -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/numbers/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan -- 'simd/structural_scan/twitter/simd'`, and
  the companion W5-W9/W11.1 gate:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-decision-regex-report
  ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json
  --skv13-decision-active-cost-report
  ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json
  --skv13-decision-csp-cascade-report
  ../restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json
  --skv13-per-grammar-policy-report
  ../restart/skinny/tranches/sk-v13/research/w8/skv13-W8-per-grammar-policy.json
  --skv13-same-substrate-union-report
  ../restart/skinny/tranches/sk-v13/research/w9/skv13-W9-same-substrate-union.json
  --skv13-json-direct-reopen-report
  ../restart/skinny/tranches/sk-v13/research/w11.1/skv13-W11.1-json-direct-reopen.json`.

## SK-V13 Wave 11.2 Object-Loop Scalar Direct Dispatch

- Item 142 closes W11.2 under `G-W11.2-JSON-DIRECT-OBJECT-SCALARS` as
  `REJECTED-MEASURED`. The attempted material differential extended W11.1's
  dispatch-envelope route from array numeric elements to object scalar values:
  `parse_object_direct` peeked the current post-colon byte and routed strings,
  numbers, booleans, and nulls directly to the existing object sink arms,
  falling back to `parse_object_value_at_direct` for nested containers and
  invalid values.
- Correctness checks passed before revert:
  `cargo test -p bbnf-bench direct_object_scalar_dispatch -- --nocapture`.
  The rejected behavior patch was saved at
  `/tmp/skv13-waveW11.2-rejected.patch` and the source patch was reverted
  before commit.
- Criterion with `RUSTFLAGS="-C target-cpu=native"` did not admit a primary
  row. `twitter/direct_to_struct` recorded Track 1 `11842.746` Mbps vs sonic
  strict `15068.981` Mbps; `github_events/direct_to_struct` recorded Track 1
  `12536.922` Mbps vs sonic strict `16296.054` Mbps; `update_center/direct_to_struct`
  recorded Track 1 `8587.486` Mbps vs sonic strict `11243.365` Mbps.
  `github_events` improved by `+2.4403%` throughput, but the absolute row
  remained below sonic+1.
- No `RESULTS.md` or `ROLLING-SOTA-DELTA.md` update was made. The routed
  remainder is a stronger object-heavy direct material differential: generated
  per-shape object member handling, sink stack specialization, or another
  CHALLENGE-accepted route that changes the object-value cost center rather
  than only removing the scalar wrapper.

## SK-V13 Wave 11.3 Direct Sink Stack Specialization

- Item 143 closes W11.3 under `G-W11.3-JSON-DIRECT-SINK-STACK` as
  `PASS-ADMIT`. W11.3 reopens `json/mesh/direct_to_struct/main` from the
  REDRESS 119/120 N-direct fixpoint and the REDRESS 142 object-loop rejection.
  The landed material differential removes closure-mediated parent callbacks
  inside the sink-only Track 1 digest stack: scalar object folds now borrow the
  object parent directly, and scalar array folds borrow the array parent
  directly before incrementing the element counter. Runtime parsing, JSON
  codegen, SIMD primitives, generic-crate behavior, and the independent Track 2
  oracle are unchanged.
- The measured row admits over same-run sonic strict. The retained W11.3
  Criterion artifact records `mesh/direct_to_struct` Track 1 `9657.892` Mbps,
  Track 2 `6959.985` Mbps, sonic strict `9569.599` Mbps, serde `7011.870`
  Mbps, threshold `9570.599` Mbps, and lower-confidence Track 1 `9623.984`
  Mbps. The refreshed campaign table records the admitted row as Track 1
  `9631`, Track 2 `7828`, sonic strict `9581`, serde `7033` Mbps in
  `RESULTS.md`, with `ROLLING-SOTA-DELTA.md` updated to mark the row admitted.
- The same Criterion run did not admit the other W11.3 primary probes:
  `canada/direct_to_struct` Track 1 `10602.676` Mbps vs sonic strict
  `12155.126` Mbps; `random/direct_to_struct` Track 1 `7891.740` Mbps vs
  sonic strict `8802.458` Mbps; `instruments/direct_to_struct` Track 1
  `12179.139` Mbps vs sonic strict `12787.011` Mbps. Those rows remain routed
  to later JSON direct reopen waves.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w11.3/skv13-W11.3-json-direct-reopen.json`.
  The retained measurement artifact is
  `restart/skinny/tranches/sk-v13/research/w11.3/direct-sink-stack-facts.json`
  with SHA-256
  `5ed0b8d300b212c5c385e78a2dd177b0a71d958a74a883fa46f559eec41e94fa`.
- Verification passed:
  `cargo test -p bbnf-bench direct_struct::tests -- --nocapture`,
  `cargo test -p bbnf-bench w11 -- --nocapture`,
  `cargo test -p bbnf-bench skv13_json_direct_reopen_report -- --nocapture`,
  `cargo test -p bbnf-bench direct_contract -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/(mesh|instruments|random|canada)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/twitter/track1_generated'`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`,
  and the W11.3 companion gate:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-json-direct-reopen-report
  ../restart/skinny/tranches/sk-v13/research/w11.3/skv13-W11.3-json-direct-reopen.json`.

## SK-V13 Wave 12 SIMD/ASM Production Wiring

- Item 144 closes W12 under `G-W12-SIMD-ASM-PRODUCTION` as `PASS-ADMIT`.
  The material differential from REDRESS 88/89/90/122/126 is production
  consumption: REDRESS 126 demoted the aarch64 primitive inventory with
  evidence but left the `a64_ascii_set_run_skip` split unwired. W12 lands
  `bbnf_simd::find_ascii_set_member64` as the scalar-reference-backed caller
  of `byte_class_from_eq_set_64` and consumes it in the generated CSS L4
  declaration-values `Scanner::scan_block` delimiter search in both runtime
  and codegen template output.
- Caller checkasm and primitive checkasm passed. The caller microbench recorded
  scalar `17.663406` ns, candidate `3.891609` ns, ratio `4.538843`, decision
  `pass`; `xtask primitive-checkasm` now includes
  `checkasm_ascii_set_member_find_64`.
- Production measurement moved the retained CSS row. Criterion for
  `nonjson_css_l4/track1_generated_css_l4_decl_values` reported slope
  `3367.7911246639546` ns, Track 1 `444.208` Mbps, versus the retained
  row's prior Track 1 `434.1316520595916` Mbps and lightningcss threshold
  `169.23458062242955` Mbps. Criterion reported `+109.87%` throughput against
  its saved baseline for the production lane, and strict equality versus
  cssparser and lightningcss remained green.
- The aarch64 orphan audit classifies every file under
  `crates/bbnf-simd/src/aarch64`; final `orphan_count_after = 0`. Historical
  checkasm-only/test-only bodies remain `inventory_demoted_with_evidence`
  under REDRESS 126 rather than retained production orphans.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w12/skv13-W12-simd-asm-production.json`.
  Retained artifacts:
  `restart/skinny/tranches/sk-v13/research/w12/simd-production-facts.json`
  SHA-256 `6b314ca6bb6e915f66d2e0e8013d08b132717fb6a0c3ad5bddcd1418f69869e2`,
  and `restart/skinny/tranches/sk-v13/research/w12/orphan-inventory.json`
  SHA-256 `0189f98d8e832de133bfbfc543028e1c7c198e21fdc8113487cefa91e9b54562`.
- Verification passed:
  `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_ascii_set_member_find_64 -- --nocapture`,
  `BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo run -p xtask --release -- primitive-checkasm`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench lightningcss_sidecar_matches_generated_track1_and_cssparser -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench writes_gate_consumed_css_l4_report -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4 -- nonjson_css_l4/track1_generated_css_l4_decl_values`,
  and the W12 companion gate:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-simd-asm-production-report
  ../restart/skinny/tranches/sk-v13/research/w12/skv13-W12-simd-asm-production.json`.

## SK-V13 Wave 13.1 Typed Product Numbers Surface

- Item 145 closes W13.1 under `G-W13.1-TYPED-NUMBERS` as `PASS-ADMIT`.
  The material differential from REDRESS 70-72/103-110 is a real generated
  typed product surface for the `numbers` corpus, not a direct digest,
  hidden sink, proof-only fixture, or generated no-op. W13.1 adds a
  `Vec<f64>` root schema, generates `parse_numbers`, routes the
  `real_typed_struct` Track 1/Track 2/serde/sonic lanes through the normal
  fixture machinery, and consumes the admission through the companion
  `gate-json` report.
- The measured typed row admits over same-run sonic strict. The companion
  facts record Track 1 mean `13070.204` Mbps, Track 2 oracle `9717.621`
  Mbps, sonic strict `11867.518` Mbps, serde `9744.091` Mbps, threshold
  `11868.518` Mbps, lower-confidence Track 1 `13031.183` Mbps, and margin
  `1201.686` Mbps over threshold. The refreshed campaign table records the
  admitted row as Track 1 `13157`, Track 2 oracle `9706`, sonic strict
  `11870`, serde `9749` Mbps in `RESULTS.md`, with
  `ROLLING-SOTA-DELTA.md` marking `json/numbers/real_typed_struct/main`
  admitted.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w13.1/skv13-W13.1-typed-product.json`.
  The retained measurement artifact is
  `restart/skinny/tranches/sk-v13/research/w13.1/numbers-typed-facts.json`
  with SHA-256
  `fb5512723d73a6c7b225cba10f0c271b80a4c917944df461e765d0e94e43bd77`.
- Verification passed:
  `cargo xtask regen-real-typed`,
  `cargo xtask check-real-typed`,
  `cargo test -p bbnf-bench numbers_typed -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w13_numbers -- --nocapture`,
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_1_parent_diff_under_w13_1_scope -- --nocapture`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv13_typed_product_report_flag -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/numbers/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`,
  and the W13.1 companion gate:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-typed-product-report
  ../restart/skinny/tranches/sk-v13/research/w13.1/skv13-W13.1-typed-product.json`.

## SK-V13 Wave 13.2 Typed Product Unicode Basic Surface

- Item 146 closes W13.2 under `G-W13.2-TYPED-UNICODE-BASIC` as
  `PASS-ADMIT`. The material differential from REDRESS 70-72/103-110 is
  a real generated typed product surface for the `unicode_basic` corpus,
  not a direct digest, hidden sink, proof-only fixture, or generated no-op.
  W13.2 adds a generated `Vec<UnicodeBasicRecord<'i>>` root schema, routes
  `real_typed_struct` Track 1 through
  `generated_real_typed::parse_unicode_basic`, and consumes the row through
  the companion `gate-json` report.
- The measured typed row admits over same-run sonic strict. The companion
  facts record Track 1 mean `6571.726` Mbps, Track 2 oracle `4250.935`
  Mbps, sonic strict `6002.882` Mbps, serde `4271.309` Mbps, threshold
  `6003.882` Mbps, lower-confidence Track 1 `6560.371` Mbps, and margin
  `567.843` Mbps over threshold. The refreshed campaign table records the
  admitted row as Track 1 `6584`, Track 2 oracle `4268`, sonic strict
  `6032`, serde `4274` Mbps in `RESULTS.md`, with
  `ROLLING-SOTA-DELTA.md` marking
  `json/unicode_basic/real_typed_struct/main` admitted.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w13.2/skv13-W13.2-typed-product.json`.
  The retained measurement artifact is
  `restart/skinny/tranches/sk-v13/research/w13.2/unicode-basic-typed-facts.json`
  with SHA-256
  `e8ef051be1d0d6bc0a6fbb28ff965ee64cbc36b04a1fc75942124ba81b3fd7d9`.
- Verification passed:
  `cargo xtask regen-real-typed`,
  `cargo xtask check-real-typed`,
  `cargo test -p bbnf-bench unicode_basic_typed -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w13_unicode_basic -- --nocapture`,
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_2_parent_diff_under_w13_2_scope -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/unicode_basic/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`,
  and the W13.2 companion gate:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-typed-product-report
  ../restart/skinny/tranches/sk-v13/research/w13.2/skv13-W13.2-typed-product.json`.

## SK-V13 Wave 13.3 Typed Product Random Surface

- Item 147 closes W13.3 under `G-W13.3-TYPED-RANDOM` as `PASS-ADMIT`.
  The material differential from REDRESS 70-72/103-110 is a real generated
  typed product surface for the `random` corpus, not a direct digest,
  hidden sink, proof-only fixture, or generated no-op. W13.3 adds generated
  `RandomDocument`, `RandomUser`, and `RandomFriend` product roots, routes
  `real_typed_struct` Track 1 through `generated_real_typed::parse_random`,
  and consumes the row through the companion `gate-json` report.
- The measured typed row admits over same-run sonic strict. The companion
  facts record Track 1 mean `8568.644` Mbps, Track 2 oracle `5347.998`
  Mbps, sonic strict `7263.180` Mbps, serde `5347.005` Mbps, threshold
  `7264.180` Mbps, lower-confidence Track 1 `8556.729` Mbps, and margin
  `1304.464` Mbps over threshold. The refreshed campaign table records the
  admitted row as Track 1 `8559`, Track 2 oracle `5346`, sonic strict
  `7260`, serde `5347` Mbps in `RESULTS.md`, with
  `ROLLING-SOTA-DELTA.md` marking
  `json/random/real_typed_struct/main` admitted.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w13.3/skv13-W13.3-typed-product.json`.
  The retained measurement artifact is
  `restart/skinny/tranches/sk-v13/research/w13.3/random-typed-facts.json`
  with SHA-256
  `a9b1d495b5faf3896a300508725ad2fd0665a5b611aa9e844911bf6347648c27`.
- Verification passed:
  `cargo xtask regen-real-typed`,
  `cargo xtask check-real-typed`,
  `cargo test -p bbnf-bench random_typed -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w13_random -- --nocapture`,
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_3_parent_diff_under_w13_3_scope -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/random/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`,
  and the W13.3 companion gate:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-typed-product-report
  ../restart/skinny/tranches/sk-v13/research/w13.3/skv13-W13.3-typed-product.json`.

## SK-V13 Wave 13.4 Typed Product Instruments Surface

- Item 148 closes W13.4 under `G-W13.4-TYPED-INSTRUMENTS` as
  `PASS-ADMIT`. The material differential from REDRESS 70-72/103-110 is
  a real generated typed product surface for the `instruments` corpus, not
  a direct digest, hidden sink, proof-only fixture, or generated no-op.
  W13.4 adds generated `InstrumentsDocument`, `Instrument`,
  `InstrumentEnvelope`, `InstrumentPattern`, and `InstrumentSample` product
  roots, routes `real_typed_struct` Track 1 through
  `generated_real_typed::parse_instruments`, and consumes the row through
  the companion `gate-json` report.
- The measured typed row admits over same-run sonic strict. The companion
  facts record Track 1 mean `20968.426` Mbps, Track 2 oracle
  `12094.945` Mbps, sonic strict `15977.585` Mbps, serde `12107.227`
  Mbps, threshold `15978.585` Mbps, lower-confidence Track 1
  `20921.829` Mbps, and margin `4989.840` Mbps over threshold. The
  refreshed campaign table records the admitted row as Track 1 `21026`,
  Track 2 oracle `12087`, sonic strict `15995`, serde `12106` Mbps in
  `RESULTS.md`, with `ROLLING-SOTA-DELTA.md` marking
  `json/instruments/real_typed_struct/main` admitted.
- Gate evidence is consumed by
  `restart/skinny/tranches/sk-v13/research/w13.4/skv13-W13.4-typed-product.json`.
  The retained measurement artifact is
  `restart/skinny/tranches/sk-v13/research/w13.4/instruments-typed-facts.json`
  with SHA-256
  `a7dc75ec5d0b04aa7786bc4e38e34e067769c06dbbff56dcdb9f5d60a81fcf5d`.
- Verification passed:
  `cargo xtask regen-real-typed`,
  `cargo xtask check-real-typed`,
  `cargo test -p bbnf-bench instruments_typed -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w13_instruments -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture`,
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_4_parent_diff_under_w13_4_scope -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/instruments/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'`,
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`,
  and the W13.4 companion gate:
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory --skv13-typed-product-report
  ../restart/skinny/tranches/sk-v13/research/w13.4/skv13-W13.4-typed-product.json`.

## SK-V13 Wave 13.5 Typed Product GSOC Surface

- Item 149 closes W13.5 under `G-W13.5-TYPED-GSOC-2018` as
  `MEASURED-REJECT`. The material differential from REDRESS 70/103/105/110
  was real: the rejected patch added a generated map-entry typed product root
  for all 1,264 `gsoc-2018` proposal records, including nested sponsor and
  author objects, and routed it through the `real_typed_struct` Track 1
  consumer. It was not a direct digest, hidden sink, proof-only fixture, root
  key collector, or no-op wrapper.
- Correctness passed before measurement:
  `cargo xtask regen-real-typed && cargo xtask check-real-typed`, and
  `cargo test -p bbnf-bench gsoc_2018_typed -- --nocapture`. Native
  Criterion then measured Track 1 generated typed `6053.150` Mbps, Track 2
  serde oracle `6357.845` Mbps, sonic strict typed `6817.382` Mbps, and
  serde_json typed `6413.127` Mbps. The pinned threshold was
  `sonic + 1 = 6818.382` Mbps, so Track 1 missed by `765.232` Mbps.
- The rejected implementation patch is saved at
  `/tmp/skv13-waveW13.5-rejected.patch`. The retained redress note is
  `restart/skinny/tranches/sk-v13/research/w13.5/redress.md`.
- `json/gsoc-2018/real_typed_struct/main` remains `MISSING`. A second
  in-tranche GSOC reopen triggers the round-trip rule unless it names a fresh
  material differential, such as schema specialization that avoids generic
  map-entry string matching or a row-specific string-copy deletion.

## SK-V13 Wave 13.6 Typed Product Unicode Mixed Surface

- Item 150 closes W13.6 under `G-W13.6-TYPED-UNICODE-MIXED` as
  `MEASURED-REJECT`. The material differential from REDRESS 70-72/103-110
  was real: the rejected patch added a generated `UnicodeMixed` typed product
  root covering metadata and all 4,185 records, including `id`, `type`,
  `value`, and `n`, and routed it through the `real_typed_struct` Track 1
  consumer. It was not a direct digest, parse-only row, unicode codec proof,
  hidden sink, or partial string-only surface.
- Correctness passed before measurement:
  `cargo xtask regen-real-typed`, `cargo xtask check-real-typed`,
  `cargo test -p bbnf-bench unicode_mixed_typed -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w13_unicode_mixed -- --nocapture`,
  and
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_6_parent_diff_under_w13_6_scope -- --nocapture`.
  Native Criterion then measured Track 1 generated typed `413.518` Mbps,
  Track 2 serde oracle `441.723` Mbps, sonic strict typed `703.581` Mbps,
  and serde_json typed `439.933` Mbps. The pinned threshold was
  `sonic + 1 = 704.581` Mbps, so Track 1 missed by `291.063` Mbps.
- The rejected implementation patch is saved at
  `/tmp/skv13-waveW13.6-rejected.patch`. The retained measurement facts are
  `restart/skinny/tranches/sk-v13/research/w13.6/unicode-mixed-typed-facts.json`;
  the retained redress note is
  `restart/skinny/tranches/sk-v13/research/w13.6/redress.md`.
- `json/unicode_mixed/real_typed_struct/main` remains `MISSING`. A second
  in-tranche reopen triggers the round-trip rule unless it names a fresh
  material differential, such as decode-allocation deletion, row-specific
  string borrowing policy, or SIMD unicode string decode consumption.

## SK-V13 Wave 13.7 Typed Product Y String Unicode Surface

- Item 151 closes W13.7 under `G-W13.7-TYPED-Y-STRING-UNICODE` as
  `MEASURED-REJECT`. The material differential from REDRESS 70-72/103-110 and
  REDRESS 150 was real: the rejected patch added a generated
  `y_string_unicode` typed product root as `Vec<Cow<'input, str>>`, routed it
  through the `real_typed_struct` Track 1 consumer, and compared it against
  Track 2, sonic-rs strict, and serde_json typed outputs. It was not a direct
  digest, parse-only row, unicode codec proof, hidden sink, or partial fixture.
- Correctness passed before measurement:
  `cargo xtask regen-real-typed`, `cargo xtask check-real-typed`,
  `cargo test -p bbnf-bench y_string_unicode_typed -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w13_y_string_unicode -- --nocapture`,
  and
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_7_parent_diff_under_w13_7_scope -- --nocapture`.
  Native Criterion then measured Track 1 generated typed `639.759` Mbps,
  Track 2 serde oracle `718.903` Mbps, sonic strict typed `823.749` Mbps,
  and serde_json typed `720.387` Mbps. The pinned threshold was
  `sonic + 1 = 824.749` Mbps, so Track 1 missed by `184.990` Mbps.
- The rejected implementation patch is saved at
  `/tmp/skv13-waveW13.7-rejected.patch`. The retained measurement facts are
  `restart/skinny/tranches/sk-v13/research/w13.7/y-string-unicode-typed-facts.json`;
  the retained redress note is
  `restart/skinny/tranches/sk-v13/research/w13.7/redress.md`.
- `json/y_string_unicode/real_typed_struct/main` remains `MISSING`. A second
  in-tranche reopen triggers the round-trip rule unless it names a fresh
  material differential, such as row-specific escape decode deletion, SIMD
  unicode decode consumption, or a typed product shape that avoids per-string
  allocation pressure without weakening strict equality.

## SK-V13 Wave 13.8 Typed Product Unicode Escapes Surface

- Item 152 closes W13.8 under `G-W13.8-TYPED-UNICODE-ESCAPES` as
  `MEASURED-REJECT`. The material differential from REDRESS 70-72/103-110 and
  REDRESS 150/151 was real: the rejected patch added a generated
  `unicode_escapes` typed product root covering corpus metadata and all escaped
  records, including `id` and `v`, and routed it through the
  `real_typed_struct` Track 1 consumer. It was not a direct digest,
  parse-only row, unicode codec proof, hidden sink, or partial fixture.
- Correctness passed before measurement:
  `cargo xtask regen-real-typed`, `cargo xtask check-real-typed`,
  `cargo test -p bbnf-bench unicode_escapes_typed -- --nocapture`,
  `cargo test -p bbnf-bench --bin gate w13_unicode_escapes -- --nocapture`,
  and
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_8_parent_diff_under_w13_8_scope -- --nocapture`.
  Native Criterion then measured Track 1 generated typed `511.121` Mbps,
  Track 2 serde oracle `512.133` Mbps, sonic strict typed `997.986` Mbps,
  and serde_json typed `512.945` Mbps. The pinned threshold was
  `sonic + 1 = 998.986` Mbps, so Track 1 missed by `487.865` Mbps.
- The rejected implementation patch is saved at
  `/tmp/skv13-waveW13.8-rejected.patch`. The retained measurement facts are
  `restart/skinny/tranches/sk-v13/research/w13.8/unicode-escapes-typed-facts.json`;
  the retained redress note is
  `restart/skinny/tranches/sk-v13/research/w13.8/redress.md`.
- `json/unicode_escapes/real_typed_struct/main` remains `MISSING`. A second
  in-tranche reopen triggers the round-trip rule unless it names a fresh
  material differential, such as SIMD unicode escape decode consumption,
  row-specific escape-allocation deletion, or a typed product shape that avoids
  per-string decode overhead without weakening strict equality.

## SK-V13 Wave 13.9 Typed Product Canada Surface

- Item 153 closes W13.9 under `G-W13.9-TYPED-CANADA` as
  `CORRECTNESS-REJECT`. The material differential from REDRESS 80 and
  REDRESS 119/120 was real: the rejected patch added a generated Canada
  GeoJSON typed product root covering top-level type, feature type,
  `properties.name`, geometry type, and all 111,126 coordinate numbers in
  source order. It was not a direct digest, count-only coordinate checksum,
  parse-only row, or f64 mantissa replay.
- Schema and local gate checks passed before full-corpus parity:
  `cargo xtask regen-real-typed`, `cargo xtask check-real-typed`,
  `cargo test -p bbnf-bench --bin gate w13_canada -- --nocapture`, and
  `cargo test -p bbnf-bench lock14_baseline::tests::admits_sk_v13_w13_9_parent_diff_under_w13_9_scope -- --nocapture`.
  The full Canada typed fixture failed strict equality under
  `cargo test -p bbnf-bench canada_typed -- --nocapture`: Track 1 checksum
  `7760849640330549600` differed from Track 2 checksum
  `17574774450138172291`.
- The first isolated mismatch was a one-ULP f64 rounding difference in ring
  `0`, point `4`, coordinate `1`: Track 1 materialized
  `43.47470900000013` (`0x4045bcc343b70f08`), while serde/sonic materialized
  `43.474709000000125` (`0x4045bcc343b70f07`). Native Criterion was not run
  because the parity precondition failed.
- The rejected implementation patch is saved at
  `/tmp/skv13-waveW13.9-rejected.patch`. The retained correctness facts are
  `restart/skinny/tranches/sk-v13/research/w13.9/canada-typed-facts.json`;
  the retained redress note is
  `restart/skinny/tranches/sk-v13/research/w13.9/redress.md`.
- `json/canada/real_typed_struct/main` remains `MISSING`. A second in-tranche
  reopen triggers the round-trip rule unless it names a fresh material
  differential, such as exact f64 materialization for generated typed products
  or a coordinate-specific product shape that preserves serde/sonic f64 bits
  without weakening strict equality.

## SK-V13 Wave 14.1 Numbers Parse-Only Admission

- Item 154 closes W14.1 under `G-W14.1-JSON-PARSE-NUMBERS` as `ADMIT`.
  The material differential from REDRESS 102 and the pre-pin parse-only
  firewall is narrow: W14.1 does not change parser runtime, union substrate,
  or SIMD code. It supplies the missing gate-consumed strict DOM
  output-plane contract for `json/numbers/parse_only/main`, with independent
  Track 2 evidence and measured UTF-8 / escape completeness.
- Native full-capture Criterion was rerun with
  `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity`.
  The W14.1 companion facts measured Track 1 mean `19102.844` Mbps, Track 1
  lower confidence `19002.696` Mbps, Track 2 `19289.254` Mbps, sonic strict
  `13610.385` Mbps, threshold `13611.385` Mbps, and mean margin
  `5491.459` Mbps. The gate-generated RESULTS slope row records Track 1
  `19267` Mbps, Track 2 `19126` Mbps, sonic strict `13666` Mbps, and rolling
  margin `5600.00` Mbps.
- The source change adds `sk-v13-json-parse-only-v1`, companion
  `gate-json` / `xtask` plumbing, generated RESULTS support for the admitted
  row, rolling delta status discipline, and a metadata validator correction:
  stale non-required `real_typed_struct` Criterion directories are ignored for
  fixtures that do not have a real typed product surface.
- Verification passed:
  `cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_numbers_admit -- --nocapture`,
  `cargo test -p bbnf-bench skv13_json_parse_only_report_arg_allows_json_check_only -- --nocapture`,
  `cargo test -p bbnf-bench w14_1_numbers_parse_only_reopens_only_sonic_plus_one_numbers -- --nocapture`,
  `cargo test -p bbnf-bench admits_sk_v13_w14_1_parent_diff_under_w14_1_scope -- --nocapture`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv13_json_parse_only_report_flag -- --nocapture`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.1/skv13-W14.1-json-parse-only.json`.
- The retained measurement facts are
  `restart/skinny/tranches/sk-v13/research/w14.1/numbers-parse-facts.json`
  with SHA-256
  `a4c6afeb13a342691fd1639f5a15bcecd274f2e8f7e8e2a7a410d653735fce50`;
  the retained redress note is
  `restart/skinny/tranches/sk-v13/research/w14.1/redress.md`.

## SK-V13 Wave 14.2 CITM Catalog Parse-Only Admission

- Item 155 closes W14.2 under `G-W14.2-JSON-PARSE-CITM-CATALOG` as
  `ADMIT`. The material differential from REDRESS 102 and W14.1 is narrow:
  W14.2 does not alter parser runtime, generated JSON parser bodies, union
  substrate, or SIMD code. It generalizes the W14 parse-only admission
  firewall to a configured row table and supplies gate-consumed strict DOM
  evidence for `json/citm_catalog/parse_only/main`.
- Native Criterion was refreshed for
  `json/citm_catalog/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)`
  with `RUSTFLAGS="-C target-cpu=native"`. The companion facts measured Track
  1 mean `30196.751` Mbps, Track 1 lower confidence `30134.073` Mbps, Track 2
  oracle `20598.200` Mbps, sonic strict `25567.165` Mbps, threshold
  `25568.165` Mbps, and mean margin `4628.586` Mbps. The gate-generated
  RESULTS slope row records Track 1 `30150` Mbps, Track 2 `20574` Mbps, sonic
  strict `25565` Mbps, and rolling margin `4584.00` Mbps.
- `gate-json --update-results` initially rejected stale
  `simd_structural_scan/*_simd/metadata.toml` capture identity after the
  W14.2 Criterion lanes were refreshed. The SIMD scan bench was rerun only to
  refresh required gate metadata; W14.2 does not claim a SIMD admission.
- Verification passed:
  `cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture`,
  `cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture`,
  `cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture`,
  `cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.2/skv13-W14.2-json-parse-only.json`.
- The retained measurement facts are
  `restart/skinny/tranches/sk-v13/research/w14.2/citm-catalog-parse-facts.json`
  with SHA-256
  `47f0e3c552f8ac3dc7f408549ef8a1416bb1fd780b1f86725193ff1550aff44f`;
  the retained redress note is
  `restart/skinny/tranches/sk-v13/research/w14.2/redress.md`.
- `canada`, `marine_ik`, and `mesh` remain positive-margin OPEN parse-only
  rows until they receive their own W14.N packet and REDRESS citation.

## SK-V13 Wave 14.3 Canada Parse-Only Admission

- Item 156 closes W14.3 under `G-W14.3-JSON-PARSE-CANADA` as `ADMIT`.
  The material differential from REDRESS 102 and W14.1/W14.2 is row-local:
  W14.3 does not alter parser runtime, generated JSON parser bodies, union
  substrate, or SIMD code. It adds a single W14 parse-only admission spec for
  `json/canada/parse_only/main` and supplies gate-consumed strict DOM evidence
  for that row.
- Native Criterion was refreshed for
  `json/canada/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)`
  with `RUSTFLAGS="-C target-cpu=native"`. The companion facts measured Track
  1 mean `17052.470` Mbps, Track 1 lower confidence `17006.626` Mbps, Track 2
  oracle `17101.405` Mbps, sonic strict `14078.954` Mbps, threshold
  `14079.954` Mbps, and mean margin `2972.516` Mbps. The gate-generated
  RESULTS slope row records Track 1 `16977` Mbps, Track 2 `17119` Mbps, sonic
  strict `14101` Mbps, and rolling margin `2875.00` Mbps.
- `gate-json --update-results` initially rejected stale
  `simd_structural_scan/*_simd/metadata.toml` capture identity after the
  W14.3 Criterion lanes were refreshed. The SIMD scan bench was rerun only to
  refresh required gate metadata; W14.3 does not claim a SIMD admission.
- Verification passed:
  `cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture`,
  `cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture`,
  `cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture`,
  `cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.3/skv13-W14.3-json-parse-only.json`.
- The retained measurement facts are
  `restart/skinny/tranches/sk-v13/research/w14.3/canada-parse-facts.json`
  with SHA-256
  `f1a34375b0126fbb3c2d9fe273f3f32fb8d4d9d8c169b43b73940fe8b8a41df2`;
  the retained redress note is
  `restart/skinny/tranches/sk-v13/research/w14.3/redress.md`.
- `marine_ik` and `mesh` remain positive-margin OPEN parse-only rows until
  they receive their own W14.N packet and REDRESS citation.

## SK-V13 Wave 14.4 Marine IK Parse-Only Admission

- Item 157 closes W14.4 under `G-W14.4-JSON-PARSE-MARINE-IK` as `ADMIT`.
  The material differential from REDRESS 102 and W14.1-W14.3 is row-local:
  W14.4 does not alter parser runtime, generated JSON parser bodies, union
  substrate, SIMD primitives, output digests, or decision-engine policy. It
  adds a single W14 parse-only admission spec for
  `json/marine_ik/parse_only/main` and supplies gate-consumed strict DOM
  evidence for that row.
- Native Criterion was refreshed for
  `json/marine_ik/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)`
  with `RUSTFLAGS="-C target-cpu=native"`. The companion facts measured Track
  1 mean `12272.307` Mbps, Track 1 lower confidence `12196.346` Mbps, Track 2
  oracle `12311.606` Mbps, sonic strict `9901.936` Mbps, threshold
  `9902.936` Mbps, and mean margin `2369.371` Mbps. The gate-generated
  RESULTS slope row records Track 1 `12357` Mbps, Track 2 `12302` Mbps, sonic
  strict `9902` Mbps, and rolling margin `2454.00` Mbps.
- `gate-json --update-results` initially rejected stale
  `simd_structural_scan/*_simd/metadata.toml` capture identity after the
  Marine IK Criterion lanes were refreshed. The SIMD scan bench was rerun only
  to refresh required gate metadata; W14.4 does not claim a SIMD admission.
  The `json_parity` regex also refreshed adjacent Marine IK serde
  direct/typed comparator lanes; W14.4 claims only the parse-only row.
- Verification passed:
  `cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture`,
  `cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture`,
  `cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture`,
  `cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv13_json_parse_only_report_flag -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.4/skv13-W14.4-json-parse-only.json`.
  Compatibility checks for W14.1, W14.2, and W14.3 parse-only reports also
  passed after the rolling-delta refresh.
- The retained measurement facts are
  `restart/skinny/tranches/sk-v13/research/w14.4/marine-ik-parse-facts.json`
  with SHA-256
  `01a4ce96289ab0efadb88c55d3817f84da05dfe1cde8fe0b2f908475dcf9f1bc`;
  the retained redress note is
  `restart/skinny/tranches/sk-v13/research/w14.4/redress.md`.
- `mesh` remains the only positive-margin OPEN parse-only row in the W14
  table-admission pattern. After mesh receives its own W14.N packet, remaining
  JSON rows need implementation work rather than status/report-only admission.

## SK-V13 Wave 14.5 Mesh Parse-Only Admission

- Item 158 closes W14.5 under `G-W14.5-JSON-PARSE-MESH` as `ADMIT`.
  The material differential from REDRESS 102 and W14.1-W14.4 is row-local:
  W14.5 does not alter parser runtime, generated JSON parser bodies, union
  substrate, SIMD primitives, output digests, or decision-engine policy. It
  adds a single W14 parse-only admission spec for `json/mesh/parse_only/main`
  and supplies gate-consumed strict DOM evidence for that row.
- Native Criterion was refreshed for
  `json/mesh/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)`
  with `RUSTFLAGS="-C target-cpu=native"`. The companion facts measured Track
  1 mean `12897.188` Mbps, Track 1 lower confidence `12865.574` Mbps, Track 2
  oracle `11513.615` Mbps, sonic strict `11760.010` Mbps, threshold
  `11761.010` Mbps, and mean margin `1136.179` Mbps. The gate-generated
  RESULTS slope row records Track 1 `12987` Mbps, Track 2 `11522` Mbps, sonic
  strict `11758` Mbps, and rolling margin `1228.00` Mbps.
- `gate-json --update-results` initially rejected stale
  `simd_structural_scan/*_simd/metadata.toml` capture identity after the Mesh
  Criterion lanes were refreshed. The SIMD scan bench was rerun only to
  refresh required gate metadata; W14.5 does not claim a SIMD admission. The
  `json_parity` regex also refreshed adjacent Mesh serde direct/typed
  comparator lanes; W14.5 claims only the parse-only row.
- Verification passed:
  `cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture`,
  `cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture`,
  `cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture`,
  `cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture`,
  `cargo test -p xtask gate_json_passthrough_accepts_skv13_json_parse_only_report_flag -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.5/skv13-W14.5-json-parse-only.json`.
  Compatibility checks for W14.1, W14.2, W14.3, and W14.4 parse-only reports
  also passed after the rolling-delta refresh.
- The retained measurement facts are
  `restart/skinny/tranches/sk-v13/research/w14.5/mesh-parse-facts.json` with
  SHA-256
  `271d51a7e795e821982780f65020028980d53e67ee7212bada13ad0bb3345fd5`; the
  retained redress note is
  `restart/skinny/tranches/sk-v13/research/w14.5/redress.md`.
- W14.5 exhausts the positive-margin report-only parse admission pattern. No
  remaining JSON row should be closed by status/report plumbing alone. The next
  campaign wave must land a real implementation or a measured architectural
  block, with `json/instruments/direct_to_struct/main` and
  `json/update_center/real_typed_struct/main` the closest pinned-margin
  targets.

## SK-V13 Wave 11.4 Direct Cursor Byte-Fetch Reject

- Item 159 closes W11.4 under `G-W11.4-JSON-DIRECT-CURSOR-BYTE` as `REJECT`.
  The material differential from REDRESS 119/120/143 was generated direct
  parser byte-fetch specialization: replace the four hot
  `bytes.get(*cursor).copied()` direct-dispatch fetches with an explicit bounds
  check followed by `get_unchecked`. The rejected patch is saved at
  `/tmp/skv13-waveW11.4-rejected.patch` with SHA-256
  `7ce243dc25e321d8e370670c9939055db5627d962a51d91ce404a55abf550cd7` and has
  been reverted from source.
- Native Criterion was refreshed for
  `json/(instruments|mesh|random|canada|github_events)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)`
  with `RUSTFLAGS="-C target-cpu=native"`. Same-run Track 1 vs sonic strict
  measured: `instruments` `12025.558` vs `12721.724` Mbps (misses sonic+1 by
  `697.166`, time delta `+2.249%`), `mesh` `9665.378` vs `9744.222` Mbps
  (misses by `79.844`, `+2.380%`), `random` `7815.510` vs `8944.573` Mbps
  (misses by `1130.064`, `+1.697%`), `canada` `10983.047` vs `12201.442`
  Mbps (misses by `1219.395`, `-1.101%`), and `github_events` `12474.864` vs
  `16161.279` Mbps (misses by `3687.414`, `+0.148%`).
- Verification passed:
  `cargo test -p bbnf-bench direct_struct::tests -- --nocapture`,
  `cargo test -p bbnf-bench direct_contract -- --nocapture`, and
  `cargo test -p runtime json -- --nocapture`. `cargo test -p codegen json
  -- --nocapture` failed outside the W11.4 owner slice in
  `tests::json_config_policy_fields_are_consumed` because the test searches
  for the exact `config::STRING_NEEDS_DECODE` spelling while current generated
  code consumes the policy through `config::needs_decode_flags()` and
  `config::string_needs_decode`.
- W11.4 updates no `RESULTS.md` or rolling-delta row. The next JSON wave must
  use a materially different implementation route; the direct byte-fetch
  envelope is not the missing instruments/direct SOTA gap.

## SK-V13 Wave 15.1 UpdateCenter Typed Plugin Fast-Path Admission

- Item 160 closes W15.1 under
  `G-W15.1-JSON-TYPED-UPDATE-CENTER-PLUGIN` as `ADMIT`. The material
  differential from REDRESS 70-72, 103-110, 119, 120, 143, and 159 is a
  generated typed-product specialization, not a direct digest stand-in,
  hidden sink, proof-only root, or direct cursor byte-fetch patch. W15.1 emits
  an ordered `Plugin` parser for the observed 654-entry UpdateCenter plugin map
  and falls back to the generic typed parser on mismatch.
- Native Criterion was refreshed for
  `json/(update_center|twitter|github_events|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)`
  with `RUSTFLAGS="-C target-cpu=native"`. The retained facts measured
  UpdateCenter Track 1 mean `13264.676` Mbps, Track 1 lower confidence
  `13220.831` Mbps, Track 2 oracle `10407.236` Mbps, sonic strict
  `12598.123` Mbps, threshold `12599.123` Mbps, and lower-confidence margin
  `621.708` Mbps. The gate-generated RESULTS slope row records Track 1
  `13191` Mbps, Track 2 `10417` Mbps, sonic strict `12623` Mbps, and rolling
  margin `567.00` Mbps. `ROLLING-SOTA-DELTA.md` was regenerated from the same
  gate-consumed RESULTS snapshot; W15.1 claims only
  `json/update_center/real_typed_struct/main`, and direct-plane rows that remain
  `OPEN` are carried as current gate state rather than W15.1 row movement.
- Typed guard rows held against same-run sonic strict after a focused
  `github_events` confirmation rerun: `twitter` `17891.124` vs `15483.243`
  Mbps, `github_events` `13055.906` vs `12619.336`, `mesh` `9685.424` vs
  `8856.805`, and `marine_ik` `12143.344` vs `9198.260`. The SIMD scan bench
  was rerun only to refresh required gate metadata after an aggregate stale
  metadata rejection; W15.1 claims no SIMD admission.
- Verification passed:
  `cargo xtask regen-real-typed`,
  `cargo test -p bbnf-bench generated_update_center_typed_parser_matches_sidecars -- --nocapture`,
  `cargo test -p bbnf-bench w2_full_real_typed_fixtures_match_sidecars -- --nocapture`,
  `cargo test -p codegen emits_typed_direct_consumer_module -- --nocapture`,
  `cargo xtask check-real-typed`,
  `cargo test -p bbnf-bench real_typed_struct -- --nocapture`,
  `cargo test -p bbnf-bench admits_sk_v13_w15_1_parent_diff_under_w15_1_scope -- --nocapture`,
  `cargo test -p bbnf-bench w15_update_center_typed_admits_only_strict_sonic_plus_one_pass -- --nocapture`,
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory`, and
  `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-typed-product-report ../restart/skinny/tranches/sk-v13/research/w15.1/skv13-W15.1-typed-product.json`.
- Earlier in the wave, `cargo test -p codegen json -- --nocapture` failed
  outside the W15.1 owner slice in
  `tests::json_config_policy_fields_are_consumed`: the test searches for the
  exact `config::STRING_NEEDS_DECODE` spelling while current generated code
  consumes the policy through `config::needs_decode_flags()` and
  `config::string_needs_decode`.
- The retained measurement facts are
  `restart/skinny/tranches/sk-v13/research/w15.1/update-center-typed-facts.json`
  with SHA-256
  `2a652e0b8e3ec3608ca2bdd4c1bf539557f337653112c58f7076c3ee37147112`;
  the retained redress note is
  `restart/skinny/tranches/sk-v13/research/w15.1/redress.md`.

## SK-V14 Wave 1 PRUNE-1 JSON Comparator Equality Prune

- Item 161 closes json/numbers/parse_only/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v2-json-validation:§1-2 and sk-v13/v6-comparator-integrity:§1+§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the sonic_rs::Skipper parse-only comparator and timed per-iteration equality.
- Item 162 closes json/citm_catalog/parse_only/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v2-json-validation:§1-2 and sk-v13/v6-comparator-integrity:§1+§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the sonic_rs::Skipper parse-only comparator and timed per-iteration equality.
- Item 163 closes json/canada/parse_only/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v2-json-validation:§1-2 and sk-v13/v6-comparator-integrity:§1+§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the sonic_rs::Skipper parse-only comparator and timed per-iteration equality.
- Item 164 closes json/marine_ik/parse_only/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v2-json-validation:§1-2 and sk-v13/v6-comparator-integrity:§1+§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the sonic_rs::Skipper parse-only comparator and timed per-iteration equality.
- Item 165 closes json/mesh/parse_only/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v2-json-validation:§1-2 and sk-v13/v6-comparator-integrity:§1+§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the sonic_rs::Skipper parse-only comparator and timed per-iteration equality.
- Item 166 closes json/citm_catalog/direct_to_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict direct digest comparator and timed per-iteration equality.
- Item 167 closes json/apache_builds/direct_to_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict direct digest comparator and timed per-iteration equality.
- Item 168 closes json/marine_ik/direct_to_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict direct digest comparator and timed per-iteration equality.
- Item 169 closes json/instruments/direct_to_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict direct digest comparator and timed per-iteration equality.
- Item 170 closes json/numbers/direct_to_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict direct digest comparator and timed per-iteration equality.
- Item 171 closes json/unicode_basic/direct_to_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§3; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict direct digest comparator and timed per-iteration equality.
- Item 172 closes json/twitter/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 173 closes json/citm_catalog/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 174 closes json/apache_builds/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 175 closes json/github_events/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 176 closes json/update_center/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 177 closes json/mesh/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 178 closes json/random/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 179 closes json/marine_ik/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 180 closes json/instruments/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 181 closes json/numbers/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.
- Item 182 closes json/unicode_basic/real_typed_struct/main under `G-SK-V14-W1-PRUNE-1` as `PRUNE`.
  The prior admission is audit-falsified by sk-v13/v6-comparator-integrity:§1+§3 and sk-v13/v2-json-validation:§4; W1 keeps the cold measurements only as gate evidence after rebinding the row to the strict typed direct comparator and timed per-iteration equality.

## SK-V14 Wave 2 R4 Regen-CSS Round-Trip

- Item 183 closes `G-W2-FULL-ROUNDTRIP` under `G-SK-V14-W2-R4` as `REJECTED`.
  The skinny-side rostered `regen-css` command can be built for the seven existing CSS L4 runtime profiles, but the required destructive gate also deletes `crates/core/src/runtime/css_l4/`, and no current generator restores that Pattern H runtime tree. Root `cargo xtask regen --grammar css_l4` reproducibly emits only `crates/core/src/grammar/generated/css_l4.{rs,registry.json}`; `crates/core/src/runtime/css_l4/` remains seven hand-written runtime files routed to W6 PRUNE-4 by MIGRATION. The rejected source attempt is retained at `/tmp/skv14-waveW2-rejected-regen-css.patch`; the evidence packet is `restart/skinny/tranches/sk-v14/research/skv14-W2-redress.md`. W2 rejection blocks W3/W4/W5/W6/W7 and all new-admit waves by hard entry gates, so no later SK-V14 implementation wave is legally dispatchable from this state.

  Supersession note after Pass Omega V3/V4: REDRESS-183 remains the historical
  rejection for the original dual-tree W2 shape, but it no longer blocks
  dispatch. Pass Omega V3 W2R amended W2 to skinny-side `regen-css` only; W2
  admitted at `45568e669`, W3 production CSS corpus staging admitted at
  `b0a864f0b`, and `crates/core/src/runtime/css_l4/` remains W6.0 after W5.
  The live SK-V14 blocker became REDRESS-184 until Pass Omega V4 W4R amended
  W4 to ledger-only PRUNE and moved CSS provider/template deletion to W5.

## SK-V14 Wave 4 PRUNE-2 Provider-Deletion Cycle

- Item 184 closes `G-SK-V14-W4-PRUNE-2` as `REJECTED` under the current SPEC shape.
  W4 must delete the seven CSS L4 provider modules and template directories, then
  immediately run `cargo xtask regen-css` as the same-wave consumer. The live W2
  `regen-css` path still compiles through those provider modules via
  `skinny/xtask/src/regen.rs:18`, `skinny/crates/codegen/src/lib.rs:1-7`, and
  `skinny/crates/codegen/src/lib.rs:166-208`; the generic replacement belongs to
  W5, but W5 is gated by W4. A throwaway worktree deletion probe reproduced
  `error[E0583]` for all seven `css_l4_*_provider` modules before regeneration
  could run. The evidence packet is
  `restart/skinny/tranches/sk-v14/research/skv14-W4-redress.md`, and the
  corrective proposal is
  `restart/skinny/tranches/sk-v14/research/skv14-W4R-corrective-packet.md`.
  W5/W6/W7 remain blocked by the PRUNE chain, and W8/W9/W10 remain globally
  blocked until PRUNE-1 through PRUNE-5 close.

## SK-V14 Wave 4R CSS L4 Admit-Ledger PRUNE

- Item 185 closes `css_l4/declaration_values/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 186 closes `css_l4/declarations/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 187 closes `css_l4/css_variables/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 188 closes `css_l4/calc_expressions/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 189 closes `css_l4/var_url_functions/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 190 closes `css_l4/color_functions/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 191 closes `css_l4/stylesheet_root/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 192 closes `css_l4/selectors/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 193 closes `css_l4/pseudo_classes/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 194 closes `css_l4/pseudo_elements/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 195 closes `css_l4/attribute_selectors/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 196 closes `css_l4/gradients/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 197 closes `css_l4/transforms/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 198 closes `css_l4/filters/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 199 closes `css_l4/easing_functions/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 200 closes `css_l4/at_rules_keyframes/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 201 closes `css_l4/media_queries/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 202 closes `css_l4/vendor_prefixes/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 203 closes `css_l4/custom_at_rules/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 204 closes `css_l4/nested_rules/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`, including §1 C5's nested-rule regex shortcut; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 205 closes `css_l4/logical_properties/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`, including §1 C5's nested-rule regex shortcut; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 206 closes `css_l4/grid/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`, including §1 C5's nested-rule regex shortcut; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 207 closes `css_l4/flexbox/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`, including §1 C5's nested-rule regex shortcut; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.
- Item 208 closes `css_l4/typed_property_groups/direct_to_struct/main` under `G-SK-V14-W4-PRUNE-2` as `PRUNE`.
  The prior CSS L4 admission is audit-falsified by `sk-v13/v1-css-l4-validation:§1-6`, including §1 C5's nested-rule regex shortcut; W4 reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS source, generator, provider, template, runtime-twin, or `regen_css` deletion is performed in W4; deletion remains routed to W5 after replacement provider generation exists.

## SK-V14 Wave 5 PRUNE-3 Lock 14 Generator Capability Gap

- Item 209 closes `G-SK-V14-W5-PRUNE-3` as `REJECTED` under the current SPEC
  shape. W5 requires the eight per-grammar provider modules to collapse into one
  grammar-agnostic generator template consuming grammar source plus workspace
  metadata, then requires the seven CSS provider modules and seven CSS template
  directories to be deleted in the same replacement slice. Current HEAD still
  emits `regen-css` through `codegen::emit_runtime_profile(target.profile)` and
  the static provider/template mesh; the CSS source and metadata inputs are
  hashed for freshness only, not passed into codegen. The skinny grammar parser
  also rejects the CSS L4 source surface (`->` value projections and `@{...}`
  span capture). Static centralization would hide the same hand-written
  per-profile runtime bodies in one file, not satisfy Lock 14. The evidence
  packet is `restart/skinny/tranches/sk-v14/research/skv14-W5-redress.md`, and
  the corrective proposal is
  `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md`.
  W6/W7 remain blocked by the PRUNE chain, and W8/W9/W10 remain globally blocked
  until PRUNE-1 through PRUNE-5 close.
  Supersession note: Pass Omega V5 G-Omega closed on 2026-05-26 and amends the
  dispatch route only. REDRESS-209 remains the historical rejection of the
  original monolithic W5; current dispatch is W5A source-consuming generator
  capability followed by W5B provider/template deletion after W5A admits, per
  `restart/audit/totality/astral/V5/G-OMEGA-SIGNOFF.md`.

## SK-V14 Wave 5B PRUNE-3B Provider Deletion Gate Gap

- Item 210 closes `G-SK-V14-W5B-PRUNE-3B` as `REJECTED` under the current SPEC
  shape. W5A admitted the source-consuming request boundary at commit
  `286233fa2`, but the actual runtime bytes are still produced by
  `render_runtime_profile`, `RuntimeProvider`, and the CSS/JSON provider
  modules inside `skinny/crates/codegen/src/`. W5B's deletion exit gate requires
  the provider count to fall to zero before a provider-free generator body
  exists. Deleting those files now would either fail to compile or reintroduce
  REDRESS-209 as static centralization under a new file name. The SPEC's
  generic-crate grep is also ripgrep-inaccurate as written (`rg -E` is an
  encoding flag) and over-broad when aimed at root `crates/`. The evidence
  packet is `restart/skinny/tranches/sk-v14/research/skv14-W5B-plan.md`, and
  the corrective proposal is
  `restart/skinny/tranches/sk-v14/research/skv14-W5BR-corrective-packet.md`.
  W6/W7 remain blocked by the PRUNE chain, and W8/W9/W10 remain globally blocked
  until PRUNE-1 through PRUNE-5 close.
  Supersession note: Pass Omega V6 G-Omega closed on 2026-05-26 and amends the
  dispatch route only. REDRESS-210 remains the historical rejection of the V5
  W5B deletion gate; current dispatch is W5B-GEN provider-free runtime generator
  body followed by W5C-DELETE provider/template deletion and Lock 14 baseline
  close after W5B-GEN admits, per
  `restart/audit/totality/astral/V6/G-OMEGA-SIGNOFF.md`.

## SK-V14 Wave W5B-GEN PRUNE-3B Provider-Free Generator Body Gap

- Item 211 closes `G-SK-V14-W5B-GEN-PRUNE-3B` as `REJECTED` under the current
  SPEC shape. W5A admitted the source-consuming request boundary at commit
  `286233fa2`, but W5B-GEN requires a provider-free runtime generator body that
  emits CSS L4 and JSON runtime bytes from grammar source plus workspace
  metadata. Current HEAD still routes runtime emission through
  `render_runtime_profile`, `RuntimeProvider`, `json_provider`, and the CSS
  provider/template mesh. The W5A source-fact scanner does not compile CSS L4
  source into IR, and the current skinny parser accepts only `@import` and
  `@token`, not the CSS L4 constructs W5B-GEN must consume (`@ws`, `@pretty`,
  `?w`, `>>`, `<<`, span capture, typed host projections). Static
  centralization or reading committed generated output would be a workaround,
  not an admit. Evidence: the W5B-GEN research packet
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-{A..E}-*.md`, the
  folded plan `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md`,
  the corrective packet
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md`,
  and the clean V2/V3 challenge convergence under
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-GEN-challenge/`.
  The proof bundle is recorded in
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-redress.md`.
  Corrective route: Pass Omega V7 W5B-GENR must split the current gate into
  generic BBNF frontend/import/IR closure with CSS L4 as strict positive
  witness, provider-free generator body, then provider/template deletion.
  W5C-DELETE, W6, W7, and W8/W9/W10 remain blocked until the PRUNE chain is
  rerouted and closed.

## SK-V14 Wave W5B-FRONTEND PRUNE-3B Sub-Wave Authority Gap

- Item 212 closes `G-SK-V14-W5B-FRONTEND-PRUNE-3B` as `REJECTED` under the
  current SPEC shape. Pass Omega V7 correctly routed W5B-FRONTEND before
  W5C-GEN, but the active SPEC still grants W5B-FRONTEND one capped wave slot
  while the accepted target requires serial Lock 14 routing, import closure,
  layout/discard lowering, pretty/span/projection lowering, and request-consumer
  proof carry. The W5B-FRONTEND V2 plan tried to fit the dispatch-hard-cap
  discipline with internal sub-slices, but the V2 CH4/CH6 challenge found that
  informal slices need SPEC authority and that the maintain gate also conflicts
  with the current +/-1.0% full-table wording. Narrowing to one cap-valid slice
  would keep W5B-FRONTEND open and would not unblock W5C-GEN; forcing the whole
  closure through one wave would violate the hard-cap discipline. Evidence: the
  folded plan
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md`, the V2
  challenge archive
  `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/`,
  the corrective packet
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md`,
  and the proof bundle
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-redress.md`.
  Corrective route: Pass Omega V8 must formalize W5B-FRONTEND into capped
  sub-waves W5B.0 LOCK14-GATE, W5B.1 IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD,
  W5B.3 PRETTY-SPAN-PROJECTION, and W5B.4 REQUEST-CONSUMER, with W5C-GEN still
  blocked until aggregate W5B-FRONTEND close. W5D-DELETE, W6, W7, and W8/W9/W10
  remain blocked until the PRUNE chain is rerouted and closed.

## SK-V14 Wave W6.0 CSS L4 Root Runtime Projection Gap

- Item 213 closes `G-SK-V14-W6.0-CSS-L4-ROOT-RUNTIME-COLLAPSE` as `REJECTED`
  under the current SPEC shape. W6.0 requires `cargo xtask regen-css` to
  destructively restore `crates/core/src/runtime/css_l4/` from grammar/registry
  inputs, but the root workspace has no `regen-css` subcommand and root
  `cargo xtask regen --grammar css_l4 --output /tmp/w6-css-parser-proof`
  reproducibly emits only `css_l4.rs` and `css_l4.registry.json`. The emitted
  registry is structural; it does not carry the CSS domain projection data
  required to regenerate `CssColor` constructors, unit enums, arena-family
  placement, recursive color references, document focus/path traversal, or the
  `CssStructBuilder` rule-id frame routing. Static centralization of the
  current seven runtime files would reintroduce the fake-generated-template
  failure class, so no source workaround was landed. Evidence is recorded in
  `restart/skinny/tranches/sk-v14/research/skv14-W6.0-redress.md`; the narrow
  corrective packet is
  `restart/skinny/tranches/sk-v14/research/skv14-W6.0R-corrective-packet.md`.
  W6.1..W6.8, W7, and W8/W9/W10 remain blocked by the PRUNE-4 order until a
  real runtime projection source and emitter can make the W6.0 destructive
  gate pass.

## SK-V14 Wave W7 PRUNE-5 Policy/Union Runtime Wiring

- Item 214 closes `json/numbers/direct_to_struct/main` under
  `G-SK-V14-W7-PRUNE-5` as `PRUNE-CONSUMED`. W7 moves the W8/W9 policy and
  same-substrate union surfaces out of report-only evidence and into the
  compiler spine: `passes::compile` now emits per-rule `per_grammar_policy`
  and `same_substrate_union` facts, `codegen::lower::rust::lower_to_rust`
  fails closed when either is missing or stale, and generated JSON/CSS runtime
  outputs consume the resulting policy constants. Lock-1 triad:
  substrate_target=direct_sink; retention_lifetime=generated_function;
  policy_owner=generated_grammar. The named W7 hot-leaf handoff is
  pre=parse_value_at and post=parse_w11_1_number_array_direct for the
  generated direct numbers row. CSS L4 fact-stream generation consumes the
  second-family triad
  substrate_target=admitted_fact_output; retention_lifetime=output_row;
  policy_owner=generated_grammar without adding a sixth BackendShape or any
  UnionTape/class-lane retained substrate. The row remains AUDIT-FALSIFIED/NO-GO
  because W7 is a load-bearing policy/union prune wave, not a direct admission.

## SK-V14 Wave W8 CSS L4 Production Re-Admit

- Item 215 initially closed all 24 CSS L4 rows under
  `G-SK-V14-W8-CSS-L4-READMIT` as `REJECTED` for the first tranche attempt.
  The executable probe in `skinny/crates/bbnf-bench/src/css_l4_w8.rs` loaded
  the SK-V14 production CSS corpus (`979638` bytes), verified lightningcss
  full-parse over all four files, verified a cssparser stylesheet probe over
  all four files, and ran all seven post-W7 generated CSS runtime profiles
  over every corpus. The generated Track 1 outputs were fact streams in all 28
  profile/corpus runs: W7 policy and frontend metadata were present, but the
  output plane remained `*_fact_stream`, not a CSS full-parse equality plane.
  Initial evidence: `cargo test -p bbnf-bench css_l4_w8 -- --nocapture` and
  `restart/skinny/tranches/sk-v14/research/skv14-W8-redress.md`.

- W8R supersedes that rejection without erasing it. The generator now emits an
  additive `parser::parse_full` / `generated::emit_full_parse` route for every
  CSS L4 runtime profile, the W8 harness rejects fact-stream leakage, and
  `skinny/RESULTS.md` plus `restart/skinny/ROLLING-SOTA-DELTA.md` move all 24
  CSS L4 rows to `AUDIT-SUSTAINED` / `ADMITTED` on the
  `css_l4_full_parse` plane. Release-native retained evidence:
  `track1_mbps=2319.041`, `lightningcss_mbps=929.281`,
  `cssparser_mbps=2362.037`, `profiled_bytes=54859728`,
  `profile_iters=8`, and `margin_mbps=1388.760` from
  `cargo test --release -p bbnf-bench css_l4_w8 -- --nocapture` with
  `RUSTFLAGS="-C target-cpu=native"`. Retained files:
  `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-prototype.md`,
  `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.tsv`,
  and
  `restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-profile.raw.log`.

- SK-V15 W1 supersedes the W8R admission posture without deleting the retained
  evidence. The W8R tuple is diagnostic broadcast evidence only: zero CSS rows
  may close as live admits from the shared `SK-V14-W8R-css-full-parse-profile-cold-8`
  measurement, rolling delta keeps the numeric margin as `OPEN`, legacy
  sustained validators reject W8R CSS rows, and the W8 harness reports
  diagnostic disposition with `admitted_rows=0`. Typed CSS provider proof and
  same-workload retiming remain routed to SK-V15 W5/W6.
  Fact-stream adapters, tiny fixtures, and CANONICAL_FIXTURE/profile-template
  shortcuts remain rejected.

## SK-V14 Wave W9 JSON Direct + Typed Re-Admit

- Item 216 closes `G-SK-V14-W9-JSON-DIRECT-TYPED-READMIT` as `MIXED`. The W9
  executable probe selected all 17 JSON direct rows and all 17 JSON typed rows.
  Eleven typed rows admit from cold `profile_direct` evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W9-profile-direct.tsv`:
  `twitter`, `citm_catalog`, `apache_builds`, `github_events`,
  `update_center`, `mesh`, `random`, `marine_ik`, `instruments`, `numbers`,
  and `unicode_basic`. Each row has Track 1 typed product output, independent
  Track 2/serde typed output, sonic typed strict comparator evidence, and
  `Track 1 > sonic + 1.0 Mbps` in the cold run. All 17 direct rows remain open
  because their executable plane is still `direct_struct::{track1,track2,
  sonic}_digest`, not a per-corpus `<corpus>::strict_struct_deser` product.
  Six typed rows remain missing product surfaces: `canada`, `gsoc-2018`,
  `unicode_mixed`, `unicode_escapes`, `distinct_values`, and
  `y_string_unicode`. Evidence: `cargo test --profile ax-iter -p bbnf-bench
  json_w9 -- --nocapture`, `cargo test --profile ax-iter -p bbnf-bench
  direct_struct -- --nocapture`, `cargo test --profile ax-iter -p bbnf-bench
  real_typed_struct -- --nocapture`, and
  `restart/skinny/tranches/sk-v14/research/skv14-W9-redress.md`. Routed
  remainder: W10 may dispatch, but it must not cite W9 direct rows or missing
  typed rows as admitted evidence, and it still carries the unconditional
  Stage-0 F-V2-P1ABC-RERECORD obligation.

## SK-V14 Wave W10 JSON parse_only Distinct Path Re-Admit

- Item 217 closes `G-SK-V14-W10-JSON-PARSE-ONLY-READMIT` as `MIXED`. W10
  shipped the unconditional Stage-0 F-V2-P1ABC-RERECORD evidence before row
  admission: release build under native target CPU and retained interactive
  samply profile at
  `restart/skinny/tranches/sk-v14/research/skv14-W10-stage0-profile.json.gz`.
  The implementation adds a distinct generated JSON `parse_only` path that
  returns `Result<(), ParseError>` without building a full offset tape and
  wires `json_parity` Track 1 to `runtime::generated_json::parse_only`.
- Six parse_only rows admit from cold `profile_direct` evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W10-profile-direct.tsv`:
  `mesh`, `marine_ik`, `numbers`, `unicode_escapes`, `unicode_basic`, and
  `y_string_unicode`. Each admitted row has Track 1 distinct-path output,
  independent Track 2 structural-oracle output, `parse_only/sonic_rs::Skipper`
  strict comparator evidence, `Track 1 > Skipper + 1.0 Mbps`, no warmup
  iterations, and per-iteration equality PASS.
- Eleven parse_only rows remain open because the cold distinct path did not
  clear the Skipper + 1.0 Mbps threshold: `twitter`, `citm_catalog`, `canada`,
  `apache_builds`, `github_events`, `update_center`, `random`, `gsoc-2018`,
  `instruments`, `unicode_mixed`, and `distinct_values`. These rows are routed
  as measured residuals; they must not be cited as admitted W10 evidence.
  Evidence: `cargo test -p runtime generated_parse_only_accepts_and_rejects_json
  -- --nocapture`, `cargo test -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`,
  `cargo test -p bbnf-bench skv14_json_parse_only_report_accepts -- --nocapture`,
  `cargo test -p xtask -- --nocapture`, and the retained W10 redress/close
  packets under `restart/skinny/tranches/sk-v14/research/`.

## SK-V14 W10R JSON parse_only Prefix Continuation

- Item 218 closes `G-SK-V14-W10R-JSON-PARSE-ONLY-PREFIX` as `MIXED`.
  The source implementation adds
  `parse_that_regex::match_string_at_quote_after_plain_prefix_trusted_utf8`
  and regenerates the JSON runtime so the generated parse-only path keeps its
  tiny-string fast path but resumes the trusted SIMD string matcher after the
  already-scanned plain prefix for longer strings. This is a terminal scanner
  improvement, not a tape build, hidden DOM path, or comparator relabel.
- One additional parse_only row admits from cold `profile_direct` evidence
  retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W10R-parse-only-profile-direct.tsv`:
  `canada`. The row has Track 1 `16709.901` Mbps, independent Track 2
  `8631.499` Mbps, `parse_only/sonic_rs::Skipper` strict comparator
  `12970.929` Mbps, no warmup iterations, and per-iteration equality PASS.
- Ten parse_only rows remain open because the W10R cold run did not clear
  Skipper + 1.0 Mbps: `twitter`, `citm_catalog`, `apache_builds`,
  `github_events`, `update_center`, `random`, `gsoc-2018`, `instruments`,
  `unicode_mixed`, and `distinct_values`. Current parse_only state is 7 / 17
  admitted and 10 / 17 open. Evidence:
  `cargo xtask check-json`, `cargo test -p parse-that-regex
  trusted_string_matcher_continues_after_plain_prefix -- --nocapture`,
  `cargo test -p runtime generated_parse_only_accepts_and_rejects_json
  -- --nocapture`, `cargo test -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`,
  and `restart/skinny/tranches/sk-v14/research/skv14-W10R-parse-only-prefix-continuation.md`.

## SK-V14 W10S JSON parse_only String-End Prefix Scan

- Item 219 closes `G-SK-V14-W10S-JSON-PARSE-ONLY-STRING-END` as `MIXED`.
  The source implementation adds a raw-end trusted string matcher plus a
  word-mask tiny-string prefix scan for generated JSON `parse_only`, and
  regenerates the JSON runtime from `runtime_generator.rs`. This is still the
  strict parse_only plane: no tape construction, no DOM path, and no comparator
  relabel.
- One additional parse_only row admits from cold `profile_direct` evidence
  retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W10S-parse-only-string-end-profile-direct.tsv`:
  `unicode_mixed`. The row has Track 1 `7379.340` Mbps, independent Track 2
  `5656.917` Mbps, `parse_only/sonic_rs::Skipper` strict comparator
  `7011.268` Mbps, no warmup iterations, and per-iteration equality PASS.
- Nine parse_only rows remain open because the current cold evidence has not
  cleared Skipper + 1.0 Mbps: `twitter`, `citm_catalog`, `apache_builds`,
  `github_events`, `update_center`, `random`, `gsoc-2018`, `instruments`, and
  `distinct_values`. Current parse_only state is 8 / 17 admitted and 9 / 17
  open. Evidence: `cargo xtask check-json`, `cargo test -p parse-that-regex
  trusted_ -- --nocapture`, `cargo test -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, `cargo test
  -p codegen emits_distinct_json_parse_only_path_without_tape_builder --
  --nocapture`, and the retained W10S profile artifacts.

## SK-V14 W10T JSON parse_only Open-Row Sweep

- Item 220 closes `G-SK-V14-W10T-JSON-PARSE-ONLY-OPEN-SWEEP` as `MIXED`.
  No new source patch landed in W10T; it is the cold same-binary open-row
  sweep after the W10S source implementation. The sweep preserves the strict
  parse_only plane: generated Track 1 `runtime::generated_json::parse_only`,
  independent Track 2 structural oracle, and `sonic_rs::Skipper` as the strict
  comparator.
- One additional parse_only row admits from cold `profile_direct` evidence
  retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W10T-parse-only-open-sweep.tsv`:
  `instruments`. The row has Track 1 `4281.770` Mbps, independent Track 2
  `2748.324` Mbps, `parse_only/sonic_rs::Skipper` strict comparator
  `3457.276` Mbps, no warmup iterations, and per-iteration equality PASS.
- Eight parse_only rows remain open because the W10T cold evidence still does
  not clear Skipper + 1.0 Mbps: `twitter`, `citm_catalog`, `apache_builds`,
  `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`. Current parse_only state is 9 / 17 admitted and 8 / 17
  open. Evidence: retained W10T raw log + TSV, `cargo test -p bbnf-bench
  skv14_json_parse_only_report_accepts -- --nocapture`, `cargo test -p xtask
  -- --nocapture`, and `cargo xtask gate-json --check-results
  --skv14-existing-results-capture`.

## SK-V14 W10U JSON parse_only Number-End Scanner Reject

- Item 221 closes `G-SK-V14-W10U-JSON-PARSE-ONLY-NUMBER-END` as `REJECTED`.
  The attempted source change added a parse-only number matcher that returned
  only the end offset rather than building `NumberSpan`. It was abrogated after
  measurement and is not present in HEAD.
- No open parse_only row admitted under the W10U cold same-binary evidence.
  Worse, the same binary regressed existing admits: `instruments/parse_only`
  measured Track 1 `4161.964` Mbps against Skipper `4517.452` Mbps, and
  `unicode_mixed/parse_only` measured Track 1 `2686.532` Mbps against Skipper
  `4886.333` Mbps.
- Disposition: keep the current full `NumberSpan` matcher canonical for
  generated JSON `parse_only`. Do not reattempt a standalone end-offset number
  scanner unless it beats same-run strict evidence and preserves existing
  admits. Evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W10U-parse-only-number-end-reject.md`,
  `.tsv`, and `.raw.log`.

## SK-V14 W10V JSON parse_only Current-HEAD Resweep

- Item 222 closes `G-SK-V14-W10V-JSON-PARSE-ONLY-CURRENT-HEAD-RESWEEP` as
  `MIXED`. No source patch landed. The W10V source candidates were abrogated
  before ledger movement: REDRESS-84-adjacent object value-byte carry, a
  constant-width literal matcher, a container entry cleanup, and a parse-only
  string-tail scalar finish whose A/B showed the admitted row was already
  admitted on current HEAD.
- One additional parse_only row admits from cold current-HEAD `profile_direct`
  evidence retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W10V-parse-only-current-head-resweep.tsv`:
  `citm_catalog`. The row has Track 1 `9079.838` Mbps, independent Track 2
  `13566.569` Mbps, strict `parse_only/sonic_rs::Skipper` comparator
  `8335.772` Mbps, `serde_json` `5121.472` Mbps, no warmup iterations, and
  per-iteration equality PASS.
- Seven parse_only rows remain open after W10V because the cold evidence still
  does not clear Skipper + 1.0 Mbps: `twitter`, `apache_builds`,
  `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`. Current parse_only state is 10 / 17 admitted and 7 / 17
  open.

## SK-V14 W10W JSON parse_only Iterative Stack

- Item 223 closes `G-SK-V14-W10W-JSON-PARSE-ONLY-ITERATIVE-STACK` as
  `MIXED`. The source patch replaces the recursive generated JSON
  `parse_only` container walk with an explicit generated iterative stack for
  objects and arrays. The cap-16 tiny-string candidate was abrogated because
  it did not robustly admit any open row.
- One additional parse_only row admits from cold `profile_direct` evidence
  retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W10W-parse-only-iterative-stack.tsv`:
  `apache_builds`. The admitted 4000-iteration row has Track 1 `13129.331`
  Mbps, independent Track 2 `9065.855` Mbps, strict
  `parse_only/sonic_rs::Skipper` comparator `12951.668` Mbps, `serde_json`
  `3964.266` Mbps, no warmup iterations, and per-iteration equality PASS.
  Two further same-binary 4000-iteration repeats also cleared Skipper + 1.0
  Mbps with Track 1 `13285.106` and `13305.497` Mbps against Skipper
  `13007.626` and `12868.672` Mbps.
- Six parse_only rows remain open after W10W because the cold evidence still
  does not clear Skipper + 1.0 Mbps: `twitter`, `github_events`,
  `update_center`, `random`, `gsoc-2018`, and `distinct_values`. Current
  parse_only state is 11 / 17 admitted and 6 / 17 open.

## SK-V14 W10X JSON parse_only Residual Rejects

- Item 224 closes `G-SK-V14-W10X-JSON-PARSE-ONLY-RESIDUAL-REJECTS` as
  `REJECT`. No source patch lands.
- Three source candidates were tested after W10W and rejected by cold
  `profile_direct` evidence:
  inline parse-only frame stack, 64-byte trusted string-special sweep, and
  trusted string syntax-mask scan. All held correctness during local tests, but
  none admitted any of the six remaining parse_only residual rows.
- Current parse_only state remains 11 / 17 admitted and 6 / 17 open:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`. Evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W10X-parse-only-residual-rejects.md`
  plus the three retained raw logs named there.

## SK-V14 W10Y/W10Z JSON parse_only Residual Rejects

- Item 225 closes `G-SK-V14-W10Y-W10Z-JSON-PARSE-ONLY-RESIDUAL-REJECTS` as
  `REJECT`. No source patch lands.
- Two materially distinct generated parse-only source candidates were tested
  after W10X and rejected by cold `profile_direct` evidence: a plain-string
  structural fast path and a cursor-return helper ABI. Both held correctness
  during local tests, but neither admitted any of the six remaining
  `parse_only` residual rows. The structural fast path also regressed guard
  rows, so the route is explicitly abrogated rather than retained as a support
  primitive.
- Current parse_only state remains 11 / 17 admitted and 6 / 17 open:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`. Evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W10Y-W10Z-parse-only-rejects.md`
  plus the two retained raw logs named there.

## SK-V14 W9Y JSON real_typed y_string_unicode Generated Root Reject

- Item 226 closes `G-SK-V14-W9Y-JSON-TYPED-Y-STRING-UNICODE` as `REJECT`.
  No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate used the valid generated DirectBuild route: a
  transient `parse_y_string_unicode -> Vec<Cow<'i, str>>` root in
  `skinny/xtask/src/real_typed_schema.rs`, regenerated
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, and temporary
  `real_typed_struct` dispatch/checksum/parity wiring. The earlier
  bench-private hand parser route is rejected as invalid for W9 Track 1
  generated-source discipline.
- Correctness gates passed before measurement:
  `cargo xtask regen-real-typed`, `cargo xtask check-real-typed`, and
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p
  bbnf-bench y_string_unicode_typed -- --nocapture`.
- Cold `profile_direct` evidence rejects the row: generated Track 1 measured
  `3661.016` Mbps against strict `sonic-rs` typed at `3906.865` Mbps. The
  admission floor is `sonic + 1.0 = 3907.865` Mbps, so the margin is
  `-246.849` Mbps. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W9Y-y-string-unicode-typed.md`,
  `.tsv`, and `.raw.log`.
- Current JSON real_typed_struct state remains 11 / 17 ADMITTED and 6 MISSING:
  `canada`, `gsoc-2018`, `unicode_mixed`, `unicode_escapes`,
  `distinct_values`, and `y_string_unicode`.

## SK-V14 W9AA JSON real_typed distinct_values Dynamic Payload Admit

- Item 227 closes `G-SK-V14-W9AA-JSON-TYPED-DISTINCT-VALUES` as `ADMIT`.
  The source patch extends generated DirectBuild typed schemas with an
  unknown-field string-entry capture policy and routes
  `distinct_values/real_typed_struct` through the regenerated
  `parse_distinct_values` root.
- The admitted product is full row payload materialization, not a fixed-field
  shortcut: each object retains `timestamp`, `seq`, `status`, and every
  dynamic `key_*` string/value pair as `DistinctField { key, value }`, with
  serde and sonic sidecars using the same typed product for equality.
- Correctness gates passed before measurement:
  `cargo xtask regen-real-typed`, `cargo xtask check-real-typed`,
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p
  bbnf-bench distinct_values_typed -- --nocapture`, and
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen
  unknown_string_capture -- --nocapture`.
- Cold `profile_direct` evidence admits the row: generated Track 1 measured
  `8827.520` Mbps against strict `sonic-rs` typed at `3895.064` Mbps. The
  admission floor is `sonic + 1.0 = 3896.064` Mbps, so the margin is
  `4931.456` Mbps. Independent Track 2 measured `3245.184` Mbps and
  serde_json measured `3334.552` Mbps. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W9AA-distinct-values-typed.md`,
  `.tsv`, and `.raw.log`.
- Current JSON real_typed_struct state is now 12 / 17 ADMITTED and 5 MISSING:
  `canada`, `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and
  `y_string_unicode`.

## SK-V14 W9AB JSON real_typed canada Numeric Lexeme Admit

- Item 228 closes `G-SK-V14-W9AB-JSON-TYPED-CANADA` as `ADMIT`. The source
  patch extends generated DirectBuild typed schemas with a `NumberString`
  scalar and routes `canada/real_typed_struct` through the regenerated
  `parse_canada` root.
- The admitted product preserves exact coordinate numeric lexemes as
  `Cow<'i, str>` rather than f64 values. The f64 route was rejected before
  admission because serde and sonic round long GeoJSON coordinate literals
  differently; exact lexeme parity is the stable typed product surface.
- Correctness gates passed before measurement:
  `cargo xtask regen-real-typed`, `cargo xtask check-real-typed`,
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p
  bbnf-bench canada_typed -- --nocapture`, and
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen
  emits_typed_direct_number_string_capture -- --nocapture`.
- Cold `profile_direct` evidence admits the row: generated Track 1 measured
  `4761.909` Mbps against strict `sonic-rs` typed at `2736.418` Mbps. The
  admission floor is `sonic + 1.0 = 2737.418` Mbps, so the margin is
  `2024.491` Mbps. Independent Track 2 measured `3397.878` Mbps and
  serde_json measured `3383.986` Mbps. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W9AB-canada-typed.md`,
  `.tsv`, and `.raw.log`.
- Current JSON real_typed_struct state is now 13 / 17 ADMITTED and 4 MISSING:
  `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`.

## SK-V14 W9AC JSON real_typed gsoc-2018 Numeric-Key Reject

- Item 229 closes `G-SK-V14-W9AC-JSON-TYPED-GSOC-2018` as `REJECT`.
  No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate used a valid generated DirectBuild route with a fresh
  material differential over W13.5: `gsoc-2018` root object keys were parsed as
  `u32` via transient `MapU32EntriesVec` codegen and a generated
  `parse_gsoc_2018 -> Vec<GsocProposalEntry<'i>>` root. The product preserved
  the numeric key plus every nested proposal, sponsor, and author string field.
- Correctness gates passed before measurement:
  `cargo xtask regen-real-typed`, `cargo xtask check-real-typed`,
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen
  emits_typed_direct_u32_keyed_map_entries -- --nocapture`, and
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p
  bbnf-bench gsoc_2018_typed -- --nocapture`.
- Cold `profile_direct` evidence rejects the row: generated Track 1 measured
  `5711.366` Mbps against strict `sonic-rs` typed at `6017.313` Mbps. The
  admission floor is `sonic + 1.0 = 6018.313` Mbps, so the margin is
  `-306.947` Mbps. Independent Track 2 measured `5631.957` Mbps and
  serde_json measured `5639.270` Mbps. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W9AC-gsoc-2018-typed.md`,
  `.tsv`, and `.raw.log`.
- Current JSON real_typed_struct state remains 13 / 17 ADMITTED and 4 MISSING:
  `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`.

## SK-V14 W10AA JSON parse_only Fused String/Object-Loop Reject

- Item 230 closes `G-SK-V14-W10AA-JSON-PARSE-ONLY-FUSED-STRING-OBJECT-LOOP`
  as `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate was a generated parse_only source route with a fresh
  material differential over W10X/W10Y/W10Z: a fused trusted-UTF-8 string-end
  helper plus removal of the redundant initial object-key-or-end frame after
  the non-empty object case had already checked for `}`.
- Correctness gates passed before measurement:
  `cargo xtask regen-json`, `cargo xtask check-json`,
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p
  parse-that-regex trusted_ -- --nocapture`,
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, and
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows:
  `twitter` margin `-3027.426` Mbps, `github_events` margin `-2894.566` Mbps,
  `update_center` margin `-4022.023` Mbps, `random` margin `-2263.534` Mbps,
  `gsoc-2018` margin `-13180.314` Mbps, and `distinct_values` margin
  `-5027.487` Mbps versus the `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W10AA-parse-only-fused-string-object-loop.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11H JSON parse_only Value-Byte Carry Reject

- Item 238 closes `G-SK-V14-W11H-JSON-PARSE-ONLY-VALUE-BYTE-CARRY` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate extended W11G into a generated value-byte carry:
  `parse_only_key_colon` validated the key, consumed the colon, skipped
  post-colon whitespace, returned the first value byte, and object member
  parsing fed that byte into a new `parse_only_begin_value_with_byte` helper
  covering all value arms. It was not W11F's string/object-only fast arm and
  did not use W11D delimiter threading, array comma carry, object comma key
  carry, structural pre-scans, cursor-return ABI changes, or W10AA object-loop
  cleanup. The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11H-parse-only-value-byte-carry-rejected.patch` with SHA-256
  `eb79dd2154f972812478f2b191583b8a457fb8740fc4d14979fddb2dd81f08d8`.
- Correctness gates passed before measurement: `cargo xtask regen-json`,
  `cargo xtask check-json`, `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, and
  `cargo test --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows:
  `twitter` margin `-3592.480` Mbps, `github_events` margin `-3339.155` Mbps,
  `update_center` margin `-4266.445` Mbps, `random` margin `-2326.277` Mbps,
  `gsoc-2018` margin `-13566.659` Mbps, and `distinct_values` margin
  `-5371.352` Mbps versus the `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11H-parse-only-value-byte-carry.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11I JSON parse_only Array Value-Byte Carry Reject

- Item 239 closes `G-SK-V14-W11I-JSON-PARSE-ONLY-ARRAY-VALUE-BYTE-CARRY` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate added generated array comma-to-next-value byte carry:
  `parse_only_consume_array_next` returned the already-found next value byte
  after a comma, and `ArrayAfterValue` fed that byte into all value arms
  through `parse_only_begin_value_with_byte`. It did not alter object
  key-colon handling, W11H object-member value-byte carry, W11D delimiter
  threading, W11F object-member string/object fast arms, object comma key
  specialization, structural pre-scans, cursor-return ABI changes, or W10AA
  object-loop cleanup. The source patch was reverted after measurement and
  retained as
  `/tmp/skv14-W11I-parse-only-array-value-carry-rejected.patch` with SHA-256
  `2ad5a499b1f4deae57aa0fd2cdf4ea733bd49627a5efbf89c02066090c185c64`.
- Correctness gates passed before measurement: `cargo xtask regen-json`,
  `cargo xtask check-json`, `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, and
  `cargo test --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows:
  `twitter` margin `-3791.053` Mbps, `github_events` margin `-3063.966` Mbps,
  `update_center` margin `-4311.089` Mbps, `random` margin `-2186.630` Mbps,
  `gsoc-2018` margin `-13458.892` Mbps, and `distinct_values` margin
  `-5098.971` Mbps versus the `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11I-parse-only-array-value-carry.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11J JSON parse_only Object Key Specialization Reject

- Item 240 closes `G-SK-V14-W11J-JSON-PARSE-ONLY-OBJECT-KEY-SPECIALIZATION` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate split the generated object delimiter state so an
  object comma path skipped following whitespace, required the next key quote,
  and dispatched directly into key parsing instead of returning through
  generic `ObjectExpectKey`. It did not fuse key-string plus colon handling,
  carry object value bytes, thread value context, add object-member
  string/object fast arms, carry array value bytes, use a structural pre-scan,
  use a cursor-return ABI, or reuse W10AA object-loop cleanup. The source patch
  was reverted after measurement and retained as
  `/tmp/skv14-W11J-parse-only-object-key-specialization-rejected.patch` with
  SHA-256
  `a1428c1561d4baaaff5dc8049796aaa87a6aa5cdcbef95199f557a8b075ecb5b`.
- Correctness gates passed before measurement: `cargo xtask regen-json`,
  `cargo xtask check-json`, `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, and
  `cargo test --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows:
  `twitter` margin `-3649.409` Mbps, `github_events` margin `-3417.568` Mbps,
  `update_center` margin `-3998.806` Mbps, `random` margin `-2157.062` Mbps,
  `gsoc-2018` margin `-13774.879` Mbps, and `distinct_values` margin
  `-5257.476` Mbps versus the `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11J-parse-only-object-key-specialization.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11K JSON y_string_unicode Fused Materializer Reject

- Item 241 closes `G-SK-V14-W11K-JSON-Y-STRING-FUSED-MATERIALIZER` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate added a fused trusted-UTF-8 JSON string materializer
  in `parse-that-regex`, generated `parse_y_string_unicode`, and routed
  `y_string_unicode` through typed and direct strict products. It preserved the
  tiny plain-string borrowed fast path and decoded escaped strings while
  scanning. The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11K-y-string-fused-materializer-rejected.patch` with SHA-256
  `f12d67fea15eaff2fbfcc212cb78b37fc8db674e79dbd769e7ad4f2365fadb4d`.
- Correctness gates passed before measurement: focused parse-that-regex
  materializer tests, focused codegen typed-direct tests, `cargo run --profile
  ax-iter -p xtask -- regen-real-typed`, `cargo run --profile ax-iter -p xtask
  -- check-real-typed`, focused `y_string_unicode_typed` tests, and focused
  direct strict-product parity tests.
- Cold `profile_direct` evidence rejects both attempted rows:
  `y_string_unicode/real_typed_struct` margin `-1978.443` Mbps and
  `y_string_unicode/direct_to_struct` margin `-2352.255` Mbps versus the
  `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11K-y-string-fused-materializer.md`,
  `.tsv`, and `.raw.log`.
- Current JSON direct_to_struct state remains 13 / 17 ADMITTED and 4 OPEN.
  Current JSON real_typed_struct state remains 13 / 17 ADMITTED and 4 MISSING:
  `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`.

## SK-V14 W11G JSON parse_only Key-Colon Fusion Reject

- Item 237 closes `G-SK-V14-W11G-JSON-PARSE-ONLY-KEY-COLON-FUSION` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate fused generated key-string validation and colon
  consumption inside `parse_only_key_colon`: it called
  `parse_only_string_end` directly, checked the post-key colon or intervening
  whitespace, and then stopped after colon whitespace. It deliberately carried
  no next value byte, retained the existing `ObjectAfterValue` delimiter path,
  and did not use W11D context-threading, W11F object-member fast arms,
  structural pre-scans, cursor-return ABI changes, or W10AA object-loop
  cleanup. The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11G-parse-only-key-colon-fusion-rejected.patch` with SHA-256
  `c538adcc2abd703d7fc77a39e546dcfff0e12a15f9ba9edc7d9a21826d42f210`.
- Correctness gates passed before measurement: `cargo xtask regen-json`,
  `cargo xtask check-json`, `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, and
  `cargo test --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows:
  `twitter` margin `-3661.483` Mbps, `github_events` margin `-3343.328` Mbps,
  `update_center` margin `-4020.596` Mbps, `random` margin `-2248.300` Mbps,
  `gsoc-2018` margin `-13483.416` Mbps, and `distinct_values` margin
  `-5365.590` Mbps versus the `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11G-parse-only-key-colon-fusion.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11A JSON direct_to_struct Strict Product Admit

- Item 231 closes `G-SK-V14-W11A-JSON-DIRECT-STRICT-PRODUCT` as `ADMIT`.
  The source patch adds an explicit strict-product direct route over the
  generated DirectBuild typed products and keeps the public workload as
  `direct_to_struct`; no digest-plane result is re-admitted.
- Thirteen direct rows admit from cold native `profile_direct` evidence:
  `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`,
  `update_center`, `mesh`, `random`, `marine_ik`, `instruments`, `numbers`,
  `unicode_basic`, and `distinct_values`.
- Four direct rows remain open because they still lack generated strict product
  surfaces at HEAD: `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and
  `y_string_unicode`.
- Correctness and compile gates passed before ledger movement:
  `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p
  bbnf-bench direct_strict_product -- --nocapture`.
- Cold evidence is retained at
  `restart/skinny/tranches/sk-v14/research/skv14-W11A-direct-strict-product.md`,
  `.tsv`, and `.raw.log`. Current JSON direct_to_struct state is 13 / 17
  ADMITTED and 4 OPEN.

## SK-V14 W11B JSON Unicode Product Probe Reject

- Item 232 closes `G-SK-V14-W11B-JSON-UNICODE-PRODUCTS` as `REJECT`. No source
  patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate added transient generated strict product surfaces for
  `unicode_mixed` and `unicode_escapes`, then routed both
  `real_typed_struct` and W11A-style `direct_to_struct` strict-product
  consumers over those products. The patch was reverted after measurement and
  retained as `/tmp/skv14-W11B-unicode-products-rejected.patch` with SHA-256
  `6fa6aa72ee5afd1fc701a17aa3871ed003b5ba9d3a46e2ce456167bba8b72aa5`.
- Correctness gates passed before measurement:
  `cargo run --profile ax-iter -p xtask -- regen-real-typed`, `cargo run
  --profile ax-iter -p xtask -- check-real-typed`, `cargo test
  --manifest-path skinny/Cargo.toml --profile ax-iter -p bbnf-bench unicode_
  -- --nocapture`, and `cargo test --manifest-path skinny/Cargo.toml
  --profile ax-iter -p bbnf-bench direct_strict_product -- --nocapture`.
- Cold `profile_direct` evidence rejects both products and both direct-strict
  rows: `unicode_mixed/real_typed_struct` measured `3074.922` Mbps against
  strict sonic `5166.402` Mbps, `unicode_mixed/direct_to_struct` strict product
  measured `3130.925` Mbps against strict sonic `5144.127` Mbps,
  `unicode_escapes/real_typed_struct` measured `3870.109` Mbps against strict
  sonic `7649.956` Mbps, and `unicode_escapes/direct_to_struct` strict product
  measured `3829.754` Mbps against strict sonic `7762.353` Mbps. Each row
  misses the `sonic + 1.0` floor by at least `2014.202` Mbps.
- Current JSON direct_to_struct state remains 13 / 17 ADMITTED and 4 OPEN.
  Current JSON real_typed_struct state remains 13 / 17 ADMITTED and 4 MISSING:
  `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`.

## SK-V14 W11C JSON GSoC Product Probe Reject

- Item 233 closes `G-SK-V14-W11C-JSON-GSOC-PRODUCTS` as `REJECT`. No source
  patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidates added transient generated strict product surfaces for
  `gsoc-2018`: numeric top-level object keys, ordered fixed-member parsing for
  the Schema.org proposal/sponsor/author objects, identity-product and
  full-product variants, plus required-field full-product parsing. The patch
  was reverted after measurement and retained as
  `/tmp/skv14-W11C-gsoc-products-rejected.patch` with SHA-256
  `258bdb69a286b0e60b57543f127be7c57ca0561a5657454d0ce5d7639a74faa9`.
- Correctness gates passed before measurement: `cargo run --profile ax-iter -p
  xtask -- regen-real-typed`, `cargo run --profile ax-iter -p xtask --
  check-real-typed`, `cargo test --profile ax-iter -p codegen typed_direct_ --
  --nocapture`, `cargo test --profile ax-iter -p bbnf-bench
  gsoc_2018_typed -- --nocapture`, and `cargo test --profile ax-iter -p
  bbnf-bench direct_strict_product -- --nocapture`.
- Cold `profile_direct` evidence rejects the closest full-product route:
  `gsoc-2018/real_typed_struct` measured `5789.034` Mbps against strict sonic
  `6482.407` Mbps, and `gsoc-2018/direct_to_struct` strict product measured
  `5834.269` Mbps against strict sonic `6111.175` Mbps. The identity-product
  route reached higher absolute Track 1 throughput (`19909.635` typed,
  `19938.076` direct-strict) but widened the strict sonic gap
  (`24783.657` typed sonic, `24927.218` direct-strict sonic).
- Current JSON direct_to_struct state remains 13 / 17 ADMITTED and 4 OPEN.
  Current JSON real_typed_struct state remains 13 / 17 ADMITTED and 4 MISSING:
  `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`.

## SK-V14 W11D JSON parse_only Threaded Context Reject

- Item 234 closes `G-SK-V14-W11D-JSON-PARSE-ONLY-THREADED-CONTEXT` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate was a generated parse_only source route with a fresh
  material differential over W10AA: value context was threaded through the
  iterative parser so completed scalar values and empty containers consumed
  their enclosing object/array delimiter immediately instead of returning
  through the generic after-value frame first. The candidate retained the
  existing key-colon stop after colon whitespace, carried no next value byte,
  and did not use a structural pre-scan, cursor-return ABI, or the W10AA fused
  string/object-loop cleanup route. The source patch was reverted after
  measurement and retained as
  `/tmp/skv14-W11D-parse-only-threaded-context-rejected.patch` with SHA-256
  `98b9494008e0d810699788c1ed8c667b2de29727301be6d27b3f6cf65d2b7146`.
- Correctness gates passed before measurement: `cargo xtask regen-json`,
  `cargo xtask check-json`, `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, and
  `cargo test --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows:
  `twitter` margin `-3898.964` Mbps, `github_events` margin `-3216.303` Mbps,
  `update_center` margin `-4231.665` Mbps, `random` margin `-2333.190` Mbps,
  `gsoc-2018` margin `-13844.268` Mbps, and `distinct_values` margin
  `-5258.386` Mbps versus the `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11D-parse-only-threaded-context.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11E JSON parse_only 64-Byte Whitespace Reject

- Item 235 closes `G-SK-V14-W11E-JSON-PARSE-ONLY-WHITESPACE64` as `REJECT`.
  No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate replaced `parse-that-regex::skip_ascii_whitespace`
  with a grammar-neutral 64-byte set-member skip over JSON whitespace using
  the existing `bbnf-simd` `byte_class_from_eq_set_64` primitive. The patch
  was reverted after measurement and retained as
  `/tmp/skv14-W11E-parse-only-whitespace64-rejected.patch` with SHA-256
  `0d07dd3120d54cbf2424c90ba861f134b85081f10840d5df254049ecbad4d47f`.
- Correctness and primitive gates passed before measurement: `cargo test
  --profile ax-iter -p parse-that-regex
  ascii_whitespace_skip_matches_json_space_set -- --nocapture`, `cargo test
  --profile ax-iter -p runtime generated_parse_only_accepts_and_rejects_json
  -- --nocapture`, and `cargo test --profile ax-iter -p bbnf-simd --test
  checkasm_byte_class_from_eq_set_64 -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows and
  regresses the shared primitive route: `twitter` margin `-8114.740` Mbps,
  `github_events` margin `-7174.497` Mbps, `update_center` margin
  `-4343.837` Mbps, `random` margin `-5973.598` Mbps, `gsoc-2018` margin
  `-17949.627` Mbps, and `distinct_values` margin `-7026.793` Mbps versus
  the `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11E-parse-only-whitespace64.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11F JSON parse_only Object-Member Fast Arm Reject

- Item 236 closes `G-SK-V14-W11F-JSON-PARSE-ONLY-OBJECT-MEMBER-FAST-ARM` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate added a generated parse_only object-member fast arm:
  after `parse_only_key_colon`, values beginning with a string or object
  dispatch directly to the string parser or object opener, while arrays,
  numbers, literals, and other values fall back to the generic value
  dispatcher. It retains the existing `ObjectAfterValue` delimiter state, so
  it is not W11D value-context delimiter threading; it also carries no value
  byte from key-colon and does not use a structural pre-scan or cursor-return
  ABI. The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11F-parse-only-object-member-fast-arm-rejected.patch` with
  SHA-256 `78e72f694a683de1a54c4f877205ada36e37e2376e89b904eaf541b28dee9aee`.
- Correctness gates passed before measurement: `cargo xtask regen-json`,
  `cargo xtask check-json`, `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, and
  `cargo test --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows:
  `twitter` margin `-3437.756` Mbps, `github_events` margin `-3356.062` Mbps,
  `update_center` margin `-4089.845` Mbps, `random` margin `-2441.619` Mbps,
  `gsoc-2018` margin `-14105.227` Mbps, and `distinct_values` margin
  `-5342.646` Mbps versus the `sonic + 1.0` floor. Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11F-parse-only-object-member-fast-arm.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11M unicode_escapes Decoded Product Reject

- Item 242 closes
  `G-SK-V14-W11M-JSON-UNICODE-ESCAPES-DECODED-PRODUCT` as `REJECT`.
  No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate added a generic typed
  `DirectScalar::DecodedJsonString` and generated `parse_unicode_escapes`
  root. Track 1 returned a per-field product carrying raw escaped source for
  escaped strings, borrowed decoded source for plain strings, and decoded
  semantic facts `(fingerprint, len)`; Track 2, serde_json, and sonic-rs
  independently produced the same semantic product from decoded strings. It did
  not use generic `parse_only`, `JsonDigestSink`, `JsonDirectDigest`, or an
  aggregate document checksum.
- The route is materially distinct from REDRESS-54/55, REDRESS-66/67/68/69,
  and REDRESS-117/118 because it changes the typed product surface rather than
  feeding decoded string facts into the direct digest representation. It still
  failed the cold gate: `unicode_escapes/real_typed_struct` measured Track 1
  `5824.372` Mbps versus sonic `7073.230` Mbps, margin `-1249.858` Mbps, and
  `unicode_escapes/direct_to_struct` measured Track 1 `5707.469` Mbps versus
  strict sonic `7620.832` Mbps, margin `-1914.363` Mbps.
- Correctness gates passed before measurement: `cargo run --profile ax-iter -p
  xtask -- regen-real-typed`, `cargo run --profile ax-iter -p xtask --
  check-real-typed`, focused `unicode_escapes` product tests including
  malformed escape/control/surrogate rejection, `cargo test --profile ax-iter
  -p codegen emits_typed_direct -- --nocapture`, and focused direct
  strict-product parity.
- The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11M-unicode-escapes-rejected.patch` with SHA-256
  `a774358440dd49ae6a46762a2ef5cbd848a5e1e8684f34f954dc2eb34b53d090`.
  Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11M-unicode-escapes-decoded-product.md`,
  `.tsv`, and `.raw.log`.
- Current JSON direct_to_struct state remains 14 / 17 ADMITTED and 3 OPEN:
  `gsoc-2018`, `unicode_mixed`, and `unicode_escapes`. Current JSON
  real_typed_struct state remains 14 / 17 ADMITTED and 3 MISSING:
  `gsoc-2018`, `unicode_mixed`, and `unicode_escapes`.

## SK-V14 W11P unicode_escapes Codepoint Product Reject

- Item 243 closes
  `G-SK-V14-W11P-JSON-UNICODE-ESCAPES-CODEPOINT-PRODUCT` as `REJECT`.
  No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate added a generated
  `DirectScalar::DecodedJsonCodepoints` product and generated
  `parse_unicode_escapes` root. Track 1 decoded JSON string escapes directly
  into Unicode scalar fingerprints and scalar counts, validating surrogate
  pairs and malformed escapes without materializing decoded strings. Track 2,
  serde_json, and sonic-rs independently produced the same semantic facts from
  decoded Rust strings. It did not use generic `parse_only`,
  `JsonDigestSink`, `JsonDirectDigest`, skipped payloads, or an aggregate
  document checksum.
- The route is materially distinct from REDRESS-242 because it folds decoded
  scalar values rather than UTF-8 bytes and avoids W11M's borrowed/raw-source
  product boundary. It still failed the cold gate:
  `unicode_escapes/real_typed_struct` measured Track 1 `4211.977` Mbps versus
  sonic `6908.358` Mbps, margin `-2696.381` Mbps, and
  `unicode_escapes/direct_to_struct` measured Track 1 `4186.323` Mbps versus
  strict sonic `7217.462` Mbps, margin `-3031.139` Mbps.
- Correctness gates passed before measurement: `cargo run --profile ax-iter -p
  xtask -- regen-real-typed`, `cargo run --profile ax-iter -p xtask --
  check-real-typed`, focused `unicode_escapes` product tests including
  malformed escape and surrogate rejection, `cargo test --profile ax-iter -p
  codegen typed_direct -- --nocapture`, and focused direct strict-product
  parity.
- The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11P-unicode-escapes-codepoint-product-rejected.patch` with
  SHA-256
  `68e11bbad6c6708fb34b8ee83566707899c6e50325477afbd831bc10b913bfb1`.
  Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11P-unicode-escapes-codepoint-product.md`,
  `.tsv`, and `.raw.log`.
- At W11P close, JSON direct_to_struct state remained 16 / 17 ADMITTED and
  1 OPEN: `unicode_escapes`. JSON real_typed_struct state remained
  16 / 17 ADMITTED and 1 MISSING: `unicode_escapes`.

## SK-V14 W11Q Parse-Only Indexed Strings Reject

- Item 244 closes `G-SK-V14-W11Q-JSON-PARSE-ONLY-INDEXED-STRINGS` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate extended the JSON structural scan to produce
  quote/punctuation positions plus a risky-string-start side table. Generated
  `parse_only` used the index to skip full string validation for scanner-proven
  plain strings and kept the existing validator for strings with escapes or
  control bytes. UTF-8 validation stayed in `parse_only_bytes`; number,
  literal, delimiter, and EOF validation stayed on the existing generated
  parse_only path.
- The route is materially distinct from REDRESS-224 through REDRESS-240 and
  W10Y/W10Z because it does not rely on delimiter threading, whitespace64,
  value-byte carry, object specialization, or a global plain-string assumption.
  It still failed the cold gate for every parse_only residual row against the
  `sonic + 1.0` floor: `twitter` margin `-3125.119` Mbps,
  `github_events` margin `-2629.482` Mbps, `update_center` margin
  `-5513.509` Mbps, `random` margin `-2456.138` Mbps, `gsoc-2018` margin
  `-16725.001` Mbps, and `distinct_values` margin `-1516.426` Mbps.
- Correctness gates passed before measurement: `cargo run --profile ax-iter -p
  xtask -- regen-json`, `cargo run --profile ax-iter -p xtask -- check-json`,
  `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, and
  `cargo test --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`.
- The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11Q-parse-only-indexed-strings-rejected.patch` with SHA-256
  `cd8620ba8f53caa51851069eb83d114ce73968f1edfff6231d32b5d422436a52`.
  Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11Q-parse-only-indexed-strings.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11R unicode_escapes Fixed-Shape Floor Reject

- Item 245 closes
  `G-SK-V14-W11R-JSON-UNICODE-ESCAPES-FIXED-SHAPE-FLOOR` as `REJECT`.
  No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate added a transient fixed-shape `unicode_escapes`
  floor parser to `profile_direct`. Track 1 consumed the concrete
  `{meta, records}` fixture shape, verified `mode == "escapes"` and
  `ensure_ascii == true`, validated every record id, decoded string escape,
  surrogate pair, and raw-control rejection, and folded every decoded payload
  UTF-8 byte into a strict semantic product. serde_json and sonic-rs sidecars
  independently produced the same product through a matching strict struct.
- The route is materially distinct from REDRESS-242 and REDRESS-243 because it
  removes generated typed/direct product-surface overhead and tests a
  fixed-shape minimum floor while still consuming every decoded payload unit.
  It still failed the cold gate: `unicode_escapes/direct_to_struct` measured
  Track 1 `751.889` Mbps versus sonic `1191.214` Mbps, margin `-439.325`
  Mbps, and `unicode_escapes/real_typed_struct` measured Track 1 `819.515`
  Mbps versus the same strict sonic sidecar `1191.214` Mbps, margin
  `-371.699` Mbps. Against the required `sonic + 1.0` floor, the deficits are
  `-440.325` Mbps and `-372.699` Mbps.
- Correctness gates passed before measurement: `cargo fmt --manifest-path
  skinny/Cargo.toml --package bbnf-bench` and `CARGO_TARGET_DIR=/tmp/skv14-w11r-test-target
  RUSTC_WRAPPER= cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench
  --bin profile_direct unicode_escapes_floor -- --nocapture`. The focused test
  suite covered full-fixture parity against serde_json and sonic-rs,
  surrogate-pair acceptance, invalid escape and surrogate rejection, wrong
  mode rejection, and raw control-character rejection.
- The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11R-unicode-escapes-floor-rejected.patch` with SHA-256
  `268b3d5207b9d252df10cdab37319eafeb11a197d4e72e75d4b3a2e85f471f16`.
  Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11R-unicode-escapes-fixed-shape-floor.md`,
  `.tsv`, and `.raw.log`.
- At W11R close before W11U, JSON direct_to_struct state remained
  16 / 17 ADMITTED and 1 OPEN: `unicode_escapes`. JSON real_typed_struct
  state remained 16 / 17 ADMITTED and 1 MISSING: `unicode_escapes`.

## SK-V14 W11T Parse-Only Structural Stream Reject

- Item 246 closes `G-SK-V14-W11T-JSON-PARSE-ONLY-STRUCTURAL-STREAM` as
  `REJECT`. No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate changed the JSON scanner to emit punctuation plus
  real quote positions and a scanner-owned risky-string-start side table.
  Generated parse_only then used the structural stream as the parser driver
  for containers, delimiters, and string boundaries, while preserving fallback
  validation for escaped or control-bearing strings. This is materially
  distinct from W11Q indexed plain-string skipping because W11T replaced the
  parse_only driver instead of decorating the old byte-loop string validator.
- Correctness gates passed before measurement: `cargo run --profile ax-iter -p
  xtask -- regen-json`, `cargo run --profile ax-iter -p xtask -- check-json`,
  `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`, `cargo test
  --profile ax-iter -p codegen
  emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`,
  and `cargo test --profile ax-iter -p runtime match_scalar -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows against
  the `sonic + 1.0` floor: `twitter` margin `-2136.592` Mbps,
  `github_events` margin `-4010.756` Mbps, `update_center` margin
  `-3479.153` Mbps, `random` margin `-2791.399` Mbps, `gsoc-2018` margin
  `-4349.073` Mbps, and `distinct_values` margin `-2483.550` Mbps. Guard rows
  also failed the same floor: `canada` margin `-883.804` Mbps, `instruments`
  margin `-824.606` Mbps, `apache_builds` margin `-1395.578` Mbps, and
  `citm_catalog` margin `-7206.933` Mbps.
- The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11T-parse-only-structural-stream-rejected.patch` with SHA-256
  `fb7788d2b376efb91f61c08eae030c55613e355e368e884c820731de245da25b`.
  Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11T-parse-only-structural-stream.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11U unicode_escapes Supersession Note

W11U admits `unicode_escapes/direct_to_struct` and
`unicode_escapes/real_typed_struct` through a generated raw JSON string lexeme
product. This supersedes the W11R/W11P live-row status while preserving
REDRESS-242, REDRESS-243, and REDRESS-245 as pre-blocks for decoded-string,
decoded-codepoint, and fixed-shape decoded floor retries without a fresh
material differential.

## SK-V14 W11V Parse-Only String64 Reject

- Item 247 closes `G-SK-V14-W11V-JSON-PARSE-ONLY-STRING64` as `REJECT`.
  No source patch lands, no `RESULTS.md` row moves, and
  `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.
- The measured candidate added a 64-byte aarch64 JSON string-special mask
  primitive to `bbnf-simd` and routed the trusted parse-only string-end path
  through it in `parse-that-regex`. This is materially distinct from W11T:
  it does not add a structural stream, side table, or replacement parse_only
  driver. It targets the W11S rank-1 `parse_only_string` hot leaf directly.
- Correctness gates passed before measurement:
  `BBNF_SIMD_STRICT=1 cargo test --profile ax-iter -p bbnf-simd
  sk_v3_intrinsic_parity_aarch64 --test checkasm_parity -- --nocapture`,
  `cargo test --profile ax-iter -p parse-that-regex trusted_string --
  --nocapture`, and `cargo test --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`.
- Cold `profile_direct` evidence rejects all six open parse_only rows against
  the `sonic + 1.0` floor: `twitter` margin `-2696.309` Mbps,
  `github_events` margin `-1943.020` Mbps, `update_center` margin
  `-4316.162` Mbps, `random` margin `-2239.792` Mbps, `gsoc-2018` margin
  `-10542.039` Mbps, and `distinct_values` margin `-3130.596` Mbps. One
  guard row also failed the floor: `instruments` margin `-1184.010` Mbps.
- The source patch was reverted after measurement and retained as
  `/tmp/skv14-W11V-string64-rejected.patch` with SHA-256
  `74bd6832bfc243e7a44ba6584ff316e44f8fccc99eb032dbec3b1f3c06ee163c`.
  Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11V-parse-only-string64.md`,
  `.tsv`, `.raw.log`, and baseline `.tsv` / `.raw.log`.
- Current JSON parse_only state remains 11 / 17 ADMITTED and 6 OPEN:
  `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
  `distinct_values`.

## SK-V14 W11W Parse-Only Memchr Trusted-String Split Admit

- W11W closes `G-SK-V14-W11W-JSON-PARSE-ONLY-MEMCHR` as `ADMIT`. The source
  patch lands, `skinny/RESULTS.md` moves the six remaining JSON `parse_only`
  rows, and `restart/skinny/ROLLING-SOTA-DELTA.md` records full JSON
  `parse_only` admission.
- The measured route changes the trusted plain-string scanner in
  `parse-that-regex`: `memchr2` finds the next quote or backslash, and a
  separate SWAR control-byte scan checks the exact prefix before that syntax
  byte. It preserves raw-control rejection and is materially distinct from
  REDRESS-247's custom 64-byte string-special scanner and REDRESS-246's
  structural-stream driver.
- Correctness gates passed before admission: `CARGO_TARGET_DIR=/tmp/skv14-w11w-regex-test-target
  RUSTC_WRAPPER= cargo test --manifest-path skinny/Cargo.toml --profile
  ax-iter -p parse-that-regex trusted_string -- --nocapture` and
  `CARGO_TARGET_DIR=/tmp/skv14-w11w-runtime-test-target RUSTC_WRAPPER= cargo
  test --manifest-path skinny/Cargo.toml --profile ax-iter -p runtime
  generated_parse_only_accepts_and_rejects_json -- --nocapture`.
- Cold release-native `profile_direct` evidence admits all six remaining
  rows against the same-run `sonic + 1.0` floor: `twitter` margin
  `3435.195` Mbps, `github_events` margin `3133.149` Mbps,
  `update_center` margin `962.732` Mbps, `random` margin `155.460` Mbps,
  `gsoc-2018` margin `1856.855` Mbps, and `distinct_values` margin
  `1920.426` Mbps. Previously admitted guard rows also stay above the same
  floor under the W11W binary: `canada` margin `822.009` Mbps, `instruments`
  margin `788.838` Mbps, `apache_builds` margin `2051.664` Mbps, and
  `citm_catalog` margin `1318.103` Mbps.
- Retained evidence:
  `restart/skinny/tranches/sk-v14/research/skv14-W11W-parse-only-memchr.md`,
  `.tsv`, and `.raw.log`.
- Current JSON parse_only state is 17 / 17 ADMITTED and 0 OPEN.

## SK-V15 W6 CSS L4 Typed Same-Workload Retime Reject

- Item 248 closes `G-SK-V15-W6-CSS-L4-TYPED-RETIME` as `ROUTE-W6-REJECT`.
  No CSS result row moves, no CSS floor is admitted, and every CSS L4 row
  remains `AUDIT-FALSIFIED` / `OPEN`.
- The measured route is materially distinct from REDRESS-215 and the W8R
  diagnostic broadcast: Track 1 is root `CssL4Parser::parse` producing a typed
  `CssDocument` plus `CssVisitor` traversal; Track 2 is a same-run
  `cssparser::StyleSheetParser` typed-summary walk over the same 979638-byte
  CSS corpus. Live W6 admission sources exclude W8R, `CSS_GENERATED_RS`,
  fact-stream output, `CssFullParseSummary`, `parse_full`, brace-counter proof,
  and `lightningcss`.
- The executable retime landed at `cec47b56e`. Evidence command:
  `SKV15_W6_REPORT_OUT=/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v15/research/w6/skv15-W6-css-typed-retime.json RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf --test css_l4_w6_typed_retime --release -- --nocapture`.
  The generated report SHA-256 is
  `31439e588849f557abf79e84ce35bf371e89c5b1c7467b01b5a271c88b0ba37e`.
- W6 rejects on both correctness and speed: Track 1 parses `2/4` corpus files,
  cssparser parses `4/4`, typed-summary equality is false, Track 1 measures
  `4.317` Mbps, cssparser measures `2051.911` Mbps, and the `cssparser + 1`
  margin is `-2048.594` Mbps. `admitted_rows=0`.
- The planned skinny `bbnf-bench` integration was not committed because adding
  the root typed parser crate to the skinny workspace causes a Cargo lockfile
  package-identity collision. The implementation moved to the root test
  surface where the typed CSS provider and direct `cssparser` dev-dependency
  already coexist. That is a routed correction, not a compatibility shim.
- W6 consumes `DEP-W6-CSS-GENERATED-RS`,
  `DEP-W6-CSS-SUMMARY-FACT-STREAM`, `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`,
  `DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM`, and re-attests
  `DEP-W1-CSS-BROADCAST` as diagnostic-only. W7 may proceed against the fresh
  measured rejection; any future CSS admission must be a new typed
  same-workload row.

## SK-V15 W7 Decision Engine Spine Admit

- Item 249 closes `G-SK-V15-W7-DECISION-SPINE` as `ADMIT-W7`. W7 consumes
  `DEP-W7-DECISION-SPINE` by replacing the zero-rule e-graph and tautological
  CSP record with an executable decision spine and generated-selection
  consumer.
- The e-graph route adds `NormalizeDirectSinkCost`, a scheduled rewrite that
  asserts an equivalent normalized direct-sink candidate only after
  `SinkOnly` with `DirectBuildNoConsumer` is already eligible. `ActiveCostFacts`
  now records `egraph_rewrite_count`, sourced from `RunReport.total_applied`.
- The CSP route removes the tautological parity predicate and makes capacity a
  falsifiable generic requirement. The W7 exact CSP test proves the same
  selected `OffsetTape` candidate is `sat` with `capacity_cost=0` and `unsat`
  with `capacity_cost=2`.
- Grammar-named generic decision records are retired from the live spine:
  `csp_named_grammars`, `static_css_provider_status`,
  `json_sink_only_status`, and the `JSON-CSS-W7-*` block id no longer exist in
  the `DecisionCspFacts` path.
- Required evidence passed:
  `cargo test --manifest-path skinny/Cargo.toml -p passes decision_egraph_rewrite_changes_selected_shape -- --exact`,
  `cargo test --manifest-path skinny/Cargo.toml -p passes decision_csp_rejects_missing_required_fact -- --exact`, and
  `cargo test --manifest-path skinny/Cargo.toml -p codegen decision_spine_changes_generated_selection_fixture -- --exact`.
  Broader `cargo test --manifest-path skinny/Cargo.toml -p passes` passed 13
  tests.
- Broader `cargo test --manifest-path skinny/Cargo.toml -p codegen` is still
  blocked by pre-existing dirty CSS generated runtime files:
  `tests::css_l4_generated_runtimes_reproducible_from_request` reports
  `DifferentFile("generated.rs")`. Those generated files are not part of W7's
  staged slice.

## SK-V15 W8 EagerTape OffsetTape Lowerer Admit

- Item 250 closes `G-SK-V15-W8-LOWERERS-A` as `ADMIT-W8`. W8 consumes
  `DEP-W8-LOWERERS-A` for EagerTape and OffsetTape only; EventTape,
  SinkOnly, CollapsedStage, and the all-five gate remain W9 scope.
- The W8 implementation replaces the old `rule X -> eager_tape` and
  `rule X -> offset_tape` format-string scaffolds with a shared
  `BackendExpr` operation-plan renderer. The renderer emits runtime-relevant
  operations for entry, sequence, alternation, repeats, optional branches,
  literals, regex spans, calls, span marks, tape emits, direct builds, value
  projection, and return.
- The new lowerer output names `ParserState` and `TapeBuilder` and includes
  shape-specific operations such as `eager_match_literal_hex`,
  `offset_match_literal_hex`, `capture_span_value`, `record_span_offsets`, and
  `ParserState::emit_plain_offset`. It is no longer a label string or
  pass-through shell.
- Required evidence passed:
  `cargo test --manifest-path skinny/Cargo.toml -p codegen backend_lowerer_fixture_rejects_label_string_scaffold -- --exact`,
  `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_eager_tape_emits_runtime_relevant_diff -- --exact`, and
  `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_offset_tape_emits_runtime_relevant_diff -- --exact`.
  The W7 guard `decision_spine_changes_generated_selection_fixture` also
  passed.
- Full codegen package testing remains blocked by the same pre-existing dirty
  CSS generated runtime files recorded in W7. Those files are not part of the
  W8 staged slice.

## SK-V15 W9 Remaining Lowerer All-Five Gate Admit

- Item 251 closes `G-SK-V15-W9-LOWERERS-B` as `ADMIT-W9`. W9 consumes
  `DEP-W9-LOWERERS-B` for EventTape, SinkOnly, CollapsedStage, and the
  all-five BackendShape report gate.
- EventTape and CollapsedStage now use the shared `BackendExpr`
  operation-plan renderer instead of `rule X -> event_tape` and
  `rule X -> collapsed_stage` label strings. EventTape emits
  `runtime_plan::EventTapeRule` over `ParserState+TapeBuilder+EventGrammar`;
  CollapsedStage emits `runtime_plan::CollapsedStageRule` over
  `ParserState+CollapsedStagePlan`.
- SinkOnly keeps the real `SinkOnlyProgram` compiled-runtime path and now
  marks the per-rule plan as `runtime_plan::SinkOnlyRule
  generated_runtime=JsonSink+DirectBuild`.
- `cargo xtask gate-json --check-results --skv15-backend-lowerers-report
  restart/skinny/tranches/sk-v15/research/w9/skv15-W9-backend-lowerers-report.json`
  consumes the exact five-shape canon and rejects missing or extra lowerers,
  non-implemented statuses, missing command evidence, and EventTape anti-sidecar
  fields that are not `absent`.
- Required evidence passed:
  `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_event_tape_emits_runtime_relevant_diff -- --exact`,
  `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_sink_only_emits_runtime_relevant_diff -- --exact`,
  `cargo test --manifest-path skinny/Cargo.toml -p codegen lower_collapsed_stage_emits_runtime_relevant_diff -- --exact`, and
  the W9 `gate-json` report command above. The broadened
  `backend_lowerer_fixture_rejects_label_string_scaffold` guard also passed.
- Product-code scans over `skinny/crates/codegen/src/lower` and EventTape
  anti-sidecar roots returned no scaffold, sidecar vector, retained parser
  stream, public substrate API, public `UnionTape`, alternate document
  projection, or sixth BackendShape hits. Test-only negative assertions remain
  in `skinny/crates/codegen/src/lib.rs`.
- Full codegen package testing remains blocked by the same pre-existing dirty
  CSS generated runtime files recorded in W7 and W8. Those files are not part
  of the W9 staged slice.

## SK-V15 W10 FNV Quarantine Admit

- Item 252 closes `G-SK-V15-W10-FNV-QUARANTINE` as `ADMIT-W10`. W10 consumes
  `DEP-W10-FNV-QUARANTINE` by making the W11L/W11N/W11O closed-enum/FNV
  evidence bench-only metadata, not a runtime selector, production arbiter, or
  correctness proof.
- The bench quarantine witness rejects a hash-equal typed-semantic mismatch and
  rejects any sidecar declared to share the same closed enum. Hash metadata is
  accepted only after all Track 1, Track 2, serde, and sonic typed checksums are
  already equal under an independent typed-semantic sidecar domain.
- `cargo xtask gate-json --check-results --skv15-fnv-quarantine-report
  restart/skinny/tranches/sk-v15/research/w10/skv15-W10-fnv-quarantine-report.json`
  consumes schema `sk-v15-fnv-quarantine-v1`, the exact six W11L/W11N/W11O
  row ids, adversarial fixture statuses, production migration block status, and
  non-absent production FNV scan classifications.
- Production scan evidence is non-empty by design:
  `crates/core/src/generate/regex/emit/dfa/accel.rs` uses an FNV-1a-style hash
  for compile-time DFA structure interning/canonical hashing; this is
  codegen-internal and not an admission or runtime-correctness proof.
  `skinny/crates/codegen/src/runtime_generator.rs` and the dirty
  `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` files emit
  `input_fnv64` as old CSS diagnostic fact-stream metadata, already outside live
  CSS admission after W6.
- Required evidence passed:
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine::tests::fnv_quarantine_rejects_matching_hash_with_mismatched_typed_semantics -- --exact`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine::tests::fnv_quarantine_rejects_shared_closed_enum_sidecar -- --exact`,
  `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench fnv_quarantine::tests::fnv_quarantine_report_accepts_bench_only_metadata -- --exact`,
  the W10 `gate-json` report command above, and
  `rg -n "fnv|FNV" crates/core/src/runtime crates/core/src/backend crates/core/src/generate skinny/crates/runtime/src skinny/crates/codegen/src`.

## SK-V15 W11 Close Reconciliation Admit

- Item 253 closes `G-SK-V15-W11-CLOSE-NO-ORPHANS` as `ADMIT-W11`. W11 consumes
  `DEP-W11-CLOSE-NO-ORPHANS` and records every SK-V15 dependency row as
  admitted, routed with REDRESS, or intrinsically blocked by row-level proof.
- PASS-IMPL V2 verdict is `ACCEPT-SK-V15-CLOSE-WITH-ROUTED-BLOCKS`, recorded in
  `restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md`.
- CSS L4 is not admitted. The W11 typed same-workload retime re-proves
  `admitted_rows=0`, Track 1 `2/4` parses, cssparser `4/4`, unequal typed
  summaries, Track 1 `3.426 Mbps`, cssparser `1995.168 Mbps`, and margin
  `-1992.742 Mbps`.
- Required W11 evidence passed: `cargo xtask check-json`,
  `cargo xtask gate-json --check-results`, W7 decision tests, W8/W9 lowerer
  tests, W9 all-five report gate, W10 FNV quarantine tests and report gate,
  the W6 typed CSS retime, lock count `16`, Pattern H count `67`, and the
  line-1 Pattern H provenance scan.
- Broad full-codegen and real-typed generated checks remain routed because of
  pre-existing dirty generated files outside W11 ownership. SK-V16 receives
  those as routed remainder after proof, not as substitute evidence for SK-V15
  close.
