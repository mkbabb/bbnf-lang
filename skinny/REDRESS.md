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
