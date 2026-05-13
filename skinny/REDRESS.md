# Skinny Redress: Mbps, Tape Materialization, and Spec Amendments

Date: 2026-05-12.

This note records the implemented redress after the skinny prototype was brought
closer to the restart skinny/full contracts. The measured findings are now also
recorded in the runnable prototype surfaces under `skinny/`; the guarded
`restart/` authority surfaces remain outside this implementation pass.

## Current Bench Fact

The gate report is canonicalized to Mbps. Two facts must stay separate.

First, the original skinny triad passed: generated Track 1 and independent
hand-coded Track 2 both beat the sonic-rs anchor on twitter, citm_catalog, and
canada, and Track 1 stayed inside the Track 2 parity band. That validates the
lazy offset tape/direct projection substrate for the original skinny premise.

Second, the current measured authority is the expanded corpus in
`skinny/RESULTS.md`, which records **overall outcome G / NoGo**. Five rows are
currently binding: `github_events`, `update_center`, `random`,
`unicode_escapes`, and `y_string_unicode`. The expanded result does not refute
the tape/direct union; it exposes primitive and lowering gaps that the next
SOTA-BEAT packet must close.

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | Track 1 / sonic | Track 2 / sonic |
|---|---:|---:|---:|---:|---:|
| twitter | 23628 | 23242 | 21461 | 110.1% | 108.3% |
| citm_catalog | 30102 | 29969 | 25330 | 118.8% | 118.3% |
| canada | 16264 | 16217 | 13945 | 116.6% | 116.3% |

Structural scan is not the current blocker: the `canada` structural-only scan
reports 68896 Mbps against a 40000 Mbps floor.

Lazy tape materialization is now reported per corpus:

| Corpus | Offsets | Logical offset bytes | Flag bytes | Allocated tape bytes | Opens | Closes | String quotes | Numbers | Literals | Separators |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| twitter | 73362 | 293448 | 1560 | 295008 | 2314 | 2314 | 36198 | 2109 | 4737 | 25690 |
| citm_catalog | 162594 | 650376 | 5 | 650381 | 21388 | 21388 | 53208 | 14392 | 1263 | 50955 |
| canada | 334385 | 1337540 | 0 | 1337540 | 56049 | 56049 | 24 | 111126 | 0 | 111137 |

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
   corpus remains overall G / NoGo.

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
   G / NoGo classification.

   The compact report now renders all three competitor anchors, names the
   fastest `S` row used by the classifier, and reports Track 1 / S plus Track 2
   / S. SIMD scan benches persist parity-hash metadata, the gate rechecks the
   persisted hash against the scalar hash, peak RSS is sampled in one-shot
   subprocess probes for bbnf and the fastest `S` anchor, and `xtask
   check-conformance` exercises UTF-8 rejection, surrogate rejection,
   non-character acceptance, and float-bit parity over the expanded corpus
   manifest. This moved `bbnf-bench` and `xtask` above their old micro-budgets,
   so WORKSPACE.md redresses those local caps to 2,400 and 500 LOC while the
   total skinny remains far under the 31,400 handwritten ceiling.

27. SK-V3 reprofile split the expanded blockers by mechanism.

   `random` and `unicode_escapes` are dominated by
   `runtime::generated_json::generated::parse_value_at`, so the next parser
   work is typed event cursor consumption over the tape projection, not string
   decode or another tape-width perturbation. `update-center` spreads across
   parse entry, sparse-flag capacity, and allocation growth, so builder
   capacity policy is a measured SOTA item. Profiles live under
   `skinny/profile/reprofile-2026-05-12/`.

## Sonic Closeness

The parser now works as the tape/direct hybrid the spec requires and beats the
sonic-rs anchor on the original three measured rows. Twitter remains the
binding triad row and classifies as outcome A in that narrower gate: Track 1 is
110.1% of sonic-rs and Track 2 is 108.3% of sonic-rs. Citm_catalog and canada
also classify as outcome A.

The expanded corpus is stricter and is now authoritative for SOTA-BEAT. It
classifies as G / NoGo because the failing rows expose three remaining
families of work: object/key-dispatch and small-document overhead
(`github_events`, `update_center`, `random`), parse-only event-cursor overhead
on escape-heavy input (`unicode_escapes`), and Unicode string projection
(`y_string_unicode`).

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
  parser-split wins move the historical triad to pass. The expanded gate remains
  overall G / NoGo.
- The report now renders the actual fastest-anchor `S` comparator rather than
  only sonic-rs; conformance and SIMD parity metadata gates are executable.
- `bbnf-simd` is now the scanner crate used by runtime and bench, with
  byte-level and corpus parity tests under `crates/bbnf-simd/tests/`.
- Skinny and full specs now use the prototype workspace result path
  `skinny/RESULTS.md` for the runnable prototype, with `restart/skinny/` kept
  as spec authority.
- Original three-corpus gate: twitter outcome A / GO (Track 1 23628 Mbps,
  Track 2 23242, sonic-rs 21461); citm_catalog outcome A / GO (30102 / 29969 /
  25330); canada outcome A / GO (16264 / 16217 / 13945).
- Current expanded-corpus gate: `skinny/RESULTS.md` records overall G / NoGo,
  with `github_events`, `update_center`, `random`, `unicode_escapes`, and
  `y_string_unicode` as the binding misses.

## Closed Reporting Gates

- The compact report now includes the signed Track 2 checklist required by the
  long-form `BENCH.md` result template. The current report states that Track 2
  uses `runtime::tape::TapeBuilder`, shares the Track 1 parity oracle, and
  never calls `runtime::generated_json::parse`.
- Peak RSS is now measured through row metadata and rendered by the compact
  gate. The current report shows bbnf peak RSS below the fastest competitor on
  all three corpora: twitter 77,348,864 vs 92,798,976 bytes, citm_catalog
  155,566,080 vs 159,744,000 bytes, and canada 190,709,760 vs 195,067,904
  bytes.

## Next No-Workaround Work

1. Keep the rejected-route ledger intact: structural-index typed parser prepass,
   NEON no-escape string matcher, separator elision, generic SWAR whitespace,
   12-byte/width churn, and dispatch-table/function-pointer alternates remain
   non-canonical unless a future bench row overturns them.
2. Carry the original A / Go triad into V1 planning as JSON-class substrate
   validation inside the skinny bounds.
3. Carry the expanded G / NoGo into V1 planning as the current SOTA-BEAT block:
   string/Unicode primitives, random/key-dispatch overhead, and update-center
   shape overhead are now implementation requirements, not optional tuning.
