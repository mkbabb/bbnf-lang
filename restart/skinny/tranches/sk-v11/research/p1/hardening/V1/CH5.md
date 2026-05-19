# SK-V11 S-P1 V1 CH5: Hidden Coupling

Disposition: ACCEPT.
Date: 2026-05-19.
Scope: S-P1 P1-A through P1-F, W0 baseline, REDRESS W3/sidecar record, and
P1-cited source paths.
Output: this file.

## Findings

CH5 returns ACCEPT. The S-P1 V1 profile keeps the substrate union intact and
does not normalize a parallel substrate, sidecar producer, renamed scanner, or
Track 1/Track 2 equivalence claim.

1. No parallel substrate is normalized.

   P1-E names `structural_rediscovery` as a hot-leaf family, but immediately
   routes the tempting fixes to REDRESS 96, REDRESS 97, and REDRESS 98 and says
   class columns, sidecar vectors, streaming cursors, parser-owned cursors,
   event vectors, whitespace bitmaps, aux projections, structural-scan closes,
   and lazy-tape admissions are not allowed from this profile alone
   (`p1e-hot-leaf-attribution.md:220`). P1-C likewise records structural scan,
   masking probes, and lazy-tape facts as diagnostic only, with no behavior
   route proposed (`p1c-samply-mode-3.md:151`, `:164`, `:170`). P1-D says the
   high-c/B shape is work per byte and data movement, not evidence for a
   retained sidecar cursor (`p1d-pmu-cycles.md:204`).

   This matches REDRESS: event-cursor and parser-local structural-mask cursor
   routes are rejected (`skinny/REDRESS.md:742`, `:784`), the parser must not
   bolt on a second scanner (`skinny/REDRESS.md:807`), SK-V8 W3 made no row
   admission and did not reopen sidecar producers (`skinny/REDRESS.md:2669`),
   SK-V9 W3 class-column and streaming-cursor implementations both failed
   measured gates (`skinny/REDRESS.md:2797`, `:2852`), and REDRESS 98 retires
   `G-W3-UNION-SUBSTRATE` rather than leaving it as a renameable route
   (`skinny/REDRESS.md:2910`, `:2934`). SK-V10 W3 then firewall-closed the
   parse-only route with no behavior source or row movement
   (`skinny/REDRESS.md:3042`).

2. Track 1 and Track 2 remain separated.

   P1-B defines `T1` as generated Track 1 and `T2` as independent hand-coded
   Track 2 (`p1b-samply-mode-2.md:80`), and its hot-leaf map keeps generated
   direct symbols, hand Track 2 symbols, parse-that-regex primitives, and
   serde/oracle symbols distinct (`p1b-samply-mode-2.md:88`). P1-E does the
   same in its vocabulary table: generated tiny string, Track 2 parse tiny,
   direct hand tiny, generated typed string, and oracle/serde typed leaves are
   separate entries (`p1e-hot-leaf-attribution.md:76`). P1-E also explicitly
   says typed Track 2 symbols are oracle/comparator evidence, not generated
   product hot leaves (`p1e-hot-leaf-attribution.md:185`, `:280`).

   The cited source backs that separation. Generated direct Track 1 calls
   `runtime::generated_json::parse_direct`, while direct Track 2 calls the hand
   parser (`skinny/crates/bbnf-bench/src/direct_struct.rs:401`,
   `:408`). The generated parser source is marked generated and keeps
   `attach_structural_index` inert (`skinny/crates/runtime/src/grammars/json/generated.rs:1`,
   `:14`). The hand parse Track 2 parser is a separate recursive parser with
   its own `TapeBuilder` (`skinny/crates/bbnf-bench/src/track2/json.rs:10`,
   `:14`). Typed Track 1 dispatches through generated typed parsers, while typed
   Track 2 routes to `serde_typed` (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:355`,
   `:384`). This is not Track 1 == Track 2 dishonesty.

3. Generated runtime, hand paths, and oracle paths stay in their lanes.

   P1-B product-plane attribution stays inside existing generated direct parser,
   hand Track 2 parser, parse-that-regex primitives, digest folding, and typed
   direct parser, and says no observation reopens the W3 sidecar family
   (`p1b-samply-mode-2.md:266`). P1-D adds product PMU visibility for direct
   and typed rows but keeps PMU/cycles fenced from admission
   (`p1d-pmu-cycles.md:169`, `:233`). The source also preserves lane identity:
   `profile_direct` dispatches direct Track 1, direct Track 2, sonic, serde,
   and real-typed modes through distinct arms
   (`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:150`), and typed parity
   compares generated Track 1, Track 2/serde, serde, and sonic checksums rather
   than merging their profiles
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:449`).

4. W0-clamped rows are not admitted.

   W0 records `instruments`, `numbers`, and `unicode_mixed` as W0-clamped
   non-admissions and says W0 captures do not admit behavior rows
   (`W0-open-baseline.md:47`, `:54`). P1-B repeats that the rows remain
   planning evidence, not admissions (`p1b-samply-mode-2.md:123`, `:256`).
   P1-C keeps the same W0-clamped rows as `N-direct / NO-GO`
   (`p1c-samply-mode-3.md:139`, `:146`). P1-E isolates them in a separate
   W0-clamped table and says treating them as closed would be a paper close
   (`p1e-hot-leaf-attribution.md:157`, `:167`, `:286`). P1-F's extracted row
   table agrees (`p1f-results-delta.md:147`). No row is admitted by floor math
   alone.

5. Parse-only and sidecar evidence remain non-product.

   P1-A is explicitly parse-only diagnostic coverage and states no parse row is
   an SK-V11 SOTA target (`p1a-samply-mode-1.md:5`, `:75`). P1-C says Mode III
   probes and structural-scan rows are diagnostic and not product admissions
   (`p1c-samply-mode-3.md:53`, `:85`). P1-F extracts `canada/parse_only` as
   `L / NO-GO`, treats `S` as a diagnostic enum rather than admission, records
   historical or absent sidecars as planning signals only, and confirms
   structural scan, masking probes, PMU, and cycles are uniform nonproducers
   (`p1f-results-delta.md:49`, `:59`, `:186`, `:198`). This aligns with the W0
   baseline, which says parse-only remains diagnostic and cannot become a SOTA
   close target (`W0-open-baseline.md:84`), and with the SK-V8 comparator rules
   that stale sidecars and sidecar-only evidence are guard telemetry only
   (`restart/skinny/tranches/sk-v8/SPEC.md:65`).

## Required Fold

No CH5 fold is required for S-P1 V2. Carry forward the existing cautions:
structural-scan-only, masking probes, PMU/cycles, sidecar freshness, lazy-tape
facts, parse-only rows, and W0-clamped direct rows are evidence for later
planning, not behavior producers or admissions.
