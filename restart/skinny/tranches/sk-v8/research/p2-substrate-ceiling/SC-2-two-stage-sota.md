---
tranche: SK-V8
phase: P2 substrate-ceiling
artefact: SC-2
title: The two-stage SOTA architecture vs the bbnf single-pass offset-tape
date: 2026-05-17
scope: research-only; no code edits; no commits
lens: substrate-ceiling — does the offset-tape substrate impose an irreducible per-structural-element cost?
authority_inputs:
  - skinny/RESULTS.md (post SK-V7 W10c)
  - restart/skinny/tranches/sk-v7/research/skv7-A2-sota-strict-beat.md
  - restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md
  - restart/skinny/tranches/sk-v7/SYNTHESIS.md
  - skinny/crates/runtime/src/tape/{mod,assembler}.rs
  - skinny/crates/runtime/src/grammars/json/{scan,generated,parser}.rs
external_anchors:
  - "Langdale & Lemire, 'Parsing Gigabytes of JSON per Second', VLDB Journal 2019 (arXiv:1902.08318)"
  - "simdjson source: src/generic/stage1, src/generic/stage2, On Demand iterator"
  - "sonic-rs upstream README + skip-scan architecture notes"
  - "yyjson upstream README + yyjson.c read path"
  - "asmjson docs.rs + bbnf.asm:316-368 DPDA model"
---

# SC-2 — The Two-Stage SOTA Architecture

## §1. Findings

### 1.0 Source-anchor caution for comparator architecture claims

The upstream comparator summaries in this artefact are research architecture
summaries unless the claim is anchored to exact upstream source file/line
evidence here. In particular, sonic-rs, yyjson, asmjson, and simdjson claims
below should not be treated as admission evidence for bbnf unless they are tied
to exact source anchors in this artefact or later challenge work. They preserve
the prior S-P2/SK-V7 taxonomy as an external-anchor map, not as a substitute
for same-run strict comparator rows.

This caution matters most for sonic-rs: absent exact upstream source evidence
to the contrary, SC-2 treats sonic-rs as having no persistent document-wide
structural index. Its SIMD projection is described as locally re-derived and
consumed inside skip/parse operations, not as a retained index substrate.
asmjson's permissive fast path remains a flaw probe for what a register-resident
DPDA can avoid; it is not a strict-plane admission anchor.

### 1.1 The simdjson two-stage architecture, precisely

simdjson (Langdale & Lemire, "Parsing Gigabytes of JSON per Second",
arXiv:1902.08318, §3–§4) is explicitly and irreducibly **two stages**, and the
separation is the whole performance thesis.

**Stage 1 — the structural indexer.** Stage 1 reads the input exactly once,
64 bytes at a time (two 32-byte AVX2 loads, or one 64-byte AVX-512 load), and
produces *one output*: a flat array of `u32` indices, one per *structural or
pseudo-structural character*. The structural characters are `{ } [ ] : ,` and
the first character of every atom (string-opening `"`, and the leading byte of
each number/`true`/`false`/`null`). Stage 1 is, in order:

1. **Quote/escape state resolution** — a backslash mask is computed, a prefix
   problem (`escape_mask`) cancels escaped quotes, and a *carry-less prefix-XOR*
   (`_mm_clmulepi64_si128` against `0xFF...FF`) turns the surviving quote bitmap
   into an "inside-string" bitmap in one instruction. This is the paper's §4.2
   — the single most-cited trick.
2. **Character classification** — `vpshufb`/TBL lookups assign each byte to a
   class (whitespace, structural, other) using a 16-entry nibble table.
3. **Structural-bit identification** — structural bits = (classified-structural
   OR atom-leading) AND NOT inside-string.
4. **Bit-to-index extraction** — the per-block 64-bit structural bitmap is
   converted to indices by an *unrolled, branch-free* `tzcnt`/`blsr` loop that
   `compressstore`s 8 indices at a time regardless of how many bits are set
   (the paper §4.4: writing 8 and overwriting is faster than branching on
   popcount).

Stage 1 is **completely branch-free over data** — there is no `if` whose
direction depends on the document's content in the hot loop. Branch
mispredictions are the dominant cost of scalar JSON parsers (paper §2), and
stage 1's design goal is to spend zero of them. Its output, the structural
index, is *grammar-agnostic at the byte level*: it knows nothing about JSON
nesting; it only knows "these byte offsets are where something interesting
begins". UTF-8 validation runs as fused work in the same pass.

**Stage 2 — the tape builder.** Stage 2 walks the *structural index*, not the
input. It is a goto-threaded state machine (`object_begin`, `array_begin`,
`object_field`, etc.). At each step it reads the *next index* from stage 1,
dereferences the input only at that offset, dispatches on the single byte
there, and appends a 64-bit *tape word* (the simdjson tape: a tagged
`(type, value/payload)` 64-bit cell, with strings/numbers spilled into a side
buffer). Crucially, stage 2 **never scans** — it never executes an inner
character loop searching for a closing quote or a number's end, because stage 1
already told it where the *next* structural character is. The closing quote of
a string is simply "the next index". A number's extent is "this index to the
next index". Stage 2's only loop is *one iteration per structural element*.

**Why the separation buys throughput.** The paper's §6 measurement isolates it:
the two stages run at very different IPC and very different
branch-misprediction rates. Stage 1 is a wide, predictable, memory-streaming
kernel at ~3 GB/s+ with near-zero data-dependent branches. Stage 2 is a
narrow, pointer-chasing state machine — but because it is *fed indices*, its
inner work per element is O(1) and its branches dispatch on a tiny finite
alphabet of structural bytes. The conflated single-pass alternative would force
the predictable wide kernel and the unpredictable narrow machine to *share one
instruction stream and one branch-predictor budget* — every string body would
be re-walked by the same code that does structural dispatch, and the scan for
"where does this string end" would run in the slow, branchy plane. The
two-stage split is a **work-partitioning** that lets each plane run at its own
natural IPC. Stage 2's per-element cost is bounded and *independent of token
length*: a 2-byte string and a 2000-byte string both cost stage 2 exactly one
index-read + one tape-append. Token length is paid *once*, in the wide
branch-free stage-1 stream.

**On Demand.** simdjson On Demand (the lazy front-end) keeps stage 1 verbatim
and *replaces stage 2 with a forward-only iterator* over the same structural
index — values materialise only on access (`obj["field"]`), "values can only be
parsed once". This is the proof that stage 1's output is a *reusable
substrate*: the same index drives an eager tape build or a lazy iterator.

### 1.2 asmjson — SWAR + AVX-512, single-pass DPDA

asmjson (per `bbnf.asm:316-368` model and skv7-A2 §6, skv6-A1) is *not*
simdjson's index-then-walk shape. It is a **9-state DPDA** (deterministic
pushdown automaton) that fuses classification and structural emission into one
streaming pass. Its loop per 64-byte AVX-512BW chunk:

1. `vpcmpeqb` × N against the structural byte set, `korq`-reduced to a 64-bit
   class mask (`BYTE_CLASS_FROM_EQ_SET_64`).
2. `tzcnt`-driven seek to the next set bit (`BITMAP_NEXT_SET_BIT`), 18× per
   the AVX-512 instruction histogram.
3. PC-as-state direct threading: register `r10` holds the next-state target
   across chunk boundaries — *no state-variable memory traffic*
   (`FSM_DISPATCH_THREADED`).
4. An explicit bounded stack (`frames_buf[64]`, `open_buf[64]`) for
   bracket-pair tracking (`FRAME_PUSH_BOUNDED` / `FRAME_POP_BOUNDED`).

asmjson differs from simdjson in two ways that matter to this report. First,
**it does not produce a separate persistent structural index** — the mask is
consumed within the same chunk that produced it, and the DOM tape word is
emitted directly from the DPDA transition. It is "two-stage" only in the sense
that classification *precedes* emission *within a chunk*; there is no
materialised intermediate array spanning the document. Second, asmjson hits
10.93 GiB/s on *plain AVX-512BW + BMI1* — `vpcmpeqb`, `korq`, `tzcnt`, no
esoterica — and pays for it with **permissive strictness**: it treats every
byte `<0x20` as whitespace and never scans string bodies for unescaped control
characters (skv7-A2 §3). asmjson's design lesson is that the *DPDA-in-registers*
model collapses the stage-1/stage-2 boundary *without re-materialisation* — but
only because the per-element work is a register-resident state transition, not
a tape append through memory. asmjson does not lower to typed visitors;
generality is JSON-specific in the ASM source.

### 1.3 sonic-rs — single-pass with a skip-scan, no persistent index

sonic-rs (skv7-A2 §2–§3, upstream README) **explicitly rejects simdjson's
two-stage tape**. There is no materialised structural index that survives
between passes. sonic-rs's SIMD work is a set of *bounded skip-scan kernels*
invoked *on demand inside* a single recursive-descent / `serde::Deserialize`
driver: `skip_string`, `skip_number`, `get_from` use SIMD (`vpcmpeqb` + bitmask
+ `tzcnt`) to *jump* over a token in 16/32/64-byte strides, but the result is
consumed immediately by the value constructor — it is never written to an index
array. sonic-rs is therefore **single-pass with vectorised inner scans**: the
structural projection is *ephemeral, per-token, register-resident*. Its lazy
modes (`LazyValue`, `get_from` pointer access) skip-scan past unwanted subtrees
without ever building their tape. The architectural bet: a persistent
structural index is *memory traffic you can avoid* if your inner scans are fast
enough to be re-derived locally — and for product-shaped JSON (twitter, gsoc)
sonic-rs's bet wins, which is why sonic-rs is the strict comparator bbnf loses
to on exactly those rows.

### 1.4 yyjson — single-pass scalar, no SIMD, no index

yyjson (skv7-A2 §2–§3, upstream README) is single-pass C89 scalar with **no
explicit SIMD and no structural index whatsoever**. It walks the input one
byte at a time inside one force-inlined function that fits in ≤20 KiB i-cache,
appending typed-cell DOM nodes as it goes. `\uXXXX` decode is fused *into* the
string-walk loop — no function-call boundary. yyjson is the proof that a
*perfectly fused* single pass with disciplined i-cache footprint beats SIMD
when the SIMD implementation pays dispatch overhead or stage-boundary memory
traffic — on M5 Max twitter, yyjson at 3687 MiB/s beats simdjson DOM at
2923 MiB/s. yyjson does NOT two-stage; its lesson is the *opposite* of
simdjson's: total fusion of a narrow scalar machine. The two SOTA poles are
thus (a) simdjson — wide branch-free stage 1, fed-index stage 2; and
(b) yyjson — total scalar fusion. Both beat bbnf on string-heavy corpora.

### 1.5 What bbnf's offset-tape actually is, and what it conflates

bbnf's substrate is the `OffsetTape` (`runtime/src/tape/mod.rs`,
`tape/assembler.rs`): a `Vec<u32>` of offsets, sparse tape-internal
`(flag_cursor, flag_value)` facts for `HAS_ESC`/`HAS_CONTROL`, and an (empty in
every measured row) `PayloadArena`. The flag facts share the same producer and
cursor domain as the offsets and have no independent retained lifetime outside
the tape. RESULTS.md confirms the tape is genuinely offset-only: every row
reports `0 payload bytes`,
`0/0 writes/allocations`. So far this *looks* like a simdjson-style structural
index — it is, structurally, "an array of `u32`, one per structural element".

There is even a real stage-1 in the tree: `scan.rs::scan_structurals` is a
genuine branch-free-ish NEON structural indexer — it does the quote/escape
prefix-XOR (`prefix_xor_64`), the TBL classification
(`classify_block_from_table`), and `compact_mask` bit-to-index extraction.
**That kernel is a simdjson stage 1.**

The conflation — and this is the load-bearing finding — is that **the generated
recursive-descent parser does not consume the structural index that
`scan_structurals` produces.** `parser.rs:18` calls `structural_capacity_for`,
which calls `scan_structurals` *only to size the `Vec`* (`CapacityPlan::Exact`
and `OneShotSimd` literally throw the index away after counting it; the
production default `GrowOnly` doesn't even scan). The generated `parse_value_at`
(`generated.rs:30+`) then **re-walks the input byte-by-byte from scratch**:
`match_tiny_plain_string` and `match_string_at_quote` (`generated.rs:95,98,
147,151`) each run their *own* inner character loop searching for a string's
closing quote. This is precisely the inner scan that a fed-index stage 2 never
performs. bbnf is therefore *single-pass at the substrate level the way sonic-rs
and yyjson are* — but **without sonic-rs's vectorised skip-scans and without
yyjson's total i-cache fusion**. It pays the single-pass branchy inner-scan cost
of yyjson with the function-call-boundary overhead yyjson explicitly avoids
(`match_string_at_quote` is a cross-crate call into `parse-that-regex`,
`generated.rs:6,193,625`).

The offset-tape is then *built a third time*, conceptually: `TapeBuilder::
push_offset` appends an offset per structural element as the recursive-descent
parser discovers it. So bbnf's actual per-structural-element pipeline is:
**(a) scan_structurals produces an index that is discarded; (b) the recursive
descent parser re-discovers every structural position by branchy inner scan;
(c) each discovered position is appended to the offset-tape `Vec<u32>`.** The
index built in (a) is byte-identical in *content* to the offsets written in
(c). bbnf builds the structural projection twice and consumes it once, and the
consume happens in the slow plane.

## §2. Substrate-ceiling verdict

**Does the two-stage separation explain bbnf's string-plane losses? Yes —
decisively, and the mechanism is specific.**

The substrate-ceiling hypothesis (P2's framing) is that the offset-tape imposes
an irreducible per-structural-element cost. The two-stage lens *refines* that
hypothesis into something falsifiable and correct: the cost is not the
`Vec<u32>` of offsets itself (that is cheap — RESULTS.md shows tape sizes of
0.05x–0.50x input, and `push_plain_offset` is a branch-predicted unsafe write).
The irreducible cost is that **bbnf's structural projection is consumed in the
branchy recursive-descent plane instead of being produced once branch-free and
walked as indices.**

The evidence is exact and corpus-correlated:

- The SK-V7 cohort measured `match_string_at_quote` ≈ 47% + `match_tiny_plain_
  string` ≈ 28% ≈ **75% of total self-time** on the string-heavy rows
  (SYNTHESIS §3.4). Those two functions *are* the per-string inner scan that a
  fed-index stage 2 does not run. In simdjson, finding a string's end is "read
  the next index" — O(1), branch-free, already done in stage 1. In bbnf it is a
  byte loop with a data-dependent branch per byte.
- The losses cluster *exactly* on string/escape density. RESULTS.md: number-
  heavy corpora where strings are rare *win* — canada (+54.6% vs sonic strict,
  12 string quotes, 111126 numbers), mesh (+51.5%, 11 quotes), numbers, marine_ik.
  String-heavy corpora *lose* — twitter (-35.8%, 18099 quotes), update_center
  (-63.4%, 27229 quotes), distinct_values (-70.8%, 9796 quotes), apache_builds
  (-65.3%, 5289 quotes), y_string_unicode (-54.4%). The discriminant is the
  string-quote count. That is the *signature of a per-string inner-scan cost*,
  i.e. the signature of a missing stage-2-fed-index.
- SK-V7 rejected *every micro-kernel* (Eisel-Lemire, per-`\uXXXX` TBL, NEON
  plain-string scan, control/key compaction, PMULL/CSSC bodies) precisely
  because each one optimised *the inner scan* without removing it. A faster
  branchy inner scan is still a branchy inner scan in the same instruction
  stream as structural dispatch. The substrate ceiling is real, and it is the
  *absence of the stage-1/stage-2 cleavage*, not the offset-tape's data layout.

The verdict, sharply: bbnf already *has* a SOTA-grade stage 1
(`scan_structurals`). It throws the result away and re-derives the same
information in the slow plane. The "irreducible cost" is self-inflicted by
*not consuming the index that has already been built*. The substrate is not
the ceiling — the **missing index-driven stage 2** is the ceiling.

## §3. Candidate — tiered W3 union substrate

The user's framing — "a union of tape and structural projection" — remains a
falsifiable candidate, not a selected W3 prescription. SC-2 now splits that
candidate into two scopes so S-P3 cannot buy the narrow migration and claim the
larger string-plane close.

The architecture evidence is also split. **simdjson** is the retained,
document-wide stage-1 index precedent. **sonic-rs** is not used here as
persistent-index proof; absent exact upstream anchors, it remains a same-run
strict performance anchor plus a local skip-scan/single-pass comparator. Any
later stronger sonic-rs architecture claim must add exact upstream source
anchors before it can support this candidate.

### 3.1 Tier A — structural-class cursor migration

Tier A's candidate is narrow:

> **The structural index becomes the retained tape's structural-position and
> opaque structural-class cursor.**

Tier A promotes `scan_structurals` from a capacity-sizing throwaway to the one
producer of retained structural positions. The `Vec<u32>` of positions is the
tape's offset column, and an aligned `classes: Vec<u8>` column records opaque
structural-class ordinals. The scan product is move-consumed into the retained
`Tape`; no post-build `StructuralIndex` query API, sidecar, aux table, density
cache, parser-owned cursor, or parallel offset append path may survive.

Tier A may delete structural-byte rediscovery such as JSON `consume_structural`
and replace those calls with cursor/class reads. It **does not** delete
string-boundary work, does not stop re-walking strings, and does not close the
quote/backslash/parity plane. Existing measured parser validation for string
boundaries, UTF-8, escapes, and unescaped controls stays in the production row
unless Tier B is also implemented in the same accepted W3 slice. Transient masks
inside the scanner may remain implementation detail for structural-position
correctness; they are not retained string facts and cannot be counted as a
string-boundary consumer.

Tier A's same-wave production consumer is therefore the retained JSON parser
path that consumes structural positions/classes from the retained `Tape`.
Direct, `SinkOnly`, `path!`, retained view traversal, generated Track 1, and
Track 2 are not Tier A proof unless the W3 plan names exact owner paths and
same-wave verification for each touched path. Otherwise they are
touched/proven-untouched rows or Tier B/residual work, not Tier A consumers.

### 3.2 Tier B — string-boundary / parity / CostFacts-template union

Tier B is the larger string-plane candidate. It may claim string-boundary
closure only if it co-indexes quote/backslash/parity-derived bounds or facts
inside the singular retained `Tape`, rewrites the relevant string consumers in
the same wave, and keeps validation evidence in the measured strict row. A
view-boundary UTF-8 claim, post-parse check, stale sidecar, or telemetry-only
`tape_vs_tape` row cannot satisfy Tier B.

Tier B is also where `match_tiny_plain_string` /
`match_string_at_quote` deletion, direct/SinkOnly/path migration, and
`CostFacts` template parity belong unless a W3 plan proves they fit the
650-LOC template-parity cap and verification budget. The string-heavy loss
cluster (twitter, update_center, distinct_values, apache_builds,
github_events, the unicode rows) is the diagnostic target for Tier B, not proof
that Tier A has closed the string plane.

### 3.3 Tier A S-P3 owner, cost, and proof table

| Requirement | Tier A S-P3 challenge contract |
|---|---|
| Source owner files | `skinny/crates/bbnf-simd/src/lib.rs`, `skinny/crates/bbnf-simd/src/scalar/mod.rs`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`, `skinny/crates/runtime/src/tape/{mod,assembler,offsets}.rs`, `skinny/crates/runtime/src/grammars/json/{scan,generated,parser}.rs`, `skinny/crates/codegen/src/json_templates/{generated,parser}.rs`, and any regenerated JSON output named by the plan. |
| Source LOC budget | Tier A only: about +55 `bbnf-simd`, +90/-10 `runtime/src/tape`, -30 regenerated JSON parser output, +45 JSON codegen/table emission; net about +150 source LOC. Quote/backslash/parity masks, string-boundary consumers, and `CostFacts` template parity are Tier B and cannot spend this budget. |
| Generated-output audit | Regenerate rather than hand-patch `runtime/src/grammars/json/`. Review a byte diff for `scan.rs`, `generated.rs`, `parser.rs`, `view.rs`, and `value.rs`; any change outside structural-position/class consumption must be routed to Tier B or residual. |
| Strict row/plane targets | Same-run strict JSON parse plane only. Tier A must preserve strict validation fields in the measured row (`Strictness`, `parse_utf8`, `escape_complete`, `flaw_probe`) and must not rely on sidecar, view-boundary, post-parse, or stale comparator evidence. Candidate rows: twitter, update_center, distinct_values, apache_builds as structural/string-heavy diagnostics; canada, mesh, numbers as number-heavy maintain guards; all 38 current main rows keep the W3 maintain budget. |
| Same-wave production consumer | JSON retained parser consumes retained `Tape` positions/classes at `consume_structural` sites. `ValueRef::offset()` may be touched only to preserve cursor/offset semantics. No direct/SinkOnly/path claim counts unless separately owned and verified in the same wave. |
| Named parser tests | `cargo test --manifest-path skinny/Cargo.toml -p runtime`; `cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench parity`; and the plan's named generated parser tests for invalid escapes, C0 controls, block-boundary strings, nested arrays/objects, and trailing-data rejection. |
| Scalar oracle command | Add/extend a scalar oracle for positions plus classes, then run `cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --test classifier_parity` and `cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --test corpus_parity`. The oracle must produce the same positions/classes as SIMD on selected rows. |
| checkasm command | Add the named cell `checkasm_bbnf_simd_compact_mask_positions_classes`, then run `cargo test --manifest-path skinny/Cargo.toml -p bbnf-simd --profile ax-iter --test checkasm_parity`. If the primitive-checkasm wrapper is updated, also run `cargo run --manifest-path skinny/Cargo.toml -p xtask --release -- primitive-checkasm`. |
| Full-gate rerun budget | One full SK-V8 gate refresh: `cargo run --manifest-path skinny/Cargo.toml -p xtask --release -- gate-json` or the W0-updated equivalent. A second rerun requires a REDRESS cost note. |
| Revert slice | Revert `bbnf-simd`, `runtime/src/tape`, JSON templates/generated output, gate/report/RESULTS changes, and any named non-JSON proof edits as one W3 slice. Preserve rejected evidence under the W3 research directory. |
| Non-JSON proof | If any generic crate or generic template changes, the same Tier A plan must price CSS L4, Sheets, and BBNF-self proof per SPEC §2.1: no-op dry run, focused test, or unchanged-output audit proving no JSON structural roles are required to compile, lower, cost, or run. |

Tier A touched/proven-untouched rows:

| Row/path | Tier A status |
|---|---|
| Generated Track 1 retained parse | Touched. Owner paths are `runtime/src/grammars/json/{scan,generated,parser}.rs`, matching codegen templates, and `runtime/src/tape/`. This is the only Tier A production consumer. |
| Retained view / `ValueRef` | Touched or proven untouched. Owner paths are `runtime/src/tape/mod.rs`, `runtime/src/grammars/json/{view,value}.rs`, and matching templates. Verification must prove borrowed spans, offsets, and `DocumentView` traversal still use the measured retained `Tape`. |
| `path!` | Proven untouched unless S-P3 names an actual owner path/API. If touched, it must verify same-wave cursor semantics over the retained `Tape`; otherwise it is residual. |
| Direct / `SinkOnly` generated loops | Proven untouched for Tier A or routed to Tier B. Owner paths, if touched, include `runtime/src/grammars/json/generated.rs`, `runtime/src/grammars/json/sink.rs`, `codegen/src/sink_direct.rs`, and `codegen/src/lower/sink_only.rs`. |
| Generated Track 1 direct rows | Proven untouched unless the direct parser is explicitly in scope. Existing direct GO rows must maintain; they do not prove Tier A. |
| Independent Track 2 | Proven untouched. Owner paths are `bbnf-bench/src/track2/json.rs` and parity/gate code. Track 2 must remain structurally independent and cannot consume generated Track 1 or the new retained cursor. |

### 3.4 Shared falsifiability contract

- **Measured-path proof.** Validation and structural/tape facts must be emitted
  and consumed by the measured production row. Sidecar-only facts,
  view-boundary UTF-8, post-parse validation, stale C++ sidecars, or
  telemetry-only rows are guard evidence only.
- **Scalar reference expectation.** Tier A needs a scalar oracle for positions
  plus classes. Tier B additionally needs scalar parity for quote/backslash
  carries, string bounds, and any admitted tape-internal string facts.
- **checkasm expectation.** Any admitted SIMD primitive needs a named
  checkasm-style parity test before it can become a production gate.
- **Selection bar.** Both tiers remain unselected until an S-P3/W3 challenge
  supplies owner paths, revert protocol, numeric thresholds, strict same-run
  comparator planes, and accepted challenge evidence.

The predicted impact is tiered. Tier A should remove structural rediscovery
overhead and prove the one-producer/one-retained-`Tape` shape without claiming
the 75% string hot-leaf deletion. Tier B is the route that may remove the
string inner-scan leaf and challenge the string-heavy parse losses.

## §4. Generalisation — grammar-neutral

The union candidate is not JSON-specific when it is kept to byte sets, opaque
structural-class ordinals, generated per-grammar data, and one retained `Tape`.
Stage 1 still knows no grammar semantics; it only classifies bytes supplied by
generated grammar modules.

- **Tier A's grammar-neutral interface** is a structural byte alphabet plus
  opaque class ordinals (JSON: `{}[],:"`; CSS L4: `{}();:,` plus `/` `*` `@`;
  Sheets: `(),;` plus operators; BBNF-self: `|()[]{}` plus `::=`). Generic
  SIMD/runtime code consumes byte sets and ordinals; generated grammar modules
  interpret meaning from parser state plus class/byte. If Tier A edits generic
  crates, CSS L4, Sheets, and BBNF-self proof is part of Tier A's cost, not an
  optional later audit.

- **Tier A's stage-2 change** is structural cursor walking only. It replaces
  structural-byte rediscovery with cursor/class reads where the token boundary
  is already known by the retained tape. It does not replace string end scans,
  quote/backslash parity, escape validation, or grammar-specific recovery facts.

- **Tier B's grammar-neutral interface** is the string-boundary extension:
  generated string-like delimited-region sets, quote/backslash/parity facts, and
  `CostFacts` template parity. CSS strings/comments/url(), Sheets quoted text,
  and BBNF-self delimited spans are Tier B proof obligations if that route is
  challenged.

- **CSS L4 specifically.** Tier A may prove declaration-body structural cursor
  walking for CSS alphabets, but it cannot claim CSS string/comment/url()
  boundary closure or `@error(recover)` behavior without Tier B string facts and
  recovery/layout ownership.

- **The union is a framework-level candidate**, not a JSON hack, only under the
  singular-substrate invariant: every admitted grammar with a structural-byte
  alphabet gets one branch-free structural scan whose retained output is the
  tape. Typed/event/sink lowerings consume it only when their owner paths and
  same-wave verification are in the accepted W3 plan.

## §5. Risks

1. **Tier A can be oversold as Tier B.** Structural-class cursor migration does
   not remove `match_tiny_plain_string` / `match_string_at_quote` and therefore
   cannot claim the string-plane close. Mitigation: every W3 plan names Tier A
   and Tier B rows separately, and direct/SinkOnly/path claims are residual
   unless same-wave owner paths and tests are present.

2. **Stage-1 carry correctness across 64-byte blocks.** Tier A still depends on
   exact structural-position correctness outside strings. Tier B additionally
   depends on exact quote/backslash parity and string-boundary facts. `scan.rs`
   already has a `fast_path_is_strict` guard that falls back to
   `resolve_string_masks_64` scalar when the SIMD prefix-XOR path cannot prove
   strictness; that fallback must remain measured-row validation, not sidecar
   proof. Mitigation: differential parity (`scalar_parity_report` already
   exists) gating every selected corpus row.

3. **Index density vs cache.** A `Vec<u32>` index plus class column over a
   string-dense corpus is larger than over a number-dense one (RESULTS.md:
   gsoc-2018 tape 0.05x input, marine_ik 0.48x). For pathological
   all-structural input the index approaches 0.5x input size before the class
   column. simdjson accepts retained index traffic; bbnf must measure it, not
   assume it is free.

4. **The lazy-consume vs eager-tape decision is a Tier B cost-model fork.** For
   `real_typed_struct` rows that access *all* fields (mesh, marine_ik — every
   numeric required), lazy access has no skip-work win. Direct/SinkOnly and
   typed materialisation claims must be priced in Tier B or marked untouched in
   Tier A. Mitigation: `CostFacts` records access density per rule before any
   template route is selected.

5. **Refactor surface.** Tier A touches scanner, tape, codegen, and generated
   JSON parser output. Tier B touches string-rule lowering and
   `parse-that-regex` string matchers. Both are Lock-14-class changes.
   Mitigation: byte-identical generated-output diffing and SPEC §2.1
   non-JSON proof gate every generic edit.

6. **Not a micro-kernel — not selected here.** The SK-V7 failure mode was 14
   waves of micro-kernels that moved no production number. The tiered union is
   a substrate-change candidate; if S-P3/W3 challenges it, the plan must provide
   scalar oracle, checkasm parity, same-wave production consumer, and
   falsifiable strict-row gates. SC-2 does not select that wave.

## §6. Sources

External (primary):

- Langdale, G. & Lemire, D. "Parsing Gigabytes of JSON per Second." VLDB
  Journal 28(6), 2019. arXiv:1902.08318. §2 (branch-misprediction cost), §3
  (two-stage decomposition), §4.2 (carry-less prefix-XOR string-mask), §4.4
  (branch-free bit-to-index `compressstore`), §6 (per-stage IPC measurement).
- simdjson source architecture: `src/generic/stage1/` (structural indexer),
  `src/generic/stage2/` (tape builder), On Demand iterator
  (`include/simdjson/generic/ondemand/`) — forward-only, parse-once, stage-1
  index reused.
- simdjson On Demand 0.8.0 docs: `https://simdjson.org/api/0.8.0/md_doc_ondemand.html`
  — "values can only be parsed once"; lazy materialisation over stage-1 index.
- sonic-rs upstream README + skip-scan source — rejects two-stage tape;
  `skip_string`/`skip_number`/`get_from` ephemeral per-token SIMD scans;
  `LazyValue` lazy access.
- yyjson upstream README + `yyjson.c` read path — single-pass C89 scalar, no
  SIMD, no structural index, `\uXXXX` decode fused into string walk,
  ≤20 KiB i-cache discipline.
- asmjson docs.rs `https://docs.rs/asmjson/` — AVX-512BW + SWAR routing,
  10.93 GiB/s permissive single-thread DOM (Zen 4); 9-state DPDA model.
- RFC 8259 — strict JSON: four whitespace bytes; strings escape `"`, `\`,
  C0 controls U+0000–U+001F.

In-tree (authoritative):

- `skinny/RESULTS.md` (post SK-V7 W10c) — 17 parse-G rows; per-row string-quote
  / number counts; tape materialisation sizes; hot-leaf provenance.
- `restart/skinny/tranches/sk-v7/research/skv7-A2-sota-strict-beat.md` §2–§3,
  §5–§6 — per-parser architecture taxonomy; what each parser avoids.
- `restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md` §1, §5–§7
  — dav1d data-vs-code split; DPDA/CollapsedStage admissibility matrix;
  grammar-neutral primitive vocabulary.
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md` §3.3–§3.6 — string-scanner pair
  ≈75% self-time; twitter "win" is skip-work.
- `skinny/crates/runtime/src/tape/mod.rs`, `tape/assembler.rs` — `OffsetTape`
  substrate: `Vec<u32>` offsets + sparse tape facts + empty `PayloadArena`;
  `TapeBuilder::push_offset`.
- `skinny/crates/runtime/src/grammars/json/scan.rs` — `scan_structurals`: the
  existing branch-free NEON stage-1 structural indexer (prefix-XOR, TBL
  classify, `compact_mask`).
- `skinny/crates/runtime/src/grammars/json/generated.rs` (lines 6, 30, 95–98,
  147–151, 161–193, 409–625) — generated recursive-descent parser; inner-scan
  call sites `match_tiny_plain_string` / `match_string_at_quote`.
- `skinny/crates/runtime/src/grammars/json/parser.rs` (lines 5, 11, 18, 23) —
  `structural_capacity_for` uses `scan_structurals` for sizing only; the index
  is discarded.

End of SC-2.
