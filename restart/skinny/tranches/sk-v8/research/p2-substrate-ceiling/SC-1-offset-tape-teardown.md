---
agent: SC-1
lens: substrate-ceiling
generated_at: 2026-05-17
files_audited_count: 9
---

# SC-1 — Offset-Tape Substrate Teardown

Scope: tear down the offset-tape substrate, quantify its per-structural-element
cost, and answer whether the tape is the `parse_only` throughput ceiling.

Files audited end-to-end: `tape/mod.rs`, `tape/assembler.rs`, `tape/offsets.rs`,
`codegen/src/lower/{offset_tape,event_tape,eager_tape}.rs`,
`grammars/json/{generated.rs,parser.rs,scan.rs,view.rs}`, `skinny/RESULTS.md`,
plus cross-reference to `restart/skinny/SUBSTRATE.md` and SK-V5 A4 tape-union
audit.

---

## §1 Findings

### 1.1 The exact tape data layout

The implemented substrate is **not** the 16-byte `TapeToken` described in
`SUBSTRATE.md` §1.1. That `TapeToken` is dead spec text — SK-V5 A4 §4 confirms
"No `TapeToken` type lives in the implementation". The live substrate is three
parallel arrays plus an empty arena (`tape/mod.rs:90-97`):

```rust
pub struct Tape<'input> {
    source: &'input [u8],   // borrowed, zero-cost
    offsets: Vec<u32>,      // ONE u32 per structural element  <-- the tape
    flag_cursors: Vec<u32>, // sparse: only escaped/control strings
    flag_values: Vec<u8>,   // sparse: paired with flag_cursors
    payloads: PayloadArena, // EMPTY on the JSON hot path (0 writes, all corpora)
}
```

**Per-structural-element cost — bytes written.** Every structural element costs
exactly **4 bytes** in the dense `offsets` array. There is no retained
`StructuralClass` ordinal, no span, no payload slot. One `u32` source-byte
offset is the entire record
(`assembler.rs:71-85`, `push_plain_offset`). The element vocabulary and what
each writes:

| Element            | Offsets written | Emit site (generated.rs) |
|--------------------|-----------------|--------------------------|
| object open `{`    | 1 × u32         | `consume_structural` :303 via `parse_object` :63 |
| array open `[`     | 1 × u32         | `consume_structural` :303 via `parse_array` :122 |
| object/array close | 1 × u32         | `consume_container_next` :334 / `consume_array_next` :374 |
| string quote `"`   | 1 × u32         | `consume_quote_at_cursor` :268 (the OPEN quote only) |
| number             | 1 × u32         | `parse_number` :209 `emit_plain_offset(number.start)` |
| literal true/false/null | 1 × u32    | `parse_literal` :233 `emit_plain_offset(start)` |
| `,` and `:` separators | **0**       | consumed by `skip_ws`/`consume_delimiter`, never emitted |

RESULTS.md `separators 0` on every corpus confirms separators are not taped.
Closes ARE emitted (RESULTS.md `closes` column non-zero everywhere) despite
`SUBSTRATE.md` §1.5 claiming "close-token elision" — the spec is stale; the
implementation emits a close offset.

**Per-element cost — instructions retired.** `push_plain_offset`
(`assembler.rs:70-85`) on the hot path (capacity not exhausted) is: one load of
`offsets.len()`, one compare against `offsets.capacity()`, one predicted-
not-taken branch, one `ptr.add(len)`, one `u32` store, one `set_len(len+1)`.
~5–7 retired instructions + one store to a cache line that grows monotonically.
The `checked_u32` debug-assert (`mod.rs:228-231`) is compiled out in release.
This is genuinely cheap **in isolation** — but see §1.3 for why it is not in
isolation.

**Flag patching** (`assembler.rs:93-113`, `patch_flags`): only fires for strings
that `needs_decode()` (escape/control). For an escaped string it pushes a `u32`
cursor + a `u8` value (5 bytes) into the sparse side arrays. For plain strings
it is a single `flags.bits() != 0` test that early-returns
(`generated.rs:100,153` only call it inside the `needs_decode()` branch). On
escape-heavy corpora this is real cost: gsoc-2018 8545 flag bytes, unicode_mixed
9795, unicode_escapes 9385, y_string_unicode 9000 (RESULTS.md Notes).

**Per-element READ cost (the hidden tax).** Because the tape stores a bare
`u32` with **no retained `StructuralClass` ordinal**, every consumer must
re-derive the structural role by reading `source[offsets[cursor]]`.
`JsonNodeKind::at_cursor` does exactly this for the current JSON instance;
`view.rs` calls it 6+ times per traversal step
(`view.rs:313,320,335,355,360,365,394,417`). `next_sibling_cursor`
(`view.rs:354-381`) does an O(subtree) depth-counted walk re-classifying every
offset to skip a container — there is no sibling-skip cache (the
`payload_or_skip` field in `SUBSTRATE.md` §1.1 does not exist). The tape is
write-cheap but **read-expensive**: it defers all structural meaning to a second
source-byte fetch. The neutral form of this finding is governed by SC-3/SC-6:
the retained identity, if any, is an opaque generated `StructuralClass`
ordinal, not JSON-only structural semantics.

### 1.2 Write amplification vs win/loss correlation

RESULTS.md Notes give, per corpus: offsets count, logical offset bytes, sparse
flag bytes, allocated tape bytes, and the allocated/input ratio. Cross-tabulated
against the available comparator evidence. Only same-run strict anchors can
support a strict admission claim; historical SK-V6 rows, no-anchor rows, lossy
rows, and sidecar/permissive rows are planning signals only.

| Corpus          | alloc tape / input | evidence class | observed delta / signal | posture |
|-----------------|-------------------:|----------------|-------------------------:|---------|
| canada          | 0.47x              | same-run sonic strict | **+54.6%** | strict win evidence |
| mesh            | 0.72x              | same-run sonic strict | **+51.5%** | strict win evidence |
| numbers         | 0.44x              | historical SK-V6; no same-run strict anchor | +51% vs SK-V6 | planning signal, not strict evidence |
| marine_ik       | 0.70x              | historical SK-V6; no same-run strict anchor | +37% vs SK-V6 | planning signal, not strict evidence |
| citm_catalog    | 0.30x              | same-run sonic strict | -11.3% | strict residual |
| instruments     | 0.30x              | no strict anchor | +10.6% | planning signal, not strict evidence |
| twitter         | 0.21x              | same-run sonic strict | -25.1% | strict loss |
| update_center   | 0.49x              | same-run sonic strict | -63.4% | strict loss |
| apache_builds   | 0.26x              | same-run sonic strict | -65.3% | strict loss |
| github_events   | 0.25x              | same-run sonic strict | -61.7% | strict loss |
| gsoc-2018       | **0.08x**          | historical SK comparison | -53.3% vs SK | planning loss signal |
| unicode_escapes | **0.07x**          | lossy/permissive sidecar plus historical SK | +113% lossy / -34% SK | planning signal, not strict evidence |
| distinct_values | 0.43x              | same-run sonic strict | -70.8% | strict loss |
| y_string_unicode| 0.75x              | same-run sonic strict | -54.4% | strict loss |

**The correlation is the opposite of the hypothesis.** High tape-byte ratio
does NOT predict a loss. canada (0.47x) and mesh (0.72x) are strict wins;
marine_ik (0.70x) and numbers (0.44x) are high-ratio historical planning win
signals. gsoc-2018 (0.08x) and unicode_escapes (0.07x) are the *lowest*
tape-byte producers and planning loss signals. y_string_unicode is the single
highest ratio (0.75x) and a -54% strict loss, but its absolute offset count is
tiny (2202 offsets) and its 9000 flag bytes signal an all-escaped-string
corpus.

Within JSON telemetry, the discriminator that separates the same-run strict
wins and losses is **string quote-count share**, not tape bytes. canada: 12
string quotes / 223236 offsets. mesh: 11 / 80250. twitter: 18099 / 29573
(~61%). update_center: 27229 / 35281 (~77%). distinct_values: 9796 / 11118
(~88%). Historical/no-anchor rows such as numbers, marine_ik, gsoc-2018,
unicode_escapes, and instruments can support planning hypotheses, but not
strict admission. The JSON quote-count observation must travel as per-grammar
`RecognizerFacts`/`CostFacts` telemetry, not as a generic density selector
policy. The measured loss is in the string scanner pair (SK-V7 §3.4:
`match_string_at_quote` ~47% + `match_tiny_plain_string` ~28% = ~75%
self-time), not the offset write.

**Verdict on §1.2: the offset write is not the amplifier.** A `u32` push per
element is ~1 byte-of-store-traffic per ~4–10 input bytes — far below the input
read bandwidth. The tape-byte ratio tracks structural density (number-heavy
corpora have many cheap numeric offsets and show strict or planning win
signals); it does not track throughput.

### 1.3 Is the tape build fused into the structural scan?

**No — and this is the substrate's actual defect.** There are two independent
structural passes and they are NOT fused:

1. **The SIMD scan** (`scan.rs:22-30`, `scan_structurals` → NEON `neon::scan`
   at `scan.rs:207-275`). This is a full-input NEON pass that classifies the
   `{}[],:"` alphabet with `vqtbl` + prefix-XOR string-masking and emits a
   `Vec<u32>` of every structural position. It exists, it is fast (RESULTS.md
   canada "structural scan: 69075 Mbps", floor 40000).

2. **The recursive-descent parse** (`generated.rs:20-378`). `parse_value_at`
   (:37-43) reads `state.bytes[state.cursor]` directly, dispatches, and walks
   source bytes a *second time* through `skip_ws`, `match_tiny_plain_string`,
   `match_number_span`, `consume_structural`, etc. It writes the tape itself via
   `emit_plain_offset` as it goes.

**`attach_structural_index` is a no-op** (`generated.rs:14-17`: `let _ = state;`).
The SIMD scan's output is **never consumed by the parser.** The scan is invoked
only by the bench's structural-floor probe and by `structural_capacity_for`
under the non-default `CapacityPlan::OneShotSimd` (`scan.rs:51`); the production
`CapacityPlan::GrowOnly` default (`assembler.rs:27`) doesn't even run it.

So the tape is built by the recursive-descent pass as a **separate, second
structural traversal of the input**. Cost of the non-fusion:

- The input is structurally classified twice if the SIMD scan ran (it doesn't,
  on the default plan — so the scan investment is simply wasted), OR
- The parser does all structural classification itself byte-at-a-time with no
  SIMD, which is what actually happens on the default `GrowOnly` plan.

SK-V5 A4 §3.4 names this exactly: "the scan runs, its product is never consulted
along the canonical path... missing-consumer of the scan output." `SUBSTRATE.md`
§1.5 admits it too: "generated parse functions still walk source bytes through
`cursor`, `skip_ws`, and `parse_value_at`."

The empirical anchor (`SUBSTRATE.md` §1.6) is decisive: simdjson's stage-2
`advance()` is `&buf[*next_structural++]` — a single u32-indexed pointer add per
dispatch; whitespace and delimiters are never re-scanned. bbnf's generated
parser re-scans whitespace (`skip_ws` :240, called ~once per element) and
re-classifies every byte. The tape *write* is cheap; the **second structural
traversal that produces it** is not.

### 1.4 Verdict — is the offset-tape an irreducible ceiling?

**No. The offset-tape data structure is not the ceiling. The non-fusion of
tape-build into the structural scan, plus the absence of a retained
`StructuralClass` ordinal, is the cost — and both are removable.**

Evidence the tape itself is not the ceiling:

- The two same-run strict `parse_only` wins (canada +54.6%, mesh +51.5%) and
  the two historical SK-V6 planning win signals (numbers, marine_ik) all run
  the *same* `push_plain_offset` path and all produce high tape-byte ratios. If
  the offset write were the ceiling these would be the first rows to regress.
- Payload arena is provably zero-cost: `0/0 writes/allocations` on every corpus
  (RESULTS.md Notes). The arena is not in the picture at all.
- SK-V7's six rejected micro-kernels (W2/W4/W5/W6/W10/W10b) were all *inside*
  the string/number scanners — none touched the tape write. Their rejection is
  evidence the scanner is the bottleneck, and silent on the tape.
- The ~75% self-time SK-V7 §3.4 attributes to `match_string_at_quote` +
  `match_tiny_plain_string` is string *content* scanning, not offset emission.

Evidence of removable headroom a kernel never reached:

- The SIMD structural scan already exists and already hits 69075 Mbps on canada
  but its output is discarded (`attach_structural_index` no-op). Wiring the
  parser to consume `scan` offsets instead of re-walking source is a
  *substrate-consumption* refactor, not a kernel. No SK-V7 wave attempted it
  (all six waves were kernels inside the existing recursive-descent body).
- The class-free `u32` offset forces every read-side consumer to re-fetch
  `source[offset]` (`view.rs` `at_cursor`). A scan-written
  `StructuralClass` ordinal, stored as a neutral co-indexed tape column or
  otherwise under the SC-3/SC-6 tape-union design, would remove the re-fetch.
  Not a kernel; an encoding and ownership change.

So the answer to the user's framing ("Is our tape design flawed? union of tape
and structural projection"): **the tape design's flaw is precisely that it is
NOT unioned with the structural projection.** SUBSTRATE.md §1.5 *claims* "the
structural projection IS the tape's storage" — but the implementation has a SIMD
scan producing one structural projection that is thrown away, and a recursive-
descent parser producing a *second* offset projection that becomes the tape. The
union the user asks for is the missing fusion, not a new data structure.

The honest residual ceiling is the **string scanner pair** (SK-V7 §3.4) — but
that is downstream of this finding: even a perfect string scanner still leaves
the double structural traversal in place. The two are additive.

---

## §2 Substrate-ceiling verdict

**The offset-tape does NOT cap `parse_only` throughput.** Three independent
lines of evidence:

1. **Win/loss anti-correlation.** The corpora with high tape-byte amplification
   include same-run strict wins (canada 0.47x, mesh 0.72x) and historical SK-V6
   planning win signals (marine_ik 0.70x, numbers 0.44x). If the tape write
   were the ceiling, write-amplification would predict loss. It predicts the
   opposite. For JSON, the useful telemetry is string quote-count share, which
   drives the string scanner, not the tape. That telemetry is grammar-specific
   `RecognizerFacts`/`CostFacts`, not generic selector policy.

2. **The write path is genuinely cheap.** `push_plain_offset` is ~5–7 retired
   instructions and one `u32` store to a sequentially-growing buffer; the arena
   is provably untouched (`0/0` everywhere). At ~4 bytes of tape per ~4–10 input
   bytes, offset-store traffic is a small fraction of the mandatory input read.

3. **The real cost is structural — but it is the *non-fusion*, not the
   *structure*.** The substrate runs the input through structural classification
   twice: a wasted SIMD scan (`attach_structural_index` is a no-op,
   `generated.rs:14-17`) plus a full byte-at-a-time recursive-descent re-walk
   that also re-scans whitespace (`skip_ws`). simdjson's stage-2 does one
   `u32`-indexed pointer-add per element and never re-scans
   (`SUBSTRATE.md` §1.6). bbnf re-derives every structural decision in the
   parser. That gap is a substrate-consumption defect, removable without
   touching the tape's bytes.

**Headroom a kernel never reached:** the existing 69 Gbps SIMD scan output is
discarded. Wiring the generated parser to dispatch off the scan's offset stream
(the genuine "tape ≡ structural projection" union SUBSTRATE.md §1.5 only claims
to have) is the unexploited lever. All six SK-V7 micro-kernels operated inside
the recursive-descent body and so could never reach it.

---

## §3 Candidate Research Recommendation

This section records a candidate research direction only. It does **not** select
an SK-V8 implementation wave, prescribe W3, or close the planning question.
S-P3/W3 must supply exact owner paths, same-wave production consumer, revert
protocol, numeric measurement thresholds, and accepted challenge proof before
any implementation dispatch. The posture remains one producer, one retained
`Tape`, no parser-owned cursor/facts sidecar, and no new BBNF directive, BIR
variant, `BackendShape` variant, public substrate type, or independent substrate
node.

Candidate substrate direction for later challenge:

1. **Tier A: structural-class cursor migration.** Make
   `attach_structural_index` actually attach the `scan_structurals` offset
   stream, and rewrite the generated `consume_*` / `dispatch_value` family so
   structural dispatch is a `u32`-indexed advance over the scan offsets
   (`buf[offsets[i++]]` pattern), not a source-byte re-walk with `skip_ws`.
   Tier A only claims structural cursor migration: parser dispatch stops
   rediscovering structural bytes and whitespace/delimiter boundaries. It does
   not claim string-boundary closure, reusable quote/backslash parity facts,
   string-content skipping, or CostFacts-template selection.

2. **Tier A: retain scan-written structural identity in neutral form.** Store
   an opaque generated `StructuralClass` ordinal alongside the offset, governed
   by SC-3/SC-6 rather than JSON-only structural semantics. The preferred
   candidate shape is a co-indexed representation inside the retained `Tape`
   that preserves the dense `u32` offset lane and keeps the identity
   scan-written, not parser-patched. This removes the `source[offset]` re-fetch
   in every `at_cursor` call on the read side (`view.rs`) and lets
   `next_sibling_cursor` skip containers without re-classifying. This is the
   missing retained-view capability that `SUBSTRATE.md` §1.1 specs but the
   implementation never built; it is not a new substrate surface.

3. **Tier B: string-boundary / quote-backslash-parity /
   CostFacts-template union.** Any reusable string-boundary facts, quote and
   backslash parity masks, template-level CostFacts, or grammar recognizer
   selection belong to a separate Tier B proof. Tier B must name its own owner
   paths, LOC/cap budget, per-plane gates, and same-run strict rows. Until that
   proof exists, JSON quote-count share remains diagnostic
   `RecognizerFacts`/`CostFacts` telemetry only.

4. **Sequence after, not instead of, the string-scanner work.** The string
   scanner pair (SK-V7 §3.4, ~75% self-time on loss corpora) is the larger
   single number. Tier A fusion and Tier B/string-scanner work are additive:
   neither subsumes the other. Tier A is only a lead hypothesis because it is a
   substrate-consumption refactor with a measurable falsifiable gate (parser
   stops calling `skip_ws` for structural dispatch; structural dispatch becomes
   a pointer-add) and it un-wastes an already-built 69 Gbps kernel.

**Tier A W3 gate table template for any future fusion candidate:** W0 has not
run, so profile artifact, hot leaf, owner file, and numeric threshold cells are
post-W0 required and intentionally unfilled here. S-P3/W3 must complete them
before selection. The table is candidate-plane telemetry; it is not SOTA
admission and does not count `tape_vs_tape` or any other telemetry-only row as a
production same-wave consumer.

| row | workload | candidate plane | strict comparator source | baseline Track 1 Mbps | baseline Track 2 Mbps | threshold | maintain budget | profile artifact | hot leaf | owner file | pass/fail rule |
|---:|---|---|---|---:|---:|---|---|---|---|---|---|
| 1 | canada | `parse_only` candidate telemetry | same-run strict anchor required post-W0 | post-W0 required | post-W0 required | post-W0 required | no regression budget to be set by S-P3/W3 | post-W0 required | post-W0 required | post-W0 required | pass only if Track 1/2 maintain budget and profile artifact prove the structural-dispatch leaf is reduced under the filled numeric threshold |
| 2 | mesh | `parse_only` candidate telemetry | same-run strict anchor required post-W0 | post-W0 required | post-W0 required | post-W0 required | no regression budget to be set by S-P3/W3 | post-W0 required | post-W0 required | post-W0 required | pass only if Track 1/2 maintain budget and profile artifact prove the structural-dispatch leaf is reduced under the filled numeric threshold |
| 3 | numbers | `parse_only` candidate telemetry | same-run strict anchor required post-W0 | post-W0 required | post-W0 required | post-W0 required | no regression budget to be set by S-P3/W3 | post-W0 required | post-W0 required | post-W0 required | pass only if Track 1/2 maintain budget and profile artifact prove no number-heavy regression under the filled numeric threshold |
| 4 | twitter | `parse_only` candidate telemetry | same-run strict anchor required post-W0 | post-W0 required | post-W0 required | post-W0 required | no regression budget to be set by S-P3/W3 | post-W0 required | post-W0 required | post-W0 required | pass only if residual profile mass is assigned to the string scanner or another named leaf, not to unowned structural re-walk |
| 5 | update_center | `parse_only` candidate telemetry | same-run strict anchor required post-W0 | post-W0 required | post-W0 required | post-W0 required | no regression budget to be set by S-P3/W3 | post-W0 required | post-W0 required | post-W0 required | pass only if residual profile mass is assigned to the string scanner or another named leaf, not to unowned structural re-walk |

**Do NOT** redesign the tape into a wide `TapeToken`. SK-V5 redress items 16/17/
18/50/53 already measured and rejected wider tokens, dense aux columns, sparse
aux side tables, and event-cursor prepasses. The encoding stays `u32`-dense; the
only admitted candidate addition is a scan-written opaque `StructuralClass`
ordinal column under SC-3/SC-6 as representation replacement inside retained
`Tape`, and the only structural change is *who produces it* (the SIMD scan, not
a second recursive-descent traversal).

---

## §4 Generalisation — does this hold grammar-neutrally?

The finding generalises, with one important caveat per grammar class.

**Holds for any grammar with a SIMD-classifiable structural alphabet.** The
substrate (`Tape<'input>` = dense `u32` offsets + sparse flags + arena) is
grammar-neutral by construction (`SUBSTRATE.md` §0). The non-fusion defect is
also grammar-neutral: the codegen lowerers `offset_tape.rs`, `event_tape.rs`,
`eager_tape.rs` are all 17-line stubs that emit `format!("rule {} ->
offset_tape")` — i.e. **no shape-specific lowering exists at all yet**
(`codegen/src/lower/*.rs`). Tier A generalises only as structural-class cursor
migration: the scan produces the offset projection plus opaque generated class
ordinals, and generated grammar code dispatches from that cursor instead of
re-walking source bytes. CSS L4's structural alphabet (`{};:()` + selectors)
and Sheets' formula tokens can use the same neutral byte-set + opaque-ordinal
shape, with event meaning interpreted inside the generated grammar module by
parser state plus byte/class. No generic JSON role, grammar-name branch, public
grammar API, or directive is admitted.

**Caveat — context-sensitive / recursive grammars.** A grammar whose structural
meaning is not one-byte-decidable from `source[offset]` (e.g. CSS where `{` is a
block open vs. a declaration depends on selector context, or Sheets formulas
with host-function chains) cannot dispatch purely off a flat offset stream;
those are exactly the grammars `SUBSTRATE.md` §1.6 routes to retained
event-shaped representations rather than bare `OffsetTape`. For them the
recommendation is the same in shape (consume the scan, do not re-walk) but the
representation must carry opaque generated `StructuralClass` ordinals or closed
neutral facts, not JSON roles and not parser-owned sidecar facts. If Tier A
cannot satisfy that invariant for a grammar class, the union candidate is
rejected or routed to a separate S-P3 proof; it cannot keep an old
`OffsetTape`-via-recursive-descent producer beside the retained-index design.

**Does NOT generalise:** the specific win/loss correlation (string quote-count
share drives JSON rows) is JSON-corpus-specific telemetry. For CSS the
analogous facts might be unquoted-token/comment share; for Sheets,
string-literal and host-call share. Those variables belong in per-grammar
`RecognizerFacts`/`CostFacts` with per-plane gates. The method generalises
(the tape-byte ratio is not the predictor; content-scan telemetry is), but the
specific predictor variable does not.

---

## §5 Risks — REDRESS entries to pre-block

| # | Risk | Pre-block |
|---|------|-----------|
| R1 | Fusion wave re-introduces a parallel prepass (`generated_eventcursor.rs` shape, SK-V5 A4 §3.2) — a scan that runs upfront in front of an unchanged recursive-descent body. SK-V4 §4 already refuted this; it regressed and grew the hot hub. | The fused cursor MUST be the dispatch boundary *inside* the generated body (`buf[offsets[i++]]` per `consume_*` site), not a `ParseIndexCursor`-style sidecar attached before `parse_value`. Gate: `grep` the generated parser for any retained `StructuralIndex`/`Vec<JsonEvent>` field on `ParserState` — must be zero, and no parser-owned cursor/facts field may be introduced. |
| R2 | Tier A regresses the number-heavy strict/planning rows (canada/mesh/numbers) because the recursive-descent re-walk was hiding a real advantage (number spans need byte-level scanning anyway, so structural dispatch is a small fraction there). | Bench gate: canada/mesh/numbers `parse_only` candidate telemetry must stay inside the post-W0 maintain budget filled by S-P3/W3 with same-run strict anchors where admission is claimed. If they miss that budget, the Tier A union candidate is net-negative for number-heavy rows and must be rejected or routed to a later independent S-P3 proof. It must not preserve `OffsetTape`-via-recursive-descent as a mixed old producer inside the same retained-index design; any later route still must satisfy one producer, one retained `Tape`, and no new directive/BIR/substrate. |
| R3 | Packing `StructuralClass` into the offset `u32` overflows 32 bits on inputs >256 MiB if 4 bits are stolen for the ordinal (offset range drops to 2^28). | `checked_u32` (`mod.rs:228`) already debug-asserts the 32-bit bound; avoid offset packing unless W3 proves the bound is acceptable. Prefer a parallel co-indexed `Vec<u8>` class lane (costs 1 byte/element, ratio still <1x input) to keep the full 32-bit offset range and preserve the dense offset array byte-identically. |
| R4 | The SIMD scan and recursive-descent parser disagree on structural count for adversarial inputs (escaped-quote parity edge cases — `scan.rs:164-198` `resolve_string_masks_64` slow path). A fused parser that trusts the scan would mis-parse where the standalone recursive-descent did not. | The scalar parity hash (`scan.rs:38-45`, `scalar_parity_report`) must gate the fused path in CI across the full corpus before the fusion wave commits; this is the existing Lock-8 Exact-mode contract (`SUBSTRATE.md` §3.4). Reusing string-boundary or quote/backslash-parity facts as retained template facts is Tier B, not part of Tier A closure. |
| R5 | `SUBSTRATE.md` is stale on two load-bearing points (claims close-token elision; claims "structural projection IS the tape's storage" while the scan output is discarded). A wave that trusts the doc designs against fiction. | SK-V8 must correct `SUBSTRATE.md` §1.5 and §3.6 alongside the fusion code (per the `document-alongside-code` memory): record that closes ARE emitted and that pre-fusion the scan output was unconsumed. Stale-doc-driven design is a recurring REDRESS pattern. |
| R6 | Effort misallocation: a wave spends its budget on Tier A structural cursor migration and the headline `parse_only` numbers on string-loss corpora (twitter, update_center) barely move because the ~75% string-scanner cost (SK-V7 §3.4) is untouched. | State the prediction up front: Tier A targets the *structural-dispatch* fraction (~10–25% on object-heavy corpora per SK-V7 §3.3), not the string-scan fraction. The candidate's gate must use the W3 table's filled numeric threshold for the named structural-dispatch hot leaf, not "twitter flips to GO". Route string-boundary, quote/backslash-parity, and string-scanner residuals separately as Tier B. |

---

## §6 Sources

- `skinny/crates/runtime/src/tape/mod.rs:90-97` — `Tape` three-array layout; :228-231 `checked_u32`; :134-146 `offset_at`/`flags_at`.
- `skinny/crates/runtime/src/tape/assembler.rs:70-91` — `push_plain_offset` hot path + cold reserve; :93-113 `patch_flags`; :13-40 `CapacityPlan` (GrowOnly default :27).
- `skinny/crates/runtime/src/tape/offsets.rs:1-6` — `OffsetTapeStats`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:14-17` — `attach_structural_index` no-op; :37-43 `parse_value_at` source-byte dispatch; :161-185 `match_tiny_plain_string`; :205-211 `parse_number` emit; :233 literal emit; :268 quote emit; :303/:334/:374 structural/close emit.
- `skinny/crates/runtime/src/grammars/json/parser.rs:7-12` — `ParserState`; :35-43 `emit_plain_offset`/`patch_flags`; :47-52 `parse` (calls no-op `attach_structural_index`).
- `skinny/crates/runtime/src/grammars/json/scan.rs:22-30` — `scan_structurals`; :47-54 `structural_capacity_for`; :207-275 NEON `neon::scan`; :38-45 `scalar_parity_report`.
- `skinny/crates/runtime/src/grammars/json/view.rs:309-381` — `at_cursor` re-fetch pattern, `next_sibling_cursor` O(subtree) re-classification; :415-441 `token_from_cursor`.
- `skinny/crates/codegen/src/lower/{offset_tape,event_tape,eager_tape}.rs` — all three are 17-line stubs; no shape-specific lowering exists.
- `skinny/RESULTS.md:5-42` — `parse_only` Δ-vs-sonic rows; :153-215 Notes (per-corpus offsets count, logical/allocated tape bytes, ratios, element census, `0/0` arena counters).
- `restart/skinny/SUBSTRATE.md` §1.1 (dead `TapeToken` spec), §1.5 (stale "projection IS the tape" claim + close-token elision), §1.6 (simdjson stage-2 `advance` anchor), §3.4/§3.6 (parity hash, materialization gate).
- `restart/skinny/tranches/sk-v5/research/skv5-A4-tape-union-audit.md` §2 (`derive_backend_shape` absent), §3.4 (scan output unconsumed), §9.2 (WIRE recommendation).
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md` §3.3–§3.4 (string scanner pair ~75% self-time), §6 (six rejected micro-kernels, all inside the recursive-descent body).
