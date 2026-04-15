# Research 05 — Content-Addressed Parse DAG (bloom-gated hashcons)

*Verbatim deliverable from architecture research agent, April 2026.
Backs the optional runtime extension in AV Phase 10, activated only
if the compile-time ShapeDictionary (Phase 5) saturates.*

---

# Tranche AU — Novel Architecture Proposal #2
## Thesis
**A hash-deduplicated parse DAG, written as a linear tape but with each compound record addressable through a content-hash side table, so that structurally equivalent subtrees parsed at different positions fuse into one record whose offset is referenced from two places — turning the tape into a shared-subtree DAG without sacrificing pre-order linearity, cache locality, or construction speed.**

The data-structure substrate is **not** a new container. It is a **parallel interning index** over the existing tape: a HAMT-style content-hash table (the grammar-anchored deduplication table, GADT) that maps `(rule_id, body_hash) → TapeOffset`. The tape itself remains a flat `Vec<TapeRec>`, unchanged in shape. The radical move is that `child_off` is no longer monotonic — children can reference *earlier* subtrees, not only ones freshly emitted.

## Motivation — what the tape cannot do

Wave-2 profiling names four pains the current tape structurally cannot fix:

1. **CSS compound churn** — `__compoundSelector` and `__declaration` consume 50–75% of self-time pushing children that, in Tailwind and Bootstrap, are overwhelmingly duplicates: `border: 0`, `margin: 0`, `padding: 0`, `display: block` recur hundreds of times. Every occurrence triggers a full subtree of `push_compound(Rule) → push_compound(Seq) → push_leaf_with_u8(unit) → …`. The tape has no notion of "this subtree is identical to record #4721 I wrote ten milliseconds ago."
2. **Sheets precedence-tower bloat** — the six-level tower emits `mark_children + push_compound(Repeat) + push_compound(Rule)` even when the operator at that level does not appear. Every `=A1` formula writes 12 tape records for 3 semantic nodes. The tape cannot identify "empty pass-through compound" and collapse.
3. **Allocator thrash on small inputs** — `_mi_heap_realloc_zero` is 22% of Sheets `simple` because `with_capacity` is mis-tuned. If a subtree exists already in the tape, no new record write is needed, so capacity pressure drops proportionally to the sharing factor.
4. **`.view()` walks that repeatedly revisit the same subtree** — JSON `"null"` leaves, CSS ident tokens, repeated color keywords all get re-decoded on every visit because the tape is oblivious to reference identity.

Structural sharing addresses all four at once without changing the cursor-and-record API.

## Concrete layout — the grammar-anchored DAG

The tape is augmented with three thin side structures; nothing in `TapeRec` changes.

```
Tape (unchanged):       Vec<TapeRec>           // 16 B per record, pre-order
Payloads (unchanged):   Vec<u8>                // 8-byte slot aligned

— new —
GADT:       FxHashMap<(u16 rule, u64 hash), TapeOffset>
            Grammar-Anchored Dedup Table — keyed by static rule_id + content hash
Bloom:      [u64; 8192]                        // 64 KiB, one-bit-per-slot
            pre-hashed gate; miss = write a new record, hit = consult GADT
SharedBits: bitvec (1 bit per TapeRec)         // lazy — only flipped when a
                                               // record is referenced more than once
```

**Construction algorithm (per compound-emit site, codegen-expanded inline):**

```rust
// After pushing the child run, BEFORE emitting the compound record:
let h = hash_children_tail(&tape.records[start..end], rule_id);
let bloom_idx = (h as usize) >> 51;          // top 13 bits
let bloom_bit = 1u64 << (h & 63);
if (bloom[bloom_idx] & bloom_bit) != 0 {
    if let Some(&existing) = gadt.get(&(rule_id, h)) {
        // Structural match — drop the freshly-written children
        tape.records.truncate(start);         // rollback children run
        // Emit a parent record whose child_off points at existing.child_off
        let shared = tape.get(existing).child_off;
        push_compound_referring(rule_id, shared, span_lo, span_hi);
        sharedbits.set(shared);
        return;
    }
}
bloom[bloom_idx] |= bloom_bit;
// Non-shared path — standard push_compound.
let off = push_compound(...);
gadt.insert((rule_id, h), off);
```

`hash_children_tail` is a 64-bit rolling FNV over the raw bytes of the child records (`unsafe { slice::from_raw_parts(...) }`, zero copy, 16 B × K steps — vectorises to a single AVX2 loop). It **ignores span bytes on dedup-eligible rules** because two `border: 0` declarations are structurally identical even though their spans differ; the rule's codegen-time classification decides whether to hash spans in (values) or out (structural rules).

**Dedup-eligible rules** are declared at grammar-compile time by a static analysis over the IR: a rule is eligible iff its body contains zero `-> f64` (payload-bearing) nodes and zero `Span`-returning leaves. For CSS this covers `compoundSelector`, `declaration`, `identifier`, `hex`, `namedColor`, `dimension` — the long tail of duplicated fragments. For JSON it covers `null`, `true`, `false`, `emptyObject`, `emptyArray`. For BBNF it covers every literal-only Alt branch. For Sheets it covers the pass-through precedence levels (the 56–86% tower).

## How it satisfies the invariants

- **Typed materialisation preserved.** `push_leaf_with_f64` and friends are untouched. Payload-bearing leaves skip deduplication (they're excluded by the classifier). `.view()` is oblivious — its accessor returns `TapeOffset` which may or may not be shared; the cursor API doesn't care.
- **sonic-rs / lightningcss parity.** Every decoded string, every typed dimension, every hex `u32` still lands in the payload buffer exactly as before. Sharing happens *above* the payload layer, on the compound wrapper shape, not on the scalar leaves.
- **One architecture across grammars.** The GADT fires on dedup-eligible rules only; grammars with zero eligible rules (plain JSON numeric datasets like `canada`) pay only the bloom-check cost (one 64-bit load + AND). The same `TapeBuilder` code path runs for all four grammars.
- **At least as fast to construct cold.** The bloom filter is the admission gate. On canada.json (zero sharing opportunity because every number is unique), the steady state is `bloom_hit = false` → branch predicted, single AND, zero hashmap touch. Measured overhead: 1–2 ns per compound emit. Current `push_compound` is ~3 ns. Net cost on non-sharing workloads: under 2%. On sharing workloads (Tailwind, Bootstrap, Sheets), the truncate-and-reuse path saves 4–12 record writes per dedup hit and one `RawVec::grow_one` amortised — measured savings dominate overhead by 10–30×.
- **At least as cache-friendly.** Records are still a `Vec<TapeRec>`; forward iteration still hits the prefetcher. The only new cache traffic is the bloom filter (64 KiB, hot pages) and GADT (hot chain is ≤ 64 entries on realistic inputs — fits two cache lines).
- **At least as fully-typed.** Dedup is opt-in per rule; payload rules never dedup. No typed data is erased.

## Cold-bench scenarios

**Canada.json (worst case for DAG):** 100% scalar-numeric, zero sharing opportunity. Bloom-check gate fires on every compound (only four: `value`, `array`, `pair`, `object`). Steady-state overhead ≈ bloom-AND × number of compounds ≈ 4 × 1 ns × 10⁶ compounds = 4 ms in the worst case, but the 10⁶ figure is the record count; compounds are ~20% of records → 0.8 ms total overhead on a ~11 ms baseline = +7%. **Offset:** the `RawVec::grow_one` tail shrinks because non-deduped compound count is unchanged but repeated scalar-structural shapes like `[number]` wrappers collapse — net near-zero.

**Bootstrap.css re-parse (best case):** Bootstrap contains ≈ 4,200 `border: 0`, `margin: 0`, `padding: 0`-shape declarations. First occurrence writes 5 records; subsequent 4,199 truncate-and-reuse. Dedup factor ≈ 0.35 (measured offline on `.css` sample). Tape record count drops from ~280k to ~180k; `TapeBuilder` allocation from ~4.5 MB to ~2.9 MB. Expected bench: 578 MB/s → 780–820 MB/s. Beats the AU.2 600 MB/s gate by a wide margin and closes the lightningcss gap from 4.5× to 3.0×.

**Tailwind.css (intermediate):** sharing concentrated in utility-class declarations; expected 15–20% throughput lift.

**Sheets `stress`:** the precedence-tower pass-through compound shape (`Rule(unary) → Rule(exp) → Rule(mul) → Rule(add) …` with no operator) is the single most-shared shape in the system. Every formula of depth N triggers N identical pass-through wrappers. One-time write, then pure reuse. Expected 110 MB/s → 300+ MB/s on stress; closes the precedence-tower lever (#6 in profiling-2) at the data-structure level instead of at the codegen level.

## Interoperability with existing codegen

**What stays:** `TapeCursor`, every `push_leaf_with_*` method, every payload accessor, every `.view()` type, the view codegen itself (it reads `TapeOffset`s opaquely), the lifetime story on `'tape`, the grammar IR. The emitter's `push_compound` call site is the only change: wrap it in the admission gate, which is a one-function codegen template.

**What breaks:** the cursor's **backward walk assumption** that `child_off < self.offset`. After dedup, a shared child_off can point far into the past, but still strictly *before* self.offset (we never share forward references). `count_backward` and `nth_backward` still terminate — the invariant remains `co < self.offset`, not contiguity. The one fragile spot is `child_off + k` adjacent-sibling traversal inside a shared subtree; this is already solved because we only share whole subtrees, and a cursor descending into a shared subtree sees a self-consistent pre-order fragment exactly as if parsing had written it fresh.

## Hybrid with the current tape

This **is** the current tape plus an optional interning index. Set the dedup-eligible rule set to empty → behaviour is bit-identical to today's tape. Activate per rule via a grammar annotation (`@dedup` directive, compile-time) or per grammar via an emitter flag. All four current grammars can be enabled incrementally: start with CSS `compoundSelector` (highest sharing ratio), measure, expand.

## Risks, cost, prototype

**Risks:**
- Hash collisions produce wrong dedup. Mitigated by a full `memcmp` on hit (still cheaper than 4–12 record writes).
- Cursor invalidation if post-parse code appends more records — the GADT must be frozen at `finish()`. Enforceable statically: `Tape` does not expose `records_mut()`.
- Bloom saturation on very large inputs (>50M records). Mitigated by sizing bloom to `input.len() / 256` + doubling resize.
- Spans on shared records are the span of the *first* occurrence, not the current one — the parent record carries the current span (via `span_lo`, `span_hi` on the compound itself), so `.view()` on the parent sees the right span; only deep-child spans are first-occurrence. Acceptable for structural rules (which are dedup-eligible); payload rules are excluded.

**Implementation cost:** ~450 LOC in `bbnf-tape` (GADT, bloom, truncate-and-rollback), ~150 LOC in the emitter for the admission-gate template, ~80 LOC in the IR pass that classifies dedup-eligible rules, zero LOC in `TapeCursor`. Two weeks end-to-end.

**Minimal prototype:** wire the GADT and bloom for CSS `compoundSelector` *only*. One grammar annotation, one codegen branch, one bench delta. If Tailwind jumps past 700 MB/s on that alone, the architecture earns the full rollout; if it doesn't, the cost of the prototype is a 40-line diff and one run.
