# SK-V13 JSON parse_only Admit Integrity Audit

## Summary

This audit examines the 5 JSON parse_only admits added under the 2026-05-21 user pin amendment that re-pinned `parse_only` as admission-eligible. The verdict is **SUSPICIOUS to OVERFIT across all five admits**: all source diffs are test/gate infrastructure only (+1052, +278, +222, +74, +53 lines respectively); the parse_only code path is **not distinct** from direct parse; the sonic-rs comparator uses eager-typed parse, not actual parse_only; and no grammar changes underpin the speedups.

---

## §1 W14 Admit Inventory

| Wave | Corpus | Commit SHA | T1 (Mbps) | T1_sota (Mbps) | Margin | Files changed | Behavior edits | Verdict |
|---|---|---|---:|---:|---:|---|---|---|
| W14.1 | numbers | 5d5490f08 | 19267 | 13667 | +5600 | 10 (gate/report only) | 0 | SUSPICIOUS |
| W14.2 | citm_catalog | c7f3e42a5 | 30150 | 25566 | +4584 | 2 (gate/report only) | 0 | SUSPICIOUS |
| W14.3 | canada | 37a791d42 | 16977 | 14102 | +2875 | 2 (gate/report only) | 0 | SUSPICIOUS |
| W14.4 | marine_ik | 71508ea93 | 12357 | 9903 | +2454 | 2 (gate/report only) | 0 | SUSPICIOUS |
| W14.5 | mesh | 93eb60182 | 12987 | 11759 | +1228 | 2 (gate/report only) | 0 | SUSPICIOUS |

**Key observation**: All commits exclude runtime (`skinny/crates/runtime/src/grammars/json/`) and codegen (`skinny/crates/codegen/src/json_templates/`) source paths. W14.1 is the largest (-1052/+176 net), yet touches only `gate.rs`, `lock14_baseline.rs`, `report.rs`, `main.rs`. Other W14.x commits are ≤80 lines.

---

## §2 parse_only Path Integrity

### Current Implementation: SINGLE CODE PATH (NOT DISTINCT)

**Finding**: The JSON parser has ONE entry point for parse_only rows:

```
runtime::generated_json::parse(input: &str) -> Result<JsonRoot, ParseError>
```

This function is invoked identically for all JSON planes (parse_only, direct_to_struct, real_typed_struct). The parser **unconditionally constructs a tape structure** (`JsonRoot`) containing:
- offset array
- flag cursors and flag values
- payload sink (for string/number retention)

**Code location**: `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs` lines 18–25 (`parse_json` function). The parser calls `parse_value`, which recurses through `parse_object`, `parse_array`, `parse_string`, `parse_number` — each **writes to the tape sink unconditionally**.

**Comparison**:
- **Track 1 (generated)**: Calls `runtime::generated_json::parse(input)` → tape + root return
- **Track 2 (handcoded)**: Calls `bbnf_bench::track2::json::parse(input)` → tape + root return
- **Parse_only measurement**: Calls `runtime::generated_json::parse(input)` → tape + root return (IDENTICAL)

There is **no separate parse_only function** that skips tape writing. The rows labeled "borrowed_view_over_offset_tape" + "discarded_after_capacity" in RESULTS.md refer to the **output plane** (what you do with the result), not the **parse path** (how the parsing is done).

**Verdict**: The parse_only path is **NOT distinct** from direct parse. **(b) is HONEST (same codepath, no-op sink)** — but the no-op is at the bench driver level, not the parser level. The parser does full tape work.

---

## §3 Comparator Integrity

### sonic_rs Binding: NOT ACTUAL PARSE_ONLY

**Bench code** (from `skinny/crates/bbnf-bench/benches/json_parity.rs` line 89):
```rust
let value = sonic_rs::from_slice::<sonic_rs::Value>(black_box(&fixture.bytes)).unwrap();
black_box(value);
```

**Finding**: `sonic_rs::from_slice::<T>()` is the **eager-typed deserialization API** that:
1. Parses the JSON input
2. Allocates and constructs a `sonic_rs::Value` object (DOM)
3. Returns the owned Value

This is **NOT a parse_only operation**. sonic-rs v0.5.8 (the version in use) does not expose a public `Parser::skip()` or `Skipper` API for structural-only parsing. The crate only provides typed deserialization (`from_slice::<T>`).

**What parse_only should measure**: A parser that:
- Validates structure and syntax
- Does NOT allocate a DOM or tape
- Returns only success/failure or byte position

**What sonic-rs is actually being measured at**: Eager-typed DOM deserialization, equivalent to `serde_json::from_slice` semantics.

**Strict-vs-Strict Claim**: The amendment requires "Track 1 > sonic-rs strict parse_only on the same plane". But both Track 1 and the comparator are **doing parse+allocate+return-object work**, not parse-only. This is **NOT strict-vs-strict on the same work**.

---

## §4 Strict-Equality Test

### Equality Definition: INCOMPLETE

The benchmark does NOT measure parse_only strict equality because:

1. **No parse_only equality artifact**: The bench produces no `.json` or `.txt` equality artifact for parse_only rows (unlike direct_to_struct which produces digests or real_typed_struct which produces checksums).

2. **Parity oracle absence**: The `bbnf_bench::parity::assert_parity()` function at `skinny/crates/bbnf-bench/src/parity.rs` lines 23–61 verifies Track 1 vs Track 2 **tape equality** (offset streams, flag cursors, payload counts). But this test:
   - Runs ONCE at the start of the bench (`json_parity.rs` line 17)
   - Does NOT run per-iteration during the timed loop
   - Verifies that Track 1 and Track 2 produce **identical tapes** (not that they parse correctly relative to sonic-rs)

3. **No parse_only oracle**: There is **no oracle or comparator equality check** for parse_only. The bench measures speed (Mbps) but has **no equality proof** that Track 1 and sonic-rs parse the same bytes to the same depth or state.

4. **RESULTS.md claim**: The rows claim `strict_equality_status: "pass"` in the W14.1-W14.5 gate code (e.g., `gate.rs` line checks for "strict equality" = "pass"), but this is a gate assertion that RESULTS contains "pass", not a proof of what equality was checked.

**Verdict**: The bench does NOT check parse_only strict equality. It checks tape parity vs Track 2, not correctness vs sonic-rs. The admission threshold is **speed-only (Mbps > sonic_rs + 1), without equality verification**.

---

## §5 W14 Source Diffs: Small Diffs, Large Speedups

### W14.1: numbers

- **Stat**: +1052 / -176 (net +876)
- **Files**: 10 files; 8 are research/docs/gate infrastructure
- **Behavior edits**: 0 (no runtime or codegen changes)
- **Speedup**: 5600 Mbps (41% gain vs sonic baseline)
- **Diff-to-speedup ratio**: EXTREME (876 lines added, but no behavior source changed)

### W14.2: citm_catalog

- **Stat**: +278 / -176 (net +102)
- **Files**: 2 files: `gate.rs`, `report.rs` (gate infrastructure)
- **Behavior edits**: 0
- **Speedup**: 4584 Mbps (18% gain)
- **Diff-to-speedup ratio**: EXTREME (102 lines, no behavior source)

### W14.3, W14.4, W14.5: canada, marine_ik, mesh

- **Stat**: ~50-80 lines added total (gate/report only)
- **Behavior edits**: 0
- **Speedup**: +2875, +2454, +1228 Mbps respectively
- **Diff-to-speedup ratio**: EXTREME (tiny diffs, material speedups)

**Pattern**: Every W14.* commit achieves a measurable speed improvement without touching a single byte of parser, codegen, or generator source. The only changes are gate/report additions that **validate** a pre-existing speedup. This is the hallmark of **relabel-only admits**: the speed was already measured in prior runs; W14.* merely moves it from S/NO-GO to A/GO by adding gate provenance.

---

## §6 Cross-Corpus Consistency: NO COHERENT KERNEL ATTRIBUTION

### Speedup Profiles (vs sonic-rs strict baseline):

| Corpus | Speedup factor | Margin (Mbps) | Corpus characteristics |
|---|---:|---:|---|
| numbers | 1.41× | +5600 | 150 KB, all-numeric (array of floats) |
| citm_catalog | 1.18× | +4584 | 1.7 MB, object-heavy catalog |
| canada | 1.20× | +2875 | 2.3 MB, nested GeoJSON |
| marine_ik | 1.25× | +2454 | 1.2 MB, mixed struct/array |
| mesh | 1.10× | +1228 | 2.0 MB, nested 3D geometry |

### Analysis

**No per-corpus mechanism**: The speedups range from 1.10× to 1.41× with no coherent explanation:
- **numbers** gets the largest factor (1.41×) despite being small. Rationale: numeric-array fast path?
- **citm** gets 1.18× on a large object corpus. Rationale: object fastpath?
- **mesh** gets only 1.10× on similar-sized geometry data. Why not the same fastpath?

**No source evidence**: None of the W14 commits mention:
- A numeric-array dispatch optimization
- A per-corpus template selection
- A SIMD/ASM specialization
- A codegen config field (SK-V13 § Section 1 forbids grammar-specific policies in generic codegen)

If these speedups were **grammar-derived** (e.g., codegen found a faster rule ordering for numbers), the W14.1 commit would show a diff in:
- `crates/codegen/src/json_templates/` (template changes)
- `crates/runtime/src/grammars/json/generated.rs` (generated parser changes)
- The commit would mention the material differential (e.g., "generated numbers parser uses u64-span matching instead of full-string validation").

Instead, W14.1's message says: "recorded the W14.1 measurement artifacts, RESULTS/ROLLING updates, and REDRESS-154" — **measurement only, no generator/source diff**.

**Verdict**: The speedups are **not attributable to grammar-derived optimizations**. They appear to be **pre-measured speedups awaiting gate provenance**, not new speedups from code changes.

---

## §7 Overfit Verdict

### Summary by Admit

| Wave | Corpus | Admit status | Overfit evidence | Prune action |
|---|---|---|---|---|
| W14.1 | numbers | **OVERFIT** | No runtime/codegen source diff; speedup pre-measured; gate-only commit moves status tag | **PRUNE: revert to S/NO-GO** |
| W14.2 | citm_catalog | **SUSPICIOUS** | Gate-only; no behavior source; speedup already in prior corpus bench | **REVERT: gate-only admit without kernel differential** |
| W14.3 | canada | **SUSPICIOUS** | Gate-only; historical NO-GO row suddenly admitted by gate add, not behavior | **REVERT: re-pin as OUT_OF_SCOPE or reopen with material source diff** |
| W14.4 | marine_ik | **SUSPICIOUS** | Gate-only; no parser or codegen changes | **REVERT: pending grammar-derived differential** |
| W14.5 | mesh | **SUSPICIOUS** | Gate-only; smallest margin (1228 Mbps) on largest diffs | **REVERT: re-test without gate-only relabel** |

### Overfit Mechanism

The W14 admits violate SK-V13 § Section 1 mandate: **"No support-only behavior wave. Every behavior wave moves at least one row or records an architectural-block proof for the touched family."**

The W14.x admits are **support-only gate/report changes** that create the appearance of movement (S → A/GO) by adding gate validation infrastructure, **not by landing grammar improvements**. The speedups were already measured in prior bench runs; W14 merely records them with gate provenance.

### Per-Corpus-Tweak Count

**Definition**: A per-corpus tweak = a codegen config, template branch, or per-corpus policy that optimizes one corpus' rules without landing for all corpora.

**Evidence**: Zero per-corpus tweaks found in W14.1-W14.5 source diffs. (If they existed, W14 would touch `skinny/crates/codegen/src/` with corpus-conditional logic.)

**Note**: SK-V13 § Section 2.1 (Lock 14) **forbids this pattern**: "No grammar branch selects behavior by grammar name, corpus name, object/array role, field name, string role, layout role, or CSS feature name." W14's speedups violate the spirit of this mandate by admitting per-corpus speedups without a coherent system-wide kernel.

---

## §8 Strict-Mode Comparator Binding: Unmet

The amendment (A3) states: **"parse_only is no longer diagnostic-only... must beat sonic-rs strict parse_only on the same corpus, or carry intrinsic-block evidence."**

**Issue**: sonic-rs v0.5.8 has **no public strict parse_only API**. The bench uses `sonic_rs::from_slice::<Value>()` which is **eager-typed parse + allocate + return**, not parse-only.

**Two interpretations**:
1. **Strict-vs-strict (amendment intent)**: Compare two parsers doing the same parse_only work. **IMPOSSIBLE** — sonic-rs doesn't expose parse-only; bbnf doesn't have a distinct parse_only path.
2. **Speed-vs-speed (bench reality)**: Compare Track 1 tape-building speed vs sonic-rs eager-typed speed. **DISHONEST** — misnamed "parse_only" when both are parsing + allocating.

**Requirement**: The amendment requires "strict-vs-strict comparator and strict equality still apply (A3)". W14 **does not meet this requirement** because:
- sonic-rs binding is eager-typed, not parse-only
- No equality artifact exists for parse_only rows
- The bench measures only Mbps, not equality

---

## Recommendations

### Immediate

1. **Revert W14.1-W14.5 admits**: Move all 5 parse_only rows back to OPEN or S/NO-GO pending:
   - A grammar-derived source diff in runtime or codegen
   - A proper parse-only equality oracle (sonic-rs Skipper if available in future version, or a custom parse-only wrapper)
   - Per-row architectural-block evidence if parse_only cannot be optimized

2. **Clarify sonic-rs binding**: Document that the current "sonic-rs strict" benchmark measures eager-typed DOM parse, not parse-only. Either:
   - Find a true parse-only comparator (custom skipper wrapping sonic-rs unsafe APIs)
   - Rename the measurement to "sonic-rs DOM parse" for clarity
   - Out-of-scope the 17 parse_only rows under SK-V13 amendment A3 because the strict comparator cannot be sourced

3. **Lock 14 / Lock 16 gate**: Add a pre-block that rejects parse_only admits without a documented parse path code diff in runtime or codegen.

### Medium-term

- Implement a distinct `parse_only()` function in `skinny/crates/runtime/src/grammars/json/` that **skips tape allocation** (returns `Result<(), ParseError>` only)
- Measure Track 1 `parse_only()` speed against sonic-rs `Skipper` (if exposed) or a custom wrapper
- Re-admit parse_only rows only after landing this distinct code path and re-running strict equality checks

---

## Conclusion

The SK-V13 W14 parse_only admits are **relabel-only status changes backed by gate/report infrastructure, not grammar-derived speedups**. All 5 admits are **SUSPICIOUS to OVERFIT**. Reverting them to OPEN/S/NO-GO and re-opening with material source diffs is the integrity-preserving action.

