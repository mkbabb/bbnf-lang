# SK-V13 Scoping: SIMD/ASM/Union Post-W5 Surface and Wave Shortlist

**Date:** 2026-05-21  
**Scope:** SK-V13 post-W5 aarch64 SIMD/ASM/union surface, prior to wave assignment  
**Authority:** SK-V12 close PASS-ADMIT, USER PIN D3/D4 (union/ASM-gen unblocks), REDRESS-126 (W4 disposition)  
**PIN Carry-Forward:** CSS L4 authoritative; >SOTA bar; union/ASM-gen unblocked at category; Lock 16 + §2.2 discipline binding

---

## §1 Post-W5 aarch64 Inventory Delta

**Context:** SK-V12 closed with five orphan primitives demoted to `inventory_demoted_with_evidence` by REDRESS-126. W4 selected `a64_ascii_set_run_skip` as a separate candidate, achieved `decision=pass` on the microbench (4.72× speedup over scalar), and deferred production wiring to a later split. No production SIMD/ASM admission was taken in SK-V12 close path.

| Primitive name | File path | Pre-pin state | Post-W5 state | Production wired? | Row movement attributable |
|---|---|---|---|---|---|
| `bitmap_prefix_xor_64` | `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs` | Orphan (scalar-delegate aarch64 body; PMULL blocked REDRESS 88) | `inventory_demoted_with_evidence` | NO — active consumer is JSON scan, remains scalar-delegating; no aarch64 PMULL/EOR3 body admitted | Historical REDRESS 88 PMULL rejection; consumed by JSON scan but neon body never implemented |
| `bitmap_next_set_bit` | `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs` | Orphan (unused scalar-delegate wrapper; CSSC CTZ blocked REDRESS 89) | `inventory_demoted_with_evidence` | NO — no production consumer found; support-only; CTZ route historically blocked | Consumed by `compact_mask` but that caller is not active in hot path; aarch64 CTZ never implemented |
| `bulk_emit_positions_64` | `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs` | Orphan (scalar-delegate used in `compact_mask`; no aarch64 kernel) | `inventory_demoted_with_evidence` | NO — consumed by `compact_mask` (JSON scan), remains scalar-delegating; support primitive | Historically part of REDRESS 89 (bulk emit), demoted as "no dedicated bulk-emit aarch64 body" |
| `byte_context` | `skinny/crates/bbnf-simd/src/aarch64/byte_context.rs` | Orphan (vextq_u8 shift helpers; no production consumer) | `inventory_demoted_with_evidence` | NO — support-only; no production caller wired; only test coverage | Future string-special C4 variant could consume, but deferred pending consumer proof |
| `cache_hints` | `skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs` | Orphan (PRFM/STNP prefetch; no production consumer; SK-V11 demoted) | `inventory_demoted_with_evidence` | NO — support-only; no production caller; hint placement never proved on hot path | SK-V11 hardening ruled out broad output/tape emit; remains inventory-only pending caller |
| `a64_ascii_set_run_skip` (W4 candidate, separate from 5-row set) | Not in tree (deferred production split) | Microbench PASS: scalar 18.51 ns/iter, candidate 3.92 ns/iter, speedup 4.72×, parity PASS | **Routing to production split (not retroactive W5)** | DEFERRED — microbench pass but production CSS wiring explicitly routed to separate wave; no W5 production merge | W4 would move CSS delimiter-dispatch/layout-skip rows; wiring gate: named consumer in CSS scan-block, Lock 14 authorization, fresh Criterion/equality, W2 prerequisite if needed |

**Interpretation of `inventory_demoted_with_evidence`:** The five orphans are factually present in the source tree (`.rs` files exist), they have scalar references and checkasm tests (supporting evidence), but their aarch64 NEON bodies are either (1) scalar delegates (bitmap_*, bulk_emit_*), (2) support-only with no production caller (byte_context, cache_hints). Operationally, they are NOT in any hot production path on aarch64. New attempts to admit any of the same architectural space (prefix-XOR, CTZ bulk, cross-chunk shifts, store hints) must cite the demoted entry, demonstrate material differential, and pass §2.2 gate (scalar reference + checkasm + same-wave consumer).

---

## §2 The W4 Production-Wiring Deferred Work

### W4 ASM-gen Candidate Summary

**Candidate:** `a64_ascii_set_run_skip`  
**Caller API:** `find_ascii_set_member64(bytes, cursor, end, set: &[u8]) -> usize`  
**Delimiter set:** `b"{};"` (hex `7b7d3b`)  
**Microbench Result:**
- Scalar: `18.510497846 ns/iter`
- Candidate (NEON): `3.923145814 ns/iter`
- Speedup ratio: `4.718279341`×
- Threshold: `1.01`×
- Parity: `pass`
- Decision: `pass`

**Microbench Artifact:** `restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`

### Production Wiring Deferred

SK-V12 W4 PLAN-V4 explicitly routes production wiring to a **separate future production/gate split** rather than retroactive W5 work. The default W4 branch (which executed) was microbench-only, producing **no source edits** to:
- CSS runtime template/generated scan-block
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/RESULTS.md`, report/gate schema, CLI
- Criterion lanes or equality artifacts

### What Must Happen in SK-V13 for Production Wiring

**Wave assignment:** W4 repeat or new wave (e.g., W4b, W4-prod, or W5-candidate depending on SK-V13 plan structure)

**Required work envelope:**

1. **CSS scan-block production consumer:** Identify/create a grammar-supplied layout/delimiter byte-set caller in the CSS L4 `declaration_values` generated scanner. The current CSS runtime has direct scalar loops at `skip_ws_and_comments`, `trim_start`, `trim_end`. One of these must name `find_ascii_set_member64` or a wrapper that dispatches to it.

2. **Lock 14 parent authorization:** Add `sk-v12-waveW4-production` or equivalent narrow parent diff to `lock14_baseline.rs`. This authorizes reuse of the W4 microbench candidate in production CSS parsing (not retroactive W5, not JSON-guard-only).

3. **Same-wave Criterion/equality artifacts:**
   - Scalar reference (existing `find_ascii_set_member_scalar`): ✓ provided in PLAN-V4 §Scalar Reference
   - CSS fact-stream equality: strict equality with current lightningcss baseline on the `nonjson_css_l4` plane
   - Criterion microbench: measured CSS delimiter/layout loops (the real consumer path in scan-block)

4. **W2 prerequisite rerun (if needed):** If the CSS consumer path moves into string/escape regions, verify `escape_mask_64` correctness (W2 already resolved REDRESS-122; conditional rerun only if new escape consumer is wired).

5. **Gate and report:** `sk-v12-w4-ascii-set-run-skip-production-split` or similar; schema: pass/reject, lock14 parent, fact-stream hash, throughput delta vs scalar, CSS L4 row status.

**LOC estimate:** ~80 LOC (CSS caller wrapper, Lock 14 entry, test harness) + microbench/artifact gen (~40 LOC) = ~120 LOC.

**Same-wave consumer status:** The microbench proved the first-nonmember extraction (e.g., `skip_ws_and_comments` finding the first non-whitespace/comment delimiter byte). The actual CSS generated scanner consumer must be wired in the SAME wave as the production admission, per §2.2.

---

## §3 Rust-Union Substrate SK-V13 Candidates

**Context:** USER PIN D3 unblocks union-substrate at category level. REDRESS 96/97/98 remain historical measured implementations of specific union variants:
- REDRESS-96: V1 class-column event-model (retained structural vector with class-per-position)
- REDRESS-97: V2 streaming-cursor event-model (class + cursor tuple yielded inline)
- REDRESS-98: V3 class-lane-only event-model (structurally minimal; failed CHALLENGE)

New implementations attempting the same architectural goal must cite prior REDRESS, name the material differential, and pass CHALLENGE.

### Three Novel Union Variants Not Previously Attempted

**Candidate Union-A: GrammarConfig-Driven Heterogeneous Union Per Grammar**

**Material differential vs REDRESS 96/97/98:** The prior attempts treated union shape as fixed (global V1 class-column, V2 cursor tuple, V3 class-lane only). This variant derives the union shape from grammar introspection: JSON uses a narrow event-lane (class + offset) because JSON structural rules are tight; CSS uses a wider class+context+rule-segment tuple because CSS has optional/grouped constructs; Sheets uses a schema-pinned tuple because column-order is known at compile time. Instead of a single global UnionTape, each grammar's generated parser owns a grammar-specific `UnionConfig` struct that holds the event-lane schema, and the substrate emits only the shape each grammar needs.

**LOC envelope:** ~400 LOC (GrammarConfig derive macro + union-codec abstraction + 3 grammar instances) + ~150 LOC (test harness + microbench) = ~550 LOC.

**Row movement candidate:** `json_structural_class_lane` (JSON guard), `css_selector_and_context_lane` (CSS L4), `sheets_cell_schema_lane` (Sheets if candidate time allows).

**CHALLENGE risk per SK-V12 §6:**
- CH1 (correctness): Derive macro must produce byte-compatible schema; checkasm required per grammar shape
- CH2 (generality/Lock 14): Schema derivation must be grammar-neutral (no hardcoded JSON/CSS bias); requires grammar parameter test
- CH3 (regression): Heterogeneous shape may break equality on cross-grammar corpus; CSS L4 row must hold >=SOTA
- CH5 (hidden coupling): Grammar-specific tuple shape may couple to generated parser internals; requires bounded codegen review
- CH6 (anti-paper close): Not a proof-only artifact; same-wave consumer gate required; measured row delta on at least one JSON/CSS plane

---

**Candidate Union-B: E-Graph-Selected Union Shape Per Rule**

**Material differential vs REDRESS 96/97/98:** The prior attempts fixed union shape globally or by grammar. This variant treats the choice of which fields to emit (class only, class+position, class+context, class+captured-bytes) as an e-graph cost-minimization problem: analyze the generated parser's use of structural events per rule, compute the cost (memory + compute) of each union shape, and select the minimum-cost shape per rule group. Rules that only test structural class pay the class-only cost; rules that also materialize positions or context pay the full cost only where needed.

**LOC envelope:** ~600 LOC (e-graph cost model + shape-selection pass + codegen adapter) + ~200 LOC (benchmark + equality check) = ~800 LOC.

**Row movement candidate:** `json_structural_projection` (JSON direct/typed), `css_rule_emission_filter` (CSS if structure allows).

**CHALLENGE risk:**
- CH1 (correctness): E-graph cost function must not omit required fields; formal proof or exhaustive test matrix required
- CH2 (generality): Cost model must not depend on JSON-specific call patterns; requires corpus-neutral validation
- CH3 (regression): Per-rule shape variation may regress on rules with complex cost trade-offs; careful threshold tuning needed
- CH4 (cost): E-graph analysis itself adds compilation cost; must be amortized by generated-code savings
- CH5 (hidden coupling): Shape selection may depend on grammar codegen internals; circuit-breaker rule needed if internals change
- CH6 (anti-paper close): Real measured delta on JSON direct row or CSS projected row required

---

**Candidate Union-C: ARMv9.2 SIMD-First Union Using PMULL+CSSC-CTZ Instead of Scalar Consume-Structural**

**Material differential vs REDRESS 96/97/98:** The prior attempts used scalar consume-structural loops (iterate bytes, check structural markers, emit events). This variant flips the architecture: use PMULL bit-matrix to extract all structural positions in one pass (per 64-byte window), then use CSSC CTZ to consume bits in rank order, emitting class+position tuples without scalar iteration. The union schema is still event-lane but the consumption path is entirely SIMD-native: no scalar loop, direct NEON→memory writeback.

**LOC envelope:** ~250 LOC (PMULL structural-position matrix + CTZ bit-extraction loop + union-codec sink) + ~180 LOC (checkasm + microbench + parity) = ~430 LOC.

**Row movement candidate:** `json_structural_class_lane` (JSON guard), potentially `json_direct_projection` if structural throughput is the bottleneck.

**CHALLENGE risk:**
- CH1 (correctness): PMULL must extract ALL structural bytes without omission; checkasm matrix coverage required; cross-validated against scalar reference
- CH2 (generality/Lock 14): Structural markers (quotes, brackets, colons, commas) are grammar-neutral but positions are; grammar parameter required; CSS/Sheets must skip SIMD path if structural semantics differ
- CH3 (regression): PMULL matrix overhead (register allocation, ALU stalls) may regress on small windows or sparse structural data; CPU cycle accounting critical
- CH5 (hidden coupling): CSSC CTZ popcount and fast-path rank order depend on aarch64 uarch; validate against ARMv8.0+ fallback if SK-V13 targets wider deployment
- CH6 (anti-paper close): Measured row delta on JSON guard or direct row required; same-wave consumer gate non-negotiable

---

---

## §4 ARMv9.2-A Still-Untapped Surface

**Source audit:** `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md` §3

### Wired in SK-V12 W4

- **NEON TBL (vqtbl4q_u8):** Used in `classify_tbl4` (low-6 byte-class table, 4×16-byte stripes). Active in dispatch.
- **Byte-class mask (eq-set):** `byte_class_from_eq_set_64_neon` is real NEON body; scalar ref and checkasm complete; consumed via `dispatch.rs:68` generic scan dispatch.
- **UDOT (vdotq_u32):** Parsed `digit_mac::parse_4_digits_dotprod` exists under `dotprod` feature; proof-only (no same-wave consumer wired).
- **String-special scan (vext boundary):** `string_block` 16-byte NEON + scalar ref; consumed by JSON parse-that; no 64-byte oracle or CSS consumer.
- **Movemask (vshrn+vzip idiom):** Consumed by all classify/string paths as support-only.

### Remaining Untapped (After SK-V12)

| Instruction class | Instruction name | Current status | Candidate hot leaf | SK-V13 feasibility |
|---|---|---|---|---|
| NEON TBL | TBX (graceful out-of-range) | Not used; TBL exists | Unicode escape nibble fallback, string special code-point fallback | Deferred; TBL is sufficient for current candidates; TBX is refinement |
| NEON TBL | LD4 deinterleave (vld1q_u8_x4) | I1 inventory; not called | JSON UTF-8 deinterleave if 4-window unrolled | Out of scope; unrolling adds compile complexity without measured consumer proof |
| PMULL | Polynomial multiply (vmull_p64, vmull2_p64) | REDRESS 88 blocks default `bitmap_prefix_xor_64` body | Prefix-XOR structural carry (union candidate C) | Unblocked by D4 only if candidate shows material differential and consumer wiring |
| CSSC | CTZ, CNT, ABS, SMIN/SMAX, UMAX/UMIN | CTZ/CNT inventory only; ABS/SMAX untapped | CTZ for bit extraction (union candidate C), ABS for signed compare reduction | CTZ unblocked by D4 only if consumer is same-wave; ABS/extrema have no P1 hot-leaf attribution |
| SHA3 | EOR3, BCAX (ternary boolean) | No aarch64 body; no local 3-input fold | Quote/escape/control fusion for string special; digest-plane bit mixing | Non-selectable; no P1 hot leaf names 3-input expression; defer unless new string profile appears |
| BFDOT / BFMMLA | BF16 matrix mult | Not applicable; no BF16 tokens in JSON/CSS/Sheets | n/a | Out of scope; text parsing has no numeric matrix |
| SVE2 | Scalable vectors (sve_*) | Not in scope per tranche (NEON-only, M5 Max fixed 128-bit) | n/a | Out of scope; tranche is ARMv8/v9 NEON, not SVE |

### Untapped Prioritization for SK-V13

**High priority (material delta evidence):**
1. CSSC CTZ (union candidate C as consumer; REDRESS-89 unblock requires same-wave consumer proof)
2. PMULL (union candidate C, prefix-XOR route; REDRESS-88 unblock requires material differential proof)

**Medium priority (missing infrastructure):**
3. ABS/SMIN/SMAX (no current hot-leaf attribution; requires new P1 profiling if compare-reduce becomes bottleneck)
4. TBX (refinement of TBL; useful if error paths need graceful bounds)

**Deferred (out of scope or low ROI):**
5. EOR3/BCAX (no 3-input expression identified; defer until string-wide profile)
6. LD4 (unrolling benefit unproven; defer until JSON UTF-8 bottleneck confirmed)
7. SVE2 (out of scope; NEON-only)
8. BFDOT (out of scope; no numeric matrix)

---

## §5 SK-V13 SIMD/ASM/Union Wave Shortlist

**Context:** SK-V13 must complete production wiring for W4's `a64_ascii_set_run_skip` candidate and address the union-substrate category (unblocked D3) and ASM-gen category (unblocked D4) with material differentials + CHALLENGE discipline.

**Wave ranking:** Expected_value / Risk

### Wave Candidate 1: W4b — `a64_ascii_set_run_skip` Production Wiring (Narrow, Low Risk)

**Target rows:** `css_l4_delimiter_dispatch`, `css_l4_whitespace_skip` (CSS L4), `json_structural_class_skip` (JSON guard, optional).

**Description:** Wire the W4 microbench-passed `a64_ascii_set_run_skip` candidate into CSS scan-block production. Scalar ref and microbench parity already proved. Lock 14 narrow parent authorization required.

**LOC envelope:** ~120 LOC (CSS consumer wrapper + Lock 14 entry + test harness).

**S-P2 research questions:**
1. Does CSS scan-block have a named layout/delimiter caller that can consume `find_ascii_set_member64`?
2. Is the wrapping API semantically compatible with the CSS trim/skip policy (e.g., does it preserve comment byte handling)?
3. Does same-wave CSS L4 row measurement show >=1% delta over scalar baseline, maintaining >SOTA vs lightningcss?

**Scalar reference + checkasm + same-wave consumer status:** ✓ (all three provided by W4 microbench; consumer gate is CSS scan-block integration).

**Expected close metric:** CSS L4 row holds >=SOTA; optional JSON guard measurement.

**Risk:** Low. Microbench proof is complete. Consumer wiring is routine CSS generation integration. The gate is Lock 14 legality (already narrowed) + CSS L4 equality hold.

**Rank:** **#1 (highest expected_value/risk)** — Microbench is "in the can," consumer is identified, risk is containment.

---

### Wave Candidate 2: Union Candidate A — GrammarConfig-Driven Heterogeneous Union Per Grammar (Medium Risk, High Value)

**Target rows:** `json_structural_class_lane` (JSON guard), `css_selector_and_context_lane` (CSS), `sheets_cell_schema_lane` (Sheets).

**Description:** Implement grammar-introspected union shape derivation. Each grammar's generated parser emits only the fields it needs (JSON: class+offset; CSS: class+context+rule; Sheets: schema-pinned tuple). Derive macro generates per-grammar UnionConfig. Tests measure memory / throughput delta vs generic fixed-shape union.

**LOC envelope:** ~550 LOC (GrammarConfig derive + union-codec + 3 instances + test harness).

**S-P2 research questions:**
1. Does the derive macro produce byte-compatible event schemas across JSON/CSS/Sheets without hardcoded bias?
2. Is schema derivation deterministic (no non-determinism from HashMap iteration or floating hash)? (Lock 14 requirement)
3. Which rows move most under heterogeneous shape (JSON guard throughput, CSS selector emission, Sheets cell binding)?
4. Does per-rule shape vary smoothly or show cliff-edge regressions (CH3 risk)?
5. What is the checkasm matrix size (grammar variant × rule type × shape permutation)?

**Scalar reference + checkasm + same-wave consumer status:** ✓ Scalar reference is existing fixed-shape union code; checkasm required per grammar/shape combination (estimated 200+ test cases); same-wave consumer gate is JSON structural path (JSON structural_class_lane consumer in generated code or JSON direct projection) + CSS selector rule emitter.

**Expected close metric:** Measured JSON guard row delta (e.g., JSON direct >=1% faster), CSS L4 row equality hold or optional measured delta, Sheets row measurement if time allows.

**Risk:** Medium.
- CH2 (generality): Derive macro must not encode JSON/CSS assumptions; requires grammar-neutral derivation test.
- CH3 (regression): Per-grammar shape choice may introduce subtle performance cliffs if cost model misses rule interactions; cycle accounting required.
- CH5 (hidden coupling): Macro may break if generated parser internals change; requires codegen bounds review.

**Rank:** **#2** — Concrete material differential vs REDRESS 96/97/98 (shape heterogeneity), unblocked by D3, but higher complexity than W4b.

---

### Wave Candidate 3: Union Candidate C — ARMv9.2 SIMD-First Union Using PMULL+CSSC-CTZ (High Risk, High Reward)

**Target rows:** `json_structural_class_lane` (JSON guard), potentially `json_direct_projection` if structural throughput dominates.

**Description:** Replace scalar consume-structural loops with NEON PMULL (structural position matrix) + CSSC CTZ (rank-order bit extraction) + direct tuple writeback. Union schema is still event-lane but consumption is entirely SIMD. Requires REDRESS-88 and REDRESS-89 unblock (PMULL + CTZ) as measured-pass evidence, not as proof-only.

**LOC envelope:** ~430 LOC (PMULL matrix builder + CTZ extraction loop + union-codec sink + checkasm + microbench).

**S-P2 research questions:**
1. Does PMULL extract ALL structural positions without omission across 64-byte window with aligned and unaligned boundaries?
2. What is the checkasm matrix (structural density, boundary case, JSON token overlap)?
3. Does uarch-specific CTZ fast-path (popcount, carry) remain stable across ARMv8.0 and ARMv9.2+?
4. Is PMULL overhead amortized on real JSON (not synthetic worst-case)?
5. Which grammar's structural consumption path is the bottleneck (JSON direct, JSON guard, or CSS)?

**Scalar reference + checkasm + same-wave consumer status:** ✓ Scalar reference is existing consume-structural loop; checkasm required for PMULL matrix coverage + CTZ ordering (estimated 300+ cases); same-wave consumer gate is JSON structural path (JSON direct projection or JSON structural_class_lane in guard).

**Expected close metric:** JSON structural row >=1.5× speedup vs scalar, JSON guard floor maintained or raised, Lock 14 authorization for PMULL+CTZ dual admission.

**Risk:** High.
- CH1 (correctness): PMULL matrix correctness must be formally verified or exhaustively tested; omitted structural byte is catastrophic.
- CH3 (regression): PMULL stall + register pressure may exceed scalar on sparse windows; CPU cycle modeling required.
- CH5 (hidden coupling): CTZ popcount latency on ARMv9.2 vs ARMv8.0 may require branch-prediction or fallback codegen; platform dependency risk.
- CH4 (cost): Implementation complexity is 3-4× higher than W4b; scope creep risk.

**Rank:** **#3** — Highest expected value if structural consumption is proved JSON bottleneck; high complexity and uarch coupling risk.

---

### Wave Candidate 4: W4-Repeat (Defer) — String-Special Scan 64-Byte Oracle (Union Candidate Dependency, Medium Risk)

**Target rows:** `json_string_special_scan` (JSON guard or direct), `css_escaped_identifier_scan` (CSS, if CSS strings are added to fixture).

**Description:** Expand `string_block` from 16-byte to 64-byte scalar oracle + NEON body; refresh boundary/tail checkasm (cross-chunk context handling). Required as backing for union candidate C if the CTZ consumer needs string terminator position, not just structural positions. Alternatively, standalone row movement if string-special scan becomes JSON profiling bottleneck.

**LOC envelope:** ~280 LOC (64-byte scalar oracle + checkasm boundary/tail sweep + NEON movemask/EXT tune).

**S-P2 research questions:**
1. Is 64-byte scan profitable on JSON (vs 16-byte + loop overhead)? Requires JSON string length distribution measurement.
2. Does cross-chunk context (vextq_u8 bridging) add measurable overhead or pipeline benefit?
3. Which rows move under string-special widening (JSON direct, JSON guard, CSS if strings added)?
4. Does 64-byte oracle interact safely with union candidate C's structural positions?

**Scalar reference + checkasm + same-wave consumer status:** ✓ Scalar (straightforward 4× loop unroll of existing 16-byte); checkasm required for alignment/boundary/cross-chunk (estimated 150+ cases); same-wave consumer gate is JSON string path (consumed by JSON direct projection or parse-that string materializer).

**Expected close metric:** JSON string row >=1.2× speedup vs 16-byte loop, JSON guard floor maintained, CSS optional if strings are added.

**Risk:** Medium.
- CH3 (regression): Unrolled scalar may miss cache line optimizations of original 16-byte loop; cycle accounting critical.
- CH5 (hidden coupling): vextq_u8 cross-chunk data dependency may break if JSON parser changes string boundary semantics.

**Rank:** **#4** — Useful if union candidate C is selected; defer if union is not chosen this wave.

---

### Wave Candidate 5: ASM-Gen MEASURED-REJECT (Deferred REDRESS Proof) — UDOT Digit-Run Span (Low Priority, Deferred Consumer)

**Target rows:** `css_l4_number_token` (CSS, sparse), `json_number_projection` (JSON, measured guard).

**Description:** Attempt UDOT x4 digit-run span consumer on JSON number projection (JSON has denser numeric spans than CSS L4). Provide complete x4 scalar oracle, strict parity checkasm (valid/invalid lanes, mixed, overflow), and microbench. Route as MEASURED-REJECT if UDOT overhead exceeds scalar on JSON sparse numeric tokens.

**LOC envelope:** ~280 LOC (x4 digit oracle + x4 parity checkasm + microbench + JSON number consumer wrapper).

**S-P2 research questions:**
1. What is the JSON numeric token density (how many digit runs per 1000 bytes)? If <1%, UDOT setup cost dominates.
2. Is UDOT x4 parity correct for invalid/overflow/mixed-valid lanes?
3. Does JSON number consumer path (emitting numeric tokens, not materializing values) actually benefit from UDOT position hints?

**Scalar reference + checkasm + same-wave consumer status:** ✓ Scalar (existing `parse_4_digits` fallback); checkasm (required for x4 strict parity, currently smoke-only); same-wave consumer gate is JSON numeric row (JSON guard number projection or direct literal).

**Expected close metric:** MEASURED-REJECT evidence for REDRESS-3290+ (UDOT digit attempt); optional measured-pass if JSON profiling shows >=2% numeric density and >=1.1× speedup.

**Risk:** Medium-High.
- Consumer is deferred pending JSON numeric profiling; premature SIMD may regress.
- UDOT overhead (setup, register allocation) may dwarf benefit on sparse tokens.

**Rank:** **#5** — Deferred unless JSON numeric profiling (S-P1) shows dense token streams. Better as MEASURED-REJECT evidence (measured attempt, not proof-only reuse).

---

### Wave Candidate 6 (Optional, High Risk): Union Candidate B — E-Graph-Selected Union Shape Per Rule (Deferred, Compilation Cost Risk)

**Target rows:** `json_structural_projection` (JSON direct if grammar analysis shows per-rule savings), `css_rule_emission_filter` (CSS, if structure allows).

**Description:** E-graph cost model to derive per-rule union shape (class-only, class+position, class+context, class+bytes). Shape selection is code-generator time; runtime is union consumption without decision overhead.

**LOC envelope:** ~800 LOC (e-graph cost model + shape-selection pass + codegen adapter + benchmark).

**S-P2 research questions:**
1. What is the e-graph cost function (time, memory, register presure) for each shape? Requires CPU cycle modeling.
2. Is cost function grammar-neutral (no JSON/CSS hardcoding)?
3. What is the compilation-time overhead of e-graph analysis? Must be <5% total codegen time.
4. Do per-rule shapes vary smoothly or show cliff-edge transitions (CH3 risk)?
5. Measured delta on JSON direct or CSS rule row: how much is union shape vs other optimizations?

**Risk:** Very High.
- CH4 (cost): E-graph codegen analysis overhead is a new compilation-time cost; unless runtime savings >10×, not worth shipping.
- CH5 (hidden coupling): Shape selection is tightly coupled to generated parser IR; any codegen change may invalidate cost model.
- CH6 (anti-paper close): Requires real measured delta on JSON direct or CSS rule row; proof-only is not admissible.

**Rank:** **#6 (Optional, conditional on profiling)** — Defer unless S-P1 profiling shows that union shape alone accounts for >3% of JSON direct row time. Too high compilation-cost risk for speculative SIMD.

---

### Wave Candidate 7 (Optional, Deferred): ARMv9.2 Untapped Refinements — TBX, SMIN/SMAX, EOR3 (Inventory-Only)

**Inventory:**
- TBX (vqtbx4q_u8): Graceful out-of-range fallback for byte-class table. Useful if whitespace/delimiter lookup allows invalid bytes. Deferred; TBL is sufficient for current candidates.
- SMIN/SMAX: Signed compare reduction for parser state branches (e.g., "is this character in the range A-Z or a-z?"). No P1 hot leaf identified. Defer unless new profiling shows compare-intensive codepath.
- EOR3/BCAX: Three-input boolean for quote/escape/control fusion. No P1 expression identified. Defer until string-special wave has measured CSS evidence.

**Rank:** Not ranked (deferred inventory). May become candidate 8/9 if profiling surfaces new hot leaf in string/escape/compare.

---

## §6 Lock 16 + §2.2 Continued Discipline

**Authority:** SK-V12 USER PIN §D5, SPEC §2.2 (Lock 16 micro-prove-first), SPEC §8 (SIMD/ASM category admission).

### Admission Gate for SK-V13 SIMD/ASM Additions

Every new primitive (or re-routed existing primitive like W4b's `a64_ascii_set_run_skip` production split) must satisfy:

1. **Scalar reference:** Executable, deterministic, no proof-only fallback. If new primitive, standalone scalar function with full parameter coverage. If existing primitive being re-routed (e.g., W4b), scalar must have been proved in prior wave (W4 microbench proof).

2. **Checkasm parity:** Bit-for-bit equivalence or bounded-error mathematical equivalence (e.g., floating rounding). Checkasm test must exercise:
   - Boundary cases (empty input, single-byte, window boundaries)
   - Alignment variants (0-15 byte offset within cache line, unaligned 64-bit boundaries)
   - All-zero, all-ones, sparse/dense input patterns
   - Grammar-specific inputs if grammar-dependent (JSON quote patterns, CSS delimiter sets, Sheets schema constraints)
   - Invalid/error cases if applicable (invalid UTF-8, out-of-range nibble, overflow)

   Checkasm count: ~150+ test cases minimum for window-based primitives; ~50+ for narrow utilities.

3. **Same-wave production consumer:** Named in the source tree (not in telemetry-only paths, not in `parse_only` diagnostic paths). The consumer must:
   - Exist as a callable function or generated code path (not a design goal for future wave)
   - Execute in the same campaign wave as the primitive admission (no deferral of consumer to future campaign)
   - Be measured in the same wave (Criterion microbench or gate throughput) on the target row (JSON guard, JSON direct, CSS L4, or Sheets if applicable)

   **Exception (one-time, SK-V12 W4 only):** `a64_ascii_set_run_skip` passed microbench and deferred production split to SK-V13. SK-V13 W4b must complete the split in the same wave. No further production-split deferral is permitted.

### No Orphans Rule (Strict Carry)

New primitives must not create orphans. If a primitive is implemented (aarch64 file exists in the tree) but has no same-wave consumer (or no consumer at all), it is immediately eligible for demotion or removal before close. Orphan demotion is permitted (as SK-V12 did with the five orphans) ONLY if the primitive is explicitly documented in `REDRESS.md` with evidence (file exists, scalar ref exists, checkasm exists, but consumer is missing or blocked by REDRESS entry).

**Example (valid):** Demoting `bitmap_next_set_bit` in SK-V12 W4 was valid because the primitive existed in the tree, had scalar ref + checkasm, but had no production consumer and CTZ was REDRESS-blocked.

**Example (invalid):** If SK-V13 W5 implements a new `a64_foo_bar` primitive for consumption by a future SK-V14 wave, the primitive becomes an orphan at SK-V13 close. Not permitted. W5 must either (1) skip the primitive, or (2) include the consumer in W5 and measure the row in the same wave.

### W4's `inventory_demoted_with_evidence` Disposition Is History Only

The five SK-V12 W4 orphan demotions (bitmap_*, bulk_emit_*, byte_context, cache_hints) are case-closed. They remain in the source tree as historical inventory (not deleted) but are not "admitted" primitives. New attempts to emit similar structures (prefix-XOR, CTZ bulk, cross-chunk shifts, store hints) must:
1. Cite the corresponding SK-V12 REDRESS entry (88, 89, 126, etc.)
2. Name a material differential (e.g., "union candidate C gates CTZ through PMULL matrix, avoiding scalar iteration entirely" vs "REDRESS-89 scalar CTZ without matrix")
3. Pass CHALLENGE (CH1–CH6) with the new differential as the distinguishing factor
4. Wire same-wave consumer (per §2.2)

---

**End of scoping document.**

