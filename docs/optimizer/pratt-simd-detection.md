# Optimizer — Pratt + SIMD Auto-Detection

bbnf-lang's optimiser auto-detects Pratt parsing and SIMD scanner emission from grammar shape — no `@pratt` or `@simd` directives, per Lock 10. This page covers: the detection model (§1), the Pratt heuristic (§2), the SIMD heuristic (§3), worked examples (§4), and the `--explain-optimizer` diagnostic (§5).

## §1 Model

Auto-detection is a pipeline stage in the BB.W3 optimiser composition:

```text
   GrammarIR (post-layout-lowering)
         |
         v
   Recogniser miners
       |   crates/ir/src/passes/recognizers/operator_chain.rs    → OperatorChainFacts
       |   crates/ir/src/passes/sets/structural_alphabet.rs      → StructuralAlphabetFacts
       |   crates/ir/src/passes/recognizers/delim_scan.rs         → DelimScanFacts
       v
   E-graph saturation (rewrite rules apply at this stage)
         |
         v
   Cost-model extraction
       |   crates/egraph/src/extract.rs reads facts; decides per-rule strategy
       v
   CSP strategy selection
       |   crates/ir/src/passes/csp_strategy/mod.rs solves over decisions
       v
   Codegen
       |   per-rule emission picks Pratt fn / SIMD scanner / scalar dispatch
```

The miners are fact producers; the cost model is the decider. Miners do not decide whether to emit Pratt or SIMD; they classify rules + grammars by structural shape and pass facts forward. The cost model integrates per-rule emit cost, per-rule call frequency (from grammar coverage), and the alternative emit cost; it solves for the minimum-cost strategy per rule.

## §2 Pratt heuristic

Pratt parsing emerges from rules with **left-recursive operator-chain shape**:

```bbnf
expr := factor (op factor)*  // left-associative
   | (op_high factor)? factor  // for unary prefix
;
```

The detection at `crates/ir/src/passes/recognizers/operator_chain.rs` walks the IR and identifies rules where:

1. **Shape**: the rule has the form `expr := factor (op factor)+` with at least one repeating operator-chain segment.
2. **Operator closure**: `op` is a closed enumerated set (e.g., `op := '+' | '-' | '*' | '/'`); not an open recursion.
3. **Left-recursion**: the rule references itself only via the leftmost branch (left-associative); right-recursion classifies as right-associative Pratt.
4. **Chain depth**: at least 2 operators in the closed set; rules with single operators fall through to recursive descent.

Detection emits an `OperatorChainFacts { precedence, associativity, op_set, chain_depth }` record. The cost model decides Pratt vs. non-Pratt emission based on:

| Input fact | Pratt-favoured | Non-Pratt-favoured |
|---|---|---|
| Chain depth | ≥ 2 ops | < 2 ops |
| Op-set cardinality | ≥ 3 closed | open/unbounded |
| Left-recursion present | yes | no |
| Branch alt cardinality | small (≤ 4) | large (≥ 8) |
| Cost of recursive-descent fallback | high (deep AST) | low (flat AST) |

When all four "Pratt-favoured" rows fire, the cost model emits Pratt. The fallback cost model accounts for false-positive Pratt classifications: if a rule's actual runtime profile shows recursive-descent winning, the `--explain-optimizer` diagnostic surfaces the false-positive cost as a hint for grammar-level refactoring.

### Pratt emit shape

```rust
// Generated for a Pratt-classified rule (e.g., binary_factor)
fn parse_pratt_<rule>_expression<'i, 'p>(
    bytes: &[u8], p: &mut usize, state: &mut ScanState,
    arena: &mut <G>Arena<'p>, cursor: &mut PathCursor<'_, '_>,
    min_precedence: u8,
) -> Result<<G>TypedValue<'p>, ParseErr> {
    let mut left = parse_<factor>(bytes, p, state, arena, cursor)?;
    loop {
        let op = peek_op(bytes, *p)?;
        let prec = op.precedence();
        if prec < min_precedence { break; }
        *p += op.len();
        let right = parse_pratt_<rule>_expression(
            bytes, p, state, arena, cursor,
            prec + op.right_associativity_offset()
        )?;
        left = <G>TypedValue::BinOp { left: arena.alloc(left), op, right: arena.alloc(right) };
    }
    Ok(left)
}
```

The emit is parametric over `min_precedence`; recursive descent at increasing precedence levels resolves operator binding. The codegen emits one `parse_pratt_<rule>_expression` per Pratt-classified rule.

## §3 SIMD heuristic

SIMD emission emerges from rules with **structural-alphabet density**:

```text
   FIRST set of rule R                             # bytes that initiate any branch of R
   structural alphabet of R                        # bytes that distinguish branches at top level
   alphabet density = |alphabet| / |FIRST set|     # 1.0 = each byte distinguishes; lower = ambiguous
```

The detection at `crates/ir/src/passes/sets/structural_alphabet.rs` mines the structural alphabet by walking each rule's top-level Alt branches and recording which bytes initiate which branches. JSON's top-level dispatch:

```bbnf
value := object | array | string | number | boolean | null
       // FIRST = { '{', '[', '"', '-' | digit, 't' | 'f', 'n' }
       // structural alphabet = { '{', '[', '"', '0'..'9', 't', 'f', 'n' }
       // alphabet density = 1.0 (each branch distinguishes)
```

The cost model decides SIMD vs. scalar emission based on:

| Input fact | SIMD-favoured | Scalar-favoured |
|---|---|---|
| Alphabet cardinality | ≥ 6 distinct bytes | < 6 distinct bytes |
| Alphabet density | ≥ 0.8 | < 0.8 |
| Expected input length | ≥ 1 KB | < 1 KB |
| Per-byte dispatch frequency | ≥ 1 hit per 16 bytes | ≤ 1 hit per 64 bytes |
| SIMD setup cost | amortised over input | not amortised |

The `simd_threshold_bytes` parameter is grammar-derived:

```text
simd_threshold_bytes = α / structural_alphabet.cardinality + β / first_set.density
                     # α = 4096 (chosen so cardinality 4 → threshold 1024 bytes)
                     # β = 8192 (chosen so density 0.5 → +16 KB threshold lift)
```

For JSON (cardinality 8, density 1.0): threshold ≈ 512 + 8192 = 8704 bytes; SIMD always wins on twitter.json (601 KB).

For CSV (cardinality 2, density 1.0): threshold ≈ 2048 + 8192 = 10240 bytes; SIMD almost never wins (CSV inputs typically < 10 KB).

For CSS L4 (cardinality 25, density 0.7): threshold ≈ 164 + 11700 = 11864 bytes; SIMD wins on bootstrap.css (132 KB).

### SIMD emit shape

```rust
// Generated for a SIMD-classified rule
fn scan_<rule>_simd<'i>(bytes: &[u8], start: usize) -> usize {
    use simd_scan::ScanAlphabet;
    let alphabet = const { ScanAlphabet::from_bytes(&[b'{', b'}', b'[', b']', b',', b':']) };
    simd_scan::find_first_in_alphabet(bytes, start, &alphabet)
}
```

The `simd_scan` kernel selector at `crates/simd-scan/src/lib.rs` provides AVX-512, AVX2, NEON, scalar-fallback variants; the runtime CPU dispatch chooses the kernel.

## §4 Worked examples

### Example 1: BBNF `binary_factor` (Pratt)

```bbnf
binary_factor := factor (binary_op factor)*
binary_op := '+' | '-' | '*' | '/'
factor := number | identifier | '(' expr ')'
```

Detection:
- Shape: `factor (binary_op factor)*` matches the operator-chain pattern.
- Op-set closure: `binary_op` enumerates 4 operators.
- Left-recursion: yes (the `(binary_op factor)*` repeat is left-associative).
- Chain depth: 4 operators ≥ 2.

Cost model: emits `parse_pratt_bbnf_binary_factor`. The bench shows Pratt wins on inputs ≥ 4 nested operators.

### Example 2: CSV (no Pratt, no SIMD)

```bbnf
csv := row ('\n' row)*
row := field (',' field)*
field := quoted_field | unquoted_field
```

Detection:
- Operator chain: NO (no operators).
- Structural alphabet: cardinality 2 (`,`, `\n`).

Cost model: scalar dispatch. SIMD threshold ≈ 10 KB; CSV typical input < 10 KB → scalar wins.

### Example 3: JSON `value` (SIMD; no Pratt)

```bbnf
value := object | array | string | number | boolean | null
```

Detection:
- Operator chain: NO.
- Structural alphabet: cardinality 8 (`{`, `[`, `"`, digit, `-`, `t`, `f`, `n`).

Cost model: SIMD scan for top-level dispatch on inputs ≥ 8 KB; scalar below threshold. Twitter.json (601 KB) → SIMD wins; small inline JSON literals (< 8 KB) → scalar.

### Example 4: CSS L4 declaration block (SIMD)

```bbnf
declaration_block := '{' (declaration ';')* '}'
declaration := property_name ':' value '!'? 'important'?
```

Detection:
- Operator chain: NO.
- Structural alphabet: cardinality 7 at the declaration boundary (`;`, `}`, `:`, `!`, ...) plus the property-name dispatch alphabet.

Cost model: SIMD scan for `;` / `}` boundaries; scalar for property-name parsing (PHF for ~250 named properties). Bootstrap.css → SIMD scan amortises; the BB-G1 ≤ 3.5 ms target depends on this.

## §5 `--explain-optimizer` diagnostic

The `xtask explain-optimizer <grammar>::<rule>` command surfaces the cost-model decision for each rule:

```sh
$ xtask explain-optimizer bbnf::binary_factor
Rule: bbnf::binary_factor
  Pratt-classification: YES
    op-set: {'+', '-', '*', '/'}, cardinality 4
    chain-depth: 2 (left-recursive)
    associativity: left
    cost: 0.18 (Pratt) vs 0.32 (recursive-descent) → Pratt selected
  SIMD-classification: NO (alphabet cardinality 4 < threshold 6)

$ xtask explain-optimizer json::value
Rule: json::value
  Pratt-classification: NO (no operators)
  SIMD-classification: YES
    structural alphabet: {'{', '[', '"', digit, '-', 't', 'f', 'n'}, cardinality 8
    alphabet density: 1.0
    threshold: 8704 bytes
    cost: 0.05 (SIMD@8KB) vs 0.21 (scalar@8KB) → SIMD selected
```

The diagnostic is consumed by grammar authors investigating optimiser misfires (the F07-E5, F07-E6 verbatim warnings per `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:28-29`):

```text
warning: rule 'bbnf::custom_op_chain' was not lowered with Pratt because branch 'fallback' is not an operator-chain segment;
         emitted recursive descent instead
   --> grammar/bbnf/bbnf.bbnf:42
    |
 42 |     custom_op_chain := factor (op factor)* | factor fallback
    |                                            -- non-chain alt branch detected
    |
help: split into separate rules
    |     custom_op_chain := factor (op factor)*
    |     custom_op_chain_with_fallback := custom_op_chain fallback?
```

```text
note: SIMD scanner not emitted for rule 'csv::field';
      estimated input length < 10240 bytes and scalar dispatch is cheaper for this leaf pattern
```

## §6 Pratt-misfire mitigation

If the cost model classifies a non-Pratt rule as Pratt (false positive), the emitted `parse_pratt_<rule>_expression` runs but the rule's runtime profile shows it underperforming the recursive-descent fallback. Detection: bench measures Pratt cost > recursive-descent cost for representative inputs.

The mitigation:
1. **Negative test fixtures**: BB.W3c M3 enumerates non-Pratt rules with operator-chain shape and verifies they DO NOT route to Pratt (e.g., a rule named `additive_expr` whose body is structurally similar but not actually left-recursive).
2. **Cost model accounts for false-positive cost**: each Pratt classification carries an expected cost; if the actual cost exceeds the expectation, the cost model demotes the classification on the next regen.
3. **Author override** (last-resort): the grammar author can split the rule into separate productions to prevent classification (per the F07-E5 fix-it).

## §7 SIMD threshold tuning

The α and β coefficients in §3 are tuned against representative input sizes per grammar:

| Grammar | α calibration | β calibration | Resulting threshold |
|---|---:|---:|---:|
| JSON | 4096 | 8192 | 8704 bytes (twitter, citm, canada all > threshold) |
| CSS L4 | 4096 | 8192 | 11864 bytes (bootstrap, tailwind > threshold; small `<style>` blocks < threshold) |
| CSV | 4096 | 8192 | 10240 bytes (typical CSV < threshold; scalar wins) |
| BBNF | 4096 | 8192 | ~6500 bytes (BBNF source files vary; threshold falls in mid-range) |

The coefficients are NOT user-tunable per Lock 10; they live at `crates/egraph/src/cost_model/simd_thresholds.rs` and update via grammar-coverage profiling at BB.W3c M4. The `--explain-optimizer` diagnostic surfaces the chosen threshold per rule; if a grammar author observes systematic SIMD misfires, the diagnostic guides toward grammar-level refactoring (e.g., splitting a high-cardinality alphabet rule into smaller alternatives) rather than directive-based override.

## §8 Cross-references

- `crates/ir/src/passes/recognizers/operator_chain.rs` — Pratt detection mining
- `crates/ir/src/passes/sets/structural_alphabet.rs` — SIMD detection mining
- `crates/egraph/src/extract.rs` — cost-model extraction
- `crates/ir/src/passes/csp_strategy/mod.rs` — strategy selection
- `crates/core/src/codegen/rust/emitter/shapes/pratt/struct_direct.rs` — Pratt fn codegen
- `crates/simd-scan/src/lib.rs` — SIMD kernel selector
- `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:28-29` — F07-E5, F07-E6 verbatim warnings
- `docs/tranches/BB/audit/W3-rank-tier-with-consumer.md` — Era V abrogation evidence for the same-wave consumer rule
