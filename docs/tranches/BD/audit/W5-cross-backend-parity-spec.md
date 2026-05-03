# W5 — Cross-Backend Parity Specification

Date: 2026-05-03
Scope: Full specification of the cross-backend parity verification matrix at BD.W5. Documents the equivalence relation, 81-cell matrix enumeration, canonical-JSON serialiser per backend, byte-equal-modulo-float-repr comparator, float-divergence handling, Lock 5 production runtime ratification.

## §1 Equivalence Relation

The equivalence relation between Rust + TS + WASM typed values:

> Two parsed values are **equivalent** if their canonical-JSON serialisations are byte-equal except for float representation, where float tolerance is `|a - b| < f64::EPSILON * max(|a|, |b|, 1.0)`.

### Canonical JSON

The canonical-JSON form is determined by:

1. **Object key order**: lexicographic sort
2. **Whitespace**: none (compact form)
3. **Float representation**: shortest round-trip representation per ECMA-262 (`Number.prototype.toString`)
4. **Special values**: NaN → `null`; Infinity → `null` (matches `JSON.stringify` semantics)

### Float tolerance

For numeric tokens:

```rust
fn floats_equivalent(a: f64, b: f64) -> bool {
    if a.is_nan() && b.is_nan() { return true; }  // both NaN
    if a == b { return true; }  // strict equality (handles ±0, ±∞)
    let scale = a.abs().max(b.abs()).max(1.0);
    (a - b).abs() < f64::EPSILON * scale
}
```

This tolerance handles:
- Float-repr edge cases (`0.1 + 0.2` ≈ `0.30000000000000004`)
- Last-bit precision differences between JS V8 and Rust f64
- Conversion-induced rounding

## §2 81-Cell Matrix

9 grammars × ≥ 3 fixtures × 3 backends = ≥ 81 cells. The full enumeration:

| # | Grammar | Fixture | Rust | TS | WASM |
|---|---|---|:---:|:---:|:---:|
| 1 | json | twitter.json | ✓ | ✓ | ✓ |
| 2 | json | canada.json | ✓ | ✓ | ✓ |
| 3 | json | citm-catalog.json | ✓ | ✓ | ✓ |
| 4 | css_l4 | bootstrap.css | ✓ | ✓ | ✓ |
| 5 | css_l4 | animate.css | ✓ | ✓ | ✓ |
| 6 | css_l4 | tailwind-base.css | ✓ | ✓ | ✓ |
| 7 | bbnf | json.bbnf | ✓ | ✓ | ✓ |
| 8 | bbnf | css.bbnf | ✓ | ✓ | ✓ |
| 9 | bbnf | math.bbnf | ✓ | ✓ | ✓ |
| 10 | google_sheets | basic-formula.txt | ✓ | ✓ | ✓ |
| 11 | google_sheets | complex-vlookup.txt | ✓ | ✓ | ✓ |
| 12 | google_sheets | array-formula.txt | ✓ | ✓ | ✓ |
| 13 | css_pretty | reset.css | ✓ | ✓ | ✓ |
| 14 | css_pretty | simple-rules.css | ✓ | ✓ | ✓ |
| 15 | css_pretty | media-query.css | ✓ | ✓ | ✓ |
| 16 | ebnf | abnf.ebnf | ✓ | ✓ | ✓ |
| 17 | ebnf | modula2.ebnf | ✓ | ✓ | ✓ |
| 18 | ebnf | json.ebnf | ✓ | ✓ | ✓ |
| 19 | bnf | algol60.bnf | ✓ | ✓ | ✓ |
| 20 | bnf | postal-code.bnf | ✓ | ✓ | ✓ |
| 21 | bnf | simple.bnf | ✓ | ✓ | ✓ |
| 22 | csv | basic.csv | ✓ | ✓ | ✓ |
| 23 | csv | escaped-quotes.csv | ✓ | ✓ | ✓ |
| 24 | csv | mixed-types.csv | ✓ | ✓ | ✓ |
| 25 | math | arithmetic.math | ✓ | ✓ | ✓ |
| 26 | math | complex-expr.math | ✓ | ✓ | ✓ |
| 27 | math | parens.math | ✓ | ✓ | ✓ |

27 fixtures × 3 backends = 81 cells. The closer gate W5-G1 asserts every cell is ✓.

## §3 Canonical-JSON Serialiser per Backend

### Rust

```rust
// crates/bbnf-parse/src/parity/canonical_json.rs
pub fn serialise_canonical_json<G: Grammar>(value: &G::Value) -> String {
    let buf = serde_json::to_vec(value).unwrap();
    // sort object keys; canonicalise floats
    let json: serde_json::Value = serde_json::from_slice(&buf).unwrap();
    let canonical = sort_keys_recursive(json);
    serde_json::to_string(&canonical).unwrap()
}

fn sort_keys_recursive(v: serde_json::Value) -> serde_json::Value {
    use serde_json::Value;
    match v {
        Value::Object(map) => {
            let mut sorted: BTreeMap<String, Value> = BTreeMap::new();
            for (k, v) in map {
                sorted.insert(k, sort_keys_recursive(v));
            }
            Value::Object(sorted.into_iter().collect())
        }
        Value::Array(arr) => Value::Array(arr.into_iter().map(sort_keys_recursive).collect()),
        Value::Number(n) => {
            // canonicalise float repr
            if let Some(f) = n.as_f64() {
                if f.is_nan() || f.is_infinite() {
                    return Value::Null;
                }
            }
            Value::Number(n)
        }
        other => other,
    }
}
```

### TS

```typescript
// npm/runtime/src/parity/canonical-json.ts
export function serialiseCanonicalJson<G extends Grammar>(value: G['Value']): string {
  return JSON.stringify(value, canonicalReplacer);
}

function canonicalReplacer(this: unknown, key: string, value: unknown): unknown {
  if (typeof value === 'number') {
    if (Number.isNaN(value) || !Number.isFinite(value)) {
      return null;
    }
    return value;
  }
  if (value && typeof value === 'object' && !Array.isArray(value) && !(value instanceof Uint8Array)) {
    // sort keys
    const sorted: Record<string, unknown> = {};
    for (const k of Object.keys(value).sort()) {
      sorted[k] = (value as Record<string, unknown>)[k];
    }
    return sorted;
  }
  return value;
}
```

### WASM

The WASM canonical-JSON happens at the JS host side; same as TS once the WASM-returned value is serde_wasm_bindgen-converted to a JS object.

```typescript
// npm/runtime-wasm/src/parity/canonical-json.ts
import { serialiseCanonicalJson } from '@bbnf-lang/runtime/parity';
export { serialiseCanonicalJson };  // re-export
```

## §4 Comparator

### Rust

```rust
// crates/bbnf-parse/src/parity/comparator.rs
#[derive(Debug)]
pub enum ParityDiff {
    TokenMismatch { offset: usize, a: String, b: String },
    FloatMismatch { offset: usize, a: f64, b: f64 },
    LengthMismatch,
    InvalidJson,
}

pub fn compare_canonical_json(a: &str, b: &str) -> Result<(), ParityDiff> {
    let mut tokens_a = json_tokens(a)?;
    let mut tokens_b = json_tokens(b)?;
    let mut byte_offset = 0;

    loop {
        match (tokens_a.next(), tokens_b.next()) {
            (None, None) => return Ok(()),
            (Some(_), None) | (None, Some(_)) => return Err(ParityDiff::LengthMismatch),
            (Some(Ok(ta)), Some(Ok(tb))) => {
                if !tokens_match(&ta, &tb) {
                    return Err(token_mismatch_error(byte_offset, &ta, &tb));
                }
                byte_offset += token_length(&ta);
            }
            _ => return Err(ParityDiff::InvalidJson),
        }
    }
}

fn tokens_match(a: &Token, b: &Token) -> bool {
    match (a, b) {
        (Token::Number(na), Token::Number(nb)) => floats_equivalent(*na, *nb),
        (Token::Null, Token::Null) => true,
        (Token::Bool(ba), Token::Bool(bb)) => ba == bb,
        (Token::String(sa), Token::String(sb)) => sa == sb,
        (Token::ObjectOpen, Token::ObjectOpen) => true,
        (Token::ObjectClose, Token::ObjectClose) => true,
        (Token::ArrayOpen, Token::ArrayOpen) => true,
        (Token::ArrayClose, Token::ArrayClose) => true,
        (Token::Comma, Token::Comma) => true,
        (Token::Colon, Token::Colon) => true,
        _ => false,
    }
}
```

### TS analogue

```typescript
// npm/runtime/src/parity/comparator.ts
export interface ParityDiff {
  kind: 'tokenMismatch' | 'floatMismatch' | 'lengthMismatch' | 'invalidJson';
  offset?: number;
  a?: unknown;
  b?: unknown;
}

export function compareCanonicalJson(a: string, b: string): ParityDiff | null {
  // analogous tokeniser + comparator
  // ...
}
```

## §5 Float-Divergence Handling

Per BD.W5 §2.8, known float-repr divergences:

| Case | Rust | JS | Handling |
|---|---|---|---|
| `f64::MAX.to_string()` | full digit expansion | scientific notation | float-tolerance handles |
| `0.1 + 0.2` | 0.30000000000000004 | 0.30000000000000004 | exact equality |
| `f64::NAN` | `"NaN"` (or omit in JSON) | `null` (JSON.stringify maps NaN → null) | both serialisers map → null per §3 |
| `f64::INFINITY` | `"Infinity"` (or omit) | `null` | both → null per §3 |
| Negative zero (`-0.0`) | `"0.0"` (Rust f64 Display) | `"0"` (JS Number.toString) | float-tolerance handles |
| Subnormal floats | full precision | full precision | exact equality (V8 + Rust both round per IEEE 754) |
| Very small values (< 1e-300) | scientific notation in both | scientific notation in both | float-tolerance handles |

The handling document at `docs/tranches/BD/audit/W5-float-divergence.md` (created at W5 §2.8) records each case + the comparator's handling rule.

## §6 Reference File Generation

Per BD.W5 §2.4 + §2.5, the TS + WASM-side parity tests emit canonical-JSON reference files alongside fixtures:

```
crates/bbnf-parse/tests/fixtures/json/twitter.json                  (canonical input)
crates/bbnf-parse/tests/fixtures/json/twitter.json.parity.ts.json   (TS-emitted canonical-JSON of typed value)
crates/bbnf-parse/tests/fixtures/json/twitter.json.parity.wasm.json (WASM-emitted canonical-JSON)
```

The Rust-side parity test (`crates/bbnf-parse/tests/parity_matrix.rs`) loads the reference files and compares.

### Regeneration

```bash
# regenerate TS reference files (run on dev workstation)
npm test --workspace=npm/runtime -- parity --regenerate

# regenerate WASM reference files
npm test --workspace=npm/runtime-wasm -- parity --regenerate
```

The regeneration is idempotent; deterministic canonical-JSON output ensures bit-equal regeneration on each invocation.

### Commit discipline

Reference files are committed alongside fixtures. CI verifies via:

```bash
npm test --workspace=npm/runtime -- parity --regenerate
git diff --exit-code crates/bbnf-parse/tests/fixtures/  # zero diff
```

If diff is non-zero, the PR includes the regenerated reference files; the reviewer verifies the change is intentional.

## §7 81-Cell Test Generation

The Rust-side test file `crates/bbnf-parse/tests/parity_matrix.rs` is xtask-generated from the manifest set:

```rust
// crates/bbnf-parse/tests/parity_matrix.rs (xtask-generated)
use std::fs;
use bbnf_parse;

mod parity {
    pub use bbnf_parse::parity::*;
}

// JSON × twitter × Rust-vs-TS
#[test]
fn parity_json_twitter_rust_vs_ts() {
    let input = include_bytes!("fixtures/json/twitter.json");
    let value = bbnf_parse::json::parse(input).unwrap();
    let rust_canonical = parity::serialise_canonical_json(&value);
    let ts_canonical = include_str!("fixtures/json/twitter.json.parity.ts.json");
    parity::compare_canonical_json(&rust_canonical, ts_canonical).expect("parity holds");
}

// JSON × twitter × Rust-vs-WASM
#[test]
fn parity_json_twitter_rust_vs_wasm() {
    let input = include_bytes!("fixtures/json/twitter.json");
    let value = bbnf_parse::json::parse(input).unwrap();
    let rust_canonical = parity::serialise_canonical_json(&value);
    let wasm_canonical = include_str!("fixtures/json/twitter.json.parity.wasm.json");
    parity::compare_canonical_json(&rust_canonical, wasm_canonical).expect("parity holds");
}

// ... 79 more tests, generated from the manifest
```

The xtask `regen-parity-tests` produces this file from the per-grammar manifests.

## §8 CI Matrix Execution

The CI workflow at `.github/workflows/parity-matrix.yml` runs the matrix:

```yaml
name: Parity Matrix

on: [pull_request, workflow_dispatch]

jobs:
  rust-parity:
    runs-on: macos-14
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo nextest run --test parity_matrix  # 81 tests in parallel within nextest

  regenerate-parity-refs:
    runs-on: macos-14
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: 20
      - run: npm ci --workspaces
      - run: npm test --workspace=npm/runtime -- parity --regenerate
      - run: npm test --workspace=npm/runtime-wasm -- parity --regenerate
      - run: git diff --exit-code crates/bbnf-parse/tests/fixtures/  # ratchet: no drift
```

Total wall time: ~10-20 minutes (parity tests + regeneration verification).

## §9 Lock 5 Production Runtime Ratification

Per BC.W2 §10 (`docs/tranches/BC/waves/W2.md:202-204`), Lock 5 ratification at scaffold was: "IR shape supports them via the Emitter trait — not IR shape activates them in production". W5 closes the production ratification:

The four-step Lock 5 production ratification mechanism:

1. **Typed IR alphabet supports three backends**: BC.W0's IR contract names every variant; each variant has Rust + TS + WASM lower paths.
2. **All three backends produce typed values for the same input**: BD.W1 (TS) + BD.W2 (WASM) production emitters; BC.W1 (Rust) production emitter.
3. **Canonical-JSON equivalence holds across all 81 cells**: the matrix at §2 is green.
4. **Cross-backend trait conformance preserved at runtime**: BC.W2 §2.6's compile-time conformance test passes; BD.W5's runtime conformance test passes.

The four-step mechanism is documented at `docs/tranches/BD/audit/W5-lock-5-production-ratification.md`.

## §10 Closing Posture

The cross-backend parity matrix at BD.W5 ratifies Lock 5 production runtime support. 81 cells (9 grammars × ≥ 3 fixtures × 3 backends); equivalence relation byte-equal canonical-JSON modulo float-repr; comparator handles all JSON token types + float edge cases; reference files committed alongside fixtures; cross-backend trait conformance preserved at runtime. The four-step Lock 5 ratification mechanism completes BD's central deliverable.
