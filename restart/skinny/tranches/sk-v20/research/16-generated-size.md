# Generated-Code Budget Audit

## Census: Both Trees

### Full tree — `crates/core/src/grammar/generated/`

| Grammar | Generated `.rs` | Registry `.json` | PATH_PLAN entries | Source `.bbnf` | Expansion ratio |
|---------|----------------|------------------|-------------------|----------------|-----------------|
| `css_l4` | 108,406 | 8,742 | 1,054 | 1,329 (15 files) | 82× |
| `bbnf` | 21,557 | 2,232 | 257 | 133 (3 files) | 162× |
| `google_sheets` | 13,861 | 1,250 | 148 | 185 | 75× |
| `ebnf` | 7,659 | 1,261 | 43 | 51 | 150× |
| `css_pretty` | 9,052 | 545 | 82 | 68 | 133× |
| `bnf` | 3,304 | 177 | 30 | ~30 | 110× |
| `json` | 3,505 | 205 | 21 | 20 | 175× |
| `csv` | 1,702 | 102 | 12 | ~20 | 85× |
| `math` | 875 | 19 | 1 | ~20 | 44× |
| **Total** | **169,921** | **14,533** | | | |

Grand total with JSON sidecars: **184,454 lines**.

### Skinny tree — `skinny/crates/runtime/src/grammars/`

| Grammar | `generated.rs` |
|---------|----------------|
| `sheets` | 1,591 |
| `json` | 960 |
| `css_l4_declaration_values` | 886 |
| **Total** | **3,437** |

The skinny tree also carries `bbnf-bench/src/generated_real_typed.rs` (4,321 lines) and
`bbnf-bench/src/lock14_baseline.rs` (5,283 lines, not truly generated — see below).

`skinny/Cargo.toml` declares `generated_loc_budget = 4000` for the JSON grammar in
`[workspace.metadata.bbnf.grammars.json.codegen]`. No enforcement code consumes this field
anywhere in the codebase — it is decorative. The full tree has no `generated_loc_budget`
field at all.

---

## The 108k Outlier: `css_l4.rs`

`css_l4.rs` is **108,406 lines**, the single largest generated file by a factor of five
over the next largest (`bbnf.rs` at 21,557). The file is not superlinear relative to its
grammar input — it is roughly proportional — but it crossed the 15k budget threshold by 7×.

### Growth trajectory (committed versions)

| Commit | Lines | Delta | Driver |
|--------|-------|-------|--------|
| `cb3a40d55` (AZ-IV.W0.3) | 85,618 | baseline | fixed lowering quartet |
| `bc9996c3a` (W1.2) | 88,227 | +2,609 | typed-leaf codegen lift |
| `9807343a7` (W1.9) | 89,534 | +1,307 | emitter lifts |
| `1c75f55c5` (W1-CLOSE.B) | 90,865 | +1,331 | alt-dispatch payload |
| `d89a9dd9b` (W1-CLOSE.A) | 91,185 | +320 | operator-chain drift |
| `937361d56` (W3.3) | 97,541 | +6,356 | **PATH_PLAN static — 1,054 entries × 6 lines** |
| `9cef79c56` (W4 merge) | 101,992 | +4,451 | W4 cherry-pick integration |
| `d8413f68e` (AZ-IV.W5-T4) | 107,138 | +5,146 | registry+xtask routing |
| `ea8138056` (W5 parity) | 108,182 | +1,044 | cssparser typed-summary parity |
| `f4b6f757c` (W6) | 108,406 | +224 | LazyLock hoist |

The first committed version of `css_l4.rs` was **203,499 lines** at `6d44aa21c` (B2.W1.regen).
The subsequent reduction to 85k was a genuine win. The re-inflation from 85k to 108k over
AZ-IV is driven by three compounding mechanisms detailed below, none of which are superlinear
per se, but all of which materialise unconditionally regardless of grammar complexity.

### Primary growth drivers

**PATH_PLAN static inflation.** The W3.3 commit (`937361d56`) added a `PATH_PLAN` static of
1,054 `PathPlanEntry` structs rendered as 6-line struct literals, consuming 6,324 lines
(`crates/core/src/grammar/generated/css_l4.rs:1215`). The path plan is O(rules) — CSS L4's
576 rules produce ~1,054 entries. The struct-literal form is the most verbose possible
representation; a flat byte array with a `const fn` constructor would shrink this by 5×.

**Per-rule LazyLock StructLayout statics.** The current file contains 208 `LazyLock`
instances (`grep -c "LazyLock"`). Most are per-rule `StructLayout` statics of the form
`static __FOO_LAYOUT: LazyLock<StructLayout> = LazyLock::new(|| StructLayout { ... })`.
Each occupies 10–40 lines depending on field count. Three such statics are visible at lines
24,810, 27,376, and 31,585; the census is O(rules-with-typed-projection). The W6 commit
hoisted the flat `StructLayout` to `LazyLock` across the file.

**Deep nesting.** The emitter renders deeply nested match/if bodies in-place rather than as
named sub-functions. Maximum indentation in `bbnf.rs` is 120 spaces (30 levels at 4-space
indent, at line 15,477). This inflates physical line count without semantic content — each
expression body occupies 5–10 lines that a named auxiliary function would collapse to one
call site.

---

## AVX2/x86_64 Scanner in an aarch64-Only Project

The `emit_support_module` function at
`crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:426` emits a
`__shape_support_<Grammar>` module into **every generated grammar file** unconditionally.
That module contains:

- `nospace_bitmap_64_avx2`: ~30 lines of `core::arch::x86_64` intrinsics
  (`support.rs:597`), protected by `#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]`.
- `first_quote_or_backslash_avx2`: ~33 lines of the same (`support.rs:685`).

Both functions are emitted into all 9 grammars. The project is declared aarch64/Apple-M only;
x86_64 is out of scope. The aarch64 equivalents (`_neon` variants) are the actually-executed
paths. The AVX2 bodies are dead weight on the production target, and their emission into every
grammar file violates the arch-scope precept.

The structural-scan emitter at `crates/core/src/generate/regex/emit/simd.rs:140` generates
`'avx2_scan: { ... }` labeled-break blocks, which appear 4 times in `bbnf.rs` (lines 16,080,
16,255, 17,134, and 21,217). These are the `break 'label { expr }` constructs flagged in the
task brief. The `#![allow(clippy::all)]` suppression at the file header masks any
`clippy::semicolon_inside_block` or related diagnostics. No other grammar currently receives
the structural bitmap kernel — only `bbnf.rs`, because only the BBNF grammar mines a
structural alphabet through `emit_structural_bitmap_kernel` in `simd.rs`.

The `simd-scan` library crate at `crates/simd-scan/src/` provides `avx2.rs` (370 lines) and
`avx512.rs` (149 lines) as separate `#[cfg]`-gated modules (`lib.rs:58–61`). These are
appropriate: the library is general-purpose and the gates correctly exclude them on aarch64.
The problem is limited to the code emitter (`support.rs`) copying AVX2 bodies verbatim into
each generated grammar file, which is an emitter-side architecture decision, not a library
decision.

The `skinny/crates/bbnf-simd/src/` correctly contains only `aarch64/` and `scalar/` — no
`x86_64/` directory. The x86 exclusion is confirmed structurally. The `report.rs` references
to `x86_64` and `avx2` at lines 1,678 and 10,578 are inside a validation gate that
**rejects** x86 admission evidence; those are policing code, not violations.

### Dead first_quote_or_backslash emissions

`first_quote_or_backslash` is a JSON/string-scanning primitive. Among the 9 grammars:

| Grammar | Calls `first_quote_or_backslash(` |
|---------|----------------------------------|
| `json` | 1 |
| `google_sheets` | 1 |
| `bbnf`, `bnf`, `css_l4`, `css_pretty`, `csv`, `ebnf`, `math` | 0 |

Seven grammars receive a full NEON + AVX2 + scalar triple for a function they never invoke.
The `#[allow(dead_code)]` at file header suppresses the dead-code warning, hiding the
structural omission. The emitter should gate `first_quote_or_backslash` on the presence of a
string-literal scanner in the grammar's IR.

---

## Superlinear Growth Analysis

Growth since the 85k trough (`cb3a40d55`) to current (`f4b6f757c`): **+26.9%** in generated
lines while grammar source held constant at 1,329 lines. This is not superlinear in the
complexity-theory sense — the grammar did not grow — but it is cost-without-input-change,
which is the operative concern. The primary drivers are all additive O(rules) features that
materialise their full cost in static literals rather than data structures:

1. PATH_PLAN as struct-literal array (O(rules × 6 lines))
2. Per-rule LazyLock StructLayout statics (O(rules-with-projection × ~15 lines))
3. AVX2 support module stamped once per grammar regardless of scanner type (~160 lines waste)

The `bbnf.rs` file is also over budget at 21,557 lines. Its 120-level deep nesting is the
primary driver; the grammar itself is 133 source lines (162× expansion). The four
`'avx2_scan` blocks add ~240 lines but the structural nesting is the dominant factor.

---

## Registry JSON Sidecars

The `css_l4.registry.json` sidecar at `crates/core/src/grammar/generated/css_l4.registry.json`
is 8,742 lines, the second largest generated artifact. This is consumed by the `bbnf-path`
proc-macro at expansion time. It is not compiled into the binary but it is checked into the
repo and must be re-checked after every regen. It is not subject to the same compiler budget
pressure as the `.rs` files but contributes to repo diff noise and regen latency.

---

## Proposed Per-Grammar Line Budgets

Budgets are set at 1.10× current to give headroom for grammar growth while flagging
architectural regressions. The `css_l4` ceiling is capped at 90k to force the PATH_PLAN
and LazyLock inflation to be addressed before the next wave.

| Grammar | Current | Proposed ceiling | Enforcement |
|---------|---------|-----------------|-------------|
| `css_l4` | 108,406 | 90,000 | xtask regen --check |
| `bbnf` | 21,557 | 18,000 | xtask regen --check |
| `google_sheets` | 13,861 | 15,000 | xtask regen --check |
| `ebnf` | 7,659 | 8,500 | xtask regen --check |
| `css_pretty` | 9,052 | 10,000 | xtask regen --check |
| `bnf` | 3,304 | 3,700 | xtask regen --check |
| `json` | 3,505 | 3,900 | xtask regen --check |
| `csv` | 1,702 | 1,900 | xtask regen --check |
| `math` | 875 | 1,000 | xtask regen --check |

---

## Recommendations

**AVX2 emitter.** The `emit_support_module` function (`support.rs:426`) must gate AVX2 and
x86_64 bodies behind `if cfg!(target_arch = "x86_64")` at the **generator level**, not inside
the emitted `#[cfg(...)]` guards. On an aarch64-only project, the generator has no reason to
materialise dead arch code. The idiomatic fix: add an `arch` enum to `GrammarIR` indicating
the compile target, and emit only the NEON + scalar bodies when `arch == Aarch64`. This
removes ~63 dead lines per grammar (9 grammars = ~567 lines total).

**Dead `first_quote_or_backslash`.** Gate the emission of `first_quote_or_backslash` on
whether the grammar's IR contains a string-escape scanner node. Seven of nine current grammars
receive this dead triple; the check is a single IR predicate. Estimated savings: 7 × 80 lines
= ~560 lines.

**PATH_PLAN format.** Replace the struct-literal static array with a compact wire encoding: a
flat `&[u32]` where each entry packs `rule_id` (16 bits), `segment_kind` (4 bits),
`decision_tag` (4 bits), and `decision_payload` (8 bits) into one `u32`. The `lookup()` fn
decodes on read. For CSS L4: 1,054 entries × 1 line each = 1,054 lines instead of 6,324 —
a reduction of ~5,270 lines. `PathPlanEntry` and `const fn decode()` live in
`crates/core/src/backend/rust/emitter/path_plan.rs`, which already owns the
`emit_path_plan_module` function.

**LazyLock StructLayout statics.** The per-rule lazy statics are architecturally correct
(avoid duplicate allocation) but verbosely emitted. Each should be collapsed to a single-line
`LazyLock::new(|| registry.layout_for(RULE_ID))` where `registry` is the grammar-wide
`REGISTRY` lazy that already exists at `css_l4.rs:7575`. The per-rule construction data moves
into the grammar registry JSON, not into emitted Rust literals, cutting the 208 `LazyLock`
expansions from ~15 lines each to 1 line each — ~2,900 lines saved.

**Budget enforcement.** The `generated_loc_budget` field in `skinny/Cargo.toml:67` is read
nowhere in Rust code. The budgets proposed above should be enforced in `cargo xtask regen
--check` by reading the field from workspace metadata and failing if the emitted file exceeds
the ceiling. The full-tree grammars in `crates/core/src/grammar/generated/` have no budget
field at all; add `generated_loc_budget` entries to the workspace `[metadata.bbnf.grammars]`
table and enforce them in `xtask/src/regen_simple_runtime.rs`.
