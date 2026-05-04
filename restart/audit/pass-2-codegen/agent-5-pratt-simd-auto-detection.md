# PASS-2 Agent 5: Pratt + SIMD Auto-Detection

## §1 Scope + Framing

Lens: specify auto-detection for Pratt and SIMD emission. Lock 10 says Pratt, SIMD, and PHF recognizers are auto-detected from grammar shape, not user directives (`restart/locks/14-LOCKS.md:52`). README repeats that Pratt, SIMD, and PHF are auto-detected (`restart/README.md:180-182`). PASS-2 prompt assigns Pratt plus SIMD auto-detection to this pass (`restart/prompts/PASS-2-CODEGEN.md:3`).

The current backend already carries Pratt and SIMD residues. Rust grammar emission writes Pratt precedence LUT constants (`crates/core/src/backend/rust/emitter/grammar.rs:194-202`), and generated strategy state includes delimiter scan configs, key dispatch, regex patterns, and materialization maps (`crates/core/src/backend/driver/mod.rs:52-121`). PASS-2 must move those decisions upstream into PASS-1 analysis plus Backend IR annotations, then lower them uniformly.

## §2 Per-Item Table

| Detector | Pro | Con | Explication | Challenge | Disposition |
|---|---|---|---|---|---|
| Pratt detection | Eliminates hand-marked expression parsers; README says auto-detect (`restart/README.md:180-182`). | Detection can misclassify recursive grammar shapes. | PASS-1 recognizes prefix/infix/postfix recursive patterns and emits `PrattSpine` with operator table. | No directive fallback in BBNF surface. | KEEP-REINVENT. |
| PHF keyword detection | Current Rust emission has PHF keyword table path (`crates/core/src/backend/rust/emitter/grammar.rs:155-163`). | PHF can be overused for tiny sets. | Cost model selects branch, match, PHF, or scanner based on set size and frequency. | Selection must be recorded in BIR snapshot. | KEEP-MODIFY. |
| SIMD structural scan | `simd-scan` already accepts data-driven alphabets (`crates/simd-scan/src/alphabet.rs:1-18`). | Overhead can exceed payoff for tiny leaves. | PASS-1 extracts delimiter/structural byte alphabet and cost model chooses `SimdScan`. | No user directive; the cost table explains decisions. | KEEP. |
| Regex Unicode | README puts Unicode coverage in regex layer (`restart/README.md:131-143`). | SIMD byte scanner must not pretend to be Unicode parser. | SIMD scans structural bytes; regex handles Unicode class algebra internally. | Clear boundary in BIR. | KEEP-MODIFY. |
| Lookbehind | Lookbehind is in grammar surface (`restart/README.md:125-129`). | Reverse scanning complicates SIMD and regex composition. | Fixed/bounded width lookbehind lowers to `Lookbehind`; optional SIMD only for simple byte windows. | Unbounded patterns rejected upstream. | REINVENT. |
| Layout detection | `@layout` is in V1 (`restart/README.md:176-178`). | Over-scanning layout can mask errors. | Layout policy is explicit in BIR and consumed before/after relevant nodes. | Error recovery sees skipped spans. | KEEP-MODIFY. |

## §3 Architectural Commitments Ratified

1. **No directives for Pratt/SIMD/PHF.** Metadata can contain grammar identity and fixtures, but not a force/skip knob for these recognizers in V1. This follows Lock 10 (`restart/locks/14-LOCKS.md:52`).

2. **Every auto-decision has an audit row.** Backend IR snapshots include `Decision { rule, detector, selected, rejected_alternatives, cost }`. Lessons learned require producer and consumer gates for contracts (`docs/precepts/instructions/LESSONS-LEARNED.md:74-80`).

3. **Pratt emits one `PrattSpine` node per expression family.** The BC variant table already includes `PrattSpine` (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:220-230`). PASS-2 keeps it and requires the Rust/WASM lowerers to consume the same table.

4. **SIMD uses `simd-scan` kernel shapes.** The alphabet code already defines `KernelShape` as Empty, NibbleLut, WideLut, and MultiCmp (`crates/simd-scan/src/alphabet.rs:98-125`). PASS-2 cost model should emit one of those categories.

5. **Structural scan is tape-aligned.** SIMD scanner returns offsets; Rust/WASM lowerers map those offsets into Tape leaf nodes and `sib_skip` links. This keeps scan output part of the one Tape substrate, not a side channel.

## §4 New Facilities Proposed

Pratt detector output:

```rust
pub struct PrattPlan {
    pub rule: RuleId,
    pub operators: Vec<Operator>,
    pub prefix: Vec<Operator>,
    pub postfix: Vec<Operator>,
    pub associativity: Vec<Assoc>,
    pub fallback: Option<NodeId>,
    pub evidence: DetectionEvidence,
}
```

SIMD detector output:

```rust
pub struct SimdPlan {
    pub alphabet: StructuralAlphabet,
    pub kernel: KernelShape,
    pub estimated_hits_per_kb: u32,
    pub scalar_cost: u64,
    pub simd_cost: u64,
    pub selected: bool,
}
```

Decision log:

| Grammar family | Expected detector result | Evidence source |
|---|---|---|
| JSON | SIMD structural scan for `{ } [ ] : , "`, no Pratt | simdjson structural index lesson (`restart/corpora/SOTA.md:73-89`) |
| CSS L4 | SIMD for delimiters, regex Unicode for identifiers, host chains for color functions | lightningcss parser/visitor lessons (`restart/corpora/SOTA.md:97-126`) |
| BBNF | Pratt for expression-like grammar parts, PHF for directives/keywords | current Pratt LUT emission path (`crates/core/src/backend/rust/emitter/grammar.rs:194-202`) |
| Sheets | Pratt for formulas, host chains for functions | `@host fn` and chaining V1 scope (`restart/README.md:145-166`) |

## §5 Cross-Cuts To PASS-1 / PASS-3

PASS-1 owns detection and cost extraction; PASS-2 owns codegen consumption. README's pass order places shape mining, e-graph, cost extraction, and Backend IR lowering before per-backend lower (`restart/README.md:188-217`).

PASS-3 receives decision logs and materialisation cost. API docs can expose why a parser uses Pratt or SIMD, but PASS-3 must not add grammar directives for these decisions because Lock 10 rejects directives (`restart/locks/14-LOCKS.md:52`).

## §6 Risk + Mitigation Table

| Risk | Impact | Mitigation |
|---|---|---|
| Pratt detector rewrites semantics | Expression grammar parses differently | Snapshot operator tables and run corpus fixtures through pre/post canonical output. |
| SIMD selected for sparse alphabet | Slower parser | Cost model compares scalar and SIMD costs and records rejected choice. |
| Unicode leaks into SIMD byte alphabet | Incorrect non-ASCII parse | Regex Unicode remains in `RegexDfa`; structural scan only handles byte delimiters. |
| Host chain detection mixes with Pratt | Function-call grammar becomes opaque | Host calls remain `HostCall` nodes invoked by Pratt parselets when needed. |
| Auto-decision logs become stale | Debugging impossible | Regen writes decision snapshots with metadata hash; `xtask regen --check` enforces equality. |

## §7 Inheritance Ledger

| Source | KEEP | REINVENT | DISCARD |
|---|---|---|---|
| Lock 10 | Auto-detect posture (`restart/locks/14-LOCKS.md:52`). | Add decision logs and BIR snapshots. | User directives for Pratt/SIMD/PHF. |
| Current Rust emitter | Existing Pratt/PHF table knowledge (`crates/core/src/backend/rust/emitter/grammar.rs:155-202`). | Move selection out of emitter into BIR annotations. | Lowerer-local strategy selection. |
| `simd-scan` | Data-driven alphabets and kernel shapes (`crates/simd-scan/src/alphabet.rs:1-18`, `crates/simd-scan/src/alphabet.rs:98-125`). | Couple scan results to Tape node offsets. | Grammar-specific scanner variants. |
| SOTA | simdjson and lightningcss performance anchors (`restart/corpora/SOTA.md:12-16`, `restart/corpora/SOTA.md:130-136`). | Use anchors as gates for BIR cost trajectory. | Blind benchmark claims without corpus rows. |
