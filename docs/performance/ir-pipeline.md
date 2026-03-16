---
title: IR Pipeline
order: 7
section: BBNF
---

# IR Pipeline

Both AOT and VM paths share the same IR (`bbnf-ir`'s `GrammarIR`), with eleven optimization passes transforming it before codegen or bytecode compilation.

## Optimization Passes

Applied in order, each pass is idempotent:

```flow-chart
{ "title": "IR Optimization Passes",
  "nodes": [
    {"label": "canonicalize_aliases", "detail": "Resolve A = B chains transitively", "color": "green", "href": "#canonicalize_aliases"},
    {"label": "prune_unreachable", "detail": "Remove rules unreachable from entry", "color": "green", "href": "#prune_unreachable"},
    {"label": "eliminate_epsilon", "detail": "Remove redundant ε productions", "color": "green", "href": "#eliminate_epsilon"},
    {"label": "inline_acyclic", "detail": "Inline non-recursive rules at call sites", "color": "blue", "href": "#inline_acyclic"},
    {"label": "merge_literals", "detail": "Coalesce adjacent string literals", "color": "blue", "href": "#merge_literals"},
    {"label": "factor_common_prefixes", "detail": "Factor shared prefixes", "color": "blue", "href": "#factor_common_prefixes"},
    {"label": "refine_span_eligibility", "detail": "Mark rules for zero-copy span parsing", "color": "purple", "href": "#refine_span_eligibility"},
    {"label": "compute_follow_sets", "detail": "Compute FOLLOW sets for conflict detection", "color": "purple", "href": "#compute_follow_sets"},
    {"label": "generate_dispatch_tables", "detail": "Build O(1) FIRST-set dispatch tables", "color": "purple", "href": "#generate_dispatch_tables"},
    {"label": "refine_memo_strategies", "detail": "Select memoization strategy per rule", "color": "purple", "href": "#refine_memo_strategies"},
    {"label": "infer_types", "detail": "Propagate type descriptors for codegen", "color": "purple", "href": "#infer_types"}
  ] }
```

Pass categories: **Cleanup** (green) removes dead weight, **Optimization** (blue) restructures for speed, **Analysis** (purple) computes metadata for codegen.

The pass pipeline typically converges in 2–3 iterations of the fixed-point loop for `sp_method_rules` (span-eligible rules).

### Cleanup

## canonicalize_aliases

Resolves transitive alias chains (`A = B`, `B = C`) to their terminal non-alias target, then rewrites all `Ref(alias_id)` nodes throughout the IR to point directly at the canonical rule. Fires whenever the lowering step marks a rule with `is_alias` — typically single-production rules whose body is a bare nonterminal reference. Eliminates indirect call overhead in both the interpreter and AOT codegen by removing one level of indirection per alias hop, and exposes the true call target to downstream passes like `inline_acyclic` and `generate_dispatch_tables`.

## prune_unreachable

Performs a DFS from the grammar's entry rule (including recovery expressions) to collect reachable rule IDs, then removes all unreachable rules and compacts the RuleId space with a contiguous remapping. Fires when imported grammars or left-recursion elimination introduce rules that no live path references. Reduces IR size, shrinks bytecode programs, and prevents unreachable rules from polluting dispatch tables or inflating the string table.

## eliminate_epsilon

Strips `Epsilon` nodes from `Seq` children, unwraps singleton `Seq` and `Alt` wrappers, and collapses all-epsilon sequences to a single `Epsilon`. Fires after `inline_acyclic` (which can introduce epsilon remnants) and after left-recursion elimination (which inserts explicit epsilon alternatives). Reduces IR node count, which lowers bytecode instruction count and simplifies pattern matching in subsequent passes like `merge_literals` and `factor_common_prefixes`.

### Optimization

## inline_acyclic

Replaces `Ref(id)` nodes with the referenced rule's body when the target is non-cyclic (no SCC membership), is not the entry point, and has at most 3 IR nodes. Fires on small leaf-like rules such as `comma = "," ?w` or `colon = ":" ?w` that appear frequently as helpers. Eliminates `Call`/`Return` overhead in the interpreter, and by splicing the body inline, exposes adjacent literals to `merge_literals` and shared prefixes to `factor_common_prefixes`.

## merge_literals

Scans `Seq` nodes for runs of adjacent `Literal` children and coalesces them into a single `Literal` with the concatenated string (e.g., `Seq([Lit("a"), Lit("b")])` becomes `Lit("ab")`). Fires after `inline_acyclic`, which frequently places formerly-separate literals next to each other. Reduces the number of `MatchString` opcodes in bytecode, turning N sequential string matches into one, and shrinks the IR tree for faster traversal by later passes.

## factor_common_prefixes

Groups consecutive alternation branches that share the same leading IR node and rewrites them as `Seq(prefix, Alt(remainders))`. Uses sequential grouping (not arbitrary reordering) to preserve alternation priority semantics. Fires on patterns like `"if" ident | "if" "(" expr ")"`, keyword tables, or any grammar where branches share a literal or nonterminal prefix. Eliminates redundant backtracking by parsing the shared prefix once, and can enable `generate_dispatch_tables` on the resulting inner alternation by making its branches' FIRST sets disjoint.

### Analysis

## refine_span_eligibility

Iterates to a fixed point over all non-cyclic rules, marking each as span-eligible if its entire body — leaves, combinators, and all transitively referenced rules — can produce a `Span<'a>` without semantic transformations (`Map`, boxing, enum wrapping). Cyclic rules are excluded because SpanParser has no recursive variant. Enables `_sp()` method generation in AOT codegen (zero-copy, vtable-free parsing) and allows `infer_types` to assign `Span` instead of `BoxedEnum` for references to span-eligible rules inside concatenations.

## compute_follow_sets

Computes `FOLLOW(A)` — the set of ASCII characters that can appear immediately after rule A in any sentential form — using the standard textbook algorithm with fixed-point iteration for cyclic grammars. Propagates through `Seq` (FIRST of suffix), `Alt` (union), `Repeat` (self-loop), `Skip`/`Next`/`Minus` (binary), and handles nullable suffixes by adding `FOLLOW(container)`. Consumed by two downstream passes: `generate_dispatch_tables` uses FOLLOW sets to assign nullable alternation branches to dispatch entries, and `refine_memo_strategies` uses FOLLOW set cardinality as a signal for memoization benefit.

## generate_dispatch_tables

Walks the entire IR tree and annotates each `Alt` node whose branches have pairwise disjoint FIRST sets with an `AltDispatch` — a 128-byte lookup table mapping each ASCII byte to a branch index (or 255 for no match). When FOLLOW sets are available, a single nullable branch can participate in dispatch by using `FOLLOW(containing_rule)` as its effective dispatch set, provided it is disjoint from all other branches' FIRST sets. Converts O(n) linear branch trial in the interpreter to O(1) table lookup, which is the single largest performance improvement for grammars with many-branch alternations (e.g., JSON `value` with 6+ branches).

## refine_memo_strategies

Assigns one of three memoization strategies per rule: `Full` for SCC entry points (required for termination of cyclic parsing), `None` for non-entry cyclic rules (subsumed by the entry point's cache), and `Selective` for non-cyclic rules whose cross-rule reference count exceeds a threshold. The threshold is modulated by FOLLOW set cardinality: rules with large FOLLOW sets (>= 8 characters, indicating many calling contexts) have the threshold lowered by 1, while rules with small FOLLOW sets (< 4 characters) have it raised by 1. Avoids unconditional memoization overhead on rarely-referenced rules while ensuring hot rules and recursive entry points cache their results.

## infer_types

Walks each rule body in topological order and assigns a `TypeDesc` (`Span`, `Vec<T>`, `Option<T>`, `Tuple(...)`, `BoxedEnum`, `Enum`, `Named`) describing the Rust/TS output type. Handles five special cases: sp_method Span overrides in Seq nodes (B.1), `@pretty`/`@no_collapse` tuple preservation (B.2), custom mapping return types (B.3), cyclic-context BoxedEnum overrides for acyclic references (B.4), and sub-variant collection for heterogeneous alternations with cross-rule uniqueness validation (B.5). Populates `GrammarIR::types` and `RuleMeta::sub_variants`, which are consumed by all codegen backends (Rust TokenStream, bytecode compiler, TS interpreter) to emit correctly typed parsers and enum definitions.

## AOT vs VM Compile Time

The paths diverge after pass 11:

```bench-chart
{ "title": "AOT vs VM Compile Time (8-rule JSON)", "unit": "ms",
  "labels": ["IR → TokenStream (AOT)", "rustc compile (AOT)", "IR → Bytecode (VM)", "MessagePack serialize (VM)"],
  "series": [{"label": "Time", "values": [1, 2000, 0.5, 0.1]}] }
```

AOT's compile time is dominated by `rustc`. IR → TokenStream takes ~1 ms even for large grammars. The VM path avoids `rustc` entirely, making it suitable for the WASM playground where users edit grammars interactively.

## Bytecode

The VM compiles each IR rule into a sequence of opcodes:

```
 MatchString | MatchRegex | Epsilon | Jump | Call |
 SaveState | Dispatch | RepeatBegin | ...
```

Bytecode programs are serialized via MessagePack for crossing the WASM boundary. A typical JSON grammar compiles to ~2 KB of bytecode.

## IR vs Direct AST

The previous codegen architecture walked the AST directly. The IR-based architecture adds a lowering step but enables cross-rule optimizations that weren't possible before—`inline_acyclic` and `factor_common_prefixes` can't operate on a raw AST because they need whole-grammar visibility.
