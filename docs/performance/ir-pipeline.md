---
title: IR Pipeline
order: 7
section: BBNF
---

# IR Pipeline

Both AOT and VM paths share the same IR (`bbnf-ir`'s `GrammarIR`), with fifteen optimization passes (thirteen unique) transforming it before codegen or bytecode compilation.

## Optimization Passes

Applied in order, each pass is idempotent:

```flow-chart
{ "title": "IR Optimization Passes",
  "nodes": [
    {"label": "canonicalize_aliases", "detail": "Resolves transitive alias chains and rewrites all Ref nodes to point at the canonical rule, eliminating indirection.", "color": "green", "href": "#canonicalize_aliases"},
    {"label": "prune_unreachable", "detail": "DFS from the entry rule to collect reachable IDs, then removes unreachable rules and compacts the RuleId space.", "color": "green", "href": "#prune_unreachable"},
    {"label": "inline_acyclic", "detail": "Replaces Ref nodes with the referenced rule's body when the target is non-cyclic and small (at most 3 IR nodes).", "color": "blue", "href": "#inline_acyclic"},
    {"label": "prune_unreachable", "detail": "Removes rules made dead by inlining.", "color": "green", "href": "#prune_unreachable"},
    {"label": "fuse_single_use", "detail": "Inlines rules referenced exactly once regardless of body size, guarded by SCC membership.", "color": "blue", "href": "#fuse_single_use"},
    {"label": "prune_unreachable", "detail": "Removes rules made dead by fusing.", "color": "green", "href": "#prune_unreachable"},
    {"label": "eliminate_epsilon", "detail": "Strips Epsilon nodes from Seq children, unwraps singleton wrappers, and collapses all-epsilon sequences.", "color": "green", "href": "#eliminate_epsilon"},
    {"label": "merge_literals", "detail": "Coalesces runs of adjacent Literal children in Seq nodes into a single concatenated Literal.", "color": "blue", "href": "#merge_literals"},
    {"label": "merge_regex_alts", "detail": "Fuses Alt branches that are all Regex leaves into one combined Regex pattern.", "color": "blue", "href": "#merge_regex_alts"},
    {"label": "factor_common_prefixes", "detail": "Groups alternation branches sharing a leading IR node and rewrites them as Seq(prefix, Alt(remainders)).", "color": "blue", "href": "#factor_common_prefixes"},
    {"label": "refine_span_eligibility", "detail": "Fixed-point iteration marking non-cyclic rules as span-eligible when their entire body can produce a Span without semantic transforms.", "color": "purple", "href": "#refine_span_eligibility"},
    {"label": "compute_follow_sets", "detail": "Computes FOLLOW(A) for every rule using fixed-point iteration, propagating through Seq, Alt, Repeat, and binary operators.", "color": "purple", "href": "#compute_follow_sets"},
    {"label": "generate_dispatch_tables", "detail": "Annotates Alt nodes with disjoint FIRST sets with a 128-byte O(1) lookup table mapping each ASCII byte to a branch index.", "color": "purple", "href": "#generate_dispatch_tables"},
    {"label": "project_types", "detail": "Walks rule bodies in topological order assigning TypeDesc values consumed by all codegen backends for typed parser emission.", "color": "purple", "href": "#project_types"}
  ] }
```

Pass categories: **Cleanup** (green) removes dead weight, **Optimization** (blue) restructures for speed, **Analysis** (purple) computes metadata for codegen.

The pass pipeline typically converges in 2–3 iterations of the fixed-point loop for `sp_method_rules` (span-eligible rules).

### Cleanup

## `canonicalize_aliases`

Resolves transitive alias chains (`A = B`, `B = C`) to their terminal non-alias target, then rewrites all `Ref(alias_id)` nodes throughout the IR to point directly at the canonical rule.

Fires whenever the lowering step marks a rule with `is_alias`—typically single-production rules whose body is a bare nonterminal reference. Eliminates indirect call overhead in both the interpreter and AOT codegen by removing one level of indirection per alias hop, and exposes the true call target to downstream passes like `inline_acyclic` and `generate_dispatch_tables`.

## `prune_unreachable`

Performs a DFS from the grammar's entry rule (including recovery expressions) to collect reachable rule IDs, then removes all unreachable rules and compacts the RuleId space with a contiguous remapping.

Fires when imported grammars or left-recursion elimination introduce rules that no live path references. Reduces IR size, shrinks bytecode programs, and prevents unreachable rules from polluting dispatch tables or inflating the string table.

## `eliminate_epsilon`

Strips `Epsilon` nodes from `Seq` children, unwraps singleton `Seq` and `Alt` wrappers, and collapses all-epsilon sequences to a single `Epsilon`.

Fires after `inline_acyclic` (which can introduce epsilon remnants) and after left-recursion elimination (which inserts explicit epsilon alternatives). Reduces IR node count, which lowers bytecode instruction count and simplifies pattern matching in subsequent passes like `merge_literals` and `factor_common_prefixes`.

### Optimization

## `inline_acyclic`

Replaces `Ref(id)` nodes with the referenced rule's body when the target is non-cyclic (no SCC membership), is not the entry point, and has at most 3 IR nodes. Fires on small leaf-like rules such as `comma = "," ?w` or `colon = ":" ?w` that appear frequently as helpers.

Eliminates `Call`/`Return` overhead in the interpreter, and by splicing the body inline, exposes adjacent literals to `merge_literals` and shared prefixes to `factor_common_prefixes`.

## `fuse_single_use`

Inlines rules referenced exactly once at their call site, regardless of body size. Unlike `inline_acyclic` (which is size-gated at 3 nodes), this pass targets rules that appear in only one location—since there's no fan-out, inlining always reduces total work.

Guarded by SCC membership: rules in strongly connected components are skipped to avoid infinite expansion. Runs after `inline_acyclic` + prune to pick up remaining single-use rules that were too large for the size threshold.

Rules marked with `@token` are also fused at this stage: the body is inlined at every call site (fusion-style), but the rule's enum variant is preserved so `@pretty` consumers can dispatch on it. `@token` implies span-eligible, so these rules participate in span-only codegen without additional annotation.

Exposes additional optimization opportunities for `merge_literals`, `factor_common_prefixes`, and `generate_dispatch_tables` by eliminating call boundaries that previously hid adjacent patterns.

## `merge_literals`

Scans `Seq` nodes for runs of adjacent `Literal` children and coalesces them into a single `Literal` with the concatenated string (e.g., `Seq([Lit("a"), Lit("b")])` becomes `Lit("ab")`).

Fires after `inline_acyclic`, which frequently places formerly-separate literals next to each other. Reduces the number of `MatchString` opcodes in bytecode, turning N sequential string matches into one, and shrinks the IR tree for faster traversal by later passes.

## `merge_regex_alts`

Scans `Alt` nodes for branches that are all `Regex` leaves and fuses them into a single `Regex` with a combined pattern (e.g., `Alt([Regex("a+"), Regex("b+")])` becomes `Regex("(?:a+)|(?:b+)")`).

Fires after `merge_literals` and `inline_acyclic`, which can surface regex-only alternations that weren't visible at the AST level. Reduces alternation trial overhead in both the interpreter and AOT codegen by replacing N regex matches with one, and can enable `generate_dispatch_tables` on the containing alternation by collapsing branches.

## `factor_common_prefixes`

Groups consecutive alternation branches that share the same leading IR node and rewrites them as `Seq(prefix, Alt(remainders))`. Uses sequential grouping (not arbitrary reordering) to preserve alternation priority semantics.

Fires on patterns like `"if" ident | "if" "(" expr ")"`, keyword tables, or any grammar where branches share a literal or nonterminal prefix. Eliminates redundant backtracking by parsing the shared prefix once, and can enable `generate_dispatch_tables` on the resulting inner alternation by making its branches' FIRST sets disjoint.

### Analysis

## `refine_span_eligibility`

Iterates to a fixed point over all non-cyclic rules, marking each as span-eligible if its entire body—leaves, combinators, and all transitively referenced rules—can produce a `Span<'a>` without semantic transformations (`Map`, boxing, enum wrapping). Cyclic rules are excluded because SpanParser has no recursive variant.

Enables `_sp()` method generation in AOT codegen (zero-copy, vtable-free parsing) and allows `project_types` to assign `Span` instead of `BoxedEnum` for references to span-eligible rules inside concatenations.

## `compute_follow_sets`

Computes `FOLLOW(A)`—the set of ASCII characters that can appear immediately after rule A in any sentential form—using the standard textbook algorithm with fixed-point iteration for cyclic grammars. Propagates through `Seq` (FIRST of suffix), `Alt` (union), `Repeat` (self-loop), `Skip`/`Next`/`Minus` (binary), and handles nullable suffixes by adding `FOLLOW(container)`.

Consumed by `generate_dispatch_tables`, which uses FOLLOW sets to assign nullable alternation branches to dispatch entries.

## `generate_dispatch_tables`

Walks the entire IR tree and annotates each `Alt` node whose branches have pairwise disjoint FIRST sets with an `AltDispatch`—a 128-byte lookup table mapping each ASCII byte to a branch index (or 255 for no match). When FOLLOW sets are available, a single nullable branch can participate in dispatch by using `FOLLOW(containing_rule)` as its effective dispatch set, provided it is disjoint from all other branches' FIRST sets.

Converts O(n) linear branch trial in the interpreter to O(1) table lookup, which is the single largest performance improvement for grammars with many-branch alternations (e.g., JSON `value` with 6+ branches).

## `project_types`

Walks each rule body in topological order and assigns a `TypeDesc` (`Span`, `Vec<T>`, `Option<T>`, `Tuple(...)`, `BoxedEnum`, `Enum`, `Named`) describing the Rust/TS output type. Handles five special cases: sp_method Span overrides in Seq nodes (B.1), `@pretty`/`@no_collapse` tuple preservation (B.2), custom mapping return types (B.3), cyclic-context BoxedEnum overrides for acyclic references (B.4), and sub-variant collection for heterogeneous alternations with cross-rule uniqueness validation (B.5).

Populates `GrammarIR::types` and `RuleMeta::sub_variants`, which are consumed by all codegen backends (Rust TokenStream, bytecode compiler, TS interpreter) to emit correctly typed parsers and enum definitions.

## Dispatch Tables in Practice

When alternation branches have disjoint FIRST sets, the codegen emits an O(1) character-dispatch lookup:

```code-tabs
---rust---
// Generated by #[derive(Parser)]
fn value(&self, state: &mut ParserState<'a>) -> Parser<'a, Value<'a>> {
    dispatch! {
        b'"' => self.string(state),
        b'0'..=b'9' | b'-' => self.number(state),
        b'{' => self.object(state),
        b'[' => self.array(state),
        b't' => self.parse_true(state),
        b'f' => self.parse_false(state),
        b'n' => self.parse_null(state),
    }
}
---typescript---
// Generated by ASTToParser()
const value = dispatch({
    '"': jsonString,
    "0-9": jsonNumber,
    "-": jsonNumber,
    "{": jsonObject,
    "[": jsonArray,
    "t": string("true").map(() => true),
    "f": string("false").map(() => false),
    "n": string("null").map(() => null),
});
```

The leading byte selects the parser in constant time, eliminating sequential trial across branches.

## FIRST Sets

Every rule's FIRST set is a 128-bit `CharSet` covering ASCII, computed iteratively to fixed point over cyclic rules:

```code-tabs
---rust---
use bbnf::analysis::CharSet;

// FIRST set computation — iterates to fixed point
let first_sets = grammar.compute_first_sets();
// first_sets["value"] = CharSet { '"', '0'..='9', '-', '{', '[', 't', 'f', 'n' }

// Use in dispatch table generation
let table = grammar.generate_dispatch_table(&first_sets);
---typescript---
import { computeFirstSets } from "@mkbabb/bbnf-lang";

// FIRST set computation — iterates to fixed point
const firstSets = computeFirstSets(grammar);
// firstSets.get("value") = Set { '"', '0'-'9', '-', '{', '[', 't', 'f', 'n' }

// Use in dispatch table generation
const table = generateDispatchTable(firstSets);
```

```
value: {", 0-9, -, {, [, t, f, n}
object: {{}
array: {[}
string: {"}
number: {0-9, -}
```

When FIRST sets overlap between alternation branches, the codegen falls back to sequential `any()` and the LSP emits an ambiguity warning.

## AOT vs VM Compile Time

The paths diverge after pass 14:

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

## Codegen Optimizations

### Phase 1: Inline direct-dispatch codegen

`ir_codegen/inline.rs` generates flat match-arm dispatch instead of combinator chains. `InlineCtx` manages scope, and `emit_rule_body_inline` handles Seq/Alt/Repeat/Ref nodes. Measured 0% gain on JSON (grammar too flat), but provides the architectural foundation for deeper inlining in subsequent phases.

### Phase 2a: Vec unboxing

The `in_vec` context parameter is threaded through codegen to emit `Vec<Enum>` instead of `Vec<Box<Enum>>`. `project_node_in_vec` is a sub-pass in `project.rs` that determines when unboxing is safe. Transparent rules generate an `_unboxed()` method for zero-cost enum extraction. Results on JSON benchmarks:

| Dataset | Before (MB/s) | After (MB/s) | Gain |
|---------|---------------|--------------|------|
| data.json | 792 | 1,543 | +95% |
| apache | 825 | 1,638 | +99% |
| citm_catalog | 693 | 1,520 | +119% |
| canada | 297 | 1,260 | +324% |
| twitter | 851 | 1,599 | +88% |
| data_xl | 604 | 1,052 | +74% |

## IR vs Direct AST

The previous codegen architecture walked the AST directly. The IR-based architecture adds a lowering step but enables cross-rule optimizations that weren't possible before—`inline_acyclic` and `factor_common_prefixes` can't operate on a raw AST because they need whole-grammar visibility.
