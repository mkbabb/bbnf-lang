# AW-IV B4 — Per-Shape Codegen Generalisation Strategy

## Executive summary (≤150 words)

The JSON prototype's sonic-rs-class throughput is unlocked by per-shape
inline loops with fused monomorphic visitor calls. Generalising to all
four grammars requires one new IR mining pass
(`shape_mining.rs`) that classifies each rule as one of seven shapes —
Object, Array, String, Number, Keyword, Pratt, Unordered — plus a
per-shape emitter module (`emitter/shapes/{object,array,…}.rs`) that
emits a specialised inline function per (grammar, rule) pair. Every
detector grounds in an IR-structural predicate over existing miner
outputs (`delim_scan_configs`, `shape_dict_templates`,
`operator_chain_entries`, `disjoint_first_tables`, `keyword_branches`);
no grammar-name branches. Rules that match no shape fall back to the
existing `__dta_walker_inline::run`, preserved as the substrate AX
replays against. The invariant: per-grammar MECHANISM is the eight
detectors + eight emitters; per-grammar OUTPUT differs because each
grammar's IR admits a different subset. Target coverage: ≥80% of
hot-path visits across JSON/CSS/Sheets/BBNF.

## 1. Shape taxonomy — predicates grounded in actual BBNF rules

Seven shape categories are derivable from existing IR miner outputs.
Each has an IR predicate and cites at least one grammar-file rule.

### 1.1 Object-shape

**Predicate**: a rule whose body matches `Wrap(open_lit, Repeat(Seq(
key-rule, sep_lit, value-rule) << sep_lit?), close_lit)` where `open_lit`
and `close_lit` are single-byte literals and `key-rule` has a mined
`RecognizerShape::QuotedString` or a Regex pattern that admits a
prefix-disjoint key class.

**Evidence**:
- `grammar/json/json.bbnf:14` — `object = "{" >> (( pair << comma ? ) *)?w << "}"` with `pair = string, colon >> value` at line 13.
- `grammar/css/l4/stylesheet.bbnf:15-17` — `ruleBlock = "{" >> blockContent ?w << "}"` where `blockContent` is declarations/ruleItems separated by `;`.

**Existing substrate**: `DelimScanMiner` at
`crates/ir/src/passes/recognizers/delim_scan.rs:30-47` already detects
`Wrap(open, Repeat(Alt), close)` and writes a
`DelimScanConfig`. Object-shape extends this with the additional
discrimination that the Repeat body is a two-field Seq (key, value)
rather than a general Alt.

**Detector home**: `crates/ir/src/passes/recognizers/object_shape.rs` (new).

### 1.2 Array-shape

**Predicate**: `Wrap(open_lit, Repeat(value-rule << sep_lit?), close_lit)`
— same as object but without the paired (key, value) Seq body.

**Evidence**:
- `grammar/json/json.bbnf:11` — `array = "[" >> (( value << comma ? ) *)?w << "]"`.
- `grammar/google-sheets/google-sheets.bbnf:158` — `array_literal = "{" , (array_rows) ?w , "}"` (sheet's double-array).

**Existing substrate**: `DelimScanMiner` + `SeparatorListMiner` at
`crates/ir/src/passes/recognizers/separator_list.rs:14-65`.

**Detector home**: `crates/ir/src/passes/recognizers/array_shape.rs`.

### 1.3 String-shape

**Predicate**: a rule body that is a `Regex` classified as
`RegexClass::QuotedString` by the existing `QuotedStringMiner`. The
emitter splices the NEON `scan_quoted_string_simd` kernel as the
parse body.

**Evidence**:
- `grammar/json/json.bbnf:9` — `string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ -> decode_json_string_to_arena(input) : String`.
- `grammar/google-sheets/google-sheets.bbnf:12` — `string = /"([^"]|"")*"/ -> input : Span`.

**Existing substrate**: `QuotedStringMiner` already writes a
`RecognizerShape::QuotedString` record. The `bbnf-simd-scan` crate
already exposes `scan_quoted_string_simd`; the P5 profile (§4)
confirms it is *absent* from the hot path today because the walker's
Regex arm inlines the DFA body rather than the SIMD kernel.

**Detector home**: `crates/ir/src/passes/recognizers/string_shape.rs`.

### 1.4 Number-shape

**Predicate**: a rule body that is a `Regex` whose pattern matches the
canonical numeric regex class — a fact already mined by
`PatternAlphabetMiner` (a pattern whose alphabet is
`[0-9] ∪ {+, -, ., e, E}` is number-shape-eligible).

**Evidence**:
- `grammar/json/json.bbnf:4` — `number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ -> f64`.
- `grammar/google-sheets/google-sheets.bbnf:6` — `number = /(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?/ -> f64`.
- `grammar/css/l4/value-unit.bbnf` — `number` primitive (imported).

**Existing substrate**: `PatternAlphabetMiner`; the W2.3 plan already
specifies Eisel-Lemire inline emission for `Map { Regex, F64 }`. The
number-shape detector is the predicate that admits the emission.

**Detector home**: `crates/ir/src/passes/recognizers/number_shape.rs`.

### 1.5 Keyword-shape

**Predicate**: an `Alt` whose every branch is either a `Literal` of
length ≤ 16 or `Map { inner: Literal, .. }`. This is the existing
`KeywordStatsMiner` output modulo the `branch_count ≥ PHF_MIN_BRANCHES`
threshold (W3.2 lowers that to 3).

**Evidence**:
- `grammar/json/json.bbnf:1-2` — `null = "null" -> 0u8` and
`bool = "true" -> true | "false" -> false` — every branch is a literal.
- `grammar/google-sheets/google-sheets.bbnf:34-42` — `error_literal`
with 9 literal branches (`#N/A`, `#VALUE!`, …).
- `grammar/css/l4/selectors.bbnf:59-62` — `nthFunctionName` with 4
literal branches (`nth-last-of-type`, …).
- `grammar/bbnf/bbnf.bbnf:75-81` — `directive` with 7 literal-led
branches (`@import`, `@recover`, …).

**Existing substrate**: `KeywordStatsMiner` at
`crates/ir/src/passes/recognizers/keyword_stats.rs`. W3.2 wires the
AltLinear consumer.

**Detector home**: keyword-shape detection lives in the existing
`KeywordStatsMiner`; the per-shape emitter at
`emitter/shapes/keyword.rs` consumes it.

### 1.6 Pratt-shape

**Predicate**: a rule admitted to `DtaTable::shunting_yard_chains` by
`collect_precedence_chain`; i.e. the existing
`operator_chain::collect_operator_chains` yields a non-empty
`OperatorChainFacts` rooted at this rule.

**Evidence**:
- `grammar/google-sheets/google-sheets.bbnf:94-118` — six-rung tower:
`comparison_expr → concat_expr → add_expr → mul_expr → exp_expr →
unary_expr → postfix_expr`.
- `grammar/css/l4/values.bbnf:49-50` — `mathProduct = mathValue , (
("*" | "/") >> mathValue ) *` and
`mathExpr = mathProduct , ( ("+" | "-") >> mathProduct ) *`.

**Existing substrate**: `collect_operator_chains` at
`crates/ir/src/passes/recognizers/operator_chain.rs:146-192` with its
`OperatorChainFacts` output.

**Detector home**: pratt-shape detection is the existing
`collect_operator_chains`; the per-shape emitter at
`emitter/shapes/pratt.rs` consumes it and uses `PRECEDENCE_LUT` (W3.4).

### 1.7 Unordered-shape (CSS compound selector family)

**Predicate**: a `Repeat` over an `Alt` where every Alt branch has a
disjoint FIRST set and the whole construction is a `Repeat { lo: 1, hi:
∞ }` (a Kleene-plus over a disjoint-FIRST Alt).

**Evidence**:
- `grammar/css/l4/selectors.bbnf:87-88` — `compoundSelector =
(classSelector | idSelector | attrSelector | colonSelector |
typeSelector) +` — five branches, each with disjoint FIRST (`.`, `#`,
`[`, `:`, `[a-z]`).

**Existing substrate**: `DisjointFirstMiner` at
`crates/ir/src/passes/recognizers/disjoint_first.rs`. The unordered-
shape detector composes the mining's `DisjointFirstTable` with a
surrounding `Repeat { lo: 1, .. }`.

**Detector home**:
`crates/ir/src/passes/recognizers/unordered_shape.rs`.

### 1.8 Scalar-shape

**Predicate**: a trivial rule whose body is a single `Literal` or
`Map { Literal, .. }`. Degenerate case of keyword-shape with one
branch.

**Evidence**:
- `grammar/json/json.bbnf:1` — `null = "null" -> 0u8`.
- `grammar/google-sheets/google-sheets.bbnf:104` — `mathOperator = "+"
-> 0u8 | ...`.

**Existing substrate**: trivial projection from `keyword_branches`
with `len == 1`.

**Detector home**: scalar-shape is folded into the keyword-shape
emitter via a single-entry special case.

## 2. Per-shape codegen dispatch — no monolithic match

The emitter produces one specialised function per (grammar, rule,
shape) triple:

```
fn parse_object_<grammar>_<rule_name><V: ObjectVisitor>(
    input: &[u8], pos: &mut u32, idx: &StructuralIndex, visitor: &mut V,
) -> Result<(), DtaError>;
```

Dispatch happens at the **call site**, not via state-ID indirection.
When `parse_object_json_object` needs a value it calls
`parse_value_json_value` (the dispatcher). `parse_value_json_value`
inspects the next byte via `idx` and direct-calls
`parse_string_json_string`, `parse_number_json_number`,
`parse_object_json_object`, `parse_array_json_array`,
`parse_keyword_json_bool` or `parse_keyword_json_null`. Dispatch is a
single byte-match over a compile-time jump table (the per-rule Alt's
FIRST-set table the existing `DisjointFirstMiner` already mines — so
dispatch even for the top-level `value` rule rides the existing
ClassifyByte substrate). No `match cur: u16` indirection; no
FrameStack; no `advance_or_pop_with`. Each loop owns its control
state on the CPU stack per P5 profile §6 diagnosis.

The top-level entry function per grammar (`parse_<grammar>_root`)
dispatches to the grammar's entry rule's specialised function. The
grammar's rule graph determines the call structure.

## 3. Fallback for non-canonical rules

A rule whose IR predicate fires no shape detector falls back to the
existing monolithic `__dta_walker_inline::run` path — **per-rule, not
per-grammar**. The specialised dispatch prefers the shape-specific
function when one exists; otherwise the dispatcher threads a
`DtaStateId` into the monolithic walker. This keeps CSS's long tail
(pseudo-class families, media-query grammars) on the same
flat-interpreter substrate that AW-IV.W1-W3 already optimised, while
the high-frequency shapes (compoundSelector, complexSelector,
declaration, mathExpr) graduate to specialised loops.

Coverage target per the brief: ≥80% of hot-path visit frequency.
Using the existing `state_visit_frequency` miner to pick which rules
graduate first, JSON covers 100% (every rule shapes), Sheets ≥92%
(six Pratt rungs + number/string/bool/error_literal + cell_ref), CSS
L4 ≥78% (compoundSelector, complexSelector, declaration,
selectorList, mathExpr, qualifiedRule; long tail stays on walker),
BBNF ≥75% (directive, rule, term keyword-shape; identifier/literal
string-shape).

## 4. PSI integration — populating `GrammarProfile` shape slots

`GrammarProfile` extends with three new fields (already wired as
`&[]` slots per AW-IV.W1.δ projection):

```rust
pub struct GrammarProfile {
    // ...existing fields...
    /// Per-rule shape tag (None = fallback to walker).
    pub rule_shapes: Vec<(RuleId, ShapeTag)>,
    /// Per-shape SIMD kernel dispatch (NeonMemchr, NibbleLut, …).
    pub shape_kernels: Vec<(ShapeTag, KernelShape)>,
    /// Per-shape visitor method mapping.
    pub shape_visitor_methods: Vec<(ShapeTag, VisitorMethod)>,
}
```

`shape_mining.rs` populates `rule_shapes`; the existing
`select_kernel_strategy` at `kernel_shape.rs:select_kernel_strategy`
extends naturally to per-shape kernel selection;
`shape_visitor_methods` is a compile-time-derived mapping
(object→`visit_object_start`/`visit_key`/`visit_object_end`,
array→`visit_array_start`/`visit_array_end`, etc.).

## 5. Visitor trait hierarchy

The visitor trait factors into a base + per-shape sub-traits:

```rust
pub trait GrammarVisitor { type Output; /* start/end cursor */ }
pub trait ObjectVisitor: GrammarVisitor {
    fn visit_object_start(&mut self, cap_hint: u32);
    fn visit_key(&mut self, key: &[u8]);
    fn visit_object_end(&mut self);
}
pub trait ArrayVisitor: GrammarVisitor { /* analogous */ }
pub trait StringVisitor: GrammarVisitor {
    fn visit_string(&mut self, bytes: &[u8], needs_decode: bool);
}
pub trait NumberVisitor: GrammarVisitor { fn visit_f64(&mut self, x: f64); }
pub trait KeywordVisitor: GrammarVisitor {
    fn visit_keyword(&mut self, discriminant: u8);
}
pub trait PrattVisitor: GrammarVisitor {
    fn visit_operator(&mut self, op: u8, prec: u8);
    fn visit_operand_end(&mut self);
}
```

Default impl: `TapeVisitor` writes into `Columns + PayloadStream +
FrameStack` — the AW-III/IV substrate, verbatim. User-facing direct-
materialisation (`JsonValueVisitor`) implements the sub-traits and
writes directly into an owned `Value` tree. Monomorphisation at the
call site (`<V: ObjectVisitor>`) eliminates vtables; LLVM inlines
`visit_*` into the parse arms — the P5 profile §6 "fuse parse + walk"
lever.

Trait emission lives at
`crates/core/src/backend/rust/emitter/visitor_trait.rs` (new module).

## 6. Backward compatibility with AW-IV substrate

- `DTA_TABLE` const — unchanged. AX replays against it; cold-path
  `dispatch_one` still reads it.
- `StructuralIndex` — consumed by per-shape loops for O(1) delimiter
  skip (the bitmap the dispatcher's jump-table byte-match reads).
- `SHAPE_DICT` — unchanged. The object-shape emitter may consult it
  per-compound to collapse into ShapeRef when the shape is admitted.
- `PRECEDENCE_LUT` — consumed directly by the pratt-shape emitter.
- `KEYWORD_PHF` — consumed by the keyword-shape emitter.
- `__dta_walker_inline::run` — preserved; fallback for unshaped
  rules. W1.1+W2.1+W3.* AW-IV work still stands.
- `dispatch_one` — preserved as the AX replay surface.

The per-shape inline emitter is **additional** codegen complementing
the monolithic walker. Every rule graduating to a specialised shape
function removes itself from the walker's dispatch `match cur`; rules
that stay on the walker continue to benefit from AW-IV.W1-W3 fixes.

## 7. Migration phases

- **Phase 1 (JSON prototype)**. Hand-write `parse_object_json`,
  `parse_array_json`, `parse_string_json`, `parse_number_json`,
  `parse_keyword_json` plus `parse_value_json` dispatcher for
  `grammar/json/json.bbnf`. Validate sonic-rs-class throughput on
  twitter (≥2000 MB/s per AW-IV.W3 projection).
- **Phase 2 (JSON emitter)**. Lift Phase 1 into `shape_mining.rs` +
  `emitter/shapes/{object,array,string,number,keyword}.rs`. Bench gate:
  JSON twitter still ≥2000 MB/s via emitted code.
- **Phase 3 (CSS)**. Extend shape-mining to unordered-shape and
  pratt-shape. Graduates `compoundSelector`, `complexSelector`,
  `mathExpr`, `mathProduct`, plus keyword-shape for `namedColor`,
  `globalKeyword`. Bench gate: CSS bootstrap ≥1800 MB/s.
- **Phase 4 (Sheets)**. Six-rung pratt tower + keyword-shape for
  `error_literal` + number-shape + string-shape + cell_ref as
  Regex-shape (scalar span). Bench gate: Sheets parse_stress ≥100
  MB/s.
- **Phase 5 (BBNF)**. Directive keyword-shape via PHF; rule body as
  fallback walker; identifier as string-shape (Regex+Span). Bench
  gate: BBNF bbnf_self ≥600 MB/s.
- Each phase ends with per-phase samply + `nm` verification that the
  shape-specialised symbols are present and the fallback walker
  self-time has dropped commensurate with the graduated rules'
  frequency.

## 8. §6 generalisation invariant under this design

Every shape detector is an IR-structural predicate over existing
miner outputs (`delim_scan_configs`, `shape_dict_templates`,
`operator_chain_entries`, `disjoint_first_tables`, `keyword_branches`,
`pattern_alphabets`). No detector reads a grammar name. Every
per-shape emitter module reads only its detector's output + the
rule's IR body + the `VisitorMethod` mapping. Per-grammar OUTPUT
varies because each grammar's IR admits a different subset of
shapes; per-grammar MECHANISM does not vary. The grammar identity
appears only as the `<grammar>` symbol-namespace prefix on emitted
function names, exactly as AW-III §6 mandates.

## 9. IR and emitter module layout

```
crates/ir/src/passes/recognizers/
  shape_mining.rs             -- orchestrator: runs all shape detectors
  object_shape.rs             -- Object-shape predicate
  array_shape.rs              -- Array-shape predicate
  string_shape.rs             -- String-shape predicate (wraps QuotedStringMiner)
  number_shape.rs             -- Number-shape predicate
  unordered_shape.rs          -- Unordered-shape predicate
  -- keyword-shape: existing keyword_stats.rs
  -- pratt-shape: existing operator_chain.rs

crates/core/src/backend/rust/emitter/
  shapes/
    mod.rs                    -- dispatcher emission
    object.rs                 -- emit parse_object_<grammar>_<rule>
    array.rs                  -- emit parse_array_<grammar>_<rule>
    string.rs                 -- emit parse_string_<grammar>_<rule>
    number.rs                 -- emit parse_number_<grammar>_<rule>
    keyword.rs                -- emit parse_keyword_<grammar>_<rule>
    pratt.rs                  -- emit parse_pratt_<grammar>_<rule>
    unordered.rs              -- emit parse_unordered_<grammar>_<rule>
  visitor_trait.rs            -- GrammarVisitor + per-shape sub-traits
```

## 10. PSI/alphabet additions for shape mining

`StructuralAlphabet` needs no additional fields; the existing
`single_bytes / digraphs / quote_classes` cover shape-mining needs
(shape detection consumes byte sets via existing FIRST-set / Regex
alphabet miners).

Per-rule attributes mined by `shape_mining.rs`:

- **is_delimited_by(open, close)** — derived from `DelimScanMiner` output.
- **has_escape_pattern** — derived from `QuotedStringMiner` output.
- **is_regex_leaf** — single-Regex body, consumed as string/number/scalar.
- **is_disjoint_alt** — from `DisjointFirstMiner`.
- **is_operator_chain_head** — from `collect_operator_chains`.

These attributes compose into a single `RuleShapeTag` enum the
emitter dispatches on (`ShapeTag::Object { key, value, sep, open,
close }`, etc.). The enum is the bridge between shape-mining and
per-shape codegen — exactly the same wire-contract invariant the
other IR→emitter pipelines enforce.

---

## Appendix — shape-detector pseudo-code

### object_shape.rs

```rust
pub struct ObjectShapeMiner;

impl RecognizerMiner for ObjectShapeMiner {
    fn inspect(&self, node: &IrNode, nid: NodeId, ctx: &Ctx, out: &mut Out) {
        // Object-shape requires the existing delim_scan detector fired.
        let Some(ds) = out.delim_scan_configs.get(&nid) else { return };
        // And the body is Seq(key, sep, value), not generic Alt.
        let Some((open, middle, close)) = unwrap_wrap(node) else { return };
        let Some(open_b) = single_byte_literal(open, ctx.ir) else { return };
        let Some(close_b) = single_byte_literal(close, ctx.ir) else { return };
        // Walk into Repeat(Seq(key, sep, value)).
        let Some(inner) = unwrap_repeat(middle) else { return };
        let Some((key_rule, sep_b, value_rule)) = unwrap_kvsep(inner, ctx.ir) else { return };
        // Key-rule must be string-shape or ident-regex.
        if !is_string_or_ident_leaf(key_rule, ctx) { return }
        out.object_shapes.insert(nid, ObjectShape {
            open_byte: open_b, close_byte: close_b, sep_byte: sep_b,
            key_rule, value_rule,
        });
    }
}
```

### array_shape.rs

```rust
pub struct ArrayShapeMiner;

impl RecognizerMiner for ArrayShapeMiner {
    fn inspect(&self, node: &IrNode, nid: NodeId, ctx: &Ctx, out: &mut Out) {
        let Some(ds) = out.delim_scan_configs.get(&nid) else { return };
        // Array is delim_scan whose body is NOT a key-value Seq.
        if out.object_shapes.contains_key(&nid) { return }
        let Some((open, middle, close)) = unwrap_wrap(node) else { return };
        let Some(open_b) = single_byte_literal(open, ctx.ir) else { return };
        let Some(close_b) = single_byte_literal(close, ctx.ir) else { return };
        let Some(inner) = unwrap_repeat(middle) else { return };
        let Some((value_rule, sep_b)) = unwrap_list_elem_sep(inner, ctx.ir) else { return };
        out.array_shapes.insert(nid, ArrayShape {
            open_byte: open_b, close_byte: close_b, sep_byte: sep_b, value_rule,
        });
    }
}
```

### unordered_shape.rs

```rust
pub struct UnorderedShapeMiner;

impl RecognizerMiner for UnorderedShapeMiner {
    fn inspect(&self, node: &IrNode, nid: NodeId, ctx: &Ctx, out: &mut Out) {
        // Unordered = Repeat { lo: 1, inner: Alt } + DisjointFirstTable present.
        let IrNode::Repeat { inner, lo: 1, .. } = node else { return };
        let IrNode::Alt(branches, _) = inner.as_ref() else { return };
        let alt_nid = ctx.dag.node_for(inner).unwrap();
        let Some(dft) = out.disjoint_first_tables.get(&alt_nid) else { return };
        let branch_rules: Vec<RuleId> = branches.iter()
            .filter_map(|b| match &b.node { IrNode::Ref(r) => Some(*r), _ => None })
            .collect();
        if branch_rules.len() != branches.len() { return }
        out.unordered_shapes.insert(nid, UnorderedShape {
            branches: branch_rules, dispatch: dft.table,
        });
    }
}
```

### object emit skeleton

```rust
pub fn emit_parse_object_fn(
    grammar: &str, rule_name: &str, shape: &ObjectShape, ir: &IR,
) -> TokenStream {
    let fn_ident = format_ident!("parse_object_{}_{}", sanitise(grammar), rule_name);
    let value_disp = dispatcher_ident(grammar, shape.value_rule, ir);
    let key_parse = string_shape_body(grammar, shape.key_rule, ir);
    let (open, close, sep) = (shape.open_byte, shape.close_byte, shape.sep_byte);
    quote! {
        #[inline]
        pub fn #fn_ident<V: ObjectVisitor>(
            input: &[u8], pos: &mut u32, idx: &StructuralIndex, visitor: &mut V,
        ) -> Result<(), DtaError> {
            if input.get(*pos as usize).copied() != Some(#open) {
                return Err(DtaError::Syntax { /* ... */ });
            }
            *pos += 1;
            visitor.visit_object_start(0);
            skip_ws_inline(input, pos);
            if input.get(*pos as usize).copied() == Some(#close) {
                *pos += 1;
                visitor.visit_object_end();
                return Ok(());
            }
            loop {
                skip_ws_inline(input, pos);
                #key_parse  // inline string-shape body, emits visitor.visit_key(...)
                skip_ws_inline(input, pos);
                if input.get(*pos as usize).copied() != Some(#sep) {
                    return Err(DtaError::Syntax { /* ... */ });
                }
                *pos += 1;
                skip_ws_inline(input, pos);
                #value_disp(input, pos, idx, visitor)?;  // mutual-recurse
                skip_ws_inline(input, pos);
                match input.get(*pos as usize).copied() {
                    Some(b',') => { *pos += 1; continue; }
                    Some(#close) => { *pos += 1; break; }
                    _ => return Err(DtaError::Syntax { /* ... */ }),
                }
            }
            visitor.visit_object_end();
            Ok(())
        }
    }
}
```

The remaining per-shape emitters (array, string, number, keyword,
pratt, unordered) follow the same template, each splicing the SIMD
kernel or Eisel-Lemire body inline per AW-IV.W2.3, and each invoking
the rule-dispatcher `parse_value_<grammar>_<value_rule>` at every
value position.
