# BC.W0 — Typed IR Variant Table

Date: 2026-05-03
Status: settled. The variant table is the IR contract's normative alphabet. The Rust emitter (BC.W1), the TS scaffold (BC.W2), and the WASM scaffold (BC.W2) each consume this alphabet through the `Emitter` trait. Every grammar feature observable through `bbnf-parse`'s lowering reaches a variant in this table; the table is exhaustive.

## §1 Cardinality justification

Per `audit/research-anchors.md:§1`, prior-art cardinalities span 16-60 variants. bbnf-lang's typed IR is a *grammar-form* IR (closer to rustc HIR `ExprKind` at 35 than to MLIR `arith` at 60) so the 7-variant placeholder in the prior draft is structurally undersized. The variant table below carries **22 variants**, decomposed:

| Category | Count | Variants |
|---:|---:|---|
| Structural | 4 | Rule, Seq, Optional, Ref |
| Branching | 2 | AltDispatch, AltSpeculative |
| Iteration | 1 | Repeat |
| Lexical | 4 | CharClass, Keyword, Lit, Scanner |
| Composition | 4 | HostCall, MapExpr, FoldResult, Span |
| Layout-pass anchor | 1 | Layout |
| Optimisation-pass anchor | 2 | PrattSpine, SimdScan |
| Recovery / Debug | 2 | ErrorRecovery, DebugMarker |
| Synthesis | 2 | RegexDfa, EnumDiscriminator |

**Total: 22 variants.** Every variant has a producing source (where in the lowering pipeline it is created) and a consuming sink (per-backend lowering rule). No variant is speculative; each corresponds to an observed grammar feature in the nine production grammars (JSON, CSS L4, BBNF, Sheets, BNF, CSV, EBNF, CSS Pretty, Math).

## §2 Variant table

The columns: name | payload type (Rust signature) | lower-time invariants | Rust lowering | TS lowering | WASM lowering | generation site | example grammar fragment.

### Rule

| Field | Value |
|---|---|
| Payload | `Rule { rule_id: RuleId, body: TypedIRNodeId, layout: Layout, type_desc: TypeDesc }` |
| Lower-time invariants | `rule_id` resolves through `bbnf-ir::registry`; `body` is a single TypedIRNode (Seq/Alt/Repeat/etc.); `layout` is fully resolved (no `Layout::Unresolved`); `type_desc` is `TypeDesc::Named(<G>Value)` for compound rules, `TypeDesc::Slice(&str)` for terminal-typed rules |
| Rust | `pub fn parse_<rule>(input: &'i str, pos: &mut usize) -> Result<<G>Value, Err> { ... }` + `<G>Value::<Variant>` constructor at the body's bind site |
| TS | `export function parse<Rule>(ctx: ParseCtx): <G>Value { ... }` + discriminated-union variant `{ kind: '<rule>', ... }` |
| WASM | `(func $parse_<rule> (param $input i32) (param $pos i32) (result i32) ...)` + struct layout for typed result |
| Generation site | `bbnf-parse::lower::rule_lowering` (one Rule per grammar rule entry) |
| Grammar example | `value -> object \| array \| string \| number ;` |

### Seq

| Field | Value |
|---|---|
| Payload | `Seq { children: Vec<TypedIRNodeId>, layout: Layout }` |
| Lower-time invariants | `children` non-empty; `layout` is `Layout::Struct { fields: Vec<FieldLayout> }` projecting each child to a struct field; field positions are stable across regen |
| Rust | Linear push: `let f0 = parse_child0(input, pos)?; let f1 = parse_child1(input, pos)?; ...; <G>Value::<Variant> { f0, f1, ... }` |
| TS | Object literal: `const f0 = parseChild0(ctx); const f1 = parseChild1(ctx); ...; return { kind: '<rule>', f0, f1, ... }` |
| WASM | Linear-memory struct write: `(local.set $f0 (call $parse_child0 ...)) (local.set $f1 (call $parse_child1 ...)) (struct.new $StructType (local.get $f0) (local.get $f1) ...)` |
| Generation site | `bbnf-parse::lower::seq_lowering` (one Seq per concatenation; sub-Seqs flatten if same-position) |
| Grammar example | `pair -> string ":" value ;` |

### AltDispatch

| Field | Value |
|---|---|
| Payload | `AltDispatch { branches: Vec<AltBranch>, dispatch: AltDispatchKind, layout: Layout }` where `AltDispatchKind = ByteDisjoint(BitMap) \| CharClass(CharClassId) \| Keyword(PhfId) \| Regex(RegexId)` |
| Lower-time invariants | `dispatch` is byte-disjoint (every branch has a unique first-byte set); `branches.len() ≥ 2`; `layout` is `Layout::Enum { variants: Vec<VariantLayout> }`; the cost-model selected this dispatch over speculative |
| Rust | `match input.as_bytes()[*pos] { 0x7b => parse_object(input, pos), 0x5b => parse_array(input, pos), b'"' => parse_string(input, pos), ... }` |
| TS | `switch (ctx.bytes[ctx.pos]) { case 0x7b: return parseObject(ctx); case 0x5b: return parseArray(ctx); ...; }` |
| WASM | `(br_table $obj_label $arr_label $str_label ... (i32.load8_u (local.get $pos)))` |
| Generation site | `bbnf-codegen::optimiser::alt_dispatch::classify_byte_disjoint` (cost-model branch) |
| Grammar example | `value -> object \| array \| string \| number \| "true" \| "false" \| "null" ;` (byte-disjoint on `{`, `[`, `"`, digit-set, `t`, `f`, `n`) |

### AltSpeculative

| Field | Value |
|---|---|
| Payload | `AltSpeculative { branches: Vec<AltBranch>, layout: Layout }` |
| Lower-time invariants | branches ordered by likelihood (most-frequent first per BB.W3 cost-model); each branch carries its own checkpoint/restore; `layout` is `Layout::Enum`; the cost-model rejected byte-disjoint (overlap or shared prefix) |
| Rust | `let cp = checkpoint(*pos); match parse_branch_0(...) { Ok(v) => return Ok(v), Err(_) => restore(*pos, cp) }; match parse_branch_1(...) { ... }; ...` |
| TS | `const cp = ctx.pos; try { return parseBranch0(ctx) } catch { ctx.pos = cp; }; try { return parseBranch1(ctx) } catch { ... }; ...` |
| WASM | `(block $branch_0 ... (br_if $branch_0 (i32.eqz (call $parse_branch_0 ...))) (br $branches_done)) (block $branch_1 ...)` with explicit pos save/restore via `local.set`/`local.get` |
| Generation site | `bbnf-codegen::optimiser::alt_dispatch::classify_speculative` (cost-model branch) |
| Grammar example | `expr -> binary_factor \| unary_factor ;` (overlap on operand prefix) |

### Repeat

| Field | Value |
|---|---|
| Payload | `Repeat { body: TypedIRNodeId, kind: RepeatKind, separator: Option<TypedIRNodeId>, layout: Layout }` where `RepeatKind = ZeroOrMore \| OneOrMore \| ZeroOrMoreNonGreedy \| OneOrMoreNonGreedy \| Bounded { lo: u32, hi: u32 }` |
| Lower-time invariants | `body` non-recursive at first-byte (Repeat must terminate); `layout` is `Layout::Vec { element: Box<Layout> }` or `Layout::SmallVec { element, inline_capacity }` per cost-model; bounded repeats use `[T; N]` if N small |
| Rust | `let mut buf = SmallVec::new(); loop { match parse_body(input, pos) { Ok(v) => buf.push(v), Err(_) => break } if let Some(sep) = ... { /* consume separator or break */ } }` |
| TS | `const buf: T[] = []; while (true) { try { buf.push(parseBody(ctx)) } catch { break }; ... }` |
| WASM | `(loop $repeat_loop (local.set $val (call $parse_body ...)) (call $vec_push (local.get $buf) (local.get $val)) (br_if $repeat_loop (i32.eqz (call $parse_failed))))` |
| Generation site | `bbnf-parse::lower::repeat_lowering` (one Repeat per `*` / `+` / `?{lo,hi}` operator; non-greedy from explicit `*?` / `+?`) |
| Grammar example | `array -> "[" value ("," value)* "]" ;` |

### Optional

| Field | Value |
|---|---|
| Payload | `Optional { body: TypedIRNodeId, layout: Layout }` |
| Lower-time invariants | `body` is a single TypedIRNode; `layout` is `Layout::Option { inner: Box<Layout> }`; first-byte set computed for peek-then-commit |
| Rust | `let v = if input.as_bytes().get(*pos).map_or(false, \|b\| FIRST_<rule>.contains(b)) { Some(parse_body(input, pos)?) } else { None };` |
| TS | `const v = FIRST_<rule>.has(ctx.bytes[ctx.pos]) ? parseBody(ctx) : null;` |
| WASM | `(if (call $first_set_check ...) (then (call $parse_body ...)) (else (i32.const 0)))` |
| Generation site | `bbnf-parse::lower::optional_lowering` (one Optional per `?` operator) |
| Grammar example | `signed_number -> "-"? digit+ ;` |

### CharClass

| Field | Value |
|---|---|
| Payload | `CharClass { class: CharClassId, layout: Layout }` |
| Lower-time invariants | `class` resolves through `bbnf-ir::char_class_table`; `layout` is `Layout::Slice(&str)` for span-capture or `Layout::Char` for single-char; bitmap or DFA pre-computed |
| Rust | Bitmap path: `if (CHAR_CLASS_<id>_BITMAP[b as usize >> 5] & (1 << (b & 31))) != 0 { ... }`; SIMD path (post BB.W3 detection): `simd_scan_class_<id>(input, pos)` |
| TS | `if (CHAR_CLASS_<id>_REGEX.test(String.fromCharCode(ctx.bytes[ctx.pos]))) { ... }` |
| WASM | `(i32.and (i32.load (i32.add (global.get $CHAR_CLASS_BITMAP) (i32.shr_u (local.get $b) (i32.const 5)))) (i32.shl (i32.const 1) (i32.and (local.get $b) (i32.const 31))))` |
| Generation site | `bbnf-parse::lower::char_class_lowering` (one CharClass per `[a-z]` / `\d` / `[^,]` style) |
| Grammar example | `digit -> [0-9] ;` |

### Keyword

| Field | Value |
|---|---|
| Payload | `Keyword { keyword: KeywordId, layout: Layout }` |
| Lower-time invariants | `keyword` resolves through PHF table when ≥ 4 keywords share the same Alt; `layout` is `Layout::Unit` (zero-sized; the keyword identity is the rule) or `Layout::Slice(&'static str)` for keyword-bearing rules |
| Rust | Single keyword: `if input[*pos..].starts_with(b"true") { *pos += 4; ... }`; PHF: `match phf_lookup(...) { ... }`; suffix elision when next byte cannot continue keyword |
| TS | `if (ctx.bytes.subarray(ctx.pos, ctx.pos + 4).every((b, i) => b === KEYWORD_TRUE[i])) { ... }` |
| WASM | Per-byte i32 compare via `i32.eq` chain or `memcmp` import |
| Generation site | `bbnf-parse::lower::keyword_lowering` (one Keyword per quoted-string literal in grammar) |
| Grammar example | `bool -> "true" \| "false" ;` |

### Lit

| Field | Value |
|---|---|
| Payload | `Lit { value: LitValue, layout: Layout }` where `LitValue = Bytes(Vec<u8>) \| Str(String)` |
| Lower-time invariants | `value` is a literal byte sequence (distinct from Keyword which carries identity); `layout` is `Layout::Slice(&'static [u8])` or `Layout::Unit` for syntactic punctuation |
| Rust | `if input.as_bytes()[*pos..].starts_with(b"<lit>") { *pos += <lit>.len(); ... }` |
| TS | `if (ctx.bytes.subarray(ctx.pos, ctx.pos + N).every((b, i) => b === LIT[i])) { ... }` |
| WASM | Per-byte compare or memcmp |
| Generation site | `bbnf-parse::lower::lit_lowering` (separate from Keyword to allow Lit to appear in non-Alt positions, e.g. punctuation in Seq) |
| Grammar example | `array -> "[" elements "]" ;` (the `[` and `]` are Lits, not Keywords) |

### Scanner

| Field | Value |
|---|---|
| Payload | `Scanner { scanner_id: ScannerId, kind: ScannerKind, layout: Layout }` where `ScannerKind = RegexDfa \| BespokeNfa \| InlineByteTest` |
| Lower-time invariants | `scanner_id` resolves through `bbnf-ir::scanner_table`; `kind` is cost-model-selected (RegexDfa for complex classes, BespokeNfa for moderate, InlineByteTest for trivial 1-2 byte tests); `layout` is `Layout::Slice(&str)` |
| Rust | RegexDfa: `let m = bbnf_regex::dfa_<id>::run(input, *pos)?; *pos = m.end;`; BespokeNfa: hand-written byte loop; InlineByteTest: `while is_<class>(input.as_bytes()[*pos]) { *pos += 1; }` |
| TS | RegexDfa: TS `RegExp` equivalent; InlineByteTest: TS byte loop |
| WASM | RegexDfa: bytecode walk against pre-computed DFA table; InlineByteTest: WASM byte loop |
| Generation site | `bbnf-codegen::optimiser::scanner_classify` (cost-model selects the kind) |
| Grammar example | `whitespace -> [ \t\r\n]+ ;` (InlineByteTest); `string_body -> /[^"\\]*/* ;` (RegexDfa) |

### Ref

| Field | Value |
|---|---|
| Payload | `Ref { rule_id: RuleId, layout: Layout }` |
| Lower-time invariants | `rule_id` resolves through `bbnf-ir::registry`; `layout` is `Layout::Named(<G>Value)` mirror of the referenced rule's body Layout; recursion-cycle-safe (cycles permitted, infinite recursion guarded by Repeat termination check) |
| Rust | `let v = parse_<rule>(input, pos)?;` |
| TS | `const v = parse<Rule>(ctx);` |
| WASM | `(call_indirect $parse_<rule>_index ...)` |
| Generation site | `bbnf-parse::lower::ref_lowering` (one Ref per non-terminal grammar reference) |
| Grammar example | `pair -> string ":" value ;` (the `string` and `value` are Refs) |

### HostCall

| Field | Value |
|---|---|
| Payload | `HostCall { fn_id: HostFnRef, args: Vec<TypedIRNodeId>, layout: Layout }` |
| Lower-time invariants | `fn_id` resolves through per-grammar host-fn metadata at `grammar/<g>/host/`; `args` are typed IR nodes whose results pipe into the host call; `layout` is the host fn's declared return Layout per grammar metadata |
| Rust | `let v = grammar::host::<g>::<fn_name>(arg0, arg1, ...)?;` (where `<g>` is per-grammar host namespace per surgery G05-1) |
| TS | `const v = runtime.<fnName>(arg0, arg1, ...);` (host fn resolution table emitted as runtime parameter, BD scope) |
| WASM | `(call $host_fn_<id>_extern (local.get $arg0) (local.get $arg1) ...)` (indexed extern import, BD scope) |
| Generation site | `bbnf-parse::lower::host_lowering` (one HostCall per host-fn invocation in grammar source); `bbnf-codegen` reads per-grammar host metadata, never hardcodes grammar names per surgery G05-4 |
| Grammar example | `hex -> "#" [0-9a-f]+ -> parse_hex_color ;` (the `parse_hex_color` is a HostCall) |

### MapExpr

| Field | Value |
|---|---|
| Payload | `MapExpr { inner: TypedIRNodeId, target: MapTarget, layout: Layout }` where `MapTarget = TypedEnumVariant { variant_id: VariantId } \| FieldProjection { field_path: Vec<FieldId> } \| Custom { fn_id: HostFnRef }` |
| Lower-time invariants | `inner` produces the source value; `target` names the projection; `layout` is the target's Layout (variant constructor or field's Layout); MapExpr never discards values per `feedback_no_value_discard` |
| Rust | TypedEnumVariant: `<G>Value::<Variant>(inner)` ; FieldProjection: `inner.<field>` ; Custom: host-fn invocation |
| TS | TypedEnumVariant: `{ kind: '<variant>', value: inner }` ; FieldProjection: `inner.<field>` |
| WASM | TypedEnumVariant: tag write + payload write ; FieldProjection: struct field load |
| Generation site | `bbnf-parse::lower::map_expr_lowering` (one MapExpr per `->` arrow in grammar) |
| Grammar example | `value -> string -> JsonValue::String ;` |

### Layout

| Field | Value |
|---|---|
| Payload | `Layout { node: TypedIRNodeId, sink: LayoutSink }` (note: this is a *marker* variant, not a structural one — it anchors the layout-lowering pass) |
| Lower-time invariants | Layout markers ANCHOR the layout-lowering pass; they appear in the IR before layout resolution and are eliminated post-resolution; per surgery 4, `Layout` replaces every `TypeDesc` reference in IR vocabulary |
| Rust | None at lower time (Layout markers do not emit code; they are pass anchors) |
| TS | None |
| WASM | None |
| Generation site | `bbnf-parse::lower::layout_marker` (one Layout per compound rule pre-resolution; eliminated post `bbnf-ir::passes::layout::resolve`) |
| Grammar example | (no direct grammar fragment — Layout is a pass anchor, not a syntactic feature) |

### Span

| Field | Value |
|---|---|
| Payload | `Span { inner: TypedIRNodeId, kind: SpanKind, layout: Layout }` where `SpanKind = ByteRange \| StrSlice \| ByteSlice` |
| Lower-time invariants | `inner` is the wrapped node whose span is captured; `layout` is `Layout::Range { kind: SpanKind }`; the span captures `(start, end)` of the inner's match |
| Rust | `let start = *pos; let inner_v = parse_inner(input, pos)?; let end = *pos; (input[start..end], inner_v)` (StrSlice) or `(start..end, inner_v)` (ByteRange) |
| TS | `const start = ctx.pos; const innerV = parseInner(ctx); const end = ctx.pos; { span: ctx.input.substring(start, end), value: innerV }` |
| WASM | Two i32 locals for start/end; struct.new with span fields |
| Generation site | `bbnf-parse::lower::span_lowering` (one Span per `@span` annotation in grammar; auto-emitted for terminals if Layout requires) |
| Grammar example | `identifier @span -> [a-zA-Z_][a-zA-Z0-9_]* ;` |

### FoldResult

| Field | Value |
|---|---|
| Payload | `FoldResult { source: TypedIRNodeId, fold_fn: FoldFnId, layout: Layout }` |
| Lower-time invariants | `source` is typically a Repeat or AltDispatch; `fold_fn` resolves through `bbnf-ir::fold_table`; `layout` is the fold's accumulator Layout |
| Rust | `let acc = source.iter().fold(init, fold_fn);` |
| TS | `const acc = source.reduce(foldFn, init);` |
| WASM | `loop $fold_loop (local.set $acc (call $fold_fn (local.get $acc) (local.get $next))) (br_if $fold_loop ...)` |
| Generation site | `bbnf-parse::lower::fold_lowering` (one FoldResult per `@fold` annotation; cost-model may emit fold for left-associative repeats) |
| Grammar example | `expr -> term ("+" term)* @fold(plus_fold) ;` |

### PrattSpine

| Field | Value |
|---|---|
| Payload | `PrattSpine { atom: TypedIRNodeId, ops: Vec<PrattOp>, layout: Layout }` where `PrattOp = { op_token: KeywordId, prec: u8, assoc: Assoc, kind: PrattOpKind }` and `PrattOpKind = Prefix \| Postfix \| InfixLeft \| InfixRight` |
| Lower-time invariants | Mined from left-recursive operator-chain grammar shape per BB.W3 cost-model; `atom` is the non-recursive base; `ops` enumerated by BB.W3's mining; `layout` is `Layout::Pratt { atom_layout, op_layouts }` projecting to the operator-chain's typed AST |
| Rust | Pratt parsing loop: `let mut left = parse_atom(...)?; loop { let op = peek_op(...); if op.prec < min_prec { break; } let right = parse_atom_with_prec(op.prec_next)?; left = match op.kind { ... }; }` |
| TS | Same pattern in TS idiom |
| WASM | Pratt loop with explicit precedence stack via WASM table |
| Generation site | `bbnf-codegen::optimiser::pratt_detect` (cost-model branch); never user-annotated per Lock 10 |
| Grammar example | `expr -> expr "+" expr \| expr "*" expr \| atom ;` (mined to PrattSpine with `+` prec=1 left-assoc, `*` prec=2 left-assoc) |

### SimdScan

| Field | Value |
|---|---|
| Payload | `SimdScan { alphabet: SimdAlphabetId, kind: SimdScanKind, layout: Layout }` where `SimdScanKind = RangeShuffle \| BitMask \| EqMask` |
| Lower-time invariants | `alphabet` is a structural alphabet (delimiter set, whitespace set, etc.) mined per BB.W3 SIMD auto-detection; `kind` selected by cost-model + alphabet density; `layout` is `Layout::Slice(&str)` |
| Rust | NEON / SSE intrinsics: `let v = vld1q_u8(input.as_ptr().add(*pos)); let mask = vceqq_u8_or_chain(...); let first = first_set_bit(mask) ; *pos += first;` |
| TS | Falls back to scalar loop in TS scaffold (BD scope for actual SIMD-WASM) |
| WASM | `v128.load`, `v128.shuffle`, `i8x16.eq`, `v128.bitmask` |
| Generation site | `bbnf-codegen::optimiser::simd_detect` (cost-model branch); never user-annotated per Lock 10 |
| Grammar example | `until_quote -> /[^"\\]*/* ;` (mined to SimdScan if input length > threshold) |

### ErrorRecovery

| Field | Value |
|---|---|
| Payload | `ErrorRecovery { node: TypedIRNodeId, sync_set: Vec<TokenId>, layout: Layout }` |
| Lower-time invariants | `sync_set` is the set of recovery tokens (per `@recover` annotation or auto-detected from Repeat boundaries); `layout` is `Layout::Result { ok: Box<Layout>, err: Box<ErrorLayout> }` |
| Rust | `match parse_node(...) { Ok(v) => Ok(v), Err(e) => { skip_until(input, pos, &SYNC_SET_<id>); Err(e) } }` |
| TS | Same pattern |
| WASM | Same pattern |
| Generation site | `bbnf-parse::lower::recovery_lowering` (per `@recover` annotation; auto-emitted at Repeat boundaries when error recovery is enabled at compile time) |
| Grammar example | `stmt @recover(";") -> ... ;` |

### DebugMarker

| Field | Value |
|---|---|
| Payload | `DebugMarker { node: TypedIRNodeId, marker: DebugMarkerKind, layout: Layout }` where `DebugMarkerKind = Breakpoint \| Trace \| AssertionPoint` |
| Lower-time invariants | Anchored per `@debug` directive in grammar source per `project_debug_infra`; `layout` is the wrapped node's layout (DebugMarker is structurally transparent); emitted only when `debug` feature flag is set |
| Rust | `#[cfg(feature = "debug")] { trace!("entering rule <name>"); } let v = parse_node(...)?; #[cfg(feature = "debug")] { trace!("exiting rule <name>"); }` |
| TS | Conditional `console.debug` calls |
| WASM | Conditional host-fn `debug_trace` import |
| Generation site | `bbnf-parse::lower::debug_lowering` (per `@debug` directive); compile-time stripped when feature off |
| Grammar example | `rule @debug -> ... ;` |

### RegexDfa

| Field | Value |
|---|---|
| Payload | `RegexDfa { dfa_id: DfaId, layout: Layout }` |
| Lower-time invariants | `dfa_id` resolves to a pre-computed DFA in `bbnf-regex::dfa_table`; the DFA was synthesised at xtask regen time per Lock 6; `layout` is `Layout::Slice(&str)` |
| Rust | `let m = bbnf_regex::dfa::DFA_<id>.run(input, *pos)?; *pos = m.end;` |
| TS | TS regex equivalent (or DFA bytecode walker) |
| WASM | DFA bytecode walker against pre-computed table |
| Generation site | `bbnf-codegen::optimiser::regex_synthesise` (cost-model upgrade from Scanner::RegexDfa when DFA table cache hits) |
| Grammar example | `string_body -> /([^"\\]\|\\["\\bfnrt\\\/]\|\\u[0-9a-fA-F]{4})*/ ;` |

### EnumDiscriminator

| Field | Value |
|---|---|
| Payload | `EnumDiscriminator { source: TypedIRNodeId, discriminator: DiscriminatorKind, layout: Layout }` where `DiscriminatorKind = Tag(u8) \| FieldValue(FieldId, Vec<u8>) \| TypeBranch` |
| Lower-time invariants | `source` produces an inner value that needs typed-enum tag projection; `discriminator` resolves which variant; `layout` is `Layout::Enum { discriminator_kind: DiscriminatorKind, variants: ... }`; per `feedback_typed_materialization_invariant`, every `->` reaches the tape emitter through the discriminator |
| Rust | `<G>Value::<Variant>(source)` with explicit tag write; the variant id derives from compile-time analysis |
| TS | `{ kind: '<variant>', value: source }` |
| WASM | Tag write before payload write |
| Generation site | `bbnf-parse::lower::discriminator_lowering` (one EnumDiscriminator per typed-enum constructor site; closes G05 G05-7's inverse-layout-audit invariant: every compound rule reaches a tagged emit) |
| Grammar example | `value -> object -> JsonValue::Object \| array -> JsonValue::Array \| ... ;` |

## §3 Variant table summary

| # | Name | Category | Generation site |
|---:|---|---|---|
| 1 | Rule | Structural | `bbnf-parse::lower::rule_lowering` |
| 2 | Seq | Structural | `bbnf-parse::lower::seq_lowering` |
| 3 | Optional | Structural | `bbnf-parse::lower::optional_lowering` |
| 4 | Ref | Structural | `bbnf-parse::lower::ref_lowering` |
| 5 | AltDispatch | Branching | `bbnf-codegen::optimiser::alt_dispatch::classify_byte_disjoint` |
| 6 | AltSpeculative | Branching | `bbnf-codegen::optimiser::alt_dispatch::classify_speculative` |
| 7 | Repeat | Iteration | `bbnf-parse::lower::repeat_lowering` |
| 8 | CharClass | Lexical | `bbnf-parse::lower::char_class_lowering` |
| 9 | Keyword | Lexical | `bbnf-parse::lower::keyword_lowering` |
| 10 | Lit | Lexical | `bbnf-parse::lower::lit_lowering` |
| 11 | Scanner | Lexical | `bbnf-codegen::optimiser::scanner_classify` |
| 12 | HostCall | Composition | `bbnf-parse::lower::host_lowering` |
| 13 | MapExpr | Composition | `bbnf-parse::lower::map_expr_lowering` |
| 14 | FoldResult | Composition | `bbnf-parse::lower::fold_lowering` |
| 15 | Span | Composition | `bbnf-parse::lower::span_lowering` |
| 16 | Layout | Layout-pass anchor | `bbnf-parse::lower::layout_marker` (eliminated post-resolution) |
| 17 | PrattSpine | Optimisation-pass anchor | `bbnf-codegen::optimiser::pratt_detect` |
| 18 | SimdScan | Optimisation-pass anchor | `bbnf-codegen::optimiser::simd_detect` |
| 19 | ErrorRecovery | Recovery / Debug | `bbnf-parse::lower::recovery_lowering` |
| 20 | DebugMarker | Recovery / Debug | `bbnf-parse::lower::debug_lowering` |
| 21 | RegexDfa | Synthesis | `bbnf-codegen::optimiser::regex_synthesise` |
| 22 | EnumDiscriminator | Synthesis | `bbnf-parse::lower::discriminator_lowering` |

## §4 Cardinality defence

| Comparator | Variants | Conclusion |
|---|---:|---|
| MLIR `arith` (single dialect) | 60 | bbnf-lang's IR spans grammar features, not arithmetic ops; 60 is the wrong scale |
| Cranelift `InstructionData` | 40 | Memory ops are not in scope for bbnf-lang's IR; control flow ops collapse to AltDispatch / AltSpeculative |
| rustc HIR `ExprKind` | 35 | Closest analogue; bbnf-lang has fewer expression forms but adds optimisation-pass anchors and recovery; net 22 is bounded above by 35 |
| rustc HIR `ItemKind` | 16 | Module-level concerns are not in scope for grammar IR |
| chalk `TyKind` | 23 | Type-only IR; bbnf-lang's typed IR is grammar-form, not type-form, but the 23-variant cardinality is a valid upper bound for the *typed* projection |

The 22-variant table is in the right scale — denser than chalk (which carries no grammar features) and thinner than HIR (which carries Rust-specific expression forms). Each variant has a producing site and a consuming sink across all three backends; no variant is speculative.

## §5 Invariants

§I1. **Every `->` arrow reaches EnumDiscriminator** per `feedback_typed_materialization_invariant`. The lowering chain `MapExpr → EnumDiscriminator` is the typed-emit reach guarantee.

§I2. **Layout is a pass anchor, not a runtime variant**. Per surgery 4, `Layout` replaces `TypeDesc` in IR vocabulary; `TypeDesc` survives only as a *field* of resolved Layout, never as a separate canonical IR term.

§I3. **No grammar names in `bbnf-codegen`**. Per G05-4, host-fn / map-expr / discriminator names resolve through per-grammar metadata emitted by `bbnf-parse`; codegen never hardcodes JSON / CSS / Sheets / BBNF identifiers.

§I4. **Scanner / RegexDfa / SimdScan are cost-model selected**, not user-annotated. Per Lock 10 and `feedback_pluggable_components`, the optimiser owns the kind selection through the BB.W3 cost model.

§I5. **DebugMarker is feature-gated**. Compile-time stripped when the `debug` feature is off; zero runtime overhead for production builds.

## §6 Lowering rules summary

| Lower-time pass | Variants emitted | Variants eliminated |
|---|---|---|
| `bbnf-parse::lower::rule_lowering` | Rule, Seq, Optional, Ref, Repeat, CharClass, Keyword, Lit, HostCall, MapExpr, FoldResult, Span, Layout (markers), DebugMarker, ErrorRecovery, EnumDiscriminator | (none — this is the lower entry) |
| `bbnf-ir::passes::layout::resolve` | (annotates all variants with resolved Layout) | Layout (markers) |
| `bbnf-codegen::optimiser::scanner_classify` | Scanner | (subsumes inline byte-tests when Scanner kind is InlineByteTest) |
| `bbnf-codegen::optimiser::alt_dispatch::classify_*` | AltDispatch, AltSpeculative | (replaces raw Alt with classified variants) |
| `bbnf-codegen::optimiser::pratt_detect` | PrattSpine | (replaces left-recursive Alt+Repeat chains with PrattSpine) |
| `bbnf-codegen::optimiser::simd_detect` | SimdScan | (replaces CharClass / Scanner with SimdScan when alphabet density + length cross threshold) |
| `bbnf-codegen::optimiser::regex_synthesise` | RegexDfa | (replaces Scanner::RegexDfa with explicit RegexDfa when DFA table cached) |

## §7 Open notes

- The variant set is exhaustive for the nine production grammars at BC time. New grammar features (e.g. negative lookahead) require a new variant + a `Layout`/`LayoutSink` rule, gated by an in-plan amendment to BC.W0 before the feature ships.
- The variant table is the IR contract's normative alphabet; the contract document at `docs/codegen-IR-CONTRACT.md` (landed at BC.W0a) reproduces this table verbatim with cross-references to the lowering pass sites.
- BD's TS/WASM activation per carry BC→BD.C1 consumes the `Rust` / `TS` / `WASM` columns directly; no new variants are introduced at BD time without an in-plan amendment.
