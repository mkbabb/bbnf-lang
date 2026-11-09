# SK-V18 G2 — RESEARCH + PLAN: CSS lowering (R-B B⊃A — `css_balanced_component_scan` primitive + fact-keyed projection)

G2 RESEARCH+PLAN. Entry gate = G1 closed ∧ P3 closed ∧ P4 live (DUAL gate). No cargo / no
regen this pass. Every claim grounded in the live tree (cwd `skinny/`). **Line numbers in the
SPEC (`:701`, `:1125`, etc.) reference an OLDER revision; the actual `CSS_GENERATED_RS` const is
at `runtime_generator.rs:344`→`:1254` and the canonical runtime copy is
`crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:1-910`. All cites below are
to the CURRENT tree.**

---

## 0. Executive verdict (the honest crux up front)

**G2 is achievable BUT requires a documented SPEC amendment — the EXACT analog of the ratified
G1 split-equivalence AMENDMENT.** The CSS provider CAN be made grammar-DERIVED from
`stylesheet.bbnf` (+ its 14 l4 imports); the `CSS_GENERATED_RS` courier CAN be deleted; the
94.1% scan CAN land as the `css_balanced_component_scan` named primitive with grammar-derived
args; the 9-field cssparser oracle parity and the >SOTA ratio CAN be preserved. The irreducible
honesty problem, identical in shape to G1's: the `CssFullParser` is a **flat balanced-delimiter
recognizer** whose CONTROL SHELL (the `parse_stylesheet`/`parse_block`/`parse_declaration`
driver loop + the mutually-recursive `find_component_delim`/`consume_balanced_at` scan) is NOT
byte-derivable from a generic grammar walk — the grammar `stylesheet.bbnf` describes a RICH
RECURSIVE CSSOM, and reproducing the FLAT scan byte-exact from a tree walk is impossible (a tree
walk yields the combinator-shaped descent that IS lightningcss and regresses >SOTA). So the
binding gate must be: **(i) the `css_balanced_component_scan` scan-core as a grammar-PARAMETERIZED
named primitive (HARD: blob-free + arg-mutation-fires + (d) narrow-leaf); (ii) STRUCTURAL +
BEHAVIORAL equivalence (NOT byte-equivalence) for the drivers + lazy projection emitted as
grammar-fact-keyed `push_str` blocks; (iii) the 9-field oracle parity EXACT; (iv) the >SOTA
same-run ratio held.** This is precisely the B⊃A hybrid the SPEC §5 already specifies — the
amendment merely makes EXPLICIT that "grammar-derived" means structural+behavioral (G1's
precedent), not byte-for-byte, for everything except the named primitive's blob-free + arg gate.

**The hardest single risk is NOT the scan primitive — it is the LOWERING GAP (deliverable #2).**
CSS literally cannot reach `lower::lower_to_rust` today, and the reason is structural, not a
config flip (see §2). G2 must author a NEW grammar-AST analysis (`lower/css_scan.rs`) that walks
the raw parsed grammar (NOT the BackendIr, which CSS never builds), because `passes::compile`
rejects CSS's `@token`/`@pretty`/`->`-projection constructs. That is the largest piece of G2
work and the most likely REDRESS site.

---

## 1. CURRENT-STATE MAP — the CSS provider end to end

### 1.1 — The emit path (what G2 deletes)

`emit_request_facts` (`runtime_generator.rs:74-101`) builds the CSS provider as a `BTreeMap` of
4 files, each a `normalize(CONST)` splice of a `r#"..."#` raw-string body:

| file | source const | LOC | role |
|---|---|---|---|
| `generated.rs` | `CSS_GENERATED_RS` (`:344`→`:1254`) | **~910** | THE courier — the entire `CssFullParser` + lazy projection |
| `mod.rs` | `CSS_MOD_RS` (`:241-253`) | 13 | re-exports |
| `parser.rs` | `CSS_PARSER_RS` (`:255-306`) | 52 | `parse`/`summary`/`rich_summary` thin wrappers over `generated::parse_into_tape` |
| `sink.rs` | `CSS_SINK_RS` (`:308-324`) | 17 | `CssFactError` struct |
| `config.rs` | `render_request_facts_config` (`:103-132`) | computed | `ROW_ID`/`ENTRY_RULE`/etc. request-identity consts |

The `.bbnf` is **NEVER consumed** for the body: `emit_from_request` (`:12-27`) routes
`RuntimeEmitterKind::RequestFacts` → `emit_request_facts`, which ignores `facts` for the body and
only reads `facts.source_hash`/`frontend.{sources,imports,layout}.len()` for the config consts
(`:122-131`). Only `RuntimeEmitterKind::CompiledLowering` (JSON) reaches `emit_from_source` →
`passes::compile` → `lower::lower_to_rust` (`lib.rs:107-118`, `emit_with_layout:154-179`). **CSS
does not touch `lower/` at all.** P3 has already collapsed the 7 css_l4 replicas to ONE
(`crates/runtime/src/grammars/css_l4_declaration_values/` is the sole surviving dir; ledger
`waves/w-prune/P3-collapse-ledger.md`); the canonical `generated.rs` md5 was `b654562c…`.

### 1.2 — `CssFullParser` structure (the load-bearing recognizer)

`struct CssFullParser{ bytes, pos, tape }` (`:768-772`). It is a FLAT balanced-delimiter
recognizer that pushes structural offsets into the EXISTING skinny offset tape
(`push_offset`/`push_plain_offset`, `:787-801`) and recovers node kind LAZILY from the source
byte + a sparse `AT_RULE_FLAG = GRAMMAR_BIT0` (`CssNodeKind::at_cursor`, `:366-383`) — the CSS
analog of `JsonNodeKind::at_cursor`. The drivers (≤4.2% combined, per S-P1):

- `parse_stylesheet` (`:803-820`) — top loop: skip ws/comments, skip CDO/CDC/`@charset`, dispatch
  on `@` → `parse_at_rule`, `;` → skip, else → `parse_qualified_rule`.
- `parse_at_rule` (`:822-851`) — consume `@name`, then `find_component_delim(pos, b";{}")`; on `{`
  push at-rule flag + recurse `parse_block`.
- `parse_qualified_rule` (`:853-869`) — `find_component_delim(pos, b"{};")`; on `{` push qualified
  rule + `parse_block`.
- `parse_block` (`:871-889`) — loop until `}`; dispatch `@`/`;`/else→`parse_block_item`.
- `parse_block_item` (`:891-921`) — `find_component_delim(pos, b"{};")`; `{`→nested rule, `;`/`}`
  with a colon-before → `parse_declaration`, else error.
- `parse_declaration(start, colon)` (`:923-942`) — `find_component_delim(pos, b";}")`; push the
  declaration at its `:` offset.

### 1.3 — The 94.1% hot leaf, mapped PRECISELY (grammar-data vs hand-written control)

The hot leaf is TWO mutually-recursive fns that are the SAME scalar scanning machine:

**`find_component_delim(pos, delimiters: &[u8])` (`:1000-1023`, 79.5% parser self-time):**
```
while pos < len:
  byte = bytes[pos]
  if delimiters.contains(byte): return Some((byte, pos))   // (A) the STOP set — GRAMMAR DATA
  pos = match byte {                                         // (B) the SKIP dispatch — STRUCTURAL FAMILY
    b'\'' | b'"'              => consume_string_at(pos),     //   string skip
    b'/' if next==b'*'        => consume_comment_at(pos),    //   comment skip
    b'(' => consume_balanced_at(pos, b')'),                  //   nested () balance
    b'[' => consume_balanced_at(pos, b']'),                  //   nested [] balance
    b'{' => consume_balanced_at(pos, b'}'),                  //   nested {} balance
    b')' | b']' | b'}'        => return Err(...),            //   unexpected close
    _ => pos + 1,                                            //   inert advance (THE vectorizable run)
  }
```

**`consume_balanced_at(start, close: u8)` (`:1036-1056`, 14.6%):** the identical scan with a
`close`-byte stop and recursion into nested `()[]{}` — the inner-recursive half.

**What is GRAMMAR-DATA (the ARGS the primitive must take):**
- The **delimiter STOP set** passed at each call site: `b";{}"` (at-rule), `b"{};"`
  (qualified/block-item), `b";}"` (declaration value), `b":{};"` (`find_colon_before`,
  `:1025-1034`), `b":{}"`/`b"{};"` family. These are the structural delimiters
  **emergent from the rule shapes**: `{` `}` from `ruleBlock = "{" >> blockContent << "}"`
  (`stylesheet.bbnf:16`); `;` from the `";"` terminators in `properties.bbnf` declarations
  (`colorDecl … , ";" ?`, `:161` etc.) and `genericAtRule … atRuleBody = ruleBlock | ";"`
  (`stylesheet.bbnf:33`); `:` from the declaration `":"` split (`properties.bbnf:161` `colorProps , ":" …`)
  and the selector colon (`selectors.bbnf:63,69` `":"`/`"::"`).
- The **structural-byte SKIP family** `' " / ( ) [ ] {`: the string quotes from `cssString`
  (`properties.bbnf:36`) / `string` (selectors); the comment `/*` from the `@ws` regex
  (`stylesheet.bbnf:12`); the open/close pairs `( )` from the selector pseudo-functions
  (`selectors.bbnf:54-78`, 10×`"("` / 11×`")"`), `[ ]` from `attrSelector` (`selectors.bbnf:39`),
  `{ }` from `ruleBlock`.
- The **at-rule-vs-qualified branch tag** (`AT_RULE_FLAG`): the `BackendRule` branch projection —
  at-rule branch (`mediaRule|keyframesRule|genericAtRule`, `stylesheet.bbnf:41`) vs qualified
  (`qualifiedRule`, `:19`).

**What is HAND-WRITTEN CONTROL FLOW (NOT derivable from any single rule, the §6 finding):**
- The `while pos<len { match byte {…} }` scan-and-skip loop structure itself.
- The mutual recursion `find_component_delim ⇄ consume_balanced_at` for nested balancing.
- The string-skip (`consume_string_at:1069-1093`), comment-skip (`consume_comment_at:1058-1067`),
  escape-skip (`consume_escape_at:1095-1100`) sub-machines.
- The error contract (`css_full_error` at unexpected-close `:1017`/`:1050`, unclosed
  `:1055`/`:1066`/`:1092`).

This split is the heart of B⊃A: the STOP-set and SKIP-family are grammar-DERIVED ARGS; the
scan loop + recursion + skip machines are the irreducible ALGORITHM that lands as the named
primitive (blob-free, in `runtime_scan`/`runtime_simd`, NOT a `r#"..."#` splice).

### 1.4 — The lazy rich projection (preserve-rich-ast, the >SOTA product)

The typed CSSOM (`:385-700`), recovered LAZILY from the tape (zero arena writes, isomorphic to
JSON's `value_from_ref`):
- `CssNode` (`:387-453`) — 8-byte `ValueRef` cursor; `kind()`/`value()` decode from source byte +
  `AT_RULE_FLAG`; `left_boundary` (`:410-429`) scans back to the nearest `{`/`}`/`;`.
- `CssRule` (`:467-502`) — `is_at_rule`, `prelude()` (lazy selector slice), `selector_count()`
  (`:492-501`, `1 + count_top_level_commas(prelude)` → the SHARED neutral kernel
  `crate::runtime_simd::count_top_level_commas`, `:1152-1154`).
- `CssDeclaration` (`:508-547`) — `property()` (`colon`-split), `value_span()` (`scan_value_end`,
  `:1156-1179`), `typed_value()` → `CssTypedValue::classify`.
- `CssTypedValue::classify` (`:568-597`) — value-head byte classification into
  Dimension/Number/Color/Function/Keyword/Other (`number_has_unit:1199-1221`,
  `leading_ident_is_function:1225-1236`). The S-P1-flagged second hot candidate post-NEON.
- `CssDocument::summary`/`rich_summary` (`:622-674`) — the 4-field / 9-field loops over
  `self.nodes()`. `rich_summary` is the >SOTA `track1_rich` product; equality-by-construction
  with the recognizer counts.

The 9-field rich summary is the parity surface: `{rules, at_rules, qualified_rules, declarations,
selectors, dimensions, numbers, colors, functions}` (`CssRichSummary`, `:689-700`). The oracle is
`bbnf-bench/src/nonjson_css_l4.rs::assert_rich_strict_equality` (`:128`), which renders BOTH
`rich_summary` and an independent cssparser `OracleParser` to the canonical
`css-l4-rich-summary-v1` string and asserts exact equality.

---

## 2. THE LOWERING GAP — CSS cannot reach `lower/` (the decisive structural finding)

**Question:** can `stylesheet.bbnf` lower through the existing `lower/` infrastructure (the
`SinkOnlyExpr`/`BackendShape` path G1 uses), or does CSS need a distinct lowering?

**Answer: CSS needs a DISTINCT lowering, and it CANNOT reuse the `SinkOnlyProgram` path — for two
independent, disk-grounded reasons.**

### 2.1 — CSS is structurally REJECTED by `passes::compile`/the CompiledLowering route

`emit_runtime_from_request` (`grammar_provider.rs:98-122`) **skips** `first_unsupported()` ONLY
for the `RequestFacts` emitter (`:110` `if … emitter != RequestFacts { … first_unsupported … }`).
`first_unsupported` (`grammar/src/lib.rs:214-234`) returns an error for these construct kinds:
`TokenDirective`, `PrettyDirective`, `Projection`, `TypedProjection`, `HostCapture`. CSS's
grammar carries ALL of the first four:
- `@token ident ;` (`stylesheet.bbnf:13`) → `TokenDirective` (UNSUPPORTED).
- `@pretty stylesheet block ;` ×8 (`stylesheet.bbnf:53-60`) → `PrettyDirective` (UNSUPPORTED).
- **113 `->` typed-projection arrows** (`properties.bbnf` 99, `selectors.bbnf` 13, `stylesheet.bbnf`
  1; e.g. `colorProps = "color" -> 0u8 | …`) → `Projection`/`TypedProjection` (UNSUPPORTED).

So CSS is on the `RequestFacts` path **precisely because** `passes::compile` →
`lower::lower_to_rust` would FAIL on these constructs. **CSS never builds a `BackendIr`, never
builds a `SinkOnlyProgram`, never reaches `lower/sink_only.rs`.** A naive "route CSS through
`emit_compiled`" (the SPEC's Candidate-A-as-stated, `rB-css-lowering.md:75-76`) is BLOCKED at
`passes::compile` until those construct supports land — which is out of G2's scope and would be
a much larger change.

### 2.2 — Even if it compiled, `SinkOnlyExpr` has NO node for a balanced-delimiter scan

`SinkOnlyExpr` (`lower/sink_only.rs:68-96`) models
`Entry/Seq/Alt/RepeatLoop/OptionalBranch/ByteLiteral/RegexProgram/CallRule/SpanMark/TapeEmit/
DirectBuild/ValueProject/Return`. There is no "balanced-component scan" shape; `lower_program`
(`:122-168`) returns `None` unless `direct_shapes` is non-empty (the JSON `DirectBuild` sink
shape), which CSS has none of. The CSS recognizer's delimiter STOP-set is an EMERGENT property of
the `stylesheet`/`ruleBlock`/`declaration` rule shapes, not a literal in any one `BackendRule`.

### 2.3 — What BackendShape does CSS cost-model to?

Moot under §2.1 — CSS never reaches the cost model (it never builds the BackendIr the cost model
runs over). The 5 `BackendShape`s (`lower/mod.rs:18-26`,
`EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage`) are JSON-path-only today. The CSS
recognizer is an `OffsetTape`-substrate consumer behaviorally (it pushes offsets into the same
`Tape`/`TapeBuilder`), but it reaches that substrate via the hand-written `CssFullParser`, NOT
via a lowered `BackendShape` renderer. **The balanced-scan recognizer is a GENUINELY DISTINCT
EMIT POLICY**, not any existing `SinkOnlyExpr` shape.

### 2.4 — Resolution: a parallel `lower/css_scan.rs` over the RAW grammar AST

G2 must add a NEW lowering that walks the **raw parsed grammar** (`grammar::parse_grammar` /
the import-resolved rule set), NOT the BackendIr. This is the SPEC's `lower/css_scan.rs` producing
a `CssScanProgram` — the CSS analog of `SinkOnlyProgram`, but built from the grammar AST + the
`RuntimeSourceFacts` frontend closure (which the request ALREADY carries:
`grammar::parse_runtime_source_facts`, `grammar_provider.rs:108`), because that closure has the
imports/sources/layout but NOT the rule tree. The arg-derivation needs the actual rule
ALTERNATIVES (which literals open/close/terminate), so it parses the grammar sources
(`request.sources`, already on the request) into the rule AST and extracts the structural
alphabet from the rule literals. The emitter `css_scan_direct::render(program)` (sibling of
`json_sink_direct::render`) then emits the recognizer + lazy projection as `push_str` blocks
parameterized by those facts. This is the same template-keyed-by-grammar-facts discipline G1
landed (`json_sink_direct/mod.rs:30-44`), NOT a generic IR tree-walk.

**This is the make-or-break and the largest single G2 effort** (the structural-alphabet-derivation
pass over the raw grammar AST). The mitigation IS Candidate B: derive only the ARG byte-sets (a
small, tractable extraction from rule literals) and keep the scan ALGORITHM in the named
primitive; then (b) MUST prove the args vary under `.bbnf` mutation.

---

## 3. THE `css_balanced_component_scan` §6 PRIMITIVE design

### 3.1 — The per-primitive (a)-(d) contract

**(a) grammar-INVOKED.** The emitted `generated.rs` calls `css_balanced_component_scan(pos,
STOP_SET, …)` by name; the STOP_SET byte-array constants are derived by `lower/css_scan.rs` from
`stylesheet.bbnf`'s block/declaration/at-rule literal shapes — the `{`/`}` from `ruleBlock`
(`:16`), the `;` from the declaration `";"` terminators (`properties.bbnf:161+`) and
`atRuleBody` (`:33`), the `:` from the declaration `":"` split (`properties.bbnf:161+`). NOT a
literal in the emitter source.

**(b) mutate-falsifier (the single largest paper-close surface, R-A0-3).** Mutate the invoking
rule's structural-byte shape in `stylesheet.bbnf`/`properties.bbnf` → the emitted delimiter
byte-array constant must CHANGE. Concrete falsifier (analog of G1's "drop `bool` → `b't'`/`b'f'`
arms vanish"): change `ruleBlock = "{" >> blockContent << "}"` to a different delimiter, or drop
the `";"` from a declaration → the emitted `STOP_SET = b";{}"` constant must lose/change the
corresponding byte. An arg keyed only off a decorative parameter FAILS (b). Then REVERT and
re-regen → byte-exact restore.

**(c) blob-free.** `CSS_GENERATED_RS` grep == 0; the scan body lives in the shared
`runtime_scan`/`runtime_simd` runtime surface (alongside the existing
`runtime_simd::count_top_level_commas`/`find_css_significant`), NOT a `r#"..."#` splice in
codegen. `verbatim_blob_present == false`.

**(d) PROFILE-PROVEN-NARROW-LEAF.** The primitive shell covers the single 94.1% scalar-scan hot
leaf (`find_component_delim` + `consume_balanced_at`); its shell LOC ≤ the profiled extent
(`g2_balanced_scan_primitive_loc <= g2_balanced_scan_profiled_leaf_extent`). The two hot fns are
~24 + ~21 = ~45 LOC in the current body (`:1000-1056`); the primitive must not exceed that. The
drivers (`parse_block`/`parse_declaration` ≤4.2%) + the lazy projection
(`CssNode`/`CssRule`/`classify`) are fact-keyed-emitted AROUND it (Candidate A), NOT absorbed —
the god-kernel REJECT.

### 3.2 — NEUTRALITY-PROOF obligation (CH6/s6/C4) — the FORCED CSS-scoped name

**Verdict: the recognizer SHELL is CSS-EXERCISED-ONLY → the FORCED CSS-scoped name
`css_balanced_component_scan` is correct; a NEUTRAL name with zero structurally-compatible non-CSS
caller would be REJECTED.** This is settled and disk-grounded (V9/CH6, re-confirmed this pass):

- The CSS shell `consume_balanced_at` (`generated.rs` current `:1036-1056`) returns offsets and
  **emits NOTHING** (no `sink.*`, no `push_leaf`, no `.emit`) — a pure byte-SKIP recognizer.
- The two offered non-CSS dischargers are PARSE-WITH-EMIT descents, structurally incompatible:
  JSON `{}`/`[]` (`json/generated.rs` `b'{' => parse_object_direct(…, sink)` / `b'[' =>
  parse_array_direct(…, sink)`) carries `sink` (emits); Sheets `paren_expr = "(" , expression ?w ,
  ")"` (`google-sheets.bbnf:137`) descends into `expression` (`:163`, parse-with-build).
- Therefore "invoke the SAME primitive from a non-CSS grammar" is **structurally UNREACHABLE**;
  the demotion to `css_balanced_component_scan` is FORCED, not contingent.
- **The split:** the INNER alphabet-scan sub-kernel (`bbnf-simd`'s `byte_class_from_eq_set_64` /
  `find_ascii_set_member64`, already neutral caller-data, already rides JSON's `scan_structurals`
  checkasm corpus) remains genuinely NEUTRAL; ONLY the balanced-recognizer SHELL is CSS-scoped.
  `g2_balanced_scan_neutrality_discharged == true` via the CSS-scoped RENAME, NOT a fabricated
  cross-grammar caller. (The neutral inner kernel is the G6 retarget seam, §5.)

**Recommendation:** name it `css_balanced_component_scan` from the OUTSET; the neutral inner
kernel stays in `bbnf-simd`/`runtime_simd` as alphabet-data and is the G6 wire point.

---

## 4. FEASIBILITY VERDICT — every CssFullParser structure, derivable or not

For each piece of the hand-written `CssFullParser`, classified (a) walk-derivable, (b) legitimate
§6 named primitive, or (c) forced "grammar-driven banner over hand-written body" L1 REJECT:

| CssFullParser piece | classification | basis |
|---|---|---|
| delimiter STOP-sets (`b";{}"`, `b"{};"`, `b";}"`, `b":{};"`) | **(a) walk-derivable** | emergent from `ruleBlock`/declaration/`atRuleBody` literals — the `lower/css_scan.rs` arg-derivation |
| structural SKIP family `' " / ( ) [ ] {` | **(a) walk-derivable** | from `cssString`/`string` quotes, `@ws` `/*`, selector `()`/`[]`, `ruleBlock` `{}` |
| `AT_RULE_FLAG` branch tag (at-rule vs qualified) | **(a) walk-derivable** | the `BackendRule` branch projection: `atRule` (`:41`) vs `qualifiedRule` (`:19`) |
| driver loop shells (`parse_stylesheet`/`parse_block`/`parse_block_item`/`parse_declaration`/`parse_at_rule`) | **(a) STRUCTURAL+BEHAVIORAL, fact-keyed** | the at-rule/declaration/block dispatch is fact-keyed-emitted (Candidate A `push_str` blocks); structurally derived, behaviorally equal — NOT byte-equal (G1 precedent) |
| **the scan loop + mutual recursion** (`find_component_delim`/`consume_balanced_at`) | **(b) §6 named primitive** | the irreducible balanced-scan ALGORITHM — `css_balanced_component_scan`, grammar-parameterized by the STOP/SKIP args; blob-free; arg-mutation-fires; (d) narrow |
| string/comment/escape skip machines (`consume_string_at`/`consume_comment_at`/`consume_escape_at`) | **(b) absorbed in the primitive** | sub-machines of the same scan kernel; ≤2% each; live in the primitive shell, not re-emitted |
| lazy projection (`CssNode`/`CssRule`/`CssDeclaration`/`CssTypedValue::classify`) | **(a) fact-keyed emit** | derived from at-rule/declaration/value-class grammar facts (Candidate A); the `classify` value-head byte-classes map to the `-> u8` projection facts; structural+behavioral |
| `count_top_level_commas` (selector count) | already neutral primitive | calls `crate::runtime_simd::count_top_level_commas` (the existing shared neutral kernel) — UNCHANGED |
| error contract (`css_full_error` positions) | (b) in primitive | the scan's own error semantics; the primitive owns them; oracle parity proves correctness |

**No piece is a forced (c) L1 REJECT — PROVIDED the gate is the B⊃A hybrid (structural+behavioral
for drivers/projection, blob-free+arg-mutation for the named primitive), not byte-for-byte.** The
ONLY thing that would force a (c) is demanding the emitted recognizer be BYTE-IDENTICAL to the
current `CssFullParser` — which is the exact over-strict gate the G1 AMENDMENT already relaxed for
the iterative parse-only courier, and which here would be even more clearly impossible (the
grammar is recursive, the recognizer is flat).

### 4.1 — Is G2 achievable as-specified, or does it need an amendment?

**Achievable WITH a documented amendment.** SPEC §5 already specifies the B⊃A hybrid and the
named-primitive (a)-(d) gate, and §5 G2.2#1 already softens the LOC figure ("cohort-carried, NOT
gate-keyed on the exact figure; the binding gate is `verbatim_blob_present == false` + the
mutation test"). What is NOT yet explicit in §5 is the **G1-analog split-equivalence ratification**:
that the drivers + lazy projection are held to STRUCTURAL + BEHAVIORAL equivalence (9-field oracle
parity + same-run >SOTA + arg-mutation-fires + blob-free), NOT byte-equivalence vs the retired
`CssFullParser`. The G2.2 exit gate as written (conjunct 1 `verbatim_blob_present == false` +
conjunct 3 oracle parity + conjunct 4 >SOTA) ALREADY implies this — there is NO byte-equivalence
conjunct in G2.2, unlike G3's conjunct 6. **So G2 is achievable as-specified; the amendment is a
RATIFICATION that makes the G1-precedent split explicit for the CSS drivers/projection, mirroring
`waves/g1/AMENDMENT.md`.** Recommended amendment text in §6.

### 4.2 — The hardest >SOTA-preservation challenge

The same-run `track1_rich/lightningcss > 1.0×` gate (S-P1 directional antecedents: bootstrap
2.190, tailwindcss 3.375, mcw 1.658, animate 2.101) is preserved by the named primitive
preserving the FLAT scan SHAPE the profile attributes the win to. The genuine risk is NOT the
scan — it is the **fact-keyed driver/projection emit accidentally inflating the per-byte work**:
if `css_scan_direct::render` emits the drivers as per-rule recursive descent (the Candidate C
combinator shape) instead of the flat dispatch loop, it regresses >SOTA categorically. The gate
discharge: the emitted drivers MUST be the flat `parse_block` loop calling
`css_balanced_component_scan`, NOT a `qualifiedRule→selectorList→complexSelector→…` tree walk.
The binding measurement is the same-run ratio captured at G2 ENTRY (pre-G2 tree-checkout) AND
post-G2 build in ONE quiet run (`css_canon_bench`, cold, corpus-in-timer,
`track1_rich_over_lcss_ratio_pre_g2`), so a uniform host-load depression cancels. The S-P1
absolutes are DIRECTIONAL, not the floor; the gate is `ratio > 1.0× ∧ no same-run regression vs
pre-G2`, passing requires ≥1 REGULAR corpus (animate OR bootstrap) crossing > 1.0× (tailwind
below 1.0× is an honest residual, not blocking, provided a regular corpus holds).

---

## 5. IMPLEMENTATION STEP PLAN (concrete edit sequence + per-step verification)

**Owner paths (new):** `crates/codegen/src/lower/css_scan.rs` (arg-derivation),
`crates/codegen/src/css_scan_direct/` (directory module, the emitter — mirroring
`json_sink_direct/`), the `css_balanced_component_scan` primitive shell in
`crates/runtime/src/runtime_scan.rs` (new) or extending `runtime_simd.rs`.
**Owner paths (edited):** `runtime_generator.rs` (`emit_request_facts` `:74-101` routes CSS body
through `css_scan_direct::render` instead of `normalize(CSS_GENERATED_RS)`; DELETE the
`CSS_GENERATED_RS` const `:344-1254`), `lib.rs` (wire the CSS lowering), `nonjson_css_l4.rs`
(parity oracle unchanged — the gate), `css_canon_bench.rs` (the same-run pre/post capture).
**Carried from P3:** the per-companion gate/report/lane validators flagged in
`P3-collapse-ledger.md` §"Consumers NOT re-pointed" (`report.rs`, `gate.rs`,
`lock14_baseline.rs`, `codegen/lib.rs:304 CSS_PROFILE_IDS`) are G2's to de-overfit (P3 deferred
them to "the G2 owner").

**Step G2.a — Capture the pre-G2 >SOTA baseline (BEFORE any code change).** Build the pre-G2
tree, run `css_canon_bench` cold N≥50 corpus-in-timer, record `track1_rich_over_lcss_ratio_pre_g2`
per corpus. Verify: the same-run harness produces both pre-G2 and (later) post-G2 in ONE quiet
run (the regression falsifier fires at G2 exit; the pre-G2 figure is NOT re-derivable post-G2).

**Step G2.b — Author `lower/css_scan.rs` (the arg-derivation pass, the make-or-break).** Parse the
grammar sources (already on `request.sources`) into the rule AST; derive the `CssScanProgram`:
entry rule (`stylesheet`), the STOP-set byte-arrays per call context (at-rule `b";{}"`,
qualified/block `b"{};"`, declaration `b";}"`, colon-find `b":{};"`), the SKIP family `' " / ( )
[ ] {`, the at-rule branch-tag set (`mediaRule|keyframesRule|genericAtRule`), the comment/string
skip flags, the `->`-projection value-class map for `classify`. Verify: unit test the derived
byte-sets equal the current hand-written delimiter args; the arg-mutation test fires (mutate
`ruleBlock`/`";"` → emitted constants change).

**Step G2.c — Land the `css_balanced_component_scan` primitive (blob-free, shared surface).** Move
the scan loop + recursion + skip machines into `runtime_scan.rs`/`runtime_simd.rs` as a
grammar-parameterized fn taking the STOP/SKIP byte-set args. The inner inert-run skip calls the
existing neutral `find_css_significant`/`byte_class_from_eq_set_64` kernel (the G6 seam). Verify:
`g2_balanced_scan_primitive_loc <= g2_balanced_scan_profiled_leaf_extent` ((d) god-kernel check);
`verbatim_blob_present == false`; CSS_GENERATED_RS grep == 0.

**Step G2.d — Author `css_scan_direct/render` (the fact-keyed emitter, mirroring json_sink_direct).**
Emit the driver shells + lazy projection as `push_str` blocks parameterized by `CssScanProgram`,
calling `css_balanced_component_scan` for the scan. The drivers stay the FLAT dispatch loop (NOT
per-rule recursive descent — the >SOTA guard). Verify: distinct output (CSS = tape-push + lazy
CSSOM, JSON = sink-dispatch); `g2_css_rich_projection_not_flattened == true` (CssRule/CssDeclaration
rich API intact, G4 co-gate not foreclosed).

**Step G2.e — Delete the courier + regen-wire.** Replace `("generated.rs", normalize(CSS_GENERATED_RS))`
at `runtime_generator.rs:89` with `("generated.rs", css_scan_direct::render(&program)?)`; DELETE
`CSS_GENERATED_RS` (`:344-1254`); keep `CSS_MOD_RS`/`CSS_PARSER_RS`/`CSS_SINK_RS` (support idioms,
or fact-key them too if trivial). Regen the single P3-collapsed `css_l4_declaration_values`. Verify:
`cargo xtask regen --check` clean; `g2_css_replica_singular == true`; `runtime_target_rows_collapsed
== true`.

**Step G2.f — Discharge the exit gate (gate-before-speed ordering).**
1. **Oracle parity FIRST** (`g2_cssparser_oracle_parity`): `assert_rich_strict_equality` EXACT
   9-field, 4/4 corpora (bootstrap/tailwindcss/mcw/animate). An incomplete structural-alphabet
   derivation diverges here → parity REJECT (do NOT hand-patch to match — that collapses to a blob).
2. **Then >SOTA** (`g2_sota_ratio_held`): same-run `track1_rich/lightningcss > 1.0×` per corpus AND
   no same-run regression vs `track1_rich_over_lcss_ratio_pre_g2`; ≥1 regular corpus crosses > 1.0×;
   `corpus_in_timer == true`.
3. **Arg-mutation falsifier** (`g2_balanced_scan_arg_mutation_fires`): mutate → args change → revert
   → byte-exact restore.
4. **Neutrality** (`g2_balanced_scan_neutrality_discharged`): true via the CSS-scoped name.
5. **(a)-(d) all green** (`g2_balanced_scan_primitive_abcd_pass`); **JSON 51/51 maintained** (G1
   untouched).

**Telemetry emitted + consumed by `gate-json` (SPEC §5 G2.5):** `g2_css_generated_grammar_derived`,
`g2_balanced_scan_primitive_abcd_pass`, `g2_balanced_scan_primitive_loc`,
`g2_balanced_scan_profiled_leaf_extent`, `g2_balanced_scan_arg_mutation_fires`,
`g2_balanced_scan_neutrality_discharged`, `g2_cssparser_oracle_parity`, `g2_sota_ratio_held`,
`g2_sota_ratio_directional_antecedent`, `corpus_in_timer`, `g2_css_rich_projection_not_flattened`,
`g2_css_replica_singular`, `verbatim_blob_present`, `runtime_target_rows_collapsed`.

**Revert plan:** revert `lower/css_scan.rs` + `css_scan_direct/` + primitive-shell + regenerated
output together; restore the `CSS_GENERATED_RS` courier; record the G2 REDRESS naming the failed
gate (if arg-derivation is the failure, record the structural-alphabet-derivation gap — do NOT
paper-close with a hand-patched blob). **G2 REJECTION BLOCKS G3, G4, G6, PROVE.**

---

## 6. RECOMMENDED SPEC AMENDMENT (the G1-analog ratification)

> **G2 Gate Amendment — split derivation-equivalence (the CSS B⊃A analog of the ratified G1
> AMENDMENT).** The `CssFullParser` is a FLAT balanced-delimiter recognizer; `stylesheet.bbnf`
> describes a RICH RECURSIVE CSSOM. Reproducing the flat scan BYTE-EXACT from a grammar walk is
> impossible (it would force the combinator-shaped recursive descent = lightningcss = >SOTA
> regress) and is NOT required. Split the G2 derivation gate:
> - **HARD (the named primitive):** `css_balanced_component_scan` is blob-free
>   (`verbatim_blob_present == false`, `CSS_GENERATED_RS` grep == 0), passes the arg-mutation
>   falsifier (mutate `stylesheet.bbnf` structural-byte shape → emitted byte-set constants change),
>   and (d) narrow (shell LOC ≤ profiled 94.1% extent).
> - **STRUCTURAL + BEHAVIORAL (the drivers + lazy projection):** the `parse_block`/`parse_declaration`
>   shells + `CssNode`/`CssRule`/`CssDeclaration`/`classify` are grammar-fact-keyed `push_str`
>   blocks (Candidate A), held to 9-field cssparser oracle parity (EXACT, 4/4 corpora) +
>   same-run `track1_rich/lightningcss > 1.0×` (no regression vs pre-G2) + `.bbnf`-mutation
>   derivation + regen --check clean — NOT byte-equivalence vs the retired `CssFullParser`.
>
> Additionally ratify the LOWERING-GAP resolution: CSS lowers via a NEW `lower/css_scan.rs`
> grammar-AST analysis (NOT the BackendIr `SinkOnlyProgram` path — CSS is structurally rejected by
> `passes::compile`'s `first_unsupported` on its `@token`/`@pretty`/`->`-projection constructs).
> This does NOT relax the un-fork (G3) or the grammar-derivation; it relaxes ONLY the over-strict
> byte-equivalence on the architecturally-distinct flat recognizer, exactly as G1's AMENDMENT
> relaxed it for the iterative parse-only courier.

---

## 7. Carries / open items for the implementer

- **The arg-derivation pass over the RAW grammar AST is the largest piece and the most likely
  REDRESS site** (SPEC G2.6 already authorizes a documented larger cap "if the arg-derivation pass
  under-delivers"). Mitigation: derive ONLY the ARG byte-sets (small, tractable), keep the
  ALGORITHM in the primitive; prove (b) the args vary.
- **CSS does NOT reach `lower::lower_to_rust`** — do not attempt to route CSS through
  `emit_compiled`/`SinkOnlyProgram` (blocked by `first_unsupported`); author the parallel
  `lower/css_scan.rs` over `request.sources` parsed to the rule AST.
- **The G1 codegen test `css_l4_generated_runtimes_reproducible_from_request`** currently FAILS at
  HEAD (a stale on-disk CSS runtime vs the generator, a pre-G2 condition noted in
  `waves/g1/G1-LEDGER.md` §6). G2's regen fixes it; it is a G2 entry-state artifact, not a new
  failure.
- **P3-deferred per-companion gate cleanup** (`report.rs`/`gate.rs`/`lock14_baseline.rs`/
  `codegen/lib.rs:304`) is G2's to de-overfit per the P3 ledger.
- **The G6 seam is set:** `css_balanced_component_scan` is the ONE NEON-retarget call site; the
  inner inert-run skip calls the already-neutral `find_css_significant`/`byte_class_from_eq_set_64`.
  Do not re-emit vector code per grammar.
