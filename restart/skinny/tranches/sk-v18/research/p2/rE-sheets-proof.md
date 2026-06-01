# SK-V18 S-P2 — Class R-E: Sheets Third-Grammar Proof (the PROVE negative control)

Research pass. Grounded in the actual tree at cwd `/Users/mkbabb/Programming/bbnf-lang`
(repo root one level above `skinny/`). Every claim cites `path:line`. No cargo run. No code
written. This digest is the binding R-E input S-P3 sequences into the PROVE wave.

## §0 — Frame: PROVE is a NEGATIVE control, and today it FAILS by construction

The generalization is REAL only if a THIRD, structurally-distinct grammar
(`grammar/google-sheets/google-sheets.bbnf`, 185 lines) emits a working parser THROUGH the
un-forked generator — not JSON, not CSS, zero hand-authored runtime Rust. Per addendum 2 +
SYNTHESIS §337/§383, the close-gate is: Sheets `generated.rs` md5-distinct from JSON and CSS;
`grep -c 'const.*_RS.*r#'` for any Sheets blob == 0; `sheets_grammar_shape == pratt-operator`;
the Sheets value type instantiates the G4 shared trait. The fallback (SYNTHESIS §0.5/§383) is
binding: **if Sheets cannot be emitted via the generator ONLY, the generalization is NOT real —
surface honestly, do NOT stub-prove, do NOT hand-write a `_GENERATED_RS` Sheets block.**

**The negative control currently FAILS three ways (disk-verified this pass):**

1. **Sheets fails closed at the gate today.** `codegen/src/lib.rs:1049-1071`
   (`w5a_sheets_bbnf_fail_closed_through_runtime_contract`) asserts BOTH `google_sheets` and
   `bbnf` return `Err(Lowering("frontend closure missing import closure"))`. Sheets has NO
   `@import` (confirmed: `grep '@import' google-sheets.bbnf` → NONE), so the
   `RuntimeFrontendRequirements{import_closure:true}` of the RequestFacts contract
   (`grammar_provider.rs:263-265`) rejects it. The witness is a 25-LOC stub
   (`sheets_witness/event_grammar_witness.rs` — a test-only `SheetsEventGrammar` + 3 `FactId`
   consts), NO `.bbnf` consumed, NO `RuntimeTarget` row in any xtask table.

2. **There is NO grammar-agnostic emitter to route Sheets through.** Both emit paths are
   grammar-specialized verbatim couriers:
   - CSS (`RuntimeEmitterKind::RequestFacts`) emits `normalize(CSS_GENERATED_RS)` — a fixed
     `&str` const at `runtime_generator.rs:701` (~910 LOC), IDENTICAL regardless of input
     grammar (`lib.rs:1099` asserts the two CSS requests produce byte-equal `generated.rs`).
     This is exactly the R4 witness: all 7 css_l4 `generated.rs` share md5
     `b654562ccff46ed62dd48e9ace325830` (re-verified this pass, `uniq -c` == 7).
   - JSON (`RuntimeEmitterKind::CompiledLowering`) renders `json_sink_direct::render(sink_only)`
     (`runtime_generator.rs:37`). This LOOKS grammar-derived, but `render_entry`/
     `render_value_dispatch`/`render_container_rules`/`render_string_rule`/`render_number_rules`
     (`json_sink_direct.rs:96,124,251,326,367`) each take ONLY `out: &mut String` and
     `push_str` verbatim JSON parser bodies dispatching on `b'{'`/`b'['`/`b'"'`/digit into
     `parse_object_direct`/`parse_array_direct`/`JsonSink`. The `SinkOnlyProgram` is consumed
     only by `render_header` (a comment, `:68`) and `validate` (`:18`). The grammar STRUCTURE
     does not drive the body. **`emit_compiled` (`runtime_generator.rs:29-74`) hardcodes
     `include_str!("json_templates/...")` + `JSON_PARSE_ONLY_*` + `JsonSink`.** Routing Sheets
     through `emit_compiled` emits a JSON parser, not a Sheets parser.

   So the un-fork (G3) is not a flag-deletion — it is the AUTHORING of the first real
   grammar-agnostic body emitter. Sheets is the proof that emitter exists; it cannot proceed
   until G3 produces grammar-DERIVED bodies (sequencing §4).

3. **The phantom `<G>` (G4) is unresolved.** `tape/mod.rs:175`
   `ValueRef<K = AnyKind, G: EventGrammar = AnyGrammar>` — `G` is never bound to a production
   type outside `#[cfg(test)]` `_proof_compiles::<SheetsEventGrammar>`. The Sheets value type
   must instantiate-or-the-trait-deletes-`G`, and must NOT LCD-flatten JSON's rich nav.

This is the honest baseline: PROVE is the wave where "does it generalize?" is answered. The
candidates below are about WHAT Sheets must exercise (which novel constructs make the litmus
non-hollow) and HOW its `generated.rs` is held md5-distinct + branch-free.

## §1 — Construct census: what Sheets has that JSON+CSS lack (the generality surface)

Disk census (`grep -oE` over the three `.bbnf`, this pass):

| construct | JSON | CSS stylesheet | Sheets | novelty for the generator |
|---|---:|---:|---:|---|
| `->` projections | 5 | 3 | **32** | 6.4×/10.7× density; many typed leaves |
| `Nu8` tagged-alt (`Nu8 -> Ku8`) | 0 | 3 (stylesheet.bbnf only; **295** across the CSS L4 import closure) | **21** (`error_literal` 9, `compare_op` 6, `add_op`/`mul_op`/`unary_prefix` 2ea) | SHARED — CSS uses it 14× MORE; the generator must already handle Nu8 at scale to emit CSS, so NOT litmus-novel |
| `-> true`/`-> false` bool-alt | 2 (`bool`) | 0 | 2 (`boolean`, case-insensitive regex form) | regex-alt → bool (CSS lacks) |
| recursion depth / precedence tower | flat (value↺object/array) | flat (rule↺block) | **7-level left-assoc tower** `comparison→concat→add→mul→exp→unary→postfix→primary` + cyclic `paren_expr→expression` | THE Pratt stress — no JSON/CSS rule exercises an operator-precedence cascade |
| `<<` repeat-with-separator | 4 | 3 | 4 (`func_args`, `let_binding`, `array_row`, `let_args`) | shared (already in JSON/CSS) |
| `?w` ws-modifier | 0 (uses `>>`/`<<` discard) | via `?w` | **23** | heavy inline-ws |
| `@import` directive | yes | yes | **NONE** | Sheets needs the import-closure requirement RELAXED |
| `@ws`/`@token` directive | n/a | yes | **NONE** | Sheets has no top-level ws/token directive |
| host-capture (`-> crate::f(input):T`) | 0 (uses `decode_json_string_to_arena`) | 0 | **0 live** (2 in `// TODO AU.6.7` comments only) | none live — Sheets is host-free |
| string/number span regex `-> Span`/`-> f64` | yes | yes | yes (`: Span` ×4, `number -> f64`) | shared |

**The genuinely-novel, litmus-load-bearing construct is ONE:** the 7-level
**operator-precedence tower** with cyclic `paren_expr → expression` recursion (the Pratt shape —
`sheets_grammar_shape == pratt-operator`, SYNTHESIS §574 gate). The **`Nu8`-tagged alternation
family** (21 small-enum discriminants — `error_literal`, the operator rules) is NOT novel: CSS L4
uses `-> Nu8u8` **295×** across its import closure vs Sheets' 21×, so the generator must already
handle it at scale to emit CSS — it is a SHARED construct, demoted from the litmus. Everything
else (`Nu8`, `<<`, span regex, `?w`) JSON or CSS already exercise, so exercising them alone would
be a "third JSON" hollow litmus (SYNTHESIS §614 REJECT predicate
`sheets_grammar_shape ∈ {flat-stream,tree}`).

## §2 — CANDIDATE classes (what Sheets must exercise to prove generality)

### CANDIDATE R-E-1 — FULL Sheets grammar, formula corpus, full precedence + value-API (MAXIMAL)
Adopt `google-sheets.bbnf` whole; emit the complete 7-level tower + `Nu8` family + cell/range
refs + array literals + LET/LAMBDA through the un-forked generator; bench a cold real-formula
corpus; instantiate the G4 trait over the Sheets value type.
- **Proves:** the generator lowers operator-precedence recursion (the Pratt stress), dense
  tagged-alt discriminants, AND a non-JSON/non-CSS value tree — the maximal honest litmus.
- **Trade-off:** highest authoring + regression surface; the precedence tower is the most
  likely place the generator's recursion lowering breaks (§3 risk). Largest LOC. A Sheets
  formula corpus must be sourced cold/per-parse (no broadcast, addendum 5 / Lock pre-block).
- **Risk:** MED-HIGH — if the generator cannot lower the left-assoc tower without hand-shaping,
  this is the §6 honest-finding surface (a NAMED, grammar-invoked, parameterized precedence
  primitive with a checkasm/scalar ref — NOT a `_RS` blob, NOT a paper-close).

### CANDIDATE R-E-2 — Precedence-tower CORE only (the minimal honest Pratt stress) — RECOMMENDED
Emit the precedence/expression core that NO JSON/CSS rule exercises:
`formula → comparison_expr → … → primary`, the `Nu8` operator rules (`compare_op`/`add_op`/
`mul_op`/`unary_prefix`), `number`/`string`/`boolean`/`error_literal` leaves, `paren_expr`
(the cyclic recursion), and `func_call` (one `<<`-separated arg list). DEFER the leaf-typed
`cell_ref`/`range_ref` aggregates and `LET`/`LAMBDA` (which the grammar ITSELF leaves as
`-> input : Span` / TODO AU.6.7 — see `google-sheets.bbnf:62,74` — i.e. they are NOT yet typed
in the grammar, so emitting them adds no generality the grammar expresses).
- **Proves:** EXACTLY the two novel constructs (§1): the operator-precedence cascade
  (`pratt-operator` shape) + the dense `Nu8`-tagged-alt family. Honest by construction — it
  exercises what JSON+CSS structurally cannot, and nothing contrived.
- **Trade-off:** smaller corpus/LOC than R-E-1; still hits the single hardest lowering
  (recursive precedence). The `cell_ref`/`range` deferral is GROUNDED in the grammar's own
  TODOs (not a dodge) — those rules currently project a raw `Span`, identical shape to JSON's
  `string -> input : Span`, so they prove no NEW generality.
- **Risk:** MED — the precedence tower is still the stress, but the surface is bounded.

### CANDIDATE R-E-3 — Tagged-alt + leaf SUBSET, precedence FLATTENED (MINIMAL / hollow-risk)
Emit only the `Nu8`-tagged-alt family + leaves, replacing the precedence tower with a flat
`expression = primary (op primary)*` single-level rule.
- **Proves:** dense small-enum discriminants from byte/regex literals — a real construct CSS
  only touches 3× and JSON 0×.
- **Trade-off / Risk: REJECT as the litmus.** Flattening the tower to one level erases the
  `pratt-operator` shape — `sheets_grammar_shape` degrades to `flat-stream`, which SYNTHESIS
  §614 names a REJECT (third-JSON hollowing). This is the contrived-construct failure mode the
  task warns against. Listed only to mark the floor: anything that drops the precedence
  recursion is NOT a general-generator proof.

## §3 — RECOMMENDATION: R-E-2 (precedence-tower core), the minimal HONEST proof

R-E-2 is the recommended candidate. Rationale, all grounded:

- It exercises the SOLE construct JSON+CSS structurally lack (§1): the 7-level
  left-associative operator-precedence cascade (`add_expr = mul_expr ?w , (add_op ?w , mul_expr
  ?w) *`, `google-sheets.bbnf:109`) with cyclic `paren_expr → expression` recursion (`:137`).
  (The `Nu8`-tagged-alt family is SHARED — CSS uses it 295× across its import closure — not part
  of the litmus.) This satisfies `sheets_grammar_shape ==
  pratt-operator` (SYNTHESIS §574) — non-hollow by construction.
- It is HONEST: the deferred `cell_ref`/`range_end`/`LET`/`LAMBDA` aggregates are NOT a dodge —
  the grammar ITSELF leaves them as raw `-> input : Span` pending AU.6.7's aggregate codegen
  (`google-sheets.bbnf:62,74` explicit TODOs). Emitting a raw-Span cell_ref proves the SAME
  generality JSON's `string -> input : Span` already does; including it would pad LOC without
  adding a novel lowering. R-E-2 emits what the grammar TYPES, defers what the grammar itself
  defers — no contrivance either direction.
- It minimizes the regression + authoring surface relative to R-E-1 while preserving the single
  hardest lowering (recursive precedence) — the place the generator most plausibly cannot
  generalize, which is precisely where the §6 honest-finding must be able to fire.

**The Pratt note (the one expressivity check):** the tower is NOT classic left-recursion the
generator must rewrite — `google-sheets.bbnf` already encodes precedence as right-iterated EBNF
(`A = B (op B)*`), which lowers to the generator's EXISTING vocabulary: `Seq` + `RepeatLoop` +
`Alt{Dispatch}` + `CallRule` (the `SinkOnlyExpr` set, `lower/sink_only.rs:69-96`). So the
precedence shape does NOT require a new Pratt primitive in the IR — it requires the un-forked
G3 emitter to render recursive `CallRule` chains + `RepeatLoop` bodies from grammar structure
(which the JSON courier does NOT do today, §0.2). The stress is on G3's GENERALITY (does the
body come from the grammar?), not on a missing IR construct. IF a real left-recursion or a
construct outside `{Seq,Alt,RepeatLoop,Optional,ByteLiteral,RegexProgram,CallRule,DirectBuild}`
is reached, §6 fires: a named, `.bbnf`-invoked, parameterized precedence primitive with a
scalar/checkasm reference — never a silent blob.

## §4 — How the Sheets `generated.rs` is held md5-DISTINCT + BRANCH-FREE (addendum 2 co-gate)

Addendum 2 is a CONJUNCTION (`SYNTHESIS-AUDIT-OVERFIT.md:59`), md5-distinct is
NECESSARY-NOT-SUFFICIENT. The four co-gates, each as an executable check over the live tree:

1. **md5-distinct** (`generated_md5_distinct == true`): `md5 -q` over
   `grammars/{json,sheets,css_l4_*}/generated.rs` → all distinct (`uniq -d` empty). Trivially
   true IF the body is grammar-derived (Sheets' precedence tower + `Nu8` family produce a body
   structurally unlike JSON's `{`/`[` dispatch and CSS's delimiter scanner). A repeated md5
   would mean a courier was reused — REJECT.

2. **branch-count == 0** (`generator_grammar_branch_count == 0`): the canonical Lock-14
   arm-census `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|(GoogleSheets|Sheets)\w*\s*=>
   |Bbnf\w*\s*=>' skinny/crates/codegen/src skinny/xtask/src` → 0. (`GoogleSheets`
   un-abbreviated; `Sheets\w*` does NOT match `GoogleSheets =>`, SYNTHESIS §32.) The un-forked
   G3 emitter must render Sheets from the SAME `render(program)` path as JSON+CSS — no
   `match grammar { GoogleSheets => emit_sheets() }` arm.

3. **type-count == 0** (`generator_grammar_type_count == 0`): `rg 'JsonParser|CssL4Parser|
   GoogleSheetsParser|BbnfBootstrap' skinny/crates/codegen/src skinny/xtask/src` → 0. The
   emitter must NOT re-emit a grammar-named parser/`EventGrammar` type literal. NOTE the
   witness-emission coupling (SYNTHESIS-AUDIT §2.1 item 2): if G3 emits a Sheets
   `EventGrammar` literal into the generated runtime, `FORBIDDEN_GENERIC_TOKENS` must carry
   `EventGrammar`/`*EventGrammar` so it is caught at the emit site — Sheets is the FIRST grammar
   that would actually exercise this (CSS today emits no EventGrammar; the courier has none).

4. **row-collapse** (`runtime_target_rows_collapsed == true`): the Sheets `RuntimeTarget` row
   (NEW, in `regen_css.rs` or a new `regen_sheets.rs`) must carry a DISTINCT `grammar_name`
   (`"google_sheets"`, not `"css_l4"`/`"json"`) so the per-`grammar_name` config-tuple collapse
   counts a genuine THIRD grammar (`generator_grammar_count == 3`), not a relabeled CSS row.
   Per R16 (`SYNTHESIS-AUDIT §5 fact 5`) the collapse must inline BOTH nested structs
   (`frontend_requirements` AND `output_labels`) — the PREFERRED mechanism is adding
   `PartialEq` to `RuntimeTarget` (one derive line; `regen.rs:5` today carries only
   `Clone,Copy,Debug`; both nested structs already derive `PartialEq,Eq` at
   `grammar_provider.rs:45/91`). Sheets' single row collapses to itself (count==1 per
   `grammar_name`), trivially passing once it is a real distinct-grammar row.

**Blob co-gate** (addendum 1): `grep -c 'const.*_RS.*r#' codegen/src` for any Sheets blob == 0
(SYNTHESIS §337). Sheets has ZERO hand-authored runtime Rust — `sheets_witness/` (25-LOC stub)
either becomes the generated output dir or is deleted; the generated runtime falls out of G3.

**Import-closure relaxation (the one frontend change Sheets forces):** Sheets has no `@import`
(§0.1). The RequestFacts contract's `import_closure: true` requirement (`grammar_provider.rs:263`)
must NOT be a hard gate for a single-file grammar — either Sheets carries its own
`RuntimeFrontendRequirements` with `import_closure: false`, or the requirement is derived from
the grammar facts (present-iff-grammar-has-imports) rather than asserted by the contract.
This is a frontend-requirements data change, NOT a generator branch — it must not become a
`match grammar` arm. (This is also the honest reason `w5a_sheets` fails closed TODAY; PROVE
flips that test from "fails closed: missing import closure" to "emits a working parser.")

## §5 — Why a hand-written shim ⇒ NOT general (the negative-control teeth)

The proof is a negative control: a hand-written Sheets parser, a Sheets-specific emit branch,
a relabeled `_RS` blob, or a flattened-precedence "third JSON" each FALSIFIES generality and is
a REJECT (SYNTHESIS §383/§614). Concretely the litmus FAILS if any of: (a) Sheets `generated.rs`
needs a `const SHEETS_GENERATED_RS` courier (addendum 1, `grep` ≠ 0); (b) G3 routes Sheets via
a `GoogleSheets =>` arm (addendum 2 co-gate 2); (c) `sheets_grammar_shape != pratt-operator`
(precedence flattened, §2 R-E-3); (d) the Sheets value type cannot instantiate the G4 trait
without LCD-flattening JSON's rich nav (addendum 4). Per the fallback, NONE of these is patched
silently: each becomes a §6 honest-finding (a named, `.bbnf`-invoked, parameterized primitive
with a scalar/checkasm reference), or the litmus is declared failed and B1/B2 (G1/G3) iterate
(SYNTHESIS §238, V≤5). PROVE does not paper-close.

## §6 — KEY RISK + PRUNE/SEQUENCING DEPENDENCY

**KEY RISK (MED-HIGH):** the un-forked G3 emitter does not exist yet — BOTH current emit paths
are grammar-specialized verbatim couriers (CSS const `:701`; JSON `json_sink_direct::render`
push_str bodies that ignore `program` structure, §0.2). Sheets is the FIRST grammar whose body
CANNOT be a relabeled JSON/CSS courier (its precedence tower + `Nu8` family are structurally
unlike both), so it is the true test of whether G3 emits grammar-DERIVED bodies. If G3 cannot
render recursive `CallRule`/`RepeatLoop` chains from grammar structure, the precedence tower is
where it breaks first (§3) — the §6 honest-finding surface. Secondary risk: the import-closure
requirement must relax for a single-file grammar without becoming a grammar branch (§4).

**PRUNE/SEQUENCING DEPENDENCY (binding, `SYNTHESIS-AUDIT §5` graph):**
PROVE is the LAST GENERALIZE-cluster wave: `PRUNE(P1–P5) → G1 → G2 → G3 → G4 → G5/G6 → PROVE →
H1`. PROVE **entry-gates on G3** (un-fork) — it emits Sheets THROUGH the un-forked generator, so
**G3 failure BLOCKS PROVE** (sequencing fact 3). Transitively PROVE depends on G1 (JSON
projection) and P3 (the CSS replica collapse + `RuntimeTarget` row-collapse, which the Sheets
row must extend with a distinct `grammar_name`). PROVE also consumes G4 (the Sheets value type
instantiates the shared trait — the phantom-`<G>` resolution). It does NOT depend on G5/G6
(NEON) for correctness, but the Sheets corpus, if benched, must be cold/per-parse (addendum 5,
no broadcast). Net: Sheets is dispatched ONLY after G3 closes; a REDRESSed G3 halts PROVE.
The Sheets adoption itself is +~200 LOC (`.bbnf` referenced not authored; generated runtime
falls out of G3; skinny grammar-root + xtask target +~30), per alphaE B4 budget.
