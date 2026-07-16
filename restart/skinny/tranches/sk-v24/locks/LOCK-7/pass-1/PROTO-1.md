# LOCK-7 PROTO-1 — Close the byte-421 build-projection wall; hold the fixpoint

Family 7B TOTAL-SELF-HOST (Plane A). Probe 1. Worktree: `wf_63419230-36d-32`.
Date 2026-07-16. loadavg at run ≈ 8.9 (2-day-up host, 1 user).

## QUESTION

Is closing leaf-projection lowering (`-> Span`, `-> i64/f64`) a **BOUNDED** augment
that unblocks the *builder* self-emit (CompiledLowering) while holding 3× byte-identity —
or does it **CASCADE** into recursive-with-state construction (arena/ExprId / a general
emitter) that forces the 7C host-fn floor?

PASS = CompiledLowering emit returns Ok with 0 BBNF-UNSUPPORTED-PROJECTION AND 3/3
byte-identical AND the emitted parser reparses the triple.
Verdict rule: N = distinct projection/construction kinds needing new lowering.
N≤5 AND none needs `&mut` cursor/arena/ExprId state ⇒ BOUNDED. N>5 OR any needs mutable
arena/ExprId ⇒ CASCADE (INC-2 falls to 7C floor>0).

## METHOD (what actually ran)

The `regen-bbnf-probe` xtask arm named in the brief does **not** exist in-tree
(`xtask/src/main.rs` has `regen-json`, `regen-css`, `regen-real-typed` only), and
`FnDescriptor::{SpanCapture,NumberConvert}` do **not** exist anywhere in the codebase —
they are the *proposed* target abstraction, not extant. So rather than stand up a whole
regen arm (which would emit JSON templates regardless — see Wall 3), I drove the exact
functions the CompiledLowering arm calls, directly, over the real corpus.

Corpus: `grammar/bbnf/{types,expressions,bbnf}.bbnf`, `@import` lines stripped and the three
files concatenated (imports only bring rule names into scope; no closure/host/`@{}`/`[]`/`{}`/`ε`
meta-feature is *used* by these files — those appear only as string literals inside the
`term`/`closure`/`host_directive` rule *definitions*). 132 lines inlined.

Probe: `skinny/crates/codegen/examples/bbnf_probe.rs` (added; clean, read-only). It runs, in
order, the precise call chain of `RuntimeEmitterKind::CompiledLowering`
(`runtime_generator.rs:23 → codegen::emit_from_source`):

1. `grammar::parse_grammar` — the hand-seed builder frontend.
2. `passes::compile` — frontend IR → BackendIr.
3. `codegen::emit_from_source` — full builder emit (lower + `json_sink_direct` render + `emit_compiled`).
0. `grammar::parse_runtime_source_facts` → `first_unsupported()` — the recognizer facts gate
   (the `BBNF-UNSUPPORTED-PROJECTION` raise at `grammar/src/lib.rs:220`).

Command: `cargo run --profile ax-iter -p codegen --example bbnf_probe -- <inlined.bbnf>`.

## WHAT RAN — the numbers

**Run A (tree unmodified):**
```
STAGE 1 parse_grammar : ERR BBNF-PARSE "unexpected token `-`" offset=417
STAGE 0 facts gate    : first_unsupported code=BBNF-UNSUPPORTED-PROJECTION offset=417
```
Both walls hit the **same** construct at the same byte: the `->` of `int_lit = /…/ -> i64`
(the inlined analogue of `identifier … -> Span` @421). The hand seed's `parse_seq` breaks
only on `; ) | EOF` and `parse_atom` accepts only `" / ( ident` — so `->` after a regex
leaf is `unexpected token '-'`. Wall confirmed, RUN-backed.

**Run B (recognize-only cascade-peel applied — `cascade-peel.patch`):** I relaxed the hand
seed to *skip* the four constructs the JSON-subset seed lacks (drop `-> <ident>`, treat `,`
as concatenation, swallow `?w`, accept every `@directive`) — recognize-only, no IR node built
— to expose the *next* wall:
```
STAGE 1 parse_grammar : OK  rules=53 exprs=347
STAGE 2 passes::compile: OK  backend rules=53
STAGE 3 emit_from_source: ERR Lowering("sink-only renderer missing Number RegexProgram in BIR")
```

Reading of Run B: the **recursion is not the blocker** — once the leaf tokens are relaxed,
the deeply self-referential bbnf grammar (`rhs→alternation→concatenation→binary_factor→
mapped_factor→factor→term→(rhs)`, 53 rules / 347 exprs) parses and lowers to a 53-rule
BackendIr cleanly. The wall is the **emitter**: `json_sink_direct/mod.rs:34-45` *hard-requires*
the BIR contain the JSON tri-span set {String, Number, Whitespace} **and** DirectBuild shapes
**and** literal recognizers **and** an entry rule matching a DirectBuild shape. The bbnf BIR
does not fit the JSON DirectBuild mold, so the renderer rejects it. And `emit_compiled`
(`runtime_generator.rs:29-74`) emits `json_templates/{parser,value,view,visitor}.rs` verbatim —
a JSON parser — for *any* grammar; only `generated.rs` is grammar-derived. The CompiledLowering
path is a JSON-specialized emitter, not a general one.

## N and the arena test

Distinct projection/construction kinds that need new lowering before CompiledLowering can
return Ok on the bbnf corpus (each empirically forced by Run A/Run B, not speculative):

1. `-> Span` leaf projection (regex leaf **and** parenthesised group — `identifier`, `literal`, `regex`, comments, `string_lit`)
2. `-> i64` leaf projection (`int_lit`)
3. `-> f64` leaf projection (`float_lit`)
4. comma `,` concatenation → `Seq` (hand seed lacks it entirely; used pervasively)
5. `?w` whitespace/layout modifier (explicitly rejected by the seed)
6. non-`@import` directive class (`@pretty`; frontend `ExprKind` has no directive/annotation node the seed will build)
7. general sink-only render for a non-JSON BIR — the `json_sink_direct` tri-span/DirectBuild guard (Run B STAGE 3 wall)
8. general `emit_compiled` beyond the JSON-hardcoded `parser/value/view/visitor` templates

**N = 6 at the frontend/parse+facts layer alone (items 1–6), before the emitter (7–8). N > 5.**

Mutable arena/ExprId test — **fails in two independent places**:
- Frontend `ir::ExprKind` (lib.rs:211-238) has **no** projection/Map node — only
  Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation. Representing `/regex/ -> i64` requires
  a **new arena node** built via `GrammarIr::add_expr` wrapping a child `ExprId` — i.e. new
  ExprId-arena construction (plus its nullability/validation/lowering arms).
- The emitter generalization (items 7–8) is a **recursive tree-walking BackendExpr→Rust
  codegen** — recursive-with-state construction by definition. The current path is a JSON
  template substitution, not a walker.

## VERDICT

**CASCADE.** N = 6 > 5 **and** the augment requires new ExprId-arena construction (frontend
projection node) **and** a general recursive emitter (the STAGE-3 JSON-specialization wall).
Closing leaf projections is *necessary but far from sufficient*: it is the first of six
parse-layer kinds, and even with all six relaxed the builder dies in the JSON-only emitter.
The dominant blocker is not the projection at all — it is that CompiledLowering emits a JSON
parser for every grammar. The PASS condition is not met (emit never returns Ok for bbnf; 3×
byte-identity is unreachable because there is no Ok emit to run thrice).

**INC-2 falls to the 7C floor>0.** Builder self-emit of bbnf forces a general BackendExpr
emitter (and, for the RECOGNIZES→BUILDS gap, the 7C host-fn/general-construction class), not
a bounded leaf-projection augment. 7A build-fixpoint / 7B Plane A / 7D strong-oracle are NOT
jointly unblocked by the bounded route.

Nuance for the planners: the *narrow* reading — "are the projections **themselves** leaf or
recursive?" — is genuinely leaf. `-> Span` is a span capture; `-> i64/f64` is span→number; none
builds runtime arena/ExprId state. If a future tranche builds the general emitter first, the
projection lowering that rides on top of it is small (≈3 target kinds, no runtime arena). The
cascade is in the *surrounding* self-host scaffolding (parse-layer breadth + JSON-only emitter),
not in the projection semantics.

## Deliverables
- `PROTO-1.md` (this file)
- `bbnf_probe.rs` — the diagnostic probe (clean addition, `skinny/crates/codegen/examples/`)
- `cascade-peel.patch` — the throwaway recognize-only hand-seed relaxation used for Run B
  (reverted from the worktree; the seed is left sane)
