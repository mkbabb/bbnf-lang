# PROTO-P6 — HM-core inference spike over `->` maps (JSON + google-sheets)

Instrument, not landed code. Worktree spike; all evidence reproducible from the
diffs below. Spike file: `crates/core/tests/p6_hm_spike.rs` (worktree
`wf_be85d0ab-073-12`).

## The falsifiable question and the answer

**Q.** Do `MapExpr::synth` + a first-order unifier + `HostSigTable` (report 28's
steps 1–3) type JSON and google-sheets end-to-end with **zero `unwrap_or(Span)`
defaults**, with annotations **checking** (not replacing) inference?

**A — SUPPORTS, with two sharp corrections.** A ~189-LOC core (bidirectional
`synth`/`check` + union-find `Unifier` + `HostSigTable`) types every `->` map in
both grammars with **span_default = 0, failed = 0**; all 33 `FnDescriptor::Expr`
descriptors resolve (json 4 checked + 1 synth; gs 7 checked + 21 synth).
Annotations *check* — 11 annotated sites independently synthesize and **unify**
with their annotation, zero conflicts. Both injected errors fail **at the
annotation/param edge** with real messages. The two corrections:

1. **The union-find unifier is *not exercised* by this corpus.** JSON and
   google-sheets contain no `-> f1 -> f2` chain, no generic rule, and no host
   call with typed arguments surviving to the type layer. Every equality check
   reduces to structural `TypeDesc` equality + one `Span→{F64,U32}` coercion +
   the integer-literal variable. The var/occurs-check path is proven correct
   only *synthetically*.
2. **Correct-*width* typing is blocked by lowering, not inference.** The
   `unwrap_or(Span)` lie fires **in production on both grammars** — on 22 real
   `-> Nu8` discriminant maps whose `u8` suffix is destroyed before the type
   layer. `synth` eliminates the Span default at all 22, but recovers `I64`
   (HM numeric default), not `U8`, because the width is already gone. The fix is
   upstream (preserve the suffix), after which `check` confirms `U8`.

Confidence 80%.

## What the corpus actually is (ground truth)

Compiling both grammars through the real frontend (`bbnf::pipeline::compile_grammar`)
and dumping every `FnDescriptor::Expr`:

```
json (6 fns):
  fn[0] IntLit(0)                       return_type=None        <- `null -> 0u8`
  fn[1] BoolLit(true)                   Some(Bool)
  fn[2] BoolLit(false)                  Some(Bool)
  fn[3] Input                           Some(F64)               <- `-> f64` shorthand
  fn[4] NumberConvert{..}                                       (specialized, not Expr)
  fn[5] FnCall(decode_json_string_to_arena, [])  Some(Named "String")
google-sheets (28 fns): Input/F64, Input/Span x4, BoolLit x2,
  IntLit(0..8) return_type=None x9   (error_literal),
  IntLit(0/1) return_type=None x12   (compare/add/mul/unary ops)
```

Two facts dominate:

- **Every user `->` map carries a `return_type` *except* the `Nu8` tags** — and
  those lose their suffix. `value_expr/literals.rs:7,17` (`parse_int_literal` /
  `parse_float_literal`) explicitly discard `_suffix`; the recovery path
  `expression/wrap.rs:589-606` then fails because the mapped leaf's span text is
  the bare digits (`"0"`), so `split_numeric_suffix("0")` returns an empty
  suffix -> `return_type = None` -> `generate.rs:371` `unwrap_or(Span)` types the
  tag as **Span**. Live mistype count: **1 (JSON null) + 21 (google-sheets) = 22**.
- **The host call's argument is dropped too.** `decode_json_string_to_arena(input)`
  lowers to `FnCall{ args: [] }` — same defect class. The `@host` *param* edge is
  therefore unreachable on the corpus; only the return type (annotation/table)
  survives.

So the `unwrap_or(Span)` site is not a dormant hypothetical — it is a live,
quantifiable lie, and the thing that makes it *look* dormant on annotated maps is
exactly the upstream data-loss that also caps synth at `I64`.

## The spike (resurrectable core)

Extended type with unification variables over `TypeDesc`; `IntVar` is the one
variable class the bare-`IntLit` surface forces even absent chains/generics:

```rust
enum Ty { Con(TypeDesc), Var(usize), IntVar(usize) }
```

Union-find `Unifier` (82 code lines): occurs-check + reflexive short-circuit +
integer-class constraint on `IntVar`; `zonk` defaults `IntVar->I64`:

```rust
fn unify(&mut self, a: &Ty, b: &Ty) -> Result<(), String> {
    let (a, b) = (self.resolve(a), self.resolve(b));
    if a == b { return Ok(()); }                    // identical representative
    match (&a, &b) {
        (Ty::Var(i),_)|(Ty::IntVar(i),_) => self.bind(*i, b.clone()),
        (_,Ty::Var(j))|(_,Ty::IntVar(j)) => self.bind(*j, a.clone()),
        (Ty::Con(x), Ty::Con(y)) => self.unify_con(x, y),   // structural, shallow
    }
}
// bind() enforces: is_int[v] => t is an integer Con or another IntVar
```

Bidirectional `synth` (=>, 64 lines) / `check` (<=, 12 lines) over the *closed*
`MapExpr` operator set:

```rust
fn synth(u,e,env) -> Ty {
  IntLit  => u.fresh_int();          FloatLit => F64;   BoolLit => Bool;
  StringLit => Span;                 Input => env.input_ty;
  InputProp{len} => U64; {as_str} => Span;
  FnCall{name,args} => { sig = hosts.get(name)?; arity+unify(arg, param); sig.ret }
  BinOp arith => unify(l,r)->l;  cmp => unify(l,r)->Bool;  logic => (Bool,Bool)->Bool
  UnaryOp Neg => inner;   Not => unify(inner,Bool)->Bool
}
fn check(u,e,expected,env) {
  // `-> f64`/`-> u32` over Span input is a *coercion* (fused scan+convert), admit
  if e==Input && env.input_ty==Span && expected in {F64,U32} { return Ok(()) }
  unify(synth(e), Con(expected))     // annotation CHECKS inference
}
```

The load-bearing replacement for `generate.rs:371/:399`:

```rust
match return_type {
    Some(ann) => check(expr, ann, env),   // was: annotation REPLACES inference
    None      => synth(expr, env),        // was: unwrap_or(Span)
}
```

## Results (all 5 tests pass, cold; loadavg 6.15 at dispatch, ~6 at run; single cargo)

```
json:  fn0 SYNTH->I64  fn1 CHECK->Bool  fn2 CHECK->Bool  fn3 CHECK->F64  fn5 CHECK->Named(String)
       -> checked=4 synth=1 span_default=0 failed=0
gs:    fn0 CHECK->F64  fn1/13/14/15 CHECK->Span  fn2/3 CHECK->Bool
       fn4..12,16..27 SYNTH->I64 (22 tags)      -> checked=7 synth=21 span_default=0 failed=0
TOTAL  checked=11 synth=22   span_default=0   failed=0
```

Negative injections — each a **real** failure at the edge:

```
wrong annotation  `-> true : u8`  -> Err("annotation-edge: inferred Bool vs annotated U8: cannot unify Bool with U8")
host arg mismatch decode(Bool) vs (Span)->String -> Err("@host arg edge: cannot unify Bool with Span")
host ret mismatch decode(input):f64 vs ->String  -> Err("annotation-edge: inferred Named(String) vs F64: cannot unify Named with F64")
```

## Findings that shape the tranche plan

1. **Bidirectional check/synth is load-bearing, not decoration.** Integer literals
   have no width in synth mode (`IntVar` -> `I64`); only `check` pushes the
   annotation width *down* and confirms `U8`. And `-> f64`/`-> u32` are
   *coercions* (Span->scalar), not identities — a naive `unify(synth,ann)` would
   *reject* correct grammars; only check-mode coercion (matching the
   `NumberConvert`/`HexConvert` specializations) admits them. Report 28 sec a2 is
   right that this is the interface discipline; here it is also the *only* way
   `Nu8` widths and `-> f64` survive.

2. **The lacuna is upstream of the type layer.** Three instances of one defect
   class — dropped `u8` suffix, dropped `Nu8` suffix, dropped host-call argument —
   starve inference of data it needs. Step 1 of any type-system tranche must be
   *lowering preserving the projection payload* (suffix into `return_type` or a
   width-carrying `IntLit`; args into `FnCall`). Absent that, `synth` upgrades the
   status quo (Span->I64) but cannot reach the intended `U8`.

3. **`HostSigTable` needs a real source.** These grammars have no `@host fn`
   syntax; the return column is seeded from the call-site `: T` annotation and the
   param column has *nothing* to bind to (args dropped). The `BBNF-HOST-SIGNATURE-
   MISMATCH` unification failure is real (proven synthetically) but **unreachable**
   until (a) `@host fn` declarations exist and (b) lowering keeps the arguments.

4. **LOC — the ~250 estimate is ~25% high for this corpus; corrected to ~190.**
   `synth`+`check` = **76** (report ~80, matches). Union-find `Unifier` = **82**,
   not ~150 — because `TypeDesc` leaves carry no nested variables in this domain,
   so `occurs`/`unify_con` stay shallow. `HostSigTable` = 13; `Ty` enum + `is_int`
   = 18. The ~150 (hence ~250) becomes fair only once `Vec<Var>`/`Option<Var>`,
   `Record`, and `Sum` unification (report 28 sec a4 / sec 5.5, **absent from this
   corpus**) are added — which is also where the union-find finally earns its keep.

## Verdict on the success criteria

- Zero Span defaults at the former `unwrap_or` sites — **YES** (0/33).
- Annotations check, not replace — **YES** (11 checked, 0 conflicts).
- Wrong annotation + host mismatch each a real edge failure — **YES** (3/3 Err).
- LOC sizing — **corrected**: ~190 core for this corpus (unifier 82, not 150),
  ~250 once records/sums/generics land.
- End-to-end at *correct types* — **NO for the 22 `Nu8` tags** (land at `I64`);
  gated on a lowering fix, not on the inference core.
