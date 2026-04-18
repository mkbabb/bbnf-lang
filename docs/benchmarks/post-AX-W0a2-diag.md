# AX.W0a.2 — parse() routing diagnosis + scope-reveal halt

## Pre-fix state

`cargo expand` on each bench binary reveals the current `parse()` routing
(saved at `/tmp/ax-w0a2-{json,css,sheets,bbnf}-entry.txt`):

| Grammar | Entry rule | Entry shape | Root-type | `parse()` body call |
|---|---|---|---|---|
| JSON | `value` | transparent (Alt-of-Refs) | Alt | `parse_JsonParser_value(...)` (shape dispatcher) |
| CSS L4 | `stylesheet` | Array | `ruleList ?w` (non-`[...]`) | `dta_run_CssL4Parser(...)` (walker) |
| Sheets | `formula` | Flat | Seq | `dta_run_GoogleSheetsParser(...)` (walker) |
| BBNF | `grammar` | Array | `rule+` (non-`[...]`) | `dta_run_BbnfBootstrap(...)` (walker) |

JSON routes through the shape dispatcher. CSS / Sheets / BBNF fall through to
the walker branch of `emit_grammar_impl`'s `if use_shape_dispatch { ... } else
{ ... }` because `has_shape_dispatcher_entrypoint(ir)` returns `false` for
each.

## Root-cause layers

Diagnosis went three layers deep under contact.

### Layer 1 — predicate over-strict (false-negative, Case A)

`has_shape_dispatcher_entrypoint` (`shapes/mod.rs:331`) iterates every
classified rule in the IR and rejects when any classified rule has a
value-position Ref to an unclassified target, whether or not that rule is
reachable from the entry. The docstring states the intent as "every
value-position Ref **transitively reachable from the entry's classified shape
fns** resolves to a classified rule" — the implementation was broader.

**Fix landed.** Predicate rewritten as a BFS from the entry through classified
Ref targets. Unclassified rules unreachable from the entry no longer block
admission. Code matches docstring intent. Behaviour change under fixed-point
testing: no grammar's admission flips (all 6 non-JSON grammars have genuine
entry-reachable unclassified Refs — see Layer 2).

### Layer 2 — grammars genuinely fail entry-reachable closure

A diagnostic probe (`tests/ax_w0a2_predicate_probe.rs`, subsequently removed)
walked the entry-reachable Ref graph for each grammar. Results:

| Grammar | Entry-reachable unclassified Refs |
|---|---|
| JSON | 0 (Alt-of-Refs branches all classified) |
| CSS L4 | 34 (e.g. `atRuleBody → ruleBlock`, `bgDecl → value`, `compoundSelector → attrSelector`, `declaration → alignDecl`, 20+ `*Decl → value`) |
| Sheets | 1 (`exp_expr → unary_expr`) |
| BBNF | 4 (`host_directive → type_name`, `import_directive → {import_items, import_path}`, `rhs → alternation`) |
| EBNF | 2 (`identifier → letter`, `rule → alternation`) |
| BNF | 2 (`rule → alternation`, `rule → nonterminal`) |

Every non-JSON grammar has a genuine entry-reachable unclassified Ref. The
predicate correctly rejects to prevent the `__value` fallback emission which
would infinite-loop at runtime on non-Alt-rooted grammars (root's `__value`
calls root's shape fn calls root's `__value` → …).

### Layer 3 — Array-shape emitter structurally incompatible with non-`[...]` entries

Even if the predicate were forced to admit CSS / BBNF, the emitted shape fn
body would fail on every input. `parse_array_<grammar>_<rule>` at
`src/backend/rust/emitter/shapes/array.rs:105` begins with:

```rust
if input.get(*p).copied() != Some(b'[') {
    return Err(DtaError::Syntax { ... });
}
```

The Array emitter canonicalises to JSON's `"[" >> (value << ","?)* ?w << "]"`
shape. CSS's `stylesheet = ruleList ?w` is classified as Array (via the
list-rule detector) but has no `[` / `]` delimiters and a completely
different body structure. Routing `parse()` through
`parse_array_CssL4Parser_stylesheet` would produce a syntax-error-on-every-byte-0
failure mode. BBNF's `grammar = rule+` has the same problem.

## Conclusion — scope-reveal halt

The wave spec's W0a.2 premise is that non-Alt-rooted grammars can be routed
through per-shape entry fns by widening `has_shape_dispatcher_entrypoint`. That
premise holds ONLY when:

1. The grammar's entry-reachable Ref graph is closed over classified targets
   (fails for CSS / Sheets / BBNF / EBNF / BNF — 5 of the 7 non-JSON cases).
2. The shape emitter's body correctly handles the entry's structural shape
   (fails for CSS `stylesheet`, BBNF `grammar` — the Array emitter is
   JSON-specific).

Closing W0a.2's routing deliverable requires deeper emitter surgery than the
wave can absorb per the SPEC §Scope-reveal contract. Specifically:

- **Detector work** — classify the 34+4+1+2+2 unclassified entry-reachable
  rules, or prove they admit a Scalar/HRegex fallback. Work crosses
  `crates/ir/src/passes/recognizers/shape_dispatch.rs` + every detector
  module + substantial grammar-IR coverage audit.
- **Array emitter generalisation** — split JSON-`[...]`-canonical Array from
  list-rule Array with arbitrary head/tail. Work crosses
  `crates/core/src/backend/rust/emitter/shapes/array.rs` + likely a new shape
  variant or emitter-branch in `emit_parse_array`.
- **OR, walker-per-rule entry** — give shape emitters a non-`__value` fallback
  for unclassified Refs that routes to a per-rule walker entry. Requires
  emitter changes crossing into `dta_walker/` which the wave spec forbids.

## What this wave lands

Per `docs/instructions/tranche/SPEC.md` §Scope-reveal — halt with diagnosis
over partial-fix. This wave therefore lands:

1. **Predicate narrowing** (`shapes/mod.rs:331` `has_shape_dispatcher_entrypoint`)
   — brings implementation in line with docstring intent. No admission
   outcome changes today; architectural cleanliness improvement with no
   behavioural risk.
2. **Wire-contract test** (`crates/core/tests/gate_predicate_wire_contract.rs`)
   — freezes per-grammar × per-predicate outputs (7 grammars × 3 predicates =
   21 assertions). Any subsequent wave that widens any predicate MUST amend
   this test, per invariant 9.
3. **Bootstrap regen** if the emitter-side narrowing shifts emission (it
   should not — predicate is read-only at shape-emission time and outcomes
   are unchanged, but the bootstrap re-verifies idempotency).

The W0a.2 `parse()` routing deliverable is **halted**. Orchestrator re-plan
required. Suggested sub-waves:
- W0a.2.a — Array-shape emitter generalisation (split JSON-Array from
  list-rule-Array; new `ListArray` shape variant).
- W0a.2.b — Detector coverage for entry-reachable unclassified rules (CSS
  `value`, `ruleBlock`, `attrSelector`, Sheets `unary_expr`, BBNF
  `alternation`, etc.).
- W0a.2.c — Re-route `parse()` once (a) + (b) close the entry-reachable
  graph.

Or, a single wave landing a **walker-per-rule fallback** emission — but that
crosses into `dta_walker/` which is owned by W0b.

## Verification

- Predicate narrowing compiles clean (`cargo check --workspace` zero errors).
- Existing test `aw_v_w5_2_per_ref_routing::admission_rejects_grammars_with_
  unclassified_value_ref_targets` still passes (Sheets still rejects under
  narrowed predicate for correct reasons).
- Wire-contract test encodes frozen outputs.
- Post-change `cargo expand` of CSS / Sheets / BBNF parse() bodies still
  routes through walker — consistent with the halt reasoning.
