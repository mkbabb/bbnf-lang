# PROTO-1-7B — LOCK-7 pass-1, probe 1 (family 7B TOTAL-SELF-HOST, Plane A): the self-emit budget + recognition round-trip

**Probe class:** RUNNING (RAN_MEASURED). Measured quantities: a deterministic construct-rejection
**byte offset**, a deterministic generated **LOC count**, and 3×-byte-identical **determinism** —
all reproducible independent of machine load (load avg 9.96/10.02/9.28 at capture, recorded for
the one place a wall time would have appeared; see build-wall below).

Sibling to the existing `PROTO-1.md` (probe t1, family f1). t1 asked whether the **hand front-end**
`grammar::parse_grammar` parses bbnf's surface. This probe asks the **generator** question one
layer down: does `codegen::emit_runtime_from_request` — the JSON/CSS runtime-emit path
(`xtask` `regen-json`/`regen-css` machinery) — **self-emit** a bbnf runtime, at what generated-LOC
and cold-build budget, and does emission even succeed or blow up toward the 21,557-LOC legacy ghost?

---

## Question (verbatim from the brief)

> Does the CURRENT skinny generator self-emit bbnf.bbnf, at what generated-LOC and cold-build
> budget, and does emission even succeed — or blow up toward the 21,557-LOC legacy ghost? This
> number is unmeasured and bounds 7A, 7B, and 7D jointly.

## Method

In this worktree, a throwaway `regen-bbnf-probe` xtask arm mirroring `JSON_TARGET`
(`skinny/xtask/src/main.rs:175`), pointed at the committed self-grammar corpus
`grammar/bbnf/{bbnf,expressions,types}.bbnf` with `entry_rule = "grammar"`, `source_roots =
[grammar/bbnf/bbnf.bbnf]`. **@import is resolved by the production frontend closure** inside
`emit_runtime_from_request` (`grammar::parse_runtime_source_facts` →
`resolve_runtime_import_closure`, `grammar_provider.rs:108`) — never a manual concat; the probe
supplies the three sources by rel-path and the closure links `from "expressions"`/`from "types"`.

Two emitter variants were exercised, because the "current generator" is **two hardcoded
per-grammar templates**, not one general compiler:

1. **CompiledLowering** — the exact `JSON_TARGET` wiring (structural runtime). Runs
   `facts.first_unsupported()` before emitting.
2. **RequestFacts** — the exact `css_l4_declaration_values` wiring (direct-to-struct runtime),
   `first_unsupported()` skipped, `frontend_requirements` relaxed to `none()` so the fact-emitter
   is not gated on typed-projection/`@token` (which bbnf.bbnf *defines as rules* but never *uses*).

Each variant runs emit **3× in-process** (`EmittedSource: Eq`) for determinism; on `Ok`, LOC =
Σ `source.lines().count()` over `emitted.files()`; on `Err`, the exact `CodegenError`.

## What ran — the numbers

```
corpus: grammar/bbnf/bbnf.bbnf (89 LOC) + expressions.bbnf (37) + types.bbnf (7)

--- variant: compiled-lowering (emitter=CompiledLowering) ---   [exact JSON_TARGET mirror]
GATE_A: FAIL (emit did not return Ok)
GATE_C: PASS (3×-identical error = true)
FIRST_UNSUPPORTED: BBNF-UNSUPPORTED-PROJECTION  @ grammar/bbnf/bbnf.bbnf@421
                   (the first `->` — `identifier = /…/ -> Span ;`, line 9 col 40)

--- variant: request-facts (emitter=RequestFacts) ---           [exact css_l4 mirror, relaxed]
GATE_A: OK  (emit returned Ok)
GATE_C: PASS (3× byte-identical = true)
TOTAL_LOC: 998        config.rs 9 · generated.rs 910 · mod.rs 12 · parser.rs 51 · sink.rs 16
RATIO_VS_HAND_2142:   0.466        (LESS than the hand front-end)
RATIO_VS_LEGACY_21557: 0.046       (4.6% — no blow-up toward the ghost)
GATE_B: PASS (998 ≤ 8568 = 4× hand)
```

### The decisive qualifier — the 998-LOC "Ok" is a phantom

`generated.rs` emitted for **bbnf** is **byte-identical** (excluding the one-line generated
header) to the canonical **`css_l4_declaration_values/generated.rs`**:

```
$ diff <(tail +2 bbnf_probe/generated.rs) <(tail +2 css_l4_declaration_values/generated.rs)
  → IDENTICAL_BODY   (910 = 910 LOC; 143 `Css*` refs; mod.rs re-exports CssDocument/CssNodeKind/…;
                       parser.rs doc-comment: "recognize the stylesheet")
```

`runtime_generator.rs:76-94` (`emit_request_facts`) confirms the mechanism: it emits
`normalize(CSS_GENERATED_RS)` / `CSS_MOD_RS` / `CSS_PARSER_RS` / `CSS_SINK_RS` — **fixed CSS
string constants** — and stamps *only* config-level facts (source hash, `IMPORT_COUNT = 3`,
`ENTRY_RULE = "grammar"`, `ROW_ID`) into `config.rs`. **The grammar's productions are never
read for the recognizer body.** The RequestFacts emitter would emit the identical CSS recognizer
for *any* input grammar; the CompiledLowering emitter likewise templates `json_templates/*.rs`.
Neither path is a grammar-generic descent emitter that consumes bbnf's rules.

Consequence: the **recognition round-trip is impossible** with the current generator — the 998-LOC
artifact recognizes CSS declaration values, not bbnf. `parse("identifier = /…/ -> Span ;")` against
it exercises the CSS recognizer. The generated output was deleted, not retained, precisely because
it is a mislabelled CSS copy, not a bbnf runtime.

### Build-wall

**NOT MEASURED — deliberately, and honestly.** The only `Ok` artifact is the CSS runtime, and it
is non-self-contained: `generated.rs` opens with `use crate::tape::{OffsetFlags, Tape, …}` (runtime
crate internals). A cold `cargo build` of it would time the **CSS** runtime wearing a bbnf label —
a number that would *look* like a bbnf self-host build budget while measuring nothing of the kind.
Per the RUN-honest / no-fabrication rule, no such number is reported. A real bbnf build-wall is
unmeasurable until a general emitter exists (it does not).

## Cross-check with sibling t1

Both probes wall at **the identical byte — 421, the first `->`** — through two *different* code
paths, corroborating a single root cause:

| path | mechanism | verdict at byte 421 |
|---|---|---|
| t1: `grammar::parse_grammar` (hand front-end) | `parse_atom`/`parse_postfix` have no `->` production | `BBNF-PARSE: unexpected token '-'` |
| this: `emit_runtime_from_request` (generator) | runtime facts scanner classifies `->` as a projection | `BBNF-UNSUPPORTED-PROJECTION` |

The runtime facts scanner (`scan_runtime_source`) is *more* permissive than `parse_grammar` — it
recognizes `->` as a projection construct — yet still classifies it **unsupported**. So the `->`
map/projection is the universal first wall on **every** current ingestion path, and the sole path
that returns `Ok` does so by *ignoring the grammar entirely*.

## Verdict

**7B Plane A self-emit is ARCHITECTURALLY ABSENT, not one-lowering-rule short.**

- **Gate A (emit Ok):** FAIL on the faithful (CompiledLowering) path — `BBNF-UNSUPPORTED-PROJECTION`
  @ bbnf.bbnf:421. The `Ok` on RequestFacts is a **facade** (byte-identical CSS template); it is not
  a bbnf recognizer, so it does not satisfy "self-emit bbnf" in any grammar-faithful sense.
- **Gate B (LOC ≤ 8568):** the only number available is **998**, but it is the **CSS template's**
  LOC, not a bbnf-derived figure. It PASSES numerically (0.47× hand, 0.046× legacy — *no* blow-up),
  but **it does not bound 7B**: a phantom cannot certify a budget. The genuine bbnf self-emit LOC
  remains **UNMEASURED** because the generator that would produce it does not exist.
- **Gate C (3× byte-identical):** PASS on both variants — the determinism precondition for any
  future fixpoint holds for the generator machinery itself (deterministic error, deterministic
  template).

**First augment-BBNF task named by the faithful path (bounding 7A/7B/7D jointly):** projection
lowering — the `->` map operator (`BBNF-UNSUPPORTED-PROJECTION`, bbnf.bbnf:421), which is *upstream*
of the brief's candidate list (`@{}` host-capture, `|params|` closure). Those come after; `->`
is first. But the deeper finding this probe adds beyond t1: even with `->` parsed, **there is no
grammar-generic emitter to lower it into** — both current emitters are fixed JSON/CSS templates.
The bbnf leg of 7A/7B/7D is BLOCKED-candidate on *building a general descent emitter*, not on
patching a rule into an existing one.

**7B Plane A demotes to SPEC-level for the LOC budget** (Gate B cannot be honestly certified on a
phantom), while this probe itself **RAN and MEASURED** the real quantities that establish the
demotion: the 421 wall, the 998-LOC CSS phantom, and byte-identical determinism.

## Patch

`PROTO-1-7B-selfemit.patch` — the throwaway `regen-bbnf-probe` arm on `skinny/xtask/src/main.rs`
(+117 lines, one file). Generated probe output (the CSS copy) was written then deleted; it is not
part of the patch. Reproduce: `cargo run -p xtask --profile ax-iter -- regen-bbnf-probe`.
