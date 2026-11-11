# CRIT-P7 — the TOML end-to-end probe verified against git+code truth

Verdict: the prototype's central architectural claim is INVERTED against the code it claims to test.
One pillar (JSON-tuned leaf classification) is real and verified; the exhaustive break-list and the
TW6 pricing rest on a phantom pre-un-fork architecture that was deleted before the base under test.
The item's underlying QUESTION is directionally answered NO with a genuine mechanism, but P7 as the
certifying INSTRUMENT for SPEC §9 is compromised and must be re-derived. Convergence: 40.

## What is verified TRUE

- **BREAK 3 is accurate, verbatim.** `classify()` at `skinny/crates/bbnf-regex/src/lib.rs:92-105`
  decides `RegexKind` by three JSON literal spellings — `pattern == r"[ \t\n\r]*"`,
  `starts_with('"')`, `starts_with("-?(0|[1-9]") && contains(r"\d")… && contains("[eE]")`. TOML's
  `integer`/`float`/date leaves match none, so they fall to `RegexKind::Unknown`. This is real, current,
  and is the substance behind the Lock-14 concern.
- **BREAK 2 is real** (line numbers wrong). `validate()` is at
  `skinny/crates/codegen/src/json_sink_direct/mod.rs:125`, requiring
  `SinkOnlySpanKind::{String, Number, Whitespace}` (mod.rs:142-144) — the prototype cited mod.rs:34-45,
  which is `render()`, not `validate()`. The census requirement holds; the citation does not.
- **The unsupported-projection error code exists** at `skinny/crates/grammar/src/lib.rs:219-221`
  (`Projection | TypedProjection => "BBNF-UNSUPPORTED-PROJECTION"`).
- The high-level VERDICT — TOML does not emit a correct parser end-to-end; the wall is a generator
  defect in the emitter back-half, not a grammar-authoring defect — is directionally correct and is
  supported by the two verified findings above.

## What is verified FALSE / INVERTED (the disqualifying defects)

1. **"At committed master there is no un-forked general emitter" — FALSE.** At the tested base
   `05302109d` (byte-identical to current HEAD on this file), `emit_from_request`
   (`skinny/crates/codegen/src/runtime_generator.rs:26`) dispatches on `LoweredArtifact { SinkOnly,
   Scan }` (runtime_generator.rs), whose own doc comment reads: "The neutral lowered-artifact sum type
   — the ONE post-lowering shape the un-forked `render` dispatches on. The variant is the OUTPUT of the
   shape-derived lowering pipeline (`lower_for_runtime`), never a grammar tag nor a configured field."
   This is exactly the un-forked, shape-derived emitter the prototype asserts does not exist.
2. **`RuntimeEmitterKind { CompiledLowering, RequestFacts }` does not exist.** `git grep` for
   `RuntimeEmitterKind`, `CompiledLowering`, `RequestFacts`, and `enum RuntimeEmitterKind` at
   `05302109d` and at HEAD returns nothing under `skinny/crates/codegen/`. The prototype's paragraph 2,
   its BREAK 4/5 mechanisms (`emit_compiled` / `emit_request_facts` splicing verbatim JSON/CSS keyed on
   this enum), and its grammar_provider.rs:40-43 citation describe a codebase that does not exist at the
   base it names.
3. **"The un-fork exists only as uncommitted changes… never committed" — FALSE.** `1aba0422e`
   ("general recursive-descent emitter — Sheets via the un-forked generator") and `b5ab2e17c`
   ("un-fork the emitter — DELETE RuntimeEmitterKind, dispatch on the lowered-artifact type") are both
   ANCESTORS of the tested base `05302109d` and of master (`git merge-base --is-ancestor 1aba0422e
   05302109d` → true). The un-fork is committed and live in the very code the prototype ran against.
4. **The xtask reproduction is fabricated.** `RuntimeTarget` (`skinny/xtask/src/regen.rs:6`) has no
   `emitter:` field at `05302109d`; `git grep "emitter" regen.rs` is empty. The prototype's diff
   (`emitter: codegen::RuntimeEmitterKind::CompiledLowering`) references a struct field and an enum that
   do not exist, so the quoted `regen-toml` wiring cannot compile as shown. The concrete error strings
   (`BBNF-UNSUPPORTED-PROJECTION…@1016`, `sink-only renderer missing Number RegexProgram`) are therefore
   not reliably reproducible from the stated base.

## Why this matters for the plan (not merely pedantry)

The single fact P7 was commissioned to establish — "is the emitter un-forked, and does a third grammar
pass through it?" — is answered backwards. The correct, verified finding is stronger and more useful:
**the emitter DISPATCH is already un-forked (shape-derived `LoweredArtifact`), but the `SinkOnly`
renderer is not yet grammar-general — its `classify()` leaf recognizer and its `validate()` span census
are JSON-tuned.** A third grammar lowers to `SinkOnly` and then emits JSON-shaped classification. That
is the precise TW6 residual.

Consequently the prototype's TW6 pricing bullet (a) "delete `RuntimeEmitterKind` and the two verbatim
template families" prescribes ALREADY-COMMITTED work, inflating the TW6 cost estimate with a phantom
un-fork. The real remaining work is narrow and correctly named in the prototype's (b)/(c)/(d): make
`classify()` structural (derive kind from HIR shape), make `validate()`/`render()` census shape-derived
rather than pinned to JSON's three span kinds, and carry `->` through to the emitter. The plan should
inherit (b)/(c)/(d) and DROP (a).

## The spec section (§9) — over-claim check

SPEC §9 is honest and, ironically, more correct than the prototype: it presumes "the un-forked emitter"
exists (true) and keeps the novelty claim narrow ("first GENERATOR to emit grammar-derived block-wide
SIMD structural classification", SPEC-DRAFT-P1.md:295-297). §9 does not over-claim in its prose. The
mild residual optimism is in §11's TW6 close gate ("ten grammars, three serious targets",
SPEC-DRAFT-P1.md:362): landing TOML requires a real `SinkOnly`-renderer generalization (structural
`classify` + shape-derived span census) that §9 under-prices, because §9 leaned on P7's inverted
"it's the un-fork" framing rather than the true "the un-fork is done; the renderer internals are
JSON-tuned" framing. The prototype's own Lock-14 recommendation — widen the gate to key on an emitted
TOML parser round-tripping the `toml` crate, not on directory paths — is sound and survives.

## What a re-pass must produce

Re-run a clean `regen`-of-a-third-grammar against the ACTUAL `05302109d`/`LoweredArtifact` architecture
and report the real code path a `SinkOnly`-routed TOML grammar takes; re-derive the break list keyed on
the true dispatch (SinkOnly vs Scan by `first_unsupported`), retaining only the verified BREAK 2/BREAK 3
pillars; and re-price TW6 as "generalize the `SinkOnly` renderer's leaf classification + span census",
not "un-fork the emitter". Until that lands, P7 cannot certify §9's break inventory.
