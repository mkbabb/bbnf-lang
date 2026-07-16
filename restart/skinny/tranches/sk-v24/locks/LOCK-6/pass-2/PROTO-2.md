# PROTO-2 — 6B honest-scope decider (LOCK-6 / pass-2, probe 2)

**Family:** 6B — SPEC-GENERATED-SKELETON (the isomorphism plane, rust/ts split).
**Verdict class:** RAN_MEASURED.
**HEAD measured on:** `7cdbff4ef411154dd9bf4b80731c0616b1579d22` (base-pin). My worktree opened at `d2b32f6e3`, which is an **ancestor** of the pin (behind, not stale-divergent); fast-forwarded with `git merge --ff-only 7cdbff4ef` before any measurement (`merge-base --is-ancestor 7cdbff4ef HEAD` → true after). **No wrong-artifact risk.**
**loadavg:** 26.88 → 9.48 (1-min, before→after). Behavioral / LOC probe, no perf claim; load recorded for provenance only.

---

## Q — the two questions

6B was crowned to govern the largest surface with the least evidence. Two things to settle in a live bbnf worktree:

1. **Anchor (STEP 1):** does ANY seam DERIVE a layout fact (flag-bit / decode-class assignment) FROM the grammar, or is every such fact hand-authored policy? Does `MaterializationClass` yield a bit ASSIGNMENT or only a class?
2. **The number (STEP 2):** on the schema face that provably exists, gen-LOC (emitted) vs hand-LOC (what a human would author) for JSON + ≥1 more grammar. `≤1.0` across ≥2 grammars = generation earns its cost; `>1.0` = null hypothesis R3 stands.

---

## STEP 1 — the anchor. Verdict: **YES (partial), with a named seam**

### The cited flag-bit anchor is confirmed PHANTOM on the live surface

- `STRING_NEEDS_DECODE` / `GRAMMAR_BIT0` / `OffsetFlags`: **0 hits tree-wide under `crates/`** (the live bbnf surface). Grep confirmed.
- The only occurrences live under **`skinny/`** (parse-that, READ-ONLY): `skinny/crates/runtime/src/grammars/json/config.rs` holds `const STRING_NEEDS_DECODE: u8 = OffsetFlags::GRAMMAR_BIT0`, set at runtime by a **hand-coded, JSON-specific** predicate `if span.needs_decode() { state.patch_flags(...) }` (`skinny/.../json/generated.rs:97`). The file is marked `@generated` but the flag-bit *semantics* (which bit, when to set it) are authored per-grammar in the codegen template, not derived from BBNF grammar structure.
- So the specific **flag-BIT** the brief cited is (a) off the live surface entirely and (b) hand-authored policy where it does exist.

### But a real grammar→layout-fact DERIVATION exists — two live seams

**Seam 1 — `classify_materialization`** (`crates/ir/src/passes/materialization/classify.rs`).
Walks every rule's IR tree bottom-up and assigns each `NodeId` a `MaterializationClass` ∈ {`MustTape`, `TapeSpanOnly`, `TransparentElide`} — a monotone 3-tier lattice (`lattice.rs`). The tier is DERIVED FROM the grammar: structural shape (leaf/Seq/Alt/Repeat/Map…) **gated by the monotone e-graph facts lattice** (`is_fixed_shape ∧ elision_safe ∧ closure_free ∧ all_descendants_elidable`, computed by a fixed-point over `Ref` boundaries in the same file) and refined by the CSP pass (`csp_strategy/mod.rs`). It is written to the `ir.materialization` sidecar and CONSUMED to drive concrete emission:
  - `pipeline.rs:473` — `TransparentElide` ⇒ `CallStrategy::InlineBody` (no function, inlined at call site).
  - `serialize.rs:36` — `≠ TransparentElide` ⇒ rule pushes a tape record + gets a `__dispatch_serialize` variant arm.
  - `payload/layout.rs:316` — gates payload-layout admission.

**Seam 2 — `compute_payload_layouts`** (`crates/ir/src/passes/payload/layout.rs`).
Derives per-rule **byte-offset slot layouts** (e.g. `[F64 @ 0, U8 @ 8]`) from the projected `TypeDesc`. This is a genuine grammar→byte-layout derivation.

### The precise answer to "bit ASSIGNMENT or only a class"

`MaterializationClass` yields **ONLY A CLASS** — a 3-variant tape-record-shape *tier* (selects `push_compound` vs `push_leaf(Span)` vs no-record). It is a **decode-class assignment** (grammar-derived), **not a raw flag-bit**; no bit in a flags byte is derived from it. The runtime flag-bits (`variant_idx` in the flags byte; the skinny `GRAMMAR_BIT0` decode flag) are stamped by hand-authored per-grammar codegen, not by the class.

**Anchor verdict = YES** by the brief's own definition ("a flag-bit / **decode-class assignment**"): the decode-class/tier IS derived from the grammar — seam `classify_materialization` (+ `compute_payload_layouts` for byte-offsets). **BUT** the finer flag-BIT layer (`STRING_NEEDS_DECODE` and its `GRAMMAR_BIT0` family) is hand-authored policy and off the live surface. → **Face I is PARTIALLY generated:** record-shape tier + payload byte-offsets are GENERATED; the fixed decode flag-bit constants are LINTED. This is exactly the split FAMILY-SPECS §5 proposed (line 376) — now measured, not asserted.

---

## STEP 2 — the number. Verdict: generation **earns its cost** on the operative baseline

### What actually ran

The "extension" the brief specified — teach the Emitter to emit a TS interface for the JSON cell from the live schema descriptor — **already exists and is live**: `TsEmitter::emit_type_definitions_impl` (`crates/core/src/backend/ts/emitter/grammar.rs:103`) emits, from `ir.types` (the projected-`TypeDesc` schema descriptor) via `type_desc_to_ts` (`ts/projection.rs`), a discriminated union `type <Enum> = | { tag: "<rule>"; value: <TsType> } | …` — one tagged variant per non-transparent rule — plus `type <Named> = unknown` host-type aliases. So the parsimonious probe **measures the existing generation** rather than duplicating it. The only new code is a throwaway measurement rig.

Added `crates/core/tests/backend_ts_schema_dump.rs`: emits the TS module for JSON / Google Sheets / CSS-L4, dumps to a scratch dir, and runs the **same `tsc --noEmit --strict --target es2020 --lib es2020,dom` harness** that `backend_ts_typecheck.rs` uses.

**Run:** `cargo test -p bbnf --test backend_ts_schema_dump -- --nocapture` (package is `bbnf`; the brief's `bbnf-core` is a mislabel).

```
GRAMMAR json:          full_ts_lines=267   tsc_strict_ok=true
GRAMMAR google_sheets: full_ts_lines=1132  tsc_strict_ok=true
GRAMMAR css_l4:        full_ts_lines=16201 tsc_strict_ok=true
test result: ok. 1 passed; 0 failed;  finished in 1.78s
```

All three emitted modules **type-check clean under `tsc --strict`.** Generation is real and correct, not SPEC.

### gen-LOC — the emitted schema face (declarative lines: the named aliases + the discriminated union, excluding comment/blank decoration and the shared `Span`/`ParserState` runtime interfaces)

| grammar | gen-LOC (schema face) | union variants | = grammar rule count |
|---|---|---|---|
| json | **9** | 7 | scales with rules |
| google_sheets | **37** | 36 | scales with rules |
| css_l4 | **184** | 182 | scales with rules |

The schema face is **≈ one line per non-transparent grammar rule** — it SCALES with grammar size, it is NOT a fixed ~15-constant surface.

### hand-LOC — two baselines (both authored + measured; faithful one typechecked)

**Baseline A — FAITHFUL** (`*.hand-faithful.ts`): the exact tagged type a human must author to consume the emitted `parse()` output. `parse()` returns the rule-tagged union, so any faithful consumer type IS that union.

| grammar | gen | hand (faithful) | **ratio** |
|---|---|---|---|
| json | 9 | **9** (tsc-verified OK) | **1.00** |
| google_sheets | 37 | 37 (≡ by construction) | **1.00** |
| css_l4 | 184 | 184 (≡ by construction) | **1.00** |

**Baseline B — IDIOMATIC** (`*.hand-idiomatic.ts`): a human-modeled domain AST (JSON recursive value; Sheets AST that collapses the 5 precedence-ladder rules to one `binary` node and the opaque-`Span` leaves to a handful). Not a faithful mirror of `parse()`.

| grammar | gen | hand (idiomatic) | **ratio** |
|---|---|---|---|
| json | 9 | 7 | **1.29** |
| google_sheets | 37 | 12 | **3.08** |

### Adjudication — which baseline is operative

The emitted union **is `parse()`'s return type.** To consume the parser at all, the TS side MUST carry this exact shape; the idiomatic type is a *different artifact* that does not match `parse()` output and would need a hand-written adapter. For the **isomorphism plane** (one grammar → isomorphic rust type ∥ ts type), the **faithful** union is the required artifact, and the faithful ratio is operative:

- **gen/hand (faithful) = 1.00 across 3 grammars → ≤1.0 → generation earns its cost.**
- The verbosity Baseline B exposes (gen>hand) is a **faithfulness cost that hand-authoring shares equally** — a human writing the faithful consumer type writes the SAME 9/37/184 lines (json faithful = 9 = gen, tsc-proven). It is **not** a generation cost.
- **R3's "linter is cheaper" premise is falsified for the schema face:** the face is not ~15 fixed constants — it scales with grammar (9→37→184 ≈ rule count). A linter cannot own a per-grammar, grammar-shaped surface.
- **Amortization:** the schema-face generator body is fixed (~90–160 decl-LOC: `type_desc_to_ts` 41 + `emit_type_definitions_impl` 49 + `collect_named_types`) and produces json(9)+sheets(37)+css(184)+… for free, rust/ts isomorphic **by construction** (both backends project the same `ir.types`). Cumulative faithful hand-authoring across just these 3 grammars = 230 lines, re-derived on every grammar edit and authored twice (rust+ts) — the exact drift LOCK-6 exists to prevent.

---

## The numbers (summary)

| metric | json | sheets | css_l4 |
|---|---|---|---|
| emitted full TS module lines | 267 | 1132 | 16201 |
| **gen-LOC (schema face)** | **9** | **37** | **184** |
| `tsc --strict` | OK | OK | OK |
| hand-LOC faithful → **ratio** | 9 → **1.00** | 37 → **1.00** | 184 → **1.00** |
| hand-LOC idiomatic → ratio | 7 → 1.29 | 12 → 3.08 | (≫1.0) |

---

## Verdict line

**(a) Anchor = YES** — a real grammar→layout-fact derivation exists: **`classify_materialization`** derives a per-node `MaterializationClass` decode-class/tier (record-shape tier: `push_compound`/`push_leaf(Span)`/elide) from grammar shape + the monotone e-graph facts lattice + CSP refinement; **`compute_payload_layouts`** derives per-rule byte-offset slot layouts. **Caveat:** `MaterializationClass` yields only a CLASS (a tier), not a raw flag-bit; the cited `STRING_NEEDS_DECODE`/`GRAMMAR_BIT0` flag-bit is hand-authored per-grammar policy and lives ONLY in read-only `skinny/`, absent from the live `crates/` surface. → **Face I is PARTIALLY generated:** schema face + record-shape tier + payload byte-layout are GENERATED (6B); the fixed decode flag-bit constants are LINTED (6A). The §5 partition line (spec line 376) is CONFIRMED by measurement.

**(b) gen-LOC/hand-LOC = 1.00 (faithful baseline) across json + sheets + css_l4 (3 grammars) → ≤1.0 → generation earns its cost.** The idiomatic baseline gives >1.0 (json 1.29, sheets 3.08), but that measures the parser-faithfulness tax, which hand-authoring pays equally — not a generation cost. **R3's null hypothesis does NOT stand on the schema face:** the face scales with grammar (9→37→184 ≈ rule count), so it is generated-or-hand-authored per grammar, never lint-owned; and the isomorphism plane specifically requires the faithful mirror, whose ratio is 1.0.

**Both outcomes redraw §5 as FAMILY-SPECS proposed:** 6B governs {schema face (GENERATED, ratio 1.0, tsc-clean), classify/materialization tier (GENERATED, real derivation), payload byte-layout (GENERATED)}; the ~fixed decode-flag constants (`GRAMMAR_BIT0` family) are LINTED, not generated.

---

## Provenance / caveats

- Probe **RAN** (RAN_MEASURED), not SPEC_ONLY. `cargo test … backend_ts_schema_dump` passed; all three emitted modules pass `tsc --noEmit --strict`.
- The faithful JSON hand baseline was independently authored AND `tsc --strict`-verified (`FAITHFUL-HAND-JSON tsc: OK`); sheets/css faithful ratios = 1.0 by the identity "faithful consumer type ≡ `parse()` return type ≡ emitted union."
- The Emitter "extension" the brief requested was found **already live** (`emit_type_definitions_impl`); the parsimonious probe measured the existing generation. Only new code = the throwaway measurement test (`backend_ts_schema_dump.rs`, see `PROTO-2.patch`).
- The brief's cited codegen seams (`DirectSchemaSet`, `grammar_profile.rs`, `json_templates/config.rs`, `JsonNodeKind::at_cursor`) confirmed **not on the live `crates/` surface** — `json_templates/config.rs` survives only in read-only `skinny/`.
- parse-that untouched (READ-ONLY respected); no x86; single cargo per target (sole agent in this worktree).
- Package is `bbnf` (`crates/core/Cargo.toml`), not `bbnf-core`.
