# HARDENING-PASS-2-V9.2

V9.2 audit for `target=PASS-2`, cycle `V9.2`, against the lazy-tape substrate amendment proposed at `restart/skinny/audit/LAZY-TAPE-DESIGN.md`. Prior verification `restart/audit/hardening/HARDENING-PASS-2-V9.1.md` closed at AMENDMENT-REQUIRED-NARROW for spelling/citation residues; this report opens a new amendment surface — the dual-mode (Eager | Lazy) tape substrate. Scope: `restart/audit/pass-2-codegen/PASS-2.md` and codegen-adjacent surfaces PASS-2 cites. The hardening lens is V1 lens contract A-K with foci on Lane 1 (Lock 5 + Lock 6 + Lock 14), Lane 3 (BIR alphabet cohesion), Lens I (contrivance), Lens J (host-language leverage), and Lens N (graduation mechanicality).

This report does not amend the target documents.

---

## §1 Target identification

| Item | Value |
|---|---|
| Amendment under audit | `restart/skinny/audit/LAZY-TAPE-DESIGN.md` (846 lines) |
| Target codegen surface | `restart/audit/pass-2-codegen/PASS-2.md` (633 lines) |
| Adjacent V1 surfaces inspected | `restart/ARCHITECTURE.md` §7.2 (lines 900-1008), §7.5 (lines 1096-1162), §8 grammar surface |
| Adjacent skinny surfaces | `restart/skinny/SUBSTRATE.md`, `restart/skinny/COMPILER.md`, `restart/skinny/BENCH.md` (cited; not re-audited here) |
| Locks consulted | `restart/locks/14-LOCKS.md` Locks 1, 5, 6, 9, 13, 14 |
| Prior PASS-2 hardening | `restart/audit/hardening/HARDENING-PASS-2-V9.1.md` |
| Lens set | V1 lens contract A-K, focused on dual-mode admissibility for PASS-2 codegen-side |
| Empirical premise | SK-V2 outcome G; eager-tape ~12.5K Mbps geomean vs sonic-rs ~21K Mbps; `restart/skinny/audit/LAZY-TAPE-DESIGN.md:9-19` |

The design proposes per-grammar `tape_mode ∈ {eager, lazy}` selected from workspace metadata. Lazy mode treats the structural-offset array as the tape, deletes `TapeToken` for JSON, lowers `TapeEmit` to a no-op, and computes node kind from `source[offsets[cursor]]`. Eager mode is preserved verbatim for CSS L4 / BBNF-self / Sheets.

---

## §2 Steelman of the proposal

The empirical premise is hard. Three skinny iterations land outcome G uniformly; three rejected micro-amendments (dispatch-table, 12-byte token, pair-fusion) failed to close the ~1.6× gap; the diagnosis is substrate materialization cost (16-byte writes per offset across 40K-167K offsets per corpus), not codegen or SIMD throughput. The remaining honest move on this evidence is to remove the write-stream: let the structural index BE the tape.

The proposal preserves the load-bearing PASS-2 contracts:

- `ValueRef<'doc, 'input, K>` cursor shape is retained; the field renames `index → cursor` and indexes `offsets` rather than `tokens`. Identity invariant `(TapeId, cursor, kind_at_cursor)` is a pure function of immutable `(tape.source, tape.offsets[cursor])`; identity proof is structurally equivalent to the eager `payload_class_of(tokens[index])` story (LAZY §2.3).
- `DocumentView`, `Visitor`, `PayloadArena`, `ParserState`, `parse(&str)/parse_in/parse_owned` surfaces stay byte-identical at the user API (LAZY §6.3, §10.3).
- The 20-variant BIR alphabet is left untouched; the proposal explicitly says "ARCH §7.2's 20 BIR variants are unchanged. The same `TapeEmit` and `DirectBuild` rows exist; their lowering differs by mode" (LAZY §8.2).
- Lock 5 (BIR-only lowering) survives: the lowerer reads BIR + `tape_mode` metadata; the metadata is a lowering parameter, not a BIR variant addition or a Grammar IR import (LAZY §4.3, §8.2).
- Lock 9 (slice-borrow primary): lifetime discriminant unchanged; `parse(&str)`'s `'doc = 'input = 'a` collapse works identically over the offsets array (LAZY §6.3).
- Lock 14 (full grammar generalization): the JSON discriminator function is generated per grammar, not hard-coded in the runtime crate; the same template generates a different discriminator for CSS-scan if it opts in (LAZY §4.3).
- Falsifiability is sharp and binds against the existing bench gate: `T1 > 14K Mbps` validates, `< 13K Mbps` refutes, bench harness unchanged (LAZY §9.4, §11.2).

This is not a parallel substrate. There is no `enum TapeShape { Lazy, Eager }` runtime polymorphism; the design is explicit that mode is per-grammar-crate monomorphic via cfg/feature gating (LAZY §8.3): "There is no `enum TapeShape { Lazy(...), Eager(...) }` in the runtime — that would BE a parallel substrate." Eager mode persists verbatim for CSS L4, BBNF-self, Sheets, where layout/recovery/payload-bearing tokens require stored classes.

The proposal is admissible in spirit. The audit below scrutinizes whether PASS-2 (as the codegen-quadrant authority) can absorb it without violating its existing contracts.

---

## §3 Lane 1 — Lock-Adherence verification

### 3.1 Lock 1 (Tape is the substrate; no parallel substrate)

Lock 1 verbatim (`restart/locks/14-LOCKS.md:34`): "Tape is the greenfield's contiguous parsed-token-stream-with-payload-arena, unioned with direct-to-struct typed values that borrow into it (`&'i Tape<'i>` + index). [...] Plans that resurrect parallel substrates [...] or implement tape with consumer-later sequencing are faults."

The lazy proposal changes the *content* of the tape — from `Vec<TapeToken>` to `Box<[u32]>` of structural offsets — while preserving the *shape* (a contiguous parsed buffer owned by `Tape<'input>`, borrowed by typed views via `&'doc Tape<'input>` + cursor). The "parsed-token-stream-with-payload-arena" phrase reads as a constraint on the eager-mode shape; the spirit of Lock 1 is the no-parallel-substrate clause, not the literal "token stream" word.

Status against Lock 1:

- **No parallel substrate.** The lazy-mode tape is the only substrate per grammar; eager-mode tape is the only substrate for eager grammars. The two modes do not coexist within one grammar. The runtime never carries `enum TapeShape { Lazy, Eager }` runtime polymorphism. The `ValueRef`, `DocumentView`, `Visitor`, `PayloadArena` surfaces are shared at the trait level (LAZY §4.1, §8.3).
- **No OpenFrame clone.** The lazy mode does not reintroduce frame-clone substrates. `TapeAssembler` is a verifier-route over offsets; rollback is length-truncation as in the eager case (LAZY §2.4).
- **No columnar SoA.** The lazy mode is a single `Box<[u32]>` plus auxiliary candidate arrays (`string_candidates`). The candidate arrays are sonic-rs-analogue parser auxiliaries, not per-field stripes; ARCH §7.2 already admits this shape at lines 1002-1008 ("`SimdScan` has two runtime products. `StructuralIndex` is the exact structural offset stream [...] JSON's parse index may add parser columns when the full parse row recovers the extra cost").
- **No type ambivalence.** Mode is per-grammar-crate monomorphic; within a grammar, exactly one materialization shape exists.

**Lock 1 verdict: HONORED, with one amendment surface required.** Lock 1's literal phrase "parsed-token-stream-with-payload-arena" requires an amendment to admit the lazy-mode shape; this is the §4 LAZY-TAPE-DESIGN proposed amendment. PASS-2's §2 commitment 3 (PASS-2:36) cites Lock 1 verbatim and must absorb the same amendment. The lock's spirit (no parallel substrate; no OpenFrame ladder; no Vec<OpenFrame>::clone) holds untouched.

The amendment text proposed in LAZY-TAPE-DESIGN §4.1 is acceptable in shape but the audit punch list §12 below tightens specific phrases.

### 3.2 Lock 5 (IR + per-backend lower)

Lock 5 verbatim (`restart/locks/14-LOCKS.md:42`): "There is no source-emit-per-backend duplication; there is no trait-based emitter walking grammar directly. The IR is the contract."

The lazy design's lowering rule is the load-bearing Lock 5 question: under `tape_mode = "lazy"`, `TapeEmit` lowers to a no-op (LAZY §7.2). The proposal explicitly says (LAZY §4.3, §8.2):

> "Lock 5 (IR + per-backend lower) survives. BIR remains the contract. Lazy mode adds zero BIR variants; it changes the lowering of `TapeEmit` and `DirectBuild` to no-ops or cursor-cell-writes under `tape_mode = "lazy"`. The Rust lowerer reads `tape_mode` from extracted grammar metadata; this is a lowering parameter, not a BIR addition."

PASS-2's §2 commitment 1 (PASS-2:32) states "Backend IR is the PASS-2 boundary. PASS-1 produces it after parse, validate, type inference, shape mining, e-graph, cost extraction, and lower-to-BIR." The lowerer consumes BIR plus `LowerContext` per ARCH §7.5 (`restart/ARCHITECTURE.md:1144-1153`); `LowerContext` already carries "target triple, generated-code budget cursor, grammar metadata reference, [...] lint-mode toggles." Adding `tape_mode` to `LowerContext` (or to `GrammarMeta` consumed by `emit_artefacts`) is mechanical and does not require BIR alphabet edits.

The audit-critical question: does mode-branching at the lowering site violate "no trait-based emitter walking grammar directly"? It does not. The lowerer reads BIR; the BIR's `TapeEmit` row carries the same payload (tape kind, span/value refs per ARCH §7.2 line 958); the lowerer's branch on `ctx.tape_mode` is a target-conditional emission, structurally identical to the existing branch on `target = "wasm32"` for the future `WasmBackend: Backend` impl. Both are "lowering parameter, not BIR alphabet" decisions.

One concern. The current PASS-2 §2 (PASS-2:71) describes `TapeEmit` as "tape event" lowering — the lazy-mode no-op is a *zero-emit* lowering of `TapeEmit`. This is a sharper distinction than the existing dispatch/speculative `Alt` mode branch, which still emits *something*. Zero-emit lowering of a BIR variant is novel within PASS-2's payload-refiner contract.

The audit accepts this. The BIR contract is "the lowerer consumes BIR variants and produces target source"; a target-conditional empty emission is admissible — the BIR variant is the producer-side semantic claim that *a node event occurred*; the lowerer is free to choose whether the target requires materialization of that event. Sonic-rs's structural-index *is* a materialization of every node event, just stored differently. The PASS-2 punch list §12 below adds a verbatim row to the §2 payload-refiner table making this explicit.

**Lock 5 verdict: HONORED, with mode-discriminator parameter required at LowerContext/GrammarMeta.** No BIR alphabet edit. The lowerer's mode-branch is target-conditional emission, not grammar-walking; the import-deny gate (`BBNF-CODEGEN-IMPORT-DENY` at PASS-2:270-279) still passes — the lowerer reads BIR + grammar metadata, never Grammar IR.

### 3.3 Lock 6 (committed source generation)

Lock 6 verbatim (`restart/locks/14-LOCKS.md:44`): "xtask emits committed source artefacts. No proc-macro façade. [...] Build is fast incremental because expansion is not at compile time."

The lazy design changes the *content* of the generated source under `tape_mode = "lazy"` but does not change its *shape*: emitted files remain `runtime/src/grammars/<g>/{generated.rs, parser.rs, host.rs, view.rs, value.rs, visitor.rs}`. The generated body for `generated.rs` shrinks (LAZY §7.4: -200 LOC) because `state.tape.emit(...)` lines vanish; `view.rs` grows (+100 LOC) because the depth-tracked walkers move there. Net JSON generated LOC drops ~100.

PASS-2's §6 generated LOC budget table (PASS-2:423-435) sets per-grammar caps. The lazy-mode shift on JSON would push generated LOC *down* from 3,500 → ~3,400; the PASS-2 max of 3,570 (+2%) is not exceeded. Eager-mode grammars (bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, math) keep their current generated LOC. No budget violation.

The xtask regen-equality gate (PASS-2:344-356 split, PASS-2:464 wall-time budget) requires byte-identical regeneration. Under lazy mode, the gate must verify that the *new* generated `generated.rs` (with `TapeEmit` lowering to no-op) is byte-identical to the committed snapshot. This is a straight regen pass; the gate runs unchanged.

The BIR snapshot gate (PASS-2:268 + `cargo xtask bbnf bir --all --check`) is more subtle. The BIR snapshot is the *upstream* contract; it should not differ between eager and lazy modes for the same grammar, because BIR alphabet is unchanged. The mode is `LowerContext` data, not BIR data. The snapshot under lazy-JSON should be byte-identical to the eager-JSON snapshot of the same grammar source — and the proposal says exactly this (LAZY §8.2: "ARCH §7.2's 20 BIR variants are unchanged. The same `TapeEmit` and `DirectBuild` rows exist; their lowering differs by mode").

**Lock 6 verdict: HONORED.** The lazy-mode change is target-side content, not contract-side shape. Generated LOC stays under budget. BIR snapshot is mode-invariant. Regen equality verifies the post-amendment source state.

### 3.4 Lock 9 (slice-borrow primary)

Lock 9 verbatim (`restart/locks/14-LOCKS.md:50`): "Default API is `&'i str` slices + `Cow<'i, str>` for transformations [...] Bumpalo arena is opt-in via `parse_in(input, &bump)`. Owned (no-borrow) is opt-in via `parse_owned(input)`. The three are surfaces over the same parse implementation; the lifetime parameter is the discriminant."

LAZY §6.3 commits: "`ValueRef<'doc, 'input, K>` retains both lifetime parameters. `'doc` borrows `tape` (which owns `offsets` and references `source`); `'input` is the bytes the tape references. In lazy mode the offsets array is owned by the tape, exactly as the tokens array was; the `parse(&str)` collapse to `'doc = 'input = 'a` works identically."

The `parse_in(input, &bump)` arena escape is preserved. LAZY §10.3 binds the `parse_in` row explicitly: "The user-facing API (`Json::parse(&str) -> Result<JsonRoot<'i>, ParseError>`) is unchanged. Downstream consumers (path-core, visitor, LSP cursor positioning) see the same `ValueRef` shape and the same `(TapeId, cursor, kind)` identity."

**Lock 9 verdict: HONORED, untouched.** Lifetime discriminant is the same. The `Tape<'input>` shape gains `offsets: Box<[u32]>` instead of `tokens: Vec<TapeToken>`, but both are owned-then-borrowed identically.

### 3.5 Lock 13 (no god directories)

Lock 13 verbatim (`restart/locks/14-LOCKS.md:58`): "Every directory partitions one cohesive concern; siblings are peer partitions of that concern; sub-modules express finer partitions. [...] Files >500 LOC outside `generated/` are forbidden; directories with >10 immediate children mixing concerns are forbidden."

The lazy design adds:

- `runtime/src/tape/builder.rs` rename to `assembler.rs` (LAZY §2.4). Same single file; no child-count increase.
- `runtime/src/tape/view.rs` lazy-mode walker functions (LAZY §10.1: +200 LOC). Current PASS-2 `runtime/src/tape/` has four children (`mod.rs`, `node.rs`, `payload.rs`, `checkpoint.rs` per PASS-2:284-287). Adding `view.rs` brings the count to 5 — still within Lock 13's 4-10 child band.
- `codegen/src/lower/rust.rs` mode-branching (LAZY §10.1: +200 LOC, -100 LOC, +100 net). Current PASS-2 `codegen/src/lower/rust/` has 8 children (`mod.rs`, `types.rs`, `rule.rs`, `node.rs`, `scanner.rs`, `host.rs`, `pratt.rs`, `error.rs` per PASS-2:232-240). The +100 net LOC distributed across the existing 8 children may push individual files over the 500 LOC handwritten cap (PASS-2:448).

PASS-2:448 requires `codegen/src/lower/rust/*` files to stay under 500 LOC each. The lazy-mode lowering branch for `TapeEmit` lives most naturally in `codegen/src/lower/rust/node.rs` (the per-BIR-node lowerer). If `node.rs` is already near the cap, the branch needs its own sibling — say `codegen/src/lower/rust/tape_mode.rs` — or absorption into a sub-module. The audit cannot determine current `node.rs` size from this PASS-2 read alone, but the punch list §12 below adds a row to the PASS-2 child-count proof table covering the lazy-mode lowering branch placement.

PASS-2:451 binds `runtime/src/*` to ≤10 children. Current children are `tape/`, `value/`, `error/`, `visitor/`, `layout/`, `owned/`, `grammars/` per PASS-2:284-306. Seven children. Adding `view.rs` as a sub-module of `tape/` does not change the top-level child count; this is internal cohesion.

**Lock 13 verdict: HONORED-WITH-PUNCH-LIST.** The 500-LOC cap on `codegen/src/lower/rust/node.rs` (or its successor) requires a child-count proof after the lazy-mode lowering branch lands; the punch list §12 row P2-V9.2-7 below specifies the verification command. The 4-10 child band is preserved.

### 3.6 Lock 14 (full grammar generalisation; zero overfitting)

Lock 14 verbatim (`restart/locks/14-LOCKS.md:60`): "The substrate carries ZERO grammar-specific code. Every grammar plugs into the fleet via three declarative surfaces only: (a) a grammar source file (`<name>.bbnf`), (b) workspace metadata declaring its strategy (recognisers, host fns, output-dir, pratt eligibility, simd eligibility, etc., per Lock 5's IR contract), and (c) optionally a per-grammar declaration crate. [...] Per-grammar runtime modules [...] are emitted from a single grammar-agnostic generator template that consumes (grammar source + workspace metadata) and produces typed Rust."

The lazy design adds `tape_mode = "lazy"` | `"eager"` to workspace metadata (LAZY §8.1). This fits Lock 14's surface (b) — workspace metadata declaring strategy. The `kind_at_cursor` discriminator function is generated per-grammar from the grammar's terminal alphabet (LAZY §5.1); it is not hardcoded in the runtime crate. The same template generates different discriminators for JSON vs hypothetical CSS-scan.

The substrate code change does not introduce a `match grammar { Json => ..., CssL4 => ..., ... }` arm anywhere in the generic crates. The mode-branch is on `tape_mode` (a per-grammar metadata value), not on a grammar identifier. The Lock 14 verification commands stay green:

- `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/{ir,codegen,runtime,...}/src/` returns zero (no grammar names in generic crates).
- `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|...' crates/` returns zero (no per-grammar match arms).

The mode-branch produces:

```rust
match ctx.tape_mode {
    TapeMode::Eager => emit_eager_tape_event(node, span, payload),
    TapeMode::Lazy => /* no-op; advance cursor through span */ emit_lazy_cursor_advance(span),
}
```

This is a branch on `TapeMode`, not on grammar identity. Lock 14 honored.

PASS-2's §6 future-grammar smoke at PASS-2:407-417 carries the two-surface invariant: "Author input consists only of `grammars/yaml.bbnf` and one `[workspace.metadata.bbnf.grammars.yaml]` block in `Cargo.toml`." The lazy design extends this metadata block with an optional `[workspace.metadata.bbnf.grammars.<g>.runtime] tape_mode = "lazy"` line (LAZY §8.1, §3.1 SUBSTRATE.md amendment). This is metadata, not code. Author surface unchanged.

**Lock 14 verdict: HONORED.** Mode metadata is a workspace-metadata-level switch, not a code-level grammar discriminator. The generated artefacts are per-grammar template output; no generic-crate grammar branching.

### 3.7 Summary

| Lock | Verdict | Required punch-list edit |
|---|---|---|
| Lock 1 | HONORED — verbatim text needs amendment to admit lazy shape | `restart/locks/14-LOCKS.md:34` + LAZY §4.1 amendment text; PASS-2 §2 commitment 3 (PASS-2:36) absorbs the same amendment by reference |
| Lock 5 | HONORED — mode is `LowerContext`/`GrammarMeta` parameter, not BIR | `LowerContext` field add at ARCH §7.5; PASS-2 §2 payload-refiner table row 15 (`TapeEmit`) and row 16 (`DirectBuild`) gain lazy-mode columns |
| Lock 6 | HONORED — content shift, not shape shift | regen gate + BIR snapshot gate unchanged; verification rerun under post-amendment source |
| Lock 9 | HONORED — lifetime discriminant unchanged | no edit |
| Lock 13 | HONORED-WITH-PUNCH-LIST | child-count proof for `codegen/src/lower/rust/*` after lazy-mode emission lands |
| Lock 14 | HONORED — mode is workspace-metadata key, not grammar identifier | metadata schema row at PASS-2:407-417 (two-surface invariant explanation gains an optional `tape_mode` row) |

---

## §4 Lane 3 — Cohesion: BIR alphabet under dual mode

The cohesion question is: does PASS-2's BIR alphabet contract (20 variants per PASS-2:34, ratifying ARCH §7.2) admit the dual-mode `TapeEmit` row without alphabet edits?

The proposal claims yes (LAZY §8.2). The audit verifies:

### 4.1 PASS-2 payload-refiner table absorption

PASS-2:50-76 lists 20 rows. The PASS-2 §2 prose at PASS-2:84 ratifies: "PASS-2 is payload refiner, not BIR re-owner. The variant alphabet, the variant inventory, and the producer-side semantics belong upstream at PASS-1 + Architecture §7."

The payload-refiner contract (PASS-2:86-95) lists "Refinement scope (PASS-2 may sharpen)" and includes "Layout-tag specialisation (e.g., `Alt { mode }` Dispatch-vs-Speculative selection at lower time)" and "Per-payload runtime template parameters."

A `tape_mode`-conditional `TapeEmit` lowering is payload-refinement-shaped: the BIR carries `TapeEmit { kind, span, payload_slot? }`; the lowerer's emission depends on `ctx.tape_mode`. This is identical in spirit to `Alt { mode: Dispatch | Speculative }` (ARCH §7.2 line 915) where the same BIR row lowers differently depending on a discriminator.

The key difference: `Alt`'s mode is a per-node payload field; `tape_mode` is a per-grammar `LowerContext` field. Both are "lowering parameter, not BIR alphabet" decisions. The audit accepts this.

**Conclusion: the BIR alphabet does NOT need a mode discriminator at the variant level.** The 20-variant shape survives.

### 4.2 The lowering-test gate row (PASS-2:106)

PASS-2:106 says: "`cargo test -p codegen --test tape_value_lowering` — `TapeEmit` + `DirectBuild` projection over one node identity, payload class, and scalar-cache policy."

Under lazy mode, "scalar-cache policy" is empty (no payload cache stored for JSON), and the test must exercise both mode lowerings: the eager-mode `TapeEmit` emits tape events, and the lazy-mode `TapeEmit` is a no-op. The gate needs a per-mode matrix axis: `(grammar, tape_mode) → expected lowering`.

The punch list §12 below adds a row to PASS-2:97-107 strengthening the gate.

### 4.3 Per-construct contribution plan (PASS-2:499-507)

PASS-2:507 lists `Layout` (currently `LayoutScope` per V9.1 punch); the table covers BIR-shaped constructs. The lazy-mode design introduces a new lowering shape for `TapeEmit` that should appear in this table: "Lazy mode: TapeEmit lowers to cursor-advance through span; no tape event emitted; net I/O removed." The punch list §12 adds this row.

### 4.4 Runtime emission table (PASS-2:511-523)

PASS-2:511-523 maps grammars to emission sources. Every row currently says "`generated.rs` | BIR snapshot + tape kinds + view structs". Under lazy mode, JSON's `generated.rs` carries fewer tape-emit calls and more cursor-advance calls; the column shape stays, but the cell content is different.

The punch list §12 adds a `tape_mode` column to PASS-2:511 so the regen contract is explicit about JSON being lazy-mode and the other 8 grammars being eager-mode.

### 4.5 SOTA gate (PASS-2:472-484)

The lazy-mode design's falsifiability target (LAZY §9.1: T1 > 14K Mbps on twitter for validation) maps onto PASS-2's existing SOTA gate at PASS-2:476: "sonic-rs `436 µs` / simd-json `424 µs` | JSON twitter | M1 Pro | ≤ 380 µs". The bbnf target of ≤ 380 µs is ~17.2K Mbps; LAZY §9.1's strong-validation target (T1 > 17K Mbps) maps to this PASS-2 row. Coherent.

The punch list §12 adds a "PASS-2 mechanism" amendment to the JSON twitter row at PASS-2:476: the current "`SimdScan` BIR + `simd-scan` structural index" mechanism becomes "`SimdScan` BIR + `simd-scan` structural index + lazy-mode tape (`tape_mode = "lazy"`)".

---

## §5 Lens I — contrivance: is the dual-mode template mechanical?

The lens-I question: is the dual-mode codegen template a mechanical branching artifact, or is it apparatus?

### 5.1 The branching structure

Per LAZY §7.1-7.3, the codegen template carries two emission shapes:

```rust
// Eager mode (today's behavior):
state.tape.emit(NodeKind::<kind>, __span, <payload_slot>);

// Lazy mode (new):
state.advance_through_span(__span);  // OR: nothing at all if the offset is already past
```

The branch on `ctx.tape_mode` lives in `codegen/src/lower/rust/node.rs` at the `TapeEmit` lowering site. This is one match arm per mode. The branch is mechanical — there is no inversion of the lowering pipeline, no new pass, no new IR.

Compare to existing PASS-2 contrivance candidates:

- The `Backend` trait's two-method shape (`lower` + `emit_artefacts`) was contrivance per Phase-8.4 α1 fold; the per-method dispatch was collapsed because "the four were always co-emitted from the same input" (PASS-2:131). The current trait shape is one method that emits the four artefact families from one input. The lazy-mode addition does not break this — `lower` still produces `RustSource`, `emit_artefacts` still produces `ArtefactSet`. The mode-branch lives inside `lower`'s body.
- The internal `BackendLowerer` 8-method trait (PASS-2:133-134) per Phase-8.4 α7 carries "no V1 polymorphism; only `RustLowerer` implements it." Lazy mode does not add a new impl; the mode-branch lives inside `RustLowerer::emit_node` or whichever method handles `TapeEmit`. No new V1 trait impls.

**Lens I verdict: MECHANICAL.** The dual-mode template is a single `match ctx.tape_mode` branch in one method. It is not apparatus. The branching is structurally identical to the existing `Alt { mode: Dispatch | Speculative }` lowering branch (PASS-2:56), which is already in the canonical BIR alphabet.

### 5.2 The view-side walker

LAZY §7.3 introduces `next_sibling_cursor` and `find_matching_close_quote` depth-tracked walker functions in `view.rs` (LAZY §10.1: +200 LOC for view-layer walkers). These are new generic facilities. Are they apparatus?

The walker functions are per-grammar generated from the grammar's structural-bracket alphabet (LAZY §5.1 JSON discriminator; CSS-scan would have a different alphabet). The generator template that emits them is grammar-agnostic; it reads "structural-open bytes" and "structural-close bytes" from BIR `SimdScan` payload metadata (ARCH §7.2 line 951: "`SimdScanMode::{Exact, Prefilter}`, needle/class, fallback, verifier route") plus shape metadata, and synthesizes a depth-counter walker.

The walkers are O(subtree size) for `JsonObject::iter().nth(k)` (LAZY §9.3); LAZY §9.3 calls this out as a known risk and mitigates with an optional sidecar `subtree_skip_index`. The risk is measurement-bound, not contrivance.

**Lens I verdict on walkers: MECHANICAL with measurement-gated fallback.** The depth-track walker is straightforward per-grammar template output. The O(n) sibling skip is the cost lazy mode pays; the sidecar fallback is a clean architectural escape if measurement requires it. The proposal commits to landing without the sidecar to test the pure hypothesis (LAZY §9.3 falsification: "if post-implementation `serialize_canonical` is > 1.5× slower, the sidecar lands").

---

## §6 Lens N — graduation mechanicality: V1 Rust lowerer template

Lens N asks: does the V1 Rust lowerer template change mechanically, or architecturally?

Lock 5 verbatim (`restart/locks/14-LOCKS.md:42`): "Codegen emits a backend-agnostic typed IR; per-backend lowerers produce native source. [...] The IR is the contract."

The lowerer's contract per ARCH §7.5 (line 1108):

```rust
pub trait Backend {
    type Output;
    type Error;
    fn lower(&self, bir: &BackendIR, ctx: &LowerContext) -> Result<Self::Output, Self::Error>;
    fn emit_artefacts(&self, grammar: &GrammarMeta, schemas: &SchemaSet) -> Result<ArtefactSet, Self::Error>;
}
```

The lazy-mode amendment requires:

1. **`LowerContext` gains a `tape_mode: TapeMode` field.** This is a new field on an existing struct. The struct already carries "target triple, generated-code budget cursor, grammar metadata reference, a `&SideTables` reference, lint-mode toggles" (ARCH §7.5 line 1144-1147). Adding `tape_mode` is mechanical — one field add.

2. **Or, alternatively, `GrammarMeta` carries `tape_mode`.** Since `tape_mode` is per-grammar (LAZY §8.1: declared in `[workspace.metadata.bbnf.grammars.<g>.runtime]`), this is the more natural carrier. `GrammarMeta` is passed to `emit_artefacts`; the lazy-mode codegen also needs it inside `lower` for the `TapeEmit` branch. Either `LowerContext` carries a reference to `GrammarMeta`'s `tape_mode`, or `LowerContext` is constructed from `GrammarMeta` and carries the mode directly.

3. **The Rust lowerer's `TapeEmit` arm gains a mode branch.** One match arm split into two. The eager arm is unchanged; the lazy arm emits cursor-advance (or nothing).

4. **The Rust lowerer's `DirectBuild` arm gains a mode branch.** Eager: builds typed views over `u32` token indices. Lazy: builds typed views over `u32` cursor positions into `offsets`. The struct shape is the same `u32`; the *semantic* meaning differs. The lowerer emits the same field-access code; the difference is upstream (`Tape<'i>::tokens[index]` vs `Tape<'i>::offsets[cursor]`).

5. **Runtime template `tape.rs` becomes mode-monomorphic via a feature/cfg gate.** LAZY §8.3: "The substrate crate (`crates/runtime/src/tape/`) exposes BOTH layouts as `#[cfg]`-selected or feature-selected modules. The public API is mode-monomorphic at the per-grammar-crate level: `runtime::grammars::json::Tape` is the lazy variant; `runtime::grammars::css_l4::Tape` is the eager variant. They are distinct types with the same trait surface (`DocumentView` impls), not a runtime polymorphic dispatch."

Item 5 is where the architectural decision lives. The proposal places mode-selection at the *per-grammar-crate* level via cfg/feature gating. This produces two distinct concrete types under different names in the public API, each implementing a shared trait. The runtime crate gains a feature flag per grammar (or equivalent module-level cfg) — this is genuinely new infrastructure.

The audit asks: does this violate Lock 14's "single grammar-agnostic generator template"?

Lock 14 (`restart/locks/14-LOCKS.md:60`): "Per-grammar runtime modules (value, document, view, kind) are emitted from a single grammar-agnostic generator template that consumes (grammar source + workspace metadata) and produces typed Rust."

The lazy-mode template is *still single and grammar-agnostic*. It consumes `(grammar source + workspace metadata.tape_mode)` and produces typed Rust. The cfg/feature gate is *generated* from the metadata, not hand-edited. The template's mode-branch is grammar-agnostic; it selects an emission shape from a metadata value, not from a grammar identifier.

**Lens N verdict: MECHANICAL.** The Rust lowerer template change is:

- One field add to `LowerContext` (or one access to `GrammarMeta.tape_mode`).
- One match arm split for `TapeEmit` lowering.
- One match arm split for `DirectBuild` lowering.
- Generated `tape.rs` mode-selection via metadata-driven cfg/feature.

No new BIR variants. No new trait impls. No new pass. No grammar-walking. The lowerer reads BIR + metadata, branches on metadata, emits target source. Lock 5 honored.

---

## §7 Lens J — host-language leverage

Lens J asks: does lazy-mode walk-at-view-method-call-time leverage Rust's iterator/Cow shape correctly?

### 7.1 The Cow leverage

LAZY §4 (SUBSTRATE.md amendment §1.3, view-side `JsonString::as_str` rewrite, LAZY §2.2):

```rust
pub fn as_str(self) -> Cow<'input, str> {
    let cursor = self.cursor.cursor as usize;
    let start = self.cursor.tape.offsets[cursor] as usize + 1;  // skip "
    let end_offset_idx = find_matching_close_quote(...);
    let end = self.cursor.tape.offsets[end_offset_idx] as usize;
    let raw = &self.cursor.tape.source[start..end];
    let has_escapes = string_candidates_in_range(...);
    if has_escapes {
        Cow::Owned(unescape_json(raw))
    } else {
        Cow::Borrowed(unsafe { std::str::from_utf8_unchecked(raw) })
    }
}
```

This is the lightning-css `Cow<'i, str>` shape ratified at Lock 9 (`restart/locks/14-LOCKS.md:50`). The lazy mode walks string-candidate offsets at view-method-call time and decides borrow-vs-own per call. No eager scan; no per-token escape flag stored upfront.

The pattern is correct for Rust. `Cow<'input, str>` mediates between zero-copy (the common case for non-escape JSON strings) and owned (escape strings). The host-language leverage is the same as lightning-css; the difference is *when* the decision fires: eager mode (today) computes the flag at parse time and stores it; lazy mode computes it at access time.

**Pro:** unused strings pay zero unescape cost. **Con:** repeated access to the same string pays repeated string-candidate scans. The risk register at LAZY §9.3 implicitly covers this — the cost of `serialize_canonical` walking every node hits exactly this issue, and the falsification gate is 1.5× regression. The audit accepts.

### 7.2 Iterator shape

LAZY §7.3 `JsonMemberIter`:

```rust
impl<'doc, 'input> Iterator for JsonMemberIter<'doc, 'input> {
    type Item = (JsonString<'doc, 'input>, JsonValueRef<'doc, 'input>);
    fn next(&mut self) -> Option<Self::Item> { ... }
}
```

The iterator returns owned `(JsonString, JsonValueRef)` pairs each carrying `'doc` + `'input` lifetimes. This is the canonical Rust iterator shape; no `&Self::Item` borrowing required. The `Copy` semantics of `ValueRef` (per SUBSTRATE.md §1.3 preserved by `PhantomData<fn() -> K>`) means the iterator never has to manage a backing buffer of items — each `next()` call walks one more offset and yields a freshly-constructed `ValueRef`.

This is good Rust. The iterator state is `{ tape: &'doc Tape<'input>, cursor: u32, done: bool }` per LAZY §7.3 — three machine words. No `Vec<Item>` allocation; no `&Item` lifetime management.

**Lens J verdict: CORRECT host-language leverage.** Cow shape mediates borrow/owned; iterator shape returns owned `Copy` `ValueRef`s without backing-buffer management. The lazy-mode walkers are Rust-idiomatic.

### 7.3 LLVM compilation of the discriminator

LAZY §5.1 claims: "The discriminator function compiles to a 256-entry jump table (LLVM does this for byte-disjoint match arms automatically; see `COMPILER.md:489` where the same pattern is used for alt-dispatch). On M1 Pro this is one cache-line load from `tape.source[off]`, one indirect branch, ~1-2 ns total."

The claim is structurally correct: LLVM's `match` on a `u8` with byte-disjoint arms lowers to a jump table or a binary search depending on density. For the JSON discriminator (11 reachable bytes out of 256), the density is sparse but the bytes are clustered (digits 0x30-0x39, alpha bytes for keywords); LLVM may emit a small switch or a multi-level branch. Either way, the cost is bounded by one cache-line load + one indirect branch + one match-arm body. This is the same cost the eager-mode `tape.tokens[index].kind` read pays — except eager pre-stores the kind as a u16 in the token, and lazy computes it from a source-byte read.

The cache-locality analysis at LAZY §9.2 is honest: lazy mode's 4× smaller offset array fits more cache, but each kind query touches `source[off]` which is a second cache line. The net win is corpus-dependent (cache-bound on small inputs; bandwidth-bound on large inputs).

**Lens J verdict on LLVM leverage: CORRECT.** The discriminator function is the same byte-match shape PASS-2 already lowers for alt-dispatch (`Alt { mode: Dispatch }`); LLVM's jump-table optimization fires identically. The Lens-J host-language-leverage is consistent.

---

## §8 Lane 2 — sequencing discipline

The amendment lands in two phases per LAZY §11.3:

1. **Skinny-side implementation** (1-2 weeks per LAZY §10.2). Implement lazy-mode JSON in skinny substrate; re-bench against the existing harness; classify under existing outcome matrix (BENCH.md §6).
2. **V1 graduation** (post-skinny-validation). Lock 1 amendment lands, ARCH §9.1 Tape invariants amendment, SUBSTRATE.md §1.1-§1.3, COMPILER.md §3+§6, BENCH.md §1.1-§1.3+§3.4, INDEX.md cross-quadrant invariants, WORKSPACE.md `tape_mode` key.

PASS-2's tranche dispatch perspective:

- V1 Tranche B (runtime substrate) absorbs lazy-mode for JSON.
- V1 Tranche F (Rust lowerer template) gains mode-branching emit path.
- V1 Tranche I (LSP / incremental parse) re-anchors `ReparsePlan::Reuse { unchanged: Vec<TapeRange> }` from token-range to offset-range. LAZY §10.3 explicitly says "No I tranche redesign required; only the data type for 'reusable range' changes."
- V1 Tranche J (memory residency / SOTA close) gains the lazy-mode RSS win (LAZY §10.3: canada's 3.572 MB allocated tape drops to ~668 KB offsets; ~5× memory win).

The substrate-consumer wiring is same-wave: the lazy-mode tape and its consumer (the generated parser + view layer) land together in B + F. No substrate-without-consumer wave. Lock 1's "no substrate-first/consumer-later" clause is honored.

**Lane 2 verdict: SEQUENCING DISCIPLINED.** The amendment's V1 tranche mapping is explicit and same-wave-consumed. No orphan substrate.

---

## §9 Lane 4 — SOTA anchoring

The lazy-mode falsifiability targets at LAZY §9.1, §9.4, §11.2 are anchored to specific SOTA numbers:

- Validation: T1 > 14K Mbps on twitter (1.12× today's 12.5K) — outcome C/D per BENCH.md §6.
- Strong validation: T1 > 17K Mbps on twitter (1.36× today's 12.5K, ~= sonic-rs 18.4K with 8% margin) — outcome A/B.
- Refutation: T1 < 13K Mbps on twitter — outcome G repeats; lazy-mode hypothesis is wrong.

The strong-validation target maps onto PASS-2's existing SOTA gate row at PASS-2:476:

> sonic-rs `436 µs` / simd-json `424 µs` | JSON twitter | M1 Pro | ≤ 380 µs | `SimdScan` BIR + `simd-scan` structural index | `cargo bench -p bbnf-bench --bench sota_json -- twitter`

twitter at 616 KB; 380 µs ≈ 17,200 Mbps. LAZY's strong-validation target (T1 > 17K Mbps) exactly matches PASS-2's existing bbnf target. Consistent.

The lazy-mode mechanism (`SimdScan` BIR + `simd-scan` structural index + lazy-mode tape) supersedes the current eager-tape mechanism for JSON at the same SOTA gate. The punch list §12 below adds a mechanism-column amendment to PASS-2:476.

The other rows (citm, canada, CSS bootstrap, CSS animate, structural scan) are not directly affected: citm and canada gain the lazy-mode mechanism for JSON (their mechanisms at PASS-2:477-478 should also be amended); CSS rows are eager-mode grammars (PASS-2 says CSS L4 keeps `tape_mode = "eager"`).

**Lane 4 verdict: SOTA ANCHORED.** The lazy-mode mechanism amendment lands at PASS-2:476-478. The structural-scan gate (PASS-2:481-482) is unchanged — `simd-scan` is the kernel under both modes.

---

## §10 Lane 5 — grammar-authoritative discipline

The amendment must NOT introduce grammar-specific code in generic crates. The audit verifies:

- The `tape_mode` workspace metadata key is per-grammar metadata, not generic-crate code (LAZY §8.1). Honored.
- The `kind_at_cursor` discriminator function is generated per-grammar (LAZY §5.1: "runtime/src/grammars/json/view.rs — generated"). The discriminator is generated source under `runtime/src/grammars/<g>/`, not handwritten in `runtime/src/tape/`. Honored.
- The `JsonKind` enum is a per-grammar generated type, not a substrate type. Honored.
- The depth-tracked walker (`next_sibling_cursor`, `find_matching_close_quote`) is generated per-grammar from the grammar's structural-bracket alphabet. The walker template lives in `codegen/src/runtime_template/` (grammar-agnostic) and produces per-grammar source. Honored.

Lock 14 verification commands (PASS-2 lines 60, 416):

- `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/{ir,parse,codegen,runtime,...}/src/` returns zero. Honored — no grammar-name leakage from lazy-mode.
- `find crates/runtime/src -mindepth 1 -maxdepth 1` returns the generated subdir only for any grammar. Honored — generated subdirs only.

**Lane 5 verdict: GRAMMAR-AUTHORITATIVE DISCIPLINE HELD.** No grammar-name leakage; no `match grammar` arms; mode is metadata-driven.

---

## §11 Lane 6 — generated-code budget

LAZY §7.4 + §10.1 forecast:

| File | Eager LOC | Lazy LOC | Delta |
|---|---:|---:|---:|
| JSON `generated.rs` | ~600 | ~400 | -200 |
| JSON `view.rs` | ~250 | ~350 | +100 |
| JSON `parser.rs` | ~120 | ~120 | 0 |
| JSON `host.rs` | ~5 | ~5 | 0 |
| JSON `value.rs` | ~80 | ~80 | 0 |
| JSON `visitor.rs` | ~100 | ~100 | 0 |
| JSON total | ~1,155 | ~1,055 | -100 |

PASS-2's §6 budget for JSON is 3,500 → 3,570 (+2% cap, PASS-2:432). The lazy-mode change brings JSON generated LOC *down* by ~100 — well under cap.

Note: the LAZY §7.4 numbers (~1,155) appear lower than PASS-2's 3,500 baseline. The difference is the skinny crate envelope vs the full bbnf JSON generated tree — skinny is a minimal subset for benchmarking; the full runtime adds ~2,300 LOC of typed value/document/visitor/error code that the skinny strips. Under V1 graduation, the full bbnf JSON tree absorbs the lazy-mode change proportionally: net LOC drop ~100-300 LOC for JSON. Well under the 3,570 cap.

For the 8 eager-mode grammars (bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, math), the generated LOC is unchanged. No budget movement.

**Lane 6 verdict: BUDGET HONORED.** The lazy-mode amendment is generated-LOC-negative on JSON and neutral on all other grammars. The +2% PASS-2 cap is not stressed.

---

## §12 Punch list — per-target absorption edits

These are the surgical edits PASS-2 (and adjacent V1 surfaces PASS-2 owns) must absorb to admit the dual-mode tape design at the V1-corpus level. Each item names the file, the section, the verbatim or surgery-described edit, and the verification gate.

### P2-V9.2-1 — PASS-2 §2 commitment 3 (tape materialisation)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:36`

**Current text:** "Tape/direct-to-struct is one materialisation plan. Every rule has a `TapeShape` and `ValueShape`. `TapeShape` owns token kind, span class, payload class, traversal skip policy, and scalar-cache policy."

**Surgery:** Append to the end of the paragraph: "Per LAZY-TAPE-DESIGN §4 (Lock 1 amendment), `TapeShape` admits a `mode: TapeMode ∈ {Eager, Lazy}` discriminator declared per-grammar in workspace metadata at `[workspace.metadata.bbnf.grammars.<g>.runtime] tape_mode`. Eager mode stores 16-byte tokens; lazy mode stores a structural-offset stream and computes kind from `source[offsets[cursor]]` via a generated per-grammar discriminator. Both modes share `ValueRef<'doc, 'input, K>`, `DocumentView`, `Visitor`, and `PayloadArena` surfaces; mode is a lowering parameter, not a BIR alphabet edit. CSS L4, BBNF-self, Sheets, math, ebnf, csv, css_pretty, bnf retain `tape_mode = "eager"` (layout/recovery/payload-bearing tokens require stored classes); JSON adopts `tape_mode = "lazy"` post-SK-V2 ratification."

**Verification:** `rg -n 'tape_mode' restart/audit/pass-2-codegen/PASS-2.md` returns the new commitment plus the §6 emission-table column added below (P2-V9.2-6).

### P2-V9.2-2 — PASS-2 §2 payload-refiner table row 15 (`TapeEmit`)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:71` (the row "`TapeEmit` | tape kind, payload class, scalar cache policy | BIR builder | tape event | Linear-memory or host-object representation in V2.")

**Surgery:** Replace the "Rust V1 lowering" cell ("tape event") with: "Eager mode (`tape_mode = "eager"`): emit `state.tape.emit(NodeKind::<kind>, span, payload)`. Lazy mode (`tape_mode = "lazy"`): no-op; the structural offset is already present in the parse index, no tape event emitted; verifier-route work fires through `state.advance_through_span(span)` for bracket-depth tracking."

**Verification:** the lowering-test gate `cargo test -p codegen --test tape_value_lowering` exercises both mode emissions (per P2-V9.2-4 below).

### P2-V9.2-3 — PASS-2 §2 payload-refiner table row 16 (`DirectBuild`)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:72` (the row "`DirectBuild` | generated field projection, owner type | type lower | typed direct view | Host-native object/view shape in V2.")

**Surgery:** Replace the "Rust V1 lowering" cell ("typed direct view") with: "Eager mode: build typed view fields pointing at sealed tape token indices (`Json<Shape> { field_1: <u32 token index>, ... }`). Lazy mode: build typed view fields pointing at sealed `offsets` cursor positions (`Json<Shape> { field_1: ValueRef { tape, cursor: <u32 offset index>, .. }, ... }`). Field shape unchanged; cursor semantics changed by mode."

**Verification:** the same lowering-test gate exercises both shapes; identity invariant `(TapeId, cursor, kind)` (lazy) ↔ `(TapeId, index, payload_class)` (eager) verified in fixture.

### P2-V9.2-4 — PASS-2 §2 lowering-test gate row (tape/direct/value)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:106` (the row "Tape/direct/value | `cargo test -p codegen --test tape_value_lowering` — `TapeEmit` + `DirectBuild` projection over one node identity, payload class, and scalar-cache policy.")

**Surgery:** Replace the test description with: "`cargo test -p codegen --test tape_value_lowering` — `TapeEmit` + `DirectBuild` projection over one node identity, payload class, and scalar-cache policy, exercised across both `tape_mode = "eager"` (CSS L4 fixture) and `tape_mode = "lazy"` (JSON fixture). Eager-mode test asserts `state.tape.emit(...)` calls fire with expected kind/span/payload; lazy-mode test asserts zero `state.tape.emit(...)` calls fire and verifies cursor-advance through every node span. Identity invariant `(TapeId, cursor, kind_at_cursor)` for lazy mode and `(TapeId, index, payload_class)` for eager mode is asserted byte-equal across regenerations."

**Verification:** test fixture set covers both modes; no V1 grammar lacks coverage in its mode.

### P2-V9.2-5 — PASS-2 §7 perf-gate JSON row (twitter)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:476` (the row "sonic-rs `436 µs` / simd-json `424 µs` | JSON twitter | M1 Pro | ≤ 380 µs | `SimdScan` BIR + `simd-scan` structural index | `cargo bench -p bbnf-bench --bench sota_json -- twitter`")

**Surgery:** Replace the "PASS-2 mechanism" cell with: "`SimdScan` BIR + `simd-scan` structural index + lazy-mode tape (`tape_mode = "lazy"`); the structural-offset array IS the tape, node kind computed from `source[offsets[cursor]]` at view-method-call time; `TapeEmit` BIR rows lower to no-op per the §2 payload-refiner table". Apply equivalent edits to PASS-2:477 (citm) and PASS-2:478 (canada).

**Verification:** post-SK-V2 ratification, the SK re-bench's twitter Mbps maps to ≤ 380 µs at 616 KB (~17.2K Mbps); the LAZY-TAPE-DESIGN strong-validation target (T1 > 17K Mbps on twitter) maps onto this PASS-2 row.

### P2-V9.2-6 — PASS-2 §6 runtime emission table (column add)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:511-523` (the table mapping grammars to `generated.rs`, `parser.rs`, `host.rs`, host source, layout source, error source, Pratt/SIMD source).

**Surgery:** Add a column "tape_mode" with values:

| Grammar | tape_mode |
|---|---|
| bbnf | eager |
| bnf | eager |
| csv | eager |
| css_l4 | eager |
| css_pretty | eager |
| ebnf | eager |
| google_sheets | eager |
| json | lazy |
| math | eager |
| yaml (smoke) | eager (provisional; opens to lazy post-onboarding measurement) |

**Verification:** the column matches LAZY-TAPE-DESIGN §8.1 metadata declarations; the regen contract emits the correct shape per grammar.

### P2-V9.2-7 — PASS-2 §6 non-generated budget table (child-count proof for lower/rust)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:448` (the row "`codegen/src/lower/rust/*` | No handwritten file > 500 LOC | 4-10 children partitioned by emitted concern (types, rule, node, scanner, host, pratt, error), not by grammar. | [enforcing command]")

**Surgery:** Append to the "Child-count proof" cell: "Post-V9.2 lazy-mode amendment: the mode-branch for `TapeEmit` and `DirectBuild` lowering lives in `node.rs` if its post-amendment LOC stays under the 500 LOC cap; otherwise it splits into a sibling `tape_mode.rs` under the same `codegen/src/lower/rust/` directory, keeping the child count within 4-10."

**Verification:** `find crates/codegen/src/lower/rust -mindepth 1 -maxdepth 1 | wc -l` ∈ [4,10]; `wc -l crates/codegen/src/lower/rust/*.rs` returns no row > 500.

### P2-V9.2-8 — PASS-2 §6 future-grammar smoke (two-surface invariant)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:416` (the row "Two-surface invariant | Author input consists only of `grammars/yaml.bbnf` and one `[workspace.metadata.bbnf.grammars.yaml]` block in `Cargo.toml`.")

**Surgery:** Replace the row's text with: "Author input consists only of `grammars/yaml.bbnf` and one `[workspace.metadata.bbnf.grammars.yaml]` block in `Cargo.toml`. The metadata block MAY include an optional `[workspace.metadata.bbnf.grammars.yaml.runtime] tape_mode = "lazy"` sub-table; default is `"eager"`. The `tape_mode` value is the only mode-discriminator surface; no grammar-source change is required to switch modes. Generated runtime/path/visitor/host/diagnostic/budget files may be committed as xtask output, but they are not author inputs."

**Verification:** the two-surface verification commands at PASS-2:416 remain green; the optional `tape_mode` row is metadata, not author code.

### P2-V9.2-9 — PASS-2 §7 per-construct contribution plan (TapeEmit row)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:499-507` (the per-construct contribution-plan table covering `Alt`, `SimdScan`, `RegexProgram`, `PrattSpine`, `CallHost`, `LayoutScope`).

**Surgery:** Insert a new row after the `LayoutScope` row at PASS-2:507:

> | `TapeEmit` (mode-conditional) | Eager mode: appends 16-byte tape token per node event. Lazy mode: no-op; the parse_index already carries the structural offset, no write fires. Net I/O removed under lazy mode. | LAZY-TAPE-DESIGN §7.1-§7.2 + this V9.2 punch list. |

**Verification:** the table row mirrors the §2 payload-refiner row (P2-V9.2-2).

### P2-V9.2-10 — PASS-2 §8 diagnostic ledger (no edit required, verify silence)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:570-579` (the diagnostic ledger).

**Verification:** The lazy-mode amendment introduces no new compile-time diagnostic (mode is metadata-driven; invalid `tape_mode` values fall through workspace-metadata validation upstream, not codegen-side). PASS-2 §8 needs no new diagnostic row. If a grammar declares `tape_mode = "lazy"` but has recovery/layout/payload-bearing tokens that require eager mode, the upstream metadata validator emits a clear error before codegen; PASS-2's `BBNF-CODEGEN-IMPORT-DENY` and `BBNF-RUNTIME-TEMPLATE-METADATA-MISSING` rows remain authoritative.

### P2-V9.2-11 — PASS-2 §5 PASS-1 handoffs (Layout/error annotations row)

**Target:** `restart/audit/pass-2-codegen/PASS-2.md:402` (the row "Layout and error annotations | `LayoutScope` and `ErrorRecover` nodes")

**Surgery:** No change required to the row; the lazy-mode amendment does not change PASS-1's hand-off shape. But add a new row at the end of the table at PASS-2:403:

> | Tape materialisation mode | `tape_mode` per `[workspace.metadata.bbnf.grammars.<g>.runtime]`; consumed by `passes::extract` and routed into `LowerContext` for the `RustBackend: Backend` impl. PASS-1 validates that grammars declaring `tape_mode = "lazy"` carry no layout/recovery/payload-bearing token requirements; otherwise PASS-1 emits `BBNF-TAPE-MODE-INCOMPATIBLE` and rejects extraction before BIR. |

**Verification:** the new row binds PASS-1 to mode-validity; PASS-2 consumes the validated mode.

### P2-V9.2-12 — Adjacent V1 surface: ARCH §7.5 LowerContext schema

**Target:** `restart/ARCHITECTURE.md:1144-1147` (the `LowerContext` description: "target triple (or wasm32-equivalent), generated-code budget cursor, grammar metadata reference, a `&SideTables` reference whose definition lives at §7.3 (one struct over `LayoutFacts`, `ShapeFacts`, `RecognizerFacts`, `CostFacts`, `RecoveryFacts`, `BridgeJustification`), and lint-mode toggles.")

**Surgery:** Append: "and `tape_mode: TapeMode` for the active grammar, sourced from `GrammarMeta.runtime.tape_mode` per the workspace metadata schema (LAZY-TAPE-DESIGN §4, §8.1). The `Backend::lower` body branches on `tape_mode` for `TapeEmit` and `DirectBuild` lowering arms; no BIR alphabet edit required (per ARCH §7.2 invariant: lowering parameters live in `LowerContext`, not in BIR variants)."

**Verification:** The ARCH §7.5 trait obligations table at ARCH:1130-1133 admits no new method; `LowerContext` carries the new field; the Backend trait surface stays two-method.

### P2-V9.2-13 — Adjacent V1 surface: ARCH §7.2 BIR invariant on TapeEmit

**Target:** `restart/ARCHITECTURE.md:992-1000` (the Backend IR invariants table).

**Surgery:** Append a new row to the invariants table:

> | `TapeEmit` lowering admits dual-mode emission: eager-mode emits a tape token per node event; lazy-mode emits zero source per node event and relies on the structural-offset parse index for materialisation. The BIR variant is unchanged; the lowering choice is target-conditional via `LowerContext.tape_mode`. | Snapshot tests assert `TapeEmit` BIR payload byte-equal across modes; lowering tests verify mode-specific emission per P2-V9.2-4. |

**Verification:** the BIR snapshot under lazy-JSON is byte-identical to the eager-JSON snapshot of the same grammar source; the lowered Rust source differs by the mode-branch only.

### P2-V9.2-14 — Adjacent V1 surface: Lock 1 amendment text

**Target:** `restart/locks/14-LOCKS.md:34` (Lock 1 verbatim).

**Surgery:** Apply LAZY-TAPE-DESIGN §4.1 amendment text verbatim with one tightening: replace the bullet "Lazy mode (`tape_mode = "lazy"`)" sentence "the typed walker (direct-to-struct projections) IS the materialisation" with "the structural-offset array IS the parsed-stream substrate; typed walkers (direct-to-struct projections) compute kind from `source[offsets[cursor]]` at view-method-call time. The structural-offset array is owned by `Tape<'input>` and sealed at parse close; typed values borrow it via `&'doc Tape<'input>` + cursor exactly as eager-mode typed values borrow the token stream + index."

**Verification:** Lock 1 verbatim text accepts both modes; the no-parallel-substrate / no-OpenFrame-clone / no-columnar-SoA clauses are preserved unchanged.

### P2-V9.2-15 — Adjacent V1 surface: ARCH §9.1 Tape invariants (forward declaration)

**Target:** Future ARCH §9.1 Tape invariants section (does not currently exist in ARCH; LAZY-TAPE-DESIGN §11.3 step 2 names it as the landing site).

**Surgery:** Open a new ARCH §9.1 with: "Tape invariants — eager and lazy modes. Tape contents are mode-conditional per `[workspace.metadata.bbnf.grammars.<g>.runtime] tape_mode ∈ {Eager, Lazy}`. Eager mode: `Tape::tokens: Box<[TapeToken]>` (or private `Vec<TapeToken>` sealed); 16-byte tokens carry kind, flags, span, payload-or-skip. Lazy mode: `Tape::offsets: Box<[u32]>` of structural offsets; kind computed from `source[offsets[cursor]]` plus per-grammar discriminator function. Both modes share `Tape::source: &'input [u8]`, `Tape::payloads: PayloadArena`, `Tape::id: TapeId`. Both modes share the `(TapeId, cursor, kind)` identity invariant per Lock 1 amendment. Both modes share `ValueRef<'doc, 'input, K>` cursor shape, `DocumentView`, `Visitor` trait surfaces. Per-grammar-crate mode-monomorphic via cfg/feature gating; no runtime polymorphism."

**Verification:** ARCH §9.1 lands at V1 graduation; SUBSTRATE.md §1 amendments (LAZY §3.1) point at ARCH §9.1 for the invariant authority.

---

## §13 Final verdict

**Decision: AMENDMENT-REQUIRED-NARROW (admissible with the punch list above).**

The lazy-tape design is an empirically-grounded, architecturally-honest amendment to the substrate. SK-V2's outcome G on three corpora, three rejected micro-amendments, and the diagnosis that materialisation cost is the dominant gap leave lazy-mode as the remaining architectural move. The proposal:

- Preserves the BIR alphabet (20 variants unchanged).
- Preserves the `ValueRef`, `DocumentView`, `Visitor`, `PayloadArena`, `parse(&str)` surfaces (identity invariant survives via `(TapeId, cursor, kind)`).
- Preserves Lock 5 (BIR-only lowering; mode is `LowerContext` parameter, not BIR variant).
- Preserves Lock 6 (committed source; regen-equality unchanged).
- Preserves Lock 9 (slice-borrow primary; lifetime discriminant unchanged).
- Preserves Lock 14 (full grammar generalisation; mode is workspace metadata, not generic-crate code).
- Preserves Lock 13 (no god directories; net LOC drop on JSON; child-count proof at P2-V9.2-7).

The required amendments (Lock 1 verbatim text; ARCH §7.5 `LowerContext` field; ARCH §7.2 invariant row; ARCH §9.1 forward declaration; PASS-2 §2/§5/§6/§7 absorption) are mechanical. No BIR alphabet edit. No new pass. No new trait impl. No grammar-walking.

The risk register (LAZY §9) is honest: per-byte kind cost (~2-3 ns/query), cache locality on large corpora, O(n) sibling-skip in pathological access patterns. Each risk has a falsification gate; each falsification is testable against the existing BENCH harness without new probes. The proposal commits to landing without the `subtree_skip_index` sidecar to test the pure hypothesis (LAZY §9.3); if `serialize_canonical` regresses > 1.5×, the sidecar lands as v3.

Lens A-K verdicts:

| Lens | Verdict | Reason |
|---|---|---|
| A / lock adherence | AMENDMENT-REQUIRED-NARROW | Lock 1 verbatim text needs amendment per LAZY §4.1 with V9.2 tightening; Locks 5/6/9/13/14 honored per §3 |
| B / sequencing | READY | Same-wave substrate-consumer wiring; Tranche B + F dispatch is consistent with the V1 plan; no orphan substrate |
| C / cohesion | READY | BIR alphabet unchanged; payload-refiner table absorbs lazy-mode rows mechanically |
| D / SOTA anchoring | READY | LAZY §9.1 strong-validation target (T1 > 17K Mbps twitter) maps onto PASS-2:476 (≤ 380 µs); mechanism amendment is P2-V9.2-5 |
| E / grammar-authoritative | READY | `tape_mode` is workspace metadata; no grammar-name leakage; Lock 14 commands stay green |
| F / LLM-bias | READY | Proposal is grounded in explicit empirical evidence; no hedging; falsifiability is sharp |
| G / overfitting | READY | Mode is per-grammar metadata; substrate stays grammar-agnostic; per-grammar discriminator is generated template output |
| H / provenance | READY | LAZY-TAPE-DESIGN cites skinny §3.1 verbatim per-file before/after edits; line citations check out within the design doc |
| I / contrivance | READY | Dual-mode template is one match arm per mode; no apparatus; structurally identical to existing `Alt { mode }` branch |
| J / host-language leverage | READY | Cow shape for borrow/own; iterator returns `Copy` ValueRef; LLVM jump-table on byte-disjoint match arms |
| K / meta-grammar discipline | READY | V1 RustBackend consumes mode via LowerContext; V2 WasmBackend/TsBackend inherit the same mode parameter without re-architecting BIR |
| N / graduation mechanicality | READY | Rust lowerer template change: one field add, two match-arm splits, generator-driven cfg gate. No new BIR. No new pass |

The punch list at §12 carries 15 surgical absorption edits. Hereupon: PASS-2 absorbs the V9.2 amendment after SK-V2 re-bench validates the lazy-mode hypothesis (T1 > 14K Mbps on twitter). If SK-V2 lands outcome G again, the V9.2 amendment is held; PASS-2 stays at its current eager-tape commitment.

The proposal is admissible. No re-draft threshold is met. Recommend SK-V2 ratification, then PASS-2 amendment execution per §12 punch list.
