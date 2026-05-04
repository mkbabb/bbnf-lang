# Agent 5 - Error Recovery + Incremental Parsing

## §1 Scope + framing

Lens: `@error`, `@recover`, incremental parsing, diagnostics, LSP document state, DAP stepping, and cookbook/error-message obligations. Restart README places error recovery, incremental parsing, and VM debug in the PASS-3 layer (`restart/README.md:344-348`), and dispatch authority adds `@error` and `@layout` as in-scope BBNF extensions.

Phase-1 parallel assumption: PASS-1/PASS-2 syntheses are not consumed as authority under this dispatch. This report treats existing `analysis`, `lsp`, DAP, and playground code as source references for user-surface needs, not as code to edit.

## §2 Pro / Con / Explication / Challenge ledger

| Item | Explication | Pro | Con | Challenge | Verdict |
| --- | --- | --- | --- | --- | --- |
| `@error` grammar directive | Dispatch authority says `@error` is in. It should define grammar-specific diagnostic surfaces and expected spans/messages. | Gives authors a declarative way to shape user diagnostics. | Can become an ad hoc semantic action language. | Keep it declarative: codes, labels, notes, recovery expectations, no rewrite-mode behavior. | KEEP |
| `@recover` recovery directive | Existing analysis extracts `@recover` blocks and diagnostics (`crates/analysis/src/directives/recover.rs:10-37`, `crates/analysis/src/directives/recover.rs:39-77`), and hover docs already explain it (`crates/analysis/src/features/hover/directive.rs:22-44`). | Preserves concrete prior work and lets LSP/playground show recoverable parse states. | Old implementation reparses full documents after edits. | Lift directive semantics into generated metadata and incremental snapshots. | REINVENT |
| `@layout` directive | Dispatch authority says `@layout` is in, and README anchors layout near `@error` (`restart/README.md:172-176`). | Needed for formatter, diagnostics, and source-preserving edits. | Layout can bleed into grammar semantics. | Treat layout as trivia/formatting metadata consumed by visitors/formatter, not parse acceptance unless grammar says so. | KEEP |
| Rewrite-mode | Prompt/README stale text includes rewrite-mode (`restart/README.md:473`, `restart/prompts/PASS-1-SUBSTRATE.md:31`). | None for PASS-3. | Conflicts with settled authority and would blur mutation/recovery semantics. | Explicitly route rewrite-like tasks through visitor edit builders and formatter/layout. | DISCARD |
| Full reparse on LSP change | Current LSP applies incremental text edits then calls `on_change`, which reparses and publishes diagnostics (`crates/lsp/src/server/protocol.rs:82-109`, `crates/lsp/src/server/mod.rs:56-80`). | Simple and currently functional. | Does not satisfy restart incremental parsing ambition. | Keep incremental text edit application, reinvent document analysis around parse snapshots and affected ranges. | REINVENT |
| Current analysis document state | `DocumentState::update` replaces text and reparses AST/diagnostics (`crates/analysis/src/state/mod.rs:55-83`). | Clear state model to inherit. | Reanalysis cost grows with document size. | Split text storage, parse snapshot, diagnostics, semantic index, and stale-region queue. | REINVENT |
| DAP parse stepping | DAP already maps breakpoints and produces debug events (`crates/lsp/src/dap/mod.rs:121-143`, `crates/lsp/src/dap/mapping.rs:41-92`). | Strong ecosystem differentiator. | Debug traces can conflict with incremental snapshots. | Tie debug sessions to immutable snapshot IDs. | KEEP |

## §3 Incremental parsing model

PASS-3 should specify a snapshot-based model:

```rust
pub struct DocumentSnapshot<'input> {
    id: SnapshotId,
    text: Rope,
    tape: Tape<'input>,
    diagnostics: DiagnosticSet,
    semantic: SemanticIndex,
}

pub struct IncrementalEdit {
    range: TextRange,
    replacement_len: usize,
    new_text_hash: Hash,
}

pub enum ReparsePlan {
    Reuse { unchanged: Vec<TapeRange> },
    Reparse { dirty: Vec<TextRange>, anchors: Vec<TapeId> },
}
```

Current code already separates edit application from change handling: `did_change` applies LSP edits then calls `on_change` (`crates/lsp/src/server/protocol.rs:82-109`), and import helper code handles range-based text replacement (`crates/lsp/src/server/imports.rs:143-178`). Restart should keep that entry shape but replace full document parse with affected-range parsing and snapshot reuse.

## §4 Diagnostics contract

Diagnostics must be grammar-derived, stable, and exact. Style guidance says user-facing text should be polished and not over-explain implementation (`docs/precepts/instructions/STYLE.md` was required reading); cookbook materials provide concrete examples for lifetime and path diagnostics (`docs/cookbook/lifetime-surfaces.md:69-89`, `docs/tranches/BB/audit/W5-pointer-syntax-decision.md:92-128`).

Proposed restart-wide diagnostic families:

```text
error[BBNF-RECOVER001]: recovery rule `name` cannot consume input
help: add a token, synchronization set, or explicit empty-recovery marker

error[BBNF-ERROR001]: @error directive references unknown rule `rule`
help: define the rule or attach the directive to an existing production

warning[BBNF-LAYOUT001]: @layout directive is unused by generated formatter
help: attach it to a rule, token, or trivia class consumed by the formatter

warning[BBNF-INCR001]: edit invalidated the full parse snapshot
help: enable trace logging to inspect the missing incremental anchor
```

The warning form for full-snapshot invalidation should be dev/trace output, not end-user LSP spam.

## §5 LSP, playground, and DAP commitments

1. **Language server consolidation.** Historical audit recommended consolidating analysis, LSP, and DAP into `bbnf-language-server` (`restart-archive-2026-05-04/audit/passes/PASS-C.md:90-92`) and deleting old extension/server packaging split points (`restart-archive-2026-05-04/audit/passes/PASS-C.md:158-159`). Restart should follow that consolidation.

2. **Document service.** `bbnf-language-server` owns a `DocumentService` with rope text, parse snapshots, diagnostics, semantic index, import graph, and debug sessions. Old `analysis` and `lsp` concepts are kept as internal modules, not separate crates unless SYNTHESIS chooses otherwise.

3. **Playground API.** Existing wasm exposes `compile`, `parse`, `lspBatch`, and `debugStep` (`playground/src/composables/wasm/index.ts:233-256`, `playground/src/composables/wasm/index.ts:274-322`). Restart should preserve these user concepts but back them with generated grammar metadata and tape snapshots.

4. **VS Code extension.** Current extension starts a language client and DAP adapter (`extension/src/extension.ts:30-55`, `extension/src/extension.ts:80-100`). Restart should treat the extension as a thin packaged client over `bbnf-language-server`.

## §6 Cross-pass hand-offs

PASS-1 must supply recoverable tape nodes, node identity stable enough for snapshot reuse, dirty-range anchors, and span maps that survive edit application. Recovery/layout flags belong in tape metadata.

PASS-2 must emit recovery tables, `@error` diagnostic metadata, layout metadata, formatter hooks, and incremental parse anchors. It must not implement rewrite-mode; rewrite-like behavior routes through visitor edit builders.

SYNTHESIS must align the incremental model with PASS-1's final tape layout and PASS-2's parser strategy. PASS-3 only demands the user-visible behavior: low-latency edits, stable diagnostics, debug snapshots, and no reparse-only design as the end state.

## §7 Risk + mitigation

Risk: incremental parsing becomes over-promised before substrate support exists. Mitigation: expose a traceable `ReparsePlan` that can fall back to full parse while benchmarks report fallback rates.

Risk: recovery nodes pollute typed APIs. Mitigation: generated root accessors distinguish valid nodes from recovered placeholders, while visitors and diagnostics expose recovery explicitly.

Risk: DAP/debug snapshots retain too much memory. Mitigation: cap retained snapshots and make trace payloads optional.

## §8 Verdict

KEEP `@error`, `@recover`, `@layout`, DAP stepping, LSP concepts, and range edit application. REINVENT current full-reparse analysis into snapshot-based incremental parsing. DISCARD rewrite-mode and any design that treats incremental edit handling as sufficient without incremental parse reuse.

## Wave 2 correction note

`@recover` is folded into `@error(recover = ...)`; standalone `@recover` survives only as a legacy alias for migration parsers, not as a new V1 grammar surface. Incremental fallback is now bound to dataset-level thresholds at PASS-3.md §5 (JSON corpus >= 85 percent reuse / <= 5 percent fallback; CSS corpus >= 75 percent / <= 10 percent; BBNF self-edit >= 70 percent / <= 15 percent). LSP user-facing output is silent on fallback by default; debug channel reports under `BBNF_LSP_DEBUG=1`. The layout diagnostic strings `BBNF-LAYOUT001` and `BBNF-LAYOUT002` are committed verbatim at PASS-3.md §6b.
