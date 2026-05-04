# Agent 4 - Tape Union Architect

## §1 Scope + framing

Lens: runtime substrate exposure at the user layer, tape/direct-to-struct union, cursor identity, debug stepping, and compatibility with SOTA on-demand parser lessons. The settled authority is explicit: tape is the substrate, properly unioned with direct-to-struct; columnar SoA and parallel substrates are dead; do not rename tape to `ParseStream`. This report therefore marks `ParseStream` prompt clauses as stale (`restart/prompts/PASS-3-RUNTIME.md:3`, `restart/prompts/PASS-3-RUNTIME.md:79-81`) and follows Lock 1 (`restart/locks/14-LOCKS.md:34`).

Phase-1 parallel assumption: PASS-1/PASS-2 syntheses are not consumed as authority under this dispatch. This report states the PASS-3 user/runtime requirements that SYNTHESIS must route into PASS-1 substrate and PASS-2 codegen.

## §2 Pro / Con / Explication / Challenge ledger

| Item | Explication | Pro | Con | Challenge | Verdict |
| --- | --- | --- | --- | --- | --- |
| Tape as public advanced substrate | Tape stores parse events/nodes, spans, payload references, skip data, and stable kind IDs. README already says runtime design is tape plus direct-to-struct union (`restart/README.md:285-314`). | Matches simdjson on-demand lessons, supports debug/path/LSP without reparsing (`restart/corpora/SOTA.md:64-77`). | Old SOTA notes once recommended against tape (`restart/corpora/SOTA.md:205-214`). | Lock 1 and dispatch authority supersede the old anti-tape recommendation; keep tape lean and typed. | REINVENT |
| Direct-to-struct roots | Generated ASTs remain the default authoring surface. | Best ergonomics for normal grammar users. | Can diverge from tape if identity is not shared. | Every direct node carries tape ID/span and can project to a `ValueRef`. | KEEP |
| `ParseStream` public name | PASS-3 prompt and inheritance index refer to `ParseStream` as the keystone (`restart/prompts/PASS-3-RUNTIME.md:79-81`, `restart/inheritance/INDEX.md:65-66`). | It was an attempt to avoid old baggage. | Dispatch authority says do not rename tape; stale name would conflict with Lock 1. | Mention only as stale prompt conflict; do not emit APIs with that name. | DISCARD |
| Columnar SoA substrate | Earlier substrate explorations considered parallel/columnar shapes. | Could help analytic scans. | User authority declares columnar SoA and parallel substrates dead. | Do not leave future extension hooks that imply a second substrate. | DISCARD |
| On-demand value cursor | `ValueRef` traverses tape lazily and can materialize direct structs or borrowed scalars. | Preserves sonic/simdjson on-demand wins and enables generic tools (`restart/corpora/SOTA.md:35-42`, `restart/corpora/SOTA.md:64-77`). | Cursor lifetime and invalidation rules must be clear. | Cursor validity is tied to document snapshot identity; incremental edits create new snapshots. | KEEP |
| Debug trace over tape | Existing playground/wasm and DAP expose debug stepping (`playground/src/composables/wasm/index.ts:274-322`, `crates/lsp/src/dap/mod.rs:21-35`). | Tape identity can unify debugger, path, and diagnostics. | Trace payloads can bloat parse memory. | Keep trace spans/events optional and compile/runtime gated. | REINVENT |
| Single mutation path | README mandates mutation through visitors (`restart/README.md:318`). | Keeps tape/direct identity coherent. | Users may want `value.set(...)`. | Expose edit builders and owned visitors instead of arbitrary value mutation. | KEEP |

## §3 Tape union model

Recommended PASS-3-facing model:

```rust
pub struct Tape<'input> {
    source: &'input str,
    tokens: Box<[TapeToken]>,
    payloads: PayloadArena,
    diagnostics: Box<[Diagnostic]>,
}

#[repr(C)]
pub struct TapeToken {
    kind: NodeKindId,
    flags: NodeFlags,
    start: u32,
    end: u32,
    payload: u32,
    sibling_skip: u32,
}

pub struct ValueRef<'doc, 'input, K = AnyKind> {
    document: DocumentId,
    tape: &'doc Tape<'input>,
    index: u32,
    _kind: PhantomData<K>,
}
```

This is a user-surface contract, not final PASS-1 layout. PASS-1 owns exact packing and ABI. PASS-3 requires only these semantic capabilities:

1. Stable `DocumentId`/snapshot identity.
2. Stable node kind IDs shared with codegen metadata.
3. Cheap span and payload lookup.
4. Child/sibling traversal for path/select/visitor.
5. Recovery and layout flags.
6. Optional trace/debug event ranges.

## §4 Direct-to-struct union details

Direct roots should not be separate from tape. The generated `Document` owns or borrows both:

```rust
pub struct Document<'arena, 'input> {
    tape: Tape<'input>,
    root: RootNode<'arena, 'input>,
    metadata: &'static GrammarMetadata,
}
```

For small or token-heavy grammars, PASS-2 may choose direct-to-struct first and derive tape indexes during construction. For tool-facing modes, PASS-2 may build tape first and lazily project direct nodes. The public invariant is the same either way: every visible node has a tape identity, and every tape node can be viewed through `ValueRef` or an untyped `Value`.

This is where the stale README conflict matters. `restart/README.md:473` says "typed-enum + slice-borrow + ParseStream union" and includes stale extension language; the same document earlier settles lookbehind/rewrite/Unicode differently (`restart/README.md:123`, `restart/README.md:133-143`) and names the tape/direct union (`restart/README.md:285-314`). PASS-3 should cite line 473 as stale and resolve to the earlier settled sections plus dispatch authority.

## §5 Debug/runtime ecosystem hooks

The current DAP implementation already has server, session, breakpoint, and trace concepts (`crates/lsp/src/dap/mod.rs:45-83`, `crates/lsp/src/dap/mod.rs:121-143`, `crates/lsp/src/dap/mod.rs:153-180`), while mapping code translates line/offset breakpoints (`crates/lsp/src/dap/mapping.rs:41-92`). The playground already calls `debugStep` and receives snapshots from wasm (`playground/src/composables/wasm/index.ts:274-322`, `playground/src/composables/wasm/types.ts:166-183`). Restart should unify these on tape events:

```rust
pub enum TraceEvent {
    Enter { node: TapeId, rule: RuleId, span: Span },
    Exit { node: TapeId, rule: RuleId, span: Span },
    Recover { node: TapeId, diagnostic: DiagnosticId },
    HostCall { span: Span, host_fn: HostFnId },
}
```

Trace events must be optional; normal parse benchmarks should not pay for full debug payloads.

## §6 Cross-pass hand-offs

PASS-1 must define tape token packing, payload arenas, span widths, sibling skip representation, recovery/layout/debug flags, and snapshot identity. It must not rename tape to `ParseStream`, even though PASS-1 prompt wording still contains that stale term (`restart/prompts/PASS-1-SUBSTRATE.md:3`, `restart/prompts/PASS-1-SUBSTRATE.md:66`).

PASS-2 must generate direct structs carrying tape identity, `ValueRef` projections, metadata kind tables, and optional trace instrumentation. It should treat host functions as metadata-bound primitives, not per-grammar crates (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:36-56`).

PASS-3 ecosystem crates must consume tape identity consistently: path/select plans, visitor pruning, diagnostics, LSP semantic features, DAP stepping, playground inspection, and benchmark attribution all refer to the same node IDs.

## §7 Risk + mitigation

Risk: tape becomes a slow, bloated universal IR. Mitigation: keep token layout compact, trace optional, and direct-to-struct available for hot grammar-specific paths.

Risk: direct structs and tape disagree. Mitigation: generated constructors must assert or test node identity round-trips in fixtures and benches.

Risk: old anti-tape SOTA text causes design churn. Mitigation: explicitly mark it as superseded by Lock 1 and the dispatch authority in PASS-3 synthesis.

## §8 Verdict

KEEP direct-to-struct roots, on-demand `ValueRef`, tape-backed debug, and visitor-mediated mutation. REINVENT tape as the single advanced substrate unioned with direct structs. DISCARD `ParseStream` as a public name, columnar SoA, parallel substrates, and arbitrary mutable value handles.

## Wave 2 correction note

Tape identity now carries an explicit field/method delta budget at PASS-3.md §7 (<= 1 field plus 2 methods per regen; larger deltas open a named amendment). The bbnf aggregator child-count repair at PASS-3.md §6 places `tape/` and `value/` as cohesive children alongside `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, and `metadata/`; tape projection LOC counts against the runtime module budget.
