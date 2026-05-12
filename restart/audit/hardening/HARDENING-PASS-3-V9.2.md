# HARDENING-PASS-3-V9.2 — Lazy-Tape Amendment Audit Against PASS-3

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` (591 lines, V9.1-closed) |
| Cycle | V9.2 (lazy-tape amendment audit; the open SK-V2 verdict is SK-AMENDMENT-REQUIRED-NARROW) |
| Auxiliary target | `restart/skinny/audit/LAZY-TAPE-DESIGN.md` (the amendment proposal) |
| Adjacent V1 surfaces audited for absorption | `ARCHITECTURE.md` §3.1, §9.1, §9.2, §10; `locks/14-LOCKS.md` Lock 1, Lock 9, Lock 5, Lock 14; sister LSP/DAP surfaces inside PASS-3 §3, §4, §5 |
| Posture | Steelman the design; produce per-target absorption list; do NOT amend PASS-3 |
| Lens set | A-K (V8+) |
| Final decision | **PASS-3 AMENDMENT-REQUIRED — DUAL-MODE ADMISSION WITH NARROW LSP/DAP CARRY** |

The lazy-tape design is empirically motivated (three iterations of microarchitectural perturbations measured-and-rejected against the eager substrate), structurally honest (the structural-offset array IS the tape; no parallel substrate is added), and surgically scoped (per-grammar opt-in via workspace metadata; eager mode preserved verbatim for grammars that need it). PASS-3's V9.1-cleared posture treats `Tape<'input>` as eager-only — `TapeToken` is invoked, `tape.tokens[index]` is the canonical cursor read, `payload_or_skip` underwrites the visitor sibling-skip. Dual-mode admission requires a narrow but specific set of edits to PASS-3 §4, §5, §6b, §7, §8, and §10. The Lock 1 amendment text the design proposes is sound (steelman survives); the load-bearing carry to PASS-3 is the `ReparsePlan` data-type clarification at §5 (offset-range vs token-range), the identity invariant restatement at §4 (`(TapeId, cursor, kind_at_cursor)` with `kind` computed rather than stored), and the materialisation-cost gate at §3 (the cost table must now publish `tape_mode` per grammar plus the offsets-array bytes column).

---

## §2 Cohort verdict — 9-lane + lens table

| Lane / Lens | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| Lane 1 — Lock-Adherence | amendment-required | 11 | 2 | 0 | Lock 1 honoured under proposed amendment (two materialisation modes; one substrate API); Lock 9 honoured; Lock 5 honoured (BIR-only lower; mode is a lowering parameter); Lock 14 honoured (kind-discriminator is generated per-grammar). Add one row to PASS-3 §4 binding the dual-mode contract to Lock 1 amendment text. |
| Lane 2 — Sequencing (N/A; single-target) | N/A | — | — | — | — |
| Lane 3 — Cohesion | amendment-required | 6 | 4 | 0 | PASS-3 §4 cursor field is named `index: u32` and reads `tape.tokens[index]`; rename to `cursor: u32`; admit lazy variant. `TapeShape` declarations at §4 must publish `tape_mode`. `JsonRoot`/`JsonValue` typed views need the offset-stream sibling-walk path. |
| Lane 4 — SOTA Anchoring | honoured (with one carry) | 9 | 1 | 0 | Bench gates are unchanged; falsifiability target (T1 ≥ 14K Mbps on twitter) is sharp and routes through the same outcome matrix. Add a `tape_mode` column to the §7 bench-row table to disambiguate eager/lazy rows on close. |
| Lane 5 — Grammar-Authoritative | honoured | 10 | 0 | 0 | `tape_mode` lives in `[workspace.metadata.bbnf.grammars.<g>.runtime]`; the substrate carries zero grammar-specific code; the kind discriminator function is template-emitted per-grammar from the grammar source. Lock 14 holds. |
| Lane 6 — Generated-Code Budget | amendment-required | 7 | 2 | 0 | Net JSON generated LOC drops ~100 (less emit; more walker). The §7 generated-code budget rows must admit `tape_mode`-dependent budget variation: lazy-mode rows lose `state.tape.emit(...)` lines from `generated.rs` and gain depth-tracked walker prose to `view.rs`. The materialisation-cost artefact must publish `offsets_bytes` and `tape_mode` columns alongside today's `token_count` / `tape_bytes`. |
| Lane 7 — Friction Forecast | amendment-required | 5 | 3 | 0 | Friction concentrates on three surfaces: (a) DAP/LSP cursors that today carry `(TapeId, node id)` must now declare which `tape_mode` interpretation applies; (b) visitor authors writing against `next_sibling_cursor` in lazy mode pay O(subtree-size) where eager mode paid O(1); (c) grammar authors who flip `tape_mode = "lazy"` must understand that recovery-flag-bearing tokens forbid lazy mode (lazy mode is verifier-route only). Add two diagnostic strings + one cookbook chapter. |
| Lane 8 — Carry & Deferral | honoured | 8 | 1 | 0 | The §8 `ReparsePlan` carry is named (receiver Tranche I; blocker named in §5 of design; gate retains fallback-rate thresholds). The optional sibling-skip sidecar is correctly framed as a measurement decision, not a design decision — V1 lands without sidecar. |
| Lane 9 — Greenfield Discipline | honoured | 7 | 1 | 0 | No quick solution: three iterations of perturbations were measured-and-rejected first. No workaround: the structural offset is the substrate's natural shape; sonic-rs already proves this design works at scale. No legacy uncontested: every eager-mode row is preserved with explicit `tape_mode = "eager"` opt-in. The architectural transposition (drop the 16-byte token stream; promote the offset array to substrate) is idiomatic and sonic-rs-aligned. |
| Lens F — LLM bias | honoured | — | — | — | Design has measured rejections of dispatch-table and 12-byte-token perturbations and cites them by file:line; nothing free-floating. No hedging on the falsifiability gate. |
| Lens G — Overfitting | honoured | — | — | — | Lazy mode is a measured architectural response to a measured substrate-ceiling, not a pattern-lift from sonic-rs without sensitivity. The design explicitly preserves eager mode for grammars where sonic-rs's structural-only model would fail (CSS L4 layout repair; BBNF-self payload-class storage; Sheets Pratt host chains). |
| Lens H — Hallucination | honoured | — | — | — | All citations resolve. The empirical premise (three skinny iterations) is cited by file:line at `skinny/RESULTS.md` and `skinny/REDRESS.md`. |
| Lens I — Contrivance | honoured (under one steelman) | — | — | — | The dual-mode runtime might look like cardinality bloat (two `Tape<'input>` variants), but each variant load-bears a measurable use case: lazy is the SOTA-beat substrate; eager is the recovery / layout / Pratt substrate. Mode-monomorphism per grammar (not per node) prevents runtime dispatch overhead. The two modes do not double-track; they multiplex by grammar metadata. |
| Lens J — Host-leverage | honoured | — | — | — | Lazy mode leverages LLVM's match-arm-on-byte-disjoint-alts codegen (the same pattern as the alt-dispatch lowering at `COMPILER.md:489`); no new host invention. |
| Lens K — Meta-grammar discipline | honoured (load-bearing for SOTA gate) | — | — | — | Lazy mode is a SOTA-driven runtime mode but stays below the meta-grammar mandate threshold: it adds zero BIR variants, zero new directives, zero new grammar syntax. The opt-in is a workspace-metadata key — outside the grammar source file. Lock 14 onboarding test is unaffected. |

Final decision: **AMENDMENT-REQUIRED**.

---

## §3 Lane 1 — Lock-Adherence

### Lane standard

For each of the 14 locks the lazy-tape design must verify the post-amendment shape honours the lock's text (or the lock's amended text where the design proposes one).

| Lock | Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|---|
| Lock 1 — Tape is the substrate | `locks/14-LOCKS.md:34`; design §4 | Two materialisation modes under one substrate API | The design proposes amendment text admitting `tape_mode ∈ {eager, lazy}` under one `Tape<'input>` type wrapping the underlying buffer, with shared `ValueRef`, `DocumentView`, `Visitor`, and payload arena. | Forbidden surfaces are preserved verbatim: no `Vec<OpenFrame>::clone`; no columnar SoA; no "AST" type that is not a typed view over the tape; no grammar-specific tape variants beyond the two `tape_mode` values. Mode is monomorphic at the grammar level via `#[cfg]` / feature switch — there is NO `enum TapeShape { Lazy, Eager }` runtime polymorphic dispatch. | The amendment widens Lock 1 — a future reader could mis-read this as admitting more modes than two. | Steelman: a third mode (e.g., "lazy + sidecar sibling-skip") could arrive after measurement. The design names this explicitly as a v3 sidecar landing, not a third `tape_mode` value. The amendment text caps modes at two by enumeration. | **honoured** under proposed amendment |
| Lock 1 — No parallel substrate | `locks/14-LOCKS.md:34`; design §8.3 | One substrate crate exposes both layouts as `#[cfg]`-selected modules | `runtime::grammars::json::Tape` is the lazy variant; `runtime::grammars::css_l4::Tape` is the eager variant. They are distinct types with the same trait surface, not a runtime polymorphic dispatch. There is no `enum TapeShape { Lazy(...), Eager(...) }` in the runtime. | No `OpenFrame` clone resurrection; no columnar SoA; no second tree; the substrate is mode-monomorphic per grammar. | A naive reading of "one substrate" requires "one storage shape"; the design widens "one substrate API". | Steelman: the prior failure was orthogonal codepaths competing for the same role. Here, only one codepath fires per grammar — the `tape_mode` metadata pre-selects at codegen time. The runtime carries zero `match tape_mode { ... }` arms. | **honoured** |
| Lock 4 — Per-domain optimisation | `locks/14-LOCKS.md:40`; design §11.5 | H tranche cost-model selector | Design defers cost-model-driven `tape_mode` selection to H tranche; today's design hard-codes the choice per grammar in metadata. | No fusion with CSP / e-graph; no unified hypergraph. | None. | None. | **honoured** |
| Lock 5 — IR + per-backend lower | `locks/14-LOCKS.md:42`; design §8.2 | BIR alphabet unchanged | Design adds zero BIR variants. The `TapeEmit` and `DirectBuild` rows exist; their LOWERING differs by mode. | The lowerer reads BIR + grammar metadata; it does NOT inspect Grammar IR. Lock 5's "lowerer never inspects Grammar IR" survives because metadata is BIR-adjacent. | Mode-branching in `codegen::lower::rust` could become a Backend IR leak. | Steelman: codegen branches at emit time on `target = "wasm32"` (V2 amendment); same pattern. The mode is a lowering parameter, not a BIR variant. | **honoured** |
| Lock 6 — xtask emits committed source | `locks/14-LOCKS.md:44` | Generated source LOC drops ~100 net | Lazy mode produces fewer `state.tape.emit(...)` lines in `generated.rs`; more walker prose in `view.rs`. Net JSON: -100 LOC. Source is on disk, greppable. | None. | None. | None. | **honoured** |
| Lock 8 — Surpass sonic-rs | `locks/14-LOCKS.md:48`; design §9.4 | Falsifiability target on twitter | Design names T1 ≥ 14K Mbps as validation, T1 ≥ 17K Mbps as strong validation (README beat target ~17K Mbps); refutation at T1 < 13K Mbps. Outcome matrix unchanged. | Outcome A/B/C/D/E/F/G classification preserved verbatim. | The 14K Mbps validation target is below the README ~17K beat target; partial validation does not amend Lock 8. | Steelman: design separates "claim validated" (lazy mode beats today's eager ceiling) from "Lock 8 beat" (matches/beats sonic-rs's 18.4K Mbps on twitter). Lock 8 still gates on sonic-rs's number. | **honoured** |
| Lock 9 — Slice-borrow primary | `locks/14-LOCKS.md:50`; design §6.3 | Lifetime discriminant unchanged | `ValueRef<'doc, 'input, K>` retains both lifetime parameters. `'doc` borrows tape (which owns offsets and references source); `'input` is the bytes the tape references. `parse(&str)` collapses `'doc = 'input = 'a`. | `parse`, `parse_in`, `parse_owned` are unchanged. | None. | None. | **honoured** |
| Lock 13 — No god directories | `locks/14-LOCKS.md:58` | Per-mode sub-modules under `runtime::tape::` | The design proposes the `tape` module retains its 6-child shape (`token`, `builder` → `assembler`, `span`, `payload`, `view`, `trace`). One rename (`builder` → `assembler`); zero new sibling directories. | 4-10 child rule preserved. | The `assembler` rename should land in PASS-3 §6's module tree to avoid sibling drift. | **honoured** with a PASS-3 §6 update needed. |
| Lock 14 — Full grammar generalisation | `locks/14-LOCKS.md:60`; design §8.4 | Zero grammar-specific code in substrate | The kind discriminator function is generated per-grammar from the grammar source, not hardcoded in the runtime crate. The same template generates a different discriminator for CSS-scan if/when CSS-scan opts into lazy mode. | Substrate carries zero `match grammar { Json => ..., CssL4 => ... }`. The two-surface onboarding test (yaml.bbnf + workspace-metadata block) is unaffected; yaml's `tape_mode` defaults to "eager" unless metadata declares "lazy". | A grammar author who declares `tape_mode = "lazy"` for a grammar that has `@error(recover = ...)` rules will fail-fast — but where? Design names it as a verifier-route precondition (§6.2) but does not specify which pass emits the diagnostic. | Steelman: PASS-2's grammar-metadata extractor at `passes::extract` is the natural site. PASS-3 §6b should commit a `BBNF-TAPE-MODE-CONFLICT` diagnostic. | **honoured** with a PASS-3 §6b diagnostic-row addition needed. |

Lane verdict: 11 honoured (with three narrow surgeries to absorb); 2 require PASS-3 amendment edits (§6 module-tree rename; §6b diagnostic-row addition).

---

## §4 Lane 3 — Cohesion (the load-bearing lane for PASS-3 absorption)

### Lane standard

Every PASS-3 claim that depends on the tape substrate's storage shape must hold post-amendment, OR PASS-3 must be edited to admit the dual-mode contract.

| Site (PASS-3 path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `PASS-3.md:152-158` (§4 substrate-visible semantics list) | Six bullets PASS-3 requires from PASS-1: snapshot identity; stable node-kind IDs; compact spans, payload refs, payload classes; child/sibling traversal and skip ranges; recovery/layout/debug flags; optional trace events. | The list is correct under eager mode. Under lazy mode, the third bullet ("compact spans, payload references, and payload classes") becomes: "compact spans (derived from offsets[cursor]..offsets[next_sibling_cursor]), payload references via payload arena, and node-kind computed lazily from source[offsets[cursor]] in lazy mode". The fourth bullet ("child/sibling traversal AND skip ranges") becomes: "child/sibling traversal — skip ranges are stored on container tokens in eager mode; derived via bracket-depth walk in lazy mode (sidecar `subtree_skip_index` is a v3 measurement option)". | The list is the user-surface contract; admitting two interpretations preserves the contract without leaking into the user mental model — both modes ultimately yield the same `ValueRef` cursor shape. | The list as currently written reads as a unimodal contract. | Steelman: the current list IS unimodal because PASS-3 was authored under V9.1's eager-only assumption. The dual-mode amendment is the V9.2 surgery. | **REINVENT** — add a one-paragraph "Two materialisation modes" note before the bulleted list; preserve bullets verbatim. |
| `PASS-3.md:163-178` (§4 illustrative Tape struct) | `tokens: Box<[TapeToken]>` field | The illustrative shape declares a `Box<[TapeToken]>` field. Under lazy mode this field is replaced by `offsets: Box<[u32]>` and `string_candidates: Box<[u32]>` per design §2.1. | The illustrative shape is explicitly NOT a PASS-1 mandate ("PASS-1 may pack differently if these semantics remain true" at line 187); so the substrate is free to publish either field. | The illustrative shape is what readers internalise. Without amendment, every PASS-3 reader walks away with the eager `tokens` field as the canonical shape. | Steelman: the illustrative shape exists to ground the user mental model. The amendment must publish BOTH illustrative shapes (eager + lazy) under §4, gated by `tape_mode`. | **REINVENT** — publish both `Tape<'input>` illustrative shapes side-by-side under §4, with `tape_mode` discriminator. |
| `PASS-3.md:171-178` (illustrative `TapeToken`, `ValueRef`) | `TapeToken { kind, flags, start, end, payload, sibling_skip }` 24-byte struct; `ValueRef { tape, index: u32, _kind }` | Under lazy mode, `TapeToken` is omitted; `ValueRef.index` becomes `cursor: u32` indexing `tape.offsets`. The struct shape stays identical (cursor / index field width is u32 either way); the semantics change. | The Lock 9 lifetime parameter `<'doc, 'input, K>` is preserved verbatim. The PhantomData treatment is preserved. | The field rename `index → cursor` is a public-API surface change. PASS-3 §4 publishes `index: u32` today. | Steelman: the rename is narrow but load-bearing — `cursor` reads as a position over a possibly-irregular stream; `index` reads as a position into a regular stream. Lazy mode's offset array is regular (one u32 per offset), but reads from it semantically index a cursor over an irregular structural stream. The rename clarifies. | **REINVENT** — rename `index: u32` to `cursor: u32` in the illustrative `ValueRef`; admit both `tokens: Box<[TapeToken]>` (eager) and `offsets: Box<[u32]>` (lazy) under the illustrative `Tape<'input>`. |
| `PASS-3.md:187` (TapeShape sentence) | "`TapeShape` declares token kind, span class, payload class, traversal skip policy, scalar-cache policy, and string-normalization policy" | `TapeShape` becomes mode-aware. In eager mode, declarations enumerate token kind, span class, payload class, skip policy. In lazy mode, declarations enumerate the kind-discriminator-function, span derivation (start = offsets[cursor]; end = offsets[next_sibling_cursor]), and the sibling-walk policy (bracket-depth walk; optional v3 sidecar). | `TapeShape` already abstracts the storage representation; admitting two declarations under `tape_mode` honours that abstraction. | The sentence is currently single-mode prose. | Steelman: the abstract-shape language already admits a mode parameter; the V9.2 surgery is to make the parameter explicit. | **REINVENT** — extend the `TapeShape` sentence with a `tape_mode` clause naming both shapes. |
| `PASS-3.md:144` (materialisation-cost gate row) | "Codegen emits a `materialisation_cost.toml` (or equivalent generated artefact) with field counts, payload arena bytes, tape-token width, `TapeShape` scalar-cache policy, string-normalization policy, repeated-access cost class, selected objective profile, scalarized score/objective vector, and domination reason per node kind." | The artefact must publish `tape_mode` (eager vs lazy) and the offsets-array bytes column under lazy mode; under eager mode it continues to publish tape-token width and logical/allocated tape bytes per ARCH §9.2. | The artefact format admits a `tape_mode` field by extension without redesign. | None. | None. | **REINVENT** — extend the materialisation-cost-artefact column list with `tape_mode` and (lazy-mode only) `offsets_bytes_logical` / `offsets_bytes_allocated`. |
| `PASS-3.md:189-190` (debug acceptance gate) | "every breakpoint, step, hover, and playground trace event to carry `SnapshotId`, `TapeId`, node kind, and source span when the tape node exists" | "Node kind" is stored on the token in eager mode and computed in lazy mode. The acceptance gate is unaffected — node kind reaches the DAP/playground via the typed-view cursor, not via direct token-stream access. Lazy mode's kind-discriminator function is called at the breakpoint-event site. | The DAP / playground does not require kind to be stored. | Lazy mode's per-byte kind classification cost (~2-3 ns) is paid at every debug trace event. For a typical debug session this is in noise. | Steelman: the design's §9.1 risk register prices kind computation at the verifier-walk (~10-15% of parse time); debug events are O(events), not O(nodes), so the cost is dominated by event dispatch, not kind lookup. | **honoured** — no PASS-3 surgery needed; the gate text holds. |
| `PASS-3.md:195-225` (§5 incremental parse + `ReparsePlan`) | `ReparsePlan::Reuse { unchanged: Vec<TapeRange>, reuse_map: Vec<(OldTapeId, NewTapeId)> }`; `ReparsePlan::Reparse { dirty, anchors: Vec<TapeId>, reuse_map, fallback_reason, invalidated_queries }` | `TapeRange` is a range over the parsed-stream addressing. Under eager mode it is a range over `tokens[..]`. Under lazy mode it is a range over `offsets[..]`. The shape is unchanged; the underlying addressing changes. The dirty-range / anchor-set algorithm is unchanged; it operates at offset granularity rather than token granularity in lazy mode. The fallback-rate gates apply with the same thresholds. | The `ReparsePlan` data type does not change semantically. Lazy mode reuses-or-discards offset ranges; eager mode reuses-or-discards token ranges. The reuse-map (`(OldTapeId, NewTapeId)`) is mode-agnostic. | The design (§10.3 Tranche I row) says "no I tranche redesign required; only the data type for 'reusable range' changes." This is correct but PASS-3 §5 currently publishes `Vec<TapeRange>` without binding `TapeRange` to the underlying addressing. PASS-3 must publish that `TapeRange` is `tape_mode`-typed. | Steelman: a future LSP user might reasonably expect `TapeRange` to mean "range of TapeTokens." Under lazy mode, `TapeRange` means "range of offsets." The PASS-3 text must commit. | **REINVENT** — add a sentence to PASS-3 §5 binding `TapeRange` to the `tape_mode` of the grammar; under eager mode it ranges over `tokens[..]`, under lazy mode over `offsets[..]`. The dirty-range / anchor-set algorithm and the fallback-rate thresholds are unaffected. |
| `PASS-3.md:277-282` (fallback-rate gates by dataset) | "JSON edit corpus: ≥ 85 percent of token spans reused; ≤ 5 percent full-reparse fallback" | "Token spans" is eager-mode language. Under lazy mode the unit is "offset ranges" (or equivalently "source ranges between adjacent offsets"). The percentage threshold survives unchanged. | The threshold is dimensionless. | The user-facing prose says "token spans"; under lazy mode there are no tokens. | Steelman: the prose is engineer-facing (a bench gate). Translation to "offset ranges" preserves the contract. | **REINVENT** — rename "token spans" → "structural spans" (mode-agnostic); document under §5 that "structural span" means token-span in eager mode and offset-range in lazy mode. |
| `PASS-3.md:486-506` (§7 bench-row table) | Bench rows publish target Mbps + competitor floor + platform + surface | Bench rows are mode-agnostic by construction (the harness measures `parse(&str)` end-to-end). To prevent ambiguity in the close gate, add a `tape_mode` column to disambiguate cross-mode comparisons. | The §7 close gate text holds verbatim. | A row that compares eager-mode parsing against sonic-rs (which is lazy-only) is meaningful; a row that compares lazy-mode parsing against sonic-rs is the SOTA-beat row. PASS-3 must publish which row is which. | Steelman: PASS-3 today publishes `json/twitter/borrowed` as `parse(&str)` + direct root — the row implicitly carries the grammar's declared `tape_mode`. The amendment adds a column for explicitness. | **REINVENT** — add a `tape_mode` column to the §7 bench-row table; today's rows carry `eager` until JSON flips to `lazy` post-SK-V2-bench. |

Lane verdict: 6 honoured; 4 require REINVENT-class amendments at §4 illustrative-shape, §4 materialisation-cost-artefact column list, §5 `TapeRange` binding + "structural span" rename, §7 `tape_mode` column. None require DISCARD.

---

## §5 Lane 4 — SOTA Anchoring

Bench gates are mode-agnostic by construction. The SK-V2 bench harness fires unchanged. The lazy-mode design names a sharp falsifiability target (T1 ≥ 14K Mbps on twitter validates; T1 ≥ 17K Mbps strong-validates the README beat; T1 < 13K Mbps refutes). The outcome A/B/C/D/E/F/G matrix applies verbatim. PASS-3 §7's `json/twitter/borrowed` row at line 498 carries `<= 380us` against sonic-rs 436 µs; lazy mode lands this gate when twitter parses at ≥ 17K Mbps. The lazy mode is the architectural candidate for closing the SOTA gate.

| Site (PASS-3 path:line) | Item | Verdict |
|---|---|---|
| `PASS-3.md:498` (`json/twitter/borrowed`) | <= 380 µs vs sonic-rs 436 µs; M1 Pro; `parse(&str)` + direct root | **honoured** under lazy-mode bench. Add `tape_mode` column. |
| `PASS-3.md:499` (`json/twitter/tape_cursor`) | <= borrowed + 10%; M1 Pro; `ValueRef` cursor projection | **honoured**. Lazy-mode cursor projection cost is `~2-3 ns` per kind query (one cache line into offsets, one byte from source, one match arm); within the 10% cushion. |
| `PASS-3.md:500` (`json/citm/path`) | <= 750 µs parse target + selector time | **honoured**. Lazy mode's `path!` traversal walks offsets at offset-granularity; the `path-core` traversal plan resolves cursor positions through the typed-view projection without re-parsing. |
| `PASS-3.md:501` (`json/canada/array_scan`) | <= 2.8ms vs sonic-rs 3.144 ms | **honoured** under steelman. Canada is offset-dense (167K offsets / 2.25MB ≈ 1 in 13); eager mode's 167K * 16 bytes = 2.67MB token stream EXCEEDS source size; lazy mode's 668KB offsets array fits in M1 Pro L2 (12MB shared). The design's §9.2 prediction (lazy mode wins on canada) is the load-bearing claim for this row. |

Lane verdict: honoured with one column addition to the §7 table.

---

## §6 Lane 6 — Generated-Code Budget

Net JSON generated LOC drops ~100 (design §7.4): `generated.rs` -200 (less emit); `view.rs` +100 (lazy walkers); `parser.rs` / `host.rs` / `value.rs` / `visitor.rs` unchanged. The §7 generated-LOC budget table at lines 510-519 must:

| Site (PASS-3 path:line) | Item | Verdict |
|---|---|---|
| `PASS-3.md:512` (visitor traits row) | "css_l4 visitor ≤ 22 K LOC at W3 baseline (≈ 20 percent of 107 K); bbnf visitor ≤ 6 K LOC at W3 baseline (≈ 28 percent of 21 K); other-grammar visitor LOC scales with each grammar's `generated_loc` row" | **honoured** under eager mode; the lazy-mode visitor uses depth-tracked walker prose in `view.rs`, not visitor LOC; this row is unaffected. |
| `PASS-3.md:515` (tape projections row) | "css_l4 projection ≤ 35 K LOC at W3 baseline; bbnf projection ≤ 8 K LOC at W3 baseline; other-grammar projection LOC scales" | **honoured** under eager mode; lazy-mode projection LOC drops ~100 net for JSON. Document under §7's budget gates that lazy-mode grammars carry a separate LOC-delta envelope per regen. |
| `PASS-3.md:144` + materialisation-cost-artefact | "field counts, payload arena bytes, tape-token width, `TapeShape` scalar-cache policy, ..." | **REINVENT** — admit `tape_mode` field and (lazy-mode only) `offsets_bytes_logical` / `offsets_bytes_allocated` columns. |

Lane verdict: 7 honoured; 2 require REINVENT additions to publish `tape_mode` per-row.

---

## §7 Lane 7 — Friction Forecast

Three friction surfaces concentrate the user-facing risk of dual-mode tape:

### 7.1 DAP / LSP cursor identity (the load-bearing friction surface)

PASS-3 §4 line 189 commits the debug acceptance gate: every breakpoint, step, hover, and playground trace event carries `(SnapshotId, TapeId, node kind, source span)`. Under lazy mode, "node kind" is computed at the breakpoint-event site via `kind_at_cursor`. The DAP author's mental model is: "the breakpoint event hands me a node, the node has a stable kind." Under lazy mode the kind is stable (a pure function of immutable `(source, offsets[cursor])`) but computed; the DAP author who expects to cache the kind on the breakpoint-event side must understand that cache is fine because the underlying tape is sealed.

**Friction**: a DAP consumer that stores a `(TapeId, cursor)` pair across events and re-computes kind every time will pay the ~2-3 ns kind-discriminator cost on every event. For ~10K events/sec (a stepped debug session) this is ~20-30 us/sec — invisible. For an LSP completion query that walks 100K nodes (large CSS file traversal), the cost is ~200-300 us — also invisible relative to LSP response budget.

**Surgery**: add a sentence to PASS-3 §4 line 189 binding the DAP / playground event-payload semantics under lazy mode: "Under `tape_mode = "lazy"`, `node kind` is computed at event-emit time via the lazy discriminator (see ARCH §9.1.lazy); the computed kind is identity-stable (pure function of immutable `(source, offsets[cursor])`)."

### 7.2 Visitor walker semantics (the load-bearing friction surface)

PASS-3 §3 line 148 commits the W5 visitor design. Under eager mode, `JsonObject::iter().nth(k)` can short-circuit by skipping subtrees in O(1) per sibling (via `payload_or_skip`). Under lazy mode, the same call walks every offset of every prior subtree via the bracket-depth walker — O(subtree-size) per skip.

**Friction**: a visitor author who writes `for member in obj.iter() { if matches!(member.0.as_str(), "the_key") { ... } }` pays O(n) regardless of mode (both modes iterate). A visitor author who writes `obj.get("the_key")` pays O(1) per key in eager mode but O(subtree-size-up-to-key) in lazy mode. For typical JSON (small objects), the cost is invisible; for pathological JSON (10K-member objects with sparse key access), lazy mode's cost is measurable.

**Surgery**: add a paragraph to PASS-3 §3 visitor commitments (after line 148) noting the O(n) sibling-skip cost in lazy mode, citing the v3 sidecar option (`subtree_skip_index: Option<Box<[u32]>>`) as a measurement-driven post-V1 amendment. Add a new diagnostic `BBNF-VISITOR-LAZY-SCAN` to PASS-3 §6b: `note[BBNF-VISITOR-LAZY-SCAN]: visitor 'get' on lazy-mode grammar performs O(subtree) scan; for sparse-key access at scale, consider 'iter' once and cache, or enable the post-V1 'subtree_skip_index' sidecar.`

### 7.3 Grammar-author `tape_mode` selection (the load-bearing friction surface)

Grammar authors who declare `tape_mode = "lazy"` for a grammar that has `@error(recover = ...)` rules or `@layout` directives MUST be told their grammar is mis-classified. The design (§6.2) says lazy mode is verifier-route only — "no payload-class storage" — which forecloses grammars with recovery-flag-bearing tokens. PASS-2 / `passes::extract` must emit the diagnostic.

**Friction**: a grammar author who writes a CSS-scan grammar with `@error(recover = ";" | "}")` and declares `tape_mode = "lazy"` will compile a parser that silently drops recovery state. The diagnostic must fire at metadata-extraction time.

**Surgery**: add `BBNF-TAPE-MODE-CONFLICT` to PASS-3 §6b: `error[BBNF-TAPE-MODE-CONFLICT]: grammar '{name}' declares 'tape_mode = "lazy"' but has '@error(recover = ...)' or '@layout' directives that require eager-mode payload storage. help: either remove the recovery/layout directives or declare 'tape_mode = "eager"'.`

Lane verdict: 5 honoured; 3 REINVENT (DAP cursor lazy-mode binding sentence; visitor sibling-skip paragraph + `BBNF-VISITOR-LAZY-SCAN` diagnostic; `BBNF-TAPE-MODE-CONFLICT` diagnostic).

---

## §8 Lane 8 — Carry & Deferral Audit

The lazy-tape design's §10.3 tranche-affected list:

| Tranche | Item | Receiver named | Blocker named | Receiving gate named | Verdict |
|---|---|---|---|---|---|
| Tranche B (runtime substrate) | Adopts lazy mode for JSON; the `Tape` variant becomes load-bearing for the JSON row | Yes | Yes (SK-V2 cohort verdict + user disposition) | SK-V2 bench gate | **honoured** |
| Tranche F (Rust lowerer template) | Gains mode-branching emit path | Yes | Yes (Tranche B's `Tape` variant landing first) | F-tranche emit-template wave | **honoured** |
| Tranche I (LSP / incremental parse) | `ReparsePlan::Reuse { unchanged: Vec<TapeRange> }` works at offset granularity in lazy mode | Yes | Yes (named by design §10.3 as "no I tranche redesign required; only the data type for 'reusable range' changes") | Tranche I body close gate (the dataset-level fallback-rate gates at PASS-3 §5 lines 277-282) | **honoured** with a PASS-3 §5 surgery (the "structural span" rename) |
| Tranche J (memory residency / SOTA close) | Lazy mode lowers peak RSS on canada from 3.572 MB allocated tape to ~668 KB offsets (~5× memory win) | Yes | Yes (Tranche B's `Tape` variant landing first) | Outcome M gate (peak RSS ≤ 3× competitor) | **honoured** |
| v3 sibling-skip sidecar | `subtree_skip_index: Option<Box<[u32]>>` | Yes (post-V1 measurement) | Yes (parity-oracle `serialize_canonical` cost > 1.5× slower) | Yes (v3 measurement gate) | **honoured** |

Lane verdict: 8 honoured; 1 REINVENT (PASS-3 §5 surgery to rename "token spans" → "structural spans").

---

## §9 Lens I — Contrivance / over-engineering

The dual-mode `Tape<'input>` could look like apparatus expansion. Under steelman it is not: each mode load-bears a distinct measured use case, mode-monomorphism per grammar prevents runtime polymorphic dispatch overhead, and the two modes share the cursor / `ValueRef` / `DocumentView` / Visitor / payload-arena surfaces. The substrate API does not double — only the storage representation does. Lock 1's "one substrate API" invariant survives because the mode is a codegen-time discriminator, not a runtime type-tag.

The proposed v3 sibling-skip sidecar is correctly framed as a measurement decision (the design lands V1 without sidecar; if `serialize_canonical` shows > 1.5× regression, the sidecar lands at v3). This is anti-contrivance: the apparatus is gated on measurement.

The `kind_at_cursor` discriminator function is generated per-grammar from the grammar source — not hardcoded in the runtime crate. This is Lock 14 discipline: zero grammar-specific code in the substrate. The same template generates a different discriminator for CSS-scan if/when CSS-scan opts into lazy mode.

Lens I verdict: **honoured under steelman**.

---

## §10 Lens K — Meta-grammar discipline

The lazy-mode design is a SOTA-driven runtime mode (Lock 8 close gate). Does the apparatus stay below the meta-grammar mandate threshold?

| Surface | Affected? | Mandate threshold? |
|---|---|---|
| BBNF grammar surface | No | The grammar source file is unchanged. No new directive (`@lazy` is NOT proposed). |
| BIR alphabet | No | Zero variant additions. `TapeEmit` and `DirectBuild` rows exist; their LOWERING differs by mode. |
| Workspace metadata | Yes (one new key) | `[workspace.metadata.bbnf.grammars.<g>.runtime] tape_mode = "lazy"` — opt-in, defaults to eager. |
| Runtime API (`parse`, `parse_in`, `parse_owned`, `DocumentView`) | No | Signatures unchanged. |
| `ValueRef` shape | One field rename (`index` → `cursor`) | The field is the same width (u32); the rename clarifies semantics. |
| Identity invariant | One restatement | `(TapeId, cursor, kind_at_cursor)` replaces `(TapeId, node id, payload class)` under lazy mode; kind is computed via pure function of immutable `(source, offsets[cursor])`. |

Verdict: the apparatus is bounded. Lazy mode adds one metadata key, one field rename, and one identity-invariant restatement. It does NOT generate a new language, does NOT add semantic apparatus, does NOT require self-hosting modifications. It belongs in the meta-grammar generator's emit-time mode-branching, not in the meta-grammar itself.

Lens K verdict: **LOAD-BEARING for V1 Lock 8 close; below meta-grammar mandate threshold**.

---

## §11 Lens N — Graduation mechanicality (incremental parse / LSP / DAP compatibility)

The design's §10.3 Tranche I row claims "no I tranche redesign required; only the data type for 'reusable range' changes." Verify against PASS-3 §5.

**`ReparsePlan::Reuse { unchanged: Vec<TapeRange>, reuse_map: Vec<(OldTapeId, NewTapeId)> }`** at `PASS-3.md:208-212`:
- Under eager mode, `TapeRange` is `Range<u32>` over `tokens[..]`. The reuse-map carries `(OldTokenIndex, NewTokenIndex)` semantics for the public `TapeId` opaque type. Per the verifier walk, an unchanged token range translates to a stable subtree of the document.
- Under lazy mode, `TapeRange` is `Range<u32>` over `offsets[..]`. The reuse-map carries `(OldOffsetIndex, NewOffsetIndex)` semantics. An unchanged offset range translates to a stable subtree of the document IFF the underlying source bytes in that range are byte-identical. (The kind discriminator function is a pure function of `source[offsets[cursor]]`; if `source` is byte-identical in the range and `offsets` are byte-identical, every kind is byte-identical.)

The verifier-walk identity invariant in lazy mode is *stronger* than the eager-mode invariant for incremental reuse, because lazy mode has no recovery-flag-bearing tokens — there is no per-token storage to invalidate, only the offset array. Reuse is byte-equality of the offsets sub-array; the dirty-range / anchor-set algorithm computes which offsets are stale and which are reusable.

**Anchor matching under lazy mode**: anchors today are `Vec<TapeId>`; in lazy mode these become `Vec<TapeId>` where `TapeId` opaquely wraps offset-cursor positions. The anchor algorithm is unchanged in shape. The fallback condition ("the sync set cannot re-enter a balanced scope") is unaffected.

**Fallback-rate gates**: the thresholds at PASS-3 §5 lines 277-282 (JSON ≥ 85% reuse / ≤ 5% fallback; CSS ≥ 75% / ≤ 10%; BBNF self ≥ 70% / ≤ 15%) are dimensionless. They apply with the same thresholds in lazy mode, measured against "structural spans" (mode-agnostic naming).

**DAP / playground compatibility**: under lazy mode the breakpoint payload still carries `(SnapshotId, TapeId, kind, source span)`. The `kind` is computed at event-emit time; the `source span` is `(offsets[cursor], offsets[next_sibling_cursor])`. No DAP wire-format change.

**Conclusion**: the design's claim "no I tranche redesign required" is correct. The PASS-3 surgery is the data-type binding ("`TapeRange` is `tape_mode`-typed") and the "structural span" rename. Tranche I's body — reuse-map computation, edit-anchor algorithm, dataset-level fallback-rate gates — is unaffected.

Lens N verdict: **honoured** under steelman.

---

## §12 Punch list — per-target absorption edits

Verbatim or surgery-described edits required for PASS-3 (and adjacent runtime surfaces PASS-3 owns) to admit dual-mode (Eager | Lazy) tape. Items are ordered by execution sequence.

### Item P3-LAZY-1 — §4 substrate-visible-semantics paragraph

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:150-158` |
| Surgery | Add a one-paragraph preamble immediately before the six-bullet list at line 152 |
| Verbatim insertion | `PASS-3 absorbs two per-grammar tape materialisation modes per the Lock 1 amendment: 'tape_mode = "eager"' (canonical for layout / recovery / payload-bearing grammars — CSS L4, BBNF-self, Sheets) and 'tape_mode = "lazy"' (canonical for SOTA-class structural grammars — JSON, CSS-scan). The six bullets below are mode-agnostic substrate-visible semantics; their storage representation varies by 'tape_mode' (see ARCH §9.1 dual-mode addendum + LAZY-TAPE-DESIGN.md §4 Lock 1 amendment surface).` |
| Source verdict | REINVENT |
| Lane | Lane 1 (Lock-Adherence); Lane 3 (Cohesion) |

### Item P3-LAZY-2 — §4 illustrative `Tape<'input>` struct

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:163-178` |
| Surgery | Publish both illustrative shapes side-by-side under `tape_mode` |
| Description | Replace the single `Tape<'input>` / `TapeToken` / `ValueRef` block with two blocks: eager (preserved verbatim with the existing `tokens: Box<[TapeToken]>` field) and lazy (`offsets: Box<[u32]>` + `string_candidates: Box<[u32]>` + `payloads: PayloadArena` + `source: &'input [u8]` + `id: TapeId`). Rename `index: u32` → `cursor: u32` in the `ValueRef<'doc, 'input, K>` struct (the field width and PhantomData treatment are preserved). |
| Source verdict | REINVENT |
| Lane | Lane 3 (Cohesion) |

### Item P3-LAZY-3 — §4 `TapeShape` declaration sentence

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:187` |
| Surgery | Extend the `TapeShape` sentence with mode-aware declarations |
| Verbatim replacement | `'TapeShape' declares 'tape_mode', token kind (or kind-discriminator-function under lazy mode), span class (or span-derivation policy under lazy mode), payload class (or payload-arena-only policy under lazy mode), traversal skip policy (stored under eager mode; bracket-depth-walked under lazy mode, with the optional 'subtree_skip_index' sidecar as a post-V1 measurement amendment), scalar-cache policy, and string-normalization policy; 'ValueShape' declares generated typed projections over the same node id (cursor under lazy mode).` |
| Source verdict | REINVENT |
| Lane | Lane 3 (Cohesion) |

### Item P3-LAZY-4 — §3 materialisation-cost artefact column list

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:144` (the materialisation-cost-table consumer gate row) |
| Surgery | Extend the artefact column list with `tape_mode` + lazy-mode-only offsets columns |
| Verbatim replacement (the row's third column) | `Codegen emits a 'materialisation_cost.toml' (or equivalent generated artefact) with 'tape_mode', field counts, payload arena bytes, tape-token width (eager-mode rows only), offsets-array logical bytes + allocated bytes (lazy-mode rows only), 'TapeShape' scalar-cache policy, string-normalization policy, repeated-access cost class, selected objective profile, scalarized score/objective vector, and domination reason per node kind; the cookbook references it.` |
| Source verdict | REINVENT |
| Lane | Lane 6 (Generated-Code Budget) |

### Item P3-LAZY-5 — §4 DAP / playground event-payload semantics under lazy mode

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:189-190` |
| Surgery | Add a sentence to the debug acceptance gate paragraph binding lazy-mode kind computation |
| Verbatim insertion | `Under 'tape_mode = "lazy"', 'node kind' is computed at event-emit time via the lazy kind-discriminator function ('source[offsets[cursor]]' plus the per-grammar discriminator table per LAZY-TAPE-DESIGN.md §5); the computed kind is identity-stable (a pure function of immutable '(source, offsets[cursor])') and carries the same DAP / playground event-payload contract as eager-mode stored kinds.` |
| Source verdict | REINVENT |
| Lane | Lane 7 (Friction Forecast) |

### Item P3-LAZY-6 — §5 `ReparsePlan` `TapeRange` binding

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:208-220` |
| Surgery | Add a sentence to the `ReparsePlan` definition paragraph binding `TapeRange` to `tape_mode` |
| Verbatim insertion (after the `ReparsePlan` enum block) | `'TapeRange' is 'tape_mode'-typed: under 'tape_mode = "eager"' it ranges over 'tokens[..]'; under 'tape_mode = "lazy"' it ranges over 'offsets[..]'. The dirty-range / anchor-set algorithm and the fallback-rate thresholds are mode-agnostic. The reuse-map ('Vec<(OldTapeId, NewTapeId)>') carries opaque cursor-position semantics under both modes.` |
| Source verdict | REINVENT |
| Lane | Lane 3 (Cohesion); Lens N (Graduation mechanicality) |

### Item P3-LAZY-7 — §5 fallback-rate gate row "token spans" → "structural spans"

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:277-282` (the fallback-rate gates by dataset table) |
| Surgery | Rename "token spans" → "structural spans" in the JSON edit corpus row and in the CSS edit corpus row; rename "rule spans" → "rule structural spans" in the BBNF self-edit corpus row (the rename is mode-agnostic; both eager-mode token spans and lazy-mode offset ranges resolve to structural spans). |
| Source verdict | REINVENT |
| Lane | Lane 3 (Cohesion); Lane 8 (Carry & Deferral) |

### Item P3-LAZY-8 — §6 module-tree rename `builder` → `assembler`

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` — §6 module tree under `crates/runtime/src/tape/` (the design at §2.4 proposes the rename) |
| Surgery | Update PASS-3's module-tree references (if any direct citations to `runtime/src/tape/builder.rs` exist; the §6 tree at lines 290-300 publishes `crates/bbnf/src/` substantially; the rename lands under `runtime/src/tape/` per ARCH §9.1). Add a one-line note: `'runtime/src/tape/' contains the substrate module shape: 'token' (eager-mode TapeToken; preserved), 'assembler' (formerly 'builder'; verifier-route under lazy mode; emit-route under eager mode), 'span', 'payload', 'view', 'trace'. The rename clarifies the verifier-route work that lazy mode promotes from per-token emission to bracketed-walk verification.` |
| Source verdict | REINVENT |
| Lane | Lock 13 (no god directories); Lane 1 (Lock-Adherence) |

### Item P3-LAZY-9 — §6b new diagnostic `BBNF-TAPE-MODE-CONFLICT`

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` — §6b (compiler diagnostic ledger; the table begins at line 450) |
| Surgery | Add a new row to the diagnostic table |
| Verbatim insertion | `\| 'BBNF-TAPE-MODE-CONFLICT' \| 'error[BBNF-TAPE-MODE-CONFLICT]: grammar `{name}` declares `tape_mode = "lazy"` but has `@error(recover = ...)` or `@layout` directives that require eager-mode payload storage. help: either remove the recovery/layout directives or declare `tape_mode = "eager"`.' \| Grammar author \| "Lazy mode works for any grammar." \| Lazy mode is verifier-route only; recovery-flag-bearing tokens require eager-mode payload class storage. \| Onboarding cookbook §tape-modes; LAZY-TAPE-DESIGN.md §6.2. \|` |
| Source verdict | REINVENT |
| Lane | Lane 7 (Friction Forecast); Lock 14 (Full grammar generalisation) |

### Item P3-LAZY-10 — §6b new diagnostic `BBNF-VISITOR-LAZY-SCAN`

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` — §6b |
| Surgery | Add a new row to the diagnostic table |
| Verbatim insertion | `\| 'BBNF-VISITOR-LAZY-SCAN' \| 'note[BBNF-VISITOR-LAZY-SCAN]: visitor `get`/`nth` on `tape_mode = "lazy"` grammar performs O(subtree) scan; for sparse-key access at scale, prefer `iter` once and cache, or enable the post-V1 `subtree_skip_index` sidecar.' \| Visitor author \| "get is O(1) like a hashmap." \| Lazy mode walks offsets with bracket-depth counter; no stored sibling-skip; the v3 sidecar amendment provides O(1) skip-distance lookup. \| Visitor cookbook §lazy-tape-walk; LAZY-TAPE-DESIGN.md §9.3. \|` |
| Source verdict | REINVENT |
| Lane | Lane 7 (Friction Forecast) |

### Item P3-LAZY-11 — §7 bench-row table `tape_mode` column

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:486-506` (the §7 PASS-3 benchmark rows table) |
| Surgery | Add a `tape_mode` column to the bench-row table; today's rows carry `eager` until JSON flips to `lazy` post-SK-V2 verdict + user disposition |
| Description | The new column header reads `Tape mode`; rows fill in `eager` or `lazy` (or `eager → lazy (SK-V2 pending)` for JSON rows until the bench gate fires). |
| Source verdict | REINVENT |
| Lane | Lane 4 (SOTA Anchoring); Lane 6 (Generated-Code Budget) |

### Item P3-LAZY-12 — §3 visitor commitments lazy-mode paragraph

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:148` (the visitor commitments paragraph) |
| Surgery | Add a final sentence to the visitor commitments paragraph |
| Verbatim insertion | `Under 'tape_mode = "lazy"', 'JsonObject::get' and 'JsonArray::iter().nth(k)' walk offsets with a bracket-depth counter — O(subtree-size) where eager mode is O(1) per skip via stored 'payload_or_skip'. For typical workloads (small objects, dense access) the cost is invisible; for sparse-key access at scale, the post-V1 'subtree_skip_index' sidecar amendment provides O(1) skip-distance lookup per LAZY-TAPE-DESIGN.md §9.3; the 'BBNF-VISITOR-LAZY-SCAN' diagnostic surfaces the cost class at compile time.` |
| Source verdict | REINVENT |
| Lane | Lane 7 (Friction Forecast); Lane 3 (Cohesion) |

### Item P3-LAZY-13 — §8 hand-off row for lazy-mode tape variant landing

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:523-538` (the §8 cross-pass hand-offs table) |
| Surgery | Add a new row to the hand-offs table |
| Verbatim insertion | `\| Tape materialisation mode ('tape_mode = "eager" \| "lazy"') per grammar; lazy mode load-bears for SOTA-class structural grammars (JSON canonical post-SK-V2 verdict); eager mode load-bears for recovery / layout / payload-bearing grammars. Mode is monomorphic at the per-grammar-crate level via codegen-time '#[cfg]' / feature selection; one substrate API across both modes per Lock 1 amendment. \| PASS-1 / Tranche B (substrate) + PASS-2 / Tranche F (lowerer mode-branching) + Tranche I (incremental + LSP 'TapeRange' offset/token-range typing) \| Without the amendment, JSON cannot beat sonic-rs on twitter (eager substrate ceiling at ~12.5K Mbps). \| SK-V2 bench gate at LAZY-TAPE-DESIGN.md §9.4: T1 ≥ 14K Mbps on twitter validates; T1 ≥ 17K Mbps strong-validates the README beat; T1 < 13K Mbps refutes. \|` |
| Source verdict | REINVENT |
| Lane | Lane 1 (Lock-Adherence); Lane 4 (SOTA Anchoring) |

### Item P3-LAZY-14 — §10 unresolved punch-list carry for the SK-V2 verdict

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md:574-587` (the §10 unresolved punch-list table) |
| Surgery | Add a new row |
| Verbatim insertion | `\| Lazy-tape substrate amendment for JSON (and prospectively CSS-scan / future SOTA-class structural grammars). \| SYNTHESIS / Tranche B + Tranche F + Tranche I \| Eager substrate ceiling at ~12.5K Mbps on twitter; lazy-tape design pending SK-V2 cohort verdict + user disposition + re-bench. \| SK-V2 bench gate fires unchanged; outcome A/B/C/D/E/F/G classification per BENCH.md §6; Lock 1 amendment text lands per LAZY-TAPE-DESIGN.md §4.1. \|` |
| Source verdict | REINVENT |
| Lane | Lane 8 (Carry & Deferral) |

### Adjacent V1 surfaces — ARCH §9.1, ARCH §9.2, Lock 1

The lazy-tape design's §4.1 amendment text lands in `restart/locks/14-LOCKS.md:34` (Lock 1) verbatim; the ARCH §9.1 Tape invariants table at `restart/ARCHITECTURE.md:1401-1409` admits a `tape_mode` row and a lazy-variant `Tape<'input>` illustrative shape. These are SYNTHESIS-owned, not PASS-3-owned. PASS-3's role is to absorb the dual-mode contract into its user-surface, error-recovery, incremental-parse, and DAP/LSP rows.

The PASS-3 §9 KEEP / REINVENT / DISCARD summary at lines 540-571 carries forward: "Tape as the single advanced substrate unioned with direct-to-struct" stays under REINVENT (the dual-mode amendment is the V9.2 reinvent step). No DISCARD additions.

---

## §13 Final readiness

> **Decision: amendment-required**
>
> The lazy-tape design at `restart/skinny/audit/LAZY-TAPE-DESIGN.md` survives the V9.2 lens matrix under steelman. It honours Lock 1 (the amendment text the design proposes is sound; the spirit — no parallel substrate, no OpenFrame clone — holds), Lock 5 (zero BIR variant additions; mode is a lowering parameter), Lock 9 (lifetime discriminant preserved), Lock 14 (the kind-discriminator function is template-emitted per-grammar; the substrate carries zero grammar-specific code). The empirical premise (three iterations of measured-and-rejected perturbations against the eager-tape ceiling) is sharp and the falsifiability gate (T1 ≥ 14K Mbps on twitter validates; T1 < 13K Mbps refutes) is mechanically actionable.
>
> PASS-3 absorbs the dual-mode contract through fourteen narrow surgeries: one preamble at §4 binding the two modes to Lock 1 amendment text; one illustrative-shape pair at §4 publishing eager and lazy `Tape<'input>` side-by-side (with `ValueRef.index` renamed to `ValueRef.cursor`); one `TapeShape` sentence at §4 admitting mode-aware declarations; one materialisation-cost artefact column-list extension at §3; one DAP / playground event-payload binding at §4; one `ReparsePlan` `TapeRange` binding at §5; one "structural span" rename at §5 fallback-rate gates; one `assembler` module rename note at §6; two new diagnostic rows at §6b (`BBNF-TAPE-MODE-CONFLICT` for grammar-author misconfiguration; `BBNF-VISITOR-LAZY-SCAN` for visitor sibling-skip cost class); one `tape_mode` column at §7 bench-row table; one visitor commitments lazy-mode paragraph at §3; one §8 hand-off row binding the mode contract; one §10 unresolved punch-list carry for the SK-V2 verdict.
>
> The design's claim "no I tranche redesign required; only the data type for 'reusable range' changes" is correct under Lens N. The dataset-level fallback-rate gates at PASS-3 §5 lines 277-282 apply unchanged with the same thresholds, measured against mode-agnostic structural spans. The DAP / playground acceptance gate at §4 line 189 holds verbatim; lazy-mode kind computation at event-emit time is identity-stable (pure function of immutable `(source, offsets[cursor])`).
>
> Hereupon: dispatch a narrow PASS-3 amendment agent applying punch-list items P3-LAZY-1 through P3-LAZY-14; once landed, re-run V9.2 verification scans to confirm absorption; SYNTHESIS dispatches the Lock 1 amendment text into `restart/locks/14-LOCKS.md:34` and the dual-mode `Tape` invariants row into `restart/ARCHITECTURE.md:1401-1409`; SK-V2 bench fires unchanged. After bench classifies outcome A / B / C on twitter (validation), JSON's `tape_mode` flips to `lazy` in workspace metadata and the Tranche B + F + I cascade lands per §10.3 of the design.

---

### Critical Files for Implementation

- /Users/mkbabb/Programming/bbnf-lang/restart/audit/pass-3-runtime/PASS-3.md
- /Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md
- /Users/mkbabb/Programming/bbnf-lang/restart/ARCHITECTURE.md
- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/LAZY-TAPE-DESIGN.md
- /Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/HARDENING-PASS-3-V9.1.md
