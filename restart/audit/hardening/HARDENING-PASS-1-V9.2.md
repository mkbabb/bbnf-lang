# HARDENING-PASS-1-V9.2 — Lazy-Tape Amendment Absorption Audit

## §1 Target identification

| Field | Value |
|---|---|
| V9.2 audit subject | `restart/skinny/audit/LAZY-TAPE-DESIGN.md` (845 lines) |
| Target surface | `restart/audit/pass-1-substrate/PASS-1.md` (361 lines, post-V9.1) |
| Adjacent surfaces audited | `restart/locks/14-LOCKS.md` Lock 1 (line 34); `restart/ARCHITECTURE.md` §7.2 (BIR alphabet, lines 900-1008), §9.1 (Tape invariants, 1388-1410), §12.2 (per-grammar matrix, 1605-1654), §3.1 (parse API, 184-224) |
| Prior cycle | V9.1 `AMENDMENT-REQUIRED-NARROW` (single residue: PASS-1:212 stale-prompt allusion). |
| V9.2 charge | Audit whether the lazy-tape proposal is coherent with the V1 corpus surfaces PASS-1 owns, and produce the V1 amendment punch list. |
| Lens set | A-K (V8+ contract per `restart/prompts/HARDENING.md:194`). |
| Time consumed | ~35 min of the 40-min cap. |

## §2 Cohort verdict — 9-lane table

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | honoured-with-amendment | 8 | 1 (Lock 1) | 0 | Lock 1 verbatim amendment per design §4.1 is load-bearing. Other locks survive. |
| 2 Sequencing | honoured | n/a | n/a | n/a | Single-PASS audit; multi-wave sequencing not in scope. |
| 3 Cohesion | partial | 4 | 3 | 0 | PASS-1 §9.1-adjacent commitments need synchronization with offsets-as-tape. |
| 4 SOTA | honoured | 1 | 0 | 0 | Proposal adds no Lock-8-gates; sk-side falsifiability targets cited per `skinny/RESULTS.md:5-7`. |
| 5 Grammar-authoritative | partial | 6 | 2 | 0 | `tape_mode` metadata key needs Lock-14 fence text. Per-grammar matrix at ARCH §12.2 needs a `tape_mode` column or sentence. |
| 6 LOC budget | partial | 0 | 1 | 0 | Per-grammar generated-LOC budget at PASS-1 §8 punch + ARCH §12.2 needs lazy-mode deltas (proposal cites -100 LOC on JSON). |
| 7 Friction forecast | honoured-with-amendment | 0 | 1 | 0 | New diagnostic surface needed: lazy-mode subtree-walk surprise; `BBNF-TAPE-MODE-CONFLICT` (eager grammar declaring recovery + lazy mode). |
| 8 Carry/deferral | partial | 2 | 2 | 0 | Two carries to V1 H tranche (cost-model `tape_mode` override) and V1 I tranche (offset-range `ReparsePlan`) need receiver/blocker/gate triples. |
| 9 Greenfield | honoured | n/a | n/a | n/a | The design is a transposition for performance (Lock 9 mandate). It rejects parallel substrates explicitly. |
| F LLM bias | mostly honoured | — | — | — | One residue: §9.1 prediction "lazy mode should land **14-16K Mbps on twitter**" is pseudo-precise without provenance for the cache-model derivation; flagged but not blocker. |
| G Overfitting | honoured | — | — | — | Lazy-mode is a measured perturbation, not a SOTA-mimetic transplant. |
| H Hallucination | honoured | — | — | — | Every cited `path:line` resolves (verified `RESULTS.md:5-7`, `REDRESS.md:158-188`, `REDRESS.md:295-301`). |
| I Contrivance | partial | — | — | 1 | The `subtree_skip_index: Option<Box<[u32]>>` sidecar (proposal §9.3) is correctly deferred — keep deferred, do not absorb into PASS-1 yet. |
| J Host-language leverage | honoured | — | — | — | LLVM compiles the byte-discriminator to a jump table (proposal §5.1 cites `COMPILER.md:489`); proposal leans on host correctly. |
| K Meta-grammar discipline | honoured | — | — | — | The dual-mode dispatch is grammar-data-driven via workspace metadata, not invented apparatus. LOAD-BEARING for the SOTA aspiration; ASPIRATIONAL for V1 correctness (eager mode suffices for correctness on all extant grammars). |

**Final decision: AMENDMENT-REQUIRED-NARROW.** The proposal is architecturally coherent with PASS-1. The amendment surface is well-bounded; ~11 surgical edits (per §12) absorb it into the V1 corpus. The proposal correctly steelmans its own Lock-1 amendment text. The remaining V9.1 residue (PASS-1:212 stale-prompt allusion) is unchanged in V9.2 scope but acknowledged.

## §3 Lock-adherence verdict (per-lock summary)

| Lock | Verdict | Notes |
|---|---|---|
| Lock 1 | REINVENT per proposal §4.1 verbatim | The single load-bearing structural edit. Steelman of strict reading (keep single-mode) fails: SK-V2 evidence pins eager-tape ceiling at 60-65% sonic-rs; spirit (no parallel substrate; no OpenFrame clone) preserved verbatim in amended text. |
| Lock 2 | KEEP | Layout pass unaffected; `passes::layout` remains public boundary; mode read downstream at `passes::extract`. |
| Lock 3 | KEEP | Cursor + byte-skip unified; `__EAGER_EMPTY_PATH` elision is path-consumer concern. |
| Lock 4 | KEEP | Per-domain orthogonal optimization unaffected; CSP/egraph/cost-model do not inspect substrate layout. |
| Lock 5 | KEEP (with PASS-1 amendment naming `tape_mode` as metadata-derived BIR-extraction parameter) | BIR alphabet (20 rows) unchanged; lowerer reads BIR + metadata; metadata is BIR-adjacent, not Grammar-IR. |
| Lock 6 | KEEP | xtask emits committed source; generated `generated.rs` shrinks ~200 LOC and `view.rs` grows ~100 LOC under lazy JSON. |
| Lock 7 | KEEP (with PASS-1 line amendment per P3) | path crate consumes `ValueRef`; `cursor: u32` substitutes for `index: u32` byte-equally. |
| Lock 8 | KEEP | SOTA gates measured at H.W3/J.W1 unchanged; proposal adds no new Lock-8 throughput gates. |
| Lock 9 | KEEP | Slice-borrow primary preserved verbatim. `parse(&str)`, `parse_in(&str, &Arena)`, `parse_owned` survive. Owned wrapper holds Tape whose `offsets: Box<[u32]>` is owned but borrows source from wrapper. |
| Lock 10 | KEEP | Pratt + SIMD auto-detection unaffected; structural-scan column unchanged. |
| Lock 11 | KEEP | path-deps unaffected; sister crates (`egraph`, `csp-solver`, `parse-that`) not touched. |
| Lock 12 | KEEP | Archive ceremony was A-precondition; unaffected. |
| Lock 13 | KEEP | No god directories; the runtime tape module gains lazy-mode children (`assembler.rs`); directory remains 4-10 children. Mild risk: `view.rs` ~350 LOC under 500. |
| Lock 14 | KEEP (with explicit fence at PASS-1 + ARC §12.2 per P5 + P6) | Dispatch is on `tape_mode` not grammar name. `kind_at_cursor` template-generated per grammar. `rg -nP 'match \w+ { Json => ... }'` returns ZERO. |

**Lane 1 verdict: honoured-with-amendment.** Lock 1 is the only REINVENT. Eight other locks KEEP outright; five carry minor PASS-1-text amendments.

## §4 Cross-quadrant disposition

- Lane 2: N/A (single-PASS audit)
- Lane 3 (Cohesion): partial — 5 surgical edits required (P1-P4 plus V9.1 P11 carry)
- Lane 4 (SOTA): KEEP — proposal correctly silent on inventing new gates
- Lane 5 (Grammar-authoritative): partial — `tape_mode` metadata schema needs explicit ARC §12.2 column + WORKSPACE.md schema entry
- Lane 6 (LOC): partial — JSON generated LOC reprojection at ARC §12.2 footnote
- Lane 7 (Friction): partial — `BBNF-TAPE-MODE-CONFLICT` diagnostic needed
- Lane 8 (Carry): partial — receiver/blocker/gate triples for `ReparsePlan` offset-range and cost-model `tape_mode` override
- Lane 9 (Greenfield): KEEP — transposition for performance per Lock 9 mandate
- Lens I (contrivance): not contrivance; dual-mode is grammar-driven discipline
- Lens J (host-language): honoured; LLVM jump-table for byte-discriminator
- Lens K (meta-grammar): LOAD-BEARING for V1 H-gate close, ASPIRATIONAL for V1 correctness

## §5 Punch list — V1-corpus absorption edits

11 surgical edits, all narrow. Lock 1 (the proposal §4.1 amendment text) is the single load-bearing structural edit; the rest are mechanical row / paragraph / matrix-cell amendments.

### P1 — REINVENT PASS-1 §2 Backend-IR ownership row (Tape/direct/value)
- Target: `restart/audit/pass-1-substrate/PASS-1.md:53-55`
- Surgery: append " (under `tape_mode = "eager"` grammars; under `tape_mode = "lazy"` grammars, payload-slot is the computed `kind_at_cursor` rather than a stored field, per the §4 Lock 1 amendment)" to the "Payload category" cell
- Footnote: "`tape_mode` is a per-grammar `[workspace.metadata.bbnf.grammars.<g>.runtime]` key; ARC §9.1 owns the dual-mode invariant statement."

### P2 — REINVENT PASS-1 §2 builder-frame replacement paragraph
- Target: `restart/audit/pass-1-substrate/PASS-1.md:59`
- Surgery: change "plus `TapeBuilder` checkpoints" to "plus `TapeBuilder` checkpoints (eager mode) or `TapeAssembler` cursor checkpoints (lazy mode)"; update rollback semantics for both modes

### P3 — REINVENT PASS-1:187 identity statement
- Target: `restart/audit/pass-1-substrate/PASS-1.md:187`
- Surgery: change `(TapeId, node id, payload class)` to `(TapeId, node id, payload class)` (eager mode) or `(TapeId, cursor, kind_at_cursor)` (lazy mode); both forms share the snapshot-scoped `TapeId` and a stable u32 substrate-index discriminator
- Mirror: identical surgery at `restart/ARCHITECTURE.md:1409` and `restart/audit/pass-3-runtime/PASS-3.md:187`

### P4 — REINVENT PASS-1 §4/§5 hand-off rows
- Targets: `restart/audit/pass-1-substrate/PASS-1.md:197` (hand-off to PASS-2) and `:206` (hand-off to PASS-3)
- Surgery: extend blocker cells to include `tape_mode` metadata key and per-mode `ReparsePlan` reuse-range shape

### P5 — REINVENT ARC §12.2 per-grammar authority matrix
- Target: `restart/ARCHITECTURE.md:1616-1627`
- Surgery: add `tape_mode` column before "Declaration-crate status"; cell values: 9 grammars `eager`, `json` `lazy`, `yaml` `eager` (default)
- LOC reprojection footnote: "(post-lazy-mode: projected to ~3,400 per `restart/skinny/audit/LAZY-TAPE-DESIGN.md` §7.4)"

### P6 — REINVENT skinny/WORKSPACE.md (and V1 WORKSPACE equivalent)
- Target: `restart/skinny/WORKSPACE.md` (and V1 corpus WORKSPACE)
- Surgery: add `tape_mode: "eager" | "lazy"` (default `"eager"`) to `[workspace.metadata.bbnf.grammars.<g>.runtime]` schema enumeration

### P7 — REINVENT PASS-1:278-283 onboarding proof table
- Target: `restart/audit/pass-1-substrate/PASS-1.md:278-283`
- Surgery: clarify that `[...runtime]` sub-keys (including `tape_mode`) are metadata, not a third onboarding surface

### P8 — REINVENT PASS-1 §2 diagnostic-strings table + ARC §7.4 catalogue
- Targets: `restart/audit/pass-1-substrate/PASS-1.md:122-134` and `restart/ARCHITECTURE.md:1057-1088`
- Surgery: add `BBNF-TAPE-MODE-CONFLICT` row with verbatim message and producer site at `passes::extract` metadata-resolve stage

### P9 — REINVENT PASS-3 §1 ReparsePlan type declaration (PASS-1 dispatches to PASS-3)
- Target: `restart/audit/pass-3-runtime/PASS-3.md:208-220`
- Surgery: widen `ReparsePlan::Reuse { unchanged: Vec<TapeRange> }` to per-mode-typed enum

### P10 — ADMIT V1 H tranche body work-item (cost-model `tape_mode` override)
- Target: V1 MASTER-PLAN H tranche body
- Surgery: add work item per LAZY-TAPE-DESIGN.md §11.5

### P11 — Carry V9.1 residue (PASS-1:212 stale-prompt allusion)
- Target: `restart/audit/pass-1-substrate/PASS-1.md:212`
- Surgery: per V9.1 §5 residue surgery — replace with canonical grammar-surface prose

## §6 Final readiness

> **Decision: AMENDMENT-REQUIRED-NARROW.**
>
> The lazy-tape design proposal is coherent with the V1 PASS-1 surface. Its Lock 1 amendment text (proposal §4.1) is a verbatim transposition that preserves the lock's spirit (no parallel substrate; no OpenFrame clone; no second tree) while admitting per-grammar `tape_mode ∈ {eager, lazy}` materialization. Locks 2-14 survive verbatim; Lock 1 is the single REINVENT. Eleven surgical edits absorb the proposal into the V1 corpus (P1-P11); each is narrow and mechanical. The dual-mode design is not contrivance (Lens I) but a measured architectural transposition mandated by SK-V2's empirically-pinned eager-tape ceiling. Lock 14 holds — the dispatch is on `tape_mode` metadata, not on grammar name; the discriminator function is template-generated per grammar in the existing `runtime/src/grammars/<g>/view.rs` shape. Lock 9's `&'i str` / `parse_in` / `parse_owned` API survives byte-equal. The proposal's falsifiability hooks (post-implementation T1 < 13K Mbps refutes; > 14K Mbps validates; > 17K Mbps meets SOTA-beat target) keep the architectural claim mechanically falsifiable; if refuted, the V1 corpus reverts via single-commit `tape_mode = "eager"` flip for `json` and the eager-tape work persists for CSS-L4 / BBNF-self / Sheets.
>
> Hereupon the V1 PASS-1 amendment agent dispatches against P1-P11 under the V9.2 narrow-amendment cycle. The Lock 1 amendment text (P1+P5+P6) lands FIRST; P9 (PASS-3 `ReparsePlan`) dispatches next as Tranche I substrate-consumer prep; P11 closes the V9.1 carry-residue. The lazy-tape skinny implementation gate is independent of the amendment cycle but blocks on (a) approval and (b) measurement; the V1 corpus updates land when the skinny re-bench classifies outcome A/B/C per BENCH §6.
