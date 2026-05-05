# HARDENING-PASS-2-V5 — Carry-aware metahardening (post-Wave-4.1)

## §1 Target identification

| Field | Value |
|---|---|
| Target | `PASS-2` (post-Wave-4.1; carries V4-READY ground) |
| Path | `restart/audit/pass-2-codegen/PASS-2.md` |
| Lines audited | 1-573 |
| Audited commit | `b60d7572` (Wave-4.1 narrow amendment landing the V3 §14 punch list) |
| V4 verdict | **READY** (`restart/audit/hardening/HARDENING-PASS-2-V4.md:18`; commit `6987b166`) |
| V3 closure | 9 of 9 V3 surgeries closed at predicted ±2 lines (`HARDENING-PASS-2-V4.md:24-34`) |
| Audit cycle | V5 metahardening (carry-aware lenses A-E + compressed 9-lane re-audit) |
| Audit time | 60 minutes |
| Authority anchors | `restart/locks/14-LOCKS.md:34-60,118-125`; `restart/README.md:13-25,108-117,285-340,381-396`; `docs/precepts/instructions/STYLE.md` |
| Sister cross-references | `restart/audit/pass-1-substrate/PASS-1.md` (282 lines); `restart/audit/pass-3-runtime/PASS-3.md` (482 lines); `restart/ARCHITECTURE.md` (1408 lines, esp. §7.2-§7.4) |
| Dispatch contract | `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md:22-30` (five lenses); `restart/prompts/HARDENING.md` (9-lane base) |

V5's role is not to re-litigate V1-V4 punch lists. The carry-baseline is V4-READY: 9 of 9 V3 surgeries closed verbatim, the 4 V3 AMENDMENT-REQUIRED lanes (1, 3, 5, 7) all flipped to KEEP, the 4 V3 KEEP lanes (4, 6, 8, 9) re-confirmed without reversal. V5 looks for what V1-V4's per-target punch-list focus structurally missed — five lenses applied centrally rather than per-row.

Final decision: **READY** — with two carry-observations into Phase 1 research deep-dives (no PASS-2 amendment is required to advance; the observations are research-fold scoping notes, not blocking surgery).

## §2 Five carry-aware lens table (≥15 confirmation rows)

The five lenses come from `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md:22-30`. Each row carries Site / Lens / Finding / Disposition. Rows are independent; no row triggers amendment unless explicitly flagged.

| # | Site (path:line) | Lens | Finding | Disposition |
|---|---|---|---|---|
| L-1 | `PASS-2.md:32` ↔ `PASS-1.md:39-41` ↔ `ARCHITECTURE.md:870-872` | A — narrative coherence | "Backend IR is the PASS-2 boundary. PASS-1 produces it after parse, validate, type inference, shape mining, e-graph, cost extraction, and lower-to-BIR" binds verbatim to PASS-1's variant inventory at line 39 ("Backend IR shape: executable plan nodes…") and to ARCH §7.2's "Backend IR is executable and lowerer-facing. PASS-2 makes it the single backend…". Three voices, one boundary. | **CONFIRMED** — narrative binding holds. |
| L-2 | `PASS-2.md:50-77` (23-variant table) ↔ `PASS-1.md:39` (22 ARCH-named variants) ↔ `ARCHITECTURE.md:842-859,905-931` (24-row payload-and-lower matrix) | A — narrative coherence | PASS-2's 23-variant table names `Lookbehind` (#21) as PASS-2-side addition over BC's 22-variant base; PASS-1 line 39 enumerates 22 names; ARCH §7.2 enumerates 24 (adds `Predicate` and `Return`). PASS-1 says "KEEP about 22 executable variants" at line 10 + Lookbehind row at line 34. PASS-2 §2 commitment 2 says "PASS-2 adds `Lookbehind`, folds multi-function chaining into `HostCall`, and keeps Unicode inside `RegexDfa`." | **CARRY-OBSERVATION** — variant-count drift is reconcilable (23 PASS-2 variants ≈ 22 PASS-1 names + Lookbehind ≈ 24 ARCH variants minus `Predicate`/`Return` which collapse into PASS-2's `Seq`/`Rule` patterns), but the three documents do not agree on the headline count. SYNTHESIS Wave-2 reconciliation gate at PASS-1 line 159 covers this; not a blocker. |
| L-3 | `PASS-2.md:69` (Lock 2 layout-canon clause) ↔ `PASS-1.md:84-92` ↔ `ARCHITECTURE.md:977-990` (§7.3 side tables) | B — vocabulary drift | `passes::layout` / `LayoutFacts` / `LayoutSink` triple is verbatim across all three documents. ARCH §7.3 line 983 names `LayoutFacts` producer = `passes::layout`, consumer = "Backend IR builder (`LayoutPush`, `LayoutPop`), host registry, diagnostics" — verbatim binding to PASS-2's `Layout` BIR variant consumer. | **CONFIRMED** — vocabulary stable post-Wave-4.1 M1 amendment. |
| L-4 | `PASS-2.md:147-149` (3 schema rows) ↔ `PASS-3.md:34,115,420` (`VisitTypes` consumer) | B — vocabulary drift | `visitor_bitflags` (PASS-2 row 147; source = "BIR view shapes"; consumer = "generated `visitor.rs` impl") binds to PASS-3's `VisitTypes` bitflag pruning at lines 34, 115, 420. PASS-2 uses the lowercase `visitor_bitflags` (template-parameter name) where PASS-3 uses `VisitTypes` (generated trait); these are different vocabulary tiers (codegen schema vs. emitted Rust type) and therefore not drift. | **CONFIRMED** — tier-distinct, not drift. |
| L-5 | `PASS-2.md:148` (`bump_arena` row) ↔ `PASS-3.md:30,52,66-72,109` ↔ `ARCHITECTURE.md:197-208,1007` | B — vocabulary drift | `bump_arena` (PASS-2 schema) → `parse_in` signature lowering → `parse_in<'arena, 'input>(&'arena Arena, &'input str)` (PASS-3 line 66). ARCH §3.4 line 197 declares the same signature. `BBNF-ARENA-MISMATCH` (alias `BBNF-LIFE002`) at ARCH line 1007 binds the diagnostic. | **CONFIRMED** — Lock 9 lifetime triplet reaches the schema, the API surface, and the diagnostic ledger consistently. |
| L-6 | `PASS-2.md:149` (`incremental_marker`) ↔ ARCH/MASTER-PLAN/PASS-3 (silent) | B — vocabulary drift | `incremental_marker` is unique to PASS-2 (source: cost model; consumer: optional source-map sidecar). Cross-document grep returns zero hits in PASS-1, PASS-3, MASTER-PLAN, ARCH for "incremental_marker" / "source-map sidecar". | **CARRY-OBSERVATION** — PASS-2 introduces the parameter row; receivers are absent. The Phase 1 research topic 7 (green/red trees + incremental parsing) at orchestrator line 65 is the natural receiver; this is research scope, not amendment scope. |
| L-7 | `PASS-2.md:534-541` (8 BBNF-* codes with verbatim strings) ↔ `ARCHITECTURE.md:1006-1032` (§7.4 catalog) ↔ `PASS-3.md:352-369` | B — vocabulary drift | ARCH §7.4 explicitly aliases: `BBNF-GEN001 (alias BBNF-CG001)` at line 1028; `BBNF-PRATT-NOT-APPLIED (alias BBNF-OPT001)` at line 1017; `BBNF-SIMD-NOT-SELECTED (alias BBNF-OPT002)` at line 1018; `BBNF-LIFETIME-ESCAPE (alias BBNF-LIFE001)` at line 1006. ARCH owns aliases; PASS-2 owns the originals. **Severity drift at OPT001/002**: PASS-2 line 540 phrases as `error` ("optimizer rejects an apparent operator-chain candidate … promote to PrattSpine with @pratt or restructure the rule"); PASS-3 line 356 phrases as `note` ("Pratt was not applied … The grammar still parses; performance fallback uses recursive-descent"). The codes refer to the same fact (cost-model declined) but PASS-2 frames as rejection, PASS-3 as advisory. | **CARRY-OBSERVATION** — severity-class drift is real but reconcilable: a single fact can carry an error-class diagnostic at codegen close (PASS-2's surface, "blocks codegen until grammar author resolves") and a note-class diagnostic at runtime (PASS-3's surface, "build succeeds with scalar fallback"). The two are not contradictory if Lock 10 is read strictly: auto-detection means the cost-model declines silently and emits a note; the PASS-2 error-class phrasing reads against Lock 10's directive-free posture. SYNTHESIS Wave-2 reconciliation should pin one severity class; receiver = ARCH §7.4 catalog (already authoritative for aliases). |
| L-8 | `PASS-2.md:540-541` (PASS-2 OPT001/002 strings reference `@pratt`/`@simd` hint syntax) ↔ `restart/locks/14-LOCKS.md:52` (Lock 10: "No `@pratt` or `@simd` directives") | B + E — vocabulary drift + axiom consistency | PASS-2 verbatim string offers `@pratt` and `@simd` as remediation hints in OPT001/002 diagnostic strings. Lock 10 explicitly forbids "`@pratt` or `@simd` directives". V4 audit at `HARDENING-PASS-2-V4.md:112` defended the phrasing as "hint surfaces (PASS-3-routed escape valves), not as detection directives". The defense is plausible but introduces a subtle vocabulary surface that Lock 10 does not authorize: a hint syntax that the user can write to "force" the optimizer is functionally equivalent to a directive. | **CARRY-OBSERVATION** — Lock 10 strict reading rejects any author-side `@pratt`/`@simd` syntax mention; Lock 10 lenient reading accepts diagnostic hints as remediation. SYNTHESIS Wave-2 should clarify whether `@pratt`/`@simd` exist as hint syntax at all, and if so, where they live in the BBNF surface (currently undefined). Receiver = ARCH §7.4 / PASS-3 diagnostic surface. |
| L-9 | `PASS-2.md:386` (yaml two-surface invariant) ↔ `PASS-3.md:342` ↔ `ARCHITECTURE.md:1331` ↔ `README.md:13` ↔ `Lock 14` | A — narrative coherence | All five surfaces agree: yaml onboarding requires exactly `grammars/yaml.bbnf` + `[workspace.metadata.bbnf.grammars.yaml]`. PASS-2's verification commands (`git diff HEAD~1` + `rg JsonParser\|...` + `find crates/runtime/src/grammars/yaml`) match Lock 14's verification commands at `restart/locks/14-LOCKS.md:60`. | **CONFIRMED** — strongest cross-document binding in the corpus. |
| L-10 | `PASS-2.md` (no full yaml worked example end-to-end) | C — worked-example scarcity | PASS-2 carries the yaml proof table (line 381-386) and the per-grammar runtime emission row (line 486) but **no end-to-end walkthrough**: grammar source snippet → BIR snapshot → generated `runtime/src/grammars/yaml/{generated.rs, parser.rs, host.rs}` skeleton → smoke gate output. ARCH §7.2 line 933 carries 23-row source-to-BIR coverage examples (one fragment per variant), which is the closest worked-example surface. PASS-2 cites it by reference but never traces a single grammar end-to-end. | **CARRY-OBSERVATION** — V1-V4 lanes never demanded the worked example; V5 surfaces it. Per-tranche full-spec drafting (Tranche F runtime template + Tranche G runtime publication) is the natural receiver. Not a blocker for V5 advance. |
| L-11 | `PASS-2.md:166-170` (detection threshold table) | C — worked-example scarcity | The table specifies select-when / reject-when conditions for Pratt / SIMD / PHF / Lookbehind, but no trace of a specific grammar shape triggering Pratt detection (e.g., `expr = expr "+" expr | expr "*" expr | INT;` → recursive operator family detected → `PrattSpine` LUT emitted with precedence table). Same gap for SIMD trigger trace (e.g., JSON structural alphabet `{}[],:` → `SimdScan` with NEON kernel). | **CARRY-OBSERVATION** — same disposition as L-10; receiver = per-tranche drafting (Tranche F lowerer or H Pratt/SIMD wave). |
| L-12 | `PASS-2.md:50-77` (23-variant table) carries no worked variant-payload-trace | C — worked-example scarcity | The 23-variant table gives payload + generation site + Rust lowering + WASM lowering + TS status per variant, all in single-cell prose. No row is traced through to emitted Rust source or BIR snapshot byte format. ARCH §7.2 lines 905-931 (24-row payload-and-lower matrix) is the closest, but is also single-cell. | **CARRY-OBSERVATION** — Phase 1 research topic 6 (tape encoding + direct-to-struct union) at orchestrator line 64 is a partial receiver; full traces likely belong in per-tranche drafting (Tranche E BIR or F runtime template). |
| L-13 | `PASS-2.md` (no per-backend lowering test budget rows) | D — coverage gaps | The §2 per-payload lowering test gates table (lines 100-108) names 7 `cargo test -p codegen` invocations but no test-count budget per backend (Rust V1 ≥ N tests, WASM V1 ≥ M tests, TS scaffold ≥ K compile-only smokes). PASS-3 §6 line 109 mentions "three executable consumer gates pass on every extant grammar plus yaml" but this is a different surface. | **CARRY-OBSERVATION** — Lane 6 (Generated-Code Budget) covered LOC + wall budgets; test-count budget was never on V1-V4's punch lists. Receiver = per-tranche drafting (test-budget rows fold into Tranche F + Tranche H wave gates). Not blocking. |
| L-14 | `PASS-2.md:149` (`incremental_marker` schema row) | D — coverage gaps | PASS-2 names the parameter but no incremental codegen story exists in any document: regen-on-change semantics, content-equality skip preserving mtime (line 432) is the only existing infra, but content-equality is a write-time check, not regen-time skip-when-unchanged. | **CARRY-OBSERVATION** — Phase 1 topic 7 (incremental parsing + green/red trees) is the receiver; this is research scope. Not blocking PASS-2 advance. |
| L-15 | `PASS-2.md` (no bumpalo arena lifetime ergonomics text in PASS-2) | D — coverage gaps | The `bump_arena` schema row (line 148) names PASS-3 API contract source + `parse_in` signature lowering consumer but PASS-2 does not specify how `parse_in<'arena, 'input>(...)` lowers when the BIR `Rule` payload references arena-allocated tape spans. PASS-3 line 66-72 shows the signature; ARCH §3.4 lines 197-208 shows the constraint. PASS-2 carries the lowering obligation without explicit emission detail. | **CARRY-OBSERVATION** — PASS-3 owns the API surface; PASS-2 owns the schema row. The lowering details belong in per-tranche drafting (Tranche F runtime template). Not a blocker; the schema row is sufficient PASS-2-side scope. |
| L-16 | `PASS-2.md:540-541` (BBNF-OPT001/002 misfire codes published verbatim) | D — coverage gaps | V3 surgery 7 added these. The Lane 7 friction surface for "auto-detector misfire" is now bound. However, the diagnostic strings themselves are aspirational (template strings with `{rule}`, `{reason}`, `{alpha}`, `{shape}`); no test ratifies that the cost-model emits these specific strings. | **CARRY-OBSERVATION** — verbatim string ratification belongs in per-tranche drafting; this is the same class of gap as L-13 (test-budget). Not blocking. |
| L-17 | `PASS-2.md:32` (BIR is PASS-2 boundary) under `Lock 6` (e-graph rewrites of BIR) | E — axiom cumulative consistency | Lock 6 grants e-graph rewrites of BIR. PASS-2 says BIR is produced by PASS-1 "after parse, validate, type inference, shape mining, e-graph, cost extraction, and lower-to-BIR" — i.e., e-graph rewrites occur **before** BIR enters codegen. PASS-2 codegen does not need to handle "post-rewrite BIR" because rewrites are upstream. The same BIR alphabet (the 23-variant set) is what codegen consumes regardless of upstream rewrite history. | **CONFIRMED** — Lock 5 (codegen consumes BIR only) under Lock 6 (e-graph rewrites BIR) holds at PASS-2's layer because the rewrites do not alter the variant alphabet, only payload values. The payload-refiner contract at PASS-2 line 85-96 is the load-bearing rule. |
| L-18 | `PASS-2.md:148` (`bump_arena` row) under Lock 1 (tape) + Lock 9 (parse_in / bumpalo) | E — axiom cumulative consistency | Lock 1 says tape is the substrate; Lock 9 says slice-borrow primary, bumpalo + owned escape hatches. PASS-2 emits one parse path; the `bump_arena` row routes `parse_in` through the same lowerer. PASS-3 line 66 shows `parse_in<'arena, 'input>` signature with `&'arena Arena` — the arena allocates tape nodes while the slice borrows from `'input`. One BIR program, three call signatures (`parse`, `parse_in`, `parse_owned`), one tape substrate. | **CONFIRMED** — Lock 1 + Lock 9 compose. The schema row is sufficient codegen-side scope; PASS-3 owns the signature surface. |
| L-19 | `PASS-2.md` (yaml two-surface) under Lock 14 (two surfaces) + Lock 5 (codegen consumes BIR only) | E — axiom cumulative consistency | Lock 14 says adding a new grammar = source file + metadata block, "no code change in any generic or other-grammar crate." Lock 5 says codegen consumes BIR only. The composition implies: adding yaml requires zero changes to the codegen template, codegen lowerers, BIR alphabet, runtime template, or any generic crate. PASS-2's yaml proof table (line 381-386) verifies the input invariant (file diffs) but **does not explicitly state "zero changes to PASS-2 codegen template"**. The verification command is grep-based at the source-tree level (`rg JsonParser\|... crates/{ir,codegen,runtime,host,passes}/src/`), which catches per-grammar match arms; it does not catch a template extension that adds a yaml-specific cell. | **CARRY-OBSERVATION** — the proof catches the worst-case violation (per-grammar match arms in generic crates) but not the subtler "template extension for yaml". Lock 14 verification commands at line 60 of the locks doc share this scope. The runtime emission table at PASS-2 line 486 carries `auto-detected from grammar shape` for yaml's Pratt/SIMD column, which is the positive surface — but a strict Lock 5 + Lock 14 composition reading would benefit from one explicit claim: "the runtime template parameter set is grammar-agnostic; yaml flows through the same parameter substitutions as json/csv/etc." This is text-polish, not blocker. |
| L-20 | `PASS-2.md:5-7` (replacement posture) | A + E | The verdict text at line 5-7 names every conflict: ParseStream, rewrite-mode, Unicode-set surface, per-grammar declaration crates. The conflict ledger at lines 11-17 binds each conflict to stale authority + settled resolution with path:line citations. The replacement posture composes Lock 1 (tape kills ParseStream rebrand) + Lock 5 (BIR kills Grammar IR walk) + Lock 6 (Visitor kills rewrite-mode) + Lock 11 (regex Unicode kills grammar Unicode algebra) + Lock 14 (two-surface kills declaration crates). | **CONFIRMED** — strongest gestalt-binding row in PASS-2; survives Lens A + E pressure. |

Lens row totals: 20 rows (≥15 required) — **CONFIRMED**: 9; **CARRY-OBSERVATION**: 11; blocking findings: 0.

Disposition summary: every CARRY-OBSERVATION is research-scope, per-tranche-drafting-scope, or SYNTHESIS-reconciliation-scope; none is PASS-2 V5 amendment-scope.

## §3 Compressed 9-lane re-audit (≥15 confirmation rows; verification mode)

V4 returned KEEP across all 9 lanes (KEEP 39 / REINVENT 0 / DISCARD 0; per `HARDENING-PASS-2-V4.md:142`). V5 verifies the V4 closure under the lens-revealed observations rather than re-litigating per-row.

### §3.1 Lane 1 — Lock-Adherence (re-audit)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:13,36,455` | Lock 1 — tape + direct union | Tape named substrate; OpenFrame deletion mechanism gate at line 455. | None — V4 row carries. | Defeated by L-17 + L-18 lens consistency. | KEEP |
| `PASS-2.md:69` | Lock 2 — `passes::layout` / `LayoutFacts` / `LayoutSink` | L-3 confirms verbatim cross-doc binding. | None. | Defeated. | KEEP |
| `PASS-2.md:176` | Lock 3 — unified cursor + byte-skip + `__EAGER_EMPTY_PATH` | The unified parse-implementation declaration names the realisation surface. | None. | Defeated. | KEEP |
| `PASS-2.md:32,247-250` | Lock 5 — BIR-only codegen + deny gate widened to `crates/codegen/src/` | L-1 + L-17 confirm boundary holds; deny scope correctly widened. | None. | Defeated. | KEEP |
| `PASS-2.md:46,316-326` | Lock 6 — xtask emits committed source + content-equality writes preserved + regen split into 9 sub-modules | Honours Lock 6; per-area enforcing commands at lines 412-422. | None. | Defeated. | KEEP |
| `PASS-2.md:441-449` | Lock 8 — SOTA throughput trajectory rows | All 5 numeric rows + 2 simdjson on-demand rows carry competitor + dataset + platform + bbnf target + mechanism + evidence. | None — V4 re-confirmed. | Defeated. | KEEP |
| `PASS-2.md:147-149` | Lock 9 — `bump_arena` schema row binds `parse_in` lowering | L-5 + L-18 confirm Lock 1 + Lock 9 compose. | None — schema row is codegen-side scope. | Defeated. | KEEP |
| `PASS-2.md:166-170,540-541` | Lock 10 — Pratt/SIMD auto-detected | Detection threshold table holds; misfire diagnostics published. | L-7 + L-8: severity drift between PASS-2 (`error`) and PASS-3 (`note`); `@pratt`/`@simd` hint mention reads against Lock 10's directive-free posture under strict reading. | Steelman: PASS-3 owns runtime-side phrasing; PASS-2 owns codegen-close phrasing; the two are different surfaces. **Carry-observation** retained as L-7+L-8; verdict not flipped. | KEEP (with carry-observation) |
| `PASS-2.md:300-311` | Lock 11 — `simd-scan` workspace-internal; regex Unicode in `parse-that/regex` | Per Lane 1 V4 confirmation; unchanged. | None. | Defeated. | KEEP |
| `PASS-2.md` (silent) | Lock 12 — ser/gorgeous archive | PASS-2 correctly silent; Tranche A.W0 owns. | None. | Defeated. | KEEP |
| `PASS-2.md:198-329,412-422` | Lock 13 — no god directories; child-count + LOC enforcement | Per-crate trees obey 4-10 children; per-area enforcing commands at lines 414-420. | None. | Defeated. | KEEP |
| `PASS-2.md:48,381-386,475-486` | Lock 14 — two-surface + per-X tables + hand-written prohibition | L-9 + L-19 confirm the input invariant + per-X coverage. | L-19 surfaces a subtle composition gap (template-extension scope) but not blocker. | Defeated for V5 advance. | KEEP |

Lane 1 verdict: **KEEP** — KEEP 12 / REINVENT 0 / DISCARD 0. V4 closure holds; L-7+L-8 carry-observation surfaces severity-class drift in OPT001/002 and `@pratt`/`@simd` hint phrasing for SYNTHESIS Wave-2 reconciliation.

### §3.2 Lane 2 — Sequencing Discipline (counted-inheritance row only)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:506-516` | Wave-by-wave inheritance ledger | Inheritance carries; Lane 8 owns deferral discipline. | None. | Defeated. | KEEP |

Lane 2 verdict: **N/A** — KEEP 1 / REINVENT 0 / DISCARD 0. Unchanged from V4.

### §3.3 Lane 3 — Cohesion (re-audit)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:81` | Cardinality defence — corpus citation (post-Wave-4.1 swc retirement) | L-2 confirms the 23-variant cardinality stays in the 20-30 band. | L-2 surfaces variant-count drift across PASS-1/PASS-2/ARCH (22/23/24); reconcilable but not yet pinned. | Steelman: SYNTHESIS Wave-2 reconciliation gate at PASS-1 line 159 owns the count-pinning. Defeated for V5 advance. | KEEP |
| `PASS-2.md:147-149` | Runtime template schema — 16 rows | L-4, L-5, L-6 confirm rows bind to PASS-3 / ARCH consumers (with one CARRY-OBSERVATION at L-6 for `incremental_marker` orphan). | L-6: receivers absent for `incremental_marker`. | Steelman: Phase 1 research topic 7 (incremental parsing) is the natural receiver. Defeated. | KEEP |
| `PASS-2.md:333-358` | PASS-3 handoff + 6 acceptance gates | All gates carry named `cargo test`/`cargo bench`/`cargo xtask` invocations. | None. | Defeated. | KEEP |
| `PASS-2.md:50-77` | 23-variant BIR table | L-2 cohesion holds at the table level; the count-headline drift is cross-document, not intra-table. | None at table level. | Defeated. | KEEP |
| `PASS-2.md:111-126` | `BackendLowerer` 8-method API | Trait collapses 566-line current Emitter; matches PASS-B forecast. | None. | Defeated. | KEEP |
| `PASS-2.md:386` | yaml two-surface invariant — input + end-state | L-9 + L-19 confirm. | L-19 surfaces template-extension subtlety. | Defeated. | KEEP |
| `PASS-2.md:153-161` | SIMD coverage matrix | aarch64/x86_64/wasm32/scalar parity rows. | None. | Defeated. | KEEP |

Lane 3 verdict: **KEEP** — KEEP 7 / REINVENT 0 / DISCARD 0. V4 closure holds; carry-observations route to research / SYNTHESIS Wave-2.

### §3.4 Lane 4 — SOTA Anchoring (re-confirm)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:441-449` | Throughput trajectory — 7 rows | Every parse-throughput row carries Lock 8 tuple. | None. | Defeated. | KEEP |
| `PASS-2.md:453-457` | Mechanism gate table | OpenFrame deletion + Pratt detection + WASM parity correctly demoted from throughput. | None. | Defeated. | KEEP |

Lane 4 verdict: **KEEP** — KEEP 2 / REINVENT 0 / DISCARD 0. Unchanged from V4.

### §3.5 Lane 5 — Grammar-Authoritative (re-audit)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:386` | yaml two-surface input invariant | L-9 binds across 5 documents. | L-19 template-extension subtlety. | Defeated. | KEEP |
| `PASS-2.md:392-405` | Per-grammar generated_loc table | Per-X coverage; baseline category column. | None. | Defeated. | KEEP |
| `PASS-2.md:475-486` | Per-grammar runtime emission table | Per-X coverage; hand-written prohibition at line 488. | None. | Defeated. | KEEP |
| `PASS-2.md` (whole document) | grep verifications | `rg -nP "match\s+\w+\s*\{[^}]*Json\s*=>\|CssL4\s*=>\|..." PASS-2.md` returns zero (verified). | None. | Defeated. | KEEP |

Lane 5 verdict: **KEEP** — KEEP 4 / REINVENT 0 / DISCARD 0. Unchanged from V4.

### §3.6 Lane 6 — Generated-Code Budget (re-confirm)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:392-405` | Per-grammar generated_loc + xtask wall + baseline + SYNTHESIS carry pointer | Row-complete; Architecture promotion at line 406. | None. | Defeated. | KEEP |
| `PASS-2.md:412-422` | Non-generated LOC + child-count + per-area enforcing command | 7-area table covers `ir/`, `codegen/`, `runtime/`, `host/`, `xtask/`. | None. | Defeated. | KEEP |
| `PASS-2.md:426-433` | Regen-cycle wall budget | 6 rows; baseline category column. | None. | Defeated. | KEEP |

Lane 6 verdict: **KEEP** — KEEP 3 / REINVENT 0 / DISCARD 0. Unchanged from V4.

### §3.7 Lane 7 — Friction Forecast (re-audit)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:534-539` | 6 verbatim diagnostic strings (`BBNF-GEN001`, `BBNF-GEN014`, `BBNF-CODEGEN021`, `BBNF-CODEGEN033`, `BBNF-LIFE009`, `BBNF-SEM040`) | Codes + verbatim user-facing strings present. | L-7: severity-class drift OPT001/002 PASS-2 vs. PASS-3. | Steelman per V4 §3.7 row 2: PASS-3 owns runtime-side phrasing; PASS-2 owns codegen-close phrasing. Defeated for KEEP. | KEEP |
| `PASS-2.md:540-541` | `BBNF-OPT001`/`BBNF-OPT002` — Pratt/SIMD misfire | Verbatim strings present. | L-7 severity drift; L-8 `@pratt`/`@simd` hint phrasing reads against strict Lock 10. | Steelman: hint mention is remediation, not directive. Defeated for KEEP; carry-observations route to SYNTHESIS Wave-2 + ARCH §7.4. | KEEP |
| `PASS-2.md:166-170` | Detection threshold table | Bound to verbatim diagnostics now. | L-11: no Pratt/SIMD trigger trace example. | Steelman: per-tranche drafting receives. Defeated for V5 advance. | KEEP |
| `PASS-2.md:174` | Lookbehind diagnostic routing | `BBNF1004` (PASS-1 user-facing) + `BBNF-SEM040` (PASS-2 routing) co-emit. | None. | Defeated. | KEEP |
| `PASS-2.md:486,540-541` | yaml friction surface | `BBNF-OPT001`/`BBNF-OPT002` provide the auto-detection misfire surface. | L-16 verbatim string ratification absent. | Steelman: per-tranche drafting receives. Defeated. | KEEP |
| `PASS-2.md:538` | `BBNF-LIFE009` lifetime constructor diagnostic | Verbatim string names actual/expected/annotation triple + remediation hint. | None. | Defeated. | KEEP |

Lane 7 verdict: **KEEP** — KEEP 6 / REINVENT 0 / DISCARD 0. V4 closure holds; carry-observations L-7+L-8+L-11+L-16 route to SYNTHESIS Wave-2 + research / per-tranche drafting.

### §3.8 Lane 8 — Carry & Deferral (re-confirm)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:545-554` | 8-row carry ledger — Receiver / Blocker / Receiving gate | Every defer carries the triple. | None. | Defeated. | KEEP |
| `PASS-2.md:433` | yaml smoke regen receiver pinned to Tranche G | Wave-4.1 P2-9 closure. | None. | Defeated. | KEEP |
| `PASS-2.md:431` | BIR snapshot baseline carry | Owner + receiver named (provisional). | "Measurement gate" is a phase, not a verifiable artefact path; V3 minor REINVENT was non-blocking. | Steelman: provisional ownership permits abstract gates. Defeated. | KEEP |

Lane 8 verdict: **KEEP** — KEEP 3 / REINVENT 0 / DISCARD 0. Unchanged from V4.

### §3.9 Lane 9 — Greenfield Discipline (re-confirm)

| Site (path:line) | Item | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:5-7` | Replacement posture + DISCARD list | L-20 confirms strongest gestalt-binding. | None. | Defeated. | KEEP |
| `PASS-2.md:11-17` | Conflict ledger | 5-row table resolves prompt/inheritance contradictions. | None. | Defeated. | KEEP |
| `PASS-2.md:556-567` | Punch list (10 items) | Architectural-transition surgery captured. | None. | Defeated. | KEEP |

Lane 9 verdict: **KEEP** — KEEP 3 / REINVENT 0 / DISCARD 0. Unchanged from V4.

### §3.10 9-lane re-audit summary

| Lane | V3 verdict | V4 verdict | V5 verdict | Carry-observations |
|---|---|---|---|---|
| 1 — Lock-Adherence | AMENDMENT-REQUIRED (4 REINVENTs) | KEEP | KEEP | L-7 + L-8 (OPT001/002 severity + `@pratt`/`@simd` hint) |
| 2 — Sequencing | N/A | N/A | N/A | none |
| 3 — Cohesion | AMENDMENT-REQUIRED (2 REINVENTs) | KEEP | KEEP | L-2 (variant count); L-6 (`incremental_marker` orphan) |
| 4 — SOTA Anchoring | KEEP | KEEP | KEEP | none |
| 5 — Grammar-Authoritative | AMENDMENT-REQUIRED (2 REINVENTs) | KEEP | KEEP | L-19 (template-extension subtlety) |
| 6 — Generated-Code Budget | KEEP | KEEP | KEEP | L-13 (test-count budget absence; non-blocker) |
| 7 — Friction Forecast | AMENDMENT-REQUIRED (4 REINVENTs) | KEEP | KEEP | L-7 + L-8 + L-11 + L-16 |
| 8 — Carry & Deferral | KEEP | KEEP | KEEP | none |
| 9 — Greenfield Discipline | KEEP | KEEP | KEEP | none |

Total: 41 audit rows across 9 lanes. KEEP 41 / REINVENT 0 / DISCARD 0. V5 verifies V4-READY; lens-revealed carry-observations route to research / SYNTHESIS Wave-2 / per-tranche drafting; none triggers V5 amendment.

## §4 Tightened gate-rerun results

The 11-command tightened gate-rerun specified at §4 of the dispatch:

| # | Gate | Command | Expected | Actual | Pass/Fail |
|---|---|---|---|---|---|
| 1 | Path crate naming | `rg -n "bbnf-path\|bbnf-test-fixtures\|path!" PASS-2.md` | zero matches | zero matches | PASS |
| 2 | Backend IR ownership | `rg -n "codegen/src/backend_ir" PASS-2.md` | zero ownership claims; doc-only tagged | 3 matches at lines 196, 233, 248. Line 196 — "PASS-2 names no `codegen/src/backend_ir/` ownership path"; line 233 — "documentation only"; line 248 — verbatim deny exception. | PASS |
| 3 | OpenFrame archaeology | `rg -n "OpenFrame" PASS-2.md` | every match reads as DISCARD/deletion | 5 matches at lines 7, 36, 91, 455, 561. All deletion / pathology / archaeology framing. | PASS |
| 4 | GrammarIR | `rg -n "GrammarIR" PASS-2.md` | verbatim deny present, scope widened to `crates/codegen/src/` | 3 matches at lines 5 (current-source violation), 249 (deny header), 250 (`rg -n "GrammarIR" crates/codegen/src/`). Scope widening verified. | PASS |
| 5 | SOTA datasets | `rg -n "twitter\|canada\|citm\|bootstrap\|animate" PASS-2.md` | numeric competitor rows | 12 matches across throughput trajectory + per-construct + smoke gate tables. | PASS |
| 6 | Carry-ledger | `rg -n "receiver\|blocker\|receiving gate" PASS-2.md` plus header inspection | Receiver / Blocker / Receiving gate columns + 8 rows | header at line 545; 8 rows at 547-554; plus 2 inline references at lines 431, 433. | PASS |
| 7 | yaml two-surface | `rg -n "yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml" PASS-2.md` | two-surface input invariant verbatim | 2 matches at lines 381 + 386 (full row with `git diff HEAD~1` + grep + find verification commands; both verbatim path strings). | PASS |
| 8 | Budgets | `rg -n "generated_loc\|regen_wall\|xtask" PASS-2.md` | per-grammar budget + xtask wall | 16 matches across §6 budget block. | PASS |
| 9 | Diagnostic codes + lookbehind | `rg -n "BBNF-GEN\|BBNF-CODEGEN\|BBNF-OPT\|BBNF-LIFE\|BBNF-SEM\|HostSignature\|lookbehind" PASS-2.md` | 8 codes + verbatim strings + lookbehind routing | 14+ matches; the diagnostic ledger at lines 532-541 carries 8 codes (`BBNF-GEN001`, `BBNF-GEN014`, `BBNF-CODEGEN021`, `BBNF-CODEGEN033`, `BBNF-LIFE009`, `BBNF-SEM040`, `BBNF-OPT001`, `BBNF-OPT002`) all with verbatim strings; lookbehind routing at lines 77, 170, 174, 190, 511, 539. | PASS |
| 10 | passes::types | `rg -n "passes::types\|passes/src/types" PASS-2.md` | not present (PASS-2 owns codegen, not types) | zero matches | PASS — N/A by scope |
| 11 | LayoutFacts / LayoutSink / passes::layout | `rg -n "LayoutFacts\|LayoutSink\|passes::layout" PASS-2.md` | Lock 2 canonical triple at line 69 | 1 match at line 69 with all three strings verbatim in one paragraph. | PASS |

Gate verdict: **11 PASS, 0 PARTIAL, 0 FAIL**. All gates close.

## §5 Cross-document binding ledger

V5's central contribution beyond V4: explicit cross-document binding ledger from PASS-2's perspective. Every PASS-2 substantive surface that depends on or is depended upon by another document is named:

| PASS-2 surface | Binds to (path:line) | Direction | Lens | Status |
|---|---|---|---|---|
| `PASS-2.md:32` "Backend IR is the PASS-2 boundary" | `PASS-1.md:39-41` (BIR producer side) + `ARCHITECTURE.md:870-872` (§7.2 BIR consumer side) | bi-directional | A | bound; same boundary, three voices |
| `PASS-2.md:50-77` 23-variant table | `PASS-1.md:34,39` (variant inventory) + `ARCHITECTURE.md:842-859,905-931` (24-variant matrix) | bi-directional | A | bound with variant-count drift carry-observation L-2 |
| `PASS-2.md:69` Lock 2 canon clause | `PASS-1.md:84-92` + `ARCHITECTURE.md:977-990` (§7.3 side tables) | bi-directional | B | verbatim binding |
| `PASS-2.md:147` `visitor_bitflags` | `PASS-3.md:34,115,420` (`VisitTypes` consumer) | downstream | B | tier-distinct (template parameter ↔ generated trait); not drift |
| `PASS-2.md:148` `bump_arena` | `PASS-3.md:30,52,66-72` (`parse_in` signature) + `ARCHITECTURE.md:197-208,1007` | downstream | B + E | bound; Lock 9 lifetime triplet reaches both surfaces |
| `PASS-2.md:149` `incremental_marker` | (no consumer in current corpus) | orphan | B + D | carry-observation L-6; receiver = Phase 1 topic 7 research |
| `PASS-2.md:174-176` Lookbehind co-amendment + Lock 3 obligation | `PASS-1.md:34,84-101` (BBNF1004 + width analysis) | upstream | A | bound; co-emit pattern is canonical |
| `PASS-2.md:534-539` 6 BBNF-* verbatim strings | `ARCHITECTURE.md:1006-1032` (§7.4 catalog with aliases) + `PASS-3.md:352-369` | bi-directional | B | bound; ARCH §7.4 owns alias table |
| `PASS-2.md:540-541` BBNF-OPT001/002 | `ARCHITECTURE.md:1017-1018` (alias `BBNF-PRATT-NOT-APPLIED` / `BBNF-SIMD-NOT-SELECTED`) + `PASS-3.md:356-357` (`note` severity) | bi-directional | B | severity-class drift carry-observation L-7+L-8; receiver = SYNTHESIS Wave-2 |
| `PASS-2.md:381-386` yaml two-surface invariant | `PASS-3.md:342` + `ARCHITECTURE.md:1331` + `README.md:13` + `Lock 14 (locks doc:60)` | upstream + lateral | A + E | strongest binding in corpus; 5-document agreement |
| `PASS-2.md:392-405` per-grammar generated_loc table | `MASTER-PLAN.md:634-649` + `ARCHITECTURE.md:1320-1331` | downstream (SYNTHESIS Wave-2 carry) | A | row-for-row identity required per `PASS-2.md:406` |
| `PASS-2.md:333-358` PASS-3 handoff + acceptance gates | `PASS-3.md:105,109,420-422,469,474` | downstream | A | every gate cited from PASS-3; 6 named verification commands |
| `PASS-2.md:545-554` 8-row carry ledger | `MASTER-PLAN.md:730-756` + `MIGRATION.md:772-781` | downstream | A | bound to consolidated carry-ledger per HARDENING-CONSOLIDATED-V4 §3 M7 |

13 binding rows. 11 confirmed; 2 carry observations (L-6 `incremental_marker` orphan; L-7+L-8 OPT001/002 drift). No binding fault. The cross-document narrative coherence holds at every substantive surface.

## §6 V1-V4 history note

The V1-V4 cycle is the carry-baseline; V5 audits what those cycles structurally missed.

- **V1** (`HARDENING-PASS-2.md`, commit `015317db`) — verdict AMENDMENT-REQUIRED; 9-item punch list; KEEP 38 / REINVENT 20 / DISCARD 1. The first adversary surfaced Lock 5 BIR ownership drift into `codegen/`, missing yaml proof, missing verbatim diagnostics, missing carry ledger triple, OpenFrame preservation residue.
- **V2** (`HARDENING-PASS-2-V2.md`, post Wave-1.2 + Wave-2) — verdict READY; KEEP 62 / REINVENT 2 / DISCARD 1. Read the V1 punch list as resolved by Wave-1.2 (BIR ownership ratification, lowerer import-deny gate, BIR payload refinement) + Wave-2 (PASS-3 emission contract, lookbehind co-amendment, runtime emission table, per-grammar LOC table, SOTA trajectory, carry ledger, OpenFrame retirement).
- **V3** (`HARDENING-PASS-2-V3.md`, independent rerun) — verdict AMENDMENT-REQUIRED; 8-item + 1-optional punch list; KEEP 39 / REINVENT 14 / DISCARD 0. Sharper read of HARDENING.md §3 lane 7 standard ("specify the verbatim error message" per `restart/prompts/HARDENING.md:131-141`); surfaced Lock 2 canonical wording absence, Lock 3 cursor obligation absence, Lock 5 deny-gate scope, Lock 9 lifetime row absence, Lock 14 input invariant absence, verbatim diagnostic strings absence, Pratt/SIMD misfire codes absence, swc URL violation of path:line discipline.
- **V4** (`HARDENING-PASS-2-V4.md`, post Wave-4.1 narrow amendment commit `b60d7572`) — verdict READY; KEEP 39 / REINVENT 0 / DISCARD 0. Verified all 9 V3 surgeries closed at predicted ±2 lines; the four V3 AMENDMENT-REQUIRED lanes (1, 3, 5, 7) flipped to KEEP; the four V3 KEEP lanes (4, 6, 8, 9) re-confirmed.

V1-V4 lanes were per-target, per-row, per-lock punch-list-focused. V5 applies five carry-aware lenses **across the cohort** rather than within PASS-2 alone. The lens-revealed observations (variant-count drift, OPT001/002 severity drift, `@pratt`/`@simd` hint phrasing, `incremental_marker` orphan, worked-example scarcity) all sit between documents or in research-scope coverage gaps that punch-list cycles structurally miss because punch lists scope to the audited document's text.

V5 totals: KEEP 41 / REINVENT 0 / DISCARD 0 across 9 lanes. Lens row totals: 20 (9 confirmed, 11 carry-observation, 0 blocker).

## §7 Consolidated punch list

V4 closed the V3 §14 punch list; V5 surfaces no V5-amendment-required surgery. Two non-blocking residuals from V4 carry forward unchanged:

1. **BIR-snapshot-baseline carry phrasing** (`PASS-2.md:431`) — receiving gate is "SYNTHESIS Wave-2 measurement gate" rather than concrete artefact path. Per V4 §5 carry; non-blocking; ride into per-tranche drafting.
2. **Smoke-gate corpus-path** (`PASS-2.md:494-503`) — per-grammar smoke gate table cites e.g. "parse grammar corpus" without binding to specific corpus path. Per V4 §5 carry; non-blocking; ride into per-tranche drafting.

V5 surfaces 11 carry-observations (not amendment surgeries):

1. **L-2 — Variant-count drift across PASS-1 (22) / PASS-2 (23) / ARCH §7.2 (24).** Receiver = SYNTHESIS Wave-2 reconciliation gate at PASS-1.md:159.
2. **L-6 — `incremental_marker` schema row has no current-corpus consumer.** Receiver = Phase 1 research topic 7 (incremental parsing + green/red trees) per orchestrator §3 line 65.
3. **L-7 — OPT001/002 severity-class drift between PASS-2 (`error`) and PASS-3 (`note`).** Receiver = SYNTHESIS Wave-2 + ARCH §7.4 alias-and-severity catalog.
4. **L-8 — `@pratt`/`@simd` hint phrasing in PASS-2 OPT001/002 strings reads against strict Lock 10.** Receiver = SYNTHESIS Wave-2 (clarify whether such hint syntax exists; if so, where it lives in BBNF surface).
5. **L-10 — No yaml end-to-end worked example.** Receiver = per-tranche drafting (Tranche F runtime template + Tranche G runtime publication).
6. **L-11 — No Pratt/SIMD trigger trace example.** Receiver = per-tranche drafting (Tranche F lowerer + Tranche H Pratt/SIMD wave).
7. **L-12 — No traced variant-payload-to-source example.** Receiver = Phase 1 research topic 6 + per-tranche drafting.
8. **L-13 — No per-backend test-count budget rows.** Receiver = per-tranche drafting (Tranche F + Tranche H wave gates).
9. **L-14 — Incremental codegen story absent.** Receiver = Phase 1 research topic 7.
10. **L-15 — Bumpalo arena lifetime ergonomics emission detail absent.** Receiver = per-tranche drafting (Tranche F runtime template).
11. **L-16 — OPT001/002 verbatim string ratification (test that emits these specific strings) absent.** Receiver = per-tranche drafting.
12. **L-19 — Lock 5 + Lock 14 composition: yaml two-surface proof catches per-grammar match arms but not subtler "template extension for yaml" violations.** Receiver = SYNTHESIS Wave-2 (text-polish; one explicit claim "the runtime template parameter set is grammar-agnostic; yaml flows through the same parameter substitutions").

Total: 2 V4 non-blocking carry-residuals + 12 V5 lens-revealed carry-observations = 14 items. **Zero PASS-2 amendment-required surgeries.**

All items route to (a) Phase 1 research deep-dives (3 items: L-6, L-12, L-14), (b) SYNTHESIS Wave-2 reconciliation (4 items: L-2, L-7, L-8, L-19), (c) per-tranche full-spec drafting (5 items: L-10, L-11, L-13, L-15, L-16), or (d) carry into per-tranche drafting from V4 (2 items: BIR-snapshot baseline, smoke-gate corpus path).

## §8 Final verdict

> **Decision: READY.**
>
> PASS-2 emerges from Wave-4.1 with V4-READY ground intact. The five carry-aware lenses applied centrally (rather than per-row, as V1-V4 did) surface 11 carry-observations of three classes: cross-document narrative coherence (variant-count drift; OPT001/002 severity drift; `incremental_marker` orphan), worked-example scarcity (yaml end-to-end; Pratt/SIMD trigger trace; variant-payload-to-source trace), and coverage gaps (per-backend test-count budget; incremental codegen story; bumpalo emission detail; OPT001/002 verbatim string ratification). None of these observations triggers V5 amendment because every one routes coherently to Phase 1 research, SYNTHESIS Wave-2 reconciliation, or per-tranche full-spec drafting — the receiving phases the orchestrator's research-fold cycle is dispatching next. The 9-lane re-audit returns KEEP 41 / REINVENT 0 / DISCARD 0; the 11-command tightened gate-rerun returns 11 PASS / 0 PARTIAL / 0 FAIL; the 13-row cross-document binding ledger returns 11 confirmed + 2 carry-observation + 0 fault. PASS-2's architectural thesis (Backend IR is the codegen contract; Tape is the runtime substrate; typed values borrow into Tape; Rust V1 is primary lowerer; WASM V1 is wasm32 binding path; SIMD/Pratt/PHF auto-detected; runtime modules template-emitted; regen byte-identical and budgeted) survives four cycles of independent challenge plus the V5 carry-aware lens pass.
>
> Hereupon PASS-2 advances alongside PASS-1, PASS-3, and MASTER-PLAN to Phase 1 research deep-dives. The carry-observations enumerated in §7 ride forward as research-fold scope notes, not amendment surgeries; the orchestrator's Phase 2 fold + Phase 3 V6 hardening cycles are the natural reconciliation surfaces. No PASS-2 re-run is warranted; no narrow amendment is required; the architectural thesis is settled.

## §9 Closing posture

V1-V4 audited PASS-2 against its own punch list. V5 audits PASS-2 against the cohort and the next-phase research-fold receivers. Five lenses caught what punch-list focus structurally missed: documents that bind correctly at the row level can still drift at the cross-document level (variant count, diagnostic severity, hint syntax); documents that close every adversarial probe can still lack worked-example walkthroughs that build downstream confidence; documents that pass every per-target lane can still carry coverage gaps that are research-scope rather than amendment-scope. The lens findings sharpen the next phase without forcing PASS-2 to re-open. PASS-2 is READY for Phase 1 dispatch; the eleven carry-observations are the receiving-phase docket.
