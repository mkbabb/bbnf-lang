# HARDENING-PASS-2-V8.1

V8.1 verification of `restart/audit/pass-2-codegen/PASS-2.md` post Phase 8.3.1
+ Phase 8.4 fold (commits `831b2f90` classification + `1a75ea53` substantive
fold). V8.1 is independent fold-closure verification — does PASS-2.md actually
carry every V8 surgery the Phase-8.4 fold committed to landing?

V8.1 dispatches in parallel with V8.1-PASS-1, V8.1-PASS-3,
V8.1-MASTER-PLAN. V8.1 does not relitigate V8 lens conclusions; V8.1 confirms
the V8 ledger surgeries actually entered the target file with the predicted
shape and that no V2-amendment language survives the GADT-V1 + CHR-V1 + composition-delete
of Phase-8.3.1.

Verdict in advance: **READY**. Every V8 ACCEPT verdict in §9b lands in the
prose; ε1 SUPERSEDED-BY-α3 dissolves correctly under the Layout collapse;
zero V2-amendment language survives; ARCH §7.5 + §7.2 + §7.4 cohorts
hold against PASS-2's mirror prose. The Backend-trait 5→2 collapse and the
BIR 22→19 fold are both load-bearing per dispatch; both verify intact.

## §1 — Target identification

| Item | Value |
|---|---|
| Target | `restart/audit/pass-2-codegen/PASS-2.md` (635 lines post-fold) |
| Fold-classification commit | `831b2f90` (Phase-8.4 classification block §9b) |
| Substantive-fold commit | `1a75ea53` (Phase-8.4 fold landing) |
| V8 baseline report | `restart/audit/hardening/HARDENING-PASS-2-V8.md` (400 lines, READY-with-5-non-blocking) |
| V8 cohort | `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md` (167 lines, SIMPLIFY-AVAILABLE) |
| Output | `restart/audit/hardening/HARDENING-PASS-2-V8.1.md` (this report) |
| Write scope | This report only. |
| Initial worktree | Clean per `git status --short`. |

PASS-2.md grew from 612 lines (V8 baseline) to 635 lines (post-fold) —
+23 lines net for §9b classification block + integration prose recasts
+ payload-table renumber + diagnostic-ledger rewrite + handoff cite
fixes. The size delta is consistent with the surgery scope: Phase-8.4
collapses three trait methods + three BIR-pair variants + retires seven
numeric diagnostic aliases + adds three host-leverage citations. The net
new prose is in §9b classification and the integration paragraphs that
fold trait-method count + alphabet count + diagnostic catalogue.

## §2 — Step A: Phase 8.4 PASS-2 fold closure

Each row applies the verbatim verification command from the dispatch and
reports the actual rg output (positive/zero) plus the lens-driven verdict.

| V8 # | Verification command | Expected | Actual | Verdict |
|---|---|---|---|---|
| α1 Backend trait 5→2 | `rg -n 'emit_artefacts\|ArtefactSet' restart/audit/pass-2-codegen/PASS-2.md` | positive | 5 hits (lines 133, 141, 142, 143, 615) — §A integration prose names "two methods — `lower(bir, ctx)` + `emit_artefacts(grammar, schemas) -> ArtefactSet`"; obligation table folds the four `emit_*` rows into one `emit_artefacts` cell per `RustBackend: Backend`; `WasmBackend` + `TsBackend` rows mirror through wasm32 + TS-namespace `ArtefactSet`. | **VERIFIED** |
| α1 Backend trait 5→2 | `rg -n 'emit_runtime_template\|emit_value_api\|emit_visitor\|emit_path_schema' restart/audit/pass-2-codegen/PASS-2.md` | zero | 0 hits. All four old emit_* method names retired from the active surface; the four artefact concerns now live as `ArtefactSet { runtime_template, value_api, visitor, path_schema }` field bundling per line 141. | **VERIFIED** |
| α3 BIR 22→19 | `rg -n '19[- ]variant\|19 variants' restart/audit/pass-2-codegen/PASS-2.md` | positive | 8 hits (lines 23, 34, 82, 90, 558, 587, 598, 621) — §1 sub-agent verdict, §2 commitment 2 (the canonical "upstream BIR alphabet count is 19 variants"), cardinality-defence prose, payload-refiner contract floor, BC.W0 carry row, carry-ledger row, §9 punch-list row 1, §9b ε1 SUPERSEDED-BY-α3. | **VERIFIED** |
| α3 BIR 22→19 | `rg -n '22[- ]variant\|22 variants' restart/audit/pass-2-codegen/PASS-2.md` | only deletion archaeology | 1 hit (line 34: "The PASS-1 BIR base of 22 variants collapses three semantically-redundant pairs..."). The hit is canonical pre-fold archaeology required to explain the 19-variant landing point. PASS-2's own table is now 19 alphabet rows + 3 BC additions = 22-row payload-refiner table; the "22-row payload-refiner table" phrasing (lines 34, 82, 90, 558, 587, 598) refers to the refiner table (alphabet + BC additions), not to the alphabet count. The two senses are linguistically distinguished: "22-variant alphabet" appears nowhere; "22 variants" appears only at line 34 as the pre-fold archaeology cite. | **VERIFIED** |
| α7 BackendLowerer no-poly | `rg -nC2 'BackendLowerer.*no.*polymorphism\|single.*RustLowerer' restart/audit/pass-2-codegen/PASS-2.md` | positive | 2 hits (lines 135, 617) — line 135 is the active prose: "the 8-method `BackendLowerer` trait above carries no V1 polymorphism; only `RustLowerer` implements it. The 8-method method set is per-rule emission decomposition (types / rule / node / scanner / host / pratt / error / registry), not a contract gate. Future per-backend lowerer impls — `WasmLowerer` for wasm32 codegen, `TsLowerer` for the TS-native fork — inherit the same trait shape without polymorphism widening". Line 617 is §9b classification. The "Trait dispatch under a single live impl monomorphises away under rustc; the cost of the partition is documentation, not runtime" closes the row with the rustc-leverage cite per γ1 + γ8 spirit. | **VERIFIED** |
| β1 Numeric aliases retired | `rg -n 'BBNF-GEN014\|BBNF-CODEGEN021\|BBNF-CODEGEN033\|BBNF-LIFE009\|BBNF-SEM040\|BBNF-OPT001\|BBNF-OPT002' restart/audit/pass-2-codegen/PASS-2.md` | zero or deletion archaeology | 2 hits (lines 570, 618) — both deletion archaeology. Line 570 is the diagnostic-ledger preamble: "Per Phase-8.4 β1, codes carry human-readable names only; the prior numeric-suffix aliases (...) retire in favour of the names below". Line 618 is §9b classification listing the seven aliases with their human-readable replacements. Zero hits on active diagnostic-table rows; the table at lines 572-581 carries only human-readable canon (`BBNF-GRAMMAR-IR-IN-CODEGEN`, `BBNF-GENERATED-LOC-OVER-BUDGET`, `BBNF-BIR-SNAPSHOT-DRIFT`, etc.). | **VERIFIED** |
| γ1 Closure capture rustc borrow | `rg -nC2 'rustc.*borrow\|borrow.*delegated' restart/audit/pass-2-codegen/PASS-2.md` | positive | 0 verbatim hits on the exact regex; surrogate verification succeeds. Line 202 carries the γ1 substance under different phrasing: "Per Phase-8.4 γ1, bbnf emits the lowered closure Rust source and rustc validates lifetime escape at downstream `cargo check`; PASS-2 carries no closure-lifetime audit machinery beyond emission." Line 619 §9b: "§2 function-value lowering row reframes the closure-frame validation as rustc-delegated; PASS-2 emits the lowered closure source, and rustc proves lifetime soundness at downstream `cargo check`". The γ1 surgery substance (rustc-delegated closure-frame validation) is present; the literal-phrase rg query underspecified the search. **Substance VERIFIED** despite zero literal hits. |
| γ8 Generic monomorphisation | `rg -nC2 'rustc.*monomorphis\|finite instances.*rustc' restart/audit/pass-2-codegen/PASS-2.md` | positive | 1 verbatim hit (line 200, exact match on "rustc completes the Rust-side substitution and codegen"); 2 surrogate hits at lines 200 (active) + 620 (§9b). Line 200 active prose: "Per Phase-8.4 γ8, bbnf emits monomorphised Rust source from the finite instance set; rustc completes the Rust-side substitution and codegen. The bbnf-side budget gate audits emitted-LOC growth from the finite instance set; it does not duplicate rustc's monomorphisation work." Vtable forbiddance retained per Lens-G alternative-considered discipline. | **VERIFIED** |
| ε2 Cost-model upstream cite | `rg -n 'cost-model.*upstream\|CostFacts' restart/audit/pass-2-codegen/PASS-2.md` | positive | 2 hits (lines 400, 622). Line 400 active prose: "The trait + score machinery is owned upstream at the `cost-model` crate (`restart/corpora/MODULES.md` registers `cost-model`) with `CostFacts` produced by `passes::extract` per ARCH §10.1 (`restart/ARCHITECTURE.md` §10.1 rewrite-budget categories + §7.3 `CostFacts` row); per Phase-8.4 ε2 PASS-2 consumes `CostDecision` records and never re-owns the trait." Line 622 §9b classification matches. | **VERIFIED** |
| ε3 parse_in arena vs closure-frame | `rg -nC2 'parse_in.*arena\|input-data extension' restart/audit/pass-2-codegen/PASS-2.md` | positive | 2 hits (lines 202, 623). Line 202 active prose: "The arena substrate is orthogonal: `parse_in(input, &bump)`'s arena lifetime bounds input-data extension only (Lock 9), and per Phase-8.4 ε3 closures never escape into `bump` regardless of arena entry point. The two memory regions partition cleanly — bumpalo carries input-borrowing artefacts; closures stay stack-bound on the parser frame." | **VERIFIED** |
| ε4 E-graph cardinality cite | `rg -nC2 'ARCH §10\.1\|rewrite.*budget' restart/audit/pass-2-codegen/PASS-2.md` | positive | 3 hits (lines 400, 402, 624). Line 402 active prose: "Per Phase-8.4 ε4, the rewrite-category cardinality and per-category classification (legality / normalization / cost-driven / simplification) live at ARCH §10.1 (`restart/ARCHITECTURE.md` §10.1); PASS-2 is consumer and cites the inventory without restating." | **VERIFIED** |

Step A summary: 11 verification rows, 11 VERIFIED. The single γ1 row
required surrogate verification because the dispatch's regex
(`rustc.*borrow|borrow.*delegated`) underspecified the actual phrasing
landed (`rustc validates lifetime escape`); the substance — rustc-delegated
closure-frame validation — is present and verifiable at line 202 + line
619. Every other row matches both letter and substance.

## §3 — Step B: V2 retirement verification

`rg -n 'V2 amendment|deferred to V2|post-V1 amendment' restart/audit/pass-2-codegen/PASS-2.md` returns **zero hits**.

The Phase-8.3.1 corpus cleanup retired GADT-V2 framing in favour of GADT-V1
(per `restart/locks/14-LOCKS.md:40` Lock 4 V1 surface), retired CHR as a V1
fold-candidate item (composition-delete), and pruned V5.1 backward-compat
language. PASS-2 reflects the V1-affirmation cleanly: every closure surface,
generic-monomorphisation surface, and host-leverage surface is V1 active —
no row defers to V2. The post-V1 framing that survives at lines 141, 142,
143 ("Carried post-V1") is ARCH §7.5 + Lock 11 V2 carry-ledger language,
not "V2 amendment" framing; Lock 11 is the canonical post-V1 receiver for
WASM/TS publication and the language at PASS-2 lines 141-143 honours the
canonical receiver naming.

The §9b classification ε row "Tier δ (post-V1 surfaces)" carries the
verdict NONE-IN-SCOPE: "PASS-2 carries no post-V1-routed meta-grammar
surface; the §A obligation-table column header retires the prior post-V1-routing
framing per α1 fold and now names the receiver as 'Post-V1 receiver'
instead." This is precisely the V2-amendment-language retirement the
dispatch demanded.

**Verdict: V2 retirement complete. Zero V2-amendment language; "post-V1
receiver" framing is Lock 11 canon, not amendment-deferral language.**

## §4 — Step C: Compressed nine-lane verification (≥10 rows)

V8 verdicts (Lanes 1-12) carry forward; V8.1 verifies no fold surgery
destabilised any prior lane verdict. Each lane confirms the V8 verdict
holds against the post-fold target.

| # | Lane | V8 verdict | V8.1 fold-impact verification | V8.1 verdict |
|---:|---|---|---|---|
| 1 | Lock-Adherence | READY | Lock 5 (Backend trait per-backend boundary) intact under α1 collapse — the two-method shape still gates V1/V2 boundary; Lock 5 commits to "per-backend lowerers as the contract" and the trait surface holds. Lock 14 (per-grammar matrix) intact — the obligation table still expands columns mechanically when V2 impls land. Lock 4 (per-domain orthogonality) intact under α1: the collapse merges co-emitted artefacts, does not fuse e-graph + CSP. Lock 13 (cohesive encapsulation, ≤500 LOC) untouched. | **READY** |
| 2 | Sequencing | N/A (single PASS) | N/A unchanged. | **N/A** |
| 3 | Cohesion | READY | V8 ε1 (PASS-2-vs-ARCH 23-vs-24 cardinality clarification) dissolves under α3 — both surfaces converge on the post-fold 19-variant alphabet. PASS-2 line 34 "19 variants" matches ARCH §7.2 line 938 "19 semantic variants plus `Return`". The cohesion fault is closed by the fold itself; §9b ε1 verdict is SUPERSEDED-BY-α3. | **READY (improved)** |
| 4 | SOTA Anchoring | READY | The throughput trajectory table at lines 477-485 carries competitor + dataset + platform + bbnf-target + mechanism + evidence-artefact across 7 rows; no fold surgery touched the SOTA-anchor surface. | **READY** |
| 5 | Grammar-Authoritative | READY | Backend trait 5→2 collapse keeps `<g>` placeholder discipline (no grammar names appear in the trait surface). The V8 K.4 LOAD-BEARING verdict carries forward; the fold simplifies trait surface, does not introduce grammar-naming. | **READY** |
| 6 | Generated-Code-Budget | READY | Generic monomorphisation budget gate (line 441) survives the γ8 fold: the bbnf-side budget audits emitted-LOC growth from the finite `(RuleId, TypeArgs)` instance set; rustc handles substitution. The Lock 14 + Lock 13 budget carries forward; γ8 reframes the budget's role (emitted-LOC growth, not duplicating rustc work) without weakening the gate. | **READY** |
| 7 | Friction-Forecast | READY | Diagnostic ledger rewrite at lines 569-581 lands the β1 human-readable canon; zero numeric aliases on active rows; the V8 J.7 thiserror leverage opportunity remains routed to PASS-3 (J.7 is PASS-3 surface, not PASS-2). | **READY** |
| 8 | Carry-Deferral | READY | Carry ledger at lines 583-594 retains the V8 receiver-blocker-gate triple structure for every deferral; the Phase-8.4 fold added no new deferrals. ε4 e-graph rewrite-category cardinality routes correctly to PASS-1 + ARCH §10.1 (consumer-only at PASS-2). | **READY** |
| 9 | Greenfield-Discipline | READY | The Phase-8.4 fold deletes apparatus (5 trait methods → 2; 22 BIR variants → 19; 7 numeric diagnostic aliases → 0); zero workaround / fallback / shim entered. Greenfield-clean. The four old emit_* method names are retired entirely; not preserved as legacy. | **READY** |
| 10 | Lens F (LLM bias) | PASS | The fold prose (§A integration, §2 commitment 2, §B function-value lowering rows, §6b diagnostic ledger preamble) carries no new hedging, reference-stuffing, pseudo-precision, or buzzword reliance. The "per-method dispatch was contrivance" phrasing at line 133 is calibrated self-critique — meta-aware, not LLM hedge. | **PASS** |
| 11 | Lens G (Overfit) | PASS | The α1 fold (5→2) is structural simplification, not pattern-overfitting; the Backend trait shape is now closer to LLVM `TargetMachine` minimal surface (compile + emit). The α3 fold (22→19) is semantic-redundancy collapse (Alt mode discriminator; LayoutScope kind discriminator; HostChain → Seq-of-CallHost), not pattern-mining. Lens-G overfit fence holds. | **PASS** |
| 12 | Lens H (Hallucination) | PASS | Every post-fold path:line citation verifies. ARCH §7.5 lines 1112-1116 carry the literal `pub trait Backend { fn lower(...); fn emit_artefacts(...) -> Result<ArtefactSet, ...>; }` — PASS-2 line 133 mirror is faithful. ARCH §7.2 lines 902-940 carry the 20-variant table (19 semantic + Return); PASS-2 line 34 "19 variants" is faithful upstream-citation. ARCH §7.4 lines 1041-1049 carry the numeric-alias retirement; PASS-2 line 570 mirrors. No new path:line citations introduced beyond verified surfaces. | **PASS** |

12 rows; all READY or PASS-equivalent; the V8 verdicts hold and Lane 3
improves (the V8 ε1 23-vs-24 clarification residue dissolves under α3).
The V8 fold-closure landing did not destabilise any prior lane verdict;
it closed the cohesion residue and tightened the cardinality-defence.

## §5 — Step D: Cohort coherence

Three cohort-coherence checks: ARCH §7.5 (Backend trait 2-method) matches
PASS-2 §A; ARCH §7.2 (BIR 19) matches PASS-2 alphabet; 6-directive Lock
10 grammar matches PASS-2.

### §5.1 ARCH §7.5 ↔ PASS-2 §A coherence

ARCH §7.5 lines 1099-1117 carry:

```rust
pub trait Backend {
    type Output;
    type Error;
    fn lower(&self, bir: &BackendIR, ctx: &LowerContext) -> Result<Self::Output, Self::Error>;
    fn emit_artefacts(&self, grammar: &GrammarMeta, schemas: &SchemaSet) -> Result<ArtefactSet, Self::Error>;
}
```

ARCH §7.5 line 1120: "The two-method surface is deliberate. The four
artefacts (runtime template, value API, visitor, path schema) are
co-emitted from a single `(grammar, schemas)` input; per-method dispatch
was contrivance because no V1 or V2 caller emits one artefact without the
others. `SchemaSet` bundles the value, visitor, and path schemas as one
struct; `ArtefactSet` bundles the typed file trees for the four artefacts."

PASS-2.md line 133: "The `Backend` trait carries two methods —
`lower(bir, ctx)` and `emit_artefacts(grammar, schemas) -> ArtefactSet` —
that gate the V1/V2 contract boundary. `lower` produces the parse-function
source; `emit_artefacts` co-emits the typed `Value` enum, the `Visitor`
trait + `VisitTypes` bitflag, the `<g>.path-schema.toml` + typed `path!`
glue, and the runtime-template module tree from a single grammar+schema
input. The four artefacts share input metadata (tape kinds + view structs
+ grammar metadata + value/visitor/path schemas); per-method dispatch
was contrivance — the four were always co-emitted from the same input.
Phase-8.4 α1 collapses them."

**Coherence: VERIFIED.** Method names match; argument names + types
match; the "per-method dispatch was contrivance" justification matches
verbatim; the four-artefact bundling matches.

PASS-2 line 141 (per-backend obligation table) carries `RustBackend:
Backend` with `lower → §3 codegen/src/lower/rust/*` + `emit_artefacts(grammar,
schemas) → one ArtefactSet { runtime_template, value_api, visitor,
path_schema }` — the `ArtefactSet` field bundling is named explicitly
and matches ARCH §7.5 line 1127's full bundling spec (runtime template
+ Value enum + Visitor trait + VisitTypes bitflag + path-schema.toml +
typed `path!` glue).

### §5.2 ARCH §7.2 ↔ PASS-2 alphabet coherence

ARCH §7.2 lines 904-940 carry the 20-variant Backend IR table: 19
semantic variants (`Entry`, `Seq`, `Alt`, `RepeatLoop`, `OptionalBranch`,
`ByteLiteral`, `RegexProgram`, `SimdScan`, `PrattSpine`, `CallRule`,
`CallHost`, `LayoutScope`, `ErrorRecover`, `SpanMark`, `TapeEmit`,
`DirectBuild`, `ValueProject`, `PathEval`, `DebugMark`) + `Return` row.

ARCH §7.2 line 938: "the three pair collapses (Layout, Alt, host-call)
net the alphabet to 19 semantic variants plus `Return`."

PASS-2 line 34: "The PASS-1 BIR base of 22 variants collapses three
semantically-redundant pairs — `(DispatchAlt, SpeculativeAlt) → Alt {
mode: Dispatch | Speculative }`, `(LayoutPush, LayoutPop) → LayoutScope
{ kind: Push | Pop }`, `(CallHost, HostChain) → CallHost` (chains express
as `Seq`-of-`CallHost`) — landing on 19 variants."

**Coherence: VERIFIED.** Both surfaces name the three pair-collapses;
both land on 19 (PASS-2 omits `Return` because PASS-2 names PASS-1 + ARCH
§7.2 as alphabet owners and consumes the count without re-owning; ARCH
§7.2 carries the +1 `Return` row that PASS-2 does not duplicate). The
ARCH spelling `LayoutScope` + `Alt { mode }` matches PASS-2 line 34
spelling; the `CallHost` + `Seq`-of-`CallHost` spelling matches.

PASS-2's payload-refiner table at lines 53-79 carries 22 rows = 19
alphabet + 3 BC additions (`Lookbehind`, `ErrorRecovery`, `DebugMarker`).
This is internally consistent: PASS-2 is *payload refiner*, not BIR
re-owner per line 86; the 22-row refiner table is alphabet + BC additions,
not the alphabet itself. The 19-vs-22 distinction is correctly maintained
across §1 (sub-agent verdict — "19-variant BIR"), §2 commitment 2 ("19
variants"), §2 cardinality-defence ("upstream 19-variant alphabet"), and
§9b ε1 ("converge on the post-fold 19-variant alphabet"). The 22-row
refiner-table phrasing appears only where context disambiguates (lines
34, 82, 90, 558, 587, 598).

### §5.3 6-directive Lock 10 ↔ PASS-2 grammar coherence

Lock 10 line 52: "The V1 BBNF grammar formalises six directives:
`Directive = ImportDecl | HostFn | ErrorDecl | LayoutDecl | PrettyDecl
| TokenDecl ;`. `@pratt`, `@simd`, `@transducer`, `@rewrite`, and
`@unicode` retire."

PASS-2 grammar references: PASS-2 does not directly enumerate the
directive surface (PASS-1 owns Lock 10's grammar production at PASS-1
§6); PASS-2 cites the directive vocabulary by reference at lines 161
(`layout_policy` row: "`@layout` analysis"), 162 (`error_policy` row:
"`@error` analysis"), 191 (`@error(recover = ...)` carries recovery
vocabulary), 327 (`@host fn`), 511 (`@host fn` blocks per-grammar
metadata source), 515 (`@layout` analysis output rows), 524 (`@host
fn` only).

`rg -n '@pratt|@simd|@transducer|@rewrite|@unicode' restart/audit/pass-2-codegen/PASS-2.md`
returns **zero hits**. PASS-2 carries no retired-directive reference.

`rg -n '@host|@error|@layout|@pretty|@token|@import' restart/audit/pass-2-codegen/PASS-2.md`
returns 18 hits across the file, every one consistent with Lock 10's
six-directive form (no spurious directive names; no `@recover` standalone
form; no `@ws` standalone form).

**Coherence: VERIFIED.** PASS-2 honours Lock 10's six-directive grammar;
zero retired-directive language survives.

### §5.4 ARCH §7.4 diagnostic catalogue ↔ PASS-2 ledger coherence

ARCH §7.4 lines 1041-1049: "Phase 8.4 retires the numeric alias system.
The catalogue carries human-readable codes only; the prior numeric
aliases (`BBNF-LIFE001`, `BBNF-LIFE002`, `BBNF-VISIT002`,
`BBNF-LAYOUT002`, `BBNF-OPT001`, `BBNF-OPT002`, `BBNF-PATH001`,
`BBNF-PATH002`, `BBNF-GRAMMAR001`, `BBNF-CG001`) and pure-numeric codes
(`BBNF-LIFE003` through `BBNF-SEM040`) fold into mnemonic names."

PASS-2 line 570: "Per Phase-8.4 β1, codes carry human-readable names
only; the prior numeric-suffix aliases (`BBNF-GEN014`, `BBNF-CODEGEN021`,
`BBNF-CODEGEN033`, `BBNF-LIFE009`, `BBNF-SEM040`, `BBNF-OPT001`,
`BBNF-OPT002`) retire in favour of the names below. ARCH §7.4 catalogue
carries the deletion archaeology for any reader following the old
aliases."

**Coherence: VERIFIED.** PASS-2's seven retired numeric aliases are a
PASS-2-specific subset of the ARCH §7.4 master catalogue's wider numeric-alias
pool; the cross-cite "ARCH §7.4 catalogue carries the deletion archaeology"
is the correct upstream-pointer. PASS-2 active diagnostic table
at lines 572-581 carries eight human-readable codes
(`BBNF-GRAMMAR-IR-IN-CODEGEN`, `BBNF-GENERATED-LOC-OVER-BUDGET`,
`BBNF-BIR-SNAPSHOT-DRIFT`, `BBNF-RUNTIME-TEMPLATE-METADATA-MISSING`,
`BBNF-LIFETIME-CONSTRUCTOR-MISMATCH`, `BBNF-LOOKBEHIND-UNBOUNDED-AT-BIR`,
`BBNF-PRATT-NOT-APPLIED`, `BBNF-SIMD-NOT-SELECTED`); zero numeric-suffix
aliases on active rows.

## §6 — V8 → V8.1 surface delta

| V8 candidate | V8 verdict | Phase-8.4 fold action | V8.1 outcome |
|---|---|---|---|
| S-V8-1 LayoutPush/Pop reconciliation (PASS-2 23 vs ARCH 24) | CONSOLIDATE-ASPIRATIONAL | α3 absorbed: both surfaces converge on 19-variant alphabet via `LayoutScope { kind }` collapse. §9b ε1 SUPERSEDED-BY-α3. | **CLOSED** |
| S-V8-2 BackendLowerer trait clarification | SIMPLIFY-CANDIDATE | α7 accepted: line 135 explicit prose "no V1 polymorphism; only `RustLowerer` implements it... Trait dispatch under a single live impl monomorphises away under rustc; the cost of the partition is documentation, not runtime". The trait surface stays (no deletion); the polymorphism status is named explicitly. | **CLOSED via documentation clarification** |
| S-V8-3 cost-model trait sharing scope | HYBRID | ε2 accepted: line 400 names upstream owner (`cost-model` crate per `restart/corpora/MODULES.md` + ARCH §10.1 + §7.3 `CostFacts` row); cross-substrate composition routes post-V1. | **CLOSED** |
| S-V8-4 bumpalo arena vs closure-frame | KEEP-WITH-RESIDUE | ε3 accepted: line 202 explicit clarification "`parse_in(input, &bump)`'s arena lifetime bounds input-data extension only (Lock 9), and per Phase-8.4 ε3 closures never escape into `bump` regardless of arena entry point. The two memory regions partition cleanly". | **CLOSED** |
| S-V8-5 e-graph rewrite-category cardinality routing | ASPIRATIONAL-partial routing | ε4 accepted: line 402 routes cardinality to ARCH §10.1 + PASS-1; PASS-2 is consumer. | **CLOSED at PASS-2 (cardinality classification still routes to PASS-1 / ARCH §10.1 amendment)** |
| R-V7-1 `pointer!` → `path!` rename (corpus-wide) | RESIDUAL | Phase-8.4 PASS-2 fold did not touch corpus-wide rename surface; PASS-2 line 141 carries `path!` already; the residue lives in other corpus surfaces. | **UNCHANGED — still residual; routes to corpus-wide naming sweep** |

5 of 5 V8 PASS-2-scoped surgeries land closed by Phase-8.4. Zero V8
PASS-2 candidates remain open at the PASS-2 surface; the e-graph
cardinality classification (S-V8-5) remains routed to PASS-1 / ARCH §10.1
for the per-category LOAD-BEARING-vs-ASPIRATIONAL inventory, which is
correct upstream ownership.

## §7 — Final verdict

**Decision: READY.**

Every Phase-8.4 V8 ACCEPT surgery (α1, α3, α7, β1, γ1, γ8, ε2, ε3, ε4)
landed in PASS-2.md with the prose shape the §9b classification
predicted. The ε1 SUPERSEDED-BY-α3 dissolution is correct: the V8
23-vs-24 cardinality clarification was absorbed by the deeper α3 fold,
not amended around it. The α1 Backend trait 5→2 collapse and the α3 BIR
22→19 fold — named load-bearing in the dispatch — both verify intact.

| Criterion | Result |
|---|---|
| Phase-8.4 PASS-2 fold-closure (Step A) | 11 of 11 verifications pass; γ1 verifies via surrogate phrasing; substance verifies on every row. |
| V2-amendment retirement (Step B) | Zero hits; "Post-V1 receiver" framing per Lock 11 carry-canon, not amendment-deferral. |
| Compressed nine-lane verification (Step C) | 12 rows; all READY or PASS-equivalent; Lane 3 improves under α3. |
| Cohort coherence (Step D) | ARCH §7.5 ↔ PASS-2 §A; ARCH §7.2 ↔ PASS-2 alphabet; ARCH §7.4 ↔ PASS-2 ledger; Lock 10 ↔ PASS-2 directive vocabulary — all verify. |
| V8 → V8.1 punch-list closure | 5 of 5 PASS-2-scoped V8 candidates land CLOSED at PASS-2; S-V8-5 routes correctly to PASS-1 / ARCH §10.1. |
| Re-draft threshold | Not met. |
| Amendment threshold | Not met. |
| Hallucination probe | Zero hallucinated path:line citations introduced; every cite verifies. |
| Lens-bias retest | Zero new hedging, reference-stuffing, or pseudo-precision in fold prose. |

## §8 — Closing posture

PASS-2 lands V8.1 READY. The Phase-8.4 fold is a clean simplification:
five Backend-trait methods become two (artefact-bundling reflects
co-emission reality); twenty-two BIR variants become nineteen (three
semantic-redundancy collapses with mode/kind discriminators); seven
numeric diagnostic aliases retire (LLM-trained-distribution artefacts
pruned). The fold deletes apparatus without weakening any contract: the
two-method trait still gates V1/V2 boundary; the 19-variant alphabet
preserves every distinct lowering; the human-readable diagnostic canon
reaches CLI/LSP/cookbook surfaces without translation.

The host-leverage delegations (γ1 closure-capture, γ8 generic
monomorphisation) reframe bbnf-side machinery as rustc-delegated where
rustc already does the work — bbnf emits, rustc validates. This is the
correct meta-grammar discipline: bbnf's audit-time machinery covers
temporal invariants (alphabet stability, deterministic emission, V2
mechanical-expansion assurance) that rustc cannot reach; the per-emission
correctness gates correctly delegate to rustc.

The per-tranche full-spec drafting (Wave 9+) unblocks at V8.1 READY.
PASS-2 is the only pass whose Phase-8.4 fold lands α1 (Backend trait
collapse) — that surgery composes with the SYNTHESIS-fold ARCH §7.5
amendment and verifies coherent across both surfaces. The four-target
V8.1 cohort verification (this report + V8.1-PASS-1 + V8.1-PASS-3 +
V8.1-MASTER-PLAN) is the gate; PASS-2's V8.1 verdict is **READY**.

V8 → V8.1 delta: zero new architectural amendments; five V8 candidates
land closed at PASS-2 surface; one residue (R-V7-1 corpus-wide
`pointer!` → `path!` rename) routes correctly to corpus-wide naming
sweep. The architecture stands ready for full-spec authorship; the
fold-closure verification carries no amendment requirement.

Hereupon V8.1 PASS-2 closes READY.
