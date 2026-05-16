# Reviewer B — Architectural-Integrity Audit Of The Post-Hardening Synthesis Trio

## §1 Audit Target Identification

Reviewer B audits whether the architectural commitments survive when the
consolidated four-target hardening's amendments land. The substrate identity,
the IR contract, the tape + direct union, the path crate triplet, the layout
vocabulary, and the workspace shape are the constraints under verification.

| Audit input | Path | Lines | Role |
|---|---|---:|---|
| Gestalt anchor | `restart/README.md` | 475 | Greenfield manifesto; settled positions; locks summary; performance gates; voice. |
| Lock corpus | `restart/locks/LOCKS.md` | 249 | Lock 1 reframe; Lock 2 layout-lowering canon; Lock 5 IR contract; Lock 7 path consolidation; Lock 13 tree shape; Lock 14 grammar generalisation. |
| Architecture | `restart/ARCHITECTURE.md` | 1259 | Workspace shape (§1), DAG (§2), public API (§3), private internals (§4), Cargo schema (§5), pipeline (§6), IR contract (§7), BBNF surface (§8), runtime (§9), codegen (§10), perf (§11), future-grammar (§12). |
| Master plan | `restart/MASTER-PLAN.md` | 727 | Verdict ledger (§1), workspace (§2), gates (§4), tranche A-J set (§5-§15), schema handoff (§16), commit chain (§17), migration timeline (§18), risk register (§23), carry ledger (§24). |
| Migration | `restart/MIGRATION.md` | 740 | Disposition alphabet (§1), aggregate counts (§2), per-crate dispositions (§3), root metadata (§4), `core` disposition (§5), gates (§19). |
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` | 235 | Substrate verdicts; Grammar IR variant table; per-crate `src/` proposals. |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` | 467 | Backend IR 23-variant table; per-crate trees; runtime template parameter schema; SIMD coverage. |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` | 382 | User runtime; tape/direct union; path/select DSL; `bbnf` aggregator tree; benchmark rows. |
| Hardening consolidated | `restart/audit/hardening/HARDENING-CONSOLIDATED.md` | 619 | 13 cross-target conflicts; 47-row consolidated punch list; AMENDMENT-REQUIRED verdict; rerun checklist. |

The lane-by-lane verification below treats the post-amendment shape — i.e.,
ARCHITECTURE / MASTER-PLAN / MIGRATION as written today combined with the
HARDENING-CONSOLIDATED §3 conflict resolutions and §4 punch list — as the
candidate architecture. Reviewer B verifies whether that candidate is
internally coherent.

---

## §2 Lane 1 — Backend IR Ownership Coherence

The consolidated §3 conflict #1 says PASS-2 places `codegen/src/backend_ir/`
under codegen; MASTER-PLAN treats Backend IR as the lowerer contract; README
gives `ir` ownership. The resolution is to move BIR to `ir/src/backend_ir/`.

| Claim | Source | Verification | Verdict |
|---|---|---|---|
| README says `ir` owns Grammar IR + Backend IR types (no passes). | `restart/README.md:43` | Reads "`ir` — Grammar IR + Backend IR types (no passes) — workspace-internal". Single owner stated. | HONOURED |
| ARCHITECTURE places BIR types under `ir/src/backend_ir/`. | `restart/ARCHITECTURE.md:407-414` | The `ir/src/` tree contains `lib.rs`, `grammar_ir/`, `backend_ir/`, `side_tables/`, `validate/`, `pretty/`. BIR types live in `ir`. | HONOURED |
| ARCHITECTURE's `codegen/src/` tree has no `backend_ir/` directory. | `restart/ARCHITECTURE.md:443-450` | The `codegen/src/` tree contains `lib.rs`, `lower/`, `rust/`, `wasm/`, `simd/`, `templates/`, `verify/`. No backend_ir. | HONOURED |
| ARCHITECTURE's BIR invariant requires lowerers to never inspect Grammar IR. | `restart/ARCHITECTURE.md:924` | Reads "Lowerers never inspect Grammar IR. Compile-time module boundary, import-deny tests, and `ir::backend_ir` snapshots." | HONOURED |
| MASTER-PLAN's tranche E carries Backend IR ownership through `ir`. | `restart/MASTER-PLAN.md:353-355` | Hard close runs `cargo test -p ir backend_ir` and `cargo test -p codegen backend_lowerer_boundary`. The variant set lives in `ir`; codegen tests boundary. | HONOURED |
| MIGRATION assigns `ir/src/backend_ir` as the destination of new BIR types. | `restart/MIGRATION.md:179, 330` | Both rows route Backend IR pieces to `ir/src/backend_ir`. PASS-2 BIR snapshots must "live under `ir::backend_ir` and feed codegen import-deny tests" (`MIGRATION.md:727`). | HONOURED |
| PASS-2 retains a `codegen/src/backend_ir/` directory. | `restart/audit/pass-2-codegen/PASS-2.md:158-160` | The PASS-2 codegen tree begins `codegen/src/`, `backend_ir/`, `README.md`. Documentation only ("a `codegen/src/backend_ir/README.md` is documentation only: it records the import boundary and points contributors to `ir::backend_ir`" at line 190). | TENSION — see below |
| PASS-2 declares `Backend IR types live in ir`. | `restart/audit/pass-2-codegen/PASS-2.md:190` | Reads "Backend IR types live in `ir`, because Lock 5 makes BIR the cross-crate contract." Affirmative. | HONOURED |
| Hardening lowerer-import-deny gate exists in MIGRATION. | `restart/MIGRATION.md:657-658` | Migration §19.3 runs `rg "GrammarIr\|GrammarIR\|grammar_ir" crates/codegen/src` and asserts no codegen import of Grammar IR. The grep is exact for the conflict's gate. | HONOURED |
| Punch item #2 (lowerer import-deny) lands in PASS-2 surface. | `restart/audit/pass-2-codegen/PASS-2.md:194-199` | Import-deny floor table specifies the four gates. | HONOURED |

**Tension under verification.** PASS-2's `codegen/src/backend_ir/` README-only
directory is structurally unnecessary post-amendment because the import-deny
gate at MIGRATION.md:657-658 enforces the boundary at the type level, and
ARCHITECTURE's codegen tree at line 443-450 omits the directory entirely. The
two trees do not match. PASS-2 retains the documentation-only directory as a
contributor pointer; ARCHITECTURE removes it.

The remediation is a one-line clarification in PASS-2 stating that the
documentation-only directory is a PASS-2 implementation hint, not a
contractual artefact: ARCHITECTURE owns the structural truth, and the README
file exists in `codegen/src/backend_ir/` only as a redirect — PASS-2 acquires
no ownership of the BIR alphabet by carrying that file. The hardening
consolidated already implies this in punch item #1 ("Move Backend IR type
definitions and variant ownership to `ir/src/backend_ir/`; keep `codegen`
limited to lowerers, adapters, snapshots, and emission tests" at HARDENING-
CONSOLIDATED.md:167). The structural decoupling is correct; the residual is
PASS-2's optional pointer file.

**Verdict.** Backend IR ownership is coherent post-amendment. ARCHITECTURE,
MASTER-PLAN, and MIGRATION all locate BIR types under `ir/src/backend_ir/`;
PASS-2 acknowledges this in body prose; the import-deny gate is greppable.
The only residual is PASS-2's documentation-only directory, which the
hardening consolidated treats as a non-blocking surface.

---

## §3 Lane 2 — Tape + Direct-To-Struct Union Substrate

The README §8 names the tape + direct-to-struct union as the architectural
keystone of PASS-3. The user has directed: "tape is fine if we can implement
it properly." The lane verifies (a) tape lives at `runtime/src/tape/`,
(b) typed values borrow `&'i Tape<'i>` + index, (c) per-grammar runtime
modules are template-emitted under `runtime/src/grammars/<name>/`, (d) one
materialisation surface; no parallel substrate, (e) same-wave consumer wiring
at Tranche F.

| Claim | Source | Verification | Verdict |
|---|---|---|---|
| Tape lives at `runtime/src/tape/`. | `restart/locks/LOCKS.md:34`; `restart/ARCHITECTURE.md:454, 1062`; `restart/audit/pass-2-codegen/PASS-2.md:204-209` | Lock 1 places tape at `runtime/src/tape/`. ARCHITECTURE §4.3 codegen/runtime tree carries `runtime/src/`, `tape/`. §9.1 explicitly names `runtime/src/tape` and lists modules `token`, `builder`, `span`, `view`, `trace`. PASS-2's runtime tree carries `runtime/src/tape/{mod.rs, node.rs, payload.rs, checkpoint.rs}`. | HONOURED |
| Typed values borrow `&'i Tape<'i>` + index. | `restart/README.md:296-308`; `restart/audit/pass-3-runtime/PASS-3.md:107-131` | README spells out `JsonValue<'i>` with `tape: &'i Tape<'i>` and `idx: u32`. PASS-3 illustrates `pub struct ValueRef<'doc, 'input, K = AnyKind> { tape: &'doc Tape<'input>, index: u32, _kind: PhantomData<K> }`. | HONOURED |
| Per-grammar runtime modules at `runtime/src/grammars/<name>/`. | `restart/ARCHITECTURE.md:1085-1099`; `restart/audit/pass-2-codegen/PASS-2.md:220-226`; `restart/MIGRATION.md:282-285, 466` | ARCHITECTURE §9.2 names `runtime/src/grammars/<grammar>/` with `mod.rs`, `generated.rs`, `view.rs`, `value.rs`, `visitor.rs`, `host.rs` and confirms "files are template-emitted. They are not hand-written production crates." PASS-2 carries `runtime/src/grammars/<name>/{mod.rs, generated.rs, parser.rs, host.rs}`. MIGRATION §5.3 disposes `crates/core/src/runtime/<grammar>/**` to `runtime/src/grammars/<name>/**` as GENERATED-REPLACE. | HONOURED |
| One materialisation surface; no parallel substrate. | `restart/locks/LOCKS.md:34`; `restart/README.md:285-314`; `restart/audit/pass-3-runtime/PASS-3.md:31, 80, 96-135` | Lock 1's reframe is explicit: "no parallel substrate; no orthogonal codepath; no `Vec<OpenFrame>::clone` pathology". README §8: "no orthogonal codepath; no parallel substrate; no Vec<OpenFrame> ladder. One representation; one materialisation surface; one Visitor pattern." PASS-3 §4 Verdict on tape/direct union: "Direct structs and tape are not competing products. Direct roots are what normal users author against. Tape-backed `ValueRef` is the shared cursor for `pointer!`, `select!`, visitors, debugger, CLI projections, LSP features, and playground inspection." | HONOURED |
| OpenFrame clone-stack discipline. | `restart/ARCHITECTURE.md:871, 926, 1075, 1138`; `restart/MASTER-PLAN.md:113, 244, 254, 628`; `restart/MIGRATION.md:284-289, 669` | ARCHITECTURE BIR `SpeculativeAlt` invariant: "Must not clone OpenFrame stacks." Backend IR invariant: "OpenFrame clone stacks are absent." Tape invariant: "Rollback is bounded and does not clone OpenFrame stacks." MASTER-PLAN B.W1 gate: "Speculative branch test without OpenFrame clone." Hard close grep: `rg "OpenFrame\|Vec<OpenFrame>\|ParseStream" crates/runtime/src crates/codegen/src`. MIGRATION §19.4 same gate plus tape/direct union test. | HONOURED |
| Same-wave consumer wiring at Tranche F. | `restart/MASTER-PLAN.md:373-389` | Tranche F.W0-F.W5: Rust lowerer → tape/direct emit and builder integration → host calls/chains/layout → generated module template → generated LOC budget → nine-grammar regen. F.W1's gate: "Runtime parse returns `DocumentView`." F.W3 regen equality. F.W5 nine-grammar build. The consumers (parsing through tape/direct, returning DocumentView, regenerating nine grammars) land in the same tranche as the substrate. | HONOURED |
| ParseStream is permanently retired except as `syn` proc-macro internal. | `restart/ARCHITECTURE.md:22`; `restart/MASTER-PLAN.md:24-28, 47, 113, 257`; `restart/MIGRATION.md:140, 669-677`; `restart/audit/pass-3-runtime/PASS-3.md:32` | ARCHITECTURE §0 conflict ledger: "Do not rename tape to ParseStream." MASTER-PLAN executive summary: "ParseStream is not a replacement term." MASTER-PLAN B.W0 hard close: any `ParseStream` hit must be macro parser code using `syn`. MIGRATION §3.1.path: `Proc-macro `syn::ParseStream` use → KEEP-MODIFY → `path/src/macro_impl` "this is not runtime ParseStream". PASS-3 verdict: DISCARD. | HONOURED |

**Verdict.** The tape + direct-to-struct union is internally coherent across
all canonical documents. The substrate name is settled (tape, not
ParseStream). The location is settled (`runtime/src/tape/`). The borrow shape
is settled (`&'i Tape<'i>` + index). The materialisation discipline is
settled (one surface; no parallel substrate). The consumer wiring is settled
(Tranche F.W0-F.W5 lands the lowerer in the same wave-set as runtime parse,
DocumentView, regen equality, and nine-grammar regeneration). The OpenFrame
preservation language is deleted everywhere except as deletion-path
archaeology. Lock 1's reframe is the architectural keystone; it survives.

---

## §4 Lane 3 — Path Crate Triplet + Workspace Naming

The consolidated §3 conflicts 2-3 + punch items 17-19 say path crates are
`path` / `path-core` / `path-ts` (no `bbnf-` prefix); the public macro is
`pointer!` not `path!`; the `bbnf` aggregator's `src/` tree must obey 4-10
children.

| Claim | Source | Verification | Verdict |
|---|---|---|---|
| Path crates are `path`, `path-core`, `path-ts` (unprefixed). | `restart/README.md:50-52`; `restart/ARCHITECTURE.md:55-57, 597-619`; `restart/MASTER-PLAN.md:80, 152, 400-402` | README crate table: `path`, `path-core`, `path-ts`. ARCHITECTURE §1: `path` (Public), `path-core` (Internal/shared), `path-ts` (Public). ARCHITECTURE Cargo skeleton members list: `crates/path`, `crates/path-core`, `crates/path-ts`. MASTER-PLAN §2 Path layer: `path`, `path-core`, `path-ts`. MASTER-PLAN G.W0-G.W4 owns the triplet. | HONOURED |
| `test-fixtures` is unprefixed. | `restart/README.md:58`; `restart/ARCHITECTURE.md:63, 621`; `restart/MASTER-PLAN.md:82` | README: `test-fixtures` (Internal/dev). ARCHITECTURE: `test-fixtures` (Internal/dev). Cargo skeleton: `crates/test-fixtures`. | HONOURED |
| Public macro is `pointer!`, not `path!`. | `restart/README.md:50, 280`; `restart/ARCHITECTURE.md:271, 283, 312`; `restart/MASTER-PLAN.md:152, 400, 409` | README §2: "`path` — Rust `pointer!` + `select!` proc-macro shells". README §8 dual-macro table: "`pointer!(Json, ["a", "b", 0])` — sonic-rs idiom". ARCHITECTURE §3.4 shows `pointer!("/rules/0/name")` and `select!("rule[name='expr'] > alt:nth(0)")` as the public Rust macros. ARCHITECTURE §3.5 lists `pointer!`, `select!` as path's public exports. MASTER-PLAN G column: "`pointer!`, `select!`, visitor mutation". | HONOURED |
| `bbnf` aggregator's `src/` tree obeys 4-10 children. | `restart/ARCHITECTURE.md:347-354`; `restart/audit/pass-3-runtime/PASS-3.md:164-176` | ARCHITECTURE §4.1: `bbnf/src/` contains `lib.rs`, `grammar/`, `document/`, `value/`, `host/`, `workspace/`, `prelude.rs`. That is 7 immediate children — within Lock 13's 4-10. PASS-3 §6 carries `bbnf/src/` with `lib.rs`, `prelude.rs`, `grammar/`, `parse/`, `document/`, `value/`, `tape/`, `visitor/`, `diagnostics/`, `host/`. That is 10 children — at the upper bound. | TENSION — see below |
| Workspace count is 24. | `restart/README.md:60`; `restart/ARCHITECTURE.md:34, 597-622`; `restart/MASTER-PLAN.md:71, 76` | README: "Final count: ~24 workspace members." ARCHITECTURE Cargo skeleton lists 24 `crates/*` members plus `xtask` (24 production members). MASTER-PLAN §2: "the 24-crate set specified in `restart/ARCHITECTURE.md` §1-§5". | HONOURED |
| `bbnf-` prefix retained on user-facing crates only. | `restart/README.md:31-60`; `restart/ARCHITECTURE.md:34-40`; `restart/MASTER-PLAN.md:73, 76` | README: prefix yes on `bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`; no on internal substrate. ARCHITECTURE table mirrors. MASTER-PLAN treats user entrypoints as `bbnf*` quartet. | HONOURED |

**Tension under verification.** ARCHITECTURE §4.1 (line 347-354) and PASS-3
§6 (line 164-176) carry different `bbnf/src/` trees:

| Source | Children | Count |
|---|---|---:|
| `restart/ARCHITECTURE.md:347-354` | `lib.rs`, `grammar/`, `document/`, `value/`, `host/`, `workspace/`, `prelude.rs` | 7 |
| `restart/audit/pass-3-runtime/PASS-3.md:164-176` | `lib.rs`, `prelude.rs`, `grammar/`, `parse/`, `document/`, `value/`, `tape/`, `visitor/`, `diagnostics/`, `host/` | 10 |

Both fall inside Lock 13's 4-10 bound, so neither violates the discipline.
But the trees are not identical. ARCHITECTURE omits `parse/`, `tape/`,
`visitor/`, `diagnostics/`; PASS-3 omits `workspace/`. The two specs name
different concerns.

The hardening consolidated punch item #19 ("Restructure `crates/bbnf/src/` to
4-10 immediate children such as `lib.rs`, `prelude.rs`, `parse/`,
`document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/`" at HARDENING-
CONSOLIDATED.md:281) names a third candidate set. Three different proposed
trees exist:

| Source | Tree |
|---|---|
| ARCHITECTURE.md:347-354 | `lib.rs`, `grammar/`, `document/`, `value/`, `host/`, `workspace/`, `prelude.rs` |
| PASS-3 §6 | `lib.rs`, `prelude.rs`, `grammar/`, `parse/`, `document/`, `value/`, `tape/`, `visitor/`, `diagnostics/`, `host/` |
| HARDENING punch #19 | `lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/` |

Reviewer B notes: post-amendment, ARCHITECTURE is the executable authority
("MASTER-PLAN trio must be the executable authority after amendment; PASS
outputs remain evidence and source design records" — HARDENING-CONSOLIDATED
§5 closing). PASS-3's tree is evidence; the punch item recommends a third
tree. ARCHITECTURE has not yet absorbed the punch's recommendation.

This is a fault. The amendment has not yet adjudicated which of the three
trees is canonical. Lane 3 verdict cannot return COHERENT until ARCHITECTURE
ratifies one tree and PASS-3 commits to it.

The surgical fix: ARCHITECTURE §4.1 absorbs PASS-3's tree shape (or punch
#19's, whichever the user adjudicates), making `bbnf/src/` carry between 8
and 10 immediate children with explicit naming for `parse/`, `document/`,
`value/` (or `query/`), `visitor/`, `diagnostics/`, `metadata/` (or
`workspace/`), and the leaf files `lib.rs`, `prelude.rs`. PASS-3 §6 then
mirrors. The candidates are interchangeable on Lock 13 grounds; the
architectural fault is divergence, not the tree shape itself.

**Verdict.** Path triplet naming and macro naming and workspace naming and
workspace count are coherent. The `bbnf` aggregator's tree is **NOT**
coherent — three sources name three different child sets, all individually
within Lock 13 bounds but not mutually consistent. The amendment must pick
one. This blocks lane 3 from returning COHERENT.

---

## §5 Lane 4 — Layout Terminology Coherence

The consolidated §3 conflict #4 says PASS-1/2/3 use varying layout vocab;
MASTER-PLAN exposes `TypeFacts` vocabulary. Resolution: `layout lowering` /
`LayoutFacts` / `passes::layout`.

| Claim | Source | Verification | Verdict |
|---|---|---|---|
| Lock 2 binds the canonical name `layout lowering`. | `restart/locks/LOCKS.md:36` | "Layout lowering is the canonical IR pass name. The term replaces *type projection / type collapsing / type inference / type elaboration / TypeMap / StructLayout / TypeDesc / schema synthesis* everywhere. Old terms appear only in archived docs. The IR module is `bbnf-ir/src/passes/layout/`; the IR record is `Layout`; the trait that consumes it is `LayoutSink`." | HONOURED — though the lock text still says `bbnf-ir`, which is stale per Lock 7's de-prefixing. |
| README binds `passes/layout/` and retires `TypeDesc`/`StructLayout`/`TypeMap`. | `restart/README.md:384` | Lock 2 row in the §11 lock table: "Honoured at `passes/layout/`; `TypeDesc`/`StructLayout`/`TypeMap` aliases retire workspace-wide." | HONOURED |
| ARCHITECTURE side-table table names `LayoutFacts`. | `restart/ARCHITECTURE.md:944` | Side tables row: "`LayoutFacts` — Layout pass — `LayoutPush`, `LayoutPop`." Honoured. | HONOURED |
| ARCHITECTURE retains `TypeFacts` as a separate side-table. | `restart/ARCHITECTURE.md:937, 1008` | Side tables row: "`TypeFacts` — Type inference — Backend IR builder, host registry, diagnostics." Type rules row: "Inference is grammar-wide. Rule references and host calls unify through `TypeFacts`." | TENSION — see below |
| MASTER-PLAN C.W1 produces `TypeFacts`. | `restart/MASTER-PLAN.md:279` | "C.W1 — TypeFacts and HM/bidirectional core. — Host-free seed grammar typechecks." | TENSION — same as above |
| MASTER-PLAN owns layout-lowering as a separate concept. | `restart/MASTER-PLAN.md:684` | Carry ledger row: "Layout lowering — D/F — `@layout` remains parser metadata and does not lower through `LayoutFacts` and BIR. — LayoutFacts test plus BIR `LayoutPush`/`LayoutPop` replay." | HONOURED |
| MIGRATION uses `passes/src/types` for type facts and `passes/src/shapes` for shape facts. | `restart/MIGRATION.md:181, 182` | Type facts → `passes/src/types`, `ir/src/side_tables`. Shape facts → `passes/src/shapes`, `ir/src/side_tables`. | TENSION — see below |
| PASS-1 names a `passes/layout/` child. | `restart/audit/pass-1-substrate/PASS-1.md:100` | PASS-1 §3 per-crate `src/` tree row for `passes`: `lib.rs`, `normalize/`, `types/`, `layout/`, `facts/`, `recognizers/`, `extract/`, `validate/`. Layout is named. | HONOURED |
| PASS-1 retains `types/` as separate from `layout/`. | `restart/audit/pass-1-substrate/PASS-1.md:100` | Both `types/` and `layout/` exist as siblings. | TENSION — same as above |

**Tension under verification.** Lock 2's text is uncompromising: "The term
replaces *type projection / type collapsing / type inference / type
elaboration / TypeMap / StructLayout / TypeDesc / schema synthesis*
everywhere." Yet ARCHITECTURE keeps `TypeFacts` as a separate side-table
producer (type inference) distinct from `LayoutFacts` (layout pass). PASS-1
has both `types/` and `layout/` as sibling pass directories. MASTER-PLAN C.W1
specifically calls out `TypeFacts` as the consumer-facing artefact of the
HM/bidirectional core.

The reading that reconciles Lock 2 with ARCHITECTURE: Lock 2 retires the
*pass name* "type inference / type projection / type collapsing / type
elaboration / TypeMap / StructLayout / TypeDesc / schema synthesis". It does
not retire the typed-facts side-table. `TypeFacts` is the HM/bidirectional
output side-table; `LayoutFacts` is the layout-lowering output side-table.
Both can coexist as separate passes producing separate side-tables.

But the hardening consolidated §3 conflict #4 reads more strictly: "Use
`layout lowering`, `LayoutFacts`, and `passes::layout` as the public pass
surface; keep HM/CSP type checking as subroutine language only" (HARDENING-
CONSOLIDATED.md:145). That recommendation pushes harder: only `LayoutFacts`
should appear as the public pass surface; `TypeFacts` becomes subroutine
language.

The post-amendment shape if the consolidated punch item is taken literally:

- `passes::layout` is the public pass surface name.
- `LayoutFacts` is the public side-table name.
- `passes::types` (which PASS-1 names) becomes a subroutine within
  `passes::layout`, not a peer.
- `TypeFacts` (which ARCHITECTURE and MASTER-PLAN C.W1 name) becomes
  internal to layout lowering, not a public side-table.

That is a stricter reading than ARCHITECTURE / MASTER-PLAN currently
implement. ARCHITECTURE §7.3's side-table table has `TypeFacts` and
`LayoutFacts` as peer rows. MASTER-PLAN C.W1 names `TypeFacts` as the
consumer-facing artefact of an HM/bidirectional pass.

The amendment has not yet adjudicated whether `TypeFacts` is a public side-
table peer of `LayoutFacts` (ARCHITECTURE position) or an internal
subroutine of layout lowering (HARDENING-CONSOLIDATED position).

There is also a Lock 2 wording fault: "The IR module is `bbnf-ir/src/passes/
layout/`" still names `bbnf-ir` with a prefix that Lock 7 / README §2 retire
for internal crates. The Lock 2 text is stale on the prefix. Either Lock 2
gets a `bbnf-` prefix correction in lockstep with the amendment, or the
amendment carries a documentation note that Lock 2's pre-prefix-drop wording
maps to `ir/src/passes/layout/`.

The surgical fix:

1. ARCHITECTURE §7.3 retains `TypeFacts` and `LayoutFacts` as peer side-
   tables but states explicitly that `TypeFacts` is a *subroutine output* of
   layout lowering — it is consumed only by Backend IR builder, host
   registry, and diagnostics, while `LayoutFacts` is the *public lowering
   product*. The two coexist, both honour Lock 2 (which retires the names
   "type elaboration / type projection / TypeMap / StructLayout / TypeDesc /
   schema synthesis", not the existence of typed facts). PASS-1 §3's
   `types/` directory becomes a sub-module of `passes/layout/types/` to
   enforce subroutine status.

2. Lock 2's `bbnf-ir/src/passes/layout/` wording earns a one-line note: "the
   `bbnf-` prefix retires under Lock 7 / README §2; the canonical post-
   amendment path is `ir/src/passes/layout/`."

**Verdict.** Layout terminology is **PARTIALLY** coherent. `LayoutFacts` /
`layout lowering` / `passes::layout` are uniformly used. `TypeFacts` and
`passes::types` survive in ARCHITECTURE, MASTER-PLAN, and PASS-1, with no
explicit declaration of whether they are peers of `LayoutFacts` or
subroutines under it. The hardening's stricter reading (subroutine only) has
not been ratified into the synthesis trio. Lock 2's wording is stale on the
crate prefix.

---

## §6 Lane 5 — Two-IR Contract + Grammar IR / Backend IR Boundary

The README §4 names Grammar IR + Backend IR. PASS-1's IR Architect describes
the contract. PASS-2 was supposed to consume Backend IR, not Grammar IR. The
hardening's punch item #2 says: `rg -n "GrammarIR" crates/codegen/src/lower
crates/codegen/src/runtime_template` returns zero.

| Claim | Source | Verification | Verdict |
|---|---|---|---|
| README names two IRs and gives variant counts. | `restart/README.md:104-118` | "Two IRs. ... Grammar IR (~12-15 variants) ... Backend IR (~22 variants per BC.W0; refines in PASS-1)." Settled position Q9-Q11. | HONOURED |
| ARCHITECTURE §7.1 enumerates Grammar IR variants with field schemas. | `restart/ARCHITECTURE.md:773-829` | 15-variant table: `Rule`, `Seq`, `Alt`, `Repeat`, `Optional`, `Literal`, `Regex`, `Ref`, `Predicate`, `Lookbehind`, `Map`, `HostCall`, `LayoutDirective`, `ErrorDirective`, `Annotation`. Each row carries Variant / Purpose / Payload-shape / Lower-time-invariant / Main-BIR-consumer columns. Followed by Grammar IR invariants. | HONOURED |
| ARCHITECTURE §7.2 enumerates Backend IR variants with payload + lowering rules. | `restart/ARCHITECTURE.md:830-918` | 23-variant table covering Entry / Seq / DispatchAlt / SpeculativeAlt / RepeatLoop / OptionalBranch / ByteLiteral / RegexProgram / SimdScan / PrattSpine / CallRule / CallHost / HostChain / LayoutPush / LayoutPop / ErrorRecover / SpanMark / TapeEmit / DirectBuild / ValueProject / PathEval / DebugMark / Return. Each row carries Payload-shape / Rust-lowerer / VM-behavior / WASM-or-SIMD-note. | HONOURED |
| ARCHITECTURE §7.1 vs §7.2 boundary is named. | `restart/ARCHITECTURE.md:771-772, 920-928` | §7.1 introduction: "Grammar IR is semantic and close to the BBNF source. It keeps grammar-level meaning, typed annotations, host references, layout/error directives, and lookbehind." §7.2 invariants: "Lowerers never inspect Grammar IR." Boundary is explicit. | HONOURED |
| Codegen does NOT import Grammar IR types. | `restart/MIGRATION.md:657-660`; `restart/audit/pass-2-codegen/PASS-2.md:194-199`; `restart/MASTER-PLAN.md:355` | MIGRATION §19.3: `rg "GrammarIr\|GrammarIR\|grammar_ir" crates/codegen/src` and `rg "use .*grammar_ir\|crate::grammar_ir\|ir::grammar_ir" crates/codegen/src` both must return zero. PASS-2 import-deny floor: "Rust lowerer imports — `codegen::lower::*` imports `ir::backend_ir::*` and does not import Grammar IR modules." MASTER-PLAN E hard close: `cargo test -p codegen backend_lowerer_boundary`. Greppable; gated; tested. | HONOURED |
| Variant count band check. | `restart/README.md:111-112`; `restart/ARCHITECTURE.md:773-797, 833-859` | Grammar IR target ~12-15; ARCHITECTURE delivers 15. Backend IR target ~22; ARCHITECTURE delivers 23 (matching PASS-2's 23-variant final table). | HONOURED |
| Optimised IR is side-tables only, not a third IR. | `restart/README.md:113-114`; `restart/ARCHITECTURE.md:931-944` | README: "The 'optimised IR' of the prior plan is Grammar IR with extra metadata — not a third type." ARCHITECTURE §7.3: side tables are `TypeFacts`, `ShapeFacts`, `RecognizerFacts`, `EGraphFacts`, `CspSolution`, `CostFacts`, `RecoveryFacts`, `LayoutFacts`. No third IR. | HONOURED |
| BIR's variant count discrepancy reconciled (PASS-1 22 vs PASS-2 23). | `restart/ARCHITECTURE.md:860-862`; `restart/audit/pass-2-codegen/PASS-2.md:33-34` | ARCHITECTURE: "If an implementation keeps PASS-1's 22-variant table without `Return`, it must prove equivalent control-flow closure before codegen. The architecture default is PASS-2's final 23-variant table." PASS-2: "PASS-2 adds Lookbehind, folds multi-function chaining into HostCall, and keeps Unicode inside RegexDfa." The 22→23 split is named and adjudicated. | HONOURED |
| MASTER-PLAN tranche C/E ownership splits Grammar IR (C) and Backend IR (E). | `restart/MASTER-PLAN.md:96-98, 148-150, 327-356` | C row: "Grammar IR variants — `restart/ARCHITECTURE.md` §7.1 — C/D — Grammar-to-IR tests." E row: "Backend IR variants — `restart/ARCHITECTURE.md` §7.2 — E/F/H — BIR validation and VM replay." MASTER-PLAN E.W0-E.W4 owns BIR, VM, and lowerer trait/boundary. | HONOURED |
| Side-table side-products are IR contract not pass-output sprawl. | `restart/ARCHITECTURE.md:931-944`; `restart/audit/pass-1-substrate/PASS-1.md:122-128` | ARCHITECTURE §7.3 names the 8-row side-table list with producer/consumer per row. PASS-1 §4 hand-offs to PASS-2 enumerate Grammar IR / Backend IR / cost-model / e-graph rewrite plug-in / host metadata / tape-direct value as named contracts. | HONOURED |

**Verdict.** The two-IR contract is internally coherent post-amendment.
Grammar IR has 15 enumerated variants with field schemas. Backend IR has 23
enumerated variants with payload + lowering rules. The boundary is named in
ARCHITECTURE §7.1 / §7.2 prose and enforced by the import-deny gate at
MIGRATION §19.3. The variant count discrepancy between PASS-1 (22) and
PASS-2 (23) is adjudicated in ARCHITECTURE in favour of PASS-2. Side-tables
are not a third IR. MASTER-PLAN tranche ownership splits cleanly (C owns
Grammar IR; E owns Backend IR). The codegen boundary is greppable and
tested. The hardening's punch items #1, #2, #3, #4 land cleanly in this
post-amendment surface.

---

## §7 Lane 6 — Tranche Set Sequencing + 24-Crate Workspace

The README §2 names ~24 workspace crates. The MASTER-PLAN names tranches
A-J + further. Verify the workspace count post-amendment, the tranche set's
wave-by-wave sequencing against Era V failure mode, and the 14 locks' owning
tranche assignments.

| Claim | Source | Verification | Verdict |
|---|---|---|---|
| Workspace count post-amendment is 24. | `restart/README.md:60`; `restart/ARCHITECTURE.md:34, 597-622`; `restart/MASTER-PLAN.md:71, 76` | README: "Final count: ~24 workspace members." ARCHITECTURE §1 lists the 24 crates with role/visibility/inheritance. ARCHITECTURE §5 Cargo skeleton has 24 production members + `xtask`. MASTER-PLAN: "the final workspace is the 24-crate set specified in `restart/ARCHITECTURE.md` §1-§5". Per-crate count via grep: 24 confirmed. | HONOURED |
| Per-grammar declaration crates not in default workspace. | `restart/README.md:11-25`; `restart/locks/LOCKS.md:60`; `restart/ARCHITECTURE.md:1023-1027`; `restart/audit/pass-1-substrate/PASS-1.md:63-72` | README §1 anthem: "the optional declaration crate (Lock 14's escape valve) is not used for any of the 9 extant grammars." Lock 14: declaration crates are escape valves only. ARCHITECTURE §8.3: "Per-grammar declaration crates are not a default escape hatch." PASS-1 rare escape-valve fence: "Extant grammars — Exception table is empty for bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, and math." | HONOURED |
| Tranche set is A-J. | `restart/MASTER-PLAN.md:14, 144-156`; `restart/audit/hardening/HARDENING-CONSOLIDATED.md:7` | Inheritance index: "the new plan is A-J". MASTER-PLAN §5 tranche table: A through J with stub waves and primary close gates. Hardening lane 2 verdict: AMENDMENT-REQUIRED but tranche set carries through. | HONOURED |
| Tranche A archives `ser` and `gorgeous` per Lock 12. | `restart/locks/LOCKS.md:56`; `restart/MASTER-PLAN.md:85-86, 192-225`; `restart/MIGRATION.md:439-454` | Lock 12: archive before BA.W0 (now A.W0). MASTER-PLAN A.W0: "Branch/tag preflight, archive `ser` and `gorgeous`, remove them from workspace." MIGRATION §10 archive procedure with `git mv` commands. | HONOURED |
| Era V failure mode (substrate without same-wave consumer) is named. | `restart/locks/LOCKS.md:34, 73-83`; `restart/audit/hardening/HARDENING-CONSOLIDATED.md:58-61, 416-422` | Lock 1: "substrate-first/consumer-later (Era V failure mode)". HARDENING-CONSOLIDATED §2 lane 2: "B.W3 builds direct views before C.W2 produces ShapeFacts, while C.W2 says the direct builder consumes those facts." HARDENING punch #40: B/C sequencing repair. HARDENING punch #41: C/E/H consumer repair. | TENSION — see below |
| H/J SOTA gate sequencing post-amendment. | `restart/audit/hardening/HARDENING-CONSOLIDATED.md:62-63, 355-359` | HARDENING lane 2: "H/J relation survives after H gains numeric early thresholds and J keeps final close authority." Punch #31: "Replace H progress reports with numeric early thresholds and leave final thresholds to J." | TENSION — see below |
| Tranche A through J consumer gates exist for every wave. | `restart/MASTER-PLAN.md:209-214, 241-247, 276-283, 308-315, 342-348, 373-380, 406-412, 438-445, 471-478, 502-510` | Each tranche carries a "Wave / Scope / Consumer gate" table with a consumer gate per wave. | HONOURED — modulo B/C and C/E/H repair |
| Lock 14 grammar generalisation is auditable per tranche. | `restart/MASTER-PLAN.md:108-110, 626-642`; `restart/MIGRATION.md:691-702` | MASTER-PLAN §4 hard architectural gate: "No grammar-name dispatch — `rg` for parser type names, strategy tables, and grammar-name registries — A through J." Lock 14 ownership row: A/D/F/G/J. MIGRATION §19.6 future-grammar gate. | HONOURED |
| 14 locks' owning tranche assignments cohere with wave dependencies. | `restart/MASTER-PLAN.md:626-642` | Lock ownership table assigns each lock to one or more tranches. Lock 1 → B/F/H. Lock 2 → D/F. Lock 3 → B/H. Lock 4 → C. Lock 5 → E/F/H. Lock 6 → F/J. Lock 7 → G. Lock 8 → H/J. Lock 9 → B/G. Lock 10 → C/H. Lock 11 → A/J. Lock 12 → A/J. Lock 13 → A through J. Lock 14 → A/D/F/G/J. Each lock lands on a tranche whose deliverables include that lock's proof. | HONOURED |

**Tension under verification (sequencing).** The HARDENING-CONSOLIDATED §2
lane 2 verdict identifies two concrete sequencing faults that the synthesis
trio has not yet repaired:

1. **B/C sequencing.** B.W3 ("Direct builder shell and tape identity hooks
   — Direct view borrows spans from tape." per MASTER-PLAN.md:246) builds
   direct views before C.W2 ("ShapeFacts and value-shape mining — Direct-
   builder shell contract consumes ShapeFacts in a C fixture and records B
   integration gaps." per MASTER-PLAN.md:280) produces the shape facts that
   the direct builder is meant to consume. C.W2 is post-amended to consume
   the B-shell contract; this paper-thin reconciliation hides the issue —
   the substrate (direct builder shell) lands before its primary fact
   producer.

2. **C/E/H sequencing.** C.W3 ("RecognizerFacts and Pratt/SIMD candidate
   mining — Facts feed E-owned BIR snapshots, not placeholder hints." per
   MASTER-PLAN.md:281) and C.W5 ("CostFacts and extraction skeleton —
   Backend IR builder receives selected alternatives." per MASTER-PLAN.md:
   283) produce facts whose consumers (E's BIR builder, H's Pratt/SIMD
   recognizers) land in later tranches. The post-amendment text reads "Facts
   feed E-owned BIR snapshots", which honours Era V if E.W1 ("Grammar IR +
   side tables to BIR builder — Seed grammar produces BIR.") is the same-
   wave-set consumer; but C.W5 and E.W1 are different tranches. C produces
   facts; E consumes them later.

The hardening punches #40 and #41 name the surgery:

- Punch #40: "Move ShapeFacts before B.W3, split B.W3 into shell plus C-
  owned materialization, or change C.W2's consumer away from B direct
  builder."
- Punch #41: "Give C.W3/C.W5 same-wave BIR snapshot consumers or move
  recognizer/extraction proof into E/H where real BIR and Pratt/SIMD
  consumers exist."

Neither surgery is yet absorbed into MASTER-PLAN. The post-amendment shape
that the hardening declares ready (AMENDMENT-REQUIRED, not RE-DRAFT)
predicates Lock 1's Era V honour on these surgeries landing. Until they do,
the tranche set repeats the very failure mode it claims to retire.

**Tension under verification (locks 1 vs 11).** Lock 11 names sister-crate
incubation as path-deps "until each API stabilises". MASTER-PLAN §2 lists
`egraph`, `egraph-derive`, `csp-solver`, `parse-that`, `simd-scan` as
sister crates. Lock 11 ownership row in MASTER-PLAN's lock table: A/J. A
provides path-dep incubation; J judges publication readiness. This is
internally coherent.

**Verdict.** Workspace count, tranche letter set, lock ownership, archive
sequencing, future-grammar generalisation are all coherent. **Wave
sequencing is NOT yet coherent**: B/C and C/E/H Era V violations are named
in hardening punches #40 and #41 but have not yet landed in MASTER-PLAN.
Until they do, lane 2's AMENDMENT-REQUIRED verdict gates lane 6's coherence
finding.

---

## §8 Reviewer-B Verdict

**ARCHITECTURE REQUIRES STRUCTURAL AMENDMENT.**

The architectural commitments survive the proposed amendments in five of the
six lanes. The substrate identity (tape + direct-to-struct union) is settled
across README / locks / ARCHITECTURE / MASTER-PLAN / MIGRATION / PASS-3. The
two-IR contract (Grammar IR + Backend IR) is settled with full variant
schemas, payload tables, lowering rules, and a greppable / tested codegen
boundary. The path crate triplet (`path` / `path-core` / `path-ts`) is
unprefixed and consistent. The macro name (`pointer!`) is consistent. The
24-crate workspace count is consistent. Lock ownership is consistent.

Three faults remain.

**Fault 1 — `bbnf/src/` aggregator tree divergence (Lane 3).** ARCHITECTURE
§4.1 (line 347-354), PASS-3 §6 (line 164-176), and HARDENING-CONSOLIDATED
punch #19 (line 281) carry three different child sets for `bbnf/src/`. All
three fall within Lock 13's 4-10 bound, but they name different concerns
(`workspace/` vs `tape/` + `visitor/` + `diagnostics/` vs `query/` +
`metadata/`). The amendment must adjudicate one. Surgery: ARCHITECTURE §4.1
absorbs PASS-3's tree (or punch #19's, by user adjudication); PASS-3 §6
mirrors. The candidate trees are interchangeable on Lock 13 grounds; the
fault is divergence, not the choice itself.

**Fault 2 — Layout vocabulary partial coherence (Lane 4).** Lock 2 retires
`TypeMap` / `StructLayout` / `TypeDesc` / `schema synthesis` workspace-wide.
ARCHITECTURE §7.3 keeps `TypeFacts` and `LayoutFacts` as peer side-tables;
MASTER-PLAN C.W1 names `TypeFacts` as the consumer-facing artefact;
HARDENING-CONSOLIDATED §3 conflict #4 stricter resolution makes
`LayoutFacts` the only public surface and `TypeFacts` an internal
subroutine. ARCHITECTURE has not absorbed the stricter resolution. Lock 2's
text still names `bbnf-ir/src/passes/layout/` with a stale prefix that
Lock 7 / README §2 retire. Surgery: ARCHITECTURE §7.3 explicitly states
that `TypeFacts` is a *subroutine output* of layout lowering (consumed only
by Backend IR builder, host registry, diagnostics) while `LayoutFacts` is
the *public lowering product*. Lock 2 earns a one-line note that `bbnf-ir/`
maps to `ir/` post-Lock-7 prefix-drop.

**Fault 3 — B/C and C/E/H sequencing Era V violation (Lane 6).** B.W3
builds direct views before C.W2 produces shape facts; C.W3 / C.W5 produce
recognizer + extraction facts whose consumers (E.W1 BIR builder; H.W0/H.W1
Pratt/SIMD recognizers) land in later tranches. The hardening punches #40
and #41 name the surgery but the synthesis trio has not absorbed it.
Surgery: MASTER-PLAN moves ShapeFacts production to the same wave as B.W3
(or splits B.W3 into a shell wave plus a C-owned materialisation wave); for
C/E/H, give C.W3 / C.W5 same-wave BIR snapshot consumers (so the facts
exercise a consumer in the same wave-set) or relocate the recognizer/
extraction proof into E/H where the real consumers live. Without this
surgery, the tranche set repeats Era V's substrate-then-substrate-then-ship
failure mode that Lock 1's reframe explicitly retires.

The first two faults are minor naming / divergence repairs. The third is
load-bearing: a sequencing fault that, if left in place, makes the post-
amendment plan inherit the very failure mode the greenfield was constituted
to retire. The fault is not architectural in the sense of substrate / IR /
materialisation choice — those are all coherent. The fault is in the
tranche-graph topology.

The hardening's AMENDMENT-REQUIRED verdict and its punch #40 / #41 / #19
items are precisely the surgical corrections needed. Reviewer B confirms
the punch list is necessary and sufficient *for these three faults*.
Reviewer B does **NOT** find any architectural commitment that fails to
survive the amendment; Reviewer B finds three places where the amendment
itself has not yet landed in the synthesis trio's text.

The verdict is **ARCHITECTURE REQUIRES STRUCTURAL AMENDMENT** — three
named faults, three surgical fixes, no re-draft warranted. The architecture
itself (tape union, two IRs, path triplet, layout-lowering canon, 24-crate
workspace, A-J tranches, 14 locks) is internally coherent and survives.
The structural amendment is the absorption of HARDENING-CONSOLIDATED's
punches #19, #40, #41 into ARCHITECTURE / PASS-3 / MASTER-PLAN, plus the
clarification that `TypeFacts` is subroutine-internal to layout lowering.

Hereupon the punches must land before tranche full-spec drafting; without
the absorption, the gestalt's own discipline (Lock 1 reframe, Lock 2
canon, Lock 13 child-count) is contradicted by the very documents that
claim to honour it.
