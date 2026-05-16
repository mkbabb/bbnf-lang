# Deferral Audit 7 — Locks and Architecture

## §1 Scope and Corpus

Audit subject: the 14 governing locks (`restart/locks/LOCKS.md`) and the
authoritative `restart/ARCHITECTURE.md`. Greenfield mandate: every architectural
commitment that V2 would have to rewrite is a candidate for absorption now,
because the cost of moving lock text post-V1 is paid in cited-line drift across
every PASS, MASTER-PLAN, MIGRATION, and audit document.

Corpus references consulted:

| File | Use |
|---|---|
| `restart/locks/LOCKS.md` | Authoritative lock text (249 lines, 14 numbered commitments). |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` | §3 binding ledger, §4 cross-target conflicts, §5 residue ledger, §8 topic ownership, §9 verification commands. |
| `restart/ARCHITECTURE.md` | §0 authority/conflict ledger, §6 pipeline, §7.3 side tables, §8.2 type system, §9 runtime, §10 lowerers, §11 perf, §12 yaml onboarding. |
| `restart/research/fold-synthesis.md` | Latest absorbed positions for Topics 1-8. |

Scope restriction: this audit touches only
`restart/research/deferral-audit-7-locks-architecture.md`. Lock text and
ARCHITECTURE.md are not modified here; this audit produces only proposals.

Sibling audits in this tranche cover non-overlapping surfaces (1 type system,
2 function/value, 3 BBNF surface, 4 sibling crates, 5 runtime, 6 codegen).
Audit 7 covers cross-cutting lock cohesion plus ARCHITECTURE.md global
deferrals — the surfaces that bind the other six.

## §2 Per-Lock Audit

Each row records: current settled V1 commitment; deferral language present
(direct quotes, path:line); greenfield-fold value (HIGH = lock text shifts
post-V1 unless folded; MEDIUM = lock text holds but downstream specs migrate;
LOW = wording polish only; NONE = no deferral worth absorbing).

### §2.1 Lock 1 — Tape Substrate, Properly Unioned

Settled: tape lives at `runtime/src/tape/`; typed-value records borrow into it;
no parallel substrate; columnar SoA stays buried
(`restart/locks/LOCKS.md:34`).

Deferral language: a 2026-05-04 reframe ("the prior restart's wholesale
retirement of the tape name was an over-correction... Lock 1's spirit holds;
the no-rename clause is amended"). The amendment is *retrospective*, not
forward-deferred. Lock 1 carries no post-V1 hook.

Greenfield-fold value: **NONE**. The lock is settled and inheritance-free.

### §2.2 Lock 2 — Layout Lowering Canonical IR Pass

Settled: `passes::layout` is the canonical name; HM/CSP run as subroutines;
`LayoutFacts` is public, `TypeFacts` is internal
(`restart/locks/LOCKS.md:36`; reinforced by V6 §4 conflict #2 and
ARCHITECTURE §7.3 `restart/ARCHITECTURE.md:990-1013`).

Deferral language: none in the lock itself. The fold-synthesis records Topic 1
absorption that strengthens the in-V1 contract rather than deferring anything.

Greenfield-fold value: **NONE** at lock level. Lock 2 is the canonical
counter-example to "we'll rename it later" failure.

### §2.3 Lock 3 — Cursor + Byte-Skip Unified

Settled: one parse implementation, `__EAGER_EMPTY_PATH` elides cursor calls
on the empty path (`restart/locks/LOCKS.md:38`).

Deferral language: none.

Greenfield-fold value: **NONE**. Lock 3 admits no future amendment.

### §2.4 Lock 4 — Per-Domain Orthogonal Optimization

Settled: CSP, e-graph, miners, shape, cost compose by output piping; no
unified hypergraph; egglog fusion is a known SOTA pressure
(`restart/locks/LOCKS.md:40`).

Deferral language: "Egglog-style Datalog/equality-saturation fusion is a known
SOTA pressure, not an omitted option; **V1 rejects that fusion** because
diagnostics, public proof records, monotone bridge boundaries, and independent
stabilization gates must stay owned by the domain that produced them. **Fusion
remains a post-V1 research comparison, not the governing architecture**"
(`restart/locks/LOCKS.md:40`, emphasis added).

Greenfield-fold value: **LOW**. The deferral is a *negative* commitment — V1
does not adopt egglog fusion. Folding it into V1 would invert the lock, not
absorb it. The text already names the rejection rationale (per V6 R4 closure,
`restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:177`); no further fold is
useful. Tracked as research-comparison residue, not architectural debt.

### §2.5 Lock 5 — IR + Per-Backend Lower

Settled: backend-agnostic typed BIR; per-backend lowerers; "TS+WASM at BD+"
(`restart/locks/LOCKS.md:42`).

Deferral language: "per-backend lowerers (Rust now, **TS+WASM at BD+**)"
(`restart/locks/LOCKS.md:42`).

Greenfield-fold value: **HIGH**. The "TS+WASM at BD+" clause cites legacy
tranche letters (BA-BD), which `restart/ARCHITECTURE.md:14-16` declares
inheritance, not governing truth. ARCHITECTURE §11 already names WASM exact
gates with measured SOTA `{N}`/`{M}` placeholders
(`restart/ARCHITECTURE.md:1320`); H.W3 is the WASM gate
(`restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:178` R6) and J.W3 is
publication. Lock 5 should rename `BD+` to a tranche-letter-stable phrase
("TS and WASM lowerers at H.W3/J.W3 receivers"), removing the post-V1
reference entirely. This is wording hygiene with no semantic change but
prevents the lock from re-citing retired tranche names.

### §2.6 Lock 6 — xtask Emits Committed Source

Settled: no proc-macro façade; generated source is greppable
(`restart/locks/LOCKS.md:44`).

Deferral language: none.

Greenfield-fold value: **NONE**.

### §2.7 Lock 7 — `crates/path/` Consolidated

Settled: runtime cursor merges into `crates/path/`; `pointer!` macro lives
there; TS variant at `crates/path-ts/`; optional `crates/path-core/` shared
non-proc-macro crate (`restart/locks/LOCKS.md:46`).

Deferral language: "A `crates/path-core/` (non-proc-macro) crate **may exist**
to share the path-AST + compile logic between the two proc-macro shells; if
so, it is the only deduplication mechanism allowed"
(`restart/locks/LOCKS.md:46`, emphasis added).

Greenfield-fold value: **MEDIUM**. The conditional ("may exist") is a deferred
decision: V1 either has `path-core` or does not. ARCHITECTURE §1 lists
`path-core` as a required internal crate
(`restart/ARCHITECTURE.md:62`). The architecture has *already decided*; the
lock has not been updated to reflect it. Greenfield fold: change "may exist"
to "exists" in Lock 7. Risk: low (path-core is a settled crate name in the
rest of the corpus).

### §2.8 Lock 8 — Surpass sonic-rs / simdjson / lightning-css

Settled: every perf gate names a competitor + dataset + platform; AU is never
mentioned (`restart/locks/LOCKS.md:48`).

Deferral language: none in the lock. Trapped in V6 R6 is a placeholder
`{N}`/`{M}` for the WASM lightning-css comparison
(`restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:178`); ARCHITECTURE
§11 row `simd/structural_scan` has no WASM target row.

Greenfield-fold value: **MEDIUM**. The lock itself is settled. The deferred
WASM benchmark anchor is an implementation gate (H.W3), not a lock-level
defect. Recommend: add to Lock 8 the explicit WASM competitor anchor
("lightning-css WASM comparison; H.W3 owner; placeholder until measured")
so future readers see Lock 8 covers the WASM target as well as the native
ones. Optional; not a fault.

### §2.9 Lock 9 — Slice-Borrow + bumpalo + owned

Settled: `&'i str` default; `parse_in(input, &bump)` opt-in; `parse_owned`
escape; one parse implementation (`restart/locks/LOCKS.md:50`).

Deferral language: none.

Greenfield-fold value: **NONE**. Lock 9 is fully scoped to V1.

### §2.10 Lock 10 — Pratt + SIMD Auto-Detected

Settled: no `@pratt`/`@simd` directives; optimizer mines grammar shape
(`restart/locks/LOCKS.md:52`).

Deferral language: none in the lock. Topic 8 (`fold-synthesis.md` §Topic 8,
T8-A4) absorbed verifier-bound prefilter and `regex-automata` oracle posture
*into* the architecture, leaving the lock unchanged.

Greenfield-fold value: **NONE** at the lock level. Architecture-side: SIMD
exact/prefilter mode contract is settled in
`restart/ARCHITECTURE.md:1276` and `BBNF-OPT002`
(`restart/ARCHITECTURE.md:1041`).

### §2.11 Lock 11 — Path-Deps for Incubating Sister Crates

Settled: egraph, egraph-derive, csp-solver, bbnf-regex, parse-that path-dep
until stable; **promote to registry once stable**
(`restart/locks/LOCKS.md:54`).

Deferral language: "**promote to registry once stable**"
(`restart/locks/LOCKS.md:54`). A staged-publication post-V1 hook.

Greenfield-fold value: **HIGH**. Promotion-to-registry is a V2/J.W3-class
event (V6 references publication split: synthesis amendment 2026-04-12 noted
"Lock 11 publication split"). The lock prescribes the *transition* but not
the gate that triggers it. Greenfield fold: bind Lock 11 to the J.W3
publication gate explicitly — "promote to registry at J.W3 publication-gate
acceptance, with stability criterion measured by zero breaking-change commits
across two consecutive tranches".

Risk of amendment: **MEDIUM**. Lock 11 is cited by sibling crate audits
(`restart/research/deferral-audit-4-sibling-crates.md`) and the workspace
shape table (`restart/ARCHITECTURE.md:325-329`). The fold sharpens the gate
without renaming the lock; cited-line stability is preserved if the sentence
about promotion is appended rather than restructured.

### §2.12 Lock 12 — ser + gorgeous Archive BEFORE BA.W0

Settled: ser/gorgeous archive is the precondition for BA.W0 to begin
(`restart/locks/LOCKS.md:56`).

Deferral language: references "BA.W0" — a legacy tranche slot.
ARCHITECTURE.md:14-16 declares BA-BD inheritance, not governing.

Greenfield-fold value: **HIGH**. The lock cites a tranche letter that is no
longer the governing slot. The current MASTER-PLAN tranche shape uses
A-J letters (V6 §3 binding ledger references C/D/H/I/J as authoritative).
The archive precondition is real (the work must precede tranche A.W0); the
text should read "before tranche A.W0" not "before BA.W0".

Risk: **LOW**. Single sentence wording change; no semantic shift.

### §2.13 Lock 13 — No God Directories; Cohesive Encapsulation

Settled: 4-10 children per directory; no handwritten file >500 LOC outside
generated; sibling APIs uniform (`restart/locks/LOCKS.md:58`).

Deferral language: none. The lock is exhaustive about exception handling
("Files >500 LOC outside `generated/` are forbidden; directories with >10
immediate children mixing concerns are forbidden").

Greenfield-fold value: **NONE** at the lock level. ARCHITECTURE §13 records
the exception ledger in line with the lock
(`restart/ARCHITECTURE.md:1438-1445`).

### §2.14 Lock 14 — Full Grammar Generalisation; Zero Overfitting

Settled: substrate carries zero grammar-specific code; per-grammar
declaration crates are rare escape valves with the §5.6 review form
(`restart/locks/LOCKS.md:60`; ARCHITECTURE §5.6 form
`restart/ARCHITECTURE.md:739-770`).

Deferral language: "Per-grammar deviations... are encoded in the grammar
metadata + source, NOT in branching code in any other crate" — this is
an in-V1 prohibition, not a deferral. The yaml onboarding probe
(`restart/ARCHITECTURE.md:1331-1376`) carries "parity fixtures are a later
J gate" for the *fixture* surface, but the *lock* is silent on yaml.

Greenfield-fold value: **NONE** at the lock level. The yaml proof rides
A.W4 / J close gates, not Lock 14 amendments.

## §3 ARCHITECTURE Deferral Ledger

Direct grep for futurity language across `restart/ARCHITECTURE.md`. Each
hit classified by greenfield-fold value.

| # | Source | Current language | Classification | Fold value |
|---:|---|---|---|---|
| A1 | `:35` | "fusion remains a **post-V1 research comparison** rather than the governing substrate." | Echoes Lock 4 §2.4. Negative future commitment, not deferred work. | NONE |
| A2 | `:669` | "crate ownership and dependency graph do not change without an explicit **architecture amendment**." | Stability clause; standard. | NONE |
| A3 | `:802-804` | Type/layout CSP runs inside `passes::layout`; "the **later** global CSP solve consumes public solved or narrowed facts for extraction-time legality." | Pipeline-internal "later"; means downstream-pass, not post-V1. Not a deferral. | NONE |
| A4 | `:1134` | "Standalone `@recover` directive... PASS-3 amendment; no production for `Recover ::= ...` outside `@error` body." | "PASS-3 amendment" cites the absorbed PASS amendment, not future amendment. | NONE |
| A5 | `:1162-1166` | "Higher-rank, existential, indexed, or GADT-like grammar types **are out of V1 unless a later architecture amendment** opens a Dunfield-Krishnaswami or OutsideIn-style proof gate." | Explicit deferred type-system extension. | LOW (§4 below) |
| A6 | `:1168-1171` | "Open row polymorphism and unbounded structural record subtyping **are routed to a later type-system research gate**." | Same shape: explicit deferred extension. | LOW (§4 below) |
| A7 | `:1331-1376` | "future grammar test" / yaml "parity fixtures are a **later J gate**." | Names the receiver tranche (J). Already routed. | NONE |
| A8 | `:1376` | yaml "Provisional yaml budget row reports `generated_loc <= 4,000`, regen wall, and bench-manifest metadata; parity fixtures are a **later J gate**." | Receiver-named (J). Architectural truth. | NONE |
| A9 | `:1414` | "Any **future entry** must populate the eight-field §5.6 review form." | Rare escape valve; review form is settled. | NONE |
| A10 | `:1453` | Style: "no marketing prose, no filler, no **future-only placeholders**." | Negative quality bar. | NONE |
| A11 | `:1320` (perf §11) | `simd/structural_scan` row uses `>= 5 GB/s` / `>= 7 GB/s` competitor anchors but no WASM row. | WASM benchmark deferred to H.W3 measurement. | MEDIUM |

ARCHITECTURE has two genuine post-V1 deferrals (A5, A6) plus one
implementation-gate measurement deferral (A11). The remainder are
in-pipeline ordering ("later in the same parse"), absorbed amendments
(referenced as "PASS-3 amendment"), or stability clauses.

## §4 Cross-Lock Cohesion Findings

### F1 — Legacy tranche letter drift across locks

Locks 5 and 12 cite legacy tranche letters (BD+, BA.W0). ARCHITECTURE
§0 declares BA-BD inheritance; the governing tranche shape is A-J. Three
locks plus one yaml row mention legacy slot references; the lock file
itself uses BA letters in §0 (line 13: "the bbnf-lang BA-restart"). The
locks were drafted for BA-BD planning and carry that diction.

Surgery: rename "BA-restart" prose-context to "tranche-A restart" or
"greenfield restart"; rename "BD+" in Lock 5 to the H.W3/J.W3 receivers;
rename "BA.W0" in Lock 12 to "A.W0".

Risk: **LOW** (sentence-level wording). Citations are line-stable.

### F2 — Lock 11 publication gate underspecified

Lock 11 says "promote to registry once stable" but does not bind the
stability gate. V6 §3 §5 R7 names the publication split as a deferred
implementation row. Sibling-crate publication policy is a Lock-11 child,
not an orthogonal concern.

Surgery: append to Lock 11 the J.W3 publication gate and the
two-consecutive-tranche stability criterion.

Risk: **MEDIUM** (Lock 11 is cited from MASTER-PLAN tranche-J close
proof). Append-only edit preserves citation lines.

### F3 — Type-system post-V1 hooks undeferred-elsewhere

ARCHITECTURE A5 and A6 explicitly route higher-rank/GADT and
row-polymorphism to "a later type-system research gate" without naming
the gate. V6 Topic 1/2/3 fold-synthesis closes them as **future proof
gates** but neither MASTER-PLAN nor any tranche letter owns the gate.
The diagnostic code `BBNF-LOCAL-EQUALITY-ANNOTATION`
(`restart/ARCHITECTURE.md:1052`) names "Future indexed/GADT-like
extension gate" — the gate is named in diagnostic code form but not
bound to a tranche.

Surgery: ARCHITECTURE §8.2 names the post-V1 receiver explicitly as
"post-J amendment, gated by a separate research dispatch outside the V1
tranche set". This is *not* a fold candidate (the user has confirmed V1
HM is rank-1 only); it is a routing-clarity edit.

Risk: **LOW**. The architecture already declines to absorb GADT-like
types; the surgery is naming the deferral receiver.

### F4 — yaml onboarding J-gate parity

ARCHITECTURE §12 binds yaml onboarding to grammar source + metadata
only; parity fixtures land at a "later J gate"
(`restart/ARCHITECTURE.md:1376`). MASTER-PLAN J.W3 is the publication
gate. The yaml parity fixtures and the publication gate are *both*
J-receivers. They should be distinct rows in MASTER-PLAN §24 close
proof. (Out of scope for this audit; flagged for synthesis.)

### F5 — Lock 14 `path-core` redundancy with Lock 7

Lock 7 declares `path-core` exists conditionally ("may exist"). Lock 14
implicitly assumes `path-core` is a generic crate
(`restart/locks/LOCKS.md:60`: "Generic crates — `bbnf-parse`,
`bbnf-codegen`, `bbnf-runtime`, `bbnf-ir`, `path`, `path-core`,
`egraph`, ..."). Lock 14 hardcodes `path-core` in its zero-grammar-code
list; Lock 7 says the crate may not exist. Cohesion failure.

Surgery: align Lock 7 with Lock 14 — `path-core` exists; remove the
"may exist" conditional. Same as §2.7 fold proposal.

Risk: **LOW**.

## §5 Lock-Modification Proposals

Each row names the lock, the surgery (verbatim before/after), the citation
risk, and whether the change is greenfield-fold (V1 absorbs) or hygiene
(text drift only).

| Lock | Surgery | Citation risk | Class |
|---|---|---|---|
| Lock 5 | Replace "TS+WASM at BD+" with "TS and WASM lowerers at H.W3 (recognizer/scanner) and J.W3 (publication) receivers". | LOW (single phrase). | Greenfield-fold (eliminates legacy tranche letter from a structural lock). |
| Lock 7 | Replace "A `crates/path-core/` (non-proc-macro) crate **may exist**" with "A `crates/path-core/` (non-proc-macro) crate **exists**". | LOW (mid-paragraph clause). | Greenfield-fold (settles a conditional V2 decision). |
| Lock 8 | Append a WASM competitor anchor row: "lightning-css WASM comparison; H.W3 owner; measured placeholder until benchmark lands." | LOW (additive). | Hygiene (covers an existing routed measurement). |
| Lock 11 | Append: "Promotion to registry triggers at the J.W3 publication gate after two consecutive tranche closes show no breaking API change." | MEDIUM (Lock 11 cited from MASTER-PLAN tranche-J close proof; append-only preserves citations). | Greenfield-fold (binds the deferred publication transition). |
| Lock 12 | Replace "BA.W0" with "A.W0". | LOW (single token). | Hygiene (legacy tranche letter). |
| Locks file §1 prose | Replace "bbnf-lang BA-restart" with "bbnf-lang greenfield restart" or "tranche-A restart". | LOW (prose only; no numbered lock affected). | Hygiene. |
| Lock 14 §1 prose (fourteen vs. twelve) | Section §168 ("twelve locks above are settled") drifts from header §30 ("fourteen locks"). Update §168 to "fourteen". | LOW. | Hygiene (V5/V6 R1 named lock-number drift; uncorrected here). |

No lock requires *renaming* (which would propagate across PASS, MASTER-PLAN,
MIGRATION, audit). All proposals are sentence-level edits within an existing
numbered lock.

## §6 Recommended V1 Folds (Sorted by Greenfield Value)

Sorted descending by greenfield value: HIGH first, then MEDIUM, then LOW.

| # | Item | Source | Surgery | Greenfield value | Risk |
|---:|---|---|---|---|---|
| 1 | Lock 5 BD+ → H.W3/J.W3 | `restart/locks/LOCKS.md:42` | Rename inline phrase. | HIGH (eliminates legacy tranche letter from structural lock). | LOW |
| 2 | Lock 11 promotion gate binding | `restart/locks/LOCKS.md:54` | Append J.W3 publication clause. | HIGH (settles V2 transition). | MEDIUM |
| 3 | Lock 12 BA.W0 → A.W0 | `restart/locks/LOCKS.md:56` | Token replace. | HIGH (legacy slot reference in a lock that gates BA tranche start). | LOW |
| 4 | Lock 7 `path-core` conditional → declarative | `restart/locks/LOCKS.md:46` | Replace "may exist" with "exists". | MEDIUM (cohesion with Lock 14). | LOW |
| 5 | Lock 8 WASM competitor anchor | `restart/locks/LOCKS.md:48` | Append WASM row. | MEDIUM (covers routed H.W3 measurement). | LOW |
| 6 | ARCH §8.2 post-V1 type-system receiver naming | `restart/ARCHITECTURE.md:1162-1171` | Name the post-J research-dispatch receiver explicitly. | LOW (routing clarity). | LOW |
| 7 | Locks §168 fourteen-vs-twelve drift | `restart/locks/LOCKS.md:201` | Token replace. | LOW (V5/V6 R1 lingering hygiene). | LOW |
| 8 | Locks §1 prose "BA-restart" → greenfield | `restart/locks/LOCKS.md:3` | Single phrase. | LOW. | LOW |

Cumulative effect of items 1-5: **two HIGH-value lock-text amendments and
three MEDIUM amendments fold post-V1 transitions into V1 itself**. None
require renaming a lock (which is the only high-friction lock surgery).
None alter the semantics of any lock; all sharpen the receiver, gate, or
slot reference.

V1 cost of executing folds 1-5: roughly four sentence-level edits. V2 cost
of *not* executing them: every lock-cited line in MASTER-PLAN, MIGRATION,
PASS-1/2/3, and the V6 hardening trail must be rewalked when the legacy
tranche letters retire and the publication gate finally lands.

## §7 Open Questions for Synthesis

| Q | Surface | Question |
|---|---|---|
| Q1 | Lock 5 + tranche shape | Synthesis confirms current MASTER-PLAN tranche letters are A-J (not BA-BJ). If a tranche letter scheme other than A-J governs publication, fold #1 above must use those letters instead. |
| Q2 | Lock 11 promotion vs. tranche cadence | Synthesis confirms two-consecutive-tranche stability is the right gate; V6 R7 mentions "stable" without bounding cadence. Alternative: register-on-J.W3-acceptance, single-gate. |
| Q3 | Lock 7 `path-core` conditional | Sibling audit 4 (sister crates) may also recommend on `path-core`. Synthesis should reconcile and produce one canonical form. |
| Q4 | Lock 8 WASM anchor | The H.W3 placeholder `{N}/{M}` (`HARDENING-CONSOLIDATED-V6.md:178` R6) is an unmeasured row. Folding the WASM anchor into Lock 8 *before* H.W3 measures is asserting a number that does not exist. Recommend: append a *measurement-pending* row to Lock 8 with H.W3 owner, no number. |
| Q5 | ARCH §8.2 GADT receiver | Synthesis decides whether the post-V1 type-system gate gets a tranche letter (post-J), a research-dispatch slot, or stays unbound. |
| Q6 | Lock 4 egglog rationale | V6 R4 closed the rationale fold. No further surgery. Confirmed silent. |
| Q7 | yaml J.W3 vs. publication J.W3 | Two distinct J.W3 obligations exist (yaml parity fixture, sister-crate publication). MASTER-PLAN §24 carry-ledger should distinguish them. (Out of audit-7 scope; flagged for synthesis.) |

## §8 Summary

The 14 locks are mostly settled. Two locks (5 and 12) cite legacy tranche
letters that the architecture has retired; one lock (7) carries a
conditional that another lock (14) treats as declarative; one lock (11)
defers a publication transition that V1 can bind. ARCHITECTURE.md carries
two genuine post-V1 deferrals (higher-rank type-system and row polymorphism)
that are correctly out of V1, and one measurement deferral (WASM SOTA anchor)
correctly routed to H.W3.

Recommendation: execute folds 1-5 from §6 in a single sentence-level lock
amendment pass before V1 implementation begins. The amendments preserve
every cited line, settle four post-V1 transitions, and remove three legacy
tranche-letter references from structural locks. Item 6 (architecture
post-J receiver naming) is independent and can land in the next ARCHITECTURE
amendment pass.

No lock requires re-draft. No new Lock 15 is recommended.
