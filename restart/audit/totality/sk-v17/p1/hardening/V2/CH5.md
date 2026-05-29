---
lens: CH5 HIDDEN-COUPLING
pass: T-P1-excavation
cycle: V2
subject: SK-V17 T-P1 excavation artefacts (restart/audit/totality/sk-v17/p1/*.md)
generated_at: 2026-05-29T22:40:00Z
firewall: Lock 1 substrate-union + Lock 10 5-shape BackendShape + Track-1≡Track-2 honesty
artefacts_present: [1f-anti-pattern.md, 1f-coherence-scan.md, 1f-past-corpora.md]
artefacts_absent: [1a-substrate-evidence.md, 1b-codegen-evidence.md, 1c-runtime-evidence.md, 1d-skinny-lessons.md, 1e-locks-evidence.md, 1f-coherence-scan(canonical 1F)]
live_truth_method: "Read over crates/core/src/runtime/tape/{mod,record}.rs, crates/core/src/runtime/css_l4/builder.rs, crates/ir/src/registry/struct.rs:84-114,202; grep over crates/core/src/{grammar/generated/json.rs,backend/rust/emitter/shapes/substrate.rs}, crates/simd-scan/src/alphabet.rs, skinny/crates/runtime/src/tape/mod.rs, skinny/crates/bbnf-simd/src/dispatch.rs; sed over restart/{ARCHITECTURE.md:1088,1206, locks/LOCKS.md:75-158, skinny/tranches/sk-v17/SPEC.md:805-860}; no cargo/build mutation"
counts:
  accept: 6
  revise: 2
  reject: 1
---

## Lens Charge

CH5 HIDDEN-COUPLING (V2) is the Lock-1 substrate-union firewall. It scans the
SK-V17 T-P1 excavation for: (a) any catalogued state implying a **parallel
substrate**, a **sidecar producer**, or a **renamed-scanner** violation; (b)
whether the **5-shape BackendShape canon** (Lock 10) is excavated whole and not
mis-stated as a 6th-shape pressure; (c) **Track-1 ≡ Track-2 dishonesty** — a
producer masquerading as an independent oracle; (d) whether the
**tape-as-unified-substrate** question is framed without implying a parallel or
second substrate beside the live one. Per ORCHESTRATOR §3W the firewall is
adversarial against recalled couplings; per §8.1 the structural projection IS the
tape — a catalogued "sidecar producer" is a divergence, not a feature.

## Scope Note — Artefact Census (load-bearing for the verdict)

The SK-V17 `p1/` directory holds **only** the three 1F outputs
(`1f-anti-pattern.md`, `1f-coherence-scan.md`, `1f-past-corpora.md`). The
PASS-1-EXCAVATION §2 scope matrix names 1A (substrate-layer evidence) as the
dedicated owner of the Lock-1 union spine and 1B as the BackendShape (Lock 10)
owner; **neither 1A nor 1B exists in this tree** (the 1A/1B/1C/1D/1E inventories
are absent; the canonical `1F-coherence-scan` prose is folded into `1f-*` lower-cased
filenames here). Per PASS-1 §8.1 ("the Lock 1 substrate-union audit is 1A's spine
and CH5's firewall"), CH5's primary subject is structurally **missing**. The
substrate-union and BackendShape facts CH5 must firewall are therefore carried
entirely by 1F's anti-pattern scan + coherence scan, which is **out-of-charter
load** for 1F (1F is the cross-corpus/anti-pattern scanner, not the substrate
inventory). This is itself a hidden-coupling exposure: the firewall has no 1A
spine to firewall against. Catalogued below as **CH5-S0 (REJECT, orphaning a
structural gap)** — not a defect in the 1F prose, but a pass-completeness
violation that blocks the §4 convergence criterion (a CHALLENGE wave cannot
return ≥95% ACCEPT when its primary subject inventory is absent).

The dispositions below grade the substrate/BackendShape content **as carried by
the present 1F artefacts**; every claim was resolved to live file:line.

## Section Dispositions

### CH5-S0 — Missing 1A/1B inventories (the firewall has no spine)
**Subject:** PASS-1 §2 scope matrix rows 1A + 1B; §8.1.
**Disposition: REJECT.**
**Evidence:** `restart/audit/totality/sk-v17/p1/` contains only `1f-{anti-pattern,coherence-scan,past-corpora}.md`; no `1a-substrate-evidence.md`, no `1b-codegen-evidence.md`. PASS-1 §2 makes 1A the substrate-union owner and §8.1 names it "CH5's firewall" counterpart.
**Why REJECT not REVISE:** This is not a prose defect to be re-evidenced; the load-bearing inventory the lens exists to firewall is structurally absent. The substrate-union (Lock 1) spine and the whole-5-shape BackendShape inventory (Lock 10, the 8-step `derive_backend_shape`, the per-grammar matrix) that §8.2 requires "inventoried whole by 1B" have no home. 1F carrying them is scope-bleed (CH2/Lock-14-adjacent: the cross-corpus scanner is doing the substrate inventory's job).
**Concrete fix:** Before V3 CHALLENGE, dispatch 1A and 1B per §2. 1A owns: ARCH §1/§7.1/§9 + Lock 1 substrate-union ↔ `crates/core/src/runtime/tape/{mod,record,arena,cursor}.rs` + skinny `Tape`/`ValueRef`; the `&'i Tape<'i>` borrow shape; the 20-variant BIR alphabet. 1B owns: the whole 5-shape canon + `derive_backend_shape` ↔ `crates/core/src/backend/rust/emitter/shapes/`. The substrate facts now in `1f-anti-pattern.md`/`1f-coherence-scan.md` MIGRATE to 1A; 1F retains only the cross-document/anti-pattern/past-corpora scan. The §4 convergence count cannot be computed until the 1A/1B `divergence_count` frontmatter exists.

### CH5-S1 — Parallel-substrate firewall row (1f-anti-pattern AP17-001)
**Subject:** `1f-anti-pattern.md:55` + Executive Summary `:31-49`; CH5 Firewall Verdict `:61-72`.
**Disposition: ACCEPT.**
**Evidence:** The claim "crates/core holds ONE tape construct (`TapeStructBuilder`, `crates/core/src/runtime/tape/mod.rs:58`), it is UNWIRED" resolves: `TapeStructBuilder` declared at `tape/mod.rs:58`; grep over `crates/core/src/` returns **zero** references outside `runtime/tape/` (verified). The live substrate is the eager `CssStructBuilder` `OpenFrame` (`css_l4/builder.rs:16,66`) — confirmed at those lines. The firewall conclusion — "the tape is NOT a sidecar beside a live tape, it is the SK-V18 fold target sitting dormant; NO parallel-substrate violation within crates/core" — is the correct Lock-1 reading: an unwired dead construct is not a second substrate, and the AoS-vs-SoA cross-tree gap (COH17-001) is a fold-convergence question, not a same-tree second substrate.
**Why ACCEPT:** No Track-1 ≡ Track-2 dishonesty asserted; the verify_action ("grep to confirm exactly one tape survives once wired; confirm OpenFrame builders are RETIRED not retained beside it") is exactly the Lock-1 firewall the fold inherits.

### CH5-S2 — Sidecar/retained-structural-projection row (1f-anti-pattern AP17-002 + U-AP17-001)
**Subject:** `1f-anti-pattern.md:56`, `:65-72`, `:78`.
**Disposition: ACCEPT.**
**Evidence:** `OnceCell<StructuralIndex>` initialized via `scan_structural` resolves at `crates/core/src/grammar/generated/json.rs:701-702,722,732` (cited `:686,702,732` — `:686` is the doc-comment opening, `:701-702` the field, `:732` the init; all within the cited band). The Lock-1 prose "if structural offsets are retained, the structural projection IS the tape" is verbatim at `ARCHITECTURE.md:1088` (verified). The disposition — "here the retained index feeds the eager OpenFrame builders, NOT a tape; it is a retained scan cache, not the tape projection … classify at fold: retained scan cache vs tape projection" — is the precise Lock-1 distinction. The open question U-AP17-001 correctly binds it to the four `substrate_target` manifest values (`LOCKS.md:120-127`, verified: `local_temp_only`/`existing_tape`/`direct_sink`/`admitted_fact_output`).
**Why ACCEPT:** This is the strongest hidden-coupling catch in the present set — it names the only LIVE retained projection, refuses to pre-classify it, and pins it to REDRESS-53 (SPEC §9). No parallel-substrate assertion; the firewall holds.

### CH5-S3 — Renamed-scanner / cross-call classifier-state row (1f-anti-pattern, Lock 1 v+1)
**Subject:** `1f-anti-pattern.md:59`.
**Disposition: ACCEPT.**
**Evidence:** "crates/core scan is `scan_structural(input, &alphabet)` producing a `StructuralIndex` per call; no cross-call classifier-state retention visible" — `scan_structural` confirmed in `crates/simd-scan/src/`; Lock 1 v+1 ELEVATION ("no cross-call retained classifier state … carry MUST stay within a single chunk-call boundary") resolves at `LOCKS.md:137-149` (verified). The row correctly states the REJECT class (`retained-across-call-boundary`, `LOCKS.md:141-148`) is NOT tripped, and gives a concrete verify_action (grep `prev_state|carry|prefix_xor|retained` over `crates/simd-scan/src/`).
**Why ACCEPT:** The renamed-scanner / classifier-carry violation is the subtlest Lock-1 v+1 trap; the row holds it open with the right primitive (`retention_lifetime`) and does not falsely clear it.

### CH5-S4 — God-module row, "nine pending_* Vecs"
**Subject:** `1f-anti-pattern.md:57` ("9 `pending_*` Vecs (`:74-79`)"); Executive Summary `:48` ("nine `pending_*` slabs").
**Disposition: REVISE.**
**Evidence:** `CssStructBuilder` (`crates/core/src/runtime/css_l4/builder.rs:66-80`) declares **7** `pending_*` fields, of which **6 are `Vec<…>`** (`pending_rules`, `pending_decls`, `pending_selectors`, `pending_values`, `pending_blocks`, `pending_components`) plus **1 `Option`** (`pending_value: Option<CssTypedValue>`, not a Vec). The count "nine `pending_*` Vecs" overstates by both the field count (7 not 9) and the Vec count (6 Vecs). The 817-LOC file size and the god-module disposition are correct; only the slab count is wrong.
**Why REVISE not REJECT:** The structural conclusion (god-module-shaped, fold-deletion target) stands; only the numeric is inflated. Inflated counts are a CH1/CH6 paper-close smell even when the verdict is right.
**Concrete fix:** Edit `1f-anti-pattern.md:48` and `:57` to read "six `pending_*` `Vec`s plus a `pending_value: Option` (`:71-79`)". Re-cite the band as `:71-79` (the struct body), not `:74-79`, since `pending_value` at `:71` is part of the slab count.

### CH5-S5 — Grammar-name-leak row (1f-anti-pattern AP17-003, Lock 14 firewall)
**Subject:** `1f-anti-pattern.md:58`.
**Disposition: ACCEPT.**
**Evidence:** `CssStructBuilder`/`JsonStructBuilder` referenced by literal path in `crates/core/src/backend/rust/emitter/shapes/substrate.rs` — confirmed: `substrate.rs:60` carries the literal `"::bbnf::runtime::css_l4::CssStructBuilder"` doc, and `builder_path`/`document_path` (`:41,:43,:53,:55`) splice the path from `EmitStrategy::StructDirect { rust, .. }` as **data**, not a grammar-name branch. The row's verdict — "these are PER-GRAMMAR runtime surfaces (Lock 14 ALLOWED); the emitter consumes a `builder_path`/`document_path` as DATA, not a grammar-name branch" — resolves against the live code. The generic tape carrying NO route strings is verbatim at `tape/mod.rs:54-56` (verified: "dispatches on the [`StructLayout`] … never on per-grammar route strings").
**Why ACCEPT:** The hardest Lock-14 firewall case — distinguishing a per-grammar runtime surface (allowed) from a grammar-name branch in generic code (forbidden) — is correctly drawn at the data-vs-branch seam, with the right verify_action for the fold.

### CH5-S6 — 5-shape BackendShape canon framing (Lock 10) (1f-coherence COH17-004 + Cross-Tree map row)
**Subject:** `1f-coherence-scan.md:70` (COH17-004), `:85` (BackendShape map row), `:94`, `:106`.
**Disposition: ACCEPT.**
**Evidence:** Lock 10's 5-shape search domain `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` resolves verbatim at `LOCKS.md:107-108` (verified). `CollapsedStage` "x86 AVX-512 … `target.arch == x86` + `target.avx512bw` … aarch64 mechanically refused" resolves at `ARCHITECTURE.md:1206` (verified). The COH17-004 verdict — "the 5-shape canon holds at Lock 10; the canon's CollapsedStage admission is x86-pinned, mechanically refused on aarch64; SK-V17's NEON union sits entirely under the four LLVM shapes' scan-leaf FFI, NOT CollapsedStage; T-P2 must map how aarch64-NEON absorbs into the canon without a 6th shape" — is the exactly-correct Lock-10 framing. The "tape-as-unified-substrate" question is posed as **absorption into the existing 5 shapes**, never as a 6th shape or a new substrate (SPEC §9 `:807-811` bars "sixth `BackendShape`" — verified at the read band).
**Why ACCEPT:** The lens's core BackendShape charge — canon excavated, no 6th-shape pressure, aarch64-NEON framed as absorption — holds. The "tape-as-unified-substrate" is framed as the ONE Lock-1 substrate projecting via the 5 shapes, not a parallel plane.

### CH5-S7 — Second-substrate / monotonic-direction firewall (1f-past-corpora PC17-006 + D6)
**Subject:** `1f-past-corpora.md:57` (Second-substrate D6 row); Direction-Monotonicity Note `:60-69`.
**Disposition: ACCEPT.**
**Evidence:** SPEC §9 second-substrate block names skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`, public `UnionTape`, "sixth `BackendShape`", "parallel source passes", "cross-call classifier-state retention" as forbidden — resolves at `SPEC.md:807-811` (verified). The row's most load-bearing catch — "the §9 second-substrate block names skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor` as FORBIDDEN-in-skinny constructs; these are precisely the crates/core fold-target names; the fold must NOT relocate them INTO skinny as a second substrate; SK-V18 adopts the PROVEN skinny `Tape`/`ValueRef` shape into crates/core, not vice-versa" — is the exact hidden-coupling the firewall exists for: a naming collision between the forbidden-in-skinny names and the fold-target names that could mask a direction-reversal (totality → skinny) second-substrate smuggle. The monotonic skinny→totality direction is verbatim-grounded at SPEC `:110-114`.
**Why ACCEPT:** This is the single most important Track-1 ≡ Track-2 honesty catch in the corpus and it is correctly drawn. No parallel-substrate assertion; the direction guard is explicit.

### CH5-S8 — Cross-tree shape-drift framing (1f-coherence COH17-001/003, Executive Summary)
**Subject:** `1f-coherence-scan.md:67` (COH17-001), `:69` (COH17-003), Exec Summary `:42-61`.
**Disposition: REVISE.**
**Evidence:** Both rows resolve precisely (core AoS `TapeRec` at `record.rs:103,120` const-asserted 16-byte `#[repr(C, align(4))]`, verified; skinny SoA `Tape` `offsets:Vec<u32>`+`flag_cursors`+`flag_values`+`payloads` at `skinny/.../tape/mod.rs:94`, verified; `ValueRef<'doc,'input,K,G>` at `:175`, verified; TapeStructBuilder grep-zero outside tape/, verified). COH17-001 correctly closes with "NOT a parallel substrate within either tree — a cross-tree shape mismatch" and COH17-003 with "an asymmetry T-P2 must reconcile". **The REVISE is on a firewall-completeness gap, not an error:** the Executive Summary and the Cross-Tree Substrate Map (`:80`) describe TWO tape encodings (core AoS / skinny SoA) coexisting across trees as the fold subject — which is honest — but neither artefact states the firewall invariant that **after** the SK-V18 fold there must be exactly ONE encoding (Lock 1: "columnar SoA is dead; orthogonal codepaths and parallel substrates are dead", `LOCKS.md:75`). The catalogue frames the two-encodings state but does not pin the post-fold one-substrate firewall as an explicit divergence row; U-COH17-002 raises it only as an open question, leaving the Lock-1 closure obligation un-catalogued.
**Why REVISE not ACCEPT:** A reader could take "both exist, neither is parallel within its tree" as license for the two encodings to persist post-fold across trees — a latent two-substrate end-state the firewall must name now. The omission is a hidden coupling: the cross-tree mismatch is admissible only as a transient fold-state, never an end-state.
**Concrete fix:** In `1f-coherence-scan.md`, add to COH17-001's Note (or a new divergence row): "Lock-1 closure obligation — post-SK-V18 exactly ONE tape encoding survives across both trees (`LOCKS.md:75` 'parallel substrates are dead'); the AoS/SoA coexistence is admissible ONLY as a transient fold-state. A post-fold dual encoding is a Lock-1 violation, not a tree-local choice." Upgrade U-COH17-002 from open-question to a catalogued divergence with this invariant.

## CH5 Firewall Verdict (Track 1 ≡ Track 2)

Within the **present** 1F artefacts, NO Track-1 ≡ Track-2 dishonesty is asserted
and NO catalogued state implies a live parallel substrate, sidecar producer, or
renamed-scanner violation. The substrate-union (Lock 1) facts that ARE carried —
the unwired single tape, the `OnceCell<StructuralIndex>` retained scan cache, the
no-route-string generic builder, the monotonic skinny→totality direction guard,
the forbidden-name collision catch — are excavated correctly and resolve to live
file:line. The 5-shape BackendShape canon (Lock 10) is excavated whole, x86-pinned
CollapsedStage correctly flagged, aarch64-NEON framed as absorption without a 6th
shape. The tape-as-unified-substrate question is framed as the ONE substrate
projecting via the 5 shapes, never a parallel plane.

The firewall's two live exposures are: **(1) CH5-S0** — the 1A/1B inventories that
SHOULD carry these facts are absent, so the substrate-union spine has no dedicated
home and 1F is doing 1A's job (REJECT, blocks §4 convergence); **(2) CH5-S8** —
the catalogue frames the AoS/SoA cross-tree coexistence honestly but does not pin
the Lock-1 post-fold one-substrate closure obligation as an explicit divergence
(REVISE). One numeric inflation (CH5-S4, nine→six pending_ slabs, REVISE).

## Disposition Summary

| Section | Subject | Disposition |
|---|---|---|
| CH5-S0 | Missing 1A/1B inventories (firewall has no spine) | **REJECT** |
| CH5-S1 | Parallel-substrate firewall (AP17-001) | ACCEPT |
| CH5-S2 | Sidecar / retained structural projection (AP17-002) | ACCEPT |
| CH5-S3 | Renamed-scanner / cross-call classifier state | ACCEPT |
| CH5-S4 | God-module "nine pending_*" count | **REVISE** |
| CH5-S5 | Grammar-name-leak (Lock 14) | ACCEPT |
| CH5-S6 | 5-shape BackendShape canon framing (Lock 10) | ACCEPT |
| CH5-S7 | Second-substrate D6 + monotonic direction | ACCEPT |
| CH5-S8 | Cross-tree shape-drift / Lock-1 closure obligation | **REVISE** |

**Counts: ACCEPT 6 · REVISE 2 · REJECT 1 (9 sections).**
ACCEPT rate 66.7%. The pass does NOT meet the §4 ≥95% criterion; the REJECT
(CH5-S0) is a structural pass-completeness defect that blocks convergence until
1A/1B are dispatched and the substrate facts migrate out of 1F.
