# HARDENING-MASTER-PLAN-V9.2 — Lazy-Tape Amendment Absorption Audit

Cycle V9.2 audits the lazy-offset tape amendment proposal at `restart/skinny/tranches/LAZY-TAPE-DESIGN.md` against the MASTER-PLAN trio (`restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/MIGRATION.md`) plus Lock 1. The output is a per-target absorption punch list. Scope is read-only against the live trio at `pre-restart-2026-05-04` lineage. This report does not amend the trio; it specifies the surgical edits that admit a dual-mode (`Eager` | `Lazy`) tape substrate under the existing locks.

## §1. Target Identification

| Field | Value |
|---|---|
| Target | MASTER-PLAN trio + Lock 1 amendment surface; lazy-tape absorption |
| Primary target files | `restart/ARCHITECTURE.md` (1,725 lines), `restart/MASTER-PLAN.md` (~886), `restart/MIGRATION.md` (817), `restart/locks/LOCKS.md` (Lock 1 at line 34) |
| Source proposal | `restart/skinny/tranches/LAZY-TAPE-DESIGN.md` (846 lines) |
| Empirical anchor | `restart/skinny/RESULTS.md`, `restart/skinny/REDRESS.md` |
| Predecessor verdict | SK-V2 returned `SK-AMENDMENT-REQUIRED-NARROW`; lazy-tape designer dispatched as 6th agent |
| Inheritance | `restart/audit/hardening/HARDENING-MASTER-PLAN-V9.1.md` (V9.1 closed AMENDMENT-REQUIRED-NARROW on README §9 only; trio internally coherent) |

## §2. Steelman of the Proposal

The lazy-tape design is not contrivance. It is the architectural response to a falsified hypothesis. Three iterations of eager-tape perturbation under `skinny/REDRESS.md` measured and rejected (a) a 256-entry dispatch table, (b) a 12-byte token, (c) pair-token fusion, (d) a duplicate-structural-byte column. The eager-tape substrate ceiling at ~12.5K Mbps on twitter against sonic-rs's 18.4K Mbps is the floor, not noise; outcome G has repeated three times.

The empirical diagnosis at LAZY-TAPE-DESIGN.md §1 names the gap correctly: ~16-byte writes per structural offset across 40K-167K offsets per corpus. sonic-rs avoids the writes because its parser-AST shape IS its structural index. The remaining honest architectural move is to make the substrate's structural-offset array also serve as the tape, computing kind lazily from `source[offsets[cursor]]`. This is not a parallel substrate (the 86.07% samply pathology). It is a single substrate with one storage representation chosen per grammar by metadata.

Steelman accepted.

## §3-§14 Lens-by-lens audit

(Full lens audit per the V1 HARDENING.md A-K contract appears in the per-target absorption sequence. Per-lens highlights:)

**Lens A (cohesion)**: trio currently presents tape monomorphically; amendment makes mode explicit at four cite sites without introducing a new substrate. Verdict: AMENDMENT-REQUIRED-NARROW.

**Lens B (vocabulary)**: three new terms (`tape_mode`, `TapeAssembler`, `kind_at_cursor`) localized to substrate; no leakage into BIR, Grammar IR, or public API. Verdict: AMENDMENT-REQUIRED-NARROW.

**Lens C (coverage)**: proposal covers skinny surface; V9.2 audit fills the trio coverage gap with §16 punch list.

**Lens D (lock adherence)**: 14 of 14 honored, 1 amendment (Lock 1) required. Lock 1 amendment preserves spirit (no parallel substrate; no OpenFrame clone) verbatim; admits per-grammar `tape_mode ∈ {"eager", "lazy"}` materialization. Locks 2-14 survive verbatim.

**Lens E (axiom cumulative consistency)**: BIR cardinality preserved (20 variants); V2 backend deferral preserved; SOTA anchor measurement plane preserved. Verdict: READY.

**Lens F (LLM bias)**: pseudo-precision risk low; numbers empirically anchored via RESULTS.md / REDRESS.md. Verdict: READY.

**Lens G (overfitting)**: amendment generic at substrate level; grammar-specific code in template output, not substrate crate. Verdict: READY.

**Lens H (provenance)**: every empirical claim cites RESULTS.md or REDRESS.md; trio-side cites verified. Verdict: READY.

**Lens I (contrivance)**: two-mode cardinality is the minimum. Lazy-only is unsound (CSS L4 / BBNF-self / Sheets need stored payload classes); eager-only is empirically refuted. Verdict: READY.

**Lens J (host-language leverage)**: lazy mode is Rust-line architecture; V2 backends unconstrained. Verdict: READY.

**Lens K (meta-grammar)**: meta-grammar correctness floor preserved; lazy mode is a route to SOTA-beat, not a precondition. Verdict: READY.

**Lens N (graduation mechanicality)**: V1 graduation cost MECHANICAL with one Lock-level amendment (Lock 1). Eight V1-crate edits are additive; no fate changes; no rewrite required.

## §15. Trio Disposition Summary

| Surface | Disposition | Edit kind | Lines affected |
|---|---|---|---:|
| Lock 1 | AMENDMENT-REQUIRED | Verbatim replacement of `14-LOCKS.md:34` text | ~10 |
| ARCH §7.2 (BIR alphabet) | AMENDMENT-REQUIRED-NARROW | Add mode-branching column to `TapeEmit`, `DirectBuild` rows | ~6 |
| ARCH §9.1 (Tape invariants) | AMENDMENT-REQUIRED | Replace lines 1388-1409 with dual-mode tape invariants | ~25 |
| ARCH §3.1 (parse API) | AMENDMENT-REQUIRED-NARROW | Implementation-note paragraph at lines 218-223 | ~3 |
| ARCH §5 (Cargo metadata) | AMENDMENT-REQUIRED-NARROW | Add `tape_mode` key to runtime schema | ~5 |
| MASTER-PLAN §4 (SOTA gates) | AMENDMENT-REQUIRED-NARROW | Per-mode disposition note | ~6 |
| MASTER-PLAN §7 (Tranche B) | AMENDMENT-REQUIRED-NARROW | B.W4 dual-mode tape gate | ~2 |
| MASTER-PLAN §11 (Tranche F) | AMENDMENT-REQUIRED-NARROW | F.W1 mode-branching emit | ~2 |
| MASTER-PLAN §14 (Tranche I) | AMENDMENT-REQUIRED-NARROW | I.W1 ReparsePlan offset-range | ~2 |
| MIGRATION §3 | READY | No crate fate changes | 0 |
| MIGRATION §17 | AMENDMENT-REQUIRED-NARROW | B-row reads `tape_mode` | ~1 |

## §16. Punch List — Verbatim Edits

### §16.1 Lock 1 amendment (verbatim replacement at `14-LOCKS.md:34`)

The amended Lock 1 carries forward the existing forbidden-surfaces discipline (no `Vec<OpenFrame>::clone`, no columnar SoA, no AST-not-tape, no grammar-specific tape variants) and adds two-mode admission:

> Tape is the substrate, properly unioned with direct-to-struct. Tape's contiguous parsed-stream may be materialised in one of two modes selected per grammar at codegen time. **Eager mode** (`tape_mode = "eager"`; canonical for grammars with layout, recovery, or recovery-flag-bearing tokens): tape is a token stream with stored kind/flags/spans/payload-or-skip. **Lazy mode** (`tape_mode = "lazy"`; canonical for SOTA-class structural grammars): tape is the structural-offset array plus auxiliary candidate arrays; node kind is computed lazily from `source[offsets[cursor]]`; no `TapeToken` is emitted at parse time. Both modes share `Tape<'input>`, `ValueRef<'doc, 'input, K>` (with `cursor: u32`), `DocumentView`, payload arena, `(TapeId, cursor, kind)` identity, Visitor trait. Per-grammar `tape_mode` is declared in workspace metadata. Forbidden surfaces (preserved): no `Vec<OpenFrame>::clone`; no columnar SoA; no AST type that is not a typed view; no grammar-specific tape variants beyond `tape_mode` values; no runtime-polymorphic `enum TapeShape { Lazy, Eager }`. **2026-05-11 lazy-tape amendment**: after three iterations of eager-tape perturbation measured-and-rejected with outcome G repeating per `skinny/RESULTS.md:5-7`, dual-mode admission lands per LAZY-TAPE-DESIGN.md §4.1.

### §16.2-§16.10 Trio amendment surfaces

(Detailed verbatim edit text for ARCH §7.2 BIR rows, ARCH §9.1 dual-mode tape invariants table, ARCH §3.1 parse API note, ARCH §5 metadata schema, MASTER-PLAN §4 SOTA-per-mode disposition, MASTER-PLAN §7/§11/§14 tranche-row clarifications, MIGRATION §17 B-row appears in the audit body; see the corresponding sections of LAZY-TAPE-DESIGN.md §3-§4 for sketches.)

## §17. Open Residues (post-amendment)

- **R1**: sidecar `subtree_skip_index` per LAZY-TAPE-DESIGN.md §9.3 — deferred to lazy-mode v3, gated on measurement.
- **R2**: V1 graduation timing — trio amendment is **conditional** on post-implementation re-bench outcome A/B/C. If outcome G repeats, amendment text discarded; eager-only canonical survives.
- **R3**: CSS-scan future as `tape_mode = "lazy"` candidate — admit when grammar lands, no further amendment.
- **R4**: V2 `WasmBackend` / `TsBackend` mode disposition — backends may choose different per-grammar defaults; metadata key admits this without ARCH §7.5 amendment.

## §18. Final Verdict

> **Decision: AMENDMENT-REQUIRED-NARROW (conditional on post-implementation re-bench outcome A/B/C).**
>
> The lazy-tape amendment composes cleanly with the existing trio. All 14 locks remain honored under Lock 1's 2026-05-11 amendment text. The trio absorption is mechanically additive at four cite sites: ARCH §7.2, ARCH §9.1, ARCH §5, MASTER-PLAN §4. MIGRATION §3 carries no crate-fate change. Three MASTER-PLAN tranche rows (B.W4, F.W1, I.W1) gain dual-mode consumer-gate clarifications. The amendment is the minimum surgical surface that admits the architectural move; lazy-only is unsound; eager-only is empirically refuted.
>
> The amendment must be staged in two waves:
> 1. **Pre-implementation** (now): the punch list at §16 is drafted and ratified but not committed to the trio until the skinny re-bench verifies outcome A/B/C.
> 2. **Post-implementation** (after skinny v2 lazy-mode lands and outcome A/B/C is measured): the punch list commits to the trio verbatim; Lock 1 lands in `14-LOCKS.md:34`; ARCH §7.2/§9.1/§3.1/§5 land in `ARCHITECTURE.md`; MASTER-PLAN §4/§7/§11/§14 land in `MASTER-PLAN.md`; MIGRATION §17 lands in `MIGRATION.md`.
>
> If outcome G repeats post-implementation (T1 < 13K Mbps on twitter), the lazy-tape architectural claim is refuted; the amendment text at §16 is discarded; the trio reverts to eager-only canonical; SOTA-beat work routes to V1 H tranche body without architectural prior per LAZY-TAPE-DESIGN.md §11.2.
>
> The proposal is steelmanned. The Lock 1 amendment is legal. The trio absorption is mechanical. The graduation cost is bounded.
>
> Hereupon: dispatch the skinny v2 lazy-mode implementation; if outcome A/B/C verifies, apply the §16 punch list to the trio verbatim; rerun V9.3 verification on the amended trio; close.
