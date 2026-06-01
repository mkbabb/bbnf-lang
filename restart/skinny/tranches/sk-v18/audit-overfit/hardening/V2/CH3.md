# S-P0 audit-overfit hardening V2 — CH3 REGRESSION (post-fold confirm)

Lens (CH3 REGRESSION): the residual-overfit audit must not (1) re-open a §0.4 pre-blocked
REDRESS family, (2) prune >SOTA-bearing code, (3) contradict a carried-forward VALIDATED
(αD) fact, or (4) ship a PRUNE-sequencing graph whose revert chain lets a downstream wave
march over a REDRESSed predecessor. V2 is the POST-FOLD confirm of the single V1 CH3 REVISE
(R1-CH3, the revert-chain prose disambiguation — (i) SYNTHESIS §5 fact-3 G2 dual entry-gate,
(ii) the a2 §4 directional-inversion title). Subject: `a0..a3` + `SYNTHESIS-AUDIT-OVERFIT.md`
+ `hardening/HARDENING-S-P0-CONSOLIDATED.md`. Every disk claim re-grepped this pass at live
HEAD `83b66db42` (`git rev-parse HEAD` = `83b66db4232374db6a5f9fa009882f41acc04342`) — not
inherited from the audit prose nor from the prior V2/CH3 draft.

## Disk re-verification (CH3-critical facts, independently re-grepped at `83b66db42`)

- **P3 collapses byte-IDENTICAL replicas — no unique code lost.** All 7
  `crates/runtime/src/grammars/css_l4_*/generated.rs` share md5
  `b654562ccff46ed62dd48e9ace325830` (re-computed across all 7 this pass). The collapse
  deletes 6 byte-for-byte replicas; nothing distinct is pruned.
- **P1 prunes DORMANT x86 — zero aarch64-reachable, zero >SOTA.** `find` confirms
  `src/x86_64/` = 24 files + `ext/x86/` = 4 files; `build.rs:37-41` early-returns when
  `CARGO_CFG_TARGET_ARCH != "x86_64"` ("AArch64 + wasm32 + RISC-V + scalar: no assembler
  invocation needed. return"). The tree is build-inert on aarch64; deletion removes no
  >SOTA-bearing code. The build-soundness coupling (9 active `bbnf_simd::x86_64::…` sites in
  `checkasm_parity.rs:458…502`) is correctly the exit-gate `cargo test --no-run` clean,
  decoupled SAME-wave (a2 §3) — not a deferred follow-on that could leave a broken-build state.
- **P2 keeps the headline harness.** `css_canon_bench` is KEPT in the prune (P2 deletes only
  the warm `measure_mbps`/`lightningcss_facts` micro-fixture path, which V3 C3 confirms did NOT
  produce the headline numbers). No >SOTA-bearing measurement deleted.

## V1 REVISE discharge — R1-CH3 (both halves, disk-confirmed)

The lone V1 CH3 REVISE was the revert-chain prose disambiguation. DISCHARGED in BOTH halves,
re-grepped this pass:

- **(i) G2 dual entry-gate.** `SYNTHESIS-AUDIT-OVERFIT.md:204-205` now carries: "(Note the dual
  entry-gate: G2 entry-gates on BOTH G1 AND P3 — a P3 failure also blocks G2, independent of G1;
  the dependency-graph diagram above is authoritative)." A reader reconstructing the revert chain
  can no longer read it as G2-blocks-on-G1-only — the **P3 → blocks G2** edge is explicit, which
  is exactly the edge that prevents G2 marching while the 7-replica collapse (P3) is still RED.
- **(ii) a2 §4 directional inversion.** The §4 title is now "**Sequencing constraint 3: G1/G3
  co-derive; G3-failure blocks PROVE**" (`a2:282`), with the directional note "the binding
  revert/precondition arrow is **G3-un-fork-FAILURE → blocks PROVE** (forward), NOT a backward
  'G3 gates G1/G2'" (`a2:285`). The §4 disposition (`a2:386`), §7 addenda table (`a2:497`), and
  §8 summary (`a2:519`) all read the forward arrow. A whole-corpus grep for the inverted phrasing
  `gates G1/G2 | gating G1/G2 | G3 gates | un-fork gates` returns ONLY the explicit
  quote-and-correct fold notes (`a2:41` ledger, `a2:285/288/387` corrective phrasing, `a0:469`
  corrective phrasing, `HARDENING-S-P0-CONSOLIDATED.md:26`) — zero load-bearing assertion inverts
  the arrow.

Both are prose-disambiguation of an already-correct, SYNTHESIS-declared-authoritative dependency
graph (`PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1`). No gate touched, no architecture
changed, the revert chain itself never inverts. DISCHARGED.

## Dispositions (each independently re-verified)

- **No §0.4 pre-block re-opened (ACCEPT).** Cross-checked the prune/generalize surface against
  the binding pre-block families (`SYNTHESIS.md:386-455`): AZ-IV eager-value-tree (G4 trait stays
  LAZY over the tape — `:386` "no second substrate"); StructRegistry/Arena<G>/Builder<G> per-leaf
  indirection (none introduced); CSS fact-stream-String admission plane (G2 retires the courier
  TOWARD lowering, not toward a fact-stream String); `W5C_REQUEST_FACT_PROFILES` relocation
  (policed structurally by the P3 collapse gate, inherited not re-opened); 24-row broadcast
  (`:417-418`, untouched); FNV/fixture contrivances (stay bench-only); x86/AVX/SVE (P1 ENFORCES by
  deletion — enforcement, NOT re-open, `:38-41`); the verbatim-blob / phantom-generic /
  distinct-grammar-output re-entries (`:42-50` — the audit's L1/L4/L2 lenses ARE these pre-blocks'
  machine-checks). The audit re-opens NONE; every prune is the pre-block's own enforcement arm.
- **Prune list deletes no >SOTA-bearing code (ACCEPT).** Disk-verified above: P1 = DORMANT x86 (0
  reachable intrinsics, `build.rs` early-returns); P2 = the warm path that did NOT produce the
  headline (KEEPS `css_canon_bench`); P3 = byte-identical replicas (no unique code); P4 =
  gate-tightening; P5 = symbol rename at source. The CLEAN-KEEP inventory
  (`SYNTHESIS-AUDIT-OVERFIT.md:107-111`) explicitly preserves `css_canon_bench`, the 14-file
  checkasm discipline (12 single-kernel + 2 aggregate, KEPT-and-DECOUPLED aarch64 parity), the
  neutral NEON kernel, and the substrate — the aarch64 hardening is not thrown out with the x86
  bathwater.
- **VALIDATED (αD) facts preserved (ACCEPT).** The checkasm count is the disk-true **14** (a2:277:
  "12 single-kernel + 2 aggregate, NOT 18 — alphaE is the count-correct reference"), matching the
  folded αD V3-REVISE resolution; no audit row contradicts a carried αD CLEAN/KEEP fact (Lock 1
  substrate-union, JSON >sonic cold strict, CSS canonical cold, neutral kernel, honest harness,
  regen plumbing, FNV quarantine). No fold edit this cycle contradicts a folded αD fact.
- **R16 / F-A3.5 does not regress the gate (ACCEPT, now broader).** The R1-CH5 fold widened the
  `runtime_target_rows_collapsed` recipe to inline BOTH nested structs — `frontend_requirements`
  (#11) AND `output_labels` (#12). This SHARPENS the IMPLEMENTATION RECIPE (cleanest mechanism:
  `#[derive(PartialEq)]` on `RuntimeTarget`, `regen.rs:5` currently Clone/Copy/Debug only; both
  nests already `PartialEq, Eq`); the by-exclusion close-gate DEFINITION is unchanged (it already
  excludes only `output_dir`/`expected_files`, so both nested structs are in the comparison set).
  The gate stays correctly RED pre-P3 (7 distinct `profile` + `output_labels`). a3 §3 reconciles
  with a2 §4a / a0 §L2 without contradiction. No regression.
- **REDRESS-W2-1 single-emitter (ACCEPT — SUBJECT, not re-open).** Carried as the G3 SUBJECT
  "admitted to be discharged here" (`SYNTHESIS.md:445`), the discharge of an SK-V17 residual — NOT
  a re-opened REDRESS family. The L3 single-emitter lens IS the discharge machine-check; the R1-CH3
  directional fold makes the discharge arrow (G3-failure → PROVE) explicit, so PROVE cannot emit
  Sheets through a surviving fork.

## Sequencing-soundness (the CH3 fourth vector — the revert chain itself)

The entry-gate dependency chain (`a2:206`, `SYNTHESIS §5:184-193`) is sound and now
prose-unambiguous: every wave that fails its exit gate BLOCKS each downstream wave that
entry-gates on it (G1 → G2/G3/G4/PROVE; G3-un-fork → PROVE; the dual P3 → G2 edge). The
intra-PRUNE edge P4-before-emitter-rebuild (`a2 §2.1c`) prevents a grammar-named branch landing
under a blind Lock-14 gate. The P1↔`checkasm_parity.rs` decouple is bound SAME-wave, so no
intermediate broken-build state exists. No reachable revert path lets a wave march over a
REDRESSed predecessor.

## REVISE (0) / REJECT (0)

The single V1 CH3 REVISE is discharged in both halves on disk; no §0.4 pre-block re-opened, no
>SOTA-bearing surface pruned (the prune deletes only dormant/identical/warm-non-headline code),
no folded αD VALIDATED fact contradicted, and the revert chain prose now reads at the graph's
direction with the dual P3 → G2 edge explicit. Zero orphan REVISE survives into this lens.

## Tally
ACCEPT 6 · REVISE 0 · REJECT 0 — **100%**. R1-CH3 DISCHARGED (G2 dual entry-gate at
`SYNTHESIS-AUDIT-OVERFIT.md:204-205`; a2 §4 title + body all "G1/G3 co-derive; G3-failure blocks
PROVE", inverted phrasing only in quote-and-correct fold notes). 7× css_l4 md5 identical
(collapse loses nothing); x86 dormant (`build.rs:37-41`); `css_canon_bench` KEPT; checkasm 14;
REDRESS-W2-1 is the G3 SUBJECT not a re-open. No pre-block re-opened, no >SOTA pruned, no αD fact
contradicted, no revert-chain march-over.

TALLY accept=6 revise=0 reject=0
