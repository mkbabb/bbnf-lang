# SK-V18 S-P0 Audit-Overfit — A2: PRUNE-list + Architectural Sequencing Constraints

**Pass:** S-P0 Overfit Audit (the FIRST pass of SK-V18; `PASS-0-OVERFIT-AUDIT.md`).
**Cycle:** V3 (second-consecutive ≥95% confirm), agent A2.
**Scope (this artefact):** the SK-V18 **PRUNE-list** (P1–P5) + the **architectural
sequencing constraints** that bind the GENERALIZE/PROVE waves to PRUNE — specifically
the four named couplings:

1. **PRUNE-before-GENERALIZE-before-PROVE** (the standing order);
2. **build-soundness coupling P1 ↔ `checkasm_parity.rs`** (the `src/x86_64/` deletion
   is not build-sound without decoupling 9 active compile-coupled call sites);
3. **G1/G3 co-derive; G3-failure blocks PROVE** (one grammar-agnostic emitter cannot
   exist while two forks survive; the binding directional claim is that **G3 un-fork
   FAILURE blocks PROVE** — the graph orders G3 *after* G1/G2 (PRUNE → G1 → G2 → G3), and
   G1+G3 *co-derive* the unified emitter; "gating" here is the un-fork's PROVE precondition,
   not a backward arrow from G3 onto G1/G2 — see §4 / R1-CH3 fold);
4. **G4 phantom-generic DELETE-default + JSON-richness guard** (the `G: EventGrammar`
   axis is the phantom — DELETE is the abrogate-before-patch default; the shared trait
   must not LCD-flatten JSON's rich navigation).

**Bracket / disk truth:** all path:line citations below were grep-verified LIVE at
**HEAD `83b66db42`** (the contract was authored at bracket `318d9c046`; the V5/confirm
propagation residuals have since been folded into the binding `SYNTHESIS.md`/`HANDOFF.md`
rows — confirmed this pass, see §6).

**Cited inputs (binding):**
- the **V3 backlog**: `restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md`
  (D1–D4, C1–C3, §SK-V18 actionable backlog P1–P5/G1–G6/PROVE/H1) +
  `AGENT-{1..6}-*.md`.
- the **Alpha shortlist**: `restart/skinny/tranches/sk-v18/research/alpha/alphaE-candidate-shortlist.md`
  (CANDIDATE A = P1–P5; B1–B4; the SUMMARY TABLE; CROSS-CUTTING NOTES; the F1–F16 fold
  ledger).
- the LOCKED goalset:
  `restart/skinny/tranches/sk-v18/{SYNTHESIS.md,HANDOFF.md}` +
  `research/alpha-hardening/CONSOLIDATED-CONVERGED.md`.

**V2 FOLD LEDGER (the V1 `hardening/V1/CH{1..7}.md` REVISE mass resolved into this artefact):**

| V1 REVISE | target | A2 disposition this cycle |
|---|---|---|
| **R1-CH3** | a2 §4 constraint TITLE ("G3 un-fork gating G1/G2" inverts the arrow) | **FOLDED** — the §4 title + §0 item 3 + §7/§8 restated to the accurate directional sense "**G1/G3 co-derive; G3-failure blocks PROVE**" (the graph orders G3 *after* G1/G2; the binding revert claim is G3-failure→PROVE, never a backward G3→G1/G2 arrow). The SYNTHESIS fact-3 dual-entry-gate half of R1-CH3 was already discharged at HEAD (`SYNTHESIS-AUDIT-OVERFIT.md:203-204`). |
| **R1-CH5** | a3 §3 / SYNTHESIS fact 5 recipe-pin (recurse into BOTH nested structs) | a2 was already CORRECT (§4a line 321 enumerates `frontend_requirements` in the operative set, so the by-exclusion INVARIANT a2 states already covers it — CH5 confirms this). **SHARPENED** in a2 §4a: the structural P3 collapse-check must inline EVERY nested-struct field (`frontend_requirements` AND `output_labels`), and the cleanest sufficient mechanism is `RuntimeTarget: PartialEq` (a one-line derive add — disk: `regen.rs:5` carries only `#[derive(Clone, Copy, Debug)]`, both nested structs already derive `PartialEq, Eq`). The recipe-pin REVISE itself binds a3/SYNTHESIS, not a2. |
| R1-CH1 | a1 §L1 CSS-courier "646–910 LOC" range vs disk-measured 910 (`:701`→`:1611`) | NOT-a2 (a1 fold). a2 §1/§4a cite the 910-LOC body only via P3's "6 × 910" collapse — already the disk-measured figure; carried a one-line consistency note in §1 P3. |
| R1-CH2 | a1 §L1 (b) parameterization predicate → per-primitive mutate-falsifier | NOT-a2 (a1 fold). |
| R1-CH4 | G1 ±5% line-count = SOFT tripwire; binding = oracle diff-match | NOT-a2-primary (alphaE feeder / a0 §2.1 fold). a2 §7 G1 reference carries the oracle-diff-match framing already; no a2 edit needed. |
| R1-CH6 / R2-CH6 | SYNTHESIS R-A0-1 "beats" qualifier / R-A0-2 collapse-to-one answer | NOT-a2 (a0/SYNTHESIS folds). |

CH7 = 100% (0 REVISE / 0 REJECT). No REJECT raised against a2 in any V1 lens.

**V3 FOLD CONFIRM (the V2 `hardening/V2/CH{1..7}.md` re-grep + the V3 second-consecutive confirm):**
The V2 hardening cycle re-grepped every a2 witness at HEAD `83b66db42` and raised **ZERO REVISE /
ZERO REJECT against a2** across all seven lenses (CH1 §"a2 PRUNE-sequencing" ACCEPT; CH2 §"a2 §2/§4
sequencing" + §"a2 §3 build-soundness" ACCEPT; CH3 §"sequencing-soundness" ACCEPT; CH4 §"A4
P1↔checkasm" ACCEPT; CH5 §"P3↔G3" + §"build-soundness coupling" ACCEPT; CH6 §"(3) PRUNE-sequencing
SOUND" ACCEPT; CH7 §"A6 PRUNE-sequencing sound" ACCEPT). Both a2-bound V1 REVISEs (**R1-CH3**
directional-arrow, **R1-CH5** both-nested-struct recipe) are confirmed DISCHARGED on disk by every
V2 lens (CH3 V2 §"V1 REVISE discharge — R1-CH3 (both halves)"; CH5 V2 §"V1 REVISE discharge
(R1-CH5)"). **V3 hardening then re-confirms the same** (`hardening/V3/CH3.md` "R1-CH3 … holds in
BOTH halves"; `hardening/V3/CH5.md` "R1-CH5 … holds in BOTH parts"). The only non-blocking V2 item
(CH7 V2 §"One accuracy nit") is an explicitly **sub-REVISE a3 field-numbering cosmetic** routed to
the a3/synthesis layer — NOT an a2 defect, and the a2 §4a field-number references (`regen.rs:17`/
`:18` for the two nested structs) are themselves disk-exact (re-verified this cycle). Per
ORCHESTRATOR §3Z, the V2+V3 pair is **≥95% × 2 consecutive (both 100%, all 7 lenses), zero orphan
REVISE, V=3 ≤ 5** — the audit-overfit lens is CONVERGED; this a2 artefact carries no orphan REVISE.

---

## §0 — Standing verdict (A2)

The SK-V18 PRUNE surface is **NOT residually overfit**: the 5 PRUNE items are pure
deletion + gate-tightening (zero generalization risk, zero >SOTA-bearing code removed),
and the V3 backlog + Alpha shortlist enumerate them correctly. The S-P0 mandate for A2
is therefore not "find new overfit in the PRUNE-list" (there is none to find — PRUNE is
the *cure*, not a *symptom*) but to (a) certify every PRUNE item is anchored to LIVE disk
truth so a downstream wave cannot paper-close it on a stale citation, and (b) **bind the
four architectural sequencing constraints** so that the GENERALIZE/PROVE waves cannot
march over a RED predecessor or re-introduce the very overfit PRUNE deletes.

The one RESIDUAL-overfit risk that *does* survive into the GENERALIZE surface is the
**relocated-overfit-seam** (a per-grammar branch moved out of a `match grammar` arm into
a neutral-identifier `RuntimeTarget` data-table, which the arm-census regex is
syntactically incapable of catching). That seam is policed STRUCTURALLY by the P3
collapse close-gate (the F13→F16 projection-tuple lineage), NOT by the regex — and A2
binds it to P3 below.

**A2 disposition:** PRUNE-list ACCEPT (anchored, disk-true); sequencing constraints
ACCEPT-AND-BIND (the four couplings are real, load-bearing, and each carries a
build-soundness or generality hazard if violated).

---

## §1 — The SK-V18 PRUNE-list (P1–P5), disk-verified at HEAD `83b66db42`

Each row: the V3 finding id (cited from `CONSOLIDATED-AUDIT.md`), the Alpha-shortlist
sub-item (cited from `alphaE` CANDIDATE A), the LIVE disk witness verified this pass, and
the close-gate.

### P1 — DELETE the WHOLE x86 surface crate-wide (aarch64-only) — `[V3 D3 + CH5 V3 §C.5 + CH6 V4 §1 + V5 R-2/CH5 §F.6]`

The single most consequential PRUNE item and the only one with a **build-soundness
coupling** (§3 below). The V3 audit (`CONSOLIDATED-AUDIT.md:38-39` D3) found **one** x86
surface (`src/x86_64/`, 742 LOC, 24 files, 14 `unimplemented!("Wave 6")` stubs, 0 real
intrinsics); the Alpha fold ledger (`alphaE` F15, CH5 V3 §C.5) found the cohort had
**omitted a SECOND surface** — the deletion target is the union, reach-matched to the
verify grep so the gate is satisfiable-by-construction (a deletion list narrower than the
grep ships a RED-by-construction gate — the exact mirror of the V3 escape this fold
fixes).

**LIVE disk witness (`skinny/crates/bbnf-simd/`, HEAD `83b66db42`):**

| sub-target | disk truth (verified this pass) | citation |
|---|---|---|
| (a) `src/x86_64/` | `find … -type f` = **24** files | D3; alphaE F8/F15 |
| (b) `ext/x86/` | **4** files: `bbnf.asm`, `x86util.asm`, `x86inc.asm`, `LICENSE-VENDOR` (~3554 LOC vendored x264/FFmpeg ASM) | CH5 V3 §C.5; alphaE F15 |
| (c) `build.rs` | present, **102** LOC (nasm-rs x86-assembler driver; `:38-40` non-`x86_64` early-return) | alphaE F15 |
| (d) `src/lib.rs:247` | "Contract documented in ext/x86/bbnf.asm" doc reference | alphaE F15 |
| (e) `Cargo.toml:19` | `nasm-rs = "0.3"` ACTIVE build-dep (+`:14-16` companion comments) | CH6 V4 §1 / V5 R-1 |
| (f) `src/lib.rs:5` | `pub mod x86_64;` + `#[cfg(target_arch="x86_64")]` dispatch arms `:285-288` | CH6 V4 §1 / V5 R-1 |
| (g) doc surfaces | `CONCRETIZATION-REPORT.md`, `CHECKASM-REPORT.md` x86 narrative | CH6 V4 §1 |
| **(h)** `tests/checkasm_parity.rs` | **11** `x86_64` tokens; **9 ACTIVE compile-coupled** `bbnf_simd::x86_64::…::*_scalar(…)` call sites at **`:458,:464,:467,:477,:478,:484,:493,:497,:502`** (verified verbatim this pass) + the `#[ignore]` x86 parity harness | V5 R-2/CH5 §F.6 — **build-soundness coupling** |
| (i) `src/scalar/byte_class_from_eq_set_64.rs` | residual x86 doc strings `:10,:12,:15` ("AVX-512 BW"/"AVX2") | V5 R-2/CH5 §F.6 |

Crate-wide grep today: `grep -riE --include='*.rs' --include='Cargo.toml'
'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` = **186 lines** (must collapse to the
aarch64-neutral-comment floor).

**Close-gate (crate-wide, NOT `src/`-scoped — `ext/x86/` is a sibling of `src/`,
`build.rs`+`Cargo.toml` at crate root):** `find …/src/x86_64 …/ext/x86 -type f` = 0;
`grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm'
skinny/crates/bbnf-simd/` → only aarch64-neutral comments, **every active hit on the
(a)–(i) removal list**; BUILD-SOUNDNESS: `cargo build` AND `cargo test --no-run` clean.
`x86_tree_deleted == true`. **REVISE-not-REJECT rationale (held from CH5 §C.5):** the
surface is DORMANT (`build.rs` early-returns on non-`x86_64`; `ext/x86/` has no aarch64
caller) — deleting it is pure prune, zero aarch64 risk; the close claim "x86 gone" is
simply FALSE until the deletion lands, so it must be deleted, not asserted.

### P2 — DELETE the OLD contrived warm CSS bench — `[V3 C3]`

V3 `CONSOLIDATED-AUDIT.md:52-53` (C3): `nonjson_css_l4.rs` `lightningcss_facts:528` /
warm `measure_mbps` — 16+2000 warm iters over **85–357-byte SHA256-pinned
micro-fixtures** (not the real corpus), and the timed lightningcss does MORE work
(parse + SHA256 + a second cssparser re-parse). **It did NOT produce the headline
numbers** (those came from `css_canon_bench`) but it is a live contrivance + confusion
hazard hitting THREE of the six addenda at once (timed-plane-asymmetry +
corpus-out-of-the-timer + more-work-competitor).

**LIVE witness:** `grep -c 'measure_mbps\|lightningcss_facts'
crates/bbnf-bench/src/nonjson_css_l4.rs` = **48** (present). alphaE CANDIDATE A P2 names
`:528`, `measure_mbps:3091`, the per-fixture SHA256/byte scaffolding, and
`benches/nonjson_css_l4.rs`. **KEEP** `css_canon_bench.rs` + `w2_rich_cssom_bench.rs`
(the honest harness) + the 9-field `assert_rich_strict_equality` oracle
(`nonjson_css_l4.rs:451`, the ONE honest artefact extracted before the file is gutted).
**Close-gate:** `grep -n 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs` = 0;
`css_canon_bench.rs` present + green; `corpus_in_timer == true`.

### P3 — COLLAPSE the 7 byte-identical CSS replicas — `[V3 D1 + F13/F16]`

V3 `CONSOLIDATED-AUDIT.md:33` (D1): the 7 `css_l4_*/generated.rs` are **byte-identical**
— ONE CSS parser replicated 7×, materially overstating "7 grammars admitted."

**LIVE witness:** all 7 share md5 **`b654562ccff46ed62dd48e9ace325830`** (verified this
pass across all 7 dirs); each is **910 LOC** (disk-measured, not estimated — the CSS body
span `runtime_generator.rs:701`→`:1611` per R1-CH1; the V3-seed "646–910" range is the
seed's pre-measurement estimate, superseded), so the collapse deletes **6 × 910 = −5460**.
Collapse to ONE CSS `generated.rs` OR N **non-identical**
generated files each from a distinct `.bbnf` (the distinct-grammar-output addendum).
**This PRUNE item carries the relocated-overfit-seam structural gate** (§4 below): the 7
xtask `RuntimeTarget` css_l4 rows must collapse to ONE config-tuple per `grammar_name`
modulo the two generated-artefact path columns. **Close-gate:** one CSS `generated.rs`
remains OR md5-distinct census passes; `runtime_target_rows_collapsed == true`
(F13→F16); `generated_md5_distinct == true`.

### P4 — make the Lock-14 gate MEANINGFUL — `[V3 D4]`

V3 `CONSOLIDATED-AUDIT.md:41-42` (D4): `GENERIC_SCAN_ROOTS`
(`lock14_baseline.rs:2409`) deliberately OMITS `runtime_generator.rs` (the `JSON_*_RS`/
`CSS_*_RS` template consts) + routes the leak files into a weaker check that never runs
the neutrality scan; the x86 tree is tagged `"diagnostic-x86"`. **Correction to premise
(D4, carried verbatim):** the gate PASSES today (`accepts_current_allowlist` = 2/0) — **a
green gate over standing leaks is worse than a red one.**

**LIVE witness:** `lock14_baseline.rs:2409` `GENERIC_SCAN_ROOTS`; `:2442`
`SKV15_W2_EXTRA_COVERAGE_ROOTS` (the weaker check); `:2463`
`("crates/bbnf-simd/src/x86_64","diagnostic-x86")` (the exclusion entry); `:4956`
asserted. alphaE P4: move `runtime_generator.rs`, `json_sink_direct.rs`,
`json_typed_direct.rs`, `json_templates/`, `grammar_provider.rs` from `:2442` INTO strict
`:2409`; extend `FORBIDDEN_GENERIC_TOKENS:2420` with `CSS_`/`_RS` template-const
patterns; drop the `"diagnostic-x86"` entry (x86 is gone, P1). **Close-gate:**
`accepts_current_allowlist` GREEN **and meaningful** — re-introducing a `JsonSink` token
into `runtime_generator.rs` must turn it RED (proving coverage);
`lock14_gate_scans_codegen == true`. **P4 has a sequencing obligation: it MUST land
BEFORE the G1/G2/G3 emitter rebuild** (§3, constraint 1c) — so the un-forked emitter is
scanned for neutrality AS it is built, not after.

### P5 — PURGE the metalang bench-wave-id leak — `[V3 Other]`

V3 `CONSOLIDATED-AUDIT.md:58` (Other, MEDIUM-PRUNE): bench wave-id
`parse_w11_1_number` is baked into the SHIPPED
`runtime/src/grammars/json/generated.rs` — violates clean-regen discipline.

**LIVE witness:** `grep -c 'parse_w11_1_number'
crates/runtime/src/grammars/json/generated.rs` = **7** (alphaE P5 cites
`:801,841,881,955,1007,1019,1031`). Fix at the **generator/template source** (not the
shipped file) so `regen --check` stays clean. **Close-gate:** `grep -c
parse_w11_1_number …/json/generated.rs` = 0; `regen --check` clean;
`metalang_leak_present == false`.

**PRUNE net LOC:** ≈ **−10800** (P1 ≈ −4500 BOTH x86 surfaces incl `.asm`+`ext/x86/`
3554 + `build.rs` 102; P3 ≈ −5460; P2 ~−700; P4 +~15; P5 rename-only). Per
`[generated-size-budget]` this is a *reduction* — no overflow risk (alphaE CANDIDATE A
LOC budget; SUMMARY TABLE).

---

## §2 — Sequencing constraint 1: PRUNE-before-GENERALIZE-before-PROVE (the standing order)

The standing order is **PRUNE → GENERALIZE → PROVE → HONESTY** (`SYNTHESIS.md` §Section 3;
HANDOFF §Next Move; alphaE CROSS-CUTTING NOTE 1). A2 binds it as an **explicit
entry-gate dependency graph** with three load-bearing edges:

```
PRUNE(P1–P5) ──► G1 ──► G2 ──► G3 ──► G4 ──► G5/G6 ──► PROVE ──► H1
```

- **(1a) PRUNE reduces the GENERALIZE surface.** Each PRUNE item clears a surface a later
  GENERALIZE candidate must touch: P3 collapses the 7 replicas BEFORE G2 derives CSS (G2
  cannot route through grammar lowering while 7 byte-identical replicas claim to be 7
  grammars); P1 deletes the x86 tree BEFORE G5/G6 touch the SIMD dispatch (no point wiring
  NEON next to a 24-file x86 stub tree); P5 purges the metalang leak BEFORE G1 regenerates
  JSON (G1's regen --check must be clean).
- **(1b) PRUNE carries zero generalization risk** (pure deletion + gate-tightening, no
  >SOTA-bearing code removed — V3 C3 confirms the deleted bench "did NOT produce the
  headline numbers"), so it is the natural standing-order predecessor; bundling it FIRST
  is `[refactor-first-order]` discipline (prune → generalize → prove).
- **(1c) P4 MUST land BEFORE the emitter rebuild (G1/G2/G3).** This is the
  load-bearing intra-PRUNE edge (alphaE CROSS-CUTTING NOTE 1; CH6 §5): a green Lock-14
  gate must be meaningful BEFORE the un-forked emitter is built, otherwise G1/G2/G3 could
  re-leak a grammar-named branch under a blind gate. P4-before-B1 is an entry-gate, not a
  preference.
- **(1d) Exit-gate-blocks-successor (the revert clause, CH6 §5 → S-P3).** A wave that
  FAILS its exit gate BLOCKS every downstream wave that entry-gates on it — no downstream
  wave dispatches over a REDRESSed predecessor. In particular **G1 failure blocks
  G2/G3/G4/PROVE**, and **G3 (un-fork) failure blocks PROVE** (PROVE emits Sheets THROUGH
  the un-forked generator — see constraint 3). S-P3 MUST encode this as an explicit
  exit-gate-blocks-successor clause in the revert protocol (sanctioned-deferred to S-P3
  per PASS-ALPHA §4.4, but the dependency graph itself is binding HERE).

**A2 disposition:** the PRUNE-before-GENERALIZE-before-PROVE order is binding; the three
edges (1a/1c/1d) are load-bearing and each carries a concrete re-leak or stale-state
hazard if reordered.

---

## §3 — Sequencing constraint 2: build-soundness coupling P1 ↔ `checkasm_parity.rs`

This is the **build-soundness coupling** — the reason P1 is not a one-line `rm -rf`. The
verify grep that gates P1 fires on `tests/checkasm_parity.rs`, and that file has **9
ACTIVE compile-coupled** `bbnf_simd::x86_64::…::*_scalar(…)` call sites (verified verbatim
this pass at `:458,:464,:467,:477,:478,:484,:493,:497,:502`). These are NOT dead doc
references — they are real Rust expressions that resolve into `src/x86_64/`:

```
:458  bbnf_simd::x86_64::avx2::classify::classify_block_scalar(&block32, STRUCTURAL_SET);
:464  bbnf_simd::x86_64::avx2::bmi2_emit::compact_mask_scalar(0, 0b1010_1010, &mut out);
:467  bbnf_simd::x86_64::avx2::prefix_xor::prefix_xor_scalar(0b0001_0010, false);
:477  bbnf_simd::x86_64::avx512_vbmi2::classify::classify_block_scalar(&block64, …);
:478  bbnf_simd::x86_64::avx512_gfni::classify_affine::classify_block_scalar(…);
:484  bbnf_simd::x86_64::avx512_bitalg::multiclass::classify_full_scalar(…);
:493  bbnf_simd::x86_64::avx512_vbmi2::mask_fuse::fuse_emit_scalar(0xFF, 0x0F, 0xF0);
:497  bbnf_simd::x86_64::avx_ifma::mantissa::mul52_low_scalar(0x1_0000_0000_0000, 10);
:502  bbnf_simd::x86_64::avx512_vnni::digit_mac::parse_8_digits_scalar(b"12345678");
```

**The coupling:** deleting `src/x86_64/` (P1 sub-target (a)) WITHOUT decoupling these 9
sites **BREAKS THE BUILD** — the test crate fails to compile against the deleted
`bbnf_simd::x86_64::…` paths. Therefore P1 is NOT complete at "delete the directory"; the
close-gate is **`cargo build` AND `cargo test --no-run` clean**, and the only way to reach
it is to **DECOUPLE-OR-DELETE** the `checkasm_parity.rs` x86_64 reference block (+ the
`#[ignore]` x86 parity harness), **retaining the aarch64 parity assertions** in the file
(V5 R-2/CH5 §F.6). This is the mirror of the V5 R-1 finding (`Cargo.toml`/`lib.rs` reach):
a deletion list narrower than the grep it is gated by ships a RED-by-construction gate —
the deletion list and the verify grep MUST be reach-matched. Sub-target (i)
(`byte_class_from_eq_set_64.rs:10,12,15` doc strings) is the same class (grep-firing but
doc-only — clean to aarch64-neutral).

**A2 disposition:** the P1 ↔ `checkasm_parity.rs` coupling is real and BUILD-BLOCKING.
P1's exit gate is `cargo test --no-run` clean, and the `checkasm_parity.rs` decoupling is
the *only* thing that makes the `src/x86_64/` deletion build-sound. S-P3 MUST sequence
the decoupling in the SAME commit/wave as the `src/x86_64/` deletion (not a follow-on) —
an intermediate commit with `src/x86_64/` deleted and `checkasm_parity.rs` un-decoupled
is a broken-build state. The 12 aarch64 single-kernel differential harnesses +
`checkasm_common.rs` are preserved untouched (the scalar-ref-as-spec discipline is KEPT;
checkasm count = **14** = 12 single-kernel + 2 aggregate, NOT 18 — alphaE is the
count-correct reference per CH7 §5).

---

## §4 — Sequencing constraint 3: G1/G3 co-derive; G3-failure blocks PROVE (one emitter, not two forks)

**Directional note (R1-CH3 fold):** the binding revert/precondition arrow is
**G3-un-fork-FAILURE → blocks PROVE** (forward), NOT a backward "G3 gates G1/G2." The
dependency graph orders G3 *after* G1/G2 (PRUNE → G1 → G2 → G3 → …), and **G1+G3 co-derive**
the unified emitter (alphaE bundles them in CANDIDATE B1). The earlier title "G3 un-fork
gating G1/G2" loosely inverted the arrow; the load-bearing facts below are unchanged — un-fork
is *meaningless* unless ≥1 grammar genuinely projects (so G1 projects FIRST, co-deriving the
unified emitter G3 retires the fork into), and a surviving fork makes PROVE structurally false.

The "grammar-driven generator" **does not exist** — it is two forked hand-written parsers
(V3 D1). The fork is `RuntimeEmitterKind = {CompiledLowering(JSON), RequestFacts(CSS)}` —
verified LIVE at `grammar_provider.rs:40-42` (`:33` `pub emitter: RuntimeEmitterKind`,
`:40` `pub enum`, `:41` `CompiledLowering`, `:42` `RequestFacts`, dispatched `:110`). G3
RETIRES this fork into ONE grammar-agnostic emitter.

**The gating relation (alphaE CANDIDATE B1, the structural core):**

- **G3 un-fork is meaningless unless ≥1 grammar genuinely PROJECTS.** alphaE bundles
  **G1+G3 in CANDIDATE B1** precisely because un-forking an emitter that still emits
  verbatim const-`&str` blobs for both grammars is a relabel, not a generalization. JSON
  is the closer of the two (its spine is already grammar-shaped — V3 A1/AGENT-3 §5), so it
  projects FIRST and proves the unified emitter works before CSS rides it.
- **G1 (JSON projection) must precede G2 (CSS lowering).** G2 entry-gates on B1 closed
  (the unified emitter exists) AND P3 closed (replicas collapsed) — alphaE B2 entry-gate.
  CSS rides the same lowering path JSON established; deriving CSS through a path no
  grammar has yet validated is building on sand.
- **G3 failure blocks PROVE.** PROVE emits Sheets THROUGH the un-forked (G3) generator —
  if the fork survives, "one generator emits three grammars" is structurally false. This
  is edge (1d) of the dependency graph, made specific: **G3 un-fork failure blocks
  PROVE**.
- **The un-fork close-gate is the FULL Lock-14 three-surface model**, not just "the enum
  is gone." Verified the binding `SYNTHESIS.md:333` (G3 row) carries all three surfaces:
  (i) the arm census `rg 'match…{…Json=>|CssL4=>|(GoogleSheets|Sheets)…=>|Bbnf…=>'` over
  codegen AND xtask → 0 (`generator_grammar_branch_count == 0`); (ii) the grammar-named-
  *type* census `rg 'JsonParser|CssL4Parser|GoogleSheetsParser|BbnfBootstrap'` → 0
  (`generator_grammar_type_count == 0`); (iii) the STRUCTURAL relocated-seam check
  (`runtime_target_rows_collapsed`, §4a below). md5-distinctness is
  **necessary-not-sufficient** — a neutral md5-distinct output can still come from a
  grammar-branching body (caught by (i)/(ii)) or a relocated metadata data-table (caught
  by (iii)).

### §4a — The relocated-overfit-seam (the one residual-overfit risk surviving into GENERALIZE)

This is the residual contrivance the S-P0 mandate exists to catch: a per-grammar branch
RELOCATED out of a `match grammar` arm into a neutral-identifier `RuntimeTarget`
data-table. The arm-census regex is **syntactically incapable** of detecting it (a
neutral-identifier table carries no `Json =>` arm syntax — disk-confirmed: arm-census over
`xtask/src` = 0 matches against the live `grammar_name: "css_l4"` DATA table, CH2 V3
§8.1). The lineage (necessary-not-sufficient, carried one level deeper each cycle):

- **V1:** md5-distinctness → necessary-not-sufficient (a const courier can be md5-distinct).
- **V2 (F10):** grep-alphabet → canonical four-grammar Lock-14 alphabet
  `Json|CssL4|(GoogleSheets|Sheets)|Bbnf`, `GoogleSheets` un-abbreviated (`Sheets\w*` does
  NOT match `GoogleSheets =>`).
- **V3 (F13):** grep-cannot-fire-on-data-table → the defense MOVES from the regex to the
  **P3 collapse structural row-count check**.
- **V4→V5 (F16):** the V3-prescribed projection `(source_roots, entry_rule)` is ITSELF too
  narrow → the projection WIDENS to the full per-`grammar_name` config-tuple modulo the two
  generated-artefact path columns.

**LIVE disk truth (this pass, `skinny/xtask/src/regen.rs:6-19`):** `RuntimeTarget` is
exactly **12 fields** (corroborating the binding contract's "12-field" — the confirm
wave's "13-field" slip is folded out at HEAD `83b66db42`). The two excluded path columns
are `output_dir` (`:11`) and `expected_files` (`:16`); the **operative non-path set is the
other 10**: `grammar_name`/`profile`/`entry_rule`/`source_roots`/`check_command`/
`source_inputs`/`metadata_inputs`/`emitter`/`frontend_requirements`/`output_labels`.
Critically, `fact_schema`/`row_id`/`output_plane` are **NOT struct fields** — they are
per-profile content the `profile` discriminator selects, exactly as the F16 by-exclusion
statement asserts. The 7 css_l4 rows share `(source_roots, entry_rule)` =
`CSS_L4_ROOTS`/`"stylesheet"` but carry **7 distinct `profile`** + per-profile
`source_inputs`/`metadata_inputs` — so the `(source_roots, entry_rule)`-only projection is
GREEN (false-green) while the full-config-tuple collapse is correctly **RED pre-P3**.

**The P3 ↔ G3 coupling (binds P3 to the un-fork):** the P3 collapse close-gate
(`count(distinct config-tuple-minus-(output_dir,expected_files)) == 1` per `grammar_name`,
a tiny xtask assertion) is the STRUCTURAL machine-check for the relocated seam the arm
census cannot do — and it only goes GREEN after the 7 profiles genuinely collapse to one
CSS config. **P3 must PRESERVE profile-distinctness where the 7 profiles are genuinely
distinct grammars** (collapse to one config ONLY when they are truly one grammar;
otherwise differentiate by distinct `.bbnf` roots — `color.bbnf`/`media.bbnf`/
`selectors.bbnf` exist — never erase the `profile` discriminator). This is why P3 is not a
pure "delete 6 of 7 files" — the *which* (collapse-vs-differentiate) is decided in B2.

**The config-tuple must be the FULLY-EXPANDED row (R1-CH5 sharpening — both nested structs,
not one).** `RuntimeTarget` carries **two** nested-struct fields, BOTH inside the operative
non-path set: `frontend_requirements: codegen::RuntimeFrontendRequirements` (`regen.rs:17`,
struct `grammar_provider.rs:46`) AND `output_labels: Option<codegen::RuntimeOutputLabels>`
(`regen.rs:18`, struct `grammar_provider.rs:92`, carrying the 7-distinct nested
`fact_schema`/`row_id`/`output_plane`). The collapse-check must inline **EVERY** nested-struct
field — comparing `output_labels` deeply but `frontend_requirements` shallowly (or by `Option`
discriminant) is the EXACT shallow-compare false-green a3's R16 names, displaced one field over;
a future relocated seam riding `frontend_requirements` would slip a one-nested-struct recipe
(today `frontend_requirements == REQUEST_FACTS_REQUIREMENTS` across all 7 css_l4 rows,
`regen_css.rs:47…155`, so it is not yet a LIVE divergence vector — but the gate's PURPOSE is to
forbid a future seam). The **cleanest sufficient mechanism is a full-row `RuntimeTarget:
PartialEq`** collapse: it covers both nested structs automatically and cannot be coupled to a
hand-rolled field list. This costs **one derive line** — disk: `regen.rs:5` carries only
`#[derive(Clone, Copy, Debug)]` (NOT `PartialEq`), while both nested structs already derive
`#[derive(…, PartialEq, Eq)]` and the `&'static [&'static str]` slice fields support `PartialEq`
— so adding `PartialEq` to `RuntimeTarget` is viable and is the preferred realization over a
prose-field comparison. (Serialize-then-hash or `jq`-over-a-dumped-table are equally sufficient;
the forbidden form is any compare that recurses into only ONE of the two nested structs.)

**A2 disposition:** G1/G3 co-derive the unified emitter; G3-un-fork-FAILURE blocks PROVE (the
binding forward arrow, NOT a backward "gates G1/G2"); the un-fork
close-gate is the full three-surface Lock-14 model; the relocated-overfit-seam is the one
residual-overfit risk and is policed structurally by the P3 collapse check (F16
projection, by-exclusion over the 12-field struct, inlining BOTH nested structs — preferably
via `RuntimeTarget: PartialEq`), NOT by the regex. This coupling
**binds P3 (a PRUNE item) to G3 (a GENERALIZE item)** — they are not independent.

---

## §5 — Sequencing constraint 4: G4 phantom-generic DELETE-default + JSON-richness guard

V3 D2 (`CONSOLIDATED-AUDIT.md:35-36`, HIGH): `ValueRef<G: EventGrammar>` is a PHANTOM
generic — never instantiated with a real grammar in production; the only instantiations
are test-only `_proof_compiles::<JsonEventGrammar>`/`::<SheetsEventGrammar>`/`::<AnyGrammar>`.

**LIVE disk truth (`tape/mod.rs:175`, verified this pass):**
```
pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar> { … }
```
This corroborates the contract's two-axis distinction precisely:

- **`K = AnyKind` is the ALREADY-REAL axis** (instantiated with real `Kind` types) — NOT
  the phantom. G4 must NOT touch it.
- **`G: EventGrammar = AnyGrammar` is the PHANTOM axis** — `G` defaults to `AnyGrammar`
  and is never bound to a production grammar type. THIS is the G4 target.

**The two binding sub-constraints (alphaE B3, folds F6/F7/F9):**

- **(4a) DELETE is the abrogate-before-patch DEFAULT** (`[abrogate-before-patch]`; F6).
  `CssEventGrammar` does NOT exist at HEAD (only the test-only `JsonEventGrammar` +
  `SheetsEventGrammar` witnesses) — so "INSTANTIATE" entails AUTHORING a new grammar-named
  type, a new coupling surface un-budgeted in the "±0 LOC" estimate. The default is
  DELETE the `<G>` parameter; INSTANTIATE is the **burden-of-proof** branch, not a
  symmetric alternative. The shared `Value`/`Document`/`Cursor` trait's existence is
  **INDEPENDENT** of the `<G>` phantom — deleting `<G>` and defining the trait are
  separable; do NOT couple the trait's shape to animating `<G>` (that would manufacture
  the very phantom being deleted). The phantom-resolution grep is **test-excluded** (F6):
  the DELETE branch is `grep -c 'G: EventGrammar' tape/mod.rs` = 0; the INSTANTIATE branch
  requires `grep -rn 'ValueRef<.*EventGrammar>' --include='*.rs' crates/runtime/src | grep
  -v 'tests\.rs\|#\[cfg(test)\]'` ≥1 — the standing test-only `JsonEventGrammar` line MUST
  NOT satisfy the gate.

- **(4b) The JSON-richness guard (preserve-rich-ast; F7).** The shared trait must NOT be a
  lowest-common-denominator collapse that flattens JSON's richness. The value API is
  genuinely DIVERGENT today: JSON = recursive document tree (`get(key)` + typed-`Kind` +
  `JsonVisitor` + `DocumentView`); CSS = flat rule/decl/typed-token stream (`CssTypedNode`,
  no visitor, not `DocumentView`). A both-impl grep is **necessary-not-sufficient** — JSON's
  `get(key)` + typed-`Kind` accessors + visitor must remain reachable **THROUGH** the
  shared trait (not via a bypass), so that a ≥2 impl-count cannot LCD-flatten JSON's
  navigation. This is `json_rich_navigation_preserved == true`, a SEPARATE checked
  condition from `shared_value_trait_instantiations >= 2`. A thin LCD trait that flattens
  JSON's richness is a `[preserve-rich-ast]` regression and is REJECTed **even at ≥2
  impls** (`SYNTHESIS.md:334` G4 row; telemetry `:570`).

- **(4c) No second substrate (Lock 1).** The shared trait + the un-forked emitter emit
  accessors over the EXISTING `Tape`/`ValueRef` — an introduced
  `StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the landed `Tape`/`ValueRef` is
  a Lock 1 type-ambivalence violation (REJECT). The trait stays LAZY over the tape — it
  does NOT re-introduce an eager value tree (the AZ-IV 118× pre-block).

**G4 entry-gate:** B1 + B2 closed (both grammars derive; now unify their value surface).
The shared-trait both-impl grep is test-excluded (F9), mirroring the F6 phantom-grep
exclusion on the trait-impl axis: a `#[cfg(test)] impl SharedValueTrait for CssTestNode`
must NOT false-green the ≥2 gate.

**A2 disposition:** G4's phantom target is the `G` axis (NOT `K`), DELETE is the default,
the trait is independent of the `<G>` phantom, and the JSON-richness guard
(`json_rich_navigation_preserved`) is a separate checked condition that a ≥2 impl-count
does NOT satisfy. Disk truth (`tape/mod.rs:175`) corroborates the two-axis distinction
exactly.

---

## §6 — Propagation-residual check (the V5/confirm REVISE mass, re-verified at HEAD `83b66db42`)

The hardening `CONSOLIDATED-CONVERGED.md` recorded the confirm wave at 88.9% with 10
REVISEs — all "propagation residual": the F.6 x86-deletion-list reach (h)+(i) and the F16
by-exclusion projection landed in the αC/αE feeders but several lenses (CH3/CH5/CH6) found
them NOT yet carried verbatim into the binding `SYNTHESIS.md:326`/`:576` +
`HANDOFF.md:101-112` rows (plus a "13-field"/12-field struct-count slip). **A2
re-verifies this is now FOLDED at HEAD `83b66db42`:**

- `SYNTHESIS.md` references `checkasm_parity.rs` **4×** (the (h) build-soundness coupling
  is in the binding P1 row `:326` + telemetry `:576`).
- `SYNTHESIS.md` carries "enumerate-by-exclusion"/"by EXCLUSION" **6×** (the F16 projection
  is in the G3 row `:333`, §0.4 `:410`, telemetry `:566`, gate consumer `:608`).
- `SYNTHESIS.md` says "12-field struct" (the struct-count slip is corrected; disk =
  `regen.rs:6-19` = 12 fields, confirmed).
- 16-lock count = **16** (`grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md`).

**A2 disposition:** the confirm-wave propagation residual is DISCHARGED in the binding
contract at HEAD `83b66db42` — the PRUNE-list and sequencing constraints A2 cites are
disk-true and binding-row-complete. The §3Z formal non-convergence (97.9% V5 + 88.9%
confirm ≠ 2-consecutive ≥95%) is a *measurement-of-convergence* gap, NOT a
contract-soundness gap: zero REJECT all cycles, zero orphan architectural defect, zero
stranded >SOTA, zero re-opened pre-block. The contract spine, goalset, and six addenda are
structurally sound and disk-true; S-P0 proceeds on a sound surface.

---

## §7 — The 6 addenda as binding S-P0 PRUNE/sequencing lenses (A2 fold)

A2's PRUNE-list + sequencing constraints bind each of the 6 addenda to a specific
PRUNE/sequence obligation (the addenda are the load-bearing S-P0 output; A2 maps them onto
the prune-then-generalize spine):

| addendum | PRUNE/sequence binding (A2) |
|---|---|
| **verbatim-blob** | G2 retires `CSS_GENERATED_RS` (the verbatim `const &str`, `runtime_generator.rs:701`); G1 retires `json_sink_direct::render`'s fixed string literals — neither may replace one const-string courier with another. `verbatim_blob_present == false`. Sequencing: G2 entry-gates on B1 (the projecting renderer must exist first). |
| **distinct-grammar-output** | **P3** collapses the 7 byte-identical replicas (md5 `b654562c…`); PROVE requires Sheets `generated.rs` md5-distinct from JSON+CSS. `generated_md5_distinct == true`. md5-distinctness is necessary-not-sufficient — co-gated by the neutral-body greps (G3 surfaces (i)/(ii)) + the P3 config-tuple collapse (iii). |
| **single-emitter-path** | **G3** retires `RuntimeEmitterKind::{CompiledLowering,RequestFacts}` (`grammar_provider.rs:40-42`). `emitter_fork_present == false`. Sequencing: G1/G3 co-derive the unified emitter (§4) and G3-un-fork FAILURE blocks PROVE (§2 edge 1d). |
| **phantom-generic** | **G4** resolves the `G: EventGrammar` axis (`tape/mod.rs:175`), DELETE default (§5). `phantom_generic_resolved ∈ {instantiated,deleted}`; the JSON-richness guard `json_rich_navigation_preserved == true` prevents a ≥2 impl-count LCD-flatten. |
| **timed-plane-symmetry + corpus-in-the-timer** | **P2** deletes the OLD warm micro-fixture path (`nonjson_css_l4.rs measure_mbps`); H1 frames the rest (lazy-rich-summary vs eager-full-CSSOM); `css_canon_bench` is the honest harness KEPT. `corpus_in_timer == true`. |
| **acceleration-wiring** | **G6** wires the CSS NEON into the hot path AT ADMISSION (today `find_css_significant`/`find_comment_close` are `#[cfg(test)]`-only, C1) OR honestly retires it on a samply non-top-N MEASUREMENT (not an assertion). `acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}` — `cfg-test-only` is NO-GO. Sequencing: G6 is in the PROVE/HONESTY cluster (B4), entry-gated on B1+B2+B3. |

---

## §8 — A2 summary (the load-bearing output)

1. **PRUNE-list (P1–P5) is disk-true and ACCEPT.** Every item anchored to a LIVE witness
   at HEAD `83b66db42`: P1 (24 + 4 files + 102-LOC build.rs + nasm dep + 9 checkasm
   sites), P2 (48 grep hits), P3 (md5 `b654562c…` ×7), P4 (`:2409`/`:2442`/`:2463`/`:4956`),
   P5 (×7 `parse_w11_1_number`). Pure deletion + gate-tightening, net ≈ −10800 LOC, zero
   >SOTA-bearing code removed.
2. **The four sequencing constraints are binding and load-bearing:**
   - **PRUNE→GENERALIZE→PROVE** with edges 1a (P3-before-G2, P1-before-G5/G6,
     P5-before-G1), **1c (P4-before-emitter-rebuild)**, 1d (exit-gate-blocks-successor:
     G1→G2/G3/G4/PROVE, G3→PROVE).
   - **P1 ↔ `checkasm_parity.rs` build-soundness coupling:** the `src/x86_64/` deletion is
     build-BLOCKING without decoupling 9 active call sites (`:458…:502`); P1's exit gate is
     `cargo test --no-run` clean; decouple in the SAME wave (no intermediate broken-build
     commit).
   - **G1/G3 co-derive the unified emitter; G3-un-fork FAILURE blocks PROVE** (un-fork is
     meaningless without ≥1 projecting grammar; JSON
     projects first; the binding arrow is forward G3-failure→PROVE, never backward onto G1/G2);
     the un-fork close-gate is the full Lock-14
     three-surface model; the relocated-overfit-seam (the one residual-overfit risk) is
     policed by the **P3 collapse structural check** (F16 by-exclusion over the 12-field
     struct), binding P3 (PRUNE) to G3 (GENERALIZE).
   - **G4 phantom DELETE-default** on the `G` axis (NOT `K`) + the **JSON-richness guard**
     (`json_rich_navigation_preserved`, a separate checked condition a ≥2 impl-count does
     NOT satisfy) + no-second-substrate (Lock 1).
3. **The relocated-overfit-seam is the only residual contrivance surviving into
   GENERALIZE,** and it is structurally policed (P3 collapse, F16 projection), not regex
   policed. Disk truth (`regen.rs:6-19` 12 fields; `fact_schema`/`row_id`/`output_plane`
   not struct fields) corroborates the F16 by-exclusion statement exactly. The collapse-check
   must inline BOTH nested-struct fields (`frontend_requirements` `regen.rs:17` AND
   `output_labels` `regen.rs:18`) — the cleanest sufficient mechanism is a full-row
   `RuntimeTarget: PartialEq` (one derive line; `regen.rs:5` carries Clone/Copy/Debug only,
   both nested structs already derive `PartialEq, Eq`) — R1-CH5 fold.
4. **Propagation residuals from the confirm wave are FOLDED at HEAD `83b66db42`** (the (h)
   build-soundness coupling + F16 by-exclusion + 12-field count are all in the binding
   `SYNTHESIS.md` rows) — S-P0 proceeds on a sound, disk-true surface.
5. **V2 fold (the V1 `hardening/V1/CH{1..7}.md` REVISE mass): RESOLVED; V3 fold (the
   V2 `hardening/V2/CH{1..7}.md` lenses): NOTHING TO RESOLVE — all 7 V2 lenses 100% ACCEPT,
   zero REVISE / zero REJECT against a2.** The one a2-direct V1 REVISE (R1-CH3, the §4
   inverted-arrow title) is restated to "G1/G3 co-derive; G3-failure blocks PROVE" throughout
   (§0 item 3, §4 title + framing, §7, §8); the a2-adjacent R1-CH5 (both-nested-struct recipe) is
   SHARPENED into §4a; both are re-confirmed discharged by every V2 AND V3 lens. The remaining V1
   REVISEs (R1-CH1/R1-CH2/R1-CH4/R1-CH6/R2-CH6) bind a0/a1/a3/SYNTHESIS, not a2 (see V2 FOLD LEDGER
   + V3 FOLD CONFIRM above). The lone non-blocking V2 item (CH7 "One accuracy nit") is a
   sub-REVISE a3 field-numbering cosmetic, NOT an a2 defect. CH7 = 100% every cycle; zero REJECT
   against a2 in any cycle. **Convergence: V2 (100% ×7) + V3 (100% ×7) = ≥95% ×2 consecutive,
   zero orphan REVISE, V=3 — §3Z met.**
