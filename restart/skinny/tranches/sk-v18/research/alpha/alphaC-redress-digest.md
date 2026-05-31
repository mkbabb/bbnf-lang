# αC — REDRESS Digest (cycle V5): the SK-V18 PRUNE waves + the pre-blocks the GENERALIZATION must NOT re-open

Agent: alphaC (Pass Alpha SK-V17→SK-V18, **cycle V5** — the GENERALIZATION cycle). Host: aarch64 Apple
M5 Max ONLY (x86 OUT). Cold benches only. HEAD of record: `318d9c046` (SK-V17 closed at `f6a38445b`;
V3 audit + alpha-A/B/C/D/E + V1/V2/V3/V4 CHALLENGE committed since; all live facts re-verified at this HEAD
this V5 cycle). All re-open tests grep the **skinny benched tree** (`skinny/crates/…`, `skinny/xtask/…`,
`skinny/crates/bbnf-bench/…`); a gate keyed to `crates/core/…` is a CH1 defect (it is satisfiable in the
un-benched totality tree while the benched surface is untouched).

## §0 — What this digest does + the V4→V5 FOLD

SK-V18's subject is the **inflection backtrack**: retire the hand-written/forked parsers into ONE
grammar-driven generator emitting JSON + CSS + a third grammar (Sheets) from `.bbnf`, over the
already-unified tape/`ValueRef` substrate, with a shared value API — **preserving the >SOTA**. It is a
PRUNE-then-GENERALIZE cycle, not a new-feature cycle.

This digest has two halves, both load-bearing for the SK-V18 Pass-Alpha goalset:

- **§1 — the SK-V18 PRUNE waves (P1–P5).** The V3 audit's prune list, each verified live at HEAD
  `318d9c046`, framed as a PRUNE wave with its owner paths, its delete-or-fix obligation, and the
  same-wave/close gate that proves the prune landed. PRUNE runs FIRST (the campaign's standing order:
  "PRUNE first, then GENERALIZE").
- **§2 — the pre-block REDRESS families (the six the generalization must not re-open):** **AZ-IV
  eager**, **StructRegistry per-leaf**, **fact-stream-as-output**, **24-broadcast**, **FNV-in-runtime**,
  **x86/AVX/SVE**. Each carries a classification (PERMANENT vs ADMIT-UNDER-DIFFERENT-FRAMING) and the
  **re-open test keyed to the SK-V18 rebuild** (the grammar-driven generator + shared value-API trait +
  phantom-`<G>` instantiation are the new surfaces that could silently re-land a refuted carrier).

The single load-bearing distinction every SK-V18 wave holds is restated at §3.

### §0.A — V5 FOLD (the CHALLENGE-V4 dispositions resolved into this revision)

The V4 CHALLENGE wave (`research/alpha-hardening/V4/{CH1..CH7}.md`) dispositioned αC **ACCEPT on every
section across all seven lenses, with ONE cross-artefact §8 REVISE (CH2 §8.1) that names αC's
relocated-seam machine-check framing at `αC:399`.** Explicitly: CH1 §"αC REDRESS Digest — **ACCEPT**"
(`V4/CH1.md:133,352`); CH2 §3 "**αC tally: ACCEPT ×4, REVISE ×0**" — the αC *mechanism* ACCEPTs; the
projection-tuple correction is filed once at CH2 §8.1 as the cross-artefact REVISE to avoid
double-counting (`V4/CH2.md:111-144,294-360,421`); CH3 §"alphaC-redress-digest.md — **ACCEPT** (the
load-bearing CH3 artefact, V4-correct)" — "the artefact that **correctly authored FOLD-1**," FOLD-1 +
FOLD-2 both landed, "Zero orphan REVISE entering V4 on αC" (`V4/CH3.md:201-202,239-249`); CH4
§"**Disposition: ACCEPT**" (`V4/CH4.md:142`); CH5 §3 "**αC tally: ACCEPT ×4, REVISE ×0, REJECT ×0**"
(`V4/CH5.md:101-143`); CH6 §9 "alphaC PRUNE-wave close gates — **ACCEPT**" — the P1
x86-surface-enumeration question is explicitly NOT raised as a separate αC REVISE because αC already
handled it via FOLD-1 (`V4/CH6.md:79,287-300`); CH7 "**alphaC overall: ACCEPT (all sections)**" — "§6
FOLD-1 is the gold-standard treatment … alphaA must match THIS," "No REVISE/REJECT on alphaC"
(`V4/CH7.md:167-171,319`). **No V4 CONSOLIDATED was produced on disk** (`V4/CONSOLIDATED.md` is absent —
the V4 wave returned CH1..CH7 only; this V5 cycle is the confirming/fold cycle that resolves the V4
dispositions directly).

The V4 wave issued **exactly ONE REVISE that touches αC** (CH2 §8.1, a cross-artefact projection-tuple
correction shared by SYNTHESIS / HANDOFF / αE / αC) and **zero REJECT**. It is orphan-free (it carries a
concrete three-step fix). This V5 revision folds it so it cannot recur:

1. **FOLD-3 — CH2 §8.1: the `runtime_target_rows_collapsed` machine-check projects onto the TWO INVARIANT
   columns and the "N collapsed-identical rows" framing at `αC:399` is empirically false (the sole V4
   REVISE touching αC; CH2 `V4/CH2.md:294-360,421`; ledger row `V4/CH2.md:421`).** The V4 αC §2.2 / §3
   relocated-seam corollary (FOLD-2 from V3 CH2 §8.1) correctly moved the relocated-overfit-seam defense
   from the syntactically-incapable arm-census grep TO the **P3 structural row-count collapse** — the
   right MECHANISM. But it framed the structural check as a `sort -u` over `(source_roots, entry_rule)`
   and the threat as "a data-table that carries **N collapsed-identical** rows for one `.bbnf`." CH2's
   fresh V4 sweep, **reproduced live at HEAD `318d9c046` this V5 cycle**, refutes the *projection tuple*
   (not the mechanism):
   - The `RuntimeTarget` struct (`skinny/xtask/src/regen.rs:6-19`) carries **12 fields**
     (`grammar_name`, `profile`, `entry_rule`, `source_roots`, `output_dir`, `check_command`,
     `source_inputs`, `metadata_inputs`, `emitter`, `expected_files`, `frontend_requirements`,
     `output_labels`); the V4 αC:399 projection uses **2 of them**.
   - The 7 live css_l4 rows are byte-identical on the projected pair — live
     `grep -E 'source_roots:|entry_rule:' regen_css.rs | paste - - | sort -u | wc -l` = **1**
     (`CSS_L4_ROOTS` / `"stylesheet"`), and `grep -E 'grammar_name:' … | sort -u | wc -l` = **1** — so
     `runtime_target_rows_collapsed == true` reports GREEN.
   - But the SAME 7 rows carry **7 distinct** values in EACH of `fact_schema`, `output_plane`, `row_id`,
     and `output_dir` — live `grep -E 'fact_schema:' … | sort -u | wc -l` = **7**, `output_plane` = **7**,
     `row_id` = **7**, `output_dir` count = **7**. The gate projects onto exactly the 2 columns that are
     invariant across the 7 rows and discards the 5 where the per-profile divergence demonstrably lives.

   **Why this is a generality hole, not cosmetics:** the relocated-overfit-seam threat is "a per-grammar
   branch moved into a neutral-identifier strategy DATA table." A relocated branch does NOT need to vary
   `(source_roots, entry_rule)` to encode per-grammar routing — it can ride `fact_schema` / `output_plane`
   / `emitter` (all per-row in the live table). An un-forked emitter that internally dispatches on
   `target.fact_schema` (or `target.output_plane`) to select a different generated body per CSS profile is
   EXACTLY the relocated seam — and it sails through `runtime_target_rows_collapsed == true` because those
   7 distinct values are invisible to a `sort -u` over `(source_roots, entry_rule)`. The αC:399 framing
   "N **collapsed-identical** rows" is the giveaway: the live rows are NOT collapsed-identical; they
   differ in 5 columns. The gate reports collapse=1 precisely BECAUSE it discards the columns the
   divergence is in. This is the SAME necessary-not-sufficient lineage carried one level deeper: V1 (md5)
   → V2 (grep alphabet) → V3 (grep cannot fire on a data-table → move to the structural row-count check)
   → V4/V5 (the structural check picked the right MECHANISM but the wrong PROJECTION).

   **Why REVISE not REJECT (held from CH2 §8.1):** the Lock-14 spine is correct, the P3 collapse
   mechanism is the right defense, and the threat IS policeable by it — only the projected column set is
   too narrow. The fix is one struct-field-set away; the structural collapse stays the answer.

   **FOLDED into §1-P3 + §1-P4 + §2.2 + §3:** the `runtime_target_rows_collapsed` machine-check is
   re-projected from `(source_roots, entry_rule)` onto the **full per-grammar config tuple modulo the
   generated-artefact path columns**. The corrected obligation reads: *"all `RuntimeTarget` rows sharing
   one `grammar_name` MUST be byte-identical in EVERY field except the generated-artefact path columns
   (`output_dir`, `expected_files`) — i.e. `fact_schema` / `row_id` / `output_plane` / `emitter` /
   `entry_rule` / `source_roots` / `check_command` / `frontend_requirements` collapse to ONE distinct
   tuple per `grammar_name`."* Machine-check: a tiny xtask assertion (or `awk`/`jq`) over the rows for each
   `grammar_name` asserting `count(distinct config-tuple-minus-{output_dir,expected_files}) == 1`. Under
   live HEAD this FAILS today (7 distinct `fact_schema` / `output_plane` / `row_id`) — which is CORRECT:
   the gate must be RED pre-P3 and only go GREEN after the 7 profiles genuinely collapse to one CSS config
   (the P3 deliverable). The αC:399 "collapsed-identical" prose is corrected to: *"a data-table whose rows
   for one `grammar_name` are NOT identical modulo `{output_dir, expected_files}` IS the relocated seam —
   the per-grammar-config-tuple collapse check catches it; a `(source_roots, entry_rule)`-only projection
   does NOT, because the divergence rides `fact_schema` / `output_plane` / `emitter`."* The row-count
   MECHANISM and the §3 prose obligation ("every residual routing entry names the `.bbnf` rule it derives
   from") are KEPT; only the projected column set widens. Resolves FOLD-3 + its cross-artefact twins on
   SYNTHESIS:481/:253(iii)/:328 / HANDOFF:260 / αE:196/:226 (those are αF/αE's faces; this digest's §1-P3
   / §1-P4 / §2.2 re-open test is the redress-digest face).

No REJECT was issued against αC by any V1, V2, V3, OR V4 lens. The sole V4 REVISE touching αC (FOLD-3) is
folded with a concrete mechanism — **zero orphan REVISE on αC entering V5.** This V5 revision preserves
every ACCEPTed clause from V1/V2/V3/V4 (including FOLD-1 + FOLD-2) and folds the one V4 disposition.

### §0.A.prior — V1/V2/V3 fold record (carried, all ACCEPTed; do not re-litigate)

The V3 REVISE (CH5 C.5 — the `ext/x86/`+`build.rs`+`lib.rs:247` SECOND x86 surface) was folded in the V4
cycle as **FOLD-1** and re-verified ACCEPTed across all seven V4 lenses (CH3 "the artefact that correctly
authored FOLD-1," CH7 "§6 FOLD-1 is the gold-standard … alphaA must match THIS") — retained verbatim at
§1-P1 + §2.6 + §3 x86-corollary below. The V3 §8 sharpening (CH2 §8.1 — relocated-seam grep is
necessary-not-sufficient, structural P3 collapse is primary) was folded in V4 as **FOLD-2** — retained at
§1-P3 + §1-P4 + §2.2 re-open test + §3 relocated-seam corollary, NOW further sharpened by FOLD-3 (the
projection-tuple correction). The V1 REVISEs (CH2 §3.5 P3 collapse-default; CH5 C.4 P4
witness/`EventGrammar`; CH3 P1 x86-tag same-commit) were folded in V2 and re-verified ACCEPTed at V2+V3+V4
— retained as the now-ACCEPTed clauses in §1-P1/§1-P3/§1-P4. The V2 non-blocking accuracy notes (x86 LOC
dual-figure 742 `.rs`/847 all; V1 CONSOLIDATED exists; αA-scoped) remain folded: §1-P1 carries BOTH x86
LOC figures (742 recoverable `.rs` + 105 `.asm` = 847 for `src/x86_64/`).

### §0.B — State-delta carried from SK-V17 (do NOT re-block as if still live)

`emit_fact_stream` is **gone** from the shipped CSS `generated.rs`
(`grep -c emit_fact_stream …/css_l4_declaration_values/generated.rs` = **0** at HEAD);
`W5C_REQUEST_FACT_PROFILES` is now a **retirement comment** (`codegen/src/lib.rs:298`), no longer a live
array. The fact-stream pre-block (§2.3) therefore narrows to its *residual* surfaces (`CSS_GENERATED_RS`
still hand-written; `RuntimeEmitterKind::RequestFacts` still the CSS fork) — it is NOT re-fought from
scratch.

---

## §1 — The SK-V18 PRUNE waves (P1–P5; all verified live at HEAD `318d9c046`)

PRUNE deletes the overfit / wrong-arch / contrivance the GENERALIZATION must not inherit. Each row: the
V3 finding id, the live evidence (path:line + census at HEAD), the delete-or-fix obligation, and the
close gate that makes the prune meaningful.

### P1 — DELETE the ENTIRE x86 surface (`src/x86_64/` + `ext/x86/` + `build.rs` + nasm dep) [V3 D3 / AGENT-4 F3; FOLD-1 CH5 C.5]

**Live at HEAD — the `src/x86_64/` tree:** `skinny/crates/bbnf-simd/src/x86_64/` = **24 files**
(`find … -type f | wc -l` = 24; 23 `.rs` + 1 `.asm` — `byte_class_from_eq_set_64.{rs,asm}`). **LOC:**
**742 recoverable `.rs` LOC + 105 `.asm` LOC = 847 total** (live: `.rs`-only `wc -l` = 742; the `.asm`
= 105). Declared **unconditionally** at `bbnf-simd/src/lib.rs:5` (`pub mod x86_64;` — NOT
`#[cfg(target_arch="x86_64")]`-gated). Census: **0** real x86 intrinsics (`_mm256/_mm512/_mm_`), **14**
`unimplemented!()` stub bodies. The only live call site is `lib.rs:285-287` behind
`#[cfg(all(target_arch="x86_64", target_feature="avx512bw"))]` — dead on aarch64. Module families:
`avx2/`, `avx512_{bitalg,gfni,kmask,vbmi2,vnni,vpclmul}/`, `avx_ifma/`.

**Live at HEAD — the SECOND x86 surface (FOLD-1; the V3 REVISE the V1/V2 cycles missed):**
- `skinny/crates/bbnf-simd/ext/x86/` — vendored x264/FFmpeg x86 ASM: `bbnf.asm` (485 LOC), `x86util.asm`
  (1036 LOC), `x86inc.asm` (1978 LOC), `LICENSE-VENDOR` (55 LOC) = **3554 LOC** (live `find …/ext/x86
  -type f | xargs wc -l | tail -1` = 3554).
- `skinny/crates/bbnf-simd/build.rs` — the **nasm-rs x86 assembler driver** (102 LOC; `:1` header,
  `:28-30` `rerun-if-changed=ext/x86/*.asm`, `:38-40` non-x86 early-return, `:52` `include_root=…/ext/x86`,
  `:56-76` `nasm_rs::Build … rustc-link-lib=static=bbnf_simd_asm`).
- `skinny/crates/bbnf-simd/Cargo.toml` — `build = "build.rs"` (`:8`) + `nasm-rs = "0.3"` build-dep
  (`:13-19`).
- `skinny/crates/bbnf-simd/src/lib.rs:247` — the scalar-reference contract points at `ext/x86/bbnf.asm`.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` — **COMPILE-COUPLED, NOT doc/test-only (V5 R-2/F.6
  correction):** 11 `x86_64` reference tokens, of which **9 are ACTIVE `bbnf_simd::x86_64::{avx2,
  avx512_vbmi2,avx512_gfni,avx512_bitalg,avx_ifma,avx512_vnni}::…::*_scalar(…)` call sites**
  (`:458,:464,:467,:477,:478,:484,:493,:497,:502`) that resolve into the `src/x86_64/` tree — deleting
  `src/x86_64/` + `pub mod x86_64;` WITHOUT decoupling these BREAKS THE BUILD (the test crate fails to
  compile). The verify grep `grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` fires on this
  file. (Plus `:454,:470,:500` comments + the `:672-673` `#[ignore]` `sk_v3_intrinsic_parity_x86_64`
  harness.) DECOUPLE-OR-DELETE so P1's `src/x86_64/` deletion stays build-sound.
- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs` — carries residual x86 reference
  strings in doc comments (`:10,:12,:15` "AVX-512 BW"/"AVX2"/"AVX-512 BW"); the verify grep fires on
  them. CLEAN the x86 refs (re-word to aarch64/scalar-neutral) so the grep returns aarch64-neutral only.
- **Dormancy (why REVISE not REJECT):** `build.rs:38-40` early-returns on non-`x86_64`; `ext/x86/` has
  **no** aarch64-code consumer (live `grep -rln 'ext/x86|x86inc|x86util|bbnf.asm' src/aarch64/ src/scalar/`
  = scalar-ref doc-string only). Dormant-on-aarch64, but falsifies the literal "x86 gone" close claim.

**Obligation:** DELETE the **entire x86 surface** — (1) `rm -rf src/x86_64/` + remove `pub mod x86_64;`
(`lib.rs:5`) + the `#[cfg(target_arch="x86_64")]` call site (`lib.rs:285-287`); (2) `rm -rf ext/x86/`;
(3) delete or neutralize `build.rs` (the nasm driver) + drop `build = "build.rs"` and the `nasm-rs`
build-dependency from `Cargo.toml`; (4) drop the `src/lib.rs:247` `ext/x86/bbnf.asm` contract reference,
re-home the scalar-reference contract into the aarch64/scalar module doc; **(4a) DECOUPLE-OR-DELETE
`tests/checkasm_parity.rs` — it carries 11 `x86_64` tokens, 9 of them ACTIVE compile-coupled
`bbnf_simd::x86_64::…::*_scalar(…)` call sites (`:458,:464,:467,:477,:478,:484,:493,:497,:502`) that
resolve into `src/x86_64/`; deleting `src/x86_64/` without decoupling these BREAKS THE BUILD — drop the
x86_64 reference block (and the `:672` `#[ignore]` `sk_v3_intrinsic_parity_x86_64` harness) so the test
crate compiles aarch64-only;** (4b) CLEAN the residual x86 doc strings in
`src/scalar/byte_class_from_eq_set_64.rs` (`:10,:12,:15` "AVX-512 BW"/"AVX2" — re-word aarch64/scalar-neutral);
(5) remove the
`x86_64` gate entries in `lock14_baseline.rs` (the `("crates/bbnf-simd/src/x86_64", "diagnostic-x86")`
tag and any `accepts_current_allowlist`/`validate_frozen_status_output` x86 cases). **V2-FOLD (CH3
sequencing, held):** the x86-tag removal AND the corresponding `accepts_current_allowlist`
`…contains("…/x86_64")` assertion drop land in the **same commit** as the tree deletion — otherwise the
gate desyncs (asserts a path that no longer exists). This is the aarch64-only mandate (§0 binding pin;
invariant §5.3): there must be NONE.

**LOC accounting (FOLD-1):** `src/x86_64/` (−847: 742 `.rs` + 105 `.asm`) + `ext/x86/` (−3554) +
`build.rs` (−102) + the `Cargo.toml` nasm lines = **≈ −4500 LOC** across the deleted x86 surface (vs the
V1/V2 cycles' −847-only headline). The close gate is by content/grep, not LOC-budget, so the figure is
accounting-honesty, not the gate criterion.

**Close gate (FOLD-1 — crate-wide, not `src/`-scoped):** `find skinny/crates/bbnf-simd/src/x86_64
-type f` returns empty AND `find skinny/crates/bbnf-simd/ext/x86 -type f` returns empty AND
`bbnf-simd/build.rs` is gone-or-aarch64-neutral AND `Cargo.toml` carries no `build=`/`nasm-rs`; the
**crate-wide** verify `grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` (over `src/`, `ext/`,
`build.rs`, `Cargo.toml`, `tests/`) returns only aarch64-neutral comments (modulo a single comment noting
the arch is aarch64-only) — every active hit, including the 9 `tests/checkasm_parity.rs`
`bbnf_simd::x86_64::…` call sites and the `src/scalar/byte_class_from_eq_set_64.rs` doc strings, is on the
(4a)/(4b) removal list; **BUILD-SOUNDNESS: `cargo build` AND `cargo test --no-run` are clean — the
`checkasm_parity.rs` decoupling (4a) is what keeps the `src/x86_64/` deletion from breaking compilation**;
the Lock-14 baseline gate no longer carries an x86 exclusion tag AND its
allowlist assertion no longer names the deleted path. **PERMANENT pre-block §2.6 binds the rebuild so the
deletion is not silently re-introduced.**

### P2 — DELETE the OLD contrived CSS bench path [V3 C3 / AGENT-2 F4/F5]

**Live at HEAD:** `bbnf-bench/src/nonjson_css_l4.rs` — `lightningcss_facts` (`:528`) does, per call:
`validate_fixture_shape` (SHA256, `:1989` checks `input.len() != 187`) + `StyleSheet::parse` + a
projection walk + **a second `cssparser` re-parse** — strictly more work than the skinny plane
(comparator-inflation). `measure_mbps` (`:3091`) is a **warm** bench (warmup loop at `:3097`, then hot
iters) over the **85–357-byte SHA-pinned micro-fixtures** (`EXPECTED_FIXTURE_BYTES = 187`, `:66`), NOT
the real 71KB–495KB corpus. This did **not** produce the SK-V17 headline numbers (those came from
`css_canon_bench`) but it is a live contrivance + confusion surface — it violates `[no-warm-benches]`,
timed-plane-symmetry, and corpus-in-the-timer.

**Obligation:** DELETE the `measure_mbps` warm-micro-fixture path + the comparator-inflated
`lightningcss_facts` (SHA256 + second-parse) + the per-fixture SHA256/byte-len/expected-projection pins
(`:59–203`, `:1989+`, `expected_fixture_projection :2502`) that sit in or gate the timed path.
Fixture-shape regression guards survive ONLY in `#[test]`, never inside a `measure_mbps`-reached
function. Keep `css_canon_bench` (the honest cold real-corpus harness) — it is the H1 keeper.

**Close gate:** `grep -n 'fn measure_mbps\|fn lightningcss_facts\|EXPECTED_FIXTURE_BYTES\|_FIXTURE_SHA256'
nonjson_css_l4.rs` returns zero (or test-only); no warm/micro-fixture timed CSS path survives; the only
timed CSS comparator is the symmetric `css_canon_bench` real-corpus cold harness. **This is also the H1
honesty close (lazy-rich-summary vs eager-full-CSSOM disclosed, or a symmetric materialization-depth
comparator added).**

### P3 — COLLAPSE the 7 byte-identical CSS `generated.rs` replicas [V3 D1 / AGENT-3]

**Live at HEAD:** the seven `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`
(`css_l4_{at_rules_and_media, declaration_values, declaration_values_extended, nested_layout,
stylesheet_selectors, vendor_and_custom_atrules, visual_functions}`) are **byte-identical** —
`md5` over all seven yields a **single** digest (7 files → 1 hash; live
`for d in …/css_l4_*/generated.rs; do md5 -q "$d"; done | sort -u | wc -l` = **1**). ONE CSS parser
replicated 7×; materially overstates "7 grammars admitted." All seven share `entry_rule:"stylesheet"` +
`CSS_L4_ROOTS = ["grammar/css/l4/stylesheet.bbnf"]` (ONE `.bbnf`; `regen_css.rs:24,39-40` + the
per-target `RuntimeTarget` DATA rows `:35`).

**Obligation (V2-FOLD, CH2 §3.5 collapse-vs-differentiate, held + ACCEPTed):** the **DEFAULT and correct
obligation is COLLAPSE-to-ONE** — the 7 directories are demonstrably one grammar (one `.bbnf`, one entry
rule), so they collapse to a single grammar-derived CSS provider that the G2/G3 generalization emits.
"Differentiate into N distinct `generated.rs`" is admissible **ONLY IF** N distinct `.bbnf` roots are
genuinely authored (distinct grammars, not config labels over one `.bbnf`); absent that, "N distinct
generated.rs" is a **hollow distinct-grammar-output target** satisfiable by cosmetic divergence, which the
diff-census addendum must REJECT. This binds the **distinct-grammar-output** CHALLENGE addendum to
*provenance*, not cosmetics: after GENERALIZE, the (one) CSS `generated.rs`, the JSON `generated.rs`, and
the Sheets `generated.rs` are each non-identical real output of the single generator **because they derive
from distinct `.bbnf`** — NOT because 7 cosmetic CSS variants were minted.

**FOLD-2 (CH2 V3 §8.1 — the relocated-seam enforcement attribution): the P3 collapse is the PRIMARY
structural defense against the relocated-overfit-seam threat, NOT the grammar-name grep.** A
`match grammar`-style arm-census regex CANNOT catch a **neutral-identifier metadata data-table** (a
`RuntimeTarget`-style DATA table keyed by config rows over one `.bbnf`, `regen_css.rs:35`). The
structural row-count collapse (7→1 CSS provider) IS the defense; the grammar-name grep is
NECESSARY-NOT-SUFFICIENT. The relocated-seam pre-block (§2.2) carries this attribution explicitly.

**FOLD-3 (CH2 V4 §8.1 — the projection-tuple correction; the sole V4 REVISE touching αC): the structural
collapse check must project onto the FULL per-grammar config tuple, not onto `(source_roots, entry_rule)`.**
The `RuntimeTarget` struct (`regen.rs:6-19`) has 12 fields; a `(source_roots, entry_rule)`-only `sort -u`
discards the columns where per-grammar divergence actually rides. **Verified live at HEAD this V5 cycle:**
`(source_roots, entry_rule)` sort -u = **1** (false-GREEN) AND `grammar_name` sort -u = **1**, BUT
`fact_schema` / `output_plane` / `row_id` / `output_dir` sort -u = **7** each. A relocated branch that
rides `fact_schema` / `output_plane` / `emitter` (an emitter dispatching on `target.fact_schema` to select
a per-profile generated body) sails through a `(source_roots, entry_rule)` projection. The corrected
`runtime_target_rows_collapsed` machine-check: **"all `RuntimeTarget` rows sharing one `grammar_name` MUST
be byte-identical in EVERY field except the generated-artefact path columns (`output_dir`, `expected_files`)
— `fact_schema` / `row_id` / `output_plane` / `emitter` / `entry_rule` / `source_roots` / `check_command`
/ `frontend_requirements` collapse to ONE distinct tuple per `grammar_name`."** Today this FAILS RED (7
distinct `fact_schema`) — correct: the gate must be RED pre-P3 and GREEN only after the 7 profiles
genuinely collapse to one CSS config. The MECHANISM (row-count collapse) is held; only the projected
column set widens.

**Close gate (FOLD-3-corrected):** the CSS replica count is **1** (the directory count itself collapses to
one CSS provider — the prior `md5 … | sort -u` no longer reports a single hash standing for 7 directories);
the cross-grammar diff-census (CSS vs JSON vs Sheets) shows N non-identical `generated.rs` where N = the
count of genuinely-distinct authored `.bbnf` roots, and **every** such pair traces to a distinct `.bbnf`
(no cosmetic-only divergence passes); the `runtime_target_rows_collapsed` machine-check — **projected onto
the full per-grammar config tuple modulo `{output_dir, expected_files}`** — passes GREEN (one distinct
config tuple per `grammar_name`), which it does NOT today (the RED-pre-P3 / GREEN-post-P3 falsifiability
proof); the `RuntimeTarget` DATA table no longer carries 7 per-profile-divergent CSS rows over one `.bbnf`.

### P4 — FIX the Lock-14 gate exclusion holes [V3 D4 / AGENT-4 F4/F5]

**Live at HEAD:** `bbnf-bench/src/lock14_baseline.rs` — `GENERIC_SCAN_ROOTS` omits `runtime_generator.rs`;
that file is routed instead into a **weaker** extra-coverage check that checks coverage-column presence,
NOT the forbidden-token neutrality scan. The x86 tree is tagged `("crates/bbnf-simd/src/x86_64",
"diagnostic-x86")` (`:2463`); `accepts_current_allowlist` (`:2729`) **PASSES** — *because* the leak files
are excluded from the neutrality scan. A green gate over standing leaks is worse than a red one (the
SK-V18 generalization's neutrality proof depends on the gate being meaningful).

**Obligation:** EXTEND `GENERIC_SCAN_ROOTS` to cover `runtime_generator.rs` (and, post-G3, whatever the
unified emitter file is) + the JSON/CSS template surfaces; extend the forbidden-token list with `CSS_*_RS`
/ `JSON_*_RS` / template-const patterns. Remove the `diagnostic-x86` tag (P1 deletes the tree it labels;
see P1 same-commit coupling).

**V2-FOLD (CH5 C.4 — the witness / `EventGrammar` grammar-named-type seam, held + ACCEPTed):** the witness
files (`grammars/json/event_grammar_witness.rs:4 JsonEventGrammar`,
`grammars/sheets_witness/event_grammar_witness.rs:4 SheetsEventGrammar`) carry grammar-NAMED types in the
RUNTIME crate, which the generic-crate Lock-14 scan never sees — fine, because runtime is grammar-specific
**by construction**. The danger is one-directional: if G4 *instantiates* the `<G>` axis, a
`ValueRef<…,JsonEventGrammar>` may appear in production `runtime/` code (legitimate), BUT if the un-forked
generator (G3) ever **EMITS** a `ValueRef<…,XEventGrammar>` type **literal** as a string, that is a
grammar-name leak the generic-crate-scoped P4 gate cannot catch. **Add to P4 (or to the G4 close
condition) the clause:** "if G4 instantiates the `<G>` axis, the generic emitter
(`runtime_generator.rs`/post-G3 unified) MUST NOT emit a grammar-named `EventGrammar` type literal — the
witness type is supplied by the runtime-side hand-written grammar module and injected by
NAME-PARAMETER, never templated as a string literal in codegen. Add `EventGrammar` / `*EventGrammar` to
the emitter's `FORBIDDEN_GENERIC_TOKENS` scanned over `runtime_generator.rs` (post-G3 unified emitter)."
Without this, G4's "instantiate" branch can re-couple the generic emitter to grammar names under a green
gate. This is the §3 "checked twice" corollary applied to the witness surface.

**FOLD-2 + FOLD-3 (CH2 §8.1 V3+V4 — the arm-census-grep reach AND the structural-check projection):** the
P4 neutrality scan's `match grammar`-style arm-census regex is NECESSARY-NOT-SUFFICIENT — it does NOT
catch a neutral-identifier metadata data-table (a `RuntimeTarget`-style table of neutral config rows that
relocates per-grammar branching into DATA). P4's forbidden-token scan is paired with the **structural**
defense (P3 row-count collapse, §2.2) for the relocated-seam class. **FOLD-3 binds the structural check's
projection:** the paired structural defense is the `runtime_target_rows_collapsed` machine-check projected
onto the **full per-grammar config tuple modulo `{output_dir, expected_files}`** — NOT onto
`(source_roots, entry_rule)` (which is invariant across the 7 live css_l4 rows and therefore blind to a
branch riding `fact_schema` / `output_plane` / `emitter`). The gate must not claim either the grep alone
OR the 2-column projection alone catches a data-table relocation.

**Close gate:** `GENERIC_SCAN_ROOTS` includes the emitter file(s); the forbidden-token set includes
`CSS_*_RS`/`JSON_*_RS`/template-const AND `EventGrammar`/`*EventGrammar` patterns scanned over the
emitter; `accepts_current_allowlist` PASSES *after* the rebuild because the scanned surface is genuinely
neutral (not because the dirty files are excluded); the gate-scope-honesty CH-addendum (diff
`GENERIC_SCAN_ROOTS` against the generic-crate file inventory) reports zero un-scanned production `.rs`
under `crates/codegen/src`; the relocated-seam class is enforced by the P3 row-count structural check
projected onto the full per-grammar config tuple modulo `{output_dir, expected_files}` (FOLD-3), with the
grep as a secondary. This is invariant §5.5 (Lock 14 gate must actually scan the generic crates).

### P5 — PURGE the metalang bench-wave-id leak from the shipped JSON runtime [V3 misc / AGENT-1]

**Live at HEAD:** the shipped `runtime/src/grammars/json/generated.rs` carries the bench wave-id
`parse_w11_1_number` baked into production: **7 occurrences** (`grep -c` = 7;
`parse_w11_1_number_direct`/`_object_direct`/`_array_direct` + call sites). A bench-wave label (`w11_1`)
in a SHIPPED runtime parser violates `[clean-regen-discipline]` (generated files are output of fresh
regen, never carry conversation/wave metadata).

**Obligation:** PURGE the `w11_1` wave-id from the generated JSON parser — the generator (post-G1) must
emit grammar-derived rule names, not bench-wave ids. This is a regen-discipline fix, not a behaviour
change: the function bodies stay, the names lose the wave label. Gated by G1 (the JSON generator must
derive names from the grammar, not template a wave-id literal).

**Close gate:** `grep -rc 'parse_w11_1_number\|_w[0-9]' runtime/src/grammars/json/generated.rs` returns
zero; `cargo xtask regen --check` is clean (the regenerated file matches with grammar-derived names).

**PRUNE ordering note (V2-FOLD, held):** P1/P2/P3/P5 are deletions; P4 is the gate that makes the
post-PRUNE + post-GENERALIZE neutrality claim *meaningful*. P4's emitter scan-root + `EventGrammar`-token
extension lands fully only after the emitter unification (G3) names the file it must scan — but the
x86-tag removal **and its matching `accepts_current_allowlist` assertion drop** land WITH P1 (same commit,
per CH3), and the `runtime_generator.rs` scan-root addition can land at PRUNE; the unified-emitter
scan-root + the `EventGrammar` forbidden-token follow G3/G4. Per `[abrogate-before-patch]`: every PRUNE
asks "can we delete?" before "patch?" — P1/P2/P3/P5 are deletions, P4 is the only patch (a gate-scope
correction).

---

## §2 — The pre-block REDRESS families the GENERALIZATION must NOT re-open

Two-bucket classification (unchanged from the campaign convention):
- **PERMANENT PRE-BLOCK** — intrinsically refuted; no framing recovers it. Re-opening is a CH3 regression
  failure outright.
- **ADMIT-UNDER-DIFFERENT-FRAMING** — the *intent* (typed / rich / retained) is correct; only the prior
  *carrier* (eager / allocating / fragmented / serialized) was refuted. The tape + lazy-view +
  grammar-driven generator IS the different framing.

**SK-V18-specific danger:** the generalization introduces THREE new surfaces — (a) the grammar-driven
generator (G1 JSON projection, G2 CSS lowering, G3 un-fork); (b) the shared `Value`/`Document`/`Cursor`
trait (G4); (c) the instantiated-or-deleted `ValueRef<…, G: EventGrammar = AnyGrammar>` (G4; phantom
confirmed live at `tape/mod.rs:175`). Each is a fresh place a refuted carrier can silently re-land. The
re-open tests below are keyed to these new surfaces.

### §2.1 — AZ-IV eager-value-tree materialization (118×) → ADMIT-UNDER-DIFFERENT-FRAMING

**Measured refutation:** AZ-IV W5 made the monolithic path parse *into* a value tree by default — canada
1.83ms → 215.7ms = **118×** (`cb14970f`). Root cause = eager per-leaf payload materialization
(f64-alloc-per-number, typed value built at parse time).

**SK-V18 re-open surfaces (the new ones):** (i) **G1** — when `json_sink_direct::render` PROJECTS the
parser from the `SinkOnlyProgram`/grammar, the projected parser must keep the lazy on-demand model
(`value_from_ref` reads one byte at the offset, wraps a `Copy ValueRef`, zero per-node heap alloc) — a
generator that projects an *eager* typed-node build per leaf re-lands AZ-IV through the generator. (ii)
**G2** — the grammar-DERIVED CSS recognizer must not materialize a typed CSS value per leaf at parse time.
(iii) **G4** — the shared `Value`/`Document`/`Cursor` trait must expose lazy view accessors (read-on-
demand over the sealed tape), NOT force an eager value tree at the trait boundary.

- **Re-open test (CH3 fail if true):** any per-leaf typed-node / `f64` / `Box` heap allocation at parse
  time on either grammar's generated parser path — under `runtime/src/grammars/{json,css_l4_*,sheets_*}/`,
  or emitted BY the generator into those files — that is not a re-readable source span. Anchored to the
  *construct* (per-leaf eager payload), not a fixed symbol list (CSS typed-value names are prospective).
- **Different-framing admission (CH3 pass):** the typed value is produced by **lazy view projection** over
  the sealed tape — structure reconstructed on demand from offsets + the rule's `LayoutFacts`/`BackendRule`
  shape, scalars re-read from source offset, `PayloadArena` (`tape/mod.rs`) used ONLY for irreducible
  decode (f64 bits, hex). The generator EMITS these lazy accessors; it does not emit an eager builder.
  JSON already proves this (`value_from_ref`, `json/value.rs`); CSS + Sheets must hold the same invariant
  THROUGH the generator.
- **Telemetry binding:** per-corpus payload-arena write/alloc counters in RESULTS; the gate fails any row
  (JSON, CSS, OR Sheets) whose per-leaf payload counters are non-zero except for the named
  irreducible-decode set. This is the proof the generator did not re-eagerize.

### §2.2 — StructRegistry / Arena<G> / Builder<G> per-leaf indirection → SPLIT

**Measured refutation:** AZ-IV W5 reintroduced `StructRegistry` + `Arena<G>`/`Builder<G>` registry
indirection in the hot path — **28–65×** bbnf/sheets, **983×** css bootstrap (606.4ms), **10583×
WATCHDOG** tailwind (77.6s). (AU.4.2 had *deleted* StructRegistry; AZ-IV bolted it back.)
`StructRegistry`/`OpenFrame`/`TapeStructBuilder` are grep-clean-absent from `skinny/crates/` at HEAD —
this pre-block guards against *re-introduction*, not against deleting an extant skinny construct.

**SK-V18 re-open surfaces (the new ones):** (i) **G3 un-fork** — the single grammar-agnostic emitter must
NOT dispatch a per-leaf registry/hashmap lookup keyed on grammar or rule-id; the emitter derives tape ops
from the rule's `BackendRule`/`LayoutFacts` shape ONCE per rule (compile-time), never per leaf. (ii) **G4
shared trait** — the `Value`/`Document`/`Cursor` trait must be a thin read-cursor over the EXISTING
`Tape`/`ValueRef`, NOT a generic `Builder<G>` that dispatches a registry per leaf. (iii) the
generalization must NOT introduce a new hand-coded per-grammar profile/route table (the relocated-overfit
seam — Lock 14; CH2 §3.2 named this the strongest pre-block for generality).

- **PERMANENT PRE-BLOCK (§2.2a, the indirection):** any per-leaf or per-compound registry/hashmap
  dereference under `runtime/src/`; any per-compound `Vec` heap alloc (`split_off`, `Vec<Vec<T>>`,
  `Box::new` per value) on any grammar's parse path; any checkpoint that clones a frame stack. No framing
  admits a registry deref per leaf.
- **ADMIT-UNDER-DIFFERENT-FRAMING (§2.2b, the layout itself):** the *layout description* is built once per
  rule (compile-time) and IS the generality vehicle — it is exactly what the SK-V18 generator must
  consume. The emitter DERIVES tape ops from `LayoutFacts.backend_shape ∈ {EagerTape, OffsetTape,
  EventTape, SinkOnly, CollapsedStage}` (`ir/src/cost.rs`) via `lower/{tape_plan,offset_tape,event_tape}.rs`.
  **NB Lock 2: canonical name is `Layout`/`LayoutFacts`, NOT `StructLayout` (RETIRED, `LOCKS.md`).**
  Admission requires the emitter to *derive* from the rule shape, not hardcode a per-grammar table or look
  up a registry per leaf.
- **Re-open test (CH3 fail if true):** the un-forked generator emits a per-leaf registry/`Builder<G>`
  lookup; OR any new hand-coded per-grammar profile/route table parallel to the (now-retired)
  `W5C_REQUEST_FACT_PROFILES`; OR a `split_off`/`Vec<Vec>` arena / frame-clone checkpoint on any grammar
  path. **FOLD-2 + FOLD-3 (CH2 §8.1 V3+V4 — the relocated-seam enforcement):** the strongest form of this
  re-open is a **neutral-identifier metadata data-table** (a `RuntimeTarget`-style table that relocates
  per-grammar branching into DATA rows over one `.bbnf`) — this is NOT caught by a `match grammar`
  arm-census grep. The PRIMARY enforcement is the **P3 structural row-count collapse** (7→1 CSS provider;
  §1-P3 close gate). **FOLD-3 binds the projection:** the structural check must `sort -u` the
  `RuntimeTarget` rows for each `grammar_name` over the **full per-grammar config tuple modulo
  `{output_dir, expected_files}`** — NOT over `(source_roots, entry_rule)` alone. The two-column
  projection is invariant across the 7 live css_l4 rows (`sort -u` = 1, false-GREEN) while
  `fact_schema` / `output_plane` / `row_id` each carry 7 distinct per-profile values (`sort -u` = 7) — a
  relocated branch riding `target.fact_schema` / `target.output_plane` / `target.emitter` to select a
  per-profile generated body passes the narrow projection invisibly. The widened-tuple collapse check
  catches it where both the grep and the 2-column projection cannot. The grep is
  NECESSARY-NOT-SUFFICIENT; the 2-column structural projection is necessary-not-sufficient; the
  full-config-tuple collapse is the catcher.
- **Different-framing admission (CH3 pass):** Open/Close/Leaf records pushed via `TapeBuilder`
  (`push_plain_offset` = one branchless `u32`); children recovered by cursor arithmetic over
  `Tape`/`ValueRef` (no split_off, no per-compound Vec); O(1) checkpoint = `offsets.len()` marker +
  truncate. The generator derives all of this from `LayoutFacts` — ONE emitter, no grammar fork (G3), no
  registry. A `RuntimeTarget`-style table is admissible ONLY if each row traces to a distinct authored
  `.bbnf` (provenance, not config-over-one-grammar), AND all rows of one `grammar_name` collapse to one
  config tuple modulo `{output_dir, expected_files}`.
- **Telemetry binding:** the falsifiability gate names canada/bootstrap/tailwind explicitly and requires
  **no-WATCHDOG** (tailwind completes within bounded cold time — the 77.6s is the disqualifier); the
  samply attribution row shows **zero** registry-lookup / split_off / frame-clone self-time on every
  grammar's hot leaf, including Sheets; the `runtime_target_rows_collapsed` structural check — projected
  onto the full per-grammar config tuple modulo `{output_dir, expected_files}` (FOLD-3) — reports the
  post-collapse CSS provider count = 1 (no neutral-identifier data-table standing in for N grammars via a
  per-profile-divergent column). This check is RED-pre-P3 (today: 7 distinct `fact_schema`) and
  GREEN-only-post-collapse — its falsifiability is the proof the seam closed.

### §2.3 — CSS fact-stream String serialization → PERMANENT PRE-BLOCK (as output plane)

**Measured refutation:** the skinny CSS path historically emitted a tab-delimited fact-stream `String`
(`emit_fact_stream`) — ~34% of CSS self-time was `emit_*` String building; the benched figure measured
*string serialization*, not typed CSSOM. **SK-V17 W1 RETIRED this**: `emit_fact_stream` is **gone** from
the shipped CSS `generated.rs` (`grep -c` = 0 at HEAD); `W5C_REQUEST_FACT_PROFILES` is a retirement
comment (`codegen/src/lib.rs:298`). So the String-as-output-plane is *already gone* — the pre-block is now
about not RE-INTRODUCING it, and about the **residual fork**.

**Residual at HEAD (the SK-V18 retirement obligation, NOT a fresh re-block):** `CSS_GENERATED_RS` is still
a **hand-written const `&str`** (`runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"…"#`)
emitted verbatim as the CSS `generated.rs` (consumed at `:91` `normalize(CSS_GENERATED_RS)`); the CSS emit
path is `RuntimeEmitterKind::RequestFacts` (`grammar_provider.rs:40-42` `enum {CompiledLowering,
RequestFacts}`, dispatched `:110`) — a grammar-family fork. The four JSON blobs are likewise verbatim
(`runtime_generator.rs:195 JSON_PARSE_ONLY_GENERATED_RS`, `:550 JSON_PARSE_ONLY_PARSER_RS`, `:572
JSON_MOD_RS`, `:594 JSON_HOST_RS`). The grammar `.bbnf` never feeds the CSS recognizer. This is the
**verbatim-blob** + **single-emitter-path** CHALLENGE addenda: a `@generated` file that is a verbatim
`&str` literal in codegen is hand-written, not derived (REJECT as "grammar-driven"); a grammar-family fork
(`RuntimeEmitterKind` JSON-vs-CSS) is not one grammar-agnostic emitter.

- **Re-open test (CH3/CH5 fail if true):** SK-V18 benches/admits CSS Track 1 as a `String` fact-stream; OR
  the generated view-time projection emits a serialized String into the hot path; OR any
  `push_str`/`to_string`/fnv64 on the per-parse CSS hot path. A retained String product is also a
  Track1==Track2 / sidecar dishonesty (Lock 1, CH5).
- **Re-open test — retirement clause (CH2/CH5 fail if NOT done):** SK-V18 closes with `CSS_GENERATED_RS`
  still a hand-written const `&str` (verbatim-blob), OR `RuntimeEmitterKind::RequestFacts` still the CSS
  emit fork (single-emitter-path failure). G2 must retire the const string for a grammar-DERIVED
  recognizer; G3 must un-fork the emitter. A CSS `generated.rs` that is still the normalized const (not
  real generator output) fails the distinct-grammar-output diff-census against JSON/Sheets.
- **Different-framing admission (CH3 pass):** the benched/admitted CSS Track 1 is the **typed CSSOM
  produced by lazy view projection over the tape**, emitted by the SAME grammar-agnostic generator that
  emits JSON and Sheets; the String serialization cost is *deleted from the hot path*, replaced by tape
  append (one branchless `u32` per structural token, exactly JSON's `push_plain_offset`). Any diagnostic
  fact-stream survives ONLY behind a forbidden-token scan (Lock 1 fact-stream clause).
- **Telemetry binding:** RESULTS Output-plane column reads `typed direct` / `borrowed view`, never
  `digest`/`FactStream`, for any admitted CSS row; the W6 8-field structural equality
  (`rules=10136/style=9561/sel=9561/decls=20043`) is the parity proof the typed value carries the rich
  shape, re-proven against the grammar-DERIVED recognizer (not the old const).

### §2.4 — The 24-row broadcast measurement → PERMANENT PRE-BLOCK

**Measured refutation:** all 24 CSS L4 "row admits" were ONE measurement broadcast 24× — identical
`track1=2319.041, lightningcss=929.281, cssparser=2362.037` repeated across 24 conceptual feature row-ids
(`css_l4_w8.rs measure_full_parse_profiles`, `W8_SELECTED_CSS_ROWS=24`). Workload-mismatched (brace-counter
`CssFullParseSummary` vs full CSSOM). The 24 falsified rows were demoted to diagnostic/non-admitted at
SK-V15 W1.

**SK-V18 re-open surface:** the generalization re-benches three grammars (JSON, CSS, Sheets) across
multiple corpora. The danger is that a single aggregate timing loop projects one number across multiple
corpus/feature row-ids — especially for Sheets, the NEW grammar with a fresh corpus that has no
established per-corpus discipline.

- **Classification: PERMANENT PRE-BLOCK.** A measurement-honesty fault; no framing recovers a single
  number broadcast across N conceptual rows (Lock 8: "Repeated throughput tuples across conceptual row IDs
  are non-admit unless each row has independent command/input/equality/timing").
- **Re-open test (CH1/CH5 fail if true):** SK-V18 publishes N rows (any grammar) sharing the same Mbps
  tuple; OR a single aggregate loop times a combined corpus and projects the result across multiple
  `measurement_row_id`s; OR rows lack independent `command/input/equality/timing` + `broadcast_group_id`.
  Applies to the Sheets corpus rows verbatim.
- **No different-framing admission.** The only legitimate path: partition the corpus, time **each corpus
  independently** (distinct `measurement_row_id` + `broadcast_group_id`, Lock 8), N≥50 cold + **median**.
  This is replacement, not re-framing. CSS corpus = animate / bootstrap / tailwindcss /
  material-components-web (`css_l4_corpus.rs:22-54`); Sheets gets its own per-corpus discipline from the
  same template.
- **Telemetry binding:** the canonical bench is N≥50 cold samples + median per corpus; the
  timed-plane-symmetry + corpus-in-the-timer CHALLENGE addenda forbid micro-fixture timing (P2 deletes the
  surviving instance).

### §2.5 — FNV / fixture contrivances → PERMANENT PRE-BLOCK (as runtime/admission)

**Measured refutation:** (1) FNV closed-enum arbiter — hashed the decoded string + matched a closed table;
bench-only, never linked into production runtime. (2) Fixture-named overfit — `generated_real_typed.rs`
carries **148** fixture-named parse fns + hand-tuned per-corpus capacity constants. FNV stays bench-only /
quarantined (SK-V15 W10); production migration remains BLOCKED.

**SK-V18 re-open surface:** the grammar-driven generator + the shared value-API trait are the new places a
fixture-named constant or an FNV selector could leak into a SHIPPED runtime — and the generator's tape
pre-sizing is exactly where a per-corpus capacity constant could re-land.

- **Classification: PERMANENT PRE-BLOCK (as runtime/admission); ADMIT only as bench-only-quarantined.**
  FNV "cannot be used as a runtime selector, production arbiter, or correctness proof."
- **Re-open test (CH1/CH5 fail if true):** any FNV/checksum migrates into `skinny/crates/runtime/` (or any
  production crate) as a selector/arbiter/correctness proof; any per-corpus / fixture-named parse fn or
  hand-tuned capacity constant EMITTED BY the generator onto the tape path; any schema/policy/witness
  header keyed to a fixture name. The generator's scratch/tape pre-sizing must derive from `input.len()` +
  the grammar's `BackendRule`/`LayoutFacts` shape, grammar-generally, never per-corpus. **P5 is the live
  instance of this class** (the `parse_w11_1_number` bench-wave-id leak in the shipped JSON `generated.rs`
  — a non-grammar-derived name baked into production).
- **Different-framing admission (CH3 pass):** fixtures are *inputs* to scalar-reference / checkasm parity,
  never *selectors*; FNV stays a diagnostic equality-witness behind the W10 quarantine; capacity/sizing
  derives from `input.len()` + grammar shape. The grammar-derived typed projection (G1/G2/G4) REPLACES the
  148 fixture-named parse fns — it does not re-emit them.
- **Telemetry binding:** the SK-V18 gate carries the W10 FNV-quarantine no-runtime-migration check + a Lock
  14 grammar-name/grammar-shape leak census that fails on any fixture-named OR bench-wave-id symbol in the
  tape/projection/generator output path (this is the P5 close gate generalized).

### §2.6 — x86 / AVX / SVE paths → PERMANENT PRE-BLOCK (this pass; diagnostic-only)

**What it is:** x86_64 AVX2/AVX-512 classify modules exist in `bbnf-simd` but are out-of-scope for the
aarch64 Apple M5 Max proof. SVE is disallowed (Apple cores have NO SVE — NEON+AMX only; SVE paths are dead
code on M5 Max). **P1 DELETES the ENTIRE x86 surface (FOLD-1):** `src/x86_64/` (24 files, 742 `.rs` +
105 `.asm` = 847 LOC) **AND** the second x86 surface the V3 CHALLENGE found — `ext/x86/` (3554 LOC
vendored ASM), `build.rs` (102 LOC nasm driver), the `Cargo.toml` nasm build-dep, and the `lib.rs:247`
contract reference. So this pre-block is the *binding* that the generalization (and the G6 ASM backlog)
must not re-introduce any of it.

- **Classification: PERMANENT PRE-BLOCK (this pass).** x86 carries zero admission weight in SK-V18; it may
  be a successor-phase escalation only (PASS-ALPHA §8). Lock 16 keeps x86/AVX-512 rows as optional
  flaw-probe / diagnostic only.
- **Re-open test (CH4/CH5/CH6 fail if true):** any SK-V18 wave (especially **G5** JSON-onto-neutral-NEON and
  **G6** CSS-NEON-wiring + the UDOT/PMULL/TBX/CSSC ASM backlog) lands an x86/AVX path as a same-wave
  consumer or claims a row movement on x86; any RESULTS row whose admitted Mbps came from an x86 build; any
  SVE/SME primitive filed as NEON; **re-creation of `bbnf-simd/src/x86_64/` OR `bbnf-simd/ext/x86/` OR a
  nasm `build.rs` after P1 deletes them** (FOLD-1). The re-open test is the **crate-wide** grep
  (`grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`), not a `src/`-scoped one — the V3 REVISE
  proved `ext/x86/` + `build.rs` are SIBLINGS of `src/` that a `src/`-scoped grep misses.
- **Different-framing admission:** none in this pass. The hot-leaf vocabulary is aarch64 NEON
  intrinsics-first (`core::arch::aarch64::*`), each with scalar-ref + checkasm + same-wave consumer. The G6
  ASM backlog is aarch64-only: PMULL prefix-XOR (`bitmap_prefix_xor_64`), UDOT digit MAC (wire the orphan
  `aarch64/digit_mac.rs:27 parse_4_digits_dotprod`), TBX range-extension (`byte_class_from_table_64`),
  CSSC clamp (`eob_pad_clamp`) — the 5 scalar-passthrough kernels get real NEON bodies OR are marked
  honestly. **V2-FOLD (CH4 §5, held; V3 corrected count):** the G6 NEON-body count is BOUNDED in the
  contract (the named kernel set, not an open "+150 LOC/body" budget) and the checkasm differential count
  is stated honestly (**12 single-kernel + 2 composite = 14**, NOT "18" — the V3 αA §3.4 / αE F4 / αD §1
  correction; an "18-present" gate is un-satisfiable on a clean tree) — these are αA/αD/αE corrections that
  this digest defers to but does not re-state numerically beyond binding the 14-figure here.
- **Telemetry binding:** every SK-V18 NEON/ASM primitive records the Lock 16 manifest (owner, scalar
  oracle, checkasm command, aarch64 hardware gate, same-wave consumer, row-movement target); the
  **acceleration-wiring** CHALLENGE addendum requires the kernel be reached AT ADMISSION (in the hot path),
  not only under `#[cfg(test)]` — directly binding the SK-V17 C1 correction (CSS NEON was dead at
  admission). No x86 in any manifest; the **crate-wide** `grep` for x86/AVX/SVE/nasm in `bbnf-simd/`
  returns zero after P1 (FOLD-1: `src/x86_64/`, `ext/x86/`, `build.rs`, `Cargo.toml` nasm all gone).

---

## §3 — The single load-bearing distinction for SK-V18

> **Typed/rich/retained is the goal (admit). Eager/allocating/fragmented/serialized/forked/
> hand-written-verbatim is the refuted carrier (pre-block).** The flat lazy-offset tape (parse side: one
> branchless append via `TapeBuilder`, O(1) checkpoint) + the layout-driven typed projection (view side:
> derived from `BackendRule`/`LayoutFacts`, lazy source re-read over `Tape`/`ValueRef`, `PayloadArena`
> only for irreducible decode), **emitted by ONE grammar-agnostic generator from `.bbnf`**, is the ONLY
> admissible carrier. Any construct from §2.1–§2.6 that lands on that carrier — zero per-leaf alloc, zero
> registry indirection, zero String hot-path, zero `RequestFacts` fork still admitting, zero hand-written
> const-string `@generated` blob, per-corpus honest timing, no FNV/fixture/bench-wave-id selector, no x86
> anywhere in the crate — is admitted. Any construct that re-lands on the AZ-IV-eager / StructRegistry /
> fact-stream / 24-broadcast / FNV / x86 carrier is a CH3 regression or CH5 hidden-coupling failure and is
> REJECTED at the CHALLENGE gate.

**The SK-V18-specific corollary (the generalization litmus — "checked TWICE"):** the generator is the new
carrier surface. A pre-block re-opens not only by a hand-written runtime construct but by the GENERATOR
*emitting* one. Every re-open test above is therefore checked TWICE: against the runtime output
(`runtime/src/grammars/*/generated.rs`) AND against the emitter that produces it
(`codegen/src/runtime_generator.rs` → post-G3 unified emitter). A generator that emits an eager builder, a
per-leaf registry, a String hot-path, a broadcast harness, a fixture-named constant, a grammar-named
`EventGrammar` type literal (V2-FOLD §1-P4), or an x86 path has re-opened the pre-block at its source.

**The relocated-seam enforcement corollary (FOLD-2 + FOLD-3, CH2 §8.1 V3+V4):** the relocated-overfit seam
(per-grammar branching relocated into a **neutral-identifier metadata data-table**) is NOT caught by a
`match grammar` arm-census grep — the grep is NECESSARY-NOT-SUFFICIENT (V3). The PRIMARY structural
enforcement is the **P3 collapse row-count check** — BUT (FOLD-3, the V4 sharpening) that check must
project onto the **full per-grammar config tuple modulo the generated-artefact path columns
(`output_dir`, `expected_files`)**, NOT onto `(source_roots, entry_rule)` alone. The narrow 2-column
projection is itself necessary-not-sufficient: across the 7 live css_l4 `RuntimeTarget` rows it is
invariant (`sort -u` = 1, false-GREEN) while `fact_schema` / `output_plane` / `row_id` carry 7 distinct
per-profile values (`sort -u` = 7 each) — exactly the columns a relocated branch (an emitter dispatching
on `target.fact_schema`/`target.output_plane`) can ride invisibly. The "N **collapsed-identical** rows"
framing is empirically false on the live table; the correct framing is "rows for one `grammar_name` that
are NOT identical modulo `{output_dir, expected_files}` ARE the relocated seam." The widened-tuple collapse
check (RED-pre-P3 today on 7 distinct `fact_schema`, GREEN-only-after-genuine-collapse) is the catcher; the
grep + the 2-column projection are each necessary-not-sufficient. This is the same md5 → grep-alphabet →
grep-can't-fire-on-data-table → 2-column-projection-too-narrow necessary-not-sufficient lineage carried
each cycle one column-set deeper.

**The x86-surface corollary (FOLD-1, CH5 C.5):** "x86 gone" is enforced **crate-wide**, not
`src/`-scoped. `ext/x86/` (vendored ASM) and `build.rs` (nasm driver) are SIBLINGS of `src/` that a
`src/`-scoped grep or LOC accounting misses; the P1 close gate + the §2.6 re-open test both use the
crate-wide `grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`.

**Locks load-bearing for these pre-blocks:** Lock 1 substrate-union / no-parallel-substrate + fact-stream
clause; Lock 2 canonical `Layout`/`LayoutFacts` (`StructLayout` RETIRED); Lock 6/14 generated-output +
`CSS_GENERATED_RS`-centralization-rejects clause; Lock 8 row-plane/broadcast; Lock 14 grammar-neutrality
(the P4 gate must actually scan the generic crates, AND — V2-FOLD — the witness `EventGrammar`-type seam,
AND — FOLD-2 — the relocated data-table enforced structurally not by grep alone, AND — FOLD-3 — the
structural check projected onto the full per-grammar config tuple, not `(source_roots, entry_rule)`);
Lock 16 primitive-manifest / aarch64-only. 16-lock count verified at HEAD
(`grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` = 16).

## §4 — Sources (every upstream artefact cited; all live facts verified at HEAD `318d9c046`)

- `restart/prompts/pass-contracts/PASS-ALPHA.md` §2 (α-C row), §3 CH3, §9.
- `restart/prompts/SK-V18-GENERALIZATION-HANDOFF.md` §0/§3/§4/§5 (binding pin, PRUNE/GENERALIZE goalset,
  CHALLENGE addenda, invariants).
- `restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md` (D1–D4, C1–C3, the prune backlog).
- **V4 CHALLENGE dispositions folded (the V5 mandate):** αC was dispositioned **ACCEPT on every section
  across all seven V4 lenses, with ONE cross-artefact §8 REVISE (CH2 §8.1) touching αC at `αC:399`** —
  `restart/skinny/tranches/sk-v18/research/alpha-hardening/V4/CH1.md:133,352` (αC ACCEPT),
  `…/V4/CH2.md:111-144` ("αC tally: ACCEPT ×4, REVISE ×0"; §8.1 cross-artefact REVISE `:294-360`; ledger
  `:421`), `…/V4/CH3.md:201-202,239-249` (αC ACCEPT — "the load-bearing CH3 artefact, V4-correct";
  "correctly authored FOLD-1"; FOLD-2 reproduced live; "Zero orphan REVISE entering V4 on αC"),
  `…/V4/CH4.md:142` (ACCEPT), `…/V4/CH5.md:101-143` ("αC tally: ACCEPT ×4, REVISE ×0, REJECT ×0"),
  `…/V4/CH6.md:79,287-300` (PRUNE close gates ACCEPT; P1 enumeration explicitly NOT a separate αC REVISE),
  `…/V4/CH7.md:167-171,319` (αC overall ACCEPT — "§6 FOLD-1 is the gold-standard … alphaA must match
  THIS"). **No V4 CONSOLIDATED on disk** — the V4 wave returned CH1..CH7 only; this V5 cycle is the
  confirming/fold cycle. **The V4 disposition folded:** FOLD-3 (CH2 §8.1 — the
  `runtime_target_rows_collapsed` projection onto `(source_roots, entry_rule)` is necessary-not-sufficient;
  the "N collapsed-identical rows" framing at αC:399 is empirically false) → §0.A.1 + §1-P3 (projection
  widened to the full per-grammar config tuple modulo `{output_dir, expected_files}`; close gate
  RED-pre-P3/GREEN-post-collapse) + §1-P4 (the structural defense's projection bound) + §2.2 re-open test +
  §3 relocated-seam corollary. **Zero orphan REVISE on αC entering V5.**
- **V3 CHALLENGE dispositions (folded at V4 as FOLD-1 + FOLD-2; re-verified ACCEPTed at V4):**
  `…/V3/CH5.md:99-157` (C.5 REVISE — the `ext/x86/`+`build.rs`+`lib.rs:247` x86 surface → FOLD-1) +
  `…/V3/CH2.md:117-146,304-432` (§8.1 sharpening — relocated-seam grep necessary-not-sufficient, structural
  P3 collapse primary → FOLD-2). Both folded + re-verified ACCEPTed at V4 — retained as the now-ACCEPTed
  clauses in §1-P1/§2.6/§3 (FOLD-1) and §1-P3/§1-P4/§2.2/§3 (FOLD-2, now sharpened by FOLD-3).
- V2 + V1 CHALLENGE: `…/research/alpha-hardening/V{1,2}/{CH1..CH7}.md` (+ V1/V2 CONSOLIDATED). All V1/V2
  REVISEs (CH2 §3.5 P3 collapse-default; CH5 C.4 P4 witness/`EventGrammar`; CH3 P1 x86-tag same-commit;
  V2 §8.1 F10 xtask-grep) folded + re-verified ACCEPTed at V3+V4 — retained as now-ACCEPTed clauses in
  §1-P1/P3/P4 + §2.2.
- Live HEAD verification (`318d9c046`, re-run this V5 cycle): `bbnf-simd/src/x86_64/` = 24 files (23 `.rs`
  + 1 `.asm`) / 742 `.rs` LOC + 105 `.asm` = 847 total / 14 `unimplemented!` / 0 x86 intrinsics,
  `lib.rs:5,285-287`; `bbnf-simd/ext/x86/` = 3 `.asm` + `LICENSE-VENDOR` = 3554 LOC; `bbnf-simd/build.rs`
  = 102 LOC nasm driver; `bbnf-simd/Cargo.toml:8` `build="build.rs"` + `:13-19` `nasm-rs="0.3"`;
  `bbnf-simd/src/lib.rs:247` `ext/x86/bbnf.asm` contract reference; `nonjson_css_l4.rs:66,528,1989,3091,
  3097`; 7 `css_l4_*/generated.rs` md5-identical (`… | sort -u | wc -l` = 1) over one `.bbnf`
  (`regen_css.rs:24,39-40` + `RuntimeTarget` DATA rows `:35`); **`RuntimeTarget` struct = 12 fields
  (`regen.rs:6-19`); the 7 css_l4 rows: `(source_roots,entry_rule)` `sort -u` = 1 AND `grammar_name`
  `sort -u` = 1, BUT `fact_schema` / `output_plane` / `row_id` / `output_dir` `sort -u` = 7 each
  (FOLD-3 live proof)**; `lock14_baseline.rs` (in `bbnf-bench/src/`) `GENERIC_SCAN_ROOTS` / `diagnostic-x86`
  (`:2463`) / `accepts_current_allowlist` (`:2729`); `json/generated.rs` `parse_w11_1_number` ×7;
  `grammar_provider.rs:40-42,110` `RuntimeEmitterKind{CompiledLowering,RequestFacts}`;
  `runtime_generator.rs:91,195,550,572,594,701` (`const CSS_GENERATED_RS: &str` at `:701`, consumed `:91`;
  `JSON_*_RS` consts at `:195/:550/:572/:594`); `tape/mod.rs:175 ValueRef<'doc,'input,K=AnyKind,
  G:EventGrammar=AnyGrammar>` (phantom; only `JsonEventGrammar`
  (`grammars/json/event_grammar_witness.rs:4`) / `SheetsEventGrammar`
  (`grammars/sheets_witness/event_grammar_witness.rs:4`) witness impls; no `CssEventGrammar` at HEAD);
  `sheets_witness/` = 25 LOC stub; `codegen/src/lib.rs:298` (`W5C_REQUEST_FACT_PROFILES` retirement
  comment); `css_l4_declaration_values/generated.rs` `emit_fact_stream` count = 0 (SK-V17 W1 retired).
- `restart/locks/LOCKS.md` (Lock 1; Lock 2; Lock 6/14; Lock 8; Lock 14; Lock 16; 16-lock count verified = 16).
