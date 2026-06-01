# SK-V18 S-P0 Overfit Audit — a3: Arch / Measurement / Gate Residual

Date: 2026-05-31. Cycle: V1. Agent: A3.
Axis: A1 (measurement integrity) + A3 (Lock-14) + A6 (pre-restart recurrence — wrong-arch).
Addendum in scope: 5 timed-plane-symmetry + corpus-in-the-timer (+ the x86 deletion, the
relocated-seam structural gate, the metalang leak).
Live HEAD: `83b66db42` (every path:line re-grepped this pass).

Companion artefacts (the canonical S-P0 axis set this a3 slots into): `a0-goalset-residual-overfit.md`
(A0 — goalset residual verdict + R-A0-1/2/3), `a1-six-addenda-lens-registry.md` (A1 — the L1–L6
lens registry, the load-bearing output), `a2-prune-sequencing.md` (A2 — the PRUNE-list P1–P5 + the
four sequencing constraints). a3 carries the arch/measurement/gate axis AND surfaces the one NEW
finding the cohort did not (F-A3.5 / R16, the nested-`output_labels` gate-recipe hazard — see §3),
which SHARPENS A2 §4a and A0 §L2's relocated-seam treatment from "fact_schema/row_id/output_plane
are not struct fields" to "they ARE fields of the nested `output_labels` struct (a top-level
`RuntimeTarget` field #12), so the by-exclusion projection must RECURSE into it."

## §1 — Timed-plane-symmetry + corpus-in-the-timer (Addendum 5, C2/C3, LIVE)

### F-A3.1 — the OLD warm micro-fixture path is STILL on disk (C3, HIGH, LIVE)

`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3091` `fn measure_mbps` — re-verified live:
16 WARMUP iters + 2000 TIMED iters over 85–357-byte SHA256-pinned MICRO-FIXTURES (NOT the
real corpus), and the timed competitor `lightningcss_facts:528` does MORE work (parse +
SHA256 + a second cssparser re-parse). This is the SK-V16 contrivance family — warm,
micro-fixture-in-the-timer, more-work-competitor — STILL in the tree.

Crucial honesty fact: it did NOT produce the SK-V17 headline numbers (those came from the
canonical `css_canon_bench`). But it is a LIVE contrivance surface and a confusion hazard:
a future wave could accidentally cite it, or a reader could mistake it for the >SOTA proof.

- **Disposition:** PRUNE (P2 — delete `measure_mbps`/`*_lightningcss_facts` + the
  per-fixture SHA scaffolding; KEEP `css_canon_bench`).
- **Gate:** `grep -n 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs = 0`;
  `corpus_in_timer == true`.

### F-A3.2 — the canonical harness IS honest (V6, CONFIRMED CLEAN)

`skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` — re-verified live: cold per-parse
(`//! no-warm-benches cold-per-parse contract`, `std::hint::black_box`, N≥50 gate, real
corpus 71KB–495KB). This is the measurement plane SK-V18 KEEPS.

### F-A3.3 — the lazy-vs-eager framing asymmetry (C2/I9, MEDIUM, the honesty residual)

The canonical numbers are MEASUREMENT-VALID, but the framing is asymmetric: Track 1 *counts*
9 aggregate fields LAZILY (zero payload writes, value-head classification) while lightningcss
*builds an owned typed CSSOM*. Not equal-work. The rich rider does cost ~25–33% over the
4-field path (real per-node work — "materially less severe than a brace-counter"), but the
honest framing is **"lazy rich-summary beats eager full-CSSOM,"** not "equal-work CSSOM
beats CSSOM."

- **Disposition:** HONESTY (H1 — re-frame OR add a symmetric materialization-depth comparator
  (lightningcss tokenize-only)).
- **Gate:** `materialization_framing ∈ {lazy-rich-vs-eager-cssom, symmetric-comparator}`.
- **Residual-audit verdict:** the timed-plane lens fires twice — once on the OLD warm path
  (P2 deletes it) and once on the framing asymmetry of the GOOD path (H1 frames it). Both
  are bound. **S-P0 carries the corpus-in-the-timer obligation forward: every SK-V18 >SOTA
  re-proof (G1/G2/G5 re-runs) must time the REAL corpus COLD; no micro-fixture, no warm
  iteration, no more-work competitor in the timed region.**

## §2 — x86 deletion: aarch64-only, the wrong-arch residual (Addendum-adjacent, D3, HIGH, LIVE)

The aarch64-only mandate is violated by TWO x86 surfaces, both re-verified live at HEAD
`83b66db42`:

- **Surface (a):** `skinny/crates/bbnf-simd/src/x86_64/` = 24 files (AVX2/AVX512/GFNI/VNNI/
  IFMA stubs), declared unconditionally at `lib.rs:5 pub mod x86_64;`, 0 real x86 intrinsics,
  14 `unimplemented!("Wave 6")` stubs.
- **Surface (b):** `skinny/crates/bbnf-simd/ext/x86/` = 4 files (vendored x264/FFmpeg ASM:
  `bbnf.asm`/`x86inc.asm`/`x86util.asm` + LICENSE), assembled by `bbnf-simd/build.rs` (nasm-rs
  driver), declared via `Cargo.toml:8 build="build.rs"` + `:19 nasm-rs = "0.3"`, referenced at
  `src/lib.rs:247` ("Contract documented in ext/x86/bbnf.asm").

Plus the compile-coupled + cfg-arm surfaces the crate-wide verify grep ALSO fires on:
`lib.rs:285` `#[cfg(all(target_arch = "x86_64", target_feature = "avx512bw"))]` dispatch arms;
`tests/checkasm_parity.rs` (11 `x86_64` tokens, 9 ACTIVE `bbnf_simd::x86_64::…::*_scalar(…)`
call sites resolving into `src/x86_64/`); `src/scalar/byte_class_from_eq_set_64.rs` residual
x86 doc strings.

### F-A3.4 — the deletion list must be reach-matched to the verify grep (CH6 V4 / CH5 V3)

The single most consequential Alpha fold (and the residual S-P0 must NOT let drift): the P1
deletion list and the crate-wide verify grep must be REACH-MATCHED, or the gate is
RED-by-construction (a deletion list narrower than the grep leaves the gate firing on
un-listed surfaces — inviting a receiver to silently narrow the grep back). The contract
correctly enumerates all of: `src/x86_64/` (a) + `ext/x86/` (b) + `build.rs` + `Cargo.toml`
nasm-rs dep + `lib.rs:5`/`:247`/`:285-288` + `checkasm_parity.rs` decouple (build-soundness)
+ `byte_class_from_eq_set_64.rs` doc-string clean.

- **Disposition:** PRUNE (P1 — delete BOTH surfaces crate-wide, deletion list reach-matched).
- **Gate:** `x86_tree_deleted == true`; `find …/src/x86_64 …/ext/x86 -type f = 0`;
  `grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm'
  skinny/crates/bbnf-simd/` → aarch64-neutral only; `cargo build` AND `cargo test --no-run`
  clean (the build-soundness close-gate).
- **Residual-audit verdict:** the x86 tree is DORMANT on aarch64 (`build.rs` early-returns on
  non-x86_64; `ext/x86/` referenced by no aarch64 admission path) — so it is pure-prune,
  zero aarch64 risk. But "x86 gone" is LITERALLY FALSE until deleted, and the crate-wide grep
  must collapse to the neutral-comment floor. S-P0 confirms the contract's reach-matched
  deletion list is COMPLETE against the live grep surface (all firing files named). **No
  residual escape: the deletion list = the grep's reach.**

## §3 — The relocated-overfit-seam structural gate (the DEEPEST residual + a NEW precision finding)

This is the most subtle residual on the goalset surface, and it is where S-P0 contributes a
NEW precision finding the Alpha CHALLENGE did not fully surface.

### Background

The defense against a per-grammar branch relocated into a neutral-identifier `RuntimeTarget`
data-table is the STRUCTURAL `runtime_target_rows_collapsed` check (the arm-census regex is
syntactically incapable — a neutral table has no `Json =>` token). The Alpha contract
correctly redefined the check, by EXCLUSION, as: all `RuntimeTarget` rows sharing one
`grammar_name` must be byte-identical in EVERY field EXCEPT the generated-artefact path
columns (`output_dir`, `expected_files`); `count(distinct config-tuple-minus-(output_dir,
expected_files)) == 1` per `grammar_name`.

### F-A3.5 — NEW precision finding: `fact_schema`/`row_id`/`output_plane` are NESTED in `output_labels`, not top-level RuntimeTarget fields

S-P0 re-grepped the live `RuntimeTarget` struct (`skinny/xtask/src/regen.rs:6-19`, HEAD
`83b66db42`) and reconciled it against the css_l4 row construction
(`xtask/src/regen_css.rs:36-53`). The live struct is exactly **12 fields**. In the block below
the `NN:` prefixes are the `regen.rs` SOURCE-LINE numbers (the `:6-19` span), NOT field-position
ordinals — the 12 fields are `grammar_name`…`output_labels` (field #1…#12), occupying source
lines 7-18; `frontend_requirements` is field #11 (source line 17) and `output_labels` is field
#12 (source line 18). Reading the line-prefix `17:`/`18:` as "field #17/#18" is the exact slip
this annotation forecloses; the gate is field-NAMED and mechanism-agnostic, so no gate keys on
either the line number or the field ordinal:

```
regen.rs:6   pub(crate) struct RuntimeTarget {            // (line 6 — struct header, not a field)
regen.rs:7       grammar_name,    line 8: profile,         line 9:  entry_rule,    line 10: source_roots,
regen.rs:11      output_dir,      line 12: check_command,  line 13: source_inputs, line 14: metadata_inputs,
regen.rs:15      emitter,         line 16: expected_files, line 17: frontend_requirements,   // field #11
regen.rs:18      output_labels: Option<codegen::RuntimeOutputLabels>,                         // field #12
regen.rs:19  }
```

**The discovery:** `fact_schema`, `row_id`, `output_plane` are NOT top-level `RuntimeTarget`
fields — they are fields of the NESTED `RuntimeOutputLabels` struct (verified a distinct struct
at `grammar_provider.rs:92`) that is the VALUE of the `output_labels` field (field #12). The 7
css_l4 rows carry 7 DISTINCT `output_labels` values (verified: `fact_schema:
"css-l4-at-rules-media-facts-v1"` … `"css-l4-visual-function-facts-v1"`, each with a distinct
`row_id`/`output_plane`).

**Reconciliation with A2 §4a / A0 §L2.** A2 §4a and A0 §L2 correctly observe that
`fact_schema`/`row_id`/`output_plane` are "not struct fields" of `RuntimeTarget` and describe
them as "per-profile content the `profile` discriminator selects" (matching the contract's F16
prose). a3 SHARPENS this: they ARE struct fields — of the nested `RuntimeOutputLabels`, reachable
THROUGH `RuntimeTarget.output_labels` (a top-level field that IS in the by-exclusion comparison
set). This is not a contradiction — A2/A0 are right that they are not TOP-LEVEL fields, and the
by-exclusion close-gate is therefore SOUND (it excludes only `output_dir`/`expected_files`, so
`output_labels` and its nest are compared). The sharpening is about the IMPLEMENTATION RECIPE: a
gate-consumer author taking the prose's 3 named pseudo-fields literally could compare
`output_labels` SHALLOWLY (by `Option` discriminant) and MISS the 7 distinct nested values — a
false-green. The recipe must recurse into the nest.

**Why this matters for the gate (load-bearing, not cosmetic):**

1. **The contract's by-exclusion form is STRUCTURALLY CORRECT** — it excludes only
   `output_dir` + `expected_files`, so `output_labels` (the nest carrying the 7 distinct
   fact_schema/row_id/output_plane) IS in the operative comparison set. The gate correctly
   stays RED pre-P3 because the 7 rows differ in `profile`, `output_dir`, AND `output_labels`.
   **The by-exclusion statement is authoritative and SOUND.**

2. **But the contract's PROSE enumeration is IMPRECISE in a way that could mislead an
   implementer.** Every F13/F16 restatement lists `fact_schema`/`row_id`/`output_plane` as if
   they were operative fields "the `profile` discriminator selects" — describing them as
   per-profile CONTENT rather than as the FIELDS of a nested `output_labels` struct. An
   implementer authoring the machine-check (`count(distinct config-tuple-minus-(output_dir,
   expected_files))`) from the PROSE enumeration (rather than the by-exclusion form) could:
   - flatten the wrong fields, OR
   - compare `output_labels` shallowly (by `Option` discriminant or pointer) and MISS the 7
     distinct nested values — a FALSE-GREEN exactly of the kind the gate exists to prevent.

3. **The machine-check MUST recurse into EVERY nested-struct field**, not just
   `output_labels`. `RuntimeTarget` carries TWO nested-struct fields, BOTH reachable through a
   top-level field the by-exclusion set includes: `frontend_requirements: RuntimeFrontendRequirements`
   (field #11, `regen.rs:17`; struct at `grammar_provider.rs:46`) AND `output_labels:
   Option<RuntimeOutputLabels>` (field #12, `regen.rs:18`; struct at `grammar_provider.rs:92`).
   A recipe author who recurses into `output_labels` ONLY (the literal prose pin) would inline
   one nested struct and not the other — the SAME shallow-compare false-green displaced one field
   over. Today `frontend_requirements == REQUEST_FACTS_REQUIREMENTS` across all 7 css_l4 rows
   (`regen_css.rs:47…155`, re-grepped) so it is not a LIVE divergence vector — but the recipe pin
   exists to forbid a FUTURE relocated seam, and a seam riding `frontend_requirements` would slip
   a one-nested-struct recipe. So the recipe must derive the per-`grammar_name` config-tuple from
   the FULLY-EXPANDED row (BOTH nested struct fields inlined). A `#[derive(PartialEq)]`-based row
   comparison over the 12-field struct satisfies this automatically and CANNOT be coupled to a
   hand-rolled field list (it covers both nested structs by construction); a hand-rolled field-list
   comparison from the prose enumeration does NOT. **Cost note (disk-verified this pass):**
   `RuntimeTarget` derives only `#[derive(Clone, Copy, Debug)]` (`regen.rs:5`) — NOT `PartialEq` —
   so the full-row `PartialEq` mechanism requires ADDING the derive (one line). It is viable:
   both nested structs already derive `PartialEq, Eq` (`grammar_provider.rs:45`/`:91`) and the
   `&'static str` / `&'static [&'static str]` field types support it. The one-line derive addition
   is the pin's cost, and the full-row derive is PREFERABLE precisely because it covers both nested
   structs automatically.

- **Disposition:** binding correction to the `runtime_target_rows_collapsed` machine-check
  spec carried into S-P3 (the gate-consumer author). The CLOSE-GATE definition is already
  sound (by-exclusion); only the IMPLEMENTATION RECIPE must be pinned: compare the full
  expanded row inlining EVERY nested-struct field (`frontend_requirements` AND `output_labels`),
  NOT the prose's 3 named pseudo-fields and not a single named nested struct.
- **Gate (the INVARIANT, mechanism-agnostic — CH2 fold):** `runtime_target_rows_collapsed ==
  true` where the per-`grammar_name` config-tuple is over the FULLY-EXPANDED row (every nested
  field inlined — BOTH `frontend_requirements`'s fields AND `output_labels`'s
  `fact_schema`/`row_id`/`output_plane` included) MINUS the path columns (`output_dir`,
  `expected_files`); `count(distinct tuple) == 1` per `grammar_name`. The INVARIANT is
  implementation-independent and stated at the FULL-EXPANDED-ROW altitude (not at one named
  nested struct); S-P3 may realize it by any sufficient mechanism — a `RuntimeTarget: PartialEq`
  (deriving `PartialEq` on `RuntimeTarget`, which today derives only `Clone, Copy, Debug` —
  a one-line addition; both nested structs already derive `PartialEq, Eq`) full-row collapse is
  ONE sufficient mechanism and is PREFERABLE because it covers both nested structs automatically
  and cannot be coupled to a hand-rolled field list; serialize-then-hash or `jq` over the
  expanded rows are others. What is forbidden is a SHALLOW compare (by `Option` discriminant or
  pointer) that misses the nested distinct values of EITHER nested struct.
- **Residual-audit verdict:** the by-exclusion close-gate is SOUND and correctly RED pre-P3.
  The NEW finding is a precision hazard in the implementation recipe (the prose names nested
  fields as if top-level), which S-P0 pins so S-P3 cannot author a shallow-compare false-green.
  This is the deepest layer of the necessary-not-sufficient lineage (V1 md5 → V2 grep-alphabet
  → V3 grep-cannot-fire → V4 row-count-projects-too-narrow → V5 by-exclusion → **S-P0:
  by-exclusion is sound but the recipe must recurse into the nested `output_labels`**).

## §4 — The metalang leak (Other / I10, MEDIUM, LIVE)

`skinny/crates/runtime/src/grammars/json/generated.rs` carries `parse_w11_1_number` 7× —
re-verified live (`grep -c parse_w11_1_number = 7`). A SK-V14 bench-wave-id tag is baked into
the SHIPPED production runtime, violating `clean-regen-discipline`/`no-metalanguage-docs`.

- **Disposition:** PRUNE (P5 — fix at the generator/template source `json_sink_direct.rs` so
  regen --check stays clean; rename `parse_w11_1_number_*` → `parse_number_*`).
- **Gate:** `metalang_leak_present == false`; `grep -c parse_w11_1_number = 0`; `regen --check`
  clean.
- **Residual-audit verdict:** the leak is a regen-discipline residual — a bench-wave-id that
  became a permanent production symbol. P5 fixes it at the SOURCE (not by hand-patching the
  generated file). The S-P0 carry: add a grep gate that the shipped runtime carries no
  `w[0-9]+`/corpus-name/`sk_v` metalang tag.

## §5 — a3 disposition summary

| Finding | Addendum/axis | Live witness (`83b66db42`) | Disposition | Gate | Severity |
|---|---|---|---|---|---|
| F-A3.1 warm micro-fixture path | 5 timed-plane | `nonjson_css_l4.rs:3091` warm 2000-iter | P2 delete | `corpus_in_timer==true` | HIGH |
| F-A3.2 canonical harness honest | 5 (clean) | `css_canon_bench.rs` cold N≥50 | KEEP | — | CLEAN |
| F-A3.3 lazy-vs-eager framing | 5 honesty | `track1_rich` lazy vs lightningcss eager | H1 frame | `materialization_framing` disclosed | MEDIUM |
| F-A3.4 x86 two surfaces | A6 wrong-arch | `src/x86_64/` 24f + `ext/x86/` 4f + nasm | P1 crate-wide | `x86_tree_deleted==true` | HIGH |
| F-A3.5 nested-struct recipe (BOTH `frontend_requirements` + `output_labels`) | A3 gate-precision | `regen.rs:17-18` + `regen_css.rs:47-52` | S-P3 recipe pin | full-row collapse incl. BOTH nests (full-row `PartialEq`, +1-line derive) | MEDIUM (NEW) |
| F-A3.6 metalang leak | A1/regen | `json/generated.rs` 7× parse_w11_1 | P5 source-fix | `metalang_leak_present==false` | MEDIUM |

**One NEW residual finding (F-A3.5)** beyond the V3-found set: the `runtime_target_rows_collapsed`
close-gate is SOUND by-exclusion, but its implementation recipe must recurse into the nested
`output_labels` struct (the 3 prose-named fields are nested, not top-level) — pinned to S-P3
so the gate-consumer cannot author a shallow-compare false-green. All other a3 residuals are
the V3-found set verified STILL LIVE with the addenda gates bound correctly.
