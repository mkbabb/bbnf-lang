# SK-V18 SPEC — PLAN PA: The PRUNE Cluster (P1–P5) Wave Specs

Date: 2026-06-01. Pass: S-P3 (synthesis-PLAN) of SK-V18, the GENERALIZATION cycle. This
file is the binding wave manifest for the PRUNE cluster — the SPEC section that turns the
CONVERGED S-P2 candidate shortlist into executable wave specs for P1–P5. It is NOT an
implementation dispatch. It folds the S-P0 audit (`SYNTHESIS-AUDIT-OVERFIT.md` §4 PRUNE-list
+ §5 sequencing + the 6 addenda §1), the A2 prune-sequencing axis
(`audit-overfit/a2-prune-sequencing.md` §1–§5), and the S-P2 research synthesis
(`research/p2/SYNTHESIS-RESEARCH.md` §3 entry-gates) into the PRUNE-cluster wave manifest.
Live audit HEAD `83b66db42`; every path:line below re-grepped on disk this pass (results in
§PA.0 disk-truth ledger). Host: aarch64 / Apple M5 Max ONLY.

Authority (the binding inputs this section consumes):

- `restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (§4 PRUNE-list
  P1–P5; §5 sequencing constraints; §1 the 6 binding addenda; §2 residual census R8/R13/R4/R9/R15).
- `restart/skinny/tranches/sk-v18/audit-overfit/a2-prune-sequencing.md` (§1 disk-verified
  PRUNE-list; §3 build-soundness coupling P1↔checkasm_parity; §4a relocated-overfit-seam +
  R16 recipe-pin; §7 addenda→prune binding).
- `restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md` (§3 per-wave entry-gates
  for the P-cluster; §2 coupling lattice; the falsifier framing).
- `restart/skinny/tranches/sk-v18/research/p1/SYNTHESIS-PROFILE.md` (the hot leaves the prune
  must not disturb: CSS 94.1% scalar scan, JSON 91.5% SinkOnly leaves; the warm-bench did NOT
  produce the headline numbers — R13/P2 is pure contrivance deletion).
- `restart/skinny/tranches/sk-v17/SPEC.md` (the structure template: Section-2 wave manifest,
  per-wave entry/exit/falsifier/revert idiom, the Lock-14 gate).

Dispatch lock:

- No SK-V18 implementation wave dispatches from S-P3 itself.
- The P-cluster (P1–P5) lands FIRST in the standing order — it carries ZERO generalization
  risk (pure deletion + gate-tightening, zero >SOTA-bearing code removed) and reduces the
  surface every GENERALIZE wave touches. It is the natural standing-order predecessor
  (`[refactor-first-order]`: prune → optimize → grammar/semantic).
- P1, P2, P3, P5 have NO entry-gate (independent, dispatchable as soon as the P-cluster
  triumvirate is dispatched). P4 has no entry-gate either, but a hard EXIT obligation: it
  MUST land BEFORE G2/G3 (the emitter rebuild), so the new emitter is neutrality-scanned AS
  it is authored. P1–P5 are mutually independent within the cluster (no intra-cluster edge),
  so they MAY land in parallel commits provided no two race a shared file (`[agent-orchestration]`:
  commit before parallelizing; P4 touches `lock14_baseline.rs`, P2 touches `nonjson_css_l4.rs`,
  P5 touches `json_sink_direct.rs`/`json/generated.rs`, P1 touches `bbnf-simd/`, P3 touches
  `xtask/regen*.rs` + the 7 `css_l4_*/generated.rs` — disjoint roots, parallel-safe).

## Section PA.0 — Disk-Truth Ledger (re-grepped at HEAD `83b66db42`, this pass)

Every PRUNE falsifier keys on a LIVE witness re-verified on disk this S-P3 pass (a downstream
wave cannot paper-close on a stale citation):

| Prune | Witness (re-grepped this pass) | Disk result |
|---|---|---|
| P1 | `find crates/bbnf-simd/src/x86_64 crates/bbnf-simd/ext/x86 -type f` | **28** files (24 + 4) |
| P1 | `grep -c x86_64 crates/bbnf-simd/tests/checkasm_parity.rs` | **11** tokens (9 active compile-coupled call sites) |
| P2 | `grep -c 'measure_mbps\|lightningcss_facts' crates/bbnf-bench/src/nonjson_css_l4.rs` | **48** |
| P3 | `md5 crates/runtime/src/grammars/css_l4_*/generated.rs \| sort \| uniq -c` | **7 ×** `b654562ccff46ed62dd48e9ace325830` (byte-identical) |
| P3/R16 | `sed -n '5p' xtask/src/regen.rs` | `#[derive(Clone, Copy, Debug)]` (NO `PartialEq`); nested `frontend_requirements` (struct-line 17) + `output_labels` (struct-line 18); both nested structs derive `PartialEq, Eq` at `grammar_provider.rs:45`/`:91` |
| P4 | `grep -n … crates/bbnf-bench/src/lock14_baseline.rs` | `GENERIC_SCAN_ROOTS:2409`, `FORBIDDEN_GENERIC_TOKENS:2420`, `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442`, `("…/src/x86_64","diagnostic-x86"):2463`, `accepts_current_allowlist:2729` |
| P5 | `grep -c parse_w11_1_number crates/runtime/src/grammars/json/generated.rs` | **7** |

PRUNE net LOC ≈ **−10800** (deletes far more than the whole SK-V18 campaign adds). Per
`[generated-size-budget]` this is a *reduction* — no overflow risk.

## Section PA.1 — P1: DELETE the WHOLE x86 surface crate-wide (aarch64-only)

Residual: **R8** (x86 two surfaces, wrong-arch). Addendum axis: A6 (pre-restart recurrence).
The single most consequential PRUNE item and the ONLY one with a build-soundness coupling.

Owner paths:

- `skinny/crates/bbnf-simd/src/x86_64/` (24 files — DELETE)
- `skinny/crates/bbnf-simd/ext/x86/` (4 files: `bbnf.asm`, `x86util.asm`, `x86inc.asm`,
  `LICENSE-VENDOR`, ~3554 LOC vendored x264/FFmpeg ASM — DELETE)
- `skinny/crates/bbnf-simd/build.rs` (102-LOC nasm-rs x86-assembler driver — DELETE)
- `skinny/crates/bbnf-simd/Cargo.toml:19` (`nasm-rs = "0.3"` build-dep + `:14-16` comments — DELETE)
- `skinny/crates/bbnf-simd/src/lib.rs:5` (`pub mod x86_64;`), `:247` (doc ref to `ext/x86/bbnf.asm`),
  `:285-288` (`#[cfg(target_arch="x86_64")]` dispatch arms — DELETE)
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` (the 9 ACTIVE compile-coupled
  `bbnf_simd::x86_64::…::*_scalar(…)` call sites at `:458,:464,:467,:477,:478,:484,:493,:497,:502`
  + the `#[ignore]` x86 parity harness — **DECOUPLE-OR-DELETE**, retaining the aarch64 parity
  assertions)
- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:10,12,15` (x86 doc strings
  "AVX-512 BW"/"AVX2" — CLEAN to aarch64-neutral)
- `skinny/crates/bbnf-simd/{CONCRETIZATION-REPORT.md,CHECKASM-REPORT.md}` (x86 narrative — CLEAN)

Entry gate: **none** (pure deletion, zero generalization risk). Dispatchable as soon as the
P-cluster triumvirate is dispatched.

Tasks:

1. DELETE both x86 surfaces (`src/x86_64/`, `ext/x86/`) + the nasm driver (`build.rs`) +
   the nasm build-dep (`Cargo.toml`) + the `pub mod x86_64;` declaration + the
   `#[cfg(target_arch="x86_64")]` dispatch arms.
2. DECOUPLE the `checkasm_parity.rs` x86_64 reference block (the 9 active `*_scalar(…)` call
   sites + the `#[ignore]` x86 harness) IN THE SAME COMMIT as the `src/x86_64/` deletion —
   retain the 12 aarch64 single-kernel differential harnesses + `checkasm_common.rs` +
   `parity.rs` untouched (checkasm count = 14 = 12 single-kernel + 2 aggregate; the
   scalar-ref-as-spec discipline is KEPT).
3. CLEAN the residual x86 doc strings (`byte_class_from_eq_set_64.rs:10,12,15`) + the
   x86 narrative in the two report MDs to aarch64-neutral.

Exit-gate falsifier (the concrete grep/test that turns RED):

- `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f` == **0**
  (today: 28 → RED if any survives).
- `grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm'
  skinny/crates/bbnf-simd/` → only aarch64-neutral comments; **every active hit on the
  removal list** (today: 186 lines → must collapse to the neutral-comment floor; a non-neutral
  hit turns RED).
- **BUILD-SOUNDNESS (the build-blocking falsifier):** `cargo build` AND `cargo test --no-run`
  clean. The 9 `checkasm_parity.rs` x86_64 call sites resolve into `src/x86_64/`; deleting the
  directory WITHOUT the same-commit decouple BREAKS the build (the test crate fails to compile
  against the deleted paths) — `cargo test --no-run` is RED on any intermediate
  directory-deleted/parity-un-decoupled state.
- Telemetry: `x86_tree_deleted == true`.

LOC delta estimate: **≈ −4500** (24 `src/x86_64/` files + 4 `ext/x86/` files incl. ~3554 LOC
vendored ASM + 102-LOC `build.rs` + the nasm dep + the dispatch/doc reach + the checkasm
decouple). Pure deletion (the checkasm decouple is a small net negative; the aarch64 asserts
are retained).

Impl/redress cap: **30 min** (`[dispatch-hard-cap]` redress default; "at 0.9N commit, at N
halt"). LOC budget: 0 source-add; ≈ −4500 deletion; the checkasm decouple counts ≤ 30 edited
LOC.

Binding sequencing note (AUDIT §4 P1 + a2 §3 + §2 edge 1a): **P1 ↔ `checkasm_parity.rs` is a
build-soundness coupling, NOT a one-line `rm -rf`** — the deletion list MUST be reach-matched
to the verify grep (a deletion list narrower than the grep ships a RED-by-construction gate,
the exact mirror of the V5 reach finding), and the decouple MUST land in the SAME commit/wave
as the `src/x86_64/` deletion (an intermediate commit with `src/x86_64/` deleted and
`checkasm_parity.rs` un-decoupled is a broken-build state). P1 reduces the surface BEFORE
G5/G6 touch the SIMD dispatch (no point wiring NEON next to a 24-file x86 stub tree); the
single-arch kernel surface P1 leaves is the R-F retarget target for G6. P1 is INDEPENDENT
within the cluster (no entry-gate, parallel-safe with P2/P3/P4/P5 — disjoint file roots).

## Section PA.2 — P2: DELETE the warm micro-fixture CSS bench

Residual: **R13** (warm micro-fixture CSS bench). Addendum: **5** (timed-plane-symmetry +
corpus-in-timer). The warm path hits THREE addenda at once (timed-plane-asymmetry +
corpus-out-of-the-timer + more-work-competitor) but **did NOT produce the headline numbers**
(those came from the canonical `css_canon_bench`, S-P1 §0) — so this is pure contrivance
deletion, zero >SOTA risk.

Owner paths:

- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` (DELETE `measure_mbps`/`*_lightningcss_facts`
  — 48 grep hits incl. `lightningcss_facts:528`, `measure_mbps:3091` — + the per-fixture SHA256
  / byte scaffolding over the 85–357-byte SHA256-pinned micro-fixtures)
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs` (the bench harness entry)
- KEEP `css_canon_bench.rs` (the cold/real-corpus harness) + `w2_rich_cssom_bench.rs` + the
  9-field `assert_rich_strict_equality` oracle (`nonjson_css_l4.rs:451`, the ONE honest
  artefact extracted before the file is gutted)

Entry gate: **none** (independent deletion). Dispatchable immediately.

Tasks:

1. EXTRACT the 9-field `assert_rich_strict_equality` oracle (`nonjson_css_l4.rs:451`) to a
   retained location BEFORE gutting the file (it is the honest correctness anchor; do not
   delete it with the warm path).
2. DELETE the warm `measure_mbps` / `*_lightningcss_facts` machinery + the per-fixture SHA256
   / byte scaffolding + the micro-fixture corpus.
3. Confirm `css_canon_bench` (cold, N≥50, real corpus, no broadcast) remains the sole CSS
   throughput harness.

Exit-gate falsifier:

- `grep -c 'measure_mbps\|lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
  == **0** (today: 48 → RED if any survives).
- `css_canon_bench.rs` present + green (a deletion that also removes the canonical harness is
  RED).
- The extracted 9-field oracle still asserts (its absence after the gut is RED).
- Telemetry: `corpus_in_timer == true`.

LOC delta estimate: **≈ −700** (the warm-bench machinery + SHA256 scaffolding + micro-fixtures;
+ a small retained-oracle relocation).

Impl/redress cap: **30 min** (redress default).

Binding sequencing note (AUDIT §4 P2 + a2 §1 P2 + §7): P2 leaves only the cold/real-corpus
harness, so the R-F (G6) speedup measurement + H1 framing measure honestly — the warm path
would otherwise let a Mbps figure escape the symmetric corpus-in-timer plane one wave too
early. P2 is INDEPENDENT within the cluster (no entry-gate, parallel-safe). **R14/H1
disclosure is NOT P2's job** — P2 deletes the warm path; the lazy-rich-vs-eager-CSSOM framing
honesty is disclosed at H1 (the S-P1 §0 framing: full-value-materialization lazy-rich vs the
lightningcss full CSSOM).

## Section PA.3 — P3: COLLAPSE the 7 byte-identical css_l4 replicas + RuntimeTarget row-collapse

Residual: **R4** (7 byte-identical css_l4 replicas). Addendum: **2** (distinct-grammar-output,
a 3-co-gate CONJUNCTION). This PRUNE item carries the **relocated-overfit-seam structural
gate** (the one residual-overfit risk surviving into GENERALIZE) and the **R16 recipe-pin**.

Owner paths:

- `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` (7 files, all md5 `b654562c…`,
  910 LOC each — COLLAPSE to ONE CSS `generated.rs`, OR N non-identical files each from a
  distinct `.bbnf` root IFF genuinely differentiated)
- `skinny/xtask/src/regen.rs` (the `RuntimeTarget` table; ADD `PartialEq` to the `:5` derive —
  the R16 recipe-pin; collapse the 7 css_l4 `RuntimeTarget` rows to ONE config per `grammar_name`)
- `skinny/xtask/src/regen_css.rs:45,63,81,99,117,135,153` (the 7 `profile`-distinct rows;
  `frontend_requirements == REQUEST_FACTS_REQUIREMENTS` across all 7 today, `:47…155`)
- the distinct `.bbnf` roots IFF the differentiate-branch is taken (`color.bbnf`/`media.bbnf`/
  `selectors.bbnf` exist)

Entry gate: **none** (independent of P1/P2/P4/P5). Dispatchable immediately. **The
collapse-vs-differentiate decision is bound HERE (S-P3), not deferred** (resolving R-A0-2):
the DISK EVIDENCE is collapse-to-one (one `stylesheet.bbnf`, byte-identical output;
`generator_grammar_count == 3` = json + css + sheets, NOT json + 7-css + sheets) — so the
default is COLLAPSE-TO-ONE. **Manufacturing 7 fake `.bbnf` roots to satisfy a distinctness
gate is the EXACT overfit addendum 2 forbids** (a0 §5); differentiate ONLY where the profiles
are genuinely distinct grammars, NEVER erase a real `profile` discriminator, and NEVER mint a
fake root.

Tasks:

1. COLLAPSE the 7 byte-identical `css_l4_*/generated.rs` to ONE CSS `generated.rs` (the
   disk-evidence default), preserving the cold-bench corpus coverage.
2. ADD `PartialEq` to `RuntimeTarget`'s derive (`regen.rs:5`: `Clone, Copy, Debug` →
   `Clone, Copy, Debug, PartialEq`) — the **R16 recipe-pin** (one line; both nested structs
   already derive `PartialEq, Eq`). This is the structural mechanism for the
   `runtime_target_rows_collapsed` co-gate.
3. COLLAPSE the 7 `RuntimeTarget` css_l4 rows to ONE config per `grammar_name` (modulo the
   two generated-artefact path columns `output_dir`/`expected_files`).

Exit-gate falsifier (the 3-co-gate CONJUNCTION — addendum 2 is necessary-NOT-sufficient on
md5 alone):

- **md5-distinct** (the necessary floor): `md5 …/css_l4_*/generated.rs | sort | uniq -c` →
  NO byte-identical pair across the surviving `generated.rs` (today: 7× `b654562c…` → RED).
  `generated_md5_distinct == true`.
- **`runtime_target_rows_collapsed == true`** (the STRUCTURAL co-gate, R16): a
  `RuntimeTarget: PartialEq` FULL-ROW collapse-check over the structurally-expanded row
  inlining EVERY nested-struct field — BOTH `frontend_requirements` (field #11) AND
  `output_labels` (field #12), via the derived `PartialEq` so neither can be shallow-compared.
  Falsifier: `count(distinct config-tuple-minus-(output_dir,expected_files)) per grammar_name`
  must == 1 for `css_l4`; a per-grammar branch relocated into a neutral `RuntimeTarget`
  data-table (the relocated seam) turns this RED even though the arm-census grep is
  syntactically blind to it. A recipe that recurses into ONLY ONE nested struct (e.g.
  `output_labels` deep, `frontend_requirements` by `Option`-discriminant) is the EXACT
  shallow-compare false-green R16 names — FORBIDDEN; the `PartialEq` full-row derive covers
  both automatically.
- The co-gate also requires `generator_grammar_branch_count == 0` ∧
  `generator_grammar_type_count == 0` (the G3 surfaces) — but those are G3's exit, not P3's;
  P3 lands the structural-collapse half of the conjunction so the G3 un-fork can satisfy the
  whole.

LOC delta estimate: **≈ −5460** (6 × 910 LOC deleted replica bodies) **+1** (the `PartialEq`
derive line) **+ ~−40** (the 6 collapsed `RuntimeTarget`/`regen_css` rows). Net ≈ **−5500**.

Impl/redress cap: **30 min** (redress default).

Binding sequencing note (AUDIT §4 P3 + a2 §4a + §2-research coupling 2/7): **P3 is a dual-gate
predecessor of G2** — G2 entry-gates on BOTH G1 AND P3 (a P3 failure blocks G2 independent of
G1), because G2 re-derives the CSS scan and would re-create the replica overfit if it derived
into 7 byte-identical files. P3 also **binds to G3** (the relocated-seam structural check IS
the G3 un-fork's third close-gate surface — `runtime_target_rows_collapsed`), so P3 is the
PRUNE item that couples to a GENERALIZE item: the `RuntimeTarget: PartialEq` derive P3 lands is
the ONE structural co-gate threading R-A (un-fork) / R-B (P3 collapse) / R-E (distinct
`grammar_name="google_sheets"` row). The collapse-vs-differentiate WHICH-branch decision is
made HERE, NOT deferred to a RED-by-design B2 gate. P3 is INDEPENDENT within the cluster (no
entry-gate; parallel-safe with P1/P2/P4/P5 — but it touches `xtask/regen*.rs` which P4 also
reads, so P3 and P4 commit serially if they touch the same xtask file, else parallel).

## Section PA.4 — P4: FIX the Lock-14 green-by-exclusion gate (MUST LAND BEFORE G2/G3)

Residual: **R9** (Lock-14 green-by-exclusion gate). Addendum axis: A3 (Lock-14
generic-crate). The gate PASSES today (`accepts_current_allowlist` = 2/0) — **a green gate
over standing leaks is worse than a red one** — because `GENERIC_SCAN_ROOTS` deliberately OMITS
the codegen leak surface, routing it into a weaker check that never runs the neutrality scan,
and tags the x86 tree `"diagnostic-x86"`.

Owner paths:

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2409` (`GENERIC_SCAN_ROOTS` — MOVE the
  codegen leak surface INTO this strict scan), `:2420` (`FORBIDDEN_GENERIC_TOKENS` — EXTEND),
  `:2442` (`SKV15_W2_EXTRA_COVERAGE_ROOTS` — REMOVE the codegen surface from this weaker check),
  `:2463` (`("crates/bbnf-simd/src/x86_64","diagnostic-x86")` — DROP, x86 is gone via P1),
  `:2729` (`accepts_current_allowlist` — the asserting test)
- the moved surfaces: `crates/codegen/src/runtime_generator.rs`, `json_sink_direct.rs`,
  `json_typed_direct.rs`, `json_templates/`, `grammar_provider.rs`

Entry gate: **none** — BUT a hard EXIT obligation: **P4 MUST land BEFORE the emitter rebuild
(G1/G2/G3).** This is the load-bearing intra-cluster sequencing fact (it is an entry-gate ON
G2/G3, not a preference): the un-forked emitter must be neutrality-scanned AS it is authored,
or a grammar-named branch / const-`&str` courier could be re-introduced under a blind gate.

Tasks:

1. MOVE `runtime_generator.rs` + the JSON sink/typed/template surfaces from the weak
   `SKV15_W2_EXTRA_COVERAGE_ROOTS` (`:2442`) INTO strict `GENERIC_SCAN_ROOTS` (`:2409`).
2. EXTEND `FORBIDDEN_GENERIC_TOKENS` (`:2420`) with the template-const patterns
   `{CSS_, _RS, EventGrammar, *EventGrammar}` — catching the JSON `_RS` couriers G1 retires,
   the CSS `CSS_GENERATED_RS` courier G2 retires, AND any `EventGrammar` literal G3 would emit
   into the generated runtime (Sheets is the FIRST grammar to exercise the witness-emission
   coupling; see the HANDOFF invariant-5 note below).
3. DROP the `"diagnostic-x86"` exclusion (`:2463`) — x86 is deleted by P1.

Exit-gate falsifier (the gate must be MEANINGFUL, not merely green):

- **The re-inject falsifier** (proves coverage): re-inject a `JsonSink`/`CSS_…_RS` token into
  `runtime_generator.rs` → `accepts_current_allowlist` turns **RED**, then revert. A gate that
  stays GREEN on the re-injected token has NOT achieved coverage (the green-by-exclusion bug
  persists). `lock14_gate_scans_codegen == true`.
- `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_, _RS, EventGrammar, *EventGrammar}` (a missing token is RED).
- `GENERIC_SCAN_ROOTS` contains the moved codegen surfaces; `SKV15_W2_EXTRA_COVERAGE_ROOTS` no
  longer routes them through the weaker check; the `diagnostic-x86` entry is gone.
- `accepts_current_allowlist` GREEN **and meaningful** (green AFTER the re-inject/revert proof,
  not green-by-exclusion).

LOC delta estimate: **≈ +15** (root-list moves + token-list extension + the `diagnostic-x86`
deletion; this is gate-tightening, a small net positive — the one P-item that is not pure
deletion).

Impl/redress cap: **30 min** (redress default).

Binding sequencing note (AUDIT §4 P4 + §5 fact 2 + a2 §2 edge 1c + §2-research coupling 6):
**P4 MUST land BEFORE G2/G3** — this is the single load-bearing intra-cluster ordering
constraint, an entry-gate on the emitter waves, not a preference. The
`FORBIDDEN_GENERIC_TOKENS` extension is the cross-cutting predecessor of every emitter wave: it
catches G1's retired JSON `_RS` couriers, G2's retired CSS courier, and any Sheets
`EventGrammar` literal G3 would emit. **Witness-emission scan-root coupling (HANDOFF invariant
5):** the `JsonEventGrammar`/`SheetsEventGrammar` witnesses live in `runtime/` (NOT the P4
codegen scan root); IF the un-forked generator EMITS a grammar-named `EventGrammar` literal
into the generated runtime, the `runtime_generator.rs`-scoped `FORBIDDEN_GENERIC_TOKENS` must
catch it at its emit site — which is why `EventGrammar`/`*EventGrammar` is in the token-set even
though the witness definitions are not in scope. P4 touches `lock14_baseline.rs` (disjoint from
P1/P2/P5; shares only the conceptual x86 reference with P1's `diagnostic-x86` row — P1 deletes
the tree, P4 deletes the exclusion entry; commit P1 first OR same-wave so the dropped exclusion
does not dangle on a still-present tree).

## Section PA.5 — P5: PURGE the metalang bench-wave-id leak

Residual: **R15** (metalang leak `parse_w11_1_number` ×7). Addendum axis: A1/regen
(`[clean-regen-discipline]`, `[no-metalanguage-docs]`). The bench wave-id `parse_w11_1_number`
is baked into the SHIPPED `runtime/src/grammars/json/generated.rs` — a regen-discipline
violation (the shipped runtime carries a conversation/wave tag).

Owner paths:

- `skinny/crates/codegen/src/json_sink_direct.rs` (the GENERATOR/TEMPLATE source — fix HERE,
  not the shipped file, so `regen --check` stays clean)
- `skinny/crates/runtime/src/grammars/json/generated.rs:801,841,881,955,1007,1019,1031` (the 7
  shipped `parse_w11_1_number_*` symbols — regenerated clean as `parse_number_*` once the
  template source is fixed; NEVER hand-patched, `[clean-regen-discipline]`)

Entry gate: **none** (independent deletion/rename). Dispatchable immediately.

Tasks:

1. RENAME `parse_w11_1_number_*` → `parse_number_*` AT THE GENERATOR/TEMPLATE SOURCE
   (`json_sink_direct.rs`) — never hand-patch the generated file.
2. Regenerate `json/generated.rs` so the shipped runtime carries no `w[0-9]+`/corpus/`sk_v`
   wave tag.

Exit-gate falsifier:

- `grep -c parse_w11_1_number skinny/crates/runtime/src/grammars/json/generated.rs` == **0**
  (today: 7 → RED if any survives).
- No `w[0-9]+` / corpus-name / `sk_v` tag in the shipped runtime (a surviving metalang tag is RED).
- `cargo xtask regen --check` clean (a hand-patch that diverges from fresh generator output
  fails `regen --check` → RED; this is the proof the fix landed at the SOURCE, not the artefact).
- Telemetry: `metalang_leak_present == false`.

LOC delta estimate: **≈ 0** (rename-only at the template source; the regenerated file is a
1:1 symbol rename, no net line change).

Impl/redress cap: **30 min** (redress default).

Binding sequencing note (AUDIT §4 P5 + a2 §1 P5 + §2 edge 1a): P5 purges the metalang leak
BEFORE G1 regenerates JSON (G1's `regen --check` must be clean — a surviving `parse_w11_1_number`
would fail G1's clean-regen gate). The fix MUST be at the generator/template source, not the
shipped artefact — a hand-patch to the generated file is itself a `[clean-regen-discipline]`
violation that `regen --check` catches. P5 is INDEPENDENT within the cluster (no entry-gate;
parallel-safe — touches `json_sink_direct.rs`/`json/generated.rs`, disjoint from P1/P2/P3/P4).

## Section PA.6 — P-Cluster Wave Manifest (the binding summary table)

| Wave | Residual | Addendum | Entry gate | Exit falsifier (turns RED) | LOC Δ | Cap | Sequencing |
|---|---|---|---|---|---:|---:|---|
| **P1** | R8 x86 two surfaces | A6 | none | `find …/x86_64 …/ext/x86 = 0` ∧ crate-wide aarch64-neutral grep ∧ `cargo build`/`cargo test --no-run` clean | ≈ −4500 | 30 min | checkasm decouple SAME COMMIT; before G5/G6 |
| **P2** | R13 warm CSS bench | 5 | none | `grep measure_mbps\|lightningcss_facts = 0` ∧ `css_canon_bench` green ∧ 9-field oracle retained | ≈ −700 | 30 min | leaves cold harness for G6/H1; INDEPENDENT |
| **P3** | R4 7 replicas | 2 | none | md5-distinct ∧ `runtime_target_rows_collapsed == true` (`RuntimeTarget: PartialEq` full-row, BOTH nested structs) | ≈ −5500 | 30 min | DUAL-gates G2; binds G3 row-collapse; WHICH-branch decided HERE |
| **P4** | R9 green-by-exclusion | A3 | none | re-inject `JsonSink` → RED, revert; `FORBIDDEN ⊇ {CSS_,_RS,EventGrammar,*EventGrammar}`; `lock14_gate_scans_codegen == true` | ≈ +15 | 30 min | **MUST land BEFORE G2/G3** (entry-gate on emitter waves) |
| **P5** | R15 metalang leak | A1/regen | none | `grep -c parse_w11_1_number = 0` ∧ no `w[0-9]+`/`sk_v` tag ∧ `regen --check` clean | ≈ 0 | 30 min | fix at SOURCE; before G1 regen; INDEPENDENT |

P-cluster net LOC ≈ **−10800**. The P-cluster lands FIRST in the standing order
(PRUNE → G1..G6 → PROVE → H1, AUDIT §5). P1/P2/P3/P5 are independent; P4 carries the one hard
ordering obligation (BEFORE G2/G3). The G-wave entry-gates that consume the P-cluster: **G1**
entry = P-cluster closed (P4 live); **G2** entry = G1 ∧ **P3** closed ∧ P4 live (dual gate);
**G3** entry = G1 ∧ G2 closed ∧ P4 live ∧ **P3** row-collapse; **G5/G6** entry = **P1** ∧ **P3**
∧ G3 closed ∧ the S-P1 94.1% hot-leaf measurement.

## Section PA.7 — Telemetry Columns The P-Cluster Emits (consumed by `--skv18-generalization-report`)

The P-cluster's exit gates emit these columns; the `gate-json --skv18-generalization-report`
consumer REJECTs on each (every emitted field is consumed in the same wave —
`[typed-materialization-invariant]`; a producer-only field fails the wave):

```text
x86_tree_deleted                  (P1; true)
corpus_in_timer                   (P2; true — warm path gone)
generated_md5_distinct            (P3; true — no byte-identical pair)
runtime_target_rows_collapsed     (P3; true — RuntimeTarget: PartialEq full-row, BOTH nested structs)
lock14_gate_scans_codegen         (P4; true — re-inject falsifier proves coverage)
metalang_leak_present             (P5; false)
```

The `runtime_target_rows_collapsed` column is the structural co-gate the R16 recipe-pin binds:
the gate-consumer author MUST compute it over the structurally-expanded row inlining EVERY
nested-struct field — BOTH `frontend_requirements` AND `output_labels` — via the
`RuntimeTarget: PartialEq` full-row derive (NOT a hand-rolled prose-field list, which risks a
shallow-compare false-green of either nested struct). This is the ONLY check that catches the
relocated seam (a per-grammar branch moved into a neutral data-table) that the arm-census grep
is syntactically incapable of seeing.

## Section PA.8 — CH7 Overfit-Prune Lens (carried into the P-cluster CHALLENGE)

The CH7 lens is binding on the P-cluster CHALLENGE (AUDIT §6): every prune is pure deletion +
gate-tightening (zero generalization risk, zero >SOTA-bearing code removed — the warm bench
DID NOT produce the headline numbers, S-P1 §0); no prune may relabel a deletion as a
generalization; the falsifiers are concrete greps/tests (not prose); P4 makes the Lock-14 gate
MEANINGFUL (the re-inject proof, not a green assertion); P3's collapse-vs-differentiate WHICH
is decided in-plan (no fake `.bbnf` roots minted to satisfy a distinctness gate — the EXACT
overfit addendum 2 forbids); and the R16 recipe-pin (`RuntimeTarget: PartialEq` full-row,
covering BOTH nested structs) is the structural co-gate threading P3→G3→PROVE. A prune that
ships a RED-by-construction gate (a deletion list narrower than its verify grep — the P1
reach-match hazard) is a REJECT.
