# SK-V18 S-P3 / PD — G5/G6 (neutral scan) + PROVE (Sheets) + H1 (honesty) wave specs

This file is the binding SPEC section for the TAIL of the SK-V18 generalization lattice — the
three waves downstream of the emitter rebuild: **G5/G6** (neutral scan retarget), **PROVE**
(Sheets third-grammar proof, PARALLEL to G5/G6), and **H1** (CSS framing honesty + regen-clean
close). It is the sibling of `pa-prune-waves.md` (PRUNE P1–P5), `pb-g1-g2-waves.md` (JSON
projection + CSS lowering), `pc-g3-g4-waves.md` (un-fork emitter + shared trait), and consumes
the gate/telemetry binding authored in `pe-gate-telemetry-close.md`. It is NOT an implementation
dispatch; it is the executable plan. Authority: `research/p2/SYNTHESIS-RESEARCH.md` §3 (the
binding sequencing + per-wave entry-gate), §1 R-E/R-F (the recommended candidates), §4 (the §6
findings), §5 (the residual risks); `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1 addenda 5/6 +
§2.1 + §5 sequencing facts 4/5 + R16; `research/p1/SYNTHESIS-PROFILE.md` §2/§3 (the hot leaves,
G6=WIRE, json/scan.rs zero-sampled); `research/p2/rE-sheets-proof.md` + `rF-neutral-scan.md`
(the class digests). Structure mirrors `sk-v17/SPEC.md` (Section 0 outcome-enum/telemetry, then
per-wave entry-gate / tasks / exit-gate / falsifiers / caps / revert). Host: aarch64 / Apple
M5 Max ONLY — x86 is a P1 prune target, out of scope for every kernel below; NO x86, NO AVX-512,
NO SVE, NO i8mm-without-antecedent.

## §0 — Outcome enum, comparator plane, and shared telemetry (the binding header for PD)

### §0.1 — Outcome enum (carried from `sk-v17/SPEC.md §0.3`, SK-V18-scoped)

```text
A    admit-shaped (generalization claim crosses its gate)
C    correctness (parity/equality held)
G    GO
L    loss / REDRESS (gate RED, downstream blocked)
N    not-applicable / not-needed (e.g. G5 json/scan.rs retire is N-shaped: nothing on product path)
S    substrate-guard / honest-residual (measured, recorded, NOT paper-closed)
```

`A`/`C`/`L`/`N`/`S` are valid PD outcomes. A NEON speedup CLAIM is `A` ONLY when the timed-plane
binding (§G6 exit) holds; a checkasm PASS without the corpus-in-timer figure is `C` (correctness
proven, speedup deferred to H1) — NEVER `A`. A Sheets emission that needs a shim is `L` (surface
honestly), never `S`. The §6 honest-finding for a §6-fired primitive is `S` only when (a)-(d)-gated.

### §0.2 — Comparator plane (the only honest >SOTA bar; carried from `sk-v17/SPEC.md §0.2`)

The CSS >SOTA bar is lightningcss full-CSSOM, same-run re-baselined, N≥50 cold, MEDIAN,
corpus-in-timer. The P2-survivor `css_canon_bench` is the SOLE measurement plane (the warm
micro-fixture `measure_mbps`/`*_lightningcss_facts` path is DELETED in P2 — `pa-prune-waves.md`).
Any Mbps figure G6 or H1 emits MUST come from this symmetric, cold, corpus-in-timer harness
(both sides do equal work on the REAL 71KB–495KB corpus). The checkasm differential is a
CORRECTNESS plane ONLY — it never produces a speedup figure. JSON's >sonic-rs-strict position is
carried forward from the W0 lock; this PD does not re-bench JSON throughput (its product path is
scan-free — `SYNTHESIS-PROFILE §2`), only re-confirms the 51/51 guard within ±1.0%.

### §0.3 — Shared PD telemetry (every column consumed by `gate-json --skv18-generalization-report`)

The full schema is bound in `pe-gate-telemetry-close.md`; the PD-specific columns the three
waves below produce and the gate REJECTs on (producer-only fields fail the wave —
typed-materialization-invariant):

```text
# G5/G6 (neutral scan)
acceleration_at_admission                  (admission | dead — caller census over grammars/*/generated.rs, NOT #[cfg(test)])
simd_admission_caller                      (the runtime_simd::find_… call site in grammars/*/generated.rs; empty == FAIL)
neon_significant_skip_matches_scalar       (PASS | FAIL — guard over the REAL 71KB-495KB corpora, not micro-cases)
checkasm_differential                      (PASS | FAIL — correctness plane; pre-H1 PASS/FAIL only)
css_scan_call_site_singular                (true — exactly ONE generated call site post-P3; re-emit-7-ways == FAIL)
significant_set_is_caller_data             (true — generator emits the CALL; kernel hand-authored once in bbnf-simd)
g6_speedup_median_mbps                     (corpus-in-timer ONLY; null pre-H1 — deferred to the H1 symmetric timer)
json_scan_rs_neutralized                   (retired | neutralized — the zero-sampled json/scan.rs; G5)
json_guard_held                            (51/51 within ±1.0% of SK-V18-open)
# PROVE (Sheets)
generated_md5_distinct                     (true — md5 over grammars/{json,sheets,css_l4}/generated.rs all distinct)
sheets_verbatim_blob_present               (false — grep -c 'const.*_RS.*r#' for any Sheets blob == 0)
sheets_grammar_shape                        (pratt-operator — NOT flat-stream/tree)
generator_grammar_branch_count             (0 — no GoogleSheets => arm)
generator_grammar_type_count               (0 — no GoogleSheetsParser/SheetsEventGrammar literal)
generator_grammar_count                    (3 — json + css_l4 + google_sheets)
runtime_target_rows_collapsed              (true — RuntimeTarget: PartialEq full-row, incl. BOTH nested structs)
sheets_value_instantiates_g4_trait         (true — the Sheets value type instantiates the R-D Cursor/DocumentView seam)
import_closure_relaxation_is_data          (true — present-iff-grammar-has-imports from facts, NOT a match-grammar arm)
sheets_emission_path                       (generator-only | shim — shim == L, generalization NOT real)
# H1 (honesty)
materialization_framing                    (lazy-rich-vs-eager-cssom — disclosed explicitly)
corpus_in_timer                            (true)
regen_check_clean                          (true — cargo xtask regen --check exit 0)
```

### §0.4 — Hard caps (standing `[dispatch-hard-cap]`, per `SYNTHESIS-RESEARCH §3`)

Research 20 / plan 15 / redress 30 min; "at 0.9N commit, at N halt". The Sheets/NEON cluster
(PROVE/G6) is MED-HIGH and carries a documented larger redress cap of **45 min** (the un-forked
emitter generality stress + the retarget parity surface). G5 (json/scan.rs neutralize) is LOW
and folds into the G6 commit. H1 is LOW (documentation + symmetric re-measure + regen-check),
standard 30-min redress cap.

---

## §1 — G5/G6: neutral scan retarget (R-F Candidate A — inner-skip vectorize)

**Lever:** retarget the existing checkasm-gated `bbnf-simd` kernel
(`byte_class_from_eq_set_64` / `find_ascii_set_member64`, `bbnf-simd/src/lib.rs:209`) onto the
scalar recursive shell of `find_component_delim` (CSS hot leaf, 79.5% alone / 94.1% with
`consume_balanced_at`). **WIRE** per `SYNTHESIS-PROFILE §3` (94.1% ≫ ~8% wire threshold).
**G5 = neutralize/retire the zero-sampled `json/scan.rs`** (cheap, no JSON classifier authored;
JSON product path is scan-free, `SYNTHESIS-PROFILE §2`). Candidate A is a RETARGET, not a
wire-as-is — the dead `find_css_significant`/`find_comment_close` (R7) were written for a
flatter function and do NOT cover the recursive hot path (`rF §0.2`). Candidate B
(balanced-consume bitmap) is the DOCUMENTED upgrade path (record in this SPEC, build only if a
post-A measurement shows the `consume_balanced_at` 14.6% tail dominates); Candidate C
(table-classifier unify) is REJECTED (lo6-collision hazard + JSON↔CSS coupling).

### Entry gate (the binding predicate, GREEN before dispatch)

Per `SYNTHESIS-RESEARCH §3` G5/G6 row: **P1 ∧ P3 ∧ G3 closed ∧ the S-P1 94.1% hot-leaf
measurement** (no orphan kernel). Concretely:

- **P1 closed** — the x86 surface is gone crate-wide (`find …/src/x86_64 …/ext/x86 -type f = 0`);
  the `bbnf-simd` kernel surface is single-arch when R-F retargets (`rF §4`). A live x86 arm
  blocks G5/G6 (the retarget must be aarch64-only).
- **P3 closed** — the 7 byte-identical `css_l4_*/generated.rs` replicas are collapsed to ONE CSS
  scan, AND `runtime_target_rows_collapsed == true` (R16 full-row `PartialEq`). The retargeted
  call site MUST land into the P3-COLLAPSED single CSS scan — re-emitting per-replica re-forks
  the shape G3 un-forks (`rF §4`, the load-bearing sequencing fact). A P3 failure blocks G6
  independent of G3.
- **G3 closed** — the un-forked grammar-agnostic emitter exists (`emit_shape_source ==
  lowered_program`, `emitter_fork_present == false`). The `runtime_simd` CALL must be emitted by
  the single un-forked emitter, not a CSS-family fork — else G6 re-forks the shape it polices.
- **S-P1 profile present** — the 94.1% / 79.5% hot-leaf measurement (`SYNTHESIS-PROFILE §3`) is
  the standing mandate for the WIRE branch (sequencing fact 4). No kernel lands without a
  profile-anchored hot leaf.
- **CHALLENGE accepts** (first-of-class, primitive-touching): the kernel ALREADY EXISTS and is
  alphabet-data (the generator emits a CALL, not vector code per grammar); the significant set
  spans ≤13 bytes (>8 eq-set cap) and uses the two-fan OR-reduce SALVAGED byte-exact from the
  dead `find_css_significant:180-204`; the vector skip stops AT `([{'"/` and hands recursion
  back to the scalar shell; error positions come from the scalar shell, not the kernel.

### Tasks

1. **G5 — neutralize the zero-sampled `json/scan.rs`.** It appears ZERO times in the JSON
   profile (`SYNTHESIS-PROFILE §2`); its NEON scanner lives only on tape/`parse_only` probe
   paths. Retire it or fold it onto the shared `bbnf-simd` surface — NO bespoke JSON classifier
   authored (no orphan kernel; profile-first). `json_scan_rs_neutralized = retired|neutralized`.
2. **G6 — author the `runtime_simd` retarget entry** (a thin neutral wrapper over the existing
   `bbnf-simd::find_ascii_set_member64`/`byte_class_from_eq_set_64`), with the set-split logic
   salvaged byte-exact from the dead `find_css_significant:180-204` (two ≤8 eq-set fans
   OR-reduced for the ≤13-byte significant family). The set is CALLER DATA — CSS passes its
   delimiter set; the structural family `' " / ( [ { ) ] }` is the recognizer's own constant.
3. **G6 — swap the generated inner-skip call site.** The single P3-collapsed CSS scan's per-byte
   `_ => pos + 1` inert advance routes to the `runtime_simd` entry; the recursion / string-skip /
   comment-skip stay scalar (they are the cold <2%/14.6% tail; `consume_balanced_at`'s OWN inert
   advance reuses the SAME entry). The call site is emitted by the G3 un-forked emitter, ONCE
   (`css_scan_call_site_singular == true`). Land the consumer (`generated.rs` inner loop) and the
   `runtime_simd` entry in ONE commit (addendum 6 no-orphan law).
4. **G6 — author the dav1d scalar-reference + checkasm differential FIRST.** Scalar reference =
   the existing `find_component_delim` inner loop itself (and `significant_ref`, `lib.rs:506`);
   checkasm differential = extend `checkasm_byte_class_from_eq_set_64`; retarget the runtime
   parity guard `neon_significant_skip_matches_scalar` (`lib.rs:562`) to the recursive shell over
   the REAL 71KB–495KB corpora (NOT the micro-cases at `lib.rs:564-570`). aarch64 NEON/dotprod
   ONLY.
5. **G6 — DELETE-or-salvage the dead R7/R10/R11 kernels in the same wave.** Salvage the
   set-split into the retarget entry; retire `find_comment_close` ONLY if retargeting to the
   comment-consume proves unsafe (gated on the samply non-top-N measurement, never a bare
   assertion). No dead `#[cfg(test)]`-only NEON kernel survives.

### Exit gate (MEASURABLE)

- **`acceleration_at_admission == admission`**, PROVEN by the generated-`generated.rs` caller
  census: `rg runtime_simd::find_… skinny/crates/runtime/src/grammars/*/generated.rs` returns
  NON-EMPTY (`simd_admission_caller` is a `generated.rs` hot-loop call site, NOT a
  `#[cfg(test)]` caller). Post-G6 the L6 census target MOVES off `lib.rs:574` — a surviving
  test-only admission proof FAILS the wave (`rF §2.5`).
- **`neon_significant_skip_matches_scalar == PASS`** over the REAL 71KB–495KB corpora (the guard
  retargeted to the recursive shell; micro-case-only PASS does NOT satisfy this).
- **`checkasm_differential == PASS`** — the CORRECTNESS plane. G6 may report ONLY this PASS/FAIL
  pre-H1. The three retarget seams are covered bit-exact: (a) the ≤13-byte two-fan OR-reduce
  salvage; (b) the skip stops AT `([{'"/` (find-significant, not find-delimiter); (c) error
  positions reproduced from the scalar shell (`rF §3`).
- **Timed-plane binding (addendum 5).** Any Mbps/speedup FIGURE comes from the corpus-in-timer
  symmetric `css_canon_bench` harness (the P2-survivor cold/real-corpus plane, same plane both
  sides) and inherits §5-risk-7's QUIET-recapture caveat. **The speedup CLAIM is DEFERRED to the
  H1 symmetric timer** — `g6_speedup_median_mbps` is null pre-H1; addendum 5 is enforced in H1,
  not one wave too late. The G6 outcome is `C` (correctness) until H1 produces the figure.
- **`css_scan_call_site_singular == true`** (exactly ONE generated call site post-P3) ∧
  **`significant_set_is_caller_data == true`** (the generator emits the CALL).
- **`json_scan_rs_neutralized ∈ {retired, neutralized}`** (G5; outcome `N` — nothing on product
  path) ∧ **`json_guard_held == true`** (51/51 within ±1.0% of `SK-V18-open`).

### Falsifiers (each gate must be RED-able)

- **Caller-census falsifier:** revert the generated call-site swap → `simd_admission_caller`
  goes empty → `acceleration_at_admission == dead` → gate RED. Re-apply.
- **Parity falsifier:** mutate the salvaged two-fan OR-reduce by one byte in the significant set
  → `neon_significant_skip_matches_scalar == FAIL` over the corpora → gate RED. Revert.
- **Singular-site falsifier:** if P3 has NOT collapsed and the call is emitted 7 ways →
  `css_scan_call_site_singular == false` → gate RED (this is why G6 entry-gates on P3).
- **Orphan-kernel falsifier:** author a JSON classifier with no hot consumer → it has no profile
  anchor in `SYNTHESIS-PROFILE §2` → REJECT (no orphan kernel; G5 authors NOTHING for JSON).
- **Plane-mismatch falsifier:** emit a `g6_speedup_median_mbps` from the checkasm plane (not
  corpus-in-timer) → addendum-5 REJECT; the gate accepts the figure ONLY from `css_canon_bench`.

### Caps + revert

MED-HIGH; redress cap 45 min (G5 folds in, LOW). Revert the `runtime_simd` retarget entry +
scalar twin + checkasm extension + the generated call-site swap + the `json/scan.rs`
neutralization as ONE slice; restore `SK-V18-open` RESULTS; add REDRESS naming the seam that
failed (the ≤13-byte salvage, the skip-stop boundary, the error-position reproduction, or the
caller census).

### Downstream

G5/G6 does NOT block PROVE (Sheets does not use the CSS NEON — PARALLEL per `SYNTHESIS-RESEARCH
§3`). G5/G6 ∧ PROVE both gate H1.

---

## §2 — PROVE: Sheets via the un-forked generator ONLY (R-E Candidate R-E-2 — precedence-tower core)

**The negative control.** Generalization is REAL only if a THIRD, structurally-distinct grammar
(`grammar/google-sheets/google-sheets.bbnf`, 185 lines) emits a working parser THROUGH the
un-forked G3 generator — not JSON, not CSS, zero hand-authored runtime Rust. **R-E-2** emits the
precedence-tower CORE (the 7-level left-assoc tower `comparison→concat→add→mul→exp→unary→
postfix→primary` + cyclic `paren_expr→expression` + `Nu8` operator rules + `number`/`string`/
`boolean`/`error_literal` leaves + `func_call`'s one `<<`-separated arg list), DEFERRING the
`cell_ref`/`range`/`LET`/`LAMBDA` aggregates the grammar ITSELF leaves as raw `-> input : Span`
(TODO AU.6.7, `google-sheets.bbnf:62,73-75`). The **precedence tower is the SOLE
Sheets-distinctive construct** JSON+CSS structurally lack (`sheets_grammar_shape ==
pratt-operator`). The `Nu8`-tagged-alt family is NOT the litmus — it is SHARED: CSS L4 uses
`-> Nu8u8` **295×** across its import closure vs Sheets' 21×, so the generator must already
handle it at scale to emit CSS at all (`rE §1`). R-E-3 (flattened precedence) is REJECTED as a
hollow "third-JSON" litmus; R-E-1 (maximal) is deferred (highest authoring + regression surface).

### Entry gate

Per `SYNTHESIS-RESEARCH §3` PROVE row: **G3 ∧ G4 closed** (transitively G1 ∧ P3). PARALLEL to
G5/G6. Concretely:

- **G3 closed** — the un-forked emitter renders grammar-DERIVED bodies (`emit_shape_source ==
  lowered_program`); it can render recursive `CallRule`/`RepeatLoop` chains from grammar
  structure. Sheets is the FIRST grammar whose body CANNOT be a relabeled JSON/CSS courier
  (`rE §0.2`), so a REDRESSed G3 HALTS PROVE (sequencing fact 3).
- **G4 closed** — the shared `Cursor`/`DocumentView` seam exists with the phantom `<G>` resolved
  by DELETE; G4 is a DIRECT conjunct because the Sheets value type instantiates the R-D trait
  (the phantom-`<G>` resolution made concrete by a third impl, `SYNTHESIS-RESEARCH §2 coupling 5`).
- **Transitively G1 ∧ P3** — G1 (JSON projection is a `SinkOnlyExpr` walk, not a courier) and P3
  (the CSS replica collapse + `RuntimeTarget` row-collapse the Sheets row extends with a distinct
  `grammar_name`).
- **CHALLENGE accepts** (first-of-class, the generality stress): the tower is right-iterated EBNF
  (`A = B (op B)*`, `google-sheets.bbnf:109`) lowering to the EXISTING `SinkOnlyExpr` vocabulary
  (`Seq`+`RepeatLoop`+`Alt{Dispatch}`+`CallRule`, `lower/sink_only.rs:69-96`) — NO new IR/Pratt
  primitive; the stress is on G3's GENERALITY, not a missing construct. The `cell_ref`/`range`/
  `LET`/`LAMBDA` deferral is GROUNDED in the grammar's own TODOs, not a dodge.

### Tasks

1. **Add the Sheets grammar root + xtask target** (`.bbnf` referenced, not authored; the
   generated runtime FALLS OUT of G3). The 25-LOC `sheets_witness/` stub either becomes the
   generated output dir or is deleted (`rE §4`). ~+30 LOC skinny grammar-root + xtask; total
   Sheets adoption ~+200 LOC per the alphaE budget.
2. **Emit the precedence-tower core THROUGH the un-forked G3 generator** — `formula →
   comparison_expr → … → primary`, the `Nu8` operator rules, the leaves, `paren_expr` (the
   cyclic recursion), `func_call`. The body comes from the grammar; NO hand-authored Sheets Rust.
3. **Relax the import-closure requirement as DATA, not a branch.** Sheets has NO `@import`
   (`grep '@import' google-sheets.bbnf` → NONE), so the RequestFacts contract's
   `import_closure: true` (`grammar_provider.rs:263`) rejects it today (the honest reason
   `w5a_sheets` fails closed). The relaxation is `present-iff-grammar-has-imports`, DERIVED from
   the grammar facts — a `RuntimeFrontendRequirements` data change (`import_closure_relaxation_is_data
   == true`), NEVER a `match grammar { GoogleSheets => … }` arm.
4. **Instantiate the G4 shared trait over the Sheets value type** (`sheets_value_instantiates_g4_trait
   == true`) — the phantom-`<G>` resolution made concrete, without LCD-flattening JSON's rich nav.
5. **Add the distinct `RuntimeTarget` row** with `grammar_name = "google_sheets"` so the
   per-`grammar_name` config-tuple collapse counts a genuine THIRD grammar
   (`generator_grammar_count == 3`), not a relabeled CSS row. The row collapses to itself
   (count==1 per `grammar_name`) under the R16 full-row `PartialEq` (covering BOTH nested
   structs `frontend_requirements` AND `output_labels`).

### Exit gate (MEASURABLE)

- **`generated_md5_distinct == true`** — `md5 -q` over `grammars/{json,sheets,css_l4}/generated.rs`
  all distinct (`uniq -d` empty). A repeated md5 means a courier was reused — REJECT.
- **`sheets_verbatim_blob_present == false`** — `grep -c 'const.*_RS.*r#' codegen/src` for any
  Sheets blob == 0 (addendum 1).
- **`sheets_grammar_shape == pratt-operator`** — non-hollow by construction (NOT `flat-stream`/
  `tree`; the R-E-3 flattened-tower REJECT predicate).
- **`generator_grammar_branch_count == 0`** — the arm-census `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|
  CssL4\s*=>|(GoogleSheets|Sheets)\w*\s*=>|Bbnf\w*\s*=>' skinny/crates/codegen/src skinny/xtask/src`
  → 0 (`GoogleSheets` un-abbreviated; `Sheets\w*` does NOT match `GoogleSheets =>`). Sheets
  renders from the SAME `render(program)` path as JSON+CSS.
- **`generator_grammar_type_count == 0`** — `rg 'JsonParser|CssL4Parser|GoogleSheetsParser|
  BbnfBootstrap' …` → 0. If G3 emits a Sheets `EventGrammar` literal into the generated runtime,
  the `runtime_generator.rs`-scoped `FORBIDDEN_GENERIC_TOKENS` must carry
  `EventGrammar`/`*EventGrammar` so it is caught at the emit site (Sheets is the FIRST grammar
  that exercises this coupling, addendum §2.1 item 2).
- **`generator_grammar_count == 3`** (json + css_l4 + google_sheets) ∧
  **`runtime_target_rows_collapsed == true`** (R16 full-row `PartialEq`, both nested structs).
- **`sheets_value_instantiates_g4_trait == true`** ∧ **`import_closure_relaxation_is_data == true`**.
- **`w5a_sheets` flips** from "fails closed: missing import closure" to "emits a working parser"
  via the import-closure-derived-from-facts relaxation (a frontend-requirements data change).
- **`sheets_emission_path == generator-only`** — the BINDING fallback.

### Falsifiers + the binding fallback (the negative-control teeth, `rE §5`)

The litmus FAILS — and each failure is surfaced HONESTLY as `sheets_emission_path == shim`,
outcome `L`, NEVER paper-closed — if ANY of:
- **(a)** Sheets `generated.rs` needs a `const SHEETS_GENERATED_RS` courier
  (`sheets_verbatim_blob_present` grep ≠ 0);
- **(b)** G3 routes Sheets via a `GoogleSheets =>` arm (`generator_grammar_branch_count` ≠ 0);
- **(c)** `sheets_grammar_shape != pratt-operator` (precedence flattened);
- **(d)** the Sheets value type cannot instantiate the G4 trait without LCD-flattening JSON's
  rich nav.

**The binding fallback (SYNTHESIS-RESEARCH §5-risk-5, `rE §0/§5`):** if Sheets cannot emit via
the generator ONLY, **generalization is NOT real — surface honestly, do NOT stub-prove, do NOT
hand-write a `_GENERATED_RS` Sheets block.** If the precedence tower breaks because G3 cannot
render recursive `CallRule`/`RepeatLoop` chains from grammar structure, that becomes a §6
honest-finding (§3 below): a named, `.bbnf`-invoked, parameterized precedence primitive with a
scalar/checkasm reference — never a silent blob, never a paper-close. Per the V≤5 iteration
discipline, B1/B2 (G1/G3) iterate; PROVE does not paper-close.

### Caps + revert

MED-HIGH; redress cap 45 min (the un-forked emitter generality stress is the most likely
REDRESS site). Revert the Sheets grammar root + xtask target + the `RuntimeTarget` row + the
import-closure relaxation + the generated Sheets runtime as ONE slice; restore RESULTS; add
REDRESS naming the construct the generator could not lower (the precedence tower is the expected
break point, `rE §6`).

### Downstream

PROVE ∧ G5/G6 both gate H1.

---

## §3 — The §6 honest-finding surfaces PD must plan as gated primitives (`SYNTHESIS-RESEARCH §4`)

A fully grammar-derived parser CANNOT preserve >SOTA at three places without a hand-shaped core,
admissible ONLY as a NAMED, `.bbnf`-INVOKED, grammar-DERIVED-data, machine-(a)-(b)-(c)-(d)-gated
primitive (never a silent blob, never a paper-close). The two PD-relevant surfaces:

- **G6 — the CSS balanced delimiter scan (the PRIMARY §6 finding).** The 94.1% hot leaf is the
  G6 retarget target. The `balanced_component_scan` primitive (named in G2, `pb-g1-g2-waves.md`)
  IS the G6 NEON-retarget call site — one seam for G2+G6. **NEUTRALITY-PROOF obligation (CH6):**
  `balanced_component_scan` is named neutrally but is exercised ONLY by CSS in this campaign. Its
  inner alphabet-scan sub-kernel (the `bbnf-simd` eq-set member scan) IS genuinely neutral
  (caller-supplied byte set — the same kernel JSON's `scan_structurals` rides). But the
  balanced-recognizer SHELL must be PROVEN neutral by at least one NON-CSS invocation in this
  campaign — the JSON object/array balanced `{}`/`[]` nesting OR the Sheets `paren_expr`
  balancing must invoke the SAME primitive — ELSE it is demoted to an honestly CSS-scoped name
  (`css_balanced_component_scan`), not a false neutral. A neutrally-named CSS-only primitive is
  an overfit-in-waiting. **PD binding:** PROVE's Sheets `paren_expr` is the natural non-CSS
  invocation candidate; if neither JSON nor Sheets invokes the shell, G6/H1 renames it
  CSS-scoped (honest), not neutral.
- **PROVE — the Sheets precedence tower (a §6 CANDIDATE, not yet realized).** The 7-level tower
  lowers to the EXISTING `SinkOnlyExpr` vocabulary — so it needs NO new IR primitive; the stress
  is on G3's GENERALITY. IF G3 cannot render recursive `CallRule`/`RepeatLoop` chains from
  grammar structure, the tower breaks first = a §6 finding surfaces (a named, `.bbnf`-invoked,
  parameterized precedence primitive with a scalar/checkasm reference). This is the PROVE
  make-or-break.

Every primitive above is admissible ONLY under (a) grammar-INVOKED-by-name + (b)
emitted-output-VARIES-under-invoking-rule-mutation + (c) `verbatim_blob_present == false` + (d)
PROFILE-PROVEN-NARROW-LEAF (the primitive covers a SINGLE hot leaf; the surrounding structural
SKELETON MUST be walk-derived; a "primitive" spanning a rule's whole body or an unprofiled region
is a REJECT regardless of (a)-(c), machine-checkable as primitive LOC vs the profiled hot-leaf
extent). A primitive failing any of the four is a relabeled hand-written blob = REJECT.

---

## §4 — H1: CSS framing honesty + corpus-in-timer + regen --check clean

**The honesty close.** Lever: disclose the CSS materialization framing, bind the
corpus-in-timer symmetry, produce the deferred G6 speedup figure on the symmetric timer, and
prove the regen is clean. H1 is the LAST PD wave.

### Entry gate

Per `SYNTHESIS-RESEARCH §3` H1 row: **G5/G6 ∧ PROVE closed**.

- **G5/G6 closed** — `acceleration_at_admission == admission`, `checkasm_differential == PASS`,
  `neon_significant_skip_matches_scalar == PASS` over the real corpora.
- **PROVE closed** — `sheets_emission_path == generator-only`, `generator_grammar_count == 3`,
  the four addendum-2 co-gates GREEN.

### Tasks

1. **Disclose `materialization_framing == lazy-rich-vs-eager-cssom`** (the honest S-P1 framing).
   `track1_rich` is LAZY-rich (`css_l4_declaration_values/generated.rs:297-304`): it re-derives
   every field from `(source, offset)` spans, writing nothing to the arena ("lazy, not eager").
   The honest framing is **full-value-materialization, lazy-rich vs the lightningcss full
   CSSOM** — equal-depth typed value work, NOT a count-only structural probe. The
   materialization-depth asymmetry is disclosed EXPLICITLY: an unqualified "beats CSSOM"/
   "equal-work" claim behind the lazy-rich re-label is a REJECT (R-A0-1 / R14).
2. **Bind `corpus_in_timer == true`** — the symmetric `css_canon_bench` plane, both sides equal
   work on the REAL corpus, COLD, no micro-fixtures, no more-work-competitor (the P2-survivor
   harness).
3. **Produce the DEFERRED G6 speedup figure** on the symmetric timer (addendum 5 enforced HERE,
   not in G6). `g6_speedup_median_mbps` comes from the corpus-in-timer harness; it inherits
   §5-risk-7's QUIET-recapture caveat — the S-P1 capture ran under load (loadavg 4.35), so
   absolute Mbps is DIRECTIONAL until a QUIET re-capture. The load-robust outputs (same-run
   >SOTA ratios + relative hot-leaf rank) are the ground-truth; H1 requires the quiet re-capture
   before any ABSOLUTE Mbps claim.
4. **Prove `regen_check_clean == true`** — `cargo xtask regen --check` exit 0 (generated files
   are fresh regen output, never hand-patched; clean-regen-discipline). Resolve the git-dirty
   generated CSS files as clean regen.
5. **Confirm the G2 >SOTA-regression gate held** — `track1_rich/lightningcss >= the S-P1 ratio`
   on `css_canon_bench` (the explicit regression gate distinct from cssparser parity): bootstrap
   2.190×, tailwindcss 3.375×, material-components-web 1.658× (min), animate 2.101×
   (`SYNTHESIS-PROFILE §1`). The G2 CSS re-derivation MUST NOT regress these — oracle parity
   alone does not prove throughput preservation.

### Exit gate (MEASURABLE)

- **`materialization_framing == lazy-rich-vs-eager-cssom`** disclosed (the lazy-rich asymmetry
  EXPLICIT; no unqualified "beats CSSOM" claim).
- **`corpus_in_timer == true`** (the symmetric, cold, real-corpus plane).
- **`regen_check_clean == true`** (`cargo xtask regen --check` exit 0).
- **The G6 speedup figure**, if claimed absolute, rides a QUIET re-capture (loadavg-clean); else
  reported DIRECTIONAL with the load caveat (outcome `S` — honest residual, not `A`).
- **`json_guard_held == true`** (51/51 within ±1.0%); CSS >SOTA ratios held vs the S-P1 floor.

### Falsifiers

- **Framing falsifier:** an unqualified "beats CSSOM"/"equal-work" close-report claim behind the
  lazy-rich re-label, WITHOUT the materialization-depth asymmetry disclosed → R-A0-1 REJECT.
- **Plane falsifier:** a speedup figure NOT from the corpus-in-timer symmetric harness (warm,
  micro-fixture, or asymmetric-work-competitor) → addendum-5 REJECT.
- **Regen falsifier:** hand-patch a generated file → `regen --check` exit ≠ 0 → gate RED. Revert.
- **Load falsifier:** an ABSOLUTE Mbps claim emitted under loadavg > quiet threshold without a
  quiet re-capture → §5-risk-7 REJECT (directional only).

### Caps + revert

LOW; standard 30-min redress cap. No source revert by default (documentation + symmetric
re-measure + regen-check). On mismatch, reopen the producing wave (G6 for the speedup figure,
PROVE for the Sheets row) or mark close blocked with a mismatch list naming file paths, rows,
and missing evidence.

### Downstream

On H1 close, the SK-V18 generalization closes: one generator emits JSON+CSS+Sheets from `.bbnf`,
the shared trait both instantiate, the phantom resolved, the CSS scan NEON-accelerated at
admission, >SOTA preserved honestly, x86 gone, the Lock-14 gate meaningful. SK-V19 is the
totality-fold tranche (`crates/core/` adoption) + BBNF-self as the fourth grammar litmus.
