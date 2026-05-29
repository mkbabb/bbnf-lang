# SK-V17 P3-A: Candidate Shortlist

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-29.
Scope: distil the S-P2 CHALLENGE-survivor pool (L1–L9) into a shortlist of ≤8 ACTIVE candidate interventions; each carries owner path · scalar-ref status · checkasm-parity status · same-wave consumer · falsifiability gate (named corpus rows + Mbps thresholds). Drop every S-P2-REJECTed candidate.
Output: this file.
Pass Alpha goalset: SYNTHESIS §0.1 close-condition (tape activation + layout-driven projection + CSS typed 8-field equality + preserve-rich-ast + ≥1 regular corpus BEATS lightningcss full-CSSOM at N≥50 median + NEON hot-leaf union gated behind tape + clean regen + foldable-to-TOTALITY) and §0.5 per-corpus close (at least one of animate/bootstrap crosses the same-run lightningcss bar; tailwind allowed honest residual).
Candidate pool: `research/p2/` post-CHALLENGE survivors — the LOCKED pool of `HARDENING-S-P2-V3-CONSOLIDATED.md §3` (L1–L9), eligible set per §7 HANDOFF; REJECTed set per §4 barred.

## §1 — Synthesis (each candidate cites P1 hot leaf + P2 candidate + goalset line)

The S-P1 profile (`HARDENING-S-P1-V4-CONSOLIDATED.md §3.3`) resolved the benched CSS
recognition self-time into TWO leaf families and ONE substrate floor:

1. **The ~69% byte-class-membership scan leaf** — `find_component_delim` 59.24%
   (`generated.rs:288`, hot at `:295 delimiters.contains(&byte)`) + `consume_balanced_at`
   10.31% (`:320`), proven byte-for-byte the SAME `while pos<len` per-byte `match` inner
   loop differing only in the membership test → ONE NEON byte-class target, not two
   (`§3.3 :143–144,:148`). The same bytes are re-walked 2–3× by `parse_block_item:211`
   → `find_colon_before:314` → `parse_declaration:247` (`§3.3 :150`).
2. **The fact-stream String allocation floor** — `emit_fact_stream` 25.01% self-time
   (`generated.rs:5`) + ~64% `libsystem` page-zero / `String` realloc+free / memcpy
   (`§3.3 :158–159`), 4.4× instr/byte over `full_parse` (215–365 vs 46–58 i/B,
   `§3.1`). **Lever order is tape FIRST, then NEON** — the scan is masked by the String
   floor on the typed plane (`§3.4 :184–186`).
3. **The recognition-control loop** — `parse_stylesheet`/`parse_block`/`parse_block_item`
   28.87% + 2.45% (`§3.3 :145`), classed structural-recognition-control, NOT
   speculative-rollback (zero measured rollback self-time on either benched plane).

The S-P2 research designed eight active primitives + one conditional codegen property
against these antecedents, every one carrying a scalar-ref/checkasm/same-wave-consumer
field and a grammar-neutral verdict, all ACCEPT across CH1–CH7 (`§3`). The REJECTed set
(§4: orphan udot/i8mm digit decode, FNV/hex diagnostic, asmjson FSM, lo6-on-CSS reuse,
D6 second substrate) is barred — each is grammar-neutral in shape but has NO benched CSS
S-P1 antecedent (`§4`). This shortlist draws ONLY from the eligible pool.

**Lever-order mapping (the goalset's four-lever stack, SYNTHESIS §3 / §0.5):**
- Lever 1 (kill fact-stream String) = **L2** append + **L3** projection + **L8** flags + **L7** reserve (the tape substrate).
- Lever 2 (O(1) tape checkpoint, no split_off / Vec<Vec> / eager payload) = **L2/L3** carry the `offsets.len()` marker + truncate; **L9** rides D3's O(1) checkpoint as the codegen property.
- Lever 3 (NEON structural pre-scan) = **L1** classifier + **L5** comment mask + **L6** bracket mask (the W3 NEON cohort). **L4** is NOT a NEON kernel — it is the W2 projection-walk's tokenize-once *consumer* of the W3-produced (or, until W3 lands, W1-single-walk) structural index; L4 single-values to **W2** per the binding SPEC (`SPEC.md:494–499`, Section 5), reconciled across this artefact (the CH4-6 fold, twin of the L7→W1 fold).
- Lever 4 (commit-by-construction spine) = **L9** (CONDITIONAL, post-W1 re-profile gate; CF-1=tape-activation lands in W1).

**Residual R1 first-touch fold (HARDENING-S-P2-V3 §5):** this artefact does not touch
`p2a`/`p2c` source, so the R1 cosmetic fold (`p2a:3` `Cycle: V2`→`V3`; `p2c:318/325`
verbatim re-word) is carried forward to the first wave-triumvirate that edits those
artefacts; recorded here as an obligation, not applied (no `p2a`/`p2c` byte changed).

## §2 — Deliverable: the candidate shortlist (8 ACTIVE + 1 CONDITIONAL)

Master HEAD verified `f87ee713a`. Every owner-path symbol below was re-resolved at HEAD
this cycle (`select_classifier` `dispatch.rs:42`; `push_plain_offset` `assembler.rs:71`;
`byte_class_from_eq_set_64_neon` `byte_class_from_eq_set_64.rs:33`; `W5C_REQUEST_FACT_PROFILES`
`lib.rs:336`; `find_component_delim`/`consume_balanced_at`/`emit_fact_stream`
`css_l4_declaration_values/generated.rs:288/320/5` — all present).

The shortlist is **8 active candidates (≤8, CH4-compliant)** plus **L9 carried as a
conditional, NOT counted against the active 8** — L9 admits to a wave ONLY if the
post-W1 re-profile gate fires (§3), so on the LOCKED profile the active count is 8.

---

### S1 — NEON byte-class structural classifier (eq-set fan via `select_classifier`)
*Pool: L1 — aliases CP-A1/C-B1/C1→C2/CF-2/G3.*

- **Owner file path.** `skinny/crates/bbnf-simd/src/dispatch.rs:42` (`select_classifier(alphabet:&'static [u8;64])`); backend `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33` (`byte_class_from_eq_set_64_neon`, four `vld1q_u8` stripes + `vceqq_u8`/`vorrq_u8` reduce). Consuming surface `runtime/src/grammars/css_l4_declaration_values/generated.rs:288/320` (the scan replaced).
- **S-P1 antecedent.** `find_component_delim` 59.24% + `consume_balanced_at` 10.31% = ONE ~69% membership-scan leaf (`HARDENING-S-P1-V4 §3.3 :143–144,:148`).
- **Scalar-reference status.** PRESENT — `byte_class_from_eq_set_64` scalar twin (`src/scalar/byte_class_from_eq_set_64.rs`). (Lock-16 satisfied.)
- **Checkasm-parity status.** PRESENT — `tests/checkasm_byte_class_from_eq_set_64.rs` (eq-set differential). The vectorized-256-table form is REQUIRED-NEW *iff* ever selected — NOT selected here (the CSS `;{` pair collides under the lo6 `& 0x3f` mask → eq-set fan is the CSS-admissible backend, `§3 L1`).
- **Same-wave consumer.** S2 (tape build consumes the `Vec<u32>` structural index). The scan + the tape land together or neither — no orphan kernel.
- **Falsifiability gate.** No standalone Mbps row (a SIMD primitive does not bench alone); gated through S2/S3's typed-tape exit gate (the structural-pre-scan wave, P3-B W3). Binding identity: the produced `Vec<u32>` IS the tape `offsets` (§4 cond-1). NEON gated behind tape activation (no structural index to pre-scan into until S2/S3 land — SYNTHESIS §0.1 NEON gate). Telemetry: `native_simd_status ∈ {checkasm-pass}`, `simd_non_json_exercise = css_l4`.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL (JSON+CSS witnessed); `select_classifier(alphabet)` is the Lock-14 vehicle, alphabet = caller data (`LOCKS.md:393–397`).

### S2 — Tape-append materialization op (`push_plain_offset`)
*Pool: L2 — aliases CP-A2/C-B2/D1/CF-1-append.*

- **Owner file path.** `skinny/crates/runtime/src/tape/assembler.rs:71` (`push_plain_offset`, one branchless u32 write into `offsets:Vec<u32>`, `reserve_offsets_cold` cold path `:89`); single non-generic `TapeBuilder` `assembler.rs:42`. Retires the live plane `runtime/src/grammars/css_l4_*/generated.rs:5` (`emit_fact_stream`).
- **S-P1 antecedent.** `emit_fact_stream` 25.01% self-time + the ~64% `String` realloc/alloc floor (91% reached FROM `emit_fact_stream`, `§3.3 :158–159`).
- **Scalar-reference status.** N/A — substrate op, not a vector kernel; append is a scalar branchless write already exercised by JSON.
- **Checkasm-parity status.** Correctness analogue — tape↔fact_stream corpus-parity + cssparser 8-field structural equality (`rules=10136/style=9561/sel=9561/decls=20043`); `PayloadArena.write_count==0` on source-re-readable leaves (SYNTHESIS §0.1 tape-activation PROOF).
- **Same-wave consumer.** S3 lazy projection (same substrate, Lock 1) — the pair lands together.
- **Falsifiability gate.** Tape-activation gate: `Tape`/`ValueRef`/`TapeBuilder` greppable non-zero in the benched CSS parse path + `css_l4_*/generated.rs`; `tape_activated=true`; benched Track 1 stops returning `String`. THEN the >SOTA corpus rows: ≥1 of {animate, bootstrap} crosses the same-run N≥50 median lightningcss full-CSSOM bar (SYNTHESIS §0.5; per-corpus threshold = the Wave-0-re-baselined lightningcss median, UNMEASURED-PENDING until W0 emits — NO inferred endpoint).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL; `TapeBuilder` carries no grammar-keyed field; which positions push is `BackendRule`-derived data (cond-3, §4).

### S3 — Grammar-parametric lazy `ValueRef<G>` projection (cursor API)
*Pool: L3 — aliases CP-A3/D2/CF-1-projection.*

- **Owner file path.** `skinny/crates/runtime/src/tape/mod.rs:175` (`ValueRef<'doc,'input,K,G>`, `_grammar:PhantomData<fn()->G>`); isomorphic to JSON `runtime/src/grammars/json/value.rs:143` (`value_from_ref`). Generator emits `document/value/view/visitor` from `skinny/crates/codegen/` (`grammar_provider.rs` + `lower/{tape_plan,offset_tape,event_tape}.rs`).
- **S-P1 antecedent.** The String-materialization floor `emit_fact_stream` carries — L3 replaces the eager typed materialization (NOT the AZ-IV eager tree, §4 pre-block).
- **Scalar-reference status.** N/A — cursor read; `value_from_ref` is the existing JSON reference impl.
- **Checkasm-parity status.** Correctness analogue — cssparser 8-field equality round-trip (gate-before-speed, SYNTHESIS §0.1).
- **Same-wave consumer.** It IS S2's consumer — the pair lands in the same commit (no eager `Box::new`, preserve-rich-ast).
- **Falsifiability gate.** `lazy_view_generated=true` + `css_rich_ast_preserved=true` (CSSOM via lazy `ValueRef`, value-plane count parity with the eager-tree baseline: dimensions/colors/functions/lists). `projection_generality_exercise ∈ {json, css_l4}`. Same corpus >SOTA rows as S2 (shared wave).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL by construction — `ValueRef<…,G:EventGrammar=AnyGrammar>` is generic over G (`mod.rs:175`); JSON+CSS instantiate the SAME cursor type. Sheets/BBNF-self SK-V18.

### S4 — Tokenize-once shared-scan reuse
*Pool: L4 — alias CP-A4.*

- **Owner file path.** `runtime/src/grammars/css_l4_declaration_values/generated.rs` — the per-grammar consumption template over S1's structural index, eliminating the `find_component_delim:288` / `find_colon_before:314` / `parse_declaration:247` 2–3× re-walk; the index IS the tape (no parser-local second cursor).
- **S-P1 antecedent.** The 2–3× re-walk of the same bytes (`§3.3 :150`, P1-D §2.5).
- **Scalar-reference status.** N/A — consumption pattern over the neutral S1 index.
- **Checkasm-parity status.** Correctness analogue — cssparser equality (output-invariant under reuse).
- **Same-wave consumer.** S4 lands in **W2** (the projection walk, `SPEC.md:494–499` Section 5): it IS the W2 projection's tokenize-once consumer of the structural index. When W3's NEON scan has landed, S4 reuses W3's `Vec<u32>`; until then S4 consumes the W1 single-walk (`SPEC.md:498–499`). S4 is single-valued to W2 across the cohort (the CH4-6 fold — the twin of the L7→W1 single-value fold; S4 is a W2 *consumer*, not a W3 NEON *producer*).
- **Falsifiability gate.** Bound to the single-substrate REDRESS-53 shape: index == tape-offsets identity (§4 cond-1). Measured via the **W2** projection-wave corpus rows (the re-walk elimination must not regress the cssparser equality count and must hold the W2 maintain budget — no worse than −2.0% median vs the W1 typed-tape baseline, P3-C §2.2/§3); its contribution to the >SOTA delta on ≥1 regular corpus is realised at the W3 close (when the NEON index it consumes lands). No separate Mbps row (a consumption pattern does not bench alone; gated through W2's exit).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL — generic reuse pattern consumed by a per-grammar template (Lock-14 phrase #1); which bytes index is grammar data.

### S5 — `comment_body_mask_64` (NET-NEW suppressor mask)
*Pool: L5 — alias G1.*

- **Owner file path.** NET-NEW kernel in `skinny/crates/bbnf-simd/src/` (digraph-parameterised `(open:[u8;2],close:[u8;2])`; region fill reuses the `escape_mask_64` `overflowing_add` carry idiom `lib.rs:188`, NOT PMULL — clear of REDRESS-88); AND-NOTed into S1's structural index. Scalar twin `src/scalar/comment_body_mask_64.rs` (NET-NEW). Checkasm `tests/checkasm_comment_body_mask_64.rs` (NET-NEW).
- **S-P1 antecedent.** The comment-skip arm of `find_component_delim`/`consume_balanced_at` (the ~69% scan leaf).
- **Scalar-reference status.** ABSENT-with-verbatim-§2-sketch — `p2e:120–129` (executable Rust over `open[0]/open[1]/close[0]/close[1]`, never a literal `/`/`*`); `src/scalar/comment*` confirmed absent → genuinely net-new, scalar twin lands same wave.
- **Checkasm-parity status.** REQUIRED-NEW (`checkasm_comment_body_mask_64`, ABSENT today) — lands same wave as the kernel.
- **Same-wave consumer.** S1 (the structural-index composition G3).
- **Falsifiability gate.** Gated through W3 (the structural-pre-scan wave): the mask must AND-NOT correctly (cssparser equality preserved across comment-bearing corpora — material-components-web/bootstrap carry comments) and contribute to the W3 >SOTA delta. 1-bit carry threads WITHIN one block sequence (transient producer, not a retained sidecar — CH5).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL by digraph parameterisation (C/Rust/JS/SQL block comments); no CSS pin.

### S6 — `bracket_depth_mask_64` (NET-NEW depth-balance mask, scalar-balance default)
*Pool: L6 — alias G2.*

- **Owner file path.** NET-NEW in `skinny/crates/bbnf-simd/src/` over open/close MASKS (already abstracted from bracket bytes by S1 — sees masks, never literal bracket bytes); SHIPPED/DEFAULT body = a SCALAR running balance over the two precomputed masks with an i32 `depth_carry` threaded WITHIN a single `scan_components_to_index` call, init-0-per-parse, never retained (`p2e:150–154`). Scalar twin `src/scalar/bracket_depth_mask_64.rs`. Checkasm `tests/checkasm_bracket_depth_mask_64.rs`.
- **S-P1 antecedent.** `consume_balanced_at` 10.31% recursion (the bracket-balance arm of the ~69% scan leaf).
- **Scalar-reference status.** ABSENT-with-verbatim-§2-sketch (`p2e:155–165`); `src/scalar/bracket*` absent → net-new, scalar IS the default body.
- **Checkasm-parity status.** REQUIRED-NEW (`checkasm_bracket_depth_mask_64`, ABSENT today).
- **Same-wave consumer.** S1 (the structural-index composition G3).
- **Falsifiability gate.** Gated through W3. BINDING (cond-4, §4): the shipped body is the scalar balance; the CTZ-ranges refinement is consumer-only + parity-gated + REVISE-back-conditioned (NOT the default — promotion re-opens REDRESS-89). Measured: balance correctness via cssparser equality across nested corpora (tailwind = deeply nested) + W3 >SOTA contribution.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL — the canonical Lock-14 nested-balance primitive (JSON arrays/objects, CSS component blocks, BBNF `()`/`[]`, Sheets parens); sees only masks.

### S7 — One-shot SIMD capacity reservation
*Pool: L7 — alias D4.*

- **Owner file path.** `skinny/crates/runtime/src/tape/` `CapacityPlan::OneShotSimd` sizing the EXISTING `offsets` vector (`scan_structurals(src).positions().len()+8`, JSON precedent `json/scan.rs:53`), one cold `Vec::reserve`. No second vector, no per-corpus capacity literal. **Lands in W1 with S2** (SPEC §4, `SPEC.md:388,428–430`): L7 is the W1 tape-append's same-wave reservation consumer; on the LOCKED profile (W3 NEON scan not yet landed) it sizes `offsets` from a conservative byte-proportional bound — never the S1 scan count, never a per-corpus literal. The S1-scan-count refinement is a W3 follow-on, NOT the W1-default body.
- **S-P1 antecedent.** The ~64% alloc floor (grow churn, `§3.3 :158`).
- **Scalar-reference status.** PRESENT — the byte-proportional bound is a scalar size computation; when the W3 scan count is available it reuses `scan_structurals_scalar` (no new kernel).
- **Checkasm-parity status.** N/A in W1 (byte-proportional bound is scalar arithmetic, not a vector kernel); the W3 scan-count refinement reuses S1's existing classifier differential.
- **Same-wave consumer.** S2 (the tape it sizes), **same W1 commit** — L7 is not deferred behind the NEON scan; the W1 default body is the byte-proportional cold reserve, single-valued to W1 (SPEC §4, `SPEC.md:391`).
- **Falsifiability gate.** Gated through the **W1** tape-activation wave (SPEC §4 exit): the grow-churn fraction of the ~64% alloc floor must fall (visible in the instr/byte drop toward the 46–58 i/B `full_parse` band) WITHOUT a per-corpus capacity literal (the FNV/fixture re-entry, §4). Measured: the typed-plane instr/byte trends off the 215–365 i/B fact-stream band; no per-corpus constant in source (grep-clean).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL — `CapacityPlan` is grammar-free; the count reuses the shared classifier with the CSS alphabet as the only per-grammar datum.

### S8 — Sparse-flag side-table (`BackendRule` branch-tag projection)
*Pool: L8 — alias D5.*

- **Owner file path.** `skinny/crates/runtime/src/tape/assembler.rs:93–113` (EXISTING `flag_cursors`/`flag_values` sparse pair) + `mod.rs:144–150` (`flags_at` binary-search). NOT a new vector, NOT a widened per-position record, NOT a dense parallel column — paid only where non-zero.
- **S-P1 antecedent.** Mechanism supporting S3 (indirect, guarded — S3's kind-disambiguation).
- **Scalar-reference status.** N/A — substrate op.
- **Checkasm-parity status.** Corpus-parity analogue (round-trips with S2).
- **Same-wave consumer.** S3 (the projection that reads the flags) — same wave.
- **Falsifiability gate.** BINDING (cond-2, §4): each flag bit MUST be a `BackendRule` branch-tag projection, NOT a hand-curated per-rule catalogue (else it relocates `W5C_REQUEST_FACT_PROFILES` into flag form → CH2 REVISE). Measured via S3's equality + preserve-rich-ast gate (the flags disambiguate kinds the projection reads); `w5c_profile_array_retired=true` (no relocated per-rule catalogue greppable).
- **Grammar-neutral verdict.** GENERALISABLE-WITH-GUARD — the flag bit = `BackendRule` branch-tag; the side-table adds no substrate (CH5-clean, Lock 1).

---

### S9 (CONDITIONAL — NOT in the active 8; admits only post-re-profile)
**Commit-by-construction Alt-mode codegen property** — *Pool: L9 — aliases CF-3/D3.*

- **Owner file path.** `skinny/crates/codegen/src/lower/tape_plan.rs` (the emitter emits NO speculative checkpoint for pure-lexical keyword-dispatch Alts depositing nothing structural; spine commits as it scans, driven by S1's index). Rides D3's O(1) `offsets.len()` checkpoint / `truncate` rollback (SK-V16-banked, one offset vector — no `split_off`, no `Vec<Vec>`).
- **S-P1 antecedent (CONDITIONAL).** The recognition-control loop 28.87% + 2.45% (`§3.3 :145`) is classed structural-recognition-control, NOT a measured speculative-rollback antecedent (P1-E measured ZERO rollback self-time on either benched plane). The re-profile that could unmask it is keyed to **post-W1** (`SPEC.md:616,637`): the S-P2 §3 L9 antecedent (`HARDENING-S-P2-V3 §3 L9`) is un-masked by the **W1-retired alloc floor (S2 tape activation)**, NOT by the W3 NEON scan collapse — W1 (not W3) is the wave whose close makes the typed-tape path exist for re-profiling.
- **Scalar-reference status.** N/A — codegen control-flow.
- **Checkasm-parity status.** Recognizer-output equality with/without the Alt-mode pass (byte-identical tape).
- **Same-wave consumer.** The post-W1 CSS recognizer spine — GATED on the post-W1 re-profile as the ADMISSION gate, not a live consumer on the LOCKED profile.
- **HARD admission gate (§4 cond-5, HANDOFF §6; SPEC §7 entry, `SPEC.md:616,637`).** Admit to a wave (W4) ONLY if a **post-W1** (post-S2 tape-activation) typed-`Tape`/`ValueRef` re-profile (N≥50) surfaces the recognition-control loop (un-masked by the W1-retired alloc floor — the S-P2 §3 L9 antecedent) OR a speculative-rollback leaf as top-N self-time. The LOCKED 28.87%+2.45% figures are NOT a measured rollback antecedent → S9 may NOT ship on the current profile. The re-profile is run post-W1 (not post-W3 — the alloc floor, not the scan, is the masker); falsifiability gate is the re-profile itself (named top-N self-time leaf), authored by W4's research phase.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL codegen property derived from `BackendRule` Alt shape, JSON-witnessed; not CSS-keyed.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

Per-candidate gates above resolve to two measurable rollups; both key on the benched
set `{bootstrap, tailwindcss, material-components-web, animate}`
(`css_l4_corpus.rs:22–54`), N≥50 cold median (`css_canon_bench.rs:250` asserts N≥50):

- **Substrate gate (S2/S7 land W1 tape-activation; S3/S4/S8 land W2 projection; S1/S5/S6 land W3 NEON scan).** `tape_activated=true` ∧
  `lazy_view_generated=true` ∧ `css_rich_ast_preserved=true` ∧ `w5c_profile_array_retired=true`
  ∧ EXACT 8-field cssparser equality (`rules=10136/style=9561/sel=9561/decls=20043`,
  `track1_errors=0`, `cssparser_errors=0`, 4/4 corpora) re-proven on the NEW typed path.
  Equality is gate-before-speed (SYNTHESIS §0.1) — no Mbps row admits until it holds.
- **>SOTA gate (the tranche close, S2/S3/S4/S5/S6/S7/S8 cumulative + S1 NEON).** At least
  ONE of {animate, bootstrap} has `css_track1_typed_median_mbps` >
  `css_lightningcss_full_cssom_median_mbps` on the SAME N≥50 run (delta_vs_lightningcss
  > 1.0×, `css_comparator_plane = full-cssom`). The per-corpus lightningcss bar is the
  **Wave-0-re-baselined same-run median** (SYNTHESIS §0.5) — NOT a prior fixed number
  (793/833/929/974 are run-dependent references); it is **UNMEASURED-PENDING** until W0
  emits the per-corpus split (alphaB's inferred animate↔164/tailwind↔51/material↔60
  endpoints are self-flagged INFERRED, NOT citable thresholds — no wave exit-gate may
  key on them). tailwindcss is allowed an honest residual recorded in REDRESS provided
  ≥1 regular corpus crosses. JSON 51/51 strict same-plane holds throughout (the
  regression tripwire).

The Mbps THRESHOLD VALUES are bound by P3-C against the W0 re-baseline; P3-A's binding is
the *predicate* (typed-median > same-run lightningcss-median on ≥1 regular corpus) and the
*named corpus rows* (animate, bootstrap as the gating pair; material as the integration
check; tailwind as the honest-residual hold-out). A gate keyed on an inferred per-corpus
endpoint FAILS CH1 — P3-A explicitly does not assert one.

## §4 — Pre-blocked routes + binding shortlist conditions

**Barred from this shortlist (HARDENING-S-P2-V3 §4 — REJECTed class):** orphan `udot`
4-digit decode (CF-4a/C5/C-B3/G4 — no benched CSS digit leaf, P1-E §4.4(a) categorical);
net-new `i8mm` digit/dimension kernel (CF-4b/C6 — doubly orphan-blocked); FNV/`push_ascii_lower_hex`
(retires WHOLESALE with the String, never a primitive); asmjson collapsed-stage FSM (x86
AVX-512, dead on aarch64); lo6 `classify_tbl4` reuse on the CSS alphabet (the `;{`→slot-59
`& 0x3f` collision + table-NEON scalar passthrough → it would claim a SIMD win it runs
scalar); D6 second substrate (Lock-1 no-go anchor). None appears in S1–S9.

**Binding shortlist conditions (HARDENING-S-P2-V3 §6 — carry verbatim; a candidate
violating one CH-REJECTs at the wave):**
1. **S1/S4 (G3) index == tape-offsets identity.** The produced `Vec<u32>` IS the tape's
   `offsets`; carry/depth threads WITHIN a single `scan_components_to_index` call, reset
   per parse. Retaining the index parallel to a retained parse = REDRESS-53.
2. **S8 (D5) flag bit = `BackendRule` branch-tag projection** — not a hand-curated
   per-rule catalogue (= relocated `W5C_REQUEST_FACT_PROFILES`, CH2 REVISE).
3. **S2/S3 (CF-1/D1) routing derived-from-grammar.** `W5C_REQUEST_FACT_PROFILES`
   (`codegen/src/lib.rs:336`, consumed `:567/:611`, selected `:299`) RETIRED; every
   residual CSS routing entry names its `.bbnf` rule; relocating per-rule branching into
   projection DATA is the Lock-14-phrase-#1 re-entry seam (FORBIDDEN).
4. **S6 (G2) scalar-balance default.** May NOT ship with CTZ as the unconditional body;
   CTZ-ranges is consumer-only + parity-gated; promotion re-opens REDRESS-89 (CH3 REVISE).
5. **S9 (CF-3/D3) hard post-W1 re-profile obligation.** Admit as active (W4) ONLY if a
   **post-W1** (post-S2 tape-activation) typed-tape re-profile (N≥50) surfaces the
   recognition-control loop or a speculative-rollback leaf as top-N; the re-profile keys
   to W1 (the alloc floor is the masker, not the W3 scan — `SPEC.md:616,637`); the
   28.87%+2.45% figures are NOT a measured rollback antecedent.

**Pre-block families each wave must NOT re-open (SYNTHESIS §0.4, carried for P3-E/P3-F):**
AZ-IV eager-value-tree; StructRegistry/`Arena<G>`/`Builder<G>` hot-path indirection;
CSS fact-stream String as an admission plane; the `W5C_REQUEST_FACT_PROFILES` hand-coded
routing (RETIRE not extend); the 24-row broadcast (one tuple → N rows); fixture/FNV
contrivances (per-corpus `real_typed.rs` fixtures, per-corpus capacity constants, FNV
production proof); x86/AVX/SVE; second substrate / retained sidecar / retained cursor /
aux density table / sidecar event vector / public `UnionTape`. REDRESS families
(semantics, not just ids): 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98,
183/184/209-213, 215, 242-247, FNV closed-enum production migration.

**Residual R1 first-touch fold (HARDENING-S-P2-V3 §5):** not applied here (this artefact
edits no `p2a`/`p2c` byte); carried to the first wave editing those artefacts —
`p2a:3` `Cycle: V2.`→`Cycle: V3.`; `p2c:318/325` "(deferred to P2-F CF-4a/CF-4b)" →
verbatim "(grammar-neutral SHAPE per §C5/§C6; P2-F CF-4a/CF-4b carries the cross-grammar
digit-run verdict in-pass)".

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 (P3-A scope: ≤8 shortlist, the
  five per-candidate fields), §2.1 (frontmatter), §3 (CH1–CH6), §8 (bbnf-lang axes).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  §3 (LOCKED pool L1–L9), §4 (REJECTed barred set), §5 (R1 residual fold), §6 (binding
  conditions 1–5), §7 (eligible-pool HANDOFF). Commit `f87ee713a`.
- `restart/skinny/tranches/sk-v17/research/p2/{p2a,p2b,p2c,p2d,p2e,p2f}.md` (per-candidate
  aliases + scalar/checkasm/same-wave fields + grammar-neutral verdicts).
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  §3.1 (canonical N≥50 bench, instr/byte bands), §3.3 (hot leaves: `find_component_delim`
  59.24%, `consume_balanced_at` 10.31%, `emit_fact_stream` 25.01%, the ~64% alloc floor),
  §3.4 (lever order: tape FIRST then NEON). Commit `0ae1caa52`.
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1 (close conditions), §0.4 (pre-blocks
  + generality clause), §0.5 (per-corpus close + benched set), §0.6 (strict comparator),
  Section 2 (telemetry binding). Commit `6496fecae`.
- Source verified at HEAD `f87ee713a`: `bbnf-simd/src/dispatch.rs:42`,
  `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33`,
  `runtime/src/tape/assembler.rs:42/71/89`, `runtime/src/tape/mod.rs:175`,
  `codegen/src/lib.rs:336`, `runtime/src/grammars/css_l4_declaration_values/generated.rs:5/288/320`.
- `restart/skinny/tranches/sk-v8/SPEC.md` (the SPEC shape S-P3 mirrors — read for the
  shortlist-row provenance discipline).
- `restart/locks/LOCKS.md` (Lock 1 substrate-union, Lock 14 grammar-neutrality
  `:386–387/:393–397`, Lock 16 SIMD parity).
