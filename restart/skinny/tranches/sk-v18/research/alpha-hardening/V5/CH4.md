# CH4 — COST lens (V5) — Pass Alpha SK-V18 alpha-hardening

**Lens:** CH4 Cost (PASS-ALPHA §3 / ORCHESTRATOR §3W).
**Subject:** SK-V18 = the GENERALIZATION cycle (the inflection backtrack). ONE grammar-driven
generator emitting JSON+CSS+Sheets from `.bbnf`, over the unified tape/`ValueRef` substrate,
shared value-API, PROVEN on a 3rd grammar (Sheets), PRESERVING `>SOTA`. NOT a feature cycle.
**Artefacts reviewed:** `research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md`.
alphaF = SYNTHESIS+HANDOFF (no separate `alphaF*.md` on disk — verified `ls alphaF*` = no match;
SYNTHESIS+HANDOFF ARE the α-F output per PASS-ALPHA §2/§6). CH4 reviews cost-bearing claims
across all; alphaB/alphaD are competitor/ledger axes — only their cost-bearing rows are in scope.
**Focus per dispatch:** each candidate's **LOC budget + risk classification + same-wave consumer
+ scalar-ref/checkasm (SIMD) status**, and that grammar-DERIVATION preserves the `>SOTA` threshold
honestly.
**Cycle:** V5 — the ceiling iteration (V≤5 per ORCHESTRATOR §3Z). V4 wave aggregate was 90.8%
(sub-95%, non-converging); CH4 itself closed V4 at **100% (6A/0R)** with zero orphan. The V4
non-convergence was driven by REVISEs on OTHER lenses (CH1/CH3/CH7 x86-FOLD-1 orphan, CH2 §8.1
projection-tuple, CH6 reach/citation), folded into V5 as **F15** (crate-wide x86) + **F16**
(config-tuple projection) + the SYNTHESIS/HANDOFF reach-extensions. CH4's V5 mandate is to
re-verify that those V5 folds introduce **no cost regression** and that the CH4 cost cohort stays
clean and convergence-ready on the cost axis.
**Method:** every cost/LOC/checkasm/kernel/owner-path/structural-gate claim re-verified LIVE on
disk at the benched skinny tree (`skinny/crates/` + `skinny/xtask/`) at this V5 pass. Citations are
`path:line` from disk. HEAD bracket `318d9c046`.

---

## §0 — V4→V5 fold status (the prior CH4 dispositions + the new V5 αE folds)

V4 CH4 disposition was **ACCEPT 6 · REVISE 0 · REJECT 0** (`V4/CH4.md:395`). There is **no orphan
V4 CH4 REVISE to discharge** — CH4 converged at V4. The V5 αE fold ledger introduces two new folds
(neither is a CH4-originated REVISE; both are cross-artefact CHALLENGE folds α-E records). CH4's V5
job: confirm each is **cost-free or cost-favourable** and disk-true.

| αE V5 fold | source | what it changes (cost axis) | V5 disk state | verdict |
|---|---|---|---|---|
| **F15** | CH1 §αE + CH3 (`V4/CH3.md:316-345`); seeded CH5 V3 §C.5 | The x86 P1 deletion scope + close-gate widened from `src/x86_64/`-only to **crate-wide** (the SECOND x86 surface the V1–V4 αE row omitted): `ext/x86/` vendored ASM + `build.rs` nasm driver + `Cargo.toml:19 nasm-rs` dep + `lib.rs:247` ref. **Cost effect: the P1 LOC Δ DEEPENS from −847 → ≈ −4500.** A net-MORE-deleted correction — the favourable direction for a prune. | **DISK-TRUE** (§0.1): `ext/x86/`=4 files/**3554** LOC; `build.rs`=**102** LOC; `Cargo.toml:19 nasm-rs="0.3"` + `:8 build="build.rs"` ACTIVE; `lib.rs:5 pub mod x86_64`; `lib.rs:247 ext/x86/bbnf.asm` ref; the x86 cfg-dispatch arm `lib.rs:285-288` (`cfg(all(target_arch="x86_64",target_feature="avx512bw"))`). PROPAGATED into the contract (SYNTHESIS:58-124; HANDOFF:74-110, removal targets reach-matched to the grep). | **FOLDED + DISK-CORRECT — cost-FAVOURABLE (−847→−4500)** |
| **F16** | CH2 §8.1 (`V4/CH2.md:294-447`) | The relocated-seam structural check projection widened from `(source_roots, entry_rule)` (V3 F13) → the **full per-grammar config-tuple modulo `output_dir`/`expected_files`**. **Cost effect: ZERO LOC** — it re-projects an existing structural check over more of an existing data-table's columns; a `sort -u`/`awk`/`jq` over data already on disk. No new code to write to EVALUATE; strictly STRENGTHENS the gate. | **DISK-TRUE** (§0.1): 7 css_l4 `RuntimeTarget` rows SHARE `entry_rule:"stylesheet"`+`source_roots:CSS_L4_ROOTS` (old projection collapse=1 GREEN) BUT carry **7 DISTINCT** `fact_schema` values (`regen_css.rs:49,67,85,103,121,139,157`) — so old projection was FALSE-GREEN; widened gate correctly RED today. PROPAGATED (SYNTHESIS:89-156,322(iii),397; HANDOFF:21-24,273-279,330-332). | **FOLDED + DISK-CORRECT — cost-FREE (zero LOC, strengthens gate)** |

The V4 CH4 cost cohort is therefore **zero-orphan at V5** (CH4 had no V4 REVISE), and neither V5
fold introduces a cost regression: F15 deepens the prune (favourable), F16 is a zero-LOC
projection widening (cost-free). The single most-likely cost-regression site — an un-satisfiable
P1 gate (reach-mismatch between deletion-list and grep, the CH6 V4 §1 RED-by-construction hazard)
— is verified ABSENT: the deletion list is now reach-matched to the `--include`-scoped grep
(SYNTHESIS:117; HANDOFF:107-110), satisfiable-by-construction.

### §0.1 — Disk-verified cost ground truth (V5 re-check, this pass)

Every cost-bearing figure re-measured LIVE on disk at the V5 entry HEAD `318d9c046`:

| claim (αE / SYNTHESIS / HANDOFF) | disk verification (V5 this pass) | verdict |
|---|---|---|
| **F15 P1 `src/x86_64/`** = 24 files / 742 `.rs` / 105 `.asm` / 14 `unimplemented!` | `find …/src/x86_64 -type f`=**24**; `.rs` LOC=**742**; `.asm` LOC=**105**; `unimplemented!`=**14** | **EXACT** |
| **F15 P1 `ext/x86/`** = 4 files / 3554 LOC vendored ASM | `find ext/x86 -type f`=**4**; total LOC=**3554** | **EXACT** |
| **F15 P1 `build.rs`** = 102 LOC nasm driver | `wc -l build.rs`=**102** | **EXACT** |
| **F15 P1 `Cargo.toml`** nasm dep + build ref | `:8 build="build.rs"`, `:19 nasm-rs="0.3"`, `:14-16` companion comments (27-line manifest) | **EXACT** |
| **F15 P1 `lib.rs`** module decl + cfg-dispatch arm + doc ref | `:5 pub mod x86_64;`; cfg-dispatch arm `:285-288` (`cfg(all(target_arch="x86_64",target_feature="avx512bw"))` → `x86_64::byte_class_from_eq_set_64`); `:247 ext/x86/bbnf.asm` doc ref | **EXACT** (αE:94 line range `:285-288` disk-true; earlier bare-`target_arch` grep missed it because the cfg uses `all(…, target_feature=…)` — the αE owner-path is correct) |
| **F15 P1 LOC Δ ≈ −4500** = 847 (`src/x86_64/` incl `.asm`) + 3554 (`ext/x86/`) + 102 (`build.rs`) | 742+105+3554+102 = **4503** | **EXACT (≈ −4500)** |
| **F16 — 7 css_l4 rows share `(source_roots,entry_rule)` but carry 7 distinct `fact_schema`** | `grep fact_schema regen_css.rs`=**7 distinct** (`…at-rules-media…` … `…visual-function…`); `entry_rule:"stylesheet"`+`source_roots:CSS_L4_ROOTS` shared across all 7; `RuntimeTarget` struct `regen.rs:6` (9-field census; `RuntimeTarget {`=**7** instances) | **EXACT — old projection false-green confirmed; F16 widening correct** |
| P2 `nonjson_css_l4.rs` = 3737 LOC; oracle KEPT | `wc -l`=**3737**; `assert_rich_strict_equality:451` (KEPT); `lightningcss_facts:528`; `measure_mbps:3091` | **EXACT** |
| P3 7 CSS `generated.rs`, byte-identical | `find css_l4_*/generated.rs`=**7**; LOC=**6370** (≈910 ea); `md5` at_rules_and_media ≡ visual_functions = `b654562ccff46ed62dd48e9ace325830` | **EXACT** |
| P5 `parse_w11_1_number` ×7 | `grep -c` json/generated.rs=**7** | **EXACT** |
| B1 `json_templates/` + `json_sink_direct.rs` | `json_templates/`=**6 files / 1149 LOC**; `json_sink_direct.rs`=**561 LOC** | **EXACT** |
| B2 `CSS_GENERATED_RS` span + siblings | `CSS_GENERATED_RS`=**:701**; `CSS_MOD_RS`=**:598**; `CSS_PARSER_RS`=**:612**; `CSS_SINK_RS`=**:665** | **EXACT** |
| B3 phantom `ValueRef<…,G:EventGrammar=AnyGrammar>` | `tape/mod.rs:175` `G: EventGrammar = AnyGrammar`; impls `:183/:185/:191`; default `AnyGrammar` (no production bind) | **EXACT** |
| B4 checkasm count | `ls checkasm_*.rs`=**14** = 12 single-kernel + `checkasm_common.rs` + `checkasm_parity.rs` | **EXACT** (F4/F14 hold) |
| PROVE Sheets stub | `sheets_witness/`=**25 LOC** | **EXACT** |
| αF telemetry cost/generalization columns | `grep -oE` over SYNTHESIS: `x86_tree_deleted`×6, `runtime_target_rows_collapsed`×8, `verbatim_blob_present`×4, `emitter_fork_present`×3, `phantom_generic_resolved`×3, `generated_md5_distinct`×3, `generator_grammar_count`×2, `acceleration_at_admission`×3, `corpus_in_timer`×3, `sheets_real_grammar`×2 | **PRESENT (incl. both V5-fold columns)** |

**The V5 αE cost ground truth is accurate in full.** No stale count, no wrong owner-path, no
un-satisfiable gate. The F15 crate-wide x86 surface is disk-true in every removal target (847 +
3554 + 102 + nasm dep), the −4500 P1 LOC Δ recomputes EXACT, and the F16 config-tuple divergence
(7 distinct `fact_schema` over a shared `(source_roots,entry_rule)`) is disk-confirmed — so the
F16 gate is correctly RED today and goes GREEN only post-collapse, at ZERO LOC cost.

---

## §1 — CANDIDATE A (PRUNE P1–P5) — **ACCEPT**

**LOC:** αE budgets net **≈ −10800** (`alphaE:108,227`), the V5 figure incorporating F15's
crate-wide x86 surface. Disk recompute this pass: P1 **≈ −4500** (742 `.rs` + 105 `.asm` + 3554
`ext/x86/` + 102 `build.rs`, F15) + P3 ≈ −5460 (6×910 redundant `generated.rs`) + P2 ~−700 (of
3737-LOC `nonjson_css_l4.rs`, keeping the `assert_rich_strict_equality:451` oracle) + P4 +~15 + P5
rename-only ⇒ **≈ −10600…−10800**. The F15 fold (P1 −847 → ≈ −4500) is carried on the P1 row
(`alphaE:94` LOC column) and re-rolled into the candidate-A total (`alphaE:108`) and the SUMMARY
net (`alphaE:227` ≈ −12850). Direction strongly favourable; no `[generated-size-budget]` overflow
(pure reduction). **Cost-FAVOURABLE shift vs V4** — the V5 fold deletes ~3700 LOC MORE.

**Risk:** LOW correct. Pure deletion + one gate-scope patch (P4). No `>SOTA`-bearing code touched:
`src/x86_64/` = 0 real intrinsics (14 `unimplemented!`); `ext/x86/` is DORMANT (`build.rs:38-40`
non-`x86_64` early-return, no aarch64 admission caller, the `lib.rs:247` ref is a doc-comment not a
call site — F15 holds REVISE-not-REJECT because the surface is dead, deleting it is zero aarch64
risk). The headline numbers ride `css_canon_bench` (KEPT — source present) not the deleted
contrived bench (V3 C3). The P3 collapse-vs-differentiate judgement is correctly deferred to B2
(`alphaE:96,109`).

**Same-wave consumer:** present per sub-item (P4 → `accepts_current_allowlist` now meaningful, and
made RED by a re-introduced `JsonSink` token, `alphaE:106`; P3 → runtime `lib.rs` `pub mod
generated_*` roster + `regen.rs` **AND the F13+F16 per-grammar config-tuple collapse structural
check** as a P3-close consumer; P5 → `regen --check`). Correct (`alphaE:102`).

**Scalar-ref/checkasm:** N/A correct — P1 deletes BOTH x86 surfaces (no checkasm in either; the
`ext/x86/` ASM is referenced by no aarch64 admission path). The **12 aarch64 single-kernel
differentials + `checkasm_common.rs` + `checkasm_parity.rs`** are untouched (`alphaE:101`, F4/F14
count); disk confirms 14 `checkasm_*.rs`, 12 single-kernel. Consistent.

**V5 NEW (F15):** the P1 exit gate is now **crate-wide** and **reach-matched** (`alphaE:104`):
`grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`
→ only aarch64-neutral comments, AND `find …/src/x86_64 …/ext/x86 -type f` → 0, AND no `nasm-rs` in
`Cargo.toml`. This closes the V4 `src/`-scoped FALSE-GREEN (a gate that read 0 GREEN while ~3656
LOC of x86 ASM survived under `ext/x86/` + `build.rs`) AND the CH6 V4 §1 RED-by-construction hazard
(deletion-list now reach-matched to the grep — the gate is satisfiable-by-construction). This is a
**cost-FAVOURABLE** strengthening: it deletes more (−4500 vs −847) and the close-gate now actually
certifies "x86 gone" (the R10 binding pin) rather than green-on-dirty. Disk-verified the `--include`
scoping is correct (the prose/doc surfaces are excluded by scoping the grep to `*.rs`/`*.toml`,
SYNTHESIS:117/HANDOFF:110).

**Disposition: ACCEPT.** Mandatory entry-gate; LOC defensible AND deeper (F15 crate-wide); risk
LOW; same-wave consumers present; the V5 F15 crate-wide P1 gate is disk-true, reach-matched, and
cost-favourable; the F13+F16 P3-structural gate is disk-true and cost-free.

---

## §2 — CANDIDATE B1 (G3+G1: un-fork + project JSON) — **ACCEPT**

**LOC:** net **≈ −800** (delete `JSON_*_RS` consts + `json_templates/` 1149 LOC; the projecting
`render` is smaller than the verbatim blobs). Disk supports the direction: `json_sink_direct.rs`
(561 LOC) takes `&SinkOnlyProgram` but emits fixed `push_str` constant bodies — making them
PROJECT nets toward deletion of the 1149-LOC template surface. **−800 remains the softest budget
in the shortlist** (the projecting renderer's true LOC is unknown until written). αE binds the
cost-control: same-wave regen must show `json/generated.rs` within **±5%** of today
(`alphaE:128`). CH4 re-affirms that ±5% generated-line gate as the S-P3 binding cost-control — it
converts the soft estimate into a falsifiable same-wave consumer condition. Unchanged at V5.

**Risk:** MEDIUM correct — JSON is the `>SOTA` holdout with a real hot kernel; the projection must
reproduce the hand-written hot loop exactly. Mitigation present (`json_templates/` held as
byte-for-byte oracle, deleted only after `diff`-match, `[clean-regen-discipline]`, `alphaE:129`).
The thinnest-margin tripwire is correctly named: **apache_builds/parse_only at +1.4% over
sonic-strict** (`alphaE:125`) — a derived parser dropping 1.4% loses `>SOTA` on that row. This is
the load-bearing PRESERVED->SOTA cost-of-failure tripwire and it is named AND propagated into the
contract (SYNTHESIS, HANDOFF carry `+1.4%` = apache_builds thinnest). Unchanged at V5.

**Same-wave consumer:** present — `xtask regen` → `json/generated.rs`, same commit, `regen --check`
+ `generated_real_typed.rs` bench (`alphaE:123`). No orphan.

**Scalar-ref/checkasm:** N/A (codegen layer; the JSON scanner is B4/G5, correctly deferred
`alphaE:121`).

**V5 fold touch:** B1's SINGLE-EMITTER-PATH gate (`alphaE:127`) carries the F13 disclaimer (the
xtask-grep catches a *self-disclosing grammar-token* branch, NOT a neutral-identifier table — the
P3 structural check covers that) AND the F16-widened structural projection by reference. Disk-true
(arm census over codegen+xtask = 0; the neutral-identifier table is policed by the F16 config-tuple
collapse). Cost claim unchanged; the V5 fold is on the gate's projection, zero LOC.

**Disposition: ACCEPT.** No re-block (G3 single-emitter = SK-V17 REDRESS-W2-1 SUBJECT admitted to
discharge, not a re-open — `alphaE:130`/SYNTHESIS:322). ±5% generated-line gate binds the soft LOC.
The F13/F16 reach-scope folds are cost-free and correct.

---

## §3 — CANDIDATE B2 (G2: derive CSS from lowering) — **ACCEPT**

**LOC:** net **≈ −1500**. Disk: `CSS_GENERATED_RS` (910 LOC, `:701`) + `CSS_MOD_RS`/`CSS_PARSER_RS`/
`CSS_SINK_RS` (~590 combined, `:598`/`:612`/`:665`) ⇒ ~1500 const LOC retired, replaced by the
shared B1 renderer parameterized by the CSS program. **−1500 EXACT.** The `[generated-size-budget]`
guard (halt + trace if derived CSS `generated.rs` exceeds hand-written by >20%) is present
(`alphaE:157`). Good cost discipline. Unchanged at V5.

**Risk:** LOW correct and well-supported — V3 A2: the CSS hot path is *already scalar*,
cache-resident; **there is no fragile hand-tuned kernel to preserve**, so `>SOTA` does not ride
hand-shaping. Disk confirms `find_css_significant`/`find_comment_close` are NOT reachable from CSS
runtime non-test code (`#[cfg(test)]`-only) — dead at admission (`alphaE:141`). Risk class
correctly LOW.

**Same-wave consumer:** present — `xtask regen` → `css_l4_*/generated.rs` consumed by the honest
`css_canon_bench.rs` (source present) + `assert_rich_strict_equality:451` (present) 9-field oracle
on the real 71KB–495KB corpus (`alphaE:143`). Both keepers verified on disk.

**Scalar-ref/checkasm:** N/A at codegen (CSS NEON is B4/G6).

**`>SOTA` preservation:** gate#1 pins the **N=200 close-ledger per-row floors** (bootstrap
≥2398.9 / animate ≥2850.0 / tailwind ≥2690.2 / material ≥2540.0 Mbps Track1, `alphaE:149-152`),
H1-framed (lazy-rich-summary vs eager-full-CSSOM), with the N=80 live reproduction as cross-check
only (F1 plane discipline, `alphaE:154`). Threshold preserved honestly — the planes are NOT mixed
in the −3% gate. The honest-finding escape (`alphaE:158`) is the correct cost fallback: a
hand-shaped recognizer becomes a named, `.bbnf`-invoked, checkasm-referenced primitive — not a
silent blob, not a relabeled blob. The cost of a generalization shortfall is a *named primitive*,
not a hidden hand-write.

**V5 fold touch (F16):** the B2 DISTINCT-GRAMMAR-OUTPUT gate (`alphaE:156`) now binds the
relocated-seam check to the **full per-grammar config-tuple collapse** (`count(distinct
(fact_schema, row_id, output_plane, emitter, entry_rule, source_roots)) == 1` per `grammar_name`),
correctly RED today (7 distinct `fact_schema`), GREEN only post-collapse. Disk-confirmed. Cost-free
correction (re-projects an existing data-table); consistent with the contract (SYNTHESIS:322(iii)).

**Disposition: ACCEPT.** Lowest-risk GENERALIZE; LOC EXACT; consumers + keepers verified; the F16
full-config-tuple structural gate is disk-true and cost-free.

---

## §4 — CANDIDATE B3 (G4: shared value trait + kill phantom `<G>`) — **ACCEPT**

**LOC:** net **≈ ±0** (a trait + 2–3 impls replaces hand-copied surface). LOC-neutral is right for
a trait-extraction. The F6 caveat is correct and cost-load-bearing: the **DELETE branch is DEFAULT**
(`abrogate-before-patch`, `alphaE:169`) — keeps ±0; the **INSTANTIATE branch is burden-of-proof**
because `CssEventGrammar` does NOT exist at HEAD (disk: only `JsonEventGrammar` +
`SheetsEventGrammar`, both test-only witnesses) — authoring it is a new grammar-named coupling
surface, **un-budgeted in ±0**. The contract default (DELETE the `G` parameter, `tape/mod.rs:175`)
avoids the un-budgeted LOC. Good cost posture. Unchanged at V5.

**Risk:** MEDIUM correct — the trait must be **zero-cost** (no vtable in the hot path) AND must not
flatten the rich JSON AST (`[preserve-rich-ast]` non-negotiable). The cost-risk is hidden dispatch
cost, gated by `alphaE:181` (JSON `parse_full_traversal`/`path_lookup` + CSS rich-summary within
−3% — if the trait adds vtable/dispatch cost the abstraction is wrong). F7 adds the preserve-rich-ast
structural gate (both-impl grep is necessary-not-sufficient; JSON `get(key)`/typed `Kind`/visitor
must remain reachable THROUGH the trait, `alphaE:179`). Properly fenced — the cost of the
abstraction is bounded above by zero-cost-or-reject.

**Same-wave consumer:** present — both JSON `value_from_ref`/`DocumentView` AND CSS
`CssNode::value()` must `impl` the SAME generated trait in the same commit (no orphan trait,
`alphaE:173`). Phantom `ValueRef<…,G:EventGrammar=AnyGrammar>` verified `tape/mod.rs:175`; the
test-excluded grep (F6 :177, F9 :178) is correct — the standing test-only `JsonEventGrammar` line
must NOT false-green the INSTANTIATE/SHARED-TRAIT gates. The F9 fold (test-excluded +
canonical-trait-named SHARED-TRAIT grep) aligns the research recipe to the SYNTHESIS:394
machine-checked close gate ("≥2 real production instantiations; test-only does NOT count").

**Scalar-ref/checkasm:** N/A (value-API layer).

**Disposition: ACCEPT.** Cost LOC-neutral on the DELETE default; the vtable-dispatch cost-risk is
gated by the −3% zero-cost-trait threshold; the un-budgeted INSTANTIATE LOC is correctly gated
behind burden-of-proof. No V5 change to this candidate; clean.

---

## §5 — CANDIDATE B4 (PROVE Sheets + G5 + G6) — **ACCEPT**

**LOC:** net **≈ +250 (capped)** — PROVE +~200 (Sheets `.bbnf` referenced not authored; generated
runtime falls out of B1; skinny grammar-root + xtask target +~30); G5 −~100 (bespoke scanner
retired onto shared kernel); G6 +~150 (one PMULL body + its 1 checkasm differential). The cap is
real, not open-ended (F5: PMULL `bitmap_prefix_xor_64` is the ONE committed body; every OTHER kernel
RETIRED or honestly relabelled UNLESS a same-wave hot-path consumer exists). `alphaE:211`. Unchanged
at V5. ACCEPT.

**Risk:** MEDIUM-HIGH correct and well-justified — the generalization litmus (3 distinct
`generated.rs`) AND the only real `>SOTA`-regression surface (G5 migrates JSON's bespoke
`json/scan.rs:201` scanner, the speed holdout). G6 PMULL/UDOT are real asm ⇒ full checkasm
discipline (N=12 + each new body adds 1). Correctly the highest-risk candidate.

**Same-wave consumer:** **the strongest axis** — `alphaE:202-205` binds each item to its hot-path
consumer in the same commit (PROVE→Sheets bench + G4 trait; G5→JSON `parse_only` bench; G6 each
kernel WITH its caller; "a kernel with no admission-path consumer is RETIRED, not shipped"). Directly
answers the V5 orphan-kernel pattern. The UDOT orphan is verified on disk (0 runtime callers) ⇒
correctly a wire-or-retire target. SYNTHESIS sharpens further: the **retire branch is gated on a
samply non-top-N MEASUREMENT, not an assertion** (HANDOFF:335 `acceleration_at_admission` ∈
{admission, scalar-passthrough-labeled, retired}, NOT `cfg-test-only`) — it cannot close G6 by
marking all NEON "retired" with zero acceleration wired. Excellent cost posture (no incentive to
fabricate NEON bodies to pass a gate, and no incentive to mark everything retired).

**Scalar-ref status:** STRONG and the spec — every aarch64 kernel has a scalar reference as the
executable spec; the 5 passthrough kernels have scalar twins (`dispatch.rs:80-85`). SK-V18 gives
real NEON bodies (checkasm oracle) OR honestly drops the `_neon` suffix (`alphaE:200`). Correct
`[_neon-suffix-truth]` discipline.

**Checkasm status:** disk = 14 `checkasm_*.rs` = **12 single-kernel + 2 harness/aggregate**
(`checkasm_common.rs`, `checkasm_parity.rs`). αE carries this exactly (`alphaE:101,201` + fold
ledger). Each new NEON body adds exactly 1 differential (N=12 → 12+k). The false-gate hazard ("18
present" un-satisfiable) is removed from αE AND the contract (SYNTHESIS:43,94 carry "12
single-kernel + 2 = 14"; "18" appears only as a correction record); F14 confirms α-E does NOT
inherit the αD-only stale "18".

**V5 fold touch (F16):** the B4 DISTINCT-GRAMMAR-OUTPUT litmus (`alphaE:207`) now carries the
full-config-tuple collapse as the relocated-seam structural defense (the V3 `(source_roots,
entry_rule)`-only projection that missed the 7 distinct `fact_schema` is replaced). The Sheets PROVE
owner-set (`alphaE:197`) requires the new Sheets `RuntimeTarget` to carry a `grammar_name` DISTINCT
from css_l4's AND json's, so the config-tuple collapse counts a genuine third grammar config — the
litmus is non-hollow by construction. Cost-free correction; consistent with the contract.

**`>SOTA` preservation:** G5 gate (`alphaE:210`) names JSON `parse_only` within −3% on
twitter/canada/citm/github, with the correct fallback (V3 F5: expose the JSON string-mask path AS a
parametric kernel rather than regress). CSS rows hold the SAME N=200 per-row floors as B2 gate#1
(F1). Sound.

**Disposition: ACCEPT.** Architecture, risk class, same-wave-consumer discipline are sound; checkasm
count disk-honest; G6 LOC-capped at +~150 with samply-gated retire; the F16 full-config-tuple litmus
is disk-true and cost-free. No remaining cost defect in this candidate.

---

## §6 — αF contract (SYNTHESIS + HANDOFF) cost-inventory review — **ACCEPT**

CH4 reviews the cost-bearing claims in the binding contract artefacts (SYNTHESIS+HANDOFF = αF).
**The V2 CH4 REVISE (SYNTHESIS:348 stale "18 differential harnesses") remains discharged at V5,
AND the V5 F15 + F16 folds are propagated into the contract:**

- **"18"→14 discharged (carried from V3/V4):** SYNTHESIS:43 ("18 differential harnesses is corrected
  to the disk-true 12 single-kernel"); SYNTHESIS:94 ("Section 1 checkasm ledger already carries the
  disk-true 12 single-kernel + 2 = 14; the lone surviving '18' was in" the corrected record).
  **Zero live false count.** Disk: 14 `checkasm_*.rs`, 12 single-kernel.
- **F15 propagated (crate-wide x86):** SYNTHESIS:58-124 records the SECOND x86 surface (`ext/x86/`
  3554 LOC, nasm `build.rs`, `Cargo.toml:19 nasm-rs`, `lib.rs:247` ref), widens P1 crate-wide, and
  reach-matches the deletion list to the `--include`-scoped grep (closing the CH6 V4 §1
  RED-by-construction hazard, SYNTHESIS:103-124). `x86_tree_deleted` telemetry redefined "NO x86
  surface anywhere in `bbnf-simd`" (SYNTHESIS:72). HANDOFF:74-110,217 carry the full removal list +
  the pre-block. **Disk-verified disk-true: 847+3554+102 = 4503 ≈ −4500.**
- **F16 propagated (config-tuple projection):** SYNTHESIS:89-156 records the empirical refutation of
  the `(source_roots,entry_rule)` projection (7 css_l4 rows GREEN on 2 invariant columns but carry 7
  distinct `fact_schema`/`output_plane`/`emitter`), and REDEFINES `runtime_target_rows_collapsed` to
  the full per-grammar config-tuple modulo `output_dir`/`expected_files` (SYNTHESIS:148-155,322(iii),
  397). HANDOFF:21-24,273-279,330-332 carry the same widened column with the RED-pre-P3 / GREEN-post
  semantics. **Disk-verified disk-true: 7 distinct `fact_schema` over shared `(source_roots,
  entry_rule)`.**

**Other αF cost claims verified clean at V5:**
- SYNTHESIS:73,122 — the P1 x86 deletion narrative correctly notes the `ext/x86/`+`.asm` "only
  DEEPENS the net-LOC-deleted claim" (a net-positive correction). SYNTHESIS uses a count/structure
  based, not a stale-LOC-based, P1 close test. No false LOC gate.
- SYNTHESIS:318 P4 `GENERIC_SCAN_ROOTS:2409` extended to scan `runtime_generator.rs` + template
  consts, `"diagnostic-x86"` exclusion removed (x86 gone, P1). The Lock-14 gate becomes meaningful;
  `accepts_current_allowlist` passes ONLY because the leaks are gone (not excluded). Right cost
  posture.
- HANDOFF:335 G6 `acceleration_at_admission` / retire-on-**samply-measurement** makes the G6 cost
  claim machine-checkable per row; `cfg-test-only` is NO-GO for an acceleration claim,
  `retired`/`scalar-passthrough-labeled` are honest non-claims (does not force fabricating NEON
  bodies to pass a gate). Right cost posture.
- αF telemetry cost/generalization columns present (incl. the BOTH V5-fold columns: `x86_tree_deleted`
  ×6 and the F16-widened `runtime_target_rows_collapsed` ×8): `verbatim_blob_present`,
  `emitter_fork_present`, `phantom_generic_resolved`, `generated_md5_distinct`,
  `generator_grammar_count`, `acceleration_at_admission`, `corpus_in_timer`, `sheets_real_grammar`.
  The cost-bearing generalization claims are machine-checkable per row.

**Disposition: ACCEPT.** The V2 REVISE ("18"→14) stays discharged; the V5 F15 crate-wide x86
correction is propagated at SYNTHESIS:58-124 + `x86_tree_deleted` redefinition + HANDOFF:74-110; the
V5 F16 config-tuple correction is propagated at SYNTHESIS:89-156,322(iii) + HANDOFF:21-24,273-279
with the widened `runtime_target_rows_collapsed` column; no live un-satisfiable count, no wrong
owner-path, no stale LOC gate, no RED-by-construction P1 gate remains in the αF artefacts.

---

## §7 — Cross-cutting cost / wave-alignment review — **ACCEPT**

1. **Net LOC ≈ −12650…−12850 (αE SUMMARY:227):** recomputed this pass — A −10600…−10800 (incl.
   crate-wide x86 per F15: P1 ≈ −4500 = 847 `src/` + 3554 `ext/x86/` + 102 `build.rs`; + P3 ≈ −5460
   + P2 ~−700) + B1 −800 + B2 −1500 + B3 ±0 + B4 +250 (capped) ⇒ **≈ −12650…−12850**. A
   generalization cycle that deletes far more than it adds — the correct cost signature for an
   inflection backtrack, **deepened ~3700 LOC by the V5 F15 fold** (the favourable direction). No
   `[generated-size-budget]` overflow on any candidate (every B candidate carries the >20%-overflow
   halt-and-trace guard; A is pure reduction). ACCEPT.

2. **Sequencing / entry-gates:** A → B1 → B2 → B3 → B4, each B entry-gated on its predecessor; P4
   (Lock-14 gate meaningful) lands BEFORE B1 so the un-forked emitter is scanned for neutrality as
   it is built (`alphaE:233`). Right cost-of-coupling ordering. The CH6 exit-gate-blocks-successor
   clause is carried into S-P3 (`alphaE:233`; HANDOFF:301-303,355-358). ACCEPT.

3. **Same-wave consumer — present on EVERY candidate** (the V5 orphan-kernel guard). A: gate is its
   own consumer **+ the F13+F16 config-tuple P3-close check**; B1: `regen`→`json/generated.rs` (±5%
   line gate); B2: `css_canon_bench`+oracle; B3: both trait impls same commit; B4: each kernel WITH
   hot-path caller, orphan ⇒ retire-on-samply-measurement. Uniformly applied. ACCEPT.

4. **Revert protocol / hard caps:** correctly sanctioned-deferred to S-P3 per PASS-ALPHA §4.4
   (HANDOFF:355-358 converts the deferral into "revert dependency graph encoding the entry-gate
   chain + a halt ceiling" — two binding carries). The Pass-Alpha/S-P3 boundary is contract-mandated
   — not a CH4 defect. CH6 owns confirming S-P3 receives the cap binding. ACCEPT.

5. **Telemetry cost-gate columns** machine-check the cost-bearing generalization claims per row
   (`alphaE:236`, SYNTHESIS), now including BOTH V5-fold columns: the F15 `x86_tree_deleted`
   ("NO x86 surface anywhere in `bbnf-simd`") and the F16-widened `runtime_target_rows_collapsed`
   (full per-grammar config-tuple modulo `output_dir`/`expected_files`). The
   `generator_grammar_branch_count==0` column consumes the canonical four-grammar arm census over
   BOTH codegen AND xtask, paired with the type census. Disk-verified all greps land at 0 today
   (arm census), and the F16 config-tuple gate correctly RED (7 distinct `fact_schema`). ACCEPT.

6. **No re-blocked route re-opened (cost of regression):** verified against the pre-block list
   (AZ-IV eager, StructRegistry per-leaf, fact-stream-output, 24-broadcast, FNV-runtime,
   x86/AVX/SVE/nasm — now crate-wide per F15). The shortlist is additive-by-deletion (still exactly
   5 candidates: A, B1–B4; the V4→V5 folds added/removed NO candidate); no candidate re-introduces a
   cost-bearing refuted carrier (CH3 owns the full regression sweep; CH4 confirms no cost re-entry).
   ACCEPT.

7. **F15 + F16 cost-of-correction:** both V5 folds are verification-surface / scope corrections, not
   architecture. **F15** deepens the P1 prune from −847 to ≈ −4500 (cost-FAVOURABLE — deletes a
   second dead x86 surface + the nasm build driver) AND closes the V4 `src/`-scoped false-green +
   the RED-by-construction reach-mismatch (the gate now actually certifies "x86 gone"). **F16** is a
   ZERO-LOC projection widening (re-projects an existing structural check over more of an existing
   data-table's columns — `awk`/`jq`/`sort -u` over data already on disk), strictly STRENGTHENING the
   relocated-seam machine-check (the `(source_roots,entry_rule)`-only projection was false-green on
   the live 7-distinct-`fact_schema` table; the full-config-tuple collapse is correctly RED). Neither
   introduces a `>SOTA` impact, an un-satisfiable gate, or new LOC to evaluate. These are the correct,
   lowest-cost discharges of the V4 cross-artefact REVISEs. ACCEPT.

---

## §8 — Disposition summary

| section | candidate / axis | V4 → V5 | disposition | cost note |
|---|---|---|---|---|
| §1 | CANDIDATE A (PRUNE P1–P5) | ACCEPT → ACCEPT | **ACCEPT** | F15 P1 −847→≈−4500 crate-wide (cost-FAVOURABLE); deletion-list reach-matched to grep (RED-by-construction closed); F13+F16 P3 config-tuple gate disk-true + cost-free; risk LOW |
| §2 | CANDIDATE B1 (G3+G1) | ACCEPT → ACCEPT | **ACCEPT** | −800 softest; ±5% generated-line gate binds it; apache +1.4% tripwire named + in contract; F13/F16 reach-scope cost-free |
| §3 | CANDIDATE B2 (G2) | ACCEPT → ACCEPT | **ACCEPT** | −1500 EXACT; LOW risk (no kernel); keepers verified; F16 full-config-tuple bound to gate, RED today |
| §4 | CANDIDATE B3 (G4) | ACCEPT → ACCEPT | **ACCEPT** | ±0 LOC on DELETE default; vtable-cost gated by −3%; F6/F9 grep-exclusion correct; unchanged at V5 |
| §5 | CANDIDATE B4 (PROVE+G5+G6) | ACCEPT → ACCEPT | **ACCEPT** | checkasm 12/14; G6 +250 capped, samply-gated retire; F16 full-config-tuple litmus disk-true; same-wave-consumer strongest axis |
| §6 | αF contract (SYNTHESIS+HANDOFF) | ACCEPT → ACCEPT | **ACCEPT** | "18"→14 stays discharged; F15 crate-wide x86 + `x86_tree_deleted` redefinition + F16 `runtime_target_rows_collapsed` widening propagated (SYNTHESIS:58-156,322; HANDOFF:74-110,21-24,273-279) |

**V4→V5 delta:** there was **no orphan V4 CH4 REVISE** (CH4 closed V4 at 6A/0R, 100%). The V5 αE
fold ledger introduced two folds neither of which is a CH4-originated defect: **F15** (CH1 §αE /
CH3 — the SECOND x86 surface not propagated into αE's P1 row + exit gate) is folded — P1 widened
crate-wide, exit gate moved `src/`-scoped → crate-wide + reach-matched, LOC corrected −847 → ≈ −4500,
propagated into the contract (SYNTHESIS:58-124; HANDOFF:74-110; `x86_tree_deleted` redefinition);
**F16** (CH2 §8.1 — the relocated-seam projection `(source_roots,entry_rule)` too narrow) is folded
— the projection widened to the full per-grammar config-tuple modulo `output_dir`/`expected_files`,
RED today (7 distinct `fact_schema`), propagated (SYNTHESIS:89-156,322(iii); HANDOFF:21-24,273-279).
**No orphan REVISE remains; no new cost defect introduced; the candidate count is unchanged at 5
(additive-by-deletion); both V5 folds are cost-free (F16) or cost-favourable (F15).**

**Cost verdict:** the candidate shortlist's cost signature (net ≈ −12650…−12850, deepened ~3700 LOC
by the V5 crate-wide x86 fold, every candidate same-wave-consumed, sequenced PRUNE→GENERALIZE→PROVE,
G6 LOC-capped at +~150 with samply-gated retire, checkasm count disk-honest = 12 single-kernel + 2 =
14 in BOTH αE and the binding contract, the V5 F15 crate-wide P1 gate disk-true + reach-matched +
satisfiable-by-construction, and the V5 F16 relocated-seam check widened to a zero-LOC
full-config-tuple structural invariant disk-true-RED-today) is correct for a generalization
backtrack and preserves the `>SOTA` thresholds (JSON ≥ sonic-strict, apache +1.4% tripwire named +
in contract; CSS N=200 per-row floors H1-framed) from the grammar-DERIVED parsers. Every
cost-bearing figure re-verified live on disk this pass. The cohort is convergence-ready on the cost
axis.

TALLY accept=6 revise=0 reject=0
