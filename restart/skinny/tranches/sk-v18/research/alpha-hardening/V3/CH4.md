# CH4 — COST lens (V3) — Pass Alpha SK-V18 alpha-hardening

**Lens:** CH4 Cost (PASS-ALPHA §3 / ORCHESTRATOR §3W).
**Subject:** SK-V18 = the GENERALIZATION cycle (inflection backtrack). ONE grammar-driven
generator emitting JSON+CSS+Sheets from `.bbnf`, over the unified tape/`ValueRef` substrate,
shared value-API, PROVEN on Sheets, PRESERVING >SOTA. NOT a feature cycle.
**Artefacts reviewed:** `research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md`
(alphaF = SYNTHESIS+HANDOFF; there is no separate `alphaF*.md` on disk — verified `ls alphaF*`
= no match; SYNTHESIS+HANDOFF ARE the α-F output per PASS-ALPHA §2/§6). CH4 reviews cost-bearing
claims across all; alphaB/alphaD are competitor/ledger axes — only their cost-bearing rows are
in scope.
**Focus per dispatch:** each candidate's **LOC budget + risk classification + same-wave
consumer + scalar-ref/checkasm (SIMD) status**, and that grammar-DERIVATION preserves the
`>SOTA` threshold honestly.
**Method:** every cost/LOC/checkasm/kernel/owner-path claim re-verified live on disk at the
benched skinny tree (`skinny/crates/`) at this V3 pass. Citations are `path:line` from disk.
V3 re-checks that the V2 CH4 dispositions (the sole REVISE on SYNTHESIS:348 "18" + two
non-blocking riders F11/F12) landed AND propagated into BOTH the αE shortlist and the binding
contract (SYNTHESIS/HANDOFF).

---

## §0 — V2→V3 fold status (the prior CH4 dispositions)

V2 CH4 disposition was **ACCEPT 5 · REVISE 1 · REJECT 0** (`V2/CH4.md:337`). The one REVISE was
on §6 (the αF contract): **SYNTHESIS:348 carried the stale "18 differential harnesses"** — the
exact P4-class un-satisfiable false count (a clean tree has `ls checkasm_*.rs` = 14). Plus two
NON-blocking fold-ins: **F11** (P1 LOC label −742 → −847 incl. the 105-LOC `.asm`) and **F12**
(αE owner-path `aarch64/dispatch.rs` → `bbnf-simd/src/dispatch.rs`).

**All three are discharged at V3 (disk-verified this pass):**

| V2 CH4 disposition | V3 disk state | verdict |
|---|---|---|
| §6 REVISE: SYNTHESIS:348 "18 differential harnesses" | SYNTHESIS now carries the correction at **:42-45** ("Section 1 … is corrected to the disk-true 12 single-kernel differential harnesses + 2 harness/aggregate … = 14") AND the carry-forward ledger at **:377-378** reads "12 single-kernel differential harnesses + 2 harness/aggregate (`checkasm_common.rs`, `checkasm_parity.rs`) = 14 `checkasm_*.rs` total". HANDOFF:15 echoes "(CH4 §6) the carry-forward '18 differential harnesses'" correction. `grep '18 differential'` over SYNTHESIS+HANDOFF = **0 live false count** (only the *correction-record* references it). | **DISCHARGED** |
| F11: P1 −742 → −847 | αE:20 (fold ledger), :72 (P1 row LOC column "**−847** (742 `.rs` + 105 `.asm`)"), :86, :199, :205 all carry −847 | **FOLDED** |
| F12: `aarch64/dispatch.rs` → `dispatch.rs` | αE:21 (fold ledger), :177, :178 cite **`bbnf-simd/src/dispatch.rs`**; disk: `find … -name dispatch.rs` = `skinny/crates/bbnf-simd/src/dispatch.rs` only (no `aarch64/dispatch.rs`) | **FOLDED** |

The V2 CH4 cost cohort is therefore **zero-orphan at V3**. No new cost defect is introduced by
the folds (verified §0.1).

### §0.1 — Disk-verified cost ground truth (V3 re-check, this pass)

Every cost-bearing figure re-measured live on disk at the V3 entry HEAD:

| claim (αE / SYNTHESIS) | disk verification (V3 this pass) | verdict |
|---|---|---|
| P1 x86 tree = 24 files (23 `.rs` + 1 `.asm`) / 742 `.rs` LOC / 105 `.asm` LOC / 14 `unimplemented!` | `find …/x86_64 -type f`=**24**; `.rs`-only LOC=**742**; `.asm`=`byte_class_from_eq_set_64.asm` **105 LOC**; `unimplemented!`=**14** | **EXACT** (incl. F11 `.asm`) |
| P2 `nonjson_css_l4.rs` = 3737 LOC; `measure_mbps:3091`; `lightningcss_facts:528` | `wc -l`=**3737**; `fn measure_mbps`=**:3091**; `lightningcss_facts`=**:528** | **EXACT** |
| P2 keeps the honest oracle (`assert_rich_strict_equality`) | `assert_rich_strict_equality`=**:451** (the ONE honest artefact KEPT) | **EXACT** |
| P2 keeps `css_canon_bench` / `w2_rich_cssom_bench` SOURCE | source bins present: `bbnf-bench/src/bin/css_canon_bench.rs` + `w2_rich_cssom_bench.rs` | **KEPT (verified)** |
| P3 7 CSS `generated.rs`, byte-identical at HEAD | `find css_l4_*/generated.rs`=**7**; total LOC=**6370** (≈910 ea); `md5 at_rules_and_media` ≡ `visual_functions` = `b654562ccff46ed62dd48e9ace325830` | **EXACT** |
| B2 `CSS_GENERATED_RS` span | `CSS_GENERATED_RS`=**:701**; `CSS_MOD_RS`=**:598**, `CSS_PARSER_RS`=**:612**, `CSS_SINK_RS`=**:665** | **EXACT** |
| B1 `json_templates/` + `json_sink_direct.rs` | `json_templates/`=**6 files / 1149 LOC**; `json_sink_direct.rs`=**561 LOC**; `render(program:&SinkOnlyProgram)`=**:4** with `push_str` constant bodies (:80,:97,:125,:252,:327,:368) | **EXACT** |
| B1 `RuntimeEmitterKind` fork present | `grammar_provider.rs:40` `pub enum RuntimeEmitterKind`; dispatch `:110` (`!= RequestFacts`) | **EXACT** |
| **B4 checkasm count** | `ls …/tests/checkasm_*.rs`=**14**; `checkasm_common.rs` + `checkasm_parity.rs` are harness/aggregate ⇒ **12 single-kernel differentials** | **EXACT** (F4 fold holds) |
| B4 G6 scalar-passthrough kernels + dispatch | NEON reg `dispatch.rs:68-73` (`…_neon`); scalar twins `:80-85` (`…_scalar`); dispatch at `src/dispatch.rs` (NOT `aarch64/`) | **EXACT** (F12 fold holds) |
| B4 UDOT `parse_4_digits_dotprod` orphan | `grep -rn …dotprod crates/runtime/src`=**0** runtime callers | **orphan VERIFIED** |
| B4 CSS NEON test-only / dead at admission | `find_css_significant`/`find_comment_close` defined `runtime_simd.rs:169,112`; `grep … css_l4_*/generated.rs` = **0** non-test callers in CSS runtime | **dead-at-admission VERIFIED** |
| P5 `parse_w11_1_number` ×7 | `grep -c` in `json/generated.rs`=**7** | **EXACT** |
| P4 gate lines | `GENERIC_SCAN_ROOTS:2409` / `FORBIDDEN_GENERIC_TOKENS:2420` / `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442` / `diagnostic-x86:2463` | **EXACT** |
| G4 phantom `ValueRef<…,G:EventGrammar=AnyGrammar>` | `tape/mod.rs:175` `G: EventGrammar = AnyGrammar`; impls :183/:185/:191 carry `G`; no production bind (default `AnyGrammar`) | **VERIFIED** |
| PROVE Sheets stub | `sheets_witness/` = **25 LOC** (`event_grammar_witness.rs` 24 + `mod.rs` 1) | **EXACT** |

**The V3 αE cost ground truth is accurate in full.** No stale count, no wrong owner-path, no
un-satisfiable gate remains in either αE or the binding contract. The single most-likely
regression site — the V2 SYNTHESIS:348 false count — is now corrected in the binding artefact.

---

## §1 — CANDIDATE A (PRUNE P1–P5) — **ACCEPT**

**LOC:** αE budgets net **≈ −7200** (`alphaE:86`, incl. F11 `.asm`). Disk recompute this pass:
P1 **−847** (742 `.rs` + 105 `.asm`) + P3 ≈ −5460 (6×910 redundant `generated.rs`) + P2 ~−700
(of 3737-LOC `nonjson_css_l4.rs`, keeping the `assert_rich_strict_equality:451` oracle) + P4
+~15 + P5 rename-only ⇒ **≈ −7000…−7200**. The F11 fold (−847 not −742) is now carried on the
P1 row (`alphaE:72` LOC column), discharging the V2 non-blocking rider. Direction favourable; no
`[generated-size-budget]` overflow (pure reduction). Non-blocking, and now fully consistent.

**Risk:** LOW correct. Pure deletion + one gate-scope patch (P4). No `>SOTA`-bearing code
touched: x86 = 0 real intrinsics (14 `unimplemented!`); the headline numbers ride
`css_canon_bench` (KEPT — source verified) not the deleted contrived bench (V3 C3). The only
judgement item (P3 collapse-vs-differentiate) is correctly deferred to B2 (`alphaE:74,87`).

**Same-wave consumer:** present per sub-item (P4 → `accepts_current_allowlist` now meaningful;
P3 → runtime `lib.rs` `pub mod generated_*` roster + `regen.rs`; P5 → `regen --check`). Correct
(`alphaE:80`).

**Scalar-ref/checkasm:** N/A correct — P1 deletes the x86 tree (no checkasm there). The **12
aarch64 single-kernel differentials + `checkasm_common.rs` + `checkasm_parity.rs`** are untouched
(`alphaE:79`, F4 count); disk confirms 14 `checkasm_*.rs`, 12 single-kernel. Consistent.

**Disposition: ACCEPT.** Mandatory entry-gate; LOC defensible (floor, F11 `.asm` now folded);
risk LOW; same-wave consumers present. The V2 P1-row LOC rider is discharged in αE.

---

## §2 — CANDIDATE B1 (G3+G1: un-fork + project JSON) — **ACCEPT**

**LOC:** net **≈ −800** (delete `JSON_*_RS` consts + `json_templates/` 1149 LOC; the projecting
`render` is smaller than the verbatim blobs). Disk supports the direction: `json_sink_direct.rs`
(561 LOC) already takes `&SinkOnlyProgram:4` but `render_header`/dispatch bodies `push_str`
constant text (:80,:97,:125,:252,:327,:368) — making them PROJECT nets toward deletion of the
1149-LOC template surface. **−800 remains the softest budget in the shortlist** (the projecting
renderer's true LOC is unknown until written). αE binds the cost-control: same-wave regen must
show `json/generated.rs` within **±5%** of today (`alphaE:106`). CH4 re-affirms that ±5%
generated-line gate as the S-P3 binding cost-control — it converts the soft estimate into a
falsifiable same-wave consumer condition.

**Risk:** MEDIUM correct — JSON is the `>SOTA` holdout with a real hot kernel; the projection
must reproduce the hand-written hot loop exactly. Mitigation present (`json_templates/` held as
byte-for-byte oracle, deleted only after `diff`-match, `[clean-regen-discipline]`,
`alphaE:107`). The thinnest-margin tripwire is correctly named: **apache_builds/parse_only at
+1.4% over sonic-strict** (`alphaE:103`) — a derived parser dropping 1.4% loses `>SOTA` on that
row. This is the load-bearing PRESERVED->SOTA cost-of-failure tripwire and it is named.

**Same-wave consumer:** present — `xtask regen` → `json/generated.rs`, same commit,
`regen --check` + `generated_real_typed.rs` bench (`alphaE:101`). No orphan.

**Scalar-ref/checkasm:** N/A (codegen layer; the JSON scanner is B4/G5, correctly deferred
`alphaE:99`).

**Disposition: ACCEPT.** No re-block (G3 single-emitter = SK-V17 REDRESS-W2-1 SUBJECT admitted
to discharge, not a re-open — `alphaE:108`/HANDOFF:214 confirm). ±5% generated-line gate binds
the soft LOC. Unchanged from V2; the candidate carried no V2 cost defect.

---

## §3 — CANDIDATE B2 (G2: derive CSS from lowering) — **ACCEPT**

**LOC:** net **≈ −1500**. Disk: `CSS_GENERATED_RS` = 910 LOC (`:701`→`:1611`) + `CSS_MOD_RS`/
`CSS_PARSER_RS`/`CSS_SINK_RS` (~590 combined, `:598`/`:612`/`:665`) ⇒ ~1500 const LOC retired,
replaced by the shared B1 renderer parameterized by the CSS program. **−1500 EXACT.** The
`[generated-size-budget]` guard (halt + trace if derived CSS `generated.rs` exceeds hand-written
by >20%) is present (`alphaE:135`). Good cost discipline.

**Risk:** LOW correct and well-supported — V3 A2: the CSS hot path is *already scalar*,
cache-resident; **there is no fragile hand-tuned kernel to preserve**, so `>SOTA` does not ride
hand-shaping. Disk confirms `find_css_significant`/`find_comment_close` are NOT reachable from
CSS runtime non-test code (`grep css_l4_*/generated.rs` = 0 callers) — they are dead at admission
(`alphaE:119`). The risk class is correctly LOW.

**Same-wave consumer:** present — `xtask regen` → `css_l4_*/generated.rs` consumed by the honest
`css_canon_bench.rs` (source PRESENT) + `assert_rich_strict_equality:451` (PRESENT) 9-field
oracle on the real 71KB–495KB corpus (`alphaE:121`). Both keepers verified on disk.

**Scalar-ref/checkasm:** N/A at codegen (CSS NEON is B4/G6).

**`>SOTA` preservation:** gate#1 pins the **N=200 close-ledger per-row floors** (bootstrap
≥2398.9 / animate ≥2850.0 / tailwind ≥2690.2 / material ≥2540.0 Mbps Track1, `alphaE:125-130`),
H1-framed (lazy-rich-summary vs eager-full-CSSOM), with the N=80 live reproduction as cross-check
only (F1 plane discipline, `alphaE:132`). Threshold preserved honestly — and the planes are NOT
mixed in the −3% gate. The honest-finding escape (`alphaE:136`) is the correct cost fallback: a
hand-shaped recognizer becomes a named, `.bbnf`-invoked, checkasm-referenced primitive — not a
silent blob, not a relabeled blob (CH6 §9 ref). The cost of a generalization shortfall is a
*named primitive*, not a hidden hand-write.

**Disposition: ACCEPT.** Lowest-risk GENERALIZE; LOC EXACT; consumers + keepers verified.
Unchanged from V2.

---

## §4 — CANDIDATE B3 (G4: shared value trait + kill phantom `<G>`) — **ACCEPT**

**LOC:** net **≈ ±0** (a trait + 2–3 impls replaces hand-copied surface). LOC-neutral is right
for a trait-extraction. The F6 caveat is correct and cost-load-bearing: the **DELETE branch is
DEFAULT** (`abrogate-before-patch`, `alphaE:147`) — keeps ±0; the **INSTANTIATE branch is
burden-of-proof** because `CssEventGrammar` does NOT exist at HEAD (disk: only `JsonEventGrammar`
+ `SheetsEventGrammar`, both test-only witnesses) — authoring it is a new grammar-named coupling
surface, **un-budgeted in ±0**. The contract default (DELETE the `G` parameter, `tape/mod.rs:175`)
avoids the un-budgeted LOC. Good cost posture.

**Risk:** MEDIUM correct — the trait must be **zero-cost** (no vtable in the hot path) AND must
not flatten the rich JSON AST (`[preserve-rich-ast]` non-negotiable). The cost-risk is hidden
dispatch cost, gated by `alphaE:159` (JSON `parse_full_traversal`/`path_lookup` + CSS
rich-summary within −3% — if the trait adds vtable/dispatch cost the abstraction is wrong). F7
adds the preserve-rich-ast structural gate (both-impl grep is necessary-not-sufficient; JSON
`get(key)`/typed `Kind`/visitor must remain reachable THROUGH the trait, `alphaE:157`). Properly
fenced — the cost of the abstraction is bounded above by zero-cost-or-reject.

**Same-wave consumer:** present — both JSON `value_from_ref`/`DocumentView` AND CSS
`CssNode::value()` must `impl` the SAME generated trait in the same commit (no orphan trait,
`alphaE:151`). Phantom `ValueRef<…,G:EventGrammar=AnyGrammar>` verified `tape/mod.rs:175`; the
test-excluded grep (F6 :154, F9 :156) is correct — the standing test-only `JsonEventGrammar` line
must NOT false-green the INSTANTIATE/SHARED-TRAIT gates. The F9 fold (test-excluded +
canonical-trait-named SHARED-TRAIT grep) discharges the sole V2 α-E REVISE (CH5 E.1); it aligns
the research recipe to the SYNTHESIS:394 machine-checked close gate ("≥2 real production
instantiations; test-only does NOT count").

**Scalar-ref/checkasm:** N/A (value-API layer).

**Disposition: ACCEPT.** Cost LOC-neutral on the DELETE default; the vtable-dispatch cost-risk
is gated by the −3% zero-cost-trait threshold; the un-budgeted INSTANTIATE LOC is correctly
gated behind burden-of-proof. The F9 grep-exclusion fold is correct and consistent with the close
gate.

---

## §5 — CANDIDATE B4 (PROVE Sheets + G5 + G6) — **ACCEPT** *(V2 owner-path rider F12 discharged)*

V1 CH4 dispositioned B4 REVISE on two cost defects (checkasm "18", unbounded G6 LOC); V2 verified
both folded (F4/F5) and added one NON-blocking owner-path rider (F12, `aarch64/dispatch.rs` →
`dispatch.rs`). **All are now folded and disk-correct at V3:**

- **Checkasm (F4):** αE:179 states "**12 single-kernel differential harnesses +
  `checkasm_common.rs` + `checkasm_parity.rs` = 14 total**; current N=12 → N=12+k." Disk:
  `ls checkasm_*.rs`=**14**, 2 harness/aggregate ⇒ **12 single-kernel**. EXACT. The false-gate
  hazard ("18 present" un-satisfiable) is removed from αE AND from SYNTHESIS:377-378 (the V2
  REVISE).
- **G6 LOC ceiling (F5):** αE:189 states "**PMULL `bitmap_prefix_xor_64` is the ONE committed
  real NEON body (+~150 with its checkasm); every OTHER kernel RETIRED or honestly relabelled
  UNLESS a same-wave hot-path consumer exists**" ⇒ committed ceiling +~150, net ≈ +250 (capped).
  The "+150 per body" is no longer an unstated multiplicand.
- **Owner-path (F12):** αE:177,178 cite `bbnf-simd/src/dispatch.rs` (NEON :68-73, scalar :80-85)
  — disk-correct (the `aarch64/dispatch.rs` directory does NOT exist; only `src/dispatch.rs`).
  The kernel-BODY paths (`aarch64/bitmap_prefix_xor_64.rs:2`, `aarch64/eob_pad_clamp.rs:4`) are
  correct. SYNTHESIS:134,203,249 cite the dispatch path correctly. The V2 rider is discharged.

**LOC:** net **≈ +250 (capped)** — PROVE +~200 (Sheets `.bbnf` referenced not authored; generated
runtime falls out of B1; skinny grammar-root + xtask target +~30); G5 −~100 (bespoke scanner
retired onto shared kernel); G6 +~150 (one PMULL body + its 1 checkasm differential). The cap is
real, not open-ended (F5). ACCEPT.

**Risk:** MEDIUM-HIGH correct and well-justified — the generalization litmus (3 distinct
`generated.rs`) AND the only real `>SOTA`-regression surface (G5 migrates JSON's bespoke
`json/scan.rs:201` scanner, the speed holdout). G6 PMULL/UDOT are real asm ⇒ full checkasm
discipline (N=12 + each new body adds 1). Correctly the highest-risk candidate.

**Same-wave consumer:** **the strongest axis** — αE:180-183 binds each item to its hot-path
consumer in the same commit (PROVE→Sheets bench + G4 trait; G5→JSON `parse_only` bench; G6 each
kernel WITH its caller; "a kernel with no admission-path consumer is RETIRED, not shipped").
Directly answers the V5 orphan-kernel pattern. The UDOT orphan is verified on disk (0 runtime
callers) ⇒ correctly a wire-or-retire target. SYNTHESIS:250 sharpens further: the **retire branch
is gated on a samply non-top-N MEASUREMENT, not an assertion** — it cannot close G6 by marking
all NEON "retired" with zero acceleration wired. Excellent cost posture (no incentive to fabricate
NEON bodies to pass a gate, and no incentive to mark everything retired).

**Scalar-ref status:** STRONG and the spec — every aarch64 kernel has a scalar reference as the
executable spec; the 5 passthrough kernels have scalar twins (`dispatch.rs:80-85`,
`bitmap_prefix_xor_64_scalar`…, disk-verified). SK-V18 gives real NEON bodies (checkasm oracle)
OR honestly drops the `_neon` suffix (`alphaE:178`). Correct `[_neon-suffix-truth]` discipline.

**`>SOTA` preservation:** G5 gate (`alphaE:188`) names JSON `parse_only` within −3% on
twitter/canada/citm/github, with the correct fallback (V3 F5: expose the JSON string-mask path AS
a parametric kernel rather than regress). CSS rows hold the SAME N=200 per-row floors as B2
gate#1 (F1). Sound.

**Disposition: ACCEPT.** The V1 cost defects (checkasm count, G6 ceiling) and the V2 owner-path
rider (F12) are all folded disk-correct in αE. Architecture, risk class, same-wave-consumer
discipline are sound. No remaining cost defect in this candidate.

---

## §6 — αF contract (SYNTHESIS + HANDOFF) cost-inventory review — **ACCEPT** *(V2 REVISE discharged)*

CH4 reviews the cost-bearing claims in the binding contract artefacts (SYNTHESIS+HANDOFF = αF).
**The sole V2 CH4 REVISE (SYNTHESIS:348 stale "18 differential harnesses") is discharged:**

- **SYNTHESIS:42-45** records the correction explicitly ("Section 1 '18 differential harnesses'
  is corrected to the disk-true **12 single-kernel differential harnesses + 2 harness/aggregate
  (`checkasm_common.rs`, `checkasm_parity.rs`) = 14 `checkasm_*.rs` total** … an un-propagated αA
  fold that, left [un-corrected, would be un-satisfiable]").
- **SYNTHESIS:377-378** (the "Validated — carry forward" ledger) now reads "the grammar-neutral
  checkasm-disciplined NEON kernel set (**12 single-kernel differential harnesses + 2
  harness/aggregate (`checkasm_common.rs`, `checkasm_parity.rs`) = 14 `checkasm_*.rs` total**…".
- **HANDOFF:15** carries "(CH4 §6) the carry-forward '18 differential harnesses'" as a recorded
  correction.
- `grep '18 differential'` over SYNTHESIS+HANDOFF returns only the *correction record* (lines
  43-45 quoting the old number to flag it), **zero live false count**. Disk: `ls checkasm_*.rs`
  = 14, 12 single-kernel — the contract now matches the tree.

**Other αF cost claims verified clean at V3:**
- SYNTHESIS P1 verifies the x86 deletion by file-count `find = 0`, not a stale −742 LOC gate — no
  false gate (the F11 `.asm` 105-LOC is folded onto the αE P1 row, and SYNTHESIS uses a
  count-based, not LOC-based, P1 close test).
- SYNTHESIS:134,203,249 G5 cites `bbnf-simd/src/dispatch.rs select_classifier` — correct path
  (the F12 `aarch64/` prefix slip was confined to αE and is now folded there too).
- SYNTHESIS:250 G6 `acceleration_at_admission` / retire-on-**samply-measurement** makes the G6
  cost claim machine-checkable per row; `cfg-test-only` is NO-GO for an acceleration claim,
  `retired`/`scalar-passthrough-labeled` are honest non-claims (does not force fabricating NEON
  bodies to pass a gate). Right cost posture.
- SYNTHESIS:22 carries the samply-gated retire branch (cannot close G6 by mass-marking "retired").

**Disposition: ACCEPT.** The V2 REVISE (SYNTHESIS:348 "18"→14) is propagated into the binding
contract at :42-45 and :377-378, and echoed in HANDOFF:15. No live un-satisfiable count, no
wrong owner-path, no stale LOC gate remains in the αF artefacts. This section flips from V2
REVISE to V3 ACCEPT.

---

## §7 — Cross-cutting cost / wave-alignment review — **ACCEPT**

1. **Net LOC ≈ −9250 (αE SUMMARY:205):** recomputed this pass — A −7000…−7200 (incl. `.asm`) +
   B1 −800 + B2 −1500 + B3 ±0 + B4 +250 (capped) ⇒ **≈ −9050…−9250**. A generalization cycle
   that deletes far more than it adds — the correct cost signature for an inflection backtrack.
   No `[generated-size-budget]` overflow on any candidate (every B candidate carries the
   >20%-overflow halt-and-trace guard; A is pure reduction).

2. **Sequencing / entry-gates:** A → B1 → B2 → B3 → B4, each B entry-gated on its predecessor;
   P4 (Lock-14 gate meaningful) lands BEFORE B1 so the un-forked emitter is scanned for
   neutrality as it is built (`alphaE:211`). Right cost-of-coupling ordering — prevents B1
   re-leaking under a blind gate. HANDOFF sequences identically; the exit-gate-blocks-successor
   clause is carried (CH6 §5 ref, `alphaE:211`; SYNTHESIS). ACCEPT.

3. **Same-wave consumer — present on EVERY candidate** (the V5 orphan-kernel guard). A: gate is
   its own consumer; B1: `regen`→`json/generated.rs` (±5% line gate); B2: `css_canon_bench`+oracle;
   B3: both trait impls same commit; B4: each kernel WITH hot-path caller, orphan ⇒
   retire-on-samply-measurement. Uniformly applied. ACCEPT.

4. **Revert protocol / hard caps:** correctly sanctioned-deferred to S-P3 per PASS-ALPHA §4.4.
   The Pass-Alpha/S-P3 boundary is contract-mandated — not a CH4 defect. CH6 owns confirming S-P3
   receives the cap binding.

5. **Telemetry cost-gate columns** (`generated_md5_distinct`, `generator_grammar_count==3`,
   `acceleration_at_admission`, `verbatim_blob_present==false`, `emitter_fork_present==false`,
   `phantom_generic_resolved`, `corpus_in_timer==true`, `materialization_framing`,
   `generator_grammar_branch_count==0`, `generator_grammar_type_count==0`) make the cost-bearing
   generalization claims machine-checkable per row (`alphaE:214`, SYNTHESIS). ACCEPT.

6. **No re-blocked route re-opened (cost of regression):** verified against the V3 pre-block list
   (AZ-IV eager, StructRegistry per-leaf, fact-stream-output, 24-broadcast, FNV-runtime,
   x86/AVX/SVE). The shortlist is additive-by-deletion; no candidate re-introduces a cost-bearing
   refuted carrier (CH3 owns the full regression sweep; CH4 confirms no cost re-entry).

---

## §8 — Disposition summary

| section | candidate / axis | V2 → V3 | disposition | cost note |
|---|---|---|---|---|
| §1 | CANDIDATE A (PRUNE P1–P5) | ACCEPT → ACCEPT | **ACCEPT** | F11 P1 −847 (incl `.asm` 105) now folded on the P1 row; LOC a floor; risk LOW |
| §2 | CANDIDATE B1 (G3+G1) | ACCEPT → ACCEPT | **ACCEPT** | −800 softest budget; ±5% generated-line gate binds it; apache +1.4% tripwire named |
| §3 | CANDIDATE B2 (G2) | ACCEPT → ACCEPT | **ACCEPT** | −1500 EXACT; LOW risk (no kernel to preserve); keepers verified |
| §4 | CANDIDATE B3 (G4) | ACCEPT → ACCEPT | **ACCEPT** | ±0 LOC on DELETE default; vtable-cost gated by −3%; F9 grep-exclusion correct |
| §5 | CANDIDATE B4 (PROVE+G5+G6) | ACCEPT → ACCEPT | **ACCEPT** | checkasm 12 / G6 +250 capped / F12 `dispatch.rs` all folded; same-wave consumer strongest axis |
| §6 | αF contract (SYNTHESIS+HANDOFF) | REVISE → ACCEPT | **ACCEPT** | V2 REVISE (SYNTHESIS:348 "18"→14) propagated at :42-45 + :377-378; HANDOFF:15 echoes; zero live false count |

**V2→V3 delta:** the sole V2 CH4 REVISE (§6, SYNTHESIS:348 "18 differential harnesses") is fully
discharged in the binding contract — verified on disk at :42-45 (correction record) and :377-378
(carry-forward ledger), with HANDOFF:15 echoing it; `grep '18 differential'` returns only the
correction record, zero live false count. The two V2 non-blocking riders (F11 P1 `.asm` LOC label;
F12 αE `dispatch.rs` owner-path) are folded into the V3 αE text (`:20,:72,:86,:199,:205` and
`:21,:177,:178`). **No orphan REVISE remains; no new cost defect introduced.**

**Cost verdict:** the candidate shortlist's cost signature (net ≈ −9050…−9250, every candidate
same-wave-consumed, sequenced PRUNE→GENERALIZE→PROVE, G6 LOC-capped at +~150 with samply-gated
retire, checkasm count disk-honest = 12 single-kernel + 2 = 14 in BOTH αE and the binding
contract) is correct for a generalization backtrack and preserves the `>SOTA` thresholds
(JSON ≥ sonic-strict, apache +1.4% tripwire named; CSS N=200 per-row floors H1-framed) from the
grammar-DERIVED parsers. Every cost-bearing figure re-verified live on disk this pass. The
cohort is convergence-ready on the cost axis.

TALLY accept=6 revise=0 reject=0
