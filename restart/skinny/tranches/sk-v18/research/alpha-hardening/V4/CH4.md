# CH4 — COST lens (V4) — Pass Alpha SK-V18 alpha-hardening

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
**Method:** every cost/LOC/checkasm/kernel/owner-path/structural-gate claim re-verified LIVE on
disk at the benched skinny tree (`skinny/crates/` + `skinny/xtask/`) at this V4 pass. Citations are
`path:line` from disk. V4 re-checks: (a) the V3 dispositions (ACCEPT ×6, sole REVISE on
SYNTHESIS:348 "18" already discharged) still hold on disk; (b) the NEW V4 αE fold **F13** (the
relocated-overfit-seam machine-check re-attributed from the xtask arm-census regex → the P3
`sort -u` row-count structural check) is disk-true AND propagated into the binding contract; (c)
the no-op **F14** (αD-only stale "18", which α-E does NOT inherit) introduces no cost regression.

---

## §0 — V3→V4 fold status (the prior CH4 dispositions + the new αE folds)

V3 CH4 disposition was **ACCEPT 6 · REVISE 0 · REJECT 0** (`V3/CH4.md:353`). There is **no orphan
V3 CH4 REVISE to discharge** — the sole V2 CH4 REVISE (SYNTHESIS:348 "18 differential harnesses")
was already discharged at V3 and remains so at V4. The V4 αE fold ledger introduces two new folds
(neither is a CH4-originated REVISE; both are cross-artefact CHALLENGE folds α-E records):

| αE V4 fold | source | what it changes | V4 disk state | verdict |
|---|---|---|---|---|
| **F13** | CH2 V3 §8.1 (single open V3 cross-artefact REVISE; touches α-E:185) | re-attributes the **relocated-overfit-seam** machine-check from the xtask-rooted arm-census **regex** (syntactically incapable of firing on a neutral-identifier `RuntimeTarget` DATA table) → the **P3 collapse `sort -u` row-count structural check** (`RuntimeTarget` table = exactly ONE row per distinct `(source_roots, entry_rule)` pair). Verification-surface attribution fix, NOT architectural. | **DISK-TRUE** (see §0.1): 7 `css_l4` `RuntimeTarget` rows → **1** distinct `(source_roots, entry_rule)` pair via `sort -u`; arm-census regex = **0** matches over `xtask/src` AND `codegen/src`. **PROPAGATED** into the contract (SYNTHESIS:80-87, :253(iii); HANDOFF:19-21,260-267,310-311 carry the `runtime_target_rows_collapsed` column). | **FOLDED + DISK-CORRECT** |
| **F14** | CH1 §αD / CH7 §4 | αD-only stale "18 differential harnesses"; **α-E does NOT inherit it** — α-E carries the disk-true **12 single-kernel + 2** in four places (`alphaE:79,179,213` + the F4 fold-ledger line). No-op confirmation in the α-E artefact. | α-E count-correct in 4 places, disk = 14 `checkasm_*.rs` = 12 single-kernel + 2. **No cost change.** | **NO-OP (α-E clean)** |

The V3 CH4 cost cohort is therefore **zero-orphan at V4**. No new cost defect is introduced by the
V4 folds (verified §0.1). The single most-likely cost-regression site — a re-introduction of the
SYNTHESIS:348 false count, or a contract that still mis-attributes the relocated-seam check to the
regex — is verified ABSENT and CORRECTED in the binding artefacts.

### §0.1 — Disk-verified cost ground truth (V4 re-check, this pass)

Every cost-bearing figure re-measured LIVE on disk at the V4 entry HEAD:

| claim (αE / SYNTHESIS / HANDOFF) | disk verification (V4 this pass) | verdict |
|---|---|---|
| P1 x86 tree = 24 files (23 `.rs` + 1 `.asm`) / 742 `.rs` LOC / 105 `.asm` LOC / 14 `unimplemented!` | `find …/x86_64 -type f`=**24**; `.rs`-only LOC=**742**; `.asm`=`byte_class_from_eq_set_64.asm` **105 LOC**; `unimplemented!`=**14** | **EXACT** (incl. F11 `.asm`) |
| P2 `nonjson_css_l4.rs` = 3737 LOC; `measure_mbps:3091`; `lightningcss_facts:528` | `wc -l`=**3737**; `fn measure_mbps`=**:3091**; `pub fn lightningcss_facts`=**:528** | **EXACT** |
| P2 keeps the honest oracle (`assert_rich_strict_equality`) | `pub fn assert_rich_strict_equality`=**:451** (the ONE honest artefact KEPT; consumer call site :3540) | **EXACT** |
| P3 7 CSS `generated.rs`, byte-identical at HEAD | `find css_l4_*/generated.rs`=**7**; total LOC=**6370** (≈910 ea); `md5 at_rules_and_media` ≡ `visual_functions` = `b654562ccff46ed62dd48e9ace325830` | **EXACT** |
| **F13 — `sort -u` over `RuntimeTarget` `(source_roots, entry_rule)` collapses css_l4 to ONE row** | `grep -c 'RuntimeTarget {'`=**7** rows; `(entry_rule, source_roots)` paired+`sort -u`=**1** distinct pair (`entry_rule:"stylesheet"` + `source_roots: CSS_L4_ROOTS` for all 7); `CSS_L4_ROOTS` at `xtask/src/regen_css.rs:24` = `["grammar/css/l4/stylesheet.bbnf"]` | **EXACT — the F13 structural check is disk-true** |
| **F13 — arm-census regex syntactically incapable on the neutral-identifier table** | `rg -nE 'Json\s*=>\|CssL4\s*=>\|(GoogleSheets\|Sheets)\w*\s*=>\|Bbnf\w*\s*=>' xtask/src`=**0**; same over `codegen/src`=**0** (the live `grammar_name:"css_l4"` DATA table carries no `Json =>` token) | **EXACT — confirms the regex cannot fire; the row-count check is the correct defense** |
| B2 `CSS_GENERATED_RS` span | `CSS_GENERATED_RS` def=**:701** (emit site :91; doc-comment :685); `CSS_MOD_RS`=**:598**, `CSS_PARSER_RS`=**:612**, `CSS_SINK_RS`=**:665** | **EXACT** |
| B1 `json_templates/` + `json_sink_direct.rs` | `json_templates/`=**6 files / 1149 LOC**; `json_sink_direct.rs`=**561 LOC** | **EXACT** |
| B1 `RuntimeEmitterKind` fork present | `grammar_provider.rs:40` `pub enum RuntimeEmitterKind`; dispatch `:110` (`!= RuntimeEmitterKind::RequestFacts`) | **EXACT** |
| B4 checkasm count | `ls …/tests/checkasm_*.rs`=**14**; `checkasm_common.rs` + `checkasm_parity.rs` are harness/aggregate ⇒ **12 single-kernel differentials** | **EXACT** (F4/F14 hold) |
| B4 G6 dispatch NEON/scalar twins | NEON reg `dispatch.rs:68-73` (`…_neon`); scalar twins `:80-85` (`…_scalar`); dispatch at `bbnf-simd/src/dispatch.rs` (the ONLY `dispatch.rs`; `aarch64/dispatch.rs` does NOT exist) | **EXACT** (F12 fold holds) |
| B4 UDOT `parse_4_digits_dotprod` orphan | `grep -rn …dotprod crates/runtime/src`=**0** runtime callers | **orphan VERIFIED** |
| B4 CSS NEON test-only / dead at admission | `find_css_significant`/`find_comment_close` defined `runtime_simd.rs:169,112`; callers in `css_l4_at_rules_and_media/generated.rs` = **0** | **dead-at-admission VERIFIED** |
| P5 `parse_w11_1_number` ×7 | `grep -c` in `json/generated.rs`=**7** | **EXACT** |
| P4 gate lines | `GENERIC_SCAN_ROOTS:2409` / `FORBIDDEN_GENERIC_TOKENS:2420` / `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442` / `diagnostic-x86:2463` | **EXACT** |
| G4 phantom `ValueRef<…,G:EventGrammar=AnyGrammar>` | `tape/mod.rs:175` `G: EventGrammar = AnyGrammar`; impls :183/:185/:191 carry `G`; no production bind (default `AnyGrammar`) | **VERIFIED** |
| PROVE Sheets stub | `sheets_witness/` = **25 LOC** (`event_grammar_witness.rs` 24 + `mod.rs` 1) | **EXACT** |
| αF "18 differential" → 14 correction | `grep '18 differential'` over SYNTHESIS+HANDOFF = **only the correction record** (SYNTHESIS:43, HANDOFF:22 both FLAG the old number); the carry-forward ledger reads "12 single-kernel + 2 = 14" (SYNTHESIS:434-435,93) | **DISCHARGED (V2 REVISE remains discharged at V4)** |
| αF telemetry cost columns | `grep -c` of the 8 generalization cost columns + `runtime_target_rows_collapsed` over SYNTHESIS = **25 hits** | **PRESENT** |

**The V4 αE cost ground truth is accurate in full.** No stale count, no wrong owner-path, no
un-satisfiable gate, and — the new V4 surface — the F13 relocated-seam structural check is
disk-true (7→1 `sort -u`) AND propagated into the binding contract. The arm-census regex is
verified syntactically incapable on the neutral-identifier table (0 matches), confirming F13's
re-attribution is correct, not a paper fix.

---

## §1 — CANDIDATE A (PRUNE P1–P5) — **ACCEPT**

**LOC:** αE budgets net **≈ −7200** (`alphaE:97`, incl. F11 `.asm`). Disk recompute this pass:
P1 **−847** (742 `.rs` + 105 `.asm`) + P3 ≈ −5460 (6×910 redundant `generated.rs`) + P2 ~−700
(of 3737-LOC `nonjson_css_l4.rs`, keeping the `assert_rich_strict_equality:451` oracle) + P4
+~15 + P5 rename-only ⇒ **≈ −7000…−7200**. The F11 fold (−847 not −742) is carried on the P1 row
(`alphaE:83` LOC column). Direction favourable; no `[generated-size-budget]` overflow (pure
reduction). Consistent at V4.

**Risk:** LOW correct. Pure deletion + one gate-scope patch (P4). No `>SOTA`-bearing code touched:
x86 = 0 real intrinsics (14 `unimplemented!`); the headline numbers ride `css_canon_bench` (KEPT —
source present) not the deleted contrived bench (V3 C3). The P3 collapse-vs-differentiate judgement
is correctly deferred to B2 (`alphaE:98`).

**Same-wave consumer:** present per sub-item (P4 → `accepts_current_allowlist` now meaningful;
P3 → runtime `lib.rs` `pub mod generated_*` roster + `regen.rs` **AND the new F13 `sort -u`
row-count structural check** as a P3-close consumer; P5 → `regen --check`). Correct (`alphaE:91`).

**Scalar-ref/checkasm:** N/A correct — P1 deletes the x86 tree (no checkasm there). The **12
aarch64 single-kernel differentials + `checkasm_common.rs` + `checkasm_parity.rs`** are untouched
(`alphaE:90`, F4/F14 count); disk confirms 14 `checkasm_*.rs`, 12 single-kernel. Consistent.

**V4 NEW (F13):** the P3 collapse close-gate now carries a SECOND, structural exit condition —
`sort -u` over the `RuntimeTarget` `(source_roots, entry_rule)` pairs = ONE css_l4 config row
(`alphaE:85,94`). Disk-verified disk-true (7 rows → 1 pair). This is a cost-FREE strengthening
(it re-uses an existing structural property of the live table; no new code to write to evaluate it
— it is a `sort -u` over data already on disk). It converts the P3 exit from a file-count check
into a file-count + table-structure check, closing the relocated-seam cost-of-regression at zero
LOC cost. Favourable.

**Disposition: ACCEPT.** Mandatory entry-gate; LOC defensible (floor, F11 `.asm` folded); risk LOW;
same-wave consumers present; the V4 F13 P3-structural gate is disk-true and cost-free.

---

## §2 — CANDIDATE B1 (G3+G1: un-fork + project JSON) — **ACCEPT**

**LOC:** net **≈ −800** (delete `JSON_*_RS` consts + `json_templates/` 1149 LOC; the projecting
`render` is smaller than the verbatim blobs). Disk supports the direction: `json_sink_direct.rs`
(561 LOC) takes `&SinkOnlyProgram` but emits fixed `push_str` constant bodies — making them
PROJECT nets toward deletion of the 1149-LOC template surface. **−800 remains the softest budget
in the shortlist** (the projecting renderer's true LOC is unknown until written). αE binds the
cost-control: same-wave regen must show `json/generated.rs` within **±5%** of today
(`alphaE:117`). CH4 re-affirms that ±5% generated-line gate as the S-P3 binding cost-control — it
converts the soft estimate into a falsifiable same-wave consumer condition. Unchanged at V4.

**Risk:** MEDIUM correct — JSON is the `>SOTA` holdout with a real hot kernel; the projection must
reproduce the hand-written hot loop exactly. Mitigation present (`json_templates/` held as
byte-for-byte oracle, deleted only after `diff`-match, `[clean-regen-discipline]`, `alphaE:118`).
The thinnest-margin tripwire is correctly named: **apache_builds/parse_only at +1.4% over
sonic-strict** (`alphaE:114`) — a derived parser dropping 1.4% loses `>SOTA` on that row. This is
the load-bearing PRESERVED->SOTA cost-of-failure tripwire and it is named AND propagated into the
contract (SYNTHESIS:201, HANDOFF:42-43 carry `+1.4%` = apache_builds thinnest).

**Same-wave consumer:** present — `xtask regen` → `json/generated.rs`, same commit, `regen --check`
+ `generated_real_typed.rs` bench (`alphaE:112`). No orphan.

**Scalar-ref/checkasm:** N/A (codegen layer; the JSON scanner is B4/G5, correctly deferred
`alphaE:110`).

**V4 NEW (F13):** the B1 SINGLE-EMITTER-PATH gate (`alphaE:116`) now correctly disclaims the
xtask-grep reach — it states the xtask root catches a *self-disclosing grammar-token* branch, NOT
a neutral-identifier table (the row-count check covers that). Disk-verified: the arm-census over
`codegen/src skinny/xtask/src` = 0, AND the type census `JsonParser|CssL4Parser|…` is the second
surface. The cost claim (the un-forked emitter is scanned for neutrality as it is built, P4
before B1) is unchanged and correct; only the over-claimed reach is re-scoped — at zero LOC cost.

**Disposition: ACCEPT.** No re-block (G3 single-emitter = SK-V17 REDRESS-W2-1 SUBJECT admitted to
discharge, not a re-open — `alphaE:119`/SYNTHESIS:299). ±5% generated-line gate binds the soft LOC.
The F13 reach-scope fold is cost-free and correct.

---

## §3 — CANDIDATE B2 (G2: derive CSS from lowering) — **ACCEPT**

**LOC:** net **≈ −1500**. Disk: `CSS_GENERATED_RS` (910 LOC, `:701`) + `CSS_MOD_RS`/`CSS_PARSER_RS`/
`CSS_SINK_RS` (~590 combined, `:598`/`:612`/`:665`) ⇒ ~1500 const LOC retired, replaced by the
shared B1 renderer parameterized by the CSS program. **−1500 EXACT.** The `[generated-size-budget]`
guard (halt + trace if derived CSS `generated.rs` exceeds hand-written by >20%) is present
(`alphaE:146`). Good cost discipline. Unchanged at V4.

**Risk:** LOW correct and well-supported — V3 A2: the CSS hot path is *already scalar*,
cache-resident; **there is no fragile hand-tuned kernel to preserve**, so `>SOTA` does not ride
hand-shaping. Disk confirms `find_css_significant`/`find_comment_close` are NOT reachable from CSS
runtime non-test code (0 callers in `css_l4_*/generated.rs`) — dead at admission (`alphaE:130`).
Risk class correctly LOW.

**Same-wave consumer:** present — `xtask regen` → `css_l4_*/generated.rs` consumed by the honest
`css_canon_bench.rs` (source present) + `assert_rich_strict_equality:451` (present) 9-field oracle
on the real 71KB–495KB corpus (`alphaE:132`). Both keepers verified on disk.

**Scalar-ref/checkasm:** N/A at codegen (CSS NEON is B4/G6).

**`>SOTA` preservation:** gate#1 pins the **N=200 close-ledger per-row floors** (bootstrap
≥2398.9 / animate ≥2850.0 / tailwind ≥2690.2 / material ≥2540.0 Mbps Track1, `alphaE:136-141`),
H1-framed (lazy-rich-summary vs eager-full-CSSOM), with the N=80 live reproduction as cross-check
only (F1 plane discipline, `alphaE:143`). Threshold preserved honestly — the planes are NOT mixed
in the −3% gate. The honest-finding escape (`alphaE:147`) is the correct cost fallback: a
hand-shaped recognizer becomes a named, `.bbnf`-invoked, checkasm-referenced primitive — not a
silent blob, not a relabeled blob. The cost of a generalization shortfall is a *named primitive*,
not a hidden hand-write.

**V4 NEW (F13):** the B2 DISTINCT-GRAMMAR-OUTPUT gate (`alphaE:145`) now binds the relocated-seam
check to the P3 `sort -u` structural invariant (disk-true 7→1) rather than the regex, AND retains
the differentiate-branch obligation (if P3 differentiates, each row names its distinct
`(source_roots, entry_rule)`). Cost-free correction; consistent with the contract (SYNTHESIS:253(iii)).

**Disposition: ACCEPT.** Lowest-risk GENERALIZE; LOC EXACT; consumers + keepers verified; the F13
row-count structural gate is disk-true and cost-free.

---

## §4 — CANDIDATE B3 (G4: shared value trait + kill phantom `<G>`) — **ACCEPT**

**LOC:** net **≈ ±0** (a trait + 2–3 impls replaces hand-copied surface). LOC-neutral is right for
a trait-extraction. The F6 caveat is correct and cost-load-bearing: the **DELETE branch is DEFAULT**
(`abrogate-before-patch`, `alphaE:158`) — keeps ±0; the **INSTANTIATE branch is burden-of-proof**
because `CssEventGrammar` does NOT exist at HEAD (disk: only `JsonEventGrammar` +
`SheetsEventGrammar`, both test-only witnesses) — authoring it is a new grammar-named coupling
surface, **un-budgeted in ±0**. The contract default (DELETE the `G` parameter, `tape/mod.rs:175`)
avoids the un-budgeted LOC. Good cost posture. Unchanged at V4.

**Risk:** MEDIUM correct — the trait must be **zero-cost** (no vtable in the hot path) AND must not
flatten the rich JSON AST (`[preserve-rich-ast]` non-negotiable). The cost-risk is hidden dispatch
cost, gated by `alphaE:170` (JSON `parse_full_traversal`/`path_lookup` + CSS rich-summary within
−3% — if the trait adds vtable/dispatch cost the abstraction is wrong). F7 adds the preserve-rich-ast
structural gate (both-impl grep is necessary-not-sufficient; JSON `get(key)`/typed `Kind`/visitor
must remain reachable THROUGH the trait, `alphaE:168`). Properly fenced — the cost of the
abstraction is bounded above by zero-cost-or-reject.

**Same-wave consumer:** present — both JSON `value_from_ref`/`DocumentView` AND CSS
`CssNode::value()` must `impl` the SAME generated trait in the same commit (no orphan trait,
`alphaE:162`). Phantom `ValueRef<…,G:EventGrammar=AnyGrammar>` verified `tape/mod.rs:175`; the
test-excluded grep (F6 :166, F9 :167) is correct — the standing test-only `JsonEventGrammar` line
must NOT false-green the INSTANTIATE/SHARED-TRAIT gates. The F9 fold (test-excluded +
canonical-trait-named SHARED-TRAIT grep) aligns the research recipe to the SYNTHESIS:394
machine-checked close gate ("≥2 real production instantiations; test-only does NOT count").

**Scalar-ref/checkasm:** N/A (value-API layer).

**Disposition: ACCEPT.** Cost LOC-neutral on the DELETE default; the vtable-dispatch cost-risk is
gated by the −3% zero-cost-trait threshold; the un-budgeted INSTANTIATE LOC is correctly gated
behind burden-of-proof. No V4 change to this candidate; clean.

---

## §5 — CANDIDATE B4 (PROVE Sheets + G5 + G6) — **ACCEPT**

**LOC:** net **≈ +250 (capped)** — PROVE +~200 (Sheets `.bbnf` referenced not authored; generated
runtime falls out of B1; skinny grammar-root + xtask target +~30); G5 −~100 (bespoke scanner
retired onto shared kernel); G6 +~150 (one PMULL body + its 1 checkasm differential). The cap is
real, not open-ended (F5: PMULL `bitmap_prefix_xor_64` is the ONE committed body; every OTHER kernel
RETIRED or honestly relabelled UNLESS a same-wave hot-path consumer exists). `alphaE:200`. Unchanged
at V4. ACCEPT.

**Risk:** MEDIUM-HIGH correct and well-justified — the generalization litmus (3 distinct
`generated.rs`) AND the only real `>SOTA`-regression surface (G5 migrates JSON's bespoke
`json/scan.rs:201` scanner, the speed holdout). G6 PMULL/UDOT are real asm ⇒ full checkasm
discipline (N=12 + each new body adds 1). Correctly the highest-risk candidate.

**Same-wave consumer:** **the strongest axis** — αE:191-194 binds each item to its hot-path consumer
in the same commit (PROVE→Sheets bench + G4 trait; G5→JSON `parse_only` bench; G6 each kernel WITH
its caller; "a kernel with no admission-path consumer is RETIRED, not shipped"). Directly answers
the V5 orphan-kernel pattern. The UDOT orphan is verified on disk (0 runtime callers) ⇒ correctly a
wire-or-retire target. SYNTHESIS:302 sharpens further: the **retire branch is gated on a samply
non-top-N MEASUREMENT, not an assertion** — it cannot close G6 by marking all NEON "retired" with
zero acceleration wired. Excellent cost posture (no incentive to fabricate NEON bodies to pass a
gate, and no incentive to mark everything retired).

**Scalar-ref status:** STRONG and the spec — every aarch64 kernel has a scalar reference as the
executable spec; the 5 passthrough kernels have scalar twins (`dispatch.rs:80-85`, disk-verified:
`bitmap_prefix_xor_64_scalar` etc. at :81-85). SK-V18 gives real NEON bodies (checkasm oracle) OR
honestly drops the `_neon` suffix (`alphaE:189`). Correct `[_neon-suffix-truth]` discipline.

**Checkasm status:** disk = 14 `checkasm_*.rs` = **12 single-kernel + 2 harness/aggregate**
(`checkasm_common.rs`, `checkasm_parity.rs`). αE carries this exactly in four places
(`alphaE:90,190,213` + fold-ledger). Each new NEON body adds exactly 1 differential (N=12 → 12+k).
The false-gate hazard ("18 present" un-satisfiable) is removed from αE AND the contract; F14
confirms α-E does NOT inherit the αD-only stale "18".

**V4 NEW (F13):** the B4 DISTINCT-GRAMMAR-OUTPUT litmus (`alphaE:196`) is the most-elaborated F13
site — it correctly splits the threat into three surfaces: (i) self-disclosing `Json =>` arm
(arm-census, 0 on disk), (ii) re-emitted grammar-named TYPE (type census), (iii) relocated branch
into a neutral-identifier `RuntimeTarget` data-table — machine-checked by the P3 `sort -u` row-count
structural check (disk-true 7→1), NOT the regex (which is syntactically incapable, disk-confirmed 0
matches). The Sheets PROVE owner-set (`alphaE:186`) additionally requires the new Sheets
`RuntimeTarget` to carry a `(source_roots, entry_rule)` pair DISTINCT from css_l4's, so the `sort -u`
count rises to a genuine third grammar row — the litmus is non-hollow by construction. Cost-free
correction; consistent with the contract.

**`>SOTA` preservation:** G5 gate (`alphaE:199`) names JSON `parse_only` within −3% on
twitter/canada/citm/github, with the correct fallback (V3 F5: expose the JSON string-mask path AS a
parametric kernel rather than regress). CSS rows hold the SAME N=200 per-row floors as B2 gate#1
(F1). Sound.

**Disposition: ACCEPT.** Architecture, risk class, same-wave-consumer discipline are sound; checkasm
count disk-honest; G6 LOC-capped at +~150 with samply-gated retire; the F13 three-surface litmus is
disk-true and cost-free. No remaining cost defect in this candidate.

---

## §6 — αF contract (SYNTHESIS + HANDOFF) cost-inventory review — **ACCEPT**

CH4 reviews the cost-bearing claims in the binding contract artefacts (SYNTHESIS+HANDOFF = αF).
**The V2 CH4 REVISE (SYNTHESIS:348 stale "18 differential harnesses") remains discharged at V4,
AND the new V4 F13 relocated-seam correction is propagated into the contract:**

- **"18"→14 discharged (carried from V3):** `grep '18 differential'` over SYNTHESIS+HANDOFF returns
  ONLY the correction record (SYNTHESIS:43 "is corrected to the disk-true 12 single-kernel … = 14";
  HANDOFF:22 "the lone surviving '18 differential harnesses'"). The carry-forward ledger reads "12
  single-kernel + 2 = 14" (SYNTHESIS:434-435; cross-ref :93). **Zero live false count.** Disk: 14
  `checkasm_*.rs`, 12 single-kernel.
- **F13 propagated (the V4 surface):** SYNTHESIS:80-87 records the empirical refutation of the
  over-claim ("a NEUTRAL-identifier data-table by construction carries no `Json =>` arm syntax, so
  the arm-census … cannot fire") and binds the relocated-seam check to the P3 collapse close-gate
  ("the xtask `RuntimeTarget` table must carry exactly ONE row per distinct `(source_roots,
  entry_rule)` pair"). SYNTHESIS:253 G3 verify-block carries all three surfaces (i) arm census over
  codegen AND xtask, (ii) type census, (iii) the structural `runtime_target_rows_collapsed`
  `sort -u` row-count check. HANDOFF:19-21,260-267,310-311 carry the `runtime_target_rows_collapsed`
  telemetry column with the same attribution. **Disk-verified disk-true: 7 css_l4 rows → 1 distinct
  pair; arm-census regex = 0 over xtask/src + codegen/src.**

**Other αF cost claims verified clean at V4:**
- SYNTHESIS:73 — the P1 x86 deletion narrative correctly notes the `.asm` "only DEEPENS the
  net-LOC-deleted claim" (a net-positive correction, consistent with the αE −847 P1 row); SYNTHESIS
  uses a count-based, not a stale-LOC-based, P1 close test. No false LOC gate.
- SYNTHESIS:255 G5 cites `bbnf-simd/src/dispatch.rs select_classifier` — correct path (the F12
  `aarch64/` prefix slip is confined to nowhere on disk; the only `dispatch.rs` is `src/dispatch.rs`).
- SYNTHESIS:302 G6 `acceleration_at_admission` / retire-on-**samply-measurement** makes the G6 cost
  claim machine-checkable per row; `cfg-test-only` is NO-GO for an acceleration claim,
  `retired`/`scalar-passthrough-labeled` are honest non-claims (does not force fabricating NEON
  bodies to pass a gate). Right cost posture.
- αF telemetry cost columns present (`grep -c` = 25 hits over the 8 generalization columns +
  `runtime_target_rows_collapsed`): `verbatim_blob_present==false`, `emitter_fork_present==false`,
  `phantom_generic_resolved`, `generated_md5_distinct`, `generator_grammar_count`,
  `acceleration_at_admission`, `corpus_in_timer`, `runtime_target_rows_collapsed`. The cost-bearing
  generalization claims are machine-checkable per row.

**Disposition: ACCEPT.** The V2 REVISE (SYNTHESIS:348 "18"→14) stays discharged; the V4 F13
relocated-seam correction is propagated at SYNTHESIS:80-87,253(iii) and HANDOFF:19-21,260-267,310-311
with the `runtime_target_rows_collapsed` column; no live un-satisfiable count, no wrong owner-path,
no stale LOC gate remains in the αF artefacts.

---

## §7 — Cross-cutting cost / wave-alignment review — **ACCEPT**

1. **Net LOC ≈ −9050…−9250 (αE SUMMARY:216):** recomputed this pass — A −7000…−7200 (incl. `.asm`)
   + B1 −800 + B2 −1500 + B3 ±0 + B4 +250 (capped) ⇒ **≈ −9050…−9250**. A generalization cycle that
   deletes far more than it adds — the correct cost signature for an inflection backtrack. No
   `[generated-size-budget]` overflow on any candidate (every B candidate carries the >20%-overflow
   halt-and-trace guard; A is pure reduction). SYNTHESIS:73 corroborates ("DEEPENS the
   net-LOC-deleted claim"). ACCEPT.

2. **Sequencing / entry-gates:** A → B1 → B2 → B3 → B4, each B entry-gated on its predecessor; P4
   (Lock-14 gate meaningful) lands BEFORE B1 so the un-forked emitter is scanned for neutrality as
   it is built (`alphaE:222`). Right cost-of-coupling ordering. The CH6 §5 exit-gate-blocks-successor
   clause is carried into S-P3 (`alphaE:222`). ACCEPT.

3. **Same-wave consumer — present on EVERY candidate** (the V5 orphan-kernel guard). A: gate is its
   own consumer **+ the new F13 `sort -u` P3-close check**; B1: `regen`→`json/generated.rs` (±5%
   line gate); B2: `css_canon_bench`+oracle; B3: both trait impls same commit; B4: each kernel WITH
   hot-path caller, orphan ⇒ retire-on-samply-measurement. Uniformly applied. ACCEPT.

4. **Revert protocol / hard caps:** correctly sanctioned-deferred to S-P3 per PASS-ALPHA §4.4
   (HANDOFF:581-582 converts the deferral into "revert TBD with a binding dependency graph + a halt
   ceiling"). The Pass-Alpha/S-P3 boundary is contract-mandated — not a CH4 defect. CH6 owns
   confirming S-P3 receives the cap binding. ACCEPT.

5. **Telemetry cost-gate columns** machine-check the cost-bearing generalization claims per row
   (`alphaE:225`, SYNTHESIS), now including the V4 `runtime_target_rows_collapsed == grammar_count`
   structural column (the relocated-seam defense the arm-census regex cannot provide). The
   `generator_grammar_branch_count==0` column consumes the canonical four-grammar arm census over
   BOTH codegen AND xtask, paired with the type census. Disk-verified all greps land at 0 today.
   ACCEPT.

6. **No re-blocked route re-opened (cost of regression):** verified against the V4 pre-block list
   (AZ-IV eager, StructRegistry per-leaf, fact-stream-output, 24-broadcast, FNV-runtime,
   x86/AVX/SVE). The shortlist is additive-by-deletion (still exactly 5 candidates: A, B1–B4; the
   V3→V4 folds added/removed NO candidate); no candidate re-introduces a cost-bearing refuted carrier
   (CH3 owns the full regression sweep; CH4 confirms no cost re-entry). ACCEPT.

7. **F13 cost-of-correction:** the V4 fold is verification-surface attribution, not architectural.
   It re-uses an existing structural property of the live `RuntimeTarget` table (a `sort -u` over
   data already on disk) — **zero LOC to evaluate, zero new code to write, zero `>SOTA` impact**. It
   strictly STRENGTHENS the relocated-seam machine-check (the regex could not catch the
   neutral-identifier table; the row-count check can). This is the correct, lowest-cost discharge of
   the sole open V3 cross-artefact REVISE. ACCEPT.

---

## §8 — Disposition summary

| section | candidate / axis | V3 → V4 | disposition | cost note |
|---|---|---|---|---|
| §1 | CANDIDATE A (PRUNE P1–P5) | ACCEPT → ACCEPT | **ACCEPT** | F11 P1 −847 holds; F13 P3 `sort -u` row-count gate disk-true (7→1) + cost-free; risk LOW |
| §2 | CANDIDATE B1 (G3+G1) | ACCEPT → ACCEPT | **ACCEPT** | −800 softest; ±5% generated-line gate binds it; apache +1.4% tripwire named + in contract; F13 reach-scope cost-free |
| §3 | CANDIDATE B2 (G2) | ACCEPT → ACCEPT | **ACCEPT** | −1500 EXACT; LOW risk (no kernel); keepers verified; F13 row-count bound to gate |
| §4 | CANDIDATE B3 (G4) | ACCEPT → ACCEPT | **ACCEPT** | ±0 LOC on DELETE default; vtable-cost gated by −3%; F9 grep-exclusion correct; unchanged |
| §5 | CANDIDATE B4 (PROVE+G5+G6) | ACCEPT → ACCEPT | **ACCEPT** | checkasm 12/14; G6 +250 capped, samply-gated retire; F13 three-surface litmus disk-true; same-wave-consumer strongest axis |
| §6 | αF contract (SYNTHESIS+HANDOFF) | ACCEPT → ACCEPT | **ACCEPT** | V2 "18"→14 stays discharged; F13 `runtime_target_rows_collapsed` propagated (SYNTHESIS:80-87,253; HANDOFF:19-21,260-267) |

**V3→V4 delta:** there was **no orphan V3 CH4 REVISE** (V3 CH4 closed 6A/0R). The V4 αE fold ledger
introduced two folds neither of which is a CH4-originated defect: **F13** (the sole open V3
cross-artefact REVISE, CH2 §8.1) is folded — the relocated-seam machine-check is re-attributed from
the regex (syntactically incapable on a neutral-identifier table, disk-confirmed 0 matches) to the
P3 `sort -u` row-count structural check (disk-true 7→1), and this is propagated into the binding
contract (SYNTHESIS:80-87, :253(iii); HANDOFF:19-21,260-267,310-311 carry `runtime_target_rows_collapsed`);
**F14** is a no-op confirmation that α-E does NOT inherit the αD-only stale "18". **No orphan REVISE
remains; no new cost defect introduced; the candidate count is unchanged at 5 (additive-by-deletion).**

**Cost verdict:** the candidate shortlist's cost signature (net ≈ −9050…−9250, every candidate
same-wave-consumed, sequenced PRUNE→GENERALIZE→PROVE, G6 LOC-capped at +~150 with samply-gated
retire, checkasm count disk-honest = 12 single-kernel + 2 = 14 in BOTH αE and the binding contract,
and the V4 F13 relocated-seam check bound to a zero-cost `sort -u` structural invariant disk-true
7→1) is correct for a generalization backtrack and preserves the `>SOTA` thresholds (JSON ≥
sonic-strict, apache +1.4% tripwire named + in contract; CSS N=200 per-row floors H1-framed) from
the grammar-DERIVED parsers. Every cost-bearing figure re-verified live on disk this pass. The
cohort is convergence-ready on the cost axis.

TALLY accept=6 revise=0 reject=0
