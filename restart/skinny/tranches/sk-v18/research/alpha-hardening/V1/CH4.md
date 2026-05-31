# CH4 — COST lens (V1) — Pass Alpha SK-V18 alpha-hardening

**Lens:** CH4 Cost (PASS-ALPHA §3 / ORCHESTRATOR §3W).
**Subject:** SK-V18 = the GENERALIZATION cycle (inflection backtrack). One grammar-driven
generator emitting JSON+CSS+Sheets from `.bbnf`, over the unified tape/`ValueRef`
substrate, shared value-API, PROVEN on Sheets, PRESERVING >SOTA. NOT a feature cycle.
**Artefacts reviewed:** `research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md` +
`SYNTHESIS.md` + `HANDOFF.md` (alphaF). (`alphaB`/`alphaD` are competitor/ledger axes —
CH4 reviews only their cost-bearing claims.)
**Focus per dispatch:** each candidate's **LOC budget + risk classification + same-wave
consumer + scalar-ref/checkasm (SIMD) status**, and that grammar-DERIVATION preserves the
`>SOTA` threshold honestly.
**Method:** every cost/LOC/checkasm/kernel claim re-verified live on disk at the benched
skinny tree (`skinny/crates/`). Working tree carries the regen-noise M-flags noted by αA §6;
replica-identity claims pinned to `f6a38445b` per αA. Citations are `path:line` from disk.

---

## §0 — Disk-verified cost ground truth (the basis for every disposition)

| claim (artefact) | disk verification | verdict |
|---|---|---|
| P1 x86 tree = 742 LOC / 24 files / 14 `unimplemented!` | `find crates/bbnf-simd/src/x86_64 -type f`=**24**; `wc -l`=**742** | EXACT |
| P3 7 CSS `generated.rs` replicas | `find css_l4_*/generated.rs`=**7**; total `wc -l`=**6370** (≈910 ea) | count EXACT; LOC see §P3 |
| B2 `CSS_GENERATED_RS` const span | `runtime_generator.rs:701`→const ends **:1611** = **~910 LOC** | EXACT (matches "910") |
| B1 `json_templates/` | **6 files / 1149 LOC**; `JSON_PARSE_ONLY_GENERATED_RS:195` | confirms blob surface |
| B1 `json_sink_direct.rs` | **561 LOC**; `render(program:&SinkOnlyProgram):4`; `render_header`+ `push_str` constant bodies `:80,97,125,252,327,368` | confirms "pushes constant text" |
| B4 checkasm "18 differential harnesses" | `ls checkasm_*.rs \| wc -l`=**14** (incl. `checkasm_common.rs`, `checkasm_parity.rs`) ⇒ ~**12 single-kernel differentials** | **WRONG — overcount (see §B4-defect)** |
| B4 "5 scalar-passthrough kernels at `dispatch.rs:66-74`/`:68-73`" | kernels registered `dispatch.rs:67-85` (NEON `:67-73` + scalar `:79-85`); **bodies live in `aarch64/<kernel>.rs`**, not dispatch | path imprecise (see §B4) |
| B4 UDOT `parse_4_digits_dotprod` orphan | `grep -rn parse_4_digits_dotprod crates/runtime/src`=**0** runtime callers | orphan VERIFIED |
| P5 `parse_w11_1_number` ×7 | `grep -c`=**7** in `json/generated.rs` | EXACT |
| P4 `GENERIC_SCAN_ROOTS:2409` / `FORBIDDEN_GENERIC_TOKENS:2420` / `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442` / `diagnostic-x86:2463` | all four line numbers EXACT | EXACT |
| P2 `measure_mbps:3091` / `lightningcss_facts:528` / `EXPECTED_FIXTURE_BYTES:66` | EXACT (`nonjson_css_l4.rs` 3737 LOC) | EXACT |
| B2/H1 keepers `assert_rich_strict_equality:451` + `css_canon_bench.rs` | both PRESENT | EXACT |
| G4 phantom `ValueRef<…,G:EventGrammar=AnyGrammar>` | `tape/mod.rs:175`; `G` only ever `AnyGrammar` | VERIFIED |
| PROVE Sheets stub | `sheets_witness/`=**25 LOC** (24+1) | EXACT |

The cost ground truth is overwhelmingly accurate. **One material defect (B4 checkasm
overcount) + two minor path imprecisions (P3 file-count framing, B4 dispatch line).**

---

## §1 — CANDIDATE A (PRUNE P1–P5) — **ACCEPT**

**LOC:** αE budgets net **≈ −7100**. Disk: P1 −742 (exact) + P3 collapse of 6 of 7
replicas ≈ **−5460** (6×910; not the αE "−5000", a conservative ~9% under-count — favours
the budget, no overflow risk) + P2 ~−700 (of 3737 LOC `nonjson_css_l4.rs`; the
`measure_mbps`/`lightningcss_facts`/fixture-pin subset is plausible) + P4 +~15 + P5
rename-only. Sum ≈ **−6900…−7100**. **The −7100 is defensible; if anything understated.**
Per `[generated-size-budget]` this is a pure reduction — zero overflow.

**Risk:** LOW is correct. Pure deletion + one gate-scope patch (P4). No `>SOTA`-bearing
code touched: V3 C3 confirms the old bench did NOT produce the headline (those are
`css_canon_bench`, KEPT — verified present). x86 = 0 real intrinsics (verified). The only
judgement item (P3 collapse-vs-differentiate) is correctly deferred to B2.

**Same-wave consumer:** present and correct per sub-item — P4's consumer is its own
`accepts_current_allowlist` (now meaningful); P3's is the runtime `lib.rs` `pub mod` roster +
`regen.rs`; P5's is `regen --check`. ACCEPT.

**Scalar-ref/checkasm:** N/A correctly stated — P1 deletes the x86 tree (no checkasm), the
aarch64 differentials are untouched. **But the inventory it cites as "untouched (18
harnesses)" is itself wrong — see §B4.** That overcount does not change CANDIDATE A's
disposition (A touches no checkasm), but it must be corrected wherever stated.

**REVISE-rider (non-blocking, folds into A's text):** §P3 "−~5000 (**35→5 files**)"
conflates two counts. On disk there are **35 total files** across the 7 `css_l4_*` dirs but
only **7 `generated.rs`** (6370 LOC). Collapsing the grammar to one CSS target removes ~6
dirs (~30 files) but the LOC mover is the 6 redundant `generated.rs` (≈−5460). The "35→5
files" phrasing is ambiguous (is "5" dirs? files?) and should read "7 `generated.rs` → 1 (or
N-distinct); ~30 ancillary files in 6 collapsed dirs." Cost conclusion unchanged.

**Disposition: ACCEPT** (with the P3 file-count phrasing revise folded into the candidate
text; `alphaE-candidate-shortlist.md:42,151`). No re-block; A is the mandatory entry-gate.

---

## §2 — CANDIDATE B1 (G3+G1: un-fork + project JSON) — **ACCEPT**

**LOC:** net **≈ −800** (delete `JSON_*_RS` consts `:195`+ `json_templates/` 1149 LOC; the
projecting `render` is smaller than the verbatim blobs). Disk supports the *direction*:
`json_sink_direct.rs` (561 LOC) already takes `&SinkOnlyProgram:4` but `render_header`/
`render_value_dispatch` `push_str` constant bodies (`:80,97,125,252,327,368`) — making them
*project* nets toward deletion of the 1149-LOC template surface. **−800 is plausible**, though
it is the *softest* budget in the shortlist (the projecting renderer's true LOC is unknown
until written). CH4 accepts with the explicit S-P3 instruction: the same-wave regen MUST show
`json/generated.rs` within ±5% of today (αE already states this, `:74`).

**Risk:** MEDIUM is correct — JSON is the `>SOTA` holdout with a real hot kernel; the
projection must reproduce the hand-written hot loop exactly (αE mitigation: `json_templates/`
held as byte-for-byte oracle through the wave, deleted only after `diff`-match,
`[clean-regen-discipline]`). Sound. **The thinnest-margin tripwire is correctly named**:
αA §1 records apache_builds/parse_only at **+1.4%** vs sonic-strict — a derived JSON parser
that drops 1.4% loses `>SOTA` on that row. CH4 flags this as the highest cost-of-regression
row and notes B1's falsifiability gate (`alphaE:71`) names it.

**Same-wave consumer:** present — unified emitter's only consumer is `xtask regen` →
`json/generated.rs`, same commit, `regen --check` + `generated_real_typed.rs` bench. Correct,
no orphan.

**Scalar-ref/checkasm:** N/A (codegen layer; the JSON *scanner* is B4/G5, correctly
deferred). Accurate.

**Disposition: ACCEPT.** No re-block (G3 single-emitter is the SK-V17 REDRESS-W2-1 SUBJECT,
admitted to discharge, not a re-open — αC/HANDOFF confirm). LOC is the softest in the
shortlist; CH4 binds the ±5% generated-line gate as the cost-control.

---

## §3 — CANDIDATE B2 (G2: derive CSS from lowering) — **ACCEPT**

**LOC:** net **≈ −1500**. Disk: `CSS_GENERATED_RS` = **910 LOC** (`:701`→`:1611`, EXACT) +
`CSS_MOD_RS:598`/`CSS_PARSER_RS:612`/`CSS_SINK_RS:665` (~590 LOC combined) ⇒ ~1500 const LOC
retired, replaced by the shared B1 renderer parameterized by the CSS program. **−1500 is
accurate.** The `[generated-size-budget]` guard (halt + trace if derived CSS `generated.rs`
exceeds hand-written by >20%) is present (`:94`). Good cost discipline.

**Risk:** LOW is correct and well-supported — αA §3.4 / V3 A2: the CSS hot path is *already
scalar*, cache-resident; **there is no fragile hand-tuned kernel to preserve**, so the `>SOTA`
does not ride hand-shaping. This is the central reason B2 is the lowest-risk GENERALIZE
candidate, and disk confirms (no CSS kernel in the hot path; `find_css_significant`/
`find_comment_close` are `#[cfg(test)]`-only, `lib.rs:574,598,608` inside the parity-guard
module `:500-501`).

**Same-wave consumer:** present — `xtask regen` → `css_l4_*/generated.rs` consumed by the
honest `css_canon_bench.rs` (PRESENT) + `assert_rich_strict_equality:451` (PRESENT) 9-field
oracle on the real 71KB–495KB corpus. Both keepers verified on disk. Correct.

**Scalar-ref/checkasm:** N/A at codegen (CSS NEON kernels are B4/G6). Accurate.

**`>SOTA` preservation:** gate #1 names bootstrap 2.210× / animate 2.355× / tailwind 3.348× /
material 1.996× within the SK-V17 envelope, honestly framed (H1 lazy-vs-eager). Matches αA §2.
Threshold preserved. The honest-finding escape (`:95`, HANDOFF §6) is the correct fallback:
if the derived recognizer can't hold `>SOTA` without hand-shaping → named grammar-parameterized
primitive, not a silent blob.

**Disposition: ACCEPT.** Lowest-risk GENERALIZE; LOC accurate; consumers + keepers verified.

---

## §4 — CANDIDATE B3 (G4: shared value trait + kill phantom `<G>`) — **ACCEPT**

**LOC:** net **≈ ±0** (a trait + 2–3 impls replaces hand-copied surface). LOC-neutral is the
right call for a trait-extraction; no budget concern. Phantom `ValueRef<…,G:EventGrammar=
AnyGrammar>` verified at `tape/mod.rs:175` (G never bound to a real type) — instantiate-or-
delete is structurally falsifiable (αE gate `:110`: `grep ValueRef<…,(Json|Css|Sheets)
EventGrammar>` ≥1 OR `grep 'G: EventGrammar' tape/mod.rs`=0). Good.

**Risk:** MEDIUM is correct — the trait must be **zero-cost** (no vtable in the hot path) AND
must not flatten the rich JSON AST (`[preserve-rich-ast]` non-negotiable, αA §3.3: JSON tree+
visitor vs CSS flat stream are genuinely divergent surfaces). The cost-risk is a *hidden
dispatch cost*, correctly gated by αE `:113` (JSON `parse_full_traversal`/`path_lookup` + CSS
rich-summary within −3%). This is a real cost surface, properly fenced.

**Same-wave consumer:** present — both JSON `value_from_ref`/`DocumentView` AND CSS
`CssNode::value()` must `impl` the SAME generated trait in the same commit (no orphan trait,
`:108`). The instantiate-or-delete decision is the right `[abrogate-before-patch]` discipline.

**Scalar-ref/checkasm:** N/A (value-API layer). Accurate.

**Disposition: ACCEPT.** Cost is LOC-neutral; the genuine cost-risk (vtable dispatch) is
gated by the −3% zero-cost-trait threshold.

---

## §5 — CANDIDATE B4 (PROVE Sheets + G5 + G6) — **REVISE**

This is the only cost-bearing SIMD candidate, and CH4's lens (scalar-ref/checkasm) bites
here. The candidate is **structurally sound and ACCEPT-able in substance**, but carries a
**material checkasm-inventory cost error** + LOC-budget softness that must be corrected before
the goalset binds an S-P3 cost gate on a false count.

**LOC:** αE budgets **≈ +250** (PROVE +~200 but Sheets generated falls out of B1 ⇒ near-zero
hand LOC; G5 −~100; G6 +~150 per real NEON body with checkasm). The **"+150 per real NEON
body"** is the load-bearing number, and it is **unbounded** as written: G6 enumerates 5
scalar-passthrough kernels + UDOT + PMULL + TBX + CSSC. If even 3 get real bodies, that is
+450, not +150. **The budget must state how many NEON bodies SK-V18 commits to** (αE's own
gate `:135` says "a kernel with no admission-path consumer is RETIRED, not shipped" — so the
*count is conditional on same-wave consumers existing*, which is correct discipline but leaves
the LOC open). REVISE: bound the G6 NEON-body count explicitly (e.g. "PMULL `bitmap_prefix_
xor_64` first, V3 §6; others retired/relabelled unless a same-wave hot-path consumer exists"),
so the +250 net is a real ceiling, not a per-body multiplier with an unstated multiplicand.

**Risk:** MEDIUM-HIGH is correct and well-justified — this is the generalization litmus
(3 distinct `generated.rs`) AND the only real `>SOTA`-regression surface (G5 migrates JSON's
bespoke `neon::scan`, `json/scan.rs:201`, the speed holdout). G6 PMULL/UDOT are real asm ⇒
full checkasm discipline. Correctly the highest-risk candidate.

**Same-wave consumer:** **the strongest part of the candidate** — αE `:132-135` binds each
item to its hot-path consumer in the same commit (PROVE→Sheets bench + G4 trait; G5→JSON
`parse_only` bench; G6 each kernel WITH its caller, "a kernel with no admission-path consumer
is RETIRED"). This directly answers the V5 orphan-kernel pattern (`[no-orphan-kernel]`). The
UDOT orphan is verified on disk (0 runtime callers) — so it is correctly a "wire-or-retire"
target, not a ship-as-is. ACCEPT this axis.

**Scalar-ref status:** STRONG and accurate — every aarch64 kernel has a scalar reference as
the executable spec; the 5 passthrough kernels have scalar bodies under a `_neon` suffix
(verified: `dispatch.rs:67-73` NEON entries point at `aarch64/<kernel>::<kernel>_neon`, scalar
twins `:79-85`); SK-V18 gives real NEON bodies (checkasm oracle) OR honestly drops the `_neon`
suffix. Correct `[_neon-suffix-truth]` discipline.

**Checkasm status — THE DEFECT (REVISE-blocking for this candidate's text):** αE `:47,131,165`
(and αA, and the SUMMARY) assert **"18 differential checkasm harnesses + `checkasm_common.rs`"**
KEPT and EXTENDED. **On disk there are 14 `checkasm_*.rs` files total** (`ls checkasm_*.rs |
wc -l`=14), of which `checkasm_common.rs` (the trampoline/canary harness) and
`checkasm_parity.rs` (aggregate) are not single-kernel differentials ⇒ **~12 single-kernel
differential harnesses, not 18.** This is a **cost-inventory error**: B4 claims to "KEEP and
EXTEND 18" and prices G6 against that surface; the real surface is smaller. Consequences CH4
must surface:
  - the "KEPT" baseline is overstated by ~50% (12 → claimed 18);
  - any S-P3 gate that asserts "18 checkasm harnesses present" will be **un-satisfiable** and
    will red-flag a clean tree (a false gate, the exact P4-class anti-pattern this cycle is
    fixing);
  - the per-new-kernel "+150 with checkasm" budget rides on a miscounted baseline.
**REVISE:** correct the count to the disk-verified **~12 single-kernel differentials + 2
harness/aggregate files** (`checkasm_common.rs`, `checkasm_parity.rs`); re-state "KEPT and
EXTENDED (current N=12, each new G6 body adds 1)". Propagate the correction to αA §3.4 and the
SYNTHESIS/HANDOFF "18" if echoed (grep: αA, αE; SYNTHESIS does not number it — good).

**Path-precision minor (fold-in):** αE `:129` cites "the 5 scalar-passthrough `_neon` kernels"
at `dispatch.rs:66-74` and "`dispatch.rs:68-73`". The *registration* is there (`:67-73` NEON,
`:79-85` scalar), but the *kernel bodies* (the thing G6 rewrites) live in
`aarch64/bitmap_prefix_xor_64.rs:2`, `aarch64/eob_pad_clamp.rs:4`, etc. The owner-path list
should cite the per-kernel files (where the body is rewritten) AND `dispatch.rs:67-85` (where
the relabel/retire lands), not `dispatch.rs:66-74` alone. Non-blocking but the owner-path is
where S-P3 will edit.

**`>SOTA` preservation:** G5 gate (`:140`) names JSON `parse_only` within −3% on twitter/
canada/citm/github, with the correct fallback (V3 F5: if the neutral kernel is slower, expose
the JSON string-mask path AS a parametric kernel rather than regress). Sound.

**Disposition: REVISE** — substance ACCEPT-able; the **checkasm "18" overcount** (must be ~12)
and the **unbounded G6 "+150 per body" LOC** (must state a committed body-count ceiling) are
the two cost defects. Both are corrections to the candidate's cost accounting, not to its
architecture. Fix: `alphaE-candidate-shortlist.md:47,131,141,165` (checkasm count + LOC
ceiling); echo-fix `alphaA-results-extraction.md §3.4`.

---

## §6 — Cross-cutting cost / wave-alignment review

1. **Net LOC ≈ −9150 (αE SUMMARY):** recomputed from disk-verified parts: A −6900…−7100,
   B1 −800, B2 −1500, B3 ±0, B4 +250 (capped) ⇒ **−8950…−9150**. Accurate. **A generalization
   cycle that deletes far more than it adds** — the correct cost signature for an inflection
   backtrack. No `[generated-size-budget]` overflow on any candidate.

2. **Sequencing / entry-gates (cost of wave order):** A → B1 → B2 → B3 → B4, each B
   entry-gated on its predecessor; P4 (Lock-14 gate meaningful) lands BEFORE B1 so the
   un-forked emitter is scanned for neutrality as it is built (`alphaE:163`). This is the right
   cost-of-coupling ordering — it prevents B1 re-leaking under a blind gate. The HANDOFF §Next
   Move sequences identically. ACCEPT.

3. **Same-wave consumer — present on EVERY candidate** (the V5 orphan-kernel guard). A: gate is
   its own consumer; B1: `regen`→`json/generated.rs`; B2: `css_canon_bench`+oracle; B3: both
   trait impls same commit; B4: each kernel WITH hot-path caller, orphan ⇒ retire. **This is
   the cost-discipline this cycle most needed and it is uniformly applied.** ACCEPT.

4. **Revert protocol / hard caps:** correctly **sanctioned-deferred to S-P3** per PASS-ALPHA
   §4.4 (SYNTHESIS `:445`, HANDOFF `:267`). CH4 notes this is the contract-mandated boundary
   (Pass Alpha supplies goalset + telemetry; S-P3 authors per-wave revert/cap). Not a CH4
   defect — but CH6 (Next-Tranche-Impact) owns confirming S-P3 receives the cap binding.

5. **Telemetry cost-gate (Section 2):** the `generated_md5_distinct`, `generator_grammar_count
   == 3`, `acceleration_at_admission ∈ {admission, scalar-passthrough-labeled, retired}`
   columns make the cost-bearing generalization claims machine-checkable per row. The
   `acceleration_at_admission` enum correctly admits `scalar-passthrough-labeled`/`retired` —
   so an honestly-relabelled (not-accelerated) kernel is NOT a NO-GO, only a `cfg-test-only`
   *acceleration claim* is. This is the right cost posture for G6 (it does not force fabricating
   NEON bodies to pass a gate). ACCEPT.

6. **No re-blocked route re-opened (cost of regression):** verified against the V3 pre-block
   list (AZ-IV eager, StructRegistry per-leaf, fact-stream-output, 24-broadcast, FNV-runtime,
   x86/AVX/SVE). The shortlist is additive-by-deletion. No candidate re-introduces a cost-bearing
   refuted carrier. ACCEPT (CH3 owns the full regression sweep; CH4 confirms no cost re-entry).

---

## §7 — Disposition summary

| section | candidate / axis | disposition | cost defect (if any) |
|---|---|---|---|
| §1 | CANDIDATE A (PRUNE P1–P5) | **ACCEPT** | P3 "35→5 files" phrasing folds into A text (non-blocking) |
| §2 | CANDIDATE B1 (G3+G1) | **ACCEPT** | −800 softest budget; ±5% generated-line gate binds it |
| §3 | CANDIDATE B2 (G2) | **ACCEPT** | −1500 EXACT; LOW risk well-supported |
| §4 | CANDIDATE B3 (G4) | **ACCEPT** | ±0 LOC; vtable-cost gated by −3% |
| §5 | CANDIDATE B4 (PROVE+G5+G6) | **REVISE** | checkasm "18"→~12 overcount; G6 "+150/body" unbounded LOC |

**ACCEPT 4 · REVISE 1 · REJECT 0** over the 5 candidate clusters.

**The one REVISE (B4) is orphan-free and concrete:** correct the checkasm differential count
from 18 to the disk-verified ~12 single-kernel harnesses (+2 harness/aggregate files), at
`alphaE-candidate-shortlist.md:47,131,165` + echo-fix `alphaA-results-extraction.md §3.4`; and
bound the G6 NEON-body LOC by a committed body-count ceiling (PMULL-first, others retired/
relabelled unless a same-wave consumer exists) at `alphaE:131,141`. Both are cost-accounting
corrections; B4's architecture, risk class, and same-wave-consumer discipline are sound and the
candidate ACCEPTs once the count and budget-ceiling are honest. No candidate is REJECTed; the
shortlist's cost signature (net ≈ −9150, every candidate same-wave-consumed, sequenced
PRUNE→GENERALIZE→PROVE) is correct for a generalization backtrack and preserves the `>SOTA`
thresholds (JSON 51/51 ≥ sonic-strict; CSS 1.996×–3.348× lightningcss, H1-framed) from the
grammar-DERIVED parsers.

TALLY accept=4 revise=1 reject=0
