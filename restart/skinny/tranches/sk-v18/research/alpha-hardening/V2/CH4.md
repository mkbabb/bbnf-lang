# CH4 — COST lens (V2) — Pass Alpha SK-V18 alpha-hardening

**Lens:** CH4 Cost (PASS-ALPHA §3 / ORCHESTRATOR §3W).
**Subject:** SK-V18 = the GENERALIZATION cycle (inflection backtrack). ONE grammar-driven
generator emitting JSON+CSS+Sheets from `.bbnf`, over the unified tape/`ValueRef` substrate,
shared value-API, PROVEN on Sheets, PRESERVING >SOTA. NOT a feature cycle.
**Artefacts reviewed:** `research/alpha/{alphaA..F}.md` + `SYNTHESIS.md` + `HANDOFF.md`
(alphaF = SYNTHESIS+HANDOFF). CH4 reviews cost-bearing claims across all; alphaB/alphaD are
competitor/ledger axes — only their cost-bearing rows are in scope.
**Focus per dispatch:** each candidate's **LOC budget + risk classification + same-wave
consumer + scalar-ref/checkasm (SIMD) status**, and that grammar-DERIVATION preserves the
`>SOTA` threshold honestly.
**Method:** every cost/LOC/checkasm/kernel/owner-path claim re-verified live on disk at the
benched skinny tree (`skinny/crates/`). Citations are `path:line` from disk. V2 re-checks
that the V1 CH4 REVISEs (F4 checkasm count, F5 G6 ceiling) landed AND propagated to the
binding contract (SYNTHESIS/HANDOFF), not merely into αE.

---

## §0 — V1→V2 fold status (the prior CH4 dispositions)

V1 CH4 disposition was **ACCEPT 4 · REVISE 1 · REJECT 0**, the one REVISE on CANDIDATE B4
for two cost-accounting defects:
- **checkasm "18" overcount** → must be ~12 single-kernel differentials (+2 harness/aggregate);
- **G6 "+150 per body" unbounded** → must state a committed body-count ceiling.

**Both V1 REVISEs landed in V2 αE** (verified):
- **F4** (`alphaE:19,164`): checkasm corrected to "**12 single-kernel differential harnesses +
  `checkasm_common.rs` + `checkasm_parity.rs` = 14 `checkasm_*.rs` total**", with "current
  N=12 → N=12+k where k = committed bodies."
- **F5** (`alphaE:20,174`): G6 bounded — "**PMULL `bitmap_prefix_xor_64` is the ONE committed
  real NEON body (+~150 with its checkasm differential); every OTHER kernel … RETIRED or
  honestly relabelled UNLESS a same-wave hot-path consumer exists**" ⇒ committed ceiling
  +~150, net ≈ +250 capped.

Both folds are disk-correct (see §0.1). **The V1 B4 REVISE is therefore discharged in αE.**

### §0.1 — Disk-verified cost ground truth (V2 re-check)

| claim (artefact) | disk verification (this pass) | verdict |
|---|---|---|
| P1 x86 tree = 24 files (23 `.rs` + 1 `.asm`) / 742 LOC / 14 `unimplemented!` | `find …/x86_64 -type f`=**24**; `.rs`-only `wc -l`=**742**; `.asm`=`byte_class_from_eq_set_64.asm` (**105 LOC**); `unimplemented!`=**14** | files/`.rs`-LOC/unimpl EXACT; **`.asm` adds 105 (see §1 rider)** |
| P3 7 CSS `generated.rs` replicas, byte-identical at HEAD | `find css_l4_*/generated.rs`=**7**; total `wc -l`=**6370** (≈910 ea); 35 files in 7 dirs; md5 of `at_rules_and_media` ≡ `visual_functions` (`b654562c…`) | EXACT |
| B2 `CSS_GENERATED_RS` span | `runtime_generator.rs:701`→const closes **:1611** (file EOF) = **~910 LOC**; `CSS_MOD_RS:598`→`:610`, `CSS_PARSER_RS:612`→`:663`, `CSS_SINK_RS:665`→`:681` (~590 combined) | EXACT |
| B1 `json_templates/` | **6 files / 1149 LOC**; `json_sink_direct.rs`=**561 LOC**, `render(program:&SinkOnlyProgram):4` | EXACT |
| **B4 checkasm count** | `ls …/tests/checkasm_*.rs`=**14**; `checkasm_common.rs` + `checkasm_parity.rs` are harness/aggregate ⇒ **12 single-kernel differentials** | **F4 fold EXACT** |
| B4 G6 scalar-passthrough kernels + dispatch | NEON registration `dispatch.rs:68-73` (`bitmap_prefix_xor_64_neon`…); scalar twins `:80-85` (`…_scalar`); bodies in `aarch64/<kernel>.rs` | reg-lines correct; **path prefix wrong (see §5)** |
| B4 UDOT `parse_4_digits_dotprod` orphan | `grep -rn …dotprod crates/runtime/src`=**0** runtime callers; defined `aarch64/digit_mac.rs:27` | orphan VERIFIED |
| B4 CSS NEON test-only | `find_css_significant`/`find_comment_close` defined `runtime_simd.rs:169,112`; runtime callers `lib.rs:574,598,608` are inside `#[cfg(test)] mod tests` (opens `:51`) | test-only VERIFIED |
| P5 `parse_w11_1_number` ×7 | `grep -c` in `json/generated.rs`=**7** | EXACT |
| P4 gate lines | `GENERIC_SCAN_ROOTS:2409` / `FORBIDDEN_GENERIC_TOKENS:2420` / `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442` / `diagnostic-x86:2463` | EXACT |
| G4 phantom `ValueRef<…,G: EventGrammar = AnyGrammar>` | `tape/mod.rs:175`; `G` defaults to `AnyGrammar`, no production bind | VERIFIED |
| PROVE Sheets stub | `sheets_witness/`=**25 LOC** (2 files) | EXACT |

The V2 αE cost ground truth is accurate. **One stale-count defect in the binding contract
(SYNTHESIS:348 "18 differential harnesses") + one owner-path prefix imprecision (αE B4
`aarch64/dispatch.rs`) + one minor LOC-label undercount (P1 `.asm` 105).**

---

## §1 — CANDIDATE A (PRUNE P1–P5) — **ACCEPT**

**LOC:** αE budgets net **≈ −7100** (`alphaE:71`). Disk recompute: P1 −742 (`.rs`) + P3 ≈
−5460 (6×910 redundant `generated.rs`) + P2 ~−700 (of 3737-LOC `nonjson_css_l4.rs`) + P4
+~15 + P5 rename-only ⇒ **≈ −6900…−7100**. αE itself says "if anything understated" — correct,
and it is **further understated by the P1 `.asm` 105 LOC** (F8 names the `.asm` for deletion,
but the LOC label on the P1 row stays "−742"; the true P1 deletion is **−847**). This pushes
the floor toward −7000…−7200. Direction is favourable; no `[generated-size-budget]` overflow
(pure reduction). Non-blocking.

**Risk:** LOW correct. Pure deletion + one gate-scope patch (P4). No `>SOTA`-bearing code
touched: x86 = 0 real intrinsics (14 `unimplemented!`); the headline numbers ride
`css_canon_bench` (KEPT), not the deleted contrived bench (V3 C3). The only judgement item
(P3 collapse-vs-differentiate) is correctly deferred to B2.

**Same-wave consumer:** present per sub-item (P4 → `accepts_current_allowlist` now meaningful;
P3 → runtime `lib.rs` `pub mod generated_*` roster + `regen.rs`; P5 → `regen --check`). Correct.

**Scalar-ref/checkasm:** N/A correct (P1 deletes the x86 tree — no checkasm there; the 12
aarch64 differentials are untouched). αE's checkasm-untouched note now cites the **corrected 12**
(F4), not the stale 18 — consistent.

**REVISE-rider (NON-blocking, fold into A's text):** the P1 row LOC label "−742" should read
"**−847** (742 `.rs` + 105 `.asm`)" since F8 deletes the `.asm`. αE:57 already names the
`.asm`; the LOC column merely omits its 105. Cost conclusion unchanged (net is a floor).

**Disposition: ACCEPT.** Mandatory entry-gate; LOC defensible (if anything understated);
risk LOW; same-wave consumers present.

---

## §2 — CANDIDATE B1 (G3+G1: un-fork + project JSON) — **ACCEPT**

**LOC:** net **≈ −800** (delete `JSON_*_RS` consts + `json_templates/` 1149 LOC; the projecting
`render` is smaller than the verbatim blobs). Disk supports the direction: `json_sink_direct.rs`
(561 LOC) already takes `&SinkOnlyProgram:4` but `render_header`/dispatch bodies `push_str`
constant text — making them project nets toward deletion of the 1149-LOC template surface.
**−800 is plausible but the softest budget in the shortlist** (the projecting renderer's true
LOC is unknown until written). αE already binds the cost-control: same-wave regen must show
`json/generated.rs` within **±5%** of today (`alphaE:91`). CH4 re-affirms that ±5% line as the
S-P3 binding gate.

**Risk:** MEDIUM correct — JSON is the `>SOTA` holdout with a real hot kernel; the projection
must reproduce the hand-written hot loop exactly. Mitigation present (`json_templates/` held as
byte-for-byte oracle, deleted only after `diff`-match, `[clean-regen-discipline]`). The
thinnest-margin tripwire is correctly named: **apache_builds/parse_only at +1.4% over
sonic-strict** (αA:70, αE:88) — a derived parser dropping 1.4% loses `>SOTA` on that row.

**Same-wave consumer:** present — `xtask regen` → `json/generated.rs`, same commit,
`regen --check` + `generated_real_typed.rs` bench. No orphan.

**Scalar-ref/checkasm:** N/A (codegen layer; the JSON scanner is B4/G5, correctly deferred).

**Disposition: ACCEPT.** No re-block (G3 single-emitter = SK-V17 REDRESS-W2-1 SUBJECT admitted
to discharge, not a re-open — αC/HANDOFF:207 confirm). ±5% generated-line gate binds the soft LOC.

---

## §3 — CANDIDATE B2 (G2: derive CSS from lowering) — **ACCEPT**

**LOC:** net **≈ −1500**. Disk: `CSS_GENERATED_RS` = 910 LOC (`:701`→`:1611`) + `CSS_MOD_RS`/
`CSS_PARSER_RS`/`CSS_SINK_RS` (~590 combined) ⇒ ~1500 const LOC retired, replaced by the shared
B1 renderer parameterized by the CSS program. **−1500 EXACT.** The `[generated-size-budget]`
guard (halt + trace if derived CSS `generated.rs` exceeds hand-written by >20%) is present
(`alphaE:120`). Good cost discipline.

**Risk:** LOW correct and well-supported — V3 A2: the CSS hot path is *already scalar*,
cache-resident; **there is no fragile hand-tuned kernel to preserve**, so `>SOTA` does not ride
hand-shaping (disk confirms `find_css_significant`/`find_comment_close` are `#[cfg(test)]`-only).

**Same-wave consumer:** present — `xtask regen` → `css_l4_*/generated.rs` consumed by the honest
`css_canon_bench.rs` (PRESENT) + `assert_rich_strict_equality:451` (PRESENT) 9-field oracle on
the real 71KB–495KB corpus. Both keepers verified.

**Scalar-ref/checkasm:** N/A at codegen (CSS NEON is B4/G6).

**`>SOTA` preservation:** gate#1 pins the **N=200 close-ledger per-row floors** (bootstrap
≥2398.9 / animate ≥2850.0 / tailwind ≥2690.2 / material ≥2540.0 Mbps Track1, `alphaE:110-116`),
H1-framed (lazy-rich-summary vs eager-full-CSSOM), with the N=80 live reproduction as cross-check
only (F1 plane discipline). Threshold preserved honestly. The honest-finding escape (`alphaE:121`)
is the correct fallback: a hand-shaped recognizer becomes a named, `.bbnf`-invoked,
checkasm-referenced primitive — not a silent blob.

**Disposition: ACCEPT.** Lowest-risk GENERALIZE; LOC EXACT; consumers + keepers verified.

---

## §4 — CANDIDATE B3 (G4: shared value trait + kill phantom `<G>`) — **ACCEPT**

**LOC:** net **≈ ±0** (a trait + 2–3 impls replaces hand-copied surface). LOC-neutral is right
for a trait-extraction. The F6 caveat is correct and cost-load-bearing: the **DELETE branch is
DEFAULT** (`abrogate-before-patch`) — keeps ±0; the **INSTANTIATE branch is burden-of-proof**
because `CssEventGrammar` does NOT exist at HEAD (disk: only `JsonEventGrammar` +
`SheetsEventGrammar`, both test-only witnesses) — authoring it is a new grammar-named coupling
surface, un-budgeted in ±0. The contract default avoids the un-budgeted LOC. Good.

**Risk:** MEDIUM correct — the trait must be **zero-cost** (no vtable in the hot path) AND must
not flatten the rich JSON AST (`[preserve-rich-ast]` non-negotiable). The cost-risk is hidden
dispatch cost, gated by `alphaE:144` (JSON `parse_full_traversal`/`path_lookup` + CSS
rich-summary within −3%). F7 adds the preserve-rich-ast structural gate (both-impl grep is
necessary-not-sufficient; JSON `get(key)`/typed `Kind`/visitor must remain reachable THROUGH the
trait). Properly fenced.

**Same-wave consumer:** present — both JSON `value_from_ref`/`DocumentView` AND CSS
`CssNode::value()` must `impl` the SAME generated trait in the same commit (no orphan trait,
`alphaE:136`). Phantom `ValueRef<…,G:EventGrammar=AnyGrammar>` verified `tape/mod.rs:175`; the
test-excluded grep (F6) is correct — the standing test-only `JsonEventGrammar` line must NOT
false-green the INSTANTIATE gate.

**Scalar-ref/checkasm:** N/A (value-API layer).

**Disposition: ACCEPT.** Cost LOC-neutral on the DELETE default; the vtable-dispatch cost-risk
is gated by the −3% zero-cost-trait threshold; the un-budgeted INSTANTIATE LOC is correctly
gated behind burden-of-proof.

---

## §5 — CANDIDATE B4 (PROVE Sheets + G5 + G6) — **ACCEPT** *(V1 REVISE discharged by F4/F5)*

V1 CH4 dispositioned B4 REVISE on two cost defects (checkasm "18", unbounded G6 LOC). **Both
are folded into V2 αE and verified disk-correct:**

- **Checkasm (F4):** αE:164 now states "**12 single-kernel differential harnesses +
  `checkasm_common.rs` + `checkasm_parity.rs` = 14 total**; current N=12 → N=12+k." Disk:
  `ls checkasm_*.rs`=**14**, 2 of which are harness/aggregate ⇒ **12 single-kernel**. EXACT.
  The false-gate hazard ("18 present" un-satisfiable on a clean tree) is removed from αE.
- **G6 LOC ceiling (F5):** αE:174 now states "**PMULL `bitmap_prefix_xor_64` is the ONE
  committed real NEON body (+~150 with its checkasm); every OTHER kernel RETIRED or honestly
  relabelled UNLESS a same-wave hot-path consumer exists**" ⇒ committed ceiling +~150, net
  ≈ +250 (capped). The "+150 per body" is no longer an unstated multiplicand.

**LOC:** net **≈ +250 (capped)** — PROVE +~200 (Sheets `.bbnf` referenced not authored; generated
runtime falls out of B1; skinny grammar-root + xtask target +~30); G5 −~100 (bespoke scanner
retired onto shared kernel); G6 +~150 (one PMULL body + its 1 checkasm differential). The cap is
now real, not open-ended. ACCEPT.

**Risk:** MEDIUM-HIGH correct and well-justified — the generalization litmus (3 distinct
`generated.rs`) AND the only real `>SOTA`-regression surface (G5 migrates JSON's bespoke
`json/scan.rs:201` scanner, the speed holdout). G6 PMULL/UDOT are real asm ⇒ full checkasm
discipline (N=12 + each new body adds 1). Correctly the highest-risk candidate.

**Same-wave consumer:** **the strongest axis** — αE:165-168 binds each item to its hot-path
consumer in the same commit (PROVE→Sheets bench + G4 trait; G5→JSON `parse_only` bench; G6 each
kernel WITH its caller; "a kernel with no admission-path consumer is RETIRED, not shipped").
Directly answers the V5 orphan-kernel pattern. The UDOT orphan is verified on disk (0 runtime
callers) ⇒ correctly a wire-or-retire target. SYNTHESIS:221 sharpens further: the **retire branch
is gated on a samply non-top-N MEASUREMENT, not an assertion** — it cannot close G6 by marking
all NEON "retired" with zero acceleration wired. Excellent cost posture.

**Scalar-ref status:** STRONG and the spec — every aarch64 kernel has a scalar reference as the
executable spec; the 5 passthrough kernels have scalar twins (`dispatch.rs:80-85`,
`bitmap_prefix_xor_64_scalar`…). SK-V18 gives real NEON bodies (checkasm oracle) OR honestly
drops the `_neon` suffix. Correct `[_neon-suffix-truth]` discipline.

**Owner-path precision (NON-blocking fold-in):** αE:162 cites the registration/relabel/retire
site as `bbnf-simd/src/aarch64/dispatch.rs:67-85`. **The dispatch file is at
`bbnf-simd/src/dispatch.rs`, NOT `aarch64/dispatch.rs`** (the latter does not exist; disk:
`find … -name dispatch.rs` = `src/dispatch.rs` only). The line numbers (NEON :68-73, scalar
:80-85) are correct; only the `aarch64/` directory prefix is wrong. The *kernel-body* paths
(`aarch64/bitmap_prefix_xor_64.rs:2`, `aarch64/eob_pad_clamp.rs:4`, etc.) ARE correct. SYNTHESIS
cites the dispatch correctly (`SYNTHESIS:174` "`bbnf-simd/src/dispatch.rs select_classifier`"),
so this prefix slip is confined to αE:162 and must be corrected to `bbnf-simd/src/dispatch.rs`
since S-P3 will edit exactly that file. Non-blocking for the disposition (count + LOC are correct
post-fold); the prefix is a one-token owner-path fix.

**`>SOTA` preservation:** G5 gate (`alphaE:173`) names JSON `parse_only` within −3% on
twitter/canada/citm/github, with the correct fallback (V3 F5: expose the JSON string-mask path
AS a parametric kernel rather than regress). CSS rows hold the SAME N=200 per-row floors as B2
gate#1 (F1). Sound.

**Disposition: ACCEPT.** The two V1 cost defects (checkasm count, G6 ceiling) are folded
disk-correct in αE. The remaining αE:162 `aarch64/dispatch.rs` prefix is a non-blocking
owner-path fold-in (corrected to `bbnf-simd/src/dispatch.rs`). Architecture, risk class,
same-wave-consumer discipline are sound.

---

## §6 — αF contract (SYNTHESIS + HANDOFF) cost-inventory review — **REVISE (one stale count)**

CH4 reviews the cost-bearing claims in the binding contract artefacts (SYNTHESIS = αF). One
defect: a fold corrected in αA/αE did NOT propagate into SYNTHESIS.

**DEFECT — SYNTHESIS.md:348 carries the stale "18 differential harnesses".** The "Validated
(carry forward)" ledger reads "the grammar-neutral checkasm-disciplined NEON kernel set (**18
differential harnesses**, scalar-ref-as-spec)". This is the EXACT false count the V1 CH4 REVISE
corrected and that **αA:195 explicitly warns is un-satisfiable** ("Any S-P3 gate that asserts
'18 checkasm harnesses' … `ls …/checkasm_*.rs | wc -l` = 14"). Disk: **12 single-kernel
differentials + 2 harness/aggregate = 14 total**. SYNTHESIS is the binding contract that S-P3
consumes for the §4.4 wave plan; a downstream gate echoing "18 differential harnesses" from
SYNTHESIS would be un-satisfiable on a clean tree — the precise P4-class false gate this cycle
fixes. The αF draft echoed the pre-fold number while its own upstream αA already corrected it.

**REVISE (concrete fix):** `restart/skinny/tranches/sk-v18/SYNTHESIS.md:348` — change
"18 differential harnesses" → "**12 single-kernel differential harnesses + 2 harness/aggregate
(`checkasm_common.rs`, `checkasm_parity.rs`) = 14 `checkasm_*.rs` total**", matching αA:22-25,195
and αE:164 (F4). This is a one-line cost-inventory correction in the contract; no architecture
change. (HANDOFF carries no checkasm count — clean. SYNTHESIS:348 is the sole un-propagated F4
echo.)

**Other αF cost claims verified clean:**
- SYNTHESIS:165 P1 verifies by `find … = 0` (file-count), not a stale −742 LOC gate — no false
  gate (the `.asm` 105-LOC label gap is confined to αE:57's column, §1 above).
- SYNTHESIS:174 G5 cites `bbnf-simd/src/dispatch.rs select_classifier` — correct path (αE:162
  is the only place with the wrong `aarch64/` prefix).
- SYNTHESIS:400 `acceleration_at_admission` enum (`admission / cfg-test-only /
  scalar-passthrough-labeled / retired`) makes the G6 cost claim machine-checkable per row;
  `cfg-test-only` is NO-GO for an acceleration claim, `retired`/`scalar-passthrough-labeled` are
  honest non-claims. Right cost posture (does not force fabricating NEON bodies to pass a gate).
- SYNTHESIS:221 retire branch gated on a **samply non-top-N measurement** (not assertion) —
  closes the "mark everything retired with zero acceleration" loophole. Correct.

---

## §7 — Cross-cutting cost / wave-alignment review — **ACCEPT**

1. **Net LOC ≈ −9150 (αE SUMMARY):** recomputed A −6900…−7100 (−7000…−7200 incl. `.asm`) + B1
   −800 + B2 −1500 + B3 ±0 + B4 +250 (capped) ⇒ **≈ −8950…−9250**. A generalization cycle that
   deletes far more than it adds — the correct cost signature for an inflection backtrack. No
   `[generated-size-budget]` overflow on any candidate.

2. **Sequencing / entry-gates:** A → B1 → B2 → B3 → B4, each B entry-gated on its predecessor;
   P4 (Lock-14 gate meaningful) lands BEFORE B1 so the un-forked emitter is scanned for
   neutrality as it is built (`alphaE:196`). Right cost-of-coupling ordering — prevents B1
   re-leaking under a blind gate. HANDOFF:257 sequences identically. The exit-gate-blocks-
   successor clause is carried (CH6 §5 ref, `alphaE:196`; SYNTHESIS:477). ACCEPT.

3. **Same-wave consumer — present on EVERY candidate** (the V5 orphan-kernel guard). A: gate is
   its own consumer; B1: `regen`→`json/generated.rs`; B2: `css_canon_bench`+oracle; B3: both
   trait impls same commit; B4: each kernel WITH hot-path caller, orphan ⇒ retire-on-measurement.
   Uniformly applied. ACCEPT.

4. **Revert protocol / hard caps:** correctly sanctioned-deferred to S-P3 per PASS-ALPHA §4.4
   (SYNTHESIS:151, HANDOFF:297). The Pass-Alpha/S-P3 boundary is contract-mandated — not a CH4
   defect. CH6 owns confirming S-P3 receives the cap binding.

5. **Telemetry cost-gate columns** (`generated_md5_distinct`, `generator_grammar_count==3`,
   `acceleration_at_admission`, `verbatim_blob_present==false`, `emitter_fork_present==false`,
   `phantom_generic_resolved`, `corpus_in_timer==true`, `materialization_framing`) make the
   cost-bearing generalization claims machine-checkable per row (`alphaE:199`, SYNTHESIS:398-400).
   ACCEPT.

6. **No re-blocked route re-opened (cost of regression):** verified against the V3 pre-block
   list (AZ-IV eager, StructRegistry per-leaf, fact-stream-output, 24-broadcast, FNV-runtime,
   x86/AVX/SVE). The shortlist is additive-by-deletion; no candidate re-introduces a cost-bearing
   refuted carrier. ACCEPT (CH3 owns the full regression sweep; CH4 confirms no cost re-entry).

---

## §8 — Disposition summary

| section | candidate / axis | disposition | cost defect (if any) |
|---|---|---|---|
| §1 | CANDIDATE A (PRUNE P1–P5) | **ACCEPT** | P1 LOC label "−742"→"−847" (incl `.asm` 105); non-blocking fold |
| §2 | CANDIDATE B1 (G3+G1) | **ACCEPT** | −800 softest budget; ±5% generated-line gate binds it |
| §3 | CANDIDATE B2 (G2) | **ACCEPT** | −1500 EXACT; LOW risk well-supported |
| §4 | CANDIDATE B3 (G4) | **ACCEPT** | ±0 LOC; vtable-cost gated by −3%; INSTANTIATE LOC behind burden-of-proof |
| §5 | CANDIDATE B4 (PROVE+G5+G6) | **ACCEPT** | V1 REVISE (checkasm 12 / G6 ceiling) discharged by F4/F5; αE:162 `aarch64/dispatch.rs`→`dispatch.rs` non-blocking fold |
| §6 | αF contract (SYNTHESIS) | **REVISE** | **SYNTHESIS:348 stale "18 differential harnesses" → 12 single-kernel + 2 (=14); P4-class false-gate hazard** |

**The one REVISE (§6) is orphan-free and concrete:** `SYNTHESIS.md:348` change "18 differential
harnesses" → "12 single-kernel differential harnesses + 2 harness/aggregate (`checkasm_common.rs`,
`checkasm_parity.rs`) = 14 total", matching αA:195 + αE:164 (F4). This is the αF contract failing
to propagate a fold its own upstream (αA) already made — a binding-contract cost-inventory error,
not an architecture change.

**Two NON-blocking fold-ins** (do not gate disposition, but should land for owner-path accuracy):
αE:57 P1 LOC column "−742"→"−847 (742 `.rs` + 105 `.asm`)"; αE:162 owner-path
`aarch64/dispatch.rs:67-85`→`bbnf-simd/src/dispatch.rs` (NEON :68-73, scalar :80-85).

**Cost verdict:** the candidate shortlist's cost signature (net ≈ −9150, every candidate
same-wave-consumed, sequenced PRUNE→GENERALIZE→PROVE, G6 LOC-capped, checkasm count
disk-honest in αE) is correct for a generalization backtrack and preserves the `>SOTA`
thresholds (JSON 51/51 ≥ sonic-strict, apache +1.4% tripwire named; CSS N=200 per-row floors
H1-framed) from the grammar-DERIVED parsers. The V1 B4 REVISE is fully discharged. The sole
remaining cost defect is the un-propagated "18" in the binding SYNTHESIS contract.

TALLY accept=5 revise=1 reject=0
