# CH1 — CORRECTNESS (cycle V4) — SK-V18 Pass-Alpha CHALLENGE

Lens: **CH1 Correctness** per `PASS-ALPHA.md §3` ("does every claim cite RESULTS.md
row, REDRESS entry, commit SHA, or measurement file? Are falsifiability gates
measurable? Are competitor deltas computed against the correct strictness plane?") +
ORCHESTRATOR §3W. Subject: the Pass-Alpha SK-V18 artefacts
`research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md` (the α-F deliverable
per PASS-ALPHA §2's output mapping — there is no separate `research/alpha/αF` file;
SYNTHESIS + HANDOFF together constitute α-F, contract-compliant).

Discipline applied: every load-bearing path:line / SHA / count / Mbps figure was
re-verified live at HEAD `318d9c046` (the bracket HEAD, unchanged since V3). Honesty
focus per the V3 mandate: >SOTA framing must disclose the lazy-vs-eager asymmetry (H1);
gates must be measurable against the ACTUAL close condition; competitor deltas must be
on the correct strictness plane (sonic-strict, not lossy).

**V4 entry note (the redress cycle ran).** The artefacts (mtime 14:18–14:22) are NEWER
than the V3 hardening CH-pass (14:07): a redress pass folded the V3 dispositions into the
αA–αE + SYNTHESIS + HANDOFF cohort. This V4 pass re-verifies the folds landed and
independently re-checks every load-bearing claim. The V3 CH1 sole REVISE (αD:85 stale
"18" checkasm) is **resolved** (αD:105 now carries 14; §8.V4 R1 records the fold). One
NEW correctness defect surfaces this cycle: an **orphan REVISE on αE** — the V3 CH5 §C.5
x86-scope-widening disposition, which CH5:148 explicitly directs to "αC §1 **+ the αE P1
row** + SYNTHESIS P1," was folded into αC / SYNTHESIS / HANDOFF but **NOT into αE**.

**Disk re-verification (all PASS at HEAD `318d9c046`):**

| Claim | Command | Result | Status |
|---|---|---|---|
| SK-V17 close SHA | `git log --oneline -1 f6a38445b` | `…W5 close — SK-V17 CLOSED` | ✓ |
| V3 audit SHA | `git log --oneline -1 7dbe44c22` | `audit(skinny-impl-overfit-v3)… SK-V18 = generalization` | ✓ |
| bracket HEAD | `git log --oneline -1` | `318d9c046 docs(sk-v18-handoff)…` | ✓ |
| checkasm files | `ls …/tests/checkasm_*.rs \| wc -l` | **14** (12 single + 2) | ✓ |
| x86 `src/x86_64/` files | `find …/src/x86_64 -type f \| wc -l` | **24** (23 `.rs`/742 + 1 `.asm`/105 = 847) | ✓ |
| x86 `src/x86_64/` `unimplemented!` | grep count | **14** | ✓ |
| **x86 `ext/x86/` (the V3 C.5 surface)** | `find …/ext/x86 -type f \| xargs wc -l` | **3554** (`bbnf.asm`/`x86util.asm`/`x86inc.asm`/`LICENSE-VENDOR`) | ✓ EXISTS |
| **x86 `build.rs` (nasm driver)** | `ls -la …/build.rs` + `:1` header | `3784 B`, "assembles vendored + authored x86_64 .asm sources" | ✓ EXISTS |
| **`Cargo.toml` nasm dep** | `grep nasm-rs …/Cargo.toml` | `:19 nasm-rs = "0.3"` | ✓ EXISTS |
| CSS replica md5 | `md5 …css_l4_*/generated.rs \| sort -u \| wc -l` | **1** | ✓ |
| `parse_w11_1` ×N | `grep -c … json/generated.rs` | **7** | ✓ |
| `CSS_GENERATED_RS` | `sed -n 701p runtime_generator.rs` | `const CSS_GENERATED_RS: &str = r#"` | ✓ |
| `RuntimeEmitterKind` | `sed -n 40p grammar_provider.rs` | `pub enum RuntimeEmitterKind {` | ✓ |
| `ValueRef` two-axis | `sed -n 175p tape/mod.rs` | `…K = AnyKind, G: EventGrammar = AnyGrammar>` | ✓ |
| `render(SinkOnlyProgram)` | `sed -n 4p json_sink_direct.rs` | `pub fn render(program: &SinkOnlyProgram)…` | ✓ |
| sonic strict skipper | `sed -n 3,7p sonic_skipper.rs` | `IgnoredAny::deserialize` + `deserializer.end()` | ✓ |
| `GENERIC_SCAN_ROOTS` | `grep -n … lock14_baseline.rs` | `:2409` (iter `:2467`, `:2508`) | ✓ |
| old CSS bench | `grep -n 'fn measure_mbps\|fn lightningcss_facts' nonjson_css_l4.rs` | `:3091` / `:528` | ✓ |
| W5 ledger N=200 medians | `sed -n 99,102p skv17-W5-close-ledger.md` | bootstrap 2473.1/1119.1/2.210×; animate 2937.9/1247.7/2.355×; tailwind 2773.4/828.5/3.348×; material 2618.5/1312.0/1.996× | ✓ |
| LOCKS canonical alphabet | `sed -n 349p LOCKS.md` (Lock 14) | `JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser` arm-census command | ✓ |
| 16-lock count | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` | **16** | ✓ |
| RESULTS twitter parse_only | `grep '^\| twitter \| parse_only' RESULTS.md` | t1=8349.290 / sonic=4913.095 / serde=857.188 / Δ=+69.9% | ✓ |
| RESULTS apache_builds parse_only | row | t1=13129.331 / sonic=12951.668 / Δ=+1.4% | ✓ |
| RESULTS unicode_escapes parse_only | row | t1=7897.449 / sonic=2984.079 / Δ=+164.7% | ✓ |
| RESULTS canada simdjson DOM | row | 11493 DOM / Δ=+45.4% | ✓ |

Every load-bearing citation in the αA–αD cohort + SYNTHESIS + HANDOFF resolves as
stated. The one correctness defect is an **orphan REVISE on αE** (the V3 CH5 §C.5
x86-scope widening was not folded into αE's P1 row + exit gate, leaving αE's P1
falsifiability gate `src/`-scoped — false-greening over the live 3554-LOC `ext/x86/`
surface). Detailed below.

---

## §αA — Results Extraction — **ACCEPT**

Every claim cites a RESULTS row, a path:line, or a SHA, and each was re-verified.

- The 51-row JSON >sonic-strict table (§1) is reproduced from `skinny/RESULTS.md`
  `parse_only`/`direct_to_struct`/`real_typed_struct` tuples. Spot-checked twitter
  (8349.3/4913.1/+69.9%), canada (16709.9/12970.9/+28.8%), apache_builds
  (13129.3/12951.7/+1.4%), unicode_escapes (7897.4/2984.1/+164.7%) — **all match RESULTS
  to the decimal.** The Δ-range "+1.4% … +164.7%" is correct and correctly attributed
  (apache_builds thinnest, unicode_escapes widest).
- The CSS >SOTA (§2) is correctly NOT sourced from RESULTS.md (which holds only the
  FALSIFIED 24-row broadcast) but from the W5 close ledger
  (`skv17-W5-close-ledger.md:99-102`) — re-verified: the four N=200 medians and ratios
  match exactly (2.210/2.355/3.348/1.996×).
- **Honesty (H1):** §2.1 states the lazy-vs-eager caveat plainly ("lazy rich-summary
  beats eager full-CSSOM," NOT "equal-work CSSOM beats CSSOM"), with the ~25–33% rich
  rider as the anti-brace-counter evidence. Correct framing.
- The checkasm count is **correctly stated as 12 single-kernel differentials + 2** (§3.4,
  §5) with the explicit warning that an "18 present" gate would be un-satisfiable — disk
  confirms 14 `checkasm_*.rs`. αA is the cohort's canonical source for this correction.
- The x86 figures (24 files / 742 `.rs` + 105 `.asm` = 847 / 14 `unimplemented!`) all
  verify for the `src/x86_64/` surface. **Scope note (not an αA defect):** αA §3.2/§0/§5
  are scoped to `src/x86_64/`; αA's role is the results inventory, and its P1 close gate
  in §5.4 is `find …/x86_64 -type f = 0`. The `ext/x86/` widening is the binding
  contract's (SYNTHESIS/αC) obligation, NOT αA's — αA is a results-extraction artefact,
  not a prune-scope owner; CH5 §C.5 names "αC §1 + αE P1 row + SYNTHESIS P1," not αA. αA
  carries no "x86 gone" close-claim, so no orphan attaches here.
- The competitor strictness plane is correct: Δ vs **sonic-strict** (the `IgnoredAny` +
  `.end()` skipper, `sonic_skipper.rs:3-7` verified), not lossy; sparse
  simdjson/yyjson columns honestly disclosed `n/a` except canada/parse_only.

No misattributed claim, no un-cited number, no wrong-plane comparator. ACCEPT.

## §αB — Competitor Deltas — **ACCEPT**

The most precise honesty artefact in the cohort, and every figure verifies.

- The strictness-plane inventory (§1.1) is correct: sonic strict Skipper
  (`sonic_skipper.rs:1-7`, `Cargo.toml:23` `default-features=false features=["sort_keys"]`
  — no `utf8_lossy`) is the strict bar; sonic lossy is correctly quarantined as
  flaw-probe-only; yyjson/asmjson/RapidJSON are honest `None` on aarch64. Correct plane
  per PASS-ALPHA §4.2 + §9 (the SK-V6 finding).
- The per-corpus Track-1/sonic/Δ table (§1.2) matches RESULTS to the decimal
  (spot-checked twitter/canada/apache/numbers/unicode_escapes; the serde figures
  twitter 857.188 etc. verify). The apache_builds +1.4% canary call-out is correct and
  load-bearing. The instruments +23.9% (αB) vs +23.8% (αA) is a defensible rounding of
  4281.770/3457.276 = +23.85% — not a defect.
- **Honesty (§2, §3.2):** the CSS comparison is framed ASYMMETRIC up front (lazy 9-field
  vs eager full-CSSOM), with the H1 disposition options (A symmetric / B rename+footnote)
  sourced to AGENT-5 §8. The dual N-plane discipline (N=200 headline / N=80 cross-check)
  is disclosed, the canary-plane divergence (material@N=200 vs tailwind@N=80) flagged, and
  the planes are explicitly non-mixable. Correct >SOTA-honest framing.
- **Cross-cohort awareness, correctly disposed (§13/§3.3/§5/§6):** αB *correctly
  identifies* the V3 CH5 §C.5/§F.7 REVISE (the `ext/x86/` + `build.rs` x86 surface, "x86
  gone" literally false until P1 widens crate-wide) and *correctly* states it
  "NEITHER orphan-touches an αB section" — αB makes NO "x86 gone" close-claim; its
  §3.3 asmjson-AVX512-OUT line is the *comparator face* of the mandate (states the
  comparator is OUT, makes no implementation close-claim). This is the correct
  disposition for αB. **But αB's §13 fold-ledger names the REVISE owners as "αC §1 / αE
  P1 row / SYNTHESIS §2"** — i.e. αB itself flags that the αE P1 row is a co-owner of
  this REVISE. This is the pointer that, followed, exposes the αE orphan (§αE below).
  αB's own disposition is correct; it is ACCEPT.
- The GoogleSheets-no-competitor row (§4) is correct: the bar is GENERATION, not
  throughput. The canonical Lock-14 alphabet `Json | CssL4 | Bbnf | GoogleSheets`
  (`LOCKS.md:349`) is cited correctly.

All deltas on the correct plane; the cross-cohort x86 REVISE correctly excluded from αB.
ACCEPT.

## §αC — REDRESS Digest — **ACCEPT**

Every PRUNE wave and pre-block carries a measurable close gate with a verified live
fact, and the V3 CH5 §C.5 x86-scope REVISE is **fully folded** (FOLD-1).

- **FOLD-1 (CH5 C.5) correctly landed:** §0.A.1 + §1-P1 + §2.6 + §3 x86-surface corollary
  widen P1 to delete the ENTIRE x86 surface — `src/x86_64/` (−847) **+ `ext/x86/`
  (−3554) + `build.rs` (−102) + `Cargo.toml` nasm dep + `lib.rs:247` reference ≈ −4500
  LOC**. The P1 close gate is correctly moved from `src/`-scoped to **crate-wide**:
  `grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` (covers `ext/`, `build.rs`,
  `Cargo.toml`). Every cited live fact verifies on disk: `ext/x86/` = 3554 LOC (`bbnf.asm`
  485 + `x86util.asm` 1036 + `x86inc.asm` 1978 + `LICENSE-VENDOR` 55), `build.rs` =
  nasm-rs driver, `Cargo.toml:19 nasm-rs = "0.3"`. **This is the model fold the αE P1 row
  failed to apply.**
- **FOLD-2 (CH2 §8.1) correctly landed:** the relocated-seam enforcement is re-attributed
  to the P3 structural row-count collapse (PRIMARY) with the arm-census grep
  NECESSARY-NOT-SUFFICIENT (secondary) — §1-P3 + §1-P4 + §2.2 + §3 corollary. The
  `RuntimeTarget` neutral-identifier data-table threat is correctly identified as one the
  `match grammar` regex is syntactically incapable of catching.
- P1–P5 close gates are each a concrete runnable predicate; the §0.B state-delta
  (`emit_fact_stream` gone, `W5C_REQUEST_FACT_PROFILES` retirement comment) is correctly
  carried. The crate-path pin (`lock14_baseline.rs` in `bbnf-bench`, not `codegen`) is
  correct. The §4 verification log re-greps every fact live at HEAD.

Gates measurable, citations verified, the binding V3 REVISE folded crate-wide. ACCEPT.

## §αD — Validated/Invalidated Ledger — **ACCEPT** (V3 REVISE resolved)

The V3 CH1 sole REVISE (αD:85 stale "18 differential harnesses") is **resolved**.

- **The V4 VALIDATED row (αD:105) now carries the disk-true 14** ("14 checkasm files =
  12 single-kernel differentials + `checkasm_common.rs` + `checkasm_parity.rs`", with the
  explicit "an '18-present' gate is un-satisfiable on a clean tree" warning), cites the
  live HEAD command `ls bbnf-simd/tests/checkasm_*.rs | wc -l → 14`, and concords with
  αA §3.4 / αC §2.6 / αE F4 / SYNTHESIS / HANDOFF. Disk re-verified: 14 (12+2). The §8.V4
  R1 fold log records the correction explicitly ("18 → 14") and certifies "no
  measurement/disposition/path:line of αD reversed." The §6 verification log (row "checkasm
  harness count (V4 fold R1)") carries the live command and result.
- Every I1–I10 path:line independently re-verified (`CSS_GENERATED_RS:701`,
  `RuntimeEmitterKind:40`, `ValueRef…:175`, `parse_w11_1_number` = 7, md5
  `b654562c…` replica identity, the test-only `G` precision at `event_grammar_tests.rs:18,
  20,89`, `CssEventGrammar` absent). The phantom-`G`-vs-real-`K` two-axis precision and
  the CSS-fact-stream-RETIRED fold are correct.
- The S1 owner row (αD:152) scopes P1 to `bbnf-simd/src/x86_64/` + `lib.rs:5,285-287` +
  `lock14_baseline.rs` x86 entries. **Scope note:** unlike αE (below), αD's S1 is a
  STILL-OPEN ledger pointer (not a falsifiability gate with a runnable close predicate);
  it names the trigger (I7) and parity oracle, and I7 itself is the "x86 tree exists"
  finding. The binding close gate lives in αC/SYNTHESIS. CH5 §C.5 named "αC §1 + αE P1
  row + SYNTHESIS P1" as the fix owners — **not αD's S1** — because αD's S1 is a ledger
  entry, not the prune-scope gate. αD carries no "x86 gone" measurable close-claim that
  the `src/`-scope would false-green. No orphan attaches to αD on this axis.

The V3 REVISE is resolved; αD is internally consistent and well-cited. ACCEPT.

## §αE — Candidate Shortlist — **REVISE** (orphan: the V3 CH5 §C.5 x86-scope fold was not applied to αE)

αE's falsifiability triple, sequencing, checkasm count (12, correct in four places), F13
relocated-seam re-attribution, and F14 no-op confirmation are all correct. **But αE
carries an ORPHAN V3 REVISE: the CH5 §C.5 x86-scope widening was folded into αC /
SYNTHESIS / HANDOFF but NOT into αE — leaving αE's P1 falsifiability gate `src/`-scoped,
i.e. it would PASS GREEN while the live 3554-LOC `ext/x86/` surface + the nasm `build.rs`
survive.** This is precisely the false-greening scope CH5 §C.5 condemned, and the V3 CH5
fix text at `V3/CH5.md:148` explicitly directs the fix to "αC §1 **+ the αE P1 row** +
SYNTHESIS P1." αE folded F13 + F14 but treated CH5 §C.5 as "not a defect IN α-E."

**The orphan, with disk evidence:**

- **`alphaE-candidate-shortlist.md:83` (P1 sub-row):** owner path is
  `skinny/crates/bbnf-simd/src/x86_64/` ONLY; LOC `−847` (`742 .rs + 105 .asm`). It does
  NOT name `ext/x86/` (3554 LOC, verified on disk), `build.rs` (the nasm driver), or the
  `Cargo.toml` `nasm-rs` build-dep. Compare αC §1-P1 (crate-wide, −4500) and SYNTHESIS
  :246 ("EVERY x86 surface … (a) `src/x86_64/` (b) `ext/x86/` (c) `build.rs` (d)
  `lib.rs:247`").
- **`alphaE-candidate-shortlist.md:93` (the P1 exit/falsifiability gate — the
  load-bearing defect):**
  > P1 exit: `grep -rE '_mm(256\|512)?_\|x86_64\|avx\|gfni\|sve' skinny/crates/bbnf-simd/src` → 0; `find crates/bbnf-simd/src/x86_64 -type f` → 0 …

  Both predicates are **`src/`-scoped.** `ext/x86/` is a SIBLING of `src/` and `build.rs`
  is at the crate ROOT — neither is reached by `…/bbnf-simd/src` or `…/src/x86_64`. This
  gate would report **GREEN with 3554 LOC of x86 ASM + an x86-assembler build driver still
  present** — the exact "x86 gone is literally false while it survives green" false-gate
  CH5 §C.5 identified (and αC/SYNTHESIS fixed by moving the verify to crate-wide
  `grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`).
- **`alphaE-candidate-shortlist.md:210` (summary table):** "x86=0 (24 files incl `.asm`)"
  — counts only the `src/x86_64/` 24 files, not the `ext/x86/` surface.
- **`alphaE:97,216` (LOC budget):** net "≈ −7200" / "≈ −9250" understates by ~3654 LOC
  (the un-counted `ext/x86/` + `build.rs`). αC §1-P1 restates P1 alone as ≈ −4500;
  SYNTHESIS :73 notes the widened deletion "only DEEPENS the net-LOC-deleted claim."
- **`alphaE:14,21,230` (the fold-ledger):** αE enumerates only F13 + F14 as the V3
  REVISEs and asserts "neither is a defect IN α-E" + "CH5 §E … ACCEPT on B3/B1/B2/B4."
  This is **incomplete:** CH5 §C.5 (`V3/CH5.md:112-157`) is a SEPARATE V3 REVISE from CH5
  §E (the αE candidate-architecture section), and its fix at CH5:148 names the αE P1 row.
  αB:13 and αC FOLD-1 both independently name "αE P1 row" as a co-owner. αE's claim that
  the only V3 REVISEs touching it are F13/F14 is the source of the miss.

**Why this is a CH1 (correctness) defect, not merely cosmetic:** αE is the candidate
shortlist that feeds the downstream S-P3 wave plan (PASS-ALPHA §2 / §4.4). PASS-ALPHA §3
binds CH1 to "Are falsifiability gates measurable?" — and αE's P1 exit gate is the
falsifiability gate for the entire PRUNE-A candidate. A gate that passes green while the
close condition ("x86 gone," R10 / invariant §5.3) is materially unmet is **not
measurable against its actual close condition**; it is a P4-class false-gate of exactly
the family this cycle exists to delete. An implementer wiring the S-P3 P1 wave from αE's
gate would mark P1 closed with 3554 LOC of x86 ASM surviving. Per ORCHESTRATOR §3Z (zero
orphan REVISE), an un-folded V3 disposition named against αE blocks convergence.

**Concrete fix (localized to αE; mirror αC FOLD-1 / SYNTHESIS):**

1. **`:83` P1 owner row** — widen the owner path to the WHOLE x86 surface:
   `skinny/crates/bbnf-simd/src/x86_64/` AND `skinny/crates/bbnf-simd/ext/x86/` (3554 LOC
   vendored `bbnf.asm`/`x86util.asm`/`x86inc.asm`/`LICENSE-VENDOR`) AND
   `bbnf-simd/build.rs` (nasm-rs driver, delete-or-neutralize) AND drop
   `build = "build.rs"` + `nasm-rs = "0.3"` from `Cargo.toml` AND re-home the
   `src/lib.rs:247` `ext/x86/bbnf.asm` contract reference. Restate the LOC as
   `−847 (src/x86_64/) + −3554 (ext/x86/) + −102 (build.rs) ≈ −4500`.
2. **`:93` P1 exit gate** — replace the `src/`-scoped predicates with the **crate-wide**
   verify (matching αC §1-P1 / SYNTHESIS :246): `find skinny/crates/bbnf-simd/src/x86_64
   skinny/crates/bbnf-simd/ext/x86 -type f` = 0 AND `bbnf-simd/build.rs` gone-or-aarch64-
   neutral AND `Cargo.toml` carries no `build=`/`nasm-rs` AND `grep -riE
   'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` returns only aarch64-neutral comments.
3. **`:210` summary table + `:97`/`:216` LOC budget** — restate "x86=0 (whole surface
   crate-wide)" and re-add the ~−3654 LOC so net A ≈ −10,700 (or restate net per αC's
   ≈ −4500 P1).
4. **`:14`/`:21`/`:230` fold-ledger** — add an **F15** row recording CH5 §C.5 as a V3
   REVISE folded into αE's P1 row + exit gate (mirroring αC FOLD-1 / SYNTHESIS V3 fold
   item (1)), so the V4 fold-ledger is complete and the "neither is a defect IN α-E" claim
   is corrected to "the αE P1 row co-owns CH5 §C.5; folded as F15."

All of αE's other content (B1–B4 architecture, the falsifiability triple, F13 relocated-
seam re-attribution, the corrected checkasm 12+2, the −3% N=200 CSS floors, the
honest-`None` competitor posture) is correct and verified — the REVISE is localized to the
P1 x86-scope rows + the fold-ledger. Disposition REVISE.

## §SYNTHESIS (αF) — **ACCEPT**

The αF contract output. Every Section-0 close-condition gate is measurable, and all three
V3 REVISEs — including the BLOCKING CH5 §C.5/§F.7 x86-scope — are correctly folded.

- **CH5 §C.5/§F.7 (the BLOCKING V3 fold) correctly landed:** §V4-fold preamble (lines
  57–76), the §0 close-condition gate (line 246 "PRUNE P1 — x86 deleted (the WHOLE x86
  surface, crate-wide)"), the ground-truth inventory (lines 157–169 "TWO surfaces"), the
  `x86_tree_deleted` telemetry column (line 491, redefined "NO x86 surface anywhere in
  `bbnf-simd` — `src/x86_64/` gone AND `ext/x86/` gone AND `build.rs` carries no
  nasm/x86-assembler path … verified crate-wide `grep -riE 'avx|gfni|sve|x86|nasm'` …
  NOT `src/`-scoped"), and the path-forward (lines 296, 339–341, 389) all carry the
  crate-wide widening. Every cited `ext/x86/`/`build.rs`/`Cargo.toml` fact verifies on disk.
  **This is the binding contract artefact — its x86 close gate is correct.**
- **CH2 §8.1 (arm-census reach over-claim) correctly folded:** lines 76–91 re-scope the
  grep to "self-disclosing grammar-token" and bind the neutral-identifier data-table
  threat to the P3 structural row-count check (`sort -u` over `RuntimeTarget`
  `(source_roots,entry_rule)`). Correct.
- **CH1 §αD / CH7 §4 (stale "18") correctly handled:** the binding contract (§1, line
  377-378 / 433-435) already carries "12 single-kernel + 2 = 14"; SYNTHESIS notes the
  lone "18" lived only in αD:85 (now fixed). Disk-verified 14.
- The JSON >SOTA range is correctly +1.4%–164.7% (lines 15, 201–202, 269, 427) with the
  widest row correctly attributed to unicode_escapes (the V1 "+1.4%–78%"/marine_ik echo
  error correctly folded). **Honesty (H1):** the CSS framing is lazy-rich-summary vs
  eager-full-CSSOM (lines 210, 258, 304, 415) with a machine-checkable
  `materialization_framing ∈ {lazy-rich-vs-eager-cssom, symmetric-comparator}` column
  (494). The Section-2 telemetry schema is machine-checkable per row; the gate-reject
  conditions (`verbatim_blob_present`, `phantom_generic_resolved`,
  `acceleration_at_admission`, `x86_tree_deleted == true`, single-tuple broadcast) make
  every generalization axis falsifiable; the honest-finding escape is itself gated. The
  competitor strictness plane (§0.6) forbids a fabricated competitor column.

Gates measurable + machine-checkable; citations verified; framing honest; the BLOCKING V3
fold landed crate-wide. ACCEPT.

## §HANDOFF (αF) — **ACCEPT**

- Consistent with SYNTHESIS; the three V3 REVISE folds are correctly recorded (lines
  9–24): **(CH5 §C.5/§F.7 — BLOCKING)** P1 widened crate-wide covering `ext/x86/` +
  `build.rs` + `lib.rs:247`, `x86_tree_deleted` redefined "NO x86 surface anywhere"
  verified crate-wide (lines 12–17, 70–72, 98–102, 208–210, 242–244, 288–289, 315–316);
  **(CH2 §8.1)** arm-census reach re-scoped; **(CH1 §αD/CH7 §4)** the lone "18" in
  alphaD:85 corrected.
- The current-state inventory (path:lines) verifies; the JSON range +1.4%–164.7% with
  unicode_escapes widest (lines 42–43) is correct; checkasm "12 single-kernel + 2 = 14"
  (lines 22–24). Invariant 3 (line 242) correctly states "aarch64-only: zero
  x86/AVX/SVE/nasm in `bbnf-simd` CRATE-WIDE — `src/x86_64/` AND `ext/x86/` AND
  `build.rs` all gone."
- Pre-blocked routes carry full semantics; the Lock-14 three-surface gate model matches
  `LOCKS.md:349`. The revert dependency graph + hard-cap-default carries are measurable
  handoff obligations.

No un-cited or wrong-plane claim; gates measurable; the BLOCKING fold landed. ACCEPT.

---

## §Cross-artefact correctness note (for the CONSOLIDATOR)

**The cohort is internally consistent on every load-bearing fact EXCEPT the x86 prune
scope.** αC, SYNTHESIS, and HANDOFF all carry the V3-CH5-§C.5-corrected **crate-wide**
x86 deletion (`src/x86_64/` + `ext/x86/` + `build.rs` + `Cargo.toml` nasm dep,
crate-wide verify grep). αB correctly identifies the REVISE and correctly excludes itself
(it makes no "x86 gone" close-claim). **Only αE retains the stale `src/`-scoped P1 row +
exit gate** (`:83`, `:93`, `:210`, plus the un-counted LOC at `:97`/`:216`) — the lone
surviving un-folded CH5 §C.5 in the cohort, and the single CH1 V4 REVISE.

This is the V4 structural twin of the V3 cycle's αD:85 stale "18": one binding-feeder
artefact lagging the cohort on a single V-disposition. There, αD lagged on a count; here,
αE lags on a prune scope. Both are localized, mechanical fixes that touch no other
disposition. αE's REVISE is the more consequential of the two: a count error seeds an
un-satisfiable gate (red on a clean tree), whereas αE's `src/`-scoped P1 gate seeds a
**false-green** gate (green on a dirty tree) — the gate would certify P1 closed while
3554 LOC of x86 ASM survive. Resolving it brings the cohort to full x86-scope consistency
and removes the one re-citation path to a `src/`-scoped P1 false-gate flowing into S-P3.

No other CH1 defect. JSON deltas are on the strict-vs-strict plane; CSS deltas disclose
the lazy-vs-eager asymmetry (H1) honestly; the >SOTA framing is honest throughout; every
OTHER falsifiability gate is measurable against its actual close condition.

---

## Disposition summary

| Artefact | Disposition | Basis |
|---|---|---|
| αA Results Extraction | **ACCEPT** | every claim cited + disk-verified; honest H1 framing; checkasm 12 correct; no "x86 gone" close-claim (not a C.5 owner) |
| αB Competitor Deltas | **ACCEPT** | correct strictness plane; serde/sonic figures verify; asymmetry disclosed; correctly excludes itself from the C.5 REVISE |
| αC REDRESS Digest | **ACCEPT** | CH5 §C.5 x86-scope folded crate-wide (FOLD-1); CH2 §8.1 relocated-seam folded (FOLD-2); measurable PRUNE gates |
| αD Validated/Invalidated | **ACCEPT** | V3 REVISE resolved — `:105` V4 row now "12+2=14"; §8.V4 R1 fold; all I1–I10 re-verified |
| αE Candidate Shortlist | **REVISE** | orphan: V3 CH5 §C.5 (x86-scope, named "αE P1 row" at CH5:148) NOT folded; `:93` P1 exit gate `src/`-scoped → false-greens over live 3554-LOC `ext/x86/` + nasm `build.rs`; fix `:83`/`:93`/`:210`/`:97`/`:216` crate-wide + add F15 fold-ledger row |
| SYNTHESIS (αF) | **ACCEPT** | BLOCKING CH5 §C.5/§F.7 folded crate-wide (`x86_tree_deleted` redefined); all V3 REVISEs landed; H1 framing; checkasm 14; path:lines verify |
| HANDOFF (αF) | **ACCEPT** | consistent with SYNTHESIS; crate-wide x86 invariant; checkasm 14; JSON range correct; measurable handoff obligations |

TALLY accept=6 revise=1 reject=0
