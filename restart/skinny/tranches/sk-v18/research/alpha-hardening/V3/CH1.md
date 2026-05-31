# CH1 — CORRECTNESS (cycle V3) — SK-V18 Pass-Alpha CHALLENGE

Lens: **CH1 Correctness** per `PASS-ALPHA.md §3` ("does every claim cite RESULTS.md
row, REDRESS entry, commit SHA, or measurement file? Are falsifiability gates
measurable? Are competitor deltas computed against the correct strictness plane?") +
ORCHESTRATOR §3W. Subject: the Pass-Alpha SK-V18 artefacts
`research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md`.

Discipline applied: every load-bearing path:line / SHA / count / Mbps figure was
re-verified live at HEAD `318d9c046` (the bracket HEAD). Honesty focus per the V3
mandate: >SOTA framing must disclose the lazy-vs-eager asymmetry (H1); gates must be
measurable; competitor deltas must be on the correct strictness plane (sonic-strict,
not lossy).

**Disk re-verification (all PASS at HEAD `318d9c046`):**

| Claim | Command | Result | Status |
|---|---|---|---|
| SK-V17 close SHA | `git log --oneline -1 f6a38445b` | `…W4…W5 close — SK-V17 CLOSED` | ✓ |
| V3 audit SHA | `git log --oneline -1 7dbe44c22` | `audit(skinny-impl-overfit-v3)… SK-V18 = generalization` | ✓ |
| bracket HEAD | `git log --oneline -1` | `318d9c046 docs(sk-v18-handoff)…` | ✓ |
| x86 file count | `find …/x86_64 -type f \| wc -l` | **24** | ✓ |
| x86 `.rs` LOC | `find …/x86_64 -name '*.rs' \| xargs wc -l` | **742** | ✓ |
| x86 `.asm` LOC | `wc -l …/byte_class_from_eq_set_64.asm` | **105** | ✓ |
| x86 `unimplemented!` | grep count | **14** | ✓ |
| CSS replica md5 | `md5 …css_l4_*/generated.rs \| sort -u \| wc -l` | **1** | ✓ |
| `parse_w11_1_number` ×N | `grep -c … json/generated.rs` | **7** | ✓ |
| checkasm file count | `ls …/tests/checkasm_*.rs \| wc -l` | **14** (12 single + 2) | ✓ |
| `CSS_GENERATED_RS` | `sed -n 701p runtime_generator.rs` | `const CSS_GENERATED_RS: &str = r#"` | ✓ |
| `RuntimeEmitterKind` | `sed -n 40p grammar_provider.rs` | `pub enum RuntimeEmitterKind {` | ✓ |
| `ValueRef` two-axis | `sed -n 175p tape/mod.rs` | `…K = AnyKind, G: EventGrammar = AnyGrammar>` | ✓ |
| `render(SinkOnlyProgram)` | `sed -n 4p json_sink_direct.rs` | `pub fn render(program: &SinkOnlyProgram)…` | ✓ |
| sonic strict skipper | `sed -n 3,7p sonic_skipper.rs` | `IgnoredAny::deserialize` + `deserializer.end()` | ✓ |
| `GENERIC_SCAN_ROOTS` line | `grep -n … lock14_baseline.rs` | `:2409` (iter `:2467`, `:2508`) | ✓ |
| old CSS bench | `grep -n 'fn measure_mbps\|fn lightningcss_facts' nonjson_css_l4.rs` | `:3091` / `:528` | ✓ |
| W5 ledger N=200 medians | `sed -n 99,102p skv17-W5-close-ledger.md` | bootstrap 2473.1/1119.1/2.210×; animate 2937.9/1247.7/2.355×; tailwind 2773.4/828.5/3.348×; material 2618.5/1312.0/1.996× | ✓ |
| LOCKS canonical alphabet | `sed -n 349p LOCKS.md` | `Json\|CssL4\|…\|GoogleSheetsParser` + arm-census command | ✓ |
| 16-lock count | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` | **16** | ✓ |
| RESULTS twitter parse_only | `grep '^\| twitter \| parse_only' RESULTS.md` | t1=8349.290 / sonic=4913.095 / serde=857.188 / Δ=+69.9% | ✓ |
| RESULTS canada serde col | field 21 | 4581.994 (matches αB) | ✓ |
| N=80 lightningcss medians | provenance grep | AGENT-5:48 `1086.1 / 827.1 / 1240.4 / 1225.8` | ✓ |

Every load-bearing citation in the cohort resolves as stated. The one correctness
defect is a stale **count** carried into a binding ledger row (αD V4), inconsistent with
the rest of the cohort's correct value — detailed below.

---

## §αA — Results Extraction — **ACCEPT**

Every claim cites a RESULTS row, a path:line, or a SHA, and each was re-verified.

- The 51-row JSON >sonic-strict table (§1) is reproduced from `skinny/RESULTS.md`
  `parse_only`/`direct_to_struct`/`real_typed_struct` tuples. Spot-checked twitter
  (8349.3/4913.1/+69.9%), canada (16709.9/12970.9/+28.8%), apache_builds
  (13129.3/12951.7/+1.4%), unicode_escapes (7897.4/2984.1/+164.7%) — **all match RESULTS
  to the decimal.** The Δ-range "+1.4% … +164.7%" is correct and is correctly attributed
  (apache_builds thinnest, unicode_escapes widest).
- The CSS >SOTA (§2) is correctly NOT sourced from RESULTS.md (which holds only the
  FALSIFIED 24-row broadcast) but from the W5 close ledger
  (`skv17-W5-close-ledger.md:99-102`) — re-verified: the four N=200 medians and ratios
  match exactly (2.210/2.355/3.348/1.996×).
- **Honesty (H1):** §2.1 states the lazy-vs-eager caveat plainly ("lazy rich-summary
  beats eager full-CSSOM," NOT "equal-work CSSOM beats CSSOM"), with the ~25–33% rich
  rider as the anti-brace-counter evidence. Correct framing.
- The checkasm count is **correctly stated as 12 single-kernel differentials** (§3.4,
  §5) with the explicit warning that an "18 present" gate would be un-satisfiable — disk
  confirms 14 `checkasm_*.rs` (12 + `checkasm_common.rs` + `checkasm_parity.rs`). αA is
  the cohort's canonical source for this correction.
- The x86 figures (24 files / 742 `.rs` + 105 `.asm` = 847 / 14 `unimplemented!`) all
  verify. The CH1-Note folds (dual LOC figure; working-tree md5 collapse) are internally
  consistent and disk-true.
- The competitor strictness plane is correct: Δ vs **sonic-strict** (the `IgnoredAny` +
  `.end()` skipper, `sonic_skipper.rs:3-7` verified), not lossy; the sparse
  simdjson/yyjson columns are honestly disclosed as `n/a` except canada/parse_only.

No misattributed claim, no un-cited number, no wrong-plane comparator. ACCEPT.

## §αB — Competitor Deltas — **ACCEPT**

The single most precise honesty artefact in the cohort, and every figure verifies.

- The strictness-plane inventory (§1.1) is correct: sonic strict Skipper
  (`sonic_skipper.rs:3-7`, `Cargo.toml:23` `default-features=false features=["sort_keys"]`
  — no `utf8_lossy`) is the strict bar; sonic lossy is correctly quarantined as
  flaw-probe-only; yyjson/asmjson/RapidJSON are honest `None` on aarch64. This is the
  correct strictness plane per PASS-ALPHA §4.2 + §9 (the SK-V6 finding).
- The per-corpus Track-1/sonic/Δ table (§1.2) matches RESULTS to the decimal
  (spot-checked twitter/canada/apache/numbers/unicode_escapes). The serde figures cited
  (twitter 857.188, canada 4581.994, …) verify against the RESULTS serde column. The
  apache_builds +1.4% canary call-out is correct and load-bearing.
- The instruments Δ is stated +23.9% (αB) vs +23.8% (αA) — both are defensible roundings
  of the same 4281.770/3457.276 = +23.85%; **not a correctness defect** (rounding, not
  miscitation).
- **Honesty (§2, §3.2):** the CSS comparison is framed ASYMMETRIC up front (lazy 9-field
  vs eager full-CSSOM), with the H1 disposition options (A symmetric comparator / B
  rename+footnote) correctly sourced to AGENT-5 §8. The dual N-plane discipline (N=200
  headline / N=80 cross-check, AGENT-5:48 verified) is disclosed, the canary-plane
  divergence (material@N=200 vs tailwind@N=80) is correctly flagged, and the planes are
  explicitly non-mixable. This is the correct >SOTA-honest framing.
- The GoogleSheets-no-competitor row (§4) is correct: the bar is GENERATION, not
  throughput — no fabricated Sheets speed win.

All deltas are on the correct (strict-vs-strict for JSON; disclosed-asymmetric for CSS)
plane. ACCEPT.

## §αC — REDRESS Digest — **ACCEPT**

Every PRUNE wave and pre-block carries a measurable close gate with a verified live
fact.

- P1–P5 close gates are each a concrete, runnable predicate (`find …/x86_64 -type f` =
  0; `grep 'fn measure_mbps\|fn lightningcss_facts'` = 0; md5-distinct census;
  `GENERIC_SCAN_ROOTS` coverage; `grep -c parse_w11_1_number` = 0). All cited live facts
  (24 files, md5=1, ×7 leak, `RuntimeEmitterKind` fork, `CSS_GENERATED_RS:701`) verify.
- The dual x86 LOC figure (742 `.rs` + 105 `.asm` = 847) and the file-count gate framing
  are correct and disk-true.
- The §2 pre-blocks each carry a measurable re-open test keyed to the SK-V18 surfaces
  (generator emission checked TWICE — runtime output AND emitter). The Lock-2 canonical
  name note (`Layout`/`LayoutFacts`, not `StructLayout`) and the Lock-1 no-second-substrate
  clause are correct.
- The crate-path pin (`lock14_baseline.rs` lives in `bbnf-bench`, not `codegen`) is a
  genuine correctness sharpening — verified.
- The §0.B state-delta (`emit_fact_stream` gone, count = 0; `W5C_REQUEST_FACT_PROFILES`
  a retirement comment) is correctly carried so a retired surface is not re-fought.

Gates measurable, citations verified, no wrong-plane claim. ACCEPT.

## §αD — Validated/Invalidated Ledger — **REVISE**

αD is substantively correct — the VALIDATED/INVALIDATED/DEMOTED/STILL-OPEN sets are
accurate, every I1–I10 path:line was re-verified (`CSS_GENERATED_RS:701`,
`RuntimeEmitterKind:40`, `ValueRef…:175`, `parse_w11_1_number` = 7, md5 b654562c…
replica identity, the test-only `G` precision at `event_grammar_tests.rs:18,20,89`), and
the §6 verification log is a model of disciplined re-grep. The phantom-`G`-vs-real-`K`
two-axis precision and the `CssEventGrammar`-absent fold are correct.

**One CH1 correctness defect (REVISE, blocking):**

- **`alphaD-validated-invalidated.md:85` (the V4 VALIDATED row)** states:

  > **NEON checkasm discipline … 18 differential harnesses … `tests/checkasm_*.rs` (18)…**

  Disk truth at HEAD `318d9c046`: `ls …/tests/checkasm_*.rs | wc -l` = **14** (12
  single-kernel differentials + `checkasm_common.rs` + `checkasm_parity.rs`). The "18" is
  **wrong**, and it directly contradicts the rest of the cohort, which all carry the
  corrected count:
  - αA §3.4/§5: "current N=12 single-kernel differentials … any gate must assert against
    12, not a phantom 18."
  - αE F4 / §candidate-A / §cross-cutting-3: "corrected from '18' to the disk-verified 12
    single-kernel differentials + 2."
  - SYNTHESIS §1 (line 377-378): "12 single-kernel differential harnesses + 2 … = 14
    `checkasm_*.rs` total."
  - HANDOFF (CH4 §6 fold note): "'18 differential harnesses' corrected to the disk-true 12
    single-kernel + 2 = 14."

  αD's V4 row is the **lone surviving "18"** in the cohort. This is not a cosmetic
  staleness: αD §1's own fold-preamble folds the phantom-`G`, fact-stream, and
  `CssEventGrammar` sharpenings — but it does NOT fold the checkasm-count correction into
  its V4 row, even though that correction was a V2 CH4 disposition the SYNTHESIS/HANDOFF
  explicitly carry. A carried-forward VALIDATED ledger that asserts "18 differential
  harnesses present" seeds **exactly the P4-class un-satisfiable false-gate anti-pattern
  this entire cycle is fixing** (αA §3.4 names this verbatim: a downstream gate asserting
  "18 present" would red-flag a clean tree). αD is a binding feeder into αE/αF, so the
  stale count is one re-citation away from a downstream gate.

  **Concrete fix:** at `alphaD-validated-invalidated.md:85`, replace "18 differential
  harnesses" → "**12 single-kernel differential harnesses + 2 harness/aggregate
  (`checkasm_common.rs`, `checkasm_parity.rs`) = 14 `checkasm_*.rs` total**" and replace
  the citation "`tests/checkasm_*.rs` (18)" → "`tests/checkasm_*.rs` (14: 12 single + 2);
  see αA §3.4 / §5". Add a one-line V3 fold note in §8 recording the count correction
  (mirroring how αA/αE/SYNTHESIS carry it), so the V4 row is consistent with the cohort
  and cannot re-seed an "18-present" gate.

  (Note: αD §6 verification log and §8 fold table do not re-assert "18" — only the V4 row
  at :85 does. The fix is localized to that one row + a §8 fold note.)

Aside from this single stale count, αD is accurate and well-cited. Disposition REVISE on
the V4 row; the remainder of αD is correct.

## §αE — Candidate Shortlist — **ACCEPT**

Gates are measurable, falsifiable, and the checkasm count is **correct here** (12, via
F4 — this is what makes αD's :85 the lone outlier).

- The falsifiability triple (PRESERVED->SOTA / GRAMMAR-DERIVATION-PROOF /
  DISTINCT-GRAMMAR-OUTPUT) is operationally measurable per candidate: each carries a
  runnable grep/diff/Mbps predicate.
- **PRESERVED->SOTA gates are pinned to concrete numbers:** the CSS B2 gate#1 table
  (bootstrap ≥2398.9 / animate ≥2850.0 / tailwind ≥2690.2 / material ≥2540.0 Track1, −3%
  of the N=200 close-ledger medians) is the correct measurable floor on the correct
  N-plane; the JSON apache_builds +1.4% tripwire is correctly named.
- The competitor plane is correct: cross-cutting note 4 carries the honest-`None`
  posture for yyjson/asmjson/RapidJSON (the gate must NOT require an un-run engine's
  number) — the correct CH1/CH5 strictness-honesty foreclosure.
- The N=80-vs-N=200 plane discipline (cross-cutting note 7) is correct and binds the −3%
  gate to N=200, demoting N=80 to cross-check.
- The checkasm inventory (F4, §candidate-A checkasm-status, §cross-cutting-3) is
  **correctly 12 single-kernel + 2** — the fold that αD failed to apply.
- LOC budgets are floored/ceilinged (P1 −847 incl `.asm`; G6 PMULL-first bounded
  body-count ceiling) — measurable and consistent with disk.
- The neutrality grep alphabet is the canonical `Json|CssL4|(GoogleSheets|Sheets)|Bbnf`
  (F10) matching `LOCKS.md:349` (verified). md5-distinct is correctly stated
  necessary-not-sufficient with the arm-census + type-census co-gate.

Every gate is measurable; every figure verifies. ACCEPT.

## §SYNTHESIS — **ACCEPT**

The αF contract output (SYNTHESIS + HANDOFF together constitute the PASS-ALPHA §2 α-F
deliverable — there is no separate `research/alpha/αF` file, which is contract-compliant
per §2's output mapping "SYNTHESIS.md + HANDOFF.md").

- Every Section-0 close-condition gate is **measurable** with a runnable verify command
  (P1 `find …/x86_64 -type f` = 0; P5 `grep -c parse_w11_1_number` = 0; G2 `grep -c
  CSS_GENERATED_RS` = 0; G3 the FULL-alphabet arm census + type census; G4
  `shared_value_trait_instantiations >= 2` production-only). All cited path:lines verify.
- **The checkasm count is correct (§1, line 377-378): "12 single-kernel … + 2 = 14"** —
  the V2 CH4 fold correctly carried (this is precisely the correction αD:85 dropped).
- **Honesty (§0.6, H1, §Section-2 `materialization_framing` column):** the CSS >SOTA is
  framed lazy-rich-summary vs eager-full-CSSOM with a machine-checkable
  `materialization_framing ∈ {lazy-rich-vs-eager-cssom, symmetric-comparator}` column;
  the JSON plane is strict-vs-strict with yyjson/asmjson/RapidJSON honest-`None`. Correct.
- The JSON >SOTA range is correctly +1.4%–164.7% (§0.2, §1) with the widest row
  correctly attributed to unicode_escapes (the V1 "+1.4%–78%"/marine_ik echo error is
  correctly folded, line 14-15, 150).
- The Section-2 telemetry schema is machine-checkable per row, and the gate-reject
  conditions (`verbatim_blob_present == true`, `phantom_generic_resolved == phantom`,
  `acceleration_at_admission == cfg-test-only`, single-tuple broadcast) make every
  generalization axis falsifiable. The honest-finding escape is itself gated (a)-(c) so it
  cannot paper-close.
- The competitor strictness plane (§0.6) is correct and explicitly forbids a fabricated
  competitor column.

Gates measurable + machine-checkable; citations verified; framing honest. ACCEPT.

## §HANDOFF — **ACCEPT**

- Consistent with SYNTHESIS; the three V2 REVISE folds (neutrality alphabet,
  checkasm-count, shared-trait test-exclusion) are correctly recorded (line 11-18), and
  **the checkasm count is correct here too: "18 … corrected to the disk-true 12
  single-kernel + 2 = 14"** (line 15-16).
- The current-state inventory (path:lines) verifies; the JSON range +1.4%–164.7% with
  unicode_escapes widest is correct.
- Pre-blocked routes carry full semantics; the Lock-14 three-surface gate model (invariant
  5) matches `LOCKS.md:349` (verified the canonical alphabet + arm-census command).
- The revert dependency graph + hard-cap-default carries are measurable handoff
  obligations (not paper-closes). The gate consumer enumerates the machine-checkable
  columns.

No un-cited or wrong-plane claim; gates measurable. ACCEPT.

---

## §Cross-artefact correctness note (for the CONSOLIDATOR)

The cohort is internally consistent on every load-bearing fact **except** the checkasm
count: αA (12), αB (n/a), αC (n/a), αE (12), SYNTHESIS (12), HANDOFF (12) all carry the
disk-true 12-single-kernel figure; **only αD:85 carries the stale "18."** This is the
sole orphan-staleness in the cohort and is the single CH1 REVISE. It is a localized,
mechanical fix (one row + one §8 fold note) and does not touch any αD measurement,
disposition, or path:line — αD's substance is otherwise correct. Resolving it brings the
cohort to full count-consistency and removes the one re-citation path to a P4-class
"18-present" un-satisfiable gate.

No other CH1 defect. JSON deltas are on the strict-vs-strict plane; CSS deltas disclose
the lazy-vs-eager asymmetry (H1) honestly; the >SOTA framing is honest throughout; every
falsifiability gate is measurable with a runnable predicate.

---

## Disposition summary

| Artefact | Disposition | Basis |
|---|---|---|
| αA Results Extraction | **ACCEPT** | every claim cited + disk-verified; honest H1 framing; checkasm 12 correct |
| αB Competitor Deltas | **ACCEPT** | correct strictness plane; serde/sonic figures verify; asymmetry disclosed |
| αC REDRESS Digest | **ACCEPT** | measurable PRUNE close gates; pre-blocks keyed + verified |
| αD Validated/Invalidated | **REVISE** | `:85` V4 row "18 differential harnesses" stale — disk = 14 (12+2); contradicts αA/αE/SYNTHESIS; P4-class false-gate seed; fix the row + §8 fold note |
| αE Candidate Shortlist | **ACCEPT** | measurable falsifiability triple; checkasm 12 correct; honest-`None` competitor posture |
| SYNTHESIS (αF) | **ACCEPT** | machine-checkable gates; checkasm 12 correct; H1 framing; all path:lines verify |
| HANDOFF (αF) | **ACCEPT** | consistent with SYNTHESIS; checkasm 12 correct; measurable handoff obligations |

TALLY accept=6 revise=1 reject=0
