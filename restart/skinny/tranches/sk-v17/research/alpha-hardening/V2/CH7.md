# CH7 — OVERFIT-PRUNE (Pass Alpha SK-V17, cycle V2)

Lens: CH7 OVERFIT-PRUNE. Focus: **no contrivance** — no fixture/FNV/broadcast/fact-stream
re-entry; CSS variants derived from grammar projections, not hand-curated; the path is
**genuinely generalized, not CSS-special-cased**. Adversarial review of
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA..alphaE}.md` + `SYNTHESIS.md` +
`HANDOFF.md` per PASS-ALPHA §3 + ORCHESTRATOR §3W/§3Z.

Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25` (verified
`git rev-parse HEAD` = `1c5bd7a25250640f3a6fcfc00abed11f556f674f`). Every disposition
carries a path:line + the measured/grepped fact it rests on.

**Cycle context.** V1 CH7 produced ONE substantive REVISE (the relocated-CSS-overfit
pruning gate, missing a falsifiability test) + four citation/number/scope REVISEs, and
flagged αF/SYNTHESIS/HANDOFF as unauthored (implicit REVISE-when-authored). This V2
review (a) verifies the V1 dispositions folded, and (b) extends the lens to the
now-authored αE-V2, SYNTHESIS, and HANDOFF.

---

## §1 — Verification battery (re-greped at HEAD `1c5bd7a25`, this lens)

| Claim under test | Artefact cite | Ground truth (verified this cycle) | Verdict |
|---|---|---|---|
| `StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`/`begin_compound` ABSENT from skinny | αA §0:48; αC §0; αD §0; αE §0:78; SYNTH:26-30 | `grep -rln` across `skinny/crates/` = **EMPTY** (✓) | CONFIRMED |
| fixture parse fns = **148** (not the stale 187) | αC §5:268; αD O5:148; αE C0:53; SYNTH (implied) | `grep -c "fn parse_" generated_real_typed.rs` = **148** (✓) | CONFIRMED (V1 defect FIXED) |
| `W5C_REQUEST_FACT_PROFILES` const exists, hand-coded CSS routing | αC §0/§3; αD O5; αE C0:52; SYNTH:47; HANDOFF:17 | `codegen/src/lib.rs:336` const decl; iterated :567,:611; declared :299 (✓) | CONFIRMED |
| i8mm grep-clean-absent from skinny | αE C4b:376; αD O3; SYNTH:53 | `grep -rln i8mm skinny/crates/` = **EMPTY** (✓) | CONFIRMED |
| `css_l4.toml` 594 / `json.toml` 34 — the overfit asymmetry — is a TOTALITY (repo-root xtask) artefact, NOT skinny | αC §0:34; αD §0:39 + O5:148 | `wc -l xtask/runtime-projections/{css_l4,json}.toml` = **594/34** (✓); `find skinny -path '*projection*' *.toml` = **EMPTY**; the only `css_l4.toml` is repo-root `xtask/runtime-projections/` (✓) | CONFIRMED |
| `digit_mac` udot orphan: asm present, never called in skinny runtime | αD O3; αE C4a:343; SYNTH:50-52 | `digit_mac.rs:5` `fn parse_4_digits`, `:27` `parse_4_digits_dotprod`, `:40` `udot`; `grep parse_4_digits skinny/crates/runtime/` = **EMPTY** (orphan ✓) | CONFIRMED |
| `select_classifier`/`PrimitiveKernels`/`OnceLock`/`lo6_table_admissible` grammar-general entry | αE C2:235; αA §7:342; SYNTH:49 | `dispatch.rs:42` `select_classifier`, `:50` `PrimitiveKernels`, `:59` `OnceLock`, `:101` `lo6_table_admissible` (✓) | CONFIRMED |
| benched CSS Track 1 = `Result<String,String>` fact-stream | αA §0:50; αB §0.1:68; αC §3:164; αD §0:38; αE C0:113; SYNTH:37; HANDOFF:15 | `nonjson_css_l4.rs:596` `pub fn track1_facts(input:&str)->Result<String,String>` (✓) | CONFIRMED |
| `sheets_witness` is a 25-line stub; BBNF-self absent | αD §0:55; αE §0:59; SYNTH §0.4:218 | `sheets_witness/` = `event_grammar_witness.rs` 24 + `mod.rs` 1 = **25 LOC** (✓) | CONFIRMED |
| Lock 14:349 ("CSS L4 colour-function emit … encoded in metadata+source, NOT branching code"); Lock 2:160 (`StructLayout` retired → `Layout`); Lock 14 census:380 (one-witness scoping) | αC §7; αD; αE §3; SYNTH | All verified EXACT against `restart/locks/LOCKS.md` (Lock 14 even names "CSS L4 14-variant `OpenFrame`" as the failure mode) (✓) | CONFIRMED |

Every load-bearing fact this lens depends on is grep-verified true at HEAD. No uncited
number survives into V2.

---

## §2 — The V1 → V2 fold ledger (did the V1 dispositions land?)

V1 CH7 produced five required revisions (§5 of V1 CH7). Each is verified folded:

| V1 revision | V2 status | Evidence |
|---|---|---|
| **1. Add a TOML-convergence / grammar-source-derivation pruning gate to C1+O1+O5** | **FOLDED** | αE C1 pre-blocks now carry "**no relocated-overfit into projection data**" + a derive-from-grammar-source pruning test (C1:218-225: "wave FAILS if CSS needs per-rule-id branching JSON does not, OR if the CSS projection/route surface does not trend toward parity with the JSON route surface; every residual CSS entry must name the `.bbnf` rule"). αD O1:144 carries the identical "Anti-relabel pruning gate (Lock 14, LOCKS.md:349)". αD O5:148 makes "TOML-LOC convergence an explicit telemetry-bound exit gate". SYNTH §0.1 Layout-driven-projection gate (:101) carries it. |
| **2. Bind the Lock-14 census as a C1 exit condition (Sheets/BBNF-self witness)** | **FOLDED** | αE §3:471-480 + C1:188-197 downgrade four-grammar generality to "**JSON-witnessed, Sheets-required-as-EXIT-gate**" (not "by construction"); αD §0:53-59 + O1/O2 carry "JSON-witnessed only; Sheets/BBNF by-construction-not-by-exercise"; SYNTH §0.4 generality clause (:215-224) makes it a binding exit choice. The 25-line `sheets_witness` stub reality is disclosed everywhere. |
| **3. Fix the path citations (skinny tape, `Layout`/`LayoutFacts` not `StructLayout`)** | **FOLDED** | αA §6:292-301 + §8:376-378 now cite skinny `TapeBuilder`/`Tape`/`ValueRef`/`PayloadArena` and flag `TapeStructBuilder` as core-tree-EMPTY; αC §2b:122 cites `Layout`/`LayoutFacts` + "Lock 2 RETIRED the name `StructLayout`"; αD §0:30-39 table maps every core symbol to its skinny equivalent. |
| **4. Fix the fixture-fn integer 187 → 148** | **FOLDED** | αC §5:268 ("148 … the architecture doc's '187' is stale"); αD O5:148; αE C0:53. |
| **5. De-bake inferred per-corpus endpoints (animate↔164/tailwind↔51)** | **FOLDED** | αB §2:147-169 marks every inferred cell `[INF]` **inline** + UNMEASURED-PENDING; §6.2:265-274 forbids any wave exit-gate keying on an inferred endpoint; SYNTH §0.5:244-247 carries "All per-corpus endpoints are UNMEASURED-PENDING". |

**All five V1 revisions are folded with verified evidence.** This is a clean convergence
on the V1 CH7 dispositions — no orphan REVISE carries forward.

---

## §3 — Per-section dispositions (V2)

### alphaA (results extraction) — ACCEPT (V1 REVISE cleared)

- **§0–§5, §6, §7, §8: ACCEPT.** The V1 §6 V6 / §8 path defect is fixed: §6:292-301 and
  §8:376-378 now cite the skinny tape (`TapeBuilder`/`Tape`/`ValueRef`/`PayloadArena`,
  `mod.rs:94,175,38`) and explicitly mark `TapeStructBuilder`/`crates/core/...` as
  core-tree-EMPTY fold-targets. The 8-field equality (10136/9561/9561/20043) is correctly
  framed as the structural-honesty / anti-flatten gate (§3:182-185) with rich value-plane
  population (dimensions=2963/colors=1169/functions=883/lists=6754, §3:179). The 24-row
  broadcast is correctly pre-blocked, not lifted as baseline (§2:122-137). The neutrality
  vehicle is the skinny `select_classifier(alphabet)`/`lo6_table_admissible`
  (§7:340-348, verified dispatch.rs:42,101) — not architecture-doc names. No contrivance.

### alphaB (competitor deltas) — ACCEPT (V1 §2 REVISE cleared)

- **§0–§6 + verification ledger: ACCEPT.** The V1 §2 inferred-endpoint REVISE is fully
  folded: every per-corpus cell is marked `[INF]` inline (§2:149-152, §3:184-187),
  UNMEASURED-PENDING is explicit (§2:166-169), and §6.2:265-274 forbids any wave exit-gate
  from keying on the inferred animate↔164 / tailwind↔51 / material↔60 endpoints until the
  N≥50 harness emits the split. Only the corpus-aggregate ~14× / ~36× rows are cited as
  measured. The §0.1 benched-substrate disclosure (the benched Track 1 is a `String`,
  the typed CSSOM is the *intended* SK-V17 subject) is the correct anti-contrivance honesty
  — it prevents mistaking a not-yet-built typed product for a measured one. The
  cssparser-is-not-the-speed-bar plane discipline (§0, §3) is the CSS analogue of the
  SK-V6 `utf8_lossy` finding and is sound. No contrivance.

### alphaC (REDRESS digest) — ACCEPT (V1 §2b + §5-number REVISEs cleared)

- **§0–§8: ACCEPT.** This is the strongest overfit-prevention artefact and the V1 defects
  are fixed. §2b (the generality vehicle) now correctly cites `Layout`/`LayoutFacts` +
  `BackendRule.backend_shape` (`ir/cost.rs:119-121,259-271`) and states "**Lock 2
  (LOCKS.md:160) RETIRED the name `StructLayout`**" (§2b:122) — the V1 lock-retired-construct
  defect is gone. §5:268 corrects 187→148 with the grep cited inline. The two-bucket
  PERMANENT-PRE-BLOCK vs ADMIT-UNDER-FRAMING split holds, every re-open test greps the
  **skinny** benched tree (§0:45, §7:333, §8:363), and each pre-block carries a verified
  LOCKS anchor. Critically for my lens: §3:191-195 adds the **retirement clause** (CH3/CH5
  fail if the 7 `RequestFacts` registrations or `W5C_REQUEST_FACT_PROFILES` still drive an
  admitted row), §2b:143 + §5:284 forbid "any new hand-coded per-grammar profile/route
  table parallel to `W5C_REQUEST_FACT_PROFILES` (relocated overfit — Lock 14)". §0:34
  correctly classifies `css_l4.toml` as a **core-tree artefact that does NOT exist in
  skinny** — so the skinny overfit fingerprint is `W5C_REQUEST_FACT_PROFILES` + the 7
  registrations + 148 fns, all skinny-greppable. No fact-stream/FNV/broadcast/fixture
  re-entry survives. This is airtight.

### alphaD (validated/invalidated ledger) — ACCEPT (V1 O1 + O5 REVISEs cleared)

- **§0–§5: ACCEPT.** The central V1 CH7 finding (O1 "thread CSS-specific routing as DATA"
  was the relocated-overfit seam) is fully discharged: O1:144 now carries an explicit
  "**Anti-relabel pruning gate (Lock 14, LOCKS.md:349):** … wave FAILS if CSS needs match
  arms / hand-curated packing constants JSON does not, OR if the CSS regen profile array
  does not trend toward the JSON shape." O5:148 makes "TOML-LOC convergence an explicit
  telemetry-bound exit gate" and correctly tags the 594-line `css_l4.toml` as a
  "**TOTALITY-tree artifact** … the fold target, not a skinny owner path" (the wrong-tree
  trap is named, not stepped into). The 187→148 fix is in O5. The JSON-witnessed-only
  generality downgrade (§0:53-59) is honest. I1 (per-lever micro-opt does not move the
  floor: 3.093→3.178 noise) is correctly banked as anti-contrivance. No contrivance.

### alphaE (candidate shortlist) — ACCEPT (V1 C0-number + C1-pruning REVISEs cleared)

- **§0, C0, C1, C2, C3, C4a, C4b, §2–§4: ACCEPT.** The V2 changelog (αE:12-34) documents
  the exact V1 fold: C4 split into C4a (unconditional orphan wiring) / C4b (GATED net-new
  i8mm, lands only if re-profile proves the digit leaf is top-N tailwind); C0 retire-list
  names `W5C_REQUEST_FACT_PROFILES`; §3 binds the generality witness as an EXIT gate
  (JSON-witnessed, Sheets-required); C1 pre-blocks carry no-relocated-overfit +
  derive-from-grammar-source pruning; 187→148 corrected; lightningcss bar = same-run
  re-baselined median, not a frozen literal. C2 remains the model anti-contrivance
  candidate: it REUSES the checkasm-gated `select_classifier`/`PrimitiveKernels` surface,
  keys on the grammar's alphabet (NOT CSS literals), produces ONLY a `Vec<u32>` index
  (speed from scan, never from dropping structure), and `lo6_table_admissible` is the
  honest scalar-fallback when the CSS alphabet collides — genuine generalization. C4b's
  hard entry gate ("if the digit leaf is not top-N, C4b does NOT land — no orphan kernel")
  is the correct no-orphan-kernel discipline. The de-bake of the per-corpus endpoints and
  the N≥50-median binding are present. No contrivance.

### SYNTHESIS.md (αF contract draft) — REVISE (one residual seam) + otherwise ACCEPT

- **Benched-surface note, §0.1 most gates, §0.2–§0.4, §0.5, §0.6, Section 1–3: ACCEPT.**
  The contract carries the V1 CH7 carry-forward §7 requirements verbatim: skinny (not
  core-tree) path citations (the binding benched-surface note :21-58), the
  derive-from-grammar pruning intent in the Layout-driven-projection gate, the Lock-14
  witness-honest generality clause (§0.4:215-224), the UNMEASURED-PENDING per-corpus
  marking (§0.5:244-247), the corrected discipline throughout. The `tape_activated`
  telemetry column is honestly defined as "NOT satisfiable by a grep in `crates/core/`"
  (Section 2:330) — directly closing the wrong-tree-dishonesty escape. The pre-block §0.4
  names `W5C_REQUEST_FACT_PROFILES` retirement and forbids "relocating its per-rule
  branching into projection DATA … every residual CSS routing entry must name the `.bbnf`
  rule it derives from" (:178-183). This is the pruning gate stated correctly and
  skinny-greppably. Strong contract.

- **§0.1 Layout-driven projection gate — the css_l4.toml clause: REVISE (residual
  wrong-tree seam).** The gate at SYNTHESIS:101 states "*the 594-vs-34-line `css_l4.toml`-
  vs-`json.toml` asymmetry must trend toward parity*" as part of a CSS **close condition**.
  But `css_l4.toml` is grep-confirmed a **TOTALITY (repo-root `xtask/runtime-projections/`)
  artefact — it does NOT exist in `skinny/`** (`find skinny -path '*projection*' *.toml`
  = EMPTY; verified §1 above). αC §0:34 and αD §0:39/O5:148 both correctly flag this and
  scope the css_l4.toml metric as a "(TOTALITY fold)" item, NOT a skinny owner-path gate —
  but SYNTHESIS folds the css_l4.toml clause into a close condition (a Gate-table row) and
  into the benched-surface §0.1 **without the totality-fold caveat the source artefacts
  carry**. As worded, a wave could be marked "met/not-met" on a metric (`css_l4.toml` LOC)
  that lives on the un-benched tree — the exact wrong-tree dishonesty this contract
  elsewhere REJECTs (benched-surface note :29-30; `tape_activated` :330). The
  skinny-verifiable pruning gate is already present and sufficient (W5C retirement +
  "no per-rule-id match arms in skinny generic crates" + "every residual CSS routing entry
  names its `.bbnf` rule" + the CSS regen-profile-array-trends-toward-JSON-shape, all
  skinny-greppable). **Fix:** demote the `css_l4.toml`-trends-toward-`json.toml` clause in
  SYNTHESIS:101 (and the parallel HANDOFF CH7 §:122 mention) to an explicitly-labelled
  **SK-V18 totality-fold metric (informational, not an SK-V17 close gate)**, mirroring
  αD O5:148's "(TOTALITY fold)" tag; keep the skinny close gate keyed strictly to the
  W5C-retirement + no-match-arms + every-residual-names-its-`.bbnf`-rule + regen-profile-
  array-trends-toward-JSON-shape tests, all of which grep `skinny/crates/`. This is the
  single substantive V2 REVISE — it does not change the contract's intent (which is
  correct), only removes a residual gate that could "close" on the wrong tree.

### HANDOFF.md — REVISE (inherits the same css_l4.toml seam) + otherwise ACCEPT

- **Benched-substrate disclosure, Current State, What SK-V17 Opens, Authority, Gate
  Posture, Pre-Blocked Routes, Next Move: ACCEPT.** The handoff cites the skinny tape
  correctly (:11-17), names the `W5C_REQUEST_FACT_PROFILES` retirement as a pre-block
  (:137-138, "RETIRE, do not extend or relocate into projection data — the overfit re-entry
  seam"), carries the no-second-substrate Lock-1 clause (:147-150), the JSON-witnessed
  generality reality, and the `tape_activated`-not-by-`crates/core/`-grep gate (:193-196).
  The CH7-scope paragraph (:116-123) correctly frames CH7 as a pass-added monotonic
  extension lens (CH1-CH6 is the §3W canon; CH7 is added beyond it) — an honest
  orchestrator-discipline framing.

- **CH7-scope paragraph — css_l4.toml LOC trend (:122): REVISE (same seam as SYNTHESIS).**
  The HANDOFF states CH7's scan scope includes "*that the `css_l4.toml` LOC trends toward
  `json.toml` parity*". Same defect as SYNTHESIS:101 — `css_l4.toml` is a totality artefact
  absent from skinny, so a CH7 scan gate keyed to it cannot be discharged on the benched
  surface. **Fix:** reword to the skinny-greppable scope already used in the pre-block
  (:137-138): CH7 checks (a) `W5C_REQUEST_FACT_PROFILES` is retired and not relocated into
  projection DATA, (b) no per-rule-id `match` arms or hand-curated packing/color constants
  enter the skinny generic crates that JSON does not need, (c) every residual CSS routing
  entry names its `.bbnf` rule, (d) the CSS regen profile array trends toward the JSON
  shape — and note the `css_l4.toml` LOC convergence as an SK-V18 totality-fold metric only.

---

## §4 — The contrivance ledger (my lens's bottom line, V2)

| Contrivance vector | Re-entry blocked? | By which artefact clause | CH7 V2 verdict |
|---|---|---|---|
| Fact-stream String as admitted product | YES (permanent pre-block + retirement clause) | αC §3:191-195; αE C0:151-158; SYNTH §0.4:175-177; HANDOFF:134-138 | clean |
| 24-row broadcast (one tuple ×N rows) | YES (permanent, no re-frame; gate rejects single-tuple broadcast) | αC §4; αB §2/§6; SYNTH Section 2:351-353 | clean |
| FNV closed-enum arbiter | YES (bench-quarantine only) | αC §5a; αD §4; SYNTH §0.4:186-189 | clean |
| Fixture-named parse fns / per-corpus capacity consts | YES (input.len()-sized, grammar-general; 148-fn surface named for retirement) | αC §5b; αD O5:148; αE C0/C4; SYNTH §0.4:186-189 | clean (number now 148, FIXED) |
| x86 / AVX / SVE | YES (out-of-scope, diagnostic-only) | αC §6; αE C2/C4 pre-blocks; SYNTH §0.4:190-191 | clean |
| **CSS-special-casing relocated into projection DATA** (hex packing / color order / rule-id sets as TOML or `match rule_id`) | **YES (the V1 open risk is now bound)** — falsifiable skinny pruning gate present | αE C1:218-225; αD O1:144 + O5:148; SYNTH §0.4:178-183 ("every residual CSS routing entry must name the `.bbnf` rule") | **clean** (V1's one substantive REVISE is folded) |
| Inferred per-corpus numbers baked into goalset | YES (UNMEASURED-PENDING, no exit-gate may key on them) | αB §2/§6.2; SYNTH §0.5:244-247 | clean (V1 REVISE folded) |
| Citing core-tree (`StructLayout`/`TapeStructBuilder`/`css_l4.toml`) as the benched surface | **PARTIAL** — αA/αC/αD/αE all corrected; but SYNTH:101 + HANDOFF:122 fold the `css_l4.toml`-LOC metric into a close/scan gate without the totality-fold caveat | SYNTH §0.1; HANDOFF CH7 §ª | **REVISE** (the one residual seam) |

The headline: **the V1 open contrivance risk (relocated CSS-special-casing into projection
data) is now bound with a falsifiable, skinny-greppable pruning gate** — αE C1, αD O1/O5,
and SYNTH §0.4 all carry "no per-rule-id match arms JSON does not need; every residual CSS
routing entry names its `.bbnf` rule." The discipline holds. The single V2 REVISE is a
narrower descendant of the same theme: the `css_l4.toml`-LOC-convergence metric (a TOTALITY
artefact) leaked into a close/scan gate in SYNTHESIS:101 + HANDOFF:122 without the
totality-fold caveat the source artefacts (αC §0, αD O5) carry — making one sub-gate
potentially "closeable" on the un-benched tree. The fix is a one-line demotion to an
informational SK-V18 fold metric; the skinny-greppable pruning gate already stands on its own.

---

## §5 — Required revisions (concrete, for V3 / commit)

1. **SYNTHESIS.md §0.1 Layout-driven-projection gate (line 101):** demote the clause
   "*the 594-vs-34-line `css_l4.toml`-vs-`json.toml` asymmetry must trend toward parity*"
   from an SK-V17 close condition to an explicitly-labelled **SK-V18 totality-fold metric
   (informational)**, matching αD O5:148's "(TOTALITY fold)" tag (`css_l4.toml` is a
   repo-root `xtask/runtime-projections/` artefact, grep-absent from `skinny/`). Keep the
   SK-V17 close gate keyed to the skinny-greppable tests: `W5C_REQUEST_FACT_PROFILES`
   retired, no per-rule-id `match` arms / hand-curated packing-color constants in skinny
   generic crates that JSON does not need, every residual CSS routing entry names its
   `.bbnf` rule, and the CSS regen profile array trends toward the JSON shape.
2. **HANDOFF.md CH7-scope paragraph (line 122):** reword the "*`css_l4.toml` LOC trends
   toward `json.toml` parity*" scan-scope item to the skinny-greppable scope already in the
   pre-block (:137-138), and note `css_l4.toml` LOC convergence as an SK-V18 totality-fold
   metric only.

No other revision is required. All five V1 CH7 revisions are folded with verified evidence
(§2). No fixture/FNV/broadcast/fact-stream re-entry survives; no x86/SVE admission; no
flattened-AST contrivance; the relocated-projection-data risk is now falsifiably gated.

---

## §6 — Disposition summary

**13 reviewable sections** across alphaA–E + SYNTHESIS + HANDOFF (αF is realised as the
SYNTHESIS + HANDOFF pair; there is no separate `alphaF-contract-draft.md` — the contract
draft was authored directly into the tranche-root `SYNTHESIS.md`/`HANDOFF.md` per
PASS-ALPHA §6, which this lens accepts as the αF deliverable).

- **ACCEPT (11):** αA (all §); αB (all §); αC (all §); αD (all §); αE (§0, C0, C1, C2, C3,
  C4a, C4b, §2–§4); SYNTHESIS (benched-surface note, §0.2–§0.4, §0.5, §0.6, Section 1–3,
  and the W5C-keyed portion of §0.1); HANDOFF (benched-substrate disclosure, Current State,
  What SK-V17 Opens, Authority, Gate Posture, Pre-Blocked Routes, Next Move).
- **REVISE (2):** SYNTHESIS §0.1 Layout-driven-projection gate (the `css_l4.toml`-LOC
  totality-tree clause leaked into a close condition); HANDOFF CH7-scope paragraph (the
  same `css_l4.toml`-LOC scan-scope leak).
- **REJECT (0):** No section proposes a fixture/FNV/broadcast/fact-stream re-entry, an x86
  admission, a flattened-AST contrivance, or a relocated-projection-data overfit. The
  discipline holds.

Counts: **ACCEPT 11 / REVISE 2 / REJECT 0** → 84.6% ACCEPT.

The path is **genuinely generalizing, not CSS-special-cased** — the C2 NEON reuse
(`select_classifier`/alphabet, not CSS literals), the 8-field equality anti-flatten gate,
the W5C-retirement-and-derive-from-grammar pruning gate, and the JSON-witnessed-only
generality honesty prove it. The V1 single substantive REVISE (the relocated-overfit
pruning gate) is folded and verified. The two V2 REVISEs are a single residual seam (a
totality-tree `css_l4.toml`-LOC metric leaked into an SK-V17 close/scan gate) that the
source research artefacts (αC §0, αD O5) already handle correctly; the fix is a two-line
demotion to an informational SK-V18 fold metric. With that landed, CH7 closes clean.

(Note for the orchestrator §3Z tally: this lens is below the 95% ACCEPT bar at 84.6% on a
13-section denominator because the 2 REVISEs are real, but both are one-line wording
demotions of the same totality-vs-skinny seam, fully concrete and non-orphan — they will
clear in a single V3 fold.)
