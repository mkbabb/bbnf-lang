# CH7 — OVERFIT-PRUNE (Pass Alpha SK-V17, cycle V1)

Lens: CH7 OVERFIT-PRUNE. Focus: **no contrivance** — no fixture/FNV/broadcast/fact-stream
re-entry; CSS variants derived from grammar projections not hand-curated; the path is
**genuinely generalized, not CSS-special-cased**. Adversarial review of
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA..E}.md` per PASS-ALPHA §3 + ORCHESTRATOR §3W.

Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25` (verified `git rev-parse HEAD`).
Every disposition below carries a path:line + the measured/grepped fact it rests on.

**Scope note (binding on counts):** αF (`alphaF-contract-draft.md`), `SYNTHESIS.md`, and
`HANDOFF.md` are **not yet authored** (directory holds alphaA–E only; verified `ls`). They
are therefore out of scope for V1 disposition and carry an implicit REVISE-when-authored
flag noted in §7. The contrivance/overfit discipline below MUST be carried into αF verbatim.

---

## §1 — Verification battery (what I grepped at HEAD `1c5bd7a25`)

| Claim under test | Artefact cite | Ground truth (this lens verified) | Verdict |
|---|---|---|---|
| `generated_real_typed.rs` is 187 fixture-named fns | αE C0 §5; αC §5 ("187 fixture fns") | File is **4941 lines** (✓), but `grep -c "fn parse_"` = **148**, total `fn ` = 174. The "187" is **uncited/wrong**. | NUMBER DEFECT |
| `css_l4.toml` 594 vs `json.toml` 34 lines (the overfit asymmetry) | αE C0:108; αD O5; αC §5 | `wc -l` = **594 / 34** EXACT (✓) | CONFIRMED |
| skinny CSS emits fact-stream String (`emit_fact_stream`, `track1_facts -> Result<String,String>`, push_str/fnv) | αE C0:65–69; αC §3; αD §3 O1 | `css_l4_declaration_values/generated.rs:5` `pub fn emit_fact_stream(input:&str)->Result<String,CssFactError>`; `nonjson_css_l4.rs:596` `track1_facts -> Result<String,String>`; generated via `RuntimeEmitterKind::RequestFacts` (`lib.rs:291,326`) (✓) | CONFIRMED |
| W6 harness single-sample (`sample_count=1`) | αA §1; αB §0; αE §0; αD I7 | `nonjson_css_l4.rs:1134` `sample_count: track1_measure.iterations` (✓) | CONFIRMED |
| `digit_mac` udot orphan: asm! present, never called in prod | αE C4; αD O3 | `digit_mac.rs:27` `parse_4_digits_dotprod`, asm!`udot` at :39/:62 (✓); `grep parse_4_digits skinny/crates/runtime/` = **EMPTY** (orphan confirmed). Cited lines `:38-40,62-63` drift ~1 line (udot at 27, asm at 39). | CONFIRMED (line drift) |
| `i8mm` ZERO in skinny | αE C4:272 | `grep -rln i8mm skinny/crates/` = **EMPTY** (✓) | CONFIRMED |
| `PrimitiveKernels`/`select_classifier` OnceLock fn-table exists, grammar-general | αE C2:160–164 | `dispatch.rs:42` `select_classifier`, `:50` `PrimitiveKernels`, `:58` `OnceLock` (✓) | CONFIRMED |
| `value_from_ref` lazy cursor (JSON proof model) | αE C0:117; αC §1 | `json/value.rs:143` `value_from_ref<'doc,'input>` (✓) | CONFIRMED |
| `scan_structurals` + scalar twin (CH4 ref) | αE C2:175–176 | `json/scan.rs:22` `scan_structurals`, `:32` `scan_structurals_scalar` (✓) | CONFIRMED |
| skinny tape substrate location | αE C1:120 vs αA §6/αD V6 | **αE correct**: `skinny/crates/runtime/src/tape/{mod.rs,assembler.rs}` exist (`TapeBuilder`/`Tape`/`ValueRef`/`PayloadArena`, mod.rs:10,94,38). **αA/αD WRONG**: cite `crates/core/src/runtime/tape/` + `TapeStructBuilder`/`TapeCursor`/`TapeRef` — these are the **TOTALITY tree**; `grep TapeStructBuilder skinny/` = **EMPTY**. | SPLIT (αE ✓ / αA,αD ✗) |
| `StructLayout` is the skinny generality vehicle | αC §2b; αD O2 | `grep StructLayout skinny/crates/` = **EMPTY**. `StructLayout` lives ONLY in the core/totality tree (`crates/core/tests/`). Lock 2 (LOCKS.md:160) **retired the term StructLayout**. | DEFECT |
| LOCKS citations (Lock 1:75, Lock 8:595, Lock 14:603/349, Lock 16:607, FactStream:585, census:380) | αC throughout | All verified EXACT against `restart/locks/LOCKS.md` (✓) | CONFIRMED |

---

## §2 — The load-bearing CH7 verdict (read before the per-section dispositions)

The contrivance discipline in αC (the pre-block digest) is **excellent and largely
sound** — it correctly classifies fact-stream / broadcast / FNV / fixture / x86 as
PERMANENT-PRE-BLOCK and the eager-value-tree / StructLayout-the-data as
ADMIT-UNDER-DIFFERENT-FRAMING, with verified LOCKS anchors. The re-open tests are
falsifiable. That is the spine of overfit prevention and it holds.

But there is **one structural contrivance-risk the shortlist does not fully discharge**,
and it is precisely my lens's mandate:

> **αD O1 (and by inheritance αE C1) states the CSS tape wiring "MUST thread the
> CSS-specific routing the eager builder encodes as DATA (declarations/selectors/
> aggregate/numeric/functions/colors rule-id sets, 0/1/N value-list collapse, selector
> span-vs-single, hex packing, color component order) — none recoverable from StructLayout
> alone" (αD:79).**

This is the seam where CSS-special-casing re-enters under the banner of "data". Lock 14
(LOCKS.md:349) is unambiguous: *"Per-grammar deviations (CSS L4 colour-function emit …)
are encoded in the grammar metadata + source, NOT in branching code"* and *"The substrate
carries ZERO grammar-specific code."* The 594-line `css_l4.toml` (vs 34-line `json.toml`)
is the **measured fingerprint of exactly this asymmetry** — and it is a TOTALITY artefact
(`output_dir = crates/core/src/runtime/css_l4`), not even the skinny benched surface. If
SK-V17 wires CSS onto the tape by porting "hex packing / color component order / rule-id
sets" as TOML data or `match rule_id` arms, it has **not generalized** — it has relocated
the overfit from `generated.rs` into the projection catalogue. The αE shortlist's Lock-14
clause (§3) asserts generality "by construction" but **does not state the falsifiable test
that the css_l4.toml asymmetry shrinks toward json.toml** (or that the CSS deviations are
derived from the `.bbnf` grammar source, not hand-curated). That is the missing pruning gate.

This is REVISE-grade, not REJECT-grade: the shortlist's *intent* is correct (αE §3 names
Lock 14, αD O5 names "dissolving the hand-curated TOML catalogue"), but the *falsifiability
gate* that would catch a relocated-overfit is absent. The fix is concrete (§3, C1/O5 below).

---

## §3 — Per-section dispositions

### alphaA (results extraction)

- **§0–§5 (CSS standing, cold bench, 8-field equality, 20x checkpoint, W6 ledger): ACCEPT.**
  The 8-field equality (10136/9561/9561/20043) is the load-bearing anti-flatten /
  anti-contrivance gate and αA correctly frames it as the "structural-honesty gate proving
  Track 1 produces a real typed CSSOM … NOT a flattened summary, NOT a constant" (§3:135). The
  20x checkpoint provenance (`8153236e8`, generic, "no CSS special-case", §4:161) is
  verified-grammar-neutral and correctly carried as foundation. No contrivance.

- **§6 V6 (flat-tape substrate) + §8 citation ledger: REVISE.** Path defect.
  αA cites the banked substrate as `crates/core/src/runtime/tape/{record,arena,cursor,mod}.rs`
  with `TapeStructBuilder`/`TapeCursor`/`TapeRef` — that is the **TOTALITY tree**. `grep
  TapeStructBuilder skinny/` is EMPTY; the **benched skinny** substrate is
  `skinny/crates/runtime/src/tape/{mod.rs,assembler.rs}` with `TapeBuilder`/`Tape`/`ValueRef`/
  `PayloadArena`. **Fix:** re-cite §6 V6 against the skinny tree (as αE C1:120 already does
  correctly), and mark the core-tree `TapeStructBuilder` symbols as totality-only. Without
  this, the SK-V17 goalset would target a substrate that does not exist on the benched
  surface — a CH1 reject under αE's own warning (αE:50).

### alphaB (competitor deltas)

- **§0–§4 (plane taxonomy, lightningcss as fair bar, cssparser plane-mismatch): ACCEPT.**
  The plane discipline is exactly the anti-contrivance posture for CSS (the analogue of the
  SK-V6 sonic-rs `utf8_lossy` finding). Naming cssparser the parity oracle / NOT the speed
  bar (§3) prevents the inverse contrivance (claiming a token-scanner beat as SOTA). Sound.

- **§2 per-corpus endpoint mapping (animate↔164, tailwind↔51): REVISE.** αB itself flags
  this as "the one inferential step" (§2:118, §verification:242) — the per-corpus Track-1
  split and per-corpus lightningcss split are NOT published; the endpoint-to-corpus mapping
  is *reasoned from corpus character*, not measured. This is honestly disclosed, so it is
  not a contrivance — but a Pass-Alpha goalset that targets "animate crosses first" must not
  bake an *inferred* per-corpus number into a §4.1 close condition. **Fix:** the αF goalset
  must state these as UNMEASURED-PENDING (the N≥50 harness produces them) and forbid any
  wave exit-gate keyed to the inferred 164/51 endpoints until the harness emits them.

### alphaC (REDRESS digest) — the contrivance core; my lens's primary subject

- **§1 (AZ-IV eager-value 118x), §2 (StructRegistry/Arena indirection), §3 (fact-stream
  String), §4 (24-row broadcast), §5 (FNV/fixture), §6 (x86): ACCEPT on classification +
  re-open tests.** This is the strongest artefact for overfit-prevention. Each pre-block has
  a falsifiable re-open test and a verified LOCKS anchor (all line refs checked EXACT in §1
  above). The two-bucket split (PERMANENT vs ADMIT-UNDER-FRAMING) is the correct discipline:
  it admits the *intent* (typed/rich/retained) and pre-blocks the *carrier*
  (eager/allocating/serialized). The §4 broadcast pre-block ("There is no different-framing
  admission … per-corpus independent timing", §4:185) and the §5 fixture pre-block ("scratch
  must size from `input.len()` … grammar-generally", §5:224) are exactly the anti-contrivance
  gates my lens demands. No fact-stream/FNV/broadcast/fixture re-entry path is left open.

- **§5 number defect: REVISE.** "187 fixture-named parse fns" is wrong — `grep -c "fn
  parse_"` = **148** (174 total `fn`). Same defect propagated from αE. **Fix:** cite the
  measured count (148 parse_ fns / 4941 lines) or drop the specific integer. Does not change
  the verdict (the file IS textbook overfit), but an uncited number is CH1-rejectable.

- **§2b (StructLayout the layout-itself = ADMIT-UNDER-FRAMING): REVISE — generality-vehicle
  defect.** αC leans the entire generality argument on `StructLayout { rule_id, kind, fields }`
  + `FieldSource` (§2b:97) as "the load-bearing generality vehicle … built once per rule."
  **But `StructLayout` does not exist in skinny** (`grep StructLayout skinny/crates/` EMPTY)
  AND **Lock 2 (LOCKS.md:160) explicitly retired the term StructLayout** ("replaces *type
  projection / … / StructLayout / …* everywhere; the IR record is `Layout`"). αC is citing a
  totality-tree, lock-retired construct as the skinny generality vehicle. This is the same
  core-tree-vs-skinny confusion as αA §6. **Fix:** re-frame §2b against the skinny generality
  surface that ACTUALLY exists — the projection TOML + the codegen template (`runtime_generator.rs`)
  — and state the admission as "the generator reads grammar-derived layout facts ONCE per
  rule, not a per-leaf registry lookup", using the canonical `Layout`/`LayoutFacts` name
  (Lock 2), not retired `StructLayout`.

### alphaD (validated/invalidated ledger)

- **§1 (validated wins), §2 (invalidated: I1 micro-opt refuted, I5 AZ-IV pre-block, I7
  single-sample): ACCEPT.** The "per-lever micro-opt does not move the floor" finding (I1,
  refuted by measurement: 3.093→3.178) is correctly banked as anti-contrivance — it forecloses
  the temptation to claim a noise-level lift as progress. Pre-block families (§2:59–64) carry
  fact-stream/broadcast/fixture/FNV/x86 verbatim. Sound.

- **§3 O1 (tape wiring "thread CSS-specific routing as DATA"): REVISE — the central
  CH7 finding (see §2 above).** The phrase *"thread the CSS-specific routing … as DATA
  (… hex packing, color component order) — none recoverable from StructLayout alone"*
  (O1:79) is the seam where overfit can re-enter relabeled as data. Per Lock 14
  (LOCKS.md:349), CSS deviations must be "encoded in the grammar metadata + source, NOT in
  branching code", and the 594-vs-34-line TOML asymmetry is the live fingerprint of this risk.
  **Fix:** O1 must add a falsifiable pruning gate — *"the CSS routing data MUST be derived
  from the `.bbnf` grammar source (not hand-curated per-rule-id tables); the gate measures
  the `css_l4.toml` projection-LOC and FAILS the wave if CSS requires per-rule-id `match`
  arms or hand-curated capacity/packing constants that JSON does not. Generality is proven by
  the Sheets/BBNF-self witness (Lock 14 census, LOCKS.md:380), not asserted."* Also fix the
  `StructLayout`/`crates/core/...` path citations (O1/O2 cite `css_l4/builder.rs`, `CssArena`
  in the core tree as the thing being replaced — verify those are the benched surface or
  re-cite to skinny `css_l4_*/generated.rs`).

- **§3 O5 (codegen unification + overfit removal): ACCEPT with one strengthening (REVISE-lite).**
  O5 correctly names the dissolution target: "retire `json_templates/` special-case;
  quarantine `emit_fact_stream`/`CSS_GENERATED_RS` to diagnostic-only; derive CSS typed
  variants from grammar→projections, dissolving the hand-curated TOML catalogue" (O5:83). This
  IS the right anti-overfit move and directly answers my lens. **Strengthen:** make the
  TOML-LOC convergence an explicit telemetry-bound exit gate ("css_l4.toml projection-LOC
  trends toward json.toml's per-rule density; any residual CSS-only TOML section names the
  grammar-source rule it derives from"). Without that gate O5 is a goal, not a falsifiable
  close condition.

### alphaE (candidate shortlist) — the most important artefact for SK-V17

- **§0 ground-truth anchors + architecture-doc translation correction (lines 37–51): ACCEPT,
  and commend.** αE is the ONLY artefact that catches the core-tree-vs-skinny path confusion
  ("there is no `StructLayout` … no `OpenFrame` … no `CssArena` in skinny … CH1 will reject
  any goalset citing core-tree paths as the benched surface", :40–51). This is the correct
  pruning of a documentation contrivance. αA §6 and αC §2b/αD O1 should have inherited this
  correction and did not (flagged above).

- **C0 (de-fact-stream typed Track 1): ACCEPT.** Correctly identifies that the benched
  "Track 1" is a `String` (`track1_facts -> Result<String,String>`, verified
  `nonjson_css_l4.rs:596`) and that until it emits a typed summary there is NO measurable CSS
  close condition. The pre-blocks (C0:102–109) forbid re-introducing emit_fact_stream/fnv64 as
  admission, StructRegistry indirection, eager-by-default, and **"hand-curate a per-grammar
  type catalogue (the 594-line css_l4.toml asymmetry) — derive from the grammar/BackendRule
  shape"** (C0:108). That last clause is exactly my lens's requirement, stated correctly.
  **One number fix:** C0 inherits the "187 fixture fns" error (→ 148); REVISE-lite the integer.

- **C1 (tape wiring + lazy cursor): REVISE.** The candidate is sound and the falsifiability
  gate (≥30 Mbps, 8-field equality EXACT, N≥50 median) is honest. But C1 inherits αD O1's
  "thread CSS-specific routing" risk **without C0's pruning clause attached to C1's gate**.
  C1's pre-blocks (C1:151–155) name "no columnar SoA / no per-leaf Box" but do NOT name "no
  relocated-overfit into the projection data". **Fix:** add to C1's gate the same derive-from-
  grammar-source pruning test as O5/O1 above, so the wave cannot pass by porting hex-packing /
  color-order tables as data. C1 must demonstrably also serve JSON (already on tape) AND a
  Sheets/BBNF-self witness per Lock 14 census — αE §3 asserts this "by construction" but C1's
  gate does not list the witness as an exit condition.

- **C2 (NEON pre-scan via PrimitiveKernels): ACCEPT.** This is the model anti-contrivance
  candidate: it REUSES the existing checkasm-gated `select_classifier`/`PrimitiveKernels`
  surface (verified dispatch.rs:42,50,58), keys on the grammar's delimiter/alphabet set
  (NOT CSS literals), produces ONLY a `Vec<u32>` index (speed from scan, never from dropping
  structure), carries scalar-ref (`scan_structurals_scalar`, verified json/scan.rs:32) +
  checkasm + same-wave consumer, and explicitly checks `lo6_table_admissible` so that "if it
  collides, the scalar path is the honest answer (NOT a CSS special-case)" (C2:186). This is
  genuine generalization. The `digit_mac` orphan + i8mm-clean claims verified. **Accept.**

- **C3 (commit-by-construction spine): ACCEPT.** The non-deposition property is asserted to be
  a *codegen-proven structural property, not heuristic* (C3:238), and the equality count is
  the catch-net. The pre-block "no speculative-rollback re-introduction disguised as a fast
  path" (C3:257) is correct. Grammar-general (emitter, not CSS patch). Sound.

- **C4 (tailwind: udot + i8mm): ACCEPT.** Correctly retires the verified `digit_mac` orphan
  (asm! present, never called — confirmed), gates the new i8mm kernel with scalar-ref +
  checkasm (Lock 16), forbids `is_aarch64_feature_detected!` in the hot loop, forbids x86/SVE,
  and — critically for my lens — forbids "fixture/per-corpus hand-tuned capacity constants …
  tuning is a generic delimiter-density heuristic, not a tailwind literal" (C4:303). The
  no-paper-close honest-residual posture (C4:289) is the correct CH6/CH7 closeout. **Accept.**
  (Minor: digit_mac line cites drift ~1 line; non-blocking.)

- **§2 dependency order, §3 cross-cutting discipline, §4 escalation: ACCEPT.** §3 names all
  four anti-contrivance gates (N≥50 median, plane disclosure, 8-field EXACT, Lock-14
  grammar-neutral, no-paper-close). The escalation note (§4) correctly makes the
  borrowed-slice-vs-lazy decision an ENTRY GATE. Sound.

---

## §4 — The contrivance ledger (my lens's bottom line)

| Contrivance vector | Re-entry blocked? | By which artefact clause | CH7 verdict |
|---|---|---|---|
| Fact-stream String as admitted product | YES (permanent pre-block) | αC §3; αE C0:102; αD §2 | clean |
| 24-row broadcast (one tuple ×N rows) | YES (permanent, no re-frame) | αC §4; αB §3 plane-split; N≥50 median | clean |
| FNV closed-enum arbiter | YES (bench-quarantine only) | αC §5a; αD §4 | clean |
| Fixture-named parse fns / per-corpus capacity consts | YES (input.len()-sized, grammar-general) | αC §5b; αE C4:303; αD O5 | clean (number off: 148 not 187) |
| x86 / AVX / SVE | YES (out-of-scope, diagnostic-only) | αC §6; αE C2/C4 pre-blocks | clean |
| **CSS-special-casing relocated into projection DATA** (hex packing / color order / rule-id sets as TOML or `match rule_id`) | **PARTIAL** | αE C0:108 + αD O5:83 name the intent; **but no falsifiable pruning gate on C1/O1** | **REVISE** |
| Inferred per-corpus numbers baked into goalset | PARTIAL (disclosed, not yet gated) | αB §2:118 | REVISE |
| Citing core-tree (StructLayout / TapeStructBuilder) as the benched surface | PARTIAL (αE caught it; αA/αC/αD did not) | αE §0:40–51 | REVISE (αA §6, αC §2b, αD O1/O2) |

The headline: **no fixture/FNV/broadcast/fact-stream re-entry survives** — those gates are
airtight. The **one open contrivance risk** is relocated CSS-special-casing into the
projection catalogue/data, which the shortlist *names as a danger* but does *not yet bind
with a falsifiable pruning gate*. That is the single substantive REVISE my lens produces.

---

## §5 — Required revisions (concrete, for αF / V2)

1. **Add a TOML-convergence / grammar-source-derivation pruning gate** to C1 + O1 + O5: the
   CSS tape-wiring wave FAILS if CSS requires per-rule-id `match` arms or hand-curated
   capacity/packing/color-order constants that JSON does not, OR if `css_l4.toml`
   projection-LOC does not trend toward json.toml's per-rule density. Every residual CSS-only
   projection entry must name the `.bbnf` grammar-source rule it derives from. (Lock 14,
   LOCKS.md:349.)
2. **Bind the Lock-14 census as a C1 exit condition**: C1/C2/C3 must each carry a Sheets OR
   BBNF-self witness (LOCKS.md:380 — "With only one of Sheets or BBNF-self, the claim is
   scoped to the witnessed grammars"). αE §3 asserts generality "by construction"; make it a
   measured exit gate, not an assertion.
3. **Fix the path citations**: αA §6 V6, αC §2b, αD O1/O2 must cite the **skinny** tape
   (`skinny/crates/runtime/src/tape/`, `TapeBuilder`/`Tape`/`ValueRef`) and the canonical
   `Layout`/`LayoutFacts` term (Lock 2, LOCKS.md:160), not core-tree `StructLayout`/
   `TapeStructBuilder`. αE C0:40–51 is the template.
4. **Fix the fixture-fn integer**: 187 → **148** parse_ fns (4941 lines) across αE C0, αC §5,
   αD O5 — or drop the specific count.
5. **De-bake the inferred per-corpus endpoints**: αB §2's animate↔164 / tailwind↔51 mapping
   must be marked UNMEASURED-PENDING in the αF goalset; no wave exit-gate may key on them
   until the N≥50 harness emits the per-corpus split.

---

## §6 — Disposition summary

13 reviewable sections across alphaA–E (αF/SYNTHESIS/HANDOFF unauthored, out of V1 scope).

- **ACCEPT (8):** αA §0–§5; αB §0–§4 (plane taxonomy); αC §1/§3/§4/§5/§6 (pre-block core);
  αD §1/§2; αE §0; αE C0; αE C2; αE C3; αE C4; αE §2–§4. (Counted as the major accepted blocks.)
- **REVISE (5):** αA §6 (path defect); αB §2 (inferred endpoints); αC §2b (StructLayout
  generality-vehicle defect) + §5 number; αD §3 O1 (relocated-overfit pruning gate) + O5
  strengthening; αE C1 (inherit pruning gate) + C0 number.
- **REJECT (0):** No section proposes a fixture/FNV/broadcast/fact-stream re-entry, an x86
  admission, or a flattened-AST contrivance. The discipline holds; the defects are
  citation/number/gate-completeness, all REVISE-grade.

The path is **genuinely generalizing**, not CSS-special-cased — the C2 NEON reuse and the
8-field equality gate prove it — **provided** revision §5.1 (the TOML-convergence pruning
gate) lands so that CSS-special-casing cannot re-enter relabeled as projection data. That is
the load-bearing condition for my ACCEPT of the shortlist as a whole.

---

## §7 — Carry-forward to V2 / αF (binding)

αF (`alphaF-contract-draft.md`), `SYNTHESIS.md`, `HANDOFF.md` are unauthored at V1. When
authored they MUST carry: (a) the §5 pruning gate verbatim in the §4.1 close conditions; (b)
the Lock-14 witness as a per-wave exit gate; (c) skinny (not core-tree) path citations; (d)
the corrected fixture-fn count; (e) UNMEASURED-PENDING marking on inferred per-corpus numbers.
αF inherits an implicit REVISE until these land.
