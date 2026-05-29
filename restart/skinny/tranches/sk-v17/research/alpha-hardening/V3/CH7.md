# CH7 — OVERFIT-PRUNE (Pass Alpha SK-V17, cycle V3)

Lens: CH7 OVERFIT-PRUNE. Focus: **no contrivance** — no fixture/FNV/broadcast/fact-stream
re-entry; CSS variants derived from grammar projections, not hand-curated; the path is
**genuinely generalized, not CSS-special-cased**. Adversarial review of
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA..alphaE}.md` + `SYNTHESIS.md` +
`HANDOFF.md` per PASS-ALPHA §3 + ORCHESTRATOR §3W/§3Z.

Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25` (verified
`git rev-parse HEAD` = `1c5bd7a25250640f3a6fcfc00abed11f556f674f`). Every disposition
carries a path:line + the measured/grepped fact it rests on.

**Cycle context.** V1 CH7 produced ONE substantive REVISE (relocated-CSS-overfit pruning
gate missing a falsifiability test) + four citation/number/scope REVISEs; all five folded
clean into V2 (verified by V2 CH7 §2). V2 CH7 produced exactly TWO REVISEs — both the SAME
residual seam: the `css_l4.toml`-LOC-convergence metric (a TOTALITY artefact) leaked into an
SK-V17 close/scan gate in SYNTHESIS:101 + HANDOFF:122 without the totality-fold caveat the
source research artefacts (αC §0, αD O5) already carried. This V3 review (a) verifies the two
V2 REVISEs folded, and (b) re-scans the full αA-E + SYNTHESIS + HANDOFF surface for any new
or surviving contrivance vector.

---

## §1 — Verification battery (re-greped at HEAD `1c5bd7a25`, this lens)

| Claim under test | Artefact cite | Ground truth (verified this cycle) | Verdict |
|---|---|---|---|
| `StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`/`begin_compound` ABSENT from skinny | SYNTH:30; HANDOFF:12-13; αD §0; αE C0:100 | `grep -rln` across `skinny/crates/` = **EMPTY** (✓) | CONFIRMED |
| fixture parse fns = **148** (not stale 187) | αC §5:276; αD O5:154; αE 0:76 | `grep -c "fn parse_" generated_real_typed.rs` = **148** (✓) | CONFIRMED |
| `W5C_REQUEST_FACT_PROFILES` const exists at lib.rs:336, iterated :567/:611, selected :299 | αC §0/§3; αD O5; αE C0:136; SYNTH:51; HANDOFF:19 | `grep -n` lib.rs: decl :336, iterated :567,:611, selected :299 (✓) | CONFIRMED |
| 7 per-grammar `RequestFactsProfile` literals carry `emitter: RuntimeEmitterKind::RequestFacts` in regen_css.rs | αE V3-changelog:18-20; SYNTH:172; αC §3:174 | `grep -n "emitter:" regen_css.rs` = :45,:63,:81,:99,:117,:135,:153 = **7** (✓) | CONFIRMED |
| broadcast rows = **24** (not 6) | αA §2:125-126; SYNTH:140; HANDOFF:49 | `grep -c '^| css_l4/.*/direct_to_struct/main ' RESULTS.md` = **24** (✓) | CONFIRMED (V2 CH1-R1 reconciliation folded) |
| benched CSS Track 1 = `Result<String,String>` fact-stream at :596 | αA §0; αC §3:168; αD §0:38; αE C0:134; SYNTH:41; HANDOFF:17 | `nonjson_css_l4.rs:596` `pub fn track1_facts(input:&str)->Result<String,String>` (✓) | CONFIRMED |
| `assert_lightningcss_strict_equality` defined :776, call sites :1057,:3460 | SYNTH:110 | `grep -n` = def :776, :1057, :3460 (✓ exact) | CONFIRMED (V2 CH1-R2 citation folded) |
| `sheets_witness` = 25 LOC stub, no `.bbnf`/parser/`BackendRule`; BBNF-self absent | αD §0:56-60; αE 0:82; SYNTH §0.4:240; HANDOFF:88-90 | `event_grammar_witness.rs` 24 + `mod.rs` 1 = **25 LOC** (✓) | CONFIRMED |
| i8mm grep-clean-absent from skinny | αD O3:152; αE 0:81; SYNTH:57 | `grep -rln i8mm skinny/crates/` = **EMPTY** (✓) | CONFIRMED |
| `digit_mac` udot orphan never called in skinny runtime | αD O3:152; αE C4a:381; SYNTH:54-56 | `grep -rn parse_4_digits skinny/crates/runtime/` = **EMPTY** (orphan ✓) | CONFIRMED |
| `select_classifier`/`PrimitiveKernels`/`OnceLock`/`lo6_table_admissible` grammar-general entry | αE C2:273; SYNTH:53; αD O3:152 | `dispatch.rs:42` `select_classifier`, `:50` `PrimitiveKernels`, `:59` `OnceLock`, `:101` `lo6_table_admissible` (✓) | CONFIRMED |
| `css_l4.toml` 594 lines is a repo-root TOTALITY (`xtask/runtime-projections/`) artefact, grep-absent from skinny | αC §0:34; αD §0:39 + O5:154; SYNTH:105; HANDOFF:140 | `find skinny -path '*projection*' -name '*.toml'` = **EMPTY** (✓) | CONFIRMED |

Every load-bearing fact this lens depends on is grep-verified true at HEAD. No uncited
number survives into V3.

---

## §2 — The V2 → V3 fold ledger (did the two V2 REVISEs land?)

V2 CH7 produced exactly two required revisions (§5 of V2 CH7), both descendants of one seam:
a TOTALITY-tree `css_l4.toml`-LOC metric leaked into an SK-V17 close/scan gate. Each is
verified folded:

| V2 revision | V3 status | Evidence |
|---|---|---|
| **1. SYNTHESIS.md §0.1 Layout-driven-projection gate (line 101): demote the `css_l4.toml`-trends-toward-`json.toml` clause from an SK-V17 close condition to an explicitly-labelled SK-V18 totality-fold metric (informational)** | **FOLDED** | SYNTHESIS:105 now reads "(The 594-vs-34-line `css_l4.toml`-vs-`json.toml` asymmetry is a TOTALITY artefact — `css_l4.toml` is grep-clean-absent from `skinny/`, it lives only at the repo-root totality tree — so its LOC convergence is an SK-V18 totality-fold metric, INFORMATIONAL only, NOT an SK-V17 close gate; gating an SK-V17 close on a non-benched totality file would be the wrong-tree dishonesty this contract REJECTs.)" The SK-V17 close gate is now keyed strictly to the four skinny-greppable tests (W5C retired, no per-rule-id match arms, every residual CSS routing entry names its `.bbnf` rule, regen profile array trends toward the JSON emitter shape). SYNTHESIS:9-10 records the fold in the V3 changelog ("the `css_l4.toml`-is-totality SK-V18-fold demotion"). |
| **2. HANDOFF.md CH7-scope paragraph (line 122): reword the `css_l4.toml` LOC scan-scope item to the skinny-greppable scope, note `css_l4.toml` LOC convergence as an SK-V18 totality-fold metric only** | **FOLDED** | HANDOFF:133-142 now lists the CH7 scan scope as exactly the four skinny-greppable tests (a) W5C retired/not-relocated, (b) no per-rule-id match arms / hand-curated packing-color constants JSON does not need, (c) every residual CSS routing entry names its `.bbnf` rule, (d) the CSS regen profile array (`regen_css.rs:45-153`) trends toward the JSON emitter shape — then explicitly: "The `css_l4.toml` LOC convergence is NOT a CH7 scan gate — `css_l4.toml` is a TOTALITY artefact grep-clean-absent from `skinny/`, so its `json.toml`-parity trend is an SK-V18 totality-fold metric only, noted not gated." HANDOFF:7 records the fold in the V3 changelog. |

**Both V2 REVISEs are folded with verified evidence.** The fold is exactly the two-line
demotion V2 CH7 §5 prescribed — neither widened nor narrowed. No orphan REVISE carries
forward. The αE artefact, separately, already folded its own two V2 dispositions (CH2-V2-F1
sheets_witness-non-dischargeable + the C1 owner-path `resolve_builder_routes` strike) per its
V3 changelog (αE:12-31), both verified true at HEAD (sheets_witness 25 LOC; the 7
RequestFactsProfile literals are the real seam-flip surface).

---

## §3 — Per-section dispositions (V3)

### alphaA (results extraction) — ACCEPT

The 24-row broadcast is correctly framed as a **PERMANENT-PRE-BLOCK, not a baseline**
(§2:125-136: `not_admitted:SK-V15-W0-broadcast-diagnostic`/`AUDIT-FALSIFIED`, grep-verified
24, "must NOT be a baseline"). The "zero ADMITTED typed CSS rows" framing is the honest
anti-contrivance position — there is no SK-V16 per-corpus typed-CSS row to delta against, so
no fabricated endpoint is admitted. The "no CSS special-case" claim (§2:218) is cited to the
O(1)-checkpoint report (`sk-v16-w6p2-o1-checkpoint-report.md:25-37`), which is the
grammar-neutral banked win, not a CSS-specific patch. The benched CSS Track 1 is correctly
disclosed as the `RuntimeEmitterKind::RequestFacts` fact-stream String (§0; §1:321;
nonjson_css_l4.rs:596), not a typed product. No contrivance.

### alphaB (competitor deltas) — ACCEPT

Every per-corpus endpoint cell is marked `[INF]` **inline** (§2:161-164: animate↔164,
bootstrap↔70, material↔60, tailwind↔51 all carry `[INF]`/`[RNG][INF assign]`); the
endpoint-to-corpus assignment is UNMEASURED-PENDING (§2:178) and §6 forbids any SK-V17 wave
exit-gate keying on an inferred endpoint until the N≥50 harness emits the split. Only the
corpus-aggregate ~14×/~36× rows are cited as measured. The CH1-R1 broadcast-count
reconciliation (6→24) is folded (§:28-33). The cssparser-is-a-flaw-probe plane discipline is
the CSS analogue of the SK-V6 `utf8_lossy` finding and prevents a token-scan win from masking
as a >SOTA claim. No fabricated per-corpus measurement is admitted. No contrivance.

### alphaC (REDRESS digest) — ACCEPT

This remains the strongest overfit-prevention artefact. The skinny overfit fingerprint is
correctly localized: `W5C_REQUEST_FACT_PROFILES` + the 7 `RequestFacts` registrations + the
148 fixture parse fns (§0:34, §5:276), all skinny-greppable. The **retirement clause**
(§3:195-198: CH3/CH5 fail if the 7 `RequestFacts` registrations or `W5C_REQUEST_FACT_PROFILES`
still drive an admitted row) is the load-bearing anti-relocation gate. §2b:147-148 +
table-row 2a/2b/3 (§:347-349) forbid any new hand-coded per-grammar profile/route table
parallel to `W5C_REQUEST_FACT_PROFILES` (relocated overfit, Lock 14, LOCKS.md:380-387). The
`css_l4.toml` is correctly classified as a core-tree artefact absent from skinny (§0:34). The
Lock 2 name-retirement (`StructLayout`→`Layout`/`LayoutFacts`, LOCKS.md:160) is cited
correctly (§2b). No fact-stream/FNV/broadcast/fixture re-entry survives. Airtight.

### alphaD (validated/invalidated ledger) — ACCEPT

The Anti-relabel pruning gate is present and falsifiable (O1:150: "wave FAILS if CSS needs
match arms / hand-curated packing constants JSON does not, OR if the CSS regen profile array
does not trend toward the JSON shape"). The `css_l4.toml` is correctly tagged a
**TOTALITY-tree artifact** (§0:39, O5:154 "(TOTALITY fold)"), not a skinny owner path —
this is the SOURCE artefact that V2 CH7 cited as already-correct, and it remains correct. The
148→fixture-overfit retire-list is in O5. The JSON-witnessed-only generality downgrade is
honest (§0:56-64: "tape-generality demonstrated today is JSON-witnessed only"; sheets a stub;
bbnf-self absent). O2:151 correctly strikes the sheets_witness projection target as
structurally non-dischargeable. No contrivance.

### alphaE (candidate shortlist) — ACCEPT

The V3 changelog (αE:12-31) documents the exact V2 fold: C1 owner path struck the fabricated
`resolve_builder_routes` symbol and named the real 7-literal seam-flip surface; the
sheets_witness "emit a ValueRef view" clause is struck (option-b′ applied) and the exercised
projection riders are **JSON + CSS only** with a real Sheets rider demoted to SK-V18. C2
remains the model anti-contrivance candidate: it REUSES the checkasm-gated
`select_classifier`/`PrimitiveKernels` surface, keys on the grammar's alphabet (NOT CSS
literals), produces ONLY a `Vec<u32>` index (speed from scan, never from dropping structure),
and `lo6_table_admissible` is the honest scalar-fallback when the CSS alphabet collides mod
0x3f — genuine generalization. C4a admits unconditionally (orphan udot wiring, scalar +
checkasm present); C4b is GATED behind a Wave-5 re-profile proving the digit leaf is top-N
tailwind self-time (no orphan kernel). The relocated-overfit pruning gate (C1:259-263) is
present with the every-residual-CSS-entry-names-its-`.bbnf`-rule test. The per-corpus
endpoints are UNMEASURED-PENDING and the N≥50-median binding is present. No contrivance.

### SYNTHESIS.md (αF contract draft) — ACCEPT (V2 REVISE cleared)

The V2 single substantive REVISE is folded: SYNTHESIS:105 demotes the `css_l4.toml`-LOC
clause to an explicitly-labelled SK-V18 totality-fold metric (INFORMATIONAL, NOT an SK-V17
close gate) with the wrong-tree-dishonesty rationale stated inline; the SK-V17 close gate is
keyed strictly to the four skinny-greppable tests (§2 fold ledger). The benched-surface note
(:25-58) correctly localizes every citation to `skinny/crates/`; the `tape_activated`
telemetry column is honestly defined as "NOT satisfiable by a grep in `crates/core/`"
(Section 2:363). The pre-block §0.4 names `W5C_REQUEST_FACT_PROFILES` retirement and forbids
relocating its per-rule branching into projection DATA ("every residual CSS routing entry must
name the `.bbnf` rule it derives from", :200-201). The generality clause (§0.4:234-257) is
witness-honest: JSON+CSS only, sheets_witness non-dischargeable, four-grammar claim NOT proven
in SK-V17. The 24-row broadcast is pre-blocked and the gate rejects single-tuple broadcast
(Section 2:387). The `projection_generality_exercise` column rejects `sheets_witness` as a
value (:365). Strong contract; no residual seam.

### HANDOFF.md — ACCEPT (V2 REVISE cleared)

The V2 second REVISE is folded: HANDOFF:133-142 rewords the CH7 scan scope to exactly the
four skinny-greppable tests and explicitly notes `css_l4.toml` LOC convergence as a NOT-gated
SK-V18 totality-fold metric. The benched-substrate disclosure (:9-20) cites the skinny tape
correctly; the `W5C_REQUEST_FACT_PROFILES` retirement is a pre-block ("RETIRE, do not extend
or relocate into projection data — the overfit re-entry seam", :156-157); the no-second-
substrate Lock-1 clause (:167-170) and the `tape_activated`-not-by-`crates/core/`-grep gate
(:213-216) are present. The generality scope (:86-96) is JSON+CSS-only, sheets_witness
non-dischargeable. The 6-vs-24 broadcast-count reconciliation is folded (:49). No residual seam.

---

## §4 — The contrivance ledger (my lens's bottom line, V3)

| Contrivance vector | Re-entry blocked? | By which artefact clause | CH7 V3 verdict |
|---|---|---|---|
| Fact-stream String as admitted product | YES (permanent pre-block + retirement clause) | αC §3:195-198; αE C0; SYNTH §0.4:192-201; HANDOFF:153-157 | clean |
| 24-row broadcast (one tuple ×24 rows) | YES (permanent, no re-frame; gate rejects single-tuple) | αA §2:125-136; αB §:28-33; SYNTH Section 2:387; HANDOFF:158-160 | clean |
| FNV closed-enum arbiter | YES (bench-quarantine only) | αC §5a; SYNTH §0.4:205-208; HANDOFF:161 | clean |
| Fixture-named parse fns / per-corpus capacity consts (148-fn surface) | YES (named for retirement; tuning grammar-derived, not corpus literal) | αC §5:273-276; αD O5:154; αE C0/C4a/C4b; SYNTH §0.4:205-208 | clean (number 148, verified) |
| x86 / AVX / SVE | YES (out-of-scope, diagnostic-only) | αC §6; αE pre-blocks; SYNTH §0.4:209-210; HANDOFF:162-163 | clean |
| **CSS-special-casing relocated into projection DATA** (hex packing / color order / rule-id sets as TOML or `match rule_id`) | YES — falsifiable skinny-greppable pruning gate (every residual CSS entry names its `.bbnf` rule; wave FAILS if CSS needs match arms JSON does not) | αE C1:259-263; αD O1:150; SYNTH §0.1:105 + §0.4:200-201; HANDOFF:134-138 | clean |
| Inferred per-corpus numbers baked into goalset | YES (UNMEASURED-PENDING; no exit-gate may key on them) | αB §2/§6; SYNTH §0.5:274-280 | clean |
| Citing core-tree (`StructLayout`/`TapeStructBuilder`/`css_l4.toml`) as the benched surface | **YES (the V2 residual seam is now closed)** — SYNTH:105 + HANDOFF:140 both demote the `css_l4.toml`-LOC metric to an INFORMATIONAL SK-V18 totality-fold, NOT an SK-V17 close/scan gate | SYNTH §0.1; HANDOFF CH7-scope | **clean** (V2's two REVISEs folded) |

The headline: **every contrivance vector this lens owns is bound, and the single residual
seam from V2 (a TOTALITY-tree `css_l4.toml`-LOC metric leaked into an SK-V17 close/scan gate)
is now closed in BOTH SYNTHESIS:105 and HANDOFF:140 with the prescribed two-line demotion to
an informational SK-V18 fold metric.** The discipline holds end to end. The path is
genuinely generalizing, not CSS-special-cased — proven by the C2 NEON reuse
(`select_classifier`/alphabet, not CSS literals), the 8-field-equality anti-flatten gate, the
W5C-retirement-and-derive-from-grammar pruning gate, and the JSON-witnessed-only generality
honesty (no fabricated four-grammar claim). No fixture/FNV/broadcast/fact-stream re-entry; no
x86/SVE admission; no flattened-AST contrivance; no relocated-projection-data overfit; no
wrong-tree close gate.

---

## §5 — Required revisions (V3)

**NONE.** All five V1 CH7 revisions folded clean into V2 (verified by V2 CH7 §2). Both V2 CH7
revisions are folded clean into V3 (verified §2 above). No new contrivance vector surfaces on
the αA-E + SYNTHESIS + HANDOFF surface. The αE artefact independently folded its two V2
dispositions. Every load-bearing grep fact is true at HEAD `1c5bd7a25`.

---

## §6 — Disposition summary

**13 reviewable sections** across alphaA-E + SYNTHESIS + HANDOFF (αF is realised as the
SYNTHESIS + HANDOFF pair at the tranche root per PASS-ALPHA §6; there is no separate
`alphaF-contract-draft.md`, which this lens accepts as the αF deliverable).

- **ACCEPT (13):** αA (all §); αB (all §); αC (all §); αD (all §); αE (§0, C0, C1, C2, C3,
  C4a, C4b, §2-§4); SYNTHESIS (all sections — the V2 §0.1 `css_l4.toml` seam is folded);
  HANDOFF (all sections — the V2 CH7-scope `css_l4.toml` seam is folded).
- **REVISE (0):** Both V2 REVISEs (the SYNTHESIS §0.1 + HANDOFF CH7-scope `css_l4.toml`-LOC
  totality-tree leaks) are folded with verified evidence (§2). No orphan REVISE.
- **REJECT (0):** No section proposes a fixture/FNV/broadcast/fact-stream re-entry, an x86/SVE
  admission, a flattened-AST contrivance, or a relocated-projection-data overfit.

Counts: **ACCEPT 13 / REVISE 0 / REJECT 0** → 100% ACCEPT.

CH7 OVERFIT-PRUNE closes clean at V3. The two V2 REVISEs were a single residual seam (a
TOTALITY-tree `css_l4.toml`-LOC metric leaked into an SK-V17 close/scan gate); both are folded
with the exact prescribed two-line demotion to an informational SK-V18 totality-fold metric.
The path is genuinely generalized, not CSS-special-cased; no contrivance survives. This is the
second consecutive ≥95%-ACCEPT cycle from this lens's perspective (V2 was below bar only on the
two now-folded REVISEs); the §3Z convergence condition is met from CH7's standpoint.
