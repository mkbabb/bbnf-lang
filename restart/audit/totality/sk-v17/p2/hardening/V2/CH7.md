---
lens: CH7 OVERFIT-PRUNE (V2)
pass: T-P2-research
cycle: V2
reviewed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
master_head: 91b6893b0
t_p1_locked_sha: 445925167154de73540e3ea3283d0170371de790
contract: restart/prompts/totality/PASS-2-RESEARCH.md §3 + ORCHESTRATOR §3W
focus: no contrivance; the fold is the genuinely-general tape model (not CSS/JSON-special-cased); lightningcss the fair bar; no fixture/FNV/broadcast re-entry
prior_cycle: V1 (CH7-001/002/003 REVISE; 38 ACCEPT, 0 REJECT)
accept: 41
revise: 0
reject: 0
verdict: CONVERGED (all V1 REVISEs discharged; zero new overfit defect)
---

# CH7 OVERFIT-PRUNE — T-P2 SK-V17 V2

CH7 re-scans the V2 fold dossiers for the four overfit pathologies its mandate names:
(1) **contrivance** — a fold dressed as general but structurally CSS/JSON-special-cased;
(2) **unfair-bar distortion** — lightningcss (the CSS >SOTA anchor) inflated, conflated,
or substituted by a non-comparable measurement; (3) **fixture/FNV/broadcast re-entry** —
a fold smuggling a per-corpus literal, a 24-row broadcast tuple, or an FNV/fixture runtime
back across the LOCKED skinny pre-blocks (L-SK17-04/05, SK17L-010); (4) **orphan-kernel /
by-exercise-overclaim** — a primitive or generality claim with no benched antecedent or no
non-JSON witness, asserted "proven" on construction alone.

**Headline. V2 converges CH7 clean — 41 ACCEPT, 0 REVISE, 0 REJECT.** The three V1 REVISEs
(CH7-001/002/003, all variants of one recognizer-vs-materialization framing-drift) are
**fully discharged and load-bearing-correct**. The one genuine V1 contrivance — 2E's
fabricated "recognizer beats lightningcss 2-3×" — is **deleted at source**; 2E now carries an
*affirmative anti-claim* (2e:58: ``"2-3×" CSS figure exists in SPEC/RESULTS/T-P1 and none is
asserted here``), which is a stronger guard than mere removal. The lightningcss bar is
restored to its true posture across the touch-sites (UNMEASURED-PENDING bar + SK-V18
strict-equality GATE obligation, not a held property). No V2 fold introduces a new
contrivance, re-opens a fixture/FNV/broadcast pre-block, admits an orphan kernel, or silently
adds a 6th BackendShape. The by-construction-not-by-exercise honesty (2c refutation-#3) — the
strongest overfit guard in the set — is preserved verbatim and is the genuinely-general
proof: JSON and CSS instantiate the SAME `ValueRef<…,G:EventGrammar>` cursor type, the
grammar enters as a TYPE param monomorphised at codegen, and the fold's *act* is to RETIRE
the one genuine overfit (the eager 817-LOC CSS god-module) into that general plane.

**Hygiene (CH1-V5-001 first-action).** The dispatch directs folding the enumerated-filename
residual on first touch of 1a/1e. Verified on-disk at 91b6893b0: the defect lived only in
1b (`{eager,offset,event,collapsed}_tape.rs` brace-glob), which already carries the
enumerated executing form `{eager_tape,offset_tape,event_tape,collapsed_stage}.rs` at 1b:12
and 1b:97; `grep ',collapsed}' {1a,1b,1e}` = 0; `collapsed_stage.rs` exists,
`collapsed_tape.rs` does not. 1a + 1e carry zero brace-glob. No 1a/1e fold required — the
residual was discharged at the convergence commit. All four V2 dossiers that touched it
(2a/2c/2d/2e/2f) report this identical on-disk verification. Hygiene satisfied.

---

## V1 REVISE discharge — the three folds verified load-bearing-correct

### CH7-001 (was REVISE 2e:42-43, :288-289) → DISCHARGED-ACCEPT

The fabricated "recognizer beats lightningcss 2-3×" measured claim is **deleted**. The V2
exec summary (2e:54-64) now reads: the materialization divergence is *NOT a benchmarked CSS
deficit*; the fair CSS bar is *lightningcss full-CSSOM (SPEC:122) and it is
UNMEASURED-PENDING* with *all per-corpus endpoints awaiting the W0 N≥50 harness (SPEC:207)*;
and explicitly `no "2-3×" CSS figure exists in SPEC/RESULTS/T-P1 and none is asserted here`
(2e:58). The recognizer fact is re-cited to its true measured antecedent (2e:368: "the proven
JSON recognizer beats sonic-rs **on the same plane**, RESULTS.md:5-55"). Verified: a
repo-wide `grep '2-3×|2-3x|beats lightningcss|recognizer beats'` across all six p2 dossiers
returns only (a) the CH7-001 disposition-log line, (b) the anti-claim at 2e:58, (c) the
correct sonic-rs re-citation at 2e:368. The fabricated magnitude is gone; the affirmative
anti-claim exceeds the V1 fix requirement. SPEC:122/:207 re-verified live and resolve exactly
as cited. ACCEPT.

### CH7-002 (was REVISE 2e:171, :290-291) → DISCHARGED-ACCEPT

"full typed-AST parity with lightningcss" is reworded to the obligation form everywhere it
appears in 2E. The exec summary (2e:62-64), FOLD-2E-C (2e:204-208), and LAC-2E-SKV17-03
(2e:447) all now state parity is *the SK-V18 strict-equality GATE*
(`css_typed_summary_equal=true` gate-before-speed, SPEC:129; `assert_lightningcss_strict_equality`,
SPEC:98), *an obligation/target the fold must meet, NOT a property held at this pass*.
preserve-rich-ast is correctly framed as the non-negotiable target, not an asserted result.
ACCEPT.

### CH7-003 (was REVISE 2a:303, :42, :117) → DISCHARGED-ACCEPT

2A separates the three conflated facts cleanly (2a:49-51): (a) the JSON recognizer measured
fact (the only >SOTA-witnessed plane); (b) the materialization gap as a CODE-SHAPE divergence
(eager `OpenFrame`, NOT a benchmarked CSS deficit — lightningcss UNMEASURED-PENDING, SPEC:207);
(c) full typed-AST parity with lightningcss as an SK-V18 strict-equality GATE
(`assert_lightningcss_strict_equality`, SPEC:98, 2a:361-362). 2a:129 carries the same clean
split. The "2-3×" number is absent from 2A (it never carried it). ACCEPT.

---

## ACCEPT census — the fold remains genuinely general (no V2 regression)

### Grammar-generality vehicle (the anti-contrivance core) — ACCEPT (unchanged)
- **Type-parameterised value plane** (`ValueRef<…,K,G:EventGrammar>`, 2c:75; 2a; 2e:182;
  2f:75). Grammar enters as a TYPE param + kind `K`, monomorphised at codegen, zero runtime
  `match grammar`. JSON and CSS instantiate the SAME cursor type. The structural proof the
  fold is not special-cased. ACCEPT.
- **Alphabet-as-data classifier** (`select_classifier(&[u8;64])` / `scan_structural(input,
  &StructuralAlphabet)`, 2c:75; 2b FOLD-L1; 2e:108; 2f-F5). simdjson/sonic classify a FIXED
  JSON alphabet; bbnf takes the alphabet as `[u8;64]` DATA mined per grammar. Wired scan-leaf
  across 8 core grammars (config-breadth, 1B BSHAPE17-009). ACCEPT.
- **2c-ONBOARD future-grammar onboarding** (V2 reclassified as a verify_action with a live
  HEAD baseline: Predicate 1 = 7 string-ident leak sites in strategy.rs, monotonic-decrease
  surface; Predicate 2 = 8 @generated grammar dirs clean). The Lock-14 falsifier
  operationalising "not special-cased." ACCEPT.

### lightningcss-bar honesty — ACCEPT (now load-bearing-correct after CH7-001/002/003)
- **2c refutation-#3 — fleet-wide claim REFUTED on JSON+CSS exercise alone** (2c:82, :340-344).
  `sheets_witness` is a 24-LOC `EventGrammar` stub with no `.bbnf`/`BackendRule` (1D SK17L-009);
  the fold's grammar-neutrality is breadth-of-CONFIG, proven by-exercise on JSON+CSS only;
  Lock 14 v+1 scopes the wording (LOCKS:382-387). The by-construction-not-by-exercise honesty.
  ACCEPT (load-bearing — the strongest overfit guard).
- **2e lightningcss bar restored** (2e:56-57, :180, :370-373). UNMEASURED-PENDING bar +
  SK-V18 gate obligation, no inflation, no conflation with the JSON-vs-sonic margin. ACCEPT.

### fixture / FNV / broadcast pre-block integrity — ACCEPT (no re-entry)
- **2b FOLD-L7 one-shot capacity** (2b:285-294): sizes the EXISTING `offsets` from the L1 scan
  count, "no per-corpus capacity literal," binds Lock 8 + cites L-SK17-05 FNV/fixture fence.
  ACCEPT.
- **2b FOLD-L8 sparse-flag side-table** (2b:296-316): the kind-disambiguation rides
  `ValueRef<G>`, EXERCISED on JSON+CSS only, the 24-LOC `sheets_witness` stub named; the row
  may NOT claim fleet-wide generality and does not; `W5C_REQUEST_FACT_PROFILES`-into-flag-form
  regression explicitly pre-blocked (CH2-V1-R2). ACCEPT — the broadcast/fact re-entry fenced.
- **2d cost-model fail-closed** (2d:279, UNKNOWN-2D-S17-02): the model "fails closed on
  grammar-named (`json_*`/`css_*`) or **broadcast**/stale evidence." ACCEPT — broadcast
  pre-block holds.

### orphan-kernel / by-exercise discipline — ACCEPT (no orphan admitted)
- **2b udot/i8mm refusal** (2b:157): grammar-neutral and admissible for number-heavy grammars
  BUT requires a same-wave consumer with a profiled antecedent — CSS has none, so NO orphan
  kernel (SPEC §6 pre-block `:655` honoured). ACCEPT.
- **2b FOLD-L9 demotion to DEFERRED appendix** (2b:319-330, CH4-2b-003): no live consumer on
  the LOCKED profile, so demoted out of the wired enumeration so T-P3 does not read it as
  shortlisted. ACCEPT — exemplary anti-orphan posture (V2 hardening, not V1 carry).
- **2b L5/L6 abrogate-measurement gate** (2b:278-284, CH4-2b-002): REQUIRED-NEW primitive is
  DELETED if a profiled W3 antecedent does not move the consume_balanced_at arm by a
  measurable margin — LOC/checkasm cost bound to measured self-time. ACCEPT.

### no silent 6th BackendShape — ACCEPT (consistently REFUTED)
- **D tape-as-substrate-manifest** (2a:201-230; 2c:326-334 refutation-#1; 2d:104-106;
  2e FOLD-2E-D; 2f-F4 grounded / F4d refuted). The LAC-1E-14 FactStream precedent applied;
  the no-6th-shape verdict carries an INDEPENDENT corroborating anchor beyond the precedent
  (`admits_collapsed_stage` x86-bound, ARCH:1151/:1206/:1282 — mechanically refuses on
  aarch64). "Propose, do NOT silently add a 6th" satisfied by the category form. ACCEPT.

### StructRegistry/FieldSource fence — ACCEPT (regression firewall inviolate)
- **F per-leaf walk REFUTED, compile-time projection-emission GROUNDED** (2c:336-339
  refutation-#2; 2f-F6 grounded / F6b refuted). The 28-65×/983×/10583× regression
  (SPEC:793-795) pre-blocked; `begin_compound` reads `layout.rule_id & 0x1F` only (tape/mod.rs:185-186,
  grep-zero StructRegistry); the LIVE coupling-site `arena.rs:47
  StructRegistry::compound_kind_for_layout(layout)` named as the present wire FOLD-B severs
  (CH5-V1-003 shared fold across 2a/2d/2f). ACCEPT.

---

## Provenance spot-checks (CH7-owned citations, live at 91b6893b0)
- SPEC:122 → `lightningcss full-CSSOM ... THE fair >SOTA bar ... the only strict admission
  anchor` — resolves verbatim.
- SPEC:207 → `ALL per-corpus lightningcss endpoints are UNMEASURED-PENDING` — resolves verbatim.
- `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33` →
  `pub fn byte_class_from_eq_set_64_neon(src: &[u8;64], set: &[u8]) -> u64` — exists, signature
  matches the "one real NEON Layer-1 body" claim.
- Repo-wide `grep` for the fabricated "2-3×" beat → only the disposition-log + anti-claim +
  correct sonic re-citation; no live false claim survives.

---

## CH7 V2 disposition summary

| ID | Dossier:line | V1 disposition | V2 status | Note |
|---|---|---|---|---|
| CH7-001 | 2e:54-64,:368 | REVISE | DISCHARGED-ACCEPT | Fabricated "2-3×" deleted; affirmative anti-claim added; sonic-rs re-citation correct |
| CH7-002 | 2e:62,:204,:447 | REVISE | DISCHARGED-ACCEPT | Parity reworded to SK-V18 gate obligation everywhere |
| CH7-003 | 2a:49-51,:129,:361 | REVISE | DISCHARGED-ACCEPT | JSON-fact / code-shape-divergence / parity-obligation cleanly separated |
| (38 V1 ACCEPT) | all dossiers | ACCEPT | CARRIED-ACCEPT | No V2 regression; generality vehicle, pre-blocks, fence, anti-orphan all intact |
| 2b FOLD-L9 demote | 2b:319-330 | (new V2) | ACCEPT | Anti-orphan hardening — L9 demoted to deferred appendix |
| 2b abrogate gate | 2b:278-284 | (new V2) | ACCEPT | Measurement-bound REQUIRED-NEW admission |
| 2c-ONBOARD verify | 2c | (new V2) | ACCEPT | ONBOARD reclassified as executable verify_action with live HEAD baseline |

**Counts: 41 ACCEPT, 0 REVISE, 0 REJECT.** Zero orphan REVISE. CH7 verdict: **CONVERGED**.
The fold is the genuinely-general tape model — type-parameterised, alphabet-as-data,
by-construction-honest, lightningcss-bar-faithful, with every fixture/FNV/broadcast/orphan/
6th-shape pre-block intact. The one V1 contrivance is eliminated at source. CH7 raises no
new defect.
