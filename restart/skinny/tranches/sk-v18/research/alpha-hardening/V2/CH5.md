# CH5 — HIDDEN-COUPLING (V2) — SK-V18 Pass Alpha hardening

Lens: CH5 HIDDEN-COUPLING. Cycle V2. Adversarial review of the Pass-Alpha SK-V18
artefacts per `PASS-ALPHA §3` + `ORCHESTRATOR §3W`.

**Lens charter (binding for every disposition):** substrate-union Lock 1 preserved; **no
second substrate** introduced through the new generator / value-API surfaces; the
**shared value trait does not silently re-fork** the emitter or substrate; the **phantom
`<G>` is instantiated-or-deleted**, not animated into a new coupling. Hidden coupling = a
dependency the close conditions do NOT name that, if it exists, lets a refuted carrier
re-land or lets the generalization claim a unification it did not achieve.

## V2 method

CH5 V1 returned 16 ACCEPT / 7 REVISE / 0 REJECT, clustered on two root causes: (a) the
`ValueRef` two-axis precision (`K` real / `G` phantom) and (b) the shared-trait gates
false-greening by impl-count without a richness assertion. The V2 pass does three things:
(1) re-greps every load-bearing coupling surface at working HEAD to re-anchor ground
truth; (2) verifies each V1 REVISE is folded into the artefact it targeted; (3) runs a
FRESH adversarial sweep for NEW seams the V1 folds may have opened or left.

## Ground-truth re-verification (this review, at working HEAD)

Re-greped, not trusted:

- `skinny/crates/runtime/src/tape/mod.rs:175`
  `pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar>` —
  **four-slot, two defaulted axes confirmed** (the V1 distinction holds).
- **No second substrate:** `grep -rln 'StructLayout|TapeStructBuilder|TapeCursor'
  skinny/crates/` → **EMPTY workspace-wide**. The Lock-1 carriers the artefacts pre-block
  are absent in the skinny tree. (Nuance: `LOCKS.md:616` records `StructLayout` live across
  960 sites in the TOTALITY `crates/` tree — but SK-V18 governs the SKINNY tree, and αC
  §2.2 correctly scopes its Lock-2 pin to skinny; no contradiction.)
- **Phantom `<G>` confirmed:** `grep -rn 'ValueRef<.*EventGrammar>' skinny/crates/runtime/src
  | grep -v 'tests.rs'` → **EMPTY**. Only `event_grammar_tests.rs` carries a non-`AnyGrammar`
  instantiation. Production rides `AnyGrammar`.
- **`CssEventGrammar` does NOT exist:** `grep -rn 'CssEventGrammar' skinny/crates/runtime/src`
  → **EMPTY**. Only `grammars/json/event_grammar_witness.rs` (`JsonEventGrammar`) +
  `grammars/sheets_witness/event_grammar_witness.rs` (`SheetsEventGrammar`) exist, both
  inert witnesses. The V1 finding that "instantiate ≥2" entails authoring a new
  grammar-named type stands.
- **Divergent value API confirmed:** `impl DocumentView for …` lives ONLY in
  `grammars/json/view.rs`; NO `css_l4_*` implements it.
  `css_l4_declaration_values/generated.rs:15` comments *"the CSS analogue of
  `JsonNodeKind::at_cursor`"*, `:25` re-declares `at_cursor`, `:46`
  `node: ValueRef<'doc,'input>` (both K and G defaulted, untyped). The `at_cursor` pattern
  is hand-copied with NO shared trait. G4 thesis ground-truth-anchored.
- **Fork confirmed:** `runtime_generator.rs:17,25` branch
  `RuntimeEmitterKind::{CompiledLowering,RequestFacts}`; `:195` `JSON_PARSE_ONLY_GENERATED_RS`,
  `:701` `CSS_GENERATED_RS` const-`&str`.
- **16-lock count:** `grep -cE '^[0-9]+\. \*\*' LOCKS.md` = **16**, matches SYNTHESIS:179.

Every CH5-relevant citation in the V2 artefacts resolves as stated. Dispositions below are
about coupling-surface COMPLETENESS, not citation accuracy.

## αF note

There is NO separate `alphaF.md`. Per `PASS-ALPHA.md:27`, the α-F deliverable IS
`sk-v18/SYNTHESIS.md` + `sk-v18/HANDOFF.md` (at the tranche root, not under `research/alpha/`).
This is contract-correct, not a missing artefact. CH5 reviews them as α-F.

## V1 REVISE-fold verification (the seven)

Each V1 REVISE is re-checked against the artefact it targeted:

| V1 REVISE | Target | V2 status | Evidence |
|---|---|---|---|
| αA §3.3 phantom two-axis | αA | **FOLDED** | αA:19-21,167-182,247 name `K=AnyKind`/`G:EventGrammar`; "G4 targets the `G` axis; `K` already real" |
| αC §1 P4 witness/EventGrammar scan gap | αC | **FOLDED** | αC:47-57,174-194,451 add the NAME-PARAMETER clause + `EventGrammar`/`*EventGrammar` to forbidden-token scan; "checked twice" applied to witness |
| αD I5 phantom single-axis | αD | **FOLDED** | αD:83 two-axis caveat (`K` real `view.rs:86,143,…`; `G` phantom); "EventGrammar axis ONLY" |
| αD S9 DocumentView citation | αD | **FOLDED** | αD:119 re-pins to `grammars/json/view.rs:68` (verified SOLE impl); `tape/mod.rs:227` marked "TRAIT/assoc def, line at close SHA" |
| αE B3 G4 phantom+trait false-green | αE | **FOLDED** | αE:21(F6),22(F7),132,139-142 — DELETE default, test-excluded phantom grep, CssEventGrammar burden, preserve-rich-ast structural gate |
| SYNTHESIS G4 two-axis+CssEventGrammar | SYN | **FOLDED** | SYN:20-21,173,219 name `G` vs already-real `K`, DELETE default, trait independent of `<G>` |
| SYNTHESIS §2 trait-count false-green | SYN | **FOLDED** | SYN:394-395 add `json_rich_navigation_preserved`; gate consumer :414,:427 REJECTs LCD-flatten behind ≥2 impl-count |

**All seven V1 REVISEs are correctly and substantively folded.** None is a cosmetic
re-word; each carries the concrete two-axis / test-exclusion / richness mechanism the V1
disposition demanded. The fold-back references (αA:277, αD:225, αC:47, αE:21-22) cite the
V1 CH5 line numbers, which is the correct triumvirate-discipline trace.

---

## §A — αA results-extraction.md

### A.1 — §3.3 phantom two-axis (V1 REVISE re-check) — **ACCEPT**

αA:167-182 now states the four-slot signature explicitly, the §3.3 table at :181-182
separates "phantom `G` axis" from the "typed `ValueRef<…,Kind>` … real for JSON" and binds
"**G4: instantiate-or-delete the `G` axis.**" The V1 collapse is gone. ACCEPT.

### A.2 — §4 no-second-substrate pre-block — **ACCEPT** (unchanged, re-verified absent at HEAD).

### A.3 — §3.1/§3.2/§6 fork + replica + working-tree caveat — **ACCEPT** (unchanged, CH5-clean).

---

## §B — αB competitor-deltas.md

### B.1 — §0/§3 plane-asymmetry — **ACCEPT** (JSON near-symmetric strict / CSS lazy-vs-eager disclosed; no cross-plane coupling).

### B.2 — §1.4/DM1 typed-row conditionality — **ACCEPT** (typed rows quarantined; not the preservation bar).

### B.3 — §4 Sheets no-competitor-bar — **ACCEPT** (Sheets bar is GENERATION not throughput; no fabricated speed coupling).

---

## §C — αC redress-digest.md

### C.1 — §2.2 StructRegistry SPLIT + Lock-2 pin — **ACCEPT**

αC:290 still carries "**NB Lock 2: canonical name is `Layout`/`LayoutFacts`, NOT
`StructLayout` (RETIRED, `LOCKS.md`).**" Verified `StructLayout` absent in skinny. The
strongest CH5 anchor in the cohort; forecloses a `StructLayout`-named second substrate
re-introduced as "the layout the generator consumes." ACCEPT.

### C.2 — §2.3 fact-stream PERMANENT + retirement clause — **ACCEPT** (unchanged; `CSS_GENERATED_RS`/`RequestFacts` retirement bound to a close gate).

### C.3 — §3 "checked TWICE (runtime AND emitter)" corollary — **ACCEPT**

αC:445-456 keeps the keystone and now extends it to the witness surface (:451 "a
grammar-named `EventGrammar` type literal (V2-FOLD §1-P4)"). The deepest hidden-coupling
vector in a generalization cycle — a pre-block re-opened by the GENERATOR *emitting* it —
remains closed, now including the witness leak. ACCEPT; lifted to CONSOLIDATED.

### C.4 — §1 P4 witness/EventGrammar scan gap (V1 REVISE re-check) — **ACCEPT**

αC:174-194 folds the V1 REVISE: the witness type is injected by NAME-PARAMETER from the
runtime-side hand-written grammar module, never templated as a string literal; the close
gate at :190-191 adds `EventGrammar`/`*EventGrammar` to the emitter-scoped forbidden-token
set scanned over `runtime_generator.rs` (post-G3 unified). The Lock-14-invisible seam (the
generic-crate scan never sees `runtime/`-side grammar names) is named and gated. ACCEPT.

---

## §D — αD validated-invalidated.md

### D.1 — V1/I5/§5 substrate+phantom carry-forward — **ACCEPT** (citations re-verified `tape/mod.rs:175`; §5 no-second-substrate clause intact).

### D.2 — I5 phantom two-axis (V1 REVISE re-check) — **ACCEPT**

αD:83 now carries the explicit two-axis caveat: `K` REAL+load-bearing (`view.rs:86,143,
197,222,244,256`), `G` PHANTOM (always `AnyGrammar`, 0 non-test call sites), "instantiate-
or-delete the phantom **`<G>` (the EventGrammar axis ONLY)** — `K` is real, must NOT be
conflated or removed." V1 looseness discharged. ACCEPT.

### D.3 — S9 DocumentView citation (V1 REVISE re-check) — **ACCEPT**

αD:119 re-pins the owner surface to `grammars/json/view.rs:68` (`impl DocumentView for
JsonDocument` — verified the SOLE impl at HEAD) and marks `tape/mod.rs:227` as
"TRAIT/assoc def, line at close SHA." The V1 soft-citation is now a hard impl-site
citation. ACCEPT.

### D.4 — DM2 Sheets ready-not-proven — **ACCEPT** (unchanged; proof routed through generator ONLY).

---

## §E — αE candidate-shortlist.md

### E.1 — CANDIDATE B3 (G4 shared trait + phantom) — **REVISE**

B3 is now largely strong: F6 (αE:21,132,139-140) makes DELETE the abrogate-before-patch
DEFAULT, test-excludes the phantom grep (`grep -v 'tests.rs|#[cfg(test)]'` at :140), and
flags `CssEventGrammar` non-existence as the un-budgeted INSTANTIATE burden. F7 (αE:22,142)
adds the preserve-rich-ast structural gate. Both V1 REVISE items (E.1.1, E.1.2) are folded.

**One residual NEW seam (V2-fresh):** the SHARED-TRAIT gate at **αE:141** —
`grep -l 'impl.*Document.*for' crates/runtime/src/grammars/{json,css_l4_*}` — **is NOT
test-excluded**, while its sibling phantom grep one line above (:140) explicitly IS
(`grep -v 'tests.rs|#[cfg(test)]'`, the F6 fold). A test-only
`impl SharedValueTrait for CssTestNode` under `#[cfg(test)]` would false-green the
trait-impl gate exactly as the standing test `JsonEventGrammar` line would have
false-greened the phantom gate before F6. This is the SAME false-green class CH5 V1 §E.1.2
raised, applied to the trait-impl axis — F6 fixed the phantom grep but the sibling
trait-impl recipe was not given the analogous guard.

**Mitigating fact (why REVISE not REJECT):** the CONTRACT-level gate is already safe — the
SYNTHESIS telemetry column `shared_value_trait_instantiations` (SYN:394) defines it as "≥2
real **production** instantiations … test-only `_proof_compiles` does NOT count." So the
machine-checked close gate cannot false-green; only the αE *research recipe* at :141 is
loose, and a downstream implementer who copies the αE grep verbatim (rather than the
SYNTHESIS definition) could miscount. Cheap, orphan-free.

**Fix:** make αE:141 read `grep -rn 'impl .* SharedValueTrait .* for' --include='*.rs'
crates/runtime/src/grammars | grep -v 'tests\.rs\|#\[cfg(test)\]'` → BOTH families present
in NON-test code, mirroring the :140 phantom-grep exclusion exactly. (Equivalently: cite
the SYNTHESIS:394 "production instantiations, test-only does NOT count" definition inline so
the recipe inherits the exclusion.)

### E.2 — CANDIDATE A/B1/B2/B4 sequencing + no-second-substrate pre-blocks — **ACCEPT**

Each carries the no-second-substrate pre-block (B2 "no second CSS tape … rides
`flag_cursors`/`flag_values`"; B4 "Lock 1 (Sheets rides the existing tape, no third
substrate)"). PRUNE-before-GENERALIZE entry-gating prevents a blind-gate re-leak.
CH5-clean on the substrate axis. ACCEPT.

### E.3 — B4 G6 acceleration-wiring same-wave-consumer — **ACCEPT**

αE:164,172 bind every NEON/ASM kernel to a same-wave hot-path consumer and the
acceleration-at-admission gate (`_neon` kernel reached at admission, NOT `#[cfg(test)]`),
closing the orphan-kernel hidden coupling. The "honest-finding escape is GATED" clause
(αE:197) forbids a relabeled blob masquerading as a primitive. ACCEPT.

---

## §F — SYNTHESIS.md + HANDOFF.md (the α-F deliverable)

### F.1 — §0.4 pre-blocks + no-second-substrate + hidden-coupling escapes — **ACCEPT**

SYN:256-285 enumerates the full carrier set: verbatim-blob re-entry (:256-258),
distinct-grammar-output re-entry (:262-264), single-emitter unification admitted-not-
re-opened (:274), and the no-second-substrate clause verbatim (:283-285
"`StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the landed `Tape`/`ValueRef` is
a Lock 1 type-ambivalence violation"). HANDOFF:191-205 mirrors it. The most complete
hidden-coupling pre-block list in the cohort, ground-truth consistent (no second substrate
at HEAD). ACCEPT.

### F.2 — G4 close condition: two-axis + CssEventGrammar (V1 REVISE re-check) — **ACCEPT**

SYN:173 + :219 now state "The G4 target is the **`G: EventGrammar` axis** … (NOT the
already-real `K=Kind` axis)", "The shared trait's existence is INDEPENDENT of the `<G>`
phantom — deleting `<G>` and defining the trait are separable; do NOT couple the trait's
shape to animating `<G>`", and "**DELETE is the abrogate-before-patch DEFAULT** (no
`CssEventGrammar` witness exists at HEAD)." Both V1 REVISE items (F.2.1 single-axis, F.2.2
CssEventGrammar un-budgeted) are discharged. The trait-count is decoupled from the phantom
(`shared_value_trait_instantiations` at SYN:414 is separate from
`phantom_generic_resolved` at :413), so the DELETE default does not block trait-count ≥2.
ACCEPT.

### F.3 — §2 telemetry trait-count false-green (V1 REVISE re-check) — **ACCEPT**

SYN:394-395 adds `json_rich_navigation_preserved` (boolean, must be `true`: JSON
`get(key)` + typed-`Kind` + visitor reachable THROUGH the shared trait) AND the column
def for `shared_value_trait_instantiations` is itself test-excluded ("test-only
`_proof_compiles` does NOT count"). The gate consumer (SYN:414) requires
`json_rich_navigation_preserved == true` and REJECTs (:427)
`json_rich_navigation_preserved == false` "(LCD-flatten regression behind a ≥2
impl-count)." The V1 hidden coupling between "shared trait" and a silent preserve-rich-ast
regression is closed at the machine-checked gate. ACCEPT.

### F.4 — G3 un-fork + relocated-overfit seam — **ACCEPT**

SYN:172 retires `RuntimeEmitterKind` and adds the canonical grammar-neutrality grep
(`rg 'match … Json => | CssL4 => | Sheets => ' codegen` → 0) with the explicit
"md5-distinctness alone is necessary-not-sufficient — a neutral md5-distinct output can
still come from a grammar-branching body." §0.4 binds the relocated-overfit seam
(per-rule branching relocated into projection DATA is forbidden; every residual routing
entry names its `.bbnf` rule). The un-fork cannot re-couple grammar-family logic into a
data table. ACCEPT.

### F.5 — §0.6/§2 timed-plane + Track1≡Track2 guard — **ACCEPT**

`corpus_in_timer` column (SYN:405) + the Track1≡Track2 sidecar pre-block + single-tuple
broadcast REJECT (:430-431). No hidden timed-plane coupling survives. ACCEPT.

### F.6 — HANDOFF next-move + gate consumer — **ACCEPT**

HANDOFF:104-105 carries the two-axis G4 (`G`-not-`K`, DELETE default); :232-234 carries
the P4 witness/EventGrammar forbidden-token clause; :159-165 + :191-200 carry the full
addenda-lens set (verbatim-blob, distinct-grammar-output, single-emitter-path,
phantom-generic) + the no-second-substrate clause; :268 routes the full
`--skv18-generalization-report` consumer. The substrate/phantom/fork/richness columns all
carried. ACCEPT.

---

## CH5 cross-cutting findings (for CONSOLIDATED)

1. **All seven V1 REVISEs are correctly folded.** The two V1 root causes — (a) `ValueRef`
   two-axis precision and (b) shared-trait richness assertion — are discharged across αA,
   αC, αD, αE, SYNTHESIS, and HANDOFF with the exact two-axis / test-exclusion / richness
   mechanisms demanded. The folds are substantive, not cosmetic, and carry V1-CH5
   line-number traces.

2. **One residual NEW seam (the sole V2 REVISE):** the αE:141 SHARED-TRAIT grep recipe is
   NOT test-excluded, while its F6-folded sibling phantom grep at :140 IS. Same false-green
   class as V1 §E.1.2, applied to the trait-impl axis. **Mitigated** at the contract level —
   the SYNTHESIS telemetry column (SYN:394) IS test-excluded, so the machine gate is safe;
   only the αE research recipe is loose. Cheap one-line fix (add `grep -v 'tests.rs|
   #[cfg(test)]'`), no orphan.

3. **The substrate-union foundation is real and correctly guarded** (re-verified V2): no
   second substrate workspace-wide in skinny; the phantom `<G>` is test-only; `CssEventGrammar`
   does not exist (DELETE-default justified); the divergent value API (DocumentView JSON-only)
   is accurately diagnosed; the fork is intact pending G3. The contract guards each.

4. **The genuinely-strong CH5 anchors carried forward** (ACCEPT, lift): αC §2.2 Lock-2
   `Layout`-not-`StructLayout` pin; αC §3 "checked TWICE (runtime AND emitter)" extended to
   the witness surface; SYN §0.4 hidden-coupling-escape enumeration (verbatim-blob /
   distinct-grammar-output / single-emitter-path / phantom-generic / no-second-substrate);
   the SYN:414 decoupling of trait-count from the phantom axis + the
   `json_rich_navigation_preserved` richness gate.

## Disposition summary

| Section | Disposition |
|---|---|
| αA §3.3 phantom two-axis (V1-fold) | ACCEPT |
| αA §4 no-second-substrate | ACCEPT |
| αA §3.1/3.2/6 fork+replica+caveat | ACCEPT |
| αB §0/§3 plane-asymmetry | ACCEPT |
| αB §1.4/DM1 typed conditionality | ACCEPT |
| αB §4 Sheets no-competitor | ACCEPT |
| αC §2.2 StructRegistry SPLIT + Lock-2 pin | ACCEPT |
| αC §2.3 fact-stream + retirement clause | ACCEPT |
| αC §3 checked-twice corollary (witness-extended) | ACCEPT |
| αC §1 P4 witness/EventGrammar scan (V1-fold) | ACCEPT |
| αD V1/I5/§5 substrate+phantom | ACCEPT |
| αD I5 phantom two-axis (V1-fold) | ACCEPT |
| αD S9 DocumentView citation (V1-fold) | ACCEPT |
| αD DM2 Sheets ready-not-proven | ACCEPT |
| αE B3 G4 phantom+trait (residual trait-grep false-green) | REVISE |
| αE A/B1/B2/B4 sequencing+pre-blocks | ACCEPT |
| αE B4 G6 acceleration same-wave | ACCEPT |
| SYNTHESIS §0.4 pre-blocks+escapes | ACCEPT |
| SYNTHESIS G4 phantom two-axis+CssEventGrammar (V1-fold) | ACCEPT |
| SYNTHESIS §2 richness telemetry (V1-fold) | ACCEPT |
| SYNTHESIS G3 un-fork+relocated-seam | ACCEPT |
| SYNTHESIS §0.6/§2 timed-plane | ACCEPT |
| HANDOFF next-move+gate consumer | ACCEPT |

23 sections: **22 ACCEPT, 1 REVISE, 0 REJECT.**

**CH5 V2 verdict:** the substrate-union Lock-1 foundation is real and correctly guarded —
no second substrate at HEAD (workspace-wide in skinny), the pre-block lists are complete
and ground-truth consistent, the phantom `<G>` and divergent value API are accurately
diagnosed. All seven V1 REVISEs are substantively folded with the exact two-axis /
test-exclusion / richness mechanisms demanded; the convergence from 7 REVISE → 1 REVISE is
real, not asserted. The single residual REVISE (αE:141 trait-impl grep not test-excluded)
is a research-recipe loose-end already neutralized at the contract gate (SYN:394 IS
test-excluded), carries a one-line concrete fix, and is not an orphan. No REJECT: nothing
in the cohort re-opens a hidden-coupling carrier, hides a second substrate, or animates the
phantom into a new coupling. CH5 is at convergence (>95% ACCEPT) modulo the single cheap
REVISE.

TALLY accept=22 revise=1 reject=0
