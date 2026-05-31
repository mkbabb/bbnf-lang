# CH5 — HIDDEN-COUPLING (V3) — SK-V18 Pass Alpha hardening

Lens: CH5 HIDDEN-COUPLING. Cycle V3. Adversarial review of the Pass-Alpha SK-V18
artefacts per `PASS-ALPHA §3` + `ORCHESTRATOR §3W`.

**Lens charter (binding for every disposition):** substrate-union Lock 1 preserved; **no
second substrate** introduced through the new generator / value-API surfaces; the
**shared value trait does not silently re-fork** the emitter or substrate; the **phantom
`<G>` is instantiated-or-deleted**, not animated into a new coupling. Hidden coupling = a
dependency the close conditions do NOT name that, if it exists, lets a refuted carrier
re-land or lets the generalization claim a unification it did not achieve.

## V3 method

CH5 V1 returned 16A/7R/0X (two root causes: `ValueRef` two-axis; shared-trait
richness). CH5 V2 returned 22A/1R/0X (one residual: αE:141 trait-impl grep not
test-excluded). The V3 pass does three things: (1) re-greps every load-bearing coupling
surface at working HEAD to re-anchor ground truth; (2) verifies the sole V2 REVISE
(αE:141 / fold F9) and the cross-artefact folds (F10/F11/F12) are substantively landed;
(3) runs a FRESH adversarial sweep for NEW seams the V2→V3 folds may have opened or left
— this is where V3 earns its keep, and it surfaced ONE genuine new hidden-coupling the
whole cohort omits (the `ext/x86/` + `build.rs` x86-assembler surface, §C.5/§F.7 below).

## Ground-truth re-verification (this review, at working HEAD)

Re-greped, not trusted:

- `skinny/crates/runtime/src/tape/mod.rs:175`
  `pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar>` —
  **four-slot, two defaulted axes confirmed** (V1/V2 distinction holds).
- **No second substrate:** `grep -rln 'StructLayout|TapeStructBuilder|TapeCursor'
  skinny/crates/` → **EMPTY workspace-wide**. Lock-1 carriers absent in the skinny tree.
- **Phantom `<G>` confirmed:** `grep -rn 'ValueRef<.*EventGrammar>'
  skinny/crates/runtime/src | grep -v 'tests.rs'` → **EMPTY**. Production rides
  `AnyGrammar`; only `event_grammar_tests.rs` carries a non-`AnyGrammar` instantiation.
- **`CssEventGrammar` does NOT exist:** `grep -rn 'CssEventGrammar'
  skinny/crates/runtime/src` → **EMPTY**. Only `grammars/json/event_grammar_witness.rs`
  (`JsonEventGrammar`) + `grammars/sheets_witness/event_grammar_witness.rs`
  (`SheetsEventGrammar`), both inert. The DELETE-default justification stands.
- **Divergent value API confirmed:** `impl DocumentView` lives ONLY in
  `grammars/json/view.rs`; NO `css_l4_*` implements it. G4 thesis ground-truth-anchored.
- **Fork confirmed:** `runtime_generator.rs:1,17,25` branch
  `RuntimeEmitterKind::{CompiledLowering,RequestFacts}`; `:195`
  `JSON_PARSE_ONLY_GENERATED_RS`, `:701` `CSS_GENERATED_RS` const-`&str`.
- **F10 xtask seam is REAL:** `xtask/src/regen_css.rs:35` `const TARGETS: &[RuntimeTarget]`
  with `entry_rule`/`source_roots` fields — exactly the workspace-metadata DATA surface
  F10 widens the neutrality scan to cover; a relocated `match grammar` branch could live
  here invisibly to a codegen-only grep. Verified present.
- **F10 grammar-named-type census surface is REAL:** `codegen/src/lib.rs:1317` +
  `codegen/src/lower/tape_plan.rs:25` carry the token `EventGrammar` in codegen — exactly
  the grammar-named-type leak surface the F10 second-grep (`JsonParser|CssL4Parser|
  GoogleSheetsParser|BbnfBootstrap`) guards. Verified present.
- **Lock-14 canonical alphabet confirmed:** `restart/locks/LOCKS.md:349` verification
  command reads `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|
  GoogleSheets\w*\s*=>' crates/` — F10's un-abbreviated `GoogleSheets` + `Bbnf` alphabet
  matches the lock verbatim. F10 is correct, not a guess.

Every CH5-relevant citation in the V3 artefacts resolves as stated. Dispositions are
about coupling-surface COMPLETENESS, not citation accuracy — and V3 found one completeness
hole the cohort missed.

## αF note

There is NO separate `alphaF.md` / `alphaF` artefact (the prompt names "alphaA..F" but the
α-F deliverable per `PASS-ALPHA.md:27` IS `sk-v18/SYNTHESIS.md` + `sk-v18/HANDOFF.md` at
the tranche root). The cohort is alphaA..E + SYNTHESIS + HANDOFF. This is contract-correct,
not a missing artefact. CH5 reviews SYNTHESIS+HANDOFF as α-F (§F below).

## V2 REVISE-fold + V2→V3 fold verification

| fold | source | V3 status | evidence |
|---|---|---|---|
| **F9** (sole V2 REVISE) | CH5 V2 E.1 | **FOLDED** | αE:156 SHARED-TRAIT grep now `grep -rn 'impl .* SharedValueTrait .* for' … \| grep -v 'tests\.rs\|#\[cfg(test)\]'` → BOTH families in NON-test code; mirrors the :155 phantom-grep exclusion exactly; cites SYNTHESIS:394 inheritance |
| F10 | CH2 V2 §8.1 | **FOLDED + ground-truth-correct** | αE:185,214 neutrality grep = canonical four-grammar alphabet (`GoogleSheets` un-abbreviated, `Bbnf` forward-safe), scan-roots widened to `xtask/src` (the `regen_css.rs:35 RuntimeTarget` DATA table — verified real), + grammar-named-type census second surface (the `EventGrammar` token in `codegen/lib.rs:1317`/`tape_plan.rs:25` — verified real) |
| F11 | CH4 V2 §1 | **FOLDED + disk-verified** | αE:72 P1 LOC −742→−847 (742 `.rs` + 105 `.asm`); `src/x86_64/byte_class_from_eq_set_64.asm` exists on disk |
| F12 | CH4 V2 §5 | **FOLDED + disk-verified** | αE F12 dispatch path corrected to `bbnf-simd/src/dispatch.rs`; disk shows `src/dispatch.rs` only, no `aarch64/dispatch.rs` |

All V2 folds are substantive and ground-truth-correct. The convergence 7R(V1)→1R(V2)→
(F9 folded) is real. The V3 REVISE below is a FRESH finding, NOT a re-raise.

---

## §A — αA results-extraction.md — **ACCEPT (all)**

- **A.1 §3.3 phantom two-axis** (V1-fold, re-verified): αA:167-182 carries the four-slot
  signature, separates "phantom `G` axis" from "typed `ValueRef<…,Kind>` real for JSON,"
  binds "G4: instantiate-or-delete the `G` axis." ACCEPT.
- **A.2 §4 no-second-substrate**: re-verified absent at HEAD. ACCEPT.
- **A.3 §3.1/3.2/6 fork+replica+caveat**: CH5-clean. ACCEPT.

## §B — αB competitor-deltas.md — **ACCEPT (all)**

- **B.1 §0/§3 plane-asymmetry**: JSON near-symmetric strict / CSS lazy-vs-eager disclosed;
  no cross-plane coupling. ACCEPT.
- **B.2 §1.4/DM1 typed-row conditionality**: typed rows quarantined; not the preservation
  bar. ACCEPT.
- **B.3 §4 Sheets no-competitor-bar**: Sheets bar is GENERATION not throughput. ACCEPT.

## §C — αC redress-digest.md

- **C.1 §2.2 StructRegistry SPLIT + Lock-2 pin** — **ACCEPT.** The `Layout`/`LayoutFacts`-
  not-`StructLayout` (RETIRED) pin is carried; `StructLayout` verified absent in skinny.
  Strongest CH5 anchor; forecloses a `StructLayout`-named second substrate.
- **C.2 §2.3 fact-stream PERMANENT + retirement clause** — **ACCEPT.** `CSS_GENERATED_RS`/
  `RequestFacts` retirement bound to a close gate.
- **C.3 §3 "checked TWICE (runtime AND emitter)" corollary** — **ACCEPT.** Extended to the
  witness surface (a grammar-named `EventGrammar` literal the generator could emit). The
  deepest hidden-coupling vector (a pre-block re-opened by the GENERATOR *emitting* it)
  remains closed.
- **C.4 §1 P4 witness/EventGrammar scan gap** (V1-fold) — **ACCEPT.** NAME-PARAMETER
  injection + `EventGrammar`/`*EventGrammar` in the emitter-scoped forbidden-token set.
- **C.5 §1 P1/P5 prune scope — the `ext/x86/` + `build.rs` x86 surface is UN-NAMED** —
  **REVISE.** *(NEW V3 finding — the sole V3 REVISE; see §F.7 for the contract-side twin.)*

  αC §1 P1 (and the αE P1 row, SYNTHESIS:194, HANDOFF:90) scope x86 deletion to
  `skinny/crates/bbnf-simd/src/x86_64/` (24 files, 847 LOC per F11). CH5's fresh sweep
  found a SECOND x86 coupling the whole cohort omits:

  1. `skinny/crates/bbnf-simd/ext/x86/` — a vendored x86 ASM tree: `bbnf.asm` (23.8 KB),
     `x86inc.asm` (59.5 KB), `x86util.asm` (22.9 KB) ≈ **106 KB / ~3000+ LOC** of x86
     assembly (x264/FFmpeg `cglobal`/AVX-512 ZMM macro headers, per
     `ext/x86/LICENSE-VENDOR:50-51`). Verified on disk.
  2. `skinny/crates/bbnf-simd/build.rs` — the **nasm-rs x86 assembler driver**
     (`:1` "assembles vendored + authored x86_64 .asm sources"; `:28-30`
     `rerun-if-changed=ext/x86/{x86inc,x86util,bbnf}.asm`; `:52` `include_root =
     …/ext/x86`; `:56-76` `nasm_rs::Build … compile_objects … rustc-link-lib=static=
     bbnf_simd_asm`). A build-graph x86 coupling.
  3. `src/lib.rs:247` — the aarch64 scalar reference contract still points at
     `ext/x86/bbnf.asm` ("Contract documented in ext/x86/bbnf.asm").

  **Why this is a hidden coupling, not pedantry:** the mandate is **"x86 gone"** (R10,
  SYNTHESIS:61/282, V3 P1 backlog). Deleting `src/x86_64/` while `ext/x86/` + the nasm
  `build.rs` survive means a refuted carrier (x86) persists in the build graph and the
  vendor tree. The P1 verify command (SYNTHESIS:194) `grep -riE 'avx|gfni|sve|x86'
  skinny/crates/bbnf-simd/src/` is scoped to `src/` — `ext/x86/` is a SIBLING of `src/`
  and `build.rs` is at the crate ROOT, so **both escape the verify grep entirely**, and the
  P1 LOC accounting (−847) omits the ~3000+ LOC vendor ASM + the build driver. The
  cohort's "x86 gone" claim is literally false while these survive.

  **Why REVISE and not REJECT:** `build.rs:40` returns early on non-`x86_64`
  (`if target_arch != "x86_64" { return; }`), and `ext/x86/` is referenced by NO aarch64
  path (`grep -rln 'ext/x86|x86inc|x86util|bbnf.asm' src/aarch64/ src/scalar/` → NONE). So
  the surface is **dormant on aarch64**, not active-on-target — it does not corrupt a
  running aarch64 build. It is a present-but-inert x86 carrier: a hidden-coupling that
  falsifies the close CLAIM (and the `x86_tree_deleted` telemetry, §F.7) without breaking
  the build. That is REVISE-severity, not REJECT.

  **Fix (αC §1 + the αE P1 row + SYNTHESIS P1):** widen P1 scope to delete the ENTIRE x86
  surface, not just `src/x86_64/`: (a) `rm -rf skinny/crates/bbnf-simd/ext/x86/`; (b)
  delete or neutralize `bbnf-simd/build.rs` (the nasm driver — with no x86 sources and no
  `ext/x86/` headers it has no reason to exist on an aarch64-only crate); (c) drop the
  `src/lib.rs:247` `ext/x86/bbnf.asm` contract reference (re-home the scalar-reference
  contract into the aarch64/scalar module doc). Re-state the P1 LOC as `src/x86_64/`
  (−847) **+ `ext/x86/` (~−3000 ASM) + `build.rs` (~−100)**. Change the P1 verify command
  from `…/src/` to **`grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`**
  (crate-wide, including `ext/` and `build.rs`) → only aarch64-neutral comments. Without
  this, "x86 gone" is a paper-claim and `x86_tree_deleted` false-greens.

## §D — αD validated-invalidated.md — **ACCEPT (all)**

- **D.1 V1/I5/§5 substrate+phantom carry-forward**: citations re-verified
  `tape/mod.rs:175`; §5 no-second-substrate clause intact. ACCEPT.
- **D.2 I5 phantom two-axis** (V1-fold): `K` REAL / `G` PHANTOM caveat carried;
  "instantiate-or-delete the `<G>` (EventGrammar axis ONLY)." ACCEPT.
- **D.3 S9 DocumentView citation** (V1-fold): re-pinned to `grammars/json/view.rs`
  (verified SOLE impl). ACCEPT.
- **D.4 DM2 Sheets ready-not-proven**: proof routed through the generator ONLY. ACCEPT.

## §E — αE candidate-shortlist.md

- **E.1 B3 G4 phantom+trait (F6/F7/F9 re-check)** — **ACCEPT.** F9 (αE:156) test-excludes
  the SHARED-TRAIT grep mirroring the :155 phantom-grep exclusion; F6 (αE:147,154) makes
  DELETE the abrogate-before-patch DEFAULT and flags `CssEventGrammar` non-existence as the
  INSTANTIATE burden (verified: no `CssEventGrammar` at HEAD); F7 (αE:160-161) carries the
  preserve-rich-ast structural condition. The sole V2 false-green seam is closed. ACCEPT.
- **E.2 A/B1/B2/B4 sequencing + no-second-substrate pre-blocks** — **ACCEPT.** Each carries
  the no-second-substrate pre-block (B2 "no second CSS tape … rides `flag_cursors`/
  `flag_values`"; B4 "Lock 1 (Sheets rides the existing tape, no third substrate)",
  αE:162). PRUNE-before-GENERALIZE entry-gating (αE:211). CH5-clean on the substrate axis.
- **E.3 B4 G6 acceleration-wiring same-wave-consumer** — **ACCEPT.** Every NEON/ASM kernel
  bound to a same-wave hot-path consumer; acceleration-at-admission gate. Closes the
  orphan-kernel coupling.
- **E.4 F10 distinct-grammar-output litmus (NEW fold re-check)** — **ACCEPT.** αE:185,214
  fold the canonical four-grammar Lock-14 alphabet + the widened `xtask/src` scan-root +
  the grammar-named-type census. CH5 verified all three target surfaces are REAL
  (`regen_css.rs:35 RuntimeTarget` DATA table; `codegen/lib.rs:1317`/`tape_plan.rs:25`
  `EventGrammar` token; `LOCKS.md:349` alphabet). This is a genuinely strong CH5 addition —
  it closes the relocated-`match grammar`-into-a-data-table re-coupling the md5-distinct
  gate alone cannot catch. The "md5-distinct is NECESSARY-NOT-SUFFICIENT" framing
  (αE:185) is exactly the CH5 distinct-grammar-output discipline. ACCEPT, lift to
  CONSOLIDATED.

## §F — SYNTHESIS.md + HANDOFF.md (the α-F deliverable)

- **F.1 §0.4 pre-blocks + no-second-substrate + hidden-coupling escapes** — **ACCEPT.**
  Full carrier set enumerated (verbatim-blob, distinct-grammar-output, single-emitter,
  no-second-substrate verbatim). Most complete pre-block list in the cohort,
  ground-truth consistent.
- **F.2 G4 two-axis + CssEventGrammar** (V1-fold) — **ACCEPT.** `G`-not-`K` axis named;
  trait independent of `<G>`; DELETE the abrogate-before-patch DEFAULT.
- **F.3 §2 richness telemetry** (V1-fold) — **ACCEPT.** `json_rich_navigation_preserved`
  column + gate-consumer REJECT of LCD-flatten behind ≥2 impl-count.
- **F.4 G3 un-fork + relocated-overfit seam** — **ACCEPT.** `RuntimeEmitterKind` retired;
  grammar-neutrality grep; md5-distinctness flagged necessary-not-sufficient; relocated-
  branch seam bound (now reinforced by the F10 xtask-surface widening, §E.4).
- **F.5 §0.6/§2 timed-plane + Track1≡Track2 guard** — **ACCEPT.** `corpus_in_timer` column;
  Track1≡Track2 sidecar pre-block; single-tuple broadcast REJECT.
- **F.6 HANDOFF next-move + gate consumer** — **ACCEPT.** Two-axis G4, P4 witness/
  EventGrammar clause, full addenda-lens set, full `--skv18-generalization-report`
  consumer. Substrate/phantom/fork/richness columns carried.
- **F.7 §2 `x86_tree_deleted` telemetry + P1 verify scope — `ext/x86/`+`build.rs` UN-NAMED**
  — **REVISE.** *(The contract-side twin of §C.5 — same root, the gate that would
  false-green. Counted as ONE REVISE with §C.5: see disposition table.)*

  SYNTHESIS:433 defines `x86_tree_deleted` = "(`bbnf-simd/src/x86_64/` gone; aarch64-only)"
  and SYNTHESIS:194 P1 verify = `grep -riE 'avx|gfni|sve|x86' skinny/crates/bbnf-simd/src/`.
  Both are scoped to `src/x86_64/` / `src/`. As §C.5 establishes, the `ext/x86/` vendor ASM
  tree (~3000 LOC) + the nasm `build.rs` driver + the `lib.rs:247` `ext/x86/bbnf.asm`
  contract reference survive that scope. **So `x86_tree_deleted == true` can be asserted
  while a 106 KB x86 ASM tree + an x86-assembler build driver remain** — a hidden coupling
  between the machine-checkable "x86 gone" gate and a surviving x86 carrier. This is the
  exact CH5 failure class: a close condition (telemetry column) that does not name a
  dependency (`ext/x86/`+`build.rs`), letting a refuted carrier (x86) re-land green.

  **Fix:** redefine `x86_tree_deleted` (SYNTHESIS:433) as "NO x86 surface anywhere in
  `bbnf-simd` — `src/x86_64/` gone AND `ext/x86/` gone AND `build.rs` carries no nasm/x86
  assembler path." Change the P1 verify (SYNTHESIS:194, HANDOFF:90, the SYNTHESIS:208
  invariant "zero x86/AVX/SVE in bbnf-simd") from `…/src/` to crate-wide
  `grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` (covers `ext/` + `build.rs`)
  → only aarch64-neutral comments. Add `ext/x86/` + `build.rs` to the P1 deletion ledger
  (SYNTHESIS:226,244, HANDOFF:90,269) and re-state the P1 net-LOC (the αE:205 −9250
  net-LOC FLOOR only deepens — this is a net-positive correction for the "deletes more than
  it adds" claim, but the gate must still be made honest so the deletion is verified, not
  asserted).

---

## CH5 cross-cutting findings (for CONSOLIDATED)

1. **All V1+V2 REVISEs are folded; F9 (the sole V2 REVISE) is substantively landed**
   (αE:156 trait-impl grep now test-excluded, mirroring the phantom-grep exclusion). The
   F10/F11/F12 cross-artefact folds are ground-truth-correct (xtask `RuntimeTarget` DATA
   table, `EventGrammar` codegen token, `.asm` file, `dispatch.rs` path all disk-verified).
   The substrate-union foundation, phantom `<G>` test-only status, `CssEventGrammar`
   non-existence (DELETE-default justified), and divergent value API are all re-verified.

2. **NEW V3 hidden-coupling (the sole V3 REVISE, counted once across §C.5+§F.7):** the
   cohort scopes x86-deletion to `bbnf-simd/src/x86_64/` and OMITS `ext/x86/` (≈3000 LOC
   vendored x86 ASM), `build.rs` (the nasm x86-assembler driver), and the `lib.rs:247`
   `ext/x86/bbnf.asm` contract reference. The P1 verify (`…/src/`) and the
   `x86_tree_deleted` telemetry both false-green on the surviving surface. Dormant on
   aarch64 (`build.rs:40` early-returns; `ext/x86/` referenced by no aarch64 path) → REVISE
   not REJECT, but "x86 gone" is literally false until P1 scope widens to the whole crate.
   Concrete fix in §C.5 + §F.7: `rm -rf ext/x86/`, delete/neutralize `build.rs`, re-home
   the `lib.rs:247` contract, and change every P1 verify/telemetry from `src/`-scoped to
   crate-wide `grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`.

3. **The genuinely-strong CH5 anchors carried forward** (ACCEPT, lift): αC §2.2 Lock-2
   `Layout`-not-`StructLayout` pin; αC §3 "checked TWICE (runtime AND emitter)" extended
   to the witness surface; SYN §0.4 hidden-coupling-escape enumeration; the F10
   distinct-grammar-output discipline (md5-distinct NECESSARY-NOT-SUFFICIENT + canonical
   four-grammar alphabet + xtask-surface widening + grammar-named-type census), which CH5
   verified targets REAL surfaces and closes the relocated-`match grammar`-into-DATA seam.

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
| αC §1 P1 `ext/x86/`+`build.rs` x86 surface un-named (NEW V3) | REVISE |
| αD V1/I5/§5 substrate+phantom | ACCEPT |
| αD I5 phantom two-axis (V1-fold) | ACCEPT |
| αD S9 DocumentView citation (V1-fold) | ACCEPT |
| αD DM2 Sheets ready-not-proven | ACCEPT |
| αE B3 G4 phantom+trait (F6/F7/F9 folded) | ACCEPT |
| αE A/B1/B2/B4 sequencing+pre-blocks | ACCEPT |
| αE B4 G6 acceleration same-wave | ACCEPT |
| αE F10 distinct-grammar-output litmus (NEW fold) | ACCEPT |
| SYNTHESIS §0.4 pre-blocks+escapes | ACCEPT |
| SYNTHESIS G4 two-axis+CssEventGrammar (V1-fold) | ACCEPT |
| SYNTHESIS §2 richness telemetry (V1-fold) | ACCEPT |
| SYNTHESIS G3 un-fork+relocated-seam | ACCEPT |
| SYNTHESIS §0.6/§2 timed-plane | ACCEPT |
| SYNTHESIS §2 `x86_tree_deleted` + P1 verify scope (twin of αC P1) | REVISE |
| HANDOFF next-move+gate consumer | ACCEPT |

26 sections: **24 ACCEPT, 2 REVISE, 0 REJECT.** The two REVISEs are the contract-side and
artefact-side faces of ONE hidden coupling (the un-named `ext/x86/`+`build.rs` x86 surface):
αC P1 scopes the deletion, SYNTHESIS:433 is the telemetry gate that would false-green on it.
Both carry the identical concrete fix (widen P1 to the whole `bbnf-simd` crate); neither is
an orphan REVISE.

**CH5 V3 verdict:** the substrate-union Lock-1 foundation is real and correctly guarded —
no second substrate at HEAD (skinny-wide), the phantom `<G>` is test-only, `CssEventGrammar`
does not exist (DELETE-default justified), the divergent value API is accurately diagnosed,
and the fork is intact pending G3. All V1+V2 REVISEs (incl. the sole V2 F9) are
substantively folded with ground-truth-correct mechanisms. The V3 sweep surfaced ONE fresh
hidden coupling the prior cycles missed: the cohort's "x86 gone" claim deletes only
`src/x86_64/` while ~3000 LOC of vendored `ext/x86/` ASM + the nasm `build.rs` driver +
the `lib.rs:247` contract reference survive, and the `x86_tree_deleted` telemetry + P1
verify (both `src/`-scoped) would false-green on them. It is dormant on aarch64 (REVISE,
not REJECT) but falsifies the literal close claim until P1 scope widens crate-wide. No
REJECT: nothing re-opens a second substrate, animates the phantom into a new coupling, or
silently re-forks the emitter/trait. The two REVISEs are the two faces of one coupling,
share one concrete fix, and are orphan-free.

TALLY accept=24 revise=2 reject=0
