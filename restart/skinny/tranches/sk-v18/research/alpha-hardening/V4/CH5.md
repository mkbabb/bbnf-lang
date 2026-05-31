# CH5 — HIDDEN-COUPLING (lens, cycle V4) — SK-V18 Pass-Alpha artefact review

Lens: CH5 Hidden Coupling (PASS-ALPHA §3 / ORCHESTRATOR §3W). Cycle: V4.
HEAD of record: `318d9c046` (SK-V17 closed `f6a38445b`; V3 audit `7dbe44c22`).
Scope per the dispatch: substrate-union Lock 1 preserved; no second substrate; shared
value trait does not fork; instantiate-or-delete the phantom `<G>`. Plus the standing
CH5 mandate — no parallel substrate / sidecar producer / renamed-scanner Lock-1
violation / Track 1 ≡ Track 2 dishonesty; typed product plane gates structurally honest
(Track 2 ≠ Track 1); and the new addenda (verbatim-blob, distinct-grammar-output,
single-emitter-path, phantom-generic, timed-plane-symmetry+corpus-in-timer,
acceleration-wiring).

Artefacts reviewed: `research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md` +
`SYNTHESIS.md` + `HANDOFF.md` (the α-F output per PASS-ALPHA §2; there is no separate
`alphaF.md` — the contract maps α-F to SYNTHESIS+HANDOFF, confirmed by §6 of the
contract).

## §0 — Live ground-truth re-verification (the CH5 load-bearing facts)

Every CH5-dispositive coupling claim re-grepped at HEAD `318d9c046` this cycle:

| Claim | Command / inspection | Result |
|---|---|---|
| Phantom `G` axis vs real `K` axis | `sed -n '175p' crates/runtime/src/tape/mod.rs` | `pub struct ValueRef<'doc,'input:'doc, K = AnyKind, G: EventGrammar = AnyGrammar>` — TWO axes; `K` real, `G` phantom. **Confirms the two-axis distinction every artefact carries.** |
| No second substrate (type level) | `grep -rln 'StructLayout\|TapeStructBuilder\|TapeCursor' crates/runtime/src crates/codegen/src` | **empty** — none present. The pre-block guards against re-introduction, correctly. |
| `UnionTape` is a forbidden-token guard, not a live type | `grep -rn 'UnionTape' …/lib.rs …/lock14_baseline.rs` | only forbidden-token strings (`"Lock 14 forbids UnionTape in IR surface"`, `lock14_baseline.rs:2403-2404`) — NOT a live second-substrate type. |
| Forked emitter | `grep -rn 'enum RuntimeEmitterKind' crates/codegen/src` | `grammar_provider.rs:40` — the JSON-vs-CSS fork is live. |
| `CssEventGrammar` absent (G4 INSTANTIATE = creation) | `grep -rln 'CssEventGrammar' crates/runtime/src` | **empty** — only `Json`+`Sheets` witnesses exist. |
| Shared value trait absent today (value API divergent) | `grep -rln 'SharedValueTrait\|trait Value\b\|trait Cursor\b' crates/runtime/src` | **empty**; `DocumentView` impl is JSON-only (`json/view.rs:68 impl … for JsonDocument`). Confirms divergent API, no shared trait, the G4 surface. |

All four CH5 dispatch pins are anchored to verifiable disk truth. The cohort's
two-axis precision (`G` phantom, `K` real), no-second-substrate posture, single-emitter
fork identification, and shared-trait-absence are each disk-correct. No fabricated
coupling claim; no missed coupling surface in scope.

---

## §1 — αA results-extraction — CH5 disposition

αA §3.3 (phantom generic + divergent value API), §4 (substrate validated, Lock 1),
§3.1 (fork/replica), §6 (caveats) are the CH5-bearing sections.

**§3.3 phantom-generic two-axis.** αA:185-201 states the `ValueRef` four-slot decl, names
`K` (real for JSON) vs `G` (the phantom), and binds G4 to the `G` axis SPECIFICALLY with
an explicit foreclosure: "Binding `K` to a real `Kind` (already done for JSON) does NOT
discharge the phantom; nor does deleting `G` while leaving the impl coupling unchanged."
This is the precise hidden-coupling failure mode CH5 exists to catch — a false-green
"instantiation" of the wrong axis. Disk-verified at `tape/mod.rs:175`. **ACCEPT.**

**§4 substrate validated / Lock 1 / no second substrate.** αA:226-238 carries the
no-second-substrate pre-block verbatim — `StructLayout`/`TapeStructBuilder`/`TapeCursor`
alongside the landed `Tape`/`ValueRef` is named a Lock-1 violation; "the projection
generator emits accessors over the EXISTING types." Disk confirms those types are absent
and `UnionTape` is only a forbidden-token guard. This is the correct CH5 posture: the
substrate is the foundation, the generalization rides it, no parallel substrate is
admitted. **ACCEPT.**

**§3.1 fork + replica + verbatim-blob; §6 caveats.** The fork (`grammar_provider.rs:40`),
the 7 byte-identical replicas (single md5, both SHAs), the const-`&str` CSS blob
(`runtime_generator.rs:701`), and the NEON-wiring `#[cfg(test)]` honesty are all stated
with citation. The §6 caveat correctly forecloses the "Track1 not in RESULTS.md" /
broadcast confusion and the simdjson/yyjson honest-`None` posture — no fabricated
competitor column, no Track1≡Track2 dishonesty. **ACCEPT.**

**αA tally: ACCEPT ×3, REVISE ×0, REJECT ×0.**

---

## §2 — αB competitor-deltas — CH5 disposition

CH5-bearing axes: the two-plane comparator framing (Track 1 vs Track 2; recognition vs
materialization) and the honest-`None` foreclosure (no fabricated/sidecar competitor).

**B.1 plane symmetry / Track-1-vs-Track-2 honesty.** αB:50-58 + §3 state the JSON plane
as near-symmetric strict-vs-strict (both recognition-plane; "sonic skipper does NOT build
owned tree, bbnf `parse_only` does NOT either") and the CSS plane as ASYMMETRIC
lazy-vs-eager, disclosed per H1. Crucially αB:128-137 (§1.4) QUARANTINES the Track 2
typed rows as conditional-on-hand-tuned-schema and explicitly forbids citing them as the
preservation bar — "The unconditional, generalizable JSON >SOTA bar is `parse_only` ↔
sonic-strict." This is exactly the Track-2≠Track-1 structural-honesty CH5 demands: the
typed product plane is not silently merged into the recognition plane. **ACCEPT.**

**B.2 honest-None / no sidecar-competitor.** αB §3.3 + §5 foreclose the corpus-in-timer /
fabricated-competitor contrivance: yyjson/asmjson/RapidJSON are honest `None` on aarch64,
"a column populated with an un-run engine's number would be a contrivance." No hidden
coupling between the comparator plane and a sidecar. **ACCEPT.**

**B.3 Sheets no-competitor-bar.** αB:289,293 — Sheets' bar is GENERATION not throughput;
no fabricated SOTA-Sheets comparison. **ACCEPT.**

**B.4 x86-comparator-OUT vs implementation-x86-scope (V4 cross-cohort).** αB §3.3/§6
correctly isolates its own comparator-OUT statement (asmjson AVX-512 OUT) from the
implementation-side P1 scope-widening REVISE — αB makes no "x86 gone" close-claim, so it
inherits no orphan. This is the right boundary; αB carries no coupling defect from the
V3 CH5 C.5 fold (that fold lands on αC/αE/SYNTHESIS, where it is folded). **ACCEPT.**

**αB tally: ACCEPT ×4, REVISE ×0, REJECT ×0.**

---

## §3 — αC redress-digest — CH5 disposition

This is the most CH5-dense artefact (it enumerates the pre-block families + the
relocated-seam coupling). The V3 wave issued exactly one REVISE here (CH5 C.5, the
`ext/x86/`+`build.rs` second x86 surface) plus a CH2 §8.1 attribution sharpening; both
are folded in this V4 revision.

**C.1 FOLD-1 (CH5 C.5 — the second x86 surface).** αC §0.A.1 + §1-P1 + §2.6 + §3
x86-corollary now scope P1 crate-wide: `src/x86_64/` (847 LOC) + `ext/x86/` (3554 LOC
vendored ASM) + `build.rs` (102 LOC nasm driver) + `Cargo.toml` nasm dep + `lib.rs:247`
contract reference, with the close gate moved from `src/`-scoped to crate-wide
`grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`. The dormancy rationale
(REVISE-not-REJECT: `build.rs:38-40` early-returns on non-x86_64) is carried. This fully
discharges the only V3 REVISE on αC — the hidden x86 carrier that falsified "x86 gone."
**ACCEPT (fold landed).**

**C.2 FOLD-2 (CH2 §8.1 — relocated-overfit-seam attribution).** αC §2.2 re-open test +
§3 relocated-seam corollary now correctly attribute the relocated-seam defense to the
STRUCTURAL P3 row-count collapse (genuinely-distinct `.bbnf` ↔ non-identical
`generated.rs`, `sort -u` over `(source_roots,entry_rule)`), with the `match grammar`
arm-census grep demoted to NECESSARY-NOT-SUFFICIENT. This is the right CH5 call: a
per-grammar branch relocated into a neutral-identifier `RuntimeTarget` DATA table is a
hidden coupling a token-based regex is syntactically incapable of catching; the structural
row-count check is what closes it. **ACCEPT.**

**C.3 §2.1–§2.6 pre-block families (the hidden-coupling carriers).** Each re-open test is
"checked TWICE" (§3 corollary) — against the runtime output AND against the emitter that
produces it. AZ-IV eager (§2.1), StructRegistry/Builder<G> per-leaf indirection (§2.2),
CSS fact-stream String / Track1≡Track2 sidecar dishonesty (§2.3, explicitly "A retained
String product is also a Track1==Track2 / sidecar dishonesty (Lock 1, CH5)"), 24-broadcast
(§2.4), FNV/fixture (§2.5), x86/AVX/SVE (§2.6). This is the complete CH5 pre-block census
and it correctly names the new generator surface as the fresh place each can silently
re-land. The §2.2b witness-`EventGrammar`-type-literal clause (FOLD from V2 C.4) is the
exact hidden-coupling-through-codegen escape: "if the un-forked generator EMITS a
`ValueRef<…,XEventGrammar>` type literal as a string, that is a grammar-name leak the
generic-crate-scoped P4 gate cannot catch." Disk-confirmed no second substrate, no live
`UnionTape` type, no `CssEventGrammar` to be silently spliced. **ACCEPT.**

**C.4 no-second-substrate / substrate-union.** αC §3 + §4 sources bind Lock 1 explicitly;
the §2.2 different-framing admission requires the trait be "a thin read-cursor over the
EXISTING `Tape`/`ValueRef`, NOT a generic `Builder<G>`." Correct. **ACCEPT.**

**αC tally: ACCEPT ×4, REVISE ×0, REJECT ×0.** (Both V3 dispositions — CH5 C.5 and CH2
§8.1 — are concretely folded; zero orphan.)

---

## §4 — αD validated/invalidated ledger — CH5 disposition

CH5-bearing: I5 (phantom `G`/divergent API), the §5 pre-block (no second substrate, no
eager tree behind the shared trait), the S9 G4 wave row.

**D.1 I5 phantom-`G` precision (test-only).** αD:124 + §6 carry the tightened claim: the
ONLY non-default `G` instantiations are `event_grammar_tests.rs:18,20,89` (test-coverage),
ZERO production. The phantom-generic gate must NOT accept the test-only use as a real
instantiation. This is the precise false-green hidden-coupling CH5 caught at V1 and the
fold holds — disk-confirmed `CssEventGrammar` absent, so INSTANTIATE is creation not
rename. **ACCEPT.**

**D.2 S9 G4 — `G`-axis-only + no LCD-flatten + no second substrate.** αD:160 binds G4 to
the `G` (EventGrammar) axis ONLY, mandates the `K` axis stays (deleting it "would destroy
the typed-view machinery `json/view.rs` rides"), and §5 forbids a second
`StructLayout`/`TapeStructBuilder`/`TapeCursor` cursor/builder type. The phantom-generic
gate is correctly stated as test-only-`G`-does-NOT-pass. This is the full CH5 G4 posture.
**ACCEPT.**

**D.3 §5 pre-block (eager-tree / registry / second-substrate behind the trait).** αD:178-208
carries every CH5 carrier: the shared trait stays lazy over the existing `ValueRef` (no
eager tree, AZ-IV); G4 is a trait abstraction not a registry/indirection; one substrate.
The fact-stream pre-block is correctly elevated to RETIRED-re-land-is-REJECT. **ACCEPT.**

**D.4 V4 fold (R1, the αD-only stale "18"→14).** The lone V3 REVISE in the cohort was αD's
stale checkasm "18"; §1 V4 + §8.V4 correct it to disk-true 14 (12 single-kernel + 2),
re-verified live. This is not a CH5 hidden-coupling defect (it is a CH1/CH4 count), but it
is folded clean and leaves no orphan that could mis-seed a downstream gate. **ACCEPT (no
CH5 defect; fold noted).**

**αD tally: ACCEPT ×4, REVISE ×0, REJECT ×0.**

---

## §5 — αE candidate-shortlist — CH5 disposition

CH5-bearing candidates: B1 (single-emitter-path), B3 (shared trait + phantom `<G>`), B4
(no orphan kernel / no second substrate / Sheets via generator), and the cross-cutting
F13 relocated-seam attribution.

**E.1 B1 single-emitter-path (no fork hidden behind abstract clothing).** αE B1 gate #3
requires `RuntimeEmitterKind`/`CompiledLowering`/`RequestFacts` → 0 AND the canonical
neutrality grep → 0. The fork is the de-facto grammar-family branch wearing an abstract
enum (the courier variant cannot emit a different grammar). Correctly identified as the
structural core; entry-gated on P4 (gate must scan the emitter before it is built — closing
the blind-gate hidden coupling). **ACCEPT.**

**E.2 B3 shared trait + phantom `<G>` (DELETE-default, test-excluded grep,
preserve-rich-ast, no second substrate).** This is the densest CH5 candidate and it is
exemplary:
- F6: DELETE is the DEFAULT (abrogate-before-patch); INSTANTIATE is burden-of-proof
  because `CssEventGrammar` does not exist (disk-confirmed). Un-budgeted-LOC caveat carried.
- F9: shared-trait grep is test-excluded (`grep -v 'tests.rs|#[cfg(test)]'`), mirroring the
  phantom-grep exclusion — closes the `#[cfg(test)] impl SharedValueTrait for CssTestNode`
  false-green seam. This is precisely the hidden-coupling-via-test-impl CH5 guards.
- F7: both-impl grep is NECESSARY-NOT-SUFFICIENT; JSON `get(key)`+`Kind`+visitor must remain
  reachable THROUGH the trait — the preserve-rich-ast / LCD-flatten guard. The trait is an
  abstraction over both depths, not a lowest-common-denominator collapse.
- Pre-blocks: no eager tree, no StructRegistry, Lock 1 (one substrate, trait rides the
  existing `Tape`), zero-cost (no vtable in hot path).
Every CH5 axis on G4 — phantom resolution on the RIGHT axis, no-fork via shared trait, no
LCD-flatten, no second substrate, no eager tree — is structurally gated. **ACCEPT.**

**E.3 B4 no-orphan-kernel / Sheets-via-generator / no-second-substrate.** B4's
same-wave-consumer rule ("a kernel with no admission-path consumer is RETIRED") forecloses
the V5 orphan-kernel hidden coupling; the acceleration-wiring gate requires admission not
`#[cfg(test)]`; Sheets rides the existing tape (no third substrate); the distinct-grammar-
output gate is md5-distinct + grammar-neutral-body + the F13 row-count structural check.
The honest-finding escape is itself gated (cross-cutting 2: a "primitive" that is a
relabeled blob without `.bbnf`-invocation+parameterization+reference is REJECTed) — closing
the paper-close hidden coupling. **ACCEPT.**

**E.4 F13 relocated-seam attribution (the V3 cross-artefact REVISE touching αE:185).** αE
folds F13 in place across all five touched sites + cross-cutting 5: the xtask arm-census
grep catches only a SELF-DISCLOSING grammar-token branch; the neutral-identifier data-table
relocation is caught STRUCTURALLY by the P3 `sort -u` row-count check, NOT the regex. This
is the correct re-attribution of a machine-check that was over-claimed; the hidden-coupling
threat (per-grammar branching relocated into neutral DATA) is now policed by the only check
that can see it. F14 (the αD-only "18") is correctly carried as a no-op (αE already
count-correct in four places). **ACCEPT.**

**αE tally: ACCEPT ×4, REVISE ×0, REJECT ×0.** (Both open V3 REVISEs — F13 architectural
attribution, F14 no-op — folded; zero orphan on αE.)

---

## §6 — SYNTHESIS + HANDOFF (the α-F output) — CH5 disposition

The binding contract. CH5-bearing: the G3/G4 close gates, §0.4 pre-blocks (no second
substrate, hidden-coupling escapes), Section 2 telemetry (the machine-checkable coupling
columns), the §0.A.5/§0.6 plane-honesty.

**F.1 G4 close gate (SYNTHESIS:254 / 0.3 / HANDOFF G4) — the phantom + shared trait +
no-LCD + no-second-substrate.** The gate is structurally complete on every CH5 axis:
- "The G4 target is the `G: EventGrammar` axis … (NOT the already-real `K=Kind` axis)" —
  the RIGHT-axis foreclosure, disk-confirmed.
- "The shared trait's existence is INDEPENDENT of the `<G>` phantom — deleting `<G>` and
  defining the trait are separable; do NOT couple the trait's shape to animating `<G>` (that
  would manufacture the very phantom we are deleting)." This is a sharp, correct CH5 guard
  against a coupling the implementer could otherwise introduce.
- "DELETE is the abrogate-before-patch DEFAULT (no `CssEventGrammar` witness exists)."
- `json_rich_navigation_preserved == true` — the LCD-flatten guard at telemetry level.
- "over the EXISTING `Tape`/`ValueRef` (no second substrate, Lock 1)."
**ACCEPT.**

**F.2 G3 single-emitter-path + relocated-seam structural check.** SYNTHESIS:253 retires the
fork, requires the canonical neutrality grep → 0 over codegen AND xtask, the grammar-named-
TYPE census → 0, AND the structural `runtime_target_rows_collapsed` row-count check for the
neutral-identifier data-table the regex cannot catch (CH2 V3 §8.1, folded). The three-surface
model is honest: each surface catches a leak class the others miss, and the contract states
this explicitly. No single-emitter dishonesty escapes. **ACCEPT.**

**F.3 §0.4 pre-blocks — no second substrate + hidden-coupling escapes.** SYNTHESIS:363-373 +
HANDOFF:219-236 carry the full hidden-coupling escape list (retained sidecars / sidecar
tables / sidecar event vectors / second tapes / public `UnionTape` / Track 1 ≡ Track 2
sidecars / wrong-plane comparator admission / cross-call classifier-state retention) and the
explicit "No second substrate: an introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor`
alongside the landed `Tape`/`ValueRef` is a Lock 1 type-ambivalence violation (REJECT)." This
is the complete CH5 pre-block surface, disk-confirmed (those types absent, `UnionTape` a
forbidden-token guard). **ACCEPT.**

**F.4 Section 2 telemetry — the machine-checkable coupling columns.** The
`phantom_generic_resolved ∈ {instantiated,deleted}` (the `G` axis, test-only does NOT
count), `shared_value_trait_instantiations >= 2` (NON-test, test-excluded),
`json_rich_navigation_preserved == true`, `emitter_fork_present == false`,
`generator_grammar_branch_count == 0`, `generator_grammar_type_count == 0`,
`runtime_target_rows_collapsed == true`, `verbatim_blob_present == false`,
`acceleration_at_admission` (NOT `cfg-test-only`) columns make every CH5 coupling axis a
per-row machine gate, and the gate consumer REJECTS each dishonest value explicitly
(SYNTHESIS:517-529). This is the strongest possible CH5 posture: the hidden couplings are not
just prose pre-blocks, they are gate-rejected telemetry. **ACCEPT.**

**F.5 plane honesty (Track 2 ≠ Track 1, materialization framing).** SYNTHESIS §0.6 + Section
2 retained CSS schema bind `css_typed_summary_equal` (EXACT, gate before speed),
`materialization_framing`, `corpus_in_timer`; the JSON typed plane is the guard not the bar.
No Track1≡Track2 dishonesty; the asymmetric CSS plane is disclosed per H1. **ACCEPT.**

**F.6 V4 fold — the x86 second-surface (CH5 C.5/F.7), the most consequential CH5 fold.**
SYNTHESIS:57-75 + 246 + 491 + HANDOFF:12-16,98-103 widen P1 crate-wide, redefine
`x86_tree_deleted` as "NO x86 surface anywhere in `bbnf-simd`," and move the verify grep
crate-wide. This was a literal "x86 gone" falsity (a hidden x86 carrier surviving a
`src/`-scoped grep); the fold closes it honestly and the net-LOC correction is acknowledged
as net-positive. **ACCEPT.**

**SYNTHESIS+HANDOFF tally: ACCEPT ×6, REVISE ×0, REJECT ×0.**

---

## §7 — Cross-cutting CH5 findings

1. **No second substrate is verified, not merely asserted.** The three forbidden second-
   substrate type names are absent from disk; `UnionTape` is a live forbidden-token guard;
   the only substrate is `Tape`/`ValueRef`/`PayloadArena` at `tape/`. The generalization is
   architecturally pinned to emit accessors over the EXISTING types. Lock 1 is preserved at
   the contract level AND structurally enforceable.

2. **The shared value trait does not fork.** G4's gate forbids LCD-flattening
   (`json_rich_navigation_preserved`), forbids a second substrate, makes the trait separable
   from the `<G>` phantom, and requires ≥2 NON-test production impls. A trait that flattened
   JSON to CSS's thinner surface, or that smuggled a second cursor type, or that coupled its
   shape to animating the phantom, is gate-rejected. The value-API isomorphism is a real
   abstraction over both depths, not a fork and not a collapse.

3. **The phantom `<G>` is instantiate-or-delete on the RIGHT axis.** Every artefact + the
   contract pin the phantom to the `G: EventGrammar` axis and explicitly forbid the two
   false-greens: (a) binding the already-real `K` axis and calling the phantom discharged;
   (b) the test-only `_proof_compiles::<JsonEventGrammar>` use standing in for a production
   instantiation. DELETE is the abrogate-before-patch default; INSTANTIATE is burden-of-proof
   creation (`CssEventGrammar` absent). This is the complete, correct phantom-generic posture.

4. **The relocated-overfit-seam re-attribution (V3 CH2 §8.1) is the one substantive
   architectural sharpening, and it is folded across the cohort.** A per-grammar branch
   relocated into a neutral-identifier `RuntimeTarget` DATA table is a hidden coupling that the
   token-based arm-census regex is syntactically incapable of catching; the cohort now correctly
   attributes the defense to the STRUCTURAL P3 `sort -u` row-count check (in αC §2.2/§3, αE F13,
   SYNTHESIS:480-481/G3 gate, HANDOFF invariant 5). The grep is kept as necessary-not-sufficient.
   No hidden coupling escapes between the grep's real reach and the structural check.

5. **No Track 1 ≡ Track 2 dishonesty; no sidecar producer.** The JSON plane is recognition-
   vs-recognition (αB §1.1), the typed plane is quarantined as conditional (αB §1.4 / αD DM1),
   the CSS fact-stream String is RETIRED and any re-land is named a Track1≡Track2 sidecar
   dishonesty (αC §2.3). The full hidden-coupling escape list is pre-blocked verbatim
   (SYNTHESIS §0.4 / HANDOFF).

6. **The acceleration-wiring + orphan-kernel hidden couplings are gated at admission.** A
   `_neon` label on a scalar passthrough, a kernel reached only under `#[cfg(test)]`, and an
   orphan kernel with no admission-path consumer are each gate-rejected
   (`acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}`, NOT
   `cfg-test-only`; same-wave-consumer mandatory). This closes the SK-V17 W3 overstatement at
   the contract level.

---

## §8 — Tally

CH5 reviewed six artefact-sections (αA, αB, αC, αD, αE, SYNTHESIS+HANDOFF). Every CH5
hidden-coupling axis in the dispatch — substrate-union Lock 1 preserved, no second
substrate, shared value trait no-fork, instantiate-or-delete the phantom `<G>` on the
right axis — is structurally gated AND disk-verified. The two open V3 REVISEs touching
CH5-bearing artefacts (CH5 C.5 the second x86 surface; CH2 §8.1 the relocated-seam
attribution) are concretely folded across αC/αE/SYNTHESIS/HANDOFF with zero orphan. No new
hidden-coupling defect is found at V4. All sections ACCEPT.

- αA: ACCEPT ×3
- αB: ACCEPT ×4
- αC: ACCEPT ×4
- αD: ACCEPT ×4
- αE: ACCEPT ×4
- SYNTHESIS+HANDOFF: ACCEPT ×6

Total: ACCEPT 25, REVISE 0, REJECT 0.

TALLY accept=25 revise=0 reject=0
