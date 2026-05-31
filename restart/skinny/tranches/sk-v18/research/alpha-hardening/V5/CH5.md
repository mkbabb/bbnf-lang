# CH5 — HIDDEN-COUPLING (lens, cycle V5) — SK-V18 Pass-Alpha artefact review

Lens: CH5 Hidden Coupling (PASS-ALPHA §3 / ORCHESTRATOR §3W). Cycle: V5.
HEAD of record: `318d9c046` (SK-V17 closed `f6a38445b`; V3 audit `7dbe44c22`).
Scope per the dispatch: substrate-union Lock 1 preserved; no second substrate; shared
value trait does not fork; instantiate-or-delete the phantom `<G>`. Plus the standing CH5
mandate — no parallel substrate / sidecar producer / renamed-scanner Lock-1 violation /
Track 1 ≡ Track 2 dishonesty; typed product plane gates structurally honest (Track 2 ≠
Track 1); the addenda (verbatim-blob, distinct-grammar-output, single-emitter-path,
phantom-generic, timed-plane-symmetry+corpus-in-timer, acceleration-wiring).

Artefacts reviewed: `research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md` +
`SYNTHESIS.md` + `HANDOFF.md` (the α-F output per PASS-ALPHA §2/§6; no separate
`alphaF.md`). V5 is the redress cycle: the five V4 REVISE clusters (V4/CONSOLIDATED) fold
in. CH5 was 100% (25/0/0) at V4; this cycle re-verifies the core CH5 axes survive the
fold AND audits the one V4 fold that lands directly in the CH5 domain — the crate-wide P1
x86 deletion reach (CH6 V4 §1 + CH5 V3 §C.5/§F.7).

## §0 — Live ground-truth re-verification (re-grepped at HEAD `318d9c046` THIS cycle)

| Claim | Command / inspection | Result |
|---|---|---|
| Phantom `G` vs real `K` axis | `grep -n 'pub struct ValueRef' …/tape/mod.rs` | `:175 pub struct ValueRef<'doc,'input:'doc, K = AnyKind, G: EventGrammar = AnyGrammar>` — TWO axes; `K` real, `G` phantom. **Two-axis distinction confirmed.** |
| No second substrate (type level) | `grep -rln 'StructLayout\|TapeStructBuilder\|TapeCursor' …/runtime/src …/codegen/src` | **empty** — the pre-block guards re-introduction correctly. |
| `UnionTape` forbidden-token guard, not a live type | `grep -rn 'UnionTape' …/codegen/src/lib.rs …/runtime/src` | only `lib.rs:1329 "UnionTape",` (forbidden-token list) — NOT a live second substrate. |
| Forked emitter live | `grep -rn 'enum RuntimeEmitterKind' …/codegen/src` | `grammar_provider.rs:40` — the JSON-vs-CSS fork is the G3 subject, live. |
| `CssEventGrammar` absent (G4 INSTANTIATE = creation) | `grep -rln 'CssEventGrammar' …/runtime/src` | **empty** — DELETE is abrogate-before-patch default. |
| Shared value trait absent (value API divergent) | `grep -rln 'SharedValueTrait\|trait Value\b\|trait Cursor\b' …/runtime/src` | **empty** — confirms divergent API, no shared trait, the G4 surface. |
| **P1 verify-grep firing surface (the V5 fold domain)** | `grep -riE --include='*.rs' --include='Cargo.toml' 'avx\|gfni\|sve\|x86\|nasm' …/bbnf-simd/` (the SYNTHESIS-scoped grep, `-l`) | **5 files**: `Cargo.toml`, `build.rs`, `src/lib.rs`, **`src/scalar/byte_class_from_eq_set_64.rs`**, **`tests/checkasm_parity.rs`** (+ the `src/x86_64/` tree). |
| `tests/checkasm_parity.rs` active x86 coupling | `grep -cE 'x86_64' …/tests/checkasm_parity.rs`; `:454-482` | **11 active references** — `bbnf_simd::x86_64::avx2::classify::classify_block_scalar`, `avx2::bmi2_emit::compact_mask_scalar`, `avx2::prefix_xor::prefix_xor_scalar`, `avx512_vbmi2::classify::classify_block_scalar`, `avx512_gfni::classify_affine::classify_block_scalar`. **Compile-coupled to `pub mod x86_64`.** |
| `src/scalar/byte_class_from_eq_set_64.rs` x86 hits | `grep -inE 'avx\|x86' …` `:10,12,15` | comment-only ("vector body (NEON, AVX-512 BW)…", "asmjson … AVX-512 BW", "AVX2 path"). Grep fires; benign body. |

All six core CH5 pins anchored to disk truth. The seventh + eighth rows are the live
re-grep of the P1 fold's firing surface — the load-bearing new evidence for §1 below.

---

## §1 — SYNTHESIS + HANDOFF (the α-F binding output) — CH5 disposition

The binding contract. CH5-bearing: the G3/G4 close gates, §0.4 pre-blocks (no second
substrate), Section 2 telemetry coupling columns, plane honesty, and the V4-folded
crate-wide P1 x86 deletion (CH5 V3 §C.5 / CH6 V4 §1, which lands in the CH5 domain).

**F.1 G4 close gate — phantom + shared trait + no-LCD + no-second-substrate. ACCEPT.**
SYNTHESIS:323 / §0.3 row :555-557 / HANDOFF:369 are structurally complete on every CH5
axis: the `G: EventGrammar` axis is the target "(NOT the already-real `K=Kind` axis …
`tape/mod.rs:175`)" — the RIGHT-axis foreclosure, disk-confirmed; "the shared trait's
existence is INDEPENDENT of the `<G>` phantom — deleting `<G>` and defining the trait are
separable; do NOT couple the trait's shape to animating `<G>` (that would manufacture the
very phantom we are deleting)" — a sharp, correct guard against an implementer-introduced
coupling; "DELETE is the abrogate-before-patch DEFAULT (no `CssEventGrammar` witness)";
`json_rich_navigation_preserved == true` (LCD-flatten guard at telemetry level, :557);
`shared_value_trait_instantiations >= 2` NON-test (:556, test-excluded `grep -v
'tests.rs|#[cfg(test)]'`); "over the EXISTING `Tape`/`ValueRef` (no second substrate, Lock
1)." Disk confirms the substrate is singular and `CssEventGrammar`/shared-trait absent.

**F.2 G3 single-emitter-path + three-surface model. ACCEPT.** SYNTHESIS:322 retires
`RuntimeEmitterKind` (`grammar_provider.rs:40`, disk-live), requires (i) the arm-census
over the FULL canonical alphabet across codegen AND xtask, (ii) the grammar-named-*type*
census (`generator_grammar_type_count == 0`, :554), AND (iii) the STRUCTURAL
`runtime_target_rows_collapsed` row-count check — now WIDENED (V5 F16) to all non-path
columns (`fact_schema`/`row_id`/`output_plane`/`emitter`/`entry_rule`/`source_roots`/
`check_command`/`frontend_requirements`), closing the relocated-into-a-neutral-data-table
seam the regex is syntactically incapable of catching. The three surfaces are honestly
disjoint; each catches a leak class the others miss; the contract states this. The V4
projection-tuple defect (CH2 V4 §8.1) is correctly folded (:553, :592). No single-emitter
dishonesty escapes.

**F.3 §0.4 pre-blocks — no second substrate + hidden-coupling escape list. ACCEPT.**
SYNTHESIS:441 + §0.4 carry the full escape list (retained sidecars / sidecar tables /
sidecar event vectors / second tapes / public `UnionTape` / Track 1 ≡ Track 2 sidecars /
wrong-plane comparator admission / cross-call classifier-state retention) and "No second
substrate: an introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the
landed `Tape`/`ValueRef` is a Lock 1 type-ambivalence violation (REJECT)." Disk-confirmed:
those types absent, `UnionTape` a forbidden-token guard. Complete CH5 pre-block surface.

**F.4 Section 2 telemetry — the machine-checkable coupling columns. ACCEPT.**
`phantom_generic_resolved ∈ {instantiated,deleted}` (the `G` axis, test-only does NOT
count, :555), `shared_value_trait_instantiations >= 2` NON-test (:556),
`json_rich_navigation_preserved == true` (:557), `emitter_fork_present == false` (:551),
`generator_grammar_branch_count == 0` (:552), `generator_grammar_type_count == 0` (:554),
`runtime_target_rows_collapsed == true` (:553, full-tuple), `verbatim_blob_present ==
false` (:550), `acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}`
NOT `cfg-test-only` (:562) — every CH5 coupling axis is a per-row machine gate, REJECTed at
the consumer (:589-601). Strongest possible CH5 posture: hidden couplings are gate-rejected
telemetry, not merely prose.

**F.5 plane honesty (Track 2 ≠ Track 1). ACCEPT.** §0.6 + Section 2 bind
`css_typed_summary_equal` (EXACT, gate before speed), `materialization_framing`,
`corpus_in_timer`; the JSON typed plane is the guard not the bar (yyjson/asmjson/RapidJSON
honest-`None`-on-aarch64, :16). No Track1≡Track2 dishonesty; the asymmetric CSS plane
disclosed per H1.

**F.6 — V5 P1 x86 deletion-list reach: `tests/checkasm_parity.rs` +
`src/scalar/byte_class_from_eq_set_64.rs` ESCAPE the binding deletion list while the
binding verify grep fires on them. REVISE.**

This is the one CH5 defect at V5, and it is in the CH5 domain (the V4 fold's reach is a
hidden-coupling question). The V4→V5 fold (CONSOLIDATED Cluster 2) widened the binding P1
deletion list to (a) `src/x86_64/`, (b) `ext/x86/`, (c) `build.rs`, (d) `lib.rs:247` ref,
(e) `Cargo.toml:19` nasm dep, (f) `lib.rs:5 pub mod x86_64;` + `:285-288` dispatch arms, (g)
in-crate doc surfaces (scrubbed OR grep-scoped). The V5 binding verify grep is
`grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm'
skinny/crates/bbnf-simd/` (SYNTHESIS:315/:563, HANDOFF:110), and the contract asserts
"every active hit the grep flags is on the (a)-(g) removal list" / "the deletion list is
reach-matched to the verify grep so the gate is satisfiable-by-construction."

Re-grepped LIVE at HEAD this cycle, that exact grep (`-l`) fires on **5 `.rs`/`.toml`
files**, of which **TWO are NOT on the (a)-(g) list**:

1. **`tests/checkasm_parity.rs`** — `grep -cE 'x86_64'` = **11**; `:454-482` actively `use`
   the module that target (f) deletes (`bbnf_simd::x86_64::avx2::classify::
   classify_block_scalar`, `…avx2::bmi2_emit::compact_mask_scalar`,
   `…avx512_vbmi2::classify::…`, `…avx512_gfni::classify_affine::…`). This is the
   consequential surface: it is **compile-coupled** to `pub mod x86_64`. Executing exactly
   (a)-(g) deletes `pub mod x86_64;` (f) and `src/x86_64/` (a), and `checkasm_parity.rs`
   **fails to compile** — an unnamed coupling between the P1 deletion and the test harness
   the binding contract does not surface. Independently, the grep stays RED on this file's
   11 `x86_64` hits → the gate is **RED-by-construction**, the exact mirror-defect CH6 V4
   §1 claimed to close (a deletion list narrower than the grep), re-incurred one reach
   level deeper (the V4 fold audited `src/`, `ext/`, `build.rs`, `Cargo.toml`, `lib.rs` but
   never `tests/`).

2. **`src/scalar/byte_class_from_eq_set_64.rs`** — `:10,12,15` carry comment-only
   `AVX-512 BW`/`AVX2` cross-references in the scalar reference's doc. Benign body, but the
   `--include='*.rs'` grep fires → the gate stays RED on it unless it too is scrubbed or
   the grep is narrowed. Not named on (a)-(g).

αC (the research feeder) is **reach-complete here** — αC:168-169, αC:179, and αC:196
EXPLICITLY name dropping "the residual x86 strings in `src/scalar/byte_class_from_eq_set_64.rs`
+ `tests/checkasm_parity.rs`" (step 4) and scope the close-gate grep over "`src/`, `ext/`,
`build.rs`, `Cargo.toml`, `tests/`." The defect is **propagation**: αC's `tests/` +
`scalar/` reach was NOT carried into the BINDING SYNTHESIS P1 row (:315), the
`x86_tree_deleted` telemetry (:563), or the HANDOFF P1 receiver (:101-112). The binding
inventory-of-record is therefore narrower than its own grep, AND silent on the
compile-coupling.

**Why REVISE not REJECT:** direction correct (delete the whole x86 surface; verify
crate-wide-AND-`tests/`-inclusive), zero architectural re-open, and the fold is mechanical
— αC already carries the verbatim text to propagate. **Concrete fix (propagate αC:168-179
verbatim into the binding rows):** add to the SYNTHESIS P1 deletion list (:315) + the
`x86_tree_deleted` telemetry (:563) + the HANDOFF P1 receiver (:101-112) two removal
targets — **(h)** `tests/checkasm_parity.rs:454-482`: re-home or delete the x86_64
scalar-reference assertions (`avx2`/`avx512_vbmi2`/`avx512_gfni` `classify_block_scalar`
etc.) so the test does not import the deleted `x86_64` module (closes the compile-coupling
AND the grep RED); **(i)** `src/scalar/byte_class_from_eq_set_64.rs:10-15` doc-comment x86
cross-references scrubbed to aarch64-neutral. Then make the binding verify grep
**`tests/`-inclusive** (drop the implicit `src/`+manifest framing) so it matches αC:196 —
`grep -riE --include='*.rs' --include='Cargo.toml' … skinny/crates/bbnf-simd/` already
covers `tests/` by path, so the only correction is naming (h)+(i) on the removal list. With
(h)+(i) the list is genuinely reach-matched (5 firing `.rs`/`.toml` files, 5 named removal
targets) and satisfiable-by-construction.

**SYNTHESIS+HANDOFF tally: ACCEPT ×5 (F.1–F.5), REVISE ×1 (F.6).**

---

## §2 — αA results-extraction — CH5 disposition

αA §3.3 (phantom generic + divergent value API), §4 (substrate / Lock 1), §3.1
(fork/replica/verbatim-blob), §6 (caveats) are the CH5-bearing sections.

**A.1 phantom-generic two-axis + §4 no-second-substrate + §3.1 fork/replica/blob + §6
caveats. ACCEPT ×3.** αA names the `ValueRef` four-slot decl with `K` real vs `G` phantom
and forecloses the false-green "binding `K` discharges the phantom"; carries the
no-second-substrate pre-block (`StructLayout`/`TapeStructBuilder`/`TapeCursor` absent,
disk-confirmed); states the fork (`grammar_provider.rs:40`), the 7 byte-identical replicas
(single md5), the const-`&str` CSS blob, and the NEON `#[cfg(test)]` honesty with citation;
the §6 caveat forecloses the Track1-not-in-RESULTS / broadcast confusion and the
simdjson/yyjson honest-`None`. No fabricated coupling, no missed coupling in scope.

**A.2 αA P1 x86 reach (the V4 cross-cohort fold).** The V4 CONSOLIDATED records αA was
crate-wide-redressed (prior "V5 R-1"; residual `find …/src/x86_64` mentions describe the
OLD gate as the defect). αA is a research feeder, not the binding inventory; the F.6 reach
gap is filed against the BINDING SYNTHESIS/HANDOFF rows, not αA (αA does not author the
removal list). No CH5 defect inherited here. ACCEPT (covered in the ×3).

**αA tally: ACCEPT ×3, REVISE ×0, REJECT ×0.**

---

## §3 — αB competitor-deltas — CH5 disposition

CH5-bearing: two-plane comparator framing (Track 1 vs Track 2) + honest-`None` foreclosure.

**B.1 plane symmetry / Track-1≠Track-2 honesty. ACCEPT.** The JSON plane is near-symmetric
recognition-vs-recognition (sonic skipper builds no owned tree; bbnf `parse_only` builds
none either); the CSS plane is ASYMMETRIC lazy-vs-eager, disclosed per H1; the Track 2 typed
rows are QUARANTINED as conditional-on-hand-tuned-schema and explicitly forbidden as the
preservation bar (the unconditional bar is `parse_only` ↔ sonic-strict). Exactly the
Track-2≠Track-1 structural honesty CH5 demands.

**B.2 honest-None / no sidecar-competitor. ACCEPT.** yyjson/asmjson/RapidJSON honest `None`
on aarch64 ("a column populated with an un-run engine's number would be a contrivance"). No
corpus-in-timer / fabricated-competitor coupling.

**B.3 Sheets no-competitor-bar. ACCEPT.** Sheets' bar is GENERATION not throughput; no
fabricated SOTA-Sheets comparison.

**B.4 x86-comparator-OUT vs implementation-x86-scope boundary. ACCEPT.** αB isolates its
comparator-OUT statement (asmjson AVX-512 OUT) from the implementation-side P1 scope; αB
makes no "x86 gone" close-claim, so it inherits no F.6 orphan.

**αB tally: ACCEPT ×4, REVISE ×0, REJECT ×0.**

---

## §4 — αC redress-digest — CH5 disposition

The most CH5-dense artefact (pre-block families + relocated-seam + the P1 x86 obligation).

**C.1 P1 x86 obligation — reach-COMPLETE including `tests/` + `scalar/`. ACCEPT.** αC §0.A.1
/ §1-P1 (`:155-199`) scope the deletion crate-wide AND its step (4) (`:177-179`) names
dropping "the residual x86 strings in `src/scalar/byte_class_from_eq_set_64.rs` +
`tests/checkasm_parity.rs`," AND its close-gate (`:192-197`) scopes the verify grep over
"`src/`, `ext/`, `build.rs`, `Cargo.toml`, `tests/`." This is the reach-complete posture
the binding SYNTHESIS/HANDOFF rows MUST inherit (F.6) — αC is the count-correct,
reach-correct reference. The V2-FOLD same-commit `lock14_baseline.rs` tag/assertion desync
guard (`:181-185`) is carried. The dormancy rationale (REVISE-not-REJECT, `build.rs:38-40`
non-x86 early-return, no aarch64 consumer) holds.

**C.2 relocated-seam structural attribution. ACCEPT.** αC §2.2/§3 attribute the
relocated-seam defense to the STRUCTURAL P3 row-count collapse (genuinely-distinct `.bbnf`
↔ non-identical `generated.rs`), with the arm-census grep demoted to
NECESSARY-NOT-SUFFICIENT — the correct CH5 call (a per-grammar branch relocated into a
neutral-identifier `RuntimeTarget` DATA table is a coupling a token regex cannot catch).
The V5 F16 projection-tuple widening (all non-path columns) is folded.

**C.3 §2.1–§2.6 pre-block families. ACCEPT.** Each re-open test checked TWICE (runtime
output AND emitter): AZ-IV eager (§2.1), StructRegistry/Builder<G> per-leaf (§2.2), CSS
fact-stream String / Track1≡Track2 sidecar (§2.3, "A retained String product is also a
Track1==Track2 / sidecar dishonesty"), 24-broadcast (§2.4), FNV/fixture (§2.5),
x86/AVX/SVE/nasm (§2.6, now crate-wide). The §2.2b witness-`EventGrammar`-type-literal
clause (codegen-emitted `ValueRef<…,XEventGrammar>` string is a grammar-name leak P4 cannot
catch) is the exact hidden-coupling-through-codegen escape. Disk-confirmed: no second
substrate, no live `UnionTape` type, no `CssEventGrammar` to splice.

**C.4 no-second-substrate / substrate-union. ACCEPT.** §3+§4 bind Lock 1; the §2.2
different-framing admission requires the trait be "a thin read-cursor over the EXISTING
`Tape`/`ValueRef`, NOT a generic `Builder<G>`."

**αC tally: ACCEPT ×4, REVISE ×0, REJECT ×0.** (αC is the reach-correct reference the
binding rows under-propagated; the defect is filed at F.6, not here.)

---

## §5 — αD validated/invalidated ledger — CH5 disposition

CH5-bearing: I5 (phantom `G`/divergent API), §5 pre-block, S9 G4 wave row.

**D.1 I5 phantom-`G` precision (test-only does NOT count). ACCEPT.** The ONLY non-default
`G` instantiations are `event_grammar_tests.rs:18,20,89` (test-coverage), ZERO production;
the phantom-generic gate must NOT accept the test-only use as a real instantiation —
disk-confirmed `CssEventGrammar` absent, so INSTANTIATE is creation not rename.

**D.2 S9 G4 — `G`-axis-only + no-LCD-flatten + no-second-substrate. ACCEPT.** G4 binds the
`G` axis ONLY; the `K` axis stays ("deleting it would destroy the typed-view machinery
`json/view.rs` rides"); §5 forbids a second cursor/builder type. Full CH5 G4 posture.

**D.3 §5 pre-block (eager-tree / registry / second-substrate behind the trait). ACCEPT.**
The shared trait stays lazy over the existing `ValueRef` (no eager tree, AZ-IV); G4 is a
trait abstraction not a registry; one substrate; fact-stream elevated to RETIRED-re-land-
is-REJECT.

**D.4 V5 fold (αD-only stale "18"→14). ACCEPT (no CH5 defect).** Not a coupling defect (a
CH1/CH4 count); folded clean per V4 CONSOLIDATED Cluster, leaves no orphan that could
mis-seed a gate.

**αD tally: ACCEPT ×4, REVISE ×0, REJECT ×0.**

---

## §6 — αE candidate-shortlist — CH5 disposition

CH5-bearing: B1 (single-emitter-path), B3 (shared trait + phantom), B4 (no-orphan-kernel /
Sheets-via-generator / no-second-substrate), F13/F16 (relocated-seam), F15 (P1 x86 scope).

**E.1 B1 single-emitter-path. ACCEPT.** Gate #3 requires
`RuntimeEmitterKind`/`CompiledLowering`/`RequestFacts` → 0 AND the canonical neutrality
grep → 0; entry-gated on P4 (the gate scans the emitter before it is built — closing the
blind-gate coupling).

**E.2 B3 shared trait + phantom `<G>`. ACCEPT.** DELETE is the DEFAULT (no `CssEventGrammar`
witness); the shared-trait grep is test-excluded (`grep -v 'tests.rs|#[cfg(test)]'`, closing
the `#[cfg(test)] impl SharedValueTrait for CssTestNode` false-green); the both-impl grep is
NECESSARY-NOT-SUFFICIENT (JSON `get(key)`+`Kind`+visitor must remain reachable THROUGH the
trait — the preserve-rich-ast guard); pre-blocks: no eager tree, no StructRegistry, Lock 1,
zero-cost. Every CH5 G4 axis structurally gated.

**E.3 B4 no-orphan-kernel / Sheets-via-generator / no-second-substrate. ACCEPT.** The
same-wave-consumer rule forecloses the V5-pattern orphan kernel; acceleration-wiring at
admission not `#[cfg(test)]`; Sheets rides the existing tape (no third substrate); the
distinct-grammar-output gate is md5-distinct + grammar-neutral-body + the F13/F16
row-count structural check; the honest-finding escape is itself gated (a relabeled blob
without `.bbnf`-invocation+parameterization+reference is REJECT).

**E.4 F15 (P1 x86 crate-wide) + F16 (projection-tuple). ACCEPT.** αE folds F15 (P1 row
`:94` widened crate-wide, exit gate `src/`-scoped → crate-wide, LOC −847 → ≈ −4500) and F16
(projection widened to the full per-grammar config-tuple). αE's P1 receiver (`:94`,
`:100-101`) carries the `src/x86_64/` + `ext/x86/` + `build.rs` + `Cargo.toml` + `lib.rs`
surfaces; the checkasm `12+2` count is correct (`:101,:201`). Note: αE's P1 receiver,
like the binding rows, does not separately name `tests/checkasm_parity.rs` as a
deletion/re-home target — but αE is a feeder shortlist, not the binding inventory, and αC
(the digest) IS reach-complete (§4 C.1); the propagation defect is filed once, at the
binding SYNTHESIS/HANDOFF rows (F.6). αE inherits no separate REVISE here.

**αE tally: ACCEPT ×4, REVISE ×0, REJECT ×0.**

---

## §7 — Cross-cutting CH5 findings

1. **The four core CH5 axes are structurally honest AND disk-verified at V5.** No second
   substrate (the three forbidden type names absent; `UnionTape` a forbidden-token guard;
   only `Tape`/`ValueRef`/`PayloadArena` exists). The shared value trait does not fork (G4
   forbids LCD-flatten via `json_rich_navigation_preserved`, forbids a second substrate,
   makes the trait separable from `<G>`, requires ≥2 NON-test impls). The phantom `<G>` is
   instantiate-or-delete on the RIGHT (`G: EventGrammar`) axis, with both false-greens
   foreclosed (binding `K`; test-only `_proof_compiles`). The relocated-overfit-seam is
   policed STRUCTURALLY by `runtime_target_rows_collapsed`, now widened to all non-path
   columns. Every one of these is a gate-rejected telemetry column, not mere prose.

2. **The single V5 CH5 defect is reach-propagation, not architecture (F.6).** The binding
   SYNTHESIS P1 deletion list / `x86_tree_deleted` telemetry / HANDOFF P1 receiver are
   NARROWER than their own crate-wide `--include='*.rs'` verify grep: the grep fires on
   `tests/checkasm_parity.rs` (11 active `x86_64::` imports — compile-coupled to `pub mod
   x86_64`, the V4 target (f)) and `src/scalar/byte_class_from_eq_set_64.rs` (comment hits),
   neither named on the (a)-(g) removal list. This is the EXACT RED-by-construction
   mirror-defect CH6 V4 §1 fixed, re-incurred one reach level deeper (`tests/` was never
   audited), PLUS a genuine hidden compile-coupling between P1's deletion and the test
   harness. αC (the digest, §4 C.1) is reach-complete and carries the verbatim fix; the
   defect is solely that αC's `tests/`+`scalar/` reach was not propagated into the binding
   inventory-of-record. Concrete fix in §1 F.6: add removal targets (h)
   `tests/checkasm_parity.rs:454-482` (re-home/delete the x86_64 scalar-ref assertions) +
   (i) `src/scalar/byte_class_from_eq_set_64.rs:10-15` (scrub doc x86 cross-refs), and make
   the binding verify grep `tests/`-inclusive per αC:196.

3. **No Track 1 ≡ Track 2 dishonesty; no sidecar producer.** JSON plane is
   recognition-vs-recognition (αB B.1), the typed plane quarantined as conditional (αB
   B.1 / αD DM1), the CSS fact-stream String RETIRED with any re-land named a Track1≡Track2
   sidecar dishonesty (αC §2.3). Full escape list pre-blocked verbatim (SYNTHESIS §0.4 /
   HANDOFF).

4. **The acceleration-wiring + orphan-kernel couplings remain gated at admission.** A
   `_neon` label on a scalar passthrough, a kernel reached only under `#[cfg(test)]`, and an
   orphan kernel with no admission-path consumer are each gate-rejected
   (`acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}` NOT
   `cfg-test-only`; same-wave-consumer mandatory).

---

## §8 — Tally

CH5 reviewed six artefact-sections (αA, αB, αC, αD, αE, SYNTHESIS+HANDOFF). The four core
CH5 hidden-coupling axes in the dispatch — substrate-union Lock 1 preserved, no second
substrate, shared value trait no-fork, instantiate-or-delete the phantom `<G>` on the right
axis — are structurally gated AND disk-verified at HEAD `318d9c046`. One V5 defect: the
binding P1 x86 deletion list under-propagated αC's reach, leaving
`tests/checkasm_parity.rs` (compile-coupled, 11 active imports) and
`src/scalar/byte_class_from_eq_set_64.rs` off the removal list while the binding grep fires
on both — a RED-by-construction gate + a hidden compile-coupling. REVISE (mechanical fold;
αC carries the verbatim fix; direction unchanged).

- αA: ACCEPT ×3
- αB: ACCEPT ×4
- αC: ACCEPT ×4
- αD: ACCEPT ×4
- αE: ACCEPT ×4
- SYNTHESIS+HANDOFF: ACCEPT ×5, REVISE ×1 (F.6 — P1 deletion-list reach: `tests/checkasm_parity.rs` + `src/scalar/byte_class_from_eq_set_64.rs`)

Total: ACCEPT 24, REVISE 1, REJECT 0.

TALLY accept=24 revise=1 reject=0
