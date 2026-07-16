# LOCK-6 — THE ISOMORPHISM PLANE · pass-3 · ADVERSARIAL CRITIC 2 of 2

*Critic 2 weights the CROSS-LOCK view. The finding that dominates this pass is not a family
defect — it is that **the gestalt I am hardening (FAMILY-SPECS.md) is unverified against its own
on-disk evidence.** Convergence math computed from FAMILY-SPECS is computed against a stale
evidence set and MUST be re-derived. Every per-family % below is scored against the ACTUAL
prototype artifacts (proto-1-code/, PROTO-1.md, PROTO-2.md, research-D fold), not the synthesis
prose.*

---

## META-FINDING M1 — UNVERIFIED GESTALT (the headline; failure-mode: *unverified gestalt* + *prototype-evidence misuse*)

FAMILY-SPECS.md (mtime 15:26) and the two on-disk PROTO writeups (mtime 15:47 / 15:57) describe
**mutually contradictory realities**. The synthesis was frozen BEFORE the pass's two decisive
probes ran, and was never reconciled:

| item | FAMILY-SPECS §0/§1/§3 claims | actual on-disk artifact |
|---|---|---|
| PROTO-1.md | "6B's crown emit_classify RAN, byte-identical, monomorphism PASSED" | **6C WASM-CORE comparator** — RAN, `TS-native-home HOLDS`, **wasm-core FALSIFIED** |
| PROTO-2.md | "6C's wasm-scan comparator SCAFFOLDED but **UNRUN**, 4th deferral" | **6A MIRROR-TREE linter** — BUILT + RAN, cap==13, RED 2/2, real Node drive |
| §3 6C verdict | "scaffolded-but-unrun a fourth consecutive pass" | **FALSE** — PROTO-1.md ran it and produced a decisive falsification |
| §1 6A verdict | "linter STILL UNBUILT… single largest remaining SPEC_ONLY crown (Brief 2)" | **FALSE** — PROTO-2.md built and ran it |
| §6 Brief 1 / Brief 2 | "the two sharpest uncertainties for the **next** pass" | **both already discharged this pass** |

The `proto-1-code/emit_classify.rs` header self-labels "PROTO-1 / pass-3 probe 1 (family 6B)",
but the file **`PROTO-1.md` on disk is the 6C comparator** — so the 6B emit-machine's
measured run-record (the 0-diff byte counts, the `%HaveSameMap` pre/post results) **exists
nowhere as a standalone artifact**; it survives ONLY as FAMILY-SPECS §0-P1 prose. Under
Constitution vii (concrete artifact or SPEC_ONLY) and XL-H (every number names its base), 6B's
largest-crown-conversion has **no independent, base-pinned writeup** — the code is present, the
run-record is not. An agglomerator reading `FAMILY-SPECS + PROTO-1.md + PROTO-2.md` in good
faith would derive an *incoherent* plane state.

**This is not a downgrade of the work — more was accomplished than the synthesis claims (both
"next-pass briefs" got run). It is a downgrade of the SYNTHESIS as a trustworthy convergence
instrument.** REQUIRED before any convergence talk: reconcile FAMILY-SPECS against
PROTO-1.md/PROTO-2.md, give the 6B emit run its own base-pinned writeup, and recompute every
gap list. Until then the plane is **NOT eligible for a clean pass** regardless of family scores.

## META-FINDING M2 — SPLIT SUBSTRATE (failure-mode: *cross-lock contradiction*, XL-A)

The families are measured on **two different trees**:
- 6A/PROTO-2 ran on **top-level `crates/core`, `crates/bbnf-path`, `crates/bbnf-path-ts`** (verified: these dirs exist at top level; `skinny/crates/bbnf-path` does **not** exist).
- 6B/6C ground on **`skinny/crates/runtime`, `skinny/crates/bbnf-simd`** (skinny@HEAD).

XL-A is binding: **top-level `crates/` is LEGACY, archives at GN-0; skinny@HEAD is the
measurement substrate.** research-A/PROTO-2 mislabel top-level `crates/bbnf-path` as "the
greenfield tree" — it is not; the greenfield workspace is GN-1 and does not yet exist. Therefore
**6A's entire linter proof is measured on a tree slated for archival, with no skinny/greenfield
equivalent** — a wrong-artifact-risk under XL-A/XL-H, and a plane whose four families do not
share a substrate. This is unflagged in FAMILY-SPECS (§1 asserts the pair is "the ONE live
cross-language pair" without noting it lives in the legacy tree that GN-0 deletes).

---

## Family 6A — MIRROR-TREE · convergence **66%**

**Real state (PROTO-2, not FAMILY-SPECS):** the linter is BUILT and RAN — `cap == exemptions ==
13 = 4+2+1+2+4` on the clean tree (grep-derived, not fit), RED 2/2 on `BogusPayload`/`FooView`,
and a REAL `serde-wasm-bindgen`/Node drive (not the `compile_path_native` proxy). The #1
three-pass-undischarged directive IS discharged. Credit is due; this is a genuine build.

But the run **exposed a foundational defect the synthesis treats as a win**, and left the
family's own architecture (the LOCK-3 seam) unbuilt.

**Enumerated open gaps:**
1. **BIFURCATED RUST REFERENCE — the isomorphism target is ill-posed (new, blocking).** PROTO-2's
   AGREE=2/DIVERGE=3 root cause is that canonical `check_path_against_registry` leaves
   `TypeDesc::Named(_)` intact (→ `UnregisteredRule` for any descent through a `Named` boundary)
   while the cdylib resolves `Named` eagerly via `registry.layout(id)`. **The two RUST frontends
   are themselves not isomorphic.** "Rust↔TS isomorphism" is undefined until the owner names
   WHICH Rust behavior is canonical (offline checker vs eager proc-macro resolver). PROTO-2 files
   this as a *strengthening* of names-green/behavior-red; it is equally a **substrate-red the
   linter cannot see and 6D cannot adjudicate** (agreement≠correctness → LOCK-7 N+1, unrun). This
   is bigger than "routing forks"; the reference is forked. **Routed to LOCK-1/LOCK-3/LOCK-7,
   unresolved.**
2. **Legacy-tree substrate (M2).** The pair archives at GN-0; the skinny/greenfield equivalent is
   absent. The 13-exemption count is a fact about a tree being deleted.
3. **LOCK-3 provenance-manifest seam UNBUILT — the family re-homes provenance it was told not to
   own.** research-A Q3 splits LOCK-3-owns-`provenance.manifest` / LOCK-6-owns-linter. PROTO-2's
   linter **hardcodes T in `mirror.toml`** and classifies all decls itself — it does NOT consume a
   LOCK-3 manifest. This is exactly the tower-duplication research-A warned against (LOCK-6
   re-deriving grammar-provenance). The Class-1-PROVENANCE route is un-exercised end to end.
4. **Typed-`PathError` surface still un-driven.** PROTO-2 found `makePathTag`'s
   `try{…throw new PathError}catch{throw raw}` **swallows the typed error** — the drive parses
   `PathErrorPayload` out of the raw `JsError.message`, never through the typed `PathError` /
   `format`/`reasonText` presentation. The error-PRESENTATION isomorphism (the third limb's actual
   surface) remains behaviorally untested; the rebox is dead code.
5. **Class-closure `I=5` asserted, not stressed.** research-A task-4 ("attempt a sixth idiom
   class") was not run; `cap` is validated on one 13-name pair, never at tape/N-grammar scale.
6. CASE totality+injectivity (`next`/`next_sibling`→camelCase collision), file-tree SOFT-warn gate
   (e), and cross-repo F6 CI ownership — all still unbuilt.

**Why not higher:** the crown is built but on the legacy tree, the LOCK-3 seam that makes it a
two-lock design is absent, and the run surfaced a forked-reference that makes the very notion of
"the isomorphism" undefined until an owner/LOCK-7 verdict lands. ADVANCED, not converging.

---

## Family 6B — SPEC-GENERATED-SKELETON · convergence **70%**

**Real state:** the `emit_classify(spec, Backend)` machine exists (`proto-1-code/`) and — per
FAMILY-SPECS prose — emits JSON/Sheets/TS classify byte-identical with monomorphism preserved.
The mechanism is the strongest ground in the plane and is genuinely BUILT at the classify face.

**Enumerated open gaps:**
1. **NO STANDALONE RUN-RECORD (M1).** The 6B measured results have no base-pinned PROTO writeup on
   disk (overwritten by the 6C comparator). Constitution-vii/XL-H exposure: the "byte-identical +
   `%HaveSameMap` PASSED" numbers are synthesis assertion, not an independent artifact. Re-run and
   write it up before crediting.
2. **CLASSIFY FACE ONLY — the hard 96% is unbuilt.** 165 LOC→0 covers `classify` (27 TS LOC); the
   **walk/leaf/nav body (~598 remaining hand TS LOC)** is untouched, and the monomorphism gate must
   re-pass on the **full generated decoder**, not a 27-LOC fragment spliced into the hand harness.
   The load-bearing claim ("R3 not refuted, monomorphism by construction") is proven on the *easy*
   fragment; the megamorphizing risk lives in the walk (research-B R2's boxed-union counterexample).
   This is an **elegant-reduction**: "generation preserves monomorphism" reads
   *and-then-the-full-decoder*.
3. **R3 PayloadDecode axis unmodeled (research-B, under-foregrounded).** Sheets `as_f64`/`as_bool`/
   `as_tag` are three payload decoders keyed on rule; a `LayoutSpec` carrying only discriminator
   recovery **under-generates** — "skeleton generated" narrows to the discriminator only unless a
   `PayloadDecode` axis lands. FAMILY-SPECS mentions R2 but not this; it is the sharper
   expressibility risk.
4. **R2 flag-collision unbuilt.** One `u8` slot cannot carry a discriminator tag AND an escape bit;
   a string-escape leaf inside an operator tower collides. Resolvable by a spec-declared
   bit-partition, but unbuilt.
5. **Not landed in `xtask regen`; no full real-tape fp.** Standalone `rustc` binary; the twitter fp
   held via a classify-independent lane, NOT a full `doc.walk` (the SoA `tape_dump.rs` was not
   rebuilt). Lockstep-regen gate (a) is only PARTIALLY demonstrated.
6. **TS `emit_direct_sink` unbuilt.** Q3's negative-cost fused-sink win is measured **Rust-only**;
   the symmetric TS face is owed.
7. **Q4 VM instruction-stream — owed to LOCK-4C/LOCK-8C (cannot resolve in LOCK-6).** "The same
   machine generalizes to `OpcodeDecode` with no new machinery" is **asserted, not run**, and the
   GO/NO-GO depends on VM-vs-closure (LOCK-4C) + interpreter-vs-closure under V8 (LOCK-8C), both
   unmeasured. A live cross-lock double-home, correctly flagged, unresolved.
8. **Face-I flag-bit VALUES are LINTED not generated** — the same face 6A's (unbuilt-on-skinny)
   linter must own; the 6A⊥6B partition on that face is drawn on paper but neither side is built on
   the shared substrate.

**Why not higher:** the crown is real but confined to the classify face; the full-decoder scale
(where monomorphism actually risks deopt), R3/R2, xtask landing, and the TS symmetric face are all
open, and the run-record itself is missing.

---

## Family 6C — WASM-CORE · convergence **66%** (as BANKED)

**Real state (PROTO-1.md, RAN — reverses FAMILY-SPECS §3 "unrun 4th time"):** the v128 kernel +
cold Chrome-150 harness ran. Parity holds on every doc. Verdict: **`TS-native-home HOLDS`,
wasm-core-home FALSIFIED** at bbnf batch doc sizes. The kernel wins 4.7–7.3× on `scanOnly`, but
**marshalling (JS-string→linear-memory `TextEncoder`+copy) is 72–90% of `wasmFull`** on real docs
and swamps the win below ~2 MB. Net win only on multi-MB ASCII-heavy branch-hostile input (canada).
This is a decisive advance and it **firms BANKED**.

But it also **weakens the §5 stratification spine and falsifies research-C's own prediction:**
1. **The §5 PRIMARY graft (6B⊗6C zero-copy crossing) is now measured-TENSE.** PROTO-1 shows the
   crossing of a **JS-string source** into wasm is a **net loss <2 MB** — and bbnf browser source
   arrives as a JS string (fetch/editor). The crossing only pays when bytes are **already resident
   in wasm linear memory**, which requires the whole parse→tape pipeline in wasm, i.e.
   **6C-as-primary — which is REFUTED (pass-2 R-A + F1).** So "6B generates the contract, 6C crosses
   it zero-copy" is economically self-tensioned: cheap crossing ⇒ 6C-primary ⇒ refuted. FAMILY-SPECS
   §5 still lists this graft as a clean compatibility. **The spine needs re-drawing given PROTO-1.**
2. **research-C Q1 prediction FALSIFIED by its own probe.** research-C predicted "wasm wins
   skip-heavy, loses dense" (movemask-emulation axis). PROTO-1 shows the axis is **marshalling**
   (ASCII vs UTF-16-backed encode cost + branch-hostility), NOT skip-heaviness — the kernel wins
   everywhere on `scanOnly`. A research prediction refuted by the probe it motivated; the 6C
   narrative must be recut.
3. **Grow-hazard two-parse case STILL unexercised.** PROTO-1's static `bss` arena never grows;
   research-C Q2's `memory.grow` detach corruption (two parses through one growing instance) is
   confirmed by spec but **not run** — the copy-out margin claim (≥2× headroom) is inherited from
   PROVE-14, not re-measured under grow.
4. **value.js divorce owner-bound (XL-G P1).** 4.0.0 (`tranche-u` WIP) imports zero parse-that; the
   confinement oracle re-homes to latex-paper (still imports). Cdylib pricing (one consolidated vs
   two instances) rests on an unratified branch.
5. **Homing collision unpriced.** `wasm/Cargo.toml` already claims `name="bbnf-wasm"`; proposed
   `crates/bbnf-wasm` absent.
6. **9A/9B kernel-home interlock + LIVE x86 in tree.** `6C.wasm-scan ⇐ 9A.kernel-home` unresolved;
   `simd-scan/avx2.rs`/`avx512.rs` violate Constitution iv ("x86 dead"), routed to gestalt, still
   present.

**Why not lower/higher:** BANKED is the right fate and PROTO-1 REINFORCES it (the crossing loses
for the real JS-string case). But the pass's own probe undercuts the §5 spine that the plane's
stratification depends on, so 6C's *contribution to the plane* is now more contested, not less.

---

## Family 6D — ORACLE-ISOMORPHISM (the FLOOR) · convergence **66%**

**Real state (research-D):** the cross-language fold RAN — bits lane bit-identical
(`b99874e8`), native-decimal diverges on 3 enumerable classes; injected-divergence static 4/5,
latency 1. The never-run mechanism ran at corpus scale. Floor role confirmed.

**Enumerated open gaps:**
1. **IN-FILE CONTRACT CONTRADICTION (sharper than FAMILY-SPECS's D2).** The checked-in
   `contract-6D.toml [in]` sets `number_fold = "f64 LE-byte mix"` (**the BITS lane**), while
   research-D's own recommended `oracle-contract.toml` sets `[canonical.number] form =
   "contract-normalized", exponent = "never", zero_sign = "positive"` (**the DECIMAL lane**, bits
   as `alt_lane` only). The frozen artifact and the research that justifies it **commit to opposite
   primaries** — on top of the cross-pass bit→span→bit flip (three representations, three passes).
   A "mandatory floor" whose number-lane contract contradicts itself **within the checked-in file**
   is not a settled floor. Routed to GESTALT is acceptable, but LOCK-6 must NOT present 6D as
   converged while its own contract is self-inconsistent.
2. **R5 debuggability UNRUN — the verdict-deciding number (elegant-reduction).** 6D's entire
   cost/benefit case against 6A/6B reduces to "time-to-root-cause under a bare digest mismatch vs
   mirror-maintenance cost." That is **the** axis where 6D is dominated (a root-digest mismatch says
   "something differs," not "escape decoder off-by-one at line X") and it is **unrun**. The
   null-hypothesis-defeat verdict cannot be settled without it. Everything measured this pass
   (marginal-fold-LOC≈0, catch rates) is a *proxy* for the number that actually decides.
3. **Fuzzer teeth REASONED, not RUN.** research-D Q2's "coverage-fuzzer 5/5" is explicitly reasoned;
   only static 4/5 and latency-1 ran. The generator is unbuilt. The empirical case for the fuzzer
   (bool-collapse escapes a bool-free corpus forever) is a hypothesis. SPEC_ONLY dressed as a number
   in FAMILY-SPECS §0-D1 ("coverage-fuzzer 5/5").
4. **u32 FALSE-GREEN risk (a gate that silently passes).** The fold ran at u32; a u32 `fp` collides
   at ~77k inputs (birthday). At fuzzer scale (10⁶+) two DIFFERENT value trees can digest identically
   → gate-(a) passes on divergent input. `digest="u64"` is spec text in the toml, **not a diff**.
5. **agreement≠correctness hole, made CONCRETE.** 6A/PROTO-2's bifurcated Rust reference is exactly
   the both-engines-disagree case 6D cannot adjudicate; LOCK-7's N+1 reference is unrun. The floor
   has a known, now-demonstrated hole.
6. **No non-JSON end-to-end fold.** The "O(new variants) not O(new grammar)" affirmative case (K=0
   for CSV) is asserted, not run; no TOML/CSV value tree was folded.

**Why not higher:** the fold ran (real), but the floor's own contract is internally contradictory,
the one number that decides its cost/benefit is unrun, its headline fuzzer number is reasoned, and
its correctness hole was just demonstrated by a sibling probe.

---

## §5 incompatibility map / stratification — cross-lock hardening

- **The stratification table is stale.** It lists 6C's comparator as unrun (PROTO-1 ran it) and
  6A's linter as UNBUILT/Brief-2 (PROTO-2 built it). The whole §5 rests on "one MEASURED number
  (6B classify face) plus one still-owed number (6A ratio)" — but the 6A ratio **is now measured**
  (PROTO-2, 13 exemptions), just on the legacy tree. Re-derive §5 against the actual runs.
- **The PRIMARY graft (row: tape layout = 6B generates + 6C crosses zero-copy) is measured-tense**
  (6C gap-1). The stratification's spine is the weakest-evidenced row post-PROTO-1, not the firmest.
- **XL-D tension unnamed.** XL-D ratifies the build-time emitter as the fast TS face and demotes
  *runtime decoders* to ORACLES. 6C/PROTO-1's `TS-native-home` verdict sits on the PROVE-14
  **runtime tape-decoder walk** — a runtime decoder. Whether the tape-decoder-walk is "the fast
  face" (6C) or "an oracle" (XL-D) is unreconciled; FAMILY-SPECS never distinguishes tape-decoder
  from combinator-runtime-decoder. Name it.
- **XL-C regex exemption not free under 6C.** 6C claims "under 6C the regex exemption dissolves
  (Rust DFA→wasm, no TS-`RegExp` mirror)." PROTO-1's marshalling finding means regex-in-wasm pays
  the SAME encode+copy tax for small/cold patterns (latex-paper's usage) — the "dissolves favorably"
  claim inherits PROTO-1's <2 MB loss and is unpriced.
- **DAG gating is correct but total.** 6A freezes after LOCK-1+LOCK-7; 6B after LOCK-1+LOCK-7 + owes
  Q4 to LOCK-4/8; 6C after LOCK-1/5/7/9; 6D after LOCK-1+LOCK-7. LOCK-6 is structurally gated on
  six other locks — the plane cannot pass 100% until those land, which is right, but means the
  ceiling this pass is a *rigorously-converged core with exact gaps*, never a clean-pass close.

---

## Verdict

| family | disposition | convergence | gates that cannot yet fire |
|---|---|--:|---|
| 6A | ADVANCED · crown BUILT (on legacy tree) | **66%** | forked Rust reference (LOCK-7); LOCK-3 seam unbuilt; typed-error surface un-driven |
| 6B | ADVANCED · crown BUILT (classify face; no run-record on disk) | **70%** | full-decoder monomorphism; R3 PayloadDecode; xtask; VM→LOCK-4/8 |
| 6C | BANKED · reinforced by PROTO-1 | **66%** | §5 spine tension (zero-copy⇒6C-primary⇒refuted); grow-hazard; x86 in-tree |
| 6D | ADVANCED · the FLOOR · fold RAN | **66%** | in-file bits⊥decimal contract; R5 unrun; fuzzer reasoned; u32 false-green |

**Plane-level:** the pass accomplished MORE than its own synthesis records (both "next-pass
briefs" ran), yet is LESS converged than a naive "crown BUILT" read suggests — the runs exposed a
forked reference (6A), a tense spine (6C), and a self-contradictory floor contract (6D). **The
gestalt is not eligible for a clean pass until M1 (reconcile FAMILY-SPECS with the on-disk
prototypes; give the 6B emit run a base-pinned writeup) and M2 (name the legacy-vs-skinny
substrate split) are discharged.** Aggregate: a rigorously-converged CORE (~67%) with the exact
gaps enumerated above — advance to a reconciliation pass, not to GESTALT.
