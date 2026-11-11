# LOCK-6 — THE ISOMORPHISM PLANE: ADVERSARIAL CRITIC 2 of 2, pass 3 (against the 22:44 re-synthesis)

*Critic 2 weights the CROSS-LOCK view: where this lock contradicts or double-homes work against the
extant GN/PT codex and the other locks, and where the freshest running evidence has moved past the
synthesis. XL-A…XL-H are treated as settled fact; challenges route to the gestalts. Every number names
its artifact and its tree; base-pin `ce51c36fc` per XL-H. Load-bearing structural claims re-verified
against live HEAD (`d4a6be499`) this critique — results inline. This SUPERSEDES the 16:16 CRIT-2, which
critiqued the retired 15:26 synthesis.*

---

## 0. THREE PLANE-LEVEL FINDINGS THAT PRECEDE THE FAMILIES

### F-STALE-2 — the 22:44 synthesis is STALE against its own §9 briefs; both "pass-4" probes RAN 24–37 min after it froze. The F-STALE pattern RECURS one generation later.

The 16:16 CRIT-1/CRIT-2 caught the 15:26 synthesis being stale against `PROTO-1.md`/`PROTO-2.md`. The
22:44 FAMILY-SPECS.md explicitly supersedes that one and folds those in — but it then names §9's "TWO
SHARPEST PASS-4 PROTOTYPE BRIEFS," and **both ran inside this same pass directory, after the freeze**
(verified mtimes): FAMILY-SPECS **22:44** → `PROTO-1-shipping-kernel.md` **23:08** (Brief 1, 6C resident
lane) → `PROTO-2-6B.md` **23:21** (Brief 2, 6B full decoder). Two crowns move that the synthesis does not
carry:

- **6B (Brief 2 discharged, UPWARD):** synthesis §2 gap [1] says "the ~598-LOC full decoder is
  ungenerated" and the mono gate "becomes real only on the full generated decoder." `PROTO-2-6B.md` RAN
  exactly that: a `LayoutSpec`-generated **non-byte-identical** (sha-distinct, 337 lines changed) full
  `TapeDoc` decoder is V8-monomorphic (`%HaveSameMap` pre+post, all 29,573 `classify` results unboxed
  Smi, four hot sites maglev/turbofan no-deopt), folds to fp **986912547**/`3431570937`, and the gate is
  proven **non-vacuous** by a two-hazard negative control (post-ctor field → `HaveSameMap` flips;
  boxed-union classify → Smi-uniformity flips). This discharges the two 6B defects the 16:16 CRIT-1 rated
  most serious (low-information gate on the byte-identical direction; missing `gen_tape.mjs`; M1 now
  closed, emit byte-reproducible). Any registry cut against 22:44 under-rates 6B.

- **6C (Brief 1 discharged, SIDEWAYS — falsifies the synthesis's own flat prediction):** §3/§7/§9 record
  the resident-bytes lane as the "one surviving win … PREDICTED win, unmeasured," and §3 recites "v128
  wins **4.7–7.3× at every size**." `PROTO-1-shipping-kernel.md` measured the **shipping `crates/simd-scan`
  crate as-shipped** and CORRECTS the flat prediction to **bimodal**: **0.9× at 1 KB single**, ≤1.8× for
  all 1 KB configs, clearing ≥2× only from **~8 KB batched upward** (2.5–3.7×), twitter 631 KB = 2.6×. The
  earlier flat "no size crossover" (PROTO-1.md, 20:17) is disclosed there as an **artifact of a bespoke
  persistent-OUTPUT ABI** that elided per-call alloc + LUT rebuild. The §5 spine row "6C pays ONLY for
  wasm-resident bytes" must narrow to "**resident bytes AND ≥~8 KB batched**" — small browser docs are
  doubly disadvantaged (marshal tax + unamortized per-call cost).

**Not a design defect — the recurring bookkeeping-lag hazard.** A cut against 22:44 under-rates 6B and
mis-records 6C's resident win as a flat prediction when it is a measured bimodal.

### F-SUBSTRATE-DRIFT — live HEAD has advanced past the substrate pin; and one family's entire crown is off-substrate.

Live `git HEAD = d4a6be499`; `ce51c36fc` (the declared MEASUREMENT SUBSTRATE) is now a strict ancestor
(verified). Structural claims re-verified stable on live HEAD (below) — no wrong-artifact fire — but XL-H
means the substrate pin is stale for the gestalt and must be refreshed. Sharper: **6A is the ONLY family
whose running evidence is 100% off-substrate** — its live pair is legacy top-level
`crates/{core,bbnf-path,bbnf-path-ts}` (verified present; all archive at GN-0), and `skinny/crates/bbnf-path`
is **absent** (verified). 6B/6C/6D ground on skinny@HEAD; 6A grounds on a tree scheduled for deletion,
with no port to skinny or greenfield even attempted. Its whole `T=4` taxonomy may not survive the re-cut.

### F-VERIFY — load-bearing structural claims hold at live HEAD (Constitution-vii re-check).

`skinny/crates/codegen/src/general_descent/{view.rs 431, emit.rs 452}` = **883 LOC present** ✓.
`emit_classify`/`LayoutSpec` = **0 hits across `skinny/crates`** ✓ (the named spec machine is a
`pass-3/proto-1-code/` scratch probe, NOT a landed organ). `json_templates/view.rs` = **488 LOC present**
✓ (the hand template 6B proposes to fold/delete). `MaterializationDescriptor` seat live at
`passes/src/lib.rs:1132` ✓. The synthesis's self-verification is honest; I confirm it.

---

## 1. Family 6A — MIRROR-TREE · crown BUILT+RAN, but on a doomed tree, with a consumer-less cross-lock seam and an ill-posed target

**Credit.** The linter RAN (`PROTO-2.md`): `cap == exemptions == 13 = 4+2+1+2+4` computed independently
(not fit), REDs `BogusPayload`/`FooView` 2/2, and a real `wasm-pack --target nodejs` + `serde-wasm-bindgen`
+ Node drive through the actual `makePathTag`/`PathError` confirms names-green-behavior-red. A genuine
SPEC_ONLY→RAN conversion.

**Cross-lock and soundness defects (weighted for the gestalt):**

- **[CONSUMER-LESS SUBSTRATE, Y.13 — cross-lock].** 6A gate (c) claims the linter "consumes the LOCK-3
  provenance manifest end-to-end," and the seams note asserts the live `fixture.rs:53-90` two-origin
  `"statuses"` fork "the LOCK-3 provenance pass REDs day one." **No such manifest and no such pass exist** —
  the provenance manifest is an unbuilt LOCK-3 deliverable (`cargo xtask regen` byproduct, Q3 OWNER:
  LOCK-3), and `PROTO-2` classified idioms by a **local `syn` predicate**, consuming no manifest. The
  "clean producer/consumer seam" is asserted with the producer absent, and a live bug is dispatched to a
  pass that does not exist. The design's arbiter is external and unbuilt.

- **[gate fails-wrong — the fit-to-count literal is in the RUNNING artifact].** `mirror-lint/src/main.rs`
  gates `green = exemptions==cap && cap==13` with `let expected=13usize`. On lawful growth both rise to 14,
  `==cap` holds, `==13` REDs a legitimate change. Synthesis gap [1] concedes "must NOT survive" (honest),
  but the *running* PROTO whose "not-fit-to-count PASS" is cited as the crown still carries the clause. The
  verdict is self-undercut until the production gate is `exemptions==cap` alone and re-proven
  GREEN-on-growth / RED-on-unbudgeted.

- **[ill-posed TARGET — INC-6, cross-lock].** The two Rust frontends (offline checker leaving `Named(_)`
  intact → `UnregisteredRule`; eager cdylib resolver → `UnknownField`) are not isomorphic to *each other*,
  so "Rust↔TS isomorphism" has **no defined target** until an owner/LOCK-1/3/7 names the canonical Rust
  behavior. Neither 6A (blind to it) nor 6D (agreement≠correctness) can adjudicate. Correctly routed — but
  6A's advertised axis is undefined, not merely unmeasured.

- **[confounded floor-necessity datum].** DIVERGE=3 is **rust⊥rust** (the two frontends) surfaced through
  wasm, not the Class-2 rust⊥ts routing drift the family advertises. The one *clean* rust⊥ts datum fresh
  research-A found — `PathError.format` 11/13, the `{:?}`-escape vs raw-interpolate alternatives-escaping
  fork — lives on the render seam **6A itself DECLARES out-of-contract** and routes to 6D. So 6A's cleanest
  drift example is out of its own contract; its in-contract necessity proof is confounded.

- **[unbuilt].** I=5 class-closure asserted, not stressed (no sixth-idiom attempt); CASE
  totality/injectivity unbuilt; not landed in `xtask`/CI; the incidental `makePathTag` rethrow-raw bug is a
  live execution defect.

**Exact open gaps (6A):** (i) delete the `cap==expected` literal; production gate `exemptions==cap` alone,
re-proven GREEN-on-lawful-growth / RED-on-unbudgeted. (ii) the LOCK-3 provenance manifest 6A consumes
end-to-end **has no producer** — build it (LOCK-3) or the seam is SPEC_ONLY. (iii) port the pair off legacy
`crates/*` onto skinny/greenfield — 6A has **zero** on-substrate evidence; confirm `T=4` survives the GN
re-cut. (iv) the ill-posed Rust target (INC-6) is a precondition, not a footnote — no 6A convergence number
is well-defined until the canonical Rust is named. (v) re-take the floor-necessity proof on a clean
rust⊥ts drift (a dropped TS union variant), not the confounded rust⊥rust datum. (vi) CASE
totality/injectivity + xtask/CI landing unbuilt.

**Convergence: 66%.** Crown ran, but it is the only family with 100% off-substrate evidence, its declared
LOCK-3 consumer has no producer, its isomorphism target is ill-posed pending an owner decision, and the
running gate still carries the fit-to-count clause. **ADVANCED, not BLOCKED.**

---

## 2. Family 6B — SPEC-GENERATED-SKELETON · the pass's biggest mover; now gated only by a cross-lock fast-face question and an external landing

**Credit (folding `PROTO-2-6B.md`, which post-dates the synthesis).** 6B is no longer "classify FACE
only." A `LayoutSpec`-generated, **non-byte-identical** full decoder is V8-monomorphic on the load-bearing
path, folds to the PROVE-14 fingerprints exactly, and the gate is proven non-vacuous by a two-hazard teeth
control. M1 discharged (`gen_tape.mjs` exists, byte-reproducible). The strongest SPEC_ONLY→RAN conversion
in the plane; it closes the two 6B defects the 16:16 CRIT-1 rated most serious. The synthesis (§2 gap [1])
is stale below its own crown.

**Remaining defects — both CROSS-LOCK, which is why 6B still cannot fully converge inside LOCK-6:**

- **[XL-D — is the generated decoder the fast face, or an ORACLE? cross-lock, load-bearing].** XL-D binds:
  runtime decoders are −22%/ties-naive and "survive only as isomorphism/correctness ORACLES; the fast TS
  face is the BUILD-TIME EMITTER (4B-TS)." `PROTO-2-6B`'s decoder is a **runtime tape-walk**
  (`walk`/`nextSibling`/`getKey`/`classify` run at consume time). The synthesis asserts (§3 [5], §8 item 2)
  that a "value-navigation over a pre-built tape" is a *value* face compatible with XL-D's *parser*-face
  verdict — **plausible, but asserted, never measured against the build-time-emitter alternative.** If the
  gestalt places the generated decoder on the oracle side of XL-D, then 6B's headline "the ~364-LOC hand
  `tape.ts` drift surface closes to 0" is about **oracle** code, not the deployed fast face — materially
  weaker than "generation is drift-free AND V8-safe on shipped TS." The distinction is the whole worth of
  the crown and it is a routed-to-GESTALT open question, not a settled within-6B win.

- **[LANDING is a LOCK-3/LOCK-2 act, not a LOCK-6 one — XL-B].** `emit_classify`/`LayoutSpec` have **0 hits
  in skinny** (verified) — a LOCK-6 scratch probe. Q5 correctly homes production to **LOCK-3** (owns the
  `emit_*` tower + `LayoutSpec`) and **LOCK-2** (chooses `MaterializationClass`). Under XL-B (WORKING, final
  at GESTALT-B) neither has consumed it. The 488-LOC JSON template still ships (verified); "folds to ≈40
  lines of `LayoutSpec` data" is proven foldable in a probe, **not landed**. 6B's production convergence is
  capped inside LOCK-6 — its landing is a GESTALT-B decision.

- **[declarative-half is narrower than "drift impossible by construction"].** Face-I fixed flag-bit VALUES
  (`GRAMMAR_BIT0`) are hand-authored policy → **LINTED (6A), not generated** — the §5 seam. PayloadDecode
  axis unmodeled; R2 one-`u8` flag-plane collision (discriminator tag AND escape bit) needs a spec-declared
  bit-partition, unbuilt; TS `emit_direct_sink` symmetric face owed; Q4 VM `OpcodeDecode` stream is GO/NO-GO
  under LOCK-4C+LOCK-8C (both unmeasured, cross-lock).

**Exact open gaps (6B):** (i) settle XL-D: is the generated runtime decoder the fast face or an oracle? The
"drift surface 364→0" claim's strength depends entirely on the answer — measure the decoder-walk vs the
4B-TS build-time-emitter path, do not assert compatibility. (ii) land `emit_*`+`LayoutSpec` into LOCK-3's
tower + LOCK-2's class choice; fold the 488-LOC JSON template; prove `regen∘regen` byte-identity wired into
`xtask regen`. (iii) PayloadDecode axis + R2 bit-partition unbuilt. (iv) Face-I fixed flag-bit VALUES are
LINTED not generated — the declarative-half claim is schema+tier+offsets+classify+decoder, not "everything
declarative." (v) TS `emit_direct_sink` + Q4 VM stream (4C/8C) cross-lock.

**Convergence: 76%.** The full-decoder mono gate + teeth + M1 discharge lift 6B above CRIT-1's 70% and make
it the pass's highest-moving family. Held short by the cross-lock XL-D fast-face-vs-oracle question (on which
the crown's worth turns) and by a production landing that is a LOCK-3/LOCK-2/GESTALT-B act. **ADVANCED, not
BLOCKED.**

---

## 3. Family 6C — WASM-CORE (BANKED) · disposition firm; the resident win is now MEASURED bimodal, not the flat prediction the synthesis recites

**Credit.** BANKED is firm and now triply-grounded: pass-2's wasm 0.34–0.47× native NEON, the JS-string
wasm-core falsification, plus (Brief 1, post-freeze) the resident-lane measurement. Settled, not aspirational.

**Cross-lock and soundness defects:**

- **[the synthesis's flat-resident prediction is FALSIFIED for the shipping kernel].** §3 recites the
  resident win as flat "4.7–7.3× at every size" and §7 calls it "the one unmeasured sub-question."
  `PROTO-1-shipping-kernel.md` measures the crate as-shipped: **bimodal**, 0.9× at 1 KB, ≥2× only from
  ~8 KB batched. The §5 spine row must narrow to "resident bytes **AND ≥~8 KB batched**"; the browser
  small-doc face is TS-native, now measured, not predicted.

- **[elegant-reduction watch — the recovery path is unmeasured AND cross-lock].** `PROTO-1-shipping`'s
  fix-forward ("`scan_into(&mut StructuralIndex)` + hoist the LUT out of `scan_nibble` → push the crossover
  toward the raw-SIMD flat win") is (a) **unmeasured**, (b) a **LOCK-9 kernel-API change, not a 6C change**,
  and (c) leaves cost (i), the JS↔wasm boundary crossing, which buffer reuse does **not** remove. The "flat
  win" it reaches toward is the very bespoke-ABI number the same probe disclosed as a measurement artifact.
  "Extending it to small browser docs is a bounded amendment" is a load-bearing and-then-the-hard-part step
  that is unproven and homed in another lock.

- **[cross-lock — the resident magnitude is contingent on LOCK-9's undecided kernel-home].** 6C's measured
  win rides `crates/simd-scan` (LIVE x86 `avx2.rs`/`avx512.rs` vs Constitution-iv; the bimodal crossover is
  *shaped by the shipping API*). LOCK-9's disposition (9A one-kernel-library vs 9B native-rebirth) is
  unsettled, and the resident-lane verdict — magnitude and crossover point — depends on which kernel and
  entry-point API LOCK-9 ships. §8 item 6 files this "CLOSED, carried"; it is **not** closed — the resident
  verdict is contingent on a LOCK-9 decision.

- **[unexercised].** `memory.grow()` two-parse detach: confirmed by spec, **not by a running crash+copy-out
  recovery**. Homing collision unpriced (`wasm/Cargo.toml` claims `name="bbnf-wasm"`; `crates/bbnf-wasm`
  absent). value.js-4.0.0 divorce owner/F6-bound (revalidate at gestalt).

**Exact open gaps (6C):** (i) the resident win is bimodal (measured), not flat — the §5 spine and the "one
surviving win" framing must be re-stated as resident-AND-batched; small browser docs are lost. (ii) the
`scan_into`+LUT-hoist recovery is unmeasured and is a LOCK-9 kernel-API item; the boundary crossing survives
buffer reuse. (iii) resident magnitude/crossover is contingent on LOCK-9 kernel-home (9A vs 9B) + LIVE x86
purge — cross-lock, not closed. (iv) grow-detach two-parse case unexercised (spec-confirmed only). (v)
homing collision unpriced; value.js divorce owner-bound.

**Convergence: 80%.** Highest disposition-certainty: BANKED is firm, and the resident lane — the last open
sub-question — is now measured, strengthening the browser-dead/batch-only verdict. Held below converged only
by refinements to a settled banked verdict (the unmeasured cross-lock recovery, the grow-case), not by a
blocker. **BANKED.**

---

## 4. Family 6D — ORACLE-ISOMORPHISM (the mandatory FLOOR) · lowest; a "frozen" contract holds a live contradiction, the decisive number is unrun, and it is DAG-blocked behind two other locks

**Credit.** The cross-language fold RAN (research-D, three corpora, bit-identical: twitter/canada/citm),
two independent parsers agreeing bit-for-bit — the N-version property PROVE-14 lacked.

**Soundness and cross-lock defects:**

- **[a "frozen" contract with a LIVE representation flip — verified].** `contract-6D.toml` still commits
  `number_fold = "f64 LE-byte mix"` (raw bits) — verified in-file. Three positions have floated across
  three passes (bits → span → bits); pass-2's registry-credited correction *forbade* raw bits; research-D
  fresh rehabilitates bits-with-canonicalization. A mandatory floor whose checked-in "frozen" contract picks
  the representation a prior pass forbade is, by definition, **not converged**. Synthesis §4 [1] surfaces it
  and routes to GESTALT (honest, not masked) — but the §5 partition cites this very file as the evidence for
  "Behavior (6D): YES isomorphic." A self-contradictory contract cannot anchor a YES cell.

- **[the decisive verdict number is UNRUN — null-hypothesis-by-default / elegant-reduction].** 6D "wins as
  the null hypothesis, defeated only by measured drift/maintenance cost." The **R5 debuggability**
  comparison — time-to-root-cause under a bare digest mismatch ("something differs," no line/offset) vs
  mirror-maintenance cost — is the **single axis on which 6D is dominated, and it is unrun**. Only affirmative
  proxies ran (marginal fold LOC ≈ 0; injected-catch rates). The "reasonable point" verdict the whole plane
  turns on is asserted on the easy direction; the number that could defeat 6D-as-floor was not taken.

- **[gate teeth REASONED, not RUN + a silent-pass hole].** §0/§4 "5/5" is a **fixed injected table** over
  hand-named classes; the coverage-guided fuzzer generator is unbuilt. The fold ran at **u32** (verified
  `contract-6D.toml [meta] digest="u64" … u32 fp has ~77k-input birthday floor — widen`) — so at fuzzer
  scale gate-(a) **silently passes divergent input**. A gate that can silently pass is a gate-cannot-fail
  risk until the u64 widening is a diff, not spec text.

- **[cross-lock — agreement≠correctness is CONCRETE, and the tie-breaker lives in LOCK-7].** 6D cannot
  adjudicate the bifurcated-Rust case (INC-6) — it certifies agreement, not correctness. The tie-breaker IS
  LOCK-7's meta-circular N+1 oracle, **unrun**, in a separate lock.

- **[DAG-blocked behind LOCK-1 + LOCK-7].** `freeze_after: [LOCK-1, LOCK-7]` — the fold alphabet IS
  LOCK-1's grammar-derived Value ADT; the tie-breaker IS LOCK-7's reference parser. 6D — the floor under all
  three caps — **structurally cannot reach zero gaps** until two other (bbnf) locks freeze. Its convergence
  is capped by external ownership, not just internal work.

- **[fleet-economy is SPEC_ONLY].** "O(new value variants), K=0 for TOML/CSV/Sheets" rests on folding ≥1
  non-JSON grammar's value tree; **no such grammar exists in-tree** → reasoned, not measured. Must not be
  cited as measured until a non-JSON value tree folds cross-language.

**Exact open gaps (6D):** (i) the number-lane bits⊥decimal contradiction inside the "frozen" contract —
adjudicate via a spec-probe folding the residue + the `1e21` threshold both ways; the file cannot freeze
until then. (ii) the R5 debuggability verdict — the decisive "reasonable point" number that could defeat the
null hypothesis — unrun. (iii) fuzzer generator unbuilt; §0 "5/5" reconciled to an injected table; u64
widening is spec text, not a diff (birthday floor live at fuzzer scale). (iv) agreement≠correctness / LOCK-7
N+1 tie-breaker unrun (cross-lock). (v) `freeze_after [LOCK-1, LOCK-7]` caps convergence — two external locks
gate the floor. (vi) fold ≥1 non-JSON grammar's value tree (the affirmative economy proof).

**Convergence: 66%.** Fold RAN — a real advance — but its "frozen" contract holds a live representation flip,
the one number that could defeat its null-hypothesis default (R5) is unrun, its teeth are reasoned not run,
and it is DAG-blocked behind LOCK-1 + LOCK-7. **ADVANCED as floor; mandatory-not-blocked.**

---

## 5. The partition line (§5) and the plane verdict — the central artifact's ARBITER is a consumer-less LOCK-3 substrate

The plane's deliverable is the §5 partition line: 6B generates the declarative half, 6A lints the
hand-authored half, 6D floors both, 6C crosses the value plane. **The boundary between 6B-generate and
6A-lint is drawn by "the LOCK-3 provenance manifest is the arbiter" (single-source → 6B/skip; authored →
6A/lint).** That manifest **does not exist and is not in LOCK-3's converged set** — `emit_classify`/
`LayoutSpec` are 0 hits in skinny (verified), and 6A's `PROTO-2` used a local predicate, not a manifest. So
the plane's central artifact rests on a consumer-less substrate: the partition is drawable in prose but its
**runtime arbiter is unbuilt, in another lock**. This is the sharpest cross-lock finding — LOCK-6's
deliverable cannot execute until LOCK-3 emits the provenance manifest as a clean-regen byproduct.

Two further map caveats: (a) the "Behavior (6D): YES" row is anchored on a contract file that holds a live
representation flip (§4). (b) the "Delivery: native-core / two-binding" row rests on **SWC/Biome external
numbers** (~1.75–2.5× napi-over-wasm) transplanted as "prior art unanimous" — reasonable as a design prior,
but **SPEC_ONLY for THIS repo** (no in-tree napi-vs-wasm measurement of the bbnf/parse-that core), and it
double-homes the delivery decision with LOCK-8D + LOCK-9 (napi keeps NEON). Carry the SPEC_ONLY-for-this-repo
label.

**Net (cross-lock).** The plane advanced materially and honestly — four crowns ran, and the two §9 briefs
that ran post-freeze push 6B up (full decoder generated + monomorphic + teeth) and re-draw 6C's spine as
measured-bimodal. **It is not converged.** No family clears the bar (zero gaps + fresh audit + two clean
passes). The most urgent cross-lock actions, in order: (1) LOCK-3 must emit the **provenance manifest** — the
partition arbiter (6A) and the emit tower (6B) both consume a LOCK-3 artifact that does not exist; (2) resolve
XL-D for 6B (generated runtime decoder = fast face or oracle?) before crediting "drift surface 364→0" on
shipped TS; (3) an owner/LOCK-1/3/7 must name the **canonical Rust behavior** (INC-6) — the 6A/6D target is
ill-posed until then; (4) the registry cut must fold `PROTO-1-shipping-kernel.md` and `PROTO-2-6B.md`, which
post-date the synthesis, or two crowns are mis-recorded.

**Convergence roll-up:** 6A 66% · 6B 76% · 6C 80% (BANKED) · 6D 66%. Plane not converged; partition line
drawable-in-prose but its arbiter unbuilt (LOCK-3).
