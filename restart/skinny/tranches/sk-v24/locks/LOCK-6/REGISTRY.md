# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split) · REGISTRY

*Agglomerator record (Fable-tier judgment). One row per family; convergence is the **critics'
conservative floor** unless a floor is named as erring. The lock is not four rival answers — it is **one
mandatory behavioral FLOOR (6D) and three candidate structural CAPS (6A/6B/6C)** that conflict only where
their surfaces overlap. The plane's central artifact is the §5 stratification table; the deliverable is the
partition line, drawn by two reasonable-point NUMBERS — 6A's clean-map ratio (the linter's `cap`) and 6B's
gen-vs-hand LOC on the tape-layout face — plus the load-bearing claim beneath the whole stratification (6D is
a **mandatory** floor: structural caps necessary-not-sufficient). Current state is **after pass 3**.*

**Pass-3 headline.** Pass 3 is the pass where every crown MOVED SPEC_ONLY→RAN at some scale — the exact
prototype-allocation defect the last two passes repeated is not repeated here. But the synthesis
(`FAMILY-SPECS.md`, frozen **15:26**) is **STALE against its own on-disk prototypes** (mtime 15:47 / 15:57):
it calls 6A's linter "STILL UNBUILT… the single largest remaining SPEC_ONLY crown" and 6C's comparator
"scaffolded-but-unrun a fourth time" — **both FALSE**, both discharged this pass by artifacts sitting beside
it. **This registry is the reconciliation** (both critics' M1/F-STALE mandate): it folds `PROTO-1.md` (6C)
and `PROTO-2.md` (6A) and scores against the ACTUAL runs, not the stale prose. Two bookkeeping hazards are
recorded as findings, not buried: **(M1)** the 6B `emit_classify` machine's *code* is on disk
(`proto-1-code/`) but its *run-record* exists nowhere as a base-pinned artifact — it survives only as
FAMILY-SPECS §0-P1 prose, a Constitution-vii / XL-H exposure; and its mono harness `gen_tape.mjs` is
**absent from the deliverables** (verified), so the `%HaveSameMap` PASS is not reproducible from committed
files. **(M2)** the families sit on **two substrates**: 6A/`PROTO-2` ran on **top-level `crates/{core,bbnf-path,bbnf-path-ts}`** (LEGACY, archives at GN-0 per XL-A; **`skinny/crates/bbnf-path` does not exist** — verified) while 6B/6C ground on **skinny@HEAD**. The plane's four families do not yet share a tree.

**PROTO naming — canonical disambiguation (F-NUMBERING; the token collides three ways).**

| canonical name | on-disk | family | disposition |
|---|---|---|---|
| **PROTO-6C-wasmscan** | `PROTO-1.md` (+ `PROTO-2/wasm-scan/`, `scan.wasm`, `harness.html`) | 6C | RAN — wasm-core-home FALSIFIED (JS-string source), TS-native-home holds |
| **PROTO-6A-linter** | `PROTO-2.md` (+ `PROTO-2/mirror-lint-6A/`, `PROTO-2.patch`) | 6A | RAN — `cap==exemptions==13=4+2+1+2+4`, RED 2/2, real Node+wasm drive |
| **PROTO-6B-emit** | `proto-1-code/emit_classify.rs` (+ `gen_{json,sheets}.{rs,ts}`, `mono_check.mjs`) | 6B | code present; **NO standalone run-record** (M1); `gen_tape.mjs` missing |

*Base-pin `ce51c36fc` for all three (XL-H). PROTO-6C: `merge-base --is-ancestor` OK, HEAD==pin. PROTO-6A:
DESCENDS-OK. The 6B code header self-labels "PROTO-1 / probe 1 (family 6B)" but `PROTO-1.md` on disk is the
6C run — the source of the collision.*

---

## STATUS TABLE (after pass 3)

| Family | Status | Conv. | Δ | One-line |
|---|---|---:|:--:|---|
| **6A MIRROR-TREE** | **ADVANCED · dependent-on-6D · crown BUILT+RAN (legacy tree)** | **67%** | +1 | The #1 three-pass-undischarged directive is DISCHARGED: PROTO-6A-linter is a real `syn`-2 binary that reproduces `cap==exemptions==13` (grep-derived, not fit), REDs `BogusPayload`/`FooView` 2/2, and drives `ts/path.ts` through a REAL `serde-wasm-bindgen`/Node crossing (AGREE=2/DIVERGE=3, names-green-behavior-red confirmed+strengthened). Held near floor by four real defects: the running gate carries a `cap == 13` **fit-to-count clause** (REDs lawful growth); the DIVERGE=3 floor-necessity datum is **confounded (rust⊥rust**, not rust⊥ts); the pair is **legacy-only (M2**, no skinny equivalent); linter is **not landed in CI** and the LOCK-3 provenance seam is un-exercised. |
| **6B SPEC-GEN-SKELETON** | **ADVANCED · crown BUILT at the classify FACE** (schema-face BANKED-earned; type-algebra RETIRED) | **70%** | +16 | The largest SPEC_ONLY→RAN conversion of the pass: one `emit_classify(spec, Backend)` machine emits the Rust JSON `FromSource`, Rust Sheets `FromFlagTag`, and TS classify faces **byte-identical** to the hand code at pin; a 1-line spec edit drives 3 sites across 2 backends; AoS↔SoA is DISSOLVED into a discriminator-provenance axis **by run**. But the crown is the **classify FACE (165→0)**, the R3-critical monomorphic *slice* — the **~598-LOC full decoder is ungenerated**, the mono gate passed on the **byte-identical (trivial) direction**, the **run-record is not a standalone artifact (M1)** and its harness (`gen_tape.mjs`) is missing, and xtask-landing / R2 / PayloadDecode / TS symmetric sink remain open. |
| **6C WASM-CORE** | **BANKED** (crossing folds into the 6B/6D tape stratum; bbnf-side) · reinforced by a RUNNING falsification | **68%** | +5 | The thrice-deferred comparator RAN (PROTO-6C): v128 kernel parity-exact on all 12 docs, `scanOnly` beats TS 4.7–7.3×, but `TextEncoder` marshalling is **72–90% of `wasmFull`** and swamps the win below ~2 MB → **wasm-core-home FALSIFIED for JS-string sources, TS-native-home holds**. BANKED is now settled by a running number, not aspiration. The ONE surviving 6C win — the **wasm-resident-bytes lane** (bytes owned in wasm, no per-call marshal, `scanOnly` governs, wins at every size) — is **unmeasured**, and it is exactly where the §5 6B⊗6C spine is now measured-TENSE. |
| **6D ORACLE-ISOMORPHISM** | **ADVANCED · the mandatory FLOOR** (pure-6D-as-whole-answer BLOCKED for the tape) | **66%** | +5 | The cross-language fold RAN (research-D): raw IEEE-754 **bits fold bit-identical `b99874e8` Rust↔V8**; native-decimal DIVERGES on 3 enumerable classes proven to live in the canonical-decimal *spec*, not the machinery; in/out partition is checked-in data. The never-run mechanism ran. Lowest of the four because its "frozen" `contract-6D.toml` commits `number_fold = "f64 LE-byte mix"` (**bits**) while pass-2's credited correction forbids bits and research-D's own recommendation picks decimal — a **live in-file contradiction**; the **R5 debuggability number that could defeat its null-hypothesis default is UNRUN**; the fuzzer "5/5" is a fixed table not a running generator; u32→u64 is spec text; and its agreement≠correctness hole was just made concrete by 6A's forked reference. |

Net: **the plane advanced materially and honestly — four crowns ran where pass-2 had zero — and is NOT
converged.** No family clears the converged bar (zero enumerated gaps + fresh audit + two clean passes); the
§5 partition line is now *drawable* (both anchor numbers have running evidence) but **un-drawn** and stale in
the synthesis; and each family carries concrete internal defects. Per both critics the plane is a
**rigorously-converged CORE (~67%) with exact gaps → advance to a RECONCILIATION pass, not to GESTALT.**
Eligibility for a clean pass is **withheld** until M1 (give the 6B emit run a base-pinned writeup; commit
`gen_tape.mjs`) and M2 (name/port the legacy-vs-skinny substrate split) are discharged.

---

## 6A — MIRROR-TREE · ADVANCED · dependent-on-6D · crown BUILT+RAN · 67% (was 66)

**Floor rationale.** CRIT-1 72 / CRIT-2 66. Floor is **66**; take **67** (+1). Both critics fold PROTO-6A
and both credit the crown as BUILT+RAN — the largest correction to the synthesis's own framing. The 6-point
spread is entirely penalty-weighting: CRIT-2 holds 66 on M2 (legacy substrate) + the bifurcated Rust
reference; CRIT-1 holds 72 crediting the mechanism. The +1 over CRIT-2's floor is the one nameable
over-penalty: M2 makes the *specific 13-count* a fact about a doomed tree, but the linter **mechanism**
(`syn`-extract + predicate-derived `cap` + RED-on-synthetic + real wasm/Node drive) is substrate-portable —
a relocation, not a re-derivation — and a proven-transferable mechanism is a genuine de-risk over pass-2's
"unbuilt." No lift above 67: CRIT-2's forked-reference and CRIT-1's fit-to-count-clause discounts are both
real and unrebutted.

**What is EARNED (do not re-litigate).** (1) **The crown is BUILT and RAN (PROTO-6A).** A self-contained
`syn`-2 linter syn-extracts Rust decl surfaces + `tsc`-emitted `path.d.ts` names, resolves each non-identity
bridge decl to a spine pre-image, computes `cap` and `exemptions` **independently**, and reproduces
`13 = 4+2+1+2+4` **without fitting** (one sums predicate-hits-with-valid-pre-image, the other counts all
non-identity decls; they coincide only because the tree is disciplined). RED on `BogusPayload`
(serde-predicate match, no canonical `Bogus` pre-image) and `FooView` (no predicate) — **2/2**. (2) **A REAL
cross-language drive** — wasm cdylib (`wasm-pack --target nodejs`) + `serde-wasm-bindgen` + `node_drive.mjs`
through the actual `makePathTag`/`PathError` of `ts/path.ts` — replaces the pass-2 intra-Rust
`compile_path_native` proxy (that gap is discharged). (3) The **cap is re-derived defensibly** (D3): the
arithmetically-inconsistent `2·T+I` is gone, replaced by `cap = Σ_{c∈I}|sites matching predicate[c] with a
spine pre-image|` over I=5 relational idiom classes. (4) The **Q2 trichotomy** (Class-1 provenance / Class-2
name-map / Class-3 exemption budget) routes each class to a lawful check.

**Exact open gaps.**
1. **[fit-to-count clause in the RUNNING gate — must be deleted]** The as-built gate is
   `green = exemptions == cap && cap == expected` with `let expected = 13usize`
   (`mirror-lint/src/main.rs:318,342`). The design is emphatic the cap is "never a tuned scalar"; PROTO-6A's
   prose repeats "not fit to the count" — **the code contradicts both.** On *lawful* growth (add a spine type
   + its identity/`r(t)` mirror), `cap` and `exemptions` both rise to 14, `exemptions == cap` stays GREEN, but
   `cap == 13` fires **RED on a legitimate change**. The production gate must be `exemptions == cap` **alone**;
   the `== 13` literal is a probe scaffold that must not survive into the tranche.
2. **[the floor-necessity datum is CONFOUNDED — rust⊥rust, not rust⊥ts]** DIVERGE=3 has a single root cause:
   canonical `check_path_against_registry` leaves `TypeDesc::Named(_)` intact (`UnregisteredRule`) while the
   cdylib resolves `Named` eagerly via `registry.layout(id)` (`UnknownField`). That is a **Rust-vs-Rust
   semantic fork** surfaced *through* wasm, not the Class-2 rust⊥ts routing drift the family advertised. The
   thesis (names-green-while-behavior-red) holds regardless, but the *evidence* proves the floor catches
   "which Rust semantics the cdylib exposes," a different hazard. A clean rust⊥ts drift example (e.g. a dropped
   TS union variant) is owed before GESTALT credits 6A⊗6D on its intended axis.
3. **[BIFURCATED RUST REFERENCE — the isomorphism target is ill-posed (new, routed cross-lock)]** The two Rust
   frontends (offline checker vs eager proc-macro resolver) are **themselves not isomorphic.** "Rust↔TS
   isomorphism" is undefined until an owner/LOCK names WHICH Rust behavior is canonical. This is a
   substrate-red the linter cannot see and 6D cannot adjudicate (agreement≠correctness → LOCK-7 N+1, unrun).
   **Routed to LOCK-1/LOCK-3/LOCK-7 + owner; unresolved.**
4. **[legacy-tree substrate (M2)]** The pair lives ONLY at top-level `crates/{bbnf-path,bbnf-path-ts}`
   (archives at GN-0); **`skinny/crates/bbnf-path` does not exist** (verified). The 13-exemption count is a
   fact about a tree being deleted; the mechanism must be ported/re-homed to the measurement substrate.
5. **[not landed in CI; LOCK-3 seam un-exercised]** The linter is a standalone detached-target-dir binary,
   NOT wired into `xtask`/CI (same "ran-but-not-landed" status as 6B). And it **hardcodes T in `mirror.toml`**
   and classifies decls by local predicate — it does NOT consume a LOCK-3 `cargo xtask regen` provenance
   manifest (research-A Q3's LOCK-3-owns-manifest / LOCK-6-owns-linter split). The Class-1-PROVENANCE route is
   un-exercised end-to-end — the tower-duplication research-A warned against.
6. **[typed-`PathError` surface un-driven; incidental live bug]** `makePathTag`'s inner
   `try{…throw new PathError}catch{throw raw}` **swallows the typed `PathError`** and rethrows the raw
   `JsError`; the drive parses the payload out of `.message`, never through the typed presentation. The
   error-PRESENTATION isomorphism (the third limb's surface) is behaviorally untested; the rebox is dead code.
   A 6A-execution fix, correctly surfaced by the *behavioral* drive (a second, cleaner floor-necessity datum
   than the confounded one).
7. **[carried] CASE totality + injectivity** (`next`/`next_sibling`→one camelCase collision) unbuilt; **I=5
   class-closure asserted, not stressed** (a sixth-idiom attempt not run); **cross-repo F6 CI ownership**
   (per-repo manifest fragments + vendored corpus) proposed, unbuilt.

**Pass-4 directives.**
- *Build (the gate correction + landing):* delete the `cap == expected` clause — production gate is
  `exemptions == cap` alone; prove it GREEN on lawful growth (add a spine type + mirror) and RED only on
  unbudgeted names. Wire the linter into `xtask`/CI. Consume a LOCK-3 provenance manifest end-to-end (close
  gap 5), not local predicates.
- *Prototype (the clean floor datum + the port):* take a CLEAN rust⊥ts drift datum (drop a TS union variant;
  observe names-green/behavior-red on the *intended* axis) to replace the confounded rust⊥rust example. Port
  the linter onto skinny@HEAD / greenfield — or establish whether the `bbnf-path` pair must be recreated there
  (M2). Drive the typed `PathError` presentation surface (fix the swallow, close gap 6).
- *Research must answer:* WHICH Rust behavior is canonical (offline checker vs eager resolver) — the
  bifurcated-reference ill-posedness (gap 3), owed to LOCK-1/LOCK-3/LOCK-7 + owner; is a sixth idiom class
  reachable (I=5 closure stress).
- *Withheld from researchers:* do NOT seed "the linter is built therefore 6A is converged." Hand them the
  fit-to-count clause in the running gate, the legacy substrate, the forked Rust reference, and the confounded
  floor-necessity example — not the crown-built win.

---

## 6B — SPEC-GENERATED-SKELETON · ADVANCED · crown BUILT at the classify FACE · 70% (was 54)

**Floor rationale.** CRIT-1 70 / CRIT-2 70 — **converged; take 70**. Both critics fold the emit machine
(`proto-1-code/`), both credit the classify-face byte-identity + monomorphism-by-construction as the pass's
largest SPEC_ONLY→RAN conversion, and both hold the identical residual: the crown is a *face* (the
R3-critical monomorphic slice), not the decoder; the mono gate passed on the trivially-true direction; the
run-record is not a standalone artifact; the majority (full decoder + xtask + R2 + Face-I generation) is
untested. +16 over pass-2's 54 is the correct magnitude for a 0%→BUILT machine, and 70 is the correct ceiling
given the face-not-decoder scope.

**What is EARNED (do not re-litigate).** (1) **The machine exists and reproduces the hand faces
byte-for-byte** (PROTO-6B code, PROTO-1.md-narrated run): `emit_classify(spec, Backend)` over a concrete
backend-agnostic `LayoutSpec` emits the Rust JSON `FromSource`, Rust Sheets `FromFlagTag`, and TS classify
faces at 0-diff to pin (after learning to defer the single `default:` arm — a real generation bug found and
fixed in-probe). (2) **AoS↔SoA is DISSOLVED by run** into a discriminator-provenance axis (the tape is always
SoA; the "hard core" was never a storage question) — the pass-2 "definitional" park is answered. (3) **A
1-line spec edit (`+ "NamedRange"`) drove 3 emit sites across 2 backends** while JSON stayed invariant and the
twitter fp `986912547` held. (4) The **schema/type face stays BANKED-earned** (generates tsc-strict-clean,
scales one line per rule; R3's "linter is cheaper" falsified there — redirect energy off it). (5) The
combinator-**type**-algebra REFUTATION (ts-rs flatten) carries — that surface is 6A+6D, no 6B type-gen.

**Exact open gaps.**
1. **[M1 — NO standalone run-record; missing harness]** The 6B measured results (0-diff byte counts,
   `%HaveSameMap` pre/post) have **no base-pinned PROTO writeup on disk** — they survive only as FAMILY-SPECS
   §0-P1 prose (the `PROTO-1.md` slot holds the 6C run). And `mono_check.mjs` imports `./gen_tape.mjs`, which
   is **absent from `proto-1-code/`** (verified) — the `%HaveSameMap` PASS is **not reproducible from the
   committed artifacts.** Constitution-vii/XL-H exposure. Re-run, write it up, commit the harness before
   crediting the number.
2. **[CLASSIFY FACE ONLY — the load-bearing majority is ungenerated; low-information mono gate]** PROTO-6B
   generated **27 TS classify LOC → 0** and **138 Rust LOC → 0**; the PROVE-14 decoder is **625 hand TS LOC**,
   ~598 (walk/leaf/nav/pointer/load/verify) ungenerated. Worse, the mandated R3 mono gate spliced a
   **byte-identical** generated classify into the hand harness — so "same V8 hidden class" is a near-tautology
   (identical source ⇒ identical shape). The exact R3 risk research-B names (a generator emitting a
   post-ctor field assignment or a V8-un-lowerable switch **at a face where no hand reference pins it**) is
   precisely what a byte-identical reproduction **cannot** exercise. The mono gate becomes informative ONLY on
   the **full generated decoder** where the output is NOT byte-identical to a known-good hand version. "Crown
   BUILT" is true for the classify face; it is not true for the decoder.
3. **[R3 PayloadDecode axis unmodeled (research-B, under-foregrounded)]** Sheets `as_f64`/`as_bool`/`as_tag`
   are three payload decoders keyed on rule; a `LayoutSpec` carrying only discriminator recovery
   **under-generates** — "skeleton generated" narrows to the discriminator only unless a `PayloadDecode` axis
   lands. The sharper expressibility risk than R2.
4. **[R2 flag-plane collision unbuilt]** One `u8` slot cannot carry a discriminator tag AND an escape bit; a
   string-escape leaf inside an operator tower collides. Resolvable by a spec-declared bit-partition, unbuilt.
5. **[not landed in `xtask regen`; no full real-tape fp]** Standalone `rustc` binary; the twitter fp held via
   a **classify-independent** lane, NOT a full `doc.walk` (the SoA `tape_dump.rs` was not rebuilt). Lockstep
   regen gate (a) is only PARTIALLY demonstrated — the `+ "NamedRange"` move was on a standalone binary, not
   `xtask`; `regen∘regen` byte-identity + delete-the-hand-faces is owed.
6. **[Face I is PARTIALLY generated]** record-shape + payload byte-offsets GENERATED; fixed flag-bit *VALUES*
   (`GRAMMAR_BIT0`) are hand-authored policy → **LINTED not generated** (the §5 partition line; the "drift
   impossible on the declarative half" claim narrows to the schema+tier+offsets sub-half).
7. **[TS `emit_direct_sink` symmetric obligation unbuilt]** Q3's negative-cost fused-sink win is measured
   **Rust-only**; the symmetric TS face is owed.
8. **[cross-lock, correctly routed] Q4 VM instruction-stream** — the same `emit_classify` machine generalizes
   to `OpcodeDecode` with no new machinery, but GO/NO-GO depends on VM-vs-closure (LOCK-4C) + interpreter
   under V8 (LOCK-8C), both unmeasured. Asserted, not run; owed jointly to LOCK-4/8. Flagged so it is not read
   as a within-lock 6B convergence item.

**Pass-4 directives.**
- *Build (M1 discharge):* give the 6B emit run its own base-pinned standalone writeup (`PROTO-6B-emit.md`) and
  commit `gen_tape.mjs` — the number cannot be credited from prose alone.
- *Prototype (the informative gate):* scale `emit_classify` from the classify FACE to the FULL decoder
  (~598 remaining hand TS LOC) and re-run the monomorphism gate on **NON-byte-identical** generated output —
  the one condition under which the gate tests the crown's actual risk (research-B R2's boxed-union
  counterexample). Model the **R3 PayloadDecode axis** (`as_f64`/`as_bool`/`as_tag`). Land `emit_*` into
  `xtask regen`, delete the hand faces, prove `regen∘regen` byte-identity + a real-tape `doc.walk` fp
  (rebuild `tape_dump.rs`). Author the TS `emit_direct_sink` symmetric face. Resolve R2 with a spec-declared
  bit-partition.
- *Research must answer:* nothing new blocking within-lock; carry the Q4 VM stream to LOCK-4C/8C.
- *Withheld from researchers:* do NOT seed "monomorphism preserved by construction, gate PASSED." That gate is
  low-information (byte-identical ⇒ shape-identical). Hand them the full-decoder scale where the gate becomes
  real, the missing run-record + harness (M1), and the R3 PayloadDecode under-generation.

---

## 6C — WASM-CORE · BANKED · reinforced by a RUNNING falsification · 68% (was 63)

**Floor rationale.** CRIT-1 78 / CRIT-2 66 (both BANKED). Floor is **66**; take **68** (+2). The 12-point
spread is a genuine philosophical split: CRIT-1 scores 6C's *own* convergence high (its BANKED fate is now
settled by a running, parity-verified, base-pinned falsification — not aspiration); CRIT-2 scores it low
because PROTO-6C "undercuts the §5 spine the plane's stratification depends on." CRIT-2's concern is real but
is a **§5-plane** issue imported into the **per-family** row — per-family convergence for a BANKED family
measures disposition-settledness + gap-cleanliness, both of which PROTO-6C *improved*. The +2 over floor
credits that; no lift toward CRIT-1's 78 because the **wasm-resident-bytes lane is unmeasured** (the ONE lane
where 6C's kernel win survives) and the grow-hazard is unexercised — real residuals that keep even a settled
BANKED family from converging.

**Why BANKED (confirmed; now with a running number).** 6C-as-independent-primary was refuted in pass 2
(wasm SIMD 0.34–0.47× native NEON; full-walk 0.9×; R-E sync-ESM confinement). **PROTO-6C reinforces BANKED
with a browser-side falsification of wasm-core-home:** the v128 kernel is parity-exact on all 12 docs and
beats TS-scalar **4.7–7.3× on `scanOnly`**, but `TextEncoder` UTF-16→UTF-8 marshalling is **72–90% of
`wasmFull`** (twitter 88%, citm 90%, canada 72%) and swamps the win below ~2 MB. Gate (b): `wasmFull < ts` on
canada is TRUE (2.25 MB, ASCII + branch-hostile → wasm 1.31×), but the net-win crossover (~2 MB, a
**content** artifact not a size law) exceeds every realistic bbnf batch doc size (KB-scale grammars) → the
falsifier's second clause fires. **wasm-core-home FAILS for JS-string sources; TS-native-home holds.** The
research-C Q1 prediction ("wasm wins skip-heavy, loses dense") is **falsified by its own probe** — the axis
is marshalling, not skip-heaviness; the kernel wins everywhere on `scanOnly`.

**What is BANKED (preserved, load-bearing).** The **zero-copy tape crossing** — 6B generates the byte
contract, 6C crosses it once as typed-array views, 6D digests both sides — confined to the bbnf side. BANKED,
not RETIRED: the crossing survives and the bbnf-side home decision is genuinely still open (see gap 1).

**Exact open gaps.**
1. **[the ONE surviving 6C win is UNMEASURED — the true home decision]** PROTO-6C's decisive caveat: if bytes
   are **already resident in linear memory** (produced/owned in wasm, no per-call `TextEncoder`+copy), the
   `scanOnly` column governs and v128 wins **4.7–7.3× at every size.** The 6B⊗6C graft decision is therefore
   "**where do the bytes live?**": JS-string source → TS-native (MEASURED loss); wasm-resident bytes →
   wasm-core (PREDICTED win, **unmeasured**). This residual is the only lane the kernel win survives, and it
   is exactly where the §5 spine is now tense (below).
2. **[grow-hazard two-parse case STILL unexercised]** `memory.grow()` detaches the backing `ArrayBuffer`;
   PROTO-6C's static `bss` arena **side-steps** grow rather than exercising it. The
   two-parses-through-one-growing-instance corruption + copy-out-recovery case is confirmed by spec, not by a
   running crash. The ≥2× copy-out margin is inherited from PROVE-14, not re-measured under grow.
3. **[homing collision unpriced]** `wasm/Cargo.toml` already claims `name="bbnf-wasm"`; the proposed
   `crates/bbnf-wasm` does not exist. One consolidated cdylib vs two cold instances unpriced.
4. **[value.js divorce owner/F6-bound (XL-G P1)]** value.js 4.0.0 (`tranche-u` WIP) imports zero parse-that;
   the confinement oracle re-homes to latex-paper (still imports), unverified there. Pricing rests on an
   unratified branch.
5. **[cross-lock, routed to gestalt] LIVE x86 `avx2.rs`/`avx512.rs` in `simd-scan`** vs constitution-iv "x86
   dead" — a LOCK-9 surface; `6C.wasm-scan ⇐ 9A.kernel-home` interlock unresolved.

**Pass-4 directives.**
- *Prototype (the true home decision):* measure the **wasm-resident-bytes lane** — bytes produced/owned in
  wasm, no per-call marshal, `scanOnly` governing. This is the ONLY regime where the v128 kernel win survives
  and it decides whether the tape-crossing stratum can ever home in wasm (gap 1, and the §5 spine).
- *Prototype (the residual gate):* exercise the grow-hazard — two parses through one growing instance,
  `memory.grow` detach + copy-out recovery, assert no silent corruption; quantify copy-out against the 4.4×
  stop-early margin. Price the homing collision (one consolidated cdylib vs two).
- *Research must answer:* re-home the confinement oracle (value.js divorced; latex-paper) — flag F6/owner-bound;
  carry the x86-in-tree + `9A.kernel-home` interlock to gestalt.
- *Withheld from researchers:* do NOT seed "wasm-core is dead" (the wasm-resident lane is unmeasured and is
  where the kernel wins) NOR "wasm-core-home is viable" (the JS-string lane is FALSIFIED). Hand them the
  where-do-the-bytes-live question — the single unresolved sub-question of a settled BANKED verdict.

---

## 6D — ORACLE-ISOMORPHISM · ADVANCED · the mandatory FLOOR · 66% (was 61)

**Floor rationale.** CRIT-1 68 / CRIT-2 66 — near-agreement; **take the floor 66**. Both critics credit the
cross-language fold RAN as a genuine, large advance; both rate 6D lowest of the four for the identical
reasons: its "frozen" contract holds a live representation contradiction, the one number that could defeat its
null-hypothesis default (R5) is unrun, its gate teeth (fuzzer) are reasoned-not-run, and its agreement≠
correctness hole was just demonstrated by a sibling probe (6A's forked reference). The 2-point spread is
negligible; no nameable error → floor. 66.

**What is EARNED (do not re-litigate).** (1) **The fold RAN cross-language** (research-D D1): raw IEEE-754
bits fold **`b99874e8` bit-identical Rust↔V8**; `Math.imul == wrapping_mul(u32)` per lane makes the folds
bit-exact; object = XOR-accumulate (key order OUT of contract, proven `baseline == keyperm`); native-decimal
DIVERGES on 3 enumerable classes (exp threshold ≥1e21/<1e-6; `-0`; subnormal expansion) proven to live in the
canonical-decimal **spec**, not the machinery. The pass-2 "never-run mechanism" is discharged at corpus scale.
(2) The **in/out partition is checked-in data** (`contract-6D.toml`): values + accept/reject + error-class
enum IN; wording + span + physical member order OUT; error-class toggle ON for class-equality, OFF for
message+span. (3) **pure-6D-as-whole-answer stays BLOCKED for the tape** ("layout free per language" negates
the byte-identical SoA layout the zero-copy stop-early win requires — 6D wins as verification substrate, loses
as the whole answer for the tape).

**Exact open gaps.**
1. **[live in-file contract contradiction — the number lane]** `contract-6D.toml` commits
   `number_fold = "f64 LE-byte mix"` (raw **BITS**, verified). Pass-2's registry-credited Q10 correction
   forbids raw bits ("fold spans/canonical-decimal, **NEVER** raw bits"); research-D's **own** recommended
   `oracle-contract.toml` picks a *contract-normalized decimal* (shortest-round-trip, positional-always, `+0`)
   with `ieee754-bits` as an **escape hatch**. **Three positions float** (pass-2: decimal-never-bits /
   research-D: decimal-with-bits-fallback / checked-in: bits) and the "frozen" artifact picks the one the
   pass-2 correction forbids — the third representation flip in three passes (bit→span→bit). Routed to GESTALT
   is acceptable; a mandatory floor whose **frozen contract contradicts itself within the checked-in file** is
   not converged. It **cannot freeze** until a spec-probe (fold the residue + the `1e21` threshold both ways)
   settles it.
2. **[R5 debuggability UNRUN — the verdict-deciding number, elegant-reduction]** 6D "wins by default as the
   null hypothesis, defeated only by measured drift/maintenance cost." The **single axis where 6D is
   dominated** — time-to-root-cause under a bare digest mismatch ("something differs," not "escape decoder
   off-by-one at line X") vs mirror-maintenance cost — is **unrun.** The pass measured only affirmative proxies
   (added-grammar marginal ≈ 0 fold LOC; injected-divergence catch rates). The "reasonable point" verdict the
   whole plane turns on is asserted on the easy direction; the number that could defeat the null hypothesis was
   not taken.
3. **[fuzzer teeth REASONED, not RUN; §0/§4 self-inconsistent]** FAMILY-SPECS §0-D1 headlines
   "coverage-fuzzer 5/5"; §4 gap (v) says the generator "is unbuilt." Only static 4/5 + latency-1 ran; the
   "5/5" is at best a fixed injected table over 5 hand-named classes, not a running coverage-guided generator.
   gate-(a) still exercises mostly the agree-among-correct direction — a passing gate with a weak adversary.
4. **[u32 FALSE-GREEN risk — a gate that silently passes]** The fold ran at u32; a u32 `fp` collides at ~77k
   inputs (birthday). At fuzzer scale (10⁶+) two DIFFERENT value trees digest identically → gate-(a) passes on
   divergent input. `digest="u64"` is spec text in the toml, **not a diff.**
5. **[agreement≠correctness hole, now CONCRETE]** 6A/PROTO-6A's bifurcated Rust reference is exactly the
   both-engines-disagree case 6D cannot adjudicate; LOCK-7's N+1 reference is unrun. The floor has a known,
   now-demonstrated hole.
6. **[affirmative economy case partly SPEC_ONLY]** "6D's added-grammar cost is O(new variants)" rests on
   folding ≥1 non-JSON grammar's value tree; **no TOML/CSV grammar exists in-tree**, so the K=0 fleet-economy
   claim is *reasoned*, not run. Acceptable as a design argument; must not be cited as measured.

**Pass-4 directives.**
- *Prototype (adjudicate the contract, at last):* run the spec-probe that settles the number lane — fold the
  canonicalized residue + the `1e21` exponent threshold BOTH ways (bits vs contract-decimal), see which
  diverges; the "frozen" `contract-6D.toml` cannot freeze until this resolves (gap 1, routed to GESTALT for
  ratification).
- *Prototype (the verdict-deciding number + the teeth):* RUN the **R5 debuggability comparison**
  (time-to-root-cause under a bare digest mismatch vs mirror-maintenance cost) — the ONE number that decides
  the null-hypothesis default (gap 2). BUILD the coverage-guided fuzzer (reconcile §0 "5/5" against §4
  "unbuilt"); land **u32→u64 as a diff** (gap 4). Fold ≥1 non-JSON grammar's value tree cross-language (the
  affirmative economy proof; add a grammar not yet in-tree).
- *Research must answer:* the LOCK-7 N+1 reference for a *seeded* both-engines-wrong case (the oracle proves
  agreement, not correctness — now concrete via 6A's forked reference, gap 5); carry the DAG (freeze strictly
  AFTER LOCK-1 + LOCK-7).
- *Withheld from researchers:* do NOT seed "the fold RAN therefore 6D is the settled floor." Hand them the
  self-contradictory frozen contract, the unrun R5 decider, and the reasoned-not-run fuzzer — not the
  fold-RAN headline.

---

## CROWDING & REDIRECT (this pass)

The plane is a **stratification, not a bake-off** — one floor (6D) + three caps (6A/6B/6C) partitioned by
surface. No new banking is needed (6C is already BANKED, folded into the 6B/6D tape stratum; 6A⊥6B is
resolved by the partition line, not by banking). Pass-3's runs *sharpen* the redirects and open one
underexplored formulation whose measurement is now decision-relevant.

- **The §5 PRIMARY graft (6B⊗6C zero-copy crossing) is now measured-TENSE — the sharpest redirect.** PROTO-6C
  shows the crossing of a **JS-string source** into wasm is a **net loss <2 MB**, and bbnf browser source
  arrives as a JS string (fetch/editor). The crossing only pays when bytes are **already resident in wasm
  linear memory** — which requires the whole parse→tape pipeline in wasm, i.e. **6C-as-primary, REFUTED.** So
  "6B generates the contract, 6C crosses it zero-copy" is economically self-tensioned: cheap crossing ⇒
  6C-primary ⇒ refuted. **Redirect: open the wasm-resident-bytes lane as the underexplored formulation** — the
  ONE regime where the kernel win survives (`scanOnly` 4.7–7.3×). Measuring it is what re-draws the spine (6C
  pass-4 directive 1). Until then the stratification's spine is the *weakest-evidenced* row, not the firmest.
- **6B's schema face stays BANKED-earned; the tape/classify face is now MEASURED-BUILT.** Redirect holds from
  pass-2: stop re-measuring the schema face. The underexplored formulation opened in its place — the
  tape-layout/decoder face — is now BUILT at the classify *slice*; the redirect **narrows** to the
  full-decoder scale + the R3 PayloadDecode axis (where monomorphism actually risks deopt), plus
  **6B-as-layout-gen ⊗ LOCK-4C VM stream** (the shared-bytecode formulation, strengthened by PROTO-6B's
  machine generalizing to `OpcodeDecode`).
- **6A ⊥ 6B stays resolved by the Q2 trichotomy.** The partition is now *drawable* (both anchor numbers have
  running evidence: 6B's classify-face byte-identity + 6A's `cap==13` ratio) — but **un-drawn** in the stale
  synthesis and one anchor (6A's) is on the legacy tree. The clean partition: 6B generates the declarative
  half (schema + classify + tape byte-contract); 6A lints the hand-authored half (Class-2 taxonomy + Class-1
  provenance + Class-3 idiom residue + the ~15 fixed flag-bit VALUES, Face-I LINTED-not-generated); 6D floors
  both.
- **6D is the floor beneath all three caps (no bank).** PROTO-6A made the floor's *necessity* concrete
  (names-green/behavior-red, live through wasm) — but on a **confounded (rust⊥rust)** axis. **Redirect: the
  clean rust⊥ts drift datum (drop a TS union variant) is the underexplored formulation owed** before GESTALT
  credits 6A⊗6D on its intended axis.

## CROSS-POLLINATION (named grafts — both families developed enough, graft justified)

1. **6B ⊗ 6C — the generated tape byte-contract crossed zero-copy (PRIMARY, the stratification's spine).**
   Both legs now developed (6B classify face BUILT; 6C comparator RAN). **Status: NARROWED to one open
   sub-question, and measured-TENSE.** The 6B leg is BUILT at the classify face; the 6C crossing is
   PROVE-14-anchored; PROTO-6C shows the crossing PAYS only for wasm-resident bytes (JS-string source is a net
   loss). The graft is not resolved — it is narrowed to "where do the bytes live?" (6C gap 1). Progress,
   correctly not over-claimed.
2. **6A ⊗ 6D — 6A's Class-2/T3 corpus IS 6D's differential corpus (PRIMARY).** Both developed (6A linter
   BUILT; 6D fold RAN). **Status: thesis PROVEN by run (names-green/behavior-red), evidence CONFOUNDED.**
   PROTO-6A demonstrated the floor's necessity live through wasm+serde-wasm-bindgen — but the DIVERGE=3 datum
   is rust⊥rust (offline checker vs eager resolver), not the rust⊥ts drift the graft's intended axis needs.
   The graft holds; the evidence is owed a clean rust⊥ts re-take.
3. **6B ⊗ 6D — the generated oracle table (SECONDARY).** **Status: 6B leg NOT yet developed on the oracle
   face.** PROTO-6B built the classify face, not the oracle face (`emit_oracle` still 0% built); GN-4.W5's
   self-stub (`get==get`) is not yet unwriteable-by-construction. Keep SECONDARY; the 6B oracle leg is
   greenfield.

## CROSS-LOCK DAG ORDERING (for GESTALT)

Pass-3 carries the two pass-2 NAMED items and surfaces **two NEW cross-lock items** from the runs.

- **[NEW — the runs surfaced it] BIFURCATED RUST REFERENCE (6A gap 3).** PROTO-6A proved the two Rust
  frontends (offline `check_path_against_registry` vs eager proc-macro `registry.layout`) are **not
  isomorphic** — "Rust↔TS isomorphism" is undefined until an owner/LOCK names WHICH Rust behavior is canonical.
  Routed to **LOCK-1/LOCK-3/LOCK-7 + owner.** 6D cannot adjudicate it (agreement≠correctness → LOCK-7 N+1,
  unrun). The reference is forked, not just the routing.
- **[NEW — XL-D tension unnamed] the tape-decoder-walk: fast-face or oracle?** PROTO-6C's `TS-native-home`
  verdict sits on the PROVE-14 **runtime tape-decoder walk** — a runtime decoder. XL-D demotes *runtime
  decoders* to oracles and crowns the build-time emitter as the fast face. Whether the tape-decoder-walk is
  "the fast face" (6C) or "an oracle" (XL-D) is unreconciled; FAMILY-SPECS never distinguishes tape-decoder
  from combinator-runtime-decoder. **Name it** before GESTALT consumes the lock.
- **[NAMED, carried — the one live combinator fork] LOCK-4C / LOCK-8C — 6B-VM-stream vs hand-code.** The
  ts-rs type-gen refutation does NOT bind a shared bytecode layout; PROTO-6B's machine generalizes to
  `OpcodeDecode` with no new machinery (strengthened). GO/NO-GO depends on VM-vs-closure (LOCK-4C) +
  interpreter-under-V8 (LOCK-8C), both unmeasured. The seam GESTALT-P collides on first; adjudicate jointly.
- **[NAMED, carried] LOCK-3 / LOCK-2 — `LayoutSpec` + `emit_*` faces + `MaterializationClass`.** LOCK-3 owns
  the emit spine + the provenance manifest (research-A/B Q3/Q5); LOCK-2 owns the `MaterializationClass` +
  discriminator-provenance CHOICE (a cost decision). Named-as-open; assign before either gestalt.
- **[NAMED, carried] 6D number-lane bits ⊥ decimal (6D gap 1)** — settled by a GESTALT spec-probe.
- **[CLOSED, carried] LOCK-9 kernel double-home** (`6C.wasm-scan ⇐ 9A.kernel-home`) + **LIVE x86 in-tree**
  (`avx2.rs`/`avx512.rs` vs constitution-iv, surfaced-not-buried); **LOCK-5 Rust-DFA↔TS-`RegExp`** exemption
  (behavioral routing to 6D, compatible with XL-C's two-domain-fit verdict); **LOCK-1/LOCK-7 freeze order**
  (6D's alphabet IS LOCK-1's `Value` ADT; the tie-breaker IS LOCK-7's meta-circular oracle — every family
  freezes strictly AFTER LOCK-1 + LOCK-7).

## SEAM HYGIENE (within-lock)

- **[M1 — the 6B run-record is not an artifact]** `PROTO-6B-emit.md` does not exist; `gen_tape.mjs` is missing
  from `proto-1-code/` (verified). The `%HaveSameMap` PASS is synthesis prose, not a reproducible artifact.
- **[M2 — the substrate is split]** 6A/PROTO-6A ran on legacy top-level `crates/{core,bbnf-path,bbnf-path-ts}`
  (archives at GN-0); **`skinny/crates/bbnf-path` does not exist** (verified). The plane's four families do not
  share a tree; 6A's 13-count is legacy-bound.
- **[fit-to-count clause in a shipped gate]** `mirror-lint/src/main.rs:318,342` carries `cap == 13`
  (`expected = 13usize`) — a probe scaffold that REDs lawful growth; must not survive into the tranche.
- **[T0 byte-identity tier still consumer-less]** the born-RED wasm-plane byte cross-check (GN-4.W4 gate b)
  that 6B gate-(c) and 6C both cite has no home; `crates/bbnf-wasm` absent while `wasm/Cargo.toml` claims
  `name="bbnf-wasm"` — the homing collision is real and unpriced.
- **[stale codegen/tape seams]** research-C/priorart cite `crates/runtime/src/tape/mod.rs`; the tree
  consolidated under `crates/core/src/runtime/tape/mod.rs`. Any landing needs a seam-relocation pass first.

---

## PASS HISTORY

### Pass 3 (2026-07-16) — agglomerated

- **Artifacts folded:** FAMILY-SPECS (pass-3 synthesis, frozen 15:26 — **STALE against its own prototypes**);
  research-{A,B,C,D,priorart}; DIRECTIVES-RESEARCH; `contract-6D.toml`; **PROTO-1.md = PROTO-6C-wasmscan**
  (6C, RAN_MEASURED — wasm-core-home FALSIFIED); **PROTO-2.md = PROTO-6A-linter** (6A, RAN_MEASURED —
  cap==13, RED 2/2, real Node+wasm drive); `proto-1-code/` = **PROTO-6B-emit** (6B machine, code present,
  **no standalone run-record — M1**); CRIT-1 (within-lock: 6A 72 / 6B 70 / 6C 78 / 6D 68); CRIT-2 (cross-lock:
  6A 66 / 6B 70 / 6C 66 / 6D 66; plane ~67).
- **THE RECONCILIATION (both critics' M1/F-STALE mandate).** The synthesis calls 6A's linter "unbuilt
  (Brief 2)" and 6C's comparator "unrun a fourth time" — both FALSE; both discharged this pass. This registry
  scores against the runs. The three-way "PROTO-{1,2}" name collision is disambiguated (canonical table
  above). 6B's emit run-record has NO base-pinned writeup and its mono harness `gen_tape.mjs` is missing (M1);
  the substrate is split legacy-vs-skinny (M2). **Clean-pass eligibility WITHHELD until M1 + M2 discharge.**
- **PROTO-6A-linter disposition (6A):** RAN_MEASURED. The #1 three-pass-undischarged directive is DISCHARGED
  with a running mechanism. `cap==exemptions==13=4+2+1+2+4` grep-derived (not fit); RED on
  `BogusPayload`/`FooView` 2/2; a REAL `serde-wasm-bindgen`/Node drive of `ts/path.ts` (AGREE=2/DIVERGE=3).
  **Three defects surfaced:** the running gate carries a `cap == 13` fit-to-count clause; the DIVERGE=3
  floor-necessity datum is confounded (rust⊥rust — offline checker vs eager resolver, not rust⊥ts); the pair
  is legacy-only. Incidental live bug found (behaviorally): `makePathTag` swallows the typed `PathError`.
- **PROTO-6C-wasmscan disposition (6C):** RAN_MEASURED. v128 kernel parity-exact on all 12 docs; `scanOnly`
  beats TS 4.7–7.3×; marshalling 72–90% of `wasmFull`; net win only on canada (2.25 MB, content-dependent).
  **wasm-core-home FALSIFIED for JS-string sources; TS-native-home holds.** research-C Q1 prediction falsified
  by its own probe (axis is marshalling, not skip-heaviness). BANKED firmed by a running number; the ONE
  surviving win (wasm-resident-bytes lane) is unmeasured and is where the §5 spine is now tense.
- **PROTO-6B-emit disposition (6B):** code RAN (per synthesis prose) — one machine emits JSON/Sheets/TS
  classify byte-identical, monomorphism preserved by construction, AoS↔SoA dissolved by run. **But M1:** no
  standalone base-pinned writeup; `gen_tape.mjs` missing → `%HaveSameMap` PASS unreproducible from artifacts.
  Scope: the classify FACE (165→0), the R3-critical *slice*; the ~598-LOC full decoder is ungenerated and the
  mono gate passed on the byte-identical (trivial) direction.
- **6D fold disposition (research-D):** RAN. Raw IEEE-754 bits fold `b99874e8` bit-identical Rust↔V8;
  native-decimal diverges on 3 enumerable classes proven to live in the spec; in/out partition checked-in.
  **But `contract-6D.toml` commits `number_fold="f64 LE-byte mix"` (bits)** while pass-2 forbids bits and
  research-D recommends decimal — a live in-file contradiction (D2). The R5 debuggability number is unrun; the
  fuzzer "5/5" is a fixed table; u32→u64 is spec text.
- **The prototype-allocation defect is REVERSED (pass-2's headline weakness).** All four crowns moved
  SPEC_ONLY→RAN at some scale: 6B hit the R3-critical classify face; PROTO-6C hit the exact 6C marshalling
  decision; PROTO-6A hit the exact 6A crown + a real wasm crossing; 6D's fold ran cross-language. Residual
  prototype-evidence weaknesses are now *second-order* (6B's mono gate on the easy direction; 6D's R5
  untaken; 6A's fit-to-count clause; 6B's missing harness), not the "measured the wrong thing" defect of
  pass 2.
- **Dispositions set:** 6A ADVANCED·crown-BUILT+RAN(legacy tree) 67 (+1); 6B ADVANCED·crown-BUILT-at-classify-
  face 70 (+16, schema-face BANKED-earned, type-algebra RETIRED); 6C BANKED 68 (+5, fate settled by a running
  falsification, wasm-resident lane the one open sub-question); 6D ADVANCED·mandatory-FLOOR 66 (+5, fold RAN,
  contract self-contradictory). §5 partition line now drawable, un-drawn.
- **Cross-lock:** two NEW items surfaced by the runs (**bifurcated Rust reference** → LOCK-1/3/7+owner;
  **tape-decoder-walk fast-face-vs-oracle** XL-D tension → name it); two NAMED carried (**LOCK-4C/8C VM
  combinator fork** — strengthened by PROTO-6B's `OpcodeDecode` generalization; **LOCK-3/2 ownership**);
  6D number-lane bits⊥decimal → GESTALT spec-probe; LOCK-9/x86, LOCK-5, LOCK-1/7 carried closed.
- **Pass-4 non-negotiable minimum:** (M1) give the 6B emit run a base-pinned writeup + commit `gen_tape.mjs`;
  (M2) name/port the legacy-vs-skinny substrate; (6A) delete the `cap == 13` clause, land the linter in CI,
  take a clean rust⊥ts drift datum; (6B) scale the emit machine to the full decoder + re-run the mono gate on
  non-byte-identical output; (6C) measure the wasm-resident-bytes lane (re-draw the §5 spine); (6D) adjudicate
  the number-lane contract + RUN the R5 debuggability number + build the fuzzer + land u32→u64. Adjudicate the
  bifurcated-Rust-reference and the LOCK-4C VM fork before GESTALT-P consumes the lock.

### Pass 2 (2026-07-16) — agglomerated

- **Artifacts folded:** FAMILY-SPECS (pass-2 synthesis: 4 revised specs + §5 stratification + 2 briefs);
  research-{A,B,C,D,priorart}; DIRECTIVES-RESEARCH (Q1–Q10 + 3 cross-lock adjudications); PROTO-1 (6D/6A
  floor decider, RAN_MEASURED); PROTO-2 (6B honest-scope decider, RAN_MEASURED); CRIT-1 (within-lock: 6A 66 /
  6B 54 / 6C 63 / 6D 61); CRIT-2 (cross-lock: 6A 74 / 6B 60 / 6C 76 / 6D 66 / §5 66).
- **PROTO-1 disposition (6D floor, shared 6A):** RAN — `floor_probe.rs` produced the designed green/red PAIR.
  Structural test PASS (Rust `PathErrorReason` 8 variants ≡ TS union 8 members, set-equal); behavioral test
  FAIL by design (`$.statuses[0].nope`: canonical `UnregisteredRule` ≠ cdylib `UnknownField`). **6D-AS-
  MANDATORY-FLOOR CONFIRMED** — structural name-set parity is necessary-not-sufficient for behavioral parity.
  **SOFTENING (load-bearing):** the RED is **Rust↔Rust** (`check_path_against_registry` vs `compile_path_
  native`, both host) on a single latent bug, never a Rust↔TS fold; the TS union is parsed only for the green
  side. Floor *existence* proven a-fortiori; the rust/ts boundary it gates was never crossed.
- **PROTO-2 disposition (6B):** RAN — anchor + number. **Anchor rescued phantom→partial:**
  `classify_materialization` + `compute_payload_layouts` are genuine grammar→layout-fact derivations (verified
  live), but yield a decode-*class*/tier + byte-offsets, NOT a raw flag-bit; `STRING_NEEDS_DECODE`/
  `GRAMMAR_BIT0` are hand-authored policy absent from `crates/`. **Number = 1.00 faithful across 3 grammars,
  tsc-strict-clean — but measured on the pre-existing schema face** (`emit_type_definitions_impl`, live), the
  ratio tautological (definitional for 2/3 grammars; only non-circular idiomatic baseline is 1.29/3.08). The
  three faces that ARE the family (`emit_tape_layout`/`emit_tape_decoder_skeleton`/`emit_oracle`/`LayoutSpec`)
  are **0% built — re-verified empty tree-wide this session.** Schema face BANKED-earned (R3 falsified there);
  tape-layout face UNMEASURED (R3 survives there).
- **The prototype-allocation defect (both critics, RE-EMERGENT):** both pass-2 probes RAN but each measured a
  **softer target than its family's sharpest uncertainty** — PROTO-1 the floor via a Rust↔Rust proxy (not a
  Rust↔TS fold); PROTO-2 the gen-vs-hand ratio on the pre-existing easy face (not the greenfield tape-layout
  face). The probes moved the *feasibility* frontier, not the *mechanism* frontier. Both are less decisive
  than presented; neither is a mandate to crown the stratification.
- **Design advances credited:** 6A's Q2 Class-1/2/3 trichotomy (dissolves the identity fork) + the
  three-conjunct RED gate (replaces the vacuous budget); 6D's Q10 IEEE-754 correction (fold spans/canonical-
  decimal, NEVER raw bits — reverses pass-1's own false-divergence-generating bit-fold "sharpening"); Q6
  (Chrome-114 sync-instantiation de-escalation) + Q7 (LOCK-9 interlock). New defect surfaced: 6A's impedance
  cap `2·T+I` is arithmetically inconsistent (I=2 vs 3 idiom classes → 7≠6) as well as curve-fit to n=1.
- **Cross-lock:** 3 of 5 pass-1 ties CLOSED (LOCK-9 double-home; LOCK-5 regex exemption; LOCK-1/7 freeze
  order); 2 NAMED-not-adjudicated (**LOCK-4C combinator-layout fork — the single live unresolved item**;
  LOCK-3/2 ownership — named-as-open). GESTALT-P collides on these two first.
- **Dispositions set:** 6A ADVANCED·dependent-on-6D·SPEC_ONLY-core 66 (+4); 6B ADVANCED·SPEC_ONLY 54 (+6,
  schema-face BANKED-earned within-family, combinator-type-algebra RETIRED); 6C BANKED 63 (+3, fate firm,
  bbnf-side viability undecided a 2nd pass); 6D ADVANCED·mandatory-FLOOR·SPEC_ONLY 61 (+6, floor necessity
  proven on proxy, cross-language fold never run). §5 stratification 66 — a now-partly-evidenced hypothesis
  (one measured-soft input, one measured-tautological-on-wrong-face input, one OPEN cross-lock row).
- **Pass-3 non-negotiable minimum (both critics):** (1) build the actual `mirror-map.toml` manifest linter and
  drive the TS surface **behaviorally** (not a Rust↔Rust proxy); (2) measure gen-vs-hand LOC on the
  **tape-layout** face (not the type face), build the three 6B faces, demonstrate AoS↔SoA expressibility-or-
  blocker, and run the **cross-language digest fold at least once** (6D's never-run mechanism); (3) run the
  wasm-SIMD-vs-TS-native-scalar comparator (skipped twice); (4) adjudicate the **LOCK-4C combinator-layout
  fork** before GESTALT-P consumes the lock. Fix 6A's impedance-cap arithmetic; build 6D's fuzzer teeth so
  gate-(a) can fail.

### Pass 1 (2026-07-16) — agglomerated

- **Artifacts:** FAMILY-SPECS (synthesis: 4 family specs + §5 incompatibility map + stratification table + 2
  briefs); research-{A,B,C,D,priorart}; PROTO-1 (6C R-A NEON-crossing decider, RAN_MEASURED); PROTO-2 (6A T2
  surface-correspondence, RAN_MEASURED); CRIT-1 (within-lock: 6A 62 / 6B 48 / 6C 60 / 6D 55); CRIT-2
  (cross-lock: 6A 70 / 6B 55 / 6C 72 / 6D 60 / §5 stratification 58).
- **PROTO-1 disposition (6C, R-A):** genuine, cold, paired, parity-exact (structural-index len 30318/93731/
  223235 identical NEON vs SIMD128). **R-A resolves NEGATIVE decisively:** wasm SIMD128 at 0.34–0.47× native
  NEON (native 2.1–3.0× faster). **SETTLED:** the NEON win does not cross; `simd-scan/wasm.rs` is a genuine
  dual implementation on the hottest surface. **NOT SETTLED (the wrong comparator):** wasm-SIMD vs
  TS-native-scalar — the browser-deployable bbnf-side decision — was never made. Confirms 6C is not an
  independent primary family; it collapses into the wasm-crossing of a generated/mirrored tape.
- **PROTO-2 disposition (6A, T2):** RAN but PARTIAL. The 47/61/0 ratio came from a *python syntactic differ*;
  the rustdoc-JSON "0.25s, runs" came from a **12-line toy Parser-twin**, not the real parse-that crate; the
  TS-side extractor was **never run**. **SETTLED:** clean-map ratio 47%/61%/0% with a 39% idiom allow-list;
  rustdoc-JSON runs on the already-nightly pin; "trees too idiomatic to mirror" refuted. **NOT SETTLED:** "T2
  runs" on the real surface; the manifest + T1 linter (unbuilt); the exemption-budget RED threshold (unset);
  Brief 2's green/red PAIR.
- **The prototype-allocation defect (both critics):** FAMILY-SPECS §6 named TWO briefs; only Brief 1 (6C NEON)
  was prototyped. **Brief 2 — the cheapest, fully in-tree green/red decider for Q2 (is 6D a mandatory floor) —
  was NOT run**; 6B and 6D each received a running-prototype allocation and neither ran (ADVANCED dispositions
  carried the SPEC_ONLY label).
- **Dispositions set:** 6A ADVANCED·dependent-on-6D 62; 6B ADVANCED·SPEC_ONLY 48 (combinator-type-algebra
  RETIRED); 6C BANKED 60 (primary refuted by measured R-A; crossing folded into the 6B/6D tape stratum); 6D
  ADVANCED·mandatory-FLOOR·SPEC_ONLY 55 (pure-6D BLOCKED for the tape). §5 stratification 58.
- **Pass-2 non-negotiable minimum:** Brief 2 (the green/red PAIR); the 6B decisive number (gen-vs-hand,
  proving the grammar→layout-fact seam exists); the 6D deciders; adjudication of the LOCK-4C combinator
  layout-gen contradiction and the LOCK-9 kernel double-home. The two green probes measured corners that
  *avoid* the plane's central bet.
</content>
</invoke>
