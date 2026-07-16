# LOCK-6 — THE ISOMORPHISM PLANE: ADVERSARIAL CRITIC 1 of 2 (within-lock), pass 3

*Critic 1 weights the WITHIN-LOCK view: internal soundness of each family spec, and whether the
pass-3 prototypes actually tested the sharpest uncertainty. Binding cross-lock frame (XL-A..H) is
treated as fact; challenges to it are routed to the gestalts, not re-litigated here. Every number
below names its artifact; base-pin `ce51c36fc` per XL-H.*

---

## 0. TWO PLANE-LEVEL FINDINGS THAT PRECEDE THE FAMILIES

### F-STALE — the synthesis is stale against its own prototypes; both "next-pass" briefs RAN inside this pass.

The FAMILY-SPECS.md I am critiquing was frozen **15:26**. Its §6 names the two "sharpest uncertainties
for the **next** pass": **Brief 1** — RUN the 6C wasm-SIMD128 vs TS-scalar comparator ("scaffolded but
UNRUN a fourth time"); **Brief 2** — BUILD the 6A `mirror.toml` linter ("never built… the single largest
remaining SPEC_ONLY crown"). Both were then **executed within this same pass directory**:

- `PROTO-1.md` (**15:47**) — the 6C comparator **RAN_MEASURED**: parity on every doc; wasm-core-home
  FALSIFIED for JS-string sources (marshalling 72–90% of `wasmFull`), TS-native-home holds; kernel wins
  4.7–7.3× on `scanOnly`; crossover ~2 MB is a content artifact. **This is Brief 1, discharged.**
- `PROTO-2.md` (**15:57**) — the 6A linter **RAN_MEASURED**: `cap == exemptions == 13 = 4+2+1+2+4` on the
  clean tree; RED on both synthetic extras (2/2); a **real** `wasm-pack --target nodejs` + `serde-wasm-bindgen`
  + Node drive of `ts/path.ts` (not the `compile_path_native` proxy) showing AGREE=2 / DIVERGE=3,
  names-green-behavior-red confirmed. **This is Brief 2, discharged.**

Consequence: **the synthesis materially understates 6A and 6C convergence.** §1 still calls 6A's linter
"STILL UNBUILT… the single largest remaining SPEC_ONLY crown in the plane"; §3 still calls 6C's comparator
"scaffolded-but-unrun a fourth time." Both are contradicted by artifacts sitting beside the synthesis in the
same directory. As critic-1 I fold PROTO-1.md and PROTO-2.md in and re-rate below; the agglomerator MUST
reconcile the registry against them, not against the stale §1/§3 prose. This is not a defect of the design —
it is a bookkeeping-lag hazard: a registry cut off the 15:26 synthesis will mis-record two crowns as
SPEC_ONLY that in fact RAN.

### F-NUMBERING — a three-way PROTO-{1,2} name collision; evidence is mis-mappable.

Inside `pass-3/` the token "PROTO-1"/"PROTO-2" denotes **three different things**:
- synthesis §0: "PROTO-1" = the 6B `emit_classify` machine (code in `proto-1-code/`); "PROTO-2" = the 6C
  wasm-scan **scaffold** (`PROTO-2/wasm-scan/` + `harness.html`).
- `PROTO-1.md` = the 6C comparator **run** (which consumes the code living in the `PROTO-2/` directory).
- `PROTO-2.md` = the 6A linter run (code in `PROTO-2/mirror-lint-6A/`).

So `PROTO-1.md` documents a **6C** run out of the `PROTO-2/` tree, while the synthesis's "PROTO-1" is a **6B**
artifact with no `.md`. A reader cannot map "PROTO-1" to one artifact. This is a concrete traceability defect:
disambiguate (rename to `PROTO-6B-emit`, `PROTO-6C-wasmscan`, `PROTO-6A-linter`) before the registry cut, or
perf/mechanism claims will be misattributed across families.

---

## 1. Family 6A — MIRROR-TREE

**Movement (folding PROTO-2.md, which the synthesis omits).** The crown the synthesis calls "unbuilt" is
**BUILT and RAN**: a self-contained `syn`-2 linter that syn-extracts Rust decl surfaces + `tsc`-emitted
`path.d.ts` names, resolves each non-identity bridge decl to a spine pre-image, computes `cap` and `exemptions`
independently, reproduces `13 = 4+2+1+2+4` without fitting, REDs `BogusPayload` (serde-predicate match, no
canonical `Bogus` pre-image) and `FooView` (no predicate), 2/2. And a **real cross-language drive** — wasm
cdylib + `serde-wasm-bindgen` + `node_drive.mjs` through the actual `makePathTag`/`PathError` of `ts/path.ts`
— confirms names-green-behavior-red. The pass-2 CRIT-1 gap "(vi) behavioral TS drive is still the intra-Rust
proxy" and FAMILY-SPECS gap "(i) the mechanism is STILL UNBUILT" are **both discharged by PROTO-2.md**.

**But three within-lock soundness defects survive, and one is in the running gate itself:**

- **[fit-to-count guard — a gate that both cannot-catch-growth AND re-introduces the tautology the family
  claims to dissolve]** The as-built gate is `green = exemptions == cap && cap == expected` with
  `let expected = 13usize` (`mirror-lint/src/main.rs:318,342`). The design (research-A Q1; FAMILY-SPECS 6A Q1)
  is emphatic that the cap is "**never a tuned scalar**" and "reproduces the live count **without fitting it**,"
  and PROTO-2.md's prose repeats "the model is not fit to the count." **The code contradicts the design:** the
  third clause `cap == 13` is exactly a tuned scalar. Two failures follow. (1) On *lawful* growth — add a spine
  type + its identity/`r(t)` mirror — `cap` and `exemptions` both rise to 14, `exemptions == cap` stays GREEN,
  but `cap == 13` fires RED: **the gate REDs a legitimate change** (a gate-fails-wrong). (2) The whole
  anti-tautology argument (relational `r(t)` with pre-image requirement) is the correct one and does NOT need
  `cap == expected`; pinning it re-imports the fit-to-count the family spent Q1 dissolving. The running
  "not-fit-to-count PASS" verdict is thus partially self-undercut. **The production gate must be
  `exemptions == cap` alone; the `== 13` literal is a probe scaffold that must not survive into the tranche.**

- **[the floor-necessity proof is real but confounded — it proves rust⊥rust, not rust⊥ts drift]** The
  DIVERGE=3 result (the "6D-floor necessity proof") has a *single* root cause PROTO-2.md itself names: the
  canonical offline checker leaves `TypeDesc::Named(_)` intact (`UnregisteredRule`) while the cdylib resolves
  `Named` eagerly via `registry.layout(id)` (`UnknownField`). That is a **Rust-vs-Rust semantic fork** (offline
  checker vs eager proc-macro resolver), surfaced *through* the wasm boundary, not a TS-transliteration drift.
  The thesis "a structural name-map is blind to behavioral divergence" holds regardless (the linter is green
  while behavior forks). But the specific evidence proves the floor is needed to catch **which Rust semantics
  the cdylib exposes**, which is a different (and arguably cheaper-to-fix) hazard than the Class-2 TS-routing
  drift the family advertised (`UnregisteredRule` vs `UnknownField` *because the TS side forked*). The brief's
  own 4/1→2/3 correction admits the surprise; the confound should be stated so GESTALT does not over-credit
  6A's Class-2 argument with a datum that is really about the two Rust frontends.

- **[incidental live bug, correctly surfaced]** `makePathTag` swallows the `PathError` it constructs and
  rethrows the raw wasm `JsError` (PROTO-2.md) — the TS frontend never surfaces a typed `PathError`. Real,
  in `ts/path.ts`; a 6A-execution fix, not a design gap. Good that it was found by the *behavioral* drive the
  structural linter cannot see — itself a second, cleaner floor-necessity datum than the confounded one above.

**Exact open gaps (6A):** (i) the `cap == expected` fit-to-count clause must be deleted; the gate proven to
GREEN on lawful growth and RED only on unbudgeted names. (ii) linter is a **standalone detached-target-dir
binary**, NOT wired into `xtask`/CI (same "ran-but-not-landed" status as 6B; the registry's CI-ownership item
is undischarged). (iii) CASE totality + injectivity (a `next`/`next_sibling`→one camelCase collision) unbuilt.
(iv) cross-repo CI ownership under F6 (per-repo manifest fragments + vendored corpus) — proposed, unbuilt.
(v) LOCK-3 provenance-manifest seam (single-source names skipped) is designed but not exercised end-to-end —
the linter classified idioms by local predicate, it did not consume a `cargo xtask regen` manifest. (vi) the
floor-necessity datum is confounded (rust⊥rust); a clean rust⊥ts drift example (e.g. a dropped TS union
variant) is owed to make the 6A⊗6D graft argument on its intended axis.

**Convergence: 72%.** Crown BUILT+RAN (the largest correction to the synthesis's own framing); cap-derivation
sound in design. Held short of converged by a fit-to-count clause in the running gate, no CI landing, a
confounded floor example, and CASE/F6 unbuilt. **Not BLOCKED, ADVANCED.**

---

## 2. Family 6B — SPEC-GENERATED-SKELETON (crown built at the classify FACE)

**Movement is real.** `emit_classify(spec, Backend)` (`proto-1-code/emit_classify.rs`, 375 LOC) is a genuine
one-machine-two-backend generator over a concrete backend-agnostic `LayoutSpec`; it reproduces the JSON
`FromSource` and Sheets `FromFlagTag` faces byte-identically, and the generated TS classify (`gen_json.ts`,
inspected) is a clean monomorphic switch. The AoS↔SoA "hard core" is genuinely dissolved into a
discriminator-provenance axis, demonstrated by run. This is the pass's largest SPEC_ONLY→RAN conversion and
I do not dispute it at the classify face.

**Within-lock soundness defects:**

- **[the monomorphism gate is low-information — it passes on the trivially-true direction]** The mandated R3
  refutation gate (`mono_check.mjs`) splices the **generated** classify into a hand `TapeDoc` harness and
  asserts `%HaveSameMap` + `%HasFastProperties` pre/post-walk. But the generated classify is **byte-identical
  to the hand classify** — so "same V8 hidden class" is a near-tautology (identical source ⇒ identical shape).
  The R3 risk research-B R2 actually names — "a generator that emits a *post-ctor field assignment* or a
  *switch V8 cannot lower*" — is precisely the case a byte-identical reproduction **cannot** exercise. The
  gate therefore confirms "byte-identity implies shape-identity" (trivial), not "the generator is robust to
  emitting V8-hostile output at a face where no hand reference pins it." The synthesis's phrase "monomorphism
  preserved **by construction** (generated ≡ hand, byte-for-byte)" is honest about the mechanism but is exactly
  why the gate is not yet a real test of the crown's risk. The sharp uncertainty moves to the full decoder (next).

- **[reproducibility hole — the monomorphism evidence depends on an artifact not in the deliverables]**
  `mono_check.mjs` imports `./gen_tape.mjs`; that file is **absent from `proto-1-code/`** (only `emit_classify.rs`,
  `gen_{json,sheets}.{rs,ts}`, `mono_check.mjs` are committed). The `%HaveSameMap` PASS the synthesis cites as
  gate-(b) "PASSED" cannot be reproduced from the committed artifacts — the hand harness the generated classify
  was spliced into is not present. Either commit `gen_tape.mjs` or the gate-(b) claim is SPEC-adjacent, not
  reproducible.

- **[elegant-reduction watch — "scale from the classify FACE to the full decoder" is the load-bearing hard
  part, and it is untested]** PROTO-1 generated **27 TS classify LOC → 0** and **138 Rust LOC → 0** — but the
  PROVE-14 decoder is **625 hand TS LOC**; ~598 (walk/leaf/nav/pointer/load/verify) remain the majority of the
  face and are ungenerated. The monomorphism gate must **re-pass on the full generated decoder**, where the
  generated code is NOT byte-identical to a known-good hand version — the one condition under which the gate
  becomes informative (see above). "6B's crown is BUILT" is true **for the classify face** (the R3-critical
  monomorphic slice); it is not true for the decoder. The synthesis says this in gap (i), but its Disposition
  headline "crown BUILT" and §5's "the pass converts it from SPEC_ONLY to RAN" over-reads a face as the whole.

**Exact open gaps (6B):** (i) scale classify FACE → full decoder (~598 LOC) and re-run the mono gate on
non-byte-identical generated output. (ii) commit `gen_tape.mjs`; land `emit_*` into `xtask regen`, delete the
hand faces, prove `regen∘regen` byte-identity + a **real-tape** `doc.walk` fp (PROTO-1's twitter fp `986912547`
held via a *classify-independent* lane — `tape_dump.rs` was not rebuilt). (iii) R2 flag-plane collision (one
`u8` cannot carry a discriminator tag AND an escape bit for a string-escape leaf inside an operator tower) —
resolvable by a spec-declared bit-partition, unbuilt. (iv) Face I is **partial** — record-shape+offsets
generated, fixed flag-bit *values* (`GRAMMAR_BIT0`) hand-authored policy → LINTED not generated (the §5
partition line, honestly conceded — the strongest "drift impossible on the declarative half" claim is narrowed
to the schema+tier+offsets sub-half). (v) the TS `emit_direct_sink` symmetric obligation (Q3 measured Rust-only)
and cross-repo regen (F6) unbuilt. (vi) Q4 VM instruction-stream is cross-lock (LOCK-4C/8C) — correctly routed,
not a within-lock gap, but flagged so it is not read as a 6B convergence item.

**Convergence: 70%.** Real crown conversion at the classify face; but that is the R3-critical *slice*, the
monomorphism gate is low-information on the easy direction, the reproducing harness is missing from artifacts,
and the majority (full decoder + xtask landing + R2 + Face-I generation) is untested. **ADVANCED, not BLOCKED.**

---

## 3. Family 6C — WASM-CORE (BANKED; comparator now RAN)

**Movement (folding PROTO-1.md, which the synthesis omits).** The decision-relevant comparator — the thing
owed a fourth time — **RAN**: kernel parity-exact on all 12 docs; `scanOnly` beats TS-scalar 4.7–7.3× on real
docs; but `TextEncoder` UTF-16→UTF-8 marshalling is **72–90%** of `wasmFull`; TS-native wins twitter (631 KB)
and citm (1.7 MB); wasm nets ahead only on canada (2.25 MB, ASCII + branch-hostile), a **content** artifact.
Gate (b) FAILS (crossover ~2 MB > realistic bbnf batch doc sizes). **Verdict: TS-native-home for the bbnf
side, given a JS-string source.** This *strengthens and confirms* the BANKED disposition with a running number
— the pass-2 R-A-negative (wasm SIMD 0.34–0.47× native NEON) is now joined by a browser-side falsification of
wasm-core-home. Good, decisive, honest (paired-interleaved, load recorded, absolute noise disclaimed).

**Within-lock soundness notes:**

- **[the synthesis §3 is entirely stale on 6C]** It still frames the comparator as "scaffolded-but-unrun a
  fourth time" and lists "the decision-relevant comparator… unrun a fourth time" as residual gap (i). That gap
  is CLOSED by PROTO-1.md. The registry must not carry it forward.

- **[PROTO-1.md's decisive caveat re-opens a narrower question, not the whole family]** "If the bytes are
  already resident in linear memory (produced/owned in wasm, no per-call `TextEncoder`+copy), `scanOnly`
  governs and v128 wins 4.7–7.3× at every size." So the 6B⊗6C graft decision is correctly re-stated as **"where
  do the bytes live?"** — JS-string source → TS-native (measured); wasm-resident bytes → wasm-core (predicted,
  **unmeasured**). This residual is real: whether the tape-crossing stratum can deliver source bytes
  wasm-resident (bytes produced in wasm) is untested, and it is the *only* lane where 6C's kernel win survives.
  BANKED is correct for JS-string sources; the wasm-resident lane is an open sub-question, not a settled loss.

- **[crossover is a soft single-point inference]** ~2 MB is inferred from 3 real docs (citm TS / canada WASM)
  plus a noisy synthetic sweep whose only WASM point (4 KB) is dismissed as a ts spike. Direction (TS wins at
  realistic sizes) is robust; the "~2 MB" number is content-dependent and should not be cited as a size law
  (PROTO-1.md says exactly this — no burial).

**Exact open gaps (6C):** (i) the **wasm-resident-bytes lane** (bytes produced/owned in wasm, no per-call
marshalling) is unmeasured — the one regime where the kernel win survives; the true 6B⊗6C home decision hinges
on it. (ii) the two-parses-through-one-growing-instance corruption case is **still unexercised** — PROTO-2's
static-`bss` arena side-steps `memory.grow` rather than exercising it (Q2's detach hazard is confirmed by spec,
not by a running crash+copy-out-recovery in the harness). (iii) homing collision unpriced (`wasm/Cargo.toml`
already claims `name="bbnf-wasm"`; the proposed `crates/bbnf-wasm` does not exist). (iv) value.js-4.0.0 divorce
is in-tree real but **owner/F6-bound** (`tranche-u` WIP); the confinement oracle re-homes to latex-paper,
unverified there. (v) LIVE x86 `avx2.rs`/`avx512.rs` in `simd-scan` vs constitution-iv "x86 dead" — a LOCK-9
surface, correctly routed to gestalt.

**Convergence: 78%.** BANKED is FIRM and now carries a running falsification of wasm-core-home; the disposition
is settled, not aspirational. Held below converged only by the wasm-resident-bytes residual and the unexercised
grow-corruption case — both refinements to a settled BANKED verdict, not blockers. **BANKED.**

---

## 4. Family 6D — ORACLE-ISOMORPHISM (the mandatory FLOOR)

**Movement is real.** The cross-language fold RAN (research-D D1): raw IEEE-754 bits fold `b99874e8`
identically Rust↔V8; native-decimal-text DIVERGES on three enumerable classes (exp threshold, `-0`, subnormal
expansion); the divergence is proven to live in the *canonical-decimal spec*, not the fold machinery. The
in/out partition is checked-in data (`contract-6D.toml`). The pass-2 CRIT-1 gap "(i) the fold was never built
or run" is discharged.

**Within-lock soundness defects — the floor is advanced but its decisive verdict number and its own frozen
contract are not settled:**

- **[live unadjudicated contradiction inside a "frozen" contract — the number lane]** `contract-6D.toml`
  commits `number_fold = "f64 LE-byte mix"` (raw bits). Pass-2's registry-credited Q10 correction forbids raw
  bits ("fold spans/canonical-decimal, **NEVER** raw bits"). And research-D's **own** primary recommendation is
  a *contract-normalized decimal* (shortest-round-trip, positional-always, `+0`) with `alt_lane = ieee754-bits`
  as an **escape hatch** — which is itself a *third* position distinct from the checked-in `number_fold` bits
  commit. So three positions float (pass-2: decimal, never bits / research-D primary: contract-decimal, bits as
  fallback / contract-6D.toml: bits) and the checked-in "frozen" artifact picks the one the pass-2 correction
  forbids. The synthesis surfaces this (D2, routed to GESTALT) — **correctly not masked** — but a mandatory
  floor whose frozen contract contains an unadjudicated representation flip is, by definition, not converged.

- **[the decisive verdict number is unrun — elegant-reduction / null-hypothesis-by-default]** 6D "wins by
  default as the null hypothesis, defeated only by measured drift/maintenance cost." The **R5 debuggability
  comparison** — time-to-root-cause under a bare digest mismatch ("something differs," no "escape decoder
  off-by-one at line X") vs mirror-maintenance cost — is the single axis where 6D is *dominated*, and it is
  **unrun**. The pass measured only the *affirmative* proxies (added-grammar marginal ≈ 0 fold LOC;
  injected-divergence catch rates). The "reasonable point" verdict the whole plane turns on is thus asserted on
  the easy direction; the number that could defeat the null hypothesis was not taken.

- **[internal inconsistency: §0 claims a fuzzer ran; §4 gap (v) says the fuzzer is unbuilt]** FAMILY-SPECS §0
  D1 headlines "coverage-fuzzer 5/5"; §4 gap (v) says "the coverage-guided fuzzer teeth… the generator itself
  is unbuilt." research-D Q2 lists the fuzzer as a *recommendation* and "What a prototype must test" item 3
  lists the fuzzer-promotes-to-corpus loop as **owed**. So "coverage-fuzzer 5/5" is at best a fixed injected
  table over 5 hand-named classes, not a running coverage-guided generator — the §0 headline overstates the
  gate's teeth relative to §4/research-D. gate-(a) still exercises mostly the agree-among-correct direction
  (the pass-2 "gate cannot fail" concern is reduced, not eliminated).

- **[the affirmative economy case is partly SPEC_ONLY]** "6D's added-grammar cost is O(new variants)" rests on
  folding ≥1 non-JSON grammar's value tree; research-D states no TOML grammar exists in-tree and "end-to-end
  confirmation still owed." So the fleet-economy claim (K=0 for CSV/sheets) is *reasoned*, not run — a
  SPEC_ONLY anchor under an affirmative-cost claim. Acceptable as a design argument; must not be cited as
  measured until a non-JSON value tree folds cross-language.

**Exact open gaps (6D):** (i) the number-lane bits-vs-spans contradiction — adjudicate (a spec-probe folding
the residue + the `1e21` threshold both ways); the "frozen" `contract-6D.toml` cannot freeze until it resolves.
(ii) R5 debuggability verdict — the decisive "reasonable point" number — unrun. (iii) the fuzzer generator is
unbuilt; the §0 "5/5" claim reconciled against §4's "unbuilt." (iv) end-to-end fold of ≥1 non-JSON grammar's
value tree (the affirmative economy proof). (v) u64 widening is spec text (`[meta] digest="u64"`), not a diff;
fuzzer at 10⁶+ (the birthday-collision exercise) unrun. (vi) LOCK-7 N+1 reference for a seeded both-engines-wrong
case (the oracle proves agreement, not correctness) unrun.

**Convergence: 68%.** Floor confirmed and the fold RAN — a genuine, large advance. Lowest of the four because
its frozen contract holds a live representation contradiction, the one number that could defeat its
null-hypothesis default (R5) is unrun, and its gate teeth (fuzzer) and affirmative economy (non-JSON fold) are
unbuilt/unrun. **ADVANCED as floor; cost/benefit-as-chosen-floor measured on proxies.**

---

## 5. The partition line (§5) and the plane verdict

The plane's central deliverable is the partition line. Pass-3 now has **both anchor numbers with running
evidence**: 6B's classify-face gen-vs-hand (MEASURED byte-identical, `proto-1-code`) AND 6A's linter ratio
(`cap==exemptions==13`, RED-on-synthetic, PROTO-2.md) — the latter the synthesis §5 still calls "LINTER UNBUILT
(Brief 2)." So the line is now *drawable*, but the synthesis has not drawn it and is stale on the 6A side.
Two within-lock caveats on the map itself:

- **The 6D floor's necessity (map row "everything (behavior)") is proven, but by a confounded example (§1):**
  the DIVERGE=3 datum is rust⊥rust (offline checker vs eager resolver) surfaced through wasm, not the Class-2
  rust⊥ts routing drift advertised. The floor is necessary regardless; the *evidence* should be re-taken on a
  clean rust⊥ts drift before GESTALT credits 6A⊗6D on its intended axis.
- **The 6B⊗6C graft (map row "tape layout") spine now has a measured leg (6B classify face) and a measured
  refutation (6C JS-string wasm-core FALSIFIED), leaving one unmeasured lane (wasm-resident bytes).** The graft
  is not resolved; it is narrowed to a single open sub-question. This is progress, correctly not over-claimed.

**Prototype-evidence assessment (the pass-2 structural weakness, re-checked).** Pass-2 CRIT-1's headline
finding was "both pass-2 prototypes measured a **softer** target than the family's sharpest uncertainty."
**Pass-3 reverses this**: the 6B emit machine hit the R3-critical classify face; PROTO-1.md hit the exact 6C
decision (marshalling-bound wasm-vs-TS); PROTO-2.md hit the exact 6A crown (the linter + a real wasm crossing).
All four crowns moved SPEC_ONLY→RAN at some scale. The residual prototype-evidence weaknesses are now
*second-order*: 6B's mono gate on the byte-identical (easy) direction; 6D's decisive R5 number untaken; 6A's
running gate carrying a fit-to-count clause; 6B's reproducing harness (`gen_tape.mjs`) missing from artifacts.
These are real but they are refinements, not the "measured the wrong thing" defect of pass-2.

**Net (within-lock).** The plane advanced materially and honestly: four crowns ran where pass-2 had zero. It is
**not converged**. No family clears the converged bar (zero enumerated gaps + fresh audit + two clean passes),
the partition line is un-drawn, and each family carries concrete internal defects — 6A a fit-to-count gate
clause, 6B a low-information mono gate + missing harness + face-not-decoder scope, 6C an unmeasured
wasm-resident lane, 6D a live frozen-contract contradiction + an unrun decisive number. The most urgent
non-family action is bookkeeping: the registry cut MUST fold PROTO-1.md (6C) and PROTO-2.md (6A) — the
synthesis I critiqued does not — and disambiguate the PROTO numbering, or two crowns will be mis-recorded as
SPEC_ONLY that in fact RAN.
