# LOCK-6 — THE ISOMORPHISM PLANE · pass-3 · CRIT-1 (adversarial, WITHIN-LOCK weight)

*Critic 1 of 2. Mandate: internal soundness of each family spec + whether the prototypes tested the
SHARPEST uncertainty. Read against PORTFOLIO (LOCK-6 + Constitution), FAMILY-SPECS (22:44),
research-{A,B,C,D}, contract-6D.toml, and the four running probes. CROSS-LOCK (XL-A…XL-H) taken as
binding fact; challenges routed to GESTALT, not re-litigated. Two anchors independently verified at
HEAD (below). HARD CAP honored. This supersedes the stale 16:16 CRIT-1 (which critiqued the pre-22:44
synthesis).*

---

## §0 — THE CROSS-CUTTING DEFECT: the FAMILY-SPECS is STALE against its own two sharpest probes

The synthesis-of-record is dated **22:44**. Its two decisive §9 pass-4 briefs were then **executed the
same night** — **PROTO-2-6B (23:21)** and **PROTO-1-shipping-kernel (23:08)** — and their results are
**NOT folded into the family assessments**. Concretely:

- **6B §2 open question [1]** ("CLASSIFY FACE ONLY … the full decoder mono gate becomes real only on
  non-byte-identical generated output … the R4 falsifier could fire") is **DISCHARGED** by PROTO-2-6B:
  a `LayoutSpec`-generated, sha-distinct (337 lines changed) full `TapeDoc` decoder is V8-monomorphic
  on all four hot sites, folds to fp `986912547`/`3431570937`, and passes a **non-vacuous two-hazard
  negative control**. The R4 falsifier did **not** fire.
- **6C §3 open question [1]** ("measure the wasm-resident-bytes lane") is **DISCHARGED** by
  PROTO-1-shipping-kernel: a **MEASURED bimodal** verdict (resident wins 2.5–3.7× from ~8 KB batched;
  loses at 1 KB single = 0.9×).

A synthesis that enumerates as OPEN two questions its **own pass-3 probes have already answered** is
the exact registry-staleness pathology the FAMILY-SPECS header itself indicts (re: the "stale 15:26
synthesis"). **Action owed before GESTALT:** re-cut FAMILY-SPECS §2/§3 to fold PROTO-2-6B and
PROTO-1-shipping-kernel; the convergence numbers below already credit those discharges, but the
document of record does not. Bookkeeping RED, not a design RED — but load-bearing, because the gestalt
consumes this file, not the probes.

**Verified at HEAD this critique (Constitution-vii):**
- (i) `skinny/crates/bbnf-path` **ABSENT**; top-level `crates/{bbnf-path,bbnf-path-ts,core}` **present**
  → 6A's entire live pair grounds on the **legacy tree that archives at GN-0** (M2 confirmed).
- (ii) `PROTO-2/mirror-lint-6A/mirror-lint/src/main.rs:318` = `let expected = 13usize;`, `:342` =
  `let green = exemptions == cap && cap == expected;` → the running 6A gate's GREEN verdict is
  **literally gated on `cap == 13`** (§1[1] confirmed as a live fit-to-count clause, not merely a comment).
- (iii) `contract-6D.toml:17` = `number_fold = "f64 LE-byte mix"` (bits) → §4[1] in-file datum confirmed.

---

## §1 — Family 6A · MIRROR-TREE · convergence **72%** · ADVANCED, dependent-on-6D

The mechanism is genuinely BUILT and RAN (real `syn`-2 extractor + `wasm-pack` + `serde-wasm-bindgen`
+ Node drive) — substantial, and I credit it. But the family's **advertised value has not been
demonstrated**, and three of its four load-bearing claims are untested at the level that matters.

**Sound / credited.** cap and exemptions are computed over genuinely disjoint domains (cap = Σ
predicate-hits-with-preimage; exemptions = count of non-identity decls), so `13 = 4+2+1+2+4` reproduces
without the COMPUTATION being fit. Synthetic REDs (`BogusPayload`, `FooView`) fire 2/2. The
relational-exemption model is a real advance over research-A's weaker `cap=|I|=6` slot model (the
synthesis correctly superseded it).

**Enumerated open gaps.**
1. **The production gate has NOT been run (fit-to-count clause is live, not scaffolding).** The gate
   as-RAN is `exemptions == cap && cap == 13` — the `== 13` literal is load-bearing to GREEN, confirmed
   at `main.rs:318,342`. The synthesis asserts the production form is `exemptions == cap` ALONE, but
   that form was **never run**: no execution demonstrates GREEN on a **lawful growth to 14** (identity
   mirror added → both rise) while RED-ing an **unbudgeted 14**. The anti-fit claim is proven for the
   *cap computation* and unproven for the *gate*.
2. **I=5 class-closure is asserted, never stressed (the sharpest 6A uncertainty, unprobed).** The whole
   anti-suffix-gaming thesis rests on I being a CLOSED enumerated registry. No sixth-idiom attempt was
   run; there is no evidence a legitimate new idiom is *rare* (audit cadence) rather than *frequent* (in
   which case the reviewed manifest rots exactly as a flat allowlist would). "Growth is lawful only by
   identity mirror or a reviewed new class" is untested for whether real growth stays inside fixed I.
3. **No CLEAN rust⊥ts drift catch was demonstrated — the headline datum is CONFOUNDED.** The 2/3-diverge
   wasm drive is a **rust⊥rust** semantic fork (offline checker leaves `Named` intact → `UnregisteredRule`
   vs eager cdylib resolver → `UnknownField`) surfaced *through* wasm — the synthesis flags this itself
   ([2], INC-6). The one CLEAN rust⊥ts fork found (`PathError.format` alternatives-escaping, 11/13) lives
   on the render seam **6A declares OUT of contract**. Net: the linter's *positive* value — catching an
   organic rust⊥ts drift — is proven only by synthetic injection, **never on a real drift**; the two real
   drifts found are one it can't see (confounded) and one it disowns (render). Not fatal (its job is
   structural mirror, not behavior), but it means 6A's "names-green / behavior-red proof" certifies the
   **6D floor's necessity**, not 6A's own catch.
4. **Legacy-only substrate (M2, verified).** `cap==13` lives on `crates/{core,bbnf-path,bbnf-path-ts}`,
   archiving at GN-0. The anchor does not transfer to the tree-of-record without re-measurement on GN-1,
   which does not exist. Per XL-A licensed, but the plane's 6A anchor is measured on a doomed tree.
5. **The partition ARBITER (LOCK-3 provenance manifest) is a consumer-less substrate.** 6A's applicability
   boundary (single-source → skip; authored → lint) is *decided by* the LOCK-3 provenance manifest —
   which **does not exist** (routed as a `cargo xtask regen` byproduct). The live `fixture.rs:53-90`
   double-origin of `"statuses"` is cited as "the LOCK-3 pass REDs it day one" — **SPEC_ONLY**, that pass
   unbuilt. Elegant-reduction trap: "the linter reads LOCK-3's manifest" — and the manifest is the hard
   part, unbuilt.
6. **`makePathTag` swallows the typed `PathError` (incidental live bug).** The typed-presentation surface
   is dead code (rethrows raw `JsError`); the linter would pass a tree where the typed error contract is
   never exercised. Minor, real.
7. **BIFURCATED RUST REFERENCE (cross-lock, INC-6).** "Rust↔TS isomorphism" is undefined until an
   owner/LOCK names which Rust behavior is canonical. Correctly routed LOCK-1/3/7 + owner; not a 6A defect
   but it makes 6A's *target* ill-posed.

**Why 72%, not higher:** the mechanism runs, but the anti-gaming thesis (I-closure), the drift-catching
value (clean rust⊥ts), the production gate (no `==13`), and the arbiter (provenance manifest) are each
untested or unbuilt, and the sole anchor is on the archival tree.

---

## §2 — Family 6B · SPEC-GENERATED-SKELETON · convergence **80%** · ADVANCED (strongest family)

The sharpest uncertainty **[1] was tested and soundly discharged** (post-synthesis, PROTO-2-6B). This is
the family's decisive advance and raises it above where the 22:44 file places it.

**Sound / credited.** PROTO-2-6B is a well-run probe: `%HaveSameMap` true pre+post-walk, all 29,573
classify results unboxed Smi, four hot sites maglev-clean with no `kMarkedForDeoptimization`, fp
`986912547`/`3431570937` exact — AND the OptimizationStatus decode was **calibrated** (correctly noting
`%GetOptimizationStatus` is a *weak* falsifier under maglev, and `%IsSmi`-uniformity + `%HaveSameMap` are
the sharp probes). The two-hazard negative control (post-ctor field flips HaveSameMap; boxed classify
flips Smi-uniformity) is genuinely non-vacuous. Research-B's Q2 is also solid: **both discriminator
dialects already ship generated** (json/value.rs `Derived` + sheets/view.rs `Stored`, both carrying the
`@generated` banner) — the "one spec, both dialects" claim has real in-tree grounding, not assertion.

**Enumerated open gaps.**
1. **The generated decoder was NEVER crossed against REAL Rust-emitted bytes (the residual under the
   discharge).** PROTO-2-6B's `build_tape.mjs` **reconstructs** the four SoA planes in JS "faithful to
   skinny's Rust offset-tape builder" — it did **not** dump from a live skinny build. So the generated
   `tape.ts` was validated over a JS-rebuilt tape + the `JSON.parse` oracle (proving it is monomorphic
   and folds correctly), but the **end-to-end Rust-emit → generated-TS-decode byte-ABI crossing** is
   *inferred* (PROVE-14 proved it for the HAND decoder; the composition is asserted). Owed: run
   `tape_dump.rs` on a real skinny build and decode with the generated `tape.ts`, confirming fp parity.
   The probe honestly flags this in "Honest scope"; the FAMILY-SPECS does not enumerate it.
2. **The "non-byte-identical" condition is largely COSMETIC; the walk MACHINE was templated, not
   spec-varied.** The 337 changed lines are variable renames (`classify(cursor)→classify(cur)`), method
   reordering, module folding — "behaviourally equivalent." The DATA that varies with the `LayoutSpec`
   (K_* block + classify switch) is a small slice; the load-bearing walk/leaf/nav/pointer machine is "a
   shared template in the generator's own formatting." So the probe proves *this generator's output shape*
   stays monomorphic; it does **not** prove an arbitrary `LayoutSpec`-driven full emitter does. The R4
   risk is retired for the emitted shape, deflected for the general spec-emitter. Strong evidence, not total.
3. **PayloadDecode axis still unmodeled ([2] stands).** The spec must carry `PayloadDecode[] ∈ {SpanAsF64,
   SpanAsBool, LiteralTag, ArenaBytes}`; the probe hardcoded JSON's decode logic in its machine template
   rather than driving the three rule-keyed decoders from a spec axis. Unbuilt.
4. **Flag-plane collision unbuilt ([3]).** One `u8` cannot carry a discriminator tag AND an escape bit; a
   spec-declared bit-partition (and research-B R5's tag-width overflow at >255 rule kinds) is unmodeled.
5. **The crown itself — fold the 488-LOC JSON hand template into `general_descent`, and emit `tape.ts`
   from the same spec — is UNBUILT.** M1 stands: `emit_classify`/`LayoutSpec` have zero hits in skinny;
   the in-production kernel generates *Sheets* only, JSON's view is a **copied** 488-LOC template. The
   probe extends a scratch generator (`pass-3/proto-1-code/`); it is not landed in `xtask regen` ([4]),
   and the TS `emit_direct_sink` symmetric face is owed ([6]).
6. **VM-stream ([7])** — cross-lock, correctly routed to LOCK-4C/8C.

**Why 80%:** the sharpest V8-monomorphism uncertainty is discharged soundly and the dual-dialect claim
ships, but the real-bytes crossing, the PayloadDecode/flag-partition axes, and the actual crown
(JSON-fold + emitted `tape.ts`, landed in regen) remain — and the discharge tested a spec-varied classify
slice atop a templated machine, not a fully spec-generated decoder.

---

## §3 — Family 6C · WASM-CORE · convergence **80%** · BANKED (disposition firm)

The sharpest measurable uncertainty **[1] was tested** (post-synthesis, PROTO-1-shipping-kernel) — and,
importantly, the measurement **REINFORCED the block** rather than opening the lane. The banked disposition
is more settled, not less.

**Sound / credited.** The probe is honest and self-correcting: it re-labels the prior bespoke-ABI probe
(flat 2.9–5.8×, "no crossover") as an artifact of a persistent-OUTPUT ABI, and measures the **shipping
`crates/simd-scan` crate as shipped** (`scan_structural` allocating a fresh `StructuralIndex` + rebuilding
LUTs per call) → a real ~8–64 KB crossover. Parity is a HALT gate on every config. Cleaner than the
FAMILY-SPECS' flat "4.7–7.3×."

**Enumerated open gaps.**
1. **The architectural precondition remains CIRCULAR — the measured win is economically STRANDED.** The
   probe measured "IF bytes resident THEN win ≥2× at ≥8 KB batched." It did **not** resolve HOW bytes
   become resident for a real bbnf consumer *without* the refuted 6C-as-primary pipeline (§7). INC-1 is
   **confirmed, not dissolved**: the crossing pays only in the resident regime, and the resident regime
   requires the whole parse→tape pipeline in wasm (0.34–0.47× native NEON, refuted). The BLOCKED-candidate
   reasoning STANDS after this probe.
2. **Consumer-less substrate (Constitution viii / Y.13).** The surviving win's home — "the batch/server
   face, corpus resident in linear memory" — is **named but has no in-tree consumer**. No bbnf batch/server
   entry point produces wasm-resident bytes. The lane is asserted, not demonstrated.
3. **The "fixable in the kernel API" claim is SPEC_ONLY.** The 1 KB-single 0.9× loss is attributed to
   per-call fixed cost (LUT rebuild + `StructuralIndex` alloc), and a `scan_into(&mut StructuralIndex)` +
   LUT-hoist is asserted to "push the crossover down." **Unmeasured.** "A bounded kernel-API amendment,
   not a new subsystem" is optimism anchored to no run — flag as SPEC_ONLY.
4. **Realistic bbnf relevance is thinner than "batch/server wins" suggests.** bbnf batch docs are KB-scale
   grammar files (per PROTO-1-browser). At 1 KB **single** the win is 0.9× — below 2×. The win appears only
   BATCHED or ≥8 KB, i.e. the aggregate regime a one-grammar-at-a-time compile does not hit.
5. **Grow-hazard [2]** confirmed by SPEC, not by a running crash; the ≥2× copy-out margin is inherited,
   not re-measured under `memory.grow()`. **Homing collision [3]** unpriced. **x86 in `simd-scan` [4]** →
   LOCK-9. **XL-D tape-decoder-walk-vs-combinator distinction [5]** named but not consumed by GESTALT.

**Why 80%:** as a BANKED family, the disposition is firm and the sub-question is now measured — but the
measurement reinforced the wall (win ⇐ resident ⇐ refuted primary), the paying consumer is unbuilt, and
the escape hatch (kernel-API fix) is SPEC_ONLY. The banking is correct; the residual is honest.

---

## §4 — Family 6D · ORACLE-ISOMORPHISM · convergence **76%** · ADVANCED (mandatory FLOOR)

The FLOOR is genuinely RAN (bit-identical fold over 3 JSON corpora, **two independent parsers** — the
N-version property PROVE-14 lacked). Solid. But **every direction that could DEFEAT the family is reasoned,
not run** — and the constitution demands the null hypothesis be *defeated by measured cost or it wins*.

**Sound / credited.** The value-plane fold is well-designed: UTF-16 code units both sides (astral hazard
named and dodged), `f64→to_le_bytes` with NaN→sentinel, order-independent XOR for objects. Bit-identical
on twitter/canada/citm. Research-D's number-fold invariant (valid JSON folds identically iff both sides
correctly-rounded; Eisel-Lemire ≡ V8 strtod) is correct and cited.

**Enumerated open gaps.**
1. **The number-lane in-file datum [1] is OVERSTATED as a "live contradiction."** `contract-6D.toml:17`
   commits bits, and the synthesis frames this as an unresolved "three positions floated (bit→span→bit) …
   cannot freeze." But research-D §3 + the contract's own `[canonicalize]` block (`:31-34`: NaN→sentinel,
   signed-zero rule) + `may_either` (`:40`: canonical text RECORDED, not gated) **already resolve it
   cleanly**: fold the DOUBLE (bits), record decimal text as may-either. The `1e21`-threshold worry is
   MOOT for the bits fold (both sides parse ≥1e21 to the same f64; only decimal TEXT diverges, and text is
   may-either). Genuine residual is narrow: **run the golden-vector probe for the 3 canonicalization
   residues** (NaN payload, −0, subnormal) + ratify the contract-vs-prior-adjudication (the pass-2 "no raw
   bits" correction) at GESTALT. I **downgrade [1]** from "cannot freeze" to "one unrun golden-vector
   probe + one ratification" — NOT a design instability.
2. **R5 DEBUGGABILITY UNRUN — the verdict-deciding number (the sharpest 6D uncertainty).** The one axis
   where 6D is admittedly *dominated* — time-to-root-cause under a bare digest mismatch ("something
   differs") vs 6A mirror-maintenance — is unmeasured. The entire "reasonable point" partition (§5) the
   plane turns on is asserted only on the affirmative (economy-win) direction. Elegant-reduction gap: "6D
   wins the maintenance economy" reads *and-then-the-debuggability-cost-is-acceptable*, the hard half
   unmeasured. **CRITICAL within-lock.**
3. **Fuzzer teeth REASONED, not RUN [3].** The "5/5 in ≤16 inputs" is a fixed injected table; the
   coverage-guided generator is unbuilt. The static-corpus 2/5 IS run; the fuzzer 5/5 (the arm that
   *guarantees* every feature is emitted) is a prediction to falsify. Detection-economics headline half
   SPEC_ONLY.
4. **The RAN fold used u32; the contract demands u64 [4] — a gate that silently passes at scale.** Birthday
   collision floor ~77k inputs; at fuzzer scale gate-(a) silently passes on divergent input. The contract
   says "widen" (`:7`), but the widening was not run. A gate whose collision floor is below the corpus
   scale it must protect is a failure-mode-checklist hit ("gate that cannot fail correctly at scale").
5. **The K=0 fleet-economy — 6D's null-hypothesis-DEFEAT datum — is REASONED, not measured [6].** "Marginal
   cost per grammar = O(new value variants), 0 LOC for JSON-alphabet grammars" is the whole argument that
   6D amortizes where 6A/6B re-pay. But **no non-JSON grammar's value tree was folded** (no TOML/CSV
   in-tree). The central economic claim for the floor is unrun.
6. **Agreement≠correctness [5]** now CONCRETE (INC-6 bifurcated Rust ref is exactly the both-engines-disagree
   case 6D cannot adjudicate; LOCK-7 N+1 unrun). Correctly routed cross-lock.

**BLOCKED note sound:** pure-6D-for-the-tape correctly BLOCKED (INC-2); 6D-as-FLOOR mandatory, not blocked.

**Why 76%:** the floor is RAN and sound and its mandatory status is secure — but the debuggability
verdict-decider (R5), the fuzzer teeth, the u64 widening, and the second-grammar fleet economy are all
unrun, and those are precisely the numbers that DEFEAT-or-CONFIRM the null hypothesis the constitution
requires. The affirmative direction is proven; the defeating directions are reasoned.

---

## §5 — THE CENTRAL ARTIFACT (§5 stratification / §6 INC-map / §7 BLOCKED-ledger)

The partition line IS the deliverable, and it is well-constructed — but its **arbiter is unbuilt** and
its **spine's paying regime has no consumer**.

- **§5 partition.** Clean and defensible (6B generates declarative half, 6A lints hand-authored half, 6D
  floors both, 6C crosses value-plane zero-copy). **Defect:** the boundary arbiter (INC-3) — the LOCK-3
  provenance manifest that decides single-source→6B/skip vs authored→6A/lint — **does not exist**. The
  partition is design-complete but not mechanized; the boundary-drawing organ is a consumer-less substrate.
  **Second defect:** the spine's paying row (value-tape crossing, batch/server resident bytes) has no
  in-tree consumer, and one anchor (6A `cap==13`) is on the archival tree. Sound as a *design*; two of its
  load-bearing organs are unbuilt or mis-treed.
- **§6 INCOMPATIBILITY MAP.** The six INCs are well-reasoned and mostly measured (INC-1 measured, INC-4
  prior-art-unanimous, INC-5 a permanent measured non-mirror). INC-6 (bifurcated Rust ref) is the sharpest
  and correctly routed cross-lock. **Sound — no challenge.**
- **§7 BLOCKED-ledger.** Correct application of the "missing primitive as hard as the original problem"
  test. 6C-as-primary BLOCKED (resident-source primitive), pure-6D-for-tape BLOCKED (INC-2), 6A/6B/6D not
  blocked. **Sound — no challenge.**

---

## §6 — VERDICT

| Family | Conv. | State | The one gap that most holds it back |
|---|---:|---|---|
| **6A** MIRROR-TREE | **72%** | ADVANCED | Production gate (`==13` removed) + I-closure + clean rust⊥ts catch all UNTESTED; arbiter unbuilt; legacy-only anchor |
| **6B** SPEC-GEN-SKELETON | **80%** | ADVANCED | Sharpest [1] DISCHARGED soundly, but generated decoder never crossed against REAL Rust bytes; crown (JSON-fold + emitted `tape.ts`, landed) unbuilt |
| **6C** WASM-CORE | **80%** | BANKED | Measured win reinforces the block (resident ⇐ refuted primary); paying consumer unbuilt; kernel-API fix SPEC_ONLY |
| **6D** ORACLE-ISOMORPHISM | **76%** | ADVANCED (floor) | R5 debuggability (the verdict-decider) + fuzzer teeth + u64 + 2nd-grammar economy — every DEFEATING number unrun |

**Two probes tested the sharpest uncertainty soundly** (6B full-decoder mono; 6C resident lane) and should
be CREDITED — but the FAMILY-SPECS does not yet fold them (§0). **Two families did NOT probe their sharpest
uncertainty** (6A's I-closure + clean-drift-catch; 6D's R5 debuggability), leaving the plane's central
"reasonable point" verdict asserted on the affirmative direction only. The partition line (§5) is a sound
design whose arbiter and paying-consumer are unbuilt. No family is BLOCKED beyond the two correctly banked;
none stalls at a wall; convergence-talk is legitimate but **premature at 100%** — average ~77%, held there
by unrun defeat-direction numbers, not by architectural doubt.
