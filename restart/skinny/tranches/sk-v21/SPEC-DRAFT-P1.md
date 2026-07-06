# SK-V21 SPEC-DRAFT-P1 — The Convergence-Loop Re-Founding (pass-1 synthesis)

Date: 2026-07-05. HEAD: `734bfec51` (charter commit; no code has landed since SK-V20's close at
`05302109d` — cutover commit 0 was never executed, per PASS-ALPHA.md:3-4 and report 35:6-9).
Evidence base: pass-0 reports `research/01-32`, gap-fills `research/34-37`, adversarially verified
by 30/31 (13/14 historical and 13/15 now-state claims CONFIRMED; two corrections consumed below).
This draft amends the SK-V20 TW0-TW6 manifest wave by wave; where it supersedes, it says so and why.

Accounting note, updated at pass-1 close: report 33 (`33-cold-truth-profiling.md`) now EXISTS on
disk — the recovered cold-per-parse JSON+CSS capture (json_canon_bench, N=200, loadavg 8.3-10.2).
It joins report 21 as the profiling truth of record and supersedes 21's warm JSON numbers. Both
captures ran above the charter's loadavg<2: every performance ranking in this draft remains
DIRECTIONAL until the TW0 quiet re-capture runs (32 Part IV, U1). Where PROTO-P2's private harness
(loadavg 16-17) and report 33 (loadavg 8-10) disagree, the quieter capture governs (CRIT-P2-v2).

---

## §0 — The charter, restated against the evidence

A fully generalized parser generator that bests every SOTA parser for its targeted structured-data
languages, by: (1) backend-agnostic core, zero grammar-specific code in generic crates; (2) a
specializing emitter in the dav1d discipline — reference Rust, cost-model-selected grammar-agnostic
aarch64 kernels, checkasm bound to liveness — with the honest asm bound now quantified: hand asm
buys 0-5% over well-shaped intrinsics on Apple silicon generally, except at register-pressure
failures AND at instruction-selection misses the intrinsic cannot survive — P5 measured one such
exception on our own classify kernel (~10%, DIRECTIONAL, loadavg 17-21; static op-count 40 vs 32
is the load-independent witness) (25:263-271; P5); (3) full type resolution — a genuine rank-1 HM core over the two open-ended surfaces,
structural projection for the rest (28 §6); (4) a first-class Value API — lazy + eager over ONE
tape, one grammar-derived path alphabet, every comparison asymmetry disclosed at the claim site.
NO workarounds, NO stubs, NO legacy code; aarch64/Apple-M only; cold benchmarks only; ONE codegen
path. The "hand-rolled ASM" half of the aim is scoped explicitly (per 32's standing-aim gap): the
emitter's default emission is NEON intrinsics; an `asm!` emission path is admitted per-kernel only
on a checkasm-proven compiler failure, and prototype P5 prices that door before the plan freezes.

## §1 — The master pathology and its cure (binding on every wave)

The dig is unanimous on the single failure mode: **substrate-without-consumer**. Fourteen instances
across eighteen tranches (09:190-206); the #1 chronic anti-pattern of the AK→AV retrospective
(07:174-192); AI built six waves on infrastructure its own plan documented as dead-on-arrival
(30 claim 4); the AO structural-dispatch chain landed at 0% parse impact and was deleted wholesale
at AQ.5 (04, 30 claim 3). The one executed cure in the whole corpus is the W→X→Y→Z arc
(git-tagged, 30 claim 14): the **Y.13 consumer-invariant test** — an exhaustive-match gate over
every emitted decision variant that fails to compile until each variant has an observable consumer
(15:17-24; 35 N21).

**The cure, as standing law for SK-V21:**

1. Every **substrate/kernel/value wave (TW1-TW4)** closes on a **consumer-invariant runtime gate in
   `crates/`** — never on a document, never on a source grep. The gate template is Y.13's: exhaustive
   over the wave's emitted decision surface (kernel selection, tape-layout shape, value-plane
   variant), plus runtime evidence of activation — and, per P1's lesson, liveness alone is
   INSUFFICIENT: the gate is liveness AND a measured ≥3% win (a scan proven live under `cargo expand`
   still lost). Hygiene/stack/type waves (TW0, TW-S, TW5, TW6) close on their own named gate class
   (build+bench parity, oracle green, falsifier-fires); structural LOCKS may be path greps — that is
   a lock class, not a wave-close class, and the two are deliberately distinct (07 §5.3).
2. **No substrate lands without its consumer in the same wave.** The four-hop pipeline — mined IR
   fact → pluggable cost model → emitter selects body shape → consumer reads the emitted const —
   closes all four hops in one wave or the wave does not open (07:189-192).
3. **Profile-driven over spec-driven**: the corpus scores profile-driven tranches 2/2
   (BEAT-or-peak, zero dead substrate) against spec-driven 12/14 dead-substrate instances
   (09 §C4). Every wave benches every grammar (the CSS/Sheets panic that JSON-only closes hid —
   09 §C5 — is the standing counterexample).
4. "Complete with recorded misses" is not a close state (09:204-206). A wave that cannot close is
   re-planned under a new letter, never absorbed (memory: new-tranche-new-doc; 08 meta-lessons).

## §2 — The performance thesis, rebuilt on dig truth

The pre-restart campaign's own artifacts settle the architecture question:

- **fn-per-rule recursive descent won.** The AW-V.W2.1 prototype (`bbnf-json-prototype`, 2,246 LOC:
  RD + inline SIMD kernels + one monomorphic visitor, no interpreter) beat sonic-rs 5/5 on every
  JSON entry at 0.89-0.94× sonic ns/iter, 91.15% self-time in one monomorphized symbol (07 §3, §5.1).
  The DTA/tape-automaton family is settled-negative: non-uniform state bodies (40-400 LOC) cannot
  compile into fast code (07 §4.1). The generalization mechanism is shape classification from IR
  (12-variant ShapeTag; 10/15 hand-tunings IR-derivable — 07 §5.2).
- **The historical peak is AU**: twitter 1967 MB/s, 1.63 cyc/byte (09:144 table; report 09's prose
  "0.615" is bytes/cycle mislabeled as its inverse — the arithmetic 3.2e9/1967e6 ≈ 1.63 settles it),
  0.91× sonic — and its primitive set is recorded: flat AoS `Vec<TapeRec>` (one 16-byte store per
  push), unified `push_leaf_with(kind, PayloadData)`, `.map(|_| ())` elimination, per-grammar
  capacity fingerprint (09 §C1, §D1). Every tranche after AU regressed from it. AU was measured
  under the warm harness regime, was never re-derived by the verifiers, and is contamination-
  adjacent (30 claim 6) — it is a DIRECTIONAL aspiration, never a gate. **The single governing JSON
  floor is the TW0 fresh cold baseline**; every no-regression clause in this spec binds to it and
  to nothing else. An AU-relative cold capture (fused ns/B vs AU's recorded coordinates) is taken
  at TW0 for the ledger, as context, not as a close condition.
- **The BEAT-sonic lever is now cold-corroborated, per-corpus, with one honest loss.** The old warm
  0.99×-geomean provenance is retired: P2 measured the actual fused single-pass cold, and report 33
  independently measured the identical entry point (`direct_struct::track1_digest`) on the mandated
  `json_canon_bench`. The doubly-sourced cold facts: fused single-pass beats the tape-then-walk
  reconstruction default by ~3.2× (the reconstruction tax is real and cold-confirmed), and beats
  sonic-rs cold on twitter and citm (~0.5-0.6× ratios). **canada LOSES to sonic cold** on the
  quieter capture (33: 9936 vs 11794 Mbps, 1.19×) — the eager `materialize_f64` float tax, the
  sharpest optimisation signal in the corpus, pending TW4's Eisel-Lemire/SWAR decode. P2's private
  4/4-sweep claim is refuted on canada and is not carried. The lever is stated per-corpus, never as
  a sweep or a geomean. **Fused parse+value single-pass emission becomes the default
  materialization lane** (09 §D2) — de-risked for its core claim; the spec's actual default
  construct (fold-over-lazy-cursor, §4) remains an OPEN TW3 sub-gate, unbenched.
- **The structural pre-scan presize is resolved NEGATIVE: DELETE.** Six attempts at a universal
  stage-1 pre-pass, six losses (07 §5.4); eager whole-input scan regressed −64% on twitter (09 §D7);
  and P1 is the seventh and terminal loss: `OneShotSimd` — proven LIVE by three witnesses (runtime
  backend, `cargo expand`, `nm`) — pays a full stage-1 pass plus a throwaway positions Vec purely to
  size a tape, regressing 1.6-2.4× directional; `Sampled` presize ties `GrowOnly` within drift, so
  removing the 7-8 realloc growth events buys nothing measurable. The decision is closed, not
  carried to TW4: **delete `Sampled`/`Exact`/`OneShotSimd`, keep `GrowOnly` only** — the deletion
  rides TW1's N24 reclaim sweep. The TW0 quiet re-capture (interleaved plans, stddev/CI reported)
  certifies the magnitude and the Sampled-tie noise floor; it does not reopen the decision. A
  CONSUMED stage1→stage2 index is a different design and was never refuted (P1's scope boundary);
  it may only return via the §5 cost table with a measured win.
- **Where the time actually goes** (21, directional): the live JSON parse is entirely scalar
  single-pass; number decode is hand-rolled, NOT serde_json (the 19-vs-21 conflict is adjudicated
  for 21 — 31 item 11; 34 headline). Number materialization + digest costs ~0.6 cyc/byte, +45%
  over parse_only — that ~30% of the typed figure is the legitimate target (SIMD/SWAR + Eisel-Lemire
  via the already-vendored lexical-core path — 23 §3). This matches the external frontier exactly:
  simdjson's stage 2 / materialization dominates on Apple silicon (canada: 1.847 c/B stage-2 vs
  0.547 stage-1 — 22 §2); On-Demand's margin comes entirely from unread substructure and does not
  transfer to full materialization (22 §1) — bbnf's full-typed-AST mandate means the win lives in
  number decode and per-value store cost, not structural skipping.
- **The benchmark truth-floor**: the cold-per-parse JSON lacuna (the committed 8349 Mbps vs 25,448
  warm ~3× harness gap — 21:50-77) is CLOSED in kind: `json_canon_bench` (mirror of
  `css_canon_bench`, N=200, `proc_pid_rusage` PMU) exists and produced report 33. What remains for
  TW0 is the quiet leg: loadavg < 2 re-capture plus an interactive-samply flame — the prerequisite
  for *ranking* any lever (32 U1) — with P2's `assert_eq!` digest-parity gate folded into the
  harness (33's XOR-under-black_box does not prove parity) and the canada sign adjudicated on it. Per-corpus-shape reporting (numeric/string/nesting-dense) replaces aggregate
  single numbers (23 §7). CSS holds under cold discipline even at load (~2.0-3.6× lightningcss
  full-CSSOM — 21:32-48) but the eager-depth asymmetry stands until TW3 closes it (§4).

## §3 — The stack abrogation (new wave TW-S; no SK-V20 antecedent — 35 N26)

Report 19's headline, verified by 31 items 9/15: **every nightly feature the toolchain pin cites is
dormant** (`-Zthreads`/`-Zshare-generics` removed after a measured 28× warm regression; cranelift
commented out; `rust-toolchain.toml:6-8` vs `.cargo/config.toml:227-291`). The pin is ~12 weeks
stale. The abrogation:

1. **Edition 2024 unification**: skinny `edition = "2021"` → `"2024"` (one workspace line +
   FOUR `unsafe extern` wraps (P3's corrected count; sites enumerated in PROTO-P3:47-48) +
   `cargo fix --edition`; 166 files/~68.6K LOC; the `crates/` migration is the in-repo precedent —
   19 §a). MSRV 1.78 → 1.88 (simd-json 0.17/criterion floor). P3 proved this leg: skinny builds
   clean on edition 2024 + pinned stable, mechanically trivial.
2. **Pinned stable** (Path A, 19 §b): `channel = "1.96.1"` (then-current stable; 31 item 15).
   Retires the AttrId-ICE bisect treadmill; drop the cranelift component. Nightly returns only
   under a measured cold-build win. Prototype P3 executes and prices this before freeze.
3. **Lock-15 release profile** (OWNED by TW0, landed early so every later bench is honest; listed
   here for stack coherence only): root `[profile.release]` lto thin→fat, add `panic = "abort"`
   (the thin config is documented as halving twitter throughput — 19 §d; 31 item 10); add
   `panic = "abort"` to bench; **add an Apple-M `target-cpu` to the bench profile** — currently set
   nowhere, "the largest untapped zero-risk throughput lever visible in the profile surface" (19 §d).
4. **Dependency modernization** (19 §c): delete criterion from skinny (divan is the frozen harness;
   half-migration finished), thiserror 1→2, toml 0.8→1.x, cssparser 0.34→0.37, simd-json →0.17
   single-sourced, nom 7→8, tree-sitter →0.26, wasmtime →current, once_cell deleted (std
   `LazyLock`), `ron = "0.12"` pin confirmed. **Preserve the regex-syntax excision** — the bespoke
   HIR is already SOTA-clean; no action (19 §c.1).
5. `std::simd` is NOT adopted at any emitter layer; `core::arch` intrinsics remain the emission
   target (29:71-72).

**Scope decision (resolves P3's "MIXED" verdict):** the stable-build requirement covers **what
survives the cutover** — the skinny workspace and every root crate the totality plan retains. The
legacy root `crates/` tree's stable failure dies at one leaf: `parse_that` → `smallbox` with
`features = ["nightly"]` (`../parse-that/rust/parse_that/Cargo.toml:97`, E0554 genuine). That leaf
is retired-with-the-legacy-tree, not fixed: the cutover abrogates the pre-restart consumers of
`parse_that`, so its nightly surface is moot, not a blocker. NO "de-nightly parse-that first"
precondition is added — CRIT-P3 shows the coupling claim conflated two crates: §7.3 routes skinny
onto the canonical **regex** engine (`../parse-that/rust/regex`), whose dependency set carries no
smallbox and no `parse_that`; the regex unification does not re-import the nightly surface. One
residual verification rides TW-S: a `cargo tree` transitive check that egraph/csp-solver pull no
smallbox (P3 could not run cargo).

Close gate: skinny + all surviving `crates/` members build on the pinned stable, edition 2024;
bench parity re-proven cold against the TW0 baseline. TW-S is NOT yet priced — P3's bench leg was
withheld (loadavg 15-18) and the §3.4 dependency modernization is unprototyped; the parity
measurement is a TW-S close deliverable, not a discharged fact.

## §4 — The value plane and the one-tape substrate (TW1 + TW3, amended)

**The C1 reversal, resolved.** SK-V20's TW1 title "AoS→SoA" collides head-on with the dig's
hardest-won negative result: a 7-column SoA tape split was measured to LOSE to flat AoS
`Vec<TapeRec>` + parallel `sib_skip` on cache locality — "the most important architectural reversal
of Era VI"; the SoA 7-column pivot cost −60-76% (35 N19; 09 §C1; 14). This SPEC resolves the
tension in the dig's favor: **TW1 lands the ONE tape as AoS hardening** — single-writer
`frame_depth` stamped once by one owner (13 D1), atomic multi-column rollback, `sib_skip`
side-plane, the AU primitive set (§2) restored as the floor. Lock 1's exactly-one-encoding goal is
met by deleting the dual-encoding trap (`TapeStructBuilder`) and the three eager-builder families
(~1,080 hits), not by a columnar pivot. A SoA layout may only return via a fresh measurement that
names why its coupling differs from AY-I.W1's failure (35 §1 C1). Read-side column caches (the
4-lane reordered-unrolling reducer, 6.57× on canada — 09 §D6) are admitted as read-optimizations
that leave the write path AoS.

**The value plane (TW3)** stands on three externally-corroborated design facts:

- **Lazy-escape/HasEsc is confirmed SOTA shape**: JustJson independently converged on borrow-when-
  clean, decode-escapes-on-touch (23 §6). Escape density becomes a controlled bench axis.
- **Eager is a generic fold over the lazy cursor — one substrate.** sonic-rs ships lazy/DOM and
  direct-to-struct as two genuinely separate paths; bbnf's fold-over-lazy is the stronger claim and
  must be BENCHED against sonic-rs's dual-path direct-to-struct numbers specifically, where a
  fold-over-cursor could plausibly lose (23 §1, §8). One path alphabet (sonic-rs's `pointer!` proves
  one alphabet suffices — the four JSON-pointer-shaped implementations collapse); the serde
  `Deserialize` bridge rides the cursor; runtime reflection (facet) is explicitly rejected (23 §5).
- **The lightningcss comparison requires eager-depth parity, honestly scoped** (24): lightningcss
  materializes a ~100+-variant typed `Property` CSSOM eagerly while bbnf's typed catalog is a
  27-unit hand-curated TOML; AND lightningcss itself lazily defers `var()`-bearing and custom
  properties to `TokenList` — so full fidelity means (a) a comparably deep typed catalog, ideally
  derived from W3C/MDN machine-readable property data (a genuine novelty over lightningcss's
  hand-authored variants), and (b) an explicit Unparsed/Custom deferred-token escape hatch in the
  typed model. Minify/browserslist/nesting-transform parity is named OUT of the parse mandate
  (24 §Minification). The full-tree-touch bench plane (N15) and the co-located depth-asymmetry
  disclosures make the ">SOTA" sentence honest until TW3+TW4 make it true.
- The vendored lightningcss (1.0.0-alpha.71, cssparser 0.33) is several releases stale; the TW-S
  bump re-pins the comparator before any re-measurement (24 §Versions).

**TW3's headline close condition**: the fused parse+value lane (§2) becomes the DEFAULT
materialization path fleet-wide; every `->` reaches the emitter (`typed_materialization_invariant`
flips to built); JSON value_from_ref byte-equal; full-depth CSS oracle green. Two riders per
CRIT-P2-v2: (a) the fold-over-lazy-cursor construct is benched against sonic-rs's dual-path
direct-to-struct as an explicit OPEN sub-gate — the digest SINK P2 benched is not this construct;
(b) TW3's per-corpus floor binds to the TW0 cold baseline (§2), and **TW3's canada leg depends on
TW4's Eisel-Lemire decode** — the coupling is carried in §11, not hidden behind a parallel bar.

## §5 — The decision engine, right-sized (TW4, amended per 27)

The frontier and the repo's own sweep deliver the same verdict from opposite directions: Cranelift
ships production eqsat at average e-class size 1.13 with tree-cost greedy DP; the repo's 54-config
cost-weight sweep returned a documented null (singleton e-classes) (27 headline). And 13/14 e-graph
rewrite rules registered zero fires behind the converging normalizer (09 §C7). Therefore:

1. **The greedy tree-cost `Extractor` is the PERMANENT default engine**, not a placeholder
   (35 §1 C3). The one net-new artifact is the **measured `KernelCostTable`** —
   `(KernelId, Uarch) → cycles/64B`, populated by a checkasm/PMU harness, checked in as versioned
   data, never estimated (27 §e.1). P4 refuted the null (the thresholds do NOT already match
   measurement: LUT wins from N≥3, crossover N*≈2, MultipassCmpEq never cost-optimal at N>16) —
   but against a tests-only selector with proxy kernels. Binding corrections, per CRIT-P4: **the
   table's consumer is the LIVE `crates/simd-scan` `KernelShape::select` ladder**
   (`alphabet.rs:103,118-124`, byte-identical `1..=8 / 9..=16 / _`) and the live
   `scan_nibble/scan_wide/scan_multi` kernels — never the tests-only `crates/ir`
   `for_singleton_count` mirror, which is retired into the same source of truth. And because both
   demonstrated flips exercise only hypothetical (single-delimiter CSV/TOML) or dead-branch (N>16)
   decisions in today's fleet, **the table's flip-consumer is bound to TW6's third language** —
   §11 orders TW4's table landing so its first outcome-changing flip is exercised by a fleet
   grammar in the same campaign, or the table is the §1 pathology it exists to cure. The single
   production `perf_cost: 0` site in the retained engine (`skinny/crates/passes/src/lib.rs:590` —
   16 item 2; two further sites at `decision_csp.rs:242,254` are deleted WITH the CSP stage, per
   §5.2) is a one-line fix the table feeds. `cfg!(target_arch)` and every hardcoded threshold
   (NIBBLE_LUT_MAX=8, WIDE_LUT_MAX=16, alt_branch≥8, tiny_string_cap) demote to admissibility
   filters / table cells (27 §e.2). P4's FLIP 3 is a real in-tree fix folded into TW0's honesty
   sweep: two costs for one named movemask (`movemask.rs:4-5` `vshrn` vs the local `vaddv_u8` in
   `byte_class_from_eq_set_64.rs:79-84`) — unify on the measured-cheaper spelling.
2. **The tautological CSP second stage is retired, not ported** (decision_csp solves to recover its
   input — 31 item 5; no frontier design hands an extractor's answer to a downstream CSP — 27 §d).
   One engine over `(BackendShape × KernelStrategy)`; skinny's engine and core's `csp_strategy`
   unify (27 §e.5).
3. **Escalation is gated behind a measured ablation**: treewidth-FPT → warm-start ILP → CP-SAT only
   if greedy provably differs from the optimum on the real corpus, with Y.5's connected-components
   decomposition as the separability instrument (35 N23; 27 §e.4, the anti-theatre gate). The
   re-vendored csp-solver's `node_budget` freezing guard is verified present before any escalation
   is even considered (35 N22).
4. **The dav1d chain of consequence** (25): function-pointer kernel table resolved once at init
   (rav1d measured fn-pointers beating enum dispatch in Rust — 25 §1.3); checkasm co-emitted per
   kernel, iterating EVERY registered variant (the liveness property — 25 §4) so dead kernels
   (JSON `neon::scan` behind GrowOnly, `unescape_uxxxx`, the two dead `asm!` blocks, the lying
   `ScanBackend::Scalar` label) are wired-or-deleted, never carried. Intrinsics by default; hand asm
   admitted per-kernel only with a cargo-asm/checkasm proof of a mis-schedule, missing intrinsic,
   spill failure, **or an instruction-selection miss no intrinsic spelling survives** (P5's actual
   cause — LLVM De-Morgans the minimal ORR-tree into an 8-op-surplus form; the narrower gate would
   reject the one measured exception). The bound: 0-5% generally, ~10% directional on the classify
   kernel (P5). **Standing order, per CRIT-P5: the two cheaper doors are priced BEFORE any asm lane
   is committed** — door (a) the intrinsic-expressible `vshrn` movemask algorithm (already in-file
   at `json.rs:928`; likely beats all four variants, making the asm question moot — "algorithm vs
   algorithm", 25:250-252) and door (b) an opaque-barrier/reassociation coercion keeping the
   ORR-tree. Both are TW0-quiet-session bench items alongside the PMU re-capture that must confirm
   >5% with the same op-count cause on the pinned toolchain; only if both doors fail does the
   per-kernel asm gate open. checkasm is right-sized to the actual kernel census (~4-6 kernels, one
   ISA): the per-kernel differential-parity + bench harness P5 built IS the liveness instrument;
   no dav1d-scale machinery is imported beyond it.
5. **Apple-NEON invariants live in the cost model as data** (25 §2): the `shrn`-4 movemask is
   4-bits-per-byte (nibble-domain arithmetic — the #1 porting trap); one SIMD↔GPR crossing per
   64-byte block (~12-cycle bubble each); classification LUTs ≤32 bytes (`vqtbl2q` at 2c/4-per-cyc,
   never `vqtbl4q` without proof); saturate four 128-bit pipes via independent chains, not width.
   Host-tax discipline by construction: `MaybeUninit` scratch, panic-free hot paths, bounds-elision
   shapes (rav1d's largest real gains — 25 §1.3, §5).
6. **The SOTA-gap kernels**: grammar-emittable Eisel-Lemire number decode (lexical-core is already
   in the lockfile via simd-json — near-zero integration cost, 23 §3; **this kernel is TW3's canada
   dependency, §11**); NEON bulk unescape keyed on HasEsc; tokenize-once CSS declaration bodies.
   The structural pre-scan presize is DELETED per §2/P1 — no TW4 decision remains.

## §6 — The type plane (TW5, per 28)

Honesty first: the README/ARCHITECTURE "rank-1 HM / AC-3 / bidirectional" claim is struck — what
exists is a monotone lattice fold, independently confirmed (28 §1; 34 item 18). Then the build, in
28 §6's order: (1) `MapExpr::synth` — bottom-up over the closed operator set (~80 lines), killing
`unwrap_or(Span)` at `generate.rs:371/:399` so annotations CHECK, never replace; (2) `HostSigTable`
from `@host` declarations + a first-order unifier (~150 lines) for chains and host edges —
`BBNF-HOST-SIGNATURE-MISMATCH` becomes a real unification failure; (3) `TypeDesc::Record(Row)`
(closed rows — productions never extend records) and `TypeDesc::Sum` (typed tagged unions retiring
`HeterogeneousAltJoin`/`BoxedEnum` erasure), resolved during projection; (4) rank-1 HM (Algorithm J,
union-find + Rémy levels) for generic rules — `List : ∀a. Elem(a) → Vec<a>` inferred once,
instantiated per call, un-monomorphizing the closure at the type layer; (5) the structural fold
re-homed off the csp-solver crate onto a plain worklist (the "AC-3" naming is a category error —
28 §a5); (6) a Salsa-style incremental shell, later. Algebraic subtyping (MLsub/Simple-sub) is
explicitly rejected for the core; egglog is rejected as the type engine (e-class analyses propagate
bottom-up; typing needs top-down contexts — 28 §3). Steps 1-3 are load-bearing honesty+capability;
4-6 are frontier and carry an explicit disposition: DEFERRED until a fleet grammar demands a
generic rule or a row — never carried as silent committed scope. P6's actual de-risk accounting
(CRIT-P6): **step 1 is de-risked over two grammars** (33/33 descriptors, zero Span defaults, three
negative injections fire); step 2 is validated synthetically only (the corpus has no `@host fn` and
lowering drops call args — the unifier's param edge is unreachable today); step 3 is absent from
the corpus entirely. Three binding consequences:

- **Step 0, scheduled**: the upstream lowering fix P6 exposed — width is destroyed before typing
  (`value_expr/literals.rs:7,17`; `expression/wrap.rs:589-606`; dropped `FnCall` args), so synth
  recovers `I64` where the grammar wrote `U8` for 22 of 33 descriptors. This lands FIRST in TW5;
  without it the type plane types the wrong facts.
- **The TW5 close gate asserts correct WIDTH, not merely zero `unwrap_or(Span)`** — P6 proved the
  Span-free condition satisfiable while two-thirds of discriminant tags land at the wrong width;
  the gate adds a per-descriptor width assertion against the grammar's written annotation.
- **HostSigTable and its `@host` consumer land in the SAME wave** (§1's law applied to ourselves):
  the `@host` declaration surface moves INTO TW5 alongside the table and unifier, with a falsifier
  grammar exercising the signature-mismatch error; the land-or-delete decision on the wider
  closure surface stays in TW6.

The inference completes BEFORE emission so neither backend's type system does inference work — the
chumsky/LALRPOP portability trap is structurally avoided and the backend-agnostic type manifest is
the Rust/TS emission interface (28 §5).

## §7 — Coordination seams (dated, outbound, and folded)

1. **parse-that LR/packrat: NO-GO, dated 2026-07-05** (37 §6, closing critic gap G3). bbnf
   eliminates left-recursion at compile time (`crates/ir/src/passes/lr.rs`, wired unconditionally at
   `pipeline.rs:95-99`); the production fleet uses the PEG repetition idiom throughout; the sole
   left-recursive grammar is a named ambiguity fixture. parse-that is released from the bbnf keep
   and may retire the WDM tier, retaining the packrat memoization core for value.js. TW0 records
   the outbound letter; the GO-conditional packrat-design fold-in drops to a note.
2. **csp-solver**: the imported P0 does NOT transfer — `ac3.rs:128` is unreachable from every bbnf
   call path (zero `Pruning::Ac3` selections; both search sites use ForwardChecking; worst-case
   blast radius is a String telemetry field — 36). It gates nothing. The 0.2.0 API break is
   pre-staged mechanically: remove `decision_csp.rs:89` `backjumping: false`, add
   `..Default::default()`, sweep three Cargo.toml pins to `"0.2"` (36 §4). The Lock-4 fix
   (egraph→csp direct dep, `crates/egraph/Cargo.toml:11`) re-vendors against csc411 upstream, never
   forks (35 N27); a diff-against-upstream checkpoint precedes it.
3. **The two-`bbnf-regex` collision (N7)**: skinny's 322-line facade collides by name with the
   canonical parse-that engine (31 item 8). TW2 deletes the skinny crate and the ad-hoc FIRST
   scanner; ONE regex home — the canonical engine via the same path patch totality uses. The
   parse-that 1.0.0 cut is TypeScript-only; the Rust tree bbnf consumes is unaffected (35 N7).
   The canonical regex crate's dependency set carries no smallbox and no `parse_that` — this
   unification does NOT re-import the nightly surface P3 found in the `parse_that` parser (§3).

## §8 — Crate topology: decomposition-first (TW1 lead, per 20)

The restart authored ~600K lines of markdown describing a 24-crate greenfield it never executed;
the live tree is 13 crates dominated by a 247,428-LOC `core` god-crate whose `grammar/` subtree
alone is 171K LOC; the doc-to-code ratio is 3.74:1 by line on a frozen codebase (20 §0, §2). The
TW manifest buried the split as a TW1 footnote — the inverse of the correct order, because TW3/TW4
otherwise land value and kernel planes INTO the god-crate (20 §2c, §4a). Amendments:

1. **A TW0 doc commit marks ARCHITECTURE.md's 24-crate design and MIGRATION.md §3-14 SUPERSEDED**,
   naming the real target: the `core` split along the already-half-designed seams — frontend/
   grammar, codegen, runtime (tape+value), kernels (bbnf-simd), passes, host (20 §5.1). Silence is
   the worst option.
2. **The split is TW1's opening, gated deliverable** — decomposition precedes the substrate
   landing, so TW1's tape and TW3/TW4's planes land into clean crates (20 §5.2). MIGRATION.md's
   per-file disposition discipline (KEEP/ABROGATE tables) is the asset the wave manifest inherits
   for the split's file-level accounting (20 §3.3).
3. **The meta-lock**: a lock whose verification command matches zero files is a failing lock
   (20 §1e). TW0 runs the five-lock falsifier audit — Lock 14 (both artifacts: the live two-crate
   test gate AND the near-vacuous LOCKS.md:349 command with 8/13 phantom paths — kept distinct per
   32 C2/31 item 12), Lock 12 (ser/gorgeous precondition never met), Locks 7/11 (crates never
   created), Lock 13 (god-directories, flagrantly violated). Until the split lands, the widened
   Lock-14 falsifier keys on directory paths inside `core`, not on crates that do not exist
   (35 §2 W6 row). The generated-file marker is unified to one string (`css_l4.rs` uses
   `//! AUTO-GENERATED`, invisible to a `// @generated`-keyed gate — 20 §1a).
4. **Husks deleted**: `bootstrap` (re-export → core), x86/wasm bodies out of `simd-scan` (952 LOC +
   92 emitter-stamped AVX2 hits — 16 item 6), the ser/gorgeous Lock-12 archive decision executed;
   the N24 dead-substrate `rg` sweep (dta.rs residue, tape::visitor family) rides the same commits.
5. **The doc/code ratio gate**: a standing cap on markdown lines added per code line landed
   (20 §5.5); research passes are capped at three per cycle (SK-V20 process surgery, carried).

## §9 — Grammar generalization, certified empirically

Lock-14 is green-by-exclusion (16 item 1) and the constraint's current status has no affirmative
owner (32 Part I). Gate-widening (§8.3) makes the negative check honest; empirical certification
makes the positive claim: **a third serious target language is required** to break the JSON/CSS
duopoly (32 G4 — every SOTA researcher targeted only JSON/CSS; the "…" in the aim is unbacked).

**The adjudicated generalization statement (CRIT-P7-adjudication, superseding both PROTO-P7 and
CRIT-P7):** the generator at HEAD is MORE general than P7 claimed and less finished than either
document located. P7's BREAK 1 is FALSE — `->` projections ARE carried and materialized for
non-JSON grammars via `general_descent` (Sheets: 31 projections, `GoogleSheetsKind` emitted);
dispatch is un-forked and structural (`SinkOnly`/`Scan` by shape-harvest; the `RuntimeEmitterKind`
fork P7 tested is nonexistent at HEAD — P7 ran against a phantom base and is disqualified as
measurement, retained as a break-census instrument). The TRUE walls are three SHARED
generator defects in `general_descent`, not per-grammar gaps — which is the good news: **TW6's TOML
is certifiable WITHOUT per-grammar emitter files.** The three fixes: (WALL A) whitespace policy
derived from the grammar's `?w` declaration, replacing the hardcoded skip-whitespace between Seq
elements (`emit.rs:126-135,272,303`) that makes newline-significant structure unrecognizable;
(WALL B) a real runtime leaf regex matcher replacing the `match_leaf` delimiter-scan approximation
(`emit.rs:157-228`, `regex_first.rs`) — RFC3339 datetimes, escaped/multiline strings, `[\s\S]*?`
all break on the approximation; (WART C) delete the `has_rule("value")` name-gate
(`runtime_generator.rs:98`) that misroutes any grammar owning a rule named `value` onto verbatim
JSON templates — nominal residue, structurally forbidden. Lock-14 widens to a POSITIVE gate:
**the emitted TOML parser round-trips against the `toml` crate**, not directory-path greps.

TW6 lands TOML as a fleet member with that independent oracle (field-for-field), alongside the N4
oracle completion (CSV vs the `csv` crate first; five grammars today have zero correctness
oracle). The novelty seam this defends is confirmed still empty as of
July 2026: no system holds even the (declarative grammar × emitted block-SIMD classification) pair;
the Parabix lineage is alive but pointed at regex/GPU; XGrammar/llguidance are named and dispatched
as Axis-G constrained generation, not parsing (26 §0, §6). The claim stays narrow: "first GENERATOR
to emit grammar-derived block-wide SIMD structural classification with a typed value plane" —
never "first to vectorize parsing" (26 §7).

## §10 — The folded ledger and the precepts scorecard (normative companions)

Report 35 is the opening census of record: all six chronic items re-verified OPEN (C6 D-G reconcile
still gates everything; C1 revised per §4; C3 right-sized per §5), N1-N18 re-dispositioned, and
nine new rows N19-N27 folded (the AoS negative result, the pre-scan resurrection condition, the
Y.13 gate template, the node-budget verification, Y.5 decomposition, the reclaim sweep, the
idempotent-on-disk codegen rule — any generator writing `include!`-consumed source skips unchanged
writes or poisons cargo's fingerprint (13/35 N25) — the stack axis, and the csc411 coordination
checkpoint). Report 34 is the precepts scorecard of record: 20 of 23 directives unchanged, three
with corrected evidence that resizes work without flipping status — perf_cost is ONE production
site not ~8; inline tests are 7 core + 21+ skinny, worse than carried (TW0 relocation scoped to the
corrected count); the JSON hot-path decode is hand-rolled, so the 51/51 gate figure needs
re-narrating, not re-measuring. The pre-fix 1810 MB/s contamination magnitude was never
independently re-derived and anchors no regression budget (32 U3).

## §11 — The amended wave manifest

Order: **TW0 → TW-S → TW1a → TW1b → TW1c → TW2 → TW4.1 → {TW3 ∥ TW4.2} → TW5 → TW6**, two
couplings carried openly. The D11 coupling: TW2 may land structure-un-forked with hot leaves
behind static dispatch; TW4 replaces that dispatch with the measured table; the cutover-gate runs
at TW2 close and again at TW4 close. **The TW3↔TW4 number-decode coupling (CRIT-spec-architecture
B2): TW3's canada close leg requires TW4's Eisel-Lemire decode kernel — so TW4 is split: TW4.1
(Eisel-Lemire + the KernelCostTable substrate) lands BEFORE TW3 closes; TW4.2 (the remaining
kernel plane + dispatch-table replacement) may run parallel to TW3.** The parallel bar binds only
where no dependency crosses it.

- **TW0 — Gates, hygiene, honesty, truth-floor.** **First commit — the D-G reconcile (C6),
  defined so it is executable:** a doc-only commit that places the SK-V16/V17 REDRESS pre-block
  reconcile onto the committed ledger. Concretely: REDRESS 51/53/246/247 are entries in the
  SK-V16 pre-block route families (SK-V16 SYNTHESIS §0.4; SK-V19 SPEC D-G, CH3-V1-R2) — 51/53 from
  the 50-55 retained-shortcut family (53 is the canonical instance: a structural index retained as
  a parallel vector alongside the ONE tape; 51 its retained-parse-shortcut sibling) and 246/247
  from the 242-247 fixed-shape/Unicode-retry family (one-quartet Unicode/object-pair promotion and
  fixed-shape retry under old framing). The commit contains: (a) a DEFERRED-LEDGER entry naming
  the four ids AND their carried family meanings, so no substrate wave can reopen them by renaming;
  (b) the skinny freeze tag `sk-v19-frozen` — skinny becomes architecture-read-only, retained
  solely as differential oracle + checkasm/microbench harness; (c) the supersession pointer to
  this SPEC as the governing manifest. Zero behavior LOC; it unblocks every TW wave. Then: the
  24-crate supersession doc commit (§8.1). The five-lock falsifier audit + meta-lock (§8.3). The
  Y.13 consumer-invariant gate template adopted as standing wave-close law (§1). Lock-15 +
  target-cpu (§3.3; TW0 is the SOLE owner — TW-S inherits, never re-lands). Lock-4 fix with the
  csc411 re-vendor checkpoint (§7.2). W5d x86/wasm deletion. Inline-test relocation (7+21
  corrected, including the four `#[cfg(test)]` perf_cost sites CRIT-spec-architecture R7 flags).
  MASTER-PLAN §13.6 reconcile (16 item 9). Oracle hygiene (N5, W3C rekey). The P4 FLIP-3 movemask
  unification (§5.1). **The truth-floor bench session**: `json_canon_bench` (exists per report 33)
  gains the P2 digest-parity assert; quiet (loadavg < 2) cold re-capture of CSS + JSON both sides,
  canada sign adjudicated; the P1 four-plan interleaved re-run with stddev/CI (magnitude
  certification for the presize DELETE); the P5 PMU re-capture + doors (a)/(b) bench (§5.4);
  interactive-samply flame; per-corpus-shape reporting; the full-tree-touch plane (N15). The
  parse-that NO-GO letter recorded (§7.1). Playground junk ignored (16 tail). *Close gate:*
  locks/tests green; the quiet baseline ledgered — no later wave's no-regression clause is
  meaningful without it (20 §4c).
- **TW-S — Stack abrogation** (§3). *Close gate:* skinny + surviving crates build on pinned
  stable, edition 2024 everywhere (scope per §3); cold bench parity vs the TW0 baseline; the
  smallbox transitive check clean.
- **TW1a — The topology split** (§8.2). Core decomposition along the six named seams as its own
  gated wave — nothing else rides it. *Close gate:* the split's crate boundaries enforced by the
  widened Lock-14; 9-grammar regen byte-clean; build green both workspaces; zero behavior change
  (differential oracle green).
- **TW1b — The one-tape AoS substrate** (§4). The AoS-hardened ONE tape + the eager-builder
  retirement (~1,080 hits) in one fenced parity-gated commit — the substrate and its consumer
  together, per §1. *Close gate:* JSON+Sheets oracles field-for-field; Lock 1 green; no cold
  regression vs the TW0 baseline.
- **TW1c — Mechanical tail.** StructLayout→Layout rename (960 hits); the N24 reclaim sweep
  (including the §2 presize deletion: `Sampled`/`Exact`/`OneShotSimd` and the dead
  `attach_structural_index` no-op). *Close gate:* regen byte-clean; grep-zero on the retired
  names; no cold regression.
- **TW2 — The one generator.** As SK-V20 wrote it (BackendExpr → THE IR; real Emitter trait; xtask
  un-fork; couriers derived-or-owned; structural-predicate dispatch; grammar-derived alphabets),
  plus: ONE regex home (§7.3); the css_l4 108K-line regression trace attacking line count itself —
  the B2 lesson: relocation is not reduction (35 N6); `generated_loc_budget` enforced in
  `regen --check`; the idempotent-on-disk rule asserted (N25). *Close gate:* ONE emit path serves
  all 9 grammars; cutover-gate green for the slice; budgets met; widened Lock-14 green over codegen.
- **TW4.1 — Eisel-Lemire + the cost-table substrate** (§5). The Eisel-Lemire/SWAR number-decode
  kernel (TW3's canada dependency — lands FIRST); the `KernelCostTable` bound to the LIVE
  `crates/simd-scan::KernelShape::select` ladder and kernels (§5.1); the tests-only `crates/ir`
  selector retired into it. *Close gate:* canada eager cold beats sonic on `json_canon_bench`
  (the float-tax signal closed); the table feeds the live selector with a `cargo expand` witness.
- **TW3 — The value plane** (§4; opens after TW4.1). Fused parse+value default lane;
  ValueRef/cursor fleet-wide; eager-as-fold; one path alphabet; serde bridge; CSS depth closure
  with the derived-catalog + Unparsed escape hatch; StructRegistry projection fence;
  projection-totality invariant (`PROJECTION_DIRECT_TO_STRUCT.len() == materializers == consumers`
  — 09 §D4). *Close gate:* every `->` reaches the emitter; the fused lane IS the default with no
  cold regression vs the TW0 baseline on any corpus, per-corpus reported (twitter/citm/canada
  each named — no geomean); the fold-over-lazy-cursor benched vs sonic-rs dual-path
  direct-to-struct (the §4 OPEN sub-gate discharged); full-depth CSS oracle green; the AU-relative
  cold capture ledgered as context; asymmetries co-located.
- **TW4.2 — The decision engine + kernel plane** (§5; parallel to TW3). One engine; ablation gate
  before any escalation; per-kernel differential parity + bench (checkasm right-sized, §5.4) over
  the whole dispatch table; NEON bulk unescape + CSS tokenize-once; scanner unification (C4/C5:
  lo/hi nibble two-LUT, `vshrn` single-pack, Lock-16 co-gate); the asm door only per §5.4's
  doors-first order. *Close gate:* cutover-gate green (second leg); Lock-14 re-proven full-system;
  the compound >SOTA sentence true unqualified on the gated planes — cold, quiet, live SIMD on
  the JSON path. The cost table's fleet-exercising flip is certified at TW6 when the third
  language lands (§5.1's consumer binding).
- **TW5 — Type resolution** (§6). Step 0 lowering fix first (width + FnCall args preserved);
  then steps 1-3 with the `@host` declaration surface in-wave (§6). *Close gate:* 9 grammars type
  end-to-end with zero `unwrap_or(Span)` AND correct width per the grammar's annotation (the P6
  falsifier); the host-signature gate fires on a falsifier grammar; annotations check.
- **TW6 — Language + fleet completeness + the third language.** MapExpr byte-fold primitives;
  `type_annotation` extension; `@assoc`; land-or-delete the remaining closure surface; oracle
  completion (N4/N13); **TOML landed as fleet member ten via the three SHARED `general_descent`
  fixes (§9: ?w-derived whitespace policy, real leaf regex matcher, name-gate deletion) — no
  per-grammar emitter files**; the KernelCostTable's single-delimiter flip exercised live.
  *Close gate:* the emitted TOML parser round-trips the `toml` crate field-for-field (the widened
  positive Lock-14); zero grammars without an oracle; zero live host hatches not expressible as
  grammar; the generalization census names ten grammars, three of them serious targets.

## §12 — Prototype dispositions (pass-1 closed; instruments retained)

All seven ran; their verdicts are folded above. P1 (presize): DELETE decided, §2 — instrument
retired, magnitude certification rides TW0. P2 (fused lane): core thesis cold-corroborated, canada
refuted, fold-over-cursor still open — §2/§4/TW3. P3 (stack): skinny-on-stable settled; scope
decision taken in §3; bench leg deferred to TW-S close. P4 (cost table): null refuted; consumer
re-bound to the live selector and TW6's third language — §5.1. P5 (asm): instrument sound;
decision deferred behind the doors-first order — §5.4; the checkasm-shaped parity harness is a
TW4 asset. P6 (HM spike): step 1 de-risked; step 0 lowering fix and the width gate scheduled —
§6. P7 (TOML): disqualified as measurement (phantom fork), superseded by the adjudication — §9;
the break census (walls A/B, wart C) is the TW6 work-list. The `json_canon_bench` harness and the
P5 parity harness are the two instruments that graduate to `crates/`.

## §13 — Open questions carried to pass 2

1. The asm-emission-path question is NARROWED, not closed: doors (a)/(b) bench at TW0 quiet; only
   a >5% confirmed delta with both doors failed opens the per-kernel asm gate (§5.4).
2. The fused lane's cold survival is ANSWERED per-corpus (§2): wins twitter/citm, loses canada
   pending TW4.1. What remains open is the fold-over-lazy-cursor construct vs sonic dual-path —
   the TW3 sub-gate.
3. Whether the TW1a split-first ordering survives contact with the substrate landing's actual
   seams — the split may reveal that tape must move first; pass-2 critique adjudicates the
   TW1a/TW1b boundary (the unbundling itself is settled).
4. The severity-weight of CSS SIMD tokenization (24 §SIMD: a confirmed-by-absence open frontier
   with real comment/url() correctness risk) — deliberately NOT scheduled in TW4; it enters only
   via a costed per-grammar decision once the TW0 measurement loop is live.
5. NEW (P4): whether the live-selector cost-table rebind changes any current-fleet kernel choice
   before TW6 — if not, the table lands with its TW6 consumer named in the same commit message.
6. NEW (P3): the `cargo tree` smallbox transitive check over egraph/csp-solver (NO-cargo bound
   pass 1; discharged at TW-S).
