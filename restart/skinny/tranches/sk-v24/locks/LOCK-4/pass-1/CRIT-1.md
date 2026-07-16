# LOCK-4 — THE COMBINATOR CORE · pass-1 · CRIT-1 (Adversarial Critic 1 of 2 — within-lock view)

*Weighting: internal soundness of each family spec + whether the two prototypes actually tested the
sharpest uncertainty they are cited for. Every path:line anchor in the specs was spot-checked against
the read-only `parse-that` tree and the `bbnf-lang` IR crate; the PROTO-1 scratch copy
(`…/scratchpad/pt-scratch`) was opened and read. Verdict up front: the substrate claims are
**accurate** (no fabrication), but the keystone prototype is **misattributed across three families**,
and no family's make-or-break number for its own distinct mechanism was taken this pass.*

---

## 0. The two cross-family findings that reprice every family

### F-CRUX-1 — PROTO-1 tested a HAND-FUSED SPECIAL CASE, not the general analyzable interpreter, and is over-leveraged across 4A/4C/4D.

PROTO-1 is cited by FAMILY-SPECS §6 as *"Decides 4A/4C/4D single-tier viability at once."* It does not.
I read the scratch `json_graph.rs`. What was actually built:

- `JsonNode` is a **bespoke 5-variant enum** — `Map · Number · Array · Object · Dispatch` — **not** the
  ~25-node `Node` union 4A §1.1 specifies (`Seq(Vec)/Alt(Vec)/Many/SepBy/Opt/Wrap/Skip/Next/Not/
  Minus/LookAhead/Negate/Peek/NonTerminal/Boxed/…`).
- The `Array`/`Object` arms are **hand-written straight-line loops** (`loop { items.push(self.call(*value,
  state)?); trim; if comma.is_none() break }`), recursing by index over the **native call stack**. The
  first-byte `Dispatch` table is **hand-built** in `json_graph()`, not synthesized by a FIRST-set pass.

Consequences, family by family:

- **For 4A:** the primary open measurement 4A itself names — *"does a widened `Map`-carrying node payload
  push the `match` below the monomorphized closure chain?"* (4A open-Q (a); R2; §3.2) — is **untouched**,
  because the structural spine here is *not* a general per-node `match` over a wide union; it is bespoke
  fused arms. The proto satisfies the **letter** of P2 ("a graph producing `JsonValue` at ≥0.98×") while
  evading its **spirit** (the general interpreter's dispatch cost). P2's own words — *"≥0.98× the closure
  core head-to-head **on the SAME grammar**"* — are violated: the graph arm is hand-fused, the closure arm
  is the fully-general `Parser<JsonValue>`. Unequal fusion levels, not the same grammar two ways.
- **For 4C:** the proto is a **tree-walker** (native-stack recursion by index). 4C's entire mechanism is
  the **opposite** — a flat bytecode program with an explicit choice-point/value stack and `PartialCommit`
  back-edges, whose central risk (research-C §5.1) is *"a bytecode loop reintroduces one dispatch branch
  per instruction + explicit-stack traffic the native stack did for free; honest prior 0.7–0.9×."* A
  tree-walk arena's throughput says **nothing** about a choice-point-stack VM's throughput; they are
  different overhead regimes. Citing PROTO-1 as evidence for 4C single-tier viability is **prototype-
  evidence misuse** (constitution vii / SPEC-ONLY-style misanchoring, here a wrong-mechanism anchor).
- **For 4D:** the proto builds a **parse-that-private** `JsonNode`, and never touches `bbnf_ir`,
  `parse_with_ir`, or a single inherited optimizer pass. 4D's *decisive number* (research-D gate 2:
  interpret-mode `parse_with_ir` vs the closure core) is therefore **unmeasured**. PROTO-1 shares **none**
  of 4D's mechanism; it cannot reduce 4D's uncertainty at all.

### F-CRUX-2 — the 6% "win" is a FUSION artifact, not an analyzability result, and the fair floor is a loss.

PROTO-1's narrative ("Why the graph wins rather than merely ties") attributes the win to the graph
retiring `lazy` (`UnsafeCell` + `Box<dyn LazyParserFn>`), `SmallBox` per combinator, and the
`dispatch_byte_multi` `Vec` indirection. Every one of those is a cost of the **general, unfused** closure
core — not of "keeping the representation analyzable." The graph arm won because it was **hand-
specialized**; the closure arm was **general**. The tell is in the table: **canada** — coordinate-dense,
runtime dominated by the byte-identical `scan_number_strict_f64` where fusion of the structural glue buys
least — is the **only** corpus where the graph is *slower* (0.979–0.986×). Where structure matters (and
hand-fusion pays), graph wins 6–9%; where it does not, graph loses. That is the exact signature of *the
win is fusion, and analyzability is free-to-slightly-costly.* The honest verdict PROTO-1 supports is the
**narrow** one — *the value-plane `Map` box confines to scalar leaves and does not blow up the spine*
(and the objdump 45 `blr`-on-leaves does support that). The **broad** verdict it is used for — *"4A/4C/4D
single-tier VIABLE, two-tier not forced"* — is not earned.

### F-CRUX-3 — PROTO-1's pairing is not the interleaving the PROVE-09 law demands, at an effect size the law says to distrust.

Divan runs each `#[divan::bench]` to completion in sequence: **all** samples of `_closure`, then **all**
samples of `_graph` — adjacent, but not sample-level `A,B,A,B` interleaved (which research-B §8.6 and the
TS PROVE-11 protocol require, and which the PROVE-09 refutation was *specifically* about). "Two windows,
opposite temporal position" cancels *inter-window* drift but, with a **fixed intra-pair arm order** in both
windows, does not cancel a systematic "second-arm-runs-warmer/cooler" bias — the precise window asymmetry
that turned PROVE-09's 1.34–1.57× into parity-or-slower. The effect being claimed (~6%) is *smaller* than
the shim's refuted magnitude. Consistent W1/W2 agreement is reassuring but does not rule this out, because
both windows share the order. No `sample_count`/`sample_size` override is present in the bench; the "N≥200"
of the briefs is not configured — divan defaults govern. **This does not overturn the narrow finding, but
it forbids the 6% "graph is faster" headline from carrying any weight.**

*(All three cruxes are within-lock: they concern whether the prototypes tested the sharpest uncertainty of
the families they are cited for. They do not depend on any other lock.)*

---

## 1. FAMILY 4A — CLOSURE-TO-GRAPH — **convergence 50%**

**Sound and verified.** The substrate is real: `SpanKind` carries **29 variants** (spec said "~30") — every
node 4A needs including the `TakeUntilAny1/2/3/Lut/SIMD` ladder, flat `Seq/OneOf(Vec)`, `SepByWs`, and the
`Boxed(dyn ParserFn)` hatch — and it has live consumers (`parsers/json.rs`, `parsers/css/*`), so Y.13 is
satisfied by construction. The passes-as-rewrites framing is coherent and has an in-tree precedent
(`rust/regex/src/egraph/`). Disposition ADVANCE-candidate is defensible; 4A is the leader on cheapest-path-
to-proof. But the keystone prototype it leans on does not prove what it is cited to prove.

**Enumerated open gaps:**

1. **[keystone, from F-CRUX-1] The general-interpreter value-plane number is untaken.** P2's real question —
   does a *general* `match` over the wide union (widened by the `Map` payload) stay ≥0.98× the monomorphized
   closure chain — was reduced to a hand-fused 5-arm arena. Rerun P2 with the **actual** `Node` union +
   generic `Seq/Alt/Many/SepBy` dispatch arms, or state honestly that the value-plane result is limited to
   hand-fused grammars.
2. **P1 (fusion-as-a-separable-pass) was never built.** The proto hand-wrote the fused arena; it did not
   extract `fuse(Graph)→Graph` and show byte-identical output to the hand-fused constructors + ≥0.98×.
   The "cleverness becomes passes" thesis is still asserted, not demonstrated.
3. **The one-path collapse is unbuilt and unproven.** `json.rs` (closure) still ships **alongside**
   `json_graph.rs` in the scratch; the parity test *requires* both. §2.4.2's "parallel second codepath"
   violation is therefore *still open* — the proto added an arm, it did not fold the closure `Parser` into
   the graph. Whether the whole combinator library (not one grammar) collapses to a single tier is untested.
4. **Recursion-seam guard (R3) untested.** The proto sidesteps `lazy()` entirely (index recursion `value:0`).
   The proposed guard — keep `lazy()` as sugar auto-minting a `RuleId` via cache identity — is unbuilt, so
   the "no API break for value.js/latex-paper inline recursion" claim is unverified.
5. **P3 packrat-placement (SCC over `NonTerminal`) unbuilt.** No rule-map, no SCC analysis, no "placement ≥
   armed-everywhere on recursive / strictly-faster on non-recursive" measurement.
6. **P4 rust/ts isomorphism unbuilt, and prior signal is negative.** No `ParserNode` union or passes in TS.
   R4's warning stands unaddressed: the dispatch-LUT lever already under-delivered (~+5% vs +10% claimed);
   the Rust win is assumed to transfer, not shown.
7. **Diagnostics-annotation pass unbuilt.** Labels-as-derived-graph-property is asserted; no pass exists.

---

## 2. FAMILY 4B — COMPTIME-FUSION — **convergence 32%**

**The honest state is: zero empirical evidence this pass, on the one axis that is its entire reason to
exist.** The spec is candid — open-Q (a): *"the win is unproven and may be null"*; the §6 deferral removes
4B's inlining-reality test from the pass-1 briefs outright. So the make-or-break (`cargo asm` shows the
`blr` folded AND a paired, inlining-*attributable* win) was **not run**. Disposition ADVANCE-candidate/
REWARD-AT-RISK is accurate as a *label*, but a family whose central lever is unmeasured and whose honest
prior is NEUTRAL cannot sit above the low-30s at pass 1.

**Enumerated open gaps:**

1. **The inlining win is unmeasured and PROTO-1 cuts against it.** F-CRUX-2 showed a *runtime* analyzable
   graph already ties/beats the closure core. If closures are the thing 4B beats, and a graph that *keeps*
   analyzability already matches them, 4B's headroom to win by *erasing* the runtime representation is
   narrower than the spec implies. The spec does not surface this within-lock tension; pass 2 must.
2. **`cargo asm`/`otool` mechanism proof not produced.** No demonstration that the `blr` vtable calls fold
   to direct/inlined in a generic-struct build — the timing-independent proof the family rests on.
3. **The predicted-branch trap (R1) unrebutted.** The SmallBox indirect call is a single fixed-target branch
   the Apple-M BTB predicts perfectly on the steady loop; that inlining across it buys anything is exactly
   the PROVE-09-shaped claim, and it is untested.
4. **Weakest isomorphism of the four, and it is not flagged as a gap.** research-priorart §9 concedes 4B's
   Rust and TS fusers are *different codegen over a shared plan*. Against constitution (iii) — "isomorphism
   in layout **and in code**" — this is the softest position in the lock, yet FAMILY-SPECS lists no iso gap
   for 4B. It must be enumerated.
5. **TS build-time fuser unbuilt; PROVE-11 says the win is allocation-shaped (~+5% dispatch).** Generalizing
   `fuseAll` past arity-3 to a vite/ts-transform plugin, isolating dispatch- from allocation-elimination,
   and the bundle-size/`tsc`-clean/three-consumer-parity gates are all untouched.
6. **Comptime/bundle blowup (R3) unmeasured.** The generated-size budget binds; FastParse is the cautionary
   case; no compile-time or binary-size delta was taken.
7. **"No runtime graph at all" is admitted false** (R4: TS static recognition is undecidable). Scoped to
   fused sites, 4B-TS is *fuser + runtime-graph fallback* — which erodes its distinction from 4A to "only the
   statically-fused sites." That narrowing is not carried into the disposition.

---

## 3. FAMILY 4C — VM-CORE — **convergence 35%**

**Substrate real, headline claim untested, throughput unmeasured.** `SpanKind` is a genuine proto-VM one
step short (tree-walk, native stack); `CompiledDfa(Arc<Dfa>)` proves half the leaf vocabulary is already a
compiled machine. The disposition THROUGHPUT-OPEN is intellectually honest. But **not one line of a bytecode
VM was written this pass**, and the family's *strongest and most distinctive* claim went untested.

**Enumerated open gaps:**

1. **[from F-CRUX-1] PROTO-1 does not exercise 4C's mechanism.** A tree-walk arena is not a choice-point-
   stack VM. 4C's own central risk (§5.1, prior 0.7–0.9×, recover via threaded code) is untouched. 4C
   carries *no* throughput evidence; it must not inherit PROTO-1's.
2. **The byte-identical-program oracle (gate 3) — the headline — is unbuilt.** "Isomorphism is a property of
   the representation, strictly stronger than any LOCK-6 mirror-linter" is the family's differentiator, and
   *nothing* serializes a program or decodes it in TS. This is the single cheapest, highest-signal thing 4C
   could have proven this pass (it needs no throughput win), and it was skipped.
3. **Span-tier-VM-vs-tree-walker paired bench (gate 1) unbuilt.** The falsifiable ≥0.98× / <0.9× gate that
   would site the VM against the live `SpanParser` was not run.
4. **Threaded-code recovery (gate 2) unbuilt.** The stated fallback for a naïve-loop loss is untested, so
   "loss is recoverable" is an assumption.
5. **Value-tier honesty (gate 5) unbuilt.** research-C §5.2's own warning — Span-tier 0.98× but value-tier
   0.6× because every `Action` is an un-inlinable call — is exactly where 4C could collapse into 4B, and it
   is unmeasured.
6. **Packrat `MemoOpen/MemoCommit` + left-recursion seed-growing parity (gate 4) unbuilt.** research-C §5.4
   flags LR parity as "the one genuinely subtle facility"; no oracle exists.
7. **`proof:no-span-surface` non-reopening is asserted, not verified.** The argument "internal lowering
   target ≠ public surface" is plausible but the TS gate was not exercised against a VM introduction.

---

## 4. FAMILY 4D — GRAMMAR-SUGAR — **convergence 30% (BLOCKED-candidate; the blocking analysis itself is high-confidence)**

**The BLOCKED disposition is correct and, unusually, well-converged — I verified its load-bearing facts.**
`IrNode` carries **14 variants** matching the near-bijection table (`Literal/Regex/Epsilon/Seq/Alt/Repeat/
Ref/Skip/Next/Minus/Negate/Map/OptionalWhitespace/TokenDispatch`); `FnDescriptor` has **`EnumWrap/BoxWrap/
NumberConvert/HexConvert/SpanCapture/Expr{MapExpr}`** and **no opaque host-closure variant** — so 4D's
central wall (must add `HostClosure`) is a **real, code-verified gap**, not a rhetorical one; `parse_with_ir`
exists at `interpreter/mod.rs:373`. The Rust-side elegance ("re-homing, not rewrite") is genuine. The block
is equally genuine: the constitutional rust/ts isomorphism (clause iii, non-negotiable) rests on a TS-
execution primitive whose two horns are (i) a re-port of the grammar plane's execution engine — *a
primitive of equal-or-greater difficulty than the original LOCK-4 problem* — and (ii) a WASM crossing
PROVE-14 measured at **~0.91× on full-walk**, exactly the value-producing workload a combinator parser is.
That is the constitution's "missing primitive equal in difficulty" trigger, correctly fired.

**Enumerated open gaps (what would have to resolve to unblock, plus the intra-family unbuilt work):**

1. **The TS-execution primitive is unresolved (the block).** Neither horn is retired: horn (ii)'s gate —
   does the WASM-core crossing beat the PROVE-14 full-walk non-goal for a *value-producing* parse (not just
   stop-early)? — is unmeasured; horn (i) is conceded equal-difficulty.
2. **Cross-lock entanglement means 4D cannot converge inside LOCK-4.** It requires LOCK-5's ruling that
   `bbnf_ir` is *genuinely* the shared substrate (else the "for free" optimizer/iso claim evaporates) and
   LOCK-7's host-fn lattice for `HostClosure`. This is a hard dependency, not a soft one; 4D's ceiling this
   pass is bounded by two other locks' verdicts.
3. **4D's decisive number is unmeasured** (F-CRUX-1). No `parse_with_ir`-vs-closure paired bench; the
   interpret-mode regression risk (research-D R1, the shim lesson) and the compile-mode win are both untested.
4. **`FnDescriptor::HostClosure` + scoped-opacity guard unbuilt.** PROTO-2 argues the *current* sites don't
   poison siblings, but the variant, the fn-table, and the applicative/selective/monadic tiering that keeps
   `map`-on-result from defeating a branch's FIRST are all unbuilt.
5. **State-effecting combinators (`peek/lookAhead/recover/mapState`) are outside the IR vocabulary.** The
   "minimal lawful set of new variants vs `HostClosure`-with-state" is explicitly "must be measured, not
   guessed" — and was not.

---

## 5. PROTO-2 (CHAIN-MONADIC-CENSUS) — the substantive result holds; two soft edges

The load-bearing result is **sound and I credit it**: every `chain`/`mapState`/`recover` site across the
consumer suites fires *after a fixed-FIRST prefix parser* (the chain is on a **result**), so no site poisons
an enclosing alternation's FIRST/dispatch — the poison-scope argument passes independent of the S/M tally.
`mapState`=0, `recover`=0 call sites, and `map` is applicative-transparent by signature (verified: TS
`map<T2>(f:(v:T)=>T2)`, Rust `fn map<O2>(self, f: fn(Output)->O2)` cannot return a `Parser`). Two edges to
carry, neither fatal:

1. **The gate's |M|≤3 pass depends on optimistic S-tagging of currently-monadic sites.** Sites 1–4 are
   written as `chain` **today**; parse-that ships no `branch`/`select`. Tagging them **S** ("reformulable")
   rather than **M** ("opaque as-is") is a claim about *potential*, not *current* analyzability — and it is
   the difference between |M|=1 (pass) and |M|=5 (fail). The verdict should rest on the poison-scope
   argument (which holds for all 5 regardless), not on the S/M arithmetic (which is soft). The elegant-
   reduction risk: "reformulate the 4 S-sites as selective" reads as free but entails a **public-API
   surface change** (add `branch`/`select`) *and* migrating external, read-only consumer repos — unowned work.
2. **The census is a snapshot of today's grammars, not a property of the algebra.** "The analyzable subset
   is effectively the WHOLE grammar … and any future parameterized-terminator rule" over-generalizes from 5
   enumerated sites. Correct as measurement; the forward-looking clause is unearned.

---

## 6. Constitution sweep (within-lock)

- **(i) Grammar totality** — respected; combinators are exempt; 4D's re-subordination risk is correctly
  gated on "HIR *is* the shared substrate, not combinators grammar-generated." OK.
- **(iii) Isomorphism** — the sharpest axis here. 4C claims the strongest form (byte-identical program) and
  left it **untested** (§3 gap 2); 4D claims the strongest form (MessagePack data property) but **only Rust-
  side**, TS blocked; 4A's is mirror-tree+linter, **TS unbuilt**; **4B's is the weakest (different codegen)
  and is not enumerated as a gap** (§2 gap 4). No family has *demonstrated* iso this pass.
- **(iv) Refutation laws** — PROTO-2 is correctly measurement-free. PROTO-1 respects cold/paired/mimalloc but
  **not** the sample-level interleaving the PROVE-09 law motivates, at a distrust-band effect size
  (F-CRUX-3). The specs correctly forbid stable-port/arena perf claims; no violation there.
- **(v) Zero-runtime-cost types** — not materially exercised by LOCK-4 pass 1 (LOCK-1/8 territory); 4D's
  `HostClosure` is a runtime slot, not a type-plane claim. N/A.
- **(vi) Parsimony / one-path** — the incompatibility map correctly enforces a single terminal runtime. But
  **PROTO-1 as-built violates one-path** (parallel `json_graph` + `json`), so the proto cannot be cited as
  evidence of the single-tier collapse it is used to justify (F-CRUX-1, §1 gap 3).
- **(vii) Concrete-or-SPEC-ONLY** — the substrate anchors are concrete and verified. The misuse is the
  inverse of SPEC-ONLY-inflation: a **real** prototype cited for **three mechanisms it does not implement**
  (tree-walk arena → 4C bytecode VM; private enum → 4D `bbnf_ir`; hand-fused → 4A general interpreter).
- **(viii) Consumer-less substrate** — 4A/4C satisfied via `span_parser`'s live consumers; 4D via public-API
  preservation; PROTO-1's `json_graph` is scratch-only (acceptable for a probe).
- **(ix) Extant-tranche consciousness** — research-B §7 names the PT-2 W4 / PT-3 W4 supersessions explicitly.
  OK.

---

## 7. Disposition for pass 2 (within-lock)

- The lock's real decision (single terminal runtime × ownership) **cannot be made on pass-1 evidence**: the
  two numbers the incompatibility map says pass 2 needs (4A/4C general-interpreter value-plane, 4B inlining-
  attributable win) were **both** untaken; PROTO-1 does not supply either.
- **Cheapest high-signal pass-2 moves, in order:** (a) 4C's byte-identical-program oracle — no throughput
  needed, proves/kills the headline; (b) re-run PROTO-1's value plane with the **general** `Node` union +
  generic dispatch arms, sample-interleaved, so 4A's keystone is earned not assumed; (c) 4B's `cargo asm` +
  paired generic-struct JSON, so its axis stops being a rumor.
- **Do not** let the gestalt or a later pass inherit "4A/4C/4D single-tier VIABLE" as settled. What is
  settled: *the value-plane `Map` indirection confines to scalar leaves* (narrow, real). What is open:
  *whether the general analyzable interpreter — graph or VM — ties the closure core.*
