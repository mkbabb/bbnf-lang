# SK-V20 — Gestalt Synthesis (the campaign in totality, audited)

Date: 2026-06-10. HEAD: `36ca10646`. Inputs: 28 parallel audits + 3 adversarial verifiers (18 claims
adjudicated) + 1 completeness critic + 2 gap-fill deep-dives (`research/01..32`). This synthesis applies
the verifier corrections; where a sibling report and a verifier disagree, the verifier's re-derived
evidence governs.

## The verdict, in one paragraph

The campaign's *architecture-in-prototype* is real and the generalization thesis survived its first
genuine test (a third grammar emitted by the general generator, SK-V18). But the campaign's three
headline systems — full type resolution, the unified Value API, and the cost-model-driven decision
engine — **do not exist at HEAD in any tree**: the documented Hindley-Milner core is unimplemented
prose, the value plane is three divergent substrates over two incompatible tapes with four path APIs
and a dead direct-to-struct, and the decision engine's sole cost axis is the literal integer `0` at all
eight assignment sites. The >SOTA claims are real *as qualified* but the qualifiers are not co-located
and two planes (thermal, depth) are mixed at every compound claim site. The meta-process is producing
documentation at 3.4–7.3× the rate of code and has not shipped a fleet-scale capability to `crates/`
in five cycles. The correct move is the one this tranche takes: stop, re-found the totality spec on
audited ground, and cut over with hard runtime gates instead of documentation milestones.

## 1 · The corrected campaign picture (verifier-adjudicated)

Four citation errors propagated through sibling reports and are corrected here permanently:

1. The pre-restart AW-III bench gate result was **0 of 17** parse entries (gate was "≥15/19 strict-better"),
   not "0/19" (verify-0).
2. The fabricated CSS admissions (hand-written templates under fabricated `@generated` headers, 85–357-byte
   fixtures) originate at **SK-V12**; SK-V13 opened at AUDIT-ZERO as the *consequence* (verify-0).
3. The AX.W0b reclaim of ~78,500 LOC was a **bundled** deletion (DTA+PSI+simd-scan::emit+generated walker
   output+tests); the interpreter source proper was ~12,000 LOC (verify-2).
4. The §13.1-vs-LOCKS:635 x86 "direct contradiction" is overstated — LOCKS:635 marks the deletion target;
   MASTER-PLAN §13.1 is stale, not contradictory (verify-2).

And one refutation that *narrows* a finding: `grammar_facts.rs` and `shape_dispatch/scalar.rs` carry
grammar names only in doc comments at HEAD (code-line count: zero; the HEAD commit itself removed the
last live reference). The residual-leak problem in `crates/ir` is real but lives elsewhere
(`GrammarAuditTag` bare variants; the `shape_dict_bbnf` module's existence as a BBNF-shaped miner run
unconditionally for every grammar) — not in those two files (verify-1).

## 2 · History: the five divergence mechanisms (the negative template)

The pre-restart tree failed by five repeatable mechanisms (research/01), each with a named countermeasure
the re-founded process adopts as a **gate, not a policy**:

| Mechanism | Worst exemplar | Countermeasure (now a gate) |
|---|---|---|
| Grammar names leak into generic crates "for speed" | 9-arm grammar match in strategy.rs; 7 byte-identical generated files | Lock-14 structural scan + re-inject falsifier per generic surface (TW0 widens) |
| Parallel code paths kept as "safety hedges" | EmissionTier/PayloadKind dual axes (~32 commits to unwind) | New path ⇒ old path deleted at the same commit boundary |
| Substrate-first, no consumer at wave close | DTA/PSI interpreter: ~400 commits, 0/17 bench, ~12k LOC source deleted | samply/nm wire-contract evidence required at wave close; emit-side-only waves are not closeable |
| Hand-patched generated files | seven consecutive tranches | regen --check byte-equality is a commit hook (already live) + enforced loc budgets (TW0) |
| Deferral chains without hard gates | one item deferred eleven tranches | chronic ledger must be EMPTY before a new planning cycle opens |

The skinny arc (research/03) replayed mechanism 1 and 4 in miniature — SK-V12's CSS admission was the
campaign's first non-JSON row *and* its first fraudulent one (hand-written templates under fabricated
headers) — and the repair (SK-V14's comparator rebind + AUDIT-ZERO reset) produced the 51/51 JSON guard
that has held since. The arc's honest shape: a JSON-only optimization era (sk-v5..v11, ending in a
documented fixpoint), a fraud-and-repair era (sk-v12..v14), and a generalization era (sk-v15..v18) that
finally exercised the actual thesis — and proved it (PROVE outcome A).

## 3 · Process: the local-maxima diagnosis

The SK loop **is** at a local maximum (research/04, confirmed adversarially):

- Doc/code ratio 3.4× (SK-V17), 7.3× (SK-V18); one SK-V18 hardening directory alone is 14,581 lines
  across 46 files certifying a single goalset.
- The astral G-Omega layer (10 versions, 24,248 lines) is a fourth-order audit recursion with **zero**
  recorded instances of invalidating an Omega conclusion or triggering a code correction (verified:
  every "re-open" token in the CRUD logs is a negative assertion).
- The §3Z convergence protocol implies up to 270 hardening documents per cycle before code.
- Three of the four largest architectural items (SoA tape, C fold, decision engine) have never shipped
  to `crates/` in five cycles; SK-V19 — the designated fold cycle — shipped three W6 patches and then
  opened a research pass.

**Process surgery adopted by the re-founded SPEC:** (a) §3Z collapses to one challenge wave,
advance-on-zero-REJECT; (b) the astral ΩA–ΩF documents are abolished (SIGNOFF + CRUD-LOG remain; the
locks and master-plan are the living records); (c) a planning cycle may not open while the chronic
deferred ledger is non-empty; (d) every wave closes on a hard runtime gate (bench row or oracle in
`crates/`), never on a document; (e) the pass loop shrinks to: PLAN (one doc) → IMPLEMENT → ADVERSARIAL
AUDIT (one wave) → CLOSE-ON-GATE.

## 4 · The overfit ledger (compressed from research/07–13)

The honest compression of six audits: overfitting is **reduced but structural** in skinny and
**relocated, not removed** in totality.

- **Skinny emitter:** the JSON path is gated on the rule NAME "value", maps only `true/false/null`
  literals, and panics on containers not named "object"/"array"; the CSS path string-compares five rule
  names and hardcodes `@`/legacy-marker bytes never derived from any grammar. `general_descent` (the
  proven third path) silently no-ops TapeEmit/SpanMark, ignores DirectBuild field vectors and
  RegexProgram kinds, and probes Alts O(N) — general in shape, shallow in coverage.
- **Skinny IR:** recognizer derivation filters literals against a JSON alphabet; materialization roles
  bake in JSON keywords and bracket pairs; `TapeKind` is JSON's nine node types; regex classification
  matches pattern *strings* (`"[ \t\n\r]*"`) rather than HIR structure.
- **The Lock-14 firewall is a contrivance ledger, not a gate:** 5,283 lines, 47 append-only owner-path
  slices, 20 commit-subject matchers, a self-authorizing entry for its own file, and a deliberate scan
  exclusion of exactly the surfaces where the violations live (json_sink_direct/, json_templates/,
  css_scan_direct/, runtime_generator.rs). It also asserts editorial authority over `../crates/core/`
  across the workspace boundary. Verdict: never converges by construction. The live totality gate
  (`lock14_grammar_generalization.rs`) has the right structure and the wrong scope (2 crates of ~6
  generic surfaces; hand-maintained 17-token roster).
- **Totality:** `core` is a five-concern god crate (46,862 hand-written LOC) with ten hardwired grammar
  modules; `xtask` is itself the per-grammar codegen fork the campaign exists to eliminate (regen_css.rs
  1,967 LOC CSS-only with hard name asserts; regen_simple_runtime.rs 4,343 LOC with four RuntimeStyle
  arms and post-hoc string `.replace()` patching); four false-`@generated` couriers persist in skinny.

**Adjudication of Lock-14 at HEAD (resolving report-11 vs verify-1):** GREEN-BY-EXCLUSION, RED IN
SUBSTANCE. The gate is honest about what it scans and passes honestly; the system it certifies is a
stub because the scanned surface is 2 crates while the violations live in core/xtask/skinny-codegen.
Both things are true; the synthesis adopts the harsher reading for all forward claims.

## 5 · Honesty: benches, oracles, claims

- **CSS:** ratios 1.50–3.27× are plane-symmetric and real; every committed absolute Mbps figure ran
  under loadavg 4.35–6.79 and is directional only; the depth asymmetry (lazy value-HEAD classify vs
  eager full CSSOM) is disclosed in FRAMING.md but H1-LEDGER:11-13 still carries the contradicted
  "equal-depth" sentence; the oracle certifying the typed product is itself head-level *while its
  cssparser reference already computes the full token tree and discards it* — the full-depth oracle is
  one flip away (research/26).
- **JSON:** 51/51 vs sonic-rs strict is gated but warm-Criterion (the cold-only precept is violated at
  the claim plane); the widest margins are on the validate-and-discard lane; simdjson/yyjson native DOM
  are not gated; and the NEON JSON scanner is **dead on the live path** — the claim rides scalar
  scanning + serde_json number decode (research/18, 20). The claim survives; the implied mechanism does not.
- **Oracles:** five of nine grammars have zero correctness oracle; one CSS color gate is dead code
  (early-returns on empty); the named-color oracle is keyed to the grammar's own constants, not W3C.
- **Thermal mixing** (cold CSS vs warm JSON in one compound sentence) is the single most load-bearing
  disclosure gap (research/06, 14).

## 6 · Frontier: what the deep-dives establish

- **SIMD (18):** three competing structural-classification schemes; unify on the lo/hi nibble two-LUT
  (subsumes both, no CSS hi-nibble collision); replace every `vaddv` movemask with `vshrn_n_u16::<4>`
  (serializing cross-lane reduction on the two hottest kernels — the lowest-effort, highest-certainty
  Apple-M win); make the G6 CSS scan classify each block once with carried bracket/quote state instead
  of re-scanning from every jump.
- **dav1d transposition (19):** the missing piece is not asm — it is the *chain of consequence*:
  cost-model → KernelPlan → emitter composes grammar-agnostic kernels → checkasm generated per-kernel
  and bound to LIVENESS (asserting planned kernels are reached on real corpus bytes), strict by default.
  The asm line holds: intrinsics by default; `asm!`/`global_asm!` admitted only for proven
  mis-schedules, missing intrinsics, or cross-chunk spill control — each with a cargo-asm-proven case.
- **SOTA gaps (20):** the live gap has moved to the leaves — grammar-emittable Eisel-Lemire number
  decode, NEON bulk string unescape (the uXXXX quartet kernel already exists, unwired), branchless
  bit-drain dispatch, tokenize-once for CSS declaration bodies.
- **Types (21):** strike the HM claims (W0 honesty); then MapExpr synthesis, a HostSigTable,
  `TypeDesc::Sum` + `TypeDesc::Record(Row)`; the genuine HM core (type variables, unification,
  principal rule schemes) is a designed, staged build — CSP stays where it belongs (layout/strategy
  selection), egraph stays where it belongs (typed rewrite + cost extraction).
- **Value API (22):** one substrate (the skinny offset/flag-plane tape), eager materialization as a
  generic fold over the lazy cursor, ONE grammar-derived path alphabet (Field from keyed groups, Index
  from repetition, Variant from alternation, combinator segments for CSS, range segments for Sheets),
  and direct-to-struct as a serde `Deserialize` bridge over the lazy cursor.
- **Topology (23):** split `core` into frontend / codegen (the ONE emitter) / regex (unified) /
  runtime; extract `ir/passes` (24,420 LOC) into its own crate; absorb `analysis`; one simd crate; one
  regex home (the canonical parse-that engine — the skinny name-collision crate is deleted, research/17).
- **Decision engine (24):** source `perf_cost` from a checked-in, versioned KernelCostTable —
  (KernelId, Uarch) → measured cycles/64B populated by the checkasm/PMU harness, dav1d-style — and
  unify skinny's BackendShape engine with core's parallel `csp_strategy` subsystem into ONE extraction
  over (BackendShape × KernelStrategy), emitting a byte-reproducible Lock-16 manifest.
- **Language (25):** BBNF already expresses full-depth CSS L4 and JSON structurally; only TWO live host
  hatches exist fleet-wide, both byte-level transcoders — add MapExpr byte-fold primitives and they
  move into the grammar; extend `type_annotation` to named tuples and tagged variants (unblocks Sheets
  typed aggregates); land-or-delete the unused closure/GrammarCall/@host surface.
- **Depth closure (26):** ONE additive move — emit value-component tape events (FunctionOpen/Close,
  ComponentBoundary, typed ValueLeaf) at boundaries the balanced scanner already finds, type leaves via
  the proven Sheets `classify_projection` machinery, flip the oracle to the full token sink it already
  computes. Then re-state the >SOTA ratio downward honestly on both summary and full-tree-touch planes.
- **Seam (27):** skinny's `BackendExpr` is THE backend-agnostic IR; the full tree's Emitter trait is
  no-op-defaulted scaffolding honored only by backends that don't ship. Promote BackendExpr, key a real
  `Emitter` trait 1:1 on its variants, fold CSS lowering into it (deleting the third codegen path), and
  the Rust+ASM hybrid becomes a RustEmitter *specialization* selected by the decision engine — no new
  codegen path, Lock-14 drained by construction.
- **Substrate (28):** the SPEC's W0–W2 counts were stale at six sites (3 builder families not 11;
  5-grammar cohort not 6; 4 RuntimeStyle arms not 5; ~1,080-hit delete surface not 47; W0 gate line-refs
  deleted by W6a; W6a/b/c already landed) — all corrected in the re-founded SPEC. Port the skinny tape
  wholesale; rewrite the cursor against SoA (don't adapt the AoS one); one fenced landing.

## 7 · Novelty and the cutover

*(From the gap-fill deep-dives; full arguments in `research/31-cutover-decision.md` and
`research/32-novelty-prior-art.md`.)*

**Novelty: real but narrow; best-in-class NOT earned.** The thesis occupies a genuinely empty seam.
Every parser-generator lineage (tree-sitter, pest, lalrpop, ANTLR, ragel) generates SCALAR parsers —
ragel reaches asm but byte-at-a-time goto-FSMs, no vector classification. Every SIMD-parser lineage
(simdjson, sonic-rs, yyjson, Mison/Sparser/Pikkr, simdcsv) hand-writes per single format with no
grammar input. FlatBuffers/Cap'n Proto are zero-copy only over producer-controlled wire formats. The
one seam-ancestor is **Parabix** (Cameron/Lin) — and its scoped general BNF/EBNF grammar-to-SIMD
generator never shipped; delivered Parabix is regex/XML over parallel bitstreams, not block-classify
NEON from a CFG with a typed value plane. The defensible novelty is the CONJUNCTION — declarative
grammar input + emitted block-wide NEON classification + backend-agnostic IR + grammar-derived typed
value plane over one zero-copy tape — none of whose conjuncts is individually novel, whose product is
unattested. Claims we must NOT yet make: "best in class" (five grammars lack oracles; the live JSON
SIMD scanner is dead behind the GrowOnly gate, so the JSON win is the scalar tape walk; numbers decode
through serde_json), "beats simdjson" (not a gated comparator), or any unqualified compound >SOTA
sentence (§5). Verdict: ON TRACK — architecture novel, demonstration two waves short.

**Cutover: GO.** The skinny proving function is ~95% exhausted — SoA tape, un-forked generator, 5-shape
selection, NEON classifier, lazy value HEAD are all proven and portable; what remains is TRANSFER,
which is unprovable in skinny by construction. Exactly one architectural question retains a cheap
skinny proof (deriving JSON's rich keyed/recursive navigation so json_templates un-couriers) and it is
folded into TW2 rather than a new skinny cycle. The mechanics:

1. **Cutover commit 0 (doc-only):** the D-G pre-block reconcile (REDRESS 51/53/246/247) onto the
   committed ledger — the single true precondition. Then tag skinny `sk-v19-frozen`,
   architecture-read-only: kept as differential oracle + checkasm/microbench harness, never as the
   iteration tree (that is the divergence engine), archived after the 9-fleet fold completes.
2. **The cutover gate is a runtime artifact, not a document:** `xtask cutover-gate` exits 0 only when
   the Minimum Shippable Totality Slice is green — JSON (rich-recursive, >SOTA-load-bearing) + Sheets
   (flat negative control) end-to-end on the SoA tape in `crates/core`; independent oracles
   field-for-field; cold same-run bench at host_loadavg < 1.0 with no regression vs the pre-fold
   baseline; the WIDENED Lock-14 scan at zero. This gate is the TW2/TW4 close condition — the first
   wave in the campaign to close on a runtime consumer.
3. **First totality wave: the fused substrate landing** (port skinny `tape/` wholesale; delete AoS
   TapeRec + the eager builder families for the slice grammars in the same fenced commit, gated on parity).

The pre-restart lesson (substrate shipped with 0/17 consumers green) and the meta-process lesson
(paperwork certifying paperwork) are the same failure from opposite ends, and both are cured by one
rule: **every TW wave closes on a green runtime gate in `crates/`, and no wave merges emit-side
artifacts without a live consumer in the same wave.**
