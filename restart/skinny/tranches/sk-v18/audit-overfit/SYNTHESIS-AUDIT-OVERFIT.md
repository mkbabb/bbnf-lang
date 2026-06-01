# SK-V18 S-P0 Overfit Audit — SYNTHESIS-AUDIT-OVERFIT

Date: 2026-05-31. Cycle: V1 authored; V2 fold-applied (the 7 V1 CHALLENGE REVISEs folded in);
V3 confirmed (independent re-grep). **CONVERGED = TRUE** — §3Z met: ≥95% × 2 consecutive
(V2 100% + V3 100%), zero orphan REVISE, V≤5, CH7 the spine; see
`hardening/HARDENING-S-P0-CONSOLIDATED.md` §2. Composing agent: A3.
Pass: S-P0 (the FIRST pass of SK-V18, post-G-Alpha), per `PASS-0-OVERFIT-AUDIT.md`.
Bracket contract HEAD: `318d9c046` (SYNTHESIS/HANDOFF snapshot).
Live audit HEAD: `83b66db4232374db6a5f9fa009882f41acc04342` (the tree S-P0 grepped —
every path:line re-verified at THIS HEAD).
SK-V17 closed `f6a38445b`; V3 audit seed `7dbe44c22`.

Per-axis artefacts (the canonical S-P0 axis set, one per fan-out agent A0–A3):
`a0-goalset-residual-overfit.md` (A0 — goalset residual verdict + R-A0-1/2/3),
`a1-six-addenda-lens-registry.md` (A1 — the L1–L6 addenda lens registry, the load-bearing
output), `a2-prune-sequencing.md` (A2 — the PRUNE-list P1–P5 + the four sequencing constraints),
`a3-arch-measurement-gate-residual.md` (A3 — the arch/measurement/gate axis + the NEW R16
nested-`output_labels` gate-recipe finding). Convergence: `hardening/V{N}/{CH1..CH7}.md` +
`HARDENING-S-P0-CONSOLIDATED.md`. This SYNTHESIS (A3) consolidates a0–a3 into one binding input.

## §0 — Headline

SK-V18 is the GENERALIZATION cycle — the inflection backtrack. S-P0 audits the LOCKED
SK-V18 goalset surface for RESIDUAL overfit/contrivance/hardcoding that survived the Alpha
CHALLENGE, and formalizes the 6 new CHALLENGE addenda (from the V3 implementation-overfit
audit) into binding S-P0 lenses.

**Verdict: the goalset surface is HONEST. The residual overfit it targets is EXACTLY the
V3-found set, verified STILL LIVE at HEAD `83b66db42`, with all six addenda gates bound
correctly by the Alpha contract.** S-P0 found ONE NEW finding (F-A3.5, a precision hazard in
the relocated-seam gate recipe, MEDIUM, pinned to S-P3), and ZERO new hardcoding admitted by
the goalset. Every addendum FIRES on a real live surface — none is decorative.

Unlike SK-V13 (where S-P0's progenitor audit found the headline ADMITTED numbers were FAKE),
SK-V18's headline >SOTA is MEASUREMENT-VALID (JSON cold strict +1.4%–164.7%; CSS canonical
cold N=200 1.9–3.3×) — the residual is NOT a fake admit, it is the HAND-WRITTEN, FORKED,
REPLICATED implementation under a "grammar-driven" banner. SK-V18 prunes that overfit and
backtracks both hand-written parsers into ONE grammar-driven generator, preserving the >SOTA
honestly. This synthesis is the binding S-P0 input S-P1/S-P2/S-P3 consume.

## §1 — The 6 binding addenda (formalized; full L1–L6 lens registry in a1, goalset-residual restatement in a0 §1)

Each addendum is the generalization of a V3 dispositive finding into a forward lens. Each is
verified LIVE this pass; each is bound THREE ways by the Alpha contract (close-gate row +
§0.4 pre-block + telemetry column the `gate-json` consumer REJECTs on). These are the
load-bearing output of S-P0 — they bind into every downstream pass CHALLENGE.

| # | Addendum | Predicate (REJECT trigger) | Live witness (`83b66db42`) | Cleared by |
|---|---|---|---|---|
| 1 | **verbatim-blob** | a `@generated` file that is a verbatim `&str` literal in codegen = hand-written, NOT derived — REJECT as "grammar-driven" | `runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"`; `json_sink_direct.rs` 7× push_str | G2 (+G1) |
| 2 | **distinct-grammar-output** | N grammars must have N NON-identical `generated.rs`; md5-distinctness is NECESSARY-NOT-SUFFICIENT (also require branch-count==0 + type-count==0 + row-collapse) | 7× css_l4 `generated.rs` md5 `b654562c…` | P3 + PROVE |
| 3 | **single-emitter-path** | one grammar-agnostic emitter; flag grammar-family forks | `grammar_provider.rs:40-42 RuntimeEmitterKind{CompiledLowering,RequestFacts}` | G3 |
| 4 | **phantom-generic** | a generic `<G>` never instantiated with a real type outside `#[cfg(test)]` is decorative — instantiate-or-delete (DELETE default; preserve JSON rich nav so ≥2 impls cannot LCD-flatten) | `tape/mod.rs:175 G: EventGrammar = AnyGrammar` (test-only) | G4 |
| 5 | **timed-plane-symmetry + corpus-in-timer** | the >SOTA comparator must do EQUAL work on the REAL corpus, COLD (no micro-fixtures, no more-work-competitor; canonical `css_canon_bench` is honest) | `nonjson_css_l4.rs:3091 measure_mbps` warm 2000-iter micro-fixtures | P2 (+H1) |
| 6 | **acceleration-wiring** | a NEON/ASM acceleration claim must show the kernel reached AT ADMISSION (hot path), not only under `#[cfg(test)]` | `find_css_significant` caller only in `lib.rs:574` `#[cfg(test)]` | G6 |

**S-P0 hardening of the addenda (carried into every wave CHALLENGE):**

- Addendum 2 is a **3-co-gate CONJUNCTION**, not an md5 check: {md5-distinct ∧
  `generator_grammar_branch_count == 0` ∧ `generator_grammar_type_count == 0` ∧
  `runtime_target_rows_collapsed == true`}. The relocated-seam (a per-grammar branch moved
  into a neutral-identifier data-table) is caught ONLY by the structural row-collapse, never
  by a regex.
- Addendum 4 must point at the `G` (EventGrammar) axis, NOT the REAL `K` (Kind) axis; and the
  grep must test-exclude (the standing `_proof_compiles::<JsonEventGrammar>` is test-only and
  must NOT false-green). The companion `json_rich_navigation_preserved == true` makes the ≥2
  impl-count necessary-not-sufficient (preserve-rich-ast).
- Addendum 6's RETIRE branch is gated on a samply non-top-N MEASUREMENT (profile-first, an
  S-P1 dependency), not an assertion; every primitive lands WITH its hot-path consumer in the
  same commit (no orphan kernel).

## §2 — The residual census (every finding LIVE at `83b66db42`)

The complete residual surface, consolidated from the canonical axis artefacts: a0
(`a0-goalset-residual-overfit.md` — the goalset verdict + R-A0-1/2/3 framing residuals), a1
(`a1-six-addenda-lens-registry.md` — the L1–L6 lens witnesses), a2 (`a2-prune-sequencing.md` —
the PRUNE-list + sequencing couplings), a3 (`a3-arch-measurement-gate-residual.md` — the
arch/measurement/gate axis + R16). Severity: HIGH = falsifies a campaign claim; MEDIUM =
honesty/discipline residual. NEW = first surfaced by S-P0. The R1–R16 rows are the implementation
residuals (live overfit verified on disk); the R-A0-* rows are the goalset FRAMING residuals
(the seams the contract's own escape clauses leave open) — both feed the wave CHALLENGEs.

| # | Residual | Addendum/axis | Live witness | Disposition | Sev |
|---|---|---|---|---|---|
| R1 | CSS const-`&str` courier (generator does not exist) | 1 | `runtime_generator.rs:701` | G2 | HIGH |
| R2 | JSON fixed-literal render | 1 | `json_sink_direct.rs` 7× push_str | G1 | HIGH |
| R3 | RuntimeEmitterKind grammar-family fork | 3 | `grammar_provider.rs:40-42` | G3 | HIGH |
| R4 | 7 byte-identical css_l4 replicas | 2 | 7× md5 `b654562c…` | P3 | HIGH |
| R5 | phantom `<G>` (EventGrammar axis) | 4 | `tape/mod.rs:175` | G4 | HIGH |
| R6 | shared-trait LCD-flatten hazard | 4 | divergent JSON-tree vs CSS-flat | G4 | HIGH |
| R7 | CSS NEON dead at admission | 6 | `lib.rs:574 #[cfg(test)]` | G6 | HIGH |
| R8 | x86 two surfaces (wrong-arch) | A6 | `src/x86_64/` + `ext/x86/` + nasm | P1 | HIGH |
| R9 | Lock-14 green-by-exclusion gate | A3 | `lock14_baseline.rs:2442/2463` | P4 | MEDIUM |
| R10 | DM3 `_neon` scalar mislabel ×5 | 6 | dispatch passthroughs | G6 | MEDIUM |
| R11 | DM4 UDOT orphan | 6 | `digit_mac.rs` test-only | G6 | MEDIUM |
| R12 | JSON scanner non-neutral holdout | A6 | `json/scan.rs:201` | G5 | MEDIUM |
| R13 | warm micro-fixture CSS bench | 5 | `nonjson_css_l4.rs:3091` | P2 | HIGH |
| R14 | lazy-vs-eager framing asymmetry | 5 | `track1_rich` lazy | H1 | MEDIUM |
| R15 | metalang leak `parse_w11_1_number` ×7 | A1/regen | `json/generated.rs` | P5 | MEDIUM |
| **R16** | **nested-struct gate-recipe hazard (recipe must inline BOTH `frontend_requirements` AND `output_labels`)** | A3 gate-precision | `regen.rs:17-18` + `regen_css.rs:47-52` | S-P3 recipe pin (full-row `PartialEq`, +1-line derive) | **MEDIUM (NEW)** |
| R-A0-1 | lazy-vs-eager framing as the DEFAULT comparator (H1 "re-frame OR symmetric" — the OR lets the re-label close the honesty gate) | 5 framing | `track1_rich` lazy vs eager CSSOM | H1 (symmetric-comparator branch preferred; the re-label branch is acceptable ONLY with the materialization-depth asymmetry disclosed EXPLICITLY — an unqualified "beats CSSOM"/"equal-work" close-report claim behind a re-label is a REJECT, per a0 §4) | MEDIUM |
| R-A0-2 | P3 collapse-vs-differentiate decision DEFERRED to B2 on a RED-by-design gate (profile-distinctness-erasure hazard) | 2 distinct-output | `regen_css.rs` 7 distinct `profile` | S-P3 binds which branch each of the 7 profiles takes; DISK EVIDENCE is collapse-to-one (one `stylesheet.bbnf`, byte-identical output — `generator_grammar_count == 3` = json+css+sheets, NOT json+7-css+sheets); manufacturing 7 fake `.bbnf` roots to satisfy a distinctness gate is the EXACT overfit the addendum forbids (a0 §5) | MEDIUM |
| R-A0-3 | honest-finding "named primitive" escape — the single largest paper-close surface (prose-reviewed, not machine-checked) | 1 verbatim-blob | HANDOFF §6 / PASS-IMPL-V4 row | escape gated (a)-(c) machine-checked: grammar-invoked + grammar-derived-data + `verbatim_blob_present==false` | MEDIUM |

The R-A0-* rows are framing residuals A0 surfaced (a0 §0); a3's R16 + a2's relocated-seam binding
+ a1's L1–L6 machine-checks close them: R-A0-1 ⊆ R14 (H1 framing); R-A0-2 ⊆ R4/P3 (the collapse
decision bound to S-P3); R-A0-3 is the verbatim-blob escape, bound (a)-(c) in §6.

CLEAN (verified honest, KEEP): the unified `Tape`/`ValueRef`/`PayloadArena` substrate (Lock 1
holds — the genuine foundation); the neutral alphabet NEON kernel (caller-data, already
generalized); the canonical `css_canon_bench` (cold, N≥50, real corpus, no broadcast); the
14-file checkasm discipline (12 single-kernel + common + parity); the regen plumbing; the
FNV bench-quarantine; the JSON >sonic-rs strict cold proof; the CSS canonical cold proof.

### §2.1 — Generator/codegen-axis wave obligations (CHALLENGE-folded sharpenings)

Three generator-axis sharpenings the S-P0 CHALLENGE folded (carried as binding CH7 wave
obligations; the underlying live witnesses are a1 §L1/§L3 + a2 §4/§5):

1. **JSON projection diff-control (R2/G1).** The same-wave regen MUST diff-match the
   `json_templates/` byte-for-byte oracle BEFORE the oracle is deleted — that diff-match (not a
   ±5% line-count delta) is the BINDING proof the projection is real, not a re-stringification.
   The ±5% line-count is a SOFT tripwire only; a faithful projection may legitimately
   reorder/dedupe past it. (`clean-regen-discipline`.)
2. **Witness-emission scan-root coupling (P4).** The `JsonEventGrammar`/`SheetsEventGrammar`
   witnesses live in `runtime/` (NOT the P4 codegen scan root). IF the un-forked generator EMITS
   a grammar-named `EventGrammar` literal into the generated runtime, the
   `runtime_generator.rs`-scoped `FORBIDDEN_GENERIC_TOKENS` must add `EventGrammar`/`*EventGrammar`
   so the emitted witness is caught at its emit site (HANDOFF invariant 5).
3. **The CSS-courier LOC is cohort-carried (R1/G2).** `CSS_GENERATED_RS` is ≈910 LOC
   (cohort-carried, not gate-keyed) — no gate keys on the exact figure; the binding gate is
   `verbatim_blob_present == false` + the `.bbnf`-mutation test.

## §3 — Per-axis verdict (PASS-0-OVERFIT-AUDIT A1–A6 mapped)

| Axis | Verdict | Evidence |
|---|---|---|
| **A1 Measurement integrity** | HONEST headline; 1 contrivance surface (R13) to delete + 1 framing residual (R14) to disclose | css_canon_bench cold N≥200 real-corpus distinct medians, no broadcast (V6); the warm micro-fixture path did NOT produce the numbers but is live (P2 deletes) |
| **A2 Admit-mechanism** | every >SOTA admit cites a real parser/codegen change, strict-vs-strict same-plane, per-iter equality | JSON 51/51 strict cold; CSS 9-field EXACT cssparser oracle; no gate-relabel admit |
| **A3 Lock-14 generic-crate** | green-by-EXCLUSION (R9) — the gate reads CLEAN only because it omits the leak surface; P4 fixes BEFORE the rebuild | `GENERIC_SCAN_ROOTS` omits `runtime_generator.rs`; `diagnostic-x86` exclusion live |
| **A4 Generator-vs-hand-curated** | the generator DOES NOT EXIST — two forked hand-written parsers + 7 replicas (R1/R2/R3/R4); the round-trip passes but over hand-written content | `CSS_GENERATED_RS` const courier; `.bbnf` never consumed; 7× identical md5 |
| **A5 Decision-engine / substrate fold** | substrate REAL (Lock 1, neutral kernel); value-API + codegen NOT yet — the inflection backtrack | one `Tape`/`ValueRef`; CSS reuses sparse flag pair; no second tape |
| **A6 Pre-restart recurrence** | x86 wrong-arch tree (R8); phantom witness (R5); non-neutral JSON holdout (R12) — all named, all pruned/generalized | `src/x86_64/` + `ext/x86/`; test-only `G`; bespoke `json/scan.rs` |

**Net:** the goalset surface contains NO fake admit (unlike SK-V13). It contains the
hand-written/forked/replicated overfit the V3 audit named, verified LIVE, plus the
green-by-exclusion gate and the wrong-arch tree. SK-V18 prunes and generalizes it. The
addenda gates make the rebuild PROVABLE (a courier-swap or replica-relabel cannot pass).

## §4 — The PRUNE-list (binding before GENERALIZE; the standing order)

PRUNE lands FIRST — it reduces the surface for the GENERALIZE waves and makes the Lock-14
gate trustworthy BEFORE the emitter rebuild. Net LOC ≈ −10800 (deletes far more than the
campaign adds). Each prune cites its residual + its falsifier.

- **PRUNE-1 (P1) — DELETE the WHOLE x86 surface crate-wide (R8).** BOTH surfaces, deletion
  list reach-matched to the verify grep: `src/x86_64/` (24 files) + `ext/x86/` (vendored ASM)
  + `build.rs` (nasm driver) + `Cargo.toml` nasm-rs dep + `lib.rs:5`/`:247`/`:285-288` +
  DECOUPLE `checkasm_parity.rs` (build-soundness) + CLEAN `byte_class_from_eq_set_64.rs` doc
  strings. Falsifier: `find …/src/x86_64 …/ext/x86 -type f = 0` AND crate-wide grep
  aarch64-neutral AND `cargo build` + `cargo test --no-run` clean. ≈ −4500 LOC.
- **PRUNE-2 (P2) — DELETE the warm micro-fixture CSS bench (R13).** `nonjson_css_l4.rs`
  `measure_mbps`/`*_lightningcss_facts` + the SHA scaffolding. KEEP `css_canon_bench`.
  Falsifier: `grep measure_mbps|lightningcss_facts = 0`. ≈ −700 LOC.
- **PRUNE-3 (P3) — COLLAPSE the 7 byte-identical css_l4 replicas (R4).** To ONE CSS grammar,
  OR N-distinct if genuinely differentiated by distinct `.bbnf` roots. AND collapse the 7
  xtask `RuntimeTarget` rows to one config (preserving profile-distinctness where the profiles
  are genuinely distinct grammars). Falsifier: md5-distinct (no byte-identical pair) AND the
  structural row-collapse co-gate (see R16). ≈ −5460 LOC.
- **PRUNE-4 (P4) — FIX the Lock-14 green-by-exclusion gate (R9). MUST LAND BEFORE G2/G3.**
  Move `runtime_generator.rs` + the JSON sink/typed/template surfaces from the weak
  `SKV15_W2_EXTRA_COVERAGE_ROOTS` into strict `GENERIC_SCAN_ROOTS`; extend
  `FORBIDDEN_GENERIC_TOKENS` with `CSS_`/`_RS`/`EventGrammar`/`*EventGrammar`; drop the
  `diagnostic-x86` exclusion. Falsifier: re-inject a `JsonSink` token → gate turns RED
  (proves coverage), then revert; `lock14_gate_scans_codegen == true`.
- **PRUNE-5 (P5) — PURGE the metalang leak (R15).** Fix at the source
  (`json_sink_direct.rs`) so `regen --check` stays clean; rename `parse_w11_1_number_*` →
  `parse_number_*`. Falsifier: `grep -c parse_w11_1_number = 0`; no `w[0-9]+`/corpus/`sk_v`
  tag in shipped runtime; regen clean.

## §5 — Sequencing constraints (PRUNE → GENERALIZE → PROVE → HONESTY)

The standing order is binding. The revert dependency graph is the entry-gate chain — a wave
that fails its exit gate BLOCKS every downstream wave that entry-gates on it.

```
PRUNE (P1..P5; P4 BEFORE the emitter rebuild)
  └── G1 (JSON projection, parity oracle)         [entry: P-cluster closed]
        └── G2 (CSS lowering, retire const courier) [entry: G1 + P3 closed]
              └── G3 (un-fork emitter)              [entry: G1, G2 closed]
                    └── G4 (shared trait + phantom) [entry: G1, G2, G3 closed]
                          └── G5/G6 (JSON neutral NEON / CSS NEON wire-or-retire)
                                └── PROVE (Sheets via the un-forked generator ONLY)
                                      └── H1 (CSS framing honesty + regen --check clean)
```

**Binding sequencing facts S-P1/S-P2/S-P3 must encode:**

1. **PRUNE-before-GENERALIZE** is the standing order (V3 §backlog). No GENERALIZE candidate
   dispatches until its named PRUNE predecessor closes. PRUNE carries ZERO generalization risk
   (pure deletion + gate-tightening) and deletes no >SOTA-bearing code.
2. **P4 (Lock-14 gate) MUST land BEFORE G2/G3** — the gate must be meaningful when the new
   emitter is authored, or a grammar-named branch could be re-introduced undetected.
3. **G1 failure BLOCKS G2/G3/G4/PROVE.** G3 (un-fork) failure BLOCKS PROVE (Sheets emits
   THROUGH the un-forked generator). No downstream wave dispatches over a REDRESSed predecessor.
   (Note the dual entry-gate: G2 entry-gates on BOTH G1 AND P3 — a P3 failure also blocks G2,
   independent of G1; the dependency-graph diagram above is authoritative.)
4. **S-P1 (profile) is a hard dependency of G5/G6.** The G6 NEON retire branch is gated on a
   samply non-top-N MEASUREMENT — S-P1 must re-confirm the JSON+CSS hot leaves on the benched
   path BEFORE any G5/G6 kernel lands (profile-first, no orphan kernel; actual-profiling).
5. **R16 (nested-struct recipe) binds to S-P3** — the gate-consumer author must
   compute `runtime_target_rows_collapsed` over the STRUCTURALLY-EXPANDED row inlining EVERY
   nested-struct field — BOTH `frontend_requirements` (field #11) AND `output_labels` (field #12)
   — not the prose's 3 named pseudo-fields and not a single named nested struct (a recipe that
   recurses into `output_labels` only would slip a future seam riding `frontend_requirements`).
   A `RuntimeTarget: PartialEq` full-row collapse satisfies this and is PREFERABLE (it covers both
   nested structs automatically and cannot be coupled to a hand-rolled field list). Cost:
   `RuntimeTarget` today derives only `Clone, Copy, Debug` (`regen.rs:5`), so this requires ADDING
   the `PartialEq` derive (one line; both nested structs already derive `PartialEq, Eq` at
   `grammar_provider.rs:45`/`:91`). A hand-rolled prose-field comparison risks a shallow-compare
   false-green of EITHER nested struct.
6. **Hard caps** (standing `[dispatch-hard-cap]`): research/plan/redress 20/15/30 min, "at
   0.9N commit, at N halt"; the Sheets/NEON cluster (PROVE/HONESTY) is MED-HIGH and may carry
   a documented larger cap.

## §6 — What S-P1/S-P2/S-P3 consume from this synthesis

- **S-P1 (profile):** re-confirm the JSON+CSS hot leaves COLD on the benched path BEFORE any
  G5/G6 kernel; the samply attribution is the gate for the G6 retire branch (sequencing fact
  4). The hot CSS scan is currently SCALAR (R7) — profile confirms whether any NEON wiring is
  warranted or honest-retire is correct.
- **S-P2 (research):** the surviving grammar-neutral candidate classes for the generator
  projection (how JSON projects from `SinkOnlyProgram`; how CSS lowers from `.bbnf`; how the
  shared `Value`/`Document`/`Cursor` trait stays lazy over the EXISTING tape without a second
  substrate or eager tree). Bounded by the addenda: no const-courier swap (1), distinct
  output (2), one emitter (3), no new phantom (4), corpus-in-timer (5), accel-at-admission (6).
- **S-P3 (synthesis-plan / SPEC):** the wave manifest sequenced per §5; the executable
  `--skv18-generalization-report` gate consumer binding all telemetry columns; the R16
  recipe pin (full-row collapse incl. nested `output_labels`); the revert dependency graph +
  hard-cap defaults; the CH7 Overfit-Prune lens carried into every wave CHALLENGE.

The CH7 lens directive is binding on every SK-V18 wave: every new code is grammar-derived
(template + grammar metadata + emission command), never hand-written under `// @generated`;
Lock-14 generic-crate compliance preserved; every admit via a real source change, strict
same-plane, per-iter oracle; every generated output passes round-trip (delete + regen ⇒
byte-equivalent); no scaffold-only landing counts as an admit; the honest-finding escape
qualifies ONLY under (a) grammar-INVOKED by name + (b) grammar-DERIVED data + (c)
`verbatim_blob_present == false` — a primitive failing these is a relabeled hand-written
blob, REJECT (the single largest paper-close surface in the contract); AND the 6 addenda fire
as REJECT triggers (verbatim-blob, distinct-output, single-emitter, phantom-generic,
timed-plane + corpus-in-timer, acceleration-wiring).

## §7 — Convergence posture

The S-P0 audit-overfit pass converges under ORCHESTRATOR §3W/§3Z (≥95% ACROSS CH1–CH7 for
TWO consecutive cycles, where CH7 is the Overfit-Prune lens, zero orphan REVISE, V≤5). The
per-cycle CHALLENGE artefacts live in `hardening/V{N}/{CH1..CH7}.md`; the rolling verdict in
`HARDENING-S-P0-CONSOLIDATED.md`. The CHALLENGE has run THREE cycles and CONVERGED:
**V1 85.7% (42A/7R/0) → V2 100% (47A/0R/0) → V3 100% (47A/0R/0); CONVERGED = TRUE.** The 7 V1
REVISEs (single-edit, mechanism-correct, non-architectural) were folded into a0–a3/SYNTHESIS,
DISCHARGED at V2, and re-confirmed at V3; zero REJECT across all cycles. The one non-blocking
CH7-V2 cosmetic (the a3 §3 `RuntimeTarget` line-vs-field numbering paraphrase) is absorbed at
a3 §3 (the `NN:` prefixes are annotated as `regen.rs` source lines, not field ordinals). This
synthesis + a0–a3 are the CONVERGED substrate the CHALLENGE lenses disposed — the binding
input S-P1/S-P2/S-P3 consume.

**S-P0 found ZERO CRITICAL/HIGH NEW residual** — every HIGH residual (R1–R8, R13) is the
V3-found overfit verified STILL LIVE and already bound to a named SK-V18 wave; the one NEW
finding (R16) is MEDIUM (a gate-recipe precision pin to S-P3). Per the PASS-0 failure-mode
clause, a CRITICAL S-P0 finding would HALT the SK-V18 waves until the prune list converges;
since none was found, forward motion proceeds. The prune list feeds Pass S-P1 directly — and
here the prune list IS the goalset's own PRUNE cluster, already CHALLENGE-survived through
Alpha V5, so S-P0 confirms cleanliness (and hardens the addenda + pins R16) rather than
blocking forward motion.

**Next-tranche tee-up:** a successful SK-V18 generalization (one generator emits JSON+CSS+Sheets
from `.bbnf`, shared trait both instantiate, phantom resolved, >SOTA preserved, x86 gone, gate
meaningful) makes SK-V19 the totality-fold tranche (`crates/core/` adoption) + BBNF-self as the
fourth grammar litmus. If a generalization wave proves a grammar-derived parser CANNOT preserve
the >SOTA without hand-shaping, that is a genuine §6 finding — the hand-shaping becomes a named,
validated, grammar-parameterized primitive (gated (a)-(c)), recorded honestly, never a silent
blob.
