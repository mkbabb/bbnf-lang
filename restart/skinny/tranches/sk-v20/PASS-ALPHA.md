# SK-V20 — Pass Alpha (cycle opening): the gestalt re-founding

Date: 2026-06-10. Cycle: SK-V20 (research tranche — the gestalt pass over the campaign in totality).
Predecessor: SK-V19 design CLOSED-CERTIFIED (the W0-W6 totality-fold SPEC), implementation HALTED after
the W6 leak cluster landed (`f4633d468` W6c structural re-key; `36ca10646` W6a metadata-driven manifest +
W6b css_types removal/host relocation + W6e the live Lock-14 self-gate, GREEN at HEAD). The remaining
SK-V19 waves (W6d, W0-W5) are NOT abandoned — they are inputs to this tranche's re-founding and will be
re-validated, amended, or folded forward by its output.

## Why this cycle exists

Before opening the totality implementation, the campaign takes one full-breadth research pass — history,
prototype, process, and frontier — to ensure the totality spec is founded on audited ground rather than
accumulated momentum. The standing failure mode this guards against is the pre-restart divergence: spec
drift, hardcoded grammars, per-grammar backend files. The skinny tree is a proving ground — a prototype
whose purpose is to GUIDE and INFORM the totality spec, never to be shipped as-is or imitated uncritically.

## The aim, restated as the binding charter

A fully generalized parser generator that bests every SOTA parser for its targeted structured-data
languages (CSS, JSON, and the fleet beyond), achieved by a perfected union of:

1. **Backend-agnostic core** — grammar + IR + decision layer carry zero backend and zero grammar
   specifics (Lock 14 live and GREEN, both trees).
2. **A specializing emitter in the dav1d lineage** — reference Rust path + hand-tuned aarch64/Apple-M
   kernels + a checkasm-style generated parity/microbench harness; parse speed is a first-order priority;
   kernels are grammar-agnostic primitives composed by the emitter and selected by the cost model.
3. **Full type resolution** — the most powerful inference reasonably possible: Hindley-Milner core over
   grammar rules, CSP for layout/strategy selection, egraph for typed rewrite and cost extraction; every
   `->` projection reaches the emitter; inference composes types, never loses them.
4. **A first-class Value API** — lazy and eager evaluation over ONE unified tape and backend layout; a
   unified, grammar-derived path/query API; direct-to-struct; matching and beating lightningcss, simdjson,
   yyjson, sonic-rs on their own terms, with every asymmetry disclosed.

Standing constraints carried verbatim: NO quick solutions, NO workarounds — idiomatic, gestalt
approaches; architectural transpositions for elegance, simplicity, and performance are necessary and
desirable; NO legacy code; no overfitting or over-emphasis of JSON/CSS; aarch64/Apple-M only; cold
benchmarks only; clean-regen discipline; one codegen path.

## The research program (this tranche; NOT an implementation phase)

A 32-agent audit fan-out across four planes, adversarially verified, then synthesized into the totality
re-founding:

- **History & process** — the pre-restart divergence catalogue (the negative template); the
  MASTER-PLAN/locks census; the SK-V3.5→V19 arc ledger; the meta-process local-maxima diagnosis (is the
  pass loop producing increments or paperwork?); the chronic-deferral census; the full precepts recap
  with addressed-status per directive.
- **Overfit** — skinny emitter/IR, the Lock-14 firewall itself (is it converging or accreting
  allowlists?), core runtime hand-written boundaries, IR recognizer residuals post-W6, false
  `@generated` couriers, the xtask per-grammar regen fork, bench-claim honesty, oracle depth census,
  generated-code budgets.
- **Frontier design** — the parse-that fold map; the SIMD/NEON kernel census and simdjson-gap analysis;
  the dav1d-style hybrid Rust+ASM emitter architecture; the SOTA teardown refresh; the type-inference
  architecture; the unified Value API design; the totality crate topology; the decision-engine/cost-model
  design; BBNF language-completeness for totality; the CSS depth-parity closure; the backend-agnostic
  seam census; the substrate unification map.
- **Verification** — three adversarial verifiers over the load-bearing claims; one completeness critic
  scored against the directive's twelve demanded dimensions.

## Outputs (the tranche deliverables)

1. `research/` — the 32 audit/verify/critic reports.
2. `GESTALT-SYNTHESIS.md` — the synthesized findings: overfit verdicts, process verdict, novelty/SOTA
   assessment, and the corrected campaign picture.
3. `DEFERRED-LEDGER.md` — every chronically deferred item, folded into the new tranche (nothing
   re-deferred without disposition).
4. `PRECEPTS-RECAP.md` — every standing directive with status and evidence.
5. `SPEC.md` — the totality re-founding: the forward wave plan (absorbing/superseding the SK-V19 W0-W6
   manifest where the audits demand), the cutover decision (when skinny development stops and totality
   implementation begins), and the actionable forward prompt.

## Gates

G-Alpha is this opening. The tranche closes at a certified `SPEC.md` (the Pass-Omega analogue for a
research cycle). No implementation lands in this tranche beyond documentation; the only code commits
permitted are gate/test infrastructure if an audit reveals a broken gate. Outcome scale: A (synthesis
lands with a certified totality spec + cutover decision) / S (honest residual, named) / N (the audit
invalidates the totality premise — surfaced, never paper-closed).
