# Pass Omega V9 CH2 Generality Hardening

Verdict: ACCEPT.

Scope: Pass Omega V9 source packet at `17e7248fe` under PASS-OMEGA Section 3
CH2 generality. Reviewed V9 Omega-A through Omega-F, `locks-diff.md`,
`master-plan-diff.md`, `restart/locks/LOCKS.md`, SK-V15 `SPEC.md`,
`PASS-IMPL-OVERFIT-AUDIT.md`, T-P2, and T-P3. This review edits no live V1
surface and does not stage or commit.

## CH2 Finding

The V9 packet preserves Lock 14 generality. It does not narrow the SK-V15 repair
to JSON, does not paper-close CSS, and does not introduce a directive, BIR
variant, public syntax, public substrate API, retained sidecar, sixth
`BackendShape`, sidecar EventTape, or production FNV route.

The proposed LOCKS and MASTER updates are proposal-only and G-Omega gated. They
route implementation truth through SK-V15 W0-W11: CSS admission honesty, Lock
14/16 gate restoration, generic codegen leak abrogation, Pattern H provenance,
CSS typed Value provider, same-workload `cssparser` retime, Decision Engine
activation, all-five lowerer proof, FNV quarantine, and PASS-IMPL V2 close.

## Evidence

1. The locks diff explicitly preserves the 16 locks and five-shape canon and
   adds no forbidden surface. `restart/audit/totality/astral/V9/locks-diff.md`
   states the diff preserves the 16 numbered locks, the five `BackendShape`
   variants, and adds no directive, BIR variant, substrate, public substrate
   API, retained sidecar, lock, lock retirement, or sixth shape
   (`restart/audit/totality/astral/V9/locks-diff.md:5`-`9`). The proposed
   addendum repeats the same boundary inside the hunk
   (`restart/audit/totality/astral/V9/locks-diff.md:45`-`48`).

2. Lock 14 is strengthened as a generic rule, not as JSON/CSS policy. The
   proposed Lock 14 clause permits generic codegen to consume generated provider
   manifests, generated grammar facts, and generated sink/fact/value surfaces,
   while banning `RuntimeGenerationMode`, profile arrays, CSS profile matches,
   JSON/CSS runtime families, JSON punctuation or role mining, generic grammar
   switches, and generic-crate grammar branches
   (`restart/audit/totality/astral/V9/locks-diff.md:67`). It also requires
   future grammar onboarding through source/metadata-only CSS plus Sheets or
   BBNF-self witnesses, with no new directive, BIR variant, sixth shape, or
   generic branch (`restart/audit/totality/astral/V9/locks-diff.md:67`).

3. SK-V15's canonical receiver matrix prevents a JSON-only or CSS-only close.
   Generic surfaces require CSS L4 plus Sheets/BBNF-self or another non-CSS
   generated receiver: `grammar_provider.rs` requires CSS plus Sheets or
   BBNF-self; `runtime_generator.rs` requires CSS plus Sheets, BBNF-self, CSV, or
   math; backend lowerers and cost facts require CSS plus Sheets or BBNF-self;
   egraph/CSP require CSS plus a non-CSS receiver
   (`restart/skinny/tranches/sk-v15/SPEC.md:206`-`217`). The Lock 14 gate also
   requires every generic-crate or generic-generator wave plan to name the
   generic owner path, forbidden token scan, non-JSON receiver, proof command,
   generated-output expectation, and fail action
   (`restart/skinny/tranches/sk-v15/SPEC.md:219`-`231`).

4. The CSS path is not paper-closed. SK-V15 requires no CSS 24-row broadcast
   admit, retirement of `CSS_GENERATED_RS`, `CssFullParseSummary`,
   fact-stream-only CSS `parse()`, and brace-counter admission, a typed CSS
   value/document/view/visitor provider comparable to JSON's Value API, and a
   same-workload `cssparser` comparator before CSS admission can close
   (`restart/skinny/tranches/sk-v15/SPEC.md:54`-`63`). V9 carries that split into
   MASTER as W5 typed Value provider and W6 same-workload retime
   (`restart/audit/totality/astral/V9/master-plan-diff.md:84`-`87`,
   `restart/audit/totality/astral/V9/master-plan-diff.md:105`-`106`).

5. PASS-IMPL overfit findings remain live blockers, not prose-dismissed history.
   PASS-IMPL defines the audit axes for grammar hardcoding, Pattern H runtime
   collapse, codegen/xtask Lock 14 leaks, bench/test contrivances, and
   cross-cutting backend specialization (`restart/prompts/skinny/PASS-IMPL-OVERFIT-AUDIT.md:24`-`35`).
   T-P2 records the implementation floor as JSON honest, CSS contrived, Pattern
   H not collapsed, generic infrastructure mixed, Decision Engine scaffold, and
   FNV bench scheme quarantined (`restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:48`-`57`).
   V9 Omega-B carries the same split and says JSON is scoped guard evidence, CSS
   must be demoted/rebuilt, Pattern H is open, Lock 14/16 gates are open,
   Decision Engine is scaffold, and FNV stays bench-only
   (`restart/audit/totality/astral/V9/ΩB-skinny-lessons.md:18`-`28`).

6. Pattern H generated-output discipline is preserved. SK-V15 requires exactly
   67 root runtime files with true generator provenance at line 1 and
   regeneration/check proof (`restart/skinny/tranches/sk-v15/SPEC.md:69`-`70`,
   `restart/skinny/tranches/sk-v15/SPEC.md:327`-`334`). V9 rejects header-only
   or fake generated status and routes Pattern H to W4 provenance rather than
   deletion/collapse (`restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md:59`,
   `restart/audit/totality/astral/V9/ΩE-skinny-corpus.md:306`-`308`).

7. Decision Engine and BackendShape closure stay grammar-neutral and five-shape.
   SK-V15 requires at least one e-graph rewrite, non-tautological CSP,
   grammar-neutral facts, and all five lowerers emitting real implementation
   paths (`restart/skinny/tranches/sk-v15/SPEC.md:71`-`73`). It forbids new or
   sixth `BackendShape`, retained sidecar, public `UnionTape`, alternate
   document projection, and production FNV arbiter
   (`restart/skinny/tranches/sk-v15/SPEC.md:147`-`153`). V9 MASTER carries this
   as W7-W9 and requires real lowerers for exactly
   `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`
   (`restart/audit/totality/astral/V9/master-plan-diff.md:90`-`94`,
   `restart/audit/totality/astral/V9/master-plan-diff.md:107`-`109`).

8. Lock 14/16 gates cannot self-exempt. SK-V15 requires gates to scan omitted
   leak roots, report every exclusion, and fail on self-exempting scans
   (`restart/skinny/tranches/sk-v15/SPEC.md:64`-`68`,
   `restart/skinny/tranches/sk-v15/SPEC.md:283`-`299`). The proposed Lock 14
   clause requires included roots, excluded roots, owner, reason, self-scan
   status, primitive status, gate consumer, affected rows, and disposition
   (`restart/audit/totality/astral/V9/locks-diff.md:67`). The Lock 16 clause
   requires owner, scalar oracle, strict parity/checkasm, Apple M5 Max/aarch64
   hardware gate or fallback, same-wave consumer, row movement target, status,
   rollback, and final disposition (`restart/audit/totality/astral/V9/locks-diff.md:71`).

9. FNV remains quarantined. SK-V15 requires W11L/W11N/W11O FNV closed-enum
   products to remain bench-only and the strict-product comparator to catch
   closed-enum sidecar coupling (`restart/skinny/tranches/sk-v15/SPEC.md:74`-`75`).
   W10 owns quarantine, production-root scans, and adversarial semantic fixtures,
   while FNV cannot be a runtime selector, production arbiter, or correctness
   proof (`restart/skinny/tranches/sk-v15/SPEC.md:430`-`445`). V9 MASTER carries
   that as W10 quarantine (`restart/audit/totality/astral/V9/master-plan-diff.md:93`-`94`,
   `restart/audit/totality/astral/V9/master-plan-diff.md:110`).

10. T-P3 final hardening already accepted this CH2 axis. T-P3 V5 records CH2
    `ACCEPT`: no JSON narrowing, forbidden surface addition, retained sidecar,
    public substrate API, sixth `BackendShape`, new directive, new BIR variant,
    or new substrate enters the packet, and CSS plus Sheets/BBNF-self receivers
    remain concrete (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:35`-`37`).

## Required Folds

None for CH2.

Carry the packet forward as-is, with these invariants preserved during any
post-G-Omega CRUD:

- Do not weaken the SK-V15 non-JSON receiver matrix in MASTER, LOCKS, or skinny
  corpus updates.
- Do not turn JSON 51/51 guard evidence into CSS, Sheets, BBNF-self, or fleet
  generality proof.
- Do not retire CSS old proof before the W5 typed provider and W6 same-workload
  `cssparser` retime gates.
- Do not delete or collapse Pattern H by header-only provenance or undocumented
  runtime removal.
- Do not admit Decision Engine, lowerers, primitives, or FNV without their
  gate-consumed SK-V15 evidence.
