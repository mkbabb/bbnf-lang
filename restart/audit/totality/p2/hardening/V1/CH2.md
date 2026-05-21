---
challenge: CH2
pass: T-P2-research
cycle: V1
lens: generality / Lock 14
generated_at: 2026-05-21T09:05:00-04:00
verdict: REVISE
---

# T-P2 V1 CH2 — Generality / Lock 14

## Verdict

**REVISE.** The V1 dossiers correctly identify the main Lock 14 hazards:
JSON-shaped role mining, hardcoded provider registration, `JsonSink`,
generic `OffsetFlags` semantics, shared SIMD call sites with JSON byte policy,
and opaque regex strings. That is enough to avoid REJECT.

It is not enough to converge. Several grounded techniques are still framed as
generally transferable after only JSON or CSS evidence, and the cohort has not
yet folded the found leaks into one mandatory cross-dossier contract for
CSS L4, Sheets, and BBNF-self. V2 must make grammar-neutral transfer a
mechanical gate, not a set of parallel recommendations.

## Evidence

| dossier | CH2 finding | disposition |
|---|---|---|
| 2A SOTA landscape | 2A correctly narrows SOTA transfer to generated grammar data or policy traits and warns that CSS declaration-values is evidence, not universal proof (`2A-sota-landscape.md:77`, `:97`). It still says On-Demand transfer can be proved by "one CSS or JSON row consumer" (`:83`), which is insufficient for a fleet-wide totality claim. | Fold to require a CSS L4 row plus a Sheets or BBNF-self negative-control/witness before claiming grammar-neutral transfer for direct/typed shape selection. |
| 2B primitive vocabulary | 2B keeps Layer 1 at the byte/mask/carry level and flags JSON byte policy in shared SIMD dispatch (`2B-primitive-vocabulary.md:202`, `:238`). That is the right boundary. The revise is that it remains a proposed `G-SIMD-GRAMMAR-POLICY`, not a V1 pass-level blocker. | Promote to blocker: any shared `bbnf-simd` consumer used by CSS/Sheets/BBNF-self must receive quote, escape, control, delimiter, number, and string policy from generated grammar config or generated grammar-local code. |
| 2C grammar neutrality | 2C is the strongest CH2 dossier. It explicitly refutes JSON role mining for Sheets and BBNF-self (`2C-grammar-neutrality.md:55-56`), hardcoded `RuntimeProvider` (`:59`, `:82`), `JsonSink` as generic sink (`:60`, `:80`), and generic `OffsetFlags` semantics (`:61`, `:81`). | Accept the findings, but V2 must convert them into a single Lock 14 amendment set with acceptance tests and owner surfaces. |
| 2D cost model | 2D correctly requires backend-shape rewrites and guards to consume generated grammar metadata, not JSON role mining (`2D-cost-model.md:92`). Its `SinkOnly`, Mison, and e-graph transfer discussion remains mostly JSON/direct-plane grounded (`:46`, `:60`). | Add a per-technique grammar-transfer table for CSS L4, Sheets, and BBNF-self, and mark any unresolved entries grammar-family-specific rather than fleet-wide. |
| 2E host-arch esoterica | 2E keeps aarch64 techniques abstract at the primitive layer and refutes `svmatch_u8` as a NEON/M5 Max route (`2E-host-arch-esoterica.md:25`, `:61`, `:115`). It correctly states TBL remains neutral only with caller-provided alphabets (`:52`). | Fold the caller-provided alphabet rule into the shared Lock 14 primitive manifest. Do not let "ASCII/CSS run-skip" become a CSS-special branch in generic SIMD code. |
| 2F parse-that gaps | 2F correctly says skinny `parse-that-regex` is JSON-shaped and that generic parse-that APIs must expose grammar-neutral HIR, byte classes, scanner plans, and number/string policy structs (`2F-parse-that-gaps.md:68`, `:71`, `:77`, `:134`). | V2 must make the extraction/import boundary explicit: grammar-named helpers may live only in generated grammar modules or grammar-local facades, never in shared parse-that / bbnf-regex APIs. |

## Blockers / Fold Requirements

1. **Create one canonical Lock 14 transfer contract.** V2 must consolidate the
   overlapping 2A/2B/2C/2D/2F amendment candidates into a single mandatory
   contract covering:
   - generated provider registry, replacing hand-coded `RuntimeProvider`;
   - grammar-shape leak scanner, not just grammar-name scanner;
   - generated sink/fact/value/flag interpretation surface;
   - primitive policy manifest for byte alphabets, delimiters, quotes, escapes,
     controls, number policy, string policy, and no-string/no-number policy;
   - CSS L4 plus Sheets or BBNF-self transfer/witness requirement for any
     fleet-wide grammar-neutral claim.

2. **Do not count one CSS row as total grammar generality.** The dossiers do
   say this in places, especially 2A and 2C, but V2 must make it a hard
   disposition rule: SK-V12 CSS declaration-values is valid non-JSON evidence,
   not proof that CSS selectors, CSS values/functions, Sheets formulas, or
   BBNF-self role facts are covered.

3. **Demote JSON/CSS-only technique claims unless they carry a transfer table.**
   For every grounded technique used fleet-wide, V2 needs a compact table:
   `technique | CSS L4 transfer | Sheets transfer | BBNF-self transfer |
   required generated facts | failure mode if absent`. Techniques without a
   defensible entry must be labelled grammar-family-specific, not
   grammar-neutral.

4. **Make `RuntimeProvider`, `JsonSink`, and `OffsetFlags` closure criteria,
   not open questions.** 2C already proves these are live generic-crate leaks.
   V2 may keep implementation design open, but the pass disposition must say
   that T-P3 cannot claim Lock 14 closure while those surfaces remain
   hand-coded or JSON-semantics-bearing in generic crates.

5. **Fence shared SIMD and parse-that APIs.** V2 must state that shared
   `bbnf-simd`, `parse-that`, and future `bbnf-regex` APIs expose only
   grammar-neutral facts and byte operations. JSON/CSS helper names and policies
   are legal only in generated grammar modules or grammar-local facades.

6. **Tie the decision-engine fold to generated grammar facts.** The e-graph,
   CSP, and cost-model route is acceptable only if rewrite guards and cost
   facts consume generated grammar metadata. A resolver that still mines
   JSON object/array/pair/string/number roles from generic code is a Lock 14
   failure even if it passes JSON equality.

## Disposition

No dossier is rejected on CH2: V1 contains real grammar-neutrality evidence and
names the important leaks. The cycle still fails CH2 convergence because the
evidence is not yet integrated into a single enforceable Lock 14 gate and
because some techniques remain over-broad from JSON/CSS evidence alone.

Required V2 fold: update the six dossiers, or a consolidated V2 addendum they
all cite, so that the canonical transfer contract above is binding before
T-P3 synthesis.
