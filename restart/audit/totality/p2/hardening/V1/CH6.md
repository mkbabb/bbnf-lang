---
agent: CH6
pass: T-P2-research
cycle: V1
lens: ANTI-PAPER-CLOSE
disposition: REVISE
generated_at: 2026-05-21T09:12:00-04:00
inputs_audited:
  - restart/prompts/totality/PASS-2-RESEARCH.md
  - restart/audit/totality/p2/2A-sota-landscape.md
  - restart/audit/totality/p2/2B-primitive-vocabulary.md
  - restart/audit/totality/p2/2C-grammar-neutrality.md
  - restart/audit/totality/p2/2D-cost-model.md
  - restart/audit/totality/p2/2E-host-arch-esoterica.md
  - restart/audit/totality/p2/2F-parse-that-gaps.md
---

# T-P2 V1 CH6 Anti-Paper-Close

## Lens Contract

CH6 audits whether T-P2 V1 converts citations into bbnf-specific transfer
rules, or whether it closes on reference density. Per
`restart/prompts/totality/PASS-2-RESEARCH.md` Section 3, V1 expects revise
findings and an all-ACCEPT hardening wave is itself a paper-close risk. This
lens looks for citation-only validation, unsupported "grounded" wording, vague
future-pass deferrals, and missing row consumers.

## Verdict

REVISE. V1 is useful research and is not a citation dump: 2A, 2B, 2C, 2D, 2E,
and 2F all tie primary sources back to local REDRESS, RESULT, and source-code
constraints. However, accepting V1 as-is would let T-P3 inherit several
architecture directions as if they were validated designs. The revision is to
add concrete transfer ledgers and non-admission wording where the dossiers still
say "grounded", "mandatory", "production-real", or "supports the direction"
without enough bbnf-specific proof.

## Findings

| disposition | target | finding | required revision |
|---|---|---|---|
| ACCEPT | `restart/audit/totality/p2/2A-sota-landscape.md:20-36`, `:72-77` | 2A mostly resists paper close. It explicitly says SOTA sources do not justify replaying the retained structural union, treats simdjson/yyjson/asmjson as architecture pressure rather than admission gates, and requires scalar reference, parity, and same-wave row consumption. | None for 2A's comparator and SOTA framing. Preserve this language in V2. |
| ACCEPT | `restart/audit/totality/p2/2B-primitive-vocabulary.md:31-38`, `:163-171`, `:204-212` | 2B correctly refuses Lock 16 closure from local skeleton presence. It names opt-in strict parity, scalar delegates, support-only inventory, and the "primitive parity alone is not admission" rule. | None on the no-orphan and no-proof-only posture; see the cross-dossier terminology revision below. |
| REVISE | `restart/audit/totality/p2/2C-grammar-neutrality.md:52-63`, `:69-73`, `:87-94` | 2C has the right Lock 14 thesis, but the standards citations and future-grammar onboarding test are still too coarse for T-P3 to consume safely. The dossier grounds that CSS, Sheets, and BBNF-self falsify JSON-shaped policy, but it does not yet provide a feature-to-transfer ledger for the pinned CSS parity matrix or the negative-control grammars. Without that, T-P3 could paper-close full grammar generality from "CSS Syntax has these tokens" plus "generated metadata should exist." | Add a table mapping each non-OUT_OF_SCOPE CSS parity feature family, Sheets formula witness, and BBNF-self witness to generated facts, primitive families, code owners, equality oracle, row consumer, and telemetry field. Mark entries without a same-wave consumer as `NOT-VALIDATED` rather than "grounded". |
| REVISE | `restart/audit/totality/p2/2D-cost-model.md:20-33`, `:39-50`, `:57-60`, `:79-84` | 2D correctly refutes the P1-P8 cascade, but "the literature supports SK-V13's direction" is still direction-level validation. The open questions carry the missing work: stable `BackendExpr` vocabulary, cost axes, bounded saturation, CSP value, and lowerer coverage. Those cannot remain generic future checks if T-P3 is going to write a locks diff. | Add a bbnf-specific decision-engine transfer ledger: at least the intended rewrite families, their guards, local `BackendExpr`/BIR inputs, active cost axes, bounded-saturation caps, CSP feasibility constraints, and explicit "not validated until JSON/CSS equality plus no regression" wording. |
| REVISE | `restart/audit/totality/p2/2F-parse-that-gaps.md:20-35`, `:52-54`, `:66-69`, `:82-85`, `:123-124` | 2F's `bbnf-regex` conclusion may be right, but it currently depends on an unpinned local sibling worktree path and then calls extraction "mandatory." The file itself admits that the exact upstream revision, license/import boundary, and HIR-to-`BackendExpr` mapping are still unknown. That is a paper-close risk because T-P3 could treat local-path presence as architecture authority. | In V2, either pin the upstream parse-that revision/license/import route and map the required HIR/regex facts into current bbnf types, or downgrade the "mandatory extraction" claim to a conditional candidate with explicit blockers. Do not let an absolute local path serve as sufficient authority. |
| REVISE | `restart/audit/totality/p2/2B-primitive-vocabulary.md:143-159`, `:204-212`; `restart/audit/totality/p2/2E-host-arch-esoterica.md:145-146` | 2B and 2E both fight the zero-orphan paper-close, but they use slightly different closure states. 2B's manifest field allows `demoted_with_evidence` while 2E says historical demotion is not a permanent zero-orphan source state. If left unreconciled, T-P3 can accidentally preserve SK-V12's "inventory_demoted_with_evidence" as a close state under a stricter SK-V13 pin. | Normalize the state machine across 2B and 2E: `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`. Treat `inventory_demoted_with_evidence` as historical evidence only, not a close state. |
| REVISE | `restart/audit/totality/p2/2A-sota-landscape.md:72`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:53-55`, `restart/audit/totality/p2/2D-cost-model.md:50`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:137-148` | The union and ASM-gen reopen language repeatedly says a fresh route must name a material differential, but V1 does not yet define the checklist that distinguishes a real material differential from renamed replay. This is exactly where citation density can paper-close: "PMULL/CSSC exists" or "simdjson stages exist" can smuggle old routes back into scope. | Add a common material-differential checklist for T-P3: old REDRESS route cited, old cost source deleted or bypassed, new consumer shape named, scalar reference and checkasm cell named, micro-proof artifact required, strict row gate named, abort criteria stated, and no retained sidecar unless separately admitted. |
| REVISE | `restart/audit/totality/p2/2E-host-arch-esoterica.md:20-33`, `:54-65`, `:71-80`, `:90-97` | 2E is careful about instruction availability, but "TBL is already production-real" can be overread as Lock 16 closure. The same file says several rows are micro-proven, proof-only, background, or not selectable until a row-local fold exists. | In V2, reserve "production-real" for a primitive with an identified production caller and row consumer. Use "implemented local primitive" or "dispatchable local primitive" for TBL unless the row consumer and grammar-policy proof are named in the same row. |
| ACCEPT | `restart/audit/totality/p2/2A-sota-landscape.md:74-76`, `restart/audit/totality/p2/2F-parse-that-gaps.md:115-117` | The dossiers do not validate comparator substitution by source prestige. 2A rejects historical/n/a C++ sidecars as admission gates, and 2F preserves direct digest as a semantic-output contract rather than a generic hash substitution. | None. Preserve strict same-plane comparator wording in V2. |

## Fold Requirements For V2

1. Add a shared "citations are not admission" covenant to the dossiers or the
   consolidated verdict: a primary source grounds a candidate only; admission
   still requires bbnf scalar reference, checkasm/parity where applicable,
   same-wave row consumer, strict equality, and measured row movement or
   architectural-block evidence.
2. Fold the 2C feature-to-transfer ledger so Lock 14 cannot close from W3C/OASIS
   grammar citations alone.
3. Fold the 2D decision-engine transfer ledger so equality saturation, CSP, and
   cost extraction are not adopted as an abstract optimizer slogan.
4. Pin or downgrade 2F's upstream parse-that import claim before T-P3 treats
   `bbnf-regex` extraction as an unconditional architecture step.
5. Reconcile 2B/2E orphan terminology and define a single close-state enum for
   source-present SIMD/ASM primitives.
6. Add the common material-differential checklist for union and ASM-gen reopen
   routes.

## Disposition

REVISE, not REJECT. The V1 packet contains real refutations, local transfer
notes, and several anti-paper-close safeguards. The defects are convergence
blocking because they are exactly the seams where T-P3 could turn literature
grounding into implementation authority. V2 should be accepted only after the
six fold requirements above are explicitly reflected in the dossiers or in a
binding consolidated addendum.
