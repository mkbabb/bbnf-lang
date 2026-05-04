# Wave 2 Classification — PASS-1 amendment items 6, 7, 8, 20, 38, 45, 46

This file records the verify-then-patch classification for the seven PASS-1 amendment items dispatched in Wave 2 of the four-target hardening response. Source verdicts trace to `restart/audit/hardening/HARDENING-CONSOLIDATED.md` §4 and `restart/audit/hardening/HARDENING-PASS-1.md` §12. The amendment landed in the next commit on this branch.

## Classification table

| Item | Prior surgery directive | Current PASS-1.md state | Classification | Surgery to land |
|---|---|---|---|---|
| 6 — Block-bodied `@host fn` | Replace declaration-only `HostFn = ... ";"` with block-bodied production. | PASS-1.md:164 already shows `HostFn = "@host" "fn" Ident GenericParams? "(" Params? ")" "->" Type HostAttrs? Block ;`. The block-bodied form is in place; no `";"` declaration form remains. | verify-only-stub | Add an explicit acceptance line to §6 stating the declaration-only form is rejected and the block body owns host-fn semantics. |
| 7 — Lookbehind surface | Align grammar lookbehind with `\|<`, restate `(?<=...)` as regex-only, add finite-width legality + diagnostic. | PASS-1.md:177 has `Lookbehind = Expr "\|<" Expr \| Expr "\|<!" Expr`; PASS-1.md:192 declares `(?<=...)` regex-only; BBNF1004 (PASS-1.md:96) names the unbounded-width diagnostic. The diagnostic name `LookbehindWidth` already appears at PASS-1.md:90 in the error vocabulary, but the finite-width legality rule itself and the alphabetic alias `BBNF-LOOKBEHIND-WIDTH` are not yet committed in prose. | patch-delta | Add a finite-width legality clause alongside the lookbehind production and bind the alphabetic alias `BBNF-LOOKBEHIND-WIDTH` to BBNF1004. |
| 8 — Chain syntax + type flow | State canonical multi-function chain syntax + type-flow rule for `-> f1 -> f2`; fence method-chain form to host-fn body. | PASS-1.md:99 carries BBNF1401 chain-step type-failure diagnostic; PASS-1.md:105 sketches `a.f(x).g(y)` desugar; PASS-1.md:186-187 lists `MapTail = "->" ChainExpr` and `ChainExpr = Ident { "->" Ident }`. Type-flow rule and method-chain fence are absent from §6. | patch-delta | Add an explicit canonical chain rule near the formal grammar: `-> f1 -> f2` is the rule-level chain and threads the previous step's value type into the next; `a.f(x)` method form is permitted only inside `@host fn` bodies and carries the same chain-step diagnostic. |
| 20 — PASS-1 crate rationale | Add per-crate rationale and sibling API uniformity notes for PASS-1 child directories. | PASS-1.md:108-124 has the crate→children layout; PASS-1.md:126-133 has the sibling API uniformity floor by crate family. Per-crate rationale (why `ir/` carries `grammar_ir/` and `backend_ir/` siblings, why `passes/` separates `types/` from `layout/`, etc.) is missing. | patch-delta | Add a per-crate rationale table with the rationale for the children of each crate and the sibling-uniformity contract that crate's children follow. |
| 38 — Independent-proceed clause | Delete "PASS-2 and PASS-3 may proceed independently"; replace with reconcile-first clause. | PASS-1.md:251 already carries "SYNTHESIS must reconcile conflicting sister-pass outputs before any target is treated as settled." The verification grep for `independent\|proceed independently\|sister-pass` returns the replacement clause and no surviving independent-proceed text. | verify-only-stub | Confirm the §10 closing posture carries the reconcile-first clause; add a Wave-2 acceptance line explicitly retiring the independent-proceed wording. |
| 45 — Closure beta-reduction as research signal | Reframe current closure beta-reduction machinery as research signal only; require fresh greenfield spec + verification before reuse. | PASS-1.md:194 already carries "Current closure beta-reduction code is research signal only ...; greenfield reuse requires a fresh spec and verification gate." The verification grep returns the research-signal framing. | verify-only-stub | Confirm the §6 closure-semantics paragraph; add a Wave-2 acceptance line stating no legacy closure code is inherited by default and the greenfield spec gate is the sole reuse path. |
| 46 — OpenFrame deletion | Remove "useful backend-internal stack detail" claims; replace with generated BIR builder-frame design + TapeBuilder checkpoints. | PASS-1.md itself does not preserve OpenFrame as a useful generic substrate detail; the only PASS-1.md mention is the deletion-pathology entry at PASS-1.md:48 ("no OpenFrame clone stack; rollback is bounded"). The offending text lives in `restart/audit/pass-1-substrate/agent-6-substrate-coherence-auditor.md:17`, where the "Pro" cell still names OpenFrame "Useful as backend-internal stack detail." | patch-delta + sub-agent correction note | Add positive replacement text in PASS-1 §2: dispatch and speculation use generated Backend IR builder frames keyed by `RuleId`/`NodeId` plus `TapeBuilder` checkpoints; clone-stack OpenFrames have no role. Append a Wave-2 correction note to agent-6's report retiring the "useful" framing. |

## Routing summary

| Surgery class | Items |
|---|---|
| verify-only-stub | 6, 38, 45 |
| patch-delta | 7, 8, 20, 46 |
| sub-agent correction note | 46 (agent-6) |

## Acceptance gates carried into the amendment commit

- §6 BBNF formal specification: block-bodied `@host fn`, finite-width lookbehind legality, canonical chain syntax + type-flow + method-chain fence.
- §3 Per-crate `src/` tree: per-crate rationale rows complementing the existing children layout and sibling API uniformity floor.
- §2 Substrate Architectural Commitments: explicit BIR builder-frame + TapeBuilder replacement language for OpenFrame.
- §10 Closing Posture: Wave-2 acceptance line retiring independent-proceed wording and naming the closure beta-reduction reuse gate.
- `agent-6-substrate-coherence-auditor.md`: correction note retiring the "useful as backend-internal stack detail" line.

The amendment commit lands these surgeries verbatim against PASS-1.md and the named sub-agent report; this classification file is preserved as evidence that Wave 1.1 baseline was inspected before amendment.
