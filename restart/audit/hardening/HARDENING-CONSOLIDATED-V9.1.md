# HARDENING-CONSOLIDATED-V9.1 — Verification Pass

V9.1 verifies the V9 amendment set against the amended corpus. Four verifier
reports returned AMENDMENT-REQUIRED-NARROW, all with wording or line-citation
residue rather than architecture faults. This consolidation applies the narrow
fixes named by the reports and records the post-amendment scan result.

## §1 Target identifications

| Target | V9.1 report | Report verdict | Residue class |
|---|---|---|---|
| PASS-1 | `restart/audit/hardening/HARDENING-PASS-1-V9.1.md` | AMENDMENT-REQUIRED-NARROW | one retired-prompt allusion |
| PASS-2 | `restart/audit/hardening/HARDENING-PASS-2-V9.1.md` | AMENDMENT-REQUIRED-NARROW | BIR spelling + stale line anchors |
| PASS-3 | `restart/audit/hardening/HARDENING-PASS-3-V9.1.md` | AMENDMENT-REQUIRED-NARROW | visitor diagnostic wildcard + path/select naming |
| MASTER-PLAN trio | `restart/audit/hardening/HARDENING-MASTER-PLAN-V9.1.md` | AMENDMENT-REQUIRED-NARROW | README support wording around WASM scope |

| Cohort state | READY | AMENDMENT-REQUIRED-NARROW | RE-DRAFT |
|---|---:|---:|---:|
| Report verdicts before consolidation amendments | 0 of 4 | 4 of 4 | 0 of 4 |
| Live corpus after consolidation amendments | 4 of 4 by targeted scan | 0 active blockers | 0 |

## §2 Verification synthesis

The V9 architectural fixes hold. The V1/V2 backend boundary is coherent, BIR is
20 rows (19 semantic variants plus `Return`), retired dispatch prompts are no
longer live authority, PASS-3 diagnostics use mnemonic codes, ARCH keeps
`RuleDecl` outside `Directive`, Grammar IR carries `Call { kind: Map | Host }`,
and HANDOFF/ORCHESTRATOR point to V9.1 instead of stale Phase 8 dispatch.

The V9.1 residues were mechanical:

| Residue | Source report | Applied fix |
|---|---|---|
| PASS-1 still said a stale PASS prompt asked about rewrite-mode / Unicode algebra. | PASS-1 V9.1 §5. | Reworded PASS-1 §6 to state the canonical surface directly: rewrite-mode is deletion archaeology; Unicode algebra is regex-layer. |
| PASS-2 used stale `RegexProgram` ARCH line anchors and stale ARCH §8.4 closure inventory lines. | PASS-2 V9.1 §4. | Updated `RegexProgram` anchors to ARCH lines 919 / 950 and closure inventory anchor to ARCH lines 1357-1362. |
| PASS-2 used absolute self-line references for gates that drifted. | PASS-2 V9.1 §4. | Replaced line-number prose with section references: §6 generic monomorphisation budget, §5 cost-model handoff, and §4 codegen close gate. |
| PASS-2 still named `ErrorRecovery` and live `Layout` as BIR constructs. | PASS-2 V9.1 §4. | Replaced live spellings with `ErrorRecover` and `LayoutScope`. Historical `Layout` collapse remains only inside Phase-8.4 archaeology. |
| PASS-3 still said `BBNF-VISIT*`. | PASS-3 V9.1 §5. | Replaced with `BBNF-VISITOR-*`. |
| PASS-3 and ARCH still said `pointer/select` in live runtime/diagnostic-surface prose. | PASS-3 V9.1 §5 plus consolidation scan. | Replaced with `path/select`. |
| README still blurred WASM into V1 scope. | MASTER-PLAN V9.1 §5. | Reworded generic-rule and backend-performance prose so V1 is Rust-line and V2 owns WASM / TS backend lowering. |

## §3 Lens disposition

| Lens | Consolidated V9.1 disposition |
|---|---|
| A / Lock adherence | READY after amendment. Lock 5 / 8 backend scope is Rust V1 plus V2 WASM/TS. |
| B / Sequencing | READY. V2 receivers remain named with blockers and gates. |
| C / Cohesion | READY after amendment. Stale prompt allusion and wrong line anchors were removed. |
| D / SOTA anchoring | READY. Throughput gates still cite competitor, dataset, platform, and target. |
| E / Grammar-authoritative | READY. Match-arm scans return zero; yaml onboarding remains source + metadata only. |
| F / LLM bias | READY after amendment. Numeric pseudo-code residues survive only as explicit deletion archaeology. |
| G / Overfitting | READY. Grammar names occur as examples, fixtures, or per-X rows, not generic logic. |
| H / Provenance | READY after amendment. The live wrong-line citations surfaced by PASS-2 V9.1 were corrected. |
| I / Contrivance | READY. V1 active backend cardinality is one; V2 backends are real receivers, not V1 apparatus. |
| J / Host-language leverage | READY. Closure semantics use semantic validation plus rustc final checking; V2 backend host differences are backend API choices. |
| K / Meta-grammar discipline | READY. BBNF remains a Rust V1 parser generator with V2 backend receivers; no grammar syntax broadens for WASM/TS. |

## §4 Post-amendment scans

The consolidation reran the targeted V9.1 residue scans after applying the
narrow edits.

Primary residue scan:

```sh
rg -n 'stale PASS prompt|PASS prompt|prompt asks|README\.md:473|WASM V1|WASM-V1|Rust \+ WASM|WASM via type erasure|23 variants|23-variant|10x9|BBNF-VISIT\*|pointer/select|ErrorRecovery|`Layout`|ARCHITECTURE\.md:921|ARCHITECTURE\.md:952|ARCHITECTURE\.md:1187-1207|line 417|line 232|line 391' ...
```

Result: one accepted archaeology hit remains in PASS-2 Phase-8.4 fold ledger:
historical `Layout` collapse inside the 23-vs-24 reconciliation row. No live
residue hit remains.

Diagnostic alias scan:

```sh
rg -n 'BBNF-LIFE001|BBNF-VISIT001|BBNF-PATH001|BBNF-LAYOUT001|BBNF-OPT001|BBNF-HOST001|BBNF-GEN001|BBNF-RECOVERY001' restart/audit/pass-3-runtime/PASS-3.md restart/README.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-1-substrate/PASS-1.md
```

Result: hits only in PASS-2 deletion-archaeology prose for retired code aliases;
no PASS-3 live numeric aliases.

Cross-corpus blocker scan:

```sh
rg -n 'WASM V1|WASM-V1|23 variants|23-variant|README\.md:473|PASS-1\.md:390-407|V8 SIMPLIFY|next move is Phase 8|dispatch Phase 8|10x9|22-row payload|about 22 executable' restart/README.md restart/HANDOFF.md restart/prompts/ORCHESTRATOR.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md restart/audit/pass-3-runtime/PASS-3.md restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md
```

Result: zero live-corpus matches.

`git diff --check` passed.

## §5 Final verdict

**READY after V9.1 narrow amendments.**

The verifier reports were correct to return AMENDMENT-REQUIRED-NARROW before the
consolidation edits. Those edits have now landed in the live corpus, and the
targeted post-amendment scans find no active V9.1 blocker. The remaining
diagnostic alias and BIR collapse hits are explicit deletion archaeology inside
Phase-8 fold ledgers, not live contract text.

Known non-blocking carry:

| Carry | Receiver | Gate |
|---|---|---|
| `BBNF-PATTERN-NONEXHAUSTIVE` remains a tranche-D friction-class diagnostic specialization. | Tranche D / diagnostic cookbook work. | Tranche D diagnostic catalogue and cookbook close. |

## §6 Closing posture

V9.1 verifies the V9 hardening amendments and closes the narrow residue surfaced
by the verifier cohort. Wave 9 per-tranche full-spec drafting may dispatch from
this corpus.
