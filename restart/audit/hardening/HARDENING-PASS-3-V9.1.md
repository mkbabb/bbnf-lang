# HARDENING-PASS-3-V9.1 - PASS-3 amendment verification

## 1. Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-3-runtime/PASS-3.md` |
| Verification cycle | V9.1 |
| Target lines audited | 591 |
| Verification time | 2026-05-07 18:16:41 EDT |
| Inputs read | `restart/prompts/HARDENING.md`; `restart/audit/hardening/HARDENING-PASS-3-V9.md`; `restart/audit/hardening/HARDENING-CONSOLIDATED-V9.md`; `restart/audit/pass-3-runtime/PASS-3.md`; `restart/ARCHITECTURE.md` section 7.4 |
| Scope boundary | Verification report only; no target-document edits |
| Final decision | **AMENDMENT-REQUIRED-NARROW** |

PASS-3 absorbed the substantive V9 amendment set. The old numeric diagnostic
aliases are gone, closure broadening now routes through Lock 1, stale retired
prompt citations and `README.md:473` are gone, feeder-table cardinality is
correct, and exact `pointer!` macro mentions are retirement prose only. Two
single-phrase residues remain in live PASS-3 prose: `BBNF-VISIT*` at
`restart/audit/pass-3-runtime/PASS-3.md:148`, and `pointer/select` at
`restart/audit/pass-3-runtime/PASS-3.md:448`.

## 2. V9 punch-list closure

| V9 item | Live PASS-3 evidence | Scan evidence | Verdict |
|---|---|---|---|
| Diagnostic ledger uses ARCH mnemonic codes; no old numeric aliases in live PASS-3. | Section 6b rows use mnemonic identifiers at `restart/audit/pass-3-runtime/PASS-3.md:452-472`; ARCH canon is mnemonic at `restart/ARCHITECTURE.md:1027-1085`. | `rg -n "BBNF-(LIFE\|LAYOUT\|OPT\|GRAMMAR\|PATH\|VISIT\|RECOVERY\|TYPE\|HOST\|GEN\|CG\|SEM)[0-9]{3}\|BBNF-SEM[0-9]+" restart/audit/pass-3-runtime/PASS-3.md` returned zero. | **Closed with narrow residue:** no numeric aliases remain, but `restart/audit/pass-3-runtime/PASS-3.md:148` still says `BBNF-VISIT*`; ARCH has `BBNF-VISITOR-*`, not that wildcard. |
| Closure broadening says Lock 1 amendment surface. | `restart/audit/pass-3-runtime/PASS-3.md:191` says broadening beyond the four closure sites defers to a Lock 1 reuse-map amendment and that the broadening contract is a Lock 1 amendment surface. | `rg -n "V2 amendment surface\|Lock 1 amendment surface"` found only the Lock 1 wording in PASS-3. | **Closed.** |
| Dead retired prompt citations and out-of-bounds `README.md:473` are gone. | `restart/audit/pass-3-runtime/PASS-3.md:14` marks retired dispatch prompts as archaeology and points current hardening at the five-prompt orchestrator suite; `restart/audit/pass-3-runtime/PASS-3.md:23` names `restart/prompts/ORCHESTRATOR.md`. | `rg -n "restart/prompts/(PASS-1\|PASS-2\|PASS-3\|SYNTHESIS)\|README\\.md:473" restart/audit/pass-3-runtime/PASS-3.md` returned zero. | **Closed.** |
| Feeder-table cardinality says 10 rows across 8 columns, not 10x9. | `restart/audit/pass-3-runtime/PASS-3.md:429` says "10 grammar rows across 8 PASS-3 columns"; the table spans `restart/audit/pass-3-runtime/PASS-3.md:431-442`. | `rg -n "10x9\|10 rows\|8 columns" restart/audit/pass-3-runtime/PASS-3.md` found the corrected wording and no `10x9`. | **Closed.** |
| `pointer!` mentions are retirement/deletion archaeology, not live macro surface. | Exact `pointer!` hits are `restart/audit/pass-3-runtime/PASS-3.md:16` and `restart/audit/pass-3-runtime/PASS-3.md:92`; both say the spelling retires. Live macro surface is `path!` + `select!` at `restart/audit/pass-3-runtime/PASS-3.md:91-92`. | `rg -n "pointer!\|path!\|select!" restart/audit/pass-3-runtime/PASS-3.md` confirms the exact macro token appears only in retirement clauses. | **Closed for `pointer!`; narrow residue:** `restart/audit/pass-3-runtime/PASS-3.md:448` still says `pointer/select` as a live diagnostic surface label and should say `path/select`. |

## 3. Targeted scan ledger

| Scan | Result | Classification |
|---|---|---|
| `rg -n "BBNF-(LIFE\|LAYOUT\|OPT\|GRAMMAR\|PATH\|VISIT\|RECOVERY\|TYPE\|HOST\|GEN\|CG\|SEM)[0-9]{3}\|BBNF-SEM[0-9]+" restart/audit/pass-3-runtime/PASS-3.md` | Zero hits. | V9 numeric-alias retirement is closed. |
| `rg -n "BBNF-[A-Z0-9*]+" restart/audit/pass-3-runtime/PASS-3.md` | Mnemonic code hits in path examples, recovery prose, and section 6b; one non-canonical visitor wildcard at line 148. | `BBNF-RECOVERY*` is ratified by ARCH at `restart/ARCHITECTURE.md:1074`; `BBNF-VISIT*` is not ratified and is residue. |
| `rg -n "BBNF-VISIT\\*\|BBNF-VISITOR-\|BBNF-RECOVERY\\*" restart/audit/pass-3-runtime/PASS-3.md restart/ARCHITECTURE.md` | ARCH lists `BBNF-VISITOR-*` rows at `restart/ARCHITECTURE.md:1055-1057`; PASS-3 section 6b matches them at `restart/audit/pass-3-runtime/PASS-3.md:462-464`; PASS-3 prose has `BBNF-VISIT*` at line 148. | Section 6b is correct; surrounding prose still needs a one-token mnemonic-canon cleanup. |
| `rg -n "restart/prompts/(PASS-1\|PASS-2\|PASS-3\|SYNTHESIS)\|README\\.md:473\|10x9\|V2 amendment surface" restart/audit/pass-3-runtime/PASS-3.md` | Zero hits. | Dead citation, old cardinality, and stale closure phrasing are gone. |
| `rg -n "pointer\|Pointer" restart/audit/pass-3-runtime/PASS-3.md restart/ARCHITECTURE.md` | PASS-3 exact macro hits are retirement-only at lines 16 and 92; PASS-3 has `pointer/select` at line 448. | Macro surface is clean; diagnostic-surface prose still has stale pointer wording. |
| `rg -ni "json\|css_l4\|bbnf\|google_sheets\|sheets\|css_pretty\|bnf\|csv\|ebnf\|math" restart/audit/pass-3-runtime/PASS-3.md` | Hits are examples, fixture paths, per-grammar table rows, SOTA datasets, and budget anchors. | Ratified: no grammar-specific branch logic surfaced. |
| `rg -nP "match\\s+\\w+\\s*\\{[^}]*Json\\s*=>\|CssL4\\s*=>\|Bbnf\\w*\\s*=>\|GoogleSheets\\w*\\s*=>" restart/audit/pass-3-runtime/PASS-3.md` | Zero hits. | Lock 14 generic-crate match-arm fault is absent from PASS-3. |

## 4. V9+ lens matrix A-K

| Lens | Verdict | Evidence |
|---|---|---|
| A - Lock adherence | **Narrow residue** | Lock 1 closure wording is fixed at `restart/audit/pass-3-runtime/PASS-3.md:191`; Lock 14 onboarding remains two surfaces at `restart/audit/pass-3-runtime/PASS-3.md:420-425`; exact `pointer!` retirement holds at lines 16 and 92. The stale `pointer/select` label at line 448 violates naming-canon polish, not architecture. |
| B - Sequencing / carry | **Pass** | Receiver/blocker/receiving-gate rows remain explicit at `restart/audit/pass-3-runtime/PASS-3.md:523-538` and `restart/audit/pass-3-runtime/PASS-3.md:574-587`. |
| C - Cohesion | **Narrow residue** | Section 6b coheres with ARCH section 7.4, but prose at `restart/audit/pass-3-runtime/PASS-3.md:148` points to a non-canonical `BBNF-VISIT*` family. |
| D - SOTA anchoring | **Pass** | Competitor, dataset, platform, and surface columns remain explicit at `restart/audit/pass-3-runtime/PASS-3.md:482-506`; non-throughput rows disclaim Lock 8 peer claims. |
| E - Grammar-authoritative discipline | **Pass** | YAML onboarding is exactly `yaml.bbnf` plus metadata at `restart/audit/pass-3-runtime/PASS-3.md:420-423`; per-grammar table rows are feeder cells at lines 431-442; match-arm scan returned zero. |
| F - LLM-bias diagnostic cleanup | **Narrow residue** | Numeric aliases retired cleanly, satisfying the Phase 8.4 simplification fold; `BBNF-VISIT*` is still a trained-distribution-shaped wildcard and should collapse to `BBNF-VISITOR-*` or plain "visitor diagnostics." |
| G - Overfitting | **Pass** | Grammar-name hits are worked examples, fixtures, per-X table rows, or budget anchors; no generic-crate `match` arms surfaced. |
| H - Hallucination / provenance | **Pass** | Retired prompt-file citations and `README.md:473` are absent from live PASS-3; current prompt authority routes to `restart/prompts/ORCHESTRATOR.md` at `restart/audit/pass-3-runtime/PASS-3.md:23`. |
| I - Contrivance / over-engineering | **Pass** | The old diagnostic double-namespace is gone from the ledger; host-rendered diagnostics use `thiserror` / `miette` rather than an invented diagnostic runtime at `restart/audit/pass-3-runtime/PASS-3.md:474`. |
| J - Host-language leverage | **Pass** | Runtime diagnostics delegate rendering to Rust error tooling at line 474; language-server protocol scaffolding delegates to `tower-lsp` / `dap-types` at `restart/audit/pass-3-runtime/PASS-3.md:346`. |
| K - Meta-grammar boundary | **Pass** | V1 remains Rust-backed; TS/WASM routes remain V2 backend implementations at `restart/audit/pass-3-runtime/PASS-3.md:391-399`, `restart/audit/pass-3-runtime/PASS-3.md:478`, and `restart/audit/pass-3-runtime/PASS-3.md:529`. |

## 5. Remaining residues

| # | Site | Residue | Required narrow surgery |
|---:|---|---|---|
| V9.1-P3-1 | `restart/audit/pass-3-runtime/PASS-3.md:148` | Visitor prose says diagnostics carry `BBNF-VISIT*` codes, but ARCH section 7.4 canon lists `BBNF-VISITOR-NO-MATCHING-KINDS`, `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY`, and `BBNF-VISITOR-RECOVERY-SKIP`. | Replace `BBNF-VISIT*` with `BBNF-VISITOR-*` or with "the `BBNF-VISITOR-*` codes." |
| V9.1-P3-2 | `restart/audit/pass-3-runtime/PASS-3.md:448` | Section 6b says PASS-3 owns diagnostic strings for "runtime, pointer/select..." even though the canonical macro surface is `path!` + `select!`. | Replace `pointer/select` with `path/select`. |

No re-draft threshold is met. Both residues are vocabulary cleanup against
already-settled contracts; neither changes the runtime architecture, benchmark
gates, Lock 14 onboarding surface, closure-lifetime boundary, or carry map.

## 6. Final decision

**Decision: AMENDMENT-REQUIRED-NARROW.**

PASS-3's V9 amendment set is materially applied. The original V9 blockers are
closed in the load-bearing areas: section 6b uses mnemonic diagnostic codes,
old numeric aliases are absent, closure broadening now cites Lock 1, dead/oob
citations are gone, feeder cardinality is corrected, and exact `pointer!`
mentions are retirement-only. The remaining work is two one-line vocabulary
repairs: change `BBNF-VISIT*` to the ARCH-canon visitor mnemonic family, and
change the live `pointer/select` label to `path/select`.

Hereupon: dispatch a narrow PASS-3 wording amendment, then rerun the targeted
V9.1 scans above. After those two residues clear, PASS-3 can return READY.
