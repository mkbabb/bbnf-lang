# HARDENING-PASS-1-V9.1 - PASS-1 V9 closure verification

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-1-substrate/PASS-1.md` |
| Verification cycle | V9.1 |
| Prior report | `restart/audit/hardening/HARDENING-PASS-1-V9.md` |
| Consolidation | `restart/audit/hardening/HARDENING-CONSOLIDATED-V9.md` |
| Live target lines | 360 (`wc -l restart/audit/pass-1-substrate/PASS-1.md`) |
| Architecture lines consulted | `restart/ARCHITECTURE.md:900-938`, `:1088-1153`, `:1163-1234`, `:1666` |
| Git commit inspected | `af3d1a73` |
| Verification scope | Full V9+ A-K lens pass, focused on V9 punch-list closure |

## §2 V9 punch-list closure

| V9 item | Live evidence | Verification verdict |
|---|---|---|
| V9-P1: remove active WASM V1 obligations | PASS-1 says Rust V1 is active and WASM/TS defer to V2 `WasmBackend: Backend` / `TsBackend: Backend` (`restart/audit/pass-1-substrate/PASS-1.md:61-71`). The per-X table says `RustBackend` V1 and `WasmBackend` / `TsBackend` V2 (`restart/audit/pass-1-substrate/PASS-1.md:290`). ARCH §7.5 says V1 ships `RustBackend` only and V2 adds WASM/TS (`restart/ARCHITECTURE.md:1090-1097`). | **CLOSED** |
| V9-P2: remove retired prompt citations and out-of-bounds README residues | Direct retired prompt path citations and `restart/README.md:473` residues are absent from live PASS-1. PASS-1 line 16 now cites agent-5 archaeology plus live README rejection (`restart/audit/pass-1-substrate/PASS-1.md:16`). PASS-1 line 328 now cites live hardening discipline (`restart/audit/pass-1-substrate/PASS-1.md:328`). | **PARTIAL**: one uncited retired-prompt allusion remains at `restart/audit/pass-1-substrate/PASS-1.md:212`. |
| V9-P3: closure-capture diagnostic layer | PASS-1 now says closure environment mode is validated before emission and `BBNF-CLOSURE-CAPTURE-BY-MOVE` fires if a non-borrow mode reaches layout lowering (`restart/audit/pass-1-substrate/PASS-1.md:87`). §6 repeats `passes::layout` / closure-environment validation before Rust source emission and names rustc as final correctness gate (`restart/audit/pass-1-substrate/PASS-1.md:263`). | **CLOSED** |
| V9-BIR: 19 semantic + PASS-2 `Return`, ARCH §7.2 authority | PASS-1 says ARCH §7.2 owns the authoritative BIR set, lists 19 semantic variants, and says PASS-2's `Return` completes the 20-row lowerer-facing alphabet (`restart/audit/pass-1-substrate/PASS-1.md:41`). ARCH §7.2 lists the 20 rows and states the net shape is 19 semantic variants plus `Return` (`restart/ARCHITECTURE.md:900-938`). | **CLOSED** |
| V9-X1: ARCH RuleDecl directive drift | ARCH §8.1 now has `Grammar ::= (Directive | RuleDecl)*`, while `Directive` contains only the six directive forms (`restart/ARCHITECTURE.md:1166-1167`). ARCH text explicitly says `RuleDecl` is a grammar member, not a directive (`restart/ARCHITECTURE.md:1219-1220`), and §13.1 enforces the six-directive canon (`restart/ARCHITECTURE.md:1666`). | **CLOSED** |

## §3 Targeted `rg` scans

| Scan | Result | Classification |
|---|---|---|
| `rg -n 'WASM V1|Wasm V1|TS V1|Rust/WASM parity|WASM V1 lowering obligation|parse-time|rustc rejects|PASS-1-SUBSTRATE|PASS-2-CODEGEN|PASS-3-RUNTIME|restart/README\.md:473|about 22|about-22' restart/audit/pass-1-substrate/PASS-1.md` | Zero matches. | Confirms the exact stale V9 blocker strings are gone from live PASS-1. |
| `rg -n 'WASM and TS defer|V2 backend|RustBackend V1|WasmBackend|TsBackend' restart/audit/pass-1-substrate/PASS-1.md restart/ARCHITECTURE.md` | PASS-1 lines 61, 63-71, 208, 290; ARCH lines 733, 1094-1097, 1122-1133, 1147-1153. | Ratified V1 Rust / V2 WASM+TS wording. |
| `rg -n 'stale PASS prompt|PASS prompt|stale prompt|retired prompt|prompt asks' restart/audit/pass-1-substrate/PASS-1.md` | PASS-1 line 212 remains: "stale PASS prompt asks about them." | **Residue**. Not a path citation, but still a live retired-prompt provenance allusion. |
| `rg -ni 'closure|capture-by-move|parse-time|parse error|rustc rejects|rustc remains|passes::layout|BBNF-CLOSURE-CAPTURE-BY-MOVE' restart/audit/pass-1-substrate/PASS-1.md` | Closure hits at lines 87, 131, 263, 350, 357 use semantic validation / rustc-final-gate wording. The only `parse error` hit is line 259 for bodyless `@host fn`, not capture. | Ratified. |
| `rg -n 'about-?22|about 22|about-22|prior 22|22-variant|19 semantic|PASS-2.+Return|20-row|20-variant|ARCH §7\.2' restart/audit/pass-1-substrate/PASS-1.md restart/ARCHITECTURE.md` | No `about 22` / `about-22`. Historical `prior/original 22` appears at PASS-1 line 41 and ARCH lines 904-936 only as fold archaeology beside the current 19+`Return` statement. | Ratified archaeology, not active stale count. |
| `rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math' restart/audit/pass-1-substrate/PASS-1.md` | Matches are fixture paths, README/corpus citations, per-X table cells, diagnostic examples, or Lock 14 onboarding proof. | Ratified; no generic-crate logic hardcodes grammar names. |
| `rg -nP 'match\s+\w+\s*\{[^}]*((Json|CssL4|Bbnf\w*|GoogleSheets\w*)\s*=>)' restart/audit/pass-1-substrate/PASS-1.md restart/ARCHITECTURE.md` | Zero matches. | Confirms no proposed grammar-dispatch match arms. |
| `rg -n 'Directive\s*[:=]+.*RuleDecl|Directive\s*=.*Rule|RuleDecl.*directive|six-directive|directive-canon|Directive      =|Directive     ::=' restart/audit/pass-1-substrate/PASS-1.md restart/ARCHITECTURE.md` | PASS-1 line 216 and ARCH line 1167 show six-directive production; ARCH line 1220 states `RuleDecl` is not a directive; ARCH line 1666 names the directive-canon lint. | Ratified. |

## §4 A-K lens disposition

| Lens set | V9.1 result |
|---|---|
| A / Lock adherence | V1 backend scope now honours Lock 5: Rust active, WASM/TS V2. |
| B / Sequencing | No active V1 backend obligation is assigned to a V2 receiver; V2 backend parity is explicitly carried at PASS-1 line 208. |
| C / Cohesion | One cohesion fault remains: line 212 names a stale PASS prompt without citation or live authority. |
| D / SOTA anchoring | PASS-1 does not add a throughput gate; Rust V1/V2 backend split aligns with ARCH §7.5. |
| E / Grammar-authoritative | No grammar-name match arms; yaml onboarding remains two author inputs (`restart/audit/pass-1-substrate/PASS-1.md:275-283`). |
| F / LLM bias | No new V9 blocker; wording is mostly committed rather than hedged. |
| G / Overfitting | Grammar-name mentions are audit anchors or examples, not generic-crate plan logic. |
| H / Hallucination + provenance | Direct stale citations are gone; the line-212 stale-prompt allusion remains a provenance residue. |
| I / Contrivance | V1 WASM obligation column is gone; deferred backend column is V2-only. |
| J / Host-language leverage | Closure capture now delegates final Rust lifetime correctness to rustc and uses bbnf validation for grammar-author diagnostics. |
| K / Meta-grammar discipline | PASS-1 keeps backend-neutral substrate plus Rust V1 realization; WASM/TS bodies are V2 Backend impls. |

## §5 Remaining residue

| Residue | Site | Why it matters | Narrow surgery |
|---|---|---|---|
| Retired-prompt allusion remains in live PASS-1 prose. | `restart/audit/pass-1-substrate/PASS-1.md:212` | V9-P2 removed retired prompt citations, but the live target still leans on an uncited "stale PASS prompt" claim. It is not an active path citation, but it keeps retired prompt pressure in the current target surface. | Replace the sentence with: "The canonical grammar surface excludes rewrite-mode and grammar-level Unicode algebra. Unicode algebra is a regex-layer term." If provenance is required, cite line 16's agent-5 archaeology and README rejection instead. |

No other V9-PASS-1 residues remain. The historical 22-variant references are acceptable fold archaeology because both PASS-1 and ARCH state the current BIR contract as 19 semantic variants plus PASS-2 `Return`, with ARCH §7.2 authoritative.

## §6 Final decision

**Decision: AMENDMENT-REQUIRED-NARROW.**

PASS-1 is substantively ready on the V9 items: active WASM V1 obligations are gone, closure-capture wording now sits at semantic validation with rustc as final gate, BIR cardinality is coherent with ARCH §7.2, and ARCH RuleDecl directive drift is closed. The only remaining V9.1 residue is a single uncited retired-prompt allusion at PASS-1 line 212. Remove or re-anchor that sentence; no re-draft is warranted.
