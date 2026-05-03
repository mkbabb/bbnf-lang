# Hardening Plan Audit 08 — Carry & Deferral

Date: 2026-05-03
Standard: every deferral names a real receiving tranche, a concrete blocker, and a receiving gate. `docs/tranches/BD/BD.md` is absent.

## Carry Faults

| ID | Site | Fault | Surgery | Verdict |
|---|---|---|---|---|
| D08-1 | `docs/tranches/BA/BA.md:59`, `docs/tranches/BB/BB.md:45` | BA says BA->BB.C1 lands in BB.W0/W1; BB says specialised grammars land in BB.W1 and cohort in BB.W2. BB.W0 is sister-crate emigration at `docs/tranches/BB/BB.md:31`. | Edit BA.C1 to "BB.W1 extends to CSS L4, BBNF, Sheets; BB.W2 extends to the five-grammar cohort." Remove BB.W0. | dangling receiver |
| D08-2 | `docs/tranches/BA/BA.md:65-70` | BA->BC carries omit receiving wave columns; BC later supplies BC.W0/BC.W2 at `docs/tranches/BC/BC.md:42-46`. | Add `Receiving wave` column to BA's BC carry table. | silent-must-add |
| D08-3 | `docs/tranches/BA/waves/W0.md:142` | Fleet-wide worktree fixture carry routes to BC.W2, but BC.W2 is TS/WASM scaffold work at `docs/tranches/BC/BC.md:32`. Worktree fixture closure is BC.W5 at `docs/tranches/BC/BC.md:35`. | Change receiver to BC.W5, or add fixture materialisation to BC.W2. | dangling receiver |
| D08-4 | `docs/tranches/BA/waves/W3.md:133` | `bbnf-regex` endpoint reconciliation routes to BC.W4, but BC.W4 is visitor formalisation at `docs/tranches/BC/BC.md:34`; reconciliation is BC.W5 at `docs/tranches/BC/BC.md:35`. | Replace BC.W4 with BC.W5. | dangling receiver |
| D08-5 | `docs/tranches/BA/waves/W6.md:168-169` | Sibling dirty-state baseline and `AscentStrategy` disposition are assigned to BC.W0, but BC.W0 has no such gates at `docs/tranches/BC/BC.md:30`. Ground truth requires these artefacts at `audit/HARDENING-SYNTHESIS-2026-05-03.md:224` and `audit/HARDENING-SYNTHESIS-2026-05-03.md:227`. | Add BC.W0 gates for `W0-sibling-baseline.txt` and `W0-ascent-strategy-disposition.md`. | missing receiving gate |
| D08-6 | `docs/tranches/BB/BB.md:158` | BB says BC.W5 consumes Visitor surface for parity tests; BC.W5 is sister-crate API freeze, not visitor. BC.W4 owns visitor formalisation at `docs/tranches/BC/BC.md:34` and BC carry table line `docs/tranches/BC/BC.md:53`. | Replace BC.W5 with BC.W4. | dangling receiver |
| D08-7 | `docs/tranches/BC/BC.md:56-62`, `docs/tranches/BC/BC.md:95` | BC carries TS/WASM activation, sister publication, and worktree infrastructure to BD, but BD is not drafted. This violates the receiving-gate rule. | Draft `docs/tranches/BD/BD.md` with BD.W0 gates for BC->BD.C1..C3, or cut the BD carry table and keep TS/WASM scaffolds out of BC. | fictional/unbacked receiver |
| D08-8 | `docs/tranches/BC/BC.md:60` | BC->BD.C1 says host-fn per-backend resolution is BD scope, but no blocker is stated beyond "BD activates them." | Add blocker: "requires TS/WASM ABI choice and host-fn resolution table design." Then add BD.W0 gate. | blocker absent |
| D08-9 | `docs/tranches/BC/BC.md:35` | BC.W5 says "user adjudicates at hardening time." This is not a carry and not an execution gate. | Choose the endpoint in BC.W5 plan text or move endpoint selection to a pre-BC hardening amendment. | non-executable deferral |
| D08-10 | `docs/tranches/BA/BA.md:81`, `docs/tranches/BA/BA.md:87-88` | BA defers L4/L10/L11, and W6 says the deferrals are ratifiable at `docs/tranches/BA/waves/W6.md:136`, but the BA carry table does not list Lock 4 or Lock 10 explicitly. | Add BA->BB carry rows for L4 and L10 with receiving BB.W3 gates BB-G10 and BB-G6. | incomplete carry ledger |

## Ratified Carries

| Carry | Site | Why it holds |
|---|---|---|
| BA->BB.C2 layout canon | `docs/tranches/BA/BA.md:60`, `docs/tranches/BB/BB.md:46` | Receiver states `Layout`/`LayoutSink` only. Needs L2 alias surgery, but carry route is real. |
| BA->BB.C3 cursor unification | `docs/tranches/BA/BA.md:61`, `docs/tranches/BB/BB.md:47` | Receiver BB.W2 is named and has a wave-table gate. |
| BA->BB.C4 path-core | `docs/tranches/BA/BA.md:62`, `docs/tranches/BB/BB.md:48` | Receiver BB.W5 is named and consumes `path-core`. |
| BA->BB.C5 grammar-agnostic IR | `docs/tranches/BA/BA.md:63`, `docs/tranches/BB/BB.md:49` | Receiver BB.W3 is named and gates no hardcoded grammar arms. |
| BB->BC.C1 optimiser output-pipe | `docs/tranches/BB/BB.md:57`, `docs/tranches/BC/BC.md:51` | Receiver BC.W0 is real. |
| BB->BC.C2 direct-to-struct contract | `docs/tranches/BB/BB.md:58`, `docs/tranches/BC/BC.md:52` | Receivers BC.W0/BC.W1 are real. |
| BB->BC.C3 visitor | `docs/tranches/BB/BB.md:59`, `docs/tranches/BC/BC.md:53` | Receiver BC.W4 is real. |
| BB->BC.C4 sister crates | `docs/tranches/BB/BB.md:60`, `docs/tranches/BC/BC.md:54` | Receiver BC.W5 is real, but parse-that must be added per Lane 1. |

## Lane Verdict

| Status | Count |
|---|---:|
| ratified | 8 |
| dangling receiver | 4 |
| missing receiving gate | 1 |
| unbacked receiver | 1 |
| blocker absent | 1 |
| non-executable deferral | 1 |
| incomplete carry ledger | 1 |
| silent-must-add | 1 |

Hereupon BD is the largest fault. A named letter without a drafted receiving gate is not a carry; it is a promise with no landing field.
