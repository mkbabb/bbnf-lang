# Hardening Plan Synthesis — BA/BB/BC Restart Plan

Date: 2026-05-03
Input lanes: `audit/HARDENING-PLAN-2026-05-03-01-lock-adherence.md` through `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md`.
Research anchors: `audit/SOTA-2026-05-03.md`, `audit/CENSUS-2026-05-03.md`, `audit/MODULES-2026-05-03.md`, `audit/RESTART-SKETCH-2026-05-03.md`, `docs/HARDENING-AUDIT-PROMPT.md`, `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md`, and `docs/tranches/AV/research/04-columnar-soa.md`.

## Cohort Verdict

| Lane | Verdict | Blocking faults |
|---|---|---:|
| 01 Lock-Adherence | violated | 9 |
| 02 Sequencing Discipline | violated | 5 |
| 03 Cohesion | violated | 18 |
| 04 SOTA Anchoring | violated | 12 |
| 05 Grammar-Authoritative | violated | 9 |
| 06 Generated-Code Budget | violated | 14 |
| 07 Friction Forecast | violated | 10 |
| 08 Carry & Deferral | violated | 13 |

Decision: **requires re-draft into more granular waves/tranches**, not ready to execute. The thesis survives; the plan surface still leaves too many semantic, ownership, carry, and benchmark decisions to ad hoc execution.

## Cross-Lane Faults

| Fault | Lanes | Primary evidence | Required surgery |
|---|---|---|---|
| BA preserves legacy `OpenFrame` for non-JSON | 01, 02, 06 | `docs/tranches/BA/waves/W5.md:35`, `docs/tranches/BA/waves/W5.md:103-104` | Move all-grammar `OpenFrame` retirement up from BB.W1 or stop claiming Lock 1 at BA close. |
| Layout canon still carries retired names | 01, 02, 03, 07 | `docs/tranches/BA/waves/W2.md:9-11`, `docs/tranches/BC/BC.md:141-152` | Delete `TypeDesc`/`StructLayout`/`LayoutDesc` aliases and update BC IR contract to `Layout`. |
| Path-crate plan still names stale `bbnf-path` and keeps core path runtime | 01, 03, 08 | `docs/tranches/BA/waves/W3.md:3`, `docs/tranches/BA/waves/W3.md:21`, `docs/tranches/BA/waves/W3.md:29-32` | Rewrite to `crates/path`, `crates/path-core`, `crates/path-ts`; move `crates/core/src/path/` runtime into `crates/path/`. |
| `parse_with` deletion precedes replacement | 01, 02, 03 | `docs/tranches/BA/waves/W3.md:62-67`, `docs/tranches/BA/waves/W3.md:148`, `docs/tranches/BA/waves/W4.md:34-39` | Move BA.W3.M5 to BA.W4.M1 or emit replacement before deletion. |
| SOTA claim overstates non-perf gates | 01, 04 | `docs/tranches/BA/BA.md:11`, `docs/tranches/BC/BC.md:11` | Replace "every gate" with "every parse-throughput gate"; split toolchain/LOC gates into engineering tables. |
| CSS SOTA surface is not parse-only ratified | 04 | `docs/tranches/BB/BB.md:15-16`, `docs/tranches/BC/BC.md:16`, `audit/SOTA-2026-05-03.md:130-136` | Add local M1 Pro lightningcss parse-only measurements, or change the plan gate to the exact README benchmark operation. |
| Generated LOC gates are tranche-level, not wave-level | 06, 03 | `docs/tranches/BA/waves/W4.md:79-81`, `docs/tranches/BB/BB.md:31-36` | Add per-wave generated LOC windows for BA.W1/W3/W4 and BB.W0-W5. |
| Grammar-host moves remain generic-root shims | 05, 01 | `docs/tranches/BA/waves/W0.md:26-31`, `docs/tranches/BB/waves/W1.md:25`, `audit/CENSUS-2026-05-03.md:103-109` | Move CSS and Sheets host code under per-grammar host namespaces; no `host/<grammar>.rs` generic-root pattern. |
| BD carries have no drafted receiver | 02, 03, 08 | `docs/tranches/BC/BC.md:56-62`, `docs/tranches/BC/BC.md:95` | Draft BD.W0 or cut the carries. |
| Author-facing diagnostics are under-specified | 07, 05 | `docs/tranches/BB/BB.md:21`, `docs/tranches/BB/BB.md:88-92`, `docs/tranches/BC/BC.md:183-187` | Add pointer/lifetime/layout/Pratt/SIMD cookbook pages and diagnostic gates. |
| Semantic materialisation is assumed, not proven | 03, 05 | `docs/HARDENING-AUDIT-PROMPT.md:229-235`, `audit/RESTART-SKETCH-2026-05-03.md:390-394`, `docs/tranches/BC/BC.md:141-152` | Add an inverse-layout-audit gate before direct-to-struct claims close. |
| Specification granularity is too coarse | 02, 03, 08 | `docs/tranches/BA/waves/W4.md:34-53`, `docs/tranches/BB/BB.md:32-33`, `docs/tranches/BC/BC.md:35`, `docs/tranches/BC/BC.md:56-62` | Split broad waves into smaller waves or add drafted successor tranches; no wave may rely on "user adjudicates", "future BD", or all-grammar ownership without per-surface gates. |

## Punch List

1. Edit `docs/tranches/BA/waves/W5.md:35` and `docs/tranches/BA/waves/W5.md:99-104`: delete the requirement that CSS L4 / BBNF / Sheets retain `OpenFrame`; replace W5.M6 with `rg -n 'enum OpenFrame' crates/core/src/runtime/ ; expect 0` or move the full migration explicitly to BA.W6 with gates.
2. Edit `docs/tranches/BA/BA.md:78`, `docs/tranches/BA/waves/W5.md:146`, and `docs/tranches/BB/BB.md:32`: if all-grammar migration stays in BB, BA must mark Lock 1 as deferred-with-receiver, not honoured. Preferred surgery is to move BB.W1 up.
3. Edit `docs/tranches/BA/waves/W2.md:9-11`, `:56`, and `:134`: remove transitional `TypeDesc` / `StructLayout` aliases. The W2 close gate must grep zero retired terms with no `pub use` exception.
4. Edit `docs/tranches/BC/BC.md:30`, `:141`, and `:152`: replace `TypeDesc` with `Layout` vocabulary. If a type descriptor remains, define it as a field of `Layout`, not as a separate canonical IR term.
5. Edit `docs/tranches/BB/BB.md:34`: replace `crates/ir/src/passes/types/` with `bbnf-ir/src/passes/layout/`, and replace `crates/ir/src/egraph/` with the `crates/egraph/` path-dep crate.
6. Edit `docs/tranches/BA/waves/W3.md:3`, `:7`, `:27-32`, `:36`, `:83-84`, and `:96`: remove every stale directory spelling. Use `crates/path`, `crates/path-core`, and `crates/path-ts`, then state whether the package name remains `bbnf-path` or is renamed; update every `cargo -p` gate accordingly.
7. Add to `docs/tranches/BA/waves/W3.md` after M4: move `crates/core/src/path/` into `crates/path/src/runtime/`; W3 close leaves `crates/core/src/path/` empty or deleted.
8. Edit `docs/tranches/BC/waves/W3.md:49-53`: `bbnf-runtime` depends on `crates/path`; it does not absorb `path/`.
9. Move `docs/tranches/BA/waves/W3.md:62-67` into BA.W4 before `docs/tranches/BA/waves/W4.md:34-39`; remove the W3 `test(parse_with)` gate at `docs/tranches/BA/waves/W3.md:148`.
10. Split BA.W4 into two granular waves or milestones: W4a emits the private parse core and proves eager empty-path cursor elision; W4b reroutes `parse` and `Document::get<T>` and runs API tests. No runtime path argument may remain on the eager fast path.
11. Edit `docs/tranches/BA/BA.md:11` and `docs/tranches/BC/BC.md:11`: "Every parse-throughput gate cites..." Then move BA-G3 and BC-G4..G10 under non-SOTA engineering gates.
12. Edit `docs/tranches/BA/BA.md:23` and `docs/tranches/BA/waves/W4.md:41-46`: add a sonic-rs `get`/`get_unchecked` twitter measurement to SOTA with primary-source citation, or mark BA-G9 as internal ratio only.
13. Edit `docs/tranches/BB/BB.md:15-16` and `docs/tranches/BC/BC.md:16`: either gate the exact lightningcss benchmark operation named by SOTA or add M1 Pro parse-only lightningcss measurements before treating CSS gates as SOTA.
14. Edit `docs/tranches/BB/BB.md:141-142`: remove BBNF/Sheets perf rows without external SOTA, or amend SOTA with concrete competitor numbers.
15. Edit `docs/tranches/BA/waves/W0.md:26-31` and `docs/tranches/BB/waves/W1.md:25`: move CSS and Sheets host fns to per-grammar host namespaces; update generated path examples accordingly.
16. Edit `docs/tranches/BA/waves/W1.md:21` and `:57-62`: define recogniser plugin schema fields (`name`, `crate`, `entrypoint`, `output_kind`) so generic IR never hardcodes miner names.
17. Add BA.W2 or BA.W5 gate for `inverse-layout-audit`: every compound-typed rule, including `->`-less rules, has `Layout` and reaches emitted fields.
18. Add BA.W2.M4 fail-explicit table from `audit/CENSUS-2026-05-03.md:571-581`; every fallback/asymmetry/shim row has a grep/test gate.
19. Add BA.W0 or BA.W2 gate deleting the BBNF aggregator `pub use bbnf::*`; BBNF uses namespaced generated access like every other grammar.
20. Add generated LOC gates to `docs/tranches/BA/waves/W1.md`, `W3.md`, and `W4.md`; W4 windows: `json.rs <= 3,700`, `bbnf.rs <= 22,000`, `css_l4.rs <= 110,000`, aggregate <= +5% from W2.
21. Add BB wave LOC windows to `docs/tranches/BB/BB.md:31-36`: W0 unchanged, W1 specialised grammar windows, W2 generated-parser and runtime-template budgets separated, W3 Pratt/SIMD delta rows, W4 wrapper delta <= +2%, W5 visitor delta bounded by record count.
22. Add BC.W3 generated-output relocation budget: path moves to `crates/bbnf-parse/src/parse/generated/`, bytes unchanged; delete stale `crates/core/src/grammar/generated/` post-W3 references.
23. Edit `docs/tranches/BC/BC.md:24` and `:110`: either make BC-G10 aggregate-only <= +2% and per-file <= +2.5%, or reduce JSON's +2.3% row to <= +2%.
24. Edit `docs/tranches/BA/BA.md:59`: replace "BB.W0/W1" with "BB.W1 for CSS L4/BBNF/Sheets; BB.W2 for the five-grammar cohort."
25. Edit `docs/tranches/BB/BB.md:32-33`: resolve "all eight remaining grammars" versus cohort ownership. Split into more waves if necessary; each grammar class gets its own gate.
26. Edit `docs/tranches/BA/waves/W0.md:142`, `docs/tranches/BB/waves/W0.md:52`, and `docs/tranches/BC/BC.md:35`: normalize the fleet-wide fixture receiver to one wave, preferably BC.W5, or add real gates earlier.
27. Edit `docs/tranches/BA/waves/W1.md:23` and `:105`: delete "slow-burn carry" unless a receiving wave, blocker, and close gate are named.
28. Edit `docs/tranches/BA/waves/W3.md:133`: replace BC.W4 with BC.W5 for `bbnf-regex` endpoint reconciliation.
29. Add BC.W0 gates to `docs/tranches/BC/BC.md:30`: `docs/tranches/BC/audit/W0-sibling-baseline.txt` and `docs/tranches/BC/audit/W0-ascent-strategy-disposition.md`.
30. Edit `docs/tranches/BB/BB.md:158`: replace BC.W5 with BC.W4 as Visitor consumer.
31. Edit `docs/tranches/BC/BC.md:35`: remove "user adjudicates at hardening time"; choose one `bbnf-regex` endpoint in the plan and gate it.
32. Edit `docs/tranches/BB/waves/W3.md:57` and `:73`: make BB-G2 hard-fail or add a concrete BC receiving perf gate; no unresolved Tailwind carry to BC.W5 without a gate.
33. Draft `docs/tranches/BD/BD.md` with BD.W0 gates for BC->BD.C1..C3, or delete `docs/tranches/BC/BC.md:56-62` and all BD carry promises from BC.
34. Add `docs/cookbook/path-macro.md`, `docs/cookbook/lifetime-surfaces.md`, `docs/cookbook/visitors.md`, `docs/errors/layout-lowering.md`, `docs/optimizer/pratt-simd-detection.md`, and `docs/migration/bc-core-split.md` as gates in BA.W2, BB.W3, BB.W4, BB.W5, BC.W3, and BC.W4.
35. Add BB.W5 gate: `pointer!` produces typed terminal paths without turbofish on unambiguous paths, wildcard returns typed iterators, and invalid paths include grammar-aware diagnostics.

## Final Readiness

The plan requires a re-draft pass with fuller granularity. Execute none of BA.W0 until the punch list above is applied, because BA.W0's opening authority depends on Lock 7 path naming, Lock 12 archive verification, complete semantic materialisation gates, and an honest carry ledger with drafted receivers. A wave that says "all grammars", "user adjudicates", or "BD later" without a table of artefacts, blockers, and gates is not a plan; it is an ad hoc handoff.
