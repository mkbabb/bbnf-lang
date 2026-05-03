# Hardening Plan Synthesis — BA/BB/BC Restart Plan

Date: 2026-05-03
Input lanes: `audit/HARDENING-PLAN-2026-05-03-01-lock-adherence.md` through `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md`.

## Cohort Verdict

| Lane | Verdict | Blocking faults |
|---|---|---:|
| 01 Lock-Adherence | violated | 9 |
| 02 Sequencing Discipline | violated | 5 |
| 03 Cohesion | violated | 12 |
| 04 SOTA Anchoring | violated | 6 |
| 05 Grammar-Authoritative | violated | 4 |
| 06 Generated-Code Budget | violated | 9 |
| 07 Friction Forecast | violated | 7 |
| 08 Carry & Deferral | violated | 10 |

Decision: **ready after surgery**, not ready to execute. The thesis survives; the plan surface does not yet obey it.

## Cross-Lane Faults

| Fault | Lanes | Primary evidence | Required surgery |
|---|---|---|---|
| BA preserves legacy `OpenFrame` for non-JSON | 01, 02, 06 | `docs/tranches/BA/waves/W5.md:35`, `docs/tranches/BA/waves/W5.md:103-104` | Move all-grammar `OpenFrame` retirement up from BB.W1 or stop claiming Lock 1 at BA close. |
| Layout canon still carries retired names | 01, 02, 03, 07 | `docs/tranches/BA/waves/W2.md:9-11`, `docs/tranches/BC/BC.md:141-152` | Delete `TypeDesc`/`StructLayout`/`LayoutDesc` aliases and update BC IR contract to `Layout`. |
| Path-crate plan still names stale `bbnf-path` and keeps core path runtime | 01, 03, 08 | `docs/tranches/BA/waves/W3.md:3`, `docs/tranches/BA/waves/W3.md:21`, `docs/tranches/BA/waves/W3.md:29-32` | Rewrite to `crates/path`, `crates/path-core`, `crates/path-ts`; move `crates/core/src/path/` runtime into `crates/path/`. |
| `parse_with` deletion precedes replacement | 01, 02, 03 | `docs/tranches/BA/waves/W3.md:62-67`, `docs/tranches/BA/waves/W3.md:148`, `docs/tranches/BA/waves/W4.md:34-39` | Move BA.W3.M5 to BA.W4.M1 or emit replacement before deletion. |
| SOTA claim overstates non-perf gates | 01, 04 | `docs/tranches/BA/BA.md:11`, `docs/tranches/BC/BC.md:11` | Replace "every gate" with "every parse-throughput gate"; split toolchain/LOC gates into engineering tables. |
| Generated LOC gates are tranche-level, not wave-level | 06, 03 | `docs/tranches/BA/waves/W4.md:79-81`, `docs/tranches/BB/BB.md:31-36` | Add per-wave generated LOC windows for BA.W1/W3/W4 and BB.W0-W5. |
| CSS host move remains grammar-specific code in generic core | 05, 01 | `docs/tranches/BA/waves/W0.md:26-31`, `audit/CENSUS-2026-05-03.md:105-109` | Move CSS host code under a per-grammar host namespace. |
| BD carries have no drafted receiver | 02, 03, 08 | `docs/tranches/BC/BC.md:56-62`, `docs/tranches/BC/BC.md:95` | Draft BD.W0 or cut the carries. |
| Author-facing diagnostics are under-specified | 07, 05 | `docs/tranches/BB/BB.md:21`, `docs/tranches/BB/BB.md:88-92`, `docs/tranches/BC/BC.md:183-187` | Add pointer/lifetime/layout/Pratt/SIMD cookbook pages and diagnostic gates. |

## Punch List

1. Edit `docs/tranches/BA/waves/W5.md:35` and `docs/tranches/BA/waves/W5.md:99-104`: delete the requirement that CSS L4 / BBNF / Sheets retain `OpenFrame`; replace W5.M6 with `rg -n 'enum OpenFrame' crates/core/src/runtime/ ; expect 0` or move the full migration explicitly to BA.W6 with gates.
2. Edit `docs/tranches/BA/BA.md:78`, `docs/tranches/BA/waves/W5.md:146`, and `docs/tranches/BB/BB.md:32`: if all-grammar migration stays in BB, BA must mark Lock 1 as deferred-with-receiver, not honoured. Preferred surgery is to move BB.W1 up.
3. Edit `docs/tranches/BA/waves/W2.md:9-11`, `:56`, and `:134`: remove transitional `TypeDesc` / `StructLayout` aliases. The W2 close gate must grep zero retired terms with no `pub use` exception.
4. Edit `docs/tranches/BC/BC.md:30`, `:141`, and `:152`: replace `TypeDesc` with `Layout` vocabulary. If a type descriptor remains, define it as a field of `Layout`, not as a separate canonical IR term.
5. Edit `docs/tranches/BB/BB.md:34`: replace `crates/ir/src/passes/types/` with `bbnf-ir/src/passes/layout/`, and replace `crates/ir/src/egraph/` with the `crates/egraph/` path-dep crate.
6. Edit `docs/tranches/BA/waves/W3.md:3`, `:7`, `:27-32`, `:36`, `:83-84`, and `:96`: remove every `crates/bbnf-path*` spelling. Use `crates/path`, `crates/path-core`, and `crates/path-ts` only.
7. Add to `docs/tranches/BA/waves/W3.md` after M4: move `crates/core/src/path/` into `crates/path/src/runtime/`; W3 close leaves `crates/core/src/path/` empty or deleted.
8. Move `docs/tranches/BA/waves/W3.md:62-67` into BA.W4 before `docs/tranches/BA/waves/W4.md:34-39`; remove the W3 `test(parse_with)` gate at `docs/tranches/BA/waves/W3.md:148`.
9. Edit `docs/tranches/BA/waves/W4.md:32`: replace BA-G2 clone evidence with cursor-elision-only evidence. W4 does not own `OpenFrame` deletion.
10. Edit `docs/tranches/BA/BA.md:11` and `docs/tranches/BC/BC.md:11`: "Every parse-throughput gate cites..." Then move BA-G3 and BC-G4..G10 under non-SOTA engineering gates.
11. Edit `docs/tranches/BA/BA.md:23` and `docs/tranches/BA/waves/W4.md:41-46`: add a real sonic-rs `get_unchecked` twitter measurement to SOTA, or mark BA-G9 as internal ratio only.
12. Edit `docs/tranches/BB/BB.md:141-142`: remove BBNF/Sheets perf rows without external SOTA, or amend SOTA with concrete competitor numbers.
13. Edit `docs/tranches/BA/waves/W0.md:26-31`: move CSS host fn to `crates/core/src/grammar/host/css_l4.rs` or `crates/core/src/host/css_l4/css_types.rs`; update generated path examples accordingly.
14. Edit `docs/tranches/BA/waves/W1.md:21` and `:57-62`: define recogniser plugin schema fields (`name`, `crate`, `entrypoint`, `output_kind`) so generic IR never hardcodes miner names.
15. Add generated LOC gates to `docs/tranches/BA/waves/W1.md`, `W3.md`, and `W4.md`; W4 windows: `json.rs <= 3,700`, `bbnf.rs <= 22,000`, `css_l4.rs <= 110,000`, aggregate <= +5% from W2.
16. Add BB wave LOC windows to `docs/tranches/BB/BB.md:31-36`: W0 unchanged, W1 specialised grammar windows, W2 cohort shrink windows, W3 Pratt/SIMD delta rows, W4 wrapper delta <= +2%, W5 visitor delta bounded by record count.
17. Edit `docs/tranches/BC/BC.md:24` and `:110`: either make BC-G10 aggregate-only <= +2% and per-file <= +2.5%, or reduce JSON's +2.3% row to <= +2%.
18. Edit `docs/tranches/BA/BA.md:59`: replace "BB.W0/W1" with "BB.W1 for CSS L4/BBNF/Sheets; BB.W2 for the five-grammar cohort."
19. Edit `docs/tranches/BA/waves/W0.md:142`: change fleet-wide fixture receiver from BC.W2 to BC.W5, or add a BC.W2 fixture gate.
20. Edit `docs/tranches/BA/waves/W3.md:133`: replace BC.W4 with BC.W5 for `bbnf-regex` endpoint reconciliation.
21. Add BC.W0 gates to `docs/tranches/BC/BC.md:30`: `docs/tranches/BC/audit/W0-sibling-baseline.txt` and `docs/tranches/BC/audit/W0-ascent-strategy-disposition.md`.
22. Edit `docs/tranches/BB/BB.md:158`: replace BC.W5 with BC.W4 as Visitor consumer.
23. Edit `docs/tranches/BC/BC.md:35`: remove "user adjudicates at hardening time"; choose one `bbnf-regex` endpoint in the plan and gate it.
24. Draft `docs/tranches/BD/BD.md` with BD.W0 gates for BC->BD.C1..C3, or delete `docs/tranches/BC/BC.md:56-62` and all BD carry promises from BC.
25. Add `docs/cookbook/pointers.md`, `docs/cookbook/lifetime-surfaces.md`, `docs/errors/layout-lowering.md`, and `docs/optimizer/pratt-simd-detection.md` as gates in BB.W3/BB.W4/BB.W5 and BA.W2.

## Final Readiness

The plan is not a re-draft failure. It is a surgery case. Execute none of BA.W0 until the punch list above is applied, because BA.W0's opening authority depends on Lock 7 path naming, Lock 12 archive verification, and the carry ledger being honest before the first wave lands.
