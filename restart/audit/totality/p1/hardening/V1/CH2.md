---
lens: CH2
name: GENERALITY
pass: T-P1-excavation
cycle: V1
campaign: SK-V18-TOTALITY-EXCAVATION
disposition: REVISE
reviewed_artifacts:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/ORCHESTRATOR.md
  - restart/locks/LOCKS.md
  - restart/ARCHITECTURE.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
spot_verified_live:
  - crates/ir/src/registry/strategy.rs
  - crates/core/src/css_types.rs
  - skinny/crates/codegen/src/grammar_provider.rs
  - skinny/crates/codegen/src/runtime_generator.rs
  - skinny/crates/codegen/src/lib.rs
  - skinny/crates/codegen/src/json_templates/
  - skinny/crates/runtime/src/tape/mod.rs
  - skinny/crates/runtime/src/grammars/css_l4_*/generated.rs
  - skinny/crates/bbnf-simd/src/x86_64/
---

# T-P1 V1 CH2 — GENERALITY / Lock 14

## Verdict

REVISE. The current SK-V18 inventories carry the CH2 generality spine far better
than the prior cycle: 1D cleanly partitions JSON/CSS-empirical (J-1..C-4) from
grammar-neutral (G-1..G-13) findings; the load-bearing Lock 14 leaks
(`RuntimeEmitterKind` fork, `CSS_GENERATED_RS` courier, 7 md5-identical css_l4
replicas, the `crates/ir/src/registry/strategy.rs` grammar-named table,
`css_types.rs`, phantom `<G>`) are inventoried at path:line; and no current
divergence is mis-promoted to fleet-wide proof on JSON-or-CSS-only evidence. I
spot-verified the most load-bearing rows LIVE — `strategy.rs:137-155` (9 grammar
idents, 5 in the leak regex), `css_types.rs` (66 LOC, named-by-the-lock),
`grammar_provider.rs:40-42` (the two-arm fork), `CSS_GENERATED_RS:701`, the 7×
`b654562c…` md5, x86 = 28 files, ValueRef K+G phantom fields — all confirmed.

REVISE, not ACCEPT, on four counts: (1) 1F-coherence COH18-012 MIS-ATTRIBUTES the
Lock 14 gate scope — it claims the cited leak scan "scopes only `codegen`" and
"MISSES" the `ir` leak, but the command it cites (`ARCHITECTURE.md:2215`,
mirroring `LOCKS.md:349`) scans `crates/{ir,parse,codegen,...}/src/` and DOES
catch `strategy.rs` (11 live sites); (2) a real grammar-name leak passes
effectively uncited — `json_sink_direct`/`json_typed_direct` are grammar-named
modules and `json_templates/` is a full grammar-named runtime-template roster
inside the generic `codegen` crate, yet 1F-anti-pattern (the designated leak
scanner) lists none as a Lock 14 (a) "ZERO grammar-named modules" violation; (3)
1C's executive summary over-states the phantom — calling the whole `ValueRef` "a
phantom test-only generic" when only the `G` axis is phantom (the `K`=Kind axis
is real, as 1C's own C5/D5 and the live `_kind: PhantomData<fn()->K>` confirm);
(4) 1F's `css_types.rs` admissibility hedge under-states what the lock body
already settles. The CH2 bar requires that NO grammar-name leak pass uncited and
that 1C/1F flag EVERY grammar-named module in a generic crate
(`restart/prompts/totality/PASS-1-EXCAVATION.md:110-114`); finding (2) is a
direct breach of that bar.

REJECT is not warranted: no inventory claim I tested is recalled, false, or
fabricated. Every leak the inventories DO catalogue is live at the cited
path:line; the defects are misattribution, an uncited leak, and two
over/under-statements — all fold-correctable without re-excavation.

## Governing Evidence

| Source | CH2 requirement |
|---|---|
| `restart/prompts/ORCHESTRATOR.md:84` | CH2 GENERALITY: Lock 14 holds, no grammar-name leak; every intervention grammar-neutral, works for CSS L4 / Sheets / BBNF-self, not only JSON. |
| `restart/prompts/totality/PASS-1-EXCAVATION.md:110-114` | No divergence catalogued JSON-only when grammar-neutral; 1C flags every grammar-named module in a generic crate; 1D separates JSON/CSS-empirical from grammar-neutral; **no grammar-name leak passes uncited.** |
| `restart/prompts/totality/PASS-1-EXCAVATION.md:210` | Lock 14 grammar-neutrality excavated by 1C (runtime census) + 1F (anti-pattern scan); generic crates carry ZERO grammar-named modules; any leak catalogued at path:line. |
| `restart/locks/LOCKS.md:349` | Generic crates (`bbnf-codegen`, `bbnf-ir`, …) carry ZERO grammar-named modules, ZERO grammar-specific public types, ZERO `match grammar {Json=>…}` arms; `crates/core/src/css_types.rs` is NAMED as the overfit mess. Verification command scans `crates/{ir,parse,codegen,runtime,path,…}/src/`. |
| `restart/locks/LOCKS.md:620` | Lock 14 ValueRef clause names `G:EventGrammar` as "the generality vehicle" — the axis SK-V18 G4 DELETEs; the K-axis and `@generated` per-grammar allowance are the surviving neutrality guarantors. |
| `restart/ARCHITECTURE.md:2215` vs `:1643` | The §9 Lock-14 gate command (`:2215`) scans `crates/{ir,…}/src/` (catches `ir`); the §12 Backend-impl table row (`:1643`) is the narrow `crates/codegen/src/`-scoped command. |

## Findings

| ID | Disposition | Finding | Evidence |
|---|---|---|---|
| CH2-001 | ACCEPT | 1D cleanly separates JSON/CSS-empirical from grammar-neutral per Lock 14. JSON guard (J-1/J-2) and CSS measurement-valid + courier (J-3/C-1/C-2) sit in the JSON/CSS-empirical table; the substrate (G-1), aarch64 (G-2), 5-shape decision spine (G-3), SIMD admission (G-4), FNV quarantine (G-5), the emitter fork (G-6), the gate (G-7), and the phantom (G-8) sit in the grammar-neutral table. No grammar-neutral failure is smuggled as JSON-only and no JSON/CSS row is promoted to fleet-wide closure. | `restart/audit/totality/p1/1D-skinny-lessons.md:148-159` (JSON/CSS-empirical) vs `:160-176` (grammar-neutral); SK-V18 measurement-valid framing re-confirmed `1F-coherence-scan.md:83` COH18-013. |
| CH2-002 | ACCEPT | The load-bearing Lock 14 leaks are inventoried AND live-true. Spot-verified: `crates/ir/src/registry/strategy.rs:137-155` hand-written `PRODUCTION_MANIFEST_TABLE` carries 9 grammar idents (5 in the leak regex) — the relocated-seam analog 1F/1C/1B name; `crates/core/src/css_types.rs` is 66 LOC line-1 "Host shims for the CSS L4 grammar"; `grammar_provider.rs:40-42 RuntimeEmitterKind{CompiledLowering,RequestFacts}` + `runtime_generator.rs:18,29` dispatch; `CSS_GENERATED_RS:701` const; 7× css_l4 `generated.rs` md5 `b654562ccff46ed62dd48e9ace325830`; x86 = 28 files; ValueRef `_kind`+`_grammar` PhantomData fields. | `1F-coherence-scan.md:75` COH18-005, `:76` COH18-006; `1F-anti-pattern.md:58-59`; `1B-codegen-evidence.md:57,65-78`; `1C-runtime-evidence.md:37,50-51`; live: `strategy.rs:137-189`, `css_types.rs:1`, `grammar_provider.rs:40-42`, `runtime_generator.rs:18,701`, md5×7, `bbnf-simd/src/x86_64/` 28 files, `tape/mod.rs:178-179`. |
| CH2-003 | REVISE | `1F-coherence-scan.md` COH18-012 mis-attributes the Lock 14 gate scope. It claims "the ARCH-cited verification command scopes only `codegen` — the totality leak scan is too narrow" and "the leak … in a Lock-14-scoped crate (`ir`)" is missed, classing the row impl-exceeds-spec. FALSE as cited: `ARCHITECTURE.md:2215` (the §9 gate command COH18-012 cites) and `LOCKS.md:349` both scan `crates/{ir,parse,codegen,runtime,…}/src/`, which DOES catch `strategy.rs` (11 live sites). The genuinely codegen-scoped command is `ARCHITECTURE.md:1643` — a §12 Backend-impl-table row COH18-012 does NOT cite. The real gap is narrower than catalogued (one §12 sub-command, not "the totality leak scan"). | CORRECTION (`restart/audit/totality/p1/1F-coherence-scan.md:82` COH18-012, `:99` divergence row, `:109` gap row): re-cite the actually-narrow command as `ARCHITECTURE.md:1643` (`crates/codegen/src/`-scoped §12 row), and record that the §9 gate (`:2215`) and Lock 14 (`LOCKS.md:349`) DO catch `strategy.rs` (live: `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/ir/src/` = 11, incl. 5 `idents` rows). The row is spec-defect-on-`:1643`, not "gate too narrow on `:2215`"; reclassify off impl-exceeds-spec. |
| CH2-004 | REVISE | A real grammar-name leak passes effectively uncited — breaching the CH2 "no grammar-name leak passes uncited" + "flag every grammar-named module in a generic crate" bar. `skinny/crates/codegen/src/lib.rs:4-5` declares `mod json_sink_direct;` + `mod json_typed_direct;`, and `skinny/crates/codegen/src/json_templates/` is a FULL grammar-named runtime-template roster (config/generated/parser/value/view/visitor) — grammar-named modules in the generic `codegen` crate, the Lock 14 (a) "ZERO grammar-named modules" clause. 1F-anti-pattern (the designated grammar-name-leak scanner) has ZERO mentions of any; its leak table (`:53-60`) lists only the `_RS` literal constants, `strategy.rs`, and `css_types.rs`. 1B/1C/1E cite the file NAMES only as render/cursor/courier/gate-root concerns, never as the module-name Lock 14 leak. | ADD to `restart/audit/totality/p1/1F-anti-pattern.md` grammar-name-leak table (`:51-60`): rows for `skinny/crates/codegen/src/lib.rs:4-5` (`mod json_sink_direct`, `mod json_typed_direct` — grammar-named modules in generic codegen) and `skinny/crates/codegen/src/json_templates/` (grammar-named template-roster directory in generic codegen), each Lock 14 (a). Receiver: SK-V18 G1 (these are the JSON couriers G1 re-emits) + P4 (strict-scan-root promotion). |
| CH2-005 | REVISE | `1C-runtime-evidence.md` executive summary over-states the phantom. It says "`ValueRef<…,G:EventGrammar=AnyGrammar>` is a phantom test-only generic" (`:23`). The `ValueRef` struct is NOT phantom — it is the real grammar-neutral materialization plane with a REAL `K`=Kind axis (`_kind: PhantomData<fn()->K>` at `tape/mod.rs:178`, dispatched by `JsonNodeKind`/`RootKind` in `value.rs`); only the `G` axis is decorative. 1C's own table C5 and divergence D5 state this correctly ("`K` (Kind) axis IS real; only the `G` axis is decorative"). The summary compression risks reading as if the materialization plane itself is phantom — a CH2 mis-frame of the grammar-neutral substrate. | CORRECTION (`restart/audit/totality/p1/1C-runtime-evidence.md:23`): rephrase to "the `G:EventGrammar` AXIS of `ValueRef` is a phantom test-only generic (the `K`=Kind axis is real)," matching its own C5 (`:33`) and D5 (`:56-57`). |
| CH2-006 | ACCEPT | The Pattern H census is correctly catalogued and live-true. `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs'` = 71; per-grammar (excluding the generic `tape/` substrate dir) = 67. 1C, 1E, and 1F all record 71-total / 67-per-grammar with the +4 = `tape/{mod,cursor,arena,record}.rs` substrate trace, and route the LOCKS-bound-67 vs live-71 command drift to a T-P3 `tape/` exclusion — a grammar-neutral substrate fact correctly kept out of the per-grammar leak count. | Live: 71 total / 67 per-grammar. `1C-runtime-evidence.md:21`; `1E-locks-evidence.md:106` D-1E-V5-06, `:132` 1E-V5-U3; `1F-coherence-scan.md:77` COH18-007. |
| CH2-007 | REVISE | 1F's `css_types.rs` admissibility hedge under-states the lock text. COH18-006 (`1F-coherence-scan.md:76`) and U-COH18-002 (`:116`) hedge that `css_types.rs` "may be admissible as a per-grammar host-fn shim per Lock 14 (c)." But Lock 14 (c) admits ONLY "a per-grammar declaration crate (`crates/<grammar>/`)" — a SEPARATE crate. `css_types.rs` lives in `crates/core/src/` (the generic core crate, NOT `crates/css/`) and is NAMED verbatim in the Lock 14 body (`LOCKS.md:349`) as part of "the current overfitting mess … this lock prevents from recurring." The hedge re-opens a question the lock text already settles. | CORRECTION (`restart/audit/totality/p1/1F-coherence-scan.md:76,116`; `1F-anti-pattern.md:59`): record that `css_types.rs` is in-`crates/core/src/` (NOT a `crates/<grammar>/` declaration crate), so Lock 14 (c) does NOT apply; it is the lock-NAMED mess. Keep the SK-V19 receiver; drop the "may be admissible" framing or scope it strictly to "only if relocated to a `crates/css/` declaration crate." |

## Fold Directives

| Fold | Required V2 action |
|---|---|
| CH2-FOLD-001 | In `1F-coherence-scan.md`, fix COH18-012's gate-scope attribution: the narrow command is `ARCHITECTURE.md:1643` (codegen-scoped §12 row), NOT the cited `:2215` (which scans `crates/{ir,…}/src/` and catches `strategy.rs`). Reclassify off impl-exceeds-spec; record the live 11-site `crates/ir/src/` count. |
| CH2-FOLD-002 | In `1F-anti-pattern.md`, add grammar-named-module leak rows: `skinny/crates/codegen/src/lib.rs:4-5` (`mod json_sink_direct`, `mod json_typed_direct`) and `skinny/crates/codegen/src/json_templates/` (full grammar-named template roster) — Lock 14 (a) leaks in generic `codegen`, receivers G1/P4. No grammar-name leak may pass uncited. |
| CH2-FOLD-003 | In `1C-runtime-evidence.md:23`, scope the phantom to the `G` AXIS of `ValueRef` (the `K` axis is real), aligning the executive summary with its own C5/D5. |
| CH2-FOLD-004 | In `1F-coherence-scan.md` / `1F-anti-pattern.md`, drop the `css_types.rs` "may be Lock 14 (c) admissible" hedge: the file is in `crates/core/src/`, not a `crates/<grammar>/` declaration crate, and is lock-named as the mess. Re-anchor the admissibility note strictly to a relocate-to-declaration-crate condition. |

No source, lock, prompt, or inventory file is changed by this CH2 verdict; T-P1 catalogues, T-P3 disposes, Pass Omega merges.

TALLY accept=3 revise=4 reject=0
