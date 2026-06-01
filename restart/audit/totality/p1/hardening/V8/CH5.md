---
lens: CH5 HIDDEN-COUPLING
cycle: V8
pass: SK-V18 T-P1 TOTALITY-EXCAVATION (challenge)
targets: [1A-substrate-evidence, 1F-anti-pattern, 1F-past-corpora] (+ 1F-coherence-scan cross-read)
generated_at: 2026-06-01T00:00:00Z
method: "Independent live re-derivation of every load-bearing coupling citation from disk + V1 spec (ARCHITECTURE.md, LOCKS.md, sk-v18/SPEC.md). No re-stamp of V6/V7. rg/sed/md5 only; no cargo, no source/inventory edit, no commit."
prior_cycles: "V6 CH5 accept=7/0/0 (re-adjudicated V5's two REVISEs to ACCEPT); V7 CH5 accept=7/0/0 (declared 2-consecutive-clean fixed point for this lens)."
---

# CH5 HIDDEN-COUPLING — V8 verdict

My lens asks: does any catalogued state imply a parallel substrate, a sidecar
producer, or a renamed/parallel scanner that the inventories fail to honour
under the Lock-1 union — and is each live coupling correctly cited where it is
regenerated this cycle? I re-derived every load-bearing citation from disk this
pass rather than re-stamping V6/V7. All hold.

## Spot-verification (live disk + V1 spec, this pass)

| # | Claim under test | Result on disk | Verdict |
|---|---|---|---|
| 1 | Phantom `<G:EventGrammar=AnyGrammar>` is decoration over `&'doc Tape<'input>`+`u32` cursor; K-axis is the REAL preserved axis (1A-SUB-002/023) | `tape/mod.rs:175` field-only `_grammar:PhantomData<fn()->G>` (`:179`); `_kind:PhantomData<fn()->K>` (`:178`) preserved | ACCEPT |
| 2 | Non-test production instantiation of `G` with a real grammar type is EMPTY (1A-SUB-023, SPINE :148-160) | Every `EventGrammar`/`AnyGrammar` site = trait/enum def, `ValueRef` field+impls, two witness defs, or `event_grammar_tests.rs` (`#[cfg(test)]`, mod.rs:3). The `:89 leak` using `JsonEventGrammar` is INSIDE a string-literal compile-fail fixture — not compiled production code | ACCEPT |
| 3 | No skinny retained sidecar (parallel substrate) in runtime grammars | `rg OnceCell\|RefCell\|Cell<\|static mut\|thread_local skinny/.../grammars` = **0** | ACCEPT |
| 4 | `tape/cursor.rs` absent (G4b unimplemented) (1A-SUB-024, G1) | `ls skinny/.../tape/` = assembler/event_grammar/event_grammar_tests/mod/offsets only | ACCEPT |
| 5 | CSS retained parse holds exactly the existing `Tape` — no second skinny substrate (1A-SUB-024) | `css_l4_declaration_values/generated.rs:257` verbatim "Holds exactly the existing `Tape` — no second substrate" | ACCEPT |
| 6 | Totality `OnceCell<StructuralIndex>` probe lives in `crates/core` CONSUMER, classified `generated_function` per-parse (1F-anti-pattern:44, COH18-015) | `json.rs:701` `pub(crate) structural_index: OnceCell<StructuralIndex>` field on `ScanState`; no `static`/`OnceLock`/`lazy_static`; fresh per `ScanState::new()` (`:3442`), threaded `&mut state` (`:742`,`:759`) → ADMISSIBLE class, NOT `retained-across-call-boundary` REJECT (LOCKS:139-149) | ACCEPT |
| 7 | Emitter's own "probe substrate" diction (1F-anti-pattern:44) | `support.rs:67` "The probe substrate (OnceCell + helper)" verbatim | ACCEPT |
| 8 | `simd-scan` exports only `{StructuralIndex, next_structural_at_or_after}`; zero `OnceCell` in `simd-scan` (crate attribution honest) (COH18-015) | `simd-scan/src/lib.rs:68` exact; `rg OnceCell crates/simd-scan/src` = **0** | ACCEPT |
| 9 | Probe breadth is 8/9 (math = 0) (1F-anti-pattern:44) | per-file `ensure_structural_index`: bbnf/bnf/css_l4/css_pretty/csv/ebnf/google_sheets/json = 1, math = **0** | ACCEPT |
| 10 | Skinny lacks `next_structural_at_or_after` — two scanners FUNCTIONALLY PARALLEL, renamed-scanner risk ACTIVE not one-sided (COH18-015, 1F-anti-pattern:44) | `rg next_structural_at_or_after skinny/crates/bbnf-simd/src` = **0**; skinny exposes parallel `StructuralIndex`+`parity_hash` (`lib.rs:94`) consumed `json/scan.rs:22` | ACCEPT |
| 11 | CSS production `input_fnv64` is on the LIVE `emit_full_parse` path, not bench-only (1F-anti-pattern:41) | `generated.rs:393-394` push `input_fnv64`/`fnv64`; `fn fnv64` `:899`; called by `parser.rs:42 parse_full` = the production parser | ACCEPT |
| 12 | Skinny `parity_hash` reached ONLY via `scalar_parity_report`, NOT the `parse_json`/`parse_direct` hot path — fenced diagnostic (1F-anti-pattern:42) | `json/scan.rs:38 scalar_parity_report` → `parity_hash`; `rg scalar_parity_report\|parity_hash json/parser.rs` = **0** | ACCEPT |
| 13 | 7 css_l4 replicas byte-identical md5 `b654562c` (1F-anti-pattern:45, 1F-past R4) | `md5 css_l4_*/generated.rs` = 7× `b654562ccff46ed62dd48e9ace325830` | ACCEPT |
| 14 | LOCKS:620 "`G:EventGrammar` type parameter is the generality vehicle" + names the "separate axis" config-breadth classifier (1A-SUB-025/AMEND-001, COH18-008) | `LOCKS.md:620` verbatim both phrases; SK-V18 `SPEC.md:1202-1207` DELETEs `<G>` with K-axis PRESERVED; `ARCHITECTURE.md:1997` §9.2 prose repeats the vehicle phrase (1A-SUB-026 carrier) | ACCEPT |
| 15 | Totality 9-row `idents` table at exact lines + consumer :216 (COH18-005/012, 1F-anti-pattern:65) | `strategy.rs:137,143,149,155,161,167,173,179,185` verbatim 9 grammar-named rows; `:216 for_grammar_with_manifest(...PRODUCTION_MANIFEST_TABLE)` | ACCEPT |
| 16 | LOCKS:349 self-gate RED = 13 live sites (COH18-012) | `rg JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser crates/ir/src/ crates/analysis/src/` = **13** | ACCEPT |
| 17 | `Tape::id` at `:170` (1A SPINE self-correction over prior `:172`) | `tape/mod.rs:170 pub fn id(&self) -> TapeId` exact | ACCEPT |
| 18 | 1A makes NO "BOTH trees"/corpus-wide/fleet-wide substrate-clean claim; carries NO `crates/core`/`OnceCell`/`probe` reference — honestly skinny-scoped, totality probe 1F-owned | `rg 'both tree\|corpus-wide\|fleet-wide' 1A` = **0**; `rg 'crates/core\|OnceCell\|probe substrate' 1A` = **0**; `live_truth_method:9` enumerates only `skinny/crates/` | ACCEPT |

## Findings

| id | disposition | finding | evidence / basis |
|---|---|---|---|
| CH5-V8-001 | ACCEPT | The phantom `<G>` is decoration, never part of the Lock-1 union (`&'i Tape<'i>`+cursor). Non-test production instantiation census is EMPTY on disk; the `:89 leak` apparent counter-example is a string-literal compile-fail fixture, not compiled code. `1A-SUB-023` + SPINE are correct; the SK-V18 DELETE-default is grounded, K-axis preserved. | rows 1,2; `tape/mod.rs:175,178,179`; `event_grammar_tests.rs:89` (in-string); mod.rs:3 `#[cfg(test)]`; `SPEC.md:1202-1207`. |
| CH5-V8-002 | ACCEPT | No parallel substrate / sidecar producer on the skinny tree: runtime-grammar sidecar sweep = 0; CSS holds exactly the existing `Tape`; `tape/cursor.rs` absent. The Lock-1 union is honoured by the skinny kernel (JSON+CSS share the same sparse flag pair). `1A-SUB-024` substrate-neutral close is grammar-neutral and bound to its skinny spec claim. | rows 3,4,5; sidecar sweep = 0; `generated.rs:257`; `tape/` listing. |
| CH5-V8-003 | ACCEPT | The totality `crates/core` `OnceCell<StructuralIndex>` "probe substrate" is fully caught by 1F, lives in the CONSUMER (not `simd-scan`), and is correctly classified `generated_function` per-parse (fresh per `ScanState::new`, threaded `&mut`) — the ADMISSIBLE class, NOT the `retained-across-call-boundary` REJECT (LOCKS:139-149). The 8/9 breadth (math=0) is exact. The "do NOT close substrate-union 'BOTH trees'" fence is sound. No Track-1≡Track-2 dishonesty. | rows 6,7,9; `json.rs:701,742,759,3442`; `support.rs:67`; `LOCKS.md:139-149`. |
| CH5-V8-004 | ACCEPT | The renamed/parallel-scanner risk is correctly flagged ACTIVE, not totality-one-sided: skinny exposes a parallel `StructuralIndex`+skinny-only `parity_hash` but lacks `next_structural_at_or_after` + cross-parse `OnceCell` retention; crate attribution is honest (`simd-scan` exports `{StructuralIndex, next_structural_at_or_after}`, zero `OnceCell`). The two scanners are functionally parallel with divergent APIs — the SK-V19 scanner-unification carry. | rows 8,10; `simd-scan/lib.rs:68`; `rg next_structural_at_or_after skinny/...` = 0; skinny `lib.rs:94`. |
| CH5-V8-005 | ACCEPT | The production-vs-diagnostic hash split is honest. CSS `input_fnv64` IS live on `emit_full_parse`→`parse_full` (a real coupling, correctly cited as production telemetry / fenced non-equality, NOT bench-quarantined). Skinny `parity_hash` is reachable ONLY via `scalar_parity_report`, never the hot parser path — a fenced diagnostic. Both are non-equality, non-substrate, non-document-identity; the extended close-gate grep now sees `parity_hash`. | rows 11,12,13; `generated.rs:393,899`; `parser.rs:42`; `json/scan.rs:38`; `rg ... json/parser.rs` = 0; 7× md5 `b654562c`. |
| CH5-V8-006 | ACCEPT | The grammar-name-leak couplings (skinny `RuntimeEmitterKind` fork, CSS const courier, `json_*` codegen modules, `json_templates/`; totality 9-row `idents` table + `css_types.rs`) are all LIVE-caught at exact path:line. The single Lock-14 contradiction (phantom `<G>` as "generality vehicle" vs certified DELETE) is precisely surfaced as the 1E amendment candidate / COH18-008 (disposition T-P3 / Pass Omega), and the re-anchor onto the clause's OWN "separate axis" config-breadth classifier is grounded — LOCKS:620 names that axis verbatim. The LOCKS:349 self-gate is correctly reported RED at 13 live sites. No leak passes uncited. | rows 14,15,16; `LOCKS.md:620`; `ARCHITECTURE.md:1997`; `strategy.rs:137-185,:216`; 13-site count. |
| CH5-V8-007 | ACCEPT | No GENUINE reject under the strict convention: every spot-verified path:line resolves at HEAD; no inventory states anything FALSE on disk under this lens. The two-tree sweep found NO missed sidecar / parallel substrate / renamed scanner. 1A is honestly skinny-scoped (zero "BOTH trees"/corpus-wide claim, zero totality-probe reference); the cross-tree couplings are 1F-owned and 1F-fenced per the dispatch matrix; the firewall holds. `Tape::id` is at `:170` as the SPINE self-correction states. No residual precision REVISE survives the PROPORTIONATE standard. | rows 17,18; full table; `rg 'both tree\|corpus-wide\|fleet-wide\|crates/core\|OnceCell\|probe' 1A` = 0; `tape/mod.rs:170`. |

## Convergence note (V7 → V8)

V6 and V7 CH5 both returned accept=7/0/0; V7 declared the 2-consecutive-clean
fixed point for the HIDDEN-COUPLING lens. V8 is an INDEPENDENT re-verification,
not a re-stamp: I re-derived every load-bearing coupling citation from live disk
and the V1 spec this pass — the phantom-`<G>` census (incl. the in-string `:89
leak` non-counterexample), the zero skinny sidecar sweep, the totality
`OnceCell` per-parse `generated_function` classification (no `static`/`OnceLock`,
threaded `&mut`), the 8/9 probe breadth byte-exact, the `simd-scan` zero-OnceCell
crate attribution, the `next_structural_at_or_after`=0 scanner asymmetry, the
CSS production `input_fnv64` vs skinny diagnostic `parity_hash` split, the 7-way
md5 replica, the LOCKS:620 dual-axis clause text, the 9-row `idents` table +
:216 consumer, the 13-site LOCKS:349 self-gate, the `:170` `Tape::id`
self-correction, and 1A's honest skinny-scoping. All hold. No parallel
substrate, sidecar producer, or renamed scanner escapes the catalogues; every
live coupling is cited where regenerated. No inventory states anything FALSE on
disk, and no residual precision REVISE survives the PROPORTIONATE standard. With
V6 and V7 also clean, V8 confirms the HIDDEN-COUPLING lens remains at its
2-consecutive-clean fixed point.

No source edit, inventory edit, staging, or commit was performed for this CH5 report.

TALLY accept=7 revise=0 reject=0
