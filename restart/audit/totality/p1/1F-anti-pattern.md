---
agent: 1F
pass: T-P1-excavation
cycle: V5-SKV18-totality
generated_at: 2026-06-01T00:00:00Z
scope: anti-pattern scan of live code (god modules, parallel substrates, grammar-name leaks in generic crates)
companion: restart/audit/totality/p1/1F-coherence-scan.md (authoritative live coherence packet)
live_truth_method: "find + wc -l + rg + md5 over skinny/crates and crates/core/src at the dirty tree HEAD; no cargo, no build"
---

# 1F Anti-Pattern Scan — SK-V18 Totality Cycle (V5)

Live anti-pattern census against Lock 13 (no god directories/files), Lock 1 (no
parallel substrate), Lock 14 (no grammar-name leaks in generic crates). SK-V18
lens: every row that PRUNE/G1-G6 retires is annotated with its receiver wave.

## God Modules / God Files (Lock 13: files >500 LOC outside `generated/` forbidden; directories >10 mixing concerns forbidden)

| File / dir | Live `wc -l` / count | Verdict | Receiver / verify_action |
|---|---|---|---|
| `skinny/crates/bbnf-bench/src/report.rs` | 11863 LOC | bench surface; Lock 13 permits bench/report/gate over 500 only under explicit gate-surface budget (`restart/locks/LOCKS.md:336`) | Emit a Lock 13 transcript naming each >500 LOC bench file + its exemption or split receiver. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs` | 6175 LOC | gate surface; same exemption question | same |
| `skinny/crates/bbnf-bench/src/lock14_baseline.rs` | 5095 LOC | gate-baseline surface | P4 touches this (Lock 14 green-by-exclusion fix); split-or-budget |
| `skinny/crates/bbnf-bench/src/generated_real_typed.rs` | 4941 LOC | generated-into-bench surface (dirty in `git status`) | trace provenance; if `@generated`, Lock 13 `generated/` exemption applies |
| `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` | 3737 LOC | CSS bench; carries the warm micro-fixture `measure_mbps` SK-V18 P2 DELETES (`SYNTHESIS-AUDIT-OVERFIT.md:162`) | PRUNE-2 (P2) deletes `measure_mbps`/`*_lightningcss_facts` ≈ −700 LOC |
| `skinny/crates/bbnf-bench/src/real_typed_struct.rs` | 2827 LOC | bench surface | budget-or-split |
| `skinny/crates/grammar/src/lib.rs` | 2052 LOC | grammar crate root; production-like, NOT bench-exempt | Lock 13 split candidate; trace concerns |
| `skinny/crates/passes/src/lib.rs` | 2025 LOC | passes crate root; production, NOT bench-exempt | Lock 13 split candidate |
| `skinny/crates/codegen/src/runtime_generator.rs` | 1611 LOC | THE generator file SK-V18 un-forks; carries `CSS_GENERATED_RS` courier (`:701`) + 2 fork arms (`:17`,`:25`) | G1/G2/G3 retire the courier + fork; the file shrinks as the campaign nets −10800 LOC |
| `skinny/crates/codegen/src/lib.rs` | 1473 LOC | codegen root | budget-or-split |
| `skinny/crates/runtime/src/grammars/json/generated.rs` | 1235 LOC | `@generated` — Lock 13 `generated/`-class exemption | G1 re-emits this from the SinkOnly walk |

Note: the bench-surface god files (report/gate/lock14_baseline/real_typed) are
the recurrent Lock 13 census from prior cycles; they are NOT SK-V18 deliverables
but remain the standing budget-transcript obligation.

## Parallel Substrates / Sidecars (Lock 1: one tape; no parallel substrate; no retained sidecar)

| Surface | Live evidence | Verdict | verify_action |
|---|---|---|---|
| Generated CSS FNV input hashes | The 7 `css_l4_*/generated.rs` replicas emit `input_fnv64` + a `fnv64` helper computed by the PRODUCTION `emit_full_parse` on the LIVE Track-1 recognition path — re-anchored on the current production path:line per CH5-V3-005 (was: "prior-cycle V4 transcript"): `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:393` (`out.push_str("source\tinput_fnv64=")`), `:394` (`push_hex64(&mut out, fnv64(input.as_bytes()))`), `:899` (`fn fnv64`); generator template `runtime_generator.rs:1093`,`:1599`; CALLED by the production parser `parser.rs:42 generated::emit_full_parse(input)` and the MEASURED Track-1 plane `skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs:131 fn track1_full` (re-anchored per CH6-V4-005: the file carries the `/bin/` segment, and `fn track1_full` is at `:131` — `:130` is the `// ---- track1_full` comment; bench dispatch at `:306`/`:345`); replicas md5-identical `b654562c`. | telemetry/hash-sidecar coupling; non-equality / non-substrate / non-document-identity telemetry on the PRODUCTION path (NOT bench-quarantined — the bench-side `fnv_quarantine.rs` is the separate clean/KEEP surface); a fence obligation | Re-run the EXTENDED close-gate grep `rg -n 'input_fnv64\|stream_fnv64\|fn fnv64\|fnv64\(\|parity_hash\|ScalarParityReport' skinny/crates crates/core/src` before any close (extended per CH5-V4-012 — the prior 4-token grep could NOT see `parity_hash`); the FNV remains a non-equality arbiter (`SYNTHESIS-AUDIT-OVERFIT.md:113` "the FNV bench-quarantine" CLEAN/KEEP applies to the bench surface; the production `emit_full_parse input_fnv64` is live telemetry, fenced non-equality). |
| Skinny scan-parity diagnostic hash (CH5-V4-012) | `bbnf_simd::StructuralIndex::parity_hash` (`skinny/crates/bbnf-simd/src/lib.rs:94 pub fn parity_hash(&self, input: &[u8]) -> [u8; 32]`) is consumed at `skinny/crates/runtime/src/grammars/json/scan.rs:43` (`hash: index.parity_hash(input)`) building a `ScalarParityReport`, reached ONLY via `scalar_parity_report` (`json/scan.rs:38`) — NOT the `parse_json`/`parse_direct` hot path; the production parser callers `parser.rs:18 structural_capacity_for` and `parser.rs:49 attach_structural_index` never touch it. Previously uncited by any p1 inventory; escapes the 4-token FNV guard. | scan-parity diagnostic hash; NOT on the hot path, NOT an equality arbiter, NOT a retained sidecar — fenced | owner SK-V19 scanner-unification; the extended close-gate grep above now sees it. |
| `ValueRef<…G:EventGrammar>` phantom axis | `skinny/.../tape/mod.rs:175`,`:179` (`_grammar:PhantomData<fn()->G>`); zero non-test production consumers (`sk-v18/SYNTHESIS-RESEARCH.md:26-30`). | decorative generic; not a second substrate, but a phantom | SK-V18 G4 DELETES `<G>` (`sk-v18/SPEC.md:99-102`); preserves the REAL `K=Kind` axis. |
| Totality root `OnceCell<StructuralIndex>` (prior SKV15 COH-014) | LIVE this pass: `crates/core/src/grammar/generated/json.rs:701` `pub(crate) structural_index: ::core::cell::OnceCell<::simd_scan::StructuralIndex>` on `ScanState`; `ensure_structural_index` `:719`; `scan_structural` `:732`; emitted into **8 of 9** generated grammars (all but `math`, whose structural alphabet falls outside the `ctns_probe_admits` 12–24-byte window — `math.rs` carries only a documented-but-inert `ScanState` shell with 0 `ensure_structural_index`; breadth corrected 9→8 per CH5-V3-003); the emitter names it "The probe substrate (OnceCell + helper)" (`crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67`; gate `ctns_probe_admits` `:74-95`). Threaded `&mut ScanState` per parse (`ScanState::new` `json.rs:711` `OnceCell::new()`). | Lock-1 classified (CH5-V1-02): `substrate_target` = the structural-index PROBE; `retention_lifetime = generated_function` (per-parse `&mut ScanState`, NOT cross-call → the ADMISSIBLE class, NOT the REJECT `retained-across-call-boundary` class `LOCKS.md:139-149`); `policy_owner = generated_grammar`. Fenced as per-parse scratch — NOT a proven Lock-1 violation. | The totality emitter's own "probe substrate" diction (`support.rs:67`) + the separate-`OnceCell`-not-the-tape shape are the SK-V19-adoption Lock-1 reconcile burden, priced at ≈ +20..+217 LOC as ONE shared SK-V19 scanner-unification disposition (the ≈217-LOC probe-API reconcile + the 8/9 generated-grammar emission-site re-route; cross-linked to `1F-coherence` COH18-015 and `1E:158` per CH4-V4-008). Scan `OnceCell<StructuralIndex>\|ensure_structural_index\|scan_structural\|next_structural_at_or_after` over `crates/core/src` at SK-V19; classify each hit; do NOT close substrate-union "BOTH trees" while this is unclassified. Skinny `bbnf-simd` carries ZERO `OnceCell<StructuralIndex>` (verified empty) — but it does NOT "lack both" scanners (CH5-V4-011): it exposes a full `StructuralIndex` (`from_positions`/`positions`/`parity_hash`) + `scan_dispatch`/`scan_scalar` (`skinny/crates/bbnf-simd/src/lib.rs:72,78,82,94,106,126`), consumed in `json/scan.rs:22`; what skinny lacks is ONLY the `next_structural_at_or_after` random-access API and the cross-parse `OnceCell` retention. The two scanners are FUNCTIONALLY PARALLEL with divergent APIs — the renamed-parallel-scanner risk is ACTIVE, not totality-one-sided. |
| 7 byte-identical css_l4 replicas | `md5 skinny/.../css_l4_*/generated.rs` = 7× `b654562ccff46ed62dd48e9ace325830` (live). | replica overfit (one scan re-derived into 7 files) | PRUNE-3 (P3) collapses to ONE CSS config (`SYNTHESIS-AUDIT-OVERFIT.md:165`); md5-distinct + structural row-collapse co-gate. |

No NEW retained CROSS-CALL parallel substrate proven this pass. The SKINNY Lock 1
substrate (one `Tape`/`ValueRef`/`PayloadArena`) is verified CLEAN by the SK-V18
audit (`SYNTHESIS-AUDIT-OVERFIT.md:109` "the unified `Tape`/`ValueRef`/`PayloadArena`
substrate (Lock 1 holds — the genuine foundation)"). The TOTALITY-tree
`OnceCell<StructuralIndex>` probe (row above) is a per-parse `generated_function`
lifetime — the ADMISSIBLE class, NOT the REJECT cross-call class
(`LOCKS.md:139-149`); it is now CLASSIFIED (CH5-V1-02), not UNKNOWN, and is the
SK-V19-adoption Lock-1 reconcile carry, NOT a proven violation.

## Grammar-Name Leaks In Generic Crates (Lock 14:349: ZERO grammar-named modules / types / arms in generic crates)

| Surface | Live evidence (path:line) | Verdict | Receiver |
|---|---|---|---|
| Skinny generator fork | `skinny/crates/codegen/src/grammar_provider.rs:40-42` `enum RuntimeEmitterKind{CompiledLowering,RequestFacts}`; consumed `runtime_generator.rs:17`,`:25` | grammar-family fork in generic codegen | SK-V18 G3 DELETES `RuntimeEmitterKind` |
| Skinny CSS const courier | `skinny/crates/codegen/src/runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"…` | verbatim-blob courier (hand-written under grammar banner) | SK-V18 G2 retires |
| Skinny JSON `_RS` literals | `runtime_generator.rs:195` `JSON_PARSE_ONLY_GENERATED_RS`, `:550` `JSON_PARSE_ONLY_PARSER_RS`, `:572` `JSON_MOD_RS`, `:594` `JSON_HOST_RS`, `:598` `CSS_MOD_RS`, `:612` `CSS_PARSER_RS`, `:665` `CSS_SINK_RS` | per-grammar fixed-literal couriers | SK-V18 G1 (JSON) / G2 (CSS) retire; `verbatim_blob_present==false` campaign-wide |
| **Skinny grammar-named MODULES in generic codegen** | `skinny/crates/codegen/src/lib.rs:4` `mod json_sink_direct;`, `:5` `mod json_typed_direct;` — grammar-named modules declared in the GENERIC `codegen` crate (Lock 14 (a) "ZERO grammar-named modules") | grammar-named modules in generic codegen — Lock 14 (a) leak (CH2-004; no grammar-name leak may pass uncited) | SK-V18 G1 (these are the JSON couriers G1 re-emits) ∧ P4 (strict-scan-root promotion) |
| **Skinny `json_templates/` grammar-named directory** | `skinny/crates/codegen/src/json_templates/` — a FULL grammar-named runtime-template roster (`config.rs`/`generated.rs`/`parser.rs`/`value.rs`/`view.rs`/`visitor.rs`) inside the generic `codegen` crate | grammar-named template-roster directory in generic codegen — Lock 14 (a) leak (CH2-004) | SK-V18 G1 (JSON projection oracle, deleted post-byte-equivalence) ∧ P4 |
| **Totality `ir` crate grammar-named table** | `crates/ir/src/registry/strategy.rs:137` `PRODUCTION_MANIFEST_TABLE` carries **NINE** grammar-named `idents` rows: `["JsonParser","JsonGrammar"]` (`:137`), `["GoogleSheetsParser","GoogleSheetsGrammar"]` (`:143`), `["CssL4Parser"]` (`:149`), `["BbnfBootstrap","BbnfParser"]` (`:155`), `["CsvParser","CsvGrammar"]` (`:161`), `["MathParser","MathGrammar"]` (`:167`), `["BnfParser","BnfGrammar"]` (`:173`), `["EbnfParser","EbnfGrammar"]` (`:179`), `["CssPrettyParser","CssPrettyGrammar"]` (`:185`); builder/document paths `crate::runtime::json::JsonStructBuilder` etc. Consumed by the generator at `:216` (`for_grammar_with_manifest(…, PRODUCTION_MANIFEST_TABLE)`). | **the relocated-seam analog** — grammar names in a neutral-identifier data table in the generic `ir` crate. The leak is grammar-neutral-fleet-WIDE: ALL 9 totality grammars (`:137,:143,:149,:155,:161,:167,:173,:179,:185`), of which the strict 4-name leak regex catches only 5 ident sites (CH2-V2-009 — the leak is 9-grammar-wide, NOT 4). | SK-V19 totality fold (R16 structural row-collapse analog, which must collapse ALL 9 rows); the §12 Backend-impl-table command (`ARCHITECTURE.md:1643`) is `crates/codegen/src/`-scoped and MISSES this; the §9 gate (`:2215`) and `LOCKS.md:349` scope `crates/{ir,…}/src/` and DO catch it (11 sites) — CH2-V2-010 corrected the prior `:2215`-scoped-to-`codegen/` misattribution |
| Totality `css_types.rs` | `crates/core/src/css_types.rs:1` "Host shims for the CSS L4 grammar's `-> parse_hex_color(...)`" — the file Lock 14:349 names verbatim as the mess; in `crates/core/src/`, NOT a `crates/<grammar>/` declaration crate | grammar-named host shim in the generic core crate — the lock-NAMED mess; Lock 14 (c) does NOT apply (it admits ONLY a separate `crates/<grammar>/` declaration crate) (CH2-007) | SK-V19; admissible ONLY if RELOCATED to a `crates/css/` declaration crate, else delete (NOT admissible as-is in `crates/core/src/`) |
| Totality `ir` grammar-facts | `crates/ir/src/passes/recognizers/grammar_facts.rs:799` references `BbnfBootstrap::parse` (in a comment); strategy.rs doc-comments name `JsonParser`/`CssL4Parser` | doc-comment grammar names (lower severity) + live `idents` arrays (high) | SK-V19 fold |

## SK-V18 PRUNE-Receiver Annotation (anti-patterns the certified plan retires)

| Anti-pattern | SK-V18 receiver | Net LOC |
|---|---|---|
| x86 surface (`bbnf-simd/src/x86_64/` + `ext/x86/` + nasm `build.rs`) | PRUNE-1 (P1) DELETE crate-wide | ≈ −4500 |
| warm micro-fixture CSS bench (`nonjson_css_l4.rs measure_mbps`) | PRUNE-2 (P2) DELETE | ≈ −700 |
| 7 byte-identical css_l4 replicas + 7 `RuntimeTarget` rows | PRUNE-3 (P3) COLLAPSE | ≈ −5460 |
| Lock-14 green-by-exclusion gate | PRUNE-4 (P4) FIX (move `runtime_generator.rs` into strict `GENERIC_SCAN_ROOTS`) | gate-only |
| metalang leak `parse_w11_1_number ×7` | PRUNE-5 (P5) PURGE | rename-only |
| `CSS_GENERATED_RS` courier + `RuntimeEmitterKind` fork + JSON `_RS` literals | G1/G2/G3 GENERALIZE | net of −10800 campaign |
| phantom `<G>` axis | G4 DELETE | — |

Campaign net: **≈ −10800 LOC** (`SYNTHESIS-AUDIT-OVERFIT.md:153`). The
generalization DELETES far more than it adds — the anti-pattern surface is the
PRUNE target, not a side-effect.
