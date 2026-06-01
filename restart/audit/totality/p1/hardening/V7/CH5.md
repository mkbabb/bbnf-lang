---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-TOTALITY-EXCAVATION
cycle: V7-SKV18 (challenge cycle V7)
disposition: ACCEPT
generated_at: 2026-06-01T00:00:00Z
files_audited:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
spec_surfaces_cross_read:
  - restart/ARCHITECTURE.md (§9.2 :1990-1998)
  - restart/locks/LOCKS.md (Lock 14 :349, :620)
  - restart/skinny/tranches/sk-v18/SPEC.md (:99-102, :1202-1207)
live_code_cross_read:
  - skinny/crates/runtime/src/tape/mod.rs (:170 id, :175 ValueRef, :179 phantom; module roster)
  - skinny/crates/runtime/src/grammars/json/parser.rs (:7-12 ParserState)
  - skinny/crates/runtime/src/grammars/json/config.rs (:22-26 triad)
  - skinny/crates/runtime/src/grammars/json/scan.rs (:38,:43 parity report)
  - skinny/crates/runtime/src/grammars/css_l4_declaration_values/{config.rs,generated.rs,parser.rs}
  - skinny/crates/runtime/src/grammars/css_l4_*/generated.rs (md5)
  - skinny/crates/bbnf-simd/src/lib.rs (:94 parity_hash; next_structural_at_or_after absence)
  - skinny/crates/codegen/src/{grammar_provider.rs,runtime_generator.rs,lib.rs,json_templates/}
  - skinny/xtask/src/regen.rs (:5 derive)
  - skinny/crates/bbnf-bench/src/nonjson_css_l4.rs (:3091)
  - crates/core/src/grammar/generated/*.rs (OnceCell census, all 9)
  - crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs (:67)
  - crates/simd-scan/src/lib.rs (:68)
  - crates/ir/src/registry/strategy.rs (:137-185 idents, :216 consumer)
  - crates/core/src/css_types.rs (:1, 66 LOC)
prior_cycle: V6/CH5.md (ACCEPT; accept=7 revise=0 reject=0)
---

# CH5 HIDDEN COUPLING — SK-V18 T-P1 CHALLENGE (cycle V7)

## Verdict

ACCEPT.

V7 is the convergence-confirming re-run of the HIDDEN-COUPLING lens. The prior V6
CH5 returned ACCEPT=7 (it re-adjudicated the two V5 REVISEs to ACCEPT and found
no genuine reject). My charge for V7: drive out any RESIDUAL precision REVISE to
the 2-consecutive-clean fixed point, and catch any GENUINE reject. I did NOT
re-stamp: I independently re-verified every load-bearing HIDDEN-COUPLING citation
across 1A, 1F-coherence, 1F-anti-pattern, and 1F-past-corpora against BOTH the
live code and the V1 spec at HEAD, and I ran the lens-mandated direct
sidecar/parallel-substrate sweep over both the skinny and totality trees. Every
cited path:line resolves exactly. No inventory states anything FALSE on disk. The
firewall holds. V7 is the SECOND consecutive clean pass for this lens.

## The CH5 firewall (lens contract)

No parallel substrate, sidecar producer, renamed-scanner Lock-1 violation, or
Track-1≡Track-2 dishonesty may pass uncatalogued; the substrate inventory 1A
honours the Lock-1 union; the 1F auxiliaries (anti-pattern, past-corpora) are
correctly cited as live where regenerated this cycle. I spot-verified the most
load-bearing rows against the V1 spec and the live code, and swept BOTH trees
directly for any sidecar 1A/1F might have MISSED.

## Spot-verification of load-bearing rows (all confirmed at HEAD)

| claim (inventory) | live evidence | result |
|---|---|---|
| 1A SPINE: `Tape::id` at `:170`, not the prior `:172` | `tape/mod.rs:170` `pub fn id(&self) -> TapeId` | CONFIRMED |
| 1A-SUB-002 / 1A-SUB-023: `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` over `&'doc Tape` + `u32` cursor; `_grammar:PhantomData<fn()->G>` at `:179` | `tape/mod.rs:175,176,179` verbatim | CONFIRMED |
| 1A-SUB-011: live tape modules `assembler`/`event_grammar`/`offsets`, NOT spec-named token/builder/span/payload/view/trace | `ls tape/` = `{assembler, event_grammar, event_grammar_tests, mod, offsets}.rs`; `mod.rs:1-5` decls | CONFIRMED |
| 1A-SUB-020: skinny retained `ParserState` carries `{input, bytes, cursor, tape}` only — no retained sidecar | `json/parser.rs:7-12` verbatim 4 fields; no `Cell`/`OnceCell` | CONFIRMED |
| 1A-SUB-019: JSON config full direct triad | `json/config.rs:22-26` `SinkOnly`/`direct_sink`/`generated_function`/`generated_grammar`/`pass` verbatim | CONFIRMED |
| 1A-SUB-024 / CH5 substrate-neutral: CSS retained "Holds exactly the existing `Tape` — no second substrate"; `tape/cursor.rs` absent | `css_l4_declaration_values/generated.rs:257` verbatim; `ls tape/` has no `cursor.rs` | CONFIRMED |
| 1A-SUB-016 / 1A-DIV-005: CSS config.rs has ZERO W7/BackendShape/substrate_target row | `rg -c 'W7_\|BackendShape\|substrate_target' css_l4_declaration_values/config.rs` = 0 (exit 1) | CONFIRMED |
| 1A-SUB-025 / COH18-008: LOCKS Lock-14 clause asserts "`G:EventGrammar` type parameter is the generality vehicle" | `LOCKS.md:620` verbatim | CONFIRMED |
| 1A-SUB-026: ARCH §9.2 prose repeats "`G:EventGrammar` type parameter is the generality vehicle" | `ARCHITECTURE.md:1997` verbatim | CONFIRMED |
| 1A-SUB-023 / COH18-008: SK-V18 plan DELETEs the `<G>` axis | `sk-v18/SPEC.md:99-102` (phantom resolved by DELETE), `:1202-1207` (G4a DELETE) verbatim | CONFIRMED |
| COH18-003 / 1F-anti-pattern: skinny generator forks on `RuntimeEmitterKind{CompiledLowering,RequestFacts}`; CSS const courier | `grammar_provider.rs:40-42`; `runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"` | CONFIRMED |
| 1F-anti-pattern Lock-14(a): grammar-named codegen modules + `json_templates/` roster | `codegen/lib.rs:4-5` `mod json_sink_direct; mod json_typed_direct;`; `json_templates/` = config/generated/parser/value/view/visitor.rs | CONFIRMED |
| COH18-005/012 / 1F-anti-pattern: 9 grammar-named `idents` rows in generic `ir` crate; consumer at `:216` | `strategy.rs:137,143,149,155,161,167,173,179,185` exact names; `:216` `for_grammar_with_manifest(grammar_ident, registry, PRODUCTION_MANIFEST_TABLE)` | CONFIRMED |
| COH18-012: LOCKS:349 self-gate returns 13 live sites (11 `crates/ir/src/` + 2 `crates/analysis/src/`) | `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` = 13 (strategy.rs:9 + scalar.rs:1 + grammar_facts.rs:1 = 11 ir; ast_utils/mod.rs:2 = 2 analysis) | CONFIRMED |
| COH18-006: `css_types.rs` 66 LOC host-shim live in generic `crates/core/src/` | `crates/core/src/css_types.rs:1` "Host shims for the CSS L4 grammar's `-> parse_hex_color(...)` map"; `wc -l` = 66 | CONFIRMED |
| 1F COH18-015 / anti-pattern: totality `OnceCell<StructuralIndex>` on `ScanState`; emitter "probe substrate" diction | `crates/core/.../json.rs:701` `pub(crate) structural_index: ::core::cell::OnceCell<::simd_scan::StructuralIndex>`; `support.rs:67` "The probe substrate (OnceCell + helper)" | CONFIRMED |
| 1F: `simd-scan` exports `{StructuralIndex, next_structural_at_or_after}`; `OnceCell` lives in `crates/core` CONSUMER | `simd-scan/src/lib.rs:68` `pub use index::{StructuralIndex, next_structural_at_or_after};` verbatim | CONFIRMED |
| 1F-anti-pattern: skinny `parity_hash` reached only via `scalar_parity_report`, off the hot path | `bbnf-simd/src/lib.rs:94` `pub fn parity_hash`; `json/scan.rs:38` `scalar_parity_report`, hash consumed `:43` | CONFIRMED |
| 1F-anti-pattern: skinny CSS production FNV sidecar on the live recognition path | `css_l4_declaration_values/generated.rs:393-394` `input_fnv64=`/`fnv64(...)`; `fn fnv64` `:899`; called by `parser.rs:42 generated::emit_full_parse(input)` | CONFIRMED |
| 1F: `next_structural_at_or_after` absent in skinny; renamed/parallel scanner asymmetry ACTIVE | `rg -c next_structural_at_or_after skinny/crates/bbnf-simd/src` = 0 (exit 1) | CONFIRMED |
| 1F-anti-pattern: 7 css_l4 replicas md5-identical `b654562c…` | `md5 css_l4_*/generated.rs` = 7× `b654562ccff46ed62dd48e9ace325830` (1 distinct over 7 dirs) | CONFIRMED |
| 1F-past-corpora R16: skinny `RuntimeTarget` derives only `Clone,Copy,Debug` at `regen.rs:5` | `xtask/src/regen.rs:5` `#[derive(Clone, Copy, Debug)]` over `struct RuntimeTarget` `:6` | CONFIRMED |
| 1F-past-corpora R13: `nonjson_css_l4.rs` ambiguity; `:3091 fn measure_mbps` resolves only against `src/` | `src/nonjson_css_l4.rs` = 3737 LOC (`fn measure_mbps` `:3091`); `benches/nonjson_css_l4.rs` = 318 LOC (out of range) | CONFIRMED |

## The "8 of 9" probe breadth — re-verified to the byte (lens core)

The single most precision-sensitive HIDDEN-COUPLING claim is 1F-anti-pattern.md:44's
breadth correction (CH5-V3-003): the totality `OnceCell<StructuralIndex>` probe is
"emitted into **8 of 9** generated grammars (all but `math`, whose structural
alphabet falls outside the `ctns_probe_admits` 12–24-byte window — `math.rs`
carries only a documented-but-inert `ScanState` shell with 0
`ensure_structural_index`)." My direct disk census confirms this EXACTLY:

- `OnceCell` appears in ALL 9 `crates/core/src/grammar/generated/*.rs` files (the
  `ScanState` field + `OnceCell::new()` in `ScanState::new`), 42 occurrences
  total; the ONLY retained-cell KIND present (zero `RefCell`/`Cell<`/`static mut`/
  `thread_local`).
- The ACTIVE probe function `fn ensure_structural_index` is present exactly ONCE in
  each of the 8 non-math grammars (bbnf, bnf, css_l4, css_pretty, csv, ebnf,
  google_sheets, json) and ZERO times in `math.rs`.

So `math.rs` carries the OnceCell FIELD (the "inert shell") but no probe FUNCTION —
which is precisely 1F's "documented-but-inert `ScanState` shell with 0
`ensure_structural_index`." The 8/9 breadth is true to the byte; a naive
`OnceCell`-grep (9 files) does NOT falsify it because 1F distinguishes the field
presence from the active probe emission. No REVISE: 1F's wording is exact and
self-disambiguating.

## Direct two-tree sweep for MISSED coupling (lens-mandated)

To guard against a false "clean" claim I swept both trees directly:

- SKINNY runtime grammars `OnceCell|RefCell|Cell<|static mut|lazy_static|thread_local`
  = ZERO (exit 1). The totality `OnceCell` is correctly NOT present in skinny and
  correctly NOT attributed to skinny by any inventory.
- SKINNY retained `ParserState` = exactly `{input, bytes, cursor, tape}`
  (`json/parser.rs:7-12`); no retained structural sidecar.
- SKINNY substrate type census: `struct Tape` / `TapeBuilder` / `PayloadArena` /
  `OffsetTapeStats` / `TapeId` are the ONE catalogued substrate family — no
  renamed/parallel `*Substrate`/`*Arena` second producer.
- SKINNY grammars: no `EventCursor` / retained structural sidecar / retained index
  producer outside the catalogued set (rg = 0).
- TOTALITY generated grammars: the OnceCell probe (8 active + 1 inert math shell)
  is the ONLY retained-cell surface; it is fully catalogued and fenced by 1F
  (COH18-015 + anti-pattern row 44: classified `generated_function` per-parse,
  the ADMISSIBLE class NOT the REJECT cross-call class `LOCKS.md:139-149`, with
  the explicit "do NOT close substrate-union 'BOTH trees' while unclassified"
  fence).

No skinny-side hidden coupling was missed. The only live cross-tree coupling is
the totality probe substrate + the two-scanner (`next_structural_at_or_after`)
asymmetry, both LIVE-caught and fenced by 1F.

## The Lock-1 union honesty question — confirmed sound (no residual REVISE)

1A is a self-declared SKINNY-substrate inventory (`live_truth_method:9` enumerates
ONLY `skinny/crates/...`), makes NO "BOTH trees"/corpus-wide substrate-clean
claim, and its SPINE Net (`:171-174`) scopes the close to "JSON+CSS." Its word
"grammar-neutral"/"substrate-neutral" everywhere means neutral ACROSS GRAMMARS
(JSON↔CSS reusing one `Tape` + one sparse flag pair), evidenced on disk by
`css_l4_declaration_values/generated.rs:257`. The totality probe is the firewall
job of 1F per the dispatch matrix, and 1F discharges it. The V5 CH5 "add a
cross-reference row to 1A" REVISE was re-adjudicated to ACCEPT at V6; on fresh
proportionate review I AGREE: 1A's absence of a totality-probe row does not
MISLEAD a T-P2 reader of the full packet given (a) 1A's explicit skinny self-scope,
(b) no corpus-wide false close, and (c) 1F's load-bearing "do not close BOTH
trees" fence. Below the PROPORTIONATE REVISE threshold. It does not reopen at V7.

The single stated contradiction (LOCKS Lock-14 :620 / ARCH §9.2 :1997
"generality vehicle" vs the certified `<G>` DELETE) is correctly surfaced as the
1E amendment candidate `1A-LOCK1-AMEND-001` / sibling COH18-008, disposition T-P3
/ Pass Omega — NOT a T-P1 reject, and the inventories state it as such. Correct on
disk.

## Findings

| id | disposition | finding | evidence / basis |
|---|---|---|---|
| CH5-V7-001 | ACCEPT | The phantom `<G>` is decoration, not a second substrate; the Lock-1 union (`&'i Tape<'i>` + cursor) never touched it. `1A-SUB-023` is correct on disk; the SK-V18 DELETE-default is grounded. | `tape/mod.rs:175,179`; `sk-v18/SPEC.md:99-102,:1202-1207`; phantom is a `PhantomData<fn()->G>` field only. |
| CH5-V7-002 | ACCEPT | CSS retained parse holds exactly the existing `Tape` — no second skinny substrate; same sparse flag pair. No renamed/parallel substrate or sidecar producer on the skinny tree (direct sweep = 0). `1A-SUB-024` substrate-neutral close is grammar-neutral (JSON↔CSS), bound to its skinny spec claim. | `css_l4_declaration_values/generated.rs:257`; skinny grammars `OnceCell\|Cell\|RefCell\|static mut\|thread_local` = 0; `struct Tape`/`TapeBuilder`/`PayloadArena` are the one family. |
| CH5-V7-003 | ACCEPT | The totality `OnceCell<StructuralIndex>` "probe substrate" is fully caught by 1F, classified `generated_function` (ADMISSIBLE per-parse class, NOT the REJECT cross-call class), and fenced with "do NOT close substrate-union 'BOTH trees'." The 8/9 breadth is exact: 8 grammars carry the active `ensure_structural_index`, math.rs carries the inert OnceCell shell with 0 probe fns. No Track-1≡Track-2 dishonesty. | `1F-anti-pattern.md:44`; `1F-coherence-scan.md:104`; `json.rs:701`; `support.rs:67`; per-file census `ensure_structural_index` = 1×8 grammars / 0× math; `LOCKS.md:139-149`. |
| CH5-V7-004 | ACCEPT | 1F's crate attribution is honest: the `OnceCell` lives in the `crates/core` CONSUMER, not `simd-scan`; `simd-scan` exports only `{StructuralIndex, next_structural_at_or_after}`. The renamed/parallel-scanner risk is correctly flagged ACTIVE (skinny lacks `next_structural_at_or_after` + cross-parse retention). | `simd-scan/src/lib.rs:68`; `rg -c next_structural_at_or_after skinny/crates/bbnf-simd/src` = 0; skinny `OnceCell` in runtime grammars = 0. |
| CH5-V7-005 | ACCEPT | The grammar-name-leak couplings (skinny `RuntimeEmitterKind` fork, CSS const courier, `json_*` codegen modules, `json_templates/`; totality 9-row `idents` table + `css_types.rs`) are all LIVE-caught by 1F-anti-pattern / 1F-coherence at exact path:line; the LOCKS:349 self-gate is correctly reported RED at 13 live sites. No leak passes uncited. | `grammar_provider.rs:40-42`; `runtime_generator.rs:701`; `codegen/lib.rs:4-5`; `json_templates/`; `strategy.rs:137-185,:216`; `css_types.rs:1`; `rg ... crates/ir/src/ crates/analysis/src/` = 13. |
| CH5-V7-006 | ACCEPT | The single stated Lock-14 contradiction (phantom `<G>` as "generality vehicle" vs the certified DELETE) is correctly surfaced as the 1E amendment candidate / COH18-008, disposition T-P3 / Pass Omega — not a T-P1 reject, and the inventories label it precisely. The LOCKS:620, ARCH:1997, and SPEC:99-102/:1202-1207 anchors all resolve verbatim. | `LOCKS.md:620`; `ARCHITECTURE.md:1997`; `sk-v18/SPEC.md:99-102,:1202-1207`; `1A:180` AMEND row; `1F-coherence COH18-008`. |
| CH5-V7-007 | ACCEPT | No GENUINE reject: every spot-verified path:line resolves at HEAD; no inventory states anything FALSE on disk under this lens. The direct two-tree sweep found NO missed sidecar; the totality probe and two-scanner asymmetry are LIVE-caught and fenced by 1F. The Lock-1 union is honoured by the corpus; the firewall holds. No residual precision REVISE survives proportionate review. | Full spot-verify table; two-tree sweep = 0 missed; 8/9 probe census; `tape/cursor.rs` absent; 7 css_l4 md5 `b654562c` live. |

## Convergence note (V6 → V7)

V6 CH5 returned ACCEPT=7 (re-adjudicating the two V5 REVISEs to ACCEPT). V7 is an
INDEPENDENT re-verification, not a re-stamp: I re-derived every load-bearing
citation from live disk and the V1 spec this cycle (the `id`-at-`:170`
self-correction, the 8/9 probe breadth to the byte, the 13-site LOCKS:349
self-gate, the 9-row `idents` table, the 7-way md5 replica, the FNV/parity_hash
production-vs-diagnostic split, and the `next_structural_at_or_after` asymmetry).
All hold. No residual precision REVISE survives the PROPORTIONATE standard, and no
inventory states anything FALSE on disk. With V6 also clean for this lens, V7 is
the SECOND consecutive clean pass — the HIDDEN-COUPLING lens has reached its
2-consecutive-clean fixed point.

No source edit, inventory edit, staging, or commit was performed for this CH5 report.

TALLY accept=7 revise=0 reject=0
