---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V1
sk_cycle: SK-V18
disposition: REVISE
head_at_review: 4e4aa0648
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH1 Correctness Audit — T-P1 V1 (SK-V18 Totality Excavation)

## Verdict

REVISE. The eight inventories are a substantial improvement over the prior V1
cycle: all three 1F files are now fresh `V5-SKV18-totality` artifacts (the prior
CH1's central complaint — stale `cycle: V6`/SK-V14 1F-anti-pattern and
1F-past-corpora carrying obsolete `RuntimeProvider`/`json_provider.rs`/old-LOC
claims — is FULLY RESOLVED; every refreshed LOC and symbol now verifies at HEAD).
The load-bearing spec-claim ↔ impl rows I spot-verified resolve with high
fidelity: the IR enum spine, the tape substrate, the codegen lowerer dispatch,
the `RuntimeEmitterKind` fork, the css_l4 md5 replica family, the CSS const
courier, the 1D RESULTS/REDRESS citations, and all 16 lock headers carry their
claimed text/symbol/verdict exactly. But the cycle cannot ACCEPT under CH1: one
1D citation names the WRONG line for a real symbol (`diagnostic-x86` at `:2463`,
cited `:2456`); one 1D grammar citation resolves only to a non-obvious second
copy of the file; one 1E frontmatter path does not exist; and several
load-bearing rows (and three inventories that share them) cite a real file by a
bare filename that does not resolve from repo root. No whole inventory is
rejected; no recalled/fabricated symbol was found; the malformed line/path rows
must be corrected before consolidation.

## Findings

| ID | Disposition | Evidence |
|---|---|---|
| CH1-V1-F1 | ACCEPT | 1A IR-enum spine verifies verbatim: `BackendShape` 5 variants at `skinny/crates/ir/src/lib.rs:340-346`; `BackendExpr` 13 variants (Entry…Return) at `:358-392` with `Recognizer::SimdScan` separate at `:394-400`; `ExprKind` 8 variants at `:211-237`; `SubstrateTarget` 4 variants at `cost.rs:57-62`; `Lock1PolicyTriad::fact_stream()` emits `AdmittedFactOutput` at `cost.rs:139`; `all_backend_shapes()->[BackendShape;5]` at `cost.rs:334`. Every 1A-SUB-004/005/009/010/013 verdict matches the cited evidence. |
| CH1-V1-F2 | ACCEPT | 1A tape substrate verifies: `tape/mod.rs:1-5` modules are exactly `assembler`/`event_grammar`/`event_grammar_tests`(cfg-test)/`offsets`; `Tape<'input>` fields source/offsets/flag_cursors/flag_values/payloads/id at `:98`; `ValueRef<…,K=AnyKind,G:EventGrammar=AnyGrammar>` with `_kind:PhantomData<fn()->K>` at `:178` and `_grammar:PhantomData<fn()->G>` at `:179`. 1A-SUB-001/002/011/023 all resolve, incl. the K-axis-preserved / G-axis-phantom distinction. |
| CH1-V1-F3 | ACCEPT | 1B codegen spine verifies: `select_lowering` 5-arm match at `codegen/src/lower/mod.rs:18-26`; the four marker-string lowerers (`eager/offset/event/collapsed_tape.rs`) each 17 LOC delegating to `tape_plan::render_rule` at `tape_plan.rs:58`; `sink_only::lower_program` at `:122`; `lower/rust.rs:32 lower_to_rust` + `:112 validate_policy_facts`; `derive_backend_shape` IS at `passes/src/lib.rs:392` (grep-confirmed; `choose_backend_shape` at `:473`). Every D2/D4 row matches. |
| CH1-V1-F4 | ACCEPT | 1B D1 (RuntimeEmitterKind fork) + D3 (fixed-literal SinkOnly) verify exactly: `RuntimeProfileContract.emitter` at `grammar_provider.rs:33`, `enum RuntimeEmitterKind{CompiledLowering,RequestFacts}` at `:40-42`, dispatch `match …emitter` at `runtime_generator.rs:16-25`, CSS exemption at `grammar_provider.rs:110`; `rg RuntimeEmitterKind restart/ARCHITECTURE.md` = 0 (the "ZERO mentions" claim holds); `render_value_dispatch:124`/`render_container_rules:251`/`render_string_rule:326`/`render_utility_rules:497` take `&mut String` only, `render_header:68`/`render_number_emitter:457` read program data. |
| CH1-V1-F5 | ACCEPT | 1C quantitative census is exact: `css_l4.rs` 108406 LOC/191 `parse_` fns; all 9 grammar/generated LOC+fn counts (bbnf 21557/55 … math 875/3) match C12 verbatim; `rg -ln '@generated' crates/core/src/runtime` = 67 (= the 9-grammar-dir file count, 8+7+7+7+7+7+10+7+7); 7× `css_l4_*/generated.rs` md5 `b654562ccff46ed62dd48e9ace325830`; `CSS_GENERATED_RS:701` + siblings `CSS_MOD_RS:598`/`CSS_PARSER_RS:612`/`CSS_SINK_RS:665`; C3 leak 12 sites / 4 `parse_with.rs`. C2/D4 spec-staleness (`@generated` 0/9 at ARCH:1923-1932) is correctly catalogued. |
| CH1-V1-F6 | ACCEPT | 1D RESULTS/REDRESS resolve: `RESULTS.md` parse_only rows carry twitter 8349.290>4913.095, citm 9079.838>8335.772, canada 16709.901>12970.929 (1D's "8349>4913 / 9079>8335 / 16709>12970" exact); `REDRESS.md:126` "Tape/direct-to-struct remains one substrate", `:6262` "SK-V14 W11W Parse-Only Memchr … Admit", `:6326` "SK-V15 W7 Decision Engine Spine Admit"; `json/generated.rs:801,841,881` carry `parse_w11_1_number_*`; `lock14_baseline.rs:2442 SKV15_W2_EXTRA_COVERAGE_ROOTS` with the cited `runtime_generator.rs`/`grammar_provider.rs`/`json_sink_direct.rs`/`json_typed_direct.rs`/`json_templates` roster at `:2443-2447`. |
| CH1-V1-F7 | ACCEPT | 1E is exemplary. 16-lock headers verify at every cited LOCKS line (`:75`=Lock1 … `:453`=Lock16); `BackendShape` 5-canon at `lower/mod.rs:18-24`; `LOCKS.md:408-409` carries the "returns 67 … asserted Pattern H total" baseline; `builder.rs` 817 LOC with `enum OpenFrame<'p>` at `:16`; `Cargo.toml:81 lto="thin"` vs `skinny/Cargo.toml:80 lto="fat"`; `egraph/Cargo.toml:11 csp-solver="0.1"`; `bbnf-simd/src/lib.rs:5 pub mod x86_64`; x86 file census = 28; `diagnostic-x86` correctly at `lock14_baseline.rs:2463`. |
| CH1-V1-F8 | ACCEPT | 1F-coherence (now fresh `V5-SKV18`) new claims verify: `ir/registry/strategy.rs` `PRODUCTION_MANIFEST_TABLE` at `:134` with grammar-named `idents` (JSON `:138`, GoogleSheets `:144`, CSS `:150`, BBNF `:156`); `css_types.rs:1` "Host shims for the CSS L4 grammar's `-> parse_hex_color(...)` map" / 66 LOC; `HANDOFF.md:17-19` totality-adopt wording; `MASTER-PLAN.md:519` "Nine seed grammars build through new template"; `ARCHITECTURE.md:2003-2005` Sheets by-construction + 24-LOC stub. COH18-001..014 verdicts match. |
| CH1-V1-F9 | ACCEPT | 1F-anti-pattern (the file the prior CH1 rejected as stale) is now fully refreshed and ALL its LOC/symbol claims verify: `report.rs`=11863, `gate.rs`=6175, `lock14_baseline.rs`=5095, `generated_real_typed.rs`=4941, `nonjson_css_l4.rs`=3737, `grammar/lib.rs`=2052, `passes/lib.rs`=2025, `runtime_generator.rs`=1611, `codegen/lib.rs`=1473, `json/generated.rs`=1235; JSON `_RS` literals `JSON_PARSE_ONLY_GENERATED_RS:195`/`JSON_PARSE_ONLY_PARSER_RS:550`/`JSON_MOD_RS:572`/`JSON_HOST_RS:594`; `grammar_facts.rs:799` carries `BbnfBootstrap::parse` in a comment. The prior obsolete `8403/5698/3056/2119/842` and `RuntimeProvider`/`json_provider.rs` claims are GONE. |
| CH1-V1-F10 | REVISE (1D) | `1D-skinny-lessons.md:63` (Lock-14 spec-claim row), `:100` (D-7), and `:170` (G-7) cite `lock14_baseline.rs:2456 "diagnostic-x86"`. The `("crates/bbnf-simd/src/x86_64", "diagnostic-x86")` entry is actually at **`lock14_baseline.rs:2463`**; line `:2456` is `("crates/bbnf-simd/src/aarch64", "strict-checkasm-admitted")` under `SKV15_W2_PRIMITIVE_CLASS_ROOTS`. The symbol is real but the cited line is wrong (off by 7). CORRECTION: change all three `:2456` to `:2463` (matching the CORRECT `:2463` already used by 1E and 1F-past-corpora R9). |
| CH1-V1-F11 | REVISE (1D) | `1D-skinny-lessons.md:64,134,188,192` cite `google-sheets.bbnf:36-51` (7-level tower) and `:67` (`paren_expr="(",expression,")"`) by BARE filename. The claimed tower content is at lines 36-51 and paren_expr at `:67` ONLY in `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf`. The root-obvious `grammar/google-sheets/google-sheets.bbnf` carries the tower at `:103-121` and paren_expr at `:137`; its `:36-51` is an error-literal alternation (`#REF!`…) and `:67` is a comment. The content/symbol claim is TRUE, but the bare path resolves to the non-canonical copy. CORRECTION: cite `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf:36-51,:67` (or re-cite the canonical copy with its `:103-121,:137` lines). |
| CH1-V1-F12 | REVISE (1E) | `1E-locks-evidence.md:20` lists `skinny/crates/codegen/src/lower/json_sink_direct.rs` among `spec_surfaces_audited`. No such file exists — `json_sink_direct.rs` lives at `skinny/crates/codegen/src/json_sink_direct.rs` (NOT under `lower/`). 1B correctly cites it bare; 1E's frontmatter path is wrong. CORRECTION: drop the `lower/` segment. |
| CH1-V1-F13 | REVISE (1B/1D/1F-past-corpora) | The `regen.rs` row shared across `1B-codegen-evidence.md:113` (D5), `1D-skinny-lessons.md:139` (gap) + `:175` (G-12), and `1F-past-corpora.md:52` (R16) cites `regen.rs:5` / `regen.rs:17-18` by BARE filename. The file is `skinny/xtask/src/regen.rs`; `:5 #[derive(Clone, Copy, Debug)]` over `pub(crate) struct RuntimeTarget` at `:6` is CORRECT (the "+1-line PartialEq" claim holds), but `:17-18` points to fields `entry_rule`/`source_roots`, not the recipe, and the bare filename does not resolve from root. CORRECTION: expand to `skinny/xtask/src/regen.rs:5` and drop or re-anchor the loose `:17-18`. |
| CH1-V1-F14 | REVISE (1A) | `1A-substrate-evidence.md:120` exec-summary says "`Tape::id` returns identity at `:170`"; `pub fn id(&self)` is at `tape/mod.rs:172` (line 170 is the prior method's closing brace). The table rows are accurate; only the prose line is off by 2. Additionally 1A uses pervasive non-root-resolving shorthands (`json/scan.rs:1`, `css_l4_declaration_values/generated.rs:257`, `sk-v18/SPEC.md:1202-1207`); these are context-clear but CH1 requires direct resolution. CORRECTION: fix the `:170`→`:172` prose and prefix shorthands with their `skinny/crates/runtime/src/grammars/` / `restart/skinny/tranches/` base. |
| CH1-V1-F15 | REVISE (1C) | `1C-runtime-evidence.md:31` (C3) annotates the spec's `30 sites across 15 files` figure as "(wider `crates/` scan)". `ARCHITECTURE.md:2218` scopes that figure to `crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/` (runtime-only) and self-cross-references `1C-runtime-evidence:125`; the live runtime-only scan returns 12/4, so the spec figure is stale/over-counted WITHIN the same scope, not a scope difference. The parenthetical asserts an unconfirmed cause. CORRECTION: drop the "(wider scan)" attribution (U2 already correctly holds it UNKNOWN) — the divergence stands; the asserted explanation does not. |
| CH1-V1-F16 | REVISE (1E/1F-past-corpora) | Stale HEAD stamps: `1E-locks-evidence.md` live-truth + `LOCKS.md:408` cite verification at `e12c5323d`; `1F-past-corpora.md:37` cites residual verification at HEAD `83b66db42`. All cited commits exist, and every claim I spot-checked still verifies at the actual HEAD `4e4aa0648`, so no claim is falsified — but the stamps imply a verification point two commits behind the dirty tree. CORRECTION: re-stamp to `4e4aa0648` (or note the stamps are inherited from the cited-commit cycle). |

## Evidence Checked (spot-verifications run this pass)

- IR/cost: `skinny/crates/ir/src/lib.rs:340-346,355-400,211-237`; `ir/src/cost.rs:57-62,118-145,334`.
- Tape: `skinny/crates/runtime/src/tape/mod.rs:1-5,94-100,170-195`.
- Codegen: `codegen/src/lower/mod.rs:18-26`; `lower/{eager,offset,event,collapsed}_tape.rs` (17 LOC each); `lower/tape_plan.rs:58`; `lower/sink_only.rs:122`; `lower/rust.rs:32,112`; `passes/src/lib.rs:329,392,401,473`; `grammar_provider.rs:33,40-42,108-112`; `runtime_generator.rs:16-25,91,195,550,572,594,598,612,665,701`; `json_sink_direct.rs:4,68,124,251,326,457,497`; `xtask/src/regen.rs:5-18`.
- Runtime census: `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs'`=71 (9 grammar dirs=67 + tape/=4); `rg -ln '@generated'`=67; `crates/core/src/grammar/generated/*.rs` LOC/fn counts; 7× css_l4 md5; `crates/core/src/runtime/tape/{arena,cursor,mod,record}.rs`; `crates/core/src/runtime/css_l4/parse_with.rs:4,33,36`; leak scan 12/4.
- Locks/spec: `restart/locks/LOCKS.md` lines 75,160,170,179,181,183,200,202,260,269,319,328,336,349,408-409,436,453,620; `restart/ARCHITECTURE.md:1923-1932,1961,2003-2005,2218`; `restart/HANDOFF.md:6-10,17-19`; `restart/MASTER-PLAN.md:519`.
- 1D ledgers: `skinny/RESULTS.md:5-25`; `skinny/REDRESS.md:126-132,6262,6326`; `lock14_baseline.rs:2409,2442-2453,2455-2463`; `grammar/google-sheets/google-sheets.bbnf:103-137` vs `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf:36-67`.
- 1F totality: `crates/ir/src/registry/strategy.rs:134-184`; `crates/core/src/css_types.rs:1`; `crates/ir/src/passes/recognizers/grammar_facts.rs:799`.
- Provenance: `git rev-parse HEAD`=4e4aa0648; cited commits e12c5323d/83b66db42/91b6893b0/139ab1e4a/1c5bd7a25 all exist.

## Artifact Dispositions

| Artifact | CH1 disposition | Notes |
|---|---|---|
| `1A-substrate-evidence.md` | REVISE | All IR/tape/cost table rows resolve exactly; prose `Tape::id :170`→`:172`; expand shorthand citations to root paths (F14). |
| `1B-codegen-evidence.md` | REVISE | Major lowerer/fork/render rows verify verbatim; `regen.rs:5` correct but bare-path (F13). |
| `1C-runtime-evidence.md` | REVISE | Census/md5/LOC/leak all exact and "67 across 9 dirs" correct; C3 "(wider scan)" attribution is an unconfirmed cause for a real divergence (F15). |
| `1D-skinny-lessons.md` | REVISE | RESULTS/REDRESS numbers + leak + 5-shape all resolve; `lock14:2456`→`:2463` wrong line for a real symbol (F10); `google-sheets.bbnf` bare path resolves only to the gorgeous copy (F11). |
| `1E-locks-evidence.md` | REVISE | 16-lock + every impl claim verifies exactly (strongest inventory); one frontmatter path `lower/json_sink_direct.rs` does not exist (F12); HEAD stamp `e12c5323d` (F16). |
| `1F-coherence-scan.md` | ACCEPT | Fresh `V5-SKV18`; sampled strategy.rs/css_types.rs/HANDOFF/MASTER/ARCH cites resolve; `idents :137` vs `:138` is a benign struct-row off-by-one. |
| `1F-anti-pattern.md` | ACCEPT | Prior-cycle stale-V6 reject is RESOLVED: refreshed `V5-SKV18`; every LOC count + `_RS` literal line + `grammar_facts.rs:799` verifies at HEAD. |
| `1F-past-corpora.md` | REVISE | Fresh `V5-SKV18` ledger; SK-V18-synth + `nonjson:3091` + `regen.rs:5` content resolve; `regen.rs` bare-path / loose `:17-18` (F13); residual HEAD stamp `83b66db42` (F16). |

## Required V2 Fold

1. 1D: correct `lock14_baseline.rs:2456`→`:2463` (3 sites) and re-path `google-sheets.bbnf` citations to `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf` (the copy whose line numbers match) or to the canonical copy's `:103-121,:137`.
2. 1E: drop the non-existent `lower/` segment from the `json_sink_direct.rs` frontmatter path.
3. 1B / 1D / 1F-past-corpora: expand the shared `regen.rs` citation to `skinny/xtask/src/regen.rs:5` and re-anchor or drop the loose `:17-18`.
4. 1A: fix the `Tape::id :170`→`:172` prose; prefix the runtime/grammars and `sk-v18/SPEC.md` shorthands with their root-relative base.
5. 1C: remove the unconfirmed "(wider `crates/` scan)" cause from C3 (the 12/4-vs-30/15 divergence stands; the explanation is UNKNOWN per U2).
6. 1E / 1F-past-corpora: re-stamp the HEAD verification commit to `4e4aa0648`.

TALLY accept=9 revise=7 reject=0
