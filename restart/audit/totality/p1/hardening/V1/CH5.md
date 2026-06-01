---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V1 (SK-V18 totality re-roll)
disposition: REVISE
generated_at: 2026-06-01T00:00:00Z
files_audited:
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
  - restart/skinny/tranches/sk-v18/SPEC.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
live_truth_method: "sed/Read line reads of Lock 1 (LOCKS.md:75-165) + the eight SK-V18-V5 inventories; spot-verified the load-bearing path:line rows in live code with sed + rg + md5 over skinny/crates and crates/core/src — Tape/ValueRef struct, CSS no-second-substrate doc, PRODUCTION_MANIFEST_TABLE, RuntimeEmitterKind fork, CSS_GENERATED_RS courier, the totality OnceCell<StructuralIndex> ScanState sidecar across 9 grammars + its emitter in support.rs, skinny scan.rs/attach_structural_index, simd-scan vs bbnf-simd scanner asymmetry, 7×css_l4 md5 b654562c. No cargo, no build, no source edit, no commit."
---

## Verdict

**REVISE.** The SK-V18-V5 inventories correctly honour the Lock-1 union for the
*benched skinny tree*: one grammar-neutral `Tape<'input>`, one `ValueRef` cursor,
CSS retained parse holding "exactly the existing `Tape` — no second substrate".
Every skinny substrate row I spot-checked grounds at the cited path:line. The
relocated-seam couplings (the `RuntimeEmitterKind` config-carried fork, the
`PRODUCTION_MANIFEST_TABLE` grammar-named table in the generic `ir` crate) are
caught by 1B/1E/1F. The Track-1≡Track-2 dishonesty is NOT reopened.

But CH5 cannot ACCEPT as-is. The packet has a single, structural hidden-coupling
hole, and it appears in three inventories: **the totality `crates/core` tree
carries a LIVE retained `OnceCell<StructuralIndex>` sidecar** — its own emitter
docstring calls it "The probe substrate (OnceCell + helper)"
(`crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67`) —
emitted into nine generated grammars (`crates/core/src/grammar/generated/{json,
bnf,math,ebnf,google_sheets,bbnf,csv,css_l4}.rs`), fed by a *distinct scanner
crate* `crates/simd-scan` carrying a `next_structural_at_or_after` probe API that
skinny's `bbnf-simd` does not have. 1F downgrades this to "UNKNOWN this cycle";
1E's "retained sidecar: NONE" no-candidates census omits it even though 1E's own
frontmatter audits `crates/core/src/grammar/generated/`; 1D restates "no retained
sidecar" without scoping the claim off the totality tree. The deferral is
*scope-honest* under the certified SK-V18 skinny boundary, but the CH5 charge is
"no catalogued state implies a sidecar producer / renamed-scanner" — a LIVE
retained-structural-offset sidecar in an audited tree, named "substrate" by its
own emitter, must be CLASSIFIED under Lock 1's target/lifetime/owner vocabulary,
not left UNKNOWN.

Governing scope: Lock 1 rejects "any retained class/mask stream, parser-owned
cursor/list state, public substrate API, `UnionTape`, or second tape … unless
G-Omega explicitly amends Lock 1" and fixes "if structural offsets are retained,
the structural projection IS the tape" (`restart/locks/LOCKS.md:75`,`:120`-`127`).
The totality `OnceCell<StructuralIndex>` is a retained structural-offset stream
held in a *separate* `OnceCell` field, NOT the tape — exactly the surface Lock 1
fences. Its retention lifetime is per-parse (`ScanState::new()` →
`OnceCell::new()`, threaded `&mut state` through the whole recursive descent),
which maps to the *admissible* `generated_function` class, not the REJECT
`retained-across-call-boundary` class (`restart/locks/LOCKS.md:139`-`149`). So the
correct disposition is "classify and fence", not "reject" — which is precisely
why leaving it UNKNOWN is the defect.

## Findings

| ID | Disposition | Finding | Evidence | Correction / fold directive |
|---|---|---|---|---|
| CH5-V1-01 | ACCEPT | 1A honours the Lock-1 union for the skinny benched tree: one grammar-neutral `Tape`, one `ValueRef` cursor, CSS holds exactly the existing tape — no second substrate. | Verified live: `skinny/crates/runtime/src/tape/mod.rs:94` `Tape<'input>{source,offsets,flag_cursors,flag_values,payloads,id}`; `:175` `ValueRef<'doc,'input,K,G>` = `&Tape` + `u32` cursor; `grammars/css_l4_declaration_values/generated.rs:257` `CssDocument` "Holds exactly the existing `Tape` — no second substrate". Matches 1A-SUB-002/003 (`1A-substrate-evidence.md:65`-`66`), 1C-C4/C11 (`1C-runtime-evidence.md:32`,`:39`). | Preserve. The skinny substrate-union half is sound; do not let it be read as totality-tree closure (see CH5-V1-02). |
| CH5-V1-02 | REVISE | 1F-anti-pattern DEFERS the LIVE totality `OnceCell<StructuralIndex>` sidecar to "UNKNOWN this cycle (skinny is the SK-V18 plane)". It is not UNKNOWN: it is emitted into 9 generated grammars and its own emitter names it "The probe substrate". | Live: `crates/core/src/grammar/generated/json.rs:701`-`702` `structural_index: ::core::cell::OnceCell<::simd_scan::StructuralIndex>` on `ScanState`; `ensure_structural_index` at `:719`; `scan_structural` at `:732`; same surface in bnf/math/ebnf/google_sheets/bbnf/csv/css_l4 generated.rs; the emitter calls it "The probe substrate (OnceCell + helper)" at `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67`. 1F row at `1F-anti-pattern.md:43` ("carried; not re-grounded this pass … UNKNOWN this cycle"). | REVISE `1F-anti-pattern.md` row "Totality root `OnceCell<StructuralIndex>`": replace the UNKNOWN with a Lock-1 classification — `substrate_target`, `retention_lifetime=generated_function` (per-parse `&mut ScanState`, not cross-call), `policy_owner=generated_grammar`. Fence as per-parse scratch; record that the totality emitter's own "probe substrate" diction needs Lock-1 reconcile at SK-V19 adoption. Expand the scan to `OnceCell<StructuralIndex>\|ensure_structural_index\|scan_structural\|next_structural_at_or_after`. |
| CH5-V1-03 | REVISE | 1E's "retained sidecar: NONE" no-candidates census omits the totality `OnceCell<StructuralIndex>` even though 1E's frontmatter explicitly audits `crates/core/src/grammar/generated/` in BOTH trees. | `1E-locks-evidence.md:154` "New substrate / public substrate API / retained sidecar: NONE — one `Tape`/`ValueRef` holds … the phantom `<G>` is a DELETE, not an addition." Frontmatter `:25` audits `crates/core/src/grammar/generated/`; `:28` "BOTH trees". The OnceCell sidecar at `crates/core/src/grammar/generated/json.rs:701` is inside the audited surface. | REVISE `1E-locks-evidence.md:154`: scope the "retained sidecar: NONE" finding to the skinny benched tree explicitly, and add a totality-tree note that the `OnceCell<StructuralIndex>` per-parse probe substrate is a SK-V19-adoption Lock-1 classification carry (cross-reference CH5-V1-02). The 16-lock-count and skinny-clean claims survive; only the "NONE" universality is too broad. |
| CH5-V1-04 | REVISE | No inventory names `crates/simd-scan` as a *parallel scanner crate* carrying a probe API (`next_structural_at_or_after`) that the skinny `bbnf-simd` scanner does NOT have — the renamed/parallel-scanner axis the CH5 lens specifically guards. | `crates/simd-scan/Cargo.toml` exists; `next_structural_at_or_after` lives only in `crates/core/.../support.rs` + `crates/core/src/grammar/generated/google_sheets.rs` (rg). Skinny `bbnf-simd/src` has ZERO `next_structural_at_or_after` / `scan_structural` (verified empty). The only inventory mention of `simd-scan` is 1E-L11 as a root Cargo.toml path-dep drift (`1E-locks-evidence.md:90`), not as a scanner-coupling. | REVISE `1F-coherence-scan.md` (or `1F-anti-pattern.md`): add a row naming the scanner-crate asymmetry — totality `simd-scan` carries a retained-index probe API (`next_structural_at_or_after` + `OnceCell<StructuralIndex>`) absent from skinny `bbnf-simd`. Classify whether SK-V19 adoption unifies the two scanners or carries a renamed parallel scanner. This is the renamed-scanner check the CH5 lens demands and the V5 packet does not perform. |
| CH5-V1-05 | ACCEPT | The `RuntimeEmitterKind` config-carried grammar-family fork (the relocated-seam / Track-analog hidden coupling) is correctly caught across 1B/1E/1F. | Verified: `skinny/crates/codegen/src/grammar_provider.rs:33` `emitter: RuntimeEmitterKind` field; `:40`-`42` `enum RuntimeEmitterKind{CompiledLowering,RequestFacts}`; dispatch `runtime_generator.rs:16`-`26`. Caught at 1B-D1/D5 (`1B-codegen-evidence.md:65`,`:107`), 1E-D-1E-V5-02 (`1E-locks-evidence.md:102`), 1F (`1F-coherence-scan.md:73`, `1F-anti-pattern.md:55`). | Preserve. The config-carried strategy is correctly flagged as the relocated seam md5-distinctness cannot see; the row-collapse co-gate (`PartialEq` on `RuntimeTarget`) is the right structural answer. |
| CH5-V1-06 | ACCEPT | The `PRODUCTION_MANIFEST_TABLE` grammar-named ident table in the generic `ir` crate (the totality relocated-seam analog) is correctly caught and the too-narrow Lock-14 leak-scan scope is flagged. | Verified live: `crates/ir/src/registry/strategy.rs:137`-`160` `PRODUCTION_MANIFEST_TABLE` with `idents:&["JsonParser","JsonGrammar"]`, `["GoogleSheetsParser","GoogleSheetsGrammar"]`, `["CssL4Parser"]`, `["BbnfBootstrap","BbnfParser"]` + builder/document paths `crate::runtime::json::JsonStructBuilder`. Caught at 1F-COH18-005/012 (`1F-coherence-scan.md:75`,`:82`), 1F-anti-pattern (`1F-anti-pattern.md:58`). | Preserve. The gate-scope gap (ARCH leak scan scopes only `crates/codegen/src/`, misses `crates/ir/`) is the correct hidden-coupling catch. |
| CH5-V1-07 | ACCEPT | The CSS `same-plane-source-sidecar` comparator is correctly fenced as output-plane / bench-quarantined comparator evidence, not a retained runtime substrate. | Live: `same-plane-source-sidecar` comparator label survives at 7 sites in `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` (`:957`,`:1084`,`:1235`,`:1392`,`:1542`,`:1696`,`:1845`); `fixture_sidecar_facts` at `:2576`; `lightningcss_facts` family `:528`-`630`. 1F keeps FNV/CSS comparator bench-quarantined (`1F-anti-pattern.md:41`); Lock 1 v+1 fact-stream clause governs (`restart/locks/LOCKS.md:96`-`106`). | Preserve as comparator/output-plane evidence only; never as retained runtime substrate or CSS value API proof. Note: the V5 packet's anchor at `nonjson_css_l4.rs:2576` (this pass) supersedes the stale prior-cycle CH5 anchors (`:1080`/`:2691`). |
| CH5-V1-08 | ACCEPT | Track-1≡Track-2 dishonesty is not reopened. 1D correctly states JSON cold Track-1 beats sonic-rs strict same-plane while CSS >SOTA rides hand-written content; Lock 1 v+1 substrate-ceiling fold governs Track 2 as a probe, not a second substrate. | 1D rows (`1D-skinny-lessons.md:61`,`:67`,`:164`) cite `RESULTS.md:5-25` per-row JSON wins, `tape/mod.rs:175` single substrate, `SYNTHESIS-AUDIT-OVERFIT.md:109` Lock-1-clean. Lock 1 v+1 (`restart/locks/LOCKS.md:84`-`90`) fixes Track 2 as substrate-ceiling probe. No Track-1≡Track-2 substrate-independence claim made. | Preserve. CSS preservation-through-the-generator is correctly held open as a burden, not asserted closed. |
| CH5-V1-09 | REVISE | 1A-SUB-017 mischaracterizes the skinny `attach_structural_index`: it says JSON "retained `attach_structural_index` consumes capacity only". The live function is a NO-OP (`let _ = state;`), consuming nothing; the actual capacity scan lives in `scan.rs::scan_structurals`. | Live: `skinny/crates/runtime/src/grammars/json/generated.rs:12`-`15` `pub(crate) fn attach_structural_index(state: &mut ParserState<'_>){ debug_assert_eq!(config::STRUCTURAL_BYTES, …); let _ = state; }` — a no-op stub. The capacity scan is `scan.rs:22 scan_structurals` / `scan.rs:47 structural_capacity_for`. 1A claim at `1A-substrate-evidence.md:80`. | REVISE 1A-SUB-017: correct the symbol behavior — `attach_structural_index` is a no-op stub (debug-assert only); the structural scan returning `StructuralIndex` for capacity is `scan.rs::scan_structurals`, consumed by `structural_capacity_for`. The directional conclusion (skinny carries no retained structural sidecar; scan is `local_temp_only`) HOLDS and strengthens; only the cited symbol's stated behavior is wrong. |
| CH5-V1-10 | ACCEPT | 1D restating "no retained sidecar / Lock 1 holds CLEAN" is scope-honest: 1D is the skinny-lessons inventory, the claim is anchored to the skinny tree + the S-P0 audit, and 1D does not assert totality-tree closure. | `1D-skinny-lessons.md:36`,`:67`,`:113`,`:164` all cite skinny `tape/mod.rs:175` + `SYNTHESIS-AUDIT-OVERFIT.md:109`. No totality-tree substrate claim. | Preserve. 1D's scope is legitimately skinny; the totality OnceCell gap is 1F/1E's catalogue obligation (CH5-V1-02/03), not 1D's. |

## Fold Directives

1. **F-CH5-V1-01 — totality structural-index sidecar census (the spine fix).**
   Promote the totality `OnceCell<StructuralIndex>` from 1F-anti-pattern's UNKNOWN
   to a classified row. Scan
   `OnceCell<StructuralIndex>\|ensure_structural_index\|scan_structural\|next_structural_at_or_after`
   over `crates/core/src`; classify each hit under Lock 1
   `substrate_target`/`retention_lifetime`/`policy_owner`. The per-parse
   `&mut ScanState` lifetime is `generated_function` (admissible), but the
   emitter's own "probe substrate" diction (`support.rs:67`) and the
   separate-OnceCell-not-the-tape shape are the SK-V19 Lock-1 reconcile burden.
   Do NOT close substrate-union "BOTH trees" while this is unclassified.
2. **F-CH5-V1-02 — scope the "retained sidecar: NONE" census.** Re-author
   `1E-locks-evidence.md:154` to scope NONE to the skinny benched tree; add the
   totality OnceCell as a SK-V19-adoption Lock-1 classification carry.
3. **F-CH5-V1-03 — name the scanner-crate asymmetry.** Add a 1F row: totality
   `crates/simd-scan` carries a retained-index probe API
   (`next_structural_at_or_after` + `OnceCell<StructuralIndex>`) absent from
   skinny `bbnf-simd`. This is the renamed/parallel-scanner check the lens
   requires; the V5 packet performs it for skinny only.
4. **F-CH5-V1-04 — correct the `attach_structural_index` symbol claim.** Fix
   1A-SUB-017: the skinny `attach_structural_index` is a no-op stub; the capacity
   scan is `scan.rs::scan_structurals`. The clean-substrate conclusion holds.
5. **F-CH5-V1-05 — preserve the caught couplings.** Keep the `RuntimeEmitterKind`
   fork, the `PRODUCTION_MANIFEST_TABLE` ir-crate leak, and the
   `same-plane-source-sidecar` comparator fence exactly as catalogued; these are
   the correctly-caught live couplings and must not regress to "wired/clean".

## Non-Findings

- No NEW *cross-call* retained classifier state proven this pass in either tree.
  The totality `OnceCell<StructuralIndex>` is threaded `&mut ScanState` per parse
  (`crates/core/src/grammar/generated/json.rs:705 ScanState::new`), an admissible
  `generated_function` lifetime, NOT the REJECT `retained-across-call-boundary`
  class (`restart/locks/LOCKS.md:139`-`149`). The defect is the missing
  classification, not a proven Lock-1 violation.
- No second retained JSON document identity in skinny runtime. The skinny
  `StructuralIndex` (`scan.rs:22`) is a function return value consumed for
  capacity only; skinny carries ZERO `OnceCell<StructuralIndex>` (verified
  empty). 1A's transient-scan classification holds.
- The phantom `<G:EventGrammar>` axis is correctly classified as decoration, not
  a second substrate (`tape/mod.rs:175`,`:179`); the R-D DELETE-default is
  grounded and Lock-1-aligned.

TALLY accept=6 revise=4 reject=0
