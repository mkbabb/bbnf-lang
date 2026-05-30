# Restart Migration

This document is the Phase 2 per-file disposition contract for the greenfield
restart. It does not patch the current workspace in place. It tells tranche
authors which current modules are kept, moved, replaced, deleted, or archived,
and it names the gates that prove the migration did not carry forward stale
architecture.

## 0. Scope And Authority

The migration follows the resolved Phase 2 authority:

| Source | Migration consequence |
|---|---|
| README says onboarding is grammar source plus workspace metadata, without Rust crate or per-grammar match arms (`restart/README.md:11-25`). | Current grammar-name registries and runtime shims are not preserved as handwritten code. |
| README fixes the 24-crate workspace and crate naming (`restart/README.md:29-60`). | Existing `core`, `analysis`, `lsp`, `bbnf-path`, `bbnf-path-ts`, `bootstrap`, `ser`, and `gorgeous` do not carry over as-is. |
| Lock 1 says tape is the substrate unioned with direct-to-struct (`restart/locks/LOCKS.md:48`). | Old anti-tape notes and ParseStream language are migration conflicts, not goals. |
| Lock 5 says lowerers consume Backend IR, not grammar source (`restart/locks/LOCKS.md:113`). | Current backend walkers are mined for behavior and replaced at the architecture boundary. |
| Lock 13 sets tree and LOC discipline (`restart/locks/LOCKS.md:207`). | Current god modules are split or replaced. |
| Lock 14 forbids grammar switches/types/modules/features in generic crates (`restart/locks/LOCKS.md:220`). | Current hardcoded grammar tables become deletion gates. |
| PASS-2 says codegen/runtime wiring must be replaced, not patched (`restart/audit/pass-2-codegen/PASS-2.md:5-8`). | Migration is a greenfield rebuild with mined implementation knowledge. |

The current repository contains 13 Rust crate directories under `crates/`.
The older module corpus counted 824 Rust files and 21/23 oversized handwritten
files after generated exemptions (`restart/corpora/MODULES.md:1295-1303`).
This synthesis inventory counted 834 current Rust files with `find crates -name
'*.rs' -type f`, so tranche gates must use the live count, not only the prior
corpus.

## 0.0 Current SK-V17 Tape-Fold Migration Receiver

Pass Omega V5 SK-V17 tape-fold G-Omega CLOSED 2026-05-30 by explicit user
authorization. This is the current implementation migration authority; the
SK-V15 V9 receiver (§0.1) and the historical Pass Omega V2..V8 receivers below
are HISTORICAL SK-V15 lineage, not current dispatch authority. SK-V16 closed at
`1c5bd7a25` (shared flat-tape SUBSTRATE landed, UNWIRED for CSS). SK-V17 (the
SKINNY tape-fold **contract** for CSS-on-tape / lazy-`ValueRef` / shared-NEON;
**JSON >SOTA-proven** at `skinny/RESULTS.md`, **CSS >SOTA the SK-V18 proof
obligation, bar NOT yet met** per `restart/skinny/tranches/sk-v17/HANDOFF.md:44-45`
and `restart/skinny/tranches/sk-v17/SPEC.md:207`) S-P3 CONVERGED. The CRUD-3
LOCKS leg landed the SK-V17 T-P3 Crystallisation Addendum on Locks 1/2/10/14/16
at `7157be073` (`restart/locks/LOCKS.md:610`-`618`; 16-lock count preserved,
five-shape canon verbatim, tape recorded as substrate-manifest CATEGORY per the
LAC-1E-14 FactStream precedent — NOT a 6th `BackendShape`).

SK-V17 skinny waves W0-W5 are dispatchable under the SKINNY triumvirate; SK-V18
is the totality implementation tranche that adopts the SKINNY-proven
unified-tape / lazy-view / shared-NEON model into `crates/core/`. The five
LOCKED fold designs (T-P2 LAC-2F-FOLD-01..05,
`restart/audit/totality/sk-v17/p2/hardening/HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:77`-`221`)
are the receiver set. Source `^0.0 SK-V18 Tape-Fold Migration Receiver` is the
3F17-MH-01/02 delta (`restart/audit/totality/sk-v17/p3/3f-migration-handoff.md:67`-`68`,
`:84`-`102`).

This is a document-authority migration leg. It does not authorize source,
generated output, gate implementation, `skinny/RESULTS.md`, `skinny/REDRESS.md`,
or SK-V17 SPEC/DISPATCH edits. Those remain blocked until the owning SK-V17
wave dispatches through the skinny triumvirate, and no SK-V18 implementation
wave dispatches until G-Omega has authorized the required V1 patches (this leg).

| Receiver (LAC) | Migration rule (crates/core) | Blocker | Gate |
|---|---|---|---|
| LAC-2F-FOLD-01/02 tape-as-unified-substrate | Retire eager `OpenFrame` builders (`json/builder.rs`, `css_l4/builder.rs:16`); converge AoS `TapeRec` (`tape/record.rs:103`) onto the proven SoA `Tape` as the SINGLE surviving encoding; all-8 `OnceCell<StructuralIndex>` declare `substrate_target` before wiring. | a committed AoS/SoA dual end-state; a sidecar index. | §19.4 substrate gate + Lock 1 (`restart/locks/LOCKS.md:75`). |
| LAC-2F-FOLD-03 lazy `ValueRef<G>` value-API | One `BackendRule`/`FieldSource`-walking projection generator emits `document/value/view/visitor` over the EXISTING `Tape`/`ValueRef`; `@generated`-allowed; resolved once at codegen. JSON+CSS exercised; Sheets/BBNF-self by-construction (SK-V18). | per-grammar eager value enums kept as the live plane; a per-leaf registry walk. | §19.5 generated-equality + Lock 14. |
| LAC-2F-FOLD-03 shared NEON classifier | Register `select_classifier(alphabet)` as a Lock-16 primitive-manifest ROW (alphabet-as-data); `substrate_target=existing_tape`, transient-single-call; scalar-ref + checkasm parity; aarch64-only NEON. | x86/AVX-512/SVE close path; a cross-call classifier-state carry. | Lock 16 manifest + §9.3 simd-scan scope reconcile. |
| LAC-2F-FOLD-02 BackendShape disposition | The tape is the substrate the 5 shapes project from — a substrate-manifest CATEGORY at the Lock-1 manifest, NOT a 6th `BackendShape` variant; the 5-shape canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` holds verbatim. | a silent 6th shape; an aarch64 CollapsedStage admission without G-Omega. | Lock 1 manifest + Lock 10 5-shape domain (`restart/locks/LOCKS.md:107`-`109`). |
| LAC-2F-FOLD-04 StructRegistry/FieldSource fence | ANY per-leaf runtime `StructRegistry::layout(rule)` / `compound_kind_for_layout` indirection in the tape/projection hot path is REJECT (re-opens the measured 28-65× / 983× / 10583× regression); the FieldSource walk is compile-time-resolved-once. Live coupling at `bbnf/arena.rs:47` is severed by eager-builder retirement. | a per-leaf registry lookup re-entering the hot path. | §19.4 substrate gate + AZ-IV pre-block (SPEC `:791-794`). |
| LAC-2F-FOLD-05 (Lock-2 sub-surface) StructLayout→Layout | The Lock-2-retired `StructLayout` (960 live sites in `crates/`) renames to `Layout` GENERATOR-SIDE, regenerating all 8 parsers + ~16 tests; regen-gated, NEVER hand-patched. Price the rename as the 960-site generator surface, not 40-120 LOC. | a hand-patch outside the generator (clean-regen violation). | §19.5 generated-equality + Lock 2 (`restart/locks/LOCKS.md:160`). |

Migration fences (binding on every SK-V18 row; source 3F17-MH-03/05/06):
- EXACTLY ONE tape encoding survives post-fold; the AoS/SoA dual is admissible
  ONLY as a transient fold-state, never a Lock-1 closure. `grep` proves AoS
  `TapeRec` retired OR SoA `Tape` retired, never both live (§19.4 + Lock 1,
  `restart/locks/LOCKS.md:75`).
- No second substrate: the projection generator emits over the EXISTING
  `Tape`/`ValueRef`; an introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor`
  ALONGSIDE the proven `Tape`/`ValueRef` is a Lock-1 type-ambivalence REJECT. The
  shared NEON classifier carries `substrate_target = existing_tape` /
  `retention_lifetime = transient-single-call`; no sidecar mask producer, no
  parallel source pass, no sixth BackendShape.
- No per-leaf `StructRegistry` indirection in the tape/projection hot path
  (AZ-IV pre-block, SPEC `:791-794`); the FieldSource walk is compile-time
  projection-emission resolved ONCE at codegen.
- aarch64 NEON + optional dotprod/i8mm only; no x86/AVX-512/SVE close path.
- No fact-stream String as a live CSS admission plane (diagnostic-only).

## 0.1 Historical SK-V15 V9 Migration Receiver (not current authority)

Pass Omega V9 G-Omega was authorized and V9 CRUD applied for the SK-V15
PRUNE-then-REBUILD lineage. This receiver is HISTORICAL after the current
SK-V17 tape-fold receiver (§0.0) above. The historical implementation route
was SK-V15 W0-W11 through `restart/skinny/tranches/sk-v15/SPEC.md` and
`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`.

V9 CRUD is a document-authority migration. It does not authorize source,
generated output, gate implementation, `skinny/RESULTS.md`, `skinny/REDRESS.md`,
or SK-V15 SPEC/DISPATCH edits. Those remain blocked until the owning SK-V15
wave dispatches through the skinny triumvirate.

| Receiver | Migration rule | Blocker | Gate |
|---|---|---|---|
| Current SK-V15 authority | SK-V15 W0-W11 supersedes stale SK-V14 W5B/Omega V8 dispatch text as the current implementation route. | Any current-authority text still routing next dispatch through SK-V14 W5B. | G-Omega V9 plus CRUD-4. |
| W0 baseline/telemetry | Capture `SK-V15-open`; JSON 51/51 guard remains strict; CSS broadcast evidence is diagnostic only. | Missing gate-consumed SK-V15 telemetry. | W0 exit gate. |
| W1 CSS honesty | Demote or collapse the 24-row CSS broadcast; no live CSS admit from the W8R shared tuple. | Reused broadcast measurement as admit evidence. | `DEP-W1-CSS-BROADCAST`. |
| W2 Lock 14 / Lock 16 restoration | Gates scan previously excluded roots, report their exclusions, and classify source-present primitives. | Self-exempting scan or source-present primitive with no status. | W2 exit gate. |
| W3 codegen leaks | Remove one coherent generic leak family with same-wave generator/check consumer and non-JSON receiver proof. | Grammar-family branch, profile roster, or JSON/CSS recognizer left in generic path. | `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`. |
| W4 Pattern H provenance | Keep Pattern H at 67 root runtime files while proving true generated provenance at line 1 with regen/check evidence. | Header-only generated claim or destructive delete without provider proof. | `DEP-W4-PATTERN-H-PROVENANCE`. |
| W5 CSS typed provider | Build typed CSS value/document/view/visitor capability comparable to JSON; old CSS proof remains diagnostic. | Fact-stream-only or brace-counter path treated as live proof. | W5 exit gate. |
| W6 CSS retime and old-proof retirement | Retime against same-workload typed `cssparser`; retire `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`, and brace-counter proof from live admission. | CSS floor based on W8R, lightningcss wrong-plane proof, or missing typed comparator. | `DEP-W6-CSS-GENERATED-RS` and `DEP-W6-CSS-SUMMARY-FACT-STREAM`. |
| W7 Decision Engine spine | E-graph rewrite count is nonzero, CSP is non-tautological, and grammar-named facts are removed. | Zero-rule e-graph, marker CSP, or advisory-only cost facts. | `DEP-W7-DECISION-SPINE`. |
| W8/W9 BackendShape lowerers | EagerTape, OffsetTape, EventTape, SinkOnly, and CollapsedStage lowerers are real or gate-rejected; exactly five shapes remain. | Label-string lowerer, pass-through scaffold, sixth shape, sidecar EventTape. | `DEP-W8-LOWERERS-A` and `DEP-W9-LOWERERS-B`. |
| W10 FNV quarantine | FNV closed-enum products stay bench-only and cannot migrate into production correctness proof. | Production FNV arbiter, production hash correctness proof, or runtime leakage. | `DEP-W10-FNV-QUARANTINE`. |
| W11 close | PASS-IMPL V2 consumes every dependency row and accepts every axis or records row-level intrinsic-block proof. | Any orphan dependency row, doc-only close, or implementation-limited miss. | `DEP-W11-CLOSE-NO-ORPHANS`. |

Migration gate clause: no delete, retirement, provider/template removal,
old CSS proof retirement, runtime-shim deletion, primitive promotion, SIMD/ASM
admission, or lowerer close may proceed unless the SK-V15 dependency row proves
the rebuild provider lands no later than the delete/retire wave, or the row is
explicitly diagnostic-demotion-only.

## 0.2 Historical Pass Omega V2 Migration Receiver

Pass Omega V2 updates the migration receiver per the T-P3 V4 LOCK
packet (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V4-CONSOLIDATED.md`).
G-Omega CLOSED 2026-05-24 by explicit user sign-off; LOCKS amendments
already merged at CRUD-3 commit `85a043224`. This receiver records the
migration-side updates that fold the 7 3F-MIG deltas applied by CRUD-4.
Source, generated output, gate, `RESULTS.md`, and `REDRESS.md` changes
still require the skinny/totality wave that owns them — V2 carry-forward
binds those waves to SK-V14 PRUNE-3/4/5 receivers (W5/W6/W7).

| Receiver | V2 migration rule |
|---|---|
| Legacy crate fates (3F-MIG-001) | `ser` and `gorgeous` remain archive-only with archive-proof required (Lock 11/12/14/16; LAC-1E-06 sustained at `restart/audit/totality/p1/1E-locks-evidence.md:116`). `bbnf-path-ts` remains deferred unless a V2 manifest proves the backend surface is generated from shared `path-core` facts. The old `simd-scan` source renames to `bbnf-simd` per Lock 14 + Lock 16 — rename is the primitive-boundary opening, NOT Lock 16 closure. Lock 16 closure additionally requires LAC-1E-10 traceability manifest mapping every intrinsic/`asm!` use to allowlist row + scalar parity + corpus parity + same-wave consumer (`restart/audit/totality/p1/1E-locks-evidence.md:120`). |
| Generated-provider roster (3F-MIG-002) | Hardcoded `RuntimeProvider` enum + 8 hardcoded match arms + 30 grammar-parser-name leaks across 15 files (`crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/{parse_with,mod,document,builder,serialize}.rs`; ~190 LOC + 2.5× consumer-rewire band per CH2 V2 mechanical extraction) are ABROGATE-REPLACE; receiver is **SK-V14 W5A/W5B.0..W5B.4/W5C-GEN/W5D-DELETE PRUNE-3**. W5A proves the grammar-neutral source-consuming request boundary per `restart/skinny/tranches/sk-v14/SPEC.md §8`; W5B-FRONTEND closes generic BBNF frontend/import/IR lowering as W5B.0 LOCK14-GATE, W5B.1 IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3 PRETTY-SPAN-PROJECTION, and W5B.4 REQUEST-CONSUMER per SPEC §8B; W5C-GEN replaces live provider-backed runtime generation with a provider-free generator body per SPEC §8C; W5D-DELETE deletes the seven CSS provider modules and seven template directories only after W5C-GEN is load-bearing per SPEC §8D. Per `[no-backward-compat]`, the `dispatch_value` enum arm renames to `dispatch` trait method in the owning replacement/deletion wave; consumers update without alias period. |
| Per-grammar runtime roots + Pattern H = 67 (3F-MIG-003) | **Pattern H = 67 hand-written per-grammar runtime files** across 9 dirs under `crates/core/src/runtime/{bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math}/` (+3 vs V13 baseline 64 from css_pretty addition; LAC-1E-15 census at `restart/audit/totality/p1/1E-locks-evidence.md:102` D-1E-15) are ABROGATE-REPLACE; receiver is **SK-V14 W6 PRUNE-4 with 9 sub-waves NOT 8** per S-P0 §2.3. Pass Omega V3 W2R assigns `crates/core/src/runtime/css_l4/` to W6.0 after W5D-DELETE; W2 owns only skinny-side `regen-css` output. Substrate templates `builder_template.rs:13-31` + `arena_template.rs:1-31` opt-out doc-comments are themselves Lock 14 violations per LAC-1E-15 — substrate-doc cleanup is a PRUNE-4 sub-task. Per-tranche Pattern H census via `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` cited at every wave commit. |
| Non-JSON telemetry + FactStream 5th SUBSTRATE (3F-MIG-004 / LAC-1E-14) | CSS L4 declaration-values is admitted same-plane fact-stream row evidence (`skinny/RESULTS.md:94`) but lacks formal runtime substrate category alongside `OffsetTape`/`EventTape`/`SinkOnly`/`CollapsedStage`. Per 3C V1 ACCEPT at `restart/audit/totality/p3/3C-locks-crystallisation.md:32` (3C-L01-factstream-fifth-category) and 3C V4-3 hunk verbatim at `restart/audit/totality/p3/3C-locks-v+1-diff.md:118`-`140`, **LAC-1E-14 lands `FactStream` as the 5th admitted-product category at the Lock 1 SUBSTRATE manifest** (alongside `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`); a fact-stream row carries `substrate_target = admitted_fact_output`. The 5th category is a substrate-manifest classification only; it is **NOT a 6th `BackendShape` variant**. The 5-shape `BackendShape` search domain at Lock 10 — `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` — HOLDS. The two axes (Lock 1 substrate manifest vs Lock 10 BackendShape search domain) are ORTHOGONAL; LAC-1E-14 touches the manifest axis only. Any 6th `BackendShape` variant remains G-Omega gated per Lock 10 v+1 + PASS-3 §8.1. MIGRATION binds CSS L4 row to fenced telemetry with strict comparator provenance + gate-consumed telemetry per Lock 1 V+1 fact-stream wording (`restart/locks/LOCKS.md:100-116`). Doc-only delta with zero impl-tail at this MIGRATION row: W8 re-admit consumer-plane cost accounted at `3C-L01-factstream-fifth-category` (60-150 docs per `restart/audit/totality/p3/3C-locks-crystallisation.md:158`). |
| Decision engine (3F-MIG-005) | P1-P8 cascade + thin `CostFacts` + opaque regex programs + marker-string lowerers are ABROGATE-REPLACE; receiver is **SK-V14 W7 PRUNE-5** wiring W8 per-grammar policy SCAFFOLD + W9 same-substrate union SCAFFOLD to LOAD-BEARING (`restart/skinny/tranches/sk-v14/SPEC.md:779-838`). The W7 `same_substrate_union` module is an **ENFORCEMENT-LAYER pass** proving substrate-union compliance (every shape consumer reuses existing `Tape` substrate — zero new retained surface); it is **NOT the SK-V9 W3 retired retained-class-column-union data structure** (PERMANENT-PRE-BLOCK per REDRESS 96/97/98; `restart/skinny/tranches/sk-v14/SPEC.md:806`). Active cost extraction + eqsat + CSP + candidate generation + strict equivalence/cost evidence required before admission. |
| Primitive manifest (3F-MIG-006) | Every SIMD/ASM/hardware/table/mask/carry/source-present primitive must close as wired, deleted, scalar-delegated, or architectural-blocked with REDRESS evidence. Manifest fields required per LAC-1E-10: identity, source state, strict mode, first consumer, command, scalar fallback OR architectural block, LOC/risk, rollback path, abrogate threshold. Rename `simd-scan` → `bbnf-simd` is necessary but insufficient. Inventory demotion is not a close state. |
| dispatch_value → dispatch + SKELETON triple DELETE refusal (3F-MIG-007) | **`RuntimeProvider::dispatch_value` enum arm renames to trait-method `dispatch`** under W5C-GEN PRUNE-3C / W5D-DELETE PRUNE-3D after W5A's source-consuming request boundary and W5B-FRONTEND IR closure are load-bearing (8 hardcoded match arms; `skinny/crates/codegen/src/lib.rs:167-209`); rename is migration-full per `[no-backward-compat]`. **SKELETON triple DELETE proposal** (FSM_DISPATCH_THREADED + FRAME_PUSH_BOUNDED + FRAME_POP_BOUNDED) is **REJECTED** per T-P2 V3 LOCK cohort **refutation density 32:69 = 31.7%** (1:2 anti-paper-close pattern; canonical T-P2 V3 figure at `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:76,172,187,295`); the three primitives remain non-shortlist-blocker support under SRC-V2-FOLD. Re-proposal requires same-wave consumer + first consumer path + executable command per T-P2 V4 non-shortlist criteria. |
| LAC-1E-12 preface promotion | LAC-1E-12 (CH7 Overfit-Prune binding) is **promoted to LOCKS preface, NOT Lock 17** — the 16-lock count is preserved per `restart/prompts/totality/PASS-3-SYNTHESIS.md:210`. Live verify: `grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md` → 16. |
| LAC-2F-V5-02 substrate-union ELEVATION | Lock 1 v+1 substrate-union ELEVATION (`restart/locks/LOCKS.md:137-158`): no cross-call retained classifier state. Quote-mask, escape-mask, structural-mask, class-stream, prev-state byte, prefix-XOR carry word, or any prefix carry of any kind — none is admissible under Lock 1 substrate-union. Carry MUST stay within a single chunk-call boundary. REDRESS 96/97/98 generalises to ALL transient classifier-state primitives. The third value `retained-across-call-boundary` in `retention_lifetime` is the REJECT class under Lock 1 v+1. |
| Proposal boundary | Pass Omega CRUD artefacts at `restart/audit/totality/astral/V2/` are application logs; they record CRUD operations under G-Omega authorization (closed 2026-05-24). They do not authorize source, generated, gate, `RESULTS.md`, or `REDRESS.md` edits without the owning wave-triumvirate dispatch. This receiver is historical after the current SK-V15 V9 receiver above. |

## 0.3 Historical Pass Omega V3 W2R Migration Receiver

Pass Omega V3 W2R consumes REDRESS-183 and the W2R corrective packet. It
changes the SK-V14 wave graph only. It does not amend LOCKS, architecture,
source, generated output, gates, `RESULTS.md`, or `REDRESS.md` beyond the
already-landed W2 rejection record.

| Receiver | V3 W2R migration rule |
|---|---|
| REDRESS-183 / W2 rejection | `G-W2-FULL-ROUNDTRIP` under `G-SK-V14-W2-R4` is REJECTED. The original W2 required `regen-css` to restore both skinny CSS L4 runtime profiles and `crates/core/src/runtime/css_l4/`; no current generator restores the core runtime Pattern H tree. This block is historical after Pass Omega V3 W2R and the amended W2 admission at `45568e669`. |
| W2 amended receiver | After G-Omega V3 + CRUD, rerun W2 under a skinny-only `regen-css` gate: emit the existing CSS L4 runtime profile directories under `skinny/crates/runtime/src/grammars/css_l4_*`; run the seven exact `check-css-l4-*` companions; run the skinny-only destructive round-trip; preserve the bypass-header detector across skinny and root runtime trees. W2 may not move CSS SOTA rows, touch `crates/core/src/runtime/css_l4/`, or claim Pattern H closure. |
| Core-runtime CSS L4 receiver | `crates/core/src/runtime/css_l4/` remains Pattern H runtime-root work. It moves to W6.0 after W5D-DELETE closes over W5C-GEN's provider-free generator body. W6.0 emits or collapses the CSS L4 root-runtime tree from grammar source + workspace metadata, then passes the destructive root-runtime round-trip for that tree. |
| Dispatch block | Historical for W2R. W2 admitted at `45568e669` and W3 admitted at `b0a864f0b`; the current block is REDRESS-184 / W4R until Pass Omega V4 CRUD applies. W8/W9/W10 remain globally blocked until PRUNE-1..PRUNE-5 close. Stale notes saying W5/W6/W7/W9/W10 may proceed independently after W2 or W4 rejection are non-controlling; hard entry gates, REDRESS-183/184, and the PRUNE-before-new-admit chain control. |

## 0.4 Historical Pass Omega V4 W4R Migration Receiver

Pass Omega V4 W4R consumes REDRESS-184 and the W4R corrective packet. It
changes the SK-V14 W4/W5 wave graph only. It does not amend LOCKS,
architecture, source, generated output, gates, `RESULTS.md`, or `REDRESS.md`
beyond the already-landed W4 rejection record and the REDRESS-183 supersession
note.

| Receiver | V4 W4R migration rule |
|---|---|
| REDRESS-184 / W4 rejection | `G-SK-V14-W4-PRUNE-2` is REJECTED under the original W4 shape. Deleting CSS provider modules before W5's replacement provider path exists removes the live `regen-css` emitter and fails compilation before regeneration can run. |
| W4 amended receiver | After G-Omega V4 + CRUD, rerun W4 as CSS L4 admit-ledger PRUNE only: restore `restart/skinny/ROLLING-SOTA-DELTA.md` to 0/24 CSS L4 admitted, add 24 row-keyed REDRESS entries citing `restart/skinny/tranches/sk-v13/audit-overfit/validation/v1-css-l4-validation.md` §1-6, preserve `skinny/RESULTS.md` AUDIT-FALSIFIED state, maintain JSON rows within +/-1.0%, and do not delete CSS source, providers, templates, runtime twins, or generator code in W4. |
| W5 amended receiver | Historical after V8 W5B-FRONTENDR. V4 moved CSS provider/template deletion out of W4; V5 split that receiver into W5A source-consuming request boundary and W5B deletion; V6 superseded W5B into W5B-GEN plus W5C-DELETE; V7 split that into W5B-FRONTEND, W5C-GEN, and W5D-DELETE; V8 split W5B-FRONTEND into W5B.0 LOCK14-GATE, W5B.1 IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3 PRETTY-SPAN-PROJECTION, and W5B.4 REQUEST-CONSUMER. |
| Core-runtime CSS L4 receiver | Unchanged from V3 W2R except sequencing: `crates/core/src/runtime/css_l4/` remains W6.0 after W5D-DELETE closes over W5C-GEN's provider-free generator body. |
| Dispatch block | Historical for W4R after W4 admission. W5A admitted at `286233fa2`; the V8 dispatch route was W5B.0 LOCK14-GATE after V8 CRUD; W5B.1..W5B.4 remained blocked until preceding W5B sub-waves admitted; W5C-GEN remained blocked until aggregate W5B-FRONTEND closed; W5D-DELETE remained blocked until W5C-GEN closed; W6 remained blocked until W5D-DELETE closed; W8/W9/W10 remained globally blocked until PRUNE-1..PRUNE-5 close. |

## 0.5 Historical Pass Omega V5 W5R Migration Receiver

Pass Omega V5 W5R consumes REDRESS-209 and the W5R corrective packet. It
changes the SK-V14 W5 wave graph only. It does not amend LOCKS, architecture,
source, generated output, gates, `RESULTS.md`, or `REDRESS.md` beyond the
REDRESS-209 supersession note.

| Receiver | V5 W5R migration rule |
|---|---|
| REDRESS-209 / W5 rejection | `G-SK-V14-W5-PRUNE-3` is REJECTED under the original W5 shape. Current `regen-css` emits through static providers/templates while grammar source and metadata are freshness-only inputs, and the parser rejects CSS L4 constructs needed by runtime generation. Provider/template deletion before a source-consuming replacement would sever the live emitter. |
| W5A amended receiver | Dispatch W5A as PRUNE-3A: pass grammar source + workspace metadata into codegen; make required V1 grammar-source constructs parseable without grammar-id branches; migrate `regen-css`; prove all seven CSS L4 companions plus JSON unchanged-output and Sheets/BBNF-self fail-closed or generated-role witnesses; do not delete provider/template surfaces. |
| W5B amended receiver | Historical after V6 W5BR. V5 routed deletion to W5B only after W5A; REDRESS-210 rejects that shape and §0.6 supersedes it with W5B-GEN before W5C-DELETE. |
| Core-runtime CSS L4 receiver | Historical after V6 W5BR. `crates/core/src/runtime/css_l4/` remains W6.0 after W5C-DELETE, not after the rejected V5 W5B deletion gate. |
| Dispatch block | Historical after V7 W5B-GENR, superseded by V8 and then V9. W5A admitted at `286233fa2`; W5B.0 LOCK14-GATE was the V8 next dispatch before SK-V15 supersession; W5B.1..W5B.4 remained blocked until preceding W5B sub-waves admitted; W5C-GEN remained blocked until aggregate W5B-FRONTEND closed; W5D-DELETE remained blocked until W5C-GEN closed; W6 remained blocked until W5D-DELETE closed; W8/W9/W10 remained globally blocked until PRUNE-1..PRUNE-5 close. |

## 0.6 Historical Pass Omega V6 W5BR Migration Receiver

Pass Omega V6 W5BR consumes REDRESS-210 and the W5BR corrective packet. It
changes the SK-V14 W5B/W6 wave graph only. It does not amend LOCKS,
architecture, source, generated output, gates, `RESULTS.md`, or `REDRESS.md`
beyond the REDRESS-210 supersession note.

| Receiver | V6 W5BR migration rule |
|---|---|
| REDRESS-210 / W5B rejection | `G-SK-V14-W5B-PRUNE-3B` is REJECTED under the V5 W5B shape. W5A admitted a source-consuming request boundary at `286233fa2`, but live codegen still depends on `render_runtime_profile`, `RuntimeProvider`, `GrammarProfile`, and provider modules; deleting providers/templates before a provider-free generator body would sever the live emitter. |
| W5B-GEN amended receiver | Dispatch W5B-GEN as PRUNE-3B: replace live provider-backed runtime generation with one provider-free generator body consuming W5A's source request; prove CSS/JSON source+metadata emission; preserve Sheets/BBNF-self proof; run provider-reachability grep, `regen-css`, seven CSS companions, and `check-json`; do not delete provider/template surfaces. |
| W5C-DELETE amended receiver | Dispatch W5C-DELETE only after W5B-GEN closes: delete provider/template residue; retire old provider dispatch/registry/template surfaces; update the Lock 14 baseline; rerun `regen-css`, all seven companions, `check-json`, JSON unchanged-output proof, and Sheets/BBNF-self proof. |
| Core-runtime CSS L4 receiver | `crates/core/src/runtime/css_l4/` remains W6.0 after W5C-DELETE. W6.0 emits or collapses the CSS L4 root-runtime tree from grammar source + workspace metadata and passes the destructive root-runtime round-trip for that tree. |
| Dispatch block | Historical after V7 W5B-GENR. V6 unblocked W5B-GEN, but V7 supersedes that route. W5B-FRONTEND now unblocks after V7 CRUD; W5C-GEN remains blocked until W5B-FRONTEND close; W5D-DELETE remains blocked until W5C-GEN close; W6 remains blocked until W5D-DELETE close; W7 remains blocked until W6 close; W8/W9/W10 remain globally blocked until PRUNE-1..PRUNE-5 close. |

## 0.7 Historical Pass Omega V7 W5B-GENR Migration Receiver

Pass Omega V7 W5B-GENR consumes REDRESS-211 and the W5B-GENR corrective packet.
It changes the SK-V14 W5B/W5C/W5D/W6 wave graph only. It does not amend LOCKS,
architecture, source, generated output, gates, `RESULTS.md`, or `REDRESS.md`.

| Receiver | V7 W5B-GENR migration rule |
|---|---|
| REDRESS-211 / W5B-GEN rejection | `G-SK-V14-W5B-GEN-PRUNE-3B` is REJECTED under the V6 W5B-GEN shape. W5A admitted a source-consuming request boundary at `286233fa2`, but the generic BBNF frontend/import/IR closure was missing; CSS L4 compatibility constructs such as `@ws`, `@pretty`, `?w`, `>>`, `<<`, span capture, typed host projections, and imports must lower into canonical IR before a provider-free generator can replace the provider-backed path. |
| W5B-FRONTEND amended receiver | Dispatch W5B-FRONTEND as PRUNE-3B: close generic BBNF grammar-source frontend/import/IR lowering with CSS L4 as the strict positive witness; treat `@ws` as compatibility-lowering evidence, not public syntax; preserve JSON/Sheets/BBNF-self proof; do not replace the generator body or delete provider/template surfaces. Before source redress, add `SK_V14_W5B_FRONTEND_OWNER_PATHS`, parent-diff routing for `sk-v14-waveW5B-FRONTEND` / `sk-v14-waveW5B-FRONTEND-redress`, and a unit test in `skinny/crates/bbnf-bench/src/lock14_baseline.rs`. |
| W5C-GEN amended receiver | Dispatch W5C-GEN only after W5B-FRONTEND closes: replace live provider-backed runtime generation with one provider-free generator body consuming W5A request facts plus W5B-FRONTEND IR; prove CSS/JSON source+metadata emission; preserve Sheets/BBNF-self proof; run provider-reachability grep, `regen-css`, seven CSS companions, and `check-json`; do not delete provider/template surfaces. Before source redress, add `SK_V14_W5C_GEN_OWNER_PATHS`, parent-diff routing for `sk-v14-waveW5C-GEN` / `sk-v14-waveW5C-GEN-redress`, and a unit test in `lock14_baseline.rs`. |
| W5D-DELETE amended receiver | Dispatch W5D-DELETE only after W5C-GEN closes: delete provider/template residue; retire old provider dispatch/registry/template surfaces; close the Lock 14 baseline; rerun `regen-css`, all seven companions, `check-json`, JSON unchanged-output proof, and Sheets/BBNF-self proof. If W5C-GEN has not already made deletion paths executable in the Lock 14 gate, W5D-DELETE first adds its owner-path and parent-diff subject routing. |
| Core-runtime CSS L4 receiver | `crates/core/src/runtime/css_l4/` remains W6.0 after W5D-DELETE. W6.0 emits or collapses the CSS L4 root-runtime tree from grammar source + workspace metadata through W5B-FRONTEND IR and the W5C-GEN generator body, then passes the destructive root-runtime round-trip for that tree. |
| Dispatch block | W5B-FRONTEND unblocks after V7 CRUD. W5C-GEN remains blocked until W5B-FRONTEND close; W5D-DELETE remains blocked until W5C-GEN close; W6 remains blocked until W5D-DELETE close; W7 remains blocked until W6 close; W8/W9/W10 remain globally blocked until PRUNE-1..PRUNE-5 close. |

## 0.8 Historical Pass Omega V8 W5B-FRONTENDR Migration Receiver

Pass Omega V8 W5B-FRONTENDR consumed REDRESS-212 and the W5B-FRONTENDR
corrective packet. It changes the SK-V14 W5B/W5C/W5D/W6 wave graph only. It
does not amend LOCKS, architecture, source, generated output, gates,
`RESULTS.md`, or `REDRESS.md`. It is historical after the current SK-V15 V9
receiver above.

| Receiver | V8 W5B-FRONTENDR migration rule |
|---|---|
| REDRESS-212 / W5B-FRONTEND rejection | `G-SK-V14-W5B-FRONTEND-PRUNE-3B` is REJECTED under the V7 one-shot cap shape. W5A admitted a source-consuming request boundary and V7 correctly routed frontend/import/IR closure before provider-free generation, but the combined Lock 14 gate, import closure, layout/discard lowering, pretty/span/projection lowering, request consumer, and proof surface do not fit the one-wave cap honestly. |
| W5B-FRONTEND amended receiver | Dispatch W5B-FRONTEND as an aggregate PRUNE-3B sequence: W5B.0 LOCK14-GATE, W5B.1 IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3 PRETTY-SPAN-PROJECTION, and W5B.4 REQUEST-CONSUMER. Each W5B.N carries HARD CAP 30 min, commit-safe evidence at 27 min, halt at 30 min, and dedicated exact-test nonzero proof where applicable. |
| W5B.0 receiver | Add `SK_V14_W5B_FRONTEND_OWNER_PATHS`, parent-diff routing for `sk-v14-waveW5B-FRONTEND` / `sk-v14-waveW5B-FRONTEND-redress`, W5C/W5D subject rejection, modified-provider/template rejection, all-template guard, `grammar_provider.rs` exception, and generic owner-path leak census in `skinny/crates/bbnf-bench/src/lock14_baseline.rs`. No grammar/codegen/xtask frontend source edits in W5B.0. |
| W5B.1..W5B.4 receiver | Close import DAG resolution, layout/discard lowering, pretty/span/projection lowering, and request-consumer wiring in order. Every construct row carries owner file/type, target representation, exact positive test, and exact fail-closed test. |
| W5C-GEN amended receiver | Dispatch W5C-GEN only after aggregate W5B-FRONTEND closes. W5B.0 through W5B.3 are not aggregate close and cannot unblock provider-free generator replacement. |
| Core-runtime CSS L4 receiver | Unchanged from V7: `crates/core/src/runtime/css_l4/` remains W6.0 after W5D-DELETE closes over W5C-GEN's provider-free generator body. |
| Dispatch block | W5B.0 LOCK14-GATE unblocks after V8 CRUD. W5B.1..W5B.4 run in order. W5C-GEN remains blocked until aggregate W5B-FRONTEND closes; W5D-DELETE remains blocked until W5C-GEN closes; W6 remains blocked until W5D-DELETE closes; W7 remains blocked until W6 closes; W8/W9/W10 remain globally blocked until PRUNE-1..PRUNE-5 close. |

## 1. Disposition Alphabet

| Fate | Meaning |
|---|---|
| KEEP-OUTRIGHT | Carry the file/module with only path/name updates and normal formatting. |
| KEEP-MODIFY | Preserve the implementation idea, but adapt API boundaries, names, tests, or ownership. |
| ABROGATE-MOVE | Move the code to a new crate/module and delete the old path. |
| ABROGATE-REPLACE | Mine behavior and tests, then implement the new architecture instead of carrying the code body. |
| ABROGATE-DELETE | Delete from production without replacement. Archive only if useful for reference. |
| GENERATED-REPLACE | Replace generated source with new template output and equality/budget gates. |
| ARCHIVE | Move out of production workspace before tranche A.W0. |

## 2. Aggregate Disposition

The current 834-file inventory is disposed as follows. These are migration
planning counts, not a promise that line counts survive exactly.

| Fate | Files | Main owners | Net effect |
|---|---:|---|---|
| KEEP-OUTRIGHT | 121 | `bbnf-simd` (renamed from legacy `simd-scan`; primitive boundary per Lock 14 + Lock 16), generic pieces of `csp-solver`, generic pieces of `egraph`, fixtures/tests that remain useful. | Keeps proven generic code. |
| KEEP-MODIFY | 224 | `ir` concepts, analysis diagnostics, path parser pieces, CSP/egraph integrations, selected runtime helpers. | Updates ownership and contracts. |
| ABROGATE-MOVE | 96 | Source/span/import modules, VM utilities, LSP document logic, CLI/debug helpers. | Moves to new crate tree. |
| ABROGATE-REPLACE | 315 | `core` backend walkers, old lowering, current runtime strategy, path registries, grammar-specific shims. | Rebuilds around Backend IR and tape/direct. |
| ABROGATE-DELETE | 78 | Stale serialize paths, hardcoded registries, old fallback/legacy paths, dead adapters. | Removes old architecture. |
| GENERATED-REPLACE | Included above | Generated parsers and per-grammar runtime files. | New template output under `runtime/src/grammars/<name>`. |
| ARCHIVE | Included above | `ser`, `gorgeous`, legacy tranche docs. | Kept for reference, not production. |
| Total | 834 | Workspace-wide | Matches current synthesis file inventory. |

The important migration fact is not the exact file count. It is the direction:
generic solver/scanner/egraph pieces survive; grammar-name runtime and backend
plumbing are replaced; old archive crates leave the production workspace.

## 3. Current Crates To Restart Crates

| Current crate | Restart fate | Restart destination |
|---|---|---|
| `crates/core` | ABROGATE-REPLACE plus selective ABROGATE-MOVE. | Split across `grammar`, `source`, `pipeline`, `passes`, `codegen`, `runtime`, `host`, `bbnf`, `bbnf-cli`, `bbnf-bench`. |
| `crates/ir` | KEEP-MODIFY plus ABROGATE-MOVE. | `ir`, `passes`, `vm`, `cost-model`, and bridge modules. |
| `crates/analysis` | ABROGATE-MOVE/REPLACE. | `bbnf-language-server`, `error`, `source`, `grammar`, `pipeline`. |
| `crates/lsp` | ABROGATE-MOVE/REPLACE. | `bbnf-language-server`. |
| `crates/bbnf-path` | KEEP-MODIFY/REPLACE. | `path` plus `path-core`. |
| `crates/bbnf-path-ts` | ABROGATE-MOVE deferred to V2. | TS surface defers post-V1 alongside the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5; the legacy `crates/bbnf-path-ts` source remains in the workspace as a deferred-V2 placeholder (Lock 12 archives only `ser` and `gorgeous` at A.W0) and is reconstituted as `path-ts` in V2. The `path-core` extraction (Rust-line) lands in V1 from `crates/bbnf-path`. |
| `crates/csp-solver` | KEEP-MODIFY. | `csp-solver`; generic API remains. |
| `crates/egraph` | KEEP-MODIFY. | `egraph`; bridge logic moves to `passes`. |
| `crates/egraph-derive` | KEEP-MODIFY. | `egraph-derive`. |
| `crates/simd-scan` → **`crates/bbnf-simd`** | KEEP-OUTRIGHT/KEEP-MODIFY (rename per Lock 14 + Lock 16; primitive boundary, no JSON-specific code). | `bbnf-simd`. |
| `crates/bootstrap` | ABROGATE-REPLACE. | `bbnf-cli`, `pipeline`, bootstrap fixtures. |
| `crates/ser` | ARCHIVE. | `restart-archive`/legacy reference only. |
| `crates/gorgeous` | ARCHIVE. | `restart-archive`/legacy reference only. |

The corpus already classifies `ser` and `gorgeous` as archive-only
(`restart/corpora/MODULES.md:165-212`), and Lock 12 requires that archive before
implementation starts (`restart/locks/LOCKS.md:199`).

### 3.1 Current Inventory By Crate

This synthesis counted the current crate tree before writing the migration
document. The prior corpus count remains cited evidence; the live count is the
working target for tranche A.

| Current crate | Rust files | Current LOC | Primary fate |
|---|---:|---:|---|
| `crates/analysis` | 46 | 5,241 | Consolidate into `bbnf-language-server`, `error`, `source`. |
| `crates/bbnf-path` | 3 | 918 | Split into `path` and `path-core`. |
| `crates/bbnf-path-ts` | 6 | 1,280 | Split into `path-ts` and `path-core`. |
| `crates/bootstrap` | 4 | 465 | Replace with CLI/pipeline bootstrap commands. |
| `crates/core` | 432 | 248,077 | Split, replace, and regenerate. |
| `crates/csp-solver` | 50 | 9,686 | Keep generic core, split oversized files. |
| `crates/egraph` | 18 | 2,762 | Keep generic core, move BBNF bridge. |
| `crates/egraph-derive` | 1 | 343 | Keep with egraph. |
| `crates/gorgeous` | 17 | 1,441 | Archive. |
| `crates/ir` | 224 | 51,957 | Mine and reorganize into IR/passes/vm/cost. |
| `crates/lsp` | 13 | 4,123 | Consolidate into `bbnf-language-server`. |
| `crates/ser` | 5 | 530 | Archive. |
| `crates/simd-scan` → **`crates/bbnf-simd`** | 15 | 3,389 | Keep and wire to BIR; rename to `bbnf-simd` per Lock 14/16 (grammar-neutral primitive boundary). |
| Total | 834 | 330,212 | Live file count target; LOC includes generated code. |

The exact current LOC total is not a planning invariant because generated files
dominate `core`. The migration invariant is fate by directory and gate, not
preserving current LOC.

#### 3.1.1 Mixed-Fate Crosswalk

Every current crate that distributes files across more than one fate bucket
appears below. The crosswalk audits the 834-file disposition by family rather
than by individual file; the per-family row counts must match the live tree
when tranche A starts.

| Current crate | Mixed | Family bucket | File count (current) | New location | Owner tranche |
|---|---|---|---:|---|---|
| `crates/analysis` | yes | Diagnostics + report helpers. | ~14. | `error/`, `bbnf-language-server/diagnostics/`. | A/I. |
| `crates/analysis` | yes | Semantic index. | ~18. | `bbnf-language-server/semantic/`. | I. |
| `crates/analysis` | yes | Document snapshot/edit helpers. | ~10. | `source/snapshot/`, `bbnf-language-server/document/`. | A/I. |
| `crates/analysis` | yes | Grammar-specific assumptions. | ~4. | none (ABROGATE-DELETE). | A. |
| `crates/bbnf-path` | yes | Macro entrypoint. | ~1. | `path/src/macro_impl/`. | G. |
| `crates/bbnf-path` | yes | Parser/evaluator logic. | ~2. | `path-core/src/parse/`, `path-core/src/eval/`. | G. |
| `crates/bbnf-path-ts` | yes | TS emitter/schema. | ~3. | V2 `path-ts/src/schema/`, `path-ts/src/emit/`. | V2. |
| `crates/bbnf-path-ts` | yes | Hardcoded grammar registries. | ~1. | none (ABROGATE-DELETE). | A/G. |
| `crates/bbnf-path-ts` | yes | Fixture duplicates. | ~2. | `test-fixtures/`. | A. |
| `crates/bootstrap` | yes | IR dump/debug commands. | ~2. | `bbnf-cli/debug/`, `vm/debug/`. | E. |
| `crates/bootstrap` | yes | Bootstrap parse command. | ~1. | `grammar/bootstrap/`, `pipeline/`. | A. |
| `crates/bootstrap` | yes | Standalone crate shell. | ~1. | none (ABROGATE-DELETE). | A. |
| `crates/core` | yes | Generated grammars. | ~9 (one per seed grammar). | `runtime/src/grammars/<name>/generated.rs`. | F. |
| `crates/core` | yes | Generated registry JSON. | 1. | none (ABROGATE-DELETE). | A. |
| `crates/core` | yes | Grammar AST/parser helpers. | ~80. | `grammar/src/*`. | A/D. |
| `crates/core` | yes | Imports, source maps, spans. | ~40. | `source/src/*`. | A. |
| `crates/core` | yes | Lower / normalization. | ~30. | `passes/`, `ir/`, `codegen/`. | C/E/F. |
| `crates/core` | yes | Backend walkers (`backend/**`). | ~80. | `codegen/src/*`. | E/F. |
| `crates/core` | yes | Runtime support. | ~30. | `runtime/src/document/`, `runtime/src/support/`. | B. |
| `crates/core` | yes | Per-grammar runtime modules. | ~120. | `runtime/src/grammars/<name>/**` (GENERATED-REPLACE). | F. |
| `crates/core` | yes | Path executor. | ~5. | `path-core/`, `runtime/`. | G. |
| `crates/core` | yes | CSS types and host shims. | ~10. | `host/`, metadata, generated `host.rs`. | D/F. |
| `crates/core` | yes | Generate/serialize. | ~5. | none (ABROGATE-DELETE; `ser` archive). | A. |
| `crates/core` | yes | Old tests bound to grammar names. | ~25. | `test-fixtures/` plus owner crates. | A/G. |
| `crates/ir` | yes | IR IDs and types. | ~30. | `ir/src/grammar_ir/`, `ir/src/backend_ir/`. | C/E. |
| `crates/ir` | yes | Strategy registry. | ~3. | none (ABROGATE-DELETE). | A. |
| `crates/ir` | yes | Type / shape / recognizer / cost facts. | ~80. | `passes/src/*`, `ir/src/side_tables/`. | C. |
| `crates/ir` | yes | VM and debug. | ~40. | `vm/`. | E. |
| `crates/ir` | yes | Egraph/CSP bridge. | ~25. | `passes/src/bridge/`. | C. |
| `crates/ir` | yes | Other (shared utilities). | ~46. | `ir/src/util/` plus `passes/`. | C. |
| `crates/csp-solver` | partly | Generic core retained. | ~40. | `csp-solver/`. | A. |
| `crates/csp-solver` | partly | BBNF-specific adapters. | ~5. | `passes/src/bridge/`. | C. |
| `crates/csp-solver` | partly | Oversized modules. | ~5. | `csp-solver/` split. | A. |
| `crates/egraph` and `crates/egraph-derive` | partly | Generic core retained. | ~17. | `egraph/`, `egraph-derive/`. | A. |
| `crates/egraph` and `crates/egraph-derive` | partly | BBNF terms/adapters. | ~2. | `passes/src/bridge/`. | C. |
| `crates/lsp` | yes | LSP protocol server. | ~6. | `bbnf-language-server/protocol/`. | I. |
| `crates/lsp` | yes | Diagnostics bridge. | ~4. | `bbnf-language-server/diagnostics/`. | I. |
| `crates/lsp` | yes | Incremental parser glue. | ~3. | `bbnf-language-server/document/`, `pipeline/`. | I. |
| `crates/simd-scan` → `crates/bbnf-simd` | partly | Generic core retained; grammar-neutral primitive boundary per Lock 14 + Lock 16 (zero JSON-specific code). | ~13. | `bbnf-simd/`. | A/H. |
| `crates/simd-scan` → `crates/bbnf-simd` | partly | BBNF-specific recognizer wiring. | ~2. | `passes/src/recognizers/`. | C/H. |
| `crates/gorgeous` | no | Whole crate ARCHIVE. | 17. | `restart-archive`/legacy reference only. | A. |
| `crates/ser` | no | Whole crate ARCHIVE. | 5. | `restart-archive`/legacy reference only. | A. |
| `skinny/crates/codegen` (Lock 14 v+1) | yes | `RuntimeProvider::dispatch_value` enum arm + 8 hardcoded match arms (`skinny/crates/codegen/src/lib.rs:167-209`). | ~1 file, 8 arms. | Trait-method `dispatch` under provider-free generator body after aggregate frontend closure. | SK-V14 W5A/W5B.0..W5B.4/W5C-GEN/W5D-DELETE PRUNE-3 (3F-MIG-007); rename is migration-full per `[no-backward-compat]`. |
| SKELETON triple primitives (T-P2 V3 refusal) | no | `FSM_DISPATCH_THREADED` + `FRAME_PUSH_BOUNDED` + `FRAME_POP_BOUNDED` — **DELETE proposal REJECTED**. | 3 primitives. | Preserve as non-shortlist-blocker support under SRC-V2-FOLD; not deleted. | Refusal row per T-P2 V3 LOCK cohort refutation density **32:69 = 31.7%** (1:2 anti-paper-close pattern; `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:76,172,187,295`). Re-proposal requires same-wave consumer + first consumer path + executable command per T-P2 V4 non-shortlist criteria. |

The per-family row counts are approximate and refine to exact per-file numbers
during tranche A.W2 when the migration manifest crystallises. Aggregate row
counts must reconcile to the 834-file total before A.W2 closes.

### 3.2 Per-Crate Disposition Tables

The following rows are abbreviated by uniform directory where that is the
truthful unit. Tranche implementation must refine any row that mixes fates
before editing files.

#### `crates/analysis`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Diagnostics and report helpers | `error`, `bbnf-language-server/diagnostics` | ABROGATE-MOVE/KEEP-MODIFY | Same diagnostic codes must serve CLI and LSP. | PASS-3 recovery/LSP contract (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |
| Semantic index logic | `bbnf-language-server/semantic` | KEEP-MODIFY | Useful editor behavior, new ownership. | MODULES old analysis keep note (`restart/corpora/MODULES.md:509-565`). |
| Document snapshot/edit helpers | `source/snapshot`, `bbnf-language-server/document` | ABROGATE-MOVE | Shared with incremental parser. | README incremental rule (`restart/README.md:344-348`). |
| Grammar-specific assumptions | none | ABROGATE-DELETE | Violates Lock 14 if present. | CENSUS grammar leaks (`restart/corpora/CENSUS.md:103-122`). |

#### `crates/lsp`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| LSP protocol server | `bbnf-language-server/protocol` | KEEP-MODIFY | Protocol work remains useful. | PASS-3 ecosystem tree (`restart/audit/pass-3-runtime/PASS-3.md:160-289`). |
| Diagnostics bridge | `bbnf-language-server/diagnostics` | KEEP-MODIFY | Reuse behavior over new diagnostic types. | PASS-3 diagnostics handoff (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |
| Incremental parser glue | `bbnf-language-server/document` and `pipeline` | ABROGATE-REPLACE | Must use `DocumentSnapshot` and `ReparsePlan`. | PASS-3 commitments (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |

#### `crates/bbnf-path`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Macro entrypoint | `path/src/macro_impl` | KEEP-MODIFY | Public Rust macro surface remains. | README path API (`restart/README.md:272-318`). |
| Parser/evaluator logic | `path-core/src/parse`, `path-core/src/eval` | ABROGATE-MOVE/KEEP-MODIFY | Shared by Rust and TS. | Lock 7 path split (`restart/locks/LOCKS.md:117`). |
| Proc-macro `syn::ParseStream` use | `path/src/macro_impl` | KEEP-MODIFY | This is not runtime ParseStream. | PASS-3 stale runtime term resolution (`restart/audit/pass-3-runtime/PASS-3.md:14-23`). |

#### `crates/bbnf-path-ts`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| TS emitter/schema | V2 `path-ts/src/schema`, V2 `path-ts/src/emit` | KEEP-MODIFY-DEFER | Keep TS surface as V2 work, consuming shared semantics after `TsBackend: Backend` lands. | Lock 7 + Lock 5 (`restart/locks/LOCKS.md:117`, `restart/locks/LOCKS.md:113`). |
| Hardcoded grammar registries | none | ABROGATE-DELETE | Generic path package cannot name grammars. | CENSUS path leaks (`restart/corpora/CENSUS.md:103-122`). |
| Fixture duplicates | `test-fixtures` | ABROGATE-MOVE | Shared parity fixture ownership. | BD inheritance via index (`restart/inheritance/INDEX.md:29-40`). |

#### `crates/bootstrap`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| IR dump/debug commands | `bbnf-cli/debug`, `vm/debug` | ABROGATE-MOVE/REPLACE | Debug survives over new IRs. | README VM debug/replay (`restart/README.md:344-348`). |
| Bootstrap parse command | `grammar/bootstrap`, `pipeline` | KEEP-MODIFY | Bootstrap remains needed, not as crate. | MODULES bootstrap slim (`restart/corpora/MODULES.md:216-228`). |
| Standalone crate shell | none | ABROGATE-DELETE | Not in final 24-crate workspace. | README crate table (`restart/README.md:29-60`). |

#### `crates/core`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| `grammar/generated/*.rs` | `runtime/src/grammars/<name>/generated.rs` | GENERATED-REPLACE | New template output from BIR. | PASS-2 runtime template (`restart/audit/pass-2-codegen/PASS-2.md` §7). |
| `grammar/generated/.registry.json` | none | ABROGATE-DELETE | Metadata is source of truth. | README two-surface onboarding (`restart/README.md:11-25`). |
| `grammar` AST/parser helpers | `grammar/src/*` | ABROGATE-MOVE/KEEP-MODIFY | Grammar crate owns BBNF parsing. | PASS-1 crate tree (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| `imports`, source maps, spans | `source/src/*` | ABROGATE-MOVE | Shared source substrate. | README pipeline (`restart/README.md:188-207`). |
| `lower` and normalization | `passes`, `ir`, `codegen` | ABROGATE-REPLACE | Split semantic passes from backend lowering. | Lock 5 (`restart/locks/LOCKS.md:113`). |
| `backend/**` | `codegen/src/*` | ABROGATE-REPLACE | BIR-only lowerers replace grammar walkers. | PASS-2 (`restart/audit/pass-2-codegen/PASS-2.md:5-8`). |
| `runtime/mod.rs` generic support | `runtime/src/document`, `runtime/src/support` | KEEP-MODIFY | Useful support under tape/direct contract. | PASS-3 runtime (`restart/audit/pass-3-runtime/PASS-3.md:96-135`). |
| `runtime/<grammar>/**` | `runtime/src/grammars/<name>/**` | GENERATED-REPLACE | Template-emitted per grammar modules. | PASS-2 template schema (`restart/audit/pass-2-codegen/PASS-2.md` §7). |
| `path` executor | `path-core`, `runtime` | ABROGATE-REPLACE | Shared path semantics and runtime view integration. | README path API (`restart/README.md:272-318`). |
| `css_types.rs` and host shims | `host`, metadata, generated `host.rs` | ABROGATE-REPLACE | Host functions are generic/fenced. | Lock 14 (`restart/locks/LOCKS.md:220`). |
| `generate/serialize` | none | ABROGATE-DELETE | `ser` is archive-only. | MODULES ser archive (`restart/corpora/MODULES.md:165-184`). |
| Old tests bound to grammar names | `test-fixtures` plus owner crates | KEEP-MODIFY/REPLACE | Preserve fixtures, replace assumptions. | CENSUS duplicate runtime cohort (`restart/corpora/CENSUS.md:435-527`). |

#### `crates/ir`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| IR IDs/types | `ir/src/grammar_ir`, `ir/src/backend_ir` | KEEP-MODIFY/REPLACE | Two IRs are final architecture. | README two IRs (`restart/README.md:104-118`). |
| Strategy registry | none | ABROGATE-DELETE | Hardcoded grammar strategy violates Lock 14. | CENSUS leaks (`restart/corpora/CENSUS.md:103-122`). |
| Type facts | `passes/src/layout` (subroutine), `ir/src/side_tables` (`LayoutFacts`) | ABROGATE-MOVE/KEEP-MODIFY | HM + bidirectional + CSP run inside layout lowering per Lock 2; `TypeFacts` is internal scratch, `LayoutFacts` is the public side-table. | README type system (`restart/README.md:258-268`); Lock 2 (`restart/locks/LOCKS.md:92`). |
| Shape facts | `passes/src/shapes`, `ir/src/side_tables` | ABROGATE-MOVE/KEEP-MODIFY | Direct/value/path consumers. | PASS-1 side-table contract (`restart/audit/pass-1-substrate/PASS-1.md:24-42`). |
| Recognizer facts | `passes/src/recognizers` | ABROGATE-MOVE/KEEP-MODIFY | Pratt/SIMD auto-detection. | Lock 10 (`restart/locks/LOCKS.md:164`). |
| VM/debug | `vm` | ABROGATE-MOVE/REPLACE | VM replays BIR, not old IR. | PASS-1 VM scope (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| Egraph/CSP bridge | `passes/src/bridge` | ABROGATE-MOVE/KEEP-MODIFY | Bridge, not fused hypergraph. | Lock 4 (`restart/locks/LOCKS.md:111`). |

#### `crates/csp-solver`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Generic domains/constraints | `csp-solver` | KEEP-OUTRIGHT/KEEP-MODIFY | Generic solver survives. | MODULES csp-solver (`restart/corpora/MODULES.md:73-132`). |
| BBNF-specific adapters | `passes/src/bridge` | ABROGATE-MOVE | Keep solver generic. | Lock 4/11 (`restart/locks/LOCKS.md:111`, `restart/locks/LOCKS.md:190`). |
| Oversized modules/tests | `csp-solver` split modules | KEEP-MODIFY | Lock 13 file size. | Lock 13 (`restart/locks/LOCKS.md:207`). |

#### `crates/egraph` And `crates/egraph-derive`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Generic egraph core | `egraph` | KEEP-MODIFY | Useful sister crate. | MODULES egraph (`restart/corpora/MODULES.md:136-162`). |
| Derive macro | `egraph-derive` | KEEP-MODIFY | Keep with egraph. | MODULES egraph derive (`restart/corpora/MODULES.md:136-162`). |
| BBNF terms/adapters | `passes/src/bridge` | ABROGATE-MOVE | Generic crate stays grammar-neutral. | Lock 14 (`restart/locks/LOCKS.md:220`). |

#### `crates/simd-scan` → `crates/bbnf-simd` (rename per Lock 14 + Lock 16)

The crate renames to `bbnf-simd` because it is the grammar-neutral primitive boundary: scalar reference + per-target NEON / AVX-2 / AVX-512 kernels + dispatch API. Per Lock 14, the crate carries zero JSON-specific code (no `match grammar { Json => ... }` arms, no grammar-named modules); per Lock 16, every `core::arch::*` use-site and every `asm!` block traces to a citation in the allowlist. Grammar IR feeds rule facts (alphabet, first-set, chunk-spanning tokens) and the cost model selects which primitive to call; the primitive itself names neither the grammar nor the parser.

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Scalar scanner | `bbnf-simd/scalar` | KEEP-OUTRIGHT | Reference implementation; SWAR fallback at ~7 GB/s. | MODULES simd-scan (`restart/corpora/MODULES.md:47-69`). |
| NEON kernels | `bbnf-simd/aarch64/` (`classify_tbl4.rs`, `movemask.rs`, `string_block.rs`, `match_tiny_plain_string`, TBL-driven `\uXXXX` hex decode, LD4-interleaved classifier, BCAX/EOR3 ternary, `svmatch_u8` emulation) | KEEP-MODIFY or ABROGATE-DELETE | Source-present kernels must pass Lock 16: scalar reference, strict checkasm, same-wave consumer, measured row movement, and zero orphan state at close. Historical Class A/B pathology labels are evidence, not admission. A kernel that cannot satisfy the manifest is deleted or scalar-delegated with REDRESS evidence. | PASS-2 SIMD matrix; `MASTER-PLAN.md` §13.1 + Lock 16. |
| AVX-2 / AVX-512 kernels | `bbnf-simd/x86_64/{avx2,avx512_vbmi2,avx512_ifma,avx512_vnni,avx512_bitalg,avx512_gfni,avx512_clmul,avx512_kmask}/` | KEEP-MODIFY | Wire to `SimdScan` BIR; strict-additions-on-top-of-asmjson stack per H.W5. | PASS-2; `MASTER-PLAN.md` §13.1 esoteric AVX-512 rows. |
| Dispatch API | `bbnf-simd/dispatch` | KEEP-MODIFY | Runtime/codegen consumer boundary; CPUID at parser construction. | PASS-2 detector commitments; H.W0 packet. |
| Differential parity harness | `bbnf-simd/tests/checkasm_parity.rs` | KEEP-MODIFY | Admission gate: `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity` returns zero divergences before any bench claim. | SK-V3 packet §2 (P0.2 escape_mask_64 fix). |

#### `crates/ser` And `crates/gorgeous`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Entire `ser` crate | archive | ARCHIVE | No production caller. | MODULES ser (`restart/corpora/MODULES.md:165-184`). |
| Entire `gorgeous` crate | archive | ARCHIVE | Mostly per-grammar shims. | MODULES gorgeous (`restart/corpora/MODULES.md:188-212`). |

## 4. Root Workspace Migration

Current root metadata names the nine grammars and an old strategy table
(`Cargo.toml:18-29`, `Cargo.toml:41-56`). The strategy table names parser types,
builders, documents, and modules, which violates the future grammar contract.

Disposition:

| Current artifact | Fate | Replacement |
|---|---|---|
| `[workspace].members` old crate list | ABROGATE-REPLACE | 24-crate greenfield member list from `restart/ARCHITECTURE.md`. |
| `[workspace.metadata.bbnf].grammars = [...]` array | KEEP-MODIFY | Per-grammar tables under `[workspace.metadata.bbnf.grammars.<name>]`. |
| `[workspace.metadata.bbnf-strategy]` | ABROGATE-DELETE | Auto-detected strategy facts in `passes` and generated runtime metadata. |
| Comments requiring `PRODUCTION_MANIFEST_TABLE` edits | ABROGATE-DELETE | Future grammar gate that forbids Rust edits. |
| Current dev profile and dependencies | KEEP-MODIFY | Re-evaluate under new crates; keep only active shared dependencies. |

Hard gate:

```sh
rg "bbnf-strategy|PRODUCTION_MANIFEST_TABLE|JsonParser|CssL4Parser|GrammarAuditTag" Cargo.toml crates
```

The command must not find production hardcoded grammar dispatch in generic
crates after tranche A closes.

## 5. `crates/core` Disposition

The module corpus shows `core` is the largest current crate, with handwritten
logic mixed with generated parsers, backend code, runtime code, path execution,
serialization, and per-grammar modules (`restart/corpora/MODULES.md:589-999`).
The restart does not preserve that crate boundary.

### 5.1 Grammar And Generated Parsers

| Current path | Fate | Replacement |
|---|---|---|
| `crates/core/src/grammar/generated/*.rs` | GENERATED-REPLACE | `runtime/src/grammars/<name>/generated.rs` emitted from Backend IR templates. |
| `crates/core/src/grammar/generated/.registry.json` | ABROGATE-DELETE | Workspace metadata and generated manifest derived by pipeline. |
| `crates/core/src/grammar/*` handwritten AST/parser helpers | ABROGATE-MOVE/KEEP-MODIFY | `grammar/src/ast`, `grammar/src/parse`, `grammar/src/validate`. |
| Bootstrap parser helpers | KEEP-MODIFY | `grammar/src/bootstrap` and `pipeline` bootstrap tests. |

The corpus records nine generated grammars in the old layout
(`restart/corpora/MODULES.md:609-629`). The future layout keeps nine generated
grammar modules initially, but they are emitted from templates and are not
hand-edited.

### 5.2 Backend And Lowering

| Current path | Fate | Replacement |
|---|---|---|
| `crates/core/src/backend/**` | ABROGATE-REPLACE | `codegen/src/lower`, `codegen/src/rust`, `codegen/src/wasm`, `codegen/src/templates`. |
| Old emitter driver walking Grammar IR | ABROGATE-DELETE | BIR-only lowerer contract. |
| backend template fragments | KEEP-MODIFY | New template system after removing grammar-name assumptions. |
| tests proving emitted behavior | KEEP-MODIFY | `codegen::verify`, `vm`, `test-fixtures`. |

PASS-2 says lowerers consume Backend IR and commit regenerated output
(`restart/audit/pass-2-codegen/PASS-2.md:32-49`). Any current backend file that
walks Grammar IR is therefore replaced, even if its local formatting logic is
mined.

### 5.3 Runtime

| Current path | Fate | Replacement |
|---|---|---|
| `crates/core/src/runtime/mod.rs` and generic support | KEEP-MODIFY | `runtime/src/document`, `runtime/src/builder`, `runtime/src/support`. |
| `crates/core/src/runtime/<grammar>/**` | GENERATED-REPLACE | `runtime/src/grammars/<name>/**` generated template output. |
| OpenFrame/checkpoint-heavy fallback logic | ABROGATE-REPLACE | Tape builder with bounded checkpoints. |
| direct struct builders | KEEP-MODIFY/REPLACE | Direct builders scheduled with tape emission. |

The restart sketch measured `Vec<OpenFrame>::clone` at 86.07 percent inclusive
samples in the current path (`restart/corpora/RESTART-SKETCH.md:154-184`).
The new runtime must prove OpenFrame clone stacks are gone.

### 5.4 Source, Imports, And Pipeline

| Current path | Fate | Replacement |
|---|---|---|
| `source`/span helpers | ABROGATE-MOVE | `source/src/file`, `source/src/span`. |
| import graph helpers | ABROGATE-MOVE/KEEP-MODIFY | `source/src/include` and `grammar::metadata`. |
| pipeline drivers | ABROGATE-MOVE/REPLACE | `pipeline/src/stages` following README pass order. |

### 5.5 Host And Grammar-Specific Shims

| Current path | Fate | Replacement |
|---|---|---|
| `css_types.rs` and similar host shims | ABROGATE-REPLACE | `host` generic primitives, metadata, and `@host fn`. |
| grammar-name match arms | ABROGATE-DELETE | Metadata-driven dispatch. |
| per-grammar runtime host files | GENERATED-REPLACE | Template-emitted `host.rs` under generated grammar module. |

CENSUS names `css_types.rs`, strategy registries, path registries, and
grammar-specific runtime shims as current generalization leaks
(`restart/corpora/CENSUS.md:103-122`).

### 5.6 Serialize And Legacy Fallbacks

| Current path | Fate | Replacement |
|---|---|---|
| `generate/serialize` and ser adapters | ABROGATE-DELETE | No production replacement; `ser` archive only. |
| legacy/fallback markers | ABROGATE-DELETE | Fail-explicit diagnostics or removed path. |

CENSUS counts legacy/fallback markers and fail-explicit rows that must be
retired during restart migration (`restart/corpora/CENSUS.md:170-215`).

## 6. `crates/ir` Disposition

Current `ir` has useful raw material but not the final architecture. The corpus
calls the structure sound while identifying many large files and misplaced
responsibilities (`restart/corpora/MODULES.md:264-505`).

| Current area | Fate | Replacement |
|---|---|---|
| Grammar-like IR types | KEEP-MODIFY | `ir/src/grammar_ir`. |
| Backend/output IR pieces | ABROGATE-REPLACE | `ir/src/backend_ir` with 20 variants (19 semantic variants plus `Return`) per ARCH §7.2. |
| Strategy registries with grammar names | ABROGATE-DELETE | Metadata-derived profiles and side tables. |
| Type/checking facts | KEEP-MODIFY/ABROGATE-MOVE | `passes/src/layout` (HM + bidirectional + CSP subroutine), `ir/src/side_tables` (`LayoutFacts`). |
| Shape/mining facts | KEEP-MODIFY/ABROGATE-MOVE | `passes/src/shapes`, `passes/src/recognizers`. |
| Egraph bridge code | KEEP-MODIFY/ABROGATE-MOVE | `passes/src/bridge`, generic `egraph`. |
| CSP-facing strategy code | KEEP-MODIFY/ABROGATE-MOVE | `passes/src/extract`, `csp-solver`, `cost-model`. |
| VM/debug execution | ABROGATE-MOVE/REPLACE | `vm`. |

Hard gates:

```sh
rg "Json|Css|Csv|Google|Math|Bnf|Ebnf" crates/ir/src
rg "emit.*Grammar|walk.*Grammar" crates/ir/src crates/codegen/src
```

After migration, grammar names are not part of generic IR logic, and emitters
do not walk Grammar IR.

## 7. `analysis` And `lsp` Disposition

`analysis` and `lsp` carry diagnostics, document state, semantic indexing, and
editor behavior. They do not survive as separate production crates because the
README workspace names one `bbnf-language-server` crate
(`restart/README.md:29-60`), and PASS-3 explicitly routes error recovery,
incremental parsing, LSP, playground, and DAP into the runtime/user surface
handoff (`restart/audit/pass-3-runtime/PASS-3.md:137-158`).

| Current area | Fate | Replacement |
|---|---|---|
| Diagnostics | KEEP-MODIFY/ABROGATE-MOVE | `error`, `bbnf-language-server/diagnostics`. |
| Document snapshots | KEEP-MODIFY/ABROGATE-MOVE | `source/snapshot`, `bbnf-language-server/document`. |
| Incremental parse hooks | ABROGATE-REPLACE | `DocumentSnapshot`, `ReparsePlan`, pipeline incremental entry. |
| LSP protocol code | KEEP-MODIFY | `bbnf-language-server/protocol`. |
| Analysis-only grammar assumptions | ABROGATE-DELETE | Metadata and Grammar IR facts. |

Gate:

```sh
cargo test -p bbnf-language-server diagnostics incremental
```

## 8. Path Crates Disposition

Lock 7 consolidates old path crates into `path` and `path-core` on the V1
Rust line, with `path-ts` deferred to V2 (`restart/locks/LOCKS.md:117`).
The module corpus already identifies
duplication and registry problems in the current path crates
(`restart/corpora/MODULES.md:232-260`).

| Current area | Fate | Replacement |
|---|---|---|
| Rust macro parser | KEEP-MODIFY | `path/src/macro_impl` plus shared parser in `path-core`. |
| Shared path AST/evaluator ideas | KEEP-MODIFY | `path-core/src/ast`, `path-core/src/eval`. |
| TypeScript hardcoded registry/docs | ABROGATE-REPLACE | V2 `path-ts` generated schema from `path-core` facts. |
| Grammar-specific path mirrors | ABROGATE-DELETE | Runtime views and generated metadata. |
| Fixture duplicates | ABROGATE-MOVE | `test-fixtures`. |

Gate:

```sh
rg "json|css_l4|css_pretty|google_sheets|math" crates/path crates/path-core
```

Path crates may use fixture names in tests, but not production registries.

## 9. Sister Crates Disposition

### 9.1 `csp-solver`

The corpus classifies `csp-solver` as generic and worth keeping, while calling
out large files and split work (`restart/corpora/MODULES.md:73-132`).
It remains a finite-domain choice solver, not the owner of HM equality
unification; `passes::layout` produces internal type obligations before any
CSP-backed finite choice is solved.

Fate:

| Area | Fate |
|---|---|
| Generic domain/constraint/solve APIs | KEEP-OUTRIGHT/KEEP-MODIFY |
| BBNF-specific bridge code | ABROGATE-MOVE to `passes::bridge`, keyed by stable Grammar IR node IDs and e-class IDs rather than chosen e-node representatives. |
| Oversized modules | KEEP-MODIFY split under Lock 13 |
| Tests | KEEP-MODIFY |

### 9.2 `egraph` And `egraph-derive`

The corpus says `egraph` and `egraph-derive` are extracted together and useful
as generic crates (`restart/corpora/MODULES.md:136-162`).
The migration keeps generic arena/rewrite/extract/explain code while moving
bridge justifications, rewrite guards, and CSP legality facts into `passes`.

Fate:

| Area | Fate |
|---|---|
| Generic egraph arena/rewrite/extract/explain | KEEP-MODIFY |
| Derive macro | KEEP-MODIFY |
| BBNF bridge terms | ABROGATE-MOVE to `passes::bridge` |

### 9.3 `simd-scan` → `bbnf-simd`

The corpus marks `simd-scan` clean and KEEP-AS-IS, while noting the NEON
intrinsics file as split-exempt in the old audit (`restart/corpora/MODULES.md:47-69`).
The crate renames to `bbnf-simd` per Lock 14 + Lock 16: it is the grammar-neutral
SIMD/ASM primitive boundary, carries the V1 allowlist of admissible primitives
with citation per entry, and serves any grammar (not only JSON) whose recognizer
facts request scan/classify/match work. JSON corpus-parity tests are bench
fixtures, not crate code.

Fate:

| Area | Fate |
|---|---|
| Scalar/NEON/AVX kernels | KEEP-OUTRIGHT/KEEP-MODIFY |
| Dispatch API | KEEP-MODIFY for `SimdScan` BIR integration, with `Exact` scans proving scalar offset parity and `Prefilter` scans routing candidate offsets through `RegexProgram` or scalar verification before tape emission. |
| Tests/fixtures | KEEP-MODIFY |

PASS-2 requires SIMD coverage across scalar, NEON, AVX2, AVX512, and WASM SIMD
paths (`restart/audit/pass-2-codegen/PASS-2.md` §3).

Research-source hygiene: this migration surface relies on the local corpora and
PASS citations above for disposition evidence. Unverified research-index leads
such as Hubbard's JSON comparison row, Almomany cost-model wording, the exact
Deb bibliography variant, Ungar/Adams, and HelpMate remain bibliography
receivers, not migration evidence.
Regex and SIMD migration gates therefore compare behavior against verified
local corpora, PASS contracts, and `parse-that-regex` internal cross-engine
parity (NFA vs lazy DFA vs full DFA vs VM) rather than unverified catalogue
leads.

## 10. Archive Crates

| Crate | Fate | Reason |
|---|---|---|
| `ser` | ARCHIVE | Module corpus says no production caller and archive-only (`restart/corpora/MODULES.md:165-184`). |
| `gorgeous` | ARCHIVE | Module corpus says it is mostly per-grammar shims and archive-only (`restart/corpora/MODULES.md:188-212`). |

Archive procedure:

```sh
git mv crates/ser restart-archive-2026-05-04/crates/ser
git mv crates/gorgeous restart-archive-2026-05-04/crates/gorgeous
```

The exact archive destination belongs to tranche A. This Phase 2 document only
sets the disposition.

## 11. Generated Code And Runtime Template

PASS-2 sets the runtime template schema and generated output tree
(`restart/audit/pass-2-codegen/PASS-2.md` §7). It also sets generated LOC
budget tracking and a +2 percent ceiling (`restart/audit/pass-2-codegen/PASS-2.md` §6).

Generated migration:

| Current generated source | Replacement |
|---|---|
| `crates/core/src/grammar/generated/<name>.rs` | `runtime/src/grammars/<name>/generated.rs`. |
| Handwritten per-grammar runtime builders | Template-emitted builder/view/value/visitor files. |
| `.registry.json` | Generated manifest from metadata, not committed as source of truth. |
| Production parser manifest tables | Removed. |

Generated output rules:

1. Generated files are committed.
2. Generated files carry a header with grammar source hash, metadata hash, and
   Backend IR hash.
3. Regeneration must be byte-for-byte equal unless the tranche explicitly
   updates expected output.
4. Generated LOC budgets are tracked by grammar and by total workspace.
5. Generated files may exceed 500 LOC; handwritten files may not.

## 12. Tests And Fixtures

The restart creates `test-fixtures` because fixture and parity work is shared
by runtime, codegen, CLI, language server, and bench crates. The legacy BD
fixture spec used a shared worktree fixture package and parity matrix as close
gates; the inheritance index keeps BD as the source for parity/publication
discipline (`restart/inheritance/INDEX.md:29-40`).

Migration:

| Current tests | Fate |
|---|---|
| Generic solver/scanner/egraph tests | KEEP-MODIFY in owner crates. |
| Core generated parser tests | GENERATED-REPLACE and move fixtures to `test-fixtures`. |
| Backend golden tests | KEEP-MODIFY around Backend IR and VM replay. |
| Path duplicate fixtures | ABROGATE-MOVE to `test-fixtures`. |
| Inline tests in oversized modules | KEEP-MODIFY into module tests or integration tests. |

CENSUS identifies inline test violations that must be cleaned while splitting
files (`restart/corpora/CENSUS.md:383-399`).

## 13. New Facilities

These facilities do not exist as clean production crates today. They are not
optional; they are the replacement architecture.

| New path | Facility | First owner | Source |
|---|---|---|---|
| `crates/bbnf` | Public library facade. | A/B | README crate table (`restart/README.md:29-60`). |
| `crates/bbnf-cli` | Public CLI. | A/F/I | README crate table (`restart/README.md:29-60`). |
| `crates/bbnf-language-server` | Consolidated LSP. | A/I | PASS-3 runtime tree (`restart/audit/pass-3-runtime/PASS-3.md:160-289`). |
| `crates/bbnf-bench` | SOTA and fixture bench harness. | A/H/J | README SOTA targets (`restart/README.md:322-340`). |
| `crates/error` | Shared diagnostics. | A/I | PASS-3 recovery contract (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |
| `crates/source` | Source files, spans, snapshots. | A/I | README pipeline/incremental (`restart/README.md:188-207`, `restart/README.md:344-348`). |
| `crates/grammar` | BBNF AST/parser/validation. | A/D | PASS-1 crate tree (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| `crates/pipeline` | Stage orchestration and artifact verification. | A/F | README pass order (`restart/README.md:188-207`). |
| `crates/passes` | Type/shape/recognizer/extract/bridge passes, including HM equality obligations, expected checking, bounded coercion, finite CSP choices, stable bridge IDs, and extraction-time legality. | C/H | PASS-1 commitments (`restart/audit/pass-1-substrate/PASS-1.md:24-42`). |
| `crates/vm` | Backend IR replay/debug. | E/I | README VM debug/replay (`restart/README.md:344-348`). |
| `crates/codegen` | BIR-only lowerers and templates. | E/F/H | PASS-2 (`restart/audit/pass-2-codegen/PASS-2.md` §2-§7). |
| `crates/runtime` | Tape/direct runtime, payload policy, snapshot-scoped tape identity, typed projections, and generated grammar modules. | B/F | Lock 1 (`restart/locks/LOCKS.md:48`). |
| `crates/host` | Generic host primitive/registry system. | D/F | README host decisions (`restart/README.md:160-182`). |
| `crates/cost-model` | `CostDecision` facts, objective profiles, Pareto/frontier evidence, solver-backed extraction adapters, LOC budgets. | C/H/J | PASS-1 cost model (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| `crates/path-core` | Shared path semantics. | G | Lock 7 (`restart/locks/LOCKS.md:117`). |
| `crates/parse-that` | Parser combinator family below BBNF, paired with the regex sub-crate `crates/parse-that-regex` (renamed from legacy `bbnf-regex` per Lock 11). Grammar-owned HIR/verifier integration; cross-engine parity (NFA, lazy DFA, full DFA, VM) is internal to `parse-that-regex`; no third-party regex oracle is cited. | D/H | README Unicode routing (`restart/README.md:131-143`); Lock 11 (`restart/locks/LOCKS.md:190`). |
| `crates/test-fixtures` | Shared fixtures and parity matrix. | A/G/J | Inheritance map (`restart/inheritance/INDEX.md:29-40`). |

## 14. LOC Trajectory

The restart controls generated source because generated code currently
dominates the workspace. PASS-2 records a generated LOC baseline and a +2
percent ceiling for emitted runtime source (`restart/audit/pass-2-codegen/PASS-2.md` §6).

| Phase | Expected movement | Gate |
|---|---|---|
| Pre-A | Current generated and handwritten code remain untouched except archives. | Clean status before branch/tag. |
| A | `ser` and `gorgeous` leave production workspace; crate skeletons add small handwritten LOC. | `cargo metadata`, tree lint. |
| B | Runtime tape/direct handwritten support appears; no generated explosion yet. | Runtime tests. |
| C | IR/pass side-table code grows; old `ir` large files are split or replaced. | IR/pass tests and Lock 13. |
| D | BBNF parser/type extension LOC grows; rewrite-mode stays absent. | Extension parser tests. |
| E | Backend IR and VM LOC grows; old backend walkers still not carried over. | BIR/VM tests. |
| F | Generated runtime output lands for seed grammars; old generated layout retires. | Regen equality and LOC budget. |
| G | Path/value/visitor LOC grows; hardcoded path registries retire. | Future grammar test. |
| H | SIMD/WASM/Pratt LOC grows under feature gates. | Platform and SOTA tests. |
| I | LSP/recovery LOC consolidates `analysis` and `lsp`. | LSP incremental parity. |
| J | Final docs and publication metadata settle; no new architecture. | Full parity and docs checks. |

The steady-state goal is not “least LOC.” It is less handwritten duplication,
bounded generated growth, and no grammar-name maintenance cost.

## 15. Commit-Chain Disposition

Implementation should preserve the old workspace history while making the
greenfield change obvious in Git.

| Step | Action | Evidence |
|---|---|---|
| 1 | Tag the pre-restart state as `pre-restart-2026-05-04`. | `git rev-parse pre-restart-2026-05-04`. |
| 2 | Create `master-greenfield-2026-05-04` or the user-approved equivalent. | Branch exists and points to restart base. |
| 3 | Archive legacy-only crates in one body-bearing commit. | Diff contains only archive membership and workspace removal. |
| 4 | Create skeleton crates in dependency order. | `cargo metadata` and `cargo check --workspace`. |
| 5 | Move kept code with `git mv` where useful. | Diffs show moves rather than unrelated rewrites. |
| 6 | Replace architecture-conflicting code with tests first where possible. | Commit bodies name why, what, evidence, routed remainder. |
| 7 | Land generated runtime output only after equality and budget gates exist. | Generated commit includes budget evidence. |

The exact branch operation is future implementation work; this synthesis commit
does not create branches or tags.

Branch/tag routing floor:

| Artifact | Status | Owner | Evidence command |
|---|---|---|---|
| `pre-restart-2026-05-04` tag | Required history marker before implementation source edits. | A.W0. | `git rev-parse pre-restart-2026-05-04` resolves to a commit. |
| `master-greenfield-2026-05-04` branch | Suggested implementation branch unless the user selects another branch name. | A.W0. | `git rev-parse --verify master-greenfield-2026-05-04` resolves; `git symbolic-ref refs/heads/master-greenfield-2026-05-04` shows the tracked branch. |
| Workspace skeleton commit | Body-bearing workspace genesis with `[workspace.members]` named per Architecture §1. | A.W1. | `git diff --name-only A.W0..A.W1` lists only `Cargo.toml`, crate `Cargo.toml`, and `lib.rs` stubs. |
| Archive commits | Body-bearing and narrow to workspace/archive membership. | A.W0. | `git diff --stat A.W0~..A.W0` shows only archive-membership files; `cargo metadata --no-deps` lists no archive crate. |
| Generated commits | Body-bearing with equality, generated LOC, and routed remainder evidence. | F. | `cargo xtask bbnf build --all && git diff --exit-code crates/runtime/src/grammars`. |
| Branch operation enforcement | No tranche after A.W0 may create or rename branches/tags without an explicit migration commit. | All. | `git reflog --date=iso master` shows no rewrite-history operations after A close. |

## 16. Legacy BA-BD Inheritance

The legacy plan-set is not discarded. It is mined. The inheritance index maps
old BA/BB/BC/BD into new tranches A-J (`restart/inheritance/INDEX.md:29-40`).

| Legacy source | Keep | Do not keep |
|---|---|---|
| BA | Archive ceremony, god-module pressure, grammar generalization, close discipline. | Old anti-tape scrub and direct-only substrate. |
| BB | Optimization, Pratt/SIMD, path/visitor pressure, template thinking. | Topic-only waves and any grammar registry carry-forward. |
| BC | Backend ABI, typed IR, parity pressure. | Emitters walking grammar source and stale IR counts when PASS-2 differs. |
| BD | Fixture package, cross-backend matrix on the Rust line, publication order. | Premature TypeScript or WASM production: TS + WASM defer post-V1 as a principled architectural fork; V2 `TsBackend: Backend` and `WasmBackend: Backend` per `restart/ARCHITECTURE.md` §7.5 own the V2 carry. |

`docs/tranches/BA/BA.md` describes BA as a surgical foundation tranche with
archive and close gates (`docs/tranches/BA/BA.md:5-40`). That discipline is
kept, while its old direct-only substrate is superseded by Lock 1.

## 17. Tranche-Level Migration Sequence

The migration is sequenced by dependency and consumer gates, not by topic.
Lessons Learned says same-wave consumer gates are mandatory and split waves by
dependency, not topic (`docs/precepts/instructions/LESSONS-LEARNED.md:1-34`).

| Tranche | Migration work |
|---|---|
| A | Workspace genesis, archive crates, Cargo metadata schema, source/error/grammar skeleton, tree lint gates. |
| B | Tape/direct runtime substrate, value/document API, generated runtime template shell. |
| C | Grammar IR, internal type obligations, shape facts, stable CSP/egraph bridge facts, objective cost evidence, extraction legality. |
| D | BBNF extension parser/typing for lookbehind, rank-1 generics, host definitions/chains, bounded coercion sites, error/layout; regex Unicode below BBNF. |
| E | Backend IR, VM, extraction, lowerer contract. |
| F | Rust lowerer, runtime template output, regen equality. |
| G | Path/path-core Rust-line split, visitor, mutation API, future grammar gate; `path-ts` defers to V2 with `TsBackend`. |
| H | Pratt, verifier-bound exact/prefilter SIMD, `parse-that-regex` internal cross-engine parity, Rust-line SOTA early gates. WASM defers post-V1 alongside the V2 `WasmBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5. |
| I | Error recovery, snapshot/reuse-map incremental parsing, language server, playground/debug surfaces. |
| J | Parity, benchmarks, docs, publication readiness, close. |

This sequence keeps the tranche set at stub level. Full per-wave drafting
belongs to the next phase.

## 18. Greenfield Mechanics Summary

The synthesis prompt authorizes a greenfield restart with a commit-chain
disposition rather than hand-patching old code in place. The migration plan is:

1. Preserve the current state with a tag or branch before implementation.
2. Archive legacy-only crates and docs as the first tranche A action.
3. Replace the root workspace and crate tree in dependency order.
4. Bring forward kept code through intentional moves, not broad copy/paste.
5. Regenerate runtime output after Backend IR and lowerers exist.
6. Keep each tranche reversible through commits with body-bearing rationale.

Commit discipline comes from the local precept: broad, generated, deletion,
gate/status, benchmark, profiling, no-verify, and history-relevant commits need
bodies with why, what landed, evidence, and routed remainder
(`docs/precepts/instructions/LESSONS-LEARNED.md:56-72`).

## 19. Migration Gates

### 19.1 Generalization

```sh
cargo xtask lint-grammar-generalization
rg "JsonParser|CssL4Parser|CssPrettyParser|CsvParser|BbnfParser|EbnfParser|BnfParser|GoogleSheetsParser|MathParser" crates
rg "PRODUCTION_MANIFEST_TABLE|GrammarAuditTag|bbnf-strategy" Cargo.toml crates
```

Expected result: no production hits in generic crates.

### 19.2 Tree Shape

```sh
cargo xtask lint-tree
cargo xtask lint-loc --handwritten-max 500
```

Expected result: 4-10 children per handwritten source directory and no
handwritten Rust file over 500 LOC.

### 19.3 Backend Boundary

```sh
rg "GrammarIr|GrammarIR|grammar_ir" crates/codegen/src
rg "use .*grammar_ir|crate::grammar_ir|ir::grammar_ir" crates/codegen/src
cargo test -p codegen backend_ir_only
cargo test -p vm replay_all_backend_ir_variants
```

Expected result: codegen can name Backend IR and side-table types, but lowerers
do not walk Grammar IR.

### 19.4 Runtime Substrate

```sh
rg "OpenFrame|Vec<OpenFrame>|ParseStream" crates/runtime/src crates/codegen/src
cargo test -p runtime tape_direct_union
cargo test -p runtime __EAGER_EMPTY_PATH
cargo test -p runtime cursor_decision_skip
cargo test -p runtime tape_identity_payload_projection
cargo bench -p bbnf-bench --bench sota_json
```

Expected result: no old OpenFrame clone stack or ParseStream runtime concept.
`ParseStream` may remain only in proc-macro code that uses `syn`.
Runtime rows prove one `(TapeId, node id, payload class)` identity, direct
scalar caches over declared payload slots, validation/source ownership metadata,
and verifier-before-tape behavior for any SIMD prefilter path.

SK-V18 single-encoding closure gate (3F17-MH-03,
`restart/audit/totality/sk-v17/p3/3f-migration-handoff.md:69`): after the SK-V18
fold EXACTLY ONE tape encoding survives in `crates/core` — `grep` proves the AoS
`TapeRec` retired OR the SoA `Tape` retired, never both live; the dual AoS/SoA
state is admissible ONLY as a transient fold-state, never a Lock-1 closure (Lock
1, `restart/locks/LOCKS.md:75`). The `rg "OpenFrame|Vec<OpenFrame>|ParseStream"`
check additionally asserts `JsonStructBuilder`/`CssStructBuilder` eager
`OpenFrame` retirement; the eager-builder retirement severs the live per-leaf
`StructRegistry`/`FieldSource` coupling at `bbnf/arena.rs:47` (3F17-MH-05), so
no per-leaf `StructRegistry::layout(rule)` / `compound_kind_for_layout`
indirection re-enters the hot path (re-opens the measured 28-65× / 983× /
10583× regression; AZ-IV pre-block, SPEC `:791-794`). The projection generator
emits over the EXISTING `Tape`/`ValueRef`; an introduced
`StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the proven substrate
is a Lock-1 type-ambivalence REJECT (3F17-MH-06).

### 19.5 Generated Equality

```sh
cargo xtask bbnf build --all
git diff --exit-code crates/runtime/src/grammars
cargo xtask generated-loc-budget --max-growth 1.02
```

Expected result: regenerated output is equal and within budget.

### 19.6 Future Grammar

```sh
git diff --exit-code -- grammars/yaml.bbnf Cargo.toml
cargo xtask bbnf check yaml
cargo xtask bbnf build yaml
git diff -- crates ':!crates/runtime/src/grammars/yaml'
rg "yaml|Yaml" crates/*/src
```

Expected result: yaml enters through only `grammars/yaml.bbnf` plus workspace
metadata. Runtime output may be generated; generic crate source may not learn a
yaml name.

### 19.7 Diagnostic And Carry Proof

```sh
cargo test -p error diagnostic_codes_are_stable
cargo test -p bbnf-language-server diagnostics_match_cli
cargo xtask migration-carry --check
```

Expected result: migration does not drop receiver/blocker/gate rows for
deferred work, and public diagnostics are shared by CLI and LSP.

## 20. Unresolved Migration Punch List

Migration-implementation receivers are tracked at `restart/MASTER-PLAN.md` §24
(Carry and Friction Ledger) with `Source: migration` or
`Source: synthesis + migration` tags. The migration-sourced items — exact
generated header fields, declaration-crate review form, benchmark host
hardware profiles, archive destination for `ser`/`gorgeous`, PASS-2 BIR
snapshots, and Lock 3 cursor gates — appear in that consolidated ledger;
this section retains its heading for cross-document anchoring but no longer
carries a separate table. The `path-ts` publication timing and the WASM
exported ABI defer post-V1 alongside the V2 `TsBackend: Backend` and
`WasmBackend: Backend` impls per `restart/ARCHITECTURE.md` §7.5; both route
to V2 amendment and no longer occupy V1 carry rows. The single carry-truth
principle holds: one ledger, two sources, one set of receivers.

SK-V18 `StructLayout`→`Layout` rename row (3F17-MH-04,
`restart/audit/totality/sk-v17/p3/3f-migration-handoff.md:70`): the Lock-2-retired
`StructLayout` term is live at 960 sites in `crates/`. The rename is
GENERATOR-SIDE (regenerating all 8 parsers + ~16 tests), regen-gated, NOT a
hand-patch. Price it as the 960-site generator surface, not 40-120 LOC.
Receiver: SK-V18 codegen-rename wave (3B). Blocker: any hand-patch of
`StructLayout` outside the generator (clean-regen violation). Gate: §19.5
generated-equality (`git diff --exit-code`) + Lock 2 (`restart/locks/LOCKS.md:160`).

## 21. Migration Close

The restart migration keeps generic, tested infrastructure and removes the old
grammar-name architecture. It archives `ser` and `gorgeous`, splits `core`,
consolidates `analysis` and `lsp`, replaces backend/runtime generation around
Backend IR, preserves generic `csp-solver`, `egraph`, and `simd-scan`, and
creates the public/user crates required by the README.

The migration is done only when a new grammar can be added through `.bbnf` plus
metadata, runtime output regenerates equally, lowerers consume Backend IR only,
and no generic crate carries grammar-name dispatch.
