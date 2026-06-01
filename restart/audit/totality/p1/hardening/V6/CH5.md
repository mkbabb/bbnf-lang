---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-TOTALITY-EXCAVATION
cycle: V6-SKV18 (challenge cycle V6)
disposition: ACCEPT
generated_at: 2026-06-01T00:00:00Z
files_audited:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
  - restart/audit/totality/p1/T-P1-DISPATCH-CONTEXT.md
spec_surfaces_cross_read:
  - skinny/crates/runtime/src/tape/mod.rs
  - skinny/crates/runtime/src/tape/ (module roster)
  - skinny/crates/runtime/src/grammars/json/parser.rs
  - skinny/crates/runtime/src/grammars/ (OnceCell/RefCell/Cell sweep)
  - skinny/crates/runtime/src/grammars/css_l4_*/generated.rs (md5)
  - skinny/crates/runtime/src/grammars/{json,sheets_witness}/event_grammar_witness.rs
  - crates/core/src/grammar/generated/json.rs
  - crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs
  - crates/simd-scan/src/lib.rs
prior_cycle: V5/CH5.md (REVISE; CH5-V5-006, CH5-V5-007)
---

# CH5 HIDDEN COUPLING — SK-V18 T-P1 CHALLENGE (cycle V6)

## Verdict

ACCEPT.

V6 is the convergence-seeking re-run of the HIDDEN-COUPLING lens against the
near-converged inventory state committed at `097c4dd90` ("totality evidence
inventories 1A-1F (V1-V5 hardened, near-converged)"). The prior V5 CH5 returned
REVISE=2 — both findings (CH5-V5-006, CH5-V5-007) named ONE root: the substrate
inventory `1A` does not catalogue the totality `crates/core` `OnceCell<StructuralIndex>`
probe ("the probe substrate") and carries the bare closure word
"substrate-neutral confirmed" on `1A-SUB-024`. On fresh, proportionate review I
judge both findings to be BELOW the REVISE threshold for a near-converged
inventory: `1A` is correctly and explicitly scoped to the skinny substrate,
makes NO corpus-wide / "BOTH trees" substrate-clean claim, and the single live
cross-tree coupling (the totality probe substrate + the renamed/parallel
scanner) is fully owned and fenced by `1F`. Every load-bearing path:line in my
lens resolves at HEAD; no inventory states anything FALSE on disk; the firewall
holds. The honest tally is ACCEPT across the board.

## The CH5 firewall (lens contract)

No parallel substrate, sidecar producer, renamed-scanner Lock-1 violation, or
Track-1≡Track-2 dishonesty may pass uncatalogued; the substrate inventory `1A`
honours the Lock-1 union; the `1F` auxiliaries (anti-pattern, past-corpora) are
correctly cited as live where regenerated this cycle. I spot-verified the most
load-bearing cited rows against the V1 spec and the live code, and swept the
skinny tree directly for any sidecar `1A`/`1F` might have MISSED.

## Spot-verification of load-bearing rows (all confirmed at HEAD)

| claim (inventory) | live evidence | result |
|---|---|---|
| `1A` SPINE self-correction: `Tape::id` at `:170`, not the prior `:172` | `skinny/crates/runtime/src/tape/mod.rs:170` `pub fn id(&self) -> TapeId` | CONFIRMED |
| `1A-SUB-002`: `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` over `&'doc Tape` + `u32` cursor; `_grammar:PhantomData<fn()->G>` at `:179` | `tape/mod.rs:175`,`:178`,`:179` verbatim | CONFIRMED |
| `1A-SUB-023` SPINE: phantom `<G>` non-test production-instantiation census is EMPTY | all `EventGrammar`/`AnyGrammar` sites are the def (`event_grammar.rs:4,17,19`), the `ValueRef` field + Copy/Clone/impl blocks (`mod.rs:11,175,183,185,191`), the two witness DEFS (`json/event_grammar_witness.rs:4,17`; `sheets_witness/event_grammar_witness.rs:4,16`). Zero production instantiation of `G` with a real type. | CONFIRMED |
| `1A-SUB-011`: live tape modules are `assembler`/`event_grammar`/`offsets`, NOT spec-named token/builder/span/payload/view/trace | `ls skinny/crates/runtime/src/tape/` = `{assembler, event_grammar, event_grammar_tests, mod, offsets}.rs` | CONFIRMED |
| `1A-SUB-020`: skinny retained `ParserState` carries `{input, bytes, cursor, tape}` only — no retained sidecar | `grammars/json/parser.rs:7-12` verbatim 4 fields; no `Cell`/`OnceCell` field | CONFIRMED |
| `1A-SUB-024`: `tape/cursor.rs` (G4b) absent; CSS retained holds exactly the existing `Tape` | `ls tape/` has no `cursor.rs`; `css_l4_declaration_values/generated.rs:257` "Holds exactly the existing `Tape` — no second substrate" | CONFIRMED |
| `1F` COH18-015 / anti-pattern: totality `OnceCell<StructuralIndex>` on `ScanState` | `crates/core/src/grammar/generated/json.rs:701` `pub(crate) structural_index: ::core::cell::OnceCell<::simd_scan::StructuralIndex>` | CONFIRMED |
| `1F`: emitter names it "The probe substrate (OnceCell + helper)" | `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67` doc-comment verbatim | CONFIRMED |
| `1F`: `simd-scan` exports `{StructuralIndex, next_structural_at_or_after}`; `OnceCell` lives in the `crates/core` CONSUMER | `crates/simd-scan/src/lib.rs:68` `pub use index::{StructuralIndex, next_structural_at_or_after};` verbatim | CONFIRMED |
| `1F`: skinny carries ZERO `OnceCell`/`RefCell`/`Cell`/`static mut`/`thread_local` retained-across-call sidecar in runtime grammars | `rg` over `skinny/crates/runtime/src/grammars/` = 0 hits | CONFIRMED |
| `1F` anti-pattern: 7 css_l4 replicas md5-identical `b654562c…` | `md5 css_l4_*/generated.rs` = 7× `b654562ccff46ed62dd48e9ace325830` | CONFIRMED |

## The Lock-1 union honesty question (lens core) — re-adjudicated

The dispatch context (`T-P1-DISPATCH-CONTEXT.md:48`, §4) scopes `1A` to
"Substrate-layer evidence — Lock 1 … against `runtime/src/tape/`, `ir/src/`,
`runtime/src/grammars/`" — i.e. the SKINNY substrate paths. The totality
`crates/core` runtime substrate is assigned to `1C` (`:50` per-grammar runtime
module census) and `1F` (`:53` cross-corpus coherence + anti-pattern). `1A` is
not the inventory charged with the totality-tree probe; `1F` is.

`1A`'s `live_truth_method` (`1A-substrate-evidence.md:9`) enumerates ONLY
`skinny/crates/...` paths — `1A` is a self-declared SKINNY-substrate inventory.
It makes NO "BOTH trees" / corpus-wide / fleet-wide substrate-clean claim
(verified: `rg 'BOTH|both trees|corpus-wide|fleet-wide'` = 0). Its SPINE Net
sentence (`:171-174`) explicitly scopes the close to "JSON+CSS" — the skinny
kernel. The word "substrate-neutral" / "grammar-neutral" everywhere in `1A`
(`:38,:45,:96,:126,:137,:171`) means neutral ACROSS GRAMMARS (JSON and CSS
reusing one `Tape` + one sparse flag pair), NOT neutral across the
skinny-vs-totality TREES. `1A-SUB-024`'s spec claim cites `sk-v18/SPEC.md:1254`
(R-D / G4.2) and skinny impl paths; the verdict word "substrate-neutral
confirmed" answers the SK-V18 R-D question (is the skinny `Tape` grammar-neutral
enough to serve the un-forked generator?), which is precisely TRUE on disk.

`1F` is the firewall and it holds. `1F-anti-pattern.md:44` catalogues the
totality `OnceCell<StructuralIndex>` probe, classifies it Lock-1
(`substrate_target = structural-index PROBE`, `retention_lifetime =
generated_function`, the ADMISSIBLE per-parse class NOT the REJECT cross-call
class `LOCKS.md:139-149`), and carries the EXPLICIT fence "do NOT close
substrate-union 'BOTH trees' while this is unclassified." `1F-coherence-scan.md:104`
(COH18-015) names the renamed/parallel-scanner risk ACTIVE, not one-sided, and
routes it to the SK-V19 scanner-unification disposition. No inventory asserts
both trees are substrate-clean.

So the corpus, read as T-P2 will read it (the FULL packet, with 1A/1C/1F
co-scoped per the dispatch matrix), carries NO Track-1≡Track-2 dishonesty and NO
unfenced hidden coupling. The V5 CH5 "add a cross-reference row to 1A" REVISE
would marginally improve 1A's STANDALONE readability, but its absence does not
MISLEAD a T-P2 reader given (a) 1A's explicit skinny self-scope, (b) the absence
of any corpus-wide false close, and (c) 1F's load-bearing "do not close BOTH
trees" fence. Under the PROPORTIONATE standard ("a nit is a REVISE only if it
would mislead a T-P2 reader"), it is below the REVISE threshold. I therefore
ACCEPT where V5 REVISEd, and record the re-adjudication explicitly below.

## Direct skinny-side sweep for MISSED coupling (lens-mandated)

To guard against a false "clean" claim, I swept the skinny tree directly for any
retained/cross-call sidecar `1A`/`1F` might have failed to catalogue:

- `skinny/crates/runtime/src/grammars/` `OnceCell|RefCell|Cell<|static mut|lazy_static|thread_local` = ZERO. The totality `OnceCell` is correctly NOT present in skinny and correctly NOT attributed to skinny by any inventory.
- skinny `ParserState` carries exactly `{input, bytes, cursor, tape}` (`parser.rs:7-12`); no retained structural sidecar (1A-SUB-017 NO-OP `attach_structural_index` stub holds).
- skinny `tape/cursor.rs` (the R-D G4b view trait) is genuinely absent — 1A-SUB-024's "no `tape/cursor.rs` exists yet" is true on disk.

No skinny-side hidden coupling was missed. The only live cross-tree coupling is
the totality probe substrate + the two-scanner asymmetry, both fully caught and
fenced by `1F`.

## Findings

| id | disposition | finding | evidence / basis |
|---|---|---|---|
| CH5-V6-001 | ACCEPT | The phantom `<G>` census is EMPTY of non-test production instantiation; the decorative generic is NOT a second substrate. The Lock-1 union (`&'i Tape<'i>` + cursor) never touched it. `1A-SUB-023` is correct on disk. | All `EventGrammar`/`AnyGrammar` sites are defs / `ValueRef` field+impls / two witness defs / `event_grammar_tests.rs` (`#[cfg(test)]`). `tape/mod.rs:175,179`; `event_grammar.rs:4,17,19`. |
| CH5-V6-002 | ACCEPT | CSS retained parse holds exactly the existing `Tape` — no second skinny substrate; same sparse flag pair. No renamed/parallel substrate on the skinny tree. `1A-SUB-024` substrate-neutral close is grammar-neutral (JSON↔CSS), correctly bound to its skinny spec claim. | `css_l4_declaration_values/generated.rs:257` verbatim; skinny grammars carry ZERO `OnceCell`/`Cell`/`RefCell` retained sidecar (live sweep = 0). |
| CH5-V6-003 | ACCEPT | The totality `crates/core` `OnceCell<StructuralIndex>` "probe substrate" is fully caught by `1F`, classified `generated_function` (the ADMISSIBLE per-parse class, NOT the REJECT cross-call class), and fenced with the explicit "do NOT close substrate-union 'BOTH trees'" instruction. No Track-1≡Track-2 dishonesty. | `1F-anti-pattern.md:44`; `1F-coherence-scan.md:104` COH18-015; live `crates/core/.../json.rs:701` + emitter diction `support.rs:67`; `LOCKS.md:139-149` class boundary. |
| CH5-V6-004 | ACCEPT | `1F`'s crate attribution is honest: the `OnceCell` lives in the `crates/core` CONSUMER, not `simd-scan`; `simd-scan` exports only `{StructuralIndex, next_structural_at_or_after}`. The renamed/parallel-scanner risk is correctly flagged ACTIVE (skinny exposes a parallel `StructuralIndex` + skinny-only `parity_hash`; lacks only the probe API + cross-parse retention). | `simd-scan/src/lib.rs:68`; `support.rs:67`; `rg next_structural_at_or_after skinny/crates/bbnf-simd/src` = 0; skinny `OnceCell` in runtime grammars = 0. |
| CH5-V6-005 | ACCEPT (re-adjudicates V5 CH5-V5-006) | `1A` not cataloguing the totality probe substrate is NOT a REVISE-grade gap at the near-converged state. `1A` self-scopes to skinny (`live_truth_method:9` enumerates only `skinny/crates/`), makes NO "BOTH trees"/corpus-wide substrate-clean claim, and its SPINE Net (`:171-174`) explicitly scopes to "JSON+CSS." The totality probe is `1F`-owned and `1F`-fenced per the dispatch matrix (`T-P1-DISPATCH-CONTEXT.md:48,:50,:53`). A T-P2 reader of the full packet is not misled. Below the PROPORTIONATE REVISE threshold. | `1A-substrate-evidence.md:9` (skinny-only method), `:171-174` (JSON+CSS Net); `rg 'BOTH\|corpus-wide\|fleet-wide' 1A` = 0; `1F-anti-pattern.md:44` "do NOT close BOTH trees" fence. |
| CH5-V6-006 | ACCEPT (re-adjudicates V5 CH5-V5-007) | `1A-SUB-024`'s closure word "substrate-neutral confirmed" does NOT over-reach into a corpus-wide close. In `1A` "substrate-neutral"/"grammar-neutral" uniformly means neutral ACROSS GRAMMARS (the SK-V18 R-D question), evidenced by the note "The substrate kernel is already grammar-neutral … CSS reuses the same sparse pair … R-D adds a trait over THIS kernel." It is true on disk and scoped to the skinny kernel by its own spec-claim citation (`sk-v18/SPEC.md:1254`) and skinny impl paths. Not misleading; below the REVISE threshold. | `1A-substrate-evidence.md:96` (SUB-024 row); uses of "grammar-neutral" at `:38,:45,:126,:137,:171`; spec claim cites `sk-v18/SPEC.md:1254` + skinny `tape/mod.rs:94` / `css_l4_.../generated.rs:257`. |
| CH5-V6-007 | ACCEPT | No GENUINE reject: every spot-verified path:line resolves at HEAD; no inventory states anything FALSE on disk under this lens. The skinny-side direct sweep found NO missed sidecar; the totality probe and two-scanner asymmetry are LIVE-caught by `1F`. The Lock-1 union is honoured by the corpus; the firewall holds. | Full spot-verify table above; `1F` fence rows; skinny `OnceCell`/`Cell` sweep = 0; `tape/cursor.rs` absent; 7 css_l4 md5 `b654562c` live. |

## Re-adjudication note (V5 → V6)

V5 CH5-V5-006/007 named one root under two surfaces and proposed a single
corrective edit to `1A`. That edit was NOT folded into the committed near-converged
inventory (`097c4dd90`), and `1A` still contains zero `crates/core`/`OnceCell`/
`probe substrate` references. I do NOT carry it forward as an open REVISE. On
fresh proportionate review the absence is not misleading: `1A` is explicitly
skinny-scoped, asserts no corpus-wide substrate-clean close, and the totality
probe is owned and fenced by `1F` per the dispatch matrix. Re-flagging it would
be over-demanding for a near-converged inventory whose own scope statement and
whose sibling `1F` already discharge the firewall. This is an honest ACCEPT, not
a re-stamp: the underlying surfaces were re-verified live this cycle, and the
re-adjudication is recorded explicitly (CH5-V6-005, CH5-V6-006) rather than
silently dropped.

No source edit, inventory edit, staging, or commit was performed for this CH5
report.

TALLY accept=7 revise=0 reject=0
