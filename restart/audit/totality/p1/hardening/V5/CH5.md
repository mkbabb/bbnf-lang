---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-TOTALITY-EXCAVATION
cycle: V5-SKV18 (challenge cycle V5)
disposition: REVISE
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
spec_surfaces_cross_read:
  - skinny/crates/runtime/src/tape/mod.rs
  - skinny/crates/runtime/src/tape/event_grammar.rs
  - skinny/crates/runtime/src/tape/event_grammar_tests.rs
  - skinny/crates/runtime/src/grammars/json/{parser,generated,scan,config}.rs
  - skinny/crates/runtime/src/grammars/css_l4_declaration_values/{config,generated}.rs
  - skinny/crates/codegen/src/json_typed_direct.rs
  - skinny/crates/bbnf-simd/src/lib.rs
  - crates/core/src/grammar/generated/json.rs
  - crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs
  - crates/simd-scan/src/lib.rs
---

# CH5 HIDDEN COUPLING — SK-V18 T-P1 CHALLENGE (cycle V5)

## Verdict

REVISE.

This is a substantive re-run, not a re-stamp. The existing `V5/CH5.md` on disk
was an SK-V14/V15 artefact reviewing FNV-only couplings; the live inventories
under `restart/audit/totality/p1` were regenerated this pass (Jun-01, frontmatter
`cycle: V5-SKV18-totality`) and carry a DIFFERENT and sharper coupling surface:
the totality-tree `OnceCell<StructuralIndex>` probe (COH18-015), the skinny
`parity_hash` diagnostic, the two-scanner asymmetry, and the phantom `<G>`
census. I spot-verified the load-bearing rows against live code and they hold —
but the substrate inventory `1A`, which the CH5 firewall charges with the Lock-1
union, has a genuine seam: it never references the single surface the totality
emitter ITSELF labels "the probe substrate." That seam is REVISE-grade (a missing
cross-reference, not a false claim), because `1F` DID catch the coupling — so it
is not hidden corpus-wide, only absent from the inventory the lens names.

The CH5 firewall (`restart/prompts/totality/PASS-1-EXCAVATION.md`): no parallel
substrate, sidecar producer, renamed-scanner Lock-1 violation, or Track-1≡Track-2
dishonesty may pass uncatalogued, and the substrate inventory `1A` honours the
Lock-1 union. I judge each finding ACCEPT / REVISE / REJECT below.

## Spot-verification of load-bearing rows (all confirmed at HEAD)

| claim (inventory) | live evidence | result |
|---|---|---|
| `1A` SPINE: phantom `<G>` non-test production-instantiation census is EMPTY (1A-SUB-023) | every `EventGrammar`/`AnyGrammar` site is the trait def (`skinny/crates/runtime/src/tape/event_grammar.rs:4`,`:17`,`:19`), the `ValueRef` field (`tape/mod.rs:175`,`:179`), the two witness DEFS (`grammars/json/event_grammar_witness.rs:4`,`:17`; `grammars/sheets_witness/event_grammar_witness.rs:4`,`:16`), or `tape/event_grammar_tests.rs` (`:18`-`:89`, all `#[cfg(test)]` / `const _: fn()`). Zero production instantiation. | CONFIRMED |
| `1A` self-correction: `Tape::id` at `:170`, not the prior `:172` | `skinny/crates/runtime/src/tape/mod.rs:170` `pub fn id(&self) -> TapeId` | CONFIRMED |
| `1A-SUB-002`: `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` over `&'doc Tape` + `u32` cursor; `_grammar: PhantomData<fn()->G>` at `:179` | `tape/mod.rs:175`-`179` verbatim; `_kind` `:178`, `_grammar` `:179` | CONFIRMED |
| `1A-SUB-024` SPINE: CSS retained "Holds exactly the existing `Tape` — no second substrate" (generated.rs:257) | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:257` doc-comment verbatim; `CssDocument<'input>{ tape: Tape<'input> }` | CONFIRMED |
| `1A-DIV-006`: third cursor carrier `DirectParser.cursor` (field `:671`, struct `:668`), rollback `let checkpoint = parser.cursor` `:361` | `skinny/crates/codegen/src/json_typed_direct.rs:668`-`671`,`:361` verbatim | CONFIRMED |
| `1A-SUB-017`: skinny `attach_structural_index` is a NO-OP stub; `ParserState` retains no structural sidecar | `grammars/json/generated.rs:12`-`15` (`debug_assert_eq!` then `let _ = state;`); `ParserState{input,bytes,cursor,tape}` only (`parser.rs:7`-`12`) | CONFIRMED |
| `1A-SUB-016`/`DIV-005`: CSS `config.rs` emits ZERO W7/BackendShape/substrate_target | `rg -c 'W7_\|BackendShape\|substrate_target' css_l4_declaration_values/config.rs` = 0; JSON triad present `json/config.rs:22`-`26` | CONFIRMED |
| `1F` COH18-015 / anti-pattern: totality `OnceCell<StructuralIndex>` on `ScanState`, lazy `get_or_init` | `crates/core/src/grammar/generated/json.rs:701`-`704` field; `ensure_structural_index` `:719` `get_or_init` | CONFIRMED |
| `1F`: `simd-scan` exports `{StructuralIndex, next_structural_at_or_after}` only — `OnceCell` lives in the `crates/core` CONSUMER | `crates/simd-scan/src/lib.rs:68` verbatim; emitter "The probe substrate (OnceCell + helper)" `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67` | CONFIRMED |
| `1F`: skinny carries ZERO `next_structural_at_or_after` and ZERO `OnceCell` | `rg` both = 0 in `skinny/crates/{bbnf-simd,runtime}/src` | CONFIRMED |
| `1F` anti-pattern: `parity_hash` reached ONLY via `scalar_parity_report`, not the `parse_json`/`parse_direct` hot path | `grammars/json/scan.rs:38`,`:43` + one test `:301`; `bbnf-simd/src/lib.rs:94` | CONFIRMED |
| `1F`: 7 css_l4 replicas md5-identical `b654562c…` | `md5 css_l4_*/generated.rs` = 7× `b654562ccff46ed62dd48e9ace325830` | CONFIRMED |
| totality probe lifetime: per-parse `&mut ScanState`, `generated_function` class, NOT cross-call | `ScanState::new()` per parse `crates/core/src/grammar/generated/json.rs:3442`; threaded `&mut` | CONFIRMED |

## The Lock-1 union honesty question (lens core)

CH5 charges the SUBSTRATE inventory `1A` with the Lock-1 union: no catalogued
state may imply a parallel substrate / sidecar producer / renamed scanner, and no
Track-1≡Track-2 dishonesty may pass. The corpus splits the work: `1A` owns the
skinny substrate kernel; `1C`/`1D`/`1E`/`1F` audit BOTH trees (`1E`
live_truth_method: "in BOTH trees (skinny/crates = SK-V18 benched witness;
crates/core = totality adoption target)"; `1C:24` "Two runtime trees exist with
OPPOSITE generation truth").

`1F` is the firewall that holds. It catches every live coupling under this lens:
- COH18-015 + anti-pattern row: the totality `OnceCell<StructuralIndex>` probe is
  classified `substrate_target = structural-index PROBE`, `retention_lifetime =
  generated_function` (the ADMISSIBLE per-parse class, NOT the REJECT cross-call
  class `LOCKS.md:139-149`), and fenced — with the explicit instruction "do NOT
  close substrate-union 'BOTH trees' while this is unclassified."
- The two-scanner asymmetry is named ACTIVE, not one-sided: skinny `bbnf-simd`
  exposes a full `StructuralIndex` + a skinny-only `parity_hash`; totality adds
  `next_structural_at_or_after` + cross-parse `OnceCell` retention. The
  renamed/parallel-scanner risk is flagged for SK-V19 scanner-unification.
- The phantom `<G>` is named decorative, not a second substrate.

So there is NO Track-1≡Track-2 dishonesty in the corpus: no inventory claims
both trees are substrate-clean. `1F` and `1C` both refuse the "BOTH trees" close,
and `1A` itself never asserts it.

The seam is `1A` standing alone. `1A` is the inventory the lens names as the
Lock-1 union honourer, yet:
- Its SPINE concludes "Net: Lock 1 is partly honored. One retained tape
  (grammar-neutral kernel) for JSON+CSS; admitted direct/fact/transient planes;
  but the union is not yet ONE typed cursor + ONE `TapeEmit`/`DirectBuild`
  schedule" — a SKINNY conclusion whose only named open items are the
  cursor-unification (Ω-A) and the Lock-14 generality-vehicle contradiction.
- Its Divergences (DIV-001..008), Gaps (G1-G6), and Open Questions (UNK-001..005)
  contain NO row for the totality `OnceCell` probe substrate — the single surface
  the totality emitter ITSELF calls "the probe substrate" (`support.rs:67`).
- `1A` mentions `crates/core/` ZERO times (verified: `rg 'crates/core' 1A` = 0).

That is not dishonesty — `1A` correctly scopes to skinny and the coupling is
caught by `1F` — but it IS a REVISE-grade gap in the inventory the lens charges:
a standalone reader of the substrate inventory would see "Lock 1 partly honored"
with no pointer to the renamed/parallel scanner in the adoption-target tree. The
honest fix is one cross-reference row, not a re-scope.

## Findings

| id | disposition | finding | evidence / correction |
|---|---|---|---|
| CH5-V5-001 | ACCEPT | The phantom `<G>` census (1A-SUB-023 SPINE) is EMPTY of non-test production instantiation. The decorative generic is NOT a second substrate; the Lock-1 union (`&'i Tape<'i>` + cursor) never touched it. | Live census: all `EventGrammar`/`AnyGrammar` sites are defs / `ValueRef` field / two witness defs / `event_grammar_tests.rs` (`#[cfg(test)]`). `tape/mod.rs:175`,`:179`; `event_grammar.rs:4`; `event_grammar_tests.rs:18`-`89`. |
| CH5-V5-002 | ACCEPT | CSS retained parse holds exactly the existing `Tape` — no second skinny substrate; same sparse flag pair. No renamed/parallel substrate on the skinny tree. | `css_l4_declaration_values/generated.rs:257` verbatim "Holds exactly the existing `Tape` — no second substrate"; `ParserState` carries no sidecar (`parser.rs:7`-`12`); `attach_structural_index` NO-OP (`generated.rs:12`-`15`). |
| CH5-V5-003 | ACCEPT | The skinny `parity_hash` diagnostic is NOT a hidden equality arbiter / retained sidecar: it is reached ONLY via `scalar_parity_report`, off the `parse_json`/`parse_direct` hot path. `1F` newly cites it and extends the close-gate grep to catch it. | `grammars/json/scan.rs:38`,`:43` + test `:301`; `bbnf-simd/src/lib.rs:94`; `1F-anti-pattern.md:42` row + extended grep. |
| CH5-V5-004 | ACCEPT | The corpus carries NO Track-1≡Track-2 dishonesty: `1F` and `1C` both REFUSE the "BOTH trees" substrate close; `1A` never asserts it. The totality `OnceCell` probe is fenced per-parse (`generated_function`), the ADMISSIBLE class, not the REJECT cross-call class. | `1F-anti-pattern.md:44`,`:51`-`54`; `1F-coherence-scan.md:104` COH18-015; live lifetime `crates/core/.../json.rs:3442` per-parse `ScanState::new()`. |
| CH5-V5-005 | ACCEPT | `1F`'s crate attribution is honest: the `OnceCell` lives in the `crates/core` CONSUMER, not `simd-scan`; `simd-scan` exports only `{StructuralIndex, next_structural_at_or_after}`. The renamed/parallel-scanner risk is correctly flagged ACTIVE (skinny lacks the probe API + retention but exposes a parallel `StructuralIndex` + skinny-only `parity_hash`). | `simd-scan/src/lib.rs:68`; emitter diction `support.rs:67`; `rg next_structural_at_or_after skinny/crates/bbnf-simd/src` = 0; `OnceCell` skinny = 0. |
| CH5-V5-006 | REVISE | The SUBSTRATE inventory `1A` — the inventory CH5 charges with the Lock-1 union — does NOT catalogue the totality `OnceCell<StructuralIndex>` probe that the totality emitter itself labels "the probe substrate." `1A`'s SPINE/DIV/Gaps/UNK present a skinny-only "Lock 1 partly honored" close with no cross-reference to the renamed/parallel scanner in the adoption-target tree. Caught by `1F`, so REVISE not REJECT. **Correction (file: `restart/audit/totality/p1/1A-substrate-evidence.md`):** add ONE Divergence or Gap row (e.g. `1A-DIV-009` or `G7`) cross-referencing `1F-coherence-scan.md:104` (COH18-015) and `1F-anti-pattern.md:44`: "Totality `crates/core/src/grammar/generated/*.rs` carry a per-parse `OnceCell<StructuralIndex>` probe the emitter names 'the probe substrate' (`support.rs:67`); OUT of skinny substrate scope, fenced `generated_function`, routed to SK-V19 scanner-unification; the substrate-union 'BOTH trees' close stays OPEN." Also append to the SPINE Net sentence "(skinny tree; the totality `crates/core` probe substrate is 1F-owned and unclosed)." | `1A` `rg 'crates/core\|OnceCell\|next_structural_at_or_after\|ScanState\|probe substrate'` = ZERO hits; SPINE `1A-substrate-evidence.md:171`-`174`; the surface is live `crates/core/.../json.rs:701`-`704` + `support.rs:67`. |
| CH5-V5-007 | REVISE | `1A-SUB-024` carries the closure phrase "implemented (substrate-neutral confirmed)" for the substrate kernel. The phrase is true for the SKINNY kernel but, unqualified, reads as a corpus-wide substrate-neutrality close — exactly when the adoption-target tree carries an emitter-named "probe substrate." The verdict word over-reaches its skinny scope. **Correction (file: `restart/audit/totality/p1/1A-substrate-evidence.md`, row 1A-SUB-024 note):** scope the closure word — "implemented (skinny substrate-neutral confirmed; totality `crates/core` probe substrate out of scope, see 1F COH18-015)". This mirrors the discipline `1C` already applied to its own C12 row (downgraded a closure word "per CH6-V4-007" rather than letting `@generated` headers carry a close). | `1A-substrate-evidence.md:96` (SUB-024 note) + `:26` frontmatter counts SUB-024 as `implemented (substrate-neutral confirmed)`; contrast `1C-runtime-evidence.md:43` C12 closure-word downgrade discipline; live `support.rs:67`. |

REVISE count: 2 of 7 = 28.6%. Two findings (CH5-V5-006, CH5-V5-007) name the
same root — `1A`'s skinny-scope substrate close not fencing the totality probe
substrate — under two distinct surfaces (a missing catalogue row; an
over-reaching closure word). They share one corrective edit surface in
`1A-substrate-evidence.md` but are enumerated separately because each is a
discrete, independently-checkable defect the lens must surface.

No REJECT: every spot-verified path:line resolves at HEAD; no recalled, false, or
uncited claim was found. The couplings the lens guards against (the totality
probe substrate, the parity_hash diagnostic, the two-scanner asymmetry) are all
LIVE-caught by `1F` — the firewall holds; the gap is one inventory's silence.

No source edit, inventory edit, staging, or commit was performed for this CH5
report.

TALLY accept=5 revise=2 reject=0
