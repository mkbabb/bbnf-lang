# SK-V15 P3-E: Pre-Blocked Route Ledger

Pass: S-P3 Synthesis-Plan. Cycle: V2.
Date: 2026-05-28.
Scope: per-wave REDRESS pre-block ledger for SK-V15 W0..W11.
Output: this file.
Pass Alpha goalset: PRUNE-then-REBUILD SK-V15, with CSS honesty, Lock 14/16
coverage, codegen neutrality, Pattern H discipline, CSS Value API, Decision
Engine activation, all five BackendShape lowerers, and FNV quarantine.
Candidate pool: S-P2 V3 locked survivors only: grammar-neutral byte-set and
classifier operations, local string/literal and UTF-8 validation surfaces,
per-grammar escape/segment templates, same-tape capacity/flag/fact projection,
local mask-to-tape writers, and direct cursor/FIRST-set templates.

## Section 1 - Synthesis

P3-E treats REDRESS as a negative route map, not just a list of failed
patches. The ledger has three classes of pre-blocks:

1. Global honesty pre-blocks that apply to every SK-V15 wave: no broadcast
   admits, no wrong-plane comparator, no Track 1 == Track 2 collapse, no
   scaffold presented as load-bearing, no gate relabel, no tiny-fixture Mbps
   as production evidence, no fake generated status, no hidden sidecar
   substrate, and no bench-only FNV closed-enum migration into production.
2. Receiver-specific pre-blocks that match W0 baseline/telemetry through
   W11 close reconciliation.
3. Conditional routes that may be revisited only under materially different
   framing with fresh SK-V15 P1 evidence, scalar/parity proof, same-wave
   consumer, and a measurable gate. Reusing the old label or moving the same
   body to a new file is not a new route.

The binding historical block list shared by P3-B/P3-C/P3-E/P3-F/SPEC and
DISPATCH is:

`28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration`

Old-framing reuse is rejected.

## Section 2 - Deliverable

### Global Pre-Blocks For Every Wave

| Route | Pre-blocked entries or class | What must not happen in SK-V15 |
|---|---|---|
| Tiny-string replay | REDRESS 28+33, 60, 72, 83 | Do not rewire the old active 16-byte/Class A NEON tiny-string route, delete the scalar early-out, or present StringBlock16 as new work. REDRESS 72 admits only the specific scalar cap-16 retained split; it does not authorize global cap changes or NEON replay. |
| Parse-time sidecars and second scanners | REDRESS 50-55, 96-98 | Do not add aux projection tables, whitespace bitmaps, parser-owned structural cursors, class columns, retained structural indexes, streaming cursor state, public UnionTape, second tapes, decoded scratch, or cross-call classifier/quote/escape carry. |
| Decoded-string/materializer repeats | REDRESS 54, 55, 66-69, 82, 242-245 | Do not repackage decoded stats, quote-source fused hashing, source-hook folding, parser-owned decoded scratch, byte-output materialization, semantic string fact hashing, decoded-codepoint retries, fixed-shape unicode floors, or single-quartet Unicode production promotion as a new close. |
| Retained parse shortcut repeats | REDRESS 60-72, 84, 246-247 | Do not reopen scalar tiny deletion, wide string scans, object next-key carry, value-byte/object-pair control compaction, parse-only structural streams, string64 retries, or global Track 2/direct cap changes under a new helper name. |
| Numeric/digit fallback | REDRESS 80 | Do not claim mantissa-widen, f64 fallback removal, UDOT digit routes, or number policy changes without fresh P1 evidence naming a BBNF-side numeric hot leaf and a same-wave consumer. |
| PMULL/CSSC production promotion | REDRESS 88, 89 | Do not promote PMULL prefix-XOR or CSSC CTZ/bulk consumers into hot production paths from ISA availability or checkasm alone. |
| Wave-graph deletion cycle | REDRESS 183/184/209-213 | Do not delete or retire a provider/runtime/template before the replacement generator/provider is proven no later than the delete wave. Static centralization of committed generated text is the same rejected route. |
| CSS broadcast and wrong comparator | REDRESS 215 | Do not project one CSS timing tuple to 24 feature rows, compare a four-counter/fact-stream Track 1 to an eager-DOM/CSSOM plane, or count cssparser/lightningcss sidecars as same-workload proof until Track 1 emits comparable typed CSS output. |
| Track 1 == Track 2 dishonesty | REDRESS 34 and later Track 2 independence gates | Do not measure the same private parser/source path twice and label it Track 1/Track 2. Self-attested independence is not enough. |
| Gate relabel or paper close | REDRESS 78, 81, 91, 94 lessons; PASS-IMPL addenda | Do not move a row by schema/gate relabel, source-only parity, stale run-id acceptance, or metadata shape unless the planned wave is explicitly a non-admitting telemetry/source wave. |
| FNV closed-enum migration | W11L/W11N/W11O audit class | Do not migrate FNV-keyed closed-enum arbiters or sidecars that share Track 1's closed enum into `crates/core/src/runtime` or generic codegen. Bench-only quarantine is mandatory. |

### W0 - Baseline And Telemetry Lock

Pre-blocked routes:

- CSS W8R broadcast rows as admissions: the 24 CSS rows are either one
  diagnostic aggregate or independently measured after typed-output repair.
- Gate relabel: no row may become admitted because schema-v3, row id,
  comparator label, or run id changed.
- Tiny fixture Mbps and CANONICAL_FIXTURE/profile-template shortcuts:
  fixtures only, never production Mbps evidence.
- Track 1 == Track 2: W0 telemetry must keep source provenance and Track 2
  independence separate and fail closed on shared-source evidence.
- Lock 14/16 exclusion silence: W0 must expose exclusions rather than treat
  omitted roots as clean.

W0 may only close on a baseline/gate transcript. It must not create new
behavioral admission.

### W1 - CSS Admission Honesty

Pre-blocked routes:

- One aggregate CSS measurement cannot admit 24 conceptual feature rows.
- `CSS_GENERATED_RS` string literal as generated code: moving the hand-written
  tokenizer between files is fake generation.
- Brace-counter `CssFullParseSummary` as full parse/value output.
- sonic eager-DOM, lightningcss eager/full-CSSOM, or any eager/full-DOM
  comparator plane versus Track 1 fact streams or four counters.
- cssparser/lightningcss sidecars as proof if Track 1 emits a cheaper output
  plane.
- Provider deletion before same-wave or earlier typed rebuild provider proof
  under REDRESS 183/184/209-213.

Admissible only under different framing: a CSS row can re-admit only as one
honest aggregate diagnostic row or as independently timed feature rows with
typed CSS value/document output and same-workload comparator evidence.

### W2 - Lock 14 And Lock 16 Gate Restoration

Pre-blocked routes:

- Silent scan-root or checkasm/report exclusions.
- A self-exempting Lock 14/16 grep that cannot see
  `runtime_generator.rs`, `grammar_provider.rs`, JSON direct/typed templates,
  CSS provider material, or its own exclusion list.
- Gate-only repair that marks known leak files clean by omission.
- PMULL/CSSC body fills as covered by checkasm while production rows remain
  rejected.
- x86/AVX-512 diagnostic rows as SK-V15 admission anchors.

Admissible only under different framing: the gate may surface known exclusions
as findings and fail close. It may not declare a generic surface clean until
the surface has actually been scanned or the exclusion is reported as a
blocker.

### W3 - Codegen Leak Abrogation

Pre-blocked routes:

- Per-grammar provider modules as the codegen contract.
- Static centralization of provider bodies under a new filename.
- Grammar-family runtime modes, root `RuntimeStyle` fanout, 7-arm CSS profile
  tables, JSON-byte literals in generic passes, or JSON/CSS mode branches
  inside generic crates.
- Deleting providers/templates before the provider-free generator body exists
  and is consumed under REDRESS 183/184/209-213.
- Reading committed generated output as the source-consuming generator.

Admissible only under different framing: provider deletion becomes admissible
only after a source-consuming grammar/metadata frontend and provider-free
generator body emit the needed runtime bytes and the delete wave proves the
replacement no later than deletion.

### W4 - Pattern H Generated Discipline

Pre-blocked routes:

- Header-only Pattern H close: adding `@generated` text without generator
  ownership does not close.
- Fake generated headers that let a detector bypass hand-written core runtime
  files.
- Changing the 67-file census without proving root runtime regeneration.
- Static centralization of CSS or other bespoke runtime bodies.
- Deleting or replacing core runtime files before the generator can
  destructively restore them.

Admissible only under different framing: the line-1 generated marker is a
gate, not the proof. The proof is that runtime files round-trip from the
generator, preserve the 67-file contract, and do not require hand edits in
core runtime owner paths.

### W5 - CSS Typed Value Provider

Pre-blocked routes:

- Fact-stream-only CSS `parse()` as a Value API.
- `CssFullParseSummary` counters as a CSSOM/value proxy.
- `CSS_GENERATED_RS` or byte-identical generated.rs clones as the production
  parser source.
- Same CSS broadcast/wrong-plane/tiny-fixture routes from W1.
- Parser-owned decoded/string sidecars or materializer repeats while building
  CSS string/value support under REDRESS 50-55, 60-72, 82-84, and 242-247.
- Per-grammar provider modules returning as the Value provider.

Admissible only under different framing: W5 may provide typed CSS value,
document, view, and visitor output, but it does not set CSS SOTA floors from
W8R metrics and does not retire old proof unless W6-grade retime also lands.

### W6 - CSS Same-Workload Retime And Old-Proof Retirement

Pre-blocked routes:

- Reusing `2319.041`, `2362.037`, or `929.281` as live CSS floors.
- Retiring `CSS_GENERATED_RS`, fact-stream `parse()`, `CssFullParseSummary`,
  or brace-counter proof before fresh typed provider output is measured.
- Treating cssparser/full-parse sidecars as same-workload if Track 1 emits
  typed value/document output or vice versa.
- lightningcss admission before Track 1 emits comparable CSSOM/value output.

Admissible only under different framing: CSS SOTA comparison can be reattempted
only after Track 1 emits typed CSS value/document/view output. cssparser is the
near-term same-workload comparator; lightningcss becomes same-workload only
when Track 1 emits a comparable CSSOM/value product.

### W7 - Decision Engine Spine

Pre-blocked routes:

- Zero e-graph rewrite rules, tautological CSP, or a decision report that
  records facts without allowing them to drive selection.
- Grammar-named CSP facts such as CSS/JSON status fields in generic decision
  records.
- Gate relabel where "CSP facts present" substitutes for runtime row movement
  or generated runtime diff.
- Retained union-substrate or new BackendShape routes that reopen
  REDRESS 96-98 under decision-engine vocabulary.

Admissible only under different framing: the wave must prove at least one real
rewrite, non-tautological CSP pruning, grammar-neutral facts, and a generated
selection/report consumer.

### W8 - EagerTape And OffsetTape Lowerers

Pre-blocked routes:

- Label-string lowerer scaffolds.
- `todo!`, pass-through shells, or formatted placeholder strings presented as
  generated output.
- Lowerer fixtures that pass against the old scaffold.
- Generic lowerer edits without CSS L4 plus Sheets or BBNF-self proof when
  the generic path can affect multiple grammars.

Admissible only under different framing: EagerTape and OffsetTape lowerers
must emit runtime-relevant generated diffs or gate-consumed rejected
alternatives.

### W9 - EventTape, SinkOnly, CollapsedStage Lowerers

Pre-blocked routes:

- Label-string lowerer scaffolds for EventTape, SinkOnly, or CollapsedStage.
- EventTape as sidecar event vector, sixth BackendShape, retained
  parser-owned stream, public substrate API, or alternate document projection.
- All-five BackendShape close before the all-five gate sees
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` and no sixth
  variant.

Admissible only under different framing: EventTape is only one of the five
BackendShape lowerings. It cannot reopen retained sidecar routes.

### W10 - FNV Closed-Enum Quarantine

Pre-blocked routes:

- FNV-64 `(fingerprint, length)` closed-enum product migration into production
  runtime or generic codegen.
- Sidecars that share Track 1's closed enum, making the strict-product
  comparator blind to collisions or closed-vocabulary overfit.
- Direct digest relabel as typed product proof.
- Track 1/Track 2 shared-source or same-parser shortcuts.
- Harness hash/checksum top leaves as parser primitive evidence.

Admissible only under different framing: bench-only closed-enum fixtures may
remain quarantined if the comparator is hardened with an independent non-enum
arbiter and adversarial collision/equivalence fixtures. Production migration is
blocked unless a later Alpha/P1 cycle supplies a new product contract and
independent comparator that does not share the closed enum.

### W11 - Close Reconciliation And PASS-IMPL V2 Handoff

Pre-blocked routes:

- Closing from docs-only claims, future-wave promises, stale generated diffs,
  stale measurements, warm benches, or x86 diagnostics.
- Treating SK-V16 routing as proof for an unresolved SK-V15 miss.
- Leaving dependency-table rows orphaned, unproved, or missing intrinsic-block
  evidence at HEAD.

PASS-IMPL V2 closes SK-V15 only when each axis is ACCEPT at HEAD or is
recorded as a row-level intrinsic block with HEAD command output, generated
diffs/manifests where relevant, strict parity/checkasm where relevant, and
cold measurements where behavior changed. SK-V16 routing is routed remainder
after that proof; it is not close evidence and cannot substitute for an
SK-V15 repair.

## Section 3 - Falsifiability Binding

P3-E's gate is the CH3 regression ledger that final W0..W11 plans must
consume. A wave fails this ledger if any plan or redress:

- cites a pre-blocked REDRESS entry as positive evidence without the
  materially different framing named above;
- omits the relevant pre-blocks from its wave plan;
- uses stale CSS broadcast rows, tiny fixture/profile-template Mbps, gate
  relabels, or wrong-plane comparator rows as admission evidence;
- claims a primitive/kernel without scalar reference, parity/checkasm where
  applicable, same-wave consumer, and row-level maintain gates;
- deletes, retires, or relabels a provider/runtime/template before its
  rebuild provider is proven no later than that wave;
- reopens REDRESS 242-247 without fresh P1 evidence and a materially
  different representation;
- presents documentation-only, header-only, or scaffold-only movement as a
  close;
- treats SK-V16 routing as a substitute for SK-V15 close proof.

## Section 4 - Pre-Blocked Routes Requiring Fresh P1 Evidence

The following are not permanently impossible, but SK-V15 S-P3 must not plan
them under the old framing:

| Route family | Old blocked framing | Minimum fresh framing |
|---|---|---|
| Numeric/digit routes | REDRESS 80 mantissa/f64 fallback and P2 rejected digit-run/UDOT inventory. | Fresh P1 must name a BBNF-side numeric hot leaf, not comparator decimal work or schema-shaped wrapper cost; the plan must isolate grammar number policy outside the generic primitive and name a same-wave consumer. |
| PMULL prefix-XOR | REDRESS 88 default hot-body promotion from correct asm/checkasm. | Fresh P1 must name prefix-XOR as the current row bottleneck and bind a narrow consumer with row maintain gates; host feature availability alone is insufficient. |
| CSSC CTZ/bulk emit | REDRESS 89 production next-bit/bulk consumer promotion. | Fresh P1 must name mask-to-position emission as a bottleneck and prove the consumer is not the rejected bulk path; checkasm-only proof is insufficient. |
| Tiny-string NEON/StringBlock | REDRESS 28+33/83 active dispatch and StringBlock16 tiny probe. | Fresh P1 must identify a new literal-span boundary, use scalar-first parameterized delimiters, and prove lower overhead before any SIMD wiring. |
| Escaped-string materializers | REDRESS 54/55/66-69/82 and 242-245 under current digest/product contracts. | Fresh P1 must identify a different output/product representation or per-grammar segment surface. Another allocation-removal, decoded-hash, parser-owned scratch, decoded-codepoint retry, fixed-shape unicode floor, or one-quartet materializer is blocked. |
| Parse-only structural/string64 | REDRESS 246-247 structural-stream and string64 retries. | Fresh P1 must prove a different source/output contract with scalar oracle, same-wave consumer, and row movement; replay under old parse-only framing is blocked. |
| Retained structural substrate | REDRESS 51/53/96-98 retained cursor/class/union substrate. | Fresh P1 plus a new Alpha/G-Omega contract is required for retained sidecars. Same-call local masks that write the existing tape/sink may be planned only with scalar/parity and same-wave row movement. |
| CSS SOTA/readmit | REDRESS 215 one-measurement broadcast and brace-counter/full-CSSOM mismatch. | W1 demotion, W5 typed CSS value output, W6 distinct measurements or explicit aggregate row, same-workload cssparser/lightningcss comparator, and anti-broadcast telemetry. |
| FNV closed enum | W11L/W11N/W11O bench-only closed-enum comparator coupling. | W10 bench quarantine plus independent non-enum arbiter and adversarial fixtures. Production migration needs a future product contract and independent sidecars. |

## Section 5 - Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
- `restart/prompts/ORCHESTRATOR.md` section 3W and section 3Z.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
- `restart/prompts/skinny/PASS-IMPL-OVERFIT-AUDIT.md`.
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v15/HANDOFF.md`.
- `restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/redeploy/`.
- `skinny/REDRESS.md`.
