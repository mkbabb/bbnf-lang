# CH5 — HIDDEN-COUPLING (cycle V2)

Lens: CH5 Hidden Coupling per PASS-ALPHA §3 + ORCHESTRATOR §3W.
Subject: Pass Alpha SK-V17 artefacts — `research/alpha/{alphaA..alphaE}.md` +
`SYNTHESIS.md` + `HANDOFF.md` (α-F output = SYNTHESIS.md + HANDOFF.md per
PASS-ALPHA §2; there is no separate alphaF file).
Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25` (git rev-parse
confirmed this cycle).
Focus: no parallel substrate / sidecar / renamed-scanner / Track1==Track2
dishonesty; tape+projection is ONE substrate (Lock 1); cursor-API lazy view does
not retain a parallel eager arena.

---

## §0 — Ground truth (independently re-verified at `1c5bd7a25`, this cycle)

Every disposition below cites a fact re-greped against the benched **skinny** tree
this cycle (not inherited from V1):

| Fact | Evidence (re-verified) |
|---|---|
| Skinny tape IS one substrate: `Tape` :94, `ValueRef` :175, `PayloadArena` :38, `TapeBuilder` (assembler.rs :42), `DocumentView` trait :227, `EventGrammar`/`AnyGrammar` traits | `skinny/crates/runtime/src/tape/mod.rs:38,94,175,227`; `assembler.rs:42`; `event_grammar.rs:4,17` |
| `ValueRef` is generic over `G: EventGrammar = AnyGrammar` — the neutrality vehicle is structural, not a CSS patch | `tape/mod.rs:175,225`; `tape/mod.rs:11` (`pub use AnyGrammar, EventGrammar`) |
| Core-tree symbols are GREP-CLEAN-ABSENT in skinny: `StructLayout`=0, `OpenFrame`=0, `CssArena`=0, `TapeStructBuilder`=0, `begin_compound`=0, `TapeCursor`=0 | `grep -rn … skinny/crates/` per-symbol counts, all 0 (verified this cycle) |
| JSON rides the tape lazily: `scan_structurals`→`value_from_ref` (zero parallel eager arena) | `json/scan.rs:22,29,32`; `json/value.rs:143` |
| The JSON `StructuralIndex` (`Vec<u32>` positions) is a TRANSIENT producer feeding `value_from_ref`, not a retained parallel substrate | `json/scan.rs:25,33-35,144` |
| Skinny JSON Track 2 asserts the SAME substrate (substrate-ceiling probe, Lock 1 v+1 by design): `track2.offset_stream() == track1.offset_stream()` | `track2/json.rs:368` |
| Skinny CSS Track 2 is an INDEPENDENT cssparser oracle (`oracle_facts`, `OracleParser`), structurally distinct from Track 1 (`track1::parser::parse`) | `nonjson_css_l4.rs:597,624` + `cssparser` import :12 |
| Benched CSS Track 1 is today a fact-stream String: `track1_facts -> Result<String,String>` → `track1::parser::parse`; CSS `generated.rs` is `emit_fact_stream`/`push_str` (no SIMD) | `nonjson_css_l4.rs:596,597`; `css_l4_declaration_values/generated.rs:5,7,9` |
| CSS parse path imports ZERO tape today (UNWIRED proof) | `grep 'use crate::runtime::tape' css_l4_declaration_values/` = empty (verified) |
| The `tape_activated` gate's falsifiability is REAL: `PayloadArena` carries a `writes` counter under `bench-counters`/`test` | `tape/mod.rs:40,41,72` |
| CSS routing const `W5C_REQUEST_FACT_PROFILES` exists (the Lock-14 fingerprint to retire) | `codegen/src/lib.rs:336,567,611` |
| `RuntimeEmitterKind = {CompiledLowering, RequestFacts}`; CSS rides `RequestFacts` | `grammar_provider.rs:40-42,110` |
| SIMD neutrality vehicle exists: `select_classifier(alphabet)` :42, `lo6_table_admissible` :101, `PrimitiveKernels` OnceLock :50-59 | `bbnf-simd/src/dispatch.rs:42,50,58,101` |
| `digit_mac` udot orphan carries its scalar twin (C4a scalar-ref present): `parse_4_digits` :5, `parse_4_digits_dotprod` :27, `udot` :40 | `aarch64/digit_mac.rs:5,12,27,40` |
| i8mm is GREP-CLEAN-ABSENT from skinny source (C4b is genuinely net-new, correctly gated) | `grep -rn i8mm skinny/crates/` minus target-feature = empty (verified) |
| `sheets_witness` is a 25-LINE STUB (24 + 1 LOC) — generality witness honestly downgraded to JSON+CSS exercised | `sheets_witness/event_grammar_witness.rs` 24 + `mod.rs` 1 = 25 |

**The central CH5 disposition this cycle.** The V1 CH5 lens found ONE root coupling
defect — the totality `crates/core/` paths (`StructLayout`/`OpenFrame`/`CssArena`/
`TapeStructBuilder`) cited as the benched surface — across six REVISE items
(alphaA tree-citation; alphaD V6/O1/O2/O5; SYNTHESIS §0.1 tape gate + §0.1 layout
gate + §0.3 receivers + §Section 2 `tape_activated`; HANDOFF current-state +
four-lever). That defect, if it had reached S-P3, would have admitted a "tape
activated" gate passing in the WRONG tree while the benched CSS path still rode the
fact-stream String — the exact renamed/parallel-substrate dishonesty CH5 exists to
catch. **The V2 cohort has fully absorbed the V1 correction.** I re-verified each
formerly-flagged section against the live tree and against `alphaE:37-95` (the
load-bearing translation note, which itself is now V2-stamped). Every one now binds
to the skinny surface, names the core-tree symbols only as the SK-V18 fold target,
and adds the explicit second-substrate REJECT clause. No NEW hidden-coupling hazard
was introduced in the V2 fold.

---

## §1 — Dispositions

### alphaE (candidate shortlist) — the load-bearing CH5 artefact

**alphaE §0 ground-truth anchors + translation correction (:37-95):** ACCEPT.
The translation correction is now the contract's spine: it states verbatim that
`StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`/`begin_compound` are
grep-clean-absent in skinny (re-verified: all 0), that the benched CSS Track 1 is
`track1_facts -> Result<String,String>` → `emit_fact_stream`, that the skinny
layout-equivalent is `BackendRule` + `lower/tape_plan.rs`, and — the CH5
load-bearing sentence — "**no new cursor/builder type is introduced; the existing
`Tape`/`ValueRef`/`TapeBuilder` is reused**" (alphaE:90-91). This is one substrate,
correctly framed. No defect.

**alphaE C0/C1 (de-fact-stream + tape wiring, :107-225):** ACCEPT. C1 states "No
new cursor/builder type is introduced — the existing `Tape`/`ValueRef`/`TapeBuilder`
is the single substrate (Lock 1: no second tape, no type-ambivalence)"
(alphaE:167-169). The lazy CSS view is bound to be "isomorphic to JSON's
`value_from_ref`" (alphaE:166) over the existing `ValueRef` — verified that
`value_from_ref` is the lazy zero-alloc shape (`json/value.rs:143`). No parallel
eager arena retained. The C1 REDRESS pre-blocks explicitly forbid "no parallel
substrate / no second tape … no columnar SoA resurrection … no per-leaf eager
`Box::new`" (alphaE:214-218). CH5-honest.

**alphaE C2 (NEON structural pre-scan, :227-291):** ACCEPT. The NEON path "produces
ONLY the index (like JSON)" (alphaE:264-265) — a `Vec<u32>` structural index the C1
tape consumes, identical to JSON `scan_structurals`. Verified that JSON's
`StructuralIndex` is a transient producer (`json/scan.rs:25,144`), not a retained
parallel substrate. The C2 pre-block bars "cross-call classifier-state retention
(Lock 1, LOCKS.md:585; carry stays within a single chunk-call)" (alphaE:286-287) —
the precise anti-sidecar guard. No defect.

**alphaE C3/C4a/C4b (:293-421):** ACCEPT. C4a wires the udot orphan with its
verified scalar twin (digit_mac.rs:5/27); C4b is correctly gated as net-new (i8mm
re-verified grep-clean-absent) and "lands ONLY if a Wave-5 re-profile … proves the
digit/number leaf is in the top-N tailwind self-time. If it is not, C4b does NOT
land — no orphan kernel" (alphaE:379-382). No orphan-kernel coupling. No defect.

**alphaE §3 grammar-neutral discipline (:453-487):** ACCEPT. Generality is bound as
"witnessed, not asserted": JSON is the exercised witness, `sheets_witness` is a
25-line stub (re-verified 25 LOC), and C1's generality is an EXIT gate, not a
by-construction claim. The C2 neutrality vehicle is `select_classifier(alphabet)`
(re-verified dispatch.rs:42) — one kernel, per-grammar alphabet. This forecloses the
hidden-coupling escape of asserting fleet-wide neutrality off a single grammar. No
defect.

### alphaA (results extraction)

**alphaA §0 benched-surface disambiguation (:46-60):** ACCEPT (was REVISE in V1).
Now imports αE §0:37-51 verbatim, states "In skinny there is no `StructLayout`, no
`OpenFrame`, no `CssArena`, no `TapeStructBuilder` (all grep-clean across
`skinny/crates/`)" — re-verified all 0 — and binds every throughput claim to the
skinny benched path. The architecture-doc core-tree symbols are correctly labelled
"the TOTALITY fold target, not SK-V17 owner paths" (alphaA:56-57). The V1 REVISE
root cause is resolved.

**alphaA table row "CSS typed Track 1 (eager OpenFrame CSSOM, core-tree retime
model)" (:70) + core-tree retime citations (:154,176,195,204,250,288,295,372):**
ACCEPT. These now carry the benched-surface caveat and are explicitly tagged
"core-tree" / "TOTALITY symbol, not benched" (e.g. :203-204 "CORE-TREE — these are
the totality builder, NOT skinny benched surface"; :295-296 "`TapeStructBuilder`/
`TapeCursor` … are TOTALITY symbols"; :378 "`grep TapeStructBuilder skinny/` =
EMPTY"). The numbers are cited as evidence of the *model's* cost, not mislocated as
the benched surface. No CH5 hazard remains: the hot-leaf attribution is bound to the
right tree and explicitly carries an S-P1 re-confirm-on-benched-path obligation.

**alphaA measurement rows (W6 table, baseline retimes):** ACCEPT. Measured + cited;
no substrate-coupling claim embedded.

### alphaB (competitor deltas)

**alphaB (entire artefact):** ACCEPT. Re-greped clean for `sidecar / second tape /
Track 1 == Track 2 / parallel substrate / StructLayout`. Confines itself to
comparator deltas with plane disclosure (lightningcss full-CSSOM = the fair bar;
cssparser token-scan = flaw probe). The plane disclosure is the correct CH5-honest
comparator framing — it forecloses the wrong-plane (token-scan) comparator
admission, itself a hidden-coupling escape enumerated in SYNTHESIS §0.4. No defect.

### alphaC (REDRESS digest)

**alphaC §0 tree-disambiguation (:18-31):** ACCEPT. Now leads with the αE §0:37-51
correction, grep-verified, and adds the Lock 2 note that even the *name*
`StructLayout` is RETIRED (canonical `Layout`/`LayoutFacts`, LOCKS.md:160). The
"layout description" admission surface is re-keyed to the skinny `BackendRule` +
`LayoutFacts.backend_shape` + `lower/{tape_plan,offset_tape,event_tape}.rs`
(:29,33-34). The eager-arena/Box pathology is correctly localised to "only at
`crates/core/.../css_l4/arena.rs` (TOTALITY)"; skinny's eager pathology is the
fact-stream String (:30). The V1 caveat is fully absorbed.

**alphaC §1 AZ-IV eager-value-tree (ADMIT-UNDER-FRAMING):** ACCEPT. States the
re-open test exactly (typed value built per-leaf at parse time / per-leaf heap
alloc), binds it to the JSON `value_from_ref` zero-alloc proof and the
`PayloadArena` write/alloc counter gate — re-verified the `writes` counter is real
(`tape/mod.rs:41,72`). Structurally grounded falsifiability.

**alphaC §2 StructRegistry/Arena<G>/Builder<G> split (PERMANENT PRE-BLOCK):**
ACCEPT. Correctly classifies per-leaf registry dereference as a Lock 1
parallel-substrate violation ("the `Vec<OpenFrame>::clone` 86.07% samply pathology
is the canonical example", :117) and the layout itself as ADMIT-UNDER-FRAMING,
re-keyed to the skinny tree. Load-bearing hidden-coupling claim (no per-leaf
indirection; one substrate) is correct and now tree-correct.

**alphaC §3 fact-stream String + §4 24-row broadcast + §5/§6/§8:** ACCEPT. §3
explicitly names the Track1==Track2/sidecar failure mode and binds the output-plane
gate (never `digest`/`FactStream`). §4 pre-blocks the broadcast (one measurement →
N conceptual rows) with a distinct-row-id gate. §8 is the CH5 north star ("the flat
lazy-offset tape … is the ONLY admissible carrier"). All correct.

### alphaD (validated/invalidated ledger)

**alphaD §0 translation-correction adoption (:22-34):** ACCEPT (was REVISE in V1).
Now adopts αE:37-51 verbatim, grep-verifies the six core-tree symbols return zero,
and maps each to its skinny equivalent in a table (:32-34): flat tape →
`skinny/crates/runtime/src/tape/`; `StructLayout`/`OpenFrame` projection →
`BackendRule` + `lower/{tape_plan,offset_tape,event_tape,eager_tape,…}`;
`TapeStructBuilder` consumer trait → `EventGrammar` + `DocumentView` (re-verified
both exist at `event_grammar.rs:4` and `mod.rs:227`). The V1 root cause is resolved.

**alphaD V6 row (:76):** ACCEPT. The substrate is bound to the verified skinny
module names "NOT the doc's core-tree `record/arena/cursor` siblings," with the
no-StructRegistry guard asserted on the measured tree (grep over `skinny/crates/`
returns zero — re-verified) and the UNWIRED caveat ("zero usage of `Tape`/
`TapeBuilder`/`ValueRef` in any benched CSS parse path" — re-verified empty grep).
Honest.

**alphaD O1 (tape wiring, :144):** ACCEPT. Owner paths are all skinny
(`regen_css.rs`, `css_l4_*`, `runtime/src/tape/`, `lower/{offset_tape,tape_plan}`),
explicitly "consume `TapeBuilder`/`ValueRef`, NOT core `TapeStructBuilder`/
`TapeCursor`." Generality is "JSON-WITNESSED only" with the anti-relabel pruning
gate. The V1 hazard (wiring core-tree into a path the bench never calls) is closed.

**alphaD O2 (lazy view + cursor API, :145):** ACCEPT. The load-bearing CH5 sentence
is present verbatim: "over the EXISTING `Tape`/`ValueRef` — **no new cursor/builder
type is introduced** (a second cursor type would be a Lock-1 type-ambivalence
violation)" (:145). This is the precise anti-second-substrate guard. preserve-rich-
ast bound to on-demand reconstruction, not flattening. No defect.

**alphaD O5 (codegen unification, :148):** ACCEPT. Retire-list names
`W5C_REQUEST_FACT_PROFILES` (re-verified lib.rs:336) and the 148-fn
`generated_real_typed.rs` overfit; the 594-line `css_l4.toml` is correctly localised
as a TOTALITY-tree artefact / fold target, not a skinny owner path. The anti-relocate
overfit gate is bound. No CH5 defect.

**alphaD I1/I2 invalidated rows (:94-95):** ACCEPT. Correctly attribute the cost to
"typed-value materialization + arena/builder indirection (core) / fact-stream String
serialization (skinny)" — both trees named, neither conflated.

### SYNTHESIS.md (= α-F contract)

**Benched-surface note (:21-58):** ACCEPT (NEW in V2; resolves the V1 §0.1/§0.3
REVISE root). The note is load-bearing and binds every surface citation: core-tree
symbols are "grep-clean-absent from `skinny/crates/` (verified)" and "Any
close-condition gate keyed on them could be 'met' in `crates/core/` while the benched
CSS path is untouched — that is wrong-tree dishonesty and is REJECTed" (:29-30). The
benched substrate is named (`skinny/crates/runtime/src/tape/`, with Tape/ValueRef/
PayloadArena/DocumentView line numbers re-verified). Core-tree symbols are "the
design-intent fold target … SK-V18 work, not SK-V17 owner paths" (:55-58). Exactly
the V1 fix, folded into the contract.

**§0.1 "Tape activation (not dead code)" gate (:100):** ACCEPT (was REVISE in V1).
Now bound to the skinny tree: "`Tape`/`ValueRef`/`TapeBuilder` from
`skinny/crates/runtime/src/tape/` appear in the CSS parse path that
`track1::parser::parse` (reached via `nonjson_css_l4.rs:596`) invokes; a grep over
those files returns non-zero; `PayloadArena` write/alloc counters (per alphaC §1)
confirm the parse emits into the tape rather than into a fact-stream String." The
gate ends with "No new cursor/builder type is introduced — the EXISTING skinny
`Tape`/`ValueRef`/`TapeBuilder` is the only substrate (Lock 1, no second tape)." The
falsifiability is now tied to the benched path + the verified `writes` counter, not a
tree-agnostic grep. The exact V1 hazard (grep going non-zero in `crates/core/` while
the benched path is untouched) is foreclosed.

**§0.1 "Layout-driven projection" gate (:101):** ACCEPT (was REVISE in V1). The
generator emits accessors "by walking the SAME `BackendRule` shape the parser emits,
lowered via `skinny/crates/codegen/src/lower/{tape_plan,offset_tape,event_tape}.rs`
(the skinny equivalent of 'StructLayout-driven projection'; there is no
`StructLayout`/`OpenFrame` in skinny)" and the accessors are "`ValueRef`-cursor reads
isomorphic to JSON's `value_from_ref`" over the existing `Tape`/`ValueRef`. Both the
wrong-tree-activation and the new-type-parallel-substrate dangers V1 flagged are
explicitly closed (the latter by §0.4's second-substrate REJECT clause, below).

**§0.3 receiver rows (:153-154):** ACCEPT (was REVISE in V1). "Lazy-view projection
generator" states "NO new cursor/builder type is introduced — the existing skinny
`Tape`/`ValueRef` is reused" and maps the doc's `StructLayout`/`begin_compound`
design intent to the skinny `BackendRule` + `tape_plan.rs`. "Tape activation +
builder seam flip" retires `emit_fact_stream` → skinny `TapeBuilder` append (NOT an
`OpenFrame`→`TapeStructBuilder` flip), DELETEs `W5C_REQUEST_FACT_PROFILES`, and warns
"without re-introducing a second substrate" (:154). The §0.3 preamble (:145-149)
explicitly marks `crates/core/src/backend/rust/emitter/`, `regen_css.rs
emit_builder`, the `OpenFrame` template, `css_l4/builder.rs:274` as "the SK-V18 fold
target, NOT SK-V17 owner paths; a receiver editing them would burn LOC on an
un-benched tree." The V1 REVISE is fully resolved.

**§0.4 hidden-coupling pre-block + second-substrate clause (:201-213):** ACCEPT —
and it is the strongest CH5 paragraph in the cohort. It enumerates the full escape
set (retained sidecars, sidecar tables, sidecar event vectors, retained cursor/list,
cursor streams, aux density/projection tables, parser-owned structural projections/
streams, parallel source passes, second tapes, public `UnionTape`, new substrate
APIs, sixth `BackendShape`, production FNV arbiters, Track 1 == Track 2 sidecars,
wrong-plane comparator admission), states "A SIMD mask stream is a transient
producer, not a retained sidecar; if structural offsets are retained, the structural
projection IS the tape (Lock 1)" — which correctly resolves the C2 `Vec<u32>` index
as the tape, not a sidecar — and adds the V2-NEW clause: "**No second substrate**: if
the implementor introduces a skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`,
those would become a SECOND substrate alongside the landed `Tape`/`ValueRef` (Lock 1
type-ambivalence) and are REJECTed; the projection generator emits accessors over the
EXISTING `Tape`/`ValueRef`" (:210-213). This clause is exactly the V1 CH5 §0.3(b)
recommendation, folded verbatim. Verbatim-correct Lock 1 discipline.

**§0.4 generality clause (:215-224):** ACCEPT. Generality is "exercised, not
asserted"; JSON is the existing witness, the Sheets rider is "a 25-line stub" (re-
verified 25 LOC) and BBNF-self is absent; the generator must either emit for
`sheets_witness` or the contract states CSS+JSON-only. Forecloses the by-construction
neutrality escape. No defect.

**§0.6 comparator table — "Track 2 / oracle … structurally distinct from Track 1
(Lock 1, CH5)" (:280):** ACCEPT. Re-verified at the benched surface: the CSS oracle
is the independent cssparser `OracleParser`/`oracle_facts` (nonjson_css_l4.rs:624),
not a re-projection of Track 1 (`track1::parser::parse`, :597). The JSON Track 2
equality (`offset_stream() ==`, track2/json.rs:368) is the substrate-ceiling probe
(same substrate, Lock 1 v+1 by design); the CSS Track 2 is the independent oracle.
Both correct; neither is a Track1==Track2 dishonesty.

**§Section 2 telemetry — `tape_activated` (:330):** ACCEPT (was REVISE in V1). Now
defined as "benched `track1::parser::parse` emits into skinny `Tape`, read via
`ValueRef`; proven by `PayloadArena` write/alloc counters; NOT satisfiable by a grep
in `crates/core/`." Tied to the benched path's counter telemetry (re-verified the
`writes` counter exists), not a tree-agnostic grep. The exact V1 hazard is closed.
`css_typed_summary_equal` gate-before-speed (:327) preserved — correct ordering.

### HANDOFF.md (= α-F packet)

**Benched-substrate disclosure (:7-18):** ACCEPT (NEW in V2; resolves the V1
current-state + four-lever REVISE). Core-tree symbols "grep-clean-absent from
`skinny/crates/` (verified) and are the SK-V18 fold target, NOT SK-V17 owner paths";
the benched substrate and the benched CSS Track 1 fact-stream String path are named
with line numbers. The four-lever (:62-79) is bound to the skinny tree throughout.

**HANDOFF current-state "there is no eager `OpenFrame` tree in skinny" (:34):**
ACCEPT. The exact V1 correction — the W6 report's `OpenFrame` lineage is the totality
tree; skinny's eager pathology is the fact-stream String. Re-verified.

**HANDOFF "Pre-Blocked Routes" + "No second substrate" (:147-161):** ACCEPT.
Mirrors SYNTHESIS §0.4: "an introduced skinny `StructLayout`/`TapeStructBuilder`/
`TapeCursor` alongside the landed `Tape`/`ValueRef` is a Lock 1 type-ambivalence
violation (REJECT)" (:147-150), and the hidden-coupling escape list (:155-161)
includes Track 1 == Track 2 sidecars + cross-call classifier-state retention. The
V2-NEW second-substrate clause is present.

**HANDOFF "Next Move" `tape_activated` gate + same-wave consumer (:187,193-196):**
ACCEPT. `tape_activated` "satisfied ONLY when the benched `track1::parser::parse`
emits into the skinny runtime `Tape`, read via `ValueRef`, proven by `PayloadArena`
write/alloc counters — NOT by a grep returning non-zero in `crates/core/` (wrong-tree
dishonesty is REJECTed)" (:193-196). "Each primitive lands WITH its hot-path consumer
in the same commit (no orphan kernels)" (:187). The NEON-gated-behind-tape ordering
(no structural index to pre-scan into until the tape decodes CSS) prevents an orphan
NEON scanner sidecar. No CH5 defect.

---

## §2 — Adversarial probes that did NOT find a defect (recorded for the record)

These are the hidden-coupling escapes CH5 actively hunted and confirmed closed:

1. **Does the C2 NEON `Vec<u32>` structural index become a second retained
   substrate?** NO. SYNTHESIS §0.4 (:208) states "if structural offsets are retained,
   the structural projection IS the tape." Re-verified the JSON precedent
   (`json/scan.rs:25,144`): `StructuralIndex` is a transient producer feeding
   `value_from_ref`, not a retained parallel store. The CSS index follows the same
   shape. Closed.

2. **Does the lazy cursor view retain a parallel eager arena?** NO. The view is bound
   to the JSON `value_from_ref` shape (`json/value.rs:143`) over the existing
   `ValueRef`; alphaC §1 binds the `PayloadArena` write/alloc counter gate to prove
   zero per-leaf alloc except irreducible decode. preserve-rich-ast = on-demand
   reconstruction, not eager materialization. Closed.

3. **Could a renamed scanner satisfy `tape_activated` without moving the benched
   path?** NO. The gate is bound to `track1::parser::parse` (the benched fn) + the
   `PayloadArena` `writes` counter (re-verified real), not a tree-agnostic grep that
   could go green in `crates/core/`. Closed.

4. **Is the JSON Track2==Track1 equality a Track1==Track2 dishonesty?** NO. It is the
   declared substrate-ceiling probe (`offset_stream() ==`, track2/json.rs:368) —
   same substrate by Lock 1 v+1 design. The CSS Track 2 is an independent oracle
   (cssparser). Both honest. Closed.

5. **Does any candidate introduce a new skinny cursor/builder type (second
   substrate)?** NO. C1 (alphaE:167), O2 (alphaD:145), SYNTHESIS §0.1/§0.3/§0.4, and
   HANDOFF :147-150 all state explicitly that no new cursor/builder type is
   introduced and that introducing one is REJECTed. Closed.

---

## §3 — Disposition tally

| Disposition | Count |
|---|---|
| ACCEPT | 29 |
| REVISE | 0 |
| REJECT | 0 |

ACCEPT rate = 29/29 = **100%**. Zero orphan REVISE; zero REJECT.

The single V1 root coupling defect (totality-tree paths cited as the benched skinny
surface, surfacing as 6 REVISE) is fully resolved: the V2 cohort folded
`alphaE:37-95` into a load-bearing benched-surface note in BOTH SYNTHESIS and
HANDOFF, re-bound every formerly-flagged gate to the skinny tree, tied
`tape_activated` to the benched `track1::parser::parse` path + the verified
`PayloadArena` write counter, and added the explicit second-substrate REJECT clause
(SYNTHESIS §0.4:210-213, HANDOFF :147-150) that V1 CH5 §0.3(b) recommended. No new
hidden-coupling hazard was introduced in the fold. The hidden-coupling discipline is
present, tree-correct, and structurally verifiable by grepping `skinny/crates/`.

CH5 converges at V2: ≥95% ACCEPT, zero orphan REVISE, V=2 ≤ 5.
