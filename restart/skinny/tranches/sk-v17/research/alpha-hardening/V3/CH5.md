# CH5 — HIDDEN-COUPLING (cycle V3)

Lens: CH5 Hidden Coupling per PASS-ALPHA §3 + ORCHESTRATOR §3W.
Subject: Pass Alpha SK-V17 artefacts — `research/alpha/{alphaA..alphaE}.md` +
`SYNTHESIS.md` + `HANDOFF.md` (α-F output = SYNTHESIS.md + HANDOFF.md per
PASS-ALPHA §2; there is no separate `alphaF` file — confirmed by `find` returning
empty for `*alphaF*`/`*contract*`).
Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25` (`git rev-parse HEAD`
re-confirmed this cycle = `1c5bd7a25250640f3a6fcfc00abed11f556f674f`).
Focus: no parallel substrate / sidecar / renamed-scanner / Track1==Track2
dishonesty; tape+projection is ONE substrate (Lock 1); cursor-API lazy view does
not retain a parallel eager arena.

---

## §0 — Ground truth (independently re-greped at `1c5bd7a25`, this V3 cycle)

Every disposition below cites a fact re-verified against the benched **skinny** tree
THIS cycle — not inherited from V1/V2. The artefacts were edited at 14:20–14:25
(after V2 closed at 14:19), so V3 re-verifies that the V2-folded corrections survived
the edit and that no new coupling hazard entered.

| Fact | Evidence (re-verified V3) |
|---|---|
| Skinny tape IS one substrate: `Tape` :94, `ValueRef` :175, `PayloadArena` :38, `DocumentView` trait :227, `TapeBuilder` (assembler) :42, `EventGrammar` trait :4 | `skinny/crates/runtime/src/tape/mod.rs:38,94,175,227`; `assembler.rs:42`; `event_grammar.rs:4` |
| `ValueRef` is generic over `G: EventGrammar = AnyGrammar` — neutrality vehicle is structural, not a CSS patch | `tape/mod.rs:175` (`ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>`) |
| Core-tree symbols GREP-CLEAN-ABSENT in skinny: `StructLayout`=0, `OpenFrame`=0, `CssArena`=0, `TapeStructBuilder`=0, `TapeCursor`=0, `begin_compound`=0 | per-symbol `grep -rn … skinny/crates/`, all 0 (re-verified V3) |
| JSON rides the tape lazily: `scan_structurals`→`value_from_ref` (zero parallel eager arena) | `json/scan.rs:22`; `json/value.rs:143` |
| The JSON `StructuralIndex` is a TRANSIENT producer (`from_positions`) feeding `value_from_ref`, not a retained parallel substrate | `json/scan.rs:22,25,32,35` (`StructuralIndex::from_positions`) |
| Skinny JSON Track 2 asserts the SAME substrate (substrate-ceiling probe, Lock 1 v+1 by design): `track2.offset_stream() == track1.offset_stream()` | `track2/json.rs:368` |
| Skinny CSS Track 2 is an INDEPENDENT cssparser oracle (`oracle_facts`, `OracleParser`), structurally distinct from Track 1 (`track1::parser::parse`) | `nonjson_css_l4.rs:596,597,624,627`; `cssparser` import `:12` |
| Benched CSS Track 1 is today a fact-stream String: `track1_facts -> Result<String,String>` → `track1::parser::parse`; CSS `generated.rs` is `emit_fact_stream`/`push_str` (no SIMD) | `nonjson_css_l4.rs:596`; `css_l4_declaration_values/generated.rs:5,6,7,9` |
| CSS parse path imports ZERO tape today (UNWIRED proof) | `grep 'tape' css_l4_declaration_values/` = 0 (re-verified V3) |
| The `tape_activated` gate's falsifiability is REAL: `PayloadArena` carries a `writes` counter under `cfg(any(test, feature="bench-counters"))` | `tape/mod.rs:40,41,72` |
| Retire targets exist: `W5C_REQUEST_FACT_PROFILES` (lib.rs:336, iterated :567,:611, selected :299); seven `RuntimeEmitterKind::RequestFacts` literals (regen_css.rs:45,63,81,99,117,135,153); `regen_css` fn :164 | re-verified V3 |
| SIMD neutrality vehicle exists: `select_classifier(alphabet)` :42, `lo6_table_admissible` :101, `PrimitiveKernels` OnceLock :50,58 | `bbnf-simd/src/dispatch.rs:42,50,58,101` |
| `sheets_witness` is a 25-LOC STUB (24 + 1) with NO `.bbnf`/parser/`BackendRule` | `sheets_witness/event_grammar_witness.rs` 24 + `mod.rs` 1 = 25 (re-verified V3) |
| The fail-closed-control claim is structurally grounded: codegen treats `google_sheets`/`bbnf` as fail-closed negative controls | `codegen/src/lib.rs:1075` `w5a_sheets_bbnf_fail_closed_through_runtime_contract` (re-verified V3) |

**The central CH5 disposition this cycle.** V1 found ONE root coupling defect (totality
`crates/core/` paths cited as the benched surface, surfacing as 6 REVISE). V2 confirmed
that defect fully absorbed and converged at 100%. V3's job is independent: re-verify the
LIVE tree, then confirm the V2 corrections survived the 14:20–14:25 artefact edits and
that the edits introduced no fresh sidecar / second-substrate / renamed-scanner /
Track1==Track2 hazard. They did NOT. Every coupling-bearing line from V2 persists and
several are SHARPENED (the `sheets_witness` fail-closed control is now cited to a real
test fn `lib.rs:1075`; the `projection_generality_exercise` telemetry value-set is
restricted to `json`/`css_l4` with `sheets_witness` named NOT-valid; the C2 `Vec<u32>`
index is explicitly tied to the JSON transient-producer precedent). An adversarial grep
of the full cohort for new coupling language (`retain.*classifier`, `cross-call`,
`dual tape`, `eager arena`, `per-leaf Box`, `columnar`, `SoA`) returns ONLY pre-block /
REJECT-clause occurrences — no live admission. No NEW hidden-coupling hazard exists.

---

## §1 — Dispositions

### alphaA (results extraction)

**alphaA §0 benched-surface disambiguation + core-tree retime citations:** ACCEPT.
The benched-surface header imports the αE translation correction; core-tree symbols
(`StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`) are labelled the TOTALITY
fold target, not SK-V17 owner paths; every throughput row is bound to the skinny
benched path. Re-verified all six core symbols return 0 in `skinny/crates/`. The W6
retime numbers are cited as evidence of the *model's* cost with an S-P1
re-confirm-on-benched-path obligation, not mislocated as the benched surface. No
hidden-coupling hazard; the hot-leaf attribution is tree-correct.

**alphaA measurement rows (W6 table, baseline retimes):** ACCEPT. Measured + cited;
no substrate-coupling claim embedded.

### alphaB (competitor deltas)

**alphaB (entire artefact):** ACCEPT. Re-greped clean for `sidecar / second tape /
Track 1 == Track 2 / parallel substrate / StructLayout`. Confines itself to comparator
deltas with plane disclosure (lightningcss full-CSSOM = fair bar; cssparser token-scan
= flaw probe). The plane disclosure is the correct CH5-honest comparator framing — it
forecloses the wrong-plane (token-scan) comparator admission, itself a hidden-coupling
escape enumerated in SYNTHESIS §0.4. No defect.

### alphaC (REDRESS digest)

**alphaC §0 tree-disambiguation (:8,18):** ACCEPT. Leads with the αE §0:37-51
correction, grep-verified at HEAD `1c5bd7a25`. The admission surface is re-keyed to the
skinny `BackendRule` + tape-plan lowering, with the eager-arena/Box pathology localised
to the totality `css_l4/arena.rs`; skinny's eager pathology is the fact-stream String.

**alphaC §1 AZ-IV eager-value-tree (ADMIT-UNDER-FRAMING):** ACCEPT. States the re-open
test exactly (typed value built per-leaf / per-leaf heap alloc), binds it to the JSON
`value_from_ref` zero-alloc proof and the `PayloadArena` write/alloc counter gate —
re-verified the `writes` counter is real (`tape/mod.rs:41,72`). Structurally grounded
falsifiability, not aspirational.

**alphaC §2 StructRegistry/Arena<G>/Builder<G> split + table row 2a (:347):** ACCEPT.
Classifies per-leaf registry dereference as a PERMANENT PRE-BLOCK Lock 1
parallel-substrate violation (the `Vec<OpenFrame>::clone` 86.07% samply pathology as
canonical example) and the layout itself as ADMIT-UNDER-FRAMING re-keyed to the skinny
tree. The grep-surface in row 2a ("any per-leaf registry lookup; per-compound
Vec/split_off/Box; frame-stack clone checkpoint; new hand-coded per-grammar profile
table re-introduced into `skinny/crates/runtime/`") is the precise anti-second-substrate
detection. Load-bearing claim (no per-leaf indirection; one substrate) is correct and
tree-correct.

**alphaC §3 fact-stream String (:185,194,209):** ACCEPT. Explicitly names the
Track1==Track2/sidecar failure mode ("would also be a Track1==Track2 dishonesty /
sidecar violation (Lock 1, CH5) if the String is the retained product", :194) and binds
the output-plane gate ("never `digest`/`FactStream` for any admitted CSS row; the gate
rejects a CSS admit whose output plane is a [fact stream]", :209). Exactly the CH5
discipline. Lock 1's `FactStream`-only-as-output-plane clause (LOCKS.md:585) is cited
correctly (:185).

**alphaC §4 24-row broadcast (:215-241):** ACCEPT. PERMANENT PRE-BLOCK with the
broadcast-detection gate (distinct `measurement_row_id`; `grep -c
'css_l4/.*/direct_to_struct/main' = 24` broadcast rows + 1 distinct W6 typed row at
:154, re-verified the shape of the claim). A broadcast is the canonical
one-measurement-coupled-to-N-rows hidden-coupling escape; correctly closed and codified
into NEW-CH5-V5-02 + Lock 8.

**alphaC §5/§6/§8:** ACCEPT. FNV/fixture runtime-migration, x86 same-wave-consumer, and
the §8 north star ("the flat lazy-offset tape … is the ONLY admissible carrier") are all
coupling-honest with gate consumers. No defect.

### alphaD (validated/invalidated ledger)

**alphaD §0 translation-correction adoption:** ACCEPT. Adopts αE:37-51 verbatim,
grep-verifies the six core-tree symbols return zero, maps each to its skinny equivalent.

**alphaD V6 row (:82):** ACCEPT. Substrate bound to the verified skinny module names
"NOT the doc's core-tree `record/arena/cursor` siblings," no-StructRegistry guard
asserted on the measured tree (re-verified grep over `skinny/crates/` returns zero), and
the UNWIRED caveat is explicit ("zero usage of `Tape`/`TapeBuilder`/`ValueRef` in any
benched CSS parse path; the benched CSS Track 1 still rides `track1_facts ->
Result<String,String>` … `emit_fact_stream` … zero SIMD" — re-verified empty tape grep
in the CSS crate). Honest.

**alphaD O2 (lazy view + cursor API, :151):** ACCEPT. The load-bearing CH5 sentence is
present verbatim: "isomorphic to JSON's `value_from_ref` … over the EXISTING
`Tape`/`ValueRef` — **no new cursor/builder type is introduced** (a second cursor type
would be a Lock-1 type-ambivalence violation)" (:151). preserve-rich-ast bound to
on-demand reconstruction, not flattening. The generality clause names `sheets_witness`
as structurally non-dischargeable (24-LOC, no `BackendRule`) with the fail-closed control
cited to `codegen/src/lib.rs:1075-1090` — re-verified the test fn exists. The precise
anti-second-substrate guard. No defect.

**alphaD I1/I2 + Track1==Track2 pre-block (:115):** ACCEPT. The cost is attributed to
"typed-value materialization + arena/builder indirection (core) / fact-stream String
serialization (skinny)" — both trees named; "sidecars / second tapes / public
`UnionTape` / Track1==Track2" is in a pre-block context, not an assertion. No conflation.

**alphaD O1/O5 (tape wiring + codegen unification):** ACCEPT. Owner paths all skinny,
consuming `TapeBuilder`/`ValueRef` "NOT core `TapeStructBuilder`/`TapeCursor`"; retire
targets (`W5C_REQUEST_FACT_PROFILES`, re-verified lib.rs:336) named; the 594-line
`css_l4.toml` correctly localised as a TOTALITY fold target, not a skinny owner path.
No CH5 defect.

### alphaE (candidate shortlist) — the load-bearing CH5 artefact

**alphaE §0 translation correction + C0/C1 (:73,112-113,189,192):** ACCEPT. The CH5
load-bearing sentence is present verbatim: "isomorphic to JSON `value_from_ref`
(json/value.rs:143) — **no new cursor/builder type is introduced; the existing
`Tape`/`ValueRef`/`TapeBuilder` … is the single substrate** (Lock 1: no second tape, no
type-ambivalence)" (:112-113,192). Re-verified `value_from_ref` is the lazy zero-alloc
shape (`json/value.rs:143`). One substrate, correctly framed.

**alphaE C2 (NEON structural pre-scan, :324-325):** ACCEPT. The NEON path produces ONLY
the `Vec<u32>` index the C1 tape consumes — re-verified the JSON precedent is a TRANSIENT
producer (`StructuralIndex::from_positions`, `json/scan.rs:22,25,32`), not a retained
parallel substrate. The C2 pre-block bars "No cross-call classifier-state retention
(Lock 1, LOCKS.md:585 — 'cross-call classifier state remains rejected'; carry [stays
within a single chunk-call])" (:324-325). The precise anti-sidecar guard. No defect.

**alphaE C3/C4a/C4b + REDRESS pre-blocks (:252-255):** ACCEPT. The pre-block list bars
"no parallel substrate / no second tape (Lock 1, LOCKS.md:75,585) … no columnar SoA
resurrection; no per-leaf eager `Box::new`" (:252-255) — the full anti-coupling set. C4a
wires the udot orphan with its scalar twin; C4b is correctly gated as net-new and lands
only if the re-profile proves the digit leaf is top-N (no orphan kernel). No coupling
defect.

### SYNTHESIS.md (= α-F contract)

**Benched-surface note (:30-62):** ACCEPT. Core-tree symbols are "grep-clean-absent from
`skinny/crates/`" (:31, re-verified all 0); the benched substrate is named with line
numbers (:36); core-tree symbols are "the **design-intent fold target** … SK-V18 work,
not SK-V17 owner paths" (:59-62). The exact V1 fix, folded into the contract and surviving
the V3 edit.

**§0.1 "Tape activation" gate (:104):** ACCEPT. Bound to the skinny tree: "`Tape`/
`ValueRef`/`TapeBuilder` … appear in the benched CSS parse path … a grep over those files
returns non-zero; `PayloadArena` write/alloc counters (per alphaC §1) confirm the parse
emits into the tape rather than into a fact-stream String … No new cursor/builder type is
introduced — the EXISTING skinny `Tape`/`ValueRef`/`TapeBuilder` is the only substrate
(Lock 1, no second tape)." The falsifiability is tied to the benched path + the verified
`writes` counter, not a tree-agnostic grep. The V1 wrong-tree hazard is foreclosed.

**§0.1 "Layout-driven projection" gate (:105) + NEON-gated-behind-tape (:111):** ACCEPT.
The generator emits "`ValueRef`-cursor reads isomorphic to JSON's `value_from_ref`
(`json/value.rs:143`) over the existing skinny `Tape`/`ValueRef`"; the routing is DERIVED
from `BackendRule`, "NOT lost and NOT re-hardcoded (Lock 14)"; the `css_l4.toml` LOC
asymmetry is explicitly INFORMATIONAL / SK-V18, "gating an SK-V17 close on a non-benched
totality file would be the wrong-tree dishonesty this contract REJECTs" — an excellent
self-policing CH5 clause. The NEON leaf "produces only a `Vec<u32>` structural index, and
the tape consumes it … NEON is gated behind tape activation — there is no structural index
to pre-scan into until the tape decodes CSS" (:111) — re-verified this matches the JSON
transient-producer shape exactly. No orphan-scanner sidecar. No defect.

**§0.3 receiver rows (:171-172):** ACCEPT. "Lazy-view projection generator" states "NO new
cursor/builder type is introduced — the existing skinny `Tape`/`ValueRef` is reused"
(:171); "Tape activation + builder seam flip" retires `emit_fact_stream` → skinny
`TapeBuilder` append (the concrete seam sites cited: the seven `RequestFactsProfile`
literals `regen_css.rs:45,63,81,99,117,135,153` — re-verified all seven carry
`RuntimeEmitterKind::RequestFacts`), DELETEs `W5C_REQUEST_FACT_PROFILES` (re-verified
lib.rs:336), and warns "the seam must accept the tape sink without re-introducing a second
substrate" (:172). The §0.3 preamble marks the totality `emit_builder`/`OpenFrame`
template/`css_l4/builder.rs:274` as "the SK-V18 fold target, NOT SK-V17 owner paths; a
receiver editing them would burn LOC on an un-benched tree" (:164-165). Both V1 dangers
(wrong-tree activation; new-type parallel substrate) explicitly closed.

**§0.4 hidden-coupling pre-block + second-substrate clause (:220-232):** ACCEPT — the
strongest CH5 paragraph in the cohort. Full escape set enumerated (retained sidecars,
sidecar tables/event vectors, retained cursor/list, cursor streams, aux density/projection
tables, parser-owned structural projections/streams, parallel source passes, second tapes,
public `UnionTape`, new substrate APIs, sixth `BackendShape`, FNV arbiters, Track 1 ==
Track 2 sidecars, wrong-plane comparator admission); "A SIMD mask stream is a transient
producer, not a retained sidecar; if structural offsets are retained, the structural
projection IS the tape (Lock 1, `LOCKS.md:75`)" (:226-228) — correctly resolves the C2
`Vec<u32>` as the tape, not a sidecar; "Cross-call classifier-state retention is REJECT
under Lock 1 v+1" (:228); and the second-substrate clause: "if the implementor introduces
a skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`, those would become a SECOND
substrate alongside the landed `Tape`/`ValueRef` (Lock 1 type-ambivalence) and are
REJECTed; the projection generator emits accessors over the EXISTING `Tape`/`ValueRef`"
(:229-232). Verbatim-correct Lock 1 discipline.

**§0.4 generality clause (:234-255):** ACCEPT. Generality is "exercised, not asserted";
exercised riders are JSON + CSS only; `sheets_witness` is "NOT a viable third exercise" —
24-line `EventGrammar` byte-classification impl with no `.bbnf`/parser/`BackendRule`, and
"codegen treats `sheets`/`bbnf` as fail-closed negative controls
(`codegen/src/lib.rs:1075-1090`)" — re-verified the test fn exists. Forecloses the
by-construction neutrality escape with a structurally-grounded (not asserted) reason. No
defect.

**§0.6 comparator table — "Track 2 / oracle … structurally distinct from Track 1 (Lock 1,
CH5)" (:313):** ACCEPT. Re-verified at the benched surface: the CSS oracle is the
independent cssparser `OracleParser`/`oracle_facts` (`nonjson_css_l4.rs:624`), not a
re-projection of Track 1 (`track1::parser::parse`, :596). The JSON Track 2 equality
(`offset_stream() ==`, `track2/json.rs:368`) is the declared substrate-ceiling probe (same
substrate, Lock 1 v+1 by design); the CSS Track 2 is the independent oracle. Both correct;
neither is a Track1==Track2 dishonesty.

**§Section 2 telemetry — `tape_activated` (:363) + `projection_generality_exercise`
(:365):** ACCEPT. `tape_activated` is "boolean (benched `track1::parser::parse` emits into
skinny `Tape`, read via `ValueRef`; proven by `PayloadArena` write/alloc counters; NOT
satisfiable by a grep in `crates/core/`)" — tied to the benched path + the verified
`writes` counter, not a tree-agnostic grep. The new `projection_generality_exercise` column
restricts valid values to `json`/`css_l4` and explicitly states "`sheets_witness` is NOT a
valid value here, it has no `BackendRule` to project from" — this is a V3-sharpened
anti-coupling telemetry binding that prevents a generality claim from being satisfied by a
stub witness. No defect.

### HANDOFF.md (= α-F packet)

**Benched-substrate disclosure (:12-18) + no-eager-OpenFrame (:36):** ACCEPT. Core-tree
symbols "grep-clean-absent from `skinny/crates/` (verified) … SK-V18 fold target, NOT
SK-V17 owner paths" (:12-13); benched substrate named (:15); benched CSS Track 1
fact-stream String path named (:17-18); "there is no eager `OpenFrame` tree [in skinny]"
(:36). Re-verified.

**HANDOFF "No second substrate" + escape set (:167-181):** ACCEPT. "an introduced skinny
`StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the landed `Tape`/`ValueRef` is a
Lock 1 type-ambivalence violation … no new cursor/builder type" (:167-170); escape list
(:176-181) includes "Track 1 == Track 2 sidecars, wrong-plane comparator admission,
cross-call classifier-state retention." Mirrors SYNTHESIS §0.4. Complete and correct.

**HANDOFF `tape_activated` gate + same-wave consumer (:207-215):** ACCEPT. `tape_activated`
"satisfied ONLY when the benched `track1::parser::parse` emits into the skinny runtime
`Tape`, read via `ValueRef`, proven by `PayloadArena` write/alloc counters — NOT by a grep
returning non-zero in `crates/core/`" (:213-215); "Each primitive lands WITH its hot-path
consumer in the same commit (no orphan kernels)" (:207-208). The NEON-gated-behind-tape
ordering prevents an orphan NEON scanner sidecar. No CH5 defect.

---

## §2 — Adversarial probes run THIS V3 cycle (recorded; all closed)

These are the hidden-coupling escapes CH5 actively hunted at HEAD `1c5bd7a25` against
both the live tree and the post-edit artefacts:

1. **Did the 14:20–14:25 artefact edits introduce a NEW sidecar / second-tape / renamed-
   scanner / eager-arena clause?** NO. A cohort-wide grep for `retain.*classifier`,
   `cross-call`, `dual tape`, `two tapes`, `eager arena`, `per-leaf Box`, `columnar`,
   `SoA` returns ONLY pre-block / REJECT-clause occurrences (alphaE:254-255,324-325;
   alphaC:81,347; SYNTHESIS:107,226-228; HANDOFF:181). No live admission. Closed.

2. **Is any place asserting CSS Track2 == Track1 (the dishonesty CH5 exists to catch)?**
   NO. Every `Track1==Track2` / `Track 1 == Track 2` occurrence (alphaC:194,
   alphaD:115, SYNTHESIS:225, HANDOFF:180) is in a PRE-BLOCK or honesty-gate context.
   The only substrate-equality assertion is the JSON `offset_stream() ==`
   substrate-ceiling probe (`track2/json.rs:368`), declared Lock-1 v+1 by design; CSS
   Track 2 is the independent cssparser oracle. Closed.

3. **Does the C2 NEON `Vec<u32>` structural index become a second retained substrate?**
   NO. SYNTHESIS §0.4 (:226-228) states "if structural offsets are retained, the
   structural projection IS the tape." Re-verified the JSON precedent
   (`StructuralIndex::from_positions`, `json/scan.rs:22,25,32`): the index is a transient
   producer feeding `value_from_ref`, not a retained parallel store. CSS follows the same
   shape. Closed.

4. **Does the lazy cursor view retain a parallel eager arena?** NO. Bound to the JSON
   `value_from_ref` shape (`json/value.rs:143`) over the existing `ValueRef`; alphaC §1
   binds the `PayloadArena` write/alloc counter gate to prove zero per-leaf alloc except
   irreducible decode (re-verified the `writes` counter, `tape/mod.rs:41,72`).
   preserve-rich-ast = on-demand reconstruction, not eager materialization. Closed.

5. **Could a renamed scanner satisfy `tape_activated` without moving the benched path?**
   NO. The gate is bound to `track1::parser::parse` (the benched fn, `nonjson_css_l4.rs:596`)
   + the `PayloadArena` `writes` counter, "NOT satisfiable by a grep in `crates/core/`."
   Closed.

6. **Does the new `projection_generality_exercise` telemetry column open a coupling escape
   (a generality claim satisfiable by a stub witness)?** NO. The column's valid value-set is
   restricted to `json`/`css_l4` with `sheets_witness` explicitly NOT-valid, and the
   exclusion is structurally grounded: the fail-closed control
   (`codegen/src/lib.rs:1075-1090`, re-verified) means `sheets_witness` has no `BackendRule`
   to project from. The V3 sharpening tightens — not loosens — the anti-coupling binding.
   Closed.

7. **Does any candidate introduce a new skinny cursor/builder type (second substrate)?**
   NO. alphaE:112-113,192, alphaD:151, SYNTHESIS §0.1/§0.3/§0.4, and HANDOFF:167-170 all
   state explicitly that no new cursor/builder type is introduced and that introducing one
   is REJECTed. Closed.

---

## §3 — Disposition tally

| Disposition | Count |
|---|---|
| ACCEPT | 25 |
| REVISE | 0 |
| REJECT | 0 |

ACCEPT rate = 25/25 = **100%**. Zero orphan REVISE; zero REJECT.

The V1 root coupling defect (totality-tree paths cited as the benched skinny surface) was
fully resolved at V2 and remains resolved at V3: the benched-surface note in SYNTHESIS
(:30-62) and HANDOFF (:12-18), the skinny-tree-bound `tape_activated` gate (SYNTHESIS :363,
HANDOFF :213-215), the second-substrate REJECT clause (SYNTHESIS :229-232, HANDOFF
:167-170), and the no-new-cursor/builder discipline (alphaE :112-113, alphaD :151) all
survive the post-V2 artefact edits intact. The V3 edits SHARPENED the anti-coupling posture
(real fail-closed-control citation for `sheets_witness`; restricted
`projection_generality_exercise` value-set; explicit C2 transient-producer framing) and
introduced NO new hidden-coupling hazard. The cohort holds tape+projection as ONE substrate
(Lock 1); the cursor-API lazy view is the JSON `value_from_ref` shape with no parallel eager
arena; the CSS Track 2 is an independent oracle, not a Track-1 re-projection. Every claim
above is grep-verifiable in `skinny/crates/` at HEAD `1c5bd7a25`.

CH5 converges at V3: ≥95% ACCEPT (100%), zero orphan REVISE, V=3 ≤ 5. Combined with the
V2 100%, this is the second consecutive ≥95% ACCEPT required by ORCHESTRATOR §3Z.
