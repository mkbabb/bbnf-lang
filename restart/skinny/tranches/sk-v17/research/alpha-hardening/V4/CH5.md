# CH5 — HIDDEN-COUPLING (cycle V4)

Lens: CH5 Hidden Coupling per PASS-ALPHA §3 + ORCHESTRATOR §3W.
Subject: Pass Alpha SK-V17 artefacts — `research/alpha/{alphaA..alphaE}.md` +
`SYNTHESIS.md` + `HANDOFF.md` (α-F output = SYNTHESIS.md + HANDOFF.md per
PASS-ALPHA §2; there is no separate `alphaF` file — re-confirmed this cycle by
`find … -iname '*alphaF*' -o -iname '*alpha-f*'` returning empty).
Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25`
(`git rev-parse HEAD` = `1c5bd7a25250640f3a6fcfc00abed11f556f674f`, re-confirmed
this V4 cycle).
Focus: no parallel substrate / sidecar / renamed-scanner / Track1==Track2
dishonesty; tape+projection is ONE substrate (Lock 1); cursor-API lazy view does
not retain a parallel eager arena.

---

## §0 — Ground truth (independently re-greped at `1c5bd7a25`, this V4 cycle)

Every disposition below cites a fact re-verified against the benched **skinny** tree
THIS V4 cycle — not inherited from V1/V2/V3. The α-E artefact's V4 changelog
(`alphaE-candidate-shortlist.md:12-43`) declares the candidate content UNCHANGED from
V3 (it converged at V3 11/0/0); the V4 advance is the cycle stamp plus three
count-correction folds (V3-CH1-a, V3-CH1-b, F1 orphan) that touch only sibling
artefacts (alphaA, alphaC, alphaD) and "do NOT touch any α-E candidate, gate, owner
path, LOC budget, risk class, or REDRESS pre-block" (alphaE:42-43). CH5's V4 job is
independent: re-verify the LIVE tree, then confirm the V3 anti-coupling corrections
survived the V4 count-correction edits and that the edits introduced no fresh
sidecar / second-substrate / renamed-scanner / Track1==Track2 hazard.

| Fact | Evidence (re-verified V4) |
|---|---|
| Skinny tape IS one substrate: `Tape` :94, `ValueRef` :175, `PayloadArena` :38, `DocumentView` trait :227, `TapeBuilder` :42 | `skinny/crates/runtime/src/tape/mod.rs:94,175,38,227`; `assembler.rs:42` |
| `ValueRef` is generic over `G: EventGrammar = AnyGrammar` — neutrality vehicle is structural, not a CSS patch | `tape/mod.rs:175` (`ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>`) |
| Core-tree symbols GREP-CLEAN-ABSENT in skinny: `StructLayout`=0, `OpenFrame`=0, `CssArena`=0, `TapeStructBuilder`=0, `TapeCursor`=0, `begin_compound`=0 | per-symbol `grep -rn … skinny/crates/` = 0 each (re-verified V4) |
| JSON rides the tape lazily: `scan_structurals`→`StructuralIndex::from_positions`→`value_from_ref` (zero parallel eager arena) | `json/scan.rs:22,25,32,35`; `json/value.rs:143` |
| The JSON `StructuralIndex` is a TRANSIENT producer (`from_positions`) feeding `value_from_ref`, not a retained parallel substrate | `json/scan.rs:25,35` (`StructuralIndex::from_positions(…, ScanBackend::{Neon,Scalar})`) |
| Skinny JSON Track 2 asserts the SAME substrate (substrate-ceiling probe, Lock 1 v+1 by design): `track2.offset_stream() == track1.offset_stream()` | `track2/json.rs:366,367,368` (`emits_track1_compatible_offsets_without_calling_track1_parser`) |
| Skinny CSS Track 2 is an INDEPENDENT cssparser oracle (`OracleParser`/`oracle_facts`), structurally distinct from Track 1 (`track1::parser::parse`) | `nonjson_css_l4.rs:624,627` (`OracleParser::new`); `track1` import path `:596,597`; `cssparser` import `:12` |
| Benched CSS Track 1 is today a fact-stream String: `track1_facts -> Result<String,String>` → `track1::parser::parse` | `nonjson_css_l4.rs:596,597` (+ six sibling `*_track1_facts` fns :600-621) |
| `PayloadArena` carries a `writes` counter under `cfg(any(test, feature="bench-counters"))` — the `tape_activated` falsifiability is REAL | `tape/mod.rs:38,40,41,67,70` |
| Retire targets exist: `W5C_REQUEST_FACT_PROFILES` (lib.rs:336, selected :299, iterated :567,:611); seven `RuntimeEmitterKind::RequestFacts` literals (regen_css.rs:45,63,81,99,117,135,153); `regen_css` fn :164 | re-verified V4, all exact |
| `sheets_witness` is a 25-LOC STUB (24 `event_grammar_witness.rs` + 1 `mod.rs`) with NO `.bbnf` (`find skinny -path '*sheets*' -name '*.bbnf'` = empty)/parser/`BackendRule` | `runtime/src/grammars/sheets_witness/{event_grammar_witness.rs,mod.rs}` = 24+1 (re-verified V4) |
| Fail-closed-control is structurally grounded: codegen treats `google_sheets`/`bbnf` as fail-closed negative controls in a real test fn | `codegen/src/lib.rs:1075` `pub(super) fn w5a_sheets_bbnf_fail_closed_through_runtime_contract` (re-verified V4) |
| Broadcast row count is 24 (`grep -c '^| css_l4/.*/direct_to_struct/main '`=24); broader `grep -c 'css_l4/'`=25 (the 25th :154 is a prose reference, not a row) | `skinny/RESULTS.md` (re-verified V4) |

**The central CH5 disposition this V4 cycle.** V1 found ONE root coupling defect
(totality `crates/core/` paths cited as the benched surface, 6 REVISE); V2 absorbed
it and converged 100%; V3 re-verified the live tree and the post-V2 edits at 100%
(25/25). V4's job is to confirm the three count-correction folds (V3-CH1-a stale
meta-note rewrite, V3-CH1-b grep-substring mislabel, F1 orphan alphaD:154 O5
grammar-derivation relabel) did NOT loosen any anti-coupling binding. They did NOT.
An adversarial grep of the full V4 cohort for live coupling language —
`second tape|dual tape|two tapes|parallel substrate|eager arena|per-leaf box|
cross-call|retain.*classifier|sidecar|UnionTape|Track ?1 ?== ?Track ?2|
Track1==Track2` — returns ONLY pre-block / escape-enumeration / honesty-gate
occurrences (alphaD:115 I1/I2 pre-block list; alphaC:272,286 FNV hash-sidecar
pre-block; SYNTHESIS:228-234 and HANDOFF:180-185 the forbidden escape enumeration).
NO live admission exists. The three V4 folds are count-corrections (24 vs 6;
grep-substring labelling; TOML-LOC-vs-grammar-derivation), none of which carry a
substrate, sidecar, or Track-equality claim. No NEW hidden-coupling hazard exists.

---

## §1 — Dispositions

### alphaA (results extraction)

**alphaA §0 benched-surface disambiguation + core-tree retime citations:** ACCEPT.
Core-tree symbols (`StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`) are
labelled the TOTALITY fold target, not SK-V17 owner paths (re-verified all six return
0 in `skinny/crates/`); every throughput row binds to the skinny benched path; W6
retime numbers carry an S-P1 re-confirm-on-benched-path obligation, not mislocated as
the benched surface. No hidden-coupling hazard.

**alphaA cross-artefact reconciliation note (V4 fold, :138-148):** ACCEPT. The V4
fold rewrites the stale/self-contradictory note (V3-CH1-a) to "All cohort artefacts
state 24 / lines 112-135 as of V3 … the V2 '6' undercount is resolved across the
cohort"; re-verified `grep -c '^| css_l4/.*/direct_to_struct/main '`=24 and broader
`css_l4/`=25. This is a pure count reconciliation; it carries NO substrate / sidecar /
Track-equality claim. The substantive conclusion ("zero ADMITTED typed CSS rows; the
24 falsified broadcast rows must NOT be lifted as a baseline") is the anti-broadcast
posture CH5 endorses. No coupling hazard.

**alphaA measurement rows (W6 table, baseline retimes):** ACCEPT. Measured + cited;
no substrate-coupling claim embedded.

### alphaB (competitor deltas)

**alphaB (entire artefact):** ACCEPT. Re-greped clean this V4 cycle for `second tape /
parallel substrate / Track1==Track2 / sidecar / re-projection` (zero matches). Confines
itself to comparator deltas with plane disclosure (lightningcss full-CSSOM = the fair
materializing bar :5-7; cssparser token-scan = non-fair flaw probe, "NOT a materialized
typed CSSOM" :23,26). The plane disclosure forecloses the wrong-plane (token-scan)
comparator admission — itself a hidden-coupling escape enumerated in SYNTHESIS §0.4. No
defect.

### alphaC (REDRESS digest)

**alphaC §0 tree-disambiguation (:18-34):** ACCEPT. Leads with the αE §0:37-51
correction, grep-verified at HEAD `1c5bd7a25`. The 5-row mapping table re-keys each
core-tree symbol to its skinny benched equivalent (layout → `BackendRule`/`LayoutFacts`;
eager builder → "does not exist in skinny; eager pathology lives only at
`crates/core/.../css_l4/arena.rs`"; tape substrate → `skinny/…/tape/`; projection
catalogue `css_l4.toml` → "does NOT exist in skinny"). The admission surface is correctly
localised to skinny; the eager-arena/Box pathology is the totality tree, skinny's eager
pathology is the fact-stream String. Tree-correct.

**alphaC §1 AZ-IV eager-value-tree (ADMIT-UNDER-FRAMING):** ACCEPT. Binds the re-open
test (typed value built per-leaf / per-leaf heap alloc) to the JSON `value_from_ref`
zero-alloc proof and the `PayloadArena` write/alloc counter gate — re-verified the
`writes` counter is real (`tape/mod.rs:41,70`). Structurally grounded falsifiability.

**alphaC §2 StructRegistry/Arena<G>/Builder<G> + table row 2a (:345-347):** ACCEPT.
Row 2a classifies "any per-leaf registry lookup; per-compound Vec/split_off/Box;
frame-stack clone checkpoint; new hand-coded per-grammar profile table re-introduced
into `skinny/crates/runtime/`" as a PERMANENT PRE-BLOCK with "— (none)" admission — the
precise anti-second-substrate detection. Load-bearing claim (no per-leaf indirection;
one substrate) correct and tree-correct.

**alphaC §3 fact-stream String (:185,194,209):** ACCEPT. Names the Track1==Track2 /
sidecar failure mode ("Track1==Track2 dishonesty / sidecar violation (Lock 1, CH5) if
the String is the retained product", :194) and binds the output-plane gate ("the gate
rejects a CSS admit whose output plane is a `digest`/`FactStream`", :209). Lock 1's
`FactStream`-only-as-output-plane clause (LOCKS.md:585) cited correctly (:185). Exactly
the CH5 discipline.

**alphaC §4 24-row broadcast (V3-CH1-b fold):** ACCEPT. PERMANENT PRE-BLOCK with the
broadcast-detection gate; the V4-folded count is grep-correct (24 `^| css_l4/` rows;
25th :154 a prose REDRESS-127 companion reference; `grep 'W6.*css|tape.*direct_to_struct'`
EMPTY — no admitted/distinct W6 typed CSS row). The broadcast is the canonical
one-measurement-coupled-to-N-rows hidden-coupling escape; correctly closed. The V4 fold
sharpens the count label without loosening the pre-block.

**alphaC §5/§6/§8 + FNV hash-sidecar (:272,286):** ACCEPT. FNV/fixture runtime-migration,
x86 same-wave-consumer, and the §8 north star ("the flat lazy-offset tape … is the ONLY
admissible carrier") are coupling-honest with gate consumers. The FNV hash-sidecar
"remains blocked unless future work proves typed semantics independently of hash
sidecars" (:286) is a pre-block, not an admission. No defect.

### alphaD (validated/invalidated ledger)

**alphaD §0 translation-correction adoption:** ACCEPT. Adopts αE:37-51 verbatim,
grep-verifies the six core-tree symbols return zero, maps each to its skinny equivalent.

**alphaD I1/I2 + Track1==Track2 pre-block (:115):** ACCEPT. Cost attributed to
"typed-value materialization + arena/builder indirection (core) / fact-stream String
serialization (skinny)" — both trees named; "sidecars / second tapes / public
`UnionTape` / Track1==Track2" appears in a pre-block context, not an assertion. No
conflation.

**alphaD O2 (lazy view + cursor API, :151):** ACCEPT. The load-bearing CH5 sentence is
present verbatim: lazy `ValueRef`-cursor accessor set "**isomorphic to JSON's
`value_from_ref`** (`json/value.rs:143`) over the EXISTING `Tape`/`ValueRef` — **no new
cursor/builder type is introduced** (a second cursor type would be a Lock-1
type-ambivalence violation)." preserve-rich-ast bound to on-demand reconstruction, not
flattening. `sheets_witness` named structurally non-dischargeable (24-LOC, no
`BackendRule`) with the fail-closed control cited to `codegen/src/lib.rs:1075-1090` —
re-verified the test fn exists at :1075. The precise anti-second-substrate guard. No
defect.

**alphaD O5 (codegen unification + overfit removal, V4 F1-orphan fold, :154):** ACCEPT.
The F1 orphan fold relabels the close gate to grammar-derivation: "The skinny-greppable
exit gate is grammar-derivation, NOT TOML-LOC count … the 594-line `css_l4.toml` LOC
convergence is a TOTALITY metric (SK-V18 fold), INFORMATIONAL only, NOT an SK-V17
close/exit gate (SYNTHESIS §0.1)." This brings alphaD O5 into verbatim agreement with
SYNTHESIS §0.1:111 / HANDOFF:139-146. The retire-list correctly names
`W5C_REQUEST_FACT_PROFILES`, the 148-fn fixture surface, and (TOTALITY fold) the
`css_l4.toml` catalogue. The fold is a gate-scoping correction; it carries NO coupling
claim and tightens (does not loosen) the no-relocated-overfit posture. No defect.

**alphaD O1/O5 (tape wiring + codegen unification owner paths):** ACCEPT. Owner paths
all skinny, consuming `TapeBuilder`/`ValueRef`; the 594-line `css_l4.toml` localised as
a TOTALITY fold target, not a skinny owner path; the eager pathology localised to
`crates/core/.../css_l4/arena.rs`. No CH5 defect.

### alphaE (candidate shortlist) — the load-bearing CH5 artefact

**alphaE V4 changelog (:12-43):** ACCEPT. Correctly declares the candidate content
UNCHANGED from V3 (converged 11/0/0); the three V4 dispositions (V3-CH1-a, V3-CH1-b,
F1 orphan) target sibling artefacts and are verified already-landed, touching no α-E
candidate / gate / owner path / LOC budget / risk class / REDRESS pre-block. No
coupling content moved.

**alphaE §0 translation correction + C0/C1 (:127-151,224-225,251-255):** ACCEPT. The
CH5 load-bearing sentence is present verbatim: "**no new cursor/builder type is
introduced; the existing `Tape`/`ValueRef`/`TapeBuilder` is reused**" (:146-147) and
"the existing `Tape`/`ValueRef`/`TapeBuilder` is the single substrate (Lock 1: no second
tape, no type-ambivalence)" (:224-225). Re-verified `value_from_ref` (`json/value.rs:143`)
is the lazy zero-alloc shape. One substrate, correctly framed.

**alphaE C2 (NEON structural pre-scan, :310-311,334-338,356-360):** ACCEPT. The NEON path
produces ONLY the `Vec<u32>` index the C1 tape consumes ("NEON produces ONLY the index
(like JSON); speed comes from the scan, never from dropping structure", :336-338) —
re-verified the JSON precedent is a TRANSIENT producer (`StructuralIndex::from_positions`,
`json/scan.rs:25,35`), not a retained parallel substrate. The C2 pre-block bars "No
cross-call classifier-state retention (Lock 1, LOCKS.md:585 — 'cross-call classifier
state remains rejected'; carry stays within a single chunk-call)" (:357-360). The precise
anti-sidecar guard. No defect.

**alphaE C3/C4a/C4b + REDRESS pre-blocks (:285-296,408-410):** ACCEPT. C1 pre-block bars
"no parallel substrate / no second tape (Lock 1, LOCKS.md:75,585) … no columnar SoA
resurrection; no per-leaf eager `Box::new`" (:285-289) — the full anti-coupling set; C3
bars "No type-ambivalent dual representation (Lock 1 — 'tape and OpenFrame and
direct-to-struct competing for the same role')" (:408-410). C4a wires the udot orphan
with its scalar twin (`digit_mac.rs:27,40`); C4b is GATED net-new and lands only if the
re-profile proves the digit leaf is top-N (no orphan kernel). No coupling defect.

### SYNTHESIS.md (= α-F contract)

**Benched-surface note (:31-68):** ACCEPT. Core-tree symbols "grep-clean-absent from
`skinny/crates/`" (:36-40, re-verified all 0); the benched substrate named with line
numbers (:41-44); core-tree symbols are "the **design-intent fold target** … SK-V18 work,
not SK-V17 owner paths" (:65-68). The V1 fix folded and surviving the V4 edits intact.

**§0.1 "Tape activation" gate (:110):** ACCEPT. Bound to the skinny tree:
"`PayloadArena` write/alloc counters (per alphaC §1) confirm the parse emits into the
tape rather than into a fact-stream String … No new cursor/builder type is introduced —
the EXISTING skinny `Tape`/`ValueRef`/`TapeBuilder` is the only substrate (Lock 1, no
second tape)." Falsifiability tied to the benched path + the verified `writes` counter,
not a tree-agnostic grep. The V1 wrong-tree hazard is foreclosed.

**§0.1 "Layout-driven projection" gate (:111) + NEON-gated-behind-tape (:117):** ACCEPT.
The generated lazy accessors are "`ValueRef`-cursor reads isomorphic to JSON's
`value_from_ref` (`json/value.rs:143`) over the existing skinny `Tape`/`ValueRef`"; routing
DERIVED from `BackendRule`, "NOT lost and NOT re-hardcoded (Lock 14)"; the `css_l4.toml`
LOC asymmetry is "INFORMATIONAL only, NOT an SK-V17 close gate; gating an SK-V17 close on a
non-benched totality file would be the wrong-tree dishonesty this contract REJECTs" — the
V4-aligned self-policing CH5 clause. The NEON leaf "produces only a `Vec<u32>` structural
index, and the tape consumes it … NEON is gated behind tape activation" (:117) — matches
the JSON transient-producer shape exactly. No orphan-scanner sidecar. No defect.

**§0.3 receiver rows (:178-179):** ACCEPT. "Lazy-view projection generator" states "NO new
cursor/builder type is introduced — the existing skinny `Tape`/`ValueRef` is reused"
(:178); "Tape activation + builder seam flip" retires `emit_fact_stream` → skinny
`TapeBuilder` append (the seven `RequestFactsProfile` literals `regen_css.rs:45,63,81,99,
117,135,153` — re-verified all seven carry `RuntimeEmitterKind::RequestFacts`), DELETEs
`W5C_REQUEST_FACT_PROFILES` (re-verified lib.rs:336, consumers :567,:611, selected :299),
and warns "the seam must accept the tape sink without re-introducing a second substrate"
(:179). The §0.3 preamble marks the totality `emit_builder`/`OpenFrame`
template/`css_l4/builder.rs:274` as "the SK-V18 fold target, NOT SK-V17 owner paths"
(:171-173). Both V1 dangers (wrong-tree activation; new-type parallel substrate) closed.

**§0.4 hidden-coupling pre-block + second-substrate clause (:227-239):** ACCEPT — the
strongest CH5 paragraph in the cohort. Full escape set enumerated (retained sidecars,
sidecar tables/event vectors, retained cursor/list, cursor streams, aux density/projection
tables, parser-owned structural projections/streams, parallel source passes, second tapes,
public `UnionTape`, new substrate APIs, sixth `BackendShape`, FNV arbiters, Track 1 ==
Track 2 sidecars, wrong-plane comparator admission, cross-call classifier-state retention);
"A SIMD mask stream is a transient producer, not a retained sidecar; if structural offsets
are retained, the structural projection IS the tape (Lock 1, `LOCKS.md:75`)" (:233-235) —
correctly resolves the C2 `Vec<u32>` as the tape, not a sidecar; "Cross-call
classifier-state retention is REJECT under Lock 1 v+1" (:235); the second-substrate clause
"if the implementor introduces a skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`,
those would become a SECOND substrate alongside the landed `Tape`/`ValueRef` (Lock 1
type-ambivalence) and are REJECTed; the projection generator emits accessors over the
EXISTING `Tape`/`ValueRef`" (:236-239). Verbatim-correct Lock 1 discipline.

**§0.4 generality clause (:241-264):** ACCEPT. Generality is "exercised, not asserted";
exercised riders are JSON + CSS only; `sheets_witness` is "NOT a viable third exercise" —
a 24-line `EventGrammar` byte-classification trait impl (2 files, 25 LOC) with NO `.bbnf`/
parser/`BackendRule`, and "codegen treats `sheets`/`bbnf` as fail-closed negative controls
(`codegen/src/lib.rs:1075-1090`)" — re-verified the test fn exists at :1075 and no `.bbnf`
exists for sheets. The NEON `simd_non_json_exercise` (`css_l4`) is correctly distinguished
as the genuinely-dischargeable non-JSON exercise. Forecloses the by-construction neutrality
escape with a structurally-grounded reason. No defect.

**§0.6 comparator table — "Track 2 / oracle … structurally distinct from Track 1 (Lock 1,
CH5)" (:320):** ACCEPT. Re-verified at the benched surface: the CSS oracle is the
independent cssparser `OracleParser`/`oracle_facts` (`nonjson_css_l4.rs:624,627`), not a
re-projection of Track 1 (`track1::parser::parse`, :596,597). The JSON Track 2 equality
(`offset_stream() ==`, `track2/json.rs:368`) is the declared substrate-ceiling probe (same
substrate, Lock 1 v+1 by design); the CSS Track 2 is the independent oracle. Both correct;
neither is a Track1==Track2 dishonesty.

**§Section 2 telemetry — `tape_activated` (:370) + `projection_generality_exercise`
(:372) + `simd_non_json_exercise` (:376):** ACCEPT. `tape_activated` is "boolean (benched
`track1::parser::parse` emits into skinny `Tape`, read via `ValueRef`; proven by
`PayloadArena` write/alloc counters; NOT satisfiable by a grep in `crates/core/`)" — tied
to the benched path + the verified `writes` counter. `projection_generality_exercise`
restricts valid values to `json`/`css_l4` and states "`sheets_witness` is NOT a valid value
here, it has no `BackendRule` to project from" — an anti-coupling telemetry binding that
prevents a generality claim being satisfied by a stub witness. `simd_non_json_exercise` is
cleanly separated (`css_l4` shares the `select_classifier(alphabet)` kernel — a real
rider). No defect.

### HANDOFF.md (= α-F packet)

**Benched-substrate disclosure (:13-24) + no-eager-OpenFrame (:40):** ACCEPT. Core-tree
symbols "grep-clean-absent from `skinny/crates/` (verified) … SK-V18 fold target, NOT
SK-V17 owner paths" (:16-18); benched substrate named with lines (:18-20); benched CSS
Track 1 fact-stream String path named (:20-23); "there is no eager `OpenFrame` tree in
skinny (that is the totality tree)" (:40). Re-verified.

**HANDOFF "No second substrate" + escape set (:171-185):** ACCEPT. "an introduced skinny
`StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the landed `Tape`/`ValueRef` is a
Lock 1 type-ambivalence violation (REJECT). The projection generator emits accessors over
the EXISTING `Tape`/`ValueRef`; no new cursor/builder type" (:171-174); escape list
(:180-185) includes "Track 1 == Track 2 sidecars, wrong-plane comparator admission,
cross-call classifier-state retention." Mirrors SYNTHESIS §0.4. Complete and correct.

**HANDOFF `tape_activated` gate + same-wave consumer (:211-220):** ACCEPT. `tape_activated`
"satisfied ONLY when the benched `track1::parser::parse` emits into the skinny runtime
`Tape`, read via `ValueRef`, proven by `PayloadArena` write/alloc counters — NOT by a grep
returning non-zero in `crates/core/` (wrong-tree dishonesty is REJECTed)" (:217-220); "Each
primitive lands WITH its hot-path consumer in the same commit (no orphan kernels)" (:211-212).
The NEON-gated-behind-tape ordering (W3 before W4, :206-210) prevents an orphan NEON scanner
sidecar. No CH5 defect.

---

## §2 — Adversarial probes run THIS V4 cycle (recorded; all closed)

Hidden-coupling escapes CH5 actively hunted at HEAD `1c5bd7a25` against both the live tree
and the V4 cohort:

1. **Did the three V4 count-correction folds (V3-CH1-a, V3-CH1-b, F1 orphan) introduce a
   NEW sidecar / second-tape / renamed-scanner / eager-arena / Track-equality clause?** NO.
   All three are count/label/gate-scope corrections (alphaA:138-148 broadcast count;
   alphaC §4 grep-substring labelling; alphaD:154 O5 grammar-derivation-not-TOML-LOC). None
   carries a substrate, sidecar, or Track-equality claim. A cohort-wide grep for live
   coupling language returns ONLY pre-block / escape-enumeration / honesty-gate occurrences
   (alphaD:115; alphaC:272,286; SYNTHESIS:228-234; HANDOFF:180-185). No live admission.
   Closed.

2. **Is any place asserting CSS Track2 == Track1 (the dishonesty CH5 exists to catch)?** NO.
   Every `Track1==Track2` / `Track 1 == Track 2` occurrence is in a PRE-BLOCK or
   honesty-gate context. The only substrate-equality assertion is the JSON `offset_stream()
   ==` substrate-ceiling probe (`track2/json.rs:368`, the test fn
   `emits_track1_compatible_offsets_without_calling_track1_parser`, :364), declared Lock-1
   v+1 by design; CSS Track 2 is the independent cssparser oracle (`OracleParser`,
   `nonjson_css_l4.rs:627`). Closed.

3. **Does the C2 NEON `Vec<u32>` structural index become a second retained substrate?** NO.
   SYNTHESIS §0.4 (:233-235) states "if structural offsets are retained, the structural
   projection IS the tape." Re-verified the JSON precedent (`StructuralIndex::from_positions`,
   `json/scan.rs:25,35`): the index is a transient producer feeding `value_from_ref`, not a
   retained parallel store. CSS follows the same shape. Closed.

4. **Does the lazy cursor view retain a parallel eager arena?** NO. Bound to the JSON
   `value_from_ref` shape (`json/value.rs:143`) over the existing `ValueRef`; alphaC §1 +
   row 2a (`tape/mod.rs:38`) bind the `PayloadArena` write/alloc counter gate to prove zero
   per-leaf alloc except irreducible decode (re-verified the `writes` counter,
   `tape/mod.rs:41,70`). preserve-rich-ast = on-demand reconstruction, not eager
   materialization. Closed.

5. **Could a renamed scanner satisfy `tape_activated` without moving the benched path?** NO.
   The gate is bound to `track1::parser::parse` (the benched fn, `nonjson_css_l4.rs:596,597`)
   + the `PayloadArena` `writes` counter, "NOT satisfiable by a grep in `crates/core/`."
   Closed.

6. **Does `projection_generality_exercise` open a coupling escape (generality satisfiable by
   a stub witness)?** NO. The valid value-set is restricted to `json`/`css_l4` with
   `sheets_witness` explicitly NOT-valid; the exclusion is structurally grounded — the
   fail-closed control (`codegen/src/lib.rs:1075`, re-verified) means `sheets_witness` has no
   `BackendRule` to project from (no `.bbnf`, find = empty). Closed.

7. **Does any candidate introduce a new skinny cursor/builder type (second substrate)?** NO.
   alphaE:146-147,224-225; alphaD:151; SYNTHESIS §0.1/§0.3/§0.4; HANDOFF:171-174 all state
   explicitly that no new cursor/builder type is introduced and that introducing one is
   REJECTed. Closed.

8. **Did the F1 orphan fold (alphaD O5 :154) silently re-admit the `css_l4.toml` as an
   SK-V17 owner path or relocate overfit into projection data?** NO. The fold tightens the
   gate to grammar-derivation, demotes `css_l4.toml` LOC to an INFORMATIONAL SK-V18 totality
   metric, and re-asserts the retire-list (`W5C_REQUEST_FACT_PROFILES`, 148-fn fixture). It
   is the no-relocated-overfit posture sharpened, not loosened. Closed.

---

## §3 — Disposition tally

| Disposition | Count |
|---|---|
| ACCEPT | 27 |
| REVISE | 0 |
| REJECT | 0 |

ACCEPT rate = 27/27 = **100%**. Zero orphan REVISE; zero REJECT.

The V1 root coupling defect (totality-tree paths cited as the benched skinny surface) was
fully resolved at V2, held at V3 (100%), and remains resolved at V4: the benched-surface
note in SYNTHESIS (:31-68) and HANDOFF (:13-24), the skinny-tree-bound `tape_activated`
gate (SYNTHESIS :370, HANDOFF :217-220), the second-substrate REJECT clause (SYNTHESIS
:236-239, HANDOFF :171-174), and the no-new-cursor/builder discipline (alphaE :146-147,224-225,
alphaD :151) all survive the V4 count-correction edits intact. The three V4 folds (V3-CH1-a
broadcast-count rewrite, V3-CH1-b grep-substring relabel, F1 orphan alphaD O5
grammar-derivation relabel) are count/label/gate-scope corrections carrying NO substrate /
sidecar / Track-equality claim, and they SHARPEN (do not loosen) the anti-coupling posture.
The cohort holds tape+projection as ONE substrate (Lock 1); the cursor-API lazy view is the
JSON `value_from_ref` shape with no parallel eager arena; the CSS Track 2 is an independent
cssparser oracle, not a Track-1 re-projection; the C2 NEON `Vec<u32>` is a transient producer
that IS the tape, not a sidecar. Every claim above is grep-verifiable in `skinny/crates/` at
HEAD `1c5bd7a25`.

CH5 converges at V4: ≥95% ACCEPT (100%), zero orphan REVISE, V=4 ≤ 5. Combined with the V2
(100%) and V3 (100%), this is the third consecutive ≥95% ACCEPT — well clear of the two
consecutive required by ORCHESTRATOR §3Z.
