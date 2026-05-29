# CH5 — HIDDEN-COUPLING (cycle V1)

Lens: CH5 Hidden Coupling per PASS-ALPHA §3 + ORCHESTRATOR §3W.
Subject: Pass Alpha SK-V17 artefacts (`research/alpha/{alphaA..E}.md` + `SYNTHESIS.md`
+ `HANDOFF.md`; there is no separate alphaF — α-F output = SYNTHESIS.md + HANDOFF.md
per PASS-ALPHA §2).
Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25`.
Focus: no parallel substrate / sidecar / renamed-scanner / Track1==Track2 dishonesty;
tape+projection is ONE substrate (Lock 1); cursor-API lazy view does not retain a
parallel eager arena.

---

## §0 — Ground truth established (every disposition below cites it)

Verified at `1c5bd7a25` by grep/read against the benched **skinny** tree:

| Fact | Evidence |
|---|---|
| Skinny tape substrate IS one substrate: `Tape{source,offsets,flag_cursors,flag_values,payloads}`, `ValueRef{tape,cursor}` (Copy), `PayloadArena`, `TapeBuilder`, `DocumentView` | `skinny/crates/runtime/src/tape/mod.rs:94,175,38,10,227` |
| JSON rides it lazily: `scan_structurals`→`value_from_ref(ValueRef::new(&tape,cursor))` — zero parallel eager arena | `skinny/crates/runtime/src/grammars/json/scan.rs:22`; `json/value.rs:143`; `json/view.rs:27,191,329` |
| Skinny Track 2 (JSON) asserts SAME substrate, not a second one: `track2.offset_stream() == track1.offset_stream()` | `skinny/crates/bbnf-bench/src/track2/json.rs:367-368` |
| Skinny CSS Track 2 reference is an INDEPENDENT parser (cssparser `OracleParser`), structurally distinct from Track 1 `track1::parser::parse` | `nonjson_css_l4.rs:596,624-633` |
| Benched skinny CSS Track 1 calls `track1::parser::parse` in **`skinny/crates/runtime`** (workspace dep, `bench-counters`) | `nonjson_css_l4.rs:596`; `bbnf-bench/Cargo.toml:18` |
| **`StructLayout` is grep-clean in `skinny/crates/`**; exists ONLY in totality `crates/ir/src/registry/struct.rs` | grep verified |
| **`OpenFrame` grep-clean in `skinny/crates/`** | grep verified |
| **`crates/core/src/runtime/tape/` does NOT exist**; the real skinny path is `crates/runtime/src/tape/` | `ls` verified (exit 1 vs exit 0) |
| **`crates/core/src/backend/rust/emitter/` does NOT exist** in skinny | `ls` verified (exit 1) |
| **`css_l4/builder.rs:274` does NOT exist** in the skinny benched tree | grep/find clean |
| The W6 tape report `sk-v16-w6tape-report.md` describes the **totality** tree (`crates/core/src/runtime/css_l4/builder.rs`, `crates/core/.../tape`, `StructLayout`), NOT the benched skinny tree | w6tape-report.md:13,24,87 |
| Lock 1 v+1: "Skinny Track 2 remains a substrate-ceiling probe, not a second substrate" | `LOCKS.md:75` (v+1 fold) |

**The central CH5 finding:** the SK-V17 >SOTA *subject* is measured in `skinny/`, but
the substrate-activation *receiver obligations* are written against the totality
`crates/core/` tree. **alphaE alone resolved this** (its load-bearing translation
correction, alphaE:37-51, frames all candidates against skinny paths and warns "CH1
will reject any goalset citing core-tree paths as the benched surface"). SYNTHESIS.md,
HANDOFF.md, alphaA, and alphaD did NOT adopt the correction and cite the core-tree
seam verbatim. This is a hidden-coupling hazard, not a cosmetic path typo: it admits a
"tape activated" gate passing in the WRONG tree while the benched skinny CSS path still
rides the fact-stream String — a structural Track-1-honesty failure (the exact
renamed/parallel-substrate dishonesty CH5 exists to catch).

---

## §1 — Dispositions

### alphaA (results extraction)

**alphaA §tree-citation (lines 130,156,158,224 + W4/W6 rows):** REVISE.
alphaA cites `crates/core/tests/css_l4_w6_typed_retime.rs` (:130), `crates/core/src/
runtime/builder.rs`/`builder_template.rs` (:156,158), and "rides the eager `OpenFrame`
path" (:34,224) as the benched surface. These are TOTALITY-tree paths; the benched
CSS Track 1 is `track1::parser::parse` in `skinny/crates/runtime` (nonjson_css_l4.rs:596).
The extracted Mbps rows are not themselves wrong (they are the measured numbers), but
attributing them to the `crates/core` `OpenFrame` path mislocates the substrate that
the SK-V17 work must touch. **Fix:** add a one-line tree-disambiguation header to
alphaA stating "all benched Track 1 numbers are from `skinny/crates/runtime` CSS
parsers; `crates/core/...` `OpenFrame`/`builder.rs` paths cited from the W6 report
describe the totality tree and are the architectural lineage, not the benched
surface" — i.e. import alphaE's correction (alphaE:37-51) so the hot-leaf attribution
(W6 56%/10%/34%) is bound to the right tree.

**alphaA measurement rows (W6 table :200, baseline retimes):** ACCEPT. The numbers
are measured and cited; no hidden-coupling claim is embedded in the raw rows.

**alphaA "eager OpenFrame" hot-path characterization (:189 `Vec<OpenFrame>` clone):**
ACCEPT as architectural lineage. It correctly names the Vec<OpenFrame>::clone /
parallel-substrate pathology (Lock 1 86.07% samply) as the thing NOT to recover.
Honest hidden-coupling posture. (Carries the tree caveat from the REVISE above.)

### alphaB (competitor deltas)

**alphaB (entire artefact):** ACCEPT. grep for `sidecar / second tape / Track 1 ==
Track 2 / parallel substrate / StructLayout` returns clean — alphaB confines itself to
comparator deltas (lightningcss full-CSSOM vs cssparser token-scan plane disclosure)
and embeds no substrate-coupling claim. The plane disclosure (full-CSSOM = fair bar;
token-scan = flaw probe) is the correct CH5-honest comparator framing: it prevents a
wrong-plane (token-scan) comparator admission, which is itself a hidden-coupling escape
listed in SYNTHESIS §0.4. No CH5 defect.

### alphaC (REDRESS digest)

**alphaC §1 AZ-IV eager-value-tree (ADMIT-UNDER-FRAMING, lazy-view re-open test
:53-65):** ACCEPT. This is the single best CH5 artefact section in the cohort. It
states the re-open test exactly (typed value built per-leaf at parse time / per-leaf
heap alloc) and binds it to the JSON `value_from_ref` zero-alloc proof and a
payload-arena write/alloc counter gate (REDRESS item 8). The `PayloadArena` write/alloc
counters it relies on are REAL (`tape/mod.rs:65-88`, `bench-counters` feature) — the
falsifiability is structurally grounded, not aspirational.

**alphaC §2 StructRegistry/Arena<G>/Builder<G> SPLIT (:84-115):** ACCEPT on the
hidden-coupling axis. Correctly classifies the per-leaf registry dereference as
PERMANENT PRE-BLOCK ("a Lock 1 parallel-substrate violation … the Vec<OpenFrame>::clone
86.07% samply pathology is the canonical example", :91-92) and the layout description
as ADMIT-UNDER-FRAMING. **Caveat (folds into the SYNTHESIS REVISE):** §2 cites
`StructLayout` / `css_l4/builder.rs:274` / `crates/ir/src/passes/types/registry.rs:140`
as the admission surface; these are totality-tree, grep-clean in skinny. The CH5
*reasoning* is sound (no per-leaf registry; build layout once per rule); only the cited
benched surface is the wrong tree. Disposition stays ACCEPT because the load-bearing
hidden-coupling claim (no per-leaf indirection; one substrate) is tree-independent and
correct, but the tree caveat is recorded for SYNTHESIS to absorb.

**alphaC §3 fact-stream String (:131-156):** ACCEPT. Explicitly names the
Track1==Track2 / sidecar failure mode ("This would also be a Track1==Track2 dishonesty
/ sidecar violation (Lock 1, CH5) if the String is the retained product", :143-144) and
binds the output-plane gate (never `digest`/`FactStream`). Exactly the CH5 discipline.

**alphaC §4 24-row broadcast (:173-191):** ACCEPT. PERMANENT PRE-BLOCK, no-different-
framing, with the broadcast-detection gate (distinct `measurement_row_id` +
`broadcast_group_id`). A broadcast is a hidden-coupling escape (one measurement coupled
to N conceptual rows); correctly closed.

**alphaC §5 FNV/fixture (:196-232) and §6 x86 (:236-266):** ACCEPT. Both correctly
classify the runtime-migration / row-movement re-open tests. The FNV-as-runtime-arbiter
and x86-as-same-wave-consumer are both coupling escapes; both pre-blocked with gate
consumers. No CH5 defect.

**alphaC §8 load-bearing distinction (:285-300):** ACCEPT — this paragraph is the
correct CH5 north star ("The flat lazy-offset tape … + the layout-driven typed
projection … is the ONLY admissible carrier"). It enforces one substrate.

### alphaD (validated/invalidated ledger)

**alphaD V6 / O1 / O2 / O5 rows (:32,79,80,83) tree-citation:** REVISE. alphaD cites
`crates/core/src/runtime/tape/{record,arena,cursor,mod}.rs`, `TapeStructBuilder`/
`TapeCursor`, `crates/core/src/grammar/generated/`, `css_l4/builder.rs`,
`regen_css.rs emit_builder/emit_view/emit_document/emit_arena`, and `crates/core/src/
backend/rust/emitter/` as the activation surface. All TOTALITY-tree. The benched skinny
substrate is `crates/runtime/src/tape/` with `Tape`/`ValueRef`/`TapeBuilder` (NOT
`TapeStructBuilder`/`TapeCursor`/`PayloadArena`-named record API per the W6 core-tree
report). **Hidden-coupling hazard:** O1 ("connect V6 substrate to the parse path") and
O2 ("lazy view over tape") as written would have an implementor wire the
`crates/core/.../tape` (totality) into a parse path the benched skinny CSS bench never
calls — yielding a dead parallel substrate while the benched path keeps the fact-stream
String. **Fix:** re-state V6/O1/O2/O5 against the skinny surface per alphaE:37-51 —
the activation target is `skinny/crates/runtime/src/tape/` + `skinny/crates/codegen/
src/lower/{offset_tape,tape_plan}.rs` + the skinny `RuntimeEmitterKind`; the
"layout-driven projection" is the skinny tape-plan lowering (`BackendRule` +
`TapeFlavor`/`render_rule`), with a lazy `ValueRef`-cursor accessor set isomorphic to
JSON's `value_from_ref`. The grammar-neutrality claim (O1: "JSON/sheets/bbnf already
implement the same `StructBuilder` trait") must be re-grounded on the skinny
`EventGrammar`/`DocumentView` traits (`tape/mod.rs:227,11`), which DO exist, rather than
the core-tree `StructBuilder`.

**alphaD "no StructRegistry/Arena<G> indirection (AZ-IV absent)" caveat (:32):**
ACCEPT. The hidden-coupling claim that the landed substrate carries no parallel-arena
indirection is correct for BOTH trees (skinny `Tape` has no `Arena<G>`; the
`PayloadArena` is a single bump buffer, `tape/mod.rs:38-89`). Honest.

### SYNTHESIS.md (= α-F contract)

**§0.1 "Tape activation (not dead code)" gate (:55):** REVISE. The gate cites
`crates/core/src/runtime/tape/` and `TapeStructBuilder`/`TapeCursor` and
`tests/tape_substrate.rs`. In skinny: the path is `crates/runtime/src/tape/`; there is
no `TapeStructBuilder`/`TapeCursor` (the skinny API is `Tape`/`ValueRef`/`TapeBuilder`/
`DocumentView`); `tests/tape_substrate.rs` is not the skinny location. **The CH5
hazard is precise:** the gate's falsifiability is "a grep for tape types in a parse
path returns non-zero." If the named tape types are the core-tree ones, the grep can
go non-zero in `crates/core/` while the **benched** skinny CSS Track 1
(`track1::parser::parse`, nonjson_css_l4.rs:596) still rides the fact-stream String —
the gate passes, the SOTA subject does not move, and the >SOTA number is reported off a
substrate the benched path never decoded. That is a renamed/parallel-substrate
dishonesty. **Fix:** bind the gate to the skinny tree: "`Tape`/`ValueRef`/`TapeBuilder`
from `skinny/crates/runtime/src/tape/` appear in the CSS parse path that
`bbnf-bench/src/nonjson_css_l4.rs:596 track1::parser::parse` invokes; the grep is run
over `skinny/crates/runtime/src/grammars/css_l4_*/` and the benched `track1` fn; the
benched Track 1 Mbps is measured on THAT path."

**§0.1 "Layout-driven projection" gate (:56) + §0.3 Receiver "Lazy-view projection
generator" (:84) + "Tape activation + builder seam flip" (:85):** REVISE. These name
`StructLayout` (`bbnf_ir::registry::struct.rs` `LayoutKind`+`FieldSource`),
`css_l4/builder.rs:274` "~40-arm match", `crates/core/src/backend/rust/emitter/`,
`regen_css.rs emit_builder`, the `OpenFrame` template — all totality-tree, all
grep-clean in skinny (verified). alphaE:37-51 explicitly warns this and re-frames every
candidate against skinny surfaces; SYNTHESIS did not absorb the correction.

  The hidden-coupling danger is twofold and must be closed before S-P3:
  (a) **wrong-tree activation** (as in §0.1 above) — wiring core-tree types the bench
  never consumes; and
  (b) **new-type parallel substrate** — if the implementor, finding no skinny
  `StructLayout`/`TapeStructBuilder`, *introduces* them skinny-side, those new types
  become a SECOND substrate alongside the already-landed `Tape`/`ValueRef` (Lock 1
  "type ambivalence: tape and OpenFrame and direct-to-struct competing for the same
  role", LOCKS.md:75). The CSS lazy view MUST be the JSON `value_from_ref` shape over
  the existing `Tape`/`ValueRef` — not a new `TapeCursor`/`TapeStructBuilder` pair.

  **Fix:** re-author §0.1 layout gate + §0.3 receiver rows against the skinny seam
  (per alphaE:43-50 and alphaC's-own corrected reading): the projection generator emits
  a lazy `ValueRef`-cursor accessor set (isomorphic to `json/value.rs:143`
  `value_from_ref`) from the skinny `BackendRule`/`TapeFlavor` lowering
  (`skinny/crates/codegen/src/lower/tape_plan.rs`), NOT from `bbnf_ir StructLayout`; the
  "builder seam flip" retires `emit_fact_stream` (generated.rs:5) in favour of tape
  `TapeBuilder` append, NOT an `OpenFrame`→`TapeStructBuilder` flip (there is no
  `OpenFrame` in skinny to flip). State explicitly: the CSS lazy view reuses the
  existing `Tape`/`ValueRef` — no new cursor/builder type is introduced (Lock 1 one
  substrate).

**§0.4 hidden-coupling pre-block paragraph (:127-136):** ACCEPT — and it is excellent.
It enumerates the exact CH5 escape set: "retained sidecars, retained sidecar tables,
sidecar event vectors, retained cursor/list, cursor streams, aux density/projection
tables, parser-owned structural projections or streams, parallel source passes, second
tapes, public `UnionTape`, … Track 1 == Track 2 sidecars, wrong-plane comparator
admission. A SIMD mask stream is a transient producer, not a retained sidecar; if
structural offsets are retained, the structural projection IS the tape (Lock 1,
LOCKS.md:75). Cross-call classifier-state retention is REJECT under Lock 1 v+1." This is
verbatim-correct Lock 1 discipline. No defect — this paragraph is what makes the REVISE
items above recoverable rather than rejectable.

**§0.6 comparator table — "Track 2 / oracle … structurally distinct from Track 1
(Lock 1, CH5)" (:179):** ACCEPT. Verified true at the benched surface: the CSS oracle
is the independent cssparser `OracleParser` (nonjson_css_l4.rs:624-633), not a
re-projection of Track 1. The Track2≠Track1 honesty gate is structurally real, not
asserted. The JSON Track 2 equality (`offset_stream==`) is the *substrate-ceiling*
probe (same substrate, by Lock 1 v+1 design), and the CSS Track 2 is the *independent*
oracle — both correct, neither is a Track1==Track2 dishonesty.

**§Section 2 telemetry — `tape_activated` boolean (:228) + `css_track1_typed_passes`
(:223):** REVISE (couples to the §0.1 REVISE). The `tape_activated` column is the gate
that can be satisfied dishonestly in the wrong tree. **Fix:** define `tape_activated`
as "the benched `track1::parser::parse` path emits into `skinny/crates/runtime/src/
tape::Tape` and the typed summary is read via `ValueRef` projection — proven by the
same `PayloadArena` write/alloc counters alphaC §1 binds (zero per-leaf alloc except
irreducible decode)." Tie `tape_activated == true` to the benched path's counter
telemetry, not to a tree-agnostic grep.

**§Section 2 `css_typed_summary_equal` gate-before-speed (:225):** ACCEPT. Equality
before speed is the correct ordering and prevents a fast-but-wrong (flattened/sidecar)
product from admitting. No CH5 defect.

### HANDOFF.md (= α-F packet)

**HANDOFF "Current State" (:13) + "What SK-V17 Opens" four-lever (:35-44):** REVISE.
Cites `crates/core/src/runtime/tape/`, `TapeRec`/`PayloadArena`/`TapeCursor`, "the eager
`OpenFrame` tree" (:18), `css_l4/builder.rs`. Same tree-conflation as SYNTHESIS. **Fix:**
re-state the substrate path as `skinny/crates/runtime/src/tape/` (`Tape`/`ValueRef`/
`TapeBuilder`); state the benched CSS Track 1 is `track1::parser::parse` riding the
fact-stream String (not an `OpenFrame` tree — there is no `OpenFrame` in skinny); the
"builder seam flip" is fact-stream→tape-append, reusing the existing `Tape`/`ValueRef`.

**HANDOFF "Pre-Blocked Routes" hidden-coupling escapes (:103-109):** ACCEPT. Mirrors
SYNTHESIS §0.4 verbatim; the CH5 escape set (sidecars, second tapes, Track1==Track2,
cross-call classifier-state retention) is complete and correct.

**HANDOFF "Next Move" wave sequencing (:118-126):** ACCEPT on the same-wave-consumer
axis. "Each primitive lands WITH its hot-path consumer in the same commit (no orphan
kernels)" (:126) is the correct Lock 1 substrate-first/consumer-later guard. The SIMD
gating ("gated behind tape — there is no structural index to pre-scan into until the
tape decodes CSS", SYNTHESIS §0.1:62) correctly prevents an orphan NEON scanner sidecar.
No CH5 defect — only carries the tree-path REVISE.

---

## §2 — The single orphan-REVISE risk (for orchestrator convergence)

alphaE already contains the fix (its translation correction §37-51). The REVISE items
above are NOT independent defects — they are ONE coupling defect (totality-tree paths
cited as the benched skinny surface) that alphaE resolved and that SYNTHESIS / HANDOFF /
alphaA / alphaD failed to inherit. Convergence requires the contract (SYNTHESIS §0.1,
§0.3, §Section 2 `tape_activated`; HANDOFF current-state + four-lever) to import
alphaE:37-51 verbatim: **the benched substrate is `skinny/crates/runtime/src/tape/`
(`Tape`/`ValueRef`/`TapeBuilder`/`DocumentView`); the CSS lazy view is the JSON
`value_from_ref` shape over that ONE tape; no `StructLayout`/`OpenFrame`/`TapeCursor`/
`TapeStructBuilder` skinny type is introduced; the `tape_activated` gate is bound to the
benched `track1::parser::parse` path + its `PayloadArena` counters.** With that single
correction folded, every REVISE flips to ACCEPT and zero orphan REVISE remains.

No REJECT: nothing in the cohort proposes a parallel substrate, a retained sidecar, a
renamed scanner, or a Track1==Track2 product. The hidden-coupling *discipline* (SYNTHESIS
§0.4, alphaC throughout, HANDOFF :103-109) is present and correct; only the cited
benched *surface* is the wrong tree, and that is a REVISE because it admits a wrong-tree
activation that WOULD become a parallel substrate if implemented literally.

---

## §3 — Disposition tally

| Disposition | Count |
|---|---|
| ACCEPT | 16 |
| REVISE | 6 |
| REJECT | 0 |

ACCEPT rate = 16/22 = **72.7%** (below the §3Z 95% bar; one folded correction lifts it
to 100% — see §2). All 6 REVISE share one root cause (tree-conflation), with alphaE's
own §37-51 as the ready fix.
