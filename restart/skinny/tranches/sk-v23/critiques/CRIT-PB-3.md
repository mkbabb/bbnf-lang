# CRIT-PB-3 — The greenfield value-plane API sketch, adversarially read

Critique of `prototypes/PROTO-PB-3.md` and the codex section it tests
(`SPEC-DRAFT.md` TB4.W2, lines 290-331; PB-3 register, 712-722). Adversarial,
numerically conservative. NO cargo run; verdict is by source read against the live
skinny tree at `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/`.

**Convergence on the value-plane greenfield: 64%.** The JSON half is genuinely
de-risked and the neutral vocabulary is a real idea. But the headline verdict —
"the constructive answer to the sheets disclaimer" — does NOT follow from the
evidence, and the codex propagates that over-claim into a retirement it cannot
gate. One unresolved design question (retire-vs-carry sheets) remains, and the
codex currently resolves it wrongly.

---

## 1. Method honesty — mostly clean; two overstatements

PB-3 is a **buildability sketch, not a benchmark**. It makes no timing claim, so
the loadavg / cold / paired-window profiling law is correctly N/A — there is
nothing to label. The report is honest that it is an "analysis instrument, never
landed," dependency-free, `cargo run` exits 0 with four assertions and two
disclosed dead-code warnings. Good hygiene. The scratch crate survives at
`/private/tmp/pb3-scratch/src/main.rs` (602 LOC; neutral surface main.rs:77-360).

**The tape mirror is faithful.** `pb3-scratch/src/main.rs:20-40` reproduces
`skinny crates/runtime/src/tape/mod.rs:92-155`: `source:&[u8]` + `offsets:Vec<u32>`
+ sparse `(flag_cursors,flag_values)` + the four accessors `source()/offsets()/
offset_at()/flags_at()`. Two faithful-omission notes, neither fatal but both
worth carrying: the real `flags_at` returns `Option<OffsetFlags>` (a newtype),
not `Option<u8>` (mod.rs:142); and the real `Tape` carries `payloads:PayloadArena`
+ `id:TapeId` (mod.rs:96-97) that the sketch drops. The payload arena matters —
it is where a grammar can stash *richer decoded data than a byte span*, and the
sketch's re-scan-only model (caveat 2) silently assumes no consumer needs it.

**The JSON lane logic is internally sound.** I traced canonical/minify/path/struct
by hand (main.rs:306-360, 476-533); the assertions are self-consistent. The
generated.rs scanner citations corroborate: commas/colons are consumed but not
recorded as offsets (`generated.rs:274` skips `b':'|b','`; `327/361` handle the
comma; the offset is emitted only for the close — `318/352`). So `record_json`
seals a byte-faithful offset sequence.

**Overstatement A — the byte-identity oracle was eyeballed, not run.** The report
(line 77) says `to_canonical(root)` "equals the `json/view.rs:116`
`to_canonical_string` output character-for-character." It does no such
mechanical comparison. `main.rs:543` checks against a **hand-transcribed literal**;
the real `to_canonical_string` (view.rs:116-137) is never invoked in the crate.
The real function sorts keys AND escapes via `serde_json::to_string`
(view.rs:126-133); the sketch's `push_json_string` (main.rs:345-359) is a partial
hand escaper (`" \ \n \r \t` + control only). On the ASCII-clean fixture the two
coincide, so the literal is *plausibly* faithful — but "character-for-character
equals json/view.rs" asserts a differential that did not execute. Caveat 1
honestly flags the non-ASCII gap; the headline sentence does not inherit that
honesty. TB4.W2 must make this a **real** invocation-vs-invocation oracle, not a
literal.

**Overstatement B — "8-byte View" is false.** Report line 71 and main.rs:184-188:
`View` = `&'d Tape`(8 bytes) + `cursor:u32`(4) + `PhantomData`(0) = 16 bytes
aligned. A `&Tape` reference alone is 8 bytes; the claim is off by 2x. Real
`ValueRef` (tape/mod.rs:173-177) is the same 16 bytes. Minor — the substantive
"small Copy handle" point holds — but it is a false precision inherited from the
loose css `generated.rs:165` "8 bytes of cursor" comment.

---

## 2. THE LOAD-BEARING FLAW — the verdict does not follow for the sheets half

The report's crux claim (lines 9-15, 44-51) is that `Classify → NodeClass`
"is the constructive answer to the sheets disclaimer." Read the disclaimer it
cites (`tape/cursor.rs:8-19`) verbatim:

> "a tree-shaped grammar keeps its keyed/recursive navigation + typed value tree;
> a flat-tape grammar keeps its node sweep ... the navigation each callsite needs
> is not on the shared seam."

The disclaimer is about **navigation shape**, only secondarily about kind *names*.
PB-3 answers the kind-name half (project the 39 names into a structural alphabet)
and **assumes away the navigation half** — which is the half cursor.rs actually
calls non-collapsible. Three facts, from the live tree, break the verdict:

**(a) The neutral surface is JSON-tree-shaped through and through.** Every
navigation primitive — `next_sibling` (main.rs:107-124) balanced Enter/Leave
depth counting, `Entries`/`Items` terminating on `NodeClass::Leave`
(main.rs:251,268), `Path` Key/Index — presupposes *balanced container structure
with Leave markers*. This is the JSON tree model, ported.

**(b) Sheets has no such structure.** `sheets/view.rs:252-255`: navigation is a
**flat linear sweep** — `nodes() = (0..offsets().len()).map(...)`. Every recorded
cursor is ONE node carrying ONE `rule_kind()` (one of 39: CellRef, FuncCall,
AddExpr, LambdaCall, ArrayLiteral…, view.rs:8-48). There are **no Leave markers**
on the sheets tape. Over a Leave-free tape, PB-3's `Entries`/`Items` have no stop
token and `next_sibling` depth-counting is inapplicable. The header even says it:
"The flat node sweep lives ENTIRELY outside this trait" (view.rs:207).

**(c) NodeClass cannot carry rule_kind — projecting it IS the flattening PB-3
disavows.** The neutral alphabet is `Enter/Leave(Map|Seq) | Scalar{Str,Int,Float,
Bool,Null,Raw}` (main.rs:80-95). A sheets `FuncCall` / `CellRef` / `AddExpr` node
is none of these. The **live** sheets consumer navigates precisely by kind:
`crates/runtime/tests/sheets_conformance.rs:90-91`:

    document.nodes().find(|node| node.rule_kind() == Some(kind))

`find(rule_kind == GoogleSheetsKind::Boolean)` has no NodeClass equivalent, and
"find the CellRef node" has no representative at all. Projecting 39 rich kinds
into 8 structural classes is a **lossy flattening** — the exact operation the
report (lines 44-51) insists never happens. The consumer that reads `rule_kind`
is simply not served by the neutral plane.

**(d) "Grammar #2" is a strawman, not a sheets stand-in.** The report's "decisive
proof of neutrality" (lines 52-57) is `FlatClass` over `"1 22 333"`
(main.rs:456-470,578-599): three `Scalar(Float)` nodes, **zero Enter/Leave**,
consumed via `drive` (a Sink sweep) plus one `as_f64()` on the root. It exercises
the degenerate all-scalar path and nothing else — not the flat sweep over 39
kinds, not `entries/items/path` over a flag-plane grammar. Calling it "a
`GoogleSheetsKind`-shaped classifier" (line 56) is unsupported: it models none of
GoogleSheetsKind's structure and none of the sweep navigation. The proof shows
two grammars compile against the surface; it does **not** show the surface serves
the flat-sweep, rich-kind navigation the disclaimer is about.

**What PB-3 actually proves:** one tree-shaped *data* grammar (JSON) fits the
neutral surface, and a degenerate scalar list also compiles. CSS declaration
values are also data-tree-shaped and will likely fit. The disclaimer's genuine
hard case — a flat-tape grammar kept "at full fidelity" — is untouched. The
verdict "YES, the disclaimer is answered" overreaches to "YES for tree-shaped
data; sheets unaddressed."

---

## 3. Codex over-claim beyond the prototype

**The −1,059 retirement includes sheets, but only JSON is proven or gated.**
The arithmetic checks exactly: `json/sink.rs`(120)+`json/view.rs`(488)+
`json/value.rs`(172)+`sheets/view.rs`(279) = 1059. Two problems:

1. **The "json/sheets/css" attribution (report line 137; SPEC-DRAFT:312) is
   loose.** CSS contributes ~16 LOC (`css_l4_declaration_values/sink.rs`; there
   is no css view/value in the set). The −1,059 is really json(780) + sheets(279).

2. **The close gate (SPEC-DRAFT:319-321) names ONLY the JSON corpus** —
   "fused-sink byte-identical to the frozen `JsonSink` on the JSON corpus;
   lazy-view cursor-walk identical to `json/view.rs`." There is **no sheets
   differential oracle**. Yet the retirement deletes `sheets/view.rs`. The
   retirement surface exceeds the proof surface.

3. **The consumer-invariant / liveness gate (SPEC-DRAFT:319) is violated for
   sheets.** `sheets_conformance.rs` is a LIVE consumer that reads `rule_kind`.
   Retiring `sheets/view.rs` behind a NodeClass surface that cannot carry
   `rule_kind` either breaks that test or forces it to lose kind granularity.
   Neither PB-3 nor the codex discloses this.

**Projection-totality invariant mismatch.** The codex invariant (SPEC-DRAFT:311)
is `PROJECTION_DIRECT_TO_STRUCT.len() == materializers == consumers` — a
count over direct-to-struct projections. PB-3 (lines 139-142) restates it as
"every sealed cursor must `classify` into `NodeClass` and every leaf `scalar_span`
into valid source bytes" — a *classify-totality* invariant. These are different
properties; the report silently swaps one for the other. TB4.W2 should carry
both, named distinctly.

---

## 4. What is genuinely de-risked (credit where due)

- The JSON tree lane (eager Sink, lazy View, Path, direct-to-struct) compiles as
  Rust signatures over the carried AoS tape with a single structural enum. That
  is real and valuable; TB4.W2's JSON parity is low-risk.
- The `Classify` two-method bridge is a clean seam **for grammars whose data is a
  keyed/indexed value tree**.
- The recommendation to fold `Diagnostic` into `ViewError` (report 142-143) is
  consistent with SEAM-PT-E1 (SPEC-DRAFT:317) and sound.

---

## 5. Blocking issues (must resolve before TB4.W2 is executable-tomorrow)

1. **Retire-vs-carry sheets is an open, wrongly-resolved design question.** The
   neutral surface cannot express the sheets `rule_kind` flat-sweep; the codex
   nonetheless retires `sheets/view.rs` in the −1,059 and gates only JSON.
2. **PB-3's "sheets disclaimer answered" verdict is a strawman** (FlatClass
   models no sheets structure) and has propagated into the codex retirement.
3. **The close gate's proof surface (JSON) is narrower than its retirement set
   (JSON+sheets).** No sheets differential oracle exists.

## 6. Refinements (wave-ownable, TB4.W2)

1. **Cleanest fix — mark `sheets/view.rs` CARRY, not retire.** The cursor.rs
   disclaimer already counsels this: flat-sweep + `rule_kind` navigation is
   grammar-specific "at full fidelity." Retirement drops to −780 (JSON-only),
   which makes the JSON-only close gate *consistent*. State the delineation
   honestly: the neutral surface serves keyed/indexed value trees (JSON, CSS
   values); flat-sweep rich-kind grammars keep their native view.
2. **Alternative — add a native-tag escape to `Classify`** (`fn tag(cursor)->u16`)
   so a sweep consumer recovers `rule_kind`. But this re-introduces per-grammar
   vocabulary at the seam, weakening the "no enum crosses" thesis, and REQUIRES a
   sheets differential oracle added to the close gate. Pick (1) unless a live
   sheets value-plane consumer beyond the conformance test is demonstrated.
3. **Make the JSON oracle mechanical.** Replace the hardcoded literal
   (main.rs:543) with an actual `json/view.rs::to_canonical_string` invocation on
   a shared corpus, including escaped/non-ASCII inputs (carry the
   `STRING_NEEDS_DECODE` flag-plane unescape, config.rs:5 / view.rs:206).
4. **Correct the report's factual slips** before it seeds the plan: "8-byte
   View" → 16 bytes; "character-for-character equals to_canonical_string" →
   transcription-matched literal; the "css" share of −1,059 → ~16 LOC.
5. **Name both projection invariants** (direct-to-struct count AND classify
   totality) distinctly in TB4.W2.
