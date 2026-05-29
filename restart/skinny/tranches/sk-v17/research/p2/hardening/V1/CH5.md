# CH5 HIDDEN-COUPLING — SK-V17 S-P2 RESEARCH V1

Lens: CH5 HIDDEN-COUPLING (V1).
Pass: S-P2 Research. Cycle: V1. Date: 2026-05-29.
Contract: PASS-2-RESEARCH §3 CH5 + ORCHESTRATOR §3W. Master HEAD `0ae1caa52`.
Subjects: `restart/skinny/tranches/sk-v17/research/p2/{p2a-sota-teardown,p2b-dav1d-process,p2c-arch-esoterica,p2d-substrate-tape,p2e-parse-that-gaps,p2f-grammar-neutral}.md`.

## §0 — Mandate

CH5 disposition per candidate/section: does any candidate introduce a PARALLEL
SUBSTRATE, a SIDECAR PRODUCER, a RENAMED SCANNER, a RETAINED CURSOR, an AUX
DENSITY TABLE, or a TRACK 1 == TRACK 2 dishonesty? Does P2-D conclude the
tape + structural projection are ONE substrate (Lock 1)? The no-go surface is
fixed by `SYNTHESIS.md` §0.4 (verbatim list: "retained sidecars, retained
sidecar tables, sidecar event vectors, retained cursor/list, cursor streams,
aux density/projection tables, parser-owned structural projections or streams,
parallel source passes, second tapes, public `UnionTape`, new substrate APIs,
… Track 1 == Track 2 sidecars") and `LOCKS.md:75` ("if structural offsets are
retained, the structural projection IS the tape") + `LOCKS.md:585` (Lock 1
sidecar/second-tape/cross-call-classifier clause).

## §1 — Source verification (the load-bearing structural facts CH5 turns on)

Verified at source this cycle (`skinny/crates/runtime/src/tape/mod.rs`):

- `Tape<'input>` (`mod.rs:93`) carries `source: &'input [u8]`, `offsets: Vec<u32>`,
  `flag_cursors: Vec<u32>`, `flag_values: Vec<u8>`, `payloads: PayloadArena`,
  `id: TapeId` — ONE offset vector + the sparse flag pair + the payload arena +
  a borrowed source + an id. There is no second positions vector, no class
  column, no density table, no retained cursor field. CONFIRMED: one substrate.
- `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` (`mod.rs:175`) is
  `{ tape: &'doc Tape<'input>, cursor: u32 }` + three zero-size `PhantomData`.
  It is GENERIC over the grammar `G` — JSON and CSS instantiate the SAME cursor
  type over the SAME tape, differing only in the node-kind projection. There is
  exactly ONE cursor type; no `TapeCursor`/second cursor exists. CONFIRMED.
- `DocumentView` (`mod.rs:227`) is a trait over the same tape (`source`,
  `tape_id`, `root_value`); not a second document model. CONFIRMED.
- `SYNTHESIS.md` §0.4 no-go list reproduced verbatim above; it names "Track 1 ==
  Track 2 sidecars" explicitly as a rejected construct. P2-D's D6 is the exact
  mirror of this list. CONFIRMED the no-go anchor is real and current.

P2-D's central Lock-1 conclusion (§1.4: "tape and structural projection are ONE
substrate … the offset `Vec<u32>` is simultaneously the scan output and the tape
backbone") is SOUND against source. No artefact proposes a construct on the no-go
list. The CH5 lens finds the V1 candidate pool unusually clean on hidden coupling.

## §2 — Per-candidate / per-section dispositions

### P2-D (substrate + tape — the load-bearing Lock-1 conclusion)

| Item | CH5 finding | Disposition |
|---|---|---|
| §1.4 Lock-1 verdict (ONE substrate) | The conclusion is correct and load-bearing; the offset vector IS the tape, kind from source byte, lazy `ValueRef`, `PayloadArena` bounded escape only. No sidecar/parallel/second substrate. | **ACCEPT** |
| §1.1 "four data members" | The `Tape` struct has SIX fields (`source`, `offsets`, `flag_cursors`, `flag_values`, `payloads`, `id`); the artefact says "exactly four data members." Not a hidden-coupling defect, but the substrate-union proof's structural enumeration is inaccurate on the load-bearing struct. Fix at `p2d-substrate-tape.md:33`: enumerate all six (offsets + sparse flag pair + payloads + borrowed source + id), confirming none is a parallel positions/class/density vector. | **REVISE** (accuracy of the load-bearing claim) |
| D1 `push_plain_offset` emit | Append into the EXISTING `offsets: Vec<u32>`; output a sealed `Tape`, not a String; "generic over the tape sink" = codegen-monomorphised trait, NOT a runtime `Arena<G>` dispatch (§4 guard explicit). No second substrate. | **ACCEPT** |
| D2 lazy `ValueRef` projection | Cursor reads over the sealed tape, kind from source byte, zero stored tag, `PayloadArena` for irreducible scalars only. Isomorphic to `value_from_ref`. No retained cursor, no parallel view. | **ACCEPT** |
| D3 O(1) checkpoint/truncate | `offsets.len()` capture + `truncate`; NO `split_off`, NO `Vec<Vec>` arena. Operates on the one offset vector. No parallel rollback substrate. | **ACCEPT** |
| D4 one-shot SIMD reserve | `CapacityPlan::OneShotSimd` sizes the EXISTING `offsets` from the scan count; cold-path `Vec::reserve`. No second vector. | **ACCEPT** |
| D5 sparse-flag side-table | Uses the EXISTING `flag_cursors`/`flag_values` pair (NOT a new vector, NOT a widened per-position record). From the CH5 (hidden-coupling) lens this is NOT a parallel substrate or aux density table — it is the tape's own sparse side-vectors, paid only where non-zero. The SEMANTIC overfit risk (flag bits becoming a relocated per-rule catalogue / `W5C_REQUEST_FACT_PROFILES` in flag form) is correctly delegated to CH2 by the artefact's own GENERALISABLE-WITH-GUARD verdict. CH5-clean: it adds no substrate. | **ACCEPT** (CH5; CH2 owns the semantic guard) |
| D6 (REJECT-on-sight no-go anchor) | Explicitly enumerates every no-go construct (`StructLayout`/`TapeStructBuilder`/`TapeCursor`, retained class column, sidecar event vector, aux density table, retained cursor/list, parallel source pass, `UnionTape`, cross-call classifier carry) and REJECTs it under Lock 1. This is the CH5 anchor the lens asks for. | **ACCEPT** |

### P2-E (parse-that gaps)

| Item | CH5 finding | Disposition |
|---|---|---|
| G1 `comment_body_mask_64` | Produces a transient `u64` body mask + 1-bit carry; the carry threads within one block sequence. AND-NOTed into the delimiter mask; emits into the one structural index. No retained sidecar. | **ACCEPT** |
| G2 `bracket_depth_mask_64` | Produces a transient interior mask + an `i32 depth` carry threaded across blocks within one scan call. §4 asserts "carry/depth threads WITHIN a single `scan_components_to_index` call, reset per parse" — Lock-1-honest (within-chunk carry is admitted at LOCKS v+1). BUT the depth is richer carried state than JSON's 1-bit string carry; the artefact does not state at the §2-G2 SHAPE that `depth_carry` is initialised to 0 at parse entry and is never read back across separate `scan_components_to_index` invocations. To foreclose any reading of the depth as a retained-cursor-equivalent (a cross-call carry would be a CH5 violation), the §2-G2 shape must name the reset-per-parse / no-cross-call invariant inline (it currently lives only in §4). Fix at `p2e-parse-that-gaps.md:132-149`. | **REVISE** (state the within-call/reset-per-parse invariant at the candidate shape, not only §4) |
| G3 `scan_components_to_index` | Produces ONLY a `Vec<u32>` structural index (Lock 1, the projection IS the tape); §4 explicit "no retained cursor, no aux density table, no second scan." Per-grammar consumer, not a parallel substrate. | **ACCEPT** |
| G4 `parse_4_digits` checkasm gate | Test artefact only; decodes on demand via `ValueRef`, not eagerly. No substrate. (Orphan-gated — CH1's concern, not CH5.) | **ACCEPT** |
| G5 FNV/hex non-candidate | Recorded as a non-candidate; no substrate. | **ACCEPT** |

### P2-C (arch esoterica)

| Item | CH5 finding | Disposition |
|---|---|---|
| C1 lo6 TBL classify | Inadmissible for CSS (lo6 collision) → falls to C2; produces only a `Vec<u32>` index. §4 "No second substrate" clause explicit: every NEON candidate produces ONLY a `Vec<u32>` index, transient producer not sidecar, no cross-call classifier retention. | **ACCEPT** |
| C2 eq-set fan classify (primary) | Produces a structural-candidate `Vec<u32>` index; the tape-consuming spine resolves context. "Preserves the nested-aware semantics — it does not flatten them" — no Track1==Track2 collapse. The `vdupq_n_u8` per-member broadcast is explicitly noted as NOT the REDRESS-blocked runtime-broadcast pattern. No retained mask. | **ACCEPT** |
| C3 shrn movemask consolidation | Bit-packing sub-task folded into C1/C2; no standalone substrate. | **ACCEPT** |
| C4 host CTZ first-match | Single `trailing_zeros` extract per mask, folded into C1/C2; the bulk-consumer form is REDRESS-89-flagged, NOT proposed. No retained bulk cursor. | **ACCEPT** |
| C5/C6 udot/i8mm | Orphan/net-new, inventory-only, explicitly not proposed; decode on demand if ever admitted. No substrate. (Orphan is CH1/CH4.) | **ACCEPT** |
| §3 REDRESS-block flags + §4 "No second substrate (Lock 1)" | Correctly names transient-producer-not-sidecar and cross-call-classifier-state-REJECT. The asmjson PC-as-state route (the closest external thing to a retained class column) is host-blocked and cited as the Lock-1/CH5 concern in P2-A §1.1. | **ACCEPT** |

### P2-A (SOTA teardown)

| Item | CH5 finding | Disposition |
|---|---|---|
| CP-A1 byte-class classifier | Produces only a `Vec<u32>` index; §3 verdict "Produces only a `Vec<u32>` structural index (no JSON/CSS policy)"; §4 prefix-XOR/next-set-bit used as transient producers, not retained hot body or cross-call bulk consumer. | **ACCEPT** |
| CP-A2 tape append | Single non-generic `TapeBuilder` sink; §4 explicit it re-opens nothing if it does NOT introduce a `StructLayout`/`TapeStructBuilder`/`TapeCursor` second substrate. Operates on the existing tape. | **ACCEPT** |
| CP-A3 lazy `ValueRef` rider | Reconstructs on demand over the EXISTING `Tape`/`ValueRef`; no eager tree, no second view substrate. | **ACCEPT** |
| CP-A4 tokenize-once shared-scan | Explicitly "Bounded to the single-substrate shape (the structural projection IS the tape, Lock 1)" and "no parser-local second cursor." This is the candidate most exposed to a "second source scan" reading, and the artefact pre-empts it correctly: the index is consumed once, not re-scanned, and the reuse is over the one substrate. | **ACCEPT** |
| §1.1 simdjson Stage-1 / asmjson PC-as-state | Correctly frames Stage 1 as a transient structural projection consumed by ONE builder (NOT a retained class column; Lock 1 / CH5 cited verbatim), and asmjson PC-as-state as host-unreachable. No candidate derives a retained class column. | **ACCEPT** |

### P2-B (dav1d process)

| Item | CH5 finding | Disposition |
|---|---|---|
| C-B1 byte_class scan | §4 explicit: "must not become a parallel substrate (Lock 1 / CH5) … must NOT retain a sidecar event vector, a density table, or cross-call classifier state … The structural projection it feeds IS the tape, not a second scan." Directly addresses every CH5 vector. | **ACCEPT** |
| C-B2 push_plain_offset | §4 "must not re-open … fact-stream-as-retained-sidecar (Lock 1 FactStream clause `LOCKS.md:585`)" + single non-generic `TapeBuilder`. No second substrate, no Track1==Track2 sidecar. | **ACCEPT** |
| C-B0 admission process / C-B3 orphan | Process gate (G1–G6) and orphan rejection; no substrate proposed. | **ACCEPT** |

### P2-F (grammar-neutral)

| Item | CH5 finding | Disposition |
|---|---|---|
| CF-1 tape-append + `ValueRef` projection | "No new cursor/builder type"; `ValueRef<G: EventGrammar>` is the existing generic cursor; §4 item 6 "No `StructLayout`/`TapeStructBuilder`/`TapeCursor` … No sidecar event vector, no retained cursor, no parallel source pass (Lock 1, `LOCKS.md:585`)." | **ACCEPT** |
| CF-2 structural-membership classifier | Produces a `Vec<u32>` index the CF-1 tape consumes; §4 item 8 "Cross-call classifier-state retention REJECT … carry stays within a single chunk-call. A SIMD mask stream is a transient producer." | **ACCEPT** |
| CF-3 commit-by-construction Alt-mode | Codegen control-flow property; §4 item 3 "No type-ambivalent dual representation (Lock 1, tape vs OpenFrame vs direct-to-struct competing)." No substrate. | **ACCEPT** |
| CF-4a/CF-4b digit kernels | Decode on demand; §4 item 6 second-substrate guard applies; no substrate. (Orphan is CH1.) | **ACCEPT** |
| §4 item 6 second-substrate / item 8 cross-call carry | Both no-go vectors explicitly fenced. | **ACCEPT** |
| §1.1 `ValueRef<G>` generic-over-grammar framing | Verified against source (`mod.rs:175`): ONE generic cursor, JSON and CSS are instantiations, NOT two cursor types. This forecloses the Track1==Track2 / two-cursor dishonesty at the substrate level. | **ACCEPT** |

## §3 — Track 1 == Track 2 honesty (the specific dishonesty CH5 hunts)

The dishonesty CH5 must catch is a candidate that claims Track 1 (recognition)
and Track 2 (typed/materialised) share a substrate while secretly running two
scans, two cursors, or a sidecar bridging them. No artefact does this:

- P2-D §1.5 keeps the two PLANES (recognition `track1_full_parse` vs typed
  `track1_fact_stream`) honestly separate, and is explicit that the recognition
  plane "materialises nothing — it fails preserve-rich-ast and is a masking
  probe, NOT the subject." The tape lever attacks the typed plane; the NEON scan
  attacks the surviving recognition cost on the SAME substrate, SEQUENTIALLY, not
  as two competing tracks. No Track1==Track2 sidecar.
- P2-A §1.0 names the recognition plane "a masking probe" with 2–3.6× headroom
  and is explicit the deficit is materialisation, not recognition — no claim that
  the fast recognition number is the materialising number (that would be the
  Track1==Track2 dishonesty). The fair-bar discipline (lightningcss strict /
  cssparser flaw-probe) is held throughout.
- No candidate proposes a sidecar that both the recognition and the typed plane
  read — the one offset vector serves both, by construction.

CH5 finds NO Track 1 == Track 2 dishonesty in V1.

## §4 — Disposition counts

- Sections/candidates dispositioned: **34** (P2-D: 8; P2-E: 6; P2-C: 7; P2-A: 6;
  P2-B: 4; P2-F: 7 — counting per row in §2 plus the cross-cutting honesty review
  folded into the relevant artefact rows).
- **ACCEPT: 32**
- **REVISE: 2** (P2-D §1.1 six-vs-four data-member enumeration; P2-E G2 within-call/
  reset-per-parse depth-carry invariant stated at candidate shape)
- **REJECT: 0**

ACCEPT rate (CH5 lens): 32/34 = **94.1%**. The two REVISE items are precision
fixes, not coupling violations — neither candidate introduces a parallel
substrate, sidecar, renamed scanner, retained cursor, aux density table, or
Track1==Track2 dishonesty. Both REVISEs are folded with concrete path:line fixes
below; neither blocks the Lock-1 verdict, which CH5 confirms holds.

## §5 — Orphan-REVISE foldback (concrete fixes for V2)

- **R-CH5-1 (P2-D, `p2d-substrate-tape.md:33`):** change "owns exactly four data
  members" to enumerate all six `Tape` fields (`source`, `offsets`,
  `flag_cursors`, `flag_values`, `payloads`, `id`), confirming none is a parallel
  positions/class/density vector and that `id: TapeId` + the borrowed `source`
  are not a second substrate. Strengthens, not weakens, the §1.4 union proof.
- **R-CH5-2 (P2-E, `p2e-parse-that-gaps.md:132-149`, G2 SHAPE):** add to the §2-G2
  shape the invariant currently only in §4 — `depth_carry` is initialised to 0 at
  `scan_components_to_index` entry, threads ONLY within that single call, and is
  never read back across separate invocations (no cross-call depth retention).
  This forecloses any reading of the i32 depth as a retained-cursor-equivalent and
  makes the Lock-1 within-chunk-carry posture explicit at the candidate, not just
  the risk ledger.

Both fixes are mechanical and orphan-free (each names its target line + the
substituted text). CH5 has no open critical defect and no candidate REJECT.
