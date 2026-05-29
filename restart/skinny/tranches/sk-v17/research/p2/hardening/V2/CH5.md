# CH5 HIDDEN-COUPLING — SK-V17 S-P2 RESEARCH V2

Lens: CH5 HIDDEN-COUPLING (V2).
Pass: S-P2 Research. Cycle: V2. Date: 2026-05-29.
Contract: PASS-2-RESEARCH §3 CH5 + ORCHESTRATOR §3W. Master HEAD `0ae1caa52`.
Subjects: `restart/skinny/tranches/sk-v17/research/p2/{p2a-sota-teardown,p2b-dav1d-process,p2c-arch-esoterica,p2d-substrate-tape,p2e-parse-that-gaps,p2f-grammar-neutral}.md` (V2 cycle).
Prior: `p2/hardening/V1/CH5.md` (32/34 ACCEPT, 2 REVISE: R-CH5-1 P2-D six-vs-four fields, R-CH5-2 P2-E G2 within-call depth invariant).

## §0 — Mandate

CH5 disposition per candidate/section: does any candidate introduce a PARALLEL
SUBSTRATE, a SIDECAR PRODUCER, a RENAMED SCANNER, a RETAINED CURSOR, an AUX
DENSITY TABLE, or a TRACK 1 == TRACK 2 dishonesty? Does P2-D conclude the tape +
structural projection are ONE substrate (Lock 1)? No-go surface fixed by
`SYNTHESIS.md` §0.4 (verbatim: "retained sidecars … sidecar event vectors,
retained cursor/list, cursor streams, aux density/projection tables,
parser-owned structural projections or streams, parallel source passes, second
tapes, public `UnionTape`, new substrate APIs … Track 1 == Track 2 sidecars"),
`LOCKS.md:75` ("if structural offsets are retained, the structural projection IS
the tape"), `LOCKS.md:585` (Lock 1 sidecar/second-tape/cross-call-classifier
clause).

## §1 — V1 fold verification (the two REVISEs cleared)

Both V1 orphan-REVISEs are folded into the V2 text and verified clean:

- **R-CH5-1 (P2-D `:33` six-vs-four fields) — CLEARED.** V2 P2-D §1.1 now reads
  "owns exactly SIX data members" and enumerates `source`/`offsets`/`flag_cursors`/
  `flag_values`/`payloads`/`id` with the CRITICAL Lock-1 observation that "of the
  six members, exactly ONE is a position-keyed vector indexed parallel to the
  structural stream — `offsets`" (`p2d-substrate-tape.md:37-53`). VERIFIED at
  source: `Tape<'input>` (`skinny/crates/runtime/src/tape/mod.rs:93-101`) carries
  exactly those six fields; only `offsets: Vec<u32>` is position-parallel;
  `flag_cursors`/`flag_values` are a SPARSE binary-searched pair (`flags_at`
  `:144-150`); `payloads` is a single growable arena (not position-indexed);
  `source`/`id` are scalars. The union proof is now accurate on the load-bearing
  struct AND strengthened (it explicitly negates a parallel positions/class/
  density vector). The fix did not weaken the conclusion.
- **R-CH5-2 (P2-E G2 within-call depth invariant) — CLEARED.** V2 P2-E §2-G2
  SHAPE now states inline: "The `depth_carry` is an i32 threaded WITHIN a single
  `scan_components_to_index` call ONLY: it is initialised to 0 at the start of
  each parse and discarded at end-of-input — there is NO cross-call depth
  retention … It is a within-chunk running balance, never a retained cursor"
  (`p2e-parse-that-gaps.md:150-154`). The Lock-1 within-chunk-carry invariant is
  no longer deferred to §4; it is asserted at the candidate shape. This
  forecloses any reading of the i32 depth as a retained-cursor-equivalent. Cross-
  checked against `LOCKS.md:75/585` (cross-call classifier state remains
  rejected) and `dispatch.rs:101-113` (the lo6 guard retains no cross-call
  state). CLEAN.

## §2 — Source verification (the load-bearing structural facts CH5 turns on)

Verified at source this cycle (`skinny/`, master HEAD `0ae1caa52`):

- `Tape<'input>` (`crates/runtime/src/tape/mod.rs:93-101`): six fields, ONE
  position-keyed vector (`offsets`). No second positions vector, no class column,
  no density table, no retained-cursor field. CONFIRMED: one substrate.
- `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` (`mod.rs:175-181`):
  `{ tape: &'doc Tape<'input>, cursor: u32 }` + three zero-size `PhantomData`. It
  is GENERIC over the grammar `G` — JSON and CSS instantiate the SAME cursor type
  over the SAME tape, differing only in node-kind projection. Exactly ONE cursor
  type; no `TapeCursor`/second cursor exists. CONFIRMED. (Forecloses the
  two-cursor / Track1==Track2 dishonesty at the substrate level.)
- `lo6_table_admissible` (`crates/bbnf-simd/src/dispatch.rs:101-113`): computes
  `slot = (byte & 0x3f)` at `:106` — a low-6-bit MASK, retains a local `seen`
  array within the single call, returns. No cross-call classifier state. CONFIRMED
  the carry-within-chunk invariant every NEON candidate cites is real.
- `SYNTHESIS.md` §0.4 no-go list names "Track 1 == Track 2 sidecars" explicitly as
  a rejected construct; P2-D's D6 is the exact mirror. CONFIRMED the no-go anchor
  is current.

P2-D's central Lock-1 conclusion (§1.4: "tape and structural projection are ONE
substrate … the offset `Vec<u32>` is simultaneously the scan output and the tape
backbone") is SOUND against source. No V2 artefact proposes a construct on the
no-go list. The V2 pool is clean on hidden coupling, and the two V1 precision
fixes strengthened (did not regress) the union proof.

## §3 — Per-candidate / per-section dispositions

### P2-D (substrate + tape — the load-bearing Lock-1 conclusion)

| Item | CH5 finding | Disposition |
|---|---|---|
| §1.4 Lock-1 verdict (ONE substrate) | The offset vector IS the tape; kind from source byte; lazy `ValueRef`; `PayloadArena` bounded escape only. No sidecar/parallel/second substrate. Load-bearing and correct. | **ACCEPT** |
| §1.1 six-field enumeration | Now correctly enumerates all SIX `Tape` fields with the "exactly ONE position-keyed vector" observation (`:37-53`). R-CH5-1 cleared; the union proof is accurate and strengthened. | **ACCEPT** |
| §1.5 two-plane framing | Keeps recognition (`track1_full_parse`) and typed (`track1_fact_stream`) planes honestly separate; explicit the two levers "touch the SAME substrate" sequentially, not as competing tracks (`:194-196`). No Track1==Track2 sidecar. | **ACCEPT** |
| D1 `push_plain_offset` emit | Append into the EXISTING `offsets: Vec<u32>`; output a sealed `Tape`, not a String; "generic over the tape sink" = codegen-monomorphised trait (§4 guard `:454-456`), NOT a runtime `Arena<G>` dispatch. No second substrate. | **ACCEPT** |
| D2 lazy `ValueRef` projection | Cursor reads over the sealed tape, kind from source byte, zero stored tag, `PayloadArena` for irreducible scalars only. Isomorphic to `value_from_ref`. No retained cursor, no parallel view. | **ACCEPT** |
| D3 O(1) checkpoint/truncate | `offsets.len()` capture + `truncate`; NO `split_off`, NO `Vec<Vec>` arena. Operates on the one offset vector + matching sparse-flag truncate. No parallel rollback substrate. | **ACCEPT** |
| D4 one-shot SIMD reserve | `CapacityPlan::OneShotSimd` sizes the EXISTING `offsets` from the scan count; cold-path `Vec::reserve`. No second vector. | **ACCEPT** |
| D5 sparse-flag side-table | Uses the EXISTING `flag_cursors`/`flag_values` pair (NOT a new vector, NOT a widened per-position record); paid only where non-zero. From the CH5 lens this is the tape's OWN sparse side-vectors, not a parallel substrate or aux density table. The semantic-overfit risk (flag bits becoming a relocated per-rule catalogue) is correctly delegated to CH2 by the artefact's GENERALISABLE-WITH-GUARD verdict. CH5-clean: adds no substrate. | **ACCEPT** (CH5; CH2 owns the semantic guard) |
| D6 (REJECT-on-sight no-go anchor) | Explicitly enumerates every no-go construct (`StructLayout`/`TapeStructBuilder`/`TapeCursor`, retained class column, sidecar event vector, aux density table, retained cursor/list, parallel source pass, `UnionTape`, cross-call classifier carry) and REJECTs under Lock 1 (`:414-423`). This is the CH5 anchor the lens asks for. | **ACCEPT** |

### P2-E (parse-that gaps)

| Item | CH5 finding | Disposition |
|---|---|---|
| G1 `comment_body_mask_64` | Produces a transient `u64` body mask + a 1-bit carry that threads within one block sequence; AND-NOTed into the one structural index. No retained sidecar. | **ACCEPT** |
| G2 `bracket_depth_mask_64` | Produces a transient interior mask + an i32 `depth_carry`. R-CH5-2 cleared: the §2-G2 SHAPE now states inline that `depth_carry` is init-0 per parse, threads ONLY within a single `scan_components_to_index` call, never read back across invocations (`:150-154`) — within-chunk carry, Lock-1-honest, never a retained cursor. The richer-than-1-bit carry is now fenced at the candidate, not only §4. | **ACCEPT** |
| G3 `scan_components_to_index` | Produces ONLY a `Vec<u32>` structural index (Lock 1, the projection IS the tape); §4 explicit "no retained cursor, no aux density table, no second scan" (`:299-303`). Per-grammar consumer, not a parallel substrate. | **ACCEPT** |
| G4 `parse_4_digits` checkasm gate | Test artefact only; decodes on demand via `ValueRef`, not eagerly. No substrate. (Orphan-gating is CH1's concern, not CH5.) | **ACCEPT** |
| G5 FNV/hex non-candidate | Recorded as a non-candidate; no substrate. | **ACCEPT** |

### P2-C (arch esoterica)

| Item | CH5 finding | Disposition |
|---|---|---|
| C1 lo6 TBL classify | Inadmissible for CSS (lo6 `& 0x3f` collision) → falls to C2; produces only a `Vec<u32>` index. §4 "No second substrate (Lock 1)" explicit (`:417-421`): transient producer not sidecar, no cross-call classifier retention. | **ACCEPT** |
| C2 eq-set fan classify (primary) | Produces a structural-candidate `Vec<u32>` index; the tape-consuming spine resolves context; "preserves the nested-aware semantics — it does not flatten them" (`:183-184`) — no Track1==Track2 collapse. The `vdupq_n_u8` per-member splat is explicitly NOT the §0.4 evidence-broadcast pre-block (`:191-194`). No retained mask. | **ACCEPT** |
| C3 shrn movemask consolidation | Bit-packing sub-task folded into C1/C2; no standalone substrate. | **ACCEPT** |
| C4 host CTZ first-match | Single `trailing_zeros` extract per mask, folded into C1/C2; the bulk-consumer form is REDRESS-89-flagged, NOT proposed. No retained bulk cursor. | **ACCEPT** |
| C5/C6 udot/i8mm | Orphan/net-new, inventory-only, explicitly not proposed; decode on demand if ever admitted. No substrate. (Orphan is CH1/CH4.) | **ACCEPT** |
| §3 REDRESS flags + §4 "No second substrate (Lock 1)" | Names transient-producer-not-sidecar and cross-call-classifier-state-REJECT (`:417-421`). asmjson PC-as-state (the closest external retained-class-column) is host-blocked and cited as the Lock-1/CH5 concern. | **ACCEPT** |

### P2-A (SOTA teardown)

| Item | CH5 finding | Disposition |
|---|---|---|
| CP-A1 byte-class classifier | Produces only a `Vec<u32>` index; §3 verdict "Produces only a `Vec<u32>` structural index (no JSON/CSS policy)"; §4 uses prefix-XOR/next-set-bit as transient producers, not a retained hot body or cross-call bulk consumer (`:442-445`). | **ACCEPT** |
| CP-A2 tape append | Single non-generic `TapeBuilder` sink; §4 explicit it re-opens nothing absent a `StructLayout`/`TapeStructBuilder`/`TapeCursor` second substrate (`:418-422`). Operates on the existing tape. | **ACCEPT** |
| CP-A3 lazy `ValueRef` rider | Reconstructs on demand over the EXISTING `Tape`/`ValueRef`; no eager tree, no second view substrate. | **ACCEPT** |
| CP-A4 tokenize-once shared-scan | Explicitly "Bounded to the single-substrate shape (the structural projection IS the tape, Lock 1)" and "no parser-local second cursor" (`:357-358,:366`). The candidate most exposed to a "second source scan" reading; the artefact pre-empts it: the index is consumed once, not re-scanned, over the one substrate. | **ACCEPT** |
| §1.0 / §1.1 honesty framing | §1.0 names the recognition plane "a masking probe" with 2–3× headroom and is explicit the deficit is materialization, NOT recognition — no claim the fast recognition number is the materializing number (that would be the Track1==Track2 dishonesty). §1.1 frames simdjson Stage 1 as a transient structural projection consumed by ONE builder (NOT a retained class column; Lock 1 / CH5 cited verbatim `:63-65`). | **ACCEPT** |

### P2-B (dav1d process)

| Item | CH5 finding | Disposition |
|---|---|---|
| C-B1 byte_class scan | §4 explicit (`:244-249`): "must not become a parallel substrate (Lock 1 / CH5) … must NOT retain a sidecar event vector, a density table, or cross-call classifier state … The structural projection it feeds IS the tape, not a second scan." Directly addresses every CH5 vector. | **ACCEPT** |
| C-B2 push_plain_offset | §4 "must not re-open … fact-stream-as-retained-sidecar (Lock 1 FactStream clause `LOCKS.md:585`)" (`:256-258`) + single non-generic `TapeBuilder`. No second substrate, no Track1==Track2 sidecar. | **ACCEPT** |
| C-B0 admission process / C-B3 orphan | Process gate (G1–G6); orphan rejection. No substrate proposed. | **ACCEPT** |

### P2-F (grammar-neutral)

| Item | CH5 finding | Disposition |
|---|---|---|
| §1.1 `ValueRef<G>` generic-over-grammar framing | Verified against source (`mod.rs:175`): ONE generic cursor; JSON and CSS are instantiations, NOT two cursor types (`:31-38`). Forecloses the Track1==Track2 / two-cursor dishonesty at the substrate level. | **ACCEPT** |
| CF-1 tape-append + `ValueRef` projection | "No new cursor/builder type" (`:144`); §4 item 6 "No `StructLayout`/`TapeStructBuilder`/`TapeCursor` … No sidecar event vector, no retained cursor, no parallel source pass (Lock 1, `LOCKS.md:585`)" (`:362-365`). | **ACCEPT** |
| CF-2 structural-membership classifier | Produces a `Vec<u32>` index the CF-1 tape consumes; §4 item 8 "Cross-call classifier-state retention REJECT … carry stays within a single chunk-call. A SIMD mask stream is a transient producer" (`:368-370`). | **ACCEPT** |
| CF-3 commit-by-construction Alt-mode | Codegen control-flow property; §4 item 3 "No type-ambivalent dual representation (Lock 1, tape vs OpenFrame vs direct-to-struct competing)" (`:350-352`). No substrate. | **ACCEPT** |
| CF-4a/CF-4b digit kernels | Decode on demand; §4 item 6 second-substrate guard applies; no substrate. (Orphan is CH1.) | **ACCEPT** |
| §4 item 6 second-substrate / item 8 cross-call carry | Both no-go vectors explicitly fenced. | **ACCEPT** |

## §4 — Track 1 == Track 2 honesty (the specific dishonesty CH5 hunts)

The dishonesty CH5 must catch is a candidate claiming Track 1 (recognition) and
Track 2 (typed/materialised) share a substrate while secretly running two scans,
two cursors, or a sidecar bridging them. No V2 artefact does this:

- P2-D §1.5 keeps the two PLANES honestly separate, explicit the recognition
  plane "materialises nothing — it fails preserve-rich-ast and is a masking probe,
  NOT the subject" (`:180-181`); the tape lever attacks the typed plane and the
  NEON scan attacks the surviving recognition cost on the SAME substrate,
  SEQUENTIALLY, not as two competing tracks (`:194-196`). No Track1==Track2
  sidecar.
- P2-A §1.0 names the recognition plane "a masking probe" with 2–3× headroom and
  is explicit the deficit is materialization, not recognition (`:19-27,:41-47`) —
  no claim that the fast recognition number is the materializing number. The
  fair-bar discipline (lightningcss strict materializing bar / cssparser
  flaw-probe) is held throughout (§1.5/§1.6).
- P2-F §1.1 confirms the ONE generic cursor (`ValueRef<G>`): JSON and CSS are
  instantiations of the same type, not two cursors — the structural foreclosure of
  the two-cursor dishonesty.
- No candidate proposes a sidecar that BOTH the recognition and the typed plane
  read — the one `offsets` vector serves both, by construction (P2-D §1.4).

CH5 finds NO Track 1 == Track 2 dishonesty in V2.

## §5 — Disposition counts

- Sections/candidates dispositioned: **35** (P2-D: 10; P2-E: 5; P2-C: 6; P2-A: 5;
  P2-B: 3; P2-F: 6 — per row in §3, with the honesty review folded into the
  relevant artefact rows).
- **ACCEPT: 35**
- **REVISE: 0**
- **REJECT: 0**

ACCEPT rate (CH5 lens): **35/35 = 100%**. The two V1 REVISEs (R-CH5-1 six-field
enumeration, R-CH5-2 G2 within-call depth invariant) are folded into the V2 text
and verified clean at source; both were precision fixes that strengthened, never
weakened, the Lock-1 union proof. No V2 candidate introduces a parallel substrate,
a sidecar producer, a renamed scanner, a retained cursor, an aux density table, or
a Track1==Track2 dishonesty. P2-D's substrate-union conclusion (tape + structural
projection are ONE substrate, the single `offsets` vector is simultaneously the
scan output and the tape backbone) holds and is the load-bearing Lock-1 verdict
the lens confirms.

## §6 — Orphan-REVISE foldback

NONE. Zero open REVISE, zero REJECT, zero orphan. CH5 has no open critical defect.
The V2 pool converges on the Lock-1 substrate-union verdict at 100% CH5 ACCEPT
(prior cycle 94.1%); the convergence-criterion's "zero orphan unresolved REVISE"
condition is met for this lens.

## §7 — Sources (CH5-load-bearing facts verified this cycle)

- `skinny/crates/runtime/src/tape/mod.rs:93-101` (`Tape` six fields, one position-
  keyed `offsets` vector), `:144-150` (`flags_at` sparse binary search), `:175-181`
  (`ValueRef<G: EventGrammar>` one generic cursor type).
- `skinny/crates/bbnf-simd/src/dispatch.rs:101-113` (`lo6_table_admissible`,
  `slot = byte & 0x3f` at `:106`, local `seen` array, no cross-call state).
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.4 (no-go list incl. "Track 1 ==
  Track 2 sidecars"); `restart/locks/LOCKS.md:75` (structural-projection-IS-the-tape),
  `:585` (Lock 1 sidecar/second-tape/cross-call clause), `:141-149` (v+1
  carry-within-chunk).
- V2 P2 artefacts: `p2d-substrate-tape.md` (§1.1 `:37-53`, §1.4 `:128-169`, §1.5
  `:171-196`, D6 `:414-423`, §4 `:444-484`); `p2e-parse-that-gaps.md` (G2 SHAPE
  `:150-154`, G3 §4 `:299-303`); `p2c-arch-esoterica.md` (§4 `:388-421`);
  `p2a-sota-teardown.md` (§1.0 `:12-47`, §1.1 `:63-65`, CP-A4 `:350-376`, §4
  `:411-462`); `p2b-dav1d-process.md` (§4 `:244-264`); `p2f-grammar-neutral.md`
  (§1.1 `:31-38`, §4 items 6/8 `:362-370`).
- Prior: `p2/hardening/V1/CH5.md` (R-CH5-1, R-CH5-2 the folded REVISEs).
- Host: Apple M5 Max, aarch64-apple-darwin. Master HEAD `0ae1caa52`.
