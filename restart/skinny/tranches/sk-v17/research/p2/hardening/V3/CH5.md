# CH5 HIDDEN-COUPLING — SK-V17 S-P2 RESEARCH V3

Lens: CH5 HIDDEN-COUPLING (V3).
Pass: S-P2 Research. Cycle: V3. Date: 2026-05-29.
Contract: PASS-2-RESEARCH §3 CH5 + ORCHESTRATOR §3W. Master HEAD `0ae1caa52`.
Subjects: `restart/skinny/tranches/sk-v17/research/p2/{p2a-sota-teardown,p2b-dav1d-process,p2c-arch-esoterica,p2d-substrate-tape,p2e-parse-that-gaps,p2f-grammar-neutral}.md` (V3 cycle).
Prior: `p2/hardening/V2/CH5.md` (35/35 ACCEPT, 0 REVISE, 0 REJECT); `p2/hardening/V1/CH5.md` (32/34 ACCEPT, 2 REVISE — R-CH5-1 six-vs-four fields, R-CH5-2 G2 within-call depth invariant — both folded into V2 and verified clean).

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
clause), `LOCKS.md:141-149` (v+1 carry-within-chunk).

## §1 — Carry-forward verification (V2 dispositions, second consecutive cycle)

V2 CH5 returned 35/35 ACCEPT with both V1 orphan-REVISEs folded and verified at
source. This V3 cycle re-runs the lens against the same pool (P2-D's frontmatter
is dated V3; the load-bearing §1.1/§1.4/§1.5 prose and the D1–D6 candidate set
are content-stable from the V2-reviewed text). The convergence test the lens
serves is "≥95% ACCEPT for two consecutive cycles, zero orphan REVISE" — V3
re-confirms the V2 verdict independently against source rather than inheriting
it on assertion.

The two folded REVISEs remain CLEARED and CLEAN:

- **R-CH5-1 (P2-D six-field enumeration) — STILL CLEAR.** P2-D §1.1 reads "owns
  exactly SIX data members" and enumerates `source`/`offsets`/`flag_cursors`/
  `flag_values`/`payloads`/`id` with the CRITICAL Lock-1 observation "of the six
  members, exactly ONE is a position-keyed vector indexed parallel to the
  structural stream — `offsets`" (`p2d-substrate-tape.md:37-53`). RE-VERIFIED at
  source this cycle: `Tape<'input>` (`skinny/crates/runtime/src/tape/mod.rs:94-101`)
  carries EXACTLY `source`, `offsets: Vec<u32>`, `flag_cursors: Vec<u32>`,
  `flag_values: Vec<u8>`, `payloads: PayloadArena`, `id: TapeId`. Only `offsets`
  is position-parallel; `flag_cursors`/`flag_values` are the SPARSE binary-search
  pair (`flags_at` `:144-150`, `binary_search(&cursor)` confirmed); `payloads` is
  one growable arena; `source`/`id` are scalars. Accurate AND it explicitly
  negates a parallel positions/class/density vector.
- **R-CH5-2 (P2-E G2 within-call depth invariant) — STILL CLEAR.** P2-E §2-G2
  SHAPE states inline that `depth_carry` is "an i32 threaded WITHIN a single
  `scan_components_to_index` call ONLY … initialised to 0 at the start of each
  parse and discarded at end-of-input — there is NO cross-call depth retention …
  a within-chunk running balance, never a retained cursor"
  (`p2e-parse-that-gaps.md:150-154`). RE-VERIFIED against `LOCKS.md:141-149`
  (carry-within-chunk) and `dispatch.rs:101-113` (`lo6_table_admissible` uses a
  local `seen` array, `slot = byte & 0x3f` at `:106`, returns no cross-call
  state). The richer-than-1-bit carry is fenced at the candidate shape, not only
  in §4.

## §2 — Source verification (the load-bearing structural facts CH5 turns on)

Re-verified at source this cycle (`skinny/`, master HEAD `0ae1caa52`):

- `Tape<'input>` (`crates/runtime/src/tape/mod.rs:94-101`): six fields, ONE
  position-keyed vector (`offsets: Vec<u32>`). No second positions vector, no
  class column, no density table, no retained-cursor field. CONFIRMED: one
  substrate.
- `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` (`mod.rs:175-181`):
  `{ tape: &'doc Tape<'input>, cursor: u32 }` + three zero-size `PhantomData`,
  `Copy`. GENERIC over the grammar `G` — JSON and CSS instantiate the SAME cursor
  type over the SAME tape, differing only in node-kind projection. Exactly ONE
  cursor type; no `TapeCursor`/second cursor exists. CONFIRMED (forecloses the
  two-cursor / Track1==Track2 dishonesty at the substrate level).
- `flags_at` (`mod.rs:144-150`): `flag_cursors.binary_search(&cursor)` then
  `flag_values.get(index)` — a sparse pair, not a dense parallel column.
  CONFIRMED the flag side-table is not an aux density table.
- `lo6_table_admissible` (`crates/bbnf-simd/src/dispatch.rs:101-113`): local
  `seen` array within the single call, `slot = (byte & 0x3f)` at `:106` (a
  low-6-bit MASK, not modulo), returns. No cross-call classifier state. CONFIRMED
  the carry-within-chunk invariant every NEON candidate cites is real.
- `SYNTHESIS.md` §0.4 names "Track 1 == Track 2 sidecars" explicitly as a
  rejected construct; P2-D's D6 is the exact mirror. CONFIRMED the no-go anchor
  is current.

P2-D's central Lock-1 conclusion (§1.4 `:128-169`: "tape and structural
projection are ONE substrate in the benched skinny tree. The offset `Vec<u32>`
is simultaneously the scan output and the tape backbone; the node kind is
recovered from the source byte at the offset; typed values are lazy `ValueRef`
views; the `PayloadArena` is the bounded escape hatch for irreducible scalars
only") is SOUND against source. No V3 artefact proposes a construct on the no-go
list. The keyword sweep across all five non-D artefacts (`sidecar`, `parallel
substrate`, `second substrate`, `retained cursor`, `aux density`, `class
column`, `StructLayout`/`TapeStructBuilder`/`TapeCursor`, `UnionTape`, `event
vector`, `cross-call`, `second scan`, `Track1==Track2`) returns ONLY the
explicit fencing clauses (each candidate's §4 REJECT-anchor), never a proposal.

## §3 — Per-candidate / per-section dispositions

### P2-D (substrate + tape — the load-bearing Lock-1 conclusion)

| Item | CH5 finding | Disposition |
|---|---|---|
| §1.4 Lock-1 verdict (ONE substrate) | The offset vector IS the tape; kind from source byte; lazy `ValueRef`; `PayloadArena` bounded escape only (`:128-169`). Source-verified. No sidecar/parallel/second substrate. Load-bearing and correct. | **ACCEPT** |
| §1.1 six-field enumeration | All SIX `Tape` fields enumerated with the "exactly ONE position-keyed vector" observation (`:37-53`); matches `mod.rs:94-101` exactly. Union proof accurate and strengthened. | **ACCEPT** |
| §1.5 two-plane framing | Recognition (`track1_full_parse`) and typed (`track1_fact_stream`) planes kept honestly separate; the two levers "touch the SAME substrate" SEQUENTIALLY, not as competing tracks (`:194-196`). No Track1==Track2 sidecar. | **ACCEPT** |
| D1 `push_plain_offset` emit | Append into the EXISTING `offsets: Vec<u32>`; output a sealed `Tape`, not a String; "generic over the tape sink" = codegen-monomorphised trait (§4 guard), NOT a runtime `Arena<G>` dispatch. No second substrate. | **ACCEPT** |
| D2 lazy `ValueRef` projection | Cursor reads over the sealed tape, kind from source byte, zero stored tag, `PayloadArena` for irreducible scalars only. Isomorphic to `value_from_ref`. No retained cursor, no parallel view. | **ACCEPT** |
| D3 O(1) checkpoint/truncate | `offsets.len()` capture + `truncate`; NO `split_off`, NO `Vec<Vec>` arena. Operates on the one offset vector + matching sparse-flag truncate. No parallel rollback substrate. (Lever-status is CONDITIONAL per the artefact's own re-profile obligation — CH1's concern, not CH5.) | **ACCEPT** |
| D4 one-shot SIMD reserve | `CapacityPlan::OneShotSimd` sizes the EXISTING `offsets` from the scan count; cold-path `Vec::reserve`. No second vector. | **ACCEPT** |
| D5 sparse-flag side-table | Uses the EXISTING `flag_cursors`/`flag_values` sparse pair (NOT a new vector, NOT a widened per-position record, NOT a dense column); paid only where non-zero. From the CH5 lens this is the tape's OWN sparse side-vectors, not a parallel substrate or aux density table. The semantic-overfit risk (flag bits becoming a relocated per-rule catalogue) is correctly delegated to CH2 by the GENERALISABLE-WITH-GUARD verdict. CH5-clean: adds no substrate. | **ACCEPT** (CH5; CH2 owns the semantic guard) |
| D6 (REJECT-on-sight no-go anchor) | Explicitly enumerates every no-go construct (`StructLayout`/`TapeStructBuilder`/`TapeCursor`, retained class column, sidecar event vector, aux density table, retained cursor/list, parallel source pass, public `UnionTape`, cross-call classifier carry) and REJECTs under Lock 1 (`:414-423`). This is the CH5 anchor the lens asks for. | **ACCEPT** |

### P2-E (parse-that gaps)

| Item | CH5 finding | Disposition |
|---|---|---|
| G1 `comment_body_mask_64` | Transient `u64` body mask + a 1-bit carry threading within one block sequence; AND-NOTed into the one structural index. No retained sidecar. | **ACCEPT** |
| G2 `bracket_depth_mask_64` | Transient interior mask + an i32 `depth_carry` fenced at the candidate shape as init-0-per-parse, within-call-only, never read back across invocations (`:150-154`). Within-chunk carry, Lock-1-honest, never a retained cursor. | **ACCEPT** |
| G3 `scan_components_to_index` | Produces ONLY a `Vec<u32>` structural index (the projection IS the tape); §4 explicit "No retained cursor, no aux density table, no second scan … No `UnionTape`, no second substrate, no parser-owned projection, no cross-call classifier-state retention" (`:294-303`). | **ACCEPT** |
| G4 `parse_4_digits` checkasm gate | Test artefact; decodes on demand via `ValueRef`, not eagerly. No substrate. (Orphan-gating is CH1.) | **ACCEPT** |
| G5 FNV/hex non-candidate | Recorded as a non-candidate; no substrate. | **ACCEPT** |

### P2-C (arch esoterica)

| Item | CH5 finding | Disposition |
|---|---|---|
| C1 lo6 TBL classify | Inadmissible for CSS (lo6 `& 0x3f` collision) → falls to C2; produces only a `Vec<u32>` index. §4 "No second substrate (Lock 1)" explicit (`:417-421`): transient producer not sidecar, no cross-call classifier retention. | **ACCEPT** |
| C2 eq-set fan classify (primary) | Produces a structural-candidate `Vec<u32>` index; the tape-consuming spine resolves context; preserves nested-aware semantics (does not flatten them) — no Track1==Track2 collapse. The `vdupq_n_u8` per-member splat is explicitly NOT the §0.4 evidence-broadcast pre-block. No retained mask. | **ACCEPT** |
| C3 shrn movemask consolidation | Bit-packing sub-task folded into C1/C2; no standalone substrate. | **ACCEPT** |
| C4 host CTZ first-match | Single `trailing_zeros` extract per mask, folded into C1/C2; the bulk-consumer form is REDRESS-89-flagged, NOT proposed. No retained bulk cursor. | **ACCEPT** |
| C5/C6 udot/i8mm | Orphan/net-new, inventory-only, not proposed; decode on demand if ever admitted. No substrate. (Orphan is CH1/CH4.) | **ACCEPT** |
| §3 REDRESS flags + §4 "No second substrate (Lock 1)" (`:417-421`) | Names transient-producer-not-sidecar and cross-call-classifier-state-REJECT. asmjson PC-as-state (the closest external retained-class-column) is host-blocked and cited as the Lock-1/CH5 concern. | **ACCEPT** |

### P2-A (SOTA teardown)

| Item | CH5 finding | Disposition |
|---|---|---|
| CP-A1 byte-class classifier | Produces only a `Vec<u32>` index (no JSON/CSS policy); §4 uses prefix-XOR/next-set-bit as transient producers, not a retained hot body or cross-call bulk consumer (`:442-445`). | **ACCEPT** |
| CP-A2 tape append | Single non-generic `TapeBuilder` sink; §4 explicit it re-opens nothing absent a `StructLayout`/`TapeStructBuilder`/`TapeCursor` second substrate (`:418-422`). Operates on the existing tape. | **ACCEPT** |
| CP-A3 lazy `ValueRef` rider | Reconstructs on demand over the EXISTING `Tape`/`ValueRef`; no eager tree, no second view substrate. | **ACCEPT** |
| CP-A4 tokenize-once shared-scan | Re-verified the most-exposed-to-"second-scan" candidate: it is "Bounded to the single-substrate shape (the structural projection IS the tape, Lock 1)" (`:354`) and is the CONSUMER half of CP-A1's producer ("the index is produced by CP-A1 and read by the per-grammar declaration/selector parse in the same wave", `:366`). It ELIMINATES the 2–3× re-walk by consuming the index ONCE — it adds no second scan; it removes redundant ones. No parser-local second cursor. | **ACCEPT** |
| §1.0 / §1.1 honesty framing | §1.0 names the recognition plane a "masking probe" with 2–3× headroom and is explicit the deficit is materialization NOT recognition — no claim the fast recognition number is the materializing number (the Track1==Track2 dishonesty). §1.1 frames simdjson Stage 1 as a transient structural projection consumed by ONE builder (admissible only as a same-loop mask, NOT a retained class column; Lock 1 / CH5 cited `:63-65`). | **ACCEPT** |

### P2-B (dav1d process)

| Item | CH5 finding | Disposition |
|---|---|---|
| C-B1 byte_class scan | §4 explicit (`:244-249`): "must not become a parallel substrate (Lock 1 / CH5) … must NOT retain a sidecar event vector, a density table, or cross-call classifier state (`LOCKS.md:141-149`) … The structural projection it feeds IS the tape, not a second scan." Directly addresses every CH5 vector. | **ACCEPT** |
| C-B2 push_plain_offset | §4 "must not re-open … fact-stream-as-retained-sidecar (Lock 1 FactStream clause `LOCKS.md:585`)" (`:256-258`) + single non-generic `TapeBuilder`. No second substrate, no Track1==Track2 sidecar. | **ACCEPT** |
| C-B0 admission process / C-B3 orphan | Process gate (G1–G6); orphan rejection. No substrate proposed. | **ACCEPT** |

### P2-F (grammar-neutral)

| Item | CH5 finding | Disposition |
|---|---|---|
| §1.1 `ValueRef<G>` generic-over-grammar framing | Verified against source (`mod.rs:175`): ONE generic cursor; JSON and CSS are instantiations, NOT two cursor types (`:31-38`). Forecloses the Track1==Track2 / two-cursor dishonesty at the substrate level. | **ACCEPT** |
| CF-1 tape-append + `ValueRef` projection | "No new cursor/builder type"; §4 item 6 "No `StructLayout`/`TapeStructBuilder`/`TapeCursor` … No sidecar event vector, no retained cursor, no parallel source pass (Lock 1, `LOCKS.md:585`)" (`:401-404`). | **ACCEPT** |
| CF-2 structural-membership classifier | Produces a `Vec<u32>` index the CF-1 tape consumes; §4 item 8 "Cross-call classifier-state retention REJECT … A SIMD mask stream is a transient producer, not a retained sidecar" (`:407-409`). | **ACCEPT** |
| CF-3 commit-by-construction Alt-mode | Codegen control-flow property; §4 item 3 "No type-ambivalent dual representation (Lock 1, tape vs OpenFrame vs direct-to-struct competing)". No substrate. | **ACCEPT** |
| CF-4a/CF-4b digit kernels | Decode on demand; §4 item 6 second-substrate guard applies; no substrate. (Orphan is CH1.) | **ACCEPT** |
| §4 item 6 second-substrate / item 8 cross-call carry (`:401-409`) | Both no-go vectors explicitly fenced. | **ACCEPT** |

## §4 — Track 1 == Track 2 honesty (the specific dishonesty CH5 hunts)

The dishonesty CH5 must catch is a candidate claiming Track 1 (recognition) and
Track 2 (typed/materialised) share a substrate while secretly running two scans,
two cursors, or a sidecar bridging them. No V3 artefact does this:

- P2-D §1.5 keeps the two PLANES honestly separate, explicit the recognition
  plane "materialises nothing — it fails preserve-rich-ast and is a masking
  probe, NOT the subject" (`:180-181`); the tape lever attacks the typed plane
  and the NEON scan attacks the surviving recognition cost on the SAME
  substrate, SEQUENTIALLY (`:194-196`). No Track1==Track2 sidecar.
- P2-A §1.0 names the recognition plane a "masking probe" with 2–3× headroom and
  is explicit the deficit is materialization, not recognition — no claim that the
  fast recognition number is the materializing number. The fair-bar discipline
  (lightningcss strict materializing bar / cssparser flaw-probe) is held
  throughout.
- P2-F §1.1 confirms the ONE generic cursor (`ValueRef<G>`): JSON and CSS are
  instantiations of the same type (source-verified `mod.rs:175`), not two cursors
  — the structural foreclosure of the two-cursor dishonesty.
- No candidate proposes a sidecar that BOTH the recognition and the typed plane
  read — the one `offsets` vector serves both, by construction (P2-D §1.4).

CH5 finds NO Track 1 == Track 2 dishonesty in V3.

## §5 — Disposition counts

- Sections/candidates dispositioned: **35** (P2-D: 10; P2-E: 5; P2-C: 6;
  P2-A: 5; P2-B: 3; P2-F: 6 — per row in §3, with the honesty review folded into
  the relevant artefact rows).
- **ACCEPT: 35**
- **REVISE: 0**
- **REJECT: 0**

ACCEPT rate (CH5 lens): **35/35 = 100%**. No V3 candidate introduces a parallel
substrate, a sidecar producer, a renamed scanner, a retained cursor, an aux
density table, or a Track1==Track2 dishonesty. P2-D's substrate-union conclusion
(tape + structural projection are ONE substrate — the single `offsets` vector is
simultaneously the scan output and the tape backbone, Lock 1) holds and is the
load-bearing Lock-1 verdict the lens confirms. This is the SECOND consecutive
cycle at 100% CH5 ACCEPT (V2 35/35, V3 35/35); the two V1 REVISEs remain folded
and clean.

## §6 — Orphan-REVISE foldback

NONE. Zero open REVISE, zero REJECT, zero orphan. CH5 carries no open critical
defect into V3. The ORCHESTRATOR §3Z "zero orphan unresolved REVISE" condition is
met for this lens across two consecutive cycles (V2→V3).

## §7 — Sources (CH5-load-bearing facts re-verified this cycle)

- `skinny/crates/runtime/src/tape/mod.rs:94-101` (`Tape` six fields, one
  position-keyed `offsets: Vec<u32>`), `:144-150` (`flags_at` sparse
  `binary_search` pair), `:175-181` (`ValueRef<'doc,'input,K,G:EventGrammar>` one
  generic cursor type, `Copy`).
- `skinny/crates/bbnf-simd/src/dispatch.rs:101-113` (`lo6_table_admissible`,
  local `seen` array, `slot = byte & 0x3f` at `:106`, no cross-call state).
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.4 (no-go list incl. "Track 1 ==
  Track 2 sidecars"); `restart/locks/LOCKS.md:75` (structural-projection-IS-the-
  tape), `:585` (Lock 1 sidecar/second-tape/cross-call clause), `:141-149` (v+1
  carry-within-chunk).
- V3 P2 artefacts: `p2d-substrate-tape.md` (§1.1 `:37-53`, §1.4 `:128-169`, §1.5
  `:171-196`, D-candidates `:206-423`, §4 `:444-484`); `p2e-parse-that-gaps.md`
  (G2 SHAPE `:150-154`, G3 §4 `:294-303`); `p2c-arch-esoterica.md` (§4 `:417-421`);
  `p2a-sota-teardown.md` (§1.0/§1.1 `:19-65`, CP-A4 `:350-376`, §4 `:418-445`);
  `p2b-dav1d-process.md` (§4 `:244-258`); `p2f-grammar-neutral.md` (§1.1 `:31-38`,
  §4 items 6/8 `:401-409`).
- Prior: `p2/hardening/V2/CH5.md` (35/35 ACCEPT), `p2/hardening/V1/CH5.md`
  (R-CH5-1, R-CH5-2 the folded REVISEs).
- Host: Apple M5 Max, aarch64-apple-darwin. Master HEAD `0ae1caa52`.
