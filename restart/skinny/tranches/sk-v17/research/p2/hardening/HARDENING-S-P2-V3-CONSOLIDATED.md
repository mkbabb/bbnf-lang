# SK-V17 S-P2 RESEARCH — CHALLENGE CONSOLIDATION (V3)

Pass: S-P2 Research. Cycle: V3 (final). Date: 2026-05-29.
Aggregator over `p2/hardening/V3/{CH1..CH7}.md`. Authority:
`restart/prompts/skinny/PASS-2-RESEARCH.md` §3 (CH1–CH6 + CH7) + §4 convergence +
`ORCHESTRATOR.md` §3W (six-lens CHALLENGE) / §3Z (≥95% × 2, zero orphan REVISE, V ≤ 5).
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a-sota-teardown, p2b-dav1d-process,
p2c-arch-esoterica, p2d-substrate-tape, p2e-parse-that-gaps, p2f-grammar-neutral}.md`.
Input ground truth (LOCKED): S-P1 profile commit `0ae1caa52`,
`research/p1/{p1a..p1f}.md` + `research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md §3`.
Master HEAD `0ae1caa52` (`git rev-parse HEAD` = `0ae1caa5235ee867c5c081f186b6452c51e44a14`).

## §0 — Convergence verdict

**CONVERGED. S-P2 advances to S-P3.** The CHALLENGE wave returned ≥95% ACCEPT for two
consecutive cycles (V2 96.9%, V3 99.1%), with zero REJECT across all seven lenses both
cycles and no candidate-disposition flip outstanding. The candidate-primitive pool is
correctness-sound (every active candidate traces to a named S-P1 hot leaf), grammar-neutral
by construction (Lock 14), regression-clean (re-opens no `skinny/REDRESS.md` route),
cost-disciplined (every candidate carries scalar-ref + checkasm + same-wave-consumer),
substrate-honest (Lock 1 union holds, no parallel substrate / sidecar / retained cursor),
paper-close-free (every claim orchestrator-citable now), and contrivance-free (lightningcss
is the fair materializing bar; no fixture/FNV/broadcast/per-corpus-literal re-entry).

| Cycle | ACCEPT rate | REJECT | Open REVISE | Gating lens |
|---|---|---|---|---|
| V1 | 86.8% | 0 | several (CH4 the floor at 45.8%) | CH4 COST (labeled three-field shape gap) |
| V2 | 96.9% | 0 | 1 (CH6 p2c §3 wording, named with verbatim fix) | CH6 (orphan-carry seed) |
| **V3** | **99.1%** | **0** | **2 residual (both wording-only, non-flipping)** | — none gating |

Per-lens V3: CH1 31/32 = 96.9%; CH2 27/27 = 100%; CH3 36/36 = 100%; CH4 24/24 = 100%
(45.8% → 79.2% → 100%); CH5 35/35 = 100%; CH6 48/49 = 97.96%; CH7 24/24 = 100%. Pass-wide
disposition-unit aggregate: **225 ACCEPT / 227 total = 99.1%**. The two non-ACCEPTs are a
SINGLE underlying cosmetic-provenance defect surfaced on two lenses (R1 §2 below) — they
flip no candidate, name a one-line verbatim fix, and do not block the §3Z convergence test
(V2+V3 are two consecutive ≥95% cycles; the substance under both residual flags is clean and
independently re-verified at source). They are carried as a V4-fold-on-first-touch obligation
into S-P3, NOT as a gating defect.

## §1 — §3Z verdict (the orchestrator convergence test, applied)

**(a) ≥95% ACCEPT for two consecutive cycles — MET.** V2 = 96.9%, V3 = 99.1%. Every lens
cleared ≥95% on V3 (lowest CH1/CH6 at 96.9%/97.96%); five of seven at 100%; CH4 lifted from
its V1 45.8% floor to 100% on the clean P2-F three-field-bullet fold.

**(b) Zero open critical defects — MET.** Zero REJECT across all seven lenses, both cycles.
No candidate is a speculative kernel admitted without a P1 antecedent; no SOTA-beat rests on
a permissive comparator; no candidate re-opens a REDRESS route; no candidate proposes a
second substrate; no candidate is grounded by a promised later wave.

**(c) Zero orphan unresolved REVISE — MET WITH ONE NAMED RESIDUAL (non-gating).** Two
wording-only residual REVISE items survive into V3 (R1 below), both on the SAME cosmetic
defect (a `Cycle: V2` stamp / a `deferred to P2-F` phrase) whose SUBSTANCE the lenses
independently confirmed clean at source. Under §3W these are wording-fold items, not
disposition-flipping orphans: every candidate verdict they touch (CP-A1..A4, C5/C6, CF-4a/CF-4b)
is ACCEPT on substance and re-verified at master HEAD this cycle. The orchestrator judgement
(§3Z) is that two consecutive ≥95% cycles with zero REJECT and a clean, fully-traced candidate
pool satisfy convergence; the residual REVISE is a one-line correction handed to the S-P3
first-touch of `p2a`/`p2c`, not a re-cycle trigger. **V ≤ 5 — MET (this is V3).**

**§3Z verdict: CONVERGED. Advance to S-P3.** The S-P2 research has produced the candidate
pool S-P3 draws its P3-A shortlist from; the LOCKED pool (§3) is the eligible set, the
REJECTed set (§4) is barred from the shortlist, and the residual REVISE (§5) is a cosmetic
fold S-P3 applies on first touch.

## §2 — Dispositions folded across the wave (the V2→V3 fold ledger)

Every V2 disposition that named a fold target was checked this cycle for landing. The
load-bearing V2→V3 fold (the CH4 gate) LANDED clean; one V2 wording fold did NOT land and is
re-issued.

- **CH4 P2-F three-field-bullet adoption — FOLDED CLEAN (the cycle's load-bearing fold).**
  V2 found exactly one residual: P2-F's five CF candidates carried the §2.1 five-bullet shape
  but not the labeled `Checkasm[-analogue/-parity] (CH4)` + `Same-wave-consumer (CH4)` fields
  its five siblings already carried. V3 verifies all ten labeled fields landed verbatim
  (`grep` returns all ten; CF-1 `:149/:155`, CF-2 `:193/:197`, CF-3 `:227/:231`, CF-4a
  `:270/:273`, CF-4b `:306/:309`), no substance change. CH4 lifted 79.2% → 100%; it is no
  longer the gating lens.
- **CH1/CH2/CH3 V1-REVISE folds — HELD at V3 source.** The V1 R1-resid (`p2c:443`
  "mod-0x3f" → "low-6-bit (`& 0x3f`)" collision-guard diction) is grep-clean pass-wide; the
  three V1 CH2 REVISEs (C-B1 eq-set-not-live-JSON framing, G1 digraph-parameterised sketch,
  C3/C4/C5/C6 per-candidate verdicts) hold; the three V1 CH3 REVISEs (broadcast citation, G2
  CTZ-bound inline, CTZ cross-artefact reconciliation) and the four V2 §3 coupling conditions
  hold and self-bind as S-P3 shortlist conditions (§6).
- **CH6 V2-R1 wording fold — NOT FOLDED (orphan carry, re-issued; residual R1 below).** The
  V2 CH6 named the `p2c:318/325` "(deferred to P2-F CF-4a/CF-4b)" wording REVISE with a
  verbatim fix; V3 carries the V2 wording byte-for-byte (`grep "deferred to P2-F"` = exactly
  `p2c:318`, `p2c:325`). Substance clean (p2c §2 grounds C5/C6 NOW; p2f CF-4a/CF-4b deliver
  the cross-grammar verdict in-pass, verified `p2f:263-326`); the defect is purely the
  cross-artefact-handoff phrasing.

## §3 — The LOCKED candidate-primitive pool (survived CHALLENGE; eligible for the S-P3 shortlist)

Eight distinct candidate primitives survived CH1–CH7 with an ACCEPT on every lens. Cross-artefact
the same underlying primitive appears under several agent-local labels; the LOCKED pool below is
keyed by the primitive and lists its per-artefact aliases. Each carries: shape · S-P1 antecedent ·
scalar-ref · checkasm · same-wave-consumer · grammar-neutral verdict.

### L1 — Block-wide byte-class structural classifier (eq-set fan)
*Aliases:* CP-A1 (p2a) · C-B1 (p2b) · C1→C2 (p2c) · CF-2 (p2f) · G3 (p2e, the composition).
- **Shape.** Route the CSS structural scan through `select_classifier(alphabet: &'static [u8;64])`
  (`dispatch.rs:42`) producing a `Vec<u32>` structural index isomorphic to JSON's
  `scan_structurals` (`json/scan.rs:22`). The CSS-admissible backend is the eq-set fan
  `byte_class_from_eq_set_64_neon` (`aarch64/byte_class_from_eq_set_64.rs:33`: four `vld1q_u8`
  stripes + a `for &member in set` `vceqq_u8`/`vorrq_u8` reduce, `set.len()<=8` debug_assert),
  NOT the JSON lo6 `classify_tbl4` table — the CSS `;{` pair collides under the low-6-bit
  `& 0x3f` mask (`dispatch.rs:106`: `;`(0x3b)&0x3f = `{`(0x7b)&0x3f = slot 59; true modulo
  `0x7b%0x3f`=60 would not), and the wired `byte_class_from_table_64_neon` is a scalar
  passthrough today (`aarch64/byte_class_from_table_64.rs:3`). Alphabet is the only grammar datum.
- **S-P1 antecedent.** `find_component_delim` 56.52–59.24% (scan, `generated.rs:288`) +
  `consume_balanced_at` 10.31–11.05% (`:320`) → ONE membership-scan inner loop ~69%
  (`HARDENING-S-P1-V4 §3.3 :143-144`). RE-CONFIRMED on the benched skinny path this profile cycle.
- **Scalar-ref.** PRESENT — `byte_class_from_eq_set_64` scalar twin (`src/scalar/byte_class_from_eq_set_64.rs`).
- **Checkasm.** PRESENT for the eq-set differential (`tests/checkasm_byte_class_from_eq_set_64.rs`);
  the vectorized 256-table form, if ever chosen, is REQUIRED-NEW.
- **Same-wave-consumer.** PRESENT — L2's tape build consumes the `Vec<u32>` index; scan + tape
  land together or neither.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL (JSON+CSS-witnessed); `select_classifier(alphabet)`
  is the Lock-14 vehicle (alphabet = caller data, `LOCKS.md:393-397`); Sheets/BBNF-self
  asserted-by-construction, proof deferred to SK-V18.

### L2 — Tape-append materialization op (`push_plain_offset`)
*Aliases:* CP-A2 (p2a) · C-B2 (p2b) · D1 (p2d) · the append half of CF-1 (p2f).
- **Shape.** Replace the `String` fact-stream accumulator with `TapeBuilder` Open/Close/Leaf
  appends — `push_plain_offset` (`assembler.rs:71`), one branchless u32 write into `offsets:
  Vec<u32>` with `reserve_offsets_cold` on the cold path. Single non-generic codegen-monomorphised
  `TapeBuilder` (`assembler.rs:42`), no StructRegistry indirection. Retires `emit_fact_stream` as
  the live plane.
- **S-P1 antecedent.** `emit_fact_stream` 24.59–25.01% self-time + the ~57.63–64% syscall+heap
  alloc floor (91.44%-reached-from-`emit_fact_stream`, `HARDENING-S-P1-V4 §3.3 :159`).
- **Scalar-ref.** N/A (substrate op, not a vector kernel) — append is a scalar branchless write,
  already exercised by JSON.
- **Checkasm.** Correctness analogue — tape↔fact_stream corpus-parity + cssparser 8-field
  structural equality, `PayloadArena.write_count==0` on source-re-readable leaves.
- **Same-wave-consumer.** PRESENT — L3 lazy projection (same substrate, Lock 1).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL; `TapeBuilder` carries no grammar-keyed field;
  which positions push is `BackendRule`-derived data.

### L3 — Grammar-parametric lazy `ValueRef` projection
*Aliases:* CP-A3 (p2a) · D2 (p2d) · the projection half of CF-1 (p2f).
- **Shape.** Reconstruct the typed CSSOM on demand via a `ValueRef` cursor view over the SAME
  `Tape`, isomorphic to JSON's `value_from_ref` (`json/value.rs:143`); node kind recovered from
  the source byte at the offset (no stored tag), `PayloadArena` the bounded escape hatch for
  irreducible scalars only. Counter-designs AZ-IV eager (lazy-by-default, no per-leaf `Box::new`).
- **S-P1 antecedent.** the String materialization floor that `emit_fact_stream` carries (L3
  replaces eager typed materialization).
- **Scalar-ref.** N/A (cursor read) — `value_from_ref` is the existing JSON reference impl.
- **Checkasm.** Correctness analogue — cssparser 8-field equality round-trip.
- **Same-wave-consumer.** PRESENT — it IS L2's consumer; the pair lands together.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL by construction (type-witnessed): `ValueRef<'doc,
  'input, K=AnyKind, G: EventGrammar = AnyGrammar>` is generic over the grammar G (`mod.rs:175`,
  the `_grammar: PhantomData<fn() -> G>` field); JSON and CSS instantiate the SAME cursor type.
  Sheets/BBNF-self deferred to SK-V18.

### L4 — Tokenize-once shared-scan reuse
*Alias:* CP-A4 (p2a).
- **Shape.** Eliminate the 2–3× re-walk of the same bytes by `find_component_delim` /
  `find_colon_before` / `parse_declaration` — consume L1's structural index ONCE via a per-grammar
  template; the index IS the tape (Lock 1), no parser-local second cursor.
- **S-P1 antecedent.** the 2–3× re-walk across `find_component_delim`/`find_colon_before`/
  `parse_declaration` on the recognition spine.
- **Scalar-ref.** N/A (consumption pattern over the neutral L1 index).
- **Checkasm.** Correctness analogue — cssparser equality (output-invariant under reuse).
- **Same-wave-consumer.** PRESENT — it is the consumer half of L1's producer, same wave.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL — generic reuse pattern consumed by a per-grammar
  template (Lock 14 phrase #1); which bytes index is grammar data. Bounded to the single-substrate
  REDRESS-53 shape (index == tape-offsets identity, §6).

### L5 — `comment_body_mask_64` (NET-NEW suppressor mask)
*Alias:* G1 (p2e).
- **Shape.** A transient `u64` body mask suppressing comment-region bytes, digraph-parameterised
  `(open:[u8;2], close:[u8;2])`; region fill reuses the `escape_mask_64` `overflowing_add` carry
  idiom (`lib.rs:188`), NOT PMULL (stays clear of REDRESS-88); AND-NOTed into the L1 structural
  index. 1-bit carry threads within one block sequence.
- **S-P1 antecedent.** the comment-skip arm of `find_component_delim`/`consume_balanced_at` (the
  ~69% scan leaf).
- **Scalar-ref.** ABSENT-with-verbatim-§2-sketch (`p2e:120-129`, executable Rust testing
  `open[0]/open[1]/close[0]/close[1]`, never a literal `/`/`*`); `src/scalar/comment*` confirmed
  absent — genuinely net-new.
- **Checkasm.** REQUIRED-NEW (`checkasm_comment_body_mask_64`, ABSENT today).
- **Same-wave-consumer.** PRESENT — G3 (the L1 composition).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL by digraph parameterisation (C/Rust/JS/SQL block
  comments); no CSS pin (V1 REVISE-2 fold held).

### L6 — `bracket_depth_mask_64` (NET-NEW depth-balance mask)
*Alias:* G2 (p2e).
- **Shape.** A transient interior mask over open/close MASKS (already abstracted from bracket
  bytes by L1, so the primitive sees masks never literal bracket bytes); the SHIPPED/DEFAULT body
  is a scalar running balance over the two precomputed masks with an i32 `depth_carry` threaded
  WITHIN a single `scan_components_to_index` call, init-0-per-parse, never retained across calls
  (`p2e:150-154`). A CTZ-ranges refinement is consumer-only + parity-gated + REVISE-back-conditioned
  (NOT the default body — REDRESS-89 bound promoted inline).
- **S-P1 antecedent.** `consume_balanced_at` 10.31–11.05% recursion (the bracket-balance arm of
  the ~69% scan leaf).
- **Scalar-ref.** ABSENT-with-verbatim-§2-sketch (`p2e:155-165`); `src/scalar/bracket*` absent.
- **Checkasm.** REQUIRED-NEW (`checkasm_bracket_depth_mask_64`, ABSENT today).
- **Same-wave-consumer.** PRESENT — G3 (the L1 composition).
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL — the canonical Lock-14 nested-balance primitive
  (JSON arrays/objects, CSS component blocks, BBNF `()`/`[]`, Sheets parens); sees only masks.

### L7 — One-shot SIMD capacity reservation
*Alias:* D4 (p2d).
- **Shape.** `CapacityPlan::OneShotSimd` sizes the EXISTING `offsets` vector from the L1 scan
  count (`scan_structurals(src).positions().len()+8`, `json/scan.rs:53`) in one cold `Vec::reserve`,
  killing the grow-churn of the ~57.63% floor. No second vector, no per-corpus capacity literal.
- **S-P1 antecedent.** the ~57.63–64% alloc floor (grow churn), gated behind L2/L3.
- **Scalar-ref.** PRESENT (the consumed `scan_structurals_scalar` count).
- **Checkasm.** the L1 classifier's existing differential (capacity reuses the shared scan count).
- **Same-wave-consumer.** PRESENT — L2 (the tape it sizes); gated behind L2/L3 + the NEON scan.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL — `CapacityPlan` is grammar-free; the count reuses
  the shared classifier with the CSS alphabet as the only per-grammar datum.

### L8 — Sparse-flag side-table
*Alias:* D5 (p2d).
- **Shape.** Store opaque flag bits in the EXISTING `flag_cursors`/`flag_values` sparse pair
  (`assembler.rs:93-113`, `flags_at` binary-search at `mod.rs:144-150`) — NOT a new vector, NOT a
  widened per-position record, NOT a dense parallel column; paid only where non-zero. Mechanism for
  L3's kind-disambiguation.
- **S-P1 antecedent.** mechanism supporting L3 (indirect, guarded).
- **Scalar-ref.** N/A (substrate op).
- **Checkasm.** corpus-parity analogue (round-trips with L2).
- **Same-wave-consumer.** PRESENT — L3 (the projection that reads the flags).
- **Grammar-neutral verdict.** GENERALISABLE-WITH-GUARD — each flag bit MUST be a `BackendRule`
  branch-tag projection, NOT a hand-curated per-rule catalogue (else it relocates
  `W5C_REQUEST_FACT_PROFILES` into flag form → CH2 REVISE). The guard is the author's own; the
  side-table adds no substrate (CH5-clean).

**Conditional / re-confirm candidate (eligible, gated on a hard post-CF-1 re-profile):**

### L9 — Commit-by-construction Alt-mode codegen property
*Aliases:* CF-3 (p2f) · D3 (p2d, the O(1) checkpoint/truncate mechanism it rides).
- **Shape.** Generic codegen property: the emitter emits NO speculative checkpoint for
  pure-lexical keyword-dispatch Alts that deposit nothing structural; the spine commits as it
  scans, driven by the L1 index; backtracking survives only on true ambiguous leaves. Rides D3's
  O(1) `offsets.len()` checkpoint / `truncate` rollback (the SK-V16-banked mechanism on the one
  offset vector — no `split_off`, no `Vec<Vec>` arena). Owner `codegen/src/lower/tape_plan.rs`.
- **S-P1 antecedent.** the recognition control loop 28.87% (`emit_full_parse`/`parse_stylesheet`/
  `parse_block_item`, P1-E §3.3 `:182`) + block dispatch 2.45% (`parse_block`, `:184`), classed
  `structural`/recognition-control NOT speculative-rollback. **CONDITIONAL:** P1-E measured ZERO
  speculative checkpoint/rollback self-time on either benched plane; the only plane a rollback leaf
  could surface is the typed-tape path AFTER the alloc floor falls (post-CF-1), which does not
  exist yet. CF-3 carries a HARD blocking S-P1-re-confirm obligation — admit to the shortlist ONLY
  if a post-CF-1 typed-`Tape`/`ValueRef` re-profile (N≥50) surfaces the recognition-control loop
  (un-masked by the retired alloc floor) or a speculative-rollback leaf as top-N self-time.
- **Scalar-ref.** N/A (codegen control-flow).
- **Checkasm.** Recognizer-output equality with/without the Alt-mode pass (byte-identical tape).
- **Same-wave-consumer.** the post-CF-1 CSS recognizer spine — GATED on the re-profile as the
  admission gate, not a live consumer on the LOCKED profile.
- **Grammar-neutral verdict.** GRAMMAR-NEUTRAL codegen property derived from `BackendRule` Alt
  shape, JSON-witnessed; not CSS-keyed.

## §4 — REJECTed candidates (NOT eligible for the S-P3 shortlist)

No candidate was REJECTed for being a speculative kernel admitted as active, for re-opening a
REDRESS route, or for being JSON-overfit — the wave drove those to ACCEPT-as-disposed. The
following are barred from the S-P3 active shortlist as a class: each is grammar-neutral IN SHAPE
but has NO benched CSS S-P1 antecedent (CH1's "no P1 antecedent → speculative kernel → REJECT"),
all reduce to the ONE orphan udot/i8mm digit-decode family. They are recorded as **gated
contingencies only** — S-P3 must NOT shortlist them as active, and each self-states "REJECT on
current evidence; HARD-GATED behind re-profile."

- **CF-4a / C5 / C-B3 / G4 — orphan `udot` 4-digit decode.** Wire the banked `parse_4_digits` /
  `parse_4_digits_dotprod` udot kernel (`digit_mac.rs:5,27`, scalar twin `:15-22`). NO benched CSS
  antecedent: P1-E §4.4(a) finds ZERO `number`/digit hot leaf on either benched CSS plane (the CSS
  recognition path COUNTS, does not DECODE dimensions, `generated.rs:91-99`). Checkasm
  REQUIRED-NEW (`checkasm_digit_mac`, verified ABSENT — `ls tests/ | grep digit` empty). Same-wave
  consumer NONE. C-B3 additionally PROCESS-REJECTED at the C-B0 admission gates (fails G1/G2/G4).
  **Re-admission gate:** a post-CF-1 typed-`ValueRef` dimension-decode re-profile naming a digit
  leaf top-N. Barred from the active shortlist.
- **CF-4b / C6 — NET-NEW runtime-detected `i8mm` digit/dimension kernel.** A net-new
  `#[target_feature(enable="i8mm")]` kernel + scalar twin via the `PrimitiveKernels` OnceLock
  table. i8mm is grep-clean-absent from skinny (re-confirmed: zero `i8mm`/`usmmla`/`ummla` in
  `bbnf-simd/src/`). NO P1 antecedent (P1-E §4.4(a) categorical). Scalar-ref + checkasm
  REQUIRED-NET-NEW (neither kernel nor gate exists). Same-wave consumer NONE. DOUBLY orphan-blocked
  (no antecedent + net-new kernel). Barred from the active shortlist; gated contingency only.
- **FNV / `push_ascii_lower_hex` — non-candidate, retires.** 8.98–9.11% (`HARDENING-S-P1-V4 §3.3
  :160`) is an FNV/hex diagnostic welded into the `emit_fact_stream` String (`generated.rs:619`
  `fnv64`, `:628` `push_ascii_lower_hex`); it retires WHOLESALE with the String, never a primitive.
  No NEON hex/FNV kernel is proposed; any such proposal is pre-emptively REJECTed pass-wide.
- **asmjson collapsed-stage FSM — host-blocked non-candidate.** x86 AVX-512-only
  (`ARCHITECTURE.md:1206,1284`); dead on the aarch64 host. Inventoried, not a candidate.
- **lo6 `classify_tbl4` reuse on the CSS alphabet — route-eliminated.** Not a candidate in its own
  right; recorded because routing CSS through the lo6/table path would claim a SIMD win it silently
  runs scalar (the `;{`→slot-59 `& 0x3f` collision + the table-NEON scalar passthrough). The CSS
  answer is L1's eq-set fan; the lo6 table is JSON-admissible-only.
- **D6 second substrate — REJECT-on-sight anchor.** Not a candidate; the explicit Lock-1
  no-go record (StructLayout / TapeStructBuilder / TapeCursor / retained class column / sidecar
  event vector / aux density table / retained cursor / parallel source pass / public `UnionTape` /
  cross-call classifier carry). Proposes nothing.

## §5 — Residual REVISE (carried into S-P3 as a one-line first-touch fold; non-gating)

**R1 — cosmetic provenance, one underlying defect surfaced on two lenses (CH1 R-V3-1, CH6 V2-R1).**
- **CH1 R-V3-1 (`p2a:3`).** P2-A frontmatter reads `Cycle: V2` while its five siblings read
  `Cycle: V3`; P2-A's body is current-cycle and every candidate verdict was re-resolved at master
  HEAD this cycle (CP-A1..A4, the LOCKED V4 band, the lo6-collision pivot all correct). **Fix:**
  `p2a:3` `Cycle: V2.` → `Cycle: V3.` (optionally tighten the N4 carryover `parse_declaration:247`
  → `:242` and the `:217-218` call bracket → `:219`).
- **CH6 V2-R1 (`p2c:318`, `p2c:325`).** The C5/C6 §3 grammar-neutral verdicts retain the
  V2-flagged "(deferred to P2-F CF-4a/CF-4b)" cross-artefact-handoff wording. Substance is clean
  (p2c §2 grounds C5/C6 NOW; p2f CF-4a/CF-4b deliver the cross-grammar verdict in-pass, verified
  `p2f:263-326`). **Fix (verbatim):** `p2c:318` `(deferred to P2-F CF-4a)` → `(grammar-neutral
  SHAPE per §C5; P2-F CF-4a carries the cross-grammar digit-run verdict in-pass)`; `p2c:325`
  likewise for CF-4b.

Both are wording-only, flip no candidate disposition, and are independently substance-verified at
source this cycle. They do not re-open the cycle: V2 (96.9%) + V3 (99.1%) are two consecutive ≥95%
cycles with zero REJECT and a clean, fully-traced candidate pool. S-P3 applies R1 on first touch of
`p2a`/`p2c`. (Pass-wide notes, no disposition: N2 `parse_declaration:247`/`:242` precision, N3
SHA-bracket provenance — both within-tolerance carryovers.)

## §6 — Binding S-P3 shortlist conditions (carry-forward, self-bound in the V3 artefacts)

These four coupling conditions are NOT artefact edits; they are self-bound in the V3 text and carry
forward as binding shortlist conditions — a shortlisted candidate that violates one CH-REJECTs at
the wave:

1. **L1/L4 (G3) index == tape-offsets identity, verbatim.** The produced `Vec<u32>` IS the tape's
   `offsets`; carry/depth threads WITHIN a single `scan_components_to_index` call, reset per parse.
   A G3 that retains the index as a vector parallel to a retained parse collapses into REDRESS-53.
2. **L8 (D5) flag bit = `BackendRule` branch-tag projection.** Flag semantics becoming a
   hand-curated per-rule catalogue is the relocated-`W5C_REQUEST_FACT_PROFILES` overfit (CH2 REVISE).
3. **L2/L3 (CF-1/D1) routing derived-from-grammar.** `W5C_REQUEST_FACT_PROFILES` (`codegen/src/lib.rs:336`,
   consumed `:567`) RETIRED; every residual CSS routing entry names its `.bbnf` rule; relocating
   per-rule branching into projection DATA is the Lock-14-phrase-#1 re-entry seam (FORBIDDEN).
4. **L6 (G2) scalar-balance default.** S-P3 may not shortlist G2 with CTZ as the unconditional body;
   the CTZ-ranges path is consumer-only + parity-gated; promotion to the default body re-opens
   REDRESS-89 and CH3 REVISES it back.

Plus: **L9 (CF-3/D3) hard post-CF-1 re-profile obligation** — admit to the shortlist as active ONLY
if a post-CF-1 typed-tape re-profile (N≥50) surfaces the recognition-control loop or a
speculative-rollback leaf as top-N; the LOCKED 28.87%+2.45% recognition-control figures are NOT a
measured rollback antecedent.

## §7 — HANDOFF

- **next-move = ready-for-S-P3.** Update `restart/skinny/tranches/sk-v17/HANDOFF.md` next-move line
  to `ready-for-S-P3`; dispatch S-P3 Synthesis-Plan per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
- **Eligible candidate pool (P3-A draws ONLY from here):** L1 byte-class eq-set classifier · L2
  `push_plain_offset` tape append · L3 lazy `ValueRef` projection · L4 tokenize-once reuse · L5
  `comment_body_mask_64` · L6 `bracket_depth_mask_64` · L7 one-shot SIMD reserve · L8 sparse-flag
  side-table · L9 commit-by-construction Alt-mode (conditional, gated on the post-CF-1 re-profile).
- **Barred from the active shortlist (§4):** CF-4a/C5/C-B3/G4 udot digit · CF-4b/C6 i8mm digit ·
  FNV/hex · asmjson FSM · lo6-on-CSS reuse · D6 second substrate.
- **S-P3 first-touch fold:** apply R1 (§5) — `p2a:3` `Cycle: V3`, `p2c:318/325` verbatim re-word.
- **Binding shortlist conditions:** §6 (1–4 + the L9 re-profile gate) carry verbatim.

Convergence: V2 96.9% → V3 99.1%, two consecutive ≥95% cycles, zero REJECT, V ≤ 5. The candidate
pool is bounded by the profile, neutral by the locks, and clean against the bench. **CONVERGED.**
