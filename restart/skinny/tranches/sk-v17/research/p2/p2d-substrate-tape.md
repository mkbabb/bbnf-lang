# SK-V17 P2-D: Substrate + Tape Design

Pass: S-P2 Research. Cycle: V3.
Date: 2026-05-29.
Scope: Interrogate the offset-tape substrate of the benched skinny tree
(`skinny/crates/runtime/src/tape/`): the lazy-materialisation counters, the
logical-vs-allocated tape ratios, the structural-projection union; conclude
whether tape + structural projection are ONE substrate (Lock 1) and name, per
candidate primitive, where a tape-shape change moves a NAMED S-P1 hot leaf. No
parallel-substrate proposals.
Output: this file.
P1 hot-leaf antecedents: `CssFullParser::find_component_delim` (56.52% self,
`css_l4_declaration_values/generated.rs:288`, P1-E §2.3/§2.5);
`CssFullParser::consume_balanced_at` (11.05% self, `generated.rs:320`, folds
into the SAME byte-class scan target, P1-E §2.3); `generated::emit_fact_stream`
(24.59% self + 91.44% of the 57.63% syslib floor, `generated.rs:5`, P1-E
§2.4/§2.5); `generated::push_ascii_lower_hex` (9.11%, FNV hex diagnostic,
`generated.rs:628`, P1-E §2.4 — NO primitive); the syscall+heap allocator floor
(57.63% on the fact_stream plane, P1-E §2.4); the recognition control loop
(`parse_stylesheet`/`parse_block`/`parse_block_item` 28.87%+2.45%, classed
"recognition control loop" — NOT measured speculative rollback — at
HARDENING-S-P1-V4 §3.3 / line 145; D3 carries an explicit S-P1-re-confirm
obligation per the CH1-V1 R3 fold); the absent tape leaf (zero
`Tape`/`ValueRef` samples on either CSS plane, P1-E §2.5 — the empirical
"UNWIRED" proof).
Lock surface: Lock 1 (substrate-union — load-bearing; this artefact's central
conclusion) + Lock 14 (every tape-op candidate grammar-neutral or per-grammar
template; the alphabet/`BackendRule` shape is the only per-grammar datum).

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

### 1.1 The benched substrate is one offset vector + sparse flags + a (usually empty) payload arena

The live benched tape is `skinny/crates/runtime/src/tape/`. Its concrete shape,
read at source:

- `Tape<'input>` (`mod.rs:94-101`) owns exactly SIX data members:
  `source: &'input [u8]` (`:95`, borrows, never copies),
  `offsets: Vec<u32>` (`:96`, the structural index — one u32 per structural
  position into `source`), `flag_cursors: Vec<u32>` + `flag_values: Vec<u8>`
  (`:97-98`, a SPARSE pair-of-vectors side table — only positions whose flags are
  non-zero are recorded, binary-searched at read time `flags_at` `:144-150`),
  `payloads: PayloadArena` (`:99`), and `id: TapeId` (`:100`, a `u64`
  newtype `:91-92` identifying the tape for cursor-provenance debug asserts —
  a scalar tag, not a per-position vector). CRITICAL Lock-1 observation: of the
  six members, exactly ONE is a position-keyed vector indexed parallel to the
  structural stream — `offsets`. `flag_cursors`/`flag_values` are a SPARSE pair
  (entries only where a flag is non-zero, cursor-keyed by binary search, NOT a
  dense parallel column), `payloads` is a single growable byte arena for
  irreducible scalars (NOT position-indexed), and `source`/`id` are scalars. There
  is NO parallel positions vector, NO class column, and NO density table: the
  structural projection IS the single `offsets` vector (the §1.4 union), and the
  six-field count carries zero second-substrate construct.
- `PayloadArena` (`mod.rs:38`) is a single `bytes: Vec<u8>` (`:39`) with two
  counters (`writes`/`allocations`, `:41-43`) compiled in ONLY under
  `cfg(any(test, feature = "bench-counters"))`. `write_bytes` (`:65-78`) appends
  and bumps `writes`, bumping `allocations` only when the backing `Vec` capacity
  actually changes (`:73-74`). It is the lazy-materialisation counter surface.
- `ValueRef<'doc,'input,K,G>` (`mod.rs:175`) is `{ tape: &Tape, cursor: u32 }`
  plus three zero-size `PhantomData` markers (`:178-180`); it is `Copy`
  (`:183`), 8 bytes of live data. `offset()` (`:218-222`) is one `offsets[cursor]`
  array read. This is the cursor; there is no second cursor type.

The node KIND is NOT stored in the tape. `JsonNodeKind::at_cursor`
(`json/value.rs:29-47`) recovers it by reading the SINGLE SOURCE BYTE at the
offset (`tape.source()[offset]`, `:33`) and matching it (`{`→ObjectOpen,
`"`→String, `-|0..9`→Number, …). The tape is therefore a pure position index;
the typed projection is reconstructed lazily from `(source, offset)` with zero
stored tag and zero stored payload. `value_from_ref` (`json/value.rs:143-173`)
is the whole materialisation surface: it switches on `at_cursor` and wraps the
same `ValueRef` in a typed view struct — no allocation, no copy, no eager tree.
This is the model the SYNTHESIS §0.3 "Lazy-view projection generator" obligation
requires the CSS rider to mirror (isomorphic to `value_from_ref`,
`SYNTHESIS.md:178`).

### 1.2 The append op is one branchless u32 write; the floor it replaces is measured

`TapeBuilder::push_plain_offset` (`assembler.rs:71-85`): in the hot case
(capacity available) it is `ptr.add(len).write(offset as u32); set_len(len+1)`
— one bounds-free store + one length bump, `#[inline(always)]`. The grow path is
`#[cold] #[inline(never)]` (`reserve_offsets_cold` `:87-91`), off the hot path.
`push_offset` (`:61-68`) adds an optional `patch_flags` only when
`flags.bits() != 0` (`:64`), so the common structural push pays nothing for
flags. `patch_flags` (`:93-113`) appends to the sparse side vectors in cursor
order (`debug_assert` `:105-110`) — it does NOT widen the per-position record.

The cost this REPLACES is named and measured by P1-E: the live benched CSS
typed plane (`track1_fact_stream`, `emit_fact_stream` `generated.rs:5`) spends
**24.59% self-time in the `String` `push_str` accumulator** plus **57.63% in the
syscall+heap allocator floor (31.36% `libsystem_kernel` + 26.27%
`libsystem_malloc`), of which 91.44% is reached FROM `emit_fact_stream`'s
`String` growth** (P1-E §2.4, syslib-caller attribution). On the instr/byte
axis (the sole load-bearing cost density, HARDENING-S-P1-V4 §3.1) the
fact_stream plane is **214.56–364.51 instr/byte**, ~4.4× the recognition plane's
**46.46–57.72**. The delta IS the String-building + allocation tax. A
`push_plain_offset` append carries no `String`, no `push_str`, no per-leaf grow:
the 57.63% allocator floor collapses to the amortised geometric growth of ONE
`Vec<u32>` (`reserve_offsets_cold`, off the hot path). This is the empirical
ground for the SYNTHESIS §3 lever-1 (kill fact-stream String) + lever-2 (O(1)
tape checkpoint), not a fresh proposal.

### 1.3 Logical-vs-allocated ratio and the lazy-materialisation counters

`OffsetTapeStats` (`offsets.rs:1-6`) carries `offset_count`, `offset_bytes`,
`offset_capacity_bytes`. The logical-vs-allocated ratio is
`Tape::offset_bytes()` (`mod.rs:152-154`, = `offsets.len() * 4`) over
`offset_capacity_bytes()` (`mod.rs:160-164`, = sum of the three vectors'
`capacity() * elem`). For a recognition/structural tape this ratio is bounded by
the capacity-plan policy (`CapacityPlan`, `assembler.rs:13-40`): the production
default is `GrowOnly` (`:27`), small initial reserve + geometric grow, so the
allocated overshoot is at most ~2× logical and is amortised. The SIMD one-shot
plan (`OneShotSimd`, `:26`) reserves exactly from the scan output
(`structural_capacity_for` → `scan_structurals(source).positions().len() + 8`,
`json/scan.rs:53`), driving the ratio toward ~1.0 with zero grow events.

The lazy-materialisation counter is `PayloadArena::write_count` /
`allocation_count` (`mod.rs:80-88`). The Lock-1-honest invariant for the SK-V17
CSS rider (alphaC §1, ledger row 1, `alphaC:348`): **a correct lazy projection
emits ZERO payload bytes / writes / allocs for any leaf re-readable from
`source`** — `PayloadArena` is populated only for irreducible decoded scalars
that cannot be recovered by re-reading the source span (alphaC `:87`,
`:366`). The proof that the tape is activated (not the eager AZ-IV value tree) is
exactly `write_count == 0` (or near-zero) on the structural path while
`offset_count > 0` — the SYNTHESIS Tape-activation gate's "`PayloadArena`
write/alloc counters confirm the parse emits into the tape rather than into a
fact-stream String" (`SYNTHESIS.md:110`).

### 1.4 The substrate union holds: the structural index IS the offset tape (Lock 1)

JSON proves the union concretely. `scan_structurals` (`json/scan.rs:22-30`)
returns a `StructuralIndex` (a `Vec<u32>` of positions, via NEON `vqtbl4q_u8`
lo6-table classify on aarch64 `:24`, scalar fallback `:32-36`). Those positions
are the SAME u32 values the `TapeBuilder` writes via `push_plain_offset` — the
SIMD mask stream is the transient PRODUCER of the offset vector, and once the
offsets are retained the structural projection IS the tape (Lock 1 verbatim,
`LOCKS.md:75`: "if structural offsets are retained, the structural projection IS
the tape"; `LOCKS.md:235` in SYNTHESIS §0.4: "A SIMD mask stream is a transient
producer, not a retained sidecar"). There is ONE vector, not two. The scan does
not produce a sidecar that a second pass folds into a tape; it produces the tape
directly. `StructuralIndex::from_positions(neon::scan(input), …)` (`scan.rs:24`)
→ the positions feed `push_plain_offset`. No aux density table, no class-column
vector, no retained cursor, no parallel source pass.

For CSS the union is identical by construction: P2-C/P2-E's NEON candidate
(`select_classifier(alphabet)`, `dispatch.rs:42`) produces a `Vec<u32>`
structural index over a CSS delimiter alphabet; that vector IS the CSS tape's
`offsets`. There is no design freedom to make it a second substrate — the only
per-grammar datum is the alphabet (`StructuralAlphabet::from_bytes`,
`scan.rs:7`), and the low-6-bit-collision guard (`lo6_table_admissible`,
`dispatch.rs:101`, which computes the low-6-bit mask `(byte & 0x3f)` at
`dispatch.rs:106`, NOT a true modulo) decides scalar-vs-NEON for that alphabet
without retaining any cross-call classifier state (Lock 1 v+1,
`LOCKS.md:137-149`: carry stays within a single chunk-call). The CSS alphabet
`;`=0x3b and `{`=0x7b collide at low-6-bit slot 59 (`0x7b & 0x3f = 0x3b`), which
would NOT coincide under a true modulo (`0x7b % 0x3f = 0x3c`); this real
collision is why the lo6/`vqtbl4q_u8` route is INADMISSIBLE for the CSS
delimiter alphabet and the CSS scan routes through the eq-set fan
(`byte_class_from_eq_set_64`) instead (P2-C C2 / P2-F §1.2).

**Conclusion (the P2-D Lock-1 verdict):** tape and structural projection are ONE
substrate in the benched skinny tree. The offset `Vec<u32>` is simultaneously the
scan output and the tape backbone; the node kind is recovered from the source
byte at the offset (no stored tag); typed values are lazy `ValueRef` views; the
`PayloadArena` is the bounded escape hatch for irreducible scalars only. No
SK-V17 substrate candidate may introduce `StructLayout`/`TapeStructBuilder`/
`TapeCursor` (a SECOND substrate, REJECT under Lock 1 type-ambivalence,
`SYNTHESIS.md:238`), a sidecar event vector, a retained class column, or a
parser-owned structural stream. Every candidate below is a tape-SHAPE or
tape-WIRING change on the existing `Tape`/`ValueRef`, never a new substrate.

### 1.5 Where the CSS hot leaf lives today, and where the tape moves it

P1-E §2.5 names two hot regimes on two planes:
- **Recognition plane** (`track1_full_parse`): ~68% in ONE byte-class scan
  primitive (`find_component_delim` 56.52% + `consume_balanced_at` 11.05%,
  sharing the `while pos<len` + per-byte `match` inner loop, `generated.rs:293-308`
  ≡ `:322-338`), ~31% inlined control loop, ~0% allocator. The tape is NOT the
  bottleneck here — the scan is. A tape-shape change does NOT move this leaf; a
  NEON byte-class scan does (P2-C/P2-E owns that). But this plane materialises
  nothing — it fails preserve-rich-ast and is a masking probe (P1-E §4.1), not
  the subject.
- **Typed fact_stream plane** (`track1_fact_stream`, the live benched Track 1):
  the scan is INVISIBLE — it is overwhelmed by the 24.59% `emit_fact_stream`
  `push_str` accumulator + the 57.63% allocator floor (91.44% from
  `emit_fact_stream` growth). On THIS plane the tape-shape change is the
  dominant lever: replacing `String` `push_str` with `push_plain_offset` append
  + lazy `ValueRef` projection retires the entire 24.59%+57.63%≈82% serialization
  tax. The hot leaf MOVES from `emit_fact_stream` (`generated.rs:5`) to
  `push_plain_offset` (`assembler.rs:71`, one branchless write) + the
  `find_component_delim` scan that the fact-stream floor was hiding (P1-E
  anomaly 3: "lever-1/2 (tape) FIRST, then NEON on the surviving scan").

So the tape-shape change is the FIRST lever (it retires the measured 82% floor
and unmasks the scan); the NEON scan is the SECOND (it attacks the unmasked
~68% recognition cost). P2-D owns lever-1/2; P2-C/P2-E own the scan kernel. The
two are sequential, not competing — they touch the SAME substrate.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent + grammar-neutral verdict)

All candidates are tape-substrate operations on the EXISTING `Tape`/`ValueRef`/
`TapeBuilder`/`PayloadArena`. None is a SIMD/ASM kernel (those are P2-C/P2-E);
the scalar-ref column is therefore "N/A — substrate op, not a kernel" except
where a tape op consumes a kernel output, in which case the kernel's scalar ref
is named. Each names the NAMED S-P1 hot leaf it moves.

### D1 — `push_plain_offset` structural append as the CSS Track-1 emit op (replaces `emit_fact_stream` String)

- **Shape:** for every structural position the CSS recognizer currently feeds the
  fact-stream `String`, instead call `TapeBuilder::push_plain_offset(pos)`
  (`assembler.rs:71`) — one branchless `ptr.write` + `set_len`. The CSS parser
  becomes generic over the tape sink (the conversion report `:55` notes parse fns
  are monomorphised to a named builder today; the seam accepts the tape sink
  without a second substrate). Output is a sealed `Tape` (`finish`,
  `assembler.rs:115-123`), NOT a `String`.
- **Scalar-ref status:** N/A — a `Vec<u32>` append, not a SIMD kernel. Already
  exercised cold by all 51 JSON rows (`RESULTS.md:5-25`); it is the proven hot
  path, not a new primitive.
- **Checkasm-analogue (CH4):** N/A as a SIMD differential (no NEON body), but the
  substrate-correctness analogue IS the `PayloadArena` lazy-counter equivalence
  (`write_count`/`allocation_count`, `mod.rs:80-88`): the CSS tape-append output
  must round-trip to the SAME logical document as the fact-stream `String` it
  replaces (full typed-AST equivalence, preserve-rich-ast), proven by a
  fact-stream-vs-tape projection differential analogous to `scalar_parity_report`
  (`json/scan.rs:38`) — the tape projection and the retired String emit the same
  values, with `write_count==0` on source-re-readable leaves.
- **Same-wave-consumer (CH4):** PRESENT — D2 (the lazy `ValueRef` CSS projection)
  is the in-wave consumer of every offset D1 appends; the sealed `Tape` D1
  produces (`finish`, `assembler.rs:115`) is read by D2's view emitter. D1 and D2
  ship together; D1 has no orphan tape.
- **Arch:** architecture-neutral scalar branchless write (aarch64 here; the op is
  not ISA-specific).
- **P1 antecedent:** `emit_fact_stream` 24.59% self + 91.44% of the 57.63%
  syslib floor (`generated.rs:5`, P1-E §2.4/§2.5). The fact_stream plane's
  214.56–364.51 instr/byte (HARDENING-S-P1-V4 §3.1) is the cost this retires.
- **Grammar-neutral verdict:** GENERALISABLE. `push_plain_offset` is grammar-free
  (it takes a `usize` offset; it has no CSS knowledge). JSON rides it today; CSS
  rides the SAME op; the per-grammar datum is only WHICH positions get pushed,
  derived from the `.bbnf`/`BackendRule` shape via `lower/offset_tape.rs`
  (`SYNTHESIS.md:178`), not a per-rule branch. Lock-14 clean.

### D2 — Lazy `ValueRef` typed-CSSOM projection (kind recovered from source byte; zero payload)

- **Shape:** generate the CSS typed views (`CssRule`/`StyleRule`/`Selector`/
  `Declaration`/`CssTypedValue`/`CssColor`/`CssDimension`/`CssFunction`) as
  `ValueRef`-cursor reads over the sealed `Tape`, isomorphic to
  `json/value.rs:143` `value_from_ref`. The node kind is recovered by a
  CSS-grammar `at_cursor` (the analog of `JsonNodeKind::at_cursor`,
  `json/value.rs:29-47`) reading the source byte(s) at the offset — NO stored
  tag, NO eager tree, NO per-leaf `Box::new`. Irreducible decoded scalars (e.g.
  a parsed color component that cannot be re-read from the span) go to
  `PayloadArena` via `write_bytes` (`mod.rs:65`) ONLY.
- **Scalar-ref status:** N/A — cursor arithmetic + source re-read, not a kernel.
  Proven by JSON's `value_from_ref` riding the same `Tape`/`ValueRef`.
- **Checkasm-analogue (CH4):** the projection-equivalence differential — the CSS
  `ValueRef` view set must yield the SAME typed values as the retired
  fact-stream / the lightningcss-parity 8-field reference (preserve-rich-ast),
  isomorphic to JSON's `value_from_ref` parity. The mechanical gate is the
  `PayloadArena` lazy-counter (`write_count`/`allocation_count`, `mod.rs:80-88`):
  `write_count==0` for every source-re-readable leaf proves the view is lazy, not
  an eager AZ-IV tree. This is the substrate analogue of a checkasm differential
  (oracle = retired String projection + 8-field lightningcss reference).
- **Same-wave-consumer (CH4):** PRESENT and intrinsic — D2 IS the consumer of D1's
  tape; equivalently D1 is D2's producer. The pair is the same-wave unit (no
  consumer-less view, no producer-less tape). The downstream consumer of D2's
  views is the CSS bench / public document API (`DocumentView`, `mod.rs:227`).
- **Arch:** architecture-neutral.
- **P1 antecedent:** the typed materialisation cost — `emit_fact_stream` (the
  String the typed plane builds, `generated.rs:5`, P1-E §2.4) — is what this
  replaces with a zero-copy view. The lazy-counter invariant (`write_count==0`
  for source-re-readable leaves, alphaC `:348`) is the proof it does not re-open
  AZ-IV eager materialisation (`SYNTHESIS.md:194`).
- **Grammar-neutral verdict:** GENERALISABLE (JSON+CSS exercised, the SK-V17
  witness set, `SYNTHESIS.md:243-264`). The view emitter walks ONE `BackendRule`
  shape for both JSON and CSS (no CSS-keyed branch JSON lacks); `sheets_witness`
  is NOT a valid projection rider (no `.bbnf`/parser/`BackendRule`,
  `SYNTHESIS.md:249-255`), so non-CSS-non-JSON projection is asserted-by-
  construction with proof deferred to SK-V18. Lock-14: the kind-from-source-byte
  decode is the grammar-neutral mechanism; the per-grammar datum is the byte→kind
  table, derived from the grammar.

### D3 — O(1) `offsets.len()` checkpoint + truncate (speculative-descent rollback on the tape)

- **Shape:** the parser checkpoint = capture `offsets.len()` (a `usize` read);
  rollback = `offsets.truncate(mark)` + truncate the sparse flag side-vectors to
  the matching cursor. NO `split_off`, NO `Vec<Vec>` arena, NO per-leaf eager
  payload (`SYNTHESIS.md:179`). This is the already-banked SK-V16 O(1) generic
  checkpoint (`Section 1` Validated: "O(1) generic checkpoint, 20x sound,
  generic, `8153236e8`") applied to the CSS recognizer's speculative Alts.
- **Scalar-ref status:** N/A — vector length capture + truncate.
- **Checkasm-analogue (CH4):** N/A as a SIMD differential. The substrate
  analogue is the rollback round-trip invariant: capturing `mark = offsets.len()`
  then `offsets.truncate(mark)` (plus the matching flag-side-vector truncate)
  must restore the tape to byte-identical state — a property proven by a
  checkpoint/rollback equivalence test (parse-rollback-reparse yields the same
  tape), the same 20x-sound checkpoint banked at `8153236e8`. Oracle = the tape
  state before the speculative descent.
- **Same-wave-consumer (CH4):** PRESENT — the in-wave consumer is the CSS
  recognizer's speculative-Alt control loop (`parse_block`/`parse_block_item`,
  `generated.rs:189/209`), which calls checkpoint before a speculative branch and
  truncate on its failure. CONDITIONAL on the §2-D3 post-CF-1 re-profile: if the
  re-profile does not confirm rollback as a surviving wall, D3 ships only the
  exposed O(1) marker mechanism for the spine to consume opportunistically, not as
  a measured lever.
- **Arch:** architecture-neutral.
- **P1 antecedent:** the recognition control loop (`parse_stylesheet`/
  `parse_block`/`parse_block_item`, 28.87%+2.45% inlined, `generated.rs:118/189/
  209`, P1-E §2.3) is classed by the LOCKED S-P1 profile (HARDENING-S-P1-V4 §3.3)
  as a **recognition control loop**, NOT as measured speculative-checkpoint /
  rollback self-time. The LOCKED profile measured no isolated rollback hot leaf;
  the speculative-rollback share is a HYPOTHESIS about WHERE that control-loop
  self-time goes, not a measured antecedent, and D3 must not assert otherwise.
  D3's standing is therefore CONDITIONAL: after lever-1 (D1, the fact-stream
  String kill / CF-1 tape-append) retires the 82% serialization floor, the
  recognition control loop is the dominant SURVIVING own-compute wall, and a
  **post-CF-1 typed-tape RE-PROFILE** must re-confirm — as an explicit
  S-P1-re-confirm obligation (the same re-profile obligation P2-F CF-3 carries) —
  that the surviving control-loop self-time is in fact checkpoint/rollback before
  D3 is shortlisted as a lever. Absent that re-confirmation D3 supplies only the
  cheap O(1) marker mechanism the substrate must EXPOSE (SYNTHESIS §3 lever-4;
  alphaE C3 — the >lightningcss-cross lever), keeping rollback cheap so the spine
  can speculate without a `split_off` copy; it does not claim a measured
  speculative-rollback hot leaf on the LOCKED profile.
- **Grammar-neutral verdict:** GENERALISABLE. `offsets.len()`/`truncate` is a
  generic `Vec<u32>` operation with no grammar knowledge; JSON's tape uses the
  same checkpoint shape. Lock-14 clean. (NOTE: the *commit-by-construction*
  codegen decision of WHERE to omit checkpoints is alphaE C3 / a codegen
  property, not a substrate primitive — D3 supplies only the cheap O(1)
  marker/truncate mechanism the substrate must expose; it does NOT decide
  placement.)

### D4 — One-shot SIMD-exact offset reservation (`CapacityPlan::OneShotSimd`) to drive logical≈allocated

- **Shape:** size the CSS `TapeBuilder`'s `offsets` capacity in ONE reserve from
  the NEON structural-scan output (`structural_capacity_for(OneShotSimd, src)` =
  `scan_structurals(src).positions().len() + 8`, `json/scan.rs:53`,
  `assembler.rs:26`). This eliminates geometric-grow events (`reserve_offsets_cold`,
  `assembler.rs:87-91`), driving the logical-vs-allocated ratio (§1.3) toward
  ~1.0 with zero re-alloc on the hot path.
- **Scalar-ref status:** PRESENT (the consumed kernel's). The capacity comes from
  `scan_structurals`, whose scalar reference is `scan_structurals_scalar`
  (`json/scan.rs:32`) — the same scalar structural-index oracle P2-C/P2-E's NEON
  candidate is checked against. The reservation op itself is scalar (`Vec::reserve`).
- **Checkasm-analogue (CH4):** PRESENT via the consumed kernel — the structural
  count `scan_structurals(src).positions().len()` is gated by the SAME checkasm
  differential the NEON scan candidate carries (NEON eq-set classifier vs
  `scan_structurals_scalar`, `scalar_parity_report` `json/scan.rs:38` /
  `bbnf-simd/src/lib.rs:130`); an off-by-one in the SIMD count is caught by that
  parity gate before it can mis-size the reserve. The reserve op adds no new
  differential of its own (an over-reserve is harmless capacity; an under-reserve
  falls to the existing cold grow path `reserve_offsets_cold`).
- **Same-wave-consumer (CH4):** PRESENT — the consumer is D1's `push_plain_offset`
  append loop, which fills the reserved capacity with zero grow events. D4 has no
  standalone existence: it is the capacity-plan refinement of D1, gated behind
  D1/D2 (no tape to size) and the P2-C/P2-E NEON scan (no SIMD count). It ships in
  the same wave as its producer (the scan) and its consumer (the append).
- **Arch:** aarch64 NEON for the count (via `select_classifier`); scalar fallback
  count when the alphabet collides at the low-6-bit mask `(byte & 0x3f)`
  (`lo6_table_admissible`, `dispatch.rs:101`, mask computed at `dispatch.rs:106`).
  For the CSS delimiter alphabet the lo6 route is inadmissible (`;`/`{` collide at
  slot 59); the SIMD count therefore comes from the eq-set fan classifier, with
  the scalar structural-index oracle (`scan_structurals_scalar`, `json/scan.rs:32`)
  as the parity reference.
- **P1 antecedent:** the 57.63% allocator floor (P1-E §2.4) is partly grow
  churn; the `allocation_count` counter (`mod.rs:86`) is the proof D4 removes
  grow events (alloc count → 0 after the one-shot reserve). It is the
  capacity-plan half of SYNTHESIS §3 lever-2 (alloc removal).
- **Grammar-neutral verdict:** GENERALISABLE. `CapacityPlan` is grammar-free
  (`assembler.rs:13-40`, env-selected, cold-path read at builder construction);
  the SIMD count reuses the shared `select_classifier(alphabet)` kernel with the
  CSS alphabet as the only per-grammar datum. Lock-14 clean; same kernel as JSON.
  CAVEAT: D4 is gated behind D1/D2 (no tape to size until CSS emits into one) and
  behind the P2-C/P2-E NEON candidate (no SIMD count until the scan is wired);
  on its own it is a refinement, not a standalone lever.

### D5 — Sparse-flag side-table for CSS structural-role bits (NOT a widened per-position record)

- **Shape:** where the CSS projection needs a structural-role bit a source-byte
  re-read cannot recover (e.g. "this `{` opens an at-rule block vs a style-rule
  block" when the byte alone is ambiguous), record it via `patch_flags`
  (`assembler.rs:93-113`) into the EXISTING sparse `flag_cursors`/`flag_values`
  pair — read back by binary search (`flags_at`, `mod.rs:144-150`). The
  per-position record (`offsets[i]`, one u32) is NOT widened; flags are paid only
  where non-zero (`push_offset` `:64`).
- **Scalar-ref status:** N/A — a sparse `Vec` append + binary-search read.
- **Checkasm-analogue (CH4):** N/A as a SIMD differential. The substrate analogue
  is the flag round-trip equivalence: a value written via `patch_flags`
  (`assembler.rs:93`) at cursor `c` must read back identically via `flags_at`
  (`mod.rs:144`) — a write-then-read-back parity test, with the additional
  invariant that flags are appended in strictly increasing cursor order
  (`debug_assert`, `assembler.rs:105-110`) so the binary search is sound. Oracle =
  the bit the projection intended to record.
- **Same-wave-consumer (CH4):** PRESENT — the consumer is D2's lazy `ValueRef`
  projection, which reads the flag via `flags_at` to disambiguate a kind a
  source-byte re-read alone cannot recover. D5 is dead weight without D2 reading
  it; it ships only if D2 surfaces an ambiguous-`{` (or analogous) kind that needs
  a stored disambiguation bit, and ONLY as a `BackendRule` branch-tag projection
  (the §2-D5 guard), never a hand-curated per-rule catalogue.
- **Arch:** architecture-neutral.
- **P1 antecedent:** this is the substrate mechanism that lets D2's lazy
  projection avoid re-walking source to disambiguate kinds — keeping the typed
  view cheap so it stays below the `emit_fact_stream` 24.59% (`generated.rs:5`)
  it replaces. The 2-bit `OffsetFlags::GRAMMAR_BIT0/BIT1` slots already exist
  (`mod.rs:22-23`).
- **Grammar-neutral verdict:** GENERALISABLE-WITH-GUARD. The sparse-flag
  mechanism is grammar-free (it stores opaque `u8` bits keyed by cursor). The
  RISK is Lock-14: if the flag SEMANTICS become a per-rule-id catalogue (a
  relocated `W5C_REQUEST_FACT_PROFILES` in flag form), it is JSON-overfit and
  CH2 must REVISE it. The guard: each flag bit must name the `.bbnf` rule /
  `BackendRule` branch tag it derives from (`SYNTHESIS.md:111` "every residual
  CSS routing entry names the `.bbnf` rule it derives from"). Verdict: admissible
  only if the bit is a `BackendRule` branch-tag projection, NOT a hand-curated
  per-rule constant. Flagged for P2-F.

### D6 (REJECT-on-sight, recorded for CH3/CH5 completeness) — NO second substrate / sidecar

- **Shape:** any `StructLayout`/`TapeStructBuilder`/`TapeCursor`, retained class
  column, sidecar event vector, aux density table, retained cursor/list, parallel
  source pass, public `UnionTape`, or cross-call classifier-state carry.
- **P1 antecedent:** NONE — and that is the point. P1-E §2.5 shows zero tape leaf
  on the CSS path; the fix is to WIRE the existing tape, not to add a second one.
- **Grammar-neutral verdict:** N/A — REJECTED by Lock 1 (`SYNTHESIS.md:228-239`,
  `LOCKS.md:75,126,137-149`). Recorded here so the §4 risk ledger and the
  CHALLENGE CH5 lens have an explicit anchor: P2-D proposes NO such construct.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Cand | Mechanism (grammar-free?) | Per-grammar datum | Exercised riders | Verdict |
|---|---|---|---|---|
| D1 `push_plain_offset` | YES — `usize`→u32 append, no grammar knowledge (`assembler.rs:71`) | which positions push (from `BackendRule` shape, `lower/offset_tape.rs`) | JSON (live) + CSS (new) | GENERALISABLE; Lock-14 clean |
| D2 lazy `ValueRef` view | YES — cursor + source-byte `at_cursor` decode (`json/value.rs:29-47,143`) | byte→kind table (from grammar) | JSON (live `value_from_ref`) + CSS (new rider) | GENERALISABLE; non-JSON-non-CSS = SK-V18 (Sheets has no `BackendRule`, `SYNTHESIS.md:249-255`) |
| D3 O(1) checkpoint/truncate | YES — `Vec::len`/`truncate` (`assembler.rs`, banked `8153236e8`) | none (placement is a codegen property, not this op) | JSON + CSS | GENERALISABLE; Lock-14 clean; lever-status CONDITIONAL on post-CF-1 typed-tape re-profile (S-P1-re-confirm obligation; not a measured-rollback hot leaf on the LOCKED profile) |
| D4 one-shot SIMD reserve | YES — `CapacityPlan` env-selected, cold (`assembler.rs:13-40`) | scan alphabet (shared `select_classifier`) | JSON (`json/scan.rs:53`) + CSS | GENERALISABLE; gated behind D1/D2 + NEON scan |
| D5 sparse-flag side-table | YES mechanism / GUARDED semantics (`assembler.rs:93-113`) | flag bit MUST be a `BackendRule` branch-tag, not a per-rule constant | JSON + CSS | GENERALISABLE-WITH-GUARD; CH2 REVISE if semantics become a per-rule catalogue |
| D6 second substrate | — | — | — | REJECT (Lock 1) |

Cross-cutting: every candidate's only per-grammar input is the
alphabet / `BackendRule` shape / byte→kind table — all DERIVED from the `.bbnf`
grammar, never a hand-coded per-rule branch. The substrate ops themselves
(append, cursor read, checkpoint, reserve, sparse-flag) are the SAME ops JSON
rides today. This is the Lock-14 "grammar lives in the grammar" posture
(`PASS-2-RESEARCH.md` §8.5): JSON structural roles never enter the substrate; the
substrate stores positions and the grammar's projection decodes them.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- **AZ-IV eager-value-tree (118x):** D2 is the direct counter-design (lazy view,
  zero payload for source-re-readable leaves), but it RE-OPENS AZ-IV if the CSS
  view eagerly materialises a typed tree or `Box`es per leaf. GUARD: the
  `PayloadArena` `write_count`/`allocation_count` counters (`mod.rs:80-88`) must
  be ~0 on the structural path; any per-leaf `Box::new`/f64-alloc is the
  regression (`SYNTHESIS.md:194`, alphaC ledger row 1 `:348`).
- **StructRegistry / Arena<G> / Builder<G> indirection (28-65x; 983x css
  bootstrap; 10583x tailwind WATCHDOG):** NO candidate adds a registry lookup in
  the per-leaf hot path. `TapeBuilder` stays the single non-generic sink
  (`SYNTHESIS.md:198`); D1's "generic over the tape sink" means generic over a
  trait the codegen monomorphises, NOT a runtime `Arena<G>` dispatch.
- **CSS fact-stream String as admission plane:** D1 RETIRES `emit_fact_stream`
  (`generated.rs:5`) as the output; it must not survive as an admission surface
  (diagnostic-only, `SYNTHESIS.md` §0.4 pre-block 3).
- **`W5C_REQUEST_FACT_PROFILES` (Lock-14 phrase-#1 construct):** D1's wiring must
  derive CSS routing from `BackendRule`, not relocate the hand-coded array into
  the tape lowering or into D5's flag semantics
  (`SYNTHESIS.md:203-208`,`:111`).
- **Second substrate / sidecar / retained cursor / class column / cross-call
  classifier carry (Lock 1):** D6 is the explicit no-go list
  (`SYNTHESIS.md:228-239`, `LOCKS.md:75,126,137-149`). A skinny
  `StructLayout`/`TapeStructBuilder`/`TapeCursor` is a SECOND substrate and is
  REJECTed; the projection emits accessors over the EXISTING `Tape`/`ValueRef`.
- **`push_ascii_lower_hex` FNV hex (9.11%, `generated.rs:628`):** P1-E §2.4 marks
  it a diagnostic, NOT a primitive. No candidate wires FNV as a runtime
  selector/arbiter/correctness proof (FNV stays bench-only,
  `SYNTHESIS.md:212-215`; alphaC `:285`). D-candidates do not touch it; retiring
  `emit_fact_stream` (D1) retires its 9.11% caller along with it.
- **udot/i8mm digit kernel (`digit_mac.rs:27`):** NO benched-CSS antecedent on
  either profiled plane (P1-E §2.5 "No number leaf is hot"; §4 anomaly 4). It is
  NOT a P2-D substrate candidate; the `PayloadArena` decode path (D2) is where a
  digit decode would land IF a future typed-path re-profile proves the digit leaf
  hot — that is C4a/C4b's gated concern (`SYNTHESIS.md:182`), re-profiled on the
  NEW typed path, never inherited here.
- **No-warm-benches / N>=50 cold:** any tape-shape Mbps claim is cold per-parse,
  N>=50 median (`css_canon_bench.rs:146,250`); the `write_count`/`allocation_count`
  counters compile only under `bench-counters` (`mod.rs:40`), so they are
  measured in a counter build, not the timed release build (no instrumentation in
  the hot path, clean-instrumentation feedback).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- Benched tape substrate (the subject; all `file:line` verified this cycle):
  `skinny/crates/runtime/src/tape/mod.rs` (`PayloadArena` :38, `write_bytes`
  :65-78, `write_count` :80-83, `allocation_count` :85-88, `Tape` :94, members
  :95-100, `offset_at` :138-142, `flags_at` :144-150, `offset_bytes` :152-154,
  `offset_capacity_bytes` :160-164, `ValueRef` :175, `offset()` :218-222,
  `DocumentView` :227-232, `OffsetFlags` :16-36); `tape/assembler.rs`
  (`CapacityPlan` :13-40, `TapeBuilder` :42, `new` :50-59, `push_offset` :61-68,
  `push_plain_offset` :71-85, `reserve_offsets_cold` :87-91, `patch_flags`
  :93-113, `finish` :115-123); `tape/offsets.rs` (`OffsetTapeStats` :1-6).
- JSON projection isomorph (the model the CSS rider mirrors):
  `skinny/crates/runtime/src/grammars/json/value.rs` (`JsonNodeKind` :12-26,
  `at_cursor` source-byte decode :29-47, `value_from_ref` :143-173,
  `JsonToken`/payload fields :104-111); `json/scan.rs` (`STRUCTURAL_ALPHABET` :7,
  `scan_structurals` NEON :22-30, `scan_structurals_scalar` :32-36,
  `scalar_parity_report` :38-45, `structural_capacity_for` :48-55).
- Codegen lowering seam (the layout-driven projection vehicle):
  `skinny/crates/codegen/src/lower/tape_plan.rs` (`TapeFlavor` :5, `render_rule`
  :58); `lower/offset_tape.rs`; the `lower/` shape inventory
  (`collapsed_stage/eager_tape/event_tape/schema_direct/sink_only/rust`).
- NEON dispatch (the consumed kernel for D4): `skinny/crates/bbnf-simd/src/
  dispatch.rs` (`select_classifier` :42, `lo6_table_admissible` :101).
- S-P1 profile (every hot-leaf antecedent): `restart/skinny/tranches/sk-v17/
  research/p1/p1e-hot-leaf-attribution.md` §2.3 (recognition: `find_component_delim`
  :288 56.52%, `consume_balanced_at` :320 11.05%), §2.4 (fact_stream:
  `emit_fact_stream` :5 24.59% + 57.63% allocator floor, 91.44% from
  `emit_fact_stream`, `push_ascii_lower_hex` :628 9.11%), §2.5 (roll-up; "no tape
  leaf"), §4 (anomalies 2/3/4); `research/p1/hardening/
  HARDENING-S-P1-V4-CONSOLIDATED.md` §3.1 (instr/byte: fact_stream 214.56–364.51
  vs full_parse 46.46–57.72; within-harness ratios).
- Contract + locks: `restart/skinny/tranches/sk-v17/SYNTHESIS.md` (§0.1 Tape
  activation / Layout-driven projection / preserve-rich-ast gates :110-113,
  §0.3 receiver obligations :178-179, §0.4 pre-blocks + generality clause
  :185-264, §3 four-lever trajectory); `restart/locks/LOCKS.md` (Lock 1
  substrate-union :75, v+1 manifest :118-127, v+1 no-cross-call-carry :137-149;
  Lock 14 grammar-generalisation :603); alphaC-redress-digest.md (§1 PayloadArena
  lazy-counter invariant :87,:348,:366; substrate row :31).
- CSS recognizer source (the parser the tape replaces): `skinny/crates/runtime/
  src/grammars/css_l4_declaration_values/generated.rs` (`emit_fact_stream` :5,
  `emit_full_parse` :61, `find_component_delim` :288, `consume_balanced_at` :320,
  `push_ascii_lower_hex` :628); seam-flip site `skinny/xtask/src/regen_css.rs`
  (seven `RequestFactsProfile` literals :45,63,81,99,117,135,153), retire target
  `skinny/crates/codegen/src/lib.rs:336` (`W5C_REQUEST_FACT_PROFILES`).
- Prior tranche: `restart/audit/skinny-impl-overfit/sk-v16-w6tape-report.md`
  (substrate landed, UNWIRED for CSS), `sk-v16-w6tape-conversion-report.md`
  (:55 monomorphised-to-named-builder seam; borrowed-slice-vs-lazy directive).
- Host: Apple M5 Max, aarch64-apple-darwin. Master HEAD `0ae1caa52`; P1 baseline
  `6496fecae`; SK-V16 close `1c5bd7a25`. No x86 / no SVE.
