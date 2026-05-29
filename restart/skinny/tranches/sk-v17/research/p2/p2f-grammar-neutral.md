# SK-V17 P2-F: Grammar-Neutral Abstraction

Pass: S-P2 Research. Cycle: V3.
Date: 2026-05-29.
Scope: For every candidate primitive in the S-P2 pool (alphaE C0–C4b + the S-P1
hot-leaf-derived kernels P2-B/C/D/E formalize), the grammar-neutral abstraction —
how each generalises beyond CSS/JSON to Sheets / BBNF-self per Lock 14, expressed as
a grammar-neutral byte-set / classifier / tape operation or flagged JSON/CSS-overfit.
Output: this file.
P1 hot-leaf antecedents: `find_component_delim` 56.52% (`css_l4_declaration_values/generated.rs:288`) + `consume_balanced_at` 11.05% (`:320`) = ONE byte-class-membership scan leaf (P1-E §2.3/§2.5); `emit_fact_stream` 24.59% + ~57.63% syscall+heap floor 91.44%-reached-from-it (P1-E §2.4) = String-materialization leaf; NO `number`/`unicode`/`dispatch`/`tape` hot leaf on either benched plane (P1-E §2.5 / §4.4).
Lock surface: Lock 14 (grammar lives in the grammar — every candidate grammar-neutral or re-framed per-grammar-template/host-fn) primary; Lock 1 (substrate union — the tape append + projection are ONE substrate) secondary (P2-D owns it, P2-F confirms candidates do not split it).

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

### 1.1 The grammar-neutral vehicle already exists and is grammar-parametric by construction

The S-P1 profile names exactly one hot scan leaf reached two ways. The vehicle that
generalises it is in `skinny/crates/bbnf-simd/src/`, and it is already a per-grammar
*alphabet* abstraction, not a JSON construct:

- `select_classifier(alphabet: &'static [u8; 64]) -> SelectedClassifier`
  (`dispatch.rs:42`). The alphabet is the *only* grammar-specific datum; the kernel is
  shared. JSON passes its alphabet; CSS would pass its delimiter set. This is the
  Lock-14 neutrality vehicle (the alphabet is caller data, not a hardcoded JSON
  constant — Lock 14, `LOCKS.md:393-395` "delimiter… policy must come from generated
  grammar config or caller data, not hardcoded JSON/CSS constants").
- `StructuralAlphabet::from_bytes(bytes: &[u8])` (`lib.rs:25`) builds a `[bool; 256]`
  membership table from any byte set. JSON's is `b"{}[],:\""` (`json/scan.rs:5`). A CSS
  alphabet is the same call with a CSS byte set. The abstraction carries the grammar's
  byte set as data; it has no JSON-keyed branch.
- `ValueRef<'doc, 'input, K = AnyKind, G: EventGrammar = AnyGrammar>` (`mod.rs:175`) is
  **generic over the grammar `G`**. The cursor type is grammar-parametric by
  construction — the tape append + lazy view substrate is not JSON-shaped; JSON's
  `value_from_ref` (`json/value.rs:143`) is one instantiation over the JSON node-kind
  enum, and a CSS `value_from_ref` is the isomorphic instantiation over the CSS node-kind
  enum. Same tape, same `ValueRef`, different node-kind projection — exactly the Lock-1
  "per-grammar runtime modules emit accessors; one materialisation surface" shape
  (`LOCKS.md:75`).

The grammar-neutrality of the S-P2 candidate pool therefore reduces to one question
per candidate: **is the only grammar-specific datum a byte-set / alphabet / node-kind
enum (data, generalisable), or does the candidate need a JSON/CSS structural-role
branch in a generic crate (overfit, REVISE/REJECT)?**

### 1.2 The lo6-table backend does NOT transfer to the CSS alphabet — the load-bearing neutrality split

This is the most consequential P2-F finding and it bears directly on C2's verdict.

`select_classifier` has two backends (`dispatch.rs:11-15`): `Scalar` and `NeonTbl4`.
`NeonTbl4` is selected only when `lo6_table_admissible(alphabet)` returns true
(`dispatch.rs:90-99`), which requires every alphabet byte to occupy a **distinct** slot
under the low-6-bit `(byte & 0x3f)` mask (`dispatch.rs:101-113`, `let slot = (byte &
0x3f)` at `:106` — a bitmask, NOT a modulo). JSON's alphabet is admissible (`b"{}[],:\""`,
7 bytes, all distinct under `& 0x3f` — verified). **The CSS structural alphabet is NOT.**
Measured this cycle:

| Alphabet | bytes | lo6-admissible? | first collision |
|---|---|---|---|
| JSON `{}[],:"` | 7 | **yes** | — |
| CSS struct `;{}():,` | 7 | **no** | `;`(0x3b)→slot 59 == `{`(0x7b)→slot 59 (under `& 0x3f`) |
| CSS decl `;{}` | 3 | **no** | same `;`/`{` collision |
| CSS colon `:{};` | 4 | **no** | same |
| CSS find-delim full `;{}():,'"/[]* ` | 14 | **no** | `;`/`{` collision |

`;` (0x3b) and `{` (0x7b) both map to lo6 slot 59 under the `& 0x3f` mask (0x3b & 0x3f =
0x3b = 59; 0x7b & 0x3f = 0x3b = 59 — note this collision is specific to the *bitmask*: a
true modulo `0x7b % 0x3f = 0x3c = 60` would NOT collide, so the mechanism is the low-6-bit
fold, not arithmetic reduction) — every CSS delimiter set the hot
scan uses (`find_component_delim`'s `b";{}"`/`b":{};"`, `generated.rs:293,313` and the
`consume_balanced_at` close-byte family) contains the `;{` pair, so the JSON NEON
`classify_tbl4` lo6-table path (`json/scan.rs:9-30`) is **structurally inadmissible for
CSS** and `select_classifier` correctly falls back to scalar. Consequence: the
*interface* `select_classifier(alphabet)` is grammar-neutral, but the JSON NEON
*backend* is not transferable to CSS. CSS must route through the collision-free
membership primitive (`byte_class_from_table_64`, a full `[u8; 256]` table — no low-6-bit
`& 0x3f` folding — `dispatch.rs:51`, `StructuralAlphabet::class_table` `lib.rs:43`). This is
grammar-neutral (256-table membership has no collision for any byte set), but the NEON
impl is **a scalar passthrough today** (`byte_class_from_table_64_neon` calls
`byte_class_from_table_64_scalar`, `aarch64/byte_class_from_table_64.rs:1-4`). So the
genuine grammar-neutral SIMD primitive CSS needs (a real `vqtbl`-class NEON 256-table
classifier, or a multi-`vceqq` eq-set classifier via `byte_class_from_eq_set_64`,
`tests/checkasm_byte_class_from_eq_set_64.rs`) is the parse-that gap P2-E/P2-C own —
P2-F's contribution is to certify that the correct framing is **a grammar-neutral
byte-set primitive (256-table OR eq-set), NOT the JSON lo6-table reused on CSS**. A
candidate that proposed "reuse JSON `classify_tbl4` for CSS" would be silently wrong
(it would fall back to scalar and claim a SIMD win it never got) — flagged here so CH2
can hold C2 to the admissible primitive.

### 1.3 The dominant CSS cost (String materialization) is intrinsically grammar-neutral

P1-E §2.4 attributes ~58% of fact-stream wall time to the syscall+heap allocator floor,
91.44% of it reached from `emit_fact_stream`'s `String` growth (`generated.rs:5`), plus
24.59% in the `push_str` accumulator itself. This is `String`/`Vec` capacity growth —
*not CSS-specific logic* (P1-E §4 anomaly 2 states it explicitly: "it is `String`
`push_str` growth, not CSS-specific logic"). The intervention that retires it (tape
append `push_plain_offset` = one branchless u32 write, `assembler.rs:71`) is the SAME
`TapeBuilder` JSON already rides (`Tape` `mod.rs:94`). The grammar-specific datum is the
node-kind enum the cursor projects, not the append op. So the highest-value candidate
(C0/C1 de-fact-stream onto the tape) is grammar-neutral at its core by construction —
the only re-framing risk is the *routing* (which `.bbnf` rule maps to which tape op),
which §1.4 addresses.

### 1.4 The overfit re-entry seam is the routing, and Lock 14 names it precisely

The one place a CSS-overfit could re-enter is the routing array: the hand-coded
`W5C_REQUEST_FACT_PROFILES` const (`codegen/src/lib.rs:336`) and the per-rule routing it
encodes (declaration/selector/aggregate/numeric/function/color rule sets, 0/1/N
value-list collapse, hex packing, color-component order — SYNTHESIS §0.1 Layout row).
Lock 14 phrase #1 (`LOCKS.md:603` "may not hand-code … profile arrays, CSS profile
matches") makes this const itself a violation SK-V17 must RETIRE, deriving routing from
the `.bbnf`/`BackendRule` shape, NOT relocating the per-rule branching into projection
DATA (the explicit Lock-14 trap, alphaC §, SYNTHESIS §0.4 "relocating its per-rule
branching into projection DATA is the overfit re-entry seam and is forbidden"). P2-F's
grammar-neutral verdict for C0/C1 is therefore conditional on the routing deriving from
the grammar shape (every residual CSS routing entry naming the `.bbnf` rule it derives
from), which is the SYNTHESIS Layout close gate already.

### 1.5 The witnessed-grammar bound: JSON + CSS, NOT four-grammar

Lock 14 phrase #2 (`LOCKS.md:386-387`) holds: "With only one of Sheets or BBNF-self, the
claim is scoped to the witnessed grammars." The SK-V17 witnesses are JSON (existing
tape-wired `value_from_ref`) + CSS (the new rich rider). `sheets_witness` is a 24-line
`EventGrammar` byte-classification trait impl with NO `.bbnf` / parser / `BackendRule`
to project from, and codegen fail-closes `google_sheets`/`bbnf` as negative controls
(`codegen/src/lib.rs:1075-1090`); BBNF-self is absent (SYNTHESIS §0.4 generality clause).
So every grammar-neutral verdict below is **JSON+CSS-witnessed, asserted-by-construction
for Sheets/BBNF-self with the proof deferred to SK-V18** — no candidate may carry
fleet-wide/four-grammar wording. The NEON leaf's non-JSON exercise IS `css_l4` (a real
rider sharing the kernel), which is genuinely dischargeable and distinct from the
projection-generator bound (SYNTHESIS §0.4 last paragraph).

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent + grammar-neutral verdict)

The pool is the alphaE shortlist C0–C4b (the S-P2 candidate source until P2-B/C/D/E
formalize; folded against P2-B/C/D/E in the V1 CHALLENGE cycle). Each row carries: shape
/ scalar-ref status / arch / P1 antecedent / grammar-neutral verdict.

### CF-1 ← C0/C1: tape-append materialization op (`push_plain_offset`) + grammar-parametric lazy `ValueRef` projection
- **Shape.** Replace the `String` fact-stream accumulator with `TapeBuilder` Open/Close/Leaf
  appends (`push_plain_offset`, one branchless u32 write into a `Vec<u32>`,
  `assembler.rs:71`); reconstruct the typed CSSOM on demand via a `ValueRef` cursor view
  isomorphic to JSON's `value_from_ref` (`json/value.rs:143`), over the SAME
  `Tape`/`ValueRef` (`mod.rs:94,175`). Checkpoint = `offsets.len()` marker; rollback =
  truncate. No new cursor/builder type.
- **Scalar-ref status.** N/A — data-structure migration / codegen route, not a SIMD/ASM
  kernel. (CH4's scalar-ref requirement binds *primitives*; this is the substrate the
  primitives feed.) The append + cursor read are scalar branchless ops, already
  exercised by JSON 51/51.
- **Checkasm-analogue (CH4).** N/A as a SIMD differential; the correctness analogue is
  the tape↔fact_stream corpus-parity test (the existing `corpus_parity.rs` shape extended
  to CSS) + the cssparser 8-field structural equality (rules=10136/style=9561/sel=9561/
  decls=20043, EXACT, `1c5bd7a25`) — the tape-append output round-trips to the same logical
  document as the retired fact-stream String, with `PayloadArena.write_count==0` on
  source-re-readable leaves (`mod.rs:80-88`).
- **Same-wave-consumer (CH4).** PRESENT — the lazy `ValueRef` projection (the CSS
  `value_from_ref`-isomorph) is the in-wave consumer of the tape CF-1 appends; the append
  and the projection are the same substrate (Lock 1) and land together or neither. (This
  is the same CF-1↔projection pairing P2-A CP-A2↔CP-A3 and P2-D D1↔D2 already name.)
- **Arch.** Architecture-neutral (no NEON/aarch64 dependency in the append/cursor path).
- **P1 antecedent.** `emit_fact_stream` 24.59% self-time + the ~57.63% syscall+heap floor
  91.44%-reached-from-`emit_fact_stream` (P1-E §2.4 / §2.5 / §4 anomaly 2). This is the
  single dominant benched-CSS-Track-1 cost; the tape append directly retires it.
- **Grammar-neutral verdict.** **GRAMMAR-NEUTRAL (JSON+CSS-witnessed), conditional on
  §1.4.** The append op is grammar-agnostic; `ValueRef` is generic over `G: EventGrammar`
  (`mod.rs:175`); JSON is the existing witness, CSS the new rider, one view emitter
  walking one `BackendRule` shape for both. The ONLY grammar-specific datum is the
  node-kind enum the projection decodes — which is generated per-grammar from the
  `.bbnf`/`BackendRule` shape, the legitimate per-grammar-template surface (Lock 14
  phrase: generated provider manifests are admissible). **Condition (else REVISE):** the
  routing must derive from the grammar shape — `W5C_REQUEST_FACT_PROFILES` retired
  (`lib.rs:336`), no per-rule-id match arms in generic crates JSON does not need, every
  residual CSS routing entry naming its `.bbnf` rule (§1.4). If the per-rule branching is
  relocated into projection DATA instead of derived, this candidate becomes JSON/CSS
  overfit and CH2 marks it REVISE.

### CF-2 ← C2: `byte_class_index_64` structural-membership classifier over a per-grammar alphabet
- **Shape.** Route the CSS structural scan through `select_classifier(alphabet)`
  (`dispatch.rs:42`), producing a `Vec<u32>` structural index the CF-1 tape consumes —
  identical to JSON's `scan_structurals` (`json/scan.rs:22`). The grammar-specific datum
  is a `StructuralAlphabet::from_bytes(<grammar byte set>)` (`lib.rs:25`); the kernel is
  shared. **Per §1.2 the admissible primitive for CSS is the collision-free 256-table
  `byte_class_from_table_64` (`dispatch.rs:51`) OR a multi-`vceqq` eq-set classifier
  (`byte_class_from_eq_set_64`), NOT the JSON lo6 `classify_tbl4`** (the CSS alphabet
  collides under the low-6-bit `& 0x3f` mask, so the lo6 backend falls back to scalar).
- **Scalar-ref status.** PRESENT for the routing (`scalar::classify_chunk` `dispatch.rs:19`,
  `scan_structurals_scalar` `json/scan.rs:32`) and for membership
  (`byte_class_from_table_64_scalar`, `tests/checkasm_byte_class_from_table_64.rs`;
  `byte_class_from_eq_set_64` scalar twin, `tests/checkasm_byte_class_from_eq_set_64.rs`).
  **GAP (P2-E/P2-C own):** the *NEON* `byte_class_from_table_64_neon` is a scalar
  passthrough today (`aarch64/byte_class_from_table_64.rs:1-4`) — a real vectorized
  256-table or eq-set NEON impl is the missing primitive. The scalar reference exists; the
  vectorized form does not yet earn its keep.
- **Checkasm-parity (CH4).** REQUIRED-NEW for the vectorized form —
  `checkasm_byte_class_from_eq_set_64` (the scalar twin is the oracle; the NEON eq-set fan
  is the differential). The scalar reference exists; the vectorized form does not yet earn
  its keep (`aarch64/byte_class_from_table_64.rs:1-4` passthrough).
- **Same-wave-consumer (CH4).** PRESENT — CF-1's tape build (the `Vec<u32>` index this
  classifier emits is the tape's offsets); identical to the consumer P2-C C2 (`p2c:150`)
  and P2-F's own §1.2 framing name. Neither the scan nor the tape ships without the other.
- **Arch.** aarch64 NEON (the host ISA per P1-E §1; NO x86/SVE). The eq-set route
  (`vceqq_u8` per delimiter, OR-reduced) is the AdvSIMD-clean path for a sub-8-byte CSS
  alphabet; checkasm-gated (`checkasm_byte_class_from_eq_set_64`).
- **P1 antecedent.** `find_component_delim` 56.52% + `consume_balanced_at` 11.05% = ~68% in
  ONE byte-class-membership scan inner loop (`generated.rs:288`/`:320`, the
  `delimiters.contains(&byte)` + per-byte `match` — P1-E §2.3/§2.5). RE-CONFIRMED on the
  benched skinny path this profile cycle (P1-E §4 anomaly 3); not inherited.
- **Grammar-neutral verdict.** **GRAMMAR-NEUTRAL (JSON+CSS-witnessed) at the interface;
  the BACKEND choice is the neutrality crux.** The `select_classifier(alphabet)` interface
  is the Lock-14 vehicle (alphabet = caller data). JSON is the exercised non-CSS witness
  (`json/scan.rs` already routes here). **REVISE-on-framing flag for CH2:** the candidate
  must name the *admissible* primitive (256-table OR eq-set, §1.2), NOT "reuse JSON's
  lo6 `classify_tbl4`" — the lo6 path is JSON-alphabet-overfit (admissible only for
  alphabets distinct under the low-6-bit `& 0x3f` mask) and would silently scalar-fall-back on CSS. With the
  admissible primitive named, this is fully grammar-neutral: one kernel, per-grammar
  alphabet, witnessed by JSON+CSS. `lo6_table_admissible` (`dispatch.rs:101`) is itself
  the grammar-neutral guard that makes the fallback honest — a CSS special-case would be
  to hand-pick a non-colliding subset; the honest answer is the 256-table/eq-set
  primitive.

### CF-3 ← C3: commit-by-construction Alt-mode codegen property (non-depositing-Alt detection)
- **Shape.** Generic codegen property: the emitter emits NO speculative checkpoint for
  pure-lexical keyword-dispatch Alts that deposit nothing structural; the spine commits
  as it scans, driven by the CF-2 structural index. Backtracking survives only on true
  ambiguous leaves. Owner: `codegen/src/lower/tape_plan.rs` (`AltMode`),
  `lower/{offset_tape,event_tape}.rs`.
- **Scalar-ref status.** N/A — codegen control-flow change, not a kernel.
- **Checkasm-analogue (CH4).** N/A as a SIMD differential (codegen property). Parity =
  the recognizer output equality WITH and WITHOUT the commit-by-construction Alt-mode pass
  (the same observational-equivalence the cssparser 8-field oracle anchors); a non-depositing
  Alt that omits a checkpoint must produce a byte-identical tape to one that checkpoints.
- **Same-wave-consumer (CH4).** the CSS recognizer spine post-CF-1 (the speculative-Alt
  control loop CF-1's tape exposes the O(1) checkpoint/truncate for); GATED on the hard
  post-CF-1 typed-tape re-profile CF-3 already self-flags below — the SWC names that
  re-profile as the admission gate, not a measured live consumer on the LOCKED profile.
- **Arch.** Architecture-neutral (codegen).
- **P1 antecedent.** **NO MEASURED SPECULATIVE-ROLLBACK HOT LEAF — the antecedent is a
  post-CF-1 re-profile obligation, not a LOCKED-profile measurement.** This must be stated
  exactly: P1-E measured ZERO speculative own-compute / checkpoint / rollback self-time on
  either benched plane. What the LOCKED profile DID measure on `track1_full_parse` is 56.52%
  scan + a **recognition control loop** at 28.87% (driver frame; `emit_full_parse`/
  `parse_stylesheet`/`parse_block_item` `#[inline]`-collapsed, the recognition outer loop,
  P1-E §3.3 `:182`) + a **block dispatch loop** at 2.45% (`parse_block`, `generated.rs:189`,
  P1-E §3.3 `:184`) — P1-E §3.3 explicitly classes both as `structural`/recognition control,
  NOT speculative checkpoint/rollback self-time, and §4 anomaly 1 re-affirms the framing.
  The "~31% speculative checkpoint/rollback" figure alphaE C3 carries is a **core-tree**
  number (alphaE C3:366 self-flags "S-P1-re-confirm on benched path") and is explicitly NOT
  re-confirmed here; CF-3 does NOT claim it. **The only plane on which a speculative
  checkpoint/rollback leaf could appear is the typed-tape path AFTER the fact-stream alloc
  floor falls (post-CF-1) — a plane that does not exist yet and P1-E could not measure.**
  CF-3 therefore carries a hard, blocking S-P1-re-confirm obligation: re-profile the typed
  `Tape`/`ValueRef` path after CF-1 lands (per `actual-profiling`, N≥50), and admit CF-3 to
  the shortlist ONLY if that re-profile surfaces speculative checkpoint/rollback (or the
  28.87%+2.45% recognition control loop, now un-masked by the retired alloc floor) as a
  top-N self-time leaf. CH1 must hold CF-3 to this obligation and must NOT treat the LOCKED
  28.87%+2.45% recognition-control figures as a measured speculative-rollback antecedent.
- **Grammar-neutral verdict.** **GRAMMAR-NEUTRAL (codegen property, JSON+CSS-witnessed).**
  Non-depositing-Alt detection is a structural property of the grammar's Alt nodes, derived
  at generation time from the `BackendRule` shape — not CSS-keyed. JSON's spine is the
  witness (already single-pass-ish); the Alt-mode pass runs over the JSON grammar and emits
  identical commit-mode where JSON Alts are non-depositing, proving grammar-shape-driven.
  No grammar-specific branch.

### CF-4a ← C4a: wire the orphan `digit_mac` udot 4-digit scan into a number leaf
- **Shape.** Wire the banked `parse_4_digits` / `parse_4_digits_dotprod` udot kernel
  (`aarch64/digit_mac.rs:5,27`) — scalar fallback + `udot` asm both present — into a CSS
  number leaf via dispatch.
- **Scalar-ref status.** PRESENT — `parse_4_digits` carries the scalar byte loop
  (`#[cfg(not(target_feature="dotprod"))]`, `digit_mac.rs:15-22`); the dotprod path is
  compile-time-selected (`:10-13`).
- **Checkasm-parity (CH4).** REQUIRED-NEW — `checkasm_digit_mac` (udot==scalar
  byte-exact); verified ABSENT this cycle (`ls tests/ | grep digit` empty). The kernel is
  banked; the gate is not.
- **Same-wave-consumer (CH4).** NONE on either current CSS plane — orphan-gated; admits
  ONLY after a post-CF-1 typed-`ValueRef` dimension-decode re-profile names a digit leaf
  top-N (P1-E §4.4a). Carried as a gated contingency, NOT an active candidate. (This is the
  PRESENT-AND-NAMED-NONE form V1 CH4 required of G4; the same canonical orphan disposition
  as P2-B C-B3, P2-C C5, P2-E G4.)
- **Arch.** aarch64 dotprod (`udot`, compile-time `target_feature`, NOT runtime-detected).
- **P1 antecedent.** **ORPHAN — NO benched CSS antecedent.** P1-E §2.5 / §4 anomaly 4(a):
  there is ZERO `number`/digit hot leaf on either benched CSS plane — the CSS recognition
  path COUNTS, it does not DECODE dimensions (`generated.rs:91-99`), and the fact-stream
  plane is dominated by String materialization, not digit parse. The udot kernel
  (`digit_mac.rs:27`) is the orphan P1 explicitly names (§4.4(a): "no benched CSS
  antecedent … C4b stays orphan-blocked on the current planes"). The digit leaf MAY
  resurface once the typed lazy-`ValueRef` path decodes dimensions (a NEW plane that does
  not exist yet), which is the gated re-profile.
- **Grammar-neutral verdict.** **GRAMMAR-NEUTRAL (a generic 4-digit-run decode kernel) BUT
  CURRENTLY ORPHAN — CH1 flag.** The kernel is grammar-neutral: a 4-ASCII-digit→u32 decode
  is the `byte_class_from_range_64` digit-run family (Lock 14 `LOCKS.md:426-431` names
  digit-run as a load-bearing grammar-neutral generalisation). It is NOT CSS-overfit. BUT
  per CH1 (a candidate with no P1 antecedent is a speculative kernel — REJECT) it has NO
  benched CSS hot leaf today. Disposition: admits ONLY as an unconditional orphan
  *retirement* of an already-banked kernel (the kernel exists; wiring + a non-CSS consumer
  retires the orphan), and ONLY if a post-CF-1 re-profile of the typed `ValueRef` dimension
  decode shows a digit leaf. Without that re-profile it has no benched CSS antecedent and
  is JSON-neutral-but-CSS-orphan; flag for S-P3 to gate behind the re-profile, not ship
  speculatively.

### CF-4b ← C4b: NET-NEW runtime-detected i8mm digit/dimension kernel (GATED)
- **Shape.** A net-new `#[target_feature(enable="i8mm")]` kernel + scalar twin, runtime-
  detected through the `PrimitiveKernels` OnceLock fn-table (`dispatch.rs:50`, one
  detection at table-init, never in the per-leaf hot loop). i8mm is grep-clean-absent from
  skinny (alphaE 0-anchor: `is_aarch64_feature_detected!("i8mm")` = none).
- **Scalar-ref status.** REQUIRED, NET-NEW — the i8mm kernel MUST land with its scalar twin
  (i8mm==scalar byte-exact). Neither exists today.
- **Checkasm-parity (CH4).** REQUIRED-NET-NEW — a new `checkasm_i8mm_*` test (i8mm==scalar
  byte-exact); neither the kernel nor its gate exists today (i8mm grep-clean-absent from
  skinny).
- **Same-wave-consumer (CH4).** NONE — net-new kernel, no benched CSS antecedent (P1-E
  §4.4a categorical orphan-block); HARD-GATED behind a post-CF-1/CF-2 typed-path re-profile
  proving a digit/dimension leaf top-N. S-P3 must NOT shortlist it as active — gated
  contingency only. (Matches P2-C C6 and the CF-4b table row below.)
- **Arch.** aarch64 i8mm (runtime-detected; NO x86/AVX-512/SVE — Apple cores have no SVE,
  SVE would be dead code, SYNTHESIS §0.4).
- **P1 antecedent.** **NONE — explicitly orphan-blocked (P1-E §4.4(a)).** Same as CF-4a: no
  digit leaf on either benched plane. P1-E §4.4(a) is categorical: "C4b stays orphan-blocked
  on the current (recognition/fact-stream) planes … S-P2 must re-profile the typed path
  after W1/W2, not inherit a CSS digit-kernel hypothesis from here."
- **Grammar-neutral verdict.** **GRAMMAR-NEUTRAL in shape (a generic dimension/digit decode)
  BUT NO P1 ANTECEDENT — CH1 REJECT on current evidence; HARD-GATED behind re-profile.** The
  i8mm dot-product digit decode is grammar-neutral (digit-run family, Lock 14). But it is a
  net-new kernel with unproven hot-leaf relevance — CH1 rejects a speculative kernel with no
  P1 antecedent. Disposition: does NOT land unless a post-CF-1/CF-2 re-profile (on the
  benched typed `ValueRef` path, N≥50) proves the digit/dimension leaf is top-N tailwind
  self-time (alphaE C4b entry gate). On current S-P1 evidence it has no antecedent and S-P3
  must NOT shortlist it as an active candidate — only as a gated contingency.

### CF-0 (negative space): primitives the S-P1 profile proves are NOT needed
P1-E §2.5 / §4.4 establish, for completeness, the candidates the profile FORBIDS:
- **No `unicode` decode primitive.** CSS treats `>=0x80` as a name byte (`generated.rs:404`),
  no codepoint work; zero unicode self-time. A UTF-8-continuation classifier (the other
  `byte_class_from_range_64` sibling, `LOCKS.md:430`) has NO benched CSS antecedent → not a
  CSS candidate (grammar-neutral in the abstract, but JSON/other-grammar-only here).
- **No `dispatch`/`select_classifier` self-time** — the vehicle is correct but UNWIRED for
  CSS (zero SIMD on the CSS path, P1-E §2.5); CF-2 is what wires it. The vehicle is not a
  candidate, it is the substrate the candidate uses.
- **No `push_ascii_lower_hex`/FNV primitive** — 8.98% (P1-E §2.4, `generated.rs:628`) is the
  FNV64→hex diagnostic field serialization; it is bench/diagnostic-only (FNV quarantine,
  SYNTHESIS §0.4) and RETIRES with the fact-stream (CF-1), NOT a primitive. Flagged so no
  agent proposes an FNV/hex kernel (REDRESS pre-block, §4).

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Cand | grammar-specific datum | generic vehicle | witnessed grammars | verdict |
|---|---|---|---|---|
| CF-1 tape-append + `ValueRef` projection | node-kind enum (generated per grammar from `BackendRule`) | `TapeBuilder`/`push_plain_offset` + `ValueRef<G: EventGrammar>` | JSON (live) + CSS (new) | **GRAMMAR-NEUTRAL**, conditional on routing derived-from-grammar (§1.4); else REVISE |
| CF-2 structural-membership classifier | `StructuralAlphabet::from_bytes(byte set)` | `select_classifier(alphabet)` + **256-table/eq-set** primitive (NOT lo6) | JSON (live, lo6) + CSS (new, 256-table/eq-set) | **GRAMMAR-NEUTRAL** at interface; CH2 must hold it to the admissible backend (§1.2) — lo6-reuse-on-CSS is overfit |
| CF-3 commit-by-construction Alt-mode | non-depositing-Alt structural property of the grammar's Alts | codegen `AltMode` pass over `BackendRule` | JSON (witness) + CSS | **GRAMMAR-NEUTRAL** (codegen property); **NO measured speculative-rollback antecedent** — LOCKED profile measured only recognition control (28.87%) + block dispatch (2.45%), classed `structural` not rollback; admits ONLY on a blocking post-CF-1 typed-tape re-profile (§2 CF-3) |
| CF-4a udot 4-digit decode | none (4-ASCII-digit→u32 is universal) | `parse_4_digits` digit-run family | (orphan; non-CSS consumer needed) | grammar-neutral SHAPE but **CSS-ORPHAN** — gate behind dimension-decode re-profile |
| CF-4b i8mm dimension decode | none (digit-run, universal) | net-new i8mm kernel | (none yet) | grammar-neutral SHAPE but **NO P1 ANTECEDENT** — REJECT on current evidence, hard-gated |

Sheets/BBNF-self generalisation is **asserted-by-construction, proof deferred to SK-V18**
for every CF row (Lock 14 phrase #2, §1.5): the vehicles (alphabet, `ValueRef<G>`,
codegen `BackendRule` walk, digit-run family) are grammar-parametric by type, so a Sheets
value-grammar with its own `.bbnf`+`BackendRule` would instantiate them with no new
generic-crate branch — but SK-V17 witnesses only JSON+CSS, and no candidate may use
fleet-wide/four-grammar wording. `sheets_witness` (24-LOC `EventGrammar`, no `BackendRule`)
is NOT a viable SK-V17 projection exercise (§1.5).

**Summary disposition for S-P3:** CF-1, CF-2 are grammar-neutral and admissible to the
shortlist (with the named conditions). CF-3 is grammar-neutral as a codegen property but
carries NO measured speculative-rollback antecedent on the LOCKED profile (which measured
only the 28.87% recognition control loop + 2.45% block dispatch, both classed `structural`,
P1-E §3.3) — it is admissible ONLY contingent on a blocking post-CF-1 typed-tape re-profile
surfacing a speculative-checkpoint / un-masked recognition-control leaf; S-P3 must NOT
shortlist it as an active candidate on current evidence.
CF-4a is grammar-neutral-but-CSS-orphan (gate behind dimension-decode re-profile). CF-4b is
grammar-neutral-in-shape but has no P1 antecedent (REJECT on current evidence; gated
contingency only). NO candidate is JSON-overfit-by-construction; the single overfit
*re-entry* risk is CF-1's routing (§1.4), which the SYNTHESIS Layout close gate already
fences.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

Per SYNTHESIS §0.4 pre-blocks + the inherited REDRESS families (`28+33, 50-55, 60-72, 80,
82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV closed-enum production migration`).
Grammar-neutrality-specific re-open risks:

1. **Relocated-overfit into projection DATA (Lock 14 trap, SYNTHESIS §0.4).** CF-1's routing
   must DERIVE from the grammar/`BackendRule` shape; relocating `W5C_REQUEST_FACT_PROFILES`'
   per-rule branching (hex packing, color-component order, value-list collapse) into a
   projection-data table is the overfit re-entry seam and is FORBIDDEN. Every residual CSS
   routing entry must name its `.bbnf` rule. (Re-opens the Lock-14-phrase-#1 family.)
2. **CSS lo6-table special-casing (CF-2).** Hand-picking a non-colliding CSS delimiter
   subset to force the JSON lo6 NEON path would be a CSS special-case — the honest
   grammar-neutral answer is the 256-table/eq-set primitive with `lo6_table_admissible`
   gating the fallback (§1.2). Re-opens REDRESS 82 (single-quartet unicode classifier
   overfit family) in spirit.
3. **Speculative-rollback re-introduction disguised as a fast path (CF-3).** No
   type-ambivalent dual representation (Lock 1, tape vs OpenFrame vs direct-to-struct
   competing — `LOCKS.md:75`).
4. **Orphan kernel (CF-4a/CF-4b).** No kernel ships without a same-wave consumer + a P1
   antecedent (CH4/CH1). CF-4a/CF-4b have NO benched CSS antecedent (P1-E §4.4(a)) — shipping
   either speculatively re-opens the orphan-kernel pattern the architecture doc and REDRESS
   89 (CSSC CTZ next-bit bulk consumer, orphan) warn against. CF-4b additionally must NOT use
   `is_aarch64_feature_detected!` in the per-leaf hot loop (threads through the OnceLock table
   once, `dispatch.rs:58`).
5. **FNV/hex as a primitive (CF-0).** `push_ascii_lower_hex`/`fnv64` (`generated.rs:628,619`)
   is bench/diagnostic-only (FNV quarantine, SYNTHESIS §0.4); it RETIRES with the fact-stream,
   it is never a candidate primitive. Re-opens the FNV-production-migration pre-block.
6. **Second substrate (all CF).** No `StructLayout`/`TapeStructBuilder`/`TapeCursor` (those
   would be a SECOND substrate alongside `Tape`/`ValueRef`, Lock 1 type-ambivalence,
   SYNTHESIS §0.4); the projection emits accessors over the EXISTING `Tape`/`ValueRef`. No
   sidecar event vector, no retained cursor, no parallel source pass (Lock 1, `LOCKS.md:585`).
7. **x86/AVX/SVE (CF-2/CF-4a/CF-4b).** aarch64 only — NEON/dotprod/i8mm; NO x86, NO AVX-512,
   NO SVE (Apple cores have no SVE; SVE paths would be dead code, SYNTHESIS §0.4).
8. **Cross-call classifier-state retention (CF-2).** REJECT under Lock 1 v+1
   (`LOCKS.md:148-151`); carry stays within a single chunk-call. A SIMD mask stream is a
   transient producer, not a retained sidecar (Lock 1, `LOCKS.md:75`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- **S-P1 profile (locked, commit `0ae1caa52`):**
  `restart/skinny/tranches/sk-v17/research/p1/p1e-hot-leaf-attribution.md` §2.3 (find_component_delim
  56.52% / consume_balanced_at 11.05% = one scan leaf), §2.4 (emit_fact_stream 24.59% + 57.63%
  alloc floor 91.44%-from-emit_fact_stream; push_ascii_lower_hex 8.98%), §2.5 (roll-up: no
  number/unicode/dispatch/tape hot leaf), §4 anomalies 1–4 (recognition-mask, alloc floor,
  scan re-confirm, digit-kernel orphan-block). `p1f-bench-canonical.md`, `p1a..p1d` (mode
  profiles + PMU), `HARDENING-S-P1-V4-CONSOLIDATED.md`.
- **SK-V17 contract:** `restart/skinny/tranches/sk-v17/SYNTHESIS.md` (§0.1 close conditions,
  §0.4 pre-blocks + generality clause, §0.5 per-corpus gates, §0.6 strict comparator, Section
  2 telemetry); `research/alpha/alphaE-candidate-shortlist.md` (C0–C4b candidate pool, the
  S-P2 source); `alphaA..alphaD`.
- **Locks:** `restart/locks/LOCKS.md:75` (Lock 1 substrate-union, transient-producer clause),
  `:148-151` (cross-call classifier-state REJECT), `:386-387` (Lock 14 phrase #2 witnessed-
  grammar scoping), `:393-397` (Lock 14 grammar-neutral primitive: delimiter policy = caller
  data, non-JSON consumer requirement), `:426-431` (`byte_class_from_eq_set_64` /
  `byte_class_from_range_64` abstract-primitive split, digit-run/UTF-8/hex/identifier
  generalisation), `:585` (Lock 1 sidecar/second-tape/cross-call clause), `:603` (Lock 14
  phrase #1: no hand-coded profile arrays / CSS profile matches), `:607` (Lock 16 primitive
  manifest).
- **bbnf source (benched skinny tree, verified this cycle):**
  `skinny/crates/bbnf-simd/src/dispatch.rs:11-15,42,50-58,90-99,101-113` (select_classifier,
  PrimitiveKernels, lo6_table_admissible); `bbnf-simd/src/lib.rs:20,25,43` (StructuralAlphabet,
  from_bytes, class_table); `bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4` (NEON
  scalar-passthrough — the gap); `bbnf-simd/src/aarch64/digit_mac.rs:5,15-22,27,40,51`
  (parse_4_digits scalar+udot, dot4_i8); `bbnf-simd/tests/` (checkasm_byte_class_from_eq_set_64,
  checkasm_byte_class_from_table_64, classifier_parity, corpus_parity present);
  `skinny/crates/runtime/src/grammars/json/scan.rs:5,9-30,22,32` (STRUCTURAL_BYTES, lo6 table,
  scan_structurals, scalar); `json/value.rs:143` (value_from_ref);
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5,91-99,288,293,313,320,322-338,404,619,628`
  (emit_fact_stream, full-parse counts, find_component_delim + delims, consume_balanced_at,
  name-byte >=0x80, fnv64, push_ascii_lower_hex); `skinny/crates/runtime/src/tape/mod.rs:94,175`
  (Tape, ValueRef<G: EventGrammar>); `tape/assembler.rs:42,71` (TapeBuilder, push_plain_offset);
  `skinny/crates/codegen/src/lib.rs:336,1075-1090` (W5C_REQUEST_FACT_PROFILES, sheets/bbnf
  fail-closed negative controls); `skinny/xtask/src/regen_css.rs:45,164` (RequestFactsProfile
  literals, regen_css fn).
- **Measured this cycle (lo6 admissibility, §1.2):** `;`(0x3b) and `{`(0x7b) collide at lo6
  slot 59 under the low-6-bit `(byte & 0x3f)` mask (`dispatch.rs:106`, a bitmask not a modulo —
  0x3b & 0x3f = 0x7b & 0x3f = 0x3b = 59; a true modulo `0x7b % 0x3f = 0x3c` would NOT collide);
  JSON alphabet `{}[],:"` is admissible, every CSS structural alphabet containing the
  `;{` pair is NOT (computed against the `lo6_table_admissible` rule, `dispatch.rs:101-113`).
- **ISA:** Arm Architecture Reference Manual for A-profile (DDI 0487) — AdvSIMD `TBL`/`TBX`
  (vqtbl table lookup), `CMEQ` (`vceqq_u8`), `UDOT` (dot-product extension), `USMMLA`/`SMMLM`
  (i8mm matrix-multiply). NEON is the host SIMD ISA per P1-E §1 (Apple M5 Max,
  aarch64-apple-darwin); no SVE on Apple cores.
- **Comparator planes (SYNTHESIS §0.6):** lightningcss full-CSSOM = the materializing fair
  bar; cssparser token-scan = flaw probe (materializes nothing). P1-E §2.1 measured medians.
- **Host:** Apple M5 Max, aarch64-apple-darwin. S-P1 commit `0ae1caa52`; master HEAD `0ae1caa52`.
