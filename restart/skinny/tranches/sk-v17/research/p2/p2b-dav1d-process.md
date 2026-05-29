# SK-V17 P2-B: DAV1D / FFmpeg hand-ASM SIMD process → bbnf-simd primitive-admission gate

Pass: S-P2 Research. Cycle: V3.
Date: 2026-05-29.
Scope: The hand-written-ASM SIMD discipline of dav1d / FFmpeg / VLC — scalar-oracle-first,
the checkasm differential harness, the same-wave-consumer rule — mapped onto bbnf-simd's
`tests/checkasm_*.rs` harness + the `src/scalar/<prim>.rs` reference requirement, producing
the per-primitive admission process S-P3 will gate every candidate against.
Output: this file.
P1 hot-leaf antecedents: `CssFullParser::find_component_delim` (`generated.rs:288`, 59.24%
self, **scan**) + `consume_balanced_at` (`generated.rs:320`, 10.31%, structural-over-scan,
SAME inner loop) → ONE NEON byte-class-membership scan primitive; the `emit_fact_stream`
String floor (`generated.rs:5`, 25% self + ~64% alloc) → tape append `push_plain_offset`
(`assembler.rs:71`); the orphan udot digit kernel `parse_4_digits_dotprod`
(`bbnf-simd/src/aarch64/digit_mac.rs:27`) — process-rejected, no CSS antecedent.
Lock surface: Lock 16 (the dav1d primitive-lift row + the v+1 primitive manifest is the
admission process this artefact formalises) and Lock 14 (grammar-neutrality of every
admitted primitive). Lock 1 referenced where the same-wave-consumer rule names the tape.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

### 1.1 — The dav1d / FFmpeg process is three coupled invariants, not "write fast asm"

dav1d's SIMD development discipline (the `tests/checkasm/` harness, inherited from FFmpeg's
`libavutil/x86/x86inc.asm` lineage and the FFmpeg `checkasm` tool) is not "hand-optimise a
loop". It is a falsifiability protocol with three load-bearing invariants. Each maps to a
concrete bbnf-simd construct that ALREADY EXISTS in the tree — the process is the gate, not
a hypothetical:

1. **Scalar-oracle-FIRST.** Every kernel has a plain-C (here plain-Rust) reference written
   *before* the vector body, and the reference IS the specification. dav1d's `checkasm`
   runs the C function and the asm function over the same input and asserts bit-equality; the
   C function is never the thing under optimisation — it is the oracle. In bbnf-simd this is
   `src/scalar/<prim>.rs`: e.g. `byte_class_from_eq_set_64_scalar` (`src/scalar/byte_class_from_eq_set_64.rs:26`)
   is a 6-line `for i in 0..64 { if set.contains(&src[i]) { mask |= 1<<i } }` — the executable
   specification, doc-stamped "this is the source-of-truth implementation. The vector bodies
   are strictly correctness-equivalent fan-outs" (`:17-18`). Citation: dav1d `tests/checkasm/checkasm.c`
   `check_func`/`bench_new` pattern; FFmpeg `tests/checkasm/checkasm.c` (the harness dav1d forked).

2. **The checkasm DIFFERENTIAL harness.** dav1d's `checkasm` is adversarial, not a smoke test:
   it (a) sweeps every CPU-feature subset (`--test`, `DAV1D_CPU_MASK`), running the same input
   through each vector backend AND the C reference; (b) randomises inputs per run with a fixed
   seed; (c) guards callee-saved registers (`checkasm_checked_call` pushes sentinels into
   x19–x28 / xmm6–15 and asserts they survive — catches an ABI-violating kernel); (d) traps
   SIGSEGV/SIGBUS/SIGILL so an out-of-bounds load is a *test failure*, not a crash. bbnf-simd's
   `tests/checkasm_common.rs` reproduces ALL FOUR: `Xorshift64` deterministic fill (`:3-31`),
   `with_stack_canary_xor_fold` 1 KiB stack-canary clobber-detect (`:50-72`),
   `callee_saved_register_then` writing sentinels into x19–x28 and asserting survival
   (`:85-112`, aarch64-gated `:83`), and per-test `signal_guard::arm()` trapping
   SIGSEGV/SIGBUS/SIGILL → `panic!("checkasm: candidate kernel raised {name}")`
   (`tests/checkasm_byte_class_from_eq_set_64.rs:96-116`). The byte-class test additionally
   sweeps all 64 alignments (`:189-216`), all 8 set cardinalities (`:224-253`), adversarial
   seeds that caught a prior `escape_mask_64` handoff bug (`:261-291`), corpus parity over the
   first 2 MiB of `twitter.json` with a splitmix64 rolling digest (`:300-369`), empty-set,
   constant-fill, duplicate-entry, and a tail-padding contract that forbids reading byte 65
   (`:504-530`). Citation: dav1d `tests/checkasm/checkasm.c:checkasm_check_func` +
   `src/arm/64/*` register-clobber convention; FFmpeg `checkasm` README.

3. **The same-wave-consumer rule.** dav1d never lands a kernel that no decoder path calls in
   the same change — a kernel with no consumer is dead asm that bit-rots out of parity. Lock 16
   v+1 encodes this verbatim: "Support-only hint modules, unconsumed prefix/next bitmap bodies,
   cache hints without exact caller placement, and orphan `asm!`/intrinsic files do not close
   Lock 16" (`LOCKS.md:511-516`), and `escape_mask_64` "admits a row only when a JSON/CSS string
   or escape consumer wires it in the same wave and moves or rejects the named row under strict
   comparator evidence" (`LOCKS.md` escape_mask clause). Every Lock 16 manifest row carries a
   `same-wave production consumer` field (`LOCKS.md:607` v+1 manifest; `LOCKS.md:482-489`).

### 1.2 — bbnf-simd ALREADY implements the dav1d process for its admitted primitives

The process is not aspirational. `src/scalar/` holds 7 scalar references
(`byte_class_from_eq_set_64`, `byte_class_from_table_64`, `bitmap_prefix_xor_64`,
`bulk_emit_positions_64`, `bitmap_next_set_bit`, `eob_pad_clamp`, `swar_8byte`); `src/aarch64/`
holds the NEON bodies; `tests/` holds 11 `checkasm_*.rs` differentials
(`checkasm_byte_class_from_eq_set_64`, `_byte_class_from_table_64`, `_bitmap_prefix_xor_64`,
`_bulk_emit_positions_64`, `_bitmap_next_set_bit`, `_eob_pad_clamp`, `_structural_terminator_64`,
`_escape_mask_64`, `_ascii_set_member_find_64`, `_utf8_block`, plus `checkasm_parity` /
`classifier_parity` / `corpus_parity`). The NEON `byte_class_from_eq_set_64_neon`
(`src/aarch64/byte_class_from_eq_set_64.rs:33`) is four 16-byte stripes, each `vceqq_u8`-fanned
per set member and `vorrq_u8`-reduced, then packed via `movemask_u8x16` (`:79`) — and its
header cites "Lock 16 (SOTA-BEAT-DESIGN, dav1d primitive-lift row)" + asmjson `classify_chunk`
(`:8-19`). The dav1d process is therefore the EXISTING admission shape; S-P2's job is to state
it as the gate S-P3 applies to each candidate, and to mark which candidates already clear it.

### 1.3 — The orphan kernel that FAILS the process today (the negative exemplar)

`parse_4_digits_dotprod` (`src/aarch64/digit_mac.rs:27`) is the counter-example that pins why
the process matters. It is a raw `asm!("udot ...")` block (`:39-45`) with an inline scalar
fallback under `#[cfg(not(target_feature="dotprod"))]` (`:15-22`) — but (a) there is NO
`checkasm_digit_mac.rs` differential in `tests/` (grep: absent), so it has never been swept
against its scalar form under the canary/signal/alignment harness; (b) it has ZERO benched CSS
antecedent — P1-E §2.5 + §4.4 prove no `number`/digit leaf is hot on either CSS plane (the CSS
recognizer counts, it does not decode dimensions), and no `bbnf_simd` frame appears in any CSS
profile (HARDENING-S-P1-V4 §3.3 orphan-blocked row). It fails invariant-1 (no committed
differential) AND invariant-3 (no same-wave CSS consumer). The process disposition is: REJECT
for SK-V17, re-admission only after a typed lazy-`ValueRef` re-profile proves a digit leaf hot
(P1-E §4.4 re-admission condition) — never inherited as a hypothesis.

### 1.4 — The strict-comparator coupling (PASS-2 §8.1)

The dav1d process's "bit-equality vs the oracle" maps onto the bbnf strict-vs-strict comparator
discipline: a primitive's *parity* anchor is the scalar oracle (correctness), and its
*speed-admission* anchor is the strict comparator plane (lightningcss = materializing full-CSSOM;
cssparser = token-scan flaw-probe). `BBNF_SIMD_STRICT=1` is the admission switch — "Non-strict
parity is exploratory only and cannot admit a primitive" (`LOCKS.md:520-524`). A primitive that
clears the checkasm oracle but only beats a *permissive* comparator is not admitted; the
same-wave consumer must move (or reject) a row under the strict comparator. P1-E §2.2 fixes the
strict bar at fact_stream 0.60–0.79× lightningcss — the gap the consumer row must close.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

This pass does not invent new kernels; the candidate POOL is fixed by the S-P1 profile. P2-B's
artefact is the **admission process each candidate must clear** plus the per-candidate process
verdict. Three candidates trace to a named P1 hot leaf; one orphan is process-rejected.

### C-B1 — `byte_class_from_eq_set_64` (the ONE recognition-scan primitive)

- **Shape.** `fn(src: &[u8;64], set: &[u8]) -> u64` — bit `i` set iff `src[i] ∈ set`,
  `set.len() ≤ 8`. The CSS hot leaf's three delimiter sets (`b";{}"` / `b"{};"` / `b":{};"`,
  `generated.rs:295`) and `consume_balanced_at`'s bracket set all fit `len ≤ 8`; the primitive
  is the movemask-cascade replacement for the `delimiters.contains(&byte)` linear membership +
  per-byte `match` inner loop (`generated.rs:293-308 ≡ 322-338`).
- **Scalar-ref status: PRESENT.** `byte_class_from_eq_set_64_scalar`
  (`src/scalar/byte_class_from_eq_set_64.rs:26`) is the committed oracle.
- **Checkasm status: PRESENT + adversarial.** `tests/checkasm_byte_class_from_eq_set_64.rs`
  — 7 tests: alignment sweep (`:189`), set-size sweep (`:224`), adversarial seeds (`:261`),
  twitter corpus parity (`:300`), empty-set/constant-fill/duplicate-set/tail-padding edge cases
  (`:376-530`). Runs under canary + signal-trap + callee-saved guard.
- **Arch: NEON.** `byte_class_from_eq_set_64_neon` (`src/aarch64/byte_class_from_eq_set_64.rs:33`),
  four `vceqq_u8`/`vorrq_u8` stripes + `movemask_u8x16`. aarch64-only; scalar is the
  non-aarch64 / non-admissible-alphabet fallback. NO x86/AVX (PASS-2 axis).
- **Same-wave consumer: NOT YET WIRED on CSS — this is the gate the SK-V17 wave must satisfy.**
  Two distinct layers must NOT be conflated here:
  - **Shared classifier interface (JSON-wired).** The JSON production scan reaches a NEON
    membership classifier through `select_classifier` (`dispatch.rs:42`) → `SelectedBackend`
    (`dispatch.rs:12`), which today dispatches only `Scalar` vs `NeonTbl4` — the low-6-bit
    table route (`classify_tbl4`). The live JSON neon `scan` (`grammars/json/scan.rs:207`)
    calls `classify_structural_terminator_block_from_table` (`:219`) over `STRUCTURAL_CLASS_TABLE_LO6`
    and finishes with `escape_mask_64` / `prefix_xor_64` (`:237`/`:239`). This is the lo6/table
    backend, NOT the eq-set kernel.
  - **Eq-set backend (CSS-bound).** `byte_class_from_eq_set_64` is a SEPARATE kernel
    (`lib.rs:282` → `byte_class_from_eq_set_64_neon` `src/aarch64/byte_class_from_eq_set_64.rs:33`).
    It is NOT a `SelectedBackend` arm; the classifier dispatch never routes to it. Its only
    non-test exerciser is the corpus-parity smoke (`lib.rs:216`) and `checkasm_byte_class_from_eq_set_64`
    — i.e. checkasm/corpus-parity coverage only, NOT a live JSON production consumer. So
    "JSON-wired" is true of the SHARED classifier interface (`select_classifier`); it is FALSE
    of the eq-set NEON leaf, which is exercised today only by the differential harness.
  Why the eq-set fan is the CSS route and the lo6 table is NOT: the lo6 admissibility guard
  `lo6_table_admissible` (`dispatch.rs:101`) computes `(byte & 0x3f)` — a low-6-bit MASK, not a
  modulo — and rejects any alphabet whose low-6 slots collide. CSS punctuation collides: `;`=0x3b
  and `{`=0x7b both map to slot `0x7b & 0x3f = 0x3b = 59`, so no CSS delimiter alphabet that needs
  both is lo6-admissible (`select_backend` falls to `Scalar`). It would NOT collide under true
  modulo (`0x7b % 0x3f = 0x3c = 60 ≠ 59`), which is exactly why the distinction matters. The CSS
  route is therefore the eq-set fan (`byte_class_from_eq_set_64_neon`), aligned with P2-C C2 /
  P2-D §1.4 / P2-F §1.2. Process verdict: the kernel CLEARS invariants 1+2 already; it CLEARS
  invariant 3 only when the wave that wires the eq-set classifier into the CSS recognizer
  (replacing `find_component_delim`'s scalar inner loop) lands in the same change AND moves the
  recognition-plane row under strict comparison.
  **Critically gated behind tape activation** (HARDENING-S-P1-V4 §3.4 #1): there is no structural
  index to pre-scan into until the tape decodes CSS, and on the typed plane the scan is masked by
  the String floor — so the lever order is tape FIRST, then NEON on the surviving scan.
- **P1 antecedent: `find_component_delim` 59.24% + `consume_balanced_at` 10.31% = ~69%**
  (HARDENING-S-P1-V4 §3.3). The two collapse to ONE target (identical `while pos<len` + per-byte
  membership inner loop, differing only in the membership test). The STRONGEST-grounded candidate
  in the pool.

### C-B2 — `push_plain_offset` tape-append (the fact-stream String-floor retirement)

- **Shape.** `TapeBuilder::push_plain_offset(&mut self, offset: u32)` (`assembler.rs:71`) — one
  branchless `u32` write into the EXISTING `self.offsets`. NOT a SIMD/ASM kernel; it is the
  tape-substrate append that replaces `emit_fact_stream`'s `String` `push_str` accumulator.
- **Scalar-ref status: N/A by construction.** It is not a vector kernel, so the
  scalar-oracle-first invariant applies only to its *correctness oracle*, which is the
  existing-vs-new fact equality (the typed-AST-parity gate, `feedback_typed_materialization_invariant`),
  not a `src/scalar/` entry. The dav1d process maps here as: the consumer (the typed projection)
  must produce bit-identical facts to the current `fact_stream` String output, verified by a
  corpus-parity differential — the analogue of checkasm's bit-equality, run over the 4 CSS corpora.
- **Checkasm status: re-framed as fact-parity differential** — the SK-V17 wave owes a
  `tape ↔ fact_stream` corpus-parity test (the existing `corpus_parity.rs` shape extended to CSS),
  NOT a `checkasm_*` register/signal harness (no vector load to fault).
- **Arch: scalar (substrate).** No NEON.
- **Same-wave consumer: the typed lazy-`ValueRef` projection** (`ValueRef` `mod.rs:175`,
  `value_from_ref` JSON-proven `value.rs:143`). The tape-append and the cursor-view land together
  or neither lands — this is exactly the same-wave rule (Lock 1 substrate union + Lock 16 consumer).
- **P1 antecedent: `emit_fact_stream` 25.01% self + ~64% alloc floor** (HARDENING-S-P1-V4 §3.3,
  P1-E §2.4: 91.44% of the syslib allocator floor reached FROM `emit_fact_stream` String growth).
  The dominant intervention surface; the cost SK-V17's tape activation removes.

### C-B3 — orphan: `parse_4_digits_dotprod` udot digit kernel — PROCESS-REJECTED

- **Shape.** `unsafe fn(bytes: [u8;4]) -> u32` via raw `asm!("udot {acc}.4s, {digits}.16b, {weights}.16b")`
  (`digit_mac.rs:27-45`), DotProd-gated.
- **Scalar-ref status: inline fallback only, NO committed differential.** The
  `#[cfg(not(target_feature="dotprod"))]` arm (`:15-22`) is a fallback, not a `src/scalar/digit_mac.rs`
  oracle, and there is no `tests/checkasm_digit_mac.rs` sweeping the two against each other under
  the canary/signal/alignment harness.
- **Checkasm status: ABSENT.** Fails invariant-2.
- **Same-wave consumer: NONE on CSS.** Fails invariant-3 — zero digit self-time on either CSS
  plane (P1-E §2.5, §4.4); no `bbnf_simd` frame in any CSS profile.
- **P1 antecedent: NONE.** HARDENING-S-P1-V4 §3.3 "Orphan-blocked" row. Process disposition:
  REJECT for SK-V17. Re-admission condition (P1-E §4.4): re-profile the typed lazy-`ValueRef`
  path AFTER W1/W2; admit ONLY if a digit/dimension leaf is then a measured top-N self-time leaf.
  S-P2 must NOT inherit a CSS digit-kernel hypothesis (profile-first, ORCHESTRATOR §8).

### C-B0 — The admission process itself (the load-bearing S-P3 gate)

The deliverable PASS-2 §2 names is the **primitive-admission process S-P3 will gate against**.
Stated as a checklist every shortlisted candidate must clear, derived from the three dav1d
invariants + Lock 16 v+1 manifest fields:

| Gate | dav1d origin | bbnf construct | Pass condition |
|---|---|---|---|
| G1 scalar oracle exists | C reference written first | `src/scalar/<prim>.rs` | committed `*_scalar` fn IS the spec; vector body is a fan-out |
| G2 checkasm differential | `checkasm_check_func` bit-equality | `tests/checkasm_<prim>.rs` | alignment sweep + adversarial seeds + corpus parity, all under canary + signal-trap + callee-saved guard, `BBNF_SIMD_STRICT=1` |
| G3 ABI / OOB safety | callee-saved + SIGSEGV trap | `callee_saved_register_then` + `signal_guard::arm()` + tail-padding contract | no clobber, no fault, no read past the window |
| G4 same-wave consumer | no orphan asm | Lock 16 manifest `same-wave production consumer` | a named CSS/JSON row moves or rejects in the SAME wave the kernel lands |
| G5 strict comparator | — (bbnf-specific) | lightningcss full-CSSOM / cssparser flaw-probe | the moved row beats the STRICT (materializing) bar, not a permissive one |
| G6 grammar-neutral | dav1d abstract-primitive lift | P2-F verdict | byte-set / classifier / tape op, not a JSON/CSS role (Lock 14) |

A candidate failing ANY gate is not S-P3-shortlist-eligible. C-B1 clears G1–G3+G6 today and owes
G4+G5 to its wave; C-B2 clears the fact-parity analogue of G1–G3 and owes G4+G5; C-B3 fails G1, G2,
G4 outright.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

- **C-B1 `byte_class_from_eq_set_64`: GRAMMAR-NEUTRAL.** It is a byte-set membership classifier
  over an arbitrary `set: &[u8]` — the CSS delimiter sets, the JSON structural set `b"{}[],:"`
  (`checkasm_byte_class_from_eq_set_64.rs:320`), a Sheets cell-delimiter set, and BBNF-self's
  metacharacter set are all just different `set` arguments. The kernel knows no grammar role; the
  per-grammar template supplies the set. This is the textbook Lock 14 grammar-neutral form (dav1d
  abstract-primitive lift: cross-lane compare/reduce generalises to any byte stream). Verdict
  here: PASS (verified §3 — byte-set membership carries no grammar role); P2-F formalises the
  full cross-grammar set-mapping in-pass.
- **C-B2 `push_plain_offset`: GRAMMAR-NEUTRAL.** It is a `u32` append into the shared offset tape —
  the substrate every grammar's projection borrows into (`ValueRef`/`DocumentView`). It carries no
  CSS semantics; `emit_fact_stream`'s String growth is grammar-neutral `String` `push_str`, not CSS
  logic (P1-E §4.2). The CSS-specific part (which offsets, what they mean) lives in the per-grammar
  generated projection, not in the tape op. PASS.
- **C-B3 udot digit kernel: would be grammar-neutral IF admitted** (4-digit→u32 decode is generic),
  but it is moot — process-rejected at G1/G2/G4 with no P1 antecedent. P2-F should mark it
  orphan-blocked, not JSON-overfit (it is not overfit; it is unreached).

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- **C-B1 must not become a parallel substrate (Lock 1 / CH5).** The NEON scan emits a transient
  mask consumed within the chunk call; it must NOT retain a sidecar event vector, a density table,
  or cross-call classifier state (`LOCKS.md:141-149` Lock 1 v+1 carry-within-chunk clause). The
  structural projection it feeds IS the tape, not a second scan (REDRESS 60-72 sidecar producers;
  alphaC §0 skinny-tree disambiguation). It is also gated behind tape activation — wiring NEON
  before the tape decodes CSS would create a scan with no index to fill (HARDENING-S-P1-V4 §3.4 #1).
- **C-B1 must not re-open the SK-V6 single-quartet unicode classifier / StringBlock16 tiny probe
  (REDRESS 82-84) or PMULL prefix-XOR-as-hot-body (REDRESS 88) or CSSC-CTZ next-bit bulk consumer
  (REDRESS 89)** — those are distinct blocked routes; the byte-class membership scan is none of
  them, but the wave must not smuggle them in as "while we're in NEON".
- **C-B2 must not re-open AZ-IV eager-value-tree (REDRESS §1, 118× canada regression).** The tape
  stays lazy-by-default; `push_plain_offset` writes a re-readable source span, NOT a per-leaf typed
  node / `f64` heap alloc at parse time (alphaC §1 re-open test). It must not re-open StructRegistry
  indirection (single non-generic `TapeBuilder`), fact-stream-as-retained-sidecar (Lock 1
  FactStream clause `LOCKS.md:585`), or the W5C broadcast diagnostic.
- **C-B3 must not be inherited as a CSS hypothesis (profile-first, ORCHESTRATOR §8).** Its
  re-admission is conditional on a future typed-path re-profile, not on SK-V16 narrative carryover
  (the falsified "~70 Mbps / ~14×" N-direct row, HARDENING-S-P1-V4 §3.2).
- **No FNV/hex diagnostic carried as a primitive.** `push_ascii_lower_hex` (8.98%, `generated.rs:628`)
  is a bench-only diagnostic encode that vanishes with tape activation; it is explicitly NOT a
  candidate (HARDENING-S-P1-V4 §3.3, HANDOFF :165) and no §2 candidate references it.

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- **dav1d checkasm harness:** `tests/checkasm/checkasm.c` (`checkasm_check_func`, `bench_new`,
  per-CPU-feature sweep via `DAV1D_CPU_MASK`); `src/arm/64/*.S` callee-saved (x19–x28) convention;
  `checkasm_checked_call` register-sentinel guard. dav1d source, VideoLAN.
- **FFmpeg checkasm lineage:** `tests/checkasm/checkasm.c` (the harness dav1d forked); FFmpeg
  `libavutil/x86/x86inc.asm` macro corpus (vendored at `crates/bbnf-simd/ext/x86/x86inc.asm` per
  `LOCKS.md`). FFmpeg source, FFmpeg project.
- **asmjson (Lemire et al.) `classify_chunk`:** the AVX-512 BW `vpcmpeqb`/`korq` fan-reduce the
  NEON `vceqq_u8`/`vorrq_u8` body ports (cited in `src/aarch64/byte_class_from_eq_set_64.rs:14-15`
  + `src/scalar/byte_class_from_eq_set_64.rs:13-15`).
- **Lock 16 (the admission process this artefact formalises):** `restart/locks/LOCKS.md:478-545`
  (SOTA-BEAT-DESIGN dav1d primitive-lift row, abstract-primitive-lifts paragraph, v+1 primitive
  manifest fields, `BBNF_SIMD_STRICT=1` strict-parity clause, escape_mask same-wave-consumer
  clause, `wired`/`deleted`/`scalar-delegate`/`architectural-block` close states); Lock 16
  primitive-manifest clause `LOCKS.md:607`; Lock 1 carry-within-chunk `LOCKS.md:141-149` +
  FactStream clause `:585`; Lock 14 grammar-generalisation `:603`.
- **bbnf-simd in-tree (the existing dav1d-process implementation):**
  `tests/checkasm_common.rs` (`Xorshift64` :3, `with_stack_canary_xor_fold` :50,
  `callee_saved_register_then` :85); `tests/checkasm_byte_class_from_eq_set_64.rs`
  (signal_guard :96, alignment_sweep :189, set_size_sweep :224, adversarial_seeds :261,
  corpus_parity :300, edge cases :376-530); `src/scalar/byte_class_from_eq_set_64.rs:26`;
  `src/aarch64/byte_class_from_eq_set_64.rs:33` + `movemask_u8x16:79`; the eq-set kernel's only
  non-test reach is `bbnf_simd::prim::byte_class_from_eq_set_64` (`src/lib.rs:282`) + the
  corpus-parity smoke (`src/lib.rs:216`) — checkasm/corpus-parity coverage, NOT a live JSON
  production consumer; `src/dispatch.rs` (`select_classifier:42`, `SelectedBackend:12` —
  `Scalar`/`NeonTbl4` only, no eq-set arm; `PrimitiveKernels:50`; `lo6_table_admissible:101`
  computing the low-6-bit `(byte & 0x3f)` MASK — `;`=0x3b / `{`=0x7b collide at slot 59 under the
  mask, would NOT collide under true modulo `0x7b % 0x3f = 60`); JSON live scan path
  `grammars/json/scan.rs:207` (neon `scan`), `:219`
  (`classify_structural_terminator_block_from_table`, lo6/table route), `:237`/`:239`
  (`escape_mask_64`/`prefix_xor_64`); `src/aarch64/digit_mac.rs:27` (orphan udot, process-rejected).
- **S-P1 profile (the candidate pool + antecedents):**
  `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  §3.3 (hot leaves), §3.4 (primitive antecedents); `research/p1/p1e-hot-leaf-attribution.md`
  §2.2 (strict bar 0.60–0.79× lcss), §2.4 (alloc floor 91.44% from emit_fact_stream), §2.5
  (no number/unicode/dispatch/tape hot leaf), §4.4 (digit-kernel re-admission condition).
- **Benched CSS path:** `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
  (`emit_fact_stream` :5, `find_component_delim` :288, `consume_balanced_at` :320,
  `push_ascii_lower_hex` :628); tape substrate `skinny/crates/runtime/src/tape/`
  (`TapeBuilder` assembler.rs:42, `push_plain_offset` :71, `ValueRef` mod.rs:175,
  `value_from_ref` JSON-proven `grammars/json/value.rs:143`).
- **REDRESS pre-block:** `restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md`
  §0 (skinny-tree disambiguation), §1 (AZ-IV eager re-open test); `skinny/REDRESS.md` 28+33 /
  50-55 / 60-72 / 80 / 82-84 / 88 / 89.
- Host: Apple M5 Max, aarch64-apple-darwin. Master HEAD `0ae1caa52`. NEON only — no x86/AVX/SVE.
