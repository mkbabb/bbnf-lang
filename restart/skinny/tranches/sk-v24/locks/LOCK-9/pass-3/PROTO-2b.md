# PROTO-2b — LOCK-9 pass-3 · SHORT Brief 2 (family 9C) · the `byte_classes`-DIRECT emit

**Probe:** pass-3, LOCK-9 (Scanner & Kernel plane), **probe 2 = Short Brief 2**. Family **9C —
SCANNER-AS-DFA**. This is the **second, independent prover** of Brief 2; it does NOT supersede the
canonical `PROTO-2.md` (probe 1) — it closes probe 1's single explicitly-stated residual.
**Status:** **RAN — MEASURED (feasibility, functional).** Verdict line at bottom. **No perf claim** →
no cold paired timing owed (loadavg recorded for provenance only).

---

## The question (what probe 1 left open)

`PROTO-2.md` (probe 1) proved the 9C `[u8;256]→SIMD` emit pass feasible against the real bbnf-regex
DFA, but derived the member byte-set with the **`find_at` oracle** — 256 single-byte anchored probes —
and closed with an explicit residual (PROTO-2.md:104):

> "Membership was derived via the DFA's `find_at` oracle rather than reading `byte_classes[256]`
> class-ids directly; both are the same partition — a `byte_classes`-direct emit is the mechanical
> next step, not a new uncertainty."

**Short Brief 2 discharges exactly that clause.** It asks: does the emit pass work when it consumes the
**real `dfa.byte_classes:[u8;256]` equivalence partition directly** — the artifact 9C actually routes
into `byte_class_from_table_64` — with **no `find_at` in the derivation**, and does that direct
partition **provably agree** with probe 1's oracle? If yes, "the mechanical next step" is no longer an
assertion but a measurement, and the emit pass is proven against the *shipped DFA output field*, not a
membership bitmap reconstructed by probing.

---

## Method (what actually ran)

**Same real engine, same scratch copy, one new self-contained example file.** No mutation of the
original tree; parse-that stays READ-ONLY (R3 §8, P23 scratch-only).

- Engine: `bbnf-regex` (`rust/regex/`) in the scratch copy — the **real** `Dfa::compile_with`
  (`automata/dfa.rs:67`) → `sets/equiv.rs::compute_byte_classes` → Hopcroft-minimized DFA.
- Probe file: **`rust/regex/examples/emit_probe2.rs`** (scratch only — full source in `PROTO-2b.patch`;
  the `examples/` dir does **not** exist in the source repo — verified). Imports only
  `bbnf_regex::{Dfa, DfaOptions, DEAD}` + `core::arch::aarch64`. No `Cargo.toml` edits.
- **The novum — Method A (`byte_classes`-DIRECT):** read the three shipped public DFA fields —
  `dfa.byte_classes:[u8;256]`, `dfa.flat_transitions`, `dfa.states[..].is_accept`. A class-id `c` is a
  **member class** iff the start state (state 0) transitions on it to an accepting, non-`DEAD` state:
  `flat_transitions[0*num_classes + c] != DEAD && states[target].is_accept`. A byte is a member iff
  `class_is_member[byte_classes[b]]`. The FullTable is emitted **straight off the `[u8;256]`
  partition** (`full[b]=0xff` where the byte's class is a member class) — the exact `byte_class_from_
  table_64` seam. **No `find_at` anywhere in this derivation.**
- **Method B (oracle, probe 1's method):** `member(b) := dfa.find_at(&[b],0) == Some(1)`.
- **Cross-validation (the headline):** assert `direct_set == oracle_set` for every leaf — the two
  engine-truthful derivations must be bit-for-bit the same partition.
- Tier routing: ported `bbnf_simd::dispatch::lo6_table_admissible` (`dispatch.rs:101`) on the member
  set → separable→`vqtbl4q` lo6-table (`classify_tbl4`), non-separable→full `[u8;256]`.
- Parity: emitted classifier (from the byte_classes partition) vs an **independent** scalar reference
  scan built from `find_at` — an 8192-byte xorshift buffer, 64-byte window slid across **every** start
  offset (8129 blocks, all 64 alignments many times), both `black_box`'d, exact `==`. INJECT_BUG drops
  one member from the emitted table; bite measured over blocks containing that byte.
- The four NEON kernels + movemask are faithful ports of
  `skinny/crates/bbnf-simd/src/aarch64/{classify_tbl4, byte_class_from_table_64, movemask}.rs`.

Build: `cargo build -p bbnf-regex --example emit_probe2 --release` → Finished, exit 0 (23 edition-2024
`unsafe_op_in_unsafe_fn` warnings, no errors). Ran twice → **byte-identical output** (deterministic).
Toolchain pin `nightly-2026-04-11` (`rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`) honored;
`CARGO_TARGET_DIR` inside the scratch. Host aarch64 (Apple), NEON baseline. **No x86.**

---

## The numbers (measured, real DFA — `byte_classes`-direct)

| leaf | pattern | num_classes / states | member class-ids | members (from `byte_classes`) | **direct == oracle** | separable(lo6) | emit tier | parity blocks · mism. | INJECT_BUG bite |
|---|---|---|---|---|---|---|---|---|---|
| ws | `[ \t\n\r]+` | 2 / 2 | `[1]` | 09 0A 0D 20 | **true** | true | NeonTbl4 (`vqtbl4q`) | 8129 · **0** | drop 0x09 → **100.0%** (8122/8122) |
| json_structural | `[{}\[\]:,"\\]` | 2 / 2 | `[1]` | 22 2C 3A 5B 5C 5D 7B 7D | **true** | true | NeonTbl4 (`vqtbl4q`) | 8129 · **0** | drop 0x22 → **100.0%** (7930/7930) |
| css_colliding | `[;{}:]` | 2 / 2 | `[1]` | 3A 3B 7B 7D | **true** | **false** | FullTable (`[u8;256]`) | 8129 · **0** | drop 0x3A → **100.0%** (8105/8105) |

**Headline:** for every leaf the **`byte_classes`-direct member set is bit-identical to the `find_at`
oracle set** (`direct == oracle : true`). The partition read straight from the shipped `[u8;256]` field
+ transition table is the *same* partition probe 1 reconstructed by 256 anchored probes — measured, not
argued. The emitted-from-partition classifier is **bit-identical** to the independent DFA scan across
the full 8129-block alignment sweep (0 mismatches each).

**Gate 4 (load-bearing) — the `;`/`{` lo6 collision, proven again on the direct path:**
- `lo6(0x3B)=0x3B`, `lo6(0x7B)=0x3B` → collision; `lo6_table_admissible = false`.
- Naive lo6 table: slot `0x3B` ends holding `0x7B` (`{`) → member `0x3B` (`;`) **SILENTLY DROPPED**.
  `naive=0x0000010000100400` vs `scalar=0x0000010000100408` → **differ = true** (naive misclassifies).
- The separability gate routes the set **off** `vqtbl4q` → FullTable (emitted from `byte_classes`):
  `mask=0x0000010000100408 == scalar` → **bit-identical, no silent drop.**

**Census — Tier-X `\p{L}` (real engine):** `num_classes = 41, states = 21` (under the 255/4096
ceilings) — **reproduces PROTO-2's correction**: the engine does **not** return `None`; Tier-X
exclusion is by class-count over the SIMD membership-tier ceiling, not DFA explosion.

loadavg at run: `3.80 4.76 5.98` (18 CPUs; irrelevant — no timing claim). Deterministic across 2 runs.

---

## What this adds over PROTO-2 (probe 1) / boundary

- **Closes probe 1's sole residual.** Probe 1 emitted from a `find_at`-reconstructed membership bitmap
  and called the `byte_classes`-direct emit "the mechanical next step." This probe **runs that step**
  against the real engine: it reads `dfa.byte_classes` + `flat_transitions` + `is_accept`, emits the
  classifier from the `[u8;256]` partition, and **measures** that the direct partition equals the oracle
  partition on all three leaves. The "mechanical next step" is now a green measurement, so the emit
  pass is proven against the *actual shipped DFA field*, not a probe-reconstructed proxy.
- **Two independent derivations converge** — the direct partition (transition-table reachability) and
  the oracle (anchored matching) are structurally different reads of the DFA yet produce identical
  partitions. That mutual agreement is stronger evidence than either alone that the emit consumes the
  engine's *own* equivalence classes correctly.
- **Boundary (unchanged, honest).** This proves the *emit pass*, not production adoption. 9C stays
  cross-lock-coupled: the automata tier is still **not vendored into skinny** (LOCK-5/GN-1), tier
  selection is a LOCK-2 island, and the codegen home is a named GN-codex supersession. The four
  carry-stateful transductions (quote-parity, escape-parity, bracket-depth, comment carry) are **not**
  touched — hand-authored per 9C §5. All three probe leaves compile to `num_classes=2` (a binary
  member/non-member split); a leaf whose grammar yields a **multi-class** partition (>2 distinct
  member classes needing per-class output, e.g. a full structural classify) would exercise the
  `structural_classify_64` `vceqq`-fan path, which this short probe routes-to-FullTable but does not
  separately fan-verify (probe 1 did — its vceqq cross-check was 0 mismatches). Not a gap in the
  direct-derivation claim; a scope note on which tier each of the three chosen leaves lands in.

---

## VERDICT

**PASS (RAN — MEASURED).** The 9C emit pass built **directly from the real DFA's `byte_classes:[u8;256]`
partition** (no `find_at` in the derivation) produces, for all three leaves, a partition **bit-identical
to the `find_at` oracle** and a classifier **bit-identical to the DFA scan** (8129-block alignment
sweep, 0 mismatches each). The lo6-**colliding** CSS set is **provably rejected off the `vqtbl4q` nibble
tier with no silent drop** — naive lo6 misclassifies (drops `;`0x3B), the gate routes to the full
`[u8;256]` tier which is bit-identical. INJECT_BUG bites **100%** on all three (≥90% required). `\p{L}`
= 41 classes / 21 states (reproduces the "not `None`" correction). Exit 0, deterministic. This closes
`PROTO-2.md`'s sole stated residual: the `byte_classes`-direct emit is no longer "the mechanical next
step" — it is **measured feasible against the real bbnf-regex DFA output field**, further decoupling
9C's largest family-internal gap from the un-executed LOCK-5 automata-tier vendoring.
