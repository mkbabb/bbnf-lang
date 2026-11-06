# PROTO-2 — LOCK-9 pass-3 · Brief 2 (family 9C) · the `[u8;256]→SIMD` emit-pass feasibility

**Probe:** pass-3, LOCK-9 (Scanner & Kernel plane), probe 2 = Brief 2. Family **9C — SCANNER-AS-DFA**.
**Status:** **RAN — MEASURED (feasibility, functional).** Verdict line at bottom.

---

## The question

9C's largest FAMILY-INTERNAL gap is that the `[u8;256]→SIMD` emit pass is **unbuilt** (`equiv.rs`
emits scalar only; `bbnf_simd` has no emit path). The registry BLOCKED 9C partly on this "new code,
unbuilt" uncertainty. **Can that emit pass be built and shown correct *against the real bbnf-regex
DFA output*** — decoupling feasibility from the un-executed LOCK-5 automata-tier vendoring? Concretely
(Brief 2, FAMILY-SPECS §BRIEF 2):

1. For three leaves — ws `[ \t\n\r]+`, the JSON structural set `{}[]:,"\`, and a lo6-**colliding** CSS
   delimiter set incl. `;`(0x3B)+`{`(0x7B) — compile each via the real `Dfa::compile_with`, extract
   `byte_classes:[u8;256]` + `num_classes`.
2. Run the separability predicate (ported `bbnf_simd::dispatch::lo6_table_admissible`) on the derived
   alphabet; route separable→`vqtbl4q` lo6-table, non-separable→full `[u8;256]` (or `vceqq`-fan).
3. Assert **bit-identical** scan vs a scalar reference over xorshift blocks + an alignment sweep; an
   INJECT_BUG (flip one class entry) must break parity.
4. **Gate 4 (load-bearing):** a naive lo6/nibble emit MUST misclassify the `;`+`{` set (silent drop);
   the separability gate must route it OFF `vqtbl4q`.
5. Census real `num_classes`/state counts vs 255-class / 4096-state ceilings, incl. `\p{L}`.

This is a **feasibility/correctness** probe, not a perf probe — it makes **no perf claim**, so no cold
paired timing is owed. (loadavg recorded for provenance only.)

---

## Method (what actually ran)

**Real engine, scratch-copied per R3 §8 (parse-that is READ-ONLY; P23 scratch-only).**
`rsync` of `/Users/mkbabb/Programming/parse-that` → scratchpad `pt-scratch` (excluding `target/ .git/
node_modules/ data/`), external `[patch]` prefixes repointed to the originals, `CARGO_TARGET_DIR`
inside the copy. No mutation of the original tree. Toolchain pin honored.

- Source head: parse-that `ef10d5b`; `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)` (pin
  `nightly-2026-04-11`). Host aarch64 (Apple), NEON baseline. **No x86.**
- The engine: `bbnf-regex` (`rust/regex/`), the **real** `Dfa::compile_with` →
  `sets/equiv.rs::compute_byte_classes` → Hopcroft-minimized DFA. Built green (`cargo build -p
  bbnf-regex`, exit 0) before the probe.
- The probe: **one new self-contained file**, `rust/regex/examples/emit_probe.rs` (scratch only — the
  code is in `PROTO-2.patch`). It imports only `bbnf_regex::{Dfa, DfaOptions}` and
  `core::arch::aarch64`. No Cargo.toml edits. **The four kernels are faithful ports** of
  `skinny/crates/bbnf-simd/src/aarch64/{classify_tbl4, byte_class_from_table_64, structural_classify_64,
  movemask}.rs` + the `dispatch.rs:101` predicate.
- **Member-set is derived from the REAL DFA, not assumed:** for every byte `b∈0..256`,
  `member(b) := dfa.find_at(&[b],0) == Some(1)`. The whole pipeline is engine-truthful end to end.
- Parity: an 8192-byte xorshift buffer mixing member/non-member bytes; a 64-byte window slid across
  **every** start offset (8129 blocks — covers all 64 alignments many times); emitted-tier mask vs
  scalar mask, both `black_box`'d, exact `==`. INJECT_BUG drops one member from the emitted table and
  measures the bite rate over blocks containing that byte.

Build: `cargo build -p bbnf-regex --example emit_probe --release` → Finished, exit 0 (32 edition-2024
`unsafe_op_in_unsafe_fn` warnings, no errors). Run twice → **byte-identical output** (deterministic).

---

## The numbers (measured, real DFA)

| leaf | pattern | num_classes / states | members (from DFA) | separable(lo6) | emit tier | parity blocks · mismatches | vceqq-fan | INJECT_BUG bite |
|---|---|---|---|---|---|---|---|---|
| ws | `[ \t\n\r]+` | 2 / 2 | 09 0A 0D 20 | **true** | NeonTbl4 (`vqtbl4q`) | 8129 · **0** | 0 (match) | drop 0x09 → **100.0%** (8129/8129) |
| json_structural | `[{}\[\]:,"\\]` | 2 / 2 | 22 2C 3A 5B 5C 5D 7B 7D | **true** | NeonTbl4 (`vqtbl4q`) | 8129 · **0** | 0 (match) | drop 0x22 → **100.0%** (8011/8011) |
| css_colliding | `[;{}:]` | 2 / 2 | 3A 3B 7B 7D | **false** | FullTable (`[u8;256]`) | 8129 · **0** | — | drop 0x3A → **100.0%** (8129/8129) |

**Gate 4 (load-bearing) — the `;`/`{` lo6 collision, proven:**
- `lo6(0x3B)=0x3B`, `lo6(0x7B)=0x3B` → collision confirmed; `lo6_table_admissible = false`.
- Naive lo6 table: slot `0x3B` ends holding `0x7B` (`{`) → member `0x3B` (`;`) **SILENTLY DROPPED**.
  `naive_lo6_mask=0x…0400` vs `scalar_mask=0x…0408` → **differ = true** (naive misclassifies, as
  required — the naive emit is NOT safe here).
- The separability gate routes the set **off** `vqtbl4q` → FullTable → `mask=0x…0408` == scalar →
  **bit-identical, no silent drop.**

**Census — Tier-X `\p{L}` (real engine):** `\p{L}` compiled to **num_classes = 41, states = 21**
(under the 255-class / 4096-state DFA ceilings). **Honest correction to the pass-3 spec:** the real
engine did **not** return `None` for `\p{L}` (FAMILY-SPECS §9C predicted `dfa.rs:262 → None`). It
compiles fine; the reason `\p{L}` is Tier-X excluded is that **41 byte-classes exceed the SIMD
membership/nibble tier's practical ceiling** (`vqtbl4q` lo6 handles a *binary* membership mask, not
41-way classification), not that the DFA explodes. The exclusion stands; its stated *mechanism* was
wrong and is corrected here by measurement.

loadavg at run: `14.99 8.36 6.96` (contended host; irrelevant — no timing claim). CPUs: 18.

---

## What this proves / its boundary

- **Proven (feasible NOW, against the real DFA):** the emit pass — derive `byte_classes` from the real
  `Dfa` → run the *same* `lo6_table_admissible` separability predicate → route to the correct NEON tier
  → **bit-identical** classifier — is **buildable and correct** for membership + structural + colliding
  leaves. The separability gate is the load-bearing safety: it provably catches the `;`/`{` lo6
  collision and routes off the nibble tier with **no silent drop**, where a naive lo6 emit silently
  drops a member. This **decouples 9C's largest internal gap from the un-executed LOCK-5 vendoring**:
  the gap is now *measured-feasible*, not merely argued.
- **Boundary (unchanged, honest):** this proves the *emit pass*, not production adoption. 9C remains
  cross-lock-coupled — the automata tier is still **not vendored into skinny** (LOCK-5/GN-1), the
  tier-selection island is LOCK-2, and the codegen home is a named GN-codex supersession. The four
  carry-stateful transductions (quote-parity, escape-parity, bracket-depth, comment carry) are **not**
  touched — they stay hand-authored per 9C §5. Membership was derived via the DFA's `find_at` oracle
  (engine-truthful) rather than reading `byte_classes[256]` class-ids directly; both are the same
  partition — a `byte_classes`-direct emit is the mechanical next step, not a new uncertainty.

---

## VERDICT

**PASS (RAN — MEASURED).** All three leaves emit a **bit-identical** classifier via the correct
separability-selected tier (8129-block alignment sweep, 0 mismatches each; vceqq-fan cross-check 0
mismatches). The lo6-**colliding** CSS set is **provably rejected off the `vqtbl4q` nibble tier with no
silent drop** — the naive lo6 emit misclassifies it (drops `;`0x3B), the gate routes it to the full
`[u8;256]` tier which is bit-identical. INJECT_BUG bites **100%** on all three leaves (≥90% required).
The `[u8;256]→SIMD` emit pass is **buildable against the real bbnf-regex DFA output** — 9C's largest
family-internal gap is now measured-feasible, decoupled from the un-executed automata-tier vendoring.
Exit 0. (Deviation from spec: `\p{L}` compiles at 41 classes / 21 states — it does NOT return `None`;
Tier-X exclusion is by class-count over the SIMD tier ceiling, not DFA explosion.)
