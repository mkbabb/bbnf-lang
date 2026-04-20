# A9 — Structural scan + Pratt flattening (AY planning)

Two-part deep audit at master HEAD `851aaebc`, working from fresh A1/A2/A3
profiles (`.profiles/samply/**`, Apr 20 01:43/01:51). Read-only audit;
no source mutations, no fresh bench. Pairs to A7 (tape-inline) and A8
(finalise-fuse) as the third wave-planning lens — the question it
answers is whether two substrates the grammar mines but the hot path
ignores (Part 1: structural alphabet) or two landing patterns the hot
path emits but the tape write amortises awkwardly (Part 2: Pratt
reducer-compound) deserve W1-priority slots in AY.

## Part 1 — Structural scan with dense alphabets

### 1.1 Mining pipeline (current state)

The structural alphabet is mined by `compute_structural_alphabet` at
`crates/ir/src/passes/sets/structural_alphabet.rs:182`. W1.γ admission
(W1.γ docstring, lines 41-54) restricts the single-byte set to four
channels: single-byte `Literal` first-bytes, `Alt`-branch single-byte
literal leaders, exactly-2-byte literal first-bytes (digraph openers),
and regex `QuotedString` classifier bytes. The result lives at
`GrammarIR.structural_alphabet: Option<StructuralAlphabet>` with
`single_bytes: BTreeSet<u8>`, `digraphs: Vec<(u8,u8)>`,
`digraph_mask: [u64;4]`, `quote_classes: BTreeSet<u8>`
(`structural_alphabet.rs:120-142`).

Mining is wired in the emitter pipeline at
`crates/core/src/pipeline/compile.rs:703-705`:

```text
timer.span("compute_structural_alphabet", || {
    bbnf_ir::passes::sets::compute_structural_alphabet(&mut ir);
});
```

### 1.2 Emission to `generated.rs`

`GrammarProfile` (`crates/tape/src/profile.rs:128-152`) exposes four
`&'static` slots the emitter populates:

- `structural_alphabet: &'static [u8]` — the singleton set.
- `structural_digraphs: &'static [(u8,u8)]` — digraph pairs.
- `structural_digraph_mask: [u64;4]` — first-byte bitmap.
- `structural_quote_classes: &'static [u8]` — quote set.

Current BBNF emission (`crates/core/src/grammar/generated.rs:27-49,
156-159`) confirms the pipeline fires end-to-end:

```text
static __GRAMMAR_PROFILE_ALPHABET: [u8; 28usize] = [33,34,37,38,39,40,41,42,43,44,45,46,47,58,59,60,61,62,63,64,91,93,96,117,123,124,125,206,];
static __GRAMMAR_PROFILE_DIGRAPHS: [(u8,u8); 17usize] = [(33,61),(38,38),(42,47),(45,62),(47,42),(47,47),(58,58),(60,60),(60,61),(61,61),(62,61),(62,62),(63,119),(64,123),(117,56),(124,124),(206,181),];
...
structural_alphabet: &__GRAMMAR_PROFILE_ALPHABET,
structural_digraphs: &__GRAMMAR_PROFILE_DIGRAPHS,
structural_digraph_mask: [17582233548629213184, 1161928703861587969, 0, 16384],
structural_quote_classes: &[],
```

BBNF has a 28-byte singleton set + 17 digraphs. Mining IS firing. Per
the emitter contract (`runtime/mod.rs:37-50`), the intended consumer is
`simd_scan::StructuralAlphabet::from_profile(&GRAMMAR_PROFILE)` followed
by `simd_scan::scan_structural(input, &alphabet) -> StructuralIndex`,
with the resulting index feeding the walker's cursor.

### 1.3 Runtime consumers — VERDICT: **INACTIVE (substrate-without-
consumer)**

Grep over `crates/core/src/grammar/generated.rs`:

- `scan_structural` — **0 hits**
- `StructuralAlphabet::from_profile` — **0 hits**
- `StructuralIndex` — **0 hits**
- `simd_scan::` — **0 hits**

Grep over the workspace shows the only non-test consumers are:

- `crates/json-prototype/src/simd.rs:21` — comment stating
  `scan_structural` is NOT used (the prototype picks a different path).
- `crates/simd-scan/tests/**` and `crates/simd-scan/benches/**` — the
  crate's own internal test / bench harnesses.

Symbol verification on the prebuilt bench binaries:

```text
nm .profile-target/release/deps/css_l4-0d1a22af4b4b8964 | grep -E 'simd_scan|scan_structural|StructuralIndex|KernelShape|from_profile'   → 0 lines
nm .profile-target/release/deps/json_monolithic-683fcdeaeb1021e7 | grep -E 'simd_scan|scan_structural|StructuralIndex'                    → 0 lines
nm .profile-target/release/deps/bbnf_monolithic-1cce73194c2882ba | grep -E 'simd_scan|scan_structural|StructuralIndex'                    → 0 lines
```

**Zero structural-scan symbols in any compiled bench binary.** The
entire `simd-scan` crate is dead weight at the link boundary — its
`StructuralIndex`, `NibbleLut`, `WideLut`, `KernelShape`, and the
per-arch kernels at `simd-scan/src/{avx2,avx512,neon,wasm,scalar}.rs`
never enter the hot path.

Similarly, `ConsumeToNextStructural` (CTNS) at
`crates/ir/src/passes/recognizers/consume_to_next_structural.rs` mines a
`CtnsLiftSet` that downstream emission could route through a bitmap
jump; at `crates/core/src/lower/mod.rs:236` the set is fed as
`ctns_lifts: std::collections::HashSet::new()` — an empty set,
unconditionally. CTNS is also inactive.

The W0b.A walker retirement removed the consumer end of the structural-
scan pipeline (the walker's `Cursor` was the intended reader of
`StructuralIndex`). Post-W0b, the mining pass still runs, the const
literals still emit, and `GrammarProfile` still carries the slots; no
shape emitter consults any of them. This is the classic profile of a
W0b-orphaned substrate.

### 1.4 Where input scanning actually happens at runtime

Every shape emitter at `crates/core/src/backend/rust/emitter/shapes/{
flat,wrap,inline,arglist,array,object,unordered,alt_dispatch,keyword,
dispatcher,pratt}.rs` scans the input via `skip_space` calls on
`__shape_support_<Grammar>::ScanState` and direct
`input.get(*p).copied()` loads. The `__regex_scan_<Parser>` function
(`generated.rs:1057-1107` for BBNF) is a pattern-dispatch shell that
pointer-matches the pattern string to an inlined per-DFA byte-by-byte
walk:

```text
fn __regex_scan_BbnfBootstrap(pattern, input, pos) {
    if ptr::eq(pattern, __DTA_REGEX_0) { run DFA_0 byte-by-byte }
    if ptr::eq(pattern, __DTA_REGEX_1) { run DFA_1 byte-by-byte }
    ...
}
```

Each DFA walks one byte per iteration via `input.get(__dfa_p)` + a
nested `match` on the DFA state. This is the 25.81-26.66% self-time
hotspot per `.profiles/samply/css_l4/{normalize,bootstrap,tailwind}/
profile.json.syms.json`; it dominates CSS L4 scan cost alongside a
secondary 8-12% share on Sheets stress + BBNF-on-css_l4.

### 1.5 Dense-alphabet scan design proposal

The A2 report already identifies "regex-scan specialisation via
HIR-driven CSS alphabet" as L3 (8-15% per fixture). The structural-
alphabet pre-pass is a different, additive lever:

**Proposal — structural-scan pre-pass resurrection.** Insert a
`simd_scan::scan_structural` call at the top of every emitted
`<Parser>::parse` body for grammars whose structural-alphabet
cardinality fits `KernelShape::NibbleLut` / `WideLut` (CSS L4 = 15-25
bytes → WideLut; JSON = 7 bytes → NibbleLut; BBNF = 28 bytes →
MultiCmp; Sheets = 22 bytes → MultiCmp). The resulting
`StructuralIndex { positions, kinds }` is threaded into `ScanState`
so two consumers can tap it:

1. **CTNS emission.** Route every mined `CtnsLiftSet` node through
   `idx.positions[slot]` jumps — the CTNS substrate is already in
   place, the lifter already admits the node set; wire `ctns_lifts:
   mine_outputs.ctns_lifts` in `lower/mod.rs:236` (currently
   hardcoded empty), and emit a `fast_scan_to_structural(input, p,
   state)` helper the regex shell dispatches to for CTNS-admitted
   patterns. This skips the byte-by-byte DFA walk for every pattern
   whose match boundary IS a structural byte — the canonical case for
   CSS L4 property values scanning to `;` / `)` / `}` / whitespace.

2. **Whitespace skip acceleration.** `skip_space` is the second
   universal hot-loop; on CSS L4 tailwind it contributes to the 7.3%
   `<CssL4Parser>::parse` shell self-time (the entry-shell includes
   every outer whitespace skip). When the structural alphabet
   includes whitespace, `*p = idx.positions[slot]` on a slot whose
   `kinds[slot]` is the first non-whitespace byte replaces the
   per-byte skip loop.

**Expected attribution delta:**

- CSS L4 `__regex_scan_CssL4Parser` (26% self-time) drops by
  ~40-50% of its own share on CTNS-admissible patterns. Per
  A2 profile, property-value scans (`[^;)}]+`) are the dominant
  DFA in the scanner; they're canonical CTNS candidates. Expected
  delta: 10-13% self-time reclaim on CSS L4.
- Sheets `__regex_scan_GoogleSheetsParser` (11.82% stress) drops
  similarly on identifier + number scans whose match boundary is
  `,` / `)` / `=` / whitespace. Expected delta: 4-6%.
- JSON negligible — `__regex_scan_JsonParser` is ~0% per A1 §2 (the
  JSON scanner is inlined into per-rule bodies); structural pre-pass
  pays the index-construction cost with nothing to reclaim.

**Critical dependency on A7/A8.** The pre-pass allocates
`Vec<u32>` + `Vec<u8>` of length proportional to input's structural-
byte density. On CSS L4 tailwind (~3.6 MB, ~20% structural density),
that's ~720K × 5 bytes = 3.6 MB of fresh allocation per parse. The
allocation mass is additive to the tape substrate's push_structural
(A7) + finalise (A8) mass; the pre-pass only nets a win if those
columns are already being inlined per A7/A8. Sequencing order:
A7/A8 land first; structural-scan pre-pass lands after, with the
allocation paired against the now-inlined tape columns on the same
mi_heap page so the allocator sees one churn pattern not two.

**Orthogonality to A7/A8.** push_structural is about tape WRITES
after parsing; structural-scan is about INPUT READS before
dispatching. They touch disjoint hot-path axes.

### 1.6 Part 1 verdict

**Status: INACTIVE substrate (W0b-orphaned).** The mining pass fires,
the `GrammarProfile` slots populate, and the `simd-scan` kernels
compile — but zero emitted code consumes any of it. Symbol verification
in three bench binaries confirms the absence.

**Expected reclaim if activated (with A7/A8 as prerequisites):**
- CSS L4: 10-13% self-time (regex_scan reduction via CTNS routing).
- Sheets: 4-6% self-time.
- JSON + BBNF: negligible-to-zero.

**Wave priority:** AY.W2 or AY.W3 (after A7/A8 land inline tape
writes). Landing before A7/A8 risks a net regression on grammars
where the index construction costs exceed the regex_scan delta.

## Part 2 — Pratt flattening

### 2.1 Current emission shape

`emit_parse_pratt` at
`crates/core/src/backend/rust/emitter/shapes/pratt.rs:66-428` emits a
reducer-compound tree per Pratt rule. For Sheets `1 + 2 * 3 - 4` the
shape (per the docstring at `pratt.rs:27-42`):

```text
[0] Rule    (outer Pratt)         span=0..N  child=7
[1] ...operand '1' records
[2] Span    variant=0 payload='+'
[3] ...operand '2'
[4] Span    variant=0 payload='*'
[5] ...operand '3'
[6] Rule    (reducer 2*3)         child→[3], variant='*'
[7] Span    payload='-'
[8] ...operand '4'
[9] Rule    (reducer 1+(2*3))     child→[1], variant='+'
[10] Rule   (reducer ((1+(2*3))-4)) child→[1], variant='-'
```

The outer Rule compound's `child_off` is patched (pratt.rs:416-425) to
point at the final reduced root (`[10]`). Operand dispatch recurses
into the grammar's value-position dispatcher
(`dispatcher_fn_ident + "__value"`, pratt.rs:91-97). Operator payload
is a 1-byte arena write via `push_leaf_with_arena_payload` (landed
W0a.2.l, `64d6ab2f`).

### 2.2 Current Pratt rule inventory

Per `generated.rs` grep: **7 emitted `parse_pratt_<Grammar>_<rule>`
functions** plus each rule's paired `PRECEDENCE_LUT_<rule>` + `
PRECEDENCE_ENTRIES_<rule>` consts.

BBNF: `value_mul`, `value_add`, `value_path`, `value_cmp`, `value_and`,
`value_or`, `binary_factor` (7 LUTs). Sheets emits its own set per its
formula ladder (not in the current BBNF-bootstrapped `generated.rs`
snapshot; cross-referenced per A3 §4 bench attribution).

### 2.3 Consumer contract

Primary consumer: `collect_binary_operands` at
`crates/core/src/lower/expression.rs:546-595`. The consumer has two
branches:

1. **Pratt reducer-chain branch** (post-W0a.2.l path, routed via
   `collect_pratt_reducer_chain` at `expression.rs:628`): walks the
   reducer tree by detecting Rule records whose `variant_idx` equals
   one of the mined op_discriminants, recursing into each reducer's
   LHS/RHS to flatten into `[operand, op_leaf, operand, op_leaf, ...]`.
2. **Walker-era iteration-pair branch** (pre-Pratt fallback, legacy
   transparent wrapper layout) — still present for the non-Pratt shape
   rules whose bodies lower to `Seq` / `Repeat` pairs.

Second consumer: `gorgeous` pretty-printer derives (via the visitor
path `emit_parse_pratt_visitor` at `pratt.rs:445-666`). The visitor
dispatches `visitor.begin_pratt() → operand_end → operator → ...`
calls; the visitor consumer never sees the reducer-compound layout —
it receives only the synthesised event stream. The visitor path is
immune to tape-layout changes.

### 2.4 Why W0a.2.k flat-tape broke parity

Per PROGRESS.md commits `015d02af`/`1ab22a9d`/`7c3ea838`, reverted as
`f585ce37`/`4178254a`/`3256858d`:

- W0a.2.k removed the reducer-compound emission and replaced it with a
  flat `[operand, op, operand, op, ...]` sequence at the outer Pratt
  compound's children.
- The agent's rationale: "variant_idx = op_discriminant was corrupting
  `rule_kind()` dispatch" (PROGRESS.md:362).
- Regression: `css_l4_parity` 11/16 failed; `sheets_parity` 14/25
  failed (PROGRESS.md:367-368).

Root cause per PROGRESS.md:429-432 — `lower_binary_factor` +
`collect_binary_operands` walked the Pratt compound assuming the
reducer-tree shape; the flat layout confused the reducer's
`rule_kind()` lookup, dispatching reducer-compound-as-operand into a
`float_lit` dispatch path that panics with "unknown leading byte '"'
for rule_kind float_lit".

The revert explicitly preserved "walker-parity reducer-compound
emission IS the consumer contract for `*_parity.rs` semantic
harnesses" (PROGRESS.md:374-375). W0a.2.l (`64d6ab2f` + `e5ff835e` +
`34be629e` + `7d2fa1b8`) re-landed the per-rule LUT + Option B+C
miner + 1-byte arena payload API WITHOUT the flat-tape change.

### 2.5 Current Pratt self-time attribution

Per A3 §6 (fresh 2026-04-20 01:50 profiles):

- **Sheets** `parse_stress`: 6-arm Pratt ladder
  `{exp,comparison,mul,add,concat,unary}_expr`
  contributes ~8-9% combined self-time (each arm 0.8-2.8%, summing
  ~8-9% across arms per `profile.json.syms.json`).
- **BBNF** `parse_pratt_BbnfBootstrap_binary_factor` + Pratt's share
  of `parse_flat_*_mapped_factor`: 1.7-3.2% on every bbnf entry
  (9-entry avg 1.62% for the explicit pratt symbol alone).
  `mapped_factor` (5.35% 9-entry avg) covers the rule-root
  dispatcher that RECURSES into the Pratt frame; its self-time is
  attributable to the `Alt` shape's first-byte dispatch, not the
  Pratt reducer.
- **CSS L4**: zero `parse_pratt_*` symbols in the top-10 on any
  fixture (normalize/bootstrap/tailwind per A2 §2). CSS L4's
  `@media (min-width: 600px)` arithmetic goes through a pratt tower
  but the self-time is below the A2 top-10 threshold (~1%).
- **JSON**: zero Pratt symbols. JSON has no operator chains.

### 2.6 Pratt flattening — design verdict

Three options, ranked by KISS + invariant compatibility:

**Option A (hybrid).** Emit flat inner `[operand, op, operand, op,
...]` at the outer Pratt compound BUT preserve a reducer-compound
"header" node the walker re-reads. Net: two compounds per operator
boundary (outer Pratt + reducer header) vs current three
(outer + reducer inner + operand sub-tree). Consumer-side
`collect_binary_operands` reads the header's metadata to re-
construct the reducer tree at lower time. **Verdict: REJECTED.**
Doubles the emit-time work while still requiring consumer
re-construction. The per-op cost per A3 §6 is ≤ 2% per arm; a
flattening that trades two writes for one reader-side reconstruction
does not net a win at the current self-time share.

**Option B (full flat + consumer rewrite).** Emit flat tape,
simultaneously rewrite every `*_parity.rs` consumer +
`lower_binary_factor` + every gorgeous visitor. **Verdict:
REJECTED.** Per W0a.2.k archaeology, the consumer-side surface is
broad (6 harness files across 3 grammars + the lower/expression.rs
chain). Landing is an atomic cross-crate rewrite with high blast
radius; the A3 profile share (1-9% total across grammars) does not
justify the risk.

**Option C (emit-inline, preserve reducer tree).** Per-op PUSH
operations stay as-is; optimise their amortised cost via:

- Pre-compute the op-stack capacity from the mined per-rule chain
  depth (pratt.rs:215-216 uses `with_capacity(4)`; the OperatorChain
  miner already knows the exact per-rule rung count).
- Inline `push_leaf_with_arena_payload` per call site — the 1-byte
  arena write (pratt.rs:360-370) pays a cross-crate boundary on
  every op emission; `#[inline(always)]` the tape API matches A7's
  push_structural treatment.
- Write the op discriminant directly into the payload column rather
  than via a scratch `arena_mut().push(op_discriminant)` +
  `arena_off` lookup. The grammar knows at emit time that every
  Pratt op_leaf's payload is exactly 1 byte; the arena round-trip
  is unnecessary indirection.

**Verdict: Option C is the KISS path**. Zero consumer changes; zero
tape-layout changes; zero invariant exposure. The win is additive to
A7/A8 (both touch `push_leaf_with` + `push_compound`; Pratt is just a
specialised call pattern over those primitives).

### 2.7 Priority rationale

Pratt's 9-entry avg self-time of 1.62% (BBNF `binary_factor`) and 8-9%
combined (Sheets stress 6-arm ladder) compares against:

- A7 (`push_structural` inline): 29.75% 9-entry avg → reclaims
  20-30% per lever.
- A8 (`finalise` fuse): 19.35% avg → reclaims 10-18% per lever.
- A9-Part1 (CTNS activation): 10-13% on CSS L4.

Pratt flattening does NOT warrant W1-priority. The self-time share is
≤ 10% on the hottest grammar (Sheets stress) and ≤ 3% on the typical
BBNF entry. Landing Option C as an incremental fold into A7 (the
push_leaf_with path is shared) is the right move — no standalone
wave, no flat-tape redesign, no consumer rewrite.

### 2.8 Part 2 verdict

**Current Pratt self-time:** 1.62% (BBNF 9-entry avg) to 8-9% (Sheets
stress combined ladder). Per `.profiles/samply/google_sheets_
monolithic/parse_stress/profile.json.syms.json` and
`.profiles/samply/bbnf_monolithic/*/profile.json.syms.json`.

**Warrants W1-priority: NO.** Pratt cost is dwarfed by tape-substrate
cost (push_structural 30%, finalise 19%). Option C (inline the
existing tape primitives + hoist op_stack capacity from mined
chain-depth) folds naturally into A7's inline treatment of
`push_leaf_with` — Pratt's op-leaf emission pays the same cross-crate
boundary every other shape pays.

**Ship as:** inline op_leaf emission in AY.W1 under A7/A8's
tape-substrate-inline umbrella. No standalone Pratt wave. No tape-
layout flattening. No consumer rewrite.

## Artefacts

- `crates/ir/src/passes/sets/structural_alphabet.rs` — mining pass
  (W1.γ-restricted).
- `crates/ir/src/passes/recognizers/consume_to_next_structural.rs` —
  CTNS lifter (substrate present, consumer empty at
  `lower/mod.rs:236`).
- `crates/tape/src/profile.rs:128-152` — GrammarProfile slot
  definitions.
- `crates/core/src/grammar/generated.rs:27-49,156-159` — BBNF
  structural alphabet emission (28 singletons + 17 digraphs).
- `crates/simd-scan/src/{alphabet,lib,avx2,avx512,neon,wasm,scalar}.rs`
  — kernel crate; zero non-test consumers in the workspace.
- `crates/core/src/backend/rust/emitter/shapes/pratt.rs` — current
  reducer-compound emitter (tape + visitor paths).
- `crates/core/src/lower/expression.rs:546-720` — Pratt reducer-chain
  walker in `collect_binary_operands` + `collect_pratt_reducer_chain`.
- `crates/ir/src/passes/recognizers/operator_chain.rs` — per-rule
  Option B+C miner (landed W0a.2.l per commits `34be629e` /
  `7d2fa1b8`).
- Commits:
  - W0a.2.k landed `015d02af`/`1ab22a9d`/`7c3ea838`; reverted
    `f585ce37`/`4178254a`/`3256858d`.
  - W0a.2.l landed `64d6ab2f`/`e5ff835e`/`34be629e`/`7d2fa1b8`.
- `.profiles/samply/css_l4/{normalize,bootstrap,tailwind}/
  profile.json.syms.json` — 25.81-26.66% regex_scan self-time.
- `.profiles/samply/google_sheets_monolithic/parse_stress/
  profile.json.syms.json` — 11.82% regex_scan, ~9% combined Pratt
  ladder self-time.
- `.profiles/samply/bbnf_monolithic/*/profile.json.syms.json` —
  1.7-3.2% `parse_pratt_BbnfBootstrap_binary_factor` per entry.
- nm verification on
  `.profile-target/release/deps/{css_l4,json_monolithic,bbnf_monolithic}-*`
  — 0 `simd_scan` / `scan_structural` / `StructuralIndex` symbols.
