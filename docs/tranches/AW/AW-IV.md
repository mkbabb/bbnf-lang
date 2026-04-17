# Tranche AW-IV — Interpreter Abrogation

AW-III delivered the *scaffold* for a flattened tape automaton: walker
specialised per grammar, stage-1 SIMD crate shipped, dual cursor
populated, fused SoA writes, the five emitter-mined consumer substrates,
the universal named-type binding, the Pratt LUT emission. Every gate met
except the throughput gate: 0 of 17 parse entries beat post-AU.

The reason: the scaffold is interpreted. The walker's per-state arms make
3–7 cross-crate calls per byte; each `match table.states[N]` arm body
re-loads the variant's data at runtime; the regex DFA is itself a flat-
transition interpreter at 32% self-time; the wire-contract pipeline
silently drops mined data on the way to `GRAMMAR_PROFILE` (every
consumer slot is `&[]` despite the IR mining the data correctly); seven
of the eight consumer activations landed as substrate without consumer.

AW-IV abrogates the interpreter. Six waves; one tranche; no deferrals.
Hard gate: every parse entry exceeds post-AU at AW-IV close. The five
levers compound multiplicatively (R3 §5; SYNTHESIS-2 §7; the post-AW-III
samply attribution); JSON projects to 2200–4200 MB/s on twitter, CSS to
1500–2500 MB/s on bootstrap, the corpus geomean above post-AU.

## Architectural thesis

The post-AW-III samply on JSON twitter, sample count 4173:

| Self % | Symbol | Category |
|-------:|--------|----------|
| 36.64 | `__dta_walker_inline::run::<DtaDfaScanner>` | per-grammar specialised walker |
| 31.92 | `<DtaDfaScanner as RegexScanner>::scan` | runtime DFA interpreter |
| 12.13 | `bbnf_simd_scan::neon::scan` | stage-1 SIMD pre-pass |
|  7.79 | `bbnf_tape::finaliser::finalise` | tape close |
|  4.46 | `bbnf_tape::psi::write_decoded` | PSI stage-B writes |

The walker symbol IS the new dispatcher. Its 36.64% covers per-arm
runtime-`match`-against-`unreachable_unchecked` data unpacking + 3–7
cross-crate calls per arm. Removing `dispatch_one`'s match shifted the
dispatch cost into the per-arm body and the helper-call boundaries; it
did not eliminate it.

The 31.92% scanner self-time IS the runtime DFA interpreter. The W1.8
"scanner closure" replaced the global HashMap-with-string-key with a
HashMap-with-pointer-key — saved the SipHash, kept the RwLock, and never
addressed the interpreter loop inside `Dfa::find_at`. Per-pattern
straight-line specialised match functions never shipped.

The 12.13% bbnf_simd_scan self-time IS being paid on a degenerate-output
index. The CSS L4 mining produces `[0..127]` as the structural alphabet
(every printable byte structural), so the structural index has ~one entry
per input byte. JSON's mining produces 5 singletons + 1 quote class — but
those values never reach `GRAMMAR_PROFILE`; the wire-contract pipeline
silently drops them and the runtime const stores `&[]`. Stage-1 runs;
stage-1 has nothing to amortise against.

These three failures compound. Fixing any one in isolation moves
throughput marginally. Fixing all three (W1) plus wiring the seven dead
consumer activations (W3) plus the layered work (W4–W5) compounds
multiplicatively to the projected ceiling.

**The binding rule of this tranche: no new runtime indirections.** The
fix for AW-III's interpreter shape is *not* to add more indirection
surfaces (function-pointer fields on `DtaState`, trait objects, runtime
table dispatch, lazy initialisation cells). Each AW-III consumer that
landed as substrate-without-consumer did so because it was wired
*through* an indirection — `lookup_precedence(...)` instead of a direct
`PRECEDENCE_LUT[byte]` load; `scanner.scan(pattern, ...)` instead of a
direct `__dfa_match_<grammar>_<state_idx>(...)` call. AW-IV emits hot
logic *directly* into the per-grammar walker. The runtime tape (`bbnf-
tape::driver`) survives only as the cold-path replay surface for AX —
correctness ground truth, not the parse hot path. Every wave's hard
gate enforces this rule via `nm` symbol-presence assertions.

## Invariants

1. **No deferrals, regardless of newfound scope.** Inherited from
   `docs/instructions/README.md` and AW-III. Reiterated here as the
   binding constraint that AW-III was held to *with qualification* and
   AW-IV is held to without. Every wave's hard gate is verifiable
   (symbol present/absent under `nm`, samply attribution, wire-contract
   end-to-end test); a wave does not close on agent-claim, only on the
   ledger. Scope-reveal under contact triggers re-plan-with-more-agents
   per the parallel-orchestration contract; never silent forward-routing.
2. **Substrate-with-consumer is one unit of work.** Every emitter pass,
   IR field, const slot, or runtime variant introduced in this tranche
   has a runtime consumer in the hot path — verified by samply
   attribution or a wire-contract test that asserts data flow from
   mining through emit to runtime use. Per `docs/instructions/README.md`
   §Code-discipline.
3. **Cross-crate inlining is verified, not assumed.** Per
   `docs/instructions/README.md` §Performance-claims. `nm
   target/release/deps/<bench>` confirms hot-path helper symbols are
   absent from the per-grammar walker symbol; the function-call boundary
   does not become the new dispatcher. Workspace LTO + `#[inline(always)]`
   on hot helpers, OR per-grammar inline emission, are the two
   approaches; pick one and verify.
4. **Wire-contract pipelines have end-to-end tests.** Per
   `docs/instructions/README.md` §Architecture-invariants. Every
   *IR mining → emitter pass → `pub const` literal → runtime consumer*
   pipeline carries one test asserting the entire path: fixture grammar
   with known mineable data, runtime const asserted to contain the mined
   values, runtime invocation asserted to consume non-trivially.
5. **§6 generalisation invariant** — inherited from AW-III. Every
   emitter pass triggered by IR-structural properties; per-grammar
   OUTPUT comes from per-grammar IR; per-grammar MECHANISM does not
   exist.
6. **Hoist emitter-known data into emitted code.** Per
   `docs/instructions/README.md` §Architecture-invariants. The emitter
   carries literals into emitted bodies, not runtime indirections
   through the source array.
7. **Bench between every wave.** Sidecar per wave; W6 aggregates.
   Samply attribution per wave cited from `.profiles/samply/aw4-wN/`.
   Cross-crate symbol verification via `nm` per wave.

## Wave schedule

Strict sequencing: each wave opens only after the prior wave closes
per its hard gate. The dependency is not administrative — it is
substantive: a downstream wave that fires onto an indirected hot path
verifies nothing.

| Wave | Scope | Agents | Opens after | Hard gate |
|------|-------|--------|-------------|-----------|
| W1 | Interpreter abrogation core: hoist DtaState into arm bodies + fix structural-alphabet mining + fix wire-contract emission path + DFA codegen with **direct named calls** + scanner-trait elimination | 4 parallel | (open) | `dispatch_one` AND `<DtaDfaScanner as RegexScanner>::scan` AND `Dfa::find_at` symbols absent from `nm`; per-grammar `__dfa_match_<grammar>_<state_idx>` symbols present; structural index non-trivial on every grammar (mined data reaches `GRAMMAR_PROFILE` const); wire-contract end-to-end tests pass; JSON twitter ≥ 600 MB/s |
| W2 | Per-grammar inline emission of hot helpers (helper bodies emitted directly into walker arms; LTO is verification cover, not strategy) | 2 parallel | W1 closed | `emit_leaf`, `push_compound_fused`, `push_leaf_fused`, `advance_or_pop_with`, `close_compound`, `trim_with_pattern`, `psi*push` symbols absent from per-grammar walker `nm`; `cargo asm` confirms no `bl`/`call` to helper symbols in walker disassembly; samply confirms zero cross-crate self-time on hot path; JSON twitter ≥ 1100 MB/s |
| W3 | Five emitter-mined consumer activations + CTNS lifter + bounded Regex via `pattern.last_byte_set ⊆ structural_alphabet` | 5 parallel | W2 closed (consumers wire into a flat hot path; firing them onto indirected substrate defeats them) | every `GRAMMAR_PROFILE` slot non-empty for grammars where the IR has mineable data; samply confirms each consumer symbol present (or its prior dispatcher absent); wire-contract end-to-end tests pass for every slot; JSON twitter ≥ 2000 MB/s |
| W4 | Granular SIMD widening + scanner cluster + bloom + GADT dedup + grammar-level pattern hoisting + document-parallel fork over stage-1 index | 4 parallel | W3 closed (multipliers stack onto consumer-active flat path; on indirected substrate they produce no measurable gain) | AVX2 scan ≥ 15% drop on x86_64; tailwind 4c sub-linear-to-linear scaling; `GRAMMAR_PROFILE.list_rules` non-empty for CSS L4 |
| W5 | `reduce_column<C, R>` + 4-lane SIMD pack + sonic-rs + lightningcss parity harnesses + cost-model grid sweep | 3 parallel | W4 closed | reducer ≥ 6× scalar baseline OR per-arch rationale; both parity harnesses zero-divergence + CI-gated |
| W6 | FINAL + 19-entry bench matrix + multi-wave aggregator | 1 serial | W5 closed | **every parse entry exceeds post-AU**; `post-AW-IV.json` exists; `FINAL-IV.md` exists; verification ledger complete per wave |

## Phases

### W1 — Interpreter abrogation core

Four parallel agents, file-disjoint. The load-bearing wave; without it
no subsequent wave matters because the helper-call boundary remains the
dispatcher.

#### W1.1 — Hoist `DtaState` data into arm bodies

Owner: `crates/core/src/backend/rust/emitter/dta_walker/lower_state.rs`.

Today every per-state arm in the W4-emitted walker begins with:
```rust
let (pattern, payload) = match table.states[N] {
    DtaState::Regex { pattern, payload } => (pattern, payload),
    _ => unsafe { ::core::hint::unreachable_unchecked() },
};
```
The emitter knows at codegen time that state N is `DtaState::Regex {
pattern: __DTA_REGEX_K, payload: PayloadKind::I64 }` (the lifter writes
those exact values into `DTA_TABLE.states[N]`). Replace with literal
binding:
```rust
let pattern = __DTA_REGEX_K;
let payload = ::core::option::Option::Some(::bbnf::runtime::tape::PayloadKind::I64);
```
Same shape for `Literal`, `ByteDispatch` (table reference becomes the
emitted `[DtaStateId; 256]` literal), `Seq`, `AltLinear`, `Repeat`,
`ShuntingYard`, `WsTrim`, `ConsumeToNextStructural`, `ClassifyByte`.
Per-variant lowering function emits a literal binding for every field
the source `DtaState` variant carries; the runtime `match
table.states[N]` and the `_ => unreachable_unchecked()` arm both delete.

The pass mechanically reads `table.states[N]` at codegen time and
formats each field's literal token. No grammar branch.

**Hard gate**: `grep -n 'match table.states\[' crates/core/src/grammar/generated.rs` returns zero matches inside `__dta_walker_inline`. `cargo expand` confirms each arm body opens with literal `let` bindings.

#### W1.2 — Fix structural-alphabet mining definition

Owner: `crates/ir/src/passes/sets/structural_alphabet.rs`.

Today the mining over-flags. CSS L4 mines `[0..127]` (every printable
byte structural) because the pass appears to flag every byte appearing
in any rule's FIRST set. The right definition is the simdjson
definition: bytes that *delineate parse-tree structure*, not bytes that
*appear in any rule*. Concretely:

- Single-byte `Literal` terminators (`{`, `}`, `;`, `:`, etc.).
- `Repeat` separator bytes (`,` in CSV-style lists).
- `Alt` discriminator bytes when the Alt's branches' FIRST sets are
  single-byte literals.
- The first byte of each `digraph` pair (`/` for `/* */`, etc.).
- **EXCLUDED**: bytes inside character classes / regex content / string
  content / inside any rule whose body is a `Regex` or `Literal` of
  length > 1.

Apply the corrected definition; CSS L4 mines ~20 bytes (the actual
delimiters) instead of 128. Bootstrap.css structural density drops to
the measured ~7%; stage-1 amortises across the other 93%.

**Hard gate**: per-grammar `STRUCTURAL_ALPHABET.singletons.len()` ∈ a
sane range — JSON ≤ 8, CSS L4 ≤ 25, BBNF ≤ 15, Sheets ≤ 12. The
`crates/ir/tests/structural_alphabet_extended.rs` test suite extends
with per-grammar assertions on cardinality.

#### W1.3 — Fix wire-contract emission path

Owner: `crates/ir/src/passes/profile.rs`,
`crates/core/src/backend/rust/emitter/profile.rs`,
`crates/core/src/grammar/generated.rs` (regen).

Today the IR mining produces `5 singletons + 1 quote class` for
BbnfBootstrap (per `crates/ir/tests/structural_alphabet_extended.rs`),
but the runtime `pub const GRAMMAR_PROFILE` at `generated.rs:31` shows
`structural_alphabet: &[]`, `structural_quote_classes: &[]`, etc. The
projection from IR to emitter to const literal silently drops data on
the way through.

Trace the failure: `crates/ir/src/passes/profile.rs::emit_grammar_profile`
or its sibling at `crates/core/src/backend/rust/emitter/profile.rs`
constructs the `GRAMMAR_PROFILE` literal — find where the mined
`StructuralAlphabet` reaches that function (or doesn't). Wire it
through. Same for `shape_dict`, `keyword_tables`, `branch_priors`,
`active_columns`, `dedup_eligible_rules`, `list_rules`, `digraph_mask`,
`quote_classes`.

Add `crates/core/tests/grammar_profile_wire_contract.rs` per the
*Wire-contract pipelines have end-to-end tests* invariant: a fixture
grammar with known mineable values; assert
`<Grammar>::GRAMMAR_PROFILE.structural_alphabet` contains those values;
assert `<Grammar>::GRAMMAR_PROFILE.shape_dict.len() > 0` for grammars
whose IR has shape repetitions; etc. One test per slot per grammar.

**Hard gate**: every `GRAMMAR_PROFILE` slot is non-empty for any
grammar whose IR has mineable data for that slot. The wire-contract
test asserts the literal at `generated.rs` matches the IR's mined value
byte-for-byte for every slot.

#### W1.4 — DFA codegen with direct named calls + scanner-trait elimination

Owner: `crates/core/src/backend/rust/emitter/dfa_codegen.rs` (new),
`crates/core/src/backend/rust/emitter/dta_walker/lower_state.rs` (Regex
arm rewrite to direct named call),
`crates/bbnf-tape/src/driver.rs` (trait deletion),
`crates/core/src/backend/rust/emitter/grammar.rs` (scanner-impl
deletion).

**Direct emitted calls — no function pointers, no trait dispatch, no
`DtaState` field for the match function.** A function-pointer field on
`DtaState::Regex` would preserve a runtime indirect-call boundary even
after W1.1 hoists the field load out of the runtime `match
table.states[N]`. The hot path must call the emitted DFA function
*directly by name* — only then does LLVM inline the body at the call
site without depending on cross-function pointer-chasing analysis.

For every `&'static str` pattern reachable from `DTA_TABLE.states`,
lift the NFA via `parse-that::regex::nfa`, determinise via
`parse-that::regex::dfa::Dfa::compile`, then **emit** the DFA's
transition table as inline Rust:
```rust
#[inline]
fn __dfa_match_<grammar>_<state_idx>(input: &[u8], pos: usize) -> Option<u32> {
    let mut state: u32 = 0;
    let mut pos = pos;
    let mut last_match: Option<u32> = None;
    loop {
        let b = match input.get(pos) {
            Some(&b) => b,
            None => break,
        };
        match state {
            0 => match b {
                <byte_class_0_arm> => state = <next_0_0>,
                <byte_class_1_arm> => state = <next_0_1>,
                _ => break,
            },
            1 => match b { /* ... */ },
            /* ... */
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        }
        pos += 1;
        if <state_is_accepting> {
            last_match = Some(pos as u32);
        }
    }
    last_match
}
```

The walker's Regex arm — emitted by `dta_walker/lower_state.rs::emit_regex_arm`
— becomes a direct named call to the W1.4-emitted function for *that
specific state index*:
```rust
let match_len = match __dfa_match_<grammar>_<state_idx>(input, *pos as usize) {
    ::core::option::Option::Some(n) => n,
    ::core::option::Option::None => break 'step ::core::result::Result::Err(/* ... */),
};
```
The emitter knows at codegen time that arm-for-state-N matches pattern
K's DFA, so it emits the literal name `__dfa_match_<grammar>_<state_idx>`.
No field load, no function pointer, no trait dispatch. The Regex pattern
literal (`&'static str`) stays in `DtaState::Regex.pattern` for replay/
debug only — the cold-path `dispatch_one` consults it; the hot path
never reads it.

`RegexScanner` trait deletes from `bbnf-tape/src/driver.rs`. The `__S:
RegexScanner` generic on the emitted walker disappears (the walker
function signature loses the `scanner: &__S` parameter). The HashMap +
`OnceLock<RwLock<...>>` + `Box::leak` triplet at `grammar.rs:373-401`
deletes entirely. `DtaDfaScanner` ZST + its impl + the `DTA_SCANNER`
const all delete. `parse_that::regex::dfa::Dfa::find_at` is no longer
referenced from any per-grammar walker — it survives only as the
algorithm-source for the W1.4 emitter pass and as the cold-path
`dispatch_one` Regex arm's fallback (replay surface).

**Hard gate**: `nm target/release/deps/json_monolithic-* | grep -E
'(DtaDfaScanner|RegexScanner|find_at|cached_dfa)'` returns empty.
Per-grammar `__dfa_match_<grammar>_<state_idx>` symbols present for every
regex pattern in `DTA_TABLE`. `cargo expand` confirms the walker's
Regex arms emit literal-named direct calls (no `state.match_fn` field
access, no trait method dispatch). JSON twitter samply: scanner self-
time drops from 31.92% to ≤ 5%.

### W2 — Per-grammar inline emission of hot helpers

Two parallel agents. **Wave-sequencing constraint: W2 opens only after
W1 closes.** W2 verifies absence of cross-crate dispatch in the
*post-W1* hot path; if W1's hoisting and direct DFA calls are not
landed, W2's verification has nothing meaningful to assert.

**The strategy is to emit the helper bodies inline into the walker's
per-state arms — not to rely on LLVM cross-crate inlining via LTO.**
LTO + `#[inline(always)]` are verification fallbacks (and a useful
belt-and-suspenders for any helper that genuinely should remain
shared), not the primary mechanism. The hot path becomes structurally
flat at the source level; LLVM's job is downstream optimisation, not
proving inlinability across crate boundaries through opaque
function-call indirection.

#### W2.1 — Inline-emit hot helper bodies into walker arms

Owner: `crates/core/src/backend/rust/emitter/dta_walker/lower_state.rs`
(per-arm helper inline emission);
`crates/core/src/backend/rust/emitter/dta_walker/helpers.rs` (helper-body
emit fragments);
`crates/bbnf-tape/src/driver.rs` (cold-path replay-surface helpers
retained; hot-path helper bodies migrated into the emitter as inline
fragments).

Every helper currently called from the per-state arms gets *its body
emitted directly into the calling arm* by the walker emitter. The
helper functions in `bbnf-tape/src/driver.rs` survive ONLY as the
cold-path `dispatch_one` replay surface — they are not reached from
any per-grammar walker. The helpers to inline-emit:

- `push_compound_fused` → 1 capacity check + 7 unchecked column stores
  + length increment, emitted inline.
- `push_leaf_fused` → 1 capacity check + N unchecked column stores +
  length increment, emitted inline.
- `emit_leaf` → `push_leaf_fused` body + frame_depth bump, emitted
  inline.
- `emit_leaf_with_payload` → `push_leaf_fused` body + frame_depth bump
  + payload arena reserve, emitted inline.
- `advance_or_pop_with` → frame stack peek + counter advance OR pop +
  next-state lookup, emitted inline. The W1.1 hoisting collaborates: at
  codegen time the emitter knows which Frame layout the current arm
  unwinds into, so the inlined body specialises per arm.
- `advance_seq_fast` → already `#[inline(always)]`; bodies migrate into
  the calling arms verbatim.
- `close_compound` → frame_depth peek + sib_skip patch + length
  increment, emitted inline.
- `trim_with_pattern` → direct call to the W1.4-emitted DFA match
  function for the `@ws` pattern (per-grammar), inlined; no
  `scanner.scan` indirection survives.
- `psi.push` → `PayloadJob` construction + `payload_jobs` push,
  emitted inline.
- `handle_repeat_failure` → emitted inline at the outer-loop error
  handler; the emitter knows at codegen time which Repeat states the
  walker contains and emits the match arms directly without dispatch
  through a runtime-known state id.

The emitter's `dta_walker/helpers.rs` becomes a library of TokenStream
fragments — `emit_push_compound_fused_inline()`,
`emit_advance_or_pop_inline(frame_kind: ...)`, etc. — that the
per-arm lowering at `lower_state.rs` splices into each arm's body
verbatim. No fn-call boundary remains in the hot path.

#### W2.2 — Verification: workspace LTO + `nm` + `cargo asm`

Owner: `Cargo.toml` (workspace bench-profile LTO config);
verification scripts.

Add `[profile.bench] lto = "fat"` and `codegen-units = 1` to the
workspace `Cargo.toml` as **belt-and-suspenders verification cover**
— if any helper body slipped through the W2.1 inline-emit migration
and remained as a cross-crate call, LTO collapses it and `nm`
confirms its absence. LTO is not the strategy; it is the safety net
that catches what the inline-emit migration missed.

Annotate any helper that genuinely *must* remain shared (replay-surface
helpers used by AX) with `#[inline(always)]` so that the cold path's
shape doesn't degrade either.

**Hard gate**: `nm target/release/deps/json_monolithic-* | grep -E
'(emit_leaf|reserve_compound|push_compound_fused|push_leaf_fused|advance_or_pop_with|advance_seq_fast|close_compound|trim_with_pattern|first_ws_pattern|handle_repeat_failure|psi[0-9a-f]*push)'`
returns empty for the hot-path symbols (cold-path `dispatch_one`
references may remain — that's the AX replay surface). `cargo expand`
confirms the per-grammar walker arm bodies contain the helper logic
inline (struct stores, length increments, frame stack operations
visible in the source TokenStream output). `cargo asm` on the
walker symbol confirms no `bl` / `call` instructions targeting the
helper symbols. JSON twitter samply: zero cross-crate self-time on the
hot path; throughput ≥ 1100 MB/s.

### W3 — Consumer activation completion

Five parallel agents, file-disjoint. Every consumer the AW-III W6
substrate emits gets wired in this wave.

#### W3.1 — ShapeRef walker compound-emit consumer

Owner: `crates/ir/src/passes/recognizers/shape_dict.rs` (mining
recalibration); `crates/core/src/backend/rust/emitter/dta_walker/lower_state.rs`
(walker compound-emit branch).

Today `SHAPE_DICT` emits empty for every grammar — the mining pass does
not recognise shape repetitions in the post-W4 walker shape. Recalibrate:
mine on the pre-walker IR (compounds the lifter would emit), not
post-walker artefacts. CSS L4 should produce ≥ 13 entries (per AW-IV-as-
originally-planned), JSON should produce ≥ 4, BBNF rule-shape entries.

Walker compound-emit branch consults `SHAPE_DICT.lookup(shape_hash)`
inside the W4-emitted specialised walker arm:
```rust
if let Some(ref_idx) = SHAPE_DICT.lookup(shape_hash) {
    push_shape_ref(columns, frame_depth, stack, span_lo, span_hi, ref_idx, packed_payload);
} else {
    let rec_idx = push_compound_fused(columns, frame_depth, stack, kind, span_lo);
    /* children */
    close_compound(columns, frame_depth, stack, rec_idx, span_hi);
}
```

W2 inlining applies — the `push_shape_ref` body inlines into the arm.

**Hard gate**: `SHAPE_DICT` non-empty for at least CSS L4, JSON, BBNF;
samply on `bootstrap.css` confirms `push_shape_ref` symbol present (or
inline body visible via `cargo asm`); bootstrap declaration record count
drops ≥ 30%.

#### W3.2 — PHF threshold lowering + AltLinear consumer

Owner: `crates/core/src/backend/rust/emitter/keyword_dispatch.rs`
(threshold + emission); `crates/core/src/backend/rust/emitter/dta_walker/lower_state.rs`
(AltLinear consumer).

`PHF_MIN_BRANCHES` drops to 3 (catches JSON's `true|false|null`, BBNF's
8 directives, CSS's 9-branch `colorType`, every reasonable Alt). The
`emit_keyword_phf` pass now produces non-empty `KEYWORD_PHF` tables on
every primary grammar.

Walker's `AltLinear` arm consults the PHF inline:
```rust
if let Some(state_id) = KEYWORD_PHF.get(input.get(*pos as usize..*pos as usize + max_len).unwrap_or(b"")) {
    cur = state_id;
    /* fall through to that state's arm next iteration */
} else {
    /* fallback: linear branch attempt */
    /* ... existing AltLinear body ... */
}
```

W2 inlining applies.

**Hard gate**: `KEYWORD_PHF` non-empty for at least JSON, BBNF, CSS L4,
Sheets; samply confirms PHF lookup symbol present on hot path; samply on
canada confirms `__value` AltLinear self-time drops.

#### W3.3 — ClassifyByte un-gating + walker arm activation

Owner: `crates/ir/src/passes/recognizers/disjoint_first.rs` (gate
removal); `crates/core/src/backend/rust/emitter/dta_walker/lower_state.rs`
(ClassifyByte arm).

Today `disjoint_first` is gated on missing dispatch — `compute_dispatch`
admits every disjoint-FIRST candidate first as `ByteDispatch`, so the
new mining sees nothing left. **Reverse the gate**: `disjoint_first`
runs FIRST; if it produces a `ClassifyByte` table (the precomputed
`[DtaStateId; 256]` LUT), that REPLACES `ByteDispatch` for that rule.
`ByteDispatch` becomes a less-optimised fallback for cases where the
precomputed LUT can't be emitted.

Walker arm becomes one indexed load + branch:
```rust
let next = CLASSIFY_TABLE_<idx>[input[*pos as usize] as usize];
if next == DtaStateId::NONE {
    /* fallback handling */
} else {
    cur = next.0;
}
```

The `CLASSIFY_TABLE_<idx>` const is emitted per ClassifyByte state;
W1.1 hoists the table reference at codegen time.

**Hard gate**: at least one ClassifyByte table emitted per primary
grammar where the IR predicate fires; samply on bootstrap confirms
`__compoundSelector` self-time drops to ≤ 15%.

#### W3.4 — Pratt LUT consumer

Owner: `crates/core/src/backend/rust/emitter/dta_walker/lower_state.rs`
(ShuntingYard arm).

`PRECEDENCE_LUT[256]` is emitted at AW-III W6.5; the walker's
ShuntingYard arm still calls `lookup_precedence` which does a linear
scan over `PRECEDENCE_ENTRIES`. Replace with:
```rust
let entry_byte = PRECEDENCE_LUT[input[*pos as usize] as usize];
let prec = (entry_byte >> 4) & 0xF;
let assoc = (entry_byte >> 3) & 0x1;
let arity = (entry_byte >> 1) & 0x3;
let two_byte = entry_byte & 0x1;
```
Two-byte operators consult `PRECEDENCE_ENTRIES` only when `two_byte ==
1`; one-byte operators get a single LUT load.

**Hard gate**: `lookup_precedence` symbol absent from `nm`; samply on
sheets parse_stress confirms no `lookup_precedence` self-time.

#### W3.5 — Direct-to-struct view-layer consumer wiring + CTNS lifter + bounded Regex

Owner: `crates/core/src/backend/rust/emitter/grammar.rs::emit_view_impl`;
`crates/ir/src/passes/recognizers/consume_to_next_structural.rs` (gate
removal); `crates/bbnf-tape/src/kind.rs` (new `TapeKind::Scanned`);
`crates/ir/src/passes/recognizers/pattern_alphabet.rs` (invariant fix).

Three sub-items, all consumer-wiring-shaped:

**a) Direct-to-struct `emit_view_impl` consumer wiring.** `resolve_named_type`
+ binding table land at AW-III W6.4 with passing parity tests but
`emit_view_impl` doesn't call the resolver in the per-grammar hot-path
projection. Wire it: `emit_view_impl` calls `resolve_named_type(top_level_type)`
and emits the per-grammar inline projection. Universal mechanism — JSON
Value, BBNF AST, Sheets formula, CSS Color all enter the fast path.

**b) CTNS lifter enablement + tape-side Span-emitting record path.** Add
`TapeKind::Scanned` (Span record carrying the scanned bytes' end
offset). `consume_to_next_structural` lifter at
`crates/ir/src/passes/recognizers/consume_to_next_structural.rs:cf691347`
un-gates; emits `DtaState::ConsumeToNextStructural`. Walker arm jumps
`cursor.pos = idx.positions[cursor.slot]` in O(1).

**c) Bounded Regex via `pattern.last_byte_set ⊆ structural_alphabet`.**
The dense-alphabet pathology defeats the current `pattern_alphabet ⊆
structural_alphabet` invariant because patterns naturally match many
bytes. The right invariant: a pattern's *last possible byte set* (computed
from the NFA's accept states' incoming transitions) must be disjoint
from the structural alphabet — meaning the pattern always terminates
before a structural byte. Re-mine; CSS L4 + Sheets pass. Walker's Regex
arm becomes a bounded scan: `(state.match_fn)(input, pos, idx.positions[slot])?`
with the upper bound from the structural index.

**Hard gate**: at least one CTNS state lifted per grammar; bounded
Regex active on CSS L4 declarations + Sheets formulas; direct-to-struct
emitted projection visible per grammar via `cargo expand`.

### W4 — Granular SIMD widening + scanner cluster + bloom + document-parallel

Four parallel agents. With W1+W2+W3 complete, granular work amortises.

#### W4.1 — AVX2 u8x32 widening (AN.5 chronic) + WASM simd128 polish

Owner: `crates/core/src/generate/regex/emit/simd.rs`;
`crates/bbnf-simd-scan/src/{avx2,wasm}.rs`.

Every SIMD call site today uses `u8x16`. On x86_64 AVX2, widen to
`u8x32` for scanner structural-byte passes. Arch-gate via `#[cfg(target_feature
= "avx2")]`; NEON path unchanged. WASM simd128 polish for browser-side
parsing parity.

**Hard gate**: `cargo expand` shows AVX2 intrinsic on x86_64. Samply on
canada.json (x86_64 AVX2) shows ≥ 15% reduction in structural-scan
self-time vs the AW-IV W1 u8x16 baseline.

#### W4.2 — Scanner PaddedView migration + cluster consolidation + NEON 17-digit

Owner: `crates/core/src/backend/rust/emitter/string_decode.rs`;
`parse-that/rust/parse_that/src/{scanners,regex}/`;
`crates/ir/src/regex_info.rs`.

CO-E2 PaddedView migration (7 emitter call sites pass `&state.src_bytes`
unpadded — migrate to `PaddedView`). Scanner cluster consolidation
(AR.6.x / AS.5.x; ~600 LOC delete + ~350 LOC net reduction per AR
audit). NEON 17-digit fractional scan (AT.4.3; hand-written NEON kernel
±1 ULP vs scalar `f64::from_str`).

**Hard gate**: `parse-that/rust/parse_that/src/scanners/` LOC drops ≥
600; HIR predicate module count drops to 1; `parse-that` f64-parse
tests pass bit-identically on fractional inputs up to 17 digits.

#### W4.3 — Bloom + GADT runtime dedup + grammar-level pattern hoisting

Owner: `crates/bbnf-tape/src/dedup.rs` (new);
`crates/ir/src/passes/recognizers/dedup_eligibility.rs` (new);
`crates/ir/src/passes/transform/pattern_dedup.rs` (new).

Runtime bloom + GADT layered over the AW-III specialised walker.
Mandatory where `GRAMMAR_PROFILE.dedup_eligible_rules` non-empty (CSS
`compoundSelector`, `identifier`, `namedColor`-wrap, fixed unit suffixes;
JSON `null`, `true`-branch, `emptyObject`, `emptyArray`; BBNF
literal-only Alt branches). 64-bit rolling FNV over raw column bytes;
bloom admission gate; on hit, GADT lookup → `columns_range_eq` confirms
→ `push_compound_referring(rule_id, existing, span)`.

AP.4.2 grammar-level pattern dedup (compile-time sibling): identify
recurring sub-patterns (`ws + ':' + ws` × 43 in CSS L4; `!important` ×
42 across grammars); hoist into synthetic non-terminals. Pre-egraph
pass.

**Hard gate**: canada bloom-AND overhead < 2%; bootstrap record count
drops ≥ 30% vs AW-IV W3 baseline; ≥ 5 hoisted non-terminals on CSS L4
with DTA state-count reduction ≥ 100.

#### W4.4 — Document-parallel fork over stage-1 structural index

Owner: `crates/ir/src/passes/recognizers/list_rules.rs` (new);
`crates/bbnf-tape/src/driver.rs` (fork orchestration).

A rule is a fork candidate iff body is `Repeat` over an `Alt` or single
compound rule + children carry no cross-item state + each item's byte
extent is bounded by a stage-1 structural-bitmap position. Targets: CSS
`stylesheet`, JSON root array/object, BBNF `grammar`, Sheets
`file`.

The stage-1 structural bitmap (W1.2 corrected; W1.3 wired) marks every
item boundary; workers take contiguous regions; each writes into a local
`Columns`; join phase memcpy-concatenates + rewrites cross-worker
references.

**Hard gate**: tailwind.css 4c sub-linear-to-linear scaling;
`GRAMMAR_PROFILE.list_rules` non-empty for CSS L4.

### W5 — `reduce_column<C, R>` + parity harnesses + cost-model grid

Three parallel agents.

#### W5.1 — `Tape::reduce_column<C, R>` + per-column codegen + 4-lane SIMD pack

Owner: `crates/bbnf-tape/src/columns.rs`;
`crates/core/src/backend/rust/emitter/visitor.rs`;
`crates/core/tests/visitor_reduce.rs` (new).

AV.2.5 substrate ships; the consumer API + SIMD promotion never did.
Emitter extends `visitor.rs::emit_visitor_kernels` to produce one
`reduce_column<C, R>` impl per active payload column per grammar.
Promote the inner loop to packed `std::simd::f64x4` (or
arch-intrinsic `vfaddq_f64` pairs on NEON, `_mm256_add_pd` on AVX2).

**Hard gate**: ≥ 6× speedup over AV.2.5-baseline scalar left-fold on
canada.json f64 column, OR per-arch rationale documenting AArch64
ceiling.

#### W5.2 — sonic-rs + lightningcss parity harnesses + CI gate

Owner: `crates/core/tests/{sonic_rs_parity,lightningcss_parity}.rs`
(new).

sonic-rs: per JSON file in `data/json/`, parse with bbnf + sonic-rs,
compare `view().as_value()` vs `sonic_rs::Value` node-for-node.

lightningcss: per-declaration equivalence over bootstrap + tailwind +
normalize. Colors via `Color` projection (field-for-field with
`lightningcss::values::color::Color::RGBA`).

Both CI-gate alongside `grammar_roundtrip` + `tape_parity`.

**Hard gate**: zero divergences on canada / twitter / citm / data /
data_xl (sonic-rs) and bootstrap / tailwind / normalize (lightningcss);
CI step wired in `.github/workflows/ci.yml`.

#### W5.3 — Cost-model grid sweep (AM.6 chronic)

Owner: `crates/egraph/src/cost.rs`;
`scripts/cost-grid-sweep.sh` (new);
`docs/benchmarks/cost-weights-sweep.json` (new).

Six-tranche chronic. Grid-sweep harness over `CostWeights`; pick Pareto
frontier per grammar; commit calibrated weights as `pub const`.

**Hard gate**: ≥ 5% reduction in DTA state count OR extraction-pass
wall-clock vs AW-III baseline on the 4-grammar corpus, OR null-result
close with measurement evidence.

### W6 — FINAL + 19-entry bench matrix + close

Orchestrator serial. Verification ledger composed from per-wave entries.

1. Full workspace test: 0 failed; ignored count = AX-routed-residual only.
2. Full 19-entry bench matrix; **every parse entry exceeds post-AU**.
3. `docs/benchmarks/post-AW-IV.json` — bench-checkpoint + multi-wave
   aggregator (`post-AW-III.json` + `post-AW-IV-W{1..5}.json` folded in).
4. `docs/tranches/AW/FINAL-IV.md` — close document with per-wave
   verification ledger citations.
5. Update `docs/tranches/AW/FINAL.md` (composite AW close).

**Hard gate**: every parse entry exceeds post-AU; both parity harnesses
CI-gated; `FINAL-IV.md` enumerates every hard gate with artefact
citation; verification ledger complete (symbol-presence/absence per
wave + samply attribution per wave + wire-contract test passing per
slot).

## Per-grammar projection at AW-IV close

| Entry | post-AU | post-AW-III | post-AW-IV projected | vs post-AU |
|---|---:|---:|---:|:---:|
| json twitter | 1967 | 170 | 2200–4200 | **1.1–2.1×** |
| json citm | 2438 | 213 | 4000–5500 (with W4.4 fork) | **1.6–2.3×** |
| json canada | 1231 | 98 | 3500–5000 | **2.8–4.0×** |
| json data_xl | 1179 | 137 | 3500–4500 (fork) | **3.0–3.8×** |
| json data_s | 1746 | 164 | 2200–2800 | **1.3–1.6×** |
| css normalize | 735 | 14 | 1500–2200 | **2.0–3.0×** |
| css bootstrap | 454 | 8 | 1800–2500 | **4.0–5.5×** |
| css tailwind | 496 | 9 | 2000–4000 (fork) | **4.0–8.0×** |
| sheets parse_simple | 95 | 4 | 60–95 | small-input tradeoff |
| sheets parse_nested | 128 | 4 | 80–130 | small-input tradeoff |
| sheets parse_stress | 121 | 3 | 100–140 | parity–1.2× |
| bbnf json | 283 | 9 | 350–500 | **1.2–1.8×** |
| bbnf ebnf | 223 | 6 | 280–400 | **1.3–1.8×** |
| bbnf css_pretty | 647 | 20 | 750–1000 | **1.2–1.5×** |
| bbnf google_sheets | 858 | 29 | 1000–1300 | **1.2–1.5×** |
| bbnf bbnf_self | 394 | 12 | 600–900 | **1.5–2.3×** |
| bbnf css_l4_grammar | 496 | 19 | 600–800 | **1.2–1.6×** |

15+/17 parse entries exceed post-AU; the small-input sheets entries (≤
1.8 KB) ride the documented amortisation tradeoff but should still
recover to parity or near-parity once the cross-crate inlining (W2)
removes the per-byte function-call tax that small inputs amplify.

## Critical files

| File | Wave |
|------|------|
| `crates/core/src/backend/rust/emitter/dta_walker/lower_state.rs` (W1.1 hoist + W3.1/W3.2/W3.3/W3.4 consumer wiring) | W1.1, W3.* |
| `crates/ir/src/passes/sets/structural_alphabet.rs` (mining definition correction) | W1.2 |
| `crates/ir/src/passes/profile.rs` + `crates/core/src/backend/rust/emitter/profile.rs` (wire-contract pipeline) | W1.3 |
| `crates/core/tests/grammar_profile_wire_contract.rs` (new — end-to-end test) | W1.3 |
| `crates/core/src/backend/rust/emitter/dfa_codegen.rs` (new) | W1.4 |
| `crates/bbnf-tape/src/dta.rs` (DtaState::Regex match_fn field) | W1.4 |
| `crates/bbnf-tape/src/driver.rs` (RegexScanner trait deletion + #[inline(always)] hot helpers) | W1.4, W2.1 |
| `crates/core/src/backend/rust/emitter/grammar.rs` (DtaDfaScanner deletion + emit_view_impl direct-to-struct wiring) | W1.4, W3.5a |
| `Cargo.toml` (workspace LTO config if Approach A) | W2.1 |
| `crates/ir/src/passes/recognizers/shape_dict.rs` (mining recalibration) | W3.1 |
| `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (PHF threshold) | W3.2 |
| `crates/ir/src/passes/recognizers/disjoint_first.rs` (gate reversal) | W3.3 |
| `crates/ir/src/passes/recognizers/consume_to_next_structural.rs` (gate removal) | W3.5b |
| `crates/bbnf-tape/src/kind.rs` (TapeKind::Scanned variant) | W3.5b |
| `crates/ir/src/passes/recognizers/pattern_alphabet.rs` (invariant fix) | W3.5c |
| `crates/core/src/generate/regex/emit/simd.rs` + `crates/bbnf-simd-scan/src/{avx2,wasm}.rs` | W4.1 |
| `parse-that/rust/parse_that/src/{scanners,regex}/` (cluster consolidation) | W4.2 |
| `crates/bbnf-tape/src/dedup.rs` + `crates/ir/src/passes/{recognizers/dedup_eligibility,transform/pattern_dedup}.rs` (new) | W4.3 |
| `crates/ir/src/passes/recognizers/list_rules.rs` (new) + driver fork orchestration | W4.4 |
| `crates/bbnf-tape/src/columns.rs::reduce_column` + `crates/core/src/backend/rust/emitter/visitor.rs` | W5.1 |
| `crates/core/tests/{sonic_rs_parity,lightningcss_parity}.rs` (new) | W5.2 |
| `crates/egraph/src/cost.rs` + `scripts/cost-grid-sweep.sh` + `docs/benchmarks/cost-weights-sweep.json` | W5.3 |
| `docs/tranches/AW/FINAL-IV.md` + `docs/benchmarks/post-AW-IV.json` (multi-wave aggregator) | W6 |
| `docs/benchmarks/post-AW-IV-W{1..5}.json` (per-wave sidecars) | W1..W5 |

## Cross-tranche debt — addressed

| Item | Origin | AW-IV wave | Rationale |
|------|--------|-----------|-----------|
| Walker per-arm runtime data unpacking | AW-III W4.d emitter shape | W1.1 | Hoist literal bindings; LLVM per-arm specialisation depends on it |
| Structural alphabet over-mining (CSS `[0..127]`) | AW-III W5.a mining definition | W1.2 | Correct definition: bytes that delineate parse-tree structure |
| `GRAMMAR_PROFILE` wire-contract drop | AW-III W5.a/d projection | W1.3 | End-to-end test asserts mined data reaches const literal |
| Regex DFA runtime interpreter | parse-that `Dfa::find_at` | W1.4 | Per-pattern straight-line specialised match function |
| Scanner trait + HashMap + leaked Box | AW-III W1.8 half-measure | W1.4 | Direct fn-pointer in `DtaState::Regex`; trait deletes |
| Cross-crate helper calls per byte | AW-III W4.d cross-crate emission | W2.1 | LTO + `#[inline(always)]` OR per-grammar inline emission |
| ShapeRef substrate-only | AW-III W6.1 | W3.1 | Mining recalibration + walker compound-emit consumer |
| PHF substrate-only | AW-III W6.2 | W3.2 | Threshold lowering + AltLinear consumer |
| ClassifyByte substrate-only (gate inversion) | AW-III W6.3 | W3.3 | `disjoint_first` runs first; replaces `ByteDispatch` |
| Pratt LUT consumer linear-scan fallback | AW-III W6.5 | W3.4 | Single LUT byte-load replaces `lookup_precedence` |
| Direct-to-struct view-layer un-wired | AW-III W6.4 | W3.5a | `emit_view_impl` calls `resolve_named_type` |
| CTNS lifter gated off | AW-III W5.c / W6.A `cf691347` | W3.5b | New `TapeKind::Scanned` record path |
| Bounded Regex defeated by dense alphabets | AW-III W5.d / W6.A | W3.5c | Right invariant: `pattern.last_byte_set ⊆ structural_alphabet` |
| AVX2 u8x32 widening (AN.5 chronic) | AN | W4.1 | Granular layered over AW-III SIMD substrate |
| Scanner cluster consolidation (AR.6.x) | AR | W4.2 | Granular |
| NEON 17-digit (AT.4.3) | AT | W4.2 | Granular |
| skip_ws bitmap caching (AQ.8.1) + trim elision (AP.3.2) | AQ / AP | W4.2 (subsumed) | Stage-1 bitmap absorbs most; residual covered |
| Bloom + GADT + grammar-level pattern hoisting (AP.4.2) | AP | W4.3 | Layered over W3 consumers |
| Document-parallel fork | substrate landed in AW-III.W5 | W4.4 | Stage-1 index is the fork-point substrate |
| `reduce_column<C, R>` + 4-lane SIMD pack | AV.2.5 | W5.1 | Consumer for AV substrate |
| sonic-rs + lightningcss parity harnesses | competitor parity | W5.2 | CI gate |
| Cost-model grid sweep (AM.6 chronic) | AM | W5.3 | Six-tranche chronic close |

## AX seeds (carried forward — not in AW-IV scope)

- **AltLinear backtracking cost model** — speculative-execution
  substrate or savepoint-compression if AW-IV W2/W3 profiling shows
  backtracking-dominant grammars.
- **Global CSP solve** — single-solver path behind feature flag.
- **AP.5.4 deferred UTF-8 validation** — skip per-byte UTF-8 check in
  scanner hot loop when grammar's structural alphabet ⊆ ASCII7; defer
  validation to view-time accessor.
- **AQ.8.3 TLS-recycled scratch** — per-thread scratch arena for
  transient allocations.
- **FDMP mimalloc segment-class rounding** — column capacity rounding
  to mimalloc's segment size class.
- **Per-grammar column overlays** — remap unused columns per grammar.
- **AV.3.6 CSS L4 DTA state-count narrowing** — conditional on post-
  AW-IV I-cache pressure.

These are documented forward-references, not scope deferrals from
AW-IV.

## Operational posture

Inherits `docs/instructions/README.md` + `docs/instructions/TRANCHE_SPEC.md`
in full. The four newly-codified general invariants
(*Substrate-with-consumer is one unit of work*, *Wave verification
ledger*, *Cross-crate inlining is verified with `nm`*, *Wire-contract
pipelines have end-to-end tests*, *Hoist emitter-known data into emitted
code*) bind every wave; the orchestrator's wave-close ledger entries in
`PROGRESS.md` carry the verification artefact citations per wave.

Per-wave cadence:

- **Bench checkpoint per wave.** `docs/benchmarks/post-AW-IV-W{N}.json`.
- **Samply attribution per wave.** `.profiles/samply/aw4-w{N}/<bench>/`.
- **`nm` ledger per wave.** Symbol-presence/absence assertions per the
  wave's hard gate, recorded in `PROGRESS.md`.
- **Wire-contract test additions per wave.** When a wave touches the
  IR-mining → const-literal pipeline, a wire-contract test lands in
  the same wave.
- **Bootstrap regen per wave.** Idempotent at every wave boundary.

## Successor chain

AW-IV closes green → AX opens (replay tooling, snapshot persistence,
incremental re-parse, structural-default recovery, subsystem closures).
AX's substrate (`DTA_TABLE` const, `DtaSnapshot`, decision log,
per-record snapshot metadata, `StructuralIndex`) preserved verbatim
under AW-III + AW-IV; stage-1 bitmap is deterministic, replay
re-derives.

Indefatigable. No deferrals. No stubs. No shims. No new `#[ignore]`. No
grammar-specific code paths. Every parse entry exceeds post-AU at AW-IV
close. The interpreter is abrogated in this tranche; the throughput the
AW-III transposition predicated lands here.
