# LOCK-9 Family 9C — SCANNER-AS-DFA (pass-2 research)

The scanner layer dissolves into the regex continuum: quote, number, whitespace and
structural scanning become compiled DFA / prefilter / classifier products of the
bootstrapped grammar, and the hand-authored nibble-LUTs cease to exist as source. This
report makes the mechanism concrete, sites the substrate that already exists, answers the
three pass-2 questions with disassembly-grounded evidence, and names the falsifiers a
prototype must clear.

## 1. The mechanism: a DFA is not one thing, and neither is its execution

The family's error mode is treating "scanner-as-DFA" as "run a DFA transition per byte."
The measured truth is that the eight scanners are DFAs of THREE distinct shapes, and each
shape has a different optimal execution strategy. The mechanism is therefore a **tiered
lowering** keyed on the compiled DFA's structure, not a single Sheng runner:

- **Tier M (membership, 1–2 states, single accepting char-class).** The dominant every-byte
  pass. The DFA's entire computation is the byte→class map; there is no loop-carried state.
  This lowers to the simdjson classify-and-movemask kernel — and lowers to *exactly* it,
  because the hand LUTs are already hand-computed `byte_classes` tables. Parity is by
  construction (§3, Q1).
- **Tier S (small multistate, ≤16 states, real transitions).** `(?:\s|/\*.*?\*/)*`,
  number-scan `0|[1-9]\d*(\.\d+)?…`. Genuine loop-carried state → the Sheng question. The
  aarch64 `vqtbl1q_u8` state transition is a serial chain (§3, Q3); for the SHORT runs these
  DFAs actually see, a scalar `find_at` beats every SIMD form.
- **Tier X (explosion / transduction, NO_GO).** Unicode `\p{…}`/`\u{…}` class leaves blow
  the byte-DFA past the state ceiling; JSON `\uXXXX` decode is a transducer, not an acceptor.
  These stay on the general engine (scan) + host functions (emit).

"Hand-written scanners cease to exist" is true for Tier M (the LUTs are *derived* from
`byte_classes` at codegen) and for Tier S's recognizer (an emitted DFA runner replaces the
hand walker); it is deliberately false for Tier X, per the hybrid-grammar-host law.

## 2. The substrate that already exists (integration seams)

The full grammar→DFA pipeline is built and green in parse-that's in-repo
`bbnf-regex` (`rust/regex/`, 6742 LOC), and PROVE-10 landed the prefilter product (44/44
tests green). Concrete seams:

- **DFA runner + the serial line.** `parse-that/rust/regex/src/automata/dfa.rs:116`
  `find_at`; the per-byte body at `dfa.rs:124-135` is `class = byte_classes[b]; next =
  flat[state*num_cls + class]` — this IS the scalar Sheng, already loop-carried on `state`.
  `byte_classes:[u8;256]` at `dfa.rs:52`, `num_classes:u16` at `:54`, `flat_transitions`
  (single contiguous table) at `:50`. Hopcroft minimization `dfa.rs:291`; state_limit=4096
  bail `dfa.rs:36,262` (the explosion guard).
- **The class-map producer.** `parse-that/rust/regex/src/sets/equiv.rs:17`
  `compute_byte_classes` → `([u8;256], num_classes)`, "typically 5–20 for common patterns"
  (`equiv.rs:6`). This is the byte→class partition that a nibble-LUT compresses. It emits a
  SCALAR table only — the `[u8;256] → (lo_lut,hi_lut)` SIMD derivation is the missing pass
  (§6, R6).
- **The prefilter product.** PROVE-10 `automata/prefilter.rs` `Prefilter::from_dfa` reads the
  start-state row → entry-byte set → memchr1/2/3; `Dfa::find` branches once. Measured
  9.6×–29× on prefilterable leaves, 0.99× on non-prefilterable controls, compile axis 1.003×.
  Disassembly (PROVE-10 §2c) confirms the `shrn.8b …, #4` vshrn-narrow movemask idiom in the
  memchr hot loop — the SPEC §0/03 CONFIRMED producer.
- **The SIMD classifier kernels to emit into.** `bbnf-simd/src/aarch64/classify_tbl4.rs:22`
  `classify_chunk_from_table` (`vqtbl4q_u8` lo6-table classify + `movemask_u8x16`);
  `bbnf-simd/src/aarch64/structural_classify_64.rs` (the `vceqq_u8` equality fan, chosen
  BECAUSE the 11-byte structural set `' " / ( ) [ ] { } ; :` has lo-nibble AND lo-6-bit
  collisions — `;`=0x3B ≡ `{`=0x3B — so no single `vqtbl1q_u8`/`vqtbl4q_u8` table assigns a
  distinct kind); `bbnf-simd/src/aarch64/movemask.rs` `movemask_u8x16`.
- **The hand scanners to dissolve.** `structural_bitmap.rs:37` `classify_16` (hand nibble-LUT,
  the Tier-M template), `:269` `digraph_mask` (the 2-byte "shift-and-follow" that is a
  handwritten Tier-S transition for `/*` `*/` `->`); `ws_comment.rs:72` `classify_ws_slash_16`
  + the hand LUTs `ws_comment.rs:37-62` (a hand `byte_classes` for the 6-byte WS|`/` set);
  `quoted_simd.rs` (quote+backslash membership → Tier M); `number_simd.rs` (scan half → Tier
  S/M, decode half NO_GO); `decode.rs` (transducer, NO_GO).
- **The leaf taxonomy (census rows).** `parse-that/rust/regex/src/classify.rs:55`
  `RegexClass`: `Numeric`, `QuotedString`, `HexDigits`, `Identifier`,
  `WhitespaceWithBlockComment` (its doc names it "DFA-compatible"), `CharClassQuantified`,
  `PrefixThenClass`, `AccelDriven(u8)`, `Unknown`. `classify_regex_from_hir` `classify.rs:177`.
- **The grammar source.** `parse-that/rust/bootstrap/regex.bbnf` (72 LOC). The explosion
  candidate lives in the `escape`/`class_escape` rules `regex.bbnf:12-37`: `\p{…}`
  (`:32`), `\u{…}` (`:33`), `\uXXXX` (`:34`), `\x{2}` (`:35`).

## 3. The three pass-2 questions

### Q1 — Dense-classification parity: DFA-lowered dense classify vs hand simdjson

**Answer: parity is by construction for Tier M, because the hand simdjson kernel IS the
compiled form of a single-char-class DFA.** A DFA for `[chars]+` / `[chars]*` is a 1–2 state
machine whose only content is the accepting byte-set; its `byte_classes` partition
degenerates to `{accept, reject}` (2 classes) or a small kind partition. The every-byte
computation reduces to *membership*, which is precisely what simdjson computes with the
lo×hi nibble LUT + `vceqq` + `movemask` (`classify_16`, `structural_bitmap.rs:37`). The hand
tables (`WS_SLASH_LO_LUT`/`WS_SLASH_HI_LUT`, `ws_comment.rs:40-62`) are hand-authored
`byte_classes` maps for exactly this DFA. So the lowering `byte_classes → (lo_lut,hi_lut)`
regenerates the identical kernel; the PROVE-09 refutation law (measured no-regression, vshrn
narrow, never double-`vaddv`) is satisfiable because the emitted body is instruction-for-
instruction the hand body. The non-trivial content the family must SOLVE is *which classifier
producer* the partition admits — three tiers, decided by separability of the accepting set:
(a) nibble-separable → simdjson lo×hi two-LUT (`classify_16`); (b) lo6-separable, >nibble →
`vqtbl4q_u8` lo6-table (`classify_tbl4.rs:22`); (c) lo6-colliding (the 11-byte structural
set) → `vceqq_u8` equality fan (`structural_classify_64.rs`). A naive "byte_classes → nibble
LUT" is WRONG for (c). Parity is thus MEASURED at the emit boundary: the generated NEON for a
grammar-sourced `(?:\s|/)` classify must match `classify_ws_slash_16` to ≤1.0× reduction-
instruction count under `otool -tv`.

### Q2 — Census from `bootstrap/regex.bbnf`, including the explosion candidate

Classifying the scanner-relevant leaves + the eight scanner files by DFA tier:

| Leaf / scanner (source) | DFA shape | states | tier | in-SIMD? |
|---|---|---|---|---|
| structural set `' " / ( ) [ ] { } ; :` (`structural_bitmap`) | single kind-partition | 1 | M (fan) | yes, vceqq |
| WS|`/` `(?:\s|/)` (`ws_comment` tier-2) | single char-class | 1 | M (nibble) | yes |
| quote+backslash membership (`quoted_simd`) | single char-class | 1 | M (nibble) | yes |
| `HexDigits` `[0-9a-fA-F]+` | char-class | 2 | M / prefilter | yes |
| `Identifier` `[A-Za-z_][\w-]*` | prefix+class | 2–3 | M / prefilter | yes |
| `CharClassQuantified`, `PrefixThenClass`, `AccelDriven` | class runs | 2–4 | M+prefilter | yes |
| `WhitespaceWithBlockComment` `(?:\s|/\*.*?\*/)*` | in/star/out | 3–4 | S (Sheng ∨ scalar) | short-run scalar |
| `Numeric` `0|[1-9]\d*(\.\d+)?([eE][+-]?\d+)?` scan | int/frac/exp | 5–6 | S (scan only) | scalar; decode NO_GO |
| JSON `\uXXXX` decode (`decode.rs`) | transducer | — | X | NO_GO (host emit) |
| **`\p{L}` / `\p{…}` unicode class (`regex.bbnf:32`)** | UTF-8 multibyte | 100s–1000s | **X (explosion)** | **NO_GO (state_limit bail)** |
| `\u{…}` wide-codepoint escape (`regex.bbnf:33`) | UTF-8 multibyte | 10s–100s | X | NO_GO |
| rich-CSS `url(` unquoted body `[^)\s]+` | char-class | 2 | M | yes |

**Coverage:** every dominant JSON+CSS hot byte falls in Tier M (structural, WS, quote,
number-scan, hex, ident) or its prefilter — the family's productive surface. Tier S is two
recognizers (ws+comment, number-scan) whose runs are short (`ws_comment.rs:3-8`: 84–100% of
WS runs ≤4 bytes, 63% exactly one byte). Tier X is the residual: JSON string decode
(transducer) + the two Unicode escapes. The **explosion is `\p{…}`**: a general-category
class over UTF-8 compiles to a byte-DFA whose `num_classes` can exceed 255 (breaking the
`[u8;256]`/u8 class-id type in `equiv.rs`) and whose states exceed 4096 → `dfa.rs:262`
returns `None`. This is the correctness floor, not a scanner. The measured coverage the
prototype reports is "% of scanner-relevant `RegexClass` leaves that compile under
`state_limit` with `num_classes` ≤ SIMD ceiling (≤16 for lo6, ≤ small for the fan)"; the
census predicts Tier M+prefilter absorbs the every-byte mass and Tier X is bounded to
decode + the two Unicode escapes.

### Q3 — class-map ∘ Sheng: does `state' = tbl(row[class], state)` stall per byte?

**Answer: YES, and the stall disqualifies Sheng for the dominant pass — it is the reason
Tier M exists.** Sheng (Hyperscan's DFA-in-a-register) packs ≤16 states into a NEON lane and
performs the transition with `vqtbl1q_u8(transition_row, class_vector)`. But the recurrence
is `state[i] = tbl(T, class[i], state[i-1])`: each byte's state depends on the previous
byte's, so the loop carries a serial chain of one `vqtbl1q_u8` per byte. On Apple/Cortex the
NEON table lookup has ~2–4 cycle latency and cannot be hidden — throughput is
latency-bound at ~2–4 cyc/byte with NO cross-byte parallelism (the vector width buys table
*capacity*, not *bytes-per-instruction*). Contrast Tier M: `classify_16` processes 16
bytes/instruction and reduces via `movemask_u8x16` at ~0.1–0.3 cyc/byte. The gap is an order
of magnitude:

> Counterexample. A `[a-z]+` run of 1000 bytes. Sheng: 1000 × ~3 cyc (serial `vqtbl1q_u8`
> chain) ≈ 3000 cyc. Membership-classify: ⌈1000/16⌉ × ~4 cyc ≈ 250 cyc. ~12×.

Therefore the lowering must NEVER emit Sheng for a Tier-M (single-accepting-class)
DFA — it must detect that shape and emit the classifier. For genuine Tier-S multistate the
serial chain is unavoidable in the Sheng form, but the runs are short (ws+comment, number),
so the per-call SIMD setup dominates and the scalar `find_at` (`dfa.rs:116`, already the
scalar Sheng) is the measured winner — the `ws_comment.rs` tiering already discovered this
empirically (tier-1 is a single-byte scalar short-circuit ahead of any SIMD). The only known
route to break the Q3 serial dependency is data-parallel FSM co-processing (transition-
function composition / matrix product over blocks, Mytkowicz ASPLOS'14) which costs O(states²)
per block and loses for >~8 states — not viable for the 3–6-state Tier-S DFAs. Net: Sheng is
a NEGATIVE result for this family's hot path; the DFA lowers to classify+prefilter (Tier M)
or scalar `find_at` (Tier S), and Sheng-tbl is retained only as a measured-against baseline.

## 4. Cross-lock predecessor status (LOCK-5 — report, do not assume)

The DFA source-of-truth is contested between TWO crates and LOCK-5 owns the canonicity
verdict; the scanner family has no source until it resolves:
- **parse-that in-repo `bbnf-regex`** (`rust/regex/`): the `.cargo/config.toml` patch
  `bbnf-regex -> regex` (in-tree, R3 §1) makes THIS the crate parse-that actually links. It
  carries the full parse→HIR→NFA→DFA→Hopcroft→`byte_classes`→prefilter pipeline, builds
  green, and PROVE-10 extended it. Fidelity for a Tier-M/Tier-S scanner is PRESENT today.
- **bbnf-lang `crates/bbnf-regex`**: the campaign-side engine (LOCK-5's home). Whether it is
  byte-identical to, a superset of, or a fork of the in-repo crate is a LOCK-5 reconciliation
  — potentially TWO crates to unify. The scanner family consumes whichever LOCK-5 certifies.

**Fidelity gaps LOCK-5 must close for a scanner, measured:** (a) the `num_classes > 255` /
state>4096 Unicode explosion is a correctness bail, adequate as a floor but it means the
scanner census must EXCLUDE Tier X, not lower it; (b) `equiv.rs` emits a scalar `[u8;256]`,
not a SIMD classifier — the `byte_classes → (lo_lut,hi_lut) ∨ lo6-table ∨ vceqq-fan`
separability-solved emit pass does NOT exist and is the family's core new code (homed in the
LOCK-2 cost model / LOCK-5 emit); (c) the byte-fixpoint gate (grammar→DFA determinism /
idempotent regen) must hold at the fidelity the prefilter start-row derivation needs. Absent
(b) the family is a design, not a scanner.

## 5. Prior art (citations)

- **simdjson** (Langdale & Lemire, "Parsing Gigabytes of JSON per Second", VLDB J 28(6),
  2019): the two-stage structural pass is *membership classification*, deliberately NOT a
  state machine — the empirical proof that Tier M beats a per-byte DFA on the every-byte pass.
  The lo×hi nibble LUT is `classify_16`'s ancestor.
- **Hyperscan / Sheng** (Wang et al., "Hyperscan: A Fast Multi-pattern Regex Matcher", NSDI
  2019): DFA-in-a-register via `pshufb`/`vpshufb` (→ `vqtbl1q_u8` on NEON); the loop-carried
  transition is the documented ~1–2 GB/s ceiling — Q3's stall named in the literature.
- **regex-automata** (BurntSushi): `ByteClasses` (≡ `equiv.rs`), lazy DFA, and the
  memchr/rare-byte prefilter ladder + Teddy multi-substring tier. PROVE-10 is a direct port of
  the prefilter tier; Teddy (`vqtbl` fingerprint) is the named future tier above memchr3.
- **Data-parallel FSM** (Mytkowicz, Musuvathi, Schulte, "Data-Parallel Finite-State
  Machines", ASPLOS 2014): transition-function composition to break the serial DFA dependency
  — the theoretical answer to Q3, refuted here by the O(states²) blowup for >8 states.
- **rustc/regex `byte_classes`**: identical equivalence-class compression; validates
  `equiv.rs`'s approach.

## 6. Named risks and counterexamples

- **R1 (fatal if ignored).** Emitting Sheng for a Tier-M DFA — the 12× regression of Q3.
  *Guard:* the lowering detects single-accepting-class shape and routes to the classifier;
  Sheng is baseline-only.
- **R2.** `byte_classes` not nibble-separable → a naive lo×hi LUT is incorrect. *Counter:*
  the structural set's `;`=0x3B ≡ `{`=0x3B lo6 collision (`structural_classify_64.rs`).
  *Guard:* a separability CSP over the partition selects nibble-LUT ∨ vqtbl4-lo6 ∨ vceqq-fan.
- **R3.** Unicode `\p{…}` explosion breaks the u8 class-id type and the 4096 state ceiling.
  *Guard:* `dfa.rs:262` bail → general engine; census EXCLUDES Tier X.
- **R4.** Two `bbnf-regex` crates unreconciled → no DFA source. *Guard:* LOCK-5 canonicity
  verdict is a hard predecessor; report status, never presume.
- **R5.** Transducers mislabeled as acceptors: `\uXXXX` decode and number-*decode* EMIT
  values a DFA cannot produce. *Guard:* split scan (DFA) from emit (host fn) per the
  hybrid-grammar-host law; number-scan is Tier S, fast-float decode stays host.
- **R6 (the core gap).** The `[u8;256] → SIMD classifier` emit pass does not exist;
  today it is hand-authored per scanner. This is the family's new code and its correctness
  hinges on R2's separability solver.

## 7. What a prototype must test to move the family

1. **Membership parity (Q1, falsifiable).** Compile `(?:\s|/)` and the 11-byte structural set
   to `byte_classes` via `equiv.rs`; emit the classifier through the separability solver;
   `otool -tv` the generated NEON against hand `classify_ws_slash_16` / `structural_classify_64`
   — reduction-instruction count ≤1.0× (PROVE-09 law), and a cold paired divan run on the CSS
   ws-scan + JSON structural workloads at parity (≥0.98×, two windows, N≥200).
2. **Sheng-stall measurement (Q3, the decisive datum).** Implement a `vqtbl1q_u8` Sheng runner
   for `WhitespaceWithBlockComment` (3–4 states) and benchmark it against the scalar `find_at`
   (`dfa.rs:116`) and the `digraph_mask` shift-and-follow on a comment-heavy CSS corpus; report
   the divan ratio (PMU cyc/B where macOS allows) — expected: Sheng ≥ scalar for the short
   runs, both ≫ Tier-M membership; confirms Sheng is a negative result on the hot path.
3. **Census + coverage % (Q2).** For every `RegexClass` leaf sourced from `regex.bbnf`, compile
   via `dfa.rs` and record `{num_states, num_classes, nibble/lo6/fan tier, state_limit-bail}`;
   include `\p{L}+` and `\u{…}` as explosion rows (expect `None`). Emit the measured
   Tier-M / Tier-S / Tier-X split as the coverage percentage.
4. **Separability solver (R2/R6).** Prove the emit pass routes the lo6-colliding structural set
   to the `vceqq` fan and the WS set to the nibble-LUT — a naive nibble-only emit must be shown
   to MISCLASSIFY the structural set, demonstrating the solver is load-bearing, not decorative.
