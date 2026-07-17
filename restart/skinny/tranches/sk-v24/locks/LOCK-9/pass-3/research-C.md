# LOCK-9 Family 9C — SCANNER-AS-DFA (pass-3 research)

The scanner layer dissolves into the regex continuum: quote, number, whitespace and
structural scanning become compiled DFA / prefilter / classifier products of the bootstrapped
grammar, and the hand-authored nibble-LUTs cease to exist as *source*. Pass-3 sharpens three
things the family owed: (F3.1) the pipeline's existence, determinism gate, and the now-settled
crate-source-of-truth with its `bbnf-regex 0.1.0` name collision resolved concretely; (F3.2) a
grammar-sourced census that reports the CSS `url()` leaf and a wide-unicode leaf against the
literal 255-class / 4096-state ceilings as denominators; (F3.3) the `[u8;256] → SIMD classifier`
emit pass with a *demonstrable* misclassification when a naive nibble/lo6 form is emitted for the
structural set.

## 1. Mechanism — tiered lowering keyed on compiled-DFA shape (restated by pointer)

"Scanner-as-DFA" is not "one transition per byte." The scanners are DFAs of three shapes, each
with a distinct optimal execution:

- **Tier M (membership, 1–2 states, single accepting char-class).** The dominant every-byte pass;
  no loop-carried state — the whole computation is the byte→class map. Lowers to the
  classify-and-movemask kernel, and to *exactly* it, because the hand LUTs already ARE
  hand-computed `byte_classes` maps (parity by construction).
- **Tier S (small multistate, 3–6 states, real transitions).** `(?:\s|/\*.*?\*/)*`, number-scan.
  Genuine loop-carried state → the serial `vqtbl1q_u8` Sheng chain, latency-bound (~2–4 cyc/byte,
  no cross-byte parallelism). For the SHORT runs these DFAs see, scalar `find_at` wins — Sheng is
  a **measured negative** on this hot path (the directive weights membership + structural tiers
  for exactly this reason).
- **Tier X (explosion / transduction, NO_GO).** Unicode `\p{…}` blows the byte-DFA past the state
  or class ceiling; JSON `\uXXXX` decode is a transducer, not an acceptor. These stay on the
  general engine (scan) + typed host functions (emit), per the hybrid-grammar-host law.

## 2. F3.1 — pipeline existence, the byte-fixpoint gate, and the crate collision (RESOLVED)

**The grammar→DFA pipeline at scanner fidelity is BUILT and green — in exactly one home.** The
full `pattern → HIR → Thompson-NFA → subset/powerset DFA → Hopcroft minimize → byte_classes →
prefilter` chain lives in `parse-that/rust/regex` (crate `bbnf-regex 0.1.0`, 6742 LOC). Concrete
authoritative seams:

- `parse-that/rust/regex/src/automata/dfa.rs:62` `Dfa::compile` (pattern→minimized DFA);
  `:77` `from_nfa`; `:116` `find_at` (the scalar Sheng, per-byte body `:124-136`:
  `class = byte_classes[b]; next = flat[state*num_cls + class]`); flat table `dfa.rs:50`,
  `byte_classes:[u8;256]` `:52`, `num_classes:u16` `:54`.
- `parse-that/rust/regex/src/sets/equiv.rs:17` `compute_byte_classes → ([u8;256], u16)`, "typically
  5–20 for common patterns" (`:5`) — the byte→class partition a nibble-LUT compresses. Emits a
  SCALAR table only; the SIMD-classifier derivation is the missing pass (§4).
- The prefilter product (`automata/prefilter.rs`, `Prefilter::from_dfa` reads the start-state row →
  entry-byte set → memchr1/2/3; `Dfa::find`): measured 9.6×–29× on prefilterable leaves, 0.99× on
  non-prefilterable controls, compile axis 1.003×, 44/44 tests. Its NEON hot loop disassembles to
  the certified `shrn.8b …, #4` vshrn-narrow movemask idiom — the emit target the family must match.

**The `bbnf-regex 0.1.0` name+version collision is real and its resolution is now concrete.**
THREE crates are in play, VERIFIED:
- `parse-that/rust/regex` → package name **`bbnf-regex` 0.1.0** — the full automata engine ABOVE.
  Authoritative DFA source-of-truth (one-regex-home; grammar tier keeps `Regex(StringId)` opaque,
  engine tier keeps rich `Hir` joined by a string/format bridge, so no HIR-type unification is even
  required to consume it).
- `bbnf-lang/skinny/crates/bbnf-regex` → package name **`bbnf-regex` 0.1.0** (single 322-LOC
  `lib.rs`, NO `automata/`/`equiv`/`classify`) — the legacy campaign-side stub, consumed by
  `skinny/crates/{ir,passes}` (`bbnf-regex.workspace = true`). **This is the collision partner and
  it archives at GN-0.**
- `bbnf-lang/skinny/crates/parse-that-regex` → package name **`parse-that-regex` 0.1.0** — the
  RUNTIME-SCAN vendor (only `number/` eisel_lemire, `unicode/` utf8_hoehrmann/utf8_block,
  `integration/simd_scan_hook.rs`; depends on `bbnf-simd`), consumed by `skinny/crates/{runtime,
  bbnf-bench}`. **It does NOT vendor the `automata/` DFA tier.**

The collision is resolved by **rename-on-vendor, not version bump**: the parse-that engine, when
vendored into the greenfield, carries the package name `parse-that-regex` (the pattern already
established for the runtime-hook crate), so no two `bbnf-regex 0.1.0` packages ever coexist in one
dependency graph; the legacy `skinny/crates/bbnf-regex` is deleted at GN-0. **The concrete
substrate GAP the family must name: the `automata/` DFA tier is NOT yet inside the skinny/
greenfield tree at all — `parse-that-regex` today is runtime scan hooks + number/utf8 decode
only.** Scanner-as-DFA has no in-tree DFA source until the `automata/` + `sets/equiv.rs` +
`classify.rs` tier is vendored under that rename (or the generator emits `dfa.find` against the
parse-that home, per the SPEC's "generator inherits by emitting dfa.find"). Absent that vendoring
the family is a design, not a scanner.

**The byte-fixpoint gate (regen∘regen = regen, byte-identical) is SATISFIABLE by construction,
with one checkable invariant.** The pipeline is deterministic at every stage that feeds output
ordering: `compute_byte_classes` scans `byte_sets`/`signatures` in index order and dedups against a
*Vec* `sig_to_class` in insertion order (`equiv.rs:48-60`) — class IDs are order-stable;
`subset_construction` assigns DFA IDs by `dfa_states.len()` in **Vec-worklist LIFO** order
(`dfa.rs:230,242,265`) — the `HashMap<BTreeSet,u32> set_to_id` is used only for *lookup*, never
iterated to produce output, so its nondeterministic iteration order cannot leak into the emitted
table; Hopcroft iterates `0..num_blocks` and filters deterministically (`dfa.rs:318-366`). The
fixpoint gate therefore holds provided one lint is enforced: **no `HashMap`/`HashSet` iteration
order may reach an emitted artifact** (`set_to_id` must stay lookup-only). This is the exact gate
LOCK-5/LOCK-7 must certify at scanner fidelity; it is a debug_assert of `regen(regen(g)) ==
regen(g)` over the generated classifier bytes, not a structural hash.

## 3. F3.2 — grammar-sourced census with over-ceiling denominators

**Ceilings (literal, from the engine type surface):** class-id ceiling = **255** — `byte_classes`
stores IDs as `u8` and `next_class:u8` uses `saturating_add` (`equiv.rs:49,57`), so a partition
needing >255 classes silently saturates and CORRUPTS; state ceiling = **4096** — `DfaOptions.
state_limit = 4096` and `subset_construction` returns `None` past it (`dfa.rs:36,262`). Both are the
directive's named denominators.

**Row 1 — the rich-CSS `url()` unquoted body.** Grammar source
`bbnf-lang/grammar/css/l4/{func-body.bbnf:55, values.bbnf:69}`:
`urlFunction = "url" , "(" >> (string | /[^)"'\s]+/) << ")"` (the `values.bbnf` variant wraps it in
`@{…}` span-capture for source maps). The scanner leaf is the negated char-class `/[^)"'\s]+/` —
reject set = `)`(0x29) `"`(0x22) `'`(0x27) + whitespace `space`(0x20) `\t`(0x09) `\n`(0x0A)
`\v`(0x0B) `\f`(0x0C) `\r`(0x0D), i.e. 9 reject bytes. Compiled: **2 states** (start → accepting
self-loop), byte_classes partition = **2 classes** {reject, accept}. Ceiling usage: states 2/4096
(0.05%), classes 2/255 (0.8%) — deep in Tier M. Separability: the reject set is two hi-nibble
columns (0x0_ = the five C0 whitespace bytes; 0x2_ = `space " ' )`), which a shufti lo×hi two-LUT
membership test resolves cleanly — so `url()` lowers to the simdjson two-LUT classifier, and is
prefilter-eligible via its 9-byte terminator set folded to a stop-mask. **Tractable, membership.**

**Row 2 — the wide-unicode leaf.** Grammar source `parse-that/rust/bootstrap/regex.bbnf:32` (the
engine's own bootstrapped grammar): `escape = "\\" >> /[pP]/ >> "{" >> /[A-Za-z_]+/ << "}"` — the
`\p{Name}` / `\P{Name}` general-category escape (companion `\u{…}` wide-codepoint at `:33`,
`\uXXXX` at `:34`). Two sub-cases, both Tier X:
  - `\p{L}` interpreted as a Unicode general category expands to a UTF-8 byte-DFA over ~130k
    codepoints in disjoint ranges; the multibyte automaton needs **hundreds–thousands of states**
    and a byte_classes partition of **>255 range-boundary classes** → hits BOTH ceilings →
    `dfa.rs:262` returns `None` (and would have saturated `equiv.rs` first). Denominators: states →
    over 4096; classes → over 255. **NO_GO, correctness bail.**
  - `\u{XXXX}` a single wide codepoint → 1–4 UTF-8 bytes → a *bounded* ~4–10-state chain, classes
    ≤ ~8; technically in-ceiling, but it is a fixed literal decode, not an every-byte scanner pass —
    it belongs to the transducer/host tier, not the classifier surface.

Note the grammar-vs-engine subtlety the census must state: in the bootstrap grammar `\p{L}` is
lexed as literal tokens `\ p { L }`; the *explosion* only materializes if the engine's HIR
interprets `\p` as a category class. The parse-that HIR parser (`hir/parser/{class,escape}.rs`)
either interprets it (→ Tier-X explosion, bail) or rejects it (→ `classify.rs` `Unknown`, stays on
the general engine). Either branch keeps `\p{…}` OFF the scanner surface — the census EXCLUDES it,
it is never lowered.

**Coverage.** Over the scanner-relevant `RegexClass` leaves
(`parse-that/rust/regex/src/classify.rs` variants `Numeric{}` `:58`, `QuotedString{}` `:73`,
`HexDigits` `:82`, `Identifier{}` `:87`, `WhitespaceWithBlockComment` `:98`, `CharClassQuantified`
`:104`, `PrefixThenClass{}` `:109`, `AccelDriven(u8)` `:117`, `Unknown` `:120`) plus the CSS
`url()` body and the structural set:

| Leaf (grammar source) | states | classes | ceiling usage | tier |
|---|---|---|---|---|
| structural `' " / ( ) [ ] { } ; :` | 1 (kind-partition) | ≤12 | 12/255, 1/4096 | **M (vceqq fan)** |
| `url()` body `/[^)"'\s]+/` (css l4) | 2 | 2 | in-ceiling | **M (shufti)** |
| WS\|`/` `(?:\s\|/)`, quote+backslash | 1–2 | 2–3 | in-ceiling | **M (nibble)** |
| `HexDigits`, `Identifier`, `CharClassQuantified`, `PrefixThenClass`, `AccelDriven` | 2–4 | 3–10 | in-ceiling | **M + prefilter** |
| `WhitespaceWithBlockComment` `(?:\s\|/\*.*?\*/)*` | 3–4 | 4–6 | in-ceiling | **S (short-run scalar; Sheng loses)** |
| `Numeric` scan `0\|[1-9]\d*(\.\d+)?…` | 5–6 | 6–8 | in-ceiling | **S (scan only; f64 decode host)** |
| JSON `\uXXXX` decode | — | — | transducer | **X (host emit)** |
| `\u{XXXX}` wide codepoint (regex.bbnf:33) | ~4–10 | ≤8 | in-ceiling but literal | **X (host decode)** |
| **`\p{…}` category (regex.bbnf:32)** | **>4096** | **>255** | **over BOTH** | **X (bail/Unknown)** |

Tractable-tier coverage = **every dominant JSON+CSS every-byte leaf** (structural, WS, quote,
number-scan, hex, ident, url-body) lands in Tier M or its prefilter — the whole productive surface.
Tier S is two short-run recognizers where scalar wins. Tier X is bounded to exactly: string decode
(transducer) + the two Unicode escapes. Only ONE census row (`\p{…}`) crosses either ceiling, and
it is excluded by design. The over-ceiling denominators bound the residual; they do not gate the
scanner tiers.

## 4. F3.3 — the `[u8;256] → SIMD classifier` emit pass, and why naive nibble misclassifies

**The emit pass is the family's core new code and it exists only in embryo.** A `ScannerPlan`
selector is ALREADY wired: `crates/core/src/generate/regex/emit/scanner_plan.rs:8` imports
`parse_that::regex::classify::{ClassRangeInfo, RegexClass}` and emits either
`ScannerPlan::Shared(SharedScanner)` (a hand `parse_that` helper) or `ScannerPlan::Kernel(TokenStream)`
(an inline backend-kernel call) — the grammar→scanner routing seam is in place, keyed on the
`RegexClass` taxonomy, and its sibling emit files `emit/dfa/{mod,accel}.rs`, `emit/simd.rs`,
`byte_class.rs`, `last_byte_set.rs`, `phf.rs`, `cost_model.rs` carry the DFA-emit and selection
machinery. What is MISSING is the last mile: `equiv.rs` produces the scalar `([u8;256], num_classes)`,
but the derivation `byte_classes → {shufti lo×hi two-LUT ∨ vqtbl4 lo6-table ∨ vceqq equality fan}`
is unwritten, and the family's landing is to collapse the `Shared` (hand-scanner) arm into the
`Kernel` arm driven by that derived class table. The emitted forms already exist as HAND kernels
to match instruction-for-instruction:
- shufti / nibble two-LUT — `bbnf-simd/src/aarch64/structural_bitmap.rs` `classify_16` ancestor;
- lo6 single-table — `bbnf-simd/src/aarch64/classify_tbl4.rs:22` `classify_chunk_from_table`
  (`vqtbl4q_u8(build_lo6_table, low6)` then `structural = (class==chunk) & (class>0)`,
  `classify_tbl4.rs:30-32`);
- vceqq equality fan — `bbnf-simd/src/aarch64/structural_classify_64.rs`.

The separability selector chooses among these on the partition; the parity oracle is the movemask
producer `bbnf-simd/src/aarch64/movemask.rs` `movemask_u8x16` (single vshrn-narrow per 16-lane,
NEVER double-`vaddv` — the standing refutation law). **Two live in-tree movemask hazards the emit
must route around, verified this pass:** (a) `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:79-87`
inlines its OWN `movemask_u8x16` using the **refuted** double-`vaddv_u8(vget_low)` + `vaddv_u8(vget_high)`
idiom (the exact 2271-vs-509 `addv.8b` counterexample) — a DFA-lowered set-fan classifier must NOT
call this inline; it must route the OR-reduced mask vectors through `movemask.rs`'s vshrn form; and
(b) `movemask.rs` itself, while vshrn-based, still carries an 8-iteration scalar pack tail
(`packed[pair*2]` loop) rather than the pure `vshrn→fmov→ands` the prefilter's certified disasm shows
— a residual the emit should target, not inherit. (c) The `[u8;256]` table classifier
`bbnf-simd/src/aarch64/byte_class_from_table_64.rs:2` is presently a **scalar delegate** (`:3` calls
the scalar impl; the NEON body is unwritten) — the direct `byte_classes`→SIMD lowering has no NEON
home yet and is the family's first authoring target.

**A naive nibble/lo6-table emit DEMONSTRABLY misclassifies the CSS structural set — the collision
is exact and airtight.** Take `;` = 0x3B and `{` = 0x7B, both structural:
- low-6-bit hash (the `vqtbl4q_u8` lo6-table form): `0x3B & 0x3F = 0x3B`; `0x7B & 0x3F = 0x3B`.
  **Both hash to slot 0x3B.** `build_lo6_table` (`classify_tbl4.rs:8-14`) writes `table[byte&0x3f]
  = byte`, last-write-wins → `table[0x3B]` holds `;` OR `{` (whichever the alphabet iterates last),
  never both. The test `structural = (vqtbl4(table,low6) == chunk)` then returns TRUE for exactly
  ONE of `{`,`;` and FALSE for the other → the dropped byte is **misclassified as non-structural**.
  A hard bug, not a rounding effect.
- low-nibble also collides (`0x3B`→`0xB`, `0x7B`→`0xB`), so a lo-nibble single-table repeats it.
- shufti two-LUT (lo×hi membership) does not drop members, but the 11-byte structural set is NOT
  cleanly shufti-separable either: `[`(0x5B)/`{`(0x7B)/`;`(0x3B) all need `lo_lut[0xB]`, while
  `' " / ( )` all need `hi_lut[0x2]` — so `+`(0x2B: lo=0xB, hi=0x2) satisfies both required buckets
  and yields a **false-positive** structural hit though `+` is not structural.

Both nibble forms fail on the full structural set — which is precisely why the hand code chose the
`vceqq_u8` equality FAN (11 independent `vceqq_u8(chunk, splat(byte))` compares OR-reduced: no
hash, no false positive). The separability solver is therefore LOAD-BEARING: it routes the
lo6-colliding structural set to the fan, while routing the genuinely nibble-separable leaves (WS|`/`,
quote+backslash, `url()` body) to the cheaper shufti/lo6 form. A prototype that emits nibble-only
for the structural set can be shown to drop `;` (or `{`) from `structural_mask` on any input
containing both — the falsifier that proves the solver is not decorative.

## 5. Prior art (citations)

- **simdjson** (Langdale & Lemire, *Parsing Gigabytes of JSON per Second*, VLDB J 28(6) 2019): the
  two-stage structural pass is membership classification, deliberately NOT a per-byte state machine
  — the empirical proof Tier M beats a DFA on the every-byte pass; the lo×hi nibble LUT is
  `classify_16`'s ancestor; its structural set (`{ } [ ] : ,`) IS nibble-separable, unlike CSS's.
- **Hyperscan / Sheng** (Wang et al., NSDI 2019): DFA-in-a-register via `pshufb`→`vqtbl1q_u8`; the
  loop-carried transition is the documented ~1–2 GB/s ceiling — the Tier-S stall, named in the
  literature; the reason Sheng is baseline-only here.
- **regex-automata** (BurntSushi): `ByteClasses` ≡ `equiv.rs`; the memchr/rare-byte prefilter ladder
  (the prefilter product's origin) + the Teddy `vqtbl` fingerprint tier above memchr3 (the named
  future tier for >3-byte entry sets).
- **Data-Parallel FSM** (Mytkowicz, Musuvathi, Schulte, ASPLOS 2014): transition-function
  composition to break the serial DFA dependency — the theoretical answer to Tier-S, refuted here
  by O(states²)/block for >~8 states (loses for the 3–6-state Tier-S DFAs).
- **rustc / regex `byte_classes`**: identical equivalence-class compression validates `equiv.rs`.

## 6. Named risks and counterexamples

- **R1 (fatal).** Emitting Sheng for a Tier-M DFA → the ~12× regression (1000-byte `[a-z]+`: Sheng
  ~3000 cyc serial `vqtbl1q_u8` vs membership ~250 cyc). *Guard:* detect single-accepting-class
  shape, route to classifier; Sheng baseline-only.
- **R2 (proven this pass).** `byte_classes` not nibble/lo6-separable → naive emit misclassifies.
  *Counter:* `;`=0x3B ≡ `{`=0x7B low6/low-nibble collision drops a structural byte; `+`=0x2B shufti
  false-positive. *Guard:* separability solver selects fan over nibble for the structural set.
- **R3.** `\p{…}` explosion breaks the u8 class-id (`saturating_add` corrupts) AND the 4096 state
  ceiling. *Guard:* `dfa.rs:262` bail → general engine; census EXCLUDES Tier X.
- **R4 (substrate gap, sharpened).** The `automata/` DFA tier is NOT vendored into skinny —
  `parse-that-regex` is runtime hooks only; `skinny/crates/bbnf-regex` is a 322-LOC stub that
  archives at GN-0. *Guard:* vendor `automata/` + `equiv` + `classify` under the `parse-that-regex`
  rename (collision-free), or emit `dfa.find` against the parse-that home; without it there is no
  in-tree DFA source.
- **R5.** Transducers mislabeled as acceptors: `\uXXXX` and number-*decode* emit values a DFA
  cannot produce. *Guard:* split scan (DFA) from emit (typed host fn); number-scan is Tier S,
  Eisel–Lemire f64 decode stays host (`parse-that-regex/src/number/eisel_lemire/`).
- **R6.** Non-determinism leaking the fixpoint gate: `set_to_id: HashMap` in `subset_construction`.
  *Guard:* lint that HashMap iteration order never reaches an emitted artifact (it is lookup-only
  today); assert `regen∘regen` byte-identity over the generated classifier.

## 7. What a prototype must test to move the family

1. **Membership parity + emit reduction-count (F3.3, falsifiable).** Compile `(?:\s|/)`, the CSS
   `url()` body `/[^)"'\s]+/`, and the 11-byte structural set through `equiv.rs`; emit the
   classifier via the separability solver; `otool -tv` the generated NEON against hand
   `classify_ws_slash_16` / `classify_chunk_from_table` / `structural_classify_64` — reduction-op
   count ≤ hand (single vshrn-narrow movemask per lane, never double-`vaddv`), and cold paired divan
   on CSS ws-scan + JSON structural at ≥0.98×, two windows, N≥200, base-pinned to a stated HEAD.
2. **Separability falsifier (F3.3, decisive).** Emit a nibble/lo6-only classifier for the structural
   set and show it drops `;` (or `{`) from `structural_mask` on input containing both, and
   false-positives `+`; then show the fan-routed emit is bit-parity with `structural_classify_64`.
   Proves the solver is load-bearing.
3. **Census + coverage % (F3.2).** For every `RegexClass` leaf sourced from the grammars, compile via
   `dfa.rs` and record `{num_states, num_classes, tier, ceiling-bail}`; include `url()` body,
   `\u{…}`, and `\p{L}` (expect the last → `None` over BOTH ceilings). Emit the Tier-M / Tier-S /
   Tier-X split with 255-class / 4096-state denominators.
4. **Byte-fixpoint gate (F3.1).** Run grammar→classifier emit twice; assert byte-identical output;
   add the HashMap-order lint. Confirms the pipeline's determinism at scanner fidelity.
5. **Vendoring seam (F3.1 / R4).** Prove the `automata/` + `equiv` + `classify` tier vendors under
   the `parse-that-regex` package name with no `bbnf-regex 0.1.0` collision, and that
   `skinny/crates/{ir,passes}` repoint off the legacy stub before its GN-0 deletion.
