# AW-IV Profiling — P6 Begotten-Code Audit

Static analysis: four prebuilt bench binaries (from
`binaries.tsv` — directive hashes are stale) + prepared `expand.rs`
artefacts + `crates/core/src/grammar/generated.rs` (79 844 lines).

## Executive summary

Six plan-declared levers are compiled: inline DFA bodies, `match
table.states[N]` hoist, `dispatch_one` elim, `DtaState::Regex {
pattern, payload }` dismantle, `PRECEDENCE_LUT` hoist,
grammar-profile capacity. JSON gets a monolithic 22 KB walker with
zero `#[cold]` helpers. **CSS L4 does not.** Its walker `run` is
153.9 KB (over M-series 128 KB L1 i-cache) and emits **2 283 `bl`
calls to `#[cold] #[inline(never)] __cold_state_N` helpers** inside
`mod __dta_walker_inline`. "Flattened walker" holds for JSON,
partial for Sheets (42 cold calls) and BBNF (265), and
**contradicted** for CSS. `SHAPE_DICT` populated only for CSS.
`KEYWORD_PHF` / `CLASSIFY_TABLE_*` / `ACTIVE_COLUMNS` /
`BRANCH_PRIORS` / `DEDUP_ELIGIBLE_RULES` / `LIST_RULES` absent
everywhere.

## 1. Symbol-presence matrix

`nm -a <bin> | rustfilt > /tmp/p6/nmR-<bench>.txt`.

| Symbol | JSON | CSS | Sheets | BBNF |
|---|---|---|---|---|
| `dispatch_one` | – | – | – | – |
| `driver::try_branch` | – | ✓ | ✓ | ✓ |
| `__dfa_match_*` | – | – | – | – |
| `__dta_walker_inline::run` | ✓ | ✓ | ✓ | ✓ |
| unique `__cold_state_N` | **0** | **2 280** | **42** | **265** |
| `RegexScanner` / `DtaDfaScanner` / `find_at` | – | – | – | – |
| `emit_leaf` / `reserve_compound` / `push_compound_fused` / `push_leaf_fused` / `close_compound` / `trim_with_pattern` / `handle_repeat_failure` | – | – | – | – |
| `driver::advance_or_pop_with` | ✓ | ✓ | ✓ | ✓ |
| `psi::write_decoded` + `RawVec<PayloadJob>::grow_one` | ✓ | ✓ | ✓ | ✓ |
| `eisel_lemire::compute_f64` / `dec2flt::parse_number` | – | – | – | – |
| `bbnf_simd_scan::neon::scan` / `scan_quoted_string_simd` | – | – | – | – |

```
$ grep -E "__cold_state_" /tmp/p6/nmR-css.txt  | awk '$2=="t"' | sort -u | wc -l  # 2280
$ grep -E "__cold_state_" /tmp/p6/nmR-json.txt | awk '$2=="t"' | sort -u | wc -l  # 0
```

`#[cold] __regex_scan_<Grammar>` adapter exists; its callees
(`__dfa_match_*`) are gone — adapter splices DFA bodies inline.
`try_branch`/`advance_or_pop_with`/`psi::write_decoded` survive as
cross-crate BL targets (§3).

## 2. Walker symbol size

Per-bench `run` (first/last instr addr, bytes = last - first + 4) and
full walker module (run + all `__cold_state_N`):

| Bench | `run` bytes | `run` KB | > 128 KB L1i? | full walker mod bytes / KB |
|---|---|---|---|---|
| JSON | 22 008 | 21.5 | – | 22 008 / 21.5 |
| **CSS** | **157 556** | **153.9** | **YES** | **1 953 156 / 1 907.4** |
| Sheets | 56 036 | 54.7 | – | 83 872 / 81.9 |
| BBNF | 59 456 | 58.1 | – | 266 040 / 259.8 |

```
$ awk '/^__R.*dta_walker_inline.*(run|cold_state).*:$/{inw=1;next}
       /^__R.*:$/ && !/dta_walker_inline.*(run|cold_state)/{inw=0}
       inw && /^0000/{c++} END{print c*4}' /tmp/p6/otool-css.txt
1953156
```

## 3. Cross-crate call audit inside `run`

| Bench | `run` instrs | `run` BL | cold_state BL | top cross-crate BL targets |
|---|---|---|---|---|
| JSON | 5 502 | 153 | 0 | `Columns::grow_all`×29, `RawVec<u8>::grow_one`×29, `advance_or_pop_with`×23, `RawVec<Frame>::grow_one`×17, `nearest_variant_frame`×12 |
| CSS | 39 389 | 2 742 | **2 283** | `__cold_state_N`≥2 283, `Columns::grow_all`×110, `RawVec<u8>::grow_one`×110, `nearest_variant_frame`×78, `LiteralPayload::write_le`×46 |
| Sheets | 14 009 | 411 | 42 | `Columns::grow_all`×78, `RawVec<u8>::grow_one`×78, `nearest_variant_frame`×43, `RawVec<Frame>::grow_one`×35 |
| BBNF | 14 864 | 581 | 265 | `Columns::grow_all`×81, `RawVec<u8>::grow_one`×81, `RawVec<Frame>::grow_one`×41, `nearest_variant_frame`×40 |

Union of top cross-crate fn-symbols: `Columns::grow_all`,
`RawVec<u8>::grow_one`, `RawVec<Frame>::grow_one`,
`FrameStack::nearest_variant_frame`, `LiteralPayload::write_le`,
`Vec<u8>::append_elements`, `driver::{try_branch, advance_or_pop_with}`,
`drop_in_place::<FrameStack>`, `_mi_free`/`_mi_malloc_aligned`.

"Flattened + self-contained" is partial. Every hot arm calls
cross-crate helpers dozens–hundreds of times per parse. Per README
§Performance-claims, each call IS a dispatcher at the fn-call
boundary. Workspace LTO or per-grammar helper-body inline-emit would
close this; emitted text shows neither has fired.

## 4. `generated.rs` structural audit (BBNF grammar, 79 844 lines)

| Section | Lines | Count | % |
|---|---|---|---|
| Header + `GRAMMAR_PROFILE` + mining metadata | 1 – 912 | 912 | 1.1% |
| `__DTA_STATES` literal (496 entries) | 913 – 2 953 | 2 041 | 2.6% |
| `__DTA_RULE_ENTRIES` + shunting_yard rules | 2 954 – 3 176 | 223 | 0.3% |
| `DTA_TABLE` + shape dicts + precedence + cold regex adapter | 3 177 – 4 159 | 983 | 1.2% |
| `mod __dta_walker_inline { run + 265 __cold_state_N }` | 4 160 – 61 640 | **57 481** | **72.0%** |
| Post-walker residue (IR + view helpers) | 61 641 – 72 188 | 10 548 | 13.2% |
| `BbnfBootstrapNodeView` + projection | 72 189 – 79 525 | 7 337 | 9.2% |
| `pub fn parse(…)` | 79 527 – 79 844 | 318 | 0.4% |

DFA bodies: 28 inlined `'__dfa: { ... }` — 14 in `run`, 14 in the
cold `__regex_scan_BbnfBootstrap` adapter.

## 5. Hoist-literal verification (W1.1)

```
$ grep -c "match table.states\[" crates/core/src/grammar/generated.rs
2       # both are doc-comments at :45010 and :61656
$ grep -c "DtaState::Regex { pattern, payload }" \
    crates/core/src/grammar/generated.rs
0
```

Every arm body carries literal bindings. The 28
`unreachable_unchecked` are intra-DFA fall-throughs, not source-array
match elision. **W1.1 holds.**

## 6. Inline-DFA-body verification (W1.4-aggressive)

```
$ grep -c "'__dfa: {" crates/core/src/grammar/generated.rs
28
$ grep -c "__dfa_state: u32 = 0" expand/css_l4/expand.rs
626
```

Zero `__dfa_match_*` symbols nm-wide. JSON arm sample at
`generated.rs:45078` (inside `__dta_walker_inline::run`):

```rust
0 => {
    let payload = Some(PayloadKind::I64);
    let dfa_result: Option<u32> = { let pos: usize = *pos as usize; '__dfa: {
        let mut __dfa_state: u32 = 0; let mut __dfa_p: usize = pos;
        let mut __dfa_last_match: Option<u32> = None;
        loop {
            let b = match input.get(__dfa_p) { Some(&b)=>b, None=>break };
            match __dfa_state {
                0 => match b { 48..=57 => __dfa_state = 1, _ => break },
                1 => match b { /* ident chars */ _ => break },
                _ => unsafe { core::hint::unreachable_unchecked() },
            }
            __dfa_p += 1;
            match __dfa_state { 1 => __dfa_last_match=Some(__dfa_p as u32), _=>{} }
        }
        break '__dfa __dfa_last_match.map(|end| end - pos as u32);
    }};
```

CSS sample at `expand/css_l4/expand.rs:56444` (inside
`__regex_scan_CssL4Parser`, `#[cold]`, called from `__cold_state_N`):

```rust
if core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_466.as_ptr()) {
    return '__dfa: { let mut __dfa_state: u32 = 0; …
        match __dfa_state {
            0 => match b { 48..=57 | 65..=70 | 97..=102 => __dfa_state = 8, _ => break },
            …
```

**W1.4-aggressive holds for DFA splicing**; walker-arm-body inlining
story in §2/§3.

## 7. Consumer-activation audit

| Consumer | JSON | CSS | Sheets | BBNF |
|---|---|---|---|---|
| `SHAPE_DICT` | `&[]` | `&__SHAPE_DICT_TABLE` (13) | `&[]` | `&[]` |
| `BBNF_SHAPE_DICT` | n/a | n/a | n/a | 2 |
| `KEYWORD_PHF` / `CLASSIFY_TABLE_*` | – | – | – | – |
| `PRECEDENCE_LUT[256]` nonzero / `OPERATOR_COUNT` | 0 / 0 | 4 / 4 | 8 / 8 | 5 / 5 |
| `ACTIVE_COLUMNS` / `BRANCH_PRIORS` / `DEDUP_ELIGIBLE_RULES` / `LIST_RULES` | – | – | – | – |

Precedence consumer: walker's `ShuntingYard` arm does a byte-indexed
LDRB on the 256-byte static (no `lookup_precedence` fn). Five of nine
mining-to-consumer bridges are `&[]` — substrate-without-consumer.

## 8. Capacity pre-allocation

All four benches route through `GRAMMAR_PROFILE.capacity_for(N) =
((N as f32)*(cpb+lpb)) as usize + 2` (`bbnf-tape/src/profile.rs:281`).

| Bench | `compounds_per_input_byte` | capacity(N) |
|---|---|---|
| JSON | 0.5 | **N/2 + 2** |
| CSS | 1.0 | N + 2 |
| Sheets | 1.0 | N + 2 |
| BBNF | 1.0 | N + 2 |

No `input.len() * 4` anywhere. **W2.3 holds for JSON**; others sit
at 1.0 — legitimately higher mined density, not `× 4` pessimism.

## 9. Bottom-line verdict — JSON twitter

| Lever | State | Evidence |
|---|---|---|
| Inline DFA bodies (W1.4-aggro) | **present** | 14 `'__dfa:` blocks in run; 0 `__dfa_match_*` symbols |
| `match table.states[N]` hoist (W1.1) | **present** | 2 residual hits are doc-comments |
| `dispatch_one` / `__dfa_match_*` elim | **present** | nm: zero hits |
| `DtaState::Regex { pattern, payload }` dismantle | **present** | zero match-destructure hits |
| Grammar-profile capacity (W2.3) | **present** | N/2 + 2 for JSON |
| Walker flattening (W2.1) for JSON | **present** | 0 cold_state helpers; run is 22 KB, fits L1i |
| `PRECEDENCE_LUT` literal hoist | **present, inert** | emitted, 0 nonzero (no JSON operators) |
| `SHAPE_DICT` / `KEYWORD_PHF` / `CLASSIFY_TABLE_*` / `ACTIVE_COLUMNS` / `BRANCH_PRIORS` / `DEDUP_ELIGIBLE_RULES` / `LIST_RULES` | **absent** | `&[]` or zero symbols |
| `bbnf_simd_scan::neon` / `scan_quoted_string_simd` | **absent** | 0 symbols |
| Cross-crate helper inlining | **partial** | 153 BL calls to bbnf_tape helpers + `RawVec<*>::grow_one` inside the JSON walker `run` |

**JSON twitter: 7 firing, 1 inert-but-correct, 6 absent, 1 partial.**
Remaining JSON gap lives in 153 cross-crate BL calls per walker
invocation. CSS gap adds 2 283 `__cold_state_N` calls from a 154 KB
`run` that overflows L1i (total walker module = 1.9 MB text).

Artefacts: `/tmp/p6/{nmR,otool}-<bench>.txt`, live
`crates/core/src/grammar/generated.rs`, prepared
`.profiles/samply/prebuild/expand/<bench>/expand.rs`,
`.profiles/samply/prebuild/binaries.tsv`.
