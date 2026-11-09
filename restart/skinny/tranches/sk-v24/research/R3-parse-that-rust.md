# R3 — parse-that Rust side (SK-V24 recon)

Repo: `/Users/mkbabb/Programming/parse-that` — **READ-ONLY**. Untracked WIP present
(`git status`: `?? rust/.cargo/`, `?? rust/parse_that/rustc-ice-2026-04-22T15_05_48-88533.txt`).
All paths below are absolute-relative to `/Users/mkbabb/Programming/parse-that/`.
Every path/line verified via `ls`/`rg`/`Read`. No build tools run.

Head commit: `ef10d5b` (`docs(coordination): VALUEJS-PT-E`). Recent: `7eab78c` S.H4 1.0.0 cut,
`043c4d1` S.H2 legacy cut, `934b2fa` S.H1 packrat epoch.

---

## 1. Workspace layout

Cargo workspace root: `rust/Cargo.toml:2` → `members = ["src", "parse_that", "bootstrap", "regex"]`,
`resolver = "2"`. Profiles: `release-lto` (lto=true) `rust/Cargo.toml:6`; `bench` (lto="fat",
codegen-units=1, opt-level=3) `rust/Cargo.toml:10`.

| Member dir | Crate name | Ver | Role | src LOC |
|---|---|---|---|---|
| `rust/parse_that/` | `parse_that` | 0.4.0 | combinator core + scanners + domain parsers | 13,553 |
| `rust/regex/` | `bbnf-regex` | 0.1.0 | bespoke HIR→NFA→DFA regex engine | 6,742 |
| `rust/bootstrap/` | `regex-bootstrap` | 0.1.1 | thin re-export of `parse_that::regex::generated` for `regex_parse` bench (`rust/bootstrap/Cargo.toml`) | ~ |
| `rust/src/` | `parse_that_cli` | 0.1.0 | `[[bin]] name="main"`, JSON+CSV bench runner (`rust/src/Cargo.toml`, `rust/src/main.rs`) | — |

`parse_that` deps (`rust/parse_that/Cargo.toml:89`): `bbnf-regex`(path `../regex`), `pprint`0.3,
`memchr`2.5, `aho-corasick`1.1, `fast-float2`0.2, `smallvec`1.13, **`smallbox`0.8 features=["nightly"]**
(`:97`), `colored`(opt, `diagnostics` feature). dev-deps: `divan`, `serde_json`, `pest`, `nom`,
`jiter`, `simd-json`, `sonic-rs`, `winnow`, `serde_json_borrow`, `lightningcss`, `cssparser`, `mimalloc`.

`bbnf-regex` deps (`rust/regex/Cargo.toml:17`): `smallvec`1.13, `serde`(opt), `proc-macro2`1,
`rustc-hash`2, and path-patched `egraph`0.1 / `egraph-derive`0.1 / `csp-solver`0.1 (`:35-37`).
`bbnf-regex/src/lib.rs` carries **no `#![feature]`** — stable-clean at the feature-attr level
(nightly still forced transitively by the shared toolchain + path deps).

### CRITICAL — external path-dep resolution (`rust/.cargo/config.toml`, UNTRACKED/gitignored)

`[patch.crates-io]` redirects to sibling repos (paths relative to `rust/`):
```
parse_that      -> parse_that            (in-tree)
bbnf-regex      -> regex                 (in-tree)
pprint          -> ../../pprint/rust
pprint_derive   -> ../../pprint/rust/derive
bbnf            -> ../../bbnf-lang/crates/core
bbnf-ir         -> ../../bbnf-lang/crates/ir
bbnf-ser        -> ../../bbnf-lang/crates/ser
csp-solver      -> ../../csc411/CSC411_HW2_ProgrammingQuestion/csp-solver
egraph          -> ../../bbnf-lang/crates/egraph
egraph-derive   -> ../../bbnf-lang/crates/egraph-derive
```
All 7 external sibling dirs VERIFIED to exist. `egraph`, `egraph-derive`, `csp-solver` have **no
crates.io registry version** — the patch table is MANDATORY to build; without it resolution fails.
The `bbnf_derive` edge is retired (config comment; commit `6142387f` in bbnf-lang).

Toolchain pin `rust-toolchain.toml:39` (at repo ROOT, not under `rust/`): **`channel =
"nightly-2026-04-11"`** (resolves rustc `1.96.0-nightly 02c7f9bec 2026-04-10`), components
`rustfmt clippy rust-src rustc-codegen-cranelift`, `profile="minimal"`. The comment block
(`:6-18`) records 93 `rustc-ice-*.txt` incremental-cache ICEs (`on_disk_cache.rs:663 cannot decode
AttrId`) between 2026-04-15..22 from unpinned nightlies — the pin avoids them. A stray
`rust/parse_that/rustc-ice-2026-04-22T15_05_48-88533.txt` is present (untracked artifact).

`data/` (bench corpus, 124M) is **gitignored** and lives at repo ROOT (`data/json/*.json`,
`data/css/{bootstrap,normalize,tailwind-output}.css`). `target/` currently does NOT exist (nothing built).

---

## 2. Nightly / stable-blocker inventory (the stable-port surface)

Three independent blockers. Ranked by removal cost.

### 2a. `#![feature(portable_simd)]` — PRIMARY blocker (widest surface)
Declared `rust/parse_that/src/lib.rs:1`. Consumes `std::simd` (`u8x16`, `simd_eq`, `to_bitmask`,
`splat`, `Mask`) across 8 files:

| File | portable_simd hits | notes |
|---|---|---|
| `rust/parse_that/src/scanners.rs` | 22 | shared inline byte scanners |
| `rust/parse_that/src/parsers/scan/decode.rs` | 17 | JSON string decode-to-arena (`u8x16` only; the 2 "x86" tokens are `_mm_storeu_si128` *in comments*, `:168`,`:221`) |
| `rust/parse_that/src/parsers/scan/ws_comment.rs` | 16 | ws/comment scanners |
| `rust/parse_that/src/parsers/scan/structural_bitmap.rs` | 13 | simdjson structural bitmap |
| `rust/parse_that/src/parsers/scan/quoted_simd.rs` | 7 | `odd_parity_backslashes` etc. |
| `rust/parse_that/src/span_parser/scan.rs` | 7 | span-parser inline scan |
| `rust/parse_that/src/parsers/scan/quote_parity.rs` | 4 | `classify_stripe_64` (`:73` `use std::simd::prelude::*`) |
| `rust/parse_that/src/state.rs` | 2 | `PaddedView` / padded-copy machinery |

Stable port ⇒ migrate `std::simd` → `core::arch` intrinsics or a stable crate (`wide`/`simdeez`)
or scalar. Removal touches every hot scanner.

### 2b. `#![feature(fmt_helpers_for_derive)]` + `#![feature(stmt_expr_attributes)]`
`rust/parse_that/src/lib.rs:2-3`. Narrow; `stmt_expr_attributes` supports `#[cfg]` on the SIMD
`store_chunk` statement pattern; `fmt_helpers_for_derive` from a `Pretty`/derive path.

### 2c. `smallbox` nightly feature — combinator-core allocator blocker
`rust/parse_that/Cargo.toml:97` `smallbox = { version="0.8", features=["nightly"] }`. Sole use site:
the parser vtable box `rust/parse_that/src/parse.rs:1,62,74` — `Parser.parser_fn:
SmallBox<dyn ParserFn<'a,Output>+'a, S32>`. Stable port ⇒ drop the `nightly` feature (may cost the
inline-storage optimization) or replace with `Box<dyn ParserFn>` / an in-house small-box.

**Note:** `rust/CLAUDE.md:11,127` claim `#![feature(cold_path)]` — **STALE**; `cold_path` appears
nowhere in src (`rg` = 0 hits). CLAUDE.md also says `Box<dyn ParserFn>` (`:107`) but actual is
`SmallBox`. Treat CLAUDE.md as advisory, not authoritative.

`bbnf-regex`, `regex-bootstrap`, `parse_that_cli` declare no `#![feature]`.

---

## 3. Regex engine — `bbnf-regex` (`rust/regex/`), 6,742 src LOC

Public surface enumerated at `rust/regex/src/lib.rs:26-38` (re-exports). Module map + LOC:

### HIR (1,064 LOC)
- `rust/regex/src/hir/mod.rs` (277) — `pub enum Hir` (`:216`; variants), `ByteRange`(`:12`),
  `CodepointRange`(`:27`), `CharClass`(`:45`), `Look`(`:172`), `Repetition`(`:194`),
  `ParseError`(`:239`), `ParseOptions`(`:260`).
- `rust/regex/src/hir/parser/` — hand-written recursive-descent parser. Entry:
  `rust/regex/src/hir/parser/mod.rs:153` `pub fn parse(pattern) -> Result<Hir,ParseError>`,
  `:158` `pub fn parse_with(pattern, &ParseOptions)`. Files: `mod.rs`(161), `class.rs`(217),
  `escape.rs`(168), `group.rs`(75), `repetition.rs`(71), `atom.rs`(55), `cursor.rs`(40).

### Automata — NFA→DFA→minimize (1,023 LOC)
- `rust/regex/src/automata/nfa.rs` (449) — Thompson NFA. `pub struct Nfa`(`:46`), `NfaState`(`:28`),
  `EpsilonEdge`(`:21`, `epsilon: SmallVec<[EpsilonEdge;4]>` `:32`), `pub const DEAD`, `StateId`.
  `Nfa::from_pattern`(`:63`), `from_hir`(`:71`), `transition_byte_sets`(`:90`),
  priority-tagged `add_epsilon`(`:118`).
- `rust/regex/src/automata/dfa.rs` (438) — subset/powerset construction + **Hopcroft minimization**.
  `pub struct Dfa`(`:45`) — `byte_classes:[u8;256]`(`:52`), `DfaState`(`:14`), `DfaOptions`(`:24`,
  `minimize` flag `:27`). `Dfa::compile`(`:62`), `compile_with`(`:67`), `find_at`(`:116`),
  `find_at_shortest`(`:143`). Pipeline in `compile_with`: byte-equiv classes
  (`compute_byte_classes` from `sets/equiv.rs`) `:80` → `class_representatives`(`:83`) →
  `subset_construction`(`:86`,`:218`) → `hopcroft_minimize` when opts.minimize `:90`,`:291`.
- `rust/regex/src/automata/accel.rs` (131) — `AccelStrategy`, `StateAccel`, `detect_accel`.

### Classification (1,148 LOC — single largest file)
- `rust/regex/src/classify.rs` — `pub enum RegexClass`(`:55`) with `canonical_pattern()`(`:132`);
  `pub fn classify_regex(pattern)`(`:157`), `classify_regex_from_hir(&Hir)`(`:177`),
  `ClassRangeInfo`(`:36`). Drives `FnDescriptor` specialization on the bbnf side.

### Sets (493), Info (893), Algebra (116), E-graph (1,253), misc
- `sets/`: `byteset.rs`(251, `ByteSet`), `charset.rs`(143, `CharSet128`), `equiv.rs`(94,
  `compute_byte_classes`).
- `info/`: `mod.rs`(412, `RegexInfo`/`EngineSet`/`QuantifiedClassInfo`), `classify.rs`(153),
  `width.rs`(133), `literal_prefix.rs`(85), `one_pass.rs`(72), `dfa_size.rs`(38).
- `first.rs`(158) — `regex_first_chars` / `_from_hir`. `utf8.rs`(243), `unicode.rs`(127),
  `path_lexer.rs`(186, `lex_path`/`PathToken`, uses `proc-macro2::Span`).
- `egraph/` (1,253) — HIR equality-saturation: `mod.rs`(200), rules `repetition.rs`(156),
  `union.rs`(108), `flatten.rs`(107), `superset.rs`(90), `util.rs`(76), `redundant.rs`(68),
  `mod.rs`(73); `translate.rs`(132), `cost.rs`(102), `node.rs`(73), `saturation_cache.rs`(68).
  Depends on external `egraph`/`egraph-derive`/`csp-solver` (patch table §1). `algebra/` (116):
  `mod.rs`(100), `superset.rs`(8), `union.rs`(8).

No x86/AVX intrinsics anywhere in `bbnf-regex` (pure scalar + SmallVec).

---

## 4. Scanners — full x86/AVX census

**Only two files contain real x86 intrinsic call-sites.** All other SIMD is portable_simd (§2a).

### `rust/parse_that/src/parsers/scan/quote_parity.rs` (210 LOC) — simdjson quote-parity prefix-XOR
- `prefix_xor(mask:u64)` `:41`. x86 path `:42-49` gated
  `#[cfg(all(target_arch="x86_64", target_feature="pclmulqdq"))]`:
  `use std::arch::x86_64::*` `:44`, `_mm_set_epi64x` `:45-46`, `_mm_clmulepi64_si128` `:47`,
  `_mm_cvtsi128_si64` `:48`. aarch64 path `:51-56` (`target_feature="aes"`): `vmull_p64` `:54`,
  `vgetq_lane_u64`/`vreinterpretq_u64_p128` `:55`. Scalar fallback (5 shift-xor) `:58-68`.
- `classify_stripe_64` `:71` uses portable `u8x16` (`:73` `use std::simd::prelude::*`).
- `escape_mask_64` `:89`, `compute_in_string_bitmap` `:121`, `filter_quote_parity` `:175`
  (`positions:&mut Vec<u32>` — note `vec![0u64; nwords]` alloc `:183`). `STRIPE=64` `:34`.

### `rust/parse_that/src/parsers/scan/number_simd.rs` (267 LOC) — SIMD digit accumulation
- x86_64 SSE4.2 impl `:145-235`, gated `#[cfg(target_arch="x86_64")]` +
  `#[target_feature(enable="sse4.2")]` (`:146`,`:172`), **runtime `is_x86_feature_detected!("sse4.2")`**
  `:222`. Intrinsics (`use core::arch::x86_64::*` `:149`): `_mm_loadu_si128`+`__m128i` `:152`,
  `_mm_set1_epi8` `:153-154`,`:162`, `_mm_max_epu8`/`_mm_min_epu8` `:159`, `_mm_cmpeq_epi8` `:160`,
  `_mm_andnot_si128` `:162`, `_mm_movemask_epi8` `:163`.
- aarch64 NEON impl `:29-136` (`use core::arch::aarch64::*` `:32`): `vld1q_u8`,`vsubq_u8`,`vdupq_n_u8`,
  `vcgtq_u8`,`vld1_u8`,`vget_low/high_u8`,`vand_u8`,`vaddv_u8`.
- Scalar fallback `scan_digits_simd` `:239` for other arches. `POW10:[u64;17]` `:249`.
- Public entry `scan_digits_simd(view:PaddedView, offset)->Option<SimdDigitResult>` (per-arch
  `:118`/`:212`/`:241`); `SimdDigitResult{value,count}` `:17`.

### `rust/parse_that/src/parsers/scan/decode.rs` (405 LOC) — JSON string decode-to-arena
- **portable_simd only** (`use std::simd::prelude::*` `:19`, `u8x16`). No live x86 intrinsics; the two
  x86 tokens are `_mm_storeu_si128` in doc comments `:168`,`:221`. `StringPayload`
  {Borrowed|Owned} `:35`. Entry `decode_json_string_to_arena` `:52`. Kernels:
  `simd_scan_or_decode` `:68`, `owned_decode` `:138`, `decode_escapes_simd` `:171`,
  `store_chunk` (`u8x16`→unaligned 16-byte store into `arena:&mut Vec<u8>`) `:227`, scalar tails
  `:241`/`:262`, `decode_one_escape` `:296`. Depends on `odd_parity_backslashes` from
  `quoted_simd.rs` (`:21`).

### Supporting PaddedView invariant (feeds all SIMD kernels)
`rust/parse_that/src/state.rs:57` `pub const INPUT_PAD_BYTES: usize = 64`; `pub struct PaddedView`
`:122` (`bytes.len() == len + 64`, trailing 64 NUL). Lets every stripe load skip a tail guard.

---

## 5. Combinator core + allocation hotspots

Core type `rust/parse_that/src/parse.rs:61` `Parser<'a,Output>{ parser_fn: SmallBox<dyn
ParserFn+'a,S32>, flags:u8 }`. `ParserFn` trait `:41`, blanket impl for `Fn` `:45`.
Flag fast-paths in `call` `:80` (hot: flags==0 → direct call `:82`; FLAG_TRIM_WS `:86`),
cold path `call_with_flags_cold` `:97`. `ParserResult=Option<Output>` `:39`.

Allocation hotspots (heap / per-parse):
- `rust/parse_that/src/parse.rs:74` — `SmallBox::new(parser_fn)` per combinator construction
  (inline ≤S32, else heap). **Nightly-gated** (§2c).
- `rust/parse_that/src/lazy.rs:21,31` — `LazyParser.parser_fn: Box<dyn LazyParserFn>` + `Box::new`
  (unconditional heap box, one per `lazy()`).
- `rust/parse_that/src/combinators/methods/repetition.rs` — `many` pushes into `Vec` (`values.push`
  `:28`); `many_small`/`sep_by_small` use `SmallVec` inline storage (`:63,68,102,106,120`).
- `rust/parse_that/src/combinators/methods/sep_by.rs` — `values.push` at `:36,52,100,119,165,187`
  (Vec growth in sep-by hot loops).
- `rust/parse_that/src/combinators/methods/sequence.rs:72` — `delimiter.clone()` per chain build.
- `rust/parse_that/src/combinators/methods/recover.rs:37` — `sentinel.clone()` on recovery.
- `rust/parse_that/src/leaf.rs:182,203,209` — `TakeUntilScan::Lut(Box<[bool;256]>)` LUT box;
  `:345,348,352` build-time `Vec`/push.
- `rust/parse_that/src/combinators/macros.rs:2,80` — `seq!`/`alt!` collapse N-1 boxes into ONE
  `Box<dyn ParserFn>` (deliberate alloc reduction).

Combinator method modules: `rust/parse_that/src/combinators/methods/{sequence,alternation,
repetition,sep_by,minus,map,recover}.rs` (+`mod.rs`, `macros.rs`). Second tier: `span_parser/`
(enum-dispatched, vtable-free — `mod.rs`, `leaves.rs`, `constructors.rs`, `methods.rs`,
`combinators.rs`, `sep_by.rs`, `wrap.rs`, `scan.rs`, `span_scanner.rs`, `assertions.rs`).

---

## 6. Bench harness + exact commands

Harness = **divan** (`harness=false`, `#[global_allocator] mimalloc`). Bench targets registered in
`rust/parse_that/Cargo.toml:20-85` and `rust/regex/Cargo.toml:45-59`.

`parse_that` benches (`rust/parse_that/benches/`):
- `parse_that/combinator.rs` (`--bench parse_that_combinator`) — JSON parser, reads
  `../../data/json` (`combinator.rs:12`).
- `parse_that/css.rs` (`--bench parse_that_css`) — reads `../../data/css`.
- `parse_that/micro.rs` (`--bench micro_parse_that`) — take_until/number/quoted micro-benches.
- competitor baselines (`benches/competitors/`): `--bench {nom,serde,pest,jiter,simd_json,
  sonic_rs,winnow,serde_json_borrow,lightningcss,cssparser}`.

`bbnf-regex` benches (`rust/regex/benches/`): `--bench {regex_parse,regex_classify,
regex_hir_egraph,regex_dfa_compile}` (Tranche P; corpus of 50+ patterns; regex_dfa_compile does
full parse→HIR→NFA→DFA→minimize + `find_at` match loop).

`regex-bootstrap`: `--bench regex_parse` (diagnostic re-export).

Exact commands (run from `rust/`, nightly auto via toolchain pin):
```
cargo bench -p parse_that --bench parse_that_combinator
cargo bench -p parse_that --bench parse_that_css
cargo bench -p parse_that --bench micro_parse_that
cargo bench -p bbnf-regex --bench regex_dfa_compile
cargo bench -p bbnf-regex --bench regex_parse
cargo bench --workspace                 # all bench targets
```
Profile note: `[profile.bench]` fat-LTO/cgu=1/O3 (`rust/Cargo.toml:10`). `just` targets in
`justfile` are `rs-{clippy,test,build}` + `rs-all` (no bench recipe). MEMORY discipline:
one cargo per target dir; benches sequential; cold per-parse only.

---

## 7. Test suites + exact commands

Integration tests only (repo convention: no inline `#[cfg(test)]`).

`parse_that/tests/` (~400 tests): `combinator_test.rs`(99), `debug_test.rs`(61, diagnostics feature),
`css_parse_test.rs`(52), `decode_test.rs`(44), `json_test.rs`(37), `quoted_simd_test.rs`(33),
`number_fastpath_test.rs`(21), `ws_comment_test.rs`(18), `css_recovery_test.rs`(13, diagnostics),
`number_simd_test.rs`(12), `split_test.rs`(11), `padded_input_test.rs`(8), `quote_parity_test.rs`(7),
`structural_bitmap_test.rs`(6), `csv_test.rs`(2), `regex.rs`(wiring only).
Regex-bridge subtests via `#[path]` include in `parse_that/tests/regex.rs`
(→ `parse_that/tests/regex/{accel(4),byteset(17),dfa(49),equiv(6),hir(4),nfa(13),parser(22),
utf8(9)}_test.rs`).

`regex/tests/` (bbnf-regex): `info.rs`(11), `path_lexer.rs`(15), `egraph_rules_{flatten(3),
repetition(3),redundant(2),superset(2),union(2)}.rs`, `egraph_simplify.rs`(3),
`algebra_{superset(2),union(1)}.rs`.

Exact commands (from `rust/`):
```
cargo test --workspace
cargo test -p parse_that
cargo test -p parse_that --features diagnostics        # unlocks debug_test / css_recovery_test
cargo test -p parse_that --test decode_test
cargo test -p parse_that --test regex                  # regex-bridge subtests
cargo test -p bbnf-regex
cargo clippy --workspace -- -D warnings
```
Feature-gated tests (`debug_test`, `css_recovery_test`) require `--features diagnostics`
(pulls `colored`).

---

## 8. Scratch-copy build recipe (prover, zero mutation of original)

The original tree must stay untouched (untracked WIP present). A `git clone`/`git archive` is
INSUFFICIENT — it drops `rust/.cargo/config.toml` (gitignored) and the gitignored `data/` corpus,
so the build/bench would fail. Copy the working tree with `rsync`, excluding heavy/derived dirs,
then repoint only the *external* patch paths at the originals (read-only consumption; cargo only
reads path deps).

```bash
SRC=/Users/mkbabb/Programming/parse-that
DST=/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/scratchpad/pt-scratch

# 1. Copy working tree (dotfiles incl. .cargo, EXCLUDE target/.git/node_modules; keep data/).
rsync -a \
  --exclude 'target/' --exclude '.git/' --exclude 'node_modules/' \
  --exclude '*.png' --exclude 'rustc-ice-*.txt' \
  "$SRC/" "$DST/"

# 2. Repoint the 7 EXTERNAL sibling patch deps to the originals (absolute, read-only).
#    In-tree patches (parse_that, bbnf-regex) stay relative and resolve inside the copy.
python3 - "$DST/rust/.cargo/config.toml" <<'PY'
import sys, re
p = sys.argv[1]; s = open(p).read()
s = s.replace('../../pprint/', '/Users/mkbabb/Programming/pprint/')
s = s.replace('../../bbnf-lang/', '/Users/mkbabb/Programming/bbnf-lang/')
s = s.replace('../../csc411/', '/Users/mkbabb/Programming/csc411/')
open(p,'w').write(s)
PY

# 3. Build/test/bench in the copy with an ISOLATED target dir (never touch $SRC/rust/target).
cd "$DST/rust"                                   # rust-toolchain.toml at $DST/ pins nightly-2026-04-11
export CARGO_TARGET_DIR="$DST/target"
cargo test --workspace
cargo bench -p parse_that --bench parse_that_combinator
```

Guarantees / gotchas for the recipe:
- `rust-toolchain.toml` sits at repo ROOT; `rsync "$SRC/"` copies it to `$DST/` so cargo (cwd
  `$DST/rust`) walks up and finds the `nightly-2026-04-11` pin. If you copy ONLY `rust/`, the pin is
  lost — instead run under `rustup run nightly-2026-04-11 cargo …` or `RUSTUP_TOOLCHAIN=...`.
- `data/` (124M, gitignored) is copied by the rsync (root-level, not excluded); benches resolve it
  via `env!("CARGO_MANIFEST_DIR")/../../data/*` → `$DST/data`. Do NOT exclude it if benching.
- `rust/.cargo/config.toml` is gitignored — the rsync (dotfiles included) carries it; step 2 only
  rewrites the 3 external prefixes. The external repos (pprint, bbnf-lang, csc411) are consumed
  read-only; do not mutate them.
- Set `CARGO_TARGET_DIR` to a scratch path so no `target/` is ever written under `$SRC` (and honor
  the one-cargo-per-target-dir rule).
- The `egraph`/`egraph-derive`/`csp-solver` crates have no registry version; the patch table is
  load-bearing — a broken step 2 yields unresolved-dependency errors, not fallbacks.
