# PPRINT ADJUDICATION AND FINAL DESIGN (2026-11-05)

Adjudicator seat. Basis: the design, both critics, and my own check of pprint at local master `2b4d2d4`.

Confirmed in source at `rust/src/op.rs`:
- Line 11: `Text(&'a str)`.
- Line 49: `GroupStart{flat_width: u16}`.
- Line 78: `Mediumline`.
- Line 95: `IfBreak{break_count: u16, flat_count: u16}`.

Other checks:
- `render.rs` has `use_tabs`.
- grep for `Mediumline`/`mediumline` across `~/Programming/bbnf-lang/crates`: no hits. The only producer is `builder.rs:235`.
- Two critic claims are taken on their own probes and not re-run: the v2 op size of 24 B (critic1 `sz.rs`) and `pretty` 0.12.5.

## A. Rulings

### Critic 1

| # | Ruling | Reason |
|---|---|---|
| C1-O1 (semantics incoherent) | **ACCEPT** | Lindig, Wadler and Prettier differ in how far `fits` looks into the rest. The normative semantics is now **Wadler `best`/`be`/`fits` with rest lookahead**, which is what the `pretty` crate implements. The oracle is a literal tree transcription of Wadler 2003 §§3–5 (`be`, `better`, `fits`) plus Leijen's `align`/`fill`. Each oracle function names the equation it transcribes. Lindig is dropped as the reference. Prettier compatibility leaves the pprint goals (see C1-O4). |
| C1-O2 (static trailing broken by fill/ifBreak; O(n·depth)) | **ACCEPT** | Folded into L3′ (§B). `trailing` is computed in amortized O(n) with a monotone stack of (depth, base offset). Proof sketch: each op is pushed and popped at most once. Generators put fill and ifBreak inside trailing regions on purpose. |
| C1-O3 (trivia absent) | **ACCEPT** | New unit PP-U4b. New ops `LineSuffix`/`LineSuffixBoundary`/`BreakParent`, each with a law. G3 becomes token-and-comment stream equality. G7 and G8 depend on U4b. |
| C1-O4 (G7 byte-identity with Prettier = writing a CSS formatter) | **ACCEPT** | G7 and G8 move out of pprint into a gorgeous/bbnf "formatter parity" plan of their own. That plan keeps a per-file diff budget the owner rules on before any throughput claim, and compares throughput only between tools with equal features (trivia on, same width, same quoting). pprint keeps only G7-render: render time on Docs that gorgeous produces. |
| C1-O5 (G6 different jobs) | **ACCEPT** | G6 splits in two. G6a: always-break mode against `to_string_pretty`, valid only with byte-equal output. G6b: width-fitting mode, reported as different work with no win claimed against serde. dtoa cost is reported separately (canada). |
| C1-O6 (statistics weak) | **ACCEPT, amended** | n ≥ 30, interleaved ABAB inside one guarded window, bootstrap 95% CI on the ratio of medians. A claim needs the CI lower bound to clear the target. Records carry allocator, profile (LTO, cgu=1, or the competitor's own `[profile.release]`), low-power flag and thermal state. `powermetrics` needs root, so thermal state is recorded where available, as `pmset -g therm`, and the field is otherwise written null. Biome in-process and Biome CLI are measured separately (this lives in the parity plan). |
| C1-O7 (G4 pits correctness against speed; G5 from NOISY figures) | **ACCEPT** | G2 and G3 gate U4. G4 becomes a recorded budget, "CI upper bound of the v2/greedy ratio ≤ 1.05"; if it is exceeded, the owner rules, and it never blocks a correct U4. G5's thresholds are removed until the first QUIET v2 record, then set by owner ruling. |
| C1-O8 (publish before consumers) | **ACCEPT** | `0.4.0-alpha.N` from U4 onward. U9 and U10 run against the alpha, which meets "no path patch". The stream format is frozen in U4, not U12. Stable 0.4.0 comes after U9, U10 and the U12 format conformance. |
| C1-O9 (hints in e-class identity over-constrain the optimizer; truncated) | **ACCEPT (thrust)** | Formatting reads the **CST**, whose shape is fixed by the source grammar, not by the optimized parser IR. The obligation on the optimizer narrows to one invariant owned by the IR/passes seats: formatting output is byte-identical with the optimizer on and off. Hints do not join e-class identity. |

### Critic 2

| # | Ruling | Reason |
|---|---|---|
| C2-O1 (L3 false under fill ancestor and HAS_HARD siblings) | **ACCEPT** | (a) Inside a fill ancestor, trailing is stored per `FillSep` and decided at render time, O(1) per op. (b) The trailing region stops at the first line op of any later group that has `HAS_HARD`, because that group is known to be in break mode. This is law L3′. |
| C2-O2 (closed set vs `Mediumline` and tabs) | **ACCEPT** | `Mediumline` is a column combinator that L4 excludes, and grep finds no bbnf-lang users. It is **deleted** in U4. U0 re-greps gorgeous, the emitter and `typescript/`; any hit escalates to the owner instead of keeping the op. Tabs are forbidden inside leaves (`Err` from the builder). Tabs survive only as indentation output (`use_tabs`). Indent width per level is a fixed `indent` count, so static width holds. |
| C2-O3 (trivia) | **ACCEPT** | Merged with C1-O3. Comment-bearing inputs join every formatter corpus. |
| C2-O4 (L7 vacuous or false) | **ACCEPT** | L7 is replaced by three checkable properties. (i) No line exceeds w unless an unbreakable leaf does. (ii) At w = ∞, the output equals the `flatten` rendering for docs without `HAS_HARD`. (iii) The output agrees with the oracle at every width tested. |
| C2-O5 (`Text(&'static str)` regression) | **ACCEPT** | The source confirms `Text(&'a str)`. The ABI keeps the lifetime: `Doc<'a>`, with `Text`/`QuotedText` borrowing. |
| C2-O6 (publish order) | **ACCEPT, merged with C1-O8** | The alpha route replaces a git dependency, because the "no path or git patch" CI step needs a registry dependency. |
| C2-O7 (hidden coupling with the IR/passes seats) | **ACCEPT** | The §4.1/§4.2 obligations are restated as a gated invariant in the IR/passes designs, with one falsifier: format output byte-identical with the optimizer on and off. U9 shrinks to an ABI port. `PrettyPlan` work goes to the IR wave. |
| C2-O8a (paired runs, bootstrap CI) | **ACCEPT** | Same as C1-O6. |
| C2-O8b (P/E cores) | **ACCEPT** | Bench processes run at user-interactive QoS (`taskpolicy -t 0 -l 0`, or `pthread_set_qos_class_self_np` in the harness). The QoS and the cluster are recorded where they can be observed. |
| C2-O8c (quiet window may never come) | **ACCEPT** | Wait at most 60 min for 1-minute load < 8. Otherwise run, label the record NOISY, and allow no win claim. No gate may depend on a QUIET record. A gate needs either correctness or a recorded budget. |
| C2-O8d (G6) | **ACCEPT** | Same as C1-O5. |
| C2-O8e (post-hoc thresholds) | **ACCEPT** | Same as C1-O7. |
| C2-O8f (G7 inputs; truncated) | **ACCEPT (thrust)** | G7 is now in the parity plan. Its corpus is pinned by URL and sha256 at fixed versions, the tailwind build is pinned by version and config hash, and comment-bearing files are included. |

### Design-internal corrections

- `GroupHdr.flat_width: u32` fixes the u16 overflow present today (group width > 65,535).
- The `IfBreak` ranges become u32.
- The const assert `size_of::<FmtOp>() <= 24` stays. The critic probes show v2 fits only because of the niche in `String`'s capacity field, so the assert is the guard against losing it.

## B. Final design: pprint 0.4.0

**Thesis (unchanged).** The flat `FmtOp` stream stays, with a full Wadler/Leijen algebra on top of it. The builder computes the layout metadata at `finish()`. The renderer is one forward loop, with no backtracking and no allocation per group.

### B.1 Layers

**`op.rs`, ABI v2, ≤ 24 B (const-asserted).**
- **Leaves:**
  - `Text(&'a str)`, `OwnedText(String)` and `QuotedText(&'a str)`. A multiline leaf has the width of its last line, and multiline text sets `HAS_HARD`.
  - `Ascii(AsciiByte)`, which replaces `Char(u8)`.
  - Numeric leaves as today: `I64/U64/I128/F64`.
  - `WrapStart.open`, `WrapEnd.close` and `Sep` take `AsciiByte`.
  - Tab bytes in a leaf give `Err`.
- **Structure:**
  - `GroupStart{hdr:u32}` indexes a side table `GroupHdr{flat_width:u32, trailing:u32, end_pc:u32, flags: HAS_HARD|FILL}`.
  - `GroupEnd`.
  - `Nest(i16)`/`NestEnd`, `Align`/`AlignEnd` and `Hang`.
  - `IfBreak{then:Range<u32>, else:Range<u32>}`.
  - `FillSep{hdr:u32}` (per-separator width).
- **Lines:** `Line`, `SoftLine`, `HardLine` (zero width; the break is structural through `HAS_HARD`) and `LiteralLine`.
- **Trivia (U4b):** `LineSuffix{range}`, `LineSuffixBoundary` and `BreakParent`.
- **Builder sugar:** `IndentStart/End`, `BreakLine` and `CommaSep`.
- **Deleted:** `Mediumline`.
- Every variant has a law in B.2; the instruction set is closed.

**`builder.rs`, the compile step.**
- A stack of open groups. Each `GroupEnd` back-patches its header and propagates `HAS_HARD` (and `BreakParent`) to the parent.
- `finish()` runs one backward pass that computes `trailing` with a monotone stack (amortized O(n)).
- Output: an immutable `Doc<'a>{ops, hdrs}` that can be rendered many times. Unbalanced input returns `Err`.

**`render.rs`.**
- One forward loop.
- The mode stack is a growable bit vector, so there is no depth cliff.
- Indentation comes from a cached run of spaces.
- Width is measured in display columns.
- The output `String` is built only from `&str` and `AsciiByte` pushes. There is **no `from_utf8_unchecked`**, so `render.rs` has zero `unsafe`.

**`width.rs`.**
- ASCII is measured on a fast path using the SIMD all-ASCII check.
- Everything else goes through `unicode-width` at its current release (East Asian Wide = 2).

**`simd/`.**
- Kernels: `ascii_run_len` and `find_escape` (`"`, `\`, < 0x20).
- NEON on aarch64; scalar is always present and is the oracle.
- AVX2 behind `is_x86_feature_detected` only if the owner rules on R1.

**Other modules.**
- `pretty.rs` and `pprint_derive` move to API v2.
- **`pprint-ser`** (today's `bbnf-ser`, which needs only ryu and itoa) moves into the pprint workspace. bbnf depends on it, which clears PP-07.

**Stream format** (specified and frozen in U4):
- A versioned encoding of `{ops, hdrs}`. Its spec is the Rust crate, and Rust provides a conformance corpus.
- The TS renderer (U12) consumes it.

### B.2 Laws (normative semantics: Wadler `best` with rest lookahead)

- **L1, group:** `group(d) = flatten(d) <|> d`. The flat side is chosen if and only if `fits(w − col, flat(d) ++ rest)`.
- **L2, hard:** `HAS_HARD`/`BreakParent` forces the group and every enclosing group to break. The `+10000` sentinel is removed.
- **L3′, rest lookahead.** For group G, the rest is the ops from `end_pc(G)` up to the first of:
  - (a) a line op whose innermost enclosing group is an ancestor of G (which is in break mode);
  - (b) the first line op inside a later group that has `HAS_HARD`;
  - (c) the next `FillSep` of an enclosing fill. For (c), `trailing` stops at that separator and the renderer adds that separator's own decision at run time.

  Lines in later groups without `HAS_HARD` count at flat width. `IfBreak` inside an undecided later group counts its flat branch. `LineSuffix` content counts as zero width. The decision is `col + flat_width(G) + trailing(G) ≤ w`, with the fill case resolved dynamically in O(1).
- **L4, nest/align/hang:** these change indentation only. `column(f)` and `nesting(f)` are excluded, and any future closure combinator needs an owner ruling.
- **L5, fill:** Leijen's `fill`, decided pairwise. Each `FillSep` breaks if and only if the next item's flat width does not fit.
- **L6, ifBreak:** the branch follows the mode of the innermost enclosing group.
- **L7, the replacement for monotonicity:**
  - (i) No line exceeds w unless an unbreakable leaf does.
  - (ii) At w = ∞, the output equals the `flatten` rendering for docs without `HAS_HARD`.
  - (iii) The output agrees with the oracle at every width tested.
- **L8, total:** no panic on any sequence the builder accepts. Unbalanced input returns `Err`.
- **L9, trivia:** `LineSuffix` content is flushed before the next newline. `LineSuffixBoundary` forces a flush through a hardline if a suffix is pending. The default policy preserves at most one blank line.
- **Width:**
  - Measured in display columns.
  - Tabs appear only as indentation (`use_tabs`, one tab per indent level).
  - `flat_width` is a u32, and `max_width` is clamped to `u32::MAX/2`.
- **Oracle:**
  - `tests/oracle/` holds a scalar tree printer transcribed from Wadler 2003 and Leijen's PPrint, with an equation cited per function.
  - It shares no code with `render.rs`.
  - It lowers L3′, fill and trivia to the same tree constructs.

### B.3 Goals

Records live at `pprint/bench/records/<date>-<sha>.json`. Each record holds:
- the machine, load average, QoS, allocator, profile, and low-power and thermal fields where available;
- n ≥ 30 interleaved ABAB runs: median, p25, p75, MAD, and a bootstrap 95% CI on the ratio of medians;
- each competitor's published version and the flags it recommends.

A win needs a QUIET window and the CI lower bound above the target. The quiet-window wait is capped at 60 min; past that the run is recorded as NOISY with no claim. No gate depends on a QUIET record.

| ID | Goal |
|---|---|
| G1 | Zero `unsafe` outside `simd/`. Each kernel has a scalar oracle and a `cargo fuzz` differential target (≥ 10⁸ execs, 0 divergence). Miri is clean on the full suite with the scalar path forced. |
| G2 | 0 divergence from the Wadler oracle over ≥ 10⁶ proptest Docs (depth ≤ 12, w 0–200, mixed Unicode, with generators that place fill, ifBreak, `HAS_HARD` siblings and trivia in trailing regions). 0 divergence from `pretty` 0.12.5 on the shared subset over ≥ 10⁵ Docs. |
| G3 | For every gorgeous grammar (json, css, bnf, ebnf, bbnf, google-sheets): `fmt∘fmt = fmt`; L7(i) holds; and the **token-and-comment stream** is equal before and after formatting. The corpus includes comment-bearing files. |
| G4 | A budget, not a gate: CI upper bound of the v2/greedy ratio ≤ 1.05 at p50 on the divan suite. If it is exceeded, the owner rules. |
| G5 | Ratios against `pretty` (RcDoc) and against `{:#?}` on flat_1k, nested and tuples. The thresholds are set by the owner after the first QUIET v2 record. |
| G6a | Always-break mode against `serde_json::to_string_pretty` (current release), counted only with byte-equal output, on canada, twitter and citm. dtoa time is reported separately. |
| G6b | Width-fitting mode, reported as different work with no claim against serde. |
| G7-render | Render throughput on Docs produced by gorgeous CSS and JSON. |
| G9 | TS renderer against prettier 3.x `printDocToString` on Docs translated through the stream format. Output is byte-identical to the Rust renderer on the conformance corpus. Ratios are reported, and the threshold is owner-ruled after the first QUIET record. Where Prettier's rest-mode `fits` differs, the divergence is recorded, not counted. |
| G10 | Every MB/s, ns or × figure in the README and docs cites a record file. A docs-lint check enforces this. |

**Moved out of pprint:** G7 (CSS end to end against Biome, Malva and Prettier) and G8 (JSON end to end against Biome and dprint) go to the **gorgeous/bbnf formatter-parity plan**. That plan has:
- a per-file diff budget the owner rules on before any throughput claim;
- tools compared only at equal features;
- Biome measured in-process and as a CLI;
- corpora pinned by URL and sha256.

### B.4 Ordered units

Each unit is a branch, a falsifier and a gate, with commits by pathspec, and merges after CI.

- **PP-U0, land and CI.**
  - Branch `pp/v2-base` off `2b4d2d4`. Push it.
  - Clippy `--all-targets -D warnings`.
  - `digit_count` E0277: bind inputs outside the closure.
  - The 04-23 leftovers: drop rust-version 1.85, remove `[profile.bench]`, add `.config/nextest.toml`, track `rust/.cargo/config.toml`.
  - Fix the self-referential symlink.
  - Re-grep for `Mediumline` users across all repos.
  - Gate: CI green (nextest, clippy, `bench --no-run`).
- **PP-U1, soundness.**
  - `AsciiByte`. Delete `from_utf8_unchecked`.
  - Falsifier: `b.char(0xFF)` compiles and emits invalid UTF-8, or Miri reports UB.
  - Gate: the probe is a compile error or `Err`, and Miri is clean. G4 is recorded.
- **PP-U2, structural hard.**
  - `HAS_HARD`/`BreakParent` propagation. Remove the sentinel. Widen the header to u32.
  - Falsifier: `group(a;hardline;b;softline;c)` at w = 20000 still renders `"a\nb c"`. The expected output is `"a\nb\nc"`.
  - Gate: that probe, plus 1,000 nested groups, plus a group with flat width > 65,535.
- **PP-U3, oracle and harness, before the algorithm.**
  - The Wadler-transcription oracle (with an equation cited per function) and the proptest generators.
  - Falsifier: 0 divergence from the greedy renderer, which would mean the generators are too weak.
  - Gate: at least one recorded greedy divergence on a trailing-content case, and the oracle matches `pretty` 0.12.5 on the shared subset.
- **PP-U4, algebra v2.**
  - Header table; L3′ trailing with the monotone stack (amortized O(n)); nest, align and hang; fill with dynamic per-separator trailing; ifBreak; `LiteralLine`; the growable mode stack.
  - Delete `Mediumline`. Ban tabs in leaves.
  - **Freeze the stream format v1.**
  - Publish `0.4.0-alpha.1`.
  - Falsifier: any G2 divergence, or a failure of L7(i–iii) or L8.
  - Gate: G2, and the const assert ≤ 24 B. G4 is recorded as a budget.
- **PP-U4b, trivia.**
  - `LineSuffix`, `LineSuffixBoundary` and `BreakParent` laws (L9), oracle extension, the blank-line policy.
  - Falsifier: a generator Doc with a pending suffix across a hardline diverges from the oracle, or a comment is lost.
  - Gate: G2 with trivia generators. `alpha.2`.
- **PP-U5, display width.**
  - `width.rs` and the `ascii_run_len` kernel with its fuzz target.
  - Falsifier: CJK, emoji or combining marks overflow w under the oracle, or the kernel diverges from scalar.
  - Gate: G1 for the kernel, and the Unicode cases.
- **PP-U6, escape kernel and indent cache.**
  - Falsifier: fuzz divergence, or no gain on `pp_strings`. With no gain, the kernel is deleted.
  - Gate: G1, and a recorded gain (QUIET), or deletion.
- **PP-U7, bench harness.**
  - Record writer and JSON schema: n ≥ 30, ABAB, bootstrap CI, QoS, the 60-min quiet cap.
  - Competitors at their current releases.
  - Falsifier: a record without dispersion, CI or competitor versions validates against the schema.
  - Gate: G5, G6a and G6b records exist, honestly labelled.
- **PP-U9, consumer ABI port (bbnf-lang).**
  - A fresh worktree from origin/master `c9b93adb4`, never the dirty local master.
  - The emitter prettify backend moves to `pprint = "0.4.0-alpha"`. Regenerate `grammar/generated`. Move the parse_that derives. Drop the pprint `[patch.crates-io]` entry.
  - Fix sccache in CI. Add a crates.io-only build step.
  - Scope is the **ABI port only**; `PrettyPlan` and hint work belong to the IR wave.
  - Falsifier: the 957-error `cargo check` returns, or CI is red after setup.
  - Gate: CI green with no patch, and G3 on all six grammars.
- **PP-U10, one BBNF formatter.**
  - Replace `crates/analysis/src/features/formatting.rs` with gorgeous bbnf through pprint. Range formatting comes from a CST span plus the group subdocument.
  - Remove the dead `pprint = "0.3"` dependency.
  - Falsifier: `format_range` panics on a truncated grammar, or the old and new outputs differ on the corpus before deletion.
  - Gate: a fuzzed LSP range request never panics, and the old formatter is deleted.
- **PP-U11, wasm and playground.**
  - The wasm crate enables gorgeous's per-grammar features.
  - The playground compiles the wasm in CI, so four wasm copies become one generated copy.
  - The e2e test "AST + formatted output" runs in CI in headless Chromium, one browser per run, under guarded.
  - Gate: CI builds the wasm, and the e2e test is green.
- **PP-U12, TS renderer.**
  - Blocked until wf_d837f96c closes.
  - `@mkbabb/pprint` 0.4.0 in pprint `typescript/`, consuming stream format v1.
  - Gate: byte-identity with Rust on the conformance corpus, and a G9 record.
- **PP-U8, publish (now last).**
  - pprint 0.4.0, `pprint_derive` 0.3.0 and `pprint-ser` 0.1.0.
  - The CHANGELOG states the break from 0.3.6's Doc tree. Yank nothing.
  - Falsifier: `cargo publish --dry-run` fails without patches.
  - Gate: U9 and U10 green on the alpha, U12 conformance green (or the owner rules to cut stable before U12), and the dry-run clean. Then publish, which the begin-word authorizes.

### B.5 Cross-seat invariants (owned by the IR/passes designs, cited here)

- **XI-1:** formatting reads the CST, whose shape comes from the source grammar. Output is byte-identical with the optimizer (CSP, e-graph, inline, merge) on and off, over the G3 corpus. A diff is a blocker in the passes seat.
- **XI-2:** hints and `PrettyPlan` live in the IR wave's design. pprint consumes only `Doc<'a>` and stream format v1.

### B.6 Open owner rulings

- **R1:** the AVX2 kernel on x86.
- **R2:** G5 and G9 thresholds, after the first QUIET record.
- **R3:** G4, if its budget is exceeded.
- **R4:** the parity plan's diff budgets.
- **R5:** cutting stable 0.4.0 before U12, if wf_d837f96c is still open.
- **R6:** any `Mediumline` user found in the U0 re-grep.

Evidence: `/Users/mkbabb/Programming/pprint/rust/src/op.rs` (lines 11, 49, 78, 95), `/Users/mkbabb/Programming/pprint/rust/src/render.rs` (`use_tabs`), `/Users/mkbabb/.dev-logs/bbnf-lockstep/critic1/sz.rs` (op-size probe).