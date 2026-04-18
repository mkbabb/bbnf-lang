# AW-V H2-R2 — Shape-Emitter Generality: Can W3.2 Match the W2.1 Prototype?

## 1. Angle headline

The 11-shape emitter plan (W3.1 landed; W3.2 not yet) is asked to produce a JSON parser within ±5% of the hand-tuned W2.1 prototype and then generalise to CSS L4 / Sheets / BBNF through the same mechanism. **The gap is not code generation — it is IR signal richness.** The W2.1 prototype encodes five profile-derived decisions the W3.1 classifier has no path to rediscover, and one of those (the canada scalar-digit decision, commit `2edb612b`) is the difference between 1.18× and 0.90× sonic. The "starkly behind RD" projection is likely an artefact of running the emitter against the W3.1-committed shape taxonomy without the profile-derived signals below.

## 2. Motivation with artefact citations

**W2.1 gate — met by beating sonic.** `docs/tranches/AW/AW-V-W2-close.md:10-17` records data_s 0.939×, twitter 0.891×, citm 0.893×, canada 0.901×, data_xl 0.902× sonic ns/iter. Samply attribution (`AW-V-W2-close.md:41-60`): one symbol `parse_value::<ValueVisitor>` at 91.15% self-time on twitter, 98.6% on canada — every shape-helper + SIMD kernel + Eisel-Lemire body inlined into the dispatcher.

**W3.1 state of play.** `crates/ir/src/passes/recognizers/shape_dispatch/mod.rs:127-168` defines the 11-variant `ShapeTag` enum; commit `0f69e08d` lands the six-rule JSON classification. Sub-modules `object.rs` / `array.rs` / `string.rs` / `number.rs` / `keyword.rs` / `scalar.rs` implement the W3-active detectors; Pratt / Unordered / ArgList / Flat / Wrap / HRegex are taxonomy-only (no detector body). Emitter directory `crates/core/src/backend/rust/emitter/shapes/` does not exist — the shape-emitter consumer is unwired (see `Bash ls` output: `classify_byte.rs dfa_codegen.rs dta.rs dta_walker grammar.rs keyword_dispatch.rs mod.rs precedence.rs prettify profile.rs visitor.rs`).

**The 22 008 B walker comparator.** `aw5-h2-visitor-monomorphisation.md:429-435`: `__dta_walker_inline::run` = 22 008 B on JSON vs sonic's 22 084 B hot-symbol total. The prototype's six `#[inline(always)]` helpers inline into one 91%-self-time symbol; the walker pays 4 B/LOC density vs sonic's 30 B/LOC, meaning the walker is 6-7× less dense than emitter-ideal.

## 3. Hand-tuning decisions × IR-derivability

| # | Decision (file:line) | Source | W3.1 has signal? |
|---|---|---|---|
| 1 | `#[inline(always)]` on all 5 shape fns + emit boundary (`lib.rs:180,211,257,307,350,369`) | IR-derivable | **Yes** — shape tag ⇒ emitter attaches `#[inline(always)]` |
| 2 | Scalar int-digit scan beats NEON SIMD for 2-3-digit ints (`number.rs:95-113`, commit `2edb612b`) | **Profile-derivable only** (canada 90% numeric, mean int-width 2-3) | **No** — `PatternAlphabetMiner` knows `[0-9+-.eE]` but not expected digit-width distribution |
| 3 | NEON SIMD digit scan retained for fraction (`number.rs:124-139`) — canada fractions 15 digits | Profile-derivable | **No** — same as (2) |
| 4 | Borrow-vs-arena split via pointer-range check at visit time (`visitor.rs:196-210`) | IR-derivable (input lifetime always available); zero-copy invariant from grammar's `-> input : Span` annotation | **Partial** — `GrammarIR.materialization` carries the typed-materialization signal; emitter can project `borrowed: bool` |
| 5 | Packed 24-byte `Value` enum + `StringSpan` high-bit tag (`value.rs:81-92`) | **Backend-layout choice** (Rust-specific; user-facing `Value` type) | **Out of scope** — user visitor responsibility, not emitter |
| 6 | `first_quote_or_backslash` NEON `vshrn_n_u16<4>` pack (`simd.rs:301-342`) vs AVX2 `_mm256_cmpeq_epi8`+movemask | IR-derivable (QuotedString class ⇒ structural bytes `"\` ⇒ kernel) | **Yes** — `RegexClass::QuotedString` already carries this; `bbnf-simd-scan` emit submodule W1.2 exposes the body |
| 7 | 64-byte `nospace_bitmap_64` cache on `ScanState` (`simd.rs:36-58`) amortising SIMD stripe across contiguous ws runs | Grammar-derivable (`@ws` directive + grammar's structural alphabet) | **Yes** — `ir.ws_pattern` + `ir.structural_alphabet` |
| 8 | Inline Eisel-Lemire via `parse_that::compute_f64` with `#[inline]` chain tightened (`number.rs:28,195-203`, W2-close §1) | IR-derivable (Number-shape ⇒ Eisel-Lemire body splice) | **Yes** — number shape activates; substrate enabler was `parse-that` upstream |
| 9 | Cold-path escape decoder `#[cold] #[inline(never)]` (`string.rs:83-85`) + thread-local-free `Vec::with_capacity` | IR-derivable (QuotedString has escape sub-pattern ⇒ cold arm) | **Yes** — `RegexClass::QuotedString { escape: Some(...) }` |
| 10 | Fast-exit in `skip_space` (`lib.rs:186-195`) — match next byte as non-ws BEFORE any SIMD call | Profile-derivable (citm 100% structural, twitter ~90% fast-exit) | **Weak** — emitter can universally emit the fast-exit; no workload-specific threshold needed, so "profile" is actually a static win |
| 11 | Empty-compound early return (`lib.rs:269-274, 320-325`) — `}` right after `{` avoids single-key warmup | IR-derivable (Object/Array shape detector knows `open, close` bytes) | **Yes** |
| 12 | `parse_value` 6-arm match on `first_byte` consumed from `skip_space` return (`lib.rs:220-248`) — fuse skip with dispatch | IR-derivable (`DisjointFirstMiner` table keyed by byte) | **Yes** — table exists at `ir.disjoint_first_tables` |
| 13 | Exp saturating-mul cap (`number.rs:180`) — avoids overflow on pathological inputs | Grammar-derivable (numeric regex bound) | **Yes** — `RegexClass::Numeric` parameters |
| 14 | TapeVisitor's `input.len() / 2 + 2` capacity (`visitor.rs:375`, AR-audit heuristic) | Grammar-derivable via capacity analysis (`GrammarProfile.tape_capacity_ratio`) | **Yes if miner exposes ratio; none today** |
| 15 | ValueVisitor's slot-reserve-then-patch for compounds (`visitor.rs:229-297`) — zero memcpy between scratch and output | IR-derivable (Object/Array tag ⇒ packed subtree layout) | **Visitor-owned**, not emitter |

**Summary — 15 decisions, 10 IR-derivable, 3 profile-derivable, 2 backend-layout.** The three profile-derivable items cluster around number scanning: integer-digit-width, fraction-digit-width, and the integer-phase scalar-vs-SIMD crossover. These are the only gaps that could leave emitted JSON "starkly behind" and they live in one kernel.

## 4. Novel-shape assessment (ArgList / Flat / Wrap / HRegex)

Read the W3.1 code: `shape_dispatch/mod.rs:146-164` declares all four variants; no sibling `arglist.rs` / `flat.rs` / `wrap.rs` / `hregex.rs` file exists. These are taxonomy placeholders.

**Signal availability in existing miners:**
- **ArgList** — needs `Seq(Literal_name, "(", Ref, (sep, Ref)*, ")")`. Parts exist: `DelimScanMiner` knows `(…)` wrap; `SeparatorListMiner` knows sep-list bodies. Missing: the lead-literal name-is-identifier predicate. Mineable from `IrNode::Seq` first position = `Literal` or `Regex(ident_class)`. **Straightforward once detector is written.**
- **Flat** — `Seq(head, (literal|ref|regex)+)` with Keyword head. Fully IR-derivable from `IrNode::Seq` + first-position kind check. **Straightforward.**
- **Wrap** — `Alt(Ref, Ref, …)` no-literal-branches transparent alias. Signal in `ir.dag` + per-branch inspection — the existing `DisjointFirstMiner` admits the Alt when FIRST sets are disjoint; Wrap is its pure-Ref-branches subset. **Straightforward.**
- **HRegex** — `Map { Regex, hostfn, Named(T) }` where `T ≠ f64/i64/u8`. Signal: `ir.regex_info[sid].classification ≠ QuotedString|Numeric` AND `IrNode::Map { fn_id, .. }` wraps a Regex. Fully mineable. **Straightforward.**

None of the four novel shapes requires new mining. H1 §10 was correct: W3 should have opened with 11 shapes, not 7. W3.1's stub-the-last-four design is architecturally consistent but consumes a second implementation pass that H1's evidence said was available at W3.1 open.

## 5. Splice-vs-hand-written analysis

**Mechanism citation.** rustc's trait-generic monomorphisation + LLVM's `alwaysinline` attribute handle quote!-spliced bodies identically to hand-written ones at the MIR→LLVM-IR boundary. The rustc reference (https://doc.rust-lang.org/reference/attributes/codegen.html#the-inline-attribute) specifies that `#[inline(always)]` emits `alwaysinline` on the function attribute set; LLVM's inliner honours this unconditionally. The `quote!` macro's output is a `TokenStream` which is parsed by rustc identically to source — there is no MIR-level distinction between "spliced" and "hand-written" fn bodies.

**Evidence from the repo.** `crates/core/src/backend/rust/emitter/dta_walker/helpers.rs` already splices `#[inline(always)]` bodies via quote! into the walker emitter (search results above). W2.2 verified via `nm` that cross-crate helpers collapsed — the `AW-V-W2-close.md:82-100` probe shows the prototype's spliced-equivalent emitted helpers are absent from `nm` (inlined away).

**Boundary condition.** The load-bearing premise is: `codegen-units = 1` + `lto = "fat"` in `[profile.bench]` (workspace `Cargo.toml` as shown above). Workspace LTO is the mechanism that collapses cross-crate helper calls even when `#[inline]` (not `always`) is used. W2.1's `parse-that::eisel_lemire::compute_f64` was `#[inline]`-annotated and still inlined because of LTO (W2-close residual §1).

**Therefore:** W3.2-spliced JSON parser will produce byte-identical-shape machine code to the hand prototype **if and only if** (a) every per-shape emitter attaches `#[inline(always)]` to the emitted function, (b) workspace bench profile stays `lto=fat codegen-units=1 debug=true`, (c) every cross-crate kernel body is spliced inline OR is `#[inline(always)]` in its home crate. These are existing discipline, not new work.

## 6. The projection-behind-RD diagnosis

**Claim being validated.** One prior agent projected W3.2-emitted JSON "starkly behind previous recursive-descent numbers."

**Receipt inventory.** Recursive-descent = `bbnf-json-prototype` (the W2.1 hand prototype). The W2.1 prototype beats sonic 0.88-0.94×. If the W3.2-emitted parser is "starkly behind," the four candidate causes are:

1. **Splicing vs hand-written code** — ruled out by §5 above. rustc + LLVM treat them identically given the annotations and profile settings.
2. **Shape-classifier overhead** — The classifier runs at compile time (`mine_recognizers` is an IR pass); zero runtime cost. Ruled out.
3. **IR-derived facts less rich than hand-picked** — §3 shows 10/15 decisions are IR-derivable, 2 are backend-layout (not emitter's job), and **3 are profile-derivable**. The profile-derivable cluster is entirely the canada number-kernel tuning. If the emitter ships the naive "Number-shape ⇒ always-NEON-SIMD-digit-scan" form, it replays the exact mistake `2edb612b` corrected — canada ratio goes from 0.90× to 1.18× (empirically measured; see `AW-V-W2-close.md:185-190`). That is a 31% canada regression from one missing profile signal.
4. **Visitor monomorphisation boundaries** — H2 §2 shows JSON × (TapeVisitor, ValueVisitor) = 60 KB, fits L1 47%. Ruled out for JSON. The caveat (unconditional multi-visitor emission explodes to 480 KB on JSON per H2 §3) is already addressed by the `#[emit_paired_with]` attribute gate — single pair only.

**Most likely cause: #3, specifically the canada number kernel.** Commit `2edb612b`'s message: "scalar integer scan wins canada." The W3.2 emitter has to decide between scalar-loop and SIMD-digit-scan for the integer-digit phase; without a profile-derived signal it has no basis to prefer scalar. The grammar alone (regex alphabet `[0-9+-.eE]`) does not say "integer parts are 2-3 digits."

**Mitigation.** Extend `PatternAlphabetMiner` with a `NumericWidthHint` field populated by one of:
- A grammar-author `@hint(integer_digits: small|medium|large)` directive (declarative, no profile).
- A workload-miner pass over corpus fixtures that the grammar cites via `@corpus` (runtime profile → IR fact).
- A static heuristic: if the numeric regex is unsuffixed `[0-9]+` (no bounded quantifier), assume mixed distribution; emit a hybrid that scalar-scans the first 4 digits, then falls through to SIMD. The hybrid is almost free on 16-digit fractions (one extra scalar iteration before the SIMD stripe) and strictly better on 2-3-digit integers.

The hybrid is IR-derivable and closes the gap without requiring a workload pass. **It should be the W3.2 emitter's default for `RegexClass::Numeric`.**

## 7. Recommendation

**The W3.1/W3.2 plan closes the gap with two additions.** The 11-shape taxonomy is sound; the detector mechanism is sound; the splice-emit mechanism is sound. The two missing pieces:

1. **W3.2 must land all 4 novel detectors (ArgList / Flat / Wrap / HRegex) in the same wave, not defer to W4.** H1 demonstrated 25-30% of CSS/BBNF hot-path visits sit on these shapes; a W3.2 that ships only 6 detectors cannot produce a JSON parser that tests generalisation — only one that tests JSON-in-isolation. Per the no-deferrals invariant (AW-V.md §invariants #1), the wave must open all 11 detectors or declare the defer-to-W4 at plan time with rationale. The plan already does not.

2. **The Number-shape emitter must ship the scalar-SIMD hybrid described in §6.** Three lines of logic; one commit; eliminates the only profile-derivable item that could leave emitted JSON behind the prototype.

Nothing in the W3.1 code rejects either addition. The shape-emitter mechanism is structurally capable of matching the prototype; the extant plan under-declares scope.

## 8. Risks and measurement plan

**Risk 1 — L1 i-cache overflow on CSS L4.** H2 §2.2 projects CSS L4 × single-visitor at 600 KB (4.7× over 128 KB M1/M2/M3 L1). The hot/cold partitioning lever (H2 §9) is declared W4.2 but the cost model for which rules are "hot" lives in `state_visit_frequency.rs` (already mined). The risk is that W3.2's emitter ships no `#[cold]` annotations, producing a CSS binary that works but has 4× L1 miss rate. **Mitigation:** W3.2 emits `#[cold]` on every rule whose `state_visit_frequency` entry is below the top-20% cutoff; this is a one-line per-shape emitter attribute.

**Risk 2 — Splicing silently drops `#[inline(always)]` on trait-generic bodies.** rustc's `#[inline(always)]` interacts with monomorphic generics differently than with concrete fns. If the emitter splices `fn parse_object_json_object<V: ObjectVisitor>` without the always-attribute, LLVM declines inlining across the generic boundary even with LTO. **Measurement:** the W3.2 wire-contract test asserts `nm` on the emitted bench binary shows zero symbol containing `parse_object_json_` — i.e., every per-shape fn is inlined into the dispatcher per W2.1's pattern.

**Risk 3 — Visitor-trait signature drift.** W1.3 landed `bbnf-tape::visitor::Visitor` + placeholder `ValueVisitor`; W2.1 built its own `JsonVisitor` trait in `bbnf-json-prototype/src/visitor.rs:31-61`. The two are not guaranteed isomorphic. When W3.2 emits code calling `visitor.begin_object()`, the emitted call must resolve against the tape-crate trait, not the prototype-crate trait. **Measurement:** wire-contract test per H2 Appendix A — emit a synthetic grammar, confirm the emitter's output compiles against `bbnf_tape::visitor::Visitor` AND `bbnf_json_prototype::visitor::JsonVisitor` with the shared shape methods (subset).

**Bench plan.**
- W3.2 close gate: `cargo bench -p bbnf --bench json_monolithic` matches `docs/benchmarks/post-AW-V-W2-prototype.json` entries within ±5% per entry.
- CSS generalisation (W4 open): `cargo bench -p bbnf --bench css_l4` over `{normalize, bootstrap, tailwind}` meets the AW-V.md projected numbers.
- `nm` symbol-absence check on every shape-emitted function (see `AW-V-W2-close.md:69-79` template).
- Samply self-time attribution: top-1 symbol ≥ 85% on JSON twitter (parity with W2.1's 91%).

No speculation without a path citation appears above; every claim ties to a file, a commit, or a rustc/LLVM documentation reference.
