# 23 — SOTA Rust JSON Competitor Set, 2025–2026: What a Unified Value API Must Match or Beat

Scope: refresh the Rust-side JSON competitor census past SK-V20's report 20 (dated June 2026, HEAD
`36ca10646`), which already logged simdjson 4.3 (C++), sonic-rs, yyjson, and lightningcss against bbnf.
This report goes one layer deeper on the Rust-native competitor set specifically for the value/API
layer — the axis SK-V20's report 22 found unsettled (three divergent value substrates, four path APIs,
a dead direct-to-struct trait pair). All external claims are web-sourced this pass (July 2026); bbnf
claims are carried forward with file:line from the prior two reports rather than re-derived.

## 1. sonic-rs — the direct competitor for "one Value API, three access modes"

sonic-rs (cloudwego, github.com/cloudwego/sonic-rs) is the sharpest Rust-native precedent for exactly
what bbnf's unified Value API is trying to be: one crate offering lazy field-pick, lazy iteration, AND
eager direct-to-struct, all layered over the same underlying bytes.

- **`LazyValue`** — "a wrapper of a raw valid JSON slice" (docs.rs/sonic-rs). It borrows unparsed JSON
  text and defers parsing until the value is actually converted. This is the closest external analogue
  to bbnf's `ValueRef<'doc,'input,K>` cursor (`skinny/crates/runtime/src/tape/mod.rs:173-178` per
  SK-V20 report 22 §1) — both are "cheap handle over unparsed bytes, parse-on-touch."
- **`get()` / `get_unchecked()` with a `pointer!` macro** — `pointer!["a","b",1]` builds a JSON-pointer
  path; `get` validates as it descends, `get_unchecked` skips UTF-8/structural validation on the
  caller's promise the document is already known-valid. This is a single alphabet (field name / index)
  used for BOTH random-access lookup and the iteration APIs below — one segment vocabulary, not four.
  Contrast: bbnf currently has FOUR path implementations, all independently JSON-pointer-shaped
  (`bbnf-path`, `core/src/path`, `JsonPathQuery`, `bbnf-path-ts` — SK-V20 report 22 §4). sonic-rs proves
  a single alphabet suffices for a real production parser; bbnf's fragmentation is not a technical
  necessity, it is unconsolidated debt.
- **`to_array_iter()` / `to_object_iter_unchecked()`** — lazy forward iterators over array/object
  members; each `next()` parses exactly one member and advances, no materialization of siblings. This
  is the direct external analogue of what bbnf's `general_descent::view` emitter presently CANNOT
  derive for containers (SK-V20 report 22 §3: no `get`/`pairs`/`values` derivation, only scalar
  accessors) — sonic-rs's lazy iterator is table-stakes, not a stretch goal.
- **Direct-to-struct via serde, "no temporary data structures"** — `sonic_rs::from_str::<T>()` parses
  straight into the target struct. Its own claim (docs.rs/sonic-rs architecture notes, cross-checked
  against SK-V20 report 20 §2.2 citing `github.com/cloudwego/sonic-rs performance.md`): no intermediate
  tape at all for this path — a genuinely different substrate from the lazy/DOM paths, not a fold over
  them. bbnf's design intent (report 22 §7 Axis C: "eager = a fold over lazy, never a second
  substrate") is a DELIBERATE DIVERGENCE from sonic-rs's architecture, already flagged as
  Lock-1-mandated and disclosed as an asymmetry in report 20 §2.2 ("NOT-ADOPTED, by deliberate
  architecture"). This pass's web check confirms nothing has changed that claim: sonic-rs ships two
  genuinely separate code paths (parse-to-Value, parse-to-struct), not one substrate wearing two faces.
  If bbnf's forthcoming direct-to-struct bridge (report 22 §7, "one bridge... over the cursor") lands
  as a fold over the SAME tape, that is a real structural win over sonic-rs's dual-path architecture —
  provided the perf cost of the extra indirection is measured, not assumed.
- **SIMD targets, unchanged from report 20's framing**: long-string bodies, float fraction digits,
  field-lookup (via the pointer-path skip), whitespace skip — explicitly NOT a simdjson-style two-stage
  block classify (docs.rs description: "parsing/serializing long JSON strings, parsing the fraction of
  number, getting a specific elem/field, skipping white spaces"). bbnf chose the opposite architecture
  (block-wide NEON classify, report 20 §2.2) and that choice stands unchallenged by anything new found
  this pass.
- **ARM64/NEON status** — sonic-rs ships and benchmarks aarch64 (`docs/benchmark_aarch64.md` referenced
  from its README; the file's live contents were not retrievable in this pass, but its existence
  confirms sonic-rs treats aarch64 as a first-class benchmarked target, not an x86-first afterthought
  with aarch64 falling back to scalar). This matters for the Apple-M precept (charter §"Rust stack
  abrogation," restart precept: aarch64/Apple-M only) — sonic-rs is a legitimate aarch64 comparator
  candidate for the bench harness, not merely an x86 SIMD showcase.

**The concrete bar sonic-rs sets**: one segment alphabet across lookup AND iteration; a lazy value
wrapper that parses on touch; and an explicit, disclosed fork between the lazy/DOM substrate and the
direct-to-struct substrate (it does NOT claim these are unified — it ships them as separate,
purpose-built paths). bbnf's Axis C ambition to unify eager-as-fold-over-lazy is the more ambitious
(and more architecturally honest) claim; it must be benchmarked against sonic-rs's dual-path numbers
specifically on the direct-to-struct case, since that is where a fold-over-cursor could plausibly lose
to a hand-specialized no-tape deserializer on raw throughput even while winning on substrate hygiene.

## 2. yyjson — the C DOM oracle, allocator-model detail

yyjson (ibireme/yyjson, C, MIT) remains what SK-V20 report 20 §2.3 already used it as: a scalar-ILP DOM
baseline that vindicates "tape lever before SIMD lever." This pass adds allocator-model detail relevant
to bbnf's arena work:

- **Immutable vs mutable document split** — yyjson's read path (`yyjson_doc`) is immutable once parsed;
  mutation requires an explicit copy into a separate mutable document type (`yyjson_mut_doc`)
  (deepwiki.com/ibireme/yyjson architecture page; ibireme.github.io/yyjson introduction). This is a
  clean precedent for a design bbnf should consider naming explicitly rather than leaving implicit: the
  lazy `Tape` (read-only, borrowed `&'input`) IS bbnf's immutable-doc analogue; there is no mutable
  counterpart today because bbnf has no JSON-authoring/mutation use case — worth stating as a scoping
  decision rather than a silent gap, since yyjson treats it as a first-class fork in its type system.
- **Three allocator strategies** — default (thin libc wrapper), pool (fixed pre-allocated buffer, zero
  malloc/realloc during parse), and dynamic (on-demand with a freelist) (ibireme.github.io/yyjson doc
  pages; yyjson CHANGELOG). The pool allocator is the closest external analogue to bbnf's bump
  `PayloadArena` (report 22 §1 item 1) — yyjson treats allocator choice as a PUBLIC, user-selectable
  API surface, not an internal implementation detail. bbnf's arena is hard-wired; if a "match or beat
  yyjson" claim is ever made on allocation-heavy corpora, the honest comparison must control for this —
  yyjson's pool mode removes allocator overhead from its numbers entirely on repeat-parse workloads,
  which is a different claim than bbnf's per-parse arena.
- **Contiguous value array, not per-node heap boxing** — unchanged from report 20 §2.3; still the
  correct comparator shape (bbnf's SoA `Tape.offsets: Vec<u32>` is the same family of design). No new
  2025–2026 architectural change found in yyjson's DOM layout this pass.

## 3. serde_json — Eisel-Lemire status is UNCHANGED, and that is itself the news

SK-V20 report 20 §2.6 flagged that bbnf's JSON number leaf routes THROUGH `serde_json::from_str::<Number>`
(`view.rs:232` per that report), making any "beats serde_json" number claim structurally impossible.
This pass's web check confirms the underlying serde_json library has NOT closed this gap:

- **serde-rs/json issue #775** ("Update Moderate Path Float Parsing Algorithms," github.com) is the
  live tracking issue for adopting Eisel-Lemire in serde_json's float path; it remains open. serde_json
  ships its own "extended-float" moderate-path algorithm, not Eisel-Lemire.
- **lexical-core / rust-lexical** already carries a documented, benchmarked Eisel-Lemire implementation
  (`docs.rs/lexical-parse-float`, `Alexhuszagh/rust-lexical` `Algorithm.md`), citing the moderate-path
  speedup as "~66–75% faster than the slow path" over the classic algorithm. This number sharpens SK-V20
  report 20 item 1's ranking (Eisel-Lemire as the single highest-expected-win NOT-ADOPTED item): the
  win is not hypothetical, it is a documented same-family, same-language (Rust) benchmark delta, and the
  crate implementing it (`lexical-core`) is ALREADY a transitive dependency in bbnf's lockfile via the
  `simd-json` comparator (`Cargo.lock:696` per report 20 §2.1) — meaning the exact code that would close
  this gap is already resolved in the dependency tree, just not wired to the runtime's own number
  decode path. This is a near-zero-integration-cost fix relative to its previously-documented ranking.
- No new serde_json release in this window changed the number-parsing default; the gap SK-V20 identified
  is confirmed still open at the ecosystem level, not merely at the bbnf-adoption level.

## 4. simd-json (Rust, simd-lite) — maintained, but with a live disclosed soundness caveat

simd-json (`simd-lite/simd-json`, the Rust port of C++ simdjson) remains actively maintained
(`crates.io/crates/simd-json`; `github.com/simd-lite/simd-json`), tracking simdjson's design (currently
targeting parity with simdjson's 0.2.x-era stage-1/stage-2 architecture per its own README framing) and
auto-selecting AVX2/SSE4.2 on x86 and NEON on aarch64 at runtime. Two items sharpen SK-V20's framing:

- **A live, disclosed soundness issue.** Independent review (ecton.dev, "Surprises in the Rust JSON
  Ecosystem," 2025) flags an "open soundness issue" in simd-json's unsafe code despite its raw
  throughput (that piece measured simd-json's own no-SIMD fallback path at 760+ MB/s on the
  twitter.json corpus, still losing to a pure-safe-Rust competitor's 590 MB/s on the same file only in
  the sense that the comparison is corpus-dependent both ways — see §6 below for the full number).
  This matters for bbnf's comparator hygiene: SK-V20 report 20 already pins `simd-json =0.13.11` as a
  "~2023" stale comparator (§5 item 3); this pass adds that even the LIVE simd-json carries a known
  unsoundness flag, meaning "beat simd-json" claims should not implicitly borrow its credibility as a
  hardened reference oracle — it is a fast comparator, not a correctness oracle. yyjson/serde_json
  remain the correctness anchors; simd-json is a speed comparator only.
- **No architectural change to the stage-1/stage-2 design.** Nothing found this pass supersedes the
  bar SK-V20 report 20 §2.1 already set (simdjson 4.3's NEON string-escape and batch-int kernels as the
  moving target); simd-json (Rust) has not (as of this check) back-ported those specific 4.3-era C++
  kernels.

## 5. facet / miniserde-class reflection — a deliberately NON-competing direct-to-struct axis

facet (`facet-rs/facet`, docs.rs/facet, facet.rs) is the 2025-era successor to the miniserde lineage:
one derive macro producing reflection metadata that a separate crate (facet-json, facet-pretty, etc.)
consumes for serialization, deserialization, diffing, and pretty-printing — "the last proc macro you
should need" (github.com/facet-rs/facet tagline). Its own stated design philosophy
(fasterthanli.me "Introducing facet: Reflection for Rust") is explicit and load-bearing for how bbnf
should frame its OWN direct-to-struct bridge (report 22 §7, "one bridge... over the cursor"):

- **facet explicitly does NOT try to beat serde on speed.** The author states outright that "whatever
  replaces serde with... will not be faster [than monomorphized serde-derive code], but will have other
  characteristics that matter" — reflection buys flexibility, ergonomics, and compile-time wins at a
  runtime cost versus "monomorphize all the things."
- **This is the honest frame bbnf's dead `ser::Deserializer` trait (SK-V20 report 22 §5) must adopt or
  reject explicitly, not drift into by accident.** bbnf's forthcoming grammar-derived direct-to-struct
  bridge is closer to serde-derive's monomorphized-codegen model (a derive walks the CURSOR and emits
  concrete field assignments per grammar-derived layout, per report 22 §7) than to facet's runtime
  reflection model — which is the RIGHT choice for a "beat SOTA on speed" mandate, but it means facet is
  NOT the comparator to benchmark against for speed; facet is the comparator to cite for why bbnf should
  NOT adopt runtime reflection as its dispatch mechanism. miniserde itself (the facet author's own
  comparison point) was found to have compatibility gaps with real-world derive-heavy crates (uuid,
  chrono) when tested — an ecosystem-maturity risk, not a performance one, but worth naming if bbnf's
  bridge is ever pitched as "usable like serde" rather than "fast like serde-derive."

## 6. JustJson / ecton.dev — the sharpest EXTERNAL precedent for "lazy string, deferred escape decode"

The most load-bearing find this pass is not a headline library but a methodology-and-design essay:
Jonathan Johnson's "Surprises in the Rust JSON Ecosystem" (ecton.dev/rust-json-ecosystem/, 2025),
which builds and benchmarks a new parser, JustJson, specifically to test one idea: **can JSON strings
and floats be lazily decoded, deferring escape-processing until the value is actually used, rather than
decoding escapes eagerly at parse time as every existing Rust DOM crate does?**

- **The core finding: "every existing Rust crate that parses the JSON DOM will decode the escapes at
  the time of parsing"** — forcing an allocation the instant an escape sequence appears, regardless of
  whether the caller ever needs the decoded string. JustJson instead stores a reference to the original
  source plus escape metadata, decoding on demand — "if escapes are present, they are decoded on the fly
  as part of the string comparison operation." This is the SAME shape as bbnf's own `HasEsc`
  flag-plane design (SK-V20 report 20 §2.1: `view.rs:206-216`, "borrow non-escaped span... only then
  unescape_string" — already flagged there as matching simdjson's discipline). JustJson is independent
  confirmation from a DIFFERENT author, in the SAME language, in the SAME year-window, that this exact
  design (lazy-escape, borrow-when-clean) is the correct SOTA move — not a bbnf-specific invention nor
  an overfit. It strengthens, rather than merely repeats, the prior finding.
- **A named anti-pattern for bbnf's own bench honesty discipline (precept: cold-only, loadavg-quiet,
  single-invocation).** The essay names "JustJson is essentially cheating at this benchmark" for its
  OWN stringify path: because JSON strings/numbers are kept in original textual form, re-serializing a
  parsed `Value` back to JSON degenerates to a series of `memcpy`s, making its own round-trip numbers
  look better than a parser that actually re-encodes. This is the exact same trap bbnf's own honesty
  ledger polices for CSS (SK-V20 report 20 §5: "depth asymmetry... shallower materialization" must stay
  disclosed) — an author independently naming their own methodology's cheat is the standard bbnf's
  reports should hold themselves to, and is direct supporting precedent for continuing that discipline
  rather than relaxing it.
- **Corpus insufficiency, named directly.** The essay uses only three corpora (canada.json — coordinate
  -heavy floats; citm_catalog.json; twitter.json — string-heavy) and explicitly flags that this is not
  representative: "JSON payloads vary greatly." The measured numbers show the SAME two parsers trading
  places depending on corpus shape (JustJson 460 MB/s vs simd-json-no-SIMD 310 MB/s on canada.json,
  numeric-heavy; but simd-json 760 MB/s vs JustJson 590 MB/s on twitter.json, string-heavy) — a direct,
  reproducible demonstration that ANY single-number "X beats Y" claim is corpus-dependent, and a
  concrete argument for why bbnf's own bench harness must report per-corpus-shape numbers (numeric-
  dense vs string-dense vs nesting-dense), not one aggregate throughput figure, when making a >SOTA
  claim.
- **Library quality issues surfaced by rigorous testing, worth noting as a bar for bbnf's own
  correctness discipline**: the `json` crate has an unfixed 2021 UB report that Miri still flags; the
  `json-deserializer` crate failed conformance against the json.org test suite under testing. These are
  reminders that "fast" and "correct" are measured independently, and a >SOTA speed claim carries no
  correctness credit by association — bbnf's own conformance suite (whatever oracle it uses per the
  csp-solver/checkasm-differential discipline named in report 20 §2.3) must be run and reported
  alongside any speed number, not implied by it.

## 7. The bench-methodology synthesis: what an honest cross-parser harness looks like

Drawing the threads from ecton.dev's essay together with SK-V20's own honesty ledger (report 20 §5),
an honest harness for bbnf's JSON claims needs, concretely:

1. **Multiple corpora spanning shape, not size** — at minimum a numeric-dense corpus (canada.json-class:
   long float arrays), a string-dense corpus (twitter.json-class: many short escaped strings), and a
   deeply-nested/object-heavy corpus (citm_catalog.json-class). A single-number aggregate throughput
   claim is, per the JustJson data above, actively misleading — the SAME two parsers swap rank order
   across these three shapes.
2. **Separate numbers for DOM-materialize vs lazy-cursor-touch vs direct-to-struct**, not one blended
   "parse" number — because these are different amounts of work (a lesson sonic-rs's own architecture
   makes structural: it ships three distinct entry points precisely because the work differs). bbnf's
   own Value API design (report 22 §7 Axis C) makes the strong claim that eager is a fold over lazy; the
   bench harness must therefore report the LAZY number AND the fold's marginal cost separately, so the
   "fold over lazy" claim is falsifiable rather than asserted.
3. **Escape-density as an explicit axis**, not incidental — given both bbnf's own `HasEsc` design and
   JustJson's independent convergence on the same idea, the harness must include corpora with escape
   density as a controlled variable (0% escapes, sparse escapes, dense escapes) since this is precisely
   the axis where the lazy-borrow architecture's advantage is largest and where "beats serde_json"
   claims are easiest to overstate on the wrong corpus.
4. **Never let re-serialization (stringify) reuse the same textual-fidelity trick without disclosing
   it** — bbnf's own honesty ledger (report 20 §5, item 2: CSS typed values "classified, not decoded")
   is the same category of disclosure JustJson's author volunteers for their own stringify path; the
   SK-V21 synthesis should treat "materialization depth" and "re-encode fidelity" as two more axes on
   the disclosure checklist already opened for CSS, now generalized to JSON.
5. **Correctness gating is a precondition, not a footnote** — the `json` crate's live UB and
   `json-deserializer`'s conformance failures (found under adversarial testing, not by the authors'
   own claims) show that speed-only comparator sets silently admit broken parsers. Any comparator added
   to bbnf's bench harness should be run through the SAME conformance oracle bbnf holds itself to before
   its numbers are cited.

## 8. What the unified Value API must concretely match or beat

Synthesizing this pass against SK-V20 report 22's design (Axis A: one tape; Axis B: derived navigation;
Axis C: eager-as-fold-over-lazy; one path alphabet; one direct-to-struct bridge):

- **Against sonic-rs**: match its one-alphabet path API (`pointer!` used identically for `get` and
  iteration) — bbnf's report 22 §7 "path/query" design already targets this; this pass confirms it is
  externally validated, not aspirational. BEAT it on substrate count: sonic-rs ships lazy/DOM and
  direct-to-struct as two genuinely separate paths; bbnf's fold-over-lazy design, if it lands, is a
  strictly stronger architectural claim — but it must be BENCHMARKED against sonic-rs's dual-path
  numbers on direct-to-struct specifically, since a fold-over-cursor has a plausible speed tax a
  hand-specialized no-tape deserializer does not.
- **Against yyjson**: match its allocator-strategy honesty (name the arena choice as a public, disclosed
  decision, per its pool/dynamic/default three-way split) even if bbnf ships only one arena strategy;
  do not silently let "beats yyjson" comparisons cross allocator-strategy lines without saying so.
- **Against serde_json**: the number-decode fix (Eisel-Lemire via the already-vendored `lexical-core`)
  remains the single highest-leverage, lowest-integration-cost item from SK-V20's ranked list — this
  pass adds that the same fix is independently documented as a 66–75% moderate-path win in Rust,
  same-language, same-era, sharpening its priority further.
- **Against simd-json**: continue treating it as a speed comparator only, not a correctness oracle —
  its disclosed soundness issue (ecton.dev, 2025) means bbnf's own conformance discipline must not
  borrow simd-json's reputation as evidence of bbnf's own correctness.
- **Against facet/miniserde**: explicitly reject runtime reflection as the direct-to-struct dispatch
  mechanism (facet's own stated tradeoff — flexibility over speed) in favor of the compile-time
  grammar-derived codegen bridge report 22 §7 already specifies; cite facet only to justify NOT taking
  its approach, never as a speed comparator.
- **Against JustJson**: the lazy-escape-borrow design is independently convergent evidence bbnf's
  `HasEsc` discipline (report 20 §2.1) is the correct SOTA shape, not a local invention — treat this as
  confirmation, and adopt JustJson's self-disclosed stringify caveat as a template for how bbnf must
  keep disclosing its own materialization-depth asymmetries (report 20 §5) as the Value API unifies.

No new finding this pass overturns SK-V20's report 20 NOT-ADOPTED ranking or report 22's three-substrate
diagnosis; this pass sharpens both with fresher, cross-validating external sourcing and surfaces one
genuinely new, actionable precedent (JustJson) that did not appear in the SK-V20 corpus.

## Sources (web-verified, July 2026)

- sonic-rs: github.com/cloudwego/sonic-rs; docs.rs/sonic-rs/latest/sonic_rs/;
  github.com/cloudwego/sonic-rs/blob/main/docs/benchmark_aarch64.md (existence confirmed, contents not
  retrieved this pass).
- yyjson: github.com/ibireme/yyjson; ibireme.github.io/yyjson (introduction + doc pages);
  deepwiki.com/ibireme/yyjson/2-architecture-and-design; ibireme.github.io/yyjson/doc/doxygen/html/
  md__c_h_a_n_g_e_l_o_g.html.
- serde_json / Eisel-Lemire: github.com/serde-rs/json issue #775 ("Update Moderate Path Float Parsing
  Algorithms"); docs.rs/lexical-parse-float; github.com/Alexhuszagh/rust-lexical
  lexical-parse-float/docs/Algorithm.md.
- simd-json: github.com/simd-lite/simd-json; crates.io/crates/simd-json;
  shnatsel.medium.com "The state of SIMD in Rust in 2025" (std::simd nightly-only status, NEON-mandatory
  aarch64 framing).
- facet: github.com/facet-rs/facet; facet.rs; docs.rs/facet;
  fasterthanli.me/articles/introducing-facet-reflection-for-rust.
- JustJson / bench methodology: ecton.dev/rust-json-ecosystem/ (Jonathan Johnson, "Surprises in the
  Rust JSON Ecosystem," 2025).
- Cross-reference (bbnf-internal, carried forward not re-derived): SK-V20
  `restart/skinny/tranches/sk-v20/research/20-sota-gap-refresh.md` §§2.1-2.6, §5; SK-V20
  `restart/skinny/tranches/sk-v20/research/22-value-api-design.md` §§1, 3, 4, 5, 7.
