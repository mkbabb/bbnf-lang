# αC — REDRESS Digest: Pre-Blocks the SK-V17 Tape Work Must NOT Re-Open (cycle V3)

Agent: alphaC (Pass Alpha SK-V16→SK-V17, cycle V3).
Host: aarch64 Apple M5 Max only. RUSTFLAGS=`-C target-cpu=native`, cold (warmup_iters=0).
HEAD of record: `1c5bd7a25` (canonical cold bench). Architecture-synthesis HEAD lineage: AU `3b8b757d`,
post-AZ-IV `cb14970f`, restart begin `b5eb4651c`/`a5145a0bb`, SK-V16 W6 `f2fe49bbc`, SK-V15 close `8bada626a`/`66232b7c3`.

## §0 — What this digest does, and the tree-disambiguation header (load-bearing)

SK-V17's subject is: CSS L4 typed parsing must BEAT lightningcss (full-CSSOM comparator), via a
**unified tape / layout / projection model generalized across ALL grammars** + dav1d-style aarch64
NEON hot leaves. The danger is that the tape/lazy-view migration re-opens a route that a prior cycle
already measured into the ground. This digest walks each named pre-block, classifies it, and states the
**re-open test**: the exact condition under which the SK-V17 tape work would be re-committing the
rejected route, versus the **different-framing admission**: the exact shape under which the same
construct is legitimately admitted by the tape+lazy-view model.

**TREE-DISAMBIGUATION (αE §0:37-51, grep-verified at HEAD `1c5bd7a25`, this digest's binding correction).**
The architecture-synthesis doc (`sk-v16-css-sota-tape-architecture.md`) cites `crates/core/src/runtime/...`,
`bbnf_ir::registry::struct.rs StructLayout`, `OpenFrame`, `CssArena`, `css_l4/builder.rs:274`,
`TapeStructBuilder` — **these are the TOTALITY tree, NOT the benched skinny tree.** Grep over `skinny/crates/`
returns **zero** for `StructLayout`, `OpenFrame`, `CssArena`, `begin_compound`, `TapeStructBuilder` (verified
this cycle). The CHALLENGE pass (CH1) rejects any goalset that cites a core-tree path as the benched surface,
because a deletion/wiring gate keyed to `crates/core/...` can be satisfied in the un-benched tree while the
benched CSS path is untouched. **Every pre-block below is keyed to the skinny benched surface:**

| Concept | Core-tree (doc) symbol — DO NOT cite as benched | Skinny benched surface — the real target |
|---|---|---|
| Layout description | `StructLayout{rule_id,kind,fields}`, `bbnf_ir::registry::struct.rs` | `BackendRule` + `LayoutFacts.backend_shape ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` (`skinny/crates/ir/src/cost.rs:119-121,259-271`); lowering in `skinny/crates/codegen/src/lower/{tape_plan,offset_tape,event_tape,eager_tape}.rs`. **Lock 2 (LOCKS.md:160) RETIRED the term `StructLayout`; canonical name is `Layout` / `LayoutFacts`.** |
| Eager builder | `OpenFrame` enum, `css_l4/builder.rs:274` ~40-arm `match rule_id`, `CssArena` `Vec<Vec<T>>`/`Box<CssColor>` | does not exist in skinny; the eager-arena/Box pathology lives only at `crates/core/.../css_l4/arena.rs` (TOTALITY). Skinny's eager pathology is the **fact-stream String** (see §3). |
| Tape substrate | `crates/core/src/runtime/tape/{record,arena,cursor}.rs`, `TapeStructBuilder` | `skinny/crates/runtime/src/tape/` = `assembler.rs` (`TapeBuilder` :42), `mod.rs` (`Tape` :94, `ValueRef` :175, `PayloadArena` :38), `event_grammar.rs`, `offsets.rs`. **No `record`/`arena`/`cursor` siblings as the doc names them.** |
| CSS fact-stream routing | (doc treats fact-stream as a per-grammar branch) | `RuntimeEmitterKind::RequestFacts` registered for all 7 CSS grammars in `skinny/xtask/src/regen_css.rs:45,63,81,99,117,135,153` (`*_fact_stream` output planes), driven by the hand-coded CSS profile array `W5C_REQUEST_FACT_PROFILES` (`skinny/crates/codegen/src/lib.rs:336`). |
| Typed-value generator gate | `RuntimeEmitterKind {CompiledLowering, RequestFacts}` (doc) | same two variants, verified `grammar_provider.rs:40-42`; SK-V17 must extend/route this in skinny, not core. |
| Projection catalogue | `xtask/runtime-projections/css_l4.toml` 594 lines (doc §3) | **does NOT exist in skinny** — no `runtime-projections/` dir, no `css_l4.toml` in `skinny/`. It is a core-tree artefact. Skinny's per-grammar overfit fingerprint is `W5C_REQUEST_FACT_PROFILES` + the 7 `RequestFacts` registrations + the 148 fixture parse fns (see §5). |

Two-bucket classification:

- **PERMANENT PRE-BLOCK** — the route is intrinsically refuted; no framing recovers it. Re-opening is a
  CH3 regression failure outright.
- **ADMIT-UNDER-DIFFERENT-FRAMING** — the *intent* (typed, rich, retained) is correct; only the prior
  *implementation* (eager / allocating / fragmented / serialized) was refuted. The tape+lazy-view IS the
  different framing. The construct admits only when re-cast onto the lazy-offset-tape + layout-driven
  projection substrate, never onto the refuted carrier.

All re-open tests below **grep the skinny benched tree.** A gate that greps `crates/core/...` is unverifiable
on the measured surface and is itself a CH1 defect.

---

## §1 — AZ-IV eager-value-tree materialization (118x)

**Source of record:** `restart/audit/skinny-impl-overfit/sk-v16-css-sota-tape-architecture.md:46-66`
(citing `docs/benchmarks/post-AZ-IV.json` floors block, commit `cb14970f`, 2026-05-02).

**What was measured:** AZ-IV W5 made `json_monolithic` parse *into* a value tree by default —
canada 1.83ms → 215.7ms = **118x** regression; the post-AZ-IV.json file itself states the same input on
the tape-only path would match the AU floor (`3b8b757d`, 2026-04-15). Root cause = **eager per-leaf
payload materialization** (an f64 alloc per number, a typed value built at parse time).

**Timeline correction (load-bearing, sk-v16-arch:21-26):** the 118x is *intra-A-series self-regression*
against the post-AU floor, NOT a regression the docs→code restart caused. The restart inherited it and
then deleted the cause. So the lesson is not "the tape regressed"; it is "AZ-IV bolted an eager value
substrate onto the tape and that eager substrate is what regressed 118x."

**Classification: ADMIT-UNDER-DIFFERENT-FRAMING.**

The *intent* — produce a typed, rich value — is exactly SK-V17's goalset (typed CSSOM, preserve-rich-ast,
cssparser-parity 8-field equality). What was refuted is **eager-by-default materialization at parse
time**. The tape+lazy-view is the different framing: skinny JSON already proves it — `value_from_ref`
(`skinny/crates/runtime/src/grammars/json/value.rs:143`) reads *one* byte at the offset and wraps a
`Copy ValueRef{tape,cursor}` (`skinny/crates/runtime/src/tape/mod.rs:175`) with **zero per-node heap alloc,
zero typed value at parse time** (sk-v16-arch:79-82). This is the simdjson/sonic-rs on-demand model and is
*why JSON is >SOTA*.

- **Re-open test (CH3 fail if true):** SK-V17 produces a typed CSS value *at parse time*, per leaf,
  eagerly, before a view accessor is called — i.e. any `f64`/typed-node heap allocation on the per-leaf
  hot path that is not a re-readable source span. The tripwire is anchored to the **construct** (per-leaf
  typed-node / `f64` heap allocation at parse time), **not** to a fixed symbol list: the typed names a CSS
  value substrate would use (`CssTypedValue`/`CssColor`/`CssDimension`, …) are **prospective/illustrative
  only** — grep-clean-absent from `skinny/crates/` at HEAD `1c5bd7a25` (verified this cycle), so the gate
  must NOT key on them as extant symbols. Grep surface for the construct: any per-leaf typed/`f64`/`Box`
  allocation under `skinny/crates/runtime/src/grammars/css_l4_*/` + the benched `track1` fns in
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596-624`.
- **Different-framing admission (CH3 pass):** the typed CSSOM is produced by **lazy view projection** over
  the sealed tape — structure reconstructed on demand from offsets + the rule's `LayoutFacts`/`BackendRule`
  shape (skinny equivalent of the doc's `StructLayout`, see §2b), scalars re-read from the source offset,
  and `PayloadArena` (`tape/mod.rs:38`) used *only* for decoded scalars that cannot be re-read from source
  (f64 bits, u32 hex; sk-v16-arch:136). The skinny runtime test path already asserts **zero payload bytes,
  zero writes, zero allocations** for JSON parse/projection (REDRESS item 6, `skinny/REDRESS.md`); CSS must
  hold the same invariant.
- **Telemetry binding (SK-V17 must carry):** per-corpus payload-arena write/alloc counters published in
  RESULTS (REDRESS item 8) — the gate must fail any CSS row whose per-leaf payload counters are non-zero
  except for the named irreducible-decode set (f64/hex). This is the falsifiable proof the tape did not
  silently re-eagerize.

---

## §2 — StructRegistry / Arena<G> / Builder<G> hot-path indirection (28-65x; 983x; 10583x WATCHDOG)

**Source of record:** sk-v16-arch:46-66; AZ-IV W5 reintroduced `StructRegistry` + `Arena<G>`/`Builder<G>`
registry indirection in the hot path — **28-65x** on bbnf/sheets, **983x** on css bootstrap (606.4ms),
**10583x WATCHDOG** on tailwind (77.6s). The irony recorded: AU.4.2 had *deleted* StructRegistry; AZ-IV W5
bolted it back (sk-v16-arch:52-53).

**Why it is its own pre-block, distinct from §1:** §1 is the *value substrate* (eager typed nodes); §2 is
the *control-flow indirection* (a registry lookup in the per-leaf hot path + per-compound `Vec` arena
machinery). They co-occurred at AZ-IV but are independent faults. **Tree note:** the explicit arena pathology
the arch doc names (`css_l4/arena.rs:60` `Vec<Vec<T>>` + `Vec<Box<CssColor>>`, `end_compound` `split_off`,
OpenFrame-stack clone checkpoint, sk-v16-arch:91-95) lives **only in the core tree** — grep-clean-absent from
skinny. So in skinny this pre-block guards against *re-introducing* that machinery, not against deleting an
extant skinny construct.

**Classification: SPLIT — one PERMANENT PRE-BLOCK + one ADMIT-UNDER-DIFFERENT-FRAMING.**

- **PERMANENT PRE-BLOCK (the indirection — §2a):** a `StructRegistry`/`Arena<G>`/`Builder<G>` *registry lookup
  in the per-leaf hot path* is permanently refuted. There is no framing under which a hashmap/registry
  dereference per leaf admits. The arch synthesis verdict is explicit: "Recover the A-series *uniform
  flat-tape + lazy view*, do **not** recover StructRegistry/Arena<G>/Builder<G>" (sk-v16-arch:64-66).
  `StructBuilder`/`OpenFrame` "appear nowhere in skinny/" (sk-v16-arch:58; grep-confirmed this cycle);
  reintroducing them into the skinny benched tree is a Lock 1 parallel-substrate violation (LOCKS.md:75, the
  `Vec<OpenFrame>::clone` 86.07% samply pathology is the canonical example).

- **ADMIT-UNDER-DIFFERENT-FRAMING (the layout itself — §2b): re-keyed to the skinny tree per αE §0:37-51.**
  The doc's generality vehicle is `StructLayout{rule_id, kind: LayoutKind, fields: Vec<StructField>}` with
  `FieldSource` per field. **That symbol does not exist in skinny, and Lock 2 (LOCKS.md:160) RETIRED the
  name `StructLayout` ("the IR record is `Layout`").** The skinny benched equivalent is:
  - the per-rule `BackendRule` shape + `LayoutFacts.backend_shape ∈ {EagerTape, OffsetTape, EventTape,
    SinkOnly, CollapsedStage}` (`skinny/crates/ir/src/cost.rs:119-121,234,259-271`), computed **once per rule**
    at codegen lowering, not per leaf;
  - the lowering that consumes it: `skinny/crates/codegen/src/lower/{tape_plan,offset_tape,event_tape}.rs`
    (`TapeFlavor`/`render_rule`/`TapeEmit`/`SpanMark`), the skinny analog of the doc's "layout-driven projection";
  - the CSS generator-provenance surface `skinny/xtask/src/regen_css.rs` (the 7 grammar registrations the
    SK-V17 flip must move off `RuntimeEmitterKind::RequestFacts` onto a tape-emitting kind).

  The layout description is **not** the refuted construct: it is built once per rule (compile-time) and is the
  load-bearing generality vehicle — the unified projection reads the rule shape to resolve each field's tape
  slot (the doc's `SeqPosition`/`BranchTag`/`TypedLeaf`/`RuleReference`, sk-v16-arch:144-149; skinny realises
  these as tape-plan emit ops + cursor arithmetic over `Tape`/`ValueRef`). The fault was that the perf-critical
  *builders re-hardcode shape* (the core-tree `match rule_id` at `css_l4/builder.rs:274`; in skinny the
  equivalent overfit is the hand-coded `W5C_REQUEST_FACT_PROFILES` array + 7 `RequestFacts` registrations).
  Admission requires the emitter to *derive* tape ops from the rule's `BackendRule`/`LayoutFacts` shape once,
  not hardcode a per-grammar profile table or look up a registry per leaf.

- **Re-open test (CH3 fail if true), skinny-keyed:** any per-leaf or per-compound registry/hashmap dereference
  re-introduced anywhere under `skinny/crates/runtime/src/`; any per-compound `Vec` heap allocation
  (`split_off`, `Vec<Vec<T>>` slab, `Box::new` per value) on the CSS parse path; any checkpoint that clones a
  frame stack; any new hand-coded per-grammar profile/route table parallel to `W5C_REQUEST_FACT_PROFILES`
  (relocated overfit — Lock 14, LOCKS.md:380-387).
- **Different-framing admission (CH3 pass):** the tape-emitter pushes Open/Close/Leaf records into `TapeBuilder`
  (`assembler.rs:42`, `push_plain_offset` = one branchless `u32` write); children are the contiguous run
  between Open/Close recovered by cursor arithmetic over `Tape`/`ValueRef` — **no split_off, no per-compound
  Vec alloc** (sk-v16-arch:131-134). checkpoint/rollback collapse to a single `offsets.len()` marker +
  truncate — **O(1), no stack clone** (the generic O(1)-checkpoint banked at SK-V16, 20x sound).
- **Telemetry binding:** the SK-V17 falsifiability gate names canada/bootstrap/tailwind explicitly and
  requires the *no-WATCHDOG* property — tailwind must complete within a bounded cold time (the AZ-IV
  77.6s is the disqualifier). The samply attribution row must show **zero** registry-lookup / split_off /
  frame-clone self-time on the CSS hot leaf.

---

## §3 — CSS fact-stream String serialization

**Source of record:** sk-v16-arch:83-86, :187-198; V1 CONSOLIDATED-AUDIT:58, :86-88; SPEC #3/#9.

**What it is (skinny benched):** skinny CSS emits a **tab-delimited fact-stream `String`** (`emit_fact_stream` /
`emit_full_parse`, `css_l4_declaration_values/generated.rs:5,61` and the 7 CSS `generated.rs` siblings): dozens
of `push_str`/`to_string` + fnv64 + schema/policy/witness headers. The benched "track1" is
`Result<String,String>` (`track1_facts`, `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596`), **not a typed
value**. The ~63.9 Mbps figure measures *string serialization*, not typed CSSOM construction. ~34% of CSS
self-time is `emit_*` String building (sk-v16-arch:256-259).

**The live routing that produces it (the construct SK-V17 must retire), skinny-keyed:** the fact-stream is not
an accident of one generated file — it is *registered* for every CSS grammar via
`RuntimeEmitterKind::RequestFacts` in `skinny/xtask/src/regen_css.rs:45,63,81,99,117,135,153` (7 registrations,
each with a `*_fact_stream` output plane), and the emission shape is enumerated in the hand-coded CSS profile
array **`W5C_REQUEST_FACT_PROFILES`** (`skinny/crates/codegen/src/lib.rs:336`, iterated at `:567,:611`). That
array is itself a Lock 14 phrase-#1 hand-curated-per-grammar construct. **A SK-V17 wave that flips the benched
Mbps to a typed value but leaves `W5C_REQUEST_FACT_PROFILES` + the 7 `RequestFacts` registrations standing has
not retired the fact-stream — it has built a second substrate alongside it (Lock 1).**

**Classification: PERMANENT PRE-BLOCK as an *admission/output plane*; ADMIT only as DIAGNOSTIC-ONLY.**

The fact-stream is spec-illegal for admission **by design** (sk-v16-arch:215-221, SPEC close-condition
#3/#9: "fact streams, full-parse summaries, brace counters, FNV are DIAGNOSTIC ONLY and must NOT admit
CSS"). Lock 1's fact-stream clause (LOCKS.md:585) admits `FactStream` only as an output-plane/admitted-product
category *with typed schema/provenance and gate-consumed telemetry*; **string-only fact streams are
rejected** unless a later G-Omega amends Lock 1. The CSS close requires "generated typed value/document/
view/visitor output and same-workload `cssparser` equality before `lightningcss` CSSOM/value pressure can
admit" (Lock 8, LOCKS.md:595).

- **Re-open test (CH3/CH5 fail if true):** SK-V17 benches CSS Track 1 as a `String` fact-stream and
  reports its Mbps as the typed result; OR the tape's view-time projection emits a serialized String into
  the hot path; OR any `push_str`/`to_string`/fnv64 on the per-parse CSS hot path. This would also be a
  Track1==Track2 dishonesty / sidecar violation (Lock 1, CH5) if the String is the retained product.
- **Re-open test — retirement clause (CH3/CH5 fail if NOT done):** the 7 CSS grammars STILL register
  `RuntimeEmitterKind::RequestFacts` in `skinny/xtask/src/regen_css.rs`, OR the hand-coded
  `W5C_REQUEST_FACT_PROFILES` array (`codegen/src/lib.rs:336`) still drives a benched/admitted CSS row.
  Both must be retired (moved to diagnostic-only behind a forbidden-token scan) when the typed CSS row lands;
  a typed Mbps with the RequestFacts route still admitting is a CH5 parallel-substrate failure.
- **Different-framing admission (CH3 pass):** the fact-stream stays a **diagnostic-only** artifact behind
  a forbidden-token scan; the benched/admitted Track 1 is the **typed CSSOM produced by lazy view
  projection over the tape** (sk-v16-arch:227-231, the §2 model). The String serialization cost
  (the ~34% `emit_*`) is *deleted from the hot path*, replaced by tape append (one branchless `u32` write
  per structural token), exactly as JSON's `push_plain_offset` (sk-v16-arch:80-81). The honest Mbps the
  W6 path already measured for the typed CSSOM is ~3.09 Mbps (αE anchor :17, `css_l4_w6_typed_retime`) — the
  SK-V17 number must be the typed value, not the String summary's old ~2331 Mbps (W6 report:84-87, explicitly
  refuted: "the summary path's old 2.46x margin does not transfer to the typed lane").
- **Telemetry binding:** RESULTS Output-plane column must read `typed direct` / `borrowed view`, never
  `digest`/`FactStream`, for any admitted CSS row; the gate rejects a CSS admit whose output plane is a
  String summary. The W6 8-field structural equality (rules=10136/style=9561/sel=9561/decls=20043,
  banked at `1c5bd7a25` / W6 report:102) is the parity proof that the typed value carries the rich shape.

---

## §4 — The 24-row broadcast measurement

**Source of record:** V1 CONSOLIDATED-AUDIT:19-29, :69; V2 AGENT-5:10-13; SK-V15 close
(`8bada626a`), W1 made the 24 W8R CSS rows diagnostic/non-admitted (V2-CONSOLIDATED:28).

**What it was:** all 24 CSS L4 "row admits" were **one measurement broadcast 24 times** —
`skv14-redress-215-css-full-parse-profile.tsv` lines 2-25 show identical
`track1=2319.041, lightningcss=929.281, cssparser=2362.037` repeated for 24 conceptual feature row-ids;
`css_l4_w8.rs:206-228 measure_full_parse_profiles` runs ONE aggregate loop; `W8_SELECTED_CSS_ROWS = 24`
is a hardcoded broadcast constant; `SKV13_CSS_FEATURES` projects the one number across 24 row-ids
(V1-AUDIT:23-26). It was further workload-mismatched: a brace-counter `CssFullParseSummary` vs
lightningcss's full CSSOM, and cssparser actually beat Track 1 by ~43 Mbps in the same row (V1-AUDIT:29).

**These 24 falsified rows are still present in `skinny/RESULTS.md`** (lines 112-135, grep-verified at HEAD
`1c5bd7a25`: `grep -c 'css_l4/' skinny/RESULTS.md` = 25 substring matches, of which 24 are `^| css_l4/`
table rows (112-135) and the 25th (:154) is a prose REDRESS-127 companion reference, not a row; there is
NO admitted/distinct W6 typed CSS row in RESULTS.md (`grep 'W6.*css|tape.*direct_to_struct'` = EMPTY);
the `2319.041` broadcast tuple appears on exactly 24 lines) as `css_l4/*/direct_to_struct/main` W8R
broadcast diagnostics (not_admitted / AUDIT-FALSIFIED, identical tuple
2319.041/2362.037/929.281 across all 24). They are diagnostic-only and carry zero admission weight; SK-V17
must not lift any of them as a measured close. (Basis note: this is the single 24-row broadcast — one
measured tuple projected across 24 conceptual feature row-ids — NOT six. Any sibling artefact citing "6
css_l4/*/direct_to_struct/main rows" undercounts; the grep-verified count is 24, range 112-135.)

**Classification: PERMANENT PRE-BLOCK.**

This is a measurement-honesty fault, not an architecture choice — no framing recovers a single number
broadcast across N conceptual rows. It is codified into a CHALLENGE rule
(NEW-CH5-V5-02 broadcast-admission detection, V1-AUDIT:149) and into Lock 8 (LOCKS.md:595): "Repeated
throughput tuples across conceptual row IDs are non-admit unless each row has independent command/input/
equality/timing." SK-V15 W1 already demoted the 24 rows to diagnostic/non-admitted (V2-CONSOLIDATED:28).

- **Re-open test (CH1/CH5 fail if true):** SK-V17 publishes N CSS rows (per-feature, per-corpus) that
  carry the same Mbps tuple; OR a single aggregate loop times the combined corpus and the result is
  projected across multiple `measurement_row_id`s; OR rows lack independent `command/input/equality/timing`
  + `broadcast_group_id`.
- **There is no different-framing admission.** The only legitimate path is: partition the corpus and time
  **each corpus independently** (V1-AUDIT:102), each row carrying a distinct measured number with its own
  `measurement_row_id` and `broadcast_group_id` (Lock 8). This is not a re-framing of the broadcast; it is
  its replacement.
- **Telemetry binding:** the canonical bench is N≥50 cold samples + **median** (CONTEXT ground truth: the
  W6 harness `W6_SAMPLE_COUNT=1` single-sample is statistically inadequate). The CSS corpus is the
  SK-V14-benched set — **animate / bootstrap / tailwindcss / material-components-web** (`css_l4_corpus.rs:22-54`),
  each its own row, never aggregated-then-broadcast. (Note: `normalize` is NOT in the benched corpus set; any
  close condition keyed to `normalize` is unmeasurable — use `animate` as the regular-corpus stand-in unless
  `normalize.css` is added + re-baselined.)

---

## §5 — FNV / fixture contrivances

**Source of record:** V1 CONSOLIDATED-AUDIT:60-65, :139-142; V2 AGENT-5:5-8; SK-V15 W10 FNV quarantine
(close evidence `8bada626a`, V2-CONSOLIDATED:43-44); sk-v16-arch:206-209.

**Two distinct contrivances:**

1. **FNV closed-enum arbiter.** W11L y_string_unicode admit hashed the decoded string with FNV-64 and
   matched a closed 11-entry table; sonic-rs and serde sidecars deserialized into the *same closed enum*,
   so the strict-product comparator could not catch hash collisions (V1-AUDIT:62). Same pattern in W11N
   (unicode_mixed) and W11O (gsoc). Bench-only, NOT linked into production runtime (V1-AUDIT:64).
2. **Fixture-named overfit.** `real_typed_struct.rs` is per-corpus hand-coded (TwitterSearch/CitmCatalog/
   GsocProposal/CanadaFeatureCollection/GithubEvent); `generated_real_typed.rs` (4941 lines,
   `schema_hash sk-v14-w9ab-canada`) has **148** fixture-named parse fns (verified
   `grep -c 'fn parse_' skinny/crates/bbnf-bench/src/generated_real_typed.rs` = 148 at HEAD `1c5bd7a25`; the
   architecture doc's "187" at sk-v16-arch:209 is stale) + hand-tuned per-corpus capacity constants —
   "textbook overfit." Plus the per-corpus capacity constants in the broadcast harness (V1-AUDIT noted in §4).

**Classification: PERMANENT PRE-BLOCK (as runtime/admission); ADMIT only as bench-only-quarantined.**

The FNV technique is *quarantined to bench/diagnostic metadata* (V2 AGENT-5:5-8, SK-V15 W10) and "cannot
be used as a runtime selector, production arbiter, or correctness proof." FNV production migration
**remains blocked** unless future work proves typed semantics independently of hash sidecars
(V2-CONSOLIDATED:62-63, REBUILD-WAVE-G V1-AUDIT:139-142). The fixture-named parse fns + hand-tuned capacity
constants are overfit with no admission path — the grammar-derived typed projection (§1/§2b) replaces them.

- **Re-open test (CH1/CH5 fail if true):** any FNV/checksum value migrates from bench metadata into
  `skinny/crates/runtime/` (or any production crate under `skinny/crates/`) as a selector/arbiter/correctness
  proof; any per-corpus / fixture-named parse fn or hand-tuned capacity constant on the SK-V17 tape path; any
  schema/policy/witness header whose value is keyed to a fixture name. The tape work especially must not
  re-introduce fixture-named capacity constants for tape pre-sizing — scratch must size from `input.len()`
  (sk-v16-arch:393, Wave 5), grammar-generally.
- **Different-framing admission (CH3 pass):** scalar-reference parity / checkasm uses fixtures as *inputs*,
  never as *selectors*; FNV stays a diagnostic equality-witness behind the W10 quarantine guard;
  capacity/sizing is derived from `input.len()` and the grammar's `BackendRule`/`LayoutFacts` shape, never
  per-corpus.
- **Telemetry binding:** the SK-V17 gate carries the W10 FNV-quarantine no-runtime-migration check
  (close evidence `cargo xtask gate-json --check-results --skv15-fnv-quarantine-report ...`,
  V2-CONSOLIDATED:44) and a Lock 14 grammar-name/grammar-shape leak census (LOCKS.md:380-387) that fails
  on any fixture-named symbol in the tape/projection path.

---

## §6 — x86 / AVX paths

**Source of record:** SK-V16 SPEC:75, :117 ("x86, AVX, PEXT, and x86 side evidence are diagnostic only");
SK-V16 HANDOFF:22-24 ("Native aarch64 SIMD … no W11 routed remainder … conditional candidate only");
sk-v16-arch:6, :265-266; REDRESS x86 sites (`crates/bbnf-simd/src/x86_64/avx2/classify.rs:31`,
`avx512_vbmi2/classify.rs:28`, `avx512_gfni/classify_affine.rs:31`, `avx512_bitalg/multiclass.rs:30`,
`skinny/REDRESS.md:465-468`); CONTEXT ("No x86").

**What it is:** the existing x86_64 AVX2/AVX-512 classify modules exist in `bbnf-simd` but are not the
admission target. The architecture is **aarch64 Apple M5 Max only**; SVE-disallowed (Apple cores have
**no SVE** — NEON+AMX only, so SVE paths would be dead code on M5 Max, sk-v16-arch:265-266); the plan is
NEON + optional dotprod/i8mm only.

**Classification: PERMANENT PRE-BLOCK (for THIS pass); diagnostic-only.**

x86/AVX is not "wrong" globally — it is **out of scope** for the SK-V17 aarch64-only proof. The SK-V16
HANDOFF reserves x86 as a possible *successor phase* (PASS-ALPHA §8 escalation: "dispatch x86 successor")
but it carries zero admission weight in SK-V17. Lock 16 (LOCKS.md:607) keeps x86 / AVX-512 rows as
optional flaw-probe / diagnostic only.

- **Re-open test (CH4/CH6 fail if true):** any SK-V17 NEON hot-leaf wave that lands an x86/AVX path as a
  *same-wave consumer* or claims a row movement on x86; any RESULTS row whose admitted Mbps came from an
  x86 build; any SVE/SME primitive filed as NEON (Lock 16, LOCKS.md:607 — SVE2 requires an SVE2 host).
- **Different-framing admission:** none in this pass. x86 AVX-512 rows may appear only as
  optional/diagnostic columns in the §4.3 telemetry schema (asmjson AVX-512, marked "x86 only"); they are
  flaw-probes, never the SOTA-beat plane. The hot-leaf vocabulary is aarch64 NEON intrinsics-first
  (`core::arch::aarch64::*`, `vqtbl4q_u8`, `vpaddq_u8`, `to_bitmask64` cascade, optional `udot`/i8mm under
  `is_aarch64_feature_detected!`; sk-v16-arch:273-298), each with scalar-ref + checkasm + same-wave
  consumer (CH4, Lock 16).
- **Telemetry binding:** every SK-V17 NEON primitive records the Lock 16 manifest (owner, scalar oracle,
  checkasm command, aarch64 hardware gate, same-wave consumer, row-movement target). No x86 in the manifest.

---

## §7 — Consolidated pre-block ledger

All re-open tests grep the **skinny benched tree** (`skinny/crates/...`, `skinny/xtask/...`); a gate keyed to
`crates/core/...` is a CH1 defect (see §0).

| # | Pre-block | Measured refutation | Class | Re-open test (CH3/CH5 fail) — skinny-keyed | Different-framing admission |
|---|---|---|---|---|---|
| 1 | AZ-IV eager-value-tree | 118x (canada 1.83ms→215.7ms, `cb14970f`) | ADMIT-UNDER-FRAMING | typed value built per-leaf at parse time; per-leaf typed/f64 heap alloc under `runtime/src/grammars/css_l4_*/` or benched `track1` fns | lazy view projection over sealed tape; zero payload bytes/writes/allocs (REDRESS 6); PayloadArena (`tape/mod.rs:38`) only for irreducible decode |
| 2a | StructRegistry/Arena<G>/Builder<G> indirection | 28-65x bbnf/sheets; 983x css bootstrap (606.4ms); 10583x WATCHDOG tailwind (77.6s) | PERMANENT PRE-BLOCK | any per-leaf registry lookup; per-compound Vec/split_off/Box; frame-stack clone checkpoint; new hand-coded per-grammar profile table re-introduced into `skinny/crates/runtime/` | — (none) |
| 2b | Layout description (the layout itself) | (not refuted; built once per rule) | ADMIT-UNDER-FRAMING | emitter hardcodes a per-grammar profile/route table (`W5C_REQUEST_FACT_PROFILES`, the 7 `RequestFacts` registrations) instead of deriving tape ops from `BackendRule`/`LayoutFacts` shape | Open/Close records via `TapeBuilder` + cursor arithmetic over `Tape`/`ValueRef`; O(1) checkpoint marker+truncate; emitter derives from `LayoutFacts.backend_shape` (`ir/cost.rs`) via `lower/{tape_plan,offset_tape,event_tape}.rs`. **NB Lock 2: canonical name is `Layout`/`LayoutFacts`, NOT `StructLayout`** |
| 3 | CSS fact-stream String | ~34% self-time `emit_*`; benched ~63.9 Mbps is String not typed | PERMANENT PRE-BLOCK (as admission plane) | fact-stream String reported as typed result; push_str/fnv64 on hot path; **OR `RuntimeEmitterKind::RequestFacts` still registered for the 7 CSS grammars (`regen_css.rs:45..153`) / `W5C_REQUEST_FACT_PROFILES` (`codegen/src/lib.rs:336`) still drives an admitted row** | typed CSSOM via lazy projection; fact-stream + `W5C_REQUEST_FACT_PROFILES` retired to diagnostic-only behind forbidden-token scan |
| 4 | 24-row broadcast | one tuple ×24 row-ids (`css_l4_w8.rs:206-228`, `W8_SELECTED_CSS_ROWS=24`); all 24 falsified rows still present in `skinny/RESULTS.md` (lines 112-135, grep-verified) | PERMANENT PRE-BLOCK | N rows share Mbps tuple; aggregate-loop projected across row-ids | per-corpus independent timing (animate/bootstrap/tailwindcss/material-components-web, `css_l4_corpus.rs:22-54`), distinct measurement_row_id + broadcast_group_id (Lock 8); N≥50 cold + median |
| 5a | FNV closed-enum arbiter | bench-only; strict-product blind to hash collisions (W11L/N/O) | PERMANENT PRE-BLOCK (as runtime) | FNV migrates to `skinny/crates/runtime/` as selector/arbiter/proof | FNV diagnostic equality-witness only, behind W10 quarantine gate |
| 5b | Fixture-named overfit | 148 fixture fns (`grep -c fn parse_` = 148, not the stale 187); `sk-v14-w9ab-canada` hash; per-corpus capacity consts | PERMANENT PRE-BLOCK | fixture-named parse fns / capacity consts on tape path | scratch sizes from input.len() + `BackendRule`/`LayoutFacts`, grammar-general; fixtures as inputs not selectors |
| 6 | x86 / AVX | out-of-scope; Apple M5 Max no-SVE; diagnostic-only | PERMANENT PRE-BLOCK (this pass) | x86/AVX same-wave consumer or row-movement claim; SVE filed as NEON | x86 AVX-512 as optional/diagnostic flaw-probe column only; aarch64 NEON intrinsics-first hot leaves |

---

## §8 — The single load-bearing distinction for SK-V17

The tape+lazy-view IS the different framing for materialization. The line every SK-V17 wave must hold:

> **Typed/rich/retained is the goal (admit). Eager/allocating/fragmented/serialized is the refuted carrier
> (pre-block).** The flat lazy-offset tape (parse side, one branchless append via `TapeBuilder`, O(1)
> checkpoint) + the layout-driven typed projection (view side, derived from `BackendRule`/`LayoutFacts`,
> lazy source re-read over `Tape`/`ValueRef`, `PayloadArena` only for irreducible decode) is the ONLY
> admissible carrier for the typed CSSOM. Any construct from §1-§6 that lands on that carrier with zero
> per-leaf alloc, zero registry indirection, zero String hot-path, no `RequestFacts`/`W5C_REQUEST_FACT_PROFILES`
> route still admitting, per-corpus honest timing, no FNV/fixture selector, and no x86 — is admitted. Any
> construct that re-lands on the OpenFrame/StructRegistry/Vec<Vec>/fact-stream/broadcast/FNV/x86 carrier — is a
> CH3 regression or CH5 hidden-coupling failure and must be REJECTED at the CHALLENGE gate.

**All deletion/wiring gates grep the skinny benched tree** (`skinny/crates/runtime/src/tape/`,
`skinny/crates/codegen/src/lower/`, `skinny/crates/codegen/src/lib.rs`, `skinny/xtask/src/regen_css.rs`,
`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`). A gate keyed to `crates/core/...`, `StructLayout`,
`OpenFrame`, `CssArena`, `begin_compound`, or `TapeStructBuilder` is grep-clean-absent from skinny and is
itself a CH1 defect — those are the TOTALITY fold target, not SK-V17 owner paths.

Lock anchors load-bearing for these pre-blocks: Lock 1 substrate-union / no-parallel-substrate
(LOCKS.md:75, :585), Lock 2 canonical `Layout`/`LayoutFacts` (`StructLayout` RETIRED, LOCKS.md:160),
Lock 8 row-plane/broadcast (LOCKS.md:595), Lock 14 grammar-neutrality (LOCKS.md:603, :380-387),
Lock 16 primitive-manifest / aarch64-only (LOCKS.md:607).
