# SK-V20 / 20 — SOTA Gap Refresh (HEAD 36ca10646)

Scope: refresh the SK-V17 P2-A SOTA teardown against the trees at HEAD. The anchor
torn down a CSS path that emitted a fact-stream `String` over an UNWIRED tape, with
ZERO SIMD on CSS and a lazy `ValueRef` rider that "does not exist". That world is gone.
The four anchor candidates (CP-A1 NEON classifier, CP-A2 tape append, CP-A3 lazy
`ValueRef` rider, CP-A4 tokenize-once) have LANDED for CSS. This refresh re-grounds the
ADOPTED/NOT-ADOPTED/INAPPLICABLE ledger against the *current* code and the *current*
SOTA frontier — most consequentially simdjson 4.3 (Feb 2026), which moved the bar with
a NEON string-escaping kernel and batch integer formatting.

All bbnf claims are file:line at HEAD; external claims carry a citation. This SELECTS
nothing — it grounds the SK-V20 candidate pool and exposes the residual gaps.

## §1 — What the anchor's candidates became (the landed delta)

| Anchor candidate | Anchor status | HEAD status | Evidence |
|---|---|---|---|
| CP-A1 block-wide NEON classifier | un-landed; "zero SIMD on CSS" | **LANDED** | `runtime_scan.rs:155` `structural_classify_64(block, partition)`; bit-drain `:160-211` |
| CP-A2 tape-append sink | unwired for CSS | **LANDED** | `generated.rs:510` `push_plain_offset`; `assembler.rs:71` branchless `u32` write + `:87` cold reserve |
| CP-A3 lazy `ValueRef` rider | "does not exist — gating artefact" | **LANDED** | `generated.rs:73-140` `CssNode`/`Cursor`; `:316` `ValueRef::new`; `:247` lazy `typed_value` |
| CP-A4 tokenize-once | redundant 2–3× re-walk | **PARTIAL** | scan delegates to `css_balanced_component_scan`; but `parse_block_item`→`find_colon_before`→`parse_declaration` still re-scan (`generated.rs:615,622,650`) |

The `emit_fact_stream` String floor (~58–64% of self-time in the anchor) is GONE: the
CSS plane now materializes into the shared SoA `Tape` (`tape/mod.rs:92`: borrowed
`source: &[u8]`, `offsets: Vec<u32>`, sparse `flag_cursors`/`flag_values`, bump
`PayloadArena`). The `push_ascii_lower_hex` FNV diagnostic is retired (the residual
`fnv64`/`push_hex64` at `generated.rs:767,776` is dead relative to the hot path).

So the anchor's two top levers are spent. The refresh must look PAST them.

## §2 — Per-competitor technique ledger at HEAD

### 2.1 simdjson — stage1 / stage2 / On-Demand (+ 4.3, Feb 2026)

- **Stage-1 block-wide structural classify** → **ADOPTED** for both grammars. JSON:
  `json/scan.rs:215-253` (`escape_mask_64`/`prefix_xor_64`/`compact_mask`). CSS:
  `runtime_scan.rs:155` `structural_classify_64` over 64-byte blocks, draining a `u64`
  significant-mask via `trailing_zeros` (`:163`). This is the genuine stage-1 analogue,
  not a masking probe.
- **Stage-2 flat tape (type, offset records; no per-node box)** → **ADOPTED**. The SoA
  `Tape` is the tape; `offsets: Vec<u32>` are the (offset) records, branch tags live in
  sparse `flag_values` (`tape/mod.rs:94-97`). No `Box`-per-node.
- **On-Demand lazy forward iterator (parse values as used)** → **ADOPTED**. JSON
  `value.rs:143` `value_from_ref`; CSS `generated.rs:247` `typed_value` decodes kind on
  demand from the value head (`CssTypedValue::classify`, `:274`), reconstructing nothing
  eagerly. The `CssNode` cursor is 8 bytes (`:73-74`), no eager subtree.
- **`HasEsc` string flag → borrow non-escaped span** → **ADOPTED**. `view.rs:206-216`:
  `as_str` returns `Cow::Borrowed(self.raw())` unless `flags_at(cursor)` reports
  `string_needs_decode`; only then `unescape_string`. Exactly the simdjson discipline.
- **Eisel-Lemire number parse (stage-2 `parse_decimal_after_separator`)** →
  **NOT-ADOPTED**. JSON numbers decode via `serde_json::from_str::<Number>` and std
  `str::parse` (`view.rs:232,238`). No fast-float path, no power-of-ten table. `lexical-core`
  is in `Cargo.lock:696` but pulled ONLY transitively by the `simd-json` *comparator*
  (`Cargo.lock:1287-1300`), never by `runtime`. This is the single largest unadopted
  primitive on the JSON typed plane.
- **simdjson 4.3 NEON SIMD string-escaping + batch integer formatting (Feb 2026)** →
  **NOT-ADOPTED, NEWLY-RELEVANT**. 4.3 gives +30% on the string-heavy Twitter benchmark
  via an ARM64-NEON unescape/escape kernel and batched integer write [Phoronix,
  "simdjson 4.3", phoronix.com/news/simdjson-4.3, Feb 2026; simdjson `ondemand_design.md`].
  bbnf HAS a NEON hex-quartet kernel — `unescape_uxxxx_x4_neon` (batch-of-4,
  `aarch64/unescape_uxxxx.rs:125`) — wired ONLY through `parse-that-regex`'s
  `unescape_string` for the `\uXXXX` case. The *bulk* string-body unescape (the
  backslash-escape sweep that 4.3 vectorized) remains scalar (`parse-that-regex/src/lib.rs`
  `unescape_string`). This is now a moving target the anchor predates.
- **AVX-512 512-bit blocks (4.3)** → **INAPPLICABLE**. x86-only; host is Apple-M aarch64
  (precept 4). The NEON half of 4.3 is the applicable half.

### 2.2 sonic-rs — targeted SIMD leaves + direct typed deserialization

- **Reject two-stage; targeted SIMD at long-string/float-fraction/field-lookup/ws-skip**
  → **PARTIALLY ADOPTED / re-framed**. bbnf chose the *opposite* axis: it DOES run a
  stage-1 block classify (CSS `structural_classify_64`, JSON `scan.rs`). The sonic-rs
  "four leaves" map to: long-string scan = ADOPTED (string-body mask, `scan.rs`); ws-skip
  = scalar (`generated.rs:668` `is_ascii_whitespace` byte loop — NOT vectorized);
  float-fraction = NOT-ADOPTED (no number SIMD anywhere); field-lookup = INAPPLICABLE
  (grammar-neutral generator has no key-set to probe).
- **LTO-fused single `parse_object`/`parse_array` driver** → **ADOPTED in spirit** for
  JSON (`json/generated.rs` `ln_*_direct` dispatch chain is one fused recursive driver,
  `dispatch_value:45`). NOT-ADOPTED for CSS: `parse_block_item`/`find_colon_before`/
  `parse_declaration` are three scan passes over the same body (`generated.rs:615,622,650`),
  the un-fused re-walk CP-A4 was meant to retire — still PARTIAL.
- **Direct-into-struct (no temp tape) is faster than simd-json's parse-to-tape-then-walk**
  [sonic-rs `performance.md`, github.com/cloudwego/sonic-rs] → **NOT-ADOPTED, by
  deliberate architecture**. bbnf is tape-first by Lock 1 (one substrate). sonic-rs's
  no-tape claim is a real asymmetry to DISCLOSE, not adopt: our lazy `ValueRef` over the
  tape is the chosen materialization-without-String shape; a no-tape direct deserializer
  would fork the substrate. INAPPLICABLE under Lock 1, but the honest framing must say so.

### 2.3 yyjson — scalar-ILP DOM baseline

- **No-SIMD ILP DOM that is still SOTA on selected corpora** → its role as the
  *refutation anchor* (tape-lever-first, NEON second) is now **VINDICATED BY HISTORY**:
  the tape landed first (CP-A2), NEON second (CP-A1), matching yyjson's lesson.
- **Contiguous value-array DOM (not Box-per-node)** → **ADOPTED**: the SoA tape IS the
  contiguous array (`tape/mod.rs:94`). yyjson remains the scalar oracle shape, not a
  primitive (precept: checkasm differential baseline).

### 2.4 lightningcss — the fair CSS >SOTA bar (full L2 CSSOM)

- Still THE speed bar: full owned CSSOM via cssparser tokenize + typed value build
  [github.com/parcel-bundler/lightningcss; lightningcss.dev]. Pinned at `=1.0.0-alpha.71`
  (`bbnf-bench/Cargo.toml:14`). No 2025–2026 SIMD tokenizer landed in lightningcss
  (it still rides Mozilla cssparser; web search found no SIMD changelog). **The bar has
  NOT moved** — bbnf's CSS materialization gap is a tape-vs-typed-CSSOM gap, now closed
  to a lazy-`ValueRef`-over-tape plane (`generated.rs:316-376`). The honest residual:
  bbnf's CSS typed plane CLASSIFIES dimensions/colors (`CssTypedValue::classify:274`) but
  does NOT numerically decode them — it is a shallower materialization than lightningcss's
  fully-typed value tree. This DEPTH ASYMMETRY must stay disclosed (commit a97f5f365
  already names it).

### 2.5 cssparser — token-scan parity oracle (NOT a speed bar)

- Unchanged role: structural-equality parity oracle, not a >SOTA target
  (`bbnf-bench/Cargo.toml:13`). A faster comparator that retains less is not a fair bar.
  Correct as the anchor framed it; no refresh needed.

### 2.6 serde_json — the std baseline (and our number backend)

- serde_json is a comparator (`Cargo.toml:20`) — AND, problematically, our JSON number
  decoder (`view.rs:232`). Decoding numbers THROUGH the comparator's `serde_json::Number`
  is a self-inflicted ceiling: we can never beat serde_json on the number leaf while we
  CALL serde_json for it. NOT-ADOPTED items §3 rank this first.

## §3 — NOT-ADOPTED, ranked by expected aarch64 win (grammar-emittable only)

Each must be EMITTABLE from grammar (precept 1/6: no hand-coded per-grammar kernel), and
each cites the leaf it would retire.

1. **Eisel-Lemire fast-float / fast-int number decode** (retires `view.rs:232,238`
   serde_json+std parse). Highest win: the number leaf is currently un-optimized AND
   routed through a comparator. Apple-M has fat integer-multiply ILP that Eisel-Lemire's
   128-bit product exploits well [Lemire, "Fast float parsing in practice", lemire.me/blog
   /2020/03/10; Sep 0.11.0 hit 9.5 GB/s on M1 with a NEON-tuned number path,
   nietras.com/2025/06/17/sep-0-11-0]. GRAMMAR-EMITTABLE: a `->` leaf typed `f64`/`i64`
   emits a call to ONE generic `decode_f64_eisel_lemire(span)` primitive in bbnf-simd; the
   grammar supplies which spans are numbers, the kernel knows no grammar. This is the
   anchor's CP-BLOCKED digit kernel, now UN-blockable because the typed plane exists to
   profile (the anchor's re-admission condition is met).

2. **NEON bulk string-body unescape** (the simdjson-4.3 escaping kernel; retires the
   scalar `unescape_string` body in `parse-that-regex`). The `\uXXXX` quartet is already
   NEON (`unescape_uxxxx_x4_neon:125`); the surrounding backslash-escape sweep is not.
   aarch64 win is the 4.3-measured +30% on string-heavy corpora, on OUR ISA half.
   GRAMMAR-EMITTABLE: the unescape is keyed off the existing `HasEsc` flag
   (`view.rs:210`); a `->` string leaf already routes through `as_str` — only the kernel
   body changes, generically, for every grammar with escaped strings (JSON, Sheets quoted
   cells, CSS string tokens).

3. **Branchless structural dispatch in the bit-drain loop** (retires the 5-deep `if`
   chain `runtime_scan.rs:167-208`). Each structural bit walks up to five sequential
   `c.stop/open/quote/slash/close & bit` tests. simdjson/sonic-rs dispatch the structural
   *class* branch-free (a class index into a small table) [simdjson `ondemand_design.md`;
   "SIMD code is most effective branchless", per the stage-2 literature]. aarch64 win:
   removes mispredict stalls in the hottest CSS loop. GRAMMAR-EMITTABLE: the
   `KindPartition` already names the buckets (`runtime_scan.rs:76`); the classifier can
   emit a per-bit 3-bit class lane (a movemask-cascade) so the drain indexes an action
   table rather than testing five masks. This is the un-adopted half of CP-A1.

4. **Whitespace/trivia skip vectorization** (retires `generated.rs:668` scalar ws loop +
   `skip_ws_comments`). sonic-rs's ws-skip leaf. Lower win than 1–3 (ws runs are short in
   minified CSS/JSON corpora) but grammar-neutral and trivially emittable: a `ln_64`
   eq-set classify over the whitespace set, drained to the first non-ws bit. The kernel
   (`ln_64`, `bbnf-simd/src/lib.rs`) already exists; it is simply not wired to the ws skip.

5. **CP-A4 tokenize-once for CSS** (retires the 2–3× body re-walk, `generated.rs:615→622→650`).
   Architectural, not a kernel: scan the declaration body ONCE into the structural index
   and have `find_colon_before` consume the index instead of re-scanning. Lower ranked
   because the block classify is now cheap (NEON), so the re-walk tax shrank vs the
   anchor's scalar world — but it is still redundant work and a clean fold.

## §4 — INAPPLICABLE (recorded so they are not re-litigated)

- **AVX-512 / SVE collapsed-stage FSM (asmjson, simdjson-4.3 512-bit)** — x86/non-NEON,
  host-refused (precept 4). The NEON portions of 4.3 are the only applicable parts.
- **sonic-rs no-tape direct-to-struct** — forks the substrate; Lock 1 mandates one tape.
  A real asymmetry to DISCLOSE in the honesty framing, not a primitive to adopt.
- **simd-json mutable-buffer in-place unescape** [its `as_str` needs `&mut [u8]`] —
  bbnf borrows `&'input` source immutably (`tape/mod.rs:93`); in-place scribbling would
  break the zero-copy borrow that the `Cow::Borrowed` fast path depends on. The lazy
  `ValueRef` plane is the chosen alternative.
- **Field/key-set SIMD lookup (sonic-rs)** — needs a fixed schema key-set; the
  grammar-neutral generator has none to probe. INAPPLICABLE by construction.

## §5 — Honesty residuals the refresh surfaces

1. **JSON numbers decode through the serde_json comparator** (`view.rs:232`). Any "beats
   serde_json" number claim is structurally impossible while we call it. Rank-1 NOT-ADOPTED.
2. **CSS typed values are classified, not decoded** (`CssTypedValue::classify:274`):
   shallower than lightningcss's typed value tree. The depth asymmetry is named in commit
   a97f5f365 and must stay in every CSS >SOTA sentence.
3. **Comparator pins are stale**: simd-json `=0.13.11` (~2023), lightningcss alpha.71.
   The live SOTA frontier (simdjson 4.3, Feb 2026) is C++ and not in the Rust bench
   harness — so "beats simd-json 0.13" is NOT "beats the 2026 frontier". Disclose the pin.
4. **`unescape_uxxxx` NEON kernel is reachable only via the regex crate**, never directly
   from runtime; the bulk-unescape sweep it could anchor is still scalar.

Sources (external, web-verified June 2026): simdjson 4.3 +30% string-heavy via NEON
escaping/batch-int [phoronix.com/news/simdjson-4.3; simdjson `ondemand_design.md`];
sonic-rs no-tape direct deserialize [github.com/cloudwego/sonic-rs `performance.md`];
Eisel-Lemire adoption + NEON M1 number speed [lemire.me/blog/2020/03/10;
nietras.com/2025/06/17/sep-0-11-0]; lightningcss still cssparser-based, no SIMD changelog
[lightningcss.dev; github.com/parcel-bundler/lightningcss]; stage-1/stage-2 branchless
discipline [Langdale & Lemire arXiv:1902.08318; Mison VLDB17].
