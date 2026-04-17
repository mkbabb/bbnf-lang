# AW-V H2 — Visitor trait hierarchy, monomorphisation, and L1 i-cache fit

## Executive summary

Visitor hierarchy factors into `GrammarVisitor` top-level + seven
per-shape sub-traits (`Object`/`Array`/`String`/`Number`/`Keyword`/
`Pratt`/`Unordered`Visitor); per-shape fns are generic over one shape
trait and monomorphised at the call site. Empirical `nm` baselines:
bbnf walker = 22 008 B on JSON, 157 556 B on CSS L4; sonic
`parse_object<DocumentVisitor>` = 12 072 B, `parse_array` = 10 012 B.
JSON shape-emitted parser under both visitors fits L1 (60 KB).
CSS L4 single-visitor projects 600 KB, dual 1.2 MB — overflows L1
on every M-chip variant; hot/cold partitioning via
`state_visit_frequency` miner (W4.2) drops hot working set to
~150 KB (fits M4 192 KB). **Multi-visitor (Lever 7) caps at ≤ 2
visitor types per call site, opt-in via
`#[derive(Visitor)] #[emit_paired_with(V2)]`** — unconditional
emission explodes past L1 even on JSON. Incremental reparse lives
on a separate `ResumableVisitor` trait; streaming routes through
the cold-path `dispatch_one` replay surface, not per-shape
recursion.

---

## 1. Trait hierarchy — design summary

`GrammarVisitor<'de>` carries `type Error: From<ParseError>` +
`type Output`. Seven per-shape sub-traits extend it: `Object`,
`Array`, `String`, `Number`, `Keyword`, `Pratt`, `Unordered`Visitor.
Resume on separate `ResumableVisitor: GrammarVisitor` (Task 5).
`JsonVisitor` composes relevant sub-traits as marker alias. Every
method `#[inline(always)]`; `'de` ties visitor borrows to input so
the zero-copy `borrowed: bool` path (bbnf-tape's
`payload_string_with_source` two-mode contract) lifts through.
`declared_keys(&self) -> &'static [&'static [u8]]` on
`ObjectVisitor` exposes Lever 3's SIMD multi-key set — default
empty. Full signatures in **Appendix A**.

`#[inline(always)]` is load-bearing: LLVM collapses visitor calls
only when the concrete type is known AND the method is always-inline.
AW-IV evidence: `try_branch` stayed un-inlined across the
`bbnf-tape`/`core` boundary without it. LLVM does not "figure it out."

---

## 2. Monomorphisation cost — concrete per-grammar code-size arithmetic

Per-line machine-code density, empirically: bbnf walker = 4 B/line
(5 500 LOC → 22 008 B, state-table arms); sonic `parse_object` =
30 B/line (400 LOC → 12 072 B, SIMD+unrolled key match). Per-shape
emitted loops track sonic — project at **25 B/line**.

### 2.1 Per-grammar monomorphisation count

Shape-mining projects each grammar's rule set into a small count of
shape-emitted functions (B4 §3 coverage percentages; non-shape rules
stay on the unified walker).

| Grammar | Rules (total) | Shape-emitted fns | Coverage | Walker residual |
|---|---:|---:|---:|---|
| JSON | 6 | 6 | 100 % | 0 (walker gone on JSON) |
| Sheets | ~40 | ~18 (Pratt × 6 + number + string + bool + 9 error literals as keywords + ~3 composites) | 92 % | ~3 residual fns |
| CSS L4 | ~400 | ~80 (shape-mining reduces the long tail) | 78 % | ~85 residual fns on monolithic walker |
| BBNF | ~35 | ~12 (directive PHF + rule + identifier + alt branches) | 75 % | ~9 residual fns |

### 2.2 Per-visitor per-grammar code-size projection

Average per-shape fn LOC: JSON ~200 (B1 App A: 20-40 fn body + ~80
SIMD + ~80 Eisel-Lemire); CSS ~300 (unordered 5-branch dispatch +
per-branch sub-loop).

| Grammar × Visitor | LOC total | Machine-code bytes | Fit in 128 KB L1 i-cache? |
|---|---:|---:|:---:|
| JSON × TapeVisitor | 6 × 200 = 1 200 | 30 000 | **yes** (23%) |
| JSON × ValueVisitor | 6 × 200 = 1 200 | 30 000 | **yes** (23%) |
| JSON × (Tape + Value) both monomorphised | 2 400 | 60 000 | **yes** (47%) |
| Sheets × TapeVisitor | 18 × 250 = 4 500 | 112 500 | marginal (88%) |
| Sheets × both | 9 000 | 225 000 | **no** (176%) |
| CSS L4 × TapeVisitor | 80 × 300 = 24 000 | 600 000 | **no** (469%) |
| CSS L4 × both | 48 000 | 1 200 000 | **no** (938%) |
| BBNF × TapeVisitor | 12 × 250 = 3 000 | 75 000 | **yes** (59%) |
| BBNF × both | 6 000 | 150 000 | **no** (117%) |

M4 P-core L1 i-cache = 192 KB; M1/M2/M3 P-cores = 128 KB. Even on
M4: JSON×both fits; Sheets×both (225 KB) doesn't; CSS L4 overflows
3-6× on any variant.

### 2.3 Hot working set — the mitigating reality

Per P2/P5 profiles, CSS bootstrap touches ~20 rules hot, ~60 cold;
twitter JSON touches all 6 rules hot. Hot working set is the
high-frequency subset actually resident during steady-state parse.

| Grammar × Visitor | Hot-rule count | Hot code-size | Fit in 128 KB L1? |
|---|---:|---:|:---:|
| JSON hot | 6 | 30 000 | yes |
| Sheets hot (Pratt + number + string + cell_ref) | ~10 | 75 000 | yes |
| CSS L4 hot (compound+complex+declaration+selector+mathExpr+mathProduct+color+length) | ~20 | 150 000 | **no** (117%) |
| BBNF hot (rule + term + identifier + directive) | ~8 | 60 000 | yes |

**CSS L4 under TapeVisitor alone overflows L1's hot working set** —
confirming AW-V's claim that CSS i-cache pathology is structural,
not walker-artefact. Mitigation is grammar-shape-driven (§10).

### 2.4 Comparison to empirical walker + sonic sizes

| Symbol | Empirical size | Source |
|---|---:|---|
| `__dta_walker_inline::run` (JSON) | **22 008 B** | `nm` on bench binary |
| `__dta_walker_inline::run` (CSS L4) | **157 556 B** | `nm` on css_l4 binary |
| `bbnf_tape::driver::advance_or_pop_with` | **1 624 B** | `nm` |
| `sonic_rs::parser::parse_object<DocumentVisitor>` | **12 072 B** | `nm` |
| `sonic_rs::parser::parse_array<DocumentVisitor>` | **10 012 B** | `nm` |
| Total sonic monomorphised hot symbols (PaddedSliceRead) | **22 084 B** | sum |

Shape-emitted JSON parser projects at ~30 KB for 6 rules vs sonic's
22 KB for 2 rules; extra 8 KB = four additional leaf shapes. Fits
L1 with 75% headroom on M4.

---

## 3. Multi-visitor parallel monomorphisation (Lever 7) — explosion analysis

Lever 7's `parse_object<V1, V2>(.., v1, v2)` monomorphises as
`(visitor-types)^N`. With 4 project visitor types {`TapeVisitor`,
`ValueVisitor`, `JsonValueVisitor`, `UserCustomVisitor`}: 1-visitor =
4; 2-visitor ordered pairs = 16; 3-visitor = 64. JSON × 6 shape fns
× 16 pairs ≈ 480 000 B at 25 B/LOC × 200 LOC — **L1 fit fails even
on JSON** under unconditional binary emission.

**Recommendation: cap to ≤ 2 visitor types per call site, gated by
opt-in `#[derive(Visitor)] #[emit_paired_with(V2)]`.** Default emits
only single-visitor forms (linear in visitor count); the attribute
additionally emits the paired form
`parse_<shape><V1=Self, V2=DeclaredPair>`. Only one pair lands in
AW-V: `(TapeVisitor, ValueVisitor)` for the W2.3.a twin-visitor
validation gate. User-custom pairs are AX scope.

`Option<V2>` with stable-Rust specialisation was considered and
rejected — stable Rust lacks specialisation; a `NoVisitor`-marker
probe adds a generic parameter to every call even when single-visitor.

---

## 4. Incremental reparse compatibility

AX.2.3 re-walks from a snapshot at a compound boundary. Restart-
ability is not universal: **TapeVisitor** is natural — Columns
`splice_range` admits the pre-snapshot length as restart-point;
**ValueVisitor** is hard — partial `Value::Object(partial_map)` would
need to re-root at splice; **user visitors** are user-decided.

Restart lives on the separate `ResumableVisitor` trait. Shape
emitters invoke `restart_from_snapshot` only when the visitor
implements the trait; otherwise a fresh visitor is required. AX opens
with `TapeVisitor: ResumableVisitor` and nothing else.

---

## 5. Streaming — routed through cold-path, not per-shape recursion

Callback-based streaming (visitor methods take byte slices + return
`Poll::Pending` on incomplete) requires a state-machine wrapper —
which is exactly AW-II's DTA driver. Layering one over per-shape
recursive descent duplicates DTA, violating single-source invariant.

Chunked parse at natural boundaries works for flat lists (top-level
JSON arrays; CSS ruleset boundaries) but fails for nested compounds
(mid-object chunk = syntax error).

**Recommendation.** Streaming routes through the cold-path
`dispatch_one` replay surface, not per-shape recursive descent. The
replay surface is already state-machine-shaped; snapshot-resume is
its natural streaming API. Per-shape parse stays whole-buffer for
AW-V; streaming is AX scope via `parse_resume`.

---

## 6. Graph-output visitors — tree core + separate graph overlay

JSON is tree only. CSS's `SelectorDAG` is a *derived* analysis, not
the parse output — parse produces the `Selector` tree; a post-pass
computes specificity DAG. BBNF rule cross-refs are by-name; emit as
tree; resolve in IR pass. User graph visitors accept back-references
via `fn visit_cross_ref(&mut self, target: NodeHandle)` on a
separate extension trait.

Universal graph-visitor support would force every shape fn to track
node handles → explodes i-cache. Keep core visitors tree-shaped;
graph overlay is a separate pass consumer.

---

## 7. Compile-time vs runtime visitor resolution — both paths, mono default

`dyn GrammarVisitor` negates monomorphisation; LLVM cannot inline
through the vtable. Provide both: **monomorphic default** (`parse_json<
V: JsonVisitor>`; zero dyn dispatch; AW-V W2.3 gate) and **opt-in dyn
wrapper** (`parse_json_dyn(input, v: &mut dyn JsonVisitor)`; thin
adapter that forwards through the trait object; documented 2-5×
slower). Both compile from the same shape-emit body.

---

## 8. Error handling in visitors

Methods return `Result<(), Self::Error>` with `Self::Error:
From<ParseError>`. `?` propagation inlines the Ok branch and leaves
Err cold. **Per-shape rules:** Object/Array `begin_X` success is
required before content — emitter pre-checks visitor error *before*
advancing past the open delim (keeps AX replay-log deterministic).
String/Number/Keyword leaves: error after bytes-consumed is safe to
propagate. Pratt/Unordered operator-visit failures abort the whole
precedence-chain; partial operator stacks drop safely.

```rust
visitor.begin_object(0)?;  // visitor error aborts before any content
*pos += 1;                 // only advance on visitor success
```

---

## 9. Code-size mitigation for CSS L4

CSS L4 × single-visitor = 600 KB >> 128 KB L1. **Recommended:
hot/cold partitioning via `state_visit_frequency` miner** — top 20%
of rules by visit-frequency emit normally; remaining 80% emit
`#[cold]`, placed in a cold linker section outside the hot working
set. Hot working set ≈ 150 KB (marginally over 128 KB; fits 192 KB
M4 L1). Cold section size is irrelevant — not cache-resident. The
miner already exists at
`crates/ir/src/passes/recognizers/state_visit_frequency.rs`. Lands
in W4.2 as part of CSS shape-mining extension.

Cargo feature flags per visitor give user-level code-size control
(include only wanted visitors) but not hot-working-set control —
hot/cold partitioning is the load-bearing mechanism. Grammar-author
`@hot` / `@cold` overrides are AX future scope.

---

## 10. Conclusion — concrete recommendations for W1.3 + W3.x

1. **Trait tree**: seven per-shape sub-traits + `GrammarVisitor` +
   `ResumableVisitor`; every method `#[inline(always)]`; `Error` +
   `Output` associated types (full signatures Appendix A).
2. **Multi-visitor cap**: ≤ 2 types per call site; opt-in via
   `#[derive(Visitor)] #[emit_paired_with(V2)]`. Only
   `(TapeVisitor, ValueVisitor)` pair lands in AW-V.
3. **L1 mitigation**: hot/cold partitioning via
   `state_visit_frequency` in W4.2 for CSS L4 (top-20% hot,
   remainder `#[cold]`). JSON/BBNF/Sheets single-visitor fit
   natively.
4. **Incremental/streaming**: separate `ResumableVisitor` trait;
   streaming routes through `dispatch_one` replay surface (AX
   `parse_resume`), not per-shape recursion.
5. **Graph-output**: tree-core visitor; graph overlays as separate
   passes.
6. **Runtime resolution**: mono-default + opt-in dyn wrapper
   (documented 2-5× slower).
7. **Error handling**: `Result<(), V::Error>` with
   `From<ParseError>`; visitor-ack before `*pos` advance on compound
   openers (deterministic AX replay).
8. **Lever 7 disposition**: **conditional, opt-in** — NOT
   unconditional. W2.3.a emits `(TapeVisitor, ValueVisitor)` only
   when the prototype's `ValueVisitor` declares
   `#[emit_paired_with(TapeVisitor)]`. Unconditional emission
   explodes i-cache on every grammar larger than JSON.

---

## Appendix A — Compile-checkable trait signatures (full)

Pass through `cargo check` as-is when `DtaSnapshot` + `ParseError`
are in scope.

```rust
/// Top-level. Parse-lifetime errors; output is visitor-produced.
pub trait GrammarVisitor<'de> {
    type Error: From<ParseError>;
    type Output;
    #[inline(always)]
    fn begin_parse(&mut self, _input_len: usize) -> Result<(), Self::Error> { Ok(()) }
    fn finalize(self) -> Result<Self::Output, Self::Error>;
}

pub trait ObjectVisitor<'de>: GrammarVisitor<'de> {
    #[inline(always)]
    fn begin_object(&mut self, cap_hint: u32) -> Result<(), Self::Error>;
    #[inline(always)]
    fn visit_key(&mut self, key: &'de [u8], borrowed: bool) -> Result<(), Self::Error>;
    /// Key-set pre-declaration for SIMD multi-key compare (Lever 3).
    /// Default: empty — emitter falls back to linear-compare.
    #[inline(always)]
    fn declared_keys(&self) -> &'static [&'static [u8]] { &[] }
    #[inline(always)]
    fn end_object(&mut self, len: u32) -> Result<(), Self::Error>;
}

pub trait ArrayVisitor<'de>: GrammarVisitor<'de> {
    #[inline(always)]
    fn begin_array(&mut self, cap_hint: u32) -> Result<(), Self::Error>;
    #[inline(always)]
    fn end_array(&mut self, len: u32) -> Result<(), Self::Error>;
}

pub trait StringVisitor<'de>: GrammarVisitor<'de> {
    #[inline(always)]
    fn visit_string(&mut self, bytes: &'de [u8], borrowed: bool)
        -> Result<(), Self::Error>;
}

pub trait NumberVisitor<'de>: GrammarVisitor<'de> {
    #[inline(always)]
    fn visit_f64(&mut self, v: f64) -> Result<(), Self::Error>;
    #[inline(always)]
    fn visit_i64(&mut self, v: i64) -> Result<(), Self::Error> {
        self.visit_f64(v as f64)
    }
}

pub trait KeywordVisitor<'de>: GrammarVisitor<'de> {
    /// `discriminant` is the branch index assigned by KeywordStatsMiner.
    #[inline(always)]
    fn visit_keyword(&mut self, discriminant: u8) -> Result<(), Self::Error>;
}

pub trait PrattVisitor<'de>: GrammarVisitor<'de> {
    #[inline(always)]
    fn visit_operator(&mut self, op: u8, precedence: u8) -> Result<(), Self::Error>;
    #[inline(always)]
    fn visit_operand_end(&mut self) -> Result<(), Self::Error>;
}

pub trait UnorderedVisitor<'de>: GrammarVisitor<'de> {
    /// `branch_idx` from DisjointFirstMiner's dispatch table.
    #[inline(always)]
    fn visit_unordered_branch(&mut self, branch_idx: u8) -> Result<(), Self::Error>;
}

/// Marker composition: a full JSON visitor implements all four.
pub trait JsonVisitor<'de>:
    ObjectVisitor<'de> + ArrayVisitor<'de> + StringVisitor<'de>
    + NumberVisitor<'de> + KeywordVisitor<'de> {}

/// Resume-support lives on a separate trait (Task 5).
pub trait ResumableVisitor<'de>: GrammarVisitor<'de> {
    fn restart_from_snapshot(&mut self, snap: &DtaSnapshot)
        -> Result<(), Self::Error>;
}
```

## Appendix B — Per-grammar per-visitor code-size projections

All projections at **25 B per Rust source line** of per-shape
emitted code — calibrated to sonic-rs density (30 B/line) rather
than walker density (4 B/line).

### B.1 JSON (6 rules, 100 % coverage)

| Rule | Shape | LOC | Bytes |
|---|---|---:|---:|
| value | dispatcher | 40 | 1 000 |
| object | Object | 180 | 4 500 |
| array | Array | 160 | 4 000 |
| string | String (SIMD kernel inlined) | 280 | 7 000 |
| number | Number (Eisel-Lemire inlined) | 260 | 6 500 |
| null | Keyword (scalar) | 40 | 1 000 |
| bool | Keyword (2-branch) | 60 | 1 500 |
| **Single visitor total** | | **1 020** | **25 500** |
| **Dual-visitor pair (+100%)** | | 2 040 | 51 000 |

Both visitors fit comfortably in any M-class L1 (128-192 KB).

### B.2 CSS L4 (~400 rules, 78 % shape coverage ⇒ ~80 shape fns + 85 walker residual)

| Shape category | Fns | Avg LOC | Bytes |
|---|---:|---:|---:|
| Object (ruleBlock, mediaBlock, …) | 8 | 280 | 56 000 |
| Array (selectorList, valueList, …) | 6 | 200 | 30 000 |
| String (strings, ident regexes) | 6 | 260 | 39 000 |
| Number (length, percentage, …) | 5 | 240 | 30 000 |
| Keyword (namedColor, globalKeyword, …) | 12 | 180 | 54 000 |
| Pratt (mathExpr, mathProduct) | 4 | 320 | 32 000 |
| Unordered (compoundSelector, …) | 6 | 380 | 57 000 |
| Other shape fns (misc) | 33 | 250 | 206 250 |
| **Shape-emitted total** | **80** | | **504 250** |
| Walker residual (85 fns @ old density) | | | ~150 000 |
| **Grand total single-visitor** | | | **~654 KB** |

L1 miss is structural; hot/cold partitioning reduces the hot
working set per §9.

### B.3 Sheets (~40 rules, 92 % coverage ⇒ ~18 shape fns)

| Shape category | Fns | Avg LOC | Bytes |
|---|---:|---:|---:|
| Pratt rungs (6-rung tower) | 6 | 280 | 42 000 |
| Keyword (error_literal, bool) | 3 | 180 | 13 500 |
| Number | 1 | 240 | 6 000 |
| String (ident, string_literal) | 3 | 260 | 19 500 |
| Array (array_literal) | 1 | 200 | 5 000 |
| Cell-ref / function-call | 4 | 300 | 30 000 |
| **Total single-visitor** | **18** | | **116 000** |
| **Dual-visitor** | | | 232 000 |

Single-visitor fits; dual-visitor overflows 128 KB L1, fits 192 KB
M4. Hot working set (~10 fns, ~80 KB) fits on all.

### B.4 BBNF (~35 rules, 75 % coverage ⇒ ~12 shape fns)

| Shape category | Fns | Avg LOC | Bytes |
|---|---:|---:|---:|
| Directive (keyword + PHF) | 1 | 280 | 7 000 |
| Rule / term | 3 | 280 | 21 000 |
| String (identifier, literal) | 2 | 240 | 12 000 |
| Regex leaf | 1 | 200 | 5 000 |
| Alt branches (shape-emitted) | 5 | 220 | 27 500 |
| **Total single-visitor** | **12** | | **72 500** |
| **Dual-visitor** | | | 145 000 |

Fits comfortably on all M-class variants.

## Appendix C — Empirical symbol sizes (nm measurements, binary
`.profiles/shared-target/release/deps/json_monolithic_value-ff9373572eef197d`)

| Symbol | Size (B) | Interpretation |
|---|---:|---|
| `__dta_walker_inline::run` (JSON) | 22 008 | bbnf's JSON walker — 51 states × 5 500 LOC @ 4 B/line |
| `__dta_walker_inline::run` (CSS L4, binary `css_l4-c82f8bbe86d63cb0`) | 157 556 | the L1 overflower |
| `bbnf_tape::driver::advance_or_pop_with` | 1 624 | un-inlined helper — AW-IV residual |
| `sonic_rs::parser::parse_object<DocumentVisitor>` (PaddedSliceRead) | 12 072 | hot symbol; 74% of sonic self-time on twitter |
| `sonic_rs::parser::parse_array<DocumentVisitor>` (PaddedSliceRead) | 10 012 | 79% on canada |
| `sonic_rs::parser::parse_string_escaped` | 688 | cold escape path |
| `sonic_rs::parser::parse_object<DocumentVisitor>` (Read) | 5 468 | secondary reader impl |
| `sonic_rs::parser::parse_array<DocumentVisitor>` (Read) | 10 836 | secondary |

Total sonic hot-path (PaddedSliceRead, twitter): 22 084 B — well
under L1. Target shape-emit parity: produce a JSON parser at
comparable density.
