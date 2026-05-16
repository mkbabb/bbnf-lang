# SK-V7 A2 — SOTA Strict-vs-Strict Beat Targets

Date: 2026-05-16. Cohort: SK-V7 (post-comparator-repair). Workspace read-only.
Companion: `/tmp/skv7-A1-comparator-repair.md` (to land) defines the strict
sonic-rs rebuild; this artefact reads the four SOTA parsers as a strict-vs-strict
beat-target matrix conditional on that repair, plus a concrete admission ledger
for SK-V7's Wave 3 successor work. Sources: skinny/RESULTS.md, skinny/REDRESS.md,
SK-V5/SK-V6 cohort, V9.5 PSI excavation, and primary upstream READMEs/docs.

---

## §1. Strict-vs-strict beat targets on M5 Max NEON

The four reference parsers do not all run as strict comparators on this host.
asmjson cannot run its AVX-512BW fast path on Apple Silicon (`SK-V5 A1:122-125`,
`SK-V6 A1:113-125`); its NEON-equivalent does not exist; only the SWAR floor
runs and is permissive-by-construction. The three live strict comparators are
sonic-rs (post-rebuild), simdjson NEON, and yyjson scalar. The fourth row,
asmjson, remains an x86 successor-tranche target with all M5 Max numbers
explicitly recorded as flaw probes per `skinny/RESULTS.md:224`.

| Comparator | Output plane (twitter) | M5 Max Mbps | c/B @ 4 GHz | Strict? | SK-V7 beat-target | Realistic close |
|---|---|---:|---:|---|---:|---|
| sonic-rs Value-DOM | arena DOM | 21184 | ~1.5 | yes (post-rebuild) | ≥23300 Mbps | T1 110% sonic strict |
| sonic-rs typed `from_str::<T>()` | direct struct | 15173 (digest), 11969 (typed) | ~2.0 | yes (post-rebuild) | ≥13166 Mbps typed | already PASS on twitter (151.5% per `RESULTS.md:28`) |
| simdjson NEON tape DOM | tape DOM | 2923 (per SK-V5 cohort B3) | 1.142 | yes by default | sub-1.0 c/B | ≥3200 Mbps |
| yyjson scalar (DOM-class leader) | typed-cell DOM | 3687 (per SK-V5 cohort B3) | 0.91 | yes by default | sub-0.85 c/B | ≥3900 Mbps |
| asmjson SWAR | flat DOM | 3315/2447 (synthetic) | ~1.2 | no (permissive) | flaw probe only | not a close |

The headline reading is that the post-rebuild sonic-rs row is the only same-plane
strict comparator on the typed product plane where bbnf-lang's V9.5-admitted
`real_typed_struct` workload already PASSes (151.5% sonic on twitter,
`skinny/RESULTS.md:28`). The DOM-class strict leader on M5 Max is yyjson at
0.91 c/B per `SK-V5 A1:73-77`, and that — not sonic-rs — is the cycle-budget
floor SK-V7 must beat on the retained-parse plane for twitter, citm_catalog,
and the eleven N-direct rows.

---

## §2. What each parser does on the typed product plane

The typed product plane is the workload that `skinny/RESULTS.md:25-46` calls
`real_typed_struct`: an owned-output typed Rust struct (a `Twitter`, a
`UpdateCenter`, etc.) materialised directly from JSON without intermediate DOM.
This is the SOTA gate that V9.5's Wave 3 admit landed for twitter at commit
`ab06ff11` (per `skinny/REDRESS.md:1944-1952`).

### sonic-rs `from_str::<T>()` typed direct

sonic-rs's typed path is its headline benchmark: twitter at ~695–826 µs
deserialisation, ~35% faster than simd-json's ~1.06 ms (per upstream README and
`SK-V5 A1:95-97`). The architectural choice is the load-bearing one: sonic-rs
**rejects simdjson's two-stage tape** and rewrites the SIMD algorithms to feed
direct value construction rather than a structural-index intermediary. The
field-by-field assignment dispatches via the `serde::Deserialize` trait, fully
monomorphised per `T`; the hot leaf on twitter is the field-name match arm
chain inside the generated `Visitor::visit_map` for each struct type. Internal
NEON/AVX-2 paths classify byte chunks and emit values directly into the visitor
sink. The cost model is single-pass; there is no retained DOM behind the typed
output.

### simdjson On Demand iterator

simdjson On Demand is a lazy forward-only iterator over the structural index
that simdjson Stage 1 produces. The contract is parse-once: "values can only be
parsed once" (per upstream md_doc_ondemand.html and `SK-V5 A1:49-53`). The
caller drives the iterator via `obj["field"]` / `array.at_pointer(...)`
accessors; values materialise only on access. The cost model is *Stage 1
amortised over selectively-accessed Stage 2 leaves*. Strict UTF-8 validation
happens at Stage 1 scan boundary. On Demand on twitter on M5 Max NEON runs at
~2923 Mbps tape DOM (per `SK-V5 A1:376-382`); the lazy On Demand variant is
not the load-bearing twitter number because twitter accesses substantially all
fields.

### yyjson `yyjson_doc_*` typed accessors

yyjson is single-pass scalar with no explicit SIMD (per upstream README and
`SK-V5 A1:67-87`). The `yyjson_obj_get(obj, "key")` accessor performs a linear
scan over the parsed key list; for small N this beats hashing. For nested
structs, the call chain remains scalar all the way down. The architectural win
is *fusion quality*: one C89 force-inlined function that fits in ≤20 KiB
i-cache (Lock 15 in `bbnf-lang` is a direct port of this discipline). On M5 Max
twitter, yyjson at 3687 Mbps beats simdjson DOM (2923) and sonic-rs Value-DOM
(2438) per `SK-V5 A1:73-77`. yyjson has no typed-direct path equivalent to
sonic-rs `from_str::<T>()`; the caller builds typed structs explicitly by
reading the document.

### asmjson on x86 AVX-512

asmjson upstream does not expose a typed-direct path. Its two output planes are
`parse_to_dom` (flat 64-bit DOM tape) and `parse_with` (SAX-style sink that
calls a caller-supplied trait), per `SK-V6 A1:74-86`. The typed product plane
must be assembled by the caller around the SAX sink. This is the principal
generality limitation that V9.5-PSI flagged: asmjson's hand-written DPDA reaches
flat-DOM emission per chunk but does not lower to field-typed visitors. The
asmjson architecture is therefore evidence for the DOM-class and structural-
scan plane only, not for the typed product plane that SK-V7 measures.

---

## §3. What each parser explicitly avoids

The taxonomy is grammar-direction-of-design: every parser is good at one thing
and explicitly rejects the others.

**sonic-rs avoids**: (a) the simdjson two-stage tape — SIMD scan feeds direct
materialisation, no structural-index sidecar (per upstream README and
`SK-V5 A1:142-145`); (b) one-output-model commitment — `Value` DOM, `LazyValue`
borrowed view, `RawNumber` lossless slice, and `from_str::<T>()` are four
first-class peers; (c) ad-hoc numerics — `RawNumber` preserves lossless numeric
text (Go `encoding/json.Number` style); (d) permissive strictness — UTF-8
validation is on unless caller types `_unchecked` (per upstream README).

**simdjson avoids**: (a) single-pass parsing — the two-stage decomposition is
load-bearing for the structural-index abstraction (per `SK-V5 A1:122-127`);
(b) eager DOM in On Demand — the iterator is forward-only and parse-once;
(c) permissive strictness — RFC 8259 with explicit UTF-8 validation at Stage 1
boundary, rejects unescaped control chars and noncharacter scalars; (d) "let
the compiler vectorize" — every ISA carries a hand-authored kernel because
portable C++ does not produce the necessary mask-arithmetic patterns.

**yyjson avoids**: (a) SIMD esoterica — no explicit vector intrinsics, no
runtime CPU dispatch, no per-ISA forks; (b) parsing-as-iteration — parse always
produces a complete tree; (c) mutation-in-parse-path overhead — immutable
parse and explicit `yyjson_doc_mut_copy()` separates the costs; (d) extension-
by-default — JSON5 (comments, trailing commas, NaN/Infinity) is opt-in flag;
(e) DOM/typed-direct fusion — there is no typed-direct path. yyjson is the
Lock 15 proof: i-cache discipline plus force-inline plus single-pass scalar
fusion beats SIMD when the SIMD implementation pays dispatch overhead or
stage-boundary memory traffic (per `SK-V5 A1:82-87`).

**asmjson avoids**: (a) portability — the AVX-512BW path has no graceful
degradation contract with the SWAR fallback (per `SK-V6 A1:113-125`); the
published 10.93 GiB/s is AVX-512 only; (b) strict control-character validation
— treats all bytes `<0x20` as whitespace, never scans string bodies for
unescaped controls (per upstream README:209-222); (c) grammar generality —
state alphabet and classifier mask sets are JSON-specific in ASM source;
(d) DOM-only commitment — the SAX path is a first-class peer; (e) typed-direct
output — the caller assembles types around the SAX sink. asmjson is the
*architecture* lift, not the strict comparator.

---

## §4. What bbnf-lang already beats

From `skinny/RESULTS.md` current state (commit `9eef728c` at SK-V6 close,
post-V9.5 Wave 3 admit), the rows where Track 1 (generated runtime parse) beats
sonic-rs by ≥110% strict-time ratio:

- **canada** parse 148.3% sonic (`RESULTS.md:7`) — Eisel-Lemire vendored close;
  canada is 99.4% numbers and `parse-that-regex`'s f64 fast-float path is the
  load-bearing leaf
- **citm_catalog** parse 130.3% sonic (`RESULTS.md:6`) — Wave 2 ContainerNext
  closes the object-of-objects shape; the OffsetTape projection consumed by an
  inline EventCursor inverts the simdjson stage-boundary cost on this corpus
- **mesh** parse 121.1% sonic (`RESULTS.md:11`) — array-of-numbers shape; same
  Eisel-Lemire leaf as canada
- **marine_ik** parse 136.0% sonic (`RESULTS.md:14`) — array-heavy small-int
  shape; the inline structural projection plus scalar fast-int close it
- **numbers** parse 148.0% sonic (`RESULTS.md:16`) — pure-numeric stressor;
  Eisel-Lemire ceiling

Plus four direct-to-struct PASS rows on representative product corpora:

- **apache_builds** direct 112.6% sonic (`RESULTS.md:31`)
- **github_events** direct 114.3% sonic (`RESULTS.md:32`)
- **citm_catalog** direct 99.3% sonic (PASS within 1.10× gate, `RESULTS.md:29`)
- **instruments** direct 93.5% sonic (PASS within 1.10× gate, `RESULTS.md:39`)
- **gsoc-2018** direct 177.6% sonic (`RESULTS.md:37`) — small-string-heavy

And the V9.5 Wave 3 admit added two `real_typed_struct` PASS rows on the
typed product plane:

- **twitter** real_typed_struct 151.5% sonic (`RESULTS.md:28`) — DirectBuild
  close; this is the load-bearing typed-direct PASS
- **update_center** real_typed_struct 99.2% sonic (PASS borderline,
  `RESULTS.md:34`)

The pattern is clear: bbnf already beats sonic-rs on number-heavy corpora
(canada, mesh, numbers, marine_ik), on the typed product plane where DirectBuild
field-fact lowering applies (twitter, update_center, apache_builds,
github_events, citm_catalog), and on object-of-objects retained parse
(citm_catalog). The remaining gap is in string-heavy retained parse and in
N-direct rows where typed DirectBuild has not yet been wired.

---

## §5. What bbnf-lang does not beat yet

Thirteen of seventeen parse rows in `RESULTS.md` classify G (NO-GO). The
remaining losses cluster by primitive shape:

| Corpus | Parse vs sonic | Direct vs sonic | Dominant leaf | Missing primitive |
|---|---:|---:|---|---|
| twitter | 73.6% | 78.4% | `match_tiny_plain_string` over field-key dispatch | TBL-driven tiny-string match + per-`\uXXXX` classifier |
| random | 65.5% | 85.8% | object/string heterogeneous key shape | per-`\uXXXX` TBL classifier inside retained string path |
| unicode_mixed | 56.1% | 74.6% | `unescape_json_string` plus mixed key/value escape | NEON Class B `\uXXXX` decode (Lock 16 admitted, not yet wired) |
| y_string_unicode | 46.0% | 59.3% | `\uXXXX` decode + surrogate-pair handling | NEON Class B `\uXXXX` decode at every unit |
| unicode_escapes | 80.4% | 58.5% | `unescape_json_string` dominates | NEON Class B `\uXXXX` decode at every unit |
| distinct_values | 60.2% | 53.7% | scalar string-fold materialiser | typed DirectBuild + NEON Class B |
| gsoc-2018 | 53.6% | 177.6% | retained parse dispatch bottleneck | retained-side string dispatch (direct is already strong) |
| apache_builds | 78.0% | 112.6% | retained-vs-direct asymmetry | retained-side ContainerNext is wired but slower than sonic SIMD |
| github_events | 68.8% | 114.3% | same | same |
| update_center | 59.6% | 89.3% | string-heavy retained parse | per-`\uXXXX` + retained string match |
| instruments | 92.0% | 93.5% | borderline; ContainerNext close | tighter container open/close fusion |
| unicode_basic | 91.7% | 129.4% | borderline retained, strong direct | minor; retained-side tighter |

The primitive bbnf does not have an equivalent of, by parser:

- **sonic-rs** has a NEON `vqtbl1q_u8`-driven tiny-string equality matcher that
  bbnf has admitted at Lock 16 (Class A `match_tiny_plain_string`) but not
  consumed in the hot retained dispatch — per `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md:135-137`
  the active wiring was demoted; what is missing is *scalar threshold as cost
  fact* plus NEON kernel only where the cost model picks it, not as a broad fix
- **simdjson** has the carry-aware `vpclmulqdq`-512 prefix-XOR for quote-state
  masking; bbnf has admitted `BITMAP_PREFIX_XOR_64` as Layer 1 macro (per
  `bbnf-simd/ext/x86/bbnf.asm:178-181`) but no NEON 16-byte equivalent that
  beats the scalar carry chain
- **yyjson** has aggressive force-inline plus single-pass scalar fusion of
  `\uXXXX` decode into the string-walk loop; bbnf's `unescape_json_string` is
  a separate function call across a function boundary — the SK-V7 close is to
  inline this into the parse hot leaf per Lock 15
- **asmjson** has the `tzcnt`-driven next-event seek pattern in the AVX-512
  primitive `BITMAP_NEXT_SET_BIT`; bbnf has admitted this macro but consumes
  it only at structural-scan emission, not inside the retained string path

---

## §6. The asmjson DPDA pattern adapted for arbitrary grammars

asmjson is a 9-state DPDA (deterministic pushdown automaton), not a pure FSM
(per FSM correctness audit in `V9.5-PSI-EXCAVATION/05-fsm-correctness.md` and
`SK-V6 B1:26-31`). The architecture decomposes:

1. **Finite control** — 9 named states (`ValueWhitespace`, `StringChars`,
   `KeyChars`, `KeyEnd`, `AfterColon`, `AtomChars`, `ObjectStart`,
   `ArrayStart`, `AfterValue`) per `SK-V6 A1:50-53` and asmjson `src/lib.rs:225-254`.
   In `bbnf.asm` this is the `FSM_DISPATCH_THREADED` primitive (macro #7 in
   `bbnf-simd/ext/x86/bbnf.asm:37-41`).
2. **PC-as-state direct threading** — `r10` holds the next-state target across
   chunk boundaries; no state-variable memory traffic; per `SK-V6 A1:60-63` and
   asmjson `asm/x86_64/parse_json_zmm_sax.S:73-77`.
3. **Explicit bounded stack** — `frames_buf[64]` plus `open_buf[64]` for
   bracket-pair tracking; per `SK-V6 A1:64-66`. In `bbnf.asm` this is
   `FRAME_PUSH_BOUNDED` (macro #8) and `FRAME_POP_BOUNDED` (macro #9).
4. **Mask-driven classifier** — 4 byte-class masks per 64-byte chunk
   (whitespace, quotes, backslashes, atom delimiters); `BYTE_CLASS_FROM_EQ_SET_64`
   in `bbnf.asm:139-142` is the literal asmjson primitive shape (`k`-way
   `vpcmpeqb` + `korq` reduction).
5. **`tzcnt`-driven seek** — `BITMAP_NEXT_SET_BIT` in `bbnf.asm:220-223`;
   asmjson uses this 18 times per the AVX-512 instruction histogram.

To generalise this beyond JSON, the grammar-neutral interfaces are:

- **State count fits 8 bits** — the cost model admits `CollapsedStage` only
  when grammar admits ≤256 states. JSON has 9; CSV plausibly 5-7; CSS L4 has
  recovery branches that explode the state count past the bound.
- **Finite first-set per state** — `BYTE_CLASS_FROM_EQ_SET_64` requires ≤8
  bytes per state; for grammars with larger alphabets, the cost model selects
  `BYTE_CLASS_FROM_TABLE_64` (256-byte LUT + `vpermb`/GFNI) instead.
- **Bounded nesting** — every grammar that admits `CollapsedStage` must declare
  a max depth; that becomes the size of `frames_buf` / `open_buf`. JSON's 64
  is a JSON-specific datum; CSV's is 1; CSS L4's is unbounded per
  `SK-V6 B1:90-105`.
- **CollapsedStage admission predicate** — per `SK-V6 B1:84-110`, the
  conjunction is: (a) byte-oriented deterministic recognizer; (b) explicit
  bounded stack model; (c) strict scalar reference exists; (d) primitive
  checkasm parity green; (e) DPDA parity probe green; (f) committed kernel
  template; (g) output plane matches the row; (h) hardware and feature mask
  declared. Failing any condition routes to `OffsetTape`/`EventTape`/`SinkOnly`
  with `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` diagnostic.

The V9.5 PSI binding is non-negotiable: Rust-emitted state machines pay a
dispatch ceiling that no SIMD/parallelism lever can amortise (per V9.5 PSI
synthesis §1.6, `V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md:74-78`). The
DTA's `dispatch_one` carried 20-35% self-time across every grammar. The fix
shape is hand-written NASM with primitive-macro composition, not Rust codegen.
This is the line separating the asmjson architecture lift from the asmjson
clone failure mode.

CSS L4 / BBNF-self / Sheets do not satisfy the admission predicates: CSS L4 has
`@error(recover)`, BBNF-self has parse-time host-fn decode, Sheets has Pratt
shape, all per `SK-V6 B1:32-40`. These grammars route through `EagerTape`,
`EventTape`, `OffsetTape`, or `SinkOnly` regardless of ISA. The shared
ASM spine is grammar-neutral only for grammars that satisfy the predicates;
JSON is the only currently-admitted instance, with CSV as a plausible second.

---

## §7. The DAV1D / FFmpeg / VLC reusable vocabulary

The discipline lifted from the dav1d/FFmpeg/VLC SIMD process (per
`GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md:86-107`) is *process before hero
assembly*. The transferable architecture is:

- **Layer 0** — vendored target macro substrate. dav1d's `x86inc.asm` plus
  `x86util.asm` are vendored at `skinny/crates/bbnf-simd/ext/x86/x86inc.asm`
  and `bbnf-simd/ext/x86/x86util.asm`. These supply the ABI layer: argument
  passing, register reservation, win64-vs-sysv prologue, AVX-512 zmm save,
  and the `cglobal` macro family. Kept read-only.
- **Layer 1** — grammar-neutral primitive contracts. `bbnf-simd/ext/x86/bbnf.asm`
  declares the nine macros (per `bbnf.asm:31-39` and `SK-V5 A1:235-238`):
  `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`,
  `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`,
  `EOB_PAD_CLAMP`, `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`,
  `FRAME_POP_BOUNDED`. Macro #2 has body per cohort SK-V6 B2 admit
  (`BYTE_CLASS_FROM_EQ_SET_64` + V6 admitted `bulk_emit_positions_64` and
  `structural_terminator_64`); the remaining eight are still contract
  declarations with macro bodies pending.
- **Layer 2** — codegen-emitted per-grammar data tables and Rust/ASM shims.
  Selected by `BackendShape` (per `SK-V5 A1:331-340`). Generated parsers carry
  a 256-byte classifier table and a state-transition table sized 9 × class_set
  for JSON.
- **Layer 3** — checkasm parity and corpus-row falsifiers before primitive
  admission. Per `SK-V6 B2`, the checkasm hardening plan adds: register-clobber
  detection, `rdtsc`/`rdtscp` cycle counters, stack-canary XOR-fold, signal-safe
  context-save baseline, `func_ref` vs `func_new` differential discipline.
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` is the consumer harness.

The discipline rule: no primitive lands because it is elegant. It lands only
with scalar parity, ABI hardening, and a hot-path consumer that moves a named
row in `RESULTS.md`. This is the line that distinguishes Lock 16 admission
from substrate-without-consumer (which is the failure mode that closed Era V).

The current Layer 1 status (commit `9eef728c`) is one macro of nine has body;
the SK-V7 Wave 3 admit is the remaining eight bodies plus the consumer
wiring — same-wave consumer is mandatory per Lock 14.

---

## §8. Concrete primitive admissions for SK-V7

Per cohort C1/C5 named in `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md:213-218` and
the V5/V6 admission ledger:

1. **Per-`\uXXXX` TBL classifier inside the existing retained string path**
   (C1/C5). Targets the four `\uXXXX`-dominated rows: `unicode_mixed`,
   `unicode_escapes`, `y_string_unicode`, and the unicode-bearing slice of
   `random` and `update_center`. Scalar reference exists in
   `parse-that-regex::unicode`; the NEON Class B kernel is admitted under
   Lock 16 (per `SOTA-BEAT-DESIGN.md §3.2.2`). The admission consumer is the
   retained-string match path — NOT a parser-owned scratch buffer — per
   `skinny/REDRESS.md:1212-1219`. Same-wave consumer rule: the NEON kernel
   ships with the retained-string call site updated to call it.
2. **mesh as next typed-DirectBuild product expansion** (C2). Per V9.5 Wave 3
   pattern: define `Mesh` typed struct, add `real_typed_struct` workload, run
   bench, gate at sonic-rs × 1.10 ns slack. This is the next product-plane
   typed expansion after twitter and update_center; per
   `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md:218`.
3. **NEON `BITMAP_PREFIX_XOR_64` body** (Layer 1 macro #3). Per
   `SK-V5 A1:250-254`, the AVX-512 body uses 512-bit `vpclmulqdq`; the NEON
   body falls back to scalar carry chain because NEON's PMULL64 over bitmap
   does not beat `clmul + eor` scalar at 16-byte width. The implementation
   is therefore a scalar-coded reference under Layer 1 contract; the consumer
   is the structural quote-mask production inside the inline OffsetTape
   projection.
4. **NEON `BULK_EMIT_COMPRESSED` body** (Layer 1 macro #5). NEON has no
   `vpcompressb` equivalent at 16-byte width; the admitted implementation is
   an indexed scatter via `vqtbl1q_u8` + `vqtbl1q_u8`-driven shuffle table.
   Consumer: bulk-emit of structural-byte positions during retained-parse
   scan.
5. **NEON `BYTE_CLASS_FROM_EQ_SET_64` body** (Layer 1 macro #2). Already has
   x86 AVX-512 body per cohort B2 admit. NEON body uses `vceqq_u8` ×N +
   `vorrq_u8` reduction. Consumer: same retained-parse classifier sites that
   currently scalar-classify.
6. **Per-rule cost-fact threshold for `match_tiny_plain_string`** (C5).
   Per `GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md:135-137`: the scalar
   threshold becomes a cost fact in `LayoutFacts`; the NEON kernel is
   selected only where the cost model picks it. This is the de-overfit
   move: not a broad fix, a measured admission.

Each primitive must satisfy Lock 16 (admissibility allowlist), produce a
scalar reference (executable specification), pass checkasm differential parity,
and ship with a same-wave consumer that moves a named row in `RESULTS.md`.

---

## §9. The CollapsedStage successor route on x86

Per the SK-V6 IMPLEMENTATION-PACKET §9 (which replaces SK-V5's §9), the x86
`CollapsedStage` route is the successor tranche after the arm64 same-plane
matrix closes. The sequence:

1. The arm64 NEON Wave 3 (per §8 above) lands and closes the thirteen
   parse-G rows plus the eleven remaining N-direct rows on M5 Max. This is
   the SK-V7 close gate. The `CollapsedStage` shape is unavailable on M5 Max
   silicon and does not contribute to this close.
2. After SK-V7 close, the per-grammar NASM authoring tranche opens for JSON
   only. The hand-authored kernel rides Layer 0 + Layer 1 macro vocabulary;
   per-grammar variation lives in two `.data` sections emitted by codegen
   (a 256-byte classifier table and a state-transition table sized 9 ×
   class_set). Total hand-authored ASM ≈ 1,400 LOC per grammar × ISA per
   `SK-V5 A1:415-419`.
3. The CollapsedStage admission predicate (per `SK-V6 B1:84-110`, see §6)
   gates per-grammar admission. If any predicate fails for the target grammar,
   the compiler emits `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` and falls back to the
   next eligible non-collapsed shape per `restart/ARCHITECTURE.md`.
4. The Zen 4 silicon access requirement is non-negotiable: per the V9.5 PSI
   synthesis §3 Risk C (`V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md:116-118`)
   and `WAVE-1-2-COHORT-DIGEST.md:232-241`, the Phase 4 gate (twitter T1 ≥ 7400
   MiB/s on x86_64) cannot be measured without equivalent silicon. Without
   silicon, the close criterion collapses to "compiles successfully" which is
   the substrate-first-consumer-later signature.
5. The projected impact arithmetic per `SOTA-BEAT-DESIGN §5.7` is twitter on
   Zen 4 AVX-512 VBMI2 at ~0.35 c/B total → ~12.8 GB/s, beating asmjson's
   10.93 GiB/s headline at *matched strictness*, not at asmjson's permissive
   default. This is a target, not a claim; the gate is measurement on
   equivalent silicon with matched strictness and matched output plane.

The CollapsedStage row, when it lands, must record `automaton_class = dpda`,
`collapsed_stage_admissible = true`, `grammar_cluster`, `state_count`,
`class_count`, `max_depth`, `stack_policy`, `strict_layout_mask`,
`invalid_body_mask`, `dpda_parity_probe = pass`, `primitive_checkasm = pass`,
`kernel_source = committed_template`, and `s_anchor_eligible = true` per
`SK-V6 B1:166-185`. Rows missing these fields are invalid for S-anchor
selection.

---

## §10. Single largest-impact beat target

The largest-impact beat target for SK-V7 on M5 Max NEON is **the per-`\uXXXX`
TBL classifier inside the existing retained string path** (§8 admission #1).

Justification: of the thirteen parse-G rows in `RESULTS.md`, four are dominated
by `\uXXXX` decode (`unicode_mixed` at 56.1%, `y_string_unicode` at 46.0%,
`unicode_escapes` at 80.4%, the unicode slice of `random` at 65.5%) and another
five are dominated by string-match cost where the same primitive applies as
a sub-leaf (`update_center`, `apache_builds`, `github_events`, `twitter`,
`gsoc-2018`). One admission moves nine of thirteen parse-G rows. The scalar
reference exists; the NEON kernel is Lock-16-admitted under Class B; the
consumer is the unmodified retained-string match path; the same-wave consumer
rule is satisfied trivially.

The second-largest target is the **mesh real_typed_struct workload** (§8
admission #2): one row from G to PASS on the typed product plane.

The third-largest is **NEON `BYTE_CLASS_FROM_EQ_SET_64` body** (§8 admission
#5): consumed at multiple retained-parse classifier sites; lifts the
`ContainerNext`-bottlenecked rows (`apache_builds`, `github_events`,
`instruments`) toward parse-row PASS.

---

## §11. Sources cited

In-tree authoritative inputs:

- `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md` (lines 1-224)
- `/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/SK-V5-COHORT/skv5-A1-comparative.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/SK-V6-COHORT/skv6-A1-asmjson-generalization.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/SK-V6-COHORT/skv6-B1-asmjson-challenge.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/V9.5-PSI-EXCAVATION/06-go-no-go-synthesis.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/SOTA-BEAT-DESIGN.md`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/ext/x86/bbnf.asm` (lines 31-263)
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/ext/x86/x86inc.asm` (vendored dav1d Layer 0)

External authoritative anchors (WebFetched 2026-05-16):

- sonic-rs README: `https://github.com/cloudwego/sonic-rs/blob/main/README.md`
  — typed `from_str::<T>()`, `Value` arena DOM, `LazyValue` borrowed view,
  `RawNumber` lossless slice; rejects simdjson two-stage tape; twitter ~695 µs
  unchecked vs simd-json ~1.06 ms.
- simdjson On Demand 0.8.0: `https://simdjson.org/api/0.8.0/md_doc_ondemand.html`
  — parse-once iterator contract, forward-only restriction, "values can only
  be parsed once", lazy materialisation via Stage 1 structural index.
- yyjson README: `https://github.com/ibireme/yyjson` — C89 scalar, no explicit
  SIMD, force-inline single-pass, RFC 8259 strict default + JSON5 flag opt-in,
  immutable/mutable doc split, EPYC 1.72 GB/s parse, iPhone A14 2.39 GB/s.
- asmjson docs.rs: `https://docs.rs/asmjson/latest/asmjson/` — AVX-512BW + SWAR
  routing, 10.93 GiB/s single-thread DOM (Zen 4), 26.6 GiB/s Rayon-parallel,
  permissive control-character treatment per upstream contract.

JSON strictness reference:

- RFC 8259: `https://www.rfc-editor.org/rfc/rfc8259` — four whitespace bytes
  (space, HT, LF, CR); strings must escape `"`, `\`, and C0 controls U+0000
  through U+001F.

End of report.
