# SK-V5 A1 — Comparative Architectural Audit of Four SOTA JSON Parsers

Date: 2026-05-13. Scope: asmjson, simdjson, yyjson, sonic-rs — read for what
to lift into `bbnf-simd`, `parse-that-regex`, the generated codegen, and the
runtime tape. Output strictly read-only against tracked files; this file lives
at `/tmp/skv5-A1-comparative.md` for downstream cohort folding.

The framing throughout is *lift the strengths, avoid the weaknesses*. Each
parser is good at one thing and explicitly bad at another. The temptation to
"adopt simdjson" or "adopt asmjson" wholesale is the failure mode the V9.5 PSI
excavation already named — Rust codegen cannot recover what asmjson buys with
hand-written AVX-512, and simdjson's two-stage pipeline carries a stage-ratio
inversion on escape-heavy and number-heavy corpora that bench averaging hides.
The four parsers are studied as a vocabulary of independent design moves, not
as monolithic templates.

## §1. Architectural shapes

### asmjson (x86-64, AVX-512BW primary; SWAR floor)

asmjson is a *collapsed-stage* parser: classification, finite-control state,
and output writes happen inside one mask-driven loop that walks the input 64
bytes per chunk via AVX-512BW. The published 10.93 GiB/s on Zen 4 single-thread
DOM and 26.6 GiB/s across Rayon tasks are the headline; both numbers require
AVX-512BW silicon and the hand-authored 2,641-LOC ASM monolith
(`parse_json_zmm_dom.S` and `parse_json_zmm_sax.S`). On hosts without AVX-512BW
the runtime CPUID selector routes to a portable SWAR fallback whose throughput
is a real floor, not a stub. The state carrier is the program counter — each
state has its own classifier mask set (V/O/K/D/C/S/F/R/A in the published
state alphabet) and dispatches through `r10` holding the next-state target
across chunk boundaries; no state-variable memory traffic. The output model is
a flat 64-bit DOM tape (one TapeEntry per value) for `parse_to_dom_zmm` or a
SAX-style sink driven by a caller-supplied writer for `parse_with_zmm`. The
hot-leaf shape is the asymptote of fusion: one giant kernel, ten `vpcmpeqb`
plus six `korq` per chunk per the instruction histogram in the AVX-512 sources,
eighteen `tzcnt` sites for next-set-bit seeking, EOB padding so the classifier
loop sees zeros past true input. SIMD footprint is minimal but maximal in
fluency: AVX-512BW byte comparison and k-mask reduction, `kmovq`, `tzcnt`,
mask-driven `vmovdqu8` stores. No GFNI, no VPCLMUL, no VBMI2 esoterica per the
published docs — the architecture is the win, not the instruction selection.

### simdjson (x86-64 + arm64; two-stage tape, on-demand iterator)

simdjson is a *two-stage* parser. Stage 1 is a SIMD structural-character pass
that produces an index of positions of `{}[]"\,:` plus quote-state bookkeeping;
the carry-aware prefix-XOR construction for in-string masking via `pclmulqdq`
(or its 512-bit `vpclmulqdq` extension on Ice Lake+) is its load-bearing
trick. Stage 2 is a recursive-descent walker over the structural index that
constructs either the *tape DOM* or, in the On-Demand API, advances a forward
iterator that parses values only as the caller accesses them. The tape is an
array of 64-bit words; each entry is `('c' << 56) + x` where `c` is an ASCII
discriminator (`tfnlud"{}[]r`) and `x` is a 56-bit payload — strings live on a
separate buffer indexed by the payload of `'"' << 56` entries, numbers occupy
two 64-bit words (discriminator + raw 64-bit value), containers carry a count
in the high 32 bits of the payload and a pointer-back from the closer to the
opener. On-Demand exposes `ondemand::object` and `ondemand::array` as
borrowed forward-only iterators with the contract *values can be parsed once*
— rewinding is illegal. Stage carrier is the structural index array (one u32
per structural byte). SIMD footprint is wide and architecture-specific: AVX-2
`vpshufb` + `vpcmpeqb` on baseline x86, AVX-512BW + `vpclmulqdq` 512-bit on
Ice Lake+, NEON `vqtbl1q_u8` + `vshrn_n_u16` movemask on arm64. Runtime CPU
dispatch chooses one implementation at parser construction. The hot-leaf shape
is split: stage 1 lives in one kernel per ISA, stage 2 lives in a recursive
descent that reads through the structural index.

### yyjson (pure scalar; single-pass; mutable/immutable DOM split)

yyjson is the counter-example: ANSI C89, *no explicit SIMD*, single-pass
recursive descent that writes either an immutable `yyjson_doc` or a mutable
`yyjson_mut_doc`. The published 1.72 GB/s on AWS EPYC and 2.39 GB/s on iPhone
A14 prove that scalar fusion plus i-cache discipline plus aggressive force-
inlining produces a parser that on M5 Max measures 3687 MiB/s on the twitter
corpus DOM-class — beating simdjson DOM (2923) and sonic-rs Value-DOM (2438)
on this host per `skinny/profile/native-sidecars/PROFILE-REPORT.md`. State
carrier is the LLVM call stack: the parser is conventional recursive descent,
no explicit state machine, with one large fused inner function that fits in
~18 KiB of i-cache (single hot leaf). The output model is a tape-shaped DOM
of typed value cells; the mutable/immutable split is a load-bearing API
discipline — parsing always returns immutable, modification requires an
explicit `mutable_copy` call, eliminating mutation-cost from the parse path.
Strictness is RFC 8259 by default with opt-in JSON5 extensions
(`YYJSON_READ_ALLOW_COMMENTS`, `YYJSON_READ_ALLOW_TRAILING_COMMAS`,
`YYJSON_READ_ALLOW_INF_AND_NAN`); duplicate keys are accepted and the original
order is preserved. yyjson is the Lock 15 proof point: i-cache residency plus
force-inline plus single-pass scalar fusion beats SIMD when the SIMD
implementation carries dispatch overhead, stage-boundary memory traffic, or
unfused recursive descent.

### sonic-rs (Rust; lazy borrowed views + direct-to-struct; SIMD adapter)

sonic-rs is a Rust port-and-rewrite of sonic-cpp with borrowed SIMD algorithms
from simdjson and yyjson. The pipeline is single-pass but offers three output
models in parallel: a `Value` DOM (mutable, arena-backed), `LazyValue` (raw
JSON byte slice retained as a borrow, parsed only when accessed), and
`RawNumber` (lossless number preservation in the Go `encoding/json.Number`
style). The direct-to-struct path via `serde` integration is the headline
benchmark — twitter deserialization at ~695-826 µs vs simd-json at ~1.06 ms.
SIMD usage is "Faster in x86_64 or aarch64, other architecture is fallback
and maybe very slower" — the published material does not enumerate instruction
sets but the source carries AVX2, AVX-512, and NEON paths. State carrier
varies by output mode: scalar recursive descent for DOM, structural-index plus
deferred parse for LazyValue. The architecture's most distinctive choice is
*not* committing to simdjson's two-stage tape — sonic-rs explicitly rewrote
the SIMD algorithms to produce direct values rather than a tape intermediary.
This is the lever that produces the direct-to-struct headline. UTF-8
validation is on by default (`xx_unchecked` APIs are explicit opt-out);
trailing commas, duplicate keys, and NaN/Infinity are not loudly addressed in
the published README.

## §2. What each parser explicitly avoids

asmjson **rejects portability**: there is no graceful-degradation contract
between the AVX-512BW path and the SWAR fallback; the published throughput is
the AVX-512 number alone. asmjson **rejects strict control-character
validation**: it treats every byte with value `<0x20` as whitespace per its
own docs and does not scan string bodies for unescaped controls. asmjson
**rejects grammar generality**: the state alphabet and the classifier mask
sets are JSON-specific, embedded in the ASM source, and not derived from any
grammar description. asmjson **rejects DOM materialization as the only output
model** — the SAX path with caller-supplied sink is a first-class peer.

simdjson **rejects single-pass parsing**: the two-stage decomposition is
load-bearing for the structural-index abstraction. simdjson **rejects eager
DOM construction in On-Demand**: the iterator is forward-only and parse-once
to avoid the cost of full materialization. simdjson **rejects permissive
strictness**: the parser is RFC 8259 with explicit UTF-8 validation;
unescaped control characters and noncharacter scalars are rejected at scan
time. simdjson **rejects "let the compiler vectorize"**: every ISA carries a
hand-authored kernel because portable C++ does not produce the necessary
mask-arithmetic patterns.

yyjson **rejects SIMD esoterica**: no explicit vector intrinsics, no
runtime CPU dispatch, no per-ISA forks — the codebase is one C89-compatible
source. yyjson **rejects the parsing-as-iteration pattern**: parse always
produces a complete tree, even if immutable. yyjson **rejects mutation-in-
parse-path overhead**: the immutable/mutable doc split forces callers who
need mutation to pay a separate copy cost rather than burdening the parse
hot path with mutability bookkeeping. yyjson **rejects extension-by-default**:
JSON5-style features (comments, trailing commas, NaN/Infinity) require
explicit flags and never enter the strict default code path.

sonic-rs **rejects the simdjson two-stage tape**: the SIMD work feeds direct
materialization rather than a structural-index intermediary. sonic-rs
**rejects "one output model"**: Value DOM, LazyValue borrowed view, and
direct-to-struct via serde are three first-class peers. sonic-rs **rejects
ad-hoc number handling**: `RawNumber` preserves lossless numeric text for
roundtrip cases where `f64` is insufficient. sonic-rs **rejects permissive
strictness**: UTF-8 validation is on unless the caller types out
`xx_unchecked`.

## §3. Strictness and output-plane matrix

| Dimension                              | asmjson                          | simdjson                          | yyjson                              | sonic-rs                          |
| -------------------------------------- | -------------------------------- | --------------------------------- | ----------------------------------- | --------------------------------- |
| Unescaped control chars in strings     | accepted (treated as whitespace) | rejected at scan                  | rejected (strict RFC 8259)          | rejected (UTF-8 check on)         |
| Noncharacter scalars U+FDD0..FDEF      | passthrough                      | rejected                          | rejected                            | rejected                          |
| Noncharacter scalars U+xFFFE/U+xFFFF   | passthrough                      | rejected                          | rejected                            | rejected                          |
| Invalid UTF-8 (where caught)           | not validated                    | scan boundary (stage 1)           | parse boundary                      | scan + view boundary              |
| Trailing comma                         | not specified                    | rejected                          | flag-opt-in (`ALLOW_TRAILING_COMMAS`) | rejected                        |
| Duplicate keys                         | passthrough (last wins typical)  | passthrough                       | accepted with preserved order       | accepted                          |
| NaN / Infinity                         | not specified                    | rejected                          | flag-opt-in (`ALLOW_INF_AND_NAN`)   | rejected by default               |
| Comments                               | rejected                         | rejected                          | flag-opt-in (`ALLOW_COMMENTS`)      | rejected                          |
| Output plane (primary)                 | flat 64-bit DOM tape             | tape DOM + on-demand iterator     | immutable typed-cell DOM            | direct-to-struct + Value + Lazy   |
| Output plane (secondary)               | SAX sink                         | (none — tape and OD only)         | mutable DOM via explicit copy       | `RawNumber` lossless slice        |
| String storage                         | inline in tape                   | separate string buffer            | inline or arena                     | borrowed (Lazy) or copied (Value) |
| Strictness posture overall             | permissive                       | strict-by-default                 | strict-by-default + opt-in JSON5    | strict-by-default + opt-out      |

The cell that matters most for the bbnf-lang comparison plane is
*Unescaped control chars in strings*. asmjson's 10.93 GiB/s is measured
against a permissive scanner that does not pay for control-character
validation; simdjson, yyjson, and sonic-rs all do pay for it. Any honest
"BEAT asmjson" claim must specify whether the comparison runs under matched
strictness — bbnf-lang's measurement plane validates UTF-8 at scan boundary
and rejects unescaped controls, so the apples-to-apples comparator is asmjson
in its permissive default minus the validation cost. SOTA-BEAT-DESIGN §6.4
already records this honestly.

## §4. Generic lift table

The classifier below routes each architectural feature to one of three
destinations: GENERIC (admissible into `bbnf-simd`, `parse-that-regex`, or
generated codegen because it generalizes across grammars), GRAMMAR-SPECIFIC
(lives in the grammar definition or in codegen-emitted `.data` tables, not in
the generic crates), or REJECTED (do not adopt — names why).

| Feature                                   | Origin            | Classification     | Destination                                |
| ----------------------------------------- | ----------------- | ------------------ | ------------------------------------------ |
| 64-byte byte-class mask via TBL/PSHUFB    | asmjson, simdjson | GENERIC            | `bbnf-simd` Layer 1 `BYTE_CLASS_FROM_*`    |
| Carry-aware prefix-XOR quote mask         | simdjson          | GENERIC            | `bbnf-simd` Layer 1 `BITMAP_PREFIX_XOR_64` |
| `tzcnt` next-event seeking in mask        | asmjson           | GENERIC            | `bbnf-simd` Layer 1 `BITMAP_NEXT_SET_BIT`  |
| EOB tail padding via zero-mask load       | asmjson           | GENERIC            | `bbnf-simd` Layer 1 `EOB_PAD_CLAMP`        |
| `vpcompressb` mask-driven byte emission   | asmjson, simdjson | GENERIC            | `bbnf-simd` Layer 1 `BULK_EMIT_COMPRESSED` |
| Bounded explicit bracket stack            | asmjson           | GENERIC            | `bbnf-simd` Layer 1 `FRAME_*_BOUNDED`      |
| PC-as-state direct-threaded dispatch      | asmjson           | GRAMMAR-SPECIFIC   | per-grammar `.asm` kernel (`CollapsedStage`) |
| JSON state alphabet (V/O/K/D/C/S/F/R/A)   | asmjson           | GRAMMAR-SPECIFIC   | JSON grammar definition only               |
| Structural-index intermediate array       | simdjson          | GENERIC            | runtime tape `OffsetTape` projection       |
| On-demand forward-only value iterator     | simdjson          | GENERIC            | runtime `EventCursor`                      |
| Tape entry discriminator-in-high-byte     | simdjson          | GENERIC            | runtime tape entry layout                  |
| Separate string-tape buffer with len hdr  | simdjson          | GENERIC            | runtime tape string-payload model          |
| Immutable parse / mutable explicit-copy   | yyjson            | GENERIC            | runtime tape view discipline               |
| Single-pass force-inline scalar driver    | yyjson            | GENERIC            | codegen lowering for `OffsetTape`          |
| ≤20 KiB i-cache budget for fused hot leaf | yyjson            | GENERIC            | codegen (Lock 15 enforcement)              |
| Direct-to-struct via serde sink           | sonic-rs          | GENERIC            | codegen `SinkOnly` lowering                |
| Lossless number text preservation         | sonic-rs          | GENERIC            | `parse-that-regex` number primitive        |
| Borrowed `LazyValue` deferred parse       | sonic-rs          | GENERIC            | runtime `EventCursor` over offset tape     |
| AVX-512BW `vpcmpeqb`+`korq` reduction     | asmjson           | GENERIC            | `bbnf-simd` x86_64 AVX-512 path            |
| NEON `vqtbl4q_u8` + `vshrn` movemask      | simdjson NEON     | GENERIC            | `bbnf-simd` aarch64 Class A primitive      |
| TBL-driven `\uXXXX` hex decode            | (novel)           | GENERIC            | `bbnf-simd` aarch64 Class B primitive      |
| Permissive control-char acceptance        | asmjson           | REJECTED           | Lock 16 strictness clause forbids          |
| JSON-specific constants in primitive crate | asmjson          | REJECTED           | violates Lock 14 (zero overfit)            |
| Single-monolithic-ASM-source pattern      | asmjson           | REJECTED           | dav1d two-layer factoring saves ~half LOC  |
| Runtime function-pointer dispatch table   | (Rust attempt)    | REJECTED           | V9.5 PSI: LLVM-owned match lowering wins   |
| Hand-written Rust state machine           | (Rust attempt)    | REJECTED           | V9.5 PSI: LLVM cannot compile away overhead |
| Sidecar event-cursor producer prepass     | (sk-v3 attempt)   | REJECTED           | regressed; event-cursor is lowering boundary |
| Stage 1 universally dominant assumption   | simdjson          | REJECTED           | escape/number corpora invert stage ratio   |
| Explicit SIMD as the only fast path       | simdjson, asmjson | REJECTED-AS-AXIOM  | yyjson scalar beats both on twitter M5 Max |

Three rejection rows deserve emphasis. The *V9.5 PSI excavation* result —
that Rust-emitted automaton overhead exceeds what LLVM can compile away while
Rust recursive descent compiles into an implicit automaton via call-stack-as-
parse-state — is the load-bearing argument that bbnf-lang's `CollapsedStage`
admissibility lives only in hand-authored NASM. The *sidecar event-cursor
producer prepass* rejection is the lesson from the sk-v3 prototype: a
mask/LUT producer bolted in front of an unchanged `parse_value_at` regressed
and grew the hot hub. The *stage 1 universally dominant assumption* is the
specific simdjson lesson that escape-heavy and number-heavy corpora invert
the stage ratio — bench averaging hides this.

## §5. Concrete primitives to lift, per bbnf-lang crate

### bbnf-simd Layer 1 (grammar-neutral SIMD primitive macros, dav1d-style)

The Layer 1 vocabulary is already named in `restart/skinny/audit/SOTA-BEAT-
DESIGN.md` §5.2 and partly implemented at HEAD `9eef728c`. The nine primitive
macros — `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`,
`BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`,
`EOB_PAD_CLAMP`, `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`,
`FRAME_POP_BOUNDED` — already cover the asmjson primitive footprint plus
simdjson's prefix-XOR. The lifts specific to the comparative reading here:

From **asmjson** lift the `BYTE_CLASS_FROM_EQ_SET_64` shape — the `k`-way
`vpcmpeqb` plus `korq` reduction is asmjson's actual classifier shape, not the
table-LUT shape simdjson tends toward; both belong in the Layer 1 vocabulary
so the cost model can select per-grammar. Lift `EOB_PAD_CLAMP` verbatim as the
canonical end-of-buffer discipline — over-allocate one vector width and mask-
zero the tail so the classifier sees zeros (which lie outside every grammar's
interesting-byte set). Lift the 18-call `tzcnt`-as-next-event seek pattern as
the canonical mask-traversal vocabulary.

From **simdjson** lift `BITMAP_PREFIX_XOR_64` as the carry-aware quote-state
mask construction; this is the structural pre-pass the simdjson stage 1 turns
on. AVX-512 body uses `vpclmulqdq` at 512-bit lane width (Lock 16 admissibility
"AVX-512 VPCLMULQDQ"); NEON body falls back to scalar carry chain because
NEON's PMULL64 over bitmap does not beat `clmul + eor` scalar at this width.
Lift the runtime CPU-dispatch pattern (one selection per parser construction,
inlined per-parse-call) as the canonical Layer 1 ↔ Layer 2 binding shape.

From **sonic-rs** lift the NEON `vbcaxq_u8`/`veor3q_u8` ternary bitwise
primitive — sonic-rs uses this for fused class-or-reductions where simdjson
uses two-step `vorrq_u8` chains.

From **yyjson** lift nothing primitive-level — yyjson is the proof that the
absence of Layer-1 primitives is sometimes the best Layer-1 primitive
strategy. But lift its *consumer discipline*: yyjson's single fused 18-KiB
hot function is the i-cache budget benchmark every Layer-1 primitive's
consuming kernel must meet.

### parse-that-regex (scalar primitives — string, number, unicode)

From **sonic-rs** lift the `RawNumber` discipline: lossless number text
preservation, exposed as a primitive that returns the byte slice plus a
parsed numeric value, allowing callers who need bit-exact roundtrip to
recover the original lexeme. The materializer belongs in `parse-that/number`
as the *exact* number primitive that the codegen `SinkOnly` emitter calls
when the grammar's `->` declares numeric materialization.

From **yyjson** lift the scalar number parser shape: single-pass digit-block
consumption with mantissa accumulation in a u64, exponent fused, no
intermediate string copy. yyjson's number parser is the scalar reference the
`parse-that-regex` `number` module should match; the `fast-float`-style
SIMD-digit-block extension goes in `bbnf-simd` only when the cost model
selects `canada`-shape corpora.

From **simdjson** lift the `\uXXXX` hex-decode primitive shape — both as
scalar (`parse-that-regex/unicode`) and as NEON TBL primitive (`bbnf-simd`
Layer 1 Class B per SOTA-BEAT-DESIGN §3.2.2). The scalar reference must be
the executable specification (Lock 16 admission discipline).

From **asmjson** lift nothing into `parse-that-regex` — asmjson's primitives
are all 64-byte SIMD shapes, not scalar leaf materializers.

### codegen lower (lowering shapes and call-site emission patterns)

From **simdjson** lift the *structural-index intermediate* as the
`OffsetTape` projection — but emitted *inline* with the typed event cursor,
not as a separate stage 1 array that stage 2 walks. The V9.5 lesson is that
the simdjson two-stage decomposition's cache-miss cost can dwarf the
structural-index win on small documents; emitting `OffsetTape` as a lazy
projection consumed by the same recursive-descent driver preserves the lift
without paying the stage-boundary cost. simdjson's `find_field` ordered
lookup is the lift for codegen field-resolution — emit a sorted-field hash
or a perfect hash from grammar facts.

From **sonic-rs** lift the `SinkOnly` lowering pattern: the direct-to-struct
path bypasses any retained DOM. Codegen emits `parse_into<T: Sink>` where
`Sink` is a trait with method calls per accepted event (`begin_object`,
`field_str`, `value_i64`, etc.) — the generated parser produces values
directly without retaining identity. This is the lever that closes the
`N-direct` row in `skinny/RESULTS.md`.

From **asmjson** lift the *direct-threaded dispatch through `r10`* but only
as the codegen pattern for hand-authored `CollapsedStage` `.asm` kernels — do
*not* emit this as Rust-codegen. V9.5 PSI is explicit: Rust-emitted explicit
automaton overhead exceeds what LLVM can compile away. The lift is the
template the per-grammar `.asm` author follows, not the Rust codegen target.

From **yyjson** lift the *force-inline + single-pass + ≤20 KiB hot leaf*
discipline as the codegen lowering invariant. Generated parsers must compile
under `lto=true codegen-units=1 panic="abort"` with `#[inline(always)]` on
the recursive descent core. The Lock 15 enforcement at `skinny/Cargo.toml`
already lands this.

### runtime tape (event cursor model, structural projection union)

From **simdjson** lift the tape entry encoding for the `EagerTape`
projection: 64-bit cells, discriminator in the high byte, payload in the low
56 bits. Container entries carry a count and a pointer-back from the closer
to the opener, enabling O(1) skip-children traversal. Strings live on a
separate buffer with a length header. This is the canonical layout for
`OffsetTape` and `EagerTape` shapes per the BackendShape taxonomy.

From **simdjson On-Demand** lift the *forward-only single-pass iterator
contract* for the `EventCursor` abstraction. Per the published docs: "values
can only be parsed once" — rewinding is illegal. This is the contract every
`EventCursor` consumer must respect. The `OffsetTape` projection feeding an
`EventCursor` is the bbnf-lang analog of simdjson On-Demand consuming the
stage 1 index; the structural projection is the tape, not a retained sidecar.

From **yyjson** lift the *immutable parse / mutable explicit copy*
discipline. The runtime tape exposes views as immutable; any mutation path
goes through an explicit copy that costs at call time, not at parse time.
This eliminates mutation-cost bookkeeping from the hot parse loop.

From **sonic-rs** lift the *three-output-modes-from-one-parse* posture: the
runtime tape projection feeds `EagerTape`/`OffsetTape`/`SinkOnly` materializations
selected by `LayoutFacts.backend_shape`, but the parse machinery itself is
single. `LazyValue` is the bbnf-lang `EventCursor` over the retained
`OffsetTape`; `Value` is the `EagerTape` retained view; direct-to-struct is
`SinkOnly`. One parse, three consumer planes.

From **asmjson** lift the *flat 64-bit tape* layout for `EagerTape` —
this overlaps the simdjson lift but with asmjson's specific discipline that
each entry is one TapeEntry written directly from the classifier kernel
without intermediate buffering. The asmjson tape is the SAX-style sink
written into a tape buffer rather than into a caller writer.

## §6. What we should beat on M5 Max NEON (16-byte width)

The M5 Max host is the SK-V4 close condition per SOTA-BEAT-DESIGN §6.4.
The three live comparators on this host are sonic-rs, simdjson, and yyjson;
asmjson cannot run on arm64 silicon, so it is *not* a close-condition
comparator on M5 Max.

**sonic-rs NEON path**: their twitter c/B sits around 2.3 with the Value-DOM
output mode at ~2438 MiB/s on the M5 Max profile. The lift to beat sonic-rs
is the direct-to-struct lowering plus exact number/string/Unicode
materializers per §5. sonic-rs's NEON SIMD algorithms are borrowed from
sonic-cpp/simdjson; bbnf-lang's lift is the same Lemire 2019 `vqtbl4q_u8` +
Validark 2024 movemask construction already admitted under Lock 16 §3.2.1.
The differentiator is the codegen `SinkOnly` plus the typed-event-cursor
consumption — sonic-rs pays for serde's column-major descent into struct
fields, bbnf-lang's codegen emits the field-descent inline from grammar
facts.

**simdjson NEON port** (the simdjson arm64 implementation): twitter c/B
1.142 at ~2923 MiB/s. The lift to beat simdjson NEON is the *fused single-
pass* shape that avoids simdjson's stage 1 / stage 2 cache-boundary cost on
small documents. simdjson's NEON path uses `vqtbl1q_u8` for the structural
class plus the scalar prefix-XOR construction (NEON has no `vpclmulqdq`
analog at 16-byte width that beats scalar `clmul + eor`). bbnf-lang's lift
emits the structural projection inline with the recursive-descent driver,
eliminating the stage-2 cache miss into the structural-index array.

**yyjson scalar**: twitter c/B 0.91 at 3687 MiB/s on M5 Max — *the current
M5 Max DOM-class leader*. The lift to beat yyjson is *fusion quality plus
admitted NEON primitives consumed inside the fused leaf*. yyjson runs one
huge hot function with zero SIMD; bbnf-lang's codegen must produce the same
fused hot function shape (Lock 15: ≤20 KiB i-cache, force-inline) plus the
NEON Class A/Class B primitives at the actual hot inner loops (tiny-string
recognition, `\uXXXX` decode). The arithmetic per SOTA-BEAT-DESIGN §6.2
Phase 2 target: T1 ≥ 2375 MiB/s strict gate, with Phase 3+ pushing past
yyjson's 0.91 c/B by exploiting NEON for the inner loops yyjson cannot use.

The close discipline is *per-corpus generalization*: the gate must hold
within 2× of twitter c/B on `unicode_escapes.json` (the escape-pathology
bound; simdjson's 4.97 c/B on unicode_escapes vs 1.14 on twitter is the
upper-bound ratio). bbnf-lang's parser must not blow up worse than 2.28 c/B
on unicode_escapes if it hits 1.14 on twitter — escape-heavy corpora are
where bench averaging hides regressions.

## §7. What we should beat on x86 AVX-512 (asmjson successor)

The x86 lane is the successor-tranche target per SOTA-BEAT-DESIGN §7. It
runs on Zen 4 / Ice Lake / Sapphire Rapids silicon, not on the M5 Max host;
the SK-V4 close on M5 Max is independent of any x86 measurement.

**asmjson AVX-512BW**: 10.93 GiB/s single-thread DOM, 26.6 GiB/s Rayon-
parallel on Zen 4. The honest comparison plane subtracts the cost of strict
control-character validation, noncharacter-scalar rejection, and UTF-8
boundary checking — asmjson does none of these. With strictness matched,
asmjson's effective throughput on a strict corpus is lower than the headline
number; bbnf-lang's successor target is to beat asmjson *at matched
strictness*, not at asmjson's permissive default. The lift is the hand-
authored `CollapsedStage` NASM kernel riding the dav1d-vendored Layer 0
`x86inc.asm` and the grammar-neutral Layer 1 `bbnf.asm` (per
SOTA-BEAT-DESIGN §5.2). Per-grammar variation lives in two `.data` sections
emitted by codegen — a 256-byte classifier table and a state-transition
table sized 9 × class_set. Total hand-authored ASM ≈ 1,400 LOC across Layer
1 macro and per-ISA primitive bodies, vs asmjson's 2,641-LOC monolith.

**simdjson AVX-512**: 2923 MiB/s on twitter DOM at 1.142 c/B (the same
NEON-port number; AVX-512 should be higher but the published M5 Max profile
is NEON-only). simdjson AVX-512 uses `vpclmulqdq` 512-bit for prefix-XOR
plus `vpshufb` for structural class. The lift past simdjson AVX-512 is the
asmjson collapsed-stage architecture *plus* the Lock 16 esoterica simdjson
does not exploit: GFNI `vgf2p8affineqb` for affine-encodable class sets
(structural set `{}[],:` is affine-encodable per Lock 16), AVX-IFMA
`vpmadd52` for mantissa accumulation in number parsing, VNNI `vpdpbusd` for
fused digit-block reductions, BITALG `vpshufbitqmb` for bit-mask reorder,
VBMI2 `vpcompressb` for bulk byte emission.

**sonic-rs AVX2**: the published M5 Max number is NEON; the AVX2 path is
not separately measured in the bbnf-lang profile corpus. sonic-rs AVX2 uses
the simdjson algorithm family in Rust; the lift past sonic-rs AVX2 is the
direct-to-struct `SinkOnly` lowering plus the `CollapsedStage` AVX-512
escalation when grammar facts admit it.

The projected impact arithmetic (SOTA-BEAT-DESIGN §5.7): twitter on Zen 4
AVX-512 VBMI2 at ~0.35 c/B total → ~12.8 GB/s, beating asmjson's 10.93 GiB/s
headline. This is a target, not a claim; the gate is measurement on
equivalent silicon with matched strictness and matched output plane. The
successor-tranche LOC budget per SOTA-BEAT-DESIGN §5.4 is ~1,400 LOC of
hand-authored ASM plus ~200 LOC of Phase 3 AVX-512 VBMI2 Rust primitives
plus ~600 LOC of Phase 4 collapsed-stage codegen.

## §8. Sources cited

External authoritative anchors (all WebFetched 2026-05-13):

- asmjson docs.rs: <https://docs.rs/asmjson/latest/asmjson/> — pipeline
  description, AVX-512BW + SWAR routing, 10.93 GiB/s single-thread, 26.6
  GiB/s Rayon, permissive control-character treatment per its own published
  contract.
- simdjson On-Demand 0.8.0: <https://simdjson.org/api/0.8.0/md_doc_ondemand.html>
  — forward-only iterator contract, parse-once discipline, borrowed view
  types.
- simdjson Tape 2.0.0: <https://simdjson.org/api/2.0.0/md_doc_tape.html>
  — 64-bit tape entry layout `('c' << 56) + x`, separate string buffer,
  container count + pointer-back encoding.
- sonic-rs README: <https://github.com/cloudwego/sonic-rs> — three output
  modes (Value / LazyValue / RawNumber), serde-direct path, twitter
  ~695-826 µs deserialization, lineage from sonic-cpp/serde_json/sonic/
  simdjson/yyjson.
- yyjson README: <https://github.com/ibireme/yyjson> — C89, no explicit SIMD,
  immutable/mutable doc split, RFC 8259 strict default, JSON5 flags opt-in,
  EPYC 1.72 GB/s parse, iPhone A14 2.39 GB/s parse.
- FFmpeg checkasm.h doxygen: <https://www.ffmpeg.org/doxygen/7.1/checkasm_8h.html>
  — `func_ref` vs `func_new` differential discipline, `declare_func` macro
  family, `bench_new` perf instrumentation, signal-safe context-save baseline.

In-tree authoritative inputs (bbnf-lang `restart/skinny/audit/`):

- `ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md` (353 LOC, HEAD `9eef728c`) — SK-V4
  amendment establishing five-shape BackendShape, two-layer ASM vocabulary,
  asmjson demotion to x86 successor, per-grammar `.asm` admissibility
  conjuncts.
- `SOTA-BEAT-DESIGN.md` §5.2 (lines 334-353) — nine Layer 1 primitive macros
  with NEON / AVX-2 / AVX-512 body specifications.
- `SOTA-BEAT-DESIGN.md` §6.4 (lines 469-484) — M5 Max comparator anchors;
  yyjson 0.91 c/B; simdjson DOM 1.142 c/B; sonic-rs Value-DOM ~2.3 c/B;
  per-corpus generalization gates.
- `SOTA-BEAT-DESIGN.md` §5.7 (lines 409-426) — projected ~0.35 c/B Zen 4
  AVX-512 VBMI2 twitter total, ~12.8 GB/s aspiration.
- `skinny/profile/native-sidecars/PROFILE-REPORT.md` — measured comparator
  numbers on M5 Max twitter DOM-class.
- `skinny/RESULTS.md` — current N-direct / NoGo gate with 4 parse G rows
  and 11/17 direct red rows.
- V9.5 PSI excavation cohort under `restart/skinny/audit/V9.5-PSI-EXCAVATION/`
  — Rust-codegen-automaton rejection result; CollapsedStage admissibility
  only as hand-authored NASM.

Source-line attribution for in-tree primitives: `bbnf-simd::ext/x86/x86inc.asm:1-1978`
(dav1d vendored ABI layer); `bbnf-simd::ext/x86/bbnf.asm` (Layer 1 macro
vocabulary, skeleton at HEAD `9eef728c`); `bbnf-simd::tests/checkasm_parity.rs:1-516`
(differential parity harness, FFmpeg checkasm discipline transposed to Rust).
