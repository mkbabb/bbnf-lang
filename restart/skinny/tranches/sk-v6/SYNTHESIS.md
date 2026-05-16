# Grand Synthesis SK-V6: asmjson, DAV1D Discipline, And General SOTA Recovery

Date: 2026-05-15.

This document folds the SK-V6 twelve-agent research/profile pass into the
skinny and V1 authority set. It supplements, rather than replaces,
`GRAND-SYNTHESIS-SK-V5.md`: SK-V5 remains the substrate-history record; this
file is the current synthesis for beating same-plane JSON SOTA while preserving
grammar generality.

Inputs archived in `restart/skinny/tranches/sk-v6/research/`:

- A1 `skv6-A1-asmjson-generalization.md`
- A2 `skv6-A2-dav1d-asm-process.md`
- A3 `skv6-A3-comparator-planes.md`
- A4 `skv6-A4-history-validated-invalidated.md`
- A5 `skv6-A5-general-grammar-abstraction.md`
- A6 `skv6-A6-host-asm-instruction-map.md`
- B1 `skv6-B1-asmjson-challenge.md`
- B2 `skv6-B2-checkasm-hardening-plan.md`
- B3 `skv6-B3-profile-retained-three-way.md`
- B4 `skv6-B4-profile-direct-three-way.md`
- B5 `skv6-B5-primitive-gap-inventory.md`
- B6 `skv6-B6-spec-edit-map.md`
- C1 `skv6-C1-retained-profile.md`
- C2 `skv6-C2-direct-profile.md`
- C3 `skv6-C3-sidecar-planes.md`
- C4 `skv6-C4-host-asm-profile.md`
- C5 `skv6-C5-parse-that-gaps.md`
- C6 `skv6-C6-generality-costfacts.md`

## 1. What Has Been Done Hitherto

The skinny began as a falsifiable JSON-only prior on the V1 premise: one
grammar through the tape/direct substrate, generated and hand-coded tracks,
measured against sonic-rs and simd-json. The early eager-tape prototype proved
that an eager retained token stream capped out below SOTA; dispatch-table,
12-byte token, pair-fusion, structural-index sidecar, and related width churn
routes were measured and rejected in `skinny/REDRESS.md`.

The lazy offset-tape pass corrected the substrate direction. The structural
projection is now the tape: offsets plus flags are the committed document
identity, not a sidecar. That move validated the original twitter /
citm_catalog / canada triad as useful substrate evidence and preserved Lock 1
without introducing a second parse surface.

The expanded corpus then made the gate honest. SK-V5 landed generated
`SinkOnly`, `BackendShape`, `LayoutFacts.backend_shape`, `derive_backend_shape`,
vendored Eisel-Lemire, Canada structural scan restoration, strictness columns,
and the simd-scan/eventcursor purge. It also refuted the older Wave 3 UTF-8
fusion prescription across REDRESS 50-55. The current authority is
`skinny/RESULTS.md`: the full gate remains `N-direct / NoGo`, with retained
parse misses and direct typed-emission misses separated.

SK-V6 added two further corrections. First, strict-vs-strict comparator planes
must be recorded before any SOTA claim; asmjson's permissive behavior and the
current sonic-rs `utf8_lossy` feature prevent lazy same-plane assertions.
Second, the next close is not another scanner prepass. The hot profiles point
inside trusted string matching, generated direct materialization, and
grammar-neutral cost facts.

## 2. What asmjson Teaches

asmjson is not a generic parser generator. It is a JSON-specific DPDA
architecture: chunk byte-class masks, next-event seeking via bit operations,
direct-threaded finite control, and an explicit bounded object/array stack. It
uses a small set of ordinary instructions very well: compare bytes into masks,
combine masks, iterate the next set bit, and dispatch state directly.

The lesson to lift is not a new BBNF directive and not a JSON mode. The lift is
a grammar-neutral lowering shape:

- The grammar supplies byte-class tables, follow sets, stack policy, and output
  events through existing Grammar IR, side tables, and host/API schema facts.
- The cost model may select `CollapsedStage` when the recognizer proves a
  deterministic pushdown class, the ISA has admitted primitives, and the
  output plane can be consumed without losing diagnostics or strictness.
- The generated artefact is a per-grammar table set plus a wrapper over the
  Layer 1 primitive vocabulary. The generic crates do not learn JSON names.

asmjson is also not the same-plane Apple Silicon comparator. Its published
fast path is x86 AVX-512 and its permissive behavior accepts forms that the
strict JSON gate must reject. It remains an architectural reference and an x86
flaw-probe comparator until a same-strictness, same-output row exists.

## 3. What DAV1D / FFmpeg / VLC Teach

The DAV1D/FFmpeg/VLC lesson is process before hero assembly. Their SIMD wins
come from a small macro vocabulary, scalar executable specs, target-feature
dispatch, forced feature masks, checkasm parity, register-clobber checks, stack
canaries, cycle counters, and same-consumer admission. That discipline is
directly portable to bbnf.

The accepted bbnf pattern is:

- Layer 0: vendored target macro substrate such as x86inc-style helpers, kept
  read-only.
- Layer 1: grammar-neutral primitive contracts such as byte classification,
  prefix parity, next-set-bit, bounded stack push/pop, and bulk emission.
- Layer 2: codegen-emitted data tables and Rust/ASM shims selected by
  `BackendShape`.
- Layer 3: checkasm parity and corpus-row falsifiers before the primitive can
  enter a production dispatch table.

No primitive lands because it is elegant. It lands only with scalar parity,
ABI hardening, and a hot-path consumer that moves a named row.

## 4. Validated Research Plan Items

- The tape/direct union is the right substrate family. Eager retained tokens
  failed; offset tape plus projection survived the triad and composes with
  direct output.
- Structural scan is not the current limiter. Canada structural scan clears
  the NEON floor; remaining losses profile in string, dispatch, and direct
  materialization.
- Generated Track 1 must be real generated runtime. The older bench-private
  parser hid the gap; SK-V5 corrected it.
- Codegen and substrate costs are separable only when Track 1 and Track 2 are
  structurally different and profiled by symbol.
- Eisel-Lemire was vendor-and-wire. Number rows closed once the existing
  parse-that implementation was consumed.
- `BackendShape` as a five-shape taxonomy is the right abstraction. The work
  left is cost selection and materialization quality, not adding variants.
- DAV1D-style checkasm is mandatory for SIMD/ASM admission. Scalar parity alone
  without ABI and row impact is insufficient.
- Real typed output is the relevant direct-to-struct proof. The maximal digest
  stressor remains a guard, not the product plane.

## 5. Invalidated Or Demoted Items

- Eager tape as SOTA-beat substrate.
- Single-plan extraction as JSON-FAITHFUL without a measured cost model.
- Function-pointer dispatch tables, pair-token fusion, 12-byte token churn,
  separator elision, generic SWAR whitespace skipping, and sidecar
  event/structural-mask prepasses.
- Active Class A `match_tiny_plain_string` NEON wiring as the parse-G close.
  The scalar threshold can be a cost fact; the NEON kernel is not a broad fix.
- Broad UTF-8 fusion as the generated-baseline close. REDRESS 50-55 refute the
  family on current Track 1.
- Generic decoded visitors, sink-local decoded-stat helpers, quote-source
  streaming hash helpers, and parser-owned decoded scratch as direct closes.
- Strictness-obscured SOTA claims. `utf8_lossy` and permissive asmjson rows are
  flaw probes unless the row explicitly says otherwise.

## 6. Generalization Beyond JSON

The generalized unit is not "JSON string parser" or "JSON object parser". The
generalized unit is a fact set:

- `StructuralClassTable`: input byte to grammar class.
- `RecognizerRoute`: rule shape, first/follow disjointness, bounded stack need,
  layout policy, host-call boundary, and recovery requirements.
- `DirectFieldFacts`: field id, source path, target type, cardinality,
  duplicate/unknown policy, null/default policy, representation, materializer,
  and diagnostic context.
- `CostFacts`: selected and rejected alternatives with row-level evidence.
- `PrimitiveFacts`: scalar oracle, target ISA bodies, ABI status, feature mask,
  consumer, and corpus impact.

JSON supplies one instance of these tables. CSS, Sheets, and BBNF-self supply
different instances. Generic crates consume the tables; grammar-specific code
is confined to generated runtime modules, generated `.data`, grammar source,
and host/API schema facts.

## 7. Host-Architecture Route

Host primary is Apple Silicon arm64. The admitted practical instruction set is
NEON/AdvSIMD plus PMULL and CSSC-aware scalar bit operations where the compiler
or intrinsic path exposes them. SVE/SME remain research-only for this host
because they are not stable production assumptions in the current Rust target
surface.

The next arm64 work targets the measured hot sites:

- trusted quoted-span matching and escape-tail behavior inside the existing
  string matcher;
- direct string/Unicode materialization over field-layout facts;
- decimal span classify/materialize only where number rows regress again;
- table-driven byte-class classifiers and bit iteration where a row profile
  shows classification cost, not as a standing prepass.

x86_64 follows after the arm64 close. The x86 path is `CollapsedStage` over
strict grammar tables, not an asmjson clone. AVX-512 k-masks, VPCLMUL, VBMI,
GFNI, BITALG, and BMI next-bit iteration are admitted only where the scalar
contract and the grammar table prove the operation.

## 8. Current Diagnosis

The current retained parse profiles show three clusters:

- object/string-heavy rows where generated retained parsing spends time in
  `match_tiny_plain_string`, `match_string_at_quote`, `consume_container_next`,
  and key/colon handling;
- escape-heavy rows where `match_string_at_quote` dominates;
- scan-heavy claims are stale because Canada structural scan is already green.

The current direct profiles show generated direct loops and scalar
materialization, not a missing number algorithm. `unicode_escapes` is dominated
by `unescape_json_string` plus generated direct object value parsing. `numbers`
is at parity. `distinct_values` exposes array/object direct loop cost and
string-fold materialization.

The direct close route is therefore generated `DirectBuild` field-layout
materialization with host/API schema facts, not another checksum-only sink and
not a generic decoded visitor.

The C-pass tightens this further:

- `sonic-rs` strict anchors are invalid until the skinny bench is rebuilt
  without `utf8_lossy`; current lossy rows are flaw probes.
- retained escape work should first test a per-`\uXXXX` table/TBL classifier
  inside the existing retained string path. This is distinct from the rejected
  four-unit contiguous-run validator because it targets every unit, including
  short boundary-heavy strings.
- direct typed closure should expand generated `DirectBuild` with a `mesh`
  typed schema candidate before treating the digest stressor as product proof.
- host arm64 ISA work is measured only for NEON string-special/tail and direct
  unescape/materializer shape. PMULL, CSSC, DotProd, and SME/SVE remain
  unadmitted until exact profiles identify them.
- Lock 14 cleanup must include `parse-that-regex`; it is not just a
  `passes`/`codegen` cleanup.

## 9. Binding Path Forward

1. Correct comparator planes and schema metadata before claiming any SOTA row.
   Same-plane strictness, output, ownership, corpus, hardware, feature mask,
   and freshness become mandatory bench fields.
2. Harden `primitive-checkasm` to DAV1D/FFmpeg standard: forced feature masks,
   register clobber checks, stack canaries, cycle counters, and scalar oracle
   normalization.
3. Implement one retained-row intervention at a time from fresh profiles. The
   leading target is trusted string/escape matching; each candidate names rows,
   Mbps lift, and guard rows before code lands.
4. Implement generated typed `DirectBuild` field facts and materializers beyond
   the two current representative rows, starting with the C2 `mesh` typed
   schema candidate. The close is owned typed output, not a maximal digest
   shortcut.
5. Delete remaining grammar-name leaks in generic crates by replacing JSON
   helpers with grammar-derived class tables, recognizer facts, and
   `DirectFieldFacts`.
6. Run the full 17-corpus x workload matrix against same-plane sidecars.
7. Author x86 `CollapsedStage` only after the strict grammar-table and
   checkasm gates are green.

This is the path to beat sonic-rs, simdjson, yyjson, and asmjson in a way that
survives V1 graduation: strict planes first, grammar-neutral facts second,
measured primitives third, per-grammar collapsed assembly only where the cost
model proves it.
