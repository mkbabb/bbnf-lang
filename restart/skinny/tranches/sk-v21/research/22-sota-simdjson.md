# 22 — SOTA frontier: simdjson lineage, 2025–2026 state

## 1. Current version and the On-Demand supersession of the tape

simdjson's release cadence through 2025 sits at the 4.x line: 4.6.0 (2025-03-30, adds overflow
protection, `get_int32`/`get_uint32`, memory-mapped file loading), 4.6.1 (2025-04-03, `padded_string`
view-length fix), 4.6.2 (2025-04-17, a breaking patch that removed On-Demand wildcard-match support
because the maintainers judged the feature's design flawed for wider adoption), 4.6.3 (2025-04-20),
4.6.4 (2025-05-06, 32-bit string-builder overflow hardening) (github.com/simdjson/simdjson/releases).
The project is still Lemire/Keiser-maintained and still under active correctness hardening rather than
new-architecture churn — the tape/DOM vs. On-Demand split, not a new stage-3 idea, is where 2025-2026
attention has gone.

The original two-stage design (Langdale & Lemire, arXiv:1902.08318, VLDB Journal 2019) built a fully
materialized **tape**: stage 1 emits a structural-character bitmap via SIMD classification
(`_mm256_cmpeq_epi8` family / NEON `vceqq_u8` equivalents → bitmask), stage 2 walks that bitmap as a
goto-based automaton with an explicit stack, validating structure and writing one tape record per
value (arXiv:1902.08318 §3; internal grounding at
`docs/tranches/AW/research/aw3-r1-simdjson-cycle-attribution.md:1-17`, which already carries the
per-stage regression fit `19·F + 11·S + 0.92·B` cycles/doc from the paper's §4.3).

**On-Demand supersedes the tape as the default frontend as of simdjson 1.0 (2021-09)** and is now the
primary API surface documented at github.com/simdjson/simdjson/blob/master/doc/ondemand_design.md and
formalized in Keiser, Lemire, et al., "On-Demand JSON: A Better Way to Parse Documents?" (Software:
Practice and Experience 54(6), 2024; arXiv:2312.17149). The architectural break is not a new stage-1/2
kernel — On-Demand still runs the identical SIMD structural-indexing preprocessing pass — the break is
that stage 2 is deleted as an eager materialization step and replaced by a **single forward-only
iterator over the pre-indexed text**, with five hard constraints: (1) a value is not parsed at all
until the caller converts it to a concrete type (`get_double()`, `get_string()`, …); (2) the iterator
only moves forward — arrays/objects cannot be re-iterated, a value can only be read once; (3) iteration
looks syntactically like ordinary DOM traversal (range-for over arrays/objects) so the API remains
ergonomic; (4) type-specific parse functions eliminate the generic type-switch/branch-misprediction cost
("type blindness") that dominates DOM tree walks; (5) validation is "pay for what you use" — untouched
subtrees are never validated, not just never materialized. Sibling-field skip is depth-tracked: to skip
an unwanted field the iterator scans forward counting nesting depth until it returns to the target
depth, using the same structural bitmap stage 1 already produced, rather than re-scanning bytes
generically (ondemand_design.md, per WebFetch 2026-07-05).

The paper's own numbers make the case concretely: on the Kostya benchmark On-Demand parses in 0.067 s
against 0.11 s for simdjson DOM (Rust serde comparator) — roughly 70% faster — and geomean throughput
across its task suite is 3.3–3.6 GiB/s for On-Demand vs. 1.9–2.1 GiB/s for full-tree DOM, at roughly 60%
of DOM's instructions/byte and about half of yyjson's (arXiv:2312.17149, Tables 1/3/4, per WebFetch
2026-07-05). The paper is explicit about where the technique stops paying off: `json2msgpack`, a task
that must materialize the *entire* document into another format, only gets 0.87–0.91 GiB/s — "On-Demand
is thus unable to make gains due to its ability to skip some content" when nothing is skippable. This is
the single most important lesson for a codegen'd parser generator: **On-Demand's entire margin over the
tape comes from *unread* substructure it never touches** — a generated JSON parser that always fully
materializes a value tree (as bbnf's tape/PSI path does today, per aw3-r1 §4 row "Lazy value decode")
gets none of this margin and should not expect to.

## 2. NEON/aarch64 stage-1 specifics

simdjson compiles a distinct implementation per architecture family (icelake/AVX-512, haswell/AVX2,
westmere/SSE4.2, arm64/NEON, ppc64, fallback) selected at either compile time or runtime dispatch
(simdjson.org/api/0.4.0/md_doc_implementation-selection.html). NEON is mandatory on essentially all
AArch64 cores, so it is the sole SIMD tier on Apple Silicon and the only one worth generating for the
project's Apple-M-only benchmark constraint.

Concretely (cross-verified against the existing internal grounding at
`docs/tranches/AW/research/aw3-r1-simdjson-cycle-attribution.md:54-63`, itself sourced from the Arm
Community Blog 2022 NEON-movemask note and dougallj's Firestorm microarchitecture notes): NEON has
**no `pmovmskb` and no `PCLMULQDQ`**, so simdjson's arm64 kernel must emulate both x86 primitives:

- Byte classification: `vceqq_u8` against each structural byte (`{}[]:,"\`), 128-bit lanes, 16 bytes at
  a time — latency 2, reciprocal throughput 0.25 on the Firestorm P-core (dougallj firestorm-simd.html).
- Movemask substitute: narrow each 16-bit compare-result lane down with `vshrn_n_u16(mask, #4)`, packing
  each matching byte into 4 repeated bits in a 64-bit extracted lane (`vget_lane_u64`), then trailing-
  zero-count and divide by 4 to recover a byte index — the standard NEON "poor man's movemask" pattern.
- Quote-run detection: with no CLMUL, the parallel-prefix XOR that flags "inside a quoted string" is
  emulated with a **6-instruction shift-XOR ladder** (`x ^= x<<1; x ^= x<<2; … x ^= x<<32`) per 64-bit
  in-string block — both simdjson's arm64 tree and sonic-rs's NEON backend use this identical idiom.
- Table-driven reclassification: `vqtbl1q_u8` (16-byte NEON table lookup) substitutes for x86's
  `vpshufb`-based low-nibble LUT collapse of multiple compares into one shuffle.

Published throughput: simdjson on Apple M1 hits ~7.8 GB/s in minify-only mode (no stage 2; simdjson
GitHub discussion #1658) and ~2.0–2.5 GB/s for a full twitter.json parse at ~3.2 GHz Firestorm — i.e.
stage 1 alone is 3-4× faster than the full pipeline, meaning **stage 2 (or, on the On-Demand path, the
value-materialization the caller actually asks for) is the dominant cost on Apple Silicon, not the SIMD
classification.** This directly reframes the priority for a codegen'd Apple-M target: winning stage 1 is
necessary but not sufficient; the walker/materialization path is where the real ceiling lives. This
repo's own from-source profiling of the real simdjson C++ binary (compiled locally, not simulated) at
`skinny/profile/simdjson-expanded/PROFILE-REPORT.md:98-107` confirms the split empirically per-corpus at
3.5 GHz: twitter 0.629 c/B stage1 / 0.377 c/B stage2, citm 0.420/0.312, but **canada (a numeric,
coordinate-heavy corpus) inverts the ratio to 0.547 stage1 / 1.847 stage2** — stage 2's number-parsing
cost alone dwarfs all of stage 1 by 3.4× on that corpus. That single row is the sharpest evidence in
this whole investigation that "beat simdjson" cannot mean "beat simdjson's stage 1"; it must mean beat
the value-materialization cost that stage 2 (or On-Demand's lazy equivalent) actually pays.

## 3. Derivative literature: JSONSki, Pison, Mison, GPU JSON

- **Mison** (VLDB 2017) is the ancestor idea: a JSON parser for analytical workloads that pushes
  projection/filter predicates *into* the parser so it can jump straight to a queried field's byte
  offset without full tokenization — the earliest "don't parse what you don't need" JSON technique,
  predating and motivating On-Demand's "pay for what you use" principle.
- **Pison** (VLDB 2021) parallelizes structural-index construction itself: rather than one thread
  scanning sequentially (as simdjson's stage 1 does per input), Pison builds structural bitmaps for a
  single large record across multiple threads using SIMD *and* multi-core parallelism together — a
  technique with no analog in simdjson's design at all, relevant to any bbnf backend targeting
  multi-core batch ingestion (e.g. a large JSON-Lines corpus) rather than single-document latency.
- **JSONSki** (SIGMOD 2022, dl.acm.org/doi/10.1145/3503222.3507719; github.com/AutomataLab/JSONSki)
  generalizes Mison's field-skip idea into a full bit-parallel "fast-forward" instruction set — five
  families of forwarding primitives (`goToObjAttr`, `goToAryElem`, `goOverObj`, `goOverPriAttr`,
  `goOverElem(K)`) compiled from a **query automaton built at initialization** from the caller's
  JSONPath query, so the skip logic is *specialized per query* rather than generic. It still uses
  PCLMULQDQ and AVX2 (256-bit) under the hood for its own structural classification but claims 3×
  Pison and 4×+ simdjson on large single records by skipping far more aggressively than simdjson's
  generic index ever can (JSONSki paper abstract, WebFetch 2026-07-05; requires pclmulqdq + g++7.4+,
  i.e. an x86-only technique as published — no NEON port is documented). The load-bearing idea for bbnf
  is **query-automaton-compiled skip functions** — this is structurally identical to what a grammar
  compiler already has available at codegen time (the grammar itself is the "query"), so a bbnf-
  generated parser could, in principle, emit JSONSki-style fast-forward code paths for grammar rules
  whose values are provably unread by the consumer, which is a strictly *more* information-rich
  position than JSONSki's runtime-compiled JSONPath automaton.
- **GPU JSON**: GpJSON (VLDB 2025, atlarge-research.com/pdfs/2025-vldb-gpjson.pdf) is the most recent
  entry — GPU-resident JSON parsing exploiting massive data-parallelism for structural indexing and
  value extraction. The PDF could not be text-extracted in this pass (binary/FlateDecode stream, no
  OCR available); its existence and VLDB-2025 venue are confirmed via the atlarge-research.com listing
  and cross-referenced ResearchGate entry, but no concrete technique from it could be verified
  first-hand within the time budget — flagged as a residual gap, not a finding.

## 4. What beating simdjson COLD actually requires

The published simdjson performance guidance (simdjson.org/api/1.0.0/md_doc_performance.html, WebFetch
2026-07-05) is unambiguous that simdjson's headline multi-GB/s numbers are **warm-parser, reused-buffer**
numbers, and says so explicitly: "make a parser once and reuse it"; "creating a new parser for each
document incurs unnecessary memory allocation overhead that can dominate parsing time, especially for
smaller documents," to the point that first-parse-without-reuse throughput can fall to **under 100
MB/s** — a >30× regression from the multi-GB/s steady state, driven entirely by allocation, not by the
SIMD kernels. simdjson's own internal buffers (`structural_indexes`, tape/string scratch) are grown
lazily to the largest document ever seen and never shrunk, meaning a long-running process that once saw
one large document pays that allocation's residency cost forever afterward — a classic warm-cache
amortization pattern that a genuinely *cold*, single-document, single-invocation benchmark (this
project's own binding constraint: "cold-only, loadavg-quiet, single-invocation") would never observe as
a benefit and would instead observe as unamortized fixed cost.

Concretely, "beating simdjson cold" therefore decomposes into three cost centers simdjson itself is
weak on precisely because its architecture assumes reuse:

1. **Startup/allocation cost.** simdjson allocates `structural_indexes`, tape buffer, and string-escape
   scratch sized to the (unknown until scanned) worst case, then grows monotonically. A generator that
   knows the grammar and can either (a) size buffers from a single length-based heuristic computed once
   per grammar (not per document) or (b) avoid a separate structural-index materialization entirely (the
   On-Demand path already partially does this — no full tape write — but On-Demand still allocates a
   string-unescape buffer and structural-index scratch per `iterate()` call) has a first-parse advantage
   simdjson structurally cannot claim without breaking its reuse-oriented API contract.
2. **First-parse-without-warm-branch-predictor cost.** simdjson's stage 2 / On-Demand walker is a tight,
   monomorphic hot loop *once the branch predictor has trained on the document's shape* — but a single
   cold invocation never gets that training. A codegen'd, grammar-specialized walker (per the AW-III.W5.6
   line already reasoned out at `aw3-r1-simdjson-cycle-attribution.md:150-155` and `:114-138`) sidesteps
   this differently: it removes the *branches* rather than warming a predictor around them, so it is one
   of the few techniques that is exactly as fast cold as warm, and is where the current codebase's own
   dispatch_one gap analysis is already correctly pointed.
3. **Materialization cost as the real stage-2/On-Demand bottleneck, not classification.** As shown above
   (§2, canada corpus: 1.847 c/B stage 2 vs 0.547 c/B stage 1; §1, On-Demand's own paper admitting
   json2msgpack sees minimal gains when everything must be materialized), the frontier's own numbers say
   plainly that number formatting/parsing and full materialization dominate whenever the *consumer*
   needs the whole document, which is the common case for a compiler-generated parser producing a typed
   AST/tape rather than a query-projection library. This means the JSONSki/On-Demand "skip what's
   unread" trick has limited relevance for bbnf's stated goal (full, rich, typed AST parity — cf.
   `feedback_preserve_rich_ast.md` in project memory) and the frontier lesson to actually import is
   narrower: **number parsing itself, and per-value write/store cost, not structural skipping, is where
   a from-scratch generator can plausibly beat simdjson cold**, because simdjson's own documented
   weakness (a "few hundred MB/s" ceiling on double-heavy corpora, `md_doc_performance.html`, and the
   canada-corpus 2.437 c/B measured locally) is specifically a materialization/number-parsing weakness,
   not a structural-scan weakness.

## 5. Known simdjson weaknesses (consolidated)

- **Floating-point-dense documents.** Accurate decimal-to-binary conversion with correct rounding is
  fundamentally serial per-value; simdjson's own docs recommend integers or ≤17-digit doubles and warn
  throughput on double-heavy corpora is "a few hundred MB/s," not GB/s (md_doc_performance.html). Locally
  measured: canada.json (coordinate-heavy) is 2.437 c/B inlined vs. 0.706–0.915 c/B for object/string-
  heavy corpora (github_events, apache_builds, update_center) — a 3+× penalty
  (`skinny/profile/simdjson-expanded/PROFILE-REPORT.md:80-94`).
- **Small/tiny documents.** Fixed stage-1 bring-up cost does not amortize; the locally-measured
  y_string_unicode corpus (35,601 bytes) shows 2.055 c/B, *worse* than corpora 15-30× larger
  (`PROFILE-REPORT.md:94`, and its own prose at line 196: "on tiny documents … stage1's fixed bring-up
  cost dominates and the absolute c/B is higher than corpora 100× larger").
- **Escape-heavy strings.** unicode_escapes corpus measured at 4.968 c/B inlined — 2.0× worse than the
  worst non-Unicode corpus (canada, 2.437) and 6.3–7× worse than the best (github_events, 0.706)
  (`PROFILE-REPORT.md:92,196`), because always-inline escape handlers don't compress into a tight loop
  and spill into `OUTLINED_FUNCTION_*` cold-path fragments (up to 30% of cycles on that corpus vs.
  1-12% elsewhere).
- **Allocation-dominated first parse.** Documented explicitly, see §4.1 above; sub-100 MB/s without
  parser reuse.
- **On-Demand's forward-only/single-read constraint** is a genuine API weakness (not a performance one):
  repeated key lookups or any need to re-read a value require either buffering by the caller or paying
  a rewind, and the design flaw serious enough that a wildcard-match feature was pulled in 4.6.2
  (2025-04-17) after the maintainers judged it unsound for the forward-only model
  (github.com/simdjson/simdjson/releases, 4.6.2 notes).
- **Validation deferred to first access**, which can surprise callers expecting whole-document upfront
  validation (arXiv:2312.17149, discussed weaknesses section).

## 6. Concrete techniques extractable for bbnf-generated parsers

1. **Type-specialized, non-dispatching value readers** (On-Demand's core trick): a grammar-specialized
   parser already knows, at codegen time, the exact type of every rule position — this is *more*
   information than On-Demand's runtime "ask for a double" contract, meaning a generated parser can emit
   the equivalent of On-Demand's type-specific number/string readers unconditionally, with zero runtime
   type dispatch at all, which On-Demand cannot fully achieve (it still branches once per value on
   caller intent).
2. **NEON shift-XOR quote-parity and vshrn-based movemask emulation** are concrete, portable, dependency-
   free intrinsic sequences bbnf's own SIMD-scan gap (flagged as ABSENT at
   `crates/core/src/generate/regex/emit/simd.rs:3-9` per aw3-r1 §4) can adopt directly for aarch64 without
   waiting on PCLMULQDQ-class hardware that doesn't exist on Apple Silicon.
3. **JSONSki's query-automaton-compiled skip functions** generalize cleanly to "grammar-rule-compiled
   skip functions" — for any grammar position provably unconsumed downstream (dead-value elimination
   already knowable from the IR), emit a fast-forward function instead of a full parse, which is strictly
   easier for a compiler to prove than for JSONSki's runtime JSONPath compiler.
4. **Materialization/number-parsing is the real lever for a cold, full-materialization target**, not
   structural SIMD scanning — the frontier's own numbers (On-Demand paper's json2msgpack result; the
   canada-corpus stage-2 dominance measured locally) say plainly that once bbnf commits to full typed-AST
   materialization (a standing project constraint, not up for debate), the ceiling is set by
   number-parsing and per-value store cost, which argues for prioritizing a fused, branch-free
   float/int decode + single-store tape-write path over further investment in stage-1 SIMD beyond what
   aw3-r1 already scoped as W5.5.
5. **Allocate once, size from the grammar not the document.** simdjson's monotonic-growth buffer
   strategy is precisely the anti-pattern a cold, single-invocation benchmark punishes; a generated
   parser that computes a fixed multiplier from the grammar's own shape (not simdjson's reactive
   grow-on-largest-seen policy) avoids the >30× cold-first-parse regression simdjson's own docs concede.

## Sources

github.com/simdjson/simdjson/releases (4.6.0–4.6.4, 2025-03-30 through 2025-05-06); arXiv:1902.08318
(Langdale & Lemire, VLDB Journal 2019); github.com/simdjson/simdjson/blob/master/doc/ondemand_design.md;
arXiv:2312.17149 (Keiser, Lemire et al., "On-Demand JSON: A Better Way to Parse Documents?", Software:
Practice and Experience 54(6), 2024); simdjson.org/api/0.4.0/md_doc_implementation-selection.html;
simdjson.org/api/1.0.0/md_doc_performance.html; dl.acm.org/doi/10.1145/3503222.3507719 and
github.com/AutomataLab/JSONSki (JSONSki, SIGMOD 2022); Mison (VLDB 2017) and Pison (VLDB 2021) per
JSONSki's own related-work framing; atlarge-research.com/pdfs/2025-vldb-gpjson.pdf (GpJSON, VLDB 2025 —
existence/venue only, content unextracted); simdjson GitHub discussion #1658 (M1 minify 7.8 GB/s);
dougallj firestorm-simd.html; Arm Community Blog 2022 (NEON movemask emulation) — all as previously
cross-cited at `docs/tranches/AW/research/aw3-r1-simdjson-cycle-attribution.md:1-197`; local from-source
profiling at `skinny/profile/simdjson-expanded/PROFILE-REPORT.md:1-107` and
`skinny/profile/native-sidecars/simdjson-cpp/`.
