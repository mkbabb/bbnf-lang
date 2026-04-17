# Memo: bbnf Automaton Ambitions vs. simdjson, sonic-rs, and Related Designs

Scope: primary sources only. "Primary" here means this repository's own design/test files for bbnf's stated ambitions, plus official papers/docs/repos for the external systems.

## Question

Does bbnf's generalized DTA/PSI direction theoretically beat recursive descent and the existing high-performance parser designs?

## bbnf's stated ambition

The repo's AV design says the target pipeline is:

- Stage A: a grammar-derived counter-DFA that "reads bytes once and emits the tape's full structural skeleton" plus a `PayloadJob` stream.
- Stage B: payload fill over those jobs.
- Stage C: prefix-scan finalization of `child_off` and `span_hi`.

Source:

- `docs/tranches/AV/AV.md:67-98`
- `docs/tranches/AV/research/06-psi-dta-parallelism.md:31-76`

The same research note defines the concrete DTA/PSI split:

- DTA: a grammar-derived structural miner, "not a parser," emitting compound/leaf skeleton records.
- PSI: one payload job per scalar leaf with a payload kind.

Source:

- `docs/tranches/AV/research/06-psi-dta-parallelism.md:37-46`

The repo's later AW synthesis also states that the current implementation is not yet the intended flattened form:

- "bbnf's DTA today is not that kind of automaton. It is a byte-driven tagged-union interpreter over `&'static [DtaState]`."
- It says the load-bearing fix is a walker-specialization pass plus a stage-1 SIMD structural bitmap pass.

Source:

- `docs/tranches/AW/research/SYNTHESIS-2-PATH-FORWARD.md:18-57`
- `docs/tranches/AW/research/SYNTHESIS-2-PATH-FORWARD.md:72-158`

The tests also confirm the repo expects large lifted automata, not tiny hand-specialized machines: JSON under 500 states, BBNF under 3000, Sheets under 2500.

Source:

- `crates/core/tests/dta_counter_states.rs:46-84`

## External designs, mapped to DTA/PSI

### simdjson

simdjson's paper says most parsers use top-down recursive descent, but simdjson instead uses two passes:

- Stage 1 validates encoding and identifies the starting location of all nodes and structural characters, writing those locations as integer indexes in a separate array.
- Stage 2 processes nodes and structural characters based on their starting character.

Source:

- https://arxiv.org/abs/1902.08318
- https://ar5iv.labs.arxiv.org/html/1902.08318#S3

The same paper says stage 1 is worthwhile even when it does extra work because it replaces unpredictable branches with SIMD and bitset processing; about half of total cycles per byte are spent in stage 1.

Source:

- https://ar5iv.labs.arxiv.org/html/1902.08318

Concrete DTA/PSI comparison:

- DTA resemblance: strong. bbnf Stage A is a grammar-specific generalization of simdjson stage 1 plus some stage-2 shape knowledge.
- PSI resemblance: partial. simdjson's stage 2 consumes the index and parses values; bbnf's PSI further splits payload decode from structural skeleton emission.

### sonic-rs

sonic-rs explicitly says it does not use "the two-stage SIMD algorithms from `simd-json`." Its README says SIMD is used mainly for long strings, float fractions, field access, and whitespace skipping. It also says sonic-rs is faster than simd-json in direct struct deserialization because sonic-rs parses directly into the Rust struct with no temporary tape-like structure.

Source:

- https://github.com/cloudwego/sonic-rs
- https://raw.githubusercontent.com/cloudwego/sonic-rs/main/docs/performance.md

Concrete DTA/PSI comparison:

- DTA resemblance: weak. sonic-rs is source-backed as a direct parser with selective SIMD acceleration, not a stage-1-index-plus-stage-2 machine.
- PSI resemblance: weak. sonic-rs emphasizes direct materialization and the absence of temporary structures.

### Mison

Mison's official description says it departs from FSM-based parsers, uses a two-level approach, builds structural indices, and that these indices convert control flow into data flow, largely eliminating unpredictable branches.

Source:

- https://www.microsoft.com/en-us/research/publication/mison-fast-json-parser-data-analytics/

Concrete DTA/PSI comparison:

- DTA resemblance: moderate at stage 1 only. Mison is the clearest primary-source statement of when structural indexing wins: when branchy token-by-token traversal is the bottleneck.
- PSI resemblance: low. Mison is selective/query-driven, not full grammar-driven skeleton emission.

### JSONSki

JSONSki says conventional streaming pays a character-by-character cost, then introduces "recursive-descent streaming," "structural intervals," and bit-parallel/SIMD fast-forwarding so irrelevant substructures can be skipped.

Source:

- https://doi.org/10.1145/3503222.3507719
- https://par.nsf.gov/servlets/purl/10323318

Concrete DTA/PSI comparison:

- DTA resemblance: limited. JSONSki keeps recursive-descent streaming and adds fast-forward APIs instead of flattening the whole parser into a generalized automaton.
- PSI resemblance: none. Its focus is selective skipping, not skeleton/payload fission.

## When stage-1 SIMD structural indexing is decisive

Inference from the sources:

Stage-1 indexing is decisive when all three conditions hold:

1. Structural discovery is a dominant cost.
   simdjson makes stage 1 a full pass because locating node starts, structural characters, quotes, and invalid whitespace is expensive but branch-averse. Mison states the same idea more directly: structural indices convert control flow into data flow.
2. The index is reused for downstream work.
   In simdjson, stage 2 consumes the precomputed positions. In Mison and JSONSki, bit-parallel structure metadata enables skipping or direct access instead of rescanning.
3. The parser cannot already fuse structure and payload efficiently into a type-specific direct path.
   sonic-rs explicitly chooses the opposite strategy: direct parsing with SIMD in selected hotspots and no temporary structure.

For bbnf, the repository's own ambition is stronger than simdjson's stage 1: not just structural indexes, but grammar-derived skeleton emission plus PSI (`docs/tranches/AV/AV.md:79-98`, `docs/tranches/AV/research/06-psi-dta-parallelism.md:37-46`). That only helps if the grammar fingerprint lets Stage A emit semantically meaningful skeleton records, not merely generic JSON structurals.

## When flattened automata beat recursive descent

Inference from the sources:

Flattened automata beat recursive descent when flattening removes generic control decisions rather than adding a runtime interpreter.

Source basis:

- simdjson avoids character-by-character recursive descent with a two-pass indexed design (paper §3).
- simdjson On-Demand says use-specific parsing avoids the branchiness of a generic type switch and makes code more inlineable and compact.
- sonic-rs says it parses directly into the target struct with no temporary structure.

So the winning property is not "automaton" in the abstract. The winning property is that structure discovery and type dispatch become specialized, predictable, and reusable.

In bbnf terms, that matches the repo's own later AW diagnosis: the desired win comes from a specialized emitted walker and a consumed stage-1 structural index, not from leaving the DTA as a generic runtime table interpreter (`docs/tranches/AW/research/SYNTHESIS-2-PATH-FORWARD.md:31-57`, `72-158`).

## When generalized automata lose

Source-backed repo diagnosis:

- The current bbnf DTA is a "byte-driven tagged-union interpreter" (`docs/tranches/AW/research/SYNTHESIS-2-PATH-FORWARD.md:24-29`).
- The AV PSI note identifies dispatch overhead, per-branch snapshots/rollback, and bookkeeping as a constant tax (`docs/tranches/AV/research/06-psi-dta-parallelism.md:25-29`).
- The same note says small inputs can lose because Stage A plus Stage C overhead dominates (`docs/tranches/AV/research/06-psi-dta-parallelism.md:113-119`).
- The tests show the generalized automata are expected to be large state machines, especially for BBNF and Sheets (`crates/core/tests/dta_counter_states.rs:46-84`).

Inference from those sources:

A generalized automaton loses when:

1. Each input byte still pays runtime state interpretation.
2. The machine carries savepoint/backtracking-style machinery for general grammar features.
3. The input is too small, or too payload-heavy, to amortize Stage A and Stage C setup.
4. The automaton is generic enough that specialization and inlining do not happen at the emitted-code level.

That failure mode is different from simdjson and sonic-rs. simdjson's stage split is narrow and JSON-specific. sonic-rs explicitly avoids the two-stage path and temporary structures altogether.

## Crisp answer

Inference from the sources: **No, a generalized automaton should not theoretically win here just by being generalized.**

What the sources support is a narrower claim:

- A **specialized flattened automaton** can win.
- A **stage-1 SIMD structural index** is decisive when structural discovery can be reused and when it converts branchy control flow into reusable dataflow.
- A **generalized runtime automaton** loses when it preserves runtime state dispatch, savepoint/backtracking overhead, and multi-state-per-byte interpretation.

So for bbnf, the theoretically winning path is not "generalized automaton" simpliciter. It is:

1. grammar-derived structural indexing where the grammar fingerprint provides real extra information, and
2. emitted, specialized walkers that erase runtime interpretation overhead.

That conclusion matches the repo's own stated evolution from AV's DTA/PSI ambition to AW's diagnosis that the present interpreter-shaped DTA is not yet the winning form.

## Source links

bbnf repository sources:

- `docs/tranches/AV/AV.md`
- `docs/tranches/AV/research/06-psi-dta-parallelism.md`
- `docs/tranches/AW/research/SYNTHESIS-2-PATH-FORWARD.md`
- `crates/core/tests/dta_counter_states.rs`

External primary sources:

- simdjson paper: https://arxiv.org/abs/1902.08318
- simdjson HTML rendering: https://ar5iv.labs.arxiv.org/html/1902.08318
- simdjson On-Demand design note: https://raw.githubusercontent.com/simdjson/simdjson/master/doc/ondemand_design.md
- sonic-rs README: https://github.com/cloudwego/sonic-rs
- sonic-rs performance note: https://raw.githubusercontent.com/cloudwego/sonic-rs/main/docs/performance.md
- Mison publication page: https://www.microsoft.com/en-us/research/publication/mison-fast-json-parser-data-analytics/
- JSONSki paper: https://doi.org/10.1145/3503222.3507719
- JSONSki accessible PDF mirror: https://par.nsf.gov/servlets/purl/10323318
