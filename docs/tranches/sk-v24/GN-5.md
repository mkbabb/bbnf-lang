# GN-5 — GENERALITY AND THE FLEET

The tranche that proves the generator serves the grammar plane, not five hand-tuned
grammars wearing one facade. It re-earns the CSV number under an honest profile, adds a
fifth grammar (TOML) authored from nothing and wired through the one generator, excises the
last name-gate residue from the JSON emitter behind a falsifiable CI grep-gate, and hoists
the const-mask residualization from a CSS-only template into a shared codegen seam that every
grammar-derived stop-set travels. At close there are five grammars, one generator, zero
grammar-name predicates in codegen — measured, not asserted.

---

## §1 Charter

### Mission

Demonstrate generality by construction and by measurement. Three grammars ship the greenfield
tree at GN-1 (json, css, sheets); a fourth (csv) rode PROVE-13's structural cures into GN-1
under a directional profile. GN-5 raises the fleet to five under one generator, re-proves the
one directionally-measured lane (CSV) on the honest fat-LTO profile, and installs the
structural guarantee — a CI gate that fails on any grammar-name predicate in codegen — that
makes "no overfitting" an enforced invariant rather than a slogan. Every wave lands
production-grade: public bench and oracle surfaces, error handling, corpus fixtures, and a
consumer inside the same wave.

### Precept bindings (sk-v23 research/07 codex)

- **Precept 8, 38, 40 (Y.13 consumer-in-wave):** each wave below names the runtime caller,
  oracle, or bench that reads its emitted decision in the same wave. No substrate lands here
  without its consumer — the master pathology (14/18 dead substrate) does not recur.
- **Precept 9 (no overfitting):** W3 is the overfit cure by definition; W5 hunts residue.
- **Precept 3, 5 (transposition over patching, one path):** W4 hoists `delim_mask` into one
  shared residualization seam consumed by every stop-set emitter, collapsing the CSS-only
  template copy and the general-descent `matches!` copies into a single lowering.
- **Precept 7, 10 (no silent deferrals, wire-before-retire):** PROVE-13's disclosed
  `json_sink_direct` residue is not "future work" — W3 owns it. The residual is retired by
  cure, in this tranche, with a named gate.
- **Precept 14, 15, 16 (no god modules; directory modules; tests outside src/):** the oracle
  fleet lives in one directory-module with per-grammar submodules — the crate is born in W1
  (csv lane), grown in W2 (toml), and completed in W4 (the scattered surface folded in); tests
  in `tests/`.
- **Precept 19 (a hard gate needs an artefact; grep-only is supplementary):** W3's grep-gate
  is paired with a runtime falsification test that plants a name-gate and asserts the gate
  trips — the gate is proven able to fail before it is trusted to pass.
- **Precept 43–48 (the profiling standing law):** every number below is cold per-parse, paired
  per-sample interleaving, PMU `ri_cycles` cyc/B certified, ins/B the load-proof floor, N≥200,
  multi-window, per-block loadavg recorded. Ratios certified, absolutes directional.
- **Precept 53 (generated files regen-only, loc budget):** the TOML and CSV generated dirs
  regenerate bit-for-bit; a per-wave generated-LOC budget binds.

### Refutation guards (SPEC §0 laws that constrain this tranche)

These are the ways GN-5 could cheat, foreclosed by name:

- **No digit-scan SIMD (02 NO_GO).** CSV/TOML field scans are byte-class delimiter scans. The
  bulk-lane cost lever in W1 fuses delimiter scans; it never touches digit parsing or feeds
  Mula/Lemire. Any wave proposing a SIMD digit path is out of charter.
- **No block-scheduling pursuit (04 refuted).** The W1 bulk-row shape is a coarser tape
  granularity plus a movemask-narrow delimiter stitch — NOT multi-block software pipelining.
  The M-class core already pipelines the single-block loop; a bulk claim resting on block
  scheduling is refuted before it is measured.
- **No shim perf claims (09 refuted).** No `portable_simd`→NEON shim number appears here. New
  kernels, if any, are aarch64 NEON with a scalar reference; stability is the only value a
  shim may claim, and this tranche introduces no shim.
- **Fold-over-cursor is dead.** The CSV and TOML recognizers are recursive descent over the
  offset tape; no lazy-cursor fold reappears (2.3–4.9× behind, measured, binding).
- **Tape-navigation is not the get lane.** W4's oracle fleet round-trips via full tape
  materialization for parity; it never markets tape navigation as a get path (06 measured
  navigation at 150–1700× the stop-early cost).
- **Warm benches are disinformation.** Every table is cold-per-parse, paired, N≥200. The
  challenge wave rejects any warm figure on sight.

### Supersessions

- PROVE-13's `ax-iter` directional 0.948× vs csv-core is SUPERSEDED by W1's fat-LTO release
  re-proof. The directional number is retired the moment the release number lands; it never
  anchors a performance claim in the durable docs.
- The `csv-prove` throwaway harness (skinny `crates/csv-prove/`, deliberately dep-light for the
  prototype) is SUPERSEDED by the productionized CSV lane inside the consolidated oracle fleet
  (W4). The prototype crate is deleted, not carried.
- PROVE-13's route cure (`dispatch_alt_count > 0 && direct_shapes.len() >= 2` shape predicate
  at `runtime_generator.rs`) lands in GN-1 as part of the CSV port; GN-5 does not re-land it.
  GN-5.W3 supersedes only the disclosed *internal* residue (`json_sink_direct/mod.rs:98`).

---

## §2 Interlocks

### Depends-on

- **GN-1.W-final** (the loom). GN-5 consumes the ported greenfield tree: `xtask` as the one
  regen host with the CSV target from PROVE-13 (SPEC GN-1), the `general_descent` emitter with
  WALL-A newline-significance and WALL-B class-run scan (PROVE-13 Cure 3), the empty-shapes
  bail lift (PROVE-13 Cure 1), the shape-predicate route (PROVE-13 Cure 2), PROVE-12's
  const-mask installed in the CSS emit spine, and the bench canon with CSS paired mode (R2
  gap closed in GN-1). Without GN-1 the CSV path does not exist to re-measure.
- **GN-3 (the type plane), full close.** GN-5.W3 mutates `codegen/src/json_sink_direct/`; GN-3
  edits a DIFFERENT member of the direct-to-struct family — the typed lane (`codegen/src/
  type_plane/` + `codegen/src/json_typed_direct.rs`), never `json_sink_direct` — so there is no
  shared-file race between them. The dependency is plain sequencing: GN-3 green-and-committed
  before GN-5.W3 dispatches keeps the direct-to-struct family stable while the shape-predicate
  cure lands, so W3 measures `check-json` bit-for-bit against a settled tree. Owner gate: GN-3
  green before W3 dispatch.

### Feeds

- **GN-6 (the shopwindow).** The five-grammar fleet, the fat-LTO CSV number, and the
  zero-name-gate proof are the evidence GN-6's PERFORMANCE-THESIS and differentiator statement
  cite: "CSV at csv-core parity from a grammar (13), five grammars one generator zero
  name-gates." W4's consolidated oracle fleet IS the canon-fleet-green close gate GN-6 inherits.

### Owner gates

- GN-3 completion before W3 (shared-file sequencing, above).
- No §3 adjudication checkbox blocks GN-5. The differentiator claim GN-6 publishes is
  *substantiated* by W3's grep-gate and W4's fleet — GN-5 produces the proof, GN-6 states it.

---

## §3 Wave manifest

| id | name | scope | LOC (+new/−del) | gate | hard cap |
|----|------|-------|-----------------|------|----------|
| W1 | The Honest CSV Re-proof | fat-LTO release re-measure vs csv-core; bulk-row cost lever vs csv `read_byte_record`; born the `bbnf-oracle` crate (csv lane) | +820 / −180 | born-RED (two thresholds) | 40 min |
| W2 | The Fifth Grammar | author `toml.bbnf` net-new; regen-toml target; round-trip oracle vs the `toml` crate | +1020 / −0 | born-RED (parity + perf) | 45 min |
| W3 | The Last Name-Gate | cure `json_sink_direct` internal `"value"` residue by shape; falsifiable CI grep-gate | +300 / −20 | born-RED (falsification-proven) | 25 min |
| W4 | The Const-Mask Diaspora + Oracle Fleet | hoist `delim_mask` to shared seam over every stop-set; consolidate the scattered oracle surface into the (W1-born) five-grammar `bbnf-oracle` fleet | +780 / −840 | born-RED (immediate-verified + fleet-green) | 40 min |
| W5 | The Adversarial Re-measure | non-author re-measure of W1–W4 claims; residue hunt | +420 / −0 | born-RED (any failed claim demotes) | 30 min |

Tranche budget: **+3340 / −1040** (net +2300). Generated-LOC sub-budget: TOML generated dir
≤500 (mirrors CSV's ~364), enforced by `xtask lint-loc`.

---

## §4 Per-wave specification

### W1 — The Honest CSV Re-proof

**Objective.** Re-prove PROVE-13's CSV lane under the fat-LTO `release` profile — the profile
the adversarial verifier could not reach (loadavg 43 foreclosed the ax-iter re-run; the 0.948×
stands only DIRECTIONAL, SPEC §0 row 13). Then attack the one honestly-unmet gap PROVE-13
disclosed: bbnf trails the high-level `csv` crate's bulk `read_byte_record` reader by 1.363×
because bbnf emits a per-field tape mark where `csv` reuses one `ByteRecord`. Close it with a
cost-model-selected bulk-row recognizer shape for flat line-oriented grammars.

**File-level plan.**

- Cost lever (the shape selection). `crates/ir/src/cost.rs` — extend `enum BackendShape`
  (skinny seam `ir/src/lib.rs:361`, five shapes today) with `BulkRow` for flat, non-recursive,
  line-oriented grammars whose record rule is `field, (sep >> field)*` over one leaf shape.
  `crates/passes/src/lib.rs` — `derive_backend_shape` (skinny seam `passes/src/lib.rs:398`)
  selects `BulkRow` when the grammar's rule tower is depth-1 record-of-fields with a positive
  newline-significant leaf (the `newline_significant` fact PROVE-13 already derives, REPORT §1
  Cure 3 WALL A). The N*=3 crossover and the flatness predicate are the cost thresholds; the
  selection is pluggable (memory: `pluggable-components`), keyed on grammar shape facts, never
  grammar name.
- Emitter (the consumer of the shape). `crates/codegen/src/general_descent/emit.rs` +
  `regex_first.rs` (skinny seams `general_descent/emit.rs:452`, `regex_first.rs:230`) — under
  `BulkRow`, emit a record-granular tape: one movemask-narrow pass scans the record separator
  and field delimiters together (the class-run scan PROVE-13's REPORT §2 already emits, widened
  to record scope), recording record offsets + a lazily-split field index rather than eager
  per-field marks. The field-granular path remains for grammars the cost model does not select
  — one path per shape, chosen by the model, not a runtime branch (memory:
  `no-orthogonal-codepaths` — the shape is a compile-time decision).
- Regen + wiring. `crates/xtask/src/main.rs` `regen-csv` (ported from PROVE-13) regenerates
  `crates/runtime/src/grammars/csv/*` under the new shape; `check-csv` asserts bit-for-bit.
- Oracle crate (net-new). `crates/bbnf-oracle/` is **born this wave** — the crate skeleton
  (`Cargo.toml`, `src/lib.rs` fleet-driver stub) plus the first per-grammar submodule
  `src/csv/mod.rs` (`round_trip`/`bench` over the csv-core + csv scratch-bridge) and the
  `src/bin/csv_bench.rs` harness that the gate below drives. A directory-module with
  per-grammar submodules from birth (precept 14/15); W2 grows it with the toml submodule, W4
  consolidates the scattered oracle surface into it (§4 W4). The greenfield tree (GN-1 §4.0)
  ships `bbnf-bench` but no oracle crate — this is the crate's first appearance in-tree.

**Seams (path:line from sources).** PROVE-13 REPORT §2 (the emitted `parse_textdata` class-run
loop, the WALL-A `skip_ws` const-fold); R1 §8.3 (`ir/src/cost.rs:5` CostFacts, `:264`
PriorityStep, `BackendShape` at `ir/src/lib.rs:361`, `derive_backend_shape` at
`passes/src/lib.rs:398`); R1 §5 (regen mechanics, `xtask/src/regen.rs:19`).

**Gate (exact command, threshold, failure semantics).**

```
cargo run --release -p bbnf-oracle --bin csv_bench -- --profile release --paired --n 300
```

- **Gate A (born-RED — the re-proof):** bbnf median cyc/B ≤ 1.0× csv-core (`read_field` pull
  machine), cold paired, N≥300, ≥3 windows, every window's 95% CI excluding 1.0 in the same
  direction. RED until the fat-LTO number lands; if it exceeds 1.0×, the 0.948× claim is
  DEMOTED in the durable docs to "csv-core parity, directional" and the fat-LTO gap is a named
  W5 residual. The gate cannot pass on ax-iter — the harness reads the profile and refuses any
  non-`release` build (a gate that a wrong profile cannot silently satisfy).
- **Gate B (born-RED — the bulk-lane):** the `BulkRow` shape's median cyc/B is strictly lower
  than the field-granular shape on the identical 50 MiB corpus AND ≤ 1.2× csv
  `read_byte_record` (from the measured 1.363×). RED until both hold. If `BulkRow` does not
  beat field-granular, the cost lever is reverted and the 1.363× gap is carried to GN-6 as a
  measured non-goal with the reason (the `csv` crate's amortized `ByteRecord` reuse is a
  materialization strategy, not a scan strategy) — no shipped dead lever.

**Oracle + bench (cold paired PMU, standing law).** `csv_bench` interleaves three lanes
per sample (bbnf field-granular, bbnf `BulkRow`, csv-core, csv high-level), N≥300, min-and-median
reported, per-block loadavg logged beside the table. Correctness oracle: the RFC4180 crafted
inputs (quoted / `""`-escaped / CRLF-in-field / empty-field) PROVE-13 §3 exercised, now a
committed fixture under `crates/test-fixtures/corpus/csv/`.

**Consumer in this wave.** `csv_bench` reads the cost-model shape selection (it links the
generated `BulkRow` recognizer) and the oracle asserts byte-for-byte field parity between the
two bbnf shapes — the cost lever's decision is exercised, not merely emitted.

---

### W2 — The Fifth Grammar

**Objective.** No TOML grammar exists anywhere in either tree (R1 §6, §10). Author
`grammar/misc/toml.bbnf` net-new, wire it through the one generator via a `regen-toml` target,
and prove it round-trips against the `toml` crate. TOML is the ideal generality witness: it is
line-oriented (exercises WALL-A newline-significance), sectioned (`[table]`, `[[array-of-table]]`
— exercises the general-descent rule tower), and typed at the leaf (strings, integers, floats,
booleans, datetimes, arrays) — a shape neither JSON nor CSV covers.

**File-level plan.**

- Grammar source. `grammar/misc/toml.bbnf` — authored against TOML v1.0.0: key/value pairs,
  bare and quoted keys, basic and literal strings (single and multi-line), integers/floats/
  booleans, offset and local datetimes, inline tables, arrays, `[table]` and `[[array]]`
  headers, comments. Leaf semantics via `->` projections where the value type is expressible;
  context-dependent nesting (table paths) via the general-descent tower (memory:
  `hybrid-grammar-host` — grammar expresses leaf semantics, host handles recursive context).
  Strong typing at leaves; the datetime and multi-line-string leaves are class-run scans in the
  WALL-B idiom PROVE-13 established.
- Regen target. `crates/xtask/src/main.rs` — one `TOML_TARGET` const + `regen-toml`/`check-toml`
  arms mirroring `SHEETS_TARGET` (R1 §5 dispatch at `xtask/src/main.rs:39-52`). Source resolves
  against repo-root `grammar/` (R1 §10 path trap). Generated dir
  `crates/runtime/src/grammars/toml/{generated,parser,view,config,mod}.rs`.
- Runtime wiring. `crates/runtime/src/lib.rs` — `+4` lines wiring `generated_toml` +
  `grammars::toml`, mirroring the CSV wiring PROVE-13 §4 costed at +4.
- Oracle submodule. `crates/bbnf-oracle/src/toml/mod.rs` — the **second** per-grammar submodule
  grown onto the crate born in W1 — a `round_trip(doc: &str)` that parses via the bbnf tape and
  via the `toml` crate, normalizes both to a canonical key-path→value map, and asserts
  structural equality. The `toml` crate is a scratch-copy dev-dep of the oracle crate only
  (never a runtime dep).

**Seams.** PROVE-13 REPORT §1 Cure 3 (WALL-A/WALL-B, the general-descent emitter that TOML
inherits); R1 §5 (regen target roster), §6 (grammar source home), §10 (path trap, "PROVE-13
must add a regen target + generated dir" — the exact pattern TOML follows).

**Gate.**

```
cargo test -p bbnf-oracle --test toml_round_trip
cargo run --release -p bbnf-oracle --bin toml_bench -- --paired --n 200
```

- **Gate A (born-RED — round-trip parity):** the derived TOML recognizer round-trips against
  the `toml` crate across a committed corpus of ≥40 documents (the toml-test valid suite subset
  + crafted adversarial: nested inline tables, arrays-of-tables, multi-line literal strings,
  unicode keys, datetime variants). RED until every document's canonical map matches. A parse
  mismatch throws (precept 13/56: fail-explicit, no silent Epsilon), and the oracle prints the
  first divergent key-path.
- **Gate B (born-RED — perf directional-to-certified):** bbnf median cyc/B ≤ 1.0× the `toml`
  crate on the corpus, cold paired, N≥200. The `toml` crate is a hand-rolled recursive parser,
  not a SIMD engine; beating it from a grammar is the generality flex, and the gate is honest
  because it *can* fail (the `toml` crate's `Deserialize` fast path is nontrivial). If unmet,
  the number is carried as directional with the loadavg recorded — but Gate A alone is
  sufficient for wave close; Gate B failure opens a named W5 re-measure, not a wave block.

**Oracle + bench.** As above; the bench is a fourth `bbnf-oracle` bin, cold paired, standing
law. Corpus committed under `crates/test-fixtures/corpus/toml/`.

**Consumer in this wave.** The round-trip oracle IS the consumer — it drives the generated
recognizer over 40+ documents. No dormant grammar: the recognizer is exercised the moment it
regenerates.

---

### W3 — The Last Name-Gate

**Objective.** PROVE-13 cured the *routing* name-gate (`!has_rule("value")` →
`json_sink_direct` vs `general_descent`) with a shape predicate, and disclosed a remaining
*internal* residue: `json_sink_direct` still finds its dispatch rule by the literal name
`"value"` (REPORT §1 Cure 2 residue; VERIFY §2 confirmed the residue is pre-existing and
JSON-only). Cure it by shape, then install the structural guarantee — a CI gate that fails on
any grammar-name predicate anywhere in codegen — and *prove the gate can fail* before trusting
it to pass.

**File-level plan.**

- The cure. `crates/codegen/src/json_sink_direct/mod.rs:98` — replace
  `.find(|rule| rule.name == "value")` with `.find(|rule| is_dispatch_rule(rule, program))`.
  `is_dispatch_rule` (new private helper, same file, co-located with its sole caller — precept
  9, single-use helper inline) tests: the rule's body is an alternation whose arms each key on a
  distinct first-byte first-set (facts `first_set.rs:81` already computes) and each projects a
  `DirectBuild` shape (facts the lowering already carries, ≥2 shapes — PROVE-13's route
  predicate). Assert exactly one such rule reachable from entry; zero or two throws
  (precept 13/56 — fail-explicit, never silently pick the wrong dispatch rule). In JSON this
  resolves to `value` unchanged, so `check-json` stays bit-for-bit.
- The other two `rule.name ==` sites (`json_sink_direct/mod.rs:105`, `dispatch.rs:50`,
  `first_set.rs:81`) are generic callee/name lookups, NOT name-gates — VERIFY §2 adjudicated
  them so. They are left untouched; the grep-gate below is written to distinguish a *literal
  grammar-name string* (`"value"`, `"csv"`, `"json"`) from a *variable name comparison*
  (`rule.name == callee`).
- The gate. `crates/xtask/src/main.rs` — a `check-name-gates` subcommand (colocated with the
  other `check-*` arms) that walks `crates/codegen/src/` and fails on any string literal equal
  to a known grammar name (`json`, `css`, `csv`, `toml`, `sheets`, `value`) appearing as the
  right operand of `==`, inside `has_rule(...)`, or in a `matches!`/`match` arm over a rule/
  grammar identifier. Backed by a syntactic walk (the `syn` AST, not raw grep) so
  `rule.name == callee` (variable) is distinguished from `rule.name == "value"` (literal) — the
  artefact is an AST audit, not a grep (precept 19: grep-only is supplementary; this gate is
  runtime-backed).

**The falsification proof (the gate must fail on a planted gate).**
`crates/xtask/tests/name_gate_falsification.rs` — plants each of three name-gate forms into a
throwaway in-memory codegen source string and asserts `check-name-gates` reports a violation
for each: (i) `rule.name == "csv"`, (ii) `program.has_rule("toml")`, (iii) a `match grammar_id
{ "json" => ... }` arm. Then asserts the real `crates/codegen/src/` tree passes clean. A gate
that cannot fail is a defect (SPEC §0, HANDOFF §3 failure-mode checklist) — this test is the
proof it can.

**Seams.** `json_sink_direct/mod.rs:98` (the residue, verified present at skinny HEAD via
`grep -rn '"value"' crates/codegen/src/json_sink_direct/`); PROVE-13 REPORT §1 Cure 2 (the
residue disclosure), VERIFY §2 (the adjudication that `rule.name == callee` is generic, not a
gate).

**Gate.**

```
cargo xtask check-name-gates          # AST audit of crates/codegen/src/ — zero grammar-name predicates
cargo test -p xtask --test name_gate_falsification   # the gate trips on 3 planted forms, passes clean on HEAD
cargo xtask check-json                # JSON regen bit-for-bit after the shape cure
```

- **Born-RED:** `check-name-gates` is RED at wave open (the `"value"` residue is present until
  cured); GREEN only after the shape-predicate cure lands. The falsification test is RED until
  the gate correctly trips on all three planted forms. `check-json` must stay bit-for-bit — if
  the shape predicate resolves to any rule other than `value`, JSON output diverges and the
  wave fails (the cure is behaviour-preserving or it is wrong).

**Consumer in this wave.** The falsification test and the JSON regen are the consumers: the
cure is exercised (JSON still emits identically) and the gate is exercised (it trips on planted
gates, passes on clean). The gate then guards W2's TOML and W4's diaspora against regression.

---

### W4 — The Const-Mask Diaspora and the Oracle Fleet

**Objective.** Two consolidations, both in service of "one generator, five grammars."

1. **The const-mask diaspora.** PROVE-12 residualized the CSS component-delimiter membership
   test into a const-generic `u128` bitmask baked as an immediate (VERIFY CONFIRMED:
   disassembly shows `mov x27,#0x2800…`, no slice loads, +18 LOC, byte-identical). But
   `delim_mask` and the const-generic `find` seam live *only* in the CSS emit template
   (`CSS_GENERATED_RS` in `runtime_generator.rs`). Every other grammar-derived stop-set is
   still an interpretive `matches!(_b, 44u8 | 34u8 | 13u8 | 10u8)` (CSV textdata, PROVE-13
   REPORT §2) or a runtime scan. Hoist `delim_mask` + the const-generic membership seam into a
   shared codegen module and route every stop-set through it (precept 3, 5 — one path).

2. **The oracle fleet.** Five grammars now, but the oracle surface is still scattered (R1 §9:
   `bbnf-bench`'s `parity.rs`, `gate.rs`, `css_canon_bench`, the throwaway `csv-prove`, the
   `parity_oracle` bin, per-grammar bins). The `bbnf-oracle` crate already exists — born in W1
   (csv lane), grown in W2 (toml) — so this is a **consolidation, not a creation**: fold the
   scattered surface into that crate's directory-module of per-grammar submodules and delete the
   scattered copies — the canon-fleet-green close gate GN-6 inherits.

**File-level plan.**

- The shared residual seam. New directory-module `crates/codegen/src/residual/`
  (`mod.rs` + `delim_mask.rs` + `const_membership.rs`), owning: `const fn delim_mask<const N>(&[u8; N]) -> u128`
  (hoisted verbatim from PROVE-12) and the const-generic `find_stop<const MASK: u128>` emitter
  template. This is a directory-module owning its kernel + constants (colocation edict, precept
  14/15) — not a helper dumped into a god module. Consumers: `runtime_generator.rs` CSS
  template (now *calls* the shared seam instead of inlining it — the +18 CSS LOC net to ~0 as
  the copy is deleted), `general_descent/emit.rs` (CSV/TOML class-run stop-sets), and the JSON
  structural scan in `json_sink_direct/`. Every grammar-derived stop-set with ≤128 ASCII
  members lowers to one immediate `and`-mask test.
- The oracle fleet consolidation. `crates/bbnf-oracle/` (born GN-5.W1 with the csv submodule,
  grown W2 with toml) is **completed** to the full directory-module: the remaining submodules
  `src/{json,css,sheets}/mod.rs` land here (csv/toml already present), each exposing
  `round_trip(corpus) -> OracleResult` and `bench(corpus) -> PmuTable`, each owning its
  reference-crate scratch-bridge (serde / lightningcss / csv-core+csv / toml / none-for-sheets).
  The `src/lib.rs` stub grows into the fleet driver that runs all five; the
  `json_bench`/`css_bench`/`sheets_bench` bins land (`csv_bench`/`toml_bench` already present) +
  a `fleet` bin running the canon gate. The scattered oracle surface — `bbnf-bench`'s
  `parity.rs`/`gate.rs`, `css_canon_bench`, the `parity_oracle` bin, the per-grammar bins
  (R1 §9) — is folded into the submodules and its scattered copies deleted; the throwaway
  `crates/csv-prove/` is DELETED (superseded, §1). Tests in `crates/bbnf-oracle/tests/`
  (precept 16, never inline). This gathers R1 §9's scattered parity/gate bins into one
  colocated home.
- Sheets carry. Sheets regenerates clean under the one generator (`cargo xtask regen-sheets`,
  R1 §5) and joins the fleet as the fifth oracle submodule — the generality *boundary* grammar
  (39-branch operator tower, one leaf shape; SPEC empirical spine sheets carve-out) is proven to
  co-reside under one generator without special-casing.

**Seams.** PROVE-12 REPORT §2 (`delim_mask` const fn, the const-generic seam), §3 (the
immediate-mask disasm the diaspora must reproduce for CSV/general-descent), §7 ("generalizes to
any grammar-derived delimiter/STOP-set scan — the same const fn mask + const-generic seam");
PROVE-13 REPORT §2 (the CSV `matches!` stop-set that becomes a mask); R1 §9 (the scattered
oracle bins consolidated); R1 §5 (`regen-sheets`).

**Gate.**

```
cargo xtask regen-css regen-csv regen-toml regen-sheets    # all bit-for-bit after the diaspora
cargo run --release -p bbnf-oracle --bin fleet -- --paired --n 200   # five grammars, canon green
```

- **Born-RED (immediate-verified):** the CSV `parse_textdata` and general-descent record-sep
  scans, disassembled (`otool -tvV`), carry the mask as an immediate (`mov …,#0x…`) with NO
  slice `ldrb [xN,#M]` loads and NO length `cmp` guards — the exact PROVE-12 §3 signature,
  reproduced on a second grammar. RED until the diaspora lands and the disasm confirms.
  Regression floor: no grammar's stop-set scan is slower cyc/B than its pre-diaspora
  `matches!` form (ins/B the load-proof certified column). A stop-set that does not shrink is a
  defect, not a wash.
- **Born-RED (fleet-green):** the `fleet` bin runs all five grammars' round-trip oracles to
  parity and emits the five cold paired PMU tables in one invocation. RED until all five pass.
  The tranche exit gate: five grammars, one generator, canon fleet green.

**Consumer in this wave.** The oracle fleet bins consume the diaspora'd emitters (they link and
run the regenerated recognizers); the disasm gate consumes the generated CSV/general-descent
output. Both the const-mask and the consolidation are exercised in-wave.

---

### W5 — The Adversarial Re-measure (the challenge wave)

**Objective (precept 32, one challenge wave per pass).** A non-author fleet re-measures every
GN-5 claim on a fresh, quieter host and hunts prototype residue — the named defect class:
bench-only wiring, feature-flag limbo, hand-schema crutches, dormant kernels. Nothing this
tranche claims is trusted until an agent who did not author it reproduces or refutes it.

**File-level plan (audit instruments, not product mutation).**

- Re-measure driver. `crates/bbnf-oracle/tests/challenge_remeasure.rs` — re-runs W1's CSV
  fat-LTO gate, W2's TOML round-trip + perf, W4's fleet, on fresh windows, records per-block
  loadavg, and asserts the committed numbers reproduce within the standing-law noise band
  (ins/B the load-proof invariant; cyc/B ratios directional).
- Adversarial corpora. `crates/test-fixtures/corpus/{toml,csv}/adversarial/` — TOML: deeply
  nested inline tables, arrays-of-tables at depth, multi-line strings with embedded quotes,
  unicode/emoji keys, every datetime variant. CSV: pathological quoting (`""""`), CRLF-in-
  quoted-field at buffer boundaries, empty trailing fields, 1-column and 1000-column rows.
- Name-gate re-falsification. Independently plant three *different* name-gate forms than W3's
  (`match g { "sheets" => .. }`, `name.contains("toml")`, a `HashSet<&str>` of grammar names)
  and confirm `check-name-gates` trips each — proving the gate generalizes beyond the three
  forms W3 tested.
- Residue hunt (the audit report, returned as prose, not a shipped file). Grep + `syn` walk
  for: `#[cfg(feature = …)]` on any GN-5 path (feature-flag limbo), `BBNF_*` env-gated kernels
  reachable only under a flag (dormant kernel — R1 §5 flagged the JSON `scan.rs`
  `BBNF_TAPE_CAPACITY` precedent), hand-written schema constants (hand-schema crutch), and
  bench-only `pub` surfaces with no non-bench caller (bench-only wiring); plus the `CollapsedStage`
  emit path — GN-0.W4's `collapsed_stage_author_declared` rekey — adjudicated **consumer-or-retire**
  (a dormant shape is a named defect).

**Gate.**

```
cargo test -p bbnf-oracle --test challenge_remeasure -- --nocapture
cargo xtask check-name-gates                          # independent re-run
```

- **Born-RED (any failed claim demotes):** every W1–W4 headline that does not reproduce within
  the noise band is DEMOTED (the number moves to directional, or the lever reverts). The
  challenge wave commits its OWN paired PMU tables beside the authors' — cross-window agreement
  is the certification. A residue found (dormant kernel, flag limbo, bench-only surface,
  hand-schema) is a NAMED defect the wave either cures in place (if small) or files as a GN-6
  carried residual with the exact path. The wave cannot close clean if any claim is
  irreproducible and un-demoted.

**Consumer in this wave.** The re-measure driver is self-consuming — it runs the product paths
under adversarial input. The residue hunt consumes the entire GN-5 diff surface.

---

## §5 LOC ledger

| wave | +new | −deleted | net | generated |
|------|-----:|---------:|----:|----------:|
| W1 | 820 | 180 | +640 | csv regen (bit-for-bit); bbnf-oracle skeleton + csv lane |
| W2 | 1020 | 0 | +1020 | toml ≤500 (budget) |
| W3 | 300 | 20 | +280 | json bit-for-bit |
| W4 | 780 | 840 | −60 | css/csv/toml/sheets bit-for-bit |
| W5 | 420 | 0 | +420 | none (audit) |
| **total** | **3340** | **1040** | **+2300** | |

The net is dominated by W2 (a whole new grammar + oracle submodule) and W1 (the bulk-row lever +
bench + the born `bbnf-oracle` crate); W4 nets slightly negative because the diaspora deletes as
much duplicated template/`matches!` code as it adds, and the oracle consolidation folds the
scattered surface into the already-born crate while deleting `csv-prove` and the scattered bins. Extreme
parsimony holds: no wave adds a subsystem orthogonal to the existing generator (memory:
`system-cohesion`) — the bulk-row shape, the shared residual seam, and the oracle fleet each
fold into an existing spine (cost model, codegen, bench canon).

---

## §6 Exit criteria

1. **W1:** fat-LTO release CSV ≤ 1.0× csv-core, certified (or the 0.948× formally demoted with
   the fat-LTO gap named); the `BulkRow` shape beats field-granular and ≤ 1.2× csv
   `read_byte_record` (or reverted with a measured non-goal).
2. **W2:** `grammar/misc/toml.bbnf` exists, regenerates through `cargo xtask regen-toml`
   bit-for-bit, and round-trips against the `toml` crate over ≥40 documents.
3. **W3:** `cargo xtask check-name-gates` is GREEN on `crates/codegen/src/`; the falsification
   test proves it trips on three planted forms; JSON regen bit-for-bit.
4. **W4:** the shared residual seam carries CSV/general-descent stop-sets as mask immediates
   (disasm-confirmed); the `fleet` bin runs five grammars to parity in one invocation.
5. **W5:** a non-author reproduces W1–W4 within the standing-law noise band, or the failing
   claim is demoted; zero un-named prototype residue survives.
6. **Tranche:** five grammars (json/css/csv/toml/sheets), one generator, canon fleet green,
   zero grammar-name predicates in codegen — the differentiator GN-6 publishes, proven here.

---

## §7 Risks (with named counterexamples)

- **R1 — The bulk-row shape is a mirage.** The `csv` crate's 1.363× lead may be pure
  `ByteRecord` allocation amortization, not scan work — in which case a record-granular tape
  cannot close it without abandoning the offset-tape product (which the get/decode lanes
  require). *Counterexample to guard:* if `BulkRow` cyc/B does not beat field-granular on the
  identical corpus, Gate B reverts the lever and carries the gap as a measured materialization
  non-goal. The tranche does not ship a dead shape to chase a materialization number.
- **R2 — TOML's typed leaves exceed the general-descent emitter.** Datetimes and multi-line
  strings may need context the class-run scan cannot express, forcing a host function.
  *Counterexample:* if a leaf cannot be expressed by `->` + class-run, it routes to a host
  function (memory: `hybrid-grammar-host`) — but a host function that hard-codes TOML datetime
  parsing is a name-gate in disguise; W3's `check-name-gates` must be extended to audit host
  functions, or the datetime leaf stays grammar-expressed. The risk is that "augment BBNF"
  (memory: `no-workarounds-self-hosting`) is harder than a host shim; the gate forbids the shim.
- **R3 — The name-gate grep-gate is circumvented by construction.** A dispatch keyed on a
  grammar-name *enum variant* (`GrammarId::Csv`) rather than a string literal would pass a
  string-literal audit while being a name-gate in spirit. *Counterexample:* W5 plants exactly
  this form (`match g { GrammarId::Sheets => .. }`); the gate's AST walk must flag dispatch over
  any grammar-identity type, not just `&str` — if it does not, W5 files it as a gate gap and
  W3's gate is widened before tranche close.
- **R4 — The const-mask diaspora regresses a grammar with >128-member stop-sets.** A stop-set
  exceeding 128 ASCII bytes cannot fit one `u128`. *Counterexample:* CSS/CSV/TOML/JSON
  stop-sets are all small (≤5 members, PROVE-12/13). The seam falls back to the pre-existing
  `matches!` for any stop-set >128 members (one path per admissibility, cost-driven) — but the
  fallback must be *measured* absent, not assumed: W4's regression floor asserts every extant
  stop-set shrank, so a silently-un-masked large set would show as a non-improving scan and fail
  the gate.
- **R5 — GN-3 and W3 are sequenced, not raced.** They touch DIFFERENT members of the
  direct-to-struct family: GN-3 the typed lane (`type_plane/` + `json_typed_direct.rs`), GN-5.W3
  the sink emitter (`json_sink_direct/`) — no shared file, no race. *Counterexample guarded by the
  interlock:* W3 still opens only after GN-3 commits (owner gate, §2), so the shape-predicate cure
  lands against a settled family and `check-json` stays bit-for-bit; if GN-3 slips, W3 waits.
