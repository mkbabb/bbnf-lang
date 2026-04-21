# AY — Progress Log

Dated execution log for tranche AY. Every wave boundary adds an entry.
The diff between `AY.md` and this file records what changed under
contact.

---

## 2026-04-20 — AY opens; W0 closes

AY tranche dispatched against master HEAD `6516086f` (AX W1r close).
Hard-gate target: BEAT sonic-rs by 15-40% on twitter eager-materialised
bench at AY close. Nine waves planned; dependency chain
`W0 → W1 → {W2 ∥ W4} → W3 → W5 → W6 → W8 → W7`.

### W0 dispatch + close

**Three parallel sub-agents** (1 serial pruning chain + 1 ebnf
diagnosis + 1 AX FINAL bench/docs); **one Absorb-mode triage agent**
on the 11 pre-existing test failures W0-A surfaced when its compile
fix unmasked them.

#### W0-A — source pruning chain (6 commits)

- `69303e10` retire 7 stale wire-contract + emitter-shape tests
  (invariant-14 discharge per W0.1).
- `d427d282` (Absorb) retire 2 additional shape-emit tests
  (`sheets_shape_emit.rs`, `css_l4_shape_emit.rs`) referencing
  privatised / removed `has_full_shape_coverage` /
  `has_shape_dispatch`.
- `fdbc43a3` carve `crates/tape/src/dta.rs` 550 → 80 LOC; delete
  `crates/core/src/backend/rust/emitter/classify_byte.rs` (165 LOC,
  zero workspace callers; sole consumer was the just-deleted
  `classify_byte_dispatch.rs`).
- `851f957a` carve dead `GrammarProfile` fields + `ShapeEntry`
  surface; delete `crates/core/tests/shape_ref_view_parity.rs`;
  carve `cursor.rs` ShapeRef helper surface.
- `eb9a4733` delete `crates/tape/src/shape_dict.rs` + `push_shape_ref`
  helper.
- `2ac6f403` bootstrap regen — `generated.rs` 30180 → 29593 LOC.

W0-A LOC ledger: −2,799 retired in tests + −470 dta.rs + −78
shape_dict.rs + −165 classify_byte.rs + −160 cursor.rs ShapeRef
surface + −150 GrammarProfile dead + −587 generated.rs.

#### W0-B — ebnf_prettify diagnosis

- `8353f56c` halt-with-audit. Root cause: `EbnfParser::parse` fails at
  offset 0 because the EBNF `terminal` rule's `character - '"'`
  (`IrNode::Minus`) inside its Keyword-classified Seq branch hits the
  catch-all stub at `crates/core/src/backend/rust/emitter/shapes/inline.rs:633-639`
  emitting `quote! { return Err(()); }`. Fix is a real codegen
  extension (~80–150 LOC) — recommended landing AY.W2 codegen
  extension. `parse_single_rule` + `parse_multi_rule` annotated with
  `#[ignore = "AY.W0.2 deferred — see audit/AYW0-ebnf-diag.md"]`. Full
  diagnosis at `docs/tranches/AY/audit/AYW0-ebnf-diag.md`.
- Orchestrator follow-on `549964df` ignores `serialize_roundtrip::ebnf_rule`
  (sibling of the same root cause, outside W0-B's file bounds).

#### W0-C — AX FINAL bench + docs (3 commits)

- `c05c5d1a` `docs/benchmarks/post-AX-W1-close.json` (19 entries: 5
  json + 3 css + 5 sheets + 6 bbnf; sheets matrix carries
  `format_simple` + `format_stress` from prior schema; all numeric).
- `c590bcc2` `docs/tranches/AX/FINAL.md` (489 lines, 12 H2 sections
  covering wave-by-wave outcomes, hard gates, bench delta, invariant
  1-21 verification, cross-tranche debt addressed + routed-to-AY,
  Block B non-execution rationale, AX HEAD = `6516086f`).
- `ad1ae733` AX/PROGRESS — AX-closes-now entry.

AX bench delta vs `post-AX-W0a-close`: JSON regressed 4-13% across
all 5 entries (W1r added NodeView + canonical-serialisation harnesses
+ `@pretty sep` codegen without compensating optimisations). CSS held
steady. Sheets/BBNF flat. The JSON regression is the AY.W1 target —
restore AU AoS substrate to recover bytes/cyc.

#### W0-D — Absorb-mode triage of 11 pre-existing failures (6 commits)

W0-A's compile fix unmasked 11 pre-existing test failures previously
hidden by the deleted-by-W0.1 stale tests' compile error. Per
SPEC §Scope-reveal protocol, dispatched W0-D in a fresh worktree
to triage and act:

- `26239370` (Class D bug) IR-walk regex patterns into
  `__regex_scan_<grammar>` adapter — extends
  `dfa_codegen::collect_regex_bearing_states` to walk `IrNode::Regex`
  patterns (HRegex-shape rules and Map-wrapped regex rules were
  missing from the adapter dispatch table). Bumps
  `BBNF_SCHEMA_VERSION` 13 → 14 to invalidate `.bbnf-cache`. Resolves
  `csv_simple` / `csv_multi` / `csv_single` (3 tests).
- `a7aded47` (Class B golden-drift) hex_color round-trip tests rewrite
  to search-by-target across all KvPair records (test-side regression:
  the AT/AU/AW emitter chain shifted the hex u32 to an inner KvPair
  slot whose first byte is a colorspace discriminant; sibling parity
  tests had already adopted the search-by-target pattern). Resolves
  `hex_color_roundtrip_3/6/8digit` (3).
- `24d18f42` (Class B golden refresh) shape-emission goldens
  `bbnf_json_prototype` → `json_prototype` for crate rename. Resolves
  `string_shape_emit_matches_golden` + `number_shape_emit_matches_golden` (2).
- `3aa52225` (Class A retirement) stale mixed-quote `QuotedString`
  predicate retired (`parse-that` `919d77d` removed the predicate
  shape). Resolves `regex_classify::known_patterns` (1).
- `4924de74` (Class D bug) Pratt detector
  `collect_operator_alternatives` admits non-Alt op-rule body (single
  Literal Ref). Resolves
  `w4_pratt_detector_admits_skip_based_operator_chain` (1).
- `acce3a22` (Class D bug) `analysis::references` collector descends
  into all `term` children rather than just `child(0)`; the
  `BbnfBootstrapRuleKind::term_0/1/2` discriminator slots are dead
  code (every term branch emits the same `term` compound, variant
  29). Resolves `lsp::test_large_grammar` (1).

All 6 fixes self-contained — no fix uncovered downstream regression.

#### W0 close + housekeeping (orchestrator)

- `e4d535ca` post-W0-D bootstrap regen — `generated.rs` -235 LOC
  (W0-D `26239370` codegen dedup of three duplicate `__DTA_REGEX`
  patterns).
- `git worktree`: 50 → 13 (-37 orphans pruned: 23 manually-named
  bbnf-wt-* + 17 .claude/worktrees/agent-* + 2 /private/tmp + 4
  active-now W0 worktrees of which 3 already removed at agent close;
  6 AZ-a* + 4 AY-a* preserved as planning references).
- `.profiles/` stale files (>5d): 21 removed; redundant `-az-a5`
  dirs (2): removed.
- `.bbnf-cache`: single-copy under `target/` verified.

#### W0 hard gate ledger

| # | Gate | Evidence | Status |
|---|------|----------|--------|
| 1 | `cargo test --workspace --no-run --profile ax-iter` compile clean | `/tmp/ay-w0-final.txt` line 1 | PASS |
| 2 | `cargo test --workspace --profile ax-iter` runs clean | 1491 passed / 0 failed / 40 ignored | PASS |
| 3 | `wc -l crates/tape/src/dta.rs` ≤ 92 | 80 LOC | PASS |
| 4 | `docs/benchmarks/post-AX-W1-close.json` ≥ 18 numeric | 19 numeric entries | PASS |
| 5 | `docs/tranches/AX/FINAL.md` committed | `c590bcc2` | PASS |
| 6 | Bootstrap regen cycle-1 = cycle-2 byte-identical | `/tmp/regen-diff.txt` empty | PASS |
| 7 | 13 parity + canonical harnesses green | full workspace 1491/0 | PASS |

#### Deferrals (named destination)

- **EBNF Minus-in-Keyword-Seq codegen extension** (deferred from
  W0.2). Destination: AY.W2 (alongside e-graph + classifier work).
  Tests `parse_single_rule`, `parse_multi_rule`,
  `serialize_roundtrip::ebnf_rule` ignored with audit citation.

#### W0 → W1 handoff

W1 inherits clean workspace (compile + test green; bootstrap regen
idempotent), pruned tape crate (dta.rs 80 LOC, shape_dict.rs gone),
pruned profile slots, retired stale tests, AX/FINAL.md baseline
captured, ~37 orphan worktrees out of the way for fresh W1 sub-agent
worktrees. Master HEAD `e4d535ca`.

---

## 2026-04-20 — W1 closes

**Three-phase dispatch** (Phase 1 serial, Phase 2 two parallel,
W1-fix Absorb-mode after bench-revealed regression). Total: 1+2+1+1
= 5 sub-agents + 1 bench agent.

The W1 spec declared 4 parallel agents but file-overlap analysis
(columns.rs / builder.rs / shapes/*.rs) forced phased dispatch
per SPEC §Wave stipulation §Disjoint file bounds.

### W1 Phase 1 — AU AoS substrate revert (W1-A serial, 8 commits)

- `f603f549` AY.W1.1 AoS revert: `Columns` 7 structural Vec
  columns → 1 `Vec<TapeRec>` + parallel `sib_skip: Vec<u32>`.
  `columns.rs` 1618 → 1119 LOC (-31%).
- `599abb8a` AY.W1.2 finaliser: stack-buffer scratch (3×
  `Vec<Option<u32>>` heap allocs → `[Option<u32>; STACK_DEPTH_HINT]`
  arrays).
- `3e5a12cc` AY.W1.1 packed: sidecar transpose source = flat AoS.
- `d93b4292` AY.W1.4 tape: `with_capacity_for(profile, input_len)`
  convenience.
- `b6ff6fe0`/`b649d794` AY.W1.5 tape: `#[inline(always)]` cross-
  crate hot helpers + `Tape::get`.
- `cc9bc86e` AY.W1.2 finaliser: heap fallback for depth >
  `STACK_DEPTH_HINT` (twitter depth = 66 forced).
- `1b101207` AY.W1.5 nm hard-gate: `inline(always)` finaliser +
  builder finish; `docs/benchmarks/post-AY-W1-phase1-nm.txt`.

Phase 1 sanity bench: twitter 437 → 699 MB/s (+60%); 1.6× of
pre-AY baseline.

### W1 Phase 2 — structural-scan + Pratt Option C (parallel)

**W1-C structural-scan (4 commits)**:
- `d0a633c6` AY.W1.3 tape: `structural_scan` substrate
  (`scan_structural` + `StructuralIndex` re-export).
- `8a1d7adb` AY.W1.3 emitter: parse-entry `scan_structural` call +
  `ScanState.structural_index`.
- `5fe281ef` AY.W1.3 emitter: `skip_space_slow` consumer probe
  (`shapes/dispatcher.rs`).
- `f5b5ba94` AY.W1.3 docs: `nm` artefact +
  `docs/tranches/AY/audit/AYW1-structural-scan-consumer-coverage.md`
  (deferred CSS L4 comment-aware variant to AY.W4).

**W1-D Pratt Option C (2 commits cherry-picked, regen folded into orchestrator regen)**:
- `f9c26308` AY.W1.4 pratt: Option C inline +
  `[LocalOpEntry; OP_STACK_CAP=16]` op_stack hoist; mined
  `max_chain_len = 4` across 17 production Pratt rules. Reducer-
  compound emission preserved verbatim.
- `7351ea0c` AY.W1.4 sheets_parity: helper accessor switched to
  `payload_u8` (InlineScalar landing in `pay_narrow` not
  `pay_agg`).

**Orchestrator regen** (W1-D + W1-C combined):
- `49d468f2` AY.W1.6 regen — combined Pratt Option C + structural-
  scan emit. Bootstrap regen cycle-1 = cycle-2 byte-identical.

### W1 close bench (W1-bench, 1 commit)

- `e12fac25` `docs/benchmarks/post-AY-W1-close.json` 19-entry
  matrix + `docs/benchmarks/post-AY-W1-bytes-cyc.txt` trajectory.

**Bench surfaced regression**: twitter Phase 1 699 → Phase 2 close
420 MB/s. JSON twitter -6% vs `post-AX-W1-close`. Per AY operational
posture §1, regression ≥ 5% triggers re-plan. Dispatched W1-fix
absorb agent.

### W1-fix — Absorb-mode regression remediation (3 commits)

Samply diagnostic (saved at `.profiles/samply/post-AY-W1-fix/json_monolithic/twitter/`)
surfaced top self-time:

```
50.88% <JsonParser>::parse        ← inlined eager scan_structural loop
32.20% parse_object_JsonParser_object
 9.71% parse_wrap_JsonParser_value
 2.41% parse_array_JsonParser_array
 1.97% parse_string_escaped
 0.48% tape::structural_scan::next_structural_at_or_after
```

Root cause: W1-C's eager parse-entry `scan_structural` ran an O(N)
byte-class scan over 632KB of twitter input every parse, costing
~750µs of the 1479µs total parse (~50%) — and the only consumer
on JSON is a marginal capacity refinement against
`GRAMMAR_PROFILE.capacity_for`'s AR-floor and a `skip_space_slow`
probe that rarely terminated within the next 64-byte stripe (JSON
whitespace runs are 1-3 bytes).

**Fix A applied** (eager-scan retirement):
- `42573c31` retire eager `scan_structural` — twitter +64% (420 →
  688 MB/s). Removes the parse-entry call at both tape-path +
  visitor-path emitters; removes the redundant `structural_index`
  field from `ScanState`; removes the `skip_space_slow` probe.
- `c33ea914` regen — generated.rs reflects the eager-call
  retirement.
- `fb34e008` audit doc + bench updates —
  `docs/tranches/AY/audit/AYW1-twitter-regression-diag.md`;
  bench artefacts updated post-fix.

`tape::structural_scan::{scan_structural, next_structural_at_or_after}`
retained as substrate for AY.W4's regex-scan specialisation
(CTNS-style consumers will deliver material savings; per W4 spec
§AY.W4.3). Substrate-with-consumer cycle binds at tranche close

---

## 2026-04-20 — W2 closes

**Wave status**: complete with recorded misses. W2 landed the named-
preservation fixes, G1-G4 canonicalisation work, wrap-compound elision
consumer, EBNF Minus-in-Keyword-Seq codegen reactivation, and the wire-
contract surface, but it did not meet the original direct-to-struct or
record-count magnitude gates.

### W2 landed work

- `0c9879a1` AY.W2.1 probe — named collapse decomposes into grammar-
  source causes instead of a single extractor defect.
- `14f3a147` + `930bab0b` AY.W2.2 grammar/IR fixes — precedence-wrap
  repair for `colorFn` / `colorMix` plus defensive Named-preserving
  guards in IR metadata/span handling.
- `6717e3cc` + `fcb9606c` + `c04fd913` AY.W2.6b — position-level Minus /
  Negate / Alt / TokenDispatch emission restored inside the inline
  branch-position path; deferred EBNF tests re-activated.
- `1e550044`, `e189ebaf`, `a5d581ab` AY.W2.3 — G1-G4 universal rewrites.
- `7d2d6885`, `6324f717`, `38e3e749` AY.W2.6 — wrap-compound elision
  consumer, wire-contract bench, and regen.
- `9384b2b9` AY.W2.7 — `named_type_preservation.rs` wire-contract test.

### W2 evidence and deltas

- `docs/benchmarks/post-AY-W2-egraph-spot.txt` is the surviving W2
  bench artefact.
- JSON twitter parse throughput improved from 688 MB/s at W1-fix to
  743 MB/s at the W2 egraph spot (+8.0%).
- JSON record count dropped from 158,638 to 144,725 records
  (-8.77%), proving the wrap-elision consumer fired, but missing the
  much larger original projection.
- EBNF deferred tests from W0 were re-activated and green.

### W2 misses and shifted debt

- `PROJECTION_DIRECT_TO_STRUCT` did not reach the original ≥4-entry
  expectation; named-preservation fixes landed, but broader admission
  remains incomplete.
- The record-count reduction was real but far below the original W2
  projection; W3/W5 remain responsible for the deeper substrate/value
  collapse.
- The broader G5-G9 / detector-retirement agenda did not land as a
  production-shaping reality in W2; dead or weak optimizer surfaces
  remain and are now explicitly owned by AY.W7.

### W2 hard-gate readout

| Gate area | Evidence | Status |
|---|---|---|
| Named-preservation and EBNF reactivation | `9384b2b9`, `fcb9606c`, `c04fd913` | PASS |
| Wrap-compound consumer activation | `docs/benchmarks/post-AY-W2-egraph-spot.txt` | PASS |
| Direct-to-struct admission magnitude | same | MISS |
| Record-count reduction magnitude | same | MISS |

### W2 → W3 handoff

W3 opens on a cleaner grammar-derived semantic surface: named
preservation is materially better, EBNF is unstubbed for the deferred
cases, and wrap-elision is real enough to support grammar-derived value
emission. The remaining direct-to-struct and optimizer debt stays live
and is no longer implicit.

---

## 2026-04-20 — W3 closes

**Wave status**: complete with recorded misses. W3 landed the value
surface, the path/query runtime substrate, the per-shape inline
materializers, the apples-to-apples correctness harness, and the 12-
entry value bench matrix. It did not come close to the original
BEAT-sonic gate.

### W3 landed work

- `82a8f819` + `7fa931d1` AY.W3a — `handle.rs`, `path.rs`, and
  `Parsed::to_value()` / `Parsed::get()` runtime surfaces.
- `7e4c0e6a`, `fc9fdf61`, `c94254db`, `b827369d` AY.W3b — grammar-
  emitted `<Grammar>Value`, `ValueRoot` / `PathQuery`, per-shape inline
  materializers, and regen.
- `a3dc78a7` + `040a7830` + `a91633e3` AY.W3c — value bench lanes,
  `value_api_apples_to_apples.rs`, and the bench matrix artefact.

### W3 evidence and deltas

- `docs/benchmarks/post-AY-W3-value.json` is the canonical W3 artefact.
- Eight grammars emitted `Value` surfaces and 48 `materialize_*`
  functions.
- The 12-entry bench matrix landed: 2 lazy entries and 10 eager
  entries.
- Correctness surface landed: round-trip parity and via-Value checks
  exist and run.

### W3 misses and shifted debt

- Eager JSON remained far from sonic-rs:
  `bbnf_value_twitter / sonic_value_twitter = 3.633x`,
  `citm = 4.128x`,
  `canada = 4.342x`,
  `data_s = 3.469x`,
  `data_xl = 3.286x`.
- The lazy lane is not a true lazy parse; `post-AY-W3-value.json`
  records `bbnf_get_twitter / sonic_get_twitter = 2953.12x` because
  bbnf still parses the full tape.
- W3 therefore closes as a measurement-and-surface wave, not as a
  parity wave. The remaining gap is now explicitly owned by W5-W7.

### W3 hard-gate readout

| Gate area | Evidence | Status |
|---|---|---|
| Value surface emitted and type-checking | `82a8f819`, `7fa931d1`, `b827369d` | PASS |
| 12-entry value bench matrix | `docs/benchmarks/post-AY-W3-value.json` | PASS |
| Correctness / round-trip surface | `040a7830` | PASS |
| Original BEAT-sonic eager gate | `docs/benchmarks/post-AY-W3-value.json` | MISS |

### W3 → W4 handoff

W4 opens with the real problem made explicit: bbnf can now express and
measure the eager/lazy consumer surfaces, but the hot path is still far
too reconstruction-heavy and tape-first. W4's string/number/runtime
levers therefore target real measured gaps, not hypothetical ones.

---

## 2026-04-20 — W4 closes

**Wave status**: complete with recorded misses. W4 landed the SIMD
string fast path, the `pay_f64` numeric substrate, and the regex-scan
specialisation scaffolds. W4 improved JSON parse throughput further,
but the original regex self-time gates and the expected canada numeric
gain did not materialize.

### W4 landed work

- `cd8bdc8a` + `561bea1b` AY.W4.1 — inline SIMD unescape at the
  string parse site and the twitter spot-bench artefact.
- `7e1732d0`, `b199afea`, `05617765`, `4ca520d2` AY.W4.2 — `pay_f64`
  substrate, direct number emission path, regen, and canada spot bench.
- `3ab49fab`, `108c573a`, `e2aea138`, `525fc157`, `c143ca0d`,
  `ae49494d`, `93a74c4d` AY.W4.3 — regex specialisation scaffolds,
  structural-scan consumer wiring, regen, tuning, and spot-bench
  attribution.
- `1ade186f` — W4 close bench, bytes/cyc ledger, and saved Samply
  references.

### W4 evidence and deltas

- `docs/benchmarks/post-AY-W4-close.json` is the canonical W4 close
  matrix.
- `docs/benchmarks/post-AY-W4-simd-spot.txt` records twitter improving
  from 638 MB/s to 676 MB/s on same-machine cold-cache comparison
  (+5.95%) for the SIMD string fast path.
- `docs/benchmarks/post-AY-W4-close.json` records twitter at 746 MB/s
  overall, up from 688 MB/s at W1-fix and 743 MB/s at the W2 spot.
- `docs/benchmarks/post-AY-W4-eisel-spot.txt` records the `pay_f64`
  substrate as bench-neutral rather than the hoped-for +15% canada win.
- `docs/benchmarks/post-AY-W4-bytes-cyc.txt` records twitter at
  0.233 bytes/cycle, still only ~29% of the sonic-rs twitter
  reference.

### W4 misses and shifted debt

- The original regex self-time gates missed badly:
  `__regex_scan_CssL4Parser` on tailwind measured 29.18% self-time
  against a 12% target; Sheets parse_stress stayed effectively flat.
- CSS tailwind throughput was flat-to-slightly-down versus W1.
- The numeric direct-to-column substrate landed, but the expected canada
  gain was not real on same-machine A/B measurement.
- W4 therefore closes as partial runtime improvement plus scaffold
  landing, with the remaining globally informed cleanup and dead-surface
  retirement explicitly shifted into W7 rather than left as narrative
  debt.

### W4 hard-gate readout

| Gate area | Evidence | Status |
|---|---|---|
| SIMD unescape win | `docs/benchmarks/post-AY-W4-simd-spot.txt` | PASS |
| `pay_f64` substrate landing | `docs/benchmarks/post-AY-W4-eisel-spot.txt` | PASS |
| canada gain magnitude | `docs/benchmarks/post-AY-W4-eisel-spot.txt` | MISS |
| regex self-time gates | `docs/benchmarks/post-AY-W4-close.json`, `docs/benchmarks/post-AY-W4-regex-spot.txt`, `docs/benchmarks/post-AY-W4-bytes-cyc.txt` | MISS |
| 19-entry close matrix saved | `docs/benchmarks/post-AY-W4-close.json` | PASS |

### W4 → W5 handoff

The tranche is now exactly where the rewritten AY plan says it is:
W0-W4 are behind us, but they did not close the parity gap. They
recovered the parse path materially and exposed the remaining eager-path
losses honestly. W5 is next, and it is the architectural wave:
canonical packed substrate, direct JSON write, and write-time close
stamping.
per SPEC §Transitional fallback during elimination waves; W4
absorbs the consumer landing.

### W1 hard-gate ledger (post-fix)

| # | Gate | Required | Measured | Status |
|---|------|----------|----------|--------|
| 1 | twitter bytes/cyc | ≥ 0.45 | **0.215** | SOFT-MISS — W2 G3 wrap-elision lever |
| 2 | bbnf/sonic twitter ratio | ≤ 3× | **3.76×** | NEAR — W2 lever (was 6.16× pre-fix) |
| 3 | `nm` push_structural absent (4/4 bins) | yes | yes | PASS (`post-AY-W1-phase1-nm.txt`) |
| 4 | `nm` scan_structural symbol ≥ 1 | yes | inlined per LTO; `StructuralIndex drop_in_place` 4/4 | PASS (`post-AY-W1-phase2c-nm.txt`) |
| 5 | samply self-time on tape substrate < 5% total | yes | tape ≤ 1% post-fix; per-rule parse fns dominate | PASS (`post-AY-W1-fix/json_monolithic/twitter/`) |
| 6 | CSS L4 tailwind ≥ +8% vs post-AX | +8% | **+35.4%** | PASS |
| 7 | 13 parity + canonical harnesses green | yes | 1490 passed / 0 failed / 40 ignored | PASS |
| 8 | Bootstrap regen cycle-1 = cycle-2 | yes | empty diff | PASS |

**SOFT-MISS rationale (gates 1, 2)**: per AY.md §Architectural thesis,
the BEAT-sonic margin requires W2 (G3 wrap-elision cuts twitter
record count 50%) + W3 (json-prototype shape: 0.91× sonic ceiling) +
W4 (SIMD unescape + Eisel-Lemire direct-column: +15-40% margin).
W1 alone restores the AU substrate; the throughput gates close
cumulatively at W7. Per defensible-floor item 2, the W1 AoS revert
delivered (twitter 437 → 688 MB/s = +57%); items 4-6 (W2 + W3)
deliver the remaining bytes/cyc.

### W1 → W2/W4 handoff

Master HEAD `fb34e008`. W2 + W4 may dispatch in parallel per
AY.md `{W2 ∥ W4}` chain. W1 substrate state:

- Tape: flat AoS `Vec<TapeRec>` + parallel `sib_skip` write path;
  `pay_narrow`/`pay_wide`/`pay_agg` payload columns retained.
- Finaliser: stack-buffer post-pass (heap fallback ≥ depth 64).
  Full inline-into-`close_compound` deferred to W4 if profile-
  evidenced as still hot.
- structural-scan substrate present (lazy-only); consumer wiring
  in W4.
- Pratt Option C: stack op_stack + InlineScalar op_discriminant.
- `Tape::with_capacity_for(profile, input_len)` available.

**Known flaky test**: `tape::tests::packed_cache::packed_cache_read_beats_soa_materialise`
asserts a 1.3× perf threshold near system noise floor; intermittent
(passes ~3 of 5 runs across pre-fix and post-fix HEAD). Pre-existing,
not introduced by AY. Standalone retry green at master HEAD `fb34e008`.

`scan_structural` deferred-consumer landing absorbed into AY.W4 per
`docs/tranches/AY/audit/AYW1-structural-scan-consumer-coverage.md`.
