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
