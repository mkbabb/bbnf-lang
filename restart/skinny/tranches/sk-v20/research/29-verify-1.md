# 29 — Adversarial Verification of Load-Bearing Claims (verify-1)

HEAD surveyed: `36ca10646`. Each claim was re-derived independently from source
(git log/show, rg, Read) rather than trusted from the cited reports. Default
posture: skepticism. Verdicts: CONFIRMED / REFUTED / QUALIFIED.

---

## Claim 01 — pre-restart divergence (byte-identical CSS replicas + nine-arm strategy match)

**Verdict: CONFIRMED.**

Both concrete pre-restart Lock-14 violations re-derive cleanly.

1. The seven byte-identical `css_l4_*/generated.rs` at md5 `b654562c` is cited
   verbatim at `restart/audit/totality/p1/1F-anti-pattern.md:45`
   (`= 7× b654562ccff46ed62dd48e9ace325830`). The line and the hash match the
   prompt's citation exactly.
2. The pre-AZ-IV.W1.8 nine-arm grammar-name match in
   `crates/ir/src/registry/strategy.rs` re-derives from both the current source
   comment and git history. `strategy.rs:157` records: "Pre-W1.8 this method
   shipped a 9-arm match expression listing every production grammar by literal
   parser-name." Git confirms the arms were added incrementally
   (`6bafea7a4` GoogleSheetsParser arm; `19de7a71c` CssL4Parser arm) then
   replaced by the manifest at `fbbee8c8b` (AZ-IV.W1.8).

**Qualification (minor, does not change verdict):** the source report cites the
comment at `strategy.rs:156`; the actual comment text is at `:157` (off by one).
The seven replicas are a PAST state — at HEAD only `css_l4_declaration_values`
survives (md5 `f7460fde`, not `b654562c`); the other six were pruned by
`7de58acad` (SK-V18 W-PRUNE). The claim is explicitly framed as the historical
divergence template, so the past-tense framing is correct.

---

## Claim 02 — Lock 14 partial fix; GrammarAuditTag + shape_dict_bbnf remain RED

**Verdict: CONFIRMED.**

- The partial fix is real: `crates/ir/build.rs` is a 92-line file
  (`wc -l = 92`) that reads `[workspace.metadata.bbnf-strategy]` from the
  workspace `Cargo.toml` (build.rs:38-47) and emits `PRODUCTION_MANIFEST_TABLE`
  into `$OUT_DIR/production_manifest_table.rs`. `strategy.rs:139` `include!`s it.
  The nine grammar-named rows now live as DATA in `Cargo.toml:47-55`, not as a
  source-side arm-list. `rg 'JsonParser|CssL4Parser' crates/ir/src` = 0.
- Residual #1 (GrammarAuditTag) re-derives: `GrammarAuditTag` is defined in the
  generic crate at `crates/ir/src/passes/audit/payload_coverage.rs:68` with
  grammar-named variants `Json`/`CssL4`/`Sheets`/`Bbnf` (`:83-86`). These bare
  names are NOT in the live gate's ROSTER (which lists only full parser-struct
  idents like `JsonParser`/`CssL4Parser`), so they escape the W6e gate while
  remaining grammar-specific code in a generic crate.
- Residual #2 (shape_dict_bbnf.rs) re-derives: `shape_dict_bbnf.rs` is a
  grammar-NAMED module in the generic `bbnf-ir` crate
  (`crates/ir/src/passes/recognizers/shape_dict_bbnf.rs`, 217 LOC) with
  `BbnfShapeTemplate`/`BbnfShapeKind`/`mine_bbnf_shape_templates`. W6c
  (`f4633d468`) re-keyed the matching from rule-NAME literals to structural
  shape, but the module/type NAMES still carry the grammar name.

Lock 14 is therefore RED in substance despite the W6e gate being green — the
gate's roster cannot see bare-name enum variants or `Bbnf`-prefixed type names.

---

## Claim 03 — flat-tape LANDED BUT UNWIRED at sk-v16; CSS >SOTA is a W1+W2+W3 cascade

**Verdict: CONFIRMED.**

All four commits re-derive with matching descriptions and content:

- `1c5bd7a25` "feat(sk-v16-W6-tape): add shared flat-tape runtime substrate."
  Its own commit body states: "**CSS/JSON remain on the** [fact-stream]" and
  introduces only `crates/core/src/runtime/tape/{record,cursor,mod,arena}.rs`
  plus tests — "No generated file touched." This is the LANDED-BUT-UNWIRED state.
- `3a37c29d8` "feat(sk-v17-w1): PRUNE fact-stream String -> route CSS L4
  Track-1 into the existing skinny tape." Body: "retire the CSS fact-stream
  String materialization and route the CSS typed product into the EXISTING
  skinny offset tape." This is the W1 activation — CSS was on a String
  fact-stream at sk-v16 close and only entered the tape here.
- `6dad81fb9` (W2) "rebuild lazy RICH typed-CSSOM projection from the tape."
- `6bb4b2a6c` (W3) "NEON structural-index acceleration of the CSS scan."

The CSS >SOTA result is therefore demonstrably the sk-v17 W1+W2+W3 cascade and
not derivable from sk-v16, which closed honestly with the >SOTA gap acknowledged.
The attribution is correct.

---

## Claim 04 — Lock 14 partial; grammar_facts.rs + shape_dispatch/scalar.rs retain grammar-name occurrences

**Verdict: REFUTED (on the specific file attribution).**

The two files this claim names do NOT carry residual Lock-14 violations at HEAD:

- `crates/ir/src/passes/recognizers/grammar_facts.rs`: every grammar-name
  occurrence is in a DOC-COMMENT (illustrative "e.g. Sheets `unary_expr`"
  examples at `:237,:311,:316,…`). A non-doc-comment grammar-name count is 0.
  Critically, the HEAD commit `36ca10646` itself REMOVED the only live reference
  (`BbnfBootstrap::parse` at the old `:799`), rewriting it to "the self-host
  parser." The live W6e gate excludes doc-comments by design (`is_doc_comment`).
- `crates/ir/src/passes/recognizers/shape_dispatch/scalar.rs` (52 LOC): exactly
  ONE grammar-name occurrence, a doc-comment at `:11` ("JSON's `comma`"). No
  live code arm.

The claim's cited source (`PASS-IMPL-FOLD-READINESS.md`, actual line ~37, the
report cites `:26`) names `shape_dict_bbnf.rs` + `grammar_facts.rs` — NOT
`scalar.rs` — and counts 131 RAW `rg` occurrences without distinguishing
comments from code. That readiness doc is STALE: it predates the W6 fold that
removed the live `grammar_facts.rs` reference. The genuine live residual is
`shape_dict_bbnf.rs` (grammar-named module) + `GrammarAuditTag` — correctly
identified by Claim 02, but mis-attributed by Claim 04 to `scalar.rs`. The
high-level "Lock 14 partial, not complete" conclusion is true, but the
file-level evidence for THIS claim is wrong.

---

## Claim 05 — W6a DONE: build.rs metadata-driven, strategy.rs:139 include!, rg = 0

**Verdict: CONFIRMED.**

Every sub-assertion re-derives exactly:

- `crates/ir/build.rs` reads `doc["workspace"]["metadata"]["bbnf-strategy"]
  ["grammars"]` (build.rs:38-43) and writes the table to `$OUT_DIR`.
- The metadata array is present at `Cargo.toml:47-55` (nine grammar rows).
- `strategy.rs:139` is exactly `include!(concat!(env!("OUT_DIR"),
  "/production_manifest_table.rs"));`.
- `rg 'JsonParser|CssL4Parser' crates/ir/src` returns 0 hits (independently run).

W6a is verifiably closed at HEAD `36ca10646`.

---

## Claim 06 — CSS ratios valid within-plane; H1 binding close not completed; absolute Mbps load-depressed

**Verdict: CONFIRMED (with one qualification on the report-level framing).**

- Ratios re-derive: `G6-LEDGER.md:61,63` records tailwindcss 3.27× and animate
  1.50× (the 1.50-3.27× range), captured as same-run `track1_rich` vs
  `lightningcss` pairs in one table — structurally load-FAIR.
- H1 binding close NOT completed: `H1-LEDGER.md:27` stamps `host_loadavg = 6.79`
  (well above the 1.0 quiet bar), `:28-30` FORBIDS an absolute Mbps claim and
  marks figures DIRECTIONAL, `:61` records "Outcome S (honest residual);
  upgrades to A on a loadavg-clean G6 re-capture." No such re-capture exists.
- All `track1_rich` absolute Mbps are load-depressed: `G6-LEDGER.md` records NO
  host_loadavg at all; the H1 figures ran at 6.79; the S-P1 antecedents ran at
  loadavg 4.35 (`BENCH.md:71`). No quiet-clean (`< 1.0`) `track1_rich`
  absolute figure exists in any committed document.

**Qualification:** the prompt's claim, derived from report 06 directive-4,
characterizes MASTER-PLAN.md and BENCH.md as stating "the close as complete
without inline qualification." That secondary framing is itself INACCURATE. Both
documents carry explicit load caveats co-located with the claim:
`BENCH.md:66-72` states CSS >SOTA is "BINDING-closed only at the H1
`css_canon_bench` re-lock under `host_loadavg < 1.0`" and that "the un-caveated
MEASUREMENT-VALID closure word is forbidden before H1." `MASTER-PLAN.md:1066`
states a RATIO (1.66-3.38×), not an un-caveated absolute Mbps; `:1113`
(MP.SK18.H1) carries `host_loadavg<1.0` as an explicit exit gate, and `:991/:1007`
treat H1 close as an unsatisfied precondition for the SK-V19 fold. The
PROVENANCE assertion (every absolute figure is load-depressed) holds; the
secondary framing ("stated as complete without qualification") does not.

---

## Cross-cutting note

Claims 02 and 04 both touch Lock-14 residuals but disagree on WHERE the live
violation is. The independent evidence sides with Claim 02: the live residuals
are the grammar-named `shape_dict_bbnf.rs` module and the `GrammarAuditTag`
enum, both of which escape the W6e roster gate (bare names / type names not in
the parser-ident roster, doc-comments excluded). The W6e gate being green is
therefore NOT proof of Lock-14 closure — it proves only that the full
parser-struct idents are absent from non-comment code. A complete gate must also
forbid grammar-named modules/types and bare-name enum variants in the generic
crates, and should extend scope to `crates/core/src` and `xtask/` (currently
outside the scan boundary).
