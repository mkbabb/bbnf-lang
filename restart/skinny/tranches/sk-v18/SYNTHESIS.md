# SK-V18 Grand Synthesis — The Generalization Cycle (Inflection Backtrack)

Date: 2026-05-31.

Status: Pass Alpha (cycle V5, alphaF) contract for SK-V18 — this revision FOLDS the V4
seven-lens CHALLENGE dispositions (`research/alpha-hardening/V4/{CH1..CH7}.md`) per
`PASS-ALPHA §3`, atop the V1+V2+V3 folds already applied. Seeded directly from the binding inputs:
`restart/prompts/SK-V18-GENERALIZATION-HANDOFF.md`, the PASS-IMPL V3 audit
(`restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md` + `AGENT-{1..6}-*.md`),
and `restart/prompts/pass-contracts/PASS-ALPHA.md`. SK-V17 closed at master
`f6a38445b` (W5 close at `6bb4b2a6c`; the V3 audit committed `7dbe44c22`; HEAD at
this bracket `318d9c046`).

V1→V2 folds applied (all V1 REVISEs, no orphans): JSON >SOTA range corrected
+1.4%–78% → **+1.4%–164.7%** (CH1; the widest row is unicode_escapes, not marine_ik);
§0.6 marks yyjson/asmjson/RapidJSON honest-`None`-on-aarch64 (CH1); G3/PROVE bind the
canonical Lock-14 three-surface model + `match grammar`-arm grep co-gate
(`generator_grammar_branch_count == 0`) — md5-distinctness is necessary-not-sufficient
(CH2); PROVE adopts the EXISTING Pratt `google-sheets.bbnf` + `sheets_grammar_shape`
disclosure (CH2); G4 names the `G: EventGrammar` axis vs the already-real `K=Kind` axis,
makes DELETE the default, and adds `json_rich_navigation_preserved` so a ≥2 impl-count
cannot LCD-flatten JSON's richness (CH5); the G6 retire branch is gated on a samply
non-top-N row (CH6); the honest-finding escape carries its own (a)-(c) qualification gate
(CH6); Section 3 carries the revert dependency graph + hard-cap-default carry (CH6).

V2→V3 folds applied (all three V2 REVISEs, no orphans). The V2 CHALLENGE wave converged at
≥95% on six of seven lenses (CH1 7/0/0, CH3 7/0/0, CH4 5/1/0, CH5 22/1/0, CH6 11/0/0, CH7
7/0/0) with CH2 at 30/1/0 (96.8%); the three surviving REVISEs are folded here:
**(1) CH2 §8 — neutrality-grep alphabet + scan-root widening:** the canonical Lock-14
grammar-neutrality grep (G3 close condition, the `generator_grammar_branch_count == 0`
telemetry column, the gate consumer, and HANDOFF invariant 5) is corrected to the FULL
canonical `LOCKS.md:349` alphabet `Json|CssL4|(GoogleSheets|Sheets)|Bbnf` (`Sheets\w*`
did NOT match `GoogleSheets =>` — the verified witness type is `SheetsEventGrammar` but the
canonical Lock-14 grammar name is `GoogleSheets`, and `Bbnf` is carried for SK-V19
forward-safety), the scan-root set is widened to include the xtask workspace-metadata
surface (`skinny/xtask/src/`, the canonical surface (b)) so a per-grammar branch relocated
into a neutral-identifier `RuntimeTarget`/strategy data-table cannot escape a codegen-scoped
grep (closing the relocated-overfit-seam §0.4 pre-block at the gate), and a second canonical
surface — the grammar-named-*type* census (`JsonParser|CssL4Parser|GoogleSheetsParser|
BbnfBootstrap` per `LOCKS.md:349` surface (a)) — is added as a new `generator_grammar_type_count
== 0` telemetry column (the arm census misses a re-emitted grammar-named parser/`EventGrammar`
type literal). **(2) CH4 §6 — stale checkasm count in the carry-forward ledger:** Section 1
"18 differential harnesses" is corrected to the disk-true **12 single-kernel differential
harnesses + 2 harness/aggregate (`checkasm_common.rs`, `checkasm_parity.rs`) = 14
`checkasm_*.rs` total** (matching αA §3.2/§5 and αE F4) — an un-propagated αA fold that, left
in the binding contract, would seed a P4-class un-satisfiable downstream gate. **(3) CH5 E.1
— shared-trait grep test-exclusion (contract-mirror):** the `shared_value_trait_instantiations`
column already requires "≥2 real production instantiations … test-only `_proof_compiles` does
NOT count" (the machine gate is safe); the V3 fold makes the production-only requirement
explicit at the column AND adds the test-exclusion note so a downstream implementer copying the
recipe inherits the F6-style `grep -v 'tests.rs|#[cfg(test)]'` exclusion on the trait-impl axis,
not only the phantom axis (the αE:141 research-recipe loose-end is a research-artefact fix; the
binding contract here is hardened so it cannot false-green a test-only `impl` behind a ≥2 count).

V3→V4 folds applied (the V3 CHALLENGE wave converged ≥95% on five of seven lenses — CH1 6/1/0,
CH3 7/0/0, CH4 6/0/0, CH6 12/0/0, CH7 7/1/0 — with CH2 29/1/0 (96.7%) and CH5 24/2/0 (92.3%);
the surviving REVISEs are folded here): **(1) CH5 §C.5/§F.7 — the x86 deletion (P1) omitted the
SECOND x86 surface (BLOCKING; the most consequential V3 fold).** P1 and the `x86_tree_deleted`
telemetry scoped x86 deletion to `bbnf-simd/src/x86_64/` (24 files) ONLY — but a vendored x86 ASM
tree survives that scope: `skinny/crates/bbnf-simd/ext/x86/` (`bbnf.asm` 23.8 KB, `x86inc.asm`
59.5 KB, `x86util.asm` 22.9 KB ≈ 106 KB / ~3000 LOC of x264/FFmpeg `cglobal`/AVX-512 ZMM macro
headers, verified on disk), the nasm-rs `build.rs` x86-assembler driver
(`bbnf-simd/build.rs`, "assembles vendored + authored x86_64 .asm sources",
`rerun-if-changed=ext/x86/{x86inc,x86util,bbnf}.asm`, `nasm_rs::Build … rustc-link-lib=static=
bbnf_simd_asm`, verified), and the `src/lib.rs:247` "Contract documented in ext/x86/bbnf.asm"
reference. The old P1 verify grep was scoped to `…/src/`, so `ext/x86/` (a SIBLING of `src/`) and
`build.rs` (at the crate ROOT) escaped it — "x86 gone" was literally false while ~3000 LOC of x86
ASM + an x86-assembler build driver survived green. P1 scope is now WIDENED crate-wide: delete
`ext/x86/` AND delete/neutralize `build.rs` (the nasm driver has no reason to exist on an
aarch64-only crate) AND re-home the `lib.rs:247` scalar-reference contract; the verify command is
crate-wide `grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` (covers `ext/` + `build.rs`)
→ only aarch64-neutral comments; `x86_tree_deleted` is redefined as "NO x86 surface anywhere in
`bbnf-simd`" (this only DEEPENS the net-LOC-deleted claim — a net-positive correction — but the
gate must be honest so the deletion is verified, not asserted). Dormant on aarch64 (`build.rs:40`
early-returns on non-`x86_64`; `ext/x86/` referenced by no aarch64 path) → REVISE not REJECT, but
the close claim is corrected. **(2) CH2 §8.1 — the arm-census grep's reach claim over-stated.**
The contract asserted at five sites (the G3 close-condition row, the V2→V3 fold-ledger, the
`generator_grammar_branch_count` telemetry column, HANDOFF invariant 5) that widening the arm-census
grep scan root to `skinny/xtask/src` means "a per-grammar branch relocated into a
neutral-identifier `RuntimeTarget`/strategy data-table cannot escape." This is empirically false:
a NEUTRAL-identifier data-table by construction carries no `Json =>` arm syntax, so the arm-census
regex `match\s+\w+\s*\{[^}]*Json\s*=>…` is syntactically incapable of firing on it (verified NO
MATCH against the live `regen_css.rs:35 RuntimeTarget` table). The reach claim is scoped honestly:
the codegen+xtask arm census catches a metadata branch that SELF-DISCLOSES a grammar token
(`Json =>` etc.) and a re-emitted grammar-named type (the type census) — NOT a neutral-identifier
strategy table. The REAL machine-check against data-table relocation is the **P3 collapse
close-gate** — a structural row-count check, NOT a regex (the V3 fold bound it to a
`(source_roots,entry_rule)` projection; **the V4→V5 fold (2) below widens that projection to the full
per-`grammar_name` config-tuple modulo the generated-artefact path columns**, because the live css_l4
divergence rides `fact_schema`/`output_plane`/`emitter`, not `(source_roots,entry_rule)`) — PLUS the
§0.4 relocated-overfit-seam prose obligation reviewed at admission. The grep is kept for its real
value; only the over-stated "neutral-identifier data-table" reach claim is corrected. **(3) CH1 §αD
/ CH7 §4 — αD:85 stale checkasm count (research-artefact fix).** The binding contract (the
Section 1 checkasm ledger) already carries the disk-true "12 single-kernel + 2 = 14"; the lone surviving "18" was in
`research/alpha/alphaD-validated-invalidated.md:85` (the V4 VALIDATED row), corrected there to match
the cohort so it cannot re-seed a P4-class "18-present" un-satisfiable gate. No contract change
required (Section 1 was already correct).

V4→V5 folds applied (the V4 CHALLENGE wave reached ≥95% on three of seven lenses — CH2 29/1/0
(96.7%), CH4 6/0/0 (100%), CH5 25/0/0 (100%) — with CH1 6/1/0 (85.7%), CH7 7/1/0 (87.5%), CH6
11/2/0 (84.6%), CH3 5/2/0 (71.4%); wave aggregate 89A/9R/0 = 90.8%, sub-95% non-converging, the
V4 CONSOLIDATED records the clusters; the surviving REVISEs are folded here, no orphans). **(1) CH6 §1 / CH2-no-touch —
the P1 deletion-target obligation was NARROWER than the crate-wide verify grep it is gated by (the
mirror-image of the exact V3 escape P1 exists to fix; the most consequential V4 fold).** The V3→V4
fold widened the P1 verify grep crate-wide (`grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`)
but left the deletion-target list at four items: (a) `src/x86_64/`; (b) `ext/x86/`; (c) `build.rs`;
(d) the `lib.rs:247` doc comment. Re-grepped live at HEAD `318d9c046`, that crate-wide grep ALSO fires
on at least three ACTIVE (non-comment) x86 surfaces the four-item list never names removing: (e) the
`nasm-rs = "0.3"` ACTIVE build-dependency in `bbnf-simd/Cargo.toml:19` (+ `:14-16` companion comments);
(f) `src/lib.rs:5 pub mod x86_64;` AND the `#[cfg(all(target_arch="x86_64", target_feature="avx512bw"))]`
dispatch arms (`lib.rs:285-288`, `return crate::x86_64::byte_class_from_eq_set_64::…`) — dangling once
(a) deletes `src/x86_64/`, and grep-flagged as active on aarch64; (g) the in-crate doc surfaces
(`CONCRETIZATION-REPORT.md` ~18 x86/avx/nasm hits, `CHECKASM-REPORT.md`). A receiver executing exactly
(a)-(d) leaves the gate RED on `Cargo.toml:19` + `lib.rs:5` + `lib.rs:285-288` — a RED-BY-CONSTRUCTION
gate that invites a receiver to silently narrow the grep back or hand-wave the hits "dormant" (a
paper-close hazard on the mandatory lands-FIRST PRUNE gate). P1 + the `x86_tree_deleted` telemetry are
now EXTENDED to the grep's reach: add removal targets (e) the `nasm-rs` build-dep from `Cargo.toml`
(`:19` + `:14-16` comments); (f) `src/lib.rs:5 pub mod x86_64;` AND the `#[cfg(target_arch="x86_64")]`
dispatch arms in `lib.rs` (e.g. `:285-288`), leaving only the aarch64 + scalar arms; (g) scrub the
in-crate doc surfaces of active x86 narrative OR scope the verify grep to source+manifest
(`--include='*.rs' --include='Cargo.toml'`) so the doc-surface hits are explicitly out of band and
the "none active" claim is honest. This DEEPENS the net-LOC-deleted claim (consistent with the fold's
own net-positive framing) and makes the gate satisfiable-by-construction. REVISE-not-REJECT: the
direction (delete the whole x86 surface, verify crate-wide) is correct; only the obligation/grep reach
mismatch is closed. **This same fold is the αA + αE orphan-propagation REVISE (CH1 §αE, CH3 αA/αE,
CH7 §1): the V3→V4 FOLD-1 second-x86-surface widening landed in αC/SYNTHESIS/HANDOFF but the
`src/`-scoped close-gate survived in `alphaA-results-extraction.md` (`:13-31` FOLD log, `:178`, `:266-267`)
and `alphaE-candidate-shortlist.md` (`:83`,`:93`,`:97`,`:210`,`:216`) — those two research feeders
retained the false-green `src/`-scoped P1 gate.** The crate-wide-AND-reach-extended close-gate authored
HERE (the P1 row + the `x86_tree_deleted` telemetry) is the binding inventory-of-record; the αA/αE
feeder rows are corrected to cite it (research-artefact propagation — the binding contract was already
crate-wide and is now reach-complete; the feeders inherit it). **(2) CH2 §8.1 — the
`runtime_target_rows_collapsed` structural check projects onto the TWO INVARIANT columns and ignores the
FIVE columns the per-profile divergence actually lives in (the same necessary-not-sufficient lineage
md5→grep-alphabet→grep-cannot-fire carried one level deeper, into the F13 structural check itself).**
The V3→V4 fold correctly moved the relocated-overfit-seam defense FROM the arm-census regex (proven
syntactically incapable) TO the P3-collapse structural row-count check — but bound it as `sort -u` over
`(source_roots, entry_rule)` ONLY. Re-run live at HEAD `318d9c046` over `skinny/xtask/src/regen_css.rs`:
the 7 css_l4 `RuntimeTarget` rows are byte-identical on `(source_roots, entry_rule)` (both `CSS_L4_ROOTS`
/ `"stylesheet"`), so `runtime_target_rows_collapsed == true` is GREEN — but the SAME 7 rows carry **7
distinct** values in EACH of `fact_schema`, `row_id`, `output_plane`, `output_dir`, `emitter`. The
`RuntimeTarget` struct (`regen.rs:6`, ~12 fields) is projected onto exactly the 2 fields that are
INVARIANT across all 7 rows, discarding the 5 where per-profile divergence demonstrably lives. This is
a generality hole, not cosmetics: an un-forked emitter that internally dispatches on `target.fact_schema`
/ `target.output_plane` to select a different generated body per CSS profile IS the relocated seam — and
it sails through a `(source_roots, entry_rule)`-only `sort -u`. The contract's own "N collapsed-identical
rows" framing is empirically false (the live rows differ in 5 columns). The P3-collapse MECHANISM is
right; only the PROJECTION TUPLE is too narrow. `runtime_target_rows_collapsed` is REDEFINED at every
F13 site (the G3 (iii) close-condition row, §0.4, the telemetry column, the gate consumer, HANDOFF
inv.5) to: **all `RuntimeTarget` rows sharing one `grammar_name` MUST be byte-identical in EVERY field
except the generated-artefact path columns (`output_dir`, `expected_files`)**. The projection is stated
**by EXCLUSION** so the operative list cannot drift to a strict subset of the prose (CH2 V5 §8.1 / F16):
the live `RuntimeTarget` struct (`skinny/xtask/src/regen.rs:6`, 12 fields) collapses to ONE distinct
config-tuple per `grammar_name` over **every field EXCEPT the two excluded path columns**
(`output_dir`, `expected_files`) — i.e. the operative set is
`grammar_name`/**`profile`**/`entry_rule`/`source_roots`/`check_command`/**`source_inputs`**/**`metadata_inputs`**/`emitter`/`frontend_requirements`/`output_labels`
(the prior enumeration `fact_schema`/`row_id`/`output_plane` named per-profile *content* the
discriminator selects, but OMITTED `profile` itself — the 7-distinct per-profile discriminator that
differentiates the 7 css_l4_* configs — plus `source_inputs`/`metadata_inputs`; the by-exclusion
statement is authoritative and the explicit list is its enumeration, so a future field addition is
captured automatically). Machine-checked by a tiny xtask assertion (or `awk`/`jq`) asserting
`count(distinct config-tuple-minus-(output_dir,expected_files)) == 1` per `grammar_name`. This
gate FAILS today (7 distinct `profile` + per-profile `source_inputs`/`metadata_inputs`) — correct: it
must be RED pre-P3 and only go GREEN after the 7 profiles genuinely collapse to one CSS config, and the
P3 collapse must PRESERVE profile-distinctness where the 7 CSS profiles are genuinely distinct grammars
(collapse to one config only when they are truly one grammar; differentiate by distinct `.bbnf` roots
otherwise — do NOT erase the `profile` discriminator). REVISE-not-REJECT: the Lock-14 spine + the row-count
mechanism are correct; only the projected column set widens to the columns a relocated branch can ride.
**(3) CH6 §13 — V3→V4 fold-ledger self-citation drift (documentation-accuracy).** The V3→V4 fold
narrative back-referenced prior-cycle line numbers (`:201`/`:423`/`:377-378`) that the V4 edits
themselves shifted by ~50-60 lines (G3 → `:253`, branch-count col → `:480`, Section 1 checkasm → `:434`);
machine-gate-UNAFFECTED (the gate rows + telemetry columns + gate consumer use NAMED columns + greppable
commands, not line numbers) but audit-misleading in the authority document. FOLDED: the self-citations
are switched to fold-stable section/column anchors ("the G3 close-condition row," "the
`generator_grammar_branch_count` telemetry column," "the Section 1 checkasm ledger") so the ledger does
not drift on subsequent folds.

**SK-V18 is the GENERALIZATION cycle — the inflection backtrack the user named.**
JSON and CSS are both >SOTA with a working value API. The proof is done. SK-V18 does
NOT push further proof; it **backtracks the hand-written, forked, replicated parsers
into ONE grammar-driven generator emitting all grammars from `.bbnf`**, over the
already-unified tape/`ValueRef` substrate (Lock 1, the genuine foundation), with a
unified value API, **proven on a third grammar (Sheets)**, **preserving the >SOTA
honestly from the grammar-DERIVED parsers**, aarch64-only (x86 tree gone), with a
Lock-14 gate that is actually meaningful. The standing order is **PRUNE first, then
GENERALIZE, then PROVE.**

## Inflection-point verdict (the trigger; carry verbatim)

Per the V3 CONSOLIDATED audit (§Inflection-point verdict): **YES — we are standing
EXACTLY ON the inflection point** (both JSON+CSS >SOTA + working value API), **which
is precisely the trigger to backtrack and generalize, NOT to push further proof.**
The substrate (tape/`ValueRef`/SIMD) generalizes and is verified-unified (Lock 1
holds); the value-API + codegen demonstrably do NOT yet. This is the "backtrack and
generalize at the inflection point" the user defined. Hand-craft was acceptable to
PROVE >SOTA; it is no longer the goal. The hand-written parsers become byte-for-byte
**parity oracles**, not the product.

## Ground truth (verified at this bracket; binds every surface citation below)

This contract gates the **benched skinny tree** (`skinny/crates/`), NOT the totality
tree (`crates/core/`). Every path:line below was grep-verified at HEAD `318d9c046`.

- **Substrate is the genuine foundation (Lock 1 holds, A6 VERIFIED):** one
  `Tape`/`ValueRef`/`PayloadArena` at `skinny/crates/runtime/src/tape/` (`mod.rs:94`
  `Tape`, `:175` `ValueRef`, `:38` `PayloadArena`); both grammars ride it; the CSS
  at-rule tag reuses the sparse flag pair — no second tape. This is the unified,
  generalizable bedrock; SK-V18 builds the generator ON it, does not touch it.
- **The "grammar-driven generator" does not exist — it is two forked hand-written
  parsers (D1, HIGH):**
  - **CSS:** `CSS_GENERATED_RS` is a hand-written recursive-descent scanner emitted
    **verbatim as a Rust `const &str`** (`skinny/crates/codegen/src/runtime_generator.rs`,
    referenced `:91`, defined ~`:685`+); the `.bbnf` grammar is **never consumed** by
    the CSS emit path (`emit_request_facts` feeds only config constants,
    `:25`/`grammar_provider.rs:110`). This is the **identical SK-V16 finding,
    UN-REMEDIATED**, now wearing a real `@generated` header (provenance-honest header
    on hand-written content).
  - **JSON:** `json_sink_direct::render` emits the hot parser as fixed Rust string
    literals (`skinny/crates/codegen/src/json_sink_direct.rs:4` `render`, `:8-16`
    `render_header`/`render_entry`/`render_value_dispatch`/…); the grammar only
    `validate()`-gates emission (`:18`), does not shape it.
  - **The generator is FORKED:** `RuntimeEmitterKind = {CompiledLowering(JSON),
    RequestFacts(CSS)}` (`grammar_provider.rs:40-42`, dispatched
    `runtime_generator.rs:17,25`) — a grammar-family fork behind an abstract enum.
- **The 7 `css_l4_*/generated.rs` are byte-identical (D1):** all share one md5
  (`b654562ccff46ed62dd48e9ace325830` verified across at-rules/declaration-values/
  visual-functions; 7 dirs under `skinny/crates/runtime/src/grammars/`). ONE CSS
  parser replicated 7× — materially overstating "7 grammars admitted."
- **`ValueRef<G: EventGrammar>` is a PHANTOM generic (D2, HIGH):** never instantiated
  with a real grammar in production; the only instantiations are `_proof_compiles::<
  JsonEventGrammar>`/`::<SheetsEventGrammar>`/`::<AnyGrammar>` in
  `tape/event_grammar_tests.rs:18-21` (test-only); `AnyGrammar::STRUCTURAL_CLASS_COUNT
  == 0` (`:36`). The value API is DIVERGENT: JSON = recursive document tree
  (`json/value.rs`, `json/visitor.rs`, `json/view.rs`, `get(key)`); CSS = flat
  rule/decl/typed-token stream (`CssTypedNode`, no visitor, not `DocumentView`) — they
  hand-copied the `at_cursor` pattern, **no shared Value/Document/Cursor trait**.
- **An x86 tree exists, violating aarch64-only (D3, HIGH) — and it is TWO surfaces, not
  one (CH5 V3):**
  - `skinny/crates/bbnf-simd/src/x86_64/` = 24 files (AVX2/AVX512/GFNI/VNNI/IFMA incl.
    `avx512_vnni/digit_mac.rs`), declared unconditionally; 0 real x86 intrinsics, only
    `unimplemented!("Wave 6")` stubs.
  - `skinny/crates/bbnf-simd/ext/x86/` = a vendored x86 ASM tree (`bbnf.asm` 23.8 KB,
    `x86inc.asm` 59.5 KB, `x86util.asm` 22.9 KB ≈ 106 KB / ~3000 LOC of x264/FFmpeg
    `cglobal`/AVX-512 ZMM macro headers) + the nasm-rs `build.rs` x86-assembler driver
    (`build.rs` "assembles vendored + authored x86_64 .asm sources"; `nasm_rs::Build …
    rustc-link-lib=static=bbnf_simd_asm`) + the `src/lib.rs:247` "Contract documented in
    ext/x86/bbnf.asm" reference. Dormant on aarch64 (`build.rs:40` early-returns on
    non-`x86_64`) but a present x86 carrier that falsifies "x86 gone" until deleted.
  Pure wrong-arch scaffolding. **DELETE both surfaces (crate-wide), not just `src/x86_64/`.**
- **The Lock-14 gate papers over the leaks by exclusion (D4, MEDIUM):**
  `GENERIC_SCAN_ROOTS` (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2409`,
  iterated `:2467,:2508`, asserted `:4956`) omits `runtime_generator.rs` (the
  `JSON_*_RS`/`CSS_*_RS` template consts) + routes the leak files into a weaker check;
  the x86 tree is tagged `"diagnostic-x86"`. **Correction to premise:** the gate
  PASSES today — a green gate over standing leaks is worse than a red one. P4 makes a
  green gate meaningful.
- **CSS NEON is largely UNWIRED from the hot path (C1, HIGH):** the W3 kernels are
  checkasm-validated and live in `skinny/crates/runtime/src/runtime_simd.rs`
  (`count_top_level_commas:29`, `find_comment_close:112`, `find_css_significant:169`),
  but only `count_top_level_commas` is reached from a generated module (the *cold*
  rich-summary: `css_l4_at_rules_and_media/generated.rs:157` → `:809-810`);
  `find_css_significant`/`find_comment_close` have only `#[cfg(test)]` callers. **The
  hot CSS scan is scalar.** SK-V18 must wire-or-retire honestly.
- **JSON has its own bespoke scanner (the legacy holdout, A6):** `json/scan.rs:25`
  → `neon::scan(input)` (`json/scan.rs:201`), a JSON-private NEON path NOT routed
  through the neutral `dispatch.rs select_classifier`. G5 migrates it.
- **Sheets is a 25-LOC stub:** `skinny/crates/runtime/src/grammars/sheets_witness/`
  = 2 files, 25 LOC (`event_grammar_witness.rs` 24, `mod.rs` 1) — an `EventGrammar`
  byte-classification witness, NO `.bbnf` source / parser / `BackendRule` shape.
- **A SEPARATE OLD contrived CSS bench path still exists (C3, HIGH):**
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` `lightningcss_facts:528`
  (+ `stylesheet_selectors_…:551`, `…extended:566`, `visual_functions_…:583`) /
  warm `measure_mbps`: warm iters, 85–357-byte SHA256-pinned micro-fixtures, timed
  competitor does MORE work. It did NOT produce the headline numbers (those came from
  `css_canon_bench`, `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`) but is a
  live contrivance + confusion hazard. **DELETE.**
- **Metalang leak (Other, MEDIUM-PRUNE):** bench wave-id `parse_w11_1_number` is
  baked into the SHIPPED `skinny/crates/runtime/src/grammars/json/generated.rs` —
  violates regen discipline. **PURGE.**
- **JSON >SOTA VALID (A1/A5):** cold per-parse, strict per-iter equality vs
  sonic_rs/serde, Track 1 > sonic +1.4%–164.7%, no broadcast (`+1.4%` = apache_builds
  thinnest; `+78%` = marine_ik; `+164.7%` = unicode_escapes/parse_only widest — per
  alphaA §1 / alphaB §1.2 / `skinny/RESULTS.md`). `parse_only` is the unconditional proof.
- **CSS >SOTA MEASUREMENT-VALID but lazy-vs-eager framed (C2, MEDIUM):** the
  canonical `css_canon_bench` is cold, real-corpus (71KB–495KB), N≥200, distinct
  per-corpus medians, no broadcast, genuine independent oracle (A5 live-reproduced
  2.15/2.91/1.91/1.98×; W5 close N=200 medians bootstrap 2.210× / animate 2.355× /
  tailwind 3.348× / material 1.996×). The headline numbers are valid. BUT Track 1
  *counts* 9 aggregate fields lazily while lightningcss *builds an owned typed
  CSSOM* — honest framing is **"lazy rich-summary beats eager full-CSSOM."** H1
  re-frames or adds a symmetric comparator.

## Authority

- `restart/prompts/SK-V18-GENERALIZATION-HANDOFF.md` (the binding seed; §0 pin, §3
  backlog, §4 CHALLENGE addenda, §5 invariants, §3 R10 success criterion).
- `restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md` +
  `restart/audit/skinny-impl-overfit/V3/AGENT-{1..6}-*.md` (the dispositive findings
  D1-D4 + corrections C1-C3 + the prune/generalize/prove backlog).
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` + `HANDOFF.md` + `SPEC.md` (the
  proven substrate + the >SOTA harness + the W5 close ledger; the SK-V18 residuals
  routed there: REDRESS-W2-1 single-emitter, crates/core fold, Sheets/BBNF-self).
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/HANDOFF.md`,
  `restart/locks/LOCKS.md` (Lock 1 substrate-union + Lock 14 grammar-neutrality
  load-bearing; Lock 6/14 generated-output deletion clause; Lock 16 SIMD parity;
  16-lock count verified = 16).
- `restart/prompts/pass-contracts/PASS-ALPHA.md`, `restart/prompts/ORCHESTRATOR.md`.

The active user pin controls gate conflicts: only G-Alpha and G-Omega are the
mandatory user gates (per HANDOFF §6 "Relinquish only at G-Alpha and G-Omega"). The
detailed wave plan (PASS-ALPHA §4.4: owner paths, entry/exit gates, hard caps, revert
protocol, same-wave consumer) is deferred to skinny pass S-P3 in `sk-v18/SPEC.md`,
which consumes the goalset set here.

## Section 0 — Close Condition And Goalset

### 0.1 Close condition (R10)

SK-V18 closes only when ALL of the following are true. Every surface citation is the
benched skinny tree; each gate is written so it is verifiable by grepping
`skinny/crates/`, not `crates/core/`. The standing order is **PRUNE → GENERALIZE →
PROVE**, then HONESTY.

| Gate | Close condition |
|---|---|
| **PRUNE P1 — x86 deleted (the WHOLE x86 surface, crate-wide; deletion list = the grep's reach)** | EVERY x86 surface the crate-wide verify grep can fire on is DELETED — the deletion-target list and the verify grep are reach-matched so the gate is satisfiable-by-construction (CH6 V4 §1; a deletion list narrower than the grep ships a RED-by-construction gate, the mirror of the V3 escape this fold fixes): (a) `skinny/crates/bbnf-simd/src/x86_64/` (all 24 files); (b) `skinny/crates/bbnf-simd/ext/x86/` (the ~3000-LOC vendored `bbnf.asm`/`x86inc.asm`/`x86util.asm` tree); (c) `bbnf-simd/build.rs` deleted-or-neutralized (the nasm-rs x86-assembler driver — no x86 sources remain for it to assemble on an aarch64-only crate); (d) the `src/lib.rs:247` "Contract documented in ext/x86/bbnf.asm" reference re-homed into the aarch64/scalar module doc; (e) the `nasm-rs = "0.3"` ACTIVE build-dependency removed from `bbnf-simd/Cargo.toml:19` AND its `:14-16` companion "x86_64 .asm"/"nasm-rs" comments; (f) `src/lib.rs:5 pub mod x86_64;` removed AND the `#[cfg(all(target_arch="x86_64", …))]` dispatch arms in `lib.rs` (e.g. `:285-288`, `return crate::x86_64::byte_class_from_eq_set_64::…`) removed, leaving only the aarch64 + scalar arms; (g) the in-crate doc surfaces (`CONCRETIZATION-REPORT.md`, `CHECKASM-REPORT.md`) scrubbed of active x86 narrative OR the verify grep scoped to source+manifest (`--include='*.rs' --include='Cargo.toml'`) so the doc-surface hits are explicitly out of band; (h) the COMPILE-COUPLED `tests/checkasm_parity.rs` x86_64 reference block — 9 ACTIVE compile-coupled `bbnf_simd::x86_64::…::*_scalar(…)` call sites (`:458,:464,:467,:477,:478,:484,:493,:497,:502`) resolving into `src/x86_64/` (plus the `#[ignore]` x86 parity harness) — DECOUPLED-OR-DELETED so the test crate compiles after `src/x86_64/` deletion, retaining the aarch64 parity assertions (V5 R-2/CH5 §F.6 — a deletion list narrower than the grep ships a RED-by-construction gate, the same hazard the `Cargo.toml`/`lib.rs` widening fixes); (i) `src/scalar/byte_class_from_eq_set_64.rs`'s residual x86 doc strings (`:10,:12,:15` "AVX-512 BW"/"AVX2") CLEANED to aarch64/scalar-neutral. Zero `mod x86_64` / `avx`/`avx512`/`gfni`/`vnni`/`ifma`/`sve`/`nasm` references in `bbnf-simd`; `bbnf-simd` builds aarch64-only. Verify (crate-wide, NOT `src/`-scoped — `ext/x86/` is a sibling of `src/` and `build.rs` + `Cargo.toml` are at the crate root): `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f` = 0; `grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` returns only aarch64-neutral comments (none active) — every active hit the grep flags is on the (a)-(i) removal list (including the 9 `checkasm_parity.rs` `bbnf_simd::x86_64::…` call sites + the `byte_class_from_eq_set_64.rs` doc strings); BUILD-SOUNDNESS close-gate: `cargo build` AND `cargo test --no-run` clean — the `checkasm_parity.rs` decoupling (h) is what keeps the `src/x86_64/` deletion build-sound (without it the test crate fails to compile against the deleted `bbnf_simd::x86_64::…` paths). [V3 D3 + CH5 V3 §C.5 + CH6 V4 §1 + V5 R-2/CH5 §F.6] |
| **PRUNE P2 — old contrived CSS bench deleted** | The OLD warm micro-fixture path is DELETED: `nonjson_css_l4.rs` `lightningcss_facts:528` + its 3 sibling `*_lightningcss_facts` + warm `measure_mbps` SHA-fixture harness gone. `css_canon_bench` (the cold, real-corpus, N≥200 canonical) is KEPT as the sole CSS >SOTA harness. Verify: `grep -n 'measure_mbps\|lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns 0; `css_canon_bench.rs` present + green. [V3 C3] |
| **PRUNE P3 — 7 CSS replicas collapsed** | The 7 byte-identical `css_l4_*/generated.rs` collapse to ONE CSS grammar (one `generated.rs`, or N **non-identical** generated files each derived from a distinct `.bbnf` — the distinct-grammar-output CHALLENGE). Verify: either one CSS `generated.rs` remains, OR a diff-census shows the remaining CSS generated files are NOT byte-identical (md5-distinct). No "7 grammars" claim over byte-identical replicas survives. [V3 D1] |
| **PRUNE P4 — Lock-14 gate meaningful** | `GENERIC_SCAN_ROOTS` (`lock14_baseline.rs:2409`) is EXTENDED to cover `runtime_generator.rs` + the template-const files + (post-G2/G3) the grammar-derived emitter; the `"diagnostic-x86"` exclusion is removed (x86 is gone, P1); the leak files run the neutrality scan, not the weaker check. A GREEN gate is meaningful — it scans the surfaces where Lock-14 phrase-#1 leaks could live. Verify: the gate scans `runtime_generator.rs`; `accepts_current_allowlist` passes ONLY because the leaks are actually gone (not excluded). [V3 D4] |
| **PRUNE P5 — metalang leak purged** | `parse_w11_1_number` is purged from the SHIPPED `runtime/src/grammars/json/generated.rs` (a fresh regen with no bench-wave-id). Verify: `grep -c parse_w11_1_number skinny/crates/runtime/src/grammars/json/generated.rs` = 0; regen --check clean. [V3 Other] |
| **GENERALIZE G1 — JSON projected from grammar** | `json_sink_direct::render` actually PROJECTS the JSON parser from the `SinkOnlyProgram`/grammar shape (not fixed string literals); the current hand-written template is retained as a **byte-for-byte parity oracle** (the rendered output is diff-equal to the oracle, OR the divergence is a named, validated grammar-parameterized primitive per HANDOFF §6, NOT a silent hand-written blob). JSON >sonic-rs is PRESERVED from the grammar-DERIVED parser (the whole point). Verify: the grammar `SinkOnlyProgram` shapes the emit; parity-oracle diff banked; JSON 51/51 cold strict same-plane held. [V3 A1] |
| **GENERALIZE G2 — CSS routed through grammar lowering** | The `CSS_GENERATED_RS` const-string courier is RETIRED; CSS is a grammar-DERIVED recognizer (the `.bbnf` grammar is consumed by the emit path, not just config constants). LOW risk: the CSS >SOTA does NOT depend on hand-shaping — the hot path is scalar, no fragile kernel to preserve (A2). The verbatim-blob CHALLENGE passes: no `@generated` CSS file is a verbatim `&str` literal in codegen. CSS >lightningcss is PRESERVED from the grammar-DERIVED recognizer. Verify: `grep -c 'CSS_GENERATED_RS' codegen/src/runtime_generator.rs` = 0 (or it is grammar-projected, not a const literal); CSS canonical >SOTA held cold N≥200. [V3 A2/A3] |
| **GENERALIZE G3 — generator un-forked** | The `RuntimeEmitterKind` JSON-vs-CSS grammar-family fork (`grammar_provider.rs:40-42`) is RETIRED; ONE grammar-agnostic emitter path emits every grammar per the **canonical Lock-14 three-surface model** (`LOCKS.md` item 14: every grammar plugs in via (a) `<name>.bbnf`, (b) workspace metadata declaring its strategy, (c) optionally a per-grammar decl crate — generic crates carry ZERO `match grammar { Json => …, CssL4 => … }` arms). The single-emitter-path CHALLENGE passes: no grammar-family flag fork. Verify: `RuntimeEmitterKind::{CompiledLowering,RequestFacts}` gone; one emitter renders JSON+CSS+Sheets; AND the canonical grammar-neutrality greps are 0 over the FULL canonical alphabet (`Json|CssL4|GoogleSheets|Bbnf`, `GoogleSheets` un-abbreviated since `Sheets\w*` does NOT match `GoogleSheets =>`; `Bbnf` carried for SK-V19 forward-safety) across BOTH the codegen AND the xtask workspace-metadata surface (the canonical Lock-14 surface (b)) AND the grammar-named-*type* census: (i) `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|(GoogleSheets|Sheets)\w*\s*=>|Bbnf\w*\s*=>' skinny/crates/codegen/src skinny/xtask/src` returns ZERO (the arm census over codegen AND the xtask `RuntimeTarget`/strategy-table metadata — this catches a metadata branch that SELF-DISCLOSES a grammar token, e.g. `Json =>`, anywhere in either root; it does NOT catch a neutral-identifier strategy table by itself, since by construction such a table carries no `Json =>` arm syntax — see (iii)); (ii) `rg -nE 'JsonParser|CssL4Parser|GoogleSheetsParser|BbnfBootstrap' skinny/crates/codegen/src skinny/xtask/src` returns ZERO (the grammar-named-type census per `LOCKS.md:349` surface (a) — the un-forked emitter must not re-emit a grammar-named `EventGrammar`/parser type literal); (iii) the RELOCATED-overfit-seam (a per-grammar branch moved into a neutral-identifier `RuntimeTarget`/strategy data-table — which (i) is syntactically incapable of detecting, CH2 V3 §8.1) is machine-checked STRUCTURALLY by the P3 collapse close-gate: all `RuntimeTarget` rows sharing one `grammar_name` MUST be byte-identical in EVERY field except the generated-artefact path columns (`output_dir`,`expected_files`) — i.e. (enumerate-by-exclusion over the live `regen.rs:6` 12-field struct) `profile`/`entry_rule`/`source_roots`/`check_command`/`source_inputs`/`metadata_inputs`/`emitter`/`frontend_requirements`/`output_labels` (plus the `fact_schema`/`row_id`/`output_plane` per-profile content the `profile` discriminator selects) collapse to ONE distinct config-tuple per `grammar_name` (the 7 css_l4 rows collapse to one CSS config ONLY IF they are genuinely one grammar — TODAY they carry 7 DISTINCT `profile` + per-profile `source_inputs`/`metadata_inputs`/`fact_schema`/`output_plane`, so this gate is correctly RED pre-P3 and only goes GREEN once the profiles genuinely collapse; P3 must PRESERVE profile-distinctness where the profiles are distinct grammars, not erase the `profile` discriminator; a `(source_roots,entry_rule)`-only projection does NOT catch this, because a relocated branch rides `profile`/`source_inputs`/`fact_schema`/`output_plane`/`emitter`, the per-profile columns — CH2 V4/V5 §8.1), machine-checked by `count(distinct config-tuple-minus-(output_dir,expected_files)) == 1` per `grammar_name` (a tiny xtask assertion or `awk`/`jq`), PLUS the §0.4 prose obligation "every residual CSS routing entry names the `.bbnf` rule it derives from," reviewed at admission. md5-distinctness alone is necessary-not-sufficient — a neutral md5-distinct output can still come from a grammar-branching body (caught by (i)/(ii)) or a relocated metadata data-table (caught by (iii)). [V3 A3/A4 + CH2 V3 §8.1] |
| **GENERALIZE G4 — shared value-API trait; phantom instantiated-or-deleted** | A shared `Value`/`Document`/`Cursor` trait that BOTH JSON and CSS instantiate (value-API isomorphism), over the EXISTING `Tape`/`ValueRef` (no second substrate, Lock 1), **preserving JSON's rich navigation** (`get(key)` + typed-`Kind` + visitor must remain reachable through the shared trait — a thin LCD trait that flattens JSON's richness is a preserve-rich-ast regression and is REJECTed even at ≥2 impls). The G4 target is the **`G: EventGrammar` axis** of `ValueRef` (NOT the already-real `K=Kind` axis — `K` is instantiated; `G` is the phantom defaulting to `AnyGrammar` at `tape/mod.rs:175`): the `G` axis is instantiated with a production grammar witness OR the `G` parameter is REMOVED from the struct. The shared trait's existence is INDEPENDENT of the `<G>` phantom — deleting `<G>` and defining the trait are separable; do NOT couple the trait's shape to animating `<G>` (that would manufacture the very phantom we are deleting). **DELETE is the abrogate-before-patch DEFAULT** (no `CssEventGrammar` witness exists at HEAD, so "instantiate" entails authoring a new grammar-named type — the trait does NOT require it). The phantom-generic CHALLENGE passes: a `<G>` never instantiated with a real type outside `#[cfg(test)]` is decorative. Verify: the shared trait has ≥2 real (JSON, CSS) production instantiations with non-test call sites AND JSON rich navigation is reachable through it; the `G` axis is reached in production OR removed (no test-only `_proof_compiles` standing in). [V3 D2/A6] |
| **GENERALIZE G5 — JSON scanner on neutral NEON** | JSON's bespoke `neon::scan` (`json/scan.rs:201`) is migrated onto the neutral alphabet-parametric NEON kernel via `bbnf-simd/src/dispatch.rs select_classifier` (JSON is the legacy holdout); JSON shares the SAME kernel CSS uses. Verify: `json/scan.rs` routes through `select_classifier(alphabet)`; the JSON-private `mod neon` is retired or reduced to the shared call; JSON 51/51 held cold. [V3 A6] |
| **GENERALIZE G6 — CSS NEON wired-or-retired honestly** | The CSS NEON is wired into the HOT path AT ADMISSION (not `#[cfg(test)]`-only) OR honestly marked dead/retired — the acceleration-wiring CHALLENGE passes. The 5 scalar-passthrough kernels (`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_class_from_table_64`, `eob_pad_clamp`) are wired to real aarch64 ASM OR honestly marked scalar-passthrough (no "neon" label on a scalar body); the UDOT `digit_mac` orphan (`aarch64/digit_mac.rs`) + PMULL/TBX/CSSC backlog are wired with a same-wave hot-path consumer OR documented as a measured non-top-N leaf (no orphan kernel). Verify: any kernel claiming acceleration is reached at admission (grep the generated hot path, not tests); scalar passthroughs carry an honest label. [V3 C1/A4] |
| **PROVE — Sheets via the generator ONLY** | `sheets_witness/` (25-LOC stub) is brought up to a REAL third grammar **through the generator path ONLY** — the EXISTING `grammar/google-sheets/google-sheets.bbnf` (a genuinely-different Pratt formula grammar, not a fresh "third JSON" stub) is brought into the benched skinny tree (new grammar root + xtask target) and consumed by the SAME generator (G3), emitting a Sheets `generated.rs` NON-identical to JSON's and CSS's (distinct-grammar-output) with ZERO hand-authored runtime Rust. If one generator emits a third grammar from `.bbnf`, generalization is REAL (not JSON+CSS-overfit). The Sheets parser instantiates the shared value-API trait (G4). Verify: the Sheets `.bbnf` is in the benched tree; the generator emits a Sheets parser; `md5` of Sheets generated.rs ≠ JSON ≠ CSS; `grep -c 'const.*_RS.*r#' codegen/src` for any Sheets blob = 0; the canonical neutrality grep (G3) stays 0; the Sheets value type instantiates the G4 trait. Fallback (§0.5): if Sheets cannot be emitted via the generator ONLY, generalization is NOT real — surface honestly, do NOT stub-prove. [V3 A3/A6] |
| **HONESTY H1 — CSS comparator framing** | The CSS >SOTA is re-framed as **lazy-rich-summary vs eager-full-CSSOM**, OR a symmetric materialization-depth comparator is added (equal-work). The timed-plane-symmetry + corpus-in-the-timer CHALLENGE passes: the >SOTA comparator does equal work on the real corpus, cold (no micro-fixtures, no more-work-competitor; P2 deletes the old). The canonical `css_canon_bench` is the honest one — keep it. Verify: the close report states the materialization-depth asymmetry explicitly OR shows a symmetric comparator; no warm/micro-fixture surface remains. [V3 C2] |
| **JSON guard** | JSON 51/51 rows remain admitted, strict, same-plane on Apple M5 Max / aarch64; touched rows re-run cold; G1/G5 move no JSON row out of A/GO. JSON >sonic-rs is the >SOTA proof AND the regression tripwire for the generalization. |
| **Invariants** | 16-lock count (`grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` = 16); 5-shape BackendShape canon (tape = substrate-manifest CATEGORY, not a 6th shape); aarch64-only (zero x86/AVX/SVE in bbnf-simd, P1); substrate-union Lock 1 (one tape/`ValueRef`, no parallel/second substrate); grammar-neutral Lock 14 (zero grammar-named branches in generic crates — the gate P4 actually scans them); preserve-rich-ast (no flattened typed rules for speed); no re-opened REDRESS (the §0.4 pre-block families). |
| **Generated-state cleanliness** | `cargo xtask regen --check` clean (all CSS + JSON + Sheets generated files are fresh generator output, never hand-patched). |
| **PASS-IMPL V4 close audit** | The SK-V18 close audit (PASS-IMPL V4) accepts every axis OR records row-level intrinsic-block proof with measurement (per HANDOFF §6: if a grammar-derived parser CANNOT preserve the >SOTA without hand-shaping, surface it honestly as a named validated grammar-parameterized primitive — do NOT paper-close). **The honest-finding escape is itself GATED — a "named validated grammar-parameterized primitive" qualifies ONLY if: (a) the grammar `.bbnf` INVOKES it by name (the primitive is a callable the grammar references — e.g. a registered balanced-delimiter scanner — NOT a free-standing const the emitter splices); (b) it is parameterized by grammar-derived DATA (alphabet/delimiter set from the rule shape), NOT a fixed body; (c) it carries the same `verbatim_blob_present == false` telemetry as any other derived surface. A primitive failing (a)-(c) is a relabeled hand-written blob — REJECT, REDRESS, do NOT close.** Without this gate the escape is the single largest paper-close surface in the contract. |

### 0.2 Starting state (SK-V17 close, HEAD `f6a38445b`; bracket HEAD `318d9c046`)

| Surface | SK-V17 close | SK-V18 bracket |
|---|---|---|
| JSON parse_only / direct_to_struct / real_typed_struct | 51 / 51 admitted, strict, riding the tape | guard baseline; G1/G5 generalization tripwire |
| JSON >sonic-rs strict | VALID (cold, +1.4%–164.7%, no broadcast; widest unicode_escapes) | PRESERVE from the grammar-DERIVED parser (G1) |
| CSS >lightningcss (N=200 cold median, rich-typed vs full-CSSOM) | bootstrap 2.210× · animate 2.355× · tailwind 3.348× · material 1.996× | PRESERVE from the grammar-DERIVED recognizer (G2); honestly framed (H1) |
| Grammar-driven generator | DOES NOT EXIST — two forked hand-written parsers | BUILD (G1/G2/G3) |
| CSS emit path | `CSS_GENERATED_RS` const-string courier; `.bbnf` never consumed | RETIRE → grammar lowering (G2) |
| JSON emit path | `json_sink_direct::render` fixed string literals | PROJECT from `SinkOnlyProgram` (G1) |
| Generator fork | `RuntimeEmitterKind::{CompiledLowering,RequestFacts}` | UN-FORK (G3) |
| 7 CSS `generated.rs` | byte-identical (md5 `b654562c…`) | COLLAPSE to one / N-distinct (P3) |
| `ValueRef<G>` | PHANTOM (test-only `_proof_compiles`) | INSTANTIATE or DELETE (G4) |
| Value API | DIVERGENT (JSON tree+visitor vs CSS flat stream) | shared `Value`/`Document`/`Cursor` trait (G4) |
| x86 tree | `bbnf-simd/src/x86_64/` 24 files, stubs | DELETE (P1) |
| Lock-14 gate | passes by EXCLUSION (`GENERIC_SCAN_ROOTS` omits `runtime_generator.rs`) | make meaningful (P4) |
| CSS NEON | unwired at admission (only `count_top_level_commas` in cold rich-summary) | wire-or-retire honestly (G6) |
| JSON scanner | bespoke `json/scan.rs neon::scan` (not neutral) | migrate to `select_classifier` (G5) |
| Sheets | 25-LOC `EventGrammar` stub, no `.bbnf` | REAL third grammar via the generator (PROVE) |
| Old contrived CSS bench | `nonjson_css_l4.rs` warm SHA-fixture `measure_mbps` | DELETE (P2) |
| Metalang leak | `parse_w11_1_number` in shipped JSON `generated.rs` | PURGE (P5) |

### 0.3 Receiver goalset (PASS-ALPHA §0.3)

Every owner path below is the benched skinny tree. The totality tree
(`crates/core/`) is the SK-V19 fold target, NOT an SK-V18 owner path (a receiver
editing it would burn LOC on an un-benched tree). PRUNE waves land FIRST (they reduce
surface for the GENERALIZE waves and make the Lock-14 gate meaningful before the
emitter rebuild).

| Receiver | Obligation |
|---|---|
| **PRUNE: x86 + old-bench + replicas + gate + metalang** | Delete the WHOLE x86 surface (P1), deletion list reach-matched to the crate-wide verify grep (CH6 V4 §1): `bbnf-simd/src/x86_64/` AND `bbnf-simd/ext/x86/` (vendored ASM) AND `bbnf-simd/build.rs` (nasm driver, delete-or-neutralize) AND re-home `src/lib.rs:247`'s `ext/x86/bbnf.asm` reference AND remove the `nasm-rs` build-dep from `Cargo.toml:19` (+`:14-16` comments) AND remove `src/lib.rs:5 pub mod x86_64;` + the `#[cfg(target_arch="x86_64")]` dispatch arms (`:285-288`) AND scrub the in-crate doc surfaces OR scope the grep to source+manifest — verify crate-wide, not `src/`-scoped, every active grep hit on the removal list (CH5 V3 §C.5 + CH6 V4 §1). Delete `nonjson_css_l4.rs measure_mbps`/`*_lightningcss_facts` warm SHA-fixture path (P2). Collapse the 7 byte-identical `css_l4_*/generated.rs` to one CSS grammar / N-distinct (P3). Extend `GENERIC_SCAN_ROOTS` (`lock14_baseline.rs:2409`) to cover `runtime_generator.rs` + template-const files; drop the `"diagnostic-x86"` exclusion (P4). Purge `parse_w11_1_number` from shipped JSON `generated.rs` (P5). |
| **GENERALIZE: JSON projection (G1)** | Make `json_sink_direct::render` (`json_sink_direct.rs:4`) PROJECT the JSON parser from the `SinkOnlyProgram`/grammar shape; retain the current hand-written template as a byte-for-byte parity oracle. Preserve JSON >sonic-rs from the derived parser; JSON 51/51 cold strict held. |
| **GENERALIZE: CSS lowering (G2)** | Retire `CSS_GENERATED_RS` (`runtime_generator.rs`); route CSS through grammar LOWERING so the `.bbnf` grammar is consumed by the emit path (not just config constants `:25`/`grammar_provider.rs:110`). A grammar-DERIVED CSS recognizer; CSS >lightningcss preserved cold N≥200. |
| **GENERALIZE: un-fork (G3)** | Retire `RuntimeEmitterKind` (`grammar_provider.rs:40-42`); ONE grammar-agnostic emitter path emitting JSON+CSS+Sheets. (SK-V17 residual REDRESS-W2-1 single-emitter unification lands here.) |
| **GENERALIZE: shared value-API + phantom (G4)** | Define a shared `Value`/`Document`/`Cursor` trait both JSON+CSS instantiate over the existing `Tape`/`ValueRef`, PRESERVING JSON's `get(key)`/typed-`Kind`/visitor through the trait (no LCD flattening). Resolve the `G: EventGrammar` axis (NOT `K=Kind`, already real): INSTANTIATE `G` with a production grammar type OR DELETE the `<G>` parameter — DELETE is the default (no `CssEventGrammar` exists; the trait does not need `<G>`). No second substrate (Lock 1). |
| **GENERALIZE: JSON scanner on neutral NEON (G5)** | Migrate `json/scan.rs:201 neon::scan` onto `bbnf-simd/src/dispatch.rs select_classifier(alphabet)`; JSON shares the kernel CSS uses; JSON 51/51 held. |
| **GENERALIZE: CSS NEON wire-or-retire (G6)** | Wire `find_css_significant`/`find_comment_close` (`runtime_simd.rs:169,112`) into the CSS hot path AT ADMISSION with a same-wave consumer, OR honestly retire/mark them **with a samply attribution row proving the kernel's target leaf is non-top-N on the benched CSS hot path** (the retire branch is gated on a MEASUREMENT, not an assertion — it cannot close G6 by marking all NEON "retired" with zero acceleration wired); wire the 5 scalar-passthrough kernels to real aarch64 ASM or label them scalar-passthrough; wire the UDOT `digit_mac` orphan + PMULL/TBX/CSSC with a same-wave consumer or document the non-top-N measurement. Profile-first (samply on the benched path), no orphan kernel. |
| **PROVE: Sheets via the generator ONLY** | ADOPT the EXISTING `grammar/google-sheets/google-sheets.bbnf` (a genuinely-different Pratt formula grammar — STRENGTHENS the litmus; do NOT author a fresh minimal stub that risks producing "a third JSON" and hollowing the litmus, per alphaE §142). Bring it into the benched skinny tree (a new skinny grammar root + xtask target — today it lives in the totality tree only). Run it through the SAME (G3) generator to emit a Sheets parser; the Sheets `generated.rs` is md5-distinct from JSON+CSS; the Sheets value type instantiates the G4 shared trait. The honest third-grammar litmus — Pratt-lowering is the generality STRESS and an honest-finding candidate if the generator cannot lower Pratt. **Fallback per §0.5: if Sheets cannot be emitted via the generator ONLY, the generalization is NOT real — surface honestly, do NOT stub-prove; do NOT hand-write a `_GENERATED_RS` Sheets block.** |
| **HONESTY H1 + clean regen** | Re-frame the CSS >SOTA as lazy-rich-summary vs eager-full-CSSOM OR add a symmetric materialization-depth comparator; keep `css_canon_bench`. `regen --check` clean over all generated files. |

### 0.4 Pre-blocks (carried from alphaC + the CONTEXT REDRESS pre-block; binding)

SK-V18 must NOT reopen any of the following. The CONTEXT pre-block is binding
verbatim:

- **AZ-IV eager-value-tree materialization** (the 118x regression: parsing into a
  value tree by default, eager per-leaf payload / f64-alloc-per-number / per-color
  `Box<CssColor>`). Materialization stays lazy-by-default; the tape appends offsets,
  typed values reconstructed on demand via `ValueRef`. The shared value-API trait
  (G4) is LAZY over the tape — it does NOT re-introduce an eager value tree.
- **StructRegistry / Arena<G> / Builder<G> hot-path indirection** (28-65x on
  bbnf/sheets, 983x css bootstrap, 10583x tailwind WATCHDOG). No registry lookup in
  the per-leaf hot path; no per-leaf `StructRegistry` indirection. The `TapeBuilder`
  is a single non-generic layout-driven sink.
- **CSS fact-stream String serialization** as a live admission output plane
  (`emit_fact_stream`/`CSS_GENERATED_RS`/`CssFullParseSummary`): diagnostic-only,
  never an admission surface. (G2 retires the const-string courier; it must NOT be
  replaced by a fact-stream String.)
- **The hand-coded `W5C_REQUEST_FACT_PROFILES` CSS profile array** (retired in
  SK-V17): not re-introduced; CSS routing stays grammar-derived. Relocating per-rule
  branching into projection DATA is the overfit re-entry seam and is forbidden — every
  residual CSS routing entry names the `.bbnf` rule it derives from. This seam is
  machine-checked STRUCTURALLY by `runtime_target_rows_collapsed` (all xtask `RuntimeTarget`
  rows sharing one `grammar_name` are byte-identical modulo the generated-artefact path columns
  — enumerate-by-exclusion over the live `regen.rs:6` 12-field struct, the operative set is
  `profile`/`source_inputs`/`metadata_inputs`/`emitter`/`entry_rule`/`source_roots`/`check_command`/
  `frontend_requirements`/`output_labels` (plus the `fact_schema`/`output_plane`/`row_id` per-profile
  content the `profile` discriminator selects) collapse to ONE config-tuple per `grammar_name`, NOT a
  `(source_roots,entry_rule)`-only projection which a relocated branch rides past via `profile`/
  `source_inputs`/`fact_schema`/`output_plane`, the per-profile columns, CH2 V4/V5 §8.1), NOT by the arm-census grep — a neutral-identifier data-table
  carries no grammar-token arm syntax for the regex to fire on (CH2 V3 §8.1).
- **The 24-row broadcast measurement** (one CSS timing tuple → N conceptual admits):
  pre-blocked. One timing tuple may NOT be projected across multiple corpus rows.
- **Fixture / FNV contrivances**: per-corpus hand-coded `real_typed.rs` fixture parse
  fns, hand-tuned per-corpus capacity constants, FNV production
  selector/arbiter/correctness proof, FNV closed-enum production migration. FNV stays
  bench-only.
- **x86 / AVX / SVE**: Apple M5 Max / aarch64 only. No x86, no AVX-512, no GFNI/VNNI/
  IFMA, no SVE (Apple cores have no SVE; SVE paths would be dead code), no nasm/x86
  assembler in `build.rs`, no vendored `ext/x86/` ASM. P1 enforces by deletion crate-wide
  (`src/x86_64/` AND `ext/x86/` AND `build.rs`, verified by crate-wide grep, CH5 V3 §C.5).
- **The verbatim-blob re-entry**: a `@generated` file that is a verbatim `&str`
  literal in codegen is hand-written, NOT derived — REJECT as "grammar-driven." G1/G2
  must not replace one const-string courier with another.
- **The phantom-generic re-entry**: a generic `<G>` never instantiated with a real
  type outside `#[cfg(test)]` is decorative; G4 instantiates-or-deletes, it does not
  add a second phantom.
- **The distinct-grammar-output re-entry**: N claimed grammars must have N
  non-identical `generated.rs`; byte-identical replicas do NOT count (P3 collapses the
  7; PROVE requires the Sheets generated file be md5-distinct).
- **brace-counter CSS admission**; **lightningcss CSSOM comparison before the CSS
  recognizer emits comparable output**; **deleting legacy generated/runtime shims
  before replacement proof lands**; **full-codegen close claims while dirty generated
  files remain**; **timed-plane asymmetry / corpus-out-of-the-timer / more-work
  competitor** (the C3 contrivance family; P2 deletes the old, H1 frames the rest).

Inherited REDRESS pre-block families (semantics carried, not just ids):
`28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247,
FNV closed-enum production migration`, plus the SK-V17 residuals routed forward
(REDRESS-W2-1 single-emitter is the G3 SUBJECT, not a re-open — it is admitted to be
discharged here).

Hidden-coupling escapes are pre-blocked unless routed through Pass Omega + G-Omega
(Lock 1 substrate-union): retained sidecars / sidecar tables / sidecar event vectors,
retained cursor/list, cursor streams, aux density/projection tables, parser-owned
structural projections/streams, parallel source passes, second tapes, public
`UnionTape`, new substrate APIs, a sixth `BackendShape`, production FNV arbiters,
production hash-correctness proof, Track 1 ≡ Track 2 sidecars, wrong-plane comparator
admission, cross-call classifier-state retention. **No second substrate**: the shared
value-API trait (G4) and the un-forked emitter (G3) emit accessors over the EXISTING
`Tape`/`ValueRef` — an introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor`
alongside the landed `Tape`/`ValueRef` is a Lock 1 type-ambivalence violation
(REJECT).

### 0.5 The generalization litmus (PASS-ALPHA §4.1, generalization framing)

SK-V18 is NOT a per-corpus-throughput tranche — the throughput targets are
PRESERVATION targets, not lift targets. The per-axis close conditions are
binary-structural (the generator exists / the fork is gone / the phantom is
instantiated-or-deleted / Sheets is a real third grammar), gated on PRESERVING the
SK-V17 >SOTA from the grammar-DERIVED parsers.

| Axis | Current state | Target close state | Expected intervention | Fallback if not met |
|---|---|---|---|---|
| JSON >sonic-rs | VALID on hand-written parser | PRESERVED on grammar-DERIVED parser | G1 (project from `SinkOnlyProgram`) + G5 (neutral NEON) | if the derived JSON parser loses >SOTA: surface honestly as a named validated grammar-parameterized primitive (HANDOFF §6), do NOT paper-close; do NOT silently retain the hand-written blob |
| CSS >lightningcss | VALID on hand-written const-string (lazy-vs-eager) | PRESERVED on grammar-DERIVED recognizer, honestly framed | G2 (grammar lowering) + H1 (honest framing) | if the derived CSS recognizer loses >SOTA: same as JSON — named validated primitive, honest report, REDRESS |
| One generator, N grammars | 0 (two forks, 7 replicas) | 1 generator emits JSON+CSS+Sheets from `.bbnf`; N non-identical `generated.rs` | G3 un-fork + PROVE Sheets | if Sheets cannot be emitted via the generator only: the generalization is NOT real — surface honestly, do NOT stub-prove |
| Shared value API | DIVERGENT; `<G>` phantom | shared trait, ≥2 real instantiations; phantom instantiated-or-deleted | G4 | if no shared trait is dischargeable without an eager tree / second substrate: REJECT the trait shape, report, do NOT force a Lock-1 violation |
| aarch64-only + meaningful gate | x86 tree present (`src/x86_64/` AND `ext/x86/` AND nasm `build.rs` AND `nasm-rs` Cargo.toml dep AND `lib.rs` cfg-dispatch arms); gate excludes leaks | x86 gone crate-wide (deletion list reach-matched to the verify grep — every active grep hit removed, CH6 V4 §1); Lock-14 gate scans the leak surface | P1 (crate-wide, reach-complete, CH5 V3 + CH6 V4) + P4 | n/a — P1/P4 are deletions/extensions, mandatory |

**Tranche-level success criterion (R10):** all PRUNE + GENERALIZE waves close; one
grammar-driven generator emits JSON + CSS + a third grammar (Sheets) from `.bbnf`; the
value API is a shared trait both instantiate; the phantom `<G>` is instantiated or
deleted; JSON >sonic-rs AND CSS >lightningcss are PRESERVED (cold, real-corpus,
honestly framed) from the grammar-DERIVED parsers; aarch64-only (x86 tree gone); the
Lock-14 gate is meaningful (no exclusion holes); regen --check clean. PASS-IMPL V4
accepts every axis or records intrinsic-block proof.

### 0.6 Strict comparator gate (PASS-ALPHA §4.2)

The comparator gate is unchanged from SK-V17 (the proof is preserved, not re-derived)
but now binds the grammar-DERIVED parsers:

- **JSON:** Track 1 vs sonic-rs strict (rebuilt without `utf8_lossy`, `sonic_skipper.rs:5-6`;
  `Cargo.toml:23` `default-features=false`) and simdjson DOM + On Demand (different
  output plane, disclosed) and serde_json strict baseline; per-iter strict equality;
  cold per-parse; no broadcast. The strict-vs-strict gate (the SK-V6 finding) holds:
  permissive-comparator wins do NOT count. **yyjson / asmjson / RapidJSON are schema
  columns only — honest `None` on aarch64 (their FFI is NOT wired in `Cargo.toml`; per
  alphaB §1.1/§3.3 + PASS-ALPHA §4.2 "yes IF comparator runnable"). The gate must NOT be
  read to require an un-run engine's number; a fabricated competitor column is a
  contrivance (timed-plane addendum) and is REJECTed.** asmjson AVX-512 is x86-only, OUT.
- **CSS:** lightningcss full-CSSOM is the fair bar (re-baselined same-run, N≥200,
  cold, real corpus via `css_canon_bench`); cssparser token-scan is a flaw probe only.
  H1 binds the materialization-depth disclosure: the framing is **lazy-rich-summary vs
  eager-full-CSSOM** OR a symmetric comparator. The timed plane must do equal work on
  the real corpus, cold — no micro-fixtures (P2 deletes the old warm SHA-fixture
  path), no more-work competitor.

Every row discloses, per comparator: Mbps median, % delta, materialization/output
plane, strictness plane, and hot leaf. The grammar-DERIVED parser is the subject; the
hand-written parser is the parity oracle, NOT a comparator.

## Section 1 — Validated And Invalidated Ledger

**Validated (SK-V17 close, carry forward):** JSON 51/51 strict same-plane riding the
skinny tape (`f6a38445b`); JSON >sonic-rs (+1.4%–164.7%, cold, no broadcast; widest
unicode_escapes/parse_only); CSS
rich-typed >lightningcss full-CSSOM N=200 cold median (bootstrap 2.210× / animate
2.355× / tailwind 3.348× / material 1.996×, `6bb4b2a6c`); EXACT 9-field cssparser
structural equality; preserve-rich-ast intact (lazy `ValueRef` projection, no eager
tree); the unified `Tape`/`ValueRef`/`PayloadArena` substrate (Lock 1 holds — the
genuine generalizable foundation); the grammar-neutral checkasm-disciplined NEON
kernel set (12 single-kernel differential harnesses + 2 harness/aggregate
(`checkasm_common.rs`, `checkasm_parity.rs`) = 14 `checkasm_*.rs` total,
scalar-ref-as-spec); the canonical
`css_canon_bench` harness; the 16-lock canon; the 5-shape BackendShape canon; FNV
quarantine.

**Invalidated / still open (SK-V18 subject):** the grammar-driven generator (DOES NOT
EXIST — two forked hand-written parsers, D1); CSS const-string courier
(`CSS_GENERATED_RS`, un-remediated SK-16 finding, D1); JSON fixed-string-literal emit
(`json_sink_direct::render`, A1); the `RuntimeEmitterKind` JSON-vs-CSS fork (D1); the
7 byte-identical CSS replicas (D1); the phantom `ValueRef<G>` (D2); the divergent
value API (no shared trait, D2); the x86 tree (D3, DELETE); the Lock-14 gate
exclusion holes (D4); CSS NEON unwired at admission (C1); JSON's bespoke non-neutral
scanner (A6); the old contrived warm CSS bench (C3, DELETE); the `parse_w11_1_number`
metalang leak (Other); the 5 scalar-passthrough kernels + UDOT orphan (the ASM
backlog, A4); Sheets as a real grammar (25-LOC stub, the generalization litmus); the
CSS lazy-vs-eager framing honesty (C2/H1).

The SK-V17 close proved the substrate + the speed (cold, real-corpus, valid) but on
HAND-WRITTEN forked parsers. SK-V18 backtracks both into ONE grammar-driven generator
over the (already unified) tape/`ValueRef` substrate, with a unified value API, proven
by a third grammar (Sheets) — preserving the >SOTA. This is the inflection the user
named, arrived at honestly.

## Section 2 — Telemetry Binding (PASS-ALPHA §4.3)

SK-V18 inherits the SK-V17 JSON + CSS schemas (preserved as guard/regression
tripwires) and adds the generalization-axis columns. The harness MUST emit cold
samples + median per row; the gate rejects warm/broadcast/single-tuple rows.

Retained JSON schema (the >SOTA guard): Track 1 Mbps, Track 2 Mbps, sonic-rs strict,
simdjson DOM, yyjson, serde_json, Δ columns, strictness, output plane, hot leaf,
Signal. Retained CSS schema (the >SOTA guard): `css_corpus` (benched set only),
`css_sample_count` (≥200), `css_sample_statistic == median`, `css_sample_mode ==
cold`, `css_track1_typed_median_mbps`, `css_lightningcss_full_cssom_median_mbps`
(same-run), `css_comparator_plane`, `delta_vs_lightningcss`, `css_typed_summary_equal`
(EXACT, gate before speed), `css_rich_ast_preserved`.

New generalization-axis columns:

| Column | Type | Required |
|---|---|---|
| `grammar_derived` | boolean (the parser is PROJECTED from `.bbnf`/`SinkOnlyProgram`, NOT a const-`&str`/fixed-literal courier) | yes for G1/G2 admission |
| `parity_oracle_diff` | enum (byte-for-byte / named-validated-primitive / divergent) — the hand-written parser as oracle; `divergent` is NO-GO | yes for G1/G2 |
| `verbatim_blob_present` | boolean (must be `false` — no `@generated` file is a verbatim `&str` literal in codegen) | yes for codegen claims |
| `emitter_fork_present` | boolean (must be `false` post-G3 — no `RuntimeEmitterKind`/grammar-family fork) | yes for G3 |
| `generator_grammar_branch_count` | integer (must be `0` — the canonical Lock-14 `match grammar { Json=>/CssL4=>/GoogleSheets=>/Bbnf=> }` arm census over the FULL canonical alphabet `Json\|CssL4\|(GoogleSheets\|Sheets)\|Bbnf` across BOTH `skinny/crates/codegen/src` AND `skinny/xtask/src`; this catches a metadata branch that SELF-DISCLOSES a grammar token, NOT a neutral-identifier strategy table — the relocated-into-a-data-table seam is caught STRUCTURALLY by `runtime_target_rows_collapsed` below, not by this regex (CH2 V3 §8.1); md5-distinctness is necessary-not-sufficient, this is the neutral-emitter co-gate) | yes for G3 + PROVE |
| `runtime_target_rows_collapsed` | boolean (must be `true` — all xtask `RuntimeTarget` rows sharing one `grammar_name` are byte-identical in EVERY field except the generated-artefact path columns `output_dir`/`expected_files`; `count(distinct config-tuple-minus-(output_dir,expected_files)) == 1` per `grammar_name`, enumerate-by-exclusion over the live `regen.rs:6` 12-field struct — the operative set is `profile`/`source_inputs`/`metadata_inputs`/`emitter`/`entry_rule`/`source_roots`/`check_command`/`frontend_requirements`/`output_labels` (plus the `fact_schema`/`row_id`/`output_plane` per-profile content the `profile` discriminator selects); the 7 css_l4 rows collapse to one CSS config row ONLY IF genuinely one grammar (P3 PRESERVES profile-distinctness where they are distinct grammars) — a `(source_roots,entry_rule)`-only `sort -u` is INSUFFICIENT, the divergence rides `profile`/`source_inputs`/`fact_schema`/`output_plane`/`emitter`, the per-profile columns, CH2 V4/V5 §8.1 — the STRUCTURAL machine-check for the relocated-overfit-seam the arm census cannot detect, per CH2 V3 §8.1) | yes for P3 + G3 |
| `generator_grammar_type_count` | integer (must be `0` — the grammar-named-*type* census `rg 'JsonParser\|CssL4Parser\|GoogleSheetsParser\|BbnfBootstrap' skinny/crates/codegen/src skinny/xtask/src` per `LOCKS.md:349` surface (a); the arm census misses a re-emitted grammar-named parser/`EventGrammar` type literal) | yes for G3 + PROVE |
| `phantom_generic_resolved` | enum (instantiated / deleted) — the **`G: EventGrammar` axis** of `ValueRef` (NOT the already-real `K=Kind` axis) is reached in production OR the `G` parameter is removed; `phantom` (G stays `AnyGrammar`) is NO-GO | yes for G4 |
| `shared_value_trait_instantiations` | integer (≥2 real **production** instantiations: json + css; test-only `_proof_compiles`/`#[cfg(test)]` impls do NOT count — the count is over NON-test `impl <SharedTrait> for …`, mirroring the F6 phantom-grep exclusion (`grep -v 'tests.rs\|#[cfg(test)]'`) on the trait-impl axis, NOT only the phantom axis: a `#[cfg(test)] impl SharedValueTrait for CssTestNode` must NOT false-green the ≥2 gate) | yes for G4 |
| `json_rich_navigation_preserved` | boolean (must be `true` — JSON `get(key)` + typed-`Kind` + visitor reachable THROUGH the shared trait; a ≥2 impl-count without this is an LCD-flatten preserve-rich-ast regression) | yes for G4 |
| `generator_grammar_count` | integer (distinct grammars emitted by the ONE generator: json, css, sheets = 3) | yes for PROVE |
| `generated_md5_distinct` | boolean (the N `generated.rs` are md5-DISTINCT, not byte-identical replicas) | yes for P3 + PROVE |
| `sheets_real_grammar` | boolean (Sheets has a `.bbnf` + is emitted via the generator only, NOT a 25-LOC stub) | yes for PROVE |
| `sheets_grammar_shape` | enum (pratt-operator / flat-stream / tree) — discloses the Sheets shape so the litmus's "genuinely different shape" claim is machine-checkable; `google-sheets.bbnf` is `pratt-operator` (a third JSON/flat-stream would hollow the litmus) | yes for PROVE |
| `acceleration_at_admission` | enum (admission / cfg-test-only / scalar-passthrough-labeled / retired) — any "NEON/ASM" claim must be `admission`; `cfg-test-only` is NO-GO for an acceleration claim | yes for G5/G6 SIMD claims |
| `x86_tree_deleted` | boolean (NO x86 surface anywhere in `bbnf-simd` — `src/x86_64/` gone AND `ext/x86/` gone AND `build.rs` carries no nasm/x86-assembler path AND `lib.rs:247`'s `ext/x86/bbnf.asm` reference re-homed AND the `nasm-rs` build-dep removed from `Cargo.toml:19` (+`:14-16` comments) AND `lib.rs:5 pub mod x86_64;` + the `#[cfg(target_arch="x86_64")]` dispatch arms (`:285-288`) removed AND the in-crate doc surfaces scrubbed-or-out-of-band AND `tests/checkasm_parity.rs`'s 9 ACTIVE compile-coupled `bbnf_simd::x86_64::…::*_scalar(…)` call sites (`:458,:464,:467,:477,:478,:484,:493,:497,:502`) DECOUPLED-OR-DELETED (retaining the aarch64 parity assertions) AND `src/scalar/byte_class_from_eq_set_64.rs`'s x86 doc strings (`:10,:12,:15`) CLEANED to aarch64/scalar-neutral — the deletion list is reach-matched to the verify grep so the gate is satisfiable-by-construction, CH6 V4 §1 + V5 R-2/CH5 §F.6; verified crate-wide `grep -riE --include='*.rs' --include='Cargo.toml' 'avx\|gfni\|sve\|x86\|nasm' skinny/crates/bbnf-simd/` = aarch64-neutral only, NOT `src/`-scoped, per CH5 V3 §C.5/§F.7; BUILD-SOUNDNESS close-gate `cargo build` AND `cargo test --no-run` clean — the `checkasm_parity.rs` decoupling is what keeps the `src/x86_64/` deletion build-sound) | yes (P1) |
| `lock14_gate_scans_codegen` | boolean (`GENERIC_SCAN_ROOTS` covers `runtime_generator.rs` + templates; no `diagnostic-x86` exclusion) | yes (P4) |
| `metalang_leak_present` | boolean (must be `false` — no `parse_w11_1_number` in shipped JSON `generated.rs`) | yes (P5) |
| `materialization_framing` | enum (lazy-rich-vs-eager-cssom / symmetric-comparator) — the honest CSS framing | yes for CSS >SOTA (H1) |
| `corpus_in_timer` | boolean (real corpus inside the timed region, cold; must be `true`; no micro-fixtures) | yes for >SOTA claims |
| `regen_check_clean` | boolean (`cargo xtask regen --check` clean over all generated files) | yes for close |

S-P3 binds an executable gate consumer
`(cd skinny && cargo xtask gate-json --check-results --skv18-generalization-report
<path>)` that consumes: `grammar_derived`, `parity_oracle_diff`,
`verbatim_blob_present == false`, `emitter_fork_present == false`,
`generator_grammar_branch_count == 0` (FULL-alphabet arm census over codegen AND xtask
metadata — self-disclosing-token branches), `generator_grammar_type_count == 0`
(grammar-named-type census), `runtime_target_rows_collapsed == true` (the structural
relocated-seam check the arm census cannot do, CH2 V3 §8.1),
`phantom_generic_resolved ∈ {instantiated,deleted}`,
`shared_value_trait_instantiations >= 2`, `json_rich_navigation_preserved == true`,
`generator_grammar_count == 3`,
`generated_md5_distinct == true`, `sheets_real_grammar == true`,
`sheets_grammar_shape == pratt-operator`,
`acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}` (NOT
`cfg-test-only` for any acceleration claim), `x86_tree_deleted == true`,
`lock14_gate_scans_codegen == true`, `metalang_leak_present == false`,
`materialization_framing` disclosed, `corpus_in_timer == true`,
`regen_check_clean == true`; and re-uses the SK-V17 JSON + CSS >SOTA guard consumers
(51/51 JSON strict; CSS N≥200 cold median full-CSSOM; EXACT cssparser equality;
preserve-rich-ast). The gate REJECTS any row with `verbatim_blob_present == true`,
`emitter_fork_present == true`, `generator_grammar_branch_count > 0` (a grammar-branching
emitter body even when md5-distinct — a SELF-DISCLOSING-token branch in codegen or xtask
metadata), `runtime_target_rows_collapsed == false` (a per-grammar branch relocated into a
neutral-identifier `RuntimeTarget` data-table — the seam the arm census is syntactically
incapable of detecting, caught structurally by the per-`grammar_name` config-tuple collapse check
over all non-path columns (enumerate-by-exclusion over the `regen.rs:6` 12-field struct) including the
per-profile columns `profile`/`source_inputs`/`metadata_inputs`/`fact_schema`/`output_plane`/`emitter`/
`row_id`/`entry_rule`, NOT a `(source_roots,entry_rule)`-only projection, CH2 V4/V5 §8.1 / V3 §8.1),
`generator_grammar_type_count > 0` (a re-emitted grammar-named
parser/`EventGrammar` type the arm census misses), `phantom_generic_resolved == phantom`,
`json_rich_navigation_preserved == false` (LCD-flatten regression behind a ≥2 impl-count),
`generated_md5_distinct == false`, `sheets_grammar_shape ∈ {flat-stream,tree}` on a Sheets
claim (third-JSON hollowing), `acceleration_at_admission == cfg-test-only` on an
acceleration claim, `corpus_in_timer == false`, or any single-tuple broadcast
(`sample_count == 1` or one tuple across multiple corpus rows).

## Section 3 — Trajectory

SK-V18 is the GENERALIZATION cycle: **PRUNE → GENERALIZE → PROVE → HONESTY.**

PRUNE first (the standing order): delete the x86 tree (P1), the old contrived CSS
bench (P2), the 7 byte-identical replicas (P3); make the Lock-14 gate meaningful (P4);
purge the metalang leak (P5). PRUNE reduces the surface for the GENERALIZE waves and
makes a green Lock-14 gate trustworthy BEFORE the emitter rebuild.

Then GENERALIZE (the inflection backtrack): project the JSON parser from the grammar
(G1, the hand-written template = parity oracle); route CSS through grammar lowering,
retiring the const-string courier (G2, LOW risk — the hot path is scalar, no fragile
kernel); un-fork the generator into one grammar-agnostic emitter (G3); define the
shared `Value`/`Document`/`Cursor` trait and instantiate-or-delete the phantom
`ValueRef<G>` (G4); migrate JSON's bespoke scanner onto the neutral NEON kernel (G5);
wire-or-retire the CSS NEON honestly + the aarch64 ASM backlog (G6). Throughout, the
>SOTA is PRESERVED from the grammar-DERIVED parsers — a derived parser that loses the
speed or the equality is not done (HANDOFF §6: surface it honestly as a named
validated grammar-parameterized primitive, never a silent hand-written blob).

Then PROVE: bring Sheets up to a real third grammar via the generator ONLY — if one
generator emits a third grammar from `.bbnf` (md5-distinct `generated.rs`,
instantiating the shared trait), generalization is REAL, not JSON+CSS-overfit.

Then HONESTY: re-frame the CSS >SOTA as lazy-rich-summary vs eager-full-CSSOM or add a
symmetric comparator (H1); the canonical `css_canon_bench` is the honest harness.

If SK-V18 closes with R10 met (one generator, three grammars, shared trait, phantom
resolved, >SOTA preserved honestly, x86 gone, meaningful gate, clean regen), the
inflection backtrack is complete and SK-V19 becomes the TOTALITY-fold tranche
(`crates/core/` adoption) + BBNF-self as the fourth grammar. If a generalization wave
proves a grammar-derived parser CANNOT preserve the >SOTA without hand-shaping, that
is a genuine finding — the hand-shaping becomes a named, validated,
grammar-parameterized primitive (not a silent blob), recorded honestly, and Pass
Alpha V{N+1} revises the candidate shortlist per PASS-ALPHA §5/§8.

The detailed wave-by-wave falsifiability gates (PASS-ALPHA §4.4: owner paths, entry
gate, exit gate, hard cap, revert protocol, same-wave consumer, pre-blocked routes)
are authored downstream by skinny pass S-P3 in `sk-v18/SPEC.md`, consuming this
goalset. Alpha supplies only the measurable goalset (§0.1/§0.5), the strict comparator
gate (§0.6), the telemetry binding (Section 2), and the pre-blocked routes (§0.4).
Revert protocol, hard caps, and per-wave triumvirate discipline are sanctioned-deferred
to S-P3 per PASS-ALPHA §4.4 — but S-P3's revert protocol MUST encode the **entry-gate
dependency graph** (PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1, per alphaE
§cross-cutting 1): a wave that fails its exit gate BLOCKS every downstream wave that
entry-gates on it — no downstream wave dispatches over a REDRESSed predecessor; in
particular G1 failure blocks G2/G3/G4/PROVE, and G3 (un-fork) failure blocks PROVE
(which emits Sheets THROUGH the un-forked generator). S-P3 MUST further carry the standing
dispatch-hard-cap defaults (research/plan/redress 20/15/30 min unless a wave's risk class —
the Sheets/NEON cluster is MED-HIGH per alphaE — justifies a documented larger cap) so no
SK-V18 wave dispatches uncapped. This converts the sanctioned deferral from "revert TBD"
into "revert TBD with a binding dependency graph + a halt ceiling" — the difference between
a legitimate handoff and a paper-close.
